test_that("ordinary KM and RMST recover independent rectangle/covariance literals", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0))
  tt <- survtab(d, "exit", "event", c(1, 2, 3), rmst = 3, riskset = TRUE, events = TRUE)
  raw <- tt$meta$survival$raw
  expect_identical(raw$curves[[1L]]$risk, c(4, 3))
  expect_identical(raw$curves[[1L]]$events, c(1, 1))
  expect_identical(raw$curves[[1L]]$survival, c(3/4, 1/2))
  expect_equal(raw$queries[[1L]]$se[2L], 1/4, tolerance = 1e-14)
  expect_equal(tt$stored$rmst_1, 9/4, tolerance = 1e-14)
  # Two unit-width rectangles: Var(S1+S2), with independent hand covariance.
  covariance <- matrix(c(3/64, 1/32, 1/32, 1/16), 2L)
  expect_equal(raw$rmst[[1L]]$variance, sum(covariance), tolerance = 1e-14)
  expect_equal(tt$stored$rmst_se_1, sqrt(11)/8, tolerance = 1e-14)
  expect_identical(tt$stored$events_1, 2)
  expect_identical(tt$stored$atrisk_1, 4)
  expect_identical(tt$command, "survtab")
  expect_identical(nrow(tt$stored$table), 3L)
})

test_that("declared frequency counts agree with full independent expansion", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), w = c(10, 1, 10, 1))
  fw <- survtab(d, "exit", "event", c(1, 2, 3), fweight = "w", rmst = 3, events = TRUE, riskset = TRUE)
  expanded <- d[rep(seq_len(nrow(d)), d$w), c("exit", "event")]
  ex <- survtab(expanded, "exit", "event", c(1, 2, 3), rmst = 3, events = TRUE, riskset = TRUE)
  expect_equal(fw$stored$rmst_1, 45/22, tolerance = 1e-14)
  expect_equal(fw$meta$survival$raw$rmst[[1L]]$variance, 461/10648, tolerance = 1e-14)
  expect_equal(fw$stored$table, ex$stored$table, tolerance = 1e-14)
  expect_equal(fw$stored$rmst_se_1, ex$stored$rmst_se_1, tolerance = 1e-14)
  expect_identical(fw$stored$atrisk_1, 22)
  expect_identical(fw$stored$events_1, 11)
  expect_identical(fw$meta$survival$raw$queries[[1L]]$risk[2L], 12)
  expect_identical(fw$meta$survival$weight_type, "frequency")
})

test_that("delayed entry uses the left-open event-risk boundary", {
  d <- data.frame(subject = 1:3, start = c(0, 2, 0), stop = c(2, 3, 4), event = c(1, 1, 0))
  tt <- survtab(d, "stop", "event", c(2, 3), entry = "start", id = "subject", rmst = 3, riskset = TRUE)
  expect_identical(tt$meta$survival$raw$curves[[1L]]$risk, c(2, 2))
  expect_identical(tt$stored$table[, 1L], c(t2 = 1/2, t3 = 1/4))
  expect_equal(tt$stored$rmst_1, 5/2, tolerance = 1e-14)
  expect_equal(tt$meta$survival$raw$rmst[[1L]]$variance, 1/8, tolerance = 1e-14)
  expect_identical(tt$meta$survival$raw$queries[[1L]]$risk, c(2, 2))
})

test_that("touching split records preserve subject counts and Greenwood inference", {
  d <- data.frame(id = 1:4, entry = 0, exit = 1:4, event = c(1, 1, 0, 0))
  split <- rbind(d[1:3, ], data.frame(id = 4, entry = c(0, 2), exit = c(2, 4), event = 0))
  a <- survtab(d, "exit", "event", c(1, 2, 3), entry = "entry", id = "id", rmst = 3, events = TRUE)
  b <- survtab(split, "exit", "event", c(1, 2, 3), entry = "entry", id = "id", rmst = 3, events = TRUE)
  expect_identical(a$stored$table, b$stored$table)
  expect_identical(a$stored$rmst_se_1, b$stored$rmst_se_1)
  expect_identical(b$stored$atrisk_1, 4)
  expect_identical(nrow(b$meta$survival$records), 5L)
  bad <- split; bad$entry[5L] <- 1
  expect_error(survtab(bad, "exit", "event", 2, entry = "entry", id = "id"), class = "tabtools_error_survival_inference")
  bad <- split; bad$event[4:5] <- 1
  expect_error(survtab(bad, "exit", "event", 2, entry = "entry", id = "id"), class = "tabtools_error_survival_inference")
  bad <- split; bad$event[4L] <- 1
  expect_error(survtab(bad, "exit", "event", 2, entry = "entry", id = "id"), class = "tabtools_error_survival_inference")
  bad <- split; bad$g <- c(1, 1, 2, 1, 2)
  expect_error(survtab(bad, "exit", "event", 2, by = "g", entry = "entry", id = "id"), class = "tabtools_error_survival_inference")
  bad <- split; bad$w <- c(1, 1, 1, 1, 2)
  expect_error(survtab(bad, "exit", "event", 2, entry = "entry", id = "id", fweight = "w"), class = "tabtools_error_survival_inference")
})

test_that("near floating event times are distinct and remain in their own risk sets", {
  first <- 0.3; second <- 0.1 + 0.2
  expect_false(identical(first, second))
  d <- data.frame(exit = c(first, second, 1), event = c(1, 1, 0))
  tt <- survtab(d, "exit", "event", c(second, first, second), rmst = second, riskset = TRUE)
  curve <- tt$meta$survival$raw$curves[[1L]]
  expect_identical(curve$time, c(first, second))
  expect_identical(curve$risk, c(3, 2))
  expect_identical(tt$meta$survival$requested_times, c(second, first, second))
  expect_equal(tt$stored$table[, 1L], c(t.3 = 1/3, t.3 = 2/3, t.3 = 1/3), tolerance = 1e-14)
  expect_true(is.finite(tt$stored$rmst_se_1))
})

test_that("log-rank uses the full sample and independent scalar tie corrections", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), g = c(1, 1, 2, 2))
  tt <- survtab(d, "exit", "event", 1, by = "g", rmst = 1)
  lr <- tt$meta$survival$raw$logrank
  expect_equal(lr$score[1L], 7/6, tolerance = 1e-14)
  expect_equal(lr$covariance[1L, 1L], 17/36, tolerance = 1e-14)
  expect_equal(lr$chi2, 49/17, tolerance = 1e-13)
  expect_equal(lr$p, stats::pchisq(49/17, 1, lower.tail = FALSE), tolerance = 1e-13)
  expect_identical(lr$df, 1L)
  tied <- data.frame(exit = c(1, 1, 1, 2), event = c(1, 1, 1, 0), g = c(1, 1, 2, 2))
  lr <- survtab(tied, "exit", "event", 1, by = "g")$meta$survival$raw$logrank
  expect_equal(lr$covariance[1L, 1L], 1/4, tolerance = 1e-14)
  expect_equal(lr$chi2, 1, tolerance = 1e-14)
})

test_that("missing-group failures cannot defeat included no-failure isolation", {
  d <- data.frame(exit = c(1, 2, 3, 4, 2, 0, -1, NA), event = c(0, NA, 0, 0, 1, 1, 1, 1),
                  g = c(1, 1, 2, 2, NA, 1, 2, 1))
  original <- d
  tt <- survtab(d, "exit", "event", c(0.5, 1, 2), by = "g", rmst = 2, events = TRUE)
  expect_true(tt$meta$survival$raw$logrank$no_failure)
  expect_true(is.na(tt$stored$logrank_p))
  expect_identical(c(tt$stored$atrisk_1, tt$stored$atrisk_2), c(2, 2))
  expect_identical(c(tt$stored$rmst_1, tt$stored$rmst_2), c(2, 2))
  expect_identical(c(tt$stored$rmst_se_1, tt$stored$rmst_se_2), c(0, 0))
  expect_identical(tt$body$c1[tt$rows$key == "logrank"], "Log-rank test not possible: no failures in the analysis sample")
  expect_identical(tt$meta$survival$missing_event_rows, 2L)
  expect_identical(tt$meta$survival$exclusions$record_index, 5:8)
  expect_identical(tt$meta$survival$raw$queries[[1L]]$se, c(0, NA_real_, NA_real_))
  expect_identical(d, original)
})

test_that("reverse changes probability direction but not restricted means", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), g = c(0.1, 0.1, 0.2, 0.2))
  attr(d$g, "labels") <- c("Second label" = 0.1, "First label" = 0.2)
  a <- survtab(d, "exit", "event", c(1, 2), by = "g", rmst = 2, difference = TRUE)
  b <- survtab(d, "exit", "event", c(1, 2), by = "g", rmst = 2, difference = TRUE, reverse = TRUE)
  expect_identical(a$meta$survival$group_values, c(0.1, 0.2))
  expect_identical(a$meta$survival$group_labels, c("Second label", "First label"))
  expect_equal(b$stored$table, 1 - a$stored$table, tolerance = 1e-14)
  expect_identical(a$stored$rmst_diff, b$stored$rmst_diff)
  expect_identical(a$meta$survival$raw$rmst_contrast$direction, "group_1_minus_group_2")
  expect_match(a$header[[1L]]$text[4L], "Difference (Second label - First label)", fixed = TRUE)
  expect_match(b$footnote, "no competing risks", fixed = TRUE)
})

test_that("support flags and native endpoint median literals are explicit", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0))
  tt <- survtab(d, "exit", "event", c(2, 4, 5), median = TRUE, level = 90)
  expect_identical(tt$stored$median_1, 2)
  expect_identical(tt$meta$survival$raw$median[[1L]]$lower, 1)
  expect_true(is.na(tt$meta$survival$raw$median[[1L]]$upper))
  expect_identical(tt$meta$survival$publication$beyond_support[, 1L], c(t2 = FALSE, t4 = FALSE, t5 = TRUE))
  expect_identical(tt$stored$table[, 1L], c(t2 = 0.5, t4 = 0.5, t5 = 0.5))
  expect_error(survtab(d, "exit", "event", 2, rmst = 5), class = "tabtools_error_survival_support")
  terminal <- survtab(data.frame(exit = 2, event = 1), "exit", "event", c(1, 2), median = TRUE, rmst = 2)
  expect_true(is.na(terminal$stored$median_1))
  expect_identical(terminal$stored$rmst_1, 2)
  expect_identical(terminal$stored$rmst_se_1, 0)
  expect_true(is.na(terminal$meta$survival$raw$queries[[1L]]$se[2L]))
  # Independently measured SV009/010 medians and SV001 terminal SE query.
  two <- survtab(data.frame(exit = c(1, 2), event = 1), "exit", "event", c(1, 2), median = TRUE, level = 90)
  expect_identical(two$stored$median_1, 1)
  expect_equal(two$meta$survival$raw$queries[[1L]]$se, rep(sqrt(1/8), 2), tolerance = 1e-14)
  expect_true(is.na(two$meta$survival$raw$curves[[1L]]$se[2L]))
  tied <- survtab(data.frame(exit = c(2, 2), event = 1), "exit", "event", c(1, 2), median = TRUE)
  expect_identical(tied$stored$median_1, 2)
  expect_true(is.na(tied$meta$survival$raw$queries[[1L]]$se[2L]))
  d$w <- 1
  expect_true(is.na(survtab(d, "exit", "event", 2, fweight = "w", median = TRUE)$stored$median_1))
})

test_that("survival inputs refuse unsupported inference before writing", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), w = c(1, 1.5, 1, 1))
  expect_error(survtab(d, "exit", "event", 2, fweight = "w"), class = "tabtools_error_survival_inference")
  bad <- d; bad$event[1L] <- 2
  expect_error(survtab(bad, "exit", "event", 2), class = "tabtools_error_survival_input")
  bad <- d; bad$exit[1L] <- Inf
  expect_error(survtab(bad, "exit", "event", 2), class = "tabtools_error_survival_input")
  expect_error(survtab(d, "exit", "event", c(0, 1)), class = "tabtools_error_survival_input")
  expect_error(survtab(d, "exit", "event", 1, difference = TRUE), class = "tabtools_error_survival_input")
  expect_error(survtab(d, "exit", "event", 1, timeunit = "year"), class = "tabtools_error_survival_input")
  expect_error(survtab(d, "exit", "event", 1, entry = -1), class = "tabtools_error_survival_input")
})

test_that("literal groups, extra rows and plain frame metadata avoid fictitious models", {
  d <- data.frame(exit = rep(c(1, 2), 11), event = rep(c(1, 0), 11), g = rep(11:1, each = 2))
  tt <- survtab(d, "exit", "event", c(1, 2), by = "g")
  expect_identical(tt$meta$survival$group_values, 1:11)
  expect_identical(ncol(tt$stored$table), 11L)
  expect_true(all(is.na(tt$cols$model)))
  extra <- as.data.frame(as.list(rep("literal ` $ \\", ncol(tt$body))), stringsAsFactors = FALSE)
  edited <- survtab(d, "exit", "event", 1, by = "g", addrow = extra)
  expect_identical(unname(unlist(tail(edited$body, 1L), use.names = FALSE)), rep("literal ` $ \\", ncol(tt$body)))
  flat <- tt_flat(tt, keyed = FALSE)
  expect_identical(attr(flat, "header", exact = TRUE), tt$header)
  expect_identical(attr(flat, "command", exact = TRUE), "survtab")
  expect_identical(attr(flat, "frame", exact = TRUE), tt$meta$frame)
  expect_identical(attr(flat, "sample_accounting", exact = TRUE), tt$meta$sample_accounting)
  expect_identical(attr(flat, "composition_export", exact = TRUE), TRUE)
  expect_error(tt_flat(tt, keyed = TRUE), class = "tabtools_error_flat")
})

test_that("one-header survival layout retains its own complete geometry", {
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), g = c(1, 1, 2, 2))
  tt <- survtab(d, "exit", "event", c(1, 2), by = "g", footnote = c("One.", "Two."))
  layout <- .xlsx_layout_survtab(tt)
  expect_identical(layout$grid[2L, -1L], tt$header[[1L]]$text)
  expect_identical(layout$grid[3L, -1L], unname(unlist(tt$body[1L, ], use.names = FALSE)))
  expect_identical(layout$grid[nrow(layout$grid)-1:0, 2L], c("One.", "Two."))
  widths <- layout$rules[layout$rules$op == .OP[["width"]], c("c1", "c2", "value")]
  expect_identical(as.numeric(widths$value), c(1, 22, 18))
  expect_identical(widths$c1, c(1, 2, 3))
  lr <- tt$meta$survtab_logrank_row + 2L
  merge <- layout$rules[layout$rules$op == .OP[["merge"]] & layout$rules$r1 == lr, ]
  expect_identical(c(merge$c1, merge$c2), c(2, ncol(tt$body) + 1))
})


test_that("fractional native group identities preserve their levelsof precision", {
  g <- .sv_float(c(.1, .2))
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), group = rep(g, each = 2))
  tt <- survtab(d, "exit", "event", c(1, 2), by = "group", rmst = 2, difference = TRUE)
  expect_identical(tt$stored$group_1_value, ".1000000014901161")
  expect_identical(tt$stored$group_2_value, ".2000000029802322")
  expect_identical(tt$stored$group_1_label, ".1000000014901161")
  expect_identical(tt$meta$survival$group_values, g)
  attr(d$group, "labels") <- c("First" = g[1L], "Second" = g[2L])
  labelled <- survtab(d, "exit", "event", c(1, 2), by = "group")
  expect_identical(labelled$stored$group_1_label, "First")
  expect_identical(labelled$stored$group_1_value, ".1000000014901161")
})

test_that("survival workbook log-rank emphasis handles disabled and active thresholds", {
  withr::local_options(tabtools.boldp = NULL)
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), g = c(1, 1, 2, 2))
  original <- d
  baseline <- survtab(d, "exit", "event", c(1, 2), by = "g")
  expect_identical(baseline$style$boldp, NA_real_)
  expect_identical(baseline$style$highlight, NA_real_)
  expect_equal(baseline$stored$logrank_chi2, 49/17, tolerance = 1e-13)
  plain <- .xlsx_layout_survtab(baseline)
  expect_identical(dim(plain$grid), c(6L, 5L))
  expect_identical(baseline$meta$survtab_logrank_row, 4L)
  expect_false(any(plain$rules$op == .OP[["fill"]] & plain$rules$r1 == 6L))
  expect_false(any(plain$rules$op == .OP[["bold"]] & plain$rules$r1 %in% c(3L, 6L)))
  disabled <- survtab(d, "exit", "event", c(1, 2), by = "g", boldp = -1, highlight = -1)
  expect_identical(.xlsx_layout_survtab(disabled), plain)
  inactive <- survtab(d, "exit", "event", c(1, 2), by = "g", boldp = .05, highlight = .05)
  expect_identical(.xlsx_layout_survtab(inactive), plain)
  emphasized <- survtab(d, "exit", "event", c(1, 2), by = "g", boldp = .1, highlight = .1)
  styled <- .xlsx_layout_survtab(emphasized)
  expect_identical(styled$grid, plain$grid)
  expect_identical(styled$written, plain$written)
  expect_identical(emphasized$stored, baseline$stored)
  extra <- styled$rules[styled$rules$op %in% .OP[c("bold", "fill")] &
    styled$rules$r1 %in% c(3L, 6L), , drop = FALSE]
  expect_identical(as.matrix(extra[c("op", "r1", "r2", "c1", "c2", "code")]),
    matrix(c(2, 3, 3, 5, 5, 1, 2, 6, 6, 2, 5, 1, 7, 6, 6, 2, 5, 0),
      nrow = 3L, byrow = TRUE, dimnames = list(rownames(extra), c("op", "r1", "r2", "c1", "c2", "code"))))
  expect_identical(extra$color, c(NA_character_, NA_character_, "FFFFFFCC"))
  expect_identical(d, original)
})


test_that("no-failure survival console keeps the complete native empty-p geometry", {
  d <- data.frame(exit = c(1, 2, 3, 4, 2), event = c(0, NA, 0, 0, 1),
                  g = c(1, 1, 2, 2, NA))
  original <- d
  tt <- survtab(d, "exit", "event", c(1, 2), by = "g", median = TRUE,
                events = TRUE, riskset = TRUE, rmst = 2)
  expect_identical(tt$cols$console_width, c(NA_integer_, NA_integer_, NA_integer_, 2L))
  expect_true(is.na(tt$stored$logrank_p))
  # Literal authenticated SV002 boxed listing, pin712044f8; no R renderer oracle.
  native_box <- c(
    "  +-------------------------------------------------------------------------------------------------------------+",
    "  |                                                                            1 (N=2)             2 (N=2)    p |",
    "  |                                            Median survival, yr                  NR                  NR      |",
    "  |                                                       (95% CI)                                              |",
    "  |                                                     Events / N               0 / 2               0 / 2      |",
    "  |                                           Survival probability                                              |",
    "  |                                                           1 yr              100.0%              100.0%      |",
    "  |                                                        2 years              100.0%              100.0%      |",
    "  |                                       RMST (2-yr), yr (95% CI)   2.00 (2.00, 2.00)   2.00 (2.00, 2.00)      |",
    "  |                                                 Number at risk                                              |",
    "  |                                                           1 yr                   2                   2      |",
    "  |                                                        2 years                   1                   2      |",
    "  | Log-rank test not possible: no failures in the analysis sample                                              |",
    "  +-------------------------------------------------------------------------------------------------------------+")
  lines <- tt_console_lines(tt)
  expect_identical(lines[seq_len(14L)], native_box)
  expect_identical(lines[-seq_len(14L)], c("",
    "No failures in the analysis sample; the log-rank test is not possible and is omitted.", ""))
  expect_identical(tt$meta$survival$exclusions$record_index, 5L)
  expect_identical(d, original)
})

test_that("review 2026-10-07 B4: log-rank prose handles >0.99 and fweight NR is explained", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL, tabtools.smallcells = NULL))
  # survival::survdiff p = 0.9908 for these groups.
  same <- data.frame(t = c(3, 6, 7, 10, 8, 2, 7, 3, 5, 10, 6, 1),
                     e = c(1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1), g = rep(1:2, each = 6))
  x <- survtab(same, time = "t", event = "e", times = 2, by = "g")
  expect_gt(x$stored$logrank_p, 0.99)
  logrank <- x$body[[1L]][x$meta$survtab_logrank_row]
  expect_match(logrank, ", p > 0.99$")
  expect_false(grepl("= >", logrank, fixed = TRUE))
  expect_identical(.tt_p_prose(c("0.03", "<0.001", ">0.99")), c("p = 0.03", "p < 0.001", "p > 0.99"))
  d <- data.frame(t = 1:8, e = c(1, 1, 1, 1, 1, 0, 1, 0), w = c(2, 1, 1, 1, 1, 1, 1, 1))
  fw <- survtab(d, time = "t", event = "e", times = c(2, 4), median = TRUE, fweight = "w")
  expect_identical(fw$body[[2L]][1L], "NR")
  expect_match(fw$footnote, "NR here means not estimated", fixed = TRUE)
  plain <- survtab(d, time = "t", event = "e", times = c(2, 4), median = TRUE)
  expect_false(grepl("NR here means", plain$footnote, fixed = TRUE))
  nomedian <- survtab(d, time = "t", event = "e", times = 2, fweight = "w")
  expect_false(grepl("NR here means", nomedian$footnote, fixed = TRUE))
})
