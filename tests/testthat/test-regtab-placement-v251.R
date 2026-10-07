# Literal expectations for G05-G07/G09; external native controls live in qa.
wp4b_rows <- function() {
  x <- data.frame(term = c("gA", "gB", "gC", "x"),
                  variable = c("g", "g", "g", "x"),
                  var_type = c("categorical", "categorical", "categorical", "continuous"),
                  label = c("A", "B", "C", "Age"), var_label = c("Group", "Group", "Group", "Age"),
                  key = c("1.g", "2.g", "3.g", "x"),
                  reference_row = c(FALSE, TRUE, FALSE, FALSE),
                  estimate = c(2, NA, -1, .004), conf.low = c(1, NA, -2, NA),
                  conf.high = c(3, NA, 0, NA), p.value = c(.01, NA, .05, NA))
  x$status <- c("est", "ref", "est", "notest")
  attr(x, "effect_scale") <- "Coef."
  attr(x, "conf.level") <- .95
  x
}

test_that("reftop uses fitted states and transports numeric and flat identities", {
  d <- wp4b_rows()
  t <- regtab(d, reftop = TRUE)
  expect_identical(t$rows$key, c("g", "2.g", "1.g", "3.g", "x"))
  f <- tt_flat(t)
  expect_identical(f[["_state_1"]], c("", "ref", "est", "est", "notest"))
  expect_identical(t$stored$table_role, "raw_analytical")
  expect_identical(unname(t$stored$table[, 1L]), c(2, -1, .004))
  expect_identical(t$meta$regtab_raw_rows$key, t$rows$key)
  expect_identical(as_forest_data(t)$source_row, c(2L, 3L, 4L))
  other <- d
  other$reference_row <- c(TRUE, FALSE, FALSE, FALSE)
  other$status <- c("ref", "est", "est", "notest")
  other$estimate[1L] <- other$conf.low[1L] <- other$conf.high[1L] <- other$p.value[1L] <- NA
  other$estimate[2L] <- 1; other$conf.low[2L] <- 0; other$conf.high[2L] <- 2; other$p.value[2L] <- .1
  expect_error(regtab(d, other, reftop = TRUE), class = "tabtools_error_regtab_layout")
})

test_that("cellnote replaces exact publication cells and keeps raw records separate", {
  d <- wp4b_rows()
  literal <- '$literal `macro\' \\ "quoted"'
  t <- regtab(d, d, cellnote = list(list(row = "A", model = 1L, text = literal),
                                   list(row = "A", model = 1L, text = "last")))
  expect_identical(unname(unlist(t$body[2L, 2:4])), c("last", "", ""))
  expect_identical(t$body[[5L]][2L], "2.00")
  expect_identical(tt_flat(t)[["_state_1"]][2L], "masked")
  pub <- subset(t$meta$regtab_rows, key == "1.g" & model == 1L)
  raw <- subset(t$meta$regtab_raw_rows, key == "1.g" & model == 1L)
  expect_identical(pub$override_origin, "cellnote")
  expect_identical(pub$override_text, "last")
  expect_true(is.na(pub$estimate) && is.na(pub$conf.low) && is.na(pub$p.value))
  expect_identical(raw$estimate, 2)
  expect_identical(t$stored$table[1L, 1L], 2)
  expect_true(is.na(t$stored$publication_table[1L, 1L]))
  expect_false(any(as_forest_data(t)$label == "  A" & as_forest_data(t)$model == 1L))
  expect_identical(t$stored$smallcells$n_masked, 0L)
  expect_identical(t$stored$N_masked, 0L)
  blank <- regtab(d, cellnote = list(list(row = "A", model = 1L, text = "")))
  expect_identical(blank$body[[2L]][2L], "")
  expect_false(any(blank$stored$publication_table == 2, na.rm = TRUE))
  for (record in list(list(row = "a", model = 1, text = "x"), list(row = "Age", model = 3, text = "x"))) {
    expect_error(regtab(d, cellnote = list(record)), class = "tabtools_error_regtab_layout")
  }
  expect_error(regtab(d, stats = "n", cellnote = list(list(row = "N", model = 1, text = "x"))), class = "tabtools_error_regtab_layout")
  native <- regtab(d, cellnote = '"A" 1 "literal $x \\ text"')
  expect_identical(native$body[[2L]][2L], "literal $x \\ text")
})

test_that("body insertion follows raw anchors and all aligned companions", {
  d <- wp4b_rows()
  t <- regtab(d, reftop = TRUE, addrow = list(
    list(label = "Same $label", values = "first", after = "g"),
    list(label = "Same $label", values = "second", after = "3.g"),
    list(label = "Tail", values = "end")))
  expect_identical(t$rows$key, c("g", "2.g", "1.g", "3.g", "addrow:Same $label", "addrow:Same $label:1", "x", "addrow:Tail"))
  expect_identical(t$body[[2L]][5:6], c("first", "second"))
  expect_identical(t$rows$inserted, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(tt_flat(t)[["_state_1"]], c("", "ref", "est", "est", "", "", "notest", ""))
  expect_identical(subset(t$meta$regtab_rows, key == "x")$row, 7L)
  expect_identical(as_forest_data(t)$source_row, c(2L, 3L, 4L))
  for (anchor in c("G", "missing")) expect_error(regtab(d, addrow = list(list(label = "x", values = "x", after = anchor))), class = "tabtools_error_regtab_layout")
  s <- regtab(d, addrow = '"inside" "literal $value", after(1b.g)')
  expect_identical(s$body[[1L]][3L], "  inside")
})

test_that("dimnonsig never identifies references through rendered numeric text", {
  d <- data.frame(term = c("a", "b"), estimate = c(.004, 1), conf.low = c(NA, -.1), conf.high = c(NA, 2), p.value = NA_real_)
  attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
  # Explicit estimate state retains the absence of inference for this oracle.
  d$status <- c("est", "est")
  t <- regtab(d, refcat = "0.00", dimnonsig = TRUE)
  expect_identical(t$rows$dim, c(FALSE, TRUE))
  note <- regtab(wp4b_rows(), dimnonsig = TRUE, cellnote = list(list(row = "A", model = 1, text = "Reference")))
  expect_false(note$rows$dim[note$rows$key == "1.g"])
})

test_that("transpose has exact headers, term columns and honest orientation metadata", {
  d <- wp4b_rows()
  t <- regtab(d, d, models = c("Crude", "Adjusted"), transpose = TRUE,
              keep = c("g", "x"), nopvalue = TRUE, cilabel = "limits",
              collabels = c("1.g" = "Drug A"),
              addcol = list(list(label = "Note", values = c("one", "two"), after = "1.g")))
  expect_identical(t$body[[1L]], c("Crude", "Adjusted"))
  expect_identical(t$header[[1L]]$text, c("", "Drug A", "Note", "Group: B", "Group: C", "Age"))
  expect_identical(t$header[[2L]]$text, c("", "Coef. (limits)", "", "Coef. (limits)", "Coef. (limits)", "Coef. (limits)"))
  expect_identical(t$body[[2L]], rep("2.00 (1.00, 3.00)", 2L))
  expect_true(all(is.na(t$cols$model)))
  expect_identical(t$cols$term_key, c("", "1.g", "", "2.g", "3.g", "x"))
  expect_error(tt_flat(t), class = "tabtools_error_regtab_orientation")
  flat <- tt_flat(t, keyed = FALSE)
  expect_identical(class(flat), "data.frame")
  expect_identical(names(flat)[2L], "Drug A: Coef. (limits)")
  expect_error(tt_merge(t, t), class = "tabtools_error_regtab_orientation")
  expect_error(tt_stack(t, t), class = "tabtools_error_regtab_orientation")
  expect_identical(as_forest_data(t)$source_row, as_forest_data(regtab(d, d))$source_row)
  expect_error(regtab(d, addcol = list(Extra = "x")), class = "tabtools_error_regtab_layout")
  expect_error(regtab(d, transpose = TRUE, dimnonsig = TRUE), class = "tabtools_error_regtab_layout")
  expect_error(regtab(d, transpose = TRUE, addcol = list(Extra = c("x", "y"))), class = "tabtools_error_regtab_layout")
})

test_that("custom frames require truthful scale and inference declarations", {
  d <- data.frame(term = "x", estimate = 2, std.error = .2)
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  attr(d, "stata_cmd") <- "stintreg"
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  attr(d, "effect_scale") <- "TR"; attr(d, "distribution") <- "weibull"; attr(d, "metric") <- "log_time"
  attr(d, "se_scale") <- "link"
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  attr(d, "inference_reference") <- "normal"
  t <- regtab(d)
  expect_equal(t$meta$regtab_rows$estimate, 2, tolerance = 0)
  expect_equal(t$meta$regtab_rows$conf.low, 1.3514179622740914, tolerance = 1e-13)
  expect_equal(t$meta$regtab_rows$conf.high, 2.9598541026264154, tolerance = 1e-13)
  attr(d, "metric") <- "log_hazard"
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  attr(d, "metric") <- "log_time"; attr(d, "distribution") <- "gompertz"
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  complete <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 4, p.value = .1)
  attr(complete, "effect_scale") <- "TR"
  expect_error(regtab(complete), class = "tabtools_error_regtab_metadata")
  attr(complete, "conf.level") <- .95
  expect_identical(regtab(complete)$body[[2L]], "2.00")
  fixed <- data.frame(term = "x", estimate = 2, std.error = 0, status = "constrained", constraint_value = 2, constraint_source = "declared equality")
  attr(fixed, "effect_scale") <- "Coef."
  expect_identical(tt_flat(regtab(fixed))[["_state_1"]], "constrained")
  fixed$constraint_value <- 3
  expect_error(regtab(fixed), class = "tabtools_error_regtab_metadata")
})

test_that("case-sensitive outcomes and actual interval responses determine provenance", {
  d <- data.frame(x = 1:8, Y = c(1, 3, 2, 5, 4, 6, 9, 7), y = c(1, 2, 4, 3, 5, 7, 6, 8))
  expect_identical(tabtools:::tt_model_info(lm(Y ~ x, d))$outcome_id, "Y")
  expect_identical(tabtools:::tt_model_info(lm(y ~ x, d))$outcome_id, "y")
  skip_if_not_installed("survival")
  lower <- c(1, 2, 3, 4, 5, 6, 7, 8)
  upper <- lower + c(1, 2, 1, 3, 2, 2, 3, 2)
  fit <- survival::survreg(survival::Surv(lower, upper, type = "interval2") ~ 1, dist = "weibull", model = TRUE)
  info <- tabtools:::tt_model_info(fit)
  expect_identical(info$effect_scale, "TR")
  expect_identical(info$stata_cmd, "stintreg")
  expect_identical(info$provenance, "fitted_interval_censored_AFT")
  expect_identical(info$outcome_id, 'survival::Surv(lower, upper, type = "interval2")')
})


test_that("literal publication override arrays follow selection and composition", {
  d <- wp4b_rows()
  t <- regtab(d, cellnote = list(list(row = "A", model = 1L, text = "literal")))
  f <- tt_flat(t)
  expect_identical(attr(f[2L, ], "publication_overrides")$text, matrix("literal", 1L, 1L))
  merged <- tt_flat(tt_merge(t, regtab(d)))
  overrides <- attr(merged, "publication_overrides")
  expect_identical(overrides$origin[2L, ], c("cellnote", ""))
  expect_identical(overrides$text[2L, ], c("literal", ""))
  stacked <- tt_flat(tt_stack(t, regtab(d), groups = c("A", "B")))
  expect_identical(which(attr(stacked, "publication_overrides")$origin == "cellnote"), 3L)
  expect_error(as_forest_data(tt_merge(t, regtab(d))))
  mixed <- regtab(lm(mpg ~ wt, mtcars), structure(data.frame(term = "wt", estimate = -5, std.error = .5), effect_scale = "Coef.", inference_reference = "normal"))
  expect_true("variance_ok" %in% names(mixed$meta$regtab_raw_rows))
  expect_identical(sort(unique(mixed$meta$regtab_raw_rows$model_index)), 1:2)
})


test_that("transposed plain frames do not claim keyed-flat metadata and publish literally", {
  d <- data.frame(term = c("x", "y"), estimate = c(2, -1), conf.low = c(1, -2),
    conf.high = c(3, 0), p.value = c(.01, .2))
  attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
  x <- regtab(d, d, transpose = TRUE, nopvalue = TRUE)
  flat <- tt_flat(x, keyed = FALSE)
  expect_identical(attr(flat, "header", exact = TRUE), x$header)
  expect_identical(attr(flat, "command", exact = TRUE), "regtab")
  expect_identical(attr(flat, "frame", exact = TRUE), x$meta$frame)
  expect_true(attr(flat, "composition_export", exact = TRUE))
  expect_null(attr(flat, "full_header", exact = TRUE))
  published <- puttab(flat)
  expect_identical(unname(as.matrix(published$body)), unname(as.matrix(x$body)))
  expect_error(tt_flat(x), class = "tabtools_error_regtab_orientation")
})
