# WP-2E: Stata 2.5.1 publication, no-time, and precision contracts.
wp2e_block <- function(D = c(0, 2, 10), Y = c(100, 20, 50), cats = c("Zero", "Small", "Large"),
                       rate = ifelse(Y > 0, D / Y, NA_real_), lo = ifelse(D > 0, rate / 2, NA_real_),
                       hi = ifelse(D > 0, rate * 2, NA_real_), level = 95) {
  b <- data.frame(g = cats, D = D, Y = Y, Rate = rate, Lower = lo, Upper = hi)
  if (!is.null(level)) {
    attr(b$Lower, "label") <- paste0("Lower ", level, "% confidence limit")
    attr(b$Upper, "label") <- paste0("Upper ", level, "% confidence limit")
  }
  b
}

test_that("native stratetab console blanks only the lower Exposure header", {
  t <- stratetab(golden_strate_blocks("S05")[[1]], borderstyle = "medium", zebra = TRUE)
  lines <- tt_console_lines(t)
  # Exact Stata 2.5.1 S05_console.txt header, c215afd; stratetab.ado:879-886.
  expect_identical(lines[2], "  |    Exposure   Outcome 1                                             |")
  expect_identical(lines[3], "  |                  Events   Person-Years (PY)   Per 1,000 PY (95% CI) |")
  expect_identical(vapply(t$header, function(h) h$text[1], ""), rep("Exposure", 2))
  expect_identical(as.data.frame(t)[1:2, 1], rep("Exposure", 2))
  expect_identical(.xlsx_layout_stratetab(t)$grid[2:3, 2], rep("Exposure", 2))
  # The shared console writer leaves other commands' canonical headers intact.
  other <- t
  other$command <- "custom"
  expect_identical(tt_console_lines(other)[3],
                   "  |    Exposure      Events   Person-Years (PY)   Per 1,000 PY (95% CI) |")
})

test_that("P.2 formats and literal separators resolve explicitness and session digits", {
  b <- wp2e_block(D = 2, Y = 1, cats = "A", rate = 1234.5, lo = 1000, hi = 2000)
  expect_identical(stratetab(b, ratescale = 1, cformat = "%12.2fc", sep = "; ")$body$c4[2],
                   "1,234.50 (1,000.00; 2,000.00)")
  expect_identical(stratetab(b, ratescale = 1, cformat = "%12,2fc", sep = "; ")$body$c4[2],
                   "1.234,50 (1.000,00; 2.000,00)")
  for (sep in c(" ", " - ", "`lo' ${hi}", "")) {
    expected <- if (nzchar(sep)) sep else ", "
    expect_identical(stratetab(b, ratescale = 1, digits = 1, sep = sep)$body$c4[2],
                     paste0("1234.5 (1000.0", expected, "2000.0)"))
  }
  expect_error(stratetab(b, cformat = "%9.2f", digits = 1), class = "tabtools_error_format_conflict")
  expect_error(stratetab(b, cformat = "%9,2f"), class = "tabtools_error_format_separator")
  for (fmt in c("%9.2s", "%td", "%4.9f", NA_character_, "nonsense")) {
    expect_error(stratetab(b, cformat = fmt), class = "tabtools_error_format")
  }
  expect_error(stratetab(b, sep = NA_character_), class = "tabtools_error_format_separator")
  withr::local_options(list(tabtools.digits = 3))
  expect_identical(stratetab(b, ratescale = 1)$body$c4[2], "1234.500 (1000.000, 2000.000)")
  expect_identical(stratetab(b, ratescale = 1, digits = 0)$body$c4[2], "1235 (1000, 2000)")
  expect_no_error(stratetab(b, cformat = "%9.2f"))
})

test_that("primary suppression retains raw analytical rates and linked ratios", {
  b <- wp2e_block()
  base <- stratetab(list(b, b), outcomes = 1, rateratio = TRUE)
  t <- stratetab(list(b, b), outcomes = 1, rateratio = TRUE, smallcells = 5)
  expect_identical(t$body$c2[c(3, 7)], c("<5", "<5"))
  expect_identical(t$body$c3[c(3, 7)], rep("\u2013", 2))
  expect_identical(t$body$c4[c(3, 7)], rep("\u2013", 2))
  expect_identical(t$body$c5[c(3, 7)], c("Ref.", "\u2013"))
  expect_identical(t$stored$rates, base$stored$rates)
  expect_identical(t$stored$ratios, base$stored$ratios)
  expect_identical(t$meta$rate_rows$events, base$meta$rate_rows$events)
  expect_identical(t$stored$smallcells,
                   list(threshold = 5L, mode = "primary", n_masked = 2L, n_linked = 5L))
  expect_identical(t$meta$rate_rows$state, rep(c("est", "masked", "est"), 2))
  expect_identical(t$body$c2[c(2, 6)], c("0", "0"))
  withr::local_options(list(tabtools.smallcells = 5L, tabtools.masktext = "private"))
  expect_identical(stratetab(b)$body$c2[3], "private")
  expect_identical(stratetab(b, masktext = "explicit")$body$c2[3], "explicit")
  expect_identical(stratetab(b, masktext = NULL)$body$c2[3], "<5")
  expect_identical(stratetab(b, nosmallcells = TRUE)$stored$smallcells$threshold, 0L)
  expect_identical(stratetab(b, smallcells = NULL)$stored$smallcells$threshold, 0L)
  expect_identical(stratetab(b, smallcells = 0)$body, stratetab(b, nosmallcells = TRUE)$body)
  expect_error(stratetab(b, smallcells = 5, nosmallcells = TRUE), class = "tabtools_error_smallcells_conflict")
  expect_error(stratetab(b, smallcells = 0, masktext = "x"), class = "tabtools_error_smallcells_masktext")
})

test_that("exact zero bounds use original exposure and level with single scaling", {
  b <- wp2e_block(D = 0, Y = 36, cats = "A")
  t <- stratetab(b, zeroexact = TRUE, ratescale = 1, pyscale = 12, digits = 7)
  # Published Stata ci Technical note: zero colonies in 36 squares, upper .1024689.
  expect_identical(t$body$c4[2], "0.0000000 (0.0000000, 0.1024689)")
  expect_identical(t$meta$rate_rows$lower, 0)
  expect_equal(exp(-t$meta$rate_rows$upper * 36), .025, tolerance = 1e-14)
  expect_identical(t$meta$rate_rows$person_years, 3)
  expect_identical(t$meta$rate_rows$ci_method, "exact_poisson_zero")
  scaled <- stratetab(b, zeroexact = TRUE, ratescale = 1000)
  expect_equal(scaled$meta$rate_rows$upper / t$meta$rate_rows$upper, 1000, tolerance = 1e-14)
  b90 <- wp2e_block(D = 0, Y = 36, cats = "A", level = 90)
  expect_equal(exp(-stratetab(b90, zeroexact = TRUE, ratescale = 1)$meta$rate_rows$upper * 36),
               .05, tolerance = 1e-14)
  expect_identical(stratetab(b)$body$c4[2], "0.0 (\u2013)")
  supplied <- wp2e_block(D = 0, Y = 36, cats = "A", lo = 0, hi = .1)
  expect_identical(stratetab(supplied, zeroexact = TRUE, ratescale = 1)$meta$rate_rows$upper, .1)
  expect_error(stratetab(wp2e_block(level = NULL), zeroexact = TRUE), "no confidence-level provenance")
})

test_that("zero overrides are publication-only and person-time withholding is optional", {
  b <- wp2e_block()
  base <- stratetab(b, zeroexact = TRUE)
  for (mode in c("dash", "blank")) {
    txt <- if (mode == "dash") "\u2013" else ""
    t <- stratetab(b, zeroexact = TRUE, zerocells = mode)
    expect_identical(unname(unlist(t$body[2, 2:4])), c(txt, "100", txt))
    all <- stratetab(b, zeroexact = TRUE, zerocells = mode, zerocells_persontime = TRUE)
    expect_identical(unname(unlist(all$body[2, 2:4])), rep(txt, 3))
    expect_identical(t$stored$rates, base$stored$rates)
    expect_identical(t$meta$rate_rows, base$meta$rate_rows)
  }
  expect_error(stratetab(b, zerocells_persontime = TRUE), "requires")
  expect_error(stratetab(b, zerocells = "d"), "must be NULL")
})

test_that("no-time cells, including references and dependent ratios, are empty", {
  a <- wp2e_block(D = c(0, 2), Y = c(0, 20), cats = c("A", "B"))
  b <- wp2e_block(D = c(3, 0), Y = c(30, 0), cats = c("A", "B"))
  t <- stratetab(list(a, b), outcomes = 1, rateratio = TRUE, zeroexact = TRUE,
                  zerocells = "dash", zerocells_persontime = TRUE, smallcells = 5)
  expect_identical(unname(unlist(t$body[2, 2:5])), rep("", 4))
  expect_identical(unname(unlist(t$body[6, 2:5])), rep("", 4))
  expect_identical(t$body$c5[5], "")
  expect_identical(t$stored$N_nopt, 2L)
  expect_identical(t$stored$smallcells$n_linked, 4L)
  expect_true(is.na(t$stored$rates[1, 1]))
  expect_true(is.na(t$stored$rates[4, 1]))
  expect_true(all(is.na(t$stored$ratios)))
  bad <- wp2e_block(D = 1, Y = 0, cats = "A", rate = NA_real_, lo = NA_real_, hi = NA_real_)
  expect_error(stratetab(bad), class = "tabtools_error_rate_no_time")
  bad$D <- 0
  expect_error(stratetab(bad), class = "tabtools_error_rate_no_time")
})

test_that("zero-length events remain auditable on default and retained paths", {
  d <- data.frame(t = c(0, 10, 0, -1, 0), ev = c(1, 0, 0, 1, 1),
                  g = c("A", "A", "B", "B", "C"), w = c(2, 1, 3, 4, 0))
  legacy <- tt_rates(d, "t", "ev", by = "g", fweight = "w", float_time = FALSE)
  expect_identical(legacy$D, 0)
  expect_identical(legacy$Y, 10)
  expect_identical(attr(legacy, "zero_length"),
                   list(records = 2L, event_records = 1L, events = 2, retained = FALSE))
  retained <- tt_rates(d, "t", "ev", by = "g", fweight = "w", float_time = FALSE, zero_time = "retain")
  expect_identical(retained$g, c("A", "B"))
  expect_identical(retained$D, c(2, 0))
  expect_identical(retained$Y, c(10, 0))
  expect_equal(retained$Rate[1], .2, tolerance = 1e-14)
  expect_identical(retained$Rate[2], NA_real_)
  expect_identical(attr(retained, "zero_length")$retained, TRUE)
  expect_identical(stratetab(retained)$stored$N_nopt, 1L)
  expect_error(tt_rates(data.frame(t = 0, ev = 1), "t", "ev", zero_time = "retain"),
               class = "tabtools_error_rate_no_time")
  late <- tt_rates(data.frame(t = c(5, 5, 4, 6), enter = 5, ev = c(1, 0, 1, 0)),
                   "t", "ev", entry = "enter", zero_time = "retain", float_time = FALSE)
  expect_identical(late$D, 1)
  expect_identical(late$Y, 1)
  expect_error(tt_rates(d, "t", "ev", zero_time = "ret"), class = "tabtools_error_rate_zero_time")
})

test_that("raw grouping codes, types, labels and per-outcome exposure survive", {
  g <- factor(c("high", "low"), levels = c("low", "high"))
  attr(g, "label") <- "Grade"
  a <- wp2e_block(D = c(2, 3), Y = c(20, 30), cats = g)
  b <- wp2e_block(D = c(4, 5), Y = c(40, 50), cats = g)
  before <- a
  t <- stratetab(list(a, b), outcomes = 2, outcomeids = c("event_a", "event_b"), smallcells = 5)
  expect_identical(a, before)
  expect_identical(t$meta$rate_blocks[[1]]$g, a$g)
  expect_identical(t$meta$rate_rows$category_code, rep(c("2", "1"), each = 2))
  expect_identical(t$meta$rate_rows$category_type, rep("factor", 4))
  expect_identical(t$meta$rate_rows$outcome_id, rep(c("event_a", "event_b"), 2))
  expect_identical(t$meta$rate_rows$person_years, c(20, 40, 30, 50))
})

test_that("P2-1F-01 scaling preserves the S08 half-cent double before rounding", {
  t <- stratetab(golden_strate_blocks("S08"), outcomes = 2, outlabels = c("Death", "Readmission"),
                  explabels = c("Period 1", "Period 2"), rateratio = TRUE, level = 95,
                  ratescale = 100000, unitlabel = "100,000", digits = 2)
  r <- t$meta$rate_rows
  cell <- which(r$exposure == 2 & r$category == "Low" & r$outcome == 2)
  expect_length(cell, 1L)
  expect_identical(sprintf("%.17g", r$rate[cell]), "154.26500000000001")
  expect_match(t$body$c8[r$row[cell]], "154.27 (", fixed = TRUE)
})

test_that("publication sinks consume the masked body with literal formatting", {
  b <- wp2e_block()
  out <- withr::local_tempdir()
  t <- stratetab(b, smallcells = 5, masktext = "hidden", cformat = "%12.2f", sep = "; ",
                  csv = file.path(out, "x.csv"), markdown = file.path(out, "x.md"))
  csv <- utils::read.csv(file.path(out, "x.csv"), header = FALSE, check.names = FALSE)
  expect_identical(as.character(csv[5, 2:4]), c("hidden", "\u2013", "\u2013"))
  expect_match(paste(readLines(file.path(out, "x.md")), collapse = "\n"), "hidden | \u2013 | \u2013", fixed = TRUE)
  expect_match(paste(capture.output(print(t)), collapse = "\n"), "hidden", fixed = TRUE)
  expect_identical(unname(unlist(as.data.frame(t)[5, 2:4])), c("hidden", "\u2013", "\u2013"))
  skip_if_not_installed("openxlsx2")
  path <- file.path(out, "x.xlsx")
  tt_write_xlsx(t, path)
  cells <- openxlsx2::wb_to_df(path, col_names = FALSE, skip_empty_rows = FALSE, skip_empty_cols = FALSE)
  expect_identical(unname(unlist(cells[6, 3:5])), c("hidden", "\u2013", "\u2013"))
})

test_that("rendered help explains the publication and analytical split", {
  page <- tools::parse_Rd(test_path("..", "..", "man", "stratetab.Rd"))
  text <- paste(capture.output(tools::Rd2txt(page)), collapse = "\n")
  expect_match(text, "primary only", fixed = TRUE)
  expect_match(text, "raw", fixed = TRUE)
  expect_match(text, "N_nopt", fixed = TRUE)
  expect_match(text, "tabtools_error_format_conflict", fixed = TRUE)
  rate <- tools::parse_Rd(test_path("..", "..", "man", "tt_rates.Rd"))
  expect_match(paste(capture.output(tools::Rd2txt(rate)), collapse = "\n"), "zero_length", fixed = TRUE)
})

test_that("gt renders publication masks while raw rates remain stored", {
  skip_if_not_installed("gt")
  t <- stratetab(wp2e_block(), smallcells = 5, masktext = "hidden")
  gt <- tt_as_gt(t)
  expect_identical(unname(unlist(gt$`_data`[3, 2:4])), c("hidden", "\u2013", "\u2013"))
  expect_identical(t$meta$rate_rows$events, c(0, 2, 10))
})

test_that("tinytable renders publication masks while raw rates remain stored", {
  skip_if_not_installed("tinytable")
  t <- stratetab(wp2e_block(), smallcells = 5, masktext = "hidden")
  md <- paste(capture.output(print(tt_as_tinytable(t), output = "markdown")), collapse = "\n")
  expect_match(md, "hidden", fixed = TRUE)
  expect_false(grepl("100.0 (50.0, 200.0)", md, fixed = TRUE))
  expect_identical(t$meta$rate_rows$events, c(0, 2, 10))
})

test_that("flextable renders publication masks while raw rates remain stored", {
  skip_if_not_installed("flextable")
  t <- stratetab(wp2e_block(), smallcells = 5, masktext = "hidden")
  ft <- flextable::as_flextable(t)
  expect_identical(unname(unlist(ft$body$dataset[3, 2:4])), c("hidden", "\u2013", "\u2013"))
  expect_identical(t$meta$rate_rows$events, c(0, 2, 10))
})

test_that("M1: exact zero completes a source zero lower and missing upper", {
  b <- wp2e_block(D = 0, Y = 36, cats = "A", lo = 0, hi = NA_real_)
  t <- stratetab(b, zeroexact = TRUE, ratescale = 1, digits = 7)
  # Pinned native result from stratetab.ado:541-543; ci Technical note p.296.
  expect_identical(t$body$c4[2], "0.0000000 (0.0000000, 0.1024689)")
  expect_equal(exp(-36 * t$meta$rate_rows$upper), .025, tolerance = 1e-14)
  expect_identical(t$meta$rate_rows$ci_method, "exact_poisson_zero")
  expect_identical(t$meta$rate_blocks[[1]]$Lower, 0)
  expect_identical(t$meta$rate_blocks[[1]]$Upper, NA_real_)
  # A supplied positive lower is not replaced merely because upper is absent.
  b$Lower <- .01
  t <- stratetab(b, zeroexact = TRUE, ratescale = 1)
  expect_identical(t$meta$rate_rows$lower, .01)
  expect_identical(t$meta$rate_rows$upper, NA_real_)
  expect_identical(t$meta$rate_rows$ci_method, "supplied")
})

test_that("M2: generated exact overflow becomes missing before publication", {
  b <- wp2e_block(D = 0, Y = 1e-300, cats = "A")
  t <- stratetab(b, zeroexact = TRUE, ratescale = 1e100)
  # Independently observed pinned native result: missing interval, not (0.0, .).
  expect_identical(t$body$c4[2], "0.0 (\u2013)")
  expect_identical(t$meta$rate_rows$upper, NA_real_)
  expect_identical(t$meta$rate_rows$lower, 0)
  expect_identical(t$meta$rate_rows$ci_method, "exact_poisson_zero")
})

test_that("M3: source no-time diagnostic excludes positive exposure display underflow", {
  b <- wp2e_block(D = c(0, 0), Y = c(1e-300, 0), cats = c("Tiny", "NoTime"))
  t <- stratetab(b, pyscale = 1e100)
  # Pinned native source count is 1 despite both scaled times being zero.
  expect_identical(t$stored$N_nopt, 1L)
  expect_identical(t$meta$rate_rows$person_years, c(0, 0))
  expect_identical(unname(unlist(t$body[2, 2:4])), rep("", 3))
  expect_identical(unname(unlist(t$body[3, 2:4])), rep("", 3))
  expect_identical(unname(t$stored$rates[, 1]), c(0, NA_real_))
})

test_that("S1: per-cell provenance preserves original methods and unknown supplied bounds", {
  b <- wp2e_block(D = 3, Y = 100, cats = "A", lo = .001, hi = .9)
  t <- stratetab(b)
  expect_identical(t$meta$rate_rows$ci_method, "supplied")
  expect_identical(attr(t$meta$rate_blocks[[1]], "ci_method"), "supplied")
  attr(b, "ci_method") <- "known_external_interval"
  expect_identical(stratetab(b)$meta$rate_rows$ci_method, "known_external_interval")
  r <- tt_rates(data.frame(t = c(1, 2), e = c(1, 0)), "t", "e", float_time = FALSE)
  expect_identical(stratetab(r)$meta$rate_rows$ci_method, "lognormal")
})

test_that("S4: cformat never validates an unused session digit fallback", {
  withr::local_options(list(tabtools.digits = "bad"))
  b <- wp2e_block(D = 2, Y = 1, cats = "A", rate = 1234.5, lo = 1000, hi = 2000)
  expect_identical(stratetab(b, ratescale = 1, cformat = "%12.2fc")$body$c4[2],
                   "1,234.50 (1,000.00, 2,000.00)")
  expect_error(stratetab(b), "digits")
  expect_error(stratetab(b, cformat = "%12.2f", digits = 2), class = "tabtools_error_format_conflict")
})
