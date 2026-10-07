# stratetab() (plan task 7.4) outside the goldens: argument checks in
# Stata's order, level provenance, category handling, the adversarial
# cases of the Phase 7b brief (zero events, zero person-time, missing
# blocks, exposure levels missing from one outcome, a single outcome,
# extreme ratescale), stored metadata, and the tt_rates() provenance it
# relies on.

st_block <- function(cats = c("A", "B"), D = c(10, 20), Y = c(1000, 2000), level = 95, rate = D / Y,
                     lo = rate * 0.5, hi = rate * 2) {
  b <- data.frame(g = cats, `_D` = D, `_Y` = Y, `_Rate` = rate, `_Lower` = lo, `_Upper` = hi,
                  check.names = FALSE)
  if (!is.na(level)) {
    attr(b[["_Lower"]], "label") <- paste0("Lower ", level, "% confidence limit")
    attr(b[["_Upper"]], "label") <- paste0("Upper ", level, "% confidence limit")
  }
  b
}

test_that("argument checks follow stratetab.ado", {
  b <- st_block()
  expect_error(stratetab(list(b, b)), "`outcomes` is required")
  expect_identical(stratetab(list(b))$stored$N_outcomes, 1L)
  expect_error(stratetab(list(b, b, b), outcomes = 2), "multiple of `outcomes`")
  expect_error(stratetab(list(b), outcomes = 0), "at least 1")
  expect_error(stratetab(list(b), outcomes = 1.5), "whole number")
  expect_error(stratetab(b, rateratio = TRUE), "at least two exposure groups")
  expect_error(stratetab(b, outlabels = c("a", "b")), "outcome labels \\(2\\) must match")
  expect_error(stratetab(list(b, b), outcomes = 1, explabels = "x"), "exposure labels \\(1\\) must match")
  expect_error(stratetab(b, outlabels = " "), "blank labels")
  expect_error(stratetab(list(b, b), outcomes = 2, outcomeids = c("x", "X")), "Duplicate outcome identity")
  expect_error(stratetab(list(b, b), outcomes = 2, outcomeids = c("x", " ")), "may not be blank")
  expect_error(stratetab(list(b, b), outcomes = 2, outcomeids = "x"), "outcome IDs \\(1\\)")
  expect_error(stratetab(list(b, b), outcomes = 2, outlabels = c("Same", "same")), "Duplicate outcome identity")
  for (a in c("digits", "eventdigits", "pydigits", "ratiodigits")) {
    expect_error(do.call(stratetab, stats::setNames(list(b, 11), c("x", a))), "between 0 and 10")
    expect_error(do.call(stratetab, stats::setNames(list(b, -1), c("x", a))), "between 0 and 10")
  }
  # C5 / tabtools 2.1.12: non-finite scales are refused (2.1.11 blanks the table).
  for (v in list(0, -1, NA_real_, Inf, "1000", c(1, 2))) {
    expect_error(stratetab(b, pyscale = v), "pyscale")
    expect_error(stratetab(b, ratescale = v), "ratescale")
  }
  expect_error(stratetab(b, level = 100), "level")
  expect_error(stratetab(b, level = 1), "level")
  expect_error(stratetab(b, level = 0), "level")
  expect_error(stratetab(b, xlsx = "a.xls"), "xlsx")
  expect_error(stratetab(b, open = TRUE), "requires `xlsx`")
  expect_error(stratetab(b, mdappend = TRUE), "requires `markdown`")
  expect_error(stratetab(b, markdown = "a.txt"), "markdown")
  expect_error(stratetab(b, sheet = "a/b"), "sheet")
  expect_error(stratetab(b, zebra = NA), "TRUE or FALSE")
  expect_error(stratetab(42), "list of rate blocks")
  expect_error(stratetab(list(42), outcomes = 1), "Block 1 must be a data frame")
  expect_error(stratetab(list(b[, -2])), "Block 1 is not a valid strate block")
})

test_that("the level comes from the blocks and is never assumed", {
  known <- st_block()
  unknown <- st_block(level = NA)
  # No provenance anywhere: level required (stratetab.ado:470-479).
  expect_error(stratetab(unknown), "no confidence-level provenance")
  expect_error(stratetab(list(known, unknown), outcomes = 2), "Some blocks carry no")
  tt <- stratetab(unknown, level = 99)
  expect_identical(tt$header[[2]]$text[4], "Per 1,000 PY (99% CI)")
  expect_identical(tt$stored$ci_level, 99)
  expect_identical(stratetab(unknown, level = 0.9)$stored$ci_level, 90)
  # Labels must agree with each other and with level().
  expect_error(stratetab(list(known, st_block(level = 90)), outcomes = 2), "mixed confidence levels")
  expect_error(stratetab(known, level = 90), "conflicts with block 1's 95% intervals")
  expect_identical(stratetab(known, level = 95)$stored$ci_level, 95)
  odd <- known
  attr(odd[["_Upper"]], "label") <- "Upper 90% confidence limit"
  expect_error(stratetab(odd), "Block 1 is not a valid strate block")
  # A decimal level is shown as string(level, "%21.15g") (tabtools 2.1.12;
  # 2.1.11 printed the macro text, 99.90000000000001).
  expect_identical(stratetab(st_block(level = 97.5))$header[[2]]$text[4], "Per 1,000 PY (97.5% CI)")
  t999 <- stratetab(list(st_block(level = NA), st_block(level = NA)), outcomes = 1, level = 99.9,
                    rateratio = TRUE)
  expect_identical(t999$header[[2]]$text[c(4, 5)], c("Per 1,000 PY (99.9% CI)", "IRR (99.9% CI)"))
  expect_match(t999$stored$methods, "at the 99.9% level;", fixed = TRUE)
  expect_identical(stratetab(st_block(level = NA), level = 0.999)$header[[2]]$text[4], "Per 1,000 PY (99.9% CI)")
  # tt_rates() results record theirs.
  set.seed(3)
  d <- data.frame(t = rexp(300, 0.1), e = rbinom(300, 1, 0.3), g = rep(c("u", "v", "w"), 100))
  r90 <- tt_rates(d, "t", "e", by = "g", level = 0.9)
  tt <- stratetab(r90, rateratio = FALSE)
  expect_identical(tt$stored$ci_level, 90)
  expect_match(tt$stored$methods, "at the 90% level")
  expect_error(stratetab(r90, level = 95), "conflicts")
})

test_that("tt_rates() keeps the provenance of supplied intervals", {
  s <- st_block()
  expect_identical(attr(tt_rates(s), "level"), 0.95)
  expect_true(is.na(attr(tt_rates(st_block(level = NA)), "level")))
  expect_identical(attr(tt_rates(st_block(level = NA), level = 0.9), "level"), 0.9)
  expect_error(tt_rates(s, level = 0.9), "conflicts with the 95% intervals")
  bad <- s
  attr(bad[["_Upper"]], "label") <- "Upper 90% confidence limit"
  expect_error(tt_rates(bad), "conflicting confidence levels")
  # Only the lower label: still read.
  one <- st_block(level = NA)
  attr(one[["_Lower"]], "label") <- "Lower 99.5% confidence limit"
  expect_identical(attr(tt_rates(one), "level"), 0.995)
})

test_that("categories: labels, codes, blanks, duplicates, and alignment", {
  num <- st_block(cats = c(1, 10000000))
  expect_identical(stratetab(num)$body$c1, c("Exposure 1", "   1", "   10000000"))
  f <- st_block(cats = factor(c("lo", "hi"), levels = c("lo", "hi")))
  expect_identical(stratetab(f)$body$c1[-1], c("   lo", "   hi"))
  skip_if_not_installed("haven")
  lab <- st_block(cats = haven::labelled(c(0, 1), c(No = 0, Yes = 1)))
  expect_identical(stratetab(lab)$body$c1[-1], c("   No", "   Yes"))
  # decode gives "" for a value without a label: a blank category.
  part <- st_block(cats = haven::labelled(c(0, 1), c(No = 0)))
  expect_error(stratetab(part), "Blank category labels")
  expect_error(stratetab(st_block(cats = c("A", NA))), "Blank category labels")
  expect_error(stratetab(st_block(cats = c("A", " A "))), "Duplicate category labels")
  # Missing numeric code without a label: Stata's "." (golden S05).
  expect_identical(stratetab(st_block(cats = c(1, NA)))$body$c1[3], "   .")
  # Later outcomes realign to outcome 1 by label.
  a <- st_block(cats = c("A", "B"), D = c(1, 2))
  b <- st_block(cats = c("B", "A"), D = c(20, 10))
  tt <- stratetab(list(a, b), outcomes = 2)
  expect_identical(tt$body$c5, c("", "10", "20"))
  # strata + by from tt_rates(): the first grouping column is the category
  # (stratetab.ado:358-367), so its repeats are an error, as in Stata.
  set.seed(4)
  d <- data.frame(t = rexp(200, 0.1), e = rbinom(200, 1, 0.3), s = rep(c("x", "y"), 100),
                  g = rep(c("u", "u", "v", "v"), 50))
  expect_error(stratetab(tt_rates(d, "t", "e", by = "g", strata = "s")), "Duplicate category labels")
})

test_that("adversarial blocks: zero events, zero person-time, empty and mismatched blocks", {
  # Zero events: rate 0 with missing bounds, shown "0.0 (\u2013)" as in
  # tabtools 2.1.14 (2.1.13: "0.0 (., .)"), a dash for the ratio.
  ref <- st_block(cats = c("A", "B"), D = c(10, 0), Y = c(1000, 1000), rate = c(0.01, 0),
                  lo = c(0.005, NA), hi = c(0.02, NA))
  cmp <- st_block(cats = c("A", "B"), D = c(0, 5), Y = c(1000, 1000), rate = c(0, 0.005),
                  lo = c(NA, 0.002), hi = c(NA, 0.01))
  tt <- stratetab(list(ref, cmp), outcomes = 1, rateratio = TRUE)
  expect_identical(tt$body$c4, c("", "10.0 (5.0, 20.0)", "0.0 (\u2013)", "", "0.0 (\u2013)", "5.0 (2.0, 10.0)"))
  expect_identical(tt$body$c5, c("", "Ref.", "Ref.", "", "\u2013", "\u2013"))
  expect_true(all(is.na(tt$stored$ratios)))
  # Zero person-time: strate's rate is missing, and so are its bounds.
  zy <- st_block(cats = "A", D = 0, Y = 0, rate = NA_real_, lo = NA_real_, hi = NA_real_)
  # Stata 2.5.1 refuses a table without any source person-time.
  expect_error(stratetab(zy), class = "tabtools_error_rate_no_time")
  # A no-time category alongside a usable exposure is empty, not zero/dash.
  tt <- stratetab(list(st_block(cats = "A", D = 1, Y = 10), zy), outcomes = 1)
  expect_identical(unname(unlist(tt$body[4, ])), c("   A", "", "", ""))
  expect_true(is.na(tt$stored$rates[2, 1]))
  expect_identical(tt$stored$N_nopt, 1L)
  # Missing events with a rate: Stata's `missing > 0` passes, the bounds go missing.
  me <- st_block(cats = "A", D = NA_real_, Y = 10, rate = 0.5, lo = 0.2, hi = 0.9)
  tt <- stratetab(list(st_block(cats = "A", D = 4, Y = 10, rate = 0.4, lo = 0.2, hi = 0.8), me),
                  outcomes = 1, rateratio = TRUE, ratescale = 1)
  expect_identical(tt$body$c5[4], "1.25 (., .)")
  expect_identical(tt$body$c2[4], ".")
  # A block with no rows: the exposure keeps its header row only.
  empty <- st_block()[0, ]
  tt <- stratetab(list(st_block(), empty), outcomes = 1, level = 95)
  expect_identical(tt$body$c1, c("Exposure 1", "   A", "   B", "Exposure 2"))
  expect_identical(rownames(tt$stored$rates), c("e1_A", "e1_B"))
  expect_identical(tt$stored$N_rows, 7)
  # An exposure level missing from one outcome's block.
  expect_error(stratetab(list(st_block(), st_block(cats = "A", D = 1, Y = 10)), outcomes = 2),
               "Category count mismatch for exposure 1")
  expect_error(stratetab(list(st_block(), st_block(cats = c("A", "C"))), outcomes = 2),
               "Expected category \"B\"")
  # ... or from the reference exposure, under rateratio.
  expect_error(stratetab(list(st_block(), st_block(cats = c("A", "C"))), outcomes = 1, rateratio = TRUE),
               "No unique match for category \"C\"")
  expect_no_error(stratetab(list(st_block(), st_block(cats = c("A", "C"))), outcomes = 1))
})

test_that("a single outcome and extreme scales", {
  tt <- stratetab(st_block(), outlabels = "Death", title = "One")
  expect_identical(ncol(tt$body), 4L)
  expect_identical(tt$header[[1]]$text, c("Exposure", "Death", "", ""))
  expect_identical(colnames(tt$stored$rates), "Death")
  # Stata 2.5.1 uses %24.df for rates (stratetab.ado:778-790), so the
  # exact powers of ten below remain fixed notation; IRRs still use %11.
  big <- stratetab(st_block(), ratescale = 1e12, digits = 3)
  expect_identical(big$body$c4[2], "10000000000.000 (5000000000.000, 20000000000.000)")
  # Tiny scales round to zero; an overflowing product is missing (a
  # missing bound, too, so the cell has no interval: stratetab.ado:643).
  expect_identical(stratetab(st_block(), ratescale = 1e-10)$body$c4[2], "0.0 (0.0, 0.0)")
  huge <- stratetab(st_block(rate = c(1e10, 1e10), lo = c(1, 1), hi = c(1e11, 1e11)), ratescale = 1e300)
  expect_identical(huge$body$c4[2], ". (\u2013)")
  expect_true(is.na(huge$stored$rates[1, 1]))
  expect_identical(stratetab(st_block(), pyscale = 1e-6)$body$c3[2], "1,000,000,000")
  # pyscale divides person-years only; ratescale multiplies rates only.
  s <- stratetab(st_block(), pyscale = 1000, ratescale = 100, unitlabel = "100")
  expect_identical(unname(unlist(s$body[2, ])), c("   A", "10", "1", "1.0 (0.5, 2.0)"))
  expect_identical(s$header[[2]]$text[4], "Per 100 PY (95% CI)")
})

test_that("stored results, frame characteristics, and rate rows", {
  a <- st_block(D = c(10, 20))
  b <- st_block(D = c(5, 8))
  tt <- stratetab(list(a, b, a, b), outcomes = 2, rateratio = TRUE, outlabels = c("Death", "MI"),
                  outcomeids = c("death", "mi"), explabels = c("Men", "Women"))
  s <- tt$stored
  expect_identical(s[c("N_rows", "N_exposures", "N_outcomes", "ci_level", "outcome_ids")],
                   list(N_rows = 9, N_exposures = 2L, N_outcomes = 2L, ci_level = 95, outcome_ids = "death \\ mi"))
  expect_identical(dimnames(s$rates), list(c("e1_A", "e1_B", "e2_A", "e2_B"), c("death", "mi")))
  expect_identical(dimnames(s$ratios), list(c("A", "B"), c("death", "mi")))
  expect_equal(unname(s$ratios[, 1]), c(1, 1))
  df <- as.data.frame(tt)
  expect_identical(attr(df, "source"), "stratetab")
  # A rate-ratio table names its IRR column (tabtools 2.1.14).
  expect_identical(attr(df, "statistic_ids"), "events person_years rate_ci irr_ci")
  expect_identical(attr(as.data.frame(stratetab(list(a, b), outcomes = 2)), "statistic_ids"),
                   "events person_years rate_ci")
  expect_identical(attr(df, "n_outcomes"), 2L)
  expect_identical(attr(df, "outcome_id"), c("death", "mi"))
  expect_identical(attr(df, "outcome_id_2"), "mi")
  expect_identical(attr(df, "outcome_label"), c("Death", "MI"))
  expect_identical(nrow(df), 2L + 6L)
  rr <- tt$meta$rate_rows
  expect_identical(nrow(rr), 8L)
  expect_named(rr, c("row", "exposure", "exposure_label", "category", "outcome", "outcome_id", "outcome_label",
                     "events", "person_years", "rate", "lower", "upper", "state", "ci_method", "category_code", "category_type",
                     "irr", "irr_lower", "irr_upper"))
  expect_identical(rr$state, rep("est", 8))
  expect_identical(rr$ci_method, rep("supplied", 8))
  expect_identical(rr$category_code, rep(c("A", "A", "B", "B"), 2))
  expect_identical(rr$category_type, rep("character", 8))
  expect_identical(rr$row, rep(c(2L, 3L, 5L, 6L), each = 2))
  # The body cells are these numbers as displayed.
  expect_identical(tt$body$c2[rr$row[rr$outcome == 1]], stata_fmt(rr$events[rr$outcome == 1], "%24.0fc"))
  expect_true(all(is.na(rr$irr[rr$exposure == 1])))
  expect_equal(rr$irr[rr$exposure == 2], c(1, 1, 1, 1))
  # Three or more exposures prefix the ratio rows too (stratetab.ado:753).
  t3 <- stratetab(list(a, a, a), outcomes = 1, rateratio = TRUE)
  expect_identical(rownames(t3$stored$ratios), c("e2_A", "e2_B", "e3_A", "e3_B"))
  expect_identical(colnames(t3$stored$ratios), "Outcome_1")
  # Matrix names: strtoname, fallbacks, and repeats.
  expect_identical(tabtools:::.st_matrix_names(c("a b", "a_b", "<>", "1-2"), paste0("row", 1:4)),
                   c("a_b", "a_b_2", "row3", "_1_2"))
})

test_that("returned visibly without files, invisibly with them; sinks and converters", {
  b <- st_block()
  expect_visible(stratetab(b))
  out <- withr::local_tempdir()
  expect_invisible(tt <- stratetab(b, xlsx = file.path(out, "r.xlsx"), csv = file.path(out, "r.csv"),
                                   markdown = file.path(out, "r.md")))
  expect_identical(tt$stored$sheet, "Results")
  # Data rows only (tabtools 2.1.14: one header row, datastart(4)).
  expect_identical(tt$stored$markdown_rows, 3L)
  expect_identical(tt$stored$markdown_cols, 4L)
  # An existing sheet keeps the workbook's spelling (stratetab.ado:812-814).
  stratetab(b, xlsx = file.path(out, "r.xlsx"), sheet = "RESULTS")
  expect_identical(stratetab(b, xlsx = file.path(out, "r.xlsx"), sheet = "RESULTS")$stored$sheet, "Results")
  # Markdown: one header row, "outcome: statistic" (tabtools 2.1.14; 2.1.13
  # wrote the outcome row as the header and the statistic row as data).
  md <- readLines(file.path(out, "r.md"))
  expect_identical(md[1:3], c("| Exposure | Outcome 1: Events | Outcome 1: Person-Years (PY) | Outcome 1: Per 1,000 PY (95% CI) |",
                              "| --- | --- | --- | --- |", "| Exposure 1 |  |  |  |"))
  # An existing Markdown file is replaced unless mdappend (tabtools 2.1.12).
  stratetab(b, markdown = file.path(out, "r.md"))
  expect_identical(sum(grepl("^\\| --- ", readLines(file.path(out, "r.md")))), 1L)
  stratetab(b, markdown = file.path(out, "r.md"), mdappend = TRUE)
  expect_identical(sum(grepl("^\\| --- ", readLines(file.path(out, "r.md")))), 2L)
  # puttab() of the table takes the Markdown header row.
  p <- puttab(stratetab(b))
  expect_identical(p$header[[1]]$text, c("Exposure", "Outcome 1: Events", "Outcome 1: Person-Years (PY)",
                                         "Outcome 1: Per 1,000 PY (95% CI)"))
  expect_identical(p$body$c2[1:2], c("", "10"))
  # Converters.
  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(stratetab(b, title = "T"))
  expect_s3_class(ft, "flextable")
  skip_if_not_installed("gt")
  g <- tt_as_gt(stratetab(b))
  expect_s3_class(g, "gt_tbl")
})

test_that("W01: explicit sheet without workbook gives a classed warning", {
  expect_warning(stratetab(st_block(), sheet = "Zed"), class = "tabtools_warning_sheet_without_workbook")
})

test_that("P2-2: sheet = NULL means the default sheet", {
  expect_no_error(stratetab(st_block(), sheet = NULL))
})
