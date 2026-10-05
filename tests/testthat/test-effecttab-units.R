# effecttab() and tt_effect_rows() units: argument checks, input readers,
# labels, layout, stored results, sinks, and adversarial inputs (plan tasks
# 7.7-7.10). The golden parity is in test-golden-effecttab.R.

te_df <- function(level = 0.95) {
  d <- data.frame(equation = c("ATE", "POmean", "TME1"),
                  term = c("r1vs0.smoke", "0.smoke", "age"),
                  estimate = c(-262.979, 3406.38, -0.02),
                  conf.low = c(-307.688, 3387.86, -0.04), conf.high = c(-218.27, 3424.9, -0.01),
                  p.value = c(9.5e-31, 0, 0.002))
  attr(d, "conf.level") <- level
  d
}
te_data <- function() {
  data.frame(smoke = structure(c(0, 1, 0, 1), labels = c(Nonsmoker = 0, Smoker = 1),
                               label = "mother_smoked", class = c("haven_labelled", "vctrs_vctr", "double")))
}
mg_df <- function() {
  d <- data.frame(term = c("1b.race", "2.race", "3.race", "age"),
                  estimate = c(0, 0.0319, 0.0146, 0.0025),
                  conf.low = c(NA, 0.0161, -0.0217, 0.0021), conf.high = c(NA, 0.0478, 0.0510, 0.0029),
                  p.value = c(NA, 7.7e-5, 0.43, 1e-30))
  attr(d, "conf.level") <- 0.95
  d
}
race_data <- function() {
  data.frame(race = structure(c(1, 2, 3), labels = c(White = 1, Black = 2, Other = 3), label = "Race",
                              class = c("haven_labelled", "vctrs_vctr", "double")),
             age = structure(c(20, 30, 40), label = "Age (years)"))
}

test_that("teffects data frame: raw keys, clean labels, tlabels, equations filtered", {
  tt <- effecttab(te_df())
  expect_identical(tt$body$c1, c("r1vs0.smoke", "0.smoke"))
  expect_identical(tt$header[[2]]$text, c("", "Effect", "95% CI", "p-value"))
  expect_identical(tt$header[[1]]$text, rep("", 4))
  expect_identical(tt$stored$type, "teffects")
  expect_identical(tt$body$c2, c("-262.98", "3406.38"))
  expect_identical(tt$body$c4, c("<0.001", "<0.001"))
  # Value labels from `data`; the variable label capitalised with "_" shown as spaces.
  expect_identical(effecttab(te_df(), data = te_data(), clean = TRUE)$body$c1,
                   c("Smoker vs Nonsmoker", "Nonsmoker (PO Mean)"))
  expect_identical(effecttab(te_df(), clean = TRUE)$body$c1, c("Smoke (1 vs 0)", "Smoke = 0 (PO Mean)"))
  d <- te_data()
  attr(d$smoke, "labels") <- NULL
  expect_identical(effecttab(te_df(), data = d, clean = TRUE)$body$c1,
                   c("Mother smoked (1 vs 0)", "Mother smoked = 0 (PO Mean)"))
  # tlabels implies clean and replaces the value labels; a level it does not name keeps its code.
  expect_identical(effecttab(te_df(), data = te_data(), tlabels = c("1" = "Smoker"))$body$c1,
                   c("Smoker vs 0", "Mother smoked = 0 (PO Mean)"))
  expect_identical(effecttab(te_df(), tlabels = '0 "Never" 1 "Ever"')$body$c1,
                   c("Ever vs Never", "Never (PO Mean)"))
  expect_error(effecttab(te_df(), tlabels = '0 "Never" 1'), "pair each level")
  expect_error(effecttab(te_df(), tlabels = c("Never", "Ever")), "name")
})

test_that("teffects rows come contrasts first, whatever the input order", {
  d <- te_df()[c(2, 1), ]
  expect_identical(effecttab(d, level = 95)$body$c1, c("r1vs0.smoke", "0.smoke"))
  expect_error(effecttab(te_df()[3, ]), "No treatment-effect equations")
  d <- te_df()[3, ]
  d$equation <- ""
  expect_error(effecttab(d, type = "teffects"), "does not match")
  # A bare level could be a potential-outcome mean: no teffects rows left.
  d$term <- "2.x"
  d$equation <- "OME2"
  expect_error(effecttab(d), "No treatment-effect equations")
  d$equation <- ""
  expect_identical(effecttab(d, type = "teffects", level = 95)$body$c1, "2.x")
})

test_that("margins data frame: factor parent, Reference row, labels from data or raw keys", {
  tt <- effecttab(mg_df(), data = race_data(), effect = "AME")
  expect_identical(tt$body$c1, c("Race", "  White", "  Black", "  Other", "Age (years)"))
  expect_identical(tt$body$c2, c("", "Reference", "0.03", "0.01", "0.00"))
  expect_identical(tt$rows$type, c("cat_header", "ref", "level", "level", "var"))
  expect_identical(tt$stored$type, "margins")
  # Without the data Stata cannot find the variable: raw keys, the heading is its name.
  expect_identical(effecttab(mg_df())$body$c1, c("race", "1.race", "2.race", "3.race", "age"))
  # r(table): rows with a number, estimate and p per model, names from the labels.
  expect_identical(rownames(tt$stored$table), c("__Black", "__Other", "Age_(years)"))
  expect_identical(colnames(tt$stored$table), c("c1", "c2"))
  expect_equal(unname(tt$stored$table[, 1]), c(0.0319, 0.0146, 0.0025))
  expect_identical(tt$stored$N_rows, 8)
  expect_identical(tt$stored$N_cols, 5)
  expect_identical(tt$stored$effect_label, "AME")
  expect_identical(tt$stored$methods, "Marginal effects estimated using the margins command with 95% confidence intervals.")
})

test_that("omitted and empty cells, and constrained labels that must differ", {
  d <- data.frame(term = c("age", "o.age2", "3.race"), estimate = c(0.1, 0, NA),
                  conf.low = c(0.05, NA, NA), conf.high = c(0.15, NA, NA), p.value = c(0.01, NA, NA),
                  status = c("est", "omit", "empty"))
  tt <- effecttab(d, emptylabel = "n/a", level = 95)
  expect_identical(tt$body$c1, c("age", "o.age2", "race", "3.race"))
  expect_identical(tt$body$c2, c("0.10", "Omitted", "", "n/a"))
  expect_identical(tt$rows$type, c("var", "var", "cat_header", "empty"))
  expect_error(effecttab(d, refcat = "Omitted", level = 95), "must differ")
  # A zero estimate without an interval is never taken for a Reference row.
  z <- data.frame(term = "2.race", estimate = 0, p.value = NA_real_)
  expect_identical(suppressMessages(effecttab(z))$body$c2, c("", "0.00"))
})

test_that("matrix input: from() headers, text round trip, zero kept, labels, level", {
  m <- matrix(c(0.25, 0.1, 0.15, 0.1, -0.001, -0.004, 0.002, 0.001, 0, -0.1, 0.1, 1), 3, byrow = TRUE,
              dimnames = list(c("Tie_low", "Near_zero", "Zero_row"), NULL))
  tt <- effecttab(m, digits = 1)
  expect_identical(tt$header[[2]]$text, c("", "Estimate", "95% CI", "p-value"))
  expect_identical(tt$body$c1, c("Tie low", "Near zero", "Zero row"))
  expect_identical(tt$body$c2, c("0.2", "0.0", "0.0"))
  # A matrix bound is formatted, read back and formatted again: no -0.0.
  expect_identical(tt$body$c3, c("(0.1, 0.1)", "(0.0, 0.0)", "(-0.1, 0.1)"))
  expect_identical(tt$body$c4, c("0.10", "0.001", "1.00"))
  expect_identical(tt$stored$type, "margins")
  expect_identical(tt$stored$methods,
                   "Effect estimates were formatted from the supplied matrix with 95% confidence intervals and p-values.")
  # Full-precision estimates in r(table) (Stata stores the displayed ones).
  expect_equal(unname(tt$stored$table[, 1]), c(0.25, -0.001, 0))
  expect_identical(effecttab(m, level = 0.9)$header[[2]]$text[3], "90% CI")
  # No message for a matrix without level: from() uses 95 silently.
  expect_no_message(effecttab(m))
  expect_identical(effecttab(unname(m))$body$c1, c("r1", "r2", "r3"))
  expect_identical(effecttab(m, type = "teffects")$stored$effect_label, "Effect")
  expect_error(effecttab(m[, 1:3]), "at least 4 columns")
  expect_error(effecttab(matrix(letters[1:4], 1)), "numeric")
  expect_error(effecttab(m, te_df()), "cannot be combined")
  expect_error(effecttab(list(list(te_df(), m))), "a model of its own")
  # Repeated row names keep their rows apart.
  mm <- rbind(m[1, ], m[1, ])
  rownames(mm) <- c("A", "A")
  expect_identical(effecttab(mm)$body$c1, c("A", "A"))
})

test_that("tabtools 2.1.12: from() headers and repeated row names", {
  m <- matrix(c(1, 0, 2, 0.5, 2, 1, 3, 0.1), 2, byrow = TRUE, dimnames = list(c("A", "A"), NULL))
  tt <- effecttab(m)
  # 2.1.11's matrix-only "(95% CI)"/"p" headers are gone (golden E18, E24).
  expect_identical(tt$header[[2]]$text[3:4], c("95% CI", "p-value"))
  expect_identical(effecttab(m, level = 90)$header[[2]]$text[3], "90% CI")
  expect_identical(rownames(tt$stored$table), c("A", "A_2"))
})

test_that("confidence level: provenance, agreement, the message without one", {
  expect_identical(effecttab(te_df(0.9))$header[[2]]$text[3], "90% CI")
  expect_identical(effecttab(te_df(0.9), level = 90)$stored$ci_level, 90)
  expect_identical(effecttab(te_df(0.9), level = 0.9)$stored$ci_level, 90)
  expect_error(effecttab(te_df(0.9), level = 95), "conflicts")
  expect_error(effecttab(te_df(0.9), te_df(0.95)), "different confidence levels")
  d <- te_df()
  attr(d, "conf.level") <- NULL
  expect_message(tt <- effecttab(d), "assuming 95%")
  expect_identical(tt$stored$ci_level, 95)
  expect_no_message(effecttab(d, level = 99.9))
  # No floating-point noise in the header: string(level, "%21.15g"), as
  # tabtools 2.1.12 (2.1.11 printed 99.90000000000001).
  expect_identical(effecttab(d, level = 99.9)$header[[2]]$text[3], "99.9% CI")
  expect_match(effecttab(d, level = 99.9)$stored$methods, " 99.9%", fixed = TRUE)
  d$conf.level <- 0.9
  expect_identical(effecttab(d)$stored$ci_level, 90)
  expect_error(effecttab(d, level = 150), "proportion")
})

test_that("models: labels, named models, a single list unpacked, too many labels", {
  tt <- effecttab(A = te_df(), B = te_df(), models = NULL)
  expect_identical(tt$header[[1]]$text, c("", "A", "", "", "B", "", ""))
  expect_identical(effecttab(list(te_df(), te_df()), models = "IPW \\ AIPW")$header[[1]]$text[c(2, 5)],
                   c("IPW", "AIPW"))
  expect_warning(tt <- effecttab(te_df(), models = c("a", "b")), "extra labels")
  expect_identical(tt$header[[1]]$text[2], "a")
  expect_identical(effecttab(te_df(), te_df())$stored$methods,
                   "Effect estimates from multiple collected models were formatted with 95% confidence intervals.")
  expect_error(effecttab(te_df(), sheeet = "x"), "no argument")
  expect_error(effecttab(), "at least one")
  expect_error(effecttab(lm(mpg ~ wt, mtcars)), "cannot read")
  expect_error(effecttab(list(list(te_df(), 1))), "cannot read")
})

test_that("row union across models: blank cells, a new level joins its factor", {
  a <- data.frame(term = c("1b.g", "2.g", "age"), estimate = c(0, 1, 2), p.value = c(NA, 0.5, 0.5))
  b <- data.frame(term = c("age", "3.g"), estimate = c(3, 4), p.value = c(0.5, 0.5))
  tt <- suppressMessages(effecttab(a, b))
  expect_identical(tt$body$c1, c("g", "1.g", "2.g", "3.g", "age"))
  expect_identical(tt$body$c2, c("", "Reference", "1.00", "", "2.00"))
  expect_identical(tt$body$c5, c("", "", "", "4.00", "3.00"))
  expect_identical(dim(tt$stored$table), c(3L, 4L))
  expect_true(is.na(tt$stored$table["3_g", "c1"]))
  expect_error(suppressMessages(effecttab(list(list(a, a)))), "more than once")
})

test_that("teffects and margins cannot mix; an explicit type must match", {
  expect_error(effecttab(te_df(), mg_df()), "mixing teffects and margins")
  expect_error(effecttab(mg_df(), type = "teffects"), "does not match")
  expect_error(effecttab(te_df(), type = "margins"), "does not match")
  k <- data.frame(kind = "contrast", variable = "t", level = 1, base = 0, estimate = 1, p.value = 0.1)
  expect_identical(suppressMessages(effecttab(k))$stored$type, "teffects")
  expect_error(suppressMessages(effecttab(data.frame(kind = "bogus", variable = "t", estimate = 1))), "kind")
  expect_error(suppressMessages(effecttab(data.frame(kind = "contrast", variable = "t", estimate = 1))),
               "needs")
})

test_that("data-frame checks", {
  expect_error(effecttab(data.frame(term = "a")), "estimate")
  expect_error(effecttab(data.frame(x = 1, estimate = 1)), "term")
  expect_error(effecttab(data.frame(term = "a", estimate = "1")), "numeric")
  expect_error(effecttab(data.frame(term = "a", estimate = 1, conf.low = 0)), "both")
  expect_error(effecttab(data.frame(term = "a", estimate = 1, p.value = 2)), "\\[0, 1\\]")
  expect_error(effecttab(data.frame(term = "a", estimate = 1, status = "x")), "status")
  expect_error(effecttab(data.frame(term = c("a", "a"), estimate = 1:2)), "more than once")
  expect_error(effecttab(data.frame(term = "a", estimate = 1, conf.level = c(0.9))[c(1, 1), ][1, , drop = FALSE],
                         level = 95), "conflicts")
  # A standard error gives a Wald interval and p-value.
  tt <- suppressMessages(effecttab(data.frame(term = "a", estimate = 1, std.error = 0.5)))
  expect_identical(tt$body$c3, "(0.02, 1.98)")
  expect_identical(tt$body$c4, "0.046")
  # A label column labels the row as it is.
  expect_identical(suppressMessages(effecttab(data.frame(label = "My effect", estimate = 1)))$body$c1, "My effect")
  # All rows empty: nothing to show.
  expect_error(suppressMessages(effecttab(data.frame(term = "a", estimate = NA_real_))), "No effect rows")
})

test_that("Stata keys: interactions keep their key under the factor heading; r(table) names", {
  d <- data.frame(term = c("1bn.sex#0bn.hi", "1bn.sex#1.hi", "1._at"), estimate = c(0.04, 0.05, 0.02),
                  p.value = c(0.01, 0.01, 0.01))
  tt <- suppressMessages(effecttab(d))
  expect_identical(tt$body$c1, c("sex#hi", "1.sex#0.hi", "1.sex#1.hi", "_at", "1._at"))
  expect_identical(rownames(tt$stored$table), c("c.1_sex#c.0_hi", "c.1_sex#c.1_hi", "1__at"))
  expect_identical(tabtools:::.et_rowname(c("a b", "x[1]y[2]"), 1:2), c("r1", "r2"))
  expect_identical(tabtools:::.et_rowname(c("a, b.c", ""), 1:2), c("a_b_c", "row2"))
  expect_identical(tabtools:::.et_factor_parent("2.a#c.x"), "a#x")
  expect_true(is.na(tabtools:::.et_factor_parent("x#y")))
})

test_that("layout: console skips the blank model row; CSV and Markdown; widths; boldp", {
  tt <- effecttab(te_df(), title = "T")
  lines <- utils::capture.output(print(tt))
  expect_identical(lines[2], "T")
  expect_false(any(grepl("^  \\|\\s+\\|$", lines)))
  expect_length(grep("^  \\|", lines), 3L)
  tt2 <- effecttab(te_df(), models = "IPW")
  expect_length(grep("^  \\|", utils::capture.output(print(tt2))), 4L)
  out <- withr::local_tempdir()
  csv <- file.path(out, "a.csv")
  md <- file.path(out, "a.md")
  xl <- file.path(out, "a.xlsx")
  res <- effecttab(te_df(), models = "IPW", csv = csv, markdown = md, xlsx = xl, footnote = "F")
  expect_identical(readLines(csv)[1:2], c(",IPW,,", ",Effect,95% CI,p-value"))
  expect_identical(readLines(csv)[5], "F,,,")
  expect_identical(readLines(md)[1], "|  | IPW: Effect | IPW: 95% CI | IPW: p-value |")
  expect_identical(res$stored$markdown_rows, 2L)
  expect_identical(res$stored$sheet, "Effects")
  # Without models the blank model row is dropped from the CSV (reservedrow).
  effecttab(te_df(), csv = file.path(out, "b.csv"))
  expect_identical(readLines(file.path(out, "b.csv"))[1], ",Effect,95% CI,p-value")
  # The label column fits the effect header (X2); _tabtools_colwidth widths (X1).
  lay <- tabtools:::.xlsx_layout_regtab(effecttab(data.frame(term = "a", estimate = 1, p.value = 0.5),
                                                  effect = "A very long effect header label", level = 95))
  w <- lay$rules[lay$rules$op == 13L, ]
  expect_identical(w$value[w$c1 == 2], ceiling(nchar("A very long effect header label") * 0.85) + 2)
  expect_identical(w$value[w$c1 == 3], 22)
  expect_identical(w$value[w$c1 == 4], 16)
  expect_identical(w$value[w$c1 == 5], 8)
  # boldp reads the p text back: ">0.99" is missing, so never bold.
  d <- data.frame(term = c("a", "b"), estimate = 1:2, p.value = c(0.995, 0.0001))
  lay <- tabtools:::.xlsx_layout_regtab(effecttab(d, boldp = 0.999, level = 95))
  bold <- lay$rules[lay$rules$op == 2L & lay$rules$c1 == 5L & lay$rules$r1 >= 4, ]
  expect_identical(bold$r1, 5)
  expect_error(effecttab(te_df(), open = TRUE), "requires")
  expect_error(effecttab(te_df(), xlsx = "a.xls"), "xlsx")
  expect_error(effecttab(te_df(), markdown = "a.txt"), "markdown")
  expect_error(effecttab(te_df(), mdappend = TRUE), "requires")
})

test_that("formatting options: digits, sep, pdp/highpdp, addrow, digits option", {
  tt <- effecttab(te_df(), digits = 3, sep = "; ", pdp = 4, highpdp = 3,
                  addrow = list(N = c(4642)))
  expect_identical(tt$body$c2, c("-262.979", "3406.380", "4642"))
  expect_identical(tt$body$c3[1], "(-307.688; -218.270)")
  expect_identical(tt$body$c4[1:2], c("<0.0001", "<0.0001"))
  expect_identical(tt$rows$addrow, c(FALSE, FALSE, TRUE))
  expect_identical(tt$stored$N_rows, 6)
  expect_identical(nrow(tt$stored$table), 2L)
  withr::local_options(tabtools.digits = 1)
  expect_identical(effecttab(te_df())$body$c2, c("-263.0", "3406.4"))
  expect_identical(effecttab(te_df(), sep = "")$body$c3[1], "(-307.7, -218.3)")
  expect_error(effecttab(te_df(), digits = 7), "between 0 and 6")
  expect_error(effecttab(te_df(), method = "gmm"), "method")
  expect_identical(effecttab(te_df(), method = "AIPW")$stored$methods,
                   "Average treatment effects estimated using augmented inverse probability weighting with 95% confidence intervals.")
  expect_identical(effecttab(te_df())$stored$methods,
                   "Average treatment effects were formatted with 95% confidence intervals.")
  d <- te_df()
  attr(d, "tt_estimator") <- "ra"
  expect_match(effecttab(d)$stored$methods, "regression adjustment")
})

test_that("frame characteristics and as_forest_data()", {
  tt <- effecttab(mg_df(), mg_df()[4, ], data = race_data(), models = c("Race", ""), addrow = list(N = c(1, 2)))
  df <- as.data.frame(tt)
  expect_identical(attr(df, "source"), "effecttab")
  expect_identical(attr(df, "n_models"), 2L)
  expect_identical(attr(df, "outcome_id"), c("", ""))
  expect_identical(attr(df, "effect_scale"), c("Estimate", "Estimate"))
  expect_identical(attr(df, "model_label"), c("Race", ""))
  fd <- as_forest_data(tt)
  expect_identical(fd$rowtype, c("reference", "effect", "effect", "effect", "effect"))
  expect_identical(fd$model_label, c("Race", "Race", "Race", "Race", "Model 2"))
  expect_false(any(fd$label == "N"))
  expect_equal(fd$estimate[fd$label == "  Black"], 0.0319)
  expect_identical(attr(fd, "statistic_ids"), "estimate ci pvalue")
})

test_that("tt_effect_rows() contract", {
  r <- tt_effect_rows(te_df(), type = "teffects", data = te_data())
  expect_identical(r$kind, c("contrast", "pomean"))
  expect_identical(r$level_text, c("Smoker", "Nonsmoker"))
  expect_identical(attr(r, "source"), "data.frame")
  expect_identical(attr(r, "level"), 95)
  expect_identical(names(r), tabtools:::.et_cols)
  expect_error(tt_effect_rows(1:3), "cannot read")
})

# ---------------------------------------------------------------------------
# marginaleffects results

me_setup <- function() {
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Automatic", "Manual"))
  attr(d$am, "label") <- "Transmission"
  attr(d$wt, "label") <- "Weight (1000 lbs)"
  d$cyl <- factor(d$cyl)
  list(d = d, fit = glm(mpg ~ am + wt + cyl, data = d))
}

test_that("marginaleffects margins: factor rows, continuous rows, request order, level", {
  skip_if_not_installed("marginaleffects")
  s <- me_setup()
  cmp <- marginaleffects::avg_comparisons(s$fit, variables = "am")
  tt <- effecttab(cmp)
  expect_identical(tt$body$c1, c("Transmission", "  Automatic", "  Manual"))
  expect_identical(tt$body$c2[2], "Reference")
  sl <- marginaleffects::avg_slopes(s$fit, variables = c("wt", "am"))
  expect_identical(effecttab(sl)$body$c1, c("Weight (1000 lbs)", "Transmission", "  Automatic", "  Manual"))
  pr <- marginaleffects::avg_predictions(s$fit, variables = "cyl")
  expect_identical(effecttab(pr)$body$c1, c("cyl", "  4", "  6", "  8"))
  expect_identical(effecttab(pr)$stored$methods,
                   "Marginal effects estimated using the marginaleffects package with 95% confidence intervals.")
  pr90 <- marginaleffects::avg_predictions(s$fit, variables = "cyl", conf_level = 0.9)
  expect_identical(effecttab(pr90)$stored$ci_level, 90)
  expect_error(effecttab(pr90, level = 95), "conflicts")
  expect_identical(effecttab(marginaleffects::avg_predictions(s$fit))$body$c1, "Overall")
  pb <- marginaleffects::avg_predictions(s$fit, variables = "am", by = c("cyl", "am"))
  expect_identical(effecttab(pb)$body$c1[1:2], c("cyl#am", "  4#Automatic"))
  # A labelled key for the level rows: tt_as_factor() codes, else positions.
  expect_identical(tt_effect_rows(cmp, type = "margins")$key, c("1.am", "2.am"))
})

test_that("marginaleffects teffects: a contrast with its potential-outcome mean", {
  skip_if_not_installed("marginaleffects")
  s <- me_setup()
  ate <- marginaleffects::avg_comparisons(s$fit, variables = "am")
  po <- marginaleffects::avg_predictions(s$fit, variables = list(am = "Automatic"))
  tt <- effecttab(list(list(po, ate)))
  expect_identical(tt$stored$type, "teffects")
  expect_identical(tt$body$c1, c("r2vs1.am", "1.am"))
  expect_identical(effecttab(list(list(ate, po)), clean = TRUE)$body$c1,
                   c("Manual vs Automatic", "Automatic (PO Mean)"))
  # tlabels may name a plain factor's level by the level itself.
  expect_identical(effecttab(list(list(ate, po)), tlabels = c(Automatic = "Auto", Manual = "Man"))$body$c1,
                   c("Man vs Auto", "Auto (PO Mean)"))
  # A level tlabels does not name keeps its code; a plain factor's code is
  # its position, so it keeps its level (review F18).
  expect_identical(effecttab(list(list(ate, po)), tlabels = c(Automatic = "Auto"))$body$c1,
                   c("Manual vs Auto", "Auto (PO Mean)"))
  expect_identical(effecttab(IPW = list(ate, po))$header[[1]]$text[2], "IPW")
  expect_identical(effecttab(ate, type = "teffects")$body$c1, "r2vs1.am")
  expect_error(effecttab(marginaleffects::avg_slopes(s$fit, variables = "wt"), type = "teffects"),
               "does not match")
  expect_error(tt_effect_rows(marginaleffects::avg_slopes(s$fit, variables = "wt"), type = "teffects"),
               "teffects counterpart")
  expect_error(effecttab(marginaleffects::avg_predictions(s$fit), type = "teffects"), "treatment level")
})

test_that("marginaleffects refusals: unit-level results, by subgroups, other classes", {
  skip_if_not_installed("marginaleffects")
  s <- me_setup()
  expect_error(effecttab(marginaleffects::predictions(s$fit)), "averaged results")
  expect_error(effecttab(marginaleffects::avg_comparisons(s$fit, variables = "wt", by = "am")), "by")
  # hypotheses() output is a data frame of terms: formatted as one.
  expect_identical(effecttab(marginaleffects::hypotheses(s$fit), level = 95)$body$c1[1], "(Intercept)")
  # A subset of a result keeps its rows and its recorded level.
  pr <- marginaleffects::avg_predictions(s$fit, variables = "am")
  expect_no_message(tt <- effecttab(pr[pr$am == "Automatic", ]))
  expect_identical(tt$body$c1, c("Transmission", "  Automatic"))
})

test_that("contrast strings split at the side both levels name", {
  expect_identical(tabtools:::.et_split_contrast("mean(1) - mean(0)"), c(level = "1", base = "0"))
  expect_identical(tabtools:::.et_split_contrast("B / A"), c(level = "B", base = "A"))
  expect_identical(tabtools:::.et_split_contrast("10 - 20 - 5 - 9", c("10 - 20", "5 - 9")),
                   c(level = "10 - 20", base = "5 - 9"))
  expect_null(tabtools:::.et_split_contrast("dY/dX"))
  expect_null(tabtools:::.et_split_contrast("+1"))
})

# ---------------------------------------------------------------------------
# Review 7d findings (REVIEW.md F1-F22) and mutation survivors

test_that("F1: intervals derived from std.error are built at the requested level (z or t)", {
  df <- data.frame(term = "x", estimate = 1, std.error = 0.5)
  tt <- effecttab(df, level = 90)
  expect_identical(tt$header[[2]]$text[3], "90% CI")
  expect_identical(tt$body$c3, "(0.18, 1.82)")
  expect_identical(effecttab(df, level = 0.99)$body$c3, "(-0.29, 2.29)")
  # Without `level`: 95%, and no "assuming" message, since the interval was built here.
  expect_no_message(tt <- effecttab(df))
  expect_identical(tt$body$c3, "(0.02, 1.98)")
  # A frame that states its level builds at it, and `level` must agree.
  df90 <- df
  attr(df90, "conf.level") <- 0.9
  expect_identical(effecttab(df90)$body$c3, "(0.18, 1.82)")
  expect_error(effecttab(df90, level = 95), "conflicts")
  # Student's t with a df column (regtab's H1 rule); Inf is the normal.
  tdf <- data.frame(term = c("a", "b"), estimate = 1, std.error = 0.5, df = c(10, Inf))
  tt <- effecttab(tdf, level = 95, digits = 3)
  q <- stats::qt(0.975, 10)
  expect_identical(tt$body$c3[1], sprintf("(%.3f, %.3f)", 1 - q * 0.5, 1 + q * 0.5))
  expect_identical(tt$body$c3[2], "(0.020, 1.980)")
  expect_identical(tt$body$c4[1], sprintf("%.3f", 2 * stats::pt(-2, 10)))
  expect_error(effecttab(data.frame(term = "a", estimate = 1, std.error = 0.5, df = 0)), "positive")
  expect_error(effecttab(data.frame(term = "a", estimate = 1, std.error = -1)), "positive")
  # A supplied interval is kept as it is; a frame mixing supplied and
  # derived intervals records no level (the message).
  mix <- data.frame(term = c("a", "b"), estimate = 1, std.error = 0.5, conf.low = c(0, NA), conf.high = c(2, NA))
  expect_message(tt <- effecttab(mix), "assuming 95%")
  expect_identical(tt$body$c3, c("(0.00, 2.00)", "(0.02, 1.98)"))
})

test_that("F2: a plain ratio's p-value tests 1, with a footnote; hypothesis = 1 is kept", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  fit <- glm(vs ~ am + wt, family = binomial, data = d)
  r <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio")
  tt <- effecttab(r, effect = "RR")
  z <- (r$estimate - 1) / r$std.error
  expect_equal(unname(tt$stored$table[1, 2]), 2 * stats::pnorm(-abs(z)))
  expect_false(isTRUE(all.equal(unname(tt$stored$table[1, 2]), r$p.value)))
  expect_identical(tt$footnote, "P-values of ratios test a ratio of 1.")
  expect_identical(effecttab(r, footnote = "Weighted")$footnote, "Weighted; P-values of ratios test a ratio of 1.")
  # The interval is marginaleffects'.
  expect_identical(tt$body$c3[3], sprintf("(%.2f, %.2f)", r$conf.low, r$conf.high))
  # as_forest_data() and highlight read the corrected p-value.
  expect_equal(as_forest_data(tt)$pvalue[2], 2 * stats::pnorm(-abs(z)))
  # Stated against 1 by the user: kept, no footnote.
  h1 <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio", hypothesis = 1)
  t1 <- effecttab(h1, effect = "RR")
  expect_identical(t1$footnote, "")
  expect_equal(unname(t1$stored$table[1, 2]), h1$p.value)
  # A finite df (marginaleffects' `df =`) gives Student's t.
  r10 <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio", df = 10)
  z10 <- (r10$estimate - 1) / r10$std.error
  expect_equal(unname(effecttab(r10, effect = "RR")$stored$table[1, 2]), 2 * stats::pt(-abs(z10), 10))
  # Any other numeric null is named.
  h2 <- marginaleffects::avg_comparisons(fit, variables = "am", hypothesis = 0.1)
  expect_identical(effecttab(h2)$footnote, "P-values test a null value of 0.1.")
})

test_that("F6: lnratioavg + transform = exp is a teffects contrast, p from the log scale", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  fit <- glm(vs ~ am + wt, family = binomial, data = d)
  rr <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnratioavg", transform = exp)
  po <- marginaleffects::avg_predictions(fit, variables = list(am = "Auto"))
  tt <- suppressMessages(effecttab(list(list(rr, po)), clean = TRUE, effect = "RR"))
  expect_identical(tt$body$c1, c("Man vs Auto", "Auto (PO Mean)"))
  expect_equal(unname(tt$stored$table[1, ]), c(rr$estimate, rr$p.value))
  expect_identical(tt$body$c3[1], sprintf("(%.2f, %.2f)", rr$conf.low, rr$conf.high))
  expect_identical(tt$footnote, "")
  # Margins mode keeps the factor structure of an exponentiated odds ratio.
  or <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnoravg", transform = exp)
  expect_identical(effecttab(or)$body$c1, c("am", "  Auto", "  Man"))
  expect_identical(tabtools:::.et_split_contrast("ln(odds(Man) / odds(Auto))"), c(level = "Man", base = "Auto"))
})

test_that("F3/F12: hypothesis tests and hypotheses() are labelled by the hypothesis, keeping their level", {
  skip_if_not_installed("marginaleffects")
  withr::local_options(marginaleffects_safe = FALSE)
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  fit <- glm(vs ~ am + wt + hp, family = binomial, data = d)
  p1 <- marginaleffects::avg_predictions(fit, variables = "am", hypothesis = "b2 - b1 = 0")
  expect_identical(effecttab(p1)$body$c1, "b2 - b1")
  p2 <- marginaleffects::avg_predictions(fit, variables = "am", hypothesis = ~pairwise)
  expect_identical(effecttab(p2)$body$c1, "Man - Auto")
  c1 <- marginaleffects::avg_comparisons(fit, variables = "am", hypothesis = "b1 = 0")
  expect_identical(effecttab(c1)$body$c1, "b1")
  h <- marginaleffects::hypotheses(fit, "wt = hp", conf_level = 0.9)
  expect_no_message(tt <- effecttab(h))
  expect_identical(tt$body$c1, "wt = hp")
  expect_identical(tt$stored$ci_level, 90)
  expect_identical(effecttab(marginaleffects::hypotheses(fit))$body$c1[1], "(Intercept)")
  expect_error(effecttab(p1, type = "teffects"), "does not match")
  expect_error(tt_effect_rows(p1, type = "teffects"), "neither a treatment contrast")
  expect_identical(tabtools:::.et_hyp_label(c("b2-b1=0", "b1=1", "(A) - (B)", "a - b = 0")),
                   c("b2 - b1", "b1 = 1", "A - B", "a - b"))
})

test_that("F8/M09: contrasts follow level order in both modes (levels not alphabetical)", {
  skip_if_not_installed("marginaleffects")
  set.seed(1)
  d <- data.frame(dose = factor(sample(c("Placebo", "Low", "High"), 300, TRUE), c("Placebo", "Low", "High")),
                  x = rnorm(300))
  d$y <- rnorm(300, as.integer(d$dose) + d$x)
  fit <- lm(y ~ dose + x, data = d)
  cmp <- marginaleffects::avg_comparisons(fit, variables = "dose", vcov = "HC0")
  po <- marginaleffects::avg_predictions(fit, variables = list(dose = "Placebo"), vcov = "HC0")
  expect_identical(effecttab(list(list(cmp, po)))$body$c1, c("r2vs1.dose", "r3vs1.dose", "1.dose"))
  expect_identical(effecttab(cmp)$body$c1, c("dose", "  Placebo", "  Low", "  High"))
})

test_that("F10: list(ate, po) is one teffects model; type = 'margins' keeps two", {
  skip_if_not_installed("marginaleffects")
  s <- me_setup()
  ate <- marginaleffects::avg_comparisons(s$fit, variables = "am")
  po <- marginaleffects::avg_predictions(s$fit, variables = list(am = "Automatic"))
  tt <- suppressMessages(effecttab(list(ate, po)))
  expect_identical(tt$stored$type, "teffects")
  expect_identical(tt$body$c1, c("r2vs1.am", "1.am"))
  expect_identical(effecttab(list(ate, po), type = "margins")$stored$N_cols, 8)
  # Different variables: two models, as regtab unpacks a list.
  pw <- marginaleffects::avg_predictions(s$fit, variables = "cyl")
  expect_identical(effecttab(list(ate, pw))$stored$N_cols, 8)
})

test_that("F4: one message for an lm/glm teffects result with the default variance; HC0 is Stata's", {
  skip_if_not_installed("marginaleffects")
  withr::local_options(rlib_message_verbosity = "verbose")
  s <- me_setup()
  ate <- marginaleffects::avg_comparisons(s$fit, variables = "am")
  po <- marginaleffects::avg_predictions(s$fit, variables = list(am = "Automatic"))
  expect_message(effecttab(list(list(ate, po))), "vcov = \"HC0\"")
  ate0 <- marginaleffects::avg_comparisons(s$fit, variables = "am", vcov = "HC0")
  po0 <- marginaleffects::avg_predictions(s$fit, variables = list(am = "Automatic"), vcov = "HC0")
  expect_no_message(effecttab(list(list(ate0, po0))))
  # Margins tables say nothing.
  expect_no_message(effecttab(ate))
})

test_that("F4 pinned: on cattaneo2 the RA SE gap is lm's variance; HC0 is within 0.2% of teffects ra", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("haven")
  d <- golden_fixture("cattaneo2")
  d$mbsmoke <- as.numeric(d$mbsmoke)
  inp <- golden_effect_input("E12")[[1]]
  ate <- inp[inp$equation == "ATE", ]
  se_stata <- (ate$conf.high - ate$conf.low) / (2 * stats::qnorm(0.975))
  fit <- lm(bweight ~ mbsmoke * (mage + medu), data = d)
  se_lm <- marginaleffects::avg_comparisons(fit, variables = "mbsmoke")$std.error
  se_hc0 <- marginaleffects::avg_comparisons(fit, variables = "mbsmoke", vcov = "HC0")$std.error
  expect_gt(abs(se_lm / se_stata - 1), 0.015)
  expect_lt(abs(se_hc0 / se_stata - 1), 0.002)
})

test_that("F5: one message for a 0/1 variable's slope, none for a continuous one", {
  skip_if_not_installed("marginaleffects")
  withr::local_options(rlib_message_verbosity = "verbose")
  fit <- glm(vs ~ am + wt, family = binomial, data = mtcars)
  expect_message(effecttab(marginaleffects::avg_slopes(fit, variables = "am")), "discrete change")
  expect_no_message(effecttab(marginaleffects::avg_slopes(fit, variables = "wt")))
})

test_that("F19: the methods sentence names ATET and potential-outcome-mean tables; glm_weightit estimator", {
  d <- te_df()
  d$equation[1] <- "ATET"
  attr(d, "tt_estimator") <- "ipw"
  expect_identical(effecttab(d)$stored$methods,
                   "Average treatment effects on the treated estimated using inverse probability weighting with 95% confidence intervals.")
  po <- te_df()[2, ]
  expect_identical(effecttab(po, type = "teffects", method = "ra")$stored$methods,
                   "Potential-outcome means estimated using regression adjustment with 95% confidence intervals.")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("WeightIt")
  dd <- mtcars
  W <- suppressWarnings(WeightIt::weightit(am ~ wt, data = dd, method = "glm", estimand = "ATT"))
  f1 <- WeightIt::glm_weightit(mpg ~ am, data = dd, weightit = W)
  f2 <- WeightIt::glm_weightit(mpg ~ am * wt, data = dd, weightit = W)
  te <- function(f) effecttab(list(marginaleffects::avg_comparisons(f, variables = "am"),
                                   marginaleffects::avg_predictions(f, variables = list(am = 0))))
  expect_match(te(f1)$stored$methods, "^Average treatment effects on the treated estimated using inverse probability weighting")
  expect_match(te(f2)$stored$methods, "inverse probability weighted regression adjustment")
})

test_that("F20: a contrast other than the derivative is named in the label", {
  skip_if_not_installed("marginaleffects")
  s <- me_setup()
  expect_identical(effecttab(marginaleffects::avg_comparisons(s$fit, variables = list(wt = 1)))$body$c1,
                   "Weight (1000 lbs) (+1)")
  expect_identical(effecttab(marginaleffects::avg_slopes(s$fit, variables = "wt"))$body$c1, "Weight (1000 lbs)")
})

test_that("F16: the request order is read from the written call only, nothing evaluated", {
  expect_identical(tabtools:::.et_literal_names(quote(c("b", "a"))), c("b", "a"))
  expect_identical(tabtools:::.et_literal_names(quote(list(age = c(40, 50), sex = "pairwise"))), c("age", "sex"))
  expect_identical(tabtools:::.et_literal_names(c("x", "y")), c("x", "y"))
  expect_null(tabtools:::.et_literal_names(quote(vars)))
  expect_null(tabtools:::.et_literal_names(quote(c("a", stop("evaluated")))))
  expect_null(tabtools:::.et_literal_names(quote(system("echo x"))))
})

test_that("F17/M13: tlabels keeps the last non-empty label of a repeated level", {
  expect_identical(effecttab(te_df(), tlabels = '0 "A" 0 "B" 1 "T"')$body$c1, c("T vs B", "B (PO Mean)"))
  expect_identical(effecttab(te_df(), tlabels = '0 "A" 0 "" 1 "T"')$body$c1, c("T vs A", "A (PO Mean)"))
})

test_that("F9: the 2.1.12 dedupe of repeated r(table) names, as f42ee9cd", {
  expect_identical(.et_rowname(c("A", "A", "A_2"), 1:3), c("A", "A_2", "A_2_2"))
  long <- strrep("x", 32)
  out <- .et_rowname(c(long, long, long), 1:3)
  expect_identical(out, c(long, paste0(strrep("x", 30), "_2"), paste0(strrep("x", 30), "_3")))
  expect_true(all(nchar(out, type = "bytes") <= 32L))
  expect_false(anyDuplicated(out) > 0)
})

test_that("mutation survivors: 32-byte names, r#vs# keys, factor labels, p-only rows, 2o. levels", {
  # M19: row names are cut to 32 bytes.
  lab <- strrep("abcdefghij", 4)
  tt <- effecttab(data.frame(label = lab, estimate = 1, p.value = 0.5), level = 95)
  expect_identical(rownames(tt$stored$table), substr(lab, 1, 32))
  # M29: an r#vs# key alone (no equation column) makes a teffects table.
  d <- data.frame(term = c("r1vs0.t", "0.t"), estimate = c(1, 2), p.value = c(0.1, 0.1))
  expect_identical(suppressMessages(effecttab(d))$stored$type, "teffects")
  # M33: a tt_as_factor() factor's value labels on the data-frame path.
  fd <- tt_as_factor(te_data(), "smoke")
  expect_identical(effecttab(te_df(), data = fd, clean = TRUE)$body$c1, c("Smoker vs Nonsmoker", "Nonsmoker (PO Mean)"))
  expect_identical(effecttab(mg_df(), data = tt_as_factor(race_data(), "race"))$body$c1[1:4],
                   c("Race", "  White", "  Black", "  Other"))
  # M35: a row with a p-value and no estimate is an r(table) row.
  p <- data.frame(term = c("a", "b"), estimate = c(1, NA), p.value = c(0.2, 0.3))
  tt <- effecttab(p, level = 95)
  expect_identical(rownames(tt$stored$table), c("a", "b"))
  expect_equal(unname(tt$stored$table[2, ]), c(NA, 0.3))
  # M42: an omitted factor level (2o.f) shows omitlabel.
  o <- data.frame(term = c("1b.f", "2o.f", "3.f"), estimate = c(0, 0, 1), p.value = c(NA, NA, 0.1))
  expect_identical(effecttab(o, level = 95)$body$c2, c("", "Reference", "Omitted", "1.00"))
})

test_that("D04: sheet without xlsx is an error, as in table1_tc", {
  expect_error(effecttab(te_df(), sheet = "Zed"), "`sheet` is only available when using `xlsx`")
})

test_that("P2-2: sheet = NULL means the default sheet", {
  expect_no_error(effecttab(te_df(), sheet = NULL))
})

test_that("B03: an integer-level factor is keyed by its values, as regtab and Stata teffects key it", {
  skip_if_not_installed("marginaleffects")
  set.seed(8)
  n <- 600
  d <- data.frame(x = rnorm(n), dose = factor(sample(0:2, n, TRUE)))
  d$y <- rbinom(n, 1, plogis(-0.5 + 0.3 * d$x + 0.6 * (d$dose == "2")))
  f <- glm(y ~ dose + x, binomial, d)
  cm <- marginaleffects::avg_comparisons(f, variables = "dose")
  pr <- marginaleffects::avg_predictions(f, variables = "dose")
  # marginaleffects: 1 - 0, 2 - 0; PO means of dose 0, 1, 2.
  expect_identical(cm$contrast, c("1 - 0", "2 - 0"))
  expect_identical(as.character(pr$dose), c("0", "1", "2"))
  tl <- c("0" = "None", "1" = "Low", "2" = "High")
  expect_identical(effecttab(list(cm, pr), tlabels = tl)$body$c1,
                   c("Low vs None", "High vs None", "None (PO Mean)", "Low (PO Mean)", "High (PO Mean)"))
  # Raw keys: Stata's teffects names (r1vs0.dose, 0.dose), regtab's codes.
  expect_identical(effecttab(list(cm, pr))$body$c1, c("r1vs0.dose", "r2vs0.dose", "0.dose", "1.dose", "2.dose"))
  rk <- regtab(f)$meta$regtab_rows
  expect_true(all(c("1.dose", "2.dose") %in% rk$key))
  er <- tt_effect_rows(cm)
  expect_identical(er$level, c("1", "2"))
  expect_identical(er$base, c("0", "0"))
  expect_false(any(er$positional))
  # Unlabelled values under clean: Stata's `Tvar (1 vs 0)` form.
  expect_identical(effecttab(list(cm, pr), clean = TRUE)$body$c1,
                   c("Dose (1 vs 0)", "Dose (2 vs 0)", "Dose = 0 (PO Mean)", "Dose = 1 (PO Mean)",
                     "Dose = 2 (PO Mean)"))
  # A factor whose levels are not integers keeps position codes, and a
  # tlabels name matching the level's own text wins over its position: level
  # "2" of factor(c("2", "b")) is position 1, and tlabels names "2" and "1".
  set.seed(9)
  e <- data.frame(x = rnorm(n), g = factor(sample(c("2", "b"), n, TRUE), levels = c("2", "b")))
  e$y <- rbinom(n, 1, plogis(0.3 * e$x))
  fe <- glm(y ~ g + x, binomial, e)
  ce <- marginaleffects::avg_comparisons(fe, variables = "g")
  pe <- marginaleffects::avg_predictions(fe, variables = "g")
  expect_identical(effecttab(list(ce, pe))$body$c1, c("r2vs1.g", "1.g", "2.g"))
  expect_identical(effecttab(list(ce, pe), tlabels = c("2" = "Two", "1" = "One"))$body$c1,
                   c("b vs Two", "Two (PO Mean)", "b (PO Mean)"))
  # A character treatment with integer texts is keyed by value too.
  d$dosec <- as.character(d$dose)
  fc <- glm(y ~ dosec + x, binomial, d)
  cc <- marginaleffects::avg_comparisons(fc, variables = "dosec")
  expect_identical(tt_effect_rows(cc)$key, c("r1vs0.dosec", "r2vs0.dosec"))
})

test_that("B05: a predictions() grid is refused as a grid, not as one row per observation", {
  skip_if_not_installed("marginaleffects")
  set.seed(8)
  d <- data.frame(x = rnorm(600), trt = factor(sample(c("ctl", "hi"), 600, TRUE)))
  d$y <- rbinom(600, 1, plogis(d$x))
  f <- glm(y ~ trt + x, binomial, d)
  g <- marginaleffects::predictions(f, newdata = marginaleffects::datagrid(trt = c("ctl", "hi"), x = 0))
  expect_error(effecttab(g), "grid")
  expect_error(effecttab(g), "by = ")
  cg <- marginaleffects::comparisons(f, variables = "trt", newdata = marginaleffects::datagrid(x = 0))
  expect_error(effecttab(cg), "grid")
  # Unit-level results keep their message.
  expect_error(effecttab(marginaleffects::predictions(f)), "one row per observation")
})
