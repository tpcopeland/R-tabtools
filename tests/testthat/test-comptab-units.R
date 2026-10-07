# Unit and adversarial tests for comptab() and hrcomptab() (task 7.5). The
# goldens (test-golden-comptab.R) pin the layout against Stata; these pin
# the selection, matching and refusal rules of comptab.ado on small
# data-frame models (helper-comptab.R), so they need no Suggests.

cells <- function(tt) tabtools:::.tt_cells(tt)
body_cell <- function(tt, row_label, col) {
  b <- as.matrix(tt$body)
  unname(b[trimws(b[, 1]) == row_label, col])
}

# ---------------------------------------------------------------------------
# Rate mode

test_that("rate mode: the Table 2 layout, reference rows, and hrcomptab() = comptab()", {
  r <- ct_rates()
  m <- ct_models()
  x <- hrcomptab(r, list(m, m), rows = list(1, 4:5))
  expect_identical(x$command, "hrcomptab")
  expect_identical(x$header[[1]]$text[c(1, 2, 7)], c("Exposure", "Death", "Relapse"))
  expect_identical(x$header[[2]]$text[2:6], c("Events", "Person-Years (PY)", "Per 1,000 PY (95% CI)",
                                              "aHR (95% CI)", "p-value"))
  expect_identical(body_cell(x, "No", 5), "Reference")
  expect_identical(body_cell(x, "No", 6), "")
  expect_identical(body_cell(x, "Yes", 5), "0.80 (0.64, 1.00)")
  expect_identical(body_cell(x, "Yes", 10), "0.90 (0.72, 1.13)")
  expect_identical(body_cell(x, "High", c(5, 6)), c("1.40 (1.12, 1.75)", "0.060"))
  expect_equal(unname(unlist(x$stored[c("N_rows", "N_outcomes", "N_sections", "N_modelrows", "N_modelframes")])),
               c(10, 2, 2, 3, 2))
  expect_identical(x$stored$effect, "aHR")
  expect_identical(x$stored$rateframe, "r")
  expect_identical(x$stored$modelframes, "m m")
  y <- comptab(r, list(m, m), rows = list(1, 4:5))
  expect_identical(cells(y), cells(x))
  # rownames(), Stata strings, and the tables' data frames give the same.
  z <- hrcomptab(r, list(m, m), rownames = list("treated", c("low", "high")))
  expect_identical(cells(z), cells(x))
  z2 <- hrcomptab(r, list(m, m), rows = "1 \\ 4/5", rownames = NULL)
  expect_identical(cells(z2), cells(x))
  z3 <- hrcomptab(r, list(m, m), rownames = "treated \\ low high")
  expect_identical(cells(z3), cells(x))
  z4 <- hrcomptab(as.data.frame(r), list(as.data.frame(m), as.data.frame(m)), rows = list(1, 4:5))
  expect_identical(cells(z4), cells(x))
  expected_import <- as_forest_data(x)
  expected_import$source_frame[expected_import$source_frame == "r"] <- "as.data.frame(r)"
  expected_import$source_frame[expected_import$source_frame == "m"] <- "as.data.frame(m)"
  expect_identical(as_forest_data(z4), expected_import)
  df <- as.data.frame(x)
  expect_identical(attr(df, "source"), "hrcomptab")
  expect_identical(attr(df, "statistic_ids"), "events person_years rate_ci estimate_ci pvalue")
  expect_identical(attr(df, "outcome_id"), c("death", "relapse"))
  expect_identical(attr(df, "effect_scale"), c("HR", "HR"))
})

test_that("rate mode: selections are placed by label, whatever their order; a plain row fills the second of two", {
  r <- ct_rates()
  m <- ct_models()
  x <- hrcomptab(r, list(m, m), rows = list(1, 4:5))
  y <- hrcomptab(r, list(m, m), rows = list(1, c(5, 4)))
  expect_identical(cells(y), cells(x))
  # The indicator row "Treated" matches no category: it fills "Yes".
  expect_identical(body_cell(x, "Yes", 5), "0.80 (0.64, 1.00)")
  # A model with another base level shows its reference on that category.
  mb <- regtab(list(ct_model("death", ref = 2), ct_model("relapse", 0.1, ref = 2)), models = c("Death", "Relapse"))
  z <- hrcomptab(r, list(m, mb), rows = list(1, c(5, 3)))
  expect_identical(body_cell(z, "Low", 5), "Reference")
  expect_identical(body_cell(z, "None", 5), paste(mb$body[3, 2], mb$body[3, 3]))
})

test_that("rate mode: models are matched by outcome identity or outcomemap, never by position", {
  r <- ct_rates()
  m <- ct_models()
  rev <- ct_models(outcomes = c("relapse", "death"), labels = c("Relapse", "Death"))
  x <- hrcomptab(r, list(m, m), rows = list(1, 4:5))
  y <- hrcomptab(r, list(rev, rev), rows = list(1, 4:5))
  # The death column shows the death model's numbers (model 2 of `rev`).
  expect_identical(body_cell(y, "Yes", 5), paste(body_cell(rev, "Treated", 5), body_cell(rev, "Treated", 6)))
  expect_identical(body_cell(y, "Yes", 10), paste(body_cell(rev, "Treated", 2), body_cell(rev, "Treated", 3)))
  # Identities that do not match: refused, with a hint.
  odd <- ct_models(outcomes = c("died", "relapsed"))
  expect_error(hrcomptab(r, odd, rows = 1), "could not be matched")
  expect_error(hrcomptab(r, odd, rows = 1), "outcomemap")
  # outcomemap by model label (case and blanks ignored), or Stata's string.
  ok <- hrcomptab(r, list(odd, odd), rows = list(1, 4:5), outcomemap = c(" death", "RELAPSE"))
  expect_identical(cells(ok)[, 5], cells(x)[, 5])
  ok2 <- hrcomptab(r, list(odd, odd), rows = list(1, 4:5), outcomemap = "Death \\ Relapse")
  expect_identical(cells(ok2), cells(ok))
  expect_error(hrcomptab(r, odd, rows = 1, outcomemap = "Death"), "requires 2 identit")
  expect_error(hrcomptab(r, odd, rows = 1, outcomemap = c("Death", "Death")), "more than one rate outcome")
  expect_error(hrcomptab(r, odd, rows = 1, outcomemap = c("Death", "nothing")), "matched 0 model blocks")
  # An identity that is one model's outcome and another's label matches two.
  two <- regtab(list(ct_model("death", label = "a"), ct_model("x", label = "b")), models = c("Relapse", "death"))
  expect_error(hrcomptab(r, two, rows = 1, outcomemap = c("death", "zz")), "matched 2 model blocks")
})

test_that("rate mode: the model tables must hold one model per outcome", {
  r1 <- stratetab(list(ct_block(c("No", "Yes"), c(40, 30), c(5000, 4800))), outlabels = "Death", outcomeids = "death",
                  explabels = "Treated")
  m <- ct_models()
  expect_error(hrcomptab(r1, m, rows = 1), "must match the rate table's outcomes")
  one <- regtab(ct_model("death"), models = "Death")
  x <- hrcomptab(r1, one, rows = 1)
  expect_identical(body_cell(x, "Yes", 5), "0.80 (0.64, 1.00)")
  expect_error(hrcomptab(ct_rates(), list(m, one), rows = list(1, 4:5)), "must match the rate table's outcomes|same layout")
})

test_that("rate mode: a model lacking an exposure level leaves that cell blank, and its forest data are refused", {
  r <- ct_rates()
  m <- ct_models()
  gap <- regtab(list(ct_model("death"), ct_model("relapse", 0.1, drop = "High")), models = c("Death", "Relapse"))
  x <- hrcomptab(r, list(m, gap), rows = list(1, 4:5))
  expect_identical(body_cell(x, "High", c(5, 6)), c("1.40 (1.12, 1.75)", "0.060"))
  expect_identical(body_cell(x, "High", c(10, 11)), c("", ""))
  expect_error(as_forest_data(x), "no estimate for row 5 of outcome 2")
})

test_that("rate mode: refusals follow comptab.ado", {
  r <- ct_rates()
  m <- ct_models()
  # heading, reference, unmatched level, two rows for one category, count
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 2:3)), "heading row")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 3:4)), "reference")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, c(4, 4))), "Two selected model rows")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4)), "must match the non-reference rows")
  med <- regtab(list(ct_model("death", levels = c("None", "Medium", "High")),
                     ct_model("relapse", levels = c("None", "Medium", "High"))), models = c("Death", "Relapse"))
  expect_error(hrcomptab(r, list(m, med), rows = list(1, 4:5)), "matches no category")
  # The category left over must be the model's base level.
  four <- regtab(list(ct_model("death", levels = c("None", "Low", "Mid", "High")),
                      ct_model("relapse", levels = c("None", "Low", "Mid", "High"))), models = c("Death", "Relapse"))
  r3 <- ct_rates(dose = c("Low", "Mid", "High"))
  expect_error(hrcomptab(r3, list(m, four), rows = list(1, 5:6)), "not the reference category")
  # rownames: no match, and a row selected twice across patterns
  expect_error(hrcomptab(r, list(m, m), rownames = list("treated", "nothing")), "not found")
  expect_error(hrcomptab(r, list(m, m), rownames = list("treated", c("low", "l?w"))), "again")
  # effect scale, confidence level, rate-ratio tables, provenance
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), effect = "OR"), "hazard-ratio scale")
  ok <- hrcomptab(r, list(m, m), rows = list(1, 4:5), effect = "Hazard-Ratio")
  expect_identical(ok$header[[2]]$text[5], "Hazard-Ratio (95% CI)")
  ors <- regtab(list(ct_model("death", scale = "OR"), ct_model("relapse", scale = "OR")), models = c("Death", "Relapse"))
  expect_error(hrcomptab(r, list(ors, ors), rows = list(1, 4:5)), "not on the hazard-ratio scale")
  m90 <- regtab(list(ct_model("death", level = 0.9), ct_model("relapse", level = 0.9)), models = c("Death", "Relapse"))
  expect_error(hrcomptab(r, list(m90, m90), rows = list(1, 4:5)), "different \\(or unknown\\) confidence levels")
  x90 <- hrcomptab(ct_rates(level = 90), list(m90, m90), rows = list(1, 4:5))
  expect_identical(x90$header[[2]]$text[4:5], c("Per 1,000 PY (90% CI)", "aHR (90% CI)"))
  rr <- stratetab(list(ct_block(c("A", "B"), c(9, 8), c(900, 800)), ct_block(c("A", "B"), c(7, 6), c(700, 600))),
                  outcomes = 1, rateratio = TRUE, outcomeids = "death")
  expect_error(hrcomptab(rr, regtab(ct_model("death")), rows = 1), "without .*rateratio")
  rrdf <- as.data.frame(rr)
  expect_error(hrcomptab(rrdf, regtab(ct_model("death")), rows = 1), "without .*rateratio")
  # C8: a 3-outcome rate-ratio table has 13 columns, as a 4-outcome plain
  # one; its statistic ids alone refuse it by name, as Stata 2.1.14 reads
  # them (comptab.ado:419-426), with the layout attributes gone.
  b2 <- ct_block(c("A", "B"), c(9, 8), c(900, 800))
  rr3 <- as.data.frame(stratetab(rep(list(b2), 6), outcomes = 3, rateratio = TRUE,
                                 outcomeids = c("death", "mi", "stroke")))
  expect_identical(attr(rr3, "statistic_ids"), "events person_years rate_ci irr_ci")
  expect_identical(ncol(rr3), 13L)
  attr(rr3, "rateratio") <- NULL
  attr(rr3, "cols_per_outcome") <- NULL
  expect_error(hrcomptab(rr3, regtab(ct_model("death")), rows = 1), "without .*rateratio")
  bare <- as.data.frame(r)
  attr(bare, "statistic_ids") <- NULL
  expect_error(comptab(bare, list(m, m), rows = list(1, 4:5)), "provenance")
  tiny <- stratetab(list(ct_block("Only", 3, 100)), outlabels = "Death", outcomeids = "death")
  expect_error(hrcomptab(tiny, regtab(ct_model("death")), rows = 1), "no non-reference rows")
  short <- as.data.frame(tiny)
  att <- attributes(short)
  short <- short[1:3, ]
  for (a in setdiff(names(att), c("names", "row.names", "class"))) attr(short, a) <- att[[a]]
  expect_error(hrcomptab(short, regtab(ct_model("death")), rows = 1), "too few rows")
  flat <- stratetab(list(ct_block("Only", 3, 100), ct_block("Only", 4, 90)), outcomes = 1, outcomeids = "death")
  expect_error(hrcomptab(flat, regtab(ct_model("death")), rows = 1), "no non-reference rows")
})

test_that("rate mode: effecttab sources on the hazard-ratio scale, matched by label", {
  r1 <- stratetab(list(ct_block(c("No", "Yes"), c(40, 30), c(5000, 4800))), outlabels = "Death", outcomeids = "death",
                  explabels = "Treated")
  mhr <- matrix(c(0.8, 0.6, 1.07, 0.13), 1, dimnames = list("Treated_yes", NULL))
  e <- effecttab(mhr, effect = "HR", models = "Death")
  expect_error(hrcomptab(r1, e, rows = 1), "could not be matched")
  x <- hrcomptab(r1, e, rows = 1, outcomemap = "death")
  expect_identical(body_cell(x, "Yes", 5), "0.80 (0.60, 1.07)")
  ate <- effecttab(mhr, effect = "ATE", models = "Death")
  expect_error(hrcomptab(r1, ate, rows = 1, outcomemap = "death"), "not on the hazard-ratio scale")
})

test_that("rate mode: vertical-only options and argument checks", {
  r <- ct_rates()
  m <- ct_models()
  for (a in list(list(compact = TRUE), list(separator = 2), list(section = c("a", "b")), list(relabel = c("1" = "x")),
                 list(highlight = 0.05), list(boldp = 0.05), list(labelwidth = 20))) {
    expect_error(do.call(comptab, c(list(r, list(m, m), rows = list(1, 4:5)), a)), "not allowed with a rate table")
    expect_error(do.call(hrcomptab, c(list(r, list(m, m), rows = list(1, 4:5)), a)), "not allowed")
  }
  expect_error(hrcomptab(r, list(m, m)), "One of")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), rownames = list("a", "b")), "may not be combined")
  expect_error(hrcomptab(r, list(m, m), rows = c(1, 4)), "one selection per model table")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5, 1)), "requires 2 specifications")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 9)), "out of range")
  expect_error(hrcomptab(m, list(m)), "must be a .*stratetab")
  expect_error(hrcomptab(modeltables = list(m)), "is required")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), bogus = 1), "Unknown argument")
  expect_error(hrcomptab(r, list(m, m), NULL, NULL, NULL, NULL, NULL, "x"), "must be named")
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), open = TRUE), "requires .*xlsx")
  expect_error(comptab(list(m), rows = 1, effect = "HR"), "require a rate table")
})

test_that("rate mode: the title defaults to the rate table's, the footnote follows the listing", {
  r <- ct_rates(title = "Rates by exposure")
  m <- ct_models()
  x <- hrcomptab(r, list(m, m), rows = list(1, 4:5), footnote = "aHR, adjusted hazard ratio.")
  expect_identical(x$title, "Rates by exposure")
  out <- utils::capture.output(print(x))
  n <- length(out)
  expect_identical(out[(n - 2):n], c("", "aHR, adjusted hazard ratio.", ""))
  y <- hrcomptab(r, list(m, m), rows = list(1, 4:5), title = "Mine")
  expect_identical(y$title, "Mine")
})

test_that("rate mode: forest data fold a heading over one row, keep references and one row per outcome", {
  r <- ct_rates()
  m <- ct_models()
  f <- as_forest_data(hrcomptab(r, list(m, m), rows = list(1, 4:5)))
  expect_identical(f$rowtype, c("section", "reference", "effect", "effect", "section", "reference",
                                rep("effect", 4)))
  expect_identical(f$model[f$rowtype == "effect"], rep(1:2, 3))
  expect_identical(f$model_label[3:4], c("Death", "Relapse"))
  expect_equal(f$estimate[3:4], c(0.8, 0.9))
  expect_identical(attr(f, "source"), "hrcomptab")
  # One outcome and one category row after a heading: folded.
  r1 <- stratetab(list(ct_block(c("No", "Yes"), c(40, 30), c(5000, 4800))), outlabels = "Death", outcomeids = "death",
                  explabels = "Treated")
  f1 <- as_forest_data(hrcomptab(r1, regtab(ct_model("death"), models = "Death"), rows = 1))
  expect_identical(f1$rowtype, c("section", "reference", "effect"))
})

# ---------------------------------------------------------------------------
# Vertical mode

test_that("vertical mode: rows in each table's own order, once; sections, relabel, separators", {
  m <- ct_models()
  x <- comptab(list(m, m), rows = list(c(6, 1, 1), 4))
  expect_identical(x$command, "comptab")
  expect_identical(trimws(x$body[[1]]), c("Treated", "Age", "Low"))
  s <- comptab(list(m, m), rows = list(1, 4:5), section = c("Binary", "Dose"),
               relabel = c("2" = "Treated (any)", "3" = "Dose categories"), separator = c(4, 99))
  expect_identical(s$body[[1]], c("Binary", "Treated (any)", "Dose categories", "  Low", "  High"))
  expect_identical(s$rows$section, c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(comptab(list(m, m), rows = "1 \\ 4/5", section = "Binary \\ Dose")$body,
                   comptab(list(m, m), rows = list(1, 4:5), section = c("Binary", "Dose"))$body)
  expect_identical(comptab(list(m, m), rows = list(1, 4:5), relabel = '1 "One" 2 "Two"')$body[[1]][1:2], c("One", "Two"))
  expect_error(comptab(list(m, m), rows = list(1, 4:5), relabel = c("9" = "x")), "out of range")
  expect_error(comptab(list(m, m), rows = list(1, 4:5), relabel = '1 "One" 2'), "pairs")
  expect_error(comptab(list(m, m), rows = list(1, 4:5), section = "only one"), "requires 2 labels")
  st <- x$stored
  expect_equal(unname(unlist(st[c("N_rows", "N_cols", "N_models", "N_frames")])), c(6, 8, 2, 2))
  expect_match(st$methods, "Composite table assembled from 2 source frame\\(s\\) with 2 model column\\(s\\)")
  # comptab(list(...)) and comptab(NULL, list(...)) are the same call.
  expect_identical(cells(comptab(NULL, list(m, m), rows = list(1, 4))), cells(comptab(list(m, m), rows = list(1, 4))))
})

test_that("vertical mode: rownames patterns (substrings, * and ?) may match several rows, none is an error", {
  m <- ct_models()
  x <- comptab(list(m, m), rownames = list("e", "h?gh"))
  # "e" matches the factor heading "Dose" too.
  expect_identical(trimws(x$body[[1]]), c("Treated", "Dose", "None", "Age", "High"))
  y <- comptab(list(m), rownames = c("t*d", "age"))
  expect_identical(trimws(y$body[[1]]), c("Treated", "Age"))
  expect_error(comptab(list(m, m), rownames = list("e", "nothing")), "not found")
  # Regular-expression characters are literal.
  expect_error(comptab(list(m), rownames = "a.e"), "not found")
})

test_that("vertical mode: compact merges estimate and interval; compact sources stay compact", {
  m <- ct_models()
  x <- comptab(list(m, m), rows = list(1, 3:4), compact = TRUE)
  expect_identical(x$header[[2]]$text, c("", "HR 95% CI", "p-value", "HR 95% CI", "p-value"))
  expect_identical(body_cell(x, "Treated", 2), "0.80 (0.64, 1.00)")
  expect_identical(body_cell(x, "None", 2), "Reference")
  expect_identical(as.data.frame(x) |> attr("statistic_ids"), "estimate_ci pvalue")
  mc <- regtab(list(ct_model("death"), ct_model("relapse", 0.1)), models = c("Death", "Relapse"), compact = TRUE)
  y <- comptab(list(mc, mc), rows = list(1, 3:4))
  z <- comptab(list(mc, mc), rows = list(1, 3:4), compact = TRUE)
  expect_identical(cells(z), cells(y))
  expect_identical(y$header[[2]]$text[2], "HR 95% CI")
  # Standard and compact sources together: refused.
  expect_error(comptab(list(m, mc), rows = list(1, 1)), "Column mismatch|same layout")
})

test_that("vertical mode: model columns align on outcome, label, or model identity", {
  a <- ct_models()
  b <- ct_models(outcomes = c("relapse", "death"), labels = c("Relapse", "Death"))
  x <- comptab(list(a, b), rows = list(1, 1))
  # Row 2 (from b) shows b's death model under Death.
  expect_identical(x$body[2, 2], b$body[1, 5])
  expect_identical(x$body[2, 5], b$body[1, 2])
  # Same outcome in both models: aligned by label.
  sa <- regtab(list(ct_model("death", label = "c"), ct_model("death", 0.1, label = "a")), models = c("Crude", "Adjusted"))
  sb <- regtab(list(ct_model("death", 0.1, label = "a"), ct_model("death", label = "c")), models = c("Adjusted", "Crude"))
  y <- comptab(list(sa, sb), rows = list(1, 1))
  expect_identical(unname(as.matrix(y$body)[1, -1]), unname(as.matrix(y$body)[2, -1]))
  sm <- regtab(list(ct_model("death", label = "c"), ct_model("death", 0.1, label = "a")), models = c("Crude", "Other"))
  expect_error(comptab(list(sa, sm), rows = list(1, 1)), "missing from model table 2")
  m90 <- regtab(list(ct_model("death", level = 0.9), ct_model("relapse", level = 0.9)), models = c("Death", "Relapse"))
  expect_error(comptab(list(a, m90), rows = list(1, 1)), "different confidence levels")
  aor <- regtab(list(ct_model("death", scale = "OR"), ct_model("relapse", scale = "OR")), models = c("Death", "Relapse"))
  expect_error(comptab(list(a, aor), rows = list(1, 1)), "effect scales")
  one <- regtab(ct_model("death"), models = "Death")
  expect_error(comptab(list(a, one), rows = list(1, 1)), "Column mismatch")
})

test_that("vertical mode: effecttab tables, with and without model identities", {
  ma <- matrix(c(0.12, 0.05, 0.19, 0.0004, 0.08, 0.01, 0.15, 0.03), 2, byrow = TRUE,
               dimnames = list(c("Treated", "Dose_low"), NULL))
  fa <- effecttab(ma, effect = "RD", models = "ATE")
  fb <- effecttab(ma[1, , drop = FALSE], effect = "RD", models = "ATE")
  x <- comptab(list(fa, fb), rows = list(2, 1), section = c("Main", "Sensitivity"))
  expect_identical(x$body[[1]], c("Main", "Dose low", "Sensitivity", "Treated"))
  # A data-frame effecttab has no model identity: it aligns by label.
  dfa <- data.frame(term = c("x", "z"), estimate = c(0.1, 0.2), conf.low = c(0, 0.1), conf.high = c(0.2, 0.3),
                    p.value = c(0.04, 0.001), stringsAsFactors = FALSE)
  ea <- suppressMessages(effecttab(dfa, models = "A", type = "margins"))
  y <- comptab(list(ea, ea), rows = list(1, 2))
  expect_identical(nrow(y$body), 2L)
  eb <- suppressMessages(effecttab(dfa, type = "margins"))
  expect_error(comptab(list(eb, eb), rows = list(1, 2)), "blank model identity")
})

test_that("vertical mode: argument checks", {
  m <- ct_models()
  expect_error(comptab(list(m, m)), "One of")
  expect_error(comptab(list(m, m), rows = list(1, 1), highlight = 2), "strictly between 0 and 1")
  expect_error(comptab(list(m, m), rows = list(1, 1), boldp = 0), "strictly between 0 and 1")
  expect_error(comptab(list(m, m), rows = list(1, 1), labelwidth = 2.5), "whole number")
  expect_error(comptab(list(m, m), rows = list(1, 1), outcomemap = "x"), "require a rate table")
  expect_error(comptab(list(m, m), rows = list(1, 1), mdappend = TRUE), "requires .*markdown")
  expect_error(comptab(list(m, m), rows = list(1, 1), xlsx = "a.xls"), ".xlsx extension")
  expect_error(comptab(list(m, m), rows = list(1, 1), csv = "a.txt"), ".csv file")
  expect_error(comptab(list(m, m), rows = list(0, 1)), "out of range")
  expect_error(comptab(list(m, m), rows = list(1, 1), separator = 0), "positive whole")
  expect_error(comptab(list(m, m), rows = list(1, 1), separator = 1.5), "positive whole")
  expect_error(comptab(list(m, m), rows = list(1.5, 1)), "whole row numbers")
  expect_error(comptab(list(1, 2), rows = list(1, 1)), "must be a .*regtab")
  expect_error(comptab(list(m, data.frame(a = 1)), rows = list(1, 1)), "no model provenance")
  np <- regtab(list(ct_model("death"), ct_model("relapse")), models = c("Death", "Relapse"), nopvalue = TRUE)
  expect_error(comptab(list(np), rows = 1), "unsupported column structure")
})

test_that("vertical mode: a Reference row merges only its own model block (H-D1)", {
  # Model A's base is None, model B's is Low: row "None" is Reference in A
  # and an estimate in B. Stata 2.1.12 merges every block of the row
  # (comptab.ado:2917-2924), hiding B's interval and p-value; R merges A's.
  mix <- regtab(list(ct_model("death", label = "a"), ct_model("death", 0.1, ref = 2, label = "b")),
                models = c("A", "B"))
  x <- comptab(list(mix), rows = 1:5)
  row <- which(trimws(x$body[[1]]) == "None")
  expect_identical(x$body[row, 2], "Reference")
  expect_match(x$body[row, 5], "^[0-9]")
  lay <- tabtools:::.xlsx_layout_comptab(x)
  st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), x$style)
  r <- row + 3L
  expect_true(paste0("C", r, ":E", r) %in% st$merges)
  expect_false(paste0("F", r, ":H", r) %in% st$merges)
  expect_false(st$italic[r, 6])
})

test_that("vertical mode: boldp and highlight read the p text; <0.001 is 0, >0.99 none", {
  expect_identical(tabtools:::.ct_p_text(c("<0.001", "0.04", ">0.99", "", " 0.5")), c(0, 0.04, NA, NA, 0.5))
  m <- ct_models()
  x <- comptab(list(m), rows = c(1, 4, 6), boldp = 0.05, highlight = 0.01)
  lay <- tabtools:::.xlsx_layout_comptab(x)
  st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), x$style)
  # Row "Low" (p 0.040) bold, not highlighted; row "Age" (<0.001) both.
  expect_true(st$bold[5, 5])
  expect_true(is.na(st$fill[5, 2]))
  expect_true(st$bold[6, 5])
  expect_identical(st$fill[6, 2], "FFFFFFCC")
  expect_false(st$bold[4, 5])
})

test_that("vertical mode: forest data follow the table's rows, fold a section over one row", {
  m <- ct_models()
  x <- comptab(list(m, m), rows = list(c(4, 1, 1), 5), section = c("First", "Second"))
  f <- as_forest_data(x)
  expect_identical(f$rowtype, c("section", rep("effect", 4), "section", "effect", "effect"))
  expect_identical(f$source_row, c(NA, 1L, 1L, 4L, 4L, NA, 5L, 5L))
  expect_identical(f$source_frame[2], "m")
  expect_identical(attr(f, "source"), "comptab")
  # Stata folds a section only when it owns one plotted row: one row of a
  # two-model table is two plotted rows (comptab.ado:2514-2531).
  single <- comptab(list(m), rows = 1, section = "Only")
  expect_identical(as_forest_data(single)$rowtype, c("section", "effect", "effect"))
  one <- regtab(ct_model("death"), models = "Death")
  folded <- as_forest_data(comptab(list(one), rows = 1, section = "Only"))
  expect_identical(folded$rowtype, "effect")
  expect_identical(folded$label, "Only")
  expected_import <- as_forest_data(comptab(list(m), rows = 1))
  expected_import$source_frame[] <- "as.data.frame(m)"
  expect_identical(as_forest_data(comptab(list(as.data.frame(m)), rows = 1)), expected_import)
  legacy <- as.data.frame(m)
  attr(legacy, "composition") <- NULL
  expect_error(as_forest_data(comptab(list(legacy), rows = 1)), "not a data frame")
})

test_that("both modes: sinks, the sheet's own spelling, converters", {
  m <- ct_models()
  r <- ct_rates()
  dir <- withr::local_tempdir()
  book <- file.path(dir, "t.xlsx")
  x <- comptab(list(m, m), rows = list(1, 4:5), xlsx = book, sheet = "Table 3", csv = file.path(dir, "t.csv"),
               markdown = file.path(dir, "t.md"), title = "T", footnote = "F")
  expect_true(all(file.exists(file.path(dir, c("t.xlsx", "t.csv", "t.md")))))
  expect_identical(x$stored$sheet, "Table 3")
  expect_identical(x$stored$markdown_rows, 4L)
  y <- hrcomptab(r, list(m, m), rows = list(1, 4:5), xlsx = book, sheet = "table 3")
  expect_identical(y$stored$sheet, "Table 3")
  expect_identical(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book)), c(`Table 3` = "Table 3"))
  md <- readLines(file.path(dir, "t.md"))
  expect_identical(md[3], "|  | Death |  |  | Relapse |  |  |")
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  expect_s3_class(flextable::as_flextable(x), "flextable")
  expect_s3_class(tt_as_gt(y), "gt_tbl")
})

# ---------------------------------------------------------------------------
# Phase 7c review findings

test_that("review P2-1: label-aligned tables must agree on the outcome identities", {
  sa <- regtab(list(ct_model("death", label = "c"), ct_model("death", 0.1, label = "a")), models = c("Crude", "Adjusted"))
  sb <- regtab(list(ct_model("relapse", label = "c"), ct_model("relapse", 0.1, label = "a")), models = c("Crude", "Adjusted"))
  expect_error(comptab(list(sa, sb), rows = list(1, 1)), "outcome identities")
})

test_that("review P2-2: model tables with different statistic identities are refused", {
  m <- ct_models()
  a <- as.data.frame(m)
  b <- as.data.frame(m)
  attr(b, "statistic_ids") <- "estimate pvalue ci"
  expect_error(comptab(list(a, b), rows = list(1, 1)),
               "Imported composition text or frame identity differs",
               fixed = TRUE, class = "tabtools_error_composition")
  expect_identical(nrow(comptab(list(a, a), rows = list(1, 1))$body), 2L)
})

test_that("review P2-3: comptab(m1, m2) points to comptab(list(m1, m2))", {
  m <- ct_models()
  expect_error(comptab(m, m, rows = list(1, 1)), "not a .*stratetab.* rate table")
  expect_error(comptab(m, m, rows = list(1, 1)), "comptab\\(list\\(m1, m2\\)")
  expect_error(comptab(m, list(m), rows = 1), "pass them as one list")
})

test_that("review P3-1/P3-2: rownames strings split at blanks as in Stata; blank patterns refused", {
  m <- ct_models()
  x <- comptab(list(m), rownames = "treated age")
  expect_identical(trimws(x$body[[1]]), c("Treated", "Age"))
  expect_identical(cells(comptab(list(m, m), rownames = list("treated age", "high"))),
                   cells(comptab(list(m, m), rownames = list(c("treated", "age"), "high"))))
  # A vector of several strings is taken element by element; a quoted
  # phrase stays one pattern.
  mm <- regtab(list(ct_model("death", levels = c("None", "2 or more")), ct_model("relapse", levels = c("None", "2 or more"))),
               models = c("Death", "Relapse"))
  expect_identical(trimws(comptab(list(mm), rownames = c("treated", "2 or more"))$body[[1]]), c("Treated", "2 or more"))
  expect_identical(trimws(comptab(list(mm), rownames = 'treated "2 or more"')$body[[1]]), c("Treated", "2 or more"))
  expect_error(comptab(list(m), rownames = "  "), "blank")
  expect_error(comptab(list(m), rownames = c("treated", "")), "blank pattern")
  expect_error(comptab(list(m), rownames = c("treated", "  ")), "blank pattern")
  expect_error(comptab(list(m), rownames = '""'), "blank pattern")
  expect_error(hrcomptab(ct_rates(), list(m, m), rownames = list("treated", c("low", ""))), "blank pattern")
  # rows given as one string are a numlist.
  expect_identical(trimws(comptab(list(m), rows = "1 6")$body[[1]]), c("Treated", "Age"))
})

test_that("review P3-3: hrcomptab() refuses vertical options only when they ask for something", {
  r <- ct_rates()
  m <- ct_models()
  x <- hrcomptab(r, list(m, m), rows = list(1, 4:5))
  y <- hrcomptab(r, list(m, m), rows = list(1, 4:5), compact = FALSE, separator = NULL, section = NULL)
  expect_identical(cells(y), cells(x))
  expect_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), compact = TRUE), "not allowed")
  expect_identical(cells(comptab(r, list(m, m), rows = list(1, 4:5), compact = FALSE)), cells(x))
})

test_that("review P3-5: hrcomptab() takes effect, reflabel, outcomemap in comptab()'s order", {
  expect_identical(names(formals(hrcomptab))[1:7], names(formals(comptab))[1:7])
  r <- ct_rates()
  m <- ct_models()
  x <- hrcomptab(r, list(m, m), list(1, 4:5), NULL, "HR", "Ref.")
  expect_identical(x$header[[2]]$text[5], "HR (95% CI)")
  expect_identical(body_cell(x, "No", 5), "Ref.")
})

test_that("W01: explicit sheet without workbook gives a classed warning", {
  r <- ct_rates()
  m <- ct_models()
  expect_warning(comptab(r, list(m, m), rows = list(1, 4:5), sheet = "Zed"), class = "tabtools_warning_sheet_without_workbook")
  expect_warning(hrcomptab(r, list(m, m), rows = list(1, 4:5), sheet = "Zed"), class = "tabtools_warning_sheet_without_workbook")
})

test_that("P2-2: sheet = NULL means the default sheet in comptab() and hrcomptab()", {
  r <- ct_rates()
  m <- ct_models()
  expect_no_error(comptab(r, list(m, m), rows = list(1, 4:5), sheet = NULL))
  expect_no_error(hrcomptab(r, list(m, m), rows = list(1, 4:5), sheet = NULL))
})

test_that("B04: a named modeltables list gives no base R row-name warnings", {
  set.seed(22)
  d <- data.frame(x = rnorm(300))
  d$y1 <- rbinom(300, 1, plogis(0.3 * d$x))
  t2 <- regtab(glm(y1 ~ x + I(x^2), binomial, d))
  expect_no_warning(ct <- comptab(modeltables = list(A = t2, B = t2), rows = list(1:2, 1:2)))
  expect_identical(cells(ct), cells(comptab(modeltables = list(t2, t2), rows = list(1:2, 1:2))))
  fd <- as_forest_data(ct)
  expect_identical(rownames(fd), as.character(seq_len(nrow(fd))))
})
