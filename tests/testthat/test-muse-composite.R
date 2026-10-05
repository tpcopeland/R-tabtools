# Regressions for the Muse audit and the review of 2026-09-28 (R-Dev
# _take_action/muse_tabtools.md and review-tabtools-2026-09-28.md):
# composite tables, rendering, infrastructure and table1. Each test failed
# on commit 44241e4.

# P0-4 ----

test_that("P0-4: CSV cells a spreadsheet would run as formulas are neutralised", {
  cells <- c("=1+1", "@SUM(A1)", "+cmd|' /C calc'!A0", "-2+cmd|' /C calc'!A0",
             "=HYPERLINK(\"http://x\")", "\tx", "+ HYPERLINK(\"http://x\")", "-Current")
  keep <- c("-0.35 (-0.50, -0.20)", "-", "+1.2", "-1.5e-05", "1.20 (0.90, 1.60)", "Age, years",
            "+ Comorbidities", "- Current smoker")
  x <- puttab(data.frame(label = c(cells, keep), v = seq_along(c(cells, keep))), noheader = TRUE)
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(x, f)
  got <- utils::read.csv(f, header = FALSE, colClasses = "character", strip.white = FALSE)[[1]]
  expect_identical(got, c(paste0("'", cells), keep))
})

# P0-5 ----

p05_rates <- function() {
  b <- ct_block(c("Auto", "Man"), c(5, 8), c(100, 200))
  stratetab(b, outlabels = "vs", outcomeids = "vs", explabels = "am")
}

test_that("P0-5: hrcomptab() refuses an effecttab() difference or prediction headed HR", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  f <- glm(vs ~ am + wt, family = binomial, data = d)
  r <- p05_rates()
  rd <- effecttab(marginaleffects::avg_comparisons(f, variables = "am"), effect = "HR", models = "vs")
  expect_error(hrcomptab(r, rd, rows = 3, outcomemap = "vs"), class = "tabtools_error_not_hazard_ratio")
  pr <- effecttab(marginaleffects::avg_predictions(f, variables = "am"), effect = "HR", type = "margins", models = "vs")
  expect_error(hrcomptab(r, pr, rows = 3, outcomemap = "vs"), class = "tabtools_error_not_hazard_ratio")
  # A ratio headed HR is the user's statement; it still composes.
  lr <- effecttab(marginaleffects::avg_comparisons(f, variables = "am", comparison = "lnratioavg",
                                                   transform = exp), effect = "HR", models = "vs")
  expect_s3_class(hrcomptab(r, lr, rows = 3, outcomemap = "vs"), "tt_table")
  expect_identical(attr(as.data.frame(rd), "effect_additive") %||% rd$meta$frame$effect_additive, TRUE)
})

# Review I1 ----

test_that("I1: tt_as_tinytable() keeps text literal in Markdown output", {
  skip_if_not_installed("tinytable")
  x <- puttab(data.frame(label = c("<b>bold</b>", "&lt;5", "A_B", "*x*", "-0.35 (-0.50, 0.10)"), value = 1:5),
              title = "*cap* &amp; A", footnote = "<i>n</i> A_B")
  f <- withr::local_tempfile(fileext = ".md")
  tinytable::save_tt(tt_as_tinytable(x), f, overwrite = TRUE)
  md <- paste(readLines(f), collapse = "\n")
  for (s in c("\\<b\\>bold\\</b\\>", "\\&lt;5", "A\\_B", "\\*x\\*", "-0.35 (-0.50, 0.10)",
              "\\*cap\\* \\&amp; A", "\\<i\\>n\\</i\\> A\\_B")) {
    expect_true(grepl(s, md, fixed = TRUE), info = s)
  }
  # Rendered, the text is as written.
  skip_on_cran()
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")
  txt <- paste(system2("pandoc", c("-f", "markdown", "-t", "plain", shQuote(f)), stdout = TRUE), collapse = "\n")
  for (s in c("<b>bold</b>", "&lt;5", "A_B", "*x*", "*cap* &amp; A", "<i>n</i> A_B")) {
    expect_true(grepl(s, txt, fixed = TRUE), info = s)
  }
  # HTML output is unchanged: no Markdown backslashes.
  h <- withr::local_tempfile(fileext = ".html")
  tinytable::save_tt(tt_as_tinytable(x), h, overwrite = TRUE)
  expect_false(any(grepl("\\<", readLines(h), fixed = TRUE)))
})

test_that("I1: tt_as_tinytable() keeps text literal in Word output", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("pandoc")
  skip_if_not_installed("xml2")
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc not on PATH")
  x <- puttab(data.frame(label = c("<b>bold</b>", "&lt;5", "A_B", "*x*"), value = 1:4),
              title = "*cap* &amp; A", footnote = "<i>n</i> A_B")
  f <- withr::local_tempfile(fileext = ".docx")
  tinytable::save_tt(tt_as_tinytable(x), f, overwrite = TRUE)
  d <- withr::local_tempdir()
  utils::unzip(f, exdir = d)
  txt <- xml2::xml_text(xml2::xml_find_all(xml2::read_xml(file.path(d, "word", "document.xml")), ".//w:p"))
  expect_true(all(c("*cap* &amp; A", "<b>bold</b>", "&lt;5", "A_B", "*x*", "<i>n</i> A_B") %in% txt))
})

# Review M1 ----

test_that("M1: effecttab() footnote notes name the model they apply to", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  f <- glm(vs ~ am + wt, family = binomial, data = d)
  r1 <- marginaleffects::avg_comparisons(f, variables = "am", comparison = "ratio")
  r2 <- marginaleffects::avg_comparisons(f, variables = "am", comparison = "ratio", hypothesis = 2)
  t <- effecttab(Default = r1, Null2 = r2)
  expect_match(t$footnote, "Default: P-values of ratios test a ratio of 1.", fixed = TRUE)
  expect_match(t$footnote, "Null2: P-values test a null value of 2.", fixed = TRUE)
  # Notes shared by every model stay unprefixed; one model, no prefix.
  t2 <- effecttab(A = r1, B = r1)
  expect_identical(t2$footnote, "P-values of ratios test a ratio of 1.")
  expect_identical(effecttab(r2)$footnote, "P-values test a null value of 2.")
  # Unlabelled models are named by number.
  expect_match(effecttab(r1, r2)$footnote, "Model 2: P-values test a null value of 2.", fixed = TRUE)
})

# Review M2 ----

test_that("M2: tt_rates() blocks with a labelled event default their outcome id to the event name", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$dead <- d$status - 1
  attr(d$dead, "label") <- "Death"
  d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
  r <- stratetab(tt_rates(d, "time", "dead", by = "sex"))
  expect_identical(r$header[[1]]$text[2], "Death")
  expect_identical(r$stored$outcome_ids, "dead")
  cox <- regtab(survival::coxph(survival::Surv(time, dead) ~ sex, data = d, ties = "breslow"))
  h <- hrcomptab(r, cox, rownames = "female")
  expect_s3_class(h, "tt_table")
  # Given outlabels keep the event-name ids too; given outcomeids win.
  r2 <- stratetab(tt_rates(d, "time", "dead", by = "sex"), outlabels = "All-cause death")
  expect_s3_class(hrcomptab(r2, cox, rownames = "female"), "tt_table")
  r3 <- stratetab(tt_rates(d, "time", "dead", by = "sex"), outcomeids = "other")
  expect_error(hrcomptab(r3, cox, rownames = "female"), "could not be matched")
})

# Review M3 ----

test_that("M3: rate-mode forest data carry source_model, so they bind with vertical ones", {
  r <- ct_rates()
  m <- ct_models()
  fr <- as_forest_data(hrcomptab(r, list(m, m), rows = list(1, 4:5)))
  fv <- as_forest_data(comptab(m, rows = 1))
  expect_identical(names(fr), names(fv))
  expect_no_error(rbind(fr, fv))
  eff <- fr$rowtype == "effect"
  # Model k of the composite is the rate outcome k; its source model is the
  # model of that outcome in its own table.
  expect_identical(fr$source_model[eff], fr$model[eff])
  expect_true(all(is.na(fr$source_model[!eff])))
})

# P1-25 ----

test_that("P1-25: stratetab() warns when tt_rates(per =) blocks are scaled off the rate unit", {
  d <- data.frame(t = c(1000, 2000), e = c(1, 1), g = c("a", "b"))
  r1 <- tt_rates(d, "t", "e", by = "g", per = 1000)
  # per = 1000 and ratescale = 1000: rates per million under "Per 1,000 PY".
  expect_warning(stratetab(r1, outlabels = "E", explabels = "G", ratescale = 1000),
                 class = "tabtools_warning_rate_scale")
  # The combination the per error recommends is consistent: no warning.
  expect_no_warning(s <- stratetab(r1, outlabels = "E", explabels = "G", ratescale = 1, pyscale = 1 / 1000))
  expect_identical(s$body$c4[2], "1.0 (0.1, 7.1)")
  expect_no_warning(stratetab(r1, outlabels = "E", explabels = "G", ratescale = 1))
  expect_no_warning(stratetab(r1, outlabels = "E", explabels = "G", ratescale = 1000, unitlabel = "1,000,000"))
  # Blocks of per = 1 are not checked (unitlabel is the user's, as in Stata).
  r0 <- tt_rates(d, "t", "e", by = "g")
  expect_no_warning(stratetab(r0, outlabels = "E", explabels = "G", pyscale = 365.25, ratescale = 365250))
})

# P1-26 ----

test_that("P1-26: effecttab(estimand =) names the estimand the result estimates", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  f <- lm(mpg ~ am + wt, data = d)
  r <- marginaleffects::avg_comparisons(f, variables = "am", newdata = subset(d, am == "Man"))
  t <- effecttab(r, type = "teffects", estimand = "ATT")
  expect_match(t$stored$methods, "on the treated", fixed = TRUE)
  expect_false(grepl("^Average treatment effects were", effecttab(r, type = "teffects", estimand = "ATC")$stored$methods))
  expect_identical(effecttab(r, type = "teffects")$stored$methods,
                   effecttab(r, type = "teffects", estimand = "ATE")$stored$methods)
  expect_error(effecttab(r, estimand = "LATE"), "estimand")
})

# P1-28 ----

test_that("P1-28: a plain ratio whose interval crosses 0 is flagged", {
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x = factor(rep(c("a", "b"), each = 10)), y = rbinom(20, 1, 0.3))
  f <- glm(y ~ x, family = binomial, data = d)
  r <- marginaleffects::avg_comparisons(f, variables = "x", comparison = "ratio")
  skip_if(r$conf.low >= 0, "this seed gives no negative bound")
  expect_warning(t <- effecttab(r), class = "tabtools_warning_ratio_interval")
  expect_match(t$footnote, "lnratioavg", fixed = TRUE)
})

# P1-29 ----

test_that("P1-29: a data frame's std.error tests null.value, and a ratio without one is refused", {
  df <- data.frame(label = "Treated", estimate = 1.5, std.error = 0.2)
  expect_error(effecttab(df, effect = "RR"), class = "tabtools_error_df_null")
  t <- effecttab(cbind(df, null.value = 1), effect = "RR")
  expect_identical(t$body$c4[1], format_p(2 * pnorm(-0.5 / 0.2)))
  # An additive estimate keeps the null of 0.
  t0 <- effecttab(data.frame(label = "Treated", estimate = 0.5, std.error = 0.2), effect = "RD")
  expect_identical(t0$body$c4[1], format_p(2 * pnorm(-0.5 / 0.2)))
  # One bound given without the other is refused, not shown blank.
  expect_error(effecttab(data.frame(label = "Treated", estimate = 1.5, conf.low = 1.1, conf.high = NA,
                                    p.value = 0.01), effect = "RR"),
               class = "tabtools_error_df_half_interval")
})

# P1-31 ----

test_that("P1-31: an indicator row is not placed on a block whose categories are reversed", {
  m <- regtab(ct_model("death"))
  for (cats in list(c("Yes", "No"), c("1", "0"), c("TRUE", "FALSE"))) {
    r <- stratetab(ct_block(cats, c(5, 8), c(100, 200)), outlabels = "Death", outcomeids = "death",
                   explabels = "Treated")
    expect_error(hrcomptab(r, m, rows = 1), class = "tabtools_error_indicator_order")
  }
  r <- stratetab(ct_block(c("No", "Yes"), c(5, 8), c(100, 200)), outlabels = "Death", outcomeids = "death",
                 explabels = "Treated")
  expect_identical(hrcomptab(r, m, rows = 1)$body$c5[3], "0.80 (0.64, 1.00)")
})

# P1-39 ----

test_that("P1-39: tt_write_xlsx() replaces a workbook only once the new one is saved", {
  p <- withr::local_tempfile(fileext = ".xlsx")
  x <- puttab(data.frame(a = "old"))
  tt_write_xlsx(x, p, sheet = "S")
  before <- unname(tools::md5sum(p))
  # A save that fails part-way (after writing some bytes) leaves the old
  # file intact.
  local_mocked_bindings(.xlsx_save_to = function(wb, file) {
    writeLines("partial", file)
    stop("disk full")
  })
  expect_error(tt_write_xlsx(puttab(data.frame(a = "new")), p, sheet = "S"), "disk full")
  expect_identical(unname(tools::md5sum(p)), before)
  # and no temporary file is left beside it.
  left <- setdiff(list.files(dirname(p)), basename(p))
  expect_false(any(startsWith(left, sub("\\.xlsx$", "", basename(p)))))
})

# P1-43, P1-44 ----

test_that("P1-43: sheet names with control characters are refused", {
  p <- withr::local_tempfile(fileext = ".xlsx")
  for (s in c("a\nb", "a\tb", "a\001b")) {
    expect_error(tt_write_xlsx(puttab(data.frame(a = 1)), p, sheet = s), "control character")
  }
})

test_that("P1-44: writer paths must be one non-missing string", {
  x <- puttab(data.frame(a = 1))
  for (bad in list(NA_character_, c("a.md", "b.md"), 123)) {
    expect_error(tt_write_markdown(x, bad), class = "rlang_error")
    expect_error(tt_write_xlsx(x, bad), class = "rlang_error")
  }
  expect_error(tt_write_markdown(x, c("a.md", "b.md")), "single")
})
