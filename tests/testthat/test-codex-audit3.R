# Regressions for the codex audit of 2026-09-27 (R-Dev
# _take_action/codexaudit.md, findings F01-F09; F10 is in test-qa-lane.R).
# Each test failed on the audited snapshot (commit 04a9080).

# F01 ----

am_fits <- function() {
  d1 <- mtcars
  d1$am <- factor(d1$am, 0:1, c("Auto", "Man"))
  d1$vs <- factor(d1$vs, 0:1, c("V", "S"))
  d2 <- d1
  d2$am <- relevel(d2$am, "Man")
  d2$vs <- relevel(d2$vs, "S")
  list(d1 = d1, d2 = d2)
}

forest_est <- function(t, label, model) {
  f <- as_forest_data(t)
  f$estimate[trimws(f$label) == label & f$model == model]
}

test_that("F01: effecttab() joins a relevelled factor's predictions by level, not by position", {
  skip_if_not_installed("marginaleffects")
  d <- am_fits()
  p1 <- marginaleffects::avg_predictions(lm(mpg ~ am, data = d$d1), variables = "am")
  p2 <- marginaleffects::avg_predictions(lm(mpg ~ am, data = d$d2), variables = "am")
  t <- effecttab(A = p1, B = p2, type = "margins")
  for (lev in c("Auto", "Man")) {
    expect_equal(forest_est(t, lev, 1L), p1$estimate[p1$am == lev])
    expect_equal(forest_est(t, lev, 2L), p2$estimate[p2$am == lev])
  }
  # The body shows each level once, both models' estimates on its row.
  expect_identical(t$body$c2[2:3], t$body$c5[2:3])
})

test_that("F01: a subset missing a level keeps each estimate on its own level, in either model order", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$cyl <- factor(d$cyl, c(4, 6, 8), c("four", "six", "eight"))
  q1 <- marginaleffects::avg_predictions(lm(mpg ~ cyl, d), variables = "cyl")
  q2 <- marginaleffects::avg_predictions(lm(mpg ~ cyl, droplevels(d[d$cyl != "four", ])), variables = "cyl")
  for (t in list(effecttab(A = q1, B = q2, type = "margins"), effecttab(B = q2, A = q1, type = "margins"))) {
    f <- as_forest_data(t)
    b <- f[f$model_label == "B", ]
    expect_equal(b$estimate[trimws(b$label) == "six"], q2$estimate[q2$cyl == "six"])
    expect_equal(b$estimate[trimws(b$label) == "eight"], q2$estimate[q2$cyl == "eight"])
    expect_false(any(trimws(b$label) == "four"))
    expect_identical(trimws(t$body$c1[-1]), c("four", "six", "eight"))
  }
})

test_that("F01: combinations of relevelled factors (by = c(...)) are joined by level", {
  skip_if_not_installed("marginaleffects")
  d <- am_fits()
  r1 <- marginaleffects::avg_predictions(lm(mpg ~ am * vs, d$d1), by = c("am", "vs"))
  r2 <- marginaleffects::avg_predictions(lm(mpg ~ am * vs, d$d2), by = c("am", "vs"))
  t <- effecttab(A = r1, B = r2, type = "margins")
  expect_identical(t$body$c2[-1], t$body$c5[-1])
  expect_equal(forest_est(t, "Man#V", 2L), r2$estimate[r2$am == "Man" & r2$vs == "V"])
})

test_that("F01: teffects contrasts of a relevelled treatment are different rows", {
  skip_if_not_installed("marginaleffects")
  d <- am_fits()
  c1 <- marginaleffects::avg_comparisons(lm(mpg ~ am, d$d1), variables = "am")
  c2 <- marginaleffects::avg_comparisons(lm(mpg ~ am, d$d2), variables = "am")
  t <- suppressMessages(effecttab(A = c1, B = c2, type = "teffects", clean = TRUE))
  expect_identical(t$body$c1, c("Man vs Auto", "Auto vs Man"))
  expect_identical(t$body$c5[1], "")
  expect_identical(t$body$c2[2], "")
})

test_that("F01: pairwise contrasts of a relevelled factor are joined by level", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$cyl <- factor(d$cyl, c(4, 6, 8), c("four", "six", "eight"))
  d2 <- d
  d2$cyl <- relevel(d2$cyl, "eight")
  k1 <- marginaleffects::avg_comparisons(lm(mpg ~ cyl, d), variables = list(cyl = "pairwise"))
  k2 <- marginaleffects::avg_comparisons(lm(mpg ~ cyl, d2), variables = list(cyl = "pairwise"))
  t <- effecttab(A = k1, B = k2, type = "margins")
  expect_equal(forest_est(t, "six vs four", 2L), k2$estimate[k2$contrast == "six - four"])
  expect_equal(forest_est(t, "four vs eight", 2L), k2$estimate[k2$contrast == "four - eight"])
  expect_length(forest_est(t, "eight vs four", 2L), 0L)
  # Beside the levels' predictions, a pairwise result is not a mismatch.
  q1 <- marginaleffects::avg_predictions(lm(mpg ~ cyl, d), variables = "cyl")
  expect_no_error(effecttab(A = q1, B = k2, type = "margins"))
})

test_that("F01: different continuous increments are different rows", {
  skip_if_not_installed("marginaleffects")
  f <- lm(mpg ~ wt, data = mtcars)
  a <- marginaleffects::avg_comparisons(f, variables = list(wt = 1))
  b <- marginaleffects::avg_comparisons(f, variables = list(wt = 2))
  t <- effecttab(One = a, Two = b)
  expect_identical(t$body$c1, c("wt (+1)", "wt (+2)"))
  expect_equal(forest_est(t, "wt (+2)", 2L), b$estimate)
  expect_length(forest_est(t, "wt (+1)", 2L), 0L)
  # The same increment in two models still joins.
  expect_identical(nrow(effecttab(One = a, Two = a)$body), 1L)
})

test_that("F01: levels coded by position in one model and by value in another are refused", {
  skip_if_not_installed("marginaleffects")
  d1 <- am_fits()$d1
  d3 <- d1
  attr(d3$am, "labels") <- c(Auto = 0, Man = 1)
  p1 <- marginaleffects::avg_predictions(lm(mpg ~ am, d1), variables = "am")
  p3 <- marginaleffects::avg_predictions(lm(mpg ~ am, d3), variables = "am")
  expect_error(effecttab(A = p1, B = p3, type = "margins"), class = "tabtools_error_effect_row_mismatch")
  expect_no_error(effecttab(A = p3, B = p3, type = "margins"))
  # A variable label set in one dataset only is not a different row.
  d2 <- mtcars
  attr(d2$wt, "label") <- "Weight"
  a <- marginaleffects::avg_comparisons(lm(mpg ~ wt, mtcars), variables = list(wt = 1))
  b <- marginaleffects::avg_comparisons(lm(mpg ~ wt, d2), variables = list(wt = 1))
  expect_identical(nrow(effecttab(A = a, B = b)$body), 1L)
})

# F02 ----

test_that("F02: hrcomptab() places categories differing only by case exactly, with the right reference", {
  r <- stratetab(ct_block(c("A", "a"), c(5, 8), c(100, 200)), outlabels = "Death", outcomeids = "death",
                 explabels = "Dose")
  # Base "a": model row 3 is A = 1.30.
  m <- regtab(ct_model("death", levels = c("A", "a"), ref = 2))
  b <- hrcomptab(r, m, rows = 3)$body
  expect_identical(b$c5[2:3], c("1.30 (1.04, 1.62)", "Reference"))
  # Base "A": model row 4 is a = 1.30.
  m <- regtab(ct_model("death", levels = c("A", "a"), ref = 1))
  b <- hrcomptab(r, m, rows = 4)$body
  expect_identical(b$c5[2:3], c("Reference", "1.30 (1.04, 1.62)"))
  # A match by case alone still places a row...
  r2 <- stratetab(ct_block(c("A", "B"), c(5, 8), c(100, 200)), outlabels = "Death", outcomeids = "death",
                  explabels = "Dose")
  b <- hrcomptab(r2, regtab(ct_model("death", levels = c("a", "b"), ref = 1)), rows = 4)$body
  expect_identical(b$c5[2:3], c("Reference", "1.30 (1.04, 1.62)"))
  # ...but one matching two categories by case alone is refused.
  r3 <- stratetab(ct_block(c("x", "aa", "Aa"), c(5, 8, 3), c(100, 200, 50)), outlabels = "Death",
                  outcomeids = "death", explabels = "Dose")
  expect_error(hrcomptab(r3, regtab(ct_model("death", levels = c("x", "AA", "b"), ref = 1)), rows = 4:5),
               "more than one category")
})

# F03 ----

test_that("F03: glm.nb() with an identity or sqrt link is shown as coefficients, not IRRs", {
  skip_if_not_installed("MASS")
  set.seed(235)
  d <- data.frame(x = stats::runif(150, 0, 3))
  d$y <- stats::rnbinom(nrow(d), mu = 4 + 2 * d$x, size = 2)
  # (glm.nb() takes its link unevaluated, so each is spelled out.)
  for (f in list(MASS::glm.nb(y ~ x, data = d, link = identity), MASS::glm.nb(y ~ x, data = d, link = sqrt))) {
    t <- regtab(f, stats = "n")
    expect_identical(t$header[[2]]$text[2], "Coef.")
    fd <- as_forest_data(t)
    expect_equal(fd$estimate[fd$label == "x"], unname(stats::coef(f)["x"]))
    expect_true(fd$ll[fd$label == "x"] < stats::coef(f)["x"])
  }
  f <- MASS::glm.nb(y ~ x, data = d)
  t <- regtab(f, stats = "n")
  expect_identical(t$header[[2]]$text[2], "IRR")
  expect_equal(as_forest_data(t)$estimate[1], exp(unname(stats::coef(f)["x"])))
})

# F04 ----

test_that("F04: vce = \"model\" uses the normal distribution for a family with a fixed dispersion", {
  skip_if(getRversion() < "4.3.0", "family$dispersion is read by summary.glm() since R 4.3.0")
  fam <- stats::gaussian()
  fam$dispersion <- 1
  f <- stats::glm(mpg ~ wt, data = mtcars[1:8, ], family = fam)
  s <- summary(f)$coefficients
  fd <- as_forest_data(regtab(f, vce = "model", stats = "n"))
  expect_equal(fd$pvalue[fd$label == "wt"], s["wt", 4])
  expect_equal(fd$ll[fd$label == "wt"], s["wt", 1] - stats::qnorm(0.975) * s["wt", 2])
  # Estimated dispersion: t, as summary() does.
  g <- stats::glm(mpg ~ wt, data = mtcars[1:8, ])
  fd <- as_forest_data(regtab(g, vce = "model", stats = "n"))
  expect_equal(fd$pvalue[fd$label == "wt"], summary(g)$coefficients["wt", 4])
})

# F05 ----

test_that("F05: comptab()'s forest data follow the composite's model columns and relabel()", {
  m <- ct_models()
  rev <- ct_models(outcomes = c("relapse", "death"), labels = c("Relapse", "Death"))
  z <- comptab(list(m, rev), rows = list(1, 1), section = c("First", "Second"))
  e <- as_forest_data(z)
  e <- e[e$rowtype == "effect", ]
  # Model 1 is Death in both sections, as the table's first column shows.
  expect_identical(e$model, c(1L, 2L, 1L, 2L))
  expect_identical(e$model_label, c("Death", "Relapse", "Death", "Relapse"))
  expect_identical(sprintf("%.2f", e$estimate[e$model == 1L]), z$body$c2[c(2, 4)])
  expect_identical(sprintf("%.2f", e$estimate[e$model == 2L]), z$body$c5[c(2, 4)])
  # The second table's own index is kept as provenance.
  expect_identical(e$source_model, c(1L, 2L, 2L, 1L))
  z <- comptab(m, rows = 1, relabel = c("1" = "Custom treatment label"))
  expect_identical(unique(as_forest_data(z)$label), "Custom treatment label")
  expect_identical(z$body$c1, "Custom treatment label")
  # A relabelled section heading is relabelled in the forest data too.
  z <- comptab(list(m, rev), rows = list(1, 1), section = c("First", "Second"), relabel = c("3" = "Later"))
  f <- as_forest_data(z)
  expect_identical(f$label[f$rowtype == "section"], c("First", "Later"))
})

# F06 ----

test_that("F06: a ratio with an explicit null keeps that test and names only it", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  f <- stats::glm(vs ~ am + wt, family = stats::binomial, data = d)
  r <- marginaleffects::avg_comparisons(f, variables = "am", comparison = "ratio", hypothesis = 2)
  t <- effecttab(r)
  expect_equal(t$stored$table[1, 2], r$p.value)
  expect_identical(t$footnote, "P-values test a null value of 2.")
  # The null given through a variable is read too.
  h <- 2
  r <- marginaleffects::avg_comparisons(f, variables = "am", comparison = "ratio", hypothesis = h)
  expect_identical(effecttab(r)$footnote, "P-values test a null value of 2.")
  # Without a null, the ratio's p-value is recomputed against 1.
  r <- marginaleffects::avg_comparisons(f, variables = "am", comparison = "ratio")
  t <- effecttab(r)
  expect_identical(t$footnote, "P-values of ratios test a ratio of 1.")
  expect_false(isTRUE(all.equal(t$stored$table[1, 2], r$p.value)))
})

# F07 ----

test_that("F07: tiny importance weights give the cells of weights of ordinary scale", {
  d <- data.frame(x = c(1, 2, 3, 4, 2, 3, 4, 5), a = rep(1:2, each = 4), w = 1)
  f <- function(d) table1_tc(d, vars = c(x = "contn %8.3f"), by = "a", wt = "w")$body
  unit <- f(d)
  expect_identical(unit$c2[2], "2.500±1.291")
  for (s in c(1e-310, 1e-200)) {
    d$w <- s
    expect_identical(f(d), unit)
  }
  set.seed(1)
  e <- data.frame(x = stats::rnorm(40, 5), c = sample(c("a", "b"), 40, TRUE), g = rep(1:2, 20),
                  w0 = stats::runif(40, 0.2, 3))
  g <- function(w) {
    e$w <- w
    table1_tc(e, vars = c(x = "contn %8.3f", c = "cat"), by = "g", wt = "w", smd = TRUE)$body
  }
  expect_identical(g(e$w0 * 1e-250), g(e$w0))
})

# F08 ----

test_that("F08: weights near the double limit give finite quantiles and no false truncation", {
  z <- wttab(rep(1e308, 4), trunc = c(0.25, 0.75))
  s <- z$stored$stats
  expect_true(all(s[, c("p25", "p50", "p75")] == 1e308))
  expect_identical(z$stored$trunc$lower, 1e308)
  expect_identical(z$stored$trunc$n_low + z$stored$trunc$n_high, 0L)
  expect_identical(s$n_trunc[2], 0)
  expect_identical(s$ess, c(4, 4))
  w <- c(1, 5e307, 8e307, 8.5e307, 2)
  s <- wttab(w, trunc = c(0.2, 0.8))$stored$stats
  expect_true(all(is.finite(as.matrix(s[, c("mean", "sd", "p25", "p50", "p75", "ess")]))))
  expect_equal(s$sd[1], stats::sd(w / 2^1020) * 2^1020)
  # Ordinary weights: the SD is sd() itself.
  u <- c(0.3, 1.7, 2.2, 0.9, 1.1)
  expect_identical(wttab(u)$stored$stats$sd, stats::sd(u))
})

# F09 ----

html_text <- function(html, xpath) {
  xml2::xml_text(xml2::xml_find_all(xml2::read_html(html), xpath))
}

literal_table <- function() {
  puttab(data.frame(label = c("<b>bold</b>", "&lt;5", "A_B", "[a](b)"), value = 1:4),
         title = "&lt;5 &amp; A", footnote = "&lt;5 &amp; A_B")
}

test_that("F09: tt_as_tinytable() keeps text literal in rendered HTML", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("xml2")
  x <- literal_table()
  f <- withr::local_tempfile(fileext = ".html")
  tinytable::save_tt(tt_as_tinytable(x), f, overwrite = TRUE)
  expect_identical(html_text(f, ".//tbody//td")[c(1, 3, 5, 7)], c("<b>bold</b>", "&lt;5", "A_B", "[a](b)"))
  expect_identical(trimws(html_text(f, ".//caption")), "&lt;5 &amp; A")
  expect_identical(trimws(html_text(f, ".//tfoot")), "&lt;5 &amp; A_B")
})

test_that("F09: tt_as_gt() and tt_as_gtsummary() keep entity text literal in captions and notes", {
  skip_if_not_installed("gt")
  skip_if_not_installed("xml2")
  x <- literal_table()
  h <- gt::as_raw_html(tt_as_gt(x))
  expect_identical(trimws(html_text(h, ".//caption")), "&lt;5 &amp; A")
  expect_identical(html_text(h, ".//tbody//td")[3], "&lt;5")
  skip_if_not_installed("gtsummary")
  h <- gt::as_raw_html(gtsummary::as_gt(tt_as_gtsummary(x)))
  expect_identical(trimws(html_text(h, ".//caption")), "&lt;5 &amp; A")
  expect_identical(trimws(html_text(h, ".//tfoot")), "&lt;5 &amp; A_B")
})
