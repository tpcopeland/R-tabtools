# Regression tests for the Phase 4 review findings (IMPLEMENTATION_PLAN.md,
# "Phase 4 review outcome"). Stata expectations: the golden scenarios
# R30-R52 (test-golden-regtab.R) and tests/testthat/fixtures/regtab_review/
# (qa/stata/make_regtab_review.do).

review_fixture <- function(name) test_path("fixtures", "regtab_review", name)
`%|%` <- function(x, y) ifelse(is.na(x), y, x)

oim_stata <- function(model) {
  d <- utils::read.csv(review_fixture("oim_se.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  d <- d[d$model == model & d$se > 0 & !startsWith(d$term, "/"), ]
  d$term <- sub("^[^:]*:", "", d$term)
  d$term[d$term == "_cons"] <- "(Intercept)"
  d$term <- unname(c("2.grp" = "grpMid", "3.grp" = "grpHigh")[d$term] %|% d$term)
  d
}

# The fit with its coefficients replaced by Stata's, so the variance formula
# is compared at identical estimates.
at_stata_b <- function(fit, st) {
  fit$coefficients[st$term] <- st$b
  fit
}

# ---------------------------------------------------------------------------
# P0-1: observed information

test_that("probit, cloglog and identity-link SEs equal Stata's vce(oim) at Stata's estimates", {
  a <- golden_fixture("auto")
  nb <- golden_fixture("nbsim", factors = "grp")
  fits <- list(
    probit = glm(foreign ~ mpg + weight, binomial("probit"), a),
    cloglog = glm(foreign ~ mpg + weight, binomial("cloglog"), a),
    poisson_identity = glm(y ~ x1 + grp, poisson("identity"), nb,
                           start = oim_stata("poisson_identity")$b[c(4, 1:3)])
  )
  for (m in names(fits)) {
    st <- oim_stata(m)
    se <- sqrt(diag(tabtools:::.rt_vcov(at_stata_b(fits[[m]], st))))[st$term]
    expect_equal(unname(se), st$se, tolerance = 1e-10, label = m)
    # R's own vcov() (expected information, previous iterate) is far off.
    expect_gt(max(abs(sqrt(diag(vcov(fits[[m]])))[st$term] / st$se - 1)), 1e-4)
  }
})

test_that("Gamma-log SEs are the observed information times the Pearson scale (Stata glm)", {
  a <- golden_fixture("auto")
  f <- glm(price ~ mpg + weight, Gamma("log"), a, control = glm.control(epsilon = 1e-14, maxit = 100))
  st <- oim_stata("gamma_log")
  se <- sqrt(diag(tabtools:::.rt_vcov(f)))[st$term]
  expect_equal(unname(se), st$se, tolerance = 1e-8)
})

test_that("glm.nb SEs come from the joint observed information over (beta, ln alpha)", {
  skip_if_not_installed("MASS")
  nb <- golden_fixture("nbsim", factors = "grp")
  f <- MASS::glm.nb(y ~ x1 + grp, nb, control = glm.control(epsilon = 1e-14, maxit = 100))
  st <- oim_stata("nbreg")
  se <- sqrt(diag(tabtools:::.rt_vcov(f)))[st$term]
  expect_equal(unname(se), st$se, tolerance = 1e-8)
  # The expected information (R's vcov) differs in the 3rd digit.
  expect_gt(max(abs(sqrt(diag(vcov(f)))[st$term] / st$se - 1)), 1e-4)
})

test_that("canonical links keep the Fisher information at the final estimates", {
  a <- golden_fixture("auto")
  f <- glm(foreign ~ mpg + weight, binomial, a)
  X <- model.matrix(f)
  mu <- plogis(drop(X %*% coef(f)))
  V <- solve(crossprod(X, X * (mu * (1 - mu))))
  expect_equal(tabtools:::.rt_vcov(f), V, tolerance = 1e-12, ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# P0-2 and the vce argument

test_that("glm with an estimated dispersion uses z; vce = 'model' restores vcov() and t", {
  a <- golden_fixture("auto")
  f <- glm(price ~ mpg + weight, gaussian, a)
  w <- tabtools:::tt_wald(f)
  se <- sqrt(diag(vcov(f)))
  expect_equal(w$conf.high, unname(coef(f) + qnorm(0.975) * se), tolerance = 1e-12)
  expect_equal(w$p.value, unname(2 * pnorm(-abs(coef(f) / se))), tolerance = 1e-12)
  wm <- tabtools:::tt_wald(f, vce = "model")
  expect_equal(wm$p.value, unname(summary(f)$coefficients[, 4]), tolerance = 1e-12)
  expect_equal(wm$conf.high, unname(coef(f) + qt(0.975, f$df.residual) * se), tolerance = 1e-12)
  # lm keeps t (regress).
  g <- lm(price ~ mpg + weight, a)
  expect_equal(tabtools:::tt_wald(g)$p.value, unname(summary(g)$coefficients[, 4]), tolerance = 1e-12)
})

test_that("vce = 'model' uses each fit's own vcov()", {
  a <- golden_fixture("auto")
  f <- glm(foreign ~ mpg + weight, binomial("probit"), a)
  expect_identical(tabtools:::.rt_vcov(f, "model"), vcov(f))
  tt <- regtab(f, vce = "model", digits = 4)
  se <- sqrt(diag(vcov(f)))
  expect_identical(tt$body[1, 3], sprintf("(%.4f, %.4f)", coef(f)[2] - qnorm(0.975) * se[2],
                                          coef(f)[2] + qnorm(0.975) * se[2]))
  # Milestone 5w: "robust" is a glm vce now; unknown values are refused.
  expect_error(regtab(f, vce = "hc3"), "must be one of")
})

# ---------------------------------------------------------------------------
# P0-3: glm subclasses

test_that("subclasses of lm/glm (gam, fake svyglm) are refused, not given model-based SEs", {
  a <- golden_fixture("auto")
  g <- lm(price ~ mpg, a)
  class(g) <- c("gam", "glm", "lm")
  expect_error(regtab(g), "does not support <gam>")
  f <- glm(foreign ~ mpg, binomial, a)
  class(f) <- c("svyglm", class(f))
  # svyglm is supported since milestone 5w, but only with its design (and
  # without the survey package it is refused naming the package).
  if (!requireNamespace("survey", quietly = TRUE)) {
    expect_error(regtab(f), "needs the survey package")
    skip("survey not installed")
  }
  expect_error(regtab(f), "no survey design")
  expect_error(regtab(f, vce = "model"), "no survey design")
  d <- golden_fixture("cohort")
  des <- survey::svydesign(ids = ~region, weights = ~iptw, data = d)
  s <- suppressWarnings(survey::svyglm(cv_event ~ treated + female, design = des, family = binomial))
  # Milestone 5w (task 5.10): a genuine svyglm is Stata's svy: (goldens
  # W08, W08b; test-regtab-vce.R).
  expect_s3_class(regtab(s), "tt_table")
  expect_identical(tabtools:::tt_wald_df(s), survey::degf(des))
})

# ---------------------------------------------------------------------------
# P0-6: contrasts and multi-column terms

test_that("non-treatment contrasts and ordered factors are refused", {
  a <- golden_fixture("auto", factors = "pclass")
  a$ord <- factor(a$pclass, ordered = TRUE)
  expect_error(regtab(lm(price ~ ord + mpg, a)), "ordered factor.*ordered = FALSE")
  expect_error(regtab(lm(price ~ pclass + mpg, a, contrasts = list(pclass = "contr.sum"))),
               "contr.sum.*treatment contrasts")
  expect_error(regtab(lm(price ~ pclass + mpg, a, contrasts = list(pclass = "contr.helmert"))),
               "contr.helmert")
  cm <- matrix(c(-1, 1, 0, -1, 0, 1), 3)
  expect_error(regtab(lm(price ~ pclass + mpg, a, contrasts = list(pclass = cm))), "custom")
  # Treatment contrasts with another base, ordered with treatment contrasts: fine.
  tt <- regtab(lm(price ~ pclass + mpg, a, contrasts = list(pclass = contr.treatment(3, base = 2))))
  expect_identical(tt$body$c2[tt$body$c1 == "  Mid-range"], "Reference")
  tt2 <- regtab(lm(price ~ ord + mpg, a, contrasts = list(ord = "contr.treatment")))
  expect_identical(tt2$body$c2[tt2$body$c1 == "  Budget"], "Reference")
})

test_that("a term with several columns shows one row per coefficient", {
  a <- golden_fixture("auto")
  a$M <- cbind(m1 = a$mpg, m2 = a$weight)
  f <- lm(price ~ M + turn, a)
  tt <- regtab(f)
  expect_identical(tt$body$c1[1:2], c("Mm1", "Mm2"))
  expect_identical(tt$body$c2[1:2], sprintf("%.2f", coef(f)[c("Mm1", "Mm2")]))
  tp <- regtab(lm(price ~ poly(mpg, 2) + turn, a))
  expect_identical(tp$body$c1[1:2], c("poly(mpg, 2)1", "poly(mpg, 2)2"))
  expect_identical(nrow(tt$stored$table), 4L)
})

# ---------------------------------------------------------------------------
# P0-7: I(x^2)

test_that("foreign * I(mpg^2) is three-way for fvgen and mpg#mpg natively", {
  a <- golden_fixture("auto", factors = "foreign")
  f <- lm(price ~ foreign * I(mpg^2), a)
  expect_error(regtab(f), "two-way interactions only.*I\\(mpg\\^2\\)")
  tt <- regtab(f, interactions = "native")
  expect_true(all(c("mpg#mpg", "foreign#mpg#mpg", "1.foreign#mpg#mpg") %in% tt$body$c1))
})

test_that("I(x^2) keeps x's label after subset = and without x in the model", {
  a <- golden_fixture("auto")
  tt <- regtab(lm(price ~ I(mpg^2) + weight, a, subset = rep78 > 2))
  expect_identical(tt$body$c1[1], "Mileage (mpg)²")
})

# ---------------------------------------------------------------------------
# P0-5, P2-1: fvgen labels

test_that("fvgen labels are cut to 80 characters, vsref included", {
  d <- golden_fixture("autolong", factors = c("foreign", "pclass"))
  levels(d$pclass)[2] <- strrep("m", 90)
  tt <- regtab(lm(price ~ foreign * pclass, d), vsref = "(vs. @)")
  lab <- tt$body$c1
  expect_true(all(nchar(lab) <= 80))
  expect_true(any(lab == strrep("m", 80)))
  expect_true(any(startsWith(lab, "Foreign × mmmm")))
})

test_that("xsymbol joins fvgen parts with a space on each side; empty means the default", {
  a <- golden_fixture("auto", factors = "foreign")
  f <- lm(price ~ foreign * mpg, a)
  expect_identical(regtab(f, xsymbol = "x")$body$c1[3], "Foreign x Mileage (mpg)")
  expect_identical(regtab(f, xsymbol = "")$body$c1[3], "Foreign \u00d7 Mileage (mpg)")
})

# ---------------------------------------------------------------------------
# P0-8, P0-9: statistics

test_that("weighted lm log-likelihood is regress [aweight]'s", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg + weight, a, weights = turn)
  st <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(st$ll, -684.28042747189443, tolerance = 1e-12)
  g <- lm(price ~ mpg + weight, a)
  expect_equal(tabtools:::tt_model_stats(g, tabtools:::tt_model_info(g))$ll, as.numeric(logLik(g)))
})

test_that("glm.nb gets a Pseudo R2 against the constant-only NB", {
  skip_if_not_installed("MASS")
  d <- golden_fixture("nbsim")
  f <- MASS::glm.nb(y ~ x1, d)
  st <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(st$r2_p, 0.030134591558127144, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# P1-1: codes

test_that("tt_as_factor keeps Stata codes and matches haven::as_factor's levels", {
  x <- haven::labelled(c(0, 1, 1, 0, NA, 3), c(Domestic = 0, Foreign = 1), label = "Car origin")
  f <- tt_as_factor(x)
  expect_identical(levels(f), levels(haven::as_factor(x, levels = "default")))
  expect_identical(levels(f), c("Domestic", "Foreign", "3"))
  expect_identical(as.character(f), as.character(haven::as_factor(x, levels = "default")))
  expect_identical(attr(f, "labels"), c(Domestic = 0, Foreign = 1))
  expect_identical(attr(f, "label"), "Car origin")
  expect_identical(tabtools:::.rt_codes(f), c(Domestic = "0", Foreign = "1", "3" = "3"), ignore_attr = TRUE)
  d <- data.frame(a = 1:2)
  d$x <- haven::labelled(c(2, 1), c(Low = 1, High = 2))
  d$n <- c(5, 3)
  out <- tt_as_factor(d)
  expect_identical(levels(out$x), c("Low", "High"))
  expect_identical(out$n, c(5, 3))
  expect_identical(levels(tt_as_factor(d, vars = "n")$n), c("3", "5"))
  expect_error(tt_as_factor(d, vars = "zz"), "unknown column")
  expect_error(tt_as_factor(haven::labelled(c(1, 2), c(A = 1, A = 2))), "more than one code")
})

test_that("R13 with plain haven::as_factor() input: positional codes and a note", {
  raw <- as.data.frame(haven::read_dta(golden_fixture_path("auto")))
  u <- raw
  u$foreign <- haven::as_factor(u$foreign)
  u$rep78 <- factor(u$rep78)
  f <- lm(price ~ foreign * rep78, u)
  expect_message(tt <- regtab(f, interactions = "native", models = "Native"), "level positions")
  # Documented behaviour: foreign's levels are keyed by position (1, 2), so
  # the native rows read 1.foreign#... / 2.foreign#... where Stata has 0/1.
  lab <- tt$body$c1
  expect_true("1.foreign#1.rep78" %in% lab && "2.foreign#5.rep78" %in% lab)
  expect_false(any(startsWith(lab, "0.foreign")))
  # Through tt_as_factor() the golden matches (and no note).
  g <- tt_as_factor(raw, vars = c("foreign", "rep78"))
  expect_no_message(tt2 <- regtab(lm(price ~ foreign * rep78, g), interactions = "native", models = "Native"))
  want <- golden_read_cells("R13")
  mm <- golden_compare_cells(tt2, want)
  # The one difference allowed is the knife-edge cell of
  # regtab_known_divergences$R13 (test-golden-regtab.R): 2.rep78 is exactly
  # 1403.125 give or take 1e-11, and lm()'s last bits depend on the BLAS
  # (level-1 kernels chosen by the CPU), so R prints 1403.13 on some CI
  # runners and Stata's 1403.12 on others (ubuntu oldrel-1, 2026-09-26).
  expect_true(nrow(mm) == 0L || (identical(mm$want, "1403.12") && identical(mm$got, "1403.13")),
              label = paste(utils::capture.output(print(mm)), collapse = "\n"))
})

test_that("drop = '1.foreign' on a plain factor drops the first level, with a note", {
  a <- golden_fixture("auto")
  u <- a
  u$foreign <- factor(u$foreign, 0:1, c("Domestic", "Foreign"))
  attr(u$foreign, "label") <- "Car origin"
  expect_message(tt <- regtab(lm(price ~ mpg + foreign, u), drop = "1.foreign"), "level positions")
  expect_identical(tt$body$c1, c("Mileage (mpg)", "Car origin", "  Foreign", "Intercept"))
  g <- golden_fixture("auto", factors = "foreign")
  expect_no_message(tt2 <- regtab(lm(price ~ mpg + foreign, g), drop = "1.foreign"))
  expect_identical(tt2$body$c1, c("Mileage (mpg)", "Car origin", "  Domestic", "Intercept"))
  # labelmatch and fvgen-mode keys give no note.
  expect_no_message(regtab(lm(price ~ mpg + foreign, u), drop = "Foreign", labelmatch = TRUE))
})

# ---------------------------------------------------------------------------
# P1-2 (M31), P1-3: forest data

test_that("as_forest_data() equals Stata's eplotframe() with omitted and empty cells (R52)", {
  a <- golden_fixture("auto", factors = c("foreign", "rep78"))
  tt <- regtab(lm(price ~ mpg + mpg_dup + foreign * rep78, a), interactions = "native",
               compact = TRUE, nopvalue = TRUE, models = "Constrained")
  fd <- as_forest_data(tt)
  want <- utils::read.csv(review_fixture("eplot_R52.csv"), stringsAsFactors = FALSE,
                          na.strings = "", strip.white = FALSE, encoding = "UTF-8")
  expect_identical(fd$label, want$label)
  expect_identical(fd$rowtype, want$rowtype)
  expect_identical(fd$source_row, want$source_row)
  expect_identical(fd$model, want$model)
  expect_identical(fd$model_label, want$model_label)
  for (v in c("estimate", "ll", "ul", "pvalue")) {
    expect_identical(is.na(fd[[v]]), is.na(want[[v]]), label = v)
    ok <- !is.na(want[[v]])
    expect_equal(fd[[v]][ok], want[[v]][ok], tolerance = 1e-9, label = v)
  }
  ch <- utils::read.csv(review_fixture("eplot_R52_chars.csv"), stringsAsFactors = FALSE)
  chars <- stats::setNames(ch$value, sub("^tabtools_", "", ch$name))
  expect_identical(attr(fd, "statistic_ids"), chars[["statistic_ids"]])
  expect_identical(attr(fd, "source"), chars[["source"]])
  expect_identical(as.character(attr(fd, "ci_level")), chars[["ci_level"]])
  expect_identical(attr(fd, "effect_scale"), chars[["effect_scale_1"]])
  expect_identical(attr(fd, "outcome_id"), chars[["outcome_id_1"]])
  # The table itself keeps frame()'s statistic ids.
  expect_identical(tt$meta$frame$statistic_ids, "estimate_ci")
})

# ---------------------------------------------------------------------------
# P2-3, P2-4, P2-7, P3-2, P3-4

test_that("labels are not restored from data that changed after the fit", {
  d <- golden_fixture("auto", factors = "foreign")
  f <- lm(price ~ mpg + foreign, data = d, subset = rep78 > 2)
  expect_no_warning(tt <- regtab(f))
  expect_identical(tt$body$c1[1], "Mileage (mpg)")
  d <- mtcars
  expect_warning(tt <- regtab(f), "could not be restored")
  expect_identical(tt$body$c1[1], "mpg")
})

test_that("perfect prediction warns (QA v1015 C) instead of printing silently", {
  a <- golden_fixture("auto", factors = "rep78")
  f <- suppressWarnings(glm(foreign ~ mpg + rep78, binomial, a))
  expect_warning(tt <- regtab(f), "separation")
  expect_no_warning(regtab(glm(foreign ~ mpg, binomial, a)))
})

test_that("coxph on finegray() data is a Fine-Gray (SHR) model (Phase 5a, R20)", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")
  d$event_type <- factor(d$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = d, etype = "cv")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + female, weights = fgwt, data = fg)
  tt <- regtab(f)
  expect_identical(tt$stored$coef_label, "SHR")
  expect_identical(tt$body[[1]], c("Treatment group", "Female sex"))
})

test_that("extra model labels warn; a named list names the models", {
  a <- golden_fixture("auto")
  f1 <- lm(price ~ mpg, a)
  f2 <- lm(price ~ mpg + weight, a)
  expect_warning(tt <- regtab(f1, models = c("A", "B")), "extra labels are ignored")
  expect_identical(tt$header[[1]]$text[2], "A")
  tt2 <- regtab(list(Crude = f1, Adjusted = f2))
  expect_identical(tt2$header[[1]]$text[c(2, 5)], c("Crude", "Adjusted"))
  expect_identical(regtab(list(Crude = f1, Adjusted = f2), models = c("X", "Y"))$header[[1]]$text[2], "X")
})

test_that("addrow numbers print in full decimal form", {
  a <- golden_fixture("auto")
  tt <- regtab(lm(price ~ mpg, a), addrow = list(Big = 100000, Small = 0.00001, Third = 1 / 3))
  n <- nrow(tt$body)
  expect_identical(tt$body$c2[(n - 2):n], c("100000", "0.00001", "0.333333333333333"))
})
