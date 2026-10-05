# Regression tests for the Phase 5 review findings (IMPLEMENTATION_PLAN.md,
# "Phase 5 review outcome"). Stata expectations:
# tests/testthat/fixtures/regtab_phase5_review/ (qa/stata/make_regtab_phase5_review.do,
# Stata 17 + tabtools 2.1.12 since C5); the R calls are in helper-regtab-phase5-review.R.

# ---------------------------------------------------------------------------
# Stata fixtures compared cell for cell (exact) and stored result for stored
# result. Stored tolerance per case (absolute below 1, relative above, as
# golden_compare_stored() measures): the smallest 1-2-5 step at or above the
# largest difference observed on 2026-09-25 (in the comment), never below
# the harness default 1e-9. The differences are convergence: pscl's and
# glmmTMB's optimizers, lme4's adaptive quadrature against Stata's
# mode-and-variance quadrature (F03, B17), Stata's own default
# convergence.
p5r_tol <- c(
  C31 = 1e-8,  # 7.8e-9 (nbreg)
  B20 = 5e-6,  # 2.6e-6 (glmmTMB vs menbreg)
  G17 = 5e-6,  # 2.6e-6
  D07 = 2e-7,  # 2.0e-7 (zinb)
  G18 = 2e-7,  # 2.0e-7
  G19 = 1e-8,  # 7.8e-9
  B11 = 2e-9,  # 1.1e-9 (multinom, reltol 1e-14)
  B23 = 1e-5,  # 6.7e-6 (zip)
  G03 = 2e-5,  # 1.8e-5
  G21 = 2e-5,  # 1.8e-5
  G29 = 2e-6,  # 1.1e-6
  D10 = 1e-7,  # 7.1e-8
  G22 = 1e-7,  # 7.1e-8
  F03 = 2e-5,  # 1.9e-5 (var(_cons): lme4 AGQ vs Stata's)
  B17 = 2e-5,  # 1.7e-5
  B18 = 5e-6,  # 2.2e-6
  B05 = 1e-5,  # 9.0e-6 (zip)
  B06 = 1e-7,  # 8.2e-8
  B04 = 2e-6,  # 1.1e-6 (tobit)
  B04b = 5e-7, # 3.1e-7 (intreg)
  G11 = 5e-7,  # 3.1e-7
  G12 = 2e-6,  # 1.1e-6
  G13 = 5e-7,  # 2.4e-7
  G14 = 2e-6,  # 1.1e-6
  G15 = 2e-6,  # 1.1e-6
  B01 = 1e-8,  # 8.2e-9 (stcox)
  B02 = 1e-8,  # 8.2e-9
  B03 = 2e-7,  # 1.3e-7 (streg)
  B16 = 5e-5,  # 2.3e-5 (glmmTMB vs meprobit Laplace)
  E01 = 2e-7,  # 1.1e-7
  E02 = 2e-6,  # 1.1e-6 (zip)
  G09 = 2e-7,  # 1.7e-7
  G10 = 2e-6,  # 2.0e-6 (zip)
  G27 = 2e-7,  # 1.1e-7
  G28 = 2e-6,  # 2.0e-6
  G36 = 1e-5,  # 8.5e-6 (mixed, three-level)
  C09 = 1e-9,  # 2.7e-11 (clm)
  B10 = 1e-7,  # 8.7e-8 (polr)
  B12 = 5e-7,  # 3.0e-7 (zip)
  B13 = 1e-9,  # 1.5e-10 (survreg)
  # Since tabtools 2.1.11 (C2) the multi-equation union keeps the equations
  # model 1 lacks (take_action items 5, 8) and mecloglog shows hazard ratios
  # (item 7), so these compare exactly too; each tolerance is the smallest
  # 1-2-5 step that passed on 2026-09-26 (convergence, as above).
  G02 = 1e-5,  # zip beside poisson
  G31 = 2e-6,
  G33 = 2e-5,
  G34 = 2e-5,
  G35 = 2e-7,  # zinb
  G32 = 2e-5,
  B22 = 2e-7,  # ologit + mlogit
  G04 = 5e-5,
  G23 = 1e-8,  # stcox + mlogit
  G24 = 1e-8,  # streg + mlogit
  B15b = 1e-4  # mecloglog: lme4 AGQ vs Stata's quadrature
)

expect_p5r <- function(id) {
  tt <- suppressWarnings(p5r_run(id))
  mm <- golden_compare_cells(tt, p5r_cells(id), mode = "exact")
  if (nrow(mm)) golden_fail(mm, paste("cells", id)) else succeed()
  why <- golden_compare_stored(tt$stored, golden_methods_regtab(p5r_stored(id), id, "regtab_phase5_review"),
                               tolerance = p5r_tol[[id]])
  if (length(why)) fail(paste0("stored ", id, ":\n", paste(utils::head(why, 8L), collapse = "\n"))) else succeed()
  invisible(tt)
}

test_that("real AER Tobit and its surrogate retain censored complete-case weighted inference", {
  skip_if_not_installed("AER")
  skip_if_not_installed("survival")
  d <- withr::with_seed(1309, {
    x <- seq(-2, 2, length.out = 90)
    z <- rep(c(-0.5, 0, 0.5), 30)
    data.frame(y = pmax(1 + 1.4 * x - 0.4 * z + rnorm(90), 0), x = x, z = z,
               w = 1 + seq_len(90) %% 3, eligible = seq_len(90) %% 5 != 0)
  })
  d$y[c(7, 23)] <- NA_real_
  d$x[c(11, 44)] <- NA_real_
  d$z[c(9, 35)] <- NA_real_
  d$w[c(18, 46)] <- NA_integer_
  attr(d$x, "label") <- "Exposure"
  before_data <- d
  observed <- d[d$eligible & stats::complete.cases(d[c("y", "x", "z", "w")]), ]
  # Numeric subsetting drops labels; keep equivalent metadata on the
  # explicitly selected oracle and the genuine subset fit.
  attr(observed$x, "label") <- attr(d$x, "label")
  fit <- AER::tobit(y ~ x + z, data = d, left = 0, weights = w, subset = eligible,
                   na.action = na.exclude, model = TRUE)
  before_fit <- fit
  native <- survival::survreg(survival::Surv(y, y > 0, type = "left") ~ x + z,
    data = observed, dist = "gaussian", weights = w, model = TRUE)
  expect_s3_class(stats::model.response(stats::model.frame(fit)), "Surv")
  expect_equal(stats::coef(fit), stats::coef(native), tolerance = 1e-12)
  expect_equal(tt_vcov(fit), stats::vcov(native), tolerance = 1e-12)
  expect_no_warning(tt <- regtab(fit, keepintercept = TRUE, noreeffects = TRUE,
                                  stats = c("n", "ll")))
  rows <- tt$meta$regtab_rows
  coefficients <- stats::coef(native)[c("x", "z", "(Intercept)")]
  expected_low <- coefficients - qnorm(0.975) * sqrt(diag(stats::vcov(native))[names(coefficients)])
  expect_equal(rows$estimate, unname(coefficients), tolerance = 1e-12)
  expect_equal(rows$conf.low, unname(expected_low), tolerance = 1e-12)
  expect_identical(tt$body[[1]][1], "Exposure")
  expect_equal(tt$stored$n_1, sum(observed$w), tolerance = 0)
  expect_equal(tt$stored$ll_1, as.numeric(stats::logLik(native)), tolerance = 1e-12)
  # AER's loaded model.frame.tobit method needs the Surv formula that a
  # genuine AER object stores. The surrogate must satisfy that contract.
  surrogate <- p5r_tobit(native)
  expect_s3_class(stats::model.response(stats::model.frame(surrogate)), "Surv")
  expect_identical(surrogate$formula, stats::formula(native))
  expect_null(native$formula)
  expect_no_warning(synthetic <- regtab(surrogate, keepintercept = TRUE, noreeffects = TRUE,
                                         stats = c("n", "ll")))
  expect_identical(synthetic$body, tt$body)
  expect_identical(fit, before_fit)
  expect_identical(d, before_data)
})

test_that("every exact Phase 5 review fixture matches Stata (cells exact, stored per case)", {
  skip_on_cran()
  for (p in c("MASS", "nnet", "pscl", "glmmTMB", "lme4", "ordinal", "survival", "haven")) skip_if_not_installed(p)
  expect_setequal(intersect(names(p5r_tol), names(p5r_cases)), names(p5r_tol))
  for (id in names(p5r_tol)) expect_p5r(id)
})

# ---------------------------------------------------------------------------
# P0-3: in the coleq#colname layout Stata 2.1.9 kept only the equations
# the first model has (zip + poisson of another outcome showed no Poisson
# rows; ologit + mlogit lost the mlogit rows). R always showed every model's
# rows; 2.1.11 does too, so G02, G04, G23, G24, G31-G35 and B22 are in the
# exact list above.

test_that("a Poisson model's rows merge into the zip count equation; new terms go before its intercept", {
  skip_if_not_installed("pscl")
  d <- golden_fixture("zip")
  tt <- regtab(pscl::zeroinfl(event_count ~ treatment | zero_risk, data = d),
               glm(event_count ~ treatment + age_z, poisson, d), keepintercept = TRUE)
  expect_identical(tt$body[[1]], c("Event count: Treatment", "Event count: Age z-score", "Event count: Intercept",
                                   "Inflation equation: Structural-zero risk", "Inflation equation: Intercept"))
  # keep()/drop() still match the colname.
  expect_identical(regtab(pscl::zeroinfl(event_count ~ treatment | zero_risk, data = d),
                          glm(event_count ~ treatment + age_z, poisson, d), keep = "age_z")$body[[1]],
                   "Event count: Age z-score")
  # A single-equation table keeps the plain layout.
  expect_identical(regtab(glm(event_count ~ treatment, poisson, d))$body[[1]], "Treatment")
  # Mixed models never take the equation layout (Stata: no coleq layout
  # with random effects).
  skip_if_not_installed("lme4")
  expect_false(tabtools:::.rt_coleq_layout(list(list(equations = c("a", "b"), re_family = "none"),
                                                 list(equations = NULL, re_family = "variance"))))
})

# ---------------------------------------------------------------------------
# P0-5: mecloglog. Stata 2.1.9 printed the raw log-hazard coefficients under
# its HR header (take_action item 7); regtab always showed the hazard
# ratios, and 2.1.11 does too: B15b is in the exact list above.

# ---------------------------------------------------------------------------
# P0-2: multinom weights

test_that("weighted multinom: the information uses the weights (equals the expanded-data fit)", {
  skip_if_not_installed("nnet")
  d <- golden_fixture("cohort", "education")[1:3000, ]
  d$w <- 1 + seq_len(nrow(d)) %% 3
  fw <- nnet::multinom(education ~ index_age + female, data = d, weights = w, trace = FALSE, reltol = 1e-14, maxit = 2000)
  e <- d[rep(seq_len(nrow(d)), d$w), ]
  fe <- nnet::multinom(education ~ index_age + female, data = e, trace = FALSE, reltol = 1e-14, maxit = 2000)
  expect_equal(tabtools:::tt_vcov(fw), tabtools:::tt_vcov(fe), tolerance = 1e-6)
  expect_identical(tabtools:::tt_model_stats(fw, NULL)$N, nrow(e) + 0)
})

# ---------------------------------------------------------------------------
# P0-7: labels under subset =

test_that("glmer and hurdle keep their labels under subset =", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("pscl")
  d <- golden_fixture("nhanes2")
  attr(d$location, "label") <- "Site"
  d$age10 <- d$age / 10
  f <- lme4::glmer(highbp ~ age10 + (1 | location), family = binomial, data = d, subset = age > 30)
  expect_true("Median Odds Ratio (Site)" %in% regtab(f)$body[[1]])
  z <- golden_fixture("zip")
  h <- pscl::hurdle(event_count ~ treatment + age_z | zero_risk, data = z, subset = female == 1)
  expect_identical(regtab(h)$body[[1]], c("Event count: Treatment", "Event count: Age z-score",
                                          "Selection equation: Structural-zero risk"))
})

# ---------------------------------------------------------------------------
# P0-8: survreg distributions

test_that("survreg: Gaussian is intreg (Coef., intercept kept), other non-log-time distributions are refused", {
  skip_if_not_installed("survival")
  d <- p5r_data("cntg")
  d$yc <- pmax(d$y - 2, 0)
  f <- survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian")
  expect_identical(tabtools:::tt_model_info(f)$stata_cmd, "intreg")
  expect_identical(tabtools:::tt_model_info(p5r_tobit(f))$stata_cmd, "tobit")
  tt <- regtab(f, stats = "n")
  expect_identical(tt$stored$coef_label, "Coef.")
  expect_identical(tt$body[[1]], c("Exposure score", "sigma", "Intercept", "Observations"))
  # tobit's residual variance is a random-effects-style row: noreeffects drops it.
  expect_identical(regtab(p5r_tobit(f), noreeffects = TRUE)$body[[1]], c("Exposure score", "Intercept"))
  for (dist in c("logistic", "t")) {
    g <- survival::survreg(survival::Surv(yc + 1, yc > 0) ~ x1, data = d, dist = dist)
    expect_error(regtab(g), dist, fixed = TRUE)
  }
})

# ---------------------------------------------------------------------------
# P0-9: robust variance small-sample factor

test_that("robust coxph/survreg carry Stata's M/(M - 1); vce = 'model' keeps survival's sandwich", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")[1:400, ]
  cl <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + female, cluster = region, ties = "breslow", data = d)
  G <- length(unique(d$region))
  expect_equal(tabtools:::tt_vcov(cl), vcov(cl) * G / (G - 1))
  expect_identical(tabtools:::tt_vcov(cl, "model"), vcov(cl))
  rb <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + female, robust = TRUE, ties = "breslow", data = d)
  expect_equal(tabtools:::tt_vcov(rb), vcov(rb) * 400 / 399)
  plain <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + female, ties = "breslow", data = d)
  expect_identical(tabtools:::tt_vcov(plain), vcov(plain))
  sr <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + female, robust = TRUE, data = d)
  expect_equal(tabtools:::tt_vcov(sr), vcov(sr) * 400 / 399)
})

# ---------------------------------------------------------------------------
# P0-1 (glmer.nb) and P1-1: lme4's Laplace for non-canonical links

test_that("glmer.nb shows menbreg's lnalpha from the joint information; the Laplace note is shown once", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  withr::local_options(rlib_message_verbosity = "verbose")
  d <- p5r_data("cntg")
  f <- suppressWarnings(lme4::glmer.nb(y ~ x1 + (1 | clinic), data = d))
  expect_message(tt <- regtab(f, keepintercept = TRUE), "Laplace")
  want <- p5r_cells("B20")
  got <- golden_as_cells(tt)
  # lme4's Laplace is not Stata's: the lnalpha row still matches (4th row).
  expect_identical(got[4, ], want[4, ])
  expect_identical(got[, 1], want[, 1])
  # A probit Laplace glmer gets the note too; nAGQ = 7 does not.
  g <- lme4::glmer(y ~ x + (1 | site), family = binomial("probit"), data = p5r_data("bslope2"))
  expect_message(regtab(g), "glmmTMB")
  g7 <- lme4::glmer(y ~ x + (1 | site), family = binomial("probit"), data = p5r_data("bslope2"), nAGQ = 7)
  expect_no_message(regtab(g7))
})

# ---------------------------------------------------------------------------
# P0-10: the allow-list gate is in test-stubs.R. P1-2 and P2-6:

test_that("glmmTMB zero-inflation, dispersion submodels and nbinom1 are refused (P1-2)", {
  skip_if_not_installed("glmmTMB")
  d <- p5r_data("cntg")
  zi <- glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), ziformula = ~x1, family = poisson, data = d)
  expect_error(regtab(zi), "ziformula")
  dp <- glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), dispformula = ~x1, family = glmmTMB::nbinom2, data = d)
  expect_error(regtab(dp), "dispformula")
  n1 <- glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), family = glmmTMB::nbinom1, data = d)
  expect_error(regtab(n1), "nbinom1")
  ok <- glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), family = poisson, data = d)
  expect_s3_class(regtab(ok), "tt_table")
})

test_that("glmer nAGQ = 0 is refused; an estimated-scale family warns (P2-6, P2-4)", {
  skip_if_not_installed("lme4")
  d <- golden_fixture("nhanes2")
  f0 <- lme4::glmer(highbp ~ age + (1 | location), family = binomial, data = d, nAGQ = 0)
  expect_error(regtab(f0), "nAGQ = 0", fixed = TRUE)
  d$bmi2 <- d$bmi / 10
  gm <- suppressWarnings(lme4::glmer(bmi2 ~ age + (1 | location), family = Gamma("log"), data = d))
  expect_warning(tt <- regtab(gm), "estimated scale")
  re <- tt$body[[1]] == "var(_cons[location])"
  expect_identical(tt$body[[3]][re], "")
})

# ---------------------------------------------------------------------------
# tt_vcov() preparation for milestone 5w

test_that("tt_vcov() and tt_wald_df() refuse vce values a class does not support", {
  skip_if_not_installed("MASS")
  # Milestone 5w gave glm "robust"/"cluster", and its review glm.nb too
  # (nbreg [pw], review P0-2); polr has neither.
  f <- MASS::polr(factor(gear) ~ wt, mtcars, Hess = TRUE)
  expect_error(tabtools:::tt_vcov(f, "robust"), "not available for <polr>")
  expect_error(tabtools:::tt_wald_df(f, "cluster", cluster = ~cyl), "not available")
  expect_error(tabtools:::tt_vcov(f, c("stata", "model")), "not available")
  expect_identical(tabtools:::tt_vce_types(f), c("stata", "model"))
  g <- glm(am ~ wt, binomial, mtcars)
  expect_identical(tabtools:::tt_vce_types(g), c("stata", "model", "robust", "cluster"))
  expect_error(tabtools:::tt_vcov(g, c("stata", "model")), "not available")
})

test_that("zeroinfl and mixed-model rows read their variance through tt_vcov()", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("lme4")
  z <- golden_fixture("zip")
  f <- pscl::zeroinfl(event_count ~ treatment | zero_risk, data = z, dist = "negbin")
  V <- tabtools:::tt_vcov(f, full = TRUE)
  r <- tabtools:::tt_regtab_rows(f, tabtools:::tt_model_info(f))
  se <- (r$conf.high - r$conf.low)[r$key == "/::lnalpha"] / (2 * qnorm(0.975))
  expect_equal(se, sqrt(V["Log(theta)", "Log(theta)"]))
  # vce = "model": pscl's SE of log(theta) keeps the Ancillary interval (P2-2).
  rm <- tabtools:::tt_regtab_rows(f, tabtools:::tt_model_info(f), vce = "model")
  expect_equal((rm$conf.high - rm$conf.low)[rm$key == "/::lnalpha"] / (2 * qnorm(0.975)), unname(f$SE.logtheta))
  m <- lme4::lmer(y ~ age + (1 | region), data = golden_fixture("mixed_bp"), REML = FALSE)
  Vr <- tabtools:::tt_vcov(m, full = TRUE)
  expect_identical(rownames(Vr), c("re1", "lnsig_e"))
  expect_false(is.null(attr(Vr, "re")))
})

# ---------------------------------------------------------------------------
# P2-2, P2-3

test_that("vce = 'model' keeps a binary multinom's intervals; polr(Hess = FALSE) is not refitted (P2-2, P2-3)", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  d <- golden_fixture("cohort", "education")
  d$big <- factor(ifelse(d$education == "Tertiary", "High", "Other"), c("Other", "High"))
  tb <- regtab(nnet::multinom(big ~ index_age, data = d, trace = FALSE), vce = "model")
  expect_true(all(nzchar(tb$body[[3]])))
  p <- MASS::polr(education ~ index_age + female, data = d)
  expect_no_message(regtab(p))
  expect_silent(regtab(p))
})

# ---------------------------------------------------------------------------
# Surviving mutations of the review (P1-3)

test_that("M73: glmmTMB honours noreeffects", {
  skip_if_not_installed("glmmTMB")
  d <- p5r_data("cntg")
  f <- glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), family = poisson, data = d)
  full <- regtab(f, keepintercept = TRUE)$body[[1]]
  nore <- regtab(f, keepintercept = TRUE, noreeffects = TRUE)$body[[1]]
  expect_identical(full, c("Exposure score", "Intercept", "var(_cons[clinic])"))
  expect_identical(nore, c("Exposure score", "Intercept"))
})

test_that("M67: a supplied p.value column is kept as it is, missing entries included", {
  df <- data.frame(term = c("(Intercept)", "x", "z"), estimate = c(1, 0.5, 0.2), std.error = c(0.1, 0.25, 0.1),
                   p.value = c(0.5, 0.3, NA), stringsAsFactors = FALSE)
  tt <- regtab(df, keepintercept = TRUE)
  p <- stats::setNames(tt$body[[4]], tt$body[[1]])
  # A Wald test from std.error would give 0.046 for x and 0.046 for z.
  expect_identical(unname(p[c("x", "z")]), c("0.30", ""))
  # Without a p.value column, p comes from std.error.
  tt2 <- regtab(df[, c("term", "estimate", "std.error")], keepintercept = TRUE)
  expect_identical(tt2$body[[4]][tt2$body[[1]] == "z"], "0.046")
})
