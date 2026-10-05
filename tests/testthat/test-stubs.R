# The model gate (Phase 5 review P0-10): an allow-list on the fit's first
# class. Any other class, including a subclass of a supported one, stops
# with an error naming it; nothing reaches a default path that would print
# per-group coefficient rows (nlme), crash (svycoxph), or drop a term
# silently (frailty). P2-9: the messages name the class and never an
# internal phase.

expect_refused <- function(fit, cls, hint = NULL) {
  err <- tryCatch(regtab(fit), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, paste0("does not support <", cls, "> models"), fixed = TRUE)
  expect_false(grepl("Phase", msg, fixed = TRUE))
  if (!is.null(hint)) expect_match(msg, hint, fixed = TRUE)
}

test_that("classes outside the allow-list are refused with an error naming them", {
  mbp <- golden_fixture("mixed_bp")
  skip_if_not_installed("nlme")
  # nlme::lme is supported since task 5.19 (test-regtab-nlme.R); gls is not.
  expect_refused(nlme::gls(y ~ age + female, data = mbp), "gls", "Generalized least squares")
  skip_if_not_installed("MASS")
  nh <- golden_fixture("nhanes2")[1:3000, ]
  pql <- suppressMessages(MASS::glmmPQL(highbp ~ age, random = ~ 1 | location, family = binomial, data = nh,
                                        verbose = FALSE))
  expect_refused(pql, "glmmPQL")
  skip_if_not_installed("survival")
  c35 <- golden_fixture("cohort3500")
  fr <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + survival::frailty(region), data = c35)
  expect_refused(fr, "coxph.penal", "frailty")
  skip_if_not_installed("survey")
  des <- suppressWarnings(survey::svydesign(ids = ~1, data = c35[1:500, ]))
  expect_refused(survey::svycoxph(survival::Surv(follow_up, cv_event) ~ treated, design = des), "svycoxph",
                 "Survey-design")
  # svyglm is supported since milestone 5w (task 5.10; test-regtab-vce.R).
  expect_s3_class(suppressWarnings(regtab(survey::svyglm(cv_event ~ treated, design = des, family = quasibinomial))),
                  "tt_table")
  skip_if_not_installed("mgcv")
  expect_refused(mgcv::gam(y ~ s(age), data = mbp), "gam")
  expect_refused(lm(cbind(y, bmi) ~ age, mbp), "mlm")
})

test_that("mocked classes: betareg, coxme, lmerTest, glm and survreg subclasses, clmm", {
  # Mock calls built from strings: a quoted pkg::fn() makes R CMD check
  # report the package as an undeclared test dependency.
  expect_refused(structure(list(coefficients = list(mean = c(x = 1)), call = str2lang("betareg::betareg(y ~ x)")),
                           class = "betareg"), "betareg")
  coxme <- structure(list(coefficients = c(age = 0.1), call = str2lang("coxme::coxme()")), class = "coxme")
  expect_false(tabtools:::tt_regtab_adapter(coxme))
  # H18 (user item (d)): mestreg is parametric, so coxme has no Stata twin.
  expect_refused(coxme, "coxme", "Mixed-effects Cox models have no Stata equivalent")
  expect_refused(coxme, "coxme", "`stcox, shared()` is gamma frailty")
  # An lmerTest fit is cast to lmerMod (test-regtab-hardening.R); an object
  # that only claims the class cannot be cast and is refused by name.
  expect_error(regtab(structure(list(coefficients = c(age = 0.1), call = str2lang("lmerTest::lmer()")),
                                class = "lmerModLmerTest")),
               "could not be converted to <lmerMod>", fixed = TRUE)
  g <- glm(am ~ wt, binomial, mtcars)
  class(g) <- c("brglmFit", class(g))
  expect_refused(g, "brglmFit")
  skip_if_not_installed("survival")
  s <- survival::survreg(survival::Surv(time, status) ~ age, data = survival::lung)
  class(s) <- c("frailtyreg", class(s))
  expect_refused(s, "frailtyreg")
  skip_if_not_installed("ordinal")
  d <- golden_fixture("cohort3500", "education")[1:600, ]
  mm <- suppressWarnings(ordinal::clmm(education ~ index_age + (1 | region), data = d))
  expect_refused(mm, "clmm", "meologit")
})

test_that("every allowed class and the data-frame escape hatch pass the gate", {
  for (f in list(lm(mpg ~ wt, mtcars), glm(am ~ wt, binomial, mtcars))) {
    expect_invisible(tabtools:::.rt_check_model(f, 1L))
  }
  df <- data.frame(term = "x", estimate = 1)
  expect_invisible(tabtools:::.rt_check_model(df, 1L))
  expect_invisible(tabtools:::.rt_check_model(structure(df, class = c("tbl_df", "tbl", "data.frame")), 1L))
})
