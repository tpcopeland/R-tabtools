# Genuine fitter diagnostics, rather than fabricated convergence flags.

test_that("GEE iteration exhaustion refuses table and exported covariance", {
  skip_if_not_installed("geepack")
  d <- data.frame(id = rep(1:20, each = 4), x = rep(c(-1, 0, 1, 2), 20),
                  y = rep(c(0, 1, 0, 1, 1, 0, 1, 1), 10))
  bad <- geepack::geeglm(y ~ x, id = id, data = d, family = binomial,
                         corstr = "exchangeable", control = geepack::geese.control(maxit = 1))
  expect_identical(bad$geese$error, 1L)
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, "model", complete = FALSE), class = "tabtools_error_model_convergence")
  good <- geepack::geeglm(y ~ x, id = id, data = d, family = binomial, corstr = "exchangeable")
  expect_identical(good$geese$error, 0L)
  expect_s3_class(regtab(good), "tt_table")
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
})

test_that("Cox iteration exhaustion is refused when the literal limit is provable", {
  skip_if_not_installed("survival")
  d <- survival::lung
  bad <- NULL
  expect_warning(bad <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d,
                          control = survival::coxph.control(iter.max = 2)), "did not converge")
  expect_identical(bad$iter, 3L)
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, "model"), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, complete = FALSE), class = "tabtools_error_model_convergence")
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d,
                           control = survival::coxph.control(iter.max = 3))
  expect_identical(good$iter, 3L)
  expect_s3_class(regtab(good), "tt_table")
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
})

test_that("survreg failure to attain its null likelihood is refused", {
  skip_if_not_installed("survival")
  d <- survival::lung
  bad <- survival::survreg(survival::Surv(time, status) ~ age + sex, data = d,
                            control = survival::survreg.control(maxiter = 1))
  expect_lt(bad$loglik[2], bad$loglik[1] - 1)
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, "model"), class = "tabtools_error_model_convergence")
  # survreg's stored iter equals max both for failure and final-step convergence.
  good <- survival::survreg(survival::Surv(time, status) ~ age + sex, data = d,
                             control = survival::survreg.control(maxiter = 5))
  expect_identical(good$iter, 5L)
  expect_s3_class(regtab(good), "tt_table")
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
})

test_that("Cox zero events and singular terms do not acquire invented estimates", {
  skip_if_not_installed("survival")
  d <- data.frame(t = c(4, 9, 2, 7, 5, 12, 6, 11), e = c(1, 0, 1, 1, 0, 1, 1, 0),
                  x = c(0, 1, 1, 0, 1, 1, 0, 1))
  singular <- survival::coxph(survival::Surv(t, e) ~ x + I(2*x), data = d)
  expect_identical(unname(stats::coef(singular)[2]), NA_real_)
  expect_s3_class(regtab(singular), "tt_table")
  expect_identical(regtab(singular)$body$c2[2], "Omitted")
  d$e <- 0
  zero <- survival::coxph(survival::Surv(t, e) ~ x, data = d)
  expect_length(stats::coef(zero), 1L)
  expect_true(is.na(unname(stats::coef(zero))))
  expect_identical(regtab(zero)$body$c2, "Omitted")
})

test_that("singular GEE working correlation cannot manufacture a degenerate interval", {
  skip_if_not_installed("geepack")
  d <- data.frame(id = rep(1:3, each = 4), x = rep(c(0, 1, 0, 1), 3),
                  y = c(0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0))
  bad <- geepack::geeglm(y ~ x, id = id, data = d, family = binomial, corstr = "unstructured")
  expect_identical(bad$geese$error, 0L)
  expect_identical(unname(diag(bad$geese$vbeta.naiv)), c(0, 0))
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, "model"), class = "tabtools_error_model_convergence")
})

test_that("a mutable Cox control object does not retroactively diagnose a saved fit", {
  skip_if_not_installed("survival")
  d <- survival::lung
  control <- survival::coxph.control(iter.max = 30)
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d, control = control)
  expected <- stats::vcov(good)
  control$iter.max <- 1L
  # The original control value was not saved: today's value is not evidence
  # of the fitting limit. No environment expression is re-evaluated.
  expect_equal(tt_vcov(good, "model"), expected, tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})

test_that("survreg without an intercept is not compared to a nonnested baseline", {
  skip_if_not_installed("survival")
  good <- survival::survreg(survival::Surv(time, status) ~ sex - 1, data = survival::lung,
                             control = survival::survreg.control(maxiter = 100))
  expect_identical(attr(good$terms, "intercept"), 0L)
  expect_lt(good$loglik[2], good$loglik[1])
  expect_true(all(is.finite(stats::coef(good))))
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})

test_that("an unqualified Cox control call cannot prove its function identity", {
  skip_if_not_installed("survival")
  coxph.control <- function(iter.max) survival::coxph.control(iter.max = 20)
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = survival::lung,
                           control = coxph.control(iter.max = 2))
  expect_identical(good$iter, 3L)
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})

test_that("qualified Cox controls normalize partial and positional iteration arguments", {
  skip_if_not_installed("survival")
  partial <- positional <- dots <- NULL
  expect_warning(partial <- survival::coxph(survival::Surv(time, status) ~ age + sex,
    data = survival::lung, control = survival::coxph.control(iter.m = 2)), "did not converge")
  expect_warning(positional <- survival::coxph(survival::Surv(time, status) ~ age + sex,
    data = survival::lung, control = survival::coxph.control(1e-9, 1e-10, 2)), "did not converge")
  expect_warning(dots <- survival::coxph(survival::Surv(time, status) ~ age + sex,
    data = survival::lung, iter.m = 2), "did not converge")
  for (fit in list(partial, positional, dots)) {
    expect_identical(fit$iter, 3L)
    expect_error(regtab(fit), class = "tabtools_error_model_convergence")
    expect_error(tt_vcov(fit), class = "tabtools_error_model_convergence")
  }
  limit <- 30
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = survival::lung,
                           control = survival::coxph.control(iter.m = limit))
  limit <- 2
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})
