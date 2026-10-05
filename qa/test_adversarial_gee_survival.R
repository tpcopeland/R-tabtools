library(testthat)
library(tabtools)

# Installed public APIs and genuine fitter diagnostics.

test_that("installed GEE refuses a recorded fitting error across variance choices", {
  skip_if_not_installed("geepack")
  d <- data.frame(id = rep(1:20, each = 4), x = rep(c(-1, 0, 1, 2), 20),
                  y = rep(c(0, 1, 0, 1, 1, 0, 1, 1), 10))
  bad <- geepack::geeglm(y ~ x, id = id, data = d, family = binomial,
                         corstr = "exchangeable", control = geepack::geese.control(maxit = 2))
  expect_identical(bad$geese$error, 1L)
  for (v in c("stata", "model", "robust", "cluster")) {
    expect_error(tt_vcov(bad, v), class = "tabtools_error_model_convergence")
    expect_error(regtab(bad, vce = v), class = "tabtools_error_model_convergence")
  }
  one <- transform(d, id = 1L)
  good <- geepack::geeglm(y ~ x, id = id, data = one, family = binomial, corstr = "independence")
  expect_identical(good$geese$error, 0L)
  expect_error(tt_vcov(good, "robust"), "needs at least two", class = "rlang_error")
  expect_s3_class(regtab(good, vce = "stata"), "tt_table")
})

test_that("installed survival refuses confirmed failures and keeps final-step convergence", {
  skip_if_not_installed("survival")
  d <- survival::lung
  bad <- NULL
  expect_warning(bad <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d,
                        control = survival::coxph.control(iter.max = 2)), "did not converge")
  expect_error(tt_vcov(bad, complete = FALSE), class = "tabtools_error_model_convergence")
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  sr <- survival::survreg(survival::Surv(time, status) ~ age + sex, data = d,
                           control = survival::survreg.control(maxiter = 1))
  expect_error(tt_vcov(sr, complete = FALSE), class = "tabtools_error_model_convergence")
  expect_error(regtab(sr), class = "tabtools_error_model_convergence")
  good <- survival::survreg(survival::Surv(time, status) ~ age + sex, data = d,
                             control = survival::survreg.control(maxiter = 5))
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})

test_that("GEE missing observations keep clusters aligned to stored fitted rows", {
  skip_if_not_installed("geepack")
  d <- data.frame(id = rep(1:20, each = 4), x = rep(c(-1, 0, 1, 2), 20),
                  y = rep(c(0, 1, 0, 1, 1, 0, 1, 1), 10))
  d$x[c(2, 13)] <- NA_real_
  # geeglm requires complete data; explicitly prepare its documented sample.
  observed <- d[!is.na(d$x), ]
  good <- geepack::geeglm(y ~ x, id = id, data = observed, family = binomial,
                          corstr = "independence")
  expect_identical(length(good$id), 78L)
  expect_identical(unname(good$id), observed$id)
  expect_identical(names(good$id), rownames(observed))
  expect_identical(length(unique(good$id)), 20L)
  expected <- stats::vcov(good) * 20 / 19
  expect_equal(tt_vcov(good, "robust"), expected, tolerance = 1e-12)
  tt <- regtab(good, vce = "robust", stats = c("n", "groups"))
  expect_s3_class(tt, "tt_table")
  expect_true("78" %in% tt$body$c2)
  expect_true("20" %in% tt$body$c2)
})

test_that("Cox complete-case cluster variance matches explicit observed-row fitting", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$id <- rep(seq_len(ceiling(nrow(d) / 3)), each = 3)[seq_len(nrow(d))]
  d$age[c(2, 11)] <- NA_real_
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d, model = TRUE)
  keep <- stats::complete.cases(d[c("time", "status", "age", "sex")])
  observed <- d[keep, ]
  explicit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = observed,
                               cluster = id, model = TRUE)
  clusters <- length(unique(observed$id))
  expect_identical(good$n, sum(keep))
  expect_equal(tt_vcov(good, "cluster", cluster = d$id),
               stats::vcov(explicit) * clusters / (clusters - 1), tolerance = 1e-10)
  expect_s3_class(regtab(good, vce = "cluster", cluster = d$id), "tt_table")
})

test_that("unstructured GEE correlation guards preserve identifiable complete and shortened clusters", {
  skip_if_not_installed("geepack")
  # The seed specifies a reproducible fixture; inference is checked against
  # the fitter's saved matrix rather than against a lucky effect estimate.
  d <- withr::with_seed(893, data.frame(id = rep(1:60, each = 4), x = rnorm(240),
                                      y = rnorm(240) + rep(rnorm(60), each = 4)))
  for (removed in list(integer(), c(2L, 7L, 13L, 90L))) {
    observed <- if (length(removed)) d[-removed, ] else d
    # Omitted waves use geepack's documented within-cluster sequence; do
    # not infer an unobserved visit's original wave from the shortened fit.
    good <- geepack::geeglm(y ~ x, id = id, data = observed, corstr = "unstructured")
    expect_identical(good$geese$error, 0L)
    expect_identical(sum(good$geese$clusz), nrow(observed))
    expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
    expect_equal(tt_vcov(good, "robust"), stats::vcov(good) * 60 / 59, tolerance = 1e-12)
    expect_s3_class(regtab(good), "tt_table")
  }
  bad_data <- data.frame(id = rep(1:3, each = 4), x = rep(c(0, 1, 0, 1), 3),
                         y = c(0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0))
  bad <- geepack::geeglm(y ~ x, id = id, data = bad_data, family = binomial, corstr = "unstructured")
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, complete = FALSE), class = "tabtools_error_model_convergence")
})

test_that("installed survival preserves nonnested baseline and shadowed control fits", {
  skip_if_not_installed("survival")
  no_intercept <- survival::survreg(survival::Surv(time, status) ~ sex - 1, data = survival::lung,
                                     control = survival::survreg.control(maxiter = 100))
  expect_lt(no_intercept$loglik[2], no_intercept$loglik[1])
  expect_equal(tt_vcov(no_intercept, "model"), stats::vcov(no_intercept), tolerance = 1e-12)
  expect_s3_class(regtab(no_intercept), "tt_table")
  coxph.control <- function(iter.max) survival::coxph.control(iter.max = 20)
  shadowed <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = survival::lung,
                               control = coxph.control(iter.max = 2))
  expect_identical(shadowed$iter, 3L)
  expect_equal(tt_vcov(shadowed, "model"), stats::vcov(shadowed), tolerance = 1e-12)
  expect_s3_class(regtab(shadowed), "tt_table")
})

test_that("installed Cox diagnostics normalize literal arguments without evaluating symbols", {
  skip_if_not_installed("survival")
  bad <- NULL
  expect_warning(bad <- survival::coxph(survival::Surv(time, status) ~ age + sex,
    data = survival::lung, control = survival::coxph.control(iter.m = 2)), "did not converge")
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad, complete = FALSE), class = "tabtools_error_model_convergence")
  expect_warning(bad <- survival::coxph(survival::Surv(time, status) ~ age + sex,
    data = survival::lung, iter.m = 2), "did not converge")
  expect_error(regtab(bad), class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad), class = "tabtools_error_model_convergence")
  limit <- 30
  good <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = survival::lung,
                           control = survival::coxph.control(1e-9, 1e-10, limit))
  limit <- 2
  expect_equal(tt_vcov(good, "model"), stats::vcov(good), tolerance = 1e-12)
  expect_s3_class(regtab(good), "tt_table")
})

test_that("installed genuine AER Tobit preserves weighted censored complete cases", {
  skip_if_not_installed("AER")
  skip_if_not_installed("survival")
  d <- withr::with_seed(1309, {
    x <- seq(-2, 2, length.out = 90)
    z <- rep(c(-0.5, 0, 0.5), 30)
    data.frame(y = pmax(1 + 1.4 * x - 0.4 * z + rnorm(90), 0), x = x, z = z,
               w = 1 + seq_len(90) %% 3, eligible = seq_len(90) %% 5 != 0)
  })
  rownames(d) <- paste0("tobit-subject-", seq_len(nrow(d)))
  d$y[c(7, 23)] <- NA_real_
  d$x[c(11, 44)] <- NA_real_
  d$z[c(9, 35)] <- NA_real_
  d$w[c(18, 46)] <- NA_integer_
  attr(d$x, "label") <- "Exposure"
  before_data <- d
  observed <- d[d$eligible & stats::complete.cases(d[c("y", "x", "z", "w")]), ]
  native <- survival::survreg(survival::Surv(y, y > 0, type = "left") ~ x + z,
    data = observed, dist = "gaussian", weights = w, model = TRUE)
  coefficients <- stats::coef(native)[c("x", "z", "(Intercept)")]
  expected_low <- coefficients - qnorm(0.975) * sqrt(diag(stats::vcov(native))[names(coefficients)])
  for (na_action in list(na.omit, na.exclude)) {
    fit <- AER::tobit(y ~ x + z, data = d, left = 0, weights = w, subset = eligible,
                     na.action = na_action, model = TRUE)
    before_fit <- fit
    frame <- stats::model.frame(fit)
    response <- stats::model.response(frame)
    expect_identical(rownames(frame), rownames(observed))
    expect_s3_class(response, "Surv")
    expect_identical(attr(response, "type"), "left")
    expect_identical(sum(response[, "status"] == 0), sum(observed$y == 0))
    expect_equal(tt_vcov(fit), stats::vcov(native), tolerance = 1e-12)
    expect_equal(tt_vcov(fit, "model"), stats::vcov(native), tolerance = 1e-12)
    expect_no_warning(tt <- regtab(fit, keepintercept = TRUE, noreeffects = TRUE,
                                    stats = c("n", "ll")))
    expect_identical(tt$rows$key, c("x", "z", "_cons", "stat:n", "stat:ll"))
    expect_equal(tt$meta$regtab_rows$estimate, unname(coefficients), tolerance = 1e-12)
    expect_equal(tt$meta$regtab_rows$conf.low, unname(expected_low), tolerance = 1e-12)
    expect_identical(tt$body[[1]][1], "Exposure")
    expect_equal(tt$stored$n_1, sum(observed$w), tolerance = 0)
    expect_equal(tt$stored$ll_1, as.numeric(stats::logLik(native)), tolerance = 1e-12)
    expect_identical(fit, before_fit)
  }
  expect_identical(d, before_data)
})
