ad_transform_data <- function() {
  d <- data.frame(x = seq(-2, 2, length.out = 120L), z = rep(c(-1, 0, 1), 40L))
  d$y <- 1 + .4 * d$x + .6 * d$x^2 + .2 * d$z + .15 * sin(seq_len(nrow(d)))
  d$count <- rep(c(0, 1, 3, 1, 2, 4), 20L) + as.integer(d$x > 0)
  attr(d$x, "label") <- "Exposure score"
  d
}

ad_transform_mixed_data <- function() {
  withr::local_preserve_seed()
  set.seed(372)
  n <- 300L
  g <- rep(seq_len(50L), each = 6L)
  x <- stats::rnorm(n)
  z <- stats::rnorm(n)
  d <- data.frame(g = factor(g), x = x, z = z)
  d$y <- 1 + .3 * x + z + rep(stats::rnorm(50L), each = 6L) +
    rep(stats::rnorm(50L, sd = .6), each = 6L) * x + stats::rnorm(n)
  d$y[c(2L, 30L)] <- NA_real_
  attr(d$x, "label") <- "Exposure score"
  d
}

# Native coefficients and covariance supply an independent Wald oracle.
# Keys are specified by the test, rather than obtained from regtab's mapper.
ad_expect_transform_wald <- function(tab, fit, keys, df, exponentiate = FALSE) {
  mixed <- inherits(fit, "glmmTMB")
  b <- if (mixed) glmmTMB::fixef(fit)$cond else stats::coef(fit)
  V <- if (mixed) stats::vcov(fit)$cond else stats::vcov(fit)
  rows <- tab$meta$regtab_rows
  expect_identical(anyDuplicated(rows$key), 0L)
  expect_setequal(rows$key, unname(keys))
  i <- match(unname(keys[names(b)]), rows$key)
  expect_false(anyNA(i))
  expect_identical(rows$status[i], unname(ifelse(is.na(b), "omit", "est")))
  se <- sqrt(diag(V)[names(b)])
  q <- if (is.finite(df)) stats::qt(.975, df) else stats::qnorm(.975)
  estimates <- unname(b)
  lo <- unname(b - q * se)
  hi <- unname(b + q * se)
  if (exponentiate) {
    estimates <- exp(estimates)
    lo <- exp(lo)
    hi <- exp(hi)
  }
  expect_equal(rows$estimate[i], estimates, tolerance = 1e-10)
  expect_equal(rows$conf.low[i], lo, tolerance = 1e-10)
  expect_equal(rows$conf.high[i], hi, tolerance = 1e-10)
  p <- if (is.finite(df)) 2 * stats::pt(abs(b / se), df, lower.tail = FALSE) else
    2 * stats::pnorm(abs(b / se), lower.tail = FALSE)
  expect_equal(rows$p.value[i], unname(p), tolerance = 1e-10)
  expect_equal(tt_vcov(fit, "model"), V, tolerance = 1e-10)
  expect_equal(tab$stored$n_1, stats::nobs(fit), tolerance = 0)
}

ad_transform_keys <- function(b) {
  keys <- stats::setNames(names(b), names(b))
  keys[names(keys) == "(Intercept)"] <- "_cons"
  keys
}

test_that("lm shared-variable transforms have one row per coefficient and retain labels", {
  d <- ad_transform_data()
  before <- d
  for (formula in list(y ~ x + poly(x, 2) + z, y ~ poly(x, 2) + x + z)) {
    fit <- stats::lm(formula, data = d)
    frame <- stats::model.frame(fit)
    attrs <- attributes(fit)
    expect_identical(sum(is.na(stats::coef(fit))), 1L)
    tab <- regtab(fit, keepintercept = TRUE, stats = "n", vce = "model")
    ad_expect_transform_wald(tab, fit, ad_transform_keys(stats::coef(fit)), fit$df.residual)
    expect_identical(tab$meta$regtab_rows$label[tab$meta$regtab_rows$key == "x"], "Exposure score")
    expect_identical(stats::model.frame(fit), frame)
    expect_identical(attributes(fit), attrs)
  }
  expect_identical(d, before)
})

test_that("glm shared-variable transforms retain distinct exponentiated and omitted rows", {
  d <- ad_transform_data()
  d$count[c(2L, 30L)] <- NA_integer_
  before <- d
  fit <- stats::glm(count ~ x + poly(x, 2) + z, data = d, family = stats::poisson())
  frame <- stats::model.frame(fit)
  attrs <- attributes(fit)
  expect_identical(fit$converged, TRUE)
  expect_identical(sum(is.na(stats::coef(fit))), 1L)
  tab <- regtab(fit, keepintercept = TRUE, stats = "n", vce = "model")
  ad_expect_transform_wald(tab, fit, ad_transform_keys(stats::coef(fit)), Inf, exponentiate = TRUE)
  expect_identical(tab$meta$regtab_rows$label[tab$meta$regtab_rows$key == "x"], "Exposure score")
  expect_identical(stats::model.frame(fit), frame)
  expect_identical(attributes(fit), attrs)
  expect_identical(d, before)
})

test_that("lm nested square polynomial columns use coefficient keys and native intervals", {
  d <- ad_transform_data()
  before <- d
  fit <- stats::lm(y ~ poly(I(x^2), 2) + z, data = d)
  frame <- stats::model.frame(fit)
  tab <- regtab(fit, keepintercept = TRUE, stats = "n", vce = "model")
  ad_expect_transform_wald(tab, fit, ad_transform_keys(stats::coef(fit)), fit$df.residual)
  expect_identical(tab$rows$key[1:2], c("poly(I(x^2), 2)1", "poly(I(x^2), 2)2"))
  expect_identical(tab$body$c1[1:2], c("poly(I(x^2), 2)1", "poly(I(x^2), 2)2"))
  expect_false("c.x#c.x" %in% tab$rows$key)
  expect_identical(stats::model.frame(fit), frame)
  expect_identical(d, before)
})

test_that("glm scalar square and its polynomial basis remain separate even with an alias", {
  d <- ad_transform_data()
  fit <- stats::glm(count ~ I(x^2) + poly(I(x^2), 2) + z, data = d, family = stats::poisson())
  frame <- stats::model.frame(fit)
  expect_identical(fit$converged, TRUE)
  expect_identical(sum(is.na(stats::coef(fit))), 1L)
  keys <- ad_transform_keys(stats::coef(fit))
  keys["I(x^2)"] <- "c.x#c.x"
  tab <- regtab(fit, keepintercept = TRUE, stats = "n", vce = "model")
  ad_expect_transform_wald(tab, fit, keys, Inf, exponentiate = TRUE)
  expect_identical(sum(tab$rows$key == "c.x#c.x"), 1L)
  expect_identical(stats::model.frame(fit), frame)
})

test_that("glmmTMB polynomial and random-slope source overlap cannot duplicate fixed rows", {
  skip_if_not_installed("glmmTMB")
  d <- ad_transform_mixed_data()
  before <- d
  fit <- glmmTMB::glmmTMB(y ~ poly(x, 2) + z + (1 + x | g), data = d)
  frame <- stats::model.frame(fit)
  attrs <- attributes(fit)
  expect_identical(fit$fit$convergence, 0L)
  expect_identical(fit$sdr$pdHess, TRUE)
  tab <- regtab(fit, keepintercept = TRUE, noreeffects = TRUE, stats = "n", vce = "model")
  ad_expect_transform_wald(tab, fit, ad_transform_keys(glmmTMB::fixef(fit)$cond), Inf)
  expect_identical(tab$rows$key[1:2], c("poly(x, 2)1", "poly(x, 2)2"))
  expect_identical(stats::model.frame(fit), frame)
  expect_identical(attributes(fit), attrs)
  expect_identical(d, before)
})

test_that("glmmTMB nested-square basis has separate keys with genuine converged inference", {
  skip_if_not_installed("glmmTMB")
  d <- ad_transform_mixed_data()
  before <- d
  fit <- glmmTMB::glmmTMB(y ~ poly(I(x^2), 2) + z + (1 + x | g), data = d)
  frame <- stats::model.frame(fit)
  attrs <- attributes(fit)
  expect_identical(fit$fit$convergence, 0L)
  expect_identical(fit$sdr$pdHess, TRUE)
  tab <- regtab(fit, keepintercept = TRUE, noreeffects = TRUE, stats = "n", vce = "model")
  ad_expect_transform_wald(tab, fit, ad_transform_keys(glmmTMB::fixef(fit)$cond), Inf)
  expect_false("c.x#c.x" %in% tab$rows$key)
  expect_identical(stats::model.frame(fit), frame)
  expect_identical(attributes(fit), attrs)
  expect_identical(d, before)
})
