# Adversarial regression contracts, 2026-09-30. The numeric oracle uses
# centered sums for a two-parameter regression, rather than fit methods.

adversarial_linear_oracle <- function(x, y, cluster = NULL) {
  n <- length(x)
  slope <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
  b <- c(`(Intercept)` = mean(y) - slope * mean(x), x = slope)
  e <- y - b[1] - b[2] * x
  X <- cbind(`(Intercept)` = 1, x = x)
  den <- n * sum(x^2) - sum(x)^2
  bread <- matrix(c(sum(x^2), -sum(x), -sum(x), n), 2L) / den
  score <- X * e
  if (is.null(cluster)) {
    correction <- n / (n - 2)
  } else {
    score <- do.call(rbind, lapply(unique(cluster), function(g) {
      colSums(score[cluster == g, , drop = FALSE])
    }))
    G <- length(unique(cluster))
    correction <- G / (G - 1) * (n - 1) / (n - 2)
  }
  V <- bread %*% crossprod(score) %*% bread * correction
  dimnames(V) <- rep(list(names(b)), 2L)
  list(b = b, robust = V)
}

test_that("na.omit and na.exclude use exactly the observed rows for robust inference", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12, 11, 16, 15, 19, 20, 23),
                  x = seq_len(12), cl = rep(1:4, 3))
  d$x[c(2, 8)] <- NA_real_
  d$y[11] <- NA_real_
  keep <- complete.cases(d[c("x", "y")])
  d$cl[!keep] <- NA_integer_
  before <- d
  hand <- adversarial_linear_oracle(d$x[keep], d$y[keep])
  clustered <- adversarial_linear_oracle(d$x[keep], d$y[keep], d$cl[keep])
  for (act in list(stats::na.omit, stats::na.exclude)) {
    fit <- lm(y ~ x, data = d, na.action = act)
    expect_identical(length(fit$residuals), 9L)
    expect_equal(stats::coef(fit), hand$b, tolerance = 1e-12)
    expect_equal(tt_vcov(fit, "robust"), hand$robust, tolerance = 1e-11)
    expect_equal(tt_vcov(fit, "cluster", d$cl), clustered$robust, tolerance = 1e-11)
    expect_equal(tt_vcov(fit, "cluster", ~cl), clustered$robust, tolerance = 1e-11)
    tt <- regtab(fit, vce = "robust", stats = "n")
    expect_identical(tt$stored$n_1, 9)
    expect_identical(tt$rows$key, c("x", "_cons", "stat:n"))
    rows <- tt$meta$regtab_rows
    expect_equal(rows$estimate, unname(hand$b[c("x", "(Intercept)")]), tolerance = 1e-12)
    half <- qt(0.975, 7) * sqrt(hand$robust["x", "x"])
    expect_equal(rows$conf.low[1], hand$b[["x"]] - half, tolerance = 1e-11)
    expect_equal(rows$conf.high[1], hand$b[["x"]] + half, tolerance = 1e-11)
  }
  expect_identical(d, before)
})

test_that("rank deficiency keeps the omitted row without contaminating estimated covariance", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12), x = seq_len(6))
  d$x2 <- 2 * d$x
  fit <- lm(y ~ x + x2, d)
  hand <- adversarial_linear_oracle(d$x, d$y)
  expect_identical(is.na(stats::coef(fit)), c(`(Intercept)` = FALSE, x = FALSE, x2 = TRUE))
  V <- tt_vcov(fit, "robust")
  expect_identical(dim(V), c(3L, 3L))
  expect_equal(V[1:2, 1:2], hand$robust, tolerance = 1e-11)
  expect_true(all(is.na(V["x2", ])))
  expect_true(all(is.na(V[, "x2"])))
  tt <- regtab(fit, vce = "robust", stats = "n")
  expect_identical(tt$rows$key, c("x", "x2", "_cons", "stat:n"))
  expect_identical(tt$body[[2]][2], "Omitted")
  expect_identical(tt$meta$regtab_rows$status, c("est", "omit", "est"))
  expect_equal(tt$meta$regtab_rows$estimate[c(1, 3)], unname(hand$b[2:1]), tolerance = 1e-12)
  expect_identical(tt$stored$n_1, 6)
})

test_that("disjoint univariable samples retain each covariate's own estimate and count", {
  d <- data.frame(y = c(3, 4, 8, 9, 13, 15, 2, 3, 2, 7, 9, 8),
                  x = c(seq_len(6), rep(NA_real_, 6)),
                  z = c(rep(NA_real_, 6), seq_len(6)))
  before <- d
  expect_identical(sum(complete.cases(d)), 0L)
  uv <- regtab_uv(d, "y", c("x", "z"), method = lm,
                  method.args = list(na.action = stats::na.exclude))
  expect_identical(vapply(uv$fits, function(f) length(f$residuals), 0L), c(x = 6L, z = 6L))
  hand <- c(adversarial_linear_oracle(d$x[1:6], d$y[1:6])$b[["x"]],
            adversarial_linear_oracle(d$z[7:12], d$y[7:12])$b[["x"]])
  tt <- regtab(uv, stats = "n")
  expect_identical(tt$rows$key, c("x", "z", "stat:n"))
  expect_equal(tt$meta$regtab_rows$estimate, hand, tolerance = 1e-12)
  expect_identical(tt$stored$n_1, 6)
  expect_identical(tt$body[[2]][3], "6")
  expect_identical(d, before)
  d$y[12] <- NA_real_
  unequal <- regtab(regtab_uv(d, "y", c("x", "z"), method = lm), stats = "n")
  expect_identical(unequal$rows$key, c("x", "z"))
  expect_null(unequal$stored$n_1)
})

test_that("requested covariates fail explicitly when absent or not estimable", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12), x = seq_len(6),
                  empty = rep(NA_real_, 6), g = factor(c("a", "a", "a", "b", "b", "b")))
  expect_error(regtab_uv(d, "y", c("x", "missing"), method = lm),
               "missing.*not.*column", class = "rlang_error")
  expect_error(regtab_uv(d, "y", c("x", "empty"), method = lm),
               "model for.*empty.*failed", class = "rlang_error")
  expect_error(regtab_uv(d, "missing_outcome", "x", method = lm),
               "model for.*x.*failed", class = "rlang_error")
  expect_error(regtab_uv(d, "y", "x", method = lm, formula = "{y} ~ {x} + missing_adjuster"),
               "model for.*x.*failed", class = "rlang_error")
  d$y[d$g == "b"] <- NA_real_
  expect_error(regtab_uv(d, "y", "g", method = lm),
               "model for.*g.*failed", class = "rlang_error")
  expect_error(regtab(lm(y ~ x, d), keep = "missing"),
               "keep.*matched no coefficient rows", class = "rlang_error")
})

test_that("a factor level lost to missing outcomes stays blank only in its own model", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12, NA_real_, NA_real_),
                  g = factor(c(rep("A", 3), rep("B", 3), rep("C", 2))))
  full <- d
  full$y[7:8] <- c(15, 17)
  missing <- lm(y ~ g, d, na.action = stats::na.exclude)
  tt <- regtab(missing, lm(y ~ g, full), stats = "n")
  expect_identical(tt$rows$key, c("g", "1.g", "2.g", "3.g", "_cons", "stat:n"))
  expect_identical(unname(unlist(tt$body[4, 2:4])), rep("", 3L))
  expect_identical(tt$body[[2]][2], "Reference")
  expect_identical(tt$body[[5]][2], "Reference")
  rows <- tt$meta$regtab_rows
  expect_equal(rows$estimate[rows$model == 2L & rows$key == "3.g"], 12, tolerance = 1e-12)
  expect_identical(rows$status[rows$model == 1L & rows$key == "3.g"], "absent")
  expect_identical(c(tt$stored$n_1, tt$stored$n_2), c(6, 8))
})

test_that("MI checks missing observations and changing terms before any pooling", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12, 11, 16), x = seq_len(8),
                  z = c(1, 3, 2, 5, 4, 8, 6, 9))
  first <- d
  second <- d
  first$x[2] <- NA_real_
  second$x[7] <- NA_real_
  fits <- list(lm(y ~ x, first, na.action = stats::na.exclude),
               lm(y ~ x, second, na.action = stats::na.omit))
  expect_identical(vapply(fits, stats::nobs, 0L), c(7L, 7L))
  expect_error(regtab(tt_mi(fits)), "different observations", class = "rlang_error")
  expect_error(tt_vcov(tt_mi(fits)), "different observations", class = "rlang_error")
  alias <- d
  alias$z <- alias$x
  inconsistent <- tt_mi(list(lm(y ~ x + z, d), lm(y ~ x + z, alias)))
  expect_error(regtab(inconsistent), "aliased.*coefficients differ", class = "rlang_error")
  expect_error(tt_vcov(inconsistent), "aliased.*coefficients differ", class = "rlang_error")
  different <- tt_mi(list(lm(y ~ x + z, d), lm(y ~ x, d)))
  expect_error(tt_vcov(different), "not the same model", class = "rlang_error")
})

test_that("MI total variance and table estimates match a closed-form two-imputation oracle", {
  d <- data.frame(x = -3:3, e = c(1, -2, 1, 0, 1, -2, 1))
  d$y <- 2 + 3 * d$x + d$e
  second <- d
  second$y <- d$y - 0.5 + 0.4 * d$x
  mi <- tt_mi(list(lm(y ~ x, d), lm(y ~ x, second)))
  # Residual SSE = 12, residual df = 5, X'X = diag(7, 28).
  # For M = 2, (1 + 1/M) B = 3/4 outer(Q2 - Q1).
  W <- diag(c(12 / 35, 3 / 35))
  delta <- c(-0.5, 0.4)
  want <- W + 3 / 4 * outer(delta, delta)
  dimnames(want) <- rep(list(c("(Intercept)", "x")), 2L)
  expect_equal(tt_vcov(mi), want, tolerance = 1e-11)
  tt <- regtab(mi, stats = c("n", "mi_m"))
  expect_identical(tt$rows$key, c("x", "_cons", "stat:n", "stat:mi_m"))
  expect_equal(tt$meta$regtab_rows$estimate, c(3.2, 1.75), tolerance = 1e-12)
  expect_identical(c(tt$stored$n_1, tt$stored$mi_m_1), c(7, 2))
})

test_that("saturated positive-weight samples cannot return NaN robust covariance", {
  d <- data.frame(y = c(1, 5, 8, 10), x = c(0, 1, 2, 3), w = c(1, 1, 0, 0))
  fit <- lm(y ~ x, d, weights = w)
  expect_identical(sum(fit$weights > 0), 2L)
  expect_identical(fit$rank, 2L)
  expect_error(tt_vcov(fit, "robust"), "residual degrees of freedom",
               class = "tabtools_error_vce_sample_size")
  expect_error(tt_vcov(fit, "cluster", c("a", "b", "c", "d")), "residual degrees of freedom",
               class = "tabtools_error_vce_sample_size")
  expect_error(regtab(fit, vce = "robust"), "residual degrees of freedom",
               class = "tabtools_error_vce_sample_size")
})

test_that("unconverged fitted GLMs are refused before ordinary inference", {
  expect_warning(fit <- glm(am ~ wt + hp, binomial, mtcars, control = glm.control(maxit = 1)),
                 "algorithm did not converge")
  expect_identical(fit$converged, FALSE)
  expect_error(regtab(fit), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(lm(mpg ~ wt, mtcars), fit), "Model 2.*did not converge",
               class = "tabtools_error_model_convergence")
  expect_error(regtab(fit, vce = "model"), "did not converge",
               class = "tabtools_error_model_convergence")
  for (vce in c("stata", "model", "robust")) {
    expect_error(tt_vcov(fit, vce = vce), "did not converge",
                 class = "tabtools_error_model_convergence")
  }
  expect_error(tt_vcov(fit, complete = FALSE), "did not converge",
               class = "tabtools_error_model_convergence")
  good <- glm(am ~ wt + hp, binomial, mtcars)
  mi <- tt_mi(list(good, fit))
  for (operation in list(regtab, tt_vcov)) {
    err <- tryCatch(operation(mi), error = identity)
    expect_s3_class(err, "rlang_error")
    expect_match(conditionMessage(err), "imputation 2")
    expect_s3_class(err$parent, "tabtools_error_model_convergence")
  }
})

test_that("converged separated GLMs retain their warning and blank displayed interval", {
  d <- data.frame(x = c(-2, -1, 1, 2), y = c(0, 0, 1, 1))
  expect_warning(fit <- glm(y ~ x, binomial, d, control = glm.control(maxit = 100)),
                 "fitted probabilities numerically 0 or 1 occurred")
  expect_identical(fit$converged, TRUE)
  expect_warning(tt <- regtab(fit, stats = "n"), "complete separation", class = "rlang_warning")
  expect_identical(tt$rows$key, c("x", "stat:n"))
  expect_identical(tt$body[[3]][1], "")
  expect_identical(tt$stored$n_1, 4)
  expect_gt(tt$meta$regtab_rows$estimate[1], 1e8)
  expect_identical(tt$meta$regtab_rows$conf.high[1], Inf)
})
