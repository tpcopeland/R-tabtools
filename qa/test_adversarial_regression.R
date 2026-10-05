library(testthat)
library(tabtools)

# Installed-user scenarios. Weighted normal equations are evaluated from
# raw observations, including the independent missingness/weight mask.
adversarial_weighted_oracle <- function(x, y, w, cluster = NULL) {
  n <- sum(w > 0)
  a <- sum(w)
  b <- sum(w * x)
  c <- sum(w * x^2)
  inverse <- matrix(c(c, -b, -b, a), 2L) / (a * c - b^2)
  beta <- drop(inverse %*% c(sum(w * y), sum(w * x * y)))
  names(beta) <- c("(Intercept)", "x")
  residual <- y - beta[1] - beta[2] * x
  model <- inverse * sum(w * residual^2) / (n - 2)
  score <- cbind(1, x) * (w * residual)
  score <- score[w > 0, , drop = FALSE]
  if (is.null(cluster)) {
    correction <- n / (n - 2)
  } else {
    cluster <- cluster[w > 0]
    score <- do.call(rbind, lapply(unique(cluster), function(g) {
      colSums(score[cluster == g, , drop = FALSE])
    }))
    G <- length(unique(cluster))
    correction <- G / (G - 1) * (n - 1) / (n - 2)
  }
  robust <- inverse %*% crossprod(score) %*% inverse * correction
  dimnames(model) <- dimnames(robust) <- rep(list(names(beta)), 2L)
  list(beta = beta, model = model, robust = robust, n = n)
}

test_that("installed weighted inference aligns subset, missingness and positive weights", {
  d <- data.frame(id = seq_len(24), x = seq(-2, 2, length.out = 24),
                  w = rep(c(0, 1, 2, 0.5), 6), cl = rep(1:6, 4))
  rownames(d) <- paste0("subject-", d$id)
  d$y <- 1 + 1.2 * d$x + rep(c(-1, 0.5, 2, -0.5, 1, -2), 4)
  d$x[c(3, 11)] <- NA_real_
  d$y[20] <- NA_real_
  d$w[8] <- NA_real_
  d$cl[3] <- NA_integer_
  before <- d
  keep <- !(d$id %in% c(5, 17)) & complete.cases(d[c("x", "y", "w")])
  observed <- d[keep, ]
  hand <- adversarial_weighted_oracle(observed$x, observed$y, observed$w, observed$cl)
  fit <- lm(y ~ x, d, subset = !(id %in% c(5, 17)), weights = w, na.action = stats::na.exclude)
  expect_identical(nrow(fit$model), 18L)
  expect_identical(hand$n, 14L)
  expect_equal(stats::coef(fit), hand$beta, tolerance = 1e-11)
  expect_equal(tt_vcov(fit, "model"), hand$model, tolerance = 1e-11)
  expect_equal(tt_vcov(fit, "cluster", ~cl), hand$robust, tolerance = 1e-10)
  tt <- regtab(fit, vce = "cluster", cluster = ~cl, stats = c("n", "F"))
  expect_identical(tt$stored$n_1, 14)
  expect_equal(tt$meta$regtab_rows$estimate, unname(hand$beta[2:1]), tolerance = 1e-11)
  G <- length(unique(observed$cl[observed$w > 0]))
  half <- qt(0.975, G - 1) * sqrt(hand$robust["x", "x"])
  expect_equal(tt$meta$regtab_rows$conf.low[1], hand$beta[["x"]] - half, tolerance = 1e-10)
  expect_equal(tt$meta$regtab_rows$p.value[1],
               2 * pt(-abs(hand$beta[["x"]] / sqrt(hand$robust["x", "x"])), G - 1), tolerance = 1e-10)
  expect_identical(d, before)
})

test_that("installed univariable transforms use each fit's distinct complete cases", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12, 11, 16, 15, 19, 20, 23),
                  x = c(-1, 0, 1, 2, 3, 4, NA_real_, 6, 7, 8, 9, 10),
                  z = c(1, 2, 3, 4, NA_real_, 6, 7, 8, 9, 10, 11, 12))
  # log(0) is -Inf and cannot be fitted: the error must name its focal x.
  expect_warning(err <- tryCatch(regtab_uv(d, "y", "x", method = lm,
                                          formula = "{y} ~ log({x})"), error = identity), "NaNs produced")
  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "model for.*x.*failed")
  d$x[2] <- NA_real_
  expect_warning(uv <- regtab_uv(d, "y", c("x", "z"), method = lm,
                                 formula = "{y} ~ log({x})",
                                 method.args = list(na.action = stats::na.exclude)), "NaNs produced")
  expect_identical(vapply(uv$fits, stats::nobs, 0L), c(x = 9L, z = 11L))
  tx <- log(d$x[d$x > 0 & !is.na(d$x)])
  yx <- d$y[d$x > 0 & !is.na(d$x)]
  tz <- log(d$z[!is.na(d$z)])
  yz <- d$y[!is.na(d$z)]
  want <- c(sum((tx - mean(tx)) * (yx - mean(yx))) / sum((tx - mean(tx))^2),
            sum((tz - mean(tz)) * (yz - mean(yz))) / sum((tz - mean(tz))^2))
  tt <- regtab(uv, stats = c("n", "ll", "r2"))
  expect_identical(tt$rows$key, c("log(x)", "log(z)"))
  expect_equal(tt$meta$regtab_rows$estimate, want, tolerance = 1e-11)
  expect_null(tt$stored$n_1)
  expect_null(tt$stored$ll_1)
  expect_null(tt$stored$r2_1)
})

test_that("installed factor unions distinguish an absent level from an omitted coefficient", {
  d <- data.frame(y = c(3, 4, 6, 7, 8, 10, 12, 14, 15, 18, 19, 20),
                  x = seq_len(12), g = factor(rep(c("low", "mid", "high"), each = 4),
                                            levels = c("low", "mid", "high")))
  absent <- d
  absent$y[9:12] <- NA_real_
  fit1 <- lm(y ~ g, absent, na.action = stats::na.exclude)
  fit2 <- lm(y ~ g, d)
  tt <- regtab(fit1, fit2, stats = "n")
  expect_identical(tt$rows$key, c("g", "1.g", "2.g", "3.g", "_cons", "stat:n"))
  expect_identical(unname(unlist(tt$body[4, 2:4])), rep("", 3L))
  expect_equal(tt$meta$regtab_rows$estimate[tt$meta$regtab_rows$model == 2L &
                                          tt$meta$regtab_rows$key == "3.g"],
               mean(d$y[9:12]) - mean(d$y[1:4]), tolerance = 1e-12)
  expect_identical(c(tt$stored$n_1, tt$stored$n_2), c(8, 12))
  mi <- tt_mi(list(fit1, fit2))
  expect_error(regtab(mi), "different coefficients.*ghigh", class = "rlang_error")
  expect_error(tt_vcov(mi), "different coefficients.*ghigh", class = "rlang_error")
  single_level <- absent
  single_level$y[5:8] <- NA_real_
  expect_error(regtab_uv(single_level, "y", "g", method = lm),
               "model for.*g.*failed", class = "rlang_error")
})

test_that("installed MI pools weighted missing-data fits against raw-data normal equations", {
  d <- data.frame(x = -5:5, e = c(2, -1, 0, 1, -2, 0, 2, -1, 0, 1, -2),
                  w = c(1, 2, 0, 1, 3, 2, 1, 0, 2, 1, 2))
  d$y <- 1 + 0.8 * d$x + d$e
  d$x[5] <- NA_real_
  d$y[10] <- NA_real_
  rownames(d) <- paste0("mi-subject-", seq_len(nrow(d)))
  datasets <- lapply(c(-0.3, 0, 0.5), function(shift) {
    completed <- d
    completed$x[5] <- -1 + shift
    completed$y[10] <- 4.5 + shift
    completed
  })
  fits <- lapply(datasets, function(completed) lm(y ~ x, completed, weights = w, na.action = stats::na.exclude))
  oracle <- lapply(datasets, function(completed) {
    adversarial_weighted_oracle(completed$x, completed$y, completed$w)
  })
  beta <- do.call(cbind, lapply(oracle, `[[`, "beta"))
  average <- c(mean(beta[1, ]), mean(beta[2, ]))
  within <- (oracle[[1]]$model + oracle[[2]]$model + oracle[[3]]$model) / 3
  between <- matrix(0, 2L, 2L)
  for (m in seq_len(3)) between <- between + outer(beta[, m] - average, beta[, m] - average) / 2
  total <- within + 4 / 3 * between
  dimnames(total) <- rep(list(c("(Intercept)", "x")), 2L)
  mi <- tt_mi(fits)
  expect_equal(tt_vcov(mi), total, tolerance = 1e-10)
  tt <- regtab(mi, stats = c("n", "mi_m", "ll", "aic"))
  expect_equal(tt$meta$regtab_rows$estimate, average[2:1], tolerance = 1e-11)
  expect_identical(tt$rows$key, c("x", "_cons", "stat:n", "stat:mi_m"))
  expect_identical(c(tt$stored$n_1, tt$stored$mi_m_1), c(9, 3))
  expect_null(tt$stored$ll_1)
  expect_null(tt$stored$aic_1)
  expect_identical(anyNA(d$x), TRUE)
  expect_identical(anyNA(d$y), TRUE)
})

test_that("installed MI rejects equal-size samples with different stable observation IDs", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12, 11, 16, 15, 19), x = seq_len(10))
  rownames(d) <- paste0("person-", seq_len(10))
  a <- d
  b <- d
  a$x[c(2, 4)] <- NA_real_
  b$x[c(7, 9)] <- NA_real_
  mi <- tt_mi(list(lm(y ~ x, a, na.action = stats::na.exclude),
                   lm(y ~ x, b, na.action = stats::na.omit)))
  expect_identical(vapply(mi$analyses, stats::nobs, 0L), c(8L, 8L))
  expect_error(tt_vcov(mi), "different observations", class = "rlang_error")
  expect_error(regtab(mi), "different observations", class = "rlang_error")
  b$x[9] <- d$x[9]
  varying_n <- tt_mi(list(mi$analyses[[1]], lm(y ~ x, b)))
  expect_error(tt_vcov(varying_n), "number of observations differs", class = "rlang_error")
})

test_that("installed model stacks retain the convergence error of the failed imputation or covariate", {
  converged <- glm(am ~ wt + hp, binomial, mtcars)
  expect_warning(unconverged <- glm(am ~ wt + hp, binomial, mtcars, control = glm.control(maxit = 1)),
                 "algorithm did not converge")
  expect_identical(unconverged$converged, FALSE)
  expect_error(regtab(unconverged), "did not converge", class = "tabtools_error_model_convergence")
  mi <- tt_mi(list(converged, unconverged))
  mi_error <- tryCatch(regtab(mi), error = identity)
  expect_s3_class(mi_error, "rlang_error")
  expect_match(conditionMessage(mi_error), "imputation 2")
  expect_s3_class(mi_error$parent, "tabtools_error_model_convergence")
  covariance_error <- tryCatch(tt_vcov(mi), error = identity)
  expect_s3_class(covariance_error$parent, "tabtools_error_model_convergence")
  expect_warning(uv_error <- tryCatch(regtab_uv(mtcars, "am", "wt", method = glm,
                                               method.args = list(family = binomial,
                                                                  control = glm.control(maxit = 1))),
                                      error = identity), "algorithm did not converge")
  expect_s3_class(uv_error, "rlang_error")
  expect_match(conditionMessage(uv_error), "model for.*wt.*cannot be tabled")
  expect_s3_class(uv_error$parent, "tabtools_error_model_convergence")
})

test_that("installed covariance refuses undefined corrections and missing retained clusters", {
  d <- data.frame(y = c(1, 5, 10, 12), x = c(0, 1, 2, 3), w = c(1, 1, 0, 0))
  saturated <- lm(y ~ x, d, weights = w)
  expect_error(tt_vcov(saturated, "robust"), "residual degrees of freedom",
               class = "tabtools_error_vce_sample_size")
  expect_error(tt_vcov(saturated, "cluster", c("a", "b", "c", "d")), "residual degrees of freedom",
               class = "tabtools_error_vce_sample_size")
  one <- glm(y ~ 1, gaussian, data.frame(y = 2))
  expect_identical(one$converged, TRUE)
  expect_error(tt_vcov(one, "robust"), "at least two",
               class = "tabtools_error_vce_sample_size")
  f <- lm(y ~ x, data.frame(y = c(2, 4, 3, 6, 8, 7), x = 1:6))
  expect_error(tt_vcov(f, "cluster", c(1, 1, NA, 2, 2, 2)), "missing values", class = "rlang_error")
  expect_error(tt_vcov(f, "cluster", rep(1, 6)), "single cluster", class = "rlang_error")
  expect_error(tt_vcov(f, "cluster", ~absent_cluster), "not in the model", class = "rlang_error")
})
