library(testthat)
library(tabtools)

test_that("installed MI pooling detects lost subject identity with explicit IDs", {
  d <- data.frame(id = paste0("person", 1:14), x = 1:14,
                  y = c(4, 8, 7, 11, 9, 13, 14, 12, 18, 16, 20, 19, 23, 21))
  a <- d[-1, ]
  b <- d[-2, ]
  rownames(a) <- rownames(b) <- NULL
  fits <- list(stats::lm(y ~ x, a), stats::lm(y ~ x, b))
  mi <- tt_mi(fits, observation_ids = list(a$id, b$id), sample_check = "strict")
  expect_error(regtab(mi), class = "tabtools_error_mi_sample_mismatch")
  expect_error(tt_vcov(mi, complete = FALSE), class = "tabtools_error_mi_sample_mismatch")
  expect_error(tt_mi(fits, sample_check = "strict"), class = "tabtools_error_mi_sample_identity")
})

test_that("MI sample identity and pooling are invariant to permutation and excluded rows", {
  withr::local_preserve_seed()
  for (seed in c(821L, 931L, 1051L)) {
    set.seed(seed)
    n <- 40L
    d <- data.frame(id = paste0("person", seq_len(n)), x = stats::rnorm(n),
                    w = rep(c(0, 1, 2, 3), length.out = n))
    d$y <- 1 + 2 * d$x + stats::rnorm(n)
    d$x[c(5, 9)] <- NA_real_
    keep <- !is.na(d$x) & d$w > 0
    a <- d
    b <- d[sample.int(n), ]
    rownames(a) <- rownames(b) <- NULL
    b$y[b$w == 0] <- 1e10
    fits <- list(stats::lm(y ~ x, a, weights = w, na.action = stats::na.exclude),
                 stats::lm(y ~ x, b, weights = w, na.action = stats::na.omit))
    bids <- b$id[!is.na(b$x) & b$w > 0]
    mi <- tt_mi(fits, observation_ids = list(a$id[keep], bids), sample_check = "strict")
    got <- tt_vcov(mi, "robust")
    # Independent weighted normal equations and HC1 on positive-weight rows.
    X <- cbind(1, a$x[keep])
    y <- a$y[keep]
    w <- a$w[keep]
    bread <- solve(crossprod(X, w * X))
    beta <- drop(bread %*% crossprod(X, w * y))
    score <- X * (w * (y - drop(X %*% beta)))
    want <- bread %*% crossprod(score) %*% bread * sum(keep) / (sum(keep) - ncol(X))
    expect_equal(unname(got), unname(want), tolerance = 1e-11)
    tt <- regtab(mi)
    expect_equal(tt$meta$regtab_rows$estimate, beta[c(2, 1)], tolerance = 1e-11)
    expect_identical(tt$meta$mi_sample_identity[[1]],
                     list(method = "explicit_ids", n = rep(as.numeric(sum(keep)), 2), strict = TRUE))
  }
})

test_that("explicit IDs preserve strict policy through wrapping a mira and tt_mi", {
  fits <- list(stats::lm(mpg ~ wt, mtcars), stats::lm(mpg ~ wt, mtcars))
  mira <- structure(list(analyses = fits), class = "mira")
  ids <- lapply(fits, function(f) rownames(stats::model.frame(f)))
  mi <- tt_mi(mira, observation_ids = ids, sample_check = "strict")
  expect_identical(tt_mi(mi), mi)
  expect_identical(tt_mi(mi, sample_check = "auto")$observation_ids, ids)
  expect_identical(regtab(mi)$meta$mi_sample_identity[[1]]$method, "explicit_ids")
  expect_error(tt_mi(mi, observation_ids = list(ids[[1]], ids[[2]][-1])),
               class = "tabtools_error_mi_observation_ids")
})

test_that("installed MI consumers preserve failed-fit diagnostic classes", {
  d <- data.frame(x = 1:10, y = c(1, 1, 3, 4, 5, 8, 9, 10, 12, 16))
  good <- stats::glm(y ~ x, d, family = stats::poisson())
  expect_warning(bad <- stats::glm(y ~ x, d, family = stats::poisson(),
                                   control = stats::glm.control(maxit = 1L)), "did not converge")
  mi <- tt_mi(list(good, bad))
  expect_error(regtab(mi), "imputation 2", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(mi), "imputation 2", class = "tabtools_error_model_convergence")
})
