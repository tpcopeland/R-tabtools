# Milestone H task H2 (external review F03, F23, F24, F27, F30): multinom
# offsets, count-matrix responses, fits whose data changed, weight decay.

h2_f03 <- function() {
  set.seed(482)
  n <- 300
  d <- data.frame(y = factor(sample(1:3, n, TRUE)), x = rnorm(n))
  d$off <- I(cbind(0, rnorm(n, sd = 2), rnorm(n, sd = 2)))
  d
}

test_that("offsets enter the information: SEs equal an optimHess of the offset-inclusive likelihood (F03)", {
  skip_if_not_installed("nnet")
  d <- h2_f03()
  f <- nnet::multinom(y ~ x + offset(off), d, trace = FALSE, Hess = FALSE, model = TRUE,
                      reltol = 1e-12, maxit = 1000)
  X <- model.matrix(f)
  b <- as.vector(t(coef(f)))
  iy <- as.integer(d$y)
  nll <- function(b) {
    B <- matrix(b, 2, 2, byrow = TRUE)
    eta <- cbind(0, X %*% t(B)) + d$off
    mx <- apply(eta, 1, max)
    sum(mx + log(rowSums(exp(eta - mx))) - eta[cbind(seq_len(nrow(d)), iy)])
  }
  expect_equal(nll(b), -as.numeric(logLik(f)), tolerance = 1e-10)
  oracle <- sqrt(diag(solve(optimHess(b, nll))))
  se <- sqrt(diag(tt_vcov(f)))
  expect_equal(unname(se), oracle, tolerance = 1e-6)
  # The review's table (package SEs 0.1401732 ... before the fix).
  expect_equal(unname(se), c(0.1749905, 0.1631848, 0.1764084, 0.1619503), tolerance = 1e-6)
  tt <- regtab(f, stats = c("n", "ll", "r2"))
  expect_identical(tt$body[[3]][1:2], c("(0.65, 1.23)", "(0.51, 0.96)"))
  # Pseudo R2 against the constant-only model with the same offset (H-D10):
  # 0.0053, where the offset-free null gave -0.4395 (F27).
  f0 <- nnet::multinom(y ~ 1 + offset(off), d, trace = FALSE, reltol = 1e-12, maxit = 1000)
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(s$r2_p, 1 - as.numeric(logLik(f) / logLik(f0)), tolerance = 1e-7)
  expect_identical(tt$body[[2]][tt$body[[1]] == "Pseudo R\u00b2"], "0.005")
})

test_that("the offset-only null is the maximum also for large offsets", {
  skip_if_not_installed("nnet")
  set.seed(482)
  n <- 300
  d <- data.frame(y = factor(sample(1:3, n, TRUE)), x = rnorm(n))
  d$off <- I(cbind(0, rnorm(n, sd = 8), rnorm(n, sd = 8)))
  f <- nnet::multinom(y ~ x + offset(off), d, trace = FALSE, reltol = 1e-12, maxit = 1000)
  f0 <- nnet::multinom(y ~ 1 + offset(off), d, trace = FALSE, reltol = 1e-12, maxit = 1000)
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(s$r2_p, 1 - as.numeric(logLik(f) / logLik(f0)), tolerance = 1e-7)
})

test_that("a binary multinom's vector offset is a logit offset", {
  skip_if_not_installed("nnet")
  set.seed(3)
  n <- 300
  d <- data.frame(y = factor(sample(1:2, n, TRUE)), x = rnorm(n), o = rnorm(n))
  m <- nnet::multinom(y ~ x + offset(o), d, trace = FALSE, reltol = 1e-12)
  g <- glm(y ~ x + offset(o), binomial, d, control = glm.control(epsilon = 1e-14))
  expect_equal(unname(sqrt(diag(tt_vcov(m)))), unname(sqrt(diag(tt_vcov(g)))), tolerance = 1e-6)
})

test_that("a count-matrix response is not weighted twice: aggregated = expanded fit (F23)", {
  skip_if_not_installed("nnet")
  g <- data.frame(x = c(-1, 0, 1, 2))
  cnt <- cbind(c(30, 20, 10, 5), c(10, 20, 25, 30), c(5, 10, 20, 40))
  ma <- nnet::multinom(cnt ~ x, g, trace = FALSE, reltol = 1e-12)
  long <- data.frame(x = rep(rep(g$x, 3), c(cnt)), y = factor(rep(rep(1:3, each = 4), c(cnt))))
  ml <- nnet::multinom(y ~ x, long, trace = FALSE, reltol = 1e-12)
  se_a <- sqrt(diag(tt_vcov(ma)))
  expect_equal(unname(se_a), unname(sqrt(diag(tt_vcov(ml)))), tolerance = 1e-6)
  expect_equal(unname(se_a), c(0.19086, 0.17861, 0.23977, 0.20200), tolerance = 1e-4)
  S <- c("n", "ll", "r2")
  expect_identical(regtab(ma, stats = S)$body, regtab(ml, stats = S)$body)
  # With prior weights: each aggregated row's weight on its units.
  w <- c(1.5, 0.5, 2, 1)
  mw <- nnet::multinom(cnt ~ x, g, weights = w, trace = FALSE, reltol = 1e-12)
  long$w <- rep(rep(w, 3), c(cnt))
  mlw <- nnet::multinom(y ~ x, long, weights = w, trace = FALSE, reltol = 1e-12)
  expect_equal(unname(sqrt(diag(tt_vcov(mw)))), unname(sqrt(diag(tt_vcov(mlw)))), tolerance = 1e-6)
  expect_identical(suppressWarnings(regtab(mw, stats = S))$body, suppressWarnings(regtab(mlw, stats = S))$body)
})

test_that("data changed after fitting: refused unless they reproduce the fit (F24, H-D7)", {
  skip_if_not_installed("nnet")
  set.seed(1)
  n <- 300
  dat <- data.frame(y = factor(sample(c("a", "b", "c"), n, TRUE)), x = rnorm(n),
                    g = sample(c("a", "b", "c"), n, TRUE))
  fit <- nnet::multinom(y ~ x, dat, trace = FALSE)
  fitm <- nnet::multinom(y ~ x, dat, trace = FALSE, model = TRUE)
  want <- regtab(fit, stats = "n")$body
  keep <- dat
  dat <- dat[dat$g != "a", ]
  expect_error(regtab(fit, stats = "n"), "cannot be matched to the fit: the data give")
  expect_error(regtab(fit), "model = TRUE")
  expect_error(tt_vcov(fit), "cannot be matched")
  # A model = TRUE fit keeps its data (labels are still looked up in the
  # current data, which no longer cover its rows: a warning, 5w P2-3).
  w <- capture_warnings(tt <- regtab(fitm, stats = "n"))
  expect_match(w, "could not be restored")
  expect_identical(tt$body, want)
  # Re-sorted with the row names kept: the same fit.
  dat <- keep[sample(n), ]
  expect_identical(regtab(fit, stats = "n")$body, want)
  # Edited, or gone.
  dat <- keep
  dat$x <- rev(dat$x)
  expect_error(regtab(fit), "fitted probabilities differ")
  rm(dat)
  expect_error(regtab(fit), "cannot be matched")
  expect_identical(regtab(fitm, stats = "n")$body, want)
})

test_that("weight decay is refused by name (F30)", {
  skip_if_not_installed("nnet")
  set.seed(1)
  d <- data.frame(y = factor(sample(c("a", "b", "c"), 200, TRUE)), x = rnorm(200))
  f <- nnet::multinom(y ~ x, d, trace = FALSE, decay = 5)
  expect_error(regtab(f), "decay = 5", fixed = TRUE)
  expect_error(regtab(f), "weight decay")
  expect_s3_class(regtab(nnet::multinom(y ~ x, d, trace = FALSE, decay = 0)), "tt_table")
})
