test_that("multinom without a saved frame reconstructs missing weights and subsets", {
  skip_if_not_installed("nnet")
  d <- withr::with_seed(22, data.frame(y = factor(rep(c("a", "b", "c"), 20)),
    x = rnorm(60), w = rep(c(1, 2), 30), include = seq_len(60) <= 55))
  d$w[9] <- NA_real_
  d$x[12] <- NA_real_
  d$y[20] <- NA
  d$w[16] <- 0
  mask <- d$include & stats::complete.cases(d[c("y", "x", "w")])
  before <- d
  for (action in list(na.omit, na.exclude)) {
    stored <- nnet::multinom(y ~ x, data = d, weights = w, subset = include,
      na.action = action, model = TRUE, Hess = TRUE, trace = FALSE, maxit = 200)
    fit <- nnet::multinom(y ~ x, data = d, weights = w, subset = include,
      na.action = action, Hess = TRUE, trace = FALSE, maxit = 200)
    before_fit <- fit
    expect_identical(class(fit)[1L], "multinom")
    expect_identical(fit$convergence, 0L)
    expect_null(fit$model)
    expect_identical(rownames(stored$model), rownames(d)[mask])
    expect_equal(stats::coef(fit), stats::coef(stored), tolerance = 1e-12)
    expect_equal(tt_vcov(fit, "model"), stats::vcov(stored), tolerance = 1e-12)
    expect_no_warning(tt <- regtab(fit, vce = "model", stats = "n"))
    reference <- regtab(stored, vce = "model", stats = "n")
    expect_identical(tt$body, reference$body)
    expect_equal(tt$meta$regtab_rows$estimate, reference$meta$regtab_rows$estimate, tolerance = 1e-12)
    counts <- stats::setNames(tt$meta$sample_accounting$measures$value,
                             tt$meta$sample_accounting$measures$metric)
    expect_equal(counts[c("eligible_n", "used_n", "fitted_n", "zero_weight_n")],
      c(eligible_n = sum(mask), used_n = sum(mask & d$w > 0),
        fitted_n = sum(mask & d$w > 0), zero_weight_n = sum(mask & d$w == 0)), tolerance = 0)
    expect_equal(counts[["reported_n"]], sum(d$w[mask]), tolerance = 0)
    expect_true(is.na(counts[["input_n"]]))
    expect_true(is.na(counts[["frame_n"]]))
    expect_equal(counts[["weight_sum"]], sum(d$w[mask]), tolerance = 0)
    used_weights <- d$w[mask & d$w > 0]
    expect_equal(counts[["effective_n"]], sum(used_weights)^2 / sum(used_weights^2), tolerance = 1e-12)
    expect_identical(fit, before_fit)
  }
  expect_identical(d, before)
})

test_that("multinom count responses do not turn native unit totals into case weights", {
  skip_if_not_installed("nnet")
  d <- withr::with_seed(73, data.frame(x = rnorm(36), w = rep(c(1, 2), 18), include = seq_len(36) <= 32))
  d$counts <- I(cbind(a = 1 + seq_len(36) %% 3, b = 1 + seq_len(36) %% 4,
                      c = 1 + seq_len(36) %% 5))
  d$x[5] <- NA_real_
  d$w[8] <- 0
  mask <- d$include & stats::complete.cases(d[c("x", "w", "counts")])
  before <- d
  stored <- nnet::multinom(counts ~ x, d, weights = w, subset = include, model = TRUE,
                            Hess = TRUE, trace = FALSE, maxit = 200)
  fit <- nnet::multinom(counts ~ x, d, weights = w, subset = include,
                         Hess = TRUE, trace = FALSE, maxit = 200)
  expect_identical(unname(attr(fit$terms, "dataClasses")[1]), "nmatrix.3")
  expect_equal(as.numeric(fit$weights), d$w[mask] * rowSums(d$counts[mask, ]), tolerance = 0)
  expect_equal(tt_vcov(fit, "model"), stats::vcov(stored), tolerance = 1e-12)
  expect_no_warning(tt <- regtab(fit, vce = "model", stats = "n"))
  control <- regtab(stored, vce = "model", stats = "n")
  expect_identical(tt$body, control$body)
  values <- function(x) stats::setNames(x$meta$sample_accounting$measures$value,
                                       x$meta$sample_accounting$measures$metric)
  counts <- values(tt)
  expect_equal(counts[["eligible_n"]], sum(mask), tolerance = 0)
  expect_true(all(is.na(counts[c("fitted_n", "used_n", "zero_weight_n", "weight_sum", "effective_n")])))
  expect_equal(counts[["reported_n"]], sum(d$w[mask] * rowSums(d$counts[mask, ])), tolerance = 0)
  known <- values(control)
  expect_equal(known[c("fitted_n", "weight_sum")],
    c(fitted_n = sum(mask & d$w > 0), weight_sum = sum(d$w[mask])), tolerance = 0)
  expect_false(isTRUE(all.equal(known[["weight_sum"]], known[["reported_n"]])))
  expect_identical(d, before)
})

test_that("multinom without response-class evidence keeps weight accounting unknown", {
  skip_if_not_installed("nnet")
  d <- withr::with_seed(901, data.frame(y = factor(rep(c("a", "b", "c"), 12)),
                                      x = rnorm(36), w = rep(c(1, 2), 18)))
  fit <- nnet::multinom(y ~ x, d, weights = w, Hess = TRUE, trace = FALSE, maxit = 200)
  reference <- regtab(fit, vce = "model", stats = "n")
  attr(fit$terms, "dataClasses") <- NULL
  before <- fit
  expect_no_warning(tt <- regtab(fit, vce = "model", stats = "n"))
  expect_identical(tt$body, reference$body)
  counts <- stats::setNames(tt$meta$sample_accounting$measures$value,
                            tt$meta$sample_accounting$measures$metric)
  expect_equal(counts[["eligible_n"]], nrow(d), tolerance = 0)
  expect_true(all(is.na(counts[c("fitted_n", "used_n", "weight_sum", "effective_n")])) )
  expect_identical(fit, before)
})
