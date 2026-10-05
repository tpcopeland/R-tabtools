library(testthat)
library(tabtools)

qa_likelihood_data <- function() {
  withr::local_preserve_seed()
  set.seed(3419)
  n <- 180L
  x <- rep(seq(-1, 1, length.out = 30L), 6L)
  d <- data.frame(x = x, z = rep(c(-1, 0, 1), 60L))
  d$count <- stats::rpois(n, exp(.3 + .8 * x))
  d$count[stats::runif(n) < stats::plogis(-.7 + .6 * d$z)] <- 0
  latent <- x + stats::rlogis(n)
  d$ord <- ordered(cut(latent, c(-Inf, -.5, .7, Inf), labels = c("low", "mid", "high")))
  d$multi <- factor(sample(c("A", "B", "C"), n, TRUE), levels = c("A", "B", "C"))
  d$x[c(2L, 5L, 40L)] <- NA_real_
  d$count[10L] <- NA_integer_
  d$ord[13L] <- NA
  d$multi[17L] <- NA
  d
}

test_that("failed pscl optimizers cannot report tables or either covariance policy", {
  skip_if_not_installed("pscl")
  d <- qa_likelihood_data()
  expect_warning(zi <- pscl::zeroinfl(count ~ x | z, data = d,
                                     control = pscl::zeroinfl.control(maxit = 1)),
                 "failed to converge")
  hu <- pscl::hurdle(count ~ x | z, data = d, control = pscl::hurdle.control(maxit = 1))
  for (fit in list(zi, hu)) {
    expect_identical(fit$converged, FALSE)
    expect_true(all(is.finite(stats::vcov(fit))))
    for (policy in c("stata", "model")) {
      expect_error(tt_vcov(fit, policy), class = "tabtools_error_model_convergence")
    }
    for (policy in list("stata", "model", stats::vcov(fit))) {
      expect_error(regtab(fit, vce = policy), class = "tabtools_error_model_convergence")
    }
  }
})

test_that("a real multinom iteration-limit failure cannot use its finite Hessian", {
  skip_if_not_installed("nnet")
  d <- qa_likelihood_data()
  fit <- nnet::multinom(multi ~ x + z, data = d, maxit = 1L, trace = FALSE, model = TRUE)
  expect_identical(fit$convergence, 1L)
  expect_true(all(is.finite(stats::vcov(fit))))
  for (policy in c("stata", "model")) {
    expect_error(tt_vcov(fit, policy), class = "tabtools_error_model_convergence")
  }
  for (policy in list("stata", "model", stats::vcov(fit))) {
    expect_error(regtab(fit, vce = policy), class = "tabtools_error_model_convergence")
  }
})

test_that("a real polr iteration-limit failure cannot report either covariance policy", {
  skip_if_not_installed("MASS")
  d <- qa_likelihood_data()
  fit <- MASS::polr(ord ~ x + z, data = d, control = list(maxit = 1L), Hess = TRUE)
  expect_identical(fit$convergence, 1L)
  expect_true(all(is.finite(stats::vcov(fit))))
  for (policy in c("stata", "model")) {
    expect_error(tt_vcov(fit, policy), class = "tabtools_error_model_convergence")
  }
  for (policy in list("stata", "model", stats::vcov(fit))) {
    expect_error(regtab(fit, vce = policy), class = "tabtools_error_model_convergence")
  }
})

test_that("clm negative convergence diagnostics cannot report finite covariance", {
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  expect_warning(fit <- ordinal::clm(ord ~ x + z, data = d,
                                     control = ordinal::clm.control(maxIter = 1L)),
                 "failed to converge")
  expect_identical(fit$convergence$code, -1L)
  expect_true(all(is.finite(stats::vcov(fit))))
  for (policy in c("stata", "model")) {
    expect_error(tt_vcov(fit, policy), class = "tabtools_error_model_convergence")
  }
  for (policy in list("stata", "model", stats::vcov(fit))) {
    expect_error(regtab(fit, vce = policy), class = "tabtools_error_model_convergence")
  }
})

test_that("converged likelihood models preserve covariance and their complete-case N", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  before <- d
  fits <- list(
    zi = pscl::zeroinfl(count ~ x | z, data = d),
    hu = pscl::hurdle(count ~ x | z, data = d),
    mn = nnet::multinom(multi ~ x + z, data = d, trace = FALSE, model = TRUE),
    po = MASS::polr(ord ~ x + z, data = d, Hess = TRUE),
    cl = ordinal::clm(ord ~ x + z, data = d))
  expect_identical(fits$zi$converged, TRUE)
  expect_identical(fits$hu$converged, TRUE)
  expect_identical(fits$mn$convergence, 0L)
  expect_identical(fits$po$convergence, 0L)
  expect_identical(fits$cl$convergence$code, 0L)
  for (fit in fits) {
    expect_equal(unname(tt_vcov(fit, "model")), unname(stats::vcov(fit)), tolerance = 1e-10)
    expect_true(all(is.finite(tt_vcov(fit, "stata"))))
    tab <- regtab(fit, stats = "n")
    expect_s3_class(tab, "tt_table")
    expect_identical(tab$stored$n_1, 176)
  }
  expect_identical(d, before)
})

test_that("positive clm diagnostics retain valid inference when covariance is finite", {
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  d$x <- d$x * 1e4
  expect_warning(fit <- ordinal::clm(ord ~ x + z, data = d), "nearly unidentifiable")
  expect_true(all(fit$convergence$code > 0))
  expect_true(all(is.finite(stats::vcov(fit))))
  expect_equal(tt_vcov(fit), stats::vcov(fit), tolerance = 1e-10)
  expect_s3_class(regtab(fit), "tt_table")
})

test_that("clm nonunique solutions cannot masquerade as tables with missing covariance", {
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  d$x <- d$x * 1e-6
  expect_warning(fit <- ordinal::clm(ord ~ x + z, data = d), "numerically singular")
  expect_identical(fit$convergence$code, 1L)
  expect_true(all(is.na(stats::vcov(fit))))
  for (policy in c("stata", "model")) {
    expect_error(tt_vcov(fit, policy), class = "tabtools_error_model_covariance")
  }
  for (policy in c("stata", "model")) {
    expect_error(regtab(fit, vce = policy), class = "tabtools_error_model_covariance")
  }
})

test_that("clm aliased slopes preserve their omitted-term semantics", {
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  d$xcopy <- d$x
  fit <- ordinal::clm(ord ~ x + xcopy + z, data = d)
  expect_identical(fit$convergence$code, 0L)
  expect_true(is.na(stats::coef(fit)[["xcopy"]]))
  expect_equal(tt_vcov(fit), stats::vcov(fit), tolerance = 1e-10)
  tab <- regtab(fit, stats = "n")
  expect_identical(tab$stored$n_1, 176)
  row <- tab$meta$regtab_rows
  omitted <- which(row$key == "xcopy")
  expect_length(omitted, 1L)
  expect_identical(row$status[omitted], "omit")
})


test_that("multinom drops absent outcomes while keeping intentionally omitted covariance rows", {
  skip_if_not_installed("nnet")
  d <- qa_likelihood_data()
  d$multi <- factor(d$multi, levels = c("A", "B", "C", "absent"))
  d$g <- factor(rep(c("one", "two"), 90L), levels = c("one", "two", "unused"))
  expect_warning(fit <- nnet::multinom(multi ~ x + g, data = d, trace = FALSE, model = TRUE,
                                       maxit = 1000L, reltol = 1e-12), "absent.*empty")
  expect_identical(fit$convergence, 0L)
  expect_identical(fit$lev, c("A", "B", "C"))
  V <- tt_vcov(fit)
  inactive <- c("B:gunused", "C:gunused")
  expect_true(all(inactive %in% rownames(V)))
  expect_true(all(is.na(V[inactive, , drop = FALSE])))
  expect_true(all(is.na(V[, inactive, drop = FALSE])))
  active <- setdiff(rownames(V), inactive)
  expect_identical(length(active), 6L)
  expect_true(all(is.finite(V[active, active, drop = FALSE])))
  tab <- regtab(fit, stats = "n")
  expect_identical(tab$stored$n_1, 176)
  expect_false(any(grepl("absent|unused", tab$rows$key)))
})


test_that("installed complete-case likelihood fits preserve observed coefficients and intervals", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  d <- qa_likelihood_data()
  counts <- d[complete.cases(d[c("count", "x", "z")]), ]
  multinomial <- droplevels(d[complete.cases(d[c("multi", "x", "z")]), ])
  ordinal <- droplevels(d[complete.cases(d[c("ord", "x", "z")]), ])
  expect_identical(nrow(counts), 176L)
  expect_identical(nrow(multinomial), 176L)
  expect_identical(nrow(ordinal), 176L)
  pairs <- list(
    list(pscl::zeroinfl(count ~ x | z, data = d), pscl::zeroinfl(count ~ x | z, data = counts)),
    list(pscl::hurdle(count ~ x | z, data = d), pscl::hurdle(count ~ x | z, data = counts)),
    list(nnet::multinom(multi ~ x + z, data = d, trace = FALSE, model = TRUE),
         nnet::multinom(multi ~ x + z, data = multinomial, trace = FALSE, model = TRUE)),
    list(MASS::polr(ord ~ x + z, data = d, Hess = TRUE),
         MASS::polr(ord ~ x + z, data = ordinal, Hess = TRUE)),
    list(ordinal::clm(ord ~ x + z, data = d), ordinal::clm(ord ~ x + z, data = ordinal)))
  for (pair in pairs) {
    expect_equal(stats::coef(pair[[1L]]), stats::coef(pair[[2L]]), tolerance = 1e-10)
    expect_equal(tt_vcov(pair[[1L]]), tt_vcov(pair[[2L]]), tolerance = 1e-10)
    before <- pair[[1L]]
    tab <- regtab(pair[[1L]], stats = "n", vce = "model")
    explicit <- regtab(pair[[2L]], stats = "n", vce = "model")
    expect_identical(tab$body, explicit$body)
    expect_equal(tab$meta$regtab_rows, explicit$meta$regtab_rows, tolerance = 1e-10)
    expect_identical(pair[[1L]], before)
  }
})
