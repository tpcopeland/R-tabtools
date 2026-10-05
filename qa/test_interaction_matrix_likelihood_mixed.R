library(testthat)
library(tabtools)

# Two fixed seeds per case; native covariance comparisons use the native
# model reading, with a separate independently scaled GLM aweight check.
iml_data <- function(seed) {
  d <- withr::with_seed(seed, {
    n <- 240L
    x <- rnorm(n)
    z <- rnorm(n)
    h <- factor(rep(c("a", "b"), n / 2), levels = c("a", "b", "c", "ghost"))
    sparse <- seq(10L, 230L, by = 20L)
    h[sparse] <- "c"
    group_effect <- rep(rnorm(40L, sd = 0.8), each = 6L)
    eta <- 0.2 + 0.25 * x + 0.15 * (h == "b") - 0.1 * x * (h == "c")
    trials <- rep(c(6L, 8L, 10L), 80L)
    ybin <- rbinom(n, 1L, plogis(eta))
    yord <- ordered(cut(eta + rlogis(n), c(-Inf, -0.6, 0.7, Inf),
                         labels = c("low", "middle", "high")))
    ymulti <- factor(sample(c("a", "b", "c"), n, replace = TRUE), levels = c("a", "b", "c"))
    # Keep a genuinely sparse predictor level estimable in every outcome.
    ybin[sparse] <- rep(c(0L, 1L), length.out = length(sparse))
    yord[sparse] <- rep(c("low", "middle", "high"), length.out = length(sparse))
    ymulti[sparse] <- rep(c("a", "b", "c"), length.out = length(sparse))
    data.frame(x = x, z = z, h = h, alias = 2 * x,
      y = 1 + eta + group_effect + rnorm(n), ybin = ybin,
      trials = trials, successes = rbinom(n, trials, plogis(eta)),
      yp = rpois(n, exp(eta)), ynb = rnbinom(n, size = 2, mu = exp(eta)),
      yz = ifelse(rbinom(n, 1L, plogis(-1 + 0.2 * z)), 0L, rpois(n, exp(0.7 + eta))),
      yord = yord, ymulti = ymulti,
      ygp = rpois(n, exp(eta + group_effect)),
      g = factor(rep(seq_len(40L), each = 6L)),
      cl = rep(seq_len(40L), each = 6L), w = rep(c(1, 2, 3), 80L),
      include = seq_len(n) <= 224L)
  })
  d$x[3L] <- NA_real_
  d$h[98L] <- NA
  d$w[49L] <- NA_real_
  d$g[64L] <- NA
  d$cl[3L] <- NA_integer_ # outside every model's accepted sample
  d$w[c(4L, 16L)] <- 0
  for (name in c("y", "ybin", "successes", "yp", "ynb", "yz", "yord", "ymulti", "ygp")) {
    d[[name]][17L] <- NA
  }
  d$trials[22L] <- d$successes[22L] <- 0L
  attr(d, "iml_seed") <- seed
  d
}

iml_mask <- function(d, outcome, weighted = TRUE, grouped = FALSE, extra = character()) {
  columns <- c(outcome, "x", "h", if (weighted) "w", if (grouped) "trials", extra)
  accepted <- d$include & stats::complete.cases(d[unique(columns)])
  used <- accepted
  if (weighted) used <- used & !is.na(d$w) & d$w > 0
  if (grouped) used <- used & !is.na(d$trials) & d$trials > 0
  list(accepted = accepted, used = used)
}

iml_keys <- function(terms) {
  map <- c(`(Intercept)` = "_cons", x = "x", z = "z", hb = "2.h", hc = "3.h",
           hghost = "4.h", `x:hb` = "2.h#c.x", `x:hc` = "3.h#c.x",
           `x:hghost` = "4.h#c.x", alias = "alias")
  expect_true(all(terms %in% names(map)))
  unname(map[terms])
}

iml_native <- function(fit) {
  if (inherits(fit, "merMod")) return(list(b = lme4::fixef(fit), V = as.matrix(stats::vcov(fit))))
  if (inherits(fit, "glmmTMB")) return(list(b = glmmTMB::fixef(fit)$cond, V = as.matrix(stats::vcov(fit)$cond)))
  if (inherits(fit, "lme")) return(list(b = nlme::fixef(fit), V = as.matrix(stats::vcov(fit))))
  if (inherits(fit, "glm") && identical(fit$family$family, "gaussian") && any(fit$prior.weights == 0)) {
    expect_warning(V <- as.matrix(stats::vcov(fit)), "observations with zero weight not used for calculating dispersion")
  } else V <- as.matrix(stats::vcov(fit))
  list(b = stats::coef(fit), V = V)
}

iml_coef_rows <- function(fit, d, mask) {
  b <- if (inherits(fit, "merMod")) lme4::fixef(fit) else if (inherits(fit, "glmmTMB"))
    glmmTMB::fixef(fit)$cond else if (inherits(fit, "lme")) nlme::fixef(fit) else stats::coef(fit)
  if (inherits(fit, "multinom")) {
    X <- stats::model.matrix(~x * h, d[mask$used, ])
    empty <- colnames(X)[colSums(abs(X)) == 0]
    expect_identical(empty, c("hghost", "x:hghost"))
    expect_true(all(b[, empty, drop = FALSE] == 0))
    # nnet keeps zero coefficients for empty design columns. They are not
    # estimated contrasts; covariance parity above still covers their rows.
    b <- b[, !colnames(b) %in% empty, drop = FALSE]
    keys <- as.vector(t(outer(rownames(b), iml_keys(colnames(b)), paste, sep = "::")))
    b <- as.vector(t(b))
    ratio <- rep(TRUE, length(b))
  } else if (inherits(fit, c("zeroinfl", "hurdle"))) {
    count <- stats::coef(fit, model = "count")
    zero <- stats::coef(fit, model = "zero")
    keys <- c(paste0("yz::", iml_keys(names(count))),
              paste0(if (inherits(fit, "hurdle")) "selection::" else "zero::", iml_keys(names(zero))))
    b <- c(count, zero)
    ratio <- rep(FALSE, length(b))
  } else if (inherits(fit, "polr")) {
    keys <- c(iml_keys(names(b)), paste0("/::cut", seq_along(fit$zeta)))
    ratio <- c(rep(TRUE, length(b)), rep(FALSE, length(fit$zeta)))
    b <- c(b, fit$zeta)
  } else if (inherits(fit, "clm")) {
    keys <- c(paste0("/::cut", seq_along(fit$alpha)), iml_keys(names(fit$beta)))
    ratio <- c(rep(FALSE, length(fit$alpha)), rep(TRUE, length(fit$beta)))
    b <- c(fit$alpha, fit$beta)
  } else {
    keys <- iml_keys(names(b))
    ratio <- rep(inherits(fit, "glmerMod") || inherits(fit, "glm") &&
                   fit$family$link %in% c("log", "logit"), length(b))
  }
  expected <- unname(b)
  expected[ratio] <- exp(expected[ratio])
  list(keys = keys, raw = unname(b), expected = expected)
}

iml_check <- function(fit, d, seed, first_class, mask, original = FALSE, weighted = TRUE,
                      reported = c("records", "frequency", "frame"), frame = TRUE) {
  reported <- match.arg(reported)
  expect_identical(attr(d, "iml_seed"), seed)
  expect_identical(class(fit)[1L], first_class)
  before <- fit
  native <- iml_native(fit)
  expect_equal(tt_vcov(fit, "model"), native$V, tolerance = 1e-10)
  expect_no_warning(tt <- regtab(fit, vce = "model", keepintercept = TRUE,
                                  noreeffects = TRUE, stats = "n"))
  wanted <- iml_coef_rows(fit, d, mask)
  rows <- tt$meta$regtab_rows
  positions <- match(wanted$keys, rows$key)
  expect_false(anyNA(positions))
  expect_identical(rows$key[positions], wanted$keys)
  active <- !is.na(wanted$raw)
  expect_equal(rows$estimate[positions[active]], wanted$expected[active], tolerance = 1e-10)
  if (any(!active)) expect_identical(rows$status[positions[!active]], rep("omit", sum(!active)))
  ledger <- tt$meta$sample_accounting
  expect_type(ledger, "list")
  expect_equal(nrow(ledger$populations), 1L, tolerance = 0)
  measures <- stats::setNames(ledger$measures$value, ledger$measures$metric)
  eligible_n <- sum(mask$accepted)
  fitted_n <- sum(mask$used)
  expect_equal(measures[c("eligible_n", "used_n", "fitted_n", "zero_weight_n")],
    c(eligible_n = eligible_n, used_n = fitted_n, fitted_n = fitted_n,
      zero_weight_n = eligible_n - fitted_n), tolerance = 0)
  expect_equal(measures[["frame_n"]], if (frame) eligible_n else NA_real_, tolerance = 0)
  expect_equal(measures[["input_n"]], if (original) nrow(d) else NA_real_, tolerance = 0)
  expect_equal(measures[["excluded_n"]], if (original) nrow(d) - fitted_n else NA_real_, tolerance = 0)
  weights <- if (weighted) d$w[mask$used] else rep(1, fitted_n)
  expect_equal(measures[["weight_sum"]], sum(weights), tolerance = 1e-12)
  expect_equal(measures[["effective_n"]], sum(weights)^2 / sum(weights^2), tolerance = 1e-12)
  reported_n <- switch(reported, records = fitted_n, frequency = sum(weights), frame = eligible_n)
  expect_equal(measures[["reported_n"]], reported_n, tolerance = 1e-12)
  expect_equal(tt$stored$n_1, reported_n, tolerance = 1e-12)
  expect_identical(attr(as.data.frame(tt), "sample_accounting"), ledger)
  expect_identical(fit, before)
  invisible(tt)
}

iml_seed_done <- function(case_id, seed) {
  cat(sprintf("\nIM-SEED case_id=%s seed=%d\n", case_id, seed))
}

test_that("IML-01 lm: sparse interactions, rank alias, incomplete weights and clusters [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    before <- d
    fit <- lm(y ~ x * h + alias, d, weights = w, subset = include, na.action = na.exclude)
    mask <- iml_mask(d, "y")
    iml_check(fit, d, seed, "lm", mask)
    expect_true(is.na(stats::coef(fit)[["alias"]]))
    X <- stats::model.matrix(fit)[fit$weights > 0, !is.na(stats::coef(fit)), drop = FALSE]
    w <- d$w[mask$used]
    e <- d$y[mask$used] - drop(X %*% stats::coef(fit)[!is.na(stats::coef(fit))])
    clusters <- d$cl[mask$used]
    score <- rowsum(X * (w * e), clusters, reorder = FALSE)
    bread <- solve(crossprod(X, X * w))
    G <- length(unique(clusters))
    N <- length(w)
    expected <- bread %*% crossprod(score) %*% bread * G / (G - 1) * (N - 1) / (N - ncol(X))
    native_names <- names(stats::coef(fit))[!is.na(stats::coef(fit))]
    actual <- tt_vcov(fit, "cluster", d$cl[mask$accepted])[native_names, native_names, drop = FALSE]
    expect_equal(actual, expected, tolerance = 1e-10)
    expect_identical(d, before)
    iml_seed_done("IML-01", seed)
  }
})

test_that("IML-02 Gaussian glm: weighted incomplete sparse interactions [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- glm(y ~ x * h, gaussian(), d, weights = w, subset = include, na.action = na.exclude)
    iml_check(fit, d, seed, "glm", iml_mask(d, "y"), original = TRUE)
    iml_seed_done("IML-02", seed)
  }
})

test_that("IML-03 binary glm: case weights and independent aweight scale [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- glm(ybin ~ x * h, binomial(), d, weights = w, subset = include,
                control = glm.control(epsilon = 1e-12))
    mask <- iml_mask(d, "ybin")
    iml_check(fit, d, seed, "glm", mask, original = TRUE)
    expect_equal(tt_vcov(fit, "stata"), stats::vcov(fit) * mean(d$w[mask$used]), tolerance = 1e-8)
    iml_seed_done("IML-03", seed)
  }
})

test_that("IML-04 grouped binomial: zero trials differ from original case weights [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- glm(cbind(successes, trials - successes) ~ x * h, binomial(), d,
                weights = w, subset = include, control = glm.control(epsilon = 1e-12))
    mask <- iml_mask(d, "successes", grouped = TRUE)
    iml_check(fit, d, seed, "glm", mask, original = TRUE)
    X <- stats::model.matrix(fit)
    p <- plogis(drop(X %*% stats::coef(fit)))
    information <- crossprod(X, X * (d$w[mask$accepted] * d$trials[mask$accepted] * p * (1 - p)))
    expect_equal(tt_vcov(fit, "stata"), solve(information), tolerance = 1e-10)
    expect_false(isTRUE(all.equal(sum(fit$prior.weights), sum(d$w[mask$used]))))
    iml_seed_done("IML-04", seed)
  }
})

test_that("IML-05 Poisson glm: sparse interactions and independent aweight scale [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- glm(yp ~ x * h, poisson(), d, weights = w, subset = include,
                control = glm.control(epsilon = 1e-12))
    mask <- iml_mask(d, "yp")
    iml_check(fit, d, seed, "glm", mask, original = TRUE)
    expect_equal(tt_vcov(fit, "stata"), stats::vcov(fit) * mean(d$w[mask$used]), tolerance = 1e-8)
    iml_seed_done("IML-05", seed)
  }
})

test_that("IML-06 MASS negbin: weighted sparse interactions with omitted observations [15031;27041]", {
  skip_if_not_installed("MASS")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- MASS::glm.nb(ynb ~ x * h, d, weights = w, subset = include, control = glm.control(maxit = 100))
    iml_check(fit, d, seed, "negbin", iml_mask(d, "ynb"))
    iml_seed_done("IML-06", seed)
  }
})

test_that("IML-07 MASS polr: sparse weighted categories and native retained Hessian [15031;27041]", {
  skip_if_not_installed("MASS")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    expect_warning(fit <- MASS::polr(yord ~ x * h, d, weights = w, subset = include, Hess = TRUE,
      na.action = na.exclude, control = list(maxit = 200, reltol = 1e-10)),
      "design appears to be rank-deficient, so dropping some coefs")
    iml_check(fit, d, seed, "polr", iml_mask(d, "yord"), reported = "frequency")
    iml_seed_done("IML-07", seed)
  }
})

test_that("IML-08 ordinal clm: sparse weighted categories and cutpoints [15031;27041]", {
  skip_if_not_installed("ordinal")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- ordinal::clm(yord ~ x * h, data = d, weights = w, subset = include, na.action = na.exclude)
    iml_check(fit, d, seed, "clm", iml_mask(d, "yord"), reported = "frequency")
    iml_seed_done("IML-08", seed)
  }
})

test_that("IML-09 nnet multinom: sparse multi-equation weighted interactions [15031;27041]", {
  skip_if_not_installed("nnet")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- nnet::multinom(ymulti ~ x * h, d, weights = w, subset = include,
                          trace = FALSE, Hess = TRUE, model = TRUE, maxit = 300, reltol = 1e-10)
    iml_check(fit, d, seed, "multinom", iml_mask(d, "ymulti"), reported = "frequency")
    iml_seed_done("IML-09", seed)
  }
})

test_that("IML-10 pscl zeroinfl: incomplete weighted count and zero equations [15031;27041]", {
  skip_if_not_installed("pscl")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- pscl::zeroinfl(yz ~ x * h | z, d, weights = w, subset = include,
                          control = pscl::zeroinfl.control(maxit = 300, reltol = 1e-10))
    iml_check(fit, d, seed, "zeroinfl", iml_mask(d, "yz", extra = "z"), reported = "frequency")
    iml_seed_done("IML-10", seed)
  }
})

test_that("IML-11 pscl hurdle: incomplete weighted count and selection equations [15031;27041]", {
  skip_if_not_installed("pscl")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- pscl::hurdle(yz ~ x * h | z, d, weights = w, subset = include,
                        control = pscl::hurdle.control(maxit = 300, reltol = 1e-10))
    iml_check(fit, d, seed, "hurdle", iml_mask(d, "yz", extra = "z"), reported = "frequency")
    iml_seed_done("IML-11", seed)
  }
})

test_that("IML-12 lme4 lmerMod: sparse interactions, missing group and zero precision weights [15031;27041]", {
  skip_if_not_installed("lme4")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- lme4::lmer(y ~ x * h + (1 | g), d, weights = w, subset = include,
                       na.action = na.exclude, REML = FALSE)
    iml_check(fit, d, seed, "lmerMod", iml_mask(d, "y", extra = "g"), reported = "frame")
    iml_seed_done("IML-12", seed)
  }
})

test_that("IML-13 lme4 glmerMod: sparse Poisson interactions and incomplete groups [15031;27041]", {
  skip_if_not_installed("lme4")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- lme4::glmer(ygp ~ x * h + (1 | g), d, poisson(), weights = w, subset = include,
                        control = lme4::glmerControl(optimizer = "bobyqa"))
    iml_check(fit, d, seed, "glmerMod", iml_mask(d, "ygp", extra = "g"), reported = "frame")
    iml_seed_done("IML-13", seed)
  }
})

test_that("IML-14 lmerTest native alias: retained class and sparse weighted interactions [15031;27041]", {
  skip_if_not_installed("lmerTest")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- lmerTest::lmer(y ~ x * h + (1 | g), d, weights = w, subset = include, REML = FALSE)
    iml_check(fit, d, seed, "lmerModLmerTest", iml_mask(d, "y", extra = "g"), reported = "frame")
    iml_seed_done("IML-14", seed)
  }
})

test_that("IML-15 nlme lme: unit case weights, sparse interactions and incomplete groups [15031;27041]", {
  skip_if_not_installed("nlme")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    d$h <- droplevels(d$h) # nlme otherwise refuses an empty design column.
    fit <- nlme::lme(y ~ x * h, random = ~1 | g, data = d, subset = include,
                      na.action = na.exclude, method = "ML")
    iml_check(fit, d, seed, "lme", iml_mask(d, "y", weighted = FALSE, extra = "g"),
               original = TRUE, weighted = FALSE, frame = FALSE)
    iml_seed_done("IML-15", seed)
  }
})

test_that("IML-16 glmmTMB: adjusted alias, unused level and incomplete weighted groups [15031;27041]", {
  skip_if_not_installed("glmmTMB")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    expect_message(fit <- glmmTMB::glmmTMB(y ~ x * h + alias + (1 | g), d,
      weights = w, subset = include, control = glmmTMB::glmmTMBControl(rank_check = "adjust")),
      "dropping columns.*alias")
    iml_check(fit, d, seed, "glmmTMB", iml_mask(d, "y", extra = "g"), reported = "frame")
    expect_true(is.na(glmmTMB::fixef(fit)$cond[["alias"]]))
    iml_seed_done("IML-16", seed)
  }
})

test_that("IML-17 cluster missingness: accepted rows cannot lose a cluster silently [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- lm(y ~ x * h, d, weights = w, subset = include)
    expect_identical(class(fit)[1L], "lm")
    expect_identical(attr(d, "iml_seed"), seed)
    d$cl[8L] <- NA_integer_
    mask <- iml_mask(d, "y")
    clusters <- d$cl[mask$accepted]
    expect_error(tt_vcov(fit, "cluster", clusters), "missing", class = "rlang_error")
    expect_error(regtab(fit, vce = "cluster", cluster = clusters), "missing", class = "rlang_error")
    iml_seed_done("IML-17", seed)
  }
})

test_that("IML-18 failed glm: sparse incomplete weighted fit refuses inference [15031;27041]", {
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    expect_warning(fit <- glm(yp ~ x * h, poisson(), d, weights = w, subset = include,
      control = glm.control(maxit = 1)), "algorithm did not converge")
    expect_identical(class(fit)[1L], "glm")
    expect_identical(attr(d, "iml_seed"), seed)
    expect_identical(fit$converged, FALSE)
    for (vce in c("stata", "model", "robust")) {
      expect_error(tt_vcov(fit, vce), class = "tabtools_error_model_convergence")
      expect_error(regtab(fit, vce = vce), class = "tabtools_error_model_convergence")
    }
    iml_seed_done("IML-18", seed)
  }
})

test_that("IML-19 multinom default frame storage: omitted weights retain the original fit [15031;27041]", {
  skip_if_not_installed("nnet")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    fit <- nnet::multinom(ymulti ~ x * h, d, weights = w, subset = include,
                          trace = FALSE, Hess = TRUE, maxit = 300, reltol = 1e-10)
    expect_null(fit$model)
    tt <- iml_check(fit, d, seed, "multinom", iml_mask(d, "ymulti"),
                    reported = "frequency", frame = FALSE)
    stored <- nnet::multinom(ymulti ~ x * h, d, weights = w, subset = include,
      trace = FALSE, Hess = TRUE, model = TRUE, maxit = 300, reltol = 1e-10)
    expect_equal(stats::coef(fit), stats::coef(stored), tolerance = 1e-12)
    expect_equal(tt_vcov(fit, "model"), stats::vcov(stored), tolerance = 1e-12)
    control <- regtab(stored, vce = "model", keepintercept = TRUE, noreeffects = TRUE, stats = "n")
    expect_identical(tt$body, control$body)
    iml_seed_done("IML-19", seed)
  }
})

test_that("IML-20 multinom count response: native unit totals remain distinct from case weights [15031;27041]", {
  skip_if_not_installed("nnet")
  for (seed in c(15031L, 27041L)) {
    d <- iml_data(seed)
    d$counts <- I(cbind(a = d$yp + 1, b = 1 + seq_len(nrow(d)) %% 3,
                        c = 1 + seq_len(nrow(d)) %% 4))
    mask <- iml_mask(d, "yp")
    fit <- nnet::multinom(counts ~ x * h, d, weights = w, subset = include,
      trace = FALSE, Hess = TRUE, maxit = 300, reltol = 1e-10)
    stored <- nnet::multinom(counts ~ x * h, d, weights = w, subset = include,
      trace = FALSE, Hess = TRUE, model = TRUE, maxit = 300, reltol = 1e-10)
    before <- fit
    expect_identical(attr(d, "iml_seed"), seed)
    expect_identical(class(fit)[1L], "multinom")
    expect_identical(unname(attr(fit$terms, "dataClasses")[1]), "nmatrix.3")
    expect_equal(tt_vcov(fit, "model"), stats::vcov(stored), tolerance = 1e-12)
    expect_no_warning(tt <- regtab(fit, vce = "model", keepintercept = TRUE, stats = "n"))
    control <- regtab(stored, vce = "model", keepintercept = TRUE, stats = "n")
    expect_identical(tt$body, control$body)
    wanted <- iml_coef_rows(fit, d, mask)
    rows <- tt$meta$regtab_rows
    positions <- match(wanted$keys, rows$key)
    expect_false(anyNA(positions))
    expect_equal(rows$estimate[positions], wanted$expected, tolerance = 1e-10)
    counts <- stats::setNames(tt$meta$sample_accounting$measures$value,
                              tt$meta$sample_accounting$measures$metric)
    units <- sum(d$w[mask$accepted] * rowSums(d$counts[mask$accepted, ]))
    expect_equal(counts[["eligible_n"]], sum(mask$accepted), tolerance = 0)
    expect_true(all(is.na(counts[c("frame_n", "fitted_n", "used_n", "zero_weight_n", "weight_sum", "effective_n")])) )
    expect_equal(counts[["reported_n"]], units, tolerance = 0)
    known <- stats::setNames(control$meta$sample_accounting$measures$value,
                              control$meta$sample_accounting$measures$metric)
    expect_equal(known[c("fitted_n", "weight_sum")],
      c(fitted_n = sum(mask$used), weight_sum = sum(d$w[mask$used])), tolerance = 0)
    expect_false(isTRUE(all.equal(known[["weight_sum"]], units)))
    expect_identical(fit, before)
    iml_seed_done("IML-20", seed)
  }
})
