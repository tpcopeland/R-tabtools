library(testthat)
library(tabtools)

qa_model_sample <- function(table, population = 1L) {
  sample <- table$meta$sample_accounting
  expect_type(sample, "list")
  if (is.null(sample)) return(NULL)
  id <- sample$populations$id[[population]]
  measures <- sample$measures[sample$measures$population_id == id, ]
  stats::setNames(measures$value, measures$metric)
}

test_that("installed fitted accounting uses immutable saved counts and raw case weights", {
  d <- data.frame(y = seq_len(24) + sin(seq_len(24)), x = seq_len(24),
                  w = rep(c(1, 2, 3), 8))
  d$x[c(3, 7)] <- NA_real_
  d$y[12] <- NA_real_
  d$w[c(4, 10)] <- 0
  mask <- seq_len(nrow(d)) <= 20 & stats::complete.cases(d)
  used <- mask & d$w > 0
  for (action in list(na.omit, na.exclude)) {
    for (store in c(TRUE, FALSE)) {
      fit <- glm(y ~ x, gaussian(), d, weights = w, subset = seq_len(24) <= 20,
                 na.action = action, model = store)
      before <- fit
      expect_no_warning(tt <- regtab(fit, vce = "robust", stats = "n"))
      counts <- qa_model_sample(tt)
      if (is.null(counts)) next
      expect_equal(counts[c("input_n", "eligible_n", "fitted_n", "zero_weight_n", "excluded_n")],
        c(input_n = 24, eligible_n = sum(mask), fitted_n = sum(used),
          zero_weight_n = sum(mask & !used), excluded_n = sum(!used)))
      expect_equal(counts[["frame_n"]], if (store) sum(mask) else NA_real_)
      expect_equal(counts[["weight_sum"]], sum(d$w[used]))
      expect_equal(counts[["effective_n"]], sum(d$w[used])^2 / sum(d$w[used]^2))
      expect_equal(counts[["reported_n"]], tt$stored$n_1)
      expect_identical(attr(as.data.frame(tt), "sample_accounting"), tt$meta$sample_accounting)
      expect_identical(fit, before)
    }
  }
  # glm retains its input itself: later edits of the symbol are not evidence.
  fit <- glm(y ~ x, gaussian(), d, weights = w, subset = seq_len(24) <= 20)
  d <- data.frame(y = 1:2, x = 1:2, w = 1)
  expect_equal(qa_model_sample(regtab(fit, vce = "robust"))[["input_n"]], 24)
})

test_that("installed UV and MI preserve sources with overlapping or disjoint samples", {
  d <- data.frame(y = c(3, 4, 8, 9, 13, 15, 2, 3, 2, 7, 9, 8),
                  x = c(seq_len(6), rep(NA_real_, 6)),
                  z = c(rep(NA_real_, 6), seq_len(6)))
  uv <- regtab_uv(d, "y", c("x", "z"), method = lm)
  tt <- regtab(uv, stats = "n")
  counts <- qa_model_sample(tt)
  if (is.null(counts)) return(invisible(NULL))
  expect_identical(tt$meta$sample_accounting$populations$variable, c("x", "z"))
  for (k in 1:2) expect_equal(qa_model_sample(tt, k)[c("input_n", "fitted_n", "excluded_n")],
                             c(input_n = 12, fitted_n = 6, excluded_n = 6))
  d <- data.frame(y = seq_len(12) + sin(seq_len(12)), x = seq_len(12))
  d$x[2] <- NA_real_
  d2 <- rbind(d, data.frame(y = 13:15, x = 13:15))
  fits <- list(glm(y ~ x, gaussian(), d),
               glm(y ~ x, gaussian(), d2, subset = seq_len(15) <= 12))
  ids <- rep(list(paste0("private-subject-", seq_len(12)[-2])), 2)
  tt <- regtab(tt_mi(fits, observation_ids = ids, sample_check = "strict"), stats = "n")
  expect_equal(tt$meta$sample_accounting$populations$imputation, c(1, 2))
  expect_equal(qa_model_sample(tt, 1)[c("input_n", "fitted_n", "excluded_n")],
               c(input_n = 12, fitted_n = 11, excluded_n = 1))
  expect_equal(qa_model_sample(tt, 2)[c("input_n", "fitted_n", "excluded_n")],
               c(input_n = 15, fitted_n = 11, excluded_n = 4))
  expect_false(any(grepl("private-subject", unlist(tt$meta$sample_accounting), fixed = TRUE)))
  expect_equal(nrow(tt$meta$sample_accounting$populations), 2)
})

test_that("installed mixed fits distinguish frame rows from zero-weight contributors", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- withr::with_seed(784, {
    n <- 120
    g <- factor(rep(1:20, each = 6))
    x <- rnorm(n)
    data.frame(g = g, x = x, y = 1 + x + rep(rnorm(20), each = 6) + rnorm(n),
               w = rep(c(1, 2, 3), 40), include = seq_len(n) <= 110)
  })
  d$x[c(2, 19)] <- NA_real_
  d$y[7] <- NA_real_
  d$w[29] <- NA_real_
  d$w[c(4, 10)] <- 0
  eligible <- d$include & stats::complete.cases(d)
  used <- eligible & d$w > 0
  fits <- list(lme4::lmer(y ~ x + (1 | g), d, weights = w, subset = include),
               glmmTMB::glmmTMB(y ~ x + (1 | g), d, weights = w, subset = include))
  for (fit in fits) {
    before <- fit
    expect_no_warning(tt <- regtab(fit, vce = "model", noreeffects = TRUE, stats = "n"))
    counts <- qa_model_sample(tt)
    if (is.null(counts)) next
    expect_equal(counts[c("eligible_n", "frame_n", "used_n", "fitted_n", "zero_weight_n")],
      c(eligible_n = sum(eligible), frame_n = sum(eligible), used_n = sum(used),
        fitted_n = sum(used), zero_weight_n = 2))
    expect_true(is.na(counts[["input_n"]]))
    expect_equal(counts[["reported_n"]], stats::nobs(fit))
    expect_equal(counts[["weight_sum"]], sum(d$w[used]))
    native <- if (inherits(fit, "glmmTMB")) as.matrix(stats::vcov(fit)$cond) else as.matrix(stats::vcov(fit))
    expect_equal(tt_vcov(fit, "model"), native, tolerance = 1e-12)
    expect_identical(fit, before)
  }
})

test_that("installed Cox risk-set expansion does not invent accepted records", {
  skip_if_not_installed("survival")
  d <- survival::lung[1:80, ]
  fit <- survival::coxph(survival::Surv(time, status) ~ age + tt(age), d,
                         tt = function(x, t, ...) x * log(t))
  before <- fit
  expect_gt(nrow(fit$y), fit$n)
  expect_no_warning(tt <- regtab(fit, stats = c("n", "events")))
  counts <- qa_model_sample(tt)
  if (is.null(counts)) return(invisible(NULL))
  expect_equal(counts[c("eligible_n", "used_n", "fitted_n", "frame_n")],
    c(eligible_n = nrow(d), used_n = nrow(d), fitted_n = nrow(d), frame_n = nrow(fit$y)))
  expect_true(is.na(counts[["input_n"]]))
  expect_true(is.na(counts[["observed_n"]]))
  expect_equal(counts[["weight_sum"]], nrow(d))
  expect_equal(counts[["reported_n"]], tt$stored$n_1)
  expect_false(isTRUE(all.equal(counts[["fitted_n"]], stats::nobs(fit))))
  expect_equal(tt$stored$events_1, stats::nobs(fit))
  expect_equal(tt_vcov(fit, "model"), stats::vcov(fit), tolerance = 1e-12)
  expect_identical(fit, before)
})

test_that("installed survival frequency N stays distinct from accepted record N", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$w <- 1 + seq_len(nrow(d)) %% 3
  d$age[c(2, 11)] <- NA_real_
  d$w[19] <- NA_real_
  eligible <- seq_len(nrow(d)) <= 150 & stats::complete.cases(d[c("time", "status", "age", "sex", "w")])
  fit <- survival::survreg(survival::Surv(time, status) ~ age + sex, d,
    weights = w, subset = seq_len(nrow(d)) <= 150, model = TRUE)
  before <- fit
  expect_no_warning(tt <- regtab(fit, stats = "n"))
  counts <- qa_model_sample(tt)
  if (is.null(counts)) return(invisible(NULL))
  expect_equal(counts[c("eligible_n", "fitted_n", "frame_n")],
               c(eligible_n = sum(eligible), fitted_n = sum(eligible), frame_n = sum(eligible)))
  expect_equal(counts[["weight_sum"]], sum(d$w[eligible]))
  expect_equal(counts[["reported_n"]], sum(d$w[eligible]))
  expect_equal(tt$stored$n_1, sum(d$w[eligible]))
  expect_equal(tt_vcov(fit, "model"), stats::vcov(fit), tolerance = 1e-12)
  expect_true(is.na(counts[["input_n"]]))
  expect_identical(fit, before)
})

test_that("installed survey accounting retains original design weights instead of rescaled priors", {
  skip_if_not_installed("survey")
  d <- data.frame(y = seq_len(24) + sin(seq_len(24)), x = seq_len(24),
                  w = rep(c(1, 2, 3), 8), include = seq_len(24) <= 20)
  d$x[c(3, 7)] <- NA_real_
  d$y[12] <- NA_real_
  eligible <- d$include & stats::complete.cases(d)
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  fit <- survey::svyglm(y ~ x, subset(design, include), na.action = na.exclude)
  before <- fit
  expect_no_warning(tt <- regtab(fit, stats = "n"))
  counts <- qa_model_sample(tt)
  if (is.null(counts)) return(invisible(NULL))
  expect_equal(counts[c("eligible_n", "fitted_n", "frame_n")],
               c(eligible_n = sum(eligible), fitted_n = sum(eligible), frame_n = sum(eligible)))
  expect_true(is.na(counts[["input_n"]]))
  expect_equal(counts[["weight_sum"]], sum(d$w[eligible]))
  expect_false(isTRUE(all.equal(counts[["weight_sum"]], sum(fit$prior.weights))))
  expect_equal(counts[["effective_n"]], sum(d$w[eligible])^2 / sum(d$w[eligible]^2))
  expect_equal(counts[["reported_n"]], stats::nobs(fit))
  expect_identical(tt$meta$sample_accounting$populations$weight_type, "survey")
  expect_equal(tt_vcov(fit, "model"), stats::vcov(fit), tolerance = 1e-12)
  expect_identical(fit, before)
})

test_that("installed prepared GEE and WeightIt fits scope counts to retained preparation", {
  skip_if_not_installed("geepack")
  skip_if_not_installed("WeightIt")
  d <- withr::with_seed(80, data.frame(x = rnorm(80), z = rnorm(80), treat = rep(0:1, 40),
                                      id = rep(1:20, each = 4), include = seq_len(80) %% 5 != 0))
  d$y <- 1 + 0.7 * d$treat + 0.4 * d$x + sin(seq_len(80))
  d$y[c(3, 13)] <- NA_real_
  observed <- d[d$include & stats::complete.cases(d), ]
  gee <- geepack::geeglm(y ~ x, id = id, data = observed, corstr = "independence")
  wt <- WeightIt::weightit(treat ~ x + z, data = observed, method = "glm")
  weighted <- WeightIt::glm_weightit(y ~ treat + x, data = observed, weightit = wt, vcov = "HC0")
  for (fit in list(gee, weighted)) {
    before <- fit
    expect_no_warning(tt <- regtab(fit, stats = "n"))
    counts <- qa_model_sample(tt)
    if (is.null(counts)) next
    expect_equal(counts[c("eligible_n", "fitted_n", "frame_n")],
      c(eligible_n = nrow(observed), fitted_n = nrow(observed), frame_n = nrow(observed)))
    expect_equal(counts[["input_n"]], if (inherits(fit, "geeglm")) nrow(observed) else NA_real_)
    expect_equal(counts[["weight_sum"]], sum(fit$prior.weights))
    expect_equal(counts[["reported_n"]], stats::nobs(fit))
    expect_equal(tt_vcov(fit, "model"), stats::vcov(fit), tolerance = 1e-12)
    expect_identical(fit, before)
  }
})

test_that("installed nlme stores original input separately from accepted records", {
  skip_if_not_installed("nlme")
  d <- withr::with_seed(189, {
    x <- rnorm(80)
    data.frame(g = factor(rep(1:20, each = 4)), x = x,
               y = 1 + x + rep(rnorm(20), each = 4) + rnorm(80), include = seq_len(80) <= 72)
  })
  d$x[c(3, 11)] <- NA_real_
  d$y[20] <- NA_real_
  eligible <- d$include & stats::complete.cases(d)
  for (store in c(TRUE, FALSE)) {
    fit <- nlme::lme(y ~ x, random = ~1 | g, data = d, subset = include,
                     na.action = na.exclude, keep.data = store)
    before <- fit
    expect_no_warning(tt <- regtab(fit, noreeffects = TRUE, stats = "n"))
    counts <- qa_model_sample(tt)
    if (is.null(counts)) next
    expect_equal(counts[c("eligible_n", "fitted_n")],
                 c(eligible_n = sum(eligible), fitted_n = sum(eligible)))
    expect_equal(counts[["input_n"]], if (store) nrow(d) else NA_real_)
    expect_true(is.na(counts[["frame_n"]]))
    expect_equal(counts[["weight_sum"]], sum(eligible))
    expect_equal(counts[["reported_n"]], stats::nobs(fit))
    expect_identical(fit, before)
  }
  # nlme's weights argument describes residual precision, not case counts.
  d$variance_group <- factor(rep(c("a", "b"), 40))
  fit <- nlme::lme(y ~ x, random = ~1 | g, data = d, subset = include,
                   weights = nlme::varIdent(form = ~1 | variance_group), na.action = na.exclude)
  expect_error(regtab(fit, noreeffects = TRUE, stats = "n"), "weights.*variance function")
})
