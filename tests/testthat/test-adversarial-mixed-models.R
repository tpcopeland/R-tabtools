# Mixed-model diagnostics, 2026-09-30: real failed fits, rather than edited
# model objects. Balanced data give exact fixed-effect recovery controls.
adversarial_mixed_data <- function(boundary = FALSE) {
  d <- expand.grid(x = seq(-2.5, 2.5, by = 1), g = factor(seq_len(10)))
  d$y <- 2 + 1.5 * d$x + rep(c(1, -2, 1, -1, 2, -1), 10) / 5
  if (!boundary) d$y <- d$y + rep(seq(-2, 2, length.out = 10), each = 6)
  d
}

adversarial_mixed_capture <- function(expr) {
  warnings <- messages <- character()
  fit <- withCallingHandlers(expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(fit = fit, warnings = warnings, messages = messages)
}

test_that("real lmer optimizer exhaustion is refused through every inference entry", {
  skip_if_not_installed("lme4")
  d <- adversarial_mixed_data()
  bad <- adversarial_mixed_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1))))
  fit <- bad$fit
  expect_identical(as.integer(fit@optinfo$conv$opt), 1L)
  expect_gte(length(bad$warnings), 2L)
  expect_true(all(grepl("maxfun|convergence code|failed to converge", bad$warnings)))
  expect_error(regtab(fit), "Model 1.*did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(lm(y ~ x, d), fit), "Model 2.*did not converge", class = "tabtools_error_model_convergence")
  for (vce in c("stata", "model")) {
    expect_error(tt_vcov(fit, vce), "optimizer code 1", class = "tabtools_error_model_convergence")
  }
  expect_error(tt_vcov(fit, complete = FALSE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(fit, full = TRUE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(fit, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(fit, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
})

test_that("real glmer optimizer exhaustion cannot masquerade as a successful GLMM", {
  skip_if_not_installed("lme4")
  d <- adversarial_mixed_data()
  d$y <- as.integer(d$x + as.numeric(d$g) / 4 > 1)
  bad <- adversarial_mixed_capture(lme4::glmer(y ~ x + (1 | g), d, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1))))
  expect_identical(as.integer(bad$fit@optinfo$conv$opt), 1L)
  expect_gte(length(bad$warnings), 2L)
  expect_true(all(grepl("maxfun|convergence code|failed to converge", bad$warnings)))
  expect_error(regtab(bad$fit), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$fit), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$fit, "model"), "did not converge", class = "tabtools_error_model_convergence")
})

test_that("an optimizer success code does not make an explicitly singular GLMM Hessian usable", {
  skip_if_not_installed("lme4")
  d <- adversarial_mixed_data()
  d$y <- 0
  bad <- adversarial_mixed_capture(lme4::glmer(y ~ x + (1 | g), d, family = binomial,
    control = lme4::glmerControl(check.response.not.const = "ignore")))
  fit <- bad$fit
  expect_identical(as.integer(fit@optinfo$conv$opt), 0L)
  expect_identical(fit@optinfo$conv$lme4$code, -4L)
  expect_equal(fit@optinfo$derivs$Hessian, matrix(0, 3L, 3L), tolerance = 1e-12)
  expect_identical(length(bad$warnings), 2L)
  expect_true(all(grepl("scaled gradient|Hessian is numerically singular", bad$warnings)))
  expect_error(regtab(fit), "invalid Hessian", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(fit, "model"), "invalid Hessian", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(fit, full = TRUE), "invalid Hessian", class = "tabtools_error_model_hessian")
  expect_error(regtab(fit, vce = diag(2)), "invalid Hessian", class = "tabtools_error_model_hessian")
})

test_that("glmmTMB failures are refused even when fitting-time convergence warnings were disabled", {
  skip_if_not_installed("glmmTMB")
  d <- adversarial_mixed_data()
  bad <- adversarial_mixed_capture(glmmTMB::glmmTMB(y ~ x + (1 | g), d,
    control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 1, eval.max = 1),
                                    conv_check = "skip", eigval_check = FALSE)))
  expect_identical(bad$warnings, character())
  expect_identical(bad$fit$fit$convergence, 1L)
  expect_identical(bad$fit$sdr$pdHess, FALSE)
  expect_error(regtab(bad$fit), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$fit), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$fit, "model"), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$fit, full = TRUE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(bad$fit, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
})

test_that("glmmTMB invalid covariance is an error while rank-adjusted estimated blocks remain usable", {
  skip_if_not_installed("glmmTMB")
  d <- adversarial_mixed_data()
  bad <- adversarial_mixed_capture(glmmTMB::glmmTMB(y ~ x + I(2 * x) + (1 | g), d,
    control = glmmTMB::glmmTMBControl(rank_check = "skip", conv_check = "skip", eigval_check = FALSE)))
  expect_identical(bad$warnings, character())
  expect_identical(bad$fit$fit$convergence, 0L)
  # pdHess can itself be a false positive for a numerically singular
  # information matrix. The native estimated block remains unusable.
  expect_true(any(!is.finite(stats::vcov(bad$fit)$cond)))
  expect_error(regtab(bad$fit), "Hessian|covariance", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(bad$fit), "Hessian|covariance", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(bad$fit, "model"), "Hessian|covariance", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(bad$fit, vce = diag(3)), "Hessian|covariance", class = "tabtools_error_model_hessian")
  good <- adversarial_mixed_capture(glmmTMB::glmmTMB(y ~ x + I(2 * x) + (1 | g), d,
    control = glmmTMB::glmmTMBControl(rank_check = "adjust")))
  expect_identical(good$warnings, character())
  expect_match(good$messages, "dropping columns.*I\\(2 \\* x\\)")
  expect_identical(good$fit$sdr$pdHess, TRUE)
  V <- tt_vcov(good$fit)
  expect_identical(dim(V), c(3L, 3L))
  expect_true(all(is.na(V[3, ])))
  expect_true(all(is.finite(V[1:2, 1:2])))
  table <- adversarial_mixed_capture(regtab(good$fit, noreeffects = TRUE, stats = "n"))
  expect_identical(table$warnings, character())
  tt <- table$fit
  expect_identical(tt$meta$regtab_rows$status, c("est", "omit", "est"))
  expect_equal(tt$meta$regtab_rows$estimate[c(1, 3)], c(1.5, 2), tolerance = 1e-7)
  expect_identical(tt$stored$n_1, 60)
})

test_that("legitimate singular LMMs retain valid fixed effects and boundary random-effect rows", {
  skip_if_not_installed("lme4")
  d <- adversarial_mixed_data(boundary = TRUE)
  good <- adversarial_mixed_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE))
  expect_identical(good$warnings, character())
  expect_match(good$messages, "boundary \\(singular\\) fit")
  expect_identical(as.integer(good$fit@optinfo$conv$opt), 0L)
  expect_identical(lme4::isSingular(good$fit), TRUE)
  # Orthogonal within-group residuals: SSE=4.8, sigma^2_ML=0.08;
  # at zero group variance the fixed covariance is sigma^2 (X'X)^-1.
  hand <- diag(c(0.08 / 60, 0.08 / 175))
  dimnames(hand) <- rep(list(c("(Intercept)", "x")), 2L)
  expect_equal(tt_vcov(good$fit), hand, tolerance = 1e-10)
  expect_no_warning(tt <- regtab(good$fit, stats = c("n", "groups")))
  expect_equal(tt$meta$regtab_rows$estimate[1:2], c(1.5, 2), tolerance = 1e-12)
  re <- tt$meta$regtab_rows[tt$meta$regtab_rows$key == "var(_cons)", ]
  expect_identical(nrow(re), 1L)
  expect_identical(re$estimate, 0)
  expect_true(is.na(re$conf.low))
  expect_true(is.na(re$conf.high))
  expect_identical(c(tt$stored$n_1, tt$stored$groups_1), c(60, 10))
})

test_that("lme4 gradient advisories alone do not invalidate an accurately recovered optimum", {
  skip_if_not_installed("lme4")
  d <- adversarial_mixed_data()
  good <- adversarial_mixed_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE,
    control = lme4::lmerControl(check.conv.grad = lme4::.makeCC("warning", tol = 1e-15))))
  expect_identical(length(good$warnings), 1L)
  expect_match(good$warnings, "failed to converge with max\\|grad\\|")
  expect_identical(as.integer(good$fit@optinfo$conv$opt), 0L)
  expect_identical(good$fit@optinfo$conv$lme4$code, -1L)
  expect_no_warning(tt <- regtab(good$fit, noreeffects = TRUE))
  expect_equal(tt$meta$regtab_rows$estimate, c(1.5, 2), tolerance = 1e-12)
  expect_identical(dim(tt_vcov(good$fit)), c(2L, 2L))
  expect_true(all(is.finite(tt_vcov(good$fit))))
})

test_that("glmmTMB fixed and shared coefficient maps preserve legitimate constrained inference", {
  skip_if_not_installed("glmmTMB")
  d <- adversarial_mixed_data()
  fixed <- glmmTMB::glmmTMB(y ~ x + (1 | g), d,
    map = list(beta = factor(c(1, NA))), start = list(beta = c(2, 1.5)))
  expect_identical(fixed$fit$convergence, 0L)
  expect_identical(fixed$sdr$pdHess, TRUE)
  V <- tt_vcov(fixed)
  expect_identical(dim(V), c(2L, 2L))
  expect_true(is.finite(V[1, 1]))
  expect_true(all(is.na(V[2, ])))
  expect_no_warning(tt <- regtab(fixed, noreeffects = TRUE))
  expect_equal(tt$meta$regtab_rows$estimate, c(1.5, 2), tolerance = 1e-7)
  expect_true(is.na(tt$meta$regtab_rows$conf.low[1]))
  expect_identical(tt$body[[3]][1], "")
  shared <- glmmTMB::glmmTMB(y ~ x + (1 | g), d, map = list(beta = factor(c(1, 1))))
  expect_identical(shared$fit$convergence, 0L)
  expect_identical(shared$sdr$pdHess, TRUE)
  Vs <- tt_vcov(shared)
  expect_equal(Vs[1, ], Vs[2, ], tolerance = 1e-12)
  expect_gt(Vs[1, 1], 0)
  expect_no_warning(ts <- regtab(shared, noreeffects = TRUE))
  expect_equal(ts$meta$regtab_rows$estimate[1], ts$meta$regtab_rows$estimate[2], tolerance = 1e-12)
  expect_equal(ts$meta$regtab_rows$conf.low[1], ts$meta$regtab_rows$conf.low[2], tolerance = 1e-12)
})

test_that("glmmTMB rank-adjusted maps validate the independent retained parameters", {
  skip_if_not_installed("glmmTMB")
  withr::local_seed(22)
  n <- 120L
  g <- rep(seq_len(20), each = 6)
  x <- rnorm(n)
  z <- rnorm(n)
  d <- data.frame(g = factor(g), x = x, z = z,
    y = 1 + 0.3 * x + z + rep(rnorm(20), each = 6) + rnorm(n))
  fitted <- adversarial_mixed_capture(glmmTMB::glmmTMB(y ~ x + z + I(2 * z) + (1 | g), d,
    map = list(beta = factor(c(1, 2, 1))), start = list(beta = c(1, 0.3, 1)),
    control = glmmTMB::glmmTMBControl(rank_check = "adjust")))
  fit <- fitted$fit
  reduced <- glmmTMB::glmmTMB(y ~ x + z + (1 | g), d,
    map = list(beta = factor(c(1, 2, 1))), start = list(beta = c(1, 0.3, 1)))
  expect_identical(fitted$warnings, character())
  expect_match(fitted$messages, "dropping columns.*I\\(2 \\* z\\)")
  expect_identical(fit$fit$convergence, 0L)
  expect_identical(fit$sdr$pdHess, TRUE)
  expect_identical(length(fit$obj$env$map$beta), 3L)
  expect_identical(length(glmmTMB::fixef(fit)$cond), 4L)
  variance <- adversarial_mixed_capture(tt_vcov(fit))
  expect_gte(length(variance$warnings), 1L)
  expect_true(all(grepl("combination of mapping and columns dropped", variance$warnings)))
  V <- variance$fit
  retained <- c("(Intercept)", "x", "z")
  expect_equal(V[retained, retained], tt_vcov(reduced), tolerance = 1e-10)
  expect_true(all(is.na(V["I(2 * z)", ])))
  table <- adversarial_mixed_capture(regtab(fit, noreeffects = TRUE))
  expect_gte(length(table$warnings), 1L)
  expect_true(all(grepl("combination of mapping and columns dropped", table$warnings)))
  rows <- table$fit$meta$regtab_rows
  expect_identical(rows$key, c("x", "z", "I(2 * z)", "_cons"))
  expect_identical(rows$status, c("est", "est", "omit", "est"))
  expect_equal(rows$estimate[c(1, 2, 4)],
               unname(glmmTMB::fixef(reduced)$cond[c("x", "z", "(Intercept)")]), tolerance = 1e-10)
  expect_equal(rows$conf.low[2], rows$conf.low[4], tolerance = 1e-12)
})

test_that("stored glmmTMB metadata preserves interaction indices and the caller's fit", {
  skip_if_not_installed("glmmTMB")
  d <- expand.grid(x = seq(-2.5, 2.5, by = 1), f = factor(c("a", "b", "c")),
                   g = factor(seq_len(10)))
  d$y <- 2 + 1.5 * d$x + c(0, 1, -0.5)[as.integer(d$f)] +
    d$x * c(0, 0.25, -0.5)[as.integer(d$f)] +
    rep(seq(-2, 2, length.out = 10), each = 18) + rep(c(1, -2, 1, -1, 2, -1), 30) / 5
  attr(d$x, "label") <- "Exposure X"
  fit <- glmmTMB::glmmTMB(y ~ x * f + (1 | g), d)
  before_frame <- fit$frame
  before_form <- fit$modelInfo$allForm
  expect_identical(fit$fit$convergence, 0L)
  expect_identical(fit$sdr$pdHess, TRUE)
  expect_no_warning(tt <- regtab(fit, noreeffects = TRUE, stats = c("n", "groups")))
  expect_identical(tt$rows$key,
    c("x", "2.f", "3.f", "2.f#c.x", "3.f#c.x", "_cons", "stat:n", "stat:groups"))
  expect_equal(tt$meta$regtab_rows$estimate, c(1.5, 1, -0.5, 0.25, -0.5, 2), tolerance = 2e-6)
  expect_identical(tt$body[[1]][c(1, 4, 5)],
                   c("Exposure X", "b × Exposure X", "c × Exposure X"))
  expect_identical(c(tt$stored$n_1, tt$stored$groups_1), c(180, 10))
  expect_identical(fit$frame, before_frame)
  expect_identical(fit$modelInfo$allForm, before_form)
  expect_null(attr(fit, "model_matrix", exact = TRUE))
  expect_null(attr(fit, "model_frame", exact = TRUE))
})
