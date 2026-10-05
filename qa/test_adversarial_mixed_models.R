library(testthat)
library(tabtools)

mixed_qa_data <- function(boundary = FALSE) {
  d <- expand.grid(x = seq(-2.5, 2.5, by = 1), g = factor(seq_len(10)))
  d$y <- 2 + 1.5 * d$x + rep(c(1, -2, 1, -1, 2, -1), 10) / 5
  if (!boundary) d$y <- d$y + rep(seq(-2, 2, length.out = 10), each = 6)
  rownames(d) <- paste0("mixed-subject-", seq_len(nrow(d)))
  d
}

mixed_qa_capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(value = value, warnings = warnings, messages = messages)
}

test_that("installed lme4 failures survive serialization and cannot bypass diagnostics with user variance", {
  skip_if_not_installed("lme4")
  d <- mixed_qa_data()
  bad <- mixed_qa_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1))))
  expect_gte(length(bad$warnings), 2L)
  expect_true(all(grepl("maxfun|convergence code|failed to converge", bad$warnings)))
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(bad$value, file)
  fit <- readRDS(file)
  expect_identical(as.integer(fit@optinfo$conv$opt), 1L)
  expect_error(regtab(fit), "Model 1.*did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(fit, "model"), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(fit, full = TRUE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(fit, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(fit, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
  uv <- mixed_qa_capture(tryCatch(regtab_uv(d, "y", "x", method = lme4::lmer,
    formula = "{y} ~ {x} + (1 | g)",
    method.args = list(REML = FALSE,
                      control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1)))),
    error = identity))
  expect_true(all(grepl("maxfun|convergence code|failed to converge", uv$warnings)))
  expect_s3_class(uv$value, "rlang_error")
  expect_match(conditionMessage(uv$value), "model for.*x.*cannot be tabled")
  expect_s3_class(uv$value$parent, "tabtools_error_model_convergence")
})

test_that("installed glmmTMB checks optimizer diagnostics when fitting-time warnings are disabled", {
  skip_if_not_installed("glmmTMB")
  d <- mixed_qa_data()
  bad <- mixed_qa_capture(glmmTMB::glmmTMB(y ~ x + (1 | g), d,
    control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 1, eval.max = 1),
                                    conv_check = "skip", eigval_check = FALSE)))
  expect_identical(bad$warnings, character())
  expect_identical(bad$value$fit$convergence, 1L)
  expect_identical(bad$value$sdr$pdHess, FALSE)
  expect_error(regtab(lm(y ~ x, d), bad$value), "Model 2.*did not converge",
               class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$value, complete = FALSE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(tt_vcov(bad$value, full = TRUE), "did not converge", class = "tabtools_error_model_convergence")
  expect_error(regtab(bad$value, vce = diag(2)), "did not converge", class = "tabtools_error_model_convergence")
})

test_that("installed fits with success codes still refuse explicitly invalid Hessians", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- mixed_qa_data()
  constant <- d
  constant$y <- 0
  mer <- mixed_qa_capture(lme4::glmer(y ~ x + (1 | g), constant, family = binomial,
    control = lme4::glmerControl(check.response.not.const = "ignore")))
  expect_identical(as.integer(mer$value@optinfo$conv$opt), 0L)
  expect_identical(mer$value@optinfo$conv$lme4$code, -4L)
  expect_identical(length(mer$warnings), 2L)
  expect_true(all(grepl("scaled gradient|Hessian is numerically singular", mer$warnings)))
  expect_error(regtab(mer$value), "invalid Hessian", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(mer$value, "model"), "invalid Hessian", class = "tabtools_error_model_hessian")
  tmb <- mixed_qa_capture(glmmTMB::glmmTMB(y ~ x + I(2 * x) + (1 | g), d,
    control = glmmTMB::glmmTMBControl(rank_check = "skip", conv_check = "skip", eigval_check = FALSE)))
  expect_identical(tmb$warnings, character())
  expect_identical(tmb$value$fit$convergence, 0L)
  expect_true(any(!is.finite(stats::vcov(tmb$value)$cond)))
  expect_error(regtab(tmb$value), "Hessian|covariance", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(tmb$value, "model"), "Hessian|covariance", class = "tabtools_error_model_hessian")
  expect_error(tt_vcov(tmb$value, vce = diag(3)), "Hessian|covariance", class = "tabtools_error_model_hessian")
})

test_that("installed boundary mixed fits recover the known slope and retain appropriate random-effect rows", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- mixed_qa_data(boundary = TRUE)
  mer <- mixed_qa_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE))
  expect_identical(mer$warnings, character())
  expect_identical(lme4::isSingular(mer$value), TRUE)
  expect_match(mer$messages, "boundary \\(singular\\) fit")
  tmb <- glmmTMB::glmmTMB(y ~ x + (1 | g), d)
  expect_identical(tmb$fit$convergence, 0L)
  expect_identical(tmb$sdr$pdHess, TRUE)
  expect_lt(unname(glmmTMB::VarCorr(tmb)$cond$g[1, 1]), 1e-6)
  hand <- diag(c(0.08 / 60, 0.08 / 175))
  dimnames(hand) <- rep(list(c("(Intercept)", "x")), 2L)
  expect_equal(tt_vcov(mer$value), hand, tolerance = 1e-10)
  expect_lt(max(abs(tt_vcov(tmb) - hand)), 1e-7)
  for (fit in list(mer$value, tmb)) {
    result <- mixed_qa_capture(regtab(fit, stats = c("n", "groups")))
    expect_identical(result$warnings, character())
    tt <- result$value
    expect_identical(tt$rows$key, c("x", "_cons", "var(_cons)", "var(e)", "stat:n", "stat:groups"))
    expect_equal(tt$meta$regtab_rows$estimate[1:2], c(1.5, 2), tolerance = 1e-7)
    expect_identical(c(tt$stored$n_1, tt$stored$groups_1), c(60, 10))
    expect_lt(tt$meta$regtab_rows$estimate[3], 1e-6)
  }
})

test_that("installed mixed-model missingness matches independent GLS on the actual observed sample", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- mixed_qa_data()
  d$h <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  d$y[d$h == "c"] <- NA_real_
  d$x[5] <- NA_real_
  d$g[13] <- NA
  attr(d$x, "label") <- "Exposure X"
  attr(d$h, "label") <- "Risk class"
  before <- d
  observed <- droplevels(d[complete.cases(d[c("y", "x", "h", "g")]), ])
  expect_identical(nrow(observed), 38L)
  fits <- list(lme4::lmer(y ~ x + h + (1 | g), d, REML = FALSE, na.action = stats::na.exclude),
               glmmTMB::glmmTMB(y ~ x + h + (1 | g), d, na.action = stats::na.exclude))
  X <- cbind(`(Intercept)` = 1, x = observed$x, hb = as.numeric(observed$h == "b"))
  for (fit in fits) {
    group_var <- if (inherits(fit, "merMod")) unname(lme4::VarCorr(fit)$g[1, 1]) else
      unname(glmmTMB::VarCorr(fit)$cond$g[1, 1])
    # Direct marginal covariance, independent of either package's vcov:
    # Cov(y_i,y_j) = tau^2 I(g_i=g_j) + sigma^2 I(i=j).
    marginal <- group_var * outer(as.character(observed$g), as.character(observed$g), `==`) +
      diag(stats::sigma(fit)^2, nrow(observed))
    precision_X <- solve(marginal, X)
    hand_V <- solve(crossprod(X, precision_X))
    hand_b <- drop(hand_V %*% crossprod(X, solve(marginal, observed$y)))
    if (inherits(fit, "glmmTMB")) {
      # glmmTMB reports joint observed information over beta and the
      # variance parameters. On an unbalanced sample its beta block is
      # different from the conditional GLS covariance used by lmer.
      W <- solve(marginal)
      wr <- drop(W %*% (observed$y - drop(X %*% hand_b)))
      derivatives <- list(outer(as.character(observed$g), as.character(observed$g), `==`),
                          diag(nrow(observed)))
      H <- matrix(0, ncol(X) + 2L, ncol(X) + 2L)
      H[seq_len(ncol(X)), seq_len(ncol(X))] <- crossprod(X, precision_X)
      for (a in seq_len(2)) {
        index <- ncol(X) + a
        H[seq_len(ncol(X)), index] <- drop(crossprod(X, W %*% derivatives[[a]] %*% wr))
        H[index, seq_len(ncol(X))] <- H[seq_len(ncol(X)), index]
        for (b in seq_len(2)) {
          H[index, ncol(X) + b] <-
            -sum(diag(W %*% derivatives[[a]] %*% W %*% derivatives[[b]])) / 2 +
            drop(crossprod(wr, derivatives[[a]] %*% W %*% derivatives[[b]] %*% wr))
        }
      }
      hand_V <- solve(H)[seq_len(ncol(X)), seq_len(ncol(X)), drop = FALSE]
      dimnames(hand_V) <- rep(list(colnames(X)), 2L)
    }
    expect_equal(tt_vcov(fit), hand_V, tolerance = 1e-8)
    expect_null(attr(fit, "model_matrix", exact = TRUE))
    expect_null(attr(fit, "model_frame", exact = TRUE))
    table <- mixed_qa_capture(regtab(fit, noreeffects = TRUE, stats = c("n", "groups")))
    expect_identical(table$warnings, character())
    tt <- table$value
    expect_identical(tt$rows$key, c("x", "h", "1.h", "2.h", "_cons", "stat:n", "stat:groups"))
    expect_identical(tt$body[[1]][1:2], c("Exposure X", "Risk class"))
    expect_identical(tt$body[[2]][3], "Reference")
    rows <- tt$meta$regtab_rows
    # The default optimizer returns beta within 2e-7 of the conditional
    # normal equations here; covariance compares analytic information.
    expect_lt(max(abs(rows$estimate[rows$status == "est"] -
                      unname(hand_b[c("x", "hb", "(Intercept)")]))), 2e-7)
    expect_identical(c(tt$stored$n_1, tt$stored$groups_1), c(38, 10))
    expect_identical(stats::nobs(fit), 38L)
    expect_null(attr(fit, "model_matrix", exact = TRUE))
    expect_null(attr(fit, "model_frame", exact = TRUE))
  }
  expect_identical(d, before)
})

test_that("installed gradient advisories and absent mixed covariates retain their distinct contracts", {
  skip_if_not_installed("lme4")
  d <- mixed_qa_data()
  advisory <- mixed_qa_capture(lme4::lmer(y ~ x + (1 | g), d, REML = FALSE,
    control = lme4::lmerControl(check.conv.grad = lme4::.makeCC("warning", tol = 1e-15))))
  expect_identical(as.integer(advisory$value@optinfo$conv$opt), 0L)
  expect_identical(advisory$value@optinfo$conv$lme4$code, -1L)
  expect_identical(length(advisory$warnings), 1L)
  expect_match(advisory$warnings, "failed to converge with max\\|grad\\|")
  expect_no_warning(tt <- regtab(advisory$value, noreeffects = TRUE))
  expect_equal(tt$meta$regtab_rows$estimate, c(1.5, 2), tolerance = 1e-12)
  expect_error(regtab_uv(d, "y", "absent", method = lme4::lmer,
                         formula = "{y} ~ {x} + (1 | g)"), "absent.*not.*column", class = "rlang_error")
  d$x <- NA_real_
  expect_error(regtab_uv(d, "y", "x", method = lme4::lmer,
                         formula = "{y} ~ {x} + (1 | g)"), "model for.*x.*failed", class = "rlang_error")
})

test_that("installed glmmTMB constraints retain fixed and shared coefficient semantics", {
  skip_if_not_installed("glmmTMB")
  d <- mixed_qa_data()
  fixed <- glmmTMB::glmmTMB(y ~ x + (1 | g), d,
    map = list(beta = factor(c(1, NA))), start = list(beta = c(2, 1.5)))
  expect_identical(fixed$fit$convergence, 0L)
  expect_identical(fixed$sdr$pdHess, TRUE)
  expect_identical(glmmTMB::fixef(fixed)$cond[["x"]], 1.5)
  expect_true(all(is.na(tt_vcov(fixed)["x", ])))
  expect_no_warning(tt <- regtab(fixed, noreeffects = TRUE))
  expect_equal(tt$meta$regtab_rows$estimate, c(1.5, 2), tolerance = 1e-7)
  expect_identical(tt$body[[3]][1], "")
  expect_true(is.finite(tt$meta$regtab_rows$conf.low[2]))
  shared <- glmmTMB::glmmTMB(y ~ x + (1 | g), d, map = list(beta = factor(c(1, 1))))
  expect_identical(shared$fit$convergence, 0L)
  expect_identical(shared$sdr$pdHess, TRUE)
  V <- tt_vcov(shared)
  expect_equal(V[1, ], V[2, ], tolerance = 1e-12)
  expect_gt(V[1, 1], 0)
  expect_no_warning(ts <- regtab(shared, noreeffects = TRUE))
  expect_equal(ts$meta$regtab_rows$estimate[1], ts$meta$regtab_rows$estimate[2], tolerance = 1e-12)
})

test_that("installed rank-adjusted shared maps agree with the equivalent reduced model", {
  skip_if_not_installed("glmmTMB")
  withr::local_seed(22)
  n <- 120L
  g <- rep(seq_len(20), each = 6)
  x <- rnorm(n)
  z <- rnorm(n)
  d <- data.frame(g = factor(g), x = x, z = z,
    y = 1 + 0.3 * x + z + rep(rnorm(20), each = 6) + rnorm(n))
  fitted <- mixed_qa_capture(glmmTMB::glmmTMB(y ~ x + z + I(2 * z) + (1 | g), d,
    map = list(beta = factor(c(1, 2, 1))), start = list(beta = c(1, 0.3, 1)),
    control = glmmTMB::glmmTMBControl(rank_check = "adjust")))
  fit <- fitted$value
  reduced <- glmmTMB::glmmTMB(y ~ x + z + (1 | g), d,
    map = list(beta = factor(c(1, 2, 1))), start = list(beta = c(1, 0.3, 1)))
  expect_identical(fitted$warnings, character())
  expect_match(fitted$messages, "dropping columns.*I\\(2 \\* z\\)")
  expect_identical(fit$fit$convergence, 0L)
  expect_identical(fit$sdr$pdHess, TRUE)
  variance <- mixed_qa_capture(tt_vcov(fit))
  expect_gte(length(variance$warnings), 1L)
  expect_true(all(grepl("combination of mapping and columns dropped", variance$warnings)))
  retained <- c("(Intercept)", "x", "z")
  expect_equal(variance$value[retained, retained], tt_vcov(reduced), tolerance = 1e-10)
  expect_true(all(is.na(variance$value["I(2 * z)", ])))
  table <- mixed_qa_capture(regtab(fit, noreeffects = TRUE, stats = c("n", "groups")))
  expect_gte(length(table$warnings), 1L)
  expect_true(all(grepl("combination of mapping and columns dropped", table$warnings)))
  rows <- table$value$meta$regtab_rows
  expect_identical(rows$key, c("x", "z", "I(2 * z)", "_cons"))
  expect_identical(rows$status, c("est", "est", "omit", "est"))
  expect_equal(rows$estimate[c(1, 2, 4)],
    unname(glmmTMB::fixef(reduced)$cond[c("x", "z", "(Intercept)")]), tolerance = 1e-10)
  expect_equal(rows$conf.low[2], rows$conf.low[4], tolerance = 1e-12)
  expect_identical(c(table$value$stored$n_1, table$value$stored$groups_1), c(120, 20))
})

test_that("installed glmmTMB stored designs preserve interactions, aliases and model inputs", {
  skip_if_not_installed("glmmTMB")
  d <- expand.grid(x = seq(-2.5, 2.5, by = 1), f = factor(c("a", "b", "c")),
                   g = factor(seq_len(10)))
  d$z <- as.character(d$f)
  d$y <- 2 + 1.5 * d$x + c(0, 1, -0.5)[as.integer(d$f)] +
    d$x * c(0, 0.25, -0.5)[as.integer(d$f)] +
    rep(seq(-2, 2, length.out = 10), each = 18) + rep(c(1, -2, 1, -1, 2, -1), 30) / 5
  attr(d$x, "label") <- "Exposure X"
  before_data <- d
  formulas <- list(y ~ x * f + (1 | g), y ~ x * factor(z) + (1 | g),
                   y ~ x * f + I(2 * x) + (1 | g))
  for (i in seq_along(formulas)) {
    fitted <- mixed_qa_capture(glmmTMB::glmmTMB(formulas[[i]], d,
      control = glmmTMB::glmmTMBControl(rank_check = "adjust")))
    fit <- fitted$value
    before_frame <- fit$frame
    before_form <- fit$modelInfo$allForm
    expect_identical(fitted$warnings, character())
    expect_identical(fit$fit$convergence, 0L)
    expect_identical(fit$sdr$pdHess, TRUE)
    table <- mixed_qa_capture(regtab(fit, noreeffects = TRUE, stats = c("n", "groups")))
    expect_identical(table$warnings, character())
    tt <- table$value
    factor_key <- if (i == 2) "factor(z)" else "f"
    keys <- c("x", paste0(c("2.", "3."), factor_key),
              paste0(c("2.", "3."), factor_key, "#c.x"))
    if (i == 3) keys <- c(keys, "I(2 * x)")
    expect_identical(tt$rows$key, c(keys, "_cons", "stat:n", "stat:groups"))
    rows <- tt$meta$regtab_rows
    expect_equal(rows$estimate[rows$status == "est"],
                 c(1.5, 1, -0.5, 0.25, -0.5, 2), tolerance = 2e-6)
    b <- glmmTMB::fixef(fit)$cond
    covariance <- tt_vcov(fit)
    order <- c(2:4, if (i == 3) 6:7 else 5:6, 1)
    expected_low <- b[order] - qnorm(0.975) * sqrt(diag(covariance)[order])
    expect_equal(rows$conf.low[rows$status == "est"], unname(expected_low), tolerance = 1e-12)
    expect_identical(tt$body[[1]][c(1, 4, 5)],
      c("Exposure X", "b × Exposure X", "c × Exposure X"))
    if (i == 3) {
      expect_identical(rows$status[6], "omit")
      expect_true(is.na(rows$estimate[6]))
    }
    expect_identical(c(tt$stored$n_1, tt$stored$groups_1), c(180, 10))
    expect_identical(fit$frame, before_frame)
    expect_identical(fit$modelInfo$allForm, before_form)
    expect_null(attr(fit, "model_matrix", exact = TRUE))
    expect_null(attr(fit, "model_frame", exact = TRUE))
  }
  expect_identical(d, before_data)
})
