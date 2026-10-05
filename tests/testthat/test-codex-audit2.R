# Regression guards for the second Codex audit (codexaudit2.md, snapshot
# 35c9a12): F01 (a survreg fit's response edited after fitting passed the
# stale-data check), F05 (multiply imputed fits on different rows of the
# same size were pooled) and F06 (imputations whose coefficients share names
# but not contrasts were pooled). Every guard asserts the refusal and the
# good path.

# F01 ------------------------------------------------------------------------

ca2_surv_data <- function() {
  set.seed(83)
  data.frame(time = rexp(100), status = rbinom(100, 1, 0.75), x = rnorm(100))
}

ca2_S <- c("ll", "aic", "bic", "events")

test_that("F01: editing only a survreg fit's times or statuses is refused, whatever it stores", {
  skip_if_not_installed("survival")
  for (yy in c(TRUE, FALSE)) {
    for (edit in c("time", "status")) {
      d <- ca2_surv_data()
      fit <- survival::survreg(survival::Surv(time, status) ~ x, d, dist = "weibull", y = yy)
      want <- regtab(fit, stats = ca2_S)$body
      # Good path: the unchanged data give the same table (LL -131.09, 67
      # events in the audit's reproducer).
      expect_identical(regtab(fit, stats = ca2_S)$body, want)
      if (edit == "time") d$time <- d$time * 100 else d$status <- 1L
      expect_error(regtab(fit, stats = ca2_S), "have changed since", info = paste("y =", yy, edit))
    }
  }
  # The fit's own numbers, for the record.
  d <- ca2_surv_data()
  fit <- survival::survreg(survival::Surv(time, status) ~ x, d, dist = "weibull", y = FALSE)
  s <- tabtools:::tt_model_stats(tabtools:::.rt_anchor(fit), tabtools:::tt_model_info(fit))
  expect_equal(s$events, 67)
  expect_equal(s$ll, -131.0931, tolerance = 1e-6)
})

test_that("F01: the survreg log-likelihood check reproduces the fit's across distributions and responses", {
  skip_if_not_installed("survival")
  set.seed(1)
  d <- data.frame(time = rexp(200), status = rbinom(200, 1, 0.7), x = rnorm(200),
                  g = factor(sample(1:2, 200, TRUE)), w = runif(200, 0.5, 2))
  d$lo <- ifelse(d$status == 1, d$time, d$time * 0.5)
  d$hi <- ifelse(d$status == 1, d$time, d$time * 1.5)
  d$lo[1:10] <- NA
  d$hi[11:20] <- NA
  d$lo[21:25] <- 0   # survreg recodes (0, hi] as left-censored at hi
  d$yc <- pmax(d$x + stats::rnorm(200), 0)
  S <- survival::Surv
  fits <- list(
    survival::survreg(S(time, status) ~ x, d, y = FALSE),
    survival::survreg(S(time, status) ~ x, d, dist = "exponential"),
    survival::survreg(S(time, status) ~ x, d, dist = "lognormal", y = FALSE),
    survival::survreg(S(time, status) ~ x, d, dist = "loglogistic"),
    survival::survreg(S(time, status) ~ x, d, weights = w, y = FALSE),
    survival::survreg(S(time, status) ~ x + strata(g), d, y = FALSE),
    survival::survreg(S(lo, hi, type = "interval2") ~ x, d, y = FALSE),
    survival::survreg(S(lo, hi, type = "interval2") ~ x, d),
    survival::survreg(S(yc, yc > 0, type = "left") ~ x, d, dist = "gaussian", y = FALSE)
  )
  for (k in seq_along(fits)) {
    a <- tabtools:::.rt_anchor(fits[[k]])
    expect_false(isTRUE(a$tt_stale), info = k)
    expect_true(tabtools:::.rt_survreg_ll_matches(fits[[k]], a$model, fits[[k]]$linear.predictors), info = k)
  }
})

test_that("F01: re-sorted data without their row names: used unchanged, refused once the response is edited", {
  skip_if_not_installed("survival")
  for (yy in c(TRUE, FALSE)) {
    d <- ca2_surv_data()
    fits <- list(sr = survival::survreg(survival::Surv(time, status) ~ x, d, y = yy),
                 cox = survival::coxph(survival::Surv(time, status) ~ x, d, y = yy))
    want <- lapply(fits, function(f) regtab(f, stats = ca2_S)$body)
    d <- d[order(d$x), ]
    rownames(d) <- NULL
    for (nm in names(fits)) {
      expect_identical(regtab(fits[[nm]], stats = ca2_S)$body, want[[nm]], label = paste(nm, yy))
    }
    d$status <- 1L
    for (nm in names(fits)) {
      expect_error(regtab(fits[[nm]], stats = ca2_S), "have changed since", info = paste(nm, "y =", yy))
    }
  }
})

# F01 review: weights, ties = "exact", non-converged survreg ---------------

ca2_w_data <- function() {
  set.seed(1)
  d <- data.frame(x = rnorm(100), w = sample(1:3, 100, TRUE), w1 = 1)
  d$time <- rexp(100, exp(0.3 * d$x))
  d$status <- rbinom(100, 1, 0.7)
  d$y <- 1 + d$x + rnorm(100)
  d
}

ca2_w_stats <- function(fit, S = c("N", "ll", "events")) {
  suppressWarnings(suppressMessages(regtab(fit, stats = S)))$body
}

test_that("F01 review: a weight edit after fitting is refused, in place and re-sorted, in every class", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  # Fitted in this frame, so that the edits below reach the fits' data.
  fits <- alist(
    survreg = survival::survreg(S(time, status) ~ x, d, weights = w),
    survreg_y0 = survival::survreg(S(time, status) ~ x, d, weights = w, y = FALSE),
    survreg_w1 = survival::survreg(S(time, status) ~ x, d, weights = w1),
    coxph = survival::coxph(S(time, status) ~ x, d, weights = w),
    coxph_y0 = survival::coxph(S(time, status) ~ x, d, weights = w, y = FALSE),
    coxph_w1 = survival::coxph(S(time, status) ~ x, d, weights = w1),
    lm = lm(y ~ x, d, weights = w, model = FALSE)
  )
  for (nm in names(fits)) {
    wv <- if (grepl("w1", nm)) "w1" else "w"
    d <- ca2_w_data()
    fit <- eval(fits[[nm]])
    # Good path: the data re-sorted with their row names reset (a weighted
    # coxph(y = FALSE) included: it used to lose its Events row, backlog).
    want <- ca2_w_stats(fit)
    if (nm != "lm") expect_true("Events" %in% want[[1]], label = nm)
    d <- d[sample(100), ]
    rownames(d) <- NULL
    expect_identical(ca2_w_stats(fit), want, label = paste(nm, "re-sorted"))
    # The weights multiplied in place, and then re-sorted: the survreg
    # reproducer gave Subjects 206 -> 1030 and LL -256.51 -> -529.38.
    d <- ca2_w_data()
    d[[wv]] <- d[[wv]] * 5
    expect_error(ca2_w_stats(fit), "have changed since", info = paste(nm, "in place"))
    d <- d[sample(100), ]
    rownames(d) <- NULL
    expect_error(ca2_w_stats(fit), "have changed since", info = paste(nm, "re-sorted"))
    # Weights permuted among the rows (the multiset of weights unchanged).
    d <- ca2_w_data()
    d[[wv]] <- rev(d[[wv]])
    if (wv == "w") expect_error(ca2_w_stats(fit), "have changed since", info = paste(nm, "permuted"))
  }
})

test_that("F01 review: re-sorted data whose strata were edited are refused, whatever the fit stores", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  for (yy in c(TRUE, FALSE)) {
    set.seed(6)
    d <- data.frame(x = rnorm(200), g = factor(sample(1:2, 200, TRUE)))
    d$time <- rexp(200, exp(0.3 * d$x))
    d$status <- rbinom(200, 1, 0.7)
    fits <- list(cox = survival::coxph(S(time, status) ~ x + strata(g), d, y = yy),
                 sr = survival::survreg(S(time, status) ~ x + strata(g), d, y = yy))
    want <- lapply(fits, function(f) regtab(f, stats = ca2_S)$body)
    d <- d[sample(200), ]
    rownames(d) <- NULL
    for (nm in names(fits)) expect_identical(regtab(fits[[nm]], stats = ca2_S)$body, want[[nm]])
    # The strata re-drawn: responses, linear predictors and weights keep
    # their multiset; the (partial) log-likelihood does not.
    d$g <- factor(sample(1:2, 200, TRUE))
    for (nm in names(fits)) {
      expect_error(regtab(fits[[nm]], stats = ca2_S), "have changed since", info = paste(nm, "y =", yy))
    }
  }
})

test_that("F01 review: a binomial glm's weights are compared too (as its initialize() forms them)", {
  set.seed(2)
  d <- data.frame(x = rnorm(60), k = rbinom(60, 4, 0.4), w = sample(1:3, 60, TRUE))
  fit <- glm(cbind(k, 4 - k) ~ x, binomial, d, weights = w)
  mf <- stats::model.frame(fit)
  expect_true(tabtools:::.rt_frame_matches_fit(fit, mf))
  mf[["(weights)"]] <- mf[["(weights)"]] * 2
  expect_false(tabtools:::.rt_frame_matches_fit(fit, mf))
})

test_that("F01 review: coxph(ties = \"exact\") on re-sorted data is used, and refused once edited", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  for (yy in c(TRUE, FALSE)) {
    set.seed(2)
    d <- data.frame(x = rnorm(500))
    d$time <- ceiling(rexp(500, exp(0.3 * d$x)) * 5)   # many ties
    d$status <- rbinom(500, 1, 0.8)
    fit <- survival::coxph(S(time, status) ~ x, d, ties = "exact", y = yy)
    want <- regtab(fit, stats = ca2_S)$body
    d <- d[sample(500), ]
    rownames(d) <- NULL
    # survival's coxph.fit() recomputes "exact" as Breslow (LL -743.08 for
    # the fit's -724.02): the partial log-likelihood cannot check it.
    expect_identical(regtab(fit, stats = ca2_S)$body, want, label = paste("y =", yy))
    d$status[d$status == 0][1:20] <- 1L
    expect_error(regtab(fit, stats = ca2_S), "have changed since", info = paste("y =", yy))
  }
})

test_that("F01 review: a non-converged survreg(y = FALSE) is refused naming the non-convergence", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  set.seed(3)
  d <- data.frame(x = rnorm(200))
  d$time <- rweibull(200, 1.5, exp(0.5 * d$x))
  d$status <- rbinom(200, 1, 0.7)
  expect_warning(f0 <- survival::survreg(S(time, status) ~ x, d, y = FALSE, maxiter = 2), "did not converge")
  f1 <- suppressWarnings(survival::survreg(S(time, status) ~ x, d, y = FALSE,
                                           control = survival::survreg.control(maxiter = 2)))
  for (f in list(f0, f1)) {
    expect_true(tabtools:::.rt_survreg_unconverged(f))
    expect_error(regtab(f, stats = ca2_S), "did not converge (it ran out of iterations after 2)", fixed = TRUE)
  }
  # A converged fit is not flagged; one that keeps its response is checked
  # on it instead: used unchanged and re-sorted, refused once edited.
  expect_false(tabtools:::.rt_survreg_unconverged(survival::survreg(S(time, status) ~ x, d, y = FALSE)))
  fy <- suppressWarnings(survival::survreg(S(time, status) ~ x, d, maxiter = 2))
  want <- regtab(fy, stats = ca2_S)$body
  d0 <- d
  d <- d[sample(200), ]
  rownames(d) <- NULL
  expect_identical(regtab(fy, stats = ca2_S)$body, want)
  d <- d0
  d$time <- d$time * 2
  expect_error(regtab(fy, stats = ca2_S), "have changed since")
})

# F05 ------------------------------------------------------------------------

test_that("F05: imputations on different rows of the same size are refused; the same rows are pooled", {
  set.seed(92)
  d <- data.frame(x = rnorm(30), y = rnorm(30))
  a <- d
  b <- d
  a$x[1] <- NA
  b$x[2] <- NA
  fit1 <- lm(y ~ x, a)  # rows 2:30
  fit2 <- lm(y ~ x, b)  # rows 1, 3:30
  expect_error(regtab(tt_mi(list(fit1, fit2))), "fitted on different observations (29 each", fixed = TRUE)
  expect_error(tabtools:::tt_coef(tt_mi(list(fit1, fit2))), "different observations")
  # A subset on an imputed variable that keeps as many rows but not the
  # same ones, and zero weights on different rows.
  d$x[2:3] <- c(-5, 0)
  e <- d
  e$x[2:3] <- c(0, -5)
  expect_error(regtab(tt_mi(list(lm(y ~ x, d, subset = x > -4), lm(y ~ x, e, subset = x > -4)))),
               "different observations")
  w1 <- rep(1, 30); w1[3] <- 0
  w2 <- rep(1, 30); w2[4] <- 0
  expect_error(regtab(tt_mi(list(glm(y ~ x, data = d, weights = w1), glm(y ~ x, data = d, weights = w2)))),
               "different observations")
  # Good path: the same rows (each dropping row 1), other imputed values.
  b$x <- a$x
  b$x[5] <- b$x[5] + 0.5
  expect_no_error(tab <- regtab(tt_mi(list(fit1, lm(y ~ x, b)))))
  expect_s3_class(tab, "tt_table")
  # Datasets renumbered per imputation (a split long-format stack) share
  # no row name: compared on N only, as before.
  d2 <- d
  rownames(d2) <- 31:60
  expect_no_error(regtab(tt_mi(list(lm(y ~ x, d), lm(y ~ x, d2)))))
})

test_that("F05: coxph imputations on different rows of the same size are refused", {
  skip_if_not_installed("survival")
  set.seed(4)
  d <- data.frame(time = rexp(60), status = rbinom(60, 1, 0.7), x = rnorm(60))
  a <- d
  b <- d
  a$x[1] <- NA
  b$x[2] <- NA
  S <- survival::Surv
  expect_error(regtab(tt_mi(list(survival::coxph(S(time, status) ~ x, a), survival::coxph(S(time, status) ~ x, b)))),
               "different observations")
  b <- a
  b$x[7] <- b$x[7] + 1
  expect_no_error(regtab(tt_mi(list(survival::coxph(S(time, status) ~ x, a), survival::coxph(S(time, status) ~ x, b)))))
})

# F06 ------------------------------------------------------------------------

ca2_contrast_data <- function() {
  set.seed(5)
  d <- data.frame(g = factor(rep(c("a", "b"), each = 40)))
  d$y <- 3 * (d$g == "b") + rnorm(80)
  d
}

test_that("F06: imputations whose same-named coefficients code opposite contrasts are refused", {
  d <- ca2_contrast_data()
  C1 <- matrix(c(0, 1), ncol = 1, dimnames = list(c("a", "b"), "b"))
  C2 <- matrix(c(1, 0), ncol = 1, dimnames = list(c("a", "b"), "b"))
  fit1 <- lm(y ~ g, d, contrasts = list(g = C1))
  fit2 <- lm(y ~ g, d, contrasts = list(g = C2))
  expect_identical(names(coef(fit1)), names(coef(fit2)))
  expect_error(regtab(tt_mi(list(fit1, fit2))), "factor coding differs between imputations 1 and 2 (g)", fixed = TRUE)
  expect_error(tabtools:::tt_coef(tt_mi(list(fit1, fit2))), "factor coding differs")
  # A coding that only flips the sign, under the same name.
  C3 <- matrix(c(0, -1), ncol = 1, dimnames = list(c("a", "b"), "b"))
  expect_error(regtab(tt_mi(list(fit1, lm(y ~ g, d, contrasts = list(g = C3))))), "factor coding differs")
  # Behind the gate, every imputation's own rows are built and compared:
  # the refused single-fit coding surfaces as a refusal of imputation 2.
  x <- tt_mi(list(fit1, fit2))
  expect_error(tabtools:::tt_regtab_rows(x, tabtools:::tt_model_info(x)), "Imputation 2")
})

test_that("F06: the same coding in every imputation pools, however it is spelt", {
  d <- ca2_contrast_data()
  e <- d
  e$y <- e$y + rnorm(80, sd = 0.1)
  # Default treatment coding, and the same coding given by name, by
  # function and as its matrix.
  C1 <- matrix(c(0, 1), ncol = 1, dimnames = list(c("a", "b"), "b"))
  fits <- list(lm(y ~ g, d), lm(y ~ g, e, contrasts = list(g = "contr.treatment")),
               lm(y ~ g, e, contrasts = list(g = stats::contr.treatment)), lm(y ~ g, e, contrasts = list(g = C1)))
  tab <- regtab(tt_mi(fits))
  body <- tab$body
  expect_true(any(grepl("Reference", unlist(body))))
  # Releveled alike in every imputation (reference b): pools; releveled in
  # one only: refused.
  d$g <- stats::relevel(d$g, "b")
  e$g <- stats::relevel(e$g, "b")
  expect_no_error(regtab(tt_mi(list(lm(y ~ g, d), lm(y ~ g, e)))))
  e$g <- stats::relevel(e$g, "a")
  expect_error(regtab(tt_mi(list(lm(y ~ g, d), lm(y ~ g, e)))), "different coefficients|factor coding differs")
})

test_that("F06 review: only an imputation whose design signature differs has its own rows built", {
  d <- ca2_contrast_data()
  d$x <- rnorm(80)
  d$lg <- ifelse(d$x > 0, "hi", "lo")
  n_rows <- 0L
  orig <- tabtools:::tt_regtab_rows
  local_mocked_bindings(tt_regtab_rows = function(fit, ...) {
    if (!inherits(fit, "tt_mi")) n_rows <<- n_rows + 1L
    orig(fit, ...)
  })
  fits <- lapply(1:5, function(k) {
    e <- d
    e$y <- e$y + rnorm(80, sd = 0.1)
    lm(y ~ g + x + lg, e)
  })
  want <- regtab(tt_mi(fits))$body
  # Five imputations of one design: imputation 1's rows only.
  expect_identical(n_rows, 1L)
  # A character column in imputation 1 and the same values as a factor in
  # imputation 3 name their coefficients alike (lglo) under the same coding:
  # the classes differ, so imputation 3's rows are built too, and map alike.
  e <- d
  e$lg <- factor(e$lg)
  fits[[3]] <- lm(y ~ g + x + lg, e)
  expect_identical(names(coef(fits[[3]])), names(coef(fits[[1]])))
  n_rows <- 0L
  expect_no_error(regtab(tt_mi(fits)))
  expect_identical(n_rows, 2L)
  expect_false(isTRUE(all.equal(tabtools:::.rt_mi_design_sig(fits[[3]]), tabtools:::.rt_mi_design_sig(fits[[1]]))))
  expect_true(isTRUE(all.equal(tabtools:::.rt_mi_design_sig(fits[[2]]), tabtools:::.rt_mi_design_sig(fits[[1]]))))
})
