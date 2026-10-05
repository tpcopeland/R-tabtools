library(testthat)
library(tabtools)

ims_data <- function(seed) {
  withr::with_seed(seed, {
    n <- 144L
    x <- rnorm(n); z <- rnorm(n)
    f <- factor(rep(c("A", "B", "C"), length.out = n))
    cluster <- rep(seq_len(24), each = 6L)
    d <- data.frame(x = x, z = z, f = f, cluster = cluster,
      stratum = factor(paste0("S", (cluster - 1L) %% 3L + 1L)),
      matched = rep(seq_len(36), each = 4L),
      time = exp(1 + .2*x + .15*(f == "B") + rnorm(n, sd = .5)),
      event = rbinom(n, 1, .7), cause = sample(0:2, n, TRUE, prob = c(.25, .5, .25)),
      binary = rep(c(0, 0, 1, 1), length.out = n),
      y = 1 + .2*x - .1*z + .15*(f == "B") + rnorm(n),
      w = rep(rep(c(1, 2, 3), 8), each = 6L))
    d$include <- d$f != "C" & seq_len(n) <= 132L
    d$x[7] <- NA_real_; d$z[16] <- NA_real_; d$f[22] <- NA
    d$time[2] <- NA_real_; d$event[4] <- NA_real_; d$cause[4] <- NA_integer_
    d$y[4] <- NA_real_; d$binary[4] <- NA_real_
    d$stratum[10] <- NA; d$cluster[13] <- NA_integer_; d$matched[19] <- NA_integer_
    d$w[25] <- NA_real_
    d
  })
}

ims_seed_done <- function(case_id, seed) {
  cat(sprintf("\nIM-SEED case_id=%s seed=%d\n", case_id, seed))
}

ims_capture <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

ims_mask <- function(d, columns) {
  !is.na(d$include) & d$include & complete.cases(d[, columns, drop = FALSE])
}

ims_measure <- function(table, metric) {
  sample <- table$meta$sample_accounting
  expect_type(sample, "list")
  expect_equal(nrow(sample$populations), 1L)
  row <- sample$measures[sample$measures$metric == metric, ]
  expect_equal(nrow(row), 1L)
  row
}

ims_native_table <- function(fit, coefficients = stats::coef(fit), covariance = stats::vcov(fit)) {
  before <- fit
  expect_no_warning(table <- regtab(fit, vce = "model", stats = "n", keepintercept = TRUE))
  expect_identical(fit, before)
  expect_equal(tt_vcov(fit, vce = "model"), as.matrix(covariance), tolerance = 1e-10)
  rows <- table$meta$regtab_rows
  ratio <- table$stored$coef_label %in% c("HR", "OR", "TR", "SHR", "IRR", "RRR")
  for (term in names(coefficients)[is.finite(coefficients)]) {
    key <- if (term == "(Intercept)") "_cons" else if (term == "fB" && !inherits(fit, "crr")) "2.f" else term
    found <- rows[rows$key %in% key & rows$status == "est", ]
    expect_equal(nrow(found), 1L, info = paste("native coefficient", term))
    expected <- if (ratio) exp(unname(coefficients[term])) else unname(coefficients[term])
    expect_equal(found$estimate, expected, tolerance = 1e-10)
  }
  table
}

ims_records <- function(table, accepted, used = accepted, observed = accepted,
                        input = NA_real_, weights = rep(1, used)) {
  expect_equal(ims_measure(table, "eligible_n")$value, accepted)
  expect_equal(ims_measure(table, "used_n")$value, used)
  expect_equal(ims_measure(table, "fitted_n")$value, used)
  if (is.na(observed)) {
    expect_identical(ims_measure(table, "observed_n")$status, "unavailable")
  } else expect_equal(ims_measure(table, "observed_n")$value, observed)
  if (is.na(input)) {
    expect_identical(ims_measure(table, "input_n")$status, "unavailable")
  } else {
    expect_equal(ims_measure(table, "input_n")$value, input)
    expect_equal(ims_measure(table, "excluded_n")$value, input - used)
  }
  expect_equal(ims_measure(table, "weight_sum")$value, sum(weights))
  expect_equal(ims_measure(table, "effective_n")$value, sum(weights)^2 / sum(weights^2))
}

test_that("IMS-01: Cox strata clusters missingness and nonunit weights", {
  skip_if_not_installed("survival")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed); before <- d
    mask <- ims_mask(d, c("time", "event", "x", "f", "stratum", "cluster", "w"))
    env <- list2env(list(d = d), parent = asNamespace("survival"))
    fit <- evalq(coxph(Surv(time, event) ~ x + f + strata(stratum) + cluster(cluster),
      data = d, weights = w, subset = include, ties = "breslow", model = TRUE, y = TRUE), env)
    expect_s3_class(fit, "coxph")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask), weights = d$w[mask])
    expect_equal(fit$n, sum(mask))
    expect_equal(fit$nevent, sum(d$event[mask]))
    expect_equal(stats::nobs(fit), sum(d$event[mask]))
    expect_equal(ims_measure(table, "frame_n")$value, sum(mask))
    # Existing subject N uses each cluster's constant case weight once.
    expect_equal(ims_measure(table, "reported_n")$value, sum(d$w[mask][!duplicated(d$cluster[mask])]))
    expect_identical(d, before)
    ims_seed_done("IMS-01", seed)
  }
})

test_that("IMS-02: expanded risk records do not become subject counts", {
  skip_if_not_installed("survival")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed)
    mask <- ims_mask(d, c("time", "event", "x", "z", "stratum"))
    fit <- survival::coxph(survival::Surv(time, event) ~ tt(x) + z + strata(stratum),
      data = d, subset = include, ties = "breslow", x = TRUE, y = TRUE,
      tt = function(x, t, ...) x * log1p(t))
    expect_s3_class(fit, "coxph")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask), observed = NA_real_)
    expect_gt(NROW(fit$y), fit$n)
    expect_equal(ims_measure(table, "frame_n")$value, NROW(fit$y))
    expect_equal(fit$n, sum(mask))
    expect_equal(fit$nevent, sum(d$event[mask]))
    expect_equal(stats::nobs(fit), sum(d$event[mask]))
    ims_seed_done("IMS-02", seed)
  }
})

test_that("IMS-03: conditional logistic retains sparse matched strata", {
  skip_if_not_installed("survival")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed)
    d$binary[d$matched %in% c(2, 5)] <- 0
    mask <- ims_mask(d, c("binary", "x", "f", "matched"))
    env <- list2env(list(d = d), parent = asNamespace("survival"))
    weighted <- evalq(clogit(binary ~ x + f + strata(matched), data = d,
      weights = w, subset = include, method = "efron", model = TRUE, y = TRUE), env)
    expect_s3_class(weighted, "clogit")
    expect_error(regtab(weighted), "does not support weighted")
    fit <- evalq(clogit(binary ~ x + f + strata(matched), data = d,
      subset = include, method = "exact", model = TRUE, y = TRUE), env)
    expect_s3_class(fit, "clogit")
    expect_s3_class(fit, "coxph")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask))
    expect_equal(fit$n, sum(mask))
    expect_equal(ims_measure(table, "reported_n")$value, sum(mask))
    expect_true(any(tapply(d$binary[mask], d$matched[mask], sum) == 0))
    ims_seed_done("IMS-03", seed)
  }
})

test_that("IMS-04: parametric survival separates weighted N from records", {
  skip_if_not_installed("survival")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed)
    mask <- ims_mask(d, c("time", "event", "x", "f", "w"))
    fit <- survival::survreg(survival::Surv(time, event) ~ x + f,
      data = d, weights = w, subset = include, model = TRUE, y = TRUE,
      control = survival::survreg.control(maxiter = 100))
    expect_s3_class(fit, "survreg")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask), weights = d$w[mask])
    expect_equal(ims_measure(table, "frame_n")$value, sum(mask))
    expect_equal(ims_measure(table, "reported_n")$value, sum(d$w[mask]))
    expect_equal(table$stored$n_1, sum(d$w[mask]))
    ims_seed_done("IMS-04", seed)
  }
})

test_that("IMS-05: genuine censored Gaussian fits keep observation masks", {
  skip_if_not_installed("AER")
  skip_if_not_installed("survival")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed)
    mask <- ims_mask(d, c("y", "x", "f", "w"))
    expect_gt(sum(mask & d$y <= 0), 0)
    fit <- AER::tobit(y ~ x + f, data = d, left = 0, weights = w,
      subset = include, model = TRUE, y = TRUE,
      control = survival::survreg.control(maxiter = 100))
    expect_s3_class(fit, "tobit")
    expect_s3_class(fit, "survreg")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask), weights = d$w[mask])
    expect_equal(ims_measure(table, "frame_n")$value, sum(mask))
    expect_equal(ims_measure(table, "reported_n")$value, sum(d$w[mask]))
    expect_equal(table$stored$n_1, sum(d$w[mask]))
    ims_seed_done("IMS-05", seed)
  }
})

test_that("IMS-06: competing-risk accepted rows cannot reconstruct the supplied cohort", {
  skip_if_not_installed("cmprsk")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed)
    prepared <- d[!is.na(d$include) & d$include, ]
    mask <- ims_mask(d, c("time", "cause", "x", "z", "f"))
    covariates <- cbind(x = prepared$x, z = prepared$z, fB = as.numeric(prepared$f == "B"))
    native <- ims_capture(cmprsk::crr(prepared$time, prepared$cause, covariates))
    expect_identical(native$warnings, character())
    fit <- native$value
    expect_s3_class(fit, "crr")
    expect_true(isTRUE(fit$converged))
    covariance <- fit$var
    dimnames(covariance) <- list(names(fit$coef), names(fit$coef))
    table <- ims_native_table(fit, fit$coef, covariance)
    ims_records(table, sum(mask), observed = NA_real_)
    expect_equal(fit$n, sum(mask))
    expect_equal(ims_measure(table, "reported_n")$value, sum(mask))
    expect_identical(ims_measure(table, "frame_n")$status, "unavailable")
    ims_seed_done("IMS-06", seed)
  }
})

test_that("IMS-07: survey preparation and zero contributors remain separate", {
  skip_if_not_installed("survey")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed); d$w[11] <- 0
    design_mask <- ims_mask(d, c("cluster", "stratum", "w"))
    prepared <- d[design_mask, ]
    design <- survey::svydesign(ids = ~cluster, strata = ~stratum, weights = ~w, data = prepared, nest = TRUE)
    mask <- design_mask & complete.cases(d[, c("y", "x", "f")])
    used <- mask & d$w > 0
    native <- ims_capture(survey::svyglm(y ~ x + f, design = design))
    expect_true(all(grepl("zero weight", native$warnings, fixed = TRUE)))
    fit <- native$value
    expect_s3_class(fit, "svyglm")
    expect_s3_class(fit, "glm")
    table <- ims_native_table(fit)
    ims_records(table, sum(mask), used = sum(used), weights = d$w[used])
    expect_equal(ims_measure(table, "zero_weight_n")$value, sum(mask & d$w == 0))
    expect_equal(ims_measure(table, "frame_n")$value, sum(mask))
    expect_identical(table$meta$sample_accounting$populations$weight_type, "survey")
    expect_equal(table$stored$n_1, sum(used))
    ims_seed_done("IMS-07", seed)
  }
})

test_that("IMS-08: explicit complete-case preparation is the GEE input cohort", {
  skip_if_not_installed("geepack")
  for (seed in c(4101L, 4102L)) {
    d <- ims_data(seed); d$w[11] <- 0
    unprepared <- d[!is.na(d$include) & d$include, ]
    unprepared$f <- droplevels(unprepared$f)
    expect_error(geepack::geeglm(y ~ x + f, data = unprepared, id = cluster, weights = w,
      corstr = "exchangeable", family = gaussian()),
      "nrow\\(zsca\\) and length\\(y\\) not match")
    # geeglm requires complete data; preparation is explicit and precedes fitting.
    mask <- ims_mask(d, c("y", "x", "f", "cluster", "w"))
    prepared <- d[mask, ]
    prepared$f <- droplevels(prepared$f)
    before <- prepared
    fit <- geepack::geeglm(y ~ x + f, data = prepared, id = cluster, weights = w,
      corstr = "exchangeable", family = gaussian())
    expect_s3_class(fit, "geeglm")
    expect_s3_class(fit, "glm")
    expect_identical(fit$geese$error, 0L)
    table <- ims_native_table(fit)
    used <- mask & d$w > 0
    ims_records(table, sum(mask), used = sum(used), input = nrow(prepared), weights = d$w[used])
    expect_equal(ims_measure(table, "zero_weight_n")$value, sum(mask & d$w == 0))
    expect_equal(ims_measure(table, "frame_n")$value, sum(mask))
    expect_lt(ims_measure(table, "input_n")$value, nrow(d))
    expect_equal(table$stored$n_1, sum(used))
    expect_identical(prepared, before)
    ims_seed_done("IMS-08", seed)
  }
})
