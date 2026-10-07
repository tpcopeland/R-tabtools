library(testthat)
library(tabtools)

# Seeded installed-user interactions. Native covariance parity uses vce=model.
# The Rubin oracle uses stats::cov across native estimates, not tabtools pooling.
# Seeds are deliberately inside each case; no case depends on another's state.

imc_seed_done <- function(case_id, seed) {
  cat(sprintf("\nIM-SEED case_id=%s seed=%d\n", case_id, seed))
}

imc_data <- function() {
  n <- 96L
  d <- data.frame(id = paste0("imc-private-", seq_len(n)),
                  x = rnorm(n), z = rnorm(n),
                  treat = as.integer(seq_len(n) %% 7L == 0L),
                  w = rep(c(1, 2, 3, 20), length.out = n),
                  include = seq_len(n) <= 91L)
  d$y <- 1.2 + .8 * d$x - .4 * d$z + .6 * d$treat + rnorm(n)
  d$count <- rpois(n, exp(.3 + .25 * d$x - .2 * d$z))
  d$time <- rexp(n, exp(.15 * d$x - .1 * d$z)) + .01
  d$status <- rep(c(1L, 0L, 1L, 2L), length.out = n)
  d$g <- factor(ifelse(d$treat == 1L, "sparse", "common"),
                levels = c("common", "sparse", "absent"))
  d$x[c(3, 8)] <- NA_real_
  d$z[c(4, 9, 14, 19)] <- NA_real_
  d$y[11] <- NA_real_
  d$w[c(5, 16)] <- 0
  d
}

imc_counts <- function(table, k = 1L) {
  s <- table$meta$sample_accounting
  expect_identical(s$version, 1L)
  id <- s$populations$id[[k]]
  m <- s$measures[s$measures$population_id == id, ]
  expect_identical(length(unique(m$metric)), 12L)
  stats::setNames(m$value, m$metric)
}

imc_source_ledgers <- function(table, sources) {
  s <- table$meta$sample_accounting
  expect_identical(nrow(s$populations), length(sources))
  for (k in seq_along(sources)) {
    old <- sources[[k]]$meta$sample_accounting
    new_id <- s$populations$id[k]
    old_id <- old$populations$id[1]
    population <- s$populations[k, setdiff(names(s$populations), c("id", "component")), drop = FALSE]
    original <- old$populations[1, names(population), drop = FALSE]
    rownames(population) <- rownames(original) <- NULL
    expect_identical(population, original)
    for (field in c("measures", "exclusions")) {
      new <- s[[field]][s[[field]]$population_id == new_id, , drop = FALSE]
      want <- old[[field]][old[[field]]$population_id == old_id, , drop = FALSE]
      new$population_id <- rep(old_id, nrow(new))
      rownames(new) <- rownames(want) <- NULL
      expect_identical(new, want)
    }
  }
}

imc_native_rows <- function(table, beta, covariance, ratio = FALSE, df = Inf) {
  rows <- table$meta$regtab_rows
  keys <- ifelse(names(beta) == "(Intercept)", "_cons", names(beta))
  pos <- match(keys, rows$key)
  expect_false(anyNA(pos))
  se <- sqrt(diag(covariance))
  crit <- if (is.finite(df)) qt(.975, df) else qnorm(.975)
  transform <- if (ratio) exp else identity
  expect_equal(rows$estimate[pos], unname(transform(beta)), tolerance = 1e-10)
  expect_equal(rows$conf.low[pos], unname(transform(beta - crit * se)), tolerance = 1e-10)
  expect_equal(rows$conf.high[pos], unname(transform(beta + crit * se)), tolerance = 1e-10)
  p <- if (is.finite(df)) 2 * pt(abs(beta / se), df, lower.tail = FALSE) else
    2 * pnorm(abs(beta / se), lower.tail = FALSE)
  expect_equal(rows$p.value[pos], unname(p), tolerance = 1e-10)
}

imc_rubin <- function(fits, covariance) {
  Q <- vapply(fits, function(f) if (inherits(f, "crr")) f$coef else coef(f),
              if (inherits(fits[[1]], "crr")) fits[[1]]$coef else coef(fits[[1]]))
  W <- Reduce(`+`, lapply(fits, covariance)) / length(fits)
  B <- stats::cov(t(Q))
  list(beta = rowMeans(Q), W = W, B = B,
       total = W + (1 + 1 / length(fits)) * B)
}

imc_pool_checks <- function(mi, fits, covariance, ratio, input_n, eligible_n, used_n,
                            weight_sum = NULL) {
  before <- mi
  oracle <- imc_rubin(fits, covariance)
  expect_gt(sum(diag(oracle$B)), 0)
  expect_equal(tt_vcov(mi, "model"), oracle$total, tolerance = 1e-10)
  out <- regtab(mi, vce = "model", keepintercept = TRUE, stats = c("n", "mi_m"))
  rows <- out$meta$regtab_rows
  keys <- ifelse(names(oracle$beta) == "(Intercept)", "_cons", names(oracle$beta))
  pos <- match(keys, rows$key)
  expect_false(anyNA(pos))
  transform <- if (ratio) exp else identity
  expect_equal(rows$estimate[pos], unname(transform(oracle$beta)), tolerance = 1e-10)
  # Recover total variances independently from the published Wald limits.
  M <- length(fits)
  u <- diag(oracle$W)
  b <- diag(oracle$B)
  r <- (1 + 1 / M) * b / u
  large_df <- (M - 1) * (1 + 1 / r)^2
  if (inherits(fits[[1]], "lm") && !inherits(fits[[1]], "glm")) {
    complete_df <- min(vapply(fits, df.residual, 0))
    gamma <- (1 + 1 / M) * b / diag(oracle$total)
    observed_df <- complete_df * (complete_df + 1) * (1 - gamma) / (complete_df + 3)
    df <- 1 / (1 / large_df + 1 / observed_df)
  } else df <- large_df
  critical <- qt(.975, df)
  se <- sqrt(diag(oracle$total))
  expect_equal(rows$conf.low[pos], unname(transform(oracle$beta - critical * se)), tolerance = 1e-10)
  expect_equal(rows$conf.high[pos], unname(transform(oracle$beta + critical * se)), tolerance = 1e-10)
  expect_equal(rows$p.value[pos], unname(2 * pt(abs(oracle$beta / se), df,
                                               lower.tail = FALSE)), tolerance = 1e-10)
  expect_identical(out$stored$mi_m_1, as.numeric(M))
  expect_identical(out$meta$mi_sample_identity[[1]],
                   list(method = "explicit_ids", n = rep(as.numeric(used_n), M), strict = TRUE))
  expect_identical(out$meta$sample_accounting$populations$imputation, seq_len(M))
  for (k in seq_len(M)) {
    counts <- imc_counts(out, k)
    expect_equal(unname(counts[c("input_n", "eligible_n", "used_n", "fitted_n")]),
                 c(input_n, eligible_n, used_n, used_n), tolerance = 0)
    if (!is.null(weight_sum)) expect_equal(counts[["weight_sum"]], weight_sum, tolerance = 1e-10)
  }
  expect_false(any(grepl("imc-private-", unlist(out$meta), fixed = TRUE)))
  expect_identical(mi, before)
  out
}

test_that("IMC-01 genuine glm_weightit preserves prepared sample and estimated-weight covariance", {
  skip_if_not_installed("WeightIt")
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    raw <- imc_data()
    before_data <- raw
    keep <- raw$include & complete.cases(raw[c("y", "x", "z")]) & raw$w > 0
    d <- raw[keep, ]
    weights <- WeightIt::weightit(treat ~ x + z, data = d, method = "glm", s.weights = "w")
    fit <- WeightIt::glm_weightit(y ~ treat + x, data = d, weightit = weights)
    expect_identical(class(fit)[1], "glm_weightit")
    before <- fit
    out <- regtab(fit, vce = "model", keepintercept = TRUE, stats = "n")
    expect_equal(tt_vcov(fit, "model"), vcov(fit), tolerance = 1e-12)
    imc_native_rows(out, coef(fit), vcov(fit))
    counts <- imc_counts(out)
    expect_equal(unname(counts[c("input_n", "eligible_n", "fitted_n", "reported_n")]),
                 c(NA_real_, sum(keep), sum(keep), nobs(fit)), tolerance = 0)
    expect_equal(counts[["weight_sum"]], sum(fit$prior.weights), tolerance = 1e-10)
    expect_equal(counts[["effective_n"]], sum(fit$prior.weights)^2 / sum(fit$prior.weights^2),
                 tolerance = 1e-10)
    expect_identical(out$meta$sample_accounting$populations$weight_type, "estimated")
    expect_identical(fit, before)
    expect_identical(raw, before_data)
    imc_seed_done("IMC-01", seed)
  }
})

test_that("IMC-02 tt_mi lm and glm combine reordered explicit IDs zero weights and missing covariates", {
  skip_if_not_installed("mice")
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    d <- imc_data()
    before_data <- d
    imputations <- lapply(seq_len(3L), function(k) {
      a <- d
      a$z <- a$z + (k - 1) * (.08 * a$x + .05 * rnorm(nrow(a)))
      a <- a[sample.int(nrow(a)), ]
      rownames(a) <- NULL
      a
    })
    for (adapter in c("lm", "glm")) {
      eligible <- d$include & complete.cases(d[c(if (adapter == "lm") "y" else "count", "x", "z")])
      used <- eligible & d$w > 0
      fits <- lapply(imputations, function(a) {
        if (adapter == "lm") lm(y ~ x + z, a, weights = w, subset = include, na.action = na.exclude) else
          glm(count ~ x + z, poisson(), a, weights = w, subset = include, na.action = na.omit)
      })
      expect_identical(vapply(fits, function(f) class(f)[1], ""), rep(adapter, 3L))
      ids <- lapply(imputations, function(a) {
        mask <- a$include & complete.cases(a[c(if (adapter == "lm") "y" else "count", "x", "z")]) & a$w > 0
        a$id[mask]
      })
      mi <- tt_mi(fits, observation_ids = ids, sample_check = "strict")
      out <- imc_pool_checks(mi, fits, vcov, adapter == "glm",
                             if (adapter == "glm") nrow(d) else NA_real_,
                             sum(eligible), sum(used), sum(d$w[used]))
      expect_identical(out$meta$sample_accounting$measures$value[
        out$meta$sample_accounting$measures$metric == "zero_weight_n"], rep(2, 3L))
      # A genuine public mice conversion must retain the same strict ID policy.
      mira <- mice::as.mira(fits)
      expect_s3_class(mira, "mira")
      wrapped <- tt_mi(mira,
                       observation_ids = ids, sample_check = "strict")
      expect_equal(tt_vcov(wrapped, "model"), tt_vcov(mi, "model"), tolerance = 1e-12)
      expect_identical(regtab(wrapped, vce = "model", keepintercept = TRUE,
                              stats = c("n", "mi_m"))$body, out$body)
    }
    expect_identical(d, before_data)
    imc_seed_done("IMC-02", seed)
  }
})

test_that("IMC-03 tt_mi Cox and competing-risk pooling preserve reordered retained IDs", {
  skip_if_not_installed("survival")
  skip_if_not_installed("cmprsk")
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    d <- imc_data()
    # Cox rejects native zero weights and crr has no case-weight argument.
    # Zero-weight records are explicitly removed at preparation, never claimed as fitted.
    keep <- d$include & complete.cases(d[c("time", "status", "x", "z")]) & d$w > 0
    prepared <- d[keep, ]
    before_data <- prepared
    imputations <- lapply(seq_len(3L), function(k) {
      a <- prepared
      a$z <- a$z + (k - 1) * (.08 * a$x + .05 * rnorm(nrow(a)))
      a <- a[sample.int(nrow(a)), ]
      rownames(a) <- NULL
      a
    })
    for (adapter in c("coxph", "crr")) {
      fits <- lapply(imputations, function(a) {
        if (adapter == "coxph") survival::coxph(survival::Surv(time, status == 1L) ~ x + z,
                                                a, model = TRUE, weights = w) else
          cmprsk::crr(a$time, a$status, cov1 = as.matrix(a[c("x", "z")]))
      })
      expect_identical(vapply(fits, function(f) class(f)[1], ""), rep(adapter, 3L))
      mi <- tt_mi(fits, observation_ids = lapply(imputations, `[[`, "id"), sample_check = "strict")
      covariance <- if (adapter == "crr") function(f) {
        V <- f$var
        dimnames(V) <- rep(list(names(f$coef)), 2L)
        V
      } else vcov
      imc_pool_checks(mi, fits, covariance, TRUE, NA_real_, nrow(prepared), nrow(prepared),
                      if (adapter == "coxph") sum(prepared$w) else nrow(prepared))
    }
    expect_identical(prepared, before_data)
    imc_seed_done("IMC-03", seed)
  }
})

test_that("IMC-04 tt_uv keeps covariate-specific missingness and zero-weight populations", {
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    d <- imc_data()
    before_data <- d
    uv <- regtab_uv(d, "y", c("x", "z"), method = lm,
                    method.args = list(weights = quote(w), subset = quote(include), na.action = na.exclude))
    expect_identical(class(uv), "tt_uv")
    before <- uv
    out <- regtab(uv, vce = "model", stats = "n")
    expect_identical(out$meta$sample_accounting$populations$variable, c("x", "z"))
    expect_null(out$stored$n_1)
    expect_false("stat:n" %in% out$rows$key)
    for (k in seq_along(uv$x)) {
      v <- uv$x[k]
      eligible <- d$include & complete.cases(d[c("y", v)])
      used <- eligible & d$w > 0
      native <- lm(reformulate(v, "y"), d[used, ], weights = w)
      row <- match(v, out$meta$regtab_rows$key)
      beta <- coef(native)[v]
      se <- sqrt(vcov(native)[v, v])
      expect_equal(out$meta$regtab_rows$estimate[row], unname(beta), tolerance = 1e-10)
      expect_equal(out$meta$regtab_rows$conf.low[row], unname(beta - qt(.975, df.residual(native)) * se),
                   tolerance = 1e-10)
      counts <- imc_counts(out, k)
      expect_equal(unname(counts[c("input_n", "eligible_n", "fitted_n", "excluded_n", "zero_weight_n")]),
                   c(nrow(d), sum(eligible), sum(used), sum(!used), sum(eligible & !used)), tolerance = 0)
      expect_equal(counts[["weight_sum"]], sum(d$w[used]), tolerance = 1e-10)
    }
    expect_identical(uv, before)
    expect_identical(d, before_data)
    imc_seed_done("IMC-04", seed)
  }
})

test_that("IMC-05 marginaleffects predictions comparisons slopes hypotheses match linear algebra", {
  skip_if_not_installed("marginaleffects")
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    d <- imc_data()
    before_data <- d
    fit <- lm(y ~ treat + x, d, weights = w, subset = include, na.action = na.exclude)
    before <- fit
    grid <- data.frame(treat = c(0, 0, 1), x = c(-.7, .4, .9))
    results <- list(
      predictions = marginaleffects::avg_predictions(fit, newdata = grid, df = Inf,
                                                     numderiv = list("fdcenter", eps = 1e-4)),
      comparisons = marginaleffects::avg_comparisons(fit, newdata = grid, variables = "treat", df = Inf,
                                                     numderiv = list("fdcenter", eps = 1e-4)),
      slopes = marginaleffects::avg_slopes(fit, newdata = grid, variables = "x", df = Inf, eps = .01,
                                                     numderiv = list("fdcenter", eps = 1e-4)),
      hypotheses = marginaleffects::hypotheses(fit, hypothesis = "x = 0", df = Inf,
                                                numderiv = list("fdcenter", eps = 1e-4)))
    vectors <- list(predictions = c(1, mean(grid$treat), mean(grid$x)),
                    comparisons = c(0, 1, 0), slopes = c(0, 0, 1), hypotheses = c(0, 0, 1))
    used <- d$include & complete.cases(d[c("y", "x")]) & d$w > 0
    for (adapter in names(results)) {
      result <- results[[adapter]]
      expect_identical(class(result)[1], adapter)
      out <- effecttab(result, type = "margins", level = 95)
      a <- vectors[[adapter]]
      want <- drop(a %*% coef(fit))
      se <- sqrt(drop(a %*% vcov(fit) %*% a))
      rows <- out$meta$effect_rows
      expect_equal(rows$estimate, want, tolerance = 1e-8)
      expect_equal(rows$conf.low, want - qnorm(.975) * se, tolerance = 1e-8)
      expect_equal(rows$conf.high, want + qnorm(.975) * se, tolerance = 1e-8)
      expect_equal(rows$p.value, 2 * pnorm(abs(want / se), lower.tail = FALSE), tolerance = 1e-8)
      s <- out$meta$sample_accounting
      model <- which(s$populations$scope == "model")
      expect_length(model, 1L)
      expect_identical(imc_counts(out, model)[["fitted_n"]], as.numeric(sum(used)))
      if (adapter != "hypotheses") {
        evaluation <- which(s$populations$scope == "evaluation")
        expect_length(evaluation, 1L)
        expect_identical(imc_counts(out, evaluation)[["input_n"]], as.numeric(nrow(grid)))
        expect_identical(imc_counts(out, evaluation)[["used_n"]], NA_real_)
      }
      expect_identical(attr(as.data.frame(out), "sample_accounting"), s)
    }
    expect_identical(fit, before)
    expect_identical(d, before_data)
    imc_seed_done("IMC-05", seed)
  }
})

test_that("IMC-06 matrix dataframe and modelsummary summaries preserve missing inference and unknown N", {
  skip_if_not_installed("tibble")
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    estimates <- rnorm(3)
    matrix_input <- cbind(estimates, estimates - .5, estimates + .5, c(.04, NA_real_, .7))
    matrix_input[2, 2:3] <- NA_real_
    rownames(matrix_input) <- c("same_label", "same_label", "third")
    before_matrix <- matrix_input
    matrix_table <- effecttab(matrix_input, type = "margins", level = 95)
    expect_identical(matrix_table$meta$effect_rows$key, c("same_label", "same_label#1", "third"))
    expect_equal(matrix_table$meta$effect_rows$estimate, estimates, tolerance = 0)
    expect_equal(matrix_table$meta$effect_rows$conf.low, unname(matrix_input[, 2]), tolerance = 0)
    expect_identical(matrix_table$meta$effect_rows$p.value, unname(matrix_input[, 4]))
    expect_identical(imc_counts(matrix_table), stats::setNames(rep(NA_real_, 12), names(imc_counts(matrix_table))))
    supplied <- data.frame(term = c("x", "z", "no_inference"), estimate = estimates,
                           std.error = c(.2, .3, NA_real_))
    attr(supplied, "effect_scale") <- "Coef."
    attr(supplied, "inference_reference") <- "normal"
    before_supplied <- supplied
    effects <- effecttab(supplied, type = "margins", level = 95)
    expect_equal(effects$meta$effect_rows$conf.low,
                 estimates - qnorm(.975) * supplied$std.error, tolerance = 1e-12)
    expect_identical(effects$meta$effect_rows$p.value[3], NA_real_)
    supplied_fit <- regtab(supplied, vce = "stata", keepintercept = TRUE)
    expect_equal(supplied_fit$meta$regtab_rows$estimate, estimates, tolerance = 0)
    expect_identical(imc_counts(supplied_fit)[["fitted_n"]], NA_real_)
    tibble_input <- tibble::as_tibble(supplied)
    expect_identical(class(tibble_input)[1], "tbl_df")
    before_tibble <- tibble_input
    tibble_fit <- regtab(tibble_input, vce = "stata", keepintercept = TRUE)
    expect_equal(tibble_fit$meta$regtab_rows$estimate, estimates, tolerance = 0)
    expect_equal(tibble_fit$meta$regtab_rows$conf.low,
                 estimates - qnorm(.975) * supplied$std.error, tolerance = 1e-12)
    expect_identical(imc_counts(tibble_fit)[["fitted_n"]], NA_real_)
    expect_identical(tibble_input, before_tibble)
    # Exercise the documented modelsummary_list schema, without attributing
    # evidence to an absent modelsummary backend or inventing source records.
    ms <- structure(list(tidy = supplied,
                         glance = data.frame(nobs = 73, logLik = -41.5)), class = "modelsummary_list")
    converted <- tt_from_modelsummary(ms, exponentiate = TRUE, effect_scale = "OR", inference_reference = "normal")
    expect_equal(converted$estimate, exp(estimates), tolerance = 1e-12)
    table <- regtab(converted, vce = "stata", stats = c("n", "aic", "bic"))
    expect_equal(table$meta$regtab_rows$estimate, exp(estimates), tolerance = 1e-12)
    expect_equal(table$meta$regtab_rows$conf.low,
                 exp(estimates - qnorm(.975) * supplied$std.error), tolerance = 1e-12)
    expect_identical(imc_counts(table)[["reported_n"]], 73)
    expect_identical(imc_counts(table)[["fitted_n"]], NA_real_)
    expect_identical(imc_counts(table)[["input_n"]], NA_real_)
    expect_equal(table$stored$aic_1, -2 * (-41.5) + 2 * 3, tolerance = 0)
    expect_equal(table$stored$bic_1, -2 * (-41.5) + log(73) * 3, tolerance = 1e-12)
    expect_identical(matrix_input, before_matrix)
    expect_identical(supplied, before_supplied)
    imc_seed_done("IMC-06", seed)
  }
})

test_that("IMC-07 nested reused sources merge stack comptab presentation and dataframe transport preserve populations", {
  withr::local_preserve_seed()
  for (seed in c(3211L, 6547L)) {
    set.seed(seed)
    d <- imc_data()
    a_fit <- lm(y ~ x, d, weights = w, subset = include)
    b_fit <- lm(y ~ z, d, weights = w, subset = include)
    a <- regtab(a_fit, vce = "model", models = "Shared", stats = "n")
    b <- regtab(b_fit, vce = "model", models = "Shared", stats = "n")
    before <- list(a, b)
    merged <- tt_merge(tt_merge(a, b), a)
    stacked <- tt_stack(tt_stack(a, b), a)
    combined <- comptab(list(a, b, a), rows = list(1, 1, 1), section = c("First", "Second", "Again"))
    transported <- comptab(list(as.data.frame(a), as.data.frame(b), as.data.frame(a)),
                           rows = list(1, 1, 1), section = c("First", "Second", "Again"))
    expected_n <- c(sum(d$include & complete.cases(d[c("y", "x")]) & d$w > 0),
                    sum(d$include & complete.cases(d[c("y", "z")]) & d$w > 0))
    for (out in list(merged, stacked, combined, transported)) {
      s <- out$meta$sample_accounting
      expect_identical(nrow(s$populations), 3L)
      expect_identical(length(unique(s$populations$id)), 3L)
      expect_identical(s$measures$value[s$measures$metric == "fitted_n"],
                       as.numeric(expected_n[c(1, 2, 1)]))
      expect_identical(attr(as.data.frame(out), "sample_accounting"), s)
      expect_false(any(grepl("imc-private-", unlist(s), fixed = TRUE)))
      imc_source_ledgers(out, list(a, b, a))
    }
    expect_identical(transported$body, combined$body)
    expect_identical(transported$meta$sample_accounting, combined$meta$sample_accounting)
    forest <- as_forest_data(combined)
    expect_equal(forest$estimate, unname(c(coef(a_fit)["x"], coef(b_fit)["z"], coef(a_fit)["x"])),
                 tolerance = 1e-10)
    # Numeric values are checked before presentation; presentation must
    # preserve selected strings and the full source ledger, not recount rows.
    selected <- puttab(as.data.frame(merged), subset = 3)
    expect_identical(as.character(selected$body[1, ]), as.character(merged$body[1, ]))
    expected_ledger <- merged$meta$sample_accounting
    expected_ledger$populations$id <- paste0("source/", expected_ledger$populations$id)
    expected_ledger$populations$component <- paste0("source/", expected_ledger$populations$component)
    expected_ledger$measures$population_id <- paste0("source/", expected_ledger$measures$population_id)
    expected_ledger$exclusions$population_id <- paste0("source/", expected_ledger$exclusions$population_id)
    expect_identical(selected$meta$sample_accounting, expected_ledger)
    display <- stacktab(list(list(table = combined, rows = 1, cols = "A-B"),
                            list(table = combined, rows = 1, cols = "A-B")), spacing = 0)
    expect_identical(display$meta$sample_accounting$measures$value[
      display$meta$sample_accounting$measures$metric == "fitted_n"],
      rep(as.numeric(expected_n[c(1, 2, 1)]), 2))
    imc_source_ledgers(display, list(a, b, a, a, b, a))
    expect_identical(list(a, b), before)
    imc_seed_done("IMC-07", seed)
  }
})
