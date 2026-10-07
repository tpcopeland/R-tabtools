sample_model_measures <- function(table, population = 1L) {
  ledger <- table$meta$sample_accounting
  expect_type(ledger, "list")
  if (is.null(ledger)) return(NULL)
  id <- ledger$populations$id[[population]]
  rows <- ledger$measures[ledger$measures$population_id == id, ]
  stats::setNames(rows$value, rows$metric)
}

test_that("fitted accounting separates input, accepted records and positive weights", {
  d <- data.frame(y = seq_len(24) + sin(seq_len(24)), x = seq_len(24),
                  w = rep(c(1, 2, 3), 8))
  d$x[c(3, 7)] <- NA_real_
  d$y[12] <- NA_real_
  d$w[c(4, 10)] <- 0
  before <- d
  eligible <- seq_len(nrow(d)) <= 20 & stats::complete.cases(d)
  used <- eligible & d$w > 0
  for (action in list(na.omit, na.exclude)) {
    for (store in c(TRUE, FALSE)) {
      for (method in c("lm", "glm")) {
        fit <- if (method == "lm") lm(y ~ x, d, weights = w,
          subset = seq_len(24) <= 20, na.action = action, model = store) else
            glm(y ~ x, gaussian(), d, weights = w,
              subset = seq_len(24) <= 20, na.action = action, model = store)
        before_fit <- fit
        expect_no_warning(tt <- regtab(fit, vce = "robust", stats = "n"))
        counts <- sample_model_measures(tt)
        if (is.null(counts)) next
        expect_equal(counts[c("eligible_n", "used_n", "fitted_n", "zero_weight_n")],
                     c(eligible_n = sum(eligible), used_n = sum(used),
                       fitted_n = sum(used), zero_weight_n = sum(eligible & !used)))
        expect_equal(counts[["weight_sum"]], sum(d$w[used]))
        expect_equal(counts[["effective_n"]], sum(d$w[used])^2 / sum(d$w[used]^2))
        expect_equal(counts[["reported_n"]], stats::nobs(fit))
        expect_equal(tt$stored$n_1, stats::nobs(fit))
        expect_equal(counts[["frame_n"]], if (store) sum(eligible) else NA_real_)
        expect_equal(counts[["input_n"]], if (method == "glm") nrow(d) else NA_real_)
        expect_equal(counts[["excluded_n"]], if (method == "glm") sum(!used) else NA_real_)
        if (store) {
          expect_equal(counts[c("observed_n", "missing_n")], c(observed_n = sum(eligible), missing_n = 0))
        }
        exclusions <- tt$meta$sample_accounting$exclusions
        expect_equal(exclusions$n[exclusions$reason == "missing model variables after subsetting"], 3)
        expect_equal(exclusions$n[exclusions$stage == "eligible_to_used"], 2)
        expect_identical(fit, before_fit)
      }
    }
  }
  expect_identical(d, before)
})

test_that("stored model accounting does not evaluate a mutable data call", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12), x = seq_len(6))
  attr(d$x, "label") <- "Exposure"
  attr(d$y, "label") <- "Outcome"
  fit <- lm(y ~ x, d)
  evaluations <- 0L
  fit$call$data <- quote({ evaluations <- evaluations + 1L; stop("mutable source was evaluated") })
  before <- fit
  expect_no_warning(tt <- regtab(fit, stats = "n"))
  counts <- sample_model_measures(tt)
  if (!is.null(counts)) {
    expect_equal(counts[c("eligible_n", "fitted_n", "frame_n")],
                 c(eligible_n = 6, fitted_n = 6, frame_n = 6))
    expect_true(is.na(counts[["input_n"]]))
  }
  expect_identical(evaluations, 0L)
  expect_identical(fit, before)
})

test_that("binomial trial weights do not become original record weight totals", {
  d <- data.frame(x = seq(-1, 1, length.out = 16), trials = rep(c(8, 10, 12, 14), 4),
                  successes = c(2, 4, 5, 6, 1, 3, 6, 7, 4, 4, 7, 9, 6, 7, 8, 10),
                  w = rep(c(1, 2), 8))
  d$trials[c(3, 9)] <- d$successes[c(3, 9)] <- 0
  d$w[5] <- 0
  for (weighted in c(FALSE, TRUE)) {
    fit <- if (weighted) glm(cbind(successes, trials - successes) ~ x, binomial(), d, weights = w) else
      glm(cbind(successes, trials - successes) ~ x, binomial(), d)
    used <- d$trials > 0 & if (weighted) d$w > 0 else TRUE
    weights <- if (weighted) d$w[used] else rep(1, sum(used))
    tt <- regtab(fit, vce = "robust", stats = "n")
    counts <- sample_model_measures(tt)
    if (is.null(counts)) next
    expect_equal(counts[c("input_n", "eligible_n", "fitted_n", "zero_weight_n")],
                 c(input_n = 16, eligible_n = 16, fitted_n = sum(used), zero_weight_n = sum(!used)))
    expect_equal(counts[["weight_sum"]], sum(weights))
    expect_equal(counts[["effective_n"]], sum(weights)^2 / sum(weights^2))
    expect_false(isTRUE(all.equal(counts[["weight_sum"]], sum(fit$prior.weights))))
    expect_equal(tt$stored$n_1, stats::nobs(fit))
  }
})

test_that("equal and unequal univariable counts preserve distinct fitted populations", {
  d <- data.frame(y = c(3, 4, 8, 9, 13, 15, 2, 3, 2, 7, 9, 8),
                  x = c(seq_len(6), rep(NA_real_, 6)),
                  z = c(rep(NA_real_, 6), seq_len(6)))
  for (unequal in c(FALSE, TRUE)) {
    if (unequal) d$y[12] <- NA_real_
    tt <- regtab(regtab_uv(d, "y", c("x", "z"), method = lm), stats = "n")
    ledger <- tt$meta$sample_accounting
    expect_type(ledger, "list")
    if (is.null(ledger)) next
    expect_identical(ledger$populations$variable, c("x", "z"))
    expect_equal(ledger$populations$model, c(1, 1))
    expect_identical(length(unique(ledger$populations$id)), 2L)
    expect_equal(sample_model_measures(tt, 1)[c("input_n", "fitted_n", "excluded_n")],
                 c(input_n = 12, fitted_n = 6, excluded_n = 6))
    expect_equal(sample_model_measures(tt, 2)[c("input_n", "fitted_n", "excluded_n")],
                 c(input_n = 12, fitted_n = if (unequal) 5 else 6, excluded_n = if (unequal) 7 else 6))
  }
})

test_that("multiple imputations retain each source sample without summing", {
  d1 <- data.frame(y = seq_len(12) + sin(seq_len(12)), x = seq_len(12))
  d2 <- rbind(d1, data.frame(y = 13:15, x = 13:15))
  d2$y <- d2$y + cos(seq_len(15)) / 3
  d1$x[2] <- NA_real_
  d2$x[2] <- NA_real_
  fits <- list(glm(y ~ x, gaussian(), d1),
               glm(y ~ x, gaussian(), d2, subset = seq_len(15) <= 12))
  tt <- regtab(tt_mi(fits), stats = "n")
  ledger <- tt$meta$sample_accounting
  expect_type(ledger, "list")
  if (is.null(ledger)) return(invisible(NULL))
  expect_identical(ledger$populations$scope, c("imputation", "imputation"))
  expect_equal(ledger$populations$imputation, c(1, 2))
  expect_equal(ledger$populations$model, c(1, 1))
  for (k in 1:2) {
    expect_equal(sample_model_measures(tt, k)[c("input_n", "eligible_n", "fitted_n", "excluded_n")],
                 c(input_n = if (k == 1) 12 else 15, eligible_n = 11,
                   fitted_n = 11, excluded_n = if (k == 1) 1 else 4))
  }
  expect_equal(nrow(ledger$populations), 2)
  expect_false(any(ledger$measures$value == 22, na.rm = TRUE))
})

test_that("supplied coefficient rows and display filtering do not invent records", {
  x <- data.frame(term = c("x", "(Intercept)"), estimate = c(0.5, 1),
                  std.error = c(0.2, 0.3), statistic = c(2.5, 10 / 3),
                  p.value = c(0.02, 0.002))
  attr(x, "effect_scale") <- "Coef."
  attr(x, "inference_reference") <- "normal"
  attr(x, "glance") <- list(nobs = 100.5)
  tt <- regtab(x, stats = "n")
  counts <- sample_model_measures(tt)
  if (!is.null(counts)) {
    expect_equal(counts[["reported_n"]], 100.5)
    expect_true(all(is.na(counts[c("input_n", "eligible_n", "used_n", "fitted_n", "frame_n")])))
    status <- tt$meta$sample_accounting$measures
    expect_identical(status$status[status$metric %in% c("fitted_n", "frame_n")],
                     c("not_applicable", "not_applicable"))
    expect_identical(attr(as.data.frame(tt), "sample_accounting"), tt$meta$sample_accounting)
    expect_identical(regtab(x, keep = "x", stats = NULL)$meta$sample_accounting, tt$meta$sample_accounting)
  }
})

test_that("explicit unit case weights retain a known weight policy", {
  d <- data.frame(y = c(3, 5, 4, 8, 9, 12), x = seq_len(6), w = 1)
  tt <- regtab(lm(y ~ x, d, weights = w), stats = "n")
  counts <- sample_model_measures(tt)
  if (!is.null(counts)) {
    expect_identical(tt$meta$sample_accounting$populations$weight_type, "prior/case")
    expect_equal(counts[c("fitted_n", "weight_sum", "effective_n")],
                 c(fitted_n = 6, weight_sum = 6, effective_n = 6))
  }
})
