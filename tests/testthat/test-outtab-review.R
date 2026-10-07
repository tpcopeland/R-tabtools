ot_review_data <- function() data.frame(exposed = c(rep(1, 20), rep(0, 20)),
  event = as.numeric(seq_len(40) %% 5L %in% c(0L, 1L)),
  z = seq_len(40) %% 11L + seq_len(40) / 20)

test_that("outtab validates original binary column structure before slicing", {
  d <- data.frame(exposed = c(1, 1, 1, 0, 0, 0),
                  event = c(1, 0, 1, 1, 0, 0), panel = 1)
  for (column in c("exposed", "event", "panel")) {
    for (malformed in list(I(cbind(d[[column]], rep(9, 6))),
                           I(matrix(d[[column]], 6, 1)),
                           I(array(d[[column]], c(6, 1, 1))),
                           as.character(d[[column]]))) {
      bad <- d
      bad[[column]] <- malformed
      expect_error(outtab(bad, "event", "exposed", panels = "panel"),
        class = "tabtools_error_outtab", info = column)
      expect_error(outtab(bad, "event", "exposed", panels = "panel", subset = 1:3),
        class = "tabtools_error_outtab", info = paste(column, "selected rows"))
    }
  }
  wrong_length <- structure(list(exposed = rep(1, 12), event = d$event),
                            class = "data.frame", row.names = 1:6)
  expect_error(.ot_samples(wrong_length, "event", "exposed", NULL, NULL, NULL, NULL),
    class = "tabtools_error_outtab")
  expect_error(outtab(d, "event", "exposed", subset = matrix(TRUE, 6, 1)),
    class = "tabtools_error_outtab")
  # The structure always belongs to original records; value-domain checks
  # still deliberately ignore observations outside the selected population.
  outside <- d
  outside[6, c("exposed", "event", "panel")] <- 9
  kept <- .ot_samples(outside, "event", "exposed", "panel", NULL, NULL, 1:5)
  expect_identical(kept$base_ids, 1:5)
  expect_identical(kept$blocks[[1]]$counts, c(n1 = 3, e1 = 2, n0 = 2, e0 = 1))
  outside$exposed[6] <- NA_real_
  kept <- .ot_samples(outside, "event", "exposed", "panel", NULL, NULL, NULL)
  expect_identical(kept$base_ids, 1:5)
})

test_that("ordinary scale and poly glm frames keep strict transformation identity", {
  d <- ot_review_data()
  t <- outtab(d, "event", "exposed", models = list(~scale(z), ~poly(z, 2)),
              estimator = "poisson", vce = "model")
  formulas <- list(event ~ exposed + scale(z), event ~ exposed + poly(z, 2))
  for (k in 1:2) {
    oracle <- glm(formulas[[k]], data = d, family = poisson(),
      control = glm.control(epsilon = 1e-12, maxit = 100L), model = TRUE, x = TRUE, y = TRUE)
    fit <- t$meta$outtab$fits[[1]][[k]]
    expect_identical(fit$native_rc, 0)
    expect_identical(fit$ids, 1:40)
    expect_identical(fit$cc_ids, 1:40)
    expect_equal(fit$coefficient, unname(coef(oracle)["exposed"]), tolerance = 1e-12)
    expect_equal(fit$variance, unname(vcov(oracle)["exposed", "exposed"]), tolerance = 1e-12)
  }
  ordinary <- function(formula, data) glm(formula, data = data, family = poisson(),
                                         model = TRUE, x = TRUE, y = TRUE)
  for (spec in list(~scale(z), ~poly(z, 2))) {
    expect_identical(outtab(d, "event", "exposed", models = list(spec),
      estimator = ordinary, vce = "model")$stored$fits$rc, 0)
  }
  changed_scale <- function(formula, data) {
    fit <- ordinary(formula, data)
    x <- fit$model[["scale(z)"]]
    attr(x, "scaled:center") <- attr(x, "scaled:center") + 1
    fit$model[["scale(z)"]] <- x
    fit
  }
  changed_poly <- function(formula, data) {
    fit <- ordinary(formula, data)
    x <- fit$model[["poly(z, 2)"]]
    coefs <- attr(x, "coefs")
    coefs$alpha <- coefs$alpha + 1
    attr(x, "coefs") <- coefs
    fit$model[["poly(z, 2)"]] <- x
    fit
  }
  expect_error(outtab(d, "event", "exposed", models = list(~scale(z)),
    estimator = changed_scale), class = "tabtools_error_outtab_source")
  expect_error(outtab(d, "event", "exposed", models = list(~poly(z, 2)),
    estimator = changed_poly), class = "tabtools_error_outtab_source")
  changed_design <- function(formula, data) {
    fit <- ordinary(formula, data)
    fit$x[1, 1] <- fit$x[1, 1] + 1
    fit
  }
  changed_frame <- function(formula, data) {
    fit <- ordinary(formula, data)
    fit$model$event[1] <- 1 - fit$model$event[1]
    fit
  }
  for (bad in list(changed_design, changed_frame)) {
    expect_error(outtab(d, "event", "exposed", models = list(~scale(z)),
      estimator = bad), class = "tabtools_error_outtab_source")
  }
})

test_that("every declared formula validates before the minimum-events gate", {
  d <- ot_review_data()
  for (minimum in c(0L, 9L)) {
    expect_error(outtab(d, "event", "exposed", models = list(~missing_covariate),
      minevents = minimum), class = "tabtools_error_outtab_source")
    expect_error(outtab(d, "event", "exposed", models = list(~1, ~missing_covariate),
      minevents = minimum), class = "tabtools_error_outtab_source")
    expect_error(outtab(d, "event", "exposed", models = list(~exposed:z),
      minevents = minimum), class = "tabtools_error_outtab_capability")
  }
  never_fit <- function(formula, data) stop("Minimum-events gate must not invoke this fitter")
  skipped <- outtab(d, "event", "exposed", models = list(~z),
    minevents = 9L, estimator = never_fit)
  expect_identical(skipped$stored$fits$rc, -1)
  expect_identical(skipped$stored$fits$reason, "minimum_exposed_events")
  expect_true(is.na(skipped$stored$fits$N_cc))
  expect_length(skipped$meta$outtab$fits[[1]][[1]]$cc_ids, 0L)
})

test_that("public outtab ledgers use canonical scopes and retain original populations", {
  d <- ot_review_data()
  d$event_b <- d$event
  d$event_b[6] <- NA_real_
  d$exposed[7] <- NA_real_
  d$z[3] <- NA_real_
  d$p <- as.numeric(seq_len(40) <= 30)
  d$q <- as.numeric(seq_len(40) %% 2L == 1L)
  t <- outtab(d, c("event", "event_b"), "exposed", models = list(~1, ~z),
    panels = c("p", "q"), subset = 40:1)
  ledger <- t$meta$sample_accounting
  expect_identical(ledger$populations$scope, c("table", rep(c("variable", "model", "model"), 4)))
  expect_true(all(ledger$populations$scope %in% c("table", "group", "variable", "model", "imputation", "evaluation", "summary")))
  expect_identical(t$meta$outtab$selected_ids, 40:1)
  expect_identical(t$meta$outtab$base_ids, c(40:8, 6:1))
  expect_identical(t$meta$outtab$blocks[[1]]$ids, c(30:8, 6:1))
  expect_identical(t$meta$outtab$blocks[[2]]$ids, c(30:8, 5:1))
  value <- function(suffix, metric) {
    population <- ledger$populations$id[endsWith(ledger$populations$id, suffix)]
    expect_length(population, 1L)
    ledger$measures$value[ledger$measures$population_id == population & ledger$measures$metric == metric]
  }
  expect_equal(value("/base", "input_n"), 40)
  expect_equal(value("/base", "eligible_n"), 39)
  expect_equal(value("/eligible:1", "eligible_n"), 29)
  expect_equal(value("/eligible:2", "eligible_n"), 28)
  expect_equal(value("/fit:1:1", "observed_n"), 29)
  expect_equal(value("/fit:1:2", "observed_n"), 28)
  expect_equal(value("/fit:1:2", "fitted_n"), 28)
  expect_identical(t$meta$outtab$fits[[1]][[2]]$ids, setdiff(c(30:8, 6:1), 3L))
})
