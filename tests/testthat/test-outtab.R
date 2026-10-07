ot_truth <- function() data.frame(exposed = c(rep(1, 10), rep(0, 20)),
  event = c(rep(1, 4), rep(0, 6), rep(1, 2), rep(0, 18)))

test_that("modified Poisson RR and logit OR have independent literal variance controls", {
  d <- ot_truth()
  rr <- outtab(d, "event", "exposed")
  fit <- rr$meta$outtab$fits[[1L]][[1L]]
  expect_equal(fit$b, 4, tolerance = 1e-7)
  expect_equal(fit$coefficient, log(4), tolerance = 1e-7)
  expect_equal(fit$variance, 18 / 29, tolerance = 1e-7)
  expect_equal(c(fit$lb, fit$ub), 4 * exp(c(-1, 1) * qnorm(.975) * sqrt(18 / 29)), tolerance = 1e-7)
  expect_identical(fit$effect_scale, "RR")
  expect_identical(fit$exponentiation_count, 1L)
  expect_identical(rr$body[[2L]], "4/10 (40.0)")
  expect_identical(rr$body[[3L]], "2/20 (10.0)")
  expect_identical(rr$stored$table[1L, 1:4], c(n1 = 10, e1 = 4, n0 = 20, e0 = 2))
  odds <- outtab(d, "event", "exposed", estimator = "logit", ratiolabel = "RR")
  fit <- odds$meta$outtab$fits[[1L]][[1L]]
  expect_equal(fit$b, 6, tolerance = 1e-7)
  expect_equal(fit$variance, 35 / 36, tolerance = 1e-7)
  expect_identical(fit$effect_scale, "OR")
  poisson <- outtab(d, "event", "exposed", estimator = "poisson")
  expect_equal(poisson$meta$outtab$fits[[1L]][[1L]]$variance, 3 / 4, tolerance = 1e-7)
  expect_identical(poisson$meta$outtab$fits[[1L]][[1L]]$effect_scale, "IRR")
  d$id <- seq_len(nrow(d))
  clustered <- outtab(d, "event", "exposed", vce = "cluster", cluster = "id")
  expect_equal(clustered$meta$outtab$fits[[1L]][[1L]]$variance, 18 / 29, tolerance = 1e-7)
  expect_identical(clustered$stored$fits$N_clust, 30)
})

test_that("three populations retain original IDs and covariate loss is not estimator loss", {
  d <- ot_truth(); d$z <- rep(c(0, 1), 15); d$z[c(1L, 11L)] <- NA_real_
  d$event_b <- d$event; d$event_b[2L] <- NA_real_
  d$p <- 1; d$q <- rep(c(1, 0), 15); d$obs <- 1; d$obs[c(3L, 4L, 5L)] <- c(0, 2, NA)
  result <- outtab(d, c("event", "event_b"), "exposed", models = list(~1, ~z),
    panels = c("p", "q"), observed = c(event = "obs", event_b = "obs"))
  meta <- result$meta$outtab
  expect_identical(meta$base_ids, 1:30)
  expect_identical(meta$blocks[[1L]]$ids, c(1:2, 6:30))
  expect_identical(meta$blocks[[2L]]$ids, c(1L, 6:30))
  expect_identical(meta$blocks[[3L]]$ids, c(1L, seq.int(7L, 29L, by = 2L)))
  adjusted <- meta$fits[[1L]][[2L]]
  expect_identical(adjusted$N_cc, 25L)
  expect_identical(adjusted$N, 25L)
  expect_identical(adjusted$native_rc, 0)
  expect_identical(adjusted$cc_ids, setdiff(meta$blocks[[1L]]$ids, c(1L, 11L)))
  expect_identical(adjusted$ids, adjusted$cc_ids)
  expect_identical(result$stored$table[1L, 1:4], c(n1 = 7, e1 = 2, n0 = 20, e0 = 2))
  expect_identical(meta$display_rows, c(2L, 3L, 5L, 6L))
  expect_identical(meta$panel_rows, c(1L, 4L))
})

test_that("bounded function fits verify retained source and diagnose legitimate extra removal", {
  d <- ot_truth()
  fitter <- function(formula, data) glm(formula, data = data, family = binomial(), model = TRUE, x = TRUE, y = TRUE)
  expect_equal(unname(outtab(d, "event", "exposed", estimator = fitter)$stored$table[1L, "b1"]), 6, tolerance = 1e-7)
  reduce <- function(formula, data) fitter(formula, data[-10L, , drop = FALSE])
  r <- outtab(d, "event", "exposed", estimator = reduce)
  expect_identical(r$stored$fits$rc, -2)
  expect_identical(r$stored$fits$N_cc, 30)
  expect_identical(r$stored$fits$N, 29)
  expect_identical(r$meta$outtab$fits[[1L]][[1L]]$ids, c(1:9, 11:30))
  changed <- function(formula, data) { data$event[1L] <- 0; fitter(formula, data) }
  expect_error(outtab(d, "event", "exposed", estimator = changed), class = "tabtools_error_outtab_source")
  relabel <- function(formula, data) { rownames(data) <- rev(rownames(data)); fitter(formula, data) }
  expect_error(outtab(d, "event", "exposed", estimator = relabel), class = "tabtools_error_outtab_source")
  discard <- function(formula, data) { fit <- fitter(formula, data); fit$x <- NULL; fit }
  expect_error(outtab(d, "event", "exposed", estimator = discard), class = "tabtools_error_outtab_capability")
  weighted <- function(formula, data) glm(formula, data = data, family = poisson(), weights = rep(2, nrow(data)), model = TRUE, x = TRUE, y = TRUE)
  expect_error(outtab(d, "event", "exposed", estimator = weighted), class = "tabtools_error_outtab_capability")
  # Retained transformed values must match the snapshot from before callback
  # execution, even when a mutable formula environment still names the same term.
  transforms <- new.env(parent = baseenv())
  transforms$bump <- function(x) x
  model <- ~bump(z)
  environment(model) <- transforms
  d$z <- (1:30) %% 2
  mutate_transform <- function(formula, data) {
    transforms$bump <- function(x) x + 1
    fitter(formula, data)
  }
  expect_error(outtab(d, "event", "exposed", models = list(model), estimator = mutate_transform),
    class = "tabtools_error_outtab_source")
})

test_that("minimum events, real R failures, nonconvergence and boundaries have distinct states", {
  d <- ot_truth()
  minimum <- outtab(d, "event", "exposed", minevents = 5L)
  expect_identical(minimum$stored$fits$rc, -1)
  expect_true(is.na(minimum$stored$fits$N_cc))
  expect_identical(minimum$meta$outtab$raw_states[1L, 1L], "notest")
  expect_identical(outtab(d, "event", "exposed", minevents = 4L)$stored$fits$rc, 0)
  engine <- function(formula, data) stop(structure(list(message = "literal engine failure", call = NULL), class = c("ot_test_error", "error", "condition")))
  failed <- outtab(d, "event", "exposed", estimator = engine, failtext = "#:$literal")
  expect_true(is.na(failed$stored$fits$rc))
  expect_identical(failed$stored$fits$N_cc, 30)
  expect_identical(failed$body[[4L]], "R:$literal")
  expect_identical(failed$meta$outtab$fits[[1L]][[1L]]$error_class, c("ot_test_error", "error", "condition"))
  expect_identical(failed$meta$outtab$raw_states[1L, 1L], "empty")
  nonconv <- function(formula, data) { fit <- glm(formula, data = data, family = poisson(), model = TRUE, x = TRUE, y = TRUE); fit$converged <- FALSE; fit }
  expect_identical(outtab(d, "event", "exposed", estimator = nonconv)$stored$fits$rc, 430)
  d$event[d$exposed == 0] <- 0
  boundary <- outtab(d, "event", "exposed")
  expect_identical(boundary$stored$fits$rc, 459)
  expect_identical(boundary$stored$fits$reason, "boundary_events")
})

test_that("primary publication masks preserve raw fits and count cell occurrences", {
  d <- ot_truth()
  plain <- outtab(d, "event", "exposed", models = list(~1, ~1))
  masked <- outtab(d, "event", "exposed", models = list(~1, ~1), smallcells = 3L, masktext = "LOW")
  expect_identical(masked$stored$table, plain$stored$table)
  expect_identical(masked$stored$fits, plain$stored$fits)
  expect_identical(masked$body[[3L]], "LOW/20")
  expect_identical(unname(unlist(masked$body[4:5])), c("", ""))
  expect_identical(masked$stored$smallcells, list(threshold = 3L, mode = "primary", n_masked = 1L, n_linked = 2L))
  expect_true(all(masked$meta$outtab$publication_states == "masked"))
  expect_true(all(is.na(masked$meta$outtab$publication_ratios[[1L]])))
  expect_true(is.na(masked$meta$outtab$publication_counts[1L, "e0"]))
  expect_identical(unname(masked$meta$outtab$publication_counts[1L, "n0"]), 20)
  gated <- outtab(d, "event", "exposed", smallcells = 3L, minevents = 5L)
  expect_identical(gated$stored$fits$rc, -1)
  expect_identical(gated$body[[4L]], "")
  expect_identical(gated$meta$outtab$raw_states[1L, 1L], "notest")
})

test_that("primary thresholds include exact boundaries and never count zero cells", {
  d <- ot_truth()
  for (threshold in c(0L, 1L, 2L)) {
    tt <- outtab(d, "event", "exposed", smallcells = threshold)
    expect_identical(tt$stored$smallcells$n_masked, 0L)
    expect_identical(tt$stored$smallcells$n_linked, 0L)
  }
  tt <- outtab(d, "event", "exposed", smallcells = 5L)
  expect_identical(tt$stored$smallcells$n_masked, 2L)
  expect_identical(tt$stored$smallcells$n_linked, 1L)
  tt <- outtab(d, "event", "exposed", smallcells = 11L)
  expect_identical(tt$body[[2L]], "<11")
  expect_true(is.na(tt$meta$outtab$publication_counts[1L, "n1"]))
  expect_identical(tt$stored$smallcells$n_masked, 2L)
  d$event[d$exposed == 0] <- 0
  tt <- outtab(d, "event", "exposed", smallcells = 3L)
  expect_identical(tt$stored$smallcells$n_masked, 0L)
  expect_identical(tt$stored$smallcells$n_linked, 0L)
  expect_identical(tt$body[[3L]], "0/20 (0.0)")
})

test_that("duplicate outcome/specification occurrences keep separate fit and source maps", {
  d <- ot_truth()
  tt <- outtab(d, c("event", "event"), "exposed", models = rep(list(~1), 11L),
    modellabels = rep("same label", 11L))
  expect_identical(dim(tt$stored$table), c(2L, 48L))
  expect_identical(nrow(tt$stored$fits), 22L)
  expect_identical(tt$meta$outtab$blocks[[1L]]$outcome_index, 1L)
  expect_identical(tt$meta$outtab$blocks[[2L]]$outcome_index, 2L)
  expect_identical(tt$meta$outtab$fit_occurrence[2L, ], 12:22)
  expect_identical(tt$stored$fits$model, rep(1:11, 2L))
  expect_identical(tt$meta$outtab$slot_role, "specification")
  expect_true(all(is.na(tt$cols$model)))
  d$p <- 0
  empty <- outtab(d, "event", "exposed", panels = "p", minevents = 1L)
  expect_identical(empty$stored$table[1L, 1:4], c(n1 = 0, e1 = 0, n0 = 0, e0 = 0))
  expect_identical(empty$stored$fits$rc, -1)
  expect_identical(empty$meta$outtab$display_publication_states[1L, 1L], "")
})

test_that("literal formatting, capability refusals and destinations respect boundaries", {
  d <- ot_truth()
  separator <- paste(rep("long literal separator", 4), collapse = " ")
  tt <- outtab(d, "event", "exposed", sep = separator)
  expect_gt(nchar(tt$body[[4L]]), 40L)
  expect_true(any(grepl(separator, capture.output(print(tt)), fixed = TRUE)))
  expect_error(outtab(d, "event", "exposed", eform = TRUE), class = "tabtools_error_outtab_scale")
  expect_error(outtab(d, "event", "exposed", cformat = "%4.2f", digits = 3), class = "tabtools_error_format_conflict")
  expect_error(outtab(d, "event", "exposed", cformat = "%4,2f"), class = "tabtools_error_format_separator")
  expect_error(outtab(d, "event", "exposed", estimator = "probit"), class = "tabtools_error_outtab_capability")
  expect_error(outtab(d, "event", "exposed", models = list(event ~ 0 + exposed)), class = "tabtools_error_outtab_capability")
  expect_error(outtab(d, "event", "exposed", models = list(~exposed:z)), class = "tabtools_error_outtab")
  # Only the selected workbook alias is evaluated by destination validation.
  path <- withr::local_tempfile(fileext = ".xlsx")
  expect_silent(suppressMessages(outtab(d, "event", "exposed", xlsx = path, excel = 12)))
  expect_true(file.exists(path))
  expect_error(tt_flat(tt, keyed = TRUE), class = "tabtools_error_flat")
  expect_s3_class(as.data.frame(tt), "data.frame")
})


test_that("explicit built-in convergence controls remain caller-owned", {
  d <- ot_truth()
  controls <- list(control = glm.control(maxit = 1L))
  before <- serialize(controls, NULL)
  limited <- outtab(d, "event", "exposed", estimator = "logit", estimator_args = controls)
  expect_identical(limited$stored$fits$rc, 430)
  expect_identical(serialize(controls, NULL), before)
  expect_equal(outtab(d, "event", "exposed", estimator = "logit")$meta$outtab$fits[[1L]][[1L]]$variance,
    35/36, tolerance = 1e-7)
})
