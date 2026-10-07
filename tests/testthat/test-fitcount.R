test_that("explicit capture counts records, events and event-bearing people independently", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6,
    ev = c(1, 0, 2, 0, 1, 0), id = c("a", "a", "b", "c", "c", "d"), time = c(1, 2, 1, 3, 2, 4))
  fit <- lm(y ~ x, d)
  record <- tt_fitcount(fit, "ev", "id", "time", data = d)
  expect_identical(record$counts, c(obs = 6, events = 4, people = 4, people_ev = 3, exposure = 13))
  expect_identical(record$sample$row_ids, as.character(1:6))
  expect_identical(record$sample$source_origin, "caller_declared")
  expect_identical(record$snapshot$compatibility, "initial_only")
  expect_identical(fit$model, model.frame(fit))
})

test_that("retained source capture does not evaluate a later source", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- glm(y ~ x, data = d, family = gaussian())
  d$ev <- 100
  record <- tt_fitcount(fit, "ev")
  expect_equal(record$counts[["events"]], 4)
  expect_identical(record$sample$source_origin, "retained_fit")
  missing <- lm(y ~ x, fit$data)
  expect_error(tt_fitcount(missing, "ev"), class = "tabtools_error_fitcount")
})

test_that("model FALSE capture survives RDS and deleted caller data", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d, model = FALSE)
  record <- tt_fitcount(fit, "ev", data = d)
  path <- withr::local_tempfile()
  saveRDS(record, path)
  rm(d)
  saved <- readRDS(path)
  expect_true(.fc_validate(saved, fit))
  prepared <- .fc_prepare(list(fit), saved, NULL)$fits[[1L]]
  expect_equal(nrow(prepared$model), 6)
  expect_identical(saved$counts[c("obs", "events")], c(obs = 6, events = 4))
  table <- regtab(fit, fitcounts = saved, stats = c("obs", "events"))
  expect_equal(table$stored$obs_1, 6)
  expect_equal(table$stored$events_1, 4)
  expect_null(fit$model)
})

test_that("every available captured field is exact at reuse including tiny changes", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d, x = TRUE, y = TRUE)
  record <- tt_fitcount(fit, "ev", data = d)
  variants <- list(
    function(f) { f$model$x[1L] <- f$model$x[1L] + 1e-12; f },
    function(f) { f$y[1L] <- f$y[1L] + 1e-12; f },
    function(f) { f$x[1L, 2L] <- f$x[1L, 2L] + 1e-12; f },
    function(f) { f$coefficients[1L] <- f$coefficients[1L] + 1e-12; f },
    function(f) { f$fitted.values[1L] <- f$fitted.values[1L] + 1e-12; f },
    function(f) { names(f$fitted.values)[1L] <- "changed"; f },
    function(f) { f$call$data <- quote(other); f },
    function(f) { f$contrasts <- list(x = "contr.sum"); f })
  for (mutate in variants) expect_error(.fc_validate(record, mutate(fit)), class = "tabtools_error_fitcount")
  expect_true(.fc_validate(record, fit))
})

test_that("frozen source, count and state records cannot be changed unnoticed", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  altered <- record
  altered$snapshot$source$ev[1L] <- 9
  expect_error(.fc_validate(altered, fit), class = "tabtools_error_fitcount")
  altered <- record
  altered$counts[["events"]] <- 4 + 1e-12
  expect_error(.fc_validate(altered, fit), class = "tabtools_error_fitcount")
  altered <- record
  altered$states$status[1L] <- "constrained"
  expect_error(.fc_validate(altered, fit), class = "tabtools_error_fitcount")
})

test_that("explicit source compatibility does not authenticate unstored historic predictors", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d, model = FALSE, x = FALSE)
  declared <- d
  declared$ev <- rev(d$ev)
  record <- tt_fitcount(fit, "ev", data = declared)
  expect_identical(record$sample$source_origin, "caller_declared")
  expect_identical(record$snapshot$source$ev, rev(d$ev))
  expect_false(record$evidence$available[record$evidence$field == "model"])
  expect_true(.fc_validate(record, fit))
  incompatible <- d
  incompatible$x[1L] <- 100
  expect_error(tt_fitcount(fit, "ev", data = incompatible), class = "tabtools_error_fitcount")
})

test_that("direct retained predictors refuse even compatibility-preserving tiny changes", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d, x = TRUE)
  changed <- d
  changed$x[1L] <- changed$x[1L] + 1e-12
  expect_error(tt_fitcount(fit, "ev", data = changed), class = "tabtools_error_fitcount")
  expect_identical(tt_fitcount(fit, "ev", data = d[6:1, ])$counts[["events"]], 4)
})

test_that("a new compatible declared source does not prove unstored historical predictor equality", {
  d <- data.frame(y = c(2, 1, 4, 5, 3, 8, 6, 9, 8, 11), x = 1:10,
    z = c(1, 0, 2, 1, 3, 0, 1, 4, 2, 3), ev = rep(c(0, 1), 5))
  fit <- lm(y ~ x + z, d, model = FALSE, x = FALSE)
  b <- coef(fit)
  expect_gt(abs(b[["z"]]), 1e-6)
  declared <- d
  declared$x <- declared$x + .125
  declared$z <- declared$z - .125 * b[["x"]] / b[["z"]]
  record <- tt_fitcount(fit, "ev", data = declared)
  expect_identical(record$snapshot$source$x, declared$x)
  expect_false(identical(record$snapshot$source$x, d$x))
  expect_identical(record$sample$source_origin, "caller_declared")
  expect_true(.fc_validate(record, fit))
  retained <- lm(y ~ x + z, d, model = TRUE, x = TRUE)
  expect_error(tt_fitcount(retained, "ev", data = declared), class = "tabtools_error_fitcount")
})

test_that("count validation applies to accepted records and refuses guessed identifiers", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7, NA), x = 1:7,
    ev = c(1, 0, 2, 0, 1, 0, NA), id = c("a", "a", "b", "c", "c", "d", ""), time = c(1, 2, 1, 3, 2, 4, -1))
  fit <- lm(y ~ x, d)
  expect_identical(tt_fitcount(fit, "ev", "id", "time", data = d)$counts[["events"]], 4)
  for (value in c(-1, .5, NA, Inf)) {
    bad <- d; bad$ev[1L] <- value
    expect_error(tt_fitcount(fit, "ev", data = bad), class = "tabtools_error_fitcount")
  }
  bad <- d; bad$id[1L] <- ""
  expect_error(tt_fitcount(fit, "ev", "id", data = bad), class = "tabtools_error_fitcount")
  bad <- d; bad$time[1L] <- Inf
  expect_error(tt_fitcount(fit, "ev", exposure = "time", data = bad), class = "tabtools_error_fitcount")
  expect_error(tt_fitcount(fit, "unknown", data = d), class = "tabtools_error_fitcount")
  noids <- fit; noids$model <- NULL; names(noids$fitted.values) <- NULL
  expect_error(tt_fitcount(noids, "ev", data = d), class = "tabtools_error_fitcount")
})

test_that("case weight semantics are explicit and counts remain unweighted", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0), w = c(0, 2, 3, 1, 4, 2))
  fit <- lm(y ~ x, d, weights = w)
  expect_error(tt_fitcount(fit, "ev", data = d), class = "tabtools_error_fitcount")
  for (type in c("fweight", "iweight", "unweighted")) expect_error(tt_fitcount(fit, "ev", data = d, weight_type = type), class = "tabtools_error_fitcount")
  record <- tt_fitcount(fit, "ev", data = d, weight_type = "aweight")
  expect_identical(record$counts[c("obs", "events")], c(obs = 5, events = 3))
  expect_equal(record$sample$excluded_zero_weight, 1)
  expect_identical(record$sample$row_ids, as.character(2:6))
  changed <- fit; changed$weights[2L] <- changed$weights[2L] + 1e-12
  expect_error(.fc_validate(record, changed), class = "tabtools_error_fitcount")
})

test_that("duplicate captures keep numeric occurrence order and distinct declared records", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0))
  fit <- lm(y ~ x, d, model = FALSE)
  a <- tt_fitcount(fit, "ev", data = d)
  d$ev <- c(0, 2, 0, 0, 1, 1)
  b <- tt_fitcount(fit, "ev", data = d)
  out <- .fc_prepare(list(fit, fit, fit), list(a, a, b), NULL)
  expect_identical(out$identity$model_index, 1:3)
  expect_identical(out$identity$repeated_fit, c(FALSE, TRUE, TRUE))
  expect_identical(out$identity$repeated_record, c(FALSE, TRUE, FALSE))
  expect_identical(out$records[[3L]]$snapshot$source$ev, d$ev)
  expect_error(.fc_prepare(list(fit, fit), list(a), NULL), class = "tabtools_error_fitcount")
})

test_that("changing reporting VCE leaves the captured fitting covariance identity intact", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7, 6, 5), x = 1:8, ev = c(1, 0, 2, 0, 1, 0, 0, 1))
  fit <- lm(y ~ x, d)
  record <- tt_fitcount(fit, "ev", data = d)
  table <- regtab(fit, fitcounts = record, vce = diag(c(.5, .2)), stats = "events")
  expect_equal(table$stored$events_1, 5)
  expect_true(.fc_validate(record, fit))
})


test_that("count inputs are per-record vectors before positive-weight subsetting", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6,
    ev = c(1, 0, 2, 0, 1, 0), id = c("a", "a", "b", "c", "c", "d"),
    time = c(1, 2, 1, 3, 2, 4), w = c(0, 2, 3, 1, 4, 2))
  unweighted <- lm(y ~ x, d)
  weighted <- lm(y ~ x, d, weights = w)
  for (column in c("ev", "id", "time")) {
    for (shape in c("matrix", "array", "data.frame", "list")) {
      bad <- d
      values <- d[[column]]
      bad[[column]] <- switch(shape,
        matrix = I(cbind(values, values)),
        array = I(array(rep(values, 2L), dim = c(6L, 1L, 2L))),
        data.frame = data.frame(first = values, second = values),
        list = as.list(values))
      expect_error(tt_fitcount(unweighted, "ev", "id", "time", data = bad),
        class = "tabtools_error_fitcount", info = paste(column, shape, "unweighted"))
      expect_error(tt_fitcount(weighted, "ev", "id", "time", data = bad, weight_type = "aweight"),
        class = "tabtools_error_fitcount", info = paste(column, shape, "weighted"))
    }
  }
  record <- tt_fitcount(unweighted, "ev", "id", "time", data = d)
  expect_identical(record$counts, c(obs = 6, events = 4, people = 4, people_ev = 3, exposure = 13))
  weighted_record <- tt_fitcount(weighted, "ev", "id", "time", data = d, weight_type = "aweight")
  expect_identical(weighted_record$counts, c(obs = 5, events = 3, people = 4, people_ev = 2, exposure = 12))
  expect_identical(weighted_record$sample$row_ids, as.character(2:6))
  expect_identical(weighted_record$sample$excluded_zero_weight, 1L)
  logical_source <- d; logical_source$ev <- d$ev > 0
  expect_identical(tt_fitcount(unweighted, "ev", "id", "time", data = logical_source)$counts[["events"]], 3)
})
