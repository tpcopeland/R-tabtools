# Sample counts supplement the existing rates and weight diagnostics.
rw_sample_measure <- function(s, metric, scope = "table", index = 1L) {
  ids <- s$populations$id[s$populations$scope == scope]
  expect_gte(length(ids), index)
  rows <- s$measures[s$measures$population_id == ids[index] & s$measures$metric == metric, ]
  expect_equal(nrow(rows), 1L)
  rows
}

rw_sample_data <- function() {
  data.frame(t = c(2, NA, 3, 1, 4, 5, 2, 2, 3), entry = c(0, 0, NA, 1, 0, 0, 0, 0, 0),
    event = c(1, 1, NA, 1, NA, 0, 1, NA, 1), group = c("A", "A", "A", "B", NA, "B", "B", "A", "A"),
    fw = c(1, 1, 1, 1, 1, 0, 2, 1, NA))
}

test_that("rate accounting distinguishes eligible records and observed events", {
  d <- rw_sample_data(); before <- d
  r <- tt_rates(d, "t", "event", by = "group", entry = "entry", fweight = "fw", float_time = FALSE)
  s <- attr(r, "sample_accounting", exact = TRUE)
  expect_type(s, "list")
  eligible <- !is.na(d$t) & !is.na(d$entry) & d$t > d$entry & !is.na(d$group) & !is.na(d$fw) & d$fw > 0
  for (metric in c("eligible_n", "used_n")) expect_equal(rw_sample_measure(s, metric)$value, sum(eligible))
  expect_equal(rw_sample_measure(s, "input_n")$value, nrow(d))
  expect_equal(rw_sample_measure(s, "observed_n")$value, sum(eligible & !is.na(d$event)))
  expect_equal(rw_sample_measure(s, "missing_n")$value, sum(eligible & is.na(d$event)))
  expect_equal(rw_sample_measure(s, "excluded_n")$value, sum(!eligible))
  expect_equal(rw_sample_measure(s, "weight_sum")$value, sum(d$fw[eligible]))
  expect_equal(rw_sample_measure(s, "effective_n")$value, sum(d$fw[eligible])^2 / sum(d$fw[eligible]^2))
  expect_equal(rw_sample_measure(s, "used_n", "group", 1)$value, 2)
  expect_equal(rw_sample_measure(s, "observed_n", "group", 1)$value, 1)
  expect_equal(r$D, c(1, 2)); expect_equal(r$Y, c(4, 4))
  retained <- s$exclusions[s$exclusions$stage == "retained" & s$exclusions$population_id == s$populations$id[1], ]
  expect_identical(retained$reason, "missing_event_retained_as_no_event")
  expect_equal(retained$n, 1)
  excluded <- s$exclusions[s$exclusions$stage == "input_to_eligible" & s$exclusions$population_id == s$populations$id[1], ]
  expect_equal(sum(excluded$n), sum(!eligible))
  expect_equal(excluded$n[excluded$reason == "missing_weight"], 1)
  expect_equal(excluded$n[excluded$reason == "zero_frequency_weight"], 1)
  expect_identical(d, before)
})

test_that("rate missing-group inclusion and zero events preserve accounting", {
  d <- rw_sample_data()
  r <- tt_rates(d, "t", "event", by = "group", entry = "entry", fweight = "fw", missing = TRUE, float_time = FALSE)
  s <- attr(r, "sample_accounting", exact = TRUE)
  expect_equal(rw_sample_measure(s, "used_n")$value, 4)
  expect_equal(rw_sample_measure(s, "observed_n")$value, 2)
  expect_equal(rw_sample_measure(s, "missing_n")$value, 2)
  expect_equal(rw_sample_measure(s, "used_n", "group", 3)$value, 1)
  expect_equal(tail(r$D, 1), 0)
  expect_equal(tail(r$Y, 1), 4)
  expect_true(is.na(tail(r$Lower, 1)))
  expect_true(any(s$exclusions$reason == "missing_group_retained" & s$exclusions$n == 1))
})

test_that("stratetab preserves distinct source populations and supplied counts stay unknown", {
  d <- rw_sample_data()
  r <- tt_rates(d, "t", "event", by = "group", entry = "entry", fweight = "fw", float_time = FALSE)
  tab <- stratetab(list(r, r), outcomes = 1)
  s <- tab$meta$sample_accounting
  expect_equal(sum(s$populations$scope == "table"), 2)
  expect_false(anyDuplicated(s$populations$id) > 0)
  expect_equal(s$measures$value[s$measures$metric == "input_n" & s$measures$population_id %in% s$populations$id[s$populations$scope == "table"]], c(9, 9))
  supplied <- data.frame(category = "A", D = 3, Y = 8, Rate = 3/8, Lower = .1, Upper = .9)
  q <- tt_rates(supplied, level = .95)
  sq <- attr(q, "sample_accounting", exact = TRUE)
  counts <- sq$measures[sq$measures$metric %in% c("input_n", "eligible_n", "observed_n", "used_n", "missing_n", "excluded_n"), ]
  expect_true(all(is.na(counts$value)))
  expect_true(all(counts$status == "unavailable"))
  expect_true(all(nzchar(counts$reason)))
  st <- stratetab(q)
  expect_true(all(st$meta$sample_accounting$measures$status[st$meta$sample_accounting$measures$metric == "used_n"] == "unavailable"))
})

test_that("weight accounting separates exclusion reasons and keeps zero records", {
  d <- data.frame(w = c(0, 1, NA, 2, 3, 0), g = c("A", "A", "A", NA, "B", "B"), p = c(1, 1, 1, 1, NA, 2))
  expect_warning(t <- wttab(d, "w", by = "g", period = "p"), "Dropped 3")
  s <- t$meta$sample_accounting
  for (metric in c("eligible_n", "observed_n", "used_n", "reported_n")) expect_equal(rw_sample_measure(s, metric)$value, 3)
  expect_equal(rw_sample_measure(s, "input_n")$value, 6)
  expect_equal(rw_sample_measure(s, "zero_weight_n")$value, 2)
  expect_equal(rw_sample_measure(s, "missing_n")$value, 1)
  expect_equal(rw_sample_measure(s, "excluded_n")$value, 3)
  expect_equal(rw_sample_measure(s, "weight_sum")$value, 1)
  expect_equal(rw_sample_measure(s, "effective_n")$value, 1)
  expect_equal(t$stored$N, 3)
  expect_equal(s$exclusions$n, c(1, 1, 1))
  group_ids <- s$populations$id[s$populations$scope == "group"]
  ess <- s$measures[s$measures$metric == "effective_n" & s$measures$population_id %in% group_ids, ]
  expect_equal(sum(ess$status == "unavailable"), 4)
  expect_true(all(is.na(ess$value[ess$status == "unavailable"])))
})

test_that("weight duck inputs and truncation retain raw-weight and ESS evidence", {
  w <- c(0, 1, 2, 8)
  vector <- wttab(w, trunc = c(.25, .75))
  duck <- wttab(list(ipw.weights = w), trunc = c(.25, .75))
  expect_identical(vector$stored$stats, duck$stored$stats)
  expect_identical(vector$meta$sample_accounting, duck$meta$sample_accounting)
  s <- vector$meta$sample_accounting
  expect_match(rw_sample_measure(s, "input_n")$basis, "provided weight-vector")
  expect_equal(rw_sample_measure(s, "weight_sum")$value, sum(w))
  expect_equal(rw_sample_measure(s, "effective_n")$value, sum(w)^2 / sum(w^2))
  cells <- s$populations$id[s$populations$scope == "group"]
  expect_equal(s$measures$value[s$measures$metric == "effective_n" & s$measures$population_id %in% cells], vector$stored$stats$ess)
  zero <- wttab(c(0, 0))$meta$sample_accounting
  expect_equal(rw_sample_measure(zero, "used_n")$value, 2)
  expect_equal(rw_sample_measure(zero, "weight_sum")$value, 0)
  expect_identical(rw_sample_measure(zero, "effective_n")$status, "unavailable")
})

test_that("rate zero-weight counts describe input records without overlapping exclusions", {
  d <- data.frame(t = c(NA, 2, 3, 4, 5), e = c(1, 1, NA, 0, 1),
                  g = c("A", "A", "A", "B", "B"), w = c(0, 0, 2, 0, 3))
  r <- tt_rates(d, "t", "e", by = "g", fweight = "w", float_time = FALSE)
  s <- attr(r, "sample_accounting", exact = TRUE)
  expect_equal(rw_sample_measure(s, "zero_weight_n")$value, sum(d$w == 0))
  expect_match(rw_sample_measure(s, "zero_weight_n")$basis, "original input")
  expect_equal(rw_sample_measure(s, "zero_weight_n", "group", 1)$value, 2)
  expect_equal(rw_sample_measure(s, "zero_weight_n", "group", 2)$value, 1)
  expect_equal(rw_sample_measure(s, "used_n")$value, 2)
  e <- s$exclusions[s$exclusions$population_id == s$populations$id[1] & s$exclusions$stage == "input_to_eligible", ]
  expect_equal(e$n[e$reason == "missing_exit"], 1)
  expect_equal(e$n[e$reason == "zero_frequency_weight"], 2)
  expect_equal(sum(e$n), 3)
  expect_equal(r$D, c(0, 3)); expect_equal(r$Y, c(6, 15))
})

test_that("rate standardization preserves and validates a supplied source ledger", {
  r <- tt_rates(rw_sample_data(), "t", "event", by = "group", entry = "entry", fweight = "fw", float_time = FALSE)
  q <- tt_rates(r, level = .95)
  expect_identical(lapply(q, identity), lapply(r, identity))
  expect_identical(attr(q, "sample_accounting", exact = TRUE), attr(r, "sample_accounting", exact = TRUE))
  expect_identical(stratetab(q)$meta$sample_accounting, stratetab(r)$meta$sample_accounting)
  malformed <- r
  attr(malformed, "sample_accounting") <- list(version = 99L)
  expect_error(tt_rates(malformed), class = "tabtools_error_sample_accounting")
  expect_error(stratetab(malformed), class = "tabtools_error_sample_accounting")
})

test_that("aggregated rate weights are explicitly unknown rather than unweighted", {
  q <- tt_rates(data.frame(D = 3, Y = 8, Rate = .375, Lower = .1, Upper = .9), level = .95)
  s <- attr(q, "sample_accounting", exact = TRUE)
  expect_identical(s$populations$weight_type, "unknown")
  for (metric in c("zero_weight_n", "weight_sum", "effective_n")) {
    row <- rw_sample_measure(s, metric, "summary")
    expect_identical(row$status, "unavailable")
    expect_true(is.na(row$value))
    expect_match(row$reason, "aggregated")
  }
  expect_identical(rw_sample_measure(s, "fitted_n", "summary")$status, "not_applicable")
  expect_identical(rw_sample_measure(s, "frame_n", "summary")$status, "not_applicable")
})
