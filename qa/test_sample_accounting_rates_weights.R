library(testthat)
library(tabtools)

rw_qa_measure <- function(s, metric, scope = "table", index = 1L) {
  ids <- s$populations$id[s$populations$scope == scope]
  expect_gte(length(ids), index)
  rows <- s$measures[s$measures$population_id == ids[index] & s$measures$metric == metric, ]
  expect_equal(nrow(rows), 1L)
  rows
}

test_that("installed rates preserve record accounting separately for each outcome", {
  d <- data.frame(t = c(5, 2, NA, 0, 3, 2, 7, 1), a = c(1, NA, 1, 0, 1, 0, NA, 1),
    b = c(NA, 1, 1, 0, 0, NA, 1, 1), g = factor(c("B", "A", "A", "A", "B", "A", "B", NA)),
    w = c(2, 1, 1, 1, 1, 0, 3, 1), unrelated = NA_real_)
  before <- d
  mask <- !is.na(d$t) & d$t > 0 & !is.na(d$g) & !is.na(d$w) & d$w > 0
  rates <- lapply(c("a", "b"), function(outcome) tt_rates(d, "t", outcome, by = "g", fweight = "w", float_time = FALSE))
  for (k in seq_along(rates)) {
    s <- attr(rates[[k]], "sample_accounting", exact = TRUE)
    outcome <- c("a", "b")[k]
    expect_equal(rw_qa_measure(s, "input_n")$value, nrow(d))
    expect_equal(rw_qa_measure(s, "eligible_n")$value, sum(mask))
    expect_equal(rw_qa_measure(s, "used_n")$value, sum(mask))
    expect_equal(rw_qa_measure(s, "observed_n")$value, sum(mask & !is.na(d[[outcome]])))
    expect_equal(rw_qa_measure(s, "missing_n")$value, sum(mask & is.na(d[[outcome]])))
    expect_equal(rw_qa_measure(s, "weight_sum")$value, sum(d$w[mask]))
    expect_equal(rw_qa_measure(s, "effective_n")$value, sum(d$w[mask])^2 / sum(d$w[mask]^2))
    for (j in seq_len(nrow(rates[[k]]))) {
      sel <- mask & d$g == rates[[k]]$g[j]
      expect_equal(rw_qa_measure(s, "used_n", "group", j)$value, sum(sel))
      expect_equal(rates[[k]]$D[j], sum(d$w[sel] * (!is.na(d[[outcome]][sel]) & d[[outcome]][sel] != 0)))
      expect_equal(rates[[k]]$Y[j], sum(d$w[sel] * d$t[sel]))
    }
  }
  t <- stratetab(rates, outcomes = 2)
  s <- t$meta$sample_accounting
  ids <- s$populations$id[s$populations$scope == "table"]
  expect_equal(length(ids), 2)
  expect_identical(s$populations$variable[s$populations$scope == "table"], c("a", "b"))
  expect_equal(s$measures$value[s$measures$metric == "observed_n" & s$measures$population_id %in% ids], c(2, 3))
  expect_identical(attr(as.data.frame(t), "sample_accounting", exact = TRUE), s)
  expect_identical(d, before)
})

test_that("supplied summary and legacy blocks never manufacture subject counts", {
  supplied <- data.frame(g = c("A", "B"), D = c(0, 40), Y = c(5, 80),
    Rate = c(0, .5), Lower = c(NA, .3), Upper = c(NA, .7))
  r <- tt_rates(supplied, level = .95)
  s <- attr(r, "sample_accounting", exact = TRUE)
  for (metric in c("input_n", "eligible_n", "observed_n", "used_n", "missing_n", "excluded_n")) {
    row <- rw_qa_measure(s, metric, "summary")
    expect_identical(row$status, "unavailable")
    expect_true(is.na(row$value))
    expect_match(row$reason, "aggregated")
  }
  attr(r, "sample_accounting") <- NULL
  t <- stratetab(list(r, r), outcomes = 1)
  expect_equal(nrow(t$meta$sample_accounting$populations), 2)
  expect_true(all(t$meta$sample_accounting$measures$status == "unavailable"))
  expect_equal(t$meta$rate_rows$events, rep(c(0, 40), 2))
})

test_that("provided weight objects retain vector provenance and sequential exclusions", {
  d <- data.frame(w = c(0, 1, NA, 2, 3, 0), group = c("A", "A", NA, NA, "B", "B"),
                  period = c(1, 1, NA, 1, NA, 2))
  object <- structure(list(weights = d$w, s.weights = c(1, 2, 1, 1, 1, 1)), class = "weightitMSM")
  before <- object
  expect_warning(t <- wttab(object, data = d, by = "group", period = "period"), "Dropped 3")
  s <- t$meta$sample_accounting
  product <- d$w * object$s.weights
  mask <- !is.na(product) & !is.na(d$group) & !is.na(d$period)
  expect_equal(rw_qa_measure(s, "input_n")$value, length(product))
  expect_match(rw_qa_measure(s, "input_n")$basis, "not the original fitted cohort")
  expect_equal(rw_qa_measure(s, "used_n")$value, sum(mask))
  expect_equal(rw_qa_measure(s, "zero_weight_n")$value, sum(product[mask] == 0))
  expect_equal(rw_qa_measure(s, "weight_sum")$value, sum(product[mask]))
  expect_equal(rw_qa_measure(s, "effective_n")$value, sum(product[mask])^2 / sum(product[mask]^2))
  expect_equal(s$exclusions$n, c(1, 1, 1))
  expect_identical(object, before)
  expect_true(t$stored$s_weights)
  ipw <- list(ipw.weights = product)
  expect_warning(q <- wttab(ipw, data = d, by = "group", period = "period"), "Dropped 3")
  expect_identical(q$stored$stats, t$stored$stats)
  expect_identical(q$meta$sample_accounting, s)
})

test_that("weight sum overflow and empty or zero cells have explicit availability", {
  base <- c(0, 1, 2, 8)
  for (scale in c(1e-200, 1, 1e200)) {
    t <- wttab(base * scale, trunc = c(.25, .75))
    s <- t$meta$sample_accounting
    expect_equal(rw_qa_measure(s, "effective_n")$value, sum(base)^2 / sum(base^2), tolerance = 1e-12)
    expect_equal(rw_qa_measure(s, "weight_sum")$value / scale, sum(base), tolerance = 1e-12)
    cells <- s$populations$id[s$populations$scope == "group"]
    expect_equal(s$measures$value[s$measures$metric == "effective_n" & s$measures$population_id %in% cells], t$stored$stats$ess)
  }
  overflow <- wttab(c(1e308, 1e308))$meta$sample_accounting
  expect_identical(rw_qa_measure(overflow, "weight_sum")$status, "unavailable")
  expect_true(is.na(rw_qa_measure(overflow, "weight_sum")$value))
  expect_equal(rw_qa_measure(overflow, "effective_n")$value, 2)
  zeros <- wttab(c(0, 0), by = c("A", "B"), period = c(1, 2))
  s <- zeros$meta$sample_accounting
  expect_equal(rw_qa_measure(s, "used_n")$value, 2)
  expect_identical(rw_qa_measure(s, "effective_n")$status, "unavailable")
  expect_true(all(is.na(zeros$stored$stats$ess)))
  cells <- s$populations$id[s$populations$scope == "group"]
  expect_equal(s$measures$value[s$measures$metric == "reported_n" & s$measures$population_id %in% cells], zeros$stored$stats$n)
  expect_true(any(zeros$stored$stats$n == 0))
})

test_that("input zero-weight diagnostics remain distinct from sequential rate exclusions", {
  d <- data.frame(t = c(NA, 2, 3, 4, 5), event = c(1, 1, NA, 0, 1),
                  group = factor(c("B", "B", "B", "A", "A")), w = c(0, 0, 2, 0, 3))
  before <- d
  r <- tt_rates(d, "t", "event", by = "group", fweight = "w", float_time = FALSE)
  s <- attr(r, "sample_accounting", exact = TRUE)
  expect_equal(rw_qa_measure(s, "zero_weight_n")$value, sum(!is.na(d$w) & d$w == 0))
  expect_equal(rw_qa_measure(s, "used_n")$value, sum(!is.na(d$t) & d$t > 0 & d$w > 0))
  for (k in seq_len(nrow(r))) {
    mask <- d$group == r$group[k]
    expect_equal(rw_qa_measure(s, "input_n", "group", k)$value, sum(mask))
    expect_equal(rw_qa_measure(s, "zero_weight_n", "group", k)$value, sum(mask & d$w == 0))
  }
  e <- s$exclusions[s$exclusions$population_id == s$populations$id[1] & s$exclusions$stage == "input_to_eligible", ]
  expect_equal(e$n[e$reason == "zero_frequency_weight"], sum(!is.na(d$t) & d$t > 0 & d$w == 0))
  expect_equal(sum(e$n), 3)
  expect_identical(d, before)
})

test_that("installed rate roundtrips carry validated ledgers and unknown summaries stay unknown", {
  r <- tt_rates(data.frame(t = c(2, 3, 4), event = c(1, NA, 0), w = c(0, 2, 1)),
                "t", "event", fweight = "w", float_time = FALSE)
  q <- tt_rates(r, level = .95)
  expect_identical(attr(q, "sample_accounting", exact = TRUE), attr(r, "sample_accounting", exact = TRUE))
  expect_identical(lapply(q, identity), lapply(r, identity))
  expect_identical(stratetab(q)$meta$sample_accounting, stratetab(r)$meta$sample_accounting)
  broken <- r
  ledger <- attr(r, "sample_accounting", exact = TRUE)
  ledger$measures$value[ledger$measures$metric == "used_n"] <- -1
  attr(broken, "sample_accounting") <- ledger
  expect_error(tt_rates(broken), class = "tabtools_error_sample_accounting")
  expect_error(stratetab(broken), class = "tabtools_error_sample_accounting")
  attr(r, "sample_accounting") <- NULL
  supplied <- tt_rates(r, level = .95)
  s <- attr(supplied, "sample_accounting", exact = TRUE)
  expect_identical(s$populations$weight_type, "unknown")
  for (metric in c("zero_weight_n", "weight_sum", "effective_n")) {
    row <- rw_qa_measure(s, metric, "summary")
    expect_identical(row$status, "unavailable")
    expect_true(is.na(row$value))
    expect_match(row$reason, "aggregated")
  }
})
