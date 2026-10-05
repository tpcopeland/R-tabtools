# Adversarial missing-data contracts for rates and supplied effects.

test_that("missing events retain exposure but missing follow-up and groups do not", {
  d <- data.frame(t = c(2, 4, NA, 0, 3, 5, 7),
                  e = c(1, NA, 1, 1, 0, -2, 1),
                  g = c("A", "A", "A", "A", "B", NA, "B"),
                  unrelated = NA_real_)
  before <- d
  r <- tt_rates(d, "t", "e", by = "g", float_time = FALSE)
  expect_identical(r$g, c("A", "B"))
  expect_identical(r$D, c(1, 1))
  expect_identical(r$Y, c(6, 10))
  expect_equal(r$Rate, c(1 / 6, 1 / 10), tolerance = 1e-12)
  expect_equal(r$Lower, c(1 / 6, 1 / 10) * exp(-qnorm(.975)), tolerance = 1e-12)
  expect_equal(r$Upper, c(1 / 6, 1 / 10) * exp(qnorm(.975)), tolerance = 1e-12)
  kept <- tt_rates(d, "t", "e", by = "g", missing = TRUE, float_time = FALSE)
  expect_identical(kept$g, c("A", "B", NA_character_))
  expect_identical(kept$D, c(1, 1, 1))
  expect_identical(kept$Y, c(6, 10, 5))
  expect_identical(d, before)
})

test_that("all missing events have a defined zero rate with unavailable bounds", {
  d <- data.frame(t = c(2, 3), e = c(NA_real_, NA_real_), g = "A")
  r <- tt_rates(d, "t", "e", by = "g")
  expect_identical(r$D, 0)
  expect_identical(r$Y, 5)
  expect_identical(r$Rate, 0)
  expect_identical(r$Lower, NA_real_)
  expect_identical(r$Upper, NA_real_)
  s <- stratetab(r, outcomeids = "e")
  expect_identical(s$meta$rate_rows$events, 0)
  expect_identical(s$meta$rate_rows$person_years, 5)
  expect_identical(s$meta$rate_rows$rate, 0)
  expect_identical(s$meta$rate_rows$lower, NA_real_)
  expect_identical(s$meta$rate_rows$upper, NA_real_)
})

test_that("missing effect uncertainty and explicit empty rows never become zero", {
  d <- data.frame(term = c("observed", "unavailable", "true_zero"),
                  estimate = c(2, NA, 0), std.error = c(NA, NA, NA),
                  status = c("est", "empty", "est"))
  x <- effecttab(d, level = 95)
  expect_identical(x$body$c1, d$term)
  expect_identical(x$body$c2, c("2.00", "Empty", "0.00"))
  expect_identical(x$body$c3, rep("", 3L))
  expect_identical(x$body$c4, rep("", 3L))
  rows <- x$meta$effect_rows
  expect_identical(rows$estimate, c(2, NA_real_, 0))
  expect_identical(rows$conf.low, rep(NA_real_, 3L))
  expect_identical(rows$conf.high, rep(NA_real_, 3L))
  expect_identical(rows$p.value, rep(NA_real_, 3L))
})

test_that("unavailable SE or df cannot manufacture null-test results", {
  d <- data.frame(term = c("infinite_se", "missing_df", "nan_df", "usable"),
                  estimate = c(0, 2, 3, 2), std.error = c(Inf, 1, 1, 1),
                  df = c(Inf, NA, NaN, Inf))
  x <- effecttab(d, level = 95)
  rows <- x$meta$effect_rows
  expect_identical(rows$p.value[1:3], rep(NA_real_, 3L))
  expect_identical(rows$conf.low[1:3], rep(NA_real_, 3L))
  expect_identical(rows$conf.high[1:3], rep(NA_real_, 3L))
  expect_identical(x$body$c4[1:3], rep("", 3L))
  expect_equal(rows$p.value[4], 2 * pnorm(-2), tolerance = 1e-12)
  expect_equal(rows$conf.low[4], 2 - qnorm(.975), tolerance = 1e-12)
  expect_equal(rows$conf.high[4], 2 + qnorm(.975), tolerance = 1e-12)
})

test_that("partial and reversed effect intervals are explicitly refused", {
  d <- data.frame(term = "x", estimate = 2, conf.low = NA_real_, conf.high = 3)
  expect_error(effecttab(d, level = 95), class = "tabtools_error_df_half_interval")
  d$conf.low <- 4
  expect_error(effecttab(d, level = 95), class = "tabtools_error_df_interval")
})

test_that("extreme finite rate scales refuse overflowing bounds", {
  expect_error(tt_rates(data.frame(t = 1e-308, e = 1), "t", "e", float_time = FALSE),
               class = "tabtools_error_rate_totals")
  expect_error(tt_rates(data.frame(t = 1, e = 1), "t", "e", per = 1e100),
               class = "tabtools_error_rate_totals")
})
