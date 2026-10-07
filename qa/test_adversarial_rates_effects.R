library(testthat)
library(tabtools)

# Installed-user numerical invariants and model-dependent case deletion.

test_that("weighted entry and missingness conserve hand-counted events and exposure", {
  d <- data.frame(t = c(5, 8, 4, 9, 7, 6, NA, 10),
                  entry = c(1, 2, 4, NA, 3, 1, 0, 1),
                  event = c(1, NA, 1, 1, 0, -1, 1, 1),
                  w = c(2, 3, 4, 1, 0, 2, 1, NA),
                  group = c("A", "A", "A", "A", "B", NA, "B", "B"))
  r <- tt_rates(d, "t", "event", by = "group", entry = "entry", fweight = "w",
                missing = TRUE, float_time = FALSE)
  # Only rows 1, 2, 6 contribute: weighted exposure is 8, 18, 10.
  expect_identical(r$group, c("A", NA_character_))
  expect_identical(r$D, c(2, 2))
  expect_identical(r$Y, c(26, 10))
  expect_equal(r$Rate, c(2 / 26, 2 / 10), tolerance = 1e-12)
  expect_equal(r$Lower / r$Rate, rep(exp(-qnorm(.975) / sqrt(2)), 2), tolerance = 1e-12)
  expect_equal(r$Upper / r$Rate, rep(exp(qnorm(.975) / sqrt(2)), 2), tolerance = 1e-12)
  expect_identical(sum(r$D), 4)
  expect_identical(sum(r$Y), 36)
})

test_that("Cox covariate deletion does not silently redefine supplied rate population", {
  skip_if_not_installed("survival")
  d <- data.frame(time = c(4,9,2,7,5,12,6,11,3,10,8,13,1,14,15,16),
                  event = c(1,0,1,1,0,1,1,0,1,0,1,1,0,1,0,1),
                  g = factor(rep(c("A", "B"), 8)),
                  z = c(NA,0,1,0,1,1,0,1,0,NA,1,0,1,0,1,0))
  fit <- survival::coxph(survival::Surv(time, event) ~ g + z, data = d)
  expect_identical(fit$n, 14L)
  raw <- tt_rates(d, "time", "event", by = "g", float_time = FALSE)
  expect_identical(raw$D, c(5, 5))
  expect_identical(raw$Y, c(44, 92))
  rates <- stratetab(raw, outcomeids = "event")
  m <- regtab(fit)
  h <- hrcomptab(rates, m, rows = 3)
  c <- comptab(rates, m, rows = 3)
  expect_identical(h$body, c$body)
  expect_identical(h$body$c2[2:3], c("5", "5"))
  expect_identical(h$body$c3[2:3], c("44", "92"))
  f <- as_forest_data(h)
  effect <- f[f$rowtype == "effect", ]
  expect_identical(nrow(effect), 1L)
  expect_equal(effect$estimate, exp(unname(stats::coef(fit)["gB"])), tolerance = 1e-12)
  # Complete-case rates can be supplied explicitly and differ predictably.
  complete <- d[!is.na(d$z), ]
  rr <- tt_rates(complete, "time", "event", by = "g", float_time = FALSE)
  expect_identical(rr$D, c(4, 5))
  expect_identical(rr$Y, c(40, 82))
})

test_that("singular Cox terms remain omitted while identifiable estimates survive", {
  skip_if_not_installed("survival")
  d <- data.frame(time = c(4,9,2,7,5,12,6,11,3,10,8,13,1,14,15,16),
                  event = c(1,0,1,1,0,1,1,0,1,0,1,1,0,1,0,1),
                  g = factor(rep(c("A", "B"), 8)),
                  z = c(NA,0,1,0,1,1,0,1,0,NA,1,0,1,0,1,0))
  fit <- survival::coxph(survival::Surv(time, event) ~ g + z + I(2*z), data = d)
  expect_identical(unname(stats::coef(fit)[3]), NA_real_)
  m <- regtab(fit)
  expect_identical(m$body$c2[5], "Omitted")
  expect_identical(m$body$c3[5], "")
  f <- as_forest_data(m)
  expect_identical(sum(f$rowtype == "effect"), 2L)
  expect_equal(f$estimate[f$rowtype == "effect"], exp(unname(stats::coef(fit)[1:2])), tolerance = 1e-12)
})

test_that("finite extreme supplied intervals survive effect table composition", {
  d <- data.frame(term = c("wide", "uncertain"), estimate = c(1e100, 2),
                  conf.low = c(1e-100, NA), conf.high = c(1e200, NA), p.value = c(.2, NA))
  e <- effecttab(d, level = 95, models = "effects")
  c <- comptab(modeltables = e, rows = 1:2)
  # Both selected publication companions retain the original quantities;
  # incomplete intervals are ineligible for plotting under the P.6 contract.
  raw <- c$meta$regtab_rows
  expect_identical(nrow(raw), 2L)
  expect_identical(raw$label, c("wide", "uncertain"))
  expect_identical(raw$source_row, 1:2)
  expect_equal(raw$estimate, c(1e100, 2), tolerance = 1e-12)
  expect_equal(raw$conf.low, c(1e-100, NA_real_), tolerance = 0)
  expect_equal(raw$conf.high, c(1e200, NA_real_), tolerance = 1e-12)
  expect_identical(raw$p.value, c(.2, NA_real_))
  f <- as_forest_data(c)
  expect_identical(nrow(f), 1L)
  expect_identical(f$label, "wide")
  expect_identical(f$source_row, 1L)
  expect_equal(f$estimate, 1e100, tolerance = 1e-12)
  expect_equal(f$ll, 1e-100, tolerance = 0)
  expect_equal(f$ul, 1e200, tolerance = 1e-12)
  expect_identical(f$pvalue, .2)
  expect_false("uncertain" %in% f$label)
  expect_identical(c$body$c1, c("wide", "uncertain"))
  expect_identical(c$body$c3[2], "")
  expect_identical(c$body$c4[2], "")
})
