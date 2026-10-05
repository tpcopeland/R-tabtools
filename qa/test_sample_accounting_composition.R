library(testthat)
library(tabtools)

test_that("installed merge, stack and selection retain native source counts", {
  d <- mtcars
  d$hp[1:3] <- NA_real_
  a <- regtab(lm(mpg ~ wt, d), models = "M")
  b <- regtab(lm(mpg ~ wt + hp, d), models = "M")
  for (out in list(tt_merge(a, b), tt_stack(A = a, B = b),
                   comptab(list(a, b), rows = list(1, 1), section = c("A", "B")),
                   comptab(list(as.data.frame(a), as.data.frame(b)), rows = list(1, 1)))) {
    s <- out$meta$sample_accounting
    expect_identical(s$version, 1L)
    p <- s$populations[s$populations$scope == "model", ]
    expect_identical(as.numeric(p$model), c(1, 1))
    m <- s$measures[s$measures$metric == "fitted_n", ]
    expect_identical(m$value[match(p$id, m$population_id)], c(32, 29))
    expect_identical(attr(as.data.frame(out), "sample_accounting", exact = TRUE), s)
    expect_identical(length(unique(s$populations$id)), nrow(s$populations))
  }
  s <- tt_merge(tt_merge(a, a), a)$meta$sample_accounting
  expect_identical(s$populations$component, c("table1/table1/model1", "table1/table2/model1", "table2/model1"))
  expect_identical(s$measures$value[s$measures$metric == "fitted_n"], rep(32, 3))
})

test_that("installed summary effects and missing legacy accounting stay unavailable", {
  d <- data.frame(term = c("age", "bmi"), estimate = c(.2, .3), std.error = c(.1, .1))
  e <- effecttab(d, level = 95, models = "Effects", data = data.frame(age = 1:50))
  expect_identical(e$meta$sample_accounting$populations$scope, "summary")
  expect_identical(e$meta$sample_accounting$measures$status, rep("unavailable", 12))
  expect_identical(e$meta$sample_accounting$measures$value, rep(NA_real_, 12))
  old <- e
  old$meta$sample_accounting <- NULL
  out <- comptab(list(as.data.frame(old), as.data.frame(e)), rows = list(1, 1))
  s <- out$meta$sample_accounting
  expect_identical(nrow(s$populations), 2L)
  expect_identical(s$measures$status, rep("unavailable", 24))
  expect_identical(s$measures$value, rep(NA_real_, 24))
  expect_identical(s$measures$reason[startsWith(s$measures$population_id, "modeltable1/")],
                   rep("source has no sample ledger", 12))
})

test_that("installed presentation wrappers preserve selected sources", {
  a <- regtab(lm(mpg ~ wt, mtcars), models = "M")
  p <- puttab(a)
  selected <- puttab(as.data.frame(a), subset = 3)
  expect_identical(selected$meta$sample_accounting, p$meta$sample_accounting)
  expect_identical(nrow(selected$body), 1L)
  out <- stacktab(list(list(table = a, rows = 3, cols = "A-B"),
                       list(table = a, rows = 3, cols = "A-B")), spacing = 0)
  s <- out$meta$sample_accounting
  expect_identical(s$populations$component, c("block1/model1", "block2/model1"))
  expect_identical(s$measures$value[s$measures$metric == "fitted_n"], c(32, 32))
  expect_identical(puttab(data.frame(x = 1:3))$meta$sample_accounting$measures$status,
                   rep("unavailable", 12))
})

test_that("installed marginaleffects evaluation grid does not replace fitted N", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$mpg[1] <- NA_real_
  fit <- lm(mpg ~ wt, d, model = FALSE, x = FALSE, y = FALSE)
  x <- marginaleffects::avg_predictions(fit, newdata = data.frame(wt = c(2, 3)))
  out <- effecttab(x, type = "margins")
  s <- out$meta$sample_accounting
  fit_id <- s$populations$id[s$populations$scope == "model"]
  grid_id <- s$populations$id[s$populations$scope == "evaluation"]
  expect_length(fit_id, 1)
  expect_length(grid_id, 1)
  m <- s$measures
  expect_identical(m$value[m$population_id == fit_id & m$metric == "fitted_n"], 31)
  expect_identical(m$value[m$population_id == grid_id & m$metric == "input_n"], 2)
  expect_identical(m$status[m$population_id == grid_id & m$metric == "used_n"], "unavailable")
  d <- data.frame(mpg = NA_real_, wt = "changed")
  expect_identical(effecttab(x, type = "margins")$meta$sample_accounting, s)
})

test_that("installed HR composition retains rate and Cox source populations", {
  skip_if_not_installed("survival")
  d <- data.frame(time = c(4, 9, 2, 7, 5, 12, 6, 11, 3, 10, 8, 13, 1, 14, 15, 16),
                  event = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1),
                  g = factor(rep(c("A", "B"), 8)),
                  z = c(NA, 0, 1, 0, 1, 1, 0, 1, 0, NA, 1, 0, 1, 0, 1, 0))
  rates <- stratetab(tt_rates(d, "time", "event", by = "g", float_time = FALSE), outcomeids = "event")
  model <- regtab(survival::coxph(survival::Surv(time, event) ~ g + z, data = d))
  out <- hrcomptab(rates, model, rows = 3)
  transported <- hrcomptab(as.data.frame(rates), as.data.frame(model), rows = 3)
  expect_identical(transported$body, out$body)
  expect_identical(transported$meta$sample_accounting, out$meta$sample_accounting)
  expect_identical(out$body$c2[2:3], c("5", "5"))
  expect_identical(out$body$c3[2:3], c("44", "92"))
  s <- out$meta$sample_accounting
  expect_identical(nrow(s$populations), nrow(rates$meta$sample_accounting$populations) +
                     nrow(model$meta$sample_accounting$populations))
  fit_id <- s$populations$id[s$populations$scope == "model"]
  expect_length(fit_id, 1)
  expect_identical(s$measures$value[s$measures$population_id == fit_id &
                                    s$measures$metric == "fitted_n"], 14)
  expect_true(16 %in% s$measures$value[s$measures$metric == "input_n" &
                                      startsWith(s$measures$population_id, "ratetable/")])
})
