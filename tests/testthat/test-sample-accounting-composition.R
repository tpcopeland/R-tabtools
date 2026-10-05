# Composition preserves source populations independently of rendered rows.

test_that("merge and stack retain distinct native fitted samples", {
  d <- mtcars
  d$hp[1:3] <- NA_real_
  a <- regtab(lm(mpg ~ wt, d), models = "M")
  b <- regtab(lm(mpg ~ wt + hp, d), models = "M")
  for (out in list(tt_merge(a, b), tt_stack(A = a, B = b))) {
    s <- out$meta$sample_accounting
    expect_identical(s$version, 1L)
    p <- s$populations[s$populations$scope == "model", ]
    expect_identical(as.numeric(p$model), c(1, 1))
    m <- s$measures[s$measures$metric == "fitted_n", ]
    expect_identical(m$value[match(p$id, m$population_id)], c(32, 29))
    expect_identical(p$component, c("table1/model1", "table2/model1"))
    expect_identical(length(unique(s$populations$id)), nrow(s$populations))
  }
})

test_that("reused and nested sources remain separate without summing counts", {
  a <- regtab(lm(mpg ~ wt, mtcars), models = "M")
  out <- tt_merge(tt_merge(a, a), a)
  s <- out$meta$sample_accounting
  p <- s$populations[s$populations$scope == "model", ]
  expect_identical(p$component, c("table1/table1/model1", "table1/table2/model1", "table2/model1"))
  expect_identical(as.numeric(p$model), c(1, 1, 1))
  m <- s$measures[s$measures$metric == "fitted_n", ]
  expect_identical(m$value[match(p$id, m$population_id)], rep(32, 3))
  expect_identical(length(unique(s$populations$id)), nrow(s$populations))
  expect_identical(nrow(s$populations), 3L * nrow(a$meta$sample_accounting$populations))
})

test_that("presentation wrappers retain source accounting after cell selections", {
  a <- regtab(lm(mpg ~ wt, mtcars), models = "M")
  p <- puttab(a)
  selected <- puttab(as.data.frame(a), subset = 3)
  expect_identical(selected$meta$sample_accounting, p$meta$sample_accounting)
  expect_identical(nrow(selected$body), 1L)
  out <- stacktab(list(list(table = a, rows = 3, cols = "A-B"),
                       list(table = a, rows = 3, cols = "A-B")), spacing = 0)
  s <- out$meta$sample_accounting
  model <- s$populations[s$populations$scope == "model", ]
  expect_identical(model$component, c("block1/model1", "block2/model1"))
  expect_identical(as.numeric(model$model), c(1, 1))
  expect_identical(s$measures$value[s$measures$metric == "fitted_n"], c(32, 32))
  raw <- puttab(data.frame(x = 1:3))$meta$sample_accounting
  expect_identical(raw$populations$scope, "summary")
  expect_identical(raw$measures$status, rep("unavailable", 12))
})

test_that("selected vertical rows retain full sources and local model identities", {
  d <- mtcars
  d$hp[1:3] <- NA_real_
  a <- regtab(list(lm(mpg ~ wt, d), lm(disp ~ wt, d)), models = c("MPG", "Disp"))
  b <- regtab(list(lm(disp ~ wt + hp, d), lm(mpg ~ wt + hp, d)), models = c("Disp", "MPG"))
  out <- comptab(list(a, b), rows = list(1, 1), section = c("A", "B"),
                 relabel = c("2" = "Weight A", "4" = "Weight B"))
  expect_identical(out$body$c1, c("A", "Weight A", "B", "Weight B"))
  expect_identical(out$body$c2[4], b$body$c5[1])
  s <- out$meta$sample_accounting
  p <- s$populations[s$populations$scope == "model", ]
  expect_identical(as.numeric(p$model), c(1, 2, 1, 2))
  expect_identical(p$component, c("modeltable1/model1", "modeltable1/model2", "modeltable2/model1", "modeltable2/model2"))
  m <- s$measures[s$measures$metric == "fitted_n", ]
  expect_identical(m$value[match(p$id, m$population_id)], c(32, 32, 29, 29))
  expect_identical(nrow(s$populations), nrow(a$meta$sample_accounting$populations) +
                     nrow(b$meta$sample_accounting$populations))
  transported <- comptab(list(as.data.frame(a), as.data.frame(b)), rows = list(1, 1),
                          section = c("A", "B"), relabel = c("2" = "Weight A", "4" = "Weight B"))
  expect_identical(transported$body, out$body)
  expect_identical(transported$meta$sample_accounting, s)
})

test_that("missing legacy ledgers remain explicit in every composition path", {
  a <- regtab(lm(mpg ~ wt, mtcars), models = "M")
  old <- a
  old$meta$sample_accounting <- NULL
  for (out in list(tt_merge(old, a), tt_stack(old, a),
                   comptab(list(old, a), rows = list(1, 1)),
                   comptab(list(as.data.frame(old), as.data.frame(a)), rows = list(1, 1)))) {
    s <- out$meta$sample_accounting
    p <- s$populations[s$populations$scope == "summary", ]
    expect_identical(nrow(p), 1L)
    m <- s$measures[s$measures$population_id == p$id, ]
    expect_identical(nrow(m), 12L)
    expect_identical(m$status, rep("unavailable", 12))
    expect_identical(m$value, rep(NA_real_, 12))
    expect_identical(m$reason, rep("source has no sample ledger", 12))
  }
})

test_that("effect summaries and label data cannot manufacture cohort counts", {
  d <- data.frame(term = c("age", "bmi"), estimate = c(.2, .3), std.error = c(.1, .1))
  m <- matrix(c(.2, 0, .4, .05), nrow = 1,
               dimnames = list("age", NULL))
  for (out in list(effecttab(d, level = 95, models = "Effects", data = data.frame(age = 1:50)),
                       effecttab(m, models = "Effects"))) {
    s <- out$meta$sample_accounting
    expect_identical(s$populations$scope, "summary")
    expect_identical(as.numeric(s$populations$model), 1)
    expect_identical(s$measures$status, rep("unavailable", 12))
    expect_identical(s$measures$value, rep(NA_real_, 12))
    expect_identical(attr(as.data.frame(out), "sample_accounting", exact = TRUE), s)
    c <- comptab(out, rows = 1)
    expect_identical(c$meta$sample_accounting$measures$value, rep(NA_real_, 12))
  }
})

test_that("multiple effect pieces retain their source summaries", {
  a <- data.frame(term = "age", estimate = .2, std.error = .1)
  b <- data.frame(term = "bmi", estimate = .3, std.error = .1)
  out <- effecttab(Group = list(a, b), level = 95)
  s <- out$meta$sample_accounting
  expect_identical(s$populations$scope, rep("summary", 2))
  expect_identical(as.numeric(s$populations$model), c(1, 1))
  expect_identical(s$populations$component, c("model1/piece1", "model1/piece2"))
  expect_identical(out$body$c1, c("age", "bmi"))
  c <- comptab(out, rows = 1)
  expect_identical(nrow(c$meta$sample_accounting$populations), 2L)
})

test_that("marginaleffects fit and evaluation grid describe separate populations", {
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
  expect_identical(m$value[m$population_id == grid_id & m$metric == "frame_n"], 2)
  expect_identical(m$status[m$population_id == grid_id & m$metric == "used_n"], "unavailable")
  expect_identical(m$status[m$population_id == grid_id & m$metric == "fitted_n"], "not_applicable")
  # The stored result survives changed fitting data and an unavailable call.
  d <- data.frame(wt = "changed", mpg = NA_real_)
  fit$call$data <- quote(stop("fitting call must not be evaluated"))
  again <- effecttab(x, type = "margins")
  expect_identical(again$body, out$body)
  expect_identical(again$meta$sample_accounting, s)
})

test_that("rate and Cox sources retain different populations through both inputs", {
  skip_if_not_installed("survival")
  d <- data.frame(time = c(4, 9, 2, 7, 5, 12, 6, 11, 3, 10, 8, 13, 1, 14, 15, 16),
                  event = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1),
                  g = factor(rep(c("A", "B"), 8)),
                  z = c(NA, 0, 1, 0, 1, 1, 0, 1, 0, NA, 1, 0, 1, 0, 1, 0))
  fit <- survival::coxph(survival::Surv(time, event) ~ g + z, data = d)
  rates <- stratetab(tt_rates(d, "time", "event", by = "g", float_time = FALSE), outcomeids = "event")
  model <- regtab(fit)
  out <- hrcomptab(rates, model, rows = 3)
  expect_identical(out$body$c2[2:3], c("5", "5"))
  expect_identical(out$body$c3[2:3], c("44", "92"))
  s <- out$meta$sample_accounting
  expect_identical(nrow(s$populations), nrow(rates$meta$sample_accounting$populations) +
                     nrow(model$meta$sample_accounting$populations))
  fit_id <- s$populations$id[s$populations$scope == "model"]
  expect_length(fit_id, 1)
  expect_identical(s$measures$value[s$measures$population_id == fit_id &
                                    s$measures$metric == "fitted_n"], 14)
  rate_input <- s$measures[s$measures$metric == "input_n" &
                           startsWith(s$measures$population_id, "ratetable/"), ]
  expect_true(16 %in% rate_input$value)
  transported <- hrcomptab(as.data.frame(rates), as.data.frame(model), rows = 3)
  expect_identical(transported$body, out$body)
  expect_identical(transported$meta$sample_accounting, s)
  old <- rates
  old$meta$sample_accounting <- NULL
  legacy <- hrcomptab(old, model, rows = 3)$meta$sample_accounting
  expect_identical(legacy$populations$scope[legacy$populations$component == "ratetable"], "summary")
})
