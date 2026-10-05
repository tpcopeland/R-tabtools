# Regressions from the 2026-09-30 effects and cohort audit.

test_that("effect-row Wald levels accept proportions and enforce provenance", {
  x <- data.frame(term = "x", estimate = 2, std.error = 1, df = 8)
  r <- tt_effect_rows(x, type = "margins", level = 0.9)
  expect_equal(c(r$conf.low, r$conf.high), 2 + c(-1, 1) * stats::qt(0.95, 8),
               tolerance = 1e-12)
  expect_identical(attr(r, "level"), 90)
  expect_identical(tt_effect_rows(x, type = "margins", level = 90), r)
  attr(x, "conf.level") <- 0.9
  expect_error(tt_effect_rows(x, type = "margins", level = 95), "conflicts")
  expect_error(tt_effect_rows(x, type = "margins", level = 100), "level")
})

test_that("effect data frames reject reversed confidence bounds", {
  x <- data.frame(term = "x", estimate = 1, conf.low = 3, conf.high = 2)
  expect_error(effecttab(x, level = 95), class = "tabtools_error_df_interval")
  x$std.error <- 1
  expect_error(effecttab(x, level = 95), class = "tabtools_error_df_interval")
})

test_that("data-frame composite sources retain known additive-scale provenance", {
  x <- data.frame(term = "treat", estimate = -0.3, conf.low = -0.5,
                  conf.high = -0.1, p.value = 0.01)
  e <- effecttab(x, effect = "HR", models = "death", level = 95)
  # A recorded additive scale, as marginaleffects difference results carry.
  e$meta$frame$effect_additive <- TRUE
  r <- stratetab(ct_block(c("No", "Yes"), c(5, 8), c(100, 200)),
                  outcomeids = "death")
  expect_error(hrcomptab(r, e, rows = 1, outcomemap = "death"),
                class = "tabtools_error_not_hazard_ratio")
  expect_error(hrcomptab(r, as.data.frame(e), rows = 1, outcomemap = "death"),
                class = "tabtools_error_not_hazard_ratio")
  vertical <- comptab(e, rows = 1)
  expect_error(hrcomptab(r, vertical, rows = 1, outcomemap = "death"),
                class = "tabtools_error_not_hazard_ratio")
})

test_that("a plain ratio with unusable standard errors cannot silently lose its test", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  fit <- glm(vs ~ am + wt, data = d, family = binomial)
  x <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio",
                                        transform = exp)
  expect_true(is.finite(x$p.value))
  expect_error(effecttab(x), class = "tabtools_error_ratio_inference")
  # Inference explicitly disabled remains a valid estimates-only input.
  plain <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio",
                                            vcov = FALSE)
  expect_s3_class(effecttab(plain), "tt_table")
  log <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnratioavg",
                                          transform = exp)
  f <- as_forest_data(effecttab(log))
  expect_equal(f$pvalue[f$rowtype == "effect"], log$p.value, tolerance = 1e-12)
  explicit <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "ratio",
                                               hypothesis = 1, transform = exp)
  f <- as_forest_data(effecttab(explicit))
  expect_equal(f$pvalue[f$rowtype == "effect"], explicit$p.value, tolerance = 1e-12)
})

test_that("raw log ratios headed HR are refused, exponentiated log ratios remain valid", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$am <- factor(d$am, 0:1, c("Auto", "Man"))
  fit <- glm(vs ~ am + wt, data = d, family = binomial)
  r <- stratetab(ct_block(c("Auto", "Man"), c(5, 8), c(100, 200)), outcomeids = "vs")
  raw <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnratioavg")
  e <- effecttab(raw, effect = "HR", models = "vs")
  expect_error(hrcomptab(r, e, rows = 3, outcomemap = "vs"),
                class = "tabtools_error_not_hazard_ratio")
  vertical <- comptab(e, rows = 2:3)
  expect_error(hrcomptab(r, vertical, rows = 2, outcomemap = "vs"),
                class = "tabtools_error_not_hazard_ratio")
  expect_error(hrcomptab(r, as.data.frame(vertical), rows = 2, outcomemap = "vs"),
                class = "tabtools_error_not_hazard_ratio")
  exp <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnratioavg",
                                          transform = base::exp)
  tt <- hrcomptab(r, effecttab(exp, effect = "HR", models = "vs"), rows = 3, outcomemap = "vs")
  f <- as_forest_data(tt)
  expect_equal(f$estimate[f$rowtype == "effect"], exp$estimate, tolerance = 1e-12)
  tr <- NULL
  unknown <- marginaleffects::avg_comparisons(fit, variables = "am", comparison = "lnratioavg",
                                              transform = tr)
  unknown <- effecttab(unknown, effect = "HR", models = "vs")
  expect_error(hrcomptab(r, unknown, rows = 3, outcomemap = "vs"),
                class = "tabtools_error_not_hazard_ratio")
  expect_error(hrcomptab(r, as.data.frame(unknown), rows = 3, outcomemap = "vs"),
                class = "tabtools_error_not_hazard_ratio")
})

test_that("continuous exponentiated log ratios retain their source ratio scale", {
  skip_if_not_installed("marginaleffects")
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  x <- marginaleffects::avg_comparisons(fit, variables = "mpg", comparison = "lnratioavg",
                                        transform = exp)
  e <- effecttab(x, effect = "HR", models = "vs")
  expect_identical(e$meta$frame$effect_additive, FALSE)
  r <- stratetab(ct_block(c("No", "Yes"), c(5, 8), c(100, 200)), outcomeids = "vs")
  tt <- hrcomptab(r, e, rows = 1, outcomemap = "vs")
  f <- as_forest_data(tt)
  expect_equal(f$estimate[f$rowtype == "effect"], x$estimate, tolerance = 1e-12)
  # An older caller-declared ratio has no log-scale metadata. Its absence
  # must not turn a verified ratio in another section into an unknown scale.
  declared <- effecttab(data.frame(term = "mpg", estimate = x$estimate,
                                    conf.low = x$conf.low, conf.high = x$conf.high,
                                    p.value = x$p.value), effect = "HR", models = "vs", level = 95)
  declared$meta$frame$effect_log_scale <- NULL
  vertical <- comptab(list(declared, e), rows = list(1, 1))
  expect_s3_class(hrcomptab(r, vertical, rows = 2, outcomemap = "vs"), "tt_table")
})

test_that("rate arithmetic rejects unusable person-time and overflowing totals", {
  d <- data.frame(t = c(1, 2), e = c(1, 0), w = c(1e308, 1e308))
  expect_error(tt_rates(d, "t", "e", per = 1e100),
                class = "tabtools_error_rate_totals")
  expect_error(tt_rates(d, "t", "e", fweight = "w", float_time = FALSE),
                class = "tabtools_error_rate_totals")
  expect_error(tt_rates(data.frame(t = 1e-320, e = 1), "t", "e", float_time = FALSE),
                class = "tabtools_error_rate_totals")
  expect_error(tt_rates(data.frame(t = 1e-308, e = 1), "t", "e", float_time = FALSE),
                class = "tabtools_error_rate_totals")
  # No events is a defined zero rate with unavailable log-normal bounds.
  zero <- tt_rates(transform(d, e = 0), "t", "e")
  expect_identical(zero$Rate, 0)
  expect_true(is.na(zero$Lower) && is.na(zero$Upper))
})

test_that("empty strate input is refused before a header-only table is returned", {
  d <- data.frame(D = numeric(), Y = numeric(), Rate = numeric(),
                  Lower = numeric(), Upper = numeric())
  expect_error(tt_rates(d), class = "tabtools_error_rate_empty")
  expect_error(stratetab(d, level = 95), class = "tabtools_error_rate_empty")
  # The computed-block shortcut is subject to the same empty guard.
  attr(d, "ci_method") <- "lognormal"
  attr(d, "level") <- 0.95
  expect_error(stratetab(d), class = "tabtools_error_rate_empty")
})

test_that("rate flags and scalar analysis-column selectors are validated", {
  d <- data.frame(t = c(1, 2), e = c(1, 0))
  expect_error(tt_rates(d, "t", "e", missing = NA), "missing.*TRUE or FALSE")
  expect_error(tt_rates(d, "t", "e", float_time = "TRUE"), "float_time.*TRUE or FALSE")
  expect_error(tt_rates(d, time = c("t", "e"), event = "e"), "time.*single.*column")
})
