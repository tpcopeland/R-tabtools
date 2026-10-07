# Selected display ports from pinned Stata 4eecca4d (2.5.2+), not a
# wholesale baseline change. Raw numerical metadata stays unchanged.
test_that("default regression limits share estimate rounding and retain signed zero", {
  expect_identical(.rt_ci_text(c(0.125, -4.375, -0.0004, NA_real_),
    c(4.125, -0.375, 1, 1), 2, ", "),
    c("(0.13, 4.13)", "(-4.37, -0.37)", "(-0.00, 1.00)", ""))
  expect_identical(.rt_ci_text(0.125 + c(-1e-7, 1e-7), rep(1, 2), 2, "; "),
    c("(0.12; 1.00)", "(0.13; 1.00)"))
  spec <- .tt_resolve_numeric_format("%9.2f", sep = ", ")
  expect_identical(.rt_ci_text(0.125, 4.125, 2, ", ", spec), "(0.12, 4.12)")
  expect_identical(.rt_est_text(2.125, 2, spec), "2.12")
  expect_identical(.rt_ci_text(numeric(), numeric(), 2, ", "), character())
})

test_that("events and person-time use the same half-up decimal rule", {
  expect_identical(.st_fmt_events(c(2.5, 0.5, 1.5, 3.5, NA_real_), 0),
    c("3", "1", "2", "4", "."))
  expect_identical(.st_fmt_events(c(2.25, 0.25, 0.75, 1.25), 1),
    c("2.3", "0.3", "0.8", "1.3"))
  expect_identical(.st_fmt_py(c(2.25, 0.25, 123.5, 1234567.5, 0.35), 1),
    c("2.3", "0.3", "123.5", "1,234,567.5", "0.3"))
  expect_identical(.st_fmt_py(c(2.25, 0.25), 2), c("2.25", "0.25"))
})

test_that("fixed rate cformat pre-rounds while general and exponential remain direct", {
  spec <- .tt_resolve_numeric_format("%12.1f", sep = ", ")
  expect_identical(.st_fmt_rate_spec(2.25, 0.25, 123.5, spec), "2.3 (0.3, 123.5)")
  spec <- .tt_resolve_numeric_format("%-12.1fc", sep = ", ")
  expect_identical(.st_fmt_rate_spec(1234567.5, 0.25, 2.25, spec),
    "1,234,567.5 (0.3, 2.3)")
  spec <- .tt_resolve_numeric_format("%12,1f", sep = "; ")
  expect_identical(.st_fmt_rate_spec(2.25, 0.25, 123.5, spec), "2,3 (0,3; 123,5)")
  for (fmt in c("%12.4g", "%12.2e")) {
    spec <- .tt_resolve_numeric_format(fmt)
    expect_identical(.st_fmt_rate_spec(2.25, 0.25, 123.5, spec),
      paste0(.tt_format_numeric(2.25, spec), " (", .tt_format_numeric(0.25, spec),
        ", ", .tt_format_numeric(123.5, spec), ")"))
  }
  spec <- .tt_resolve_numeric_format("%12.1f")
  expect_identical(.st_fmt_rate_spec(2.25, NA_real_, 3, spec), "2.3 (\u2013)")
})

test_that("public rate rounding changes text and preserves complete numerical provenance", {
  b <- data.frame(arm = 1:2, `_D` = c(2.5, 0.5), `_Y` = c(2.25, 0.25),
    `_Rate` = c(2.25, 0.25), `_Lower` = c(0.25, 2.25),
    `_Upper` = c(123.5, 0.75), check.names = FALSE)
  before <- b
  tt <- stratetab(b, level = 95, ratescale = 1, eventdigits = 0, pydigits = 1, cformat = "%12.1f")
  expect_identical(tt$body$c2[-1], c("3", "1"))
  expect_identical(tt$body$c3[-1], c("2.3", "0.3"))
  expect_identical(tt$body$c4[-1], c("2.3 (0.3, 123.5)", "0.3 (2.3, 0.8)"))
  expect_equal(unname(tt$stored$rates[, 1]), c(2.25, 0.25))
  expect_identical(tt$meta$rate_rows$events, c(2.5, 0.5))
  expect_identical(tt$meta$rate_rows$person_years, c(2.25, 0.25))
  expect_identical(tt$meta$rate_rows$lower, c(0.25, 2.25))
  expect_identical(tt$meta$rate_rows$upper, c(123.5, 0.75))
  expect_identical(b, before)
})

test_that("effect CI bounds round after their existing matrix conversion", {
  expect_identical(.et_ci_text(0.125, 4.125, 2, ", ", FALSE), "(0.13, 4.13)")
  expect_identical(.et_ci_text(-4.375, -0.375, 2, ", ", FALSE), "(-4.37, -0.37)")
  expect_identical(.et_ci_text(-0.0004, 1, 2, "; ", FALSE), "(-0.00; 1.00)")
  # from() has always converted strings back to numeric first. Keep that
  # declared input route, including its loss of negative zero.
  expect_identical(.et_ci_text(0.125, 4.125, 2, ", ", TRUE), "(0.12, 4.12)")
  expect_identical(.et_ci_text(-0.0004, 1, 2, "; ", TRUE), "(0.00; 1.00)")
  expect_identical(.et_ci_text(NA_real_, 1, 2, ", ", FALSE), "")
})
