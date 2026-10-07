muse_rate_block <- function(D = 5, Y = 10, Rate = .5, Lower = .25, Upper = 1) {
  b <- data.frame(g = "A", D = D, Y = Y, Rate = Rate, Lower = Lower, Upper = Upper)
  attr(b$Lower, "label") <- "Lower 95% confidence limit"
  attr(b$Upper, "label") <- "Upper 95% confidence limit"
  b
}

test_that("native missing events retain supplied rates and propagate through ratio bounds", {
  reference <- muse_rate_block(D = 4, Y = 10, Rate = .4, Lower = .2, Upper = .8)
  missing_events <- muse_rate_block(D = NA_real_, Y = 10, Rate = .5, Lower = .2, Upper = .9)
  t <- stratetab(list(reference, missing_events), outcomes = 1, rateratio = TRUE,
    ratescale = 1, digits = 2)
  # Pinned native literal in test-stratetab-units: missing > 0 in Stata.
  expect_identical(t$body$c2[4], ".")
  expect_identical(t$body$c4[4], "0.50 (0.20, 0.90)")
  expect_identical(t$body$c5[4], "1.25 (., .)")
  supplied <- stratetab(muse_rate_block(Rate = 1234.5, Lower = 1000, Upper = 2000),
    ratescale = 1, cformat = "%12.2fc")
  expect_identical(supplied$body$c4[2], "1,234.50 (1,000.00, 2,000.00)")
})

test_that("native scaled and exact zero interval overflow becomes missing", {
  b <- muse_rate_block(Rate = 1e10, Lower = 1, Upper = 1e11)
  t <- stratetab(b, ratescale = 1e300)
  # Existing independent native scaling control keeps missing overflow.
  expect_identical(t$body$c4[2], ". (\u2013)")
  expect_true(is.na(t$stored$rates[1, 1]))
  zero <- muse_rate_block(D = 0, Y = 1e-300, Rate = 0, Lower = NA_real_, Upper = NA_real_)
  t <- stratetab(zero, zeroexact = TRUE, ratescale = 1e100)
  # Independent pinned native M2 control: a missing generated bound.
  expect_identical(t$body$c4[2], "0.0 (\u2013)")
  expect_identical(t$meta$rate_rows$upper, NA_real_)
  expect_identical(t$meta$rate_rows$lower, 0)
  expect_identical(t$meta$rate_rows$ci_method, "exact_poisson_zero")
  expect_identical(stratetab(muse_rate_block(), ratescale = 10)$body$c4[2], "5.0 (2.5, 10.0)")
})

test_that("no-time errors have one class across computed and retained branches", {
  expect_error(tt_rates(muse_rate_block(D = 5, Y = 0, Rate = NA_real_,
    Lower = NA_real_, Upper = NA_real_)), class = "tabtools_error_rate_no_time")
  for (allow in c(FALSE, TRUE)) {
    expect_error(tabtools:::.rate_ci(data.frame(), 5, 0, .95, allow_zero = allow),
      class = "tabtools_error_rate_no_time")
  }
  expect_error(tabtools:::.rate_ci(data.frame(), NA_real_, 10, .95),
    class = "tabtools_error_rate_totals")
})

test_that("computed missing character and factor groups have explicit collision-safe labels", {
  for (group in list(c("A", NA_character_), factor(c("A", NA_character_)))) {
    r <- tt_rates(data.frame(t = c(10, 20), ev = c(1, 1), g = group),
      time = "t", event = "ev", by = "g", missing = TRUE)
    t <- stratetab(r, ratescale = 1, digits = 2)
    expect_identical(t$body$c1, c("g", "   A", "   ."))
    expect_identical(t$body$c2, c("", "1", "1"))
    expect_identical(t$body$c3, c("", "10", "20"))
    expect_equal(t$meta$rate_rows$rate, c(.1, .05), tolerance = 1e-14)
  }
  r <- tt_rates(data.frame(t = c(10, 20), ev = c(1, 1), g = c(".", NA_character_)),
    time = "t", event = "ev", by = "g", missing = TRUE)
  expect_error(stratetab(r), "Duplicate category labels")
  supplied <- muse_rate_block(); supplied$g <- NA_character_
  expect_error(stratetab(supplied), "Blank category labels")
  expect_error(tt_rates(data.frame(t = 1:2, ev = c(0, 1), g = I(list("a", "b"))),
    time = "t", event = "ev", by = "g"), class = "tabtools_error_rate_group")
})

test_that("literal lognormal rate and ratio recovery preserves native formatting boundaries", {
  r <- tt_rates(data.frame(t = rep(25, 4), ev = rep(1, 4)), time = "t", event = "ev")
  expect_equal(r$D, 4, tolerance = 0)
  expect_equal(r$Y, 100, tolerance = 0)
  expect_equal(r$Rate, .04, tolerance = 1e-15)
  expect_equal(r$Lower, .01501271429652706, tolerance = 1e-10)
  expect_equal(r$Upper, .10657633046211588, tolerance = 1e-10)
  a <- muse_rate_block(D = 5, Y = 10, Rate = .5)
  b <- muse_rate_block(D = 5, Y = 10, Rate = .4)
  t <- stratetab(list(a, b), outcomes = 1, rateratio = TRUE, ratescale = 1,
    cformat = "%12.4f", ratiodigits = 2)
  expect_identical(t$body$c4[2], "0.5000 (0.2500, 1.0000)")
  expect_identical(t$body$c5[4], "0.80 (0.23, 2.76)")
})

test_that("formatter precision and allocation limits fail before reaching C", {
  for (fmt in c("%600.500f", "%500.401e", "%500.0g", "%-999999999.2f", "%10001.2f")) {
    expect_error(stata_fmt(1, fmt), class = "tabtools_error_fmt")
  }
  expect_identical(stata_fmt(1, "%600.400f"), paste0("1.", strrep("0", 400)))
  expect_identical(stata_fmt(3.14, "%-9.2f"), "3.14     ")
  expect_error(stratetab(muse_rate_block(), cformat = "%600.500f"),
    class = "tabtools_error_format")
})
