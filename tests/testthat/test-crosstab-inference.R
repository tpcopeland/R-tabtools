test_that("Pearson and RR/RD variances match independent dense arithmetic", {
  xt_no_session()
  f <- matrix(c(40, 10, 20, 30), 2)
  x <- xt_table(f, or = TRUE, rr = TRUE, rd = TRUE)
  expect_equal(x$stored$chi2, 50 / 3)
  expect_equal(x$stored$p, stats::pchisq(50 / 3, 1, lower.tail = FALSE))
  expect_equal(x$stored$or, 6)
  expect_equal(x$stored$rr, 3)
  expect_equal(x$stored$rd, .4)
  z <- stats::qnorm(.975)
  expect_equal(c(x$stored$rr_lo, x$stored$rr_hi), 3 * exp(c(-1, 1) * z * sqrt(7 / 75)))
  expect_equal(c(x$stored$rd_lo, x$stored$rd_hi), .4 + c(-1, 1) * z / sqrt(125))
  reversed <- xt_table(f[, 2:1], or = TRUE, rr = TRUE, rd = TRUE)
  expect_equal(c(reversed$stored$or, reversed$stored$rr, reversed$stored$rd), c(1 / 6, 1 / 3, -.4))
  reversed <- xt_table(f[2:1, ], or = TRUE, rr = TRUE, rd = TRUE)
  expect_equal(c(reversed$stored$or, reversed$stored$rr, reversed$stored$rd), c(1 / 6, .5, -.4))
  expect_match(x$stored$methods, "Analysis performed in R", fixed = TRUE)
  expect_false(grepl("Analysis performed in Stata", x$stored$methods, fixed = TRUE))
})

test_that("sparse Fisher uses probability ordering and sample OR rather than conditional MLE", {
  xt_no_session()
  # Hypergeometric probabilities at a=0:4 are (1,16,36,16,1)/70.
  x <- xt_table(matrix(c(1, 3, 3, 1), 2), or = TRUE)
  expect_equal(x$stored$p, 17 / 35)
  expect_equal(x$stored$or, 1 / 9)
  expect_true(is.na(x$stored$chi2))
  expect_identical(x$stored$test_method, "Fisher exact")
  # Published Epitab cci 4 386 4 1250, 90% equal-tailed Fisher interval.
  published <- xt_table(matrix(c(1250, 386, 4, 4), 2), or = TRUE, level = 90)
  expect_equal(published$stored$or, 625 / 193)
  expect_equal(c(published$stored$or_lo, published$stored$or_hi), c(.7698467, 13.59664), tolerance = 2e-4)
  boundary <- xt_table(matrix(5, 2, 2))
  expect_identical(boundary$stored$test_method, "Pearson uncorrected")
  expect_identical(xt_table(matrix(5, 2, 2), exact = TRUE)$stored$test_method, "Fisher exact")
  expect_identical(xt_table(matrix(5, 2, 2), fisher = TRUE)$stored$test_method, "Fisher exact")
  expect_identical(xt_table(matrix(c(1, 2, 1, 2, 1, 2), 2))$stored$test_method, "Fisher exact")
})

test_that("undefined requested associations and invalid confidence inputs refuse", {
  xt_no_session()
  expect_error(xt_table(matrix(c(2, 0, 0, 2), 2), or = TRUE), class = "tabtools_error_crosstab_association")
  expect_error(xt_table(matrix(c(2, 0, 1, 2), 2), rr = TRUE), class = "tabtools_error_crosstab_association")
  expect_error(xt_table(matrix(10, 2, 3), rd = TRUE), class = "tabtools_error_crosstab_association")
  expect_equal(xt_table(matrix(c(2, 1, 1, 0), 2), or = TRUE)$stored$or, 0)
  for (level in list(0, 100, NA_real_, c(90, 95), "95", 1 + 2i)) {
    expect_error(xt_table(matrix(10, 2, 2), level = level), class = "tabtools_error_crosstab_input")
  }
  for (digits in list(-1, 7, NA_real_, .5, matrix(1, 1, 1))) {
    expect_error(xt_table(matrix(10, 2, 2), digits = digits), class = "tabtools_error_crosstab_input")
  }
})


test_that("zero risk ratio retains its point and both unavailable log-Wald limits", {
  xt_no_session()
  # Independent literal: a=0, b=3, c=5, d=4; installed _crcrr uses ln(rr).
  f <- matrix(c(4, 3, 5, 0), 2)
  x <- xt_table(f, rr = TRUE)
  expect_identical(x$stored$rr, 0)
  expect_identical(c(x$stored$rr_lo, x$stored$rr_hi), c(NA_real_, NA_real_))
  expect_error(xt_table(f[, 2:1], rr = TRUE), class = "tabtools_error_crosstab_association")
})
