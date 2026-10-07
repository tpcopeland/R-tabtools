test_that("huge probability weights retain the literal two-group SMD", {
  g <- rep(1:2, each = 4L)
  x <- c(1:4, 3:6)
  w <- rep(2^1020, 8L)
  before <- w
  # Both groups have sample variance 5/3, and means 2.5 and 4.5.
  expect_equal(tabtools:::.t1w_smd("contn", x, g, w, "wt"),
               -2 / sqrt(5 / 3), tolerance = 1e-12)
  # A common scale is selected before pair filtering; an unused third
  # group must not alter the balance statistic of the declared pair.
  expect_equal(tabtools:::.t1w_smd("contn", c(x, 100), c(g, 3L),
               c(w, 2^1021), "wt"), -2 / sqrt(5 / 3), tolerance = 1e-12)
  expect_identical(w, before)
})

test_that("huge probability weights retain binary and categorical SMDs", {
  g <- rep(1:2, each = 4L)
  b <- c(0, 0, 0, 1, 0, 1, 1, 1)
  w <- rep(2^1020, 8L)
  # Proportions 1/4 and 3/4: pooled Bernoulli variance is 3/16.
  expect_equal(tabtools:::.t1w_smd("bin", b, g, w, "wt"),
               -2 / sqrt(3), tolerance = 1e-12)
  # The two-level categorical form is the absolute binary distance.
  expect_equal(tabtools:::.t1w_smd("cat", b + 1L, g, w, "wt"),
               2 / sqrt(3), tolerance = 1e-12)
})

test_that("multi-group huge-weight SMDs retain their own population scales", {
  g <- rep(1:3, each = 4L)
  x <- c(1:4, 3:6, 5:8)
  w <- rep(2^1020, 12L)
  # Group means 2.5,4.5,6.5; all group variances 5/3.
  expect_equal(tabtools:::.t1_smd_multi("contn", x, g, 3L, "maxpair", w, "wt"),
               4 / sqrt(5 / 3), tolerance = 1e-12)
  # Population reference has unweighted mean4.5 and SS47, variance47/11.
  expect_equal(tabtools:::.t1_smd_multi("contn", x, g, 3L, "population", w, "wt"),
               2 / sqrt(47 / 11), tolerance = 1e-12)
  # Nonuniform group weights change the means to3,5,7; the pooled
  # reference remains unweighted under wt(), as the declared form requires.
  nw <- rep(1:4, 3L) * 2^1018
  expect_equal(tabtools:::.t1_smd_multi("contn", x, g, 3L, "population", nw, "wt"),
               2.5 / sqrt(47 / 11), tolerance = 1e-12)
  # Frequency weights count records, so they cannot be rescaled.
  expect_identical(tabtools:::.t1w_smd_weights(c(1, 2, 3), c(1, 1, 2), "fw"), c(1, 2, 3))
})

test_that("finite weighted products recover an overflowing sum without losing counts", {
  x <- 3000 + seq_len(20L)
  w <- rep(1e300, 20L)
  before <- w
  # Each weighted square is finite and in native range; their sum is not.
  expect_true(all(w * x^2 < 2^1023))
  expect_gt(sum(w * x^2), 2^1023)
  z <- tabtools:::.t1w_cont_stats(x, w, rep(1, 20L), "contn", "wt")
  expect_equal(z$n, 20)
  expect_equal(z$a, 3010.5, tolerance = 1e-12)
  expect_equal(z$b, sqrt(35), tolerance = 1e-12)
  expect_true(is.na(z$c))
  expect_identical(w, before)
  # The established finite quotient of the centered residual stays intact.
  stable <- tabtools:::.t1w_cont_stats(seq_len(20L), rep(1e200, 20L),
                                      rep(1, 20L), "contn", "wt")
  expect_equal(stable$a, 10.5, tolerance = 1e-12)
  expect_equal(stable$b, sqrt(35), tolerance = 1e-12)
})

test_that("default rate labels retain fractional and compact exponent units", {
  d <- data.frame(g = "A", e = 1, y = 10)
  before <- d
  cases <- list(c(1e-6, "1e-06"), c(1.5e-7, "1.5e-07"), c(.5, "0.5"),
                c(1000, "1,000"), c(1500.5, "1,500.5"), c(.001, "0.001"))
  for (case in cases) {
    per <- as.numeric(case[1L])
    z <- ratetab(d, "g", "e", "y", per = per)
    expect_identical(z$header[[2L]]$text[4L], paste0("Per ", case[2L], " PY (95% CI)"))
    expect_equal(unname(z$stored$estimates[1L, "rate"]), per / 10, tolerance = 1e-13)
    expect_true(grepl(paste0("Incidence rates per ", case[2L], " person-years"),
                     z$stored$methods, fixed = TRUE))
    expect_identical(attr(z$meta$saved_data$rate, "label"),
                     paste0("Rate per ", case[2L], " person-time"))
  }
  custom <- ratetab(d, "g", "e", "y", per = 1e-6, unitlabel = "micro-unit")
  expect_identical(custom$header[[2L]]$text[4L], "Per micro-unit PY (95% CI)")
  expect_equal(unname(custom$stored$estimates[1L, "rate"]), 1e-7, tolerance = 1e-13)
  expect_identical(d, before)
})
