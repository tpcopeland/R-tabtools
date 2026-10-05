# Unit tests for the auto-typing building blocks (tasks 2.4, 2.4a, 2.4b).
# Golden parity lives in test-golden-classifier.R.

test_that("stata_runiform() leaves R's RNG alone and is reproducible", {
  set.seed(42)
  before <- .Random.seed
  u1 <- tabtools:::stata_runiform(5, 12345)
  expect_identical(.Random.seed, before)
  expect_identical(tabtools:::stata_runiform(5, 12345), u1)
  expect_false(identical(tabtools:::stata_runiform(5, 12346), u1))
  expect_true(all(u1 > 0 & u1 < 1))
  # Prefixes agree: the stream does not depend on n.
  expect_identical(tabtools:::stata_runiform(700, 1)[1:5], tabtools:::stata_runiform(5, 1))
  expect_identical(tabtools:::stata_runiform(0, 1), numeric())
})

test_that("stata_runiform() matches the first draws of the reference oracle", {
  # qa/tools/mt64_reference.py 12345 3 (identical to Stata 17 after set seed 12345)
  expect_equal(tabtools:::stata_runiform(3, 12345),
               c(0.357629722888425872, 0.400442617044061255, 0.689383317002768448),
               tolerance = 1e-15)
})

test_that("stata_runiform() validates its arguments", {
  expect_error(tabtools:::stata_runiform(-1, 1), "n")
  expect_error(tabtools:::stata_runiform(1.5, 1), "n")
  expect_error(tabtools:::stata_runiform(1, -1), "seed")
  expect_error(tabtools:::stata_runiform(1, 2^31), "seed")
  expect_error(tabtools:::stata_runiform(1, 0.5), "seed")
  expect_error(tabtools:::stata_runiform(1, NA), "seed")
  expect_length(tabtools:::stata_runiform(2, 0), 2L)
  expect_length(tabtools:::stata_runiform(2, 2147483647), 2L)
})

test_that("tt_swilk_subsample() returns positions in the full vector", {
  x <- c(NA, seq_len(2500), NA)
  x[c(10, 20)] <- NA
  pos <- tabtools:::tt_swilk_subsample(x)
  expect_length(pos, 2000L)
  expect_false(anyDuplicated(pos) > 0)
  expect_false(anyNA(x[pos]))
  # Small inputs: every non-missing position, in data order.
  y <- c(3, NA, 1, 2)
  expect_identical(tabtools:::tt_swilk_subsample(y), c(1L, 3L, 4L))
  # Selection depends only on which rows are non-missing, not on the values.
  expect_identical(tabtools:::tt_swilk_subsample(x * 10), pos)
})

test_that("tt_swilk() handles small and degenerate samples like Stata", {
  na4 <- list(W = NA_real_, V = NA_real_, z = NA_real_, p = NA_real_)
  r <- tabtools:::tt_swilk(c(1, 2, NA))
  expect_identical(r[names(na4)], na4)
  expect_identical(r$n, 2L)
  # Constant, n = 3: rho missing, so everything is missing (no error).
  r <- tabtools:::tt_swilk(c(5, 5, 5))
  expect_identical(r[names(na4)], na4)
  expect_identical(r$rc, 0L)
  # Constant, n >= 4: every coefficient is 0/0, correlate has no pairs.
  r <- tabtools:::tt_swilk(rep(2, 6))
  expect_identical(r[names(na4)], na4)
  expect_identical(r$rc, 2000L)
  # n = 3 exact branch: p can be slightly negative, then z is missing.
  r <- tabtools:::tt_swilk(c(1, 1, 2))
  expect_equal(r$W, 0.75)
  expect_lt(r$p, 0)
  expect_true(is.na(r$z))
  # Missing values are dropped.
  expect_identical(tabtools:::tt_swilk(c(NA, 1:20, NA)), tabtools:::tt_swilk(1:20))
})

test_that("tt_swilk() differs from shapiro.test() only through ties", {
  set.seed(7)
  x <- stats::rnorm(200)
  expect_equal(tabtools:::tt_swilk(x)$W, unname(stats::shapiro.test(x)$statistic), tolerance = 1e-5)
  ties <- round(stats::rnorm(80, 3, 0.6) * 2) / 2
  expect_gt(abs(tabtools:::tt_swilk(ties)$p - stats::shapiro.test(ties)$p.value), 0.01)
})

test_that("Stata storage emulation: float and macro text", {
  expect_identical(tabtools:::stata_float(16777217), 16777216)
  expect_identical(tabtools:::stata_float(0.5), 0.5)
  mac <- tabtools:::stata_macro_num
  expect_identical(mac(1 / 3), as.numeric(".3333333333333333"))
  expect_identical(mac(sqrt(2) * 1e-6), 1.41421356237e-06)
  expect_identical(mac(sqrt(2) * 1e-5), as.numeric(".0000141421356237"))
  expect_identical(mac(sqrt(2) * 1e-2), as.numeric(".014142135623731"))
  expect_identical(mac(sqrt(2) * 1e15), 1414213562373095)
  expect_identical(mac(sqrt(2) * 1e16), 1.41421356237e+16)
  expect_identical(mac(c(0, NA, Inf, NaN, -1 / 3)), c(0, NA, NA, NA, -as.numeric(".3333333333333333")))
})

test_that("tt_detect_vartype() follows the Stata branch order", {
  det <- tabtools:::tt_detect_vartype
  expect_identical(det(c("a", "b", "a")), "cat")
  expect_identical(det(factor(c("x", "y"))), "cat")
  expect_identical(det(c(NA_real_, NA_real_)), "contn")
  expect_identical(det(c(0, 1, 1, NA)), "bin")
  expect_identical(det(c(1, 2, 2)), "cat")
  # Two values beat value labels; labels beat the distinct-count rule.
  lab01 <- structure(c(0, 1, 0), labels = c(No = 0, Yes = 1))
  expect_identical(det(lab01), "bin")
  labmany <- structure(as.numeric(1:20), labels = c(Low = 1, High = 20))
  expect_identical(det(labmany), "cat")
  expect_identical(det(rep(1:7, 3)), "cat")
  # Continuous: Shapiro-Wilk below 2,001 values.
  set.seed(11)
  expect_identical(det(stats::rnorm(300)), "contn")
  expect_identical(det(stats::rexp(300)), "conts")
  # More than 5,000 values: moments.
  expect_identical(det(stats::rnorm(6000)), "contn")
  expect_identical(det(stats::rexp(6000)), "conts")
})

test_that("tt_detect_vartype() ignores missing values when counting", {
  det <- tabtools:::tt_detect_vartype
  expect_identical(det(c(0, 1, NA, NA, NA)), "bin")
  set.seed(3)
  x <- stats::rexp(400)
  expect_identical(det(c(x, rep(NA, 50))), det(x))
})
