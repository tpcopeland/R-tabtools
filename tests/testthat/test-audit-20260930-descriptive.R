test_that("Table 1 refuses complex quantities before discarding their imaginary parts", {
  d <- data.frame(g = rep(1:2, each = 2), x = c(1 + 2i, 2 + 3i, 3 + 4i, 4 + 5i))
  before <- d
  expect_error(table1_tc(d, by = "g", vars = c(x = "contn")), "x.*complex", class = "rlang_error")
  expect_error(table1_tc(d, vars = "x"), "x.*complex", class = "rlang_error")
  expect_error(table1_tc(d, by = "x", vars = c(g = "cat")), "by.*complex", class = "rlang_error")
  expect_identical(d, before)
})

test_that("small-cell certification refuses non-finite counts and inexact integer capacities", {
  for (bad in c(Inf, -Inf, NA_real_, NaN)) {
    expect_error(tabtools:::tt_smallcells(matrix(c(1, bad), 1), 3),
                 "finite", class = "tabtools_smallcells_input")
  }
  expect_error(tabtools:::tt_smallcells(matrix(c(1, 1e20), 1), 3, rowexact = 1),
               "below 2\\^53", class = "tabtools_smallcells_input")
  # Exact row total plus a large exact cell would disclose the small cell.
  # This block remains well inside double's exact-integer arithmetic range.
  safe <- tabtools:::tt_smallcells(matrix(c(1, 1e12), 1), 3, rowexact = 1)
  expect_identical(unname(safe$mask), matrix(c(1, 2), 1))
})

test_that("frequency tables refuse uncertifiable precision only when smallcells is requested", {
  d <- data.frame(g = c(1, 2), x = c(0, 1), f = c(1, 1e20))
  expect_error(table1_tc(d, vars = c(x = "cat"), by = "g", fweight = "f",
                         smallcells = 3, total = "after"),
               "below 2\\^53", class = "tabtools_smallcells_input")
  out <- table1_tc(d, vars = c(x = "cat"), by = "g", fweight = "f", nopvalue = TRUE)
  expect_s3_class(out, "tt_table")
  expect_identical(out$header[[2]]$text[2:3], c("N=1", "N=1.00000e+20"))
})

test_that("native Stata RNG checks integer lengths before conversion and leaves R state alone", {
  withr::local_preserve_seed()
  set.seed(614)
  seed <- .Random.seed
  kind <- RNGkind()
  expect_error(.Call(tabtools:::tt_stata_runiform_c, 1.5, 1), "whole number")
  expect_error(tabtools:::stata_runiform(1e20, 1), "vector length limit")
  expect_error(tabtools:::stata_runiform(Inf, 1), "whole number")
  expect_identical(.Random.seed, seed)
  expect_identical(RNGkind(), kind)
  expect_identical(tabtools:::stata_runiform(0, 1), numeric())
  expect_equal(tabtools:::stata_runiform(3, 12345),
               c(0.35762972288842593, 0.40044261704406114, 0.68938331700276839), tolerance = 1e-15)
})

test_that("unequal frequency weights and missing values preserve the two-sample p-value", {
  d <- data.frame(g = rep(1:2, each = 4), x = c(1, NA, 4, 8, 2, 5, NA, 9),
                  f = c(1, 3, 2, 4, 2, 1, 5, 3))
  expanded <- d[rep(seq_len(nrow(d)), d$f), ]
  expected <- stats::t.test(expanded$x[expanded$g == 1], expanded$x[expanded$g == 2])$p.value
  out <- table1_tc(d, by = "g", vars = c(x = "contn"), fweight = "f")
  expect_equal(unname(out$stored$table[1, "p_value"]), expected, tolerance = 1e-12)
})
