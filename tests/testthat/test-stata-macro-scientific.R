test_that("all-scientific macro codes avoid an empty fixed formatter call", {
  expect_identical(stata_macro_text(c(1e16, -1e16, 1e-6, -1e-6)),
                   c("1.00000000000e+16", "-1.00000000000e+16",
                     "1.00000000000e-06", "-1.00000000000e-06"))
  expect_identical(stata_macro_text(c(0, NA_real_, Inf, -Inf)), c("0", ".", ".", "."))
  expect_identical(stata_macro_text(numeric()), character())
  expect_identical(stata_macro_text(c(1e16, .25, -2)),
                   c("1.00000000000e+16", ".25", "-2"))
})
