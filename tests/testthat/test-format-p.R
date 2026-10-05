# Expected strings follow desctab.ado lines 805-822 (pdp = 3, highpdp = 2).
test_that("format_p applies the pdp/highpdp magnitude rules", {
  p <- c(0.8, 0.24, 0.10, 0.0995, 0.053, 0.005, 0.0004, 0, 0.995, 1, NA)
  expect_identical(
    tabtools:::format_p(p),
    c("0.80", "0.24", "0.10", "0.100", "0.053", "0.005", "<0.001", "<0.001",
      ">0.99", "1.00", "")
  )
})

test_that("format_p honours custom decimal places", {
  expect_identical(tabtools:::format_p(c(0.00004, 0.0123, 0.5), pdp = 4, highpdp = 3),
                   c("<0.0001", "0.0123", "0.500"))
})

test_that("format_p rejects out-of-range decimal places", {
  expect_error(tabtools:::format_p(0.5, pdp = 0), "pdp")
  expect_error(tabtools:::format_p(0.5, highpdp = 11), "highpdp")
})
