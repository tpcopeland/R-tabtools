test_that("tt_colwidth follows _tabtools_colwidth", {
  w <- tabtools:::tt_colwidth
  cells <- c("", "Domestic", "N=52", "3360 (2790, 3730)", "15\u00b14")
  r <- w(cells, firstrow = 3, minwidth = 12, maxwidth = 30, headerrow = 2, headerfloor = 22)
  expect_identical(r$maxlen, 17L)
  expect_equal(r$width, ceiling(17 * 0.85) + 2)
  expect_equal(r$hlen, 8L)
  # Display width: "\u00b1" counts one.
  expect_identical(w(c("", "", "58.3\u00b113.4"))$maxlen, 9L)
  # Header floor and wrap line count.
  long <- c("", "A very long group label that wraps", "1")
  r <- w(long, minwidth = 12, maxwidth = 30, headerrow = 2, headerfloor = 22)
  expect_equal(r$width, 18)
  expect_identical(r$hlines, 2L)
  # regtab estimate column: scale 1, pad -0.5, reference label excluded.
  est <- c("", "Logistic", "OR", "1.00", "Reference", "20247.25")
  expect_equal(w(est, scale = 1, pad = -0.5, minwidth = 7, headerrow = 2, exclude = "Reference")$width, 7.5)
  expect_identical(w(hlength = 30, blockwidth = 10)$hlines, 3L)
  expect_error(w(), "blockwidth")
})
