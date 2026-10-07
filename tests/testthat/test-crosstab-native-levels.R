test_that("crosstab integer levels retain exact native levelsof text", {
  xt_no_session()
  d <- xt_data(rbind(c(10, 20, 30), c(30, 20, 10)),
               colcodes = 1e16 + c(0, 2, 4))
  original <- d
  result <- crosstab(d, "row", "column", weights = "frequency", cochran = TRUE)
  expect_identical(result$header[[1L]]$text,
    c("row", "10000000000000000", "10000000000000002", "10000000000000004", "Total"))
  expect_identical(result$meta$axes$column$text,
    c("10000000000000000", "10000000000000002", "10000000000000004"))
  expect_identical(result$meta$axes$column$code, 1e16 + c(0, 2, 4))
  expect_equal(result$stored$z_trend, -sqrt(20), tolerance = 1e-12)
  expect_identical(d, original)
  fractional <- .xt_categories(c(.1, .2), FALSE)
  expect_identical(fractional$text, c(".1", ".2"))
})
