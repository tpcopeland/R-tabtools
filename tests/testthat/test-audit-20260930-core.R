test_that("factor conversion refuses ambiguous source columns", {
  d <- data.frame(first = 1:3, second = 4:6)
  names(d) <- c("x", "x")
  expect_error(tt_as_factor(d, vars = "x"), "more than one column",
               class = "rlang_error")
  expect_identical(d[[1]], 1:3)
  expect_identical(d[[2]], 4:6)

  d <- data.frame(x = 1:3)
  names(d) <- ""
  expect_error(tt_as_factor(d, vars = ""), "non-empty",
               class = "rlang_error")
  expect_identical(tt_as_factor(d, vars = character()), d)
})

test_that("oversized RGB components give a range error without changing defaults", {
  withr::local_options(tabtools.headercolor = "navy")
  for (colour in c("999999999999999999999 0 0", paste0(strrep("9", 400), " 0 0"))) {
    expect_error(tabtools_options(headercolor = colour), "between 0 and 255",
                 class = "rlang_error")
    expect_identical(getOption("tabtools.headercolor"), "navy")
  }
  expect_identical(tabtools:::tt_parse_color("000255 0 000001"), "FFFF0001")
})

test_that("display format numbers are checked before integer conversion", {
  d <- data.frame(x = 1:6)
  for (fmt in c("%99999999999999999.1f", "%99999999999999999.99999999999999999f")) {
    expect_error(table1_tc(d, vars = c(x = "contn"), format = fmt),
                 "Unsupported Stata display format", class = "rlang_error")
  }
  expect_identical(tabtools:::stata_fmt(1.25, "%9.2f"), "1.25")
})
