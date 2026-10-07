test_that("integer categories retain literal counts and ordinary missing identity", {
  xt_no_session()
  d <- data.frame(row = c(0L, 0L, 1L, 1L), column = c(0L, 1L, 0L, 1L))
  attr(d$row, "labels") <- c(Absent = 0L, Present = 1L)
  original <- d
  result <- crosstab(d, "row", "column", label = TRUE)
  expect_identical(unname(result$stored$table), matrix(1, 2L, 2L))
  expect_identical(result$meta$axes$row$text, c("Absent", "Present"))
  expect_identical(result$meta$axes$row$missing_tag, rep(NA_character_, 2L))
  expect_identical(d, original)
  missing <- data.frame(row = c(0L, 0L, 1L, 1L, NA_integer_), column = c(0L, 1L, 0L, 1L, 0L))
  attr(missing$row, "labels") <- c(Absent = 0L, Present = 1L)
  result <- crosstab(missing, "row", "column", label = TRUE, missing = TRUE)
  expect_identical(unname(result$stored$table), matrix(c(1, 1, 1, 1, 1, 0), 3L, 2L))
  expect_identical(result$meta$axes$row$text, c("Absent", "Present", "Missing"))
  expect_identical(result$meta$axes$row$missing_tag, c(NA_character_, NA_character_, ""))
  for (bad in list(as.character(d$row), factor(d$row))) {
    invalid <- d; invalid$row <- bad
    expect_error(crosstab(invalid, "row", "column"), class = "tabtools_error_crosstab_input")
  }
})

test_that("double tagged categories remain distinct from ordinary missing and literal labels", {
  xt_no_session()
  d <- data.frame(row = c(0, 1, NA_real_, haven::tagged_na("b"), haven::tagged_na("a")),
    column = c(0L, 1L, 0L, 1L, 0L))
  attr(d$row, "labels") <- c("Named zero" = 0, "Named one" = 1)
  original <- d
  result <- crosstab(d, "row", "column", label = TRUE, missing = TRUE)
  expect_identical(unname(result$stored$table),
    matrix(c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1), 5L, 2L))
  expect_identical(result$meta$axes$row$missing_tag,
    c(NA_character_, NA_character_, "", "a", "b"))
  expect_identical(result$meta$axes$row$text,
    c("Named zero", "Named one", "Missing", "Missing (.a)", "Missing (.b)"))
  expect_true(identical(d, original, single.NA = FALSE))
  expect_identical(haven::na_tag(d$row[c(4L, 5L)]), c("b", "a"))
})
