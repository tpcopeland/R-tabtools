test_that("var_label falls back to the name and honours overrides", {
  x <- structure(1:3, label = "Age (years)")
  expect_identical(tabtools:::var_label(x, "age"), "Age (years)")
  expect_identical(tabtools:::var_label(1:3, "age"), "age")
  expect_identical(tabtools:::var_label(x, "age", c(age = "Age")), "Age")
})

test_that("level_labels orders levels like Stata", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(3, 1, 2, 3, NA, 12), c(Low = 1, High = 3))
  lv <- tabtools:::level_labels(x)
  expect_identical(lv$code, c(1, 2, 3, 12))
  expect_identical(lv$label, c("Low", "2", "High", "12"))
  expect_identical(tabtools:::level_index(x, lv), c(3L, 1L, 2L, 3L, NA, 4L))
  # Character: C-locale order, as Stata's encode sorts.
  expect_identical(tabtools:::level_labels(c("small", "large", "Medium", NA))$label,
                   c("Medium", "large", "small"))
  # Factors keep their level order; unused levels dropped by default.
  f <- factor(c("b", "a"), levels = c("c", "b", "a"))
  expect_identical(tabtools:::level_labels(f)$label, c("b", "a"))
  expect_identical(tabtools:::level_labels(f, drop = FALSE)$label, c("c", "b", "a"))
  expect_equal(tabtools:::level_labels(c(TRUE, FALSE))$code, c(0, 1))
  expect_identical(tabtools:::level_labels(c(2.5, 1))$label, c("1", "2.5"))
})

test_that("golden fixture labels resolve like Stata's", {
  d <- golden_fixture("cohort")
  expect_identical(tabtools:::var_label(d$education, "education"), "Education level")
  expect_identical(tabtools:::level_labels(d$education)$label, c("Primary", "Secondary", "Tertiary"))
  expect_identical(tabtools:::level_labels(d$income_quintile)$label, as.character(1:5))
})
