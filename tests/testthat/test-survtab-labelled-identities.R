test_that("numeric labelled survival groups and IDs retain codes and literal labels", {
  skip_if_not_installed("haven")
  d <- data.frame(exit = c(1, 2, 3, 4, 2, 2), event = c(1, 0, 1, 0, 1, 0))
  d$g <- haven::labelled(c(1, 1, 2, 2, 1, NA_real_), c(Control = 1, Treated = 2))
  d$id <- haven::labelled(c(10, 11, 12, 13, NA_real_, 15), c(First = 10))
  original <- d
  prepared <- .sv_prepare(d, "exit", "event", "g", NULL, "id", NULL)
  expect_identical(prepared$records$record_index, 1:4)
  expect_identical(prepared$records$group, c(1L, 1L, 2L, 2L))
  expect_identical(prepared$group_values, c(1, 2))
  expect_identical(prepared$group_labels, c("Control", "Treated"))
  expect_identical(prepared$N, c(2, 2))
  expect_identical(prepared$excluded$record_index, 5:6)
  expect_identical(prepared$excluded$reason, c("missing_id", "missing_group"))
  tt <- survtab(d, "exit", "event", c(1, 2), by = "g", id = "id")
  expect_identical(tt$meta$frame$group_labels, c("Control", "Treated"))
  expect_identical(d, original)
})

test_that("literal empty survival IDs and groups remain excluded in priority order", {
  d <- data.frame(exit = c(1, 2, 3, 4, 2, 2), event = c(1, 0, 1, 0, 1, 0),
    g = c("a", "a", "b", "b", "a", ""), id = c("one", "two", "three", "four", "", "six"))
  original <- d
  prepared <- .sv_prepare(d, "exit", "event", "g", NULL, "id", NULL)
  expect_identical(prepared$records$record_index, 1:4)
  expect_identical(prepared$excluded$record_index, 5:6)
  expect_identical(prepared$excluded$reason, c("missing_id", "missing_group"))
  expect_identical(d, original)
})
