tiny_t1 <- function(...) {
  tt_table(
    body = data.frame(l = c("Age", "Sex", "   Male", "   Female"),
                      a = c("50\u00b110", "", "3 (60)", "2 (40)"),
                      p = c("0.24", "0.50", "", "")),
    header = list(c(" ", "All", "p-value"), c("Mean\u00b1SD", "N=5", "")),
    command = "table1_tc", ...)
}

test_that("tt_table fills metadata and validates its structure", {
  tt <- tiny_t1()
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$rows$type, c("var", "var", "level", "level"))
  expect_identical(tt$rows$block, c(1L, 2L, 2L, 2L))
  expect_identical(tt$cols$role, c("label", "group", "p"))
  expect_identical(tt$title, "")
  expect_error(tt_table(data.frame(a = "x"), list(c("a", "b"))), "header row")
  expect_error(tiny_t1(rows = data.frame(type = rep("bogus", 4))), "unknown row type")
  expect_error(tiny_t1(cols = data.frame(role = c("label", "group", "nope"))), "unknown column role")
  expect_error(tt_table(data.frame(a = "x"), list(list(text = "a", spans = data.frame(from = 1, to = 2)))),
               "merge spans")
})

test_that("as.data.frame gives the header-shaped table", {
  d <- as.data.frame(tiny_t1())
  expect_identical(dim(d), c(6L, 3L))
  expect_identical(d[2, 1], "Mean\u00b1SD")
  expect_identical(d[5, 1], "   Male")
})

test_that("print draws Stata's list box", {
  out <- utils::capture.output(print(tiny_t1(notes = "A note.")))
  expect_identical(out[1:6], c(
    "  +------------------------------+",
    "  |             All      p-value |",
    "  |------------------------------|",
    "  | Mean\u00b1SD     N=5              |",
    "  |------------------------------|",
    "  | Age         50\u00b110    0.24    |"))
  expect_true(any(out == "  |    Male     3 (60)           |"))
  expect_identical(out[length(out)], "A note.")
  r <- tt_table(data.frame(l = c("x", "  lvl"), e = c("1.00", "Reference")),
                list(c("", "M"), c("", "OR")), command = "regtab", title = "T")
  ro <- utils::capture.output(print(r))
  expect_identical(ro[1:2], c("", "T"))
  expect_true(any(ro == "  |   lvl   Reference |"))
  # Renderers follow layout fields: a new command gets default settings,
  # and any field can be overridden.
  g <- tt_table(data.frame(l = "a", v = "1"), list(c("", "V")), command = "puttab")
  expect_identical(g$layout$header_style, "plain")
  expect_identical(tt_table(data.frame(l = "a", v = "1"), list(c("", "V")), command = "regtab",
                            layout = list(align = "left"))$layout$align, "left")
  expect_error(tt_table(data.frame(l = "a"), list(""), command = ""), "command")
  expect_error(tt_table(data.frame(l = "a"), list(""), layout = list(align = "centre")), "layout")
  expect_identical(ro[length(ro)], "")
})
