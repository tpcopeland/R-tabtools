test_that("regression composition snapshots bind final placed publication", {
  fit <- lm(mpg ~ wt + factor(cyl), mtcars)
  note <- 'Reference $literal `note`'
  x <- regtab(fit, reftop = TRUE,
    cellnote = list(list(row = "wt", model = 1L, text = note)),
    addrow = list(list(label = "Final row", values = "value", after = "wt")))
  stamp <- x$meta$composition
  expect_identical(stamp$body, x$body)
  expect_identical(stamp$rows, x$rows)
  expect_identical(stamp$header, x$header)
  expect_identical(stamp$frame, x$meta$frame)
  expect_identical(stamp$companion, x$meta$regtab_rows)
  expect_identical(stamp$companion_hash, rlang::hash(x$meta$regtab_rows))
  row <- match("wt", x$rows$key)
  record <- stamp$companion[stamp$companion$row == row, , drop = FALSE]
  expect_identical(record$status, "masked")
  expect_true(all(is.na(record[c("estimate", "conf.low", "conf.high", "p.value")])))
  imported <- as.data.frame(x)
  expect_identical(attr(imported, "composition"), stamp)
  before <- serialize(x, NULL)
  source <- tabtools:::.ct_source(imported, 1L)
  expect_false(tabtools:::.ct_is_ref(source, row, 1L))
  expect_identical(serialize(x, NULL), before)
})

test_that("final transposed orientation refuses direct and imported composition", {
  fit <- lm(mpg ~ wt + factor(cyl), mtcars)
  x <- regtab(fit, transpose = TRUE, stats = "n")
  expect_identical(x$meta$composition$body, x$body)
  expect_identical(x$meta$frame$orientation, "transpose")
  expect_identical(x$meta$composition$frame$orientation, "transpose")
  imported <- as.data.frame(x)
  expect_identical(attr(imported, "orientation"), "transpose")
  before <- serialize(list(x, imported), NULL)
  for (source in list(x, imported)) {
    expect_error(comptab(source, rows = 1L), class = "tabtools_error_regtab_orientation")
  }
  exported <- tt_flat(x, keyed = FALSE)
  expect_identical(attr(exported, "composition_export"), TRUE)
  expect_error(comptab(exported, rows = 1L), class = "tabtools_error_composition")
  expect_identical(serialize(list(x, imported), NULL), before)
})

test_that("leaf parent provenance and final composition stamps coexist", {
  cell <- tabcell("est", b = 2, ll = 1, ul = 3)
  parent <- puttab(data.frame(label = "A", value = cell))
  before <- serialize(parent, NULL)
  x <- tt_stack(parent, parent, groups = c("First", "Second"))
  expect_identical(x$meta$composition$body, x$body)
  expect_false(is.null(x$meta$cell_provenance))
  expect_identical(serialize(parent, NULL), before)
  expect_error(tt_merge(cell, parent), class = "tabtools_error_composition")
})
