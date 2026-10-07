test_that("TRUE-but-truncated commits restore every original without marking history", {
  directory <- withr::local_tempdir(pattern = "muse-copy-")
  md <- file.path(directory, "report.md"); csv <- file.path(directory, "report.csv")
  writeLines("MD ORIGINAL", md); writeLines("CSV ORIGINAL", csv)
  state <- tabtools:::.tt_sink_state; previous <- as.list(state)
  withr::defer({ rm(list = ls(state, all.names = TRUE), envir = state); list2env(previous, state) })
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = md))
  before <- state$written
  copy <- tabtools:::.tt_file_copy
  local_mocked_bindings(.tt_file_copy = function(from, to, overwrite = FALSE) {
    if (identical(to, md) && identical(basename(from), "out.md")) {
      writeLines("TRUNCATED", to)
      return(TRUE)
    }
    copy(from, to, overwrite = overwrite)
  }, .package = "tabtools")
  block <- puttab(data.frame(x = "BLOCK"), xlsx = NULL, markdown = NULL)
  expect_error(suppressMessages(stacktab(list(block), csv = csv)), "every target was restored")
  expect_identical(readLines(md), "MD ORIGINAL")
  expect_identical(readLines(csv), "CSV ORIGINAL")
  expect_identical(state$written, before)
})

test_that("effecttab addrows retain native separate XLSX cells and table border", {
  x <- data.frame(term = "x", estimate = 1, conf.low = .5, conf.high = 1.5, p.value = .1)
  t <- effecttab(x, level = 95, addrow = '"N" 32')
  expect_identical(t$rows$type, c("var", "addrow"))
  lay <- tabtools:::.xlsx_layout_regtab(t)
  merge <- lay$rules[lay$rules$op == 14L & lay$rules$r1 == 5L, ]
  expect_identical(nrow(merge), 0L)
  bottom <- lay$rules[lay$rules$op == 9L & lay$rules$r1 == 5L, ]
  expect_identical(as.integer(unlist(bottom[1, c("r1", "r2", "c1", "c2", "code")], use.names = FALSE)),
                   c(5L, 5L, 2L, 5L, 1L))
  expect_identical(lay$grid[5L, 2:5], c("N", "32", "", ""))
  path <- withr::local_tempfile(fileext = ".xlsx")
  tt_write_xlsx(t, path, sheet = "Added")
  wb <- openxlsx2::wb_load(path)
  expect_false(any(grepl("C5:E5", wb$worksheets[[1]]$mergeCells, fixed = TRUE)))
})

test_that("native stack metadata names table end and first note paragraph", {
  directory <- withr::local_tempdir(pattern = "muse-notes-")
  block <- puttab(data.frame(a = c("x", "y"), b = 1:2))
  t <- suppressMessages(stacktab(list(block), xlsx = file.path(directory, "out.xlsx"),
    sheet = "Notes", note = c("one", "two", "three")))
  expect_identical(t$stored[c("rows_out", "note_row")], list(rows_out = 4L, note_row = 5L))
  lay <- tabtools:::.xlsx_layout_stacktab(t)
  expect_identical(nrow(lay$grid), 7L)
  expect_identical(lay$grid[5:7, 2L], c("one", "two", "three"))
})
