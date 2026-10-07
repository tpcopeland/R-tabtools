test_that("crosstab session explicitness and alias priority are local and preserve saved options", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells = 3L, tabtools.smallcells_mode = "primary"))
  f <- matrix(c(1, 9, 9, 1), 2)
  expect_identical(xt_table(f)$stored$smallcells$mode, "primary")
  expect_identical(xt_table(f, smallcells = 3)$stored$smallcells$mode, "strict")
  expect_identical(xt_table(f, smallcells_mode = NULL)$stored$smallcells$mode, "strict")
  for (args in list(list(smallcells = NULL), list(smallcells = 0), list(nosmallcells = TRUE))) {
    x <- do.call(xt_table, c(list(f), args))
    expect_identical(x$stored$smallcells, list(threshold = 0L, mode = "strict", n_masked = 0L, n_linked = 0L))
    expect_equal(unname(x$stored$table), f)
  }
  expect_identical(xt_table(f, nosmallcells = TRUE, smallcells_mode = "primary")$stored$smallcells$mode, "primary")
  expect_error(xt_table(f, smallcells = 0, nosmallcells = TRUE), class = "tabtools_error_smallcells_conflict")
  expect_identical(getOption("tabtools.smallcells"), 3L)
  expect_identical(getOption("tabtools.smallcells_mode"), "primary")
  expect_identical(.xt_alias("chosen.xlsx", 123, TRUE, TRUE), list(value = "chosen.xlsx", given = TRUE))
  expect_identical(.xt_alias("", "fallback.xlsx", TRUE, TRUE), list(value = "fallback.xlsx", given = TRUE))
  expect_identical(.xt_alias(NULL, NULL, FALSE, TRUE), list(value = NULL, given = TRUE))
})

test_that("all crosstab sinks receive identical redacted publication cells and paragraphs", {
  xt_no_session()
  directory <- withr::local_tempdir(pattern = "crosstab-sinks-")
  paths <- file.path(directory, c("out.xlsx", "out.csv", "out.md"))
  f <- matrix(c(1, 9, 9, 1), 2)
  x <- xt_table(f, smallcells = 3, xlsx = paths[1], excel = 123, csv = paths[2], markdown = paths[3],
                title = 'Literal "$money`"', footnote = c("First paragraph", "Second paragraph"),
                headershade = TRUE, zebra = TRUE, borderstyle = "academic", boldp = .05)
  expect_true(all(file.exists(paths)))
  csv <- readLines(paths[2], warn = FALSE)
  md <- readLines(paths[3], warn = FALSE)
  expect_true(any(grepl("Suppressed", csv, fixed = TRUE)))
  expect_true(any(grepl("Suppressed", md, fixed = TRUE)))
  expect_false(any(grepl("90.0%", c(csv, md), fixed = TRUE)))
  expect_true(all(vapply(c("First paragraph", "Second paragraph"), function(p) any(grepl(p, md, fixed = TRUE)), TRUE)))
  sheet <- openxlsx2::read_xlsx(paths[1], sheet = "Crosstab", col_names = FALSE)
  expect_true(any(as.matrix(sheet) == "Pearson's chi-squared test: Suppressed", na.rm = TRUE))
  expect_true(any(as.matrix(sheet) == "First paragraph", na.rm = TRUE))
  expect_true(any(as.matrix(sheet) == "Second paragraph", na.rm = TRUE))
  expect_match(paste(capture.output(print(x)), collapse = "\n"), "Suppressed", fixed = TRUE)
  flat <- tt_flat(x, keyed = FALSE)
  expect_true(is.data.frame(flat))
  expect_error(tt_flat(x, keyed = TRUE), class = "tabtools_error_flat")
  expect_false(any(grepl("90.0%", unlist(flat, use.names = FALSE), fixed = TRUE)))
  expect_true(all(is.na(attr(as.data.frame(x), "sample_accounting")$measures$value[
    attr(as.data.frame(x), "sample_accounting")$measures$metric != "reported_n"])))
})

test_that("crosstab layout matches total and merged inference geometry, early refusals preserve files", {
  xt_no_session()
  x <- xt_table(matrix(c(40, 10, 20, 30), 2), or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE,
                headershade = TRUE, zebra = TRUE, boldp = .05, footnote = c("one", "two"))
  layout <- .xlsx_layout_crosstab(x)
  expect_identical(layout$grid[2, -1], c("row", "1", "2", "Total"))
  expect_identical(layout$grid[5, 2], "Total")
  merge <- layout$rules[layout$rules$op == .OP[["merge"]], ]
  expect_true(all(6:10 %in% merge$r1))
  bold <- layout$rules[layout$rules$op == .OP[["bold"]] & layout$rules$code == 1, ]
  expect_true(all(c(6, 10) %in% bold$r1))
  directory <- withr::local_tempdir(pattern = "crosstab-refuse-")
  path <- file.path(directory, "existing.csv"); writeLines("preserve bytes", path)
  before <- readBin(path, "raw", file.info(path)$size)
  expect_error(xt_table(matrix(c(2, 0, 0, 2), 2), or = TRUE, csv = path), class = "tabtools_error_crosstab_association")
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
  expect_error(xt_table(matrix(10, 2, 2), open = TRUE), class = "tabtools_error_crosstab_input")
})
