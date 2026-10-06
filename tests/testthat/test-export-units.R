# puttab() and stacktab() units (tasks 7.1, 7.2): argument checks, source
# formatting edge cases, sinks, the stacking core, and the shared-core
# changes they needed (header-less tables, csv_reservedrow, replaced-sheet
# row heights). Stata parity itself is in test-golden-export.R.

hrt <- function() {
  mk <- function(term, ahr, ci) {
    d <- data.frame(term = term, ahr = ahr, ci = ci)
    attr(d$term, "label") <- "Exposure"
    attr(d$ahr, "label") <- "aHR"
    attr(d$ci, "label") <- "95% CI"
    d
  }
  list(primary = mk(c("Any HRT", "Former smoker", "Current smoker"), c("0.82", "1.14", "1.46"),
                    c("(0.69, 0.98)", "(0.97, 1.34)", "(1.21, 1.77)")),
       dose = mk(c("Low dose", "High dose"), c("0.91", "0.73"), c("(0.74, 1.12)", "(0.58, 0.92)")))
}

write_blocks <- function(path) {
  h <- hrt()
  suppressMessages({
    puttab(h$primary, xlsx = path, sheet = "Block Primary", title = "Primary", varlabels = TRUE)
    puttab(h$dose, xlsx = path, sheet = "Block Dose", title = "Dose", varlabels = TRUE)
  })
  invisible(path)
}

# ---------------------------------------------------------------------------
# puttab: arguments and sources

test_that("puttab refuses bad arguments before writing anything", {
  d <- hrt()$primary
  out <- withr::local_tempdir()
  x <- file.path(out, "a.xlsx")
  expect_error(puttab(d, open = TRUE), "requires .*xlsx")
  expect_error(puttab(d, xlsx = file.path(out, "a.xls")), ".xlsx file")
  expect_error(puttab(d, xlsx = x, sheet = "a/b"), "not allowed by Excel")
  expect_error(puttab(d, csv = file.path(out, "a.txt")), ".csv file")
  expect_error(puttab(d, mdappend = TRUE), "requires .*markdown")
  expect_error(puttab(d, markdown = file.path(out, "a.txt")), "markdown")
  expect_error(puttab(d, digits = 7), "between 0 and 6")
  expect_error(puttab(d, digits = 1.5), "whole number")
  expect_error(puttab(d, title = NA_character_), "single string")
  expect_error(puttab(d, zebra = NA), "TRUE or FALSE")
  expect_error(puttab(d, vars = "nope"), "not found")
  expect_error(puttab(d, vars = c("term", "term")), "more than once")
  expect_error(puttab(d, vars = character()), "character vector")
  expect_error(puttab(d, subset = c(TRUE, FALSE)), "one value per row")
  expect_error(puttab(d, subset = 0), "between 1 and 3")
  expect_error(puttab(d, subset = "a"), "logical vector or row numbers")
  expect_error(puttab(d, subset = rep(FALSE, 3)), "no observations")
  expect_error(puttab(d[0, ]), "no observations")
  expect_error(puttab(data.frame()), "no variables")
  expect_error(puttab(list(1, 2)), "data frame, a numeric matrix")
  expect_error(puttab(matrix("a", 1, 1)), "must be numeric")
  expect_error(puttab(matrix(numeric(), 0, 2)), "empty")
  expect_error(puttab(matrix(1, 1, 1), vars = "a"), "not allowed with a matrix")
  expect_error(puttab(matrix(1, 1, 1), subset = 1), "not allowed with a matrix")
  expect_error(puttab(puttab(d), vars = "term"), "data frame source only")
  dl <- data.frame(id = 1:2)
  dl$l <- list(1, 2)
  expect_error(puttab(dl), "List columns")
  # Matrix and data-frame columns (review F18), named.
  dm <- data.frame(g = 1:2)
  dm$m <- cbind(mean = 1:2, sd = 3:4)
  expect_error(puttab(dm), "Matrix and data-frame columns.*m")
  expect_identical(puttab(dm, vars = "g")$body[[1]], c("1", "2"))
  expect_false(file.exists(x))
})

test_that("puttab without a file returns the table visibly; with one, invisibly", {
  d <- hrt()$dose
  expect_visible(puttab(d))
  out <- withr::local_tempdir()
  expect_invisible(suppressMessages(puttab(d, csv = file.path(out, "a.csv"))))
  tt <- puttab(d, varlabels = TRUE, title = "T")
  expect_identical(tt$command, "puttab")
  expect_identical(tt$header[[1]]$text, c("Exposure", "aHR", "95% CI"))
  expect_identical(tt$stored[c("n_rows", "n_cols", "n_datarows", "source")],
                   list(n_rows = 4L, n_cols = 3L, n_datarows = 2L, source = "data"))
  expect_identical(puttab(d)$header[[1]]$text, c("term", "ahr", "ci"))
})

test_that("puttab formats columns as Stata's puttab does", {
  d <- data.frame(
    int = c(1, -2, NA),
    frac = c(0.5, -0.0004, NA),
    big = c(1e40, 2, 3),
    inf = c(Inf, -Inf, 1.25),
    fac = factor(c("a", NA, "b")),
    lgl = c(TRUE, NA, FALSE),
    chr = c("  x", NA, ""),
    lab = haven::labelled(c(1, 2, 9), c(One = 1, Two = 2)),
    dt = as.Date(c("2020-02-29", NA, "1999-12-31")),
    stringsAsFactors = FALSE
  )
  b <- as.matrix(puttab(d, digits = 1)$body)
  expect_identical(b[, 1], c("1", "-2", ""))
  expect_identical(b[, 2], c("0.5", "0.0", ""))
  # %32.0f overflows to Stata's e-notation, as stata_fmt() renders it.
  expect_identical(unname(b[1, 3]), trimws(tabtools:::stata_fmt(1e40, "%32.0f")))
  expect_identical(b[, 4], c("Inf", "-Inf", "1.2"))
  expect_identical(b[, 5], c("a", "", "b"))
  # Logicals as 1/0, in a data frame as in a matrix (review F15).
  expect_identical(b[, 6], c("1", "", "0"))
  expect_identical(puttab(matrix(c(TRUE, FALSE), 2))$body[[2]], c("1", "0"))
  expect_identical(b[, 7], c("  x", "", ""))
  expect_identical(b[, 8], c("One", "Two", "9"))
  expect_identical(b[, 9], c("29feb2020", "", "31dec1999"))
  p <- as.POSIXct(c("2020-01-02 03:04:05", NA), tz = "UTC")
  expect_identical(as.matrix(puttab(data.frame(t = p))$body)[, 1], c("02jan2020 03:04:05", ""))
})

test_that("puttab keeps a data frame's labels through subset and vars", {
  d <- hrt()$primary
  tt <- puttab(d, vars = c("ci", "term"), subset = c(FALSE, TRUE, TRUE), varlabels = TRUE)
  expect_identical(tt$header[[1]]$text, c("95% CI", "Exposure"))
  expect_identical(tt$body[[2]], c("Former smoker", "Current smoker"))
  expect_identical(puttab(d, subset = 3:2)$body[[1]], c("Former smoker", "Current smoker"))
})

test_that("the header-row rule drops only a row that repeats every label", {
  h <- data.frame(a = c("A lab", "r1"), b = c("B lab", "1"))
  attr(h$a, "label") <- "A lab"
  attr(h$b, "label") <- "B lab"
  expect_identical(puttab(h, varlabels = TRUE)$body[[1]], "r1")
  # Only with varlabels and a header, and never a lone row.
  expect_identical(puttab(h)$body[[1]], c("A lab", "r1"))
  expect_identical(puttab(h, varlabels = TRUE, noheader = TRUE)$body[[1]], c("A lab", "r1"))
  # A lone header-shaped row is data (M04): the labels are kept here, unlike
  # h[1, ], which drops them.
  one <- data.frame(a = "A lab", b = "B lab")
  attr(one$a, "label") <- "A lab"
  attr(one$b, "label") <- "B lab"
  expect_identical(puttab(one, varlabels = TRUE)$body[[1]], "A lab")
  # A blank first cell is allowed; a mismatch or a numeric column is not.
  h2 <- h
  h2$a[1] <- ""
  expect_identical(puttab(h2, varlabels = TRUE)$body[[1]], "r1")
  h3 <- h
  h3$b[1] <- "other"
  expect_identical(nrow(puttab(h3, varlabels = TRUE)$body), 2L)
  h4 <- h
  h4$n <- c(NA, 1)
  attr(h4$n, "label") <- "N"
  expect_identical(nrow(puttab(h4, varlabels = TRUE)$body), 2L)
  # The drop happens before subset: row 1 goes even when subset keeps it.
  expect_identical(puttab(h, varlabels = TRUE, subset = c(TRUE, TRUE))$body[[1]], "r1")
})

test_that("matrix stripes, default names, and the blank label header", {
  m <- matrix(1:4, 2, dimnames = list(c("eq:a", "_:b"), c("_:x", "y")))
  tt <- puttab(m)
  expect_identical(tt$header[[1]]$text, c("", "x", "y"))
  expect_identical(tt$body[[1]], c("eq:a", "b"))
  tt <- puttab(matrix(c(0.5, 1), 1))
  expect_identical(tt$header[[1]]$text, c("", "c1", "c2"))
  expect_identical(tt$body[[1]], "r1")
  expect_identical(tt$body[[3]], "1")
  expect_identical(tt$stored$source, "matrix")
})

test_that("a tt_table source keeps its cells under one flattened header", {
  fit <- glm(am ~ wt, family = binomial, data = mtcars)
  rt <- regtab(fit, models = "Model 1", title = "Reg", footnote = "Fn")
  tt <- puttab(rt)
  expect_identical(tt$header[[1]]$text, tabtools:::.md_header(rt, escape = FALSE))
  expect_true(all(startsWith(tt$header[[1]]$text[-1], "Model 1: ")))
  expect_identical(unname(as.matrix(tt$body)), unname(as.matrix(rt$body)))
  expect_identical(c(tt$title, tt$footnote), c("Reg", "Fn"))
  expect_identical(puttab(rt, title = "", footnote = "New")[c("title", "footnote")],
                   list(title = "", footnote = "New"))
  expect_length(puttab(rt, noheader = TRUE)$header, 0L)
  expect_identical(tt$stored$source, "table")
})

test_that("puttab writes every sink, reuses a sheet's spelling, and appends Markdown", {
  d <- hrt()$dose
  out <- withr::local_tempdir()
  x <- file.path(out, "a.xlsx")
  md <- file.path(out, "a.md")
  expect_message(tt <- puttab(d, xlsx = x, sheet = "Blocks", markdown = md, csv = file.path(out, "a.csv")),
                 "puttab: wrote 2 data rows x 3 cols \\(data source\\) to sheet Blocks in")
  expect_identical(tt$stored[c("sheet", "markdown_rows", "markdown_cols")],
                   list(sheet = "Blocks", markdown_rows = 2L, markdown_cols = 3L))
  expect_message(tt <- puttab(d, xlsx = x, sheet = "BLOCKS", markdown = md, mdappend = TRUE), "sheet Blocks in")
  expect_identical(tt$stored$sheet, "Blocks")
  expect_identical(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(x)), c(Blocks = "Blocks"))
  expect_identical(sum(readLines(md) == "| --- | --- | --- |"), 2L)
  # An existing Markdown file without mdappend is replaced (tabtools 2.1.12).
  suppressMessages(puttab(d, xlsx = x, markdown = md))
  expect_identical(sum(readLines(md) == "| --- | --- | --- |"), 1L)
  # A refused target stops the call before the workbook is touched.
  before <- file.info(x)$mtime
  expect_error(suppressMessages(puttab(d, xlsx = x, markdown = file.path(out, "no-dir", "a.md"))), "does not exist")
  expect_identical(file.info(x)$mtime, before)
  # csv and markdown naming one file.
  expect_error(puttab(d, csv = file.path(out, "b.csv"), xlsx = file.path(out, "b.csv")), ".xlsx")
})

test_that("noheader: no header row in any sink, a blank Markdown header, and the data kept", {
  d <- data.frame(a = c("", "x"), b = c("", "1"))
  out <- withr::local_tempdir()
  suppressMessages(tt <- puttab(d, noheader = TRUE, csv = file.path(out, "a.csv"),
                                markdown = file.path(out, "a.md"), xlsx = file.path(out, "a.xlsx")))
  # A first data row blank in every column stays (no reservedrow in puttab),
  # in the Markdown too since Stata-Tools 68c37a90 (keepblank; task C7).
  expect_identical(readLines(file.path(out, "a.csv")), c(",", "x,1"))
  expect_identical(readLines(file.path(out, "a.md")), c("|  |  |", "| --- | --- |", "|  |  |", "| x | 1 |"))
  expect_identical(tt$stored$n_rows, 2L)
  expect_identical(as.data.frame(tt)$c1, c("", "x"))
  expect_true(any(grepl("\\| x +1 \\|", capture.output(print(tt)))))
})

test_that("a one-column table writes, without the 1 x 1 footnote merge (tabtools 2.1.12)", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- file.path(out, "a.xlsx")
  suppressMessages(puttab(data.frame(make = c("a", "b")), xlsx = x, footnote = "fn", title = "t"))
  lay <- golden_sheet_layout(x, "Table")
  expect_identical(lay$merges, "A1:B1")
  s <- golden_cell_styles(x, "Table")
  expect_true(s$italic[s$address == "B5"])
  expect_identical(s$halign[s$address == "B3"], "left")
})

test_that("tt_write_xlsx() renders a puttab table from its object alone", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  a <- file.path(out, "a.xlsx")
  b <- file.path(out, "b.xlsx")
  suppressMessages(tt <- puttab(hrt()$primary, xlsx = a, sheet = "S", varlabels = TRUE, zebra = TRUE,
                                headershade = TRUE, title = "T", footnote = "F"))
  tt_write_xlsx(tt, b, sheet = "S")
  expect_length(golden_compare_styles(b, "S", a, "S", got_width_offset = 0.711, want_width_offset = 0.711), 0L)
})

# ---------------------------------------------------------------------------
# stacktab: arguments

test_that("stacktab refuses bad arguments", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  s <- "sheet(Block Dose)"
  expect_error(stacktab(s, xlsx = file.path(out, "k.xls"), sheet = "O"), ".xlsx file")
  expect_error(stacktab(s, xlsx = x), "sheet.* is required")
  expect_error(stacktab(s, xlsx = x, sheet = "O", layout = "diag"), "vstack")
  expect_error(stacktab(s, xlsx = x, sheet = "O", append = TRUE, sheetreplace = TRUE), "may not be combined")
  expect_error(stacktab(s, xlsx = x, sheet = "O", note = "a", footnote = "b"), "may not be combined")
  expect_error(stacktab(s, xlsx = x, sheet = "O", spacing = -1), "nonnegative")
  expect_error(stacktab(s, xlsx = x, sheet = "O", csv = file.path(out, "a.txt")), ".csv")
  expect_error(stacktab(s, xlsx = x, sheet = "O", mdappend = TRUE), "requires .*markdown")
  expect_error(stacktab(s, append = TRUE), "require .*xlsx")
  expect_error(stacktab(s), "Sheet blocks are read from")
  expect_error(stacktab(s, xlsx = file.path(out, "none.xlsx"), sheet = "O"), "not found")
  expect_error(stacktab(" \\ ", xlsx = x, sheet = "O"), "no blocks")
  expect_error(stacktab(c(s, s), xlsx = x, sheet = "O"), "one string")
  expect_error(stacktab(42, xlsx = x, sheet = "O"), "specification string or a list")
  expect_error(stacktab("rows(1/2)", xlsx = x, sheet = "O"), "either .*sheet.* or .*table")
  expect_error(stacktab(list(list(sheet = "a", table = puttab(hrt()$dose))), xlsx = x, sheet = "O"), "either")
  expect_error(stacktab(list(list(sheet = "Block Dose", colour = 1)), xlsx = x, sheet = "O"), "Unknown")
  expect_error(stacktab(list(list("Block Dose")), xlsx = x, sheet = "O"), "must be named")
  expect_error(stacktab(list(1), xlsx = x, sheet = "O"), "tt_table.* or a list")
  expect_error(stacktab("sheet(Block Dose) rows(3/2)", xlsx = x, sheet = "O"), "lo/hi")
  expect_error(stacktab("sheet(Block Dose) rows(a)", xlsx = x, sheet = "O"), "lo/hi")
  expect_error(stacktab(list(list(sheet = "Block Dose", rows = c(2, 5))), xlsx = x, sheet = "O"), "consecutive")
  expect_error(stacktab("sheet(Block Dose) cols(D-B)", xlsx = x, sheet = "O"), "left to right")
  expect_error(stacktab("sheet(Block Dose) cols(1-2)", xlsx = x, sheet = "O"), "column letters")
  expect_error(stacktab("sheet(Block Dose) skip(a)", xlsx = x, sheet = "O"), "must be a positive integer")
  expect_error(stacktab("sheet(Block Dose) label(unclosed", xlsx = x, sheet = "O"), "malformed")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+C"), "Malformed")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+C as \"\""), "may not be empty")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+B as X"), "with itself")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+C+D as X"), "exactly two")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+1 as X"), "not a column letter")
  expect_error(stacktab(s, xlsx = x, sheet = "O", columnmerge = "B+Z as X"), "not in the composite")
  expect_error(stacktab(s, xlsx = x, sheet = "O", borders = "left(all)"), "unrecognised")
  expect_error(stacktab(s, xlsx = x, sheet = "O", borders = "bottom(row 0)"), "between 1 and the number of rows")
  expect_error(stacktab(s, xlsx = x, sheet = "O", borders = "bottom(row 9)"), "between 1 and the number of rows")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = list(titlerowheight = 0)), "positive number")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = list(colwidth = c(A = -1))), "positive widths")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = list(colwidth = c(`1` = 5))), "not a column letter")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = list(rowheight = 3)), "Unknown")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = "colwidth(A)"), "a column and a width")
  expect_error(stacktab(s, xlsx = x, sheet = "O", style = "titlerowheight(40"), "closing parenthesis")
  expect_false("O" %in% openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(x)))
})

test_that("stacktab refuses blocks it cannot import, as Stata does", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  expect_error(stacktab("sheet(Nope)", xlsx = x, sheet = "O"), "no such sheet")
  # A cell range must lie inside the used range (Stata: r(198)).
  expect_error(stacktab("sheet(Block Dose) rows(2/9) cols(B-D)", xlsx = x, sheet = "O"), "used range")
  expect_error(stacktab("sheet(Block Dose) rows(2/3) cols(B-F)", xlsx = x, sheet = "O"), "used range")
  expect_error(stacktab("sheet(Block Dose) cols(B-F)", xlsx = x, sheet = "O"), "not found")
  # rows() alone past the end keeps nothing.
  expect_error(stacktab("sheet(Block Dose) rows(8/9)", xlsx = x, sheet = "O"), "imported 0 rows")
  expect_error(stacktab("sheet(Block Dose) rows(2/4) cols(B-D) \\ sheet(Block Primary) rows(2/5) cols(B-D)",
                        xlsx = x, sheet = "O", layout = "hstack"), "same row count")
  tt <- puttab(hrt()$dose)
  expect_error(stacktab(list(list(table = tt, rows = 2:9))), "outside")
  expect_error(stacktab(list(list(table = tt, cols = "A-F"))), "outside")
})

test_that("an existing output sheet needs append or sheetreplace; the match ignores case", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  s <- "sheet(Block Dose) rows(2/4) cols(B-D)"
  suppressMessages(stacktab(s, xlsx = x, sheet = "Out", title = "T", note = "N"))
  expect_error(stacktab(s, xlsx = x, sheet = "out"), "already exists")
  expect_message(tt <- stacktab(s, xlsx = x, sheet = "OUT", sheetreplace = TRUE), "sheet Out\\b")
  expect_identical(tt$stored$sheet, "Out")
  expect_message(tt <- stacktab(s, xlsx = x, sheet = "out", append = TRUE), "3 rows written -> sheet Out")
  expect_identical(tt$stored[c("append_start", "rows_out", "table_start")],
                   list(append_start = 5L, rows_out = 7L, table_start = "B5"))
  expect_null(tt$stored$title_cell)
  expect_null(tt$stored$note_row)
})

# ---------------------------------------------------------------------------
# stacktab: composite

test_that("the stacking core is positional and never joins on labels", {
  a <- puttab(data.frame(k = c("x", "same"), v = c("1", "2")))
  b <- puttab(data.frame(k = c("same", "y"), v = c("3", "4")))
  v <- stacktab(list(a, b))
  expect_identical(as.data.frame(v)$c1, c("k", "x", "same", "k", "same", "y"))
  expect_identical(v$meta$section_rows, c(1L, 4L))
  expect_identical(v$rows$section, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # Rows that differ are placed by position, with the F7 warning.
  expect_warning(h <- stacktab(list(a, b), layout = "hstack"), "differ at 2 positions")
  expect_identical(unname(as.matrix(as.data.frame(h))),
                   rbind(c("k", "v", "k", "v"), c("x", "1", "same", "3"), c("same", "2", "y", "4")))
  expect_identical(h$meta$section_rows, integer())
  # Narrower blocks are padded; spacing adds blank rows between blocks only.
  w <- stacktab(list(a, list(table = b, cols = "A")), spacing = 2)
  g <- as.matrix(as.data.frame(w))
  expect_identical(dim(g), c(8L, 2L))
  expect_identical(g[4:5, ], matrix("", 2, 2, dimnames = list(NULL, c("c1", "c2"))))
  expect_identical(g[6:8, 2], c("", "", ""))
  expect_identical(w$meta$section_rows, c(1L, 6L))
})

test_that("label, postfix, skip, and columnmerge follow Stata", {
  a <- puttab(data.frame(k = c("x", "y", "z"), e = c("1", "2", ""), ci = c("(a)", "", "(c)")))
  s <- stacktab(list(list(table = a, skip = 2, label = "Sec", postfix = "(p)"),
                     list(table = a, rows = 1:2)),
                columnmerge = "B+C as Est (CI)")
  g <- as.matrix(as.data.frame(s))
  expect_identical(unname(g[, 1]), c("Sec (p)", "y (p)", "z (p)", "k", "x"))
  # Merged where the second cell is not blank; the header text on every section row.
  expect_identical(unname(g[, 2]), c("Est (CI)", "2", " (c)", "Est (CI)", "1 (a)"))
  expect_identical(ncol(g), 2L)
  # "." counts as blank; letters keep their meaning after an earlier merge.
  b <- puttab(data.frame(k = "r", e1 = "1", c1 = ".", e2 = "2", c2 = "(q)"))
  s <- stacktab(list(b), layout = "hstack", columnmerge = c("B+C as E1", "D+E as E2"))
  expect_identical(unname(as.matrix(as.data.frame(s))), rbind(c("k", "E1", "E2"), c("r", "1", "2 (q)")))
  s2 <- stacktab(list(b), layout = "hstack", columnmerge = "_xcol2+_xcol3 as E1 \\ _xcol4+_xcol5 as E2")
  expect_identical(as.data.frame(s2), as.data.frame(s))
})

test_that("a Stata block string parses sub-options with quotes and nested parentheses", {
  b <- tabtools:::.stacktab_blocks('sheet("A b") rows(2/5) cols(b-d) label(x (y)) postfix((vs none)) \\ SHEET(C) skip(2)')
  expect_identical(b[[1]][c("sheet", "rows", "cols", "label", "postfix")],
                   list(sheet = "A b", rows = c(2L, 5L), cols = c(2L, 4L), label = "x (y)", postfix = "(vs none)"))
  expect_identical(b[[2]][c("sheet", "skip")], list(sheet = "C", skip = 2))
  expect_identical(tabtools:::.stacktab_rows("3", 1L), c(3L, 3L))
  expect_identical(tabtools:::.stacktab_cols(c("B", "D"), 1L), c(2L, 4L))
  expect_identical(tabtools:::.stacktab_style("colwidth(A 24 \\ c 12) titlerowheight(40)")[c("titlerowheight", "colwidth")],
                   list(titlerowheight = 40, colwidth = c(A = 24, c = 12)))
  expect_identical(tabtools:::.stacktab_borders("outer(all), bottom(row 3) bottom(row 1)", 3L),
                   list(outer = TRUE, top = FALSE, bottom = FALSE, bottom_rows = c(1L, 3L)))
})

test_that("a one-row composite (header only) writes every sink", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- file.path(out, "k.xlsx")
  a <- puttab(data.frame(k = "only", v = "row"), noheader = TRUE)
  suppressMessages(tt <- stacktab(list(a), xlsx = x, sheet = "One", title = "T", note = "N",
                                  csv = file.path(out, "a.csv"), markdown = file.path(out, "a.md")))
  expect_identical(nrow(tt$body), 0L)
  expect_identical(readLines(file.path(out, "a.csv")), c("T,", "only,row", "N,"))
  expect_identical(readLines(file.path(out, "a.md")), c("### T", "", "| only | row |", "| --- | --- |", "", "*N*"))
  expect_identical(tt$stored$markdown_rows, 0L)
  s <- golden_cell_styles(x, "One")
  expect_true(all(s$bold[s$row == 2L]))
  expect_identical(unique(s$border_bottom[s$row == 2L]), "thin")
  expect_identical(tt$stored[c("rows_out", "note_row")], list(rows_out = 2L, note_row = 3L))
})

test_that("display prints the listing, a blank line, and the note, as Stata's display", {
  a <- puttab(data.frame(k = c("x", ""), v = c("1", "")))
  out <- capture.output(invisible(stacktab(list(a), title = "T", note = "N", display = TRUE)))
  # Right-aligned, and a column at least 2 wide (Stata's list).
  expect_identical(out, c("", "T", "  +---------+", "  |  k    v |", "  |  x    1 |",
                          "  |         |", "  +---------+", "", "N", ""))
  expect_length(capture.output(invisible(stacktab(list(a)))), 0L)
})

test_that("a failed commit puts back the files already replaced", {
  out <- withr::local_tempdir()
  a <- file.path(out, "a.csv")
  writeLines("old", a)
  stage <- file.path(out, "stage")
  dir.create(stage)
  writeLines("new", file.path(stage, "1"))
  writeLines("new", file.path(stage, "2"))
  bad <- file.path(out, "missing-dir", "b.md")
  staged <- c(file.path(stage, "1"), file.path(stage, "2"))
  names(staged) <- c(a, bad)
  expect_error(tabtools:::.stacktab_commit(staged), "restored")
  expect_identical(readLines(a), "old")
  expect_false(file.exists(bad))
})

test_that("stacktab stages its files: a refused Markdown target leaves the workbook and CSV alone", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  md <- file.path(out, "no-dir", "a.md")
  before <- tools::md5sum(x)
  expect_error(stacktab("sheet(Block Dose)", xlsx = x, sheet = "O", csv = file.path(out, "a.csv"), markdown = md),
               "does not exist")
  expect_identical(tools::md5sum(x), before)
  expect_false(file.exists(file.path(out, "a.csv")))
})

test_that("stacktab replaces an existing Markdown file unless mdappend (tabtools 2.1.12)", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  md <- file.path(out, "a.md")
  writeLines("old", md)
  suppressMessages(stacktab("sheet(Block Dose)", xlsx = x, sheet = "O", markdown = md))
  first <- readLines(md)
  expect_false("old" %in% first)
  suppressMessages(stacktab("sheet(Block Dose)", xlsx = x, sheet = "O2", markdown = md, mdappend = TRUE))
  expect_identical(readLines(md), c(first, "", first))
})

# ---------------------------------------------------------------------------
# Shared core

test_that("only a plain-header table may have no header row", {
  expect_error(tt_table(body = data.frame(a = "x"), header = list(), command = "table1_tc"),
               "non-empty list of header rows")
  tt <- tt_table(body = data.frame(a = "x", b = "1"), header = list(), command = "custom")
  expect_identical(tt$cols$role, c("label", "value"))
  expect_identical(tabtools:::.md_header(tt), c("", ""))
})

test_that("converters take puttab and stacktab tables, header-less ones too", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  m <- matrix(c(1.5, 2, 3, 4), 2, dimnames = list(c("a", "b"), c("x", "y")))
  for (nh in c(FALSE, TRUE)) {
    tt <- puttab(m, noheader = nh, title = "T", footnote = "F")
    expect_s3_class(flextable::as_flextable(tt), "flextable")
    expect_s3_class(tt_as_gt(tt), "gt_tbl")
  }
  s <- stacktab(list(puttab(m), puttab(m)), columnmerge = "B+C as xy")
  expect_s3_class(flextable::as_flextable(s), "flextable")
  expect_s3_class(tt_as_gt(s), "gt_tbl")
})

test_that("replacing a sheet clears the old table's row heights (Stata's clear_sheet)", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- file.path(out, "a.xlsx")
  fit <- glm(am ~ wt, family = binomial, data = mtcars)
  long <- regtab(fit, models = strrep("A very long model label ", 4))
  tt_write_xlsx(long, x, sheet = "R")
  expect_true(2L %in% golden_sheet_layout(x, "R")$heights$row)
  tt_write_xlsx(regtab(fit, models = "Short"), x, sheet = "R")
  expect_identical(golden_sheet_layout(x, "R")$heights$row, 1L)
})


# ---------------------------------------------------------------------------
# Phase 7a review fixes (F1-F18) and the mutants that survived (M04, M05,
# M07, M19, M20, M21, M28, M44; M05 and M07 are killed by goldens P14 and P13,
# M21 by K08, M04 above).

test_that("F1: an extended missing value's label, and blanks for . and unlabelled .b", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c(1, 2.5, haven::tagged_na("a"), NA, haven::tagged_na("b")),
                       c(One = 1, Refused = haven::tagged_na("a")))
  expect_identical(puttab(data.frame(v = v))$body$c1, c("One", "2.50", "Refused", "", ""))
  # No tagged label: every missing value is blank.
  w <- haven::labelled(c(1, NA, haven::tagged_na("a")), c(One = 1))
  expect_identical(puttab(data.frame(w = w))$body$c1, c("One", "", ""))
})

test_that("C7: subset counts the rows of x before the repeated-label row is dropped (Stata since 68c37a90)", {
  h <- data.frame(a = c("A lab", "r1", "r2", "r3"), b = c("B lab", "1", "2", "3"))
  attr(h$a, "label") <- "A lab"
  attr(h$b, "label") <- "B lab"
  # Stata's `in 1/2`: the header row and r1 (golden P11); 2.1.12 counted
  # the data rows after it (review F2).
  tt <- puttab(h, subset = 1:2, varlabels = TRUE)
  expect_identical(tt$body$c1, "r1")
  expect_identical(tt$stored$n_datarows, 1L)
  expect_identical(puttab(h, subset = 3, varlabels = TRUE)$body$c1, "r2")
  expect_identical(puttab(h, subset = 4, varlabels = TRUE)$body$c1, "r3")
  expect_error(puttab(h, subset = 5, varlabels = TRUE), "between 1 and 4")
  # Row 1 outside the selection is not looked at: rows 2 and 3 are data.
  expect_identical(puttab(h, subset = 2:3, varlabels = TRUE)$body$c1, c("r1", "r2"))
  expect_identical(puttab(h, subset = c(FALSE, TRUE, TRUE, FALSE), varlabels = TRUE)$body$c1, c("r1", "r2"))
  # noembedheader: row 1 is data although it repeats the labels.
  ne <- puttab(h, varlabels = TRUE, noembedheader = TRUE)
  expect_identical(ne$body$c1, c("A lab", "r1", "r2", "r3"))
  expect_identical(ne$header[[1]]$text, c("A lab", "B lab"))
  expect_error(puttab(h, noembedheader = NA), "TRUE or FALSE")
  # Without the header rule, positions are rows of x.
  expect_identical(puttab(h, subset = 1:2)$body$c1, c("A lab", "r1"))
  # A logical subset is one value per row of x; the header row's is unused.
  expect_identical(puttab(h, subset = c(TRUE, FALSE, TRUE, TRUE), varlabels = TRUE)$body$c1, c("r2", "r3"))
})

test_that("F3: dates and date-times in Stata's display formats", {
  t <- as.POSIXct(c("2020-01-05 10:30:00.75", NA), tz = "UTC")
  expect_identical(tabtools:::.stata_datetime(t), c("05jan2020 10:30:00", ""))
  # The column's own time zone.
  expect_identical(tabtools:::.stata_datetime(as.POSIXct("2020-01-05 10:30:00", tz = "UTC") + 0,
                                              NULL), "05jan2020 10:30:00")
  tk <- as.POSIXct("2020-01-05 10:30:00", tz = "Asia/Tokyo")
  expect_identical(tabtools:::.stata_datetime(tk), "05jan2020 10:30:00")
  d <- as.Date("2021-03-05")
  expect_identical(tabtools:::.stata_datetime(d, "%tdCCYY-NN-DD"), "2021-03-05")
  expect_identical(tabtools:::.stata_datetime(d, "%tdDayname"), "05mar2021")  # unknown code: default
  expect_identical(tabtools:::.stata_datetime(d, "%tcHH:MM"), "05mar2021")     # a %tc format on a Date
  x <- as.POSIXct("2020-07-04 15:05:09.456", tz = "UTC")
  expect_identical(tabtools:::.stata_datetime(x, "%tcMonth_dd,_CCYY_hh:MM:SS.ss_am"), "July 4, 2020 3:05:09.45 pm")
  expect_identical(tabtools:::.stata_datetime(x, "%tcJJJ!/yy"), "186/20")
  # The format survives subset (`[` drops attributes).
  dd <- data.frame(d = as.Date("2021-03-04") + 1:2)
  attr(dd$d, "format.stata") <- "%tdCCYY-NN-DD"
  expect_identical(puttab(dd, subset = 2)$body$c1, "2021-03-06")
})

test_that("F5: a header-less table round-trips without gaining a header row", {
  a <- puttab(data.frame(x = c("a", "b"), y = c("1", "2")), noheader = TRUE)
  b <- puttab(a)
  expect_length(b$header, 0L)
  expect_identical(b$body, a$body)
})

test_that("F7: hstack of tt_tables warns when their rows differ, and still places them by position", {
  a <- regtab(lm(mpg ~ wt + hp, data = mtcars), models = "M1")
  b <- regtab(lm(mpg ~ hp + qsec, data = mtcars), models = "M2")
  expect_warning(s <- stacktab(list(a, b), layout = "hstack"), "wt / hp.*hp / qsec")
  g <- as.data.frame(s)
  expect_identical(g[[1]][3], "wt")
  expect_identical(g[[5]][3], "hp")
  expect_no_warning(stacktab(list(a, a), layout = "hstack"))
  # Keys, not labels, when the table has them: table1 variable and level.
  t1 <- table1_tc(mtcars, by = "am", vars = c(mpg = "contn", cyl = "cat"))
  expect_identical(tabtools:::.tt_row_keys(t1)[3:5], c("cyl:4", "cyl:6", "cyl:8"))
  # A table without keys is compared by its row labels.
  expect_warning(stacktab(list(puttab(data.frame(k = "x")), puttab(data.frame(k = "y"))), layout = "hstack"),
                 "row 2 \\(x / y\\)")
  # Sheet blocks carry no keys: no check, as in Stata.
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  expect_no_warning(stacktab("sheet(Block Primary) rows(2/4) cols(B-D) \\ sheet(Block Dose) rows(2/4) cols(B-D)",
                             layout = "hstack", xlsx = x, sheet = "H"))
})

test_that("F10: block sheets match with case, the output sheet without", {
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  expect_error(stacktab("sheet(block dose)", xlsx = x, sheet = "O"), "no such sheet")
  expect_message(stacktab("sheet(Block Dose)", xlsx = x, sheet = "O"), "sheet O\\b")
  expect_message(stacktab("sheet(Block Dose)", xlsx = x, sheet = "o", sheetreplace = TRUE), "sheet O\\b")
})

test_that("F11: Stata's space separators and a skip that names no row", {
  expect_identical(tabtools:::.stacktab_rows("2 4", 1L), c(2L, 4L))
  expect_identical(tabtools:::.stacktab_rows(" 2 / 4 ", 1L), c(2L, 4L))
  expect_identical(tabtools:::.stacktab_cols("B D", 1L), c(2L, 4L))
  a <- puttab(data.frame(k = c("x", "y", "z")))
  # A skip past the end drops nothing; since Stata-Tools 68c37a90 a skip
  # below 1 or fractional is refused (task C7).
  expect_identical(nrow(stacktab(list(list(table = a, skip = 9)))$body), 3L)
  for (sk in c(0, -1, 1.5)) {
    expect_error(stacktab(list(list(table = a, skip = sk))), "positive integer", label = paste("skip", sk))
  }
  # M28: skipping the last row.
  expect_identical(as.data.frame(stacktab(list(list(table = a, skip = 4))))$c1, c("k", "x", "y"))
  expect_error(stacktab(list(list(table = a, skip = "a"))), "positive integer")
})

test_that("F14: display without a file prints once (the table comes back invisibly)", {
  a <- puttab(data.frame(k = "x"))
  out <- capture.output(res <- withVisible(stacktab(list(a), display = TRUE)))
  expect_false(res$visible)
  expect_true(length(out) > 0L)
  expect_true(withVisible(stacktab(list(a)))$visible)
})

test_that("M19, M44: stacktab's first-column width cap and a one-column note", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- file.path(out, "k.xlsx")
  a <- puttab(data.frame(k = c(strrep("L", 60), "x"), v = "1"), noheader = TRUE)
  suppressMessages(stacktab(list(a), xlsx = x, sheet = "W", note = "n"))
  w <- golden_sheet_layout(x, "W")$widths
  expect_equal(w$width[w$col == 2L] - 0.711, 45, tolerance = 1e-6)
  b <- puttab(data.frame(k = c("x", "y")), noheader = TRUE)
  suppressMessages(stacktab(list(b), xlsx = x, sheet = "One", note = "n", title = "t"))
  expect_identical(golden_sheet_layout(x, "One")$merges, "A1:B1")
})

test_that("M20: style() keys match without regard to case (tabtools 2.1.12)", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  suppressMessages(stacktab("sheet(Block Dose) rows(2/4) cols(B-D)", xlsx = x, sheet = "S", title = "t",
                            style = "COLWIDTH(A 30) TitleRowHeight(40)"))
  lay <- golden_sheet_layout(x, "S")
  expect_equal(lay$widths$width[lay$widths$col == 2L] - 0.711, 30, tolerance = 1e-6)
  expect_identical(lay$heights$height[lay$heights$row == 1L], 40)
})

test_that("bottom(row k) draws a rule below composite row k, as tabtools 2.1.12 does", {
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  suppressMessages(stacktab("sheet(Block Primary) rows(2/5) cols(B-D)", xlsx = x, sheet = "B",
                            borders = "bottom(row 2)"))
  s <- golden_cell_styles(x, "B")
  expect_identical(unique(s$border_bottom[s$row == 3L]), "thin")   # composite row 2 at sheet row 3
  expect_true(all(is.na(s$border_bottom[s$row == 4L])))
})

test_that("rows() alone counts sheet rows, as rows() with cols() does (tabtools 2.1.12)", {
  # A composite written from B2 has no row 1; rows(2/3) alone is sheet rows
  # 2-3 in 2.1.12 (2.1.11 took the 2nd and 3rd used rows, sheet rows 3-4),
  # the same rows as rows(2/3) with cols(). Golden K08.
  out <- withr::local_tempdir()
  x <- write_blocks(file.path(out, "k.xlsx"))
  suppressMessages(stacktab("sheet(Block Primary) rows(2/5) cols(B-D)", xlsx = x, sheet = "Comp"))
  a <- stacktab("sheet(Comp) rows(2/3)", xlsx = x, sheet = "A")
  b <- stacktab("sheet(Comp) rows(2/3) cols(B-D)", xlsx = x, sheet = "B")
  expect_identical(as.data.frame(a)$c1, c("Exposure", "Any HRT"))
  expect_identical(as.data.frame(b)$c1, c("Exposure", "Any HRT"))
  # A title in A1 starts the used range at row 1: rows(3/4) alone then keeps
  # the used range's gutter column A, blank on those rows.
  suppressMessages(stacktab("sheet(Block Primary) rows(2/5) cols(B-D)", xlsx = x, sheet = "T", title = "t"))
  t <- stacktab("sheet(T) rows(3/4)", xlsx = x, sheet = "TA")
  expect_identical(as.data.frame(t)$c2, c("Any HRT", "Former smoker"))
})

test_that("F7: the hstack check uses regtab's row keys, not the labels (N19)", {
  d <- mtcars
  d$a <- factor(d$am)
  d$b <- factor(d$vs)
  attr(d$a, "label") <- "Group"
  attr(d$b, "label") <- "Group"
  ra <- regtab(lm(mpg ~ a, data = d), models = "A")
  rb <- regtab(lm(mpg ~ b, data = d), models = "B")
  # Same labels row by row ("Group", "0", "1"); different terms.
  expect_identical(ra$body[[1]], rb$body[[1]])
  expect_warning(stacktab(list(ra, rb), layout = "hstack"), "differ")
})

test_that("C7: stacktab's blocks() grammar follows Stata since 68c37a90", {
  bl <- tabtools:::.stacktab_blocks
  b <- bl("sheet(Block Dose) rows(2 4) cols(B D) \\ sheet(\"Table S3\") label(rows(3/3)) skip(9)")
  expect_identical(b[[2]]$sheet, "Table S3")
  expect_identical(b[[2]]$label, "rows(3/3)")
  expect_null(b[[2]]$rows)
  q <- bl("sheet(\"a \\\\ b (x)\") label(`\"compound \"q\"\"')")
  expect_length(q, 1L)
  expect_identical(q[[1]]$sheet, "a \\\\ b (x)")
  expect_identical(q[[1]]$label, "compound \"q\"")
  expect_error(bl("sheet(A) rowz(2)"), "unknown block suboption")
  expect_error(bl("sheet(A) sheet(B)"), "more than once")
  expect_error(bl("sheet(A) junk"), "expected a suboption")
  expect_error(bl("SHEET(A) Rows(2/3)"), NA)
})

test_that("D04: puttab and stacktab refuse sheet without xlsx, as table1_tc does", {
  d <- data.frame(t = c("A", "B"), v = c("1", "2"))
  expect_error(puttab(d, sheet = "Zed"), "`sheet` is only available when using `xlsx`")
  expect_error(stacktab(list(puttab(d)), sheet = "Zed"), "`sheet` is only available when using `xlsx`")
  expect_error(table1_tc(data.frame(g = rep(1:2, 5), x = 1:10), by = "g", vars = "x", sheet = "Z"),
               "`sheet` is only available when using `xlsx`")
})

test_that("P2-2: an explicit sheet = NULL means no sheet (the default) in puttab, stacktab and table1_tc", {
  d <- data.frame(t = c("A", "B"), v = c("1", "2"))
  expect_no_error(puttab(d, sheet = NULL))
  expect_no_error(suppressMessages(stacktab(list(puttab(d)), csv = withr::local_tempfile(fileext = ".csv"), sheet = NULL)))
  wrap <- function(tabs, xlsx = NULL, sheet = NULL, csv = NULL) stacktab(tabs, xlsx = xlsx, sheet = sheet, csv = csv)
  expect_no_error(wrap(list(puttab(d)), csv = withr::local_tempfile(fileext = ".csv")))
  expect_no_error(table1_tc(data.frame(g = rep(1:2, 5), x = 1:10), by = "g", vars = "x", sheet = NULL))
  # With xlsx, NULL takes the command's default sheet.
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(puttab(d, xlsx = f, sheet = NULL))
  expect_identical(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(f))[[1]], "Table")
})
