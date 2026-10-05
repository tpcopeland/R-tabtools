t1 <- function(...) {
  tt_table(
    body = data.frame(l = c("Age, years", "Group", "   A|B", "   say \"hi\"_x*"),
                      a = c("50\u00b110", "", "3 (60)", "2 (40)"),
                      p = c("0.24", "<0.001", "", "")),
    header = list(c(" ", "All", "p-value"), c("Mean\u00b1SD", "N=5", "")),
    command = "table1_tc", ...)
}

test_that("CSV quotes only fields with commas or quotes and keeps indents", {
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(t1(title = "Title, one", footnote = "Note."), f)
  x <- readLines(f, encoding = "UTF-8")
  expect_identical(x[1], "\"Title, one\",,")
  expect_identical(x[2], " ,All,p-value")
  expect_identical(x[4], "\"Age, years\",50\u00b110,0.24")
  expect_identical(x[7], "\"   say \"\"hi\"\"_x*\",2 (40),")
  expect_identical(x[8], "Note.,,")
  b <- readBin(f, "raw", 1e4)
  expect_false(any(b == as.raw(13L)))
  tt_write_csv(t1(), f)
  expect_identical(readLines(f)[1], " ,All,p-value")
})

test_that("Markdown escapes cells, encodes indents, and appends", {
  f <- withr::local_tempfile(fileext = ".md")
  tt_write_markdown(t1(title = "T_1", footnote = "n*"), f)
  x <- readLines(f, encoding = "UTF-8")
  expect_identical(x[1:2], c("### T\\_1", ""))
  expect_identical(x[3], "| Mean\u00b1SD | All (N=5) | p-value |")
  expect_identical(x[4], "| --- | --- | --- |")
  expect_identical(x[7], "| &nbsp;&nbsp;&nbsp;A\\|B | 3 (60) |  |")
  expect_identical(x[8], "| &nbsp;&nbsp;&nbsp;say \"hi\"\\_x\\* | 2 (40) |  |")
  expect_identical(x[9:10], c("", "*n\\**"))
  tt_write_markdown(t1(), f, append = TRUE)
  y <- readLines(f)
  expect_identical(y[11:12], c("", "| Mean\u00b1SD | All (N=5) | p-value |"))
  expect_error(tt_write_markdown(t1(), withr::local_tempfile(fileext = ".txt")), "markdown")
})

test_that("extraspace prefixes p cells in CSV/Markdown and the xlsx body only", {
  tt <- t1(meta = list(extraspace = TRUE))
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, f)
  x <- readLines(f, encoding = "UTF-8")
  expect_identical(x[1], " ,All, p-value")
  expect_identical(x[3], "\"Age, years\",50\u00b110, 0.24")
  expect_identical(x[4], "Group,,<0.001")
  expect_identical(utils::capture.output(print(tt)), utils::capture.output(print(t1())))
})

test_that("xlsx writer validates targets and replaces sheets in place", {
  skip_if_not_installed("tidyxl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  expect_error(tt_write_xlsx(t1(), sub("xlsx$", "xls", f)), ".xlsx")
  expect_error(tt_write_xlsx(t1(), f, sheet = "a/b"), "not allowed")
  expect_error(tt_write_xlsx(t1(), f, sheet = strrep("x", 32)), "31")
  expect_error(tt_write_xlsx(t1(), f, sheet = "History"), "reserved")
  expect_error(tt_write_xlsx(t1(), f, sheet = "'a"), "apostrophe")
  tt_write_xlsx(t1(title = "First"), f)
  tt_write_xlsx(t1(), f, sheet = "Second")
  tt_write_xlsx(t1(title = "Replaced"), f, sheet = "table 1")
  # Workbook order as Excel reads it (workbook.xml).
  expect_identical(unname(openxlsx2::wb_load(f)$get_sheet_names()), c("Table 1", "Second"))
  cells <- tidyxl::xlsx_cells(f, sheets = "Table 1")
  expect_identical(cells$character[cells$address == "A1"], "Replaced")
  expect_identical(cells$character[cells$address == "B6"], "   A|B")
})

test_that("xlsx styling follows the rule engine", {
  skip_if_not_installed("tidyxl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tt <- t1(style = tabtools:::tt_resolve_style(zebra = TRUE, headershade = TRUE, boldp = 0.05,
                                               highlight = 0.05, borderstyle = "academic"),
           rows = data.frame(p = c(0.24, 0.0001, NA, NA)), footnote = "Note.")
  tt_write_xlsx(tt, f, "S")
  s <- golden_cell_styles(f, "S")
  cell <- function(a) s[s$address == a, ]
  expect_identical(cell("B2")$fill, "FFDBE5F1")
  expect_identical(cell("B5")$fill, "FFFFFFCC")
  expect_identical(cell("B6")$fill, "")
  expect_identical(cell("B7")$fill, "FFEDF2F9")
  expect_true(cell("D5")$bold)
  expect_false(cell("D4")$bold)
  expect_identical(cell("B2")$border_top, "medium")
  expect_true(is.na(cell("B4")$border_left))
  expect_true(cell("B8")$italic)
  expect_identical(cell("B8")$size, 8)
  lay <- golden_sheet_layout(f, "S")
  expect_true(all(c("A1:D1", "B2:B3", "D2:D3", "B8:D8") %in% lay$merges))
})

test_that("Markdown escapes $ as \\$ for .qmd and .rmd targets, not for .md (audit A02)", {
  d <- data.frame(g = rep(c("A", "B"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), labels = c(x = "Cost ($) per visit ($)"),
                   title = "T $1", footnote = "in $")
  for (ext in c(".qmd", ".rmd", ".Rmd")) {
    f <- withr::local_tempfile(fileext = ext)
    tt_write_markdown(tab, f)
    x <- readLines(f, encoding = "UTF-8")
    expect_identical(x[1], "### T \\$1", label = ext)
    expect_true(any(startsWith(x, "| Cost (\\$) per visit (\\$) |")), label = ext)
    expect_identical(x[length(x)], "*in \\$*", label = ext)
    expect_false(any(grepl("(^|[^\\\\])\\$", x)), label = ext)
  }
  f <- withr::local_tempfile(fileext = ".md")
  tt_write_markdown(tab, f)
  x <- readLines(f, encoding = "UTF-8")
  expect_identical(x[1], "### T $1")
  expect_true(any(startsWith(x, "| Cost ($) per visit ($) |")))
  # The same through table1_tc(markdown =) and stacktab's staged write.
  q <- withr::local_tempfile(fileext = ".qmd")
  table1_tc(d, by = "g", vars = c(x = "contn"), labels = c(x = "Cost ($)"), markdown = q)
  expect_true(any(startsWith(readLines(q, encoding = "UTF-8"), "| Cost (\\$) |")))
  q2 <- withr::local_tempfile(fileext = ".qmd")
  suppressMessages(stacktab(list(tab), markdown = q2))
  expect_true(any(startsWith(readLines(q2, encoding = "UTF-8"), "| Cost (\\$) per visit (\\$) |")))
})
