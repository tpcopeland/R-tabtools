# Regression tests for the 2026-09-25 review of Phase 1 (each block names
# the finding it pins).

test_that("named colours are the ones Stata's xl() writes (finding 1)", {
  # Read back from Stata-written workbooks (headercolor(<name>), cell C2).
  xl <- c(black = "FF000000", blue = "FF0000FF", brown = "FFA52A2A", cyan = "FF00FFFF",
          dimgray = "FF696969", gold = "FFFFD700", gray = "FF808080", green = "FF008000",
          khaki = "FFF0E68C", lavender = "FFE6E6FA", lime = "FF00FF00", magenta = "FFFF00FF",
          maroon = "FF800000", navy = "FF000080", olive = "FF808000", orange = "FFFFA500",
          pink = "FFFFC0CB", purple = "FF800080", red = "FFFF0000", sienna = "FFA0522D",
          teal = "FF008080", white = "FFFFFFFF", yellow = "FFFFFF00")
  got <- vapply(names(xl), tt_parse_color, "")
  expect_identical(got, xl)
  expect_identical(tt_parse_color("NAVY"), "FF000080")
  # Accepted by Stata's validator but rejected by xl() with r(16136).
  for (nm in c("bluishgray", "dknavy", "ltblue", "gs8", "orange_red")) {
    expect_error(tt_parse_color(nm), "does not support the colour name")
  }
  expect_error(tt_parse_color("chartreuse"), "not a supported Stata colour name")
  expect_identical(tt_parse_color("200 220 240"), "FFC8DCF0")
  expect_identical(tt_parse_color("#c8dcf0"), "FFC8DCF0")
})

test_that("tables without an Excel layout fail clearly (finding 2)", {
  tt <- tt_table(data.frame(l = c("a", "b"), v = c("1", "2")), list(c("", "V")), command = "puttab")
  expect_identical(tt$layout$xlsx_rules, "none")
  expect_error(tt_write_xlsx(tt, tempfile(fileext = ".xlsx")), "No Excel layout")
  # Text sinks still work.
  f <- tempfile(fileext = ".csv")
  expect_no_error(tt_write_csv(tt, f))
})

test_that("%g handles |x| < 0.1 as Stata does (finding 3)", {
  expect_identical(stata_fmt(1e-6, "%9.0g"), "1.00e-06")
  expect_identical(stata_fmt(0.000123456789, "%9.0g"), ".0001235")
  expect_identical(stata_fmt(0.0123456789, "%8.0g"), ".012346")
  expect_identical(stata_fmt(1e-4, "%6.0g"), "1.0e-04")
  expect_identical(stata_fmt(-0.05, "%9.0g"), "-.05")
})

test_that("non-finite values format as Stata missing (finding 9)", {
  expect_identical(stata_fmt(c(Inf, -Inf, NaN, NA), "%5.1f"), rep(".", 4))
  expect_identical(stata_fmt(c(Inf, 0.5), "%9.0g"), c(".", ".5"))
})

test_that("unlabelled codes print as Stata's levelsof text (finding 4)", {
  expect_identical(level_labels(c(100000, 1e6, 0.1))$label, c(".1", "100000", "1000000"))
  expect_identical(stata_macro_text(c(-0.05, 1e-6, 12345678901234567, 0, NA)),
                   c("-.05", "1.00000000000e-06", "1.23456789012e+16", "0", "."))
  # haven character-labelled vectors show their labels (R-only input).
  x <- structure(c("b", "a", "b"), labels = c(Alpha = "a"), class = c("haven_labelled", "vctrs_vctr", "character"))
  expect_identical(level_labels(x)$label, c("Alpha", "b"))
})

test_that("column roles are inferred only from trailing special headers (finding 5)", {
  tt <- tt_table(data.frame(l = "Age", a = "1", b = "2", p = "0.5"),
                 list(c(" ", "Control", "Test", "p-value"), c("Mean±SD", "N=10", "N=48", "")),
                 command = "table1_tc")
  expect_identical(tt$cols$role, c("label", "group", "group", "p"))
  tt <- tt_table(data.frame(l = "Age", a = "1", b = "2", t = "3", s = "t test", p = "0.5", d = "0.1"),
                 list(c(" ", "A", "B", "Total", "Test", "p-value", "SMD"), c("D", "N=1", "N=2", "N=3", "", "", "")),
                 command = "table1_tc")
  expect_identical(tt$cols$role, c("label", "group", "group", "total", "test", "p", "smd"))
  tt <- tt_table(data.frame(l = "Age", t = "3", a = "1", b = "2"),
                 list(c(" ", "Total", "A", "B"), c("D", "N=3", "N=1", "N=2")), command = "table1_tc")
  expect_identical(tt$cols$role, c("label", "total", "group", "group"))
})

test_that("replacing a sheet keeps names, position, and other sheets (finding 6)", {
  skip_if_not_installed("openxlsx2")
  f <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_workbook()$add_worksheet("First")$add_data("First", "keep me")$
    add_worksheet("Table 1")$add_data("Table 1", matrix(1:4, 2), dims = "B2")$
    add_named_region("Table 1", dims = "B2:C3", name = "myrange")$
    add_worksheet("Last")$save(f)
  tt <- tt_table(data.frame(l = "Age", a = "50"), list(c(" ", "All"), c("Mean", "N=5")), command = "table1_tc")
  tt_write_xlsx(tt, f, sheet = "table 1")
  wb <- openxlsx2::wb_load(f)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), c("First", "Table 1", "Last"))
  expect_identical(openxlsx2::wb_get_named_regions(wb)$name, "myrange")
  expect_identical(openxlsx2::wb_to_df(wb, "First", col_names = FALSE)[[1]], "keep me")
  cells <- openxlsx2::wb_to_df(wb, "Table 1", col_names = FALSE)
  expect_false(any(unlist(cells) %in% c("1", "2", "3", "4")))
})

test_that("Markdown replaces an existing file without append, like Stata 2.1.12 (finding 7)", {
  tt <- tt_table(data.frame(l = "Age", a = "50"), list(c(" ", "All"), c("Mean", "N=5")), command = "table1_tc")
  f <- tempfile(fileext = ".md")
  tt_write_markdown(tt, f)
  expect_no_error(tt_write_markdown(tt, f))
  expect_identical(sum(grepl("^\\| --- \\|", readLines(f))), 1L)
  expect_no_error(tt_write_markdown(tt, f, append = TRUE))
  expect_identical(sum(grepl("^\\| --- \\|", readLines(f))), 2L)
})

test_that("default fontsize is 6-72 and bad saved defaults are ignored (finding 8)", {
  withr::local_options(tabtools.fontsize = NULL, tabtools.font = NULL)
  expect_error(tabtools_options(fontsize = 3), "between 6 and 72")
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  f <- .tt_defaults_file()
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  write.dcf(data.frame(fontsize = "big", font = "Calibri"), f)
  expect_warning(.tt_load_persisted(), "ignoring invalid saved default fontsize")
  expect_null(getOption("tabtools.fontsize"))
  expect_identical(getOption("tabtools.font"), "Calibri")
})

test_that("empty tables are refused by the xlsx writer (finding 9)", {
  tt <- tt_table(data.frame(l = character(), a = character()), list(c(" ", "All"), c("D", "N=0")),
                 command = "table1_tc")
  expect_error(tt_write_xlsx(tt, tempfile(fileext = ".xlsx")), "Nothing to export")
  tt <- tt_table(data.frame(l = "Age"), list(" ", "D"), command = "table1_tc")
  expect_error(tt_write_xlsx(tt, tempfile(fileext = ".xlsx")), "Nothing to export")
})

test_that("negative SMDs are flagged by magnitude (finding 9)", {
  skip_if_not_installed("tidyxl")
  tt <- tt_table(data.frame(l = c("Age", "Sex"), a = c("1", "2"), b = c("1", "2"), s = c("-0.250", "0.010")),
                 list(c(" ", "A", "B", "SMD"), c("D", "N=1", "N=2", "")),
                 rows = data.frame(smd = c(-0.25, 0.01)), command = "table1_tc")
  f <- tempfile(fileext = ".xlsx")
  tt_write_xlsx(tt, f)
  x <- golden_cell_styles(f, "Table 1")
  expect_true(x$bold[x$address == "E4"])
  expect_false(x$bold[x$address == "E5"])
})

test_that("a single-model p vector drives regtab highlight (finding 9)", {
  skip_if_not_installed("tidyxl")
  tt <- tt_table(data.frame(l = c("Age", "Sex"), e = c("1.10", "0.90"), ci = c("(1.0, 1.2)", "(0.8, 1.0)")),
                 list(c("", "Model", ""), c("", "OR", "95% CI")), command = "regtab",
                 style = tt_resolve_style(highlight = 0.05), meta = list(pvals = c(0.01, 0.5)))
  f <- tempfile(fileext = ".xlsx")
  expect_no_error(tt_write_xlsx(tt, f))
  x <- golden_cell_styles(f, "Regression")
  expect_identical(x$fill[x$address == "B4"], "FFFFFFCC")
  expect_identical(x$fill[x$address == "B5"], "")
})
