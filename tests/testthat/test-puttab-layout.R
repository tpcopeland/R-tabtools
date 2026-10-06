# WP-2C Stata 2.5.1 layout contracts and independent-review regressions.
# External Stata comparisons are in qa/crossval_puttab_layout.R.

wp2c_bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)
wp2c_cells <- function(path, sheet = "Table") golden_cell_styles(path, sheet)
wp2c_get <- function(cells, address, field) {
  index <- match(address, cells$address)
  expect_false(anyNA(index), info = paste(address, collapse = ", "))
  unname(cells[[field]][index])
}

test_that("WP-2C F1 malformed frame ledgers never create or replace sinks", {
  d <- data.frame(term = "x", n = 1)
  attr(d, "sample_accounting") <- list(bad = TRUE)
  directory <- withr::local_tempdir()
  paths <- file.path(directory, c("out.csv", "out.md", "out.xlsx"))
  invoke <- function() stacktab(frames = list(a = d), csv = paths[1], markdown = paths[2],
                                xlsx = paths[3], sheet = "S")
  expect_error(invoke(), class = "tabtools_error_sample_accounting")
  expect_identical(file.exists(paths), rep(FALSE, 3L))
  writeLines("KEEP CSV", paths[1])
  writeLines("KEEP MD", paths[2])
  suppressMessages(puttab(data.frame(term = "KEEP"), xlsx = paths[3], sheet = "S"))
  suppressMessages(puttab(data.frame(term = "UNRELATED"), xlsx = paths[3], sheet = "Mine"))
  before <- lapply(paths, wp2c_bytes)
  expect_error(invoke(), class = "tabtools_error_sample_accounting")
  expect_identical(lapply(paths, wp2c_bytes), before)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(paths[3]))), c("S", "Mine"))
  expect_identical(wp2c_get(wp2c_cells(paths[3], "Mine"), "B3", "value"), "UNRELATED")
})

test_that("WP-2C F2 raw panel identities survive equal displays and tagged missing codes", {
  d <- data.frame(p = c(1, 1 + .Machine$double.eps, 1 + .Machine$double.eps, 2),
                  term = letters[1:4], n = 1:4)
  expect_true(d$p[1] != d$p[2])
  tt <- puttab(d, panel = "p")
  expect_identical(tt$stored$n_panels, 3L)
  expect_identical(tt$meta$puttab_panels$heading, c(1L, 3L, 6L))
  expect_identical(tt$body[[1]], c("1.00", "   a", "1.00", "   b", "   c", "2.00", "   d"))
  attr(d$p, "labels") <- c(Same = 1, Same = 2)
  expect_identical(puttab(d, panel = "p")$stored$n_panels, 3L)
  skip_if_not_installed("haven")
  d <- data.frame(p = haven::labelled(c(haven::tagged_na("a"), haven::tagged_na("b"),
                                        haven::tagged_na("b"), NA, NA, 1),
                                     labels = c(Missing = haven::tagged_na("a"),
                                                Missing = haven::tagged_na("b"))),
                  term = letters[1:6], n = 1:6)
  tt <- puttab(d, panel = "p")
  expect_identical(tt$stored$n_panels, 3L)
  expect_identical(tt$body[[1]], c("Missing", "   a", "Missing", "   b", "   c", "d", "e", "1", "   f"))
})

test_that("WP-2C F3 frame display formats preserve mapped tagged missing labels in every sink", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c(1, NA, haven::tagged_na("a"), haven::tagged_na("b"), haven::tagged_na("c")),
                       labels = c(One = 1, Refused = haven::tagged_na("a"), Unknown = haven::tagged_na("b")))
  attr(v, "format.stata") <- "%9.0g"
  d <- data.frame(term = letters[1:5], value = v)
  before <- d
  directory <- withr::local_tempdir()
  tt <- suppressMessages(stacktab(frames = list(a = d), csv = file.path(directory, "a.csv"),
                                  markdown = file.path(directory, "a.md"),
                                  xlsx = file.path(directory, "a.xlsx"), sheet = "S"))
  expect_identical(tt$body[[2]], c("One", "", "Refused", "Unknown", ""))
  expect_identical(d, before)
  expect_identical(readLines(file.path(directory, "a.csv")),
                   c("term,value", "a,One", "b,", "c,Refused", "d,Unknown", "e,"))
  expect_identical(readLines(file.path(directory, "a.md")),
                   c("| term | value |", "| --- | --- |", "| a | One |", "| b |  |", "| c | Refused |", "| d | Unknown |", "| e |  |"))
  expect_identical(wp2c_get(wp2c_cells(file.path(directory, "a.xlsx"), "S"), paste0("C", 3:7), "value"),
                   c("One", "", "Refused", "Unknown", ""))
  later <- file.path(directory, "later.md")
  tt_write_markdown(tt, later)
  expect_identical(wp2c_bytes(later), wp2c_bytes(file.path(directory, "a.md")))
})

test_that("WP-2C F4 actual workbook and converters apply zebra after panel header fills", {
  d <- data.frame(p = c("A", "B"), term = c("a", "b"), n = 1:2)
  for (inline in c(FALSE, TRUE)) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    tt <- suppressMessages(puttab(d, panel = "p", panelheader = list(text = c("", "N")),
                                 panelinline = inline, headershade = TRUE, zebra = TRUE,
                                 headercolor = "blue", zebracolor = "pink", xlsx = path))
    cells <- wp2c_cells(path)
    expect_identical(wp2c_get(cells, "B2", "fill"), "FF0000FF")
    expect_identical(wp2c_get(cells, if (inline) c("B3", "B5") else c("B4", "B7"), "fill"),
                     if (inline) rep("FF0000FF", 2L) else c("FFFFC0CB", "FF0000FF"))
    if (requireNamespace("flextable", quietly = TRUE)) {
      ft <- flextable::as_flextable(tt)
      expect_identical(unname(ft$body$styles$cells$background.color$data[, 1]),
                       if (inline) c("#0000FF", "#FFC0CB", "#0000FF", "#FFC0CB") else
                         c("transparent", "#FFC0CB", "transparent", "#FFC0CB", "#0000FF", "#FFC0CB"))
    }
  }
  # An inline header can also land on an even body row after a two-row panel.
  d <- data.frame(p = c("A", "A", "B"), term = letters[1:3], n = 1:3)
  path <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(puttab(d, panel = "p", panelheader = list(text = c("", "N")),
                         panelinline = TRUE, headershade = TRUE, zebra = TRUE,
                         headercolor = "blue", zebracolor = "pink", xlsx = path))
  expect_identical(wp2c_get(wp2c_cells(path), c("B3", "B6"), "fill"), c("FF0000FF", "FFFFC0CB"))
})

test_that("WP-2C F5 spans preserve literals and omit comma before a blank header", {
  d <- data.frame(term = "x", n = 1000, rate = 1.2)
  attr(d$n, "label") <- "   "
  attr(d$rate, "label") <- " Rate (95% CI) "
  span <- ' Count | $macro `x` \\ "quoted" '
  directory <- withr::local_tempdir()
  tt <- suppressMessages(puttab(d, varlabels = TRUE, spanheader = list(list(text = span, first = 2L, last = 3L)),
                               csv = file.path(directory, "a.csv"), markdown = file.path(directory, "a.md"),
                               xlsx = file.path(directory, "a.xlsx")))
  before <- tt
  expect_identical(tt$stored$n_spans, 1L)
  expect_identical(tt$header[[1]]$text, c("", span, ""))
  first <- readLines(file.path(directory, "a.md"))[1]
  expect_match(first, "Count \\|", fixed = TRUE)
  expect_match(first, '"quoted" |', fixed = TRUE)
  expect_identical(tabtools:::.puttab_md_header(tt)[2], span)
  expect_identical(tabtools:::.puttab_md_header(tt)[3], paste0(span, ",  Rate (95% CI) "))
  expect_true(any(grepl("Count | $macro `x`", readLines(file.path(directory, "a.csv")), fixed = TRUE)))
  expect_identical(golden_sheet_layout(file.path(directory, "a.xlsx"), "Table")$merges, c("A1:D1", "C2:D2"))
  expect_identical(wp2c_get(wp2c_cells(file.path(directory, "a.xlsx")), "C2", "value"), span)
  tt_write_markdown(tt, file.path(directory, "later.md"))
  expect_identical(tt, before)
  expect_identical(wp2c_bytes(file.path(directory, "later.md")), wp2c_bytes(file.path(directory, "a.md")))
})

test_that("WP-2C F6 frames replace their target sheet and preserve unrelated sheets", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(puttab(data.frame(term = "KEEP"), xlsx = path, sheet = "Mine"))
  suppressMessages(puttab(data.frame(term = "OLD"), xlsx = path, sheet = "S"))
  before <- wp2c_cells(path, "Mine")
  tt <- suppressMessages(stacktab(frames = list(a = data.frame(term = "NEW")), xlsx = path, sheet = "s"))
  expect_identical(tt$stored$sheet, "S")
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))), c("Mine", "S"))
  expect_identical(wp2c_cells(path, "Mine"), before)
  expect_identical(wp2c_get(wp2c_cells(path, "S"), "B3", "value"), "NEW")
  # The ordinary block path still refuses an existing sheet without authorization.
  snapshot <- wp2c_bytes(path)
  expect_error(stacktab(list(puttab(data.frame(term = "BLOCK"))), xlsx = path, sheet = "S"), "already exists")
  expect_identical(wp2c_bytes(path), snapshot)
})

test_that("WP-2C borders, user rules, and strict coordinates reach real workbook cells", {
  d <- data.frame(term = c("a", "b"), n = 1:2, value = 3:4)
  for (style in c("default", "thin", "medium", "academic")) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    suppressMessages(puttab(d, borderstyle = style, hlines = 2, vlines = 2, boldrows = 2, xlsx = path))
    cells <- wp2c_cells(path)
    code <- if (style == "medium") "medium" else "thin"
    expect_identical(wp2c_get(cells, "C4", "border_top"), code)
    expect_identical(wp2c_get(cells, c("C2", "C3", "C4"), "border_right"), rep(code, 3L))
    expect_identical(wp2c_get(cells, c("B4", "C4", "D4"), "bold"), rep(TRUE, 3L))
    expect_identical(wp2c_get(cells, "B2", "border_top"), if (style == "academic") "medium" else code)
    expect_identical(wp2c_get(cells, "B3", "border_left"), if (style == "academic") NA_character_ else code)
    expect_identical(wp2c_get(cells, "B3", "border_right"), if (style == "academic") NA_character_ else code)
  }
  directory <- withr::local_tempdir()
  targets <- file.path(directory, c("a.csv", "a.md", "a.xlsx"))
  for (option in c("hlines", "boldrows", "vlines")) {
    for (bad in list(0, -1, 1.5, Inf, NA_real_, "1", 4)) {
      args <- c(list(x = d, csv = targets[1], markdown = targets[2], xlsx = targets[3]), stats::setNames(list(bad), option))
      expect_error(do.call(puttab, args), class = "tabtools_error_layout")
      expect_identical(file.exists(targets), rep(FALSE, 3L))
    }
  }
})

test_that("WP-2C ordered panels exclude source helper columns and preserve selected header text", {
  d <- data.frame(p = c("B", "A", "B", ""), term = letters[1:4], n = 1:4,
                  left = c("", "", "", ""), right = c("B first", "A", "B last", ""))
  tt <- puttab(d, vars = names(d), subset = 2:4, panel = "p", panelheader = c("left", "right"), noindent = TRUE)
  expect_identical(tt$header[[1]]$text, c("term", "n"))
  expect_identical(tt$body[[1]], c("A", "", "b", "B", "", "c", "d"))
  expect_identical(tt$body[[2]], c("", "A", "2", "", "B last", "3", "4"))
  expect_identical(tt$stored[c("n_panels", "n_datarows", "n_cols")], list(n_panels = 2L, n_datarows = 7L, n_cols = 2L))
  expect_error(puttab(d, panel = "p", panelheader = "right"), class = "tabtools_error_layout")
  expect_error(puttab(d, panel = "p", panelheader = c("p", "right")), class = "tabtools_error_layout")
  expect_error(puttab(d, panel = "p", panelheader = c("term", "n")), class = "tabtools_error_layout")
  expect_error(puttab(d, panelheader = list(text = c("", "N"))), class = "tabtools_error_layout")
  expect_error(puttab(d, noindent = TRUE), class = "tabtools_error_layout")
  expect_error(puttab(d, panelinline = TRUE), class = "tabtools_error_layout")
})

test_that("WP-2C inline noheader Markdown promotes structural rows without changing data roles", {
  d <- data.frame(p = c("A", "A", "B", ""), term = letters[1:4], n = 1:4)
  directory <- withr::local_tempdir()
  tt <- suppressMessages(puttab(d, panel = "p", panelheader = '"" "N"', panelinline = TRUE,
                               noheader = TRUE, csv = file.path(directory, "a.csv"),
                               markdown = file.path(directory, "a.md")))
  expect_length(tt$header, 0L)
  expect_identical(tt$stored$markdown_rows, nrow(tt$body) - 1L)
  expect_identical(readLines(file.path(directory, "a.md"))[1:3],
                   c("| A | N |", "| --- | --- |", "| &nbsp;&nbsp;&nbsp;a | 1 |"))
  expect_identical(readLines(file.path(directory, "a.csv"))[1], "A,N")
  expect_error(puttab(d, panel = "p", panelheader = list(text = c("Label", "N")), panelinline = TRUE), class = "tabtools_error_layout")
  blank <- puttab(d, panel = "p", panelheader = list(text = c("", "")), panelinline = TRUE)
  expect_identical(blank$meta$puttab_panels$heading, c(1L, 4L))
  expect_length(blank$meta$puttab_panels$inline, 0L)
  plain <- puttab(data.frame(term = "a", n = 1), noheader = TRUE)
  tt_write_markdown(plain, file.path(directory, "plain.md"))
  expect_identical(readLines(file.path(directory, "plain.md"))[1:3], c("|  |  |", "| --- | --- |", "| a | 1 |"))
  withr::local_options(tabtools.headershade = TRUE)
  expect_true(puttab(d)$style$headershade)
  expect_false(puttab(d, headershade = FALSE)$style$headershade)
})

test_that("WP-2C malformed spans and format arguments fail with classed layout errors", {
  d <- data.frame(term = "a", n = 1, value = 2)
  for (span in list(list(list(text = "", first = 2)), list(list(text = "N", first = 2, last = 1)),
                    list(list(text = "N", first = 4)), list(list(text = "N", first = 2), list(text = "V", first = 2)),
                    list(list(text = "N", first = 1.5)), '"N" 2 \\ \\ "V" 3', 'N 2', '"N" x')) {
    expect_error(puttab(d, spanheader = span), class = "tabtools_error_layout")
  }
  expect_error(puttab(d, spanheader = '"N" 2', noheader = TRUE), class = "tabtools_error_layout")
  for (fmt in list(NA_character_, "%9s", "%12.32f", "%td", c("%12.0f", "%9.0g"))) {
    expect_error(puttab(d, nformat = fmt), class = "tabtools_error_layout")
  }
  expect_error(puttab(puttab(d), nformat = "%12.0fc"), class = "tabtools_error_layout")
  expect_error(puttab(d, panel = "term", panelheader = list(text = NULL)), class = "tabtools_error_layout")
  expect_error(puttab(d, panelinline = NA), class = "tabtools_error_layout")
})

test_that("WP-2C count formats retain dates, literal labels, decimals and imported fc versus gc", {
  d <- data.frame(term = c("$macro `literal`", "second"), count = c(1234567890, 2000),
                  rate = c(1234.5, -.0004), date = as.Date(c("2020-01-01", NA)))
  attr(d$rate, "format.stata") <- "%12.2fc"
  tt <- puttab(d, nformat = " %6.0fc ", digits = 2)
  expect_identical(tt$body[[1]], d$term)
  expect_identical(tt$body[[2]], c("1,234,567,890", "2,000"))
  expect_identical(tt$body[[3]], c("1,234.50", "0.00"))
  expect_identical(tt$body[[4]], c("01jan2020", ""))
  attr(d$count, "format.stata") <- "%12.0gc"
  expect_identical(puttab(d)$body[[2]], c("1234567890", "2000"))
  attr(d$count, "format.stata") <- "%12.0fc"
  expect_identical(puttab(d)$body[[2]], c("1,234,567,890", "2,000"))
  m <- matrix(c(1234567890, 2000), 2L, dimnames = list(c("a", "b"), "N"))
  expect_identical(puttab(m, nformat = "%6.0fc")$body[[2]], c("1,234,567,890", "2,000"))
})

test_that("WP-2C frame headers, identities, width validation and legacy columnmerge contracts hold", {
  d <- data.frame(term = c("Term", "a"), n = c("N", "1"))
  attr(d$term, "label") <- "Term"
  attr(d$n, "label") <- "N"
  b <- d
  b[2, ] <- c("b", "2")
  attr(b$n, "label") <- "Other"
  b[1, 2] <- "Other"
  tt <- stacktab(frames = list(first = list(data = d, label = "Same"), second = list(data = b, label = "Same"),
                               third = puttab(d, varlabels = TRUE)))
  expect_identical(tt$header[[1]]$text, c("Term", "N"))
  expect_identical(tt$body[[1]], c("Same", "   a", "Same", "   b", "a"))
  expect_identical(tt$stored[c("n_frames", "frames", "n_panels", "blocks_loaded")],
                   list(n_frames = 3L, frames = c("first", "second", "third"), n_panels = 2L, blocks_loaded = 3L))
  expect_error(stacktab(frames = list(a = d, a = b)), class = "tabtools_error_layout")
  expect_error(stacktab(frames = list(d, data.frame(term = "x"))), class = "tabtools_error_layout")
  expect_error(stacktab(frames = list(d), spacing = 0), class = "tabtools_error_layout")
  expect_error(stacktab(frames = list(d), layout = "vstack"), class = "tabtools_error_layout")
  expect_error(stacktab(frames = list(d), append = FALSE), class = "tabtools_error_layout")
  a <- puttab(data.frame(term = "a", estimate = "1", ci = "(0, 2)"))
  out <- stacktab(list(a, a), columnmerge = 'B+C as aHR (95% CI)')
  expect_identical(out$header[[1]]$text, c("term", "aHR (95% CI)"))
  expect_identical(out$body[[2]], c("1 (0, 2)", "aHR (95% CI)", "1 (0, 2)"))
})

test_that("WP-2C rendered help scopes frame replacement and sequential sink guarantees", {
  path <- withr::local_tempfile(fileext = ".txt")
  tools::Rd2txt(test_path("..", "..", "man", "stacktab.Rd"), out = path,
                options = list(underline_titles = FALSE))
  text <- paste(readLines(path), collapse = " ")
  text <- gsub("[[:space:]]+", " ", text)
  expect_match(text, "In block mode an existing sheet", fixed = TRUE)
  expect_match(text, "created or replaced", fixed = TRUE)
  expect_match(text, "writes CSV, Markdown and Excel sequentially", fixed = TRUE)
  expect_match(text, "a later write failure can leave an earlier sink written", fixed = TRUE)
})
