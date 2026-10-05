# Self-tests for the golden harness (IMPLEMENTATION_PLAN.md §5, task 0.5).
# Each comparator must accept the golden itself and reject deliberately
# broken input; the manifest must cover the §8 catalogue.

# ---------------------------------------------------------------------------
# Manifest and golden inventory

test_that("scenario manifest is well formed", {
  sc <- golden_scenarios()
  expect_named(sc, c("id", "command", "phase", "fixture", "description", "stata_setup",
                     "stata_call", "r_call", "compare", "mask"))
  expect_false(anyDuplicated(sc$id) > 0)
  expect_true(all(sc$command %in% c("table1_tc", "regtab", "puttab", "stacktab", "stratetab", "effecttab", "wttab",
                                    "comptab", "hrcomptab")))
  expect_true(all(sc$phase %in% c(2:5, 7)))
  expect_true(all(sc$compare %in% c("exact", "tolerance", "structure")))
  fx <- unique(sc$fixture)
  if (!golden_in_source()) fx <- setdiff(fx, golden_source_only_fixtures)
  expect_true(all(file.exists(golden_path("fixtures", paste0(fx, ".dta")))))
  tokens <- unique(unlist(lapply(sc$mask, golden_mask)))
  expect_true(all(tokens %in% c(names(golden_mask_headers), "pstyle")))
  expect_true(all(startsWith(sc$stata_call, sc$command)))
  # Every R call parses (the functions it calls arrive in later phases).
  for (i in seq_len(nrow(sc))) expect_no_error(parse(text = sc$r_call[i]))
})

test_that("manifest covers every scenario in the plan's §8 catalogue", {
  ids <- golden_scenarios()$id
  catalogue <- c(sprintf("T%02d", 1:35), "T03b", "T03c", sprintf("R%02d", 1:29), "R13b", "R13c")
  covered <- vapply(catalogue, function(x) any(ids == x | startsWith(ids, x) & grepl("^[a-z]$", substring(ids, nchar(x) + 1L))), TRUE)
  expect_true(all(covered), label = paste("missing:", paste(catalogue[!covered], collapse = ", ")))
})

test_that("every scenario has all five goldens", {
  skip_if_not_installed("tidyxl")
  sc <- golden_scenarios()
  sc <- sc[sc$id %in% golden_ids_in_build(sc$id), , drop = FALSE]
  for (ext in c(".csv", ".md", "_stored.csv", "_console.txt")) {
    f <- golden_path(paste0(sc$id, ext))
    expect_true(all(file.exists(f)), label = paste("missing", ext, paste(basename(f[!file.exists(f)]), collapse = " ")))
  }
  own <- vapply(sc$id, function(id) golden_book_name(id) == paste0(id, ".xlsx"), TRUE)
  for (cmd in setdiff(unique(sc$command), "stacktab")) {
    sheets <- tidyxl::xlsx_sheet_names(golden_path(paste0(cmd, ".xlsx")))
    # comptab's demo scenario also keeps the regtab source sheets its setup
    # writes (golden_comptab_source_sheets, helper-golden-comptab.R).
    ids <- sc$id[sc$command == cmd & !own]
    expect_setequal(sheets, c(ids, unlist(golden_comptab_source_sheets[intersect(ids, names(golden_comptab_source_sheets))])))
  }
  # stacktab reads its blocks from its own workbook: one per scenario
  # (<id>.xlsx), holding the source blocks and the composite sheet <id>;
  # P02 and P09 have their own workbook too (golden_book()).
  for (id in sc$id[own]) {
    expect_true(id %in% tidyxl::xlsx_sheet_names(golden_book(id)), label = id)
  }
})

test_that("goldens record the Stata package versions they came from", {
  v <- utils::read.csv(golden_path("VERSIONS.csv"), colClasses = "character")
  ver <- setNames(v$version, v$component)
  comps <- c("desctab", "regtab", "table1_tc", "_tabtools_common", "puttab", "stacktab", "_tabtools_markdown_write",
             "stratetab", "effecttab", "comptab", "hrcomptab")
  expect_identical(unname(ver[comps]), rep("2.1.14", length(comps)))
  expect_identical(unname(ver["fvgen"]), "1.2.5")
  expect_identical(unname(ver["rng"]), "mt64")
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    st <- golden_read_stored(id)
    meta <- setNames(st$value[st$kind == "meta"], st$name[st$kind == "meta"])
    expect_identical(unname(meta[c("_golden_id", "_golden_tabtools_version", "_golden_fvgen_version")]),
                     c(id, "2.1.14", "1.2.5"))
  }
})

# ---------------------------------------------------------------------------
# Cells

test_that("cell comparator accepts every golden against itself", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    w <- golden_read_cells(id)
    expect_identical(nrow(golden_compare_cells(w, w, golden_scenario(id)$mask)), 0L, label = id)
  }
})

test_that("cell comparator rejects broken cells", {
  w <- golden_read_cells("T01")
  row_primary <- which(w[, 1] == "   Primary")
  expect_length(row_primary, 1L)

  # A lost indent is a label mismatch, whatever the mode.
  b <- w
  b[row_primary, 1] <- "Primary"
  for (mode in c("exact", "tolerance", "structure")) {
    mm <- golden_compare_cells(b, w, "p,test,statistic", mode)
    expect_identical(mm$why, "text")
  }

  # One digit off in a value cell.
  b <- w
  age <- which(w[, 1] == "Age at cohort entry (years)")
  b[age, 2] <- "58.4±13.4"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "exact")), 1L)
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 0L)
  b[age, 2] <- "58.5±13.4"
  expect_identical(golden_compare_cells(b, w, "p", "tolerance")$why, "tolerance")
  expect_identical(nrow(golden_compare_cells(b, w, "p", "structure")), 0L)
  b[age, 2] <- "58.3 (13.4)"
  expect_identical(golden_compare_cells(b, w, "p", "structure")$why, "skeleton")

  # Thousands separators are part of the number, not the skeleton.
  fem <- which(w[, 1] == "Female sex")
  b <- w
  b[fem, 2] <- "5,352 (60)"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 0L)
  b[fem, 2] <- "5,353 (60)"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 1L)

  # Masked p-values: any text passes, but blank vs non-blank does not, and
  # the header text stays exact.
  pcol <- which(w[2, ] == "p-value")
  b <- w
  b[age, pcol] <- "0.031"
  expect_identical(nrow(golden_compare_cells(b, w, "p,test,statistic")), 0L)
  expect_identical(nrow(golden_compare_cells(b, w, character())), 1L)
  b[age, pcol] <- ""
  expect_identical(golden_compare_cells(b, w, "p")$why, "masked blank pattern")
  b <- w
  b[2, pcol] <- "P"
  expect_identical(nrow(golden_compare_cells(b, w, "p")), 1L)

  # Shape.
  expect_identical(golden_compare_cells(w[-1, ], w)$why, "dimensions")
})

test_that("cell numbers carry the unit of their last displayed digit", {
  n <- golden_cell_numbers("1,234 (5.67) 1.0e+08 -0.125")
  expect_equal(n$value, c(1234, 5.67, 1e8, -0.125))
  expect_equal(n$unit, c(1, 0.01, 1e7, 0.001))
})

test_that("tt_table objects are flattened to the CSV grid", {
  tt <- structure(list(
    title = "A title",
    header = list(list(text = c(" ", "G1", "p-value")), list(text = c("Desc", "N=5", ""))),
    body = data.frame(a = c("Var", "   lvl"), b = c("1 (20)", "2"), c = c("0.5", "")),
    footnote = "Note."
  ), class = "tt_table")
  m <- golden_as_cells(tt)
  expect_identical(dim(m), c(6L, 3L))
  expect_identical(m[1, ], c("A title", "", ""))
  expect_identical(m[5, 1], "   lvl")
  expect_identical(m[6, ], c("Note.", "", ""))
})

# ---------------------------------------------------------------------------
# Sinks

copy_golden <- function(id, ext) {
  f <- tempfile(fileext = paste0(".", ext))
  file.copy(golden_path(paste0(id, ".", ext)), f)
  f
}

edit_file <- function(path, from, to) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit <- grep(from, x, fixed = TRUE)
  stopifnot(length(hit) >= 1L)
  x[hit[1]] <- sub(from, to, x[hit[1]], fixed = TRUE)
  write_lf(x, path)
  path
}

# LF line endings on every platform, like the goldens.
write_lf <- function(x, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(x, con, useBytes = TRUE)
}

test_that("sink comparator accepts every golden against itself", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    for (ext in c("csv", "md")) {
      f <- golden_path(paste0(id, ".", ext))
      expect_length(golden_compare_sink(f, f, golden_scenario(id)$mask), 0L)
    }
  }
})

test_that("CSV sink comparator rejects broken bytes and ignores masked p-values", {
  g <- golden_path("T01.csv")
  expect_length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ",0.031"), g, "p"), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ",0.031"), g, "")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), "   Primary", "  Primary"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ","), g, "p")), 0L)
  # Quoting a field that needs no quotes changes the bytes.
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), "58.3±13.4", "\"58.3±13.4\""), g, "p")), 0L)
  # CRLF line endings.
  f <- copy_golden("T01", "csv")
  x <- readLines(f, encoding = "UTF-8")
  con <- file(f, "wb")
  writeLines(x, con, sep = "\r\n", useBytes = TRUE)
  close(con)
  expect_true("line endings differ" %in% golden_compare_sink(f, g, "p"))
  # Unmasked scenario: byte compare.
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T02", "csv"), "0.0", "0.1"), golden_path("T02.csv"), "")), 0L)
})

test_that("Markdown sink comparator rejects broken bytes and ignores masked p-values", {
  g <- golden_path("T01.md")
  expect_length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "| 0.24 |", "| 0.031 |"), g, "p"), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "| 0.24 |", "|  |"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "&nbsp;&nbsp;&nbsp;Primary", "Primary"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "### Table 1", "## Table 1"), g, "p")), 0L)
  # mdappend output holds two tables; both are masked.
  g34 <- golden_path("T34.md")
  f <- copy_golden("T34", "md")
  x <- readLines(f, encoding = "UTF-8")
  x <- sub("| 0.24 |", "| 0.5 |", x, fixed = TRUE)
  write_lf(x, f)
  expect_length(golden_compare_sink(f, g34, "p"), 0L)
  expect_gt(length(golden_compare_sink(f, g34, "")), 0L)
})

# ---------------------------------------------------------------------------
# Console

test_that("console comparator accepts goldens and rejects broken listings", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    x <- golden_read_lines(golden_path(paste0(id, "_console.txt")))
    expect_length(golden_compare_console(x, x, golden_scenario(id)$mask), 0L)
  }
  w <- golden_read_lines(golden_path("T01_console.txt"))
  # Sink messages are not part of the listing.
  expect_length(golden_compare_console(golden_console_box(w), w, "p"), 0L)
  # Masked p-value text passes; a changed label or value does not.
  b <- sub("0.24    |", "0.031   |", w, fixed = TRUE)
  expect_false(identical(b, w))
  expect_length(golden_compare_console(b, w, "p,test,statistic"), 0L)
  expect_gt(length(golden_compare_console(b, w, "")), 0L)
  expect_gt(length(golden_compare_console(sub("58.3", "58.4", w, fixed = TRUE), w, "p")), 0L)
  expect_gt(length(golden_compare_console(sub("|    Primary", "|   Primary ", w, fixed = TRUE), w, "p")), 0L)
  expect_gt(length(golden_compare_console(w[-5], w, "p")), 0L)
  # A value on a level row (p blank) is not masked.
  expect_gt(length(golden_compare_console(sub("1,527 (25)", "1,528 (25)", w, fixed = TRUE), w, "p")), 0L)
  # Masked text may change width: widen the p column by 4 on every box line
  # (as a longer p-value would), then put a longer value in it.
  sp <- golden_console_span(w, "p-value")
  box <- golden_is_box_line(w)
  wide <- w
  wide[box] <- paste0(substr(w[box], 1L, sp[2]),
                      ifelse(golden_is_rule_line(w[box]), "----", "    "),
                      substring(w[box], sp[2] + 1L))
  wide <- sub("0.80        |", "<0.0001     |", wide, fixed = TRUE)
  expect_true(any(grepl("<0.0001", wide, fixed = TRUE)))
  expect_length(golden_compare_console(wide, w, "p"), 0L)
  # Test/Statistic/p-value masked together; SMD after them stays exact.
  s <- golden_read_lines(golden_path("T30e_console.txt"))
  expect_length(golden_compare_console(sub("Ind. t test", "Welch-t-tst", s, fixed = TRUE), s, "p,test,statistic"), 0L)
  smd <- s[grepl("^\\s*\\| Age at cohort entry", s)]
  smd_val <- sub("^.*\\s(\\S+)\\s+\\|\\s*$", "\\1", smd)
  expect_gt(length(golden_compare_console(sub(paste0(smd_val, " "), "9.999 ", s, fixed = TRUE), s, "p,test,statistic")), 0L)
  # Chatter printed before the box by commands inside the engine is dropped.
  m <- golden_read_lines(golden_path("T13_console.txt"))
  expect_true(grepl("^\\(", m[1]))
  expect_length(golden_compare_console(golden_console_box(m), m, "p"), 0L)
  expect_true(grepl("^\\s*\\+-", golden_console_box(m)[1]))
  # Wrapped log lines ("> " continuations) are rejoined.
  long <- w[1]
  wrapped <- c(substr(long, 1, 20), paste0("> ", substring(long, 21)), w[-1])
  expect_length(golden_compare_console(wrapped, w, "p"), 0L)
  # Unmasked (regtab): exact characters, including alignment.
  r <- golden_read_lines(golden_path("R01_console.txt"))
  expect_gt(length(golden_compare_console(sub("  1.00", " 1.00 ", r, fixed = TRUE), r, "")), 0L)
})

# ---------------------------------------------------------------------------
# Stored results

test_that("stored-results comparator checks names, values, and tolerance", {
  g <- golden_read_stored("R01")
  got <- g[g$kind != "meta", ]
  # R's methods text has no Stata software sentence; the golden's is dropped.
  expect_gt(length(golden_compare_stored(got, g)), 0L)
  meth <- got$name == "methods"
  got$value[meth] <- sub(" Analysis performed in Stata.*$", "", got$value[meth])
  expect_length(golden_compare_stored(got, g), 0L)
  b <- got
  b$value[meth] <- sub("Odds ratios", "Hazard ratios", b$value[meth])
  expect_gt(length(golden_compare_stored(b, g)), 0L)

  b <- got
  b$value[b$name == "N_rows"] <- "14"
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  b <- got
  b$value[b$name == "coef_label"] <- "HR"
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  b <- got[got$name != "methods", ]
  expect_true(any(grepl("missing macro methods", golden_compare_stored(b, g))))

  k <- which(got$name == "table")[1]
  b <- got
  b$value[k] <- sprintf("%.17g", as.numeric(got$value[k]) * (1 + 1e-12))
  expect_length(golden_compare_stored(b, g), 0L)
  b$value[k] <- sprintf("%.17g", as.numeric(got$value[k]) * (1 + 1e-6))
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  expect_length(golden_compare_stored(b, g, tolerance = 1e-3), 0L)
  expect_length(golden_compare_stored(b, g, fields = c("N_rows", "coef_label")), 0L)
})

test_that("stored-results comparator is two-way: extra R results fail (Phase 4 review P1-4)", {
  g <- golden_read_stored("R01")
  got <- g[g$kind != "meta", ]
  meth <- got$name == "methods"
  got$value[meth] <- sub(" Analysis performed in Stata.*$", "", got$value[meth])
  expect_length(golden_compare_stored(got, g), 0L)
  # An extra r(table) row (mutation M26: Reference rows in r(table)).
  tab <- got[got$name == "table", ]
  extra <- tab[1, ]
  extra$row <- "Reference_row"
  b <- rbind(got, extra)
  why <- golden_compare_stored(b, g)
  expect_true(any(grepl("^extra matrix table\\[Reference_row", why)))
  expect_true(any(grepl("^extra", golden_compare_stored(b, g, fields = "table"))))
  # A repeated key is an extra occurrence.
  b <- rbind(got, got[got$name == "coef_label", ])
  expect_true(any(grepl("^extra macro coef_label", golden_compare_stored(b, g))))
  # Names the golden lacks entirely (R-only results) are not compared, nor
  # are names outside `fields`.
  b <- rbind(got, data.frame(name = "r_only_result", kind = "scalar", row = "", col = "", value = "1"))
  expect_length(golden_compare_stored(b, g), 0L)
  b <- rbind(got, extra)
  expect_length(golden_compare_stored(b, g, fields = c("N_rows", "coef_label")), 0L)
  # End to end: a tt_table whose r(table) has one row too many fails.
  tt <- regtab(lm(price ~ mpg + mpg_dup + foreign * rep78,
                  data = golden_fixture("auto", factors = c("foreign", "rep78"))),
               refcat = "Ref.", omitlabel = "Dropped", emptylabel = "No obs",
               interactions = "native", models = "Constrained")
  expect_length(golden_compare_stored(tt$stored, golden_read_stored("R12"), fields = "table", tolerance = 1e-9), 0L)
  tt$stored$table <- rbind(tt$stored$table, Domestic = NA_real_)
  expect_true(any(grepl("^extra matrix table\\[Domestic", golden_compare_stored(tt$stored, golden_read_stored("R12"),
                                                                                fields = "table"))))
})

test_that("masked p_value rows are not counted as extra", {
  g <- golden_read_stored("T01")
  tab <- g[g$name == "table", ]
  m <- matrix(golden_stored_num(tab$value), ncol = 1, dimnames = list(tab$row, "p_value"))
  m[1, 1] <- 0.5
  expect_true(any(grepl("table", golden_compare_stored(list(table = m), g, fields = "table"))))
  expect_length(golden_compare_stored(list(table = m), g, fields = "table", mask = "p"), 0L)
})

test_that("stored lists flatten to the golden shape; mask p drops test p-values", {
  g <- golden_read_stored("T01")
  tab <- g[g$name == "table", ]
  m <- matrix(golden_stored_num(tab$value), ncol = 1, dimnames = list(tab$row, "p_value"))
  stored <- list(
    Dapa = g$value[g$name == "Dapa"],
    varlist = g$value[g$name == "varlist"],
    table = m
  )
  expect_length(golden_compare_stored(stored, g, fields = c("Dapa", "varlist", "table")), 0L)
  m2 <- m
  m2[1, 1] <- 0.5
  stored$table <- m2
  expect_gt(length(golden_compare_stored(stored, g, fields = "table")), 0L)
  expect_length(golden_compare_stored(stored, g, fields = "table", mask = "p"), 0L)
  # methods is compared under mask p (after the R test-name swap).
  expect_true(any(grepl("missing macro methods", golden_compare_stored(stored, g, fields = "methods", mask = "p"))))
  # Stata missing (".", ".d") equals NA.
  expect_identical(golden_stored_num(c(".", ".d", "0.5")), c(NA, NA, 0.5))
})

# ---------------------------------------------------------------------------
# Excel styling

test_that("style comparator accepts golden sheets against themselves", {
  skip_if_not_installed("tidyxl")
  for (id in c("T01", "T19", "T20c", "T29", "R01", "R12", "R15", "R29")) {
    wb <- golden_path(paste0(golden_scenario(id)$command, ".xlsx"))
    expect_length(golden_compare_styles(wb, id, wb, id, golden_scenario(id)$mask,
                                        got_width_offset = golden_width_offset), 0L)
  }
})

test_that("style comparator detects border, font, fill, and merge differences", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  cmp <- function(a, b, mask = "p,test,statistic") {
    golden_compare_styles(wb, a, wb, b, mask, got_width_offset = golden_width_offset)
  }
  # default (thin) vs medium borders, Calibri 11 vs Times New Roman 12
  d <- cmp("T20a", "T20b")
  expect_true(any(grepl("border_top: 'thin' vs 'medium'", d, fixed = TRUE)))
  expect_true(any(grepl("font: 'Calibri' vs 'Times New Roman'", d, fixed = TRUE)))
  # medium vs academic: academic drops the verticals
  d <- cmp("T20b", "T20c")
  expect_true(any(grepl("border_left: 'medium' vs 'NA'", d, fixed = TRUE)))
  # zebra/headershade fills and the SMD column's extra merge
  d <- cmp("T01", "T19")
  expect_true(any(grepl("fill", d, fixed = TRUE)))
  expect_true(any(grepl("^merges", d)))
  # regtab: reference-row merges differ between R01 and R02 (compact)
  rb <- golden_path("regtab.xlsx")
  d <- golden_compare_styles(rb, "R01", rb, "R02", got_width_offset = golden_width_offset)
  expect_true(any(grepl("^merges", d)))
})

# Rewrite one sheet's XML inside a copy of a golden workbook.
edit_sheet_xml <- function(xlsx, sheet, from, to) {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  tmp <- tempfile("xl")
  dir.create(tmp)
  utils::unzip(xlsx, exdir = tmp)
  wbxml <- paste(readLines(file.path(tmp, "xl/workbook.xml"), warn = FALSE), collapse = "")
  rels <- paste(readLines(file.path(tmp, "xl/_rels/workbook.xml.rels"), warn = FALSE), collapse = "")
  tag <- regmatches(wbxml, regexpr(sprintf('<sheet [^>]*name="%s"[^>]*>', sheet), wbxml))
  rid <- sub('^.*r:id="([^"]*)".*$', "\\1", tag)
  rel <- regmatches(rels, regexpr(sprintf('<Relationship [^>]*Id="%s"[^>]*>', rid), rels))
  target <- sub("^/?(xl/)?", "xl/", sub('^.*Target="([^"]*)".*$', "\\1", rel))
  f <- file.path(tmp, target)
  x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  stopifnot(grepl(from, x, perl = TRUE))
  write_lf(sub(from, to, x, perl = TRUE), f)
  out <- tempfile(fileext = ".xlsx")
  old <- setwd(tmp)
  on.exit(setwd(old))
  utils::zip(out, files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-q -X")
  out
}

test_that("style comparator detects edited merges, widths, heights, and cell styles", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  cmp <- function(f, mask = "p,test,statistic") {
    golden_compare_styles(f, "T01", wb, "T01", mask, got_width_offset = golden_width_offset)
  }
  expect_length(cmp(edit_sheet_xml(wb, "T01", "(?!)|$", "")), 0L)
  expect_true(any(grepl("^merges", cmp(edit_sheet_xml(wb, "T01", '<mergeCell ref="B2:B3"/>', "")))))
  expect_true(any(grepl("width", cmp(edit_sheet_xml(wb, "T01", '(<col [^>]*min="3"[^>]*width=")[0-9.]+', "\\140")))))
  expect_true(any(grepl("height", cmp(edit_sheet_xml(wb, "T01", '(<row r="1"[^>]*ht=")[0-9.]+', "\\145")))))
  # Point cell C5 at A1's style (bold, left-aligned title style).
  a1 <- regmatches(golden_sheet_xml(wb, "T01"), regexpr('<c r="A1"[^>]* s="[0-9]+"', golden_sheet_xml(wb, "T01")))
  s_a1 <- sub('^.* s="([0-9]+)"$', "\\1", a1)
  d <- cmp(edit_sheet_xml(wb, "T01", '(<c r="C5"[^>]* s=")[0-9]+', paste0("\\1", s_a1)))
  expect_true(any(grepl("^C5 ", d)))
})

test_that("pstyle mask ignores p-dependent bold and highlight fill only", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  x <- golden_cell_styles(wb, "T19")
  hl <- x[x$fill == "FFFFFFCC", ]
  skip_if(!nrow(hl), "T19 has no highlighted rows")
  # Give a highlighted cell the style of an unhighlighted body cell in the same column.
  cell <- hl$address[hl$col == 3L][1]
  plain <- x$address[x$col == 3L & x$row > 4L & x$fill != "FFFFFFCC" & x$row %% 2L == hl$row[hl$address == cell] %% 2L][1]
  xml <- golden_sheet_xml(wb, "T19")
  s_plain <- sub('^.* s="([0-9]+)"$', "\\1", regmatches(xml, regexpr(sprintf('<c r="%s"[^>]* s="[0-9]+"', plain), xml)))
  f <- edit_sheet_xml(wb, "T19", sprintf('(<c r="%s"[^>]* s=")[0-9]+', cell), paste0("\\1", s_plain))
  expect_length(golden_compare_styles(f, "T19", wb, "T19", "p,test,statistic,pstyle",
                                      got_width_offset = golden_width_offset), 0L)
  expect_gt(length(golden_compare_styles(f, "T19", wb, "T19", "p,test,statistic",
                                         got_width_offset = golden_width_offset)), 0L)
})

# ---------------------------------------------------------------------------
# Row-wise p mask (Phase 2 review P1-1/P1-2/P1-3): p cells, r(table)
# p_value, and boldp/highlight are compared exactly on rows whose R test is
# Stata's test (cat, bin, cate, bine, conts with > 2 groups). Each check
# shows the mutation is caught with the row-wise mask and was invisible to
# the old whole-column mask (p_rows = all TRUE / p_table = NULL).

golden_eval_edited <- function(id, edit, dir) {
  sc <- golden_scenario(id)
  code <- edit(gsub("@ID@", golden_code_path(dir, id), sc$r_call, fixed = TRUE))
  eval(parse(text = code), envir = new.env(parent = environment(golden_fixture)))
}

test_that("row-wise p mask covers exactly the rows whose R test differs from Stata's", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T30e", identity, withr::local_tempdir())
  pr <- golden_p_masked_rows(tt)
  vt <- tt$rows$vtype
  expect_true(all(pr[vt %in% c("contn", "contln", "conts")]))
  expect_false(any(pr[vt %in% c("cat", "bin")]))
  # Two groups: conts is masked; with more groups (Kruskal-Wallis) it is not.
  tt3 <- golden_eval_edited("T18", function(x) sub(")$", ", vars = c(index_age = \"conts\", female = \"bin\"))",
                                                        sub("vars = c\\(.*\\), smd", "smd", x)),
                            withr::local_tempdir())
  expect_false(any(golden_p_masked_rows(tt3)))
})

test_that("row-wise p mask: a Yates-corrected chi-squared p fails", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T01", function(x) sub(")$", ", test_args = list(chisq.test = list(correct = TRUE)))", x),
                           withr::local_tempdir())
  g <- golden_read_stored("T01")
  why <- golden_compare_stored(tt$stored, g, fields = "table", mask = "p,test,statistic",
                               p_table = golden_table_p_rows(tt))
  expect_gt(length(why), 0L)
  expect_true(all(grepl("p_value", why)))
  expect_length(golden_compare_stored(tt$stored, g, fields = "table", mask = "p,test,statistic"), 0L)
  # The label and the methods paragraph name the correction (P3-8).
  expect_match(tt$stored$methods, "with Yates' continuity correction", fixed = TRUE)
  expect_gt(length(golden_compare_stored(tt$stored, g, fields = "methods", mask = "p")), 0L)
})

test_that("row-wise p mask: a wrong pdp/highpdp fails", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T17", function(x) sub("pdp = 4, highpdp = 3", "pdp = 3, highpdp = 2", x, fixed = TRUE),
                           withr::local_tempdir())
  want <- golden_read_cells("T17")
  mm <- golden_compare_cells(tt, want, "p,test,statistic")
  expect_gt(nrow(mm), 0L)
  expect_true(all(want[2, mm$col] == "" & want[1, mm$col] == "p-value"))
  expect_identical(nrow(golden_compare_cells(tt, want, "p,test,statistic",
                                             p_rows = rep(TRUE, nrow(want)))), 0L)
})

test_that("row-wise pstyle mask: dropping boldp fails", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  dir <- withr::local_tempdir()
  tt <- golden_eval_edited("T19", function(x) sub("boldp = 0.05, ", "", x, fixed = TRUE), dir)
  xlsx <- file.path(dir, "t19.xlsx")
  tt_write_xlsx(tt, xlsx, sheet = "T19")
  wb <- golden_path("table1_tc.xlsx")
  mask <- "p,test,statistic,pstyle"
  why <- golden_compare_styles(xlsx, "T19", wb, "T19", mask, got_width_offset = golden_r_width_offset,
                               p_rows = golden_p_masked_rows(tt))
  expect_gt(length(why), 0L)
  expect_true(all(grepl(" bold: ", why, fixed = TRUE)))
  expect_length(golden_compare_styles(xlsx, "T19", wb, "T19", mask, got_width_offset = golden_r_width_offset), 0L)
})

# ---------------------------------------------------------------------------
# Derived small-cell suppression (Phase 3 review P1-1): "Suppressed" in a
# masked Test/Statistic/p-value cell is compared exactly on every path, so a
# leaked test name, statistic, or p-value of a protected variable fails
# (mutants M16 and M47 of the review were silent before).

p1_leak <- function(tt, role, value) {
  j <- which(tt$cols$role == role)
  i <- which(golden_suppressed(tt$body[[j]]))[1]
  tt$body[[j]][i] <- value
  tt
}

test_that("a leaked test name, statistic, or p-value fails the cell and console comparators", {
  skip_if_not_installed("haven")
  tt <- table1_tc(sc_pipeline_data("sccont"), by = "group", vars = "value contn \\ category cat",
                  missingsummary = TRUE, test = TRUE, statistic = TRUE, smd = TRUE, smallcells = 5)
  dir <- test_path("fixtures", "table1_phase3")
  want <- golden_read_cells_file(file.path(dir, "S3.csv"))
  cons <- golden_read_lines(file.path(dir, "S3_console.txt"))
  mask <- "p,test,statistic"
  expect_identical(nrow(golden_compare_cells(tt, want, mask)), 0L)
  expect_length(golden_compare_console(utils::capture.output(print(tt)), cons, mask), 0L)
  expect_length(golden_check_derived(tt), 0L)
  # The first Suppressed cell of each column is the contn row, whose p-value
  # is row-masked: exactly what M16 (Test/Statistic) and M47 (p) leaked.
  for (leak in list(c("test", "Welch t test"), c("statistic", "t(2.3)= -9.14"), c("p", "0.007"))) {
    bad <- p1_leak(tt, leak[1], leak[2])
    mm <- golden_compare_cells(bad, want, mask)
    expect_identical(nrow(mm), 1L, label = paste(leak[1], "cells"))
    expect_identical(mm$want, "Suppressed")
    expect_gt(length(golden_compare_console(utils::capture.output(print(bad)), cons, mask)), 0L,
              label = paste(leak[1], "console"))
    expect_gt(length(golden_check_derived(bad)), 0L, label = paste(leak[1], "suppression matrix"))
  }
  # A masked cell that is not Suppressed on either side stays masked.
  w2 <- want
  tc <- which(want[1, ] == "Test")
  w2[which(want[, tc] == "Suppressed")[1], tc] <- "Ind. t test"
  expect_identical(nrow(golden_compare_cells(p1_leak(tt, "test", "Welch t test"), w2, mask)), 0L)
})

test_that("a leaked p-value fails the CSV, Markdown, and xlsx comparators", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  dir <- withr::local_tempdir()
  tt <- golden_eval_edited("T29", identity, dir)
  mask <- golden_scenario("T29")$mask
  # T29's price row (contn: p row-masked) is Suppressed.
  i <- which(tt$rows$vtype == "contn")[1]
  expect_true(golden_p_masked_rows(tt)[i])
  expect_identical(tt$body[[which(tt$cols$role == "p")]][i], "Suppressed")
  bad <- p1_leak(tt, "p", "0.007")
  for (x in list(list(tt = tt, ok = TRUE), list(tt = bad, ok = FALSE))) {
    csv <- tempfile(tmpdir = dir, fileext = ".csv")
    md <- tempfile(tmpdir = dir, fileext = ".md")
    xlsx <- tempfile(tmpdir = dir, fileext = ".xlsx")
    tt_write_csv(x$tt, csv)
    tt_write_markdown(x$tt, md)
    tt_write_xlsx(x$tt, xlsx, sheet = "T29")
    why_csv <- golden_compare_sink(csv, golden_path("T29.csv"), mask, p_rows = golden_grid_p_rows(x$tt))
    why_md <- golden_compare_sink(md, golden_path("T29.md"), mask, p_body = golden_p_masked_rows(x$tt))
    sty <- golden_compare_styles(xlsx, "T29", golden_path("table1_tc.xlsx"), "T29", mask,
                                 got_width_offset = golden_r_width_offset, p_rows = golden_p_masked_rows(x$tt))
    if (x$ok) {
      expect_length(c(why_csv, why_md, sty), 0L)
    } else {
      expect_true(any(grepl("'0.007' vs 'Suppressed'", why_csv, fixed = TRUE)), label = "CSV sink")
      # The leaked cell is masked to "#" on the R side; Stata's stays Suppressed.
      expect_true(any(grepl("| # |", why_md, fixed = TRUE) & grepl("| Suppressed |", why_md, fixed = TRUE)),
                  label = "Markdown sink")
      expect_true(any(grepl(" value: '0.007' vs 'Suppressed'", sty, fixed = TRUE)), label = "xlsx")
    }
  }
})

test_that("methods comparator swaps the R test names and rejects other edits", {
  g <- golden_read_stored("T30e")
  w <- g$value[g$name == "methods"]
  r <- golden_methods_swap(w)
  expect_false(grepl("Stata", r, fixed = TRUE))
  expect_match(r, "Welch's t-test, Wilcoxon rank-sum test, Pearson's chi-squared test.", fixed = TRUE)
  expect_length(golden_compare_stored(list(methods = r), g, fields = "methods", mask = "p"), 0L)
  expect_gt(length(golden_compare_stored(list(methods = w), g, fields = "methods", mask = "p")), 0L)
  bad <- sub("Welch's t-test", "Student's t-test", r, fixed = TRUE)
  expect_gt(length(golden_compare_stored(list(methods = bad), g, fields = "methods", mask = "p")), 0L)
})

test_that("stored comparator matches repeated row names by occurrence", {
  g <- data.frame(name = "table", kind = "matrix", row = c("a", "___Missing", "b", "___Missing"),
                  col = "p_value", value = c("0.5", ".", "0.25", "0.75"), stringsAsFactors = FALSE)
  m <- matrix(c(0.5, NA, 0.25, 0.75), ncol = 1, dimnames = list(g$row, "p_value"))
  expect_length(golden_compare_stored(list(table = m), g), 0L)
  m[4, 1] <- 0.7
  expect_gt(length(golden_compare_stored(list(table = m), g)), 0L)
  # Row-wise p mask by r(table) row.
  expect_length(golden_compare_stored(list(table = m), g, mask = "p", p_table = c(FALSE, FALSE, FALSE, TRUE)), 0L)
  expect_gt(length(golden_compare_stored(list(table = m), g, mask = "p", p_table = c(TRUE, TRUE, TRUE, FALSE))), 0L)
})

# ---------------------------------------------------------------------------
# Fixtures and classifier/formatting goldens

test_that("fixtures carry Stata labels", {
  d <- golden_fixture("cohort", factors = "education")
  expect_identical(nrow(d), 15000L)
  expect_identical(attr(d$index_age, "label"), "Age at cohort entry (years)")
  expect_identical(levels(d$education), c("Primary", "Secondary", "Tertiary"))
  expect_identical(attr(d$education, "label"), "Education level")
  a <- golden_fixture("auto", factors = "rep78")
  expect_identical(levels(a$rep78), as.character(1:5))
})

test_that("classifier goldens are complete", {
  rng <- utils::read.csv(golden_path("rng_mt64.csv"))
  expect_identical(as.integer(table(rng$seed)[c("12345", "1", "2147483647", "20260324")]),
                   c(10000L, 1000L, 1000L, 1000L))
  expect_true(all(rng$u > 0 & rng$u < 1))

  sw <- utils::read.csv(golden_path("swilk.csv"), colClasses = c(case = "character"))
  hr <- sw[sw$case == "full" & sw$variable == "headroom", ]
  # D6: Stata's tie-averaged swilk vs R's shapiro.test on auto headroom.
  expect_equal(as.numeric(hr$p), 0.3314, tolerance = 1e-3)
  expect_lt(stats::shapiro.test(golden_fixture("auto")$headroom)$p.value, 0.01)

  ss <- utils::read.csv(golden_path("subsample_ids.csv"))
  d <- golden_fixture("cohort3500")
  for (v in unique(ss$variable)) {
    ids <- ss$id[ss$variable == v]
    expect_identical(length(unique(ids)), 2000L)
    expect_true(all(ids %in% d$id[!is.na(d[[v]])]))
  }

  at <- utils::read.csv(golden_path("autotype.csv"))
  expect_true(all(at$type %in% c("contn", "conts", "cat", "bin")))
  dv <- utils::read.csv(golden_path("detect_vartype.csv"))
  expect_true(all(file.exists(golden_path("fixtures", "vartype_branches.dta"))))
  expect_setequal(unique(dv$type), c("cat", "contn", "bin", "conts"))

  fm <- utils::read.csv(golden_path("stata_fmt.csv"), colClasses = "character")
  expect_setequal(unique(fm$kind), c("string", "round", "headerperc"))
  expect_identical(unique(fm$result[fm$kind == "string" & fm$x == "100000000" & fm$fmt == "%2.0f"]), "1.0e+08")
})

test_that("every non-exact scenario has a per-scenario stored tolerance, tighter than the old 1e-3", {
  sc <- golden_scenarios()
  # effecttab scenarios have their own runner and r(table) tolerances
  # (golden_effect_table_tol, helper-golden-effecttab.R), checked below.
  tol_ids <- sc$id[sc$compare != "exact" & sc$command != "effecttab"]
  expect_setequal(names(golden_stored_tol_tolerance), tol_ids)
  for (id in tol_ids) {
    t <- golden_stored_tol_tolerance[[id]]
    expect_named(t, c("table", "other"))
    expect_true(all(t >= 1e-9 & t <= 1e-4), label = id)
  }
})

test_that("effecttab r(table) tolerances: listed scenarios exist, none looser than 1e-3", {
  sc <- golden_scenarios()
  expect_true(all(names(golden_effect_table_tol) %in% sc$id[sc$command == "effecttab"]))
  expect_true(all(golden_effect_table_tol >= 1e-9 & golden_effect_table_tol <= 1e-3))
  # Every collected E scenario's setup wrote Stata's r(table) rows (the
  # from() matrix scenarios have no collection).
  ids <- sc$id[sc$command == "effecttab"]
  expect_setequal(ids[!grepl("golden_effect_input", sc$stata_setup[sc$command == "effecttab"], fixed = TRUE)],
                  c("E18", "E24"))
  ids <- setdiff(ids, c("E18", "E24"))
  expect_true(all(file.exists(golden_path(paste0(ids, "_input.csv")))))
})

test_that("H11: the regtab methods transform replaces only the model phrase of listed goldens", {
  d <- golden_methods_divergences()
  # Since tabtools 2.1.12 builds r(methods) from the models (take_action
  # item 11, fixed) the list would hold only models Stata tabtools does
  # not classify; the last ones, clogit (R74-R76, task 5.15), are
  # classified since 2.1.14 (C8), so the list is empty.
  expect_identical(names(d), c("set", "id", "stata", "r"))
  expect_identical(nrow(d), 0L)
  expect_false(anyDuplicated(paste(d$set, d$id)) > 0)
  expect_true(all(d$set %in% c("golden", "regtab_5w_review", "regtab_phase5_review", "regtab_h_t2b")))
  sc <- golden_scenarios()
  expect_true(all(d$id[d$set == "golden"] %in% sc$id[sc$command == "regtab"]))
  read_set <- function(set, id) {
    switch(set, golden = golden_read_stored(id),
           utils::read.csv(test_path("fixtures", set, paste0(id, "_stored.csv")), colClasses = "character",
                           na.strings = character(), encoding = "UTF-8"))
  }
  parts <- function(x) {
    re <- "^(.*) with ([0-9.]+)% confidence intervals(?: from (.*?))?( across [0-9]+ models)?\\.( Statistical significance denoted as .*)?$"
    m <- regmatches(x, regexec(re, x, perl = TRUE))[[1]]
    if (!length(m)) return(NULL)
    list(estimates = m[2], level = m[3], model = m[4], across = m[5], stars = m[6])
  }
  for (i in seq_len(nrow(d))) {
    s <- parts(d$stata[i])
    r <- parts(d$r[i])
    expect_false(is.null(s), label = paste(d$id[i], "Stata sentence shape"))
    expect_false(is.null(r), label = paste(d$id[i], "R sentence shape"))
    # Level, model count and stars never change; the model phrase does.
    expect_identical(r$level, s$level, label = d$id[i])
    expect_identical(r$across, s$across, label = d$id[i])
    expect_identical(r$stars, s$stars, label = d$id[i])
    expect_false(identical(r$model, s$model) && identical(r$estimates, s$estimates), label = d$id[i])
    # The golden or fixture still holds the listed Stata sentence.
    g <- read_set(d$set[i], d$id[i])
    expect_identical(golden_methods_regtab(g, d$id[i], d$set[i])$value[g$name == "methods"], d$r[i])
  }
  # An unlisted golden passes through unchanged.
  un <- setdiff(sc$id[sc$command == "regtab"], d$id)[1]
  g <- golden_read_stored(un)
  expect_identical(golden_methods_regtab(g, un), g)
})
