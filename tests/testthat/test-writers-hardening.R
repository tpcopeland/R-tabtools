# Milestone H hardening of the file writers (IMPLEMENTATION_PLAN.md, tasks
# H8, H12, H20; findings F09, F14, F33 in POTENTIAL_ISSUES_2026-09-26.md).

h8_data <- function() {
  data.frame(g = rep(c("a", "b"), each = 10), x = c(1:10, 3:12), s = rep(c(0, 1), 10))
}
h8_fit <- function() stats::lm(mpg ~ wt, data = mtcars)

# H8 (F09): one preflight of every target before any write --------------------

test_that("H8: csv must be a .csv file, checked before anything is written", {
  dir <- withr::local_tempdir()
  xl <- file.path(dir, "a.xlsx")
  md <- file.path(dir, "a.md")
  # xlsx = csv: the workbook is not written and then overwritten by CSV text.
  expect_error(regtab(h8_fit(), xlsx = xl, csv = xl), "must specify a .csv file")
  expect_false(file.exists(xl))
  expect_error(table1_tc(h8_data(), vars = c(x = "contn"), by = "g", xlsx = xl, csv = xl), ".csv file")
  expect_false(file.exists(xl))
  # csv = markdown: neither is written.
  expect_error(table1_tc(h8_data(), vars = c(x = "contn"), by = "g", csv = md, markdown = md), ".csv file")
  expect_false(file.exists(md))
  # csv onto an existing workbook: refused, the workbook survives.
  tt_write_xlsx(table1_tc(h8_data(), vars = c(x = "contn"), by = "g"), xl)
  before <- unname(tools::md5sum(xl))
  expect_error(regtab(h8_fit(), csv = xl), ".csv file")
  expect_error(tt_write_csv(regtab(h8_fit()), xl), "`path` must specify a .csv file")
  expect_identical(unname(tools::md5sum(xl)), before)
})

test_that("H8: mdappend cannot append onto a CSV file", {
  dir <- withr::local_tempdir()
  cf <- file.path(dir, "t.csv")
  regtab(h8_fit(), csv = cf)
  before <- readLines(cf)
  expect_error(regtab(h8_fit(), markdown = cf, mdappend = TRUE), "must specify a .md")
  expect_error(table1_tc(h8_data(), vars = c(x = "contn"), markdown = cf, mdappend = TRUE), "must specify a .md")
  expect_identical(readLines(cf), before)
})

test_that("H8: an existing Markdown file is replaced without mdappend (Stata 2.1.12), appended to with it", {
  dir <- withr::local_tempdir()
  md <- file.path(dir, "t.md")
  writeLines("existing", md)
  xl <- file.path(dir, "t.xlsx")
  cf <- file.path(dir, "t.csv")
  expect_no_error(table1_tc(h8_data(), vars = c(x = "contn"), by = "g", xlsx = xl, csv = cf, markdown = md))
  expect_true(file.exists(xl) && file.exists(cf))
  expect_false("existing" %in% readLines(md))
  unlink(c(xl, cf))
  writeLines("existing", md)
  # With mdappend every target is written.
  expect_no_error(regtab(h8_fit(), xlsx = xl, csv = cf, markdown = md, mdappend = TRUE))
  expect_true(file.exists(xl) && file.exists(cf))
  expect_identical(readLines(md)[1:2], c("existing", ""))
})

test_that("H8: a missing directory or a directory target is refused before any write", {
  dir <- withr::local_tempdir()
  xl <- file.path(dir, "t.xlsx")
  expect_error(regtab(h8_fit(), xlsx = xl, csv = file.path(dir, "nope", "t.csv")), "does not exist")
  expect_false(file.exists(xl))
  sub <- file.path(dir, "d.csv")
  dir.create(sub)
  expect_error(table1_tc(h8_data(), vars = c(x = "contn"), xlsx = xl, csv = sub), "is a directory")
  expect_false(file.exists(xl))
})

test_that("H8: target keys resolve relative paths, aliases and (where the file system ignores it) case", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  key <- tabtools:::.tt_path_key
  expect_identical(key("./a.xlsx"), key(file.path(normalizePath(dir), "a.xlsx")))
  dir.create("sub")
  expect_identical(key("sub/../a.xlsx"), key("a.xlsx"))
  expect_identical(key("~/a.xlsx"), key(file.path(path.expand("~"), "a.xlsx")))
  withr::local_options(tabtools.case_insensitive_fs = TRUE)
  expect_identical(key("A.XLSX"), key("a.xlsx"))
  withr::local_options(tabtools.case_insensitive_fs = FALSE)
  expect_false(identical(key("A.XLSX"), key("a.xlsx")))
})

test_that("H8: two arguments naming one file are refused, with the alias and case rules", {
  # The public extension rules keep xlsx/csv/markdown apart, so the duplicate
  # check is exercised with the csv extension check switched off.
  testthat::local_mocked_bindings(.tt_check_csv_path = function(...) invisible(NULL))
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  pre <- tabtools:::.tt_preflight_targets
  expect_error(pre(xlsx = "a.xlsx", csv = "a.xlsx"), "name the same file")
  expect_error(pre(xlsx = "./a.xlsx", csv = file.path(normalizePath(dir), "a.xlsx")), "name the same file")
  expect_error(pre(csv = "t.md", markdown = "t.md"), "`csv` and `markdown`")
  withr::local_options(tabtools.case_insensitive_fs = TRUE)
  expect_error(pre(xlsx = "A.XLSX", csv = "a.xlsx"), "name the same file")
  withr::local_options(tabtools.case_insensitive_fs = FALSE)
  expect_no_error(pre(xlsx = "A.XLSX", csv = "a.xlsx"))
})

# H12 (F14): Markdown escaping is Stata's, pinned byte for byte ----------------

h12_table <- function(rows) {
  tt_table(data.frame(lab = vapply(rows, `[`, "", 1), val = vapply(rows, `[`, "", 2)),
           list(c("lab", "val")), command = "custom")
}

test_that("H12: Markdown cells are literal text with Stata 2.1.12's escapes (byte-exact vs Stata)", {
  rows <- list(c("<b>literal</b>", "&lt;5"), c("[a](b)", "*x*"), c("_x_", "p|q"), c("c:\\dir", "a\\b"),
               c("  indented <i>", " spaced "), c("tick", "`code`"), c("x", "y"))
  f <- withr::local_tempfile(fileext = ".md")
  tt_write_markdown(h12_table(rows), f)
  # Bytes of `puttab lab val, markdown()` on the same cells (Stata 17,
  # tabtools 2.1.12, 2026-09-26; md1.md of qa/stata/probe_h12_markdown.do).
  # 2.1.11 escaped only \ | * _ and stopped with r(132) at the backtick.
  stata <- paste0(
    "| lab | val |\n",
    "| --- | --- |\n",
    "| \\<b\\>literal\\</b\\> | \\&lt;5 |\n",
    "| \\[a\\](b) | \\*x\\* |\n",
    "| \\_x\\_ | p\\|q |\n",
    "| c:\\\\dir | a\\\\b |\n",
    "| &nbsp;&nbsp;indented \\<i\\> | spaced |\n",
    "| tick | \\`code\\` |\n",
    "| x | y |\n")
  expect_identical(readChar(f, file.size(f), useBytes = TRUE), stata)
})

test_that("H12: title and footnote are escaped too; the file is complete", {
  rows <- list(c("tick", "`code`"), c("pair", "``a`` and `b`"), c("x", "y"))
  tt <- h12_table(rows)
  tt$title <- "T_1 `t`"
  tt$footnote <- "Note *with* `tick` ~x~."
  f <- withr::local_tempfile(fileext = ".md")
  res <- tt_write_markdown(tt, f)
  expect_identical(readLines(f), c(
    "### T\\_1 \\`t\\`", "", "| lab | val |", "| --- | --- |", "| tick | \\`code\\` |",
    "| pair | \\`\\`a\\`\\` and \\`b\\` |", "| x | y |", "", "*Note \\*with\\* \\`tick\\` \\~x\\~.*"))
  expect_identical(attr(res, "n_rows"), 3L)
  # An existing file is replaced without mdappend (Stata 2.1.12), appended
  # to with it.
  tt_write_markdown(tt, f)
  expect_identical(length(readLines(f)), 9L)
  tt_write_markdown(tt, f, append = TRUE)
  expect_identical(length(readLines(f)), 19L)
})

test_that("H12: the escape set is exactly Stata 2.1.12's", {
  esc <- tabtools:::.md_escape
  expect_identical(esc(c("\\", "|", "*", "_", "a\r\nb", "a\rb", "a\nb", "  <&>[]()`#~  ")),
                   c("\\\\", "\\|", "\\*", "\\_", "a<br>b", "a<br>b", "a<br>b", "\\<\\&\\>\\[\\]()\\`#\\~"))
  # The backslash is escaped first, so an inserted one is never doubled.
  expect_identical(esc("\\<"), "\\\\\\<")
})

# H20 (F33): atomic persisted options; write errors name the sink --------------

h20_readonly_dir <- function(env = parent.frame()) {
  skip_on_os("windows")
  d <- withr::local_tempdir(.local_envir = env)
  Sys.chmod(d, "0500")
  withr::defer(Sys.chmod(d, "0700"), envir = env)
  if (file.access(d, 2L) == 0L) skip("running with write access everywhere (root?)")
  d
}

test_that("H20: an unwritable config directory leaves the session defaults unchanged", {
  ro <- h20_readonly_dir()
  withr::local_envvar(R_USER_CONFIG_DIR = ro)
  withr::local_options(tabtools.font = NULL, tabtools.digits = 4)
  expect_error(tabtools_options(font = "Calibri", persist = TRUE), "Could not save the tabtools defaults")
  expect_null(getOption("tabtools.font"))
  expect_identical(getOption("tabtools.digits"), 4)
  # Without persist the session changes as before.
  tabtools_options(font = "Calibri")
  expect_identical(getOption("tabtools.font"), "Calibri")
})

test_that("H20: persist writes the whole file and then the session", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  withr::local_options(tabtools.font = NULL, tabtools.digits = NULL, tabtools.fontsize = NULL)
  tabtools_options(digits = 3, persist = TRUE)
  tabtools_options(font = "Calibri", persist = TRUE)
  f <- tabtools:::.tt_defaults_file()
  d <- read.dcf(f)
  expect_identical(unname(d[1, c("digits", "font")]), c("3", "Calibri"))
  expect_identical(list.files(dirname(f)), "defaults.dcf")
  expect_identical(getOption("tabtools.font"), "Calibri")
})

test_that("H20: a saved defaults file that cannot be deleted gives a warning", {
  cfg <- withr::local_tempdir()
  withr::local_envvar(R_USER_CONFIG_DIR = cfg)
  withr::local_options(tabtools.digits = NULL)
  tabtools_options(digits = 3, persist = TRUE)
  f <- tabtools:::.tt_defaults_file()
  skip_on_os("windows")
  Sys.chmod(dirname(f), "0500")
  withr::defer(Sys.chmod(dirname(f), "0700"))
  if (file.access(dirname(f), 2L) == 0L) skip("running with write access everywhere (root?)")
  expect_warning(tabtools_options(clear = TRUE, persist = TRUE), "Could not delete")
  expect_null(getOption("tabtools.digits"))
  expect_true(file.exists(f))
})

test_that("H20: an unwritable target is named in the error", {
  ro <- h20_readonly_dir()
  tab <- table1_tc(h8_data(), vars = c(x = "contn"), by = "g")
  expect_error(tt_write_csv(tab, file.path(ro, "t.csv")), "Could not write the `path` target")
  expect_error(tt_write_markdown(tab, file.path(ro, "t.md")), "Could not write the `path` target")
  expect_error(tt_write_xlsx(tab, file.path(ro, "t.xlsx")), "Could not write the workbook")
  e <- tryCatch(tt_write_xlsx(tab, file.path(ro, "t.xlsx")), error = function(e) e)
  expect_match(conditionMessage(e), "t.xlsx", fixed = TRUE)
  # Through the commands, the preflight names the argument before any write.
  expect_error(table1_tc(h8_data(), vars = c(x = "contn"), csv = file.path(ro, "t.csv")),
               "`csv` target .* not writable")
  expect_error(regtab(h8_fit(), markdown = file.path(ro, "t.md")), "`markdown` target .* not writable")
  # An existing read-only file.
  w <- withr::local_tempdir()
  f <- file.path(w, "t.csv")
  writeLines("x", f)
  Sys.chmod(f, "0400")
  withr::defer(Sys.chmod(f, "0600"))
  expect_error(regtab(h8_fit(), csv = f), "exists and is not writable")
  expect_error(tt_write_csv(tab, f), "Could not write the `path` target")
})

# Independent review of group t1 (R5, R6, R9, R15) ------------------------------

test_that("review R6: a csv symlink onto the workbook target is refused, dangling or not", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  d <- data.frame(x = 1:4)
  table1_tc(d, vars = c(x = "contn"), xlsx = "real.xlsx")
  file.symlink(file.path(dir, "real.xlsx"), "link.csv")
  expect_error(table1_tc(d, vars = c(x = "contn"), xlsx = "real.xlsx", csv = "link.csv"), "same file")
  file.symlink(file.path(dir, "new.xlsx"), "dang.csv")
  expect_error(table1_tc(d, vars = c(x = "contn"), xlsx = "new.xlsx", csv = "dang.csv"), "same file")
  expect_false(file.exists("new.xlsx"))
  # A relative link through a subdirectory, and a chain of two links.
  dir.create("sub")
  file.symlink("../new.xlsx", "sub/rel.csv")
  expect_error(regtab(h8_fit(), xlsx = "new.xlsx", csv = "sub/rel.csv"), "same file")
  file.symlink(file.path(dir, "sub", "rel.csv"), "chain.csv")
  expect_error(regtab(h8_fit(), xlsx = "new.xlsx", csv = "chain.csv"), "same file")
  expect_false(file.exists("new.xlsx"))
})

test_that("review R9: title and footnote are checked as arguments", {
  d <- h8_data()
  expect_error(table1_tc(d, vars = c(x = "contn"), title = NA_character_), "`title` must be a single string")
  expect_error(table1_tc(d, vars = c(x = "contn"), footnote = c("a", "b")), "`footnote` must be a single string")
  expect_no_error(table1_tc(d, vars = c(x = "contn"), title = "", footnote = "Note."))
})

test_that("review R15: tabtools_options() validates persist/clear; clear does not swallow settings", {
  for (v in list(NA, "yes", c(TRUE, FALSE), 1)) {
    expect_error(tabtools_options(clear = v), "`clear` must be TRUE or FALSE")
    expect_error(tabtools_options(persist = v), "`persist` must be TRUE or FALSE")
  }
  withr::local_options(tabtools.font = NULL)
  expect_error(tabtools_options(font = "Arial", clear = TRUE), "cannot be combined")
  expect_null(getOption("tabtools.font"))
})

test_that("review R9: regtab() checks title and footnote as arguments too", {
  expect_error(regtab(h8_fit(), title = NA_character_), "`title` must be a single string")
  expect_error(regtab(h8_fit(), footnote = 1), "`footnote` must be a single string")
})

test_that("Milestone D review P2-4: a non-ASCII string written again into a loaded workbook stays readable outside UTF-8", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("tidyxl")
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale")
  withr::defer(Sys.setlocale("LC_CTYPE", ""))
  tab <- regtab(glm(am ~ wt, binomial(link = "probit"), mtcars), stats = c("n", "r2"))
  p <- withr::local_tempfile(fileext = ".xlsx")
  tt_write_xlsx(tab, p, sheet = "A")
  tt_write_xlsx(tab, p, sheet = "B")
  d <- withr::local_tempdir()
  utils::unzip(p, exdir = d)
  for (f in list.files(file.path(d, "xl", "worksheets"), pattern = "xml$", full.names = TRUE)) {
    expect_false(grepl("<v>NA</v>", rawToChar(readBin(f, "raw", file.size(f))), fixed = TRUE), label = basename(f))
  }
  Sys.setlocale("LC_CTYPE", "")
  v <- tidyxl::xlsx_cells(p)
  expect_identical(sum(v$character == "Pseudo R²", na.rm = TRUE), 2L)
})

test_that("append onto a Markdown file without a final newline leaves a blank line before the table (audit A05)", {
  d <- data.frame(g = rep(c("A", "B"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"))
  f <- withr::local_tempfile(fileext = ".md")
  cat("Some paragraph text without newline", file = f)
  tt_write_markdown(tab, f, append = TRUE)
  x <- readLines(f, encoding = "UTF-8")
  expect_identical(x[1:2], c("Some paragraph text without newline", ""))
  expect_true(startsWith(x[3], "| Mean"))
  # mdappend of table1_tc takes the same path.
  g <- withr::local_tempfile(fileext = ".md")
  cat("Notes", file = g)
  table1_tc(d, by = "g", vars = c(x = "contn"), markdown = g, mdappend = TRUE)
  expect_identical(readLines(g, encoding = "UTF-8")[1:2], c("Notes", ""))
  # A file that ends in a newline still gains exactly one blank line.
  h <- withr::local_tempfile(fileext = ".md")
  writeLines("Notes", h)
  tt_write_markdown(tab, h, append = TRUE)
  expect_identical(readLines(h, encoding = "UTF-8")[1:3], c("Notes", "", readLines(f, encoding = "UTF-8")[3]))
})
