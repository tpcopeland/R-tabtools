test_that("colours accept Stata names, RGB triplets, and hex", {
  p <- tabtools:::tt_parse_color
  expect_identical(p("219 229 241"), "FFDBE5F1")
  expect_identical(p("navy"), "FF000080")
  expect_identical(p("#dbe5f1"), "FFDBE5F1")
  expect_identical(p("\"255 230 204\""), "FFFFE6CC")
  expect_error(p("chartreuse"), "not a supported")
  expect_error(p("1 2 300"), "between 0 and 255")
  expect_error(p("1 2"), "colour name")
})

test_that("style resolution maps borderstyle and validates thresholds", {
  withr::local_options(tabtools.font = NULL, tabtools.fontsize = NULL, tabtools.borderstyle = NULL,
                       tabtools.boldp = NULL, tabtools.headercolor = NULL, tabtools.zebracolor = NULL)
  s <- tabtools:::tt_resolve_style()
  expect_identical(s[c("font", "fontsize", "borderstyle", "hborder", "vborder")],
                   list(font = "Arial", fontsize = 10L, borderstyle = "thin", hborder = "thin", vborder = "thin"))
  expect_identical(s$headercolor, "FFDBE5F1")
  expect_identical(s$zebracolor, "FFEDF2F9")
  a <- tabtools:::tt_resolve_style(borderstyle = "academic")
  expect_identical(c(a$hborder, a$vborder), c("medium", NA))
  expect_identical(tabtools:::tt_resolve_style(borderstyle = "default")$borderstyle, "thin")
  expect_error(tabtools:::tt_resolve_style(borderstyle = "thick"), "borderstyle")
  expect_error(tabtools:::tt_resolve_style(fontsize = 73), "fontsize")
  expect_error(tabtools:::tt_resolve_style(boldp = 1.5), "boldp")
  expect_identical(tabtools:::tt_resolve_style(boldp = -1)$boldp, NA_real_)
  expect_error(tabtools:::tt_resolve_style(smdthreshold = 0), "smdthreshold")
})

test_that("tabtools_options sets, persists, reloads, and clears defaults", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  withr::local_options(tabtools.font = NULL, tabtools.fontsize = NULL, tabtools.borderstyle = NULL,
                       tabtools.digits = NULL, tabtools.boldp = NULL, tabtools.headercolor = NULL,
                       tabtools.zebracolor = NULL)
  expect_length(tabtools_options(), 0L)
  tabtools_options(font = "Calibri", fontsize = 11, borderstyle = "medium")
  expect_identical(tabtools_options()$font, "Calibri")
  s <- tabtools:::tt_resolve_style()
  expect_identical(c(s$font, s$borderstyle), c("Calibri", "medium"))
  expect_identical(tabtools:::tt_resolve_style(font = "Arial")$font, "Arial")
  tabtools_options(digits = 3, persist = TRUE)
  f <- tabtools:::.tt_defaults_file()
  expect_true(file.exists(f))
  options(tabtools.font = NULL, tabtools.digits = NULL)
  tabtools:::.tt_load_persisted()
  expect_identical(getOption("tabtools.font"), "Calibri")
  expect_identical(getOption("tabtools.digits"), 3)
  tabtools_options(clear = TRUE, persist = TRUE)
  expect_false(file.exists(f))
  expect_length(tabtools_options(), 0L)
  expect_error(tabtools_options(fontsize = 0), "fontsize")
  expect_error(tabtools_options(digits = 7), "digits")
  expect_error(tabtools_options(headercolor = "nope"), "colour")
})

test_that("H14: the documented fontsize ranges hold (persistent 6-72, per table 1-72)", {
  withr::local_options(tabtools.fontsize = NULL)
  expect_error(tabtools_options(fontsize = 5), "between 6 and 72")
  expect_error(tabtools_options(fontsize = 73), "fontsize")
  expect_no_error(tabtools_options(fontsize = 6))
  withr::local_options(tabtools.fontsize = NULL)
  d <- data.frame(g = rep(1:2, 5), x = 1:10)
  expect_identical(table1_tc(d, by = "g", vars = c(x = "contn"), fontsize = 3)$style$fontsize, 3L)
  expect_identical(regtab(stats::lm(mpg ~ wt, mtcars), fontsize = 1)$style$fontsize, 1L)
  expect_error(table1_tc(d, by = "g", vars = c(x = "contn"), fontsize = 73), "between 1 and 72")
})

test_that("session options query, individual clearing and validation are atomic", {
  directory <- ss_local()
  book <- file.path(directory, "book.xlsx")
  md <- file.path(directory, "report.md")
  tabtools_options(font = "Calibri", borderstyle = "academic", workbook = book,
                   markdown = md, headershade = TRUE, smallcells = 3,
                   smallcells_mode = "primary", masktext = "")
  query <- tabtools_options()
  expect_identical(query$workbook, tabtools:::.tt_path_key(book))
  expect_identical(query$markdown, tabtools:::.tt_path_key(md))
  expect_identical(query$smallcells, 3L)
  expect_identical(query$masktext, "")
  before <- tabtools_options()
  state <- as.list(tabtools:::.tt_sink_state)
  expect_error(tabtools_options(font = "Arial", workbook = "bad.txt"), "workbook")
  expect_identical(tabtools_options(), before)
  expect_identical(as.list(tabtools:::.tt_sink_state), state)
  expect_error(tabtools_options(headershade = 1), class = "tabtools_error_session_option")
  expect_error(tabtools_options(smallcells = 2), "smallcells")
  expect_error(tabtools_options(smallcells_mode = "full"), class = "tabtools_error_session_option")
  expect_error(tabtools_options(masktext = NA_character_), "masktext")
  for (key in names(query)) {
    tabtools_options(clear = TRUE)
    do.call(tabtools_options, query)
    do.call(tabtools_options, stats::setNames(list(NULL), key))
    expect_null(tabtools_options()[[key]])
    expect_identical(tabtools_options(), query[setdiff(names(query), key)])
  }
  tabtools_options(clear = TRUE)
  withr::local_dir(directory)
  tabtools_options(workbook = "./book.xlsx", markdown = "./report.md")
  withr::local_dir(dirname(directory))
  expect_identical(tabtools_options()$workbook, tabtools:::.tt_path_key(book))
  expect_identical(tabtools_options()$markdown, tabtools:::.tt_path_key(md))
})

test_that("persist saves only formatting and rejects explicit session changes", {
  directory <- ss_local()
  withr::local_envvar(R_USER_CONFIG_DIR = file.path(directory, "config"))
  tabtools_options(borderstyle = "academic", markdown = file.path(directory, "report.md"),
                   smallcells = 3, masktext = "SECRET")
  before <- tabtools_options()
  for (key in tabtools:::.tt_session_option_keys) {
    expect_error(do.call(tabtools_options, c(stats::setNames(list(NULL), key), list(persist = TRUE))),
                 class = "tabtools_error_session_persist")
  }
  expect_identical(tabtools_options(), before)
  tabtools_options(persist = TRUE)
  file <- tabtools:::.tt_defaults_file()
  expect_identical(colnames(read.dcf(file)), "borderstyle")
  expect_identical(unname(read.dcf(file)[1, "borderstyle"]), "academic")
  tabtools_options(borderstyle = NULL, persist = TRUE)
  expect_false("borderstyle" %in% colnames(read.dcf(file)))
  # Even a hand-edited DCF cannot restore session-only keys.
  write.dcf(data.frame(borderstyle = "medium", markdown = "unwanted.md"), file)
  tabtools_options(clear = TRUE)
  tabtools:::.tt_load_persisted()
  expect_identical(tabtools_options(), list(borderstyle = "medium"))
  tabtools_options(clear = TRUE, persist = TRUE)
  expect_false(file.exists(file))
})
