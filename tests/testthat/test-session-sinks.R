test_that("ordinary boundaries inherit both destinations only for an explicit sheet", {
  directory <- ss_local()
  calls <- ss_cases()
  for (name in names(calls)) {
    f <- calls[[name]]
    book <- file.path(directory, paste0(name, ".xlsx"))
    md <- file.path(directory, paste0(name, ".md"))
    tabtools_options(workbook = book, markdown = md)
    expect_s3_class(f(), "tt_table")
    expect_s3_class(f(sheet = NULL), "tt_table")
    expect_false(file.exists(book), info = name)
    expect_false(file.exists(md), info = name)
    suppressMessages(f(sheet = "Selected", title = name))
    expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book))), "Selected", info = name)
    expected_title <- if (name == "survival_rates") "survival\\_rates" else name
    expect_match(ss_text(md), expected_title, fixed = TRUE, info = name)
    before_md <- ss_bytes(md)
    before <- ss_bytes(book)
    suppressMessages(expect_warning(f(sheet = "Ignored", xlsx = NULL, markdown = NULL),
                                   class = "tabtools_warning_sheet_without_workbook"))
    expect_identical(ss_bytes(book), before, info = name)
    other <- file.path(directory, paste0(name, "-explicit.xlsx"))
    suppressMessages(f(xlsx = other, sheet = NULL, markdown = NULL))
    expect_true(file.exists(other), info = name)
    explicit_md <- file.path(directory, paste0(name, "-explicit.md"))
    suppressMessages(f(markdown = explicit_md, xlsx = NULL))
    expect_true(file.exists(explicit_md), info = name)
    suppressMessages(f(sheet = "Second", markdown = NULL))
    expect_identical(ss_bytes(md), before_md, info = name)
    expect_true("Second" %in% unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book))))
  }
})

test_that("puttab and both stacktab modes inherit omissions and honor explicit NULL", {
  directory <- ss_local()
  data <- data.frame(label = "VALUE", value = "CELL")
  block <- puttab(data)
  for (mode in c("puttab", "normal", "frames")) {
    book <- file.path(directory, paste0(mode, ".xlsx"))
    md <- file.path(directory, paste0(mode, ".md"))
    tabtools_options(workbook = book, markdown = md, headershade = TRUE)
    call <- switch(mode,
      puttab = function(...) puttab(data, ...),
      normal = function(...) stacktab(list(block), ...),
      frames = function(...) stacktab(frames = list(data), ...))
    if (mode != "puttab") expect_error(call(), "sheet.*required")
    tt <- suppressMessages(call(sheet = "S", title = mode))
    expect_true(file.exists(book))
    expect_match(ss_text(md), mode, fixed = TRUE)
    expect_identical(tt$style$headershade, mode != "normal")
    if (mode != "normal") {
      expect_false(suppressMessages(call(sheet = "Off", headershade = FALSE))$style$headershade)
    }
    old_md <- ss_bytes(md)
    old_book <- ss_bytes(book)
    expect_s3_class(call(xlsx = NULL, markdown = NULL), "tt_table")
    expect_identical(ss_bytes(md), old_md)
    expect_identical(ss_bytes(book), old_book)
  }
})

test_that("session Markdown history survives A/B/A, repeated setting and clear", {
  directory <- ss_local()
  a <- file.path(directory, "a.md")
  b <- file.path(directory, "b.md")
  publish <- function(title, ...) suppressMessages(puttab(data.frame(x = "CELL"), title = title, ...))
  writeLines("OLD", a)
  tabtools_options(markdown = a)
  publish("FIRST")
  expect_false(grepl("OLD", ss_text(a), fixed = TRUE))
  tabtools_options(markdown = a)
  publish("SECOND")
  tabtools_options(markdown = b)
  publish("B")
  tabtools_options(markdown = a)
  publish("THIRD")
  tabtools_options(clear = TRUE)
  tabtools_options(markdown = a)
  publish("FOURTH")
  for (title in c("FIRST", "SECOND", "THIRD", "FOURTH")) expect_match(ss_text(a), title, fixed = TRUE)
  expect_match(ss_text(b), "### B", fixed = TRUE)
  publish("REPLACE", markdown = a)
  expect_false(grepl("FIRST", ss_text(a), fixed = TRUE))
  publish("APPEND")
  expect_match(ss_text(a), "REPLACE", fixed = TRUE)
  expect_match(ss_text(a), "APPEND", fixed = TRUE)
  tab <- puttab(data.frame(x = "WRITER"), xlsx = NULL, markdown = NULL, title = "WRITER")
  tt_write_markdown(tab, a)
  expect_false(grepl("APPEND", ss_text(a), fixed = TRUE))
  publish("AFTER-WRITER")
  expect_match(ss_text(a), "WRITER", fixed = TRUE)
  publish("EXPLICIT-REPLACE", mdappend = FALSE)
  expect_false(grepl("AFTER-WRITER", ss_text(a), fixed = TRUE))
  publish("EXPLICIT-APPEND", mdappend = TRUE)
  expect_match(ss_text(a), "EXPLICIT-REPLACE", fixed = TRUE)
  expect_match(ss_text(a), "EXPLICIT-APPEND", fixed = TRUE)
})

test_that("explicit writers export one sink and preserve workbook sheets and fallback", {
  directory <- ss_local()
  book <- file.path(directory, "book.xlsx")
  md <- file.path(directory, "report.md")
  tab <- puttab(data.frame(x = "PRESERVE"), sheet = NULL)
  tt_write_xlsx(tab, book, sheet = "Mine")
  tabtools_options(workbook = book, markdown = md)
  tt_write_xlsx(tab, book, sheet = NULL)
  expect_false(file.exists(md))
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book))), c("Mine", "Table"))
  expect_true(any(openxlsx2::wb_to_df(openxlsx2::wb_load(book), sheet = "Mine", col_names = FALSE) == "PRESERVE"))
  expect_true(tabtools:::.tt_path_key(book) %in% tabtools:::.tt_sink_state$written)
  unlink(book)
  tt_write_csv(tab, file.path(directory, "one.csv"))
  expect_false(file.exists(book))
  expect_false(file.exists(md))
  tt_write_markdown(tab, md)
  expect_false(file.exists(book))
  unrelated <- file.path(directory, "unrelated.md")
  tt_write_markdown(tab, unrelated)
  expect_false(tabtools:::.tt_path_key(unrelated) %in% tabtools:::.tt_sink_state$written)
})

test_that("Table 1 preserves every row of native excel alias priority", {
  directory <- ss_local()
  data <- data.frame(x = 1:10)
  session <- file.path(directory, "session.xlsx")
  tabtools_options(workbook = session)
  cases <- list(
    list(excel = "b.xlsx"), list(xlsx = "a.xlsx", excel = "b.xlsx"),
    list(xlsx = NULL, excel = "b.xlsx"), list(xlsx = "a.xlsx"),
    list(xlsx = "a.xlsx", excel = NULL), list(), list(xlsx = NULL),
    list(excel = NULL), list(xlsx = NULL, excel = NULL),
    list(xlsx = "b.xlsx", excel = "b.xlsx"), list(xlsx = "unused.txt", excel = "b.xlsx"))
  targets <- c("b.xlsx", "b.xlsx", "b.xlsx", "a.xlsx", "a.xlsx", "session.xlsx",
               NA, NA, NA, "b.xlsx", "b.xlsx")
  for (i in seq_along(cases)) {
    args <- cases[[i]]
    for (key in intersect(c("xlsx", "excel"), names(args))) {
      if (!is.null(args[[key]])) args[[key]] <- file.path(directory, args[[key]])
    }
    existing <- list.files(directory, full.names = TRUE)
    if (length(existing)) unlink(existing)
    call <- function() do.call(table1_tc, c(list(data = data, vars = c(x = "contn"), sheet = "S"), args))
    if (is.na(targets[i])) {
      expect_warning(call(), class = "tabtools_warning_sheet_without_workbook")
      expect_length(list.files(directory), 0L)
    } else {
      suppressMessages(call())
      expect_identical(list.files(directory), targets[i])
    }
  }
  # A discarded alias is not an extra sink; a final workbook/Markdown alias is.
  unused <- file.path(directory, "unused.xlsx")
  md <- file.path(directory, "report.md")
  suppressMessages(table1_tc(data, vars = c(x = "contn"), xlsx = unused,
                             excel = file.path(directory, "chosen.xlsx"), markdown = md))
  expect_false(file.exists(unused))
  expect_true(file.exists(md))
})

test_that("resolved collisions and warning-to-error happen before bytes or history change", {
  directory <- ss_local()
  md <- file.path(directory, "report.md")
  csv <- file.path(directory, "report.csv")
  writeLines("MD SENTINEL", md)
  writeLines("CSV SENTINEL", csv)
  tabtools_options(markdown = md)
  state <- as.list(tabtools:::.tt_sink_state)
  withr::local_options(warn = 2)
  expect_error(regtab(stats::lm(mpg ~ wt, mtcars), sheet = "S", csv = csv), "sheet.*ignored")
  expect_identical(ss_text(md), "MD SENTINEL")
  expect_identical(ss_text(csv), "CSV SENTINEL")
  expect_identical(as.list(tabtools:::.tt_sink_state), state)
  withr::local_options(warn = 0)
  expect_warning(regtab(stats::lm(mpg ~ wt, mtcars), sheet = "S", csv = csv),
                 class = "tabtools_warning_sheet_without_workbook")
  expect_false(grepl("SENTINEL", ss_text(md), fixed = TRUE))
  expect_false(grepl("SENTINEL", ss_text(csv), fixed = TRUE))
  book <- file.path(directory, "book.xlsx")
  link <- file.path(directory, "alias.md")
  writeLines("BOOK SENTINEL", book)
  skip_on_os("windows")
  expect_true(file.symlink(book, link))
  tabtools_options(workbook = book, markdown = link)
  before <- ss_bytes(book)
  history <- tabtools:::.tt_sink_state$written
  expect_error(puttab(data.frame(x = "NEW")), "same file")
  expect_identical(ss_bytes(book), before)
  expect_identical(tabtools:::.tt_sink_state$written, history)
})

test_that("failed first write retries and sequential sinks count only actual success", {
  directory <- ss_local()
  md <- file.path(directory, "report.md")
  writeLines("OLD", md)
  tabtools_options(markdown = md)
  original <- tabtools:::.write_lines_lf
  local_mocked_bindings(.write_lines_lf = function(...) stop("INJECTED WRITE"), .package = "tabtools")
  expect_error(puttab(data.frame(x = "NEW")), "INJECTED WRITE")
  expect_identical(ss_text(md), "OLD")
  expect_length(tabtools:::.tt_sink_state$written, 0L)
  local_mocked_bindings(.write_lines_lf = original, .package = "tabtools")
  suppressMessages(puttab(data.frame(x = "RETRY")))
  expect_match(ss_text(md), "RETRY", fixed = TRUE)
  expect_false(grepl("OLD", ss_text(md), fixed = TRUE))
  book <- file.path(directory, "book.xlsx")
  fresh_md <- file.path(directory, "sequential.md")
  tabtools_options(workbook = book, markdown = fresh_md)
  expect_false(file.exists(fresh_md))
  expect_false(tabtools:::.tt_path_key(fresh_md) %in% tabtools:::.tt_sink_state$written)
  local_mocked_bindings(.xlsx_write = function(...) stop("LATER WORKBOOK"), .package = "tabtools")
  expect_error(puttab(data.frame(x = "SEQUENTIAL"), sheet = "S"), "Could not write")
  expect_match(ss_text(fresh_md), "SEQUENTIAL", fixed = TRUE)
  expect_true(tabtools:::.tt_path_key(fresh_md) %in% tabtools:::.tt_sink_state$written)
  expect_match(ss_text(md), "RETRY", fixed = TRUE)
  expect_false(tabtools:::.tt_path_key(book) %in% tabtools:::.tt_sink_state$written)
})

test_that("stacktab staging and rollback never mark original paths before commit", {
  directory <- ss_local()
  block <- puttab(data.frame(x = "BLOCK"))
  md <- file.path(directory, "report.md")
  csv <- file.path(directory, "report.csv")
  writeLines("MD SENTINEL", md)
  writeLines("CSV SENTINEL", csv)
  tabtools_options(markdown = md)
  copy <- tabtools:::.tt_file_copy
  local_mocked_bindings(.tt_file_copy = function(from, to, overwrite = FALSE) {
    if (identical(tabtools:::.tt_path_key(to), tabtools:::.tt_path_key(md)) &&
        identical(basename(from), "out.md")) {
      writeLines("PARTIAL COMMIT", to)
      return(FALSE)
    }
    copy(from, to, overwrite = overwrite)
  }, .package = "tabtools")
  expect_error(stacktab(list(block), csv = csv), "every target was restored")
  expect_identical(ss_text(md), "MD SENTINEL")
  expect_identical(ss_text(csv), "CSV SENTINEL")
  expect_length(tabtools:::.tt_sink_state$written, 0L)
  local_mocked_bindings(.tt_file_copy = copy, .package = "tabtools")
  suppressMessages(stacktab(list(block), csv = csv))
  expect_match(ss_text(md), "BLOCK", fixed = TRUE)
  expect_false(grepl("SENTINEL", ss_text(md), fixed = TRUE))
  expect_true(tabtools:::.tt_path_key(md) %in% tabtools:::.tt_sink_state$written)
  md2 <- file.path(directory, "frames.md")
  book <- file.path(directory, "frames.xlsx")
  tt_write_xlsx(block, book, sheet = "Mine")
  tabtools_options(workbook = book, markdown = md2)
  writes <- c(markdown = 0L, xlsx = 0L)
  md_writer <- tt_write_markdown
  xlsx_writer <- tt_write_xlsx
  local_mocked_bindings(
    tt_write_markdown = function(...) {
      writes[["markdown"]] <<- writes[["markdown"]] + 1L
      md_writer(...)
    },
    tt_write_xlsx = function(...) {
      writes[["xlsx"]] <<- writes[["xlsx"]] + 1L
      xlsx_writer(...)
    }, .package = "tabtools")
  suppressMessages(stacktab(frames = list(data.frame(x = "FRAME")),
                            sheet = "Publication", title = "ONLY-ONCE"))
  expect_identical(writes, c(markdown = 1L, xlsx = 1L))
  expect_equal(sum(grepl("### ONLY-ONCE", readLines(md2), fixed = TRUE)), 1L)
  expect_match(ss_text(md2), "FRAME", fixed = TRUE)
  workbook <- openxlsx2::wb_load(book)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(workbook)), c("Mine", "Publication"))
  expect_true(any(openxlsx2::wb_to_df(workbook, sheet = "Mine", col_names = FALSE) == "BLOCK"))
  expect_true(any(openxlsx2::wb_to_df(workbook, sheet = "Publication", col_names = FALSE) == "FRAME"))
})

test_that("existing mask capabilities retain session defaults and explicit opt-outs", {
  ss_local()
  raw_rates <- stratetab(ss_rate(), smallcells = 0)
  tabtools_options(smallcells = 3, smallcells_mode = "primary", masktext = "HIDDEN")
  rates <- suppressMessages(stratetab(ss_rate()))
  expect_identical(rates$stored$smallcells$mode, "primary")
  expect_identical(rates$stored$smallcells$threshold, 3L)
  expect_true(any(as.matrix(rates$body) == "HIDDEN"))
  expect_identical(rates$stored$rates, raw_rates$stored$rates)
  for (args in list(list(smallcells = NULL), list(smallcells = 0), list(nosmallcells = TRUE))) {
    result <- suppressMessages(do.call(stratetab, c(list(x = ss_rate()), args)))
    expect_identical(result$stored$smallcells$threshold, 0L)
    expect_false(any(as.matrix(result$body) == "HIDDEN"))
  }
  expect_true(any(as.matrix(suppressMessages(stratetab(ss_rate(), masktext = "EXPLICIT"))$body) == "EXPLICIT"))
  expect_identical(suppressMessages(stratetab(ss_rate()))$stored$smallcells$threshold, 3L)
  expect_error(stratetab(ss_rate(), smallcells = 3, nosmallcells = TRUE), class = "tabtools_error_smallcells_conflict")
  data <- data.frame(g = rep(c("A", "B"), each = 5), x = rep(c(0, 1, 1, 1, 1), 2))
  tab <- suppressMessages(table1_tc(data, vars = c(x = "bin"), by = "g"))
  expect_identical(tab$stored$smallcells$threshold, 3L)
  expect_identical(tab$stored$smallcells$mode, "primary")
  expect_identical(table1_tc(data, vars = c(x = "bin"), by = "g", smallcells = 0)$stored$smallcells$threshold, 0L)
  # Transport keeps native explicit-threshold strict precedence.
  resolved <- tabtools:::.tt_resolve_sinks(list(smallcells = 5), list(smallcells = TRUE), mask = "table1")
  expect_identical(resolved$values$smallcells, 5L)
  expect_identical(resolved$given$smallcells, TRUE)
  expect_identical(resolved$values$smallcells_mode, "strict")
  expect_identical(tabtools:::.tt_resolve_sinks(list(), list(), mask = "table1")$values$smallcells_mode, "primary")
})

test_that("ordinary shading stays explicit and external options preserve path identity", {
  directory <- ss_local()
  tabtools_options(headershade = TRUE)
  fit <- stats::lm(mpg ~ wt, mtcars)
  expect_false(regtab(fit)$style$headershade)
  expect_true(regtab(fit, headershade = TRUE)$style$headershade)
  expect_error(stacktab(list(puttab(data.frame(x = "BLOCK"), xlsx = NULL, markdown = NULL)),
                        headershade = FALSE), "require frames")
  md <- file.path(directory, "report.md")
  options(tabtools.markdown = md)
  suppressMessages(puttab(data.frame(x = "FIRST")))
  withr::local_dir(directory)
  options(tabtools.markdown = "./report.md")
  suppressMessages(puttab(data.frame(x = "SECOND")))
  expect_match(ss_text(md), "FIRST", fixed = TRUE)
  expect_match(ss_text(md), "SECOND", fixed = TRUE)
})


test_that("Table 1 keeps legacy invalid-threshold errors and session reuse after opt-outs", {
  ss_local()
  data <- data.frame(g = rep(c("A", "B"), each = 5), x = rep(c(0, 1, 1, 1, 1), 2))
  tabtools_options(smallcells = 3)
  for (value in list(2, 4.5, "3", NA_real_, NaN, Inf, -Inf, .Machine$integer.max + 1)) {
    error <- tryCatch(table1_tc(data, vars = c(x = "bin"), by = "g", smallcells = value),
                      error = identity)
    expect_s3_class(error, "rlang_error")
    expect_false(inherits(error, "tabtools_smallcells_input"))
    expect_match(conditionMessage(error), "integer greater than or equal to 3", fixed = TRUE)
  }
  for (value in list(NULL, 0)) {
    expect_identical(table1_tc(data, vars = c(x = "bin"), by = "g", smallcells = value)$stored$smallcells$threshold, 0L)
    expect_identical(suppressMessages(table1_tc(data, vars = c(x = "bin"), by = "g"))$stored$smallcells$threshold, 3L)
  }
})

test_that("review 2026-10-07 B3: commands without small-cell support warn once under a session threshold", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL, tabtools.smallcells = 5L))
  d <- data.frame(t = 1:8, e = c(1, 1, 1, 0, 1, 0, 1, 0), g = rep(1:2, 4), w = seq(0.5, 2, length.out = 8))
  warnings <- list()
  x <- withCallingHandlers(
    survtab(d, time = "t", event = "e", times = c(2, 6), by = "g", riskset = TRUE, events = TRUE),
    warning = function(w) { warnings[[length(warnings) + 1L]] <<- w; invokeRestart("muffleWarning") })
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "tabtools_warning_smallcells_unsupported")
  expect_match(conditionMessage(warnings[[1L]]), "threshold (5)", fixed = TRUE)
  expect_identical(x$command, "survtab")
  expect_warning(wttab(d, weights = "w", by = "g"), class = "tabtools_warning_smallcells_unsupported")
  withr::local_options(list(tabtools.smallcells = NULL))
  expect_no_warning(survtab(d, time = "t", event = "e", times = 2))
  expect_no_warning(wttab(d, weights = "w"))
})
