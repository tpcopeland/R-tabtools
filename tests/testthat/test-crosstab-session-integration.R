test_that("crosstab resolves the public session mask into literal publication values", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells = 3L,
                            tabtools.smallcells_mode = "primary", tabtools.masktext = "HIDDEN"))
  d <- data.frame(row = c(0, 1, 0, 1), column = c(0, 0, 1, 1), frequency = c(1, 9, 9, 1))
  x <- suppressMessages(crosstab(d, "row", "column", weights = "frequency"))
  expect_identical(x$stored$smallcells$threshold, 3L)
  expect_identical(x$stored$smallcells$mode, "primary")
  expect_identical(x$stored$smallcells_mode, "primary")
  expect_equal(unname(x$stored$table), matrix(c(NA, 9, 9, NA), 2), tolerance = 0)
  expect_identical(unname(x$body[cbind(c(1L, 2L), c(2L, 3L))]), c("HIDDEN", "HIDDEN"))
  raw <- suppressMessages(crosstab(d, "row", "column", weights = "frequency", smallcells = NULL))
  expect_identical(x$stored$p, raw$stored$p)
  strict <- suppressMessages(crosstab(d, "row", "column", weights = "frequency", smallcells = 3))
  expect_identical(strict$stored$smallcells$mode, "strict")
  expect_identical(strict$stored$smallcells_mode, "full")
  expect_true(is.na(strict$stored$p))
  expect_true(all(is.na(strict$stored$table)))
})

test_that("crosstab explicit disabled masks override session values without clearing them", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells = 3L,
                            tabtools.smallcells_mode = "primary", tabtools.masktext = "HIDDEN"))
  before <- options()[c("tabtools.smallcells", "tabtools.smallcells_mode", "tabtools.masktext")]
  d <- data.frame(row = c(0, 1, 0, 1), column = c(0, 0, 1, 1), frequency = c(1, 9, 9, 1))
  for (args in list(list(smallcells = NULL), list(smallcells = 0), list(nosmallcells = TRUE))) {
    x <- do.call(crosstab, c(list(data = d, rowvar = "row", colvar = "column", weights = "frequency"), args))
    expect_identical(x$stored$smallcells,
                     list(threshold = 0L, mode = "strict", n_masked = 0L, n_linked = 0L))
    expect_equal(unname(x$stored$table), matrix(c(1, 9, 9, 1), 2), tolerance = 0)
    expect_false(any(grepl("HIDDEN", x$body, fixed = TRUE)))
  }
  expect_identical(options()[names(before)], before)
  reused <- suppressMessages(crosstab(d, "row", "column", weights = "frequency"))
  expect_identical(reused$stored$smallcells$mode, "primary")
  expect_true(is.na(reused$stored$table[1L, 1L]))
})

test_that("crosstab explicit mode including NULL is independent of inherited mode", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells_mode = "primary"))
  resolve <- function(args) {
    suppressMessages(.tt_resolve_sinks(args, as.list(setNames(rep(TRUE, length(args)), names(args))),
                                      mask = "crosstab"))
  }
  expect_identical(resolve(list())$mask, list(threshold = NULL, mode = "strict", text = NULL))
  expect_identical(resolve(list(nosmallcells = TRUE))$mask,
                   list(threshold = NULL, mode = "strict", text = NULL))
  expect_identical(resolve(list(smallcells = 0, smallcells_mode = "primary"))$mask,
                   list(threshold = NULL, mode = "primary", text = NULL))
  expect_identical(resolve(list(nosmallcells = TRUE, smallcells_mode = "primary"))$mask$mode, "primary")
  withr::local_options(list(tabtools.smallcells = 3L))
  expect_identical(resolve(list())$mask$mode, "primary")
  expect_identical(resolve(list(smallcells_mode = NULL))$mask$mode, "strict")
  expect_identical(resolve(list(smallcells = 3))$mask$mode, "strict")
  expect_identical(resolve(list(smallcells = 3, smallcells_mode = "primary"))$mask$mode, "primary")
})

test_that("crosstab mask literals and refusals are resolved before file mutation", {
  xt_no_session()
  d <- data.frame(row = c(0, 1, 0, 1), column = c(0, 0, 1, 1), frequency = c(1, 9, 9, 1))
  for (text in c("", '$literal`"\\')) {
    x <- crosstab(d, "row", "column", weights = "frequency", smallcells = 3,
                   smallcells_mode = "primary", masktext = text)
    expect_identical(unname(x$body[1L, 2L]), text)
  }
  native <- crosstab(d, "row", "column", weights = "frequency", smallcells = 3,
                     smallcells_mode = "primary", masktext = NULL)
  expect_identical(unname(native$body[1L, 2L]), "<3")
  directory <- withr::local_tempdir(pattern = "crosstab-session-refusal-")
  path <- file.path(directory, "preserved.csv")
  writeLines("original bytes", path)
  bytes <- readBin(path, "raw", file.info(path)$size)
  for (k in list(1, 2, 3.5, NA_real_, Inf, TRUE, "3", c(3, 4), matrix(3, 1, 1))) {
    expect_error(crosstab(d, "row", "column", smallcells = k, csv = path), class = "error")
    expect_identical(readBin(path, "raw", file.info(path)$size), bytes)
  }
  for (args in list(list(smallcells = 0, nosmallcells = TRUE),
                    list(smallcells = 3, nosmallcells = TRUE))) {
    expect_error(do.call(crosstab, c(list(data = d, rowvar = "row", colvar = "column"), args)),
                 class = "tabtools_error_smallcells_conflict")
  }
  expect_error(crosstab(d, "row", "column", masktext = ""), class = "tabtools_error_smallcells_masktext")
  expect_error(crosstab(d, "row", "column", nosmallcells = NA), class = "tabtools_error_smallcells")
  expect_error(crosstab(d, "row", "column", smallcells_mode = "prim"), class = "tabtools_error_smallcells_mode")
})

test_that("disabled crosstab masks report no false inherited mask use", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells = 3L, tabtools.smallcells_mode = "primary",
                            tabtools.masktext = "HIDDEN"))
  for (args in list(list(smallcells = NULL), list(smallcells = 0), list(nosmallcells = TRUE))) {
    resolved <- NULL
    messages <- capture.output(resolved <- .tt_resolve_sinks(args,
      as.list(setNames(rep(TRUE, length(args)), names(args))), mask = "crosstab"), type = "message")
    expect_identical(messages, character())
    expect_identical(resolved$inherited[["smallcells"]], FALSE)
    expect_identical(resolved$inherited[["masktext"]], FALSE)
    expect_identical(resolved$values$smallcells_mode, "strict")
  }
})

test_that("adding crosstab policy preserves other sink policy results and private state", {
  xt_no_session()
  withr::local_options(list(tabtools.smallcells = 3L, tabtools.smallcells_mode = "primary",
                            tabtools.masktext = "HIDDEN"))
  before <- as.list(.tt_sink_state)
  plain <- .tt_resolve_sinks(list(), list(), mask = "none")
  expect_null(plain$mask)
  expect_null(plain$values$smallcells)
  table1 <- suppressMessages(.tt_resolve_sinks(list(), list(), mask = "table1"))
  expect_identical(table1$mask, list(threshold = 3L, mode = "primary", text = "HIDDEN"))
  rates <- suppressMessages(.tt_resolve_sinks(list(nosmallcells = FALSE), list(), mask = "rates"))
  expect_identical(rates$mask, list(threshold = 3L, text = "HIDDEN"))
  expect_error(.tt_resolve_sinks(list(), list(), mask = "not-a-policy"), class = "error")
  expect_identical(as.list(.tt_sink_state), before)
})
