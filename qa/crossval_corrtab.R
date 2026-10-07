library(testthat)
library(tabtools)

test_that("CR001-CR013 native coefficient, p, count matrices and all sinks agree", {
  skip_if_not_installed("processx")
  skip_if_not_installed("tidyxl")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp unavailable")
  native <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(native), "TABTOOLS_STATA_DIR must name the pinned 2.5.1 source export")
  native <- normalizePath(native, winslash = "/", mustWork = TRUE)
  if (grepl('["\r\n`]', native)) stop("Unsupported Stata path quoting")
  critical <- c(corrtab.ado = "341921ffea86626a5eb92583a90cdea4",
    `_tabtools_xlsx_write.ado` = "598c5cad1ad8f42d508877de7fcdabbf")
  rank_source <- "/usr/local/stata17/ado/base/s/spearman.ado"
  authenticate <- function() {
    if (!identical(unname(tools::md5sum(file.path(native, names(critical)))), unname(critical)) ||
        !identical(unname(tools::md5sum(rank_source)), "0cfb5f84ec0cee96f9cf56f64ff535e4")) {
      stop("Native corr source or installed Stata17 rank engine differs from the grounded pins")
    }
    paths <- sort(list.files(native, pattern = "\\.(ado|mata|sthlp)$", recursive = TRUE))
    stats::setNames(unname(tools::md5sum(file.path(native, paths))), paths)
  }
  before <- authenticate()
  scratch <- withr::local_tempdir(pattern = "tabtools-native-corrtab-")
  recipe <- normalizePath(test_path("stata", "make_corrtab.do"), winslash = "/", mustWork = TRUE)
  writeLines(c("version 17.0", "clear all", "set more off",
    sprintf('adopath ++ "%s"', native),
    'quietly findfile corrtab.ado', sprintf('assert r(fn) == "%s/corrtab.ado"', native),
    'quietly findfile spearman.ado', sprintf('assert r(fn) == "%s"', rank_source),
    sprintf('do "%s" "%s" "%s"', recipe, native, scratch)), file.path(scratch, "native.do"))
  run <- processx::run(Sys.which("stata-mp"), c("-b", "do", "native.do"), wd = scratch,
    error_on_status = FALSE, timeout = 120)
  log <- readLines(file.path(scratch, "native.log"), warn = FALSE)
  expect_identical(run$status, 0L)
  expect_false(any(grepl("^r\\([0-9]+\\);", trimws(log))))
  expect_true(any(trimws(log) == "CR_PARITY_COMPLETE"))
  if (run$status != 0L || !any(trimws(log) == "CR_PARITY_COMPLETE")) stop("Native correlation controls incomplete")
  expect_identical(authenticate(), before)
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  base <- data.frame(x = c(1, 2, 3, 4, NA, NA), y = c(1, 2, 4, 3, 8, NA),
                     z = c(NA, 1, 2, 3, 4, 5))
  for (k in 1:13) {
    id <- sprintf("CR%03d", k)
    d <- base
    args <- list(vars = names(d), pvalues = TRUE)
    if (k == 2L) args$upper <- TRUE
    if (k == 3L) args$full <- TRUE
    if (k == 4L) {
      d <- data.frame(x = c(1, 1, 2, 3), y = c(1, 2, 2, 3))
      args <- list(vars = names(d), spearman = TRUE, full = TRUE, pvalues = TRUE)
    }
    if (k == 5L) {
      d <- data.frame(x = c(1, 2, 3, 4, NA), y = c(1, 3, 5, 4, 2))
      args <- list(vars = names(d), spearman = TRUE, pvalues = TRUE)
    }
    if (k %in% 6:8) {
      d <- data.frame(x = 1:3, positive = 1:3, negative = 3:1)
      if (k == 8L) d <- d[1:2, ]
      args <- list(vars = names(d), full = TRUE, spearman = k != 7L, pvalues = TRUE)
    }
    if (k %in% 9:10) {
      d <- data.frame(x = 1:4, constant = 1, singleton = c(1, NA, NA, NA))
      args <- list(vars = if (k == 9L) names(d) else c("constant", "singleton"), full = TRUE, pvalues = TRUE)
    }
    if (k == 11L) args <- list(vars = names(d), full = TRUE, star = c(.1, .3, .6))
    if (k == 12L) {
      attr(d$x, "label") <- attr(d$y, "label") <- "Repeated label"
      args <- list(vars = c("x", "y"), subset = seq_len(nrow(d)) <= 4L,
        upper = TRUE, pvalues = TRUE, digits = 4L, borderstyle = "academic")
    }
    if (k == 13L) {
      d <- data.frame(x = rep(NA_real_, 2), y = NA_real_)
      args <- list(vars = names(d), full = TRUE, spearman = TRUE, pvalues = TRUE)
    }
    args <- c(args, list(data = d, title = "Title", headershade = TRUE, zebra = TRUE, sheet = "S",
      xlsx = file.path(scratch, paste0(id, "-r.xlsx")), csv = file.path(scratch, paste0(id, "-r.csv")),
      markdown = file.path(scratch, paste0(id, "-r.md"))))
    tt <- do.call(corrtab, args)
    want <- utils::read.csv(file.path(scratch, paste0(id, "_matrices.csv")), na.strings = ".",
      colClasses = c(row = "integer", col = "integer", row_variable = "character",
        column_variable = "character", C = "numeric", P = "numeric", N = "numeric"))
    K <- length(args$vars)
    expect_identical(names(want), c("row", "col", "row_variable", "column_variable", "C", "P", "N"), info = id)
    expect_identical(vapply(want[c("C", "P", "N")], typeof, ""),
                     c(C = "double", P = "double", N = "double"), info = id)
    expect_identical(nrow(want), K * K, info = id)
    expect_identical(want$row, rep(seq_len(K), each = K), info = id)
    expect_identical(want$col, rep(seq_len(K), times = K), info = id)
    expect_false(anyDuplicated(paste(want$row, want$col)) > 0L, info = id)
    for (field in c("C", "P", "N")) {
      expect_identical(dim(tt$stored[[field]]), c(K, K), info = paste(id, field))
      expect_identical(dimnames(tt$stored[[field]]), list(args$vars, args$vars), info = paste(id, field))
    }
    expect_identical(want$row_variable, rownames(tt$stored$C)[want$row], info = id)
    expect_identical(want$column_variable, colnames(tt$stored$C)[want$col], info = id)
    for (field in c("C", "P", "N")) {
      expect_equal(unname(tt$stored[[field]][cbind(want$row, want$col)]), want[[field]], tolerance = if (field == "N") 0 else 1e-12, info = paste(id, field))
    }
    for (ext in c("csv", "md")) {
      got <- file.path(scratch, paste0(id, "-r.", ext))
      native_path <- file.path(scratch, paste0(id, ".", ext))
      expect_identical(readBin(got, "raw", file.info(got)$size), readBin(native_path, "raw", file.info(native_path)$size), info = paste(id, ext))
    }
    why <- golden_compare_styles(args$xlsx, "S", file.path(scratch, paste0(id, ".xlsx")), "S",
      got_width_offset = golden_r_width_offset)
    expect_identical(why, character(), info = paste(id, paste(why, collapse = "\n")))
    cat(sprintf("STATA CORRTAB ROW id=%s source=712044f8 engine=Stata17-spearman4.2.8 matrices=3 sinks=3\n", id))
  }
  cat("STATA CORRTAB RECEIPT scenarios=13 existing_golden_updates=0\n")
})
