library(testthat)
library(tabtools)

test_that("RT001-RT012 authentic native tables and numeric returns agree", {
  skip_if_not_installed("processx")
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp unavailable")
  native <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(native), "TABTOOLS_STATA_DIR must name the pinned 2.5.1 export")
  native <- normalizePath(native, winslash = "/", mustWork = TRUE)
  if (grepl('["\r\n`]', native)) stop("Pinned native path has unsupported quoting characters")
  critical <- c(ratetab.ado = "309deb89cd39e7daa461bfd588890fa7",
    stratetab.ado = "9d92707a0f5defd9b81177a8f9129ad4",
    `_tabtools_xlsx_write.ado` = "598c5cad1ad8f42d508877de7fcdabbf")
  authenticate <- function() {
    if (!identical(unname(tools::md5sum(file.path(native, names(critical)))), unname(critical))) {
      stop("Native rate source differs from 712044f8/2.5.1")
    }
    paths <- list.files(native, pattern = "\\.(ado|mata|sthlp)$", recursive = TRUE)
    stats::setNames(unname(tools::md5sum(file.path(native, paths))), paths)
  }
  before <- authenticate()
  scratch <- withr::local_tempdir(pattern = "tabtools-native-ratetab-")
  recipe <- normalizePath(test_path("stata", "make_ratetab.do"), winslash = "/", mustWork = TRUE)
  lines <- c("version 17.0", "clear all", "set more off", "set linesize 255",
             sprintf('adopath ++ "%s"', native),
             sprintf('do "%s" "%s" "%s"', recipe, native, scratch))
  writeLines(lines, file.path(scratch, "native.do"))
  process <- processx::run(Sys.which("stata-mp"), c("-b", "do", "native.do"), wd = scratch,
                          error_on_status = FALSE, timeout = 180)
  log <- readLines(file.path(scratch, "native.log"), warn = FALSE)
  expect_identical(process$status, 0L)
  expect_false(any(grepl("^r\\([0-9]+\\);", trimws(log))))
  expect_true(any(trimws(log) == "RT_PARITY_COMPLETE"))
  if (process$status != 0L || !any(trimws(log) == "RT_PARITY_COMPLETE")) stop("Native rate controls incomplete")
  expect_identical(authenticate(), before)
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  base <- data.frame(g = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 4), h = rep(1:2, 5),
    e = c(3, 1, 2, 0, 1, 4, 0, 1, 0, 0), f = c(1, 4, 0, 1, 2, 1, 0, 0, 0, 0),
    y = c(1, 2, 1, 3, 2, 1, 3, 1, 4, 0), z = c(2, 1, 3, 1, 4, 2, 1, 3, 2, 0),
    cid = c(1, 2, 3, 4, 1, 2, 4, 5, 6, 7), w = c(2, rep(1, 9)))
  base$g <- haven::labelled(base$g, c(A = 1, B = 2, C = 3, D = 4), label = "Group")
  for (k in 1:12) {
    id <- sprintf("RT%03d", k)
    d <- base
    args <- list(by = c("g", "h"), events = "e", exposure = "y", per = 1)
    if (k %in% c(2L, 3L)) args$events <- c("e", "f")
    if (k == 2L) args$exposure <- c("y", "z")
    if (k == 3L) {
      d$f[2] <- NA
      d$g[c(5, 6)] <- NA
      d$h[6] <- NA
    }
    if (k == 4L) args <- c(args, list(ci = "poisson", level = .9, pyscale = 10, pydigits = 1))
    if (k == 5L) args <- c(args, list(ci = "cluster", cluster = "cid"))
    if (k == 6L) {
      d <- data.frame(g = c(1, 1, 2, 2), cid = c(1, 2, 2, 3), e = c(4, 2, 1, 0), y = 1)
      args <- list(by = "g", events = "e", exposure = "y", per = 1,
                   ci = "cluster", cluster = "cid", smallcells = 2, excludemasked = TRUE)
    }
    if (k %in% 7:9) {
      d <- data.frame(g = 1, cid = 1:3, e = c(6, 0, 0), y = 1)
      if (k == 8L) d$cid <- 1
      if (k == 9L) d$e <- 1
      args <- list(by = "g", events = "e", exposure = "y", per = 1, ci = "cluster", cluster = "cid")
    }
    if (k == 10L) {
      d <- data.frame(group = haven::labelled(c(2, 1), c(Low = 1, High = 2), label = "Grouping label"),
                      e = c(1, 0), y = c(2, 4), g_group = c("X", "Y"))
      args <- list(by = c("group", "g_group", "group"), events = "e", exposure = "y", per = 1,
                   saving = file.path(scratch, "RT010-r-saved.dta"))
    }
    if (k == 11L) {
      d <- data.frame(g = 1:3, e = c(1, 0, 0), y = c(2, 4, 0))
      args <- list(by = "g", events = "e", exposure = "y", per = 1, smallcells = 2,
                   zerocells = "dash", zerocells_persontime = TRUE,
                   saving = file.path(scratch, "RT011-r-saved.dta"))
    }
    if (k == 12L) {
      d <- data.frame(g = c(1, 1, 1, 2), cid = 1:4, e = c(3, 0, 0, 100), y = 1, w = c(2, 1, 1, 0))
      args <- list(by = "g", events = "e", exposure = "y", per = 1, ci = "cluster", cluster = "cid", fweight = "w")
    }
    args$data <- d
    args$title <- "Title"
    args$headershade <- args$zebra <- TRUE
    args$xlsx <- file.path(scratch, paste0(id, "-r.xlsx"))
    args$csv <- file.path(scratch, paste0(id, "-r.csv"))
    args$markdown <- file.path(scratch, paste0(id, "-r.md"))
    args$sheet <- "S"
    x <- do.call(ratetab, args)
    native_est <- utils::read.csv(file.path(scratch, paste0(id, "_estimates.csv")), na.strings = ".")
    expect_equal(unname(x$stored$estimates), unname(as.matrix(native_est)), tolerance = 1e-9, info = id)
    counts <- utils::read.csv(file.path(scratch, paste0(id, "_counts.csv")), na.strings = ".")
    for (name in names(counts)) {
      if (!is.na(counts[[name]])) expect_equal(x$stored[[name]], counts[[name]], info = paste(id, name))
    }
    if (!is.null(x$stored$clusters)) {
      expected <- utils::read.csv(file.path(scratch, paste0(id, "_clusters.csv")), na.strings = ".")
      actual <- x$stored$clusters[cbind(expected$group, expected$outcome)]
      expect_equal(unname(actual), expected$clusters, info = id)
    }
    for (ext in c("csv", "md")) {
      want <- file.path(scratch, paste0(id, ".", ext))
      got <- file.path(scratch, paste0(id, "-r.", ext))
      expect_identical(readBin(got, "raw", file.info(got)$size), readBin(want, "raw", file.info(want)$size), info = paste(id, ext))
    }
    why <- golden_compare_styles(file.path(scratch, paste0(id, "-r.xlsx")), "S",
      file.path(scratch, paste0(id, ".xlsx")), "S", got_width_offset = golden_r_width_offset)
    expect_identical(why, character(), info = paste(id, paste(why, collapse = "\n")))
    if (k %in% c(10L, 11L)) {
      want <- haven::read_dta(file.path(scratch, paste0(id, "_saved.dta")))
      got <- haven::read_dta(args$saving)
      expect_identical(names(got), names(want))
      for (name in names(want)) {
        expect_equal(as.vector(got[[name]]), as.vector(want[[name]]), tolerance = 1e-9, info = paste(id, name))
        expect_identical(attr(got[[name]], "label", exact = TRUE), attr(want[[name]], "label", exact = TRUE), info = paste(id, name, "label"))
        expect_equal(attr(got[[name]], "labels", exact = TRUE), attr(want[[name]], "labels", exact = TRUE), info = paste(id, name, "value labels"))
      }
    }
    cat(sprintf("STATA RATETAB ROW id=%s source=712044f8 version=2.5.1 numeric_returns=1 text_sinks=2 workbook_style=1\n", id))
  }
  cat("STATA RATETAB RECEIPT scenarios=12 source=712044f8 existing_golden_updates=0\n")
})
