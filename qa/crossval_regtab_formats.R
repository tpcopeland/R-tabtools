library(testthat)
library(tabtools)

# Direct string() oracle from Stata 17, pinned to the Stata-Tools 2.5.1
# contract (commit 712044f8). Expected text is produced by the native process,
# never by an R formatter or a generated fixture. The shared QA runner emits
# QA-RESULT via qa_run_testthat()/qa_done(), including missing-oracle skips.

formats_native_run <- function(scratch, source_dir, formats, values) {
  if (grepl('["\r\n`]', source_dir)) stop("TABTOOLS_STATA_DIR contains a Stata quoting character", call. = FALSE)
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  lines <- c("version 17.0", "clear all", "set more off",
             sprintf('adopath ++ "%s"', source_dir),
             "quietly findfile regtab.ado", sprintf('assert r(fn) == "%s/regtab.ado"', source_dir),
             "quietly findfile effecttab.ado", sprintf('assert r(fn) == "%s/effecttab.ado"', source_dir),
             'file open cells using "formats-native.txt", write text replace',
             'file write cells "format|value|native" _n')
  for (fmt in formats) {
    for (value in values) {
      lines <- c(lines, sprintf("scalar __value = %s", value),
                 sprintf('file write cells "%s|%s|" (strtrim(string(__value, "%s"))) _n', fmt, value, fmt))
    }
  }
  lines <- c(lines, "file close cells", 'display "FORMATS_PARITY_COMPLETE"')
  writeLines(lines, file.path(scratch, "formats.do"))
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "formats.do"), wd = scratch,
                       error_on_status = FALSE, timeout = 120)
  logs <- list.files(scratch, pattern = "\\.log$", full.names = TRUE)
  if (length(logs) != 1L) stop("Stata format oracle did not produce exactly one log", call. = FALSE)
  log <- readLines(logs, warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  if (proc$status != 0L || length(errors) || !any(trimws(log) == "FORMATS_PARITY_COMPLETE") ||
      !any(grepl("end of do-file", log, fixed = TRUE))) {
    stop("Stata format oracle failed: ", paste(errors, collapse = ", "), call. = FALSE)
  }
  utils::read.delim(file.path(scratch, "formats-native.txt"), sep = "|", quote = "",
                    colClasses = "character", na.strings = NULL, check.names = FALSE)
}

test_that("pinned native Stata f/g/e text agrees in the shared formatter and raw table cells", {
  skip_if_not_installed("processx")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to the pinned Stata 2.5.1 export")
  # Wrong or partial source configuration is a failure, never a skip/pass.
  pinned <- c("regtab.ado" = "544436a8379b28b38cb9df1571809a3c",
              "effecttab.ado" = "21ac264e06c237a7f99388843a40d3ae",
              "_tabtools_flatframe.ado" = "405711be1ad13caaf858d2b9cf8d9135")
  paths <- file.path(source_dir, names(pinned))
  expect_true(all(file.exists(paths)))
  if (!all(file.exists(paths))) stop("Pinned format source files are missing", call. = FALSE)
  expect_identical(unname(tools::md5sum(paths)), unname(pinned))
  if (!identical(unname(tools::md5sum(paths)), unname(pinned))) {
    stop("Native source is not Stata-Tools 712044f8 (tabtools 2.5.1)", call. = FALSE)
  }
  for (file in names(pinned)) {
    cat(sprintf("STATA SOURCE ROW file=%s md5=%s commit=712044f8 version=2.5.1\n", file, pinned[[file]]))
  }
  scratch <- withr::local_tempdir(pattern = "tabtools-native-formats-")
  # Positive/negative, half/tie boundaries, zero, tiny and three-digit
  # exponents; e precision includes the exact native 15-decimal mantissa.
  values <- c("0", "1", "-1", "0.45", "1.25", "12345.67", "0.000000000012345",
              "1e200", "-1e200", "9.999", "1.79775586519292e-13", "1e-200")
  formats <- c("%9.2e", "%12.0e", "%9.0e", "%9.6e", "%4.2e", "%3.2e", "%9.2ec",
               "%-9.2e", "%09.2e", "%24.15e", "%9,2e",
               "%9.2f", "%12.0fc", "%9.3g", "%12.0gc", "%9,2f", "%9,3g", "%12,0fc")
  native <- formats_native_run(scratch, source_dir, formats, values)
  expect_identical(names(native), c("format", "value", "native"))
  expect_identical(nrow(native), as.integer(length(formats) * length(values)))
  expect_false(anyDuplicated(native[, c("format", "value")]) > 0L)
  resolve <- getFromNamespace(".tt_resolve_numeric_format", "tabtools")
  render <- getFromNamespace(".tt_format_numeric", "tabtools")
  direct_fmt <- getFromNamespace("stata_fmt", "tabtools")
  x <- as.double(values)
  passed_formats <- 0L
  refused_cells <- 0L
  for (fmt in formats) {
    cells <- native[native$format == fmt, , drop = FALSE]
    expect_identical(cells$value, values, info = fmt)
    if (fmt == "%9.2ec") {
      expect_identical(cells$native, rep("", length(values)))
      expect_error(direct_fmt(x, fmt), class = "tabtools_error_fmt")
      expect_error(resolve(cformat = fmt), class = "tabtools_error_format")
      refused_cells <- refused_cells + nrow(cells)
      next
    }
    sep <- if (grepl(",", fmt, fixed = TRUE)) " to " else ", "
    spec <- resolve(cformat = fmt, sep = sep)
    expected <- cells$native
    expect_identical(render(x, spec), expected, info = fmt)
    d <- data.frame(term = paste0("v", seq_along(x)), estimate = x, conf.low = x, conf.high = x,
                    p.value = rep(0.125, length(x)))
    attr(d, "conf.level") <- 0.95
    attr(d, "effect_scale") <- "Coef."
    r <- regtab(d, cformat = fmt, sep = sep)
    m <- cbind(x, x, x, rep(0.125, length(x)))
    rownames(m) <- d$term
    e <- effecttab(m, cformat = fmt, sep = sep)
    for (table in list(r, e)) {
      expect_identical(table$body[[2]], expected, info = fmt)
      expect_identical(table$body[[3]], paste0("(", expected, sep, expected, ")"), info = fmt)
    }
    passed_formats <- passed_formats + 1L
    cat(sprintf("STATA FORMAT ROW format=%s values=%d table_estimates=%d table_intervals=%d source=712044f8\n",
                fmt, length(x), 2L * length(x), 2L * length(x)))
  }
  expect_identical(passed_formats, length(formats) - 1L)
  expect_identical(refused_cells, length(values))
  cat(sprintf("STATA FORMATS RECEIPT source=712044f8 source_files=%d formats=%d values=%d native_cells=%d ec_refusals=%d\n",
              length(pinned), length(formats), length(values), nrow(native), refused_cells))
})
