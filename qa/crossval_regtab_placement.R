library(testthat)
library(tabtools)

# Additive RL001-RL004 controls. Existing 272 fixture/artifact baseline is
# untouched. Both engines fit the same deterministic data; p-values are
# omitted, never numerically compared as a native inference oracle.
wp4b_native_pin <- c(
  "_regtab_scale.ado" = "f4ef486205ebc47c6f56c74198540a8f",
  "_regtab_modelnoun.ado" = "7781dc09b90c350911e0262728828613",
  "_regtab_methods.ado" = "a28af5152bf9a02e784873bb3c7b8106",
  "regtab.ado" = "544436a8379b28b38cb9df1571809a3c",
  "_regtab_cellnote.ado" = "9e8db4fe52a83d26ef5084393731d489",
  "_regtab_addrow.ado" = "3126fc9c3b75e0ee907d4d2dd2507eef",
  "_regtab_addcol.ado" = "0b2cb152f958b0ea93650e5f57f9e7a0",
  "_regtab_collabels.ado" = "a494a4f463487c59151c7d9312f9fee4",
  "_regtab_fvparent.ado" = "ac10341b799b1c08ded956b5f78fa3cf",
  "_tabtools_flatframe.ado" = "405711be1ad13caaf858d2b9cf8d9135")

test_that("RL001-RL004 pinned placement and transpose agree in publication sinks", {
  skip_if_not_installed("processx")
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("haven")
  skip_if_not_installed("survival")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not available")
  native <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(native), "TABTOOLS_STATA_DIR must name the pinned 2.5.1 export")
  native <- normalizePath(native, winslash = "/", mustWork = TRUE)
  if (grepl('["\r\n`]', native)) stop("Pinned native path has unsupported quoting characters")
  authenticate <- function() {
    hashes <- unname(tools::md5sum(file.path(native, names(wp4b_native_pin))))
    if (!identical(hashes, unname(wp4b_native_pin))) stop("Native source differs from 712044f8/2.5.1", call. = FALSE)
  }
  authenticate()
  scratch <- withr::local_tempdir(pattern = "tabtools-regtab-placement-")
  data <- data.frame(g = rep(1:3, each = 8), x = rep(1:8, 3))
  data$lower <- seq_len(nrow(data))
  data$upper <- data$lower + rep(c(1, 2, 1, 3), 6)
  data$Y <- c(-1, 0, 2)[data$g] + .25 * data$x + rep(c(.1, -.2, .2, -.1), 6)
  haven::write_dta(data, file.path(scratch, "input.dta"))
  options <- list(
    RL001 = list(native = "reftop", r = list(reftop = TRUE)),
    RL002 = list(native = 'reftop cellnote("A" 1 "Native note") addrow("Inside" "first" "second", after(g))',
                 r = list(reftop = TRUE, cellnote = list(list(row = "A", model = 1L, text = "Native note")),
                          addrow = list(list(label = "Inside", values = c("first", "second"), after = "g")))),
    RL003 = list(native = 'transpose stats(n) collabels(1.g "Drug A") addcol("Text" "one" "two", after(1.g))',
                 r = list(transpose = TRUE, stats = "n", collabels = c("1.g" = "Drug A"),
                          addcol = list(list(label = "Text", values = c("one", "two"), after = "1.g")))),
    RL004 = list(native = "", r = list()))
  lines <- c("version 17.0", "clear all", "set more off", "set linesize 255",
             sprintf('adopath ++ "%s"', native))
  for (file in names(wp4b_native_pin)) lines <- c(lines, sprintf('quietly findfile %s', file), sprintf('assert r(fn) == "%s/%s"', native, file))
  recipe <- normalizePath(test_path("stata", "make_regtab_placement.do"), winslash = "/", mustWork = TRUE)
  lines <- c(lines, sprintf('do "%s" "%s" "%s" "%s"', recipe, native, scratch, file.path(scratch, "input.dta")))
  writeLines(lines, file.path(scratch, "native.do"))
  process <- processx::run(Sys.which("stata-mp"), c("-b", "do", "native.do"), wd = scratch,
                           error_on_status = FALSE, timeout = 120)
  log <- readLines(file.path(scratch, "native.log"), warn = FALSE)
  expect_identical(process$status, 0L)
  expect_false(any(grepl("^r\\([0-9]+\\);", trimws(log))))
  expect_true(any(trimws(log) == "RL_PARITY_COMPLETE"))
  expect_true(any(grepl("end of do-file", log, fixed = TRUE)))
  if (process$status != 0L || !any(trimws(log) == "RL_PARITY_COMPLETE")) stop("Native placement controls incomplete")
  authenticate()
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  data$g <- stats::relevel(factor(data$g, levels = 1:3, labels = c("A", "B", "C")), "B")
  attr(data$g, "label") <- "Group"
  attr(data$g, "labels") <- c(A = 1, B = 2, C = 3)
  fit <- stats::lm(Y ~ g + x, data)
  interval_fit <- survival::survreg(survival::Surv(lower, upper, type = "interval2") ~ x, data = data, dist = "weibull", model = TRUE)
  for (id in names(options)) {
    source_fit <- if (identical(id, "RL004")) interval_fit else fit
    tt <- do.call(regtab, c(list(source_fit, source_fit), list(models = c("Crude", "Adjusted"), title = "Title",
                          nointercept = TRUE, nopvalue = TRUE, headershade = TRUE, zebra = TRUE,
                          xlsx = file.path(scratch, paste0(id, "-r.xlsx")), sheet = "S",
                          csv = file.path(scratch, paste0(id, "-r.csv")), markdown = file.path(scratch, paste0(id, "-r.md"))), options[[id]]$r))
    for (extension in c("csv", "md")) {
      a <- file.path(scratch, paste0(id, ".", extension)); b <- file.path(scratch, paste0(id, "-r.", extension))
      expect_identical(readBin(b, "raw", n = file.info(b)$size), readBin(a, "raw", n = file.info(a)$size), info = paste(id, extension))
    }
    why <- golden_compare_styles(file.path(scratch, paste0(id, "-r.xlsx")), "S", file.path(scratch, paste0(id, ".xlsx")), "S", got_width_offset = golden_r_width_offset)
    expect_identical(why, character(), info = paste(id, paste(why, collapse = "\n")))
    expect_identical(tt$stored$table_role, "raw_analytical")
    if (identical(id, "RL004")) {
      expect_identical(tt$meta$frame$effect_scale, c("TR", "TR"))
      expect_identical(tt$meta$frame$provenance, rep("fitted_interval_censored_AFT", 2L))
    } else expect_identical(tt$meta$frame$outcome_id, c("Y", "Y"))
    cat(sprintf("STATA REGTAB PLACEMENT ROW id=%s source=712044f8 version=2.5.1 text_sinks=2 workbook_style=1\n", id))
  }
  cat("STATA REGTAB PLACEMENT RECEIPT scenarios=4 source=712044f8 existing_golden_updates=0\n")
})
