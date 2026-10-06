library(testthat)
library(tabtools)

# Independent native Stata 2.5.1 artifact comparisons. Runtime workbooks,
# CSV/Markdown, do-files and licence-bearing logs stay inside unique scratch.
# TABTOOLS_STATA_DIR selects the pinned export; installed ado files are not used.

layout_stata_run <- function(scratch, source_dir) {
  if (grepl('["\r\n`]', source_dir)) stop("TABTOOLS_STATA_DIR contains a Stata quoting character", call. = FALSE)
  setup <- c("version 17.0", "clear all", "set more off", sprintf('adopath ++ "%s"', source_dir),
             "quietly findfile puttab.ado", sprintf('assert r(fn) == "%s/puttab.ado"', source_dir),
             'file open counts using "counts.csv", write text replace',
             'file write counts "id,n_datarows,n_panels,n_spans" _n')
  data <- c("clear", "set obs 3", "gen p = cond(_n < 3, 1, 2)",
            'label define panels 1 "Panel A" 2 "Panel B"', "label values p panels",
            'gen str10 term = cond(_n == 1, "a", cond(_n == 2, "b", "c"))',
            "gen n = 1000 * _n", "gen rate = 1234.5 + _n / 10", "format n %8.0gc", "format rate %12.2fc",
            'label variable term "Term"', 'label variable n "N"', 'label variable rate "Rate"',
            'gen str1 h1 = ""', 'gen str12 h2 = cond(p == 1, "Counts A", "Counts B")', 'gen str12 h3 = "Rates"')
  options <- list(
    thin = 'panel(p) panelheader(h1 h2 h3) headershade zebra headercolor(blue) zebracolor(pink) hlines(3) vlines(2) boldrows(3) nformat(%6.0fc)',
    medium = 'borderstyle(medium) panel(p) panelheader(h1 h2 h3) headershade zebra headercolor(blue) zebracolor(pink) hlines(3) vlines(2) boldrows(3) nformat(%6.0fc)',
    academic = 'borderstyle(academic) panel(p) panelheader(h1 h2 h3) headershade zebra headercolor(blue) zebracolor(pink) hlines(3) vlines(2) boldrows(3) nformat(%6.0fc)',
    inline = 'panel(p) panelheader("" "N" "Rate") panelinline noheader headershade zebra headercolor(blue) zebracolor(pink) noindent nformat(%6.0fc)',
    span = 'spanheader("Measures" 2/3) headershade nformat(%6.0fc)')
  body <- setup
  record <- function(id) sprintf('file write counts "%s," %%12.0f (r(n_datarows)) "," %%12.0f (r(n_panels)) "," %%12.0f (r(n_spans)) _n', id)
  for (id in names(options)) {
    body <- c(body, data, sprintf('quietly puttab term n rate using "%s.xlsx", sheet("S") varlabels csv("%s.csv") markdown("%s.md") %s',
                                  id, id, id, options[[id]]), record(id))
  }
  body <- c(body, "clear", "set obs 2", 'gen str10 term = cond(_n == 1, "Term", "a")',
            'gen str10 n = cond(_n == 1, "N", "1")', 'label variable term "Term"', 'label variable n "N"',
            "frame copy default first", "replace term = \"b\" in 2", "frame copy default second",
            "replace term = \"c\" in 2", "frame copy default third",
            'quietly stacktab using "frames.xlsx", frames(first "Panel A" \\ second "Panel A" \\ third) sheet("S") csv("frames.csv") markdown("frames.md")',
            record("frames"), "assert r(n_frames) == 3",
            'mata: frame_spec = fopen("frame-spec.txt", "w")',
            'mata: fput(frame_spec, st_global("r(frames)"))',
            'mata: fclose(frame_spec)',
            "file close counts", 'display "LAYOUT_PARITY_COMPLETE"')
  writeLines(body, file.path(scratch, "layout.do"))
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "layout.do"), wd = scratch,
                       error_on_status = FALSE, timeout = 120000)
  logs <- list.files(scratch, pattern = "\\.log$", full.names = TRUE)
  if (length(logs) != 1L) stop("Stata layout oracle did not produce exactly one log", call. = FALSE)
  log <- readLines(logs, warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  if (proc$status != 0L || length(errors) || !any(trimws(log) == "LAYOUT_PARITY_COMPLETE") ||
      !any(grepl("end of do-file", log, fixed = TRUE))) {
    stop("Stata layout oracle failed: ", paste(errors, collapse = ", "), call. = FALSE)
  }
  counts <- utils::read.csv(file.path(scratch, "counts.csv"), strip.white = TRUE)
  attr(counts, "native_frames") <- readLines(file.path(scratch, "frame-spec.txt"), warn = FALSE)
  counts
}

test_that("native Stata 2.5.1 puttab and frames agree in cells, styles, layouts and text sinks", {
  skip_if_not_installed("processx")
  skip_if_not_installed("tidyxl")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to the pinned Stata 2.5.1 export")
  # Reuse the committed artifact comparator, with no masks or golden updates.
  # Its pure readers parse actual workbook XML and tidyxl cell formats.
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  scratch <- withr::local_tempdir(pattern = "tabtools-layout-stata-")
  counts <- layout_stata_run(scratch, source_dir)
  expect_identical(attr(counts, "native_frames"), 'first "Panel A" \\ second "Panel A" \\ third')
  d <- data.frame(p = c(1, 1, 2), term = c("a", "b", "c"), n = c(1000, 2000, 3000), rate = c(1234.6, 1234.7, 1234.8),
                  h1 = "", h2 = c("Counts A", "Counts A", "Counts B"), h3 = "Rates")
  attr(d$p, "labels") <- c("Panel A" = 1, "Panel B" = 2)
  attr(d$term, "label") <- "Term"
  attr(d$n, "label") <- "N"
  attr(d$rate, "label") <- "Rate"
  attr(d$n, "format.stata") <- "%8.0gc"
  attr(d$rate, "format.stata") <- "%12.2fc"
  results <- list()
  for (id in c("thin", "medium", "academic", "inline", "span")) {
    args <- list(x = d, vars = c("term", "n", "rate"), varlabels = TRUE, nformat = "%6.0fc",
                 xlsx = file.path(scratch, paste0(id, "-r.xlsx")), sheet = "S",
                 csv = file.path(scratch, paste0(id, "-r.csv")), markdown = file.path(scratch, paste0(id, "-r.md")))
    if (id == "span") {
      args$spanheader <- '"Measures" 2/3'
      args$headershade <- TRUE
    } else {
      args <- c(args, list(panel = "p", headershade = TRUE, zebra = TRUE, headercolor = "blue", zebracolor = "pink"))
      if (id == "inline") {
        args <- c(args, list(panelheader = list(text = c("", "N", "Rate")), panelinline = TRUE, noheader = TRUE, noindent = TRUE))
      } else {
        args <- c(args, list(panelheader = c("h1", "h2", "h3"), hlines = 3, vlines = 2, boldrows = 3, borderstyle = id))
      }
    }
    results[[id]] <- suppressMessages(do.call(puttab, args))
  }
  frames <- lapply(c("a", "b", "c"), function(value) {
    d <- data.frame(term = c("Term", value), n = c("N", "1"))
    attr(d$term, "label") <- "Term"
    attr(d$n, "label") <- "N"
    d
  })
  results$frames <- suppressMessages(stacktab(frames = list(first = list(data = frames[[1]], label = "Panel A"),
                                                           second = list(data = frames[[2]], label = "Panel A"), third = frames[[3]]),
                                              xlsx = file.path(scratch, "frames-r.xlsx"), sheet = "S",
                                              csv = file.path(scratch, "frames-r.csv"), markdown = file.path(scratch, "frames-r.md")))
  for (id in names(results)) {
    tt <- results[[id]]
    native <- file.path(scratch, paste0(id, ".xlsx"))
    own <- file.path(scratch, paste0(id, "-r.xlsx"))
    why <- golden_compare_styles(own, "S", native, "S", got_width_offset = golden_r_width_offset)
    expect_identical(why, character(), info = paste(id, paste(why, collapse = "\n")))
    for (extension in c("csv", "md")) {
      a <- file.path(scratch, paste0(id, ".", extension))
      b <- file.path(scratch, paste0(id, "-r.", extension))
      expect_identical(readBin(b, "raw", n = file.info(b)$size), readBin(a, "raw", n = file.info(a)$size), info = paste(id, extension))
    }
    row <- counts[counts$id == id, ]
    expect_identical(nrow(row), 1L)
    expect_identical(as.integer(row$n_datarows), tt$stored$n_datarows, info = id)
    expect_identical(as.integer(row$n_panels), tt$stored$n_panels, info = id)
    expect_identical(as.integer(row$n_spans), tt$stored$n_spans, info = id)
  }
  cat(sprintf("STATA LAYOUT RECEIPT scenarios=%d text_sinks=%d workbook_style_comparisons=%d count_comparisons=%d\n",
              length(results), 2L * length(results), length(results), 3L * length(results)))
})
