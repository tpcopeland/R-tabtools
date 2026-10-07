library(testthat)
library(tabtools)

layout_current_proof <- function() {
  root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
  if (!nzchar(root)) root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
  base <- Sys.getenv("TABTOOLS_PUTTAB_GEOMETRY256_DIR",
    unset = file.path(root, "qa", "data", "post251_puttab_geometry_v256"))
  base <- normalizePath(base, mustWork = TRUE)
  expected <- c(
    "capture.do" = "f63d53d886f9654fe2124de36116980d314597429994b4102b73fa0b397b46b3",
    "capture.log" = "c43ccda524f9c52063f55bcaff7bca859c4883cf43a9a1e82c0e1bffe198d8cb",
    "cleanup.json" = "9ca7aa74d157dd3cb6432d785f4bba71807ac006d5677e3cfab9bb6c5ab70e72",
    "native-receipt.json" = "5a778df972243959d29e77f29124f637d3843033d2ee4b561d8ca861d586cd74",
    "out/academic.csv" = "accacea99acd4743aacee45e902ffd8561e37fd8fba6b84c641e98be5c0c3120",
    "out/academic.md" = "ce1cc582c2fd2fcea7f06b1120100644cbeb4e2974743414f9b5718d77fdb5d8",
    "out/academic.xlsx" = "5441ef3a2c8e2d5d4b6a0584dfb927230088b39e27e87d09483c98830252ac60",
    "out/counts.csv" = "e4bc884e1fa399aacbb79535269834017cfdb0a3b19e06d8e8ca88dbca5bed6e",
    "out/frames.csv" = "cca24713dfd96b4690c60fad22213b26291ea60b00e6f3b388ff4bf4bf05f124",
    "out/frames.md" = "b6e92f285d24a0f01e7e8509264099d7e15bb572fe34644bab1134384fbed77a",
    "out/frames.xlsx" = "b0ca4b2a65a651a3e2765464fe5b231f8d80451ac347b783660d535aab61ce57",
    "out/medium.csv" = "accacea99acd4743aacee45e902ffd8561e37fd8fba6b84c641e98be5c0c3120",
    "out/medium.md" = "ce1cc582c2fd2fcea7f06b1120100644cbeb4e2974743414f9b5718d77fdb5d8",
    "out/medium.xlsx" = "3413badd046f9dc70fe8348896a13388f66dc5fb2cf38068d79a50cf7ed49930",
    "out/panel.csv" = "f88c8e0ddbf45dffa2a12ca7885f7f97291db80091e78b1c95cb0688311b832c",
    "out/panel.md" = "b64903b9e56370cf74812a09d5c68810c579bdfd2f45968ddd507008c2f6c384",
    "out/panel.xlsx" = "5c72dd56fe3a1f6305d0bf9807f050108a75322fbde48db77531e75bd8e773d5",
    "out/thin.csv" = "accacea99acd4743aacee45e902ffd8561e37fd8fba6b84c641e98be5c0c3120",
    "out/thin.md" = "ce1cc582c2fd2fcea7f06b1120100644cbeb4e2974743414f9b5718d77fdb5d8",
    "out/thin.xlsx" = "18b2e4b33f2aa096643a61c17b6ea4144ce173b7cde7da0da897f2e7ea56ac7b",
    "process.log" = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    "source-provenance.json" = "9e8d70e642db5592845cf5561c8c470fdd72425cb5ef254f2ecb268c2a90a01b"
  )
  paths <- list.files(base, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!identical(sort(paths), sort(names(expected)))) stop("Five-case native geometry proof inventory differs")
  for (name in names(expected)) if (!identical(digest::digest(file = file.path(base, name),
    algo = "sha256", serialize = FALSE), unname(expected[[name]]))) stop(paste("Native geometry proof changed:", name))
  receipt <- jsonlite::fromJSON(file.path(base, "native-receipt.json"), simplifyVector = FALSE)
  closure <- jsonlite::fromJSON(file.path(base, "source-provenance.json"), simplifyVector = FALSE)
  cleanup <- jsonlite::fromJSON(file.path(base, "cleanup.json"), simplifyVector = FALSE)
  if (!identical(receipt$source_pin, "4eecca4d09d61df0cfe773fc2024b015920b5fe6") ||
    !identical(receipt$recipe_sha256, "cf4c6a3a1d725ebc624dc8a5c39138e6b3b6ce3b87af412cf44674354f164082") ||
    !identical(receipt$source_provenance_sha256, unname(expected[["source-provenance.json"]])) ||
    !identical(closure$pin, receipt$source_pin) || !identical(closure$version, "2.5.6") ||
    length(closure$files) != 363L || !isTRUE(cleanup$absent) || cleanup$exit_status != 0L)
    stop("Five-case native source/recipe/cleanup identity differs")
  outputs <- sub("^out/", "", names(expected)[startsWith(names(expected), "out/")])
  if (!identical(sort(names(receipt$files)), sort(outputs))) stop("Native receipt output inventory differs")
  for (name in outputs) if (!identical(receipt$files[[name]], unname(expected[[paste0("out/", name)]])))
    stop("Native receipt does not bind output bytes")
  log <- readLines(file.path(base, "capture.log"), warn = FALSE)
  if (sum(trimws(log) == "PUTTAB_CURRENT_GEOMETRY_COMPLETE") != 1L ||
    any(grepl("^r\\([0-9]+\\);$", trimws(log)))) stop("Native geometry capture incomplete")
  base
}

layout_current_peer <- function(proof, id, old, old_counts,
  golden_sheet_layout, golden_cell_styles, golden_bytes, golden_style_attrs) {
  removed <- switch(id, thin = c("B3:D3", "B7:D7"), medium = c("B3:D3", "B7:D7"),
    academic = c("B3:D3", "B7:D7"), frames = c("B3:C3", "B5:C5"), panel = c("B4:E4", "B7:E7"))
  if (is.null(removed)) stop("Only the five declared geometry cases may be qualified")
  current <- file.path(proof, "out", paste0(id, ".xlsx"))
  old_layout <- golden_sheet_layout(old, "S"); new_layout <- golden_sheet_layout(current, "S")
  expected_new <- switch(id, frames = "A1:C1", panel = c("A1:E1", "B10:E10", "B9:E9", "C2:E2"), "A1:D1")
  expect_identical(old_layout$merges, sort(c(expected_new, removed)), info = paste(id, "entire literal old merge ledger"))
  expect_identical(new_layout$merges, expected_new, info = paste(id, "entire independently captured current merge ledger"))
  expect_identical(new_layout$merges, sort(setdiff(old_layout$merges, removed)),
    info = paste(id, "only two exact panel merges change; every other merge survives"))
  expect_identical(new_layout$heights, old_layout$heights, info = paste(id, "complete old/new native heights"))
  expect_identical(new_layout$widths, old_layout$widths, info = paste(id, "complete old/new native widths"))
  # Workbook-local style-pool indices are serialization identities; compare
  # their complete resolved number format, font, alignment, fill and borders.
  attrs <- c("address", "row", "col", "number_format", golden_style_attrs)
  expect_identical(golden_cell_styles(current, "S")[attrs], golden_cell_styles(old, "S")[attrs],
    info = paste(id, "all old/new native cells and styles, including newly unmerged children"))
  counts <- utils::read.csv(file.path(proof, "out", "counts.csv"), strip.white = TRUE)
  expect_identical(counts$id, c("thin", "medium", "academic", "frames", "panel"))
  row <- counts[counts$id == id, , drop = FALSE]
  expect_identical(nrow(old_counts), 1L)
  for (field in names(old_counts)) expect_identical(as.character(row[[field]]), as.character(old_counts[[field]]),
    info = paste(id, "entire native return ledger", field))
  heads <- switch(id, frames = c(B3 = "Panel A", B5 = "Panel A"), panel = c(B4 = "var", B7 = "stat"),
    c(B3 = "Panel A", B7 = "Panel B"))
  cells <- golden_cell_styles(current, "S")
  expect_identical(cells$value[match(names(heads), cells$address)], unname(heads),
    info = paste(id, "independent literal native panel text"))
  for (ext in c("csv", "md")) expect_identical(golden_bytes(file.path(proof, "out", paste0(id, ".", ext))),
    golden_bytes(sub("\\.xlsx$", paste0(".", ext), old)), info = paste(id, "entire old/current", ext, "bytes"))
  current
}


# Independent native Stata 2.5.1 artifacts, with only five selected panel
# merge geometries qualified by exact authenticated 2.5.6 publication peers. Runtime workbooks,
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

test_that("native Stata 2.5.1 puttab and frames retain all cells/styles/sinks with exact 2.5.6 panel geometry", {
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
  current_geometry <- layout_current_proof()
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
    if (id %in% c("thin", "medium", "academic", "frames"))
      native <- layout_current_peer(current_geometry, id, native, counts[counts$id == id, , drop = FALSE],
        golden_sheet_layout, golden_cell_styles, golden_bytes, golden_style_attrs)
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
