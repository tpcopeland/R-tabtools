library(tabtools)
source("qa/tools/qa_result.R", local = TRUE)

# Native producer execution/authentication is root-owned, after source review.
# This lane consumes only its independently accepted, hash-bound output.
qa_source_root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
if (!nzchar(qa_source_root)) qa_source_root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
qa_source_root <- normalizePath(qa_source_root, mustWork = TRUE)
if (!file.exists(file.path(qa_source_root,"DESCRIPTION"))) stop("Cannot locate the staged QA package root")
directory <- Sys.getenv("TABTOOLS_OUTTAB_NATIVE_DIR", unset = file.path(qa_source_root,"qa","data","native251","outtab"))
if (!nzchar(directory) || !dir.exists(directory)) {
  stop("Set TABTOOLS_OUTTAB_NATIVE_DIR to independently authenticated OT001--OT009 output.")
}
inventory <- utils::read.csv(file.path(directory, "ARTIFACTS.csv"), colClasses = "character")
qa_check("all accepted native OT artifact bytes", identical(unname(tools::md5sum(file.path(directory, inventory$file))), inventory$md5))
source_info <- utils::read.csv(file.path(directory, "SOURCE.csv"), colClasses = "character")
info <- stats::setNames(source_info$value, source_info$key)
qa_check("full native pin", identical(unname(info["native_source_commit"]), "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129"))
qa_check("native runtime", identical(readLines(file.path(directory, "runtime.txt")), c("17", "mt64")))
d <- data.frame(exposed = c(rep(1, 10), rep(0, 20)),
  event = c(rep(1, 4), rep(0, 6), rep(1, 2), rep(0, 18)),
  z = (1:30) %% 2, p = 1, q = (1:30) %% 2, obs_event = 1, id = 1:30)
d$z[c(1L, 11L)] <- NA_real_; d$obs_event[3:5] <- c(0, 2, NA)
specifications <- list(OT001 = list(), OT002 = list(estimator = "logit"),
  OT003 = list(models = list(~z)), OT004 = list(models = list(~z), smallcells = 3L),
  OT005 = list(minevents = 5L), OT006 = list(panels = c("p", "q"), obsprefix = "obs_"),
  OT007 = list(), OT008 = list(sep = paste(rep("long literal separator", 4), collapse = " ")),
  OT009 = list(vce = "cluster", cluster = "id"))
results <- list()
scratch <- tempfile("tabtools-outtab-qa-")
if (!dir.create(scratch)) stop("Cannot create owned outtab QA scratch.")
tryCatch({
helper <- new.env(parent = globalenv())
sys.source("tests/testthat/helper-golden.R", envir = helper)
for (id in names(specifications)) {
  input <- d
  if (id == "OT007") input$event[input$exposed == 0] <- 0
  actual_book <- file.path(scratch, "outtab.xlsx")
  actual_csv <- file.path(scratch, paste0(id, ".csv"))
  actual_md <- file.path(scratch, paste0(id, ".md"))
  actual <- do.call(outtab, c(list(data = input, outcomes = "event", exposure = "exposed",
    xlsx = actual_book, sheet = id, csv = actual_csv, markdown = actual_md), specifications[[id]]))
  native <- utils::read.csv(file.path(directory, paste0(id, "_stored.csv")), colClasses = "character", na.strings = character())
  # Native raw matrices have literal default row stripes; never invent labels.
  extract <- function(name, rows, columns) {
    tab <- native[native$name == name & native$kind == "matrix", , drop = FALSE]
    row_stripes <- unique(tab$row); col_stripes <- unique(tab$col)
    qa_check(paste(id, name, "complete dimensions/unique cell stripes"),
      length(row_stripes) == rows && length(col_stripes) == length(columns) &&
      nrow(tab) == rows * length(columns) && !anyDuplicated(paste(tab$row, tab$col)))
    qa_check(paste(id, name, "literal default row stripes"), identical(row_stripes, paste0("r", seq_len(rows))))
    qa_check(paste(id, name, "exact native columns"), identical(col_stripes, columns))
    out <- matrix(NA_real_, rows, length(columns))
    values <- rep(NA_real_, nrow(tab))
    present <- tab$value != "."
    values[present] <- as.numeric(tab$value[present])
    out[cbind(match(tab$row, row_stripes), match(tab$col, col_stripes))] <- values
    out
  }
  raw <- extract("table", nrow(actual$stored$table), colnames(actual$stored$table))
  fit_columns <- c("row", "model", "N", "N_cc", "N_clust", "df_m", "converged", "rc")
  fits <- extract("fits", nrow(actual$stored$fits), fit_columns)
  qa_check(paste(id, "raw counts/ratio/CI/codes"), isTRUE(all.equal(unname(actual$stored$table), raw, tolerance = 1e-7)))
  qa_check(paste(id, "all native fit/sample diagnostics"), isTRUE(all.equal(as.matrix(actual$stored$fits[, c("row", "model", "N", "N_cc", "N_clust", "df_m", "converged", "rc")]), fits, check.attributes = FALSE, tolerance = 1e-7)))
  cells <- function(path) as.matrix(utils::read.csv(path, header = FALSE,
    colClasses = "character", na.strings = character(), check.names = FALSE))
  qa_check(paste(id, "complete printed CSV including primary masks"),
    identical(unname(cells(actual_csv)), unname(cells(file.path(directory, paste0(id, ".csv"))))))
  qa_check(paste(id, "complete Markdown bytes"),
    identical(readBin(actual_md, "raw", n = file.info(actual_md)$size),
      readBin(file.path(directory, paste0(id, ".md")), "raw", n = file.info(file.path(directory, paste0(id, ".md")))$size)))
  styles <- helper$golden_compare_styles(actual_book, id, file.path(directory, "outtab.xlsx"), id,
    mask = "", got_width_offset = helper$golden_r_width_offset)
  qa_check(paste(id, "all workbook cells/styles/merges/widths/heights"), length(styles) == 0L)
  if (id == "OT006") {
    native_styles <- helper$golden_cell_styles(file.path(directory, "outtab.xlsx"), id)
    # Actual pinned native outtab/puttab offset: sheet heading rows 3/5 are
    # plain and the immediately following outcome rows 4/6 are bold.
    qa_check("OT006 literal native heading/outcome bold coordinates",
      identical(native_styles$bold[match(c("B3", "B4", "B5", "B6"), native_styles$address)],
        c(FALSE, TRUE, FALSE, TRUE)))
    qa_check("OT006 literal public body-relative bold-row coordinates",
      identical(actual$meta$puttab_rules$boldrows, c(2L, 4L)))
  }
  native_console <- readLines(file.path(directory, paste0(id, "_console.txt")), warn = FALSE)
  boundaries <- which(grepl("^  \\+-+\\+$", native_console))
  # The capture contains the preview followed by two native sink messages.
  # Assert the whole declared tail before selecting the complete literal box.
  tail <- if (length(boundaries) == 2L) native_console[seq.int(boundaries[2L] + 1L, length(native_console))] else character()
  # Native status paths are frozen returns, even after fixture relocation.
  native_xlsx <- native[native$name == "xlsx", , drop = FALSE]
  qa_check(paste(id,"unique authentic workbook destination"),
    nrow(native_xlsx)==1L && identical(native_xlsx$kind,"macro") &&
      identical(basename(native_xlsx$value),"outtab.xlsx"))
  native_capture_directory <- dirname(native_xlsx$value)
  expected_tail <- c(paste("Markdown exported to", file.path(native_capture_directory, paste0(id, ".md"))),
    sprintf("puttab: wrote %d data rows x %d cols (frame source) to sheet %s in %s",
      nrow(actual$body), ncol(actual$body), id, native_xlsx$value))
  qa_check(paste(id, "exact native preview/sink-message boundary"),
    length(boundaries) == 2L && boundaries[1L] == 1L && identical(tail, expected_tail))
  native_box <- if (length(boundaries) == 2L) native_console[seq_len(boundaries[2L])] else native_console
  console <- helper$golden_compare_console(capture.output(print(actual)), native_box, mask = "")
  qa_check(paste(id, "complete native console box/wide cells"), length(console) == 0L)
  mutant_box <- native_box
  if (length(mutant_box) >= 2L) mutant_box[2L] <- paste0(mutant_box[2L], " literal mutant")
  qa_check(paste(id, "console cell mutation is detected without masking"),
    length(helper$golden_compare_console(mutant_box, native_box, mask = "")) > 0L)
  results[[id]] <- list(table = actual, native_table = raw, native_fits = fits,
    style_differences = styles, console_differences = console,
    native_console = native_console, expected_native_sink_tail = expected_tail)
}
}, finally = unlink(scratch, recursive = TRUE))
# Durable complete results precede generic qa_done(), which exits on failure.
destination <- Sys.getenv("TABTOOLS_OUTTAB_QA_RESULTS")
if (nzchar(destination)) saveRDS(list(results = results, executed = qa_state$executed,
  failed = qa_state$failed, skipped = qa_state$skipped), destination, version = 3)
qa_done("crossval_outtab.R")
