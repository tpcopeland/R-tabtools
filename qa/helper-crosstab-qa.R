# Shared literal cases for the installed/native QA, no test blocks here.
xt_qa_data <- function(f, rows = seq_len(nrow(f)), columns = seq_len(ncol(f))) {
  d <- expand.grid(row = rows, column = columns)
  d$frequency <- as.vector(f)
  d
}

xt_qa_cases <- function() {
  dense <- xt_qa_data(matrix(c(40, 10, 20, 30), 2))
  sparse <- xt_qa_data(matrix(c(1, 3, 3, 1), 2))
  trend <- xt_qa_data(rbind(c(10, 20, 30), c(30, 20, 10)), columns = 0:2)
  protect <- xt_qa_data(matrix(c(1, 9, 9, 1), 2))
  case <- function(data, options = list(), error = NULL, native_boundary = FALSE) {
    list(data = data, options = options, error = error, native_boundary = native_boundary)
  }
  frac <- dense; frac$row <- (frac$row - 1) / 10; frac$column <- (frac$column - 1) / 10
  translated <- trend; translated$column <- 1e16 + 2 * translated$column
  unequal <- trend; unequal$column[unequal$column == 2] <- 5
  if (!requireNamespace("haven", quietly = TRUE)) stop("The tagged-missing native case requires haven.")
  missing <- rbind(dense, data.frame(row = c(NA_real_, haven::tagged_na("a")), column = 1, frequency = c(7, 3)))
  zero <- rbind(dense, data.frame(row = 999, column = 999, frequency = 0))
  list(
    XT001 = case(dense, list(or = TRUE, rr = TRUE, rd = TRUE)),
    XT002 = case(transform(dense, column = 3 - column), list(rowpct = TRUE, or = TRUE, rr = TRUE, rd = TRUE)),
    XT003 = case(frac, list(label = TRUE, totalpct = TRUE, or = TRUE)),
    XT004 = case(sparse, list(or = TRUE)),
    XT005 = case(xt_qa_data(matrix(c(1, 2, 1, 2, 1, 2), 2))),
    XT006 = case(xt_qa_data(matrix(5, 2, 2))),
    XT007 = case(xt_qa_data(matrix(c(1250, 386, 4, 4), 2)), list(or = TRUE, level = 90)),
    XT008 = case(dense, list(rr = TRUE, rd = TRUE, level = 90)),
    XT009 = case(xt_qa_data(diag(c(2, 2))), list(or = TRUE), "tabtools_error_crosstab_association"),
    XT010 = case(trend, list(cochran = TRUE)),
    XT011 = case(translated, list(cochran = TRUE)),
    XT012 = case(unequal, list(cochran = TRUE)),
    XT013 = case(xt_qa_data(rbind(c(497, 560, 269), c(19, 29, 24)), columns = -1:1), list(cochran = TRUE)),
    XT014 = case(xt_qa_data(matrix(c(5, 3, 2, 7, 4, 6), 2)), list(trend = TRUE)),
    XT015 = case(xt_qa_data(diag(2)), list(trend = TRUE), "tabtools_error_crosstab_trend"),
    XT016 = case(xt_qa_data(matrix(c(0, 1, 1, 0), 2)), list(trend = TRUE), "tabtools_error_crosstab_trend"),
    XT017 = case(missing, list(missing = TRUE)),
    XT018 = case(zero),
    XT019 = case(dense, list(trend = TRUE, missing = TRUE), "tabtools_error_crosstab_trend"),
    XT020 = case(protect, list(smallcells = 3, or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE)),
    XT021 = case(protect, list(smallcells = 3, smallcells_mode = "primary", or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE)),
    XT022 = case(protect, list(smallcells = 3, rowpct = TRUE)),
    XT023 = case(xt_qa_data(matrix(c(1, 1, 0, 10), 2)), list(smallcells = 3, smallcells_mode = "primary", rowpct = TRUE)),
    XT024 = case(xt_qa_data(matrix(c(1, 0, 0, 1), 2)), list(smallcells = 3, smallcells_mode = "primary", totalpct = TRUE)),
    XT025 = case(dense),
    XT026 = case(dense),
    XT027 = case(xt_qa_data(matrix(5, 2, 2)), list(exact = TRUE, digits = 2, headershade = TRUE, zebra = TRUE)),
    XT028 = case(xt_qa_data(matrix(10, 3, 2)), list(cochran = TRUE), "tabtools_error_crosstab_trend"))
}


# Reader contracts are independent of production arithmetic and formatting.
xt_qa_guard <- function(ok, message) {
  if (!isTRUE(ok)) stop(structure(list(message = message, call = NULL),
    class = c("crosstab_native_contract_error", "error", "condition")))
  invisible(TRUE)
}

xt_qa_number <- function(value) {
  value <- trimws(as.character(value))
  missing <- value == "." | grepl("^\\.[a-z]$", value)
  out <- suppressWarnings(as.numeric(value))
  xt_qa_guard(all(missing | is.finite(out)), "Invalid native numeric value")
  out[missing] <- NA_real_
  out
}

xt_qa_matrix <- function(records, rows, columns) {
  xt_qa_guard(is.character(rows) && is.character(columns) && !anyNA(rows) &&
    !anyNA(columns) && !anyDuplicated(rows) && !anyDuplicated(columns), "Invalid expected axes")
  xt_qa_guard(identical(unique(records$row), rows) && identical(unique(records$col), columns),
    "Native axis order or missing-tag identity differs")
  expected <- expand.grid(row = rows, col = columns, stringsAsFactors = FALSE)
  key <- function(x) paste(x$row, x$col, sep = "\r")
  xt_qa_guard(nrow(records) == nrow(expected) && !anyDuplicated(key(records)) &&
    setequal(key(records), key(expected)), "Native matrix coordinates are incomplete or duplicated")
  matrix(xt_qa_number(records$value[match(key(expected), key(records))]), length(rows), length(columns))
}

xt_qa_scalar_keys <- function(options) {
  c("markdown_cols", "markdown_rows", "ci_level", "N", "p", "chi2",
    if (isTRUE(options$or)) "or", if (isTRUE(options$rr)) "rr", if (isTRUE(options$rd)) "rd",
    if (isTRUE(options$trend) || isTRUE(options$cochran)) "p_trend",
    if (isTRUE(options$cochran)) c("chi2_trend", "z_trend"),
    if (!is.null(options$smallcells)) c("smallcells", "N_primary_suppressed",
      "N_secondary_suppressed", "N_derived_suppressed"))
}

xt_qa_unique_keys <- function(records, expected) {
  xt_qa_guard(nrow(records) == length(expected) && !anyDuplicated(records$name) &&
    setequal(records$name, expected), "Native scalar/association family is incomplete or duplicated")
  records[match(expected, records$name), , drop = FALSE]
}

xt_qa_axes <- function(case, id) {
  if (id == "XT017") return(list(row = c("1", "2", ".", ".a"), column = c("1", "2")))
  if (id == "XT003") return(list(row = c("0", ".1"), column = c("0", ".1")))
  if (id == "XT011") return(list(row = c("1", "2"),
    column = c("10000000000000000", "10000000000000002", "10000000000000004")))
  # Finite literal numeric input identities, before any production formatting.
  used <- !is.na(case$data$frequency) & case$data$frequency > 0
  list(row = as.character(sort(unique(case$data$row[used]))),
       column = as.character(sort(unique(case$data$column[used]))))
}

xt_qa_xt018_literal <- list(rc = 0L, dimensions = c(2L, 2L), N = 100,
  row = c("1", "2"), column = c("1", "2"), table = matrix(c(40, 10, 20, 30), 2L))
# Transcribed from genuine native-xt-v2 XT018_status.csv / XT018_stored.csv;
# the frozen bundle hashes authenticate the held source, not an R-generated oracle.
xt_qa_native_manifest_sha256 <- "7d1c33158c762e1df39d0a43421ae52285387c556b48055cb04dc0c7244a32ee"
xt_qa_native_pin <- "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129"

xt_qa_sha256 <- function(path) {
  executable <- Sys.which("sha256sum")
  xt_qa_guard(nzchar(executable), "Native QA requires sha256sum")
  result <- system2(executable, shQuote(path), stdout = TRUE, stderr = TRUE)
  xt_qa_guard(is.null(attr(result, "status")) && length(result) == 1L, "SHA256 command failed")
  sub(" .*", "", result)
}

xt_qa_authenticate <- function(native) {
  xt_qa_guard(requireNamespace("jsonlite", quietly = TRUE), "Native QA requires jsonlite")
  path <- file.path(native, "artifacts.json")
  xt_qa_guard(identical(xt_qa_sha256(path), xt_qa_native_manifest_sha256), "Unapproved native artifact manifest")
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  xt_qa_guard(identical(manifest$status, "NATIVE_CAPTURE_COMPLETE") &&
    identical(manifest$source_pin, xt_qa_native_pin), "Native source pin/status differs")
  files <- names(manifest$files)
  xt_qa_guard(length(files) == 162L && !anyDuplicated(files) &&
    all(basename(files) == files), "Native artifact inventory differs")
  for (name in files) xt_qa_guard(identical(xt_qa_sha256(file.path(native, name)), manifest$files[[name]]),
    paste("Native artifact hash differs:", name))
  xt_qa_guard(identical(readLines(file.path(native, "runtime.txt"), warn = FALSE), c("17", "mt64")),
    "Native runtime identity differs")
  invisible(manifest)
}

xt_qa_footer_literals <- function(options) {
  if (is.null(options$smallcells)) return(list(R = character(), native = character()))
  xt_qa_guard(identical(options$smallcells, 3), "Native footer fixture threshold differs")
  if (identical(options$smallcells_mode, "primary")) return(list(
    R = paste0("Counts from 1 to 2 are shown as <3. Dependent percentages are withheld. ",
      "Primary protection masks no complementary counts; released totals and computed tests or association estimates may permit reconstruction."),
    native = paste0("Counts from 1 to 2 are shown as <3 without a percentage (primary suppression only: ",
      "no complementary cells are masked, and totals and tests are shown as computed). This protects printed counts only.")))
  list(R = paste0("Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction. ",
      "Percentages are withheld when their count or selected denominator is suppressed. ",
      "Tests and association estimates are withheld when primary counts are protected."),
    native = "Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction.")
}

xt_qa_footer_tail <- function(lines, expected) {
  xt_qa_guard(identical(unname(lines), expected), "Complete native/R footer differs")
  invisible(TRUE)
}

xt_qa_native_listing <- function(path) {
  lines <- readLines(path, warn = FALSE)
  edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
  xt_qa_guard(length(edge) == 2L, "Native console must contain exactly one complete table")
  export <- which(startsWith(lines, "CSV exported to "))
  xt_qa_guard(length(export) == 1L && export > edge[2L], "Native console export boundary differs")
  lines[seq.int(edge[1L], export - 1L)]
}

xt_qa_golden_helpers <- function(native) {
  # test_file() loads this sibling helper in qa/. Root supplies the reviewed
  # 3G integrity helpers in the installed source archive before execution.
  root <- normalizePath(testthat::test_path(".."), mustWork = TRUE)
  helper <- new.env(parent = globalenv())
  for (name in c("helper-golden.R", "helper-footnote-parity.R", "helper-golden-footnotes.R")) {
    sys.source(file.path(root, "tests", "testthat", name), helper)
  }
  helper$golden_artifact_path <- function(id, file) file.path(native, file)
  helper$golden_book <- function(id) file.path(native, paste0(id, ".xlsx"))
  helper$golden_scenario <- function(id) list(command = "crosstab", mask = character())
  helper$golden_publication_contract <- function(id) {
    foot <- xt_qa_footer_literals(xt_qa_cases()[[id]]$options)
    grid <- helper$golden_read_cells_file(file.path(native, paste0(id, ".csv")))
    styles <- helper$golden_cell_styles(helper$golden_book(id), id)
    list(paragraphs = foot$R, native_footers = list(csv = foot$native,
      markdown = helper$golden_fn_md(foot$native), console = foot$native, xlsx = foot$native),
      native_grid = grid, native_styles = styles,
      native_layout = helper$golden_sheet_layout(helper$golden_book(id), id),
      grid_end = nrow(grid) - length(foot$native), sheet_end = max(styles$row) - length(foot$native))
  }
  helper
}
