library(tabtools)
source("qa/tools/qa_result.R")
receipt <- new.env(parent = emptyenv())
receipt$results <- list()
work <- function() {
  qa_source_root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
  if (!nzchar(qa_source_root)) qa_source_root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
  qa_source_root <- normalizePath(qa_source_root, mustWork = TRUE)
  if (!file.exists(file.path(qa_source_root,"DESCRIPTION"))) stop("Cannot locate the staged QA package root")
  directory <- Sys.getenv("TABTOOLS_SURVTAB_NATIVE_DIR", unset = file.path(qa_source_root,"qa","data","native251","survtab"))
  if (!nzchar(directory) || !dir.exists(directory)) stop("Set TABTOOLS_SURVTAB_NATIVE_DIR to independently authenticated SV001--SV012 output.")
  inventory <- utils::read.csv(file.path(directory, "ARTIFACTS.csv"), colClasses = "character")
  qa_check("complete accepted native artifact bytes", identical(unname(tools::md5sum(file.path(directory, inventory$file))), inventory$md5))
  source <- utils::read.csv(file.path(directory, "SOURCE.csv"), colClasses = "character")
  info <- stats::setNames(source$value, source$key)
  qa_check("full source pin", identical(unname(info["native_source_commit"]), "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129"))
  qa_check("actual runtime", identical(readLines(file.path(directory, "runtime.txt")), c("17", "mt64")))
  scratch <- tempfile("wp6c-native-crossval-")
  if (!dir.create(scratch)) stop("Cannot create owned survival scratch.")
  on.exit(try(unlink(scratch, recursive = TRUE), silent = TRUE), add = TRUE)
  helper <- new.env(parent = globalenv())
  for (path in c("helper-golden.R", "helper-footnote-parity.R", "helper-golden-footnotes.R")) {
    sys.source(file.path("tests", "testthat", path), envir = helper)
  }
  d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), group = c(1, 1, 2, 2), id = 1:4)
  specifications <- list(
    SV001 = list(data = d, times = c(1, 2), by = "group", median = TRUE, events = TRUE, riskset = TRUE, rmst = 2, difference = TRUE, level = 90),
    SV002 = list(data = data.frame(exit = c(1, 2, 3, 4, 2), event = c(0, NA, 0, 0, 1), group = c(1, 1, 2, 2, NA)),
      times = c(1, 2), by = "group", median = TRUE, events = TRUE, riskset = TRUE, rmst = 2),
    SV003 = list(data = data.frame(id = 1:3, entry = c(0, 2, 0), exit = c(2, 3, 4), event = c(1, 1, 0)),
      times = c(2, 3), entry = "entry", id = "id", events = TRUE, riskset = TRUE, rmst = 3),
    SV004 = list(data = data.frame(id = c(1, 2, 3, 4, 4), entry = c(0, 0, 0, 0, 2),
      exit = c(1, 2, 3, 2, 4), event = c(1, 1, 0, 0, 0)), times = c(1, 2, 3), entry = "entry", id = "id", median = TRUE, events = TRUE, riskset = TRUE, rmst = 3),
    SV005 = list(data = data.frame(exit = 1:4, event = c(1, 1, 0, 0), frequency = c(10, 1, 10, 1)),
      times = c(1, 2, 3), fweight = "frequency", median = TRUE, events = TRUE, riskset = TRUE, rmst = 3),
    SV006 = list(data = d, times = c(1, 2, 5), by = "group", reverse = TRUE, difference = TRUE, events = TRUE, riskset = TRUE),
    SV007 = list(data = d[c("exit", "event")], times = 1:4, median = TRUE, level = 90, rmst = 3),
    SV008 = list(data = data.frame(exit = 2, event = 1), times = c(1, 2), median = TRUE, events = TRUE, riskset = TRUE, rmst = 2, level = 90),
    SV009 = list(data = data.frame(exit = c(1, 2), event = 1), times = c(1, 2), median = TRUE, events = TRUE, riskset = TRUE, rmst = 2, level = 90),
    SV010 = list(data = data.frame(exit = c(2, 2), event = 1), times = c(1, 2), median = TRUE, events = TRUE, riskset = TRUE, rmst = 2, level = 90),
    SV011 = list(data = data.frame(exit = c(0.3, 0.1 + 0.2, 1), event = c(1, 1, 0)), times = c(0.3, 0.5), events = TRUE, riskset = TRUE, rmst = 0.5),
    SV012 = list(data = data.frame(exit = 1:4, event = c(1, 1, 0, 0),
      group = c(0.10000000149011612, 0.10000000149011612, 0.20000000298023224, 0.20000000298023224)),
      times = c(1, 2), by = "group", difference = TRUE, events = TRUE, riskset = TRUE, rmst = 2))
  attr(specifications$SV006$data$group, "labels") <- c("a-b" = 1, "a b" = 2)
  notes <- list(SV002 = "No failures in the analysis sample; the log-rank test is not possible and is omitted.",
    SV006 = c(paste0("reverse reports 1 - Kaplan-Meier, which equals cumulative incidence only with a single event type ",
      "(no competing risks). With competing events, use a competing-risks estimator (Aalen-Johansen)."),
      "Times beyond the last observed follow-up repeat the final Kaplan-Meier estimate and are not supported by the data: a-b: 5 (last follow-up 2); a b: 5 (last follow-up 4)."))
  native_notes <- list(SV006 = c(
    "Note: reverse reports 1 - Kaplan-Meier, which equals the cumulative",
    "      incidence only with a single event type (no competing risks). With",
    "      competing events, 1 - KM overestimates absolute risk; use a",
    "      competing-risks estimator instead (Aalen-Johansen: stcompet, stcrreg,",
    "      or the finegray package).",
    "Note: times beyond the last observed follow-up of a group repeat the final",
    "      Kaplan-Meier estimate and are not supported by the data:",
    "      a-b: 5 (last follow-up 2); a b: 5 (last follow-up 4)"))
  read_cells <- function(path) as.matrix(utils::read.csv(path, header = FALSE,
    colClasses = "character", na.strings = character(), check.names = FALSE))
  for (id in names(specifications)) {
    args <- specifications[[id]]
    book <- file.path(scratch, "survtab.xlsx")
    csv <- file.path(scratch, paste0(id, ".csv")); md <- file.path(scratch, paste0(id, ".md"))
    actual <- do.call(survtab, c(args, list(time = "exit", event = "event", footnote = "Native annotation.",
      xlsx = book, sheet = id, csv = csv, markdown = md)))
    native <- utils::read.csv(file.path(directory, paste0(id, "_stored.csv")), colClasses = "character", na.strings = character())
    cells <- native[native$name == "table" & native$kind == "matrix", , drop = FALSE]
    rows <- unique(cells$row); columns <- unique(cells$col)
    complete_coordinates <- function(value) {
      nrow(value) == length(rows) * length(columns) &&
        !anyNA(value[c("row", "col")]) &&
        !anyDuplicated(value[c("row", "col")]) &&
        identical(value$row, rep(rows, each = length(columns))) &&
        identical(value$col, rep(columns, times = length(rows)))
    }
    qa_check(paste(id, "complete unique native coordinates"), complete_coordinates(cells))
    if (!complete_coordinates(cells)) stop("Incomplete native survival matrix.")
    duplicated_cell <- cells; duplicated_cell[nrow(cells), ] <- cells[1L, ]
    qa_check(paste(id, "duplicate/missing coordinate mutant refused"),
      !complete_coordinates(duplicated_cell))
    table <- matrix(NA_real_, length(rows), length(columns), dimnames = list(rows, columns))
    for (i in seq_len(nrow(cells))) table[cells$row[i], cells$col[i]] <- as.numeric(cells$value[i])
    qa_check(paste(id, "full raw probability/stripes"), isTRUE(all.equal(actual$stored$table, table, tolerance = 1e-12)))
    scalars <- native[native$kind == "scalar", , drop = FALSE]
    # Export row counts are checked with full sinks below; native scalar families
    # are never removed merely because their result is missing.
    scalars <- scalars[!scalars$name %in% c("markdown_rows", "markdown_cols"), , drop = FALSE]
    for (i in seq_len(nrow(scalars))) {
      expected <- if (scalars$value[i] == ".") NA_real_ else as.numeric(scalars$value[i])
      qa_check(paste(id, "native scalar", scalars$name[i]),
        isTRUE(all.equal(as.numeric(actual$stored[[scalars$name[i]]]), expected, tolerance = 1e-11)))
    }
    for (field in grep("^group_[0-9]+_(value|label)$|^by_var$", native$name, value = TRUE)) {
      qa_check(paste(id, "literal identity", field), identical(actual$stored[[field]], native$value[native$name == field]))
    }
    expected_native_methods <- "Survival was estimated using the Kaplan-Meier method."
    if (!is.null(args$fweight)) expected_native_methods <- paste(expected_native_methods,
      "Frequency weights from stset were treated with replication semantics for counts and Greenwood RMST variance.")
    if (isTRUE(args$reverse)) expected_native_methods <- paste(expected_native_methods,
      "Cumulative incidence is reported as 1 minus the Kaplan-Meier survival estimate, which is valid only in the absence of competing risks; with competing events a competing-risks estimator (Aalen-Johansen) should be used instead.")
    if (!is.null(args$by)) expected_native_methods <- paste(expected_native_methods,
      if (id == "SV002") "There were no failures in the analysis sample, so the groups were not compared with the log-rank test." else "Groups were compared using the log-rank test.")
    level <- if (is.null(args$level)) "95" else "90"
    if (isTRUE(args$median)) expected_native_methods <- paste(expected_native_methods,
      paste0("Median survival time with ", level, "% confidence intervals is reported."))
    if (!is.null(args$rmst)) {
      horizon <- if (id == "SV011") "0.5" else as.character(args$rmst)
      expected_native_methods <- paste(expected_native_methods, paste0("Restricted mean survival time was computed up to ",
        horizon, " years with ", level, "% confidence intervals based on the Greenwood variance formula."))
      if (isTRUE(args$difference) && !is.null(args$by)) {
        # SV012's display labels are independently bound to its authenticated
        # native identity fields above; no unmeasured float-label literal.
        labels <- if (id == "SV012") native$value[match(c("group_1_label", "group_2_label"), native$name)] else c("1", "2")
        expected_native_methods <- paste(expected_native_methods, paste0("The between-group RMST difference is reported as ",
          labels[1L], " minus ", labels[2L], " (the first minus the second by() group in ascending order of group), with a ",
          level, "% confidence interval and two-sided Wald p-value based on the independent-group variance."))
      }
    }
    expected_native_methods <- paste(expected_native_methods, "Analysis performed in Stata 17 (StataCorp, College Station, TX).")
    qa_check(paste(id, "entire native method/source prose"), identical(native$value[native$name == "methods"], expected_native_methods))
    qa_check(paste(id, "R engine/method identity"), startsWith(actual$stored$methods,
      "Survival was estimated using the Kaplan-Meier product-limit method with Greenwood variance.") &&
      !grepl("Analysis performed in Stata", actual$stored$methods, fixed = TRUE))
    if ("beyond_support" %in% native$name) qa_check(paste(id, "literal beyond-support stored record"),
      identical(actual$stored$beyond_support, native$value[native$name == "beyond_support"]))
    qa_check(paste(id, "actual returned destination identity"), identical(actual$stored$xlsx, book) &&
      identical(actual$stored$csv, csv) && identical(actual$stored$markdown, md) && identical(actual$stored$sheet, id))
    paragraphs <- c("Native annotation.", notes[[id]])
    got <- read_cells(csv); want <- read_cells(file.path(directory, paste0(id, ".csv")))
    end <- nrow(actual$body) + 1L
    qa_check(paste(id, "complete CSV dimensions and literal paragraphs"),
      nrow(got) == end + length(paragraphs) && nrow(want) == end + 1L &&
      identical(unname(got[(end + 1L):nrow(got), 1L]), paragraphs) &&
      identical(unname(want[end + 1L, 1L]), "Native annotation.") &&
      all(got[(end + 1L):nrow(got), -1L, drop = FALSE] == "") && all(want[end + 1L, -1L] == ""))
    qa_check(paste(id, "strict complete CSV body"), identical(unname(got[seq_len(end), , drop = FALSE]), unname(want[seq_len(end), , drop = FALSE])))
    got_md <- readLines(md, warn = FALSE); want_md <- readLines(file.path(directory, paste0(id, ".md")), warn = FALSE)
    ge <- max(which(startsWith(got_md, "|"))); we <- max(which(startsWith(want_md, "|")))
    r_footer <- unlist(lapply(helper$golden_fn_md(paragraphs), function(p) c("", p)), use.names = FALSE)
    qa_check(paste(id, "entire Markdown paragraph boundaries"), identical(got_md[-seq_len(ge)], r_footer) &&
      identical(want_md[-seq_len(we)], c("", "*Native annotation.*")))
    qa_check(paste(id, "strict complete Markdown body bytes"), identical(got_md[seq_len(ge)], want_md[seq_len(we)]))
    for (path in c(csv, md, file.path(directory, paste0(id, c(".csv", ".md"))))) {
      bytes <- readBin(path, "raw", n = file.info(path)$size)
      qa_check(paste(id, basename(path), "LF/final newline"), !any(bytes == as.raw(13)) && identical(tail(bytes, 1L), as.raw(10)))
    }
    native_book <- file.path(directory, "survtab.xlsx")
    native_styles <- helper$golden_cell_styles(native_book, id)
    native_layout <- helper$golden_sheet_layout(native_book, id)
    contract <- list(command = "survtab", paragraphs = paragraphs,
      native_footers = list(csv = "Native annotation.", markdown = "*Native annotation.*", console = character(), xlsx = "Native annotation."),
      native_styles = native_styles, native_layout = native_layout, sheet_end = max(native_styles$row) - 1L)
    # Owned QA environment only. The existing immutable footer adapter compares
    # every remaining cell/style/geometry; it never rewrites a native artifact.
    helper$golden_publication_contract <- function(id) contract
    why <- helper$golden_compare_styles(book, id, native_book, id, mask = "",
      got_width_offset = helper$golden_r_width_offset, publication_id = id)
    qa_check(paste(id, "full XLSX body/styles/geometry and every declared paragraph"), length(why) == 0L)
    split_console <- function(lines) {
      edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
      if (length(edge) < 2L) stop("Incomplete survival console box.")
      start <- edge[1L]
      end <- tail(edge, 1L)
      tail <- if (length(lines) > end) lines[seq.int(end + 1L, length(lines))] else character()
      list(before = if (start > 1L) lines[seq_len(start - 1L)] else character(),
        body = lines[seq.int(start, end)], after = tail, tail = tail[nzchar(trimws(tail))])
    }
    raw_native <- readLines(file.path(directory, paste0(id, "_console.txt")), warn = FALSE)
    # Assert complete native sink-status text before splitting the listing.
    # Frozen native return paths preserve the original capture identity even
    # when an authenticated artifact directory has been copied elsewhere.
    native_paths <- native[native$name %in% c("csv", "markdown", "xlsx", "sheet"), , drop = FALSE]
    native_path_keys <- c("csv", "markdown", "xlsx", "sheet")
    qa_check(paste(id, "complete native destination identities"), nrow(native_paths) == 4L &&
      !anyDuplicated(native_paths$name) && all(native_paths$kind == "macro") &&
      identical(sort(native_paths$name), sort(native_path_keys)))
    destinations <- stats::setNames(native_paths$value, native_paths$name)
    qa_check(paste(id, "literal native destination basenames/sheet"),
      identical(unname(basename(destinations[native_path_keys])), c(paste0(id, ".csv"),
        paste0(id, ".md"), "survtab.xlsx", id)))
    native_exports <- c(paste0("CSV exported to ", destinations[["csv"]]),
      paste0("Markdown exported to ", destinations[["markdown"]]),
      paste0("Exported to ", destinations[["xlsx"]], ", sheet ", destinations[["sheet"]]))
    qa_check(paste(id, "all three complete native sink-status lines"),
      identical(tail(raw_native, 3L), native_exports))
    gc <- split_console(capture.output(print(actual)))
    wc <- split_console(head(raw_native, -3L))
    qa_check(paste(id, "full R/native console annotations"), identical(gc$tail, paragraphs) && identical(wc$tail, if (is.null(native_notes[[id]])) character() else native_notes[[id]]))
    qa_check(paste(id, "complete R/native console paragraph spacing"),
      identical(gc$after, c(as.vector(rbind("", paragraphs)), "")) &&
      identical(wc$after, c("", native_notes[[id]])))
    no_failure <- "Note: no failures in the analysis sample; the log-rank test is not possible and is omitted"
    qa_check(paste(id, "complete R/native pre-box annotations"), identical(gc$before, character()) &&
      identical(wc$before, if (id == "SV002") no_failure else character()))
    if (id == "SV002") qa_check("native no-failure pre-box note is literal",
      identical(wc$before, no_failure))
    qa_check(paste(id, "strict complete console body"), length(helper$golden_compare_console(gc$body, wc$body, mask = "")) == 0L)
    receipt$results[[id]] <- list(actual = actual, native_stored = native, style_diagnostics = why)
  }
  refusals <- utils::read.csv(file.path(directory, "native_refusals.csv"), colClasses = "character")
  qa_check("actual native support/weight refusal codes", identical(refusals$rc, c("198", "198")))
  qa_check("all twelve native scenarios completed", identical(names(receipt$results), sprintf("SV%03d", 1:12)))
}
tryCatch(work(), error = function(e) {
  receipt$results$error <- list(class = class(e), message = conditionMessage(e), call = conditionCall(e))
  qa_check("unexpected native survival comparison error", FALSE)
})
destination <- Sys.getenv("TABTOOLS_SURVTAB_NATIVE_RESULTS")
if (nzchar(destination)) saveRDS(list(results = receipt$results, executed = qa_state$executed,
  failed = qa_state$failed, skipped = qa_state$skipped), destination, version = 3)
qa_done("crossval_survtab.R")
