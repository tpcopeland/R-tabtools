# One preflight of every output target before anything is written
# (Milestone H, H8; finding F09). table1_tc() and regtab() write up to three
# files in turn (xlsx, csv, markdown). Without a joint check, two arguments
# naming the same file made the later writer destroy the earlier output
# while the call reported success. Stata tabtools 2.1.12 refuses the same
# collisions up front (`_tabtools_check_sinks.ado`, take_action item 12,
# fixed); so does R.

# Is the filesystem case-insensitive? Windows and macOS (APFS/HFS+ default)
# are; Linux file systems are not. The option lets tests (and users on an
# unusual mount) say otherwise.
.tt_fs_case_insensitive <- function() {
  opt <- getOption("tabtools.case_insensitive_fs")
  if (!is.null(opt)) return(isTRUE(opt))
  .Platform$OS.type == "windows" || identical(unname(Sys.info()[["sysname"]]), "Darwin")
}

# A comparable key for a target path: `~` expanded, symbolic links followed
# (.tt_resolve_link(), dangling ones too), the directory resolved through
# normalizePath() (the file itself need not exist yet, and a relative
# "./a.xlsx" must meet its absolute twin), case-folded where the file
# system ignores case.
.tt_path_key <- function(path) {
  p <- .tt_resolve_link(path.expand(path))
  dir <- dirname(p)
  base <- basename(p)
  ndir <- if (dir.exists(dir)) normalizePath(dir, winslash = "/", mustWork = FALSE) else dir
  if (!grepl("^(/|[A-Za-z]:)", ndir)) ndir <- file.path(normalizePath(getwd(), winslash = "/"), ndir)
  key <- file.path(ndir, base)
  if (.tt_fs_case_insensitive()) key <- tolower(key)
  key
}

# Follow symbolic links, including a dangling one whose target does not
# exist yet (a csv link onto the workbook about to be written; review R6).
.tt_resolve_link <- function(p) {
  for (i in seq_len(40L)) {
    l <- Sys.readlink(p)
    if (is.na(l) || !nzchar(l)) break
    p <- if (grepl("^(/|[A-Za-z]:)", l)) l else file.path(dirname(p), l)
  }
  p
}

#' Check every file target of one call before any is written
#'
#' Refuses, before anything is written: a `csv` target that is not a `.csv`
#' file (`xlsx` and `markdown` extensions are checked by the callers, in
#' Stata's order); two arguments naming the same file (after resolving
#' relative paths and, on case-insensitive file systems, case); a target
#' whose directory does not exist or is not writable; an existing target
#' that is a directory or read-only. An existing Markdown file is replaced
#' unless `mdappend` (Stata tabtools 2.1.12; 2.1.11 stopped with r(602)).
#'
#' @param xlsx,csv,markdown Target paths or `NULL`.
#' @param mdappend Append to an existing Markdown file.
#' @return The resolved keys, invisibly.
#' @keywords internal
#' @noRd
.tt_preflight_targets <- function(xlsx = NULL, csv = NULL, markdown = NULL, mdappend = FALSE) {
  if (!is.null(csv)) .tt_check_csv_path(csv)
  targets <- list(xlsx = xlsx, csv = csv, markdown = markdown)
  targets <- targets[!vapply(targets, is.null, TRUE)]
  if (!length(targets)) return(invisible(character()))
  keys <- vapply(targets, .tt_path_key, "")
  dup <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  if (any(dup)) {
    args <- names(targets)[dup]
    cli::cli_abort(c("{.arg {args}} name the same file, {.file {targets[[which(dup)[1]]]}}.",
                     "i" = "Each output needs its own file; the later writer would overwrite the earlier one."),
                   call = NULL)
  }
  for (a in names(targets)) {
    p <- path.expand(targets[[a]])
    if (dir.exists(p)) cli::cli_abort("The {.arg {a}} target {.file {targets[[a]]}} is a directory.", call = NULL)
    dir <- dirname(p)
    if (!dir.exists(dir)) {
      cli::cli_abort("The directory of the {.arg {a}} target {.file {targets[[a]]}} does not exist.", call = NULL)
    }
    if (file.exists(p) && file.access(p, 2L) != 0L) {
      cli::cli_abort("The {.arg {a}} target {.file {targets[[a]]}} exists and is not writable.", call = NULL)
    }
    if (!file.exists(p) && file.access(dir, 2L) != 0L) {
      cli::cli_abort("The directory of the {.arg {a}} target {.file {targets[[a]]}} is not writable.", call = NULL)
    }
  }
  invisible(keys)
}

# file.copy() without its warnings, as one place tests can replace to inject
# a failing copy (Codex audit CX-1).
.tt_file_copy <- function(from, to, overwrite = FALSE) {
  isTRUE(suppressWarnings(file.copy(from, to, overwrite = overwrite)))
}

# Do two files hold the same bytes?
.tt_same_file <- function(a, b) {
  sa <- file.size(a)
  if (is.na(sa) || !identical(sa, file.size(b))) return(FALSE)
  identical(readBin(a, "raw", sa), readBin(b, "raw", sa))
}

# Copy `from` to `to` and verify the copy, or stop before anything else is
# written (Codex audit CX-1: stacktab's append staging and its commit
# backups ignored file.copy()'s result, so an unreadable existing target was
# replaced by the new table alone). `what` names the target argument in the
# error; NULL is the commit's backup of an existing target.
.tt_copy_checked <- function(from, to, what = NULL) {
  ok <- file.access(from, 4L) == 0L && .tt_file_copy(from, to, overwrite = TRUE) && .tt_same_file(from, to)
  if (!isTRUE(ok)) {
    if (is.null(what)) {
      cli::cli_abort(c("Could not back up the existing file {.file {from}}; nothing was written.",
                       "i" = "The file must be readable, so that it can be restored if a later write fails."),
                     call = NULL)
    }
    cli::cli_abort(c("Could not read the existing {.arg {what}} target {.file {from}}; nothing was written.",
                     "i" = "Appending to a file, or adding a sheet to a workbook, needs read access to it."),
                   call = NULL)
  }
  invisible(to)
}

# csv() must name a .csv file (Stata tabtools 2.1.12 refuses any other name
# too, `_tabtools_check_sinks.ado`).
.tt_check_csv_path <- function(path, arg = "csv") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !grepl("\\.csv$", tolower(path))) {
    cli::cli_abort("{.arg {arg}} must specify a .csv file.", call = NULL)
  }
  invisible(path)
}

# `title` / `footnote` arguments: NULL, or one non-missing string (an NA
# used to surface as an invalid-tt_table error; review R9).
.tt_check_text_arg <- function(x, arg) {
  if (!is.null(x) && (!is.character(x) || length(x) != 1L || is.na(x))) {
    cli::cli_abort("{.arg {arg}} must be a single string.", call = NULL)
  }
  invisible(x)
}
