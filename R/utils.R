`%||%` <- function(x, y) if (is.null(x)) y else x

.check_dp <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x) || x < 1 || x > 10) {
    cli::cli_abort("{.arg {arg}} must be a whole number between 1 and 10.", call = NULL)
  }
  as.integer(x)
}

.check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort("{.arg {arg}} must be a single non-empty string.", call = NULL)
  }
  x
}

.check_int_range <- function(x, arg, lo, hi) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x) || x < lo || x > hi) {
    cli::cli_abort("{.arg {arg}} must be a whole number between {lo} and {hi}.", call = NULL)
  }
  as.integer(x)
}

# _tabtools_common.ado:498-505
.check_fontsize <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x) || x < 1 || x > 72) {
    cli::cli_abort("{.arg fontsize} must be between 1 and 72.", call = NULL)
  }
  as.integer(x)
}

# _tabtools_common.ado:507-513
.check_borderstyle <- function(x) {
  if (!is.character(x) || length(x) != 1L || !x %in% c("default", "thin", "medium", "academic")) {
    cli::cli_abort("{.arg borderstyle} must be one of {.val default}, {.val thin}, {.val medium}, or {.val academic}.",
                   call = NULL)
  }
  x
}

.check_threshold <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0 || x >= 1) {
    cli::cli_abort("{.arg {arg}} must be a number strictly between 0 and 1.", call = NULL)
  }
  as.numeric(x)
}

# Excel's worksheet-name rules (_tabtools_common.ado:435-468).
.check_sheet <- function(sheet, arg = "sheet") {
  if (!is.character(sheet) || length(sheet) != 1L || is.na(sheet) || !nzchar(sheet)) {
    cli::cli_abort("{.arg {arg}}: sheet name may not be blank.", call = NULL)
  }
  if (nchar(sheet) > 31L) {
    cli::cli_abort("{.arg {arg}}: sheet name {.val {sheet}} exceeds Excel's 31-character limit.", call = NULL)
  }
  # Control characters corrupt workbook.xml (Muse audit P1-43).
  if (grepl("[[:cntrl:]]", sheet)) {
    cli::cli_abort("{.arg {arg}}: sheet name may not contain a control character (a line break or tab, for example).",
                   call = NULL)
  }
  if (grepl("[][/\\\\?*:]", sheet)) {
    cli::cli_abort("{.arg {arg}}: sheet name contains characters not allowed by Excel (\\ / ? * [ ] :).", call = NULL)
  }
  if (startsWith(sheet, "'") || endsWith(sheet, "'")) {
    cli::cli_abort("{.arg {arg}}: sheet name may not begin or end with an apostrophe.", call = NULL)
  }
  if (tolower(sheet) == "history") {
    cli::cli_abort("{.arg {arg}}: History is reserved by Excel and cannot be used as a sheet name.", call = NULL)
  }
  sheet
}

# The writers' and converters' `x` (review P3-3: a bare stopifnot()).
.tt_check_table <- function(x, arg = "x") {
  if (!inherits(x, "tt_table")) {
    cli::cli_abort("{.arg {arg}} must be a {.cls tt_table}, not {.obj_type_friendly {x}}.", call = NULL)
  }
  invisible(x)
}

# A writer's `path`: one non-missing string with the sink's extension (Muse
# audit P1-44: NA or a vector reached grepl() inside if()).
.tt_check_path <- function(path, rx, arg, what) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg {arg}} must be a single file path, not {.obj_type_friendly {path}}.", call = NULL)
  }
  if (!grepl(rx, tolower(path))) cli::cli_abort("{.arg {arg}} must specify {what}.", call = NULL)
  invisible(path)
}

# Display width of strings (a "\u00b1" counts one column, as in Stata's udstrlen).
.dwidth <- function(x) nchar(x, type = "width")

# Byte length (Stata's length()/strlen(): "\u00b1" counts two).
.blen <- function(x) nchar(x, type = "bytes")

# ---------------------------------------------------------------------------
# Stata matrix row names (tabtools 2.1.11 and 2.1.12). Both regtab and desctab keep a
# row name only if `matrix rownames` stores it unchanged. Rules probed in
# Stata 17 (qa/stata/make_rowname_probe.do): "#" between two
# operands is an interaction ("a#b" -> "c.a#c.b"); "cov(x)" is read back as
# "var(x)"; a leading "/" is dropped; a second bracket group, or a bracket
# inside an unclosed parenthesis, is rejected; names longer than 32
# characters are cut. Everything else (parentheses, "-", "/", "%", "<",
# "=", "$", "'", non-ASCII letters, a leading digit) survives.
.stata_rowname_survives <- function(s) {
  nzchar(s) &
    !grepl(".#.", s, perl = TRUE) &
    !grepl("^cov\\(.+\\)$", s, perl = TRUE) &
    !startsWith(s, "/") &
    !grepl("\\[[^]]*\\].*\\[", s, perl = TRUE) &
    !grepl("\\([^)]*\\[[^)]*$", s, perl = TRUE) &
    nchar(s, type = "chars", allowNA = TRUE) <= 32L
}

# Stata's strtoname(): every ASCII character that is not a letter, digit or
# "_" becomes "_" (non-ASCII characters are kept, symbols included:
# "\u00c5lder \u226565" -> "\u00c5lder_\u226565", "a\u20acb" stays; probed in Stata 17,
# Phase 7b review F10), a leading ASCII digit gains a "_" prefix, and the
# result is cut to 32 characters.
.stata_strtoname <- function(s) {
  s <- gsub("[\\x{01}-\\x{2f}\\x{3a}-\\x{40}\\x{5b}-\\x{5e}\\x{60}\\x{7b}-\\x{7f}]", "_", enc2utf8(s), perl = TRUE)
  s <- ifelse(grepl("^[0-9]", s), paste0("_", s), s)
  s <- ifelse(nzchar(s), s, "_")
  substr(s, 1L, 32L)
}

# Columns a function reads by name must not be named twice in the data
# frame (Codex audit CX-3): `x[[name]]` silently returns the first of them.
.tt_check_unambiguous <- function(data, cols, arg) {
  amb <- intersect(cols, names(data)[duplicated(names(data))])
  if (length(amb)) {
    cli::cli_abort(c("{.arg {arg}}: {.var {amb}} name{?s/} more than one column of the data.",
                     "i" = "Give the columns unique names first."), call = NULL)
  }
  invisible(cols)
}
