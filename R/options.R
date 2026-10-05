# Persistent defaults and style resolution (task 1.5).
#
# Stata: `tabtools set <key> <value> [, permanent]` stores TABTOOLS_* globals
# (tabtools.ado:115-203) that _tabtools_resolve_format and
# _tabtools_resolve_colors read (_tabtools_common.ado:482-567). R keeps the
# same seven keys as session options (`tabtools.<key>`), persisted to a DCF
# file only on request.

.tt_option_keys <- c("font", "fontsize", "borderstyle", "digits", "boldp",
                     "headercolor", "zebracolor")

.tt_defaults_file <- function() {
  file.path(tools::R_user_dir("tabtools", "config"), "defaults.dcf")
}

#' Persistent formatting defaults
#'
#' Session-wide house-style defaults: a value set here is used whenever the
#' corresponding argument of a tabtools command ([table1_tc()],
#' [regtab()], [stratetab()], ...) is left `NULL`. Called with no arguments,
#' returns the current defaults. The analogue of Stata's `tabtools set`.
#'
#' @param font Font family (Stata default Arial).
#' @param fontsize Default font size, a whole number from 6 to 72 (Stata's
#'   `tabtools set fontsize` range); the `fontsize` argument of
#'   [table1_tc()] and [regtab()] accepts 1 to 72 for one table (Stata's
#'   `fontsize()`).
#'   Unset, tables use 10.
#' @param borderstyle One of `"default"`, `"thin"`, `"medium"`, `"academic"`.
#' @param digits Decimal places, 0-6, for the estimates of [regtab()] and
#'   [effecttab()] and the numbers of [puttab()] and [wttab()]. Unset, they
#'   use 2.
#' @param boldp P-value threshold for bold p cells, in (0, 1).
#' @param headercolor,zebracolor Colours: a Stata colour name (`"navy"`), an
#'   `"R G B"` triplet, or a hex code (`"#DBE5F1"`).
#' @param persist Also write the current defaults to
#'   `tools::R_user_dir("tabtools", "config")`, reloaded when the package
#'   loads (Stata's `permanent`). The file is written first: if it cannot
#'   be written, the call fails and the session defaults stay as they were.
#' @param clear Remove every default (and the persisted file when `persist`;
#'   a file that cannot be deleted gives a warning).
#' @return The defaults in effect, invisibly when any were changed.
#' @examples
#' tabtools_options()
#' # Save the current font setting, so the example can restore it
#' op <- options(tabtools.font = getOption("tabtools.font"))
#' tabtools_options(font = "Calibri")
#' tabtools_options()$font
#' options(op)
#' @export
tabtools_options <- function(font = NULL, fontsize = NULL, borderstyle = NULL,
                             digits = NULL, boldp = NULL, headercolor = NULL,
                             zebracolor = NULL, persist = FALSE, clear = FALSE) {
  new <- list(font = font, fontsize = fontsize, borderstyle = borderstyle,
              digits = digits, boldp = boldp, headercolor = headercolor,
              zebracolor = zebracolor)
  new <- new[!vapply(new, is.null, TRUE)]
  for (a in c("persist", "clear")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  if (clear && length(new)) {
    cli::cli_abort(c("{.arg clear} removes every default; it cannot be combined with {.arg {names(new)}}.",
                     "i" = "Call {.code tabtools_options(clear = TRUE)} first, then set the new defaults."),
                   call = NULL)
  }
  if (clear) {
    options(stats::setNames(rep(list(NULL), length(.tt_option_keys)),
                            paste0("tabtools.", .tt_option_keys)))
    if (persist) .tt_remove_defaults_file()
    return(invisible(.tt_current_options()))
  }
  if (!length(new) && !persist) return(.tt_current_options())
  if (!is.null(new[["font"]])) .check_string(new[["font"]], "font")
  if (!is.null(new[["fontsize"]])) .check_default_fontsize(new[["fontsize"]])
  if (!is.null(new[["borderstyle"]])) .check_borderstyle(new[["borderstyle"]])
  if (!is.null(new[["digits"]])) .check_int_range(new[["digits"]], "digits", 0, 6)
  if (!is.null(new[["boldp"]])) .check_threshold(new[["boldp"]], "boldp")
  if (!is.null(new[["headercolor"]])) tt_parse_color(new[["headercolor"]], "headercolor")
  if (!is.null(new[["zebracolor"]])) tt_parse_color(new[["zebracolor"]], "zebracolor")
  cur <- .tt_current_options()
  cur[names(new)] <- new
  cur <- cur[intersect(.tt_option_keys, names(cur))]
  # persist: the file is written first and the session changed only when it
  # was (Milestone H, H20; finding F33), so a failed write leaves both as
  # they were.
  if (persist) .tt_write_defaults_file(cur)
  if (length(new)) options(stats::setNames(new, paste0("tabtools.", names(new))))
  invisible(.tt_current_options())
}

# Write the persisted defaults: a temporary file in the same directory,
# renamed over the target, so a failure never leaves a half-written file.
.tt_write_defaults_file <- function(vals) {
  f <- .tt_defaults_file()
  dir <- dirname(f)
  fail <- function(e = NULL) {
    cli::cli_abort(c("Could not save the tabtools defaults to {.file {f}}.",
                     "i" = "The session defaults were not changed."),
                   parent = e, call = NULL)
  }
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) fail()
  tmp <- tempfile("defaults", tmpdir = dir, fileext = ".dcf")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  txt <- vapply(vals, function(v) paste(v, collapse = " "), "")
  tryCatch(suppressWarnings(write.dcf(as.data.frame(as.list(txt), stringsAsFactors = FALSE), tmp)),
           error = function(e) fail(e))
  if (!suppressWarnings(file.rename(tmp, f))) fail()
  invisible(f)
}

# Remove the persisted defaults; a file that cannot be deleted is reported
# (it would bring the defaults back at the next load).
.tt_remove_defaults_file <- function() {
  f <- .tt_defaults_file()
  if (!file.exists(f)) return(invisible(TRUE))
  ok <- suppressWarnings(file.remove(f))
  if (!ok || file.exists(f)) {
    cli::cli_warn(c("Could not delete the saved tabtools defaults {.file {f}}.",
                    "i" = "The session defaults are cleared, but the file will restore them when tabtools is next loaded."),
                  call = NULL)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# `tabtools set fontsize` accepts 6-72 (tabtools.ado:144-147), narrower than
# the per-table fontsize() range (1-72, _tabtools_common.ado:497-505).
.check_default_fontsize <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x) || x < 6 || x > 72) {
    cli::cli_abort("{.arg fontsize} default must be a whole number between 6 and 72.", call = NULL)
  }
  as.integer(x)
}

# Validate one persisted value; NULL (with a warning) when it is unusable.
.tt_check_persisted <- function(k, v, file) {
  tryCatch({
    switch(k,
      fontsize = .check_default_fontsize(v <- as.numeric(v)),
      digits = .check_int_range(v <- as.numeric(v), "digits", 0, 6),
      boldp = .check_threshold(v <- as.numeric(v), "boldp"),
      font = .check_string(v, "font"),
      borderstyle = .check_borderstyle(v),
      headercolor = , zebracolor = tt_parse_color(v, k))
    v
  }, warning = function(w) NULL, error = function(e) NULL) %||% {
    warning(sprintf("tabtools: ignoring invalid saved default %s = '%s' in %s", k, v, file),
            call. = FALSE)
    NULL
  }
}

.tt_current_options <- function() {
  out <- lapply(paste0("tabtools.", .tt_option_keys), getOption)
  names(out) <- .tt_option_keys
  out[!vapply(out, is.null, TRUE)]
}

# Called from .onLoad: persisted defaults fill any key not already set.
.tt_load_persisted <- function() {
  f <- .tt_defaults_file()
  if (!file.exists(f)) return(invisible())
  d <- tryCatch(read.dcf(f), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(invisible())
  for (k in intersect(colnames(d), .tt_option_keys)) {
    opt <- paste0("tabtools.", k)
    if (is.null(getOption(opt))) {
      v <- .tt_check_persisted(k, unname(d[1, k]), f)
      if (!is.null(v)) options(stats::setNames(list(v), opt))
    }
  }
  invisible()
}

# ---------------------------------------------------------------------------
# Colours

# Named colours as Stata's Excel writer renders them. _tabtools_validate_color
# (_tabtools_common.ado:135-153) accepts 49 names, but the fill is set by Mata
# xl() (op 7 in _tabtools_xlsx_apply_styles.ado), which knows only these 23
# web colour names; the values below were read back from Stata-written
# workbooks (2026-09-25 probe). They are not Stata's graph-scheme colours.
.stata_colors <- c(
  black = "0 0 0", blue = "0 0 255", brown = "165 42 42", cyan = "0 255 255",
  dimgray = "105 105 105", gold = "255 215 0", gray = "128 128 128",
  green = "0 128 0", khaki = "240 230 140", lavender = "230 230 250",
  lime = "0 255 0", magenta = "255 0 255", maroon = "128 0 0",
  navy = "0 0 128", olive = "128 128 0", orange = "255 165 0",
  pink = "255 192 203", purple = "128 0 128", red = "255 0 0",
  sienna = "160 82 45", teal = "0 128 128", white = "255 255 255",
  yellow = "255 255 0"
)

# Names the Stata validator accepts but xl() rejects with r(16136).
.stata_colors_rejected <- c(
  "bluishgray", "cranberry", "dkgreen", "dknavy", "dkorange", "ebblue",
  "eggshell", "eltblue", "emerald", "forest_green", "ltblue", "ltbluishgray",
  "ltkhaki", "midblue", "midgreen", "mint", "orange_red", "sand", "stone",
  paste0("gs", 0:16)
)

#' Parse a colour into an Excel ARGB string
#'
#' Accepts a Stata colour name, an `"R G B"` triplet (0-255), or a hex code
#' (`"#RRGGBB"` / `"RRGGBB"`, an R extension).
#'
#' @param x A single colour specification.
#' @param arg Argument name for error messages.
#' @return `"FFRRGGBB"`.
#' @keywords internal
#' @noRd
tt_parse_color <- function(x, arg = "color") {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single colour specification.", call = NULL)
  }
  s <- trimws(gsub('"', "", x, fixed = TRUE))
  if (grepl("^#?[0-9A-Fa-f]{6}$", s)) return(paste0("FF", toupper(sub("^#", "", s))))
  if (grepl("^[A-Za-z][A-Za-z0-9_]*$", s)) {
    rgb <- .stata_colors[tolower(s)]
    if (is.na(rgb) && tolower(s) %in% .stata_colors_rejected) {
      cli::cli_abort(c("{.arg {arg}}: Stata's Excel writer does not support the colour name {.val {s}}.",
                       "i" = "Stata fails with r(16136) for it; use an RGB triplet or a hex code instead."),
                     call = NULL)
    }
    if (is.na(rgb)) {
      cli::cli_abort(c("{.arg {arg}} is not a supported Stata colour name.",
                       "i" = "Use a name such as {.val navy}, an RGB triplet like {.val 200 220 240}, or a hex code."),
                     call = NULL)
    }
    s <- unname(rgb)
  }
  if (!grepl("^[0-9]+ +[0-9]+ +[0-9]+$", s)) {
    cli::cli_abort("{.arg {arg}} must be a Stata colour name, an {.val R G B} triplet, or a hex code.", call = NULL)
  }
  v <- as.double(strsplit(s, " +")[[1]])
  if (any(!is.finite(v) | v < 0 | v > 255)) {
    cli::cli_abort("{.arg {arg}} RGB values must be between 0 and 255.", call = NULL)
  }
  paste0("FF", paste(sprintf("%02X", as.integer(v)), collapse = ""))
}

# ---------------------------------------------------------------------------
# Style resolution

#' Resolve the style list stored on a tt_table
#'
#' Argument -> session default (`tabtools_options()`) -> Stata default, as in
#' `_tabtools_resolve_format` and `_tabtools_resolve_colors`. `borderstyle`
#' `"default"` means `"thin"`; `"academic"` gives medium horizontal rules and
#' no vertical rules (`_tabtools_common.ado:509-515`).
#'
#' @return A list: font, fontsize, borderstyle, hborder, vborder (NA when
#'   academic), headershade, zebra, headercolor, zebracolor, highlightcolor,
#'   smdcolor, dimcolor (ARGB), boldp, highlight, smdthreshold, dimnonsig.
#' @keywords internal
#' @noRd
tt_resolve_style <- function(font = NULL, fontsize = NULL, borderstyle = NULL,
                             headershade = FALSE, zebra = FALSE,
                             headercolor = NULL, zebracolor = NULL,
                             boldp = NULL, highlight = NULL, smdthreshold = 0.1,
                             dimnonsig = FALSE) {
  font <- font %||% getOption("tabtools.font") %||% "Arial"
  .check_string(font, "font")
  fontsize <- fontsize %||% getOption("tabtools.fontsize") %||% 10
  fontsize <- .check_fontsize(fontsize)
  borderstyle <- borderstyle %||% getOption("tabtools.borderstyle") %||% "thin"
  borderstyle <- .check_borderstyle(borderstyle)
  if (borderstyle == "default") borderstyle <- "thin"
  boldp <- boldp %||% getOption("tabtools.boldp")
  boldp <- .threshold_or_off(boldp, "boldp")
  highlight <- .threshold_or_off(highlight, "highlight")
  if (!is.numeric(smdthreshold) || length(smdthreshold) != 1L || is.na(smdthreshold) ||
      !(smdthreshold > 0 || smdthreshold == -1)) {
    cli::cli_abort("{.arg smdthreshold} must be positive, or -1 to disable.", call = NULL)
  }
  list(
    font = font,
    fontsize = fontsize,
    borderstyle = borderstyle,
    hborder = if (borderstyle == "academic") "medium" else borderstyle,
    vborder = if (borderstyle == "academic") NA_character_ else borderstyle,
    headershade = isTRUE(headershade),
    zebra = isTRUE(zebra),
    headercolor = tt_parse_color(headercolor %||% getOption("tabtools.headercolor") %||% "219 229 241", "headercolor"),
    zebracolor = tt_parse_color(zebracolor %||% getOption("tabtools.zebracolor") %||% "237 242 249", "zebracolor"),
    highlightcolor = "FFFFFFCC",
    smdcolor = "FFFFEBCD",
    dimcolor = "FFA0A0A0",
    boldp = boldp,
    highlight = highlight,
    smdthreshold = smdthreshold,
    dimnonsig = isTRUE(dimnonsig)
  )
}

.threshold_or_off <- function(x, arg) {
  if (is.null(x) || (is.numeric(x) && length(x) == 1L && !is.na(x) && x == -1)) return(NA_real_)
  .check_threshold(x, arg)
}
