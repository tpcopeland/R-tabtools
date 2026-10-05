# table1_tc vars() parsing and per-variable resolution (plan task 2.2), a port
# of the vars() loop in _desctab_collect.ado:185-307.

.t1_types <- c("contn", "contln", "conts", "cat", "cate", "bin", "bine")

#' Parse table1_tc `vars` into variable specifications
#'
#' Three input forms, freely mixed:
#' * unnamed strings: Stata-style entries `"name [type [fmt1 [fmt2]]]"`,
#'   several separated by `\` (`"age contn %5.1f \\ sex bin"`); a bare name
#'   means `auto`;
#' * named elements: `c(age = "contn %5.1f", sex = "bin")`, the value holding
#'   `type [fmt1 [fmt2]]` (`""` means `auto`);
#' * a list of such named or unnamed strings.
#'
#' Words beyond the fourth are ignored, as Stata's `word 1..4 of` does
#' (_desctab_collect.ado:186-189).
#'
#' @param vars The `vars` argument.
#' @return List of specs: `name`, `type` (possibly `"auto"`), `fmt1`, `fmt2`
#'   (`NA` when absent).
#' @keywords internal
#' @noRd
.t1_parse_vars <- function(vars) {
  if (is.null(vars) || !length(vars)) {
    # desctab.ado:109-112 ("vars() or varlist required")
    cli::cli_abort("{.arg vars} is required.", call = NULL)
  }
  if (is.list(vars)) {
    nm <- names(vars)
    ok <- vapply(vars, function(v) is.character(v) && length(v) == 1L, TRUE)
    if (!all(ok)) cli::cli_abort("Each element of a {.arg vars} list must be a single string.", call = NULL)
    vars <- stats::setNames(unlist(vars, use.names = FALSE), nm)
  }
  if (!is.character(vars)) {
    cli::cli_abort("{.arg vars} must be a character vector, a named character vector, or a list.", call = NULL)
  }
  nm <- names(vars) %||% rep("", length(vars))
  nm[is.na(nm)] <- ""
  words_of <- function(s) {
    w <- strsplit(trimws(s), "\\s+")[[1]]
    w[nzchar(w)]
  }
  entries <- list()
  for (i in seq_along(vars)) {
    val <- vars[[i]]
    if (is.na(val)) val <- ""
    if (nzchar(nm[i])) {
      if (grepl("\\", val, fixed = TRUE)) {
        cli::cli_abort("{.arg vars}: the specification for {.val {nm[i]}} may not contain {.code \\\\}.",
                       call = NULL)
      }
      entries[[length(entries) + 1L]] <- c(nm[i], words_of(val))
    } else {
      for (part in strsplit(val, "\\", fixed = TRUE)[[1]]) {
        w <- words_of(part)
        if (length(w)) entries[[length(entries) + 1L]] <- w
      }
    }
  }
  if (!length(entries)) {
    # _desctab_collect.ado:301-304
    cli::cli_abort("{.arg vars} did not contain any variables.", call = NULL)
  }
  lapply(entries, function(w) {
    list(name = w[1],
         type = if (length(w) >= 2L && w[2] != "auto") w[2] else "auto",
         fmt1 = if (length(w) >= 3L) w[3] else NA_character_,
         fmt2 = if (length(w) >= 4L) w[4] else NA_character_)
  })
}

# Columns table1_tc cannot summarise, refused with a clear error: list
# columns (not atomic) and dates/date-times. Stata stores %td dates as days
# since 1960 and R's Date counts from 1970, so any summary of the raw
# numbers would differ from Stata's by 3,653 days (Phase 2 review P2-4); a
# mean of day numbers is rarely wanted anyway.
.t1_check_column <- function(x, nm, what = "variable") {
  lead <- if (what == "by") "{.arg by} variable {.var {nm}}" else "Variable {.var {nm}}"
  if (is.complex(x)) {
    cli::cli_abort(c(paste(lead, "is complex."),
                     "i" = "Convert it explicitly to the real-valued quantity you want to summarise."), call = NULL)
  }
  if (is.list(x)) {
    cli::cli_abort(c(paste(lead, "is a list column."),
                     "i" = "{.fn table1_tc} summarises atomic columns only."), call = NULL)
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    cli::cli_abort(c(paste(lead, "is a date or date-time ({.cls {class(x)[1]}})."),
                     "i" = "{.fn table1_tc} does not summarise dates: convert it first, e.g. to a duration or a year."),
                   call = NULL)
  }
  invisible(x)
}

# A matrix column (cbind(), poly()) or data-frame column holds several
# variables under one name; its values would be flattened into one vector
# whose length is not the number of records (audit A04). A one-column
# matrix (scale()) is its vector: the dim is dropped, other attributes
# kept. puttab refuses such columns too (puttab.R).
.t1_check_shape <- function(data, cols) {
  for (nm in cols) {
    x <- data[[nm]]
    if (is.data.frame(x)) {
      cli::cli_abort(c("Variable {.var {nm}} is a data-frame column.",
                       "i" = "{.fn table1_tc} summarises atomic columns only: give each of its columns its own name."),
                     call = NULL)
    }
    if (!is.null(dim(x))) {
      if (length(dim(x)) == 2L && NCOL(x) == 1L) {
        dim(x) <- NULL
        data[[nm]] <- x
      } else {
        cli::cli_abort(c("Variable {.var {nm}} is a matrix column with {NCOL(x)} columns.",
                         "i" = "{.fn table1_tc} summarises one variable per column: give each of its columns its own name first."),
                       call = NULL)
      }
    }
  }
  data
}

# Inf, -Inf and NaN cannot occur in a Stata variable (nor can values of
# 2^1023 = 8.99e307 or more, Stata's missing range), so no Stata cell
# exists for them; R's summaries would print "." or an infinite quartile
# (a conts `2 (1, .)` for 1, 2, Inf) or fail. Refused by name (Milestone H,
# H10), on every record (as the weight checks do), whatever the type.
.t1_check_finite <- function(x, nm) {
  if (!is.numeric(x) || is.factor(x)) return(invisible(x))
  v <- unclass(x)
  bad <- is.infinite(v) | is.nan(v)
  if (any(bad)) {
    vals <- unique(format(v[bad]))
    cli::cli_abort(c("Variable {.var {nm}} holds non-finite values.",
                     "x" = "{sum(bad)} record{?s} hold{?s/} {.val {vals}}.",
                     "i" = "Stata variables cannot hold infinities or NaN; use {.code NA} for a missing value."),
                   call = NULL)
  }
  big <- !is.na(v) & abs(v) >= .stata_missing_from
  if (any(big)) {
    vals <- unique(format(v[big]))
    cli::cli_abort(c("Variable {.var {nm}} holds values beyond Stata's largest number (8.99e+307).",
                     "x" = "{sum(big)} record{?s} hold{?s/} {.val {vals}}.",
                     "i" = "Stata stores such a value as missing; rescale the variable or use {.code NA}."),
                   call = NULL)
  }
  invisible(x)
}

# Analysis columns (vars, by, wt, fweight) whose name occurs more than once
# in `data`: data[[name]] would silently use the first.
.t1_check_duplicate_names <- function(data, used) {
  dups <- unique(names(data)[duplicated(names(data))])
  hit <- intersect(used, dups)
  if (length(hit)) {
    cli::cli_abort(c("{.arg data} has more than one column named {.var {hit}}.",
                     "i" = "Rename the columns so that each name is unique (e.g. with {.fn make.unique})."),
                   call = NULL)
  }
  invisible(NULL)
}

# Empty strings are missing in Stata (`encode` and `markout` treat "" as
# missing), and haven::read_dta() returns Stata's missing strings as "".
# Turn "" into NA in character and factor columns (a factor loses its ""
# level), keeping every other attribute (Phase 2 review P0-2).
.t1_blank_to_na <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    if ("" %in% lv) levels(x)[lv == ""] <- NA
    return(x)
  }
  if (is.character(x)) {
    blank <- !is.na(x) & x == ""
    if (any(blank)) x[blank] <- NA
  }
  x
}

# Subset a column keeping its label attributes: plain `[` drops them from
# attribute-labelled numerics, and `[.factor` keeps only levels, class and
# contrasts, so a factor lost its "label" under wt()/fweight, which subset
# every column (Milestone H, H17; user item (a)). haven_labelled vectors
# keep theirs through vctrs; an attribute the method kept is not touched.
.t1_sub <- function(x, keep) {
  out <- x[keep]
  for (a in c("labels", "label")) {
    if (is.null(attr(out, a, exact = TRUE))) attr(out, a) <- attr(x, a, exact = TRUE)
  }
  out
}

# Plain numeric (or character) values of a column.
.t1_values <- function(x) {
  if (is.factor(x)) return(as.character(x))
  if (is.logical(x)) return(as.integer(x))
  v <- unclass(x)
  attributes(v) <- NULL
  v
}

#' Resolve one parsed spec against the data
#'
#' Checks, in Stata's order: the variable exists (`confirm variable`, r(111));
#' auto types are detected on the analysis sample; the type is known (r(498));
#' non-categorical types need a numeric column (r(109)); binary variables must
#' be 0/1 (r(198)); categorical and binary variables need at least one
#' observed category (r(198)). Display formats must be ones `stata_fmt()`
#' renders (Stata's `string()` silently returns `""` for an invalid format; R
#' refuses it).
#'
#' @param spec One element of `.t1_parse_vars()`.
#' @param data The data frame.
#' @param touse Logical analysis-sample indicator (by() non-missing).
#' @param include_missing The `missing` option.
#' @param labels The `labels` override.
#' @return The spec with `type` resolved and `x`, `label` added.
#' @keywords internal
#' @noRd
.t1_resolve_var <- function(spec, data, touse, include_missing, labels = NULL) {
  nm <- spec$name
  if (!nm %in% names(data)) {
    cli::cli_abort("variable {.var {nm}} not found.", call = NULL)
  }
  x <- data[[nm]]
  .t1_check_column(x, nm)
  .t1_check_finite(x, nm)
  type <- spec$type
  if (type == "auto") {
    # _desctab_collect.ado:196-199: detection runs on the analysis sample.
    type <- tt_detect_vartype(.t1_sub(x, touse))
  }
  if (!type %in% .t1_types) {
    # _desctab_collect.ado:200-204
    cli::cli_abort(c("{.code {nm} {type}} is not allowed in {.arg vars}.",
                     "i" = "Variables must be classified as contn, contln, conts, cat, cate, bin or bine."),
                   call = NULL)
  }
  is_text <- is.character(x) || is.factor(x)
  if (!type %in% c("cat", "cate") && is_text) {
    # _desctab_collect.ado:224-229
    cli::cli_abort("{.var {nm}} must be numeric for type {.val {type}}.", call = NULL)
  }
  v <- .t1_values(x)[touse]
  if (type %in% c("bin", "bine")) {
    obs <- v[!is.na(v)]
    if (!length(obs)) {
      cli::cli_abort("no categories for {.var {nm}} ... cannot tabulate.", call = NULL)
    }
    if (!all(obs == 0 | obs == 1)) {
      # _desctab_collect.ado:238-242
      cli::cli_abort(c("binary variable {.var {nm}} must be 0 (negative) or 1 (positive).",
                       "i" = "Did you mean {.val cat}? Use {.code {nm} = \"cat\"} for a categorical variable."),
                     call = NULL)
    }
  }
  if (type %in% c("cat", "cate") && !any(!is.na(v) | include_missing)) {
    cli::cli_abort("no categories for {.var {nm}} ... cannot tabulate.", call = NULL)
  }
  for (f in c(spec$fmt1, spec$fmt2)) if (!is.na(f)) .parse_stata_fmt(f)
  spec$type <- type
  spec$x <- x
  spec$label <- var_label(x, nm, labels)
  spec
}
