# Variable and value labels (task 1.3).
#
# Stata reads the variable label (`: variable label`, falling back to the
# variable name) and the value label of each level (`: label (var) code`,
# falling back to the code itself); see _desctab_collect.ado:215-286. The R
# equivalents are the "label" attribute (haven/labelled), haven_labelled
# value labels, and factor levels.

# Validate a `labels =` override: NULL, or a named character vector (or a
# list of single strings) with non-empty names and non-missing values. An NA
# label used to give a silently blank row label (and a print() error;
# H13, F16). Returns a named character vector.
.check_label_overrides <- function(labels, arg = "labels") {
  if (is.null(labels)) return(NULL)
  if (is.list(labels)) {
    ok <- vapply(labels, function(v) is.character(v) && length(v) == 1L, TRUE)
    if (!all(ok)) cli::cli_abort("Each element of {.arg {arg}} must be a single string.", call = NULL)
    labels <- stats::setNames(unlist(labels, use.names = FALSE), names(labels))
  }
  if (!is.character(labels)) {
    cli::cli_abort("{.arg {arg}} must be a named character vector.", call = NULL)
  }
  nm <- names(labels)
  if (length(labels) && (is.null(nm) || anyNA(nm) || any(!nzchar(nm)))) {
    cli::cli_abort("Every element of {.arg {arg}} needs a variable name.", call = NULL)
  }
  if (anyDuplicated(nm)) {
    cli::cli_abort("{.arg {arg}} names {.var {unique(nm[duplicated(nm)])}} more than once.", call = NULL)
  }
  if (anyNA(labels)) {
    cli::cli_abort(c("{.arg {arg}} holds a missing label for {.var {nm[is.na(labels)]}}.",
                     "i" = "Give a string, or leave the variable out to keep its own label."),
                   call = NULL)
  }
  labels
}

#' Variable label with Stata's fallback
#'
#' @param x A vector.
#' @param name The variable name, used when `x` carries no label.
#' @param labels Optional named character vector overriding labels by name.
#' @return A single string.
#' @keywords internal
#' @noRd
var_label <- function(x, name, labels = NULL) {
  if (!is.null(labels) && !is.null(names(labels)) && name %in% names(labels)) {
    return(unname(as.character(labels[[name]])))
  }
  lab <- attr(x, "label", exact = TRUE)
  if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) lab else name
}

#' Value labels of a vector as a named vector of codes
#'
#' @param x A vector.
#' @return Named vector (names = labels, values = codes), or `NULL`.
#' @keywords internal
#' @noRd
value_labels <- function(x) {
  lab <- attr(x, "labels", exact = TRUE)
  if (is.null(lab) || !length(lab)) NULL else lab
}

# Text Stata prints for an unlabelled numeric code (`: label (var) 3` -> "3",
# 100000 -> "100000", 0.1 -> ".1"): the levelsof macro text.
.code_text <- function(code) stata_macro_text(code)

#' Observed levels in Stata order, with display labels
#'
#' Numeric and haven_labelled vectors: ascending code order, labelled codes
#' show their value label and unlabelled codes show the code. Character
#' vectors: sorted in the C locale, as Stata's `encode` orders them. Factors
#' keep their level order (an R-only case with no Stata equivalent).
#' Logical vectors are treated as 0/1. Only levels present in `x` are
#' returned unless `drop = FALSE` (factors).
#'
#' @param x A vector.
#' @param drop Drop factor levels that do not occur.
#' @return data.frame with columns `code` (the value as stored in `x`,
#'   character for character/factor input) and `label`.
#' @keywords internal
#' @noRd
level_labels <- function(x, drop = TRUE) {
  if (is.factor(x)) {
    lev <- levels(x)
    if (drop) lev <- lev[lev %in% as.character(x[!is.na(x)])]
    return(data.frame(code = lev, label = lev, stringsAsFactors = FALSE))
  }
  if (is.character(x)) {
    vl <- value_labels(x)
    v <- unclass(x)
    attributes(v) <- NULL
    u <- unique(v[!is.na(v)])
    u <- u[order(u, method = "radix")]
    lab <- u
    # haven character-labelled vectors (R-only): labelled codes show labels.
    if (!is.null(vl)) {
      hit <- match(u, unname(unclass(vl)))
      lab[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
    }
    return(data.frame(code = u, label = lab, stringsAsFactors = FALSE))
  }
  if (is.logical(x)) x <- as.integer(x)
  vl <- value_labels(x)
  v <- unclass(x)
  attributes(v) <- NULL
  u <- sort(unique(v[!is.na(v)]))
  lab <- .code_text(u)
  if (!is.null(vl)) {
    hit <- match(u, unname(unclass(vl)))
    lab[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
  }
  data.frame(code = u, label = lab, stringsAsFactors = FALSE)
}

#' Integer level index of each value of `x`, in `level_labels()` order
#'
#' @param x A vector.
#' @param levels Output of `level_labels(x)`.
#' @return Integer vector, `NA` for missing values.
#' @keywords internal
#' @noRd
level_index <- function(x, levels = level_labels(x)) {
  if (is.factor(x)) return(match(as.character(x), levels$code))
  if (is.logical(x)) x <- as.integer(x)
  v <- unclass(x)
  attributes(v) <- NULL
  match(v, levels$code)
}

#' Convert Stata-labelled columns to factors that keep the Stata codes
#'
#' `haven::read_dta()` returns value-labelled variables as `haven_labelled`
#' vectors. [regtab()] needs factors for categorical predictors, and keys
#' their levels by the Stata code (`0.foreign`, `2.rep78`) in native
#' interaction rows and in `keep`/`drop` terms. `haven::as_factor()` and
#' `labelled::to_factor()` drop the codes, so a 0/1-coded `foreign` would be
#' keyed by level position (`1.foreign`, `2.foreign`). `tt_as_factor()`
#' makes the same factor as `haven::as_factor(x, levels = "default")`
#' (levels in ascending code order, each showing its value label, or the
#' code when unlabelled) and keeps the codes in the `"labels"` attribute and
#' the variable label in `"label"`.
#'
#' @param x A data frame, or a single vector.
#' @param vars Columns to convert (data frame only). Default: every
#'   `haven_labelled` column. A named column without value labels becomes a
#'   factor of its sorted values (Stata's unlabelled `i.var`). Name the
#'   categorical predictors here rather than relying on the default when
#'   the data hold value-labelled outcomes: an event indicator or a binary
#'   outcome must stay numeric. A 0/1 `died` labelled No/Yes converted to a
#'   factor makes `survival::coxph(Surv(time, died) ~ ...)` fit a
#'   multi-state model (it stops asking for an `id`), and `glm()` reads a
#'   factor response by its level order.
#' @return `x` with the columns converted (or the converted vector).
#' @examples
#' x <- structure(c(0, 1, 1, 0), labels = c(Domestic = 0, Foreign = 1),
#'                label = "Car origin", class = c("haven_labelled", "vctrs_vctr", "double"))
#' f <- tt_as_factor(x)
#' levels(f)
#' attr(f, "labels")
#' # In a data frame, convert the categorical predictors only; keep a
#' # value-labelled event indicator numeric:
#' # d <- tt_as_factor(haven::read_dta("cancer.dta"), vars = "drug")
#' @export
tt_as_factor <- function(x, vars = NULL) {
  if (is.data.frame(x)) {
    if (is.null(vars)) vars <- names(x)[vapply(x, function(col) inherits(col, "haven_labelled"), TRUE)]
    if (!is.character(vars) || anyNA(vars) || any(!nzchar(vars))) {
      cli::cli_abort("{.arg vars} must be a character vector of non-empty column names.", call = NULL)
    }
    bad <- setdiff(vars, names(x))
    if (length(bad)) cli::cli_abort("{.arg vars} names unknown column{?s} {.val {bad}}.", call = NULL)
    .tt_check_unambiguous(x, vars, "vars")
    for (v in vars) x[[v]] <- .tt_as_factor1(x[[v]])
    return(x)
  }
  if (!is.null(vars)) cli::cli_abort("{.arg vars} applies to data frames only.", call = NULL)
  .tt_as_factor1(x)
}

.tt_as_factor1 <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.factor(x)) return(x)
  vl <- attr(x, "labels", exact = TRUE)
  v <- unclass(x)
  attributes(v) <- NULL
  if (is.null(vl) || !length(vl) || is.null(names(vl))) {
    f <- factor(v)
  } else {
    vlv <- unclass(vl)
    attributes(vlv) <- NULL
    codes <- sort(unique(c(vlv, v[!is.na(v)])))
    text <- if (is.numeric(codes)) stata_macro_text(codes) else as.character(codes)
    hit <- match(codes, vlv)
    text[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
    if (anyDuplicated(text)) {
      dup <- unique(text[duplicated(text)])
      cli::cli_abort("Value label{?s} {.val {dup}} name{?s/} more than one code; factor levels must be unique.",
                     call = NULL)
    }
    f <- factor(text[match(v, codes)], levels = text)
    if (is.numeric(vlv)) attr(f, "labels") <- stats::setNames(vlv, names(vl))
  }
  if (!is.null(lab)) attr(f, "label") <- lab
  f
}
