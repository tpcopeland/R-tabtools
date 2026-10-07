#' Format a scalar or vector of table cells
#'
#' Construct text for a manually assembled table without writing a file.
#' Only scalar inputs recycle. Assign the result to keep the cell text and
#' its aligned analytical provenance; use `puttab()` to publish a column.
#'
#' @param form One of `"est"`, `"p"`, `"n"`, `"np"`, `"enp"`, `"iqr"`,
#'   or `"rate"`.
#' @param b,ll,ul,se Estimate, limits, or standard error. Explicit `b` and
#'   positive `se` use normal inference; supplied limits are preserved.
#'   A nonpositive SE is non-estimable and requires explicit missing text.
#' @param p,n,d,e,pt Numeric p-value, count, denominator, events, or exposure.
#' @param per Positive rate multiplier, required for `"rate"`.
#' @param median,q1,q3 Supplied median and quartiles; no sample quantiles
#'   are estimated.
#' @param model A fitted model supported by `regtab()`, selected with `term`.
#' @param term Exact fitted coefficient name or named contrast row.
#' @param matrix Numeric matrix of estimates and limits.
#' @param row One exact matrix row name or integer row index.
#' @param cols Three column names or indices for estimate, lower, upper;
#'   defaults to `c("b", "ll", "ul")`.
#' @param contrast A supplied list or data frame with `estimate` and either
#'   `conf.low`/`conf.high` or `std.error`. Declare `df` (Inf for normal),
#'   `conf.level`, and `effect_scale` (`"coefficient"` or `"ratio"`).
#'   Recomputing ratio intervals additionally requires `se_scale = "log"`.
#'   Supplied inference and its reference distribution remain in provenance.
#'   Optional `native_source = "lincom"` declares a linear contrast and
#'   additionally requires `std.error`. Original estimate, SE, limits, level,
#'   statistic and p-value are projected before publication transformations;
#'   conflicting return names use `lincom_` prefixes. A normal reference has
#'   `z` and no `df` scalar; a t reference has `t` and `df`. Ratio inputs use
#'   a log-scale SE, with the original native SE projected by the delta method.
#'   `native_source = "nlcom"` requires coefficient scale, `std.error` and
#'   `df = Inf`; it returns publication scalars only. Supplied statistics
#'   must agree with the declared inference. Undeclared contrasts acquire
#'   neither native subtype by inference from their fields or a fitted model.
#' @param eform Exponentiate coefficient-scale estimates and limits once.
#' @param scale Positive multiplier applied after exponentiation (`"est"`).
#' @param format,cformat Supported Stata numeric display format aliases.
#' @param digits Decimal places, 0 to 10; conflicts with a supplied format.
#' @param sep Literal interval separator. Empty selects comma-space.
#' @param level Confidence probability strictly between 0 and 1. Applied
#'   to derived intervals; supplied intervals cannot be relabeled.
#' @param ci For `"np"`, `"exact"` requests Clopper-Pearson limits. For
#'   `"rate"`, `"exact"` (default) or `"poisson"` (log-rate Wald).
#' @param nocount Publish only the percentage for `"np"`.
#' @param mincell Nonnegative whole threshold. Counts from 1 to threshold-1
#'   are masked. Default 0 does not inherit session suppression.
#' @param missing Replacement for non-finite or noncomputable inputs.
#'   Omitted refuses them; explicit `""` creates an empty cell. Finite
#'   domain errors and numerical interval failures remain errors.
#' @param nformat,pformat Count and percentage formats.
#' @param pdp,highpdp P-value decimals below and at/above 0.10.
#' @param pstyle `"table"`, `"footnote"`, or `"Pfootnote"`.
#' @return A character vector of class `tt_cell`, with an aligned
#'   `provenance` attribute containing states, publication numerics, source
#'   inputs, inference, native scalar projections, formats and mask counts.
#'   Protected leaf counts and reconstructive percentage/rate companions
#'   are redacted. This differs from the raw matrices of rate/outcome tables.
#'   `as.character()` deliberately drops semantic provenance.
#' @details Exact binomial limits invert equal-tailed binomial tests and
#'   assume independent Bernoulli trials. Exact rate limits invert Poisson
#'   tails; log-rate Wald uses rate times exp(plus/minus z/sqrt(events)).
#'   Both rate methods use exact limits at zero events. A rate assumes
#'   independent Poisson events and a constant cell rate. Positive events
#'   require positive exposure; exact methods require whole counts.
#'
#' Decimal-comma formats with a comma interval separator are refused by
#' the shared R formatting contract. Native Stata warns for that combination.
#' Cell construction never changes destinations or the session write registry.
#' @references Thulin M (2014). The cost of using exact confidence intervals
#'   for a binomial proportion. Electronic Journal of Statistics 8:817-840.
#'   doi:10.1214/14-ejs909.
#'
#'   StataCorp (2025). Stata `[R] ci`, Methods and formulas, Binomial
#'   proportion and Poisson mean; `[ST] strate`, Remarks.
#' @examples
#' tabcell("est", b = -2, ll = -3, ul = -1, sep = "-")
#' tabcell("np", n = c(0, 3, 10), d = 20, ci = "exact")
#' tabcell("rate", e = c(0, 12), pt = 1000, per = 1000)
#' cells <- tabcell("n", n = c(12345, 3), mincell = 5)
#' as.character(cells)
#' @export
tabcell <- function(form, b = NULL, ll = NULL, ul = NULL, se = NULL,
                    p = NULL, n = NULL, d = NULL, e = NULL, pt = NULL,
                    per = NULL, median = NULL, q1 = NULL, q3 = NULL,
                    model = NULL, term = NULL, matrix = NULL, row = NULL,
                    cols = NULL, contrast = NULL, eform = FALSE, scale = 1,
                    format = NULL, cformat = NULL, digits = NULL, sep = ", ",
                    level = 0.95, ci = NULL, nocount = FALSE, mincell = 0L,
                    missing = NULL, nformat = "%12.0fc", pformat = "%4.1f",
                    pdp = 3L, highpdp = 2L, pstyle = "table") {
  given <- names(as.list(match.call()))[-1L]
  missing_given <- !missing(missing)
  if (!is.character(form) || length(form) != 1L || is.na(form) ||
      !form %in% c("est", "p", "n", "np", "enp", "iqr", "rate")) {
    .tc_abort("{.arg form} must name one of the seven cell forms exactly.", "form")
  }
  allowed <- switch(form,
    est = c("b", "ll", "ul", "se", "model", "term", "matrix", "row",
            "cols", "contrast", "eform", "scale", "format", "cformat", "digits", "sep", "level"),
    p = c("p", "pdp", "highpdp", "pstyle"),
    n = c("n", "nformat", "mincell"),
    np = c("n", "d", "ci", "level", "sep", "nocount", "nformat", "pformat", "mincell"),
    enp = c("e", "n", "nformat", "pformat", "mincell"),
    iqr = c("median", "q1", "q3", "format", "cformat", "digits", "sep"),
    rate = c("e", "pt", "per", "ci", "level", "format", "cformat", "digits", "sep", "mincell"))
  irrelevant <- setdiff(given, c("form", "missing", allowed))
  if (length(irrelevant)) .tc_abort(paste0("Options do not apply to this form: ",
                                         paste(irrelevant, collapse = ", "), "."), "form")
  if (missing_given && (!is.character(missing) || length(missing) != 1L || is.na(missing))) {
    .tc_abort("{.arg missing} must be one literal string.", "missing")
  }
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    .tc_abort("{.arg level} must lie strictly between 0 and 1.")
  }
  if (!is.numeric(mincell) || length(mincell) != 1L || !is.finite(mincell) ||
      mincell < 0 || mincell > .Machine$integer.max || mincell != floor(mincell)) {
    .tc_abort("{.arg mincell} must be a nonnegative whole threshold.")
  }
  for (flag in c("eform", "nocount")) {
    value <- get(flag, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .tc_abort(paste0(flag, " must be TRUE or FALSE."))
    }
  }
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0) {
    .tc_abort("{.arg scale} must be one positive finite multiplier.")
  }
  if (form == "rate" && (!is.numeric(per) || length(per) != 1L || !is.finite(per) || per <= 0)) {
    .tc_abort("{.arg per} must be one positive finite rate multiplier.")
  }
  if (form == "np" && !is.null(ci) && !identical(ci, "exact")) {
    .tc_abort("The percentage interval method must be {.val exact}.")
  }
  if (form == "np" && is.null(ci) && any(c("level", "sep") %in% given)) {
    .tc_abort("Percentage {.arg level} and {.arg sep} require {.arg ci}.", "form")
  }
  if (form == "rate") {
    ci <- ci %||% "exact"
    if (!is.character(ci) || length(ci) != 1L || is.na(ci) || !ci %in% c("exact", "poisson")) {
      .tc_abort("The rate method must be {.val exact} or {.val poisson}.")
    }
  }
  if (!is.character(pstyle) || length(pstyle) != 1L || is.na(pstyle) ||
      !tolower(pstyle) %in% c("table", "footnote", "pfootnote")) {
    .tc_abort("Unknown p-value style.")
  }
  pstyle <- if (tolower(pstyle) == "pfootnote") "Pfootnote" else tolower(pstyle)
  .check_dp(pdp, "pdp")
  .check_dp(highpdp, "highpdp")
  if (!is.null(format) && !is.null(cformat)) {
    .tc_abort("{.arg format} and {.arg cformat} are aliases; supply one.", "format")
  }
  numeric_format <- .tt_resolve_numeric_format(cformat = format %||% cformat,
    digits = digits, digits_given = "digits" %in% given, sep = sep,
    default_digits = if (form == "rate") 1L else 2L)
  count_format <- .tt_resolve_numeric_format(cformat = nformat, sep = " ")
  percent_format <- .tt_resolve_numeric_format(cformat = pformat,
    sep = if (form == "np" && !is.null(ci)) numeric_format$sep else " ")
  source <- list(type = "explicit", term = NA_character_, reference = "none",
                 df = NA_real_, level = NA_real_, effect_scale = "coefficient",
                 se_scale = NA_character_, inference = NULL)
  if (form == "est") {
    extracted <- .tc_est_source(b, ll, ul, se, model, term, matrix, row, cols,
                                contrast, level, "level" %in% given)
    values <- extracted$values
    source <- extracted$source
    if (eform && identical(source$effect_scale, "ratio")) {
      .tc_abort("Ratio-scale inputs cannot be exponentiated twice.", "source")
    }
    if (eform) values <- lapply(values, exp)
    values <- lapply(values, function(x) x * scale)
    # Finite inputs producing overflow are computational failures, rather
    # than ordinary input missingness that replacement text could rescue.
    before <- extracted$values
    if (any(vapply(seq_along(values), function(i)
      any(is.finite(before[[i]]) & !is.finite(values[[i]])), logical(1)))) {
      .tc_abort("Exponentiation or scaling produced non-finite values.", "interval")
    }
  } else {
    values <- switch(form, p = list(p = p), n = list(n = n), np = list(n = n, d = d),
      enp = list(e = e, n = n), iqr = list(median = median, q1 = q1, q3 = q3),
      rate = list(e = e, pt = pt))
  }
  rendered <- .tc_render(form, values, numeric_format, count_format,
    percent_format, level, ci, as.integer(mincell), nocount, per,
    missing_text = missing, missing_given = missing_given,
    pdp = pdp, highpdp = highpdp, pstyle = pstyle)
  size <- length(rendered$text)
  rows <- data.frame(source_id = rep(.tt_flat_id(), size),
    occurrence_id = seq_len(size), source_index = seq_len(size),
    form = rep(form, size), source = rep(source$type, size), term = rep(source$term, size),
    native_source = rep(source$native_source %||% NA_character_, size),
    raw_status = rendered$raw_status, status = rendered$status,
    missing = rendered$missing, missing_reason = rendered$missing_reason,
    mask_reason = rendered$mask_reason, method = rep(ci %||% source$reference, size),
    reference = rep(source$reference, size), df = rep(source$df, size),
    conf.level = rep(if (form %in% c("np", "rate") && !is.null(ci)) level else source$level, size),
    effect_scale = rep(if (eform) "ratio" else source$effect_scale, size),
    se_scale = rep(source$se_scale, size), eform_applied = rep(eform, size),
    scale = rep(if (form == "est") scale else NA_real_, size),
    per = rep(if (form == "rate") per else NA_real_, size), stringsAsFactors = FALSE)
  native_returns <- lapply(seq_len(size), function(i) {
    out <- list(missing = as.integer(rendered$missing[i]))
    if (form == "est") {
      out <- c(out, list(estimate = rendered$raw_inputs$b[i],
                        lb = rendered$raw_inputs$ll[i], ub = rendered$raw_inputs$ul[i]))
      if (!is.na(source$level)) out$level <- source$level * 100
      if ("scale" %in% given) out$scale <- scale
      if (identical(source$native_source, "lincom")) out <- c(out, source$native_returns)
    } else if (form == "rate") {
      out <- c(out, list(rate = rendered$publication$estimate[i],
        lb = rendered$publication$lb[i], ub = rendered$publication$ub[i],
        level = level * 100, per = per))
    } else if (form == "np" && !is.null(ci)) {
      out <- c(out, list(pct = rendered$publication$pct[i],
        lb = rendered$publication$lb[i], ub = rendered$publication$ub[i], level = level * 100))
    }
    out
  })
  inference <- lapply(seq_len(size), function(i) {
    original <- source$inference
    if (is.null(original)) return(NULL)
    if (identical(source$type, "explicit")) {
      return(lapply(original, function(x) x[i]))
    }
    original
  })
  provenance <- list(text = rendered$text, rows = rows, publication = rendered$publication,
    raw_inputs = rendered$raw_inputs, inference = inference,
    native_returns = native_returns, smallcells = rendered$smallcells,
    format = list(numeric = numeric_format, count = count_format,
                  percent = percent_format, pdp = pdp, highpdp = highpdp, pstyle = pstyle),
    counts = rendered$counts)
  out <- structure(rendered$text, provenance = provenance, class = c("tt_cell", "character"))
  .tc_validate(out)
  out
}

.tc_validate <- function(x) {
  if (!inherits(x, "tt_cell") || !is.character(x) || !length(x) || anyNA(x)) {
    .tc_abort("A cell vector must contain nonmissing character text.", "provenance")
  }
  provenance <- attr(x, "provenance", exact = TRUE)
  text <- x
  attributes(text) <- NULL
  if (!is.list(provenance) || !is.data.frame(provenance$rows) ||
      !is.data.frame(provenance$publication) || !is.data.frame(provenance$raw_inputs) ||
      nrow(provenance$rows) != length(x) || nrow(provenance$publication) != length(x) ||
      nrow(provenance$raw_inputs) != length(x) ||
      length(provenance$native_returns) != length(x) || length(provenance$inference) != length(x) ||
      !identical(provenance$text, text)) {
    .tc_abort("Cell text and analytical provenance are not aligned.", "provenance")
  }
  if (!all(provenance$rows$status %in% c("est", "ref", "omit", "notest", "absent",
                                        "empty", "constrained", "masked"))) {
    .tc_abort("The cell vector contains an unknown state.", "provenance")
  }
  required_rows <- c("source_id", "occurrence_id", "source_index", "form", "source", "term",
                    "raw_status", "status", "missing", "missing_reason", "mask_reason")
  required_publication <- c("estimate", "lb", "ub", "p", "count", "denominator",
                            "events", "exposure", "pct", "median", "q1", "q3")
  if (any(!required_rows %in% names(provenance$rows)) ||
      !identical(names(provenance$publication), required_publication) ||
      !all(vapply(provenance$publication, is.numeric, logical(1))) ||
      !is.logical(provenance$rows$missing) || anyNA(provenance$rows$missing)) {
    .tc_abort("The cell provenance schema is incomplete.", "provenance")
  }
  masked <- provenance$rows$status == "masked"
  if (any(masked)) {
    protected <- provenance$publication[masked, , drop = FALSE]
    event_only <- provenance$rows$form[masked] == "enp" & provenance$rows$mask_reason[masked] == "count"
    protected$denominator[event_only] <- NA_real_
    total_mask <- masked & provenance$rows$form == "enp" & provenance$rows$mask_reason == "total"
    exposed_total <- any(total_mask) &&
      (!"n" %in% names(provenance$raw_inputs) || any(!is.na(provenance$raw_inputs$n[total_mask])))
    if (exposed_total || any(!is.na(as.matrix(protected))) || any(!is.na(provenance$raw_inputs[masked, 1L]))) {
      .tc_abort("Protected leaf numerics cannot be exposed in provenance.", "provenance")
    }
  }
  if (any(!is.na(as.matrix(provenance$publication[provenance$rows$missing, , drop = FALSE])))) {
    .tc_abort("Missing cells cannot retain publication numerics.", "provenance")
  }
  invisible(x)
}

#' @rdname tabcell
#' @param x A `tt_cell` vector.
#' @param i Indices selecting or reordering complete cell occurrences.
#' @param ... Unused; additional arguments are refused.
#' @export
`[.tt_cell` <- function(x, i, ...) {
  if (length(list(...))) .tc_abort("Cell subsetting accepts one index.", "provenance")
  .tc_validate(x)
  indices <- if (missing(i)) seq_along(x) else seq_along(x)[i]
  if (anyNA(indices) || !length(indices)) {
    .tc_abort("Cell selection must retain at least one existing occurrence.", "provenance")
  }
  provenance <- attr(x, "provenance", exact = TRUE)
  provenance$text <- provenance$text[indices]
  for (field in c("rows", "publication", "raw_inputs")) {
    provenance[[field]] <- provenance[[field]][indices, , drop = FALSE]
  }
  provenance$native_returns <- provenance$native_returns[indices]
  provenance$inference <- provenance$inference[indices]
  provenance$counts <- list(N = as.integer(sum(!provenance$rows$missing)),
    N_missing = as.integer(sum(provenance$rows$missing)), N_selected = as.integer(length(indices)))
  provenance$smallcells$n_masked <- as.integer(sum(provenance$rows$status == "masked"))
  out <- structure(as.character(x)[indices], provenance = provenance, class = class(x))
  .tc_validate(out)
  out
}

#' @rdname tabcell
#' @export
as.character.tt_cell <- function(x, ...) {
  if (length(list(...))) .tc_abort("Text conversion does not accept extra arguments.", "provenance")
  .tc_validate(x)
  attributes(x) <- NULL
  x
}

#' @rdname tabcell
#' @export
print.tt_cell <- function(x, ...) {
  print(as.character(x), ...)
  invisible(x)
}

#' @rdname tabcell
#' @param row.names Optional data-frame row names.
#' @param optional,stringsAsFactors Logical compatibility arguments. The column
#'   is named `cell` and retains its character cell class.
#' @export
as.data.frame.tt_cell <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE) {
  if (length(list(...))) .tc_abort("Cell data-frame conversion does not accept extra arguments.", "provenance")
  .tc_validate(x)
  for (argument in c("optional", "stringsAsFactors")) {
    value <- get(argument, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .tc_abort(paste0(argument, " must be TRUE or FALSE."), "provenance")
    }
  }
  if (!is.null(row.names) && (length(row.names) != length(x) || anyNA(row.names) || anyDuplicated(row.names))) {
    .tc_abort("Data-frame row names must be complete and unique.", "provenance")
  }
  out <- list(x)
  if (!optional) names(out) <- "cell"
  class(out) <- "data.frame"
  attr(out, "row.names") <- if (is.null(row.names)) .set_row_names(length(x)) else row.names
  out
}
