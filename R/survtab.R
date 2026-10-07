#' Kaplan-Meier survival, restricted means and log-rank summaries
#'
#' Compute requested-time survival probabilities, risk counts, optional median
#' and restricted mean survival time (RMST), and a grouped log-rank comparison.
#' The complement selected by `reverse` is one minus KM, not a competing-risk
#' estimator. Group contrasts are always the first minus the second group.
#'
#' @param data A data frame of independent subjects or nonoverlapping intervals.
#' @param time,event Exact column names: exit time and binary numeric/logical
#'   failure indicator. Missing event is retained as censoring, as in stset.
#' @param times Positive finite numeric requested times, retaining their order
#'   and repeated occurrences. These do not restrict the log-rank sample.
#' @param by Optional column of numeric group codes or literal strings. Numeric
#'   codes sort ascending; strings sort lexically. Value labels affect display
#'   only. At least two included groups are required when `by` is supplied.
#' @param entry NULL for zero entry, a nonnegative scalar, or an entry column.
#'   Risk membership is strictly entry < time <= exit.
#' @param id Optional subject-ID column for delayed entry or split intervals.
#'   Without ID each row is an independent subject. IDs with overlapping
#'   intervals, recurrent failures, records after failure, changing groups or
#'   changing frequency refuse with `tabtools_error_survival_inference`.
#' @param fweight Optional column of nonnegative integer frequency counts;
#'   positive counts represent expanded independent subjects. No probability,
#'   importance or arbitrary case-weight inference is accepted.
#' @param rmst Optional positive RMST horizon. It cannot exceed any included
#'   group's last exit, including a group whose survival already reached zero.
#' @param median,riskset,reverse,difference,events Logical native options. Median
#'   uses first single-precision half-probability crossing, without the R
#'   quantile plateau midpoint. Declared fweights give missing medians, matching
#'   the native captured stci refusal. Difference needs exactly two groups.
#' @param timeunit Label unit: years, months, days or weeks; no time rescaling.
#' @param level CI proportion or percentage, default 95 percent.
#' @param addrow Optional data frame with exactly one column per publication
#'   column. Cells must be nonmissing strings; rows are literal annotations.
#' @param digits Decimal places (0--6), default session digits or 1.
#' @param pdp,highpdp P-value decimal places (1--10), default 3 and 2.
#' @param title,footnote Literal title and annotation paragraphs. Automatic
#'   reverse and beyond-support notes are separate paragraphs in every sink.
#' @param xlsx,excel Workbook destinations: non-NULL xlsx wins; excel is its
#'   fallback. Only the chosen alias is validated. Explicit NULL opts out.
#' @param sheet Worksheet name, default Survival. Only an explicitly supplied
#'   non-NULL sheet activates omitted session destinations.
#' @param csv,markdown Optional CSV and Markdown paths.
#' @param mdappend Append Markdown; inherited destinations use session history.
#' @param open Open a successfully written workbook in interactive sessions.
#' @param font,fontsize,borderstyle,headershade,headercolor,zebra,zebracolor
#'   House-style settings; the native survtab one-header geometry is used.
#' @param boldp,highlight Log-rank p thresholds for workbook emphasis.
#'
#' @details Missing time/entry/ID/group/frequency, nonpositive intervals and zero
#'   frequency are excluded with original row IDs and reasons. Negative entry,
#'   nonbinary events, invalid frequency and infinities refuse. Group-missing
#'   failed rows cannot change the included groups' no-failure classification.
#'
#'   KM uses product-limit event/risk ratios with exact double event times,
#'   Greenwood standard errors and log-log pointwise bands. RMST integrates
#'   survival from zero and uses the squared remaining-tail Greenwood variance.
#'   A terminal all-risk-set failure contributes zero restricted-tail variance.
#'   At zero survival the mathematical SE/bands are missing. Native queried
#'   SE follows sts generate: missing SE is forward-filled from a previous
#'   eligible value; a first terminal failure has no previous SE. This queried
#'   SE is used only for the native requested-probability contrast. The event
#'   grid and RMST covariance retain their mathematical values.
#'   This is the native independent-subject Greenwood method, including supported
#'   delayed entry and split intervals; supplying ID does not imply robust IJ
#'   or recurrent-event inference. For delayed entry, the initialized survival
#'   before first entry is a native integration convention, not evidence of
#'   unconditional survival. Independence and noninformative censoring/truncation
#'   are assumptions. Two-arm Wald variance is the sum of the arm variances.
#'
#'   Requested KM times beyond support retain final estimates with explicit
#'   flags/notes; RMST extrapolation refuses before export. The log-rank test
#'   uses the full included follow-up, native hypergeometric covariance and
#'   group-count-minus-one degrees of freedom. Only no included failures gives
#'   missing grouped test results and the exact diagnostic body row.
#'
#' @return A `tt_table` with command survtab, invisible after export. `stored$table`
#'   is the requested-time probability matrix, excluding other summaries.
#'   Native per-group scalar families and original identities are retained;
#'   `meta$survival` holds exact curves, queried SE/bands/risk, support flags,
#'   variance/confidence provenance and original row/subject accounting.
#'   The native frame is an unkeyed rendered frame; an explicit keyed request
#'   is unsupported. No fictitious fitted-model identifiers are assigned.
#' @references StataCorp (2025). Survival Analysis Reference Manual, sts,
#'   Methods and formulas (KM/Greenwood); stci (median and RMST estimand).
#'   The rendered stci RMST SE equation differs from the implemented squared-tail
#'   formula; the latter is corroborated by survtab 2.5.1 Mata1219--1258 and
#'   stci 7.4.3 software467--480.
#'   Royston P, Parmar MKB (2013). Restricted mean survival time: an alternative
#'   to the hazard ratio for the design and analysis of randomized trials with
#'   a time-to-event outcome. BMC Medical Research Methodology 13, 152.
#'   doi:10.1186/1471-2288-13-152 (estimand and independent-arm contrast).
#' @examples
#' d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0))
#' tab <- survtab(d, time = "exit", event = "event", times = c(1, 2, 3),
#'                rmst = 3, riskset = TRUE, events = TRUE)
#' tab$stored$rmst_1
#' @export
survtab <- function(data, time, event, times, by = NULL, entry = NULL, id = NULL,
                    fweight = NULL, rmst = NULL, median = FALSE, riskset = FALSE,
                    timeunit = "years", reverse = FALSE, difference = FALSE, events = FALSE,
                    level = NULL, addrow = NULL, digits = NULL, pdp = 3L, highpdp = 2L,
                    title = NULL, footnote = NULL, xlsx = NULL, excel = NULL,
                    sheet = "Survival", csv = NULL, markdown = NULL, mdappend = FALSE,
                    open = FALSE, font = NULL, fontsize = NULL, borderstyle = NULL,
                    headershade = FALSE, headercolor = NULL, zebra = FALSE,
                    zebracolor = NULL, boldp = NULL, highlight = NULL) {
  for (name in c("median", "riskset", "reverse", "difference", "events", "headershade", "zebra", "open")) {
    value <- get(name)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) .sv_abort(paste0(name, " must be TRUE or FALSE."))
  }
  if (!is.numeric(times) || is.complex(times) || !is.null(dim(times)) || !length(times) ||
      any(!is.finite(times) | times <= 0)) .sv_abort("times must contain positive finite numeric values.")
  times <- as.numeric(times)
  if (!is.character(timeunit) || length(timeunit) != 1L || is.na(timeunit) ||
      !timeunit %in% c("years", "months", "days", "weeks")) .sv_abort("timeunit must be years, months, days or weeks.")
  if (!is.null(rmst) && (!is.numeric(rmst) || is.complex(rmst) || !is.null(dim(rmst)) ||
      length(rmst) != 1L || !is.finite(rmst) || rmst <= 0)) .sv_abort("rmst must be one positive finite horizon.")
  level <- .rate_check_level(level %||% 0.95) * 100
  numeric_format <- .tt_resolve_numeric_format(digits = digits, default_digits = 1L, max_digits = 6L)
  digits <- numeric_format$digits
  pdp <- .check_int_range(pdp, "pdp", 1L, 10L)
  highpdp <- .check_int_range(highpdp, "highpdp", 1L, 10L)
  .tt_check_text_arg(title, "title")
  .tt_check_footnote_arg(footnote)
  chosen <- if (!is.null(xlsx)) xlsx else excel
  sinks <- .tt_resolve_sinks(list(xlsx = chosen, csv = csv, markdown = markdown,
    sheet = sheet, mdappend = mdappend),
    list(xlsx = !missing(xlsx) || !missing(excel), csv = !missing(csv), markdown = !missing(markdown),
      sheet = !missing(sheet), mdappend = !missing(mdappend)), policy = "sheet", mask = "none")
  xlsx <- sinks$values$xlsx; markdown <- sinks$values$markdown; mdappend <- sinks$values$mdappend
  if (open && is.null(xlsx)) .sv_abort("open requires a workbook destination.")
  if (mdappend && is.null(markdown)) .sv_abort("mdappend requires a Markdown destination.")
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
    headershade = headershade, headercolor = headercolor, zebra = zebra, zebracolor = zebracolor,
    boldp = boldp, highlight = highlight)
  prepared <- .sv_prepare(data, time, event, by, entry, id, fweight)
  G <- length(prepared$N)
  if (difference && (is.null(by) || G != 2L)) .sv_abort("difference requires exactly two included by groups.")
  if (!is.null(rmst) && any(rmst > prepared$support)) {
    .sv_abort("rmst exceeds observed follow-up support in an included group.", "tabtools_error_survival_support")
  }
  records <- lapply(seq_len(G), function(g) prepared$records[prepared$records$group == g, , drop = FALSE])
  curves <- lapply(records, .sv_curve, level = level)
  queries <- lapply(seq_len(G), function(g) .sv_query(records[[g]], curves[[g]], times,
                                                    prepared$support[g], reverse))
  medians <- if (median) lapply(seq_len(G), function(g) .sv_median(curves[[g]], prepared$declared_fweight, prepared$N[g])) else NULL
  restricted <- if (!is.null(rmst)) lapply(curves, .sv_rmst, tau = rmst, level = level) else NULL
  contrast <- if (!is.null(restricted) && !is.null(by) && G == 2L) .sv_contrast(
    vapply(restricted, `[[`, 0, "estimate"), vapply(restricted, `[[`, 0, "se"), level) else NULL
  logrank <- .sv_logrank(prepared$records, if (is.null(by)) 1L else G)
  tt <- .sv_publication(prepared, curves, queries, medians, restricted, contrast, logrank,
    times, rmst, median, riskset, reverse, difference, events, timeunit, level,
    addrow, numeric_format, pdp, highpdp, title, footnote, style, sheet %||% "Survival")
  .sv_export(tt, xlsx, csv, markdown, sheet %||% "Survival", mdappend, open)
}

.sv_export <- function(tt, xlsx, csv, markdown, sheet, mdappend, open) {
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    tt$stored$csv <- csv
  }
  if (!is.null(markdown)) {
    output <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(output, "n_rows")
    tt$stored$markdown_cols <- attr(output, "n_cols")
  }
  if (!is.null(xlsx)) {
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx; tt$stored$sheet <- sheet
    tt$meta$sheet <- sheet
  }
  if (any(!vapply(list(xlsx, csv, markdown), is.null, TRUE))) invisible(tt) else tt
}
