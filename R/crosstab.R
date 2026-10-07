#' Cross-tabulate two numeric categorical variables
#'
#' Counts, percentages and margins follow ascending underlying category codes.
#' For a two by two table the event is the second row level and the exposure is
#' the second column level. Display labels do not change that direction.
#'
#' @param data A data frame.
#' @param rowvar,colvar Single character column names. Columns must be numeric
#'   category vectors; factors, character codes and matrix columns are refused.
#' @param weights NULL, a numeric vector, or a frequency-weight column name.
#'   Nonnegative integer weights are required. Zero and missing weights do not
#'   contribute categories or counts. The expanded total must be below 2^53.
#' @param subset Logical vector with one entry per record (NA excludes), or
#'   positive integer row positions. Repeated positions select a record once.
#' @param colpct,rowpct,totalpct Mutually exclusive percentage modes. When all
#'   are FALSE, column percentages are used. Margins display counts only.
#' @param exact,fisher Force the exact Fisher test. Otherwise any expected
#'   count below 5 selects Fisher; all others select uncorrected Pearson.
#' @param or,rr,rd Report sample odds ratio, risk ratio or risk difference and
#'   confidence limits, respectively; these require two by two tables.
#' @param trend Spearman trend using frequency-expanded midranks and the
#'   installed Stata 17 t(N-2) inference convention. Positive perfect
#'   correlation has p=0 when N>2; negative-perfect or N<=2 inference is
#'   refused. This is distinct from the 2025 Stata manual's beta approximation.
#' @param cochran Cochran-Armitage trend for a binary row outcome using actual
#'   numeric column codes as scores. Positive affine recoding preserves the
#'   statistic; increasing scores and the second row event determine its sign.
#' @param label Use numeric value-label attributes for category text. Variable
#'   labels are used as descriptors independently of this switch.
#' @param missing Include ordinary and tagged missing categories. Both trend
#'   options refuse missing=TRUE, even if no missing values occur.
#' @param level Confidence level as a proportion in (0, 1) or a percentage in
#'   (1, 100), as in [ratetab()] and [survtab()]; `0.95` and `95` agree.
#' @param digits Decimal places 0 to 6; NULL uses the session default, else 1.
#' @param smallcells NULL or 0 disables inherited masking; an integer at least
#'   3 enables count protection. Omission inherits the session threshold.
#' @param smallcells_mode "strict" or "primary"; explicit NULL selects strict.
#'   An explicit active threshold defaults to strict. Omitted threshold and
#'   mode inherit together. Disabled masking defaults to strict unless the
#'   call explicitly selects primary.
#' @param nosmallcells Disable inherited count masking. Conflicts with an
#'   explicitly supplied non-NULL smallcells, including 0.
#' @param masktext Literal masking text, including an empty string. NULL uses
#'   threshold markers. An explicitly supplied string requires active masking.
#' @param xlsx,excel Workbook destinations. A nonempty xlsx wins; otherwise a
#'   nonempty excel is used. Only the selected alias is validated. Explicit
#'   NULL disables workbook inheritance when no other alias selects a path.
#' @param sheet Sheet name, default "Crosstab". An explicit non-NULL sheet
#'   activates ordinary session workbook/Markdown inheritance.
#' @param title,footnote Title and footnote paragraphs; see [tt_table()].
#' @param font,fontsize,borderstyle,headershade,headercolor,zebracolor,zebra
#'   House style controls; see [tabtools_options()].
#' @param boldp Bold ordinary and trend test rows below this p threshold.
#' @param csv,markdown Optional text destinations.
#' @param mdappend Append Markdown; omitted uses active session history.
#' @param open Open a successfully written workbook interactively.
#'
#' @details Fisher uses exact conditional probability-order inference, with
#'   no simulated fallback. The odds-ratio point estimate is the sample
#'   cross-product ratio, not Fisher's conditional maximum-likelihood estimate.
#'   With positive cells, its limits invert equal-tailed noncentral-hypergeometric
#'   Fisher tests using the pinned Stata 17 `cc` algorithm: Cornfield starting
#'   values and secant iteration stopping at a 0.5% successive-odds-ratio
#'   difference. These returned limits can differ from `stats::fisher.test()`'s
#'   root tolerance. As native `cc` does, a zero count selects the unadjusted
#'   Cornfield interval; unavailable limits remain missing.
#'   RR uses log-Wald variance `c/(a*N1)+d/(b*N0)`; RD uses independent-binomial
#'   variance `a*c/N1^3+b*d/N0^3`. Here a is the second-row/second-column count,
#'   b the second-row/first-column count, c the first-row/second-column count,
#'   and d the first-row/first-column count. Undefined requested point estimates
#'   or trend tests raise a classed error before writing any sink.
#'   R refuses a one-level row or column sample rather than returning an
#'   unusable ordinary test. Positive weights determine the observed R
#'   categories. The authenticated native XT018 literal excludes a zero-weight-only
#'   level and returns the same two by two table; no broader native domain claim
#'   is inferred from that case.
#'
#'   Strict protection certifies complementary count masking across all body
#'   counts and margins. When a primary count exists it also withholds tests,
#'   association estimates/limits and trend statistics. Primary protection
#'   masks only positive counts below the threshold and retains computed
#'   inference; released totals and tests may permit reconstruction. In BOTH
#'   modes protected returned counts and percentages are NA. Percentages depend
#'   on both numerator and the selected margin. No raw count backup is stored.
#'   Record-count/weight decompositions are unavailable when counts are masked;
#'   only a released grand margin remains in sample accounting.
#'
#'   Tables are unkeyed variable/correlation frames: [tt_flat()] requires
#'   keyed=FALSE. Model-key requests and semantic model composition are refused.
#'   Ordinary rendered data frames can be passed to [puttab()] or [stacktab()].
#'   Methods identify the R computational engine; native source provenance is
#'   stored separately. Native crosstab 2.5.1 primary counts are redacted while
#'   tests remain computed; its help has an inconsistent broad suppression
#'   sentence, so this implementation follows the actual ado source.
#'
#' @return A tt_table with stored$table (redacted count matrix), N, chi2, p,
#'   optional or/rr/rd and *_lo/*_hi, optional trend statistics, ci_level and
#'   methods. stored$smallcells is a typed list with threshold, mode, n_masked
#'   and n_linked. Active masks also return native full/primary mode and
#'   N_primary_suppressed, N_secondary_suppressed, N_derived_suppressed.
#'   meta$publication contains redacted margins/percentages and linked flags;
#'   meta$sample_accounting separates contributing records from frequency N.
#'   New boundary errors have class tabtools_error_crosstab_input, _weights,
#'   _sample, _association, _inference or _trend. Existing shared style, mask
#'   and sink diagnostics retain their classes. Writes return the table invisibly.
#' @references Armitage P (1955). Tests for Linear Trends in Proportions and
#'   Frequencies. Biometrics 11(3), 375-386. doi:10.2307/3001775.
#'   StataCorp (2025). Stata \[R\] Epitab, Methods and formulas, pp.53-55.
#'   Installed Stata 17 spearman.ado, version 4.2.8 (28feb2022), lines293-309.
#' @examples
#' d <- data.frame(outcome = c(0, 0, 1, 1), exposure = c(0, 1, 0, 1),
#'                 frequency = c(40, 20, 10, 30))
#' crosstab(d, "outcome", "exposure", weights = "frequency", or = TRUE,
#'          rr = TRUE, rd = TRUE)
#' crosstab(d, "outcome", "exposure", weights = "frequency", rowpct = TRUE)
#' @export
crosstab <- function(data, rowvar, colvar, weights = NULL, subset = NULL,
                     colpct = FALSE, rowpct = FALSE, totalpct = FALSE,
                     exact = FALSE, fisher = FALSE, or = FALSE, rr = FALSE, rd = FALSE,
                     trend = FALSE, cochran = FALSE, label = FALSE, missing = FALSE,
                     level = 95, digits = NULL, smallcells = NULL,
                     smallcells_mode = NULL, nosmallcells = FALSE, masktext = NULL,
                     xlsx = NULL, excel = NULL, sheet = "Crosstab", title = NULL,
                     footnote = NULL, font = NULL, fontsize = NULL, borderstyle = NULL,
                     headershade = FALSE, headercolor = NULL, zebracolor = NULL,
                     boldp = NULL, zebra = FALSE, csv = NULL, markdown = NULL,
                     mdappend = FALSE, open = FALSE) {
  for (name in c("colpct", "rowpct", "totalpct", "exact", "fisher", "or", "rr", "rd",
                 "trend", "cochran", "label", "missing", "headershade", "zebra", "open")) {
    .xt_flag(get(name), name)
  }
  if (sum(c(colpct, rowpct, totalpct)) > 1L) .xt_abort("Choose only one percentage mode.")
  if (trend && cochran) .xt_abort("trend and cochran are mutually exclusive.", "trend")
  if ((trend || cochran) && missing) .xt_abort("Trend options cannot include missing categories.", "trend")
  if (!is.numeric(level) || is.complex(level) || !is.null(dim(level)) ||
      length(level) != 1L || !is.finite(level) || level <= 0 || level >= 100 || level == 1) {
    .xt_abort("level must be a proportion in (0, 1) or a percentage in (1, 100).")
  }
  # The ratetab/survtab/stratetab convention: a proportion is a percentage/100.
  level <- .st_check_level(level)
  digits <- digits %||% getOption("tabtools.digits") %||% 1L
  if (!is.numeric(digits) || is.complex(digits) || !is.null(dim(digits)) ||
      length(digits) != 1L || !is.finite(digits) || digits != floor(digits) || digits < 0 || digits > 6) {
    .xt_abort("digits must be a whole number from 0 to 6.")
  }
  alias <- .xt_alias(xlsx, excel, !base::missing(xlsx), !base::missing(excel))
  sinks <- .tt_resolve_sinks(
    list(xlsx = alias$value, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, smallcells = smallcells, smallcells_mode = smallcells_mode,
         nosmallcells = nosmallcells, masktext = masktext),
    list(xlsx = alias$given, markdown = !base::missing(markdown),
         mdappend = !base::missing(mdappend), sheet = !base::missing(sheet),
         smallcells = !base::missing(smallcells), smallcells_mode = !base::missing(smallcells_mode),
         nosmallcells = !base::missing(nosmallcells), masktext = !base::missing(masktext)),
    policy = "sheet", mask = "crosstab")
  xlsx <- sinks$values$xlsx; markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend; k <- sinks$mask$threshold
  mode <- sinks$mask$mode; text <- sinks$mask$text
  sheet <- sheet %||% "Crosstab"
  if (open && is.null(xlsx)) .xt_abort("open requires a workbook destination.")
  style <- tt_resolve_style(font, fontsize, borderstyle, headershade, zebra,
                            headercolor, zebracolor, boldp = boldp)
  o <- list(exact = exact, fisher = fisher, or = or, rr = rr, rd = rd,
            trend = trend, cochran = cochran, level = level, digits = digits,
            percent = if (rowpct) "row" else if (totalpct) "total" else "column",
            smallcells = k, mode = mode, masktext = text)
  sample <- .xt_counts(data, rowvar, colvar, weights, subset, missing, label)
  sc <- .xt_masks(sample$counts, k, mode, o$percent)
  inference <- .xt_inference(sample$counts, sample$column$code, o)
  tt <- .xt_build(sample, inference, sc, o, style, sheet, title, footnote)
  tt$meta$sample_accounting <- .xt_ledger(sample$sample, sc, sample$weighted, rowvar, colvar)
  written <- FALSE
  if (!is.null(csv)) { tt_write_csv(tt, csv); written <- TRUE }
  if (!is.null(markdown)) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown; tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols"); written <- TRUE
  }
  if (!is.null(xlsx)) {
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx; tt$stored$sheet <- sheet; tt$meta$sheet <- sheet
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}
