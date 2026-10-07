#' Pairwise Pearson and Spearman correlation tables
#'
#' Compute a correlation matrix and its pair-specific observation counts.
#' Display a lower triangle by default, an upper triangle, or the full matrix.
#' Presentation changes leave the returned numerical matrices unchanged.
#'
#' @param data A data frame. No implicit model or session data source is used.
#' @param vars Character vector of at least two distinct numeric column names.
#'   Factors, character and logical columns are not converted automatically.
#' @param spearman Use Pearson correlation of average tied ranks, recomputed
#'   within each pair's complete observations, instead of Pearson correlation.
#' @param lower,upper,full Mutually exclusive logical presentation options.
#'   If all are FALSE, use the lower triangle including its diagonal.
#' @param star NULL for the default thresholds 0.001, 0.01 and 0.05, or one to
#'   three unique thresholds strictly between zero and one. Stars use strict
#'   inequality; smaller thresholds receive more stars.
#' @param pvalues Display three-decimal p-values in parentheses instead of
#'   stars. Values below 0.001 print as `<0.001`. Conflicts with explicit `star`.
#' @param digits Decimal digits for coefficients, 0 through 6. NULL uses the
#'   session digits option, falling back to 2. No format string is forwarded.
#' @param labels Named character vector overriding selected variable labels.
#'   Otherwise use each column's variable-label attribute or its exact name.
#'   Labels are literal display text; duplicate labels do not combine variables.
#' @param subset Logical selector with one value per original row. Missing
#'   selectors exclude records. Missing observations are excluded pair by pair.
#' @param xlsx,excel Optional workbook path; nonempty `xlsx` takes priority
#'   over the command-local alias `excel`. Explicit NULL or empty aliases with
#'   neither path selected disable inherited workbook output.
#' @param csv,markdown Optional CSV and Markdown publication paths.
#' @param sheet Workbook sheet. An explicitly supplied non-NULL sheet opts
#'   into session workbook and Markdown defaults; an omitted sheet does not.
#' @param title,footnote Optional title and note paragraphs. The automatic
#'   star legend precedes user paragraphs under the shared paragraph contract.
#' @param font,fontsize,borderstyle,headershade,headercolor,zebra,zebracolor
#'   Publication styles. Ordinary-command session header shading is not inherited.
#' @param mdappend Append Markdown; inherited paths use session write history.
#' @param open Open the written workbook.
#'
#' @details Pearson p-values use a two-sided Student t approximation with
#'   pair count minus two degrees of freedom. Spearman uses the same t
#'   approximation after average ranking within each complete pair, matching
#'   the installed Stata 17 engine invoked by pinned tabtools 2.5.1. It does
#'   not use R's default exact/AS89 rank inference or the beta approximation
#'   described by the current Stata manual. There is no multiplicity adjustment.
#'
#'   With two observations, off-diagonal p-values are missing. With more than
#'   two observations, perfect Pearson correlations have p-value zero; perfect
#'   positive Spearman correlation has zero, and perfect negative Spearman
#'   correlation has a missing p-value, matching that native engine boundary.
#'   Undefined correlations (constants or fewer than two complete observations)
#'   print as dots off the diagonal and blanks on the diagonal. A selected
#'   Pearson sample with no observed values refuses; the Spearman branch
#'   returns undefined matrices with zero counts. Constants retain their useful
#'   sample counts. Diagonal p-values are always missing.
#'
#'   Pairwise deletion can yield a matrix that is not positive semidefinite.
#'   The sample ledger reports selection and each pair separately; it does not
#'   invent one complete-case sample size for the whole matrix. Raw `C`, `P`
#'   and `N` matrices use original variable names. `meta$corr_rows` records
#'   analytical states and which cells are displayed. This is an unkeyed
#'   correlation frame. Use `tt_flat(x, keyed=FALSE)` for its publication data
#'   frame with truthful header, command, frame and sample metadata. Default
#'   keyed flattening and keyed regression composition refuse this frame;
#'   no model slots or regression states are invented.
#'
#' @return A [tt_table()] with command `"corrtab"`. `stored$C`, `stored$P`
#'   and `stored$N` are symmetric coefficient, p-value and integer pair-count
#'   matrices. `stored$methods` describes the actual R computation; metadata
#'   retains variable identities, display labels, method and sample provenance.
#' @references
#' StataCorp (2025). Stata Base Reference Manual, `correlate`, Methods and
#' formulas, p. 7; `spearman`, Methods and formulas, pp. 11-12 (average ranks).
#' Installed Stata 17 `spearman.ado` 4.2.8 (28 February 2022), lines 293-309.
#' Pinned tabtools 2.5.1 `corrtab.ado` (commit 712044f8), lines 166-228,
#' 304-342, 441-537 and 611-630.
#' @examples
#' d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
#' corrtab(d, c("x", "y"), pvalues = TRUE)
#' corrtab(d, c("x", "y"), spearman = TRUE, full = TRUE)
#' @export
corrtab <- function(data, vars, spearman = FALSE, lower = FALSE, upper = FALSE,
                    full = FALSE, star = NULL, pvalues = FALSE, digits = NULL,
                    labels = NULL, subset = NULL, xlsx = NULL, excel = NULL,
                    sheet = "Correlation", title = NULL, footnote = NULL,
                    font = NULL, fontsize = NULL, borderstyle = NULL,
                    headershade = FALSE, headercolor = NULL, zebra = FALSE,
                    zebracolor = NULL, csv = NULL, markdown = NULL,
                    mdappend = FALSE, open = FALSE) {
  for (arg in c("spearman", "lower", "upper", "full", "pvalues", "headershade", "zebra", "open")) {
    .cor_flag(get(arg), arg)
  }
  if (sum(c(lower, upper, full)) > 1L) .cor_abort("lower, upper and full are mutually exclusive.", "shape")
  shape <- if (upper) "upper" else if (full) "full" else "lower"
  thresholds <- .cor_stars(star, pvalues)
  effective_digits <- digits %||% getOption("tabtools.digits") %||% 2L
  if (!is.numeric(effective_digits) || is.complex(effective_digits) || !is.null(dim(effective_digits)) ||
      length(effective_digits) != 1L || !is.finite(effective_digits) ||
      effective_digits != floor(effective_digits) || effective_digits < 0 || effective_digits > 6) {
    .cor_abort("digits must be a whole number from zero through six.", "digits")
  }
  format <- .tt_resolve_numeric_format(digits = effective_digits, default_digits = 2L, max_digits = 6L)
  alias <- .cor_xlsx_alias(xlsx, excel, !missing(xlsx), !missing(excel))
  sinks <- .tt_resolve_sinks(
    list(xlsx = alias$path, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, headershade = headershade),
    list(xlsx = alias$given, markdown = !missing(markdown), mdappend = !missing(mdappend),
         sheet = !missing(sheet), headershade = !missing(headershade)), policy = "sheet", mask = "none")
  xlsx <- sinks$values$xlsx
  markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend
  sheet <- .check_sheet(sheet %||% "Correlation")
  if (open && is.null(xlsx)) .cor_abort("open requires a workbook destination.")
  .tt_check_text_arg(title, "title")
  .tt_check_footnote_arg(footnote)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
    headershade = headershade, headercolor = headercolor, zebra = zebra, zebracolor = zebracolor)
  s <- .cor_sample(data, vars, subset, labels)
  e <- .cor_engine(s, spearman)
  publication <- .cor_body(e$C, e$P, s$labels, shape, thresholds, pvalues, format)
  K <- length(vars)
  meta <- list(sheet = sheet, corr_variables = vars, corr_labels = s$labels,
    corr_spearman = spearman, corr_pvalues = pvalues, corr_shape = shape,
    corr_star = thresholds, numeric_format = format, sample_accounting = e$sample,
    corr_pairs = e$pairs, corr_rows = .cor_rows(e, s, publication$shown),
    frame = list(source = "corrtab", producer = "corrtab", type = "correlation", keyed = FALSE),
    corr_provenance = list(native_pin = "712044f8", native_version = "2.5.1",
      rank_engine = if (spearman) "Stata 17 spearman.ado 4.2.8 (28feb2022)" else NULL,
      inference = "two-sided Student t, pair N minus 2; verified native endpoint guards",
      missingness = "pairwise complete original records", ties = if (spearman) "average ranks within pair" else NULL))
  methods <- paste0(if (spearman) "Spearman correlations of pair-specific average ranks" else "Pearson product-moment correlations",
    "; pairwise-complete observations; two-sided Student t approximation with pair N-2 degrees of freedom; no multiplicity adjustment. ",
    "Computed in R ", getRversion(), " using stats::cor and stats::pt; native endpoint rules follow pinned tabtools 2.5.1/Stata 17.")
  tt <- tt_table(publication$body, header = list(c("", s$labels)),
    rows = data.frame(type = rep("var", K), indent = rep(0L, K)),
    cols = data.frame(role = c("label", rep("value", K)), model = NA_integer_, console_width = NA_integer_),
    title = title %||% "", footnote = .tt_append_footnotes(.cor_legend(thresholds), footnote),
    style = style, command = "corrtab", meta = meta,
    layout = list(indent = 0L, align = "right", console_sepby = FALSE, console_title = TRUE,
      console_blank = TRUE, console_footnote = TRUE, header_style = "plain", xlsx_rules = "corrtab",
      sheet = "Correlation", csv_reservedrow = TRUE),
    stored = list(C = e$C, P = e$P, N = e$N, methods = methods))
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  written <- FALSE
  if (!is.null(csv)) { tt_write_csv(tt, csv); tt$stored$csv <- csv; written <- TRUE }
  if (!is.null(markdown)) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (!is.null(xlsx)) {
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx
    tt$stored$sheet <- sheet
    tt$meta$sheet <- sheet
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}
