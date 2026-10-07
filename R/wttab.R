# wttab (task 7.12): weight diagnostics for inverse probability of treatment
# weighting (IPTW) and marginal structural models (MSMs), as one table on
# puttab's layout (R/puttab.R): the distribution of the weights (N, mean, SD,
# minimum, P1, P25, median, P75, P99, maximum), Kish's effective sample
# size, overall and by treatment group, by follow-up period, and after
# truncation at chosen percentiles.
#
# Stata tabtools has no wttab (EW4: a Stata twin can follow). The numbers
# follow Stata: percentiles are _pctile's (the default definition, verified
# cell for cell against Stata 17 on ties, small samples and n p / 100 knife
# edges: tests/testthat/fixtures/wttab_pctile), the SD is summarize's
# (n - 1), and truncation replaces the weights below and above the pooled
# cut-offs, as msm_weight's truncate() does (msm_weight.ado:586-605; its
# cut-offs are held in scalars, as doubles, since msm 1.4.10, Stata-Tools
# 1255176d: before, local macros could move a cut-off by an ulp and count
# a weight tied at it as truncated). The goldens (W15-W22) are
# Stata's puttab of the same table, with every number computed and
# formatted in Stata (qa/stata/golden_wttab.do).

#' Weight diagnostics for IPTW and marginal structural models
#'
#' Tabulates the distribution of inverse probability weights: N, mean, SD,
#' minimum, 1st, 25th, 50th, 75th, and 99th percentiles, maximum, and the
#' effective sample size, overall and by treatment group (`by`), by
#' follow-up period for time-varying weights (`period`), and after
#' truncation at chosen percentiles (`trunc`). The table has the geometry
#' and styling of [puttab()] (title in `A1`, the table from `B2`, header
#' rule, zebra striping, italic footnote) and is written to a workbook,
#' CSV, or Markdown file, or converted with [flextable::as_flextable()] and
#' [tt_as_gt()], like any tabtools table.
#'
#' @section Inputs:
#' * **A data frame** and the name of its weight column
#'   (`wttab(d, "sw", by = "treat")`). `by` and `period` then name columns
#'   of `x` (or are vectors with one value per row).
#' * **A numeric vector** of weights. `by` and `period` are vectors of the
#'   same length, or column names in `data`.
#' * **A WeightIt object** (`WeightIt::weightit()`): its `$weights`, grouped
#'   by its treatment `$treat` when that is binary or multi-category (unless
#'   `by` says otherwise). `Treated` is WeightIt's treated (focal) level
#'   (its `"treated"` attribute: with `estimand = "ATT", focal = 0` that is
#'   the 0 group) and comes first: a binary 0/1 treatment shows as `Treated`
#'   and `Untreated`, a binary factor treatment by its levels with the
#'   treated one first, a multi-category one by its levels. Sampling
#'   weights (`$s.weights`) are multiplied in, as in `summary.weightit()`
#'   and cobalt, so the ESS is theirs; the footnote then says so, and
#'   `s.weights = FALSE` describes `$weights` alone. A `weightitMSM` object
#'   gives its `$weights` (one per unit).
#' * **An ipw object** (`ipw::ipwpoint()`, `ipw::ipwtm()`): its
#'   `$ipw.weights`, read from any list that has one (duck typing: tabtools
#'   does not depend on ipw, which need not be installed, and any list with
#'   an `ipw.weights` element is read the same way). These objects do not
#'   keep the data, so `by` and
#'   `period` are vectors, or column names in `data` (the data frame the
#'   weights were fitted on, in the same row order). The ipw package's own
#'   `trunc` uses R's default quantile (type 7); `wttab()`'s cut-offs are
#'   Stata's, so its truncated weights can differ slightly from ipw's
#'   `$weights.trunc` (which `wttab()` does not read).
#'
#' Weights must be finite and not negative; zero weights are kept (they
#' count in N and lower the mean and the ESS). Observations with a missing
#' weight, `by`, or `period` value are dropped, with a warning.
#' Infinite or `NaN` weights are refused with
#' `tabtools_error_weights_nonfinite`; an ordinary `NA` weight is dropped.
#'
#' @section Statistics:
#' * **N**: the number of weights.
#' * **Mean**, **SD** (with divisor N - 1, as Stata's `summarize`), **Min**,
#'   **Max**.
#' * **P1**, **P25**, **Median**, **P75**, **P99**: Stata's `_pctile`
#'   (and `summarize, detail`) definition: with P = N p / 100, the average
#'   of the P-th and (P + 1)-th smallest weights when P is a whole number,
#'   otherwise the next larger one. This is the rule of R's
#'   `quantile(type = 2)`, but decided as Stata decides it, as if the
#'   decimal arithmetic were exact: `quantile()` multiplies N by the
#'   proportion p / 100, which is not exact in binary (100 x 0.07 is
#'   7.000000000000001), and even N p / 100 can miss a whole number by a
#'   rounding error (41000 x 99.9 / 100), so a P within 8 units in the last
#'   place of a whole number counts as whole. Otherwise R would take the
#'   next order statistic where Stata averages two; this matters for
#'   `trunc` levels such as 0.1/99.9.
#' * **ESS**: Kish's effective sample size, (sum w)^2 / sum(w^2): the number
#'   of equally weighted observations that would give the same precision.
#'   **ESS (%)** is 100 x ESS / N. Stabilization does not change the ESS
#'   within a group (it multiplies every weight in the group by one
#'   constant). It is computed on the weights divided by a power of two
#'   near their maximum, so tiny or huge weights (1e-200, 1e200) give the
#'   same ESS as their scaled-up or scaled-down copies, where the raw
#'   squares would underflow or overflow; all-zero weights have none.
#' * **Truncated (n)** (with `trunc`): the number of weights in the column
#'   (or row) that truncation changed.
#'
#' Counts are shown as whole numbers, ESS and ESS (%) with one decimal, and
#' the weight statistics with `digits` decimals; numbers of 1,000 or more
#' get thousands separators. A statistic that does not exist (the SD of one
#' weight, anything in an empty cell) is blank.
#'
#' @section Truncation:
#' Each pair in `trunc`, such as `c(0.01, 0.99)`, adds a copy of the table
#' in which the weights below the lower and above the upper percentile are
#' set to those percentiles. The percentiles are taken over all the weights
#' (every group and period together), as is usual for MSMs (Cole and Hernan
#' 2008) and as Stata's `msm_weight, truncate()` does; they are returned in
#' `$stored$trunc`. A lower bound of 0 truncates only from above (and an
#' upper bound of 1 only from below). `trunc_by = "period"` takes the
#' percentiles within each period instead (one cut-off pair per period in
#' `$stored$trunc`).
#'
#' For time-varying weights, pass only the person-periods at risk, as
#' `msm_weight` computes its cut-offs over the risk set: rows after the
#' event or censoring would move the percentiles. For ATT weights (the
#' treated all weigh 1), pooled percentiles include those 1s, so they
#' differ from WeightIt's `trim()`, which leaves the focal group alone.
#'
#' @section Layout:
#' * `"wide"` (the default without `period`): one row per statistic; one
#'   column per group (`Overall`, then the `by` groups), repeated for each
#'   truncation (`Untruncated: Overall`, `Truncated 1/99: Overall`, ...).
#'   A table with no `by` and no `trunc` has one column, `Weights`.
#' * `"long"` (the default, and the only choice, with `period`): one row per
#'   period, truncation, and group, in that order, and one column per
#'   statistic, as MSM papers tabulate weights by follow-up time (Cole and
#'   Hernan 2008). The leading columns name the period (headed by the
#'   variable's label, else its name), the weights (`Untruncated`,
#'   `Truncated 1/99`), and the group; a value repeated from the row above
#'   within its block is left blank. A group with no weights in a period
#'   has a row with N = 0.
#'
#' Unless `footnote` is given, the footnote defines the ESS (and the
#' truncation); `footnote = ""` leaves it out.
#'
#' @param x A data frame, a numeric vector of weights, a `weightit` or
#'   `weightitMSM` object, or an `ipwpoint`/`ipwtm` result (see Inputs).
#' @param weights Data frame `x` only: the name of the weight column.
#' @param by Optional grouping, usually the treatment: a column name (of
#'   `x`, or of `data`) or a vector with one value per weight. Factors keep
#'   their level order and labelled values (`haven::labelled()`) show their
#'   labels; an unlabelled 0/1 (or logical) treatment shows as `Treated`
#'   (1) and `Untreated` (0), in that order (use `by_labels` for a 0/1
#'   variable that is not a treatment); other values are sorted, numbers
#'   shown as [puttab()] shows a column (whole numbers, else `digits`
#'   decimals, or more where `digits` would give two values one label;
#'   round computed values such as `0.1 + 0.2` first, or they may show as
#'   `0.30000000000000004`). A blank group is shown as `(blank)`; a group
#'   called `Overall` is refused while `overall = TRUE`, as are two groups
#'   with the same label (after value labels and `by_labels`; R only, since
#'   Stata lets two values share a value label's text).
#' @param period Optional follow-up period for time-varying weights (the
#'   person-period rows of an MSM), given like `by`. Periods are sorted as
#'   `by` groups are, and labelled or factor periods show their labels;
#'   two periods may not share a label.
#' @param trunc Optional truncation percentiles, as proportions: one pair
#'   `c(lower, upper)` with `0 <= lower < upper <= 1`, or a list of pairs
#'   (`list(c(.01, .99), c(.05, .95))`).
#' @param data A data frame in which to look up `by` and `period` names when
#'   `x` is not a data frame.
#' @param overall With `by`: also show all weights together, as the first
#'   group (`Overall`). Default `TRUE`.
#' @param by_labels Optional names for the `by` groups: a named character
#'   vector whose names are the groups as `wttab()` would show them without
#'   it (for an unlabelled 0/1 variable, `"0"` and `"1"`), e.g.
#'   `c("0" = "Men", "1" = "Women")`. Groups keep their order; with
#'   `by_labels` an unlabelled 0/1 variable is not renamed Treated/Untreated.
#' @param trunc_by `"pooled"` (default): truncation percentiles over all
#'   the weights. `"period"`: within each period (needs `period`).
#' @param s.weights WeightIt objects: multiply the sampling weights
#'   (`$s.weights`) into the weights, as WeightIt and cobalt do (default
#'   `TRUE`).
#' @param layout `"auto"` (default: `"long"` with `period`, else
#'   `"wide"`), `"wide"`, or `"long"` (see Layout).
#' @param digits Decimal places for the weight statistics, 0 to 6 (default
#'   from [tabtools_options()], else 2). Stabilized weights close to 1 are
#'   often easier to read with 3.
#' @param xlsx,sheet Output workbook and sheet (default sheet `"Weights"`);
#'   the sheet is replaced if it exists and the workbook created if needed.
#' @param title Title in cell `A1` (and the first row of the CSV and the
#'   `###` heading of the Markdown file).
#' @param footnote Footnote below the table. `NULL` (default) explains the
#'   ESS (and truncation); `""` for none.
#'   Footnotes accept a character vector of paragraphs. The reserved token
#'   `" \\ "` (one backslash with surrounding spaces) splits each element,
#'   including a scalar, into paragraphs. Split pieces are trimmed and empty
#'   pieces dropped; unspaced and doubled backslashes remain literal.
#'   Automatic notes are separate paragraphs in every sink.
#' @inheritParams puttab
#' @return A `tt_table` (`command = "wttab"`), returned invisibly when
#'   `xlsx`, `csv` or `markdown` writes a file (assign it and print it to see
#'   it). `$stored` holds:
#'   * `W`: the numbers behind the body cells (NA where a cell is blank), in
#'     the table's orientation;
#'   * `stats`: one row per cell of the design (`period`, `weights`,
#'     `group`, then `n`, `mean`, `sd`, `min`, `p1`, `p25`, `p50`, `p75`,
#'     `p99`, `max`, `ess`, `ess_pct`, `n_trunc`), for plots or further
#'     checks;
#'   * `trunc` (with `trunc`): per truncation (and per `period` under
#'     `trunc_by = "period"`), the percentiles asked for
#'     (`lower_q`, `upper_q`), the cut-offs (`lower`, `upper`), and the
#'     numbers of weights raised (`n_low`) and lowered (`n_high`);
#'   * `N` (weights used), `n_dropped` (observations dropped for a missing
#'     value), and `s_weights` (whether WeightIt sampling weights were
#'     multiplied in);
#'   * as [puttab()]: `n_rows`, `n_cols`, `n_datarows`, and, for the files
#'     written, `sheet`, `file`, `csv`, `markdown`, `markdown_rows`,
#'     `markdown_cols`.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @section Differences from Stata:
#' Stata tabtools has no `wttab` command yet. The table is what Stata's
#' `puttab` writes for the same cells: every number as Stata's `summarize`
#' and `_pctile` compute it, formatted with Stata's `string()`, in the
#' table `puttab, varlabels` exports. A
#' `puttab, matrix()` of the numbers alone would show the counts with
#' decimals (puttab formats a matrix column by column) and cannot carry
#' headers with spaces (Stata drops them from matrix column names), so
#' `wttab()` formats each statistic on its own. `by_labels`, `trunc_by`,
#' and the WeightIt and ipw inputs are R additions, with no Stata
#' counterpart to check them against. Like `puttab()`, `wttab()` emits a
#' `wttab: wrote ...` message when it writes a workbook and a
#' `Markdown exported to ...` message when it writes a Markdown file.
#' @references Cole SR, Hernan MA. Constructing inverse probability weights
#'   for marginal structural models. *Am J Epidemiol*. 2008;168(6):656-664.
#'   \doi{10.1093/aje/kwn164}
#'
#'   Kish L. *Survey Sampling*. New York: Wiley; 1965.
#' @seealso [table1_tc()] with `wt =` and `smd = TRUE` for covariate
#'   balance after weighting; [regtab()] with `vce =` for the weighted
#'   outcome model; [puttab()], whose layout this table uses.
#' @examples
#' # IPTW for a point treatment: stabilized weights from a propensity score
#' set.seed(1)
#' n <- 500
#' d <- data.frame(age = rnorm(n, 60, 10), ckd = rbinom(n, 1, 0.3))
#' d$treat <- rbinom(n, 1, plogis(-3 + 0.04 * d$age + 0.8 * d$ckd))
#' ps <- glm(treat ~ age + ckd, family = binomial, data = d)$fitted.values
#' d$sw <- ifelse(d$treat == 1, mean(d$treat) / ps, (1 - mean(d$treat)) / (1 - ps))
#' wttab(d, "sw", by = "treat", digits = 3, title = "Stabilized weights")
#'
#' # The same weights truncated at the 1st/99th and 5th/95th percentiles
#' wttab(d, "sw", trunc = list(c(0.01, 0.99), c(0.05, 0.95)))
#'
#' # A WeightIt object: grouped by its treatment
#' if (requireNamespace("WeightIt", quietly = TRUE)) {
#'   W <- WeightIt::weightit(treat ~ age + ckd, data = d, estimand = "ATE",
#'                           stabilize = TRUE)
#'   wttab(W, digits = 3)
#' }
#'
#' # An MSM with a time-varying treatment A and confounder L: stabilized
#' # weights are the cumulative product over periods of P(A | past A) /
#' # P(A | past A, L); one row per follow-up period
#' pp <- data.frame(id = rep(1:300, each = 4), period = rep(1:4, 300))
#' pp$L <- rbinom(nrow(pp), 1, 0.3 + 0.1 * (pp$period > 2))
#' pp$A <- rbinom(nrow(pp), 1, plogis(-1 + 1.2 * pp$L))
#' den <- glm(A ~ L + factor(period), family = binomial, data = pp)
#' num <- glm(A ~ factor(period), family = binomial, data = pp)
#' f <- ifelse(pp$A == 1, fitted(num) / fitted(den),
#'             (1 - fitted(num)) / (1 - fitted(den)))
#' pp$sw <- ave(f, pp$id, FUN = cumprod)
#' attr(pp$period, "label") <- "Follow-up period"
#' wttab(pp, "sw", period = "period", digits = 3)
#'
#' # By period and treatment, truncated at the 1st/99th percentiles, written
#' # to a workbook
#' path <- tempfile(fileext = ".xlsx")
#' wttab(pp, "sw", period = "period", by = "A", trunc = c(0.01, 0.99),
#'       digits = 3, xlsx = path, title = "Time-varying stabilized weights")
#' @section Session destinations:
#' An explicitly supplied non-NULL `sheet` enables inherited workbook and
#' Markdown destinations from [tabtools_options()]. Default or NULL sheet does
#' not request inherited output. Explicit paths still export, and explicit NULL
#' destinations opt out. A requested sheet without a workbook gives
#' `tabtools_warning_sheet_without_workbook` before output; `options(warn = 2)`
#' interrupts the call. See [tabtools_options()] for append/history behavior.
#'
#' @export
wttab <- function(x, weights = NULL, by = NULL, period = NULL, trunc = NULL, data = NULL,
                  overall = TRUE, by_labels = NULL, trunc_by = c("pooled", "period"),
                  s.weights = TRUE, layout = c("auto", "wide", "long"), digits = NULL,
                  xlsx = NULL, sheet = "Weights", title = NULL, footnote = NULL,
                  font = NULL, fontsize = NULL, borderstyle = NULL, headercolor = NULL,
                  zebracolor = NULL, zebra = FALSE, headershade = FALSE, csv = NULL,
                  markdown = NULL, mdappend = FALSE, open = FALSE) {
  sinks <- .tt_resolve_sinks(
    list(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, headershade = headershade),
    list(xlsx = !missing(xlsx), markdown = !missing(markdown),
         mdappend = !missing(mdappend), sheet = !missing(sheet),
         headershade = !missing(headershade)),
    policy = "sheet")
  xlsx <- sinks$values$xlsx
  markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Weights"
  for (a in c("overall", "s.weights", "zebra", "headershade", "mdappend", "open")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  if (!is.character(layout) || anyNA(layout)) cli::cli_abort("{.arg layout} must be {.val auto}, {.val wide}, or {.val long}.", call = NULL)
  layout <- match.arg(layout)
  if (!is.character(trunc_by) || anyNA(trunc_by)) cli::cli_abort("{.arg trunc_by} must be {.val pooled} or {.val period}.", call = NULL)
  trunc_by <- match.arg(trunc_by)
  if (!is.null(by_labels) && (!is.character(by_labels) || is.null(names(by_labels)) || anyNA(by_labels) ||
                              any(!nzchar(names(by_labels))) || anyDuplicated(names(by_labels)))) {
    cli::cli_abort("{.arg by_labels} must be a named character vector, e.g. {.code c(\"0\" = \"Men\", \"1\" = \"Women\")}.",
                   call = NULL)
  }
  has_xlsx <- !is.null(xlsx)
  has_md <- !is.null(markdown)
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must specify a .xlsx file.", call = NULL)
  }
  sheet <- .check_sheet(sheet)
  if (!is.null(csv)) .tt_check_csv_path(csv)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L || is.na(markdown) ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  digits <- .check_int_range(digits %||% getOption("tabtools.digits") %||% 2L, "digits", 0, 6)
  for (a in c("title", "footnote")) .tt_check_text_arg(get(a), a)
  footnote <- if (is.null(footnote)) NULL else .tt_footnote_text(footnote)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra,
                            headercolor = headercolor, zebracolor = zebracolor)

  inp <- .wt_input(x, weights, by, period, data, s.weights)
  if (!is.null(by_labels) && is.null(inp$by)) cli::cli_abort("{.arg by_labels} needs {.arg by}.", call = NULL)
  cuts <- .wt_check_trunc(trunc)
  if (trunc_by == "period" && length(cuts) && is.null(inp$period)) {
    cli::cli_abort("{.code trunc_by = \"period\"} needs {.arg period}.", call = NULL)
  }
  if (layout == "auto") layout <- if (is.null(inp$period)) "wide" else "long"
  if (layout == "wide" && !is.null(inp$period)) {
    cli::cli_abort(c("A table by {.arg period} has one row per period: use {.code layout = \"long\"}.",
                     "i" = "The wide layout has one row per statistic."), call = NULL)
  }
  res <- .wt_compute(inp, cuts, overall, digits = digits, by_labels = by_labels, trunc_by = trunc_by)
  built <- if (layout == "wide") .wt_wide(res, digits) else .wt_long(res, digits)
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)

  footnote <- footnote %||% .wt_footnote(length(cuts) > 0L, trunc_by = trunc_by, s_weights = inp$s_weights)
  tt <- .puttab_table(built$header, built$body, title = title %||% "", footnote = footnote,
                      style = style, sheet = sheet, source = "weights", command = "wttab")
  tt$layout$sheet <- "Weights"
  tt$stored$W <- built$W
  tt$stored$stats <- res$stats
  if (length(cuts)) tt$stored$trunc <- res$trunc
  tt$stored$N <- length(res$w)
  tt$stored$n_dropped <- inp$n_dropped
  tt$stored$s_weights <- inp$s_weights
  tt$meta$wttab <- list(layout = layout, source = inp$source, digits = digits, trunc_by = trunc_by)
  tt$meta$sample_accounting <- .tt_sample_bind(c(list(inp$sample), res$samples))

  # Sinks in puttab's order: CSV, Markdown, then the workbook.
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    tt$stored$csv <- csv
  }
  if (has_md) {
    res_md <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res_md, "n_rows")
    tt$stored$markdown_cols <- attr(res_md, "n_cols")
    message("Markdown exported to ", markdown)
  }
  if (has_xlsx) {
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$sheet <- sheet
    tt$stored$file <- xlsx
    tt$meta$sheet <- sheet
    message(sprintf("wttab: wrote %d rows x %d cols to sheet %s in %s",
                    nrow(tt$body), ncol(tt$body), sheet, xlsx))
  }
  if (has_xlsx || has_md || !is.null(csv)) invisible(tt) else tt
}

# ---------------------------------------------------------------------------
# Inputs

.wt_input <- function(x, weights, by, period, data, s.weights = TRUE) {
  treated <- NULL
  s_used <- FALSE
  by_name <- if (is.character(by) && length(by) == 1L) by else "Group"
  if (is.data.frame(x)) {
    if (!is.null(data)) cli::cli_abort("{.arg data} is for a weight vector or model object; {.arg x} is already a data frame.", call = NULL)
    if (!is.character(weights) || length(weights) != 1L || is.na(weights)) {
      cli::cli_abort("With a data frame {.arg x}, {.arg weights} must name its weight column.", call = NULL)
    }
    if (!weights %in% names(x)) cli::cli_abort("{.arg weights}: column {.var {weights}} not found in {.arg x}.", call = NULL)
    .tt_check_unambiguous(x, weights, "weights")
    w <- x[[weights]]
    data <- x
    source <- "data"
  } else {
    if (!is.null(weights)) {
      cli::cli_abort("{.arg weights} names a column of a data frame {.arg x}; {.arg x} is {.obj_type_friendly {x}}.", call = NULL)
    }
    if (inherits(x, "weightit") || inherits(x, "weightitMSM")) {
      w <- x$weights
      source <- "weightit"
      # The weights the analysis uses are weights x sampling weights, as in
      # summary.weightit() and cobalt (review 7w F2).
      sw <- x$s.weights
      if (s.weights && is.numeric(sw) && length(sw) == length(w) && any(sw != 1, na.rm = TRUE)) {
        w <- w * sw
        s_used <- TRUE
      }
      tr <- x$treat
      type <- attr(tr, "treat.type", exact = TRUE) %||% x$treat.type
      if (is.null(by) && inherits(x, "weightit") && !is.null(tr) &&
          isTRUE(type %in% c("binary", "multinomial", "multi-category"))) {
        by <- tr
        by_name <- attr(tr, "treat.name", exact = TRUE) %||% "Treatment"
        treated <- attr(tr, "treated", exact = TRUE)
      }
    } else if (is.list(x) && !is.null(x[["ipw.weights"]])) {
      w <- x[["ipw.weights"]]
      source <- "ipw"
    } else if (is.numeric(x) && is.null(dim(x))) {
      w <- x
      source <- "vector"
    } else {
      cli::cli_abort(c("{.arg x} must be a data frame with a {.arg weights} column, a numeric vector of weights, a {.cls weightit} object, or an {.pkg ipw} result, not {.obj_type_friendly {x}}."),
                     call = NULL)
    }
    if (!is.null(data) && !is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.", call = NULL)
  }
  if (is.null(w)) cli::cli_abort("{.arg x} holds no weights.", call = NULL)
  if (is.factor(w) || is.character(w) || is.list(w) || !is.null(dim(w)) || !(is.numeric(w) || is.logical(w))) {
    cli::cli_abort("The weights must be numeric, not {.obj_type_friendly {w}}.", call = NULL)
  }
  w <- as.numeric(unclass(w))
  n <- length(w)
  if (!n) cli::cli_abort("There are no weights to tabulate.", call = NULL)
  bad <- is.nan(w) | is.infinite(w)
  if (any(bad)) {
    cli::cli_abort("The weights must be finite; {sum(bad)} {?is/are} infinite or NaN.",
                   class = "tabtools_error_weights_nonfinite", call = NULL)
  }
  if (any(w < 0, na.rm = TRUE)) {
    cli::cli_abort("The weights must not be negative; {sum(w < 0, na.rm = TRUE)} {?is/are}.", call = NULL)
  }
  period_name <- if (is.character(period) && length(period) == 1L) period else "Period"
  b <- .wt_var(by, "by", data, n)
  p <- .wt_var(period, "period", data, n)
  by_head <- if (is.null(b)) NULL else var_label(b, by_name)
  period_head <- if (is.null(p)) NULL else var_label(p, period_name)

  miss <- is.na(w)
  if (!is.null(b)) miss <- miss | is.na(b)
  if (!is.null(p)) miss <- miss | is.na(p)
  n_dropped <- sum(miss)
  reasons <- list(missing_weight = is.na(w))
  if (!is.null(b)) reasons$missing_group <- is.na(b)
  if (!is.null(p)) reasons$missing_period <- is.na(p)
  remaining <- rep(TRUE, n)
  exclusions <- lapply(names(reasons), function(reason) {
    dropped <- remaining & reasons[[reason]]
    remaining <<- remaining & !reasons[[reason]]
    .tt_sample_exclusion("input_to_eligible", reason, sum(dropped), "sequential provided weight-vector masks")
  })
  weight_sample <- .tt_sample_weights(w[!miss])
  sample <- .tt_sample_population("weights", "wttab", "table", variable = "weights", weight_type = "diagnostic",
    values = c(list(input_n = n, eligible_n = sum(!miss), observed_n = sum(!miss), used_n = sum(!miss),
      missing_n = sum(is.na(w)), excluded_n = n_dropped, zero_weight_n = sum(w[!miss] == 0),
      reported_n = sum(!miss)), weight_sample$values),
    bases = c(list(input_n = "provided weight-vector length; not the original fitted cohort",
      eligible_n = "nonmissing weight, group and period", observed_n = "nonmissing retained weights",
      used_n = "retained weights including zeros", missing_n = "missing weights within provided weight vector",
      excluded_n = "input records minus retained records", zero_weight_n = "retained zero weights",
      reported_n = "existing wttab N: retained weight records"), weight_sample$bases),
    reasons = weight_sample$reasons, statuses = weight_sample$statuses,
    exclusions = do.call(rbind, exclusions))
  if (n_dropped) {
    what <- c("weight", if (!is.null(b)) "{.arg by}", if (!is.null(p)) "{.arg period}")
    what <- if (length(what) == 1L) what else if (length(what) == 2L) paste(what, collapse = " or ") else
      paste0(paste(what[-3L], collapse = ", "), ", or ", what[3L])
    cli::cli_warn(paste0("Dropped {n_dropped} observation{?s} with a missing ", what, " value."), call = NULL)
    keep <- !miss
    w <- w[keep]
    if (!is.null(b)) b <- .wt_subset(b, keep)
    if (!is.null(p)) p <- .wt_subset(p, keep)
  }
  if (!length(w)) cli::cli_abort("No observation has a non-missing weight{if (!is.null(b) || !is.null(p)) ' and grouping' else ''}.",
                                 call = NULL)
  list(w = w, by = b, period = p, by_head = by_head, period_head = period_head, treated = treated,
       source = source, n_dropped = n_dropped, s_weights = s_used, sample = sample)
}

# `by`/`period`: a column name (of `data`) or a vector of length n.
.wt_var <- function(v, arg, data, n) {
  if (is.null(v)) return(NULL)
  if (is.character(v) && length(v) == 1L && !is.na(v) && (n != 1L || (!is.null(data) && v %in% names(data)))) {
    if (is.null(data)) {
      cli::cli_abort(c("{.arg {arg}} names a column ({.var {v}}), but there is no data frame to look it up in.",
                       "i" = "Give {.arg data}, or pass {.arg {arg}} as a vector with one value per weight."), call = NULL)
    }
    if (!v %in% names(data)) cli::cli_abort("{.arg {arg}}: column {.var {v}} not found.", call = NULL)
    .tt_check_unambiguous(data, v, arg)
    v <- data[[v]]
  }
  if (is.list(v) && !inherits(v, "POSIXlt") || !is.null(dim(v))) {
    cli::cli_abort("{.arg {arg}} must be a column name or a vector.", call = NULL)
  }
  if (length(v) != n) {
    cli::cli_abort("{.arg {arg}} has {length(v)} value{?s}; the weights have {n}.", call = NULL)
  }
  v
}

# Subset keeping the attributes that label the values.
.wt_subset <- function(v, keep) {
  at <- attributes(v)
  out <- v[keep]
  for (a in intersect(c("labels", "label", "format.stata"), names(at))) attr(out, a) <- at[[a]]
  out
}

# Groups of a by/period vector: an integer index per value and a label per
# group, in display order.
.wt_levels <- function(v, binary = FALSE, treated = NULL, digits = 2L) {
  if (is.factor(v) || is.character(v)) {
    if (is.factor(v)) {
      v <- droplevels(v)
      u <- levels(v)
    } else {
      u <- sort(unique(v), method = "radix")
    }
    # WeightIt's treated (focal) level first, as a 0/1 treatment's Treated
    # (review 7w F9).
    if (binary && is.character(treated) && length(treated) == 1L && length(u) == 2L && treated %in% u) {
      u <- c(treated, setdiff(u, treated))
    }
    return(list(index = match(as.character(v), u), labels = u))
  }
  if (is.logical(v)) v <- as.integer(v)
  if (inherits(v, c("Date", "POSIXt"))) {
    u <- sort(unique(v))
    return(list(index = match(as.numeric(v), as.numeric(u)), labels = .stata_datetime(u, attr(v, "format.stata", exact = TRUE))))
  }
  labs <- attr(v, "labels", exact = TRUE)
  x <- as.numeric(unclass(v))
  u <- sort(unique(x))
  if (binary && (is.null(labs) || !length(labs)) && all(u %in% c(0, 1))) {
    one <- if (is.numeric(treated) && length(treated) == 1L && treated %in% c(0, 1)) treated else 1
    lab <- c("Treated", "Untreated")
    code <- c(one, 1 - one)
    return(.wt_drop_empty(list(index = match(x, code), labels = lab)))
  }
  hit <- if (is.numeric(labs) && length(labs)) match(u, as.numeric(labs), incomparables = NA) else rep(NA_integer_, length(u))
  lab <- .wt_num_labels(u, digits, is.na(hit))
  lab[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
  list(index = match(x, u), labels = lab)
}

# Numeric group/period values as text. The statistics' `digits` decide the
# decimals, but they must not merge distinct values: periods 0.001 and 0.002
# both printed "0.00", the second period's heading was blanked as a repeat,
# and $stored$stats lost which rows were which (Codex audit 2, F08). On a
# collision among the values shown as numbers (`shown`: not value-labelled)
# the decimals grow until each has its own label; without one the labels
# are exactly the `digits` ones.
.wt_num_labels <- function(u, digits, shown = rep(TRUE, length(u))) {
  lab <- .puttab_fmt_num(u, digits)
  d <- digits
  while (anyDuplicated(lab[shown]) && d < 17L) {
    d <- d + 1L
    lab <- .puttab_fmt_num(u, d)
  }
  if (anyDuplicated(lab[shown])) lab <- sprintf("%.17g", u)
  lab
}

# Labels are the only identity a group or period keeps in the table and in
# $stored$stats, so two groups may not share one (Codex audit 2, F08; R
# only: Stata lets two values share a value label's text). `hint` is the
# cli bullet for the usual cause.
.wt_check_unique_labels <- function(labels, v, arg, hint) {
  dup <- unique(labels[duplicated(labels)])
  if (!length(dup)) return(invisible(labels))
  if (identical(dup, "(blank)")) {
    hint <- "Empty and blank values are all shown as {.val (blank)}: recode them to one value."
  } else if (inherits(v, c("Date", "POSIXt"))) {
    hint <- "Dates and times that differ by less than the display shows (a second) share a label: round them first."
  }
  cli::cli_abort(c("Different {.arg {arg}} values have the same label: {.val {dup}}.", "i" = hint), call = NULL)
}

.wt_drop_empty <- function(g) {
  used <- sort(unique(g$index))
  list(index = match(g$index, used), labels = g$labels[used])
}

.wt_check_trunc <- function(trunc) {
  if (is.null(trunc)) return(list())
  if (is.numeric(trunc)) trunc <- list(trunc)
  if (!is.list(trunc) || !length(trunc)) {
    cli::cli_abort("{.arg trunc} must be a pair {.code c(lower, upper)} or a list of pairs.", call = NULL)
  }
  lapply(trunc, function(q) {
    if (!is.numeric(q) || length(q) != 2L || anyNA(q) || q[1] < 0 || q[2] > 1 || q[1] >= q[2]) {
      hint <- if (is.numeric(q) && length(q) == 2L && !anyNA(q) && max(q) > 1) {
        c("i" = "Give proportions, e.g. {.code c(0.01, 0.99)} for the 1st and 99th percentiles.")
      }
      cli::cli_abort(c("Each {.arg trunc} element must be {.code c(lower, upper)} with 0 <= lower < upper <= 1.", hint),
                     call = NULL)
    }
    # Percent as Stata's _pctile p() takes it, without the representation
    # error of the proportion (0.07 * 100 = 7.000000000000001).
    pct <- round(100 * q, 10)
    list(q = as.numeric(q), pct = pct, label = paste0("Truncated ", .wt_num_text(pct[1]), "/", .wt_num_text(pct[2])))
  })
}

# A percentile as text ("1", "2.5"), through the shared Stata formatter.
.wt_num_text <- function(x) sub("\\.$", "", sub("0+$", "", trimws(stata_fmt(x, "%32.10f"))))

# ---------------------------------------------------------------------------
# Statistics

# Stata's _pctile (and summarize, detail): P = n p / 100; the mean of the
# P-th and (P+1)-th order statistics when P is whole, else the ceiling(P)-th.
# p = 0 gives the minimum and p = 100 the maximum. `xs` is sorted. p is in
# percent, as _pctile's p(). Stata decides "P is whole" as if the arithmetic
# were exact decimal; in binary n * p / 100 can miss a whole number by an
# ulp either way (41000 * 99.9 / 100 = 40958.99999999999; quantile(type = 2)'s
# n * (p / 100) misses at n = 100, p = 7). P is therefore snapped to the
# nearest whole number when within 8 ulps (review 7w F1): 0 mismatches
# against Stata 17 in the fixture tests/testthat/fixtures/wttab_pctile and
# in the review's 166,000 probed cells.
# The mean of two order statistics, (a + b) / 2 as Stata forms it, or
# a / 2 + b / 2 when a + b overflows (codex audit F08: weights of 1e308
# gave infinite quartiles, and truncation at them moved every weight).
.wt_mid <- function(a, b) {
  s <- a + b
  if (is.finite(s)) s / 2 else a / 2 + b / 2
}

.wt_pctile <- function(xs, p) {
  n <- length(xs)
  if (!n) return(rep(NA_real_, length(p)))
  vapply(p, function(pp) {
    P <- n * pp / 100
    r <- round(P)
    if (abs(P - r) <= 8 * .Machine$double.eps * max(1, P)) P <- r
    i <- floor(P)
    if (P == i) {
      if (i < 1) xs[1L] else if (i >= n) xs[n] else .wt_mid(xs[i], xs[i + 1L])
    } else {
      xs[min(i + 1L, n)]
    }
  }, 0)
}

.wt_stat_keys <- c("n", "mean", "sd", "min", "p1", "p25", "p50", "p75", "p99", "max", "ess", "ess_pct")
.wt_stat_labels <- c(n = "N", mean = "Mean", sd = "SD", min = "Min", p1 = "P1", p25 = "P25",
                     p50 = "Median", p75 = "P75", p99 = "P99", max = "Max", ess = "ESS",
                     ess_pct = "ESS (%)", n_trunc = "Truncated (n)")

.wt_stats <- function(w) {
  n <- length(w)
  out <- stats::setNames(rep(NA_real_, length(.wt_stat_keys)), .wt_stat_keys)
  out["n"] <- n
  if (!n) return(out)
  xs <- sort(w)
  out["mean"] <- mean(w)
  # The SD on the weights divided by a power of two near their maximum
  # (exact), then scaled back: squared deviations of weights near 1e308
  # overflow (codex audit F08).
  m <- max(w)
  sc <- if (m > 0) 2^min(max(ceiling(log2(m)), -1074), 1023) else 1
  if (n > 1L) out["sd"] <- stats::sd(w / sc) * sc
  out["min"] <- xs[1L]
  out["max"] <- xs[n]
  out[c("p1", "p25", "p50", "p75", "p99")] <- .wt_pctile(xs, c(1, 25, 50, 75, 99))
  # ESS is scale invariant; it is computed on the weights divided by a power
  # of two near their maximum (exact, so ordinary weights give the raw
  # formula's bits), because squaring the raw weights underflows (1e-200:
  # ESS was NA) or overflows (1e200: NaN) (Codex audit D2). All-zero
  # weights keep no ESS.
  if (m > 0) {
    ws <- w / sc
    out["ess"] <- sum(ws)^2 / sum(ws^2)
    out["ess_pct"] <- 100 * out[["ess"]] / n
  }
  out
}

.wt_compute <- function(inp, cuts, overall, digits = 2L, by_labels = NULL, trunc_by = "pooled") {
  w <- inp$w
  n <- length(w)
  # Groups: Overall (unless dropped under by) and the by groups.
  if (is.null(inp$by)) {
    groups <- list(list(label = NA_character_, rows = rep(TRUE, n)))
  } else {
    g <- .wt_levels(inp$by, binary = is.null(by_labels), treated = inp$treated, digits = digits)
    if (!is.null(by_labels)) {
      bad <- setdiff(names(by_labels), g$labels)
      if (length(bad)) {
        cli::cli_abort(c("{.arg by_labels} names {.val {bad}}, not {?a value/values} of {.arg by}.",
                         "i" = "The values are {.val {g$labels}}."), call = NULL)
      }
      hit <- match(g$labels, names(by_labels))
      g$labels[!is.na(hit)] <- unname(by_labels[hit[!is.na(hit)]])
    }
    # A blank group gets a visible name; a group called Overall would be
    # indistinguishable from the Overall column (review 7w F10).
    g$labels[!nzchar(trimws(g$labels))] <- "(blank)"
    .wt_check_unique_labels(g$labels, inp$by, "by", "Each group needs its own label: check the value labels and {.arg by_labels}.")
    if (overall && "Overall" %in% g$labels) {
      cli::cli_abort(c("A {.arg by} group is called {.val Overall}, as is the column of all weights.",
                       "i" = "Rename it with {.arg by_labels}, or use {.code overall = FALSE}."), call = NULL)
    }
    groups <- lapply(seq_along(g$labels), function(k) list(label = g$labels[k], rows = g$index == k))
    if (overall) groups <- c(list(list(label = "Overall", rows = rep(TRUE, n))), groups)
  }
  if (is.null(inp$period)) {
    periods <- list(list(label = NA_character_, rows = rep(TRUE, n)))
  } else {
    p <- .wt_levels(inp$period, digits = digits)
    .wt_check_unique_labels(p$labels, inp$period, "period", "Each period needs its own label: check the value labels of {.arg period}.")
    periods <- lapply(seq_along(p$labels), function(k) list(label = p$labels[k], rows = p$index == k))
  }
  # Truncated copies: cut-offs over all the weights (pooled, the default),
  # or within each period (trunc_by = "period").
  cut_rows <- function(sel, label) {
    xs <- sort(w[sel])
    do.call(rbind, lapply(cuts, function(ct) {
      b <- .wt_pctile(xs, ct$pct)
      if (!all(is.finite(b))) {
        cli::cli_abort("The truncation cutpoints of the weights are not finite.", call = NULL)
      }
      data.frame(period = label, weights = ct$label, lower_q = ct$q[1], upper_q = ct$q[2], lower = b[1],
                 upper = b[2], n_low = sum(w[sel] < b[1]), n_high = sum(w[sel] > b[2]), stringsAsFactors = FALSE)
    }))
  }
  trunc <- NULL
  if (length(cuts)) {
    trunc <- if (trunc_by == "period") {
      do.call(rbind, lapply(periods, function(pr) cut_rows(pr$rows, pr$label)))
    } else {
      cut_rows(rep(TRUE, n), NA_character_)
    }
    rownames(trunc) <- NULL
    if (trunc_by == "pooled") trunc$period <- NULL
  }
  versions_for <- function(k_period) {
    v <- list(list(label = if (length(cuts)) "Untruncated" else NA_character_, w = w, moved = NULL))
    for (k in seq_along(cuts)) {
      r <- if (trunc_by == "period") (k_period - 1L) * length(cuts) + k else k
      lo <- trunc$lower[r]
      hi <- trunc$upper[r]
      v[[k + 1L]] <- list(label = cuts[[k]]$label, w = pmin(pmax(w, lo), hi), moved = w < lo | w > hi)
    }
    v
  }
  rows <- list()
  samples <- list()
  for (kp in seq_along(periods)) {
    pr <- periods[[kp]]
    for (v in versions_for(kp)) for (gr in groups) {
      sel <- pr$rows & gr$rows
      s <- .wt_stats(v$w[sel])
      rows[[length(rows) + 1L]] <- data.frame(
        period = pr$label, weights = v$label, group = gr$label, t(s),
        n_trunc = if (is.null(v$moved)) NA_real_ else sum(v$moved[sel]),
        stringsAsFactors = FALSE, check.names = FALSE)
      weighted <- .tt_sample_weights(v$w[sel])
      samples[[length(samples) + 1L]] <- .tt_sample_population(
        paste0("weights/cell", length(rows)), "wttab", "group",
        component = paste0("period", kp), variable = "weights", group = gr$label,
        spec = match(v$label, vapply(versions_for(kp), `[[`, "", "label")), weight_type = "diagnostic",
        values = c(list(input_n = sum(sel), eligible_n = sum(sel), observed_n = sum(sel),
          used_n = sum(sel), missing_n = 0, excluded_n = 0,
          zero_weight_n = sum(v$w[sel] == 0), reported_n = unname(s["n"])), weighted$values),
        bases = c(list(input_n = "cell records after shared input eligibility",
          eligible_n = "cell records after shared input eligibility", observed_n = "nonmissing cell weights",
          used_n = "cell weights including zeros", missing_n = "nonmissing eligible cell weights",
          excluded_n = "no further cell exclusions", zero_weight_n = "cell zero weights",
          reported_n = "existing wttab cell N"), weighted$bases),
        reasons = weighted$reasons, statuses = weighted$statuses)
    }
  }
  stats <- do.call(rbind, rows)
  rownames(stats) <- NULL
  list(w = w, stats = stats, trunc = trunc, has_by = !is.null(inp$by), has_period = !is.null(inp$period),
       has_trunc = length(cuts) > 0L, by_head = inp$by_head, period_head = inp$period_head, samples = samples)
}

# ---------------------------------------------------------------------------
# Layouts and cell text

.wt_fmt <- function(x, key, digits) {
  fmt <- switch(key,
    n = , n_trunc = "%32.0fc",
    ess = "%32.1fc",
    ess_pct = "%32.1f",
    paste0("%32.", digits, "fc"))
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- trimws(stata_fmt(x[ok], fmt))
  out
}

.wt_keys <- function(res) c(.wt_stat_keys, if (res$has_trunc) "n_trunc")

.wt_wide <- function(res, digits) {
  st <- res$stats
  keys <- .wt_keys(res)
  head <- if (res$has_trunc && res$has_by) {
    paste0(st$weights, ": ", st$group)
  } else if (res$has_trunc) {
    st$weights
  } else if (res$has_by) {
    st$group
  } else {
    "Weights"
  }
  W <- t(as.matrix(st[, keys, drop = FALSE]))
  dimnames(W) <- list(unname(.wt_stat_labels[keys]), head)
  body <- vapply(seq_len(ncol(W)), function(j) {
    vapply(seq_along(keys), function(i) .wt_fmt(W[i, j], keys[i], digits), "")
  }, character(length(keys)))
  body <- cbind(unname(.wt_stat_labels[keys]), matrix(body, length(keys)))
  list(header = c("Statistic", head), body = body, W = W)
}

.wt_long <- function(res, digits) {
  st <- res$stats
  keys <- .wt_keys(res)
  heads <- character()
  kcols <- list()
  add <- function(h, v) {
    heads <<- c(heads, h)
    kcols[[length(kcols) + 1L]] <<- v
  }
  if (res$has_period) add(res$period_head, st$period)
  if (res$has_trunc) add("Weights", st$weights)
  if (res$has_by) add(res$by_head, st$group)
  if (!length(kcols)) add("Weights", rep("All", nrow(st)))
  full <- do.call(cbind, kcols)
  # Show a key only where its block starts: blank it when it and every key
  # to its left repeat the row above. The last key is always shown.
  shown <- full
  nk <- ncol(full)
  if (nk > 1L && nrow(full) > 1L) {
    for (i in 2:nrow(full)) {
      same <- full[i, ] == full[i - 1L, ]
      for (j in seq_len(nk - 1L)) if (all(same[seq_len(j)])) shown[i, j] <- ""
    }
  }
  W <- as.matrix(st[, keys, drop = FALSE])
  dimnames(W) <- list(apply(full, 1L, paste, collapse = ", "), unname(.wt_stat_labels[keys]))
  vals <- vapply(seq_along(keys), function(k) .wt_fmt(W[, k], keys[k], digits), character(nrow(W)))
  body <- cbind(shown, matrix(vals, nrow(W)))
  dimnames(body) <- NULL
  list(header = c(heads, unname(.wt_stat_labels[keys])), body = body, W = W)
}

.wt_footnote <- function(trunc, trunc_by = "pooled", s_weights = FALSE) {
  out <- "ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N."
  if (trunc) {
    of <- if (trunc_by == "period") "of the weights in the same period" else "of all weights"
    out <- .tt_append_footnotes(out, paste0("Truncated l/u: weights below the l-th or above the u-th percentile ", of,
                             " set to that percentile; Truncated (n) counts the weights changed."))
  }
  if (s_weights) out <- .tt_append_footnotes(out, "Weights include the sampling weights (s.weights).")
  out
}
