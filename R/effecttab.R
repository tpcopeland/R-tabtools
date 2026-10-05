# effecttab (Phase 7d, plan tasks 7.7-7.11): treatment effects and margins
# as a house-styled table. Port of Stata tabtools 2.1.12 effecttab.ado
# (1,719 lines). Stata formats a collection of `teffects` or `margins`
# results, or a matrix (from()); R formats marginaleffects results, data
# frames and matrices, and fits nothing itself (decision EO4).

#' Treatment-effect and margins table
#'
#' Formats treatment effects (average treatment effects and
#' potential-outcome means) or margins (predictive margins and average
#' marginal effects) as a table with one estimate, confidence interval and
#' p-value block per model. The estimates come from the
#' [marginaleffects](https://marginaleffects.com) package, from a data frame,
#' or from a matrix; `effecttab()` only formats them. R implementation of
#' the Stata `effecttab` command (tables of Stata's `teffects` and
#' `margins` results); the table prints, and is written to Excel with the
#' tabtools house style, and to CSV and Markdown.
#'
#' @section Input:
#' Each argument in `...` is one model (a column block of the table), or a
#' single unnamed list holds one model per element (as in [regtab()]; its
#' names become the model labels). A model is
#' * a marginaleffects result: [marginaleffects::avg_predictions()],
#'   [marginaleffects::avg_comparisons()] or [marginaleffects::avg_slopes()]
#'   (averaged results; `by` subgroups of a comparison are not read). A
#'   result with a `hypothesis =` test, and a [marginaleffects::hypotheses()]
#'   result, give one margins row per hypothesis, labelled by it (`b2 - b1`,
#'   `Manual - Automatic`);
#' * a data frame with one row per effect: `estimate` and a `term` naming
#'   the row, plus `conf.low`/`conf.high` and `p.value` (or `std.error`,
#'   from which a Wald interval and p-value are derived; the p-value tests
#'   an optional `null.value` column, default 0, and a ratio header such as
#'   `effect = "RR"` without `null.value` or `p.value` is refused). Both
#'   bounds or neither: a row with one is refused, and `conf.low` must not
#'   exceed `conf.high`. A `term` may be a
#'   Stata key, as in `r(table)`: `r1vs0.mbsmoke` (a contrast), `0.mbsmoke`
#'   (a potential-outcome mean, or a factor level under `type =
#'   "margins"`), `1b.race` (a base level), `o.x` (omitted), `age`; an
#'   `equation` column (`ATE`, `ATET`, `POmean`) keeps Stata's teffects
#'   equations and drops the treatment and outcome models' coefficients. A
#'   row may instead be described by `kind` (`"contrast"`, `"pomean"`,
#'   `"prediction"`, `"slope"`, `"level"`, `"ref"`), `variable`, `level` and
#'   `base`, or labelled directly by a `label` column. Optional: `status`
#'   (`"base"`, `"omit"`, `"empty"`), and the attributes `conf.level` and
#'   `tt_estimator` (a teffects estimator such as `"ipw"`);
#' * a numeric matrix with at least four columns, the estimate, the lower
#'   and upper bounds, and the p-value (Stata's `from()`): its row names are
#'   the labels, with `_` shown as a space;
#' * an unnamed list of marginaleffects results or data frames, stacked into
#'   one model: a teffects model is the contrast and its potential-outcome
#'   mean, e.g. `list(avg_comparisons(fit, variables = "treat"),
#'   avg_predictions(fit, variables = list(treat = 0)))`. One such model is
#'   `effecttab(list(ate, po))`: a single unnamed list of exactly one
#'   comparison and one prediction of the same variable is read as one
#'   treatment-effect model, not as two models (with `type = "margins"` it
#'   is two). Several are `effecttab(list(IPW = list(ate, po), RA =
#'   list(ate2, po2)))`, or named arguments `effecttab(IPW = list(ate, po))`.
#'
#' Rows of several models are joined by their keys, and a factor's levels
#' by level: a plain factor's or a character variable's levels are keyed
#' as [regtab()] keys them, a level whose text is an integer by that
#' integer (`factor(0:2)` is 0, 1, 2) and any other level by position, so a
#' factor relevelled in one model, or a subset lacking a level, is recoded
#' to one numbering over every model, as [regtab()] does.
#' A variable coded by position in one model and by value in another (a
#' plain factor beside a [tt_as_factor()] or haven-labelled one) whose keys
#' then name different levels is refused with an error of class
#' `tabtools_error_effect_row_mismatch`. A continuous comparison other than
#' the derivative (`+1`, `sd`) is its own row, so the `+1` and `+2`
#' contrasts of two models are two rows.
#'
#' @section Teffects and margins:
#' `type = "auto"` makes a table of treatment effects when a model stacks a
#' comparison and a prediction (a contrast and its potential-outcome mean),
#' or when a data frame holds teffects rows, and a margins table otherwise;
#' teffects and margins results cannot be mixed. In a teffects table,
#' comparisons are treatment contrasts and predictions potential-outcome
#' means. Without `clean` they show Stata's raw keys, `r1vs0.mbsmoke` and
#' `0.mbsmoke`, built from the treatment's level codes (a value-labelled or
#' numeric variable's values; a factor from [tt_as_factor()] keeps its
#' Stata codes; any other factor is coded by its integer level texts, else
#' by level position). With `clean`
#' (or `tlabels`) they read `Smoker vs Nonsmoker` and `Nonsmoker (PO Mean)`
#' when the levels have labels, else `1 if mother smoked (1 vs 0)` and `1 if
#' mother smoked = 0 (PO Mean)`, from the variable label with its first
#' letter capitalised. `tlabels` replaces the value labels: a level it does
#' not name shows its code. Stata's `teffects ..., ate` reports the control
#' group's potential-outcome mean only; `avg_predictions(fit, variables =
#' list(treat = 0))` gives that row alone.
#'
#' In a margins table a factor (a factor, character or logical variable in
#' the model) is headed by its variable label with its levels indented
#' beneath, a comparison's base level shown as `refcat`; a continuous
#' variable is one row labelled by its variable label, with the contrast
#' appended when it is not the derivative (`Age (years) (+10)`). Rows
#' follow the order the variables were requested in, as Stata lists
#' `dydx()` terms, when `variables` is written out in the call (`variables
#' = c("b", "a")`); `variables = vars` keeps marginaleffects' alphabetical
#' order. A multi-valued treatment's contrasts are listed in level order.
#' Labels come from `data`, else from the model's data. A `tlabels` level
#' of a plain factor (one without Stata codes) may be named by its level,
#' and an unnamed level of such a factor keeps its level text.
#'
#' @section Ratios (risk ratios after IPTW):
#' The recommended route to a risk ratio is the log ratio, exponentiated:
#' `avg_comparisons(fit, variables = "treat", comparison = "lnratioavg",
#' transform = exp)` (or `"lnoravg"` for an odds ratio), with its
#' potential-outcome mean in a teffects model. The p-value is marginaleffects'
#' Wald test of the log ratio against 0, that is of the ratio against 1, and
#' the interval is the exponentiated log-scale interval. This is how Stata
#' reports ratios after `teffects` (`nlcom` or `margins` on the log scale,
#' then exponentiated): the interval respects the ratio's range and the test
#' and interval agree.
#'
#' A plain ratio (`comparison = "ratio"`) carries a delta-method standard
#' error on the ratio scale, and marginaleffects tests it against 0, which
#' is not the null a ratio table displays. `effecttab()` therefore
#' recomputes that p-value against 1, z = (ratio - 1) / se (t with the
#' result's df when finite), keeps marginaleffects' interval, and adds "P-values of ratios test a
#' ratio of 1." to the footnote. A result that states a numeric
#' `hypothesis` keeps marginaleffects' test of that null, unchanged; a null
#' other than 1 (0 for a difference) is then the one the footnote names.
#' Its interval, symmetric on the ratio scale, can reach below 0; such a
#' result gives a warning and a footnote pointing to the log ratio. If a
#' plain-ratio result has a p-value but no usable ratio-scale standard
#' error (for example after `transform`), its default-null test cannot be
#' recomputed and is refused. Compute it with `hypothesis = 1` before any
#' transformation, or use the log-ratio route above. In a
#' table of several models a note that holds for some models only names
#' them (`Model 2: P-values test a null value of 2.`).
#'
#' @section Confidence level:
#' The level is the results' own: marginaleffects records it, and a data
#' frame may carry it (`attr(x, "conf.level")`). All results must agree,
#' and `level`, if given, must match them. A result that records no level
#' uses `level`, else 95% (with a message); a matrix uses `level`, else 95%,
#' silently, as Stata's `from()` does. Intervals a data frame leaves to be
#' derived from `std.error` are built at its own level, else at `level`,
#' else at 95%, from the normal distribution or Student's t with the row's
#' `df` (`df.error`), so the header always names the interval's level.
#' Derived intervals and p-values remain missing for a non-finite estimate
#' or standard error, or an explicitly missing `df`. An absent `df` column
#' or `df = Inf` uses the normal distribution. Supplied inference is kept.
#'
#' @section Differences from Stata:
#' * Stata reads the active collection; R takes the results as arguments,
#'   and no file is required. R has no frames: the returned table (and
#'   [as.data.frame()] of it) stands for `frame()`, [as_forest_data()] for
#'   `eplotframe()`.
#' * `full` (the treatment and outcome models' coefficients) is not ported:
#'   use [regtab()] on those models.
#' * Stata shows `margins, at()` and `over()` results by their raw keys
#'   (`1._at`, `1.sex#0.highbp`); R shows the values and value labels under
#'   the variable (`Age (years)`, `  40`; `sex#highbp`, `  Male#0`). A
#'   data frame with Stata's keys reproduces Stata's rows.
#' * A Reference row comes from the structure (a comparison's base level, a
#'   `1b.` key, `status = "base"`), never from a zero estimate: Stata
#'   falls back to showing a zero estimate without an interval as the
#'   reference when its collection records no base level.
#' * The methods sentence names the marginaleffects package for a
#'   marginaleffects margins result, and drops Stata's closing "Analysis
#'   performed in Stata ..." sentence.
#' * Like [regtab()], R prints no `Exported to ...` or `Markdown exported
#'   to ...` lines; `level` also takes a proportion.
#' * The methods sentence names what the table holds: "Potential-outcome
#'   means ..." for `pomeans`, "Average treatment effects on the treated
#'   ..." for an ATET (a data frame's `ATET` equation, or a
#'   `WeightIt::glm_weightit()` fit with `estimand = "ATT"`), where Stata says
#'   "Average treatment effects" for all three. The estimator of a
#'   `glm_weightit()` fit with logistic weights is read from it (`ipw` for
#'   `y ~ treat`, else `ipwra`). Other results cannot say which effect they
#'   estimate (an `avg_comparisons()` over the treated, or of a fit with
#'   ATT weights), so `estimand` (R only) states it.
#' * **Regression adjustment standard errors.** `teffects ra` reports a
#'   robust (sandwich) variance. `lm()`/`glm()` results passed to
#'   marginaleffects with its default `vcov` use the model-based variance,
#'   which is narrower: on Stata's `cattaneo2` example the ATE's standard
#'   error is 1.8% smaller. Pass `vcov = "HC0"`; the gap is then
#'   about 0.1%, the part `teffects` adds for the sampling variation of the
#'   covariates (the delta method treats them as fixed). `effecttab()` says
#'   so once per session when a treatment-effect result comes from an
#'   `lm()`/`glm()` fit with the default variance.
#' * **0/1 variables in marginal effects.** marginaleffects always reports
#'   the discrete change 1 - 0 of a 0/1 variable (also with
#'   `avg_slopes()`), which is Stata's `dydx(i.b)`; Stata's `dydx(b)`
#'   without `i.` is the derivative at the observed values. The two can be
#'   far apart: about 9% for a logit coefficient of 1.5 at a baseline risk
#'   near 30% (0.1-0.3% where the effects are small). For
#'   parity use `dydx(i.b)` in Stata; `effecttab()` says so once per
#'   session.
#'
#' @param ... Models: marginaleffects results, data frames, matrices, or
#'   lists of pieces (see Input). Named arguments label the models.
#' @param type `"auto"`, `"teffects"` or `"margins"` (see Teffects and
#'   margins).
#' @param effect Header of the estimate columns (default `"Effect"` for
#'   treatment effects, `"Estimate"` otherwise), e.g. `"ATE"`, `"RD"`,
#'   `"AME"`, `"Pr(Y)"`.
#' @param models Model labels over each block: a character vector, or a
#'   Stata-style string `"IPW \\ AIPW"`. Default: the names of named models,
#'   else blank.
#' @param clean Label treatment contrasts and potential-outcome means (see
#'   Teffects and margins); default `FALSE` (Stata's raw keys).
#' @param tlabels Treatment level labels, named by level code
#'   (`c("0" = "SSRI", "1" = "SNRI")`), or Stata's string `'0 "SSRI" 1
#'   "SNRI"'`. Implies `clean`.
#' @param data A data frame whose columns supply variable and value labels
#'   (default: each marginaleffects result's model data).
#' @param method The teffects estimator for the methods sentence of a
#'   single treatment-effects model: `"ipw"`, `"ra"`, `"aipw"`, `"ipwra"`,
#'   `"psmatch"` or `"nnmatch"` (default: a data frame's `tt_estimator`
#'   attribute, or what a `glm_weightit()` fit shows).
#' @param level Confidence level, a proportion (`0.9`) or a percentage
#'   (`90`); see Confidence level.
#' @param digits Decimals for estimates and bounds, 0 to 6 (default from
#'   [tabtools_options()], else 2).
#' @param pdp,highpdp Decimals for p-values below 0.10 and from 0.10 on (1 to
#'   10; defaults 3 and 2).
#' @param sep Separator between the interval bounds (default `", "`).
#' @param refcat,omitlabel,emptylabel Text of base-level, omitted and empty
#'   cells (defaults `"Reference"`, `"Omitted"`, `"Empty"`); must differ.
#' @param addrow Extra rows below the table: a named list
#'   (`list(N = c(4642, 4642))`, at most one value per model, in model
#'   order: fewer leave the later models' cells blank, extra values are
#'   ignored, as in Stata), or Stata's string `'"N" 4642 4642'`.
#' @param labelwidth Cap on the label column's width (default 45).
#' @param title,footnote Title in cell A1 and footnote below the table.
#' @param xlsx Output `.xlsx` workbook (the sheet is replaced if it exists).
#' @param sheet Sheet name (default `"Effects"`).
#' @param open Open the workbook after writing (interactive sessions).
#' @param borderstyle,font,fontsize,headercolor,zebracolor Styling, as in
#'   [regtab()] (defaults from [tabtools_options()]).
#' @param boldp Bold p-values below this threshold.
#' @param highlight Shade rows whose p-value is below this threshold.
#' @param zebra,headershade Shade every second body row / the header rows.
#' @param csv,markdown Also write the table to this CSV or Markdown file.
#' @param mdappend Append to an existing `markdown` file.
#' @param estimand What a treatment-effect table estimates, when the results
#'   cannot say: `"ATE"`, `"ATT"` (Stata's ATET) or `"ATC"`. It names the
#'   effect in the methods sentence and overrides what is read from a
#'   `WeightIt::glm_weightit()` fit or an `ATET` equation. Default `NULL`:
#'   read from the results, else average treatment effects. R only.
#' @return A `tt_table` (`command = "effecttab"`), returned invisibly when
#'   `xlsx`, `csv` or `markdown` writes a file (assign it and print it to see
#'   it). `$stored` holds Stata's `r()` results: `N_rows`, `N_cols`,
#'   `ci_level`, `effect_label`, `type`, `methods`, the matrix `table`
#'   (estimate and p-value per model, one row per row with a number), and,
#'   for the files written, `xlsx`, `sheet`, `markdown`, `markdown_rows`,
#'   `markdown_cols`. [as.data.frame()] of the table carries Stata's frame
#'   characteristics as attributes (`source = "effecttab"`, `ci_level`,
#'   `n_models`, `statistic_ids`, and per model `model_id`, `outcome_id`
#'   (blank), `effect_scale` and `model_label`). It also retains per-model
#'   `effect_additive` (`TRUE`, `FALSE` or `NA`) and `effect_log_scale`
#'   (`"log"`, `"ratio"`, `"unknown"`, or `""` when no log-ratio comparison
#'   is identified); [hrcomptab()] uses these to reject incompatible scales.
#'   `$meta$effect_rows` holds the rows [tt_effect_rows()] read.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @seealso [tt_effect_rows()] for the input rows; [regtab()] for
#'   coefficient tables; [as_forest_data()] for plotting.
#' @examples
#' if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'   d <- mtcars
#'   d$am <- factor(d$am, 0:1, c("Automatic", "Manual"))
#'   attr(d$am, "label") <- "Transmission"
#'   fit <- glm(vs ~ am + wt, family = binomial, data = d)
#'   # A treatment contrast (risk difference) and the control group's
#'   # potential-outcome mean, as one treatment-effect model
#'   ate <- marginaleffects::avg_comparisons(fit, variables = "am")
#'   po <- marginaleffects::avg_predictions(fit, variables = list(am = "Automatic"))
#'   print(effecttab(list(ate, po), clean = TRUE, effect = "RD"))
#'   # Predictive margins, and average marginal effects of two variables
#'   print(effecttab(marginaleffects::avg_predictions(fit, variables = "am"),
#'                   effect = "Pr(straight engine)"))
#'   print(effecttab(marginaleffects::avg_comparisons(fit, variables = "am"),
#'                   marginaleffects::avg_slopes(fit, variables = "wt"),
#'                   effect = "AME", models = c("Transmission", "Weight")))
#' }
#'
#' # Estimates from elsewhere, as a data frame: here Stata's r(table) after
#' # `teffects ipw ..., ate`
#' rt <- data.frame(equation = c("ATE", "POmean"),
#'                  term = c("r1vs0.smoke", "0.smoke"),
#'                  estimate = c(-262.98, 3406.38),
#'                  conf.low = c(-307.69, 3387.86), conf.high = c(-218.27, 3424.90),
#'                  p.value = c(9.5e-31, 0))
#' effecttab(rt, tlabels = c("0" = "Nonsmoker", "1" = "Smoker"), method = "ipw")
#'
#' # Or as a matrix with columns estimate, lower, upper and p-value
#' m <- matrix(c(0.12, 0.05, 0.19, 0.0004,
#'               -0.03, -0.08, 0.02, 0.25), 2, byrow = TRUE,
#'             dimnames = list(c("Treated_vs_control", "Dose_high"), NULL))
#' effecttab(m, effect = "RD")
#' @export
effecttab <- function(..., type = c("auto", "teffects", "margins"), effect = NULL, models = NULL,
                      clean = FALSE, tlabels = NULL, data = NULL, method = NULL, level = NULL,
                      digits = NULL, pdp = 3, highpdp = 2, sep = ", ", refcat = "Reference",
                      omitlabel = "Omitted", emptylabel = "Empty", addrow = NULL, labelwidth = 45,
                      title = NULL, footnote = NULL, xlsx = NULL, sheet = "Effects", open = FALSE,
                      borderstyle = NULL, font = NULL, fontsize = NULL, boldp = NULL, highlight = NULL,
                      zebra = FALSE, headershade = FALSE, headercolor = NULL, zebracolor = NULL,
                      csv = NULL, markdown = NULL, mdappend = FALSE, estimand = NULL) {
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Effects"
  type <- match.arg(type)
  estimand_given <- .et_check_estimand(estimand)
  fits <- .et_models(list(...), type)
  M <- length(fits)
  for (a in c("clean", "open", "zebra", "headershade", "mdappend")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  # effecttab.ado:91-104: the three constrained-cell labels must differ.
  for (a in c("refcat", "omitlabel", "emptylabel")) .check_string(get(a), a)
  if (refcat == omitlabel || refcat == emptylabel || omitlabel == emptylabel) {
    cli::cli_abort("{.arg refcat}, {.arg omitlabel}, and {.arg emptylabel} must differ from each other.", call = NULL)
  }
  sheet <- .check_sheet(sheet)
  if (is.null(digits)) digits <- getOption("tabtools.digits") %||% 2
  digits <- .check_int_range(digits, "digits", 0, 6)
  pdp <- .check_dp(pdp, "pdp")
  highpdp <- .check_dp(highpdp, "highpdp")
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) cli::cli_abort("{.arg sep} must be a single string.", call = NULL)
  if (!nzchar(sep)) sep <- ", "
  level_pct <- .et_check_level(level)
  if (!is.numeric(labelwidth) || length(labelwidth) != 1L || is.na(labelwidth)) {
    cli::cli_abort("{.arg labelwidth} must be a number.", call = NULL)
  }
  if (labelwidth <= 0) labelwidth <- 45
  for (a in c("title", "footnote", "effect")) .tt_check_text_arg(get(a), a)
  # effect("") is Stata's default header (effecttab.ado:445).
  if (!is.null(effect) && !nzchar(effect)) effect <- NULL
  if (!is.null(method)) {
    if (!is.character(method) || length(method) != 1L || !tolower(method) %in% names(.et_estimators)) {
      cli::cli_abort("{.arg method} must be one of {.or {.val {names(.et_estimators)}}}.", call = NULL)
    }
  }
  if (!is.null(data) && !is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.", call = NULL)
  tlabels <- .et_parse_tlabels(tlabels)
  if (!is.null(tlabels)) clean <- TRUE
  has_xlsx <- !is.null(xlsx)
  .tt_check_sheet_xlsx(sheet_given, has_xlsx)
  has_md <- !is.null(markdown)
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must have a .xlsx extension.", call = NULL)
  }
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  if (!is.null(csv)) .tt_check_csv_path(csv)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L || is.na(markdown) ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra, headercolor = headercolor,
                            zebracolor = zebracolor, boldp = boldp, highlight = highlight)
  addrow <- .rt_parse_addrow(addrow)
  labels <- .et_model_labels(models, attr(fits, "labels"), M)

  # The table type (effecttab.ado:332-338, :384-442), then each model's rows.
  resolved <- .et_resolve_type(fits, type)
  from_matrix <- resolved$from_matrix
  type <- resolved$type
  mrows <- vector("list", M)
  levels <- numeric()
  source <- character(M)
  estimator <- NULL
  estimand <- NULL
  notes <- vector("list", M)
  model_id <- character(M)
  additive <- rep(NA, M)
  log_scale <- rep("", M)
  p_null_default <- rep(FALSE, M)
  samples <- vector("list", M)
  for (m in seq_len(M)) {
    pieces <- .et_pieces(fits[[m]])
    samples[[m]] <- .tt_sample_bind(
      lapply(pieces, .et_sample_piece, model = m),
      prefixes = paste0("piece", seq_along(pieces)),
      commands = rep("effecttab", length(pieces)))
    add <- vapply(pieces, .et_me_additive, NA)
    additive[m] <- if (any(add %in% TRUE)) TRUE else if (all(add %in% FALSE)) FALSE else NA
    logs <- vapply(pieces, .et_me_log_scale, "")
    log_scale[m] <- if (any(logs == "log")) "log" else if (any(logs == "unknown")) "unknown" else if (any(logs == "ratio")) "ratio" else ""
    rows <- lapply(pieces, function(p) tt_effect_rows(p, type = type, data = data,
                                                   level = if (is.na(level_pct)) NULL else level_pct))
    levels <- c(levels, vapply(rows, function(r) attr(r, "level") %||% NA_real_, 0))
    src <- unique(vapply(rows, function(r) attr(r, "source"), ""))
    source[m] <- if (length(src) == 1L) src else "mixed"
    if (m == 1L) {
      estimator <- unlist(lapply(rows, function(r) attr(r, "estimator")))[1]
      estimand <- unlist(lapply(rows, function(r) attr(r, "estimand")))[1]
    }
    notes[[m]] <- unique(unlist(lapply(rows, function(r) attr(r, "notes"))))
    p_null_default[m] <- any(vapply(rows, function(r) isTRUE(attr(r, "p_null_default")), NA))
    model_id[m] <- if (from_matrix) paste0("matrix:", m) else attr(rows[[1]], "model_id") %||% ""
    r <- do.call(rbind, rows)
    # teffects rows: contrasts first, then the potential-outcome means.
    if (type == "teffects") r <- r[order(match(r$kind, c("contrast", "pomean"))), , drop = FALSE]
    if (anyDuplicated(r$key)) {
      cli::cli_abort("Model {m} holds row {.val {r$key[duplicated(r$key)][1]}} more than once.", call = NULL)
    }
    rownames(r) <- NULL
    mrows[[m]] <- r
  }
  ci_level <- .et_resolve_level(levels, level_pct, from_matrix)
  if (is.null(effect)) effect <- if (type == "teffects") "Effect" else "Estimate"
  if (any(p_null_default) && .et_is_ratio_header(effect)) {
    cli::cli_abort(c("The p-values of model {which(p_null_default)[1]} were derived from {.field std.error} against a null of 0, but the effect is headed {.val {effect}}, a ratio.",
                     "i" = "Add a {.field null.value} column (1 for a ratio on its own scale), or a {.field p.value} column."),
                   class = "tabtools_error_df_null", call = NULL)
  }
  # Notes the inputs raise (p-values of ratios recomputed against 1, a
  # non-zero null) join the footnote, in every sink.
  footnote <- .et_add_notes(footnote %||% "", .et_model_notes(notes, labels))
  o <- list(type = type, from_matrix = from_matrix, effect = effect, models = labels, clean = clean,
            tlabels = tlabels, digits = digits, pdp = pdp, highpdp = highpdp, sep = sep, refcat = refcat,
            omitlabel = omitlabel, emptylabel = emptylabel, addrow = addrow, labelwidth = labelwidth,
            ci_level = ci_level, estimator = if (is.null(method)) estimator else tolower(method),
            estimand = estimand_given %||% estimand, source = source[1], model_id = model_id, additive = additive,
            log_scale = log_scale, title = title %||% "", footnote = footnote,
            style = style, sheet = sheet,
            sample_accounting = .tt_sample_bind(samples, prefixes = paste0("model", seq_len(M)),
                                                commands = rep("effecttab", M)))
  tt <- tt_effecttab_build(mrows, o)

  written <- FALSE
  if (has_xlsx) {
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx
    tt$stored$sheet <- sheet
    written <- TRUE
  }
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    written <- TRUE
  }
  if (has_md) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}

# ---------------------------------------------------------------------------
# Arguments

.et_is_me <- function(x) inherits(x, c("predictions", "comparisons", "slopes", "hypotheses"))

# Effect rows are summaries, so their row count cannot identify the cohort.
# A marginaleffects result can additionally retain a fitted model and a
# separate evaluation grid; preserve these populations without combining N.
.et_sample_piece <- function(x, model) {
  unknown <- stats::setNames(rep(list(NA_real_), length(.tt_sample_metrics)), .tt_sample_metrics)
  if (!.et_is_me(x)) {
    return(.tt_sample_population(
      "summary", "effecttab", "summary", model = model, weight_type = "unknown",
      values = unknown,
      reasons = stats::setNames(rep(list("supplied effect summaries do not identify observation counts"),
                                    length(.tt_sample_metrics)), .tt_sample_metrics)))
  }
  fit <- .et_me_component(x, "model")
  fitted <- if (is.null(fit)) {
    .tt_sample_population("fit", "effecttab", "model", model = model, weight_type = "unknown",
                          values = unknown,
                          reasons = stats::setNames(rep(list("fitted model is not stored in the effect result"),
                                                        length(.tt_sample_metrics)), .tt_sample_metrics))
  } else .tt_sample_model_population(fit, "fit", model = model, command = "effecttab")
  grid <- .et_me_component(x, "newdata")
  evaluation <- .tt_sample_population(
    "evaluation", "effecttab", "evaluation", model = model, weight_type = "unknown",
    values = list(input_n = if (is.data.frame(grid)) as.numeric(nrow(grid)) else NA_real_,
                  frame_n = if (is.data.frame(grid)) as.numeric(nrow(grid)) else NA_real_,
                  eligible_n = NA_real_, observed_n = NA_real_, used_n = NA_real_,
                  missing_n = NA_real_, excluded_n = NA_real_, zero_weight_n = NA_real_,
                  weight_sum = NA_real_, effective_n = NA_real_),
    bases = list(input_n = "stored marginaleffects evaluation grid",
                 frame_n = "stored marginaleffects evaluation grid"),
    reasons = list(input_n = if (is.data.frame(grid)) "" else "evaluation grid is not stored",
                   frame_n = if (is.data.frame(grid)) "" else "evaluation grid is not stored",
                   eligible_n = "effect summaries do not identify evaluation eligibility",
                   observed_n = "effect summaries do not identify nonmissing evaluation outcomes",
                   used_n = "effect summaries do not identify contributing evaluation records",
                   missing_n = "effect summaries do not identify missing evaluation outcomes",
                   excluded_n = "evaluation input and contributing records cannot be reconciled",
                   zero_weight_n = "evaluation record weights are not established",
                   weight_sum = "evaluation record weights are not established",
                   effective_n = "evaluation record weights are not established"))
  .tt_sample_bind(list(fitted, evaluation), prefixes = c("fit", "evaluation"),
                  commands = rep("effecttab", 2L))
}

# Sentences appended to the footnote, as regtab's vce_note is: after a
# sentence end with a space, else after "; ".
# A note every model raises is said once; a note of some models only is
# prefixed with each such model's label ("Model m" when unlabelled), so
# a table of a ratio tested against 1 beside one tested against 2 does not
# claim both nulls for every column (review of 2026-09-28, M1).
.et_model_notes <- function(notes, labels) {
  M <- length(notes)
  all_notes <- unique(unlist(notes))
  if (M <= 1L || !length(all_notes)) return(all_notes %||% character())
  common <- Reduce(intersect, notes)
  lab <- ifelse(nzchar(labels %||% rep("", M)), labels, paste("Model", seq_len(M)))
  own <- unlist(lapply(seq_len(M), function(m) {
    n <- setdiff(notes[[m]], common)
    if (length(n)) paste0(lab[m], ": ", n)
  }))
  c(common, own)
}

.et_add_notes <- function(fn, notes) {
  for (note in notes) {
    fn <- trimws(fn)
    fn <- if (!nzchar(fn)) note else if (grepl("[.;:!?]$", fn)) paste(fn, note) else paste0(fn, "; ", note)
  }
  fn
}

# A single unnamed list of one comparison and one prediction of the same
# variable is one treatment-effect model (the contrast and its
# potential-outcome mean), not two margins models (review F10); with
# type = "margins" it stays two models.
.et_is_te_pair <- function(l, type) {
  if (identical(type, "margins") || length(l) != 2L || !is.null(names(l))) return(FALSE)
  cmp <- Filter(function(p) inherits(p, "comparisons") && !"hypothesis" %in% names(p), l)
  prd <- Filter(function(p) inherits(p, "predictions") && !"hypothesis" %in% names(p), l)
  if (length(cmp) != 1L || length(prd) != 1L || !"term" %in% names(cmp[[1]])) return(FALSE)
  md <- .et_me_component(prd[[1]], "modeldata")
  focal <- setdiff(names(prd[[1]]), .et_me_stat_cols)
  if (is.data.frame(md)) focal <- intersect(focal, names(md))
  length(focal) == 1L && identical(unique(as.character(cmp[[1]]$term)), focal)
}
.et_is_piece <- function(x) .et_is_me(x) || is.data.frame(x)
.et_is_plain_list <- function(x) is.list(x) && !is.data.frame(x) && (!is.object(x) || identical(class(x), "list"))

# The models in `...`: a single unnamed plain list is unpacked (one model
# per element, its names the labels), as regtab does.
.et_models <- function(fits, type = "auto") {
  nm <- names(fits) %||% rep("", length(fits))
  typo <- which(nzchar(nm) & vapply(fits, function(f) is.null(f) || (is.atomic(f) && !is.matrix(f)), TRUE))
  if (length(typo)) cli::cli_abort("{.fn effecttab} has no argument {.arg {nm[typo[1]]}}.", call = NULL)
  if (length(fits) == 1L && !nzchar(nm[1]) && .et_is_plain_list(fits[[1]]) && !.et_is_te_pair(fits[[1]], type)) {
    fits <- fits[[1]]
    nm <- names(fits) %||% rep("", length(fits))
  }
  if (!length(fits)) cli::cli_abort("Supply at least one result to format.", call = NULL)
  for (m in seq_along(fits)) {
    f <- fits[[m]]
    ok <- .et_is_piece(f) || is.matrix(f) ||
      (.et_is_plain_list(f) && length(f) && all(vapply(f, .et_is_piece, TRUE)))
    if (!ok) {
      if (.et_is_plain_list(f) && any(vapply(f, is.matrix, TRUE))) {
        cli::cli_abort("Model {m}: a matrix is a model of its own, not a piece of one.", call = NULL)
      }
      tt_effect_rows(if (.et_is_plain_list(f) && length(f)) f[[which(!vapply(f, .et_is_piece, TRUE))[1]]] else f)
    }
  }
  structure(unname(fits), labels = nm)
}

.et_pieces <- function(f) if (.et_is_plain_list(f)) unname(f) else list(f)

# Whether a marginaleffects result's numbers are on an additive scale (a
# prediction, a slope, a difference or a raw log-ratio comparison): TRUE,
# FALSE for a ratio, NA when unknown (a data frame, a matrix, a hypothesis
# test, or a log ratio with an unevaluated transform). hrcomptab() refuses
# an additive model whatever its
# `effect` header says (Muse audit P0-5).
.et_me_additive <- function(x) {
  if (inherits(x, c("predictions", "slopes"))) return(!("hypothesis" %in% names(x)))
  if (!inherits(x, "comparisons") || "hypothesis" %in% names(x)) return(NA)
  contrast <- if ("contrast" %in% names(x)) as.character(x$contrast) else character()
  contrast <- contrast[!is.na(contrast)]
  if (!length(contrast)) return(NA)
  log_scale <- .et_me_log_scale(x)
  if (log_scale == "log") return(TRUE)
  if (log_scale == "unknown") return(NA)
  if (log_scale == "ratio") return(FALSE)
  !any(.et_is_ratio(contrast) | grepl("^ln\\(", trimws(contrast)))
}

# marginaleffects applies transform after inference. Its components expose
# the recorded call, not the evaluated transform: only literal exp (or its
# documented string shortcut) establishes that a log ratio is a ratio.
.et_me_log_scale <- function(x) {
  if (!inherits(x, "comparisons") || "hypothesis" %in% names(x)) return("")
  cmp <- .et_me_component(x, "comparison")
  log_ratio <- is.character(cmp) && any(grepl("^ln(ratio|or)", cmp))
  if (!log_ratio) return("")
  cl <- .et_me_component(x, "call")
  if (!is.call(cl)) return("unknown")
  tr <- cl$transform
  if (is.null(tr)) return("log")
  if ((is.character(tr) && identical(tr, "exp")) || identical(tr, quote(exp)) ||
      identical(tr, quote(base::exp)) || (is.function(tr) && identical(tr, base::exp))) return("ratio")
  "unknown"
}

# Whether an effect header names a ratio (OR, RR, HR, IRR, RRR, TR, SHR,
# their adjusted forms, or anything "... ratio").
.et_is_ratio_header <- function(effect) {
  e <- gsub("[ ._/()-]", "", tolower(trimws(effect)))
  grepl("^a?(or|rr|hr|irr|rrr|tr|shr|sdhr|csh)$", e) || grepl("ratio", e, fixed = TRUE)
}

# effecttab(estimand =): what the result estimates, when it cannot say
# (an avg_comparisons() of ATT weights, of a matched sample, or a data
# frame); Muse audit P1-26. ATT is Stata's ATET.
.et_check_estimand <- function(estimand) {
  if (is.null(estimand)) return(NULL)
  ok <- c(ATE = "ATE", ATT = "ATET", ATET = "ATET", ATC = "ATC")
  if (!is.character(estimand) || length(estimand) != 1L || is.na(estimand) || !toupper(estimand) %in% names(ok)) {
    cli::cli_abort("{.arg estimand} must be one of {.val {c('ATE', 'ATT', 'ATC')}} ({.val ATET} is Stata's ATT).",
                   call = NULL)
  }
  unname(ok[toupper(estimand)])
}

.et_check_level <- function(level) {
  if (is.null(level)) return(NA_real_)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 100 || level == 1) {
    cli::cli_abort("{.arg level} must be a proportion in (0, 1) or a percentage in (1, 100).", call = NULL)
  }
  round(if (level < 1) level * 100 else level, 10)
}

# Model labels (effecttab.ado:833-851): given labels in order, blank by
# default (EO10; regtab's default is "Model"); a named model labels itself.
.et_model_labels <- function(models, names, M) {
  out <- names
  if (is.null(models)) return(out)
  if (!is.character(models) || anyNA(models)) cli::cli_abort("{.arg models} must be a character vector of labels.", call = NULL)
  if (length(models) == 1L && grepl("\\", models, fixed = TRUE)) {
    models <- trimws(strsplit(models, "\\", fixed = TRUE)[[1]])
    models <- models[nzchar(models)]
  }
  if (length(models) > M) {
    cli::cli_warn("{.arg models} has {length(models)} labels for {M} model{?s}; the extra labels are ignored.")
  }
  k <- min(length(models), M)
  out[seq_len(k)] <- models[seq_len(k)]
  out
}

# teffects or margins, from what each model holds: a data frame that says
# (its kind column, a teffects equation or key) and a slopes result are
# firm; a model stacking a comparison and a prediction is a treatment
# contrast with its potential-outcome mean. A matrix is margins unless
# `type` says otherwise (effecttab.ado:429-442).
.et_resolve_type <- function(fits, type) {
  is_mat <- vapply(fits, is.matrix, TRUE)
  if (any(is_mat)) {
    if (!all(is_mat)) cli::cli_abort("A matrix (Stata's from()) cannot be combined with other results in one table.", call = NULL)
    return(list(type = if (type == "auto") "margins" else type, from_matrix = TRUE))
  }
  firm <- character()
  pair <- FALSE
  for (f in fits) {
    p <- .et_pieces(f)
    for (x in p) {
      if (inherits(x, c("slopes", "hypotheses")) || (.et_is_me(x) && "hypothesis" %in% names(x))) firm <- c(firm, "margins")
      if (is.data.frame(x) && !.et_is_me(x)) {
        k <- .et_df_kind(x)
        if (!is.na(k)) firm <- c(firm, if (k) "teffects" else "margins")
      }
    }
    if (any(vapply(p, inherits, TRUE, "comparisons")) && any(vapply(p, inherits, TRUE, "predictions"))) pair <- TRUE
  }
  firm <- unique(firm)
  if (length(firm) > 1L) {
    cli::cli_abort("{.fn effecttab} does not support mixing teffects and margins results in one table.", call = NULL)
  }
  if (type == "auto") {
    out <- if (length(firm)) firm else if (pair) "teffects" else "margins"
    return(list(type = out, from_matrix = FALSE))
  }
  if (length(firm) && firm != type) {
    cli::cli_abort("{.code type = \"{type}\"} does not match the results ({firm}).", call = NULL)
  }
  list(type = type, from_matrix = FALSE)
}

# The confidence level (percent) of the results: every result that records
# one must agree with the others and with `level`; with none recorded,
# `level`, else 95 (effecttab.ado:245-259; _tabtools_resolve_ci_level).
.et_resolve_level <- function(levels, level_pct, from_matrix) {
  known <- levels[!is.na(levels)]
  if (length(known)) {
    txt <- .et_level_text(unique(known))
    if (any(abs(known - known[1]) > 1e-8)) {
      cli::cli_abort("The results hold intervals at different confidence levels ({txt}%).", call = NULL)
    }
    if (!is.na(level_pct) && abs(level_pct - known[1]) > 1e-8) {
      given <- .et_level_text(level_pct)
      cli::cli_abort("{.arg level} ({given}) conflicts with the results' {txt[1]}% intervals.", call = NULL)
    }
    return(known[1])
  }
  if (!is.na(level_pct)) return(level_pct)
  if (!from_matrix) {
    cli::cli_inform(c("i" = "The results do not record their confidence level; assuming 95%.",
                      " " = "Give {.arg level} if the intervals are at another level."))
  }
  95
}
