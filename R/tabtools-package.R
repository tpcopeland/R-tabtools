#' tabtools: publication-ready descriptive and regression tables
#'
#' Baseline-characteristics tables, regression tables, incidence-rate tables,
#' treatment-effect tables and weight diagnostics, written to Excel (with a
#' house style), CSV and Markdown, or converted to flextable, gt, gtsummary
#' and tinytable. tabtools is an R implementation of the Stata `tabtools`
#' commands (Stata package version 2.1.14): everything a reader sees -- row
#' labels, category indentation, reference rows, number formats, headers,
#' and Excel styling -- matches the Stata output cell for cell, while
#' hypothesis tests use R's conventional defaults.
#'
#' Start with [table1_tc()] (a "Table 1" of baseline characteristics) and
#' [regtab()] (fitted models side by side); `vignette("tabtools")` walks
#' through both. By task:
#' * Describe a sample: [table1_tc()] (alias [desctab()]).
#' * Show regression models: [regtab()], with [regtab_uv()] for univariable
#'   models, [tt_mi()] for multiply imputed fits, and [tt_vcov()] for the
#'   variance behind the standard errors; `vignette("regression-tables")`.
#' * Incidence rates and the cohort-study "Table 2": [tt_rates()] computes
#'   the rates, [stratetab()] tabulates them, and [hrcomptab()] puts hazard
#'   ratios from [regtab()] beside them.
#' * Treatment effects and margins (from marginaleffects): [effecttab()].
#' * Inverse probability weights: [wttab()]; `vignette("weighted-analyses")`.
#' * Combine tables: [tt_merge()] (side by side), [tt_stack()] (one under
#'   another), [comptab()] (selected rows of model tables); [puttab()]
#'   styles any data frame or matrix, and [stacktab()] assembles Excel
#'   sheets into one composite.
#' * Write or convert: the commands' `xlsx`, `csv` and `markdown`
#'   arguments, or [tt_write_xlsx()], [tt_write_csv()],
#'   [tt_write_markdown()], [as_flextable.tt_table()], [tt_as_gt()],
#'   [tt_as_gtsummary()], [tt_as_tinytable()]; house-style defaults with
#'   [tabtools_options()].
#'   In every command `sheet`, `open` and `mdappend` need their target:
#'   `sheet` or `open` without `xlsx`, or `mdappend` without `markdown`, is
#'   an error (`sheet = NULL` is the same as leaving it out). Stata's
#'   `desctab` refuses `sheet()` without `excel()` too; its other commands
#'   ignore it.
#'
#' `vignette("coming-from-stata")` translates Stata calls option by option
#' and lists where R differs on purpose; `vignette("compared-with-gtsummary")`
#' sets the tables beside gtsummary's. [wttab()] has no Stata twin yet.
#'
#' @section Argument names:
#' Arguments are named by one rule:
#' * A Stata option keeps its Stata name, spelled as Stata spells it:
#'   `nointercept`, `headershade`, `mdappend`, and also `percent_n` and
#'   `slashN`.
#' * A name taken from another tool's interface keeps that tool's
#'   spelling: `finegray` (the Stata command), `xsymbol` and `vsref`
#'   (Stata `fvgen`), `s.weights` (WeightIt and cobalt), `method.args`
#'   (`ggplot2::geom_smooth()`).
#' * An argument new in R, with no name to copy, is snake_case:
#'   `ci_method`, `vce_df`, `vce_note`, `gee_as`, `stat_fun`,
#'   `test_args`, `by_labels`, `float_time`, `rownames_exact`.
#'
#' The writers [tt_write_xlsx()], [tt_write_csv()] and
#' [tt_write_markdown()] are R functions with R names (`path`, `append`, as
#' in [utils::write.csv()] and [cat()]); the commands' `xlsx`, `csv`,
#' `markdown` and `mdappend` are the Stata options.
#'
#' @aliases tabtools-overview
#' @useDynLib tabtools, .registration = TRUE
"_PACKAGE"
