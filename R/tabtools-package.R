#' tabtools: publication-ready tables and command catalog
#'
#' Describe samples, compare fitted models, cross-tabulate categories, calculate
#' pairwise correlations, summarize survival, and report rates, binary outcome
#' ratios or treatment effects. Tables print in the console, write house-styled
#' Excel, CSV and Markdown, and convert to flextable, gt, gtsummary or tinytable.
#' The reference suite is Stata tabtools 2.5.1, pinned at commit 712044f8.
#' Selected disclosure, overflow and formatting repairs follow the later
#' tabtools 2.5.2–2.5.6 source at commit 4eecca4d; this does not establish
#' complete equivalence to 2.5.6.
#' Table structure and formatting follow that source within the documented
#' numerical, estimator and inference boundaries; universal equivalence is not
#' claimed. [wttab()] and R model/presentation adapters are R additions.
#'
#' @section Command catalog:
#' * Baseline characteristics: [table1_tc()] and its alias [desctab()]. Select
#'   row types, weights, missing categories, SMD type/pair, strict or primary
#'   count protection and validated literal cell replacements.
#' * Regression: [regtab()] and [regtab_uv()]; [tt_mi()] pools supported
#'   multiply imputed fits. [tt_fitcount()] freezes accepted-record and term
#'   counts for `mincount`, `obs`, `events`, `people` and `exposure` reporting.
#'   [tt_failed_model()] explicitly represents a failed column. Fit-owned states,
#'   rich statistics, `reftop`, `cellnote`, insertion and `transpose` preserve
#'   analytical identities. [tt_vcov()], [tt_vce_types()] and [tt_ci_methods()]
#'   expose the variance/interval policies; [tt_from_modelsummary()] bridges a
#'   modelsummary list.
#' * Categorical associations: [crosstab()] reports counts/percentages,
#'   uncorrected Pearson or exact Fisher tests, sample OR with native Fisher-tail/Cornfield
#'   limits, RR/RD and signed Cochran-Armitage or Spearman trends.
#' * Correlations: [corrtab()] reports Pearson or Spearman with pairwise N,
#'   stars or p-values and lower/upper/full presentation. Raw `C`, `P`, `N`
#'   matrices retain original variable identities.
#' * Survival: [survtab()] reports KM probabilities, event/risk counts, median,
#'   RMST and two-group contrasts, with explicit delayed-entry/subject IDs,
#'   frequency weights and support boundaries.
#' * Rates: [tt_rates()] and [stratetab()] retain the legacy computed/saved-rate
#'   workflow. [ratetab()] supplies separate grouping sections, multiple outcomes,
#'   exact/log-Poisson or clustered intervals and analytical saved data.
#' * Binary outcomes: [outtab()] reports crude exposed/comparator counts and
#'   ordered crude/adjusted ratio models, sample reductions and fit diagnostics.
#' * Effects: [effecttab()] and [tt_effect_rows()] consume explicit estimates,
#'   matrices or marginaleffects results. Stata's `effecttab, full` has the
#'   deliberate alternative [regtab()] on the component fits.
#' * Composition: [comptab()] and [hrcomptab()] join rate/model tables, including
#'   multiple models per outcome, keyed categorical placement and model-only
#'   rows; authenticated numerical companions drive formats and reference states.
#'   [tt_merge()] and [tt_stack()] provide semantic model composition.
#' * Direct publication: [puttab()] styles data frames/matrices, with panels,
#'   spans, rules and integer formats; [stacktab()] assembles sheets or frame
#'   panels. [tt_flat()] exports keyed regression/effect rows or explicitly
#'   unkeyed publication frames. [tt_as_factor()] imports Stata value labels.
#' * Weight diagnostics: [wttab()] reports distribution, balance-related weight
#'   summaries and effective sample size.
#' * Renderers: [tt_write_xlsx()], [tt_write_csv()], [tt_write_markdown()],
#'   [as_flextable.tt_table()], [tt_as_gt()], [tt_as_gtsummary()] and
#'   [tt_as_tinytable()]. [as_forest_data()] exposes eligible publication
#'   inference; [tt_table()] and [validate_tt_table()] define the table contract.
#'
#' @section Compact recipes:
#' `table1_tc(d, by = "group", vars = c(age = "contn", event = "bin"))`
#' describes a sample. `regtab(fit1, fit2, models = c("Crude", "Adjusted"))`
#' compares fitted models. Capture `fc <- tt_fitcount(fit, events = "event",
#' data = d, terms = TRUE)` immediately after fitting; then use
#' `regtab(fit, fitcounts = fc, mincount = 3)` for reporting hygiene.
#'
#' `tabcell("np", n = 2, d = 10, ci = "exact")` formats a percentage cell;
#' `crosstab(d, "event", "group", or = TRUE)` reports a categorical comparison;
#' `corrtab(d, vars = c("age", "score"), spearman = TRUE)` reports pairwise ranks.
#' `ratetab(d, by = "group", events = "event", exposure = "years")` reports rates;
#' `outtab(d, outcomes = "event", exposure = "treated", models = list(~ age))`
#' reports crude counts and an adjusted ratio. `survtab(d, "time", "died",
#' times = c(1, 2), by = "group", rmst = 2)` reports survival summaries.
#' Each recipe assumes the input types and statistical domain specified by its
#' help page. Table builders return `tt_table`; `tabcell()` returns `tt_cell`
#' text with aligned provenance and `tt_fitcount()` returns a frozen record.
#'
#' @section Publication and analytical values:
#' Table 1/crosstab strict masks redact protected returned numerics and sample
#' decompositions. Table 1 primary mode retains explicitly labeled analytical
#' aggregates separately, while protected publication counts/percentages and
#' linked ESS remain unavailable. Crosstab primary returns redact protected
#' counts but retain computed tests. Neither primary policy promises
#' complementary protection against reconstruction.
#'
#' Rate/outcome primary masks and regression `mincount` mask publication text,
#' retaining raw analytical returns. Regression `stored$table` is raw;
#' `stored$publication_table` and publication row companions govern eligible
#' forest values. Literal replacements invalidate affected publication inference
#' and cannot restore suppressed numerics. [tabcell()] redacts companions of
#' its masked percentage/rate cells. These policies are command-specific: a
#' protected printed table does not make its raw analytical payload shareable.
#'
#' @section Defaults and output destinations:
#' [tabtools_options()] with no arguments queries current defaults; query one
#' value with `tabtools_options()$digits`. Unnamed arguments are refused.
#' Named explicit NULL clears a session key. Session destinations are
#' opt-in; explicit command arguments, including NULL opt-outs, take precedence.
#' Ordinary table builders inherit session workbook and Markdown only when a
#' non-NULL `sheet` is explicitly supplied; an omitted default sheet requests
#' no inherited export. [puttab()] and [stacktab()] have their documented
#' command-specific omitted-destination inheritance. An explicit sheet with no
#' available workbook warns. `open = TRUE` and `mdappend = TRUE` require their
#' resolved sinks.
#' Successful session Markdown writes replace/create first and append later;
#' explicit append wins. Workbook writes preserve unrelated sheets.
#' Workbook alias priority is command-specific; Table 1 prioritizes non-NULL
#' `excel`, while correlation/cross-tabulation use nonempty `xlsx` first.
#'
#' @section Scope and migration:
#' Table 1 hypothesis tests use conventional R inference. Correlation and
#' Spearman trend inference follow the installed Stata 17 engine used by the
#' pinned suite, not the current manual's beta approximation. General formats
#' with 13 or more significant digits can differ in final rounding; `%ec` is
#' refused. Table 1 retains a finite SD at the documented native centered-sum
#' overflow boundary by explicit user choice. With accepted weights near
#' `8e307`, R can also retain finite SMDs where Stata 2.5.6 returns missing
#' values, separately from the finite SD choice. Selected later-source Table 1
#' guards close the earlier `slashN` denominator leaks and account for the
#' lower count bound released by a printed continuous summary. Strict masking
#' is not a blanket promise beyond supported layouts. MI `esampvaryok` is
#' unsupported.
#'
#' `vignette("coming-from-stata")` gives option translations and deliberate
#' differences; `vignette("regression-tables")` describes fitted-model support.
#' Unsupported estimators require an explicitly documented custom-frame
#' contract, not guessed metadata. `vignette("tabtools")` introduces the main
#' workflow; `vignette("weighted-analyses")` covers weighted analysis.
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
#' @importFrom stats ave setNames
#' @importFrom utils head tail
"_PACKAGE"
