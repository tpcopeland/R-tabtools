# tabtools (development version)

These follow Stata tabtools 2.4.0. Until the Stata goldens are regenerated
from 2.4.0, the golden tests translate the new pair header, note and
footnote back to 2.1.14's form.

* Continuous-variable SDs are computed about the mean, as Stata 2.1.15
  does, in the unweighted, `wt` and `fweight` paths. Values far from zero
  (`1e8 + c(0.1, 0.2, 0.3, 0.4)`) no longer show SD 0 or a missing SD from
  cancelling raw moments. A constant column that showed `2.68±.` now
  shows `2.68±0.00`, and geometric SDs (`contln`) change with it. A
  weighted SD is still missing when the square of the centered weighted sum
  overflows, as in Stata.
* `table1_tc()` with three or more groups and `smd = TRUE` names the pair
  its SMD compares wherever the table goes. The column header reads
  `SMD (A vs B)`, a footnote says "SMD compares A vs B only (the first two
  of 3 groups).", and the console note uses the same text. Before, only a
  console note said so, and every export (Excel, CSV, Markdown, gt,
  tinytable, `as.data.frame()`) showed a bare "SMD" column.
* New `smdpair`: the two groups a pair SMD compares, by `by` value or
  label. The header names the pair, and with three or more groups the
  footnote adds "chosen with smdpair()".
* New `smdtype` for balance across all groups. `"population"` reports the
  population standardized bias of McCaffrey et al. (2013, eq. 5 and
  section 4.2): the largest |group mean - overall mean| / overall SD.
  `"maxpair"` reports the largest pairwise SMD (Lopez and Gutman 2017,
  eq. 27), with every pair on the all-groups pooled SD. `"pair"` (the
  default) is the existing statistic. Without weights both new types
  match cobalt 4.6.3 on continuous and binary variables.
* `stored$smdtype` (with `smd = TRUE`) and `stored$smdnote` (when there
  is a note), as Stata's `r(smdtype)` and `r(smdnote)`.
* `table1_tc(smdpair=)` now resolves a token by value or by label as Stata does:
  a numeric token that only matches a value label (labels "2020", "2021") works;
  a token that is one group's value and another's label is refused unless the
  new `smdpair_as = "values"` or `"labels"` (Stata `smdpair(..., values|labels)`)
  says which is meant. A logical `by` accepts `TRUE`/`FALSE` in `smdpair`.
* The SMD column headers `SMD (A vs B)`, `Pop. SB` and `Max SMD` are recognised
  as the SMD role by hand-built tables.
* When a `table1_tc()` footnote uses ` \ ` paragraphs, the automatic small-cell
  and SMD notes are appended as their own paragraphs.

* `effecttab()` warns once per call (class `tabtools_warning_ratio_interval`),
  naming each term, when a data frame stating a ratio null (`null.value = 1`)
  or headed as a ratio (`effect = "RR"`, `"OR"`, ...) derives a Wald interval
  from `std.error` whose lower bound is 0 or below. The linear delta-method
  interval is kept, as Stata's `nlcom` and `margins` give it; supply a
  log-scale interval to avoid the warning. The `marginaleffects` ratio
  warning now also fires at a lower bound of exactly 0.
* `regtab()` on a `glmmTMB` fit says so, in a console note (class
  `tabtools_note_tmb_cov_ci`) and in the footnote of every sink, when a random-effect covariance interval is
  left blank because glmmTMB's parameterization could not be matched to
  `VarCorr()`. The refusal of `cs()`/`ar1()` terms now has class
  `tabtools_error_tmb_covstruct`.

* `table1_tc(smallcells =)` now protects counts that are not printed but follow
  from printed ones (Stata tabtools 2.1.17). A categorical variable's hidden
  missing count (group N minus the printed levels) and a binary variable's
  hidden negative and missing counts (from the denominator its percentage or
  `n/N` releases) are treated as primary cells when below `smallcells`: cells
  are coded `<k` or the greater-than-or-equal marker as needed, the
  variable's percentages are withheld, and its p-value reads `Suppressed`.
  Before, a table with `N=14`, `A 6 (50)`, `B 6 (50)` released a missing count
  of 2 by subtraction. In a table of two or more variables the group and
  total N are shared by every variable and are never withheld; a count that
  only withholding them could protect now stops with an error of class
  `tabtools_error_smallcells_shared_margin`. `smallcells` with `wtcompare` is
  refused unless `wtn` or `percent_n` is given, because the weighted columns
  would be percent-only.
  The sample-accounting ledger (`$meta$sample_accounting` and the
  `as.data.frame()` attribute) withholds the same counts, as `NA` with
  status `unavailable` and reason `suppressed_by_smallcells`.

# tabtools 0.1.0

First release: an R implementation of the Stata `tabtools` commands
`table1_tc`, `regtab`, `puttab`, `stacktab`, `stratetab`, `effecttab`,
`comptab` and `hrcomptab` (Stata tabtools 2.1.14), and a weight-diagnostics
table for inverse probability weighting. Everything a reader sees targets
cell-for-cell parity with Stata, checked against 272 Stata-generated golden
scenarios and the sheets of the Stata demo.

## Descriptive tables

* `table1_tc()` (alias `desctab()`) builds a baseline-characteristics
  table. Variables are auto-typed or declared (`contn`, `contln`, `conts`,
  `cat`, `cate`, `bin`, `bine`), in any of the three `vars` forms (names,
  named type specifications, or the Stata string). It supports every Stata
  cell-content option, totals, SMDs, missing-data rows, importance weights
  (`wt`, `wtcompare`, `wtn`, effective sample size), frequency weights
  (`fweight`) and small-cell suppression (`smallcells`).

## Regression tables

* `regtab()` builds a regression table from one or more fitted models:
  `lm`, `glm` (every family and link), `MASS::glm.nb`, `survival::coxph`
  (including Fine-Gray fits on `finegray()` data) and `clogit`,
  `survival::survreg`, `AER::tobit`, `cmprsk::crr`, `MASS::polr`,
  `ordinal::clm`, `nnet::multinom`, `pscl::zeroinfl`, `pscl::hurdle`,
  `lme4::lmer`/`glmer`, `nlme::lme`, `glmmTMB::glmmTMB`,
  `geepack::geeglm`, `survey::svyglm` (Stata's `svy:`) and
  `WeightIt::glm_weightit` (Stata's `teffects ipw`). Any other model can be
  passed as a `broom.helpers::tidy_plus_plus()`-shaped data frame, or as a
  modelsummary list through `tt_from_modelsummary()`.
* Effect labels (OR, HR, IRR, RRR, TR, SHR) and the methods sentence are
  built from the models. Intercept, cutpoint and ancillary rows are
  identified from the model, never by label. Multi-model row union,
  `keep`/`drop` with Stata keys (`2.treat`), fvgen-style or native
  interactions, random-effects rows, model statistics (`stats`), `addrow`,
  `stat_fun`, stars and `dimnonsig`.
* Standard errors are those Stata reports by default (`vce = "stata"`), or
  `"model"`, `"robust"` (Stata's `vce(robust)`/`[pweight]`), `"cluster"`
  with `cluster = ~id`, or a user-supplied variance (a function or matrix).
  `vce` can be set per model and `vce_note = TRUE` names each model's
  variance in the footnote. `tt_vcov()` returns the matrix;
  `tt_vce_types()` and `tt_ci_methods()` list what a fit supports.
* Multiply imputed models are pooled by Rubin's rules: a `mice` `mira` or
  `tt_mi()` of a list of fits, as Stata's `mi estimate:`.
* `regtab_uv()` fits one model per covariate from a formula template and
  shows them as one column, for crude-and-adjusted tables.
* Unconverged or unidentifiable fits are refused with a hint rather than
  shown with misleading numbers.

## Incidence rates and the cohort "Table 2"

* `tt_rates()` (Stata `strate`) computes events, person-time, rates and
  log-normal intervals per group.
* `stratetab()` (Stata `stratetab`) combines rate blocks per outcome and
  exposure into one table, optionally with incidence rate ratios.
* `comptab()` and `hrcomptab()` (Stata `comptab` and `hrcomptab`) put
  hazard ratios beside events, person-years and rates, or stack selected
  rows of several `regtab()` or `effecttab()` tables. Rows are placed by
  label and outcome identity, never by position.

## Treatment effects and margins

* `effecttab()` (Stata `effecttab`) formats treatment effects and margins
  from marginaleffects results, data frames (including Stata's
  `r(table)`) and matrices. `tt_effect_rows()` is its S3 input stage.

## Weight diagnostics

* `wttab()` tabulates inverse probability weights: distribution summaries
  and Kish's effective sample size, overall, by treatment group, by
  follow-up period and after truncation. It reads weight vectors, data
  frame columns, `WeightIt::weightit()` objects and `ipw` results.

## Composing and exporting tables

* `puttab()` (Stata `puttab`) writes a data frame, matrix or `tt_table` as
  one house-styled sheet.
* `stacktab()` (Stata `stacktab`) assembles blocks of existing sheets or
  `tt_table`s into one composite sheet.
* `tt_merge()` puts tables side by side, joining rows on their keys;
  `tt_stack()` stacks tables with the same columns as row groups.
* `as_forest_data()` gives the rows for a forest plot (Stata's
  `eplotframe()`).

## Output

* Every command returns a `tt_table`, printed as Stata's console listing;
  its stored results (Stata's `r()`) are in `$stored`, and analytical
  tables carry a sample-accounting ledger in `$meta$sample_accounting`.
* `xlsx =`, `csv =` and `markdown =` write the table as the Stata options
  do; `tt_write_xlsx()`, `tt_write_csv()` and `tt_write_markdown()` write
  it later, and `as.data.frame()` is Stata's `frame()`.
* `flextable::as_flextable()`, `tt_as_gt()`, `tt_as_gtsummary()` and
  `tt_as_tinytable()` carry the house style to Word, HTML, LaTeX, Typst
  and Quarto.
* `tabtools_options()` sets persistent defaults (Stata's `tabtools set`).
  `tt_as_factor()` converts haven-labelled columns to factors that keep
  Stata's value codes.
* `tt_table()` and `validate_tt_table()` expose the table object for other
  renderers. They are experimental: the object may gain fields.
* Numbers are rounded for display by the package's own exact
  binary-to-decimal conversion (Stata's rule), so the cells are identical
  on Linux, macOS and Windows.

## Where R differs from Stata on purpose

* Hypothesis tests use R's conventional tests (Welch t, Welch ANOVA,
  Wilcoxon, Kruskal-Wallis, uncorrected Pearson chi-squared, Fisher's
  exact); `test_args` changes them.
* regtab does not reproduce Stata output that is wrong or misleading, and
  R refuses input Stata cannot represent (infinite values, dates) or fits
  whose data changed after fitting.

`vignette("coming-from-stata")` lists every difference;
`vignette("compared-with-gtsummary")` sets the tables beside gtsummary's.
