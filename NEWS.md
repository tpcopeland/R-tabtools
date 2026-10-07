# tabtools 0.2.0

* Selected Stata tabtools 2.5.2–2.5.6 repairs are ported from pinned source
  `4eecca4d`: strict Table 1 denominator/continuous-summary guards, huge-weight
  recovery, CI/rate rounding, fractional and exponent rate units, unmerged
  puttab panel headings and SMD column widths. The reference baseline remains
  2.5.1; targeted later-source cases have separate provenance and do not
  establish complete 2.5.6 equivalence. The finite-SD boundary remains the
  documented R choice.
* Table 1 also retains finite SMDs at accepted weights near `8e307` where
  Stata 2.5.6 returns missing values, separately from the existing finite SD
  choice.

* The package catalog and Stata migration guide now target pinned tabtools
  2.5.1, including `tabcell()`, `ratetab()`, `outtab()`, `crosstab()`,
  `corrtab()` and `survtab()`. Statistical, estimator and numerical boundaries
  are described explicitly rather than claiming universal native equivalence.
* Regression workflows add fit-count records and explicit failed columns,
  fit-owned cell states, mincount reporting masks, richer statistics,
  reference placement, literal overrides and transposed specification tables.
  Raw analytical returns remain separate from eligible publication/forest values.
* Composite rate/model workflows add multiple models per outcome, keyed
  placement, rates-only/model-only rows and authenticated numeric companions.
  Plain publication exports do not fabricate regression keys or model states.

* `outtab()` adds binary outcome counts and ordered crude/adjusted ratios,
  separately verified sample populations, per-fit diagnostics and primary
  publication masks that preserve raw analytical returns.

* `ratetab()` adds separate grouping sections, common multi-outcome samples,
  recurrent event counts, exact/log Poisson and saturated clustered intervals,
  frequency replication, primary masks, all publication sinks, and raw
  analytical saving with preserved grouping labels and collision mapping.
  Zero-event exact upper limits apply under every method. Cluster correction
  uses the full fitted sample; a single event-contributing cluster can retain
  an interval when other clusters contribute exposure.

* `tabcell()` formats seven scalar/vector cell forms with aligned source,
  inference, state and masking provenance. Exact binomial/Poisson limits and
  log-rate intervals follow the pinned native rules. Protected leaf inputs
  and reconstructive interval companions are redacted. Cell columns publish
  through `puttab()` and retain selected-row provenance in parent tables.


* `comptab()` and `hrcomptab()` support several model blocks per rate
  outcome, explicit structured mappings, keyed factor section/level placement,
  rates-only sections and model-only appendices. HR and IRR scales remain
  distinct. Outcome spans follow actual model counts; legacy HR layouts remain.
* Composite `cformat` uses producer-owned publication numeric companions,
  retaining stars and unavailable intervals; `cisep` alone rewrites canonical
  intervals. Stale companions refuse. Plain unkeyed composite exports retain
  headers, frame identity and sample accounting and cannot become sources.
* Authenticated composite sources classify references by canonical model state,
  preserving real estimates when custom reference text has the same value.
  Imported companion mutations refuse before publication.

* `tt_fitcount()` freezes unweighted observation/event/person/exposure and
  term counts with exact fit/record reuse checks. `regtab()` adds `mincount`,
  explicit failed columns, no-test/absent/constraint labels, and separate raw
  analytical and publication numerics. Explicit source capture documents the
  limits of authenticating historical predictors the fit did not retain.
* Structured regression statistics add scalar pairs, literal text, formats,
  labels, strictest thresholds and transitive `maskwith` groups, with typed
  counters and raw per-part provenance in original numeric model order.
* `regtab()` adds fit-owned `reftop`, exact row/model `cellnote`, raw-term
  `addrow(after)` placement, and models-as-rows `transpose` with `collabels`
  and `addcol`. Every publication sink shares the final placement and text.
  Keyed flat output and composition explicitly refuse transposed orientation.
* Cell notes set publication state `masked` and clear numeric/forest companions
  without incrementing disclosure counters. Raw `stored$table` is explicitly
  labeled `raw_analytical`; `publication_table`, publication row records and
  separately labeled raw row provenance distinguish suppressed/replaced cells.
* `dimnonsig` uses analytical states and structural blocks before masks or
  overrides. Forest effects require finite estimates and ordered finite bounds.
  Outcome/model/equation identities retain case. Supported fitted interval
  `survreg` responses carry actual interval-censored AFT/TR provenance.
* Custom regression frames now require explicit `effect_scale` (including
  `Coef.`) and supplied-interval `conf.level`; SE-derived inference requires
  df/df.error or declared normal reference. Survival command declarations
  require compatible distribution/metric metadata. Explicit non-estimated and
  fixed states carry evidence; zero SE and labels never manufacture a base.
* Retained missing character/factor groups in computed rates now have explicit
  category labels; computed event/no-time errors use a consistent class.
* Verify copied export bytes before success/history, and retain original files
  when a staged copy reports success with incomplete contents.
* Limit Table 1 header replacement protection to its actual denominator
  dependencies; style effecttab added rows as added rows.

* The 272-scenario native baseline now comes directly from the authenticated
  tabtools 2.5.1 Git objects, with complete artifact and unchanged-input
  inventories. Named 2.1.14 publication cases retain their original bytes.
  The native demo has separate authenticated snapshots and an actual runtime
  catalog; fresh reconstruction and R comparison remain required before its
  generated output is promoted.

* Native demo staging now separates its source snapshots from scenario inputs,
  records native storage metadata and full schema/data hashes, and preserves
  authentic stage artifacts and failed-run diagnostics before scratch cleanup.

* Table 1 and `desctab()` add primary-only printed-count masking,
  `nosmallcells`, literal mask text and native-consistent session mode
  precedence. Strict suppression remains the default. Canonical typed mask
  metadata records thresholds/mode/display-cell counts alongside native
  active-mask compatibility fields.
* Structured `cellreplace` records select exact final row labels and headers,
  or value-column positions excluding the label. Protected cells and withheld
  components refuse replacement; all publication sinks share the final text.
  Replacements invalidate affected numeric/inference/ledger companions while
  retaining permitted unmasked/primary analytical aggregates separately.
  Active strict masking creates no raw backup. The three known native slashN
  disclosure cases remain conditional on a Stata fix.

* `tabtools_options()` adds session workbook/Markdown destinations, puttab/frame
  header shading, and supported masking defaults. Explicit NULL clears one key;
  omitted keys remain unchanged. Session keys are excluded from persistence.
* Ordinary commands inherit both destinations only for an explicit non-NULL
  sheet; puttab/stacktab inherit omitted destinations. Explicit NULL opts out
  per sink, and Table 1 retains native non-NULL `excel` alias priority. Missing
  workbooks give a classed sheet warning before output. Explicit writers remain
  single-sink APIs; workbooks preserve unrelated sheets.
* Inherited Markdown replaces on its first successful write, then appends;
  explicit destinations retain replacement defaults and explicit append wins.
  Successful active-path history survives A/B/A, repeated setting and clearing.
  Failed writes and stacktab staging never advance history; stacktab records
  destinations after its atomic commit.

* `regtab()` and `effecttab()` accept full numeric `cformat` formats for
  estimates and both confidence limits, including exponential formats.
  Explicit `digits` conflicts with `cformat`; decimal-comma formats require
  a comma-free interval separator. Separators remain literal. `regtab()` adds
  literal CI and p-value header overrides, `cilabel` and `plabel`.
* `tt_flat()` exports editable regression and effect body records with
  statistic column labels, model headers, raw row keys and model states.
  Row and column selections preserve metadata, and composed tables retain
  distinct source block identities, including repeated source tables.
  Canonical analytical states, structural headings, source provenance and
  numeric header spans are validated even after technical columns are removed;
  composition refuses source metadata whose row keys are no longer aligned.
* Invalid formatter inputs such as `integer(0)` raise `tabtools_error_fmt`.
  General formats with 13 or more significant digits retain the documented
  native rounding difference from Stata.

* `puttab()` consumes validated `tt_flat()` records, omits only their
  technical keys by default, and exports explicitly selected keys. Model
  spans under `blockheader` retain original identities after projection or
  reordering. Source metadata survives publication layout; lost or malformed
  semantics are refused before writing. Omitted-key and no-model-span notes
  appear as separate publication paragraphs in every sink.

* Footnotes accept character paragraph vectors or Stata's spaced-backslash
  separator. Console, CSV, Markdown, Excel and presentation converters show
  separate paragraphs, including automatic notes and significance legends;
  Excel adds styled note rows and repeats explicit note row heights.
* `stratetab()` adds rate display formats, literal interval separators, primary
  publication masks with raw analytical retention, exact zero-event bounds,
  and zero-event/no-time display controls. Scaled rates retain full double
  precision before rounding, correcting the S08 154.27 cell. Console headers
  show the Exposure label once, matching Stata's native console layout.
* `tt_rates(zero_time = "retain")` counts zero-length records and events;
  the default keeps its existing exclusion rule and now records an auditable
  `zero_length` diagnostic. Events without person-time are refused.
* `puttab()` now boxes non-academic tables and supports `hlines`, `vlines`,
  `boldrows`, ordered panels, repeated source or literal panel headers,
  `panelinline`, `noindent`, and spanning headers. Layout coordinates and
  widths are validated before export. Markdown promotes the first panel
  row under `noheader`; literal labels remain escaped.
* `puttab(nformat = ...)` formats integer columns and imported Stata `fc`
  formats retain grouping. Dates and value labels retain their display.
  Explicit `headershade = FALSE` overrides session shading.
* `stacktab(frames = ...)` stacks equal-width in-memory sources as panels,
  with one common header, optional headings and stored source identities.

* `table1_tc(nosmdhighlight = TRUE)` disables Excel SMD highlighting, as
  `smdthreshold = -1` does. Combining the switch with an explicit threshold
  is refused, matching Stata. Markdown has no SMD highlighting.
* SMD cross-validation now compares population and max-pair statistics for
  continuous, binary and categorical rows, with importance and frequency
  weights and groups with no values, directly against Stata 2.5.1.

# tabtools 0.1.1

Bug fixes from the 2026-10-06 audits, plus the Table 1 SMD work that follows
Stata tabtools 2.4.0. Until the Stata goldens are regenerated from 2.5.1, the
golden tests translate the new SMD pair header, note and footnote back to
2.1.14's form.

## New features

* `table1_tc()` with three or more groups and `smd = TRUE` names the pair
  its SMD compares wherever the table goes. The column header reads
  `SMD (A vs B)`, a footnote says "SMD compares A vs B only (the first two
  of 3 groups).", and the console note uses the same text. Before, only a
  console note said so, and every export (Excel, CSV, Markdown, gt,
  tinytable, `as.data.frame()`) showed a bare "SMD" column.
* New `smdpair`: the two groups a pair SMD compares, by `by` value or
  label. The header names the pair, and with three or more groups the
  footnote adds "chosen with smdpair()". New `smdpair_as = "values"` or
  `"labels"` (Stata `smdpair(..., values|labels)`) says how its tokens are
  read when a token is one group's value and another's label.
* New `smdtype` for balance across all groups. `"population"` reports the
  population standardized bias of McCaffrey et al. (2013, eq. 5 and
  section 4.2): the largest |group mean - overall mean| / overall SD.
  `"maxpair"` reports the largest pairwise SMD (Lopez and Gutman 2017,
  eq. 27), with every pair on the all-groups pooled SD. `"pair"` (the
  default) is the existing statistic. Without weights both new types
  match cobalt 4.6.3 on continuous and binary variables.
* `stored$smdtype` (with `smd = TRUE`) and `stored$smdnote` (when there
  is a note), as Stata's `r(smdtype)` and `r(smdnote)`.

## Fixed

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
* Continuous-variable SDs are computed about the mean, as Stata 2.1.15
  does, in the unweighted, `wt` and `fweight` paths. Values far from zero
  (`1e8 + c(0.1, 0.2, 0.3, 0.4)`) no longer show SD 0 or a missing SD from
  cancelling raw moments. A constant column that showed `2.68±.` now
  shows `2.68±0.00`, and geometric SDs (`contln`) change with it. With
  extreme weights (about 1e160 and up) Stata 2.5.1 prints a missing SD when
  the square of the centered weighted sum overflows; tabtools keeps the
  finite SD there, as before.
* `table1_tc(smdpair=)` resolves a token by value or by label as Stata does:
  a numeric token that only matches a value label (labels "2020", "2021")
  works, and a token that is one group's value and another's label is refused
  unless `smdpair_as` says which is meant. A logical `by` accepts
  `TRUE`/`FALSE` in `smdpair`.
* The SMD column headers `SMD (A vs B)`, `Pop. SB` and `Max SMD` are recognised
  as the SMD role by hand-built tables.
* When a `table1_tc()` footnote uses ` \ ` paragraphs, the automatic small-cell
  and SMD notes are appended as their own paragraphs.
* `regtab(sep = " ")` is accepted, as in Stata, and renders `(a b)` intervals;
  the separator is no longer trimmed. An empty `sep` means the default `", "`,
  as in `effecttab()`.
* `tt_stack()` accepts a 0-row table. `tt_stack(groups =)` heading rows have
  the keys `group:<label>` (`group:<label>#2` for a repeated label) instead of
  `NA`, and are flagged as headings. `tt_merge()` matches headings by label
  and scopes the body rows under their heading, so stacks in a different order
  align, and headings never join data rows. The prefix `group:` is reserved
  for these keys.
* `hrcomptab()`/`comptab()` `rownames` no longer split a compound quote that
  holds inner double quotes.
* `headercolor`/`zebracolor` accept all Stata colour names (`gs0`-`gs16`,
  `ltblue`, `emerald`, ...), with the RGB values of Stata's `color-<name>.style`
  definitions that tabtools 2.5.1 uses.
* `polr`/`clm` fits with a non-logit, non-probit link (cloglog, loglog,
  cauchit, ...) are no longer labelled `oprobit`; the table footnote names the
  link scale and the methods text says "ordinal regression (<link> link)".
* `effecttab()` warns once per call (class `tabtools_warning_ratio_interval`),
  naming each term, when a data frame stating a ratio null (`null.value = 1`)
  or headed as a ratio (`effect = "RR"`, `"OR"`, ...) derives a Wald interval
  from `std.error` whose lower bound is 0 or below. The linear delta-method
  interval is kept, as Stata's `nlcom` and `margins` give it; supply a
  log-scale interval to avoid the warning. The `marginaleffects` ratio
  warning now also fires at a lower bound of exactly 0.
* `regtab()` on a `glmmTMB` fit says so, in a console note (class
  `tabtools_note_tmb_cov_ci`) and in the footnote of every sink, when a
  random-effect covariance interval is left blank because glmmTMB's
  parameterization could not be matched to `VarCorr()`. The refusal of
  `cs()`/`ar1()` terms now has class `tabtools_error_tmb_covstruct`.

## Changed

* `tabtools_options()` takes only named arguments (`...` is its first
  formal), so `tabtools_options("digits")`, which used to set the session font
  to "digits", now aborts with a hint to use `tabtools_options()$digits`.
  Partial matching of option names (for example `fontsiz =`) no longer works.
  `tabtools_options()` and `tabtools_options(clear = TRUE)` are unchanged.

## Known issues (shared with Stata tabtools 2.5.1)

* With `slashN`, the printed `n/N` denominators can still let a protected
  count be reconstructed in some small tables. Stata 2.5.1 gives the same
  cells; the fix will follow Stata's.
* The Excel SMD column keeps Stata's fixed width of 8, so a long
  `SMD (A vs B)` header can be clipped.

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
