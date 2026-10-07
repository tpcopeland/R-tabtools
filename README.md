# tabtools for R — publication-ready descriptive and regression tables

**Version 0.2.0** | 2026-10-07

tabtools makes the tables of a clinical or epidemiological paper: baseline characteristics, regression estimates, categorical comparisons, correlations, survival summaries, incidence rates, binary outcome ratios, treatment effects and weight diagnostics. Every table prints in the console and writes to Excel, Word, HTML, Markdown and CSV with the same rows, labels, indented category levels and Reference rows in each.

## Quick Start

```r
library(tabtools)

d <- mtcars
d$am <- factor(d$am, 0:1, c("Automatic", "Manual"))
attr(d$mpg, "label") <- "Miles per gallon"
attr(d$cyl, "label") <- "Cylinders"
attr(d$vs, "label") <- "Straight engine"

table1_tc(d, by = "am", vars = c(mpg = "contn %5.1f", cyl = "cat", vs = "bin"),
          smd = TRUE)
```

```
  +----------------------------------------------------------------------+
  |                             Automatic   Manual     p-value   SMD     |
  |----------------------------------------------------------------------|
  | No. (Column %) or Mean±SD   N=19        N=13                         |
  |----------------------------------------------------------------------|
  | Miles per gallon            17.1±3.8    24.4±6.2   0.001     1.411   |
  |----------------------------------------------------------------------|
  | Cylinders                                          0.013     1.251   |
  |    4                        3 (16)      8 (62)                       |
  |    6                        4 (21)      3 (23)                       |
  |    8                        12 (63)     2 (15)                       |
  |----------------------------------------------------------------------|
  | Straight engine             7 (37)      7 (54)     0.34      0.347   |
  +----------------------------------------------------------------------+
```

`vars` names each row and how to summarise it: `contn` mean ± SD (here with one decimal, `%5.1f`), `conts` median and quartiles, `cat` counts and column percentages for each level, `bin` the count of 1s. Row labels come from each column's `"label"` attribute; `smd = TRUE` adds standardized mean differences.

```r
fit1 <- glm(breaks ~ wool, family = poisson, data = warpbreaks)
fit2 <- glm(breaks ~ wool + tension, family = poisson, data = warpbreaks)
regtab(fit1, fit2, models = c("Crude", "Adjusted"), stats = "n")
```

```
  +----------------------------------------------------------------------------------------+
  |                    Crude                             Adjusted                          |
  |                      IRR         95% CI   p-value         IRR         95% CI   p-value |
  |         wool                                                                           |
  |            A   Reference                            Reference                          |
  |            B        0.81   (0.74, 0.90)    <0.001        0.81   (0.74, 0.90)    <0.001 |
  |      tension                                                                           |
  |            L                                        Reference                          |
  |            M                                             0.73   (0.64, 0.82)    <0.001 |
  |            H                                             0.60   (0.53, 0.67)    <0.001 |
  | Observations          54                                   54                          |
  +----------------------------------------------------------------------------------------+
```

`regtab()` picks the effect measure from the model (incidence rate ratios for a Poisson model, odds ratios for a logistic one, hazard ratios for a Cox model) and matches rows across models by variable and level.

## Requirements

R 4.1.0 or later. Word output needs flextable and HTML output gt; both are optional.

## Installation

```r
# install.packages("remotes")
remotes::install_github("tpcopeland/R-tabtools")
```

## Functions

| Function | Description |
|---|---|
| `table1_tc()`, `desctab()` | Table 1: baseline characteristics by group, with tests, SMDs, weights and small-cell suppression |
| `regtab()` | Regression table of one or more fitted models, side by side |
| `regtab_uv()` | One univariable model per covariate, as one `regtab()` column |
| `tt_mi()` | Multiply imputed fits, pooled by Rubin's rules, for `regtab()` |
| `tabcell()` | Vectorized estimate, p-value, count, percentage, event-percentage, quartile and rate cells with analytical provenance |
| `tt_fitcount()`, `tt_failed_model()` | Capture fit-time sample/term counts; explicitly represent a failed regression column |
| `crosstab()` | Two-way counts/percentages, Pearson or Fisher tests, sample OR/RR/RD and signed trend inference |
| `corrtab()` | Pairwise Pearson/Spearman matrices with stars or p-values and pair-specific N |
| `survtab()` | Kaplan-Meier summaries, risks/events, median, RMST and two-group contrasts |
| `tt_rates()`, `stratetab()` | Legacy computed/saved incidence-rate blocks and their table |
| `ratetab()` | Separate grouping sections, multiple outcomes, exact/log-Poisson or clustered rate intervals |
| `outtab()` | Binary outcomes by binary exposure, crude counts and ordered crude/adjusted ratio specifications |
| `hrcomptab()`, `comptab()` | Rates beside HR/IRR models, multiple models per outcome, keyed placement and model-only rows |
| `effecttab()`, `tt_effect_rows()` | Treatment effects and margins from marginaleffects results, data frames or matrices |
| `wttab()` | Distribution and effective sample size of inverse probability weights |
| `puttab()`, `stacktab()` | Style any data frame or matrix as a sheet; assemble several tables into one sheet |
| `tt_merge()`, `tt_stack()` | Put tables side by side, or one under another |
| `tt_write_xlsx()`, `tt_write_csv()`, `tt_write_markdown()` | Write a table to a file |
| `flextable::as_flextable()`, `tt_as_gt()`, `tt_as_gtsummary()`, `tt_as_tinytable()` | Convert a table for Word, HTML, Quarto and LaTeX |
| `as_forest_data()`, `as.data.frame()` | The numbers behind a table, for forest plots; the table as text cells |
| `tt_flat()` | Editable regression/effect rows with keys and preserved headers; plain unkeyed publication frames for other families |
| `tt_vcov()`, `tt_vce_types()`, `tt_ci_methods()` | The variance and interval methods `regtab()` uses, for use elsewhere |
| `tt_from_modelsummary()` | Table a modelsummary model list with `regtab()` |
| `tabtools_options()` | Query/set/clear styling, masking and opt-in workbook/Markdown destinations |
| `tt_as_factor()` | Turn Stata value-labelled columns into factors |
| `tt_table()`, `validate_tt_table()` | The table object every renderer reads |

Table-building commands return a `tt_table`, which prints as above. `tabcell()` returns a vector of `tt_cell` text with aligned numerical provenance; `tt_fitcount()` returns a frozen count record. `?tabtools` is the command catalog, with short recipes and links to the detailed help pages.

## Worked Examples

### 1. Writing a file

`xlsx =`, `csv =` and `markdown =` write the table as it is built. **A command that writes a file returns the table invisibly**, so nothing prints: assign it and print it.

```r
t2 <- regtab(fit1, fit2, models = c("Crude", "Adjusted"),
             xlsx = "tables.xlsx", sheet = "Table 2")
t2
```

`tt_write_xlsx(t2, "tables.xlsx", sheet = "Table 2")` writes a table later; several tables go to one workbook as separate sheets.

Regression and effect tables accept `cformat = "%12.0fc"` for estimates and
both confidence limits, or fixed, general and exponential formats such as
`"%9.3f"`, `"%9.3g"` and `"%9.2e"`. An explicitly supplied `digits` conflicts
with `cformat`. Decimal-comma formats such as `"%9,2f"` require a comma-free
separator, for example `sep = " to "`. General formats with 13 or more
significant digits can differ from Stata's native rounding in the last digits.
`regtab(cilabel = "Confidence interval", plabel = "P")` changes the headers
in every output.

`tt_flat(t2)` returns the rendered body with statistic column names, raw
`_term` keys, `_order`, `_rowtype` and one `_state_<model-index>` column per
model. `tt_flat(t2, keyed = FALSE)` omits the technical columns. Model names
and full headers are attributes; row and column selections preserve them.
Compositions retain source block identities so repeated terms in stacked
tables remain distinct.

### 2. Word, HTML, Quarto

```r
ft <- flextable::as_flextable(t2)        # Word via flextable/officer
flextable::save_as_docx(ft, path = "table2.docx")
tt_as_gt(t2)                             # gt, for HTML and Quarto
```

Both converters carry the workbook's house style: fonts, both header rows, merged model headers, italic Reference cells, borders and fills. `title`, `footnote` and `borderstyle` are kept on the table, so they need no file.

### 3. Incidence rates beside hazard ratios

```r
library(survival)
lung$died <- as.integer(lung$status == 2)
lung$years <- lung$time / 365.25
lung$sex <- factor(lung$sex, 1:2, c("Male", "Female"))

rates <- stratetab(tt_rates(lung, time = "years", event = "died", by = "sex"),
                   outlabels = "Death", outcomeids = "died", explabels = "Sex",
                   ratescale = 100, unitlabel = "100")
cox <- regtab(coxph(Surv(years, died) ~ sex + age, data = lung))
hrcomptab(rates, cox, rownames = "female")
```

```
  +--------------------------------------------------------------------------------------------+
  |  Exposure    Death                                                                         |
  |             Events   Person-Years (PY)   Per 100 PY (95% CI)        aHR (95% CI)   p-value |
  |       Sex                                                                                  |
  |      Male      112                 107   104.7 (87.0, 126.0)           Reference           |
  |    Female       53                  84     63.5 (48.5, 83.1)   0.60 (0.43, 0.83)     0.002 |
  +--------------------------------------------------------------------------------------------+
```

`rownames` are case-insensitive patterns matched as parts of the row labels: `"female"` selects Female only, but `"male"` would select both rows (and `"treated"` would also match `Untreated`). When one label contains another, match whole labels with `rownames_exact = TRUE`, or select by row number with `rows =`.

## Demo

Two demo scripts live in this repository, not in the installed package. Clone the repository and run them from its root:

- [`qa/demo/demo_tabtools_short.R`](https://github.com/tpcopeland/R-tabtools/blob/main/qa/demo/demo_tabtools_short.R): start here. One short example of each main table on a simulated cohort (Table 1, logistic and Cox tables, rates, Table 2, a weighted Table 1, Word and CSV output), with its output in [`demo_tabtools_short.md`](https://github.com/tpcopeland/R-tabtools/blob/main/qa/demo/demo_tabtools_short.md). It writes its files to the working directory; its last section needs flextable.
- [`qa/demo/demo_tabtools.R`](https://github.com/tpcopeland/R-tabtools/blob/main/qa/demo/demo_tabtools.R): the R twin of the full Stata demo, section by section, each block headed by the Stata command it replaces. Its data include Stata's example datasets `auto` and `union`, which is why it is not in the package, and it needs the suggested packages haven, survival, lme4, geepack, MASS, nnet, pscl, WeightIt and marginaleffects.

```r
source("qa/demo/demo_tabtools_short.R", echo = TRUE)   # or run it section by section

demo <- new.env()                 # its objects stay out of your workspace
demo$out_dir <- "tabtools_demo"   # optional; the default is a folder in tempdir()
source("qa/demo/demo_tabtools.R", local = demo)
```

## Documentation

- `vignette("tabtools")`: Get started, a Table 1 and a regression table from start to finish, written to Excel, CSV, Markdown and Word.
- `vignette("regression-tables", package = "tabtools")`: the supported models and their effect measures, choosing and labelling rows, standard errors, the rate and hazard-ratio Table 2, and models regtab does not support.
- `vignette("weighted-analyses", package = "tabtools")`: IPTW and marginal structural models: balance tables, `wttab()` for the weights, robust and clustered variances, and `effecttab()` for treatment effects and margins.
- `vignette("compared-with-gtsummary", package = "tabtools")`: the same Table 1 and Table 2 made with gtsummary and with tabtools.
- `vignette("coming-from-stata", package = "tabtools")`: for Stata users, option-by-option translation and every deliberate difference.
- `?tabtools`: the 2.5.1 command catalog and compact recipes; individual help pages list arguments and return contracts.

**Using tabtools with an AI assistant:** paste [`inst/llm/tabtools-syntax.md`](inst/llm/tabtools-syntax.md) (installed at `system.file("llm", "tabtools-syntax.md", package = "tabtools")`) and a data dictionary (no row-level data) into the chat, and ask for the code of your tables.

## Coming from Stata

The development interfaces target the Stata [`tabtools`](https://github.com/tpcopeland/Stata-Tools/tree/main/tabtools) 2.5.1 command suite, including `tabcell`, `ratetab`, `outtab`, `crosstab`, `corrtab` and `survtab`. Selected disclosure, overflow and formatting repairs follow the later 2.5.2–2.5.6 source at commit `4eecca4d`; this does not claim complete equivalence to 2.5.6. Argument names follow the Stata options; fitted models and data are explicit R inputs rather than active `collect` or dataset state. `?tabtools` supplies the catalog and the migration vignette documents supported estimator mappings.

```stata
table1_tc, by(treated) vars(age contn %5.1f \ female bin \ education cat) smd xlsx("t1.xlsx")
collect: logit event i.treated age female i.education
regtab, coef("OR") noint xlsx("t2.xlsx") sheet("Logistic")
```

```r
# The same .dta file; categorical predictors become factors that keep the
# Stata codes, while the outcome stays numeric.
cohort <- tt_as_factor(haven::read_dta("cohort.dta"), vars = c("treated", "education"))
table1_tc(cohort, by = "treated",
          vars = c(age = "contn %5.1f", female = "bin", education = "cat"),
          smd = TRUE, xlsx = "t1.xlsx")
fit <- glm(event ~ treated + age + female + education, binomial, data = cohort)
regtab(fit, coef = "OR", nointercept = TRUE, xlsx = "t2.xlsx", sheet = "Logistic")
```

**Parity.** The reference target is pinned Stata `tabtools` 2.5.1 (commit `712044f8`). The retained baseline inventory has 272 scenarios; 89 additional native cases or refusals are recorded separately across fit counts (3), cell formatting (6), rates (12), outcomes (9), composites (6), cross-tabulations (28), correlations (13) and survival (12). Separately, 33 selected Stata 2.5.6 regression cases (37 native commands) have genuine native and installed-R counterpart evidence, and the expanded demo has 149 passing checks across 146 artifact records. These scoped comparisons retain their declared source pins and documented boundaries. Matching targets include displayed cells, table structure, numerical returns and workbook styling, subject to the documented differences in `vignette("coming-from-stata", package = "tabtools")`. Table 1 uses conventional R hypothesis tests; correlation/Spearman tables follow the installed Stata 17 engine invoked by the pinned suite. `wttab()` remains an R addition.

Strict Table 1/crosstab masking redacts protected numerical returns. Primary rate/outcome/regression masks retain separately identified analytical numbers: a masked publication is not a protected analytical data file. Regression `mincount` is reporting hygiene. General formats with at least 13 significant digits may differ in their final rounding digits; Table 1 closes the earlier `slashN` denominator leaks through the selected later-source guards and retains the documented finite-SD overflow difference. With accepted weights near `8e307`, R can also retain finite SMDs where Stata 2.5.6 returns missing values, separately from the finite SD choice. `effecttab(full)` has the deliberate R alternative of `regtab()` on the component models.

## QA

QA suites and how to run them are documented in [`qa/README.md`](qa/README.md).

## Version History

- **0.2.0**: pinned Stata tabtools 2.5.1 workflows, fit-count and regression publication controls, scalar cells, rates, binary outcomes, composite models, cross-tabulations, correlations and survival summaries. See NEWS.md.
- **0.1.1**: bug fixes from the 2026-10-06 audits (small-cell protection of derivable hidden counts, centered SDs, `smdpair` resolution, `tt_stack()`/`tt_merge()` group keys, Stata colour names, ordinal link labels and others), plus SMD pair naming, `smdpair` and `smdtype`. See NEWS.md.
- **0.1.0**: first release. Descriptive, regression, incidence-rate, treatment-effect, weight-diagnostic and composite tables (`table1_tc()`/`desctab()`, `regtab()`, `regtab_uv()`, `stratetab()`, `tt_rates()`, `effecttab()`, `wttab()`, `comptab()`, `hrcomptab()`, `puttab()`, `stacktab()`), table composition (`tt_merge()`, `tt_stack()`), and converters to flextable, gt, gtsummary and tinytable.

See [`NEWS.md`](NEWS.md) for details.

## Author

Timothy P Copeland, Karolinska Institutet

## License

MIT (see LICENSE)
