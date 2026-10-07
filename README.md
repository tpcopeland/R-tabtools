# tabtools for R — publication-ready descriptive and regression tables

**Version 0.1.1** | 2026-10-06

tabtools makes the tables of a clinical or epidemiological paper: a Table 1 of baseline characteristics, regression tables, incidence rates beside hazard ratios, treatment effects and margins, and inverse-probability-weight diagnostics. Every table prints in the console and writes to Excel, Word, HTML, Markdown and CSV with the same rows, labels, indented category levels and Reference rows in each.

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
| `tt_rates()`, `stratetab()` | Events, person-time and incidence rates per group, and their table |
| `hrcomptab()`, `comptab()` | Cohort-study Table 2 (rates beside hazard ratios); selected rows of several model tables |
| `effecttab()`, `tt_effect_rows()` | Treatment effects and margins from marginaleffects results, data frames or matrices |
| `wttab()` | Distribution and effective sample size of inverse probability weights |
| `puttab()`, `stacktab()` | Style any data frame or matrix as a sheet; assemble several tables into one sheet |
| `tt_merge()`, `tt_stack()` | Put tables side by side, or one under another |
| `tt_write_xlsx()`, `tt_write_csv()`, `tt_write_markdown()` | Write a table to a file |
| `flextable::as_flextable()`, `tt_as_gt()`, `tt_as_gtsummary()`, `tt_as_tinytable()` | Convert a table for Word, HTML, Quarto and LaTeX |
| `as_forest_data()`, `as.data.frame()` | The numbers behind a table, for forest plots; the table as text cells |
| `tt_flat()` | Editable regression and effect body rows with raw keys, model states and preserved headers |
| `tt_vcov()`, `tt_vce_types()`, `tt_ci_methods()` | The variance and interval methods `regtab()` uses, for use elsewhere |
| `tt_from_modelsummary()` | Table a modelsummary model list with `regtab()` |
| `tabtools_options()` | Default font, border style, digits and colours |
| `tt_as_factor()` | Turn Stata value-labelled columns into factors |
| `tt_table()`, `validate_tt_table()` | The table object every renderer reads |

Every command returns a `tt_table`, which prints as above.

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
- `?table1_tc`, `?regtab` and the other help pages list every argument.

**Using tabtools with an AI assistant:** paste [`inst/llm/tabtools-syntax.md`](inst/llm/tabtools-syntax.md) (installed at `system.file("llm", "tabtools-syntax.md", package = "tabtools")`) and a data dictionary (no row-level data) into the chat, and ask for the code of your tables.

## Coming from Stata

tabtools is the R implementation of the Stata [`tabtools`](https://github.com/tpcopeland/Stata-Tools/tree/main/tabtools) commands `table1_tc`, `regtab`, `puttab`, `stacktab`, `stratetab`, `effecttab`, `comptab` and `hrcomptab`. Argument names are the Stata option names, so a call translates option by option; `regtab()` takes the fitted models instead of reading a `collect`.

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

**Parity.** Everything a reader sees targets cell-for-cell parity with Stata `tabtools` 2.1.14: row structure, labels, indents, descriptive statistics, number formats, headers and Excel styling. The package is tested against 272 scenarios generated by Stata (56 `table1_tc`, 138 `regtab`, 15 `puttab`, 9 `stacktab`, 8 `stratetab`, 28 `effecttab`, 8 `wttab`, 8 `comptab`, 2 `hrcomptab`), compared in the table, the console listing, the CSV and Markdown files, the stored results and the Excel formatting. Every sheet of a ported command in the Stata demo matches (61 of its 77 sheets; `corrtab`, `crosstab`, `survtab` and three model families are not ported). `wttab()` has no Stata command yet; its reference tables are Stata `puttab` sheets of the same statistics. Hypothesis tests use R's conventional defaults (Welch t test, Wilcoxon rank-sum, Pearson's chi-squared without continuity correction as in Stata, Fisher's exact); the p-value format is Stata's. `vignette("coming-from-stata", package = "tabtools")` lists every place where R differs on purpose.

## QA

QA suites and how to run them are documented in [`qa/README.md`](qa/README.md).

## Version History

- **0.1.1**: bug fixes from the 2026-10-06 audits (small-cell protection of derivable hidden counts, centered SDs, `smdpair` resolution, `tt_stack()`/`tt_merge()` group keys, Stata colour names, ordinal link labels and others), plus SMD pair naming, `smdpair` and `smdtype`. See NEWS.md.
- **0.1.0**: first release. Descriptive, regression, incidence-rate, treatment-effect, weight-diagnostic and composite tables (`table1_tc()`/`desctab()`, `regtab()`, `regtab_uv()`, `stratetab()`, `tt_rates()`, `effecttab()`, `wttab()`, `comptab()`, `hrcomptab()`, `puttab()`, `stacktab()`), table composition (`tt_merge()`, `tt_stack()`), and converters to flextable, gt, gtsummary and tinytable.

See [`NEWS.md`](NEWS.md) for details.

## Author

Timothy P Copeland, Karolinska Institutet

## License

MIT (see LICENSE)
