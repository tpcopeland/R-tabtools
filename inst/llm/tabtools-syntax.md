# tabtools syntax sheet for AI assistants

## Instructions for the AI assistant

You are writing R code that uses **tabtools** to make the tables of a clinical
or epidemiological paper: Table 1 (baseline characteristics), regression
tables, incidence-rate tables, the cohort "Table 2" (rates beside hazard
ratios), treatment-effect and margins tables, and IPTW weight diagnostics.
Every command returns a `tt_table` that prints in the console and can be
written to Excel (house style), CSV, Markdown, Word (flextable) or HTML (gt).
tabtools is the R port of the Stata `tabtools` commands: **argument names are
the Stata option names** (`nointercept`, `headershade`, `mdappend`, `smd`,
`xlsx`, ...); arguments new in R are snake_case (`rownames_exact`,
`ci_method`, `vce_note`). Use only the functions and arguments on this sheet.

```r
# install.packages("remotes")
remotes::install_github("tpcopeland/R-tabtools")
library(tabtools)
```

### Preparing the data (do this before any table)

- **Categorical variables must be factors.** Level order is display order;
  the first level is the Reference row in `regtab()`. Relevel with
  `factor(x, levels = ...)` or `relevel()`. Do not use ordered factors
  (`factor(..., ordered = TRUE)`): `regtab()` refuses polynomial contrasts.
- **0/1 numeric** columns are binary (`bin`) in `table1_tc()`. Keep outcome
  and event indicators **numeric 0/1**, never factors (a factor event makes
  `coxph()` fit a multi-state model; `glm()` reads a factor response by level
  order).
- **Variable labels** are the `"label"` attribute:
  `attr(d$age, "label") <- "Age (years)"`. Row labels in every table come from
  it (else the column name). `table1_tc()` also takes `labels = c(age = "...")`.
- **Set labels last.** `factor()` and row subsetting (`d[rows, ]`,
  `subset()`) drop the `"label"` attribute. Build factors and subsets first,
  then attach labels (or re-attach them to each subset).
- **Stata data** (`haven::read_dta()`): convert the categorical predictors with
  `d <- tt_as_factor(d, vars = c("treated", "education"))`, which keeps the
  Stata value codes and the variable label; list only predictors in `vars`
  so value-labelled outcomes stay numeric.
- `table1_tc()` refuses `Date`/`POSIXct` columns, `Inf` and `NaN`: convert
  dates to numbers (e.g. follow-up years) and use `NA` for missing values.
- Survival time: make a numeric time column (e.g. `years = days / 365.25`) and
  a numeric 0/1 event column.

### Golden rules

1. Pass **fitted model objects** to `regtab()` (`regtab(fit1, fit2)` or a
   named list `regtab(list(Crude = f1, Adjusted = f2))`), never `summary()`,
   `broom::tidy()` or `mice::pool()` output. Effect measure (OR, HR, IRR,
   Coef.) and exponentiation are automatic.
2. To write a file, give a path: `xlsx = "tables.xlsx"` (with `sheet =`),
   `csv = "t1.csv"`, `markdown = "t1.md"` (`mdappend = TRUE` to add to it).
   **A call that writes a file returns the table invisibly**: assign it and
   print it (`t1 <- table1_tc(..., xlsx = "t.xlsx"); t1`). Several tables go
   to one workbook as different sheets (an existing sheet is replaced).
3. Word: `flextable::save_as_docx(flextable::as_flextable(tab), path = "t.docx")`.
   HTML/Quarto: `tt_as_gt(tab)`.
4. Convert factors **in the data**, not in the formula: with
   `glm(y ~ factor(cyl))` the row keys become `factor(cyl)`, `6.factor(cyl)`
   and `keep = "cyl"` fails.
5. Stata-identical Cox results need `coxph(..., ties = "breslow")`; R's
   default Efron ties differ on tied event times.
6. Models with inverse-probability weights: `regtab(fit, vce = "robust")`
   (Stata's `[pweight]`). `regtab()` never switches to robust by itself.
7. Stata-style string forms need a doubled backslash inside R strings:
   `"age contn %5.1f \\ sex bin"`. Prefer R vectors and lists.
8. Methods text: `tab$stored$methods` holds a ready methods sentence.

## Functions

Exported: `table1_tc` (alias `desctab`), `regtab`, `regtab_uv`, `tt_mi`,
`tt_rates`, `stratetab`, `hrcomptab`, `comptab`, `effecttab`,
`tt_effect_rows`, `wttab`, `puttab`, `stacktab`, `tt_merge`, `tt_stack`,
`tt_write_xlsx`, `tt_write_csv`, `tt_write_markdown`, `tt_as_gt`,
`tt_as_gtsummary`, `tt_as_tinytable`, `as_forest_data`, `tt_vcov`,
`tt_vce_types`, `tt_ci_methods`, `tt_from_modelsummary`, `tabtools_options`,
`tt_as_factor`, `tt_table`, `validate_tt_table`; S3 methods
`flextable::as_flextable()`, `as.data.frame()`, `print()` for `tt_table`.

Output and styling arguments shared by the table commands (not repeated
below): `xlsx`, `sheet`, `csv`, `markdown`, `mdappend`, `open`, `title`,
`footnote`, `font`, `fontsize`, `borderstyle` (`"default"`, `"thin"`,
`"medium"`, `"academic"`), `zebra`, `headershade`, `headercolor`,
`zebracolor`; `table1_tc`, `regtab`, `effecttab` also take `boldp`
(bold p below threshold) and `highlight` (shade rows with p below threshold).

### table1_tc(): Table 1 of baseline characteristics

```
table1_tc(data, vars = NULL, by = NULL, fweight = NULL, wt = NULL,
          labels = NULL, total = c("none", "before", "after"),
          missing = FALSE, test = FALSE, statistic = FALSE,
          headerperc = FALSE, smd = FALSE, nopvalue = FALSE,
          format = "%2.0f", percformat = "%5.0f", nformat = "%12.0fc",
          percent = FALSE, percent_n = FALSE, slashN = FALSE,
          catrowperc = FALSE, pdp = 3, highpdp = 2, missingsummary = FALSE,
          smallcells = NULL, wtcompare = FALSE, wtn = FALSE,
          smdthreshold = 0.1, test_args = NULL, sheet = "Table 1", ...)
desctab(data, vars, by, ...)   # identical alias
```

- `vars`: which rows and how to summarise them. `NULL` = every column, typed
  automatically. Forms: a character vector of names (auto types); a **named
  vector** `c(age = "contn %5.1f", sex = "cat", smoker = "bin")`; or one Stata
  string `"age contn %5.1f \\ sex cat"`. Each spec is `type [fmt1 [fmt2]]`:

  | type | cell | test (R defaults) |
  |---|---|---|
  | `contn` | mean ± SD | Welch t / Welch ANOVA |
  | `contln` | geometric mean (×/ GSD) | same, log scale |
  | `conts` | median (Q1, Q3) | Wilcoxon rank-sum / Kruskal-Wallis |
  | `cat` | label row + one indented row per level, n (%) | chi-squared (no continuity correction) |
  | `bin` | one row, n (%) of the 1s | chi-squared |
  | `cate`, `bine` | as `cat`, `bin` | Fisher's exact |
  | `auto` | strings/factors `cat`, 0/1 `bin`, else by distribution | |

  `fmt1` is a Stata format (`%5.1f` = one decimal) for the mean/median (for
  `cat`/`bin` the percentage format); `fmt2` for the SD/quartiles (defaults
  to `fmt1`).
- `by`: grouping column name (quoted). `total = "after"` adds a Total column.
- `smd = TRUE`: standardized mean differences between the first two groups;
  `nopvalue = TRUE` drops p-values; `test = TRUE` adds the test name column.
- `missing = TRUE`: missing as a category of `cat` variables;
  `missingsummary = TRUE`: a missing-data row per variable.
- `wt = "sw"`: IPTW/importance weights (weighted means, SDs, %; adds an
  "Effective sample size" row; p-values are dropped). `wtcompare = TRUE`
  shows crude and weighted side by side (needs `by`); `wtn = TRUE` shows
  effective counts as n (%). `fweight` = integer frequency weights.
- `smallcells = 5`: suppress counts below 5 (disclosure control).
- `format`, `percformat`: global Stata formats; `pdp`/`highpdp`: p-value
  decimals below/above 0.10.
- `test_args = list(t.test = list(var.equal = TRUE))`: arguments for the tests.
- Returns a `tt_table`; `$stored$methods` (methods paragraph), `$stored$Dapa`
  ("Data are presented as ..."), `$stored$table` (p-values and SMDs),
  `$stored$types` (resolved types).

```r
d <- survival::lung
d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
d$ecog <- factor(d$ph.ecog, 0:3, c("0", "1", "2", "3"))
d$wtloss10 <- as.integer(d$wt.loss >= 10)
attr(d$age, "label") <- "Age (years)"
attr(d$ecog, "label") <- "ECOG performance status"
attr(d$ph.karno, "label") <- "Karnofsky score"
attr(d$wtloss10, "label") <- "Weight loss >= 10 lb"
t1 <- table1_tc(d, by = "sex",
                vars = c(age = "contn %5.1f", ph.karno = "conts",
                         ecog = "cat", wtloss10 = "bin"),
                total = "after", smd = TRUE, missingsummary = TRUE)
t1
t1$stored$methods
```

### regtab(): regression tables, models side by side

```
regtab(..., models = NULL, coef = NULL, sep = ", ", nointercept = NULL,
       keepintercept = FALSE, noreeffects = FALSE, stats = NULL,
       stat_fun = NULL, relabel = FALSE, digits = NULL, level = 0.95,
       ci_method = c("wald", "profile"), keep = NULL, drop = NULL,
       labelmatch = FALSE, dimnonsig = FALSE, refcat = "Reference",
       cutlabels = NULL, compact = FALSE, nopvalue = FALSE, stars = FALSE,
       starslevels = NULL, addrow = NULL, pdp = 3, highpdp = 2,
       interactions = c("fvgen", "native"), xsymbol = "×", vsref = NULL,
       vce = c("stata", "model", "robust", "cluster"), vce_df = NULL,
       cluster = NULL, finegray = NULL, vce_note = FALSE,
       labelwidth = 45, sheet = "Regression", ...)
```

- `...`: fitted models, or one (named) list of them. Supported: `lm`, `glm`
  (incl. `MASS::glm.nb`), `survival::coxph` (incl. `clogit`, Fine-Gray on
  `survival::finegray()` data), `survival::survreg`, `MASS::polr`,
  `ordinal::clm`, `nnet::multinom`, `pscl::zeroinfl`/`hurdle`,
  `cmprsk::crr`, `lme4::lmer`/`glmer`, `glmmTMB`, `nlme::lme`,
  `geepack::geeglm`, `survey::svyglm`, `WeightIt::glm_weightit`, multiply
  imputed fits (`mice` `mira`, `tt_mi()`), a `regtab_uv()` object, and data
  frames of estimates (`term`, `estimate`, `conf.low`, `conf.high`,
  `p.value`, ...). Others (e.g. `mgcv::gam`, a `coxph` with `frailty()`) stop
  with an error: pass them as a data frame.
- `models`: column labels, e.g. `c("Crude", "Adjusted")` (default: list names).
- `coef`: header text for the estimate column (`"aHR"`, `"OR"`), does not
  change exponentiation. Default detected: OR (logit), HR (Cox), IRR
  (Poisson), RRR (multinom), TR (survreg), SHR (Fine-Gray), Coef. otherwise.
- `keep`/`drop` (not both): Stata-style row keys. A bare variable name
  (`"treat"`) selects the variable's header, levels and interactions;
  a level is `"2.education"` (level code = the level itself if it is an
  integer, else its position 1, 2, ...); `"_cons"` is the intercept; R
  coefficient names (`"educationSecondary"`) also work. `labelmatch = TRUE`
  matches `keep`/`drop` as case-insensitive substrings of displayed labels.
- `stats`: rows below the estimates, any of `"n"`, `"events"`, `"groups"`,
  `"mi_m"`, `"aic"`, `"qic"`, `"bic"`, `"ll"`, `"icc"`, `"r2"`, `"r2_a"`,
  `"rmse"`, `"F"`, `"fmi"`, `"vce"` (fixed order; `"n"` shows Subjects for
  survival models).
- `stat_fun`: extra rows, `list("C statistic" = list(fun = function(fit)
  survival::concordance(fit)$concordance, fmt = "%5.3f"))`.
- `addrow`: extra rows, one value per model: `list("P trend" = c(0.03, 0.04))`.
- `nointercept` (default: dropped when all models are on a ratio scale),
  `keepintercept = TRUE` to keep it.
- `digits` (0-6, default 2), `level` (0.90 or 90), `compact = TRUE` (estimate
  and CI in one column), `nopvalue`, `stars = TRUE`, `dimnonsig = TRUE`
  (grey rows whose CI includes the null in every model).
- `vce`: `"stata"` (default, Stata's standard errors), `"model"` (the fit's
  own `vcov()`), `"robust"` (Stata `vce(robust)`/`[pweight]`), `"cluster"`
  with `cluster = ~id`. One value, or one per model:
  `vce = c("stata", "stata", "robust")`, `cluster = list(NULL, NULL, ~id)`.
  A function `function(f) sandwich::vcovHC(f, type = "HC3")` or a matrix is
  also accepted for `lm`, `glm`, `coxph`. `vce_note = TRUE` names
  non-default variances in the footnote. Robust/cluster are available for
  `lm`, `glm`, `glm.nb`, `coxph`, `geeglm` only (`tt_vce_types(fit)`).
- `ci_method = "profile"`: profile-likelihood CIs (`lm`, `glm`, `glm.nb`, with
  `vce = "stata"`/`"model"` only).
- `interactions = "fvgen"` (default) labels interaction cells `A × B`;
  `vsref = "(vs. @)"` appends the base level to main-effect levels.
- `cutlabels`: labels for ordinal cutpoints (shown with `keepintercept`).
- Returns a `tt_table`; `$stored$methods`, `$stored$table`; see
  `as_forest_data()` for the estimates as data.

```r
d <- mtcars
d$cyl <- factor(d$cyl)
attr(d$wt, "label") <- "Weight (1000 lb)"
attr(d$cyl, "label") <- "Cylinders"
f1 <- glm(am ~ wt, family = binomial, data = d)
f2 <- glm(am ~ wt + hp + cyl, family = binomial, data = d)
regtab(f1, f2, models = c("Crude", "Adjusted"), stats = c("n", "aic"))
regtab(list(Crude = f1, Adjusted = f2), keep = "wt", compact = TRUE)
```

```r
library(survival)
lung2 <- survival::lung
lung2$event <- lung2$status - 1
lung2$sex <- factor(lung2$sex, 1:2, c("Male", "Female"))
cox <- coxph(Surv(time, event) ~ sex + age + ph.ecog, data = lung2,
             ties = "breslow")
regtab(cox, coef = "aHR", stats = c("n", "events"),
       stat_fun = list("C statistic" = list(
         fun = function(fit) survival::concordance(fit)$concordance,
         fmt = "%5.3f")))
```

### regtab_uv(): univariable (crude) models as one column

```
regtab_uv(data, y, x, method = "glm", method.args = list(),
          formula = "{y} ~ {x}")
```

- `y`: outcome column name, or `"Surv(time, status)"` for Cox. `x`: character
  vector of covariates (one model each). `method`: `glm`, `survival::coxph`,
  ... ; `method.args = list(family = binomial)` (a data column goes in
  quoted: `list(weights = quote(w))`). `formula`: template, e.g.
  `"{y} ~ {x} + age"` (each covariate adjusted for age; only `{x}` rows shown).
- Returns a `tt_uv` object to pass to `regtab()`.

```r
d <- mtcars
d$cyl <- factor(d$cyl)
crude <- regtab_uv(d, "am", c("wt", "hp", "cyl"), method = glm,
                   method.args = list(family = binomial))
adj <- glm(am ~ wt + hp + cyl, family = binomial, data = d)
regtab(list(Crude = crude, Adjusted = adj))
```

### tt_mi(): multiply imputed fits

`tt_mi(fits)`: a list of fits (one per imputed dataset) as one pooled model
(Rubin's rules, Stata `mi estimate` degrees of freedom); `lm`, `glm`,
`coxph`, `crr` only. A `mice` `mira` (`with(imp, glm(...))`) can go straight
into `regtab()`; never pass `mice::pool()` output. Use `stats = c("n",
"mi_m", "fmi")`. `mice` completed data carry no labels: label the columns
before imputing.

```r
set.seed(1)
d <- data.frame(x = rnorm(60), z = rbinom(60, 1, 0.5))
d$y <- 1 + 0.5 * d$x + d$z + rnorm(60)
d$x[sample(60, 12)] <- NA
fits <- lapply(1:5, function(m) {
  dm <- d
  miss <- is.na(dm$x)
  dm$x[miss] <- rnorm(sum(miss), mean(d$x, na.rm = TRUE), sd(d$x, na.rm = TRUE))
  lm(y ~ x + z, data = dm)
})
regtab(tt_mi(fits), stats = c("n", "mi_m", "fmi"))
```

### tt_rates(): events, person-time and rates (Stata strate)

```
tt_rates(data, time = NULL, event = NULL, by = NULL, strata = NULL,
         per = 1, level = NULL, entry = NULL, fweight = NULL,
         missing = FALSE)
```

- One row per subject: `time` exit-time column, `event` 0/1 column (failure
  when non-zero), `by` exposure/grouping column(s). `entry`: late-entry time.
- Returns a data frame with `D` (events), `Y` (person-time / `per`), `Rate`,
  `Lower`, `Upper` (log-normal CI) per group: one block for `stratetab()`.
- **Leave `per = 1`** and scale in `stratetab()` with `ratescale`.

### stratetab(): incidence-rate table

```
stratetab(x, outcomes = NULL, outlabels = NULL, outcomeids = NULL,
          explabels = NULL, digits = 1, eventdigits = 0, pydigits = 0,
          unitlabel = NULL, pyscale = 1, ratescale = 1000,
          rateratio = FALSE, ratiodigits = 2, level = NULL,
          sheet = "Results", ...)
```

- `x`: one `tt_rates()` block, or a list of blocks in the order **every
  outcome for exposure 1, then every outcome for exposure 2, ...**
  (O1_E1, O2_E1, O1_E2, O2_E2). `outcomes`: number of outcomes (required
  with several blocks).
- `outlabels`: one per outcome (default: the event column's label or name).
  `outcomeids`: identities used by `hrcomptab()` to find each outcome's Cox
  model; default is the event column name, which matches the `Surv()` event
  variable. `explabels`: one per exposure (default: the `by` column's label).
- `ratescale` (default 1000) multiplies rates; `unitlabel` is the header
  number ("Per 1,000 PY"): keep them in step, e.g. `ratescale = 100,
  unitlabel = "100"`. `pyscale` divides person-time. Time in days, rates per
  1,000 person-years: `pyscale = 365.25, ratescale = 365250`.
- `rateratio = TRUE`: IRR column comparing each exposure block with exposure
  1, category by category (not usable with `hrcomptab()`).

```r
set.seed(2)
d <- data.frame(years = rexp(400, 0.05), death = rbinom(400, 1, 0.4),
                relapse = rbinom(400, 1, 0.2),
                arm = factor(rep(c("Placebo", "Drug"), 200),
                             levels = c("Placebo", "Drug")))
blocks <- list(tt_rates(d, "years", "death", by = "arm"),
               tt_rates(d, "years", "relapse", by = "arm"))
stratetab(blocks, outcomes = 2, outlabels = c("Death", "Relapse"),
          explabels = "Treatment arm")
```

### hrcomptab() and comptab(): Table 2 and composite tables

```
hrcomptab(ratetable, modeltables, rows = NULL, rownames = NULL,
          effect = NULL, reflabel = NULL, outcomemap = NULL, ...,
          rownames_exact = FALSE)
comptab(ratetable = NULL, modeltables = NULL, rows = NULL, rownames = NULL,
        effect = NULL, reflabel = NULL, outcomemap = NULL, compact = FALSE,
        separator = NULL, section = NULL, relabel = NULL, highlight = NULL,
        boldp = NULL, labelwidth = NULL, sheet = "Composite", ...,
        rownames_exact = FALSE)
```

- **Rate mode** (`hrcomptab()`, or `comptab()` with `ratetable`): a
  `stratetab()` table plus `regtab()` Cox tables; adds HR (95% CI) and
  p-value columns per outcome. Each model table must hold **one Cox model
  per outcome**, matched by identity: the rate table's `outcomeids` against
  each Cox model's `Surv()` event variable (or give `outcomemap`, one model
  label per outcome). The models must be on the HR scale. `effect`: header,
  `"aHR"` (default), `"HR"`, `"hazard ratio"`, `"adjusted hazard ratio"`.
  `reflabel` default `"Reference"`.
- Select the model rows with `rownames` (case-insensitive **substring**
  patterns of displayed labels; `*` and `?` wildcards) or `rows` (body row
  numbers, counting factor headings and Reference rows). One selection per
  model table: `rownames = list("female", "female")`. Per exposure block with
  k categories, select k - 1 rows (the leftover category is the reference
  and must be the model's base level).
- **Substring trap**: `"male"` also matches `"Female"`, `"treated"` matches
  `"Untreated"`. Use `rownames_exact = TRUE`, a unique pattern, or `rows`.
- **Vertical mode** (`comptab(list(tab1, tab2), rownames = ...)`): selected
  rows of several `regtab()`/`effecttab()` tables stacked, with `section`
  headings (one per table), `relabel = c("3" = "New label")`, `separator`
  rows, `compact = TRUE`. The tables must hold the same models.

```r
library(survival)
d <- survival::lung
d$event <- d$status - 1
d$years <- d$time / 365.25
d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
d$ecog <- factor(pmin(d$ph.ecog, 2), 0:2, c("0", "1", "2+"))
d <- d[!is.na(d$ecog), ]
rates <- stratetab(tt_rates(d, "years", "event", by = "sex"),
                   outlabels = "Death", explabels = "Sex",
                   ratescale = 100, unitlabel = "100")
crude <- regtab(coxph(Surv(years, event) ~ sex, data = d, ties = "breslow"))
adj <- regtab(coxph(Surv(years, event) ~ sex + age + ecog, data = d,
                    ties = "breslow"))
hrcomptab(rates, adj, rownames = "female")
comptab(list(crude, adj), rownames = list("female", "female"),
        section = c("Crude", "Adjusted for age and ECOG"), compact = TRUE)
```

### effecttab(): treatment effects and margins (marginaleffects)

```
effecttab(..., type = c("auto", "teffects", "margins"), effect = NULL,
          models = NULL, clean = FALSE, tlabels = NULL, data = NULL,
          method = NULL, level = NULL, digits = NULL, addrow = NULL,
          estimand = NULL, sheet = "Effects", ...)
```

- `...`: `marginaleffects::avg_comparisons()`, `avg_predictions()`,
  `avg_slopes()` or `hypotheses()` results, data frames (`term`, `estimate`,
  `conf.low`, `conf.high`, `p.value` or `std.error`), or matrices (columns
  estimate, lower, upper, p). Named arguments label the models.
- A **treatment-effect model** is a list of a contrast and its
  potential-outcome mean: `list(ate, po)`; several:
  `effecttab(list(IPW = list(ate, po), RA = list(ate2, po2)))`.
- `effect`: estimate header (`"ATE"`, `"RD"`, `"RR"`, `"AME"`, `"Pr(Y)"`).
  `clean = TRUE` labels contrasts "Treated vs Control" (else Stata keys
  `r1vs0.treat`); `tlabels = c("0" = "SSRI", "1" = "SNRI")` sets level names.
  `method`: `"ipw"`, `"ra"`, `"aipw"`, `"ipwra"`, ... for the methods sentence;
  `estimand`: `"ATE"`, `"ATT"`, `"ATC"`.
- Risk ratio: `avg_comparisons(fit, variables = "treat",
  comparison = "lnratioavg", transform = exp)`.
- Regression adjustment with `lm()`/`glm()`: pass `vcov = "HC0"` to
  marginaleffects to match Stata's `teffects ra`. After a weighted `glm`,
  use `vcov = tt_vcov(fit, "robust"), wts = d$w`.

```r
# needs: marginaleffects
d <- mtcars
d$am <- factor(d$am, 0:1, c("Automatic", "Manual"))
attr(d$am, "label") <- "Transmission"
fit <- glm(vs ~ am + wt, family = binomial, data = d)
ate <- marginaleffects::avg_comparisons(fit, variables = "am")
po <- marginaleffects::avg_predictions(fit, variables = list(am = "Automatic"))
effecttab(list(ate, po), clean = TRUE, effect = "RD", digits = 3)
effecttab(marginaleffects::avg_comparisons(fit, variables = "am"),
          marginaleffects::avg_slopes(fit, variables = "wt"),
          effect = "AME", models = c("Transmission", "Weight"))
```

```r
# Estimates from elsewhere as a data frame (no marginaleffects needed)
est <- data.frame(term = c("Treated vs control", "Dose high"),
                  estimate = c(0.12, -0.03), conf.low = c(0.05, -0.08),
                  conf.high = c(0.19, 0.02), p.value = c(0.0004, 0.25))
effecttab(est, effect = "RD")
```

`tt_effect_rows(x, type, data)` returns the rows `effecttab()` reads, for
inspection.

### wttab(): IPTW / MSM weight diagnostics

```
wttab(x, weights = NULL, by = NULL, period = NULL, trunc = NULL, data = NULL,
      overall = TRUE, by_labels = NULL, trunc_by = c("pooled", "period"),
      s.weights = TRUE, layout = c("auto", "wide", "long"), digits = NULL,
      sheet = "Weights", ...)
```

- `x`: a data frame (then `weights` = weight column name), a numeric vector,
  a `WeightIt::weightit()`/`weightitMSM` object, or an `ipw` result.
- Rows: N, Mean, SD, Min, P1, P25, Median, P75, P99, Max, ESS, ESS (%).
  `by`: treatment (0/1 shows Treated/Untreated; `by_labels = c("0" = "...",
  "1" = "...")` renames). `period`: follow-up period for time-varying
  weights (long layout). `trunc = c(0.01, 0.99)` or a list of pairs adds
  truncated copies. `digits = 3` suits stabilized weights.

```r
set.seed(1)
n <- 500
d <- data.frame(age = rnorm(n, 60, 10), ckd = rbinom(n, 1, 0.3))
d$treat <- rbinom(n, 1, plogis(-3 + 0.04 * d$age + 0.8 * d$ckd))
ps <- glm(treat ~ age + ckd, family = binomial, data = d)$fitted.values
d$sw <- ifelse(d$treat == 1, mean(d$treat) / ps,
               (1 - mean(d$treat)) / (1 - ps))
wttab(d, "sw", by = "treat", trunc = c(0.01, 0.99), digits = 3,
      title = "Stabilized weights")
```

### puttab() and stacktab(): any data frame as a styled sheet; composites

```
puttab(x, vars = NULL, subset = NULL, digits = NULL, varlabels = FALSE,
       noheader = FALSE, sheet = "Table", ...)
stacktab(blocks, xlsx = NULL, sheet = NULL, layout = "vstack", title = NULL,
         note = NULL, columnmerge = NULL, spacing = 0, display = FALSE,
         append = FALSE, sheetreplace = FALSE, csv = NULL, markdown = NULL)
```

- `puttab()`: `x` a data frame, numeric matrix (row names become labels) or
  `tt_table`; `varlabels = TRUE` uses `"label"` attributes as headers.
- `stacktab()`: `blocks` a list of `tt_table`s or `list(table = tab, label =
  "Section", rows = , cols = )`, or a Stata string of workbook sheet ranges
  `"sheet(A) rows(2/5) cols(B-D) label(Any use) \\ sheet(B) ..."` (needs
  `xlsx`). `layout = "hstack"` places blocks side by side (same row count).
  `columnmerge = "B+C as aHR (95% CI)"` merges two composite columns.

```r
fit <- lm(mpg ~ wt + hp, data = mtcars)
m <- cbind(b = coef(fit), se = sqrt(diag(vcov(fit))))
puttab(m, title = "OLS coefficients", digits = 3)
a <- regtab(lm(mpg ~ wt, data = mtcars), compact = TRUE)
b <- regtab(lm(mpg ~ hp, data = mtcars), compact = TRUE)
stacktab(list(list(table = a, label = "Weight"),
              list(table = b, label = "Horsepower")))
```

### tt_merge() and tt_stack(): combine tables in R

- `tt_merge(..., spanners = NULL, title = NULL, footnote = NULL)`: tables side
  by side, rows joined on their keys (named arguments become spanners).
  Works for `regtab()` tables; **refuses `table1_tc()` and `puttab()`
  tables**; all tables must come from the same command with the same columns.
- `tt_stack(..., groups = NULL, title = NULL, footnote = NULL)`: tables with
  the same header one under another, each under a group label row (e.g.
  subgroup analyses).

```r
d <- mtcars
d$cyl <- factor(d$cyl)
crude <- regtab(lm(mpg ~ cyl, d), stats = "n")
adj <- regtab(lm(mpg ~ cyl + wt + hp, d), stats = "n")
tt_merge(Crude = crude, Adjusted = adj)
auto <- regtab(lm(mpg ~ cyl + wt, d[d$am == 0, ]), stats = "n")
manual <- regtab(lm(mpg ~ cyl + wt, d[d$am == 1, ]), stats = "n")
tt_stack(Automatic = auto, Manual = manual)
```

### Writers, converters and helpers

- `tt_write_xlsx(x, path, sheet = NULL, open = FALSE)`; `tt_write_csv(x,
  path)` (path must end in `.csv`); `tt_write_markdown(x, path, append =
  FALSE)` (`.md`, `.markdown`, `.qmd`, `.rmd`). Writers use R names (`path`,
  `append`); the commands use the Stata names (`xlsx`, `markdown`,
  `mdappend`).
- `flextable::as_flextable(tab)` (Word; needs flextable), `tt_as_gt(tab)`
  (HTML; needs gt), `tt_as_gtsummary(tab)` (gtsummary >= 2.3.0),
  `tt_as_tinytable(tab)` (tinytable). They keep title, footnote, indents and
  Reference rows.
- `as.data.frame(tab)`: all cells as text (header rows first).
  `as_forest_data(tab)`: estimates, `ll`, `ul`, `pvalue` per row and model
  for forest plots (from `regtab`, `effecttab`, `comptab`, `hrcomptab`).
- `tabtools_options(font =, fontsize =, borderstyle =, digits =, boldp =,
  headercolor =, zebracolor =, persist = FALSE, clear = FALSE)`: session
  defaults. `tabtools_options()` returns them.
- `tt_vcov(fit, vce = "stata", cluster = NULL)`: the variance regtab uses;
  `tt_vce_types(fit)`, `tt_ci_methods(fit)`: what a model supports.
- `tt_from_modelsummary(x, exponentiate, effect_scale = NULL)`: a
  `modelsummary(..., output = "modelsummary_list")` object as a data frame
  for `regtab()`.
- `tt_as_factor(x, vars = NULL)`: haven-labelled columns to factors that keep
  Stata codes and labels.
- `tt_table()`, `validate_tt_table()`: build a table by hand (experimental).
- `tab$title <- "..."` and `tab$footnote <- "..."` change a table after it
  is made.

```r
d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20)
tab <- table1_tc(d, by = "arm", vars = c(x = "contn"))
out <- tempfile(fileext = ".xlsx")
tt_write_xlsx(tab, out, sheet = "Table 1")
tt_write_csv(tab, tempfile(fileext = ".csv"))
md <- tempfile(fileext = ".md")
tt_write_markdown(tab, md)
tt_write_markdown(tab, md, append = TRUE)
head(as.data.frame(tab))
as_forest_data(regtab(glm(breaks ~ wool + tension, family = poisson,
                          data = warpbreaks)))
```

```r
# needs: flextable, gt
d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20)
tab <- table1_tc(d, by = "arm", vars = c(x = "contn"))
ft <- flextable::as_flextable(tab)
flextable::save_as_docx(ft, path = tempfile(fileext = ".docx"))
g <- tt_as_gt(tab)
```

## Recipes

All recipes use this simulated cohort. Replace it with the user's data and
variable names from their data dictionary.

```r
library(tabtools)
library(survival)
set.seed(2026)
n <- 1500
cohort <- data.frame(
  age = round(rnorm(n, 62, 10)),
  female = rbinom(n, 1, 0.5),
  diabetes = rbinom(n, 1, 0.25),
  bmi = rlnorm(n, log(27), 0.15),
  smoking = factor(sample(c("Never", "Former", "Current"), n, TRUE),
                   levels = c("Never", "Former", "Current"))
)
lp <- with(cohort, -0.5 + 0.03 * (age - 62) + 0.7 * diabetes)
cohort$treat <- factor(rbinom(n, 1, plogis(lp)), 0:1, c("Control", "Treated"))
haz <- with(cohort, 0.1 * exp(0.03 * (age - 62) + 0.5 * diabetes -
                                0.4 * (treat == "Treated")))
t_ev <- rexp(n, haz)
cohort$years <- pmin(t_ev, 5)
cohort$died <- as.integer(t_ev <= 5)
cohort$mace <- rbinom(n, 1, 0.15)          # a second 0/1 outcome
cohort$died_1y <- as.integer(t_ev <= 1)
labs <- c(age = "Age (years)", female = "Female sex", diabetes = "Diabetes",
          bmi = "Body-mass index", smoking = "Smoking status",
          treat = "Treatment", died = "Death", mace = "MACE",
          died_1y = "Death within 1 year")
for (v in names(labs)) attr(cohort[[v]], "label") <- labs[[v]]
```

### 1. Baseline table by treatment, written to Excel

```r
t1 <- table1_tc(cohort, by = "treat",
                vars = c(age = "contn %5.1f", female = "bin", diabetes = "bin",
                         bmi = "conts %5.1f", smoking = "cat"),
                total = "before", smd = TRUE,
                title = "Table 1. Baseline characteristics",
                xlsx = file.path(tempdir(), "tables.xlsx"), sheet = "Table 1")
t1                        # returned invisibly because a file was written
t1$stored$methods
```

### 2. Crude and adjusted logistic regression side by side

```r
crude <- glm(died_1y ~ treat, family = binomial, data = cohort)
adj <- glm(died_1y ~ treat + age + female + diabetes + smoking,
           family = binomial, data = cohort)
regtab(list(Crude = crude, Adjusted = adj), stats = "n")
# Only the exposure rows, estimate and CI in one column
regtab(list(Crude = crude, Adjusted = adj), keep = "treat", compact = TRUE)
# Crude OR for every covariate beside the adjusted model
uv <- regtab_uv(cohort, "died_1y", c("treat", "age", "female", "diabetes",
                                     "smoking"),
                method = glm, method.args = list(family = binomial))
regtab(list(Crude = uv, Adjusted = adj))
```

### 3. Crude and adjusted Cox models

```r
c1 <- coxph(Surv(years, died) ~ treat, data = cohort, ties = "breslow")
c2 <- coxph(Surv(years, died) ~ treat + age + female + diabetes + smoking,
            data = cohort, ties = "breslow")
regtab(list(Crude = c1, Adjusted = c2), stats = c("n", "events"))
```

### 4. Incidence rates + hazard ratios ("Table 2")

```r
# One tt_rates() block per outcome (every outcome for exposure 1 first)
rates <- stratetab(list(tt_rates(cohort, "years", "died", by = "treat"),
                        tt_rates(cohort, "years", "mace", by = "treat")),
                   outcomes = 2, outlabels = c("Death", "MACE"),
                   explabels = "Treatment", ratescale = 100,
                   unitlabel = "100")
rates
# One Cox model per outcome in each model table; matched by event variable
hr <- regtab(coxph(Surv(years, died) ~ treat + age + diabetes, data = cohort,
                   ties = "breslow"),
             coxph(Surv(years, mace) ~ treat + age + diabetes, data = cohort,
                   ties = "breslow"))
hrcomptab(rates, hr, rownames = "treated", rownames_exact = TRUE)
```

`rownames_exact = TRUE` makes `"treated"` match the whole label "Treated"
only (without it, it would also match "Untreated"). Patterns take `*` and `?`
wildcards, not regular expressions.

### 5. IPTW: weighted Table 1, weight diagnostics, weighted outcome models

```r
ps <- glm(treat ~ age + female + diabetes + smoking, family = binomial,
          data = cohort)$fitted.values
p1 <- mean(cohort$treat == "Treated")
cohort$sw <- ifelse(cohort$treat == "Treated", p1 / ps, (1 - p1) / (1 - ps))
vars <- c(age = "contn %5.1f", female = "bin", diabetes = "bin",
          smoking = "cat")
# Balance: crude and weighted columns side by side, with SMDs
table1_tc(cohort, by = "treat", vars = vars, wt = "sw", smd = TRUE,
          wtcompare = TRUE, wtn = TRUE)
# Weight distribution by treatment, with 1/99 truncation
wttab(cohort, "sw", by = "treat", trunc = c(0.01, 0.99), digits = 3)
# Weighted outcome models: robust (pweight) variance for the IPTW columns
iptw_or <- suppressWarnings(glm(died_1y ~ treat, family = binomial,
                                data = cohort, weights = sw))
iptw_hr <- coxph(Surv(years, died) ~ treat, data = cohort, weights = sw,
                 ties = "breslow")
regtab(list(Crude = crude, Adjusted = adj, IPTW = iptw_or),
       vce = c("stata", "stata", "robust"), keep = "treat", vce_note = TRUE)
regtab(list(Crude = c1, Adjusted = c2, IPTW = iptw_hr), keep = "treat",
       vce_note = TRUE)
```

With WeightIt (optional package):

```r
# needs: WeightIt, marginaleffects
W <- WeightIt::weightit(treat ~ age + female + diabetes + smoking,
                        data = cohort, method = "glm", estimand = "ATE",
                        stabilize = TRUE)
wttab(W, digits = 3)
fit_w <- WeightIt::glm_weightit(died_1y ~ treat, data = cohort, weightit = W,
                                family = binomial)
regtab(fit_w)                                  # M-estimation variance
fit_rd <- WeightIt::glm_weightit(died_1y ~ treat, data = cohort, weightit = W)
ate <- marginaleffects::avg_comparisons(fit_rd, variables = "treat")
po <- marginaleffects::avg_predictions(fit_rd,
                                       variables = list(treat = "Control"))
effecttab(list(ate, po), clean = TRUE, effect = "RD", digits = 3)
```

### 6. Marginal effects / ATE tables with marginaleffects

```r
# needs: marginaleffects
fit <- glm(died_1y ~ treat + age + female + diabetes + smoking,
           family = binomial, data = cohort)
# Regression adjustment: risk difference and control potential-outcome mean
ate <- marginaleffects::avg_comparisons(fit, variables = "treat",
                                        vcov = "HC0")
po <- marginaleffects::avg_predictions(fit, variables = list(treat = "Control"),
                                       vcov = "HC0")
effecttab(list(ate, po), clean = TRUE, effect = "RD", method = "ra",
          digits = 3)
# Risk ratio via the log ratio
rr <- marginaleffects::avg_comparisons(fit, variables = "treat",
                                       comparison = "lnratioavg",
                                       transform = exp)
effecttab(list(rr, po), clean = TRUE, effect = "RR", digits = 3)
# Predicted risks and average marginal effects
effecttab(marginaleffects::avg_predictions(fit, variables = "treat"),
          marginaleffects::avg_comparisons(fit, variables = c("treat", "smoking")),
          models = c("Predicted risk", "Risk difference"), effect = "Risk",
          digits = 3)
```

### 7. Exporting: Excel, Word, CSV, Markdown

```r
out <- file.path(tempdir(), "paper_tables.xlsx")
t1 <- table1_tc(cohort, by = "treat", vars = vars, smd = TRUE)
t2 <- regtab(list(Crude = c1, Adjusted = c2), keep = "treat")
tt_write_xlsx(t1, out, sheet = "Table 1")        # one workbook,
tt_write_xlsx(t2, out, sheet = "Table 2")        # one sheet per table
tt_write_csv(t2, file.path(tempdir(), "table2.csv"))
md <- file.path(tempdir(), "tables.md")
tt_write_markdown(t1, md)
tt_write_markdown(t2, md, append = TRUE)
# Or write while building: regtab(..., xlsx = out, sheet = "Table 2",
#                                 csv = "t2.csv", markdown = "t2.md")
```

```r
# needs: flextable
ft <- flextable::as_flextable(t1)
flextable::save_as_docx(ft, path = file.path(tempdir(), "table1.docx"))
```

### 8. Combining tables

```r
# Side by side: separate regtab tables joined on their rows
tt_merge(Crude = regtab(c1), Adjusted = regtab(c2))
# Subgroups, one block per subgroup
men <- cohort[cohort$female == 0, ]
women <- cohort[cohort$female == 1, ]
attr(men$treat, "label") <- attr(women$treat, "label") <- "Treatment"  # subsetting drops labels
tt_stack(Men = regtab(coxph(Surv(years, died) ~ treat + age, data = men,
                            ties = "breslow"), keep = "treat"),
         Women = regtab(coxph(Surv(years, died) ~ treat + age, data = women,
                              ties = "breslow"), keep = "treat"))
# Selected rows of several model tables, with section headings
comptab(list(regtab(c1), regtab(c2)),
        rownames = list("treated", "treated"),
        section = c("Crude", "Adjusted"), compact = TRUE)
```

## Common mistakes

- **Nothing printed** after `xlsx =`/`csv =`/`markdown =`: the table is
  returned invisibly. Assign and print.
- **Labels missing**: the `"label"` attribute was set before `factor()` or
  before subsetting rows; both drop it. Label last.
- **`keep` matched no coefficient rows**: the factor was made in the formula
  (`factor(cyl)`: key is `factor(cyl)`), or the name is misspelled. Convert
  the column in the data, or write `keep = "factor(cyl)"`; or use
  `labelmatch = TRUE` to match displayed labels.
- **`rownames` selects too much** (`"male"` matches `"Female"`): patterns are
  substrings. Use `rownames_exact = TRUE` or `rows =`.
- **"The selected model rows (k) must match the non-reference rows of the
  rate table"**: in `hrcomptab()`, select exactly (categories - 1) rows per
  exposure block, and the reference category must be the model's base level.
- **Rates 1,000 times too high / tt_rates per refused**: compute with
  `tt_rates(per = 1)` (default) and scale in `stratetab(ratescale = ,
  unitlabel = )`. A block made with `per = 1000` is refused unless
  `ratescale` is given (then use `ratescale = 1, pyscale = 1/1000`).
- **Rate header unit wrong**: `unitlabel` does not follow `ratescale`; set
  both (`ratescale = 100, unitlabel = "100"`).
- **hrcomptab cannot match outcomes**: `outcomeids` of the rate table must be
  the Cox models' event variable names (the default when blocks come from
  `tt_rates()`), else give `outcomemap`; each model table needs one model per
  outcome; models must be HR scale; the rate table must not have
  `rateratio = TRUE`.
- **Cox HRs differ from Stata** in late digits: `coxph()` defaults to Efron
  ties, Stata `stcox` to Breslow. Fit with `ties = "breslow"`; a weighted
  Efron fit with tied times gets blank log-likelihood/AIC.
- **Weighted glm with model-based SEs**: regtab warns; use `vce = "robust"`
  for IPTW weights (or `WeightIt::glm_weightit()`).
- **`vce = "robust"`/`"cluster"` refused**: only `lm`, `glm`, `glm.nb`,
  `coxph`, `geeglm` support them (`tt_vce_types(fit)`).
- **Ordered factor / `contr.sum` refused** in `regtab()`: use a plain factor.
- **Unsupported model class** (`mgcv::gam`, `survey::svycoxph`,
  `coxph` with `frailty()`): pass a data frame of estimates (`term`,
  `estimate`, `conf.low`, `conf.high`, `p.value`), with
  `attr(x, "effect_scale") <- "HR"` for ratios.
- **`mice::pool()` result refused**: pass the `mira` (`with(imp, ...)`)
  object itself, or `tt_mi(list_of_fits)`.
- **`tt_merge()` refuses a table1_tc table** (its row keys are labels):
  describe the groups in one `table1_tc()` call with `by` (and `total`), or
  place whole tables with `stacktab()`.
- **Date columns refused by table1_tc()**: convert to numbers first.
- **Event indicator as a factor** after `tt_as_factor()` or
  `haven::as_factor()`: keep outcomes numeric; list only predictors in
  `tt_as_factor(vars = )`.
- **p-values differ from Stata's `table1_tc`**: R uses Welch tests and
  chi-squared without continuity correction; use `test_args` (e.g.
  `list(t.test = list(var.equal = TRUE))`) for Stata's pooled t-test.

## Making a data dictionary for the AI assistant

Paste a dictionary of the data, never the data itself. This lists variable
names, labels, types, value labels/factor levels and missing counts, with no
row-level values (uses the labelled package, CRAN):

```r
# needs: labelled
dict <- labelled::look_for(cohort, details = "basic")
dict <- labelled::convert_list_columns_to_character(dict)
dict$tabtools_type <- strsplit(table1_tc(cohort)$stored$types, " ")[[1]]
dict$missing <- NULL   # optional: drop missing counts (small numbers can identify)
knitr::kable(dict[, c("variable", "label", "col_type", "levels",
                      "value_labels", "tabtools_type")])
# write.csv(dict, "data_dictionary.csv", row.names = FALSE)
```

`tabtools_type` is the type `table1_tc()` would choose automatically; it uses
every column, so drop `Date` columns first (`table1_tc()` refuses them).
Do not use `details = "full"` (adds ranges, i.e. minimum and maximum
values) or frequency tables with small counts when the data are sensitive.
