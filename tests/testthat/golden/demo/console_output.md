---
title: "console_output"
---

<!-- **# Console: tabtools set/get/list/detail -->

```stata
. tabtools set font Calibri
```

```
tabtools: default font set to Calibri
```

```stata
. tabtools set fontsize 11
```

```
tabtools: default font size set to 11
```

```stata
. tabtools set borderstyle thin
```

```
tabtools: default border style set to thin
```

```stata
. tabtools get
```

```
--------------------------------------------------
tabtools - Persistent Formatting Defaults
--------------------------------------------------

  Font:        Calibri
  Font size:   11
  Border:      thin

  Set with: tabtools set font Calibri
            tabtools set fontsize 11
            tabtools set borderstyle thin
            tabtools set digits 3
            tabtools set boldp 0.05
  Clear:    tabtools set clear

```

```stata
. tabtools set clear
```

```
tabtools: all persistent defaults cleared
```

```stata
. noisily tabtools
```

```
----------------------------------------------------------------------
tabtools - Publication-Ready Table Export Suite
----------------------------------------------------------------------

Descriptive Statistics
  table1_tc    - Table 1 with automatic statistical tests
  desctab      - Consolidated descriptive Table 1 engine
  crosstab     - Cross-tabulation with association measures
  corrtab      - Correlation matrix with significance

Model Results
  regtab       - Regression results from any estimation command
  effecttab    - Treatment-effect style tables from supported results
  tabcell      - One publication cell from a fit, lincom, matrix, or numbers
  outtab       - Binary outcomes by exposure: counts plus model ratios

Incidence Rates
  stratetab    - Incidence rates from strate output
  ratetab      - Events, person-time, and rates with exact/robust CIs

Survival Analysis
  survtab      - Kaplan-Meier estimates, medians, and RMST

Composite
  comptab      - Combine regtab/effecttab frames into one table
  hrcomptab    - Attach regtab frames to a stratetab scaffold

Styled Export
  puttab       - Style an in-memory dataset, frame, or matrix as one sheet
  stacktab     - Assemble multi-sheet composite Excel tables from blocks

General Purpose
  tabtools     - Suite controller and persistent defaults
  tabtools_tips - Quick reference and worked recipes

----------------------------------------------------------------------
Total commands: 17

Help:     help tabtools for overview
          tabtools_tips for quick reference and recipes
          help <command> for individual command help
Settings: tabtools set font Calibri (persistent defaults)
          tabtools get (view current defaults)
```

```stata
. noisily tabtools, detail
```

```
----------------------------------------------------------------------
tabtools - Publication-Ready Table Export Suite
----------------------------------------------------------------------

Descriptive Statistics
  ------------------------------------------------------------
  table1_tc    Create publication-ready Table 1 with descriptive
               statistics. Automatically selects appropriate
               tests (t-test, Wilcoxon, chi-square, Fisher's
               exact) based on variable type and distribution.
               Supports continuous, categorical, and binary
               variables with customizable formatting.

  desctab      Direct access to the consolidated Table 1 engine
               number formats and optional composite cells such
               as events / N (%).

  crosstab     Cross-tabulation with row/column percentages
               and association measures. Supports chi-square,
               Fisher's exact, odds ratios, and risk ratios.

  corrtab      Correlation matrix with significance stars or
               p-values. Supports Pearson and Spearman. Exports
               lower, upper, or full triangle to Excel.

Model Results
  ------------------------------------------------------------
  regtab       Export regression results from any estimation
               command to Excel. Supports logistic, Cox, Poisson,
               linear, and other models. Configurable columns
               for coefficients, confidence intervals, p-values,
               and model statistics.

  effecttab    Export treatment-effect style tables from
               supported estimation results and matrix inputs.
               Formats effect estimates, confidence intervals,
               and p-values for publication output.

  tabcell      Format one estimate (CI), p-value, n (%),
               e/n (%), or median (Q1, Q3) cell from a fit,
               lincom/nlcom, a matrix row, or numbers.

  outtab       Tabulate binary outcomes by exposure with
               events/N (%) and one ratio column per model.

Incidence Rates
  ------------------------------------------------------------
  stratetab    Export stratified incidence rates from strate
               command output. Formats person-time, events,
               rates, and confidence intervals. Supports
               rate ratios and stratified analyses.

  ratetab      Compute events, person-time, and rates by group
               from stset or event/exposure data, with exact,
               Poisson, or cluster-robust intervals.

Survival Analysis
  ------------------------------------------------------------
  survtab      Export Kaplan-Meier estimates, median survival,
               and restricted mean survival time (RMST) to
               Excel. Supports multiple groups and time points.

Composite
  ------------------------------------------------------------
  comptab      Combine multiple regtab or effecttab frames
               into a single publication-ready table. Supports
               side-by-side and stacked layouts.

  hrcomptab    Build a final Table 2-style sheet by using
               a stratetab frame as the scaffold and injecting
               selected rows from one or more regtab frames.

Styled Export
  ------------------------------------------------------------
  puttab       Style a table already in memory -- the current
               dataset, a named frame, or a Stata matrix
               (e(b), r(table), collapse output) -- as one
               house-styled Excel sheet. Feeds stacktab.

  stacktab     Assemble multi-sheet composite Excel tables from
               source blocks (vstack or hstack), with column
               merges, titles, and notes.

General Purpose
  ------------------------------------------------------------
  tabtools     Suite controller for listing commands and
               managing persistent formatting defaults with
               set and get.

  tabtools_tips Quick reference and worked recipes.

```

```stata
. log off demo
```

```stata
. noisily table1_tc, by(treated)
>     vars(index_age contn %5.1f \ female bin \
>          education cat \ income_quintile cat \
>          born_abroad bin \ civil_status cat \
>          diabetes bin \ hypertension bin \ anxiety bin \ prior_cvd bin)
```

```
  +------------------------------------------------------------------+
  |                                SSRI         SNRI         p-value |
  |------------------------------------------------------------------|
  | No. (Column %) or Mean±SD      N=8,929      N=6,071              |
  |------------------------------------------------------------------|
  | Age at cohort entry (years)    55.9±13.4    62.0±12.4    <0.001  |
  |------------------------------------------------------------------|
  | Female sex                     5,530 (62)   3,442 (57)   <0.001  |
  |------------------------------------------------------------------|
  | Education level                                          0.71    |
  |    Primary                     2,280 (26)   1,580 (26)           |
  |    Secondary                   3,524 (39)   2,360 (39)           |
  |    Tertiary                    3,125 (35)   2,131 (35)           |
  |------------------------------------------------------------------|
  | Disposable income quintile                               0.22    |
  |    1                           1,727 (19)   1,226 (20)           |
  |    2                           1,853 (21)   1,179 (19)           |
  |    3                           1,776 (20)   1,221 (20)           |
  |    4                           1,758 (20)   1,237 (20)           |
  |    5                           1,815 (20)   1,208 (20)           |
  |------------------------------------------------------------------|
  | Born outside Sweden            1,359 (15)   900 (15)     0.51    |
  |------------------------------------------------------------------|
  | Marital status                                           0.41    |
  |    Single                      2,755 (31)   1,813 (30)           |
  |    Married                     3,072 (34)   2,164 (36)           |
  |    Divorced                    1,758 (20)   1,193 (20)           |
  |    Widowed                     1,344 (15)   901 (15)             |
  |------------------------------------------------------------------|
  | Diabetes                       2,566 (29)   4,359 (72)   <0.001  |
  |------------------------------------------------------------------|
  | Hypertension                   2,765 (31)   4,282 (71)   <0.001  |
  |------------------------------------------------------------------|
  | Anxiety disorder               5,748 (64)   4,479 (74)   <0.001  |
  |------------------------------------------------------------------|
  | Prior cardiovascular disease   4,629 (52)   3,763 (62)   <0.001  |
  +------------------------------------------------------------------+
```

```stata
. log off demo
```

```stata
. noisily table1_tc, by(treated)
>     vars(index_age contn %5.1f \ female bin \
>          education cat \ income_quintile cat \
>          born_abroad bin \ diabetes bin \ hypertension bin)
>     nopvalue smd
```

```
  +-----------------------------------------------------------------+
  |                               SSRI         SNRI         SMD     |
  |-----------------------------------------------------------------|
  | No. (Column %) or Mean±SD     N=8,929      N=6,071              |
  |-----------------------------------------------------------------|
  | Age at cohort entry (years)   55.9±13.4    62.0±12.4    0.477   |
  |-----------------------------------------------------------------|
  | Female sex                    5,530 (62)   3,442 (57)   0.107   |
  |-----------------------------------------------------------------|
  | Education level                                         0.014   |
  |    Primary                    2,280 (26)   1,580 (26)           |
  |    Secondary                  3,524 (39)   2,360 (39)           |
  |    Tertiary                   3,125 (35)   2,131 (35)           |
  |-----------------------------------------------------------------|
  | Disposable income quintile                              0.040   |
  |    1                          1,727 (19)   1,226 (20)           |
  |    2                          1,853 (21)   1,179 (19)           |
  |    3                          1,776 (20)   1,221 (20)           |
  |    4                          1,758 (20)   1,237 (20)           |
  |    5                          1,815 (20)   1,208 (20)           |
  |-----------------------------------------------------------------|
  | Born outside Sweden           1,359 (15)   900 (15)     0.011   |
  |-----------------------------------------------------------------|
  | Diabetes                      2,566 (29)   4,359 (72)   0.954   |
  |-----------------------------------------------------------------|
  | Hypertension                  2,765 (31)   4,282 (71)   0.862   |
  +-----------------------------------------------------------------+
```

```stata
. log off demo
```

```stata
. noisily desctab education cv_event,
>     vars(education cat \ cv_event bin)
```

```
  +-----------------------------------+
  |                        Total      |
  |-----------------------------------|
  | No. (Column %)         N=15,000   |
  |-----------------------------------|
  | Education level                   |
  |    Primary             3,860 (26) |
  |    Secondary           5,884 (39) |
  |    Tertiary            5,256 (35) |
  |-----------------------------------|
  | Cardiovascular event   5,249 (35) |
  +-----------------------------------+
```

```stata
. log off demo
```

```stata
. noisily survtab, times(365 730 1095 1460) by(treated)
>     rmst(1460) difference median timeunit(days)
```

```
  +---------------------------------------------------------------------------------------------------------------------
> ---------------------+
  |                                                           SSRI (N=8929)                SNRI (N=6071)   Difference (S
> SRI - SNRI)        p |
  |                         Median survival, d                       5862.0                       5212.0                
>       650.0   <0.001 |
  |                                   (95% CI)             (5814.0, 5914.0)             (5116.0, 5338.0)                
>                      |
  |                       Survival probability                                                                          
>                      |
  |                                   365 days                        99.9%                        99.7%             0.2
>  (0.1, 0.4)          |
  |                                   730 days                        99.7%                        99.1%             0.6
>  (0.3, 0.8)          |
  |                                  1095 days                        99.3%                        98.6%             0.7
>  (0.3, 1.0)          |
  |                                  1460 days                        98.0%                        96.3%             1.7
>  (1.1, 2.2)          |
  |                  RMST (1460-d), d (95% CI)   1452.56 (1451.11, 1454.01)   1444.17 (1441.38, 1446.97)         8.38 (5
> .24, 11.53)          |
  | Log-rank test: chi2(1) = 196.10, p < 0.001                                                                          
>                      |
  +---------------------------------------------------------------------------------------------------------------------
> ---------------------+

```

```stata
. log off demo
```

```stata
. noisily regtab, coef("OR") noint
```

```
  +-------------------------------------------------------------------+
  |                                    Model                          |
  |                                       OR         95% CI   p-value |
  |  Age at cohort entry (years)        1.05   (1.05, 1.05)    <0.001 |
  |                   Female sex        0.75   (0.69, 0.81)    <0.001 |
  |              Education level                                      |
  |                      Primary   Reference                          |
  |                    Secondary        0.95   (0.86, 1.06)      0.35 |
  |                     Tertiary        0.95   (0.86, 1.06)      0.37 |
  |                     Diabetes        8.27   (7.59, 9.03)    <0.001 |
  |                 Hypertension        7.07   (6.48, 7.71)    <0.001 |
  |             Anxiety disorder        1.02   (0.93, 1.12)      0.68 |
  | Prior cardiovascular disease        1.13   (1.04, 1.22)     0.005 |
  +-------------------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily regtab, coef("OR") noint compact
```

```
  +------------------------------------------------------------+
  |                                            Model           |
  |                                        OR 95% CI   p-value |
  |  Age at cohort entry (years)   1.05 (1.05, 1.05)    <0.001 |
  |                   Female sex   0.75 (0.69, 0.81)    <0.001 |
  |              Education level                               |
  |                      Primary           Reference           |
  |                    Secondary   0.95 (0.86, 1.06)      0.35 |
  |                     Tertiary   0.95 (0.86, 1.06)      0.37 |
  |                     Diabetes   8.27 (7.59, 9.03)    <0.001 |
  |                 Hypertension   7.07 (6.48, 7.71)    <0.001 |
  |             Anxiety disorder   1.02 (0.93, 1.12)      0.68 |
  | Prior cardiovascular disease   1.13 (1.04, 1.22)     0.005 |
  +------------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily regtab, coef("OR") noint nopvalue
```

```
  +---------------------------------------------------------+
  |                                    Model                |
  |                                       OR         95% CI |
  |  Age at cohort entry (years)        1.05   (1.05, 1.05) |
  |                   Female sex        0.75   (0.69, 0.81) |
  |              Education level                            |
  |                      Primary   Reference                |
  |                    Secondary        0.95   (0.86, 1.06) |
  |                     Tertiary        0.95   (0.86, 1.06) |
  |                     Diabetes        8.27   (7.59, 9.03) |
  |                 Hypertension        7.07   (6.48, 7.71) |
  |             Anxiety disorder        1.02   (0.93, 1.12) |
  | Prior cardiovascular disease        1.13   (1.04, 1.22) |
  +---------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily regtab, stats(n ll aic bic r2)
```

```
  +-----------------------------------------------------------------------------+
  |                                              Model                          |
  |                                                RRR         95% CI   p-value |
  | Secondary: Age at cohort entry (years)        1.00   (1.00, 1.00)      0.75 |
  |                  Secondary: Female sex        1.08   (1.00, 1.18)     0.063 |
  |                    Secondary: Diabetes        1.01   (0.93, 1.09)      0.86 |
  |                Secondary: Hypertension        0.99   (0.91, 1.08)      0.85 |
  |  Tertiary: Age at cohort entry (years)        1.00   (1.00, 1.00)      0.76 |
  |                   Tertiary: Female sex        1.02   (0.93, 1.11)      0.71 |
  |                     Tertiary: Diabetes        1.03   (0.95, 1.12)      0.50 |
  |                 Tertiary: Hypertension        1.00   (0.92, 1.09)      0.98 |
  |                           Observations      15,000                          |
  |                                    AIC    32530.06                          |
  |                                    BIC    32606.21                          |
  |                         Log-likelihood   -16255.03                          |
  |                              Pseudo R²       0.000                          |
  +-----------------------------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily regtab, stats(n aic bic ll) models("ZIP" \ "ZINB")
```

```
  +---------------------------------------------------------------------------------------------------------------------
> -+
  |                                                 ZIP                                  ZINB                           
>  |
  |                                               Coef.           95% CI   p-value      Coef.           95% CI   p-value
>  |
  |                   Event count: Treatment      -0.15   (-0.26, -0.05)     0.003      -0.14   (-0.26, -0.03)     0.016
>  |
  |                 Event count: Age z-score       0.39     (0.34, 0.44)    <0.001       0.39     (0.33, 0.45)    <0.001
>  |
  |                      Event count: Female       0.30     (0.19, 0.41)    <0.001       0.32     (0.19, 0.45)    <0.001
>  |
  |               Inflation equation: Female       0.50     (0.15, 0.84)     0.005       0.72     (0.26, 1.19)     0.002
>  |
  | Inflation equation: Structural-zero risk       0.85     (0.66, 1.04)    <0.001       1.07     (0.79, 1.35)    <0.001
>  |
  |                             Observations      1,500                                 1,500                           
>  |
  |                                      AIC    4338.98                               4307.53                           
>  |
  |                                      BIC    4376.17                               4350.04                           
>  |
  |                           Log-likelihood   -2162.49                              -2145.76                           
>  |
  +---------------------------------------------------------------------------------------------------------------------
> -+

```

```stata
. log off demo
```

```stata
. noisily regtab, stats(n ll aic bic r2)
```

```
  +-----------------------------------------------------------------------------+
  |                                              Model                          |
  |                                              Coef.         95% CI   p-value |
  |             Annual cost: Dose intensity       1.75   (1.49, 2.02)    <0.001 |
  | Selection equation: Participation score       0.51   (0.43, 0.60)    <0.001 |
  |                            Observations      1,200                          |
  |                                     AIC    3425.86                          |
  |                                     BIC    3451.31                          |
  |                          Log-likelihood   -1707.93                          |
  |                               Pseudo R²      0.100                          |
  +-----------------------------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily corrtab index_age crp prior_hosp,
>     star(0.05 0.01 0.001)
```

```
  +----------------------------------------------------------------------------------------------------------------+
  |                               Age at cohort entry (years)   C-reactive protein (mg/L)   Prior hospitalizations |
  | Age at cohort entry (years)                          1.00                                                      |
  |   C-reactive protein (mg/L)                         -0.01                        1.00                          |
  |      Prior hospitalizations                          0.00                        0.01                     1.00 |
  +----------------------------------------------------------------------------------------------------------------+

* p<0.05, ** p<0.01, *** p<0.001

```

```stata
. log off demo
```

```stata
. noisily crosstab treated female, or label
```

```
  +----------------------------------------------------------------------------------------------+
  |                                     Treatment group            Male          Female    Total |
  |                                                SSRI   3,399 (56.4%)   5,530 (61.6%)    8,929 |
  |                                                SNRI   2,629 (43.6%)   3,442 (38.4%)    6,071 |
  |                                               Total           6,028           8,972   15,000 |
  | Pearson's chi-squared test: chi2 = 41.24, p < 0.001                                          |
  |                         OR = 0.8 (95% CI: 0.8, 0.9)                                          |
  +----------------------------------------------------------------------------------------------+

```

```stata
. log off demo
```

## smallcells(): primary and complementary suppression

### Primary suppression only: table1_tc

```stata
. noisily table1_tc category, by(group) vars(category cat)
>     total(after) smallcells(5)
```

```
  +-------------------------------------------------------------+
  |                  Control   Treatment   Total     p-value    |
  |-------------------------------------------------------------|
  | No. (Column %)   N=5       N=5         N=10                 |
  |-------------------------------------------------------------|
  | Characteristic                                   Suppressed |
  |    Absent        <5        <5          5                    |
  |    Present       <5        <5          5                    |
  +-------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

### Primary suppression only: desctab

```stata
. noisily desctab category, by(group) vars(category cat)
>     total(after) smallcells(5)
```

```
  +-------------------------------------------------------------+
  |                  Control   Treatment   Total     p-value    |
  |-------------------------------------------------------------|
  | No. (Column %)   N=5       N=5         N=10                 |
  |-------------------------------------------------------------|
  | Characteristic                                   Suppressed |
  |    Absent        <5        <5          5                    |
  |    Present       <5        <5          5                    |
  +-------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

### Primary suppression only: crosstab

```stata
. noisily crosstab group category, label smallcells(5)
```

```
  +------------------------------------------------------------+
  |                     Study group   Absent   Present   Total |
  |                         Control       <5        <5       5 |
  |                       Treatment       <5        <5       5 |
  |                           Total        5         5      10 |
  | Fisher's exact test: Suppressed                            |
  +------------------------------------------------------------+

Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction.
```

```stata
. log off demo
```

### Complementary suppression: table1_tc

```stata
. noisily table1_tc category, by(group) vars(category cat)
>     total(after) smallcells(5)
```

```
  +-------------------------------------------------------------+
  |                  Control   Treatment   Total     p-value    |
  |-------------------------------------------------------------|
  | No. (Column %)   N=10      N=10        N=20                 |
  |-------------------------------------------------------------|
  | Characteristic                                   Suppressed |
  |    Absent        <5        ≥5          8                    |
  |    Present       ≥5        <5          12                   |
  +-------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

### Complementary suppression: desctab

```stata
. noisily desctab category, by(group) vars(category cat)
>     total(after) smallcells(5)
```

```
  +-------------------------------------------------------------+
  |                  Control   Treatment   Total     p-value    |
  |-------------------------------------------------------------|
  | No. (Column %)   N=10      N=10        N=20                 |
  |-------------------------------------------------------------|
  | Characteristic                                   Suppressed |
  |    Absent        <5        ≥5          8                    |
  |    Present       ≥5        <5          12                   |
  +-------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

### Complementary suppression: crosstab

```stata
. noisily crosstab group category, label smallcells(5)
```

```
  +------------------------------------------------------------+
  |                     Study group   Absent   Present   Total |
  |                         Control       <5        ≥5      10 |
  |                       Treatment       ≥5        <5      10 |
  |                           Total        8        12      20 |
  | Fisher's exact test: Suppressed                            |
  +------------------------------------------------------------+

Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction.
```

```stata
. log off demo
```

### Binary variable suppression: table1_tc

```stata
. noisily table1_tc rare_ae, by(group) vars(rare_ae bin)
>     total(after) smallcells(5)
```

```
  +-----------------------------------------------------------------+
  |                      Control   Treatment   Total     p-value    |
  |-----------------------------------------------------------------|
  | No. (Column %)       N=50      N=50        N=100                |
  |-----------------------------------------------------------------|
  | Rare adverse event   <5        <5          5         Suppressed |
  +-----------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

### Binary variable suppression: desctab

```stata
. noisily desctab rare_ae, by(group) vars(rare_ae bin)
>     total(after) smallcells(5)
```

```
  +-----------------------------------------------------------------+
  |                      Control   Treatment   Total     p-value    |
  |-----------------------------------------------------------------|
  | No. (Column %)       N=50      N=50        N=100                |
  |-----------------------------------------------------------------|
  | Rare adverse event   <5        <5          5         Suppressed |
  +-----------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are wit
> hheld for any variable carrying a suppressed count.
```

```stata
. log off demo
```

### Primary-only suppression: table1_tc smallcells(5, primary)

```stata
. noisily table1_tc category, by(group) vars(category cat)
>     total(after) smallcells(5, primary)
```

```
  +----------------------------------------------------------+
  |                  Control   Treatment   Total     p-value |
  |----------------------------------------------------------|
  | No. (Column %)   N=10      N=10        N=20              |
  |----------------------------------------------------------|
  | Characteristic                                   0.068   |
  |    Absent        <5        6 (60)      8 (40)            |
  |    Present       8 (80)    <5          12 (60)           |
  +----------------------------------------------------------+
Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: no complementary cells are masked, an
> d other cells, totals and tests are shown as computed). This protects printed counts only.
```

```stata
. log off demo
```

### cellreplace(): overwrite one structurally non-reportable cell

```stata
. noisily table1_tc, by(treated) vars(education cat \ civil_status cat)
>     cellreplace("Widowed" "SNRI" "Not reported")
```

```
  +-------------------------------------------------------+
  |                   SSRI         SNRI           p-value |
  |-------------------------------------------------------|
  | No. (Column %)    N=8,929      N=6,071                |
  |-------------------------------------------------------|
  | Education level                               0.71    |
  |    Primary        2,280 (26)   1,580 (26)             |
  |    Secondary      3,524 (39)   2,360 (39)             |
  |    Tertiary       3,125 (35)   2,131 (35)             |
  |-------------------------------------------------------|
  | Marital status                                0.41    |
  |    Single         2,755 (31)   1,813 (30)             |
  |    Married        3,072 (34)   2,164 (36)             |
  |    Divorced       1,758 (20)   1,193 (20)             |
  |    Widowed        1,344 (15)   Not reported           |
  +-------------------------------------------------------+
```

```stata
. log off demo
```

## Session destinations and defaults

```stata
. noisily tabtools set smallcells 5 primary
```

```
tabtools: session smallcells 5 (primary; desctab, table1_tc, crosstab; nosmallcells skips it)
```

```stata
. noisily tabtools query
```

```
tabtools session settings
  Workbook:    (not set)
  Markdown:    (not set)
  Headershade: (not set)
  Smallcells:  5 (primary)
  Borderstyle: (not set)
```

```stata
. noisily tabtools set smallcells clear
```

```
tabtools: session smallcells cleared
```

```stata
. log off demo
```

## tabcell: one publication cell at a time

```stata
. noisily tabcell est treated, eform
```

```
1.09 (1.02, 1.17)
```

```stata
. noisily tabcell est treated, eform format(%5.3f) sep(" to ")
```

```
1.091 (1.017 to 1.171)
```

```stata
. quietly lincom treated + female
```

```stata
. noisily tabcell est, lincom
```

```
1.10 (1.00, 1.22)
```

```stata
. noisily tabcell p, p(0.0004)
```

```
<0.001
```

```stata
. noisily tabcell np, n(3) d(40) mincell(5)
```

```
<5
```

```stata
. noisily tabcell enp, e(2149) n(6066)
```

```
2,149/6,066 (35.4)
```

```stata
. quietly summarize follow_up, detail
```

```stata
. noisily tabcell iqr, median(`r(p50)') q1(`r(p25)') q3(`r(p75)') format(%6.0fc)
```

```
3,452 (2,032, 5,018)
```

```stata
. log off demo
```

## ratetab: events, person-years, and rates from stset data

```stata
. noisily ratetab treated education, outlabels("CV events")
>     explabels("Treatment" \ "Education")
```

```
  +----------------------------------------------------------------------+
  |     Exposure   CV events                                             |
  |                   Events   Person-Years (PY)   Per 1,000 PY (95% CI) |
  |    Treatment                                                         |
  |         SSRI       3,042              90,955       33.4 (32.3, 34.7) |
  |         SNRI       2,207              52,978       41.7 (39.9, 43.4) |
  |    Education                                                         |
  |      Primary       1,317              37,344       35.3 (33.4, 37.2) |
  |    Secondary       2,077              56,356       36.9 (35.3, 38.5) |
  |     Tertiary       1,855              50,233       36.9 (35.3, 38.6) |
  +----------------------------------------------------------------------+

```

```stata
. noisily ratetab treated, ci(cluster(region)) outlabels("CV events")
>     cformat(%5.2f) sep(" to ")
```

```
  +--------------------------------------------------------------------------+
  |        Exposure   CV events                                              |
  |                      Events   Person-Years (PY)    Per 1,000 PY (95% CI) |
  | Treatment group                                                          |
  |            SSRI       3,042              90,955   33.44 (32.50 to 34.42) |
  |            SNRI       2,207              52,978   41.66 (40.14 to 43.23) |
  +--------------------------------------------------------------------------+

(ratetab: treated: 6 clusters of region)
```

```stata
. log off demo
```

## outtab: events/N by exposure plus one ratio column per model

```stata
. noisily outtab cv_event selfharm fracture gi_bleed, exposure(treated)
>     models("" \ "index_age female i.education diabetes hypertension")
>     modellabels("Crude" \ "Adjusted")
>     estimator(poisson, irr vce(robust)) ratiolabel("RR")
```

```
  +-------------------------------------------------------------------------------------------------------------+
  |                        SNRI, events/N (%)   SSRI, events/N (%)   Crude, RR (95% CI)   Adjusted, RR (95% CI) |
  |-------------------------------------------------------------------------------------------------------------|
  | Cardiovascular event   2,207/6,071 (36.4)   3,042/8,929 (34.1)    1.07 (1.02, 1.12)       1.03 (0.98, 1.09) |
  |            Self-harm   1,234/6,071 (20.3)   1,837/8,929 (20.6)    0.99 (0.93, 1.05)       0.97 (0.89, 1.05) |
  |             Fracture   1,214/6,071 (20.0)   1,717/8,929 (19.2)    1.04 (0.97, 1.11)       1.00 (0.92, 1.08) |
  |          GI bleeding   1,261/6,071 (20.8)   1,723/8,929 (19.3)    1.08 (1.01, 1.15)       1.02 (0.94, 1.11) |
  +-------------------------------------------------------------------------------------------------------------+
```

```stata
. log off demo
```

## regtab: cformat(), transpose, and cellnote()

```stata
. noisily regtab, cformat(%5.3f) sep(" to ") noint models("Crude \ Adjusted")
```

```
  +-----------------------------------------------------------------------------------------------------------+
  |                               Crude                                 Adjusted                              |
  |                                  OR             95% CI   p-value          OR             95% CI   p-value |
  |             Treatment group   1.105   (1.032 to 1.183)     0.004       1.050   (0.966 to 1.143)      0.25 |
  | Age at cohort entry (years)                                            1.002   (1.000 to 1.005)     0.072 |
  |                  Female sex                                            1.010   (0.943 to 1.081)      0.79 |
  |             Education level                                                                               |
  |                     Primary                                        Reference                              |
  |                   Secondary                                            1.053   (0.967 to 1.147)      0.23 |
  |                    Tertiary                                            1.053   (0.965 to 1.149)      0.25 |
  |                    Diabetes                                            1.095   (1.016 to 1.180)     0.018 |
  |                Hypertension                                            0.995   (0.925 to 1.071)      0.90 |
  +-----------------------------------------------------------------------------------------------------------+

```

```stata
. noisily regtab, transpose keep(treated) stats(n) nopvalue
>     models("Crude \ Adjusted")
```

```
  +---------------------------------------------+
  |            Observations     Treatment group |
  |                                 OR (95% CI) |
  |    Crude         15,000   1.11 (1.03, 1.18) |
  | Adjusted         15,000   1.05 (0.97, 1.14) |
  +---------------------------------------------+

```

```stata
. noisily regtab, cellnote("Diabetes" 1 "Not in model") nopvalue noint
>     models("Crude \ Adjusted")
```

```
  +--------------------------------------------------------------------------------------+
  |                                      Crude                   Adjusted                |
  |                                         OR         95% CI          OR         95% CI |
  |             Treatment group           1.11   (1.03, 1.18)        1.05   (0.97, 1.14) |
  | Age at cohort entry (years)                                      1.00   (1.00, 1.01) |
  |                  Female sex                                      1.01   (0.94, 1.08) |
  |             Education level                                                          |
  |                     Primary                                 Reference                |
  |                   Secondary                                      1.05   (0.97, 1.15) |
  |                    Tertiary                                      1.05   (0.96, 1.15) |
  |                    Diabetes   Not in model                       1.09   (1.02, 1.18) |
  |                Hypertension                                      1.00   (0.92, 1.07) |
  +--------------------------------------------------------------------------------------+

```

```stata
. log off demo
```

### regtab: stats(events people exposure) from tabtools fitcount, mincount()

```stata
. noisily regtab, stats(events people exposure) exposurelabel("Person-years")
>     mincount(15) models("Crude \ Adjusted")
```

```
  +---------------------------------------------------------------------------------------------+
  |                         Crude                             Adjusted                          |
  |                            HR         95% CI   p-value          HR         95% CI   p-value |
  | Healthcare region                                                                           |
  |         Stockholm   Reference                            Reference                          |
  |    Uppsala/Orebro        1.44   (0.68, 3.03)      0.34        1.50   (0.70, 3.20)      0.30 |
  |         Southeast        1.14   (0.55, 2.38)      0.72        1.21   (0.58, 2.53)      0.61 |
  |             South           –                                    –                          |
  |              West        1.18   (0.57, 2.46)      0.65        1.24   (0.59, 2.59)      0.57 |
  |             North           –                                    –                          |
  |   Treatment group                                             1.22   (0.77, 1.92)      0.39 |
  |        Female sex                                             1.23   (0.79, 1.91)      0.36 |
  |            Events          86                                   86                          |
  |            People         450                                  450                          |
  |      Person-years       4,102                                4,102                          |
  +---------------------------------------------------------------------------------------------+

```

```stata
. log off demo
```

```stata
. noisily puttab term ahr ci using "`_pipe_xlsx'", sheet("Block Primary")
>     title("Source block: Primary HRT exposure model") varlabels
```

```
puttab: wrote 3 data rows x 3 cols (data source) to sheet Block Primary in /tmp/tabtools-3g-update-5u5x6zdp/r-tmp/RtmpHQ
> EGmA/tabtools-installed-update-24ad2eaeb7875/tabtools-demo-stata-24ae7590618b2/export/tabtools/demo/_pipeline_parts.xl
> sx
```

```stata
. log off demo
```

```stata
. noisily puttab term ahr ci using "`_pipe_xlsx'", sheet("Block Dose")
>     title("Source block: Estrogen dose-response model") varlabels
```

```
puttab: wrote 2 data rows x 3 cols (data source) to sheet Block Dose in /tmp/tabtools-3g-update-5u5x6zdp/r-tmp/RtmpHQEGm
> A/tabtools-installed-update-24ad2eaeb7875/tabtools-demo-stata-24ae7590618b2/export/tabtools/demo/_pipeline_parts.xlsx
```

```stata
. noisily stacktab using "`_pipe_xlsx'", sheet("Composite")
>     blocks(sheet(Block Primary) rows(2/5) cols(B-D) label(Any HRT use) \
>            sheet(Block Dose) rows(2/4) cols(B-D) label(By estrogen dose))
>     columnmerge(B+C as "aHR (95% CI)")
>     display
>     title("Hormone therapy and recurrent events")
>     note("aHR = adjusted hazard ratio; CI = confidence interval.")
```

```
Hormone therapy and recurrent events
  +--------------------------------------+
  |      Any HRT use        aHR (95% CI) |
  |          Any HRT   0.82 (0.69, 0.98) |
  |    Former smoker   1.14 (0.97, 1.34) |
  |   Current smoker   1.46 (1.21, 1.77) |
  | By estrogen dose        aHR (95% CI) |
  |         Low dose   0.91 (0.74, 1.12) |
  |        High dose   0.73 (0.58, 0.92) |
  +--------------------------------------+

aHR = adjusted hazard ratio; CI = confidence interval.
stacktab: 2 blocks -> 7 rows written -> sheet Composite
```

```stata
. log off demo
```

## Markdown report export

```stata
. noisily table1_tc, by(treated)
>     vars(index_age contn %5.1f \ female bin \ diabetes bin \ hypertension bin)
>     title("Table 1. Baseline Characteristics")
>     markdown("`markdown_report_export'")
```

```
  +-----------------------------------------------------------------+
  |                               SSRI         SNRI         p-value |
  |-----------------------------------------------------------------|
  | No. (Column %) or Mean±SD     N=8,929      N=6,071              |
  |-----------------------------------------------------------------|
  | Age at cohort entry (years)   55.9±13.4    62.0±12.4    <0.001  |
  |-----------------------------------------------------------------|
  | Female sex                    5,530 (62)   3,442 (57)   <0.001  |
  |-----------------------------------------------------------------|
  | Diabetes                      2,566 (29)   4,359 (72)   <0.001  |
  |-----------------------------------------------------------------|
  | Hypertension                  2,765 (31)   4,282 (71)   <0.001  |
  +-----------------------------------------------------------------+
Markdown exported to tabtools/demo/demo_markdown_report.md
```

```stata
. noisily crosstab treated female, or label
>     title("Table 2. Treatment by Sex")
>     markdown("`markdown_report_export'") mdappend
```

```
Table 2. Treatment by Sex
  +----------------------------------------------------------------------------------------------+
  |                                     Treatment group            Male          Female    Total |
  |                                                SSRI   3,399 (56.4%)   5,530 (61.6%)    8,929 |
  |                                                SNRI   2,629 (43.6%)   3,442 (38.4%)    6,071 |
  |                                               Total           6,028           8,972   15,000 |
  | Pearson's chi-squared test: chi2 = 41.24, p < 0.001                                          |
  |                         OR = 0.8 (95% CI: 0.8, 0.9)                                          |
  +----------------------------------------------------------------------------------------------+

Markdown exported to tabtools/demo/demo_markdown_report.md
```

```stata
. noisily corrtab index_age crp prior_hosp, star(0.05 0.01 0.001)
>     title("Table 3. Correlation Matrix")
>     markdown("`markdown_report_export'") mdappend
```

```
Table 3. Correlation Matrix
  +----------------------------------------------------------------------------------------------------------------+
  |                               Age at cohort entry (years)   C-reactive protein (mg/L)   Prior hospitalizations |
  | Age at cohort entry (years)                          1.00                                                      |
  |   C-reactive protein (mg/L)                         -0.01                        1.00                          |
  |      Prior hospitalizations                          0.00                        0.01                     1.00 |
  +----------------------------------------------------------------------------------------------------------------+

* p<0.05, ** p<0.01, *** p<0.001

Markdown exported to tabtools/demo/demo_markdown_report.md
```

```stata
. preserve
```

```stata
. keep id index_age treated female cv_event
```

```stata
. keep in 1/6
```

```
(14,994 observations deleted)
```

```stata
. noisily puttab id index_age treated female cv_event,
>     varlabels
>     title("Table 4. First Six Analysis Records")
>     markdown("`markdown_report_export'") mdappend
```

```
Markdown exported to tabtools/demo/demo_markdown_report.md
```

```stata
. restore
```

```stata
. noisily display as text "Markdown report written to tabtools/demo/demo_markdown_report.md"
```

```
Markdown report written to tabtools/demo/demo_markdown_report.md
```

```stata
. noisily display as text "The file contains multiple GitHub-Flavored Markdown tables appended in one report."
```

```
The file contains multiple GitHub-Flavored Markdown tables appended in one report.
```

```stata
. noisily type "`markdown_report'"
```

```
### Table 1. Baseline Characteristics

| No. (Column %) or Mean±SD | SSRI (N=8,929) | SNRI (N=6,071) | p-value |
| --- | --- | --- | --- |
| Age at cohort entry (years) | 55.9±13.4 | 62.0±12.4 | \<0.001 |
| Female sex | 5,530 (62) | 3,442 (57) | \<0.001 |
| Diabetes | 2,566 (29) | 4,359 (72) | \<0.001 |
| Hypertension | 2,765 (31) | 4,282 (71) | \<0.001 |

### Table 2. Treatment by Sex

| Treatment group | Male | Female | Total |
| --- | --- | --- | --- |
| SSRI | 3,399 (56.4%) | 5,530 (61.6%) | 8,929 |
| SNRI | 2,629 (43.6%) | 3,442 (38.4%) | 6,071 |
| Total | 6,028 | 8,972 | 15,000 |
| Pearson's chi-squared test: chi2 = 41.24, p \< 0.001 |  |  |  |
| OR = 0.8 (95% CI: 0.8, 0.9) |  |  |  |

### Table 3. Correlation Matrix

|  | Age at cohort entry (years) | C-reactive protein (mg/L) | Prior hospitalizations |
| --- | --- | --- | --- |
| Age at cohort entry (years) | 1.00 |  |  |
| C-reactive protein (mg/L) | -0.01 | 1.00 |  |
| Prior hospitalizations | 0.00 | 0.01 | 1.00 |

*\* p\<0.05, \*\* p\<0.01, \*\*\* p\<0.001*

### Table 4. First Six Analysis Records

| Person identifier | Age at cohort entry (years) | Treatment group | Female sex | Cardiovascular event |
| --- | --- | --- | --- | --- |
| 1 | 73.75 | SSRI | Male | Yes |
| 2 | 61.80 | SSRI | Female | No |
| 3 | 46.22 | SSRI | Female | No |
| 4 | 51.62 | SNRI | Female | Yes |
| 5 | 54.89 | SNRI | Female | Yes |
| 6 | 75.15 | SNRI | Male | No |
```

```stata
. log off demo
```
