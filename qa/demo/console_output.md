```r
tabtools_options(font = "Calibri")
```

```r
tabtools_options(fontsize = 11)
```

```r
tabtools_options(borderstyle = "thin")
```

```r
tabtools_options()
```

```
$font
[1] "Calibri"

$fontsize
[1] 11

$borderstyle
[1] "thin"

```

```r
tabtools_options(clear = TRUE)
```

```r
table1_tc(analysis, by = "treated", vars = c(index_age = "contn %5.1f", female = "bin", 
    education = "cat", income_quintile = "cat", born_abroad = "bin", civil_status = "cat", 
    diabetes = "bin", hypertension = "bin", anxiety = "bin", prior_cvd = "bin"))
```

```
  +------------------------------------------------------------------+
  |                                SSRI         SNRI         p-value |
  |------------------------------------------------------------------|
  | No. (Column %) or Mean±SD      N=8,934      N=6,066              |
  |------------------------------------------------------------------|
  | Age at cohort entry (years)    58.3±13.4    58.5±13.3    0.24    |
  |------------------------------------------------------------------|
  | Female sex                     5,351 (60)   3,621 (60)   0.80    |
  |------------------------------------------------------------------|
  | Education level                                          0.11    |
  |    Primary                     2,333 (26)   1,527 (25)           |
  |    Secondary                   3,530 (40)   2,354 (39)           |
  |    Tertiary                    3,071 (34)   2,185 (36)           |
  |------------------------------------------------------------------|
  | Disposable income quintile                               0.73    |
  |    1                           1,778 (20)   1,175 (19)           |
  |    2                           1,783 (20)   1,249 (21)           |
  |    3                           1,769 (20)   1,228 (20)           |
  |    4                           1,786 (20)   1,209 (20)           |
  |    5                           1,818 (20)   1,205 (20)           |
  |------------------------------------------------------------------|
  | Born outside Sweden            1,362 (15)   897 (15)     0.44    |
  |------------------------------------------------------------------|
  | Marital status                                           0.34    |
  |    Single                      2,764 (31)   1,804 (30)           |
  |    Married                     3,074 (34)   2,162 (36)           |
  |    Divorced                    1,763 (20)   1,188 (20)           |
  |    Widowed                     1,333 (15)   912 (15)             |
  |------------------------------------------------------------------|
  | Diabetes                       4,107 (46)   2,818 (46)   0.56    |
  |------------------------------------------------------------------|
  | Hypertension                   4,112 (46)   2,935 (48)   0.005   |
  |------------------------------------------------------------------|
  | Anxiety disorder               6,079 (68)   4,148 (68)   0.66    |
  |------------------------------------------------------------------|
  | Prior cardiovascular disease   5,002 (56)   3,390 (56)   0.90    |
  +------------------------------------------------------------------+
```

```r
table1_tc(analysis, by = "treated", vars = c(index_age = "contn %5.1f", female = "bin", 
    education = "cat", income_quintile = "cat", born_abroad = "bin", diabetes = "bin", 
    hypertension = "bin"), nopvalue = TRUE, smd = TRUE)
```

```
  +-----------------------------------------------------------------+
  |                               SSRI         SNRI         SMD     |
  |-----------------------------------------------------------------|
  | No. (Column %) or Mean±SD     N=8,934      N=6,066              |
  |-----------------------------------------------------------------|
  | Age at cohort entry (years)   58.3±13.4    58.5±13.3    0.019   |
  |-----------------------------------------------------------------|
  | Female sex                    5,351 (60)   3,621 (60)   0.004   |
  |-----------------------------------------------------------------|
  | Education level                                         0.035   |
  |    Primary                    2,333 (26)   1,527 (25)           |
  |    Secondary                  3,530 (40)   2,354 (39)           |
  |    Tertiary                   3,071 (34)   2,185 (36)           |
  |-----------------------------------------------------------------|
  | Disposable income quintile                              0.024   |
  |    1                          1,778 (20)   1,175 (19)           |
  |    2                          1,783 (20)   1,249 (21)           |
  |    3                          1,769 (20)   1,228 (20)           |
  |    4                          1,786 (20)   1,209 (20)           |
  |    5                          1,818 (20)   1,205 (20)           |
  |-----------------------------------------------------------------|
  | Born outside Sweden           1,362 (15)   897 (15)     0.013   |
  |-----------------------------------------------------------------|
  | Diabetes                      4,107 (46)   2,818 (46)   0.010   |
  |-----------------------------------------------------------------|
  | Hypertension                  4,112 (46)   2,935 (48)   0.047   |
  +-----------------------------------------------------------------+
```

```r
desctab(analysis, vars = c(education = "cat", cv_event = "bin"))
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

```r
regtab(ps_model, coef = "OR", nointercept = TRUE)
```

```
  +-------------------------------------------------------------------+
  |                                    Model                          |
  |                                       OR         95% CI   p-value |
  |  Age at cohort entry (years)        1.00   (1.00, 1.00)      0.27 |
  |                   Female sex        0.99   (0.93, 1.06)      0.84 |
  |              Education level                                      |
  |                      Primary   Reference                          |
  |                    Secondary        1.02   (0.94, 1.11)      0.66 |
  |                     Tertiary        1.09   (1.00, 1.18)     0.053 |
  |                     Diabetes        1.01   (0.94, 1.08)      0.78 |
  |                 Hypertension        1.10   (1.03, 1.17)     0.005 |
  |             Anxiety disorder        1.00   (0.93, 1.08)      0.96 |
  | Prior cardiovascular disease        0.98   (0.92, 1.05)      0.63 |
  +-------------------------------------------------------------------+

```

```r
regtab(ps_model, coef = "OR", nointercept = TRUE, compact = TRUE)
```

```
  +------------------------------------------------------------+
  |                                            Model           |
  |                                        OR 95% CI   p-value |
  |  Age at cohort entry (years)   1.00 (1.00, 1.00)      0.27 |
  |                   Female sex   0.99 (0.93, 1.06)      0.84 |
  |              Education level                               |
  |                      Primary           Reference           |
  |                    Secondary   1.02 (0.94, 1.11)      0.66 |
  |                     Tertiary   1.09 (1.00, 1.18)     0.053 |
  |                     Diabetes   1.01 (0.94, 1.08)      0.78 |
  |                 Hypertension   1.10 (1.03, 1.17)     0.005 |
  |             Anxiety disorder   1.00 (0.93, 1.08)      0.96 |
  | Prior cardiovascular disease   0.98 (0.92, 1.05)      0.63 |
  +------------------------------------------------------------+

```

```r
regtab(ps_model, coef = "OR", nointercept = TRUE, nopvalue = TRUE)
```

```
  +---------------------------------------------------------+
  |                                    Model                |
  |                                       OR         95% CI |
  |  Age at cohort entry (years)        1.00   (1.00, 1.00) |
  |                   Female sex        0.99   (0.93, 1.06) |
  |              Education level                            |
  |                      Primary   Reference                |
  |                    Secondary        1.02   (0.94, 1.11) |
  |                     Tertiary        1.09   (1.00, 1.18) |
  |                     Diabetes        1.01   (0.94, 1.08) |
  |                 Hypertension        1.10   (1.03, 1.17) |
  |             Anxiety disorder        1.00   (0.93, 1.08) |
  | Prior cardiovascular disease        0.98   (0.92, 1.05) |
  +---------------------------------------------------------+

```

```r
regtab(mlogit_model, stats = c("n", "ll", "aic", "bic", "r2"))
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

```r
regtab(zip_model, zinb_model, stats = c("n", "aic", "bic", "ll"), models = c("ZIP", 
    "ZINB"))
```

```
  +----------------------------------------------------------------------------------------------------------------------+
  |                                                 ZIP                                  ZINB                            |
  |                                               Coef.           95% CI   p-value      Coef.           95% CI   p-value |
  |                   Event count: Treatment      -0.15   (-0.26, -0.05)     0.003      -0.14   (-0.26, -0.03)     0.016 |
  |                 Event count: Age z-score       0.39     (0.34, 0.44)    <0.001       0.39     (0.33, 0.45)    <0.001 |
  |                      Event count: Female       0.30     (0.19, 0.41)    <0.001       0.32     (0.19, 0.45)    <0.001 |
  |               Inflation equation: Female       0.50     (0.15, 0.84)     0.005       0.72     (0.26, 1.19)     0.002 |
  | Inflation equation: Structural-zero risk       0.85     (0.66, 1.04)    <0.001       1.07     (0.79, 1.35)    <0.001 |
  |                             Observations      1,500                                 1,500                            |
  |                                      AIC    4338.98                               4307.53                            |
  |                                      BIC    4376.17                               4350.04                            |
  |                           Log-likelihood   -2162.49                              -2145.76                            |
  +----------------------------------------------------------------------------------------------------------------------+

```

```r
table1_tc(sc_primary, by = "group", vars = c(category = "cat"), total = "after", 
    smallcells = 5)
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
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
desctab(sc_primary, by = "group", vars = c(category = "cat"), total = "after", 
    smallcells = 5)
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
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
table1_tc(sc_complement, by = "group", vars = c(category = "cat"), total = "after", 
    smallcells = 5)
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
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
desctab(sc_complement, by = "group", vars = c(category = "cat"), total = "after", 
    smallcells = 5)
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
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
table1_tc(sc_binary, by = "group", vars = c(rare_ae = "bin"), total = "after", 
    smallcells = 5)
```

```
  +-----------------------------------------------------------------+
  |                      Control   Treatment   Total     p-value    |
  |-----------------------------------------------------------------|
  | No. (Column %)       N=50      N=50        N=100                |
  |-----------------------------------------------------------------|
  | Rare adverse event   <5        <5          5         Suppressed |
  +-----------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
desctab(sc_binary, by = "group", vars = c(rare_ae = "bin"), total = "after", 
    smallcells = 5)
```

```
  +-----------------------------------------------------------------+
  |                      Control   Treatment   Total     p-value    |
  |-----------------------------------------------------------------|
  | No. (Column %)       N=50      N=50        N=100                |
  |-----------------------------------------------------------------|
  | Rare adverse event   <5        <5          5         Suppressed |
  +-----------------------------------------------------------------+
Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.
```

```r
puttab(block_primary, xlsx = pipe_xlsx, sheet = "Block Primary", title = "Source block: Primary HRT exposure model", 
    varlabels = TRUE)
```

```
puttab: wrote 3 data rows x 3 cols (data source) to sheet Block Primary in /home/tpcopeland/R-tabtools/qa/demo/_pipeline_parts.xlsx
```

```r
puttab(block_dose, xlsx = pipe_xlsx, sheet = "Block Dose", title = "Source block: Estrogen dose-response model", 
    varlabels = TRUE)
```

```
puttab: wrote 2 data rows x 3 cols (data source) to sheet Block Dose in /home/tpcopeland/R-tabtools/qa/demo/_pipeline_parts.xlsx
```

```r
stacktab(hrt_blocks, xlsx = pipe_xlsx, sheet = "Composite", columnmerge = "B+C as \"aHR (95% CI)\"", 
    display = TRUE, title = "Hormone therapy and recurrent events", note = "aHR = adjusted hazard ratio; CI = confidence interval.")
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

```r
print(table1_tc(analysis, by = "treated", vars = c(index_age = "contn %5.1f", 
    female = "bin", diabetes = "bin", hypertension = "bin"), title = "Table 1. Baseline Characteristics", 
    markdown = markdown_report))
```

```
  +-----------------------------------------------------------------+
  |                               SSRI         SNRI         p-value |
  |-----------------------------------------------------------------|
  | No. (Column %) or Mean±SD     N=8,934      N=6,066              |
  |-----------------------------------------------------------------|
  | Age at cohort entry (years)   58.3±13.4    58.5±13.3    0.24    |
  |-----------------------------------------------------------------|
  | Female sex                    5,351 (60)   3,621 (60)   0.80    |
  |-----------------------------------------------------------------|
  | Diabetes                      4,107 (46)   2,818 (46)   0.56    |
  |-----------------------------------------------------------------|
  | Hypertension                  4,112 (46)   2,935 (48)   0.005   |
  +-----------------------------------------------------------------+
```

```r
puttab(analysis, vars = c("id", "index_age", "treated", "female", "cv_event"), 
    subset = 1:6, varlabels = TRUE, title = "Table 4. First Six Analysis Records", 
    markdown = markdown_report, mdappend = TRUE)
```

```
Markdown exported to /home/tpcopeland/R-tabtools/qa/demo/demo_markdown_report.md
```

```r
writeLines(paste("Markdown report written to", markdown_report))
```

```
Markdown report written to /home/tpcopeland/R-tabtools/qa/demo/demo_markdown_report.md
```

```r
writeLines("The file contains multiple GitHub-Flavored Markdown tables appended in one report.")
```

```
The file contains multiple GitHub-Flavored Markdown tables appended in one report.
```

```r
writeLines(readLines(markdown_report, encoding = "UTF-8"))
```

```
### Table 1. Baseline Characteristics

| No. (Column %) or Mean±SD | SSRI (N=8,934) | SNRI (N=6,066) | p-value |
| --- | --- | --- | --- |
| Age at cohort entry (years) | 58.3±13.4 | 58.5±13.3 | 0.24 |
| Female sex | 5,351 (60) | 3,621 (60) | 0.80 |
| Diabetes | 4,107 (46) | 2,818 (46) | 0.56 |
| Hypertension | 4,112 (46) | 2,935 (48) | 0.005 |

### Table 4. First Six Analysis Records

| Person identifier | Age at cohort entry (years) | Treatment group | Female sex | Cardiovascular event |
| --- | --- | --- | --- | --- |
| 1 | 73.75 | SSRI | Male | Yes |
| 2 | 61.80 | SSRI | Female | No |
| 3 | 46.22 | SSRI | Female | No |
| 4 | 51.62 | SNRI | Female | Yes |
| 5 | 54.89 | SSRI | Female | Yes |
| 6 | 75.15 | SSRI | Male | No |
```

