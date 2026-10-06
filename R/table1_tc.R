#' Baseline characteristics table ("Table 1")
#'
#' R implementation of the Stata `table1_tc` command. Summarises variables,
#' optionally by group, with one row per continuous or binary variable and a
#' label row plus indented level rows for each categorical variable.
#'
#' The result is a `tt_table`, which prints as a console table. The `xlsx`,
#' `csv`, and `markdown` arguments also write it to a file, and
#' [tt_write_xlsx()], [flextable::as_flextable()] or [tt_as_gt()] can write or
#' convert it later. Argument names mirror the Stata options, so a Stata call
#' translates option for option.
#'
#' Descriptive statistics, cell text, row structure, headers, and styling
#' reproduce Stata tabtools 2.1.14 exactly. Hypothesis tests use R's
#' conventional tests instead of Stata's: Welch's t-test (two groups) or
#' Welch's one-way ANOVA (more) for `contn` and `contln` (on the log scale),
#' the Wilcoxon rank-sum or Kruskal-Wallis test for `conts`, Pearson's
#' chi-squared test without continuity correction for `cat` and `bin`, and
#' Fisher's exact test for `cate` and `bine`. `test_args` passes arguments to
#' those functions, e.g. `list(t.test = list(var.equal = TRUE))` for Stata's
#' pooled-variance t-test. Fisher's exact test starts from R's default
#' workspace and enlarges it up to `2e7`; when the exact test still cannot
#' run (large tables), the p-value is simulated from 100,000 Monte Carlo
#' replicates under a fixed internal seed, so the result is reproducible and
#' the session's random-number stream is untouched. The same p-value is
#' then given in every session; it still carries Monte Carlo error (a 95%
#' margin of about 0.0014 at p = 0.05). Such rows read
#' `Fisher's exact (simulated)` in the Test column, the methods paragraph
#' names the Monte Carlo p-value, and `stored$fisher_simulated` lists the
#' variables.
#'
#' As in Stata, an empty string is a missing value (in `by` and in
#' categorical variables alike), and a factor level `""` is dropped. Date and
#' date-time columns are refused: Stata counts days from 1960 and R from
#' 1970, so their summaries could not match; convert them first. So are
#' complex columns, whose imaginary parts would otherwise be discarded.
#' Also refused are
#' `Inf`, `-Inf`, `NaN`, and values of 2^1023 (about 8.99e307) or more,
#' which a Stata variable cannot hold; use `NA` for a missing value. Very
#' large values give Stata's cells: a mean in Stata's e-notation
#' (`2.e+200`), a missing SD (`.`) when the squares overflow, and a blank
#' cell, p-value, or SMD when a sum overflows.
#'
#' As in Stata, `sheet` and `open` need `xlsx`, and `mdappend` needs
#' `markdown`. Unlike Stata, `title`, `footnote`, and `borderstyle` work
#' without a file: the table keeps them for its later destinations
#' ([flextable::as_flextable()], [tt_as_gt()], [tt_write_xlsx()],
#' [tt_write_markdown()]). Stata refuses `title()` without `excel()` or
#' `markdown()` and `borderstyle()` without `excel()` only because a Stata
#' table has nowhere else to go.
#'
#' **Weights.** `wt` names an importance (e.g. IPTW) weight: records with a
#' missing or zero weight leave the sample and a negative weight is an error.
#' Means, SDs (Stata `[aw=]`), weighted quantiles, and percentages use the
#' weight; the `N=` row counts records and an "Effective sample size" row
#' (Kish's `(sum w)^2 / sum w^2`) follows it. Weighted quartiles do not
#' depend on the scale of the weights: a weight total below 1 is multiplied
#' by a power of two first, as in Stata tabtools 2.1.14 (2.1.12's absolute tolerance of 1e-10 returned the two smallest values'
#' mean for every quartile under weights of 1e-12).
#' P-values, tests, and the methods
#' paragraph are dropped, and cells show percentages only unless `wtn`
#' (effective counts as `n (%)`) or `percent_n` is set. `wtcompare` puts the
#' crude table (`nopvalue`, no SMD) beside the weighted one, with the SMD from
#' the weighted pass. `fweight` names a frequency weight: records with a
#' missing or zero weight leave the sample, negative or non-integer weights
#' are errors, and every count, statistic, and test is on the expanded data.
#'
#' **Small cells.** `smallcells = k` protects every count block against exact
#' disclosure (a port of Stata's certified engine): counts below `k` show as
#' `<k`, complementary cells as a greater-than-or-equal sign and `k`,
#' percentages are withheld for a variable carrying a suppressed count, its
#' p-value, test, statistic, and SMD read `Suppressed`, and a standard note
#' joins the footnote. As in Stata, the engine protects the counts the table
#' shows, and also the ones that follow from them (Stata 2.1.17): a
#' categorical variable's hidden missing count is N minus its shown level
#' counts, and a binary variable's hidden negative and missing counts follow
#' from the denominator its n (%) cell releases. A hidden count below `k` is
#' protected as a primary cell, as if `missingsummary` or `slashN` printed it:
#' shown cells get a greater-than-or-equal marker where needed, percentages
#' are withheld, and the p-value is suppressed. The group and total Ns are
#' shared by every variable, so in a table of two or more variables they are
#' never withheld as complementary cells; a count that could only be
#' protected by withholding them is refused with an error of class
#' `tabtools_error_smallcells`. `smallcells` cannot be combined with
#' percent-only display, which includes `wtcompare` without `wtn` or
#' `percent_n`.
#' Count totals, artificial flow bounds, and required flows must be below
#' 2^53 for small-cell certification to preserve single-count changes;
#' larger frequency-weighted blocks are refused with `smallcells`.
#'
#' @param data A data frame.
#' @param vars Variables to summarise; `NULL` (the default) uses every column
#'   of `data`, including the `by`, `wt` and `fweight` columns, as Stata's
#'   `_all` does (desctab.ado:103-110), so you will normally pass `vars`.
#'   Columns are typed automatically (names are used as they are, spaces
#'   included).
#'   Either a character vector of names (types detected automatically), a
#'   named character vector or list mapping names to a type, optionally
#'   followed by a display format, such as
#'   `c(age = "contn %5.1f", sex = "bin")`, or one Stata-style string
#'   `"age contn %5.1f \\ sex bin"` (the unnamed string forms split on
#'   blanks and `\\`, so a name containing either needs the named form).
#'   Types, with the cell they show and the test they use:
#'   * `contn`: normal, mean and SD (Welch's t-test or ANOVA);
#'   * `contln`: log-normal, geometric mean and geometric SD (the same
#'     tests on the log scale);
#'   * `conts`: skewed, median (Q1, Q3) (Wilcoxon rank-sum or
#'     Kruskal-Wallis);
#'   * `cat`, `bin`: categorical (one row per level) or binary (one row),
#'     n (%) (chi-squared test); `cate`, `bine`: the same with Fisher's
#'     exact test;
#'   * `auto` (the default): strings and factors are `cat`, 0/1 is `bin`,
#'     labelled numbers and numbers with 7 or fewer distinct non-missing
#'     values in the analysis sample are `cat`, and other numbers are
#'     `contn` or `conts` by skewness and kurtosis (over 5,000 values) or
#'     by the Shapiro-Wilk test (Royston's approximation), computed as
#'     Stata's `swilk` computes it. The moments and the Shapiro-Wilk test take the
#'     values divided by a power of two, so huge or tiny values are typed
#'     by their true shape, as in Stata tabtools 2.1.14.
#' @param by Optional grouping variable name.
#' @param fweight Optional frequency-weight variable name (Stata
#'   `[fweight=]`): non-negative integers on every record, including records
#'   whose `by` value is missing (as Stata's `marksample`); zero and missing
#'   (`NA`) weights drop the record; `Inf` and `NaN` are errors. Hypothesis
#'   tests equal R's tests on the data expanded by the frequencies but are
#'   computed without expanding them (weighted tables, moments, and
#'   mid-ranks). Only an exact or Edgeworth-corrected Wilcoxon test, its
#'   `conf.int`, and paired tests (via `test_args`, or R's exact Wilcoxon for
#'   groups under 50) expand the data, up to 10 million records.
#' @param wt Optional importance/IPT weight variable name (Stata `wt()`):
#'   non-negative; zero and missing (`NA`) weights drop the record; `Inf` and
#'   `NaN` are errors. Cannot be combined with `fweight`. When a weighted
#'   sum, product or square would pass Stata's largest number (8.99e307),
#'   the weights are divided by a power of two (exact) and the statistic
#'   formed again, as Stata tabtools 2.1.12 does: means, SDs, quartiles,
#'   percentages, effective counts and the ESS do not depend on the scale of
#'   the weights, so weights of 1e200 give `ESS=10` and the true cells. The
#'   SMD comes from Stata's unscaled sums and is blank where they overflow.
#'   Weights whose largest is below 1 are first multiplied by a power of two
#'   (exact; R only), so tiny weights such as 1e-310 give the same cells as
#'   weights of ordinary scale rather than a missing SD or SMD.
#'   A weight of 2^1023 or more (Stata's missing range) is an error.
#' @param labels Optional named character vector (or list of single
#'   strings) overriding variable labels; every element needs a name and a
#'   non-missing string. By default labels come from the `"label"` attribute
#'   (haven/labelled).
#' @param total Add a Total column over all groups: `"none"` (default),
#'   `"before"` or `"after"` the group columns.
#' @param missing Treat missing values as a category for categorical variables.
#' @param test,statistic Show the test-name and test-statistic columns.
#' @param headerperc Add the percentage of the total to the sample-size row.
#' @param smd Show standardized mean differences between the first two
#'   groups (requires `by`): the difference in means over the pooled SD
#'   `sqrt((s1^2 + s2^2) / 2)` (log scale for `contln`), the difference in
#'   proportions over their pooled SD `sqrt((p1 (1 - p1) + p2 (1 - p2)) / 2)`
#'   for `bin`, and Yang and Dalton's multivariate SMD over the
#'   first K - 1 levels for `cat`, as in Stata. With more than two groups
#'   this compares only the first two levels of `by` (or the `smdpair`):
#'   the header then names the pair (`SMD (A vs B)`) and a footnote says
#'   so, so every export shows what was compared. See `smdtype` for
#'   statistics over all groups.
#' @param nopvalue Suppress the p-value column.
#' @param format,percformat,nformat Stata display formats for continuous
#'   statistics, percentages, and counts (`"%5.1f"` as in [sprintf()]; a
#'   trailing `c`, as in `"%12.0fc"`, adds thousands separators; `"%09.2f"`
#'   pads with leading zeros; `"%9.2g"` shows at most 2 significant digits,
#'   as Stata's `string()` does, so 14.795 reads `15` and 123456.8 reads
#'   `123457`; a format whose decimals are not fewer than its width, such as
#'   `"%5.5g"`, is an error). When `format` is not given, a
#'   geometric SD (`contln`) without a per-variable format is shown with
#'   `"%4.2f"`, as in Stata tabtools 2.1.10 and later; an explicit `format`
#'   (or a per-variable `fmt1`) applies to the GSD as well.
#' @param varlabplus Append the data type to variable labels.
#' @param iqrmiddle,sdleft,sdright,gsdleft,gsdright,percsign Cell delimiters.
#'   As in Stata, an empty `iqrmiddle`, `sdleft`, `gsdleft`, or `gsdright`
#'   falls back to its default in the cells and `varlabplus` labels but not
#'   in the descriptor or `Dapa`; `percsign` loses leading and trailing
#'   blanks in categorical and binary cells but keeps them in the
#'   missing-summary rows and `headerperc` (Stata's quirk).
#' @param spacelowpercent,extraspace,percent,percent_n,slashN,catrowperc
#'   Cell-content switches with the same meaning as in Stata.
#' @param pdp,highpdp Decimal places for p < 0.10 and p >= 0.10.
#' @param missingsummary Add a missing-data row per variable.
#' @param smallcells Optional integer >= 3 activating small-cell suppression.
#'   Not available with percent-only cells (`percent`, or `wt` without `wtn`
#'   or `percent_n`).
#' @param wtcompare Show crude and weighted columns side by side (requires
#'   `wt` and `by`).
#' @param wtn Show effective counts, `(sum w_cell / sum w_group) * N_group`,
#'   in weighted cells (requires `wt`).
#' @param smdthreshold SMD highlighting threshold in Excel; `-1` disables.
#' @param nosmdhighlight Logical; default `FALSE`. Set `TRUE` to leave SMD
#'   cells unhighlighted in Excel, equivalent to `smdthreshold = -1`. May
#'   not be combined with an explicitly supplied `smdthreshold` (error class
#'   `tabtools_error_smdhighlight`). Markdown does not highlight SMD cells.
#' @param smdtype What the SMD column (with `smd = TRUE`) reports:
#'   * `"pair"` (default): the two-group SMD above, header `SMD`.
#'   * `"population"`: the population standardized bias of McCaffrey et al.
#'     (2013, eq. 5), header `Pop. SB`. Each group's mean is compared with
#'     the mean of all analysed records, in units of their SD, and the
#'     largest absolute value over groups is shown. Under `wt` the group
#'     means are weighted but the overall mean and SD stay unweighted (the
#'     population the weights aim at); under `fweight` both are
#'     frequency-weighted. `bin` uses `sqrt(p (1 - p))` of the overall
#'     proportion. `cat` takes the largest value over levels, each level
#'     standardized by its overall proportion (as in the twang package).
#'   * `"maxpair"`: the largest absolute pairwise SMD over all pairs of
#'     groups (Lopez and Gutman 2017, eq. 27), header `Max SMD`. Every pair
#'     shares one denominator, the root of the mean of all groups'
#'     variances (`p (1 - p)` for `bin`; for `cat` the Yang-Dalton
#'     covariance averaged over all groups). With two groups it equals the
#'     absolute `"pair"` SMD.
#'
#'   For `"population"` and `"maxpair"` a footnote states the definition.
#'   A group with no values for a variable leaves that variable's statistic
#'   blank. `smdthreshold` highlights any of the three in Excel. As Stata tabtools
#'   2.4.0 `smdtype()`.
#' @param smdpair The two groups of `by` a `"pair"` SMD compares, instead of
#'   the first two: a length-2 character or numeric vector. With a numeric
#'   `by` a number names a `by` value; anything else is matched against the
#'   group labels (the values of a character or factor `by`, or the value
#'   labels of a labelled numeric `by`). Each must name exactly one group.
#'   The header names the pair (`SMD (C vs A)`); with more than two groups a
#'   footnote adds "chosen with smdpair()". Requires `smd = TRUE` and
#'   `smdtype = "pair"`. As Stata tabtools 2.4.0 `smdpair()`. A token that is
#'   the value of one group and the label of another (value labels that are
#'   themselves numbers) is refused under `smdpair_as = "auto"`. A label equal
#'   to its own group's value is not ambiguous. A logical `by` takes `TRUE` /
#'   `FALSE` (or the strings), read as 1 / 0.
#' @param smdpair_as `"auto"` (default), `"values"` or `"labels"`: read every
#'   `smdpair` token as a `by` value or as label text. As Stata's
#'   `smdpair(..., values|labels)`.
#' @param xlsx,sheet,title,footnote,open Excel target and annotations.
#' @param borderstyle One of `"thin"`, `"default"`, `"medium"`, `"academic"`.
#' @param font,fontsize Excel font family and size; `NULL` uses the session
#'   default ([tabtools_options()], else Arial 10). `fontsize` is a whole
#'   number from 1 to 72 for one table (Stata's `fontsize()`); a persistent
#'   default ([tabtools_options()]) must be 6 to 72.
#' @param boldp,highlight P-value thresholds for bold p cells and row fill.
#' @param zebra,headershade,headercolor,zebracolor Shading options.
#' @param csv,markdown,mdappend Additional export targets. `csv` must be a
#'   `.csv` file. Every target is checked before any is written: two
#'   arguments naming the same file and a missing or read-only directory are
#'   refused up front, so a failed call leaves no partial set of files (Stata
#'   checks none of this). An existing Markdown file is replaced unless
#'   `mdappend` is set, as in Stata tabtools 2.1.12.
#' @param test_args Optional named list of argument lists for the R test
#'   functions (`t.test`, `oneway.test`, `wilcox.test`, `kruskal.test`,
#'   `chisq.test`, `fisher.test`). Argument names are checked (the data
#'   arguments are reserved), `alternative` must be `"two.sided"`, and `mu`
#'   must be 0 (a shifted null has no Stata equivalent). A test that errors
#'   for a reason other than too little data leaves its p-value blank with a
#'   warning. `paired = TRUE` for `t.test` or `wilcox.test` (two groups; an
#'   R-only option, Stata has none) runs the paired t-test or the Wilcoxon
#'   signed-rank test, labelled `Paired t test` and `Wilcoxon signed-rank`
#'   and named so in the methods paragraph. There is no subject argument:
#'   the i-th record of the first group is paired with the i-th record of the
#'   second, in row order, so the groups must be of equal size (otherwise an
#'   error) and the data sorted by pair; a pair with a missing value is
#'   dropped. Under `fweight` both records of a pair must carry the same
#'   frequency (a pair observed f times; otherwise an error), and the test is
#'   R's paired test on the expanded pairs. `paired` needs exactly two groups
#'   and, like every switch in `test_args`, must be `TRUE` or `FALSE`.
#' @param dots Print a progress line with one dot per variable (Stata
#'   `dots`).
#' @param excel Synonym of `xlsx` (Stata's original option name); it wins
#'   when both are given.
#' @return A `tt_table` object. It is returned invisibly when `xlsx`, `csv` or
#'   `markdown` writes a file, so assign it (`tab <- table1_tc(...)`) and
#'   print it to see it. Its `stored` element (Stata's `r()` results) holds
#'   text for a methods section and the numbers behind the table:
#'   * `methods`: a methods paragraph naming the statistics and tests (with
#'     a `by` variable and p-values only); `Dapa`: its "Data are presented
#'     as ..." sentence alone;
#'   * `table`: a matrix of the p-values and SMDs per variable (`NA` where
#'     Stata stores `.d` for a suppressed value); `varlist` and `types`: the
#'     variables and their resolved types; `fisher_simulated`: variables
#'     whose Fisher p-value was simulated (with p-values);
#'   * with `smd = TRUE`: `smdtype`, and `smdnote`, the note naming what the
#'     SMD column compares (when there is one; it is also in the footnote);
#'   * for the files written: `xlsx`, `sheet`, `markdown`, `markdown_rows`,
#'     `markdown_cols`;
#'   * with `smallcells`: `smallcells`, `N_primary_suppressed`,
#'     `N_secondary_suppressed`, `N_derived_suppressed`, and `suppression`,
#'     a matrix over every table row (both header rows included) by group
#'     column and `pvalue`/`test`/`statistic`/`smd_str`, coded 0 visible,
#'     1 primary, 2 complementary, 3 derived.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @references
#' Yang D, Dalton JE (2012). A unified approach to measuring the effect size
#' between two groups using SAS. *SAS Global Forum 2012*, paper 335-2012.
#' (The standardized mean differences.)
#'
#' McCaffrey DF, Griffin BA, Almirall D, Slaughter ME, Ramchand R, Burgette
#' LF (2013). A tutorial on propensity score estimation for multiple
#' treatments using generalized boosted models. *Statistics in Medicine*
#' 32(19), 3388-3414. \doi{10.1002/sim.5753} (`smdtype = "population"`.)
#'
#' Lopez MJ, Gutman R (2017). Estimation of causal effects with multiple
#' treatments: a review and new ideas. *Statistical Science* 32(3),
#' 432-454. \doi{10.1214/17-STS612} (`smdtype = "maxpair"`.)
#'
#' Austin PC (2009). Balance diagnostics for comparing the distribution of
#' baseline covariates between treatment groups in propensity-score matched
#' samples. *Statistics in Medicine* 28(25), 3083-3107.
#' \doi{10.1002/sim.3697} (The 0.1 `smdthreshold` convention.)
#'
#' Kish L (1965). *Survey Sampling*. New York: Wiley. (The effective sample
#' size under `wt`.)
#'
#' Shapiro SS, Wilk MB (1965). An analysis of variance test for normality
#' (complete samples). *Biometrika* 52(3/4), 591-611.
#' \doi{10.2307/2333709}
#'
#' Royston P (1992). Approximating the Shapiro-Wilk W-test for
#' non-normality. *Statistics and Computing* 2(3), 117-119.
#' \doi{10.1007/BF01891203} (The Shapiro-Wilk test used by `auto` typing.)
#'
#' Royston P (1995). Remark AS R94: a remark on algorithm AS 181: the
#' W-test for normality. *Applied Statistics* 44(4), 547-551.
#' \doi{10.2307/2986146}
#' @examples
#' d <- data.frame(arm = rep(c("A", "B"), each = 25),
#'                 age = c(seq(40, 64), seq(45, 69)),
#'                 sex = factor(rep(c("F", "M", "F", "F", "M"), 10)),
#'                 smoker = rep(c(0, 1), 25))
#' attr(d$age, "label") <- "Age (years)"
#' table1_tc(d, by = "arm", vars = c(age = "contn %5.1f", sex = "cat", smoker = "bin"))
#'
#' # With SMDs, written to an Excel sheet: the table is then returned
#' # invisibly, so assign it and print it
#' xlsx <- tempfile(fileext = ".xlsx")
#' tab <- table1_tc(d, by = "arm", vars = c(age = "conts", sex = "cat"), smd = TRUE,
#'                  xlsx = xlsx, sheet = "Table 1")
#' tab
#' # A methods paragraph for the paper
#' tab$stored$methods
#'
#' # Three groups: balance over all of them, not just the first two
#' d3 <- data.frame(arm = rep(c("A", "B", "C"), each = 20),
#'                  age = c(seq(40, 59), seq(41, 60), seq(55, 74)))
#' table1_tc(d3, by = "arm", vars = c(age = "contn"), smd = TRUE, smdtype = "population")
#'
#' # desctab() is the same command
#' desctab(d, by = "arm", vars = c("age", "sex"))
#' @order 1
#' @export
table1_tc <- function(data, vars = NULL, by = NULL, fweight = NULL, wt = NULL,
                      labels = NULL,
                      total = c("none", "before", "after"),
                      missing = FALSE, test = FALSE, statistic = FALSE,
                      headerperc = FALSE, smd = FALSE, nopvalue = FALSE,
                      format = "%2.0f", percformat = "%5.0f",
                      nformat = "%12.0fc", varlabplus = FALSE,
                      iqrmiddle = ", ", sdleft = "\u00b1", sdright = "",
                      gsdleft = " (\u00d7/", gsdright = ")", percsign = "",
                      spacelowpercent = FALSE, extraspace = FALSE,
                      percent = FALSE, percent_n = FALSE, slashN = FALSE,
                      catrowperc = FALSE, pdp = 3, highpdp = 2,
                      missingsummary = FALSE, smallcells = NULL,
                      wtcompare = FALSE, wtn = FALSE, smdthreshold = 0.1,
                      smdtype = c("pair", "population", "maxpair"), smdpair = NULL,
                      smdpair_as = c("auto", "values", "labels"),
                      xlsx = NULL, sheet = "Table 1", title = NULL,
                      footnote = NULL, open = FALSE, borderstyle = NULL,
                      font = NULL, fontsize = NULL, boldp = NULL,
                      highlight = NULL, zebra = FALSE, headershade = FALSE,
                      headercolor = NULL, zebracolor = NULL,
                      csv = NULL, markdown = NULL, mdappend = FALSE,
                      test_args = NULL, dots = FALSE, excel = NULL, nosmdhighlight = FALSE) {
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Table 1"
  total <- match.arg(total)
  if (!is.logical(nosmdhighlight) || length(nosmdhighlight) != 1L || is.na(nosmdhighlight)) {
    cli::cli_abort("{.arg nosmdhighlight} must be TRUE or FALSE.",
                   class = "tabtools_error_smdhighlight", call = NULL)
  }
  # desctab.ado:389-405 (2.5.1): explicit threshold and switch conflict,
  # including an explicit -1; an omitted threshold is changed to -1.
  if (nosmdhighlight) {
    if (!base::missing(smdthreshold)) {
      cli::cli_abort("{.arg nosmdhighlight} and {.arg smdthreshold} may not be combined.",
                     class = "tabtools_error_smdhighlight", call = NULL)
    }
    smdthreshold <- -1
  }
  smdpair_as <- tryCatch(rlang::arg_match(smdpair_as, c("auto", "values", "labels")),
    error = function(e) cli::cli_abort("{.arg smdpair_as} must be one of {.val auto}, {.val values} or {.val labels}.",
                                       class = "tabtools_error_smdpair_as", call = NULL))
  if (smdpair_as != "auto" && is.null(smdpair)) {
    cli::cli_abort("{.arg smdpair_as} requires {.arg smdpair}.", class = "tabtools_error_smdpair_as", call = NULL)
  }
  smdtype_given <- !base::missing(smdtype)
  smdtype <- match.arg(smdtype)
  for (a in c("missing", "test", "statistic", "headerperc", "smd", "nopvalue", "varlabplus",
              "spacelowpercent", "extraspace", "percent", "percent_n", "slashN", "catrowperc",
              "missingsummary", "wtcompare", "wtn", "open", "zebra", "headershade", "mdappend", "dots")) {
    x <- get(a)
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
    }
  }
  for (a in c("iqrmiddle", "sdleft", "sdright", "gsdleft", "gsdright", "percsign")) {
    x <- get(a)
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      cli::cli_abort("{.arg {a}} must be a single string.", call = NULL)
    }
  }
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.", call = NULL)
  labels <- .check_label_overrides(labels)
  # desctab.ado:88: excel() is the original name, xlsx() its synonym; excel
  # wins when both are given.
  if (!is.null(excel)) xlsx <- excel
  for (a in c("title", "footnote")) .tt_check_text_arg(get(a), a)
  # desctab.ado:167: title("") is no title.
  if (!is.null(title) && is.character(title) && length(title) == 1L && !is.na(title) && !nzchar(title)) {
    title <- NULL
  }
  # desctab.ado:97-107: an omitted varlist is Stata's `_all`, every variable
  # auto-typed (qa/test_table1_tc.do "runs without vars()"). The names are
  # taken as they are (no parsing), so they may contain spaces.
  specs <- if (is.null(vars)) {
    nms <- names(data)
    if (!length(nms)) cli::cli_abort("{.arg vars} did not contain any variables.", call = NULL)
    # A column named "" or NA cannot be selected by name (audit A10); a
    # Stata variable always has a name.
    if (anyNA(nms) || !all(nzchar(nms))) {
      cli::cli_abort(c("{.arg data} has a column with an empty or missing name.",
                       "i" = "Name every column (e.g. with {.fn make.names}) or choose the columns with {.arg vars}."),
                     call = NULL)
    }
    lapply(nms, function(nm) list(name = nm, type = "auto", fmt1 = NA_character_, fmt2 = NA_character_))
  } else {
    .t1_parse_vars(vars)
  }

  # Duplicate column names make data[[name]] silently take the first one
  # (H13, F16); a Stata dataset cannot have them.
  used_all <- unique(c(by, wt, fweight, vapply(specs, `[[`, "", "name")))
  .t1_check_duplicate_names(data, used_all)
  # Matrix and data-frame columns have no Stata equivalent (audit A04).
  data <- .t1_check_shape(data, intersect(used_all, names(data)))

  # desctab.ado:115-138 and :140-333: option validation, in Stata's order.
  # Stata refuses a threshold beyond its integer range too ("option
  # smallcells() invalid" for 3000000000, probed; H10).
  if (!is.null(smallcells) && (!is.numeric(smallcells) || length(smallcells) != 1L ||
                               !is.finite(smallcells) || smallcells != round(smallcells) || smallcells < 3 ||
                               smallcells > .Machine$integer.max)) {
    cli::cli_abort("{.arg smallcells} must be an integer greater than or equal to 3.", call = NULL)
  }
  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1L) {
      cli::cli_abort("{.arg by} must be a single variable name.", call = NULL)
    }
    if (!by %in% names(data)) cli::cli_abort("{.arg by} variable {.var {by}} not found.", call = NULL)
  }
  has_xlsx <- !is.null(xlsx)
  has_md <- !is.null(markdown)
  if (has_xlsx || sheet_given) sheet <- .check_sheet(sheet)
  .tt_check_sheet_xlsx(sheet_given, has_xlsx)
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must specify a .xlsx file.", call = NULL)
  }
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  # Every target is checked together before anything is written (H8).
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  pdp <- .check_dp(pdp, "pdp")
  highpdp <- .check_dp(highpdp, "highpdp")
  if (!is.null(borderstyle)) .check_borderstyle(borderstyle)
  if (!is.null(wt) && !is.null(fweight)) {
    cli::cli_abort("{.arg wt} and {.arg fweight} cannot be used together.", call = NULL)
  }
  if (wtn && is.null(wt)) cli::cli_abort("{.arg wtn} requires {.arg wt}.", call = NULL)
  if (wtn && percent) {
    cli::cli_abort("{.arg wtn} and {.arg percent} are incompatible (percent suppresses all counts).", call = NULL)
  }
  # desctab.ado:282-293: weighted tables default to percent-only, which
  # smallcells() cannot protect.
  # tabtools 2.1.17 (desctab.ado:~300): wtcompare is exempt from that default,
  # but its weighted columns are percent-only without wtn/percent_n, so it is
  # refused on the same terms.
  implicit_percent <- percent || (!is.null(wt) && !(percent_n || wtn))
  if (!is.null(smallcells) && implicit_percent) {
    cli::cli_abort(c("{.arg smallcells} cannot be combined with percent-only display.",
                     "i" = "Use percent_n, wtn, or the default n (%); percentages are withheld for any variable that carries a suppressed count."),
                   class = "tabtools_error_smallcells_percent_only", call = NULL)
  }
  if (smd && is.null(by)) cli::cli_abort("{.arg smd} requires {.arg by}.", call = NULL)
  if (smdtype_given && !smd) {
    cli::cli_abort("{.arg smdtype} requires {.code smd = TRUE}.", call = NULL)
  }
  if (!is.null(smdpair) && !smd) cli::cli_abort("{.arg smdpair} requires {.code smd = TRUE}.", call = NULL)
  if (!is.null(smdpair) && smdtype != "pair") {
    cli::cli_abort("{.arg smdpair} requires {.code smdtype = \"pair\"}: population and maxpair use every group.",
                   call = NULL)
  }
  if (wtcompare && is.null(wt)) cli::cli_abort("{.arg wtcompare} requires {.arg wt}.", call = NULL)
  if (wtcompare && is.null(by)) cli::cli_abort("{.arg wtcompare} requires {.arg by}.", call = NULL)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra, headercolor = headercolor,
                            zebracolor = zebracolor, boldp = boldp, highlight = highlight,
                            smdthreshold = smdthreshold)
  for (a in c("format", "percformat", "nformat")) {
    f <- get(a)
    if (!is.character(f) || length(f) != 1L || is.na(f)) {
      cli::cli_abort("{.arg {a}} must be one Stata display format, such as {.val %5.1f}.", call = NULL)
    }
    .parse_stata_fmt(f)
  }
  .t1_check_test_args(test_args)
  if (!is.null(smallcells)) smallcells <- as.integer(smallcells)

  # Stata's empty string is missing (Phase 2 review P0-2); before the weight
  # checks, which look at by() missingness.
  used_cols <- intersect(unique(c(by, vapply(specs, `[[`, "", "name"))), names(data))
  data[used_cols] <- lapply(data[used_cols], .t1_blank_to_na)
  # Non-finite values are refused on every record of every analysed column,
  # by() included, before weights drop records (review R11).
  for (nm in used_cols) .t1_check_finite(data[[nm]], nm)
  # desctab.ado:527-536: progress dots, one per variable.
  if (dots) {
    cli::cli_inform("Processing {length(specs)} variable(s): {strrep('.', length(specs))}")
  }
  # Stata encodes a string by() over the whole dataset (desctab.ado:435), so
  # group codes count levels on records the weights drop too.
  by_full <- if (!is.null(by)) data[[by]]
  # Weights (desctab.ado:247-293, :397-409): wt() and fweight drop records
  # with missing or zero weights before anything else sees the data.
  # Paired tests under fweight: pairs are the records in row order, so both
  # members of a pair must carry the same frequency (checked before zero
  # frequencies drop records; review R3).
  .t1_check_fw_pairs(data, by, fweight, test_args)
  wprep <- .t1_weight_prep(data, wt, fweight)
  # Keep input-scale evidence before weighting subsets rows or rescales
  # tiny importance weights. Only aggregates leave this calculation.
  sample_input <- list(
    n = nrow(data),
    keep = if (is.null(wprep)) rep(TRUE, nrow(data)) else wprep$keep,
    group = if (is.null(by)) NULL else .t1_values(data[[by]]),
    weights = if (is.null(wprep)) rep(1, nrow(data)) else
      as.numeric(.t1_values(data[[if (is.null(wt)) fweight else wt]])),
    kind = if (is.null(wprep)) "none" else wprep$kind
  )
  if (!is.null(wprep)) data <- .t1_keep_rows(data, wprep$keep)
  gp <- .t1_groups(data, by, total != "none", labels, by_full = by_full)
  touse <- !is.na(gp$gid)
  smd_pair <- if (is.null(smdpair)) NULL else
    .t1_resolve_smdpair(smdpair, gp, by_numeric = !(is.character(data[[by]]) || is.factor(data[[by]])),
                        as = smdpair_as, by_logical = is.logical(data[[by]]))
  specs <- lapply(specs, .t1_resolve_var, data = data, touse = touse,
                  include_missing = missing, labels = labels)
  o <- list(total = total, missing = missing, test = test, statistic = statistic,
            headerperc = headerperc, smd = smd, smdtype = smdtype, smdpair = smd_pair,
            nopvalue = nopvalue, format = format,
            # desctab.ado:350-354 (2.1.10+): without format() a geometric SD
            # gets two decimals (%4.2f), not the %2.0f that prints "x/1".
            gsdformat = if (base::missing(format)) "%4.2f" else NULL,
            percformat = percformat, nformat = nformat, varlabplus = varlabplus,
            # _desctab_collect.ado:94-99 resets an empty delimiter to its
            # default for the cells and varlabplus labels; desctab's
            # descriptor and Dapa keep the raw text (o$desc).
            iqrmiddle = if (nzchar(iqrmiddle)) iqrmiddle else ", ",
            sdleft = if (nzchar(sdleft)) sdleft else "\u00b1", sdright = sdright,
            gsdleft = if (nzchar(gsdleft)) gsdleft else " (\u00d7/",
            gsdright = if (nzchar(gsdright)) gsdright else ")",
            desc = list(iqrmiddle = iqrmiddle, sdleft = sdleft, sdright = sdright, gsdleft = gsdleft,
                        gsdright = gsdright),
            # desctab.ado:352 strips quotes; _desctab_collect's percsign(string)
            # then trims blanks (:40), so cat/bin cells get the trimmed sign
            # while the missing-summary rows and headerperc keep desctab's
            # copy (desctab.ado:749, :768, :1176).
            percsign = trimws(gsub("\"", "", percsign, fixed = TRUE), whitespace = "[ ]"),
            percsign_raw = gsub("\"", "", percsign, fixed = TRUE),
            spacelowpercent = spacelowpercent,
            extraspace = extraspace, percent = percent, percent_n = percent_n, slashN = slashN,
            catrowperc = catrowperc, pdp = pdp, highpdp = highpdp,
            missingsummary = missingsummary, test_args = test_args, smallcells = smallcells,
            # _desctab_collect.ado (2.1.17): shared group/total Ns cannot be withheld
            # per variable once the table has two or more variables.
            sc_nvars = length(specs),
            sc_names = vapply(specs, function(x) as.character(x$name), ""),
            sc_labels = vapply(specs, function(x) as.character(x$label), ""))
  # desctab.ado:132-138: the small-cell note joins any user footnote.
  sc_note <- if (!is.null(smallcells)) tt_sc_footnote(smallcells) else NULL
  if (!is.null(sc_note) && !grepl(sc_note, footnote %||% "", fixed = TRUE)) {
    footnote <- .t1_join_note(footnote, sc_note)
  }
  # The SMD column's comparison travels with every export as a footnote
  # (desctab.ado, tabtools 2.4.0: joined with a backslash when the footnote
  # already uses that line separator, and not repeated).
  smd_note <- if (smd) .t1_smd_note(smdtype, gp, smd_pair) else NULL
  if (!is.null(smd_note) && !grepl(smd_note, footnote %||% "", fixed = TRUE)) {
    footnote <- .t1_join_note(footnote, smd_note)
  }
  tt <- .t1_run_passes(data, specs, gp, o, style, title, footnote, sheet, labels,
                       wprep = wprep, wtcompare = wtcompare, show_wtn = percent_n || wtn)
  # r(smdtype) whenever smd is on; r(smdnote) when there is a note.
  if (smd) {
    tt$stored$smdtype <- smdtype
    if (!is.null(smd_note)) tt$stored$smdnote <- smd_note
  }
  tt$meta$sample_accounting <- .t1_sample_accounting(sample_input, specs, gp, o, wtcompare)
  if (!is.null(smallcells)) tt <- .t1_sc_mask_ledger(tt, gp, length(specs))
  if (!is.null(smallcells)) {
    sc <- .t1_sc_stored(tt, gp, by, smallcells, total = total != "none" && gp$G > 1L,
                        wtcompare = wtcompare)
    tt$stored[names(sc)] <- sc
    tt$notes <- sc_note
  }
  tt <- .t1_console_widths(tt, gp, by, wtcompare)
  tt$meta[c("row_codes", "sample_codes", "derived_rows", "crude_codes", "crude_cols",
            "weighted_cols", "crude_sample_codes")] <- NULL

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
