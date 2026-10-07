# Crosstab paired-native publication contract

Execution uses the approved installed testthat driver, which stages and loads
`qa/helper-crosstab-qa.R`. Run both authored QA files from that driver, with
`CROSSTAB_NATIVE_DIR` pointing to root's authenticated `native-xt-v2` bundle.
Plain `Rscript qa/crossval_crosstab.R` is not a supported standalone runner.
Root must first integrate and review the shared crosstab sink/style/flat/compose
hooks and the exact reviewed WP3G integrity helpers. No native producer runs
inside these tests. Missing native artifacts are errors, never skips.

The immutable 162-file manifest is bound by SHA256
`7d1c33158c762e1df39d0a43421ae52285387c556b48055cb04dc0c7244a32ee`.
Every listed byte hash, the fixed 2.5.1 source pin, Stata17/mt64 receipt and all28
per-case identity/status records are checked before parity comparisons.
The root retains the full90-source closure and operational cleanup receipt.
Native records preserve literal ordinary `.` versus tagged `.a` axis identity;
protected count `.p/.s` and derived `.d` markers are checked before decoding.
Matrices require every Cartesian coordinate exactly once. Scalars and
association captures require every requested family exactly once.

Twenty-three successful cases compare complete CSV and Markdown bodies/bytes,
console listings, workbook sheet identity, body values, cell styles, widths,
merge geometry and heights. XT027 exercises genuine exact/shade/zebra output.
Five genuine refusals are matched: XT009/XT015/XT016 rc498, XT019/XT028 rc198.
XT015's N2 Spearman command fails in the native wrapper before any sink; R
refuses the same unavailable inference. N>2 positive-perfect inference remains
p0. XT018 is a successful rc0,2x2,N100 table with literal cells40,10,20,30 and
axes1,2 after its zero-weight-only999 level: it takes the ordinary full path.
This literal binds the observed case without claiming broader native domains.

Only the following explicitly different automatic paragraph boundary is
adapted. Full paragraphs and every blank companion CSV/workbook cell are
asserted independently before any boundary row is removed. Text artifacts
retain LF endings and a final newline; exact CSV footer quoting and Markdown
paragraph spacing are asserted. No numeric/body/style mask is used.

Native strict (pinned crosstab.ado125-127, authenticated XT020/XT022):

> Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction.

R strict, with the command's actual numerator/selected-margin dependency:

> Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction. Percentages are withheld when their count or selected denominator is suppressed. Tests and association estimates are withheld when primary counts are protected.

Native primary (authenticated XT021/XT023/XT024):

> Counts from 1 to 2 are shown as <3 without a percentage (primary suppression only: no complementary cells are masked, and totals and tests are shown as computed). This protects printed counts only.

R primary, with the explicit reconstruction warning:

> Counts from 1 to 2 are shown as <3. Dependent percentages are withheld. Primary protection masks no complementary counts; released totals and computed tests or association estimates may permit reconstruction.

Method prose truthfully identifies R and native computational engines separately;
source provenance is never passed off as an R result. RR0 retains its finite
point0 and both ordinary-missing unavailable log-Wald limits, following the held
installed `_crcrr.ado`18-20 formula. Dense RR/RD variances, sample OR versus
conditional limits, signed/rescaled CA and expanded-rank Spearman controls
remain independent. No original scenario input or native expected output is
changed by this source correction.
