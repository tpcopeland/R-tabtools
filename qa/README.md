# tabtools QA

This directory is `.Rbuildignore`d. It holds the QA runner and benchmarks
(below) and the Stata parity harness. The goldens the harness writes live in
`tests/testthat/golden/`, so tests and CI never need Stata.

## Transitional Phase 2 golden baselines

The default golden directory retains authentic Stata tabtools 2.1.14 / fvgen
1.2.5 output. Until WP-3G promotes the full 2.5.1 baseline, the golden readers
route P01-P15, K01-K09, S01-S08, W15 and W17-W22 to
`tests/testthat/golden/phase2-2.5.1/`. These 39 scenarios cover WP-2C's boxed
puttab borders (including every K workbook's setup sheets) and WP-2E's rate
precision, formatting and counters. Other scenarios and input datasets keep
the legacy baseline.

The selected artifacts are byte-for-byte copies from native inventory commit
`c215afdb788f88363c8d5289d7d13e3a31cfbe38` (Stata tabtools 2.5.1 / fvgen
1.2.7). `ROUTES.csv` records the scope and rationale; `ARTIFACTS.csv` records
each source path, source commit and MD5. Shared workbooks are copied whole,
with routing selected by scenario, and K comparisons retain all setup sheets.
The harness asserts each scenario's versions and copied artifact hashes,
then uses the existing strict cell, console, sink and style comparators.
P02/P09 remain source-only in both baseline directories.

The complete inventory branch stays unmerged until WP-3G. The transitional
router and versioned copies must be removed when that full baseline lands.
R's automatic footnote paragraphs across all sinks are a deliberate publication
contract; they require separate independent literal footer assertions before
strict comparisons of authentic native table content and styles. Routing alone
does not establish this stronger R contract.

`helper-golden-footnotes.R` adapts only declared footer regions: R paragraphs
come from literal scenario inputs, authenticated native automatic notes and
reference constants. It asserts complete R/native text, counts and boundaries
for cells, console, CSV, Markdown and workbooks, including every footer cell's
style, merges and explicit heights. All remaining native cells, body styles,
console lines and sink bytes still use the original comparators and tolerances.
Fault-injection tests add earlier/interleaved/trailing paragraphs and alter
footer/spacer styling or body content. Converter and hand-built renderer checks
use the same independent paragraph contract. The separately asserted legacy
SMD note/header translation remains until WP-3G.

The standard T/R/E/C/S workbook layouts serialize only each merged note's
anchor in R. The authentic native workbook also serializes blank children
with its default Calibri 11 format, General number format and no visible
styling. The bridge checks the complete native and R address sets separately,
proves every omitted native child has that exact default format inside the
same footer merge, and retains exact anchor, merge and height assertions.
This is lossless Excel serialization equivalence: the anchor supplies the
merged cell's displayed text and style. Added, removed, duplicated or styled
children still fail. P/K/W layouts retain their explicit footer cells and
exact native styles; stacktab's authentic note alignment is `general`.
The legacy S14 small-cell fixture likewise keeps its inline native note,
while independent user and authenticated automatic literals declare R's two
paragraphs; both complete footer regions and CSV bytes are asserted before
the original table and console comparisons.

Native `r(smallcells)` is a scalar threshold; R's canonical P.4 metadata is a
list. Stored comparison projects only that threshold for native scalar fields,
and the stratetab and Table1 runners separately assert the complete canonical metadata.
The original list remains intact, and every other native stored field remains
compared. Wttab's independently declared extra R paragraph rows are asserted
against native body geometry before a comparison copy projects `n_rows` back
to the authentic one-footer native count; the returned table and all other
stored fields retain their truthful R values.

WP-3F's `crossval_table1_primary.R` runs isolated native cases T38–T44
declared in `qa/data/table1_primary_cases.csv`. It authenticates the immutable
`712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129` Stata tabtools 2.5.1 tree's
73 ado files (a conservative complete helper closure), manifest and help before
prerequisite checks and after execution. Every program resolves to and is
explicitly loaded from that tree. Changed helpers and shadow adopath programs
must fail. The supervised native run uses owned `STATATMP`, checks all case
start/end and final completion markers and Stata errors, and never writes
existing goldens. T38–T44 remain owned by Table1; TC identifiers remain tabcell's.

The exact ten-row ESS control uses age 1–10, groups Small (rows 1–2) and
Large (3–10), weights 1 except row 2 weight 3. Primary protects two displayed
Ns plus the linked Weighted Small ESS (1.6); ordinary inference stays intact.
Strict and unweighted contrasts assert native matrices, counters, mode and
analytical SMD, with matrix columns matched by case-preserved identities rather
than the different frame order. Numeric value-column and quoted numeric header
selectors, repeated entries, descriptor N eligibility and native selector errors
are independent controls. Native unsafe replacement of protected ESS succeeds;
R intentionally refuses that target under default/custom/empty markers.
Publication raw fields never enter the compared grid or exported ledger.

The fast fixture `tests/testthat/fixtures/table1_primary_native/T25_stored.csv`
is an exact copy from held inventory commit `c215afdb788f88363c8d5289d7d13e3a31cfbe38`;
its `ARTIFACTS.csv` records source path, SHA256 and independently asserted MD5.
Its entire native stored contract, including mandatory active `smallcells_mode=full`,
is compared before WP-3G. The existing scalar threshold projection is reused
unchanged; separate typed-tuple/mode/count mutations must fail. WP-3G must retain
and account for these new native cases and authentic fixture provenance.

## How to run

```sh
Rscript qa/run_all.R                 # full lane (default)
Rscript qa/run_all.R adversarial     # installed adversarial cases only
Rscript qa/run_all.R sample          # installed sample-accounting cases only
Rscript qa/run_all.R interaction     # seeded combinations across adapters
Rscript qa/run_all.R crossval        # table1_tc() balance statistics against cobalt and twang
Rscript qa/run_all.R quick           # adversarial and sample-accounting cases
Rscript qa/run_all.R core            # quick + interaction + crossval, examples, weight validation
NOT_CRAN=true Rscript -e 'devtools::test(filter = "adversarial|sample-accounting")'
NOT_CRAN=true Rscript -e 'testthat::test_file("qa/test_adversarial_regression.R", load_package = "none")'
bash qa/tools/check_local.sh         # the CI release job on this machine (use on a Mac instead of the macOS CI job)
```

The GitHub Actions R-CMD-check workflow runs only when started by hand, and is kept disabled between runs (see **CI** below): `gh workflow enable R-CMD-check`, then `gh workflow run R-CMD-check --ref main` (Linux and Windows), `-f os=macos` (macOS only) or `-f os=all` (all five jobs). `qa/tools/check_local.sh` runs the steps of the macOS CI release job locally: it installs the dependencies, runs `R CMD check --as-cran` on the built tarball (failing on a WARNING), then runs the `quick` and `interaction` lanes. With `--full` it runs the full lane instead (installing `ipw`, which `validation_wttab.R` needs, and `cobalt` and `twang`, which `crossval_smd_balance.R` needs; Suggests lists none of them); `--source-tests` also runs the test suite from a copy of the source tree, as the ubuntu release job does; `--no-deps` skips installing dependencies.

The single-file command uses whichever installed copy `library(tabtools)` finds. The lane runner builds and installs the current source into a temporary library and verifies that it resolves there. Use separate scratch copies for concurrent runs to avoid sharing compilation and snapshot files. The runner prints one `RESULT:` line per executed file and ends with `RESULT: TOTAL`; a missing total line is a failure. `PASS` means every comparison ran without failure; `INCOMPLETE` means comparisons were skipped, including under `--allow-incomplete`.

The adversarial suites generate their data at runtime. They check variable-specific observed denominators, model-specific complete-case samples, missing columns, sparse factor levels, singular terms, weight exclusions and unavailable inference. They preserve each command's documented missing-data policy: they do not impute values or force different model samples to match. The regression suite also includes models deliberately stopped before convergence and fits with insufficient residual degrees of freedom.

The interaction matrix combines those conditions with fixed seeds across model
families and non-model routes. Its independent oracles use raw observations and
native estimates/covariance. The case manifest is
[data/interaction_matrix.csv](data/interaction_matrix.csv). A listed case is coverage
only when its test block and every declared seed actually run. Seed evidence
comes from successful block names or completion markers after loop assertions;
the runner reconciles both. Optional-package skips remain incomplete results.


`run_all.R` (Milestone H, H16; result reconciliation from the Codex audit,
CX-6) runs the lane in five steps:

1. It builds the package (`R CMD build --no-build-vignettes`) and installs
   the tarball, byte-compiled, into a temporary library. Nothing is
   installed into your R library, and nothing is compiled in the source
   tree.
2. It runs each selected QA file against that installed copy. Each file runs in its own supervised R process, with `NOT_CRAN=true`, `TABTOOLS_QA_LIB` naming the library, and a 600-second timeout. A timeout kills the child process tree and reports a failure.
3. It reconciles `qa/`. Every top-level `qa/*.R` must either be run by
   the runner or be listed in `qa/_skip.txt` with a reason. A skip entry
   for a script that is not in the tree (one that another branch adds) is
   reported as a note, not a failure.
4. It reconciles each file's result. Existing scripts use `qa_check()` and `qa_skip()`. The `test_adversarial_*.R`, `test_sample_accounting_*.R` and `test_interaction_matrix_*.R` files use ordinary `testthat` blocks; the runner records one comparison per block and refuses errors, uncaught warnings, empty tests, top-level expectations, file-level skips and truncated files. Both paths end with one completion marker, `QA-RESULT script=... status=... executed=n skipped=n failed=n`. The runner recomputes the status from those
   counts: **PASS** (every comparison ran, none failed), **INCOMPLETE**
   (some were skipped), **SKIP** (none ran), **FAIL** (a failed
   comparison, a non-zero exit, or no marker, i.e. the script stopped
   early). The exit status alone is never taken as a pass.
5. It prints every result with its counts, and exits with status 1 on
   any FAIL or unlisted script, 2 when nothing failed but a script was
   INCOMPLETE or SKIP (`Rscript qa/run_all.R --allow-incomplete` makes
   that 0, still reported), and 0 only when every script PASSed.

`TABTOOLS_QA_HIDE=ipw,WeightIt` makes `qa_has()` treat those packages as
missing, so the skip paths can be run without uninstalling anything
(`tests/testthat/test-qa-lane.R` does, and checks the runner's
reconciliation).

Runtime depends on the host and available optional packages; the
pre-matrix 2026-09-30 audit took about 90 seconds with R restricted to one CPU
core. The expanded full lane adds five matrix suites with 84 seeded scenarios
and a static catalog audit; its frozen run passed all 23 files and 286
comparisons. Timings and evidence are in the interaction plan above.

| Script | Run by `run_all.R` | Checks |
|---|---|---|
| `check_examples.R` | yes | The installed public API, as an installed user sees it. Every export has a help page with examples, and the examples run (one check per export). |
| `bench_fisher.R` | yes | Wall-clock bounds for Fisher's exact test: workspace escalation and the simulated fallback (one check per case). |
| `bench_fweight.R` | yes | Time (5 s) and memory (100 MB of R heap) bounds for `fweight` hypothesis tests, up to a total frequency of 5e7 (one check per case). |
| `validation_wttab.R` | yes | `wttab()` on real `ipw::ipwpoint()`/`ipwtm()` objects (ipw is not in Suggests; the fast lane uses a duck-typed list) and against WeightIt's own ESS and weight ranges (ATE, ATT, ATO, multi-category). A missing comparator package is a skip: the script is then INCOMPLETE, or SKIP when neither ran. |
| `crossval_regtab_formats.R` | yes, crossval/core/full | Pinned Stata-Tools 2.5.1 (`712044f8`) source hashes and a direct Stata 17 `string()` oracle: 18 f/g/e formats across 12 fixed values, covering width, precision, three-digit exponents, left/zero padding, decimal comma and native `ec` refusal. Native text is compared exactly with the shared R formatter and raw regtab/effecttab estimate/CI cells. The native helper is bounded to 120 seconds. Set `TABTOOLS_STATA_DIR` to the pinned export. Missing configuration/binary is a skip/incomplete result; wrong source hashes, native errors or a missing completion marker fail. Unique runtime inputs and logs are cleaned after each attempt; no golden is rewritten. |
| `crossval_puttab_layout.R` | yes, crossval/core/full | Native pinned Stata 2.5.1 puttab thin/medium/academic borders and explicit rules, panel header/zebra precedence, inline noheader panels, spanning headers, integer/imported numeric formats, and stacktab frame panels. Six scenarios compare actual workbook cells/styles/merges/widths/heights, CSV and Markdown bytes, and stored row/panel/span counts. Set `TABTOOLS_STATA_DIR` to the pinned export; missing configuration/oracle is incomplete, never PASS. |
| `crossval_puttab_flat.R` | yes, crossval/core/full | Eleven genuine native Stata 2.5.1 `regtab, frame(flat keys)` consumer scenarios: four border styles, CI/reverse projection, explicit common keys, panels, selected-input keys, no model span, and equal-name distinct selector calls. Immutable original and edited native inputs retain actual variable names, labels and per-column states; R retains per-original-model states. Complete CSV/Markdown bytes and workbook cells/styles/merges/widths/heights compare without body masks against an explicit-vars R publication peer; the stronger R default diagnostic paragraphs and counters are asserted separately in all artifacts. Installed help renders the contract. Set `TABTOOLS_STATA_DIR` to the pinned export; missing configuration/oracle is incomplete. No producer-estimate parity claim. |
| `crossval_session_sinks.R` | yes, crossval/core/full | Pinned Stata 2.5.1 actual Markdown files verify first replacement, subsequent append, repeated setting, A/B/A, clear/re-set, explicit replacement and ordinary explicit-sheet gating. Five literal title sequences and full Markdown snapshots compare; installed workbook contents/help verify R's deliberate unrelated-sheet preservation. Missing `TABTOOLS_STATA_DIR` or native oracle is incomplete. |
| `crossval_table1_primary.R` | yes, crossval/core/full | Authenticated Stata 2.5.1 T38–T44: exact primary/strict/unmasked weighted ESS and unweighted contrasts, full grids, analytical SMD, native modes/counters and suppression column identities; value-column versus numeric header selectors, descriptor N, repeated entries, native refusals and R's stronger protected-ESS refusal. Immutable full helper hashes and shadow-adopath negative controls. |
| `crossval_smd_balance.R` | yes | `table1_tc(smd = TRUE)` balance statistics against independent implementations. `smdtype = "population"` (McCaffrey et al. 2013 eq. 5) against cobalt 4.6.3 (`pairwise = FALSE, s.d.denom = "all"`), unweighted and under `wt`, and against twang 2.6.2 `mnps()` ATE eq. 5 values, unweighted and with twang's own weights. `"maxpair"` against cobalt (`pairwise = TRUE, s.d.denom = "pooled"`, unweighted; cobalt keeps the unweighted SD under weights, so weighted maxpair is checked through the two-group identity maxpair = \|pair\|), and categorical maxpair against a base-R Yang-Dalton oracle. `"pair"` and `smdpair` against cobalt on the two compared arms. Also: fweight equals the record-expanded data for every type, and `wtcompare` with `smdtype`. cobalt's `Min.Diff`/`Max.Diff` are signed extremes, so the comparison takes the larger absolute value. The categorical fixture puts the largest imbalance on the first and then the last level, so a dropped level cannot hide. A direct Stata 2.5.1 matrix (set `TABTOOLS_STATA_DIR` to its ado directory) checks population/maxpair with unweighted, importance-weighted (`wt`) and frequency-weighted data for continuous, log-continuous, binary and categorical rows. Each row kind also has a case with no values in one group, while the other rows retain finite SMDs: 30 calls, 120 numeric cells plus exact type/note metadata. Stata logs and inputs are cleaned from a unique scratch directory. Missing Stata/source configuration is reported as incomplete by the existing runner. |
| `demo_parity.R` | yes | The R demo (`qa/demo/demo_tabtools.R`) against the Stata demo: runs `tests/testthat/test-demo-parity.R` (every ported sheet, the console log and its Markdown, the Markdown report; `golden/demo/manifest.csv`). Fails on a skip too. `--update` regenerates the Stata side (see the parity harness below). |
| `test_demo_source.R` | yes, core/full | Native demo source guard controls: complete variable/observation order, values/missingness including literal ordinary/.a/.b on both source/schema sides and RDS attributes, R classes/native storage widths, labels, frozen schema, SHA-256 before haven reading, strict CLI/partial-update elements, new nonsymlink destinations/maker boundaries, honest runtime manifest routes, failed-run diagnostics/owned-child cleanup, and actual failed-child R-QA serialization before nonzero exit. Uses clearly synthetic fixtures; never generates native output. |
| `test_demo_aipw.R` | yes, core/full | Extracts only the demo's AIPW function AST. Balanced literal ATE/POM/covariance controls, an independent finite-difference Jacobian over all joint effect/outcome/treatment equations, and the actual SHA-bound native demo cohort against reviewed full-precision Stata17 coefficient/covariance fixtures. No full demo or native engine is executed by this file; missing fixtures refuse rather than skip. |
| `test_adversarial_descriptive.R` | yes | Observed and weighted denominators, covariate-specific missingness, lognormal exclusions, frequency expansion, row percentages, retained-sample weight cutpoints and ESS across extreme scales. |
| `test_adversarial_regression.R` | yes | Complete-case fitted models, absent covariates and factor levels, rank deficiency, clustered sample alignment, failed convergence and insufficient variance samples. |
| `test_adversarial_rates_effects.R` | yes | Weighted missing exposure/event/group values, Cox case deletion and singular terms, supplied rate populations, and extreme or unavailable effect intervals through composition. |
| `test_adversarial_mi_identity.R` | yes | Explicit IDs and strict MI checks after row names are erased, weighted complete cases, permutation invariance and independent HC1 calculations. |
| `test_adversarial_mixed_models.R` | yes | Genuine lmer/glmer/glmmTMB optimizer and Hessian failures, valid singular boundaries, rank adjustment and missing observations. |
| `test_adversarial_likelihood_models.R` | yes | Real failed count, multinomial and ordinal optimizers; valid positive CLM diagnostics, omitted terms and complete-case equivalence. |
| `test_adversarial_gee_survival.R` | yes | GEE fitting errors and working-correlation failures; identifiable Cox and parametric-survival failures, valid final-iteration controls, and genuine AER Tobit observed-sample inference. |
| `test_adversarial_transform_rows.R` | yes | Overlapping raw/polynomial covariates, nested matrix transforms and mixed-model random-slope metadata retain distinct basis coefficients and inference. |
| `test_sample_accounting_descriptive.R` | yes | Record vs weighted N, variable-specific missingness, retained categories, repeated specs, shared crude/weighted samples and raw-scale weights. |
| `test_sample_accounting_models.R` | yes | Stored fitted records, missing covariates, zero case weights, native N distinctions, UV and per-imputation populations. |
| `test_sample_accounting_rates_weights.R` | yes | Exposure contributors vs observed events, sequential exclusions, original-scale weights and provided-vector provenance. |
| `test_sample_accounting_composition.R` | yes | Separate fitted/evaluation populations, reused sources, nested composition and presentation wrappers. |
| `test_sample_accounting_integration.R` | yes | Independent cross-family sample distinctions, schema validation, privacy and data-frame/presentation transport. |
| `test_interaction_matrix_descriptive.R` | yes | Seeded missingness, sparse/absent categories, variablewise and weighted denominators, and extreme weights. |
| `test_interaction_matrix_likelihood_mixed.R` | yes | Native likelihood and mixed-model coefficient, covariance, and accepted-record comparisons under combined perturbations. |
| `test_interaction_matrix_survival_survey_gee.R` | yes | Survival/censored outcomes, competing risks, survey designs, and GEE under combined sample and design complications. |
| `test_interaction_matrix_composition.R` | yes | Weighted estimation, MI/UV, effect summaries, source-local populations, and composed transport. |
| `test_interaction_matrix_independent.R` | yes | Supported-adapter coverage reconciliation and independent interaction probes. |
| `make_golden.R`, `make_regtab_fixtures.R`, `make_regtab_phase5_review_data.R`, `make_regtab_5w_review_data.R`, `make_regtab_h_t2b_data.R`, `make_mi_fixture.R` | no (`_skip.txt`) | Generators, which need Stata (or mice) or rewrite committed fixtures. |

By default, multiple-imputation sample checks rely on stable observation row names when those identify the sample. Renumbering rows after filtering can erase that evidence; the count-only fallback cannot establish identity. Use `tt_mi(fits, observation_ids = ids, sample_check = "strict")` to require explicit unique observation IDs after case deletion, subsetting and zero-weight exclusion. Reordered observations are allowed when the ID sets agree. The table's `meta$mi_sample_identity` records the check method and counts without copying IDs into metadata. Caller-supplied IDs must correctly describe the estimation sample.

Fit guards use stored diagnostics rather than treating every warning as failure. A valid singular random-effect boundary may still have usable fixed-effect covariance. Cox and `survreg` discard some convergence information: default limits, literal control arguments in the fitted call, or literal limits in qualified `survival::coxph.control()` calls identify exhaustion for right-censored Breslow/Efron Cox fits. Static matching handles partial and positional control arguments without evaluating them. A materially worse-than-null likelihood identifies some failed `survreg` fits when the intercept-only fit is nested. Mutable controls, unqualified control functions, counting-process/exact Cox fits and other ambiguous survival status remain limitations. The unstructured GEE correlation guard covers implicit waves and the default named correlation design; explicit waves and custom designs remain unverified. Inspect fitting-time warnings before reporting inference.

## Lane membership

`quick` is a subset of `core`, which is a subset of `full`. The existing benchmarks and demo parity checks remain in the default full lane.

| Lane | Files |
|---|---|
| `adversarial` | The `test_adversarial_*.R` files listed above. |
| `sample` | The `test_sample_accounting_*.R` files listed above. |
| `interaction` | The `test_interaction_matrix_*.R` files listed above. |
| `crossval` | `crossval_smd_balance.R`, `crossval_puttab_layout.R`, `crossval_regtab_formats.R`, `crossval_puttab_flat.R`, `crossval_session_sinks.R`, `crossval_table1_primary.R`. |
| `quick` | `adversarial` plus `sample`. |
| `core` | `quick` plus `interaction`, `crossval`, `test_demo_source.R`, `test_demo_aipw.R`, `check_examples.R` and `validation_wttab.R`. |
| `full` (default) | `core` plus `bench_fisher.R`, `bench_fweight.R` and `demo_parity.R`. |
| `benchmark` | `bench_fisher.R` and `bench_fweight.R` only. |

The other gates are separate:

- The fast lane: `NOT_CRAN=true Rscript -e 'devtools::test()'`, which
  covers every golden, and the interop converters on every golden.
- `R CMD build` followed by `R CMD check --as-cran` on the tarball.

**Memory in `bench_fweight.R`.** The bound measures the call, not R's
byte-compiler (finding F21). Under `pkgload::load_all()` the namespace is
not byte-compiled, and the JIT compiled closures during the first
measured call. That call took 115 MB, while the same case run again took
71 MB. The script now switches the JIT off around the measured calls, and
the runner uses a byte-compiled installed copy (about 30 MB). Run by hand
under `load_all()`, it stays under 80 MB. The bound stays 100 MB
(decision H-D9).

**CI.** The GitHub Actions workflow `.github/workflows/R-CMD-check.yaml`
runs only when started by hand (no push or pull-request triggers). The workflow is kept disabled between runs, so a run is

```sh
gh workflow enable R-CMD-check
gh workflow run R-CMD-check --ref main               # os=linux-windows (default)
gh workflow run R-CMD-check --ref main -f os=macos   # macOS only
gh workflow run R-CMD-check --ref main -f os=all     # all five jobs
gh workflow disable R-CMD-check                      # after the run
```

`linux-windows` runs ubuntu release and oldrel-1, windows release, and a
hard-dependencies job (ubuntu release, `dependencies: '"hard"'`,
`_R_CHECK_FORCE_SUGGESTS_: false`) that catches an unconditional use of a
suggested package (pre-release review P1-2); `macos` runs macos-latest
release. Each job is capped at 45 minutes (Windows 75). All jobs set
`NOT_CRAN: true`, so the golden sweeps run. Its current status is not
recorded here.

Every release-R job with Suggests (Linux, Windows and macOS) also runs
`Rscript qa/run_all.R quick` and `Rscript qa/run_all.R interaction` against a
fresh temporary installation. These installed QA files are excluded from the
checked tarball and need their separate CI step. The ubuntu release job also
runs the test suite from the source tree, where the StataCorp fixtures that
`.Rbuildignore` drops exist. Missing model dependencies remain reported
skips/incomplete results; the hard-dependency job exercises the conditional
fast-test paths separately. `qa/tools/check_local.sh` runs the macOS release
job's steps on a local machine (`--source-tests` adds the source-tree run).

### Binary packages for users without a compiler

The `build-binaries` workflow, also started by hand and kept disabled, builds
binary packages: Windows (`tabtools_<version>.zip`) for R release and
oldrel-1 by default, macOS arm64 (`.tgz`) for R release with `-f os=macos`
(10x minutes), or all three with `-f os=all`. Each is checked to install into
an empty library and run, and uploaded as one artifact per platform and R
version (`gh run download <run id>`). A binary works only with the R minor
version that built it. The macOS build sets `MACOSX_DEPLOYMENT_TARGET=11.0`
and checks it, so the shared library does not require the runner's own macOS
version. To build the macOS binary on a Mac instead, at no cost, export a
clean tree (`git archive HEAD | tar -x -C <dir>/src`), run
`R CMD build <dir>/src`, then
`MACOSX_DEPLOYMENT_TARGET=11.0 R CMD INSTALL --build -l <lib> tabtools_<version>.tar.gz`.

## Export -> coverage map

The fast adversarial files are `test-adversarial-descriptive.R` (`table1_tc()`, `desctab()`, `wttab()`), `test-adversarial-regression.R` (`regtab()`, `regtab_uv()`, `tt_mi()`, `tt_vcov()`), `test-adversarial-rates-effects.R` (`tt_rates()`, `stratetab()`, `effecttab()`), and `test-adversarial-qa-runner.R` (empty, skipped and truncated QA detection). Their installed counterparts above also exercise `comptab()`, `hrcomptab()` and `as_forest_data()`.

The follow-up files `test-adversarial-mi-identity.R`,
`test-adversarial-mixed-models.R`, `test-adversarial-likelihood-models.R` and
`test-adversarial-gee-survival.R` exercise `regtab()`/`tt_vcov()` failure
guards and MI identity. Each has its installed counterpart in the file index.
`test-adversarial-transform-rows.R` and its installed counterpart cover
overlapping and nested matrix-valued terms with independent coefficient and
interval checks.

Sample-accounting fast tests are `test-sample-accounting-{contract,descriptive,models,rates-weights,composition}.R`; their installed counterparts are indexed above.

The 2026-09-30 regressions add five `test-audit-20260930-*.R` files:
`core` checks factor conversion and oversized format/colour inputs;
`descriptive` checks complex columns, suppression arithmetic and native
RNG lengths; `regression` checks literal names, user covariance matrices
and supplied statistics; `effects` checks confidence levels, scale
provenance and rate limits; `exports` checks merge headers, reference-cell
styling, validation and provenance through composition.

| Export | Fast lane (`tests/testthat/`) | QA lane |
|---|---|---|
| `table1_tc()`, `desctab()` | `test-golden-table1.R` (every table1 golden); `test-table1-*.R` (engine, weights, fweight tests, validation, review fixes, H7/H10/H17 in `test-table1-hardening.R`); `test-smallcells-*.R`; `test-classifier-units.R`, `test-golden-classifier.R` (automatic typing); `test-writers-hardening.R` (H8 targets); `test-codex-audit.R` (D1 weight scale, D5 typing); `test-table1-smdtype.R` (`smdtype`, `smdpair`, pair header and footnote); `test-adversarial-descriptive.R`; `test-sample-accounting-descriptive.R` | `check_examples.R`, `bench_fisher.R`, `bench_fweight.R`, `test_adversarial_descriptive.R`, `test_sample_accounting_descriptive.R`, `crossval_smd_balance.R` (`smdtype`, `smdpair`), `crossval_table1_primary.R` (T38–T44 masks, ESS, replacement selectors) |
| `regtab()` | `test-golden-regtab.R` (every regtab golden); `test-regtab-*.R`; `test-stubs.R` (the model allow-list); `test-model-zoo.R`; `test-writers-hardening.R` (H8); `test-codex-audit.R` (R1, R2 GEE; R4 outcome identity); `test-adversarial-regression.R`; `test-sample-accounting-models.R`; `test-regtab-formats-v251.R` (full formats, labels, file sinks); `test-tt-flat-v251.R` (source states and keys) | `check_examples.R`, `test_adversarial_regression.R`, `test_sample_accounting_models.R`, `crossval_regtab_formats.R` |
| `tt_vcov()`, `tt_vce_types()`, `tt_ci_methods()` | `test-regtab-vce.R`, `test-regtab-5w-review.R`, `test-regtab-generics.R`, `test-regtab-gee.R`, `test-regtab-h19-gee-scale.R`, `test-codex-audit.R` (R1, R2, R3); `test-adversarial-regression.R` | `check_examples.R`, `test_adversarial_regression.R` |
| `tt_mi()` | `test-regtab-mi.R` (MI01-MI08, mice integration), `test-regtab-stats-tokens.R` (`mi_m`, `fmi`), `test-codex-audit.R` (R3: `tt_vcov()` refuses what `regtab()` refuses); `test-adversarial-regression.R`, `test-adversarial-mi-identity.R`; `test-sample-accounting-models.R` | `check_examples.R`, `test_adversarial_regression.R`, `test_adversarial_mi_identity.R`, `test_sample_accounting_models.R` |
| `tt_flat()`, `[.tt_flat` | `test-tt-flat-v251.R` (keys/states, structural rows, projection, metadata/schema errors, help, RNG) | `check_examples.R`; installed selector/S3 dispatch is a separate installed-user gate |
| `stata_fmt()` (internal shared formatter) | `test-regtab-formats-v251.R` (e width/precision/extremes, ordinary f/g, classed integer(0) error) | `crossval_regtab_formats.R` (independent pinned native string values) |
| `as_forest_data()` | `test-regtab-units.R`, `test-regtab-review.R`, `test-golden-comptab.R` (composites) | `check_examples.R`, `test_adversarial_rates_effects.R` |
| `tt_table()`, `validate_tt_table()` | `test-tt-table.R`, `test-tt-table-hardening.R` (H13), `test-renderers.R`, `test-codex-audit.R` (CX-4, CX-5); `test-sample-accounting-contract.R` | `check_examples.R`, `test_sample_accounting_integration.R` |
| `print()`, `format()`, `as.data.frame()` methods | `test-renderers*.R`, `test-golden-harness.R` (console listings), `test-tt-table.R`; `test-sample-accounting-contract.R` | `check_examples.R` (via the `tt_table` and command pages), `test_sample_accounting_integration.R` |
| `tt_write_xlsx()`, `tt_write_csv()`, `tt_write_markdown()` | `test-renderers.R`, `test-renderers-golden.R`, `test-golden-harness.R` (sinks on every golden), `test-writers-hardening.R` (H8, H12 byte pins, H20), `test-codex-audit.R` (CX-2 full disk, CX-5) | `check_examples.R` |
| `flextable::as_flextable()` method, `tt_as_gt()` | `test-interop-flextable.R`, `test-interop-gt.R`, `test-interop-golden.R` (all goldens), `test-interop-units.R` | `check_examples.R` |
| `tt_as_gtsummary()`, `tt_as_tinytable()` | `test-interop-gtsummary.R`, `test-interop-tinytable.R`, `test-interop-golden.R` (all goldens) | `check_examples.R` |
| `regtab_uv()` | `test-regtab-uv.R`; `test-adversarial-regression.R` | `check_examples.R`, `test_adversarial_regression.R` |
| `tt_merge()`, `tt_stack()` | `test-tt-compose.R`; `test-sample-accounting-composition.R`; `test-tt-flat-v251.R` (source alignment, repeated/nested identity) | `check_examples.R`, `test_sample_accounting_composition.R` |
| `tt_from_modelsummary()` | `test-regtab-modelsummary.R` | `check_examples.R` |
| `tabtools_options()` | `test-options.R`, `test-writers-hardening.R` (H20) | `check_examples.R` |
| `tt_as_factor()` | `test-regtab-review.R` | `check_examples.R` |
| `puttab()`, `stacktab()` | `test-golden-export.R` (P01-P15, K01-K09), `test-export-units.R`, `test-codex-audit.R` (CX-1 appends and commits, CX-3 duplicate names); `test-sample-accounting-composition.R`, `test-puttab-layout.R`, `test-puttab-flat.R` | `check_examples.R`, `test_sample_accounting_composition.R`, `crossval_puttab_layout.R`, `crossval_puttab_flat.R` |
| `tt_rates()`, `stratetab()` | `test-rates.R` (`strate` goldens), `test-golden-stratetab.R` (S01-S08), `test-stratetab-units.R`, `test-stratetab-review.R`, `test-codex-audit.R` (D4 reserved names, `fixtures/codex_audit/`); `test-adversarial-rates-effects.R`; `test-sample-accounting-rates-weights.R` | `check_examples.R`, `test_adversarial_rates_effects.R`, `test_sample_accounting_rates_weights.R` |
| `effecttab()`, `tt_effect_rows()` | `test-regtab-formats-v251.R` (raw matrix precision, literal cells and sinks); `test-tt-flat-v251.R` (raw keys and states); `test-golden-effecttab.R` (E01-E26, W12, W13), `test-effecttab-units.R`, `test-codex-audit.R` (D3); `test-adversarial-rates-effects.R`; `test-sample-accounting-composition.R` | `check_examples.R`, `test_adversarial_rates_effects.R`, `test_sample_accounting_composition.R`, `crossval_regtab_formats.R` |
| `comptab()`, `hrcomptab()` | `test-golden-comptab.R` (C01-C10, with C01's regtab source sheets), `test-comptab-units.R` (placement, identity matching, refusals, the per-model Reference merge, forest data; data-frame models from `helper-comptab.R`), forest data against Stata's `eplotframe()` in `test-golden-comptab.R` (`fixtures/comptab_forest/`), `test-interop-golden.R` (converters), `test-codex-audit.R` (R4 identity with `type = "right"`); `test-sample-accounting-composition.R` | `check_examples.R`, `test_adversarial_rates_effects.R`, `test_sample_accounting_composition.R` |
| `wttab()` | `test-golden-wttab.R` (W15-W22), `test-wttab-units.R` (percentiles against `fixtures/wttab_pctile/`, adversarial weights, inputs), `test-interop-golden.R` (converters), `test-codex-audit.R` (D2 ESS scale); `test-adversarial-descriptive.R`; `test-sample-accounting-rates-weights.R` | `check_examples.R`, `validation_wttab.R`, `test_adversarial_descriptive.R`, `test_sample_accounting_rates_weights.R` |
| the QA runner | `test-qa-lane.R` (CX-6: result reconciliation; `validation_wttab.R` with its comparators hidden) | itself |

WP-2D coverage: `regtab()`/`effecttab()` full-format numeric text has the native cross-validation above; fast `test-regtab-formats-v251.R` checks explicit-digits conflicts (including `NULL`), literal separators, raw precision, custom headers, every compact/p-value layout, ordered complete CSV/XLSX header/body/footer matrices and complete literal Markdown lines with distinct model values. `stata_fmt()` includes classed zero-length errors and ordinary f/g/e known values. Fast `test-tt-flat-v251.R` checks fit-owned states and raw/equation/statistic keys, original-source alignment, nested/reused blocks, projected all-model provenance, reserved schemas, malformed headers/identities, help rendering and RNG invariance. Installed selector export and S3 dispatch remain part of the separate installed-user gate. High-precision general formats (13 or more significant digits) retain the documented native rounding difference and are not claimed as native parity by this oracle.

## Stata parity harness

| File | Role |
|---|---|
| `make_golden.R` | Entry point: `Rscript qa/make_golden.R [--update] [--only=T01,R01] [--fixtures] [--no-scenarios] [--no-classifier] [--no-fmt]`. Needs `stata-mp` on PATH and the dev checkouts `~/Stata-Tools/tabtools` and `~/Stata-Tools/fvgen`. Refuses to overwrite goldens without `--update`. `--stata-tools=DIR` takes `tabtools/` and `fvgen/` from DIR instead, e.g. a `git -C ~/Stata-Tools archive <commit> tabtools fvgen` export when the working tree carries uncommitted edits (VERSIONS.csv then records DIR's paths). |
| `demo_parity.R --stage-native` / `--update` | Reviewed two-run native workflow. Stage requires `--stage-output=NEW_DIR --commit=712044f8 --tabtools-version=2.5.1 --stata-tools=GIT_REPO`; it exports committed source and saves authentic artifacts, eight source snapshots, ordered native storage metadata, frozen schema, hashes and receipt. STAGED supplies no R verdict. Update uses the same explicit pin plus `--demo-data-dir=ACCEPTED_DIR --receipt-dir=NEW_DIR`, independently reconstructs all source data and compares complete schema/values/labels/RDS before manifest checks and promotion. `--work-dir` names a canonical writable parent; only a new owned child is removed. Both durable destinations must be new, nonsymlink and outside work. `--only=FILES` still executes all guards, requires the existing exact SOURCE pin and cannot replace console artifacts. `--keep-work` is rejected. SOURCE records native acceptance/R pending; a separate execution receipt records final R QA. |
| `stata/run_demo.do` | Runs the unmodified exported native demo with the export first on the adopath/version guard, confirms and copies all 16 workbooks, console log/Markdown and report. |
| `make_demo_data.R` | No arguments preserve legacy source-only projection. Native projection is explicit: `--mode=native --fixtures-dir=ACCEPTED_DIR --output=qa/demo/demo_tabtools.rds`. Verifies source pins/hashes/schema before loading and preserves column classes/labels. The seven-member RDS omits hurdle until it has a consumer; all eight original snapshots remain guarded. Dataset labels/notes are outside the existing RDS projection and retained in original native files. |
| `stata/make_fixtures.do` | Builds `tests/testthat/golden/fixtures/*.dta` (only with `--fixtures`; changing the data invalidates every golden). |
| `stata/golden_helpers.do` | `golden_versions`, `golden_run`, and the Mata `r()` dumper used by the generated driver. |
| `stata/make_classifier_goldens.do` | `swilk.csv`, `rng_mt64.csv`, `autotype.csv`, `subsample_ids.csv`, `detect_vartype.csv`, and the `vartype_branches.dta` fixture. |
| `stata/make_fmt_goldens.do` | `stata_fmt.csv`: `string(x, fmt)`, `round(x, u)`, `headerperc` probes. |
| `tools/mt64_reference.py` | Python MT19937-64 oracle for Stata's `runiform()`. |
| `stata/make_table1_review.do` | Stata expectations for the Phase 2 review fixes (`tests/testthat/fixtures/table1_review/`): display-rounding ties, empty strings, `percsign`, empty delimiters, console separators. Run from the repo root: `stata-mp -b do qa/stata/make_table1_review.do`. |
| `stata/make_table1_review_p3.do` | The same fixes in the weighted and small-cell paths (RW01-RW12 in `tests/testthat/fixtures/table1_review/`, read by `test-table1-review-p3.R`). |
| `make_regtab_fixtures.R` | Writes the Phase 4 review fixtures `golden/fixtures/nbsim.dta` (overdispersed counts) and `autolong.dta` (auto with 70+ character labels): `Rscript qa/make_regtab_fixtures.R`. Rerunning rewrites them; every golden on them must then be regenerated. |
| `stata/make_regtab_review.do` | Stata expectations for the Phase 4 review fixes that are not golden scenarios (`tests/testthat/fixtures/regtab_review/`): `vce(oim)` standard errors (probit, cloglog, Poisson identity, Gamma log, nbreg) and the `eplotframe()` of golden R52. Run from the repo root: `stata-mp -b do qa/stata/make_regtab_review.do`. |
| `bench_fisher.R` | Timing guard for Fisher's exact test (workspace escalation, simulated fallback): `Rscript qa/bench_fisher.R`. Wall-clock bounds, so not in `tests/`. |
| `bench_fweight.R` | Time and memory guard for `fweight` hypothesis tests (aggregated, no expansion; Phase 3 review P2-3): `Rscript qa/bench_fweight.R [pkg dir] [largest total]`. Every case must stay under 5 s and 100 MB of R heap up to a total frequency of 5e7. The JIT is off during the measured calls (see above). |
| `stata/probe_h10_overflow.do`, `stata/probe_h12_markdown.do` | Milestone H probes (Stata 17, tabtools `git archive` export; rerun on 2.1.12 for C5): overflow and nonfinite cells in `table1_tc`, e-notation with three-digit exponents, Mata's range; the Markdown writer's bytes. Their results are pinned in `test-table1-hardening.R`, `fixtures/table1_hardening/` and `test-writers-hardening.R`. |
| `stata/make_rowname_probe.do` | Stata's `r(table)` row names for `tests/testthat/fixtures/rownames/labels.txt`, by the exact code of regtab.ado and desctab.ado 2.1.12 (Stata built-ins only; both deduplicate with `_2`, `_3`): `stata-mp -b do qa/stata/make_rowname_probe.do`; read by `test-stata-rownames.R`. |
| `make_regtab_h_t2b_data.R`, `stata/make_regtab_h_t2b.do` | Milestone H group t2b fixtures (`tests/testthat/fixtures/regtab_h_t2b/`): the synthetic inputs (`Rscript qa/make_regtab_h_t2b_data.R`), then Stata's pseudo R-squared with `offset()` (O1-O5), `streg, vce(cluster inst)` on `survival::lung` (S1), `xtgee, scale(2)` (G0-G3) and a covariate named `p` (P1-P2): `stata-mp -b do qa/stata/make_regtab_h_t2b.do <DIR>`. |
| `stata/golden_wttab.do` | The Stata reference for `wttab()` (goldens W15-W22; Stata tabtools has no wttab): a `wttab` program that computes every statistic with `summarize`/`_pctile`, formats it with `string()`, writes the table with tabtools' `puttab ..., varlabels`, and returns `r(W)` and `r(trunc)`. Loaded by the scenarios' `stata_setup`; a starting point for a Stata twin (EW4). |
| `stata/make_wttab_pctile.do` | Stata's `_pctile` and `summarize, detail` percentiles on deterministic data (small n, ties, n p / 100 knife edges): `stata-mp -b do qa/stata/make_wttab_pctile.do` writes `tests/testthat/fixtures/wttab_pctile/stata_pctile.csv` (`%21x`), read by `test-wttab-units.R`. |
| `stata/make_fixture_comptab.do` | The `hrt_bin`/`hrt_dose` fixtures of goldens C04, C08, C09: the model data of the demo's `hrcomptab` section (seed 20260417), draw for draw. Called at the end of `make_fixtures.do`; runs on its own from the repo root: `stata-mp -b do qa/stata/make_fixture_comptab.do`. The C scenarios' rate blocks are rows of `tests/testthat/fixtures/stratetab_blocks.csv` (`stata/golden_stratetab_blocks.do`). |
| `stata/make_comptab_forest.do` | Stata 2.1.12's `eplotframe()` of three composites (hrcomptab on golden C04's frames, a vertical comptab on C05's with rows typed out of order, hrcomptab with an `ib2.` reference on C09's rates) at `%21.17g`: `stata-mp -b do qa/stata/make_comptab_forest.do STATA_TOOLS_DIR` writes `tests/testthat/fixtures/comptab_forest/`, read by `test-golden-comptab.R` (no golden covers forest data). |
| `stata/probe_nlme_lme.do` | Task 5.19: Stata `mixed`'s standard errors, variance-parameter e(V) and e(ll) on `mixed_bp` (ML, REML, random slopes), against `nlme::lme`'s varFix and `summary.lme()`'s rescaled SEs (Stata built-ins only): `stata-mp -b do qa/stata/probe_nlme_lme.do`. Results in its header, pinned in `test-regtab-nlme.R`. |
| `stata/probe_codex_audit.do` | Stata's behaviour on the Codex audit's D1 (tiny weights), D2 (msm_weight's ESS), D4 (`strate` with a grouping variable named `Y`, `_Y`, ...) and D5 (huge values in automatic typing) inputs; writes the `strate` file of `tests/testthat/fixtures/codex_audit/`. Results in its header and in the plan's "Codex audit outcome". |
| `stata/make_fixture_clogit.do`, `stata/make_fixture_cohort_pp.do`, `stata/make_fixture_effecttab.do` | Further fixtures, called at the end of `make_fixtures.do`: the conditional logistic data, the person-period split of the cohort, and the effecttab data. |
| `stata/make_rates_goldens.do`, `stata/make_smallcells_goldens.do` | Run by `make_golden.R`: Stata `strate` rates for `tt_rates()` (`rates_strate.csv`) and the small-cell suppression engine goldens (`smallcells_engine.csv`). |
| `stata/golden_stacktab_blocks.do`, `stata/golden_stratetab_blocks.do` | Source blocks for the stacktab (K) and stratetab (S) goldens, run from each scenario's `stata_setup`. |
| `stata/fmt_probe_values.txt` | The input values `make_fmt_goldens.do` formats. |
| `stata/make_table1_phase3.do`, `stata/make_table1_qa.do`, `stata/probe_weights.do` | Stata expectations for `table1_tc` beyond the goldens: weighting and small-cell combinations, the ported Stata QA contracts, and the reference values of `test-table1-weights.R`. |
| `stata/make_regtab_phase5a.do`, `stata/make_regtab_phase5b.do`, `stata/make_regtab_phase5_review.do`, `stata/make_regtab_5w_review.do`, `stata/make_regtab_vce.do`, `stata/make_regtab_mi.do`, `stata/make_regtab_methods.do`, `stata/make_regtab_union.do`, `stata/make_regtab_glm_ll.do` | Stata expectations for `regtab` cases that are not golden scenarios: ordinal, multinomial, zero-inflated, parametric survival and Fine-Gray models; mixed models and GEE; review fixes; weighted and robust variance; multiple imputation; the methods sentence; level joining across models; Gamma and inverse Gaussian log-likelihoods. |
| `stata/make_stratetab_dp.do`, `stata/make_stratetab_round.do` | `strate` files written under `set dp comma`, and Stata's rounding unit, for `stratetab`. |
| `tools/check_local.sh` | The macOS CI release job (dependencies, `R CMD check --as-cran`, the `quick` and `interaction` lanes; optionally the source-tree tests) on the local machine; the free stand-in for the macOS Actions job. |
| `tools/qa_result.R` | The QA scripts' executed/skipped/failed bookkeeping and completion marker, and the runner's `qa_reconcile()` (CX-6). |
| `tools/demo_data.R` | Source-only complete native demo data/storage/schema/RDS guards and staging CLI. Native snapshots are separate from the immutable 272-scenario inputs. |
| `notes/` | Developer notes: `effecttab_scoping.md` and `interop_research/`, indexed in `notes/README.md`. |
| `.stata-work/` | Generated drivers and Stata logs (gitignored). |

Every fixture do-file that loads tabtools takes the Stata-Tools directory as
its argument, `stata-mp -b do qa/stata/<file>.do <DIR>` (default
`~/Stata-Tools`); pass the same `git archive` export as `--stata-tools=`.
Stata then names the log after the last word of `<DIR>` (a directory
`.../stata-2.1.12` logs to `stata-2.log`), so give the export a plain name.
Baseline moves regenerate `make_golden.R --update` and every
`make_table1_*.do` and `make_regtab_*.do`.

The scenario manifest is `tests/testthat/golden/scenarios.csv` (one row per
golden; edit it, then regenerate with `--update --only=<ids>`).

**Source-only data.** StataCorp's example datasets are not redistributed
with the package (user decision 2026-09-27), so these stay in the
repository and out of the package build (`.Rbuildignore`):

- the golden fixtures `auto.dta` (`sysuse auto`, `make_fixtures.do`),
  `autolong.dta` (auto with longer labels, `make_regtab_fixtures.R`),
  `nhanes2.dta` and `union.dta` (`webuse`, `make_fixtures.do`),
  `cattaneo2.dta` and `bdsianesi5.dta` (`webuse`,
  `make_fixture_effecttab.do`), with `cohort_pp.dta` (4.5 MB, size only);
- the goldens of scenarios P02 and P09 (`golden/P02.*`, `golden/P09.*`),
  which list rows of `auto` verbatim; their sheets are in their own
  workbooks `P02.xlsx` and `P09.xlsx` (the Stata call names
  `using "@ID@.xlsx"`), not in the shipped `puttab.xlsx`;
- the R demo, `qa/demo/demo_tabtools.R`, and its datasets
  `qa/demo/demo_tabtools.rds` (which hold `auto` and `union`), with the
  Stata demo goldens `tests/testthat/golden/demo/`.

`golden_source_only_fixtures` and `golden_source_only_ids` in
`tests/testthat/helper-golden.R` list them. A source checkout is detected
by `qa/` (`golden_in_source()`), which is absent from the tarball, an
unpacked tarball and an `--install-tests` install. Every test that reads a
source-only fixture (through `golden_fixture()` or `golden_fixture_path()`)
skips when it is absent outside a source checkout, the inventory checks
leave P02 and P09 out there (`golden_ids_in_build()`), and
`test-demo-parity.R` skips the same way; in the repository they all run (a
missing file there is an error). The other goldens computed from these
datasets (csv, md, txt, xlsx) are derived statistics and stay in the
build.

## Demos

| File | Role |
|---|---|
| `demo/demo_tabtools_short.R` | The beginner demo the package README points to: one short example of each main table on a simulated cohort, no Stata data. It writes `demo_tabtools_short.xlsx`, `.docx` and `_table2.csv` to the working directory; `demo_tabtools_short.md` is the code with its output. Not run by `run_all.R`. |
| `demo/demo_tabtools.R`, `demo/demo_tabtools.rds` | The R twin of the Stata demo and its datasets (source-only, above), checked by `demo_parity.R`. The workbooks, `console_output.*` and `demo_markdown_report.md` it writes go to `out_dir`. |

Run either from the root of a clone, with tabtools installed:

```r
source("qa/demo/demo_tabtools_short.R", echo = TRUE)

demo <- new.env()                 # its objects stay out of your workspace
demo$out_dir <- "tabtools_demo"   # optional; the default is a folder in tempdir()
source("qa/demo/demo_tabtools.R", local = demo)
```


### Native 2.5.1 transition

The ordinary 272 scenarios use the complete native 2.5.1 baseline from
`c215afdb788f88363c8d5289d7d13e3a31cfbe38`. `golden/BASELINE.csv`,
`golden/ARTIFACTS.csv` and `golden/INPUTS.csv` record its origin, every root
artifact and all 21 unchanged scenario inputs. Historical W6/S18 publication
cases are explicitly named under `fixtures/backcompat/table1-2.1.14`;
the former mixed S17/RW07 files are retained under their own backcompat name.
Current S17/RW07 fixtures require the independently reviewed native transition
producer, `stata/make_table1_native_transition.do`, before acceptance.

The eight accepted native demo snapshots under `demo/data/native-2.5.1`
include native storage/schema and source/recipe hashes. They are separate from
the unchanged ordinary scenario inputs. `make_demo_data.R --mode=native`
projects the seven-member demo RDS from these authentic snapshots. The actual
native catalog contains 102 sheets in 16 workbooks, 56 console blocks and four
report tables; `golden/demo/manifest.csv` classifies all 162 entries. Its 103
headings record source provenance. Catalog classification is not R execution
acceptance. Generated demo books/logs/report and `SOURCE.csv` remain pending
until a second independent native reconstruction passes every source/RDS guard
and the affected R comparisons pass. The first transition must use the whole
manifest, without `--only`.
