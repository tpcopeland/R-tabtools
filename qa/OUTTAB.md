# Outtab acceptance

WP5C owns four new outtab modules, its generated help, native-free tests and
this standalone QA lane. It does not edit the central writer/session/flat
registries or the ordinary golden catalog. Source freeze/review precedes all
acceptance and native generation. The approved merged WP5A cell formatter is
a required dependency, with no copied or fallback renderer.

Independent fast controls use exposed4/10 and comparator2/20. Their crude RR
is4; uncorrected log-RR score-sandwich variance is3/5 and the separate Stata ML
30/29 correction gives18/29. Logistic OR is6 with model variance35/36. Plain
Poisson model variance is3/4. Confidence limits are independently specified
as ratio times exp(+/-qnorm(.975)*sqrt(variance)). Zou2004 printed p703 grounds
the log-risk model and sandwich construction, not Stata's finite-sample factor.
The held software/native VCE source grounds that correction separately.

The default model is bounded to unweighted Poisson-log or binomial-logit glm;
callback fits must retain verified original row names/model/x/y. Counts use
eligible rows before model missingness. Formula complete cases and actual
retained fit rows are separate. Extra fitter row removal is code-2; ordinary
covariate loss is not. Minimum crude exposed events skips fitting (code-1).
Actual nonconvergence is430; explicit native-equivalent unavailable/boundary
ratio logic is459. Portable R engine errors retain native_rc=NA and their
actual condition class/message. Header labels never choose the effect scale.
Raw analytical returns remain available under final primary publication masks.

Root runs the reviewed native producer in an exact isolated source archive,
with a newly allocated output directory outside that archive:

```sh
/usr/local/stata17/stata-mp -b do qa/stata/make_outtab_native.do /home/tpcopeland/R-tabtools-wt/_st251/tabtools /ABS/NEW/OWNED/OUTPUT
```

Authenticate all90 native ADO/help files against `data/outtab_native_pin.csv`
(exact Git712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129). Preserve native exit/log,
Stata17/mt64 runtime, exact nine-case success marker, every output hash, source
and new literal input hashes, plus source/process/TMPDIR cleanup receipts.
OT001 is modified Poisson; OT002 logit; OT003 adjusted missingness; OT004
primary masks/raw analytical returns; OT005 minimum events; OT006 overlapping
panels/observation indicators; OT007 boundary events; OT008 wide separator;
OT009 unique-identity cluster correction. New outtab_truth.dta is generated
from literal values without RNG; no existing fixture or producer is invoked.

After independent native authentication, root records SOURCE.csv and
ARTIFACTS.csv in the accepted output directory. The standalone QA command uses
the explicitly owned installed library and durable report destination:

```sh
TABTOOLS_OUTTAB_NATIVE_DIR=/ABS/ACCEPTED/OUTPUT TABTOOLS_OUTTAB_QA_RESULTS=/ABS/NEW/REPORT.rds Rscript --vanilla qa/crossval_outtab.R
```

The QA compares all raw counts/ratios/codes, exact native matrix stripes,
fit/sample diagnostics, CSV/Markdown publication, complete workbook styles,
geometry and console/wide cells. It cleans its owned publication scratch on
success or error and saves complete result objects before qa_done() can exit.
Missing native inputs are errors; no numerical acceptance is inferred from
optional dependencies or a successful process alone.

Root adds `export(outtab)`, the unkeyed outtab tt_flat path/keyed refusal, and
additive QA/golden registry hooks separately. Ratio columns are specifications
containing multiple outcome-specific fit occurrences, with no invented model
slot. Future keyed transport requires its own reviewed capability. Source and
native generation remain unaccepted until review, actual affected QA and fresh
final receipt approval; no existing golden input/output is promoted by this WP.
