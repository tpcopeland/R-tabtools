# Survival implementation and native acceptance

This command uses explicit exit/event/entry/subject/frequency inputs. It implements
product-limit KM, native Greenwood and log-log bands, integrated squared-tail
Greenwood RMST, independent-group normal contrasts and native hypergeometric
log-rank covariance. Recurrent/overlapping/group-changing/frequency-changing
subjects refuse with a documented inference class. Subject IDs deduplicate risk
and denominator counts; they do not imply robust IJ inference.

Source grounding: pinned `survtab.ado`123–153/333–443/503–579/1219–1258;
held2025 sts p17; held stci median/estimand text; actual installed stci7.4.3
software467–480 corroborates squared-tail variance despite the rendered stci
manual p10 discrepancy. Royston/Parmar2013 eq1 and6–7 ground the estimand and
independent-arm contrast. Pinned survival3.8-6 PDF10–12/20 grounds a separate
recursive integrated IJ diagnostic, not a replacement for native Greenwood.
No GPL reference source is copied; no new literature acquisition occurred.

Root's actual five-case endpoint capture (`wp-6c/native-capture-v1`) establishes
first .5 plateau crossing at2, one-sided lower1/uppermissing, single-subject
terminal median missing, declared-fweight stci RC101 captured as missing median,
missing-event censoring and missing-group no-failure isolation. The root's
actual SV001–SV011 capture (`wp-6c/native-sv-v1`) additionally establishes
two-subject distinct-event median1, tied-terminal median2 and literal near-double
stripes t.3/t.5. At S=0 the mathematical event-grid SE/bands are missing;
installed sts.ado406 forward-fills the generated queried SE from a prior
eligible value. SV001's terminal probability-contrast interval confirms that
projection. A first terminal event has no prior SE. All-censored curves lack
queried native SE/bands after an eligible exit; before any eligible exit the
wrapper initializes S=1/SE=0. RMST retains squared-tail covariance rather than
using this query-only forward-fill. These are actual root captures, not owner
execution or final acceptance.

The captured original11-case recipe SHA256 is
7cd2bf55118c5a132a7c7c3ce9c713b10f6da1a2a2f2f3c73929c777b764ab4e.
Its outputs contain no user annotation. The final producer adds the literal
Native annotation. and SV012 float-group case; neither change is claimed
executed by those original receipts. Root will independently review/capture
these small deltas before full-surface QA against the final recipe.

`tests/testthat/test-survtab.R` contains twelve native-free blocks: hand
rectangles plus dense covariance (mu9/4,V11/64), full frequency replication
(mu45/22,V461/10648), delayed left-open boundaries (mu5/2,V1/8), split-ID
invariance and unsupported mutations, distinct floating events, scalar log-rank
49/17 and tied finite-population correction, missing-group isolation, reverse
contrast direction, support/median endpoints, input refusals, literal plain
frames, and complete one-header layout geometry.

`qa/validation_survtab.R` runs installed public API against actual survival
survfit with explicit robustFALSE/timefixFALSE/stype1/ctype1, S-scale summary
SE, full event/risk grids and independent dense covariance integration;
survdiff is used only in its supported ordinary unweighted lane. Recursive
IJ is separately named/retained, with no delayed-entry Greenwood equality
claim. Optional survRM2 rmst2 checks both arms and the reversed package contrast
orientation; missing oracle is reported SKIP/INCOMPLETE, never PASS.

`qa/stata/make_survtab_native.do` creates only SV001–SV012 and new command-owned
input snapshots. It never calls the legacy generator or reads/writes any of
272 existing scenario inputs/outputs. Every case produces literal CSV/MD,
full stored results, console and one workbook; runtime is captured inside the
producer's17/mt64 scope. Actual support and probability-weight refusal codes
are separately retained. `qa/data/survtab_native_pin.csv` binds all73ADO/17help
files against exact Git objects712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129.

Root reviews the frozen producer, executes in a private source archive/fresh
owned output directory, authenticates all output and installed sts/stci/
logrank/stcox helper bytes, and supplies complete ARTIFACTS.csv (file,md5)
and SOURCE.csv (key,value; native_source_commit). Then set
TABTOOLS_SURVTAB_NATIVE_DIR for `qa/crossval_survtab.R`. The latter independently
constructs R inputs (including literal IEEE float group values), compares every
raw scalar/matrix/identity and all sink body cells/geometry without masks.
Full R automatic paragraphs and full native console notes are independently
declared/asserted before removing only their annotation boundary. Existing
native footer styles guard each R paragraph; native artifacts are never redrawn.

Root-owned integration hooks: export(survtab); xlsx_rules survtab validation
plus writer/interop dispatch; ordinary unkeyed flat adapter with exact header,
command,frame,sample_accounting,composition_exportTRUE attributes and classed
explicit-keyed refusal; additive QA registry/index; sparse survtab footer
serialization in the existing integrity helper after actual children are
validated. Cross-QA depends on merged reviewed3G footnote helpers; no fallback
copy exists in this worktree.

Use source freeze → fresh independent review → root installed affected QA and
native authentication → fresh final execution review → exact commit. No
acceptance or native execution by the owner is claimed. Both QA scripts save
full results/counts/errors to explicitly supplied external result paths before
generic qa_done can exit; owned scratch is cleaned on all exits. Use
TABTOOLS_SURVTAB_VALIDATION_RESULTS / TABTOOLS_SURVTAB_NATIVE_RESULTS for receipts.
