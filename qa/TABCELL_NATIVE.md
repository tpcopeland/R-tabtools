# Native tabcell leaf contracts

This additive lane consumes genuine scalar `tabcell` text and immediate `r()`
results. A leaf has no `tt_table` export: scalar dumps are the oracle. A separate
TC006 case publishes generated leaf columns with the actual native `puttab`.
The current ordinary fixtures and their 21 inputs are unchanged.

| Group | Native and installed R control |
|---|---|
| TC001 | All seven scalar forms; direct limits, normal SE, exponentiation then scale, p prose, grouped counts, percentage, event/total, supplied quartiles and log-rate Wald |
| TC002 | Exact binomial zero/full boundaries, zero denominator with/without count, empty replacement, percentage and rate primary masks, zero events under both rate methods, no exposure, separate event and total masking |
| TC003 | Finite estimate with missing limits, zero SE, explicit empty text, literal dollar/backquote/quotes in missing text and separator, cformat alias, native error-code ledger, stale lincom rejection |
| TC004 | Original literal fitting data, coefficient t versus normal inference, genuine lincom with different publication level/eform/scale and all original scalar returns, genuine nlcom, named and positional matrix rows/columns |
| TC005 | All seven generated forms with if/in selection, complete vectors including unselected blank row, missing replacement, native N and N_missing; no invented row-wise native scalar matrices |
| TC006 | Genuine puttab grid, CSV, Markdown and workbook/style parity; immediate generated and parent returns, typed publication provenance, parent stack/merge, CSV replay and explicit leaf/parent keyed-flat refusals |

The R suite uses the public installed package. Its linear-model contrast is
independently evaluated from the original data and fitted covariance; original
native lincom scalars are also captured before publication. The native nonlinear
sum is checked against that same independent contrast value/covariance and its
stored reference distribution. Numeric comparison tolerance is 2e-8 for fitted
and derived native/R parity; literal data, analytic zero-event boundary and linear
contrast controls have tighter stated tolerances. Text, field inventories and
publication grids are exact. For the TC004 normal-inference binomial fit alone,
the estimate and two interval limits use `abs(R - native) <= 2e-8 *
max(1, abs(native))`: Stata 17 and converged R retain a small optimizer difference
(the upper-limit difference is about 2.10e-8). This is a qualified cross-solver
comparison; original native returns are retained, publication text and metadata
remain exact, and no arithmetic or other scenario tolerance changes. The existing
independent method lane remains in `qa/crossval_tabcell.R`.

## Reviewed native capture and authentication

Root runs `qa/stata/make_tabcell_native.do` from the isolated source root, with
arguments `(pinned_ado_directory, new_owned_output_directory)`. It loads the
unchanged approved `qa/stata/golden_helpers.do`. The recipe verifies tabtools
2.5.1 and that tabcell, its renderer and puttab resolve inside that ado directory.
Every leaf dump is the next command after tabcell; `golden_dump_r()` reads r()
through Mata without replacing it. The recipe never calls the legacy generator,
installs anything, or modifies the original input fixtures. Output is external.

After reviewed successful execution, root supplies `SOURCE.csv` with columns
`key,value`, including `native_source_commit` equal to
`712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129`, and `ARTIFACTS.csv` with columns
`file,md5` for authenticated relative artifacts. Authentication must cover every
control named by this suite, including all scalar snapshots, original lincom/
nlcom inputs, fitting/vector data, generated vectors, native error ledger,
TC006 parent sinks and runtime record. Root's external receipt binds the source
recipe/helper/source hashes, native execution and complete output inventory.
The R consumer independently checks artifact MD5s and the pinned source/runtime
record. Each requested control must be in the authenticated inventory; an absent
or modified oracle fails rather than skips. The recipe's COMPLETE marker alone
is not acceptance evidence.

Set `TABTOOLS_TABCELL_NATIVE_DIR` to that authenticated directory. Root registers
`qa/crossval_tabcell_native.R` additively in the installed Stata lane after fresh
source review. The suite does not find or execute Stata. The parent style reader
uses `tests/testthat/helper-golden.R`; its external reader dependencies must be
installed in the isolated QA library. Missing readers are failures. R output is
confined to a uniquely allocated `withr::local_tempdir()` and automatically
removed after the parent block, including failure.

## Source and return distinctions

At native pin 712044f8, `tabcell.ado` lines 166–259 extract coefficient/lincom/
nlcom sources; 261–348 select stored matrix limits and distinguish direct limits
from normal SE inference. Lines 471–512 define formats and literal strings;
524–583 implement the generated path and its four returns. Lines 664–723 define
scalar text/form/missing, form-owned numerical fields, source/citype macros and
all original lincom scalar names. `tabcell.sthlp` Stored results (405–452) states
that scalar and generated return shapes differ. `_tabcell_render.ado` lines
84–187 implement missing/domain boundaries, 197–287 render percentages and exact
binomial limits, 288–343 render rates/masks/event-total cells, and 345–352 define
N as selected rows minus missing replacements (masked rows still count).

Native est can return a finite estimate while a missing interval causes the
whole published cell to use replacement text. That estimate belongs to the
native return projection; publication companions are missing. Masked percentage
and rate results return missing pct/rate/limits and retain no reconstructive
protected inputs in R leaf provenance. These tests do not extend the raw-matrix
D2 contracts of regtab/outtab/stratetab/ratetab to a protected leaf.

Native macro storage and ambient last-result selection are native-only. R passes
an explicit fitted source or typed contrast. The stale-lincom rc301 control tests
actual native source ownership and does not invent an ambient R lincom API.
Native's decimal-comma/comma-separator warning intentionally differs from the
shared P.2 R refusal, tested as an R-only boundary. Parent merge uses explicit
manual rowid keys, never fitted model identities.

Source authoring and R syntax parsing are the only checks performed when these
files are frozen. Native capture, installed QA and sink parity remain pending
fresh independent source review and root execution.
