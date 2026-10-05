/*  make_regtab_union.do - how Stata's regtab joins factor levels across
    models (review of Milestone H group t2a, P0-3 and P1-1).

    Stata joins levels by value code: a model with another base level
    (ib3.), a subset that lacks a level, and an encoded string missing a
    value in one model are rendered with each model's reference on its own
    level's row and a missing level empty. R joins by the level itself,
    its analogue of the value code (R/regtab_union_check.R); the cells are
    compared in tests/testthat/test-regtab-hardening.R ("review P0-3 ...").

    Data: Stata's sysuse auto, rep78 given text value labels (so the R
    factor has text levels and positional codes) and an encoded string.

    Output: tests/testthat/fixtures/regtab_union/<case>.csv (regtab's csv()
    sink).

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_union.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR holds tabtools/ (a `git archive` export of
    Stata-Tools 1255176d, tabtools 2.1.14; default ~/Stata-Tools).
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
which regtab
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
local out "tests/testthat/fixtures/regtab_union"
capture mkdir "`out'"

sysuse auto, clear
drop if missing(rep78)
label define replab 1 "poor" 2 "fair" 3 "avg" 4 "good" 5 "exc"
label values rep78 replab
generate str3 grp = cond(rep78 < 3, "lo", cond(rep78 == 3, "mid", "hi"))
encode grp, generate(grpc)

* A: another base level in model 2 (R: relevel()).
collect clear
quietly collect: regress price i.rep78 mpg
quietly collect: regress price ib3.rep78 mpg
regtab, csv("`out'/A_relevel.csv")
* B: model 2 lacks level 2 (R: lm(subset =) drops the unused level).
collect clear
quietly collect: regress price i.rep78 mpg
quietly collect: regress price i.rep78 mpg if rep78 != 2
regtab, csv("`out'/B_subset.csv")
* C: an encoded string; model 2 lacks "hi" (R: a character predictor).
collect clear
quietly collect: regress price i.grpc mpg
quietly collect: regress price i.grpc mpg if grp != "hi"
regtab, csv("`out'/C_encode_subset.csv")
* D: the subset model first.
collect clear
quietly collect: regress price i.rep78 mpg if rep78 != 2
quietly collect: regress price i.rep78 mpg
regtab, csv("`out'/D_subset_first.csv")
