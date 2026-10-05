/*  make_regtab_h_t2b.do - Stata expectations for the Milestone H group t2b
    tasks (IMPLEMENTATION_PLAN.md, Milestone H, "Group t2b outcome"):
      H21  O1-O5  pseudo R-squared of ologit/logit/probit/poisson/oprobit
                  with offset(): e(ll_0), e(r2_p) and regtab's stats(r2) row
           C1-C2  cloglog (review T2B-09): no e(r2_p), with or without an
                  offset
      H4   S1     streg ..., vce(cluster inst) on survival::lung: the
                  cluster factor G/(G - 1) with G the institutions
      H19  G0-G2  xtgee with scale(2) (independent and exchangeable working
                  correlations) against the default scale
      H5   P1-P2  a covariate named p: regtab drops it under the automatic
                  nointercept and prints it unexponentiated under
                  keepintercept (take_action item 10; R does neither)
    For each case: regtab's csv() sink <id>.csv, every r() value
    <id>_stored.csv (golden_dump_r()), and <id>_e.csv with e(b), the
    standard errors from e(V), and the e() scalars N, N_clust, ll, ll_0,
    r2_p, phi.

    Inputs: offs.dta, lung.dta and gee.dta in the output folder
    (qa/make_regtab_h_t2b_data.R).

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_h_t2b.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR is a `git archive` export of Stata-Tools f42ee9cd
    (tabtools 2.1.12) holding tabtools/ and fvgen/ (default ~/Stata-Tools).
    Read by tests/testthat/test-regtab-h21-offsets.R (O*, C*),
    test-regtab-h4-storage.R (S1), test-regtab-h19-gee-scale.R (G*) and
    test-regtab-h5-roles.R (P*), through helper-regtab-h-t2b.R.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/fvgen"
adopath ++ "`stata_tools'/tabtools"
which regtab
which fvgen
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
global GOLDEN_TABTOOLS_VERSION "`r(version)'"
golden_ado_version fvgen
global GOLDEN_FVGEN_VERSION "`r(version)'"
local out "tests/testthat/fixtures/regtab_h_t2b"

* e(b), standard errors and e() scalars of the last estimation: <id>_e.csv.
capture program drop ht2b_e
program define ht2b_e
    args file
    tempname b V fh
    matrix `b' = e(b)
    matrix `V' = e(V)
    local names : colfullnames `b'
    file open `fh' using "`file'", write text replace
    file write `fh' "term,b,se" _n
    local k = colsof(`b')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        file write `fh' "`nm'," %23.16e (`b'[1,`j']) "," %23.16e (sqrt(`V'[`j',`j'])) _n
    }
    foreach s in N N_clust ll ll_0 r2_p phi {
        file write `fh' "e(`s'),"
        if missing(e(`s')) file write `fh' "." ",." _n
        else file write `fh' %23.16e (e(`s')) ",." _n
    }
    file close `fh'
end

capture program drop ht2b_begin
program define ht2b_begin
    args data
    clear
    capture collect clear
    estimates clear
    capture macro drop TABTOOLS_*
    capture stset, clear
    use "`data'", clear
end

* ======================================================================
* H21: pseudo R-squared with offset()
* ======================================================================
* --- O1 ologit
ht2b_begin "`out'/offs.dta"
quietly collect: ologit yo x, offset(off)
ht2b_e "`out'/O1_e.csv"
regtab, stats(n ll r2) csv("`out'/O1.csv")
mata: golden_dump_r("`out'/O1_stored.csv", "O1")

* --- O2 logit
ht2b_begin "`out'/offs.dta"
quietly collect: logit yb x, offset(off)
ht2b_e "`out'/O2_e.csv"
regtab, stats(n ll r2) csv("`out'/O2.csv")
mata: golden_dump_r("`out'/O2_stored.csv", "O2")

* --- O3 probit
ht2b_begin "`out'/offs.dta"
quietly collect: probit yb x, offset(off)
ht2b_e "`out'/O3_e.csv"
regtab, stats(n ll r2) csv("`out'/O3.csv")
mata: golden_dump_r("`out'/O3_stored.csv", "O3")

* --- O4 poisson
ht2b_begin "`out'/offs.dta"
quietly collect: poisson yc x, offset(lnexpo)
ht2b_e "`out'/O4_e.csv"
regtab, stats(n ll r2) csv("`out'/O4.csv")
mata: golden_dump_r("`out'/O4_stored.csv", "O4")

* --- O5 oprobit
ht2b_begin "`out'/offs.dta"
quietly collect: oprobit yo x, offset(off)
ht2b_e "`out'/O5_e.csv"
regtab, stats(n ll r2) csv("`out'/O5.csv")
mata: golden_dump_r("`out'/O5_stored.csv", "O5")

* ======================================================================
* Review T2B-09: cloglog stores no e(r2_p) (with or without an offset)
* ======================================================================
* --- C1 cloglog
ht2b_begin "`out'/offs.dta"
quietly collect: cloglog yb x
ht2b_e "`out'/C1_e.csv"
regtab, stats(n ll r2) csv("`out'/C1.csv")
mata: golden_dump_r("`out'/C1_stored.csv", "C1")

* --- C2 cloglog with an offset
ht2b_begin "`out'/offs.dta"
quietly collect: cloglog yb x, offset(off)
ht2b_e "`out'/C2_e.csv"
regtab, stats(n ll r2) csv("`out'/C2.csv")
mata: golden_dump_r("`out'/C2_stored.csv", "C2")

* ======================================================================
* H4: streg, vce(cluster): G/(G - 1)
* ======================================================================
* --- S1
ht2b_begin "`out'/lung.dta"
stset time, failure(status)
quietly collect: streg age sex, distribution(weibull) time vce(cluster inst)
ht2b_e "`out'/S1_e.csv"
regtab, stats(n ll) csv("`out'/S1.csv")
mata: golden_dump_r("`out'/S1_stored.csv", "S1")
capture stset, clear

* ======================================================================
* H19: xtgee scale(2)
* ======================================================================
* --- G0 independent, default scale
ht2b_begin "`out'/gee.dta"
xtset id
quietly collect: xtgee y x, family(gaussian) link(identity) corr(independent)
ht2b_e "`out'/G0_e.csv"
regtab, csv("`out'/G0.csv")
mata: golden_dump_r("`out'/G0_stored.csv", "G0")

* --- G1 independent, scale(2)
ht2b_begin "`out'/gee.dta"
xtset id
quietly collect: xtgee y x, family(gaussian) link(identity) corr(independent) scale(2)
ht2b_e "`out'/G1_e.csv"
regtab, csv("`out'/G1.csv")
mata: golden_dump_r("`out'/G1_stored.csv", "G1")

* --- G2 exchangeable, scale(2)
ht2b_begin "`out'/gee.dta"
xtset id
quietly collect: xtgee y x, family(gaussian) link(identity) corr(exchangeable) scale(2)
ht2b_e "`out'/G2_e.csv"
regtab, csv("`out'/G2.csv")
mata: golden_dump_r("`out'/G2_stored.csv", "G2")

* --- G3 exchangeable, default scale (reference for G2's estimates)
ht2b_begin "`out'/gee.dta"
xtset id
quietly collect: xtgee y x, family(gaussian) link(identity) corr(exchangeable)
ht2b_e "`out'/G3_e.csv"

* ======================================================================
* H5: a covariate named p (take_action item 10)
* ======================================================================
* --- P1 automatic nointercept drops it
sysuse auto, clear
capture collect clear
gen p = mpg
quietly collect: logit foreign p weight
regtab, csv("`out'/P1.csv")
mata: golden_dump_r("`out'/P1_stored.csv", "P1")

* --- P2 keepintercept prints it unexponentiated under OR
sysuse auto, clear
capture collect clear
gen p = mpg
quietly collect: logit foreign p weight
regtab, keepintercept csv("`out'/P2.csv")
mata: golden_dump_r("`out'/P2_stored.csv", "P2")
