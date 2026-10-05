/*  make_regtab_review.do - Stata expectations for the Phase 4 review fixes
    (IMPLEMENTATION_PLAN.md, "Phase 4 review outcome") that are not golden
    scenarios:
      oim_se.csv      e(b) and sqrt(diag(e(V))) (vce(oim)) for probit,
                      cloglog, glm poisson identity, glm gamma log (tight
                      convergence) and nbreg (P0-1), written %23.16e
      eplot_R52.csv   the eplotframe() of golden R52 (R12 model, compact
                      nopvalue): rows and full-precision numbers (P1-2 M31,
                      P1-3)
      eplot_R52_chars.csv  its _dta characteristics

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_review.do [STATA_TOOLS_DIR]
    (default ~/Stata-Tools; pass a `git archive` export of a tagged commit
    when that working tree has uncommitted edits).
    tests/testthat/test-regtab-review.R reads them.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
which regtab
set linesize 255
local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_review"
capture mkdir "`out'"
local tight "tolerance(1e-14) ltolerance(1e-14) nrtolerance(1e-14)"

capture program drop dumpb
program define dumpb
    args fh tag
    tempname b V
    matrix `b' = e(b)
    matrix `V' = e(V)
    local names : colfullnames `b'
    local k = colsof(`b')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        file write `fh' "`tag',`nm'," %23.16e (`b'[1,`j']) "," %23.16e (sqrt(`V'[`j',`j'])) _n
    }
end

tempname fh
file open `fh' using "`out'/oim_se.csv", write replace
file write `fh' "model,term,b,se" _n
use "`fx'/auto.dta", clear
probit foreign mpg weight, `tight'
dumpb `fh' probit
cloglog foreign mpg weight, `tight'
dumpb `fh' cloglog
glm price mpg weight, family(gamma) link(log) `tight'
dumpb `fh' gamma_log
use "`fx'/nbsim.dta", clear
glm y x1 i.grp, family(poisson) link(identity) `tight'
dumpb `fh' poisson_identity
nbreg y x1 i.grp, `tight'
dumpb `fh' nbreg
file close `fh'

use "`fx'/auto.dta", clear
collect clear
quietly collect: regress price mpg mpg_dup i.foreign##i.rep78
regtab, eplotframe(ep, replace) models("Constrained") compact nopvalue
frame ep {
    format estimate ll ul pvalue %23.16e
    export delimited label estimate ll ul pvalue model model_label rowtype section source_row ///
        using "`out'/eplot_R52.csv", replace datafmt
    local chars : char _dta[]
    tempname ch
    file open `ch' using "`out'/eplot_R52_chars.csv", write replace
    file write `ch' "name,value" _n
    foreach c of local chars {
        local v : char _dta[`c']
        file write `ch' `"`c',"`v'""' _n
    }
    file close `ch'
}
