/*  make_table1_review_p3.do - Stata expectations for the Phase 2 review
    fixes carried into the Phase 3 paths (wt(), fweight, wtcompare,
    smallcells): display ties under weights (Mata sums in double, in data
    order), "" as missing, percsign trimming, empty delimiters, console
    separators by label text, and string by() codes numbered over the whole
    dataset.

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_table1_review_p3.do [STATA_TOOLS_DIR]
    Writes RW* expectations and inputs into
    tests/testthat/fixtures/table1_review/ (csv() sink, listing, r()).
    tests/testthat/test-table1-review-p3.R reads them.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
set linesize 255
do qa/stata/golden_helpers.do
* Record the tabtools version that resolves (desctab.ado header).
golden_ado_version desctab
global GOLDEN_TABTOOLS_VERSION "`r(version)'"
global GOLDEN_FVGEN_VERSION ""
local out "tests/testthat/fixtures/table1_review"
capture mkdir "`out'"

capture program drop rv_run
program define rv_run
    args id data out
    if `"`data'"' != "" use "`data'.dta", clear
    capture log close rv
    log using "`out'/`id'_console.txt", text replace nomsg name(rv)
    _rv_call "`out'/`id'.csv"
    * Dump r() before closing the log: -log close- clears r().
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    capture log close rv
end

local agg "tests/testthat/fixtures/table1_qa/agg"

**# Display ties under weights (30,000 rows, period 12)

clear
set obs 30000
gen g = mod(_n, 2)
gen double a = 0.15
gen double c = 1.25
gen double d = 0.05 + 0.1*(mod(_n,4)==0) - 0.1*(mod(_n,4)==2)
gen double e = 2.675
gen double w = 0.5 + 0.5*(mod(_n,4) < 2)
gen long fw = 1 + mod(_n, 3)
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(a contn %5.1f \ c contn %5.1f \ d contn %5.1f %5.3f \ e contn %5.2f) by(g) total(after) wt(w) csv(`"`1'"')
end
rv_run RW01 "" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc [fweight=fw], vars(a contn %5.1f \ c contn %5.1f \ d contn %5.1f %5.3f \ e contn %5.2f) by(g) total(after) nopvalue csv(`"`1'"')
end
rv_run RW02 "" "`out'"
keep in 1/12
save "`out'/tiesw.dta", replace

**# "" is missing under wt() and fweight

clear
input str1 g x str1 s double w long fw
"a" 1 "u" 1   1
"a" 2 "v" 2   2
"b" 3 ""  1   1
"b" 4 "u" 0.5 3
"" 5 "v"  1   1
"" 6 ""   1   2
"" 7 "u"  1   1
"a" 8 ""  1.5 1
end
save "`out'/blankw.dta", replace
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(x contn \ s cat) by(g) wt(w) wtn missing missingsummary csv(`"`1'"')
end
rv_run RW03 "`out'/blankw" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc [fweight=fw], vars(x contn \ s cat) by(g) missingsummary csv(`"`1'"')
end
rv_run RW04 "`out'/blankw" "`out'"

**# percsign and empty delimiters under weights

capture program drop _rv_call
program define _rv_call
    table1_tc, by(trt) vars(stage cat \ female bin \ age contn) wt(w) wtn percsign(" %") headerperc missingsummary csv(`"`1'"')
end
rv_run RW05 "`agg'" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, by(trt) vars(stage cat \ female bin \ age contn \ marker conts \ marker contln) wt(w) wtcompare smd percsign(" %") iqrmiddle("") sdleft("") gsdleft("") gsdright("") varlabplus csv(`"`1'"')
end
rv_run RW06 "`agg'" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc [fweight=fw], by(trt) vars(stage cat \ female bin) percsign(" %") headerperc missingsummary smallcells(3) csv(`"`1'"')
end
rv_run RW07 "`agg'" "`out'"

**# Console separators under weights (ESS row, same-label variables)

capture program drop _rv_call
program define _rv_call
    table1_tc, by(g) vars(x1 contn \ x2 contn \ c2 cat \ c3 cat) wt(w) wtn missingsummary csv(`"`1'"')
end
use "`out'/sep.dta", clear
gen double w = 1 + mod(_n, 3) / 2
rv_run RW08 "" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, by(g) vars(x1 contn \ x2 contn \ c1 cat \ b1 bin) wt(w) wtcompare smd csv(`"`1'"')
end
rv_run RW09 "" "`out'"
save "`out'/sepw.dta", replace

**# String by(): codes over the whole dataset (records dropped by weights)

clear
input str1 g double(w x) byte c long fw
"a" 0 1 0 0
"a" 0 2 1 0
"b" 1 3 0 1
"b" 1 4 1 2
"b" 1 5 1 1
"c" 2 6 0 3
"c" 1 7 1 1
"c" 1 8 0 1
end
save "`out'/strby.dta", replace
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(x contn \ c bin) by(g) wt(w) wtn smallcells(3) csv(`"`1'"')
end
rv_run RW10 "`out'/strby" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc [fweight=fw], vars(x contn \ c bin) by(g) smallcells(3) nopvalue csv(`"`1'"')
end
rv_run RW11 "`out'/strby" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(x contn \ c bin) by(g) wt(w) wtcompare smallcells(3) csv(`"`1'"')
end
rv_run RW12 "`out'/strby" "`out'"
