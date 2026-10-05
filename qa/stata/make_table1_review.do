/*  make_table1_review.do - Stata expectations for the Phase 2 review fixes
    (IMPLEMENTATION_PLAN.md, "Phase 2 review outcome"): display-rounding
    ties (P0-1), empty strings as missing (P0-2), percsign trimming (P0-3),
    empty delimiters (P2-3), and console separators by label text.

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_table1_review.do [STATA_TOOLS_DIR]
    Writes tests/testthat/fixtures/table1_review/: small input .dta files
    and, per call <ID>, the csv() sink <ID>.csv, the listing
    <ID>_console.txt, and the r() results <ID>_stored.csv (golden format).
    tests/testthat/test-table1-review.R reads them.

    The tie datasets are too large to commit (30,000 and 11,100 rows), so
    only their repeating pattern is saved; the R test rebuilds the rows in
    the same order (data order matters: Stata sums in data order).
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
    * rv_run <id> <dataset> <out>: runs program _rv_call on <dataset> (a
    * path relative to the repo root, without .dta; "" = data in memory).
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

**# P0-1: display-rounding ties (Mata sums in double, in data order)

* ties11: 30,000 rows; g has period 2 and d period 4, so rows 1-4 repeat.
clear
set obs 30000
gen g = mod(_n, 2)
gen double a = 0.15
gen double b = 0.35
gen double c = 1.25
gen double d = 0.05 + 0.1*(mod(_n,4)==0) - 0.1*(mod(_n,4)==2)
gen double e = 2.675
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(a contn %5.1f \ b contn %5.1f \ c contn %5.1f \ d contn %5.1f %5.3f \ e contn %5.2f) by(g) total(after) nopvalue csv(`"`1'"')
end
rv_run RV01 "" "`out'"
keep in 1/4
save "`out'/ties11.dta", replace

* ties13: 40 constants (0.05, 0.15, ..., 3.95) in groups of 100, 1,000 and
* 10,000 rows (rows 1-100 g = 0, 101-1100 g = 1, the rest g = 2).
clear
set obs 11100
gen g = cond(_n <= 100, 0, cond(_n <= 1100, 1, 2))
local vl ""
forvalues k = 1/40 {
    gen double c`k' = (2*`k' - 1) * 0.05
    local vl "`vl' c`k' contn %5.1f \"
}
global RV_VL = substr("`vl'", 1, length("`vl'") - 1)
capture program drop _rv_call
program define _rv_call
    table1_tc, vars($RV_VL) by(g) total(after) nopvalue csv(`"`1'"')
end
rv_run RV02 "" "`out'"
keep in 1
drop g
save "`out'/ties13.dta", replace

**# P0-2: an empty string is missing (encode), in by() and in cat variables

clear
input str1 g x str1 s
"a" 1 "u"
"a" 2 "v"
"b" 3 ""
"b" 4 "u"
"" 5 "v"
"" 6 ""
"" 7 "u"
end
save "`out'/blank.dta", replace
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(x contn) by(g) csv(`"`1'"')
end
rv_run RV03 "`out'/blank" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(s cat) by(g) csv(`"`1'"')
end
rv_run RV04 "`out'/blank" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(s cat) by(g) missing csv(`"`1'"')
end
rv_run RV05 "`out'/blank" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, by(g) missingsummary csv(`"`1'"')
end
rv_run RV06 "`out'/blank" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(s cat) missing csv(`"`1'"')
end
rv_run RV07 "`out'/blank" "`out'"

**# P0-3: percsign is trimmed in cat/bin cells only

capture program drop _rv_call
program define _rv_call
    table1_tc, by(trt) vars(stage cat \ female bin \ age contn) percsign(" %") headerperc missingsummary csv(`"`1'"')
end
rv_run RV08 "`agg'" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, by(trt) vars(stage cat \ female bin) percsign("pct ") percent_n missing spacelowpercent total(after) csv(`"`1'"')
end
rv_run RV09 "`agg'" "`out'"

**# P2-3: empty delimiters fall back to the defaults in cells and labels

capture program drop _rv_call
program define _rv_call
    table1_tc, by(trt) vars(age contn \ marker conts \ marker contln) iqrmiddle("") sdleft("") gsdleft("") gsdright("") varlabplus csv(`"`1'"')
end
rv_run RV10 "`agg'" "`out'"

**# Console: list, sepby(factor_sep) separates by label text

clear
set obs 20
gen byte g = mod(_n, 2)
gen double yN = _n^2
gen double x1 = _n
gen double x2 = 2*_n
replace x2 = . in 3
gen byte c1 = mod(_n, 3) + 1
gen byte b1 = mod(_n, 4) < 2
gen byte c2 = mod(_n, 5)
gen byte c3 = mod(_n, 4)
label variable yN "N"
label variable x1 "Marker"
label variable x2 "Marker"
label variable c1 "Group"
label variable b1 "Group"
label variable c2 "Stage"
label variable c3 "Stage"
save "`out'/sep.dta", replace
capture program drop _rv_call
program define _rv_call
    table1_tc, by(g) vars(yN contn \ x1 contn \ x2 contn \ c1 cat \ b1 bin \ c2 cat \ c3 cat) csv(`"`1'"')
end
rv_run RV11 "`out'/sep" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, by(g) vars(yN contn \ x1 contn \ x2 contn \ c1 cat \ b1 bin \ c2 cat \ c3 cat) varlabplus missingsummary csv(`"`1'"')
end
rv_run RV12 "`out'/sep" "`out'"
capture program drop _rv_call
program define _rv_call
    table1_tc, vars(x1 contn \ x2 contn \ c2 cat \ c3 cat) missingsummary csv(`"`1'"')
end
rv_run RV13 "`out'/sep" "`out'"
