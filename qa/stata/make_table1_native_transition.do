/* Narrow WP3G native probes. Existing input files are read without rewriting;
   output is an owned external directory. No legacy producer is modified. */
version 17.0
args stata_ado out
if `"`stata_ado'"' == "" | `"`out'"' == "" exit 198
adopath ++ "`stata_ado'"
discard
quietly tabtools
if "`r(version)'" != "2.5.1" exit 9
findfile table1_tc.ado
if strpos("`r(fn)'", "`stata_ado'") != 1 exit 9
set rng mt64
set linesize 255
set more off
do qa/stata/golden_helpers.do
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION ""

capture program drop wp3g_transition
program define wp3g_transition
    args id out
    log using "`out'/`id'_console.txt", text replace nomsg name(probe)
    if "`id'" == "S17" {
        table1_tc, by(arm) vars(x contn \ k cat \ b bin) wt(w) wtcompare wtn ///
            smallcells(4) catrowperc slashN smd csv("`out'/S17.csv") ///
            markdown("`out'/S17.md") xlsx("`out'/native.xlsx") sheet("S17")
    }
    else if "`id'" == "RW07" {
        table1_tc [fweight=fw], by(trt) vars(stage cat \ female bin) percsign(" %") ///
            headerperc missingsummary smallcells(3) csv("`out'/RW07.csv") ///
            markdown("`out'/RW07.md") xlsx("`out'/native.xlsx") sheet("RW07")
    }
    else if "`id'" == "NUMBY" {
        table1_tc, by(g) vars(x contn) nopvalue csv("`out'/NUMBY.csv") ///
            markdown("`out'/NUMBY.md") xlsx("`out'/native.xlsx") sheet("NUMBY")
    }
    else if "`id'" == "OVERFLOW" {
        table1_tc, by(g) vars(x contn \ b bin) wt(w) smd csv("`out'/OVERFLOW.csv") ///
            markdown("`out'/OVERFLOW.md") xlsx("`out'/native.xlsx") sheet("OVERFLOW")
    }
    else exit 198
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    log close probe
end

use tests/testthat/fixtures/table1_phase3/sg3.dta, clear
wp3g_transition S17 "`out'"
use tests/testthat/fixtures/table1_qa/agg.dta, clear
wp3g_transition RW07 "`out'"

file open refusal using "`out'/REFUSALS.csv", write text replace
file write refusal "case,rc" _n
use tests/testthat/golden/fixtures/auto.dta, clear
capture noisily table1_tc, by(rep78) vars(price contn \ foreign bin) ///
    headerperc total(after) smallcells(5)
local rc = _rc
file write refusal "S11,`rc'" _n
assert `rc' == 498
use tests/testthat/fixtures/table1_review/strby.dta, clear
capture noisily table1_tc [fweight=fw], vars(x contn \ c bin) by(g) ///
    smallcells(3) nopvalue
local rc = _rc
file write refusal "RW11,`rc'" _n
assert `rc' == 498
capture noisily table1_tc, vars(x contn \ c bin) by(g) wt(w) wtcompare smallcells(3)
local rc = _rc
file write refusal "RW12,`rc'" _n
assert `rc' == 198
file close refusal

* Independent literal probe: numeric by() header and the exact known overflow.
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 1e200
gen byte b = mod(_n, 2)
wp3g_transition NUMBY "`out'"
wp3g_transition OVERFLOW "`out'"
file open runtime using "`out'/runtime.txt", write text replace
file write runtime "`c(stata_version)'" _n "`c(rng_current)'" _n
file close runtime
display as result "RESULT: table1_native_transition tests=7 pass=7 fail=0"
