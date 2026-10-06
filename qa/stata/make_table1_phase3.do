/*  make_table1_phase3.do - Stata expectations for table1_tc weighting and
    small-cell wiring beyond the golden scenarios (plan tasks 3.1-3.6):
    option combinations, the pipeline cases of qa/test_smallcells.do, and
    edge cases (suppressed N with headerperc, no by(), wtcompare totals).

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_table1_phase3.do [STATA_TOOLS_DIR]
    Writes tests/testthat/fixtures/table1_phase3/: the input .dta files and,
    per call <ID>, the csv() sink <ID>.csv, the listing <ID>_console.txt, the
    r() results <ID>_stored.csv (golden format), and for W3/S1 sheet <ID>
    of phase3.xlsx. tests/testthat/test-table1-phase3-qa.R reads them.
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
local out "tests/testthat/fixtures/table1_phase3"
capture mkdir "`out'"
capture erase "`out'/phase3.xlsx"

capture program drop p3_run
program define p3_run
    * p3_run <id> <dataset>: runs program _p3_call on <dataset>.
    * <dataset> is a path relative to the repo root, without .dta.
    args id data out
    use "`data'.dta", clear
    capture log close p3
    log using "`out'/`id'_console.txt", text replace nomsg name(p3)
    _p3_call "`out'/`id'.csv"
    * Dump r() before closing the log: -log close- clears r() (found in the
    * Phase 2 review; the first run wrote empty _stored.csv files).
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    capture log close p3
end

**# Datasets

* agg (tests/testthat/fixtures/table1_qa), cohort and auto (golden fixtures)
* are read in place.
*
* sg3.dta and pw.dta (Phase 3 review probes K7 and I5) were written by R
* (haven::write_dta, version 15), and are read in place too:
*   sg3: set.seed(77); n <- c(40, 30, 3); g <- rep(1:3, n); N <- length(g)
*        arm = labelled(g, c(Placebo = 1, Drug = 2, Rescue = 3)),
*        x = round(rnorm(N, 60, 8), 1) (label "X"; x[c(5, 44, 72)] <- NA),
*        k = sample(1:3, N, TRUE, prob = c(.6, .3, .1)) (label "K";
*        k[c(2, 50)] <- NA), b = rbinom(N, 1, .2) (label "B"),
*        w = round(runif(N, .3, 3), 4), drawn in that order (x, k, b, w;
*        the review's mkdata2.R, which also wrote columns not used here).
*   pw:  the review's mkdata.R (set.seed(20260925), 240 records), columns
*        g3 x y b c fw only.
local agg "tests/testthat/fixtures/table1_qa/agg"
local cohort "tests/testthat/golden/fixtures/cohort"
local auto "tests/testthat/golden/fixtures/auto"

* test_smallcells.do _sc_build_2x2
clear
set obs 4
generate byte group = floor((_n - 1) / 2)
generate byte category = mod(_n - 1, 2)
generate int frequency = cond(_n == 1, 2, cond(_n == 2, 8, cond(_n == 3, 6, 4)))
expand frequency
drop frequency
save "`out'/sc2x2.dta", replace

* redundant complementary margins
clear
input byte group byte category int frequency
    0 1 1
    1 0 1
    1 1 4
end
expand frequency
drop frequency
save "`out'/scirr.dta", replace

* continuous contributing N
clear
input byte(group) double value byte category
    0 10 0
    0 12 1
    0 .  1
    0 .a 0
    1 20 0
    1 21 0
    1 22 0
    1 23 1
    1 24 1
    1 25 1
end
save "`out'/sccont.dta", replace

* wt() records
clear
input byte(group category) double wt long fw
    0 0 10 2
    0 1  1 6
    1 0  1 6
    1 1 10 4
end
expand 2
replace fw = 1
save "`out'/scwt.dta", replace

* fweight frequencies
clear
input byte(group category) long fw
    0 0 2
    0 1 8
    1 0 6
    1 1 4
end
save "`out'/scfw.dta", replace

* wtcompare composition
clear
input byte(group category) double wt int frequency
    0 0 1.0 2
    0 1 1.5 8
    0 . 0.8 1
    1 0 1.2 6
    1 1 0.9 4
    1 . 1.1 2
end
expand frequency
drop frequency
save "`out'/sccomp.dta", replace

* missing total(before) catrowperc smallcells(3)
clear
input byte(group category) int frequency
    0 0 2
    0 1 8
    0 . 5
    1 0 6
    1 1 4
    1 . 5
end
expand frequency
drop frequency
save "`out'/scmiss.dta", replace

* all-missing continuous
clear
set obs 6
gen byte group = _n > 2
gen double allmissing = .
save "`out'/scallmiss.dta", replace

* threshold above N
clear
input byte group byte category int frequency
    0 0 2
    0 1 3
    1 0 3
    1 1 2
end
expand frequency
drop frequency
save "`out'/schighk.dta", replace

* binary with missing values and slashN denominators
clear
input byte(group flag) int frequency
    0 1 3
    0 0 9
    0 . 2
    1 1 7
    1 0 8
    1 . 1
end
expand frequency
drop frequency
label variable flag "Flag"
save "`out'/scbin.dta", replace


**# Weights

capture program drop _p3_call
program define _p3_call
    table1_tc, by(trt) vars(age contn %6.2f \ marker contln %6.2f \ marker conts %6.1f \ female bin \ stage cat) wt(w) wtn smd missing missingsummary total(before) headerperc csv(`"`1'"')
end
p3_run W1 "`agg'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(trt) vars(age contn \ female bin \ stage cat) wt(w) percent_n catrowperc slashN total(after) varlabplus csv(`"`1'"')
end
p3_run W2 "`agg'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(trt) vars(age contn \ marker conts \ female bin \ stage cat) wt(w) wtcompare smd total(after) missingsummary headerperc csv(`"`1'"') xlsx("tests/testthat/fixtures/table1_phase3/phase3.xlsx") sheet("W3")
end
p3_run W3 "`agg'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(trt) vars(age contn \ female bin \ stage cat) wt(w) wtcompare total(before) wtn varlabplus csv(`"`1'"')
end
p3_run W4 "`agg'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(treated) vars(index_age conts \ lab_value contln %5.2f \ bmi contn %5.1f \ cost_sek conts) wt(iptw) smd total(after) csv(`"`1'"')
end
p3_run W5 "`cohort'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(education) vars(index_age contn %5.1f \ female bin \ income_quintile cat \ bmi conts %5.1f) wt(iptw) wtn smd csv(`"`1'"')
end
p3_run W6 "`cohort'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc [fweight=fw], by(trt) vars(age contn %6.2f \ marker contln %6.2f \ marker conts %6.1f \ female bin \ stage cat) smd total(after) missingsummary catrowperc slashN test statistic csv(`"`1'"')
end
p3_run F1 "`agg'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc index_age bmi lab_value education [fweight=fw], by(treated) smd csv(`"`1'"')
end
p3_run F2 "`cohort'" "`out'"

**# Small cells

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) total(after) test statistic smd smallcells(5) csv(`"`1'"') xlsx("tests/testthat/fixtures/table1_phase3/phase3.xlsx") sheet("S1") title("Synthetic small cells")
end
p3_run S1 "`out'/sc2x2" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) total(after) smallcells(5) csv(`"`1'"')
end
p3_run S2 "`out'/scirr" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(group) vars(value contn \ category cat) missingsummary test statistic smd smallcells(5) csv(`"`1'"')
end
p3_run S3 "`out'/sccont" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) wt(wt) wtn smallcells(5) csv(`"`1'"')
end
p3_run S4 "`out'/scwt" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category [fweight=fw], by(group) vars(category cat) total(after) smallcells(5) csv(`"`1'"')
end
p3_run S5 "`out'/scfw" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) wt(wt) wtcompare wtn total(after) slashN headerperc missingsummary smallcells(5) csv(`"`1'"')
end
p3_run S6 "`out'/sccomp" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) missing total(before) catrowperc smallcells(3) csv(`"`1'"')
end
p3_run S7 "`out'/scmiss" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(group) vars(allmissing contn) missingsummary percent_n smallcells(3) csv(`"`1'"')
end
p3_run S8 "`out'/scallmiss" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) smallcells(10) csv(`"`1'"')
end
p3_run S9 "`out'/schighk" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(rep78) vars(price contn \ foreign bin) headerperc smallcells(5) csv(`"`1'"')
end
p3_run S10 "`auto'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(rep78) vars(price contn \ foreign bin) headerperc total(after) smallcells(5) csv(`"`1'"')
end
* tabtools 2.1.17+ refuses S11 (r(498): the shared group N cannot be
* withheld). Run it under capture so regeneration continues; the R side
* asserts the refusal (test-table1-phase3-qa.R) and reads no S11 files.
capture noisily p3_run S11 "`auto'" "`out'"
local _s11_rc = _rc
capture log close p3
display as text "S11 rc = `_s11_rc' (498 expected under tabtools >= 2.1.17)"

capture program drop _p3_call
program define _p3_call
    table1_tc, vars(rep78 cat \ foreign bin \ mpg conts) smallcells(5) csv(`"`1'"')
end
p3_run S12 "`auto'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(foreign) vars(rep78 cat \ price contn) catrowperc slashN total(after) missingsummary smallcells(5) csv(`"`1'"')
end
p3_run S13 "`auto'" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc category, by(group) vars(category cat) extraspace test smd smallcells(5) footnote("Source: synthetic.") csv(`"`1'"')
end
p3_run S14 "`out'/sc2x2" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(group) vars(flag bin) slashN missingsummary total(before) smallcells(5) csv(`"`1'"')
end
p3_run S15 "`out'/scbin" "`out'"

capture program drop _p3_call
program define _p3_call
    table1_tc, by(group) vars(flag bin) slashN percent_n smallcells(5) csv(`"`1'"')
end
p3_run S16 "`out'/scbin" "`out'"

**# Phase 3 review regression cross-checks

* S17 (review P1-2, probe K7): wtcompare + smallcells with a coded group N
* (a 3-record group), so the crude and weighted suppression codes differ:
* the ESS row is Suppressed (code 3) in the weighted block only and blank
* (code 0) in the crude block.
capture program drop _p3_call
program define _p3_call
    table1_tc, by(arm) vars(x contn \ k cat \ b bin) wt(w) wtcompare wtn smallcells(4) catrowperc slashN smd csv(`"`1'"')
end
p3_run S17 "tests/testthat/fixtures/table1_phase3/sg3" "`out'"

* S18 (review P2-4, probe I5): fweight, three groups, conts (kwallis on the
* expanded, tied data; Stata's float rank sums).
capture program drop _p3_call
program define _p3_call
    table1_tc [fweight=fw], by(g3) vars(x contn \ y conts \ y contln \ b bin \ c cat) missing smd total(before) headerperc percent_n csv(`"`1'"')
end
p3_run S18 "tests/testthat/fixtures/table1_phase3/pw" "`out'"
