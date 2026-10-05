/*  make_comptab_forest.do - Stata's comptab/hrcomptab eplotframe() for the
    forest data of comptab() (Phase 7c review; no golden covers it)

    Three composites, each source made with regtab's frame() and
    eplotframe(), and the composite's eplotframe() exported:
      PB  hrcomptab: golden C04's rate frame and binary/dose Cox frames,
          rows(1 \ 3/5), outcomemap()
      PC  comptab: golden C05's two-model frames, rows(2 1 \ 5) (typed out
          of order), section()
      PE  hrcomptab: golden C09's rate blocks (one outcome, 90%), the dose
          model with ib2. (reference on Medium dose), rownames(), reflabel()

    Output: tests/testthat/fixtures/comptab_forest/<case>.csv, the numbers
    at %21.17g; read by test-golden-comptab.R ("forest data ...").

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_comptab_forest.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR holds tabtools/ (a `git archive` export of
    Stata-Tools 1255176d, tabtools 2.1.14; default ~/Stata-Tools).
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
which comptab
set more off
set varabbrev off
set linesize 255
set rng mt64
run "qa/stata/golden_helpers.do"
golden_ado_version comptab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
local root `"`c(pwd)'"'
local out "`root'/tests/testthat/fixtures/comptab_forest"
capture mkdir "`out'"
local work "`root'/qa/.stata-work"
capture mkdir "`work'"
run "qa/stata/golden_stratetab_blocks.do"

capture program drop _ep_export
program define _ep_export
    args frame file
    frame `frame' {
        format estimate ll ul pvalue %21.17g
        export delimited using "`file'", replace
    }
end

* The rate blocks are read relative to tests/testthat/golden.
quietly cd "`root'/tests/testthat/golden"

**# PB: golden C04's frames
golden_strate_blocks C04 "`work'/PB"
stratetab, using("`work'/PB_1" "`work'/PB_2" "`work'/PB_3" "`work'/PB_4" "`work'/PB_5" "`work'/PB_6") ///
    outcomes(3) frame(_pb_rates, replace) ///
    outlabels("Sustained EDSS 4" \ "Sustained EDSS 6" \ "Recurring Relapse") ///
    outcomeids(edss4 \ edss6 \ relapse) explabels("Any HRT" \ "Estrogen Dose")
foreach d in bin dose {
    use "fixtures/hrt_`d'.dta", clear
    local x = cond("`d'" == "bin", "hrt", "i.dosecat")
    collect clear
    foreach y in edss4 edss6 relapse {
        stset time, failure(`y') id(id)
        quietly collect: stcox `x' c.age i.female i.education, nolog
    }
    regtab, frame(_pb_`d', replace) eplotframe(_pb_`d'_ep, replace) noint coef("HR") ///
        models(edss4 \ edss6 \ relapse)
}
hrcomptab _pb_rates, modelframes(_pb_bin _pb_dose) rows(1 \ 3/5) ///
    outcomemap(edss4 \ edss6 \ relapse) eplotframe(_pb_ep, replace)
_ep_export _pb_ep "`out'/PB.csv"

**# PC: golden C05's frames, rows typed out of order
use "fixtures/cohort.dta", clear
stset follow_up, failure(cv_event)
collect clear
quietly collect: stcox treated female, nolog
quietly collect: stcox treated female index_age diabetes, nolog
regtab, frame(_pc_a, replace) eplotframe(_pc_a_ep, replace) coef("HR") noint models("Crude \ Adjusted")
collect clear
quietly collect: stcox treated female index_age diabetes hypertension, nolog
quietly collect: stcox treated female hypertension, nolog
regtab, frame(_pc_b, replace) eplotframe(_pc_b_ep, replace) coef("HR") noint models("Adjusted \ Crude")
comptab _pc_a _pc_b, rows(2 1 \ 5) section("Frame A" \ "Frame B") eplotframe(_pc_ep, replace)
_ep_export _pc_ep "`out'/PC.csv"

**# PE: golden C09's rate blocks, the dose model with ib2.
golden_strate_blocks C09 "`work'/PE"
stratetab, using("`work'/PE_1" "`work'/PE_2") outcomes(1) frame(_pe_rates, replace) ///
    outlabels("Sustained EDSS 4") outcomeids(edss4) explabels("Any HRT" \ "Estrogen Dose")
use "fixtures/hrt_bin.dta", clear
collect clear
stset time, failure(edss4) id(id)
quietly collect: stcox hrt c.age, nolog level(90)
regtab, frame(_pe_bin, replace) eplotframe(_pe_bin_ep, replace) noint coef("HR") level(90) models("EDSS 4")
use "fixtures/hrt_dose.dta", clear
collect clear
stset time, failure(edss4) id(id)
quietly collect: stcox ib2.dosecat c.age, nolog level(90)
regtab, frame(_pe_dose, replace) eplotframe(_pe_dose_ep, replace) noint coef("HR") level(90) models("EDSS 4")
hrcomptab _pe_rates, modelframes(_pe_bin _pe_dose) rownames(hrt \ no high low) outcomemap(EDSS 4) ///
    reflabel("1.00 (ref)") effect("HR") eplotframe(_pe_ep, replace)
_ep_export _pe_ep "`out'/PE.csv"

quietly cd "`root'"
