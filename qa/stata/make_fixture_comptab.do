/*  make_fixture_comptab.do - the hrcomptab model fixtures (Phase 7c, task 7.6)

    Called at the end of make_fixtures.do; can also run on its own from the
    repo root, since it draws its own data:
        stata-mp -b do qa/stata/make_fixture_comptab.do

    Reproduces the two model datasets of the hrcomptab demo section of
    ~/Stata-Tools/tabtools/demo/demo_tabtools.do (sheet 46, "HR
    Composite"; seed 20260417) draw for draw, so golden C04 regenerates
    demo_hrcomptab.xlsx exactly:
      hrt_bin   500 subjects, binary hormone therapy (hrt), three outcomes
      hrt_dose  500 subjects, four dose categories (dosecat, labelled),
                drawn after hrt_bin from the same stream
    Both keep only what the models use (id time age female education and
    the exposure and outcome indicators). No variable labels, as in the
    demo, so the regtab rows show the variable names on both sides.
*/

version 17.0
clear all
set more off
set varabbrev off
set rng mt64

local out "tests/testthat/golden/fixtures"

**# hrt_bin: demo_tabtools.do:1424-1436
clear
set seed 20260417
set obs 500
gen int id = _n
gen byte hrt = runiform() < 0.38
gen double age = rnormal(57, 8)
gen byte female = runiform() < 0.78
gen byte education = ceil(runiform() * 3)
gen double time = ceil(runiform() * 1825)
gen byte edss4 = runiform() < invlogit(-2.7 - 0.35 * hrt + 0.02 * (age - 55))
gen byte edss6 = runiform() < invlogit(-3.4 - 0.40 * hrt + 0.02 * (age - 55))
gen byte relapse = runiform() < invlogit(-1.6 - 0.28 * hrt + 0.01 * (age - 55))
label data "tabtools golden fixture: hrcomptab demo, binary HRT models"
save "`out'/hrt_bin.dta", replace

**# hrt_dose: demo_tabtools.do:1449-1461 (same stream, no reseed)
clear
set obs 500
gen int id = _n
gen byte dosecat = floor(runiform() * 4)
label define _hrc_dosecat 0 "No HRT" 1 "Low dose" 2 "Medium dose" 3 "High dose", replace
label values dosecat _hrc_dosecat
gen double age = rnormal(57, 8)
gen byte female = runiform() < 0.78
gen byte education = ceil(runiform() * 3)
gen double time = ceil(runiform() * 1825)
gen byte edss4 = runiform() < invlogit(-2.7 - 0.12 * (dosecat == 1) - 0.30 * (dosecat == 2) - 0.48 * (dosecat == 3) + 0.02 * (age - 55))
gen byte edss6 = runiform() < invlogit(-3.5 - 0.10 * (dosecat == 1) - 0.26 * (dosecat == 2) - 0.44 * (dosecat == 3) + 0.02 * (age - 55))
gen byte relapse = runiform() < invlogit(-1.6 - 0.08 * (dosecat == 1) - 0.20 * (dosecat == 2) - 0.34 * (dosecat == 3) + 0.01 * (age - 55))
label data "tabtools golden fixture: hrcomptab demo, dose-category models"
save "`out'/hrt_dose.dta", replace
