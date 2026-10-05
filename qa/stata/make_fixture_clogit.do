/*  make_fixture_clogit.do - the conditional logistic regression fixture
    (task 5.15, survival::clogit as Stata clogit)

    Called at the end of make_fixtures.do; can also run on its own from the
    repo root, since it draws its own data:
        stata-mp -b do qa/stata/make_fixture_clogit.do

    clogit.dta: 400 matched sets of one case and one to three controls
    (sets 1-150 are 1:1, `matched11`), seed 20260927. Within a set the case
    is drawn with probability proportional to
    exp(0.5 x1 + 0.7 smoke + 0.3 [grp = 2] + 0.6 [grp = 3]), so the
    conditional-logit estimates recover those log odds ratios.
*/

version 17.0
clear all
set more off
set varabbrev off
set rng mt64

local out "tests/testthat/golden/fixtures"

clear
set seed 20260927
set obs 400
gen int set = _n
gen byte ncontrol = cond(set <= 150, 1, 1 + floor(runiform() * 3))
expand ncontrol + 1
sort set, stable
by set: gen byte member = _n
gen double x1 = rnormal()
gen byte smoke = runiform() < 0.3
gen byte grp = 1 + floor(runiform() * 3)
gen double w = exp(0.5 * x1 + 0.7 * smoke + 0.3 * (grp == 2) + 0.6 * (grp == 3))
by set: gen double cw = sum(w)
by set: gen double u = runiform() * cw[_N] if _n == 1
by set: replace u = u[1]
by set: gen byte case = cw >= u & cw - w < u
gen byte matched11 = ncontrol == 1
label variable x1 "Exposure score"
label variable smoke "Current smoker"
label variable grp "Group"
label define _clogit_grp 1 "Low" 2 "Middle" 3 "High", replace
label values grp _clogit_grp
keep set member case x1 smoke grp matched11
compress
label data "tabtools golden fixture: 1:1 and 1:m matched sets for clogit"
save "`out'/clogit.dta", replace
