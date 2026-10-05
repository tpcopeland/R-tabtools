/*  make_fixture_cohort_pp.do - person-period split of the cohort fixture

    Milestone 5w (IPTW / marginal structural model goldens W05-W11;
    qa/notes/effecttab_scoping.md section 7.4). Called at the end of
    make_fixtures.do; can also run on its own from the repo root, since it
    only reads the committed cohort fixture:
        stata-mp -b do qa/stata/make_fixture_cohort_pp.do

    One-year person-periods of cohort.dta: follow-up in years, split every
    year of analysis time, so R never re-splits (floating-point interval
    boundaries would otherwise have to be reproduced). No random draws.
*/

version 17.0
clear all
set more off
set varabbrev off

local out "tests/testthat/golden/fixtures"

use "`out'/cohort.dta", clear
keep id treated index_age female education follow_up cv_event iptw
gen double fu_y = follow_up/365.25
stset fu_y, failure(cv_event) id(id)
stsplit period, every(1)
gen byte ev = _d
gen double pt = _t - _t0
stset, clear
keep id treated index_age female education period ev pt iptw
label variable period "Follow-up year"
label variable ev "Cardiovascular event in period"
label variable pt "Person-time (years)"
sort id period
compress
label data "tabtools golden fixture: cohort one-year person-periods"
save "`out'/cohort_pp.dta", replace
