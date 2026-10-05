/*  make_fixture_effecttab.do - the effecttab fixtures (Phase 7d, task 7.11)

    Called at the end of make_fixtures.do; can also run on its own from the
    repo root, since it reads only the StataCorp example datasets:
        stata-mp -b do qa/stata/make_fixture_effecttab.do

    Extracts of two StataCorp example datasets (`webuse`, needs the
    network) with the variables the E goldens use, labels kept:
      cattaneo2   bweight mbsmoke mage medu  (teffects manual; Cattaneo 2010)
      bdsianesi5  wage ed paed               (multi-valued treatment)
    Redistribution terms of the StataCorp example data are to be confirmed
    before a CRAN release (plan: pre-CRAN list). No random draws.
*/

version 17.0
clear all
set more off
set varabbrev off

local out "tests/testthat/golden/fixtures"

**# cattaneo2: birthweight and maternal smoking
webuse cattaneo2, clear
keep bweight mbsmoke mage medu
compress
label data "tabtools golden fixture: cattaneo2 extract (teffects)"
save "`out'/cattaneo2.dta", replace

**# bdsianesi5: wages by education level (three-valued treatment)
webuse bdsianesi5, clear
keep wage ed paed
compress
label data "tabtools golden fixture: bdsianesi5 extract (multi-valued teffects)"
save "`out'/bdsianesi5.dta", replace
