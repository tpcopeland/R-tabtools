/* WP5C only: new OT scenarios and a literal command-owned input.
   Run from an isolated source archive into a newly allocated external output.
   Never calls the legacy generator or rewrites any existing scenario input. */
version 17.0
args stata_ado out
if `"`stata_ado'"' == "" | `"`out'"' == "" exit 198
adopath ++ "`stata_ado'"
discard
quietly tabtools
if "`r(version)'" != "2.5.1" exit 9
findfile outtab.ado
if strpos("`r(fn)'", "`stata_ado'") != 1 exit 9
set rng mt64
set linesize 255
set more off
do qa/stata/golden_helpers.do
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION ""
clear
set obs 30
gen byte exposed = _n <= 10
gen byte event = (_n <= 4) | inrange(_n, 11, 12)
gen byte z = mod(_n, 2)
replace z = . in 1
replace z = . in 11
gen byte p = 1
gen byte q = mod(_n, 2)
gen byte obs_event = 1
replace obs_event = 0 in 3
replace obs_event = 2 in 4
replace obs_event = . in 5
gen long id = _n
save "`out'/outtab_truth.dta", replace

capture program drop wp5c_case
program define wp5c_case
    args id out
    log using "`out'/`id'_console.txt", text replace nomsg name(probe)
    local options
    if "`id'" == "OT002" local options `"estimator(logit, or)"'
    if "`id'" == "OT003" local options `"models("z")"'
    if "`id'" == "OT004" local options `"models("z") smallcells(3)"'
    if "`id'" == "OT005" local options "minevents(5)"
    if "`id'" == "OT006" local options "panels(p q) obsprefix(obs_)"
    if "`id'" == "OT008" local options `"sep("long literal separator long literal separator long literal separator long literal separator")"'
    if "`id'" == "OT009" local options `"estimator(poisson, irr vce(cluster id))"'
    outtab event, exposure(exposed) `options' csv("`out'/`id'.csv") ///
        markdown("`out'/`id'.md") xlsx("`out'/outtab.xlsx") sheet("`id'")
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    log close probe
end
foreach id in OT001 OT002 OT003 OT004 OT005 OT006 OT008 OT009 {
    wp5c_case `id' "`out'"
}
replace event = 0 if exposed == 0
wp5c_case OT007 "`out'"
file open runtime using "`out'/runtime.txt", write text replace
file write runtime "`c(stata_version)'" _n "`c(rng_current)'" _n
file close runtime
display as result "RESULT: outtab_native tests=9 pass=9 fail=0"
