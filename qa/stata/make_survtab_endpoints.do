/* Bounded WP6C endpoint/sample oracle. No existing fixtures are read or written.
   Root runs in an isolated archive, with a fresh task-owned output directory. */
version 17.0
args stata_ado out
if `"`stata_ado'"' == "" | `"`out'"' == "" exit 198
adopath ++ "`stata_ado'"
discard
quietly tabtools
if "`r(version)'" != "2.5.1" exit 9
findfile survtab.ado
if strpos("`r(fn)'", "`stata_ado'") != 1 exit 9
confirm new file "`out'/endpoints.log"
set rng mt64
set more off
set linesize 255
do qa/stata/golden_helpers.do
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION ""
log using "`out'/endpoints.log", text name(endpoint) nomsg
file open rcfile using "`out'/endpoint_rc.csv", write text
file write rcfile "case,stci_rc" _n
capture program drop wp6c_endpoint
program define wp6c_endpoint
    args id out options
    survtab, times(1 2 3 4) median level(90) `options' ///
        csv("`out'/`id'.csv") markdown("`out'/`id'.md") ///
        xlsx("`out'/endpoints.xlsx") sheet("`id'")
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    capture noisily stci, level(90)
    local stci_rc = _rc
    file write rcfile "`id',`stci_rc'" _n
    if !`stci_rc' {
        mata: golden_dump_r("`out'/`id'_stci.csv", "`id'")
    }
    preserve
    capture sts generate survival = s
    capture sts generate se = se(s)
    capture sts generate lower = lb(s), level(90)
    capture sts generate upper = ub(s), level(90)
    export delimited using "`out'/`id'_grid.csv", replace
    restore
end
clear
input double exit byte event
1 1
2 1
3 0
4 0
end
stset exit, failure(event)
wp6c_endpoint SVP001 "`out'" ""
clear
input double exit byte event
2 1
end
stset exit, failure(event)
wp6c_endpoint SVP002 "`out'" ""
clear
input double exit byte event
1 0
2 0
3 0
4 0
end
stset exit, failure(event)
wp6c_endpoint SVP003 "`out'" ""
clear
input double exit byte event long frequency
1 1 10
2 1 1
3 0 10
4 0 1
end
stset exit [fw=frequency], failure(event)
wp6c_endpoint SVP004 "`out'" ""
clear
input double exit byte event byte group
1 0 1
2 . 1
3 0 2
4 0 2
2 1 .
0 1 1
-1 1 2
. 1 1
end
stset exit, failure(event)
wp6c_endpoint SVP005 "`out'" "by(group)"
file close rcfile
file open runtime using "`out'/runtime.txt", write text
file write runtime "`c(stata_version)'" _n "`c(rng_current)'" _n
file close runtime
foreach helper in sts stci logrank stcox {
    findfile `helper'.ado
    copy "`r(fn)'" "`out'/installed_`helper'.ado"
}
log close endpoint
display as result "RESULT: survtab_endpoint cases=5 completed=5"
