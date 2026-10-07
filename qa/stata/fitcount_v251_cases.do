/* Authoring only: run by an isolated, source-authenticated root driver.
   Requires pinned Stata-Tools 712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129,
   Stata17 and golden_helpers.do already loaded. Never writes existing goldens.
   args: case ID, owned output directory. No adopath fallback is authorized.
   Complete source closure/pin and resolved-command authentication is root-owned.
*/
version 17.0
args id out
if !inlist("`id'", "FC001", "FC002", "FC003") exit 198
if `"`out'"' == "" exit 198
set more off
set varabbrev off
set linesize 255
clear
collect clear
estimates clear

if inlist("`id'", "FC001", "FC003") {
    input double(y x ev exposure) str1 pid
    1 1 1 1 "a"
    3 2 0 2 "a"
    2 3 2 1 "b"
    5 4 0 3 "c"
    4 5 1 2 "c"
    7 6 0 4 "d"
    end
}
if "`id'" == "FC002" {
    set obs 18
    generate byte g = mod(_n-1, 3)+1
    generate double x = floor((_n-1)/3)
    generate double y = 2+x+g+mod(_n,4)
    generate byte ev = cond(g==1,0,cond(g==2,1,2))
    quietly collect: regress y ib2.g##c.x
    tabtools fitcount, events(ev) terms
}
if "`id'" == "FC001" {
    quietly collect: regress y x
    tabtools fitcount, events(ev) people(pid) exposure(exposure) terms
    assert r(N)==6 & r(events)==4 & r(people)==4 & r(people_ev)==3 & r(exposure)==13
}
if "`id'" == "FC003" {
    forvalues m=1/12 {
        quietly collect: regress y x
        tabtools fitcount, events(ev) people(pid) exposure(exposure) terms
        quietly collect get ev=(3) total=(40) a=(10) b=(0) index=(`m'), tags(cmdset[`m'])
    }
}
log using "`out'/`id'_console.txt", text replace nomsg name(fitcount_native)
if "`id'" == "FC001" regtab, stats(obs events people exposure) exposurelabel("Person days") csv("`out'/`id'.csv") markdown("`out'/`id'.md") xlsx("`out'/regtab.xlsx") sheet(`id')
if "`id'" == "FC002" regtab, mincount(7) keepintercept csv("`out'/`id'.csv") markdown("`out'/`id'.md") xlsx("`out'/regtab.xlsx") sheet(`id')
if "`id'" == "FC003" regtab, stats(e(ev, mincell(5) maskwith(a)) e(ev|total, mincell(7)) e(a, maskwith(b)) e(b) e(index) text("Column" "m1" "m2" "m3" "m4" "m5" "m6" "m7" "m8" "m9" "m10" "m11" "m12")) csv("`out'/`id'.csv") markdown("`out'/`id'.md") xlsx("`out'/regtab.xlsx") sheet(`id')
mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
log close fitcount_native
