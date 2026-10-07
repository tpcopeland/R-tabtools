/* New SV001-SV012 only. No legacy generator or existing fixture access.
   Root supplies isolated frozen source and a fresh task-owned output folder. */
version 17.0
args stata_ado out
if `"`stata_ado'"' == "" | `"`out'"' == "" exit 198
adopath ++ "`stata_ado'"
discard
quietly tabtools
if "`r(version)'" != "2.5.1" exit 9
findfile survtab.ado
if strpos("`r(fn)'", "`stata_ado'") != 1 exit 9
confirm new file "`out'/survtab.xlsx"
set rng mt64
set linesize 255
set more off
do qa/stata/golden_helpers.do
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION ""
capture program drop wp6c_case
program define wp6c_case
    args id out times options
    save "`out'/`id'_input.dta"
    log using "`out'/`id'_console.txt", text nomsg name(svcase)
    survtab, times(`times') `options' footnote("Native annotation.") xlsx("`out'/survtab.xlsx") sheet("`id'") ///
        csv("`out'/`id'.csv") markdown("`out'/`id'.md")
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    log close svcase
end
clear
input double exit byte event byte group long id
1 1 1 1
2 1 1 2
3 0 2 3
4 0 2 4
end
stset exit, failure(event)
wp6c_case SV001 "`out'" "1 2" "by(group) median events riskset rmst(2) difference level(90)"
clear
input double exit byte event byte group
1 0 1
2 . 1
3 0 2
4 0 2
2 1 .
end
stset exit, failure(event)
wp6c_case SV002 "`out'" "1 2" "by(group) median events riskset rmst(2)"
clear
input long id double entry double exit byte event
1 0 2 1
2 2 3 1
3 0 4 0
end
stset exit, failure(event) enter(time entry) id(id)
wp6c_case SV003 "`out'" "2 3" "events riskset rmst(3)"
clear
input long id double entry double exit byte event
1 0 1 1
2 0 2 1
3 0 3 0
4 0 2 0
4 2 4 0
end
stset exit, failure(event) enter(time entry) id(id)
wp6c_case SV004 "`out'" "1 2 3" "median events riskset rmst(3)"
clear
input double exit byte event long frequency
1 1 10
2 1 1
3 0 10
4 0 1
end
stset exit [fw=frequency], failure(event)
wp6c_case SV005 "`out'" "1 2 3" "median events riskset rmst(3)"
clear
input double exit byte event byte group
1 1 1
2 1 1
3 0 2
4 0 2
end
label define arm 1 "a-b" 2 "a b"
label values group arm
stset exit, failure(event)
wp6c_case SV006 "`out'" "1 2 5" "by(group) reverse difference events riskset"
clear
input double exit byte event
1 1
2 1
3 0
4 0
end
stset exit, failure(event)
wp6c_case SV007 "`out'" "1 2 3 4" "median level(90) rmst(3)"
clear
input double exit byte event
2 1
end
stset exit, failure(event)
wp6c_case SV008 "`out'" "1 2" "median events riskset rmst(2) level(90)"
clear
input double exit byte event
1 1
2 1
end
stset exit, failure(event)
wp6c_case SV009 "`out'" "1 2" "median events riskset rmst(2) level(90)"
clear
input double exit byte event
2 1
2 1
end
stset exit, failure(event)
wp6c_case SV010 "`out'" "1 2" "median events riskset rmst(2) level(90)"
clear
set obs 3
generate double exit = cond(_n == 1, 0.3, cond(_n == 2, 0.1 + 0.2, 1))
generate byte event = _n <= 2
stset exit, failure(event)
wp6c_case SV011 "`out'" "0.3 0.5" "events riskset rmst(0.5)"
file open refusal using "`out'/native_refusals.csv", write text
file write refusal "case,rc" _n
capture noisily survtab, times(1) rmst(2)
local support_rc = _rc
file write refusal "support,`support_rc'" _n
clear
input double exit byte event double weight
1 1 1
2 0 2
end
stset exit [pw=weight], failure(event)
capture noisily survtab, times(1)
local weight_rc = _rc
file write refusal "probability_weight,`weight_rc'" _n
file close refusal
clear
set obs 4
generate double exit = _n
generate byte event = _n <= 2
generate float group = cond(_n <= 2, 0.1, 0.2)
stset exit, failure(event)
wp6c_case SV012 "`out'" "1 2" "by(group) difference events riskset rmst(2)"
file open runtime using "`out'/runtime.txt", write text
file write runtime "`c(stata_version)'" _n "`c(rng_current)'" _n
file close runtime
foreach helper in sts stci logrank stcox {
    findfile `helper'.ado
    copy "`r(fn)'" "`out'/installed_`helper'.ado"
}
display as result "RESULT: survtab_native cases=12 completed=12"
