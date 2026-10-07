* Additive RL001-RL004 recipes for the held 712044f8 / tabtools 2.5.1 pin.
* Caller authenticates source members, supplies deterministic input.dta,
* owns the output directory and invokes this in an isolated Stata process.
version 17.0
args native output input
clear all
set more off
set linesize 255
adopath ++ "`native'"
cd "`output'"
use "`input'", clear
label variable g "Group"
label define groups 1 "A" 2 "B" 3 "C"
label values g groups
collect clear
collect: regress Y ib2.g x
collect: regress Y ib2.g x
display "RL_START_RL001"
regtab, xlsx("RL001.xlsx") sheet("S") models("Crude \ Adjusted") title("Title") nointercept nopvalue headershade zebra csv("RL001.csv") markdown("RL001.md") reftop
display "RL_END_RL001"
display "RL_START_RL002"
regtab, xlsx("RL002.xlsx") sheet("S") models("Crude \ Adjusted") title("Title") nointercept nopvalue headershade zebra csv("RL002.csv") markdown("RL002.md") reftop cellnote("A" 1 "Native note") addrow("Inside" "first" "second", after(g))
display "RL_END_RL002"
display "RL_START_RL003"
regtab, xlsx("RL003.xlsx") sheet("S") models("Crude \ Adjusted") title("Title") nointercept nopvalue headershade zebra csv("RL003.csv") markdown("RL003.md") transpose stats(n) collabels(1.g "Drug A") addcol("Text" "one" "two", after(1.g))
display "RL_END_RL003"
collect clear
collect: stintreg x, interval(lower upper) distribution(weibull) time tratio
collect: stintreg x, interval(lower upper) distribution(weibull) time tratio
display "RL_START_RL004"
regtab, xlsx("RL004.xlsx") sheet("S") models("Crude \ Adjusted") title("Title") nointercept nopvalue headershade zebra csv("RL004.csv") markdown("RL004.md")
display "RL_END_RL004"
display "RL_PARITY_COMPLETE"
