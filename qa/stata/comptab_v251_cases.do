/* Six additive comptab/hrcomptab fixtures. Root runs only after review in an
   isolated owned directory with the exact 712044f8 / 2.5.1 source closure.
   Arguments: pinned tabtools directory, empty output directory, package root.
   No baseline fixture or central manifest is edited by this recipe. */
version 17.0
args native out root
if `"`native'"' == "" | `"`out'"' == "" | `"`root'"' == "" exit 198
set more off
set varabbrev off
adopath ++ "`native'"
run "`root'/qa/stata/golden_helpers.do"
golden_ado_version comptab
if "`r(version)'" != "2.5.1" exit 459
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION "not_used"
foreach id in CO001 CO002 CO003 CO004 CO005 CO006 {
    capture confirm file "`out'/`id'.csv"
    if !_rc exit 602
}
clear
input byte g byte x byte other double time byte ev byte ev2
1 1 1 10 1 1
1 2 2 10 1 1
1 3 1 10 1 0
1 4 2 10 0 0
1 5 1 10 0 0
1 6 2 10 0 0
1 7 1 10 0 0
1 8 2 10 0 0
1 9 1 10 0 0
1 10 2 10 0 0
1 11 1 10 0 0
1 12 2 10 0 0
2 1 1 10 1 1
2 2 2 10 1 1
2 3 1 10 1 1
2 4 2 10 1 1
2 5 1 10 1 1
2 6 2 10 0 1
2 7 1 10 0 0
2 8 2 10 0 0
2 9 1 10 0 0
2 10 2 10 0 0
2 11 1 10 0 0
2 12 2 10 0 0
3 1 1 10 1 1
3 2 2 10 1 1
3 3 1 10 1 1
3 4 2 10 1 1
3 5 1 10 1 1
3 6 2 10 1 1
3 7 1 10 1 1
3 8 2 10 1 1
3 9 1 10 0 1
3 10 2 10 0 0
3 11 1 10 0 0
3 12 2 10 0 0
end
label define gcat 1 "A" 2 "B" 3 "C"
label values g gcat
label define othercat 1 "No" 2 "Yes"
label values other othercat
tempfile records gblock oblock secondblock
save `records'
collect clear
quietly collect: poisson ev i.g
quietly collect: poisson ev i.g c.x
regtab, frame(_co_models, replace) eplotframe(_co_models_ep, replace) noint coef("IRR") models("Crude" \ "Adjusted")
collect clear
quietly collect: poisson ev i.g i.other c.x
regtab, frame(_co_appendix, replace) eplotframe(_co_appendix_ep, replace) noint coef("IRR")
collect clear
quietly collect: poisson ev i.g
quietly collect: poisson ev2 i.g
regtab, frame(_co_paired, replace) eplotframe(_co_paired_ep, replace) noint coef("IRR") models("First" \ "Second")
collect clear
quietly collect: poisson ev i.g
regtab, frame(_co_stars, replace) eplotframe(_co_stars_ep, replace) noint coef("IRR") stars starslevels(.8 .6 .2)

* Literal totals, with strate's log-Poisson bounds; not taken from R output.
clear
input str3 group double _D double _Y
"A" 3 120
"B" 5 120
"C" 8 120
end
generate double _Rate = _D / _Y
generate double _Lower = exp(log(_Rate) - invnormal(.975) / sqrt(_D))
generate double _Upper = exp(log(_Rate) + invnormal(.975) / sqrt(_D))
label variable _Lower "Lower 95% confidence limit"
label variable _Upper "Upper 95% confidence limit"
save "`gblock'.dta"
clear
input str3 group double _D double _Y
"No" 9 180
"Yes" 7 180
end
generate double _Rate = _D / _Y
generate double _Lower = exp(log(_Rate) - invnormal(.975) / sqrt(_D))
generate double _Upper = exp(log(_Rate) + invnormal(.975) / sqrt(_D))
label variable _Lower "Lower 95% confidence limit"
label variable _Upper "Upper 95% confidence limit"
save "`oblock'.dta"
clear
input str3 group double _D double _Y
"A" 2 120
"B" 6 120
"C" 9 120
end
generate double _Rate = _D / _Y
generate double _Lower = exp(log(_Rate) - invnormal(.975) / sqrt(_D))
generate double _Upper = exp(log(_Rate) + invnormal(.975) / sqrt(_D))
label variable _Lower "Lower 95% confidence limit"
label variable _Upper "Upper 95% confidence limit"
save "`secondblock'.dta"
stratetab, using("`gblock'") outcomes(1) outcomeids(ev) outlabels(ev) explabels(g) ratescale(100) unitlabel("100") frame(_co_rate, replace)
stratetab, using("`gblock'" "`oblock'") outcomes(1) outcomeids(ev) outlabels(ev) explabels(g \ other) ratescale(100) unitlabel("100") frame(_co_rateonly, replace)
stratetab, using("`gblock'" "`secondblock'") outcomes(2) outcomeids(ev \ ev2) outlabels("First outcome" \ "Second outcome") explabels(g) ratescale(100) unitlabel("100") frame(_co_tworates, replace)

* Keep native stdout, all returned counts, complete CSV/Markdown/XLSX geometry.
log using "`out'/CO001.console.txt", text replace name(co_case)
hrcomptab _co_rate, modelframes(_co_models) rows(2/4) keyed allmodels effect("IRR") cformat(%8.4f) csv("`out'/CO001.csv") markdown("`out'/CO001.md") xlsx("`out'/comptab.xlsx") sheet(CO001)
mata: golden_dump_r("`out'/CO001_stored.csv", "CO001")
log close co_case
log using "`out'/CO002.console.txt", text replace name(co_case)
hrcomptab _co_rateonly, modelframes(_co_models) rows(2/4) keyed allmodels effect("IRR") csv("`out'/CO002.csv") markdown("`out'/CO002.md") xlsx("`out'/comptab.xlsx") sheet(CO002)
mata: golden_dump_r("`out'/CO002_stored.csv", "CO002")
log close co_case
log using "`out'/CO003.console.txt", text replace name(co_case)
hrcomptab _co_rate, modelframes(_co_appendix) rows(all) modelonly effect("IRR") csv("`out'/CO003.csv") markdown("`out'/CO003.md") xlsx("`out'/comptab.xlsx") sheet(CO003)
mata: golden_dump_r("`out'/CO003_stored.csv", "CO003")
log close co_case
log using "`out'/CO004.console.txt", text replace name(co_case)
hrcomptab _co_tworates, modelframes(_co_paired) rows(3/4) outcomemap(Second \ First) effect("IRR") eplotframe(_co_forest, replace) csv("`out'/CO004.csv") markdown("`out'/CO004.md") xlsx("`out'/comptab.xlsx") sheet(CO004)
mata: golden_dump_r("`out'/CO004_stored.csv", "CO004")
frame _co_forest: format estimate ll ul pvalue %21.17g
frame _co_forest: export delimited using "`out'/CO004_forest.csv", replace
log close co_case
log using "`out'/CO005.console.txt", text replace name(co_case)
comptab _co_stars, rows(3/4) cformat(%8.4f) cisep(" to ") csv("`out'/CO005.csv") markdown("`out'/CO005.md") xlsx("`out'/comptab.xlsx") sheet(CO005)
mata: golden_dump_r("`out'/CO005_stored.csv", "CO005")
log close co_case
log using "`out'/CO006.console.txt", text replace name(co_case)
comptab _co_stars, rows(3/4) cisep(" / ") csv("`out'/CO006.csv") markdown("`out'/CO006.md") xlsx("`out'/comptab.xlsx") sheet(CO006)
mata: golden_dump_r("`out'/CO006_stored.csv", "CO006")
log close co_case
