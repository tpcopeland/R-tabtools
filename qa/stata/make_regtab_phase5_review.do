/*  make_regtab_phase5_review.do - Stata expectations for the Phase 5 review
    fixes (IMPLEMENTATION_PLAN.md, "Phase 5 review outcome"), from the
    reviewer's probes (ids kept). For each case: regtab's csv() sink
    <id>.csv and every r() value <id>_stored.csv (golden_dump_r(), the
    format of the golden *_stored.csv files).

    Inputs: the golden fixtures, plus cntg.dta and bslope2.dta in the
    output folder (qa/make_regtab_phase5_review_data.R).

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_phase5_review.do [STATA_TOOLS_DIR]
    (default ~/Stata-Tools; pass a `git archive` export of Stata-Tools
    f42ee9cd, tabtools 2.1.12, when that working tree has uncommitted edits).
    tests/testthat/test-regtab-phase5-review.R reads them.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/fvgen"
adopath ++ "`stata_tools'/tabtools"
which regtab
which fvgen
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
global GOLDEN_TABTOOLS_VERSION "`r(version)'"
golden_ado_version fvgen
global GOLDEN_FVGEN_VERSION "`r(version)'"
local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_phase5_review"

capture program drop p5r_begin
program define p5r_begin
    args data
    clear
    capture collect clear
    estimates clear
    capture macro drop TABTOOLS_*
    use "`data'", clear
end

* ======================================================================
* P0-1 nbreg/menbreg ancillary rows
* ======================================================================
* --- C31
p5r_begin "`fx'/nbsim.dta"
quietly collect: nbreg y x1 mid high
regtab, keepintercept csv("`out'/C31.csv")
mata: golden_dump_r("`out'/C31_stored.csv", "C31")
capture stset, clear

* --- B20
p5r_begin "`out'/cntg.dta"
quietly collect: menbreg y x1 || clinic:, intmethod(laplace)
regtab, keepintercept csv("`out'/B20.csv")
mata: golden_dump_r("`out'/B20_stored.csv", "B20")
capture stset, clear

* --- G17
p5r_begin "`out'/cntg.dta"
quietly collect: menbreg y x1 || clinic:, intmethod(laplace)
regtab, keepintercept relabel stats(n ll aic) csv("`out'/G17.csv")
mata: golden_dump_r("`out'/G17_stored.csv", "G17")
capture stset, clear

* --- D07
p5r_begin "`fx'/zip.dta"
quietly collect: zinb event_count treatment age_z, inflate(zero_risk)
quietly collect: nbreg event_count treatment age_z
regtab, keepintercept models("ZINB" \ "NB") csv("`out'/D07.csv")
mata: golden_dump_r("`out'/D07_stored.csv", "D07")
capture stset, clear

* --- G18
p5r_begin "`fx'/zip.dta"
quietly collect: zinb event_count treatment age_z, inflate(zero_risk)
quietly collect: nbreg event_count treatment age_z
regtab, keepintercept stats(n ll) models("ZINB" \ "NB") csv("`out'/G18.csv")
mata: golden_dump_r("`out'/G18_stored.csv", "G18")
capture stset, clear

* --- G19
p5r_begin "`fx'/nbsim.dta"
quietly collect: nbreg y x1 mid high
regtab, keepintercept stats(n ll aic) csv("`out'/G19.csv")
mata: golden_dump_r("`out'/G19_stored.csv", "G19")
capture stset, clear

* ======================================================================
* P0-2 weighted mlogit
* ======================================================================
* --- B11
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education index_age female [fw=fw], baseoutcome(1)
regtab, stats(n ll) csv("`out'/B11.csv")
mata: golden_dump_r("`out'/B11_stored.csv", "B11")
capture stset, clear

* ======================================================================
* P0-3 single-equation models in the coleq#colname layout
* ======================================================================
* --- B23
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
quietly collect: poisson event_count treatment age_z
regtab, models("ZIP" \ "Poisson") csv("`out'/B23.csv")
mata: golden_dump_r("`out'/B23_stored.csv", "B23")
capture stset, clear

* --- G03
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
quietly collect: regress event_count treatment age_z
regtab, keepintercept models("ZIP" \ "OLS") csv("`out'/G03.csv")
mata: golden_dump_r("`out'/G03_stored.csv", "G03")
capture stset, clear

* --- G21
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
quietly collect: poisson event_count treatment age_z
regtab, keepintercept stats(n ll) models("ZIP" \ "Poisson") csv("`out'/G21.csv")
mata: golden_dump_r("`out'/G21_stored.csv", "G21")
capture stset, clear

* --- G29
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count treatment i.female, inflate(zero_risk)
quietly collect: poisson event_count treatment i.female
regtab, models("ZIP" \ "Poisson") csv("`out'/G29.csv")
mata: golden_dump_r("`out'/G29_stored.csv", "G29")
capture stset, clear

* --- G02
p5r_begin "`fx'/zip.dta"
quietly collect: poisson event_count treatment age_z
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
regtab, models("Poisson" \ "ZIP") csv("`out'/G02.csv")
mata: golden_dump_r("`out'/G02_stored.csv", "G02")
capture stset, clear

* --- G31
p5r_begin "`fx'/zip.dta"
quietly collect: poisson event_count treatment i.female
quietly collect: zip event_count treatment i.female, inflate(zero_risk)
regtab, keepintercept models("Poisson" \ "ZIP") csv("`out'/G31.csv")
mata: golden_dump_r("`out'/G31_stored.csv", "G31")
capture stset, clear

* --- G33
p5r_begin "`fx'/zip.dta"
quietly collect: tobit event_count treatment age_z, ll(0)
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
regtab, keepintercept models("Tobit" \ "ZIP") csv("`out'/G33.csv")
mata: golden_dump_r("`out'/G33_stored.csv", "G33")
capture stset, clear

* --- G34
p5r_begin "`fx'/zip.dta"
quietly collect: logit female treatment age_z
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
regtab, keepintercept models("Logit" \ "ZIP") csv("`out'/G34.csv")
mata: golden_dump_r("`out'/G34_stored.csv", "G34")
capture stset, clear

* --- G35
p5r_begin "`fx'/zip.dta"
quietly collect: nbreg event_count treatment age_z
quietly collect: zinb event_count treatment age_z, inflate(zero_risk)
regtab, keepintercept models("NB" \ "ZINB") csv("`out'/G35.csv")
mata: golden_dump_r("`out'/G35_stored.csv", "G35")
capture stset, clear

* --- G32
p5r_begin "`fx'/zip.dta"
gen ylo = cond(event_count == 0, ., event_count)
quietly collect: intreg ylo event_count treatment age_z
quietly collect: zip event_count treatment age_z, inflate(zero_risk)
regtab, keepintercept models("Intreg" \ "ZIP") csv("`out'/G32.csv")
mata: golden_dump_r("`out'/G32_stored.csv", "G32")
capture stset, clear

* --- B22
p5r_begin "`fx'/cohort.dta"
quietly collect: ologit education index_age female
quietly collect: mlogit education index_age female, baseoutcome(1)
regtab, models("OL" \ "ML") csv("`out'/B22.csv")
mata: golden_dump_r("`out'/B22_stored.csv", "B22")
capture stset, clear

* --- G04
p5r_begin "`fx'/cohort.dta"
quietly collect: ologit education index_age female
quietly collect: mlogit education index_age female, baseoutcome(1)
regtab, keepintercept cutlabels("A \ B") models("OL" \ "ML") csv("`out'/G04.csv")
mata: golden_dump_r("`out'/G04_stored.csv", "G04")
capture stset, clear

* --- G23
p5r_begin "`fx'/cohort.dta"
stset follow_up, failure(cv_event)
quietly collect: stcox index_age female
quietly collect: mlogit education index_age female, baseoutcome(1)
regtab, models("Cox" \ "ML") csv("`out'/G23.csv")
mata: golden_dump_r("`out'/G23_stored.csv", "G23")
capture stset, clear

* --- G24
p5r_begin "`fx'/cohort.dta"
stset follow_up, failure(cv_event)
quietly collect: streg index_age female, distribution(weibull) time
quietly collect: mlogit education index_age female, baseoutcome(1)
regtab, keepintercept models("AFT" \ "ML") csv("`out'/G24.csv")
mata: golden_dump_r("`out'/G24_stored.csv", "G24")
capture stset, clear

* ======================================================================
* P0-4 mlogit order with a non-first base outcome
* ======================================================================
* --- D10
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education index_age i.smoking, baseoutcome(2)
regtab, keepintercept csv("`out'/D10.csv")
mata: golden_dump_r("`out'/D10_stored.csv", "D10")
capture stset, clear

* --- G22
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education index_age i.smoking
regtab, csv("`out'/G22.csv")
mata: golden_dump_r("`out'/G22_stored.csv", "G22")
capture stset, clear

* ======================================================================
* P0-5 mecloglog (R keeps hazard ratios; Stata prints log-hazards)
* ======================================================================
* --- B15b
p5r_begin "`out'/bslope2.dta"
quietly collect: mecloglog y x female || site:
regtab, stats(n icc ll) csv("`out'/B15b.csv")
mata: golden_dump_r("`out'/B15b_stored.csv", "B15b")
capture stset, clear

* ======================================================================
* P0-6 glmer nAGQ > 1 log-likelihood
* ======================================================================
* --- F03
p5r_begin "`out'/cntg.dta"
quietly collect: mepoisson y x1 || clinic:
regtab, stats(n ll aic bic) csv("`out'/F03.csv")
mata: golden_dump_r("`out'/F03_stored.csv", "F03")
capture stset, clear

* --- B17
p5r_begin "`out'/cntg.dta"
gen t = 1 + mod(_n, 4)
quietly collect: mepoisson y x1, exposure(t) || clinic:
regtab, stats(n ll) csv("`out'/B17.csv")
mata: golden_dump_r("`out'/B17_stored.csv", "B17")
capture stset, clear

* --- B18
p5r_begin "`out'/cntg.dta"
gen n = y + 1 + mod(_n, 5)
quietly collect: melogit y x1 || clinic:, binomial(n)
regtab, stats(n ll) csv("`out'/B18.csv")
mata: golden_dump_r("`out'/B18_stored.csv", "B18")
capture stset, clear

* ======================================================================
* P0-7 labels under if/subset
* ======================================================================
* --- B05
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count treatment age_z if female == 1, inflate(zero_risk)
regtab, csv("`out'/B05.csv")
mata: golden_dump_r("`out'/B05_stored.csv", "B05")
capture stset, clear

* --- B06
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education index_age female if treated == 1, baseoutcome(1)
regtab, csv("`out'/B06.csv")
mata: golden_dump_r("`out'/B06_stored.csv", "B06")
capture stset, clear

* ======================================================================
* P0-8 Gaussian survreg: intreg and tobit
* ======================================================================
* --- B04
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
quietly collect: tobit yc x1, ll(0)
regtab, csv("`out'/B04.csv")
mata: golden_dump_r("`out'/B04_stored.csv", "B04")
capture stset, clear

* --- B04b
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
gen ylo = cond(yc == 0, ., yc)
quietly collect: intreg ylo yc x1
regtab, keepintercept csv("`out'/B04b.csv")
mata: golden_dump_r("`out'/B04b_stored.csv", "B04b")
capture stset, clear

* --- G11
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
gen ylo = cond(yc == 0, ., yc)
quietly collect: intreg ylo yc x1
regtab, stats(n ll aic bic) csv("`out'/G11.csv")
mata: golden_dump_r("`out'/G11_stored.csv", "G11")
capture stset, clear

* --- G12
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
quietly collect: tobit yc x1, ll(0)
regtab, keepintercept stats(n ll aic bic) csv("`out'/G12.csv")
mata: golden_dump_r("`out'/G12_stored.csv", "G12")
capture stset, clear

* --- G13
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
gen ylo = cond(yc == 0, ., yc)
quietly collect: intreg ylo yc x1
regtab, nointercept csv("`out'/G13.csv")
mata: golden_dump_r("`out'/G13_stored.csv", "G13")
capture stset, clear

* --- G14
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
quietly collect: tobit yc x1, ll(0)
regtab, relabel csv("`out'/G14.csv")
mata: golden_dump_r("`out'/G14_stored.csv", "G14")
capture stset, clear

* --- G15
p5r_begin "`out'/cntg.dta"
gen yc = max(y - 2, 0)
quietly collect: tobit yc x1, ll(0)
regtab, nointercept csv("`out'/G15.csv")
mata: golden_dump_r("`out'/G15_stored.csv", "G15")
capture stset, clear

* ======================================================================
* P0-9 robust and clustered Cox/streg variance
* ======================================================================
* --- B01
p5r_begin "`fx'/cohort3500.dta"
keep if _n <= 400
stset follow_up, failure(cv_event)
quietly collect: stcox treated female, vce(cluster region)
regtab, csv("`out'/B01.csv")
mata: golden_dump_r("`out'/B01_stored.csv", "B01")
capture stset, clear

* --- B02
p5r_begin "`fx'/cohort3500.dta"
keep if _n <= 400
stset follow_up, failure(cv_event)
quietly collect: stcox treated female, vce(robust)
regtab, csv("`out'/B02.csv")
mata: golden_dump_r("`out'/B02_stored.csv", "B02")
capture stset, clear

* --- B03
p5r_begin "`fx'/cohort3500.dta"
keep if _n <= 400
stset follow_up, failure(cv_event)
quietly collect: streg treated female, distribution(weibull) time vce(robust)
regtab, csv("`out'/B03.csv")
mata: golden_dump_r("`out'/B03_stored.csv", "B03")
capture stset, clear

* ======================================================================
* P1-1 meprobit Laplace (glmmTMB)
* ======================================================================
* --- B16
p5r_begin "`out'/bslope2.dta"
quietly collect: meprobit y x female || site:, intmethod(laplace)
regtab, stats(n icc ll) csv("`out'/B16.csv")
mata: golden_dump_r("`out'/B16_stored.csv", "B16")
capture stset, clear

* ======================================================================
* P1-4 multi-equation interactions
* ======================================================================
* --- E01
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education i.female##c.index_age, baseoutcome(1)
regtab, csv("`out'/E01.csv")
mata: golden_dump_r("`out'/E01_stored.csv", "E01")
capture stset, clear

* --- E02
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count c.treatment##c.age_z, inflate(zero_risk)
regtab, csv("`out'/E02.csv")
mata: golden_dump_r("`out'/E02_stored.csv", "E02")
capture stset, clear

* --- G09
p5r_begin "`fx'/cohort.dta"
quietly collect: mlogit education i.female##i.smoking, baseoutcome(1)
regtab, csv("`out'/G09.csv")
mata: golden_dump_r("`out'/G09_stored.csv", "G09")
capture stset, clear

* --- G10
p5r_begin "`fx'/zip.dta"
quietly collect: zip event_count i.female##c.treatment age_z, inflate(zero_risk)
regtab, csv("`out'/G10.csv")
mata: golden_dump_r("`out'/G10_stored.csv", "G10")
capture stset, clear

* --- G27
p5r_begin "`fx'/cohort.dta"
fvgen i.female##c.index_age
quietly collect: mlogit education `r(allvars)', baseoutcome(1)
regtab, csv("`out'/G27.csv")
mata: golden_dump_r("`out'/G27_stored.csv", "G27")
capture stset, clear

* --- G28
p5r_begin "`fx'/zip.dta"
fvgen i.female##c.treatment
quietly collect: zip event_count `r(allvars)' age_z, inflate(zero_risk)
regtab, csv("`out'/G28.csv")
mata: golden_dump_r("`out'/G28_stored.csv", "G28")
capture stset, clear

* ======================================================================
* M65 duplicate group labels
* ======================================================================
* --- G36
p5r_begin "`fx'/nhanes2.dta"
gen zone = ceil(location / 10)
label variable zone "Area"
label variable location "Area"
quietly collect: mixed bmi age || zone: || location:
regtab, relabel stats(n groups icc) csv("`out'/G36.csv")
mata: golden_dump_r("`out'/G36_stored.csv", "G36")
capture stset, clear

* ======================================================================
* M69 oprobit via clm
* ======================================================================
* --- C09
p5r_begin "`fx'/cohort.dta"
quietly collect: oprobit education index_age female diabetes
regtab, keepintercept stats(n ll r2) csv("`out'/C09.csv")
mata: golden_dump_r("`out'/C09_stored.csv", "C09")
capture stset, clear

* ======================================================================
* P2-1 / M78 frequency-weight observations
* ======================================================================
* --- B10
p5r_begin "`fx'/cohort.dta"
quietly collect: ologit education index_age female [fw=fw]
regtab, stats(n ll) csv("`out'/B10.csv")
mata: golden_dump_r("`out'/B10_stored.csv", "B10")
capture stset, clear

* --- B12
p5r_begin "`fx'/zip.dta"
gen w = 1 + mod(_n, 3)
quietly collect: zip event_count treatment age_z [fw=w], inflate(zero_risk)
regtab, stats(n ll) csv("`out'/B12.csv")
mata: golden_dump_r("`out'/B12_stored.csv", "B12")
capture stset, clear

* --- B13
p5r_begin "`fx'/cohort.dta"
stset follow_up [fw=fw], failure(cv_event)
quietly collect: streg treated female, distribution(weibull) time
regtab, stats(n ll) csv("`out'/B13.csv")
mata: golden_dump_r("`out'/B13_stored.csv", "B13")
capture stset, clear

