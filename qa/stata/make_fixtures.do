/*  make_fixtures.do - build the labelled .dta fixtures shared by Stata and R

    Run from the R-tabtools repo root (make_golden.R does this):
        stata-mp -b do qa/stata/make_fixtures.do

    Writes one .dta per fixture to tests/testthat/golden/fixtures. Both sides read these files, so
    Stata goldens and R results are computed from identical data and labels.

    The cohort block reproduces the analysis dataset of
    ~/Stata-Tools/tabtools/demo/demo_tabtools.do (lines 130-191, seed
    20260324) draw for draw, so the demo workbooks can be regenerated exactly
    (T30, R25). Variables the golden scenarios need beyond the demo are drawn
    afterwards under a separate seed, leaving the demo draws untouched.
*/

version 17.0
clear all
set more off
set varabbrev off
set rng mt64

local stata_tools "~/Stata-Tools"
local out "tests/testthat/golden/fixtures"
capture mkdir "`out'"

**# cohort: demo analysis dataset (demo_tabtools.do:130-191)
use "`stata_tools'/_data/cohort.dta", clear
merge 1:1 id using "`stata_tools'/_data/treatment.dta", nogen keep(match)
merge 1:1 id using "`stata_tools'/_data/comorbidities.dta", nogen keep(master match) nolabel
merge 1:1 id using "`stata_tools'/_data/outcomes.dta", nogen keep(master match)

foreach v in diabetes hypertension anxiety prior_cvd {
    replace `v' = 0 if missing(`v')
}

gen byte cv_event = (cv_event_date < .)
label variable cv_event "Cardiovascular event"
label define cv_event_lbl 0 "No" 1 "Yes", replace
label values cv_event cv_event_lbl

label define female_lbl 0 "Male" 1 "Female", replace
label values female female_lbl

gen double follow_up = study_exit - study_entry
label variable follow_up "Follow-up (days)"
stset follow_up, failure(cv_event)

quietly logit treated index_age female i.education ///
    diabetes hypertension anxiety prior_cvd
predict double ps, pr
gen double iptw = cond(treated, 1/ps, 1/(1-ps))
label variable iptw "IPTW weight"

set seed 20260324

gen double crp = exp(rnormal(1.5, 0.8))
label variable crp "C-reactive protein (mg/L)"

gen byte prior_hosp = rpoisson(1.8)
label variable prior_hosp "Prior hospitalizations"

gen byte rare_event = runiform() < 0.03
label variable rare_event "Rare adverse event"

gen byte smoking = .
replace smoking = 0 if runiform() < 0.55
replace smoking = 1 if missing(smoking) & runiform() < 0.45
replace smoking = 2 if missing(smoking) & runiform() < 0.60
label variable smoking "Smoking status"
label define smoke_lbl 0 "Never" 1 "Former" 2 "Current", replace
label values smoking smoke_lbl

**# cohort: golden-only variables (separate seed; demo draws above unchanged)
set seed 20260925

* Values near 1e8: the default %2.0f renders them in e-notation (plan §10).
gen double cost_sek = round(exp(rnormal(18.4, 0.35)))
label variable cost_sek "Lifetime healthcare cost (SEK)"

* Log-normal with zeros, negatives and missing: contln treats x <= 0 as
* missing (T13).
gen double lab_value = exp(rnormal(0.5, 0.7))
replace lab_value = 0 if runiform() < 0.03
replace lab_value = -runiform() if runiform() < 0.01
replace lab_value = . if runiform() < 0.05
label variable lab_value "Lab value (non-positive allowed)"

* Observed in the control group only: no test can run (T35).
gen double ctrl_marker = rnormal(10, 2) if treated == 0
label variable ctrl_marker "Control-only marker"
gen byte ctrl_grade = 1 + floor(3 * runiform()) if treated == 0
label variable ctrl_grade "Control-only grade"
label define ctrl_grade_lbl 1 "Low" 2 "Medium" 3 "High", replace
label values ctrl_grade ctrl_grade_lbl

* Frequency weight with zeros, which fweight excludes (T24).
gen int fw = rpoisson(1.2)
label variable fw "Frequency weight"

* Competing-risks outcome (R20): death without a CV event competes.
gen byte event_type = cond(cv_event, 1, cond(death_date < . & death_date <= study_exit, 2, 0))
label variable event_type "Event type"
label define event_type_lbl 0 "Censored" 1 "CV event" 2 "Death", replace
label values event_type event_type_lbl

* Near-normal continuous variable, so the reproduced swilk subsample also
* yields contn (T03b). Drawn last: every draw above is unchanged by it.
gen double bmi = round(rnormal(26, 4), 0.01)
label variable bmi "Body mass index"

drop _st _d _t _t0 ps birth_date study_entry study_exit death_date ///
    cv_event_date selfharm_date fracture_date gi_bleed_date
stset, clear
sort id
compress
label data "tabtools golden fixture: demo analysis cohort"
save "`out'/cohort.dta", replace

**# cohort3500: first 3,500 rows (reproduced 2,000-row swilk subsample, T03b)
keep in 1/3500
label data "tabtools golden fixture: cohort rows 1-3500"
save "`out'/cohort3500.dta", replace

**# auto: sysuse auto plus scenario variables
sysuse auto, clear
set seed 20260926

gen double age = round(rnormal(50, 10), 0.1)
label variable age "Owner age"
gen byte stage = 1 + floor(3 * runiform())
label variable stage "Stage"
label define stage_lbl 1 "Early" 2 "Mid" 3 "Late", replace
label values stage stage_lbl

* String by(): encode sorts large < medium < small (T32).
gen str6 size_class = cond(weight < 2500, "small", cond(weight < 3500, "medium", "large"))
label variable size_class "Size class"

* Exact collinearity with mpg -> Omitted (R12).
gen int mpg_dup = 2 * mpg
label variable mpg_dup "Mileage (doubled)"

* Price tertiles: full-rank cross with foreign (fvgen cat x cat, R13c).
xtile pclass = price, nq(3)
label variable pclass "Price class"
label define pclass_lbl 1 "Budget" 2 "Mid-range" 3 "Premium", replace
label values pclass pclass_lbl

compress
label data "tabtools golden fixture: 1978 automobile data"
save "`out'/auto.dta", replace

**# nhanes2: webuse extract (multilevel logistic, R22)
webuse nhanes2, clear
keep sampl highbp age sex race region location bmi diabetes heartatk
compress
label data "tabtools golden fixture: NHANES II extract"
save "`out'/nhanes2.dta", replace

**# union: webuse extract (GEE with QICu, R23 and demo sheet GEE QICu)
webuse union, clear
keep idcode year union age grade not_smsa south black
compress
label data "tabtools golden fixture: NLS union extract"
save "`out'/union.dta", replace

**# mixed_bp: demo_tabtools.do sheet 15 (two-level linear mixed model)
clear
set seed 20260323
set obs 600
gen int region = ceil(_n/100)
label variable region "Healthcare Region"
gen double age = rnormal(55, 12)
label variable age "Age (years)"
gen byte female = runiform() > 0.5
label variable female "Female sex"
gen double bmi = rnormal(26, 5)
label variable bmi "BMI"
gen double u0 = .
forvalues r = 1/6 {
    replace u0 = rnormal() * 0.8 if region == `r'
}
gen double y = 2.5 + 0.01*age - 0.3*female + 0.08*bmi + u0 + rnormal()*0.6
label variable y "Systolic BP Change"
drop u0
compress
label data "tabtools golden fixture: demo mixed-model data"
save "`out'/mixed_bp.dta", replace

**# zip: demo_tabtools.do sheet 31 (zero-inflated counts)
clear
set seed 20260601
set obs 1500
gen byte treatment = runiform() < 0.45
gen double age_z = rnormal()
gen byte female = runiform() < 0.55
gen double zero_risk = rnormal()
gen double log_mu = 0.45 - 0.25 * treatment + 0.35 * age_z + 0.20 * female + rnormal() * 0.35
gen double mu = exp(log_mu)
gen byte structural_zero = runiform() < invlogit(-0.9 + 0.90 * zero_risk - 0.45 * treatment + 0.35 * female)
gen byte event_count = cond(structural_zero, 0, rpoisson(mu))
label variable event_count "Event count"
label variable treatment "Treatment"
label variable age_z "Age z-score"
label variable female "Female"
label variable zero_risk "Structural-zero risk"
drop log_mu mu structural_zero
compress
label data "tabtools golden fixture: demo zero-inflated data"
save "`out'/zip.dta", replace

**# hurdle: demo_tabtools.do sheet 32 (Cragg hurdle)
clear
set seed 20260602
set obs 1200
gen double dose_intensity = rnormal()
gen double participation_score = rnormal()
gen byte positive = runiform() < invlogit(-0.3 + 0.7 * participation_score)
gen double annual_cost = cond(positive, ///
    exp(1 + 0.4 * dose_intensity + rnormal() * 0.5), 0)
label variable annual_cost "Annual cost"
label variable dose_intensity "Dose intensity"
label variable participation_score "Participation score"
drop positive
compress
label data "tabtools golden fixture: demo hurdle data"
save "`out'/hurdle.dta", replace

**# small-cell tables (demo_tabtools.do sheets 60-65)
capture program drop _sc_labels
program define _sc_labels
    label define demo_group 0 "Control" 1 "Treatment", replace
    label values group demo_group
    label variable group "Study group"
end

clear
input byte group byte category int frequency
0 0 2
0 1 3
1 0 3
1 1 2
end
expand frequency
drop frequency
_sc_labels
label define demo_category 0 "Absent" 1 "Present", replace
label values category demo_category
label variable category "Characteristic"
label data "tabtools golden fixture: small cells, primary only"
save "`out'/sc_primary.dta", replace

clear
input byte group byte category int frequency
0 0 2
0 1 8
1 0 6
1 1 4
end
expand frequency
drop frequency
_sc_labels
label define demo_category 0 "Absent" 1 "Present", replace
label values category demo_category
label variable category "Characteristic"
label data "tabtools golden fixture: small cells, complementary"
save "`out'/sc_complement.dta", replace

clear
input byte group byte rare_ae int frequency
0 0 48
0 1 2
1 0 47
1 1 3
end
expand frequency
drop frequency
_sc_labels
label define yn 0 "No" 1 "Yes", replace
label values rare_ae yn
label variable rare_ae "Rare adverse event"
label data "tabtools golden fixture: small cells, binary"
save "`out'/sc_binary.dta", replace

**# cohort_pp: one-year person-periods of cohort (milestone 5w, W05-W11)
do "qa/stata/make_fixture_cohort_pp.do"

**# effecttab fixtures: cattaneo2, bdsianesi5 (Phase 7d, E goldens)
do "qa/stata/make_fixture_effecttab.do"

**# comptab fixtures: hrt_bin, hrt_dose (Phase 7c, C goldens)
do "qa/stata/make_fixture_comptab.do"

**# clogit fixture: 1:1 and 1:m matched sets (task 5.15, R74-R76)
do "qa/stata/make_fixture_clogit.do"
