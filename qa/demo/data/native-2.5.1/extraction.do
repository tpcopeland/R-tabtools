version 17.0
clear all
set rng mt64
set more off
* Merge cohort, treatment, comorbidities, and outcomes
use /tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/export/_data/cohort.dta, clear
merge 1:1 id using /tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/export/_data/treatment.dta, nogen keep(match)
merge 1:1 id using /tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/export/_data/comorbidities.dta, nogen keep(master match) nolabel
merge 1:1 id using /tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/export/_data/outcomes.dta, nogen keep(master match)

* Fill missing comorbidities with 0
foreach v in diabetes hypertension anxiety prior_cvd {
    replace `v' = 0 if missing(`v')
}

* Confound treatment: the fixture's treated is independent of the covariates,
* so crude and adjusted ratios would agree. Redraw it from age, sex, diabetes,
* and hypertension (the latter two predict the outcomes), keeping ~40% treated.
set seed 20261005
replace treated = runiform() < invlogit(-2.4 + 0.05 * (index_age - 58) ///
    - 0.3 * female + 2.2 * diabetes + 2.0 * hypertension)

* Derive binary outcome
gen byte cv_event = (cv_event_date < .)
label variable cv_event "Cardiovascular event"
label define cv_event_lbl 0 "No" 1 "Yes", replace
label values cv_event cv_event_lbl

* Add readable labels for female (cohort data uses yn_lbl 0=No/1=Yes)
label define female_lbl 0 "Male" 1 "Female", replace
label values female female_lbl

* Survival time for Cox models
gen double follow_up = study_exit - study_entry
label variable follow_up "Follow-up (days)"
stset follow_up, failure(cv_event)

* Generate IPTW weights for weighted demo
quietly logit treated index_age female i.education ///
    diabetes hypertension anxiety prior_cvd
predict double ps, pr
gen double iptw = cond(treated, 1/ps, 1/(1-ps))
label variable iptw "IPTW weight"

* Synthetic variables for additional demos
set seed 20260324

* Log-normal biomarker (CRP) -- for contln demo
gen double crp = exp(rnormal(1.5, 0.8))
label variable crp "C-reactive protein (mg/L)"

* Right-skewed count (prior hospitalizations) -- for conts demo
gen byte prior_hosp = rpoisson(1.8)
label variable prior_hosp "Prior hospitalizations"

* Rare binary event -- for bine (Fisher's exact) demo
gen byte rare_event = runiform() < 0.03
label variable rare_event "Rare adverse event"

* Variable with deliberate missingness -- for missing option demo
gen byte smoking = .
replace smoking = 0 if runiform() < 0.55
replace smoking = 1 if missing(smoking) & runiform() < 0.45
replace smoking = 2 if missing(smoking) & runiform() < 0.60
* ~10% remain missing
label variable smoking "Smoking status"
label define smoke_lbl 0 "Never" 1 "Former" 2 "Current", replace
label values smoking smoke_lbl

* Binary indicators for the other outcomes -- for the outtab demo
foreach _o in selfharm fracture gi_bleed {
    gen byte `_o' = (`_o'_date < .)
}
label variable selfharm "Self-harm"
label variable fracture "Fracture"
label variable gi_bleed "GI bleeding"

* Save working dataset
save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/cohort.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/cohort.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
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

* Random intercept per region
gen double u0 = .
forvalues r = 1/6 {
    replace u0 = rnormal() * 0.8 if region == `r'
}

gen double y = 2.5 + 0.01*age - 0.3*female + 0.08*bmi + u0 + rnormal()*0.6
label variable y "Systolic BP Change"

save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/mixed_bp.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/mixed_bp.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
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

save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/zip.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/zip.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
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

save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hurdle.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hurdle.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
clear
set seed 20260417
set obs 500
gen int id = _n
gen byte hrt = runiform() < 0.38
gen double age = rnormal(57, 8)
gen byte female = runiform() < 0.78
gen byte education = ceil(runiform() * 3)
gen double time = ceil(runiform() * 1825)
gen byte edss4 = runiform() < invlogit(-2.7 - 0.35 * hrt + 0.02 * (age - 55))
gen byte edss6 = runiform() < invlogit(-3.4 - 0.40 * hrt + 0.02 * (age - 55))
gen byte relapse = runiform() < invlogit(-1.6 - 0.28 * hrt + 0.01 * (age - 55))

save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hrt_bin.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hrt_bin.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
clear
set obs 500
gen int id = _n
gen byte dosecat = floor(runiform() * 4)
label define _hrc_dosecat 0 "No HRT" 1 "Low dose" 2 "Medium dose" 3 "High dose", replace
label values dosecat _hrc_dosecat
gen double age = rnormal(57, 8)
gen byte female = runiform() < 0.78
gen byte education = ceil(runiform() * 3)
gen double time = ceil(runiform() * 1825)
gen byte edss4 = runiform() < invlogit(-2.7 - 0.12 * (dosecat == 1) - 0.30 * (dosecat == 2) - 0.48 * (dosecat == 3) + 0.02 * (age - 55))
gen byte edss6 = runiform() < invlogit(-3.5 - 0.10 * (dosecat == 1) - 0.26 * (dosecat == 2) - 0.44 * (dosecat == 3) + 0.02 * (age - 55))
gen byte relapse = runiform() < invlogit(-1.6 - 0.08 * (dosecat == 1) - 0.20 * (dosecat == 2) - 0.34 * (dosecat == 3) + 0.01 * (age - 55))

save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hrt_dose.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/hrt_dose.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
webuse union, clear
save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/union.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/union.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
sysuse auto, clear
save "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/auto.dta", replace
file open ttmeta using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/data/auto.storage.csv", write text replace
file write ttmeta "variable,storage" _n
unab ttvars : _all
foreach ttv of local ttvars {
    local tttype : type `ttv'
    file write ttmeta "`ttv',`tttype'" _n
}
file close ttmeta
file open ttver using "/tmp/tabtools-3g-native-stage-c9somwtk/work/tabtools-demo-stata-22596515986b/extraction-runtime.txt", write text replace
file write ttver "`c(stata_version)'" _n "`c(rng_current)'" _n
file close ttver
display as result "RESULT: demo_data tests=8 pass=8 fail=0"
