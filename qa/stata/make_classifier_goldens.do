/*  make_classifier_goldens.do - task 0.5b goldens for the auto-typing classifier

    Run through make_golden.R (cwd = tests/testthat/golden, dev tabtools first on
    the adopath). Writes:
      swilk.csv           swilk W, V, z, p (Stata 17 swilk.ado, tie-averaged ranks)
      rng_mt64.csv        runiform() streams after set seed (mt64), %21.18f
      autotype.csv        _tabtools_detect_vartype for the T03/T03b/T03c varlists
      subsample_ids.csv   ids of the 2,000-row swilk subsample, cohort3500
      detect_vartype.csv  one row per branch of _tabtools_detect_vartype
    and the fixture fixtures/vartype_branches.dta that detect_vartype.csv uses.
*/

version 17.0
set rng mt64
capture findfile _tabtools_common.ado
run "`r(fn)'"

capture program drop _num
program define _num, rclass
    * %21.17g text for a value, "." kept for missing
    args x
    if missing(`x') return local s = string(`x')
    else return local s = strtrim(string(`x', "%21.17g"))
end

capture program drop _swilk_row
program define _swilk_row
    * _swilk_row handle case fixture variable [if]
    syntax anything [if]
    gettoken fh anything : anything
    gettoken case anything : anything
    gettoken fixture anything : anything
    gettoken var anything : anything
    marksample touse, novarlist
    quietly count if `touse' & !missing(`var')
    local n = r(N)
    capture quietly swilk `var' if `touse'
    local rc = _rc
    local W = .
    local V = .
    local z = .
    local p = .
    if !`rc' {
        local W = r(W)
        local V = r(V)
        local z = r(z)
        local p = r(p)
    }
    foreach s in W V z p {
        _num ``s''
        local `s' "`r(s)'"
    }
    file write `fh' "`case',`fixture',`var',`n',`rc',`W',`V',`z',`p'" _n
end

**# (i) swilk
tempname fh
file open `fh' using "swilk.csv", write text replace
file write `fh' "case,fixture,variable,n,rc,W,V,z,p" _n

use "fixtures/auto.dta", clear
foreach v in price mpg headroom trunk turn weight length displacement gear_ratio {
    _swilk_row `fh' full auto `v'
}
* Edge-case sample sizes (first n rows, data order). headroom is tie-heavy.
foreach v in price headroom {
    foreach n in 3 4 5 7 11 12 {
        _swilk_row `fh' first`n' auto `v' if _n <= `n'
    }
}
use "fixtures/cohort.dta", clear
foreach v in index_age crp iptw follow_up cost_sek lab_value prior_hosp ctrl_marker bmi {
    foreach n in 3 4 5 7 11 12 2000 {
        _swilk_row `fh' first`n' cohort `v' if _n <= `n'
    }
}
* Two identical values and one distinct (G == 3 path with ties), and a constant.
preserve
clear
input double x
1
1
2
end
_swilk_row `fh' ties3 inline x
replace x = 5
_swilk_row `fh' const3 inline x
restore
file close `fh'

**# (ii) mt64 runiform() streams
file open `fh' using "rng_mt64.csv", write text replace
file write `fh' "seed,draw,u" _n
foreach spec in "12345 10000" "1 1000" "2147483647 1000" "20260324 1000" {
    gettoken seed n : spec
    clear
    quietly set obs `n'
    set seed `seed'
    quietly gen double u = runiform()
    forvalues i = 1/`n' {
        file write `fh' "`seed',`i'," %21.18f (u[`i']) _n
    }
}
file close `fh'

**# (iii) auto-types for the T03/T03b/T03c varlists, and the swilk subsample
file open `fh' using "autotype.csv", write text replace
file write `fh' "fixture,variable,n,nuniq,type,skewness,kurtosis" _n
foreach spec in "auto price mpg rep78 headroom trunk weight length turn displacement gear_ratio" ///
    "cohort3500 index_age crp prior_hosp iptw follow_up cost_sek lab_value bmi female education" ///
    "cohort index_age crp prior_hosp iptw follow_up cost_sek lab_value bmi female education" {
    gettoken fixture vars : spec
    use "fixtures/`fixture'.dta", clear
    foreach v of local vars {
        quietly count if !missing(`v')
        local n = r(N)
        _tabtools_detect_vartype `v'
        local type "`result'"
        local nuniq "`result_nuniq'"
        quietly summarize `v', detail
        _num `r(skewness)'
        local sk "`r(s)'"
        quietly summarize `v', detail
        _num `r(kurtosis)'
        local ku "`r(s)'"
        file write `fh' "`fixture',`v',`n',`nuniq',`type',`sk',`ku'" _n
    }
}
file close `fh'

* Replicates _tabtools_common.ado:376-393: keep non-missing rows in data
* order, set seed 12345, u = runiform(), sort (u, _n), first 2,000.
file open `fh' using "subsample_ids.csv", write text replace
file write `fh' "fixture,variable,rank,id,u" _n
use "fixtures/cohort3500.dta", clear
foreach v in index_age crp prior_hosp iptw follow_up cost_sek lab_value bmi {
    preserve
    quietly keep if !missing(`v')
    set seed 12345
    quietly gen double _u = runiform()
    quietly gen long _tie = _n
    sort _u _tie
    forvalues i = 1/2000 {
        file write `fh' "cohort3500,`v',`i'," (id[`i']) "," %21.18f (_u[`i']) _n
    }
    restore
}
file close `fh'
* swilk on each subsample, for the port's end-to-end check.
file open `fh' using "swilk.csv", write text append
foreach v in index_age crp prior_hosp iptw follow_up cost_sek lab_value bmi {
    preserve
    quietly keep if !missing(`v')
    set seed 12345
    quietly gen double _u = runiform()
    quietly gen long _tie = _n
    sort _u _tie
    _swilk_row `fh' subsample2000 cohort3500 `v' if _n <= 2000
    restore
}
file close `fh'

**# (iv) one row per branch of _tabtools_detect_vartype (_tabtools_common.ado:274-424)
clear
set seed 20260927
quietly set obs 6000
gen long row = _n
gen str1 v_string = substr("abc", 1 + mod(_n, 3), 1)
gen double v_allmiss = .
gen byte v_bin01 = mod(_n, 2)
gen byte v_bin12 = 1 + mod(_n, 2)
gen byte v_bin01_lab = mod(_n, 2)
label define v_bin01_lab 0 "No" 1 "Yes"
label values v_bin01_lab v_bin01_lab
gen byte v_lab2_nonbin = 1 + 2 * mod(_n, 2)
label define v_lab2_nonbin 1 "One" 3 "Three"
label values v_lab2_nonbin v_lab2_nonbin
gen byte v_lab3 = 1 + mod(_n, 3)
label define v_lab3 1 "A" 2 "B" 3 "C"
label values v_lab3 v_lab3
gen int v_lab12 = 1 + mod(_n, 12)
label define v_lab12 1 "L1" 12 "L12"
label values v_lab12 v_lab12
gen byte v_cat7 = 1 + mod(_n, 7)
gen double v_cont8_n8 = _n + 0.5 * (_n == 8) if _n <= 8
gen double v_big_normal = rnormal(50, 10)
gen double v_big_skew = exp(rnormal(0, 1))
gen double v_big_kurt = rnormal()^3
gen double v_mid_normal = rnormal(50, 10) if _n <= 3000
gen double v_mid_skew = exp(rnormal(0, 1)) if _n <= 3000
gen double v_small_normal = rnormal(50, 10) if _n <= 500
gen double v_small_skew = exp(rnormal(0, 1)) if _n <= 500
gen double v_small_ties = round(rnormal(3, 0.8), 0.5) if _n <= 200
label data "tabtools golden fixture: detect_vartype branch table"
compress
* .dta headers carry a save timestamp: rewrite only when the data changed.
capture cf _all using "fixtures/vartype_branches.dta"
if _rc save "fixtures/vartype_branches.dta", replace

file open `fh' using "detect_vartype.csv", write text replace
file write `fh' "variable,branch,n,nuniq,type" _n
foreach spec in "v_string string" "v_allmiss all_missing" "v_bin01 two_values_01" ///
    "v_bin12 two_values_not01" "v_bin01_lab two_values_01_labelled" ///
    "v_lab2_nonbin two_values_labelled_not01" "v_lab3 value_label" "v_lab12 value_label_many" ///
    "v_cat7 le7_unique" "v_cont8_n8 swilk_n8" "v_big_normal gt5000_moments" ///
    "v_big_skew gt5000_skew" "v_big_kurt gt5000_kurtosis" "v_mid_normal subsample_swilk" ///
    "v_mid_skew subsample_swilk" "v_small_normal swilk_all" "v_small_skew swilk_all" ///
    "v_small_ties swilk_ties" {
    gettoken v branch : spec
    local branch = strtrim("`branch'")
    quietly count if !missing(`v')
    local n = r(N)
    _tabtools_detect_vartype `v'
    file write `fh' "`v',`branch',`n',`result_nuniq',`result'" _n
}
file close `fh'
