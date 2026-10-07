/*  golden_stratetab_blocks.do - strate-shaped block files for the stratetab
    goldens (IMPLEMENTATION_PLAN.md task 7.6, S scenarios)

    Run from a scenario's stata_setup (cwd = tests/testthat/golden):
        run "../../../qa/stata/golden_stratetab_blocks.do"
        golden_strate_blocks S01 "../../../qa/.stata-work/S01"

    Writes block k of the scenario as <prefix>_<k>.dta, one file per
    outcome x exposure block, in the shape `strate, output()` saves: a
    grouping variable `group` (absent for cat_type "none"), then _D _Y
    _Rate _Lower _Upper, with strate's interval labels ("Lower 95%
    confidence limit") when label_level is set.

    The numbers come from tests/testthat/fixtures/stratetab_blocks.csv,
    which the R twin (golden_strate_blocks() in helper-golden-stratetab.R)
    reads too, so both sides format the same blocks. cat_type: "str" a
    string category, "lab" a numeric code with the value label `cat`,
    "num" an unlabelled numeric code (blank = missing), "none" no grouping
    variable (strate without a varlist: one overall row).
*/

version 17.0

capture program drop golden_strate_blocks
program define golden_strate_blocks
    version 17.0
    args scenario prefix
    preserve
    quietly import delimited using "../fixtures/stratetab_blocks.csv", clear ///
        varnames(1) case(preserve) asdouble stringcols(1 4 5 12)
    quietly keep if scenario == "`scenario'"
    if _N == 0 {
        display as error "no blocks for scenario `scenario'"
        exit 459
    }
    quietly levelsof block, local(blocks)
    tempfile all
    quietly save `all'
    foreach b of local blocks {
        quietly use `all', clear
        quietly keep if block == `b'
        sort row
        local type = cat_type[1]
        local lev = label_level[1]
        if "`type'" == "str" {
            quietly generate str20 group = cat
        }
        else if "`type'" == "num" {
            quietly generate double group = code
        }
        else if "`type'" == "lab" {
            quietly generate double group = code
            capture label drop _golden_strate_`b'
            forvalues i = 1/`=_N' {
                label define _golden_strate_`b' `=code[`i']' `"`=cat[`i']'"', modify
            }
            label values group _golden_strate_`b'
        }
        rename (D Y Rate Lower Upper) (_D _Y _Rate _Lower _Upper)
        if "`type'" == "none" keep _D _Y _Rate _Lower _Upper
        else keep group _D _Y _Rate _Lower _Upper
        if "`lev'" != "" {
            label variable _Lower "Lower `lev'% confidence limit"
            label variable _Upper "Upper `lev'% confidence limit"
        }
        quietly save "`prefix'_`b'.dta", replace
    }
    restore
end
