/*  make_rates_goldens.do - goldens for tt_rates() (plan task 7.3)

    Run through make_golden.R (cwd = tests/testthat/golden). Runs Stata 17's
    strate on the cohort fixture and writes rates_strate.csv: one row per
    scenario x group with _D, _Y, _Rate, _Lower, _Upper at %23.16e (17 significant digits at any magnitude), read back
    from strate's own output() file. Group keys are the by-variables' codes
    ("." for missing), joined by ";" in varlist order.

    Scenarios:
      S1  by treated, per(1000)
      S2  by education, per(1000), level(90)
      S3  by treated education (two variables), per(365.25)
      S4  overall (no varlist), default per(1)
      S5  by smoking, missing (missing smoking kept as a group), per(1000)
      S6  by smoking without missing (missing rows dropped), per(1000)
      S7  stset enter(time 365): late entry, _t0 > 0, rows ending before
          entry excluded; by treated, per(1000)
      S8  stset [fweight=fw]: frequency-weighted events and person-time
      S9  by ctrl_grade, a control-only variable (treated rows drop as
          missing), per(1000)
*/

version 17.0

capture program drop _rate_run
program define _rate_run
    args fh id byvars stata_opts
    tempfile out
    quietly strate `byvars', `stata_opts' output("`out'", replace) nolist
    preserve
    quietly use "`out'", clear
    forvalues i = 1/`=_N' {
        local key ""
        foreach v of local byvars {
            local val = `v'[`i']
            local key "`key'`=cond("`key'" == "", "", ";")'`val'"
        }
        if "`byvars'" == "" local key "all"
        file write `fh' "`id',`byvars',`key'"
        * Values go straight from the variable to the file: a local macro
        * would round them (12 significant digits in e-notation).
        foreach s in _D _Y _Rate _Lower _Upper {
            if missing(`s'[`i']) file write `fh' ",."
            else file write `fh' "," (strtrim(string(`s'[`i'], "%23.16e")))
        }
        file write `fh' _n
    }
    restore
end

tempname fh
file open `fh' using "rates_strate.csv", write text replace
file write `fh' "scenario,byvars,group,D,Y,Rate,Lower,Upper" _n

use "fixtures/cohort.dta", clear
quietly stset follow_up, failure(cv_event)
_rate_run `fh' S1 "treated" "per(1000)"
_rate_run `fh' S2 "education" "per(1000) level(90)"
_rate_run `fh' S3 "treated education" "per(365.25)"
_rate_run `fh' S4 "" ""
_rate_run `fh' S5 "smoking" "per(1000) missing"
_rate_run `fh' S6 "smoking" "per(1000)"
_rate_run `fh' S9 "ctrl_grade" "per(1000)"

quietly stset follow_up, failure(cv_event) enter(time 365)
_rate_run `fh' S7 "treated" "per(1000)"

quietly stset follow_up [fweight=fw], failure(cv_event)
_rate_run `fh' S8 "treated" "per(1000)"
file close `fh'
