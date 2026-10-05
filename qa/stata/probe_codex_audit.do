/*  probe_codex_audit.do - Stata's behaviour on the inputs of the Codex audit
    findings D1, D2, D4 and D5 (codexaudit.md; plan "Codex audit outcome").

    Run on a `git archive` export of a Stata-Tools commit, never the live
    checkout, from the repo root:
      git -C ~/Stata-Tools archive <commit> tabtools | tar -x -C <DIR>
      stata-mp -b do qa/stata/probe_codex_audit.do <DIR>
    (Stata then names the log after the last word of <DIR>.)
    Results on f42ee9cd (tabtools 2.1.12; Stata 17, 2026-09-26), before the
    Stata fixes of 68c37a90 (see the C7 results at the end):
    - D1: table1_tc, vars(x conts) wt(w) on x = 1..10 prints 6 (3, 8) for
      w = 1 and w = 1e-9, but 2 (2, 2) for w = 1e-12 (the quantile walk's
      absolute tolerance of 1e-10). Not replicated (H-D1): R prints 6 (3, 8).
    - D2: Stata has no wttab. msm_weight's ESS lines, (sum w)^2 / sum(w^2)
      on raw weights, give 2.5714 for w = 1, 2, 3 but . (missing) for the
      same weights times 1e-200 (sum w^2 underflows to 0) and times 1e200
      (w^2 is missing). R's wttab computes 18/7 at every scale.
    - D4: strate with a grouping variable named Y, D or Rate works (its
      results are _D, _Y, _Rate, _Lower, _Upper); one named _Y, _D, _Rate or
      _Lower is refused, "variable _Y already defined", r(110). R's tt_rates
      names its results without the underscore and refuses those names.
      The file written for `strate Y` is
      tests/testthat/fixtures/codex_audit/strate_by_Y.dta.
    - D5: summarize, detail of 6,000 values _n * 1e100 returns skewness and
      kurtosis missing; table1_tc's automatic typing then reads `. > 1` as
      true and types x conts (median (Q1, Q3)). R standardises before the
      moments (skewness 0, kurtosis 1.8) and types contn.
    Results on 68c37a90 (still labelled 2.1.13; Stata 17, 2026-09-27, task
    C7): D1 prints 6 (3, 8) at every weight scale, as R. D5: summarize still
    returns missing moments for the raw values, but table1_tc's typing
    scales them first and types x contn (Mean+-SD), as R; 3,000 values
    _n and _n * 1e100 (the Shapiro-Wilk branch) both type conts, as R since
    C7 (it used to type the scaled one contn, a failed test).
*/
version 17.0
args dir
if "`dir'" == "" local dir "~/Stata-Tools"
clear all
set more off
adopath ++ "`dir'/tabtools"
which table1_tc

* D1
clear
set obs 10
gen double x = _n
gen double w = 1
table1_tc, vars(x conts) wt(w)
replace w = 1e-12
table1_tc, vars(x conts) wt(w)
replace w = 1e-9
table1_tc, vars(x conts) wt(w)

* D2 (msm_weight.ado's ESS lines)
foreach s in 1 1e-200 1e200 {
    clear
    set obs 3
    gen double w = _n * `s'
    quietly summarize w
    local sum_w = r(sum)
    tempvar w2
    gen double `w2' = w^2
    quietly summarize `w2'
    local sum_w2 = r(sum)
    local ess = (`sum_w'^2) / `sum_w2'
    display "D2 scale `s': sum_w=`sum_w' sum_w2=`sum_w2' ess=`ess'"
}

* D4
foreach v in Y D Rate _Y _D _Rate _Lower {
    clear
    set obs 4
    gen double t = _n + 1
    gen double e = mod(_n + 1, 2)
    gen byte `v' = cond(_n <= 2, 1, 2)
    quietly stset t, failure(e)
    capture noisily strate `v', output("probe_D4_`v'.dta", replace)
    display "rc D4 `v' = " _rc
}

* D5
clear
set obs 6000
gen double x = _n * 1e100
quietly summarize x, detail
display "D5 skew=" r(skewness) " kurt=" r(kurtosis)
table1_tc, vars(x)
* D5, Shapiro-Wilk branch (C7, Stata-Tools 68c37a90): 3,000 values, raw
* and times 1e100; the type is the descriptor of the row.
foreach sc in 1 1e100 {
    clear
    set obs 3000
    gen double x = _n * `sc'
    display "D5sw scale `sc'"
    table1_tc, vars(x)
}
