/*  make_regtab_phase5a.do - Stata expectations for regtab Phase 5a
    (IMPLEMENTATION_PLAN.md, "Phase 5a findings"): ordered and multinomial
    logit, zero-inflated counts, parametric survival, Fine-Gray, and the
    churdle escape hatch. Not golden scenarios; tests read them from
    tests/testthat/fixtures/regtab_phase5a/:
      oim_se.csv    e(b) and sqrt(diag(e(V))) at tight convergence (the MLE
                    R should reproduce) for each model family, %23.16e;
                    stcrreg with its default vce(robust)
      stats.csv     e(N), e(N_sub), e(ll), e(ll_0), e(rank), e(r2_p)
      R25t_tidy.csv churdle (golden R25t, default convergence): r(table)
                    rows (equation, term, b, se, p, ll, ul) and the
                    variable labels, the input of golden_tidy("R25t")
      R25t_stats.csv  its e(N), e(ll), e(rank), e(r2_p)
      <id>.csv      regtab csv() sinks for keepintercept / factor layouts
                    no golden covers (cutpoints, equation intercepts,
                    ancillary rows, multi-equation factor levels)

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_phase5a.do [STATA_TOOLS_DIR]
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/fvgen"
adopath ++ "`stata_tools'/tabtools"
discard
which regtab
set linesize 255
local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_phase5a"
capture mkdir "`out'"
local tight "tolerance(1e-14) ltolerance(1e-14) nrtolerance(1e-12)"

capture program drop dumpb
program define dumpb
    args fh tag
    tempname b V
    matrix `b' = e(b)
    matrix `V' = e(V)
    local names : colfullnames `b'
    local k = colsof(`b')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        file write `fh' "`tag',`nm'," %23.16e (`b'[1,`j']) "," %23.16e (sqrt(`V'[`j',`j'])) _n
    }
end

capture program drop dumps
program define dumps
    args fh tag
    local nsub = cond("`e(N_sub)'" == "", ., e(N_sub))
    local ll0 = cond("`e(ll_0)'" == "", ., e(ll_0))
    local r2p = cond("`e(r2_p)'" == "", ., e(r2_p))
    file write `fh' "`tag'," %23.16e (e(N)) "," %23.16e (`nsub') "," %23.16e (e(ll)) "," ///
        %23.16e (`ll0') "," %23.16e (e(rank)) "," %23.16e (`r2p') _n
end

tempname fh fs
file open `fh' using "`out'/oim_se.csv", write replace
file write `fh' "model,term,b,se" _n
file open `fs' using "`out'/stats.csv", write replace
file write `fs' "model,N,N_sub,ll,ll_0,rank,r2_p" _n

use "`fx'/cohort.dta", clear
ologit education index_age female diabetes hypertension, `tight'
dumpb `fh' ologit
dumps `fs' ologit
oprobit education index_age female diabetes hypertension, `tight'
dumpb `fh' oprobit
dumps `fs' oprobit
mlogit education index_age female diabetes hypertension, baseoutcome(1) `tight'
dumpb `fh' mlogit
dumps `fs' mlogit
stset follow_up, failure(cv_event)
streg treated index_age female, distribution(weibull) time `tight'
dumpb `fh' weibull
dumps `fs' weibull
streg treated index_age female, distribution(lognormal) `tight'
dumpb `fh' lognormal
dumps `fs' lognormal
streg treated index_age female, distribution(loglogistic) `tight'
dumpb `fh' loglogistic
dumps `fs' loglogistic
streg treated index_age female, distribution(exponential) time `tight'
dumpb `fh' exponential
dumps `fs' exponential

use "`fx'/cohort3500.dta", clear
stset follow_up, failure(event_type == 1)
stcrreg treated index_age female, compete(event_type == 2)
dumpb `fh' stcrreg
dumps `fs' stcrreg

use "`fx'/zip.dta", clear
zip event_count treatment age_z female, inflate(zero_risk female) `tight'
dumpb `fh' zip
dumps `fs' zip
zinb event_count treatment age_z female, inflate(zero_risk female) `tight'
dumpb `fh' zinb
dumps `fs' zinb

use "`fx'/hurdle.dta", clear
churdle linear annual_cost dose_intensity, select(participation_score) ll(0) `tight'
dumpb `fh' churdle
dumps `fs' churdle
file close `fh'
file close `fs'

* golden_tidy("R25t"): the churdle fit of golden R25t at default
* convergence, as r(table) reports it.
churdle linear annual_cost dose_intensity, select(participation_score) ll(0)
tempname T
matrix `T' = r(table)
local names : colfullnames `T'
local k = colsof(`T')
file open `fh' using "`out'/R25t_tidy.csv", write replace
file write `fh' "equation,term,var_label,estimate,std.error,p.value,conf.low,conf.high" _n
forvalues j = 1/`k' {
    local nm : word `j' of `names'
    gettoken eq term : nm, parse(":")
    local term = substr("`term'", 2, .)
    local vl ""
    capture local vl : variable label `term'
    file write `fh' `""`eq'","`term'","`vl'","' %23.16e (`T'[1,`j']) "," %23.16e (`T'[2,`j']) "," ///
        %23.16e (`T'[4,`j']) "," %23.16e (`T'[5,`j']) "," %23.16e (`T'[6,`j']) _n
}
file close `fh'
file open `fs' using "`out'/R25t_stats.csv", write replace
file write `fs' "model,N,N_sub,ll,ll_0,rank,r2_p" _n
dumps `fs' churdle
file close `fs'

* Layouts no golden covers, as regtab's csv() sink.
use "`fx'/cohort.dta", clear
collect clear
quietly collect: ologit education index_age i.smoking female
regtab, keepintercept csv("`out'/ologit_keep.csv")
collect clear
quietly collect: mlogit education index_age i.smoking female, baseoutcome(1)
regtab, keepintercept csv("`out'/mlogit_keep.csv")
stset follow_up, failure(cv_event)
collect clear
quietly collect: streg treated index_age female, distribution(weibull) time
regtab, keepintercept stats(n ll aic bic) csv("`out'/weibull_keep.csv")
collect clear
quietly collect: streg treated index_age i.smoking female, distribution(loglogistic)
regtab, keepintercept csv("`out'/loglogistic_keep.csv")
use "`fx'/zip.dta", clear
collect clear
quietly collect: zinb event_count treatment age_z female, inflate(zero_risk female)
regtab, keepintercept stats(n ll) csv("`out'/zinb_keep.csv")
collect clear
quietly collect: zip event_count treatment age_z female, inflate(_cons)
regtab, keepintercept csv("`out'/zip_cons_keep.csv")

* cdisc (plan task 5.7) on each new family: 4 digits, Estimate header,
* stats(n).
use "`fx'/cohort.dta", clear
collect clear
quietly collect: ologit education index_age female diabetes hypertension
regtab, cdisc csv("`out'/cdisc_ologit.csv")
collect clear
quietly collect: mlogit education index_age female diabetes hypertension, baseoutcome(1)
regtab, cdisc csv("`out'/cdisc_mlogit.csv")
stset follow_up, failure(cv_event)
collect clear
quietly collect: streg treated index_age female, distribution(weibull) time
regtab, cdisc csv("`out'/cdisc_weibull.csv")
use "`fx'/cohort3500.dta", clear
stset follow_up, failure(event_type == 1)
collect clear
quietly collect: stcrreg treated index_age female, compete(event_type == 2)
regtab, cdisc csv("`out'/cdisc_stcrreg.csv")
use "`fx'/zip.dta", clear
collect clear
quietly collect: zip event_count treatment age_z female, inflate(zero_risk female)
quietly collect: zinb event_count treatment age_z female, inflate(zero_risk female)
regtab, cdisc csv("`out'/cdisc_zip.csv")

* The automatic nointercept keeps lnsigma/sigma and lngamma/gamma (its
* label rule drops only /..., alpha, lnalpha, ln_p, p and 1/p).
use "`fx'/cohort.dta", clear
stset follow_up, failure(cv_event)
collect clear
quietly collect: streg treated index_age female, distribution(lognormal)
regtab, csv("`out'/lognormal_default.csv")
collect clear
quietly collect: streg treated index_age female, distribution(loglogistic) time
regtab, csv("`out'/loglogistic_time_default.csv")
