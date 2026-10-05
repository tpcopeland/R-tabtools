/*  make_regtab_vce.do - Stata expectations for regtab's weighted and robust
    variance (milestone 5w, tasks 5.9-5.13; IMPLEMENTATION_PLAN.md,
    "Milestone 5w findings").

    Run from the repo root:
        stata-mp -b do qa/stata/make_regtab_vce.do
    Only built-in estimation commands are used (no tabtools), so the result
    does not depend on the tabtools checkout.

    Writes tests/testthat/fixtures/regtab_vce/:
      vce_probe.csv  probe, term, b, se, N, N_clust, df_r, vce, vcetype, ll
      vce_V.csv      probe, row, col, value: every element of e(V)
      margins.csv    probe, term, b, se (margins after logit [pw])
    Every number %21.17g.
*/

version 17.0
clear all
set more off
set varabbrev off

local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_vce"
capture mkdir "`out'"

capture program drop probe
program define probe
    args name
    tempname b V
    matrix `b' = e(b)
    matrix `V' = e(V)
    local names : colfullnames `b'
    local k = colsof(`b')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        local se = sqrt(`V'[`j',`j'])
        file write fh "`name',`nm'," %21.17g (`b'[1,`j']) "," %21.17g (`se') "," ///
            %21.17g (e(N)) "," %21.17g (e(N_clust)) "," %21.17g (e(df_r)) "," ///
            "`e(vce)'" "," "`e(vcetype)'" "," %21.17g (e(ll)) _n
        forvalues i = 1/`k' {
            local rn : word `i' of `names'
            file write fv "`name',`rn',`nm'," %21.17g (`V'[`i',`j']) _n
        }
    }
end

file open fh using "`out'/vce_probe.csv", write text replace
file write fh "probe,term,b,se,N,N_clust,df_r,vce,vcetype,ll" _n
file open fv using "`out'/vce_V.csv", write text replace
file write fv "probe,row,col,value" _n
file open fm using "`out'/margins.csv", write text replace
file write fm "probe,term,b,se" _n

**# cohort (N = 15,000)
use "`fx'/cohort.dta", clear
gen double fu_y = follow_up/365.25
* Zero weights for every tenth subject: Stata drops zero-weight
* observations from a [pw] fit, so N and N/(N-1) exclude them.
gen double w0 = iptw * (mod(id, 10) != 0)

quietly logit cv_event treated index_age female [pw=iptw]
probe V01_logit_pw
quietly logit cv_event treated index_age female, vce(robust)
probe V02_logit_robust
quietly poisson cv_event treated index_age female [pw=iptw], exposure(fu_y)
probe V03_poisson_pw
quietly regress cv_event treated index_age female [pw=iptw]
probe V04_regress_pw
quietly regress index_age treated female cv_event, vce(robust)
probe V05_regress_robust
quietly probit cv_event treated index_age female [pw=iptw]
probe V06_probit_pw
quietly glm cv_event treated index_age female [pw=iptw], family(binomial) link(logit)
probe V07_glm_logit_pw
quietly glm index_age treated female cv_event [pw=iptw], family(gaussian) link(identity)
probe V08_glm_gauss_pw
quietly glm cv_event treated index_age female [pw=iptw], family(binomial) link(cloglog)
probe V09_glm_cloglog_pw
quietly logit cv_event treated index_age female, vce(cluster education)
probe V10_logit_cl_small
quietly regress index_age treated female cv_event [pw=iptw], vce(cluster education)
probe V11_regress_pw_cl_small
quietly regress index_age treated female cv_event, vce(cluster education)
probe V12_regress_cl_small
quietly logit cv_event treated index_age female [pw=w0]
probe V13_logit_pw_zero
quietly regress cv_event treated index_age female [pw=w0]
probe V14_regress_pw_zero
quietly poisson cv_event treated index_age female [pw=iptw], exposure(fu_y) vce(cluster education)
probe V15_poisson_pw_cl_small
quietly glm index_age treated female cv_event [pw=iptw], family(gaussian) link(identity) vce(cluster education)
probe V16_glm_gauss_pw_cl_small
quietly regress cv_event treated index_age female [aw=iptw]
probe V17_regress_aw

* Cox: crude (oim), pw (robust implied, clusters on id)
stset fu_y, failure(cv_event) id(id)
quietly stcox treated index_age female, breslow
probe V20_stcox_oim
quietly stcox treated index_age female, breslow vce(robust)
probe V21_stcox_robust
quietly stcox treated index_age female, breslow vce(cluster education)
probe V22_stcox_cl_small
stset fu_y [pw=iptw], failure(cv_event) id(id)
quietly stcox treated index_age female, breslow
probe V23_stcox_pw
stset, clear

* svy: design df
preserve
svyset _n [pw=iptw]
quietly svy: logit cv_event treated index_age female
probe V30_svy_logit
quietly svy: regress index_age treated female cv_event
probe V31_svy_regress
quietly svy: poisson cv_event treated index_age female, exposure(fu_y)
probe V32_svy_poisson
restore
preserve
svyset education [pw=iptw]
quietly svy: logit cv_event treated index_age female
probe V33_svy_logit_psu_small
restore

* margins after logit [pw] (the bridge to effecttab, Phase 7d)
quietly logit cv_event i.treated index_age female [pw=iptw]
probe V40_logit_pw_factor
quietly margins, dydx(treated)
matrix T = r(table)
local cn : colfullnames T
file write fm "M01_dydx,`: word 2 of `cn''," %21.17g (T[1,2]) "," %21.17g (T[2,2]) _n
quietly logit cv_event i.treated index_age female [pw=iptw]
quietly margins treated
matrix T = r(table)
file write fm "M02_margins,0.treated," %21.17g (T[1,1]) "," %21.17g (T[2,1]) _n
file write fm "M02_margins,1.treated," %21.17g (T[1,2]) "," %21.17g (T[2,2]) _n
quietly logit cv_event i.treated index_age female [pw=iptw]
quietly margins, dydx(index_age)
matrix T = r(table)
file write fm "M03_dydx_age,index_age," %21.17g (T[1,1]) "," %21.17g (T[2,1]) _n
quietly logit cv_event i.treated index_age female, vce(cluster education)
quietly margins, dydx(treated)
matrix T = r(table)
file write fm "M04_dydx_cluster,1.treated," %21.17g (T[1,2]) "," %21.17g (T[2,2]) _n

* teffects ipw: the only Stata analogue of WeightIt::glm_weightit (task
* 5.11): an identity-link y ~ treat fit on ATE weights is the ATE, and
* its M-estimation variance accounts for the estimated propensity score.
quietly teffects ipw (cv_event) (treated index_age female i.education diabetes hypertension anxiety), ate
probe V60_teffects_ipw_ate

**# cohort_pp (N = 151,458 person-periods, G = 15,000)
use "`fx'/cohort_pp.dta", clear
quietly logit ev treated index_age female i.period [pw=iptw], vce(cluster id)
probe V50_logit_pw_cl
quietly regress ev treated index_age female i.period [pw=iptw], vce(cluster id)
probe V51_regress_pw_cl
quietly poisson ev treated index_age female i.period [pw=iptw], exposure(pt) vce(cluster id)
probe V52_poisson_pw_cl
quietly glm ev treated index_age female i.period [pw=iptw], family(binomial) link(logit) vce(cluster id)
probe V53_glm_pw_cl
preserve
svyset id [pw=iptw]
quietly svy: logit ev treated index_age female i.period
probe V54_svy_logit_psu
restore
quietly xtset id period
quietly xtgee ev treated index_age female i.period, family(binomial) link(logit) corr(independent) vce(robust)
probe V55_xtgee_ind_robust
quietly xtgee ev treated index_age female i.period, family(binomial) link(logit) corr(exchangeable) vce(robust)
probe V56_xtgee_exch_robust

file close fh
file close fv
file close fm
