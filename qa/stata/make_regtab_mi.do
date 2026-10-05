/*  make_regtab_mi.do - Stata expectations for tasks 5.14 (multiple
    imputation) and 5.17 (stats tokens), conditional task C4
    (IMPLEMENTATION_PLAN.md, "C4 outcome").

    mi_emat.csv   for each `mi estimate` fit on mi_cohort.dta (the golden
                  fixture, imported with -mi import flong-): e(b_mi),
                  e(V_mi), e(W_mi), e(B_mi), e(df_mi), e(rvi_mi), e(fmi_mi),
                  r(table) and the scalars e(M_mi), e(N), e(fmi_max_mi),
                  e(N_fail), e(N_sub). Long form: fit,what,row,col,value.
                    A1 regress (small-sample df)         A2 logit (large sample)
                    A3 logit, vce(robust)                A4 stcox
                    A5 regress, vce(cluster region)      A6 regress, vce(robust)
                    A7 regress, no imputed covariate (B = 0, small sample)
                    A8 logit, no imputed covariate (B = 0, large sample)
                    A9 glm, family(gaussian) (large sample: glm sets no e(df_r))
                    A10 poisson, irr
                    A11 mi estimate, level(90): regress (C4 review F3)
    stats.csv     e() of the completed-data commands behind the new stats()
                  tokens: regress (plain, robust, cluster with 3 clusters,
                  [aw], [pw], noconstant, constant only), svy: regress,
                  stcox after stset [pw] with id(), stcox, streg after
                  stset [fw], stcrreg (X1-X4), streg after stset [pw] (X5); the C4 review's probes
                  T1-T12 (noconstant, few clusters, zero [aw] weights,
                  collinear terms) and S10 (a singleton dummy under
                  vce(robust): the sandwich is singular and e(F) missing).
                  Long form: id,name,value.
    mi_m1.txt     the refusal of M = 1 (mi estimate, imputations(1)).

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_mi.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR is a `git archive` export of Stata-Tools f42ee9cd
    (tabtools 2.1.12). Read by tests/testthat/test-regtab-mi.R and
    test-regtab-stats-tokens.R.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/fvgen"
adopath ++ "`stata_tools'/tabtools"
which regtab
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
local out "tests/testthat/fixtures/regtab_mi"
capture mkdir "`out'"

capture program drop mi_dump_mat
program define mi_dump_mat
    args fh fit what mat
    tempname M
    capture matrix `M' = `mat'
    if _rc exit
    local rn : rowfullnames `M'
    local cn : colfullnames `M'
    forvalues i = 1/`=rowsof(`M')' {
        forvalues j = 1/`=colsof(`M')' {
            local r : word `i' of `rn'
            local c : word `j' of `cn'
            file write `fh' "`fit',`what',`r',`c'," %21.17g (`M'[`i', `j']) _n
        }
    }
end

capture program drop mi_dump
program define mi_dump
    args fh fit
    tempname T
    matrix `T' = r(table)
    foreach m in b V W B df rvi fmi {
        mi_dump_mat `fh' `fit' `m' e(`m'_mi)
    }
    mi_dump_mat `fh' `fit' table `T'
    foreach s in M_mi N fmi_max_mi N_fail N_sub {
        file write `fh' "`fit',scalar,`s',," %21.17g (e(`s')) _n
    }
end

tempname fh
file open `fh' using "`out'/mi_emat.csv", write text replace
file write `fh' "fit,what,row,col,value" _n

use "tests/testthat/golden/fixtures/mi_cohort.dta", clear
mi import flong, m(mi_m) id(id) imputed(index_age education) clear
local rhs "treated index_age female i.education"
quietly mi estimate: regress bmi `rhs'
mi_dump `fh' A1
quietly mi estimate: logit cv_event `rhs'
mi_dump `fh' A2
quietly mi estimate: logit cv_event `rhs', vce(robust)
mi_dump `fh' A3
quietly mi estimate: regress bmi `rhs', vce(cluster region)
mi_dump `fh' A5
quietly mi estimate: regress bmi `rhs', vce(robust)
mi_dump `fh' A6
quietly mi estimate: regress bmi treated female
mi_dump `fh' A7
quietly mi estimate: logit cv_event treated female
mi_dump `fh' A8
quietly mi estimate: glm bmi `rhs', family(gaussian)
mi_dump `fh' A9
quietly mi estimate: poisson cv_event `rhs', irr
mi_dump `fh' A10
quietly mi estimate, level(90): regress bmi `rhs'
mi_dump `fh' A11
capture noisily mi estimate, imputations(1): regress bmi `rhs'
local rc1 = _rc
quietly mi stset follow_up, failure(cv_event)
quietly mi estimate: stcox `rhs'
mi_dump `fh' A4
file close `fh'
file open `fh' using "`out'/mi_m1.txt", write text replace
file write `fh' "`rc1'" _n
file close `fh'

* ---------------------------------------------------------------------------
* stats tokens: the completed-data e() results
capture program drop st_dump
program define st_dump
    args fh id
    foreach s in N r2 r2_a rmse F df_m df_r rank N_fail N_sub N_clust {
        file write `fh' "`id',`s'," %21.17g (e(`s')) _n
    }
end
file open `fh' using "`out'/stats.csv", write text replace
file write `fh' "id,name,value" _n
sysuse auto, clear
generate byte c3 = mod(_n, 3)
quietly regress price mpg weight
st_dump `fh' S1
quietly regress price mpg weight, vce(robust)
st_dump `fh' S2
quietly regress price mpg weight, vce(cluster c3)
st_dump `fh' S3
quietly regress price mpg, vce(cluster c3)
st_dump `fh' S3b
quietly regress price mpg weight [aw=turn]
st_dump `fh' S4
quietly regress price mpg weight [pw=turn]
st_dump `fh' S5
quietly regress price mpg weight, noconstant
st_dump `fh' S6
quietly regress price
st_dump `fh' S7
quietly regress price mpg weight turn, vce(cluster rep78)
st_dump `fh' S8
generate double w = turn / 40
svyset _n [pw=w]
quietly svy: regress price mpg weight
st_dump `fh' S9
generate byte single = _n == 1
quietly regress price mpg single, vce(robust)
st_dump `fh' S10
* The C4 review's probes (review-c4/probe/stats.do).
generate byte c4 = mod(_n, 4)
generate double w0 = cond(mod(_n, 7) == 0, 0, turn/40)
quietly regress price mpg weight, noconstant vce(cluster c3)
st_dump `fh' T1
quietly regress price mpg weight turn, vce(cluster c4)
st_dump `fh' T2
quietly regress price mpg weight, vce(cluster c4)
st_dump `fh' T3
quietly regress price mpg weight [aw=w0]
st_dump `fh' T4
quietly regress price mpg weight [pw=w0], vce(cluster c4)
st_dump `fh' T5
quietly regress price mpg weight, noconstant vce(robust)
st_dump `fh' T6
quietly regress price mpg weight i.foreign i.rep78
st_dump `fh' T7
generate mpg2 = 2*mpg
quietly regress price mpg mpg2 weight
st_dump `fh' T8
quietly regress price mpg mpg2 weight, vce(robust)
st_dump `fh' T9
quietly regress price, vce(robust)
st_dump `fh' T11
quietly regress price mpg, noconstant vce(cluster c3)
st_dump `fh' T12
use "tests/testthat/golden/fixtures/cohort.dta", clear
keep in 1/3000
quietly stset follow_up [pw=iptw], failure(cv_event) id(id)
quietly stcox treated index_age
st_dump `fh' X1
quietly stset follow_up, failure(cv_event)
quietly stcox treated index_age
st_dump `fh' X2
generate fw2 = 1 + mod(_n, 3)
quietly stset follow_up [fw=fw2], failure(cv_event)
quietly streg treated index_age, distribution(weibull)
st_dump `fh' X3
quietly stset follow_up, failure(event_type == 1)
quietly stcrreg treated index_age, compete(event_type == 2)
st_dump `fh' X4
keep in 1/2000
quietly stset follow_up [pw=iptw], failure(cv_event)
quietly streg treated index_age, distribution(weibull)
st_dump `fh' X5
file close `fh'
