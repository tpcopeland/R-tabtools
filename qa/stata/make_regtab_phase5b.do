/*  make_regtab_phase5b.do - Stata expectations for Phase 5b (regtab with
    mixed models and GEE, IMPLEMENTATION_PLAN.md "Phase 5b findings") that
    the golden scenarios R21-R23, R25f, R25i do not cover:
      slope.dta        simulated two-level data with a real random slope
                       (seed 12345), for me* random-slope models
      re_params.csv    r(table) of each model (b, se, ll, ul, pvalue) and
                       sqrt(diag(e(V))), written %23.16e
      re_stats.csv     regtab's r() scalars and r(table) row names
      <tag>.csv        regtab's own CSV sink for each call below

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_phase5b.do [STATA_TOOLS_DIR]
    tests/testthat/test-regtab-mixed.R and test-regtab-gee.R read them.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
which regtab
set linesize 255
local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_phase5b"
capture mkdir "`out'"

capture program drop dumpt
program define dumpt
    args fh tag
    tempname T V
    matrix `T' = r(table)
    matrix `V' = e(V)
    local names : colfullnames `T'
    local k = colsof(`T')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        file write `fh' `"`tag',"`nm'","' %23.16e (`T'[1,`j']) "," %23.16e (`T'[2,`j']) "," ///
            %23.16e (`T'[5,`j']) "," %23.16e (`T'[6,`j']) "," %23.16e (`T'[4,`j']) "," ///
            %23.16e (sqrt(`V'[`j',`j'])) _n
    }
end

capture program drop dumpr
program define dumpr
    args fh tag
    foreach s in n_1 groups_1 icc_1 aic_1 qic_1 N_rows {
        capture confirm scalar r(`s')
        if !_rc file write `fh' "`tag',scalar,`s'," %23.16e (r(`s')) _n
    }
    tempname T
    capture matrix `T' = r(table)
    if !_rc {
        local rn : rownames `T'
        local i = 0
        foreach r of local rn {
            local ++i
            file write `fh' `"`tag',rowname,`i',"`r'""' _n
        }
    }
end

tempname fp fs
file open `fp' using "`out'/re_params.csv", write replace
file write `fp' "model,param,b,se,ll,ul,p,se_V" _n
file open `fs' using "`out'/re_stats.csv", write replace
file write `fs' "call,kind,name,value" _n

* --- mixed with an unstructured random slope (mixed_bp) -------------------
use "`fx'/mixed_bp.dta", clear
collect clear
quietly collect: mixed y age female bmi || region: age, cov(unstructured)
quietly mixed
dumpt `fp' mixed_slope
regtab, coef("Coef.") stats(n groups icc) csv("`out'/slope_raw.csv")
dumpr `fs' slope_raw
regtab, coef("Coef.") stats(n groups icc) relabel csv("`out'/slope_relabel.csv")
dumpr `fs' slope_relabel
regtab, coef("Coef.") keep(age) csv("`out'/slope_keep.csv")
regtab, coef("Coef.") drop(age) csv("`out'/slope_drop.csv")
regtab, coef("Coef.") noreeffects stats(icc) csv("`out'/slope_nore.csv")
dumpr `fs' slope_nore

* --- mixed, REML ---------------------------------------------------------
collect clear
quietly collect: mixed y age female bmi || region:, reml
quietly mixed
dumpt `fp' mixed_reml
regtab, coef("Coef.") stats(n groups aic icc) relabel csv("`out'/reml_relabel.csv")
dumpr `fs' reml_relabel

* --- three-level mixed (nhanes2, zone > location) --------------------------
use "`fx'/nhanes2.dta", clear
generate byte zone = ceil(location / 10)
label variable zone "Zone"
collect clear
quietly collect: mixed bmi age i.sex || zone: || location:
quietly mixed
dumpt `fp' mixed_3lvl
regtab, coef("Coef.") stats(n groups icc) csv("`out'/lvl3_raw.csv")
dumpr `fs' lvl3_raw
regtab, coef("Coef.") stats(n groups icc) relabel csv("`out'/lvl3_relabel.csv")

* --- melogit (R22) with noreeffects; mepoisson (ICC undefined) -----------
collect clear
quietly collect: melogit highbp age i.sex || location:
quietly melogit
dumpt `fp' melogit
regtab, stats(n icc groups) noreeffects csv("`out'/melogit_nore.csv")
dumpr `fs' melogit_nore
collect clear
quietly collect: mepoisson highbp age i.sex || location:
quietly mepoisson
dumpt `fp' mepoisson
regtab, stats(n icc groups) csv("`out'/mepois_raw.csv")
dumpr `fs' mepois_raw
regtab, stats(n icc groups) relabel csv("`out'/mepois_relabel.csv")
dumpr `fs' mepois_relabel

* --- me* random slopes (simulated data; Laplace, as glmer) ----------------
clear
set seed 12345
set obs 40
generate clinic = _n
label variable clinic "Clinic ID"
generate u0 = rnormal(0, 0.8)
generate u1 = rnormal(0, 0.5)
expand 60
sort clinic, stable
generate x = rnormal()
label variable x "Exposure score"
generate z = rnormal()
generate byte yb = runiform() < invlogit(-0.3 + u0 + (0.7 + u1) * x + 0.2 * z)
generate yc = 1 + u0 + (0.7 + u1) * x + 0.2 * z + rnormal()
drop u0 u1
label data "tabtools Phase 5b fixture: simulated random-slope data"
save "`out'/slope.dta", replace
collect clear
quietly collect: melogit yb x z || clinic: x, cov(unstructured) intmethod(laplace)
quietly melogit
dumpt `fp' melogit_slope
regtab, stats(icc groups) csv("`out'/meslope_raw.csv")
dumpr `fs' meslope_raw
regtab, stats(icc groups) relabel csv("`out'/meslope_relabel.csv")
dumpr `fs' meslope_relabel
collect clear
quietly collect: mixed yc x z || clinic: x, cov(unstructured)
quietly mixed
dumpt `fp' mixed_slope2
regtab, stats(icc) relabel csv("`out'/slope2_relabel.csv")
dumpr `fs' slope2_relabel

* --- xtgee (union) ----------------------------------------------------------
use "`fx'/union.dta", clear
xtset idcode year
quietly xtgee union age grade not_smsa south, family(binomial) link(logit) corr(exchangeable)
quietly xtgee
dumpt `fp' gee_logit
quietly xtgee union age grade not_smsa south, family(binomial) link(logit) corr(exchangeable) vce(robust)
quietly xtgee
dumpt `fp' gee_logit_robust
collect clear
quietly collect: xtgee grade age not_smsa south, family(gaussian) corr(exchangeable)
quietly xtgee
dumpt `fp' gee_gauss
regtab, stats(n aic groups) csv("`out'/gee_gauss.csv")
dumpr `fs' gee_gauss
collect clear
quietly collect: xtgee union age grade not_smsa south, family(poisson) corr(exchangeable)
quietly xtgee
dumpt `fp' gee_pois
regtab, stats(n qic groups) csv("`out'/gee_pois.csv")
dumpr `fs' gee_pois

file close `fp'
file close `fs'
