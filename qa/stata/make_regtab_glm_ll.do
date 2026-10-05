/*  make_regtab_glm_ll.do - Stata's log-likelihood, AIC and BIC for glm's
    gamma and inverse Gaussian families (pre-release review P0-1).

    Stata's glm reports the log-likelihood of these families at scale 1
    (the dispersion enters the standard errors only), while R's logLik()
    plugs in deviance/n. regtab shows Stata's number; this fixture pins it
    for the unweighted, [aweight], [pweight] and vce(robust)/vce(cluster)
    paths, beside the gaussian family, whose likelihood uses the estimated
    scale on both sides. Read by tests/testthat/test-regtab-glm-ll.R.

    Output: tests/testthat/fixtures/regtab_glm_ll/
      glm_ll.csv       id, family, link, weight, vce, ll, rank, N, aic, bic
                       (e(ll), e(rank), e(N); AIC/BIC from estat ic)
      <id>.csv         regtab, stats(n ll aic bic) csv() for the ids in
                       the regtab loop, as Stata's regtab shows them
    Every number %21.17g.

    Data: sysuse auto (tests/testthat/golden/fixtures/auto.dta), `price'
    on `mpg' and `weight', weights `turn', ten clusters cl = mod(_n - 1, 10). Convergence
    tolerances 1e-14 so that the non-canonical fits agree with R's
    glm(control = glm.control(epsilon = 1e-14)) to ~1e-10.

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_glm_ll.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR holds tabtools/ (a `git archive` export of
    Stata-Tools 1255176d, tabtools 2.1.14; default ~/Stata-Tools).
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
which regtab
clear all
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
local fx "tests/testthat/golden/fixtures"
local out "tests/testthat/fixtures/regtab_glm_ll"
capture mkdir "`out'"

local tol "tolerance(1e-14) ltolerance(1e-14) nrtolerance(1e-14) iterate(500)"

tempname fh
file open `fh' using "`out'/glm_ll.csv", write text replace
file write `fh' "id,family,link,weight,vce,ll,rank,N,aic,bic" _n

* One case: id family link weight(none|aw|pw) vce(oim|robust|cluster)
capture program drop llprobe
program define llprobe
    args fh id fam lnk wt vce tol fx
    use "`fx'/auto.dta", clear
    generate cl = mod(_n - 1, 10)
    local w ""
    if "`wt'" != "none" local w "[`wt'=turn]"
    local v ""
    if "`vce'" == "robust" local v "vce(robust)"
    if "`vce'" == "cluster" local v "vce(cluster cl)"
    quietly glm price mpg weight `w', family(`fam') link(`lnk') `v' `tol'
    quietly estat ic
    tempname S
    matrix `S' = r(S)
    file write `fh' "`id',`fam',`lnk',`wt',`vce'," %21.17g (e(ll)) "," %21.17g (e(rank)) "," ///
        %21.17g (e(N)) "," %21.17g (`S'[1,5]) "," %21.17g (`S'[1,6]) _n
end

llprobe `fh' L01 gaussian identity none oim     "`tol'" "`fx'"
llprobe `fh' L02 gaussian log      none oim     "`tol'" "`fx'"
llprobe `fh' L03 gamma    log      none oim     "`tol'" "`fx'"
llprobe `fh' L04 gamma    "power -1" none oim     "`tol'" "`fx'"
llprobe `fh' L05 gamma    identity none oim     "`tol'" "`fx'"
llprobe `fh' L06 igaussian log     none oim     "`tol'" "`fx'"
llprobe `fh' L08 gamma    log      none robust  "`tol'" "`fx'"
llprobe `fh' L09 igaussian log     none robust  "`tol'" "`fx'"
llprobe `fh' L10 gamma    log      aw   oim     "`tol'" "`fx'"
llprobe `fh' L11 igaussian log     aw   oim     "`tol'" "`fx'"
llprobe `fh' L12 gaussian identity aw   oim     "`tol'" "`fx'"
llprobe `fh' L13 gamma    log      pw   oim     "`tol'" "`fx'"
llprobe `fh' L14 igaussian log     pw   oim     "`tol'" "`fx'"
llprobe `fh' L15 igaussian log     pw   cluster "`tol'" "`fx'"
llprobe `fh' L16 gamma    log      none cluster "`tol'" "`fx'"
file close `fh'

* What Stata's regtab displays for a few of them.
foreach c in "L03 gamma log none oim" "L06 igaussian log none oim" ///
             "L08 gamma log none robust" "L13 gamma log pw oim" ///
             "L14 igaussian log pw oim" {
    tokenize `c'
    use "`fx'/auto.dta", clear
    local w ""
    if "`4'" != "none" local w "[`4'=turn]"
    local v ""
    if "`5'" == "robust" local v "vce(robust)"
    collect clear
    quietly collect: glm price mpg weight `w', family(`2') link(`3') `v' `tol'
    regtab, stats(n ll aic bic) csv("`out'/`1'.csv")
}
