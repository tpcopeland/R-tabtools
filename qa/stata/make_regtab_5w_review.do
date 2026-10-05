/*  make_regtab_5w_review.do - Stata expectations for the milestone 5w
    review fixes (IMPLEMENTATION_PLAN.md, "Milestone 5w review outcome"),
    from the reviewer's display probes (ids D.., E.. kept) and new ones
    (N01-N07). For each case: regtab's csv() sink <id>.csv and every r()
    value <id>_stored.csv (golden_dump_r(), the format of the golden
    *_stored.csv files).

    Inputs: syn.dta and synpp.dta in the output folder
    (qa/make_regtab_5w_review_data.R).

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_5w_review.do [STATA_TOOLS_DIR]
    (default ~/Stata-Tools; pass a `git archive` export of Stata-Tools
    f42ee9cd, tabtools 2.1.12, when that working tree has uncommitted edits).
    tests/testthat/test-regtab-5w-review.R reads them.
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/fvgen"
adopath ++ "`stata_tools'/tabtools"
which regtab
which fvgen
set more off
set varabbrev off
set linesize 255
run "qa/stata/golden_helpers.do"
golden_ado_version desctab
if "`r(version)'" != "2.1.14" {
    display as error "tabtools is `r(version)'; these fixtures need the 2.1.14 baseline"
    exit 459
}
global GOLDEN_TABTOOLS_VERSION "`r(version)'"
golden_ado_version fvgen
global GOLDEN_FVGEN_VERSION "`r(version)'"
local out "tests/testthat/fixtures/regtab_5w_review"

* Standard errors of the last estimation, one row per coefficient (the
* regtab r(table) holds the estimates only): <id>_se.csv.
capture program drop w5r_se
program define w5r_se
    args file
    tempname b V fh
    matrix `b' = e(b)
    matrix `V' = e(V)
    local names : colnames `b'
    file open `fh' using "`file'", write text replace
    file write `fh' "term,b,se,N_clust" _n
    local k = colsof(`b')
    forvalues j = 1/`k' {
        local nm : word `j' of `names'
        file write `fh' "`nm'," %23.16e (`b'[1,`j']) "," %23.16e (sqrt(`V'[`j',`j'])) "," %23.16e (e(N_clust)) _n
    }
    file close `fh'
end

capture program drop w5r_begin
program define w5r_begin
    args data
    clear
    capture collect clear
    estimates clear
    capture macro drop TABTOOLS_*
    capture stset, clear
    capture svyset, clear
    use "`data'", clear
end

* ======================================================================
* P0-4: pseudo-log-likelihood statistics under [pweight] (and the R-squared row label)
* ======================================================================
* --- D01
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2 [pw=iptw]
regtab, stats(n ll aic bic r2) csv("`out'/D01.csv")
mata: golden_dump_r("`out'/D01_stored.csv", "D01")

* --- D02
w5r_begin "`out'/syn.dta"
quietly collect: regress yc treat x1 x2 [pw=iptw]
regtab, stats(n ll aic bic r2) csv("`out'/D02.csv")
mata: golden_dump_r("`out'/D02_stored.csv", "D02")

* --- D03
w5r_begin "`out'/syn.dta"
quietly collect: poisson cnt treat x1 [pw=iptw], exposure(expo)
regtab, stats(n ll aic bic r2) csv("`out'/D03.csv")
mata: golden_dump_r("`out'/D03_stored.csv", "D03")

* --- D05
w5r_begin "`out'/syn.dta"
stset time [pw=iptw], failure(event) id(id)
quietly collect: stcox treat x1 x2, breslow
regtab, stats(n ll aic bic) csv("`out'/D05.csv")
mata: golden_dump_r("`out'/D05_stored.csv", "D05")
w5r_se "`out'/D05_se.csv"

* --- D05b
w5r_begin "`out'/syn.dta"
stset time [pw=iptw], failure(event)
quietly collect: stcox treat x1 x2, breslow
regtab, stats(n ll) csv("`out'/D05b.csv")
mata: golden_dump_r("`out'/D05b_stored.csv", "D05b")

* --- D09
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2 [pw=wc]
regtab, stats(n ll aic bic r2) csv("`out'/D09.csv")
mata: golden_dump_r("`out'/D09_stored.csv", "D09")

* --- D10
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2 [pw=iw]
regtab, stats(n ll aic bic r2) csv("`out'/D10.csv")
mata: golden_dump_r("`out'/D10_stored.csv", "D10")

* --- D20
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2
quietly collect: logit y treat x1 x2 [pw=iptw]
quietly collect: logit y treat x1 x2 [pw=iptw], vce(cluster clusu)
regtab, models("A" \ "B" \ "C") stats(n ll aic bic r2) csv("`out'/D20.csv")
mata: golden_dump_r("`out'/D20_stored.csv", "D20")

* --- D21
w5r_begin "`out'/syn.dta"
quietly collect: probit y treat x1 x2 [pw=iptw]
regtab, stats(n ll aic bic r2) csv("`out'/D21.csv")
mata: golden_dump_r("`out'/D21_stored.csv", "D21")

* --- E08
w5r_begin "`out'/syn.dta"
quietly collect: poisson cnt treat x1, exposure(expo) vce(robust)
regtab, stats(n ll aic bic r2) csv("`out'/E08.csv")
mata: golden_dump_r("`out'/E08_stored.csv", "E08")

* --- E09
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2 [pw=iptw]
quietly collect: regress yc treat x1 x2 [pw=iptw]
regtab, stats(n ll aic bic r2) csv("`out'/E09.csv")
mata: golden_dump_r("`out'/E09_stored.csv", "E09")

* --- N05
w5r_begin "`out'/syn.dta"
quietly collect: glm yc treat x1 [pw=iptw], family(gaussian)
regtab, stats(n ll aic bic r2) csv("`out'/N05.csv")
mata: golden_dump_r("`out'/N05_stored.csv", "N05")

* --- N06
w5r_begin "`out'/syn.dta"
quietly collect: glm ypos treat x1 [pw=iptw], family(gamma) link(log)
regtab, stats(n ll aic bic) csv("`out'/N06.csv")
mata: golden_dump_r("`out'/N06_stored.csv", "N06")

* --- D14
w5r_begin "`out'/synpp.dta"
quietly collect: glm ev a z i.period [pw=sw], family(binomial) link(logit) vce(cluster id)
regtab, drop(i.period) stats(n ll aic bic) csv("`out'/D14.csv")
mata: golden_dump_r("`out'/D14_stored.csv", "D14")

* ======================================================================
* P0-5: svy: R-squared and the subpopulation (Subjects). Plain svy: logit
* since 2.1.11, which classifies svy: fits (OR, intercept dropped).
* ======================================================================
* --- D06
w5r_begin "`out'/syn.dta"
svyset psu [pw=iptw], strata(strata) singleunit(certainty)
quietly collect: svy: regress yc treat x1 x2
regtab, stats(n ll aic bic r2) csv("`out'/D06.csv")
mata: golden_dump_r("`out'/D06_stored.csv", "D06")

* --- D07
w5r_begin "`out'/syn.dta"
svyset psu [pw=iptw], strata(strata) singleunit(certainty)
quietly collect: svy: logit y treat x1 x2
regtab, stats(n ll aic bic r2) csv("`out'/D07.csv")
mata: golden_dump_r("`out'/D07_stored.csv", "D07")

* --- D11
w5r_begin "`out'/syn.dta"
svyset psu [pw=iptw], strata(strata) singleunit(certainty)
quietly collect: svy, subpop(x2): logit y treat x1
regtab, stats(n) csv("`out'/D11.csv")
mata: golden_dump_r("`out'/D11_stored.csv", "D11")

* ======================================================================
* P0-6: few clusters (AIC/BIC on e(rank) of the robust e(V): not replicated; every other cell compared)
* ======================================================================
* --- D08
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2, vce(cluster clus3)
regtab, stats(n ll aic bic r2) csv("`out'/D08.csv")
mata: golden_dump_r("`out'/D08_stored.csv", "D08")

* --- E01
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2, vce(cluster clus2)
regtab, stats(n ll aic bic r2) csv("`out'/E01.csv")
mata: golden_dump_r("`out'/E01_stored.csv", "E01")

* --- E02
w5r_begin "`out'/syn.dta"
quietly collect: regress yc treat x1 x2, vce(cluster clus3)
regtab, stats(n ll aic bic r2) csv("`out'/E02.csv")
mata: golden_dump_r("`out'/E02_stored.csv", "E02")

* --- E03
w5r_begin "`out'/syn.dta"
stset time, failure(event) id(id)
quietly collect: stcox treat x1 x2, breslow vce(cluster clus3)
regtab, stats(n ll aic bic) csv("`out'/E03.csv")
mata: golden_dump_r("`out'/E03_stored.csv", "E03")

* --- E07
w5r_begin "`out'/syn.dta"
quietly collect: probit y treat x1 x2, vce(cluster clus3)
regtab, stats(n ll aic bic) csv("`out'/E07.csv")
mata: golden_dump_r("`out'/E07_stored.csv", "E07")

* ======================================================================
* P0-2: weighted fits of other classes (Stata forces robust standard errors)
* ======================================================================
* --- D13
w5r_begin "`out'/synpp.dta"
xtset id period
quietly collect: xtgee ev a z [pw=wid], family(binomial) link(logit) corr(independent)
regtab, stats(n groups) csv("`out'/D13.csv")
mata: golden_dump_r("`out'/D13_stored.csv", "D13")
w5r_se "`out'/D13_se.csv"

* --- E06
w5r_begin "`out'/synpp.dta"
xtset id period
quietly collect: xtgee ev a z [pw=wid], family(binomial) link(logit) corr(independent) vce(robust)
regtab, stats(n groups) csv("`out'/E06.csv")
mata: golden_dump_r("`out'/E06_stored.csv", "E06")

* --- D16
w5r_begin "`out'/syn.dta"
quietly collect: nbreg cnt treat x1 [pw=iptw], exposure(expo)
regtab, stats(n ll aic bic r2) csv("`out'/D16.csv")
mata: golden_dump_r("`out'/D16_stored.csv", "D16")
w5r_se "`out'/D16_se.csv"

* --- D16k
w5r_begin "`out'/syn.dta"
quietly collect: nbreg cnt treat x1 [pw=iptw], exposure(expo)
regtab, keepintercept csv("`out'/D16k.csv")
mata: golden_dump_r("`out'/D16k_stored.csv", "D16k")

* --- N07
w5r_begin "`out'/syn.dta"
stset time [pw=iw], failure(event) id(id)
quietly collect: stcox treat x1 x2, breslow
regtab, stats(n ll aic bic) csv("`out'/N07.csv")
mata: golden_dump_r("`out'/N07_stored.csv", "N07")
w5r_se "`out'/N07_se.csv"

* ======================================================================
* P1-2: zero weights, empty clusters, several records per subject
* ======================================================================
* --- D12
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 x2 [pw=wz], vce(cluster clusz)
regtab, stats(n) csv("`out'/D12.csv")
mata: golden_dump_r("`out'/D12_stored.csv", "D12")

* --- N03
w5r_begin "`out'/syn.dta"
quietly collect: regress yc treat x1 x2 [pw=wz], vce(cluster clusz)
regtab, stats(n) csv("`out'/N03.csv")
mata: golden_dump_r("`out'/N03_stored.csv", "N03")

* --- N04
w5r_begin "`out'/synpp.dta"
quietly collect: glm ev a z i.period [pw=wz0], family(binomial) link(logit) vce(cluster id)
regtab, drop(i.period) stats(n) csv("`out'/N04.csv")
mata: golden_dump_r("`out'/N04_stored.csv", "N04")

* --- N01
w5r_begin "`out'/synpp.dta"
stset t1 [pw=wid], id(id) time0(t0) failure(ev)
quietly collect: stcox a z, breslow
regtab, stats(n ll aic bic) csv("`out'/N01.csv")
mata: golden_dump_r("`out'/N01_stored.csv", "N01")
w5r_se "`out'/N01_se.csv"

* --- N02
w5r_begin "`out'/synpp.dta"
stset t1, id(id) time0(t0) failure(ev)
quietly collect: stcox a z, breslow vce(robust)
regtab, stats(n ll) csv("`out'/N02.csv")
mata: golden_dump_r("`out'/N02_stored.csv", "N02")
w5r_se "`out'/N02_se.csv"

* --- D19
w5r_begin "`out'/syn.dta"
quietly collect: logit y treat x1 i.f4 [pw=iptw] if x2 == 1
regtab, stats(n) csv("`out'/D19.csv")
mata: golden_dump_r("`out'/D19_stored.csv", "D19")

* --- D19b
w5r_begin "`out'/syn.dta"
quietly collect: regress yc treat x1 i.f4 [pw=iptw] if x2 == 1
regtab, stats(n) csv("`out'/D19b.csv")
mata: golden_dump_r("`out'/D19b_stored.csv", "D19b")

* ======================================================================
* P1-3: Cox ties (stcox's default is Breslow)
* ======================================================================
* --- E05
w5r_begin "`out'/syn.dta"
stset time, failure(event) id(id)
quietly collect: stcox treat x1 x2
regtab, stats(n) csv("`out'/E05.csv")
mata: golden_dump_r("`out'/E05_stored.csv", "E05")

capture stset, clear
capture svyset, clear
display "make_regtab_5w_review.do done"
