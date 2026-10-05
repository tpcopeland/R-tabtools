/*  make_wttab_pctile.do - Stata's percentiles for wttab() (task 7.12)

    wttab() takes its percentiles and truncation cut-offs from Stata's
    _pctile definition (R/wttab.R .wt_pctile(), also R's quantile(type = 2)).
    This file records Stata's own results on deterministic data, so a
    change in the rule fails a test: small samples (n = 1..12), heavy ties,
    and n where n * p / 100 is a whole number in exact decimal arithmetic
    but not in binary floating point: n * 0.07 for n a multiple of 100 (R's
    quantile(type = 2) misses those), and the review 7w F1 sizes, where
    n * p / 100 itself lands an ulp off a whole number (41000 x 99.9,
    5250 x 0.4 / 99.6, 11000 x 0.7 / 99.3, 1000 x 16.1, 3000 x 1.1 ...),
    on w = 1..n and on distinct scrambled values.

    Run from the repo root:  stata-mp -b do qa/stata/make_wttab_pctile.do
    Writes tests/testthat/fixtures/wttab_pctile/stata_pctile.csv:
    case, n, a, m, dv (the data are w_i = mod(i * a, m) / dv, exact in both
    Stata and R), p (percent), pctile (_pctile r(r#)), and sumdet
    (summarize, detail r(p#) for the percentiles it reports, else "."), the
    numbers as %21x so both sides compare exact doubles. Results are held
    in scalars, never macros: `local x = r(p1)` rounds to 17 significant
    digits and can change the double (review 7w F5).

    summarize, detail's r(p#) equal _pctile's exactly (every sumdet cell
    here; an earlier note that they differ in the last bit came from
    storing them in macros).
*/
version 17.0
clear all
set more off
tempname fh
file open `fh' using "tests/testthat/fixtures/wttab_pctile/stata_pctile.csv", write text replace
file write `fh' "case,n,a,m,dv,p,pctile,sumdet" _n
local case 0
* (n a m dv) specs: small n; ties (small m); continuous-ish (large m).
local specs ""
forvalues n = 1/12 {
    local specs `"`specs' "`n' 7 5 2" "`n' 7919 10007 1000""'
}
foreach n in 13 25 50 99 100 101 200 300 333 700 1000 1500 7500 {
    local specs `"`specs' "`n' 7 4 2" "`n' 7919 10007 1000" "`n' 31 97 10""'
}
* Review 7w F1 sizes: w = 1..n, and distinct scrambled values (41011 prime).
foreach n in 1000 3000 5250 11000 21000 41000 {
    local specs `"`specs' "`n' 1 1000000000 1" "`n' 7919 41011 1000""'
}
foreach s of local specs {
    tokenize `s'
    local n `1'
    local a `2'
    local m `3'
    local dv `4'
    local ++case
    clear
    quietly set obs `n'
    quietly gen double w = mod(_n * `a', `m') / `dv'
    quietly summarize w, detail
    foreach q in 1 5 10 25 50 75 90 95 99 {
        tempname sd`q'
        scalar `sd`q'' = r(p`q')
    }
    local plist "0.1 0.4 0.5 0.7 1 1.1 2.3 2.5 4.1 5 7 10 12.5 16.1 25 33.3 50 66.7 75 90 93 95 97.5 99 99.1 99.3 99.4 99.5 99.6 99.9"
    quietly _pctile w, p(`plist')
    local k 0
    foreach p of local plist {
        local ++k
        local sdv "."
        if inlist(`p', 1, 5, 10, 25, 50, 75, 90, 95, 99) local sdv = strtrim(string(`sd`p'', "%21x"))
        file write `fh' "`case',`n',`a',`m',`dv',`p'," (strtrim(string(r(r`k'), "%21x"))) ",`sdv'" _n
    }
}
file close `fh'
