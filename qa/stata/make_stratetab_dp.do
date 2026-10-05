/*  make_stratetab_dp.do - strate files written under `set dp comma`
    (Phase 7b review F1).

    strate labels its interval variables with strsubdp() (strate.ado:444-447),
    so under `set dp comma` a 97.5% interval reads "Lower 97,5% confidence
    limit". stratetab 2.1.11-2.1.13 takes "5%" from that label; R reads
    97.5%, and so does stratetab since 2.1.14. The two files are identical,
    so the IRR is 1.

    Run from the repo root:  stata-mp -b do qa/stata/make_stratetab_dp.do
    Writes tests/testthat/fixtures/stratetab_dp/dp_1.dta and dp_2.dta.
    In Stata 2.1.13, `stratetab, using(dp_1 dp_2) outcomes(1) rateratio`
    then shows "Per 1,000 PY (5% CI)", rates 51.7 (32.7, 81.7) and 69.0
    (46.7, 101.9), and IRR 1.00 (0.98, 1.02) for both categories (the
    review's probe); 2.1.14 shows "(97.5% CI)" and IRR 1.00 (0.52, 1.91)
    and 1.00 (0.58, 1.74), as R (probed on Stata-Tools 1255176d, task C8).
*/
version 17.0
clear all
set obs 200
set seed 3
gen t = runiform()*10
gen d = runiform() < .3
gen g = mod(_n, 2)
stset t, failure(d)
set dp comma
strate g, output("tests/testthat/fixtures/stratetab_dp/dp_1", replace) level(97.5) nolist
strate g, output("tests/testthat/fixtures/stratetab_dp/dp_2", replace) level(97.5) nolist
set dp period
