/*  probe_nlme_lme.do - Stata `mixed` standard errors, variance-parameter
    variance and log-likelihood on the mixed_bp fixture, for task 5.19
    (nlme::lme as `mixed`; plan "Interop research follow-ups to Phase 5").

    Stata built-ins only (no tabtools); run from the repo root:
      stata-mp -b do qa/stata/probe_nlme_lme.do
    Results (Stata 17, 2026-09-27), read by test-regtab-nlme.R:
    - `mixed y age female bmi || region:` (ML): Std. err. age .0032326,
      female .079991, bmi .0078148, _cons .2855678; e(ll) -839.117182226987;
      e(V) lns1_1_1 .626703924210226, lnsig_e .000841753223326233, their
      covariance -.00146225216613897; e(b) lns1_1_1 -2.60116714857014,
      lnsig_e -.0226840750463907. nlme::lme(method = "ML")'s varFix gives
      the same standard errors (age 0.003232566); summary.lme() shows
      0.003243396, i.e. times sqrt(N / (N - p)) = sqrt(600 / 596)
      (adjustSigma = TRUE), with t(591) p-values. Stata does not rescale.
    - `, reml`: age Std. err. .0032412 (varFix of the REML lme fit, which
      summary.lme() leaves alone); e(ll) (restricted) -851.500608686442,
      lme's REML logLik() to 1e-12.
    - `|| region: age, cov(unstructured)`: e(ll) -838.451345302973; lme
      reaches it with lmeControl(niterEM = 200) (default: -838.45196).
    - `|| region: age` (cov(independent)): var(age) goes to the boundary
      (3.16e-13, CI 0 to .), so goldens use nhanes2 for the independent
      slope (R81).
*/

version 17.0
clear all
set more off
set linesize 200
use "tests/testthat/golden/fixtures/mixed_bp.dta", clear
mixed y age female bmi || region:
matrix list e(V), format(%20.15g)
matrix list e(b), format(%20.15g)
display %20.15g e(ll)
mixed y age female bmi || region:, reml
matrix list e(V), format(%20.15g)
display %20.15g e(ll)
mixed y age female bmi || region: age, cov(unstructured)
display %20.15g e(ll)
mixed y age female bmi || region: age
display %20.15g e(ll)
matrix list e(b), format(%20.15g)
matrix list e(V), format(%20.15g)
