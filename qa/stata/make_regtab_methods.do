/*  make_regtab_methods.do - Stata's r(methods) sentence for milestone H
    task H11 (external review F12, take_action item 11).

    Stata 2.1.11 picked the methods sentence from the estimate header
    (`regtab.ado:2866-2915`) and hard-coded "multivariable", so probit read
    "linear regression", nbreg "Poisson regression", a one-predictor logit
    "multivariable", and a coef() relabel renamed the model. 2.1.12 builds
    it from the models (take_action item 11, fixed), as R does (decision
    H-D3); tests/testthat/test-regtab-hardening.R ("H11: ...") compares
    R's sentence with this fixture for every probe R can fit.

    Output: tests/testthat/fixtures/regtab_methods/stata_methods.csv, one
    row per case: id, the Stata commands, r(coef_label), r(methods) with
    Stata's closing software sentence removed.

    Data: Stata's own example datasets (sysuse auto, sysuse cancer); the
    sentence does not depend on the numbers.

    Run from the repo root with the baseline tabtools first on the adopath:
        stata-mp -b do qa/stata/make_regtab_methods.do STATA_TOOLS_DIR
    where STATA_TOOLS_DIR holds tabtools/ (a `git archive` export of
    Stata-Tools 1255176d, tabtools 2.1.14; default ~/Stata-Tools).
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
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
local out "tests/testthat/fixtures/regtab_methods"
capture mkdir "`out'"

tempname fh
file open `fh' using "`out'/stata_methods.csv", write text replace
file write `fh' "id,commands,coef_label,methods" _n

* One case: collect the models in `cmds' (separated by " ; "), run
* regtab with `opts', and write the sentence without the software part.
capture program drop mprobe
program define mprobe
    args fh id cmds opts
    collect clear
    local rest `"`cmds'"'
    while `"`rest'"' != "" {
        gettoken one rest : rest, parse(";")
        if `"`one'"' == ";" continue
        quietly collect: `one'
    }
    quietly regtab, `opts'
    local m `"`r(methods)'"'
    local m : subinstr local m " Analysis performed in Stata `c(stata_version)' (StataCorp, College Station, TX)." ""
    local c `"`r(coef_label)'"'
    local q = char(34)
    file write `fh' `"`id',`q'`cmds'`q',`q'`c'`q',`q'`m'`q'"' _n
    display as text "`id': `m'"
end

sysuse auto, clear
generate cnt = round(price / 1000)
generate long_trip = mpg > 20
* The verification probes (external review F12).
mprobe `fh' M01 "probit foreign mpg weight" ""
mprobe `fh' M02 "glm foreign mpg weight, family(binomial) link(probit)" ""
mprobe `fh' M03 "nbreg cnt mpg weight" ""
mprobe `fh' M04 "poisson cnt mpg weight" ""
mprobe `fh' M05 "logit foreign mpg" ""
mprobe `fh' M06 "regress price mpg weight" "coef(OR)"
* The rest of H11's list.
mprobe `fh' M07 "glm foreign mpg weight, family(binomial) link(cloglog)" ""
mprobe `fh' M08 "cloglog foreign mpg weight" ""
mprobe `fh' M09 "glm price mpg weight, family(gamma) link(log)" ""
mprobe `fh' M10 "logit foreign mpg weight" "coef(Estimate)"
mprobe `fh' M11 "logit foreign mpg weight ; regress price mpg weight" ""
mprobe `fh' M12 "logit foreign mpg ; logit foreign mpg weight" ""
mprobe `fh' M13 "regress price mpg weight" ""
mprobe `fh' M14 "ologit rep78 mpg weight" ""
mprobe `fh' M15 "mlogit rep78 mpg weight" ""
mprobe `fh' M16 "zip cnt mpg, inflate(weight)" ""
mprobe `fh' M17 "mixed price mpg weight || foreign:" ""
mprobe `fh' M18 "melogit long_trip weight || foreign:" ""
mprobe `fh' M19 "logit foreign mpg weight" "cdisc"
mprobe `fh' M20 "logit foreign mpg weight, level(90)" "level(90) stars"

sysuse cancer, clear
generate id = _n
stset studytime, failure(died)
mprobe `fh' M21 "stcox age i.drug" ""
mprobe `fh' M22 "streg age i.drug, distribution(weibull) time" ""
mprobe `fh' M23 "streg age i.drug, distribution(weibull)" ""
generate cause = died
replace cause = 2 if died == 0 & mod(_n, 3) == 0
stset studytime, failure(cause == 1)
mprobe `fh' M24 "stcrreg age i.drug, compete(cause == 2)" ""
sysuse auto, clear
generate cnt = round(price / 1000)
xtset rep78
mprobe `fh' M25 "xtgee foreign mpg weight, family(binomial) link(logit)" ""

file close `fh'
display as result "wrote `out'/stata_methods.csv"
