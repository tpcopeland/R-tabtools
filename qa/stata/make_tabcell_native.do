/* Authentic leaf r() snapshots: never exported through a fabricated table.
   Root runs this reviewed recipe against pin 712044f8 in an owned output
   directory and adds SOURCE.csv / ARTIFACTS.csv authentication afterwards. */
version 17.0
args stata_ado out
if `"`stata_ado'"' == "" | `"`out'"' == "" exit 198
adopath ++ "`stata_ado'"
discard
quietly tabtools
if "`r(version)'" != "2.5.1" exit 9
foreach component in tabcell _tabcell_render puttab {
    quietly findfile `component'.ado
    if strpos("`r(fn)'", "`stata_ado'/") != 1 exit 9
}
set more off
set linesize 255
set rng mt64
set level 95
do qa/stata/golden_helpers.do
global GOLDEN_TABTOOLS_VERSION "2.5.1"
global GOLDEN_FVGEN_VERSION ""
clear
// Every dump is the next command after tabcell: Mata does not modify r().
tabcell est, b(-2) ll(-3) ul(-1) sep(" to ")
mata: golden_dump_r("`out'/TC001_est_stored.csv", "TC001_est")
tabcell est, b(.2) se(.1) level(90) eform scale(10)
mata: golden_dump_r("`out'/TC001_se_stored.csv", "TC001_se")
tabcell p, p(.0004) pstyle(Pfootnote)
mata: golden_dump_r("`out'/TC001_p_stored.csv", "TC001_p")
tabcell n, n(12345)
mata: golden_dump_r("`out'/TC001_n_stored.csv", "TC001_n")
tabcell np, n(12) d(74)
mata: golden_dump_r("`out'/TC001_np_stored.csv", "TC001_np")
tabcell enp, e(12) n(74)
mata: golden_dump_r("`out'/TC001_enp_stored.csv", "TC001_enp")
tabcell iqr, median(20) q1(18) q3(25) digits(0)
mata: golden_dump_r("`out'/TC001_iqr_stored.csv", "TC001_iqr")
tabcell rate, e(12) pt(975.6) per(1000) ci(poisson) level(90) digits(2)
mata: golden_dump_r("`out'/TC001_rate_stored.csv", "TC001_rate")
tabcell np, n(0) d(20) ci(exact)
mata: golden_dump_r("`out'/TC002_np0_stored.csv", "TC002_np0")
tabcell np, n(20) d(20) ci(exact)
mata: golden_dump_r("`out'/TC002_npfull_stored.csv", "TC002_npfull")
tabcell np, n(3) d(20) ci(exact) mincell(5)
mata: golden_dump_r("`out'/TC002_npmask_stored.csv", "TC002_npmask")
tabcell np, n(3) d(20) ci(exact) mincell(5) nocount
mata: golden_dump_r("`out'/TC002_npdash_stored.csv", "TC002_npdash")
tabcell np, n(0) d(0) ci(exact)
mata: golden_dump_r("`out'/TC002_npzero_stored.csv", "TC002_npzero")
tabcell np, n(0) d(0) ci(exact) nocount missing("")
mata: golden_dump_r("`out'/TC002_npempty_stored.csv", "TC002_npempty")
tabcell rate, e(0) pt(100) per(1000)
mata: golden_dump_r("`out'/TC002_rate0_stored.csv", "TC002_rate0")
tabcell rate, e(0) pt(100) per(1000) ci(poisson)
mata: golden_dump_r("`out'/TC002_rate0wald_stored.csv", "TC002_rate0wald")
tabcell rate, e(3) pt(100) per(1000) mincell(5)
mata: golden_dump_r("`out'/TC002_ratemask_stored.csv", "TC002_ratemask")
tabcell rate, e(3) pt(100) per(1000) ci(poisson) mincell(5)
mata: golden_dump_r("`out'/TC002_ratemaskwald_stored.csv", "TC002_ratemaskwald")
tabcell rate, e(0) pt(0) per(1000) missing("")
mata: golden_dump_r("`out'/TC002_rateempty_stored.csv", "TC002_rateempty")
tabcell enp, e(3) n(40) mincell(5)
mata: golden_dump_r("`out'/TC002_enpevent_stored.csv", "TC002_enpevent")
tabcell enp, e(1) n(3) mincell(5)
mata: golden_dump_r("`out'/TC002_enptotal_stored.csv", "TC002_enptotal")
tabcell est, b(2) ll(.) ul(.) missing("")
mata: golden_dump_r("`out'/TC003_empty_stored.csv", "TC003_empty")
tabcell est, b(2) se(0) missing("no estimate")
mata: golden_dump_r("`out'/TC003_zeroSE_stored.csv", "TC003_zeroSE")
// Literal strings constructed as data, never substituted through a command.
global TC_SENTINEL "WRONG_EXPANSION"
mata: st_local("literal", "missing " + char(36) + "TC_SENTINEL " + char(96) + "absent" + char(39) + " " + char(34) + "quote" + char(34))
mata: st_local("literal_sep", " | " + char(36) + "TC_SENTINEL " + char(96) + "absent" + char(39) + " " + char(34) + "quote" + char(34) + " | ")
tabcell est, b(.) ll(.) ul(.) missing(`"`macval(literal)'"')
mata: golden_dump_r("`out'/TC003_literal_stored.csv", "TC003_literal")
tabcell est, b(2) ll(1) ul(3) sep(`"`macval(literal_sep)'"') cformat(%9.3f)
mata: golden_dump_r("`out'/TC003_sep_stored.csv", "TC003_sep")
file open errors using "`out'/TC003_errors.csv", write text replace
file write errors "id,rc" _n
capture tabcell est, b(.) ll(.) ul(.)
local rc = _rc
assert `rc' == 459
file write errors "missing,`rc'" _n
capture tabcell est, b(2) ll(3) ul(1)
local rc = _rc
assert `rc' == 198
file write errors "reversed,`rc'" _n
capture tabcell p, p(2)
local rc = _rc
assert `rc' == 198
file write errors "pdomain,`rc'" _n
capture tabcell np, n(.5) d(20) ci(exact)
local rc = _rc
assert `rc' == 459
file write errors "npfraction,`rc'" _n
capture tabcell rate, e(.5) pt(100) per(1000)
local rc = _rc
assert `rc' == 459
file write errors "ratefraction,`rc'" _n
capture tabcell rate, e(1) pt(0) per(1000)
local rc = _rc
assert `rc' == 198
file write errors "exposure,`rc'" _n
capture tabcell est, b(2) ll(1) ul(3) format(%9.2f) cformat(%9.2f)
local rc = _rc
assert `rc' == 198
file write errors "format,`rc'" _n
capture tabcell est, b(2) ll(1) ul(3) level(90)
local rc = _rc
assert `rc' == 198
file write errors "level,`rc'" _n
// Literal original fitting data, shared by t/normal and contrast controls.
clear
set obs 24
gen double x = mod(_n, 6) - 2
gen double z = floor((_n-1)/6)
gen double y = 2 + .3*x + .5*z + .2*(mod(_n, 2)*2-1)
gen byte event = mod(_n, 4) == 0 | mod(_n, 7) == 0
export delimited x z y event using "`out'/TC004_input.csv", replace
quietly regress y x z
tabcell est x
mata: golden_dump_r("`out'/TC004_t_stored.csv", "TC004_t")
quietly lincom x + 2*z
mata: golden_dump_r("`out'/TC004_lincom_input.csv", "TC004_lincom_input")
tabcell est, lincom level(90) eform scale(10)
mata: golden_dump_r("`out'/TC004_lincom_stored.csv", "TC004_lincom")
// This immediately preceding tabcell owns r(form); it is NOT a new lincom.
capture tabcell est, lincom
local rc = _rc
assert `rc' == 301
file write errors "stale_lincom,`rc'" _n
quietly regress y x z
quietly nlcom (sum: _b[x] + 2*_b[z])
mata: golden_dump_r("`out'/TC004_nlcom_input.csv", "TC004_nlcom_input")
tabcell est sum, nlcom
mata: golden_dump_r("`out'/TC004_nlcom_stored.csv", "TC004_nlcom")
quietly logit event x z
tabcell est x, eform
mata: golden_dump_r("`out'/TC004_normal_stored.csv", "TC004_normal")
matrix M = (2,1,3 \ 4,2,6)
matrix rownames M = first second
matrix colnames M = b ll ul
tabcell est, matrix(M second)
mata: golden_dump_r("`out'/TC004_matrix_stored.csv", "TC004_matrix")
tabcell est, matrix(M 1) cols(1 2 3)
mata: golden_dump_r("`out'/TC004_cols_stored.csv", "TC004_cols")
file close errors
// Generated returns are a separate contract, not invented scalar per-row r().
clear
set obs 6
gen long rowid = _n
gen double a = cond(_n==1,0,cond(_n==2,3,cond(_n==3,20,cond(_n==4,.,cond(_n==5,0,5)))))
gen double total = cond(_n==5,0,20)
gen double exposure = cond(_n==5,0,100)
gen double pv = cond(_n==1,0,cond(_n==2,.0004,cond(_n==3,.1,cond(_n==4,.,cond(_n==5,.999,1)))))
gen double lo = a - 1
gen double hi = a + 1
export delimited rowid a total exposure pv lo hi using "`out'/TC005_input.csv", replace
tabcell est if rowid != 1 in 2/6, b(a) ll(lo) ul(hi) missing("") generate(v_est)
mata: golden_dump_r("`out'/TC005_est_stored.csv", "TC005_est")
tabcell p if rowid != 1 in 2/6, p(pv) pstyle(footnote) missing("") generate(v_p)
mata: golden_dump_r("`out'/TC005_p_stored.csv", "TC005_p")
tabcell n if rowid != 1 in 2/6, n(a) mincell(5) missing("") generate(v_n)
mata: golden_dump_r("`out'/TC005_n_stored.csv", "TC005_n")
tabcell np if rowid != 1 in 2/6, n(a) d(total) ci(exact) mincell(5) missing("") generate(v_np)
mata: golden_dump_r("`out'/TC005_np_stored.csv", "TC005_np")
tabcell enp if rowid != 1 in 2/6, e(a) n(total) mincell(5) missing("") generate(v_enp)
mata: golden_dump_r("`out'/TC005_enp_stored.csv", "TC005_enp")
tabcell iqr if rowid != 1 in 2/6, median(a) q1(lo) q3(hi) missing("") generate(v_iqr)
mata: golden_dump_r("`out'/TC005_iqr_stored.csv", "TC005_iqr")
tabcell rate if rowid != 1 in 2/6, e(a) pt(exposure) per(1000) mincell(5) missing("") generate(v_rate)
mata: golden_dump_r("`out'/TC005_rate_stored.csv", "TC005_rate")
export delimited rowid v_est v_p v_n v_np v_enp v_iqr v_rate using "`out'/TC005_vectors.csv", replace
// Parent cells include actual missing/literal text as well as protected rows.
tabcell np, n(a) d(total) ci(exact) mincell(5) missing(`"`macval(literal)'"') generate(npcell)
mata: golden_dump_r("`out'/TC006_np_generated.csv", "TC006_np_generated")
tabcell rate, e(a) pt(exposure) per(1000) mincell(5) missing(`"`macval(literal)'"') generate(ratecell)
mata: golden_dump_r("`out'/TC006_rate_generated.csv", "TC006_rate_generated")
gen str8 term = "row" + string(rowid)
label variable term "Row"
label variable npcell "Percent"
label variable ratecell "Rate"
puttab term npcell ratecell using "`out'/TC006.xlsx", sheet("Leaf") ///
    title("Leaf publication") footnote("Protected cells stay protected.") ///
    varlabels headershade zebra borderstyle(academic) boldrows(2) hlines(3) ///
    csv("`out'/TC006.csv") markdown("`out'/TC006.md")
mata: golden_dump_r("`out'/TC006_parent_stored.csv", "TC006_parent")
file open runtime using "`out'/runtime.txt", write text replace
file write runtime "`c(stata_version)'" _n "`c(rng_current)'" _n
file close runtime
display as result "TABCELL_NATIVE_COMPLETE TC001 TC002 TC003 TC004 TC005 TC006"
