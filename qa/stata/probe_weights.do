* Reference values for the table1_tc weighting unit tests
* (tests/testthat/test-table1-weights.R). Run from any directory:
*   stata-mp -b do qa/stata/probe_weights.do [STATA_TOOLS_DIR]
* Prints every value at %21.17g; the test file hardcodes them.
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
capture program drop _desctab_collect
findfile _desctab_collect.ado
run "`r(fn)'"
set linesize 255

clear
input double(x w f)
2.5 0.7 1
3.1 1.9 3
4.8 0.2 2
1.2 2.4 1
7.7 1.1 4
3.1 0.6 2
end
* [aw=] mean and SD (the weighted SD table1_tc and its SMD use)
quietly summarize x [aw=w]
display "aw_mean " %21.17g r(mean)
display "aw_sd " %21.17g r(sd)
* [fw=] mean and SD
quietly summarize x [fw=f]
display "fw_mean " %21.17g r(mean)
display "fw_sd " %21.17g r(sd)
display "fw_N " %21.17g r(N)
* ESS = (sum w)^2 / sum w^2
quietly summarize w
local sw = r(sum)
generate double w2 = w^2
quietly summarize w2
display "ess " %21.17g (`sw')^2 / r(sum)

* _t1tcfc_wquantile: unit weights (average at an exact n*p), fractional
* weights hitting the target within tolerance, zero weights dropped, a
* weight total above 1 scaling the tolerance.
mata:
x = (2.5 \ 3.1 \ 4.8 \ 1.2 \ 7.7 \ 3.1)
w = (0.7 \ 1.9 \ 0.2 \ 2.4 \ 1.1 \ 0.6)
u = J(6, 1, 1)
printf("q_unit_50 %21.17g\n", _t1tcfc_wquantile(x, u, .5))
printf("q_unit_25 %21.17g\n", _t1tcfc_wquantile(x, u, .25))
printf("q_unit_75 %21.17g\n", _t1tcfc_wquantile(x, u, .75))
printf("q_w_50 %21.17g\n", _t1tcfc_wquantile(x, w, .5))
printf("q_w_25 %21.17g\n", _t1tcfc_wquantile(x, w, .25))
printf("q_w_75 %21.17g\n", _t1tcfc_wquantile(x, w, .75))
t = (1 \ 2 \ 3 \ 4)
tw = (0.1 \ 0.2 \ 0.3 \ 0.4)
printf("q_tie_30 %21.17g\n", _t1tcfc_wquantile(t, tw, .3))
printf("q_tie_60 %21.17g\n", _t1tcfc_wquantile(t, tw, .6))
printf("q_tie_10 %21.17g\n", _t1tcfc_wquantile(t, tw, .1))
printf("q_tie_100 %21.17g\n", _t1tcfc_wquantile(t, tw, 1))
z = (5 \ 1 \ 9 \ 3)
zw = (0 \ 2 \ 0 \ 2)
printf("q_zero_50 %21.17g\n", _t1tcfc_wquantile(z, zw, .5))
b = (10 \ 20 \ 30)
bw = (1e6 \ 1e6 \ 1e6)
printf("q_big_50 %21.17g\n", _t1tcfc_wquantile(b, bw, .5))
printf("q_big_third %21.17g\n", _t1tcfc_wquantile(b, bw, 1/3))
end
