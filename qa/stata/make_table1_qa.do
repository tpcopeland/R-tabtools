/*  make_table1_qa.do - Stata expectations for the ported table1_tc QA
    contracts (plan task 2.12): qa/test_table1_tc.do legacy/aggregation
    suites and qa/validation_table1_tc.do KE-1.

    Run from the repo root with the dev tabtools first on the adopath:
        stata-mp -b do qa/stata/make_table1_qa.do [STATA_TOOLS_DIR]
    Writes tests/testthat/fixtures/table1_qa/{agg,hp,two,ke1}.dta and
    A.csv ... K.csv (the csv() sink of each call).
*/
version 17.0
args stata_tools
if `"`stata_tools'"' == "" local stata_tools "~/Stata-Tools"
adopath ++ "`stata_tools'/tabtools"
set linesize 255
local out "tests/testthat/fixtures/table1_qa"

capture program drop _t1agg_build_data
program define _t1agg_build_data
    version 17.0
    clear
    set obs 12

    gen byte trt = cond(_n <= 6, 0, 1)
    gen double age = .
    replace age = 48 in 1
    replace age = 50 in 2
    replace age = 53 in 3
    replace age = 55 in 4
    replace age = 58 in 5
    replace age = .  in 6
    replace age = 60 in 7
    replace age = 62 in 8
    replace age = 64 in 9
    replace age = 67 in 10
    replace age = 70 in 11
    replace age = 73 in 12

    gen double marker = .
    replace marker = 4 in 1
    replace marker = 5 in 2
    replace marker = 6 in 3
    replace marker = 8 in 4
    replace marker = 9 in 5
    replace marker = . in 6
    replace marker = 7 in 7
    replace marker = 8 in 8
    replace marker = 10 in 9
    replace marker = 12 in 10
    replace marker = 13 in 11
    replace marker = 15 in 12

    gen byte female = .
    replace female = 0 in 1
    replace female = 1 in 2
    replace female = 1 in 3
    replace female = 0 in 4
    replace female = 1 in 5
    replace female = . in 6
    replace female = 1 in 7
    replace female = 0 in 8
    replace female = 1 in 9
    replace female = 1 in 10
    replace female = 0 in 11
    replace female = 1 in 12

    gen byte stage = .
    replace stage = 1 in 1
    replace stage = 1 in 2
    replace stage = 2 in 3
    replace stage = 2 in 4
    replace stage = 3 in 5
    replace stage = . in 6
    replace stage = 1 in 7
    replace stage = 2 in 8
    replace stage = 2 in 9
    replace stage = 3 in 10
    replace stage = 3 in 11
    replace stage = . in 12

    gen double w = 0.75 + mod(_n, 4) / 2
    gen int fw = cond(mod(_n, 5) == 0, 3, cond(mod(_n, 3) == 0, 2, 1))

    label define trtlbl 0 "Control" 1 "Active", replace
    label values trt trtlbl
    label define yesno 0 "No" 1 "Yes", replace
    label values female yesno
    label define stagelbl 1 "Stage I" 2 "Stage II" 3 "Stage III", replace
    label values stage stagelbl

    label variable age "Age at entry"
    label variable marker "Inflammation marker"
    label variable female "Female sex"
    label variable stage "Clinical stage"
    label variable trt "Treatment group"
    label variable w "Analysis weight"
    label variable fw "Frequency weight"
end

_t1agg_build_data
save "`out'/agg.dta", replace
table1_tc, by(trt) vars(age contn %6.2f \ marker conts %6.1f \ female bin \ stage cat) smd test statistic missing total(after) nformat(%9.0f) percformat(%5.1f) percsign("%") csv("`out'/A.csv")
table1_tc, by(trt) vars(stage cat) total(before) headerperc percent nformat(%9.0f) percformat(%5.1f) percsign("%") csv("`out'/B.csv")
table1_tc, by(trt) vars(stage cat) catrowperc percent_n slashN total(after) missing nformat(%9.0f) percformat(%5.1f) percsign("%") csv("`out'/C.csv")

clear
input g y z
0 1 0
0 2 1
0 3 1
1 4 .
1 5 .
1 6 .
end
save "`out'/hp.dta", replace
table1_tc, by(g) vars(y contn \ z bin) headerperc csv("`out'/D.csv")
table1_tc, by(g) csv("`out'/K.csv")

clear
set obs 2
gen double y = _n * 10.0
gen byte g = _n - 1
label variable y "Outcome"
save "`out'/two.dta", replace
table1_tc, by(g) vars(y contn) csv("`out'/E.csv")

sysuse auto, clear
gen double miss_var = .
label variable miss_var "All Missing"
table1_tc, by(foreign) vars(miss_var contn \ price contn) csv("`out'/F.csv")
sysuse auto, clear
table1_tc rep78 foreign, by(foreign) csv("`out'/G.csv")
table1_tc, by(rep78) vars(price contn) total(before) csv("`out'/H.csv")
table1_tc, by(foreign) vars(price contn \ rep78 cat) total(after) test statistic smd csv("`out'/J.csv")

clear
input byte(g fw) double x
0 1  0
0 1 10
1 1  0
1 1  0
1 1  0
1 1 40
end
save "`out'/ke1.dta", replace
table1_tc, by(g) vars(x contn) smd nopvalue csv("`out'/I.csv")
