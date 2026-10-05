* Milestone H task H10: Stata probes for nonfinite values and overflow in
* table1_tc, and Stata's e-notation for three-digit exponents.
* Run on a `git archive` export of Stata-Tools f42ee9cd (tabtools 2.1.12;
* first written for 33528c76, 2.1.11),
* never the live checkout:
*   git -C ~/Stata-Tools archive f42ee9cd tabtools | tar -x -C <dir>
*   stata-mp -b do probe_h10_overflow.do   (with <dir>/tabtools first on the adopath)
* Results (2026-09-26, Stata 17; rerun on 2.1.12 for C5) are pinned in
* tests/testthat/test-table1-hardening.R and
* tests/testthat/fixtures/table1_hardening/stata_fmt_exp3.csv.
clear all
set more off
* adopath ++ "<dir>/tabtools"
capture program drop showcsv
program define showcsv
    args f
    display "----- `f'"
    type "`f'"
end

* A: 1e200 values, contn, no by
clear
input double x
1e200
2e200
3e200
end
capture noisily table1_tc, vars(x contn) csv("A.csv")
display "rc A = " _rc
capture showcsv A.csv

* A2: conts and contln on the same values
capture noisily table1_tc, vars(x conts) csv("A2.csv")
display "rc A2 = " _rc
capture showcsv A2.csv
capture noisily table1_tc, vars(x contln) csv("A3.csv")
display "rc A3 = " _rc
capture showcsv A3.csv

* B: 1e200 values by group with smd
clear
input double x byte g
1e200 1
2e200 1
3e200 1
1e200 2
5e200 2
6e200 2
end
capture noisily table1_tc, by(g) vars(x contn) smd csv("B.csv")
display "rc B = " _rc
capture showcsv B.csv
capture noisily table1_tc, by(g) vars(x conts) smd csv("B2.csv")
display "rc B2 = " _rc
capture showcsv B2.csv
capture noisily table1_tc, by(g) vars(x contln) smd csv("B3.csv")
display "rc B3 = " _rc
capture showcsv B3.csv
* 1e154-ish: squares overflow barely
clear
input double x
1e155
2e155
3e155
end
capture noisily table1_tc, vars(x contn) csv("A4.csv")
display "rc A4 = " _rc
capture showcsv A4.csv

* C: x = 1:3, all weights 1e308
clear
input double x double w
1 1e308
2 1e308
3 1e308
end
capture noisily table1_tc, vars(x contn) wt(w) csv("C.csv")
display "rc C = " _rc
capture showcsv C.csv

* E: w = 1, 1, 1e308 without by
clear
input double x double w
1 1
2 1
3 1e308
end
capture noisily table1_tc, vars(x contn) wt(w) csv("E.csv")
display "rc E = " _rc
capture showcsv E.csv

* D: 20 rows, two arms, one weight 1e308 among ones
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 1
replace w = 1e308 in 3
gen byte b = mod(_n, 2)
gen byte c = mod(_n, 3)
capture noisily table1_tc, by(g) vars(x contn \ x conts \ b bin \ c cat) wt(w) smd csv("D.csv")
display "rc D = " _rc
capture showcsv D.csv
capture noisily table1_tc, by(g) vars(x contn \ b bin) wt(w) wtn csv("D2.csv")
display "rc D2 = " _rc
capture showcsv D2.csv

* F: all weights 1e200 (squares overflow)
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 1e200
gen byte b = mod(_n, 2)
capture noisily table1_tc, by(g) vars(x contn \ b bin) wt(w) smd csv("F.csv")
display "rc F = " _rc
capture showcsv F.csv
* F2: weights 1e160 (squares overflow, sums fine)
replace w = 1e160
capture noisily table1_tc, by(g) vars(x contn \ b bin) wt(w) smd csv("F2.csv")
display "rc F2 = " _rc
capture showcsv F2.csv

* G: smallcells beyond integer range
clear
set obs 20
gen byte g = 1 + (_n > 10)
gen byte b = mod(_n, 2)
capture noisily table1_tc, by(g) vars(b bin) smallcells(3000000000) csv("G.csv")
display "rc G = " _rc
capture showcsv G.csv
capture noisily table1_tc, by(g) vars(b bin) smallcells(1e10) csv("G2.csv")
display "rc G2 = " _rc
capture showcsv G2.csv

* H: huge frequency weights
clear
set obs 4
gen double x = _n
gen byte g = 1 + (_n > 2)
gen double fw = 1e15
capture noisily table1_tc [fweight=fw], by(g) vars(x contn) csv("H.csv")
display "rc H = " _rc
capture showcsv H.csv
replace fw = 1e308
capture noisily table1_tc [fweight=fw], by(g) vars(x contn) csv("H2.csv")
display "rc H2 = " _rc
capture showcsv H2.csv

* --- display formats with three-digit exponents, near-overflow values/weights
file open fh using "fmt.csv", write replace
file write fh "fmt,x,s" _n
foreach f in %2.0f %3.0f %5.1f %9.0f %9.1f %9.2f %9.4f %12.0fc %4.2f %6.2f %10.3f %20.0f %5.0g %8.0g %9.0g %10.0g %2.1f %1.0f %7.0f %8.0f %8.3f %11.0f {
  foreach x in 1.23456e8 1.23456e99 1.23456e100 2e200 -2e200 1.817e200 8e307 -1.23456e100 1.23456e-100 {
    local s = string(`x', "`f'")
    file write fh "`f',`x',`s'" _n
  }
}
file close fh

* near-overflow values and weights (Stata's largest double is about 8.99e307)
clear
input double x
8e307
8e307
8e307
end
capture noisily table1_tc, vars(x contn) csv("V1.csv")
display "rc V1 = " _rc
capture noisily table1_tc, vars(x conts) csv("V2.csv")
display "rc V2 = " _rc
clear
input double x
4e307
5e307
end
capture noisily table1_tc, vars(x contn) csv("V3.csv")
display "rc V3 = " _rc
clear
input double x double w
1 1
2 1
3 8e307
end
capture noisily table1_tc, vars(x contn) wt(w) csv("E8.csv")
display "rc E8 = " _rc
clear
input double x double w
1 8e307
2 8e307
3 8e307
end
capture noisily table1_tc, vars(x contn) wt(w) csv("C8.csv")
display "rc C8 = " _rc
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 1
replace w = 8e307 in 3
gen byte b = mod(_n, 2)
gen byte c = mod(_n, 3)
capture noisily table1_tc, by(g) vars(x contn \ x conts \ b bin \ c cat) wt(w) smd csv("D8.csv")
display "rc D8 = " _rc
capture noisily table1_tc, by(g) vars(x contn \ b bin \ c cat) wt(w) wtn csv("D8n.csv")
display "rc D8n = " _rc
capture noisily table1_tc, by(g) vars(x contn \ b bin \ c cat) wt(w) wtcompare csv("D8c.csv")
display "rc D8c = " _rc
replace w = 8e307
capture noisily table1_tc, by(g) vars(x contn \ x conts \ b bin \ c cat) wt(w) smd csv("D8all.csv")
display "rc D8all = " _rc

* --- overflowing weight sums with wtn/percent_n/total; overflowing value sums
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 8e307
gen byte b = mod(_n, 2)
gen byte c = mod(_n, 3)
capture noisily table1_tc, by(g) vars(x contn \ b bin \ c cat) wt(w) wtn total(after) csv("W1.csv")
display "rc W1 = " _rc
capture noisily table1_tc, by(g) vars(b bin \ c cat) wt(w) percent_n csv("W2.csv")
display "rc W2 = " _rc
* sums overflow in one group only
replace w = 1 if g == 2
capture noisily table1_tc, by(g) vars(x contn \ x conts \ b bin \ c cat) wt(w) wtn smd total(after) csv("W3.csv")
display "rc W3 = " _rc
* 1e200 values with a total column and a group of one observation
clear
input double x byte g
1e200 1
2e200 1
3e200 2
end
capture noisily table1_tc, by(g) vars(x contn) total(before) csv("W4.csv")
display "rc W4 = " _rc
* values whose sum overflows in one group only
clear
input double x byte g
5e307 1
5e307 1
1 2
2 2
end
capture noisily table1_tc, by(g) vars(x contn \ x contln) smd total(after) csv("W5.csv")
display "rc W5 = " _rc

* --- Mata's range and extended-precision expressions
mata:
a = 5.5e201
b = 1e201
a*a/b
a*a
s = a*a
s
s/b
c = 1e200
x = (1::10)
w = J(10,1,1e200)
sxg = sum(w :* x)
sx2g = sum(w :* (x:^2))
swg = sum(w)
sxg, sx2g, swg
ss = sx2g - sxg * sxg / swg
ss
sxg * sxg
w2 = 8e307
w2 + w2
(w2 + w2) / 2
end
display %21x maxdouble()
display %21x 2^1023
display maxdouble() == 2^1023
display %21x c(maxdouble)
mata: sum((6e153^2, 6e153^2, 6e153^2))
mata: x = (6e153 \ 6e153 \ 6e153); sum(x:^2) - sum(x)*sum(x)/3

* --- review R7: the decimals follow the exponent before rounding (9.99e99,
* 9.99e-100); the review's own probe of +-9.99e99 on 14 formats and this one
* are appended to tests/testthat/fixtures/table1_hardening/stata_fmt_exp3.csv
file open fh using "g.csv", write replace
foreach f in %5.0g %8.0g %9.0g %10.0g {
  foreach x in 9.99e-100 9.96e-101 1.5e-100 9.99e-99 1.04e-100 {
    local s = string(`x', "`f'")
    file write fh "`f';`x';`s'" _n
  }
}
file close fh

* --- C5 (tabtools 2.1.12 rescales overflowing weights): the review-R2
* cells (only (sum w)^2 overflows; sum(w) * (n - 1) overflows) and the
* overflowing group with smallcells(3), pinned in test-table1-hardening.R
clear
input double x double w
2 3e153
3 3e153
4 3e153
5 3e153
end
capture noisily table1_tc, vars(x contn) wt(w) csv("R2a.csv")
display "rc R2a = " _rc
replace w = 4e153
capture noisily table1_tc, vars(x contn) wt(w) csv("R2b.csv")
display "rc R2b = " _rc
clear
input double x double w
1 4e307
2 1
3 1
4 1
end
capture noisily table1_tc, vars(x contn) wt(w) csv("R2c.csv")
display "rc R2c = " _rc
clear
set obs 20
gen double x = _n
gen byte g = 1 + (_n > 10)
gen double w = 8e307
replace w = 1 if g == 2
gen byte b = mod(_n, 2)
gen byte c = mod(_n, 3)
capture noisily table1_tc, by(g) vars(b bin \ c cat) wt(w) wtn smallcells(3) csv("SC3.csv")
display "rc SC3 = " _rc
replace w = 8e307
capture noisily table1_tc, by(g) vars(b bin \ c cat) wt(w) wtn total(after) csv("W1b.csv")
display "rc W1b = " _rc
capture noisily table1_tc, by(g) vars(b bin) wt(w) percent_n csv("W2b.csv")
display "rc W2b = " _rc
