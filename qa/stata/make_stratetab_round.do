/*  make_stratetab_round.do - Stata's 10^(-d), and the rounding unit
    stratetab uses, for round(x, unit) (Phase 7b review F9; task C8).

    Up to tabtools 2.1.13 stratetab rounded rates and ratios with an inline
    round(x, 10^(-d)), and Stata's 10^(-d) is not the correctly rounded
    double for d >= 2 (one ulp above 0.01 at d = 2). Since 2.1.14
    (Stata-Tools 1255176d, stratetab.ado:594) the unit is held in a
    macro first, `local _unit = 10^(-`digits')`, whose text reads back as
    the exact decimal, as regtab has always done (regtab.ado:321).

    Run from the repo root:  stata-mp -b do qa/stata/make_stratetab_round.do
    Writes tests/testthat/fixtures/stratetab_round/stata_round.csv, all as
    %21x so both sides compare exact doubles: d, x, unit = 10^(-d) inline,
    r = round(x, 10^(-d)) (the 2.1.13 rule), munit = the macro unit of
    2.1.14 and mr = round(x, munit) (the 2.1.14 rule). The x are exact
    binary ties at d decimals (odd k / 2^(d+1), plus 1 and 37), where the
    unit decides the direction, and a few decimal knife edges.
*/
version 17.0
clear
tempname fh
file open `fh' using "tests/testthat/fixtures/stratetab_round/stata_round.csv", write text replace
file write `fh' "d,x,unit,r,munit,mr" _n
forvalues d = 0/10 {
    local _unit = 10^(-`d')
    local u = 2^(`d' + 1)
    foreach m in 0 1 37 {
        forvalues k = 1(2)19 {
            local x = `m' + `k' / `u'
            file write `fh' "`d'," (strtrim(string(`m' + `k' / `u', "%21x"))) "," ///
                (strtrim(string(10^(-`d'), "%21x"))) "," ///
                (strtrim(string(round(`m' + `k' / `u', 10^(-`d')), "%21x"))) "," ///
                (strtrim(string(`_unit', "%21x"))) "," ///
                (strtrim(string(round(`m' + `k' / `u', `_unit'), "%21x"))) _n
        }
    }
    foreach x in 422.395 619.245 6.35 0.045 2.675 1.005 {
        file write `fh' "`d'," (strtrim(string(`x', "%21x"))) "," ///
            (strtrim(string(10^(-`d'), "%21x"))) "," ///
            (strtrim(string(round(`x', 10^(-`d')), "%21x"))) "," ///
            (strtrim(string(`_unit', "%21x"))) "," ///
            (strtrim(string(round(`x', `_unit'), "%21x"))) _n
    }
}
file close `fh'
