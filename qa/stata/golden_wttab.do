/*  golden_wttab.do - Stata reference for R's wttab() (IMPLEMENTATION_PLAN.md
    task 7.12; goldens W15-W18)

    Stata tabtools has no wttab command (EW4: a Stata twin can follow; this
    program is a starting point for it, not part of tabtools). The goldens
    need Stata's own numbers in Stata's own layout, so this program computes
    every statistic in Stata -- N, mean, SD, min and max from -summarize-,
    the percentiles from -_pctile-, ESS = (sum w)^2 / sum(w^2) from two
    -summarize- sums, and the truncation cut-offs from -_pctile- over all
    weights -- formats each cell with Stata's string(), and writes the table
    with tabtools' own -puttab- (varlist source, -varlabels- headers). The
    golden is therefore exactly what tabtools 2.1.11's puttab writes for
    those cells.

    Why not -puttab, matrix(W)-: puttab formats a matrix column by column
    (whole numbers only when the whole column is), so the N and truncated
    counts would print as "15000.00"; and Stata drops spaces from matrix
    column names ("Treated (1/99)" -> "Treated(1/99)"; probed), so headers
    such as "Truncated 1/99: SSRI" cannot be matrix stripes. The numbers are
    returned as r(W) (the body cells, in the table's orientation) and
    r(trunc) (per truncation: lower and upper percentile, cut-offs, counts
    raised and lowered), which the R runner compares at full precision.

    Run from a scenario's stata_setup (cwd = tests/testthat/golden):
        run "../../../qa/stata/golden_wttab.do"

    Syntax (mirrors wttab() in R; trunc() in percent, in pairs):
        wttab weightvar [if] [in] using file.xlsx, [by(var) period(var)
            trunc(lo hi [lo hi ...]) nooverall layout(wide|long) digits(#)
            title() footnote() nofootnote sheet() csv() markdown()
            <other puttab styling options>]
*/

version 17.0

capture program drop wttab
program define wttab, rclass
    version 17.0
    syntax varname(numeric) [if] [in] using/ , [BY(varname numeric) PERiod(varname numeric) ///
        TRUNC(numlist >=0 <=100) NOOVERall LAYout(string) DIGits(integer 2) ///
        TItle(string) FOOTnote(string) NOFOOTnote SHeet(string) CSV(string) MARKdown(string) *]

    local w `varlist'
    if "`sheet'" == "" local sheet "Weights"
    if "`layout'" == "" local layout = cond("`period'" != "", "long", "wide")
    if "`layout'" == "wide" & "`period'" != "" {
        display as error "a table by period is long"
        exit 198
    }
    marksample touse
    if "`by'" != "" markout `touse' `by'
    if "`period'" != "" markout `touse' `period'
    quietly count if `touse'
    local N = r(N)
    if `N' == 0 error 2000
    quietly count if `touse' & `w' < 0
    if r(N) {
        display as error "weights must not be negative"
        exit 459
    }

    preserve
    quietly keep if `touse'

    * ---- groups: Overall, then the by() values -------------------------
    local G 0
    if "`by'" == "" {
        local G 1
        local glab1 ""
        local gcond1 "1"
    }
    else {
        if "`nooverall'" == "" {
            local ++G
            local glab`G' "Overall"
            local gcond`G' "1"
        }
        quietly levelsof `by', local(blev)
        local vl : value label `by'
        local bin 1
        foreach v of local blev {
            if !inlist(`v', 0, 1) local bin 0
        }
        if "`vl'" == "" & `bin' {
            * An unlabelled 0/1 treatment: Treated (1), then Untreated (0).
            foreach v in 1 0 {
                if strpos(" `blev' ", " `v' ") {
                    local ++G
                    local glab`G' = cond(`v' == 1, "Treated", "Untreated")
                    local gcond`G' "`by' == `v'"
                }
            }
        }
        else {
            _wttab_levlabels `by' "`blev'" `digits'
            local k 0
            foreach v of local blev {
                local ++k
                local ++G
                local glab`G' `"`s(lab`k')'"'
                local gcond`G' "`by' == `v'"
            }
        }
    }

    * ---- periods --------------------------------------------------------
    local P 1
    local plab1 ""
    local pcond1 "1"
    if "`period'" != "" {
        quietly levelsof `period', local(plev)
        _wttab_levlabels `period' "`plev'" `digits'
        local P 0
        foreach v of local plev {
            local ++P
            local plab`P' `"`s(lab`P')'"'
            local pcond`P' "`period' == `v'"
        }
    }

    * ---- versions: the weights, then each truncated copy ---------------
    tempvar w0 w20
    quietly gen double `w0' = `w'
    quietly gen double `w20' = `w0'^2
    local V 1
    local vw1 `w0'
    local vw21 `w20'
    local vlab1 = cond("`trunc'" != "", "Untruncated", "")
    local ntr : word count `trunc'
    if mod(`ntr', 2) {
        display as error "trunc() takes pairs of percentiles"
        exit 198
    }
    tempname TR
    if `ntr' {
        matrix `TR' = J(`ntr' / 2, 6, .)
        matrix colnames `TR' = lower_q upper_q lower upper n_low n_high
        forvalues k = 1/`=`ntr' / 2' {
            local lo : word `=2 * `k' - 1' of `trunc'
            local hi : word `=2 * `k'' of `trunc'
            * The pooled cut-offs; p = 0 / 100 are the minimum / maximum.
            quietly summarize `w0'
            tempname clo chi
            scalar `clo' = r(min)
            scalar `chi' = r(max)
            local pp ""
            if `lo' > 0 & `lo' < 100 local pp "`lo'"
            if `hi' > 0 & `hi' < 100 local pp "`pp' `hi'"
            if "`pp'" != "" {
                quietly _pctile `w0', p(`pp')
                if `lo' > 0 & `lo' < 100 {
                    scalar `clo' = r(r1)
                    if `hi' > 0 & `hi' < 100 scalar `chi' = r(r2)
                }
                else if `hi' > 0 & `hi' < 100 scalar `chi' = r(r1)
                if `lo' >= 100 scalar `clo' = `chi'
            }
            local ++V
            tempvar wv wv2 mv
            quietly gen double `wv' = `w0'
            quietly replace `wv' = `clo' if `w0' < `clo'
            quietly replace `wv' = `chi' if `w0' > `chi'
            quietly gen double `wv2' = `wv'^2
            quietly gen byte `mv' = (`w0' < `clo') | (`w0' > `chi')
            local vw`V' `wv'
            local vw2`V' `wv2'
            local vmv`V' `mv'
            * The label with a leading zero, as R's wttab() writes it:
            * numlist normalises 0.1 to .1 (review 7w F4).
            local lot = strofreal(`lo', "%12.0g")
            local hit = strofreal(`hi', "%12.0g")
            if substr("`lot'", 1, 1) == "." local lot "0`lot'"
            if substr("`hit'", 1, 1) == "." local hit "0`hit'"
            local vlab`V' "Truncated `lot'/`hit'"
            quietly count if `w0' < `clo'
            local nlo = r(N)
            quietly count if `w0' > `chi'
            matrix `TR'[`k', 1] = (`lo' / 100, `hi' / 100, `clo', `chi', `nlo', r(N))
        }
    }

    * ---- the cells: period x version x group ---------------------------
    local keys "n mean sd min p1 p25 p50 p75 p99 max ess ess_pct"
    if `ntr' local keys "`keys' n_trunc"
    local nk : word count `keys'
    local C = `P' * `V' * `G'
    tempname S
    matrix `S' = J(`C', `nk', .)
    matrix colnames `S' = `keys'
    local c 0
    forvalues p = 1/`P' {
        forvalues v = 1/`V' {
            forvalues g = 1/`G' {
                local ++c
                local cond "(`pcond`p'') & (`gcond`g'')"
                quietly summarize `vw`v'' if `cond'
                local n = r(N)
                matrix `S'[`c', 1] = r(N)
                if `n' {
                    matrix `S'[`c', 2] = (r(mean), r(sd), r(min))
                    matrix `S'[`c', 10] = r(max)
                    quietly _pctile `vw`v'' if `cond', p(1 25 50 75 99)
                    matrix `S'[`c', 5] = (r(r1), r(r2), r(r3), r(r4), r(r5))
                }
                if `n' == 1 matrix `S'[`c', 3] = .
                if `ntr' & `v' > 1 {
                    quietly count if `cond' & `vmv`v''
                    matrix `S'[`c', 13] = r(N)
                }
                local cp`c' `"`plab`p''"'
                local cv`c' `"`vlab`v''"'
                local cg`c' `"`glab`g''"'
            }
        }
    }
    * ESS = (sum w)^2 / sum(w^2), from the two sums held as scalars.
    local c 0
    forvalues p = 1/`P' {
        forvalues v = 1/`V' {
            forvalues g = 1/`G' {
                local ++c
                local cond "(`pcond`p'') & (`gcond`g'')"
                quietly summarize `vw`v'' if `cond'
                if r(N) {
                    tempname s1
                    scalar `s1' = r(sum)
                    quietly summarize `vw2`v'' if `cond'
                    if r(sum) > 0 {
                        matrix `S'[`c', 11] = `s1'^2 / r(sum)
                        matrix `S'[`c', 12] = 100 * `S'[`c', 11] / `S'[`c', 1]
                    }
                    else matrix `S'[`c', 11] = .
                }
            }
        }
    }

    * ---- formats and labels ---------------------------------------------
    local lab_n "N"
    local lab_mean "Mean"
    local lab_sd "SD"
    local lab_min "Min"
    local lab_p1 "P1"
    local lab_p25 "P25"
    local lab_p50 "Median"
    local lab_p75 "P75"
    local lab_p99 "P99"
    local lab_max "Max"
    local lab_ess "ESS"
    local lab_ess_pct "ESS (%)"
    local lab_n_trunc "Truncated (n)"
    foreach k of local keys {
        local fmt_`k' "%32.`digits'fc"
    }
    local fmt_n "%32.0fc"
    local fmt_n_trunc "%32.0fc"
    local fmt_ess "%32.1fc"
    local fmt_ess_pct "%32.1f"

    if "`footnote'" == "" & "`nofootnote'" == "" {
        local footnote "ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N."
        if `ntr' local footnote "`footnote' Truncated l/u: weights below the l-th or above the u-th percentile of all weights set to that percentile; Truncated (n) counts the weights changed."
    }

    * Key-column headers of the long layout: the variable label, else name.
    if "`period'" != "" {
        local perhead : variable label `period'
        if `"`perhead'"' == "" local perhead "`period'"
    }
    if "`by'" != "" {
        local byhead : variable label `by'
        if `"`byhead'"' == "" local byhead "`by'"
    }
    clear
    tempname W
    if "`layout'" == "wide" {
        matrix `W' = `S''
        quietly set obs `nk'
        quietly gen strL c1 = ""
        label variable c1 "Statistic"
        local i 0
        foreach k of local keys {
            local ++i
            quietly replace c1 = "`lab_`k''" in `i'
        }
        forvalues c = 1/`C' {
            local j = `c' + 1
            quietly gen strL c`j' = ""
            if `ntr' & "`by'" != "" local head `"`cv`c'': `cg`c''"'
            else if `ntr' local head `"`cv`c''"'
            else if "`by'" != "" local head `"`cg`c''"'
            else local head "Weights"
            label variable c`j' `"`head'"'
            local i 0
            foreach k of local keys {
                local ++i
                quietly replace c`j' = cond(missing(el(`S', `c', `i')), "", strtrim(string(el(`S', `c', `i'), "`fmt_`k''"))) in `i'
            }
        }
        local K = `C' + 1
    }
    else {
        matrix `W' = `S'
        quietly set obs `C'
        * key columns: period, weights, group
        local nkc 0
        if "`period'" != "" {
            local ++nkc
            local kh`nkc' `"`perhead'"'
            local kt`nkc' "p"
        }
        if `ntr' {
            local ++nkc
            local kh`nkc' "Weights"
            local kt`nkc' "v"
        }
        if "`by'" != "" {
            local ++nkc
            local kh`nkc' `"`byhead'"'
            local kt`nkc' "g"
        }
        local allkeys = `nkc'
        if `nkc' == 0 {
            local nkc 1
            local kh1 "Weights"
            local kt1 "all"
        }
        forvalues j = 1/`nkc' {
            quietly gen strL c`j' = ""
            label variable c`j' `"`kh`j''"'
        }
        forvalues c = 1/`C' {
            forvalues j = 1/`nkc' {
                if "`kt`j''" == "all" local full`c'_`j' "All"
                else local full`c'_`j' `"`c`kt`j''`c''"'
            }
            * a key is shown where its block starts
            forvalues j = 1/`nkc' {
                local show 1
                if `c' > 1 & `j' < `nkc' {
                    local show 0
                    local cprev = `c' - 1
                    forvalues jj = 1/`j' {
                        if `"`full`c'_`jj''"' != `"`full`cprev'_`jj''"' local show 1
                    }
                }
                if `show' quietly replace c`j' = `"`full`c'_`j''"' in `c'
            }
        }
        local i 0
        foreach k of local keys {
            local ++i
            local j = `nkc' + `i'
            quietly gen strL c`j' = ""
            label variable c`j' "`lab_`k''"
            forvalues c = 1/`C' {
                quietly replace c`j' = cond(missing(el(`S', `c', `i')), "", strtrim(string(el(`S', `c', `i'), "`fmt_`k''"))) in `c'
            }
        }
        local K = `nkc' + `nk'
    }
    quietly compress
    local opts `"sheet("`sheet'") varlabels"'
    if `"`title'"' != "" local opts `"`opts' title(`"`title'"')"'
    if `"`footnote'"' != "" local opts `"`opts' footnote(`"`footnote'"')"'
    if `"`csv'"' != "" local opts `"`opts' csv(`"`csv'"')"'
    if `"`markdown'"' != "" local opts `"`opts' markdown(`"`markdown'"')"'
    quietly puttab c1-c`K' using `"`using'"', `opts' `options'
    local r_rows = r(n_rows)
    local r_cols = r(n_cols)
    local r_data = r(n_datarows)
    local r_mdrows = r(markdown_rows)
    local r_mdcols = r(markdown_cols)
    local r_sheet `"`r(sheet)'"'
    restore

    if `"`markdown'"' != "" display as text "Markdown exported to `markdown'"
    display as text "wttab: wrote `r_data' rows x `r_cols' cols to sheet `r_sheet' in `using'"

    return scalar n_rows = `r_rows'
    return scalar n_cols = `r_cols'
    return scalar n_datarows = `r_data'
    if `"`markdown'"' != "" {
        return scalar markdown_rows = `r_mdrows'
        return scalar markdown_cols = `r_mdcols'
        return local markdown `"`markdown'"'
    }
    if `"`csv'"' != "" return local csv `"`csv'"'
    return local file `"`using'"'
    return local sheet `"`r_sheet'"'
    return scalar N = `N'
    return matrix W = `W'
    if `ntr' return matrix trunc = `TR'
end

* Display labels of the values of a by()/period() variable, as wttab()'s:
* the value label where there is one, else puttab's number rule (whole
* numbers when every value is whole, else digits() decimals). s(lab#).
capture program drop _wttab_levlabels
program define _wttab_levlabels, sclass
    version 17.0
    args var levs digits
    local vl : value label `var'
    local allint 1
    foreach v of local levs {
        if `v' != floor(`v') local allint 0
    }
    local fmt = cond(`allint', "%32.0f", "%32.`digits'f")
    sreturn clear
    local k 0
    foreach v of local levs {
        local ++k
        local lab = strtrim(string(`v', "`fmt'"))
        if "`vl'" != "" {
            local l : label `vl' `v', strict
            if `"`l'"' != "" local lab `"`l'"'
        }
        sreturn local lab`k' `"`lab'"'
    }
end
