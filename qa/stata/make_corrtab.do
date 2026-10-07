* Additive CR001-CR013 only. Caller authenticates pinned source and owns
* the isolated destination. Never reads or rewrites existing fixtures.
version 17.0
args native output
clear all
set more off
set linesize 255
adopath ++ "`native'"
cd "`output'"

capture program drop _cr_emit
program define _cr_emit
    args id
    tempname C P N fh
    matrix `C' = r(C)
    matrix `P' = r(P)
    matrix `N' = r(N)
    local names : colnames `C'
    file open `fh' using "`id'_matrices.csv", write text replace
    file write `fh' "row,col,row_variable,column_variable,C,P,N" _n
    forvalues i=1/`=rowsof(`C')' {
        local vi : word `i' of `names'
        forvalues j=1/`=colsof(`C')' {
            local vj : word `j' of `names'
            file write `fh' "`i',`j',`vi',`vj'"
            foreach m in C P N {
                file write `fh' ","
                if missing(el(``m'',`i',`j')) file write `fh' "."
                else file write `fh' (strtrim(string(el(``m'',`i',`j'), "%23.16e")))
            }
            file write `fh' _n
        }
    }
    file close `fh'
end

input double(x y z)
1 1 .
2 2 1
3 4 2
4 3 3
. 8 4
. . 5
end
save "corrtab_v251.dta", replace
forvalues k=1/13 {
    use "corrtab_v251.dta", clear
    local id = "CR" + string(`k', "%03.0f")
    local vars "x y z"
    local qualifier ""
    local opts "pvalues"
    if `k' == 2 local opts "upper pvalues"
    if `k' == 3 local opts "full pvalues"
    if `k' == 4 {
        clear
        set obs 4
        gen double x = cond(_n<=2,1,_n-1)
        gen double y = cond(_n==1,1,cond(_n<=3,2,3))
        local vars "x y"
        local opts "full spearman pvalues"
    }
    if `k' == 5 {
        clear
        set obs 5
        gen double x = cond(_n<=4,_n,.)
        gen double y = cond(_n==1,1,cond(_n==2,3,cond(_n==3,5,cond(_n==4,4,2))))
        local vars "x y"
        local opts "spearman pvalues"
    }
    if inlist(`k',6,7,8) {
        clear
        set obs 3
        gen double x = _n
        gen double positive = _n
        gen double negative = 4-_n
        local vars "x positive negative"
        local opts "full spearman pvalues"
        if `k' == 7 local opts "full pvalues"
        if `k' == 8 drop in 3
    }
    if inlist(`k',9,10) {
        clear
        set obs 4
        gen double x = _n
        gen double constant = 1
        gen double singleton = cond(_n==1,1,.)
        local vars "x constant singleton"
        local opts "full pvalues"
        if `k' == 10 local vars "constant singleton"
    }
    if `k' == 11 local opts "full star(.1 .3 .6)"
    if `k' == 12 {
        label variable x "Repeated label"
        label variable y "Repeated label"
        local vars "x y"
        local qualifier "if _n<=4"
        local opts "upper pvalues digits(4) borderstyle(academic)"
    }
    if `k' == 13 {
        clear
        set obs 2
        gen double x = .
        gen double y = .
        local vars "x y"
        local opts "full spearman pvalues"
    }
    display "CR_START_`id'"
    corrtab `vars' `qualifier', `opts' title("Title") headershade zebra ///
        xlsx("`id'.xlsx") sheet("S") csv("`id'.csv") markdown("`id'.md")
    _cr_emit `id'
    display "CR_END_`id'"
}
display "CR_PARITY_COMPLETE"
