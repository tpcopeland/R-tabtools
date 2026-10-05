/*  make_smallcells_goldens.do - engine-level goldens for small-cell suppression

    Run through make_golden.R (cwd = tests/testthat/golden, dev tabtools first
    on the adopath). Writes smallcells_engine.csv: random count blocks with
    random exact/sensitive flag profiles, each passed to
    _tabtools_smallcells, with the resulting masks or the return code.
    Matrices are written row-major as ";"-separated values.

    Profiles: 1 every cell and margin published and sensitive (cat block with
    total); 2 cat block without total (hidden last row, unreleased row
    margins); 3 continuous block (N row unpublished, missing row per
    missingsummary); 4 every flag random.
*/

version 17.0
set rng mt64
set seed 20260928

capture program drop _sc_flat
program define _sc_flat, rclass
    args m
    local out ""
    forvalues i = 1/`=rowsof(`m')' {
        forvalues j = 1/`=colsof(`m')' {
            local v = `m'[`i', `j']
            local out "`out'`=cond("`out'" == "", "", ";")'`v'"
        }
    }
    return local s "`out'"
end

tempname fh
file open `fh' using "smallcells_engine.csv", write text replace
file write `fh' "case,profile,k,nr,nc,counts,exact,sensitive,rowexact,rowsensitive,colexact,colsensitive,grandexact,grandsensitive,rc,mask,rowmask,colmask,totalmask,n_primary,n_secondary" _n

local ncase 600
forvalues case = 1/`ncase' {
    local k = 3 + floor(3 * runiform())
    local profile = 1 + floor(4 * runiform())
    local nr = 1 + floor(4 * runiform())
    local nc = 1 + floor(4 * runiform())
    if `profile' == 3 local nr 2
    if `profile' == 2 & `nr' < 2 local nr 2
    matrix C = J(`nr', `nc', 0)
    matrix E = J(`nr', `nc', 1)
    matrix S = J(`nr', `nc', 1)
    matrix RE = J(`nr', 1, 0)
    matrix RS = J(`nr', 1, 0)
    matrix CE = J(1, `nc', 1)
    matrix CS = J(1, `nc', 1)
    local ge = 0
    local gs = 0
    forvalues i = 1/`nr' {
        forvalues j = 1/`nc' {
            local u = runiform()
            if `u' < 0.2 matrix C[`i', `j'] = 0
            else if `u' < 0.55 matrix C[`i', `j'] = 1 + floor((`k' - 1) * runiform())
            else matrix C[`i', `j'] = `k' + floor(13 * runiform())
        }
    }
    if `profile' == 1 {
        matrix RE = J(`nr', 1, 1)
        matrix RS = J(`nr', 1, 1)
        local ge = runiform() < 0.7
        local gs = `ge'
    }
    else if `profile' == 2 {
        forvalues j = 1/`nc' {
            matrix E[`nr', `j'] = 0
            matrix S[`nr', `j'] = 0
        }
    }
    else if `profile' == 3 {
        local ms = runiform() < 0.5
        local tot = runiform() < 0.5
        forvalues j = 1/`nc' {
            matrix E[1, `j'] = 0
            matrix E[2, `j'] = `ms'
            matrix S[2, `j'] = `ms'
        }
        matrix RE = (0 \ `ms' * `tot')
        matrix RS = (`tot' \ `ms' * `tot')
        local ge = `tot'
        local gs = `tot'
    }
    else {
        forvalues i = 1/`nr' {
            forvalues j = 1/`nc' {
                matrix E[`i', `j'] = runiform() < 0.8
                matrix S[`i', `j'] = runiform() < 0.8
            }
            matrix RE[`i', 1] = runiform() < 0.5
            matrix RS[`i', 1] = runiform() < 0.5
        }
        forvalues j = 1/`nc' {
            matrix CE[1, `j'] = runiform() < 0.7
            matrix CS[1, `j'] = runiform() < 0.7
        }
        local ge = runiform() < 0.5
        local gs = runiform() < 0.5
    }
    capture quietly _tabtools_smallcells, counts(C) exact(E) sensitive(S) ///
        rowexact(RE) rowsensitive(RS) colexact(CE) colsensitive(CS) ///
        grandexact(`ge') grandsensitive(`gs') smallcells(`k')
    local rc = _rc
    local m ""
    local rm ""
    local cm ""
    local tm ""
    local np ""
    local ns ""
    if `rc' == 0 {
        matrix M = r(mask)
        matrix RM = r(rowmask)
        matrix CM = r(colmask)
        local tm = r(totalmask)
        local np = r(N_primary_suppressed)
        local ns = r(N_secondary_suppressed)
        _sc_flat M
        local m "`r(s)'"
        _sc_flat RM
        local rm "`r(s)'"
        _sc_flat CM
        local cm "`r(s)'"
    }
    foreach x in C E S RE RS CE CS {
        _sc_flat `x'
        local f_`x' "`r(s)'"
    }
    file write `fh' "`case',`profile',`k',`nr',`nc',`f_C',`f_E',`f_S',`f_RE',`f_RS',`f_CE',`f_CS',`ge',`gs',`rc',`m',`rm',`cm',`tm',`np',`ns'" _n
}
file close `fh'
