/*  make_rowname_probe.do - Stata's r(table) row names for a list of labels,
    by the exact code of tabtools 2.1.12 (C5; first written for 2.1.11 in
    C2, "Golden baseline 2.1.11"):
      regtab    regtab.ado:2923-2957: "." and " " -> "_", "," and ":"
                dropped, 32 bytes, then a name that does not survive
                -matrix rownames- unchanged gets every character outside
                [A-Za-z0-9_] replaced by "_" (else row<k>), then _2, _3 ...
                (base cut to 32 bytes less the suffix) for repeated names
      desctab   desctab.ado:1497-1530: trimmed label, "." and " " -> "_",
                "," dropped, quotes/backtick/$/\/: -> "_", 32 characters,
                round trip else strtoname() else row<k>, then _2, _3 ...
                for repeated names
    Input tests/testthat/fixtures/rownames/labels.txt (one label per line);
    writes rownames.csv there (label index, regtab name, desctab name).
    Only Stata built-ins; it does not load tabtools. Labels holding a double
    quote or a backtick are left out: regtab passes the label through a
    macro, where they break the command itself.

    Run from the repo root:
        stata-mp -b do qa/stata/make_rowname_probe.do
*/
version 17.0
clear all
set more off
local dir "tests/testthat/fixtures/rownames"
import delimited using "`dir'/labels.txt", delimiters("\t") varnames(nonames) ///
    stringcols(_all) encoding("utf-8") clear
rename v1 A
tempname P fh
matrix `P' = J(1, 1, .)
file open `fh' using "`dir'/rownames.csv", write text replace
file write `fh' "k,regtab,desctab" _n
local _dnames ""
local _rnames ""
forvalues k = 1/`=_N' {
    * --- regtab
    local _rname = A[`k']
    local _rname = subinstr("`_rname'", ".", "_", .)
    local _rname = subinstr("`_rname'", " ", "_", .)
    local _rname = subinstr("`_rname'", ",", "", .)
    local _rname = subinstr("`_rname'", ":", "", .)
    local _rname = substr("`_rname'", 1, 32)
    if "`_rname'" == "" local _rname "row`k'"
    capture matrix rownames `P' = `_rname'
    local _back ""
    if _rc == 0 local _back : rownames `P'
    if `"`_back'"' != `"`_rname'"' {
        local _rname = substr(ustrregexra(`"`_rname'"', "[^A-Za-z0-9_]", "_"), 1, 32)
        capture matrix rownames `P' = `_rname'
        local _back ""
        if _rc == 0 local _back : rownames `P'
        if `"`_back'"' != `"`_rname'"' local _rname "row`k'"
    }
    local _rn_base `"`_rname'"'
    local _rn_sfx = 1
    local _rn_taken : list _rname in _rnames
    while `_rn_taken' {
        local ++_rn_sfx
        local _rn_tail "_`_rn_sfx'"
        local _rname = substr(`"`_rn_base'"', 1, 32 - strlen("`_rn_tail'")) + "`_rn_tail'"
        local _rn_taken : list _rname in _rnames
    }
    local _rnames `"`_rnames' `_rname'"'
    local _reg `"`_rname'"'
    * --- desctab
    local _rname = usubstr(ustrregexra(subinstr(subinstr(subinstr( ///
        strtrim(A[`k']), ".", "_", .), " ", "_", .), ",", "", .), ///
        "[" + char(34) + char(39) + char(96) + char(36) + char(92) + char(92) + ":]", "_"), 1, 32)
    local _ok 0
    if `"`_rname'"' != "" {
        capture matrix rownames `P' = `_rname'
        if !_rc {
            local _back : rownames `P'
            if `"`_back'"' == `"`_rname'"' local _ok 1
        }
    }
    if !`_ok' {
        local _rname = strtoname(`"`_rname'"')
        capture matrix rownames `P' = `_rname'
        if !_rc {
            local _back : rownames `P'
            if `"`_back'"' == `"`_rname'"' local _ok 1
        }
    }
    if !`_ok' local _rname "row`k'"
    local _base `"`_rname'"'
    local _kk 1
    while `: list posof `"`_rname'"' in _dnames' > 0 {
        local ++_kk
        local _rname = usubstr(`"`_base'"', 1, 32 - ustrlen("_`_kk'")) + "_`_kk'"
    }
    local _dnames `"`_dnames' `_rname'"'
    file write `fh' "`k'," `"""' `"`_reg'"' `"""' "," `"""' `"`_rname'"' `"""' _n
}
file close `fh'
