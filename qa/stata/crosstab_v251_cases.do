/* Source authoring only. Root executes one case in an owned authenticated
   Stata17 stage with golden_helpers.do loaded. Pin 712044f8 / tabtools2.5.1.
   args: XT001..XT028, owned output directory. No adopath fallback/golden edits.
   Inputs are literal and OUTSIDE loops. XT018 deliberately retains zero-only
   categories to record the actual native boundary rather than rewrite it.
   Root aggregates per-case status CSVs in ID order to crosstab-status.csv. */
version 17.0
args id out
if !regexm("`id'", "^XT0(0[1-9]|1[0-9]|2[0-8])$") exit 198
if `"`out'"' == "" exit 198
set more off
set varabbrev off
set linesize 255
clear
local opts ""
local level 95
local assoc_or 0
local assoc_cs 0

if inlist("`id'", "XT001", "XT002", "XT003", "XT008", "XT017", "XT018", "XT019", "XT025", "XT026") {
    input double(row column frequency)
    1 1 40
    2 1 10
    1 2 20
    2 2 30
    end
}
if "`id'" == "XT001" {
    local opts "or rr rd"
    local assoc_or 1
    local assoc_cs 1
}
if "`id'" == "XT002" {
    replace column = 3-column
    local opts "rowpct or rr rd"
    local assoc_or 1
    local assoc_cs 1
}
if "`id'" == "XT003" {
    replace row = (row-1)/10
    replace column = (column-1)/10
    * Stata value labels require integer codes; fractional numeric codes remain numeric.
    local opts "label totalpct or"
    local assoc_or 1
}
if "`id'" == "XT004" {
    input double(row column frequency)
    1 1 1
    2 1 3
    1 2 3
    2 2 1
    end
    local opts "or"
    local assoc_or 1
}
if "`id'" == "XT005" {
    input double(row column frequency)
    1 1 1
    2 1 2
    1 2 1
    2 2 2
    1 3 1
    2 3 2
    end
}
if inlist("`id'", "XT006", "XT027") {
    input double(row column frequency)
    1 1 5
    2 1 5
    1 2 5
    2 2 5
    end
}
if "`id'" == "XT007" {
    input double(row column frequency)
    1 1 1250
    2 1 386
    1 2 4
    2 2 4
    end
    local opts "or level(90)"
    local level 90
    local assoc_or 1
}
if "`id'" == "XT008" {
    local opts "rr rd level(90)"
    local level 90
    local assoc_cs 1
}
if inlist("`id'", "XT009", "XT015", "XT024") {
    input double(row column frequency)
    1 1 1
    2 1 0
    1 2 0
    2 2 1
    end
}
if "`id'" == "XT009" {
    replace frequency = 2*frequency
    local opts "or"
}
if inlist("`id'", "XT010", "XT011", "XT012") {
    input double(row column frequency)
    1 0 10
    2 0 30
    1 1 20
    2 1 20
    1 2 30
    2 2 10
    end
    if "`id'" == "XT011" replace column = 1e16+2*column
    if "`id'" == "XT012" replace column = 5 if column==2
    local opts "cochran"
}
if "`id'" == "XT013" {
    input double(row column frequency)
    1 -1 497
    2 -1 19
    1 0 560
    2 0 29
    1 1 269
    2 1 24
    end
    local opts "cochran"
}
if "`id'" == "XT014" {
    input double(row column frequency)
    1 1 5
    2 1 3
    1 2 2
    2 2 7
    1 3 4
    2 3 6
    end
    local opts "trend"
}
if "`id'" == "XT015" local opts "trend"
if "`id'" == "XT016" {
    input double(row column frequency)
    1 1 0
    2 1 1
    1 2 1
    2 2 0
    end
    local opts "trend"
}
if "`id'" == "XT017" {
    set obs 6
    replace row = . in 5
    replace column = 1 in 5
    replace frequency = 7 in 5
    replace row = .a in 6
    replace column = 1 in 6
    replace frequency = 3 in 6
    local opts "missing"
}
if "`id'" == "XT018" {
    set obs 5
    replace row = 999 in 5
    replace column = 999 in 5
    replace frequency = 0 in 5
}
if "`id'" == "XT019" local opts "trend missing"
if inlist("`id'", "XT020", "XT021", "XT022") {
    input double(row column frequency)
    1 1 1
    2 1 9
    1 2 9
    2 2 1
    end
    if "`id'" == "XT020" local opts "smallcells(3) or rr rd cochran"
    if "`id'" == "XT021" {
        local opts "smallcells(3, primary) or rr rd cochran"
        local assoc_or 1
        local assoc_cs 1
    }
    if "`id'" == "XT022" local opts "smallcells(3) rowpct"
}
if "`id'" == "XT023" {
    input double(row column frequency)
    1 1 1
    2 1 1
    1 2 0
    2 2 10
    end
    local opts "smallcells(3, primary) rowpct"
}
if "`id'" == "XT024" local opts "smallcells(3, primary) totalpct"
if "`id'" == "XT027" local opts "exact digits(2) headershade zebra"
if "`id'" == "XT028" {
    input double(row column frequency)
    1 1 10
    2 1 10
    3 1 10
    1 2 10
    2 2 10
    3 2 10
    end
    local opts "cochran"
}
local destination xlsx("`out'/`id'.xlsx")
if "`id'" == "XT025" local destination xlsx("`out'/`id'.xlsx") excel("`out'/UNSELECTED.bad")
if "`id'" == "XT026" local destination xlsx("") excel("`out'/`id'.xlsx")
log using "`out'/`id'_console.txt", text replace nomsg name(xt)
capture noisily crosstab row column [fw=frequency], `opts' `destination' sheet(`id') ///
    csv("`out'/`id'.csv") markdown("`out'/`id'.md")
local rc = _rc
local nr .
local nc .
if !`rc' {
    mata: golden_dump_r("`out'/`id'_stored.csv", "`id'")
    tempname xttable
    matrix `xttable' = r(table)
    local nr = rowsof(`xttable')
    local nc = colsof(`xttable')
}
log close xt
tempname status
file open `status' using "`out'/`id'_status.csv", write text replace
file write `status' "id,rc,nrow,ncol,pin" _n
file write `status' "`id',`rc',`nr',`nc',712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129" _n
file close `status'
if !`rc' & (`assoc_or' | `assoc_cs') {
    egen byte xtrow = group(row)
    egen byte xtcol = group(column)
    replace xtrow = xtrow-1
    replace xtcol = xtcol-1
    tempname ci
    file open `ci' using "`out'/`id'_association.csv", write text replace
    file write `ci' "name,value" _n
    if `assoc_or' {
        quietly cc xtrow xtcol [fw=frequency], level(`level')
        file write `ci' "or," %24.17g (r(or)) _n
        file write `ci' "or_lo," %24.17g (r(lb_or)) _n
        file write `ci' "or_hi," %24.17g (r(ub_or)) _n
    }
    if `assoc_cs' {
        quietly cs xtrow xtcol [fw=frequency], level(`level')
        file write `ci' "rr," %24.17g (r(rr)) _n
        file write `ci' "rr_lo," %24.17g (r(lb_rr)) _n
        file write `ci' "rr_hi," %24.17g (r(ub_rr)) _n
        file write `ci' "rd," %24.17g (r(rd)) _n
        file write `ci' "rd_lo," %24.17g (r(lb_rd)) _n
        file write `ci' "rd_hi," %24.17g (r(ub_rd)) _n
    }
    file close `ci'
}
