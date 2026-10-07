* Additive RT001-RT012, 712044f8 / tabtools 2.5.1. No old fixture is read
* or changed. Caller owns the isolated output directory and authenticates pin.
version 17.0
args native output
clear all
set more off
set linesize 255
adopath ++ "`native'"
cd "`output'"

capture program drop _rt_emit
program define _rt_emit
    args id
    local n = r(N)
    local nz = r(N_zero)
    local nc = r(N_noci)
    local nt = r(N_nopt)
    local nm = r(N_maskfit)
    tempname m c fh
    matrix `m' = r(estimates)
    capture matrix `c' = r(clusters)
    local has_c = !_rc
    file open `fh' using "`id'_estimates.csv", write text replace
    file write `fh' "outcome,group,level,events,persontime,rate,lb,ub" _n
    forvalues i=1/`=rowsof(`m')' {
        forvalues j=1/8 {
            if `j' > 1 file write `fh' ","
            if missing(el(`m',`i',`j')) file write `fh' "."
            else file write `fh' (strtrim(string(el(`m',`i',`j'), "%23.16e")))
        }
        file write `fh' _n
    }
    file close `fh'
    file open `fh' using "`id'_counts.csv", write text replace
    file write `fh' "N,N_zero,N_noci,N_nopt,N_maskfit" _n
    file write `fh' "`n',`nz',`nc',`nt',`nm'" _n
    file close `fh'
    if `has_c' {
        file open `fh' using "`id'_clusters.csv", write text replace
        file write `fh' "group,outcome,clusters" _n
        forvalues i=1/`=rowsof(`c')' {
            forvalues j=1/`=colsof(`c')' {
                file write `fh' "`i',`j'," (strtrim(string(el(`c',`i',`j'), "%23.16e"))) _n
            }
        }
        file close `fh'
    }
end

input double(g h e f y z cid w)
1 1 3 1 1 2 1 2
1 2 1 4 2 1 2 1
1 1 2 0 1 3 3 1
1 2 0 1 3 1 4 1
2 1 1 2 2 4 1 1
2 2 4 1 1 2 2 1
2 1 0 0 3 1 4 1
2 2 1 0 1 3 5 1
3 1 0 0 4 2 6 1
4 2 0 0 0 0 7 1
end
label define groups 1 "A" 2 "B" 3 "C" 4 "D"
label values g groups
label variable g "Group"
save "ratetab_rt.dta", replace
forvalues k=1/5 {
    use "ratetab_rt.dta", clear
    local id "RT00`k'"
    local by "g h"
    local event "e"
    local labels "e"
    local exposure "y"
    local opts ""
    if `k' == 2 {
        local event "e f"
        local labels "e \ f"
        local exposure "y z"
    }
    if `k' == 3 {
        replace f = . in 2
        replace g = . in 5
        replace g = . in 6
        replace h = . in 6
        local event "e f"
        local labels "e \ f"
    }
    if `k' == 4 local opts "ci(poisson) level(90) pyscale(10) pydigits(1)"
    if `k' == 5 local opts "ci(cluster(cid))"
    display "RT_START_`id'"
    ratetab `by', events(`event') exposure(`exposure') per(1) title("Title") ///
        outlabels("`labels'") explabels("Group \ h") headershade zebra ///
        xlsx("`id'.xlsx") sheet("S") csv("`id'.csv") markdown("`id'.md") `opts'
    _rt_emit `id'
    display "RT_END_`id'"
}

clear
input double(g cid e y)
1 1 4 1
1 2 2 1
2 2 1 1
2 3 0 1
end
display "RT_START_RT006"
ratetab g, events(e) exposure(y) per(1) ci(cluster(cid)) smallcells(2) excludemasked ///
    title("Title") outlabels("e") explabels("g") headershade zebra ///
    xlsx("RT006.xlsx") sheet("S") csv("RT006.csv") markdown("RT006.md")
_rt_emit RT006
display "RT_END_RT006"

forvalues k=7/9 {
    clear
    set obs 3
    generate double g = 1
    generate double cid = _n
    generate double e = cond(_n == 1, 6, 0)
    generate double y = 1
    local id "RT00`k'"
    if `k' == 8 replace cid = 1
    if `k' == 9 replace e = 1
    display "RT_START_`id'"
    ratetab g, events(e) exposure(y) per(1) ci(cluster(cid)) ///
        title("Title") outlabels("e") explabels("g") headershade zebra ///
        xlsx("`id'.xlsx") sheet("S") csv("`id'.csv") markdown("`id'.md")
    _rt_emit `id'
    display "RT_END_`id'"
}

clear
input double(group e y) str1 g_group
2 1 2 "X"
1 0 4 "Y"
end
label define collision 1 "Low" 2 "High"
label values group collision
label variable group "Grouping label"
display "RT_START_RT010"
ratetab group g_group group, events(e) exposure(y) per(1) saving("RT010_saved.dta", replace) ///
    title("Title") outlabels("e") explabels("Grouping label \ g_group \ Grouping label") headershade zebra ///
    xlsx("RT010.xlsx") sheet("S") csv("RT010.csv") markdown("RT010.md")
_rt_emit RT010
display "RT_END_RT010"

clear
input double(g e y)
1 1 2
2 0 4
3 0 0
end
display "RT_START_RT011"
ratetab g, events(e) exposure(y) per(1) smallcells(2) zerocells(dash, persontime) ///
    title("Title") outlabels("e") explabels("g") headershade zebra saving("RT011_saved.dta", replace) ///
    xlsx("RT011.xlsx") sheet("S") csv("RT011.csv") markdown("RT011.md")
_rt_emit RT011
display "RT_END_RT011"

clear
input double(g cid e y w)
1 1 3 1 2
1 2 0 1 1
1 3 0 1 1
2 4 100 1 0
end
drop if w == 0
expand w
display "RT_START_RT012"
ratetab g, events(e) exposure(y) per(1) ci(cluster(cid)) ///
    title("Title") outlabels("e") explabels("g") headershade zebra ///
    xlsx("RT012.xlsx") sheet("S") csv("RT012.csv") markdown("RT012.md")
_rt_emit RT012
display "RT_END_RT012"
display "RT_PARITY_COMPLETE"
