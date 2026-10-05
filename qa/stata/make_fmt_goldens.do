/*  make_fmt_goldens.do - task 0.5c goldens for stata_fmt() and stata_round()

    Run through make_golden.R (cwd = tests/testthat/golden). Writes stata_fmt.csv:
      kind = "string": result = string(x, fmt)
      kind = "round":  result = round(x, u) as %21.17g, and
                       result_fmt = string(round(x, u), "%w.df") with d = -log10(u)
                       (the regtab estimate path, regtab.ado:2079-2087)
    x is recorded as the decimal literal Stata parsed, so R parses the same
    double; x_17g is the stored value for verification.
*/

version 17.0

local values 0 -0 0.5 1.5 2.5 -2.5 12.5 0.125 -0.125 2.675 1.115 1.005 0.045 ///
    0.0005 0.00049 3.14159265 9.995 99.995 -0.0004 1234.5 12345.678 99999.5 ///
    1234567 -1234567 9999999 12345678 99999999 100000000 123456789 ///
    1000000000 1e15 0.1 0.2 0.3 58.2849 13.4417 1.0e-5 4099 8934 15000
local formats %2.0f %3.0f %5.0f %6.0f %9.0f %4.1f %5.1f %9.1f %4.2f %5.2f ///
    %6.2f %12.2f %5.3f %6.3f %9.4f %12.0fc %9.0fc %6.0fc %-12.0fc %-9.1f %9.0g %8.0g %10.0g
local units 1 0.1 0.01 0.001 0.0001

tempname fh
file open `fh' using "stata_fmt.csv", write text replace
file write `fh' "kind,x,x_17g,fmt,u,result,result_fmt" _n
foreach x of local values {
    local x17 = strtrim(string(`x', "%21.17g"))
    foreach f of local formats {
        local s = string(`x', "`f'")
        file write `fh' `"string,`x',`x17',`f',,"`s'","' _n
    }
    foreach u of local units {
        local d = round(-log10(`u'))
        local r = round(`x', `u')
        local r17 = strtrim(string(`r', "%21.17g"))
        local rf = string(`r', "%12.`d'f")
        file write `fh' `"round,`x',`x17',,`u',`r17',"`rf'""' _n
    }
}
* Probe grid from the 2026-09-25 review (223 values x 24 formats; includes
* the |x| < 0.1 %g cases). Values are %21.17g text, so R parses the same
* double. cwd is tests/testthat/golden.
local probe_formats %2.0f %4.0f %5.1f %6.1f %7.2f %8.3f %9.4f %12.2f %5.3f ///
    %9.0g %10.0g %8.0g %6.0g %12.0g %18.0g %12.0fc %9.0fc %8.0fc %15.0fc ///
    %10.2fc %12.3fc %6.1fc %-8.2f %-10.1fc
tempname pv
file open `pv' using "../../../qa/stata/fmt_probe_values.txt", read text
file read `pv' x
while r(eof) == 0 {
    local x = strtrim("`x'")
    if "`x'" != "" {
        local x17 = strtrim(string(`x', "%21.17g"))
        foreach f of local probe_formats {
            local s = string(`x', "`f'")
            file write `fh' `"string,`x',`x17',`f',,"`s'","' _n
        }
    }
    file read `pv' x
}
file close `pv'

* headerperc: round(n/den, 0.001) * 100 in %9.1f (desctab.ado:1175)
foreach pair in "8934 15000" "6066 15000" "1 3" "2 3" "5 8" "3 8" "1 1" "4099 8198" {
    gettoken n den : pair
    local den = strtrim("`den'")
    local r = round(`n'/`den', 0.001) * 100
    local r17 = strtrim(string(`r', "%21.17g"))
    local rf = string(`r', "%9.1f")
    file write `fh' `"headerperc,`n'/`den',,%9.1f,0.001,`r17',"`rf'""' _n
}
file close `fh'
