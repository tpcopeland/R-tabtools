/*  golden_helpers.do - programs shared by the golden-generation driver

    Loaded by the run.do that make_golden.R writes into
    qa/.stata-work. Defines:
      golden_versions   record which tabtools/fvgen Stata resolves (asserts dev checkouts)
      golden_run        run one scenario: fixture -> setup -> call, log console, dump r()
      golden_dump_r()   Mata: flatten r() to name,kind,row,col,value CSV
      golden_effect_input  effecttab goldens: r(table) of the last collected
                        teffects/margins command to <id>_input.csv (task 7.11)
*/

version 17.0

capture program drop golden_ado_version
program define golden_ado_version, rclass
    * Return the version on the *! header line of an ado file and its path.
    args adoname
    quietly findfile `adoname'.ado
    local fn "`r(fn)'"
    tempname fh
    file open `fh' using "`fn'", read text
    file read `fh' line
    file close `fh'
    local ver ""
    if regexm(`"`line'"', "[Vv]ersion ([0-9]+\.[0-9]+\.[0-9]+)") local ver = regexs(1)
    return local version "`ver'"
    return local path "`fn'"
end

capture program drop golden_versions
program define golden_versions
    * Record the resolved package versions; refuse to run against anything
    * other than the dev checkouts put first on the adopath by run.do, or
    * (when expect_version is given) at any other tabtools version. All
    * checks run before VERSIONS.csv is opened, so a refused run leaves the
    * committed file untouched.
    args outfile expect_tabtools expect_fvgen expect_version
    local comps desctab regtab table1_tc _tabtools_common puttab stacktab _tabtools_markdown_write stratetab effecttab comptab hrcomptab fvgen
    foreach a of local comps {
        golden_ado_version `a'
        local v_`a' "`r(version)'"
        local p_`a' "`r(path)'"
        if "`a'" == "fvgen" {
            if strpos("`p_`a''", "`expect_fvgen'") != 1 {
                display as error "fvgen resolves to `p_`a'', expected under `expect_fvgen'"
                exit 459
            }
            global GOLDEN_FVGEN_VERSION "`v_`a''"
        }
        else {
            if strpos("`p_`a''", "`expect_tabtools'") != 1 {
                display as error "`a' resolves to `p_`a'', expected under `expect_tabtools'"
                exit 459
            }
            if "`expect_version'" != "" & "`v_`a''" != "`expect_version'" {
                display as error "`a' is version `v_`a'', expected `expect_version' (make_golden.R --tabtools-version=)"
                exit 459
            }
            if "`a'" == "desctab" global GOLDEN_TABTOOLS_VERSION "`v_`a''"
        }
    }
    tempname fh
    file open `fh' using "`outfile'", write text replace
    file write `fh' "component,version,path" _n
    foreach a of local comps {
        file write `fh' "`a',`v_`a'',`p_`a''" _n
    }
    file write `fh' "stata,`c(stata_version)',`c(edition_real)'" _n
    file write `fh' "rng,`c(rng_current)'," _n
    file write `fh' "linesize,`c(linesize)'," _n
    file close `fh'
end

capture program drop golden_run
program define golden_run
    * golden_run <id> <fixture>: expects programs _golden_setup and
    * _golden_call to be defined for this scenario.
    args id fixture
    capture log close golden
    clear
    capture collect clear
    estimates clear
    capture macro drop TABTOOLS_*
    capture erase "`id'.csv"
    capture erase "`id'.md"
    capture erase "`id'_stored.csv"
    capture erase "`id'_console.txt"
    capture erase "`id'_input.csv"
    use "fixtures/`fixture'.dta", clear
    capture noisily _golden_setup
    local rc = _rc
    if `rc' == 0 {
        log using "`id'_console.txt", text replace nomsg name(golden)
        capture noisily _golden_call
        local rc = _rc
        if `rc' == 0 mata: golden_dump_r("`id'_stored.csv", "`id'")
        capture log close golden
    }
    tempname fh
    file open `fh' using "_status.csv", write text append
    file write `fh' "`id',`rc'" _n
    file close `fh'
    capture collect clear
    capture stset, clear
end

capture program drop golden_effect_input
program define golden_effect_input
    * golden_effect_input <id> <model> <kind> [<subcmd>]: write r(table) of
    * the last collected teffects/margins command to <id>_input.csv (model 1
    * starts the file, later models append), one row per column of r(table):
    * model, kind, subcmd, level, equation, term, estimate, conf_low,
    * conf_high, p_value at %21.17g. The effecttab goldens' r_call reads it
    * (golden_effect_input() in helper-golden-effecttab.R), so the R layout
    * sees Stata's own numbers (IMPLEMENTATION_PLAN.md task 7.11).
    args id model kind subcmd
    tempname T
    matrix `T' = r(table)
    local lev = r(level)
    if "`lev'" == "" | "`lev'" == "." local lev = c(level)
    mata: golden_effect_rows("`T'", "`id'_input.csv", `model', "`kind'", "`subcmd'", `lev')
end

mata:
mata set matastrict on


string scalar golden_csvq(string scalar s)
{
    if (strpos(s, `"""') | strpos(s, ",") | strpos(s, char(10)) | strpos(s, char(13))) {
        return(`"""' + subinstr(s, `"""', `""""') + `"""')
    }
    return(s)
}

string scalar golden_num(real scalar x)
{
    if (missing(x)) return(strofreal(x))
    return(strtrim(strofreal(x, "%21.17g")))
}

string scalar golden_stripe(string rowvector s)
{
    if (s[1] != "") return(s[1] + ":" + s[2])
    return(s[2])
}

void golden_dump_r(string scalar path, string scalar id)
{
    string colvector sc, ss, mc, mx
    string matrix rs, cs
    real matrix M
    real scalar fh, i, r, c

    // Snapshot names first; nothing below touches r().
    sc = st_dir("r()", "numscalar", "*")
    ss = st_dir("r()", "strscalar", "*")
    mc = st_dir("r()", "macro", "*")
    mx = st_dir("r()", "matrix", "*")

    if (fileexists(path)) unlink(path)
    fh = fopen(path, "w")
    fput(fh, "name,kind,row,col,value")
    fput(fh, "_golden_id,meta,,," + golden_csvq(id))
    fput(fh, "_golden_tabtools_version,meta,,," + st_global("GOLDEN_TABTOOLS_VERSION"))
    fput(fh, "_golden_fvgen_version,meta,,," + st_global("GOLDEN_FVGEN_VERSION"))
    fput(fh, "_golden_stata_version,meta,,," + golden_num(c("stata_version")))
    for (i = 1; i <= rows(sc); i++) {
        fput(fh, golden_csvq(sc[i]) + ",scalar,,," + golden_num(st_numscalar("r(" + sc[i] + ")")))
    }
    for (i = 1; i <= rows(ss); i++) {
        fput(fh, golden_csvq(ss[i]) + ",strscalar,,," + golden_csvq(st_strscalar("r(" + ss[i] + ")")))
    }
    for (i = 1; i <= rows(mc); i++) {
        fput(fh, golden_csvq(mc[i]) + ",macro,,," + golden_csvq(st_global("r(" + mc[i] + ")")))
    }
    for (i = 1; i <= rows(mx); i++) {
        M = st_matrix("r(" + mx[i] + ")")
        rs = st_matrixrowstripe("r(" + mx[i] + ")")
        cs = st_matrixcolstripe("r(" + mx[i] + ")")
        for (r = 1; r <= rows(M); r++) {
            for (c = 1; c <= cols(M); c++) {
                fput(fh, golden_csvq(mx[i]) + ",matrix," + golden_csvq(golden_stripe(rs[r, .])) + "," +
                    golden_csvq(golden_stripe(cs[c, .])) + "," + golden_num(M[r, c]))
            }
        }
    }
    fclose(fh)
}

void golden_effect_rows(string scalar tname, string scalar path, real scalar model,
                        string scalar kind, string scalar subcmd, real scalar lev)
{
    real matrix T
    string matrix cs, rs
    real scalar fh, j, rb, rll, rul, rp
    string scalar p

    T = st_matrix(tname)
    cs = st_matrixcolstripe(tname)
    rs = st_matrixrowstripe(tname)
    rb = selectindex(rs[., 2] :== "b")
    rll = selectindex(rs[., 2] :== "ll")
    rul = selectindex(rs[., 2] :== "ul")
    rp = selectindex(rs[., 2] :== "pvalue")
    if (model == 1 & fileexists(path)) unlink(path)
    fh = fopen(path, "a")
    if (model == 1) fput(fh, "model,kind,subcmd,level,equation,term,estimate,conf_low,conf_high,p_value")
    for (j = 1; j <= cols(T); j++) {
        p = strofreal(model) + "," + kind + "," + subcmd + "," + golden_num(lev) + "," +
            golden_csvq(cs[j, 1]) + "," + golden_csvq(cs[j, 2]) + "," +
            golden_num(T[rb, j]) + "," + golden_num(T[rll, j]) + "," +
            golden_num(T[rul, j]) + "," + golden_num(T[rp, j])
        fput(fh, p)
    }
    fclose(fh)
}

end
