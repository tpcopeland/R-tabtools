/*  run_demo.do - run the Stata tabtools demo from a pinned export

    Produces the Stata side of the R demo parity check (IMPLEMENTATION_PLAN.md,
    Milestone D, task D3): runs tabtools/demo/demo_tabtools.do exactly as
    shipped in a `git archive` export of Stata-Tools, then copies its
    artefacts (13 workbooks, console_output.log/.md, demo_markdown_report.md)
    to an output directory.

    Never run it on the ~/Stata-Tools working tree or an installed PLUS copy:
    only on an export of a released commit, e.g.

        git -C ~/Stata-Tools archive 1255176d tabtools _data tc_schemes logdoc \
            | tar -x -C <export>

    Usage (qa/demo_parity.R --update calls it this way):

        stata-mp -b do qa/stata/run_demo.do "<export>" "<outdir>" 2.1.14

    The demo itself net-installs tabtools, tc_schemes and logdoc from the
    export into a temporary PLUS directory; the export's tabtools is also put
    first on the adopath here, and the run stops unless it reports the
    expected version.
*/

version 17.0
args export outdir want_version
if `"`export'"' == "" | `"`outdir'"' == "" | "`want_version'" == "" {
    display as error "usage: do run_demo.do <export> <outdir> <tabtools version>"
    exit 198
}
clear all
set more off
set rng mt64

capture confirm file "`export'/tabtools/tabtools.pkg"
if _rc {
    display as error "not a Stata-Tools export: `export'"
    exit 601
}
foreach f in _data/cohort.dta tc_schemes/stata.toc logdoc/stata.toc {
    capture confirm file "`export'/`f'"
    if _rc {
        display as error "the export lacks `f' (git archive <commit> tabtools _data tc_schemes logdoc)"
        exit 601
    }
}

* The export's tabtools first on the adopath; refuse any other version.
adopath ++ "`export'/tabtools"
discard
quietly tabtools
if "`r(version)'" != "`want_version'" {
    display as error "tabtools `r(version)' found in the export, `want_version' expected"
    exit 9
}
findfile table1_tc.ado
if strpos("`r(fn)'", "`export'") != 1 {
    display as error "table1_tc.ado resolves outside the export: `r(fn)'"
    exit 9
}

* Start from an empty demo directory, so every artefact comes from this run
* (git archive also exports the committed demo outputs).
local demo "`export'/tabtools/demo"
local arts demo_table1 demo_desctab demo_regtab demo_regtab_models ///
    demo_comptab demo_effecttab demo_stratetab demo_corrtab demo_crosstab ///
    demo_survtab demo_hrcomptab demo_puttab demo_stacktab
foreach f of local arts {
    capture erase "`demo'/`f'.xlsx"
}
foreach f in console_output.log console_output.md demo_markdown_report.md {
    capture erase "`demo'/`f'"
}

* Run the demo from the export root, as its usage note says.
cd "`export'"
do tabtools/demo/demo_tabtools.do

* Copy the artefacts out.
capture mkdir "`outdir'"
foreach f of local arts {
    copy "`demo'/`f'.xlsx" "`outdir'/`f'.xlsx", replace
}
foreach f in console_output.log console_output.md demo_markdown_report.md {
    copy "`demo'/`f'" "`outdir'/`f'", replace
}
display as result "run_demo.do: tabtools `want_version' demo artefacts in `outdir'"
