* Milestone H task H12: the bytes of Stata's Markdown writer for cells with
* HTML, entities, link syntax, the escaped characters and a backtick.
* Run on a `git archive` export of Stata-Tools f42ee9cd (tabtools 2.1.12; first
* written for 33528c76, 2.1.11), with its tabtools directory first on the
* adopath (see probe_h10_overflow.do). Result (2026-09-26, Stata 17): md1.md
* is pinned in tests/testthat/test-writers-hardening.R (2.1.12 escapes
* backslash | * _ backtick < > & [ ] ~ and writes the backtick row; 2.1.11
* escaped only the first four and stopped with r(132) there).
set more off
* adopath ++ "<dir>/tabtools"
clear
set obs 7
gen str40 lab = ""
gen str40 val = ""
replace lab = "<b>literal</b>" in 1
replace val = "&lt;5" in 1
replace lab = "[a](b)" in 2
replace val = "*x*" in 2
replace lab = "_x_" in 3
replace val = "p|q" in 3
replace lab = "c:\dir" in 4
replace val = "a\b" in 4
replace lab = "  indented <i>" in 5
replace val = " spaced " in 5
replace lab = "tick" in 6
replace val = char(96) + "code" + char(96) in 6
replace lab = "x" in 7
replace val = "y" in 7
capture noisily puttab lab val, markdown("md1.md")
display "rc md1 = " _rc
* the same without the backtick row
drop in 6
capture noisily puttab lab val, markdown("md2.md")
display "rc md2 = " _rc
