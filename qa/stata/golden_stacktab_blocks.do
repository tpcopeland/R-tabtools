/*  golden_stacktab_blocks.do - source blocks for the stacktab goldens (K*)

    Run from a scenario's stata_setup (the working directory is
    tests/testthat/golden):
        run "../../../qa/stata/golden_stacktab_blocks.do"
        golden_hrt_blocks "K01.xlsx"
    Defines golden_hrt_blocks, which writes the two puttab blocks of the
    demo pipeline (demo/demo_tabtools.do:1514-1538, sheets "Block Primary"
    and "Block Dose") into the given workbook, then leaves the data empty.
    The R side builds the same blocks with golden_hrt_blocks() in
    tests/testthat/helper-golden-export.R.
*/

version 17.0

capture program drop golden_hrt_blocks
program define golden_hrt_blocks
    version 17.0
    args book
    clear
    quietly set obs 3
    quietly generate str22 term = ""
    quietly generate str10 ahr = ""
    quietly generate str16 ci = ""
    quietly replace term = "Any HRT" in 1
    quietly replace term = "Former smoker" in 2
    quietly replace term = "Current smoker" in 3
    quietly replace ahr = "0.82" in 1
    quietly replace ahr = "1.14" in 2
    quietly replace ahr = "1.46" in 3
    quietly replace ci = "(0.69, 0.98)" in 1
    quietly replace ci = "(0.97, 1.34)" in 2
    quietly replace ci = "(1.21, 1.77)" in 3
    label variable term "Exposure"
    label variable ahr "aHR"
    label variable ci "95% CI"
    quietly puttab term ahr ci using "`book'", sheet("Block Primary") ///
        title("Source block: Primary HRT exposure model") varlabels

    clear
    quietly set obs 2
    quietly generate str22 term = ""
    quietly generate str10 ahr = ""
    quietly generate str16 ci = ""
    quietly replace term = "Low dose" in 1
    quietly replace term = "High dose" in 2
    quietly replace ahr = "0.91" in 1
    quietly replace ahr = "0.73" in 2
    quietly replace ci = "(0.74, 1.12)" in 1
    quietly replace ci = "(0.58, 0.92)" in 2
    label variable term "Exposure"
    label variable ahr "aHR"
    label variable ci "95% CI"
    quietly puttab term ahr ci using "`book'", sheet("Block Dose") ///
        title("Source block: Estrogen dose-response model") varlabels
    clear
end
