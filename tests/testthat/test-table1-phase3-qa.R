# table1_tc weighting and small-cell wiring against Stata (plan tasks
# 3.1-3.6), beyond the golden scenarios: option combinations, the pipeline
# cases of qa/test_smallcells.do, and edge cases. Expectations are Stata
# 17's own csv() sink, listing, and r() results written by
# qa/stata/make_table1_phase3.do; test/statistic/p-value cells are masked
# (R's tests differ by design, plan D3).

p3_dir <- function() test_path("fixtures", "table1_phase3")
p3_data <- function(name) sc_pipeline_data(name)
p3_agg <- function() as.data.frame(haven::read_dta(test_path("fixtures", "table1_qa", "agg.dta")))

# Cells, listing, and stored results (Dapa, varlist, r(table) SMDs, the
# suppression matrix and counts) of one call against Stata's. The p token is
# the row-wise p mask (golden_p_masked_rows()). W1 and F1 list `marker` twice
# in a row; Stata's sepby(factor_sep) draws no rule between the two (same
# label), which the label-text blocks now reproduce.
p3_expect <- function(tt, id, mask = "p,test,statistic", console = TRUE) {
  tt <- golden_strip_smd_note(tt)
  expect_identical(golden_check_derived(tt), character(), label = paste(id, "derived suppression cells"))
  want <- golden_read_cells_file(file.path(p3_dir(), paste0(id, ".csv")))
  mm <- golden_compare_cells(tt, want, mask)
  expect_identical(nrow(mm), 0L, label = paste(id, "cells", paste(utils::capture.output(print(mm)), collapse = "\n")))
  if (console) {
    why <- golden_compare_console(utils::capture.output(print(tt)),
                                  golden_read_lines(file.path(p3_dir(), paste0(id, "_console.txt"))), mask)
    expect_identical(why, character(), label = paste(id, "console"))
  }
  stored <- utils::read.csv(file.path(p3_dir(), paste0(id, "_stored.csv")), colClasses = "character",
                            na.strings = character(), encoding = "UTF-8")
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]), c("xlsx", "sheet", "csv"))
  expect_true(all(c("Dapa", "varlist") %in% fields), label = paste(id, "stored fixture holds r()"))
  why <- golden_compare_stored(tt$stored, stored, fields = fields, mask = "p", p_table = golden_table_p_rows(tt))
  expect_identical(why, character(), label = paste(id, "stored"))
}

test_that("wt(): every column type, missing, missingsummary, total(before), headerperc, wtn, smd", {
  skip_if_not_installed("haven")
  tt <- table1_tc(p3_agg(), by = "trt",
                  vars = "age contn %6.2f \\ marker contln %6.2f \\ marker conts %6.1f \\ female bin \\ stage cat",
                  wt = "w", wtn = TRUE, smd = TRUE, missing = TRUE, missingsummary = TRUE,
                  total = "before", headerperc = TRUE)
  p3_expect(tt, "W1")
  expect_identical(tt$rows$type[1], "ess")
  # r(table): one row per analysed variable (tabtools 2.1.10+), never the
  # ESS, category-level or missing-summary rows.
  expect_identical(nrow(tt$stored$table), 5L)
})

test_that("wt(): percent_n, catrowperc, slashN, total(after), varlabplus", {
  skip_if_not_installed("haven")
  p3_expect(table1_tc(p3_agg(), by = "trt", vars = "age contn \\ female bin \\ stage cat", wt = "w",
                      percent_n = TRUE, catrowperc = TRUE, slashN = TRUE, total = "after",
                      varlabplus = TRUE), "W2")
})

test_that("wtcompare with total(after), missingsummary, headerperc, smd; workbook styling", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  xlsx <- file.path(out, "w3.xlsx")
  tt <- table1_tc(p3_agg(), by = "trt", vars = "age contn \\ marker conts \\ female bin \\ stage cat",
                  wt = "w", wtcompare = TRUE, smd = TRUE, total = "after", missingsummary = TRUE,
                  headerperc = TRUE, xlsx = xlsx, sheet = "W3")
  p3_expect(tt, "W3")
  why <- golden_compare_styles(xlsx, "W3", file.path(p3_dir(), "phase3.xlsx"), "W3",
                               got_width_offset = golden_r_width_offset)
  expect_identical(why, character())
  # Total columns inside wtcompare blocks are ordinary data columns (no
  # Total borders, desctab.ado:1786-1797).
  expect_false(any(tt$cols$role == "total"))
  expect_identical(tt$cols$pass[2:7], rep(c("crude", "weighted"), each = 3))
})

test_that("wtcompare with total(before), wtn, varlabplus", {
  skip_if_not_installed("haven")
  p3_expect(table1_tc(p3_agg(), by = "trt", vars = "age contn \\ female bin \\ stage cat", wt = "w",
                      wtcompare = TRUE, total = "before", wtn = TRUE, varlabplus = TRUE), "W4")
})

test_that("wt(): weighted quantiles, contln, and large values on the demo cohort", {
  skip_on_cran()
  p3_expect(table1_tc(golden_fixture("cohort"), by = "treated",
                      vars = "index_age conts \\ lab_value contln %5.2f \\ bmi contn %5.1f \\ cost_sek conts",
                      wt = "iptw", smd = TRUE, total = "after"), "W5")
})

test_that("wt(): three groups, wtn, smd (first-two-groups note)", {
  skip_on_cran()
  p3_expect(table1_tc(golden_fixture("cohort"), by = "education",
                      vars = "index_age contn %5.1f \\ female bin \\ income_quintile cat \\ bmi conts %5.1f",
                      wt = "iptw", wtn = TRUE, smd = TRUE), "W6")
})

test_that("fweight: every column type, catrowperc slashN, missingsummary, total(after), smd", {
  skip_if_not_installed("haven")
  p3_expect(table1_tc(p3_agg(), by = "trt", fweight = "fw",
                      vars = "age contn %6.2f \\ marker contln %6.2f \\ marker conts %6.1f \\ female bin \\ stage cat",
                      smd = TRUE, total = "after", missingsummary = TRUE, catrowperc = TRUE, slashN = TRUE,
                      test = TRUE, statistic = TRUE), "F1")
})

test_that("fweight: zero weights leave the sample before auto-typing", {
  skip_on_cran()
  p3_expect(table1_tc(golden_fixture("cohort"), vars = c("index_age", "bmi", "lab_value", "education"),
                      by = "treated", fweight = "fw", smd = TRUE), "F2")
})

test_that("smallcells: 2x2 total(after) with test/statistic/smd, title, workbook styling", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  out <- withr::local_tempdir()
  xlsx <- file.path(out, "s1.xlsx")
  tt <- table1_tc(p3_data("sc2x2"), by = "group", vars = "category cat", total = "after", test = TRUE,
                  statistic = TRUE, smd = TRUE, smallcells = 5, title = "Synthetic small cells",
                  xlsx = xlsx, sheet = "S1")
  p3_expect(tt, "S1")
  why <- golden_compare_styles(xlsx, "S1", file.path(p3_dir(), "phase3.xlsx"), "S1",
                               mask = "p,test,statistic,pstyle", got_width_offset = golden_r_width_offset)
  expect_identical(why, character())
})

test_that("smallcells: redundant complementary margins are absent (5 primary, 1 secondary)", {
  skip_if_not_installed("haven")
  tt <- table1_tc(p3_data("scirr"), by = "group", vars = "category cat", total = "after", smallcells = 5)
  p3_expect(tt, "S2")
  expect_identical(c(tt$stored$N_primary_suppressed, tt$stored$N_secondary_suppressed), c(5L, 1L))
  # The one complement is the grand total on the N row.
  expect_identical(sum(grepl("\u22655", as.matrix(as.data.frame(tt)), fixed = TRUE)), 1L)
})

test_that("smallcells pipeline cases of test_smallcells.do match Stata cell for cell", {
  skip_if_not_installed("haven")
  p3_expect(table1_tc(p3_data("sccont"), by = "group", vars = "value contn \\ category cat",
                      missingsummary = TRUE, test = TRUE, statistic = TRUE, smd = TRUE, smallcells = 5), "S3")
  p3_expect(table1_tc(p3_data("scwt"), by = "group", vars = "category cat", wt = "wt", wtn = TRUE,
                      smallcells = 5), "S4")
  p3_expect(table1_tc(p3_data("scfw"), by = "group", vars = "category cat", fweight = "fw",
                      total = "after", smallcells = 5), "S5")
  tt <- table1_tc(p3_data("sccomp"), by = "group", vars = "category cat", wt = "wt", wtcompare = TRUE,
                  wtn = TRUE, total = "after", slashN = TRUE, headerperc = TRUE, missingsummary = TRUE,
                  smallcells = 5)
  p3_expect(tt, "S6")
  # Columns interleave crude and weighted per group (desctab.ado:994-1007).
  expect_identical(colnames(tt$stored$suppression),
                   c("Cr_0", "Wt_0", "Cr_1", "Wt_1", "Cr_T", "Wt_T"))
  p3_expect(table1_tc(p3_data("scmiss"), by = "group", vars = "category cat", missing = TRUE,
                      total = "before", catrowperc = TRUE, smallcells = 3), "S7")
  p3_expect(table1_tc(p3_data("scallmiss"), by = "group", vars = "allmissing contn", missingsummary = TRUE,
                      percent_n = TRUE, smallcells = 3), "S8")
  p3_expect(table1_tc(p3_data("schighk"), by = "group", vars = "category cat", smallcells = 10), "S9")
})

test_that("a coded N row: headerperc reads missing and prints '(.)', with and without a total", {
  a <- golden_fixture("auto")
  p3_expect(table1_tc(a, by = "rep78", vars = "price contn \\ foreign bin", headerperc = TRUE,
                      smallcells = 5), "S10")
  # Stata 2.1.17+ refuses this table (S11 stopped with r(498)): a count below 5
  # can only be protected by withholding the group or total N, which the other
  # variable releases (qa/stata/make_table1_phase3.do, tabtools 2.5.1).
  expect_error(table1_tc(a, by = "rep78", vars = "price contn \\ foreign bin", headerperc = TRUE,
                         total = "after", smallcells = 5),
               class = "tabtools_error_smallcells_shared_margin")
})

test_that("smallcells without by(), with conts", {
  p3_expect(table1_tc(golden_fixture("auto"), vars = "rep78 cat \\ foreign bin \\ mpg conts",
                      smallcells = 5), "S12")
})

test_that("smallcells with catrowperc slashN: released row margins code the denominators", {
  p3_expect(table1_tc(golden_fixture("auto"), by = "foreign", vars = "rep78 cat \\ price contn",
                      catrowperc = TRUE, slashN = TRUE, total = "after", missingsummary = TRUE,
                      smallcells = 5), "S13")
})

test_that("smallcells with extraspace and a user footnote", {
  skip_if_not_installed("haven")
  tt <- table1_tc(p3_data("sc2x2"), by = "group", vars = "category cat", extraspace = TRUE, test = TRUE,
                  smd = TRUE, smallcells = 5, footnote = "Source: synthetic.")
  p3_expect(tt, "S14")
  expect_identical(tt$footnote, paste("Source: synthetic.", tabtools:::tt_sc_footnote(5)))
})

test_that("smallcells binary: slashN denominators, missingsummary, total(before), percent_n", {
  skip_if_not_installed("haven")
  d <- p3_data("scbin")
  p3_expect(table1_tc(d, by = "group", vars = "flag bin", slashN = TRUE, missingsummary = TRUE,
                      total = "before", smallcells = 5), "S15")
  p3_expect(table1_tc(d, by = "group", vars = "flag bin", slashN = TRUE, percent_n = TRUE,
                      smallcells = 5), "S16")
})

p3_review_data <- function(name) as.data.frame(haven::read_dta(file.path(p3_dir(), paste0(name, ".dta"))))

test_that("wtcompare + smallcells with a coded group N: crude and weighted codes in order (review P1-2)", {
  skip_if_not_installed("haven")
  # Review probe K7: a 3-record group codes its N in both passes; the ESS row
  # is Suppressed in the weighted block only (_desctab_collect.ado:891-910),
  # so its suppression codes are the one place crude and weighted differ
  # (desctab.ado:994-1007 interleaves Cr_/Wt_ per group).
  tt <- table1_tc(p3_review_data("sg3"), by = "arm", vars = "x contn \\ k cat \\ b bin", wt = "w",
                  wtcompare = TRUE, wtn = TRUE, smallcells = 4, catrowperc = TRUE, slashN = TRUE, smd = TRUE)
  # S17's cells, console cells and suppression map come from tabtools 2.5.1
  # (Stata-Tools 712044f8: derivable-count protection); its SMD header and
  # note stay in 2.1.14's form (_stored.csv meta row `_cells_from`).
  p3_expect(tt, "S17")
  m <- tt$stored$suppression
  expect_identical(unname(m["r3", c("Cr_3", "Wt_3")]), c(0, 3))
  expect_identical(unname(m["r2", c("Cr_3", "Wt_3")]), c(1, 1))
  expect_identical(tt$body[1, which(tt$cols$pass == "crude")[3]], "")
  expect_identical(tt$body[1, which(tt$cols$pass == "weighted")[3]], "Suppressed")
  # The SMD column lists at least 7 wide (`smd_str`) through
  # .t1_console_widths(), without a width set in the wtcompare merge (P3-1).
  expect_identical(tt$cols$console_width[tt$cols$role == "smd"], 7L)
})

test_that("fweight, three groups, conts: displayed p exact, stored Kruskal-Wallis p at 1e-4 (review P2-4)", {
  skip_if_not_installed("haven")
  tt <- table1_tc(p3_review_data("pw"), by = "g3", vars = "x contn \\ y conts \\ y contln \\ b bin \\ c cat",
                  fweight = "fw", missing = TRUE, smd = TRUE, total = "before", headerperc = TRUE,
                  percent_n = TRUE)
  # p3_expect compares the Kruskal-Wallis p cell exactly (row-wise p mask).
  p3_expect(tt, "S18")
  stored <- utils::read.csv(file.path(p3_dir(), "S18_stored.csv"), colClasses = "character",
                            na.strings = character(), encoding = "UTF-8")
  pt <- golden_table_p_rows(tt)
  kw <- which(!is.na(attr(pt, "tol")))
  expect_identical(rownames(tt$stored$table)[kw], "Biomarker")
  expect_false(pt[kw])
  # Stata's kwallis sums ranks in float (kwallis.ado:21, :25, :40): its p is
  # 4.4228421e-4 against kruskal.test()'s 4.4228868e-4, so the default
  # 1e-9 fails and only that p_value gets the relative tolerance.
  expect_equal(unname(tt$stored$table[kw, "p_value"]), 4.4228868e-4, tolerance = 1e-7)
  why <- golden_compare_stored(tt$stored, stored, fields = "table", mask = "p", p_table = as.vector(pt))
  expect_identical(length(why), 1L)
  expect_match(why, "^table\\[Biomarker,p_value\\]")
  expect_length(golden_compare_stored(tt$stored, stored, fields = "table", mask = "p", p_table = pt), 0L)
  # The tolerance is that row's only: a 1e-3 drift there fails, and every
  # other p (here the chi-squared p of Region, 0.066) keeps 1e-9.
  bump <- function(row, f) {
    st <- tt$stored
    i <- which(rownames(st$table) == row)[1]
    st$table[i, "p_value"] <- st$table[i, "p_value"] * f
    golden_compare_stored(st, stored, fields = "table", mask = "p", p_table = pt)
  }
  expect_length(bump("Biomarker", 1 + 1e-3), 1L)
  expect_length(bump("Region", 1 + 1e-6), 1L)
})
