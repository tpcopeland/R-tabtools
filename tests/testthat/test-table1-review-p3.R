# Phase 2 review fixes carried into the Phase 3 paths (wt(), fweight,
# wtcompare, smallcells), against Stata-written expectations from
# qa/stata/make_table1_review_p3.do (tests/testthat/fixtures/table1_review/
# RW01-RW12). rv_expect() and the fixture readers are in
# test-table1-review.R's helpers, repeated here so the file stands alone.

rw_dir <- function() test_path("fixtures", "table1_review")
rw_data <- function(name) as.data.frame(haven::read_dta(file.path(rw_dir(), paste0(name, ".dta"))))
rw_agg <- function() as.data.frame(haven::read_dta(test_path("fixtures", "table1_qa", "agg.dta")))

rw_expect <- function(tt, id, mask = "p,test,statistic") {
  want <- golden_read_cells_file(file.path(rw_dir(), paste0(id, ".csv")))
  mm <- golden_compare_cells(tt, want, mask)
  expect_identical(nrow(mm), 0L, label = paste(id, "cells", paste(utils::capture.output(print(mm)), collapse = "\n")))
  why <- golden_compare_console(utils::capture.output(print(tt)),
                                golden_read_lines(file.path(rw_dir(), paste0(id, "_console.txt"))), mask)
  expect_identical(why, character(), label = paste(id, "console"))
  stored <- utils::read.csv(file.path(rw_dir(), paste0(id, "_stored.csv")), colClasses = "character",
                            na.strings = character(), encoding = "UTF-8")
  fields <- unique(stored$name[stored$kind != "meta"])
  expect_true(all(c("Dapa", "varlist") %in% fields), label = paste(id, "stored fixture holds r()"))
  why <- golden_compare_stored(tt$stored, stored, fields = fields, mask = "p", p_table = golden_table_p_rows(tt))
  expect_identical(why, character(), label = paste(id, "stored"))
}

rw_ties <- function() {
  pat <- rw_data("tiesw")
  d <- pat[rep(seq_len(12L), 2500L), ]
  rownames(d) <- NULL
  d
}

test_that("weighted sums are Mata sums (double, data order): wt() display ties", {
  skip_if_not_installed("haven")
  tt <- table1_tc(rw_ties(), vars = "a contn %5.1f \\ c contn %5.1f \\ d contn %5.1f %5.3f \\ e contn %5.2f",
                  by = "g", total = "after", wt = "w")
  rw_expect(tt, "RW01")
  # The Total column's aweighted SD of a constant 2.675 is Stata's ".".
  expect_identical(tt$body[[4]][5], "2.68±.")
})

test_that("weighted sums are Mata sums (double, data order): fweight display ties", {
  skip_if_not_installed("haven")
  rw_expect(table1_tc(rw_ties(), vars = "a contn %5.1f \\ c contn %5.1f \\ d contn %5.1f %5.3f \\ e contn %5.2f",
                      by = "g", total = "after", nopvalue = TRUE, fweight = "fw"), "RW02")
})

test_that("an empty string is missing under wt() and fweight", {
  skip_if_not_installed("haven")
  b <- rw_data("blankw")
  rw_expect(table1_tc(b, vars = "x contn \\ s cat", by = "g", wt = "w", wtn = TRUE, missing = TRUE,
                      missingsummary = TRUE), "RW03")
  rw_expect(table1_tc(b, vars = "x contn \\ s cat", by = "g", missingsummary = TRUE, fweight = "fw"), "RW04")
})

test_that("percsign and empty delimiters under wt(), wtn, wtcompare, and smallcells", {
  skip_if_not_installed("haven")
  rw_expect(table1_tc(rw_agg(), by = "trt", vars = "stage cat \\ female bin \\ age contn", wt = "w", wtn = TRUE,
                      percsign = " %", headerperc = TRUE, missingsummary = TRUE), "RW05")
  tt <- table1_tc(rw_agg(), by = "trt",
                  vars = "stage cat \\ female bin \\ age contn \\ marker conts \\ marker contln", wt = "w",
                  wtcompare = TRUE, smd = TRUE, percsign = " %", iqrmiddle = "", sdleft = "", gsdleft = "",
                  gsdright = "", varlabplus = TRUE)
  rw_expect(tt, "RW06")
  # RW07's cells and suppression map come from tabtools 2.5.1 (Stata-Tools
  # 712044f8, derivable-count protection); the rest stays 2.1.14 (_stored.csv
  # meta row `_cells_from`).
  rw_expect(table1_tc(rw_agg(), by = "trt", vars = "stage cat \\ female bin", percsign = " %", headerperc = TRUE,
                      missingsummary = TRUE, smallcells = 3, fweight = "fw"), "RW07")
})

test_that("console rules follow label text under weights (ESS row, wtcompare)", {
  skip_if_not_installed("haven")
  sep <- rw_data("sepw")
  tt <- table1_tc(sep, by = "g", vars = "x1 contn \\ x2 contn \\ c2 cat \\ c3 cat", wt = "w", wtn = TRUE,
                  missingsummary = TRUE)
  rw_expect(tt, "RW08")
  expect_identical(tt$rows$type[1], "ess")
  expect_true(is.na(tt$rows$table_row[1]))
  rw_expect(table1_tc(sep, by = "g", vars = "x1 contn \\ x2 contn \\ c1 cat \\ b1 bin", wt = "w", wtcompare = TRUE,
                      smd = TRUE), "RW09")
})

test_that("string by() codes count levels on records the weights drop", {
  skip_if_not_installed("haven")
  s <- rw_data("strby")
  tt <- table1_tc(s, vars = "x contn \\ c bin", by = "g", wt = "w", wtn = TRUE, smallcells = 3)
  rw_expect(tt, "RW10")
  expect_identical(colnames(tt$stored$suppression), c("g_2", "g_3"))
  # Stata 2.1.17+: RW11 stops with r(498) (shared group N cannot be withheld) and
  # RW12 with the percent-only refusal (wtcompare without wtn/percent_n).
  expect_error(table1_tc(s, vars = "x contn \\ c bin", by = "g", smallcells = 3, nopvalue = TRUE, fweight = "fw"),
               class = "tabtools_error_smallcells_shared_margin")
  expect_error(table1_tc(s, vars = "x contn \\ c bin", by = "g", wt = "w", wtcompare = TRUE, smallcells = 3),
               class = "tabtools_error_smallcells_percent_only")
  # A factor by() keeps R's own numbering of its levels (no Stata analogue).
  f <- s
  f$g <- factor(f$g)
  expect_identical(colnames(table1_tc(f, vars = "x contn", by = "g", wt = "w", wtn = TRUE,
                                      smallcells = 3)$stored$suppression), c("g_2", "g_3"))
})

test_that("weighted tables still suppress p-values; fweight p-values use the row-wise mask", {
  skip_if_not_installed("haven")
  tt <- table1_tc(rw_agg(), by = "trt", vars = "stage cat \\ female bin \\ age contn", wt = "w")
  expect_false(any(c("p", "test", "statistic") %in% tt$cols$role))
  expect_null(tt$stored$methods)
  tt <- table1_tc(rw_agg(), by = "trt", vars = "stage cat \\ female bin \\ age contn", fweight = "fw")
  expect_identical(golden_p_masked_rows(tt)[tt$rows$type %in% c("var", "cat_header")],
                   c(FALSE, FALSE, TRUE))
})
