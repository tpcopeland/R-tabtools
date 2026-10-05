# Ported table1_tc QA contracts (plan task 2.12): qa/test_table1_tc.do
# ("legacy suite", "nopvalue option suite", "fast-collect aggregation
# contracts") and qa/validation_table1_tc.do (VC1, KE-1, KE7, VA7), minus
# the Stata test p-values. Expected cells are Stata 17's own csv() output
# (qa/stata/make_table1_qa.do); test/statistic/p-value columns are masked.

qa_fixture <- function(name) as.data.frame(haven::read_dta(test_path("fixtures", "table1_qa", paste0(name, ".dta"))))
qa_expect <- function(tt, id) {
  want <- golden_read_cells_file(test_path("fixtures", "table1_qa", paste0(id, ".csv")))
  # Row-wise p mask: cat/bin/Fisher/Kruskal p cells compared exactly.
  mm <- golden_compare_cells(tt, want, "p,test,statistic")
  expect_identical(nrow(mm), 0L, label = paste(id, paste(utils::capture.output(print(mm)), collapse = "\n")))
}

test_that("aggregation contract: every column type, missing, total(after), formats", {
  skip_if_not_installed("haven")
  agg <- qa_fixture("agg")
  tt <- table1_tc(agg, by = "trt", vars = "age contn %6.2f \\ marker conts %6.1f \\ female bin \\ stage cat",
                  smd = TRUE, test = TRUE, statistic = TRUE, missing = TRUE, total = "after",
                  nformat = "%9.0f", percformat = "%5.1f", percsign = "%")
  qa_expect(tt, "A")
  # Total sits after Test/Statistic, before p-value (Stata column order).
  expect_identical(tt$cols$role, c("label", "group", "group", "test", "statistic", "total", "p", "smd"))
  tab <- tt$stored$table
  expect_identical(colnames(tab), c("p_value", "smd"))
  # One row per analysed variable (tabtools 2.1.10+: no level rows).
  expect_identical(rownames(tab), c("Age_at_entry", "Inflammation_marker", "Female_sex", "Clinical_stage"))
  expect_equal(unname(tab[, "smd"]), c(2.9479271, 1.6959541, 0.13867505, 0.53452248), tolerance = 1e-7)
  expect_true(all(!is.na(tab[, "p_value"])))
})

test_that("percent modes, total placement, and header percentages", {
  skip_if_not_installed("haven")
  agg <- qa_fixture("agg")
  qa_expect(table1_tc(agg, by = "trt", vars = "stage cat", total = "before", headerperc = TRUE,
                      percent = TRUE, nformat = "%9.0f", percformat = "%5.1f", percsign = "%"), "B")
  qa_expect(table1_tc(agg, by = "trt", vars = "stage cat", catrowperc = TRUE, percent_n = TRUE,
                      slashN = TRUE, total = "after", missing = TRUE, nformat = "%9.0f",
                      percformat = "%5.1f", percsign = "%"), "C")
})

test_that("headerperc uses true group totals; empty binary groups show 0 ()", {
  skip_if_not_installed("haven")
  qa_expect(table1_tc(qa_fixture("hp"), by = "g", vars = "y contn \\ z bin", headerperc = TRUE), "D")
})

test_that("two observations: one-row table, SD shown as '.'", {
  skip_if_not_installed("haven")
  tt <- table1_tc(qa_fixture("two"), by = "g", vars = "y contn")
  qa_expect(tt, "E")
  expect_identical(dim(tt$stored$table), c(1L, 1L))
  expect_identical(tt$stored$varlist, "y")
})

test_that("an all-missing variable keeps its row and does not disturb the next", {
  a <- golden_fixture("auto")
  a$miss_var <- NA_real_
  attr(a$miss_var, "label") <- "All Missing"
  tt <- table1_tc(a, by = "foreign", vars = "miss_var contn \\ price contn")
  qa_expect(tt, "F")
  expect_true(is.na(tt$stored$table[1, 1]))
  expect_false(is.na(tt$stored$table[2, 1]))
  expect_identical(tt$stored$varlist, "miss_var price")
})

test_that("quick-start auto-typing contract: descriptor, Dapa, methods", {
  tt <- table1_tc(golden_fixture("auto"), by = "foreign", vars = c("rep78", "foreign"))
  qa_expect(tt, "G")
  expect_identical(tt$header[[2]]$text[1], "No. (Column %)")
  expect_identical(tt$stored$Dapa, "Data are presented as No. (%).")
  expect_true(grepl(tt$stored$Dapa, tt$stored$methods, fixed = TRUE))
  expect_identical(tt$stored$types, "cat bin")
})

test_that("total() with an unlabelled by() shows bare codes; total(after) with tests", {
  a <- golden_fixture("auto")
  qa_expect(table1_tc(a, by = "rep78", vars = "price contn", total = "before"), "H")
  qa_expect(table1_tc(a, by = "foreign", vars = "price contn \\ rep78 cat", total = "after",
                      test = TRUE, statistic = TRUE, smd = TRUE), "J")
})

test_that("KE-1: continuous SMD uses the root-mean variance denominator", {
  skip_if_not_installed("haven")
  ke <- qa_fixture("ke1")
  tt <- table1_tc(ke, by = "g", vars = "x contn", smd = TRUE, nopvalue = TRUE)
  qa_expect(tt, "I")
  x0 <- ke$x[ke$g == 0]
  x1 <- ke$x[ke$g == 1]
  oracle <- abs((mean(x0) - mean(x1)) / sqrt((stats::var(x0) + stats::var(x1)) / 2))
  expect_equal(unname(tt$stored$table[1, "smd"]), oracle, tolerance = 1e-10)
  expect_identical(colnames(tt$stored$table), "smd")
})

test_that("omitted vars uses every variable, auto-typed (U1 varlist feature)", {
  skip_if_not_installed("haven")
  tt <- table1_tc(qa_fixture("hp"), by = "g")
  qa_expect(tt, "K")
  expect_identical(tt$stored$varlist, "g y z")
  expect_identical(tt$stored$Dapa, "Data are presented as No. (%).")
})

test_that("nopvalue suite: p, test, and statistic columns go; SMD stays", {
  a <- golden_fixture("auto")
  v <- c("mpg", "price", "weight")
  expect_true("p" %in% table1_tc(a, by = "foreign", vars = v)$cols$role)
  tt <- table1_tc(a, by = "foreign", vars = v, nopvalue = TRUE)
  expect_false(any(c("p", "test", "statistic") %in% tt$cols$role))
  tt <- table1_tc(a, by = "foreign", vars = v, nopvalue = TRUE, smd = TRUE)
  expect_true("smd" %in% tt$cols$role)
  expect_true(any(nzchar(tt$body[[which(tt$cols$role == "smd")]])))
  expect_false(any(c("test") %in% table1_tc(a, by = "foreign", vars = v, nopvalue = TRUE, test = TRUE)$cols$role))
  expect_false(any(c("statistic") %in% table1_tc(a, by = "foreign", vars = v, nopvalue = TRUE, statistic = TRUE)$cols$role))
  tt <- table1_tc(a, vars = v, nopvalue = TRUE)
  expect_identical(tt$stored$varlist, "mpg price weight")
  tt <- table1_tc(a, by = "foreign", vars = v, nopvalue = TRUE)
  expect_true(grepl("P-values suppressed", tt$stored$Dapa, fixed = TRUE))
  expect_null(tt$stored$methods)
  a$highmpg <- as.numeric(a$mpg > 20)
  expect_false("p" %in% table1_tc(a, vars = "highmpg bin", by = "foreign", nopvalue = TRUE)$cols$role)
  expect_false("p" %in% table1_tc(a, vars = "rep78", by = "foreign", nopvalue = TRUE)$cols$role)
})

test_that("VC1/KE7/VA7: cells agree with direct summaries", {
  a <- golden_fixture("auto")
  tt <- table1_tc(a, by = "foreign", vars = "price contn %9.1f", sdleft = " (", sdright = ")")
  dom <- tt$body[1, 2]
  expect_identical(dom, sprintf("%.1f (%.1f)", mean(a$price[a$foreign == 0]), sd(a$price[a$foreign == 0])))
  tt <- table1_tc(a, by = "foreign", vars = "price conts %9.0f", iqrmiddle = "-")
  q <- stats::quantile(a$price[a$foreign == 0], c(0.5, 0.25, 0.75), type = 2)
  expect_identical(tt$body[1, 2], sprintf("%.0f (%.0f-%.0f)", q[1], q[2], q[3]))
  tt <- table1_tc(a[!is.na(a$rep78), ], by = "foreign", vars = "rep78 cat", percsign = "%")
  pct <- as.numeric(sub("^.*\\( *([0-9.]+)%\\)$", "\\1", tt$body[-1, 2]))
  expect_lt(abs(sum(pct) - 100), 3)
  hdr <- table1_tc(a, by = "foreign", vars = "price contn")$header[[2]]$text
  expect_identical(hdr[2:3], c(paste0("N=", sum(a$foreign == 0)), paste0("N=", sum(a$foreign == 1))))
})
