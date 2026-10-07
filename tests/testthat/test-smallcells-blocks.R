# Count blocks per variable (_desctab_collect.ado:626-839) and the display
# suppression matrices of goldens T25-T30n.

test_that("continuous blocks follow _desctab_collect.ado:646-668", {
  b <- tabtools:::tt_sc_block_cont(c(8, 3), c(10, 4))
  expect_equal(b$counts, rbind(c(8, 3), c(2, 1)))
  expect_equal(b$exact, rbind(c(0, 0), c(0, 0)))
  expect_equal(b$sensitive, rbind(c(1, 1), c(0, 0)))
  expect_equal(c(b$rowexact, b$rowsensitive), c(0, 0, 0, 0))
  expect_equal(c(b$colexact, b$colsensitive), c(1, 1, 1, 1))
  expect_equal(c(b$grandexact, b$grandsensitive), c(0, 0))

  b <- tabtools:::tt_sc_block_cont(c(8, NA), c(10, 4), missingsummary = TRUE, total = TRUE)
  expect_equal(b$counts, rbind(c(8, 0), c(2, 4)))
  expect_equal(b$exact, rbind(c(0, 0), c(1, 1)))
  expect_equal(b$sensitive, rbind(c(1, 1), c(1, 1)))
  expect_equal(b$rowexact, c(0, 1))
  expect_equal(b$rowsensitive, c(1, 1))
  expect_equal(c(b$grandexact, b$grandsensitive), c(1, 1))
})

test_that("categorical blocks add a hidden missing row unless missing is shown", {
  lc <- rbind(c(2, 6), c(8, 4))
  b <- tabtools:::tt_sc_block_cat(lc, nonmiss = c(10, 10), sample_n = c(11, 12))
  expect_equal(b$counts, rbind(c(2, 6), c(8, 4), c(1, 2)))
  expect_equal(b$exact[3, ], c(0, 0))
  expect_equal(b$rowexact, c(0, 0, 0))
  expect_identical(b$missrow, 3L)

  b <- tabtools:::tt_sc_block_cat(lc, c(10, 10), c(11, 12), missingsummary = TRUE, total = TRUE)
  expect_equal(b$exact[3, ], c(1, 1))
  expect_equal(b$rowexact, c(1, 1, 1))

  # slashN publishes the missing row (non-missing denominators) unless
  # catrowperc; slashN + catrowperc releases the level-row margins instead.
  b <- tabtools:::tt_sc_block_cat(lc, c(10, 10), c(11, 12), slashN = TRUE)
  expect_equal(b$exact[3, ], c(1, 1))
  expect_equal(b$rowexact, c(0, 0, 0))
  b <- tabtools:::tt_sc_block_cat(lc, c(10, 10), c(11, 12), slashN = TRUE, catrowperc = TRUE)
  expect_equal(b$exact[3, ], c(0, 0))
  expect_equal(b$rowexact, c(1, 1, 0))

  # missing shown as a level: no hidden row; its level index is the missing row.
  b <- tabtools:::tt_sc_block_cat(rbind(lc, c(1, 2)), c(10, 10), c(11, 12),
                                  include_missing = TRUE, missing_level = 3L,
                                  missingsummary = TRUE, total = TRUE)
  expect_identical(nrow(b$counts), 3L)
  expect_identical(b$missrow, 3L)
  expect_equal(b$rowexact, c(1, 1, 1))
})

test_that("binary blocks carry negative and missing rows", {
  b <- tabtools:::tt_sc_block_cat(matrix(c(2, 3), 1), nonmiss = c(50, 49),
                                  sample_n = c(50, 50), binary = TRUE)
  expect_equal(b$counts, rbind(c(2, 3), c(48, 46), c(0, 1)))
  expect_equal(b$exact, rbind(c(1, 1), c(0, 0), c(0, 0)))
  expect_identical(c(b$negrow, b$missrow), c(2L, 3L))
  # slashN prints the denominator, publishing both hidden rows; catrowperc
  # does not change that for binary variables.
  b <- tabtools:::tt_sc_block_cat(matrix(c(2, 3), 1), c(50, 49), c(50, 50), binary = TRUE,
                                  slashN = TRUE, catrowperc = TRUE)
  expect_equal(b$exact, rbind(c(1, 1), c(1, 1), c(1, 1)))
  expect_equal(b$rowexact, c(1, 0, 0))
})

test_that("slashN denominators: small ones are primary, derived ones code 3", {
  # Group 2 has a non-missing denominator of 4 (< 5): code 1.
  b <- tabtools:::tt_sc_block_cat(rbind(c(6, 1), c(7, 3)), nonmiss = c(13, 4),
                                  sample_n = c(13, 4), slashN = TRUE)
  v <- tabtools:::tt_sc_variable(b, 5)
  expect_equal(v$denominator[2], 1)
  expect_true(v$derived)
})

test_that("the N-header mask: a primary anywhere wins, then a complement", {
  vars <- list(list(sample = c(0, 2, 0)), list(sample = c(1, 0, 0)), list(sample = c(2, 0, 0)))
  expect_equal(tabtools:::tt_sc_sample_mask(vars), c(1, 2, 0))
})

sc_golden_cases <- list(
  T25 = list("sc_primary", "group", list(c("category", "cat")), FALSE),
  T26 = list("sc_complement", "group", list(c("category", "cat")), FALSE),
  T27 = list("sc_binary", "group", list(c("rare_ae", "bin")), FALSE),
  T28 = list("sc_complement", "group", list(c("category", "cat")), TRUE),
  T29 = list("auto", "rep78", list(c("price", "cont"), c("mpg", "cont"), c("foreign", "bin")), FALSE),
  T30l = list("sc_primary", "group", list(c("category", "cat")), TRUE),
  T30m = list("sc_complement", "group", list(c("category", "cat")), TRUE),
  T30n = list("sc_binary", "group", list(c("rare_ae", "bin")), TRUE)
)

for (id in names(sc_golden_cases)) {
  test_that(paste(id, "suppression matrix and N_* counts match the golden"), {
    cs <- sc_golden_cases[[id]]
    d <- golden_fixture(cs[[1]])
    vars <- lapply(cs[[3]], function(v) sc_var_from_data(d, cs[[2]], v[1], v[2], total = cs[[4]]))
    m <- sc_display_matrix(vars)
    g <- sc_golden_suppression(id)
    expect_equal(unname(m), unname(g$matrix))
    expect_equal(c(sum(m == 1), sum(m == 2), sum(m == 3)), c(g$primary, g$secondary, g$derived))
  })
}

test_that("demo expectations: sc_primary 4/0, sc_complement 2/2, sc_binary 2/0 with total(after)", {
  counts <- function(fixture, var, type) {
    v <- sc_var_from_data(golden_fixture(fixture), "group", var, type, total = TRUE)
    m <- sc_display_matrix(list(v))
    c(sum(m == 1), sum(m == 2))
  }
  expect_equal(counts("sc_primary", "category", "cat"), c(4, 0))
  expect_equal(counts("sc_complement", "category", "cat"), c(2, 2))
  expect_equal(counts("sc_binary", "rare_ae", "bin"), c(2, 0))
})

test_that("test_smallcells.do: redundant complementary margins are absent (5 primary / 1 secondary)", {
  d <- data.frame(group = c(0, 1, 1, 1, 1, 1), category = c(1, 0, 1, 1, 1, 1))
  v <- sc_var_from_data(d, "group", "category", "cat", total = TRUE)
  m <- sc_display_matrix(list(v))
  expect_equal(c(sum(m == 1), sum(m == 2)), c(5, 1))
})

test_that("test_smallcells.do: 2x2 with total(after) gives primary, complementary, and derived codes", {
  d <- data.frame(group = rep(c(0, 0, 1, 1), c(2, 8, 6, 4)), category = rep(c(0, 1, 0, 1), c(2, 8, 6, 4)))
  v <- sc_var_from_data(d, "group", "category", "cat", total = TRUE)
  m <- sc_display_matrix(list(v))
  expect_equal(c(sum(m == 1), sum(m == 2)), c(2, 2))
  expect_true(any(m == 3))
})

# Cases from qa/test_smallcells.do that need the full table1_tc pipeline
# (the data are those of test_smallcells.do, saved by
# qa/stata/make_table1_phase3.do; test-table1-phase3-qa.R compares the same
# calls cell by cell with Stata).
sc_body <- function(tt) as.matrix(tt$body)[, -1, drop = FALSE]
sc_markers <- function(x, k = 5) grepl(paste0("<", k), x, fixed = TRUE) | grepl(paste0("\u2265", k), x, fixed = TRUE)

test_that("pipeline: continuous contributing N and dependent statistics are suppressed", {
  skip_if_not_installed("haven")
  # test_smallcells.do:225-261
  tt <- table1_tc(sc_pipeline_data("sccont"), by = "group", vars = "value contn \\ category cat",
                  missingsummary = TRUE, test = TRUE, statistic = TRUE, smd = TRUE, smallcells = 5)
  value <- sc_body(tt)[tt$body[[1]] == "value", ]
  expect_identical(sum(value == "<5"), 1L)
  expect_gte(sum(value == "Suppressed"), 1L)
})

test_that("pipeline: wt() uses records and fweights use integer frequencies", {
  skip_if_not_installed("haven")
  # test_smallcells.do:264-312
  tt <- table1_tc(sc_pipeline_data("scwt"), by = "group", vars = "category cat", wt = "wt", wtn = TRUE,
                  smallcells = 5)
  expect_gte(sum(grepl("<5", sc_body(tt), fixed = TRUE)), 2L)
  tt <- table1_tc(sc_pipeline_data("scfw"), by = "group", vars = "category cat", fweight = "fw",
                  total = "after", smallcells = 5)
  expect_identical(sum(grepl("<5", sc_body(tt), fixed = TRUE)), 2L)
})

test_that("pipeline: slashN/headerperc/missingsummary/wtcompare share protected denominators", {
  skip_if_not_installed("haven")
  # test_smallcells.do:315-352
  tt <- table1_tc(sc_pipeline_data("sccomp"), by = "group", vars = "category cat", wt = "wt",
                  wtcompare = TRUE, wtn = TRUE, total = "after", slashN = TRUE, headerperc = TRUE,
                  missingsummary = TRUE, smallcells = 5)
  cells <- c(sc_body(tt))
  expect_gte(sum(sc_markers(cells) | cells == "Suppressed"), 2L)
  expect_false(any(grepl("/10", cells, fixed = TRUE)))
})

test_that("pipeline: percent is refused with smallcells; smallcells(3) compositions", {
  skip_if_not_installed("haven")
  # test_smallcells.do:453-508
  d <- sc_pipeline_data("scmiss")
  expect_error(table1_tc(d, by = "group", vars = "category cat", missing = TRUE, total = "before",
                         percent = TRUE, catrowperc = TRUE, smallcells = 3), "percent-only")
  tt <- table1_tc(d, by = "group", vars = "category cat", missing = TRUE, total = "before",
                  catrowperc = TRUE, smallcells = 3)
  expect_true(any(sc_markers(sc_body(tt), 3)))
  tt <- table1_tc(sc_pipeline_data("scallmiss"), by = "group", vars = "allmissing contn",
                  missingsummary = TRUE, percent_n = TRUE, smallcells = 3)
  cells <- as.matrix(as.data.frame(tt))[, -1]
  expect_true(any(sc_markers(cells, 3) | cells == "Suppressed"))
})

test_that("pipeline: sinks share one redacted source; r(table) carries .d", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  # test_smallcells.do:101-157, :358-432
  out <- withr::local_tempdir()
  xlsx <- file.path(out, "sc.xlsx")
  csv <- file.path(out, "sc.csv")
  md <- file.path(out, "sc.md")
  tt <- table1_tc(sc_pipeline_data("sc2x2"), by = "group", vars = "category cat", total = "after",
                  test = TRUE, statistic = TRUE, smd = TRUE, smallcells = 5, title = "Synthetic small cells",
                  xlsx = xlsx, csv = csv, markdown = md)
  expect_identical(tt$stored$smallcells$threshold, 5L)
  expect_identical(c(tt$stored$N_primary_suppressed, tt$stored$N_secondary_suppressed), c(2L, 2L))
  expect_gte(tt$stored$N_derived_suppressed, 2L)
  expect_true(all(c(1, 2, 3) %in% tt$stored$suppression))
  # Stata stores .d for the withheld p-value and SMD; R has NA.
  expect_true(all(is.na(tt$stored$table[1, ])))
  body <- sc_body(tt)
  expect_gte(sum(grepl("<5", body, fixed = TRUE)), 2L)
  expect_gte(sum(grepl("\u22655", body, fixed = TRUE)), 2L)
  expect_gte(sum(body == "Suppressed"), 1L)
  for (f in c(csv, md)) {
    txt <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
    expect_true(grepl("<5", txt, fixed = TRUE) && grepl("\u22655", txt, fixed = TRUE))
    expect_false(grepl("2 (20", txt, fixed = TRUE) || grepl("4 (40", txt, fixed = TRUE))
  }
  cells <- tidyxl::xlsx_cells(xlsx, sheets = "Table 1")$character
  expect_true(any(grepl("<5", cells, fixed = TRUE)) && any(grepl("\u22655", cells, fixed = TRUE)))
  expect_false(any(grepl("2 (20", cells, fixed = TRUE) | grepl("4 (40", cells, fixed = TRUE), na.rm = TRUE))
  # Without smallcells() the same call shows the raw counts and no markers.
  plain <- table1_tc(sc_pipeline_data("sc2x2"), by = "group", vars = "category cat", total = "after")
  expect_identical(plain$stored$smallcells,
                   list(threshold = 0L, mode = "strict", n_masked = 0L, n_linked = 0L))
  expect_false(any(sc_markers(sc_body(plain))))
  expect_identical(sum(grepl("^2 ", sc_body(plain))), 1L)
  expect_identical(sum(grepl("^4 ", sc_body(plain))), 1L)
})

test_that("test_smallcells.do: k above N is legal and suppresses every positive count", {
  d <- golden_fixture("sc_primary")
  v <- sc_var_from_data(d, "group", "category", "cat", k = 10)
  m <- sc_display_matrix(list(v))
  expect_identical(v$engine$smallcells, 10L)
  expect_true(all(v$cells > 0))
  expect_gte(sum(m %in% c(1, 2)), 4)
})

test_that("pipeline: smallcells(2) is refused and repeated calls are stable", {
  skip_if_not_installed("haven")
  # test_smallcells.do:511-539
  d <- sc_pipeline_data("sc2x2")
  d0 <- d
  expect_error(table1_tc(d, by = "group", vars = "category cat", smallcells = 2),
               "integer greater than or equal to 3")
  expect_error(table1_tc(d, by = "group", vars = "category cat", smallcells = 4.5),
               "integer greater than or equal to 3")
  a <- table1_tc(d, by = "group", vars = "category cat", total = "after", smallcells = 5)
  b <- table1_tc(d, by = "group", vars = "category cat", total = "after", smallcells = 5)
  expect_identical(a, b)
  expect_identical(d, d0)
  expect_identical(a$stored$smallcells$threshold, 5L)
})
