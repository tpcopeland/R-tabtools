test_that("primary masks returned counts and percentages while retaining computed inference", {
  xt_no_session()
  f <- matrix(c(1, 9, 9, 1), 2)
  raw <- xt_table(f, or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE)
  x <- xt_table(f, smallcells = 3, smallcells_mode = "primary", or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE)
  expect_identical(x$stored$smallcells,
    list(threshold = 3L, mode = "primary", n_masked = 2L, n_linked = 2L))
  expect_identical(x$stored$smallcells_mode, "primary")
  expect_equal(unname(x$stored$suppression), diag(2))
  expect_equal(unname(x$stored$table), matrix(c(NA, 9, 9, NA), 2))
  expect_equal(unname(as.matrix(x$body[1:2, 2:3])), matrix(c("<3", "9 (90.0%)", "9 (90.0%)", "<3"), 2))
  for (key in c("p", "or", "or_lo", "or_hi", "rr", "rr_lo", "rr_hi", "rd", "rd_lo", "rd_hi",
                "z_trend", "chi2_trend", "p_trend")) expect_equal(x$stored[[key]], raw$stored[[key]])
  expect_false("raw" %in% names(x$stored))
  expect_equal(x$stored$N, 20)
  expect_match(x$footnote, "may permit reconstruction", fixed = TRUE)
  ledger <- x$meta$sample_accounting$measures
  expect_true(all(is.na(ledger$value[ledger$metric != "reported_n"])))
  expect_equal(ledger$value[ledger$metric == "reported_n"], 20)
})

test_that("strict complements protect count reconstruction and invalidate all inference companions", {
  xt_no_session()
  x <- xt_table(matrix(c(1, 9, 9, 1), 2), smallcells = 3, or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE)
  expect_equal(unname(x$stored$suppression), matrix(c(1, 2, 2, 1), 2))
  expect_true(all(is.na(x$stored$table)))
  for (key in c("chi2", "p", "or", "or_lo", "or_hi", "rr", "rr_lo", "rr_hi", "rd", "rd_lo", "rd_hi",
                "z_trend", "chi2_trend", "p_trend")) expect_true(is.na(x$stored[[key]]))
  expect_identical(x$stored$smallcells,
    list(threshold = 3L, mode = "strict", n_masked = 4L, n_linked = 9L))
  expect_identical(x$stored$smallcells_mode, "full")
  expect_identical(c(x$stored$N_primary_suppressed, x$stored$N_secondary_suppressed, x$stored$N_derived_suppressed),
                   c(2L, 2L, 9L))
  expect_true(all(is.na(x$meta$publication$percentages)))
  expect_true(all(grepl("Suppressed", x$body[[1]][x$rows$crosstab_role %in% c("test", "or", "rr", "rd", "trend")], fixed = TRUE)))
  expect_false(any(c("raw", "expected", "min_expected", "source", "counts") %in% c(names(x$stored), names(x$meta))))
  # Exact margins 10/10 permit BOTH diagonal primary values 1 and 2 after
  # complementary >=3 cells, so neither primary value is reconstructed.
  candidates <- lapply(1:2, function(v) matrix(c(v, 10 - v, 10 - v, v), 2))
  expect_true(all(vapply(candidates, function(f) all(rowSums(f) == 10) && all(colSums(f) == 10) &&
    all(f[diag(2) == 1] < 3 & f[diag(2) == 1] > 0) && all(f[diag(2) == 0] >= 3), TRUE)))
})

test_that("primary percentage dependencies include protected denominators and zero numerators", {
  xt_no_session()
  f <- matrix(c(1, 1, 0, 10), 2)
  column <- xt_table(f, smallcells = 3, smallcells_mode = "primary")
  expect_equal(unname(column$meta$publication$column_totals), c(NA, 10))
  expect_identical(unname(column$meta$publication$percentage_linked), matrix(c(TRUE, TRUE, FALSE, FALSE), 2))
  expect_identical(column$body[[3]][1], "0 (0.0%)")
  row <- xt_table(f, rowpct = TRUE, smallcells = 3, smallcells_mode = "primary")
  expect_identical(row$body[[3]][1], "0")
  expect_true(is.na(row$meta$publication$percentages[1, 2]))
  expect_equal(unname(row$meta$publication$row_totals), c(NA, 11))
  expect_identical(row$stored$smallcells$n_linked, 3L)
  for (text in c("", "0.0%", '"$cash`\\literal"')) {
    x <- xt_table(matrix(c(1, 9, 9, 1), 2), smallcells = 3, masktext = text)
    expect_identical(unname(as.matrix(x$body[1:2, 2:3])), matrix(text, 2, 2))
    expect_true(all(is.na(x$stored$table)))
    expect_true(is.na(x$stored$p))
    expect_match(x$footnote, if (nzchar(text)) "chosen masking text" else "blank cells", fixed = TRUE)
  }
  safe <- xt_table(matrix(10, 2, 2), smallcells = 3)
  expect_false("raw" %in% names(safe$stored))
  expect_identical(safe$stored$smallcells$n_masked, 0L)
  expect_identical(safe$stored$smallcells$n_linked, 0L)
  expect_equal(safe$stored$p, 1)
})


test_that("crosstab strict note states the count and selected-denominator dependency", {
  xt_no_session()
  x <- xt_table(matrix(c(1, 9, 9, 1), 2), smallcells = 3)
  expect_identical(x$footnote, paste0(
    "Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction. ",
    "Percentages are withheld when their count or selected denominator is suppressed. ",
    "Tests and association estimates are withheld when primary counts are protected."))
  expect_false(grepl("any variable carrying a suppressed count", x$footnote, fixed = TRUE))
})
