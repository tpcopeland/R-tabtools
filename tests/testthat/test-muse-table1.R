test_that("header replacement protection follows each actual percentage denominator", {
  d <- data.frame(g = factor(c(rep("A", 2), rep("B", 8))), age = seq_len(10))
  base <- table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
    smallcells_mode = "primary", headerperc = TRUE, total = "after")
  descriptor <- base$header[[2]]$text[1]
  expect_identical(base$header[[2]]$text[2:4], c("<5", "8 (80.0)", "10 (100.0)"))
  for (column in 2:3) {
    t <- table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
      smallcells_mode = "primary", headerperc = TRUE, total = "after",
      cellreplace = list(list(row = descriptor, column = column, text = "literal")))
    expect_identical(t$header[[2]]$text[column + 1L], "literal")
    expect_identical(t$stored$n_cellreplace, 1L)
  }
  expect_error(table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
    smallcells_mode = "primary", headerperc = TRUE, total = "after",
    cellreplace = list(list(row = descriptor, column = 1L, text = "literal"))),
    class = "tabtools_error_cellreplace_protected")
  # With no Total, B's percentage depends on the withheld A count.
  expect_error(table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
    smallcells_mode = "primary", headerperc = TRUE,
    cellreplace = list(list(row = descriptor, column = 2L, text = "literal"))),
    class = "tabtools_error_cellreplace_protected")
})

test_that("custom and empty strict markers cover every derived statistic", {
  d <- data.frame(g = factor(rep(c("A", "B"), each = 8)), x = c(1, rep(0, 7), rep(c(0, 1), 4)))
  for (marker in c("HIDDEN", "")) {
    t <- table1_tc(d, by = "g", vars = c(x = "bin"), smallcells = 5,
      test = TRUE, statistic = TRUE, smd = TRUE, masktext = marker)
    columns <- which(t$cols$role %in% c("p", "test", "statistic", "smd"))
    row <- which(t$rows$type == "var")
    expect_length(row, 1L)
    expect_gt(length(columns), 1L)
    expect_identical(unname(unlist(t$body[row, columns])), rep(marker, length(columns)))
    expect_true(is.na(t$rows$p[row]))
    expect_true(is.na(t$rows$smd[row]))
    expect_null(t$stored$raw)
  }
})

test_that("literal primary markers protect weighted ESS while visible Total headers remain replaceable", {
  d <- data.frame(g = factor(c(rep("A", 2), rep("B", 8))), age = seq_len(10),
    w = c(1, 3, rep(1, 8)))
  for (marker in c("HIDDEN", "")) {
    t <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", percent_n = TRUE,
      wtcompare = TRUE, smallcells = 5, smallcells_mode = "primary", masktext = marker)
    ess <- which(t$rows$type == "ess")
    small <- which(t$cols$pass == "weighted" & t$cols$group == 1L)
    expect_length(ess, 1L); expect_length(small, 1L)
    expect_identical(t$body[ess, small], marker)
    expect_identical(t$stored$smallcells$n_linked, 1L)
    expect_error(table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", percent_n = TRUE,
      wtcompare = TRUE, smallcells = 5, smallcells_mode = "primary", masktext = marker,
      cellreplace = list(list(row = t$body[[1]][ess], column = small - 1L, text = "literal"))),
      class = "tabtools_error_cellreplace_protected")
  }
  base <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", percent_n = TRUE,
    wtcompare = TRUE, smallcells = 5, smallcells_mode = "primary", headerperc = TRUE, total = "after")
  columns <- which(base$cols$group == 2L)
  expect_length(columns, 2L)
  for (column in columns) {
    t <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", percent_n = TRUE,
      wtcompare = TRUE, smallcells = 5, smallcells_mode = "primary", headerperc = TRUE, total = "after",
      cellreplace = list(list(row = base$header[[2]]$text[1], column = column - 1L, text = "literal")))
    expect_identical(t$header[[2]]$text[column], "literal")
  }
})
