# Targeted post-baseline ports: Stata 2.5.2-2.5.4, source pin 4eecca4d.
# The authenticated historical 2.5.1 fixtures remain unchanged.

post251_cont <- function() {
  d <- data.frame(g = factor(c(rep(1, 4), rep(2, 6), rep(3, 6))), x = as.double(1:16))
  d$x[c(4, 11, 12)] <- NA_real_
  d
}

post251_cat <- function() {
  counts <- rbind(c(2, 0, 6), c(2, 1, 1), c(1, 0, 1), c(1, 3, 4))
  missing <- c(6, 2, 4, 1)
  d <- do.call(rbind, lapply(1:4, function(g) {
    data.frame(g = g, v = c(rep(1:3, counts[g, ]), rep(NA_integer_, missing[g])))
  }))
  d$g <- factor(d$g)
  d$v <- factor(d$v, levels = 1:3)
  d$x <- as.double(seq_len(nrow(d)))
  d
}

post251_missing <- function() {
  missing <- c(1, 4, 1, 4, 0)
  zero <- c(1, 2, 0, 2, 0)
  one <- c(3, 6, 4, 8, 12)
  d <- do.call(rbind, lapply(1:5, function(g) {
    data.frame(g = g, b = c(rep(0, zero[g]), rep(1, one[g]), rep(NA_real_, missing[g])))
  }))
  d$g <- factor(d$g)
  d
}

test_that("post251 strict continuous summaries release a lower bound without being masked", {
  d <- post251_cont()
  before <- d
  tt <- table1_tc(d, by = "g", vars = c(x = "contn"), missingsummary = TRUE, smallcells = 3)
  expect_identical(tt$header[[2]]$text[2:4], c("\u22653", "N=6", "N=6"))
  expect_identical(tt$body[[2]], c("2\u00b11", "<3"))
  expect_identical(unname(tt$stored$suppression[2L, 1:3]), c(2, 0, 0))
  expect_identical(unname(tt$stored$suppression[3L, 1:3]), c(0, 0, 0))
  expect_null(tt$stored$raw)
  ledger <- tt$meta$sample_accounting$measures
  expect_true(all(is.na(ledger$value[ledger$population_id == "group/1" & ledger$status != "not_applicable"])))
  expect_identical(d, before)
})

test_that("post251 visible Total cannot reconstruct a withheld continuous group N", {
  tt <- table1_tc(post251_cont(), by = "g", vars = c(x = "contn"),
                  missingsummary = TRUE, smallcells = 3, total = "after")
  expect_identical(tt$header[[2]]$text[2:5], c("\u22653", "N=6", "N=6", "\u22653"))
  expect_identical(tt$body[[5]][2L], "3")
  expect_identical(unname(tt$stored$suppression[2L, 1:4]), c(2, 0, 0, 2))
  expect_identical(unname(tt$stored$suppression[3L, 1:4]), c(0, 0, 0, 0))
})

test_that("post251 primary continuous mode retains ordinary N and summary values", {
  tt <- table1_tc(post251_cont(), by = "g", vars = c(x = "contn"),
                  missingsummary = TRUE, smallcells = 3, smallcells_mode = "primary")
  expect_identical(tt$header[[2]]$text[2:4], c("N=4", "N=6", "N=6"))
  expect_identical(tt$body[[2]], c("2\u00b11", "<3"))
  expect_identical(tt$stored$N_secondary_suppressed, 0L)
  expect_identical(unname(tt$stored$suppression[2L, 1:3]), c(0, 0, 0))
  expect_type(tt$stored$raw$table, "double")
})

test_that("post251 strict small slashN denominators and reconstructing Totals are withheld", {
  d <- post251_cat()
  tt <- table1_tc(d, by = "g", vars = c(x = "contn", v = "cat"),
                  slashN = TRUE, total = "after", smallcells = 3)
  values <- unlist(tt$body, use.names = FALSE)
  expect_false(any(grepl("/<3", values, fixed = TRUE)))
  expect_identical(sum(grepl("/Suppressed", tt$body[[4]], fixed = TRUE)), 1L)
  expect_identical(sum(grepl("/Suppressed", tt$body[[6]], fixed = TRUE)), 2L)
  primary <- table1_tc(d, by = "g", vars = c(x = "contn", v = "cat"),
                       slashN = TRUE, total = "after", smallcells = 3, smallcells_mode = "primary")
  expect_true(any(grepl("/<3", unlist(primary$body), fixed = TRUE)))
})

test_that("post251 visible Missing counts drop only percentages with withheld N and refuse replacement", {
  d <- post251_missing()
  tt <- table1_tc(d, by = "g", vars = c(b = "bin"), slashN = TRUE,
                  total = "before", missingsummary = TRUE, smallcells = 3)
  row <- which(trimws(tt$body[[1]]) == "Missing")
  expect_identical(length(row), 1L)
  group_columns <- which(tt$cols$role == "group")
  expect_identical(tt$body[[group_columns[2L]]][row], "4")
  expect_identical(tt$body[[group_columns[4L]]][row], "4")
  expect_identical(unname(tt$stored$suppression[2L, 1:4]), c(2, 2, 2, 2))
  # The count is visible, but its withheld percentage remains protected.
  expect_identical(unname(tt$stored$suppression[row + 2L, 2L]), 0)
  expect_error(table1_tc(d, by = "g", vars = c(b = "bin"), slashN = TRUE,
    total = "before", missingsummary = TRUE, smallcells = 3,
    cellreplace = list(list(row = "Missing", column = "2", text = "released"))),
    class = "tabtools_error_cellreplace_protected")
})

test_that("post251 released lower-bound flags validate exact shape and never become display codes", {
  engine <- tabtools:::tt_smallcells
  counts <- matrix(c(3, 1, 6, 0), 2L)
  args <- list(counts = counts, smallcells = 3, exact = matrix(c(0, 1, 0, 1), 2L),
               sensitive = matrix(1, 2L, 2L), colexact = c(1, 1), colsensitive = c(1, 1),
               lower = matrix(c(1, 0, 1, 0), 2L))
  result <- do.call(engine, args)
  expect_identical(unname(result$colmask), c(2, 0))
  expect_identical(unname(result$mask[1L, ]), c(0, 0))
  for (bad in list(matrix(0, 1L, 2L), matrix(NA_real_, 2L, 2L), matrix(2, 2L, 2L))) {
    args$lower <- bad
    expect_error(do.call(engine, args), class = "tabtools_smallcells_input")
  }
})
