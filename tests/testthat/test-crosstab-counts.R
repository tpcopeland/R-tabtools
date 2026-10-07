test_that("crosstab counts, percentages and margins have literal native orientation", {
  xt_no_session()
  f <- matrix(c(40, 10, 20, 30), 2)
  x <- xt_table(f)
  expect_equal(unname(x$stored$table), f)
  expect_equal(x$stored$N, 100)
  expect_identical(unname(as.matrix(x$body[1:3, ])),
    matrix(c("1", "2", "Total", "40 (80.0%)", "10 (20.0%)", "50",
             "20 (40.0%)", "30 (60.0%)", "50", "60", "40", "100"), 3))
  expect_identical(x$header[[1]]$text, c("row", "1", "2", "Total"))
  row_percentages <- xt_table(f, rowpct = TRUE)$meta$publication$percentages
  expect_equal(row_percentages,
    matrix(c(200 / 3, 25, 100 / 3, 75), 2, dimnames = list(c("1", "2"), c("1", "2"))))
  total_percentages <- xt_table(f, totalpct = TRUE)$meta$publication$percentages
  expect_equal(total_percentages,
    matrix(c(40, 10, 20, 30), 2, dimnames = list(c("1", "2"), c("1", "2"))))
  expect_identical(dim(row_percentages), c(2L, 2L))
  expect_identical(dim(total_percentages), c(2L, 2L))
  expect_identical(dimnames(row_percentages), list(c("1", "2"), c("1", "2")))
  expect_identical(dimnames(total_percentages), list(c("1", "2"), c("1", "2")))
  expect_error(xt_table(f, rowpct = TRUE, colpct = TRUE), class = "tabtools_error_crosstab_input")
})

test_that("frequency weights filter records before category construction and ledger units", {
  xt_no_session()
  d <- xt_data(matrix(c(40, 10, 20, 30), 2))
  d <- rbind(d, data.frame(row = 999, column = 999, frequency = 0),
             data.frame(row = 888, column = 888, frequency = NA_real_),
             data.frame(row = NA_real_, column = 1, frequency = 3))
  x <- crosstab(d, "row", "column", weights = "frequency")
  expect_equal(unname(x$stored$table), matrix(c(40, 10, 20, 30), 2))
  ledger <- x$meta$sample_accounting$measures
  value <- stats::setNames(ledger$value, ledger$metric)
  expect_equal(unname(value[c("input_n", "eligible_n", "used_n", "missing_n", "zero_weight_n", "excluded_n", "reported_n")]),
               c(7, 7, 4, 1, 1, 3, 100))
  expanded <- d[rep(1:4, d$frequency[1:4]), 1:2]
  expect_equal(crosstab(expanded, "row", "column")$stored$table, x$stored$table)
  filtered <- crosstab(d, "row", "column", weights = "frequency", subset = c(1:4, 4))
  expect_equal(filtered$stored$table, x$stored$table)
  expect_error(crosstab(d, "row", "column", weights = rep(0, 7)), class = "tabtools_error_crosstab_sample")
  for (w in list(rep(-1, 7), rep(.5, 7), matrix(1, 7, 1), rep(Inf, 7))) {
    expect_error(crosstab(d, "row", "column", weights = w), class = "tabtools_error_crosstab_weights")
  }
})

test_that("fractional values and labels remain distinct underlying categories", {
  xt_no_session()
  d <- xt_data(matrix(c(40, 10, 20, 30), 2), c(.1, .2), c(.3, .4))
  attr(d$row, "label") <- 'Outcome $cash `literal`'
  attr(d$row, "labels") <- c("Z event zero" = .1, "A event one" = .2)
  attr(d$column, "labels") <- c("Later" = .3, "Earlier" = .4)
  x <- crosstab(d, "row", "column", weights = "frequency", label = TRUE, or = TRUE)
  expect_identical(x$header[[1]]$text, c('Outcome $cash `literal`', "Later", "Earlier", "Total"))
  expect_identical(x$body[[1]][1:2], c("Z event zero", "A event one"))
  expect_equal(x$meta$axes$row$code, c(.1, .2))
  expect_equal(x$stored$or, 6)
  missing <- rbind(d, data.frame(row = NA_real_, column = .3, frequency = 7))
  x <- crosstab(missing, "row", "column", weights = "frequency", missing = TRUE)
  expect_identical(x$body[[1]][3], "Missing")
  expect_equal(unname(x$stored$table[3, ]), c(7, 0))
  for (bad in list(factor(d$row), as.character(d$row), rep(TRUE, 4), matrix(d$row, 4, 1), complex(real = d$row))) {
    dd <- d; dd$row <- bad
    expect_error(crosstab(dd, "row", "column", weights = "frequency"), class = "tabtools_error_crosstab_input")
  }
})

test_that("ordinary and tagged missing values have separate numeric category identities", {
  skip_if_not_installed("haven")
  xt_no_session()
  d <- xt_data(matrix(c(40, 10, 20, 30), 2))
  d <- rbind(d, data.frame(row = c(NA_real_, haven::tagged_na("a"), haven::tagged_na("z")),
                           column = c(1, 1, 2), frequency = c(7, 3, 4)))
  before <- d
  x <- crosstab(d, "row", "column", weights = "frequency", missing = TRUE)
  expect_identical(x$body[[1]][1:5], c("1", "2", "Missing", "Missing (.a)", "Missing (.z)"))
  expect_identical(x$meta$axes$row$missing_tag, c(NA_character_, NA_character_, "", "a", "z"))
  expect_equal(unname(x$stored$table), rbind(c(40, 20), c(10, 30), c(7, 0), c(3, 0), c(0, 4)))
  expect_equal(x$stored$N, 114)
  expect_equal(crosstab(d, "row", "column", weights = "frequency")$stored$N, 100)
  expect_identical(d, before)
  labelled <- xt_data(matrix(c(40, 10, 20, 30), 2), c(.1, .2), c(.3, .4))
  labelled <- rbind(labelled, data.frame(row = .9, column = .9, frequency = 0),
                    data.frame(row = .8, column = .8, frequency = 10))
  labelled$row <- haven::labelled(labelled$row,
    c("Z event zero" = .1, "A event one" = .2, "Excluded" = .8, "Zero only" = .9))
  labelled$column <- haven::labelled(labelled$column,
    c("Later" = .3, "Earlier" = .4, "Excluded" = .8, "Zero only" = .9))
  original <- labelled
  result <- crosstab(labelled, "row", "column", weights = "frequency", subset = 1:5, label = TRUE, or = TRUE)
  expect_identical(result$body[[1]][1:2], c("Z event zero", "A event one"))
  expect_identical(result$header[[1]]$text[2:3], c("Later", "Earlier"))
  expect_identical(result$meta$axes$row$code, c(.1, .2))
  expect_equal(unname(result$stored$table), matrix(c(40, 10, 20, 30), 2))
  expect_equal(result$stored$or, 6)
  expect_true(identical(labelled, original, single.NA = FALSE))
})


test_that("plain numeric value labels survive filtering without changing numeric axes", {
  xt_no_session()
  d <- xt_data(matrix(c(40, 10, 20, 30), 2), c(.1, .2), c(.3, .4))
  d <- rbind(d, data.frame(row = .9, column = .9, frequency = 0),
             data.frame(row = .8, column = .8, frequency = 10))
  attr(d$row, "labels") <- c("Z event zero" = .1, "A event one" = .2, "Excluded" = .8, "Zero only" = .9)
  attr(d$column, "labels") <- c("Later" = .3, "Earlier" = .4, "Excluded" = .8, "Zero only" = .9)
  before <- d
  x <- crosstab(d, "row", "column", weights = "frequency", subset = 1:5, label = TRUE, or = TRUE)
  expect_identical(x$body[[1]][1:2], c("Z event zero", "A event one"))
  expect_identical(x$header[[1]]$text[2:3], c("Later", "Earlier"))
  expect_identical(x$meta$axes$row$code, c(.1, .2))
  expect_equal(unname(x$stored$table), matrix(c(40, 10, 20, 30), 2))
  expect_equal(x$stored$or, 6)
  expect_true(identical(d, before, single.NA = FALSE))
  no_labels <- crosstab(d, "row", "column", weights = "frequency", subset = 1:5, label = FALSE)
  expect_false(identical(no_labels$body[[1]][1:2], c("Z event zero", "A event one")))
})


test_that("odds-ratio limits retain native cc numerical iteration and zero-cell boundary", {
  xt_no_session()
  dense <- xt_table(matrix(c(40, 10, 20, 30), 2), or = TRUE)
  # Independent literals from cc's equal-tailed inversion, not R fisher.test.
  expect_equal(c(dense$stored$or_lo, dense$stored$or_hi),
    c(2.2644537582212076, 16.373903236546898), tolerance = 1e-10)
  sparse <- xt_table(matrix(c(1, 3, 3, 1), 2), or = TRUE)
  expect_equal(c(sparse$stored$or_lo, sparse$stored$or_hi),
    c(.0015968245324204586, 4.7228962446454164), tolerance = 1e-10)
  expect_equal(c(dense$stored$or, sparse$stored$or), c(6, 1 / 9))
  # cci's explicit zero-cell Cornfield path gives no artificial exact upper.
  zero <- xt_table(matrix(c(2, 1, 1, 0), 2), or = TRUE)
  expect_identical(c(zero$stored$or, zero$stored$or_lo, zero$stored$or_hi), c(0, 0, 0))
  published <- xt_table(matrix(c(1250, 386, 4, 4), 2), or = TRUE, level = 90)
  expect_equal(c(published$stored$or_lo, published$stored$or_hi),
    c(.7698467, 13.59664), tolerance = 2e-4)
})

test_that("crosstab console right-aligns every native column and association row", {
  xt_no_session()
  x <- xt_table(matrix(c(40, 10, 20, 30), 2), or = TRUE, rr = TRUE, rd = TRUE)
  expect_identical(x$layout$align, "right")
  expect_identical(tt_console_lines(x), c(
    "  +---------------------------------------------------------------------------------------+",
    "  |                                                 row            1            2   Total |",
    "  |                                                   1   40 (80.0%)   20 (40.0%)      60 |",
    "  |                                                   2   10 (20.0%)   30 (60.0%)      40 |",
    "  |                                               Total           50           50     100 |",
    "  | Pearson's chi-squared test: chi2 = 16.67, p < 0.001                                   |",
    "  |                        OR = 6.0 (95% CI: 2.3, 16.4)                                   |",
    "  |                         RR = 3.0 (95% CI: 1.6, 5.5)                                   |",
    "  |                   RD = 0.400 (95% CI: 0.225, 0.575)                                   |",
    "  +---------------------------------------------------------------------------------------+",
    ""))
})
