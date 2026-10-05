# Independent sample/count oracles for incomplete descriptive data.
# Numerical cells are printed to four decimals: 5e-5 is half a printed unit.

ad_desc_numbers <- function(cell) {
  as.numeric(strsplit(cell, "±", fixed = TRUE)[[1L]])
}

ad_desc_row <- function(tab, variable, type = "var") {
  which(tab$rows$var == variable & tab$rows$type == type)
}

test_that("Table 1 uses variable-specific observations within the eligible group sample", {
  d <- data.frame(g = factor(c("A", "A", "B", "B", NA, "B"),
                             levels = c("A", "B", "unused")),
                  x = c(2, NA, 4, 6, 999, NA),
                  y = c(NA, 8, NA, 12, 999, NA), z = NA_real_)
  before <- d
  t <- table1_tc(d, vars = c(x = "contn", y = "contn", z = "contn"),
                 by = "g", total = "after", nopvalue = TRUE,
                 missingsummary = TRUE, format = "%12.4f")
  expect_identical(t$header[[1]]$text[-1], c("A", "B", "Total"))
  expect_identical(t$header[[2]]$text[-1], c("N=2", "N=3", "N=5"))
  expect_equal(ad_desc_numbers(t$body[ad_desc_row(t, "x"), 3]),
               c(5, sqrt(2)), tolerance = 5e-5)
  expect_equal(ad_desc_numbers(t$body[ad_desc_row(t, "x"), 4]),
               c(4, 2), tolerance = 5e-5)
  expect_equal(ad_desc_numbers(t$body[ad_desc_row(t, "y"), 4]),
               c(10, sqrt(8)), tolerance = 5e-5)
  expect_identical(as.character(t$body[ad_desc_row(t, "z"), -1]), c("", "", ""))
  expect_identical(as.character(t$body[ad_desc_row(t, "x", "missing_summary"), -1]),
                   c("1 (50)", "1 (33)", "2 (40)"))
  expect_identical(as.character(t$body[ad_desc_row(t, "y", "missing_summary"), -1]),
                   c("1 (50)", "2 (67)", "3 (60)"))
  expect_identical(as.character(t$body[ad_desc_row(t, "z", "missing_summary"), -1]),
                   c("2 (100)", "3 (100)", "5 (100)"))
  expect_identical(d, before)
})

test_that("categorical denominators distinguish omitted missing values from a missing category", {
  d <- data.frame(g = c("A", "A", "B", "B", "B", NA),
                  c = factor(c("a", NA, "b", "a", NA, "out"),
                             levels = c("a", "b", "unused", "out")))
  t <- table1_tc(d, vars = c(c = "cat"), by = "g", total = "after",
                 slashN = TRUE, nopvalue = TRUE, spacelowpercent = FALSE)
  expect_identical(t$body[[1]], c("c", "   a", "   b"))
  expect_identical(as.character(t$body[2, -1]), c("1/1 (100)", "1/2 (50)", "2/3 (67)"))
  expect_identical(as.character(t$body[3, -1]), c("0/1 (0)", "1/2 (50)", "1/3 (33)"))
  tm <- table1_tc(d, vars = c(c = "cat"), by = "g", total = "after",
                  missing = TRUE, slashN = TRUE, nopvalue = TRUE,
                  spacelowpercent = FALSE)
  expect_identical(tm$body[[1]], c("c", "   a", "   b", "   Missing"))
  expect_identical(as.character(tm$body[2, -1]), c("1/2 (50)", "1/3 (33)", "2/5 (40)"))
  expect_identical(as.character(tm$body[4, -1]), c("1/2 (50)", "1/3 (33)", "2/5 (40)"))
})

test_that("all-missing categorical columns have an explicit missing-category contract", {
  d <- data.frame(g = c("A", "A", "B", "B"),
                  c = factor(rep(NA_character_, 4), levels = c("a", "unused")))
  expect_error(table1_tc(d, vars = c(c = "cat"), by = "g"),
               "no categories", class = "rlang_error")
  t <- table1_tc(d, vars = c(c = "cat"), by = "g", missing = TRUE,
                 total = "after", nopvalue = TRUE, slashN = TRUE)
  expect_identical(t$body[[1]], c("c", "   Missing"))
  expect_identical(as.character(t$body[2, -1]), c("2/2 (100)", "2/2 (100)", "4/4 (100)"))
})

test_that("frequency weights expand counts after weight and group exclusions", {
  d <- data.frame(g = c("A", "A", "A", "B", "B", "B", NA, "B"),
                  x = c(2, NA, 1000, 4, 8, 1000, 1000, 1000),
                  c = c("a", "b", "zero", "a", NA, "missingweight", "nogroup", "zero"),
                  f = c(2, 3, 0, 1, 2, NA, 7, 0))
  keep <- c(1L, 2L, 4L, 5L)
  expanded <- d[rep(keep, c(2L, 3L, 1L, 2L)), ]
  args <- list(vars = c(x = "contn", c = "cat"), by = "g", total = "after",
               missing = TRUE, missingsummary = TRUE, slashN = TRUE,
               nopvalue = TRUE, format = "%12.4f", spacelowpercent = FALSE)
  weighted <- do.call(table1_tc, c(list(data = d, fweight = "f"), args))
  unweighted <- do.call(table1_tc, c(list(data = expanded), args))
  expect_identical(weighted$header[[2]]$text[-1], c("N=5", "N=3", "N=8"))
  expect_identical(weighted$body, unweighted$body)
  expect_equal(ad_desc_numbers(weighted$body[ad_desc_row(weighted, "x"), 3]),
               c(20 / 3, sqrt(16 / 3)), tolerance = 5e-5)
  expect_identical(as.character(weighted$body[ad_desc_row(weighted, "x", "missing_summary"), -1]),
                   c("3 (60)", "0", "3 (38)"))
})

test_that("importance weights use observed weighted denominators and retain record N and ESS", {
  d <- data.frame(g = c("A", "A", "A", "B", "B", "B", NA),
                  x = c(2, NA, 1000, 4, 8, 1000, 1000),
                  c = c("a", "b", "zero", "a", NA, "missingweight", "nogroup"),
                  w = c(2, 3, 0, 1, 2, NA, 7))
  t <- table1_tc(d, vars = c(x = "contn", c = "cat"), by = "g", wt = "w",
                 total = "after", missingsummary = TRUE, wtn = TRUE,
                 format = "%12.4f", percformat = "%6.2f", nformat = "%6.2f",
                 spacelowpercent = FALSE)
  expect_identical(t$header[[2]]$text[-1], c("N=2.00", "N=2.00", "N=4.00"))
  expect_equal(ad_desc_numbers(t$body[ad_desc_row(t, "x"), 3]),
               c(20 / 3, 8 / 3), tolerance = 5e-5)
  # Kish ESS is computed from all eligible records, including missing x/c.
  er <- which(t$body[[1]] == "Effective sample size")
  expect_length(er, 1L)
  expect_equal(as.numeric(sub("ESS=", "", as.character(t$body[er, -1]), fixed = TRUE)), c(25 / 13, 9 / 5, 64 / 18), tolerance = 0.005)
  ar <- which(t$body[[1]] == "   a")
  expect_identical(as.character(t$body[ar, -1]),
                   c("0.80 (40.00)", "1.00 (100.00)", "1.50 (50.00)"))
})

test_that("desctab forwards incomplete-data options without changing the table", {
  d <- data.frame(g = c("A", "A", "B", "B"), x = c(1, NA, 4, 6),
                  c = c("one", NA, "one", "two"), f = c(2, 1, 3, 1))
  args <- list(data = d, vars = c(x = "conts", c = "cat"), by = "g",
               fweight = "f", missing = TRUE, missingsummary = TRUE,
               total = "before", slashN = TRUE, nopvalue = TRUE)
  t <- do.call(table1_tc, args)
  a <- do.call(desctab, args)
  expect_identical(a$body, t$body)
  expect_identical(a$header, t$header)
  expect_identical(a$stored$table, t$stored$table)
})

test_that("single-record groups and empty variable groups leave unavailable inference blank", {
  d <- data.frame(g = c("A", "B", "B"), x = c(2, NA, NA),
                  c = factor(c("one", NA, NA), levels = c("one", "unused")))
  expect_silent(t <- table1_tc(d, vars = c(x = "contn", c = "cat"), by = "g",
                               smd = TRUE, missingsummary = TRUE))
  expect_identical(t$header[[2]]$text[2:3], c("N=1", "N=2"))
  expect_identical(as.character(t$body[ad_desc_row(t, "x"), 2:3]), c("2±.", ""))
  expect_identical(t$body[[1]][t$rows$type == "level"], "   one")
  expect_true(all(is.na(t$stored$table)))
})

test_that("empty samples and one observed grouping level cannot create fictitious groups", {
  d <- data.frame(g = factor(c("A", "A"), levels = c("A", "unused")), x = c(1, NA))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g"),
               "at least 2", class = "rlang_error")
  d$g <- NA_character_
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g"),
               "no observations", class = "rlang_error")
  expect_error(table1_tc(d[FALSE, ], vars = c(x = "contn")),
               "no observations", class = "rlang_error")
  d$w <- c(0, NA_real_)
  for (arg in c("wt", "fweight")) {
    args <- c(list(data = d, vars = c(x = "contn")), setNames(list("w"), arg))
    expect_error(do.call(table1_tc, args), "no observations", class = "rlang_error")
  }
})

test_that("nonfinite observations and weights cannot hide on excluded records", {
  for (bad in c(Inf, -Inf, NaN)) {
    d <- data.frame(g = c("A", "B", NA), x = c(1, 2, bad), w = c(1, 1, 0))
    expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", wt = "w"),
                 "infinities|NaN", class = "rlang_error")
    d$x <- c(1, 2, NA_real_)
    d$w[3] <- bad
    for (arg in c("wt", "fweight")) {
      args <- c(list(data = d, vars = c(x = "contn"), by = "g"), setNames(list("w"), arg))
      expect_error(do.call(table1_tc, args), "finite", class = "rlang_error")
    }
  }
})

test_that("wttab applies a union of missingness filters and retains zero weights", {
  d <- data.frame(w = c(0, 2, 4, 100, NA, 100, 100),
                  g = factor(c("A", "A", "B", NA, "B", "B", NA),
                             levels = c("A", "B", "unused")),
                  p = c(1, 1, 2, 1, 2, NA, NA))
  before <- d
  expect_warning(t <- wttab(d, "w", by = "g", period = "p"),
                 "Dropped 4", class = "rlang_warning")
  expect_identical(t$stored$N, 3L)
  expect_identical(t$stored$n_dropped, 4L)
  s <- t$stored$stats
  expect_identical(s$group, rep(c("Overall", "A", "B"), 2L))
  expect_identical(s$n, c(2, 2, 0, 1, 0, 1))
  expect_equal(s$mean[c(1, 2, 4, 6)], c(1, 1, 4, 4), tolerance = 1e-12)
  expect_equal(s$ess[c(1, 2, 4, 6)], c(1, 1, 1, 1), tolerance = 1e-12)
  expect_equal(s$ess_pct[c(1, 2, 4, 6)], c(50, 50, 100, 100), tolerance = 1e-12)
  expect_true(all(is.na(s$mean[c(3, 5)])))
  expect_true(all(is.na(s$ess[c(3, 5)])))
  expect_true(all(is.na(s$sd[c(3, 4, 5, 6)])))
  expect_identical(d, before)
})

test_that("wttab distinguishes all-zero groups from empty design cells", {
  t <- wttab(c(0, 0, 2), by = factor(c("A", "A", "B"), levels = c("A", "B", "unused")),
             period = c(1, 1, 2))
  s <- t$stored$stats
  expect_identical(s$n, c(2, 2, 0, 1, 0, 1))
  expect_equal(s$mean[1:2], c(0, 0), tolerance = 1e-12)
  expect_equal(s$sd[1:2], c(0, 0), tolerance = 1e-12)
  expect_true(all(is.na(s$ess[1:3])))
  expect_true(all(is.na(s$mean[c(3, 5)])))
  expect_identical(wttab(2, by = factor("A", levels = c("A", "unused")))$stored$stats$n, c(1, 1))
  expect_error(wttab(numeric()), "no weights", class = "rlang_error")
  expect_warning(expect_error(wttab(c(NA_real_, NA_real_)), "No observation", class = "rlang_error"),
                 "Dropped 2", class = "rlang_warning")
})

test_that("wttab rejects nonfinite weights before missing-group exclusions", {
  for (bad in c(Inf, -Inf, NaN)) {
    expect_error(wttab(c(1, bad), by = c("A", NA)), "finite",
                 class = "tabtools_error_weights_nonfinite")
  }
})


test_that("all-missing continuous types keep group N while leaving summary and inference blank", {
  d <- data.frame(g = c("A", "A", "B", "B"), x = NA_real_)
  for (type in c("contn", "conts", "contln")) {
    expect_silent(t <- table1_tc(d, vars = c(x = type), by = "g", total = "after",
                                 missingsummary = TRUE, smd = TRUE))
    expect_identical(t$header[[2]]$text[2:4], c("N=2", "N=2", "N=4"))
    expect_identical(as.character(t$body[ad_desc_row(t, "x"), 2:4]), c("", "", ""))
    expect_identical(as.character(t$body[ad_desc_row(t, "x", "missing_summary"), 2:4]),
                     c("2 (100)", "2 (100)", "4 (100)"))
    expect_true(all(is.na(t$stored$table)))
  }
  expect_error(table1_tc(d, vars = c(x = "bin"), by = "g"),
               "no categories", class = "rlang_error")
})
