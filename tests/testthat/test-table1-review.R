# Regression tests for the Phase 2 independent review (IMPLEMENTATION_PLAN.md,
# "Phase 2 review outcome"). Stata-written expectations come from
# qa/stata/make_table1_review.do (tests/testthat/fixtures/table1_review/):
# the csv() sink, the listing, and r() of each call. Cells and the listing
# use the row-wise p mask; test/statistic cells stay masked (plan D3).

rv_dir <- function() test_path("fixtures", "table1_review")
rv_data <- function(name) as.data.frame(haven::read_dta(file.path(rv_dir(), paste0(name, ".dta"))))
rv_agg <- function() as.data.frame(haven::read_dta(test_path("fixtures", "table1_qa", "agg.dta")))

rv_expect <- function(tt, id, mask = "p,test,statistic") {
  want <- golden_read_cells_file(file.path(rv_dir(), paste0(id, ".csv")))
  mm <- golden_compare_cells(tt, want, mask)
  expect_identical(nrow(mm), 0L, label = paste(id, "cells", paste(utils::capture.output(print(mm)), collapse = "\n")))
  why <- golden_compare_console(utils::capture.output(print(tt)),
                                golden_read_lines(file.path(rv_dir(), paste0(id, "_console.txt"))), mask)
  expect_identical(why, character(), label = paste(id, "console"))
  stored <- utils::read.csv(file.path(rv_dir(), paste0(id, "_stored.csv")), colClasses = "character",
                            na.strings = character(), encoding = "UTF-8")
  fields <- unique(stored$name[stored$kind != "meta"])
  expect_true(all(c("Dapa", "varlist") %in% fields), label = paste(id, "stored fixture holds r()"))
  why <- golden_compare_stored(tt$stored, stored, fields = fields, mask = "p", p_table = golden_table_p_rows(tt))
  expect_identical(why, character(), label = paste(id, "stored"))
}

# ---------------------------------------------------------------------------
# P0-1: sums in double, in data order

test_that("P0-1: sums are sequential IEEE double, not long double", {
  x <- c(1e16, 1, -1e16, rep(0.1, 3))
  seq_sum <- function(v) {
    s <- 0
    for (e in v) s <- s + e
    s
  }
  expect_identical(tabtools:::.stata_sum(x), seq_sum(x))
  y <- rep(2.675, 15000)
  expect_identical(tabtools:::.stata_sum(y), seq_sum(y))
  expect_identical(tabtools:::.stata_sum(numeric()), 0)
  # A constant 2.675 in 15,000 rows: Stata's ss is below -1e-8, so the SD is
  # missing ("2.68±."), while an exact sum would give 0.
  ms <- tabtools:::.t1_mean_sd(y)
  expect_true(is.na(ms[["sd"]]))
  expect_identical(stata_fmt(ms[["mean"]], "%5.2f"), "2.68")
})

test_that("P0-1: display ties match Stata (p11: constants, 2.68±.)", {
  skip_if_not_installed("haven")
  pat <- rv_data("ties11")
  d <- pat[rep(1:4, 7500), ]
  rownames(d) <- NULL
  tt <- table1_tc(d, vars = "a contn %5.1f \\ b contn %5.1f \\ c contn %5.1f \\ d contn %5.1f %5.3f \\ e contn %5.2f",
                  by = "g", total = "after", nopvalue = TRUE)
  rv_expect(tt, "RV01")
  expect_identical(unname(unlist(tt$body[5, 2:4])), rep("2.68±.", 3))
})

test_that("P0-1: display ties match Stata (p13: 40 constants, three group sizes)", {
  skip_if_not_installed("haven")
  vals <- rv_data("ties13")
  d <- vals[rep(1L, 11100), ]
  rownames(d) <- NULL
  d$g <- rep(0:2, c(100, 1000, 10000))
  tt <- table1_tc(d, vars = paste(paste0("c", 1:40, " contn %5.1f"), collapse = " \\ "),
                  by = "g", total = "after", nopvalue = TRUE)
  rv_expect(tt, "RV02")
})

# ---------------------------------------------------------------------------
# P0-2: "" is missing

test_that("P0-2: an empty string is missing in by() and in categorical variables", {
  skip_if_not_installed("haven")
  b <- rv_data("blank")
  expect_true(any(b$g == "") && any(b$s == ""))
  rv_expect(table1_tc(b, vars = "x contn", by = "g"), "RV03")
  rv_expect(table1_tc(b, vars = "s cat", by = "g"), "RV04")
  rv_expect(table1_tc(b, vars = "s cat", by = "g", missing = TRUE), "RV05")
  rv_expect(table1_tc(b, by = "g", missingsummary = TRUE), "RV06")
  rv_expect(table1_tc(b, vars = "s cat", missing = TRUE), "RV07")
})

test_that("P0-2: factors lose an empty level; labelled character vectors too", {
  skip_if_not_installed("haven")
  b <- rv_data("blank")
  f <- b
  f$g <- factor(f$g)
  f$s <- factor(f$s)
  expect_identical(tabtools:::.tt_grid(table1_tc(f, vars = "s cat", by = "g", missing = TRUE)),
                   tabtools:::.tt_grid(table1_tc(b, vars = "s cat", by = "g", missing = TRUE)))
  h <- b
  h$s <- haven::labelled(h$s, label = "S")
  tt <- table1_tc(h, vars = "s cat", by = "g")
  expect_identical(tt$body[[1]], c("S", "   u", "   v"))
  # A column of empty strings has no categories, as in Stata.
  e <- data.frame(g = c(0, 1, 0, 1), s = "")
  expect_error(table1_tc(e, vars = "s cat", by = "g"), "no categories")
})

# ---------------------------------------------------------------------------
# P0-3 / P2-3: delimiters

test_that("P0-3: percsign is trimmed in cat/bin cells, untrimmed in missing-summary and headerperc", {
  skip_if_not_installed("haven")
  tt <- table1_tc(rv_agg(), by = "trt", vars = "stage cat \\ female bin \\ age contn", percsign = " %",
                  headerperc = TRUE, missingsummary = TRUE)
  rv_expect(tt, "RV08")
  expect_identical(tt$header[[2]]$text[2], "6 (50.0 %)")
  rv_expect(table1_tc(rv_agg(), by = "trt", vars = "stage cat \\ female bin", percsign = "pct ", percent_n = TRUE,
                      missing = TRUE, spacelowpercent = TRUE, total = "after"), "RV09")
})

test_that("P2-3: empty delimiters use the defaults in cells and varlabplus labels only", {
  skip_if_not_installed("haven")
  tt <- table1_tc(rv_agg(), by = "trt", vars = "age contn \\ marker conts \\ marker contln", iqrmiddle = "",
                  sdleft = "", gsdleft = "", gsdright = "", varlabplus = TRUE)
  rv_expect(tt, "RV10")
  expect_match(tt$stored$Dapa, "meanSD", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Console separators (Phase 3 finding): sepby(factor_sep) is by label text

test_that("console rules follow label text: same-label variables share a block", {
  skip_if_not_installed("haven")
  sep <- rv_data("sep")
  v <- "yN contn \\ x1 contn \\ x2 contn \\ c1 cat \\ b1 bin \\ c2 cat \\ c3 cat"
  tt <- table1_tc(sep, by = "g", vars = v)
  rv_expect(tt, "RV11")
  # A variable labelled "N": label blanked, no r(table) row, listed in the
  # N row's block (desctab.ado:1058, :1080). r(table) has one row per
  # other variable (2.1.10+: no category-level rows).
  expect_identical(tt$body[[1]][1], "")
  expect_identical(nrow(tt$stored$table), 6L)
  rv_expect(table1_tc(sep, by = "g", vars = v, varlabplus = TRUE, missingsummary = TRUE), "RV12")
  rv_expect(table1_tc(sep, vars = "x1 contn \\ x2 contn \\ c2 cat \\ c3 cat", missingsummary = TRUE), "RV13")
})

test_that(".t1_blocks starts a block wherever the key changes", {
  expect_identical(tabtools:::.t1_blocks(c("a", "a", "b", "a")), c(1L, 1L, 2L, 3L))
  expect_identical(tabtools:::.t1_blocks(c("N", "x", "x")), c(-2L, 1L, 1L))
  expect_identical(tabtools:::.t1_blocks(character()), integer())
})

# ---------------------------------------------------------------------------
# P1-4 / P2-6 / P2-5 / P3-8: tests

fisher_data <- function() {
  set.seed(7)
  data.frame(g = sample(1:4, 300, TRUE), c = sample(1:6, 300, TRUE))
}

test_that("P1-4: the simulated Fisher fallback is labelled, reproducible, and leaves the RNG alone", {
  d <- fisher_data()
  run <- function() table1_tc(d, vars = c(c = "cate"), by = "g", test = TRUE,
                              test_args = list(fisher.test = list(workspace = 1)))
  set.seed(1)
  before <- .Random.seed
  kind <- RNGkind()
  t1 <- run()
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  set.seed(2)
  t2 <- run()
  expect_identical(t1$stored$table[1, "p_value"], t2$stored$table[1, "p_value"])
  test_col <- which(t1$cols$role == "test")
  expect_identical(t1$body[[test_col]][1], "Fisher's exact (simulated)")
  expect_identical(t1$stored$fisher_simulated, "c")
  expect_match(t1$stored$methods,
               "P-values were calculated using Fisher's exact test with a Monte Carlo p-value (100,000 replicates).",
               fixed = TRUE)
  # An exact result stores no simulated variables.
  t3 <- table1_tc(d[d$c <= 2 & d$g <= 2, ], vars = c(c = "cate"), by = "g", test = TRUE)
  expect_identical(t3$body[[which(t3$cols$role == "test")]][1], "Fisher's exact")
  expect_identical(t3$stored$fisher_simulated, character())
  expect_match(t3$stored$methods, "using Fisher's exact test.", fixed = TRUE)
})

test_that("P2-6: Fisher starts from the default workspace and fails over quickly", {
  set.seed(7)
  n <- 5000
  d <- data.frame(g = sample(1:4, n, TRUE), c = sample(1:8, n, TRUE))
  tt <- table1_tc(d, vars = c(c = "cate"), by = "g", test = TRUE)
  expect_identical(tt$stored$fisher_simulated, "c")
  small <- table(c(1, 1, 2, 2, 1), c(1, 2, 1, 2, 2))
  r <- tabtools:::.t1_fisher(small, NULL)
  expect_true(is.na(r$B))
  expect_equal(r$p.value, stats::fisher.test(small)$p.value)
})

test_that("P2-5: test_args are validated; non-data test errors warn", {
  d <- data.frame(g = rep(0:1, 50), b = rep(c(0, 1, 1, 0, 1), 20), x = seq_len(100) %% 7)
  expect_error(table1_tc(d, vars = c(b = "bin"), by = "g", test_args = list(chisq.test = list(corect = FALSE))),
               "corect")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(alternative = "less"))),
               "two.sided")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(x = 1))), "reserved")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(ttest = list())), "named list keyed")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(1))), "named list")
  # A test error that is not about the data warns and leaves p blank.
  expect_warning(tt <- table1_tc(d, vars = c(x = "contn"), by = "g",
                                 test_args = list(t.test = list(conf.level = 2))),
                 "t.test")
  expect_identical(tt$body[[which(tt$cols$role == "p")]][1], "")
  # Unequal groups cannot be paired: an error since Milestone H (H7).
  d2 <- d[-1, ]
  expect_error(table1_tc(d2, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(paired = TRUE))),
               "equal size")
  # Data-driven failures stay silent (Stata's capture): a single group with data.
  d3 <- data.frame(g = c(0, 0, 1, 1), x = c(1, 2, NA, NA))
  expect_no_warning(table1_tc(d3, vars = c(x = "contn"), by = "g"))
  # Two-sided alternatives are accepted.
  expect_no_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(alternative = "two.sided"))))
})

test_that("P3-8: the test label and methods follow the method R ran (Yates)", {
  d <- data.frame(g = rep(0:1, each = 50), b = c(rep(0:1, c(30, 20)), rep(0:1, c(15, 35))))
  tt <- table1_tc(d, vars = c(b = "bin"), by = "g", test = TRUE, test_args = list(chisq.test = list(correct = TRUE)))
  expect_identical(tt$body[[which(tt$cols$role == "test")]][1], "Pearson's chi-squared (Yates)")
  expect_match(tt$stored$methods, "Pearson's chi-squared test with Yates' continuity correction.", fixed = TRUE)
  expect_equal(tt$stored$table[1, "p_value"], stats::chisq.test(table(d$b, d$g), correct = TRUE)$p.value)
})

# ---------------------------------------------------------------------------
# P2 edge cases

test_that("P2-1: zero rows is 'no observations', with or without by()", {
  d <- data.frame(x = 1:3, g = c(0, 1, 0))
  expect_error(table1_tc(d[0, ], vars = "x"), "no observations")
  expect_error(table1_tc(d[0, ], vars = "x", by = "g"), "no observations")
})

test_that("P2-2: vars = NULL keeps column names with spaces and backslashes", {
  d <- data.frame(g = c(0, 1, 0, 1), check.names = FALSE)
  d[["body mass"]] <- c(20, 22, 25, 30)
  d[["a\\b"]] <- c(1, 0, 1, 1)
  tt <- table1_tc(d, by = "g", nopvalue = TRUE)
  expect_true(all(c("g", "body mass", "a\\b") %in% tt$body[[1]]))
  expect_identical(tt$stored$varlist, "g body mass a\\b")
  # The named form works for any name.
  expect_identical(table1_tc(d, vars = c(`body mass` = "contn"), by = "g")$body[[1]], "body mass")
})

test_that("P2-4: Date and date-time columns are refused", {
  d <- data.frame(g = c(0, 1, 0, 1), dt = as.Date("2020-01-01") + 0:3)
  d$ct <- as.POSIXct(d$dt)
  expect_error(table1_tc(d, vars = "dt", by = "g"), "date")
  expect_error(table1_tc(d, vars = "ct contn", by = "g"), "date")
  expect_error(table1_tc(d, vars = "g", by = "dt"), "date")
})

test_that("P3-9: a list column gets a clear error", {
  d <- data.frame(g = c(0, 1))
  d$l <- list(1, 2)
  expect_error(table1_tc(d, by = "g"), "list column")
})

# ---------------------------------------------------------------------------
# P3 items

test_that("P3-1: labels overrides the by() label in the methods paragraph", {
  d <- data.frame(g = rep(0:1, 5), x = 1:10)
  tt <- table1_tc(d, vars = "x contn", by = "g", labels = c(g = "Treatment arm"))
  expect_match(tt$stored$methods, "groups defined by Treatment arm.", fixed = TRUE)
})

test_that("P3-2: dots prints progress; excel is a synonym of xlsx", {
  skip_if_not_installed("tidyxl")
  d <- data.frame(g = rep(0:1, 5), x = 1:10, y = rep(0:1, each = 5))
  expect_message(table1_tc(d, vars = "x contn \\ y bin", by = "g", dots = TRUE), "Processing 2 variable\\(s\\): \\.\\.")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tt <- table1_tc(d, vars = "x contn", by = "g", excel = f, sheet = "S")
  expect_true(file.exists(f))
  expect_identical(tt$stored$xlsx, f)
  expect_identical(tidyxl::xlsx_sheet_names(f), "S")
})

test_that("P3-3 (KE0.1): categorical SMD is coding invariant and reduces to the binary SMD", {
  f <- data.frame(g = c(0, 0, 0, 1, 1, 1), category = c(1, 2, 3, 1, 2, 3), n = c(30, 50, 20, 45, 35, 20))
  d <- f[rep(seq_len(nrow(f)), f$n), 1:2]
  d$reversed <- c(3, 2, 1)[d$category]
  d$binary <- as.numeric(d$category == 1)
  smd <- function(v, t) table1_tc(d, by = "g", vars = stats::setNames(t, v), smd = TRUE)$stored$table[1, "smd"]
  expect_lt(abs(smd("category", "cat") - smd("reversed", "cat")), 1e-10)
  expect_lt(abs(smd("binary", "bin") - smd("binary", "cat")), 1e-10)
})

test_that("P3-4: r(table) names are cut at 32 characters and stay valid UTF-8", {
  # desctab 2.1.10+ cuts with usubstr() (characters), so the multi-byte
  # character that 2.1.9 split survives whole.
  nm <- tabtools:::.t1_rowname("Body mass index in kilogramsx p\u00e5 m\u00b2", 1L)
  expect_true(validUTF8(nm))
  expect_identical(nm, "Body_mass_index_in_kilogramsx_p\u00e5")
  expect_no_error(nchar(nm))
})

test_that("P3-5: title = \"\" is no title", {
  d <- data.frame(g = rep(0:1, 5), x = 1:10)
  expect_no_error(tt <- table1_tc(d, vars = "x contn", by = "g", title = ""))
  expect_identical(tt$title, "")
})
