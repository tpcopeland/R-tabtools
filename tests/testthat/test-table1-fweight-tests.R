# fweight hypothesis tests without expanding the data (Phase 3 review
# P2-3). The contract: every test under fweight equals R's own test on the
# data expanded by the frequencies (the §2 test map on the expanded data),
# to 1e-10, with the same test label and statistic text. The aggregated
# forms are in R/table1_weights.R (.t1_ttest_fw, .t1_oneway_fw,
# .t1_wilcox_fw, .t1_kruskal_fw, .t1_fw_table).

fw_expand_test <- function(type, v, g, fw, test_args = NULL, include_missing = FALSE) {
  i <- rep.int(seq_along(v), fw)
  list(agg = tabtools:::.t1w_test_fw(type, v, g, fw, include_missing, test_args),
       exp = tabtools:::.t1_test(type, v[i], g[i], include_missing, test_args))
}

expect_fw_equal <- function(type, v, g, fw, test_args = NULL, include_missing = FALSE, label = type) {
  r <- fw_expand_test(type, v, g, fw, test_args, include_missing)
  expect_false(is.na(r$exp$p), label = paste(label, "expanded test ran"))
  expect_equal(r$agg$p, r$exp$p, tolerance = 1e-10, label = paste(label, "p"))
  expect_identical(r$agg$test, r$exp$test, label = paste(label, "test"))
  expect_identical(r$agg$statistic, r$exp$statistic, label = paste(label, "statistic"))
  expect_identical(r$agg$used, r$exp$used, label = paste(label, "used"))
  invisible(r)
}

# Continuous values with (rounded) or without ties among the records;
# frequencies 1-5, so the expanded data always have ties unless every
# frequency is 1.
fw_grid_data <- function(G, ties, n = 90, seed = 1) {
  withr::with_seed(seed, {
    g <- rep_len(seq_len(G), n)
    x <- rnorm(n, mean = g / 3)
    if (ties) x <- round(x, 1)
    list(g = g, x = x, pos = exp(x), fw = sample(1:5, n, TRUE),
         cat = sample(1:4, n, TRUE), bin = rbinom(n, 1, 0.4),
         catm = sample(c(1:3, NA), n, TRUE))
  })
}

test_that("fweight tests equal R's tests on the expanded data: every type, 2 and 3 groups, ties or not", {
  for (G in 2:3) {
    for (ties in c(FALSE, TRUE)) {
      d <- fw_grid_data(G, ties, seed = G * 10 + ties)
      lab <- function(t) sprintf("%s G=%d ties=%s", t, G, ties)
      expect_fw_equal("contn", d$x, d$g, d$fw, label = lab("contn"))
      expect_fw_equal("contln", d$pos, d$g, d$fw, label = lab("contln"))
      # 90 records x frequencies 1-5: every group has >= 50 expanded
      # observations, so the Wilcoxon runs its normal approximation.
      expect_fw_equal("conts", d$x, d$g, d$fw, label = lab("conts"))
      expect_fw_equal("cat", d$cat, d$g, d$fw, label = lab("cat"))
      expect_fw_equal("cat", d$catm, d$g, d$fw, include_missing = TRUE, label = lab("cat missing"))
      expect_fw_equal("bin", d$bin, d$g, d$fw, label = lab("bin"))
      small <- seq_len(24)
      expect_fw_equal("cate", d$cat[small], d$g[small], pmin(d$fw[small], 2), label = lab("cate"))
      expect_fw_equal("bine", d$bin[small], d$g[small], d$fw[small], label = lab("bine"))
    }
  }
})

test_that("frequencies of 1 are the expanded data: no ties, R's exact Wilcoxon", {
  d <- fw_grid_data(2, FALSE, n = 30, seed = 5)
  r <- expect_fw_equal("conts", d$x, d$g, rep(1, 30), label = "conts all-1")
  expect_identical(r$agg$p, stats::wilcox.test(d$x[d$g == 1], d$x[d$g == 2])$p.value)
})

test_that("small groups take R's exact Wilcoxon on the (small) expanded data", {
  g <- rep(1:2, each = 6)
  fw <- c(1, 2, 1, 1, 3, 1, 2, 1, 1, 1, 1, 2)
  x <- c(1:6, 3:8 + 0.5)
  expect_fw_equal("conts", x, g, fw, label = "exact branch")
  expect_fw_equal("conts", x, g, fw, test_args = list(wilcox.test = list(correct = FALSE)), label = "exact, no cc")
})

test_that("test_args reach the aggregated forms", {
  d <- fw_grid_data(2, TRUE, seed = 7)
  ta <- list(
    list(t.test = list(var.equal = TRUE)), list(t.test = list(mu = 0.25, conf.level = 0.9)),
    list(wilcox.test = list(correct = FALSE)), list(wilcox.test = list(exact = FALSE, mu = 0.1)),
    list(wilcox.test = list(digits.rank = 1)), list(wilcox.test = list(conf.int = TRUE))
  )
  for (a in ta) {
    type <- if (names(a) == "t.test") "contn" else "conts"
    expect_fw_equal(type, d$x, d$g, d$fw, test_args = a, label = paste(deparse(a), collapse = ""))
  }
  d3 <- fw_grid_data(3, TRUE, seed = 8)
  expect_fw_equal("contn", d3$x, d3$g, d3$fw, test_args = list(oneway.test = list(var.equal = TRUE)),
                  label = "pooled ANOVA")
  expect_fw_equal("cat", d$cat, d$g, d$fw, test_args = list(chisq.test = list(simulate.p.value = TRUE, B = 500)),
                  label = "simulated chi-squared")
  expect_fw_equal("bin", d$bin, d$g, d$fw, test_args = list(chisq.test = list(correct = TRUE)),
                  label = "Yates")
})

test_that("fallbacks and data failures match the expanded data", {
  # One record (frequency 1) in a group: Welch fails, the pooled t runs.
  expect_fw_equal("contn", c(1, 2, 3, 4), c(1, 2, 2, 2), c(1, 2, 1, 3), label = "pooled fallback")
  # A group of one record with frequency 3 has zero variance: Welch ANOVA
  # gives NaN, the pooled ANOVA runs.
  expect_fw_equal("contn", c(5, 1, 2, 3, 4, 9, 8), c(1, 2, 2, 2, 3, 3, 3), c(3, 1, 2, 1, 1, 2, 2),
                  label = "zero-variance group")
  # Constant data: every test leaves p blank, aggregated or expanded.
  r <- fw_expand_test("contn", rep(2, 6), rep(1:2, 3), c(1, 2, 3, 1, 2, 3))
  expect_true(is.na(r$agg$p) && is.na(r$exp$p))
})

test_that("table1_tc under fweight equals the expanded table: cat, bin, contn, contln, conts, 2 and 3 groups", {
  for (G in 2:3) {
    d <- fw_grid_data(G, TRUE, n = 120, seed = 30 + G)
    df <- data.frame(g = d$g, x = d$x, p = d$pos, y = d$x, c = d$catm, b = d$bin, f = d$fw)
    e <- df[rep(seq_len(nrow(df)), df$f), ]
    vars <- "x contn \\ p contln \\ y conts \\ c cat \\ b bin"
    a <- table1_tc(df, by = "g", vars = vars, fweight = "f", test = TRUE, statistic = TRUE, missing = TRUE)
    b <- table1_tc(e, by = "g", vars = vars, test = TRUE, statistic = TRUE, missing = TRUE)
    expect_identical(a$body, b$body)
    expect_equal(a$stored$table, b$stored$table, tolerance = 1e-10)
    expect_identical(a$stored$methods, b$stored$methods)
  }
})

test_that("huge total frequencies run without expanding; expansion is capped with a pointer to nopvalue", {
  # One record of frequency 1e15 (Stata runs it too, N=1.00000e+15).
  d1 <- data.frame(g = c(0, 0, 1, 1), x = c(1, 2, 3, 4), f = c(1e15, 2, 1, 2))
  tt <- table1_tc(d1, by = "g", vars = "x contn \\ x conts", fweight = "f")
  expect_identical(tt$header[[2]]$text[2], "N=1.00000e+15")
  expect_false(anyNA(tt$rows$p[tt$rows$vtype %in% c("contn", "conts")]))
  # Frequencies repeat across the groups, so each pair has equal frequencies
  # (a paired fweight test needs them; review R3).
  d <- data.frame(g = rep(1:2, each = 30), x = round(sin(1:60) * 10), f = 1e6 + rep((1:30) %% 7, 2))
  expect_gt(sum(d$f), 5e7)
  tt <- table1_tc(d, by = "g", vars = "x contn \\ x conts \\ x cat", fweight = "f")
  expect_true(all(nzchar(tt$body[[4]][tt$rows$type != "level" & tt$rows$vtype %in% c("contn", "conts", "cat")])))
  # Only the cases R computes on the expanded data need it; beyond 1e7
  # records they are refused with a classed error naming nopvalue.
  expect_error(table1_tc(d, by = "g", vars = "x conts", fweight = "f",
                         test_args = list(wilcox.test = list(exact = TRUE))),
               "nopvalue", class = "tabtools_fw_limit")
  expect_error(table1_tc(d, by = "g", vars = "x contn", fweight = "f",
                         test_args = list(t.test = list(paired = TRUE))),
               "nopvalue", class = "tabtools_fw_limit")
  expect_no_error(table1_tc(d, by = "g", vars = "x conts", fweight = "f", nopvalue = TRUE,
                            test_args = list(wilcox.test = list(exact = TRUE))))
})
