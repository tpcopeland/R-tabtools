# table1_tc weighting internals (plan tasks 3.1-3.4). Reference values are
# Stata 17 output of qa/stata/probe_weights.do (summarize [aw=]/[fw=] and
# tabtools' own Mata _t1tcfc_wquantile), printed at %21.17g.

px <- c(2.5, 3.1, 4.8, 1.2, 7.7, 3.1)
pw <- c(0.7, 1.9, 0.2, 2.4, 1.1, 0.6)
pf <- c(1, 3, 2, 1, 4, 2)

test_that("weighted mean and SD match summarize [aw=] (the wt() formula)", {
  s <- tabtools:::.t1w_cont_stats(px, pw, rep(1, 6), "contn", "wt")
  expect_equal(s$a, 3.1608695652173915, tolerance = 1e-14)
  expect_equal(s$b, 2.3863537688320449, tolerance = 1e-14)
  # n counts records under wt(), not the weight total.
  expect_identical(s$n, 6)
  # var = n / (sum(w) (n - 1)) * sum(w (x - mean)^2)
  m <- sum(pw * px) / sum(pw)
  expect_equal(s$b^2, 6 / (sum(pw) * 5) * sum(pw * (px - m)^2), tolerance = 1e-14)
})

test_that("frequency-weighted mean and SD match summarize [fw=]", {
  s <- tabtools:::.t1w_cont_stats(px, pf, pf, "contn", "fw")
  expect_equal(s$a, 4.5846153846153843, tolerance = 1e-14)
  expect_equal(s$b, 2.340529197228038, tolerance = 1e-14)
  expect_identical(s$n, 13)
  # Equal to the unweighted SD of the expanded data.
  expect_equal(s$b, stats::sd(rep(px, pf)), tolerance = 1e-14)
})

test_that("unit weights reproduce the unweighted Phase 2 statistics exactly", {
  set.seed(1)
  for (type in c("contn", "contln", "conts")) {
    x <- c(round(stats::rexp(40) * 10, 1), NA, 0, -1)
    got <- tabtools:::.t1w_cont_stats(x, rep(1, length(x)), rep(1, length(x)), type, "none")
    want <- tabtools:::.t1_cont_stats(x, type)
    want$n <- as.numeric(want$n)
    expect_identical(got, want, info = type)
  }
})

test_that("ESS is (sum w)^2 / sum w^2 per column", {
  expect_equal(tabtools:::.t1w_ess(pw, list(rep(TRUE, 6))), 4.1508282476024387, tolerance = 1e-14)
  ess <- tabtools:::.t1w_ess(pw, list(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), rep(FALSE, 6)))
  expect_equal(ess[1], (0.7 + 1.9)^2 / (0.7^2 + 1.9^2))
  expect_true(is.na(ess[2]))
})

test_that("weighted quantiles match _t1tcfc_wquantile, ties and tolerance included", {
  q <- tabtools:::.t1w_quantile
  u <- rep(1, 6)
  expect_identical(q(px, u, 0.5), 3.1000000000000001)
  expect_identical(q(px, u, 0.25), 2.5)
  expect_identical(q(px, u, 0.75), 4.7999999999999998)
  expect_identical(q(px, pw, 0.5), 3.1000000000000001)
  expect_identical(q(px, pw, 0.25), 1.2)
  expect_identical(q(px, pw, 0.75), 3.1000000000000001)
  # Cumulative weights 0.1, 0.3, 0.6: floating sums that hit p * total only
  # within the 1e-10 tolerance still average with the next value.
  t <- c(1, 2, 3, 4)
  tw <- c(0.1, 0.2, 0.3, 0.4)
  expect_identical(q(t, tw, 0.3), 2.5)
  expect_identical(q(t, tw, 0.6), 3.5)
  expect_identical(q(t, tw, 0.1), 1.5)
  expect_identical(q(t, tw, 1), 4)
  # Zero weights are dropped.
  expect_identical(q(c(5, 1, 9, 3), c(0, 2, 0, 2), 0.5), 2)
  # The tolerance scales with the weight total: 1/3 of 3e6 hits 1e6.
  expect_identical(q(c(10, 20, 30), rep(1e6, 3), 0.5), 20)
  expect_identical(q(c(10, 20, 30), rep(1e6, 3), 1 / 3), 15)
  # Unit weights are quantile(type = 2).
  set.seed(2)
  x <- round(stats::runif(37) * 20)
  for (p in c(0.25, 0.5, 0.75)) {
    expect_equal(q(x, rep(1, 37), p), unname(stats::quantile(x, p, type = 2)))
  }
  expect_true(is.na(q(numeric(), numeric(), 0.5)))
  expect_true(is.na(q(1:3, c(0, 0, 0), 0.5)))
})

test_that("weighted SMDs use aweighted moments, proportions, and weight sums", {
  g <- c(1, 1, 1, 2, 2, 2)
  s <- tabtools:::.t1w_smd("contn", px, g, pw, "wt")
  a <- tabtools:::.t1w_cont_stats(px[1:3], pw[1:3], rep(1, 3), "contn", "wt")
  b <- tabtools:::.t1w_cont_stats(px[4:6], pw[4:6], rep(1, 3), "contn", "wt")
  expect_equal(s, (a$a - b$a) / sqrt((a$b^2 + b$b^2) / 2), tolerance = 1e-12)
  v <- c(1, 0, 1, 0, 0, 1)
  p1 <- sum(pw[1:3] * v[1:3]) / sum(pw[1:3])
  p2 <- sum(pw[4:6] * v[4:6]) / sum(pw[4:6])
  expect_equal(tabtools:::.t1w_smd("bin", v, g, pw, "wt"),
               (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2), tolerance = 1e-12)
  # Frequency weights equal the unweighted SMD of the expanded data.
  idx <- rep(seq_along(px), pf)
  expect_equal(tabtools:::.t1w_smd("contn", px, g, pf, "fw"),
               tabtools:::.t1_smd("contn", px[idx], g[idx], 1, 2), tolerance = 1e-12)
  lev <- c(1, 2, 3, 1, 3, 2)
  expect_equal(tabtools:::.t1w_smd("cat", lev, g, pf, "fw"),
               tabtools:::.t1_smd("cat", lev[idx], g[idx], 1, 2), tolerance = 1e-12)
  # A single record per group has no aweighted SD.
  expect_true(is.na(tabtools:::.t1w_smd("contn", c(1, 2), c(1, 2), c(1, 1), "wt")))
})

test_that("weight validation follows Stata", {
  d <- data.frame(g = c(0, 0, 1, 1, NA), x = c(1, 2, 3, 4, 5), w = c(1, 2, 1, 2, -1), f = c(1, 2, 1, 2, 1))
  # A negative wt() anywhere in the sample is an error, even with by() missing.
  expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "w"), "non-negative")
  d$w[5] <- 1
  expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "nope"), "not found")
  expect_error(table1_tc(transform(d, w = as.character(w)), by = "g", vars = "x contn", wt = "w"),
               "must be numeric")
  d$f[5] <- -1
  expect_error(table1_tc(d, by = "g", vars = "x contn", fweight = "f"), "negative weights")
  d$f <- c(1, 1.5, 1, 1, 1)
  expect_error(table1_tc(d, by = "g", vars = "x contn", fweight = "f"), "noninteger")
  # Stata validates fweights on every record before by() marks anything out
  # (desctab.ado:37, :398; Phase 3 review P2-1, Stata probe C2 on a record
  # with by() missing: r(401)).
  d$f <- c(1, 2, 1, 2, 1.5)
  expect_error(table1_tc(d, by = "g", vars = "x contn", fweight = "f"), "noninteger")
  # ... and on a record whose variable is missing (novarlist; probed, r(401)).
  d2 <- transform(d, f = c(1, 2, 1, 2.5, 1), x = c(1, 2, 3, NA, 5))
  expect_error(table1_tc(d2, by = "g", vars = "x contn", fweight = "f"), "noninteger")
  # A negative weight is reported before a fractional one (probed, r(402)).
  d2$f <- c(1, -1, 1.5, 2, 1)
  expect_error(table1_tc(d2, by = "g", vars = "x contn", fweight = "f"), "negative weights")
  # Tiny fractions count (Stata: 1 + 1e-12 is r(401)).
  d2$f <- c(1 + 1e-12, 2, 1, 2, 1)
  expect_error(table1_tc(d2, by = "g", vars = "x contn", fweight = "f"), "noninteger")
  d$f <- c(1, 2, 1, 2, 1)
  expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "w", fweight = "f"), "cannot be used together")
  expect_error(table1_tc(d, by = "g", vars = "x contn", wtn = TRUE), "requires")
  expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "w", wtn = TRUE, percent = TRUE), "incompatible")
  expect_error(table1_tc(d, vars = "x contn", wt = "w", wtcompare = TRUE), "requires")
  d$w <- 0
  expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "w"), "no observations")
})

test_that("Inf, -Inf and NaN weights are refused with a cli error (Phase 3 review P2-2)", {
  d <- data.frame(g = rep(0:1, each = 4), x = 1:8, w = c(1, 2, 1, 2, 1, 2, 1, 2))
  for (bad in c(Inf, -Inf, NaN)) {
    d$w[1] <- bad
    expect_error(table1_tc(d, by = "g", vars = "x contn", wt = "w", wtn = TRUE), "must be finite",
                 class = "rlang_error")
    expect_error(table1_tc(d, by = "g", vars = "x contn", fweight = "w"), "must be finite",
                 class = "rlang_error")
  }
  # NA still means "leave the sample".
  d$w[1] <- NA
  expect_identical(table1_tc(d, by = "g", vars = "x contn", fweight = "w")$header[[2]]$text[2], "N=5")
  # Finite but huge weights run, as in Stata (probed: wt() 1e300 produces a
  # table; [fweight] 1e15 is in test-table1-fweight-tests.R).
  d$w[1] <- 1e300
  expect_no_error(table1_tc(d, by = "g", vars = "x contn", wt = "w", wtn = TRUE))
})

test_that("zero and missing weights leave the sample; labels survive", {
  d <- data.frame(g = c(0, 0, 0, 1, 1, 1), x = c(1, 2, 3, 2, 1, 3), f = c(1, 2, 0, 3, 1, NA))
  attr(d$x, "label") <- "Level"
  tt <- table1_tc(d, by = "g", vars = "x cat", fweight = "f")
  expect_identical(tt$header[[2]]$text[2:3], c("N=3", "N=4"))
  # Level 3 exists only on zero/missing-weight records, so it is not a level.
  expect_identical(tt$body[[1]], c("Level", "   1", "   2"))
  tt <- table1_tc(d, by = "g", vars = "x cat", wt = "f", wtn = TRUE)
  expect_identical(tt$header[[2]]$text[2:3], c("N=2", "N=2"))
  expect_identical(tt$body[[1]][1], "Effective sample size")
})

test_that("weighted Dapa wording and suppressed p-values (desctab.ado:1366-1372)", {
  d <- data.frame(g = rep(0:1, each = 4), x = 1:8, b = c(0, 1, 0, 1, 1, 1, 0, 1), w = c(1, 2, 1, 2, 1, 2, 1, 2))
  tt <- table1_tc(d, by = "g", vars = "x contn \\ b bin", wt = "w")
  expect_identical(tt$stored$Dapa,
                   "Weighted data are presented as mean\u00b1SD for continuous measures, and % for categorical measures. P-values suppressed.")
  expect_null(tt$stored$methods)
  expect_false(any(tt$cols$role %in% c("p", "test", "statistic")))
  expect_identical(tt$header[[2]]$text[1], "Column % or Mean\u00b1SD")
  tt <- table1_tc(d, by = "g", vars = "x contn \\ b bin", wt = "w", smd = TRUE, test = TRUE)
  expect_match(tt$stored$Dapa, "P-values suppressed\\. SMD reflects weighted comparison\\.$")
  expect_false(any(tt$cols$role == "test"))
  tt <- table1_tc(d, by = "g", vars = "x contn \\ b bin", wt = "w", wtcompare = TRUE)
  expect_identical(tt$stored$Dapa, paste0("Crude and weighted data are presented as mean\u00b1SD for continuous ",
                                          "measures, and No. (%) for categorical measures. P-values suppressed. ",
                                          "SMD reflects weighted comparison."))
  expect_identical(tt$header[[1]]$text, c(" ", "Crude g = 0", "Crude g = 1", "Weighted g = 0", "Weighted g = 1"))
  expect_identical(tt$body[1, 2:5], data.frame(c2 = "", c3 = "", c4 = "ESS=4", c5 = "ESS=4"),
                   ignore_attr = TRUE)
  # fweight keeps p-values and the unweighted wording.
  tt <- table1_tc(d, by = "g", vars = "x contn \\ b bin", fweight = "w")
  expect_identical(tt$stored$Dapa,
                   "Data are presented as mean\u00b1SD for continuous measures, and No. (%) for categorical measures.")
  expect_true(any(tt$cols$role == "p"))
})

test_that("fweight tests equal the tests on the expanded data", {
  d <- data.frame(g = rep(0:1, each = 5), x = c(3, 5, 2, 8, 6, 9, 4, 7, 7, 1), f = c(1, 2, 3, 1, 2, 2, 1, 3, 1, 2))
  e <- d[rep(seq_len(nrow(d)), d$f), ]
  a <- table1_tc(d, by = "g", vars = "x contn", fweight = "f", test = TRUE, statistic = TRUE)
  b <- table1_tc(e, by = "g", vars = "x contn", test = TRUE, statistic = TRUE)
  expect_identical(a$body, b$body)
  expect_identical(a$stored$table, b$stored$table)
})
