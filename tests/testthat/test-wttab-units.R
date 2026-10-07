# wttab() (task 7.12): unit and adversarial tests. The Stata goldens are
# W15-W22 (test-golden-wttab.R).

wt_hex <- function(s) {
  # Stata's %21x: "+1.47ae147ae147cX-007", the exponent in hexadecimal too.
  m <- regmatches(s, regexec("^([+-])([0-9a-fA-F.]+)X([+-])([0-9a-fA-F]+)$", s))
  vapply(m, function(p) {
    e <- strtoi(p[5], 16L) * if (p[4] == "-") -1L else 1L
    as.numeric(sprintf("%s0x%sp%d", p[2], p[3], e))
  }, 0)
}

wt_data <- function(n = 400, seed = 1) {
  set.seed(seed)
  d <- data.frame(id = seq_len(n), treat = rbinom(n, 1, 0.4))
  d$w <- exp(rnorm(n, 0, 0.3))
  d
}

wt_body <- function(tt) unname(as.matrix(tt$body))

test_that("percentiles are Stata's _pctile, cell for cell (ties, small n, n p / 100 knife edges)", {
  g <- utils::read.csv(test_path("fixtures", "wttab_pctile", "stata_pctile.csv"), stringsAsFactors = FALSE,
                       colClasses = c(pctile = "character", sumdet = "character"))
  want <- wt_hex(g$pctile)
  expect_false(anyNA(want))
  got <- numeric(nrow(g))
  type2 <- numeric(nrow(g))
  nosnap <- numeric(nrow(g))
  # The rule without the review 7w F1 snap: P = n p / 100 as computed.
  raw <- function(xs, p) {
    n <- length(xs)
    vapply(p, function(pp) {
      P <- n * pp / 100
      i <- floor(P)
      if (P == i) (if (i < 1) xs[1L] else if (i >= n) xs[n] else (xs[i] + xs[i + 1L]) / 2) else xs[min(i + 1L, n)]
    }, 0)
  }
  for (cs in unique(g$case)) {
    k <- g$case == cs
    s <- g[k, ][1, ]
    w <- (seq_len(s$n) * s$a) %% s$m / s$dv
    got[k] <- tabtools:::.wt_pctile(sort(w), g$p[k])
    nosnap[k] <- raw(sort(w), g$p[k])
    type2[k] <- unname(stats::quantile(w, g$p[k] / 100, type = 2))
  }
  expect_identical(got, want)
  # The review's failing sizes, against Stata: w = 1..n.
  one <- function(n, p) want[g$n == n & g$a == 1 & g$p == p]
  expect_identical(one(41000, 99.9), 40959.5)
  expect_identical(one(5250, 0.4), 21.5)
  expect_identical(one(5250, 99.6), 5229.5)
  expect_identical(one(11000, 0.7), 77.5)
  expect_identical(one(11000, 99.3), 10923.5)
  expect_identical(tabtools:::.wt_pctile(as.numeric(1:41000), 99.9), 40959.5)
  # Without the snap, those cells are wrong: the fixture reaches the edge.
  expect_true(any(nosnap != want))
  expect_true(all(c(99.9, 99.6, 0.7, 16.1) %in% g$p[nosnap != want]))
  # R's quantile(type = 2) misses other knife edges (n * 0.07 for n = 100
  # is 7.000000000000001, beyond quantile()'s fuzz of 4 eps).
  expect_true(any(type2 != want))
  expect_true(any(type2 != want & g$p == 7 & g$n == 100))
  # summarize, detail's r(p#) equal _pctile's exactly (review 7w F5).
  sd <- g$sumdet != "."
  expect_gt(sum(sd), 500)
  expect_identical(wt_hex(g$sumdet[sd]), want[sd])
})

test_that("the statistics: mean, SD (n - 1), min/max, percentiles, Kish ESS", {
  d <- wt_data()
  tt <- wttab(d, "w")
  s <- tt$stored$stats
  w <- d$w
  expect_equal(s$n, length(w))
  expect_equal(s$mean, mean(w))
  expect_equal(s$sd, sd(w))
  expect_equal(c(s$min, s$max), range(w))
  expect_equal(c(s$p1, s$p25, s$p50, s$p75, s$p99), unname(quantile(w, c(.01, .25, .5, .75, .99), type = 2)))
  expect_equal(s$ess, sum(w)^2 / sum(w^2))
  expect_equal(s$ess_pct, 100 * s$ess / length(w))
  # One column headed "Weights"; counts whole, weights at digits, ESS at 1.
  expect_identical(tt$header[[1]]$text, c("Statistic", "Weights"))
  b <- wt_body(tt)
  expect_identical(b[, 1], c("N", "Mean", "SD", "Min", "P1", "P25", "Median", "P75", "P99", "Max", "ESS", "ESS (%)"))
  expect_identical(b[1, 2], "400")
  expect_identical(b[2, 2], sprintf("%.2f", mean(w)))
  expect_match(b[11, 2], "^[0-9]+\\.[0-9]$")
  expect_identical(dim(tt$stored$W), c(12L, 1L))
  expect_identical(tt$stored$N, 400L)
  expect_identical(tt$stored$n_dropped, 0L)
})

test_that("by: Overall first, unlabelled 0/1 as Treated then Untreated; stabilization leaves the group ESS alone", {
  d <- wt_data()
  tt <- wttab(d, "w", by = "treat")
  expect_identical(tt$header[[1]]$text, c("Statistic", "Overall", "Treated", "Untreated"))
  s <- tt$stored$stats
  expect_identical(s$n, c(400, sum(d$treat == 1), sum(d$treat == 0)))
  expect_equal(s$ess[2], sum(d$w[d$treat == 1])^2 / sum(d$w[d$treat == 1]^2))
  # Multiplying a group's weights by a constant keeps its ESS.
  d2 <- d
  d2$w[d2$treat == 1] <- d2$w[d2$treat == 1] * 0.37
  expect_equal(wttab(d2, "w", by = "treat")$stored$stats$ess[2:3], s$ess[2:3])
  # overall = FALSE drops the Overall column.
  expect_identical(wttab(d, "w", by = "treat", overall = FALSE)$header[[1]]$text, c("Statistic", "Treated", "Untreated"))
  # Logical treatment: the same names.
  expect_identical(wttab(d$w, by = d$treat == 1)$header[[1]]$text, c("Statistic", "Overall", "Treated", "Untreated"))
  # Factors keep their level order; labelled values show labels; strings sort.
  f <- factor(ifelse(d$treat == 1, "New", "Old"), levels = c("Old", "New"))
  expect_identical(wttab(d$w, by = f)$header[[1]]$text, c("Statistic", "Overall", "Old", "New"))
  skip_if_not_installed("haven")
  lab <- haven::labelled(d$treat, c(SSRI = 0, SNRI = 1))
  expect_identical(wttab(d$w, by = lab)$header[[1]]$text, c("Statistic", "Overall", "SSRI", "SNRI"))
  expect_identical(wttab(d$w, by = ifelse(d$treat == 1, "b", "a"))$header[[1]]$text,
                   c("Statistic", "Overall", "a", "b"))
})

test_that("trunc: pooled cut-offs, blocks, Truncated (n); cut-offs equal to the min or max", {
  d <- wt_data()
  tt <- wttab(d, "w", by = "treat", trunc = list(c(.01, .99), c(.05, .95)))
  h <- tt$header[[1]]$text
  expect_identical(h[1:4], c("Statistic", "Untruncated: Overall", "Untruncated: Treated", "Untruncated: Untreated"))
  expect_identical(h[5], "Truncated 1/99: Overall")
  expect_identical(h[8], "Truncated 5/95: Overall")
  tr <- tt$stored$trunc
  cut <- unname(quantile(d$w, c(.01, .99), type = 2))
  expect_equal(c(tr$lower[1], tr$upper[1]), cut)
  expect_identical(c(tr$n_low[1], tr$n_high[1]), c(sum(d$w < cut[1]), sum(d$w > cut[2])))
  s <- tt$stored$stats
  # The truncated copies: pooled cut-offs applied to each group.
  wt <- pmin(pmax(d$w, cut[1]), cut[2])
  k <- which(s$weights == "Truncated 1/99" & s$group == "Treated")
  expect_equal(s$mean[k], mean(wt[d$treat == 1]))
  expect_equal(s$n_trunc[k], sum((d$w < cut[1] | d$w > cut[2])[d$treat == 1]))
  expect_true(all(is.na(s$n_trunc[s$weights == "Untruncated"])))
  b <- wt_body(tt)
  expect_identical(b[13, 1], "Truncated (n)")
  expect_identical(b[13, 2], "")
  expect_identical(b[13, 5], as.character(tr$n_low[1] + tr$n_high[1]))
  # Without by: blocks named by the truncation alone.
  expect_identical(wttab(d, "w", trunc = c(.01, .99))$header[[1]]$text,
                   c("Statistic", "Untruncated", "Truncated 1/99"))
  # A lower bound of 0 is the minimum: nothing raised. Same for 1 and the max.
  t0 <- wttab(d, "w", trunc = list(c(0, .9), c(.1, 1)))
  expect_identical(t0$stored$trunc$n_low[1], 0L)
  expect_identical(t0$stored$trunc$n_high[2], 0L)
  expect_identical(t0$stored$trunc$lower[1], min(d$w))
  expect_identical(t0$stored$trunc$upper[2], max(d$w))
  expect_identical(t0$header[[1]]$text[3], "Truncated 0/90")
  # Small n: P1 is the minimum and P99 the maximum, so nothing is truncated.
  t5 <- wttab(c(0.5, 1, 1.5, 2, 3), trunc = c(.01, .99))
  expect_identical(t5$stored$trunc$lower, 0.5)
  expect_identical(t5$stored$trunc$upper, 3)
  expect_identical(t5$stored$stats$n_trunc[2], 0)
  # Non-integer percentiles label as typed; 0.07 is 7, not 7.000000000000001.
  expect_identical(wttab(d, "w", trunc = c(.025, .975))$header[[1]]$text[3], "Truncated 2.5/97.5")
  t7 <- wttab(d, "w", trunc = c(.07, .93))
  expect_identical(t7$header[[1]]$text[3], "Truncated 7/93")
  # n = 400, p = 7: n p / 100 = 28 exactly, so Stata averages the 28th and
  # 29th smallest; quantile(type = 2) reads 400 * 0.07 as just above 28.
  xs <- sort(d$w)
  expect_identical(t7$stored$trunc$lower, (xs[28] + xs[29]) / 2)
  expect_false(identical(t7$stored$trunc$lower, unname(quantile(d$w, .07, type = 2))))
})

test_that("period: long layout, one row per period x truncation x group, repeated keys blank", {
  pp <- data.frame(id = rep(1:50, each = 3), period = rep(1:3, 50))
  set.seed(3)
  pp$A <- rbinom(150, 1, 0.5)
  pp$sw <- exp(rnorm(150, 0, 0.2))
  attr(pp$period, "label") <- "Follow-up year"
  tt <- wttab(pp, "sw", period = "period", by = "A", trunc = c(.05, .95))
  h <- tt$header[[1]]$text
  expect_identical(h[1:3], c("Follow-up year", "Weights", "A"))
  expect_identical(h[4:6], c("N", "Mean", "SD"))
  expect_identical(h[length(h)], "Truncated (n)")
  b <- wt_body(tt)
  expect_identical(nrow(b), 3L * 2L * 3L)
  expect_identical(b[1:7, 1], c("1", "", "", "", "", "", "2"))
  expect_identical(b[1:4, 2], c("Untruncated", "", "", "Truncated 5/95"))
  expect_identical(b[1:3, 3], c("Overall", "Treated", "Untreated"))
  # A new period shows every key again.
  expect_identical(b[7, 1:3], c("2", "Untruncated", "Overall"))
  expect_identical(dim(tt$stored$W), c(18L, 13L))
  expect_identical(rownames(tt$stored$W)[1], "1, Untruncated, Overall")
  expect_equal(tt$stored$W[2, "Mean"], mean(pp$sw[pp$period == 1 & pp$A == 1]))
  # The cut-offs are pooled over every period.
  expect_equal(tt$stored$trunc$lower, unname(quantile(pp$sw, .05, type = 2)))
  # Wide is refused with a period; long is allowed without one.
  expect_error(wttab(pp, "sw", period = "period", layout = "wide"), "layout = \"long\"")
  tl <- wttab(pp, "sw", by = "A", layout = "long")
  expect_identical(tl$header[[1]]$text[1:2], c("A", "N"))
  expect_identical(wt_body(wttab(pp, "sw", layout = "long"))[, 1], "All")
  # Unlabelled period: headed by its name.
  pp2 <- pp
  attr(pp2$period, "label") <- NULL
  expect_identical(wttab(pp2, "sw", period = "period")$header[[1]]$text[1], "period")
  expect_identical(wttab(pp2$sw, period = pp2$period)$header[[1]]$text[1], "Period")
})

test_that("a period with no treated has an N = 0 row with blank statistics", {
  pp <- data.frame(period = rep(1:2, each = 4), A = c(0, 0, 0, 0, 1, 0, 1, 0), w = c(1, 2, 3, 4, 1, 1, 2, 5))
  tt <- wttab(pp, "w", period = "period", by = "A")
  b <- wt_body(tt)
  row <- which(b[, 1] == "1") + 1L
  expect_identical(b[row, 2], "Treated")
  expect_identical(b[row, 3], "0")
  expect_true(all(b[row, -(1:3)] == ""))
  expect_true(all(is.na(tt$stored$W[row, -1])))
})

test_that("adversarial weights: zero, negative, missing, one observation, all equal", {
  # Zero weights count in N and lower the mean and the ESS.
  t0 <- wttab(c(0, 0, 1, 1))
  expect_identical(t0$stored$stats$n, 4)
  expect_equal(t0$stored$stats$ess, 2)
  expect_equal(t0$stored$stats$ess_pct, 50)
  # All zero: no ESS (blank), no error.
  tz <- wttab(c(0, 0, 0))
  expect_true(is.na(tz$stored$stats$ess))
  expect_identical(wt_body(tz)[11:12, 2], c("", ""))
  # Negative and infinite weights are refused; NA dropped with a warning.
  expect_error(wttab(c(1, -1, 2)), "must not be negative")
  expect_error(wttab(c(1, Inf)), "must be finite")
  expect_warning(tn <- wttab(c(1, NA, 2)), "Dropped 1 observation")
  expect_identical(tn$stored$N, 2L)
  expect_identical(tn$stored$n_dropped, 1L)
  # Review 7w F14: "weight or `by`", "weight, `by`, or `period`".
  expect_warning(tb <- wttab(c(1, 2, 3), by = c(1, NA, 0)), "missing weight or `by` value", fixed = TRUE)
  expect_warning(wttab(c(1, 2, 3), by = c(1, NA, 0), period = c(1, 1, 2)), "missing weight, `by`, or `period` value",
                 fixed = TRUE)
  expect_warning(wttab(c(1, 2, 3), period = c(1, NA, 2)), "missing weight or `period` value", fixed = TRUE)
  expect_warning(wttab(c(1, NA, 3)), "missing weight value", fixed = TRUE)
  expect_identical(tb$stored$stats$n, c(2, 1, 1))
  expect_error(suppressWarnings(wttab(c(NA_real_, NA_real_))), "No observation")
  # One observation: no SD (blank); percentiles are the value; ESS 1.
  t1 <- wttab(2.5)
  s <- t1$stored$stats
  expect_true(is.na(s$sd))
  expect_identical(c(s$min, s$p1, s$p50, s$p99, s$max), rep(2.5, 5))
  expect_identical(s$ess, 1)
  expect_identical(wt_body(t1)[3, 2], "")
  # All equal: SD 0, ESS = N, ESS (%) 100.
  te <- wttab(rep(1.2, 10), trunc = c(.01, .99))
  expect_identical(te$stored$stats$sd[1], 0)
  expect_equal(te$stored$stats$ess, c(10, 10))
  expect_identical(wt_body(te)[12, 2], "100.0")
  expect_identical(te$stored$trunc$n_low + te$stored$trunc$n_high, 0L)
})

test_that("inputs: data frame, vector + data, ipw objects (duck-typed), argument errors", {
  d <- wt_data()
  ref <- wttab(d, "w", by = "treat")
  expect_identical(wttab(d$w, by = d$treat)$body, ref$body)
  expect_identical(wttab(d$w, by = "treat", data = d)$body, ref$body)
  # ipwpoint()/ipwtm() return plain lists with $ipw.weights.
  ipw_like <- list(ipw.weights = d$w, call = quote(ipwpoint()), num.mod = NULL, den.mod = NULL)
  ti <- wttab(ipw_like, by = "treat", data = d)
  expect_identical(ti$body, ref$body)
  expect_identical(ti$meta$wttab$source, "ipw")
  expect_error(wttab(ipw_like, by = "treat"), "no data frame to look it up")
  expect_error(wttab(d, "nope"), "not found")
  expect_error(wttab(d), "must name its weight column")
  expect_error(wttab(d$w, "w"), "names a column of a data frame")
  expect_error(wttab(d, "w", data = d), "already a data frame")
  expect_error(wttab(d$w, by = d$treat[-1]), "has 399 values")
  expect_error(wttab(list(a = 1)), "must be a data frame")
  expect_error(wttab(letters), "must be a data frame")
  d$chr <- "a"
  expect_error(wttab(d, "chr"), "must be numeric")
  expect_error(wttab(d, "w", trunc = c(1, 99)), "Give proportions")
  expect_error(wttab(d, "w", trunc = c(.99, .01)), "lower < upper")
  expect_error(wttab(d, "w", trunc = list(c(.01, .99), .5)), "lower < upper")
  expect_error(wttab(d, "w", layout = "tall"), "should be one of")
  expect_error(wttab(d, "w", digits = 7), "between 0 and 6")
  expect_error(wttab(d, "w", overall = NA), "TRUE or FALSE")
  expect_error(wttab(d, "w", markdown = "x.txt"), "\\.md")
})

test_that("WeightIt objects: grouped by $treat; ESS as WeightIt's summary; multi-category treatment", {
  skip_if_not_installed("WeightIt")
  set.seed(7)
  n <- 300
  d <- data.frame(x1 = rnorm(n), x2 = rbinom(n, 1, 0.5))
  d$t2 <- rbinom(n, 1, plogis(0.5 * d$x1 - 0.3 * d$x2))
  d$t3 <- factor(sample(c("A", "B", "C"), n, TRUE, c(0.5, 0.3, 0.2)))
  W <- WeightIt::weightit(t2 ~ x1 + x2, data = d, estimand = "ATE")
  tt <- wttab(W)
  expect_identical(tt$header[[1]]$text, c("Statistic", "Overall", "Treated", "Untreated"))
  expect_identical(tt$meta$wttab$source, "weightit")
  ess <- summary(W)$effective.sample.size
  expect_equal(unname(tt$stored$stats$ess[2:3]), unlist(ess["Weighted", c("Treated", "Control")], use.names = FALSE))
  # by overrides the treatment grouping.
  expect_identical(wttab(W, by = d$x2)$header[[1]]$text, c("Statistic", "Overall", "Treated", "Untreated"))
  W3 <- WeightIt::weightit(t3 ~ x1 + x2, data = d, estimand = "ATE")
  t3 <- wttab(W3, digits = 3)
  expect_identical(t3$header[[1]]$text, c("Statistic", "Overall", "A", "B", "C"))
  expect_identical(t3$stored$stats$n, c(n, as.numeric(table(d$t3))))
  ess3 <- summary(W3)$effective.sample.size
  expect_equal(unname(t3$stored$stats$ess[2:4]), unlist(ess3["Weighted", c("A", "B", "C")], use.names = FALSE))
  # A continuous treatment is not a grouping.
  d$tc <- d$x1 + rnorm(n)
  Wc <- suppressWarnings(WeightIt::weightit(tc ~ x2, data = d))
  expect_identical(wttab(Wc)$header[[1]]$text, c("Statistic", "Weights"))
})

test_that("sinks, footnote, digits, and the converters", {
  d <- wt_data()
  tt <- wttab(d, "w", by = "treat", trunc = c(.01, .99), digits = 3)
  expect_match(tt$footnote, "^ESS = effective sample size")
  expect_match(tt$footnote, "Truncated l/u")
  expect_identical(wttab(d, "w", footnote = "")$footnote, "")
  expect_identical(wttab(d, "w", footnote = "Own.")$footnote, "Own.")
  expect_match(wt_body(tt)[2, 2], "^[0-9]\\.[0-9]{3}$")
  withr::local_options(tabtools.digits = 4)
  expect_match(wt_body(wttab(d, "w"))[2, 2], "^[0-9]\\.[0-9]{4}$")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "w.xlsx")
  msgs <- character()
  withCallingHandlers(
    out <- wttab(d, "w", by = "treat", xlsx = path, csv = file.path(dir, "w.csv"), markdown = file.path(dir, "w.md")),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_match(msgs[1], "^Markdown exported to .*w\\.md")
  expect_match(msgs[2], "^wttab: wrote 12 rows x 4 cols to sheet Weights in .*w\\.xlsx")
  expect_true(file.exists(path))
  expect_identical(out$stored$sheet, "Weights")
  expect_identical(out$stored$markdown_rows, 12L)
  expect_identical(readLines(file.path(dir, "w.csv"))[1], "Statistic,Overall,Treated,Untreated")
  skip_if_not_installed("openxlsx2")
  wb <- openxlsx2::wb_to_df(path, sheet = "Weights", col_names = FALSE)
  expect_identical(unname(unlist(wb[2, 2:5])), c("Statistic", "Overall", "Treated", "Untreated"))
  # print and as.data.frame
  expect_output(print(tt), "Truncated \\(n\\)")
  expect_identical(nrow(as.data.frame(tt)), 14L)
  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(tt)
  expect_s3_class(ft, "flextable")
  skip_if_not_installed("gt")
  expect_s3_class(tt_as_gt(tt), "gt_tbl")
})

test_that("more adversarial inputs: one by level, blank groups, Date periods, huge weights, digits 0, factor levels", {
  expect_identical(wttab(rep(1, 3), by = c(1, 1, 1))$header[[1]]$text, c("Statistic", "Overall", "Treated"))
  # Review 7w F10: a blank group is named; a group called Overall is refused.
  expect_identical(wttab(c(1, 2, 3), by = c("x", "", "x"))$header[[1]]$text, c("Statistic", "Overall", "(blank)", "x"))
  expect_error(wttab(c(1, 2, 3), by = c("Overall", "x", "x")), "called \"Overall\"")
  expect_identical(wttab(c(1, 2, 3), by = c("Overall", "x", "x"), overall = FALSE)$header[[1]]$text,
                   c("Statistic", "Overall", "x"))
  d <- data.frame(w = c(1, 2, 3, 4), dt = as.Date("2020-01-01") + c(0, 1, 0, 1))
  expect_identical(wt_body(wttab(d, "w", period = "dt"))[, 1], c("01jan2020", "02jan2020"))
  big <- wt_body(wttab(c(1, 1e20)))
  expect_identical(big[10, 2], "100,000,000,000,000,000,000.00")
  expect_identical(big[1, 2], "2")
  expect_identical(wt_body(wttab(c(1.4, 2.6), digits = 0))[2, 2], "2")
  f <- factor(c("a", NA, "b", "a"), levels = c("a", "b", "z"))
  expect_warning(tf <- wttab(c(1, 2, 3, 4), by = f), "Dropped 1")
  expect_identical(tf$header[[1]]$text, c("Statistic", "Overall", "a", "b"))
  # Labelled periods show their labels, in code order.
  skip_if_not_installed("haven")
  per <- haven::labelled(c(2, 1, 2, 1), c(Baseline = 1, `Year 1` = 2))
  expect_identical(wt_body(wttab(c(1, 2, 3, 4), period = per))[, 1], c("Baseline", "Year 1"))
})


test_that("review 7w F2: WeightIt sampling weights are multiplied in, as summary.weightit() and cobalt", {
  skip_if_not_installed("WeightIt")
  set.seed(1)
  n <- 600
  d <- data.frame(x1 = rnorm(n), x2 = rbinom(n, 1, 0.4))
  d$t <- rbinom(n, 1, plogis(0.5 * d$x1 - 0.3 * d$x2))
  d$s <- runif(n, 0.5, 2)
  W <- WeightIt::weightit(t ~ x1 + x2, data = d, s.weights = "s")
  tt <- wttab(W)
  e <- summary(W)$effective.sample.size
  expect_equal(tt$stored$stats$ess[2:3], unlist(e["Weighted", c("Treated", "Control")], use.names = FALSE))
  expect_equal(tt$stored$stats$mean[1], mean(W$weights * d$s))
  expect_true(tt$stored$s_weights)
  expect_match(tt$footnote, "sampling weights \\(s.weights\\)")
  # s.weights = FALSE describes $weights alone.
  t0 <- wttab(W, s.weights = FALSE)
  expect_equal(t0$stored$stats$mean[1], mean(W$weights))
  expect_false(t0$stored$s_weights)
  expect_no_match(t0$footnote, "s.weights")
  # Without sampling weights nothing changes and the footnote says nothing.
  W1 <- WeightIt::weightit(t ~ x1 + x2, data = d)
  expect_false(wttab(W1)$stored$s_weights)
  expect_no_match(wttab(W1)$footnote, "s.weights")
})

test_that("review 7w F3: non-integer by and period levels use digits, as puttab's rule", {
  tt <- wttab(c(1, 2, 3, 4), by = c(0.5, 1.25, 0.5, 1.25), digits = 3)
  expect_identical(tt$header[[1]]$text, c("Statistic", "Overall", "0.500", "1.250"))
  tp <- wttab(c(1, 2, 3, 4), period = c(0.5, 1.25, 0.5, 1.25), digits = 1)
  expect_identical(wt_body(tp)[, 1], c("0.5", "1.2"))
  # Whole numbers stay whole.
  expect_identical(wttab(c(1, 2), by = c(2, 3), digits = 3)$header[[1]]$text, c("Statistic", "Overall", "2", "3"))
})

test_that("review 7w F6: WeightIt's focal level is Treated (ATT, focal = 0); labels survive dropping NA rows", {
  skip_if_not_installed("WeightIt")
  set.seed(4)
  n <- 500
  d <- data.frame(x = rnorm(n))
  d$t <- rbinom(n, 1, plogis(0.4 * d$x))
  W <- WeightIt::weightit(t ~ x, data = d, estimand = "ATT", focal = 0)
  expect_identical(attr(W$treat, "treated"), 0)
  tt <- wttab(W)
  expect_identical(tt$header[[1]]$text, c("Statistic", "Overall", "Treated", "Untreated"))
  s <- tt$stored$stats
  expect_identical(s$n[2], as.numeric(sum(d$t == 0)))
  expect_identical(s$sd[2], 0)
  expect_identical(c(s$min[2], s$max[2]), c(1, 1))
  # Labelled by with a missing value: labels and the variable label kept.
  skip_if_not_installed("haven")
  dd <- data.frame(w = c(1, 2, 3, 4, 5))
  dd$dose <- haven::labelled(c(1, 2, NA, 1, 2), c(Low = 1, High = 2), label = "Dose")
  expect_warning(tl <- wttab(dd, "w", by = "dose", layout = "long"), "Dropped 1")
  expect_identical(tl$header[[1]]$text[1], "Dose")
  expect_identical(wt_body(tl)[, 1], c("Overall", "Low", "High"))
  # A plain numeric vector with a "labels" attribute (no haven class, so
  # `[` drops it) and a Date period with its own Stata format keep both.
  plain <- structure(c(1, 2, NA, 1, 2), labels = c(Low = 1, High = 2))
  expect_warning(tp <- wttab(c(1, 2, 3, 4, 5), by = plain), "Dropped 1")
  expect_identical(tp$header[[1]]$text, c("Statistic", "Overall", "Low", "High"))
  dt <- structure(as.Date("2020-01-01") + c(0, 31, 0, NA), format.stata = "%tdCCYY-NN-DD")
  expect_warning(tdt <- wttab(c(1, 2, 3, 4), period = dt), "Dropped 1")
  expect_identical(wt_body(tdt)[, 1], c("2020-01-01", "2020-02-01"))
})

test_that("review 7w F8/F9: by_labels names the groups; a factor treatment puts WeightIt's treated level first", {
  d <- data.frame(w = c(1, 2, 3, 4), female = c(0, 1, 0, 1))
  tt <- wttab(d, "w", by = "female", by_labels = c("0" = "Men", "1" = "Women"))
  expect_identical(tt$header[[1]]$text, c("Statistic", "Overall", "Men", "Women"))
  # Unnamed values keep their own label; unknown names are refused.
  expect_identical(wttab(d, "w", by = "female", by_labels = c("1" = "Women"))$header[[1]]$text,
                   c("Statistic", "Overall", "0", "Women"))
  expect_error(wttab(d, "w", by = "female", by_labels = c("2" = "x")), "not a value")
  expect_error(wttab(d, "w", by_labels = c("0" = "x")), "needs")
  expect_error(wttab(d, "w", by = "female", by_labels = c("Men", "Women")), "named character vector")
  f <- factor(c("a", "b", "a", "b"))
  expect_identical(wttab(d$w, by = f, by_labels = c(b = "B"))$header[[1]]$text, c("Statistic", "Overall", "a", "B"))
  skip_if_not_installed("WeightIt")
  set.seed(5)
  n <- 400
  dw <- data.frame(x = rnorm(n))
  dw$arm <- factor(ifelse(rbinom(n, 1, plogis(0.3 * dw$x)) == 1, "drug", "control"), levels = c("control", "drug"))
  W <- WeightIt::weightit(arm ~ x, data = dw, estimand = "ATT", focal = "drug")
  expect_identical(wttab(W)$header[[1]]$text, c("Statistic", "Overall", "drug", "control"))
})

test_that("review 7w F11: trunc_by = \"period\" truncates within each period; pooled stays the default", {
  set.seed(6)
  pp <- data.frame(period = rep(1:3, each = 200))
  pp$sw <- exp(rnorm(600, 0, 0.1 * pp$period))
  tp <- wttab(pp, "sw", period = "period", trunc = c(0.05, 0.95), trunc_by = "period")
  tr <- tp$stored$trunc
  expect_identical(tr$period, c("1", "2", "3"))
  for (k in 1:3) {
    wk <- pp$sw[pp$period == k]
    b <- tabtools:::.wt_pctile(sort(wk), c(5, 95))
    expect_identical(c(tr$lower[k], tr$upper[k]), b)
    expect_identical(tr$n_low[k] + tr$n_high[k], sum(wk < b[1] | wk > b[2]))
    s <- tp$stored$stats
    r <- which(s$period == as.character(k) & s$weights == "Truncated 5/95")
    expect_equal(s$max[r], b[2])
    expect_equal(s$mean[r], mean(pmin(pmax(wk, b[1]), b[2])))
  }
  expect_match(tp$footnote, "of the weights in the same period")
  # Pooled: one cut-off pair, no period column.
  tq <- wttab(pp, "sw", period = "period", trunc = c(0.05, 0.95))
  expect_null(tq$stored$trunc$period)
  expect_identical(nrow(tq$stored$trunc), 1L)
  expect_match(tq$footnote, "of all weights")
  expect_error(wttab(pp, "sw", trunc = c(0.05, 0.95), trunc_by = "period"), "needs `period`")
  expect_error(wttab(pp, "sw", trunc_by = "x"), "should be one of")
})

test_that("W01: explicit sheet without workbook gives a classed warning", {
  expect_warning(wttab(wt_data(), "w", sheet = "Zed"), class = "tabtools_warning_sheet_without_workbook")
})

test_that("P2-2: sheet = NULL means the default sheet", {
  expect_no_error(wttab(wt_data(), "w", sheet = NULL))
})
