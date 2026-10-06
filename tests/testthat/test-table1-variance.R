# B2: SDs are centered (Stata tabtools 2.1.15 F05, _desctab_collect.ado:1893-1901)
# sd of the stored doubles (1e8 + .1 is not exact; ulp 1.5e-8), via exact y - 1e8
true_sd <- sd(1e8 + c(0.1, 0.2, 0.3, 0.4) - 1e8)
stopifnot(abs(true_sd - 0.1290994) < 1e-7)
y <- 1e8 + c(0.1, 0.2, 0.3, 0.4)

test_that("unweighted helper is translation stable", {
  r <- tabtools:::.t1_mean_sd(y)
  expect_lt(abs(r[["sd"]] - true_sd) / true_sd, 1e-10)
  expect_equal(r[["mean"]], mean(y))
})

test_that("weighted helper is translation stable (aw/iw and fw)", {
  n <- length(y)
  a <- tabtools:::.t1w_cont_stats(y, rep(1, n), rep(1, n), "contn", "wt")
  expect_lt(abs(a$b - true_sd) / true_sd, 1e-10)
  a2 <- tabtools:::.t1w_cont_stats(y, rep(2, n), rep(1, n), "contn", "wt")
  expect_lt(abs(a2$b - true_sd) / true_sd, 1e-10)
  f <- tabtools:::.t1w_cont_stats(c(y, y), rep(1, 2 * n), rep(1, 2 * n), "contn", "fw")
  ref <- sd(c(y, y) - 1e8) # fw: n_eff = 2n, same values twice
  expect_lt(abs(f$b - ref) / ref, 1e-10)
})

test_that("overflow semantics are kept", {
  expect_true(is.na(tabtools:::.t1_mean_sd(c(1e154, 2e154, 3e154))[["sd"]]))
  expect_true(is.na(tabtools:::.t1_mean_sd(c(1e200, 2e200, 3e200))[["sd"]]))
  expect_true(is.na(tabtools:::.t1_mean_sd(c(5e307, 5e307, 5e307))[["mean"]]))
})

test_that("table1_tc shows the corrected SD on every sink", {
  d <- data.frame(g = rep(c("A", "B"), each = 4), x = rep(y, 2), w = rep(c(1, 2), 4),
                  f = rep(c(1L, 2L), 4))
  yc <- 1e8 + c(0.1, 0.2, 0.3, 0.4) - 1e8
  w4 <- c(1, 2, 1, 2)
  ssw <- sum(w4 * (yc - sum(w4 * yc) / sum(w4))^2)
  exp_sd <- c(none = sd(yc), wt = sqrt(4 / (sum(w4) * 3) * ssw),
              fw = sqrt(ssw / (sum(w4) - 1)))
  args <- list(none = list(), wt = list(wt = "w"), fw = list(fweight = "f"))
  for (k in names(args)) {
    t1 <- do.call(table1_tc, c(list(d, by = "g", vars = "x contn %9.7f"), args[[k]]))
    df <- as.data.frame(t1)
    want <- sprintf("%.7f", exp_sd[[k]])
    expect_true(any(grepl(want, unlist(df), fixed = TRUE)), info = k)
    expect_false(any(grepl("\u00b10.0000000", unlist(df), fixed = TRUE)), info = k)
  }
})

test_that("Stata 2.5.1 @712044f8 oracle strings for 1e8 + c(.1,.2,.3,.4)", {
  # Strings from stata-mp running table1_tc, vars(x contn %24.15g), by(g),
  # none / wt(w) / fweight(f) on the 8-row data below.
  d <- data.frame(g = rep(1:2, each = 4), x = rep(y, 2), w = rep(c(1, 2), 4),
                  f = rep(c(1L, 2L), 4))
  want <- c(none = "100000000.25\u00b1.129099448721044",
            wt = "100000000.266667\u00b1.127656951332388",
            fw = "100000000.266667\u00b1.121106017609084")
  args <- list(none = list(), wt = list(wt = "w"), fw = list(fweight = "f"))
  for (k in names(args)) {
    t1 <- do.call(table1_tc, c(list(d, by = "g", vars = "x contn %24.15g"), args[[k]]))
    df <- as.data.frame(t1)
    expect_true(want[[k]] %in% unlist(df), info = k)
  }
})

test_that("csv, markdown and as.data.frame show the same corrected SD", {
  d <- data.frame(g = rep(1:2, each = 4), x = rep(y, 2))
  csv <- tempfile(fileext = ".csv"); md <- tempfile(fileext = ".md")
  on.exit(unlink(c(csv, md)))
  t1 <- table1_tc(d, by = "g", vars = "x contn %9.7f", csv = csv, markdown = md)
  want <- "\u00b10.1290994"
  expect_true(any(grepl(want, unlist(as.data.frame(t1)), fixed = TRUE)))
  expect_true(any(grepl(want, readLines(csv, encoding = "UTF-8"), fixed = TRUE)))
  expect_true(any(grepl(want, readLines(md, encoding = "UTF-8"), fixed = TRUE)))
})

test_that("weighted overflow regimes keep Stata semantics", {
  one <- function(y, w) tabtools:::.t1w_cont_stats(y, w, rep(1, length(y)), "contn", "wt")
  # mean (sum w*y) overflows even after rescaling-free weights -> blank mean and SD
  s <- one(c(5e307, 5e307, 5e307), rep(1, 3))
  expect_true(is.na(s$a)); expect_true(is.na(s$b))
  # raw moment sum(w y^2) missing (y^2 out of range): SD missing, mean kept
  s <- one(c(1e160, 2e160, 3e160), rep(1, 3))
  expect_false(is.na(s$a)); expect_true(is.na(s$b))
  # sum(wdev)^2 overflow: Mata ^2 has no extended range -> SD missing
  # (Stata 2.5.1: 1.101299626e+133 +- .)
  yy <- c(2.9026338025909847e133, -1.3002621865103486e133, 1.7015272624644451e133)
  s <- one(yy, rep(1e40, 3))
  expect_equal(s$a, 1.101299626e133, tolerance = 1e-9)
  expect_true(is.na(s$b))
  # huge weights are rescaled (probability weights are scale-free)
  s1 <- one(c(1, 2, 3, 4), rep(1e307, 4))
  s2 <- one(c(1, 2, 3, 4), rep(1, 4))
  expect_equal(s1$b, s2$b)
})
