# Milestone H hardening of table1_tc (IMPLEMENTATION_PLAN.md, tasks H7,
# H10, H17; findings F08, F11 and user item (a) in
# POTENTIAL_ISSUES_2026-09-26.md).

p_cell <- function(tt, row = 1L) tt$body[[which(tt$cols$role == "p")]][row]
col_of <- function(tt, role) tt$body[[which(tt$cols$role == role)]]

# H7 (F08): paired tests ------------------------------------------------------

test_that("H7: a paired t-test is labelled and described as paired", {
  d <- data.frame(x = c(1, 2, 4, 7, 2, 4, 5, 9), g = rep(c("a", "b"), each = 4))
  tt <- table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE, statistic = TRUE,
                  test_args = list(t.test = list(paired = TRUE)))
  ref <- stats::t.test(c(1, 2, 4, 7), c(2, 4, 5, 9), paired = TRUE)
  expect_equal(tt$rows$p[tt$rows$var %in% "x"][1], ref$p.value)
  expect_identical(p_cell(tt, 1), "0.014")
  expect_identical(col_of(tt, "test")[1], "Paired t test")
  expect_identical(col_of(tt, "statistic")[1], sprintf("t(3)=%6.2f", ref$statistic))
  expect_match(tt$stored$methods, "calculated using paired t-test.", fixed = TRUE)
  expect_no_match(tt$stored$methods, "Welch")
})

test_that("H7: a paired contln test runs on the log scale and says so", {
  d <- data.frame(x = c(1, 2, 4, 7, 2, 4, 5, 9), g = rep(1:2, each = 4))
  tt <- table1_tc(d, vars = c(x = "contln"), by = "g", test = TRUE,
                  test_args = list(t.test = list(paired = TRUE)))
  expect_identical(col_of(tt, "test")[1], "Paired t test, logged data")
  expect_equal(tt$rows$p[1], stats::t.test(log(c(1, 2, 4, 7)), log(c(2, 4, 5, 9)), paired = TRUE)$p.value)
})

test_that("H7: a paired Wilcoxon test is the signed-rank test", {
  d <- data.frame(x = c(1, 2, 4, 7, 11, 2, 4, 5, 9, 10), g = rep(c("a", "b"), each = 5))
  tt <- table1_tc(d, vars = c(x = "conts"), by = "g", test = TRUE, statistic = TRUE,
                  test_args = list(wilcox.test = list(paired = TRUE)))
  ref <- suppressWarnings(stats::wilcox.test(c(1, 2, 4, 7, 11), c(2, 4, 5, 9, 10), paired = TRUE))
  expect_equal(tt$rows$p[1], ref$p.value)
  expect_identical(col_of(tt, "test")[1], "Wilcoxon signed-rank")
  expect_identical(col_of(tt, "statistic")[1], paste0("V=", ref$statistic))
  expect_match(tt$stored$methods, "calculated using Wilcoxon signed-rank test.", fixed = TRUE)
  expect_no_match(tt$stored$methods, "rank-sum")
})

test_that("H7: pairs follow row order within the groups; a pair with a missing value is dropped", {
  d <- data.frame(x = c(1, NA, 4, 7, 2, 4, NA, 9), g = rep(c("a", "b"), each = 4))
  tt <- table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE,
                  test_args = list(t.test = list(paired = TRUE)))
  # Pairs (1, 2) and (7, 9): not (1, 2), (4, 4), (7, 9) as a per-group NA drop would give.
  expect_equal(tt$rows$p[1], stats::t.test(c(1, NA, 4, 7), c(2, 4, NA, 9), paired = TRUE)$p.value)
  # Interleaved groups pair the i-th record of each group.
  e <- d[c(1, 5, 2, 6, 3, 7, 4, 8), ]
  expect_identical(table1_tc(e, vars = c(x = "contn"), by = "g", test = TRUE,
                             test_args = list(t.test = list(paired = TRUE)))$rows$p, tt$rows$p)
})

test_that("H7: unequal groups cannot be paired", {
  d <- data.frame(x = c(1, 2, 4, 7, 2, 4, 5), g = c(rep("a", 4), rep("b", 3)))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(paired = TRUE))),
               "equal size")
  expect_error(table1_tc(d, vars = c(x = "conts"), by = "g", test_args = list(wilcox.test = list(paired = TRUE))),
               "row order")
})

test_that("H7: paired tests under fweight equal the paired test on the expanded data", {
  d <- data.frame(x = c(1, 2, 4, 2, 5, 3), g = rep(1:2, each = 3), f = c(2, 1, 1, 2, 1, 1))
  e <- d[rep(seq_len(nrow(d)), d$f), ]
  for (ta in list(list(t.test = list(paired = TRUE)), list(wilcox.test = list(paired = TRUE)))) {
    type <- if (names(ta) == "t.test") "contn" else "conts"
    a <- suppressWarnings(table1_tc(d, vars = stats::setNames(type, "x"), by = "g", fweight = "f",
                                    test = TRUE, test_args = ta))
    b <- suppressWarnings(table1_tc(e, vars = stats::setNames(type, "x"), by = "g", test = TRUE, test_args = ta))
    expect_identical(a$body, b$body)
    expect_identical(a$stored$methods, b$stored$methods)
  }
})

test_that("H7: a nonzero mu is refused; mu = 0 and the var.equal labels are unchanged", {
  d <- data.frame(x = c(1, 2, 4, 7, 2, 4, 5, 9, 3, 8), g = rep(c("a", "b"), 5))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(mu = 5))),
               "must be 0")
  expect_error(table1_tc(d, vars = c(x = "conts"), by = "g", test_args = list(wilcox.test = list(mu = 1))),
               "mu")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(mu = NA))),
               "must be 0")
  expect_identical(table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE,
                             test_args = list(t.test = list(mu = 0)))$body,
                   table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE)$body)
  tt <- table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE, test_args = list(t.test = list(var.equal = TRUE)))
  expect_identical(col_of(tt, "test")[1], "t test")
  expect_identical(col_of(table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE), "test")[1], "Welch t test")
})

# H10 (F11): nonfinite values and overflow -------------------------------------
# Stata cells from qa/stata/probe_h10_overflow.do (Stata 17, tabtools 2.1.12
# for C5; first probed on 2.1.11). Since 2.1.12 an overflowing weighted sum
# makes Stata divide the weights by a power of two and recompute, so the
# weighted cells, ESS and effective counts are the true values; R does the
# same (the 2.1.11 blanks and "5 ()" cells were take_action items 1-6,
# fixed). A sum of values (not weights) beyond the range is still missing.

h10_cells <- function(tt) {
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, f)
  readLines(f, encoding = "UTF-8")
}

test_that("H10: Inf, -Inf, NaN and values beyond Stata's range are refused by name, for every type", {
  for (type in c("contn", "contln", "conts", "bin", "cat")) {
    for (bad in list(Inf, -Inf, NaN, 1e308)) {
      d <- data.frame(x = c(0, 1, bad, 1), g = c(1, 1, 2, 2))
      expect_error(table1_tc(d, vars = stats::setNames(type, "x"), by = "g"),
                   if (is.nan(bad) || is.infinite(bad)) "non-finite" else "largest number",
                   label = paste(type, bad))
    }
  }
  # The refusal names the variable, and records outside by() count too.
  expect_error(table1_tc(data.frame(age = c(1, 2, Inf), g = c(1, 2, NA)), vars = c(age = "contn"), by = "g"), "age")
  # A missing value stays a missing value.
  expect_no_error(table1_tc(data.frame(x = c(1, 2, NA)), vars = c(x = "contn")))
})

test_that("H10: 1e200 values give Stata's cells (mean in e-notation, SD '.')", {
  x <- c(1e200, 2e200, 3e200)
  expect_identical(h10_cells(table1_tc(data.frame(x = x), vars = c(x = "contn")))[3], "x,2.e+200\u00b1.")
  expect_identical(h10_cells(table1_tc(data.frame(x = x), vars = c(x = "conts")))[3],
                   "x,\"2.e+200 (1.e+200, 3.e+200)\"")
  expect_match(h10_cells(table1_tc(data.frame(x = x), vars = c(x = "contln")))[3], "^x,2\\.e\\+200 \\(")
  expect_identical(h10_cells(table1_tc(data.frame(x = c(1e155, 2e155, 3e155)), vars = c(x = "contn")))[3],
                   "x,2.e+155\u00b1.")
  # By group: SD '.', and no p-value or SMD from overflowing moments
  # (Stata: blank p, blank SMD; not a p of 1 from an infinite SE).
  d <- data.frame(x = c(1e200, 2e200, 3e200, 1e200, 5e200, 6e200), g = rep(1:2, each = 3))
  tt <- table1_tc(d, by = "g", vars = c(x = "contn"), smd = TRUE, test = TRUE)
  expect_identical(h10_cells(tt)[3], "x,2.e+200\u00b1.,4.e+200\u00b1.,,,")
  expect_true(is.na(tt$rows$p[1]) && is.na(tt$rows$smd[1]))
  # conts: the medians, and the (mean/SD-based) SMD is blank as in Stata.
  tt <- table1_tc(d, by = "g", vars = c(x = "conts"), smd = TRUE)
  expect_match(h10_cells(tt)[3], "^x,\"2.e\\+200 \\(1.e\\+200, 3.e\\+200\\)\",\"5.e\\+200 \\(1.e\\+200, 6.e\\+200\\)\",.*,$")
  # contln on the log scale is unaffected: SMD 0.670 as in Stata.
  expect_identical(sprintf("%.3f", table1_tc(d, by = "g", vars = c(x = "contln"), smd = TRUE)$rows$smd[1]), "0.670")
  # Squares beyond Stata's range (6e153^2 = 3.6e307, three of them 1.08e308):
  # Stata's sum is missing, so the SD is "." (R's own range would give 0).
  expect_identical(h10_cells(table1_tc(data.frame(x = rep(6e153, 3)), vars = c(x = "contn")))[3],
                   "x,6.e+153\u00b1.")
  # A sum beyond Stata's range: a blank cell (Stata: blank), the log scale
  # still summarised.
  d <- data.frame(x = c(5e307, 5e307, 1, 2), g = c(1, 1, 2, 2))
  tt <- table1_tc(d, by = "g", vars = "x contn \\ x contln", smd = TRUE, total = "after")
  expect_identical(tt$body[1, 2:4], data.frame(c2 = "", c3 = "2\u00b11", c4 = ""))
  expect_true(is.na(tt$rows$smd[1]))
  expect_identical(sprintf("%.3f", tt$rows$smd[2]), "2043.308")
})

test_that("H10: weights whose squares overflow give the true ESS and Stata's SD (probes F, F2)", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10), w = 1, b = (1:20) %% 2)
  # Exact doubles: without long double R reads 1e200 several ulps high, and
  # the group mean 5.5000000000000018 then falls to 5.4999999999999991.
  for (w in exact_num(c("1e200", "1e160"))) {
    d$w <- w
    out <- h10_cells(table1_tc(d, by = "g", vars = c(x = "contn", b = "bin"), wt = "w", smd = TRUE))
    # Stata 2.5.1 prints "6\u00b1." at 1e200 (its centered ss squares sum(wdev)
    # with Mata ^2, no extended range); R keeps 2.1.14's finite SD on purpose
    # (Stata-Dev item 2026-10-06-tabtools-centered-ss-square-overflow.md).
    expect_identical(out[3:5], c("Effective sample size,ESS=10,ESS=10,", "x,6\u00b13,16\u00b13,3.303", "b,50,50,0.000"),
                     label = paste("weights", w))
  }
})

test_that("H10: one weight near Stata's limit among ones (probes D8, D8n, E8)", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10), w = 1, b = (1:20) %% 2, c = (1:20) %% 3)
  d$w[3] <- 8e307
  tt <- table1_tc(d, by = "g", vars = "x contn \\ x conts \\ b bin \\ c cat", wt = "w", smd = TRUE)
  out <- h10_cells(tt)
  expect_identical(out[3], "Effective sample size,ESS=1,ESS=10,")
  # The weight on x = 3 dominates group 1. The SMD comes from Stata's
  # unrescaled sums (blank where w * x overflows), as before.
  expect_identical(out[4], "x,3\u00b10,16\u00b13,")
  expect_identical(out[5], "x,\"3 (3, 3)\",\"16 (13, 18)\",")
  expect_identical(out[6:10], c("b,100,50,1.414", "c,,,2.160", "   0,100,30,", "   1,0,30,", "   2,0,40,"))
  # wtn: effective counts.
  out <- h10_cells(table1_tc(d, by = "g", vars = "x contn \\ b bin \\ c cat", wt = "w", wtn = TRUE))
  expect_identical(out[3:8], c("Effective sample size,ESS=1,ESS=10", "x,3\u00b10,16\u00b13", "b,10 (100),5 (50)",
                               "c,,", "   0,10 (100),3 (30)", "   1,0 (0),3 (30)"))
  # Three records, weight 8e307 on x = 3.
  out <- h10_cells(table1_tc(data.frame(x = 1:3, w = c(1, 1, 8e307)), vars = c(x = "contn"), wt = "w"))
  expect_identical(out[3:4], c("Effective sample size,ESS=1", "x,3\u00b10"))
})

test_that("H10: weight sums beyond Stata's range give the rescaled cells (probes D8all, W1b, W2b, SC3)", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10), w = 8e307, b = (1:20) %% 2, c = (1:20) %% 3)
  out <- h10_cells(table1_tc(d, by = "g", vars = "x contn \\ x conts \\ b bin \\ c cat", wt = "w", smd = TRUE))
  expect_identical(out[3], "Effective sample size,ESS=10,ESS=10,")
  # The cells are the equal-weight values; the SMD stays blank (Stata's
  # SMD sums are not rescaled).
  expect_identical(out[4:10], c("x,6\u00b13,16\u00b13,", "x,\"6 (3, 8)\",\"16 (13, 18)\",", "b,50,50,", "c,,,",
                                "   0,30,30,", "   1,40,30,", "   2,30,40,"))
  out <- h10_cells(table1_tc(d, by = "g", vars = "b bin \\ c cat", wt = "w", wtn = TRUE, total = "after"))
  expect_identical(out[3:8], c("Effective sample size,ESS=10,ESS=10,ESS=20", "b,5 (50),5 (50),10 (50)", "c,,,",
                               "   0,3 (30),3 (30),6 (30)", "   1,4 (40),3 (30),7 (35)", "   2,3 (30),4 (40),7 (35)"))
  out <- h10_cells(table1_tc(d, by = "g", vars = "b bin", wt = "w", percent_n = TRUE))
  expect_identical(out[3:4], c("Effective sample size,ESS=10,ESS=10", "b,50 (5),50 (5)"))
  # Only one group overflows; smallcells runs on the rescaled counts.
  d$w[d$g == 2] <- 1
  tt <- table1_tc(d, by = "g", vars = "b bin \\ c cat", wt = "w", wtn = TRUE, smallcells = 3)
  expect_identical(tt$body$c2, c("ESS=10", "5 (50)", "", "3 (30)", "4 (40)", "3 (30)"))
  expect_identical(tt$body$c3, c("ESS=10", "5 (50)", "", "3 (30)", "3 (30)", "4 (40)"))
})

test_that("H10: all weights 8e307 give the true ESS and cells, not an error (probe C8)", {
  out <- h10_cells(table1_tc(data.frame(x = 1:3, w = rep(8e307, 3)), vars = c(x = "contn"), wt = "w"))
  expect_identical(out[3:4], c("Effective sample size,ESS=3", "x,2\u00b11"))
})

test_that("H10: weights of 2^1023 or more (Stata's missing range) and overflowing frequencies are refused", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10), w = 1)
  d$w[3] <- 1e308
  expect_error(table1_tc(d, by = "g", vars = c(x = "contn"), wt = "w"), "largest number")
  d$w <- 1e308
  expect_error(table1_tc(d, by = "g", vars = c(x = "contn"), wt = "w"), "largest number")
  expect_error(table1_tc(data.frame(x = 1:3, w = c(1, 1, 1e308)), vars = c(x = "contn"), wt = "w"), "rescale")
  expect_error(table1_tc(data.frame(x = 1:4, g = rep(1:2, 2), f = c(1, 1, 4.6e307, 4.6e307)), by = "g",
                         vars = c(x = "contn"), fweight = "f"), "frequencies")
  # 1e15 frequencies still run (Stata: N=2.00000e+15).
  tt <- table1_tc(data.frame(x = 1:4, g = rep(1:2, each = 2), f = 1e15), by = "g", vars = c(x = "contn"), fweight = "f")
  expect_identical(h10_cells(tt)[2:3], c("Mean\u00b1SD,N=2.00000e+15,N=2.00000e+15,", "x,2\u00b11,4\u00b11,<0.001"))
})

test_that("H10: smallcells beyond the integer range is refused without a coercion warning (Stata refuses too)", {
  d <- data.frame(b = c(0, 1, 1, 0), g = c(1, 1, 2, 2))
  for (k in list(Inf, 3e9, 1e10, -Inf, NaN)) {
    expect_no_warning(expect_error(table1_tc(d, by = "g", vars = c(b = "bin"), smallcells = k),
                                   "integer greater than or equal to 3"))
  }
  expect_no_warning(expect_error(tabtools:::tt_smallcells(matrix(c(1, 5, 6, 7), 2), Inf), "smallcells"))
  expect_no_warning(expect_error(tabtools:::tt_smallcells(matrix(c(1, 5, 6, 7), 2), 3e9), "smallcells"))
})

test_that("H10: Stata's e-notation for three-digit exponents (Stata-probed format x value cells, incl. the rounding carry across 1e100)", {
  p <- utils::read.csv(test_path("fixtures", "table1_hardening", "stata_fmt_exp3.csv"), colClasses = "character")
  got <- mapply(function(f, x) tabtools:::stata_fmt(exact_num(x), f), p$fmt, p$x, USE.NAMES = FALSE)
  expect_identical(got, p$stata)
})

# H17 (user item (a)): a factor's label under wt / fweight ---------------------

test_that("H17: a labelled factor keeps its label under wt, wt + wtcompare, and fweight", {
  d <- data.frame(arm = rep(1:2, each = 6), stage = factor(rep(c("I", "II", "III"), 4)),
                  age = 41:52, smoker = rep(c(0, 1, 1), 4), w = rep(c(0.5, 1.5, 2), 4), fw = rep(1:3, 4))
  attr(d$stage, "label") <- "Tumour stage"
  attr(d$age, "label") <- "Age (years)"
  attr(d$smoker, "label") <- "Current smoker"
  want <- c("Age (years)", "Current smoker", "Tumour stage")
  labs <- function(tt) intersect(tt$body[[1]], c(want, "stage", "age", "smoker"))
  vars <- c(age = "contn", smoker = "bin", stage = "cat")
  expect_identical(labs(table1_tc(d, by = "arm", vars = vars)), want)
  expect_identical(labs(table1_tc(d, by = "arm", vars = vars, wt = "w")), want)
  expect_identical(labs(table1_tc(d, by = "arm", vars = vars, wt = "w", wtcompare = TRUE)), want)
  expect_identical(labs(table1_tc(d, by = "arm", vars = vars, fweight = "fw")), want)
  # The by() variable's label too, and a weight that drops records.
  attr(d$arm, "label") <- "Arm"
  d$w[1] <- 0
  tt <- table1_tc(d, by = "arm", vars = vars, wt = "w")
  expect_identical(labs(tt), want)
  expect_identical(tt$header[[1]]$text, table1_tc(d, by = "arm", vars = vars, nopvalue = TRUE)$header[[1]]$text)
  # Levels, codes and counts are unchanged by keeping the attribute.
  a <- table1_tc(d, by = "arm", vars = c(stage = "cat"), fweight = "fw")
  attr(d$stage, "label") <- NULL
  b <- table1_tc(d, by = "arm", vars = c(stage = "cat"), fweight = "fw")
  expect_identical(a$body[-1, ], b$body[-1, ])
})

test_that("H17: haven-labelled columns keep their labels under weights", {
  skip_if_not_installed("haven")
  d <- data.frame(arm = rep(1:2, each = 6), w = c(0, rep(c(1, 2, 3), 4)[-1]))
  d$g <- haven::labelled(rep(c(1, 2, 3), 4), c(Low = 1, Mid = 2, High = 3), label = "Grade")
  tt <- table1_tc(d, by = "arm", vars = c(g = "cat"), wt = "w")
  expect_identical(tt$body[[1]][2:5], c("Grade", "   Low", "   Mid", "   High"))
})

# Independent review of group t1 (R1-R3, R8, R11) ------------------------------

test_that("review R1: paired and the other test switches must be TRUE or FALSE", {
  d <- data.frame(x = c(1, NA, 4, 7, 2, 4, NA, 9), g = rep(c("a", "b"), each = 4))
  for (pv in list(1, 1L, "yes", NA, c(TRUE, FALSE))) {
    expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test = TRUE,
                           test_args = list(t.test = list(paired = pv))), "paired", label = deparse(pv))
    expect_error(table1_tc(d, vars = c(x = "conts"), by = "g", test = TRUE,
                           test_args = list(wilcox.test = list(paired = pv))), "paired", label = deparse(pv))
  }
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(var.equal = 1))),
               "var.equal")
  expect_error(table1_tc(d, vars = c(x = "conts"), by = "g", test_args = list(wilcox.test = list(exact = "no"))),
               "exact")
  expect_error(table1_tc(data.frame(b = c(0, 1, 1, 0), g = c(1, 1, 2, 2)), vars = c(b = "bin"), by = "g",
                         test_args = list(chisq.test = list(correct = 1))), "correct")
  # FALSE and NULL still pass.
  expect_no_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(paired = FALSE))))
  expect_no_error(table1_tc(d, vars = c(x = "conts"), by = "g", test_args = list(wilcox.test = list(exact = NULL))))
})

test_that("review R2: the ESS and SD whose intermediate products overflow (probes R2a-R2c, P3)", {
  # Stata 2.1.12 forms both as R always did (take_action items 5-6, fixed;
  # 2.1.11 printed "ESS=." and "1\u00b1.").
  out <- h10_cells(table1_tc(data.frame(x = 2:5, w = 3e153), vars = c(x = "contn"), wt = "w"))
  expect_identical(out[3:4], c("Effective sample size,ESS=4", "x,4\u00b11"))
  # (sum w)^2 beyond R's own double range too (4 x 4e153): still ESS=4.
  out <- h10_cells(table1_tc(data.frame(x = 2:5, w = 4e153), vars = c(x = "contn"), wt = "w"))
  expect_identical(out[3:4], c("Effective sample size,ESS=4", "x,4\u00b11"))
  # n / (sum(w) * (n - 1)) beyond the range: the SD is essentially 0 (all
  # the weight on x = 1), and the ESS comes from the rescaled weights.
  out <- h10_cells(table1_tc(data.frame(x = 1:4, w = c(4e307, 1, 1, 1)), vars = c(x = "contn"), wt = "w"))
  expect_identical(out[3:4], c("Effective sample size,ESS=1", "x,1\u00b10"))
  # Where Mata has the extended range, sx * sx / n (probe P3), R matches
  # Stata: "x,3.e+153\u00b18.e+152".
  out <- h10_cells(table1_tc(data.frame(x = c(2e153, 3e153, 4e153, 3e153)), vars = c(x = "contn")))
  expect_identical(out[3], "x,3.e+153\u00b18.e+152")
})

test_that("review R3: paired fweight tests need equal frequencies within each pair", {
  d <- data.frame(g = rep(c("a", "b"), each = 3), x = c(1, 2, 4, 2, 5, 3), f = c(2, 1, 1, 1, 1, 2))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", fweight = "f", test = TRUE,
                         test_args = list(t.test = list(paired = TRUE))), "frequenc")
  # A zero frequency on one side only (it would drop one record and shift the rest).
  d0 <- data.frame(g = rep(c("a", "b"), each = 4), x = c(1, 2, 4, 7, 2, 4, 5, 9), f = c(0, 1, 1, 1, 1, 1, 1, 0))
  expect_error(table1_tc(d0, vars = c(x = "contn"), by = "g", fweight = "f", test = TRUE,
                         test_args = list(t.test = list(paired = TRUE))), "Pairs 1, 4 have")
  expect_error(table1_tc(d0, vars = c(x = "conts"), by = "g", fweight = "f", test = TRUE,
                         test_args = list(wilcox.test = list(paired = TRUE))), "frequenc")
  # Equal frequencies, zero on both sides of a pair: each pair counted f times.
  d1 <- data.frame(g = rep(c("a", "b"), each = 4), x = c(1, 2, 4, 7, 2, 4, 5, 9), f = c(0, 2, 1, 3, 0, 2, 1, 3))
  tt <- table1_tc(d1, vars = c(x = "contn"), by = "g", fweight = "f", test = TRUE,
                  test_args = list(t.test = list(paired = TRUE)))
  a <- rep(c(2, 4, 7), c(2, 1, 3))
  b <- rep(c(4, 5, 9), c(2, 1, 3))
  expect_equal(tt$rows$p[1], stats::t.test(a, b, paired = TRUE)$p.value)
  # Without paired tests, unequal frequencies are fine.
  expect_no_error(table1_tc(d, vars = c(x = "contn"), by = "g", fweight = "f", test = TRUE))
})

test_that("review R8: mu boundary, non-positive contln pairs, more than two groups", {
  d <- data.frame(x = c(1, 2, 4, 7, 2, 4, 5, 9), g = rep(c("a", "b"), each = 4))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(mu = 0.5))), "must be 0")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(mu = -1e-9))), "must be 0")
  d$x[1] <- 0
  tt <- table1_tc(d, vars = c(x = "contln"), by = "g", test = TRUE, test_args = list(t.test = list(paired = TRUE)))
  expect_equal(tt$rows$p[1], stats::t.test(log(c(NA, 2, 4, 7)), log(c(2, 4, 5, 9)), paired = TRUE)$p.value)
  d3 <- data.frame(x = 1:9, g = rep(1:3, 3))
  expect_error(table1_tc(d3, vars = c(x = "contn"), by = "g", test_args = list(t.test = list(paired = TRUE))),
               "exactly two groups")
  expect_error(table1_tc(d3, vars = c(x = "conts"), by = "g", test_args = list(wilcox.test = list(paired = TRUE))),
               "exactly two groups")
})

test_that("review R11: Inf in by and on records that weights drop is refused; the 2^1023 boundary", {
  expect_error(table1_tc(data.frame(x = 1:4, g = c(1, 1, Inf, Inf)), vars = c(x = "contn"), by = "g"),
               "`g` holds non-finite")
  expect_error(table1_tc(data.frame(x = c(1, 2, Inf, 4), w = c(1, 1, 0, 1)), vars = c(x = "contn"), wt = "w"),
               "non-finite")
  expect_error(table1_tc(data.frame(x = c(1, 2^1023)), vars = c(x = "contn")), "largest number")
  expect_no_error(table1_tc(data.frame(x = c(1, (2 - 2^-52) * 2^1022)), vars = c(x = "conts")))
  expect_error(table1_tc(data.frame(x = 1:2, w = c(1, 2^1023)), vars = c(x = "contn"), wt = "w"), "largest number")
  expect_no_error(table1_tc(data.frame(x = 1:2, w = c(1, (2 - 2^-52) * 2^1022)), vars = c(x = "contn"), wt = "w"))
})

# Audit 2026-09-29 (A04, A08, A10) ----------------------------------------

test_that("A04: matrix and data-frame columns are refused; a one-column matrix is a vector", {
  d <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 11:15))
  d$m <- cbind(d$x, d$x * 100)
  expect_error(table1_tc(d, by = "g", vars = c(m = "contn %6.1f")), "matrix column with 2 columns")
  expect_error(table1_tc(d, by = "g", vars = "m"), "`m`")
  d2 <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 11:15))
  d2$df <- data.frame(p = 1:10, q = 1:10)
  expect_error(table1_tc(d2, by = "g", vars = c(df = "contn")), "data-frame column")
  d3 <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 11:15))
  d3$w <- matrix(1, 10, 2)
  expect_error(table1_tc(d3, by = "g", vars = c(x = "contn"), wt = "w"), "matrix column")
  d4 <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 11:15))
  d4$s <- scale(d4$x)
  expect_identical(as.data.frame(table1_tc(d4, by = "g", vars = c(s = "contn %6.2f"))),
                   as.data.frame(table1_tc(transform(d4[c("g", "x")], s = as.vector(scale(x))),
                                           by = "g", vars = c(s = "contn %6.2f"))))
  # ... also under weights, which subset the rows first.
  d4$w <- rep(1:2, 5)
  expect_no_error(table1_tc(d4, by = "g", vars = c(s = "contn"), wt = "w", wtn = TRUE))
})

test_that("A08: format, percformat and nformat must each be one format string", {
  d <- data.frame(g = rep(c("a", "b"), 10), x = 1:20, b = rep(0:1, each = 10))
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", nformat = NULL), "`nformat`")
  expect_error(table1_tc(d, vars = c(b = "bin"), by = "g", percformat = NULL), "`percformat`")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", format = character()), "`format`")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", format = c("%5.1f", "%5.3f")), "`format`")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", format = NA_character_), "`format`")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", nformat = 5), "`nformat`")
})

test_that("A10: a used column with an empty or missing name is refused by name", {
  d <- data.frame(a = 1:10, b = rep(0:1, 5))
  names(d)[1] <- ""
  expect_error(table1_tc(d), "empty or missing name")
  names(d)[1] <- NA
  expect_error(table1_tc(d), "empty or missing name")
  # A column that is not used does not matter.
  expect_no_error(table1_tc(d, vars = c(b = "bin")))
})
