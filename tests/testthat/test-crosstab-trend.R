test_that("Cochran-Armitage sign, variance and affine score recoding match literal truth", {
  xt_no_session()
  f <- rbind(c(10, 20, 30), c(30, 20, 10))
  d <- xt_data(f, colcodes = c(0, 1, 2))
  x <- crosstab(d, "row", "column", weights = "frequency", cochran = TRUE)
  expect_equal(x$stored$z_trend, -sqrt(20))
  expect_equal(x$stored$chi2_trend, 20)
  expect_equal(x$stored$p_trend, stats::pchisq(20, 1, lower.tail = FALSE))
  for (scores in list(c(100, 102, 104), 1e16 + c(0, 2, 4), c(-1e308, 0, 1e308))) {
    dd <- xt_data(f, colcodes = scores)
    expect_equal(crosstab(dd, "row", "column", weights = "frequency", cochran = TRUE)$stored$z_trend, -sqrt(20))
  }
  flipped <- xt_table(f[2:1, ], cochran = TRUE)
  expect_equal(flipped$stored$z_trend, sqrt(20))
  unequal <- crosstab(xt_data(f, colcodes = c(0, 1, 5)), "row", "column", weights = "frequency", cochran = TRUE)
  expect_equal(unequal$stored$chi2_trend, 125 / 7)
  paper <- xt_table(rbind(c(497, 560, 269), c(19, 29, 24)), cochran = TRUE)
  # Table 1: N=1398, E=72, score mean=-223/1398. The centered
  # event score is 3841/233 and score SS is 1081253/1398.
  # These original integer moments give the full-precision statistic;
  # Armitage p.378 prints its rounded chi-square 7.19 and p=.007.
  paper_chi2 <- 3841^2 * 233 / (442 * 1081253)
  paper_p <- stats::pchisq(paper_chi2, 1, lower.tail = FALSE)
  expect_equal(paper$stored$chi2_trend, paper_chi2, tolerance = 1e-12)
  expect_equal(paper$stored$p_trend, paper_p, tolerance = 1e-12)
  expect_identical(round(paper_chi2, 2), 7.19)
  expect_identical(round(paper_p, 3), .007)
})

test_that("Spearman uses literal expanded ties and installed t inference", {
  xt_no_session()
  f <- matrix(c(5, 3, 2, 7, 4, 6), 2)
  d <- xt_data(f)
  expanded <- d[rep(seq_len(nrow(d)), d$frequency), ]
  r <- rank(expanded$row); c <- rank(expanded$column)
  rho <- sum((r - mean(r)) * (c - mean(c))) / sqrt(sum((r - mean(r))^2) * sum((c - mean(c))^2))
  truth <- 2 * stats::pt(abs(rho) * sqrt((27 - 2) / (1 - rho^2)), 25, lower.tail = FALSE)
  x <- crosstab(d, "row", "column", weights = "frequency", trend = TRUE)
  expect_equal(x$stored$p_trend, truth)
  expect_equal(crosstab(expanded, "row", "column", trend = TRUE)$stored$p_trend, truth)
  expect_identical(x$stored$trend_method, "Spearman rank correlation")
  expect_error(xt_table(diag(2), trend = TRUE), class = "tabtools_error_crosstab_trend")
  expect_equal(xt_table(diag(c(1, 2)), trend = TRUE)$stored$p_trend, 0)
  expect_equal(xt_table(diag(c(2, 3, 4)), trend = TRUE)$stored$p_trend, 0)
  expect_error(xt_table(matrix(c(0, 1, 1, 0), 2), trend = TRUE), class = "tabtools_error_crosstab_trend")
  expect_error(xt_table(matrix(c(0, 2, 3, 0), 2), trend = TRUE), class = "tabtools_error_crosstab_trend")
})

test_that("trend options reject mismatched samples and outcome layouts", {
  xt_no_session()
  f <- matrix(10, 2, 3)
  expect_error(xt_table(f, trend = TRUE, cochran = TRUE), class = "tabtools_error_crosstab_trend")
  expect_error(xt_table(f, trend = TRUE, missing = TRUE), class = "tabtools_error_crosstab_trend")
  expect_error(xt_table(f, cochran = TRUE, missing = TRUE), class = "tabtools_error_crosstab_trend")
  expect_error(xt_table(matrix(10, 3, 3), cochran = TRUE), class = "tabtools_error_crosstab_trend")
  expect_error(crosstab(data.frame(r = rep(1, 4), c = 1:4), "r", "c", cochran = TRUE),
               class = "tabtools_error_crosstab_sample")
})
