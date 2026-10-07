test_that("Pearson matrices have independent literal coefficient and t controls", {
  t <- corrtab(data.frame(x = 1:4, y = c(1, 2, 4, 3)), c("x", "y"), pvalues = TRUE)
  expect_equal(t$stored$C, matrix(c(1, .8, .8, 1), 2, dimnames = list(c("x", "y"), c("x", "y"))), tolerance = 1e-14)
  expect_equal(t$stored$P[2, 1], .2, tolerance = 1e-14)
  expect_true(all(is.na(diag(t$stored$P))))
  expect_identical(t$stored$N, matrix(4L, 2, 2, dimnames = dimnames(t$stored$C)))
  expect_identical(t$body$c2, c("1.00", "0.80 (0.200)"))
  expect_identical(t$body$c3, c("", "1.00"))
  expect_identical(t$command, "corrtab")
  expect_match(t$stored$methods, "Computed in R", fixed = TRUE)
})

test_that("Spearman ranks are average tied ranks recomputed within pairs", {
  t <- corrtab(data.frame(x = c(1, 1, 2, 3), y = c(1, 2, 2, 3)), c("x", "y"), spearman = TRUE)
  expect_equal(t$stored$C[2, 1], 5 / 6, tolerance = 1e-14)
  expect_equal(t$stored$P[2, 1], 1 / 6, tolerance = 1e-14)
  d <- data.frame(x = c(1, 2, 3, 4, NA), y = c(1, 3, 5, 4, 2))
  t <- corrtab(d, c("x", "y"), spearman = TRUE)
  expect_equal(t$stored$C[2, 1], .8, tolerance = 1e-14)
  expect_equal(t$stored$P[2, 1], .2, tolerance = 1e-14)
  # Ranking the five y values before deletion gives 11/sqrt(175), not .8.
  expect_gt(abs(t$stored$C[2, 1] - 11 / sqrt(175)), .02)
})

test_that("pairwise counts and ledger never invent a single used sample", {
  d <- data.frame(x = c(1, 2, 3, 4, NA, NA), y = c(1, 2, 4, 3, 8, NA),
                  z = c(NA, 1, 2, 3, 4, 5))
  t <- corrtab(d, names(d), full = TRUE, pvalues = TRUE)
  expect_identical(unname(t$stored$N), matrix(c(4L, 4L, 3L, 4L, 5L, 4L, 3L, 4L, 5L), 3))
  expect_equal(t$stored$C[3, 2], 17 / sqrt(415), tolerance = 1e-14)
  expect_equal(t$stored$P[3, 2], 1 - 17 / sqrt(415), tolerance = 1e-14)
  sr <- corrtab(d, names(d), full = TRUE, spearman = TRUE)
  expect_equal(sr$stored$C[3, 2], .8, tolerance = 1e-14)
  expect_equal(sr$stored$P[3, 2], .2, tolerance = 1e-14)
  ledger <- t$meta$sample_accounting
  expect_true(all(ledger$measures$status[ledger$measures$metric == "used_n"] == "not_applicable"))
  expect_true(all(t$meta$corr_pairs$population_id %in% ledger$populations$id))
  expect_equal(nrow(t$meta$corr_rows), 9L)
  expect_identical(t$meta$corr_variables, names(d))
})

test_that("verified native endpoints retain missing inference honestly", {
  for (ranked in c(FALSE, TRUE)) {
    d <- data.frame(x = 1:3, positive = 1:3, negative = 3:1)
    t <- corrtab(d, names(d), spearman = ranked, full = TRUE, pvalues = TRUE)
    expect_equal(t$stored$C[2, 1], 1)
    expect_equal(t$stored$C[3, 1], -1)
    expect_equal(t$stored$P[2, 1], 0)
    if (ranked) {
      expect_true(is.na(t$stored$P[3, 1]))
      expect_identical(t$body$c2[3], "-1.00")
    } else expect_equal(t$stored$P[3, 1], 0)
    t2 <- corrtab(d[1:2, ], names(d), spearman = ranked)
    expect_true(all(is.na(t2$stored$P)))
  }
  d <- data.frame(x = 1:4, constant = 1, singleton = c(1, NA, NA, NA))
  t <- corrtab(d, names(d), full = TRUE)
  expect_identical(unname(diag(t$stored$N)), c(4L, 4L, 1L))
  expect_identical(t$body$c3, c(".", "", "."))
  expect_identical(t$body$c4, c(".", ".", ""))
  expect_s3_class(corrtab(d, c("constant", "singleton")), "tt_table")
  expect_error(corrtab(data.frame(a = c(NA_real_, NA_real_), b = NA_real_), c("a", "b")), class = "tabtools_error_corrtab_sample")
  empty <- corrtab(data.frame(a = c(NA_real_, NA_real_), b = NA_real_), c("a", "b"), spearman = TRUE)
  expect_true(all(is.na(empty$stored$C)))
  expect_true(all(empty$stored$N == 0L))
})

test_that("triangle selection and duplicate literal labels leave raw identity unchanged", {
  d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
  a <- corrtab(d, names(d), labels = c(x = "Same", y = "Same"))
  b <- corrtab(d, names(d), upper = TRUE, digits = 4)
  c <- corrtab(d, names(d), full = TRUE)
  expect_identical(a$stored$C, b$stored$C)
  expect_identical(b$stored$C, c$stored$C)
  expect_identical(a$body$c1, c("Same", "Same"))
  expect_identical(a$meta$corr_variables, c("x", "y"))
  expect_identical(b$body$c2, c("1.0000", ""))
  expect_identical(b$body$c3, c("0.8000", "1.0000"))
  expect_equal(sum(a$meta$corr_rows$displayed), 3L)
  expect_equal(sum(c$meta$corr_rows$displayed), 4L)
})

test_that("strict star thresholds and p-value text have literal boundary controls", {
  C <- matrix(c(1, .2, .2, 1), 2)
  P <- matrix(c(NA, .05, .05, NA), 2)
  fmt <- .tt_resolve_numeric_format(digits = 2)
  a <- .cor_body(C, P, c("x", "y"), "lower", c(.001, .01, .05), FALSE, fmt)
  expect_identical(dim(a$body), c(2L, 3L))
  expect_identical(dimnames(a$body), list(NULL, c("labels", "", "")))
  expect_identical(unname(a$body[2, 2]), "0.20")
  P[2, 1] <- .05 - 1e-10
  expect_identical(unname(.cor_body(C, P, c("x", "y"), "lower", c(.001, .01, .05), FALSE, fmt)$body[2, 2]), "0.20*")
  P[2, 1] <- .001
  expect_identical(unname(.cor_body(C, P, c("x", "y"), "lower", numeric(), TRUE, fmt)$body[2, 2]), "0.20 (0.001)")
  P[2, 1] <- .0009
  expect_identical(unname(.cor_body(C, P, c("x", "y"), "lower", numeric(), TRUE, fmt)$body[2, 2]), "0.20 (<0.001)")
  C[2, 1] <- -.0001
  P[2, 1] <- NA_real_
  expect_identical(unname(.cor_body(C, P, c("x", "y"), "lower", numeric(), TRUE, fmt)$body[2, 2]), "0.00")
  expect_identical(.cor_legend(c(.001, .01, .05)), "* p<0.05, ** p<0.01, *** p<0.001")
})

test_that("corrtab rejects option conflicts and nonnumeric or empty sources", {
  d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
  expect_error(corrtab(d, names(d), lower = TRUE, full = TRUE), class = "tabtools_error_corrtab_shape")
  for (star in list(c(.05, .05), c(.01, .02, .03, .04), 0, 1, NA_real_, numeric())) {
    expect_error(corrtab(d, names(d), star = star), class = "tabtools_error_corrtab_star")
  }
  expect_error(corrtab(d, names(d), pvalues = TRUE, star = .05), class = "tabtools_error_corrtab_star")
  expect_error(corrtab(d, names(d), digits = 7), class = "tabtools_error_corrtab_digits")
  expect_error(corrtab(d, c("x", "x")), class = "tabtools_error_corrtab")
  expect_error(corrtab(transform(d, y = factor(y)), names(d)), class = "tabtools_error_corrtab")
  expect_error(corrtab(d, names(d), subset = rep(FALSE, 4)), class = "tabtools_error_corrtab_sample")
  expect_error(corrtab(transform(d, x = c(Inf, 2, 3, 4)), names(d)), class = "tabtools_error_corrtab")
})

test_that("native worksheet geometry has one header and separate paragraph rows", {
  t <- corrtab(data.frame(x = 1:4, y = c(1, 2, 4, 3)), c("x", "y"),
    pvalues = TRUE, title = "Title", footnote = c("First note", "Second note"),
    headershade = TRUE, zebra = TRUE)
  lay <- .xlsx_layout_corrtab(t)
  expect_identical(dim(lay$grid), c(6L, 4L))
  expect_identical(lay$grid[1, ], c("Title", "", "", ""))
  expect_identical(lay$grid[2, ], c("", "", "x", "y"))
  expect_identical(lay$grid[4, ], c("", "y", "0.80 (0.200)", "1.00"))
  expect_identical(lay$grid[5:6, 2], c("First note", "Second note"))
  merges <- lay$rules[lay$rules$op == .OP[["merge"]], c("r1", "r2", "c1", "c2")]
  expect_equal(unname(as.matrix(merges)), matrix(c(1, 1, 1, 4, 5, 5, 2, 4, 6, 6, 2, 4), 3, byrow = TRUE))
  fill <- lay$rules[lay$rules$op == .OP[["fill"]], ]
  expect_equal(fill$r1, c(2L, 4L))
  expect_true(all(fill$c1 == 2L & fill$c2 == 4L))
})
