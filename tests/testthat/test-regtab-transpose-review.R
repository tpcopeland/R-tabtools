# RL003 independent native capture uses these original literal observations,
# value labels and base 2. Releveling changes treatment order, not value codes.
transpose_review_fit <- function() {
  d <- data.frame(g = rep(1:3, each = 8), x = rep(1:8, 3))
  d$Y <- c(-1, 0, 2)[d$g] + .25*d$x + rep(c(.1, -.2, .2, -.1), 6)
  d$g <- stats::relevel(factor(d$g, levels = 1:3, labels = c("A", "B", "C")), "B")
  attr(d$g, "label") <- "Group"
  attr(d$g, "labels") <- c(A = 1, B = 2, C = 3)
  stats::lm(Y ~ g + x, d)
}

test_that("transpose uses native value-code order before placing added columns", {
  fit <- transpose_review_fit()
  t <- regtab(fit, fit, models = c("Crude", "Adjusted"), nointercept = TRUE,
    nopvalue = TRUE, transpose = TRUE, stats = "n", collabels = c("1.g" = "Drug A"),
    addcol = list(list(label = "Text", values = c("one", "two"), after = "1.g")))
  expect_identical(t$header[[1L]]$text, c("", "Observations", "Drug A", "Text", "Group: B", "Group: C", "x"))
  expect_identical(t$header[[2L]]$text,
    c("", "", "Coef. (95% CI)", "", rep("Coef. (95% CI)", 3L)))
  expect_identical(t$cols$term_key, c("", "", "1.g", "", "2.g", "3.g", "x"))
  expect_identical(t$body[[2L]], rep("24", 2L))
  expect_identical(t$body[[3L]], rep("-1.00 (-1.18, -0.82)", 2L))
  expect_identical(t$body[[4L]], c("one", "two"))
  expect_identical(t$body[[5L]], rep("Reference", 2L))
  expect_identical(t$body[[6L]], rep("2.00 (1.82, 2.18)", 2L))
  source <- t$meta$regtab_source_rows$key
  analytic <- which(nzchar(t$cols$term_key))
  expect_identical(source[t$cols$term_block[analytic]], t$cols$term_key[analytic])
  # Layout reordering does not rewrite raw model rows, states or coefficients.
  ordinary <- regtab(fit, fit, nointercept = TRUE, nopvalue = TRUE)
  expect_identical(t$stored$table, ordinary$stored$table)
  expect_identical(t$meta$regtab_raw_rows$key, ordinary$meta$regtab_raw_rows$key)
  ref <- regtab(fit, fit, nointercept = TRUE, nopvalue = TRUE, transpose = TRUE, reftop = TRUE)
  expect_identical(ref$cols$term_key, c("", "2.g", "1.g", "3.g", "x"))
  paired <- regtab(fit, nointercept = TRUE, transpose = TRUE)
  expect_identical(paired$cols$term_key, c("", "1.g", "1.g", "2.g", "2.g", "3.g", "3.g", "x", "x"))
})

test_that("each transposed native block has its literal single-cell header merge", {
  fit <- transpose_review_fit()
  t <- regtab(fit, fit, models = c("Crude", "Adjusted"), nointercept = TRUE,
    nopvalue = TRUE, transpose = TRUE, stats = "n", collabels = c("1.g" = "Drug A"),
    addcol = list(list(label = "Text", values = c("one", "two"), after = "1.g")))
  layout <- tabtools:::.xlsx_layout_regtab_transpose(t)
  merge <- layout$rules[layout$rules$op == 14L & layout$rules$r1 == 2L, ]
  expect_equal(merge$r1, rep(2L, 6L), tolerance = 0)
  expect_equal(merge$r2, rep(2L, 6L), tolerance = 0)
  expect_equal(merge$c1, 3:8, tolerance = 0)
  expect_equal(merge$c2, 3:8, tolerance = 0)
  state <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), t$style)
  expect_true(all(c("C2:C2", "D2:D2", "E2:E2", "F2:F2", "G2:G2", "H2:H2") %in% state$merges))
  expect_identical(unname(layout$grid[2L, 3:8]), c("Observations", "Drug A", "Text", "Group: B", "Group: C", "x"))
})
