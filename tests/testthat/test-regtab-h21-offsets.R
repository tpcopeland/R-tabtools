# Milestone H task H21 (external review F27): the pseudo R-squared of models
# with an offset, against Stata 17 (qa/stata/make_regtab_h_t2b.do, cases
# O1-O5 on offs.dta). Stata reports e(r2_p) only where it forms a
# constant-only model with the offset: poisson does (0.039011 here; the
# verification probe's 0.149230 was on its own data), ologit/oprobit report
# no e(ll_0) and logit/probit no e(r2_p), so regtab's row is blank. The
# multinom null (H-D10: mlogit refuses offsets) is in
# test-regtab-h2-multinom.R.
#
# Stored tolerance per case: the smallest 1-2-5 step at or above the
# largest difference observed on 2026-09-26 (in the comment), never below
# 1e-9; the larger ones are polr's optim() convergence against ologit's.

ht2b_S <- c("n", "ll", "r2")
ht2b_ctl <- glm.control(epsilon = 1e-14, maxit = 100)

ht2b_offs <- function() {
  d <- ht2b_read("offs")
  d$yo <- factor(d$yo, ordered = TRUE)
  d
}

test_that("ologit/oprobit with an offset: no pseudo R-squared, as Stata (O1, O5)", {
  skip_on_cran()
  for (p in c("MASS", "ordinal", "haven")) skip_if_not_installed(p)
  d <- ht2b_offs()
  expect_identical(ht2b_e("O1")$e[["ll_0"]], NA_real_)
  expect_ht2b(regtab(MASS::polr(yo ~ x + offset(off), d, Hess = TRUE), stats = ht2b_S), "O1", 5e-6)  # 3.9e-7
  expect_ht2b(regtab(ordinal::clm(yo ~ x + offset(off), data = d), stats = ht2b_S), "O1", 1e-7)     # 1.4e-8
  expect_ht2b(regtab(MASS::polr(yo ~ x + offset(off), d, method = "probit", Hess = TRUE), stats = ht2b_S),
              "O5", 2e-5)  # 1.3e-5
  expect_ht2b(regtab(ordinal::clm(yo ~ x + offset(off), data = d, link = "probit"), stats = ht2b_S),
              "O5", 1e-6)  # 1.3e-7
  tt <- regtab(MASS::polr(yo ~ x + offset(off), d, Hess = TRUE), stats = ht2b_S)
  expect_false("Pseudo R\u00b2" %in% tt$body[[1]])
  expect_null(tt$stored$r2_p_1)
  # Without the offset the marginal null still applies.
  s <- tabtools:::tt_model_stats(MASS::polr(yo ~ x, d, Hess = TRUE), NULL)
  expect_true(is.finite(s$r2_p))
})

test_that("logit/probit with an offset: blank; poisson: against the offset-only null (O2-O4)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- ht2b_offs()
  expect_identical(ht2b_e("O2")$e[["r2_p"]], NA_real_)
  expect_ht2b(regtab(glm(yb ~ x + offset(off), binomial, d, control = ht2b_ctl), stats = ht2b_S), "O2")
  expect_ht2b(regtab(glm(yb ~ x + offset(off), binomial("probit"), d, control = ht2b_ctl), stats = ht2b_S), "O3")
  # An `offset =` argument is an offset too.
  expect_ht2b(regtab(glm(yb ~ x, binomial, d, offset = off, control = ht2b_ctl), stats = ht2b_S), "O2")
  fp <- glm(yc ~ x + offset(lnexpo), poisson, d, control = ht2b_ctl)
  expect_ht2b(regtab(fp, stats = ht2b_S), "O4")
  expect_equal(tabtools:::tt_model_stats(fp, NULL)$r2_p, ht2b_e("O4")$e[["r2_p"]], tolerance = 1e-9)
  expect_ht2b(regtab(glm(yc ~ x, poisson, d, offset = lnexpo, control = ht2b_ctl), stats = ht2b_S), "O4")
  # The same under a robust variance (Stata's [pw] reading).
  tr <- regtab(glm(yb ~ x + offset(off), quasibinomial, d, control = ht2b_ctl), vce = "robust", stats = ht2b_S)
  expect_false("Pseudo R\u00b2" %in% tr$body[[1]])
  # Without an offset, logit keeps its pseudo R-squared.
  t0 <- regtab(glm(yb ~ x, binomial, d, control = ht2b_ctl), stats = ht2b_S)
  expect_true("Pseudo R\u00b2" %in% t0$body[[1]])
})

test_that("glm.nb's null keeps the offset", {
  skip_if_not_installed("MASS")
  set.seed(21)
  d <- data.frame(x = rnorm(300), expo = runif(300, 0.5, 4))
  d$lnexpo <- log(d$expo)
  d$yc <- MASS::rnegbin(300, d$expo * exp(0.2 + 0.4 * d$x), theta = 1.5)
  f <- MASS::glm.nb(yc ~ x + offset(lnexpo), d)
  f0 <- MASS::glm.nb(yc ~ 1 + offset(lnexpo), d)
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(s$r2_p, 1 - as.numeric(logLik(f) / logLik(f0)), tolerance = 1e-6)
  # y = FALSE: the same null, from the rebuilt response (was "arguments
  # imply differing number of rows", F05/F28).
  fy <- MASS::glm.nb(yc ~ x + offset(lnexpo), d, y = FALSE)
  expect_equal(tabtools:::.rt_negbin_ll0(fy), tabtools:::.rt_negbin_ll0(f), tolerance = 1e-8)
  expect_identical(regtab(fy, stats = ht2b_S)$body, regtab(f, stats = ht2b_S)$body)
})
