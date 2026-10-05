# Milestone H task H19 (external review F29): geeglm's fixed scale, against
# Stata 17's xtgee scale(2) (cases G0-G2 of qa/stata/make_regtab_h_t2b.do).

h19_gee <- function() ht2b_read("gee")

test_that("geeglm scale.value: a constant vector is xtgee's scale(#) (H19; Stata G0-G2)", {
  skip_on_cran()
  for (p in c("geepack", "haven")) skip_if_not_installed(p)
  d <- h19_gee()
  n <- nrow(d)
  g0 <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "independence")
  expect_ht2b(regtab(g0), "G0")
  # The scale from the data (nrow(d), with data = d): the fit keeps only
  # the expression, so it must not depend on anything else (Codex audit R1).
  g1 <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "independence", scale.fix = TRUE,
                        scale.value = rep(2, nrow(d)))
  e1 <- ht2b_e("G1")
  expect_equal(unname(sqrt(diag(tt_vcov(g1)))), unname(e1$se[c("_cons", "x")]), tolerance = 1e-8)
  expect_ht2b(regtab(g1), "G1")
  # scale(2) against the default scale: the variance times 2 / phi-hat.
  phi <- ht2b_e("G0")$e[["phi"]]
  expect_equal(tt_vcov(g1), tt_vcov(g0) * 2 / phi, tolerance = 1e-8)
  # A constant column of the data.
  dc <- d
  dc$sc <- 2
  g1s <- geepack::geeglm(y ~ x, gaussian, dc, id = id, corstr = "independence", scale.fix = TRUE, scale.value = sc)
  expect_equal(tt_vcov(g1s), tt_vcov(g1))
  # A symbol outside the data: its value at fit time is not stored, so it
  # is refused (Codex audit R1; test-codex-audit.R has the mutation guard).
  sc <- rep(2, n)
  g1v <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "independence", scale.fix = TRUE, scale.value = sc)
  expect_error(tt_vcov(g1v), "refers to\\s+`sc`,\\s+which\\s+the\\s+fit\\s+does\\s+not\\s+store")
})

test_that("exchangeable: xtgee's scale(2) is the default fit's variance x 2/phi; geepack's scale.fix fit differs (T2B-07)", {
  skip_on_cran()
  for (p in c("geepack", "haven")) skip_if_not_installed(p)
  d <- h19_gee()
  n <- nrow(d)
  ctl <- geepack::geese.control(epsilon = 1e-12, maxit = 100)
  # xtgee's correlation estimate does not depend on the scale: Stata's G2
  # (scale(2)) and G3 (default) estimates are equal, and the default
  # geeglm fit reproduces them.
  expect_identical(ht2b_e("G2")$b, ht2b_e("G3")$b)
  g3 <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "exchangeable", control = ctl)
  expect_equal(unname(coef(g3)), unname(ht2b_e("G3")$b[c("_cons", "x")]), tolerance = 1e-8)
  # G2's variance is that fit's xtgee variance times 2 / phi-hat.
  phi3 <- ht2b_e("G3")$e[["phi"]]
  expect_equal(unname(sqrt(diag(tt_vcov(g3)) * 2 / phi3)), unname(ht2b_e("G2")$se[c("_cons", "x")]),
               tolerance = 1e-7)
  # geepack's scale.fix = TRUE fit estimates the correlation with the
  # dispersion held at its independence start value (gamma = G0's phi), so
  # its alpha and beta move: 8.2e-5 relative on this fixture, an estimator
  # difference, not convergence. Pinned, so a geepack change is noticed.
  g2 <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "exchangeable", scale.fix = TRUE,
                        scale.value = rep(2, nrow(d)), control = ctl)
  rel <- abs(coef(g2)[["x"]] / ht2b_e("G2")$b[["x"]] - 1)
  expect_gt(rel, 1e-5)
  expect_lt(rel, 2e-4)
  expect_message(regtab(g2), "estimates the correlation differently")
  # The cells still agree at display precision, the stored results within
  # that estimator difference (1e-4: 8.2e-5 observed).
  expect_ht2b(suppressMessages(regtab(g2)), "G2", 1e-4)
})

test_that("a non-constant or non-positive geeglm scale.value is refused (F29)", {
  skip_if_not_installed("geepack")
  set.seed(8)
  d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60), y = rnorm(60))
  d$v <- seq_len(60) / 60 + 1
  g <- geepack::geeglm(y ~ x, gaussian, d, id = id, scale.fix = TRUE, scale.value = v)
  expect_error(regtab(g), "not one positive constant")
  expect_error(tt_vcov(g), "scale.value = v", fixed = TRUE)
  z <- geepack::geeglm(y ~ x, gaussian, d, id = id, scale.fix = TRUE, scale.value = rep(-1, 60))
  expect_error(regtab(z), "positive constant")
  # scale.fix = TRUE without scale.value: geepack's default 1.
  u <- geepack::geeglm(y ~ x, gaussian, d, id = id, scale.fix = TRUE)
  expect_identical(tabtools:::.rt_gee_scale(u), 1)
})
