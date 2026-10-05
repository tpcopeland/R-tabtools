# Milestone H task H5 (external review F06, decision H-D2): intercept,
# cutpoint and ancillary rows are identified by their structural role, set
# by each adapter, never by their label. Stata tabtools 2.1.12 decides them
# the same way (from the coefficient's equation and name; take_action item
# 10, fixed): a covariate named or labelled `p`, `alpha`, `cut1`, ... is
# kept under the automatic nointercept and exponentiated under
# keepintercept (cases P1-P2 of qa/stata/make_regtab_h_t2b.do; 2.1.11
# dropped it and printed it unexponentiated, which R never copied), and a
# covariate named like its own model's ancillary parameter shows both rows
# (goldens R58-R63). The existing goldens keep their rows
# (test-golden-regtab.R and the Phase 5 fixtures run unchanged).

h5_reserved <- c("p", "alpha", "lnalpha", "ln_p", "constant", "intercept", "_cons", "cut1", "/x", "Ancillary: x")

test_that("covariates named or labelled like reserved rows survive nointercept and are exponentiated", {
  d <- mtcars
  or <- unname(exp(coef(glm(am ~ mpg + wt, binomial, d))["mpg"]))
  for (nm in h5_reserved) {
    dn <- d
    dn[[nm]] <- dn$mpg
    fn <- glm(stats::as.formula(paste0("am ~ `", nm, "` + wt")), binomial, dn)
    dl <- d
    dl$z <- dl$mpg
    attr(dl$z, "label") <- nm
    fl <- glm(am ~ z + wt, binomial, dl)
    for (f in list(fn, fl)) {
      lab <- paste(nm, if (identical(f, fn)) "named" else "labelled")
      tt <- regtab(f)
      row <- tt$meta$regtab_rows
      hit <- row$key %in% c(paste0("`", nm, "`"), nm, "z")
      expect_identical(sum(hit), 1L, label = lab)
      expect_equal(row$estimate[hit], or, tolerance = 1e-12, label = lab)
      expect_false("Intercept" %in% tt$body[[1]], label = lab)
      tk <- regtab(f, keepintercept = TRUE)
      rk <- tk$meta$regtab_rows
      hk <- rk$key %in% c(paste0("`", nm, "`"), nm, "z") & rk$label != "Intercept"
      expect_equal(rk$estimate[hk], or, tolerance = 1e-12, label = paste(lab, "keepintercept"))
      expect_true("Intercept" %in% tk$body[[1]], label = lab)
    }
  }
})

test_that("a covariate named p is kept and exponentiated, as by Stata 2.1.12 (P1, P2)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  auto <- golden_fixture("auto")
  auto$p <- as.vector(auto$mpg)  # Stata's `gen p = mpg` copies no label
  f <- glm(foreign ~ p + weight, binomial, auto, control = glm.control(epsilon = 1e-14))
  st1 <- ht2b_cells("P1")
  tt <- regtab(f)
  expect_identical(st1[-(1:2), 1], c("p", "Weight (lbs.)"))
  mm <- golden_compare_cells(golden_as_cells(tt), st1, mode = "exact")
  if (nrow(mm)) golden_fail(mm, "cells P1") else succeed()
  # keepintercept: the p row is its odds ratio, as the others; every cell
  # as Stata's except the intercept's upper bound (exp() of about 22.6,
  # which moves in the tenth digit with the convergence).
  st2 <- ht2b_cells("P2")
  tk <- regtab(f, keepintercept = TRUE)
  expect_identical(tk$body[[1]], st2[-(1:2), 1])
  expect_identical(tk$body[[2]], st2[-(1:2), 2])
  expect_identical(tk$body[[3]][1:2], st2[3:4, 3])
  expect_identical(tk$body[[4]], st2[-(1:2), 4])
})

test_that("existing ancillary rows keep Stata's nointercept behaviour, by role", {
  skip_if_not_installed("survival")
  skip_if_not_installed("MASS")
  lung <- survival::lung
  # Weibull: ln_p, p, 1/p dropped with the intercept; lognormal: lnsigma and
  # sigma kept (as Stata 2.1.12's rule; fixture lognormal_default).
  w <- survival::survreg(survival::Surv(time, status) ~ age, lung)
  expect_identical(regtab(w)$body[[1]], "age")
  expect_identical(regtab(w, keepintercept = TRUE)$body[[1]], c("age", "ln_p", "p", "1/p", "Intercept"))
  ln <- survival::survreg(survival::Surv(time, status) ~ age, lung, dist = "lognormal")
  expect_identical(regtab(ln)$body[[1]], c("age", "lnsigma", "sigma"))
  # nbreg's lnalpha/alpha, ologit's cutpoints.
  nb <- MASS::glm.nb(Days ~ Age, MASS::quine)
  expect_false(any(c("lnalpha", "alpha", "Intercept") %in% regtab(nb)$body[[1]]))
  expect_identical(utils::tail(regtab(nb, keepintercept = TRUE)$body[[1]], 3), c("lnalpha", "alpha", "Intercept"))
  o <- MASS::polr(factor(gear) ~ wt, mtcars, Hess = TRUE)
  expect_identical(regtab(o)$body[[1]], "wt")
  expect_identical(regtab(o, keepintercept = TRUE)$body[[1]], c("wt", "cut1", "cut2"))
})

test_that("a covariate named cut1 is neither dropped nor relabelled by cutlabels, and keeps its own row", {
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cut1 <- d$mpg
  o <- MASS::polr(factor(gear) ~ cut1 + wt, d, Hess = TRUE)
  expect_identical(regtab(o)$body[[1]], c("cut1", "wt"))
  tk <- regtab(o, keepintercept = TRUE, cutlabels = c("A", "B"))
  expect_identical(tk$body[[1]], c("cut1", "wt", "A", "B"))
  expect_equal(tk$meta$regtab_rows$estimate[1], exp(unname(coef(o)["cut1"])), tolerance = 1e-12)
  # Two parameters named p in one table: the Weibull's diparm keeps its own
  # row, labelled p as the covariate (Stata 2.1.12; golden R61 for ln_p).
  skip_if_not_installed("survival")
  lung <- survival::lung
  lung$p <- lung$age
  w <- survival::survreg(survival::Surv(time, status) ~ p, lung)
  tk <- regtab(w, keepintercept = TRUE)
  # (2.1.11 showed Stata's colname /p for the diparm, review T2B-14.)
  expect_identical(tk$body[[1]], c("p", "ln_p", "p", "1/p", "Intercept"))
})
