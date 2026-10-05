# Regression tests for the milestone 5w review findings (IMPLEMENTATION_PLAN.md,
# "Milestone 5w review outcome") and the 5w-only external-review findings
# F05, F07, F34-F37. Stata expectations: tests/testthat/fixtures/regtab_5w_review/
# (qa/stata/make_regtab_5w_review.do, Stata 17 + tabtools 2.1.12 since C5); the R calls
# are in helper-regtab-5w-review.R.

# ---------------------------------------------------------------------------
# Stata fixtures compared cell for cell (exact) and stored result for stored
# result. Stored tolerance per case (absolute below 1, relative above, as
# golden_compare_stored() measures): the smallest 1-2-5 step at or above the
# largest difference observed on 2026-09-26 (in the comment), never below
# the harness default 1e-9. The larger ones are convergence: Stata's
# default ML tolerance (probit, geeglm vs glm, nbreg's ln alpha against
# MASS's theta iteration).
w5r_tol <- c(
  D01 = 1e-9,  # 2.0e-12 logit [pw]
  D02 = 1e-9,  # 9.1e-15 regress [pw]
  D03 = 1e-9,  # 1.3e-10 poisson [pw]
  D05 = 1e-9,  # 3.3e-13 stcox [pw], id()
  D05b = 1e-9, # 3.3e-13
  D09 = 1e-9,  # 2.7e-12 constant [pw]
  D10 = 1e-9,  # 1.9e-12 integer [pw]
  D20 = 1e-9,  # 2.7e-12 oim / [pw] / [pw] + cluster
  D21 = 2e-9,  # 1.6e-9 probit [pw]
  E08 = 1e-9,  # 2.4e-11 poisson vce(robust)
  E09 = 1e-9,  # 2.0e-12 R2 / pseudo R2 label
  N05 = 1e-9,  # 2.6e-15 glm gaussian [pw]
  N06 = 1e-9,  # 2.6e-10 glm gamma [pw]
  D14 = 5e-9,  # 2.9e-9 geeglm as glm [pw] vce(cluster id)
  D06 = 1e-9,  # 1.1e-14 svy: regress R2
  D07 = 1e-9,  # 2.0e-12 svy: logit
  D11 = 1e-9,  # 1.8e-13 svy, subpop()
  D13 = 1e-9,  # 2.8e-14 xtgee [pw]: robust forced
  E06 = 1e-9,  # 2.8e-14 xtgee [pw], vce(robust)
  D16 = 5e-9,  # 3.4e-9 nbreg [pw]
  D16k = 1e-5, # 8.1e-6 nbreg [pw] ln alpha (MASS's theta iteration)
  N07 = 1e-9,  # 1.5e-15 stcox after stset [pw=integer weights]
  D12 = 1e-9,  # 8.8e-12 zero weights, an all-zero cluster
  N03 = 1e-9,  # 6.5e-15 regress, the same: t(G - 1) over non-empty clusters
  N04 = 5e-8,  # 3.5e-8 geeglm as glm with zero weights
  N01 = 1e-9,  # 1.4e-13 stcox [pw] with several records per subject
  N02 = 1e-9,  # 2.1e-13 stcox vce(robust), several records per subject
  D19 = 1e-9,  # 7.1e-13 empty factor level under [pw]
  D19b = 1e-9, # 3.5e-15
  E05 = 1e-9   # 3.7e-11 stcox default (Breslow) ties
)

# P0-6: with G - 1 below the number of coefficients, Stata's e(rank) after
# vce(cluster) is the rank of the robust e(V). tabtools 2.1.12 then counts
# the coefficients with a nonzero standard error, as R always counted the
# estimated parameters (take_action item 13, fixed; 2.1.11 used e(rank),
# which R did not replicate). The cases compare exactly now.
w5r_few <- c(
  D08 = 1e-9,  # 2.7e-12 logit, 3 clusters
  E01 = 1e-9,  # 2.7e-12 logit, 2 clusters
  E02 = 1e-9,  # 1.5e-14 regress, 3 clusters
  E03 = 1e-9,  # 3.7e-11 stcox, 3 clusters
  E07 = 2e-9   # 2.0e-9 probit, 3 clusters
)

expect_w5r <- function(id, tol, drop_rows = character(), drop_names = character()) {
  tt <- suppressWarnings(w5r_run(id))
  got <- golden_as_cells(tt)
  want <- w5r_cells(id)
  got <- got[!got[, 1] %in% drop_rows, , drop = FALSE]
  want <- want[!want[, 1] %in% drop_rows, , drop = FALSE]
  mm <- golden_compare_cells(got, want, mode = "exact")
  if (nrow(mm)) golden_fail(mm, paste("cells", id)) else succeed()
  st <- golden_methods_regtab(w5r_stored(id), id, "regtab_5w_review")
  st <- st[!st$name %in% drop_names, , drop = FALSE]
  why <- golden_compare_stored(tt$stored, st, tolerance = tol)
  if (length(why)) fail(paste0("stored ", id, ":\n", paste(utils::head(why, 8L), collapse = "\n"))) else succeed()
  invisible(tt)
}

w5r_skip <- function() {
  skip_on_cran()
  for (p in c("survival", "survey", "geepack", "MASS", "haven")) skip_if_not_installed(p)
}

test_that("every 5w review fixture matches Stata (cells exact, stored per case)", {
  w5r_skip()
  expect_setequal(intersect(c(names(w5r_tol), names(w5r_few)), names(w5r_cases)), names(w5r_cases))
  for (id in names(w5r_tol)) expect_w5r(id, w5r_tol[[id]])
})

test_that("few clusters: every cell matches Stata, AIC/BIC counting the parameters (P0-6, tabtools 2.1.12)", {
  w5r_skip()
  G <- c(D08 = 3, E01 = 2, E02 = 3, E03 = 3, E07 = 3)
  for (id in names(w5r_few)) {
    tt <- expect_w5r(id, w5r_few[[id]])
    b <- stats::coef(eval(w5r_cases[[id]][[2L]]))
    # G - 1 is below the parameter count, so e(rank) alone would undercount.
    expect_lt(G[[id]] - 1, length(b), label = id)
    expect_equal(tt$stored$aic_1, -2 * tt$stored$ll_1 + 2 * length(b), tolerance = 1e-12)
  }
})

# ---------------------------------------------------------------------------
# P0-1 / F36: the rows a robust variance is computed from

w5r_resort <- function(d) {
  d <- d[order(d$x1, decreasing = TRUE), ]
  rownames(d) <- NULL
  d
}

test_that("re-sorting the data after fitting never changes a robust or cluster variance (P0-1, F36)", {
  w5r_skip()
  d0 <- w5r_syn()
  d <- d0
  S <- survival::Surv
  fits <- list(
    lm = lm(yc ~ treat + x1, d, weights = iptw),
    lm_nomodel = lm(yc ~ treat + x1, d, weights = iptw, model = FALSE),
    glm = glm(y ~ treat + x1, quasibinomial, d, weights = iptw),
    glm_nomodel = glm(y ~ treat + x1, quasibinomial, d, weights = iptw, model = FALSE),
    cox = survival::coxph(S(time, event) ~ treat + x1, d, weights = iptw, ties = "breslow"),
    cox_id = survival::coxph(S(time, event) ~ treat + x1, d, weights = iptw, id = id, ties = "breslow"),
    cox_unw = survival::coxph(S(time, event) ~ treat + x1, d, ties = "breslow"),
    cox_model = survival::coxph(S(time, event) ~ treat + x1 + cluster(clusu), d, ties = "breslow", model = TRUE)
  )
  calls <- list(
    robust = function(f) tt_vcov(f, "robust"),
    cluster = function(f) tt_vcov(f, "cluster", ~clusu),
    own = function(f) if (inherits(f, "coxph") && !is.null(tabtools:::.rt_own_grouping(f))) tt_vcov(f, "cluster") else tt_vcov(f, "robust"),
    stata = function(f) tt_vcov(f, "stata")
  )
  # The variances that need rows the fit did not keep, and so may refuse.
  may_refuse <- c(paste(c("lm", "lm_nomodel", "cox", "cox_id", "cox_unw", "cox_model"), "cluster"),
                  "lm_nomodel robust", "lm_nomodel own", "cox_unw robust", "cox_unw own")
  before <- lapply(fits, function(f) lapply(calls, function(g) g(f)))
  tab_before <- lapply(fits, function(f) regtab(f, vce = "robust")$body)
  tab_w10 <- regtab(fits$cox, vce = "cluster", cluster = ~clusu, stats = "n")$body
  tab_w04 <- suppressMessages(regtab(fits$cox_id, vce = "robust", stats = "n"))$body
  d <- w5r_resort(d0)  # the same data object, re-sorted, row names reset
  for (nm in names(fits)) {
    for (v in names(calls)) {
      r <- tryCatch(calls[[v]](fits[[nm]]), error = function(e) e)
      if (inherits(r, "error")) {
        # Only a variance that needs rows the fit did not keep may refuse,
        # and it says why.
        expect_match(conditionMessage(r), "changed since", label = paste(nm, v))
        expect_true(paste(nm, v) %in% may_refuse, label = paste(nm, v, "refused"))
      } else {
        expect_identical(r, before[[nm]][[v]], label = paste(nm, v))
      }
    }
    r <- tryCatch(regtab(fits[[nm]], vce = "robust")$body, error = function(e) e)
    if (!inherits(r, "error")) expect_identical(r, tab_before[[nm]], label = paste(nm, "table"))
  }
  # Fits that keep their rows never refuse: glm (data), coxph's own robust
  # variance (weighted, id, cluster()), and model = TRUE.
  expect_identical(tt_vcov(fits$glm, "cluster", ~clusu), before$glm$cluster)
  expect_identical(tt_vcov(fits$cox_id, "robust"), before$cox_id$robust)
  expect_identical(tt_vcov(fits$cox, "robust"), before$cox$robust)
  expect_identical(tt_vcov(fits$cox_model, "cluster"), before$cox_model$own)
  # The W04 shape (stset [pw], id(): weighted coxph with id, robust).
  expect_identical(suppressMessages(regtab(fits$cox_id, vce = "robust", stats = "n"))$body, tab_w04)
  # The W10 shape (cluster = ~id on a coxph that kept no data) refuses.
  expect_error(regtab(fits$cox, vce = "cluster", cluster = ~clusu, stats = "n"), "changed since")
  expect_error(regtab(fits$lm, vce = "cluster", cluster = ~clusu), "changed since")
  # Back in the fitted order the same calls work again, identically.
  d <- d0
  expect_identical(regtab(fits$cox, vce = "cluster", cluster = ~clusu, stats = "n")$body, tab_w10)
  expect_identical(tt_vcov(fits$lm, "cluster", ~clusu), before$lm$cluster)
  # A cluster vector is taken as given (in the order of the fitted rows).
  expect_identical(tt_vcov(fits$glm, "cluster", d0$clusu), before$glm$cluster)
})

test_that("editing the weight column after fitting is caught (P0-1)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  cx <- survival::coxph(survival::Surv(time, event) ~ treat + x1, d, weights = iptw, ties = "breslow")
  V <- tt_vcov(cx, "cluster", ~clusu)
  d$iptw <- 1
  expect_error(tt_vcov(cx, "cluster", ~clusu), "changed since")
  expect_identical(tt_vcov(cx, "robust"), tt_vcov(cx, "robust"))  # the fit's own sandwich
})

# ---------------------------------------------------------------------------
# P0-2: weighted fits never get non-robust standard errors silently

test_that("a weighted geeglm is xtgee [pweight]: robust by default (P0-2)", {
  skip_if_not_installed("geepack")
  d <- w5r_pp(FALSE)
  g <- suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, d, id = id, weights = wid, corstr = "independence"))
  expect_identical(tt_vcov(g), tt_vcov(g, "robust"))
  u <- geepack::geeglm(ev ~ a + z, binomial, d, id = id, corstr = "independence")
  expect_false(isTRUE(all.equal(tt_vcov(u), tt_vcov(u, "robust"))))
  expect_no_warning(regtab(g))
})

test_that("every weighted class without a robust default warns (P0-2, W32)", {
  skip_on_cran()
  for (p in c("MASS", "nnet", "survival")) skip_if_not_installed(p)
  d <- w5r_syn()
  d$f3f <- factor(d$f3)
  S <- survival::Surv
  msg <- "inverse-probability weights|model-based standard errors"
  # glm.nb: warns, and now supports vce = "robust" (nbreg [pw]).
  nb <- MASS::glm.nb(cnt ~ treat + x1 + offset(log(expo)), d, weights = iptw)
  expect_warning(regtab(nb), "vce = \"robust\"")
  expect_no_warning(regtab(nb, vce = "robust"))
  # coxph with integer weights, and robust = FALSE: survival's naive variance.
  expect_warning(regtab(survival::coxph(S(time, event) ~ treat + x1, d, weights = iw, ties = "breslow")),
                 "model-based standard errors")
  expect_warning(regtab(survival::coxph(S(time, event) ~ treat + x1, d, weights = iptw, robust = FALSE,
                                        ties = "breslow")), "model-based standard errors")
  expect_no_warning(regtab(survival::coxph(S(time, event) ~ treat + x1, d, weights = iw, ties = "breslow"),
                           vce = "robust"))
  expect_no_warning(suppressMessages(regtab(survival::coxph(S(time, event) ~ treat + x1, d, weights = iptw,
                                                            ties = "breslow"))))
  # Classes regtab has no robust variance for.
  po <- suppressWarnings(MASS::polr(f3f ~ treat + x1, d, weights = iptw, Hess = TRUE))
  expect_warning(regtab(po), msg)
  expect_warning(regtab(nnet::multinom(f3f ~ treat + x1, d, weights = iptw, trace = FALSE)), msg)
  expect_warning(regtab(survival::survreg(S(time, event) ~ treat + x1, d, weights = iptw)), "robust = TRUE")
  skip_if_not_installed("ordinal")
  expect_warning(regtab(ordinal::clm(f3f ~ treat + x1, data = d, weights = iptw)), msg)
  # (Fits whose own warnings or unrelated regtab notes would muddle
  # expect_warning(): any of the warnings must be regtab's.)
  warns <- function(expr) expect_true(any(grepl(msg, testthat::capture_warnings(expr))))
  skip_if_not_installed("pscl")
  zi <- suppressWarnings(pscl::zeroinfl(cnt ~ treat + x1 | 1, d, weights = iptw))
  hu <- suppressWarnings(pscl::hurdle(cnt ~ treat + x1 | 1, d, weights = iptw))
  warns(regtab(zi))
  warns(regtab(hu))
  skip_if_not_installed("lme4")
  lm4 <- suppressWarnings(lme4::lmer(yc ~ treat + x1 + (1 | clusu), d, weights = iptw, REML = FALSE))
  warns(regtab(lm4))
  skip_if_not_installed("glmmTMB")
  tmb <- suppressWarnings(glmmTMB::glmmTMB(y ~ treat + x1 + (1 | clusu), family = binomial, data = d, weights = iptw))
  warns(suppressMessages(regtab(tmb)))
})

test_that("the IPTW warning fires when any weight is non-integer (W32)", {
  d <- w5r_syn()
  d$w1 <- ifelse(d$treat == 1, round(d$iptw, 2), 1)  # untreated weight exactly 1
  expect_true(any(d$w1 == 1) && any(d$w1 != round(d$w1)))
  g <- glm(y ~ treat + x1, quasibinomial, d, weights = w1)
  expect_warning(regtab(g), "inverse-probability weights")
  expect_true(tabtools:::.rt_iptw_like_w(c(1, 1, 1, 2.5)))
  expect_false(tabtools:::.rt_iptw_like_w(c(1, 2, 3)))
  expect_false(tabtools:::.rt_iptw_like_w(c(2.5, 2.5)))
})

test_that("glm.nb robust and cluster variance is nbreg's joint sandwich (P0-2)", {
  skip_if_not_installed("MASS")
  d <- w5r_syn()
  nb <- MASS::glm.nb(cnt ~ treat + x1 + offset(log(expo)), d, weights = iptw)
  expect_identical(tt_vce_types(nb), c("stata", "model", "robust", "cluster"))
  V <- tt_vcov(nb, "robust")
  Vc <- tt_vcov(nb, "cluster", ~clusu)
  expect_false(isTRUE(all.equal(V, Vc)))
  expect_identical(dimnames(V), list(names(coef(nb)), names(coef(nb))))
  expect_error(tt_vcov(nb, "cluster"), "needs `cluster`")
})

# ---------------------------------------------------------------------------
# P0-3 / F34 / F07: profile-likelihood intervals

test_that("profile intervals are refused with a robust or cluster vce (P0-3, F34)", {
  d <- w5r_syn()
  g <- glm(y ~ treat + x1, quasibinomial, d, weights = iptw)
  expect_error(regtab(g, vce = "robust", ci_method = "profile"), "cannot be combined")
  expect_error(regtab(g, vce = "cluster", cluster = ~clusu, ci_method = "profile"), "cannot be combined")
  expect_error(regtab(lm(yc ~ treat, d), vce = "robust", ci_method = "profile"), "cannot be combined")
  g0 <- glm(am ~ wt, binomial(), mtcars)
  expect_error(regtab(g0, vce = "robust", ci_method = "profile"), "Model 1|cannot be combined")
  # Model-based fits keep them.
  tt <- regtab(g0, ci_method = "profile")
  ci <- suppressMessages(confint(g0))
  r <- tt$meta$regtab_rows
  expect_equal(log(r$conf.low[r$key == "wt"]), ci["wt", 1], tolerance = 1e-10)
  # Classes whose rows are not profile intervals are refused by name.
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, event) ~ treat, d, ties = "breslow")
  expect_error(regtab(cx, ci_method = "profile"), "not available for <coxph>")
  expect_error(regtab(g0, cx, ci_method = "profile"), "model 2")
})

test_that("a failed profile is an error, never a silent Wald fallback (F07)", {
  m <- glm(am ~ mpg + wt, binomial(), mtcars, model = FALSE)
  m$call$data <- quote(nonexistent_review_data)
  m$data <- NULL
  expect_error(tabtools:::tt_wald(m, ci_method = "profile", vce = "model"), "Profile-likelihood intervals failed")
  expect_error(regtab(m, ci_method = "profile"))
  skip_if_not_installed("nnet")
  mn <- nnet::multinom(factor(cyl) ~ wt, mtcars, trace = FALSE)
  expect_error(regtab(mn, ci_method = "profile"), "not available for <multinom>")
})

# ---------------------------------------------------------------------------
# F05, F35, F37 / P1-1

test_that("glm(y = FALSE) gives Stata's variance, not vcov() (F05)", {
  f <- glm(am ~ mpg + wt, binomial("probit"), mtcars, control = glm.control(epsilon = 1e-12))
  g <- glm(am ~ mpg + wt, binomial("probit"), mtcars, control = glm.control(epsilon = 1e-12), y = FALSE)
  expect_identical(regtab(f)$body, regtab(g)$body)
  expect_equal(tt_vcov(g), tt_vcov(f), tolerance = 1e-12)
  expect_equal(tt_vcov(g, "robust"), tt_vcov(f, "robust"), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(tt_vcov(f), vcov(f), tolerance = 1e-8)))
})

test_that("cluster is refused when no model uses vce = 'cluster' (F35)", {
  g <- glm(am ~ wt, binomial(), mtcars)
  expect_error(regtab(g, vce = "robust", cluster = ~cyl), "would be ignored")
  expect_error(regtab(g, vce = "stata", cluster = ~cyl), "would be ignored")
  expect_error(tt_vcov(g, "robust", cluster = ~cyl), "only with")
  # A single cluster beside per-model vce reaches the cluster models.
  expect_no_error(regtab(g, g, vce = list("robust", "cluster"), cluster = ~cyl))
})

test_that("lm(na.action = na.exclude) robust/cluster equals na.omit (P1-1, F37)", {
  d <- w5r_syn()
  d$x1[c(3, 30, 300)] <- NA
  a <- lm(yc ~ treat + x1, d, weights = iptw, na.action = na.exclude)
  b <- lm(yc ~ treat + x1, d, weights = iptw)
  expect_equal(tt_vcov(a, "robust"), tt_vcov(b, "robust"), tolerance = 1e-14)
  expect_equal(tt_vcov(a, "cluster", ~clusu), tt_vcov(b, "cluster", ~clusu), tolerance = 1e-14)
  expect_identical(regtab(a, vce = "robust", stats = c("n", "ll"))$body,
                   regtab(b, vce = "robust", stats = c("n", "ll"))$body)
  expect_false(anyNA(sqrt(diag(tt_vcov(a, "robust")))))
  skip_if_not_installed("survival")
  ca <- survival::coxph(survival::Surv(time, event) ~ treat + x1, d, ties = "breslow", na.action = na.exclude)
  cb <- survival::coxph(survival::Surv(time, event) ~ treat + x1, d, ties = "breslow")
  expect_equal(tt_vcov(ca, "cluster", ~clusu), tt_vcov(cb, "cluster", ~clusu), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# P0-4 notes, P1-2 mutations, P1-3 ties

test_that("[pw] statistics: shown where verified, blank with a note otherwise (P0-4)", {
  d <- w5r_syn()
  # A binomial with trials given as weights: unverified, blank with a note.
  mp <- transform(mtcars, p = round(cyl) / 10, n10 = 10)
  gp <- glm(p ~ wt, binomial, mp, weights = n10)
  expect_message(tt <- regtab(gp, vce = "robust", stats = c("n", "ll", "aic")), "not been verified for this family")
  expect_null(tt$stored$ll_1)
  # The inverse Gaussian [pw] pseudo-log-likelihood is verified since the
  # pre-release review (P0-1; Stata fixtures L14, L15 in
  # test-regtab-glm-ll.R): scale 1, raw weights.
  ig <- suppressWarnings(glm(ypos ~ treat + x1, inverse.gaussian("log"), d, weights = iptw))
  expect_no_message(ti <- regtab(ig, vce = "robust", stats = c("n", "ll", "aic")))
  y <- ig$y
  mu <- fitted(ig)
  expect_equal(ti$stored$ll_1, -0.5 * sum(d$iptw * ((y - mu)^2 / (y * mu^2) + log(2 * pi * y^3))), tolerance = 1e-12)
  # Unweighted robust fits keep the ordinary log-likelihood (no note).
  g <- glm(y ~ treat + x1, binomial, d)
  expect_no_message(tu <- regtab(g, vce = "robust", stats = c("ll", "r2")))
  expect_equal(tu$stored$ll_1, as.numeric(logLik(g)), tolerance = 1e-12)
  # A grouped binomial has trials, not weights: no [pw] reading (P2-4).
  mt <- mtcars
  mt$s <- round(mt$cyl)
  gb <- glm(cbind(s, 10 - s) ~ wt, binomial, mt)
  expect_no_message(regtab(gb, vce = "robust", stats = "ll"))
  # A weighted Efron Cox fit has no Stata analogue: blank, with a note.
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, event) ~ treat, d, weights = iptw)
  # (Its other notes, about Efron ties and the weighted subject count, are
  # captured too, so the check prints nothing: pre-release review P3-6.)
  msgs <- testthat::capture_messages(te <- regtab(cx, stats = c("n", "ll")))
  expect_true(any(grepl("Efron ties have no Stata analogue", msgs)))
  expect_null(te$stored$ll_1)
})

test_that("zero-weight clusters: lm cluster df is t(G - 1) over non-empty clusters (W15)", {
  d <- w5r_syn()
  l <- lm(yc ~ treat + x1 + x2, d, weights = wz)
  expect_identical(tabtools:::tt_wald_df(l, "cluster", ~clusz), 4)  # Stata e(df_r) 4: 5 of 6 clusters
  expect_identical(length(unique(d$clusz)), 6L)
})

test_that("weighted Cox Subjects: once per subject, blank when weights vary within id (W26, P2-7, policy c)", {
  skip_if_not_installed("survival")
  p <- w5r_pp(FALSE)
  S <- survival::Surv
  cw <- survival::coxph(S(t0, t1, ev) ~ a + z, p, weights = wid, id = id, ties = "breslow")
  s <- tabtools:::tt_model_stats.coxph(cw, tabtools:::tt_model_info(cw))
  expect_equal(s$N_sub, sum(p$wid[!duplicated(p$id)]), tolerance = 1e-12)
  expect_gt(sum(p$wid), s$N_sub + 100)  # rows would count each subject several times
  expect_message(regtab(cw, stats = "n"), "sum of the subjects' weights")
  cv <- survival::coxph(S(t0, t1, ev) ~ a + z, p, weights = sw, id = id, ties = "breslow")
  expect_message(tt <- regtab(cv, stats = c("n", "ll")), "weights vary within subject")
  expect_false(any(tt$body[[1]] %in% c("Subjects", "Observations")))
  expect_null(tt$stored$n_1)
})

test_that("Cox Efron fits get a one-time note: stcox defaults to Breslow (P1-3)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  withr_opt <- options(rlib_message_verbosity = "verbose")
  on.exit(options(withr_opt), add = TRUE)
  expect_message(regtab(survival::coxph(survival::Surv(time, event) ~ treat, d)), "ties = \"breslow\"")
  expect_message(regtab(survival::coxph(survival::Surv(time, event) ~ treat, d, weights = iptw)),
                 "no Stata analogue")
  expect_no_message(regtab(survival::coxph(survival::Surv(time, event) ~ treat, d, ties = "breslow")))
  # No ties, no note.
  d$tu <- d$time + seq_len(nrow(d)) / 1e4
  expect_no_message(regtab(survival::coxph(survival::Surv(tu, event) ~ treat, d)))
})

# ---------------------------------------------------------------------------
# P2 findings and the tt_vcov() API

test_that("tt_vcov(gee_as =) is validated in the method (P2-1) and cluster refused early (P2-2)", {
  skip_if_not_installed("geepack")
  d <- w5r_pp(FALSE)
  ge <- geepack::geeglm(ev ~ a + z, binomial, d, id = id, corstr = "exchangeable")
  expect_error(tt_vcov(ge, gee_as = "glm"), "independence working correlation")
  expect_error(tt_vcov(ge, gee_as = "bogus"), "must be")
  gi <- geepack::geeglm(ev ~ a + z, binomial, d, id = id, corstr = "independence")
  expect_error(regtab(gi, vce = "cluster", cluster = ~site), "Model 1")
  expect_error(regtab(gi, vce = "cluster", cluster = ~site), "own `id`")
})

test_that("tt_vcov(complete = FALSE) drops aliased rows, for marginaleffects (P2-3)", {
  d <- w5r_syn()
  d$x1b <- d$x1 * 2
  g <- glm(y ~ treat + x1 + x1b, quasibinomial, d, weights = iptw)
  V <- tt_vcov(g, "robust")
  V2 <- tt_vcov(g, "robust", complete = FALSE)
  expect_true(all(is.na(V["x1b", ])))
  expect_identical(rownames(V2), c("(Intercept)", "treat", "x1"))
  expect_identical(V2, V[rownames(V2), rownames(V2)])
  expect_error(tt_vcov(g, "robust", complete = NA), "TRUE or FALSE")
  skip_if_not_installed("marginaleffects")
  # The fit is rank deficient on purpose; marginaleffects says so once per
  # session. Only that notice is muffled.
  ac <- withCallingHandlers(
    marginaleffects::avg_comparisons(g, variables = "treat", vcov = V2, wts = d$iptw),
    warning = function(w) if (grepl("rank deficient", conditionMessage(w))) invokeRestart("muffleWarning")
  )
  expect_false(anyNA(ac$std.error))
})

test_that("the Fine-Gray refusal names Fine-Gray fits (P2-5)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  d$ev3 <- factor(ifelse(d$event == 1, ifelse(d$x2 == 1, 1, 2), 0), 0:2, c("censor", "a", "b"))
  fg <- survival::finegray(survival::Surv(time, ev3) ~ ., data = d[, c("time", "ev3", "treat")], etype = "a")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treat, fg, weights = fgwt)
  expect_error(tt_vcov(f, "robust"), "Fine-Gray")
  expect_error(tt_vcov(f, "robust"), "stcrreg")
})

test_that("vce_note names the variance of fits that are robust by default (P2-6)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  S <- survival::Surv
  cw <- survival::coxph(S(time, event) ~ treat, d, weights = iptw, id = id, ties = "breslow")
  expect_identical(suppressMessages(regtab(cw, vce_note = TRUE))$footnote, "Standard errors: robust, clustered by id.")
  cc <- survival::coxph(S(time, event) ~ treat + cluster(clusu), d, ties = "breslow")
  expect_identical(regtab(cc, vce_note = TRUE)$footnote, "Standard errors: robust, clustered by clusu.")
  cr <- survival::coxph(S(time, event) ~ treat, d, weights = iptw, ties = "breslow")
  expect_identical(suppressMessages(regtab(cr, vce_note = TRUE))$footnote, "Standard errors: robust.")
  sr <- survival::survreg(S(time, event) ~ treat, d, robust = TRUE)
  expect_identical(regtab(sr, vce_note = TRUE)$footnote, "Standard errors: robust.")
  expect_identical(regtab(survival::coxph(S(time, event) ~ treat, d, ties = "breslow"), vce_note = TRUE)$footnote, "")
  skip_if_not_installed("geepack")
  p <- w5r_pp(FALSE)
  g <- suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, p, id = id, weights = wid, corstr = "independence"))
  expect_identical(regtab(g, vce_note = TRUE)$footnote, "Standard errors: robust, clustered by id.")
})

test_that("zero weights raise no summary.glm() dispersion warning (P2-8)", {
  d <- w5r_syn()
  g <- glm(y ~ treat + x1 + x2, quasibinomial, d, weights = wz)
  expect_no_warning(regtab(g, vce = "cluster", cluster = ~clusz, stats = "n"))
  expect_no_warning(regtab(g, vce = "model"))
})

test_that("svyglm: R2 for Gaussian identity fits, Subjects under subset() (P0-5)", {
  skip_if_not_installed("survey")
  d <- w5r_syn()
  des <- survey::svydesign(ids = ~1, weights = ~iptw, data = d)
  s <- survey::svyglm(yc ~ treat + x1, des)
  l <- lm(yc ~ treat + x1, d, weights = iptw)
  expect_equal(tabtools:::tt_model_stats.svyglm(s, NULL)$r2, summary(l)$r.squared, tolerance = 1e-12)
  sl <- survey::svyglm(y ~ treat + x1, des, family = quasibinomial())
  expect_no_message(tt <- regtab(sl, stats = c("n", "ll", "aic", "r2")))
  expect_identical(tt$body[[1]][nrow(tt$body)], "Observations")
  ss <- survey::svyglm(y ~ treat + x1, subset(des, x2 == 1), family = quasibinomial())
  ts <- regtab(ss, stats = "n")
  expect_identical(ts$body[[1]][nrow(ts$body)], "Subjects")
  expect_equal(ts$stored$n_1, sum(d$x2 == 1))
})

test_that("robust standard errors equal Stata's e(V) (several records per subject, integer weights, nbreg, xtgee)", {
  # The fixtures' r(table) holds estimates only; <id>_se.csv has Stata's
  # standard errors. Kills W21 (grouping by row, or G = rows, instead of by
  # subject when a subject has several records).
  w5r_skip()
  se_case <- function(id) {
    x <- utils::read.csv(w5r_path(paste0(id, "_se.csv")), strip.white = TRUE, stringsAsFactors = FALSE)
    x$term[x$term == "_cons"] <- "(Intercept)"
    x
  }
  S <- survival::Surv
  d <- w5r_syn()
  p <- w5r_pp(FALSE)
  fits <- list(
    N01 = list(survival::coxph(S(t0, t1, ev) ~ a + z, p, weights = wid, id = id, ties = "breslow"), "stata", 1e-11),
    N02 = list(survival::coxph(S(t0, t1, ev) ~ a + z, p, id = id, ties = "breslow"), "robust", 1e-11),
    N07 = list(survival::coxph(S(time, event) ~ treat + x1 + x2, d, weights = iw, id = id, ties = "breslow"),
               "robust", 1e-11),
    D05 = list(survival::coxph(S(time, event) ~ treat + x1 + x2, d, weights = iptw, id = id, ties = "breslow"),
               "stata", 1e-11),
    D13 = list(suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, p, id = id, weights = wid,
                                                corstr = "independence", control = w5r_gee_ctl())), "stata", 1e-9),
    D16 = list(MASS::glm.nb(cnt ~ treat + x1 + offset(log(expo)), d, weights = iptw), "robust", 2e-8)
  )
  for (id in names(fits)) {
    f <- fits[[id]]
    st <- se_case(id)
    V <- if (inherits(f[[1]], "negbin")) tabtools:::.rt_vcov_sandwich(f[[1]], "robust", joint = TRUE) else
      tt_vcov(f[[1]], f[[2]])
    se <- sqrt(diag(V))[st$term]
    expect_false(anyNA(se), label = id)
    anc <- st$term == "lnalpha"
    expect_lt(max(abs(se[!anc] / st$se[!anc] - 1)), f[[3]], label = paste(id, "SE vs Stata"))
    # nbreg's ln alpha: MASS's theta iteration stops at a tolerance of
    # epsilon^(1/4), ~3e-5 from Stata's ln alpha, which moves its SE by 6e-5.
    if (any(anc)) expect_lt(abs(se[anc] / st$se[anc] - 1), 1e-4, label = paste(id, "lnalpha SE"))
  }
  # G is the number of subjects (400), not of records (1,557).
  expect_identical(fits$N01[[1]]$n.id, 400L)
  expect_identical(tabtools:::.rt_robust_groups(fits$N01[[1]]), 400L)
})

# ---------------------------------------------------------------------------
# Code review of the fixes (2026-09-26): edge cases of the rows check and
# the pseudo-likelihood.

test_that("fix review: na.exclude [pw] statistics equal na.omit's", {
  d <- w5r_syn()
  d$x1[c(3, 9)] <- NA
  a <- glm(y ~ x1 + treat, quasibinomial, d, weights = iptw, na.action = na.exclude)
  b <- glm(y ~ x1 + treat, quasibinomial, d, weights = iptw)
  S <- c("n", "ll", "aic", "bic", "r2")
  expect_identical(regtab(a, vce = "robust", stats = S)$body, regtab(b, vce = "robust", stats = S)$body)
})

test_that("fix review: editing a strata() variable after fitting is refused; restoring it works", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  d0 <- d
  f <- survival::coxph(survival::Surv(time, event) ~ treat + x1 + survival::strata(clus3), d, ties = "breslow")
  V <- tt_vcov(f, "cluster", ~clusu)
  d$clus3[d$clus3 == 2] <- 1
  expect_error(tt_vcov(f, "cluster", ~clusu), "changed since")
  d <- d0
  d$extra <- 1  # adding a column is harmless
  expect_identical(tt_vcov(f, "cluster", ~clusu), V)
})

test_that("fix review: a clustered coxph's default table survives a re-sort (G is order-free)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  f <- survival::coxph(survival::Surv(time, event) ~ treat + x1, d, cluster = clusu, ties = "breslow")
  t0 <- regtab(f)$body
  d <- w5r_resort(d)
  expect_identical(regtab(f)$body, t0)
})

test_that("fix review: re-sorting with the row names kept is matched by row name (lm, coxph)", {
  skip_if_not_installed("survival")
  d <- w5r_syn()
  l <- lm(yc ~ treat + x1, d, weights = iptw)
  cx <- survival::coxph(survival::Surv(time, event) ~ treat + x1, d, weights = iptw, ties = "breslow")
  Vl <- tt_vcov(l, "cluster", ~clusu)
  Vc <- tt_vcov(cx, "cluster", ~clusu)
  d <- d[order(d$x1), ]
  expect_identical(tt_vcov(l, "cluster", ~clusu), Vl)
  expect_equal(tt_vcov(cx, "cluster", ~clusu), Vc, tolerance = 1e-14)
})

test_that("fix review: binomial fits with zero weights and model = FALSE are not refused", {
  d <- w5r_syn()
  d$w3 <- rep(c(0, 1, 2), 300)
  f <- glm(y ~ x1 + treat, binomial, d, weights = w3, model = FALSE)
  g <- glm(y ~ x1 + treat, binomial, d, weights = w3)
  expect_identical(regtab(f)$body, regtab(g)$body)
  expect_identical(tt_vcov(f, "robust"), tt_vcov(g, "robust"))
})

test_that("fix review: a weighted geeglm drops all-zero-weight clusters from G, Groups and N", {
  skip_if_not_installed("geepack")
  p <- w5r_pp(FALSE)
  p$wx <- ifelse(p$id <= 5, 0, p$wid)
  g <- suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, p, id = id, weights = wx, corstr = "independence"))
  expect_identical(tabtools:::.rt_gee_groups(g), 395L)
  s <- tabtools:::tt_model_stats.geeglm(g, NULL)
  expect_identical(s$groups, 395L)
  expect_identical(s$N, sum(p$wx > 0))
  expect_equal(tt_vcov(g), as.matrix(vcov(g)) * 395 / 394, tolerance = 1e-14, ignore_attr = TRUE)
})
