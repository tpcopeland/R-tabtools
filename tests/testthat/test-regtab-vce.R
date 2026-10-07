# Milestone 5w (tasks 5.9-5.13): weighted and robust variance for regtab.
# Stata expectations: qa/stata/make_regtab_vce.do (Stata 17, built-in
# estimation commands) -> fixtures/regtab_vce/ (vce_probe.csv: b, se, N,
# N_clust, df_r per probe; vce_V.csv: every e(V) element; margins.csv).
# Per-probe tolerances are the smallest 1-2-5 step at or above the maximum
# relative SE difference observed on 2026-09-25 (in the comments), never
# below 1e-12 (floating-point noise across platforms); the
# larger ones are Stata's default ML convergence (its logit stops ~5e-8
# from the maximum R's glm(epsilon = 1e-12) reaches), not the formula.

vce_dir <- function() test_path("fixtures", "regtab_vce")

vce_probe <- function(id) {
  p <- utils::read.csv(file.path(vce_dir(), "vce_probe.csv"), strip.white = TRUE,
                       na.strings = ".", stringsAsFactors = FALSE)
  p$term <- sub("^.*:", "", p$term)
  p <- p[p$probe == id & p$se > 0, , drop = FALSE]
  if (!nrow(p)) stop("no probe ", id)
  p
}

vce_V <- function(id) {
  v <- utils::read.csv(file.path(vce_dir(), "vce_V.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  v$row <- sub("^.*:", "", v$row)
  v$col <- sub("^.*:", "", v$col)
  v[v$probe == id, , drop = FALSE]
}

# Stata coefficient names -> R's.
r_names <- function(x) {
  x[x == "_cons"] <- "(Intercept)"
  x <- sub("^([0-9]+)\\.period$", "period\\1", x)
  sub("^1\\.treated$", "treatedf1", x)
}

# Maximum relative SE difference against a probe, over `terms` (Stata
# names; default every term).
se_err <- function(V, id, terms = NULL) {
  p <- vce_probe(id)
  if (!is.null(terms)) p <- p[p$term %in% terms, , drop = FALSE]
  se <- sqrt(diag(V))[r_names(p$term)]
  expect_false(anyNA(se), label = paste(id, "terms"))
  max(abs(se / p$se - 1))
}

# Maximum relative difference over every element of e(V).
V_err <- function(V, id) {
  v <- vce_V(id)
  got <- V[cbind(r_names(v$row), r_names(v$col))]
  big <- abs(v$value) > 1e-12
  max(abs(got[big] / v$value[big] - 1))
}

ctl <- glm.control(epsilon = 1e-12, maxit = 100)
key <- c("treated", "index_age", "female", "_cons")

cohort_vce <- function() {
  d <- golden_fixture("cohort")
  d$fu_y <- d$follow_up / 365.25
  # Zero weights for every tenth subject (probes V13, V14).
  d$w0 <- d$iptw * (d$id %% 10 != 0)
  d
}

# ---------------------------------------------------------------------------
# glm (ML) and lm formulas

test_that("glm [pweight]/vce(robust): observed-information sandwich x N/(N - 1), z", {
  d <- cohort_vce()
  g <- glm(cv_event ~ treated + index_age + female, quasibinomial, d, weights = iptw, control = ctl)
  V <- tt_vcov(g, "robust")
  expect_lt(se_err(V, "V01_logit_pw"), 1e-8)          # 5.7e-9 (logit's convergence)
  expect_lt(se_err(V, "V07_glm_logit_pw"), 1e-12)     # 1.2e-13 (glm converges tightly)
  expect_lt(V_err(V, "V07_glm_logit_pw"), 1e-11)      # every element of e(V)
  expect_identical(tabtools:::tt_wald_df(g, "robust"), Inf)
  # The factor N/(N - 1) is what separates it from HC0.
  n <- nrow(d)
  hc0 <- V * (n - 1) / n
  expect_gt(se_err(hc0, "V07_glm_logit_pw"), 3e-5)
  # binomial and quasibinomial give the same sandwich.
  g2 <- suppressWarnings(glm(cv_event ~ treated + index_age + female, binomial, d, weights = iptw, control = ctl))
  expect_equal(tt_vcov(g2, "robust"), V, tolerance = 1e-10)
  # Unweighted vce(robust).
  g3 <- glm(cv_event ~ treated + index_age + female, binomial, d, control = ctl)
  expect_lt(se_err(tt_vcov(g3, "robust"), "V02_logit_robust"), 1e-8)  # 5.5e-9
})

test_that("non-canonical links use the observed-information bread (probit, cloglog)", {
  d <- cohort_vce()
  pr <- suppressWarnings(glm(cv_event ~ treated + index_age + female, binomial("probit"), d,
                             weights = iptw, control = ctl))
  expect_lt(se_err(tt_vcov(pr, "robust"), "V06_probit_pw"), 1e-9)     # 1.8e-10
  cl <- suppressWarnings(glm(cv_event ~ treated + index_age + female, binomial("cloglog"), d,
                             weights = iptw, control = ctl))
  expect_lt(se_err(tt_vcov(cl, "robust"), "V09_glm_cloglog_pw"), 2e-9)  # 1.4e-9
  skip_if_not_installed("sandwich")
  # sandwich's expected-information bread is 1.5e-3 off for probit.
  hc <- sandwich::vcovHC(pr, type = "HC0") * nrow(d) / (nrow(d) - 1)
  expect_gt(se_err(hc, "V06_probit_pw"), 1e-3)
})

test_that("Poisson with exposure and Gaussian glm [pweight], robust and 3-cluster", {
  d <- cohort_vce()
  po <- glm(cv_event ~ treated + index_age + female + offset(log(fu_y)), poisson, d, weights = iptw, control = ctl)
  expect_lt(se_err(tt_vcov(po, "robust"), "V03_poisson_pw"), 1e-8)                       # 6.3e-9
  expect_lt(se_err(tt_vcov(po, "cluster", ~education), "V15_poisson_pw_cl_small"), 2e-8)  # 1.6e-8
  ga <- glm(index_age ~ treated + female + cv_event, gaussian, d, weights = iptw)
  expect_lt(se_err(tt_vcov(ga, "robust"), "V08_glm_gauss_pw"), 1e-12)                     # 1.3e-14
  V <- tt_vcov(ga, "cluster", "education")
  expect_lt(se_err(V, "V16_glm_gauss_pw_cl_small"), 1e-12)                                # 5.4e-14
  # ML cluster factor G/(G - 1) = 3/2, with no (N - 1)/(N - k).
  expect_identical(vce_probe("V16_glm_gauss_pw_cl_small")$N_clust[1], 3L)
  expect_identical(tabtools:::tt_wald_df(ga, "cluster", ~education), Inf)
  lo <- glm(cv_event ~ treated + index_age + female, binomial, d, control = ctl)
  expect_lt(se_err(tt_vcov(lo, "cluster", ~education), "V10_logit_cl_small"), 5e-8)      # 2.3e-8
})

test_that("lm [pweight]/vce(robust) is HC1 with t(N - k); vce(cluster) adds (N - 1)/(N - k), t(G - 1)", {
  d <- cohort_vce()
  l <- lm(cv_event ~ treated + index_age + female, d, weights = iptw)
  V <- tt_vcov(l, "robust")
  expect_lt(se_err(V, "V04_regress_pw"), 1e-12)            # 7.0e-14
  expect_lt(V_err(V, "V04_regress_pw"), 1e-11)
  expect_equal(tabtools:::tt_wald_df(l, "robust"), vce_probe("V04_regress_pw")$df_r[1])
  l2 <- lm(index_age ~ treated + female + cv_event, d)
  expect_lt(se_err(tt_vcov(l2, "robust"), "V05_regress_robust"), 1e-12)      # 1.6e-14
  expect_lt(se_err(tt_vcov(l2, "cluster", ~education), "V12_regress_cl_small"), 1e-12)  # 5.7e-14
  l3 <- lm(index_age ~ treated + female + cv_event, d, weights = iptw)
  Vc <- tt_vcov(l3, "cluster", d$education)
  expect_lt(se_err(Vc, "V11_regress_pw_cl_small"), 1e-12)  # 3.3e-14
  expect_lt(V_err(Vc, "V11_regress_pw_cl_small"), 1e-11)
  # t(G - 1) = t(2): e(df_r) = 2.
  expect_identical(tabtools:::tt_wald_df(l3, "cluster", d$education), 2)
  expect_identical(vce_probe("V11_regress_pw_cl_small")$df_r[1], 2L)
  # [aweight] (vce = "stata") keeps the model-based variance.
  expect_lt(se_err(tt_vcov(l, "stata"), "V17_regress_aw"), 1e-12)
  # regtab's interval is on t(2): Female sex -0.16 (-0.89, 0.57), as W06b.
  tt <- suppressMessages(regtab(l3, vce = "cluster", cluster = d$education))
  se <- sqrt(Vc["female", "female"])
  b <- coef(l3)[["female"]]
  expect_identical(tt$body[2, 3], sprintf("(%.2f, %.2f)", b - qt(0.975, 2) * se, b + qt(0.975, 2) * se))
})

test_that("zero probability weights are outside the sample: N and N/(N - 1) exclude them", {
  d <- cohort_vce()
  g <- glm(cv_event ~ treated + index_age + female, quasibinomial, d, weights = w0, control = ctl)
  expect_lt(se_err(tt_vcov(g, "robust"), "V13_logit_pw_zero"), 1e-8)   # 8.0e-9
  d$cv <- as.numeric(d$cv_event)  # lm() cannot refill a labelled response at zero weights
  l <- lm(cv ~ treated + index_age + female, d, weights = w0)
  expect_lt(se_err(tt_vcov(l, "robust"), "V14_regress_pw_zero"), 1e-12)  # 1.2e-13
  expect_equal(tabtools:::tt_wald_df(l, "robust"), vce_probe("V14_regress_pw_zero")$df_r[1])
  tt <- suppressWarnings(suppressMessages(regtab(g, vce = "robust", stats = "n")))
  expect_equal(tt$stored$n_1, 13500)
})

test_that("pooled person-period MSMs: cluster by id (G = 15,000)", {
  skip_on_cran()
  p <- golden_fixture("cohort_pp")
  p$period <- factor(p$period)
  g <- glm(ev ~ treated + index_age + female + period, quasibinomial, p, weights = iptw, control = ctl)
  V <- tt_vcov(g, "cluster", ~id)
  # Key terms only: Stata's logit leaves the sparse late-period dummies
  # ~1e-6 from the maximum (15.period).
  expect_lt(se_err(V, "V50_logit_pw_cl", key[1:3]), 1e-8)   # 7.8e-9
  l <- lm(ev ~ treated + index_age + female + period, p, weights = iptw)
  expect_lt(se_err(tt_vcov(l, "cluster", "id"), "V51_regress_pw_cl"), 2e-12)      # 1.9e-12
  expect_identical(tabtools:::tt_wald_df(l, "cluster", "id"), 14999)
  po <- glm(ev ~ treated + index_age + female + period + offset(log(pt)), poisson, p, weights = iptw, control = ctl)
  expect_lt(se_err(tt_vcov(po, "cluster", ~id), "V52_poisson_pw_cl", key[1:3]), 5e-11)  # 4.5e-11
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~id, weights = ~iptw, data = p)
  s <- survey::svyglm(ev ~ treated + index_age + female + period, des, family = quasibinomial(),
                      control = glm.control(epsilon = 1e-12, maxit = 100))
  expect_lt(se_err(tt_vcov(s), "V54_svy_logit_psu", key[1:3]), 1e-8)   # 7.8e-9
  expect_equal(tabtools:::tt_wald_df(s), 14999)
})

# ---------------------------------------------------------------------------
# Cox (task 5.12)

test_that("coxph: oim, vce(robust), vce(cluster) and stset [pw] match stcox", {
  skip_if_not_installed("survival")
  d <- cohort_vce()
  f <- survival::Surv(fu_y, cv_event) ~ treated + index_age + female
  c0 <- survival::coxph(f, d, ties = "breslow")
  expect_lt(se_err(tt_vcov(c0), "V20_stcox_oim"), 1e-12)                             # 1.7e-14
  expect_lt(se_err(tt_vcov(c0, "robust"), "V21_stcox_robust"), 1e-12)               # 9.6e-14
  expect_lt(V_err(tt_vcov(c0, "robust"), "V21_stcox_robust"), 2e-11)  # 1.1e-11 (a small covariance)
  expect_lt(se_err(tt_vcov(c0, "cluster", ~education), "V22_stcox_cl_small"), 1e-12)  # 1.2e-13
  cw <- survival::coxph(f, d, ties = "breslow", weights = iptw, id = id)
  expect_lt(se_err(tt_vcov(cw), "V23_stcox_pw"), 1e-12)                              # 9.5e-14
  expect_lt(se_err(tt_vcov(cw, "robust"), "V23_stcox_pw"), 1e-12)
  expect_lt(se_err(tt_vcov(cw, "cluster"), "V23_stcox_pw"), 1e-12)  # the fit's own id
  expect_error(tt_vcov(c0, "cluster"), "needs `cluster`")
  # Stata's e(N_sub) under stset [pw] is the weighted subject count.
  s <- tabtools:::tt_model_stats.coxph(cw, tabtools:::tt_model_info(cw))
  expect_equal(s$N_sub, sum(d$iptw), tolerance = 1e-12)
  expect_identical(s$N, 15000L)
  # A weighted Cox fit shows stcox's pseudo-log-likelihood (5w review
  # P0-4): the Breslow partial likelihood with the weights normalised to
  # mean 1 (stcox [pw] e(ll) -44108.11 on the cohort, "Milestone 5w
  # findings"; exact to Stata in fixtures D05, N01).
  tt <- suppressMessages(regtab(cw, stats = c("n", "ll")))
  expect_true(any(tt$body[[1]] == "Log-likelihood"))
  expect_equal(tt$stored$ll_1, -44108.11, tolerance = 0.006 / 44108.11)
  expect_identical(tt_vce_types(c0), c("stata", "model", "robust", "cluster"))
})

test_that("survreg refuses robust/cluster by name, with a hint", {
  skip_if_not_installed("survival")
  d <- cohort_vce()
  s <- survival::survreg(survival::Surv(fu_y, cv_event) ~ treated, d, dist = "weibull")
  expect_error(tt_vcov(s, "robust"), "not available for <survreg>")
  expect_error(regtab(s, vce = "robust"), "robust = TRUE")
})

# ---------------------------------------------------------------------------
# survey::svyglm (task 5.10)

test_that("svyglm: design variance, t(design df), unweighted N, no likelihood", {
  skip_if_not_installed("survey")
  d <- cohort_vce()
  des <- survey::svydesign(ids = ~1, weights = ~iptw, data = d)
  s1 <- survey::svyglm(cv_event ~ treated + index_age + female, des, family = quasibinomial(),
                       control = glm.control(epsilon = 1e-12, maxit = 100))
  expect_lt(se_err(tt_vcov(s1), "V30_svy_logit"), 5e-8)            # 4.1e-8 (Stata logit convergence)
  expect_equal(tabtools:::tt_wald_df(s1), 14999)
  expect_equal(tabtools:::tt_wald_df(s1), vce_probe("V30_svy_logit")$df_r[1])
  expect_identical(tabtools:::tt_wald_df(s1, "model"), s1$df.residual)
  s2 <- survey::svyglm(index_age ~ treated + female + cv_event, des)
  expect_lt(se_err(tt_vcov(s2), "V31_svy_regress"), 1e-12)         # 2.9e-15
  s3 <- survey::svyglm(cv_event ~ treated + index_age + female + offset(log(fu_y)), des,
                       family = quasipoisson(), control = glm.control(epsilon = 1e-12, maxit = 100))
  expect_lt(se_err(tt_vcov(s3), "V32_svy_poisson"), 1e-8)          # 6.2e-9
  des3 <- survey::svydesign(ids = ~education, weights = ~iptw, data = d)
  s4 <- survey::svyglm(cv_event ~ treated + index_age + female, des3, family = quasibinomial(),
                       control = glm.control(epsilon = 1e-12, maxit = 100))
  expect_lt(se_err(tt_vcov(s4), "V33_svy_logit_psu_small"), 5e-7)  # 4.2e-7 (3 PSUs amplify it)
  expect_equal(tabtools:::tt_wald_df(s4), 2)
  expect_error(tt_vcov(s1, "robust"), "not available for <svyglm>")
  info <- tabtools:::tt_model_info(s1)
  expect_identical(info$effect_scale, "OR")
  expect_identical(tabtools:::tt_model_info(s3)$effect_scale, "IRR")
  # No likelihood is stored: the unavailable rows stay absent, with the
  # selected upstream omission note naming exactly aic and ll.
  withr::local_options(cli.width = 120L)
  expect_message(tt <- regtab(s1, stats = c("n", "ll", "aic")),
    "^\\(regtab: no model reports 2 requested statistic\\(s\\), left out of the table: aic ll\\)$",
    class = "rlang_message")
  expect_equal(tt$stored$n_1, 15000)
  expect_null(tt$stored$ll_1)
  expect_null(tt$stored$aic_1)
})

# ---------------------------------------------------------------------------
# WeightIt::glm_weightit (task 5.11)

test_that("glm_weightit: M-estimation variance; identity y ~ treat reproduces teffects ipw", {
  skip_if_not_installed("WeightIt")
  d <- cohort_vce()
  d$education <- factor(d$education)
  d$treated <- as.numeric(d$treated)  # WeightIt cannot compare a labelled treatment
  W <- WeightIt::weightit(treated ~ index_age + female + education + diabetes + hypertension + anxiety,
                          data = d, method = "glm", estimand = "ATE")
  f <- WeightIt::glm_weightit(cv_event ~ treated, data = d, weightit = W, family = gaussian)
  p <- vce_probe("V60_teffects_ipw_ate")
  ate <- p[p$term == "r1vs0.treated", ]
  po0 <- p[p$term == "0.treated", ]
  expect_equal(unname(coef(f)["treated"]), ate$b, tolerance = 1e-12)             # 1.6e-14
  expect_equal(sqrt(tt_vcov(f)["treated", "treated"]), ate$se, tolerance = 1e-12)  # 3.0e-14
  expect_equal(unname(coef(f)["(Intercept)"]), po0$b, tolerance = 1e-12)
  expect_identical(tt_vcov(f, "model"), tt_vcov(f))
  expect_identical(tabtools:::tt_wald_df(f), Inf)
  expect_error(tt_vcov(f, "robust"), "not available for <glm_weightit>")
  tt <- suppressMessages(regtab(f, digits = 4, keepintercept = TRUE, vce_note = TRUE, stats = c("n", "ll")))
  z <- qnorm(0.975)
  expect_identical(unlist(tt$body[1, 2:3], use.names = FALSE),
                   c(sprintf("%.4f", ate$b), sprintf("(%.4f, %.4f)", ate$b - z * ate$se, ate$b + z * ate$se)))
  expect_match(tt$footnote, "M-estimation")
  expect_null(tt$stored$ll_1)
})

# ---------------------------------------------------------------------------
# geeglm (task 5.13)

test_that("geeglm: xtgee vce(robust) and the glm [pw] vce(cluster) reading", {
  skip_on_cran()
  skip_if_not_installed("geepack")
  p <- golden_fixture("cohort_pp")
  p$period <- factor(p$period)
  fm <- ev ~ treated + index_age + female + period
  gc <- geepack::geese.control(epsilon = 1e-12, maxit = 100)
  g0 <- geepack::geeglm(fm, binomial, p, id = id, corstr = "independence", control = gc)
  expect_lt(se_err(tt_vcov(g0, "robust"), "V55_xtgee_ind_robust"), 1e-10)   # 2.2e-12
  g1 <- geepack::geeglm(fm, binomial, p, id = id, corstr = "exchangeable", control = gc)
  expect_lt(se_err(tt_vcov(g1, "robust"), "V56_xtgee_exch_robust", key), 5e-7)  # 1.4e-7 (xtgee convergence)
  gw <- suppressWarnings(geepack::geeglm(fm, binomial, p, id = id, weights = iptw, corstr = "independence", control = gc))
  Vg <- tt_vcov(gw, gee_as = "glm")
  expect_lt(se_err(Vg, "V53_glm_pw_cl", key[1:3]), 2e-7)   # 1.6e-7 (Stata glm's convergence)
  expect_lt(se_err(Vg, "V50_logit_pw_cl", key[1:3]), 1e-8) # 7.8e-9 against logit
  expect_identical(tt_vcov(gw, "cluster"), Vg)
  expect_error(tt_vcov(gw, "cluster", cluster = ~id), "own `id`")
  # The glm reading classifies as logit (OR, intercept dropped), no QICu;
  # since tabtools 2.1.12 the xtgee reading is OR too (glm's family/link
  # rule), but remains a GEE.
  tg <- tabtools:::.rt_gee_tag(gw, "glm")
  expect_identical(tabtools:::tt_model_info(tg)$effect_scale, "OR")
  expect_false(tabtools:::tt_model_info(tg)$is_gee)
  expect_identical(tabtools:::tt_model_info(gw)$effect_scale, "OR")
  expect_true(tabtools:::tt_model_info(gw)$is_gee)
  expect_error(regtab(g1, gee_as = "glm"), "independence working correlation")
  tt <- suppressMessages(regtab(gw, gee_as = "glm", drop = "period", stats = c("n", "qic", "groups")))
  expect_equal(tt$stored$n_1, nrow(p))
  expect_null(tt$stored$qic_1)
  expect_null(tt$stored$groups_1)
})

# ---------------------------------------------------------------------------
# marginaleffects bridge (Phase 7d): margins after logit [pw]

test_that("marginaleffects with tt_vcov() and wts reproduces Stata margins after logit [pw]", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")
  m <- utils::read.csv(file.path(vce_dir(), "margins.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  d <- cohort_vce()
  d$treatedf <- factor(d$treated)
  fit <- suppressWarnings(glm(cv_event ~ treatedf + index_age + female, binomial, d, weights = iptw, control = ctl))
  V <- tt_vcov(fit, "robust")
  expect_lt(se_err(V, "V40_logit_pw_factor", c("1.treated", "index_age", "female")), 1e-8)
  # marginaleffects' delta method differentiates numerically: SEs agree to
  # ~3e-7, estimates to ~2e-7 (Stata's logit convergence).
  rel <- function(a, b) abs(a / b - 1)
  ac <- marginaleffects::avg_comparisons(fit, variables = "treatedf", vcov = V, wts = d$iptw)
  expect_lt(rel(ac$estimate, m$b[m$probe == "M01_dydx"]), 1e-6)
  expect_lt(rel(ac$std.error, m$se[m$probe == "M01_dydx"]), 1e-6)
  ap <- marginaleffects::avg_predictions(fit, variables = "treatedf", vcov = V, wts = d$iptw)
  expect_lt(max(rel(ap$estimate, m$b[m$probe == "M02_margins"])), 1e-6)
  expect_lt(max(rel(ap$std.error, m$se[m$probe == "M02_margins"])), 1e-6)
  as <- marginaleffects::avg_slopes(fit, variables = "index_age", vcov = V, wts = d$iptw)
  expect_lt(rel(as$estimate, m$b[m$probe == "M03_dydx_age"]), 1e-6)
  expect_lt(rel(as$std.error, m$se[m$probe == "M03_dydx_age"]), 1e-6)
  # Without wts the averages move (margins averages with the weights).
  au <- marginaleffects::avg_comparisons(fit, variables = "treatedf", vcov = V)
  expect_gt(rel(au$estimate, m$b[m$probe == "M01_dydx"]), 5 * rel(ac$estimate, m$b[m$probe == "M01_dydx"]))
  f2 <- glm(cv_event ~ treatedf + index_age + female, binomial, d, control = ctl)
  a2 <- marginaleffects::avg_comparisons(f2, variables = "treatedf", vcov = tt_vcov(f2, "cluster", ~education))
  expect_lt(rel(a2$estimate, m$b[m$probe == "M04_dydx_cluster"]), 1e-6)
  expect_lt(rel(a2$std.error, m$se[m$probe == "M04_dydx_cluster"]), 1e-6)
})

# ---------------------------------------------------------------------------
# Argument handling

test_that("per-model vce and cluster lists expand, and bad ones are refused", {
  ex <- tabtools:::.rt_expand_vce("robust", NULL, 3)
  expect_identical(ex$vce, rep(list("robust"), 3))
  ex <- tabtools:::.rt_expand_vce(c("stata", "robust"), NULL, 2)
  expect_identical(ex$vce, list("stata", "robust"))
  ex <- tabtools:::.rt_expand_vce(list("stata", "cluster"), ~id, 2)
  expect_identical(ex$cluster, list(NULL, ~id))  # a shared cluster reaches cluster models only
  ex <- tabtools:::.rt_expand_vce(NULL, list(NULL, ~id), 2, vce_missing = TRUE)
  expect_identical(ex$vce, list("stata", "cluster"))  # cluster alone implies vce = "cluster"
  ex <- tabtools:::.rt_expand_vce(NULL, ~id, 2, vce_missing = TRUE)
  expect_identical(ex$vce, list("cluster", "cluster"))
  expect_error(tabtools:::.rt_expand_vce(list("stata"), NULL, 2), "one per model")
  expect_error(tabtools:::.rt_expand_vce(c("stata", "robust", "model"), NULL, 2), "one value per model")
  expect_error(tabtools:::.rt_expand_vce("hc3", NULL, 1), "must be one of")
  expect_error(tabtools:::.rt_expand_vce(list("stata", 1), NULL, 2), "model 2 must be one of")
  expect_error(tabtools:::.rt_expand_vce(NULL, list(~id), 2, TRUE), "one per model")
  expect_error(tabtools:::.rt_expand_vce(list("stata", "robust"), list(~id, NULL), 2), "model 1")
})

test_that("regtab() applies per-model vce, cluster forms, and errors name the model", {
  d <- cohort_vce()
  crude <- glm(cv_event ~ treated, binomial, d, control = ctl)
  ipw <- glm(cv_event ~ treated, quasibinomial, d, weights = iptw, control = ctl)
  tt <- regtab(crude, ipw, vce = list("stata", "robust"))
  se_of <- function(tt, m) {
    r <- tt$meta$regtab_rows
    r <- r[r$model == m & r$key == "treated", ]
    (log(r$conf.high) - log(r$conf.low)) / (2 * qnorm(0.975))
  }
  expect_equal(se_of(tt, 1), sqrt(tt_vcov(crude)["treated", "treated"]), tolerance = 1e-10)
  expect_equal(se_of(tt, 2), sqrt(tt_vcov(ipw, "robust")["treated", "treated"]), tolerance = 1e-10)
  # cluster as a formula, a column name, and a vector give the same table.
  a <- regtab(ipw, vce = "cluster", cluster = ~education)
  b <- regtab(ipw, vce = "cluster", cluster = "education")
  v <- regtab(ipw, vce = "cluster", cluster = d$education)
  i <- regtab(ipw, cluster = ~education)
  expect_identical(a$body, b$body)
  expect_identical(a$body, v$body)
  expect_identical(a$body, i$body)
  expect_false(identical(a$body, regtab(ipw, vce = "robust")$body))
  # Errors.
  expect_error(regtab(ipw, vce = "cluster"), "Model 1")
  expect_error(regtab(ipw, vce = "cluster"), "needs `cluster`")
  expect_error(regtab(ipw, vce = "cluster", cluster = d$education[-1]), "15000 observations")
  expect_error(regtab(ipw, vce = "cluster", cluster = ~education + female), "one variable")
  expect_error(regtab(ipw, vce = "cluster", cluster = ~nope), "not in the model's data")
  dd <- d
  dd$cl <- d$education
  dd$cl[5] <- NA
  ipw2 <- glm(cv_event ~ treated, quasibinomial, dd, weights = iptw, control = ctl)
  expect_error(regtab(ipw2, vce = "cluster", cluster = ~cl), "missing values")
  expect_error(regtab(ipw, vce = "cluster", cluster = rep(1, nrow(d))), "single cluster")
  expect_error(regtab(crude, ipw, vce = list("stata", "robust"), cluster = list(~education, NULL)),
               "clusters apply only")
  expect_error(regtab(ipw, vce = "sandwich"), "must be one of")
  expect_error(regtab(ipw, vce_note = NA), "TRUE or FALSE")
  # A class without robust support is refused by name.
  skip_if_not_installed("MASS")
  po <- MASS::polr(factor(education) ~ treated, d, Hess = TRUE)
  expect_error(regtab(po, vce = "robust"), "not available for <polr>")
  expect_error(regtab(crude, po, vce = "robust"), "Model 2")
})

test_that("cluster vectors follow na.action: full-data length is subset to the fit", {
  d <- cohort_vce()
  d$x <- d$index_age
  d$x[c(3, 10)] <- NA
  g <- glm(cv_event ~ treated + x, quasibinomial, d, weights = iptw, control = ctl)
  V1 <- tt_vcov(g, "cluster", d$education)
  V2 <- tt_vcov(g, "cluster", d$education[-c(3, 10)])
  V3 <- tt_vcov(g, "cluster", ~education)
  expect_identical(V1, V2)
  expect_identical(V1, V3)
})

test_that("vce = 'stata' warns when glm/lm weights look like IPTW, and never switches", {
  d <- cohort_vce()
  ipw <- glm(cv_event ~ treated, quasibinomial, d, weights = iptw, control = ctl)
  expect_warning(tt <- regtab(ipw), "look like inverse-probability weights")
  expect_warning(regtab(ipw), "vce = \"robust\"")
  r <- tt$meta$regtab_rows
  se <- (log(r$conf.high[1]) - log(r$conf.low[1])) / (2 * qnorm(0.975))
  expect_equal(se, sqrt(tt_vcov(ipw, "stata")["treated", "treated"]), tolerance = 1e-10)
  expect_no_warning(regtab(ipw, vce = "robust"))
  expect_no_warning(regtab(ipw, vce = "model"))
  # Integer or constant weights are not flagged.
  d$fw2 <- d$fw + 1
  expect_no_warning(regtab(glm(cv_event ~ treated, binomial, d, weights = fw2)))
  d$c2 <- 2.5
  expect_no_warning(suppressMessages(regtab(suppressWarnings(glm(cv_event ~ treated, binomial, d, weights = c2)))))
  expect_warning(regtab(lm(index_age ~ treated, d, weights = iptw)), "inverse-probability")
})

test_that("[pweight] fits show Stata's pseudo-log-likelihood statistics; lm keeps R2", {
  # 5w review P0-4 (fixtures D01-D21, E09, N05, N06 compare them to Stata).
  d <- cohort_vce()
  ipw <- glm(cv_event ~ treated, quasibinomial, d, weights = iptw, control = ctl)
  expect_no_message(tt <- regtab(ipw, vce = "robust", stats = c("n", "ll", "aic", "bic", "r2")))
  expect_identical(tt$body[[1]][nrow(tt$body)], "Pseudo R\u00b2")
  mu <- fitted(ipw)
  y <- d$cv_event
  expect_equal(tt$stored$ll_1, sum(d$iptw * (y * log(mu) + (1 - y) * log(1 - mu))), tolerance = 1e-12)
  lw <- lm(index_age ~ treated, d, weights = iptw)
  tl <- suppressMessages(regtab(lw, vce = "robust", stats = c("n", "r2")))
  expect_identical(tl$body[[1]][nrow(tl$body)], "R²")
  expect_no_message(regtab(lw, vce = "robust", stats = c("n", "r2")))
  # Unweighted robust fits keep their (true) likelihood.
  g <- glm(cv_event ~ treated, binomial, d)
  tu <- regtab(g, vce = "robust", stats = "ll")
  expect_equal(tu$stored$ll_1, as.numeric(logLik(g)), tolerance = 1e-12)
})

test_that("vce_note describes each model's non-default variance in the footnote", {
  d <- cohort_vce()
  crude <- glm(cv_event ~ treated, binomial, d)
  ipw <- glm(cv_event ~ treated, quasibinomial, d, weights = iptw, control = ctl)
  tt <- regtab(crude, ipw, ipw, vce = list("stata", "robust", "cluster"), cluster = list(NULL, NULL, ~education),
               models = c("Crude", "IPTW", "IPTW (cl)"), vce_note = TRUE, footnote = "Weighted by IPTW")
  expect_identical(tt$footnote,
                   "Weighted by IPTW \\ Standard errors, IPTW: robust; IPTW (cl): robust, clustered by education.")
  expect_identical(regtab(crude, vce_note = TRUE)$footnote, "")
  expect_identical(regtab(ipw, vce = "robust", vce_note = TRUE)$footnote, "Standard errors: robust.")
})

test_that("vcenote was renamed to vce_note without an alias (P3-7)", {
  d <- cohort_vce()
  ipw <- glm(cv_event ~ treated, quasibinomial, d, weights = iptw, control = ctl)
  expect_true("vce_note" %in% names(formals(regtab)))
  expect_false("vcenote" %in% names(formals(regtab)))
  # The old name is refused, not swallowed by `...` as a silent no-op.
  expect_error(regtab(ipw, vce = "robust", vcenote = TRUE), "has no argument .*vcenote")
})

test_that("tt_vcov() contract: named square matrix, NA rows for aliased coefficients", {
  d <- cohort_vce()
  d$dup <- d$index_age * 2
  g <- glm(cv_event ~ treated + index_age + dup, quasibinomial, d, weights = iptw, control = ctl)
  V <- tt_vcov(g, "robust")
  expect_identical(dimnames(V), list(names(coef(g)), names(coef(g))))
  expect_true(all(is.na(V["dup", ])))
  expect_false(anyNA(V[c("(Intercept)", "treated", "index_age"), c("(Intercept)", "treated", "index_age")]))
  expect_true(isSymmetric(V))
  expect_error(tt_vcov(g, "robust", cluster = ~education), "only with")
  expect_identical(tt_vce_types(lm(mpg ~ wt, mtcars)), c("stata", "model", "robust", "cluster"))
  # glm/lm subclasses do not inherit robust support.
  f <- glm(am ~ wt, binomial, mtcars)
  class(f) <- c("gam", class(f))
  expect_identical(tt_vce_types(f), c("stata", "model"))
})
