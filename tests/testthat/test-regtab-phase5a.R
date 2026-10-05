# regtab Phase 5a adapters (IMPLEMENTATION_PLAN.md, "Phase 5a findings"):
# ordinal (polr, clm), multinomial (multinom), zero-inflated and hurdle
# counts (zeroinfl, hurdle), parametric survival (survreg), Fine-Gray
# (coxph on finegray() data, cmprsk::crr), and the data-frame escape hatch.
# Stata expectations: tests/testthat/fixtures/regtab_phase5a/
# (qa/stata/make_regtab_phase5a.do) and the goldens R16-R20, R25p/s/t.

p5_fixture <- function(name) test_path("fixtures", "regtab_phase5a", name)
p5_oim <- function(model) {
  x <- utils::read.csv(p5_fixture("oim_se.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  x <- x[x$model == model, ]
  data.frame(b = x$b, se = x$se, row.names = x$term)
}
p5_stats <- function(model) {
  x <- utils::read.csv(p5_fixture("stats.csv"), strip.white = TRUE, na.strings = ".",
                       stringsAsFactors = FALSE)
  as.list(x[x$model == model, ])
}
p5_cells <- function(name) golden_read_cells_file(p5_fixture(paste0(name, ".csv")))
expect_layout <- function(tt, name, mode = "tolerance", cols = NULL) {
  want <- p5_cells(name)
  got <- golden_as_cells(tt)
  if (!is.null(cols)) {
    want <- want[, cols, drop = FALSE]
    got <- got[, cols, drop = FALSE]
  }
  mm <- golden_compare_cells(got, want, mode = mode)
  if (nrow(mm)) golden_fail(mm, paste("layout", name)) else succeed()
}
# Stata 2.1.10/2.1.11 exponentiated the lognormal/loglogistic ancillary rows
# (lnsigma, sigma, lngamma, gamma) with the time ratios (take_action item
# 14), which R never copied; 2.1.12 keeps them on their own scale, so these
# layouts now compare in full.
p5_rows <- function(fit, ...) {
  info <- tabtools:::tt_model_info(fit)
  tabtools:::tt_regtab_rows(fit, info, ...)
}

# ---------------------------------------------------------------------------
# Standard errors at Stata's own estimates (vce(oim) / vce(robust))

test_that("polr: analytic observed information equals Stata's ologit/oprobit at Stata's estimates", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  d <- golden_fixture("cohort", factors = "education")
  for (m in c("ologit", "oprobit")) {
    st <- p5_oim(m)
    f <- MASS::polr(education ~ index_age + female + diabetes + hypertension, data = d, Hess = TRUE,
                    method = if (m == "ologit") "logistic" else "probit")
    V <- tabtools:::.rt_polr_vcov(f, par = st$b)
    expect_equal(unname(sqrt(diag(V))), st$se, tolerance = 1e-9, label = m)
    # polr's own vcov() comes from optim()'s numerical Hessian.
    expect_identical(tabtools:::tt_vcov(f, "model"), vcov(f))
  }
})

test_that("clm reproduces Stata's ologit estimates and SEs; polr matches clm", {
  skip_on_cran()
  skip_if_not_installed("ordinal")
  skip_if_not_installed("MASS")
  d <- golden_fixture("cohort", factors = "education")
  st <- p5_oim("ologit")
  f <- ordinal::clm(education ~ index_age + female + diabetes + hypertension, data = d)
  ord <- c(3:6, 1:2)
  expect_equal(unname(coef(f)[ord]), st$b, tolerance = 1e-9)
  expect_equal(unname(sqrt(diag(tabtools:::tt_vcov(f)))[ord]), st$se, tolerance = 1e-9)
  p <- MASS::polr(education ~ index_age + female + diabetes + hypertension, data = d, Hess = TRUE)
  expect_equal(unname(sqrt(diag(tabtools:::tt_vcov(p)))), st$se, tolerance = 1e-4)
})

test_that("multinom: analytic information equals Stata's mlogit at Stata's estimates", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  d <- golden_fixture("cohort", factors = "education")
  st <- p5_oim("mlogit")
  st <- st[!grepl("^Primary", rownames(st)), ]
  f <- nnet::multinom(education ~ index_age + female + diabetes + hypertension, data = d, trace = FALSE)
  # R's design order is (Intercept) first, Stata's _cons last.
  ord <- c(5, 1:4, 10, 6:9)
  V <- tabtools:::.rt_mn_vcov(f, par = st$b[ord])
  expect_equal(unname(sqrt(diag(V))), st$se[ord], tolerance = 1e-9)
  expect_identical(rownames(V)[1:2], c("Secondary:(Intercept)", "Secondary:index_age"))
})

test_that("zeroinfl: observed information from the analytic score equals Stata's zip/zinb", {
  skip_if_not_installed("pscl")
  z <- golden_fixture("zip")
  for (dist in c("poisson", "negbin")) {
    st <- p5_oim(if (dist == "poisson") "zip" else "zinb")
    f <- pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female, data = z, dist = dist)
    ord <- c(4, 1:3, 7, 5:6)
    par <- st$b[ord]
    se <- st$se[ord]
    if (dist == "negbin") {
      par <- c(par, -st$b[8])  # log(theta) = -lnalpha
      se <- c(se, st$se[8])
    }
    V <- tabtools:::.rt_twopart_vcov_full(f, par = par)
    expect_equal(unname(sqrt(diag(V))), se, tolerance = 1e-8, label = dist)
    # The score vanishes at pscl's estimates (to optim's tolerance).
    d <- tabtools:::.rt_twopart_design(f)
    sc <- tabtools:::.rt_zi_score(tabtools:::.rt_twopart_par(f), d, dist, make.link("logit"))
    expect_lt(max(abs(sc)), 0.05)
    expect_identical(tabtools:::tt_vcov(f, "model"), vcov(f))
  }
})

test_that("hurdle: binomial zero block equals the logit GLM's information; count block near pscl's", {
  skip_if_not_installed("pscl")
  z <- golden_fixture("zip")
  h <- pscl::hurdle(event_count ~ treatment + age_z + female | zero_risk + female, data = z,
                    dist = "negbin", zero.dist = "binomial")
  V <- tabtools:::tt_vcov(h)
  zn <- grep("^zero_", rownames(V), value = TRUE)
  g <- glm(I(event_count > 0) ~ zero_risk + female, binomial, data = z)
  X <- model.matrix(g)
  p <- plogis(drop(X %*% coef(h)[zn]))
  expect_equal(unname(V[zn, zn]), unname(solve(crossprod(X, X * p * (1 - p)))), tolerance = 1e-8)
  expect_equal(unname(sqrt(diag(V))), unname(sqrt(diag(vcov(h)))), tolerance = 1e-3)
  # Poisson and geometric count parts, count-distribution zero hurdle.
  for (zd in c("poisson", "negbin", "geometric")) {
    h2 <- pscl::hurdle(event_count ~ treatment + age_z | zero_risk, data = z, dist = "poisson", zero.dist = zd)
    expect_equal(unname(sqrt(diag(tabtools:::tt_vcov(h2)))), unname(sqrt(diag(vcov(h2)))),
                 tolerance = 2e-3, label = zd)
  }
})

test_that("survreg: vcov() is Stata's streg OIM; Log(scale) maps onto ln_p/lnsigma/lngamma", {
  skip_on_cran()
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort")
  for (dd in c("weibull", "lognormal", "loglogistic", "exponential")) {
    st <- p5_oim(dd)
    f <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age + female,
                           data = d, dist = dd)
    b <- coef(f)[c(2:4, 1)]
    se <- sqrt(diag(vcov(f)))[c(2:4, 1)]
    if (dd != "exponential") {
      sgn <- if (dd == "weibull") -1 else 1
      b <- c(b, sgn * log(f$scale))
      se <- c(se, sqrt(vcov(f)["Log(scale)", "Log(scale)"]))
    }
    expect_equal(unname(b), st$b, tolerance = 1e-6, label = dd)
    expect_equal(unname(se), st$se, tolerance = 1e-6, label = dd)
  }
})

test_that("Fine-Gray: crr's variance times N/(N-1) is stcrreg's vce(robust)", {
  skip_if_not_installed("cmprsk")
  d <- golden_fixture("cohort3500")
  st <- p5_oim("stcrreg")
  cr <- cmprsk::crr(d$follow_up, d$event_type, cov1 = as.matrix(d[, c("treated", "index_age", "female")]),
                    failcode = 1, cencode = 0)
  # stcrreg stops at its default convergence (~5e-7 from crr's maximum).
  expect_equal(unname(cr$coef), st$b, tolerance = 1e-6)
  expect_equal(unname(sqrt(diag(tabtools:::tt_vcov(cr)))), st$se, tolerance = 1e-6)
  expect_equal(tabtools:::tt_vcov(cr, "model"), cr$var, ignore_attr = TRUE)
})

test_that("Fine-Gray via coxph: subject-clustered sandwich times N/(N-1) (golden R20's p = 0.64)", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")
  d$event_type <- factor(d$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = d, etype = "cv")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + index_age + female,
                       weights = fgwt, data = fg)
  V <- tabtools:::tt_vcov(f)
  # The same fit clustered on the subject id finegray() carried along.
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + index_age + female,
                        weights = fgwt, data = fg, cluster = id)
  expect_equal(V, vcov(fc) * 3500 / 3499, tolerance = 1e-10, ignore_attr = TRUE)
  st <- p5_oim("stcrreg")
  expect_equal(unname(sqrt(diag(V))), st$se, tolerance = 2e-4)
  # coxph's own robust variance treats each expanded record as independent.
  expect_identical(tabtools:::tt_vcov(f, "model"), vcov(f))
  w <- tabtools:::tt_wald(f)
  expect_identical(format_p(w$p.value[2], 3, 2), "0.64")
  expect_identical(format_p(2 * pnorm(-abs(coef(f)[2] / sqrt(vcov(f)[2, 2]))), 3, 2), "0.62")
  # Subjects, and the label finegray() dropped.
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(c(s$N, s$N_sub), c(3500, 3500))
  tt <- regtab(f)
  expect_identical(tt$body[[1]][2], "Age at cohort entry (years)")
})

test_that("Fine-Gray labels are restored only from data that match finegray()'s input", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")
  d$event_type <- factor(d$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = d, etype = "cv")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ index_age, weights = fgwt, data = fg)
  rm(d)
  other <- golden_fixture("cohort3500")
  other$index_age <- other$index_age + 1
  expect_identical(regtab(f)$body[[1]], "index_age")
  other$index_age <- other$index_age - 1
  expect_identical(regtab(f)$body[[1]], "Age at cohort entry (years)")
})

# ---------------------------------------------------------------------------
# Stats

test_that("stats match Stata's e(N), e(ll), e(rank) and e(r2_p)", {
  skip_on_cran()
  for (p in c("MASS", "nnet", "pscl", "survival", "cmprsk", "ordinal")) skip_if_not_installed(p)
  d <- golden_fixture("cohort", factors = "education")
  z <- golden_fixture("zip")
  c35 <- golden_fixture("cohort3500")
  fits <- list(
    ologit = MASS::polr(education ~ index_age + female + diabetes + hypertension, data = d, Hess = TRUE),
    ologit_clm = ordinal::clm(education ~ index_age + female + diabetes + hypertension, data = d),
    mlogit = nnet::multinom(education ~ index_age + female + diabetes + hypertension, data = d,
                            trace = FALSE, reltol = 1e-14, maxit = 1000),
    zip = pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female, data = z),
    zinb = pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female, data = z,
                          dist = "negbin"),
    stcrreg = cmprsk::crr(c35$follow_up, c35$event_type,
                          cov1 = as.matrix(c35[, c("treated", "index_age", "female")]),
                          failcode = 1, cencode = 0)
  )
  for (dd in c("weibull", "lognormal", "loglogistic", "exponential")) {
    fits[[dd]] <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age + female,
                                    data = d, dist = dd)
  }
  for (nm in names(fits)) {
    f <- fits[[nm]]
    want <- p5_stats(sub("_clm$", "", nm))
    s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
    expect_equal(s$N, want$N, label = paste(nm, "N"))
    expect_equal(s$ll, want$ll, tolerance = 1e-8, label = paste(nm, "ll"))
    expect_equal(s$rank, want$rank, label = paste(nm, "rank"))
    # Relative agreement: expect_equal()'s tolerance is absolute for
    # numbers below it, and e(r2_p) is ~2e-5 here, so a pseudo R-squared
    # off by 300% (mutation M56, Phase 5 review) passed at 1e-4. Observed:
    # 4e-11 relative.
    if (!is.na(want$r2_p)) expect_lt(abs(s$r2_p / want$r2_p - 1), 1e-9, label = paste(nm, "r2_p"))
    if (!is.na(want$N_sub)) expect_equal(s$N_sub, want$N_sub, label = paste(nm, "N_sub"))
    expect_equal(s$aic, -2 * want$ll + 2 * want$rank, tolerance = 1e-8)
  }
})

# ---------------------------------------------------------------------------
# Layouts no golden covers, against Stata's regtab csv() sinks

test_that("keepintercept layouts: cutpoints, equation intercepts, ancillary rows, factor levels", {
  skip_on_cran()
  for (p in c("MASS", "nnet", "pscl", "survival")) skip_if_not_installed(p)
  d <- golden_fixture("cohort", factors = c("education", "smoking"))
  z <- golden_fixture("zip")
  expect_layout(regtab(MASS::polr(education ~ index_age + smoking + female, data = d, Hess = TRUE),
                       keepintercept = TRUE), "ologit_keep")
  expect_layout(regtab(nnet::multinom(education ~ index_age + smoking + female, data = d, trace = FALSE),
                       keepintercept = TRUE), "mlogit_keep")
  sw <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age + female, data = d,
                          dist = "weibull")
  expect_layout(regtab(sw, keepintercept = TRUE, stats = "n ll aic bic"), "weibull_keep")
  sl <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age + smoking + female,
                          data = d, dist = "loglogistic")
  # Stata 2.1.10+ labels a log-time-only distribution TR (2.1.9: AF unless
  # `time` was typed), as R does.
  expect_layout(regtab(sl, keepintercept = TRUE), "loglogistic_keep")
  expect_layout(regtab(pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female,
                                      data = z, dist = "negbin"), keepintercept = TRUE, stats = "n ll"),
                "zinb_keep")
  expect_layout(regtab(pscl::zeroinfl(event_count ~ treatment + age_z + female | 1, data = z),
                       keepintercept = TRUE), "zip_cons_keep")
})

test_that("the automatic nointercept keeps lnsigma/sigma and lngamma/gamma, as Stata does", {
  skip_on_cran()
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort")
  f <- survival::Surv(follow_up, cv_event) ~ treated + index_age + female
  fl <- survival::survreg(f, data = d, dist = "loglogistic")
  tt <- regtab(fl)
  expect_layout(tt, "loglogistic_time_default")
  # R keeps the ancillary rows on their own scale: ln(gamma) and gamma.
  expect_identical(tt$body[[2]][tt$body[[1]] %in% c("lngamma", "gamma")],
                   tabtools:::.rt_est_text(c(log(fl$scale), fl$scale), 2))
  expect_layout(regtab(survival::survreg(f, data = d, dist = "lognormal")), "lognormal_default")
})

test_that("cdisc holds for the new families (task 5.7)", {
  skip_on_cran()
  for (p in c("MASS", "nnet", "pscl", "survival", "ordinal", "cmprsk")) skip_if_not_installed(p)
  d <- golden_fixture("cohort", factors = "education")
  z <- golden_fixture("zip")
  f <- education ~ index_age + female + diabetes + hypertension
  expect_layout(regtab(MASS::polr(f, data = d, Hess = TRUE), cdisc = TRUE), "cdisc_ologit")
  expect_layout(regtab(ordinal::clm(f, data = d), cdisc = TRUE), "cdisc_ologit")
  expect_layout(regtab(nnet::multinom(f, data = d, trace = FALSE), cdisc = TRUE), "cdisc_mlogit")
  expect_layout(regtab(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age + female,
                                         data = d, dist = "weibull"), cdisc = TRUE), "cdisc_weibull")
  zf <- event_count ~ treatment + age_z + female | zero_risk + female
  expect_layout(regtab(pscl::zeroinfl(zf, data = z), pscl::zeroinfl(zf, data = z, dist = "negbin"), cdisc = TRUE),
                "cdisc_zip")
  c35 <- golden_fixture("cohort3500")
  e <- c35
  e$event_type <- factor(e$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = e, etype = "cv")
  expect_layout(regtab(survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + index_age + female,
                                       weights = fgwt, data = fg), cdisc = TRUE), "cdisc_stcrreg")
  cr <- cmprsk::crr(c35$follow_up, c35$event_type, cov1 = as.matrix(c35[, c("treated", "index_age", "female")]),
                    failcode = 1, cencode = 0)
  # crr keeps no labels: every column but the label column.
  expect_layout(regtab(cr, cdisc = TRUE), "cdisc_stcrreg", mode = "exact", cols = 2:4)
})

# ---------------------------------------------------------------------------
# Adapter units: keys, labels, equation prefixes, dropped rows, scale

test_that("ordinal rows: cut# keys on the raw scale, no p, dropped by nointercept, cutlabels", {
  skip_if_not_installed("MASS")
  d <- golden_fixture("cohort3500", factors = "education")
  f <- MASS::polr(education ~ index_age + female, data = d, Hess = TRUE)
  r <- p5_rows(f)
  expect_identical(r$key, c("index_age", "female", "cut1", "cut2"))
  expect_identical(r$ancillary, c(FALSE, FALSE, TRUE, TRUE))
  expect_true(all(is.na(r$p.value[3:4])))
  expect_equal(r$estimate[3:4], unname(f$zeta))
  expect_equal(r$estimate[1:2], unname(exp(coef(f))))
  expect_identical(regtab(f)$body[[1]], c("Age at cohort entry (years)", "Female sex"))
  tt <- regtab(f, keepintercept = TRUE, cutlabels = "Low to mid \\ Mid to high")
  expect_identical(tt$body[[1]][3:4], c("Low to mid", "Mid to high"))
  expect_identical(tt$body[[4]][3:4], c("", ""))
  expect_identical(regtab(f, keepintercept = TRUE, cutlabels = "only first")$body[[1]][3:4],
                   c("only first", "cut2"))
  # keep() matches the cutpoint key.
  expect_identical(regtab(f, keepintercept = TRUE, keep = "cut2")$body[[1]], "cut2")
  expect_error(regtab(f, cutlabels = 1), "cutlabels")
  # oprobit: Coef., cutpoints shown by default (no automatic nointercept).
  fp <- MASS::polr(education ~ index_age + female, data = d, Hess = TRUE, method = "probit")
  tp <- regtab(fp)
  expect_identical(tp$stored$coef_label, "Coef.")
  expect_identical(tp$body[[1]][3:4], c("cut1", "cut2"))
})

test_that("clm needs flexible thresholds, no nominal/scale effects, treatment contrasts", {
  skip_if_not_installed("ordinal")
  d <- golden_fixture("cohort3500", factors = c("education", "smoking"))
  expect_error(regtab(ordinal::clm(education ~ index_age, data = d, threshold = "equidistant")),
               "flexible thresholds")
  expect_error(regtab(ordinal::clm(education ~ index_age, nominal = ~female, data = d)), "nominal")
  sm <- d$smoking
  d$smoking <- factor(d$smoking, ordered = TRUE)
  expect_error(regtab(ordinal::clm(education ~ smoking, data = d)), "ordered factor")
  d$smoking <- sm
  tt <- regtab(ordinal::clm(education ~ smoking, data = d), keepintercept = TRUE)
  expect_identical(tt$body[[1]], c("Smoking status", "  Never", "  Former", "  Current", "cut1", "cut2"))
})

test_that("multinom rows: equation keys and prefixes, colname matching, exponentiated intercepts", {
  skip_if_not_installed("nnet")
  d <- golden_fixture("cohort3500", factors = c("education", "smoking"))
  f <- nnet::multinom(education ~ index_age + smoking, data = d, trace = FALSE)
  r <- p5_rows(f)
  # A factor header above indented levels in each equation (tabtools 2.1.12).
  expect_identical(r$key[1:6], c("Secondary::smoking", "Secondary::0.smoking", "Secondary::1.smoking",
                                 "Secondary::2.smoking", "Secondary::index_age", "Secondary::_cons"))
  expect_identical(unique(r$block), c("Secondary", "Tertiary"))
  expect_identical(r$label[c(1, 2, 5, 6)], c("Secondary: Smoking status", "Secondary:   Never",
                                            "Secondary: Age at cohort entry (years)", "Secondary: Intercept"))
  expect_identical(r$kind[1:2], c("cat_header", "level"))
  expect_identical(r$status[c(1, 2, 7, 8)], c("header", "base", "header", "base"))
  expect_equal(r$estimate[6], exp(coef(f)["Secondary", "(Intercept)"]), ignore_attr = TRUE)
  tt <- regtab(f)
  expect_false(any(grepl("Intercept", tt$body[[1]])))
  expect_identical(tt$body[[2]][1:2], c("", "Reference"))
  # keep() on a colname keeps it in every equation.
  expect_identical(regtab(f, keep = "index_age")$body[[1]],
                   c("Secondary: Age at cohort entry (years)", "Tertiary: Age at cohort entry (years)"))
  expect_identical(nrow(regtab(f, drop = "smoking")$body), 2L)
  # Binary outcome: one equation named by the second level.
  d$big <- factor(ifelse(d$education == "Tertiary", "High_edu", "Other"), c("Other", "High_edu"))
  tb <- regtab(nnet::multinom(big ~ index_age, data = d, trace = FALSE))
  expect_identical(tb$body[[1]], "High edu: Age at cohort entry (years)")
  expect_identical(tb$stored$coef_label, "RRR")
})

test_that("zeroinfl/hurdle rows: equation order, global colname order, ancillary rows", {
  skip_if_not_installed("pscl")
  z <- golden_fixture("zip")
  f <- pscl::zeroinfl(event_count ~ treatment + female | zero_risk + female, data = z, dist = "negbin")
  r <- p5_rows(f)
  # The count equation is keyed by the dependent variable, Stata's coleq
  # (Phase 5 review P0-3), so a Poisson/NB model of the outcome merges.
  expect_identical(r$key, c("event_count::treatment", "event_count::female", "event_count::_cons", "zero::female",
                            "zero::zero_risk", "zero::_cons", "/::lnalpha", "/::alpha"))
  expect_identical(r$label[c(1, 4, 7, 8)], c("Event count: Treatment", "Inflation equation: Female",
                                            "Ancillary: lnalpha", "Ancillary: alpha"))
  expect_equal(r$estimate[7], -log(f$theta))
  expect_equal(r$estimate[8], 1 / f$theta)
  expect_true(is.na(r$p.value[8]))
  expect_equal(r$conf.low[8], exp(r$conf.low[7]))
  tt <- regtab(f)
  expect_identical(tt$body[[1]], c("Event count: Treatment", "Event count: Female",
                                   "Inflation equation: Female", "Inflation equation: Structural-zero risk"))
  expect_identical(regtab(f, keep = "female")$body[[1]], c("Event count: Female", "Inflation equation: Female"))
  h <- pscl::hurdle(event_count ~ treatment + female | zero_risk, data = z)
  expect_identical(regtab(h)$body[[1]], c("Event count: Treatment", "Event count: Female",
                                          "Selection equation: Structural-zero risk"))
  # An unlabelled dependent variable shows its name, underscores as spaces.
  attr(z$event_count, "label") <- NULL
  f2 <- pscl::zeroinfl(event_count ~ treatment | 1, data = z)
  expect_identical(regtab(f2)$body[[1]], "event count: Treatment")
})

test_that("zeroinfl factor levels: a header, indented levels and a Reference row per equation", {
  skip_if_not_installed("pscl")
  z <- golden_fixture("zip")
  z$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(z)))
  f <- pscl::zeroinfl(event_count ~ grp + age_z | grp, data = z)
  tt <- regtab(f)
  # Tabtools 2.1.12's layout (2.1.11 showed the raw keys "Event count: 1.grp").
  expect_identical(tt$body[[1]], c("Event count: grp", "Event count:   a", "Event count:   b", "Event count:   c",
                                   "Event count: Age z-score", "Inflation equation: grp",
                                   "Inflation equation:   a", "Inflation equation:   b", "Inflation equation:   c"))
  expect_identical(tt$body[[2]][c(1, 2, 6, 7)], c("", "Reference", "", "Reference"))
  expect_message(regtab(f, keep = "2.grp"), "level positions")
})

test_that("survreg ancillary rows per distribution; streg log-likelihood; Subjects", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")
  mk <- function(dd) survival::survreg(survival::Surv(follow_up, cv_event) ~ treated, data = d, dist = dd)
  lab <- function(dd) regtab(mk(dd), keepintercept = TRUE)$body[[1]]
  expect_identical(lab("weibull"), c("Treatment group", "ln_p", "p", "1/p", "Intercept"))
  expect_identical(lab("lognormal"), c("Treatment group", "lnsigma", "sigma", "Intercept"))
  expect_identical(lab("loglogistic"), c("Treatment group", "lngamma", "gamma", "Intercept"))
  expect_identical(lab("exponential"), c("Treatment group", "Intercept"))
  expect_identical(regtab(mk("weibull"))$body[[1]], "Treatment group")
  f <- mk("weibull")
  r <- p5_rows(f)
  expect_equal(r$estimate[r$key == "ln_p"], -log(f$scale))
  expect_equal(r$estimate[r$key == "p"], 1 / f$scale)
  expect_equal(r$estimate[r$key == "1/p"], f$scale)
  expect_equal(r$conf.low[r$key == "1/p"], exp(-r$conf.high[r$key == "ln_p"]))
  expect_false(is.na(r$p.value[r$key == "ln_p"]))
  expect_true(all(is.na(r$p.value[r$key %in% c("p", "1/p")])))
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  expect_equal(s$ll, as.numeric(logLik(f)) + sum(log(d$follow_up[d$cv_event == 1])))
  expect_identical(regtab(f, stats = "n")$body[[1]][2], "Subjects")
  # Time ratios are exp(b), CI bounds too (tabtools 2.1.10+); the header
  # can be overridden (AF).
  expect_equal(r$estimate[1], unname(exp(coef(f)[2])))
  expect_equal(c(r$conf.low[1], r$conf.high[1]),
               unname(exp(coef(f)[2] + c(-1, 1) * stats::qnorm(0.975) * sqrt(vcov(f)[2, 2]))))
  expect_identical(regtab(f, coef = "AF")$header[[2]]$text[2], "AF")
})

test_that("crr rows: covariate names, SHR", {
  skip_if_not_installed("cmprsk")
  d <- golden_fixture("cohort3500")
  cr <- cmprsk::crr(d$follow_up, d$event_type, cov1 = as.matrix(d[, c("treated", "female")]),
                    failcode = 1, cencode = 0)
  tt <- regtab(cr, stats = "n ll")
  expect_identical(tt$body[[1]], c("treated", "female", "Subjects", "Log-likelihood"))
  expect_identical(tt$stored$coef_label, "SHR")
  expect_equal(tt$stored$table[, 1], exp(cr$coef), ignore_attr = TRUE)
})

test_that("vce = 'model' uses each fit's own vcov()", {
  for (p in c("MASS", "nnet", "pscl")) skip_if_not_installed(p)
  d <- golden_fixture("cohort3500", factors = "education")
  z <- golden_fixture("zip")
  for (f in list(MASS::polr(education ~ index_age, data = d, Hess = TRUE),
                 nnet::multinom(education ~ index_age, data = d, trace = FALSE),
                 pscl::zeroinfl(event_count ~ treatment | 1, data = z))) {
    expect_identical(tabtools:::tt_vcov(f, "model"), vcov(f))
    a <- regtab(f, vce = "model", keepintercept = TRUE)
    b <- regtab(f, keepintercept = TRUE)
    expect_identical(dim(a$body), dim(b$body))
  }
})

# ---------------------------------------------------------------------------
# Data-frame escape hatch (task 5.8)

test_that("a tidy_plus_plus() data frame renders like the fitted model", {
  skip_if_not_installed("broom.helpers")
  a <- golden_fixture("auto", factors = "rep78")
  f <- glm(foreign ~ mpg + rep78, binomial, a)
  tf <- function(x, exponentiate = FALSE, conf.level = 0.95, ...) {
    w <- tabtools:::tt_wald(x, conf.level)
    if (exponentiate) for (k in c("estimate", "conf.low", "conf.high")) w[[k]] <- exp(w[[k]])
    w
  }
  tp <- suppressWarnings(broom.helpers::tidy_plus_plus(f, tidy_fun = tf, exponentiate = TRUE,
                                                       add_header_rows = TRUE, intercept = TRUE))
  attr(tp, "stata_cmd") <- "logit"
  t1 <- suppressWarnings(regtab(f))
  t2 <- regtab(tp)
  expect_identical(t2$body[[1]], t1$body[[1]])
  expect_identical(t2$stored$coef_label, "OR")
  expect_identical(t2$body[[2]][3], "Reference")
  expect_false(any(t2$body[[1]] == "Intercept"))
  expect_identical(regtab(tp, keepintercept = TRUE)$body[[1]][nrow(t1$body) + 1L], "Intercept")
  # Without stata_cmd: broom.helpers' own record (exponentiate = TRUE,
  # coefficients_label "OR") gives the ratio scale (Milestone H review of
  # group t2a, P0-1); without that record, Coef. and no automatic
  # nointercept.
  attr(tp, "stata_cmd") <- NULL
  expect_identical(regtab(tp)$stored$coef_label, "OR")
  attr(tp, "exponentiate") <- NULL
  expect_error(regtab(tp), "looks exponentiated", fixed = TRUE)
  tp$statistic <- NULL
  expect_identical(regtab(tp)$stored$coef_label, "Coef.")
  attr(tp, "effect_scale") <- "OR"
  expect_identical(regtab(tp)$stored$coef_label, "OR")
})

test_that("data-frame input: equations, Stata equation names, glance stats, errors", {
  x <- data.frame(equation = c("annual_cost", "annual_cost", "selection_ll", "lnsigma", "_diparm1", "inflate"),
                  term = c("dose", "_cons", "score", "_cons", "/sigma", "z"),
                  var_label = c("Dose", NA, "Score", NA, NA, "Zed"),
                  estimate = c(1.5, 2, 0.5, 0.7, 2.04, 0.1), std.error = c(0.1, 0.1, 0.04, 0.05, 0.09, 1),
                  stringsAsFactors = FALSE)
  attr(x, "stata_cmd") <- "churdle"
  attr(x, "glance") <- list(nobs = 1200, logLik = -1707.93, df = 5, pseudo.r.squared = 0.1)
  tt <- regtab(x, keepintercept = TRUE, stats = "n aic r2")
  expect_identical(tt$body[[1]], c("annual cost: Dose", "annual cost: Intercept", "Selection equation: Score",
                                   "Scale: Intercept", "Ancillary: /sigma", "Inflation equation: Zed",
                                   "Observations", "AIC", "Pseudo R²"))
  expect_identical(tt$body[[2]][8], stata_fmt(-2 * -1707.93 + 10, "%12.2f"))
  expect_identical(tt$body[[3]][1], paste0("(", sprintf("%.2f", 1.5 - qnorm(0.975) * 0.1), ", ",
                                          sprintf("%.2f", 1.5 + qnorm(0.975) * 0.1), ")"))
  expect_identical(regtab(x)$body[[1]], c("annual cost: Dose", "Selection equation: Score",
                                          "Inflation equation: Zed"))
  expect_error(regtab(data.frame(term = "a")), "estimate")
  expect_error(regtab(data.frame(term = c("a", "a"), estimate = 1:2)), "repeats")
})

# ---------------------------------------------------------------------------
# Golden R20 (stcrreg) through cmprsk::crr, the exact route. The manifest's
# r_call keeps the coxph-on-finegray() fit, which only approximates stcrreg
# (SE ~1e-4, age estimate ~7e-4 relative off); crr reproduces stcrreg's
# estimates (to its default convergence) and, times N/(N-1), its vce(robust).

test_that("R20 exact route: regtab() on the equivalent crr fit matches the golden's cells", {
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort3500")
  cov <- c("treated", "index_age", "female")
  cr <- cmprsk::crr(d$follow_up, d$event_type, cov1 = as.matrix(d[, cov]), failcode = 1, cencode = 0)
  tt <- regtab(cr, models = "Fine-Gray")
  want <- golden_read_cells("R20")
  # crr keeps no variable labels, and regtab has no label argument: the rows
  # carry the covariate names, and every other cell must equal Stata's text.
  mm <- golden_compare_cells(tt, want, mode = "exact")
  body <- seq_len(nrow(want))[-(1:2)]
  expect_identical(mm[!(mm$col == 1L & mm$row %in% body), , drop = FALSE], mm[0, ])
  expect_identical(golden_as_cells(tt)[body, 1], cov)
  # Stored results: every scalar and macro field but the sink ones (no sink
  # is written here), and r(table) by position
  # (its row names are the labels).
  # r(methods) through the documented H11 transform (helper-golden.R).
  g <- golden_methods_regtab(golden_read_stored("R20"), "R20")
  scal <- g[g$kind %in% c("scalar", "macro") & !g$name %in% c("sheet", "xlsx", "markdown", "markdown_rows", "markdown_cols"), ]
  st <- tt$stored
  for (i in seq_len(nrow(scal))) {
    got <- st[[scal$name[i]]]
    if (is.numeric(got)) {
      expect_equal(got, as.numeric(scal$value[i]), label = scal$name[i])
    } else if (scal$name[i] == "methods") {
      # A crr fit has no formula: several coefficients may be one factor's
      # dummies, so no "multivariable" (review P3-3 of group t2a); the
      # coxph-on-finegray() route of the golden has one.
      expect_identical(got, sub("from multivariable ", "from ", golden_methods_swap(scal$value[i]), fixed = TRUE))
    } else {
      expect_identical(golden_methods_swap(got), golden_methods_swap(scal$value[i]), label = scal$name[i])
    }
  }
  tab <- g[g$kind == "matrix" & g$name == "table", ]
  shr <- golden_stored_num(tab$value)
  expect_equal(unname(st$table[, 1]), shr, tolerance = 1e-6)
  expect_equal(unname(exp(cr$coef)), shr, tolerance = 1e-6)
  # The manifest's coxph route is not exact: its age coefficient is ~7e-4
  # (relative) from crr's and stcrreg's (finegray()'s tied-time weights),
  # which the golden's display precision hides.
  e <- d
  e$event_type <- factor(e$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = e, etype = "cv")
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + index_age + female,
                        weights = fgwt, data = fg)
  expect_gt(max(abs(coef(fc) - cr$coef) / abs(cr$coef)), 1e-4)
})
