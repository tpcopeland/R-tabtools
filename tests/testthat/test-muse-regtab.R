# Muse audit of 2026-09-28, regtab lane: regression tests for the findings
# fixed in R/regtab*.R (log: R-Dev/_take_action/muse-fixlog-regtab.md).

test_that("muse P0-3: a clogit fit whose weights clogit ignored (exact method) is refused", {
  skip_if_not_installed("survival")
  set.seed(515)
  d <- data.frame(set = rep(1:60, each = 3), x = rnorm(180), w = runif(180, 0.5, 2))
  d$case <- as.integer(ave(d$x + rnorm(180), d$set, FUN = function(v) v == max(v)))
  f <- local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    suppressWarnings(survival::clogit(case ~ x + survival::strata(set), data = d, weights = w,
                                      method = "exact"))
  })
  expect_error(regtab(f), "weights")
  # An unweighted exact fit is still tabled.
  f0 <- local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    survival::clogit(case ~ x + survival::strata(set), data = d)
  })
  expect_s3_class(regtab(f0), "tt_table")
})

test_that("muse P1-1: a non-positive-definite observed information is refused, not blanked", {
  set.seed(3)
  d <- data.frame(x = rnorm(40))
  d$y <- exp(1 + d$x) + rnorm(40)
  f <- glm(y ~ x, family = gaussian(link = "log"), data = d)
  # Away from the MLE (as after a non-converged fit) the observed
  # information of a non-canonical link can be indefinite.
  f$coefficients[] <- c(-3, 0)
  expect_error(regtab(f), "not positive definite")
  expect_error(tt_vcov(f), "not positive definite")
  # The fit's own vcov() stays available.
  expect_s3_class(suppressWarnings(regtab(f, vce = "model")), "tt_table")
})

test_that("muse P1-2: profile intervals are named as such and the p-values as Wald in the methods sentence", {
  f <- glm(am ~ wt, data = mtcars, family = binomial)
  tt <- regtab(f, ci_method = "profile")
  expect_identical(tt$stored$methods,
                   paste("Odds ratios with 95% profile-likelihood confidence intervals from univariable",
                         "logistic regression; p-values from Wald tests."))
  # Wald (Stata's) keeps the golden sentence.
  expect_identical(regtab(f)$stored$methods,
                   "Odds ratios with 95% confidence intervals from univariable logistic regression.")
  # lm's profile intervals are its t intervals: nothing to flag.
  expect_identical(regtab(lm(mpg ~ wt, mtcars), ci_method = "profile")$stored$methods,
                   regtab(lm(mpg ~ wt, mtcars))$stored$methods)
})

test_that("muse P1-3 (superseded by B07): inverse-probability-like weights get the [aw] profile, with the IPTW warning", {
  set.seed(4)
  d <- data.frame(x = rnorm(200))
  d$y <- rbinom(200, 1, plogis(d$x))
  d$w <- runif(200, 1, 5)
  f <- suppressWarnings(glm(y ~ x, family = binomial, weights = w, data = d))
  # Under vce = "stata" the weights are analytic (Stata glm [aweight]): the
  # profile is that likelihood's, rescaled to mean 1, and regtab still warns
  # that vce = "robust" is the [pweight] reading.
  expect_warning(tt <- regtab(f, ci_method = "profile"), "inverse-probability weights")
  expect_identical(tt$stored$ci_method, "profile")
  d$wa <- d$w / mean(d$w)
  fa <- suppressWarnings(glm(y ~ x, family = binomial, weights = wa, data = d))
  r <- tt$meta$regtab_rows
  expect_equal(log(c(r$conf.low[r$key == "x"], r$conf.high[r$key == "x"])),
               unname(suppressWarnings(suppressMessages(confint(fa)))["x", ]), tolerance = 1e-6)
  # R07: vce = "model" profiles the fit's own likelihood, and warns too.
  expect_warning(tm <- regtab(f, ci_method = "profile", vce = "model"), "inverse-probability weights")
  expect_identical(tm$stored$ci_method, "profile")
  # (Its Wald intervals, the fit's own vcov(), stay silent, as before.)
  expect_no_warning(regtab(f, vce = "model"))
  # Integer (frequency) weights keep their likelihood, and the profile.
  d$wi <- round(d$w)
  f2 <- glm(y ~ x, family = binomial, weights = wi, data = d)
  expect_identical(regtab(f2, ci_method = "profile")$stored$ci_method, "profile")
})

test_that("muse P1-4: levels seen only at zero weight are not observed; a zero-weight base is refused", {
  set.seed(5)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 30)), x = rnorm(90))
  d$y <- rbinom(90, 1, 0.5)
  # Stata (logit y i.g x [fw=w2], probe 2026-09-28): 1b.g, 3.g; level 2 is
  # not in the estimation sample and has no row.
  d$w2 <- ifelse(d$g == "b", 0, 1)
  f2 <- glm(y ~ g + x, family = binomial, weights = w2, data = d)
  tt <- regtab(f2)
  expect_identical(trimws(tt$body[[1]]), c("g", "a", "c", "x"))
  expect_equal(unname(tt$stored$table[1, 1]), unname(exp(coef(f2)["gc"])))
  # Base level only at zero weight: Stata's base is then b (2b.g), and R's
  # gb would be b against c, so the table is refused.
  d$w <- ifelse(d$g == "a", 0, 1)
  f <- glm(y ~ g + x, family = binomial, weights = w, data = d)
  expect_error(regtab(f), "occurs only in rows with zero weight")
  # Refit on the positive weights: Stata's table (2b.g, 3.g = -0.2172368).
  f1 <- glm(y ~ g + x, family = binomial, weights = w, data = d, subset = w > 0)
  tt1 <- regtab(f1, digits = 4)
  expect_identical(trimws(tt1$body[[1]]), c("g", "b", "c", "x"))
  expect_equal(unname(log(tt1$stored$table[1, 1])), -0.2172368, tolerance = 1e-6)
})

test_that("muse P1-7: a data frame's ratio limits must be positive and ordered", {
  y <- data.frame(term = c("a", "b"), estimate = c(1.5, 2), conf.low = c(-0.1, 1.2),
                  conf.high = c(2.5, 3), p.value = c(0.1, 0.01))
  attr(y, "effect_scale") <- "OR"
  attr(y, "conf.level") <- .95
  expect_error(regtab(y), "negative OR confidence limits")
  v <- data.frame(term = c("a", "b"), estimate = c(1.5, 2), conf.low = c(2.6, 1.2),
                  conf.high = c(2.5, 3), p.value = c(0.1, 0.01))
  attr(v, "effect_scale") <- "OR"
  attr(v, "conf.level") <- .95
  expect_error(regtab(v), "conf.low.*above.*conf.high")
  # Negative limits of a difference stay valid.
  w <- data.frame(term = "a", estimate = 0.5, conf.low = -0.1, conf.high = 1.1, p.value = 0.1)
  attr(w, "effect_scale") <- "Coef."
  attr(w, "conf.level") <- .95
  expect_s3_class(regtab(w), "tt_table")
})

test_that("muse P1-9: the survreg classifier itself refuses distributions without a Stata equivalent", {
  skip_if_not_installed("survival")
  f <- survival::survreg(survival::Surv(time, status) ~ age, data = survival::lung, dist = "logistic")
  expect_error(tabtools:::tt_model_info(f), "no Stata equivalent")
  expect_error(regtab(f), "does not support")
  w <- survival::survreg(survival::Surv(time, status) ~ age, data = survival::lung, dist = "weibull")
  expect_identical(tabtools:::tt_model_info(w)$effect_scale, "TR")
})

test_that("muse P1-15: GEE working correlations without a verified xtgee equivalent are refused", {
  skip_if_not_installed("geepack")
  set.seed(15)
  d <- data.frame(id = rep(1:40, each = 4), t = rep(1:4, 40), x = rnorm(160))
  d$y <- 1 + 0.5 * d$x + rep(rnorm(40), each = 4) + rnorm(160)
  # geepack's ar1 is not xtgee corr(ar 1) (probe: b_x 0.49836 vs 0.50005).
  g <- geepack::geeglm(y ~ x, gaussian, d, id = id, waves = t, corstr = "ar1")
  expect_error(regtab(g), "working correlation")
  # Exchangeable and unstructured match xtgee to 1e-10: still tabled.
  for (cs in c("exchangeable", "unstructured")) {
    ge <- geepack::geeglm(y ~ x, gaussian, d, id = id, waves = t, corstr = cs)
    expect_s3_class(regtab(ge), "tt_table")
  }
})

test_that("muse P1-18: glmmTMB covariance structures without a Stata equivalent are refused", {
  skip_if_not_installed("glmmTMB")
  set.seed(18)
  d <- data.frame(g = factor(rep(1:30, each = 6)), x = rnorm(180))
  d$y <- 1 + d$x + rep(rnorm(30), each = 6) + rep(rnorm(30, sd = 0.5), each = 6) * d$x + rnorm(180)
  f <- suppressWarnings(glmmTMB::glmmTMB(y ~ x + cs(1 + x | g), data = d))
  expect_error(regtab(f), "covariance structure")
  f1 <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = d)
  # (glmmTMB/lme4 may warn once per session that nobars() moved to reformulas.)
  expect_s3_class(suppressWarnings(regtab(f1)), "tt_table")
})

test_that("muse P1-21: a survreg with strata() (one scale per stratum) is noted as an R-only layout", {
  skip_if_not_installed("survival")
  set.seed(21)
  d <- data.frame(x = rnorm(200), g = factor(sample(1:2, 200, TRUE)))
  d$time <- rexp(200, exp(0.3 * d$x))
  d$status <- rbinom(200, 1, 0.7)
  f <- survival::survreg(survival::Surv(time, status) ~ x + strata(g), data = d)
  rlang::reset_message_verbosity("tabtools_survreg_strata")
  expect_message(tt <- regtab(f, keepintercept = TRUE), "no Stata .*streg.* equivalent")
  expect_true(any(grepl("ln_p \\(", tt$body[[1]])))
})

test_that("muse P1-23: Fine-Gray subjects inferred without an id match finegray()'s own subjects (ties, equal covariates)", {
  skip_if_not_installed("survival")
  for (s in 1:25) {
    set.seed(s)
    n <- sample(20:80, 1)
    d <- data.frame(id = seq_len(n), x = sample(0:1, n, TRUE), z = sample(1:2, n, TRUE),
                    time = round(stats::rexp(n, 0.2), s %% 3) + 0.5,
                    ev = factor(sample(0:2, n, TRUE, c(0.4, 0.3, 0.3)), 0:2, c("cens", "a", "b")))
    fg <- survival::finegray(survival::Surv(time, ev) ~ x + z + id, data = d, etype = "a")
    mf <- stats::model.frame(survival::Surv(fgstart, fgstop, fgstatus) ~ x + z, data = fg, weights = fgwt)
    sid <- tabtools:::.rt_fg_infer_subject(mf, tabtools:::.rt_fg_order(mf))
    # Either the true partition into subjects, or NULL (refused): never another one.
    if (!is.null(sid)) {
      tab <- table(sid, fg$id)
      expect_true(all(rowSums(tab > 0) == 1) && all(colSums(tab > 0) == 1), info = s)
    }
  }
})

test_that("muse P2-2: fvgen interaction cells with no observations have no row (fvgen creates no variable)", {
  a <- golden_fixture("auto", factors = c("foreign", "rep78"))
  tt <- regtab(lm(price ~ foreign * rep78, a))
  # Stata 17 + fvgen 1.2.5 (probe 2026-09-28): `fvgen i.foreign##i.rep78`
  # makes no Foreign x rep78=1/2 variables (no foreign cars there), and
  # regress omits Foreign x rep78=5.
  expect_identical(trimws(tt$body[[1]]),
                   c("Foreign", "rep78=2", "rep78=3", "rep78=4", "rep78=5", "Foreign \u00d7 rep78=3",
                     "Foreign \u00d7 rep78=4", "Foreign \u00d7 rep78=5", "Intercept"))
  expect_identical(tt$body[[2]][8], "Omitted")
  # Native interactions keep Stata's Empty rows (golden R12/R13).
  tn <- regtab(lm(price ~ foreign * rep78, a), interactions = "native")
  expect_identical(tn$body[[2]][tn$body[[1]] %in% c("1.foreign#1.rep78", "1.foreign#2.rep78")], c("Empty", "Empty"))
})

test_that("muse P2-3: an interaction without its main effects, collinear with the intercept, is refused", {
  a <- golden_fixture("auto", factors = c("foreign", "rep78"))
  expect_error(regtab(lm(price ~ foreign:rep78, a), interactions = "native"), "without (its|their) main effect")
  # One slope per level (no collinearity) is Stata's i.f#c.x: still tabled.
  tt <- regtab(lm(Sepal.Length ~ Species:Petal.Width, iris), interactions = "native")
  expect_s3_class(tt, "tt_table")
  # An offset() row: no crash (the second half of the finding).
  a$o <- log(a$weight)
  expect_s3_class(regtab(glm(as.integer(rep78) ~ mpg + offset(o), poisson, a)), "tt_table")
})

test_that("muse P2-4: a bare variable name keeps or drops the rows of a poly()/spline term of it", {
  f <- lm(mpg ~ poly(wt, 2) + hp + splines::ns(disp, 2), mtcars)
  expect_identical(regtab(f, keep = "wt")$body[[1]], c("poly(wt, 2)1", "poly(wt, 2)2"))
  expect_false(any(grepl("disp", regtab(f, drop = "disp")$body[[1]])))
  # A plain name still does not select a variable whose name merely contains it.
  expect_error(regtab(lm(mpg ~ poly(wt, 2), mtcars), keep = "w"), "matched no")
})

test_that("muse P2-5: integer level texts with leading zeros are coded by their value", {
  expect_identical(unclass(tabtools:::.rt_codes(factor(c("01", "02", "10")))),
                   structure(c(`01` = "1", `02` = "2", `10` = "10"), positional = FALSE))
  d <- data.frame(y = c(1, 3, 2, 5, 4, 6, 8, 7), g = rep(c("01", "02"), 4))
  # Stata's key for the level (after destring or encode) is 2.g.
  expect_identical(regtab(lm(y ~ g, d), keep = "2.g")$body[[1]], "  02")
  # Two texts of one value keep their own texts.
  expect_identical(as.vector(tabtools:::.rt_codes(factor(c("1", "01")))), c("01", "1"))
})

test_that("muse P2-11: multi-equation data frames with reordered levels are joined by level", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("broom.helpers")
  set.seed(211)
  d <- data.frame(g = factor(sample(c("lo", "mid", "hi"), 400, TRUE), levels = c("lo", "mid", "hi")),
                  x = rnorm(400), y = factor(sample(c("A", "B", "C"), 400, TRUE)))
  d2 <- d
  d2$g <- factor(d2$g, levels = c("mid", "lo", "hi"))
  f1 <- nnet::multinom(y ~ g + x, d, trace = FALSE)
  f2 <- nnet::multinom(y ~ g + x, d2, trace = FALSE)
  t1 <- broom.helpers::tidy_plus_plus(f1, exponentiate = TRUE)
  t2 <- broom.helpers::tidy_plus_plus(f2, exponentiate = TRUE)
  attr(t1, "se_scale") <- "link"
  attr(t2, "se_scale") <- "link"
  attr(t1, "effect_scale") <- attr(t2, "effect_scale") <- "RRR"
  attr(t1, "conf.level") <- attr(t2, "conf.level") <- .95
  attr(t1, "inference_reference") <- attr(t2, "inference_reference") <- "normal"
  tt <- regtab(t1, t2)
  lab <- trimws(tt$body[[1]])
  # Model 2's reference (mid) sits on the mid row, and its lo estimate on lo.
  expect_identical(tt$body[[5]][lab == "B: mid"], "Reference")
  expect_identical(tt$body[[2]][lab == "B: lo"], "Reference")
  lo2 <- unname(exp(coef(f2)["B", "glo"]))
  expect_identical(tt$body[[5]][lab == "B: lo"], sprintf("%.2f", lo2))
})

test_that("muse P2-16: a one-cluster GEE's sandwich variance is refused, not infinite", {
  skip_if_not_installed("geepack")
  set.seed(216)
  d <- data.frame(id = 1, x = rnorm(50))
  d$y <- d$x + rnorm(50)
  g <- geepack::geeglm(y ~ x, gaussian, d, id = id)
  expect_error(regtab(g, vce = "robust"), "at least two")
  expect_s3_class(regtab(g), "tt_table")
})

test_that("muse P2-17: zinb ancillary rows match Stata's regtab, keepintercept (p for lnalpha, none for alpha)", {
  skip_if_not_installed("pscl")
  d <- golden_fixture("zip")
  f <- pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female, data = d, dist = "negbin")
  tt <- regtab(f, keepintercept = TRUE)
  # Stata 17, tabtools 2.1.14 (probe 2026-09-28): `zinb event_count
  # treatment age_z female, inflate(zero_risk female)`, `regtab, keepintercept`.
  b <- tt$body
  i <- match(c("Ancillary: lnalpha", "Ancillary: alpha"), trimws(b[[1]]))
  expect_identical(b[[2]][i], c("-1.58", "0.21"))
  expect_identical(b[[3]][i], c("(-2.08, -1.08)", "(0.12, 0.34)"))
  expect_identical(b[[4]][i], c("<0.001", ""))
})

test_that("muse P2-20: a multinomial fit with an offset is noted as having no Stata equivalent", {
  skip_if_not_installed("nnet")
  set.seed(220)
  d <- data.frame(x = rnorm(200), o = rnorm(200, sd = 0.1))
  d$y <- factor(sample(c("a", "b"), 200, TRUE))
  f <- nnet::multinom(y ~ x + offset(o), data = d, trace = FALSE)
  rlang::reset_message_verbosity("tabtools_multinom_offset")
  expect_message(regtab(f), "refuses .*offset")
})

test_that("muse P2-21: zero-inflation and hurdle zero parts do not share rows", {
  skip_if_not_installed("pscl")
  z <- golden_fixture("zip")
  zi <- pscl::zeroinfl(event_count ~ treatment + female | female, data = z)
  hu <- pscl::hurdle(event_count ~ treatment + female | female, data = z)
  tt <- regtab(zi, hu)
  lab <- trimws(tt$body[[1]])
  expect_true("Inflation equation: Female" %in% lab)
  expect_true("Selection equation: Female" %in% lab)
  i <- match("Selection equation: Female", lab)
  expect_identical(tt$body[[2]][i], "")
  expect_identical(tt$body[[5]][i], sprintf("%.2f", unname(stats::coef(hu)["zero_female"])))
})

test_that("muse M4: glm.nb with an identity link is Stata's glm, family(nbinomial ml) link(identity)", {
  skip_if_not_installed("MASS")
  d <- data.frame(x = (1:80) / 40,
                  y = c(0L, 1L, 2L, 0L, 0L, 2L, 1L, 2L, 1L, 2L, 0L, 8L, 6L, 3L, 3L, 0L, 1L, 4L, 1L, 6L, 0L, 5L,
                        8L, 1L, 7L, 2L, 0L, 4L, 6L, 2L, 3L, 1L, 4L, 1L, 1L, 2L, 5L, 7L, 1L, 4L, 4L, 1L, 0L, 2L,
                        9L, 2L, 5L, 4L, 1L, 3L, 3L, 1L, 4L, 0L, 1L, 3L, 7L, 3L, 2L, 2L, 1L, 0L, 7L, 2L, 2L, 2L,
                        3L, 8L, 2L, 8L, 4L, 2L, 16L, 3L, 0L, 4L, 1L, 7L, 15L, 2L))
  f <- MASS::glm.nb(y ~ x, data = d, link = identity, control = glm.control(epsilon = 1e-12, maxit = 100))
  # Stata 17 (probe 2026-09-28): glm y x, family(nbinomial ml) link(identity)
  # treats alpha as fixed once estimated: no /lnalpha row, e(rank) = 2, OIM
  # standard errors at fixed alpha.
  # Stata takes alpha from nbreg (log link: 0.46805 vs glm.nb's joint
  # 0.46759), so the estimates agree to ~2e-5 and the variances to ~2e-4.
  expect_equal(unname(coef(f)), c(1.65708697950115, 1.49239843304137), tolerance = 1e-4)
  se <- sqrt(diag(tabtools:::tt_vcov(f)))
  expect_equal(unname(se[c("x", "(Intercept)")]), c(0.563776613619243, 0.523606474902361), tolerance = 1e-3)
  # vce(robust): beta's sandwich at fixed alpha.
  rse <- sqrt(diag(tabtools:::tt_vcov(f, "robust")))
  expect_equal(unname(rse[c("x", "(Intercept)")]), c(0.639239155730118, 0.577376540239731), tolerance = 1e-3)
  tt <- regtab(f, keepintercept = TRUE, stats = c("n", "ll", "aic", "bic"))
  expect_identical(trimws(tt$body[[1]]), c("x", "Intercept", "Observations", "AIC", "BIC", "Log-likelihood"))
  expect_identical(tt$body[[2]], c("1.49", "1.66", "80", "358.18", "362.95", "-177.09"))
  expect_identical(tt$body[[3]][1:2], c("(0.39, 2.60)", "(0.63, 2.68)"))
})

test_that("muse P1-14: a multinom whose base is not the most frequent outcome gets a note", {
  skip_if_not_installed("nnet")
  set.seed(14)
  d <- data.frame(x = rnorm(300))
  d$y <- factor(sample(c("a", "b", "c"), 300, replace = TRUE, prob = c(0.2, 0.3, 0.5)))
  withr::local_options(rlib_message_verbosity = "verbose")
  f <- nnet::multinom(y ~ x, data = d, trace = FALSE)
  msgs <- paste(testthat::capture_messages(regtab(f)), collapse = "")
  expect_match(msgs, "base outcome", fixed = TRUE)
  expect_match(msgs, "baseoutcome", fixed = TRUE)
  expect_match(msgs, "\"c\"", fixed = TRUE)
  d$y <- stats::relevel(d$y, "c")
  f2 <- nnet::multinom(y ~ x, data = d, trace = FALSE)
  expect_false(any(grepl("base outcome", testthat::capture_messages(regtab(f2)), fixed = TRUE)))
})
