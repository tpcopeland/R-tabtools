# Milestone H task H4 (external review F05, F24, F26, F28, F31): storage
# options, data changed after fitting, saved fits, survreg's cluster
# factor, geeglm's clusters, and mixed-model fallbacks (decisions H-D6,
# H-D7). The contract: the same table, or a refusal that names the fix;
# never a changed number and never a base-R error.

h4_data <- function() {
  set.seed(11)
  n <- 200
  d <- data.frame(x = rnorm(n), g = factor(sample(c("a", "b", "c"), n, TRUE)), id = rep(1:40, each = 5))
  d$yb <- rbinom(n, 1, plogis(0.3 * d$x))
  d$yc <- rnbinom(n, mu = exp(0.5 + 0.2 * d$x), size = 2)
  d$yo <- factor(cut(d$x + rnorm(n), c(-Inf, -0.5, 0.5, Inf), labels = c("lo", "mid", "hi")), ordered = TRUE)
  d$ym <- factor(sample(c("A", "B", "C"), n, TRUE))
  d$t <- rexp(n, exp(0.2 * d$x))
  d$e <- rbinom(n, 1, 0.7)
  d$yl <- 2 + d$x + rnorm(n)
  d$yz <- ifelse(rbinom(n, 1, 0.3) == 1, 0, rpois(n, 2))
  d$w <- sample(1:3, n, TRUE)
  d
}

h4_S <- c("n", "ll", "aic", "r2")

test_that("storage options: the same table, or a refusal naming the option (F05, F28)", {
  skip_on_cran()  # heavy (CRAN-lane runtime, pre-release review P2-1); runs in every NOT_CRAN gate
  for (p in c("survival", "MASS", "nnet", "ordinal", "pscl")) skip_if_not_installed(p)
  d <- h4_data()
  cases <- list(
    lm = list(quote(lm(yl ~ x + g, d)), list(model = FALSE, x = TRUE, y = TRUE, qr = FALSE)),
    glm = list(quote(glm(yb ~ x + g, binomial("probit"), d)), list(model = FALSE, x = TRUE, y = FALSE)),
    negbin = list(quote(MASS::glm.nb(yc ~ x + g, d)), list(model = FALSE, x = TRUE, y = FALSE)),
    coxph = list(quote(survival::coxph(survival::Surv(t, e) ~ x + g, d, ties = "breslow")),
                 list(model = TRUE, x = TRUE, y = FALSE)),
    coxph_cl = list(quote(survival::coxph(survival::Surv(t, e) ~ x + g, d, ties = "breslow", cluster = id)),
                    list(model = TRUE, x = TRUE, y = FALSE)),
    survreg = list(quote(survival::survreg(survival::Surv(t, e) ~ x + g, d)), list(model = TRUE, x = TRUE, y = FALSE)),
    survreg_cl = list(quote(survival::survreg(survival::Surv(t, e) ~ x + g + cluster(id), d)),
                      list(model = TRUE, x = TRUE, y = FALSE)),
    multinom = list(quote(nnet::multinom(ym ~ x + g, d, trace = FALSE)), list(model = TRUE, Hess = TRUE)),
    polr = list(quote(MASS::polr(yo ~ x + g, d)), list(model = FALSE, Hess = TRUE)),
    clm = list(quote(ordinal::clm(yo ~ x + g, data = d)), list(model = FALSE, Hess = FALSE)),
    zeroinfl = list(quote(pscl::zeroinfl(yz ~ x + g | x, d)), list(model = FALSE, x = TRUE, y = FALSE)),
    hurdle = list(quote(pscl::hurdle(yz ~ x + g | x, d)), list(model = FALSE, x = TRUE, y = FALSE)),
    # Weighted fits, whose robust/cluster reading has Stata's [pw]
    # statistics (review T2B-05, T2B-11).
    coxph_w = list(quote(survival::coxph(survival::Surv(t, e) ~ x + g, d, ties = "breslow", weights = w)),
                   list(model = TRUE, x = TRUE, y = FALSE)),
    glm_w = list(quote(glm(yb ~ x + g, quasibinomial, d, weights = w)), list(model = FALSE, x = TRUE, y = FALSE)),
    lm_w = list(quote(lm(yl ~ x + g, d, weights = w)), list(model = FALSE, x = TRUE, y = TRUE))
  )
  # The options each class cannot do without, and the fix its refusal names.
  refused <- c(lm.qr = "qr = TRUE", clm.model = "model = TRUE", zeroinfl.model = "model = TRUE",
               hurdle.model = "model = TRUE")
  # Every vce the class supports (cluster on id), and every statistic; every
  # stored result compared, the methods sentence included (review T2B-11).
  S <- c("n", "ll", "aic", "bic", "r2")
  for (cls in names(cases)) {
    base_call <- cases[[cls]][[1]]
    base <- suppressWarnings(eval(base_call))
    for (v in tt_vce_types(base)) {
      cl_arg <- if (identical(v, "cluster")) ~id else NULL
      rt <- function(f) suppressWarnings(suppressMessages(regtab(f, vce = v, cluster = cl_arg, stats = S)))
      want <- rt(base)
      for (opt in names(cases[[cls]][[2]])) {
        cl <- base_call
        cl[[opt]] <- cases[[cls]][[2]][[opt]]
        fit <- suppressWarnings(eval(cl))
        lab <- paste(cls, opt, v)
        got <- tryCatch(rt(fit), error = function(e) e)
        key <- paste0(sub("_w$", "", cls), ".", opt)
        if (key %in% names(refused)) {
          expect_s3_class(got, "error")
          expect_match(conditionMessage(got), refused[[key]], fixed = TRUE, label = lab)
          expect_match(conditionMessage(got), paste0("`", opt, " = "), fixed = TRUE, label = lab)
        } else {
          expect_false(inherits(got, "error"), label = paste(lab, if (inherits(got, "error")) conditionMessage(got)))
          if (!inherits(got, "error")) {
            # polr's own vcov() (vce = "model") is optim()'s Hessian: a
            # Hess = FALSE fit is refitted for it from its estimates, as
            # MASS's vcov() does, and optim() stops ~1e-6 away, so the
            # Hess = TRUE and FALSE fits differ there by construction.
            tol <- if (cls == "polr" && opt == "Hess" && v == "model") 1e-4 else testthat_tolerance()
            if (tol == testthat_tolerance()) expect_identical(got$body, want$body, label = lab)
            expect_equal(got$stored, want$stored, tolerance = tol, label = lab)
          }
        }
      }
    }
  }
})

test_that("data changed or removed after fitting: the same table, or a refusal (F24, H-D7)", {
  skip_on_cran()  # heavy (CRAN-lane runtime, pre-release review P2-1); runs in every NOT_CRAN gate
  for (p in c("survival", "MASS", "nnet")) skip_if_not_installed(p)
  mk <- function() {
    d <- h4_data()
    attr(d$x, "label") <- "Exposure"
    d
  }
  fits <- list(
    lm_mf = quote(lm(yl ~ x + g, dd, model = FALSE)),
    glm_mf = quote(glm(yb ~ x + g, binomial("probit"), dd, model = FALSE)),
    nb_mf = quote(MASS::glm.nb(yc ~ x + g, dd, model = FALSE)),
    cox = quote(survival::coxph(survival::Surv(t, e) ~ x + g, dd, ties = "breslow")),
    cox_cl = quote(survival::coxph(survival::Surv(t, e) ~ x + g, dd, ties = "breslow", cluster = id)),
    sr = quote(survival::survreg(survival::Surv(t, e) ~ x + g, dd)),
    sr_cl = quote(survival::survreg(survival::Surv(t, e) ~ x + g + cluster(id), dd)),
    mn = quote(nnet::multinom(ym ~ x + g, dd, trace = FALSE)),
    polr_mf = quote(MASS::polr(yo ~ x + g, dd, model = FALSE))
  )
  for (nm in names(fits)) {
    for (mut in c("drop", "perm", "edit", "remove")) {
      dd <- mk()
      f <- eval(fits[[nm]])
      want <- regtab(f, stats = h4_S)$body
      if (mut == "drop") dd <- dd[dd$g != "a", ]
      if (mut == "perm") {
        dd <- dd[sample(nrow(dd)), ]
        attr(dd$x, "label") <- "Exposure"
      }
      if (mut == "edit") dd$x <- rev(dd$x)
      if (mut == "remove") rm(dd)
      got <- ht2b_try(suppressWarnings(regtab(f, stats = h4_S)))
      expect_same_or_refused(got, want, "model = TRUE", paste(nm, mut))
      # Re-sorting with the row names kept never refuses.
      if (mut == "perm") expect_identical(got, want, label = paste(nm, "perm"))
      # Removing, filtering or editing the data of a fit that kept none
      # always refuses (glm keeps its data).
      if (mut != "perm" && nm != "glm_mf") expect_s3_class(got, "ht2b_refusal")
    }
  }
})

test_that("a saved fit in a fresh session: the same table, or a refusal naming model = TRUE", {
  skip_on_cran()
  for (p in c("survival", "nnet")) skip_if_not_installed(p)
  rds <- tempfile(fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)
  # Fitted at the top level of one session (so the data are not saved with
  # the formula's environment), read in another.
  out1 <- ht2b_fresh_r(c(
    "set.seed(5); d <- data.frame(x = rnorm(120), g = factor(sample(letters[1:3], 120, TRUE)))",
    "d$y <- 1 + d$x + rnorm(120); d$b <- rbinom(120, 1, 0.5); d$t <- rexp(120); d$e <- rbinom(120, 1, 0.7)",
    "d$m <- factor(sample(c('A', 'B', 'C'), 120, TRUE))",
    "fits <- list(lm = lm(y ~ x + g, d), lm_mf = lm(y ~ x + g, d, model = FALSE),",
    "  glm = glm(b ~ x + g, binomial, d),",
    "  cox = survival::coxph(survival::Surv(t, e) ~ x + g, d, ties = 'breslow'),",
    "  cox_m = survival::coxph(survival::Surv(t, e) ~ x + g, d, ties = 'breslow', model = TRUE),",
    "  sr_m = survival::survreg(survival::Surv(t, e) ~ x + g, d, model = TRUE),",
    "  mn = nnet::multinom(m ~ x + g, d, trace = FALSE),",
    "  mn_m = nnet::multinom(m ~ x + g, d, trace = FALSE, model = TRUE))",
    "tabs <- lapply(fits, function(f) regtab(f, stats = c('n', 'll'))$body)",
    sprintf("saveRDS(list(fits = fits, tabs = tabs), %s)", deparse(rds)),
    "cat('saved\\n')"
  ))
  skip_if_not(any(grepl("^saved", out1)), paste(out1, collapse = "\n"))
  out2 <- ht2b_fresh_r(c(
    sprintf("s <- readRDS(%s)", deparse(rds)),
    "for (nm in names(s$fits)) {",
    "  r <- tryCatch(regtab(s$fits[[nm]], stats = c('n', 'll'))$body, error = function(e) e)",
    "  res <- if (inherits(r, 'error')) paste('refused', grepl('model = TRUE', conditionMessage(r), fixed = TRUE))",
    "    else if (identical(r, s$tabs[[nm]])) 'identical' else 'changed'",
    "  cat(nm, res, '\\n')",
    "}"
  ))
  res <- trimws(grep("^(lm|lm_mf|glm|cox|cox_m|sr_m|mn|mn_m) ", out2, value = TRUE))
  got <- stats::setNames(sub("^\\S+ ", "", res), sub(" .*$", "", res))
  expect_identical(got[c("lm", "glm", "cox_m", "sr_m", "mn_m")],
                   c(lm = "identical", glm = "identical", cox_m = "identical", sr_m = "identical", mn_m = "identical"))
  expect_identical(got[c("lm_mf", "cox", "mn")], c(lm_mf = "refused TRUE", cox = "refused TRUE", mn = "refused TRUE"))
})

test_that("polr(model = FALSE) reads the data only when they reproduce the fit (F28)", {
  skip_if_not_installed("MASS")
  d <- h4_data()
  f <- MASS::polr(yo ~ x + g, d, model = FALSE, Hess = TRUE)
  want <- regtab(MASS::polr(yo ~ x + g, d, Hess = TRUE), stats = h4_S)$body
  expect_identical(regtab(f, stats = h4_S)$body, want)
  expect_equal(tt_vcov(f), tt_vcov(MASS::polr(yo ~ x + g, d, Hess = TRUE)))
  d <- d[-1, ]
  expect_error(regtab(f), "fitted with `model = FALSE`", fixed = TRUE)
  expect_error(tt_vcov(f), "Refit with `model = TRUE`", fixed = TRUE)
})

test_that("survreg with cluster(): G/(G - 1) from the institutions, not N/(N - 1) (F26; Stata S1)", {
  skip_on_cran()
  for (p in c("survival", "haven")) skip_if_not_installed(p)
  lung <- ht2b_read("lung")
  f <- survival::survreg(survival::Surv(time, status) ~ age + sex + cluster(inst), data = lung)
  e <- ht2b_e("S1")
  expect_identical(e$e[["N_clust"]], 18)
  se <- sqrt(diag(tt_vcov(f)))
  expect_equal(unname(se[c("age", "sex", "(Intercept)")]), unname(e$se[c("_t:age", "_t:sex", "_t:_cons")]),
               tolerance = 1e-8)
  expect_equal(unname(se / sqrt(diag(vcov(f)))), rep(sqrt(18 / 17), 4), tolerance = 1e-12)
  expect_ht2b(regtab(f, stats = c("n", "ll")), "S1", 1e-9)
  # The same with model = TRUE, and inst's missing value is not a row.
  fm <- survival::survreg(survival::Surv(time, status) ~ age + sex + cluster(inst), data = lung, model = TRUE)
  expect_equal(tt_vcov(fm), tt_vcov(f))
})

test_that("geeglm with an id not sorted is refused; clusters count as geepack forms them (F31)", {
  skip_if_not_installed("geepack")
  set.seed(4)
  d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
  d$y <- d$x + rnorm(30)[d$id] + rnorm(120)
  ds <- d[sample(nrow(d)), ]
  g <- geepack::geeglm(y ~ x, gaussian, ds, id = id, corstr = "exchangeable")
  expect_gt(length(g$geese$clusz), 30)
  expect_error(regtab(g), "not sorted by `id`", fixed = TRUE)
  expect_error(tt_vcov(g, "robust"), "Sort the data by")
  expect_identical(tabtools:::.rt_gee_groups(g), length(g$geese$clusz))
  ok <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "exchangeable")
  expect_identical(tabtools:::.rt_gee_groups(ok), 30L)
  expect_s3_class(regtab(ok), "tt_table")
})

test_that("mixed-model fallbacks warn on every call and are recorded in stored$vce_fallback (H-D6)", {
  skip_if_not_installed("lme4")
  d <- golden_fixture("nhanes2")
  d$bmi2 <- d$bmi / 10
  gm <- suppressWarnings(lme4::glmer(bmi2 ~ age + (1 | location), family = Gamma("log"), data = d))
  for (k in 1:2) {
    w <- capture_warnings(tt <- regtab(gm))
    expect_length(grep("could not be formed", w), 1L)
    expect_match(tt$stored$vce_fallback, "lme4 vcov\\(\\) for the fixed effects")
  }
  expect_warning(tt_vcov(gm), "could not be formed")
  expect_warning(tt_vcov(gm), "could not be formed")
  expect_warning(tt_vcov(gm), class = "tabtools_vce_fallback")
  ok <- lme4::glmer(highbp ~ age + (1 | location), family = binomial, data = d)
  both <- suppressWarnings(regtab(ok, gm, noreeffects = TRUE))
  expect_identical(both$stored$vce_fallback[1], "")
  expect_null(regtab(ok)$stored$vce_fallback)
})

test_that("glmmTMB: a failed full variance warns instead of silently dropping intervals (F28)", {
  skip_if_not_installed("glmmTMB")
  d <- golden_fixture("nhanes2")
  f <- glmmTMB::glmmTMB(highbp ~ age + (1 | location), family = binomial, data = d)
  bad <- f
  bad$fit$par <- bad$fit$par[-1]
  expect_warning(v <- tabtools:::.rt_tmb_vcov_full(bad), class = "tabtools_vce_fallback")
  expect_null(v)
  expect_no_warning(tabtools:::.rt_tmb_vcov_full(f))
})
