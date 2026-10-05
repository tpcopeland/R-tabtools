# Model scale and metadata detection (plan task 4.3), checked against the
# stored coef_label and methods of every regtab golden by fitting each
# scenario's R models.

# Evaluate a scenario's r_call with regtab() replaced by a recorder that
# returns the fitted models and the options that matter here.
capture_regtab_call <- function(r_call) {
  # Helpers (golden_fixture) live in the test environment, not globalenv,
  # under R CMD check.
  env <- new.env(parent = environment(golden_fixture))
  env$regtab <- function(..., coef = NULL, cdisc = FALSE, nointercept = NULL,
                         keepintercept = FALSE, level = 0.95, stars = FALSE,
                         starslevels = NULL, gee_as = "xtgee") {
    models <- list(...)
    nm <- names(models) %||% rep("", length(models))
    models <- models[!nzchar(nm)]
    # geeglm fits under gee_as = "glm" (W11) classify as glm.
    models <- lapply(models, function(f) if (inherits(f, "geeglm")) tabtools:::.rt_gee_tag(f, gee_as) else f)
    list(models = models, coef = coef, cdisc = cdisc, nointercept = nointercept,
         # regtab() takes level as a proportion or a percentage (MI08: 90).
         keepintercept = keepintercept, level = if (level > 1) level / 100 else level, stars = stars,
         starslevels = starslevels)
  }
  eval(parse(text = r_call), envir = env)
}

regtab_golden_ids <- function() {
  sc <- golden_scenarios()
  sc <- sc[sc$command == "regtab" & !grepl("golden_tidy\\(", sc$r_call), ]
  sc$id
}

skip_if_models_missing <- function() {
  for (p in c("survival", "MASS", "nnet", "pscl", "lme4", "geepack", "haven", "survey")) skip_if_not_installed(p)
}

test_that("coef_label and methods match every regtab golden", {
  skip_on_cran()
  skip_if_models_missing()
  for (id in regtab_golden_ids()) {
    sc <- golden_scenario(id)
    cap <- suppressWarnings(capture_regtab_call(sc$r_call))
    infos <- lapply(cap$models, tabtools:::tt_model_info)
    s <- tabtools:::tt_models_scale(infos, coef = cap$coef, cdisc = cap$cdisc,
                                    nointercept = cap$nointercept, keepintercept = cap$keepintercept)
    st <- golden_read_stored(id)
    expect_identical(s$coef_label, st$value[st$name == "coef_label"], label = paste(id, "coef_label"))
    # The methods sentence comes from the models (task H11); goldens whose
    # Stata sentence names the wrong model go through the documented
    # transform (helper-golden.R, golden_methods_regtab()).
    methods <- tabtools:::tt_regtab_methods(infos, vapply(cap$models, tabtools:::.rt_n_predictors, 0),
                                            cap$level, cap$stars, cap$starslevels)
    st <- golden_methods_regtab(st, id)
    want <- sub(" Analysis performed in Stata.*$", "", st$value[st$name == "methods"])
    expect_identical(methods, want, label = paste(id, "methods"))
  }
})

test_that("the automatic nointercept decision matches the goldens' intercept rows", {
  skip_on_cran()
  skip_if_models_missing()
  checked <- 0L
  for (id in regtab_golden_ids()) {
    sc <- golden_scenario(id)
    cap <- suppressWarnings(capture_regtab_call(sc$r_call))
    infos <- lapply(cap$models, tabtools:::tt_model_info)
    s <- tabtools:::tt_models_scale(infos, coef = cap$coef, cdisc = cap$cdisc,
                                    nointercept = cap$nointercept, keepintercept = cap$keepintercept)
    cells <- golden_read_cells(id)
    has_icpt <- any(grepl("(^|: )Intercept$", cells[, 1]))
    # A kept ologit shows its cutpoints (relabelled by cutlabels), not an
    # intercept row, so only intercept-bearing models are checked for rows.
    bearing <- any(vapply(infos, function(i) length(i$intercept_terms) > 0L, TRUE))
    # keep() filters rows itself (R09a/b, R10b, R25k), hiding the intercept.
    if (bearing && !grepl("keep = ", sc$r_call)) {
      checked <- checked + 1L
      expect_identical(has_icpt, !s$nointercept, label = id)
    }
  }
  expect_gt(checked, 40L)
})

test_that("scale detection per class mirrors regtab.ado", {
  skip_if_models_missing()
  d <- golden_fixture("cohort3500", factors = "education")
  info <- function(fit) tabtools:::tt_model_info(fit)
  i <- info(lm(crp ~ index_age, d))
  expect_identical(c(i$stata_cmd, i$effect_scale), c("regress", "Coef."))
  expect_false(i$exponentiate)
  expect_false(i$auto_nointercept)
  expect_identical(i$intercept_terms, "(Intercept)")
  expect_identical(i$outcome_id, "crp")

  i <- info(glm(treated ~ index_age, binomial, d))
  expect_identical(c(i$effect_scale, i$null_value), c("OR", "1"))
  expect_true(i$exponentiate && i$auto_nointercept)
  i <- info(glm(treated ~ index_age, binomial(link = "probit"), d))
  expect_identical(i$effect_scale, "Coef.")
  expect_false(i$auto_nointercept)
  expect_identical(info(glm(prior_hosp ~ index_age, poisson, d))$effect_scale, "IRR")
  expect_identical(info(glm(prior_hosp ~ index_age, quasipoisson, d))$effect_scale, "IRR")
  expect_identical(info(glm(crp ~ index_age, Gamma(link = "log"), d))$effect_scale, "Coef.")
  expect_identical(info(suppressWarnings(MASS::glm.nb(prior_hosp ~ index_age, d)))$effect_scale, "IRR")

  i <- info(survival::coxph(survival::Surv(follow_up, cv_event) ~ index_age, d))
  expect_identical(c(i$stata_cmd, i$effect_scale), c("stcox", "HR"))
  expect_identical(i$outcome_id, "cv_event")
  i <- info(survival::survreg(survival::Surv(follow_up, cv_event) ~ index_age, d))
  expect_identical(i$effect_scale, "TR")
  # Time ratios are exp(b) since tabtools 2.1.10 (2.1.9: log-time b).
  expect_true(i$exponentiate)
  expect_identical(i$null_value, 1)
  expect_identical(i$ancillary_terms, "Log(scale)")

  i <- info(nnet::multinom(education ~ index_age, d, trace = FALSE))
  expect_identical(i$effect_scale, "RRR")
  expect_identical(i$equations, c("Secondary", "Tertiary"))
  i <- info(MASS::polr(education ~ index_age, d))
  expect_identical(i$effect_scale, "OR")
  expect_identical(i$cutpoint_terms, c("Primary|Secondary", "Secondary|Tertiary"))
  expect_identical(info(MASS::polr(education ~ index_age, d, method = "probit"))$effect_scale, "Coef.")
  skip_if_not_installed("ordinal")
  i <- info(ordinal::clm(education ~ index_age, data = d))
  expect_identical(i$effect_scale, "OR")
  expect_length(i$cutpoint_terms, 2L)
})

test_that("Fine-Gray coxph fits are SHR", {
  skip_if_models_missing()
  d <- golden_fixture("cohort3500")
  d$event_type <- factor(d$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, event_type) ~ ., data = d, etype = "cv")
  fit <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated, weights = fgwt, data = fg)
  i <- tabtools:::tt_model_info(fit)
  expect_identical(c(i$stata_cmd, i$effect_scale), c("stcrreg", "SHR"))
  expect_true(i$exponentiate)
})

test_that("mixed models carry random-effects family and latent ICC variance", {
  skip_if_models_missing()
  d <- golden_fixture("mixed_bp")
  i <- tabtools:::tt_model_info(lme4::lmer(y ~ age + (1 | region), d, REML = FALSE))
  expect_identical(c(i$stata_cmd, i$effect_scale, i$re_family), c("mixed", "Coef.", "variance"))
  expect_false(i$auto_nointercept)
  n <- golden_fixture("nhanes2")
  n <- n[seq(1, nrow(n), by = 5), ]
  g <- suppressWarnings(lme4::glmer(highbp ~ age + (1 | location), family = binomial, data = n))
  i <- tabtools:::tt_model_info(g)
  expect_identical(c(i$stata_cmd, i$effect_scale, i$re_family), c("melogit", "OR", "mor"))
  expect_equal(i$icc_resid, pi^2 / 3)
  g <- suppressWarnings(lme4::glmer(highbp ~ age + (1 | location), family = binomial(link = "cloglog"), data = n))
  i <- tabtools:::tt_model_info(g)
  expect_identical(c(i$stata_cmd, i$effect_scale, i$re_family), c("mecloglog", "HR", "mhr"))
  skip_if_not_installed("glmmTMB")
  t <- glmmTMB::glmmTMB(highbp ~ age + (1 | location), family = binomial, data = n)
  expect_identical(tabtools:::tt_model_info(t)$effect_scale, "OR")
  t <- glmmTMB::glmmTMB(highbp ~ age, family = binomial, data = n)
  i <- tabtools:::tt_model_info(t)
  expect_identical(c(i$stata_cmd, i$re_family), c("logit", "none"))
})

test_that("GEE follows glm's family/link rule (tabtools 2.1.12; golden R23)", {
  skip_if_models_missing()
  u <- golden_fixture("union")
  u <- u[u$idcode <= 300, ]
  i <- tabtools:::tt_model_info(geepack::geeglm(union ~ age, id = idcode, family = binomial, data = u))
  expect_identical(c(i$stata_cmd, i$effect_scale, i$family), c("xtgee", "OR", "binomial"))
  expect_true(i$exponentiate)
  expect_true(i$auto_nointercept)
  expect_true(i$is_gee)
  i <- tabtools:::tt_model_info(geepack::geeglm(grade ~ age, id = idcode, family = poisson, data = u))
  expect_identical(c(i$effect_scale, i$exponentiate), c("IRR", "TRUE"))
  i <- tabtools:::tt_model_info(geepack::geeglm(union ~ age, id = idcode, family = binomial("probit"), data = u))
  expect_identical(c(i$stata_cmd, i$effect_scale), c("xtgee", "Coef."))
  expect_false(i$exponentiate)
  expect_false(i$auto_nointercept)
})

test_that("zero-inflated and hurdle models are multi-equation Coef.", {
  skip_if_models_missing()
  z <- golden_fixture("zip")
  i <- tabtools:::tt_model_info(pscl::zeroinfl(event_count ~ treatment | zero_risk, data = z, dist = "negbin"))
  expect_identical(c(i$stata_cmd, i$effect_scale), c("zinb", "Coef."))
  expect_true(i$auto_nointercept)
  expect_identical(i$intercept_terms, c("count_(Intercept)", "zero_(Intercept)"))
  expect_identical(i$ancillary_terms, "Log(theta)")
  expect_identical(tabtools:::tt_model_info(pscl::hurdle(event_count ~ treatment, data = z))$stata_cmd, "churdle")
})

test_that("headers across models: shared, mixed, user coef, cdisc, and auto nointercept", {
  ratio <- list(effect_scale = "OR", auto_nointercept = TRUE)
  lin <- list(effect_scale = "Coef.", auto_nointercept = FALSE)
  s <- tabtools:::tt_models_scale(list(ratio, ratio))
  expect_identical(c(s$coef, s$coef_label), c("OR", "OR"))
  expect_true(s$nointercept)
  s <- tabtools:::tt_models_scale(list(lin, ratio))
  expect_identical(s$headers, c("Coef.", "OR"))
  expect_identical(c(s$coef, s$coef_label), c("Estimate", "mixed"))
  expect_true(s$mixed)
  expect_false(s$nointercept)
  s <- tabtools:::tt_models_scale(list(lin, ratio), coef = "Beta")
  expect_identical(c(s$headers, s$coef_label), c("Beta", "Beta", "Beta"))
  s <- tabtools:::tt_models_scale(list(ratio), cdisc = TRUE)
  expect_identical(c(s$coef, s$coef_label), c("Estimate", "Estimate"))
  expect_false(tabtools:::tt_models_scale(list(ratio), keepintercept = TRUE)$nointercept)
  expect_true(tabtools:::tt_models_scale(list(lin), nointercept = TRUE)$nointercept)
  expect_false(tabtools:::tt_models_scale(list(ratio), nointercept = FALSE)$nointercept)
})

test_that("nointercept row rule ports regtab.ado:1619-1632", {
  drop <- c("Intercept", "_cons", "Constant", "Inflation equation: _cons", "/cut1", "cut2",
            "Event count: cut3", "/lnalpha", "alpha", "lnalpha", "ln_p", "p", "1/p",
            "Selection equation: ln_p", "ancillary: x", "Scale: sigma", "  INTERCEPT  ")
  # After an equation prefix Stata drops only a bare "/" or ancillary name:
  # ": +(/|alpha|...)$" does not match "eq: /ln_p", so that row survives.
  keep <- c("Age", "Intercept term", "cut", "cutoff1", "Price", "alpha level", "Scale",
            "Selection equation: /ln_p")
  expect_true(all(tabtools:::tt_noint_row(drop)))
  expect_false(any(tabtools:::tt_noint_row(keep)))
})

test_that("methods sentences: phrases, levels, stars formatting", {
  m <- tabtools:::tt_regtab_methods
  info <- function(cmd, scale = "Coef.", family = NA_character_, link = NA_character_) {
    list(stata_cmd = cmd, effect_scale = scale, family = family, link = link)
  }
  cox <- info("stcox", "HR")
  expect_identical(m(list(cox)), "Hazard ratios with 95% confidence intervals from multivariable Cox proportional hazards regression.")
  lg <- info("logit", "OR", "binomial", "logit")
  expect_identical(m(list(lg, lg, lg), c(2, 3, 2), 0.9),
                   "Odds ratios with 90% confidence intervals from multivariable logistic regression across 3 models.")
  # Stata's 2.1.10+ wording for TR, and the other HR models.
  expect_identical(m(list(info("streg", "TR"))),
                   "Time ratios with 95% confidence intervals from multivariable accelerated failure-time survival regression.")
  expect_identical(m(list(info("mecloglog", "HR", "binomial", "cloglog"))),
                   "Hazard ratios with 95% confidence intervals from multivariable mixed-effects complementary log-log regression.")
  expect_identical(m(list(info("streg", "HR"))),
                   "Hazard ratios with 95% confidence intervals from multivariable parametric proportional hazards survival regression.")
  expect_identical(m(list(info("stcrreg", "SHR"))),
                   "Subhazard ratios with 95% confidence intervals from multivariable Fine-Gray competing-risks regression.")
  expect_identical(m(list(lg, info("regress")), c(2, 2)),
                   "Collected regression estimates with 95% confidence intervals across 2 models.")
  expect_match(m(list(lg), stars = TRUE), "\\* p<0.05, \\*\\* p<0.01, \\*\\*\\* p<0.001\\.$")
  expect_match(m(list(lg), stars = TRUE, starslevels = c(0.1, 0.05, 0.01)), "\\* p<.1, \\*\\* p<.05, \\*\\*\\* p<.01\\.$")
  # Univariable, multivariable, both, and an intercept-only model.
  expect_match(m(list(lg), 1), "from univariable logistic regression\\.$")
  expect_match(m(list(lg, lg), c(1, 3)), "from univariable and multivariable logistic regression across 2 models\\.$")
  expect_match(m(list(info("regress")), 0), "from linear regression\\.$")
})
