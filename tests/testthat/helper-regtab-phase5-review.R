# Phase 5 review fixtures (IMPLEMENTATION_PLAN.md, "Phase 5 review
# outcome"): Stata expectations from qa/stata/make_regtab_phase5_review.do
# in tests/testthat/fixtures/regtab_phase5_review/, one csv() sink and one
# r() dump per case. Each case below is the R equivalent of the reviewer's
# Stata probe of the same id; test-regtab-phase5-review.R runs them.

p5r_path <- function(...) test_path("fixtures", "regtab_phase5_review", ...)

p5r_data <- function(name, factors = NULL) {
  d <- as.data.frame(haven::read_dta(p5r_path(paste0(name, ".dta"))))
  if (length(factors)) d <- tabtools::tt_as_factor(d, vars = factors)
  d
}

p5r_cells <- function(id) golden_read_cells_file(p5r_path(paste0(id, ".csv")))

p5r_stored <- function(id) {
  x <- utils::read.csv(p5r_path(paste0(id, "_stored.csv")), colClasses = "character",
                       na.strings = character(), encoding = "UTF-8")
  x[!x$name %in% c("xlsx", "sheet", "markdown", "csv", "markdown_rows", "markdown_cols"), , drop = FALSE]
}

# AER::tobit() also stores the transformed Surv formula, which its own
# model.frame() method reads once that namespace has been loaded.
p5r_tobit <- function(fit) {
  fit$formula <- stats::formula(fit)
  class(fit) <- c("tobit", class(fit))
  fit
}

# multinom fits below use reltol = 1e-14: at its default reltol multinom
# stops ~1e-4 from the maximum.

# The R call of each case (evaluated with the helpers above in scope).
p5r_cases <- list(
  # P0-1: nbreg/menbreg ancillary rows.
  C31 = quote(regtab(MASS::glm.nb(y ~ x1 + mid + high, data = golden_fixture("nbsim")), keepintercept = TRUE)),
  B20 = quote(regtab(glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), family = glmmTMB::nbinom2, data = p5r_data("cntg")),
                     keepintercept = TRUE)),
  G17 = quote(regtab(glmmTMB::glmmTMB(y ~ x1 + (1 | clinic), family = glmmTMB::nbinom2, data = p5r_data("cntg")),
                     keepintercept = TRUE, relabel = TRUE, stats = c("n", "ll", "aic"))),
  D07 = quote({
    d <- golden_fixture("zip")
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d, dist = "negbin"),
           MASS::glm.nb(event_count ~ treatment + age_z, data = d), keepintercept = TRUE, models = c("ZINB", "NB"))
  }),
  G18 = quote({
    d <- golden_fixture("zip")
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d, dist = "negbin"),
           MASS::glm.nb(event_count ~ treatment + age_z, data = d), keepintercept = TRUE, stats = c("n", "ll"),
           models = c("ZINB", "NB"))
  }),
  G19 = quote(regtab(MASS::glm.nb(y ~ x1 + mid + high, data = golden_fixture("nbsim")), keepintercept = TRUE,
                     stats = c("n", "ll", "aic"))),
  # P0-2: weighted multinom.
  B11 = quote(regtab(nnet::multinom(education ~ index_age + female, data = golden_fixture("cohort", "education"),
                            weights = fw, trace = FALSE, reltol = 1e-14, maxit = 2000), stats = c("n", "ll"))),
  # P0-3: single-equation models beside multi-equation models.
  B23 = quote({
    d <- golden_fixture("zip")
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d),
           glm(event_count ~ treatment + age_z, poisson, d), models = c("ZIP", "Poisson"))
  }),
  G03 = quote({
    d <- golden_fixture("zip")
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d),
           lm(event_count ~ treatment + age_z, d), keepintercept = TRUE, models = c("ZIP", "OLS"))
  }),
  G21 = quote({
    d <- golden_fixture("zip")
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d),
           glm(event_count ~ treatment + age_z, poisson, d), keepintercept = TRUE, stats = c("n", "ll"),
           models = c("ZIP", "Poisson"))
  }),
  G29 = quote({
    d <- golden_fixture("zip")
    d$female <- structure(factor(d$female), label = "Female")  # factor() drops the label; Stata keeps it (header row since 2.1.12)
    regtab(pscl::zeroinfl(event_count ~ treatment + female | zero_risk, data = d),
           glm(event_count ~ treatment + female, poisson, d), models = c("ZIP", "Poisson"))
  }),
  G02 = quote({
    d <- golden_fixture("zip")
    regtab(glm(event_count ~ treatment + age_z, poisson, d), pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d),
           models = c("Poisson", "ZIP"))
  }),
  G31 = quote({
    d <- golden_fixture("zip")
    d$female <- structure(factor(d$female), label = "Female")  # factor() drops the label; Stata keeps it (header row since 2.1.12)
    regtab(glm(event_count ~ treatment + female, poisson, d),
           pscl::zeroinfl(event_count ~ treatment + female | zero_risk, data = d), keepintercept = TRUE,
           models = c("Poisson", "ZIP"))
  }),
  G33 = quote({
    d <- golden_fixture("zip")
    regtab(p5r_tobit(survival::survreg(survival::Surv(event_count, event_count > 0, type = "left") ~ treatment + age_z,
                                       data = d, dist = "gaussian")),
           pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d), keepintercept = TRUE,
           models = c("Tobit", "ZIP"))
  }),
  G34 = quote({
    d <- golden_fixture("zip")
    regtab(glm(female ~ treatment + age_z, binomial, d), pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d),
           keepintercept = TRUE, models = c("Logit", "ZIP"))
  }),
  G35 = quote({
    d <- golden_fixture("zip")
    regtab(MASS::glm.nb(event_count ~ treatment + age_z, data = d),
           pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d, dist = "negbin"), keepintercept = TRUE,
           models = c("NB", "ZINB"))
  }),
  G32 = quote({
    d <- golden_fixture("zip")
    regtab(survival::survreg(survival::Surv(event_count, event_count > 0, type = "left") ~ treatment + age_z, data = d,
                             dist = "gaussian"),
           pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d), keepintercept = TRUE,
           models = c("Intreg", "ZIP"))
  }),
  B22 = quote({
    d <- golden_fixture("cohort", "education")
    regtab(MASS::polr(education ~ index_age + female, data = d, Hess = TRUE),
           nnet::multinom(education ~ index_age + female, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000), models = c("OL", "ML"))
  }),
  G04 = quote({
    d <- golden_fixture("cohort", "education")
    regtab(MASS::polr(education ~ index_age + female, data = d, Hess = TRUE),
           nnet::multinom(education ~ index_age + female, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000), keepintercept = TRUE, cutlabels = "A \\ B",
           models = c("OL", "ML"))
  }),
  G23 = quote({
    d <- golden_fixture("cohort", "education")
    regtab(survival::coxph(survival::Surv(follow_up, cv_event) ~ index_age + female, ties = "breslow", data = d),
           nnet::multinom(education ~ index_age + female, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000), models = c("Cox", "ML"))
  }),
  G24 = quote({
    d <- golden_fixture("cohort", "education")
    regtab(survival::survreg(survival::Surv(follow_up, cv_event) ~ index_age + female, data = d, dist = "weibull"),
           nnet::multinom(education ~ index_age + female, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000), keepintercept = TRUE, models = c("AFT", "ML"))
  }),
  # P0-4: mlogit with a base outcome that is not the lowest value; the
  # value codes relevel() drops are restored.
  D10 = quote({
    d <- golden_fixture("cohort", c("education", "smoking"))
    codes <- attr(d$education, "labels")
    d$education <- relevel(d$education, "Secondary")
    attr(d$education, "labels") <- codes
    regtab(nnet::multinom(education ~ index_age + smoking, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000), keepintercept = TRUE)
  }),
  G22 = quote({
    d <- golden_fixture("cohort", c("education", "smoking"))
    codes <- attr(d$education, "labels")
    d$education <- relevel(d$education, "Secondary")
    attr(d$education, "labels") <- codes
    regtab(nnet::multinom(education ~ index_age + smoking, data = d, trace = FALSE, reltol = 1e-14, maxit = 2000))
  }),
  # P0-5: mecloglog (compared except the fixed-effect estimate cells).
  B15b = quote(regtab(lme4::glmer(y ~ x + female + (1 | site), family = binomial("cloglog"), data = p5r_data("bslope2"),
                                  nAGQ = 7), stats = c("n", "icc", "ll"))),
  # P0-6: glmer nAGQ > 1 log-likelihood.
  F03 = quote(regtab(lme4::glmer(y ~ x1 + (1 | clinic), family = poisson, data = p5r_data("cntg"), nAGQ = 7),
                     stats = c("n", "ll", "aic", "bic"))),
  B17 = quote({
    d <- p5r_data("cntg")
    d$t <- 1 + seq_len(nrow(d)) %% 4
    regtab(lme4::glmer(y ~ x1 + offset(log(t)) + (1 | clinic), family = poisson, data = d, nAGQ = 7),
           stats = c("n", "ll"))
  }),
  B18 = quote({
    d <- p5r_data("cntg")
    d$n <- d$y + 1 + seq_len(nrow(d)) %% 5
    regtab(lme4::glmer(cbind(y, n - y) ~ x1 + (1 | clinic), family = binomial, data = d, nAGQ = 7),
           stats = c("n", "ll"))
  }),
  # P0-7: labels under subset =.
  B05 = quote(regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = golden_fixture("zip"),
                                    subset = female == 1))),
  B06 = quote(regtab(nnet::multinom(education ~ index_age + female, data = golden_fixture("cohort", "education"),
                            subset = treated == 1, trace = FALSE, reltol = 1e-14, maxit = 2000))),
  # P0-8: Gaussian survreg as intreg; AER::tobit as tobit.
  B04 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(p5r_tobit(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian")))
  }),
  B04b = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian"),
           keepintercept = TRUE)
  }),
  G11 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian"),
           stats = c("n", "ll", "aic", "bic"))
  }),
  G12 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(p5r_tobit(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian")),
           keepintercept = TRUE, stats = c("n", "ll", "aic", "bic"))
  }),
  G13 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian"),
           nointercept = TRUE)
  }),
  G14 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(p5r_tobit(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian")),
           relabel = TRUE)
  }),
  G15 = quote({
    d <- p5r_data("cntg")
    d$yc <- pmax(d$y - 2, 0)
    regtab(p5r_tobit(survival::survreg(survival::Surv(yc, yc > 0, type = "left") ~ x1, data = d, dist = "gaussian")),
           nointercept = TRUE)
  }),
  # P0-9: robust and clustered variance.
  B01 = quote(regtab(survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + female, cluster = region,
                                     ties = "breslow", data = golden_fixture("cohort3500")[1:400, ]))),
  B02 = quote(regtab(survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + female, robust = TRUE,
                                     ties = "breslow", data = golden_fixture("cohort3500")[1:400, ]))),
  B03 = quote(regtab(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + female, robust = TRUE,
                                       data = golden_fixture("cohort3500")[1:400, ], dist = "weibull"))),
  # P1-1: meprobit Laplace through glmmTMB.
  B16 = quote(regtab(glmmTMB::glmmTMB(y ~ x + female + (1 | site), family = binomial("probit"), data = p5r_data("bslope2")),
                     stats = c("n", "icc", "ll"))),
  # P1-4: multi-equation interactions (native, and fvgen).
  E01 = quote(regtab(nnet::multinom(education ~ female * index_age, data = golden_fixture("cohort", c("education", "female")), trace = FALSE, reltol = 1e-14, maxit = 2000),
                     interactions = "native")),
  E02 = quote(regtab(pscl::zeroinfl(event_count ~ treatment * age_z | zero_risk, data = golden_fixture("zip")),
                     interactions = "native")),
  G09 = quote(regtab(nnet::multinom(education ~ female * smoking, data = golden_fixture("cohort", c("education", "female", "smoking")), trace = FALSE, reltol = 1e-14, maxit = 2000),
                     interactions = "native")),
  G10 = quote({
    d <- golden_fixture("zip")
    d$female <- structure(factor(d$female), label = "Female")  # factor() drops the label; Stata keeps it (header row since 2.1.12)
    regtab(pscl::zeroinfl(event_count ~ female * treatment + age_z | zero_risk, data = d), interactions = "native")
  }),
  G27 = quote(regtab(nnet::multinom(education ~ female * index_age, data = golden_fixture("cohort", c("education", "female")), trace = FALSE, reltol = 1e-14, maxit = 2000))),
  G28 = quote({
    d <- golden_fixture("zip")
    d$female <- structure(factor(d$female), label = "Female")  # factor() drops the label; Stata keeps it (header row since 2.1.12)
    regtab(pscl::zeroinfl(event_count ~ female * treatment + age_z | zero_risk, data = d))
  }),
  # M65: duplicate group labels fall back to variable names.
  G36 = quote({
    d <- golden_fixture("nhanes2")
    d$zone <- ceiling(d$location / 10)
    attr(d$zone, "label") <- "Area"
    attr(d$location, "label") <- "Area"
    regtab(lme4::lmer(bmi ~ age + (1 | zone/location), data = d, REML = FALSE), relabel = TRUE,
           stats = c("n", "groups", "icc"))
  }),
  # M69: oprobit through clm.
  C09 = quote(regtab(ordinal::clm(education ~ index_age + female + diabetes, data = golden_fixture("cohort", "education"),
                                  link = "probit"), keepintercept = TRUE, stats = c("n", "ll", "r2"))),
  # P2-1 / M78: frequency weights.
  B10 = quote(regtab(MASS::polr(education ~ index_age + female, data = golden_fixture("cohort", "education"),
                                weights = fw, Hess = TRUE), stats = c("n", "ll"))),
  B12 = quote({
    d <- golden_fixture("zip")
    d$w <- 1 + seq_len(nrow(d)) %% 3
    regtab(pscl::zeroinfl(event_count ~ treatment + age_z | zero_risk, data = d, weights = w), stats = c("n", "ll"))
  }),
  B13 = quote(regtab(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + female, weights = fw,
                                       subset = fw > 0, data = golden_fixture("cohort"), dist = "weibull"),
                     stats = c("n", "ll")))
)

p5r_run <- function(id) {
  env <- new.env(parent = environment(p5r_run))
  suppressMessages(eval(p5r_cases[[id]], envir = env))
}

# Largest stored difference, as golden_compare_stored() measures it
# (absolute below 1, relative above), over the fields both sides have.
p5r_stored_maxdiff <- function(tt, id, drop_rows = character()) {
  got <- golden_flatten_stored(tt$stored)
  want <- p5r_stored(id)
  want <- want[want$kind %in% c("scalar", "matrix") & !want$row %in% drop_rows, , drop = FALSE]
  k <- function(d) paste(d$name, d$kind, d$row, d$col)
  hit <- match(k(want), k(got))
  g <- golden_stored_num(got$value[hit])
  w <- golden_stored_num(want$value)
  ok <- !is.na(g) & !is.na(w)
  if (!any(ok)) return(0)
  max(abs(g[ok] - w[ok]) / pmax(1, abs(w[ok])))
}
