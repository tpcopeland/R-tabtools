# Milestone 5w review fixtures (IMPLEMENTATION_PLAN.md, "Milestone 5w
# review outcome"): Stata expectations from qa/stata/make_regtab_5w_review.do
# in tests/testthat/fixtures/regtab_5w_review/, one csv() sink and one r()
# dump per case, on the reviewer's synthetic data (syn.dta, synpp.dta;
# qa/make_regtab_5w_review_data.R). Each case below is the R equivalent of
# the Stata probe of the same id; test-regtab-5w-review.R runs them.

w5r_path <- function(...) test_path("fixtures", "regtab_5w_review", ...)

w5r_read <- function(name) {
  d <- as.data.frame(haven::zap_formats(haven::read_dta(w5r_path(paste0(name, ".dta")))))
  d[] <- lapply(d, function(x) {
    attr(x, "label") <- NULL
    as.vector(x)
  })
  d
}

w5r_syn <- function() w5r_read("syn")

w5r_pp <- function(period_factor = TRUE) {
  d <- w5r_read("synpp")
  if (period_factor) d$period <- factor(d$period)
  d
}

w5r_cells <- function(id) golden_read_cells_file(w5r_path(paste0(id, ".csv")))

w5r_stored <- function(id) {
  x <- utils::read.csv(w5r_path(paste0(id, "_stored.csv")), colClasses = "character",
                       na.strings = character(), encoding = "UTF-8")
  x[!x$name %in% c("xlsx", "sheet", "markdown", "csv", "markdown_rows", "markdown_cols"), , drop = FALSE]
}

# Tight convergence, so R's maximum is Stata's to display precision.
w5r_ctl <- glm.control(epsilon = 1e-14, maxit = 200)
w5r_gee_ctl <- function() geepack::geese.control(epsilon = 1e-12, maxit = 100)
w5r_S <- c("n", "ll", "aic", "bic", "r2")

# Stata's svyset singleunit(certainty).
w5r_certainty <- function(expr) {
  op <- options(survey.lonely.psu = "certainty")
  on.exit(options(op))
  expr
}

w5r_svy <- function(d) {
  survey::svydesign(ids = ~psu, strata = ~strata, weights = ~iptw, data = d)
}

# The R call of each case (evaluated with the helpers above in scope).
w5r_cases <- list(
  # P0-4: pseudo-log-likelihood statistics under [pweight].
  D01 = quote(regtab(glm(y ~ treat + x1 + x2, quasibinomial, w5r_syn(), weights = iptw, control = w5r_ctl),
                     vce = "robust", stats = w5r_S)),
  D02 = quote(regtab(lm(yc ~ treat + x1 + x2, w5r_syn(), weights = iptw), vce = "robust", stats = w5r_S)),
  D03 = quote(regtab(glm(cnt ~ treat + x1 + offset(log(expo)), quasipoisson, w5r_syn(), weights = iptw,
                         control = w5r_ctl), vce = "robust", stats = w5r_S)),
  D05 = quote(regtab(survival::coxph(survival::Surv(time, event) ~ treat + x1 + x2, w5r_syn(), weights = iptw,
                                     id = id, ties = "breslow"), stats = c("n", "ll", "aic", "bic"))),
  D05b = quote(regtab(survival::coxph(survival::Surv(time, event) ~ treat + x1 + x2, w5r_syn(), weights = iptw,
                                      ties = "breslow"), stats = c("n", "ll"))),
  D09 = quote(regtab(glm(y ~ treat + x1 + x2, quasibinomial, w5r_syn(), weights = wc, control = w5r_ctl),
                     vce = "robust", stats = w5r_S)),
  D10 = quote(regtab(glm(y ~ treat + x1 + x2, binomial, w5r_syn(), weights = iw, control = w5r_ctl),
                     vce = "robust", stats = w5r_S)),
  D20 = quote({
    d <- w5r_syn()
    g0 <- glm(y ~ treat + x1 + x2, binomial, d, control = w5r_ctl)
    g1 <- glm(y ~ treat + x1 + x2, quasibinomial, d, weights = iptw, control = w5r_ctl)
    regtab(g0, g1, g1, vce = list("stata", "robust", "cluster"), cluster = list(NULL, NULL, ~clusu),
           models = c("A", "B", "C"), stats = w5r_S)
  }),
  D21 = quote(regtab(glm(y ~ treat + x1 + x2, quasibinomial(link = "probit"), w5r_syn(), weights = iptw,
                         control = w5r_ctl), vce = "robust", stats = w5r_S)),
  E08 = quote(regtab(glm(cnt ~ treat + x1 + offset(log(expo)), poisson, w5r_syn(), control = w5r_ctl),
                     vce = "robust", stats = w5r_S)),
  E09 = quote({
    d <- w5r_syn()
    regtab(glm(y ~ treat + x1 + x2, quasibinomial, d, weights = iptw, control = w5r_ctl),
           lm(yc ~ treat + x1 + x2, d, weights = iptw), vce = "robust", stats = w5r_S)
  }),
  N05 = quote(regtab(glm(yc ~ treat + x1, gaussian, w5r_syn(), weights = iptw), vce = "robust", stats = w5r_S)),
  N06 = quote(regtab(glm(ypos ~ treat + x1, Gamma("log"), w5r_syn(), weights = iptw, control = w5r_ctl),
                     vce = "robust", stats = c("n", "ll", "aic", "bic"))),
  D14 = quote(regtab(suppressWarnings(geepack::geeglm(ev ~ a + z + period, binomial, w5r_pp(), id = id, weights = sw,
                                                      corstr = "independence", control = w5r_gee_ctl())),
                     gee_as = "glm", drop = "period", stats = c("n", "ll", "aic", "bic"))),
  # P0-5: svy: R-squared and the subpopulation.
  D06 = quote(w5r_certainty(regtab(survey::svyglm(yc ~ treat + x1 + x2, w5r_svy(w5r_syn())), stats = w5r_S))),
  # (svyglm evaluates `control` in its own frame: spelled out.)
  D07 = quote(w5r_certainty(regtab(survey::svyglm(y ~ treat + x1 + x2, w5r_svy(w5r_syn()), family = quasibinomial(),
                                                  control = glm.control(epsilon = 1e-14, maxit = 200)),
                                   stats = w5r_S))),
  D11 = quote(w5r_certainty({
    des <- w5r_svy(w5r_syn())
    regtab(survey::svyglm(y ~ treat + x1, subset(des, x2 == 1), family = quasibinomial(),
                          control = glm.control(epsilon = 1e-14, maxit = 200)),
           stats = "n")
  })),
  # P0-6: few clusters (AIC/BIC compared separately).
  D08 = quote(regtab(glm(y ~ treat + x1 + x2, binomial, w5r_syn(), control = w5r_ctl), vce = "cluster",
                     cluster = ~clus3, stats = w5r_S)),
  E01 = quote(regtab(glm(y ~ treat + x1 + x2, binomial, w5r_syn(), control = w5r_ctl), vce = "cluster",
                     cluster = ~clus2, stats = w5r_S)),
  E02 = quote(regtab(lm(yc ~ treat + x1 + x2, w5r_syn()), vce = "cluster", cluster = ~clus3, stats = w5r_S)),
  E03 = quote(regtab(survival::coxph(survival::Surv(time, event) ~ treat + x1 + x2, w5r_syn(), id = id,
                                     ties = "breslow"), vce = "cluster", cluster = ~clus3,
                     stats = c("n", "ll", "aic", "bic"))),
  E07 = quote(regtab(glm(y ~ treat + x1 + x2, binomial(link = "probit"), w5r_syn(), control = w5r_ctl),
                     vce = "cluster", cluster = ~clus3, stats = c("n", "ll", "aic", "bic"))),
  # P0-2: weighted fits of other classes.
  D13 = quote(regtab(suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, w5r_pp(), id = id, weights = wid,
                                                      corstr = "independence", control = w5r_gee_ctl())),
                     stats = c("n", "groups"))),
  E06 = quote(regtab(suppressWarnings(geepack::geeglm(ev ~ a + z, binomial, w5r_pp(), id = id, weights = wid,
                                                      corstr = "independence", control = w5r_gee_ctl())),
                     vce = "robust", stats = c("n", "groups"))),
  D16 = quote(regtab(MASS::glm.nb(cnt ~ treat + x1 + offset(log(expo)), w5r_syn(), weights = iptw,
                                  control = glm.control(epsilon = 1e-12, maxit = 100)),
                     vce = "robust", stats = w5r_S)),
  D16k = quote(regtab(MASS::glm.nb(cnt ~ treat + x1 + offset(log(expo)), w5r_syn(), weights = iptw,
                                   control = glm.control(epsilon = 1e-12, maxit = 100)),
                      vce = "robust", keepintercept = TRUE)),
  N07 = quote(regtab(survival::coxph(survival::Surv(time, event) ~ treat + x1 + x2, w5r_syn(), weights = iw,
                                     id = id, ties = "breslow"), vce = "robust", stats = c("n", "ll", "aic", "bic"))),
  # P1-2: zero weights, empty clusters, several records per subject.
  D12 = quote(regtab(glm(y ~ treat + x1 + x2, quasibinomial, w5r_syn(), weights = wz, control = w5r_ctl),
                     vce = "cluster", cluster = ~clusz, stats = "n")),
  N03 = quote(regtab(lm(yc ~ treat + x1 + x2, w5r_syn(), weights = wz), vce = "cluster", cluster = ~clusz,
                     stats = "n")),
  N04 = quote(regtab(suppressWarnings(geepack::geeglm(ev ~ a + z + period, binomial, w5r_pp(), id = id, weights = wz0,
                                                      corstr = "independence", control = w5r_gee_ctl())),
                     gee_as = "glm", drop = "period", stats = "n")),
  N01 = quote(regtab(survival::coxph(survival::Surv(t0, t1, ev) ~ a + z, w5r_pp(FALSE), weights = wid, id = id,
                                     ties = "breslow"), stats = c("n", "ll", "aic", "bic"))),
  N02 = quote(regtab(survival::coxph(survival::Surv(t0, t1, ev) ~ a + z, w5r_pp(FALSE), id = id, ties = "breslow"),
                     vce = "robust", stats = c("n", "ll"))),
  D19 = quote({
    d <- w5r_syn()
    d$f4 <- factor(d$f4)
    regtab(glm(y ~ treat + x1 + f4, quasibinomial, d, weights = iptw, subset = x2 == 1, control = w5r_ctl),
           vce = "robust", stats = "n")
  }),
  D19b = quote({
    d <- w5r_syn()
    d$f4 <- factor(d$f4)
    regtab(lm(yc ~ treat + x1 + f4, d, weights = iptw, subset = x2 == 1), vce = "robust", stats = "n")
  }),
  # P1-3: stcox's default ties are Breslow's.
  E05 = quote(regtab(survival::coxph(survival::Surv(time, event) ~ treat + x1 + x2, w5r_syn(), id = id,
                                     ties = "breslow"), stats = "n"))
)

w5r_run <- function(id) {
  env <- new.env(parent = environment(w5r_run))
  suppressMessages(eval(w5r_cases[[id]], envir = env))
}

# Largest stored difference, as golden_compare_stored() measures it
# (absolute below 1, relative above), over the fields both sides have.
w5r_stored_maxdiff <- function(tt, id, drop_names = character()) {
  got <- golden_flatten_stored(tt$stored)
  want <- w5r_stored(id)
  want <- want[want$kind %in% c("scalar", "matrix") & !want$name %in% drop_names, , drop = FALSE]
  k <- function(d) paste(d$name, d$kind, d$row, d$col)
  hit <- match(k(want), k(got))
  g <- golden_stored_num(got$value[hit])
  w <- golden_stored_num(want$value)
  ok <- !is.na(g) & !is.na(w)
  if (!any(ok)) return(0)
  max(abs(g[ok] - w[ok]) / pmax(1, abs(w[ok])))
}
