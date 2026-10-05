# Milestone H task H6 (external review F07, F34): the ci_method capability
# tt_ci_methods() and stored$ci_method; for each class, profile intervals
# are honoured or refused by name.

test_that("tt_ci_methods(): profile for lm, glm and glm.nb only; refused by name everywhere else (H6)", {
  expect_identical(tt_ci_methods(lm(mpg ~ wt, mtcars)), c("wald", "profile"))
  g <- glm(am ~ wt, binomial, mtcars)
  expect_identical(tt_ci_methods(g), c("wald", "profile"))
  expect_identical(tt_ci_methods(data.frame(term = "x", estimate = 1, std.error = 0.5)), "wald")
  tt <- regtab(g, ci_method = "profile")
  expect_identical(tt$stored$ci_method, "profile")
  expect_identical(regtab(g)$stored$ci_method, "wald")
  expect_equal(tt$meta$regtab_rows$conf.low[1], exp(suppressMessages(confint(g))[2, 1]), tolerance = 1e-8)
  refuse <- function(fit, cls) {
    expect_identical(tt_ci_methods(fit), "wald", label = cls)
    expect_error(suppressWarnings(regtab(fit, ci_method = "profile")), paste0("not available for <", cls, ">"),
                 fixed = TRUE, label = cls)
  }
  refuse(data.frame(term = "x", estimate = 1, std.error = 0.5), "data.frame")
  if (requireNamespace("MASS", quietly = TRUE)) {
    expect_identical(tt_ci_methods(MASS::glm.nb(Days ~ Age, MASS::quine)), c("wald", "profile"))
    refuse(MASS::polr(factor(gear) ~ wt, mtcars, Hess = TRUE), "polr")
  }
  if (requireNamespace("survival", quietly = TRUE)) {
    lung <- survival::lung
    refuse(survival::coxph(survival::Surv(time, status) ~ age, lung), "coxph")
    refuse(survival::coxph(survival::Surv(time, status) ~ age, lung, cluster = inst), "coxph")
    refuse(survival::survreg(survival::Surv(time, status) ~ age, lung), "survreg")
  }
  if (requireNamespace("nnet", quietly = TRUE)) {
    refuse(nnet::multinom(factor(gear) ~ wt, mtcars, trace = FALSE), "multinom")
  }
  if (requireNamespace("pscl", quietly = TRUE)) {
    set.seed(2)
    dz <- data.frame(x = rnorm(200))
    dz$y <- ifelse(runif(200) < 0.3, 0, rpois(200, exp(0.5 + 0.3 * dz$x)))
    refuse(pscl::zeroinfl(y ~ x | 1, dz), "zeroinfl")
    refuse(pscl::hurdle(y ~ x | 1, dz), "hurdle")
  }
  if (requireNamespace("cmprsk", quietly = TRUE)) {
    set.seed(3)
    cr <- cmprsk::crr(rexp(100), sample(0:2, 100, TRUE), cbind(x = rnorm(100)))
    refuse(cr, "crr")
  }
  if (requireNamespace("geepack", quietly = TRUE)) {
    dg <- data.frame(id = rep(1:20, each = 3), x = rnorm(60), y = rnorm(60))
    refuse(geepack::geeglm(y ~ x, gaussian, dg, id = id), "geeglm")
  }
  if (requireNamespace("ordinal", quietly = TRUE)) {
    refuse(ordinal::clm(factor(gear) ~ wt, data = mtcars), "clm")
  }
  if (requireNamespace("lme4", quietly = TRUE)) {
    refuse(suppressMessages(lme4::lmer(mpg ~ wt + (1 | cyl), mtcars)), "lmerMod")
    refuse(suppressMessages(suppressWarnings(lme4::glmer(am ~ wt + (1 | cyl), mtcars, family = binomial))), "glmerMod")
  }
  if (requireNamespace("glmmTMB", quietly = TRUE)) {
    refuse(glmmTMB::glmmTMB(mpg ~ wt + (1 | cyl), mtcars), "glmmTMB")
  }
  if (requireNamespace("survey", quietly = TRUE)) {
    des <- survey::svydesign(ids = ~1, weights = ~1, data = mtcars)
    refuse(survey::svyglm(am ~ wt, des, family = quasibinomial()), "svyglm")
  }
  if (requireNamespace("WeightIt", quietly = TRUE)) {
    w <- WeightIt::glm_weightit(am ~ wt, data = mtcars, family = binomial)
    refuse(w, "glm_weightit")
  }
  # A robust or cluster variance is refused with profile (F34, 5w review).
  expect_error(regtab(g, vce = "robust", ci_method = "profile"), "cannot be combined")
})
