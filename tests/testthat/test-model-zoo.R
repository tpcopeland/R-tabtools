# Milestone H task H23 (interop research T1, T2).
#
# T1, the model zoo: one small fit per common model class (the classes of
# qa/data/coverage_matrix.csv that are installed). Each is
# either supported, with its coefficient on the right row at the right
# scale, or refused by name with a hint; never tabled through a default
# path. An entry whose package is not installed is skipped.
#
# T2, the vce sweep: for every class and every vce value, a supported value
# gives intervals and p-values that are the Wald statistics of that vce's
# standard error (tt_vcov()) and reference distribution (tt_wald_df()), so
# a variance that changes the SE but not the CI (external review F34) is
# caught; an unsupported value is refused by name.
#
# Packages that are not in Suggests are reached through getExportedValue()
# with the name in a variable, so R CMD check does not take them for
# undeclared test dependencies.

zoo_data <- function() {
  set.seed(20260926)
  n <- 240
  d <- data.frame(id = rep(1:60, each = 4), x = rnorm(n), z = rnorm(n),
                  f = factor(sample(c("A", "B", "C"), n, TRUE)), g = factor(rep(1:12, each = 20)))
  re <- rep(rnorm(12, sd = 0.5), each = 20)
  d$yb <- rbinom(n, 1, plogis(-0.3 + 0.5 * d$x + 0.3 * d$z + re))
  d$yc <- 2 + d$x + 0.5 * d$z + re + rnorm(n)
  d$ypos <- rgamma(n, shape = 3, rate = 3 / exp(0.5 + 0.2 * d$x))
  d$ycnt <- rpois(n, exp(0.3 + 0.3 * d$x + 0.2 * d$z))
  d$ynb <- rnbinom(n, mu = exp(0.5 + 0.3 * d$x), size = 1.2)
  d$yzi <- ifelse(runif(n) < 0.3, 0L, d$ycnt)
  d$time <- rexp(n, exp(0.3 * d$x))
  d$status <- rbinom(n, 1, 0.75)
  d$cause <- factor(ifelse(d$status == 1, sample(1:2, n, TRUE, prob = c(0.7, 0.3)), 0), 0:2,
                    c("censor", "event", "compete"))
  d$ord <- cut(d$yc, stats::quantile(d$yc, 0:3 / 3), include.lowest = TRUE, labels = c("lo", "mid", "hi"))
  d$y2 <- d$yc + rnorm(n)
  d$w <- runif(n, 0.5, 2)
  d$treat <- rbinom(n, 1, plogis(0.4 * d$x))
  d
}

# A function of a package that may not be in Suggests.
zoo_fn <- function(pkg, fn) getExportedValue(pkg, fn)

# The raw coefficients, named as tt_vcov() names them.
zoo_coef <- function(fit) {
  b <- tryCatch(tabtools:::tt_coef(fit), error = function(e) NULL)
  if (is.numeric(b) && !is.null(names(b)) && is.null(dim(b))) return(b)
  if (inherits(fit, "multinom")) {
    cf <- stats::coef(fit)
    return(stats::setNames(as.vector(t(cf)), paste0(rep(rownames(cf), each = ncol(cf)), ":", colnames(cf))))
  }
  if (inherits(fit, "crr")) return(fit$coef)
  stats::coef(fit)
}

# The coefficient name behind a row key: "x", "mid::x" (multinom
# "mid:x"), "<depvar>::x" (zeroinfl/hurdle "count_x"), "zero::x" (zeroinfl),
# "selection::x" (hurdle "zero_x").
zoo_term <- function(key, nms) {
  k <- sub("^.*::", "", key)
  eq <- if (grepl("::", key, fixed = TRUE)) sub("::.*$", "", key) else ""
  # A hurdle's zero part is keyed "selection::x" (muse P2-21), coefficient "zero_x".
  if (eq == "selection") return(if (paste0("zero_", k) %in% nms) paste0("zero_", k) else NA_character_)
  cand <- c(k, paste0(eq, ":", k), paste0(eq, "_", k), if (!eq %in% c("", "zero")) paste0("count_", k))
  hit <- cand[cand %in% nms]
  if (length(hit)) hit[1] else NA_character_
}

zoo_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# Supported: a table under the expected header (given per entry, not asked
# of tabtools: review P2-4 of group t2a), and the x coefficient on its row,
# exponentiated exactly for a ratio header, with an interval around it and
# a p-value in [0, 1].
zoo_ratio <- c("OR", "IRR", "HR", "SHR", "TR", "RRR")

expect_zoo_supported <- function(fit, label, scale) {
  tt <- zoo_quiet(regtab(fit))
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$stored$coef_label, scale, label = paste(label, "header"))
  r <- tt$meta$regtab_rows
  est <- r[r$status %in% "est" & sub("^.*::", "", r$key) == "x", , drop = FALSE]
  expect_gt(nrow(est), 0L, label = paste(label, "x row"))
  if (!nrow(est)) return(invisible(tt))
  if (!is.data.frame(fit)) {
    fit0 <- if (inherits(fit, "mira")) fit$analyses[[1]] else if (inherits(fit, "tt_mi")) fit$analyses[[1]] else fit
    b <- zoo_coef(if (inherits(fit0, "lmerModLmerTest")) methods::as(fit0, "lmerMod") else fit0)
    nm <- zoo_term(est$key[1], names(b))
    expect_false(is.na(nm), label = paste(label, "coefficient of", est$key[1]))
    want <- unname(b[nm])
    if (scale %in% zoo_ratio) want <- exp(want)
    expect_equal(est$estimate[1], want, tolerance = 1e-10, label = paste(label, "estimate"))
  }
  expect_true(est$conf.low[1] <= est$estimate[1] && est$estimate[1] <= est$conf.high[1], label = paste(label, "CI"))
  expect_true(est$p.value[1] >= 0 && est$p.value[1] <= 1, label = paste(label, "p"))
  expect_true(nzchar(tt$stored$methods))
  # Milestone H task H6 (group t2b) adds tt_ci_methods(), the per-class
  # capability beside tt_vce_types(). Not merged when this was written, so
  # checked only when it exists: Wald always, and "profile" exactly when
  # regtab() honours it.
  ns <- asNamespace("tabtools")
  if (exists("tt_ci_methods", envir = ns, inherits = FALSE)) {
    fit1 <- if (inherits(fit, "lmerModLmerTest")) methods::as(fit, "lmerMod") else fit
    cm <- get("tt_ci_methods", envir = ns)(fit1)
    expect_true(is.character(cm) && "wald" %in% cm, label = paste(label, "tt_ci_methods() has wald"))
    prof <- tryCatch({
      zoo_quiet(regtab(fit1, ci_method = "profile"))
      TRUE
    }, error = function(e) FALSE)
    expect_identical("profile" %in% cm, prof, label = paste(label, "tt_ci_methods() and profile"))
  }
  invisible(tt)
}

# Refused: an error naming the class (or the refused setting) and a hint;
# never an internal or base-R error.
expect_zoo_refused <- function(expr, pattern, hint, label) {
  err <- tryCatch(zoo_quiet(expr), error = function(e) e)
  expect_s3_class(err, "error")
  if (!inherits(err, "error")) return(invisible(NULL))
  msg <- gsub("[[:space:]]+", " ", conditionMessage(err))
  expect_match(msg, pattern, fixed = TRUE, label = label)
  expect_match(msg, hint, fixed = TRUE, label = label)
  expect_false(grepl("Internal error|subscript out of bounds|missing value where", msg), label = label)
}

zoo_finegray <- function(d) {
  survival::finegray(survival::Surv(time, cause) ~ ., data = d[, c("time", "cause", "x", "z")], etype = "event")
}

zoo_lmertest_mock <- function(d) {
  m <- lme4::lmer(yc ~ x + z + (1 | g), d, REML = FALSE)
  # When installed, use the real class definition: registering an incomplete
  # lookalike first masks lmerTest's slots when its genuine case runs later.
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    return(zoo_fn("lmerTest", "as_lmerModLmerTest")(m))
  }
  env <- new.env(parent = asNamespace("lme4"))
  if (!methods::isClass("lmerModLmerTest", where = env)) {
    methods::setClass("lmerModLmerTest", contains = "lmerMod", slots = c(vcov_beta = "matrix"), where = env)
    # Keep it alive while the caller consumes the fit, then clear both
    # its binding and S4 cache entry at the end of that test's scope.
    withr::defer(methods::removeClass("lmerModLmerTest", where = env),
                 envir = parent.frame())
  }
  methods::new("lmerModLmerTest", m, vcov_beta = matrix(0))
}

# ---------------------------------------------------------------------------
# The zoo: name, packages, fit, and "supported" or a refusal (message
# pattern, hint). `vce_cluster` marks fits whose data carry `id` for
# vce = "cluster".

zoo <- list(
  list(name = "lm", scale = "Coef.", pkgs = character(), fit = function(d) lm(yc ~ x + z + f, d)),
  list(name = "glm logit", scale = "OR", pkgs = character(), fit = function(d) glm(yb ~ x + z + f, binomial, d)),
  list(name = "glm probit", scale = "Coef.", pkgs = character(), fit = function(d) glm(yb ~ x + z, binomial("probit"), d)),
  list(name = "glm cloglog", scale = "Coef.", pkgs = character(), fit = function(d) glm(yb ~ x + z, binomial("cloglog"), d)),
  list(name = "glm poisson", scale = "IRR", pkgs = character(), fit = function(d) glm(ycnt ~ x + z, poisson, d)),
  list(name = "glm quasipoisson", scale = "IRR", pkgs = character(), fit = function(d) glm(ycnt ~ x + z, quasipoisson, d)),
  list(name = "glm Gamma", scale = "Coef.", pkgs = character(), fit = function(d) glm(ypos ~ x + z, Gamma("log"), d)),
  list(name = "glm gaussian", scale = "Coef.", pkgs = character(), fit = function(d) glm(yc ~ x + z, gaussian, d)),
  list(name = "negbin", scale = "IRR", pkgs = "MASS", fit = function(d) MASS::glm.nb(ynb ~ x + z, d)),
  list(name = "coxph", scale = "HR", pkgs = "survival",
       fit = function(d) survival::coxph(survival::Surv(time, status) ~ x + z, d, ties = "breslow")),
  # survival cannot re-evaluate a tt() fit row by row: robust/cluster are
  # refused by name (Milestone H group t2b).
  list(name = "coxph with tt()", scale = "HR", pkgs = "survival", vce_refused = c("robust", "cluster"),
       fit = function(d) survival::coxph(survival::Surv(time, status) ~ x + tt(z), d, ties = "breslow",
                                         tt = function(x, t, ...) x * log(t))),
  list(name = "coxph Fine-Gray", scale = "SHR", pkgs = "survival",
       fit = function(d) survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x + z, zoo_finegray(d),
                                         weights = fgwt)),
  # Task 5.15: Stata's clogit, or. clogit() calls coxph() and strata() by
  # name in its caller's frame.
  list(name = "clogit", scale = "OR", pkgs = "survival", vce_refused = c("robust", "cluster"),
       fit = function(d) evalq(clogit(yb ~ x + z + strata(g), d),
                               list2env(list(d = d), parent = asNamespace("survival")))),
  list(name = "survreg weibull", scale = "TR", pkgs = "survival",
       fit = function(d) survival::survreg(survival::Surv(time, status) ~ x + z, d)),
  list(name = "survreg gaussian (intreg)", scale = "Coef.", pkgs = "survival",
       fit = function(d) survival::survreg(survival::Surv(yc) ~ x + z, d, dist = "gaussian")),
  list(name = "tobit", scale = "Coef.", pkgs = c("AER", "survival"),
       fit = function(d) zoo_fn("AER", "tobit")(yc ~ x + z, left = 1, data = d)),
  list(name = "polr", scale = "OR", pkgs = "MASS", fit = function(d) MASS::polr(ord ~ x + z, d, Hess = TRUE)),
  list(name = "clm", scale = "OR", pkgs = "ordinal", fit = function(d) ordinal::clm(ord ~ x + z, data = d)),
  list(name = "multinom", scale = "RRR", pkgs = "nnet", fit = function(d) nnet::multinom(ord ~ x + z, d, trace = FALSE)),
  list(name = "zeroinfl", scale = "Coef.", pkgs = "pscl", fit = function(d) pscl::zeroinfl(yzi ~ x + z | x, d)),
  list(name = "hurdle", scale = "Coef.", pkgs = "pscl", fit = function(d) pscl::hurdle(yzi ~ x + z, d)),
  list(name = "crr", scale = "SHR", pkgs = "cmprsk",
       fit = function(d) cmprsk::crr(d$time, as.integer(d$cause) - 1L, cbind(x = d$x, z = d$z))),
  list(name = "lmerMod", scale = "Coef.", pkgs = "lme4", fit = function(d) lme4::lmer(yc ~ x + z + (1 | g), d, REML = FALSE)),
  list(name = "glmerMod", scale = "OR", pkgs = "lme4", fit = function(d) lme4::glmer(yb ~ x + z + (1 | g), d, binomial)),
  list(name = "lme", scale = "Coef.", pkgs = "nlme", fit = function(d) nlme::lme(yc ~ x + z, random = ~ 1 | g, data = d, method = "ML")),
  list(name = "glmmTMB", scale = "OR", pkgs = "glmmTMB",
       fit = function(d) glmmTMB::glmmTMB(yb ~ x + z + (1 | g), d, family = binomial)),
  list(name = "geeglm", scale = "OR", pkgs = "geepack", fit = function(d) geepack::geeglm(yb ~ x + z, binomial, d, id = id)),
  list(name = "svyglm", scale = "OR", pkgs = "survey", fit = function(d) {
    des <- survey::svydesign(ids = ~id, weights = ~w, data = d)
    survey::svyglm(yb ~ x + z, design = des, family = quasibinomial)
  }),
  list(name = "glm_weightit", scale = "Coef.", pkgs = "WeightIt", fit = function(d) {
    W <- WeightIt::weightit(treat ~ z, data = d, method = "glm")
    WeightIt::glm_weightit(yc ~ treat + x, data = d, weightit = W)
  }),
  list(name = "lmerModLmerTest (mock; cast to lmerMod)", scale = "Coef.", pkgs = "lme4", fit = zoo_lmertest_mock),
  list(name = "lmerModLmerTest", scale = "Coef.", pkgs = c("lmerTest", "lme4"),
       fit = function(d) zoo_fn("lmerTest", "lmer")(yc ~ x + z + (1 | g), d, REML = FALSE)),
  list(name = "data frame (tidy_plus_plus)", scale = "OR", pkgs = "broom.helpers", fit = function(d) {
    tp <- broom.helpers::tidy_plus_plus(glm(yb ~ x + f, binomial, d), exponentiate = TRUE)
    attr(tp, "stata_cmd") <- "logit"
    attr(tp, "effect_scale") <- "OR"
    attr(tp, "se_scale") <- "link"
    attr(tp, "conf.level") <- .95
    attr(tp, "inference_reference") <- "normal"
    tp
  }),
  # Refused, by name with a hint.
  list(name = "coxph with frailty()", pkgs = "survival", refused = c("<coxph.penal>", "frailty"),
       fit = function(d) survival::coxph(survival::Surv(time, status) ~ x + survival::frailty(g), d)),
  list(name = "svycoxph", pkgs = c("survey", "survival"), refused = c("<svycoxph>", "Survey-design fits"),
       fit = function(d) survey::svycoxph(survival::Surv(time, status) ~ x, design = survey::svydesign(ids = ~id, data = d))),
  list(name = "svyolr", pkgs = c("survey", "MASS"), refused = c("<svyolr>", "Survey-design fits"),
       fit = function(d) survey::svyolr(ord ~ x, design = survey::svydesign(ids = ~id, data = d))),
  list(name = "gls", pkgs = "nlme", refused = c("<gls>", "Generalized least squares"),
       fit = function(d) nlme::gls(yc ~ x + z, data = d)),
  list(name = "glmmPQL", pkgs = c("MASS", "nlme"), refused = c("<glmmPQL>", "lme4::glmer"),
       fit = function(d) MASS::glmmPQL(yb ~ x, random = ~ 1 | g, family = binomial, data = d, verbose = FALSE)),
  list(name = "clmm", pkgs = "ordinal", refused = c("<clmm>", "meologit"),
       fit = function(d) ordinal::clmm(ord ~ x + (1 | g), data = d)),
  list(name = "glmmTMB with ziformula", pkgs = "glmmTMB",
       refused = c("fits with a `ziformula` submodel", "no zero-inflation or dispersion equations"),
       fit = function(d) glmmTMB::glmmTMB(yzi ~ x + (1 | g), d, family = poisson, ziformula = ~1)),
  list(name = "gam", pkgs = "mgcv", refused = c("<gam>", "Generalized additive models"),
       fit = function(d) mgcv::gam(yc ~ s(x) + z, data = d)),
  list(name = "rlm", pkgs = "MASS", refused = c("<rlm>", "rreg"), fit = function(d) MASS::rlm(yc ~ x + z, d)),
  list(name = "aov", pkgs = character(), refused = c("<aov>", "with `lm()`"), fit = function(d) aov(yc ~ f, d)),
  list(name = "mlm", pkgs = character(), refused = c("<mlm>", "one `lm()` per outcome"),
       fit = function(d) lm(cbind(yc, y2) ~ x, d)),
  list(name = "nls", pkgs = character(), refused = c("<nls>", "Nonlinear least squares"),
       fit = function(d) nls(ypos ~ a * exp(b * x), d, start = list(a = 1, b = 0.1))),
  list(name = "rq", pkgs = "quantreg", refused = c("<rq>", "qreg"),
       fit = function(d) zoo_fn("quantreg", "rq")(yc ~ x + z, data = d)),
  list(name = "lavaan", pkgs = "lavaan", refused = c("<lavaan>", "Structural equation models"),
       fit = function(d) zoo_fn("lavaan", "sem")("yc ~ x + z", data = d)),
  list(name = "fixest", pkgs = "fixest", refused = c("<fixest>", "Fixed-effects estimators"),
       fit = function(d) zoo_fn("fixest", "feols")(yc ~ x + z | g, d)),
  list(name = "plm", pkgs = "plm", refused = c("<plm>", "Panel estimators"),
       fit = function(d) zoo_fn("plm", "plm")(yc ~ x + z, data = d, index = c("g"), model = "within")),
  list(name = "ivreg", pkgs = "ivreg", refused = c("<ivreg>", "Instrumental-variables"),
       fit = function(d) zoo_fn("ivreg", "ivreg")(yc ~ x | z, data = d)),
  list(name = "lm_robust", pkgs = "estimatr", refused = c("<lm_robust>", "vce = \"robust\""),
       fit = function(d) zoo_fn("estimatr", "lm_robust")(yc ~ x + z, data = d)),
  list(name = "iv_robust", pkgs = "estimatr", refused = c("<iv_robust>", "vce = \"robust\""),
       fit = function(d) zoo_fn("estimatr", "iv_robust")(yc ~ x | z, data = d)),
  list(name = "betareg", pkgs = "betareg", refused = c("<betareg>", "Beta regression"),
       fit = function(d) zoo_fn("betareg", "betareg")(plogis(yc / 4) ~ x, data = d)),
  list(name = "logistf", pkgs = "logistf", refused = c("<logistf>", "Firth"),
       fit = function(d) zoo_fn("logistf", "logistf")(yb ~ x + z, data = d)),
  list(name = "coxme", pkgs = c("coxme", "survival"), refused = c("<coxme>", "Mixed-effects Cox models have no Stata equivalent"),
       fit = function(d) zoo_fn("coxme", "coxme")(survival::Surv(time, status) ~ x + (1 | g), data = d)),
  list(name = "coxme (mock)", pkgs = character(), refused = c("<coxme>", "`stcox, shared()` is gamma frailty"),
       fit = function(d) structure(list(coefficients = c(x = 0.1), call = quote(coxme(y ~ x))), class = "coxme")),
  # Bayesian fits: mocked (fitting would compile a Stan model).
  list(name = "brmsfit (mock)", pkgs = character(), refused = c("<brmsfit>", "Bayesian fits"),
       fit = function(d) structure(list(fit = NULL, formula = yc ~ x), class = "brmsfit")),
  list(name = "stanreg (mock)", pkgs = character(), refused = c("<stanreg>", "Bayesian fits"),
       fit = function(d) structure(list(coefficients = c(x = 1), call = quote(stan_glm(yc ~ x))),
                                   class = c("stanreg", "glm", "lm"))),
  # Multiply imputed fits (task 5.14): a mira (mocked with mice 3.19's
  # structure; two identical imputations, so the pooled estimate is the
  # fit's) and tt_mi() are pooled; a pooled mipo is refused.
  list(name = "mira (mock)", scale = "Coef.", pkgs = character(),
       fit = function(d) structure(list(call = quote(with(imp, lm(yc ~ x))), call1 = quote(mice(d)), nmis = c(yc = 0L),
                                        analyses = list(lm(yc ~ x, d), lm(yc ~ x, d))), class = c("mira", "matrix"))),
  list(name = "tt_mi (glm logit)", scale = "OR", pkgs = character(),
       fit = function(d) tt_mi(list(glm(yb ~ x + z, binomial, d), glm(yb ~ x + z, binomial, d)))),
  list(name = "mipo (mock)", pkgs = character(), refused = c("<mipo", "Pass the <mira>"),
       fit = function(d) structure(list(m = 2L, pooled = data.frame(term = "x", estimate = 1)), class = c("mipo", "data.frame")))
)

for (e in zoo) {
  local({
    e <- e
    test_that(paste("T1 zoo:", e$name, if (is.null(e$refused)) "is supported" else "is refused by name with a hint"), {
      for (p in e$pkgs) skip_if_not_installed(p)
      d <- zoo_data()
      fit <- zoo_quiet(e$fit(d))
      if (is.null(e$refused)) {
        expect_zoo_supported(fit, e$name, e$scale)
      } else {
        pat <- if (startsWith(e$refused[1], "<")) paste0("does not support ", e$refused[1]) else e$refused[1]
        if (e$name == "mipo (mock)") pat <- "pooled mice result (<mipo"
        expect_zoo_refused(regtab(fit), pat, e$refused[2], e$name)
      }
    })
  })
}

# Each class of qa/data/coverage_matrix.csv and the zoo
# entry that covers it.
zoo_coverage <- c(
  "lm" = "lm", "glm (logit)" = "glm logit", "glm (probit / cloglog / non-canonical)" = "glm probit",
  "glm (poisson)" = "glm poisson", "glm (gaussian/Gamma/quasi; estimated dispersion)" = "glm Gamma",
  "negbin" = "negbin", "coxph" = "coxph", "clogit" = "clogit", "survreg" = "survreg weibull",
  "tobit" = "tobit", "polr" = "polr", "clm" = "clm", "multinom" = "multinom", "zeroinfl" = "zeroinfl",
  "hurdle" = "hurdle", "crr" = "crr", "coxph on finegray() data" = "coxph Fine-Gray", "lmerMod" = "lmerMod",
  "glmerMod" = "glmerMod", "glmmTMB" = "glmmTMB", "glmmTMB (ziformula / dispformula)" = "glmmTMB with ziformula",
  "geeglm" = "geeglm", "svyglm" = "svyglm", "svycoxph" = "svycoxph", "svyolr" = "svyolr",
  "glm_weightit" = "glm_weightit", "lme" = "lme", "gls" = "gls", "glmmPQL" = "glmmPQL",
  "lmerModLmerTest" = "lmerModLmerTest", "clmm" = "clmm", "coxme" = "coxme",
  "coxph + frailty() (coxph.penal)" = "coxph with frailty()", "fixest (feols)" = "fixest",
  "fixest (feglm logit)" = "fixest", "fixest (fepois)" = "fixest", "plm (within)" = "plm", "ivreg" = "ivreg",
  "lm_robust" = "lm_robust", "iv_robust" = "iv_robust", "betareg" = "betareg", "logistf" = "logistf",
  "rq" = "rq", "gam" = "gam", "rlm" = "rlm", "lavaan" = "lavaan", "mira (mice)" = "mira (mock)",
  "mipo (mice pooled)" = "mipo (mock)", "brmsfit" = "brmsfit (mock)", "stanreg" = "stanreg (mock)"
)

test_that("T1 zoo: every class of the coverage matrix has a zoo entry", {
  names_zoo <- vapply(zoo, `[[`, "", "name")
  expect_true(all(zoo_coverage %in% names_zoo), label = paste(setdiff(zoo_coverage, names_zoo), collapse = ", "))
  # The matrix lives in the .Rbuildignored qa/ lane: checked from the source tree only.
  f <- test_path("..", "..", "qa", "data", "coverage_matrix.csv")
  skip_if_not(file.exists(f), "coverage_matrix.csv is not in the installed tests")
  cm <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_setequal(names(zoo_coverage), cm$class)
})

# ---------------------------------------------------------------------------
# T2: the vce sweep

# For each est row of x and z: the interval is (g^-1)(b -/+ q se) and the
# p-value 2 P(|T| > |b / se|), with b = g(estimate), se from tt_vcov(fit,
# vce, cluster) and q, T from tt_wald_df(). Returns the SEs used.
expect_zoo_vce <- function(fit, vce, cluster, label) {
  tt <- zoo_quiet(regtab(fit, vce = vce, cluster = cluster))
  info <- tabtools:::tt_model_info(fit)
  V <- as.matrix(tabtools::tt_vcov(fit, vce, cluster))
  df <- tabtools:::tt_wald_df(fit, vce, cluster)
  r <- tt$meta$regtab_rows
  r <- r[r$status %in% "est" & sub("^.*::", "", r$key) %in% c("x", "z"), , drop = FALSE]
  g <- if (isTRUE(info$exponentiate)) log else identity
  q <- if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)
  ses <- c()
  checked <- 0L
  for (i in seq_len(nrow(r))) {
    nm <- zoo_term(r$key[i], rownames(V))
    if (is.na(nm)) next
    se <- sqrt(V[nm, nm])
    b <- g(r$estimate[i])
    lab <- paste(label, vce, r$key[i])
    expect_equal(g(r$conf.low[i]), b - q * se, tolerance = 1e-8, label = paste(lab, "lower"))
    expect_equal(g(r$conf.high[i]), b + q * se, tolerance = 1e-8, label = paste(lab, "upper"))
    p <- if (is.finite(df)) 2 * stats::pt(-abs(b / se), df) else 2 * stats::pnorm(-abs(b / se))
    expect_equal(r$p.value[i], p, tolerance = 1e-8, label = paste(lab, "p"))
    ses[r$key[i]] <- se
    checked <- checked + 1L
  }
  expect_gt(checked, 0L, label = paste(label, vce, "rows checked"))
  list(se = ses, ci = stats::setNames(r$conf.high - r$conf.low, r$key))
}

# (Multiply imputed models are pooled over each imputation's tt_vcov(), one
# df per coefficient: test-regtab-mi.R checks them against Stata.)
for (e in Filter(function(e) is.null(e$refused) && !grepl("data frame|LmerTest|mira|tt_mi", e$name), zoo)) {
  local({
    e <- e
    test_that(paste("T2 vce sweep:", e$name), {
      skip_on_cran()  # heavy (CRAN-lane runtime, pre-release review P2-1); runs in every NOT_CRAN gate
      for (p in e$pkgs) skip_if_not_installed(p)
      d <- zoo_data()
      fit <- zoo_quiet(e$fit(d))
      ok <- tabtools::tt_vce_types(fit)
      seen <- list()
      for (v in c("stata", "model", "robust", "cluster")) {
        cl <- if (v == "cluster" && !inherits(fit, "geeglm")) ~id else NULL
        if (!v %in% ok) {
          expect_error(zoo_quiet(regtab(fit, vce = v, cluster = cl)), "is not available for", fixed = TRUE)
          next
        }
        if (v %in% e$vce_refused) {
          expect_error(zoo_quiet(regtab(fit, vce = v, cluster = cl)), "cannot be computed", fixed = TRUE)
          next
        }
        seen[[v]] <- expect_zoo_vce(fit, v, cl, e$name)
      }
      # A vce that moves the SE moves the interval with it.
      vs <- names(seen)
      for (a in vs) for (b in vs) {
        if (a >= b) next
        k <- intersect(names(seen[[a]]$se), names(seen[[b]]$se))
        moved <- abs(seen[[a]]$se[k] / seen[[b]]$se[k] - 1) > 1e-6
        if (any(moved)) {
          expect_true(all(abs(seen[[a]]$ci[k][moved] - seen[[b]]$ci[k][moved]) > 0), label = paste(e$name, a, b))
        }
      }
      expect_true("stata" %in% names(seen))
    })
  })
}

test_that("T2: profile intervals are refused beside a robust or clustered variance (F34)", {
  d <- zoo_data()
  for (fit in list(lm(yc ~ x, d), glm(yb ~ x, binomial, d))) {
    for (v in c("robust", "cluster")) {
      cl <- if (v == "cluster") ~id else NULL
      expect_error(regtab(fit, vce = v, cluster = cl, ci_method = "profile"), "profile", ignore.case = TRUE)
    }
    # With vce = "stata" the p-value stays the Wald one of that SE.
    tt <- zoo_quiet(regtab(fit, ci_method = "profile"))
    r <- tt$meta$regtab_rows
    r <- r[r$key == "x", ]
    V <- tabtools::tt_vcov(fit)
    df <- tabtools:::tt_wald_df(fit)
    b <- if (inherits(fit, "glm")) log(r$estimate) else r$estimate
    p <- if (is.finite(df)) 2 * pt(-abs(b / sqrt(V["x", "x"])), df) else 2 * pnorm(-abs(b / sqrt(V["x", "x"])))
    expect_equal(r$p.value, p, tolerance = 1e-8)
  }
})

# M30b (review P2-4 of group t2a): a robust or cluster request answered
# with the model variance would pass the sweep above, which takes the SE
# from tt_vcov() itself. On heteroscedastic data the robust SE must differ
# from the model SE, and for lm/glm equal an independent sandwich.
test_that("T2: robust and cluster variances are the sandwich, never the model variance", {
  skip_if_not_installed("sandwich")
  set.seed(7)
  n <- 300
  d <- data.frame(id = rep(1:50, each = 6), x = rnorm(n))
  d$y <- 1 + d$x + rnorm(n, sd = exp(0.8 * d$x))
  d$yb <- rbinom(n, 1, plogis(0.3 + 0.8 * d$x + rep(rnorm(50), each = 6)))
  d$yc <- rpois(n, exp(0.2 + 0.4 * d$x + rep(rnorm(50, sd = 0.5), each = 6)))
  se <- function(V) sqrt(diag(as.matrix(V)))[["x"]]
  fits <- list(lm = lm(y ~ x, d), logit = glm(yb ~ x, binomial, d), poisson = glm(yc ~ x, poisson, d))
  if (requireNamespace("MASS", quietly = TRUE)) fits$negbin <- zoo_quiet(MASS::glm.nb(yc ~ x, d))
  for (nm in names(fits)) {
    f <- fits[[nm]]
    m <- se(tabtools::tt_vcov(f, "model"))
    r <- se(tabtools::tt_vcov(f, "robust"))
    cl <- se(tabtools::tt_vcov(f, "cluster", cluster = d$id))
    expect_gt(abs(r / m - 1), 0.01, label = paste(nm, "robust vs model"))
    expect_gt(abs(cl / m - 1), 0.01, label = paste(nm, "cluster vs model"))
    # The table's CI moves with it.
    ci <- function(v, cl = NULL) {
      rr <- zoo_quiet(regtab(f, vce = v, cluster = cl))$meta$regtab_rows
      rr <- rr[rr$key == "x", ]
      rr$conf.high - rr$conf.low
    }
    expect_false(isTRUE(all.equal(ci("robust"), ci("model"))), label = paste(nm, "CI robust vs model"))
  }
  N <- n
  expect_equal(se(tabtools::tt_vcov(fits$lm, "robust")), se(sandwich::vcovHC(fits$lm, type = "HC1")), tolerance = 1e-10)
  expect_equal(se(tabtools::tt_vcov(fits$lm, "cluster", cluster = d$id)),
               se(sandwich::vcovCL(fits$lm, cluster = d$id, type = "HC1", cadjust = TRUE)), tolerance = 1e-10)
  # glm: sandwich's bread and scores come from glm()'s last IRLS iterate,
  # tabtools' from the final estimates (Stata's), a ~1e-6 relative gap.
  for (nm in c("logit", "poisson")) {
    f <- fits[[nm]]
    expect_equal(se(tabtools::tt_vcov(f, "robust")), se(sandwich::vcovHC(f, type = "HC0")) * sqrt(N / (N - 1)),
                 tolerance = 1e-5, label = paste(nm, "robust oracle"))
    expect_equal(se(tabtools::tt_vcov(f, "cluster", cluster = d$id)),
                 se(sandwich::vcovCL(f, cluster = d$id, type = "HC0", cadjust = TRUE)),
                 tolerance = 1e-5, label = paste(nm, "cluster oracle"))
  }
  skip_if_not_installed("survival")
  d$time <- rexp(n, exp(0.5 * d$x))
  d$status <- rbinom(n, 1, 0.8)
  cx <- survival::coxph(survival::Surv(time, status) ~ x, d, ties = "breslow")
  expect_gt(abs(se(tabtools::tt_vcov(cx, "cluster", cluster = d$id)) / se(tabtools::tt_vcov(cx, "model")) - 1), 0.01)
})
