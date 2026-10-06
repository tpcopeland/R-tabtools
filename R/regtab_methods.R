# The regtab methods sentence from model metadata (Milestone H task H11,
# external review F12, decision H-D3).
#
# Stata tabtools 2.1.12 builds the sentence the same way (take_action item
# 11, fixed; `_regtab_modelnoun` in regtab.ado): the estimates are named by
# each model's scale (never a `coef()`/`cdisc` relabel), the model by its
# command, glm/xtgee family and link, survival metric and prefix, and
# "univariable"/"multivariable" by the number of predictor variables. R
# uses Stata's nouns wherever R has the Stata model; the R-only models
# (pscl::hurdle's count families, ordinal::clm's non-Stata links) keep
# their own. 2.1.11 picked the sentence from the header ("linear
# regression" for a probit, "multivariable" for one predictor).

# Estimates by effect scale (the model's, never the `coef` header).
.rt_methods_estimates <- function(scale) {
  switch(scale,
    "OR" = "Odds ratios",
    "HR" = "Hazard ratios",
    "SHR" = "Subhazard ratios",
    "IRR" = "Incidence rate ratios",
    "RRR" = "Relative risk ratios",
    "TR" = "Time ratios",
    "RR" = "Risk ratios",
    "exp(b)" = "Exponentiated coefficients",
    "Coef." = "Coefficients",
    scale
  )
}

# Stata's glm family and link names for an R family object's (glm.ado's
# MapFam/MapLink as `_regtab_modelnoun` reads them).
.rt_stata_family <- function(family) {
  if (is.na(family)) return(NA_character_)
  if (family %in% c("binomial", "quasibinomial")) return("binomial")
  if (family %in% c("poisson", "quasipoisson")) return("poisson")
  if (grepl("^(Negative Binomial|nbinom|negbin)", family)) return("nbinomial")
  switch(family, gaussian = "gaussian", Gamma = "gamma", inverse.gaussian = "igaussian", family)
}

.rt_stata_link <- function(link) {
  if (is.na(link)) return(NA_character_)
  switch(link, inverse = "reciprocal", sqrt = , "1/mu^2" = "power", link)
}

# A generalized linear model by family and link (`_regtab_modelnoun`,
# tabtools 2.1.12): the named models, else "generalized linear model
# (<family> family, <link> link)".
.rt_methods_glm <- function(family, link) {
  f <- .rt_stata_family(family %||% NA_character_)
  l <- .rt_stata_link(link %||% NA_character_)
  if (is.na(f)) return("regression")
  if (is.na(l)) l <- switch(f, binomial = "logit", poisson = , nbinomial = "log", gaussian = "identity",
                            gamma = "reciprocal", "")
  key <- paste(f, l)
  switch(key,
    "binomial logit" = "logistic regression",
    "binomial probit" = "probit regression",
    "binomial cloglog" = "complementary log-log regression",
    "binomial log" = "log-binomial regression",
    "poisson log" = "Poisson regression",
    "nbinomial log" = "negative binomial regression",
    "gaussian identity" = "linear regression",
    "gamma log" = "gamma regression with a log link",
    paste0("generalized linear model (", f, " family, ", l, " link)")
  )
}

#' The model a regtab column comes from, in words
#'
#' From `tt_model_info()`: the Stata command, family and link. Mixed
#' models, survival models and GEE get their own nouns; a `survey::svyglm`
#' fit is "survey-weighted" (review P3-4 of group t2a).
#' @param info One `tt_model_info()` result.
#' @keywords internal
#' @noRd
.rt_methods_model <- function(info) {
  noun <- .rt_methods_noun(info)
  if (identical(info$class, "svyglm")) noun <- paste("survey-weighted", noun)
  # A multiply imputed model (task 5.14): `_regtab_modelnoun`'s "... with
  # multiple imputation" for the mi estimate prefix.
  if (isTRUE(info$mi)) noun <- paste(noun, "with multiple imputation")
  noun
}

.rt_methods_noun <- function(info) {
  cmd <- tolower(info$stata_cmd %||% "unknown")
  if (is.na(cmd)) cmd <- "unknown"
  one <- function(x) {
    # pscl::hurdle keeps one distribution per part (count, zero).
    if (is.list(x)) x <- x$count %||% x[[1]]
    if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1]
  }
  fam <- one(info$family)
  link <- one(info$link)
  # Stata's me* commands: "mixed-effects " and the noun of the command
  # without its "me" (`_regtab_modelnoun`); meglm by family and link.
  if (cmd %in% c("melogit", "meprobit", "mecloglog", "mepoisson", "menbreg", "meologit", "meoprobit",
                 "mestreg", "meglm", "meintreg", "metobit", "meqrlogit", "meqrpoisson")) {
    base <- info
    base$stata_cmd <- substring(cmd, 3L)
    return(paste0("mixed-effects ", .rt_methods_noun(base)))
  }
  # polr/clm links without a Stata ordered command (stata_cmd NA).
  if (identical(cmd, "unknown") && info$class %in% c("polr", "clm") && !is.na(link)) {
    return(paste0("ordinal regression (", link, " link)"))
  }
  switch(cmd,
    regress = "linear regression",
    logit = , logistic = , qrlogit = "logistic regression",
    # Stata tabtools 2.1.14 (`_regtab_modelnoun`, regtab.ado:4150; goldens
    # R74-R76, R83); up to 2.1.13 clogit was unclassified.
    clogit = "conditional logistic regression",
    probit = "probit regression",
    hetprobit = "heteroskedastic probit regression",
    qreg = , bsqreg = , sqreg = "quantile regression",
    ivregress = "instrumental-variables regression",
    cloglog = "complementary log-log regression",
    poisson = , qrpoisson = "Poisson regression",
    nbreg = "negative binomial regression",
    gnbreg = "generalized negative binomial regression",
    glm = .rt_methods_glm(fam, link),
    stcox = , cox = "Cox proportional hazards regression",
    stcrreg = , finegray = "Fine-Gray competing-risks regression",
    streg = if (identical(info$effect_scale, "HR")) "parametric proportional hazards survival regression" else
      "accelerated failure-time survival regression",
    intreg = "interval regression",
    tobit = "tobit regression",
    mlogit = "multinomial logistic regression",
    mprobit = "multinomial probit regression",
    ologit = "ordered logistic regression",
    # ordinal::clm links Stata has no ordered model for (R only).
    oprobit = if (is.na(link) || link == "probit") "ordered probit regression" else
      paste0("ordinal regression (", link, " link)"),
    zip = "zero-inflated Poisson regression",
    zinb = "zero-inflated negative binomial regression",
    # pscl::hurdle's count models are not Stata's (linear) churdle; R names
    # them (R only). A data frame declared churdle reads as Stata's.
    churdle = if (is.na(fam)) "Cragg hurdle regression" else switch(fam,
      poisson = "Poisson hurdle regression",
      negbin = "negative binomial hurdle regression",
      geometric = "geometric hurdle regression",
      "hurdle regression"),
    mixed = "linear mixed-effects regression",
    xtreg = "linear panel-data regression",
    xtlogit = "panel-data logistic regression",
    xtpoisson = "panel-data Poisson regression",
    xtgee = paste0("generalized estimating equation (GEE) ", .rt_methods_glm(fam, link)),
    "regression"
  )
}

# Right-hand-side term labels of a fitted model's fixed part: random-effect
# bars, strata(), cluster(), frailty() and offset() terms are not
# predictors; a tt() term (a time-varying effect) is.
.rt_rhs_terms <- function(fit) {
  tl <- function(tt) tryCatch(attr(tt, "term.labels"), error = function(e) NULL)
  if (inherits(fit, c("zeroinfl", "hurdle")) && is.list(fit$terms)) {
    return(unique(unlist(lapply(fit$terms[intersect(c("count", "zero"), names(fit$terms))], tl))))
  }
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f) || !inherits(f, "formula") || length(f) < 3L) return(NULL)
  lab <- tl(stats::terms(f))
  if (is.null(lab)) return(NULL)
  lab[!grepl("|", lab, fixed = TRUE) &
        !grepl("^(survival::)?(strata|cluster|frailty(\\.[a-z]+)?|offset)\\(", lab)]
}

#' Number of predictors (variables) in a model, for "univariable"
#'
#' The distinct variables of the fixed-effects right-hand side (so `poly(x,
#' 2)` or `x + I(x^2)` is one variable, `a * b` two). A data frame counts
#' its distinct `variable` values over the non-intercept, non-ancillary
#' rows (interaction variables split at `:`). A fit without a formula
#' (`cmprsk::crr`) counts its coefficients.
#' @keywords internal
#' @noRd
.rt_n_predictors <- function(fit) {
  if (inherits(fit, "tt_mi")) fit <- fit$analyses[[1]]
  if (inherits(fit, "tt_uv")) fit <- fit$fits[[1]]
  if (is.data.frame(fit)) {
    x <- .rt_df_body(fit)
    term <- as.character(x$term)
    v <- as.character(.rt_col(x, "variable", NA_character_))
    v[is.na(v)] <- term[is.na(v)]
    drop <- .rt_df_roles(x) != "coef"
    return(length(unique(unlist(strsplit(v[!drop], ":", fixed = TRUE)))))
  }
  tl <- .rt_rhs_terms(fit)
  if (is.null(tl)) {
    # No formula (cmprsk::crr keeps its coefficients in $coef, one per
    # covariate column): one coefficient is one predictor; several may be
    # one factor's dummies, so the count is unknown (NA: no adjective;
    # review P3-3 of group t2a).
    b <- if (!isS4(fit) && is.list(fit) && !is.null(fit$coef)) fit$coef else
      tryCatch(stats::coef(fit), error = function(e) NULL)
    k <- length(setdiff(names(b), "(Intercept)"))
    return(if (k <= 1L) k else NA_integer_)
  }
  vars <- unique(unlist(lapply(tl, function(t) tryCatch(all.vars(str2lang(t)), error = function(e) t))))
  length(vars)
}

# "univariable" / "multivariable" / nothing (an intercept-only model); both
# read "univariable and multivariable" whatever the model order (Stata
# 2.1.12).
.rt_methods_adjective <- function(n) {
  a <- ifelse(is.na(n), "", ifelse(n == 1, "univariable", ifelse(n > 1, "multivariable", "")))
  a <- unique(a[nzchar(a)])
  if (length(a) == 2L) return("univariable and multivariable")
  paste(a, collapse = " and ")
}

# Distinct nouns in model order: "A", "A and B", "A, B, and C" (Stata
# 2.1.12's list, with the serial comma).
.rt_methods_join <- function(x) {
  x <- unique(x)
  if (length(x) <= 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}
