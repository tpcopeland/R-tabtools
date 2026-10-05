# Survey-design and weighting-package fits for regtab (milestone 5w, tasks
# 5.10 and 5.11).
#
# survey::svyglm <-> Stata `svy:` (probes V30-V33, V54):
# - vcov() is Stata's linearized variance (svyset _n [pw]: equal to
#   logit [pw]'s robust variance, 1 +/- 4e-8, the gap being Stata's logit
#   convergence; svyset id [pw]: the clustered one; svy: regress to 3e-15).
# - Wald statistics use Student t with the design degrees of freedom,
#   e(df_r) = survey::degf(design) (N - 1 = 14,999 for svyset _n; PSUs
#   minus strata otherwise); svyglm's own df.residual (14,996) is the
#   vce = "model" distribution (EW2).
# - The family and link classify like glm: (quasi)binomial-logit OR,
#   (quasi)poisson-log IRR (Stata's e(cmd) is logit/poisson under svy:).
# - Observations is the unweighted number of rows (Subjects under a
#   subset() design, Stata's svy, subpop() e(N_sub)); svy reports no
#   log-likelihood, so ll/AIC/BIC are blank; svy: regress reports R^2.
#
# WeightIt::glm_weightit (probe V60): vcov() is the M-estimation variance
# that accounts for estimating the weights; an identity-link y ~ treat fit
# on ATE weights reproduces Stata's `teffects ipw` ATE and standard error
# (to 3e-14 on the cohort). vce = "stata" and "model" both use it; z.

#' @export
tt_regtab_adapter.svyglm <- function(fit, ...) TRUE

#' @export
tt_vcov.svyglm <- function(fit, vce = "stata", cluster = NULL, ...) {
  as.matrix(stats::vcov(fit))
}

#' @export
tt_wald_df.svyglm <- function(fit, vce = "stata", cluster = NULL, ...) {
  if (identical(vce, "model")) return(fit$df.residual)
  if (!requireNamespace("survey", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg survey} is needed for {.cls svyglm} fits.", call = NULL)
  }
  survey::degf(fit$survey.design)
}

#' @export
tt_model_stats.svyglm <- function(fit, info, ...) {
  b <- stats::coef(fit)
  n <- NROW(stats::model.matrix(fit))
  # svy: regress stores e(r2), the weighted R-squared (probe D06: 0.368, as
  # regress [pw]); svy: logit/poisson store no pseudo R-squared, and svy
  # reports no log-likelihood, so those stay blank, with no note (Stata
  # shows nothing either; review P0-5).
  r2 <- NA_real_
  if (identical(fit$family$family, "gaussian") && identical(fit$family$link, "identity")) {
    w <- fit$prior.weights
    y <- fit$y
    e <- y - fit$fitted.values
    r2 <- 1 - sum(w * e^2) / sum(w * (y - stats::weighted.mean(y, w))^2)
  }
  # svy, subpop(): Stata stores the subpopulation size as e(N_sub) and
  # regtab labels the row Subjects (probe D11); survey's subset() of a
  # design is the subpopulation idiom.
  N_sub <- if (.rt_svy_subpop(fit$survey.design)) sum(fit$prior.weights > 0) else NA_real_
  list(N = n, N_sub = N_sub, ll = NA_real_,
       rank = sum(!is.na(b)), aic = NA_real_, bic = NA_real_, r2 = r2, r2_p = NA_real_,
       r2_a = NA_real_, groups = NA_real_, qic = NA_real_, icc = NA_real_,
       F = if (is.na(r2)) NA_real_ else .rt_svy_F(fit))
}

# stats(F) for svy: regress (task 5.17): Stata tabtools shows e(F) for
# every regress, and svy: regress stores the design-based adjusted Wald F,
# W (d - q + 1) / (d q), W = b' V^-1 b over the q slopes and d the design
# degrees of freedom (probe S9: 15.7044744345 on auto, svyset _n
# [pw=turn/40]). svy: logit's e(F) is not shown (Stata's rule), nor does
# svy: regress store e(r2_a) or e(rmse).
.rt_svy_F <- function(fit) {
  b <- stats::coef(fit)
  test <- setdiff(names(b)[!is.na(b)], "(Intercept)")
  q <- length(test)
  d <- tryCatch(survey::degf(fit$survey.design), error = function(e) NA_real_)
  if (!q || !is.finite(d) || d - q + 1 <= 0) return(NA_real_)
  V <- as.matrix(stats::vcov(fit))[test, test, drop = FALSE]
  W <- tryCatch(drop(crossprod(b[test], solve(V, b[test]))), error = function(e) NA_real_)
  W * (d - q + 1) / (d * q)
}

# A survey design that is a subpopulation of a larger design: survey's
# subset() (its call), or rows kept with zero probability (infinite 1/prob).
.rt_svy_subpop <- function(design) {
  if (is.null(design)) return(FALSE)
  cl <- design$call
  (is.call(cl) && identical(cl[[1L]], as.name("subset"))) || any(is.infinite(design$prob))
}

#' @export
tt_regtab_adapter.glm_weightit <- function(fit, ...) TRUE

#' @export
tt_vcov.glm_weightit <- function(fit, vce = "stata", cluster = NULL, ...) {
  as.matrix(stats::vcov(fit))
}

#' @export
tt_wald_df.glm_weightit <- function(fit, vce = "stata", cluster = NULL, ...) Inf

#' @export
tt_model_stats.glm_weightit <- function(fit, info, ...) {
  b <- stats::coef(fit)
  # teffects stores no log-likelihood: blank, as in Stata.
  list(N = stats::nobs(fit), N_sub = NA_real_, ll = NA_real_,
       rank = sum(!is.na(b)), aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_,
       r2_a = NA_real_, groups = NA_real_, qic = NA_real_, icc = NA_real_)
}

# A genuine survey::svyglm fit carries its design (a class attribute
# alone does not make one).
#' @export
tt_regtab_check.svyglm <- function(fit, i) {
  if (is.null(fit$survey.design)) {
    cli::cli_abort("Model {i} has class {.cls svyglm} but no survey design; {.fn regtab} supports {.fn survey::svyglm} fits only.",
                   call = NULL)
  }
  invisible(TRUE)
}
