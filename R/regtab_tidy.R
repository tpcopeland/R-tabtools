# Model tidying for regtab (plan tasks 4.2 and 4.8): Wald estimates, the
# broom.helpers bridge, and the mapping from coefficients to regtab rows.
#
# Stata's regtab reads the active collection: one row per coefficient name
# (`colname`), factor levels rendered under a variable-label parent, and a
# constraint class per cell (base / omitted / empty) read from the collection
# (`regtab.ado:1398-1431`, `:2049-2065`). The R side rebuilds the same rows
# from a fitted model: broom.helpers identifies variables, levels, and
# reference rows; interactions are expanded here, either as Stata's native
# `a#b` grid or as fvgen's flattened rows (decision D9).
#
# Every row carries a Stata-style raw key, which drives the multi-model union
# (Stata unions by colname) and keep()/drop() matching
# (`_tabtools_match_rows.ado`):
#   continuous      mpg                 factor level      2.rep78
#   factor parent   rep78               intercept         _cons
#   interaction     1.foreign#3.rep78   1.foreign#c.mpg   c.mpg#c.weight
#   interaction parent (factor components)                foreign#rep78

# ---------------------------------------------------------------------------
# Wald estimates (decision D8)

# Reference distribution of a model's Wald statistics (Phase 4 review
# P0-2). `vce = "stata"`: Student t with the residual degrees of freedom for
# `lm` (Stata's regress uses t with e(df_r)); the normal for every glm family
# and link, including those with an estimated dispersion (Stata's glm reports
# z statistics and normal-quantile intervals whatever the family, probed
# 2026-09-25: `glm price mpg weight, family(gaussian)`), for glm.nb (nbreg)
# and for coxph (stcox). `vce = "model"`: the distribution R's summary()
# uses, i.e. also t for glm families whose dispersion is estimated. A
# family with a fixed dispersion (R >= 4.3: family$dispersion set, as
# summary.glm() reads it) is not estimated, so it is normal there too
# (codex audit F04).
# Milestone 5w (probes V04, V11, V12): `lm` keeps t(N - k) under
# vce = "robust" (regress, vce(robust) / [pweight]) and takes t(G - 1)
# under vce = "cluster" (regress, vce(cluster): e(df_r) = G - 1); glm stays
# normal.
#' @export
tt_wald_df.default <- function(fit, vce = "stata", cluster = NULL, ...) {
  if (inherits(fit, "negbin")) return(Inf)
  if (identical(vce, "cluster") && identical(class(fit)[1], "lm")) {
    return(.rt_sw_groups(fit, cluster) - 1)
  }
  if (inherits(fit, "glm")) {
    fd <- fit$family$dispersion
    fixed <- is.numeric(fd) && length(fd) == 1L && !is.na(fd)
    if (identical(vce, "model") && !fixed && !fit$family$family %in% c("binomial", "poisson")) {
      return(fit$df.residual)
    }
    return(Inf)
  }
  if (inherits(fit, "lm")) return(fit$df.residual)
  Inf
}

#' Wald estimates, p-values, and confidence intervals on the link scale
#'
#' Coefficients from `tt_coef()`, variance from `tt_vcov()`, reference
#' distribution from `tt_wald_df()` (`R/regtab_generics.R`); aliased
#' coefficients (NA) keep a row with every statistic missing.
#' `ci_method = "profile"` replaces the bounds with `confint()` (profile
#' likelihood for glm), leaving the Wald p-value in place; it is refused for
#' other classes and with a robust or cluster `vce`, and a failed profile is
#' an error, never a silent Wald fallback.
#' @param fit A fitted model.
#' @param conf.level Confidence level in (0, 1).
#' @param ci_method `"wald"` or `"profile"`.
#' @param vce,cluster See `tt_vcov()`.
#' @return data.frame: term, estimate, std.error, statistic, p.value,
#'   conf.low, conf.high.
#' @keywords internal
#' @noRd
tt_wald <- function(fit, conf.level = 0.95, ci_method = "wald", vce = "stata", cluster = NULL) {
  b <- tt_coef(fit)
  V <- as.matrix(tt_vcov(fit, vce, cluster))
  se <- rep(NA_real_, length(b))
  ok <- !is.na(b) & names(b) %in% rownames(V)
  se[ok] <- sqrt(diag(V)[names(b)[ok]])
  df <- tt_wald_df(fit, vce, cluster)
  stat <- b / se
  if (is.finite(df)) {
    p <- 2 * stats::pt(abs(stat), df, lower.tail = FALSE)
    q <- stats::qt((1 + conf.level) / 2, df)
  } else {
    p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    q <- stats::qnorm((1 + conf.level) / 2)
  }
  out <- data.frame(term = names(b), estimate = unname(b), std.error = se,
                    statistic = unname(stat), p.value = unname(p),
                    conf.low = unname(b - q * se), conf.high = unname(b + q * se),
                    stringsAsFactors = FALSE)
  if (identical(ci_method, "profile")) {
    .rt_check_profile(fit, vce)
    pf <- if (identical(vce, "stata")) .rt_aw_profile_fit(fit) else .rt_anchor(fit)
    ci <- tryCatch(.rt_quiet_nonint(suppressMessages(stats::confint(pf, level = conf.level))),
                   error = function(e) e)
    if (inherits(ci, "error")) {
      # Never fall back to the Wald bounds silently (external review F07).
      cli::cli_abort(c("Profile-likelihood intervals failed for this {.cls {class(fit)[1]}} fit.",
                       "i" = "Use {.code ci_method = \"wald\"} (Stata's intervals)."), parent = ci, call = NULL)
    }
    ci <- as.matrix(ci)
    hit <- match(out$term, rownames(ci))
    has <- !is.na(hit) & !is.na(out$estimate)
    if (any(!is.na(out$estimate) & is.na(hit))) {
      cli::cli_abort("Profile-likelihood intervals are missing for {.val {out$term[!is.na(out$estimate) & is.na(hit)]}}.",
                     call = NULL)
    }
    out$conf.low[has] <- ci[hit[has], 1]
    out$conf.high[has] <- ci[hit[has], 2]
  }
  out
}

# ci_method = "profile" is refused where it cannot be what it claims:
# a class whose rows are not built from confint() (external review F07;
# tt_ci_methods(): lm, where they are the t intervals, glm and glm.nb, MASS
# and stats profile()), and a robust, clustered or design-based variance,
# whose p-values would sit beside intervals of the model-based likelihood
# (review P0-3).
.rt_check_profile <- function(fit, vce, i = NULL) {
  cls <- if (is.data.frame(fit)) "data.frame" else class(fit)[1]
  where <- if (is.null(i)) "" else paste0(" (model ", i, ")")
  if (!"profile" %in% tt_ci_methods(fit)) {
    cli::cli_abort(c(
      "{.code ci_method = \"profile\"} is not available for {.cls {cls}} models{where}.",
      "i" = "Profile-likelihood intervals are available for {.fn lm}, {.fn glm} and {.fn MASS::glm.nb} fits; use {.code ci_method = \"wald\"} (Stata's intervals)."
    ), call = NULL)
  }
  if (vce %in% c("robust", "cluster", "user")) {
    cli::cli_abort(c(
      "{.code ci_method = \"profile\"} cannot be combined with {.code vce = \"{vce}\"}{where}.",
      "i" = "Profile-likelihood intervals use the model's likelihood, which ignores the robust variance; the p-values would not match the intervals.",
      "i" = "Use {.code ci_method = \"wald\"}: robust Wald intervals, as Stata reports them."
    ), call = NULL)
  }
  # A MASS::negative.binomial(theta) glm (review R04): profile.glm() takes
  # its default branch for this family name, the Pearson dispersion and an
  # F quantile, where the Wald interval (Stata's glm, family(nbinomial k))
  # has scale 1.
  if (!is.data.frame(fit) && !is.null(.rt_nbk_theta(fit))) {
    cli::cli_abort(c(
      "{.code ci_method = \"profile\"} is not available for a {.fn glm} with the {.fn MASS::negative.binomial} family{where}.",
      "i" = "R's profile of this family uses an estimated dispersion, while its Wald intervals have scale 1, as Stata's {.code glm, family(nbinomial k)}.",
      "i" = "Use {.code ci_method = \"wald\"}."
    ), call = NULL)
  }
  # Non-integer, non-constant weights are no longer refused (muse P1-3,
  # superseded by audit 2026-09-29 B07): under vce = "stata" they are
  # analytic weights, and the profile is that of Stata's glm [aweight]
  # likelihood, the one the Wald interval and log-likelihood beside it use
  # (.rt_aw_profile_fit()); regtab's inverse-probability-weights warning
  # still points to vce = "robust" for the [pweight] reading.
  invisible(TRUE)
}

# The fit a profile interval under vce = "stata" is taken from (audit
# 2026-09-29 B07): for a weighted binomial (0/1) or Poisson glm, the fit
# refitted with its weights rescaled to mean 1 over the positive weights
# (`weights = <weights> / mean`, evaluated as the call was), which is the
# likelihood Stata's glm [aweight] maximises and the one .rt_aw_scale() and
# .rt_glm_ll() read; confint() of the fit itself would profile the
# frequency-weight likelihood, sqrt(mean(w)) times narrower. Any other fit
# is profiled as it is (families with an estimated dispersion do not
# depend on the weights' scale; glm.nb's weights are nbreg [iweight]'s).
.rt_aw_profile_fit <- function(fit) {
  a <- .rt_anchor(fit)
  if (!inherits(a, "glm") || inherits(a, "negbin") || is.null(a$prior.weights)) return(a)
  K <- .rt_aw_scale(a, a$prior.weights, .rt_glm_y(a))
  if (identical(K, 1)) return(a)
  fail <- function(why) {
    cli::cli_abort(c(
      "The profile-likelihood interval of this weighted {.cls glm} refits it with its weights rescaled to mean 1 (Stata's {.code glm [aweight]}), and {why}.",
      "i" = "Use {.code ci_method = \"wald\"}, or refit with {.code weights = w / mean(w[w > 0])}."
    ), call = NULL)
  }
  cl <- tryCatch(stats::getCall(a), error = function(e) NULL)
  if (!is.call(cl) || is.null(cl$weights)) fail("the call gives no weights to rescale")
  cl$weights <- call("/", cl$weights, K)
  env <- environment(stats::formula(a)) %||% parent.frame()
  r <- tryCatch(.rt_quiet_nonint(eval(cl, env)), error = function(e) NULL)
  if (!inherits(r, "glm")) fail("that refit failed")
  b <- stats::coef(a)
  br <- stats::coef(r)
  se <- sqrt(pmax(diag(tt_vcov(a, "stata")), 0))[names(b)]
  same <- identical(names(br), names(b)) && identical(is.na(br), is.na(b)) &&
    length(r$prior.weights) == length(a$prior.weights) &&
    all(abs(br - b)[!is.na(b)] <= 1e-4 * pmax(se[!is.na(b)], 1e-12))
  if (!isTRUE(same)) fail("the refit does not reproduce the fit (have its data changed since it was fitted?)")
  # The weight column itself (review R03): a constant rescaling after
  # fitting leaves the coefficients alone but not the likelihood's scale.
  if (!isTRUE(all.equal(unname(as.vector(r$prior.weights)), unname(as.vector(a$prior.weights)) / K,
                        tolerance = 1e-10))) {
    fail("the refit's weights are not the fit's rescaled to mean 1 (has the weight column changed since it was fitted?)")
  }
  .rt_anchor(r)
}

# binomial()'s "non-integer #successes" warning: rescaled analytic weights
# are not integers by design.
.rt_quiet_nonint <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("non-integer #successes", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  })
}

# ---------------------------------------------------------------------------
# Variance of the coefficients (Phase 4 review P0-1, P0-3)
#
# `vce = "model"` is the fit's own vcov(). `vce = "stata"` (default)
# reproduces Stata's default vce(oim) for the classes whose Stata
# equivalent has been verified, and only for exactly those classes (a
# subclass such as survey::svyglm or mgcv::gam carries its own variance and
# is refused by regtab()):
# - `glm` with a canonical link (binomial-logit, poisson-log,
#   gaussian-identity, Gamma-inverse, inverse.gaussian-1/mu^2, and the quasi
#   versions): the Fisher information at the final estimates. It equals the
#   observed information for canonical links; R's vcov.glm() instead reuses
#   the working weights of the last IRLS step, formed at the previous
#   iterate (auto `logit foreign mpg weight`: intercept SE 4.5187086 in R vs
#   4.5187094448 in Stata and here).
# - `glm` with any other link (probit, cloglog, Gamma-log, ...): the
#   observed information -d2 l / d beta d beta' at the final estimates, the
#   default of Stata's probit, cloglog and glm (ML). Stata's glm divides by
#   the Pearson dispersion for the continuous families (scale(x2)), as R's
#   summary() does.
# - `negbin` (MASS::glm.nb, Stata nbreg): the beta block of the inverse of
#   the joint observed information over (beta, ln alpha), alpha = 1/theta.
# Weighted binomial/poisson glm: Stata glm [aweight], .rt_aw_scale().
# Everything else (lm, coxph, other classes) uses vcov().
# `vce = "robust"`/`"cluster"` (lm and glm only, tt_vce_types()): Stata's
# sandwich, .rt_vcov_sandwich() in R/regtab_vce.R.
#' @export
tt_vcov.default <- function(fit, vce = "stata", cluster = NULL, ...) {
  if (vce %in% c("robust", "cluster")) return(.rt_vcov_sandwich(fit, vce, cluster))
  V <- .rt_quiet_zero_weight(stats::vcov(fit))
  if (identical(vce, "model") || !class(fit)[1] %in% c("glm", "negbin")) return(V)
  # Stata's observed information needs the fit's own rows: the fit-time
  # frame and response (review P0-1; external review F05: glm(y = FALSE)
  # used to fall back to vcov() silently).
  fit <- .rt_require_frame(fit, "variance")
  b <- stats::coef(fit)
  ok <- !is.na(b)
  X <- stats::model.matrix(fit)
  nm <- names(b)[ok]
  if (!all(nm %in% colnames(X))) {
    cli::cli_abort("The model matrix of this {.cls {class(fit)[1]}} fit does not hold its coefficients {.val {setdiff(nm, colnames(X))}}.",
                   call = NULL)
  }
  X <- X[, nm, drop = FALSE]
  eta <- drop(X %*% b[ok])
  if (!is.null(fit$offset)) eta <- eta + fit$offset
  pw <- fit$prior.weights
  y <- .rt_glm_y(fit)
  if (length(pw) != nrow(X) || length(y) != nrow(X)) {
    cli::cli_abort("The weights or response of this {.cls {class(fit)[1]}} fit do not match its model matrix.", call = NULL)
  }
  Vn <- if (inherits(fit, "negbin")) .rt_vcov_negbin(fit, X, eta, y, pw) else .rt_vcov_glm(fit, X, eta, y, pw)
  if (!is.null(Vn) && !inherits(fit, "negbin")) Vn <- Vn * .rt_aw_scale(fit, pw, y)
  if (is.null(Vn)) {
    # A numerical failure (singular or non-finite information, no
    # dispersion): never substitute vcov() silently (external review F05;
    # Milestone H decision H-D6).
    cli::cli_abort(c(
      "Stata's observed-information variance cannot be computed for this {.cls {class(fit)[1]}} fit (singular or non-finite information).",
      "i" = "Use {.code vce = \"model\"} for the fit's own {.fn vcov}."
    ), call = NULL)
  }
  if (!.rt_is_pd(Vn)) {
    # Away from the maximum (a fit that did not converge) the observed
    # information of a non-canonical link can be indefinite; its inverse
    # gives negative variances and blank intervals (muse P1-1). Stata stops
    # with "Hessian is not negative semidefinite" there.
    cli::cli_abort(c(
      "Stata's observed-information variance of this {.cls {class(fit)[1]}} fit is not positive definite.",
      "i" = "The estimates are not at a maximum of the likelihood: check that the fit converged ({.code fit$converged}).",
      "i" = "Use {.code vce = \"model\"} for the fit's own {.fn vcov}."
    ), call = NULL)
  }
  V[nm, nm] <- Vn
  V
}

# A symmetric variance matrix with every eigenvalue positive and finite.
.rt_is_pd <- function(V) {
  if (!all(is.finite(V))) return(FALSE)
  ev <- eigen((V + t(V)) / 2, symmetric = TRUE, only.values = TRUE)$values
  all(ev > 0)
}

# summary.glm() warns that zero-weight observations are not used for the
# dispersion, which is what Stata does too ([pweight] zeros leave the
# estimation sample): not a problem to report (review P2-8).
.rt_quiet_zero_weight <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("zero weight not used for calculating dispersion", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

# Canonical family/link pairs: observed = expected information.
.rt_canonical <- function(family, link) {
  canon <- c(binomial = "logit", quasibinomial = "logit", poisson = "log", quasipoisson = "log",
             gaussian = "identity", Gamma = "inverse", inverse.gaussian = "1/mu^2")
  family %in% names(canon) && identical(unname(canon[family]), link)
}

# d2 mu / d eta2 for R's standard links (NULL for others).
.rt_link_d2 <- function(link, eta) {
  switch(link,
    identity = rep(0, length(eta)),
    log = exp(eta),
    logit = {
      m <- stats::plogis(eta)
      m * (1 - m) * (1 - 2 * m)
    },
    probit = -eta * stats::dnorm(eta),
    cloglog = {
      e <- exp(eta)
      exp(eta - e) * (1 - e)
    },
    cauchit = -2 * eta / (pi * (1 + eta^2)^2),
    inverse = 2 / eta^3,
    sqrt = rep(2, length(eta)),
    "1/mu^2" = 0.75 * eta^(-2.5),
    NULL
  )
}

# d V(mu) / d mu for R's standard variance functions (NULL for others).
.rt_var_d1 <- function(family, mu) {
  switch(family,
    binomial = , quasibinomial = 1 - 2 * mu,
    poisson = , quasipoisson = rep(1, length(mu)),
    gaussian = rep(0, length(mu)),
    Gamma = 2 * mu,
    inverse.gaussian = 3 * mu^2,
    NULL
  )
}

.rt_vcov_glm <- function(fit, X, eta, y, pw) {
  fam <- fit$family
  mu <- fam$linkinv(eta)
  d1 <- fam$mu.eta(eta)
  vmu <- fam$variance(mu)
  th <- .rt_nbk_theta(fit)
  if (.rt_canonical(fam$family, fam$link)) {
    w <- pw * d1^2 / vmu
  } else {
    d2 <- .rt_link_d2(fam$link, eta)
    vd <- if (!is.null(th)) 1 + 2 * mu / th else .rt_var_d1(fam$family, mu)
    if (!is.null(d2) && !is.null(vd)) {
      # -d/d eta of the score pw (y - mu) mu' / V(mu).
      w <- pw * (d1^2 / vmu - (y - mu) * (d2 / vmu - d1^2 * vd / vmu^2))
    } else {
      # A link or variance function without an analytic derivative here:
      # central difference of the score in eta.
      s <- function(e) {
        m <- fam$linkinv(e)
        pw * (y - m) * fam$mu.eta(e) / fam$variance(m)
      }
      h <- 1e-5 * pmax(1, abs(eta))
      w <- -(s(eta + h) - s(eta - h)) / (2 * h)
    }
  }
  if (anyNA(w)) return(NULL)
  # Stata's glm, family(nbinomial k) is fixed-scale too (audit 2026-09-29
  # B08; summary.glm() estimates a Pearson dispersion for it).
  disp <- if (fam$family %in% c("binomial", "poisson") || !is.null(th)) 1 else {
    d <- tryCatch(.rt_quiet_zero_weight(summary(fit)$dispersion), error = function(e) NA_real_)
    if (is.na(d)) return(NULL)
    d
  }
  Vn <- tryCatch(solve(crossprod(X, X * w)), error = function(e) NULL)
  if (is.null(Vn)) return(NULL)
  Vn * disp
}

# Stata's glm [aweight] for a fixed-scale family (binomial without trials,
# poisson; audit 2026-09-29 B01): glm rescales analytic weights to mean 1
# over the estimation sample (positive weights), so its observed-information
# variance is the raw-weight one times mean(w), as .rt_glm_ll() rescales the
# log-likelihood (auto, glm foreign mpg weight [aw=turn], family(binomial):
# se(mpg) .0988614 in Stata, .0157005 = [fw]/[iw] from the raw weights). The
# same holds for a MASS::negative.binomial(theta) glm (Stata glm,
# family(nbinomial k) [aw], which Stata allows; B08). The
# families with an estimated dispersion (gaussian, Gamma, inverse Gaussian,
# quasi) need nothing: the dispersion absorbs the weights' scale. A binomial
# with trials (a proportion or cbind() response) keeps its weights, which
# are the trials; glm.nb is nbreg, which has no [aweight] (its weights are
# nbreg [iweight]'s, the reading its log-likelihood uses too; probe
# 2026-09-29: nbreg/glm, family(nbinomial ml) refuse aweights).
.rt_aw_scale <- function(fit, pw, y) {
  fam <- fit$family$family
  if (!fam %in% c("binomial", "poisson") && is.null(.rt_nbk_theta(fit))) return(1)
  pos <- pw > 0
  if (!any(pos) || all(pw[pos] == 1)) return(1)
  if (fam == "binomial" && .rt_binom_trials(fit, y, pw)) return(1)
  mean(pw[pos])
}

# Whether a binomial glm's weights are its trials (review R01): a
# two-column cbind(successes, failures) response, whatever its values (all
# its groups may be all-or-nothing, which leaves fit$y all 0/1), or a
# proportion response with a value strictly between 0 and 1 (weights =
# trials). A proportion response whose values are all 0/1 cannot be told
# from 0/1 data with analytic weights, and is read as the latter.
.rt_binom_trials <- function(fit, y = .rt_glm_y(fit), pw = fit$prior.weights) {
  mf <- if (is.data.frame(fit$model)) fit$model else tryCatch({
    a <- .rt_anchor(fit)
    if (is.data.frame(a$model)) a$model else NULL
  }, error = function(e) NULL)
  resp <- if (is.data.frame(mf)) tryCatch(stats::model.response(mf), error = function(e) NULL) else NULL
  if (is.matrix(resp) && ncol(resp) == 2L) return(TRUE)
  pos <- if (length(pw) == length(y)) pw > 0 else rep(TRUE, length(y))
  !all(as.vector(y)[pos] %in% c(0, 1))
}

# theta of a glm fitted with the MASS::negative.binomial(theta) family (a
# known theta: Stata's glm, family(nbinomial 1/theta)), NULL otherwise and
# for glm.nb fits (class negbin, theta estimated: nbreg).
.rt_nbk_theta <- function(fit) {
  if (inherits(fit, "negbin")) return(NULL)
  fam <- fit$family
  if (!is.list(fam) || !is.character(fam$family) || !startsWith(fam$family, "Negative Binomial(")) return(NULL)
  th <- tryCatch(get(".Theta", envir = environment(fam$variance), inherits = FALSE), error = function(e) NULL)
  if (is.numeric(th) && length(th) == 1L && is.finite(th) && th > 0) th else NULL
}

# Negative binomial (NB2): log-likelihood per observation
#   l = lgamma(y + th) - lgamma(th) - lgamma(y + 1) + th log(th / (th + mu))
#       + y log(mu / (th + mu)),  th = 1 / alpha = exp(-tau),
# differentiated analytically in (eta, tau) and weighted by the prior
# weights; beta block of the inverse of the negative joint Hessian.
.rt_vcov_negbin <- function(fit, X, eta, y, pw, joint = FALSE, fixed = .rt_nb_glm(fit)) {
  th <- fit$theta
  if (is.null(th) || !is.finite(th) || th <= 0) return(NULL)
  fam <- fit$family
  mu <- fam$linkinv(eta)
  d1 <- fam$mu.eta(eta)
  d2 <- .rt_link_d2(fam$link, eta)
  if (is.null(d2)) return(NULL)
  l_mu <- y / mu - (y + th) / (th + mu)
  l_mumu <- -y / mu^2 + (y + th) / (th + mu)^2
  l_ee <- l_mumu * d1^2 + l_mu * d2
  l_eth <- d1 * (y - mu) / (th + mu)^2
  l_th <- digamma(y + th) - digamma(th) + log(th / (th + mu)) + (mu - y) / (th + mu)
  l_thth <- trigamma(y + th) - trigamma(th) + 1 / th - 1 / (th + mu) - (mu - y) / (th + mu)^2
  # tau = ln(alpha) = -ln(th): d th / d tau = -th.
  l_et <- -th * l_eth
  l_tt <- th^2 * l_thth + th * l_th
  H <- rbind(cbind(crossprod(X, X * (pw * l_ee)), crossprod(X, pw * l_et)),
             cbind(crossprod(pw * l_et, X), sum(pw * l_tt)))
  if (anyNA(H)) return(NULL)
  k <- ncol(X)
  # Stata's glm, family(nbinomial ml) (a non-log glm.nb): alpha "treated as
  # fixed once estimated", so the OIM variance of beta alone (muse M4).
  if (isTRUE(fixed) && !isTRUE(joint)) {
    return(tryCatch(solve(-H[seq_len(k), seq_len(k), drop = FALSE]), error = function(e) NULL))
  }
  Vj <- tryCatch(solve(-H), error = function(e) NULL)
  if (is.null(Vj)) return(NULL)
  if (isTRUE(joint)) {
    dimnames(Vj) <- list(c(colnames(X), "lnalpha"), c(colnames(X), "lnalpha"))
    return(Vj)
  }
  Vj[seq_len(k), seq_len(k), drop = FALSE]
}

# A glm.nb fit with a link other than log: Stata's glm, family(nbinomial
# ml) link(), not nbreg (muse M4; probe 2026-09-28). Stata takes alpha from
# nbreg (log link) and fits the glm at that alpha, "treated as fixed once
# estimated": no /lnalpha row, e(rank) counts the coefficients only, and
# the variance is beta's at fixed alpha. glm.nb() estimates theta jointly
# with the identity-link coefficients, so its estimates differ from Stata's
# (1e-5 relative on the unweighted probe data), while the layout, rank and
# variance follow Stata's. The gap grows with weights, since the weights
# move glm.nb's joint theta away from nbreg's (audit 2026-09-29 B09: auto,
# glm price mpg weight [iw=turn], family(nbinomial ml) link(identity):
# alpha .11219 in Stata, 1/theta .11839 in R; se(mpg) 7.855 vs 8.069, 3%).
.rt_nb_glm <- function(fit) inherits(fit, "negbin") && !identical(fit$family$link %||% "log", "log")

# Joint inverse observed information of a glm.nb fit over (beta, ln alpha),
# named, or NULL.
.rt_negbin_joint <- function(fit) {
  b <- stats::coef(fit)
  ok <- !is.na(b)
  X <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  if (is.null(X) || !all(names(b)[ok] %in% colnames(X))) return(NULL)
  X <- X[, names(b)[ok], drop = FALSE]
  eta <- drop(X %*% b[ok])
  if (!is.null(fit$offset)) eta <- eta + fit$offset
  y <- .rt_glm_y(fit)
  if (length(fit$prior.weights) != nrow(X) || length(y) != nrow(X)) return(NULL)
  .rt_vcov_negbin(fit, X, eta, y, fit$prior.weights, joint = TRUE)
}

# nbreg's /lnalpha and its alpha diparm (Phase 5 review P0-1; probes C31,
# G19): between the covariates and the intercept, lnalpha = ln(1/theta)
# with its Wald interval from the joint information, alpha = exp() of the
# estimate and bounds; nbreg reports no test for either, so no p-value.
# nointercept drops both (their labels). vce = "model": MASS's SE of theta
# by the delta method.
#' @export
tt_regtab_ancillary_rows.negbin <- function(fit, info, level = 0.95, vce = "stata", cluster = NULL, ...) {
  if (.rt_nb_glm(fit)) return(NULL)
  th <- fit$theta
  if (is.null(th) || !is.finite(th) || th <= 0) return(NULL)
  est <- -log(th)
  se <- if (identical(vce, "model")) {
    if (is.null(fit$SE.theta)) NA_real_ else fit$SE.theta / th
  } else if (vce %in% c("robust", "cluster")) {
    # nbreg [pw]/vce(robust)/vce(cluster): the joint sandwich (review P0-2).
    sqrt(.rt_vcov_sandwich(fit, vce, cluster, joint = TRUE)["lnalpha", "lnalpha"])
  } else {
    Vj <- .rt_negbin_joint(fit)
    if (is.null(Vj)) NA_real_ else sqrt(Vj["lnalpha", "lnalpha"])
  }
  z <- stats::qnorm((1 + level) / 2)
  w <- data.frame(term = "lnalpha", estimate = est, std.error = se, statistic = est / se, p.value = NA_real_,
                  conf.low = est - z * se, conf.high = est + z * se, stringsAsFactors = FALSE)
  rbind(.rt_anc_row("lnalpha", "lnalpha", w), .rt_anc_row("alpha", "alpha", w, transform = "exp"))
}

# tidy_fun for broom.helpers::tidy_plus_plus(): the Wald table above.
.rt_tidy_fun <- function(x, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE,
                         ci_method = "wald", vce = "stata", cluster = NULL, ...) {
  tt_wald(x, conf.level = conf.level, ci_method = ci_method, vce = vce, cluster = cluster)
}

# Perfect prediction (review P2-4). Stata's logit/logistic/poisson drop the
# observations a covariate pattern predicts perfectly and show those levels
# as Empty/Omitted (QA v1015 C: `logit foreign mpg i.rep78`). R's glm()
# keeps them and returns the "estimate at infinity": a huge coefficient
# whose standard error is larger still, which prints as an enormous ratio
# with a blank CI. regtab cannot refit on a reduced sample, so it warns.
# On the link scale of a binomial or Poisson model a standard error above
# 100 has no other plausible source.
.rt_warn_separation <- function(fit, wald) {
  if (!inherits(fit, "glm") || inherits(fit, "negbin")) return(invisible(NULL))
  if (!fit$family$family %in% c("binomial", "quasibinomial", "poisson", "quasipoisson")) {
    return(invisible(NULL))
  }
  bad <- wald$term[!is.na(wald$std.error) & wald$std.error > 100]
  if (length(bad)) {
    cli::cli_warn(c(
      "Coefficient{?s} {.val {bad}} {?has/have} a standard error above 100: the model shows (quasi-)complete separation.",
      "i" = "Stata drops perfectly predicted observations (and labels their levels Empty or Omitted); {.fn glm} keeps them, so these rows show meaningless estimates.",
      "i" = "Drop the perfect predictors, merge sparse levels, or use a penalised fit."
    ))
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Labels and codes

# Stata code of each factor level, used in raw keys ("2.rep78"). A factor
# that keeps its Stata value labels as a "labels" attribute (named codes, as
# haven/labelled store them, and as tt_as_factor() keeps them) uses those
# codes; otherwise a level whose text is an integer is that integer
# (unlabelled numeric codes); a logical is 0/1; a character predictor takes
# its sorted level's position (Stata's `encode` numbers sorted strings
# 1, 2, ...); otherwise the level's position, flagged in the "positional"
# attribute, because a Stata variable would carry its own codes (0/1 for
# `foreign`), which regtab cannot know (review P1-1).
.rt_codes <- function(x) {
  # Character predictors: the level order R's model.matrix() used.
  lev <- .rt_levels(x)
  lc <- .rt_level_codes(lev, attr(x, "labels", exact = TRUE), logical = is.logical(x))
  out <- stats::setNames(lc$code, lev)
  attr(out, "positional") <- any(lc$positional) && is.factor(x)
  out
}

# Codes of levels `lev` (the core of .rt_codes(), shared with effecttab's
# .et_level_codes() so both key a factor alike): a value label's code
# (`vl`, named numeric codes), else an integer text's value
# (.rt_int_codes()), else 0/1 for a logical, else the level's position,
# flagged in `positional`. Integer values and positions that would give two
# levels one code (levels "a", "1", "2": 1, 1, 2) leave every unlabelled
# level at its position (audit 2026-09-29 B06).
.rt_level_codes <- function(lev, vl = NULL, logical = FALSE) {
  code <- rep(NA_character_, length(lev))
  if (!is.null(vl) && !is.null(names(vl)) && is.numeric(vl)) {
    hit <- match(lev, names(vl))
    code[!is.na(hit)] <- stata_macro_text(unname(unclass(vl))[hit[!is.na(hit)]])
  }
  lab <- !is.na(code)
  code[!lab] <- .rt_int_codes(lev[!lab])
  if (logical) code[is.na(code)] <- c("0", "1")[is.na(code)]
  pos <- is.na(code)
  code[pos] <- as.character(seq_along(lev))[pos]
  if (!logical && anyDuplicated(code)) {
    pos <- !lab
    code[pos] <- as.character(seq_along(lev))[pos]
  }
  list(code = code, positional = pos)
}

# Integer level texts are their value, as Stata reads a destring-ed or
# encoded code: "01" is 1 (muse P2-5), unless that would give two levels one
# code (then each keeps its text); NA for any other text. Shared by
# .rt_codes() and effecttab's .et_levels() (audit 2026-09-29 B03).
.rt_int_codes <- function(lev) {
  code <- rep(NA_character_, length(lev))
  int <- grepl("^-?[0-9]+$", lev)
  num <- sub("^(-?)0+([0-9])", "\\1\\2", lev[int])
  num[num == "-0"] <- "0"
  code[int] <- if (anyDuplicated(num)) lev[int] else num
  code
}

# Levels in the order model.matrix() uses them.
.rt_levels <- function(x) {
  if (is.factor(x)) levels(x) else if (is.logical(x)) c("FALSE", "TRUE") else levels(factor(x))
}

# Text a level is displayed with: a logical is an unlabelled 0/1 variable
# in Stata (review P3-3), so its levels read "0"/"1".
.rt_level_text <- function(x, lev) {
  if (is.logical(x)) c("FALSE" = "0", "TRUE" = "1")[[lev]] else lev
}

# Whether a factor's levels count as value labels for fvgen: bare integer
# level texts and logicals are unlabelled numeric codes.
.rt_is_labelled <- function(x, codes) {
  !is.logical(x) && !all(grepl("^-?[0-9]+$", names(codes)))
}

# Reference level of a factor under its contrasts (review P0-6). regtab's
# layout (a Reference row, levels as contrasts against it) holds only for
# treatment-type contrasts: every column an indicator of one level, one
# level with no column. contr.treatment (any base) and contr.SAS qualify;
# ordered factors' contr.poly, contr.sum, contr.helmert, and custom
# matrices do not, and are refused rather than shown with a false
# Reference row.
.rt_contrast_base <- function(ctr, x, v) {
  lev <- .rt_levels(x)
  cm <- .rt_contrast_matrix(ctr, lev)
  if (!.rt_is_treatment(cm, lev)) {
    what <- if (is.character(ctr) && length(ctr) == 1L) ctr else "custom"
    if (is.ordered(x) && identical(what, "contr.poly")) {
      cli::cli_abort(c(
        "{.var {v}} is an ordered factor; its polynomial contrasts ({.fn contr.poly}) have no Stata equivalent.",
        "i" = "Convert it with {.code factor({v}, ordered = FALSE)} to show each level against a reference level."
      ), call = NULL)
    }
    cli::cli_abort(c(
      "{.var {v}} uses {.val {what}} contrasts; {.fn regtab} shows factor levels against a reference level, which needs treatment contrasts.",
      "i" = "Refit with {.code contr.treatment} (or {.code contr.SAS}) for {.var {v}}."
    ), call = NULL)
  }
  lev[rowSums(cm) == 0][1]
}

# The contrast matrix a design recorded for a factor (attr(mm,
# "contrasts")): a matrix (`contrasts(x) <- contr.treatment(3, base = 2)`),
# or a contrast function or its name evaluated at the levels; NULL if none.
.rt_contrast_matrix <- function(ctr, lev) {
  if (is.matrix(ctr)) {
    ctr
  } else if (is.function(ctr)) {
    tryCatch(ctr(lev), error = function(e) NULL)
  } else if (is.character(ctr) && length(ctr) == 1L) {
    fn <- tryCatch(match.fun(ctr), error = function(e) NULL)
    if (is.null(fn)) NULL else tryCatch(fn(lev), error = function(e) NULL)
  } else NULL
}

# Treatment-type coding: every column an indicator of one level, one level
# with no column.
.rt_is_treatment <- function(cm, lev) {
  is.matrix(cm) && nrow(cm) == length(lev) && ncol(cm) == length(lev) - 1L &&
    all(cm %in% c(0, 1)) && all(colSums(cm) == 1) && all(rowSums(cm) <= 1)
}

# Coefficient-name suffix of each level under treatment-type contrasts
# (audit 2026-09-29 B02): the name of the column that indicates the level,
# as model.matrix() names it (the column's name, or its number when the
# matrix has none: contr.treatment(3, base = 2) names its columns "1" and
# "3", so level "a" is g1 and "c" g3); NA for the base level. NULL when the
# coding is not treatment-type or unknown.
.rt_contrast_suffix <- function(ctr, x) {
  lev <- .rt_levels(x)
  cm <- .rt_contrast_matrix(ctr, lev)
  if (!.rt_is_treatment(cm, lev)) return(NULL)
  cn <- colnames(cm) %||% as.character(seq_len(ncol(cm)))
  col <- apply(cm, 1L, function(r) if (any(r == 1)) which(r == 1)[1] else NA_integer_)
  stats::setNames(cn[col], lev)
}

# A factor main effect's level rows (level, coefficient, reference) mapped
# through its contrast matrix (review R02): broom.helpers pairs coefficient
# g1 with the level whose text is "1", or else with position 1, which is
# wrong for contrasts(g) <- contr.treatment(3, base = 3) on factor(0:2) (g1
# is level "0") or for a matrix without column names (model.matrix()
# numbers its columns, g2 = the second column, which may indicate level 3).
# The level a column indicates is its row with a 1 (.rt_contrast_suffix()).
# broom.helpers' rows are kept when they already pair so; a mapping whose
# coefficient names the fit lacks is refused, never shown mislabelled.
.rt_contrast_rows <- function(v, rows, wald, mf, mm) {
  ctr <- if (!is.null(mm)) attr(mm, "contrasts") else NULL
  tf <- tryCatch(attr(attr(mf, "terms"), "factors"), error = function(e) NULL)
  if (is.null(ctr[[v]]) || !is.matrix(tf) || !v %in% rownames(tf) || !v %in% colnames(tf) ||
      !identical(unname(tf[v, v]), 1L)) {
    return(rows)
  }
  suf <- .rt_contrast_suffix(ctr[[v]], mf[[v]])
  if (is.null(suf)) return(rows)
  lev <- names(suf)
  term <- ifelse(is.na(suf), NA_character_, paste0(v, suf))
  ref <- is.na(suf)
  # Without an intercept the first factor is coded in full (one column per
  # level, named by it, no reference) although attr(terms, "factors") says
  # contrasts.
  asg <- attr(mm, "assign")
  if (!is.null(asg) && sum(asg == match(v, colnames(tf))) == length(lev)) {
    term <- paste0(v, lev)
    ref <- rep(FALSE, length(lev))
  }
  got_ref <- rows$label[rows$reference_row %in% TRUE]
  got <- stats::setNames(rows$term, rows$label)[!(rows$reference_row %in% TRUE)]
  want <- stats::setNames(term, lev)[!ref]
  if (identical(sort(got_ref), sort(lev[ref])) && setequal(names(got), names(want)) &&
      identical(unname(got[names(want)]), unname(want))) {
    return(rows)
  }
  miss <- setdiff(want, wald$term)
  if (length(miss)) {
    cli::cli_abort(c(
      "The contrast matrix of {.var {v}} cannot be matched to the model's coefficients: {.val {miss}} {?is/are} not among them.",
      "i" = "Set the reference level with {.fn relevel} or {.code contrasts = list({v} = \"contr.treatment\")} and refit."
    ), call = NULL)
  }
  out <- rows[rep(1L, length(lev)), , drop = FALSE]
  out$label <- lev
  out$term <- ifelse(ref, rows$term[1], term)
  out$reference_row <- ref
  rownames(out) <- NULL
  out
}

# fvgen's label for one factor level (`fvgen.ado:777-785`): the value label,
# or `var=level` when the variable carries no value labels. An R factor
# always has level labels; one whose level texts are bare integers is taken
# as an unlabelled numeric variable.
.rt_fv_partlabel <- function(var, level, labelled) {
  if (labelled) level else paste0(var, "=", level)
}

# fvgen stores every generated label as a Stata variable label, cut to 80
# characters (`fvgen.ado:793-811`, `usubstr(lab, 1, 80)`; review P0-5).
# regtab displays the stored label without the trailing blank a cut can
# leave (autolong: "... driving cycle in " shows as "... driving cycle in").
.rt_fv80 <- function(s) ifelse(nchar(s) > 80L, sub("[[:space:]]+$", "", substr(s, 1L, 80L)), s)

# Variable label with Stata's fallback to the name.
.rt_var_label <- function(mf, v) {
  x <- mf[[v]]
  if (is.null(x)) return(v)
  var_label(x, v)
}

# Label of x in an `I(x^2)` term (review P0-7): x's own column when it is in
# the model frame, else the label the `I(x^2)` column inherited from x.
.rt_square_label <- function(mf, sq, term) {
  if (!is.null(mf[[sq]])) return(.rt_var_label(mf, sq))
  if (!is.null(mf[[term]])) return(var_label(mf[[term]], sq))
  sq
}

# `I(x^2)`: the continuous self-interaction c.x#c.x.
.rt_square_of <- function(term) {
  m <- regmatches(term, regexec("^I\\(\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*\\^\\s*2\\s*\\)$", term))[[1]]
  if (length(m)) m[2] else NA_character_
}

# ---------------------------------------------------------------------------
# Per-model rows

# Model frame and design matrix used for empty-cell detection.
.rt_frame <- function(fit) {
  # model.frame() of an nlme::lme fit partially matches `$modelStruct`: its
  # checked frame and design instead (task 5.19).
  if (inherits(fit, "lme")) {
    pr <- .rt_lme_prep(fit)
    return(list(mf = pr$frame, mm = pr$X))
  }
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
  list(mf = .rt_restore_attrs(mf, fit), mm = mm)
}

# A model frame built with `subset =` loses the columns' "label" and
# "labels" attributes (na.action keeps them). Copy them back from the data
# the model was fitted on: `fit$data` (glm) or the call's `data` argument
# evaluated in the formula's environment.
.rt_restore_attrs <- function(mf, fit, warn = TRUE) {
  if (is.null(mf)) return(mf)
  need <- names(mf)[vapply(names(mf), function(v) {
    is.null(attr(mf[[v]], "label", exact = TRUE)) && is.null(attr(mf[[v]], "labels", exact = TRUE))
  }, TRUE)]
  if (!length(need)) return(mf)
  # `$` errors on S4 fits (lme4's merMod).
  fdata <- if (isS4(fit)) NULL else fit$data
  src <- if (is.data.frame(fdata)) fdata else {
    cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
    env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
    if (is.null(cl) || is.null(cl$data) || is.null(env)) NULL else
      tryCatch(eval(cl$data, env), error = function(e) NULL)
  }
  if (!is.data.frame(src)) return(mf)
  # The data are re-read at regtab() time (lm keeps no copy), so they may
  # have changed since the fit: restore only from rows and values that
  # still match the model frame (review P2-3).
  rows <- rownames(mf)
  # A coxph() fit with tt() terms has its model frame split at the event
  # times (rows "6", "6.1", "6.2", ... of data row "6"): map each split row
  # to its data row (review P3-8 of group t2a; the data were unchanged).
  if (inherits(fit, "coxph") && !is.null(rows) && !is.null(tryCatch(stats::getCall(fit)$tt, error = function(e) NULL))) {
    split <- !rows %in% rownames(src)
    rows[split] <- sub("\\.[0-9]+$", "", rows[split])
  }
  covered <- !is.null(rows) && all(rows %in% rownames(src))
  stale <- character()
  for (v in need) {
    # An `I(x^2)` column inherits x's label.
    sv <- if (v %in% names(src)) v else .rt_square_of(v)
    if (is.na(sv) || !sv %in% names(src)) next
    same <- covered && tryCatch({
      # By position, not src[rows, sv]: on a tibble (every haven::read_dta()
      # result) that is a one-column tibble, never equal to the frame's
      # column (pre-release review P1-1).
      a <- src[[sv]][match(rows, rownames(src))]
      if (sv != v) a <- a^2
      b <- mf[[v]]
      txt <- function(x) if (is.factor(x)) as.character(x) else as.character(unclass(x))
      # lm() drops unused factor levels from the frame, so compare texts.
      identical(unname(txt(a)), unname(txt(b))) ||
        (is.numeric(a) && is.numeric(b) && isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE)))
    }, error = function(e) FALSE)
    if (!same) {
      if (any(vapply(c("label", "labels"), function(a) !is.null(attr(src[[sv]], a, exact = TRUE)), TRUE)) ||
          !covered) stale <- c(stale, v)
      next
    }
    for (a in c("label", "labels")) {
      if (a == "labels" && sv != v) next
      val <- attr(src[[sv]], a, exact = TRUE)
      if (!is.null(val)) attr(mf[[v]], a) <- val
    }
  }
  if (length(stale) && warn) {
    cli::cli_warn(c(
      "Variable labels for {.var {stale}} could not be restored: the model's data no longer match its model frame.",
      "i" = "Labels are read from the data at {.fn regtab} time; call {.fn regtab} before modifying the data, or fit with {.fn glm}, which keeps a copy."
    ))
  }
  mf
}

# A regtab row. `role` is the row's structural kind (Milestone H task H5,
# decision H-D2), set by the adapter from the fit's coefficient structure,
# never from its label: "coef" (a covariate), "intercept", "cutpoint"
# (ordered models), "ancillary" (a parameter of Stata's `/` equation that
# nointercept drops with the intercepts: ln_p, p, 1/p, lnalpha, alpha, and
# every "Ancillary:"/"Scale:" row of the multi-equation layout),
# "auxiliary" (an ancillary parameter nointercept keeps, as Stata's rule
# does not know its name: lnsigma, sigma, lngamma, gamma), or "re" (a
# random-effects row). Default: "intercept" for kind "intercept", "re" for
# kind "re", else "coef".
.rt_row <- function(key, block, kind, label, status = "est", term = NA_character_,
                    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
                    p.value = NA_real_, sub = 0, ancillary = FALSE, role = NULL) {
  if (is.null(role)) {
    role <- rep_len("coef", length(kind))
    role[kind %in% "intercept"] <- "intercept"
    role[kind %in% "re"] <- "re"
  }
  data.frame(key = key, block = block, kind = kind, label = label, status = status,
             term = term, estimate = estimate, conf.low = conf.low, conf.high = conf.high,
             p.value = p.value, sub = sub, ancillary = ancillary, role = role, stringsAsFactors = FALSE)
}

# Roles nointercept drops (`regtab.ado:1618-1632` drops Stata's intercepts,
# cutpoints and the ancillary parameters its label rule knows; R reads the
# same set from the rows' structure).
.rt_noint_roles <- c("intercept", "cutpoint", "ancillary")

# A factor in an interaction term without its lower-order terms (`y ~ f:g`,
# `y ~ x + x:f`) is coded by R with a dummy for every level, and when that
# full set is collinear with the intercept or another term R aliases its
# last column, so every other cell is a contrast with the last one and the
# last shows as Omitted; Stata keeps its base cell instead (probe
# 2026-09-28, auto: `regress price i.foreign#i.rep78` has 0b.foreign#1b.rep78
# as base and 1.foreign#5.rep78 = 1728.17, where R's lm(price ~
# foreign:rep78) shows 0.foreign#1.rep78 = -1728.17 and 1#5 Omitted; muse
# P2-3). Refused; a full set that is not collinear (`y ~ f:x`, one slope
# per level, as Stata's i.f#c.x) is fine.
.rt_check_marginal_coding <- function(fit, mf, mm) {
  if (!is.data.frame(mf) || !is.matrix(mm)) return(invisible(TRUE))
  tf <- tryCatch(attr(stats::terms(fit), "factors"), error = function(e) NULL)
  asg <- attr(mm, "assign")
  b <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (!is.matrix(tf) || is.null(asg) || length(asg) != ncol(mm) || is.null(b) || is.matrix(b)) {
    return(invisible(TRUE))
  }
  is_fac <- vapply(rownames(tf), function(v) {
    x <- mf[[v]]
    !is.null(x) && (is.factor(x) || is.character(x) || is.logical(x))
  }, TRUE)
  for (j in seq_len(ncol(tf))) {
    full <- rownames(tf)[is_fac & tf[, j] == 2L]
    if (!length(full)) next
    cols <- colnames(mm)[asg == j]
    cols <- cols[cols %in% names(b)]
    aliased <- cols[is.na(b[cols]) & colSums(mm[, cols, drop = FALSE] != 0) > 0]
    if (length(aliased)) {
      cli::cli_abort(c(
        "The term {.code {colnames(tf)[j]}} uses {.var {full}} without {?its/their} main effect{?s}: R codes every level and drops the last cell ({.val {aliased[1]}}) as collinear, so its rows are contrasts with that cell, not with Stata's base.",
        "i" = "Include the main effects ({.code {paste(rownames(tf)[tf[, j] > 0], collapse = ' * ')}}), as Stata's {.code i.a##i.b}, or fit one combined factor ({.fn interaction})."
      ), call = NULL)
    }
  }
  invisible(TRUE)
}

# Rows of the model frame with a positive weight, or NULL when every row has
# one (or the frame has no weights, or does not match the design).
.rt_pos_weight_rows <- function(mf, mm) {
  if (!is.data.frame(mf) || !is.matrix(mm) || !identical(nrow(mf), nrow(mm))) return(NULL)
  w <- tryCatch(stats::model.weights(mf), error = function(e) NULL)
  if (!is.numeric(w) || length(w) != nrow(mf) || !any(w <= 0, na.rm = TRUE)) return(NULL)
  !is.na(w) & w > 0
}

# The part of a model-frame column in the estimation sample (positive
# weights, .rt_pos_weight_rows()).
.rt_pos_rows <- function(x, mf) {
  p <- attr(mf, "tt_pos")
  if (is.null(p) || length(p) != length(x)) x else x[p]
}

# Status of an estimated coefficient cell: "est", or, when R aliased it
# (NA), "empty" if its design column is all zero (a level or level
# combination with no observations) and "omit" otherwise (collinearity).
.rt_na_status <- function(term, est, mm) {
  if (!is.na(est)) return("est")
  if (!is.null(mm) && term %in% colnames(mm) && all(mm[, term] == 0)) return("empty")
  "omit"
}

#' Regtab rows for one fitted model
#'
#' @param fit A fitted model.
#' @param info Its `tt_model_info()`.
#' @param level Confidence level in (0, 1).
#' @param ci_method `"wald"` or `"profile"`.
#' @param vce,cluster See `tt_vcov()`.
#' @param interactions `"fvgen"` or `"native"`.
#' @param xsymbol,vsref fvgen label options.
#' @return data.frame of rows (key, block, kind, label, status, term,
#'   estimate, conf.low, conf.high, p.value on the display scale, sub order,
#'   ancillary), in the model's row order with the intercept last.
#' @keywords internal
#' @noRd
#' @export
tt_regtab_rows.default <- function(fit, info, level = 0.95, ci_method = "wald",
                           interactions = "fvgen", xsymbol = "\u00d7", vsref = NULL, vce = "stata",
                           cluster = NULL, ...) {
  fr <- .rt_frame(fit)
  mf <- fr$mf
  mm <- fr$mm
  # Contrasts: refuse anything but treatment-type coding before tidying
  # (review P0-6), and record each factor's reference level.
  ctr <- if (!is.null(mm)) attr(mm, "contrasts") else NULL
  bases <- list()
  for (cv in names(ctr)) {
    if (!is.null(mf) && !is.null(mf[[cv]])) bases[[cv]] <- .rt_contrast_base(ctr[[cv]], mf[[cv]], cv)
  }
  .rt_check_marginal_coding(fit, mf, mm)
  # Rows with zero weight are not in Stata's estimation sample (e(N), the
  # factor expansion; muse P1-4): a level seen only at zero weight is not
  # observed, and a base level seen only there leaves R's coefficients
  # contrasted with another level, so that is refused.
  zw <- .rt_pos_weight_rows(mf, mm)
  if (!is.null(zw)) {
    for (cv in names(bases)) {
      xb <- as.character(mf[[cv]])
      b <- bases[[cv]]
      if (!is.na(b) && any(xb == b, na.rm = TRUE) && !any(xb[zw] == b, na.rm = TRUE)) {
        cli::cli_abort(c(
          "The reference level {.val {b}} of {.var {cv}} occurs only in rows with zero weight.",
          "i" = "Stata leaves zero-weight rows out of the estimation sample and takes another base level; R's coefficients for {.var {cv}} are then not contrasts with {.val {b}}.",
          "i" = "Refit on the rows with a positive weight (for instance {.code subset = w > 0})."
        ), call = NULL)
      }
    }
    attr(mf, "tt_pos") <- zw
    mm <- mm[zw, , drop = FALSE]
  }
  # The Wald table first, so that a refusal from the variance (H-D6/H-D7)
  # reaches the user as it is, not wrapped by broom.helpers' tidy_fun error.
  wald <- tt_wald(fit, level, ci_method, vce, cluster)
  if (inherits(fit, "glmmTMB")) {
    # broom.helpers indexes variables using the combined formula's terms,
    # including grouping terms. The stored frame carries those same terms
    # without random-effect bars, so its full design preserves their assign
    # indices and dropped columns. Its fallback calls lme4::nobars(), whose
    # relocation now warns; native model.matrix(fit) drops aliases.
    attr(fit, "model_frame") <- stats::model.frame(fit)
    attr(fit, "model_matrix") <- stats::model.matrix(
      attr(attr(fit, "model_frame"), "terms"), attr(fit, "model_frame"),
      contrasts.arg = fit$modelInfo$contrasts
    )
  }
  tp <- broom.helpers::tidy_plus_plus(
    fit, tidy_fun = .rt_tidy_fun, conf.int = TRUE, conf.level = level, exponentiate = FALSE,
    add_reference_rows = TRUE, add_header_rows = FALSE, add_estimate_to_reference_rows = FALSE,
    intercept = TRUE, add_n = FALSE, ci_method = ci_method, vce = vce, cluster = cluster, quiet = TRUE
  )
  tp <- as.data.frame(tp, stringsAsFactors = FALSE)
  # broom.helpers can mistake a directly named numeric predictor for an
  # intercept when its escaped name contains a backslash or backtick.
  # Decode that exact coefficient symbol against the stored frame; no
  # pattern matching or guessed coefficient mapping is needed.
  for (j in which(tp$var_type %in% "intercept" & !tp$term %in% c("(Intercept)", .rt_anc_terms(info)))) {
    symbol <- tryCatch(str2lang(tp$term[j]), error = function(e) NULL)
    if (!is.name(symbol)) next
    v <- as.character(symbol)
    if (is.null(mf) || !v %in% names(mf) || !is.numeric(mf[[v]]) || is.matrix(mf[[v]])) next
    tp$variable[j] <- v
    tp$var_type[j] <- "continuous"
  }
  # broom.helpers reduces poly() variables to their underlying predictor,
  # which can duplicate coefficient metadata when that predictor also has
  # its own term or a random slope. Use the design's exact term assignment
  # to keep matrix-valued terms separate from scalar predictors and labels.
  design <- attr(fit, "model_matrix", exact = TRUE) %||% fr$mm
  design_terms <- if (inherits(fit, "glmmTMB")) {
    attr(attr(fit, "model_frame", exact = TRUE), "terms")
  } else tryCatch(stats::terms(fit), error = function(e) NULL)
  assign <- attr(design, "assign")
  matrix_terms <- if (!is.null(mf)) names(mf)[vapply(mf, is.matrix, TRUE)] else character()
  if (length(matrix_terms) && is.matrix(design) && !is.null(design_terms) &&
      length(assign) == ncol(design)) {
    design_variables <- rep(NA_character_, length(assign))
    non_intercept <- which(assign > 0L)
    design_variables[non_intercept] <- attr(design_terms, "term.labels")[assign[non_intercept]]
    variables <- design_variables[match(tp$term, colnames(design))]
    basis <- variables %in% matrix_terms
    tp$variable[basis] <- variables[basis]
    tp$var_type[basis] <- "continuous"
  }
  continuous <- tp$var_type %in% "continuous" & tp$term %in% wald$term &
    tp$term %in% colnames(design)
  duplicate <- rep(FALSE, nrow(tp))
  duplicate[continuous] <- duplicated(tp$term[continuous])
  tp <- tp[!duplicate, , drop = FALSE]
  .rt_warn_separation(fit, wald)

  # Term order as written in the formula (Stata lists collect rows in command
  # order; R's own coefficient order moves interactions last).
  tl <- tryCatch(attr(stats::terms(stats::formula(fit), keep.order = TRUE), "term.labels"),
                 error = function(e) character())
  tl_vars <- lapply(tl, function(t) tryCatch(all.vars(str2lang(t)), error = function(e) character()))
  term_pos <- function(v) {
    parts <- sort(strsplit(v, ":", fixed = TRUE)[[1]])
    hit <- which(vapply(strsplit(tl, ":", fixed = TRUE), function(p) identical(sort(p), parts), TRUE))
    # broom.helpers names a transformed term (poly(mpg, 2)) by its variable.
    if (!length(hit)) hit <- which(vapply(tl_vars, function(p) setequal(p, parts), TRUE))
    if (length(hit)) hit[1] else length(tl) + 1L
  }

  vars <- unique(tp$variable[!is.na(tp$variable)])
  int_vars <- vars[tp$var_type[match(vars, tp$variable)] == "interaction"]
  in_interaction <- unique(unlist(strsplit(int_vars, ":", fixed = TRUE)))
  if (interactions == "fvgen") {
    # fvgen flattens two-way interactions only; an `I(x^2)` component is
    # itself c.x#c.x, so `f:I(x^2)` is three-way (i.f#c.x#c.x) for fvgen.
    ncomp <- vapply(strsplit(int_vars, ":", fixed = TRUE), function(p) {
      length(p) + sum(!is.na(vapply(p, .rt_square_of, "")))
    }, 0)
    hi <- int_vars[ncomp > 2L]
    if (length(hi)) {
      cli::cli_abort(c("fvgen labelling supports two-way interactions only ({.val {hi}}).",
                       "i" = "Use {.code interactions = \"native\"} for higher-order terms."),
                     call = NULL)
    }
  }
  if (!nzchar(xsymbol)) xsymbol <- "\u00d7"

  out <- list()
  add <- function(df) out[[length(out) + 1L]] <<- df
  for (v in vars) {
    rows <- tp[tp$variable %in% v, , drop = FALSE]
    # Cutpoints and ancillary parameters: tt_regtab_ancillary_rows().
    if (all(rows$term %in% .rt_anc_terms(info))) next
    type <- rows$var_type[1]
    pos <- term_pos(v)
    if (type == "intercept") {
      w <- wald[wald$term == rows$term[1], ]
      add(.rt_row("_cons", "_cons", "intercept", "Intercept", .rt_na_status(rows$term[1], w$estimate, mm),
                  term = rows$term[1], estimate = w$estimate, conf.low = w$conf.low,
                  conf.high = w$conf.high, p.value = w$p.value, sub = 0))
      next
    }
    if (type == "interaction") {
      add(.rt_interaction_rows(v, rows, wald, mf, mm, interactions, xsymbol, pos, bases))
      next
    }
    # broom.helpers types an ordered factor with treatment contrasts as
    # continuous; the model frame decides.
    is_fac_col <- !is.null(mf) && !is.null(mf[[v]]) &&
      (is.factor(mf[[v]]) || is.character(mf[[v]]) || is.logical(mf[[v]]))
    if (type %in% c("categorical", "dichotomous") || (type == "continuous" && is_fac_col)) {
      if (is_fac_col) rows <- .rt_contrast_rows(v, rows, wald, mf, mm)
      add(.rt_factor_rows(v, rows, wald, mf, mm, pos,
                          flat = interactions == "fvgen" && v %in% in_interaction, vsref = vsref))
      next
    }
    # Continuous (numeric) terms. A term with several columns (a matrix
    # predictor, poly(), splines) gets one row per column, keyed by the
    # coefficient name (review P0-6).
    multi <- nrow(rows) > 1L
    for (k in seq_len(nrow(rows))) {
      term <- rows$term[k]
      w <- wald[wald$term == term, ]
      sq <- if (!multi && identical(term, v)) .rt_square_of(v) else NA_character_
      if (!is.na(sq)) {
        key <- paste0("c.", sq, "#c.", sq)
        label <- if (interactions == "fvgen") {
          .rt_fv80(paste0(.rt_square_label(mf, sq, v), "\u00b2"))
        } else paste0(sq, "#", sq)
      } else if (!multi && !is.null(mf) && v %in% names(mf) && identical(make.names(v), v)) {
        key <- v
        label <- .rt_var_label(mf, v)
      } else {
        key <- term
        label <- term
      }
      add(.rt_row(key, key, "var", label, .rt_na_status(term, w$estimate, mm), term = term,
                  estimate = w$estimate, conf.low = w$conf.low, conf.high = w$conf.high,
                  p.value = w$p.value, sub = pos + k / 1000))
    }
  }
  res <- if (length(out)) do.call(rbind, out) else .rt_row(character(), character(), character(), character())
  # Intercept last (Stata lists _cons after every other row, probed
  # 2026-09-25), otherwise formula order.
  ord <- order(res$kind == "intercept", res$sub)
  res <- res[ord, , drop = FALSE]
  rownames(res) <- NULL
  # One row per key, and every estimated coefficient on exactly one row: the
  # multi-model union keys rows, so a repeated key would silently hide a
  # coefficient (review P0-6).
  b <- tt_coef(fit)
  est_terms <- setdiff(names(b)[!is.na(b)], .rt_anc_terms(info))
  dup <- unique(res$key[duplicated(res$key)])
  lost <- setdiff(est_terms, res$term)
  twice <- unique(res$term[!is.na(res$term) & duplicated(res$term)])
  if (length(dup) || length(lost) || length(twice)) {
    cli::cli_abort(c("Internal error: regtab rows do not map one-to-one to the model's coefficients.",
                     "x" = "Repeated keys: {.val {dup}}; coefficients without a row: {.val {lost}}; on two rows: {.val {twice}}."),
                   call = NULL)
  }
  # Factors whose level codes are positions rather than Stata value codes
  # (see .rt_codes()), for regtab()'s note on native keys and keep/drop.
  attr(res, "positional") <- if (!is.null(mf)) {
    names(mf)[vapply(names(mf), function(v) {
      x <- mf[[v]]
      is.factor(x) && isTRUE(attr(.rt_codes(x), "positional"))
    }, TRUE)]
  } else character()
  # Display scale: ratio models exponentiate estimates and bounds, ancillary
  # parameters excepted (`regtab.ado:2069-2071`, `:2126-2129`).
  if (isTRUE(info$exponentiate)) {
    e <- !res$ancillary
    for (col in c("estimate", "conf.low", "conf.high")) res[[col]][e] <- exp(res[[col]][e])
  }
  .rt_add_ancillary(res, tt_regtab_ancillary_rows(fit, info, level = level, ci_method = ci_method, vce = vce,
                                                  cluster = cluster))
}

# A factor main effect: native layout (variable-label parent, indented
# levels, reference row) or, for a variable used in an fvgen interaction,
# fvgen's flat rows (one per non-base level, no parent, no reference).
.rt_factor_rows <- function(v, rows, wald, mf, mm, pos, flat = FALSE, vsref = NULL) {
  x <- if (!is.null(mf)) mf[[v]] else NULL
  codes <- if (!is.null(x)) .rt_codes(x) else stats::setNames(as.character(seq_len(nrow(rows))), rows$label)
  # Levels with no observations in the estimation sample are not part of a
  # Stata fit at all (factor variables expand only observed levels).
  observed <- if (!is.null(x)) {
    tab <- table(factor(as.character(.rt_pos_rows(x, mf)), levels = names(codes)))
    names(tab)[tab > 0]
  } else rows$label
  lab_parent <- .rt_var_label(mf, v)
  labelled <- if (!is.null(x)) .rt_is_labelled(x, codes) else TRUE
  txt <- function(lev) if (!is.null(x) && lev %in% names(codes)) .rt_level_text(x, lev) else lev
  base_label <- rows$label[which(rows$reference_row %in% TRUE)[1]]
  out <- list()
  if (!flat) out[[1]] <- .rt_row(v, v, "cat_header", lab_parent, "header", sub = pos)
  for (k in seq_len(nrow(rows))) {
    lev <- rows$label[k]
    if (!is.null(x) && !lev %in% observed) next
    code <- if (lev %in% names(codes)) codes[[lev]] else as.character(k)
    key <- paste0(code, ".", v)
    is_ref <- isTRUE(rows$reference_row[k])
    if (flat) {
      if (is_ref) next
      lab <- .rt_fv_partlabel(v, txt(lev), labelled)
      if (!is.null(vsref) && !is.na(base_label)) {
        lab <- paste(lab, gsub("@", .rt_fv_partlabel(v, txt(base_label), labelled), vsref, fixed = TRUE))
      }
      lab <- .rt_fv80(lab)
      kind <- "flat"
    } else {
      lab <- paste0("  ", txt(lev))
      kind <- "level"
    }
    if (is_ref) {
      out[[length(out) + 1L]] <- .rt_row(key, v, kind, lab, "base", sub = pos + k / 1000)
      next
    }
    term <- rows$term[k]
    w <- wald[wald$term == term, ]
    if (!nrow(w)) {
      # Only the fit-owned contrast mapping above establishes a reference.
      # A nonreference level without a Wald coefficient is not estimable.
      out[[length(out) + 1L]] <- .rt_row(key, v, kind, lab, "notest", term = term,
                                       sub = pos + k / 1000)
      next
    }
    st <- .rt_na_status(term, w$estimate, mm)
    if (flat && st == "empty") st <- "omit"
    out[[length(out) + 1L]] <- .rt_row(key, v, kind, lab, st, term = term, estimate = w$estimate,
                                       conf.low = w$conf.low, conf.high = w$conf.high,
                                       p.value = w$p.value, sub = pos + k / 1000)
  }
  if (flat) for (i in seq_along(out)) out[[i]]$block <- out[[i]]$key
  do.call(rbind, out)
}

# Interaction terms. Native (`regtab.ado:1304-1335`, probed 2026-09-25): a
# term with a factor component gets a parent row "a#b" (component names) and
# one unindented row per level combination of its factor components, keyed
# and labelled "1.foreign#3.rep78" / "1.foreign#mpg"; combinations at a base
# level are Reference, those with no observations Empty (which wins over
# Reference), aliased ones Omitted. A purely continuous term is one row
# "mpg#weight". fvgen (`fvgen.ado:586-673`): one flat row per estimable
# cell (every factor component at a non-base level), labelled
# "part1 x part2" with value labels and variable labels, joined by the
# xsymbol with a space on each side (`fvgen.ado:673`); cells R aliases are
# Omitted. Stata canonicalises a term's components with the factor
# variables first (`c.mpg#i.foreign` is `foreign#mpg`), keeping the written
# order otherwise (review P0-4); an `I(x^2)` component is `x#x`.
.rt_interaction_rows <- function(v, rows, wald, mf, mm, interactions, xsymbol, pos, bases = list()) {
  comps <- strsplit(v, ":", fixed = TRUE)[[1]]
  is_fac <- vapply(comps, function(cv) {
    x <- if (!is.null(mf)) mf[[cv]] else NULL
    is.factor(x) || is.character(x) || is.logical(x)
  }, TRUE)
  ord <- order(!is_fac)
  comps <- comps[ord]
  is_fac <- is_fac[ord]
  lev_of <- lapply(comps, function(cv) {
    if (!is_fac[[cv]]) return(NULL)
    x <- mf[[cv]]
    codes <- .rt_codes(x)
    tab <- table(factor(as.character(.rt_pos_rows(x, mf)), levels = names(codes)))
    names(codes)[tab > 0]
  })
  names(lev_of) <- comps
  codes_of <- lapply(comps, function(cv) if (is_fac[[cv]]) .rt_codes(mf[[cv]]) else NULL)
  names(codes_of) <- comps
  # Coefficient-name suffix of each level of a factor this term codes by
  # its contrasts (attr(terms, "factors") 1; audit 2026-09-29 B02): the
  # contrast matrix's column (`contrasts(g) <- contr.treatment(3, base = 2)`
  # names the cells g1:x and g3:x). A factor the term codes in full (2: the
  # term lacks its main effect) has one column per level, named by it.
  tf <- tryCatch(attr(attr(mf, "terms"), "factors"), error = function(e) NULL)
  ctr <- if (!is.null(mm)) attr(mm, "contrasts") else NULL
  suf_of <- lapply(comps, function(cv) {
    if (!is_fac[[cv]] || is.null(ctr[[cv]])) return(NULL)
    by_ctr <- is.matrix(tf) && cv %in% rownames(tf) && v %in% colnames(tf) && identical(unname(tf[cv, v]), 1L)
    if (by_ctr) .rt_contrast_suffix(ctr[[cv]], mf[[cv]]) else NULL
  })
  names(suf_of) <- comps
  # Coefficient name of a cell, in R's term-label component order.
  coef_names <- wald$term
  cell_term <- function(levs) {
    parts <- vapply(seq_along(comps), function(i) {
      if (!is_fac[[i]]) return(comps[i])
      s <- suf_of[[i]]
      suf <- if (!is.null(s) && levs[[i]] %in% names(s)) s[[levs[[i]]]] else levs[[i]]
      if (is.na(suf)) NA_character_ else paste0(comps[i], suf)
    }, "")
    if (anyNA(parts)) return(NA_character_)
    nm <- paste(parts, collapse = ":")
    if (nm %in% coef_names) return(nm)
    # R may order the components differently in the coefficient name.
    hit <- coef_names[vapply(strsplit(coef_names, ":", fixed = TRUE),
                             function(p) setequal(p, parts) && length(p) == length(parts), TRUE)]
    if (length(hit)) hit[1] else NA_character_
  }
  grid <- expand.grid(lapply(rev(comps), function(cv) if (is_fac[[cv]]) lev_of[[cv]] else ""),
                      stringsAsFactors = FALSE)
  grid <- grid[, rev(seq_along(comps)), drop = FALSE]
  names(grid) <- comps
  # Base level of each factor component under its (treatment-type)
  # contrasts; the first level when the design lists none.
  base_of <- vapply(comps, function(cv) {
    if (!is_fac[[cv]]) return("")
    bases[[cv]] %||% names(codes_of[[cv]])[1]
  }, "")
  count_cell <- function(levs) {
    keep <- attr(mf, "tt_pos") %||% rep(TRUE, nrow(mf))
    for (i in seq_along(comps)) if (is_fac[[i]]) keep <- keep & as.character(mf[[comps[i]]]) == levs[[i]]
    sum(keep, na.rm = TRUE)
  }
  var_lab <- function(cv) .rt_var_label(mf, cv)
  labelled <- vapply(comps, function(cv) {
    if (!is_fac[[cv]]) return(TRUE)
    .rt_is_labelled(mf[[cv]], codes_of[[cv]])
  }, TRUE)
  # Native name of a continuous component: `I(x^2)` is x#x.
  cont_name <- function(cv) {
    sq <- .rt_square_of(cv)
    if (is.na(sq)) cv else paste0(sq, "#", sq)
  }
  cont_key <- function(cv) {
    sq <- .rt_square_of(cv)
    if (is.na(sq)) paste0("c.", cv) else paste0("c.", sq, "#c.", sq)
  }
  out <- list()
  native <- interactions == "native"
  parent_key <- paste(vapply(comps, function(cv) if (is_fac[[cv]]) cv else cont_name(cv), ""), collapse = "#")
  if (native && any(is_fac)) {
    out[[1]] <- .rt_row(parent_key, parent_key, "int_header", parent_key, "header", sub = pos)
  }
  for (g in seq_len(nrow(grid))) {
    levs <- as.list(grid[g, ])
    key_parts <- vapply(seq_along(comps), function(i) {
      if (is_fac[[i]]) paste0(codes_of[[i]][[levs[[i]]]], ".", comps[i]) else cont_key(comps[i])
    }, "")
    lab_parts <- vapply(seq_along(comps), function(i) {
      if (is_fac[[i]]) paste0(codes_of[[i]][[levs[[i]]]], ".", comps[i]) else cont_name(comps[i])
    }, "")
    key <- paste(key_parts, collapse = "#")
    # A cell R estimated is shown whatever its levels (a term without its
    # main effects is coded in full); otherwise a base-level cell is Reference.
    term <- cell_term(levs)
    at_base <- is.na(term) &&
      any(vapply(seq_along(comps), function(i) is_fac[[i]] && levs[[i]] == base_of[[i]], TRUE))
    n_cell <- if (any(is_fac)) count_cell(levs) else 1L
    w <- if (!is.na(term)) wald[wald$term == term, ] else NULL
    sub <- pos + g / 1000
    if (native) {
      label <- paste(lab_parts, collapse = "#")
      block <- if (any(is_fac)) parent_key else key
      if (n_cell == 0) {
        st <- "empty"
      } else if (at_base || is.null(w) || !nrow(w)) {
        st <- "base"
      } else {
        st <- .rt_na_status(term, w$estimate, mm)
      }
      kind <- if (any(is_fac)) "int_level" else "var"
    } else {
      # fvgen creates no variable for a base cell, nor for a cell with no
      # observations, so Stata's table has no row for either (probe
      # 2026-09-28: `fvgen i.foreign##i.rep78` on auto makes no
      # _foreignXrep78_1_1/_1_2; muse P2-2). An estimable cell that is
      # collinear is Omitted.
      if (at_base || n_cell == 0) next
      label <- .rt_fv80(paste(vapply(seq_along(comps), function(i) {
        if (is_fac[[i]]) {
          .rt_fv_partlabel(comps[i], .rt_level_text(mf[[comps[i]]], levs[[i]]), labelled[[i]])
        } else var_lab(comps[i])
      }, ""), collapse = paste0(" ", xsymbol, " ")))
      block <- key
      st <- if (is.null(w) || !nrow(w) || is.na(w$estimate)) "omit" else "est"
      kind <- "flat"
    }
    if (st == "est") {
      out[[length(out) + 1L]] <- .rt_row(key, block, kind, label, st, term = term, estimate = w$estimate,
                                         conf.low = w$conf.low, conf.high = w$conf.high,
                                         p.value = w$p.value, sub = sub)
    } else {
      out[[length(out) + 1L]] <- .rt_row(key, block, kind, label, st, term = term, sub = sub)
    }
  }
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Model statistics (task 4.9, `regtab.ado:703-940`)

#' Fit statistics regtab can show for one model
#'
#' `N` (observations; Cox records), `N_sub` (subjects, survival models),
#' `ll`, `rank` (estimated coefficients plus ancillary parameters, Stata's
#' e(rank)), AIC `-2ll + 2 rank` and BIC `-2ll + rank ln(N)` recomputed as
#' regtab does, `r2`, `r2_p` (McFadden, `1 - ll/ll_0`), `r2_a`, `groups`,
#' `qic`, `icc` (the last three are NA outside Phase 5 models). An S3
#' generic: Phase 5 adapters add methods (`R/regtab_models_*.R`); a method
#' may also set `icc_note`/`qic_note` (see `.rt_stat_notes()`).
#' @keywords internal
#' @noRd
#' @export
tt_model_stats.default <- function(fit, info, vce = "stata", cluster = NULL, ...) {
  s <- list(N = NA_real_, N_sub = NA_real_, ll = NA_real_, rank = NA_real_,
            aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_,
            r2_a = NA_real_, groups = NA_real_, qic = NA_real_, icc = NA_real_)
  b <- tryCatch(stats::coef(fit), error = function(e) NULL)
  nb <- if (is.numeric(b)) sum(!is.na(b)) else NA_real_
  if (inherits(fit, "coxph")) {
    s$N <- fit$n
    # coxph keeps its subject count (n.id) when fitted with `id`, so the
    # data need not be read again (review P0-1).
    s$N_sub <- if (!is.null(fit$call$id)) {
      fit$n.id %||% length(unique(stats::model.frame(.rt_require_frame(fit, "subject count"))[["(id)"]]))
    } else fit$n
    s$ll <- fit$loglik[length(fit$loglik)]
    s$rank <- nb
  } else if (inherits(fit, "glm")) {
    s$N <- stats::nobs(fit)
    s$ll <- as.numeric(stats::logLik(fit))
    s$rank <- fit$rank + if (inherits(fit, "negbin") && !.rt_nb_glm(fit)) 1 else 0
    fam <- fit$family$family
    if (!inherits(fit, "negbin") && .rt_stores_r2p(fit)) {
      # With an offset, Stata's logit/probit store no e(r2_p) (and ologit no
      # e(ll_0)); poisson's is against the constant-only model with the
      # offset, R's null deviance (probes O1-O4, external review F27).
      # Computed from R's log-likelihood before .rt_glm_ll() rescales
      # analytic weights: the ratio does not change with the scale.
      if (!(fam == "binomial" && .rt_has_offset(fit))) {
        ll0 <- s$ll - (fit$null.deviance - fit$deviance) / 2
        s$r2_p <- 1 - s$ll / ll0
      }
    }
    if (.rt_nb_glm(fit)) {
      # Stata's glm stores no pseudo R-squared.
    } else if (inherits(fit, "negbin")) {
      # nbreg reports a McFadden pseudo R2 against the constant-only model
      # with alpha re-estimated (e(ll_0); review P0-9).
      ll0 <- .rt_negbin_ll0(fit)
      if (!is.na(ll0)) s$r2_p <- 1 - s$ll / ll0 else s$r2p_note <- TRUE
    } else {
      s$ll <- .rt_glm_ll(fit, s$ll)
    }
  } else if (inherits(fit, "lm")) {
    s$N <- stats::nobs(fit)
    s$ll <- .rt_lm_ll(fit)
    s$rank <- fit$rank
    sm <- summary(fit)
    s$r2 <- sm$r.squared
    s$r2_a <- sm$adj.r.squared
    # Task 5.17: regress's e(rmse) and e(F) (probes S1-S8,
    # tests/testthat/fixtures/regtab_mi/stats.csv).
    s$rmse <- .rt_lm_rmse(fit)
    s$F <- .rt_lm_F(fit, vce, cluster, sm)
  } else {
    s$N <- tryCatch(stats::nobs(fit), error = function(e) NA_real_)
    s$ll <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
    s$rank <- nb
  }
  if (!is.na(s$ll) && !is.na(s$rank)) {
    s$aic <- -2 * s$ll + 2 * s$rank
    if (!is.na(s$N)) s$bic <- -2 * s$ll + s$rank * log(s$N)
  }
  s
}

# Root MSE as regress reports it, sqrt(sum(w e^2) / (N - k)): N - k the
# residual degrees of freedom (observations with a positive weight minus
# the estimated coefficients), the weights ([aweight], and [pweight] under a
# robust or cluster vce) rescaled to mean 1 over the positive weights, as
# regress rescales them (auto, `regress price mpg weight [aw=turn]`:
# 2562.62366, probe S4; summary.lm()'s sigma uses the raw weights). The
# variance type does not change it (S2, S3, S5).
.rt_lm_rmse <- function(fit) {
  r <- fit$residuals
  w <- fit$weights
  if (!is.null(w)) {
    pos <- w > 0
    r <- r[pos]
    w <- w[pos] / mean(w[pos])
  } else w <- rep(1, length(r))
  df <- fit$df.residual
  if (!is.finite(df) || df <= 0) return(NA_real_)
  sqrt(sum(w * r^2) / df)
}

# regress's model F statistic, e(F): the test that every coefficient but
# the intercept is zero (all of them without an intercept).
# * vce "stata"/"model": the classical F of the (weighted) sums of squares,
#   summary.lm()'s (probes S1, S4, S6);
# * vce "robust"/"cluster": the Wald F, b' V^-1 b / q with the robust or
#   cluster-robust V (S2, S3b, S5, S8); Stata reports none when that V is
#   singular, its rank below the number of estimated coefficients k: G - 1
#   < k after vce(cluster) (S3: k = 3 with 3 clusters; T2), or a singleton
#   dummy under vce(robust) (S10), e(F) missing (C4 review F2);
# * a model with nothing to test (constant only): Stata stores e(F) = 0
#   (S7), and regtab shows it.
.rt_lm_F <- function(fit, vce = "stata", cluster = NULL, sm = summary(fit)) {
  b <- stats::coef(fit)
  est <- names(b)[!is.na(b)]
  icpt <- attr(stats::terms(fit), "intercept") == 1L
  test <- if (icpt) setdiff(est, "(Intercept)") else est
  q <- length(test)
  if (!q) return(if (icpt) 0 else NA_real_)
  if (vce %in% c("stata", "model")) {
    f <- sm$fstatistic
    return(if (is.null(f)) NA_real_ else unname(f[["value"]]))
  }
  Vf <- as.matrix(tt_vcov(fit, vce, cluster))[est, est, drop = FALSE]
  sc <- max(abs(diag(Vf)))
  if (!is.finite(sc) || sc <= 0 || qr(Vf / sc, tol = 1e-10)$rank < length(est)) return(NA_real_)
  V <- Vf[test, test, drop = FALSE]
  bt <- b[test]
  W <- tryCatch(drop(crossprod(bt, solve(V, bt))), error = function(e) NA_real_)
  W / q
}

# Log-likelihood of an lm fit as Stata's regress reports it (review P0-8).
# Unweighted, logLik() is the same number. With weights, regress [aweight]
# rescales the weights to mean 1 and reports
#   ll = -N/2 (1 + ln(2 pi) + ln(sum(w r^2) / N)),
# while R's logLik.lm() adds 0.5 sum(log w) for the raw weights (auto
# `regress price mpg weight [aw=turn]`: Stata -684.28042747, R -684.51).
.rt_lm_ll <- function(fit) {
  w <- fit$weights
  if (is.null(w)) return(as.numeric(stats::logLik(fit)))
  # fit$residuals: never padded by na.exclude (review P1-1).
  r <- fit$residuals
  pos <- w > 0
  w <- w[pos]
  r <- r[pos]
  n <- length(w)
  w <- w / mean(w)
  -n / 2 * (1 + log(2 * pi) + log(sum(w * r^2) / n))
}

# Log-likelihood of a glm (not glm.nb) as Stata's glm reports it under the
# model-based reading (pre-release review P0-1; Stata 17 probes,
# qa/stata/make_regtab_glm_ll.do -> tests/testthat/fixtures/regtab_glm_ll/):
# - gamma and inverse Gaussian: at scale 1 (the dispersion enters the
#   standard errors only), where R's logLik() plugs in deviance/n
#   (auto, gamma log: Stata -717.555, R -664.135);
# - weights, read as Stata's [aweight]: rescaled to mean 1 over the
#   positive weights, where logLik() uses them as they are (auto, poisson
#   [aw=turn]: Stata -195.174, R -7738.39; gaussian [aw]: regress [aw]'s
#   -684.280).
# Other fits keep R's logLik() (`ll`): unweighted gaussian (deviance/n, as
# Stata's glm), binomial and poisson, a binomial with trials, the quasi
# families (NA).
.rt_glm_ll <- function(fit, ll) {
  fam <- fit$family$family
  if (startsWith(fam, "quasi")) return(ll)
  w <- fit$prior.weights
  weighted <- !is.null(w) && any(w != 1)
  if (!weighted && !(fam %in% c("Gamma", "inverse.gaussian"))) return(ll)
  v <- .rt_glm_pseudo_ll(fit, normalise = TRUE)
  if (is.null(v) || !is.finite(v)) ll else v
}

# Whether the Stata command a glm stands for stores e(r2_p): logit and
# probit (binomial, logit or probit link) and poisson (log link). cloglog
# and Stata's glm command store none (probes C1-C2, review T2B-09).
.rt_stores_r2p <- function(fit) {
  fam <- fit$family$family
  link <- fit$family$link
  (fam == "binomial" && link %in% c("logit", "probit")) || (fam == "poisson" && link == "log")
}

# Whether a glm-type fit has an offset (an offset() term or `offset =`).
.rt_has_offset <- function(fit) {
  if (!is.null(fit$offset)) return(TRUE)
  mf <- if (is.data.frame(fit$model)) fit$model else NULL
  !is.null(mf) && !is.null(stats::model.offset(mf))
}

# Log-likelihood of the constant-only negative binomial model on the same
# sample (offset and prior weights kept, theta re-estimated), as nbreg's
# e(ll_0). The response is the fitted one (rebuilt for y = FALSE, external
# review F05/F28).
.rt_negbin_ll0 <- function(fit) {
  y <- .rt_glm_y(fit)
  off <- if (is.null(fit$offset)) rep(0, length(y)) else fit$offset
  d <- data.frame(y = y, off = off, w = fit$prior.weights)
  f0 <- tryCatch(suppressWarnings(do.call(MASS::glm.nb, list(
    formula = y ~ 1 + offset(off), data = d, weights = d$w, link = as.name(fit$family$link)
  ))), error = function(e) NULL)
  if (is.null(f0)) NA_real_ else as.numeric(stats::logLik(f0))
}
