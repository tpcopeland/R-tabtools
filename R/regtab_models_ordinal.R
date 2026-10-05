# regtab adapter for ordered-outcome models (plan task 5.1): MASS::polr and
# ordinal::clm, Stata's ologit (logit link) and oprobit.
#
# Stata facts (probed 2026-09-25, qa/stata/make_regtab_phase5a.do):
# - Rows: the covariates as for any single-equation model, then one row per
#   cutpoint labelled `cut1`, `cut2`, ... (the colnames of the `/` equation),
#   whose estimate and CI stay on the raw scale (`regtab.ado:2059-2062`
#   marks `cut#` rows ancillary, so the OR transform skips them) and whose
#   p-value is blank (ologit reports no test for a cutpoint). ologit has no
#   intercept.
# - `nointercept` (automatic for ologit) drops the cutpoint rows by their
#   label (`regtab.ado:1623-1624`); `cutlabels("A \ B")` then relabels the
#   kept `cut#` rows positionally (`regtab.ado:1979-1995`).
# - Standard errors: Stata's vce(oim). ordinal::clm's vcov() is the
#   analytic observed information (it matches Stata's ologit to 1e-13);
#   MASS::polr's comes from optim()'s finite-difference Hessian (2e-4 off),
#   so regtab evaluates the analytic observed information at polr's
#   estimates.
# - Stats: e(rank) counts slopes and cutpoints; e(r2_p) = 1 - ll/ll_0 with
#   ll_0 the constant-only (marginal) log-likelihood.

#' @export
tt_regtab_adapter.polr <- function(fit, ...) TRUE

#' @export
tt_regtab_adapter.clm <- function(fit, ...) TRUE

# MASS exposes optim()'s code (0 is success). ordinal's diagnostic is a
# list: negative codes mean failure, while positive codes can describe
# successful convergence with limited identifiability. Those fits are
# usable only when ordinal retained finite covariance for active terms.
.rt_check_ordinal_convergence <- function(fit, i = 1L) {
  code <- fit[["convergence"]]
  is_clm <- inherits(fit, "clm")
  if (is_clm && is.list(code)) code <- code[["code"]]
  failed <- !is.null(code) && any(if (is_clm) code < 0 else code != 0, na.rm = TRUE)
  if (failed) {
    cli::cli_abort(c(
      "Model {i} ({.cls {class(fit)[1]}}) did not converge; its estimates and inference cannot be reported reliably.",
      "i" = "Refit the model and inspect {.code fit$convergence}, the iteration limit and sparse outcome categories."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  # clm's default vcov() simply returns this stored matrix, even when
  # convergence code 1 leaves every active variance missing. Aliased terms
  # are absent from the matrix, so this does not reject omitted slopes.
  if (is_clm && !is.null(fit[["vcov"]]) &&
      (!is.matrix(fit[["vcov"]]) || !all(is.finite(fit[["vcov"]])) ||
       any(diag(fit[["vcov"]]) < 0))) {
    cli::cli_abort(c(
      "Model {i} ({.cls clm}) has invalid covariance for its estimated parameters; its inference cannot be reported reliably.",
      "i" = "Inspect {.code fit$convergence} and {.code fit$Hessian}; rescale predictors or refit an identifiable model."
    ), class = "tabtools_error_model_covariance", call = NULL)
  }
  invisible(TRUE)
}

# Distribution function of each polr/clm link and its first two
# derivatives (density f and f').
.rt_ord_link <- function(link) {
  switch(link,
    logistic = , logit = list(
      F = stats::plogis, f = stats::dlogis,
      fp = function(x) { p <- stats::plogis(x); stats::dlogis(x) * (1 - 2 * p) }),
    probit = list(F = stats::pnorm, f = stats::dnorm, fp = function(x) -x * stats::dnorm(x)),
    cloglog = list(F = function(x) -expm1(-exp(x)), f = function(x) exp(x - exp(x)),
                   fp = function(x) exp(x - exp(x)) * (1 - exp(x))),
    loglog = list(F = function(x) exp(-exp(-x)), f = function(x) exp(-x - exp(-x)),
                  fp = function(x) exp(-x - exp(-x)) * (exp(-x) - 1)),
    cauchit = list(F = stats::pcauchy, f = stats::dcauchy,
                   fp = function(x) -2 * x / (pi * (1 + x^2)^2)),
    NULL
  )
}

# Observed information of a cumulative-link model with P(Y <= k) =
# F(zeta_k - eta), eta = X beta + offset, at (beta, zeta). Per observation
# l = log(F(a) - F(b)) with a = zeta_y - eta, b = zeta_{y-1} - eta; the
# Hessian is l_aa da da' + l_bb db db' + l_ab (da db' + db da'), where
# da/d(beta, zeta) = (-x, e_y) and db = (-x, e_{y-1}).
.rt_ord_hessian <- function(X, y, beta, zeta, link, w = NULL, offset = NULL) {
  lk <- .rt_ord_link(link)
  if (is.null(lk)) return(NULL)
  n <- nrow(X)
  p <- ncol(X)
  K <- length(zeta)
  if (is.null(w)) w <- rep(1, n)
  eta <- drop(X %*% beta)
  if (!is.null(offset)) eta <- eta + offset
  yi <- as.integer(y)
  za <- c(zeta, Inf)[yi]
  zb <- c(-Inf, zeta)[yi]
  a <- za - eta
  b <- zb - eta
  fa <- ifelse(is.finite(a), lk$f(a), 0)
  fb <- ifelse(is.finite(b), lk$f(b), 0)
  fpa <- ifelse(is.finite(a), lk$fp(a), 0)
  fpb <- ifelse(is.finite(b), lk$fp(b), 0)
  P <- ifelse(is.finite(a), lk$F(a), 1) - ifelse(is.finite(b), lk$F(b), 0)
  laa <- w * (fpa / P - fa^2 / P^2)
  lbb <- w * (-fpb / P - fb^2 / P^2)
  lab <- w * (fa * fb / P^2)
  # Design of a and b in (beta, zeta).
  Ea <- matrix(0, n, K)
  Eb <- matrix(0, n, K)
  ia <- which(yi <= K)
  ib <- which(yi > 1L)
  Ea[cbind(ia, yi[ia])] <- 1
  Eb[cbind(ib, yi[ib] - 1L)] <- 1
  Da <- cbind(-X, Ea)
  Db <- cbind(-X, Eb)
  H <- crossprod(Da, Da * laa) + crossprod(Db, Db * lbb) +
    crossprod(Da, Db * lab) + crossprod(Db, Da * lab)
  if (anyNA(H)) return(NULL)
  H
}

# Model frame of a polr fit: the stored one (model = TRUE, the default), or
# the call re-evaluated on the data as they are now, used only when it
# reproduces the fit (Milestone H decision H-D7): the same rows, the stored
# linear predictor from the design and offset, and the deviance from the
# response. MASS's own model.frame() passes the call's `model = FALSE` to
# model.frame() as a variable ("variable lengths differ (found for
# '(model)')", external review F28), so the call is rebuilt without it.
.rt_polr_frame <- function(fit) {
  if (is.data.frame(fit$model)) return(fit$model)
  cl <- fit$call
  env <- tryCatch(environment(fit$terms), error = function(e) NULL) %||% parent.frame()
  keep <- intersect(names(cl), c("formula", "data", "weights", "subset", "na.action"))
  mc <- cl[c(1L, match(keep, names(cl)))]
  mc[[1L]] <- quote(stats::model.frame)
  mc$drop.unused.levels <- TRUE
  msg <- NULL
  mf <- tryCatch(eval(mc, env), error = function(e) {
    msg <<- conditionMessage(e)
    NULL
  })
  rn <- rownames(fit$fitted.values)
  if (!is.null(rn) && !anyDuplicated(rn)) mf <- .rt_align_frame(mf, rn)
  why <- if (is.null(mf)) msg %||% "the model frame cannot be rebuilt" else .rt_polr_mismatch(fit, mf)
  if (!is.null(why)) {
    cli::cli_abort(c(
      "The {.cls polr} model was fitted with {.code model = FALSE}, and its data cannot be matched to the fit: {why}.",
      "i" = "regtab reads the data again and uses them only when they reproduce the fit (the same rows, linear predictor and deviance); the data have changed since the model was fitted, or cannot be found.",
      "i" = "Refit with {.code model = TRUE} (the default), or on the current data."
    ), call = NULL)
  }
  mf
}

.rt_polr_mismatch <- function(fit, mf) {
  n <- NROW(fit$fitted.values)
  if (nrow(mf) != n) return(sprintf("the data give %d observations, the fit has %d", nrow(mf), n))
  X <- tryCatch(stats::model.matrix(fit$terms, mf, contrasts.arg = fit$contrasts), error = function(e) NULL)
  b <- stats::coef(fit)
  if (is.null(X) || !all(names(b) %in% colnames(X))) return("the design columns differ from the coefficients")
  lp <- drop(X[, names(b), drop = FALSE] %*% b)
  off <- stats::model.offset(mf)
  if (!is.null(off)) lp <- lp + off
  if (!.rt_close(lp, fit$lp)) return("the linear predictor differs")
  y <- as.integer(stats::model.response(mf))
  w <- stats::model.weights(mf) %||% rep(1, n)
  dev <- -2 * sum(w * log(fit$fitted.values[cbind(seq_len(n), y)]))
  if (!.rt_close(dev, fit$deviance, 1e-7)) return("the response differs")
  NULL
}

# Design, response, weights and offset of a polr fit.
.rt_polr_data <- function(fit) {
  mf <- .rt_polr_frame(fit)
  X <- stats::model.matrix(fit$terms, mf, contrasts.arg = fit$contrasts)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  list(X = X, y = stats::model.response(mf), w = stats::model.weights(mf), offset = stats::model.offset(mf))
}

# polr: analytic observed information (Stata's vce(oim)) at (beta, zeta),
# or at `par` = c(beta, zeta) (the tests evaluate it at Stata's
# estimates); NULL when it cannot be formed.
.rt_polr_vcov <- function(fit, par = NULL) {
  d <- .rt_polr_data(fit)
  beta <- stats::coef(fit)
  zeta <- fit$zeta
  if (!is.null(par)) {
    beta <- par[seq_along(beta)]
    zeta <- par[length(beta) + seq_along(zeta)]
  }
  if (is.null(d) || ncol(d$X) != length(beta)) return(NULL)
  H <- .rt_ord_hessian(d$X, d$y, beta, zeta, fit$method, d$w, d$offset)
  Vn <- if (is.null(H)) NULL else .rt_inv_neg(H)
  if (is.null(Vn)) return(NULL)
  nm <- c(names(stats::coef(fit)), names(fit$zeta))
  dimnames(Vn) <- list(nm, nm)
  Vn
}

# polr's own vcov() refits the model when it was fitted with Hess = FALSE
# (printing "Re-fitting to get Hessian"), so it is called only for
# vce = "model" or as a fallback (review P2-3).
#' @export
tt_vcov.polr <- function(fit, vce = "stata", cluster = NULL, ...) {
  .rt_check_ordinal_convergence(fit)
  if (identical(vce, "model")) return(.rt_polr_model_vcov(fit))
  V <- .rt_polr_vcov(fit)
  if (is.null(V)) {
    # Never polr's own (numerical) Hessian in its place (Milestone H
    # decision H-D6; external review F28).
    cli::cli_abort(c("Stata's observed-information variance cannot be computed for this {.cls polr} fit (unsupported link or singular information).",
                     "i" = "Use {.code vce = \"model\"} for the fit's own {.fn vcov}."), call = NULL)
  }
  V
}

# polr's own vcov() (optim()'s Hessian). A fit with Hess = FALSE (MASS's
# default) has none, and MASS's vcov() refits it in the caller's frame,
# where the data are not found ("object 'd' not found"); the refit is done
# in the model's environment instead, from the fit's estimates, and used
# only when it reproduces them (the data unchanged). This is what MASS's
# vcov() computes (the same restart), only in the right environment.
.rt_polr_model_vcov <- function(fit) {
  if (!is.null(fit$Hessian)) return(stats::vcov(fit))
  cl <- fit$call
  cl$Hess <- TRUE
  cl$start <- c(stats::coef(fit), fit$zeta)
  env <- tryCatch(environment(fit$terms), error = function(e) NULL) %||% parent.frame()
  refit <- tryCatch(suppressWarnings(eval(cl, env)), error = function(e) NULL)
  # optim() restarts from the estimates and stops ~1e-6 away (as MASS's own
  # vcov() refit does); a changed data set moves them far more.
  ok <- !is.null(refit) && .rt_close(c(stats::coef(refit), refit$zeta), c(stats::coef(fit), fit$zeta), 1e-4)
  if (!ok) {
    cli::cli_abort(c("{.code vce = \"model\"} needs {.fn MASS::polr}'s Hessian, which this fit did not keep ({.code Hess = FALSE}), and it cannot be refitted on its data.",
                     "i" = "Refit with {.code Hess = TRUE}."), call = NULL)
  }
  stats::vcov(refit)
}

# clm: flexible thresholds and location effects only (Stata's ologit and
# oprobit); nominal and scale effects have no Stata equivalent.
.rt_clm_check <- function(fit) {
  .rt_check_ordinal_convergence(fit)
  if (!identical(fit$threshold, "flexible")) {
    cli::cli_abort(c(
      "{.fn regtab} supports {.fn ordinal::clm} fits with flexible thresholds only (Stata {.code ologit} cutpoints).",
      "x" = "This fit uses {.val {fit$threshold}} thresholds."
    ), call = NULL)
  }
  if (!is.null(fit$call$nominal) || !is.null(fit$call$scale)) {
    cli::cli_abort("{.fn regtab} does not support {.fn ordinal::clm} fits with {.arg nominal} or {.arg scale} effects.",
                   call = NULL)
  }
  # model.matrix(<clm>) carries no contrasts attribute, so the default rows
  # cannot check treatment coding; do it here (review P0-6).
  mf <- fit$model
  for (v in names(fit$contrasts)) {
    if (!is.null(mf) && !is.null(mf[[v]])) .rt_contrast_base(fit$contrasts[[v]], mf[[v]], v)
  }
  invisible(TRUE)
}

# clm(model = FALSE) keeps neither its model frame nor its design, which
# the rows, labels and statistics need (the storage-option contract of
# Milestone H task H4: a refusal that names the option).
#' @export
tt_regtab_check.clm <- function(fit, i) {
  .rt_check_ordinal_convergence(fit, i)
  if (!is.data.frame(fit$model)) {
    cli::cli_abort(c(
      "{.fn regtab} needs the model frame of this {.fn ordinal::clm} fit (model {i}), which was fitted with {.code model = FALSE}.",
      "i" = "Refit with {.code model = TRUE} (the default)."
    ), call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_regtab_rows.clm <- function(fit, info, ...) {
  .rt_clm_check(fit)
  NextMethod()
}

# Cutpoint rows `cut1`, `cut2`, ...: raw scale, no p-value.
.rt_cut_rows <- function(b, V, cut_terms, level) {
  if (!length(cut_terms)) return(NULL)
  w <- .rt_wald_named(b, V, cut_terms, Inf, level)
  do.call(rbind, lapply(seq_along(cut_terms), function(k) {
    key <- paste0("cut", k)
    r <- .rt_anc_row(key, key, w[k, ], sub = 1e6 + k, role = "cutpoint")
    r$p.value <- NA_real_
    r
  }))
}

#' @export
tt_regtab_ancillary_rows.polr <- function(fit, info, level = 0.95, vce = "stata", ...) {
  V <- tt_vcov(fit, vce)
  .rt_cut_rows(c(stats::coef(fit), fit$zeta), V, names(fit$zeta), level)
}

#' @export
tt_regtab_ancillary_rows.clm <- function(fit, info, level = 0.95, vce = "stata", ...) {
  V <- tt_vcov(fit, vce)
  .rt_cut_rows(stats::coef(fit), V, names(fit$alpha), level)
}

# Constant-only log-likelihood of an ordinal (or multinomial) outcome:
# sum_k n_k log(n_k / n), weighted.
.rt_marginal_ll <- function(y, w = NULL) {
  if (is.null(w)) w <- rep(1, length(y))
  nk <- tapply(w, factor(y), sum)
  nk <- nk[!is.na(nk) & nk > 0]
  sum(nk * log(nk / sum(nk)))
}

.rt_ord_stats <- function(fit, y, w, rank, offset = NULL) {
  s <- tt_model_stats.default(fit, NULL)
  s$rank <- rank
  # Case (frequency) weights: their variance is Stata's ologit [fweight]
  # variance, whose e(N) is the weight sum (probe B10: 17,944; review
  # P2-1, mutation M78).
  s$N <- if (is.null(w)) length(y) else sum(w)
  # With an offset Stata's ologit/oprobit report no e(ll_0), so no pseudo
  # R-squared (probes O1, O5; external review F27: the marginal null
  # ignored the offset).
  if (is.null(offset)) {
    ll0 <- .rt_marginal_ll(y, w)
    s$r2_p <- 1 - s$ll / ll0
  }
  s$aic <- -2 * s$ll + 2 * s$rank
  s$bic <- -2 * s$ll + s$rank * log(s$N)
  s
}

#' @export
tt_model_stats.polr <- function(fit, info, ...) {
  mf <- .rt_polr_frame(fit)
  .rt_ord_stats(fit, stats::model.response(mf), stats::model.weights(mf),
                length(stats::coef(fit)) + length(fit$zeta), stats::model.offset(mf))
}

#' @export
tt_model_stats.clm <- function(fit, info, ...) {
  mf <- fit$model
  w <- if (!is.null(mf)) stats::model.weights(mf) else NULL
  off <- if (!is.null(mf)) stats::model.offset(mf) else NULL
  .rt_ord_stats(fit, fit$y, w, length(fit$alpha) + length(fit$beta), off)
}

# cutlabels (`regtab.ado:1979-1995`): labels split on backslashes like
# models(), applied positionally to rows displayed as `cut#` or `/cut#`.
.rt_parse_cutlabels <- function(cutlabels) {
  if (is.null(cutlabels)) return(NULL)
  if (!is.character(cutlabels) || anyNA(cutlabels)) {
    cli::cli_abort("{.arg cutlabels} must be a character vector of labels.", call = NULL)
  }
  if (length(cutlabels) == 1L && grepl("\\", cutlabels, fixed = TRUE)) {
    cutlabels <- strsplit(cutlabels, "\\", fixed = TRUE)[[1]]
  }
  cutlabels <- trimws(cutlabels)
  cutlabels[nzchar(cutlabels)]
}

# Only rows that are cutpoints (role "cutpoint", H5) are relabelled: a
# covariate named or labelled `cut1` keeps its label. The cutpoint is found
# by its raw name `cut<k>` (`keys`), and its whole label is replaced, as
# tabtools 2.1.12 does (`regtab.ado`, cutlabels(): `_cut_row` and
# `_raw_A == "cut#"`): in the multi-equation layout "Ancillary: cut1"
# becomes the label itself (fixture G04; 2.1.11 left the row unchanged
# there because it matched the displayed label).
.rt_apply_cutlabels <- function(labels, cutlabels, is_cut = rep(TRUE, length(labels)), keys = labels) {
  for (k in seq_along(cutlabels)) {
    hit <- is_cut & grepl(paste0("^/?cut", k, "$"), tolower(trimws(keys)))
    labels[hit] <- cutlabels[k]
  }
  labels
}
