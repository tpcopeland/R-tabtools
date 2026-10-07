# Weighted and robust variance for regtab (milestone 5w, tasks 5.9-5.13).
#
# Stata facts (Stata 17 probes, qa/stata/make_regtab_vce.do ->
# tests/testthat/fixtures/regtab_vce/, read by test-regtab-vce.R; the 5w
# review fixtures, qa/stata/make_regtab_5w_review.do ->
# tests/testthat/fixtures/regtab_5w_review/):
# - ML commands (logit, probit, poisson, glm, nbreg) with [pweight] or
#   vce(robust): the sandwich A^-1 B A^-1 with A the observed information
#   (the vce(oim) bread, which differs from the expected information for
#   non-canonical links: probit's SE moves by 1.5e-3 with the expected one)
#   and B the outer product of the weighted scores, times N/(N - 1); z
#   statistics. nbreg's sandwich is over (beta, ln alpha) jointly.
# - regress with [pweight] or vce(robust): HC1, i.e. the same sandwich times
#   N/(N - k); t(N - k).
# - ML commands with vce(cluster c): scores summed within clusters, times
#   G/(G - 1); z. regress with vce(cluster c): times G/(G - 1) (N - 1)/(N - k),
#   t(G - 1).
# - Observations with a zero probability weight are not in the estimation
#   sample (e(N) excludes them), so N and G count positive weights only.
# - Under [pweight] Stata reports a pseudo-log-likelihood, and regtab shows
#   it with AIC/BIC computed from it (review P0-4, .rt_pw_stats()):
#   sum(w log f(y)) for logit/probit/cloglog, poisson and glm's Gamma
#   (scale 1); glm's Gaussian with the scale sum(w e^2)/N; regress's
#   [aweight] formula (weights normalised to mean 1); stcox's Breslow
#   partial likelihood with the weights normalised to mean 1. logit and
#   probit store a pseudo R-squared against the weighted constant-only
#   model; poisson [pw] stores none.

# ---------------------------------------------------------------------------
# Class support

#' @export
tt_vce_types.lm <- function(fit) {
  if (identical(class(fit)[1], "lm")) c("stata", "model", "robust", "cluster") else c("stata", "model")
}

#' @export
tt_vce_types.glm <- function(fit) {
  if (identical(class(fit)[1], "glm")) c("stata", "model", "robust", "cluster") else c("stata", "model")
}

# MASS::glm.nb (nbreg [pweight], vce(robust), vce(cluster); probe X19).
#' @export
tt_vce_types.negbin <- function(fit) {
  if (identical(class(fit)[1], "negbin")) c("stata", "model", "robust", "cluster") else c("stata", "model")
}

# ---------------------------------------------------------------------------
# The data a fit was estimated on (review P0-1)
#
# A robust or cluster-robust variance pairs each observation's score with
# its cluster, so both must be the rows the model was fitted on. lm fits
# keep their model frame (fit$model) and glm fits their data (fit$data),
# but a coxph or survreg fit keeps neither unless fitted with
# `model = TRUE`: model.frame(), model.matrix() and residuals() then
# re-evaluate the call on the data object *as it is now*, so re-sorting or
# editing that data frame after fitting would silently pair the scores with
# the wrong rows. regtab therefore reads per-row values only from a
# fit-time source: the stored model frame; glm's stored data; or the
# current data when, and only when, the model frame it gives reproduces
# the fit exactly (the stored frame column for column, or for fits without
# one the stored response, weights and linear predictor row for row, for
# coxph its martingale residuals, i.e. strata and risk sets, and subject
# count, and for survreg its log-likelihood, which checks the response
# where survreg(y = FALSE) keeps none), after putting the rows back in the
# fit's order by row name. This
# catches re-sorted, filtered or edited model data; an edit of only a
# grouping column the fit did not keep (a cluster variable, a coxph id
# re-coded in place) cannot be detected, which is why `model = TRUE` or a
# cluster vector is the safe route. Otherwise the variance is an error that
# says how to fix it.

.rt_anchor_classes <- c("lm", "glm", "negbin", "coxph", "clogit", "survreg")

# The fit with its fit-time model frame attached as fit$model (model.frame(),
# model.matrix() and residuals() then use it), or marked `tt_stale` when the
# data no longer reproduce the fit.
.rt_anchor <- function(fit) {
  # polr(model = FALSE): its verified frame, or an error (H-D7).
  if (inherits(fit, "polr") && !is.data.frame(fit$model)) {
    fit$model <- .rt_polr_frame(fit)
    return(fit)
  }
  if (isS4(fit) || !is.list(fit) || is.data.frame(fit) || !class(fit)[1] %in% .rt_anchor_classes) return(fit)
  if (is.data.frame(fit$model) || isTRUE(fit$tt_stale)) return(fit)
  # coxph with tt() terms: its model frame is the tt-expanded one and
  # survival refuses model = TRUE, so it can never be anchored; the data
  # are checked on the rest of the model instead (.rt_tt_frame_ok()), and
  # only paths that pair rows with the fit refuse (review T2B-04).
  if (.rt_has_tt(fit)) {
    if (!.rt_tt_frame_ok(fit)) fit$tt_stale <- TRUE
    return(fit)
  }
  mf <- NULL
  rn <- .rt_fit_rownames(fit)
  if (inherits(fit, "glm") && is.data.frame(fit$data)) {
    mf <- tryCatch(.rt_align_frame(.rt_eval_frame(fit, fit$data), rn), error = function(e) NULL)
    if (!is.null(mf) && !.rt_frame_matches_fit(fit, mf)) mf <- NULL
  }
  if (is.null(mf)) {
    mf <- tryCatch(.rt_align_frame(.rt_eval_frame(fit), rn), error = function(e) NULL)
    if (!is.null(mf) && !.rt_frame_matches_fit(fit, mf)) mf <- NULL
  }
  if (is.null(mf)) {
    fit$tt_stale <- TRUE
    return(fit)
  }
  fit$model <- mf
  fit
}

# The model frame re-evaluated from the call, on `data` (default: the call's
# data as it is now).
# model.frame() with `xlev` (survival passes the fit's xlevels) warns
# "contrasts dropped from factor" for a factor that carries contrasts(),
# although the fit's own contrasts are what regtab uses (backlog, found in
# 7c): that one warning is muffled.
.rt_eval_frame <- function(fit, data = NULL) {
  withCallingHandlers(.rt_eval_frame0(fit, data), warning = function(w) {
    if (startsWith(conditionMessage(w), "contrasts dropped from factor")) invokeRestart("muffleWarning")
  })
}

.rt_eval_frame0 <- function(fit, data = NULL) {
  f2 <- fit
  f2$model <- NULL
  f2$tt_stale <- NULL
  # survival rewrites a survreg formula's cluster() term into a `cluster =`
  # argument of the call, which model.frame(<survreg>) leaves out: the
  # re-evaluated frame then has no "(cluster)" column and keeps the rows
  # whose cluster is missing (survival::lung, inst; external review F26).
  if (inherits(fit, "survreg") && !is.null(fit$call$cluster)) {
    fc <- fit$call
    keep <- intersect(names(fc), c("formula", "data", "weights", "subset", "na.action", "cluster"))
    mc <- fc[c(1L, match(keep, names(fc)))]
    mc[[1L]] <- quote(stats::model.frame)
    mc$formula <- fit$terms
    mc$xlev <- fit$xlevels
    if (!is.null(data)) mc$data <- data
    env <- environment(fit$terms) %||% parent.frame()
    return(eval(mc, env))
  }
  if (is.null(data)) return(stats::model.frame(f2))
  # Pass `data` by value: survival >= 3.8-12 model.frame.coxph() splices the
  # unevaluated argument into the stored call and evaluates it in the
  # formula environment, where the symbol `data` can find utils::data.
  do.call(stats::model.frame, list(f2, data = data))
}

# The call's data object as it is now (NULL if it cannot be evaluated).
.rt_call_data <- function(fit) {
  cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (is.null(cl) || is.null(cl$data) || is.null(env)) return(NULL)
  tryCatch(eval(cl$data, env), error = function(e) NULL)
}

.rt_num_equal <- function(a, b, tol = 1e-10) {
  a <- as.vector(unclass(a))
  b <- as.vector(unclass(b))
  length(a) == length(b) && isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE))
}

# Two model frames hold the same rows (names, values, row names).
.rt_same_frame <- function(a, b) {
  if (!is.data.frame(a) || !is.data.frame(b) || nrow(a) != nrow(b) ||
      !identical(names(a), names(b)) || !identical(rownames(a), rownames(b))) return(FALSE)
  for (k in names(a)) {
    x <- a[[k]]
    y <- b[[k]]
    same <- if (is.factor(x) || is.character(x) || is.factor(y) || is.character(y)) {
      identical(as.character(x), as.character(y))
    } else .rt_num_equal(x, y, 1e-12)
    if (!same) return(FALSE)
  }
  TRUE
}

# The response a glm was fitted to, on its fitting scale (proportions for
# binomial), from a model frame (as glm's family initialize() forms it).
.rt_glm_response <- function(fam, y) {
  if (fam$family %in% c("binomial", "quasibinomial")) {
    if (is.factor(y)) return(as.numeric(y != levels(y)[1L]))
    if (is.matrix(y) && ncol(y) == 2L) {
      n <- rowSums(y)
      return(ifelse(n == 0, 0, y[, 1L] / n))
    }
  }
  as.numeric(y)
}

# The response of a glm fit as it was fitted: fit$y, or, for glm(y = FALSE),
# rebuilt from the working residuals, which glm.fit() stores as
# (y - mu) / (d mu / d eta) at the final estimates (external review F05).
.rt_glm_y <- function(fit) {
  if (!is.null(fit$y)) return(fit$y)
  eta <- fit$linear.predictors
  r <- fit$residuals
  if (is.null(eta) || is.null(r) || length(r) != length(eta)) {
    cli::cli_abort(c("The response of this {.cls {class(fit)[1]}} fit is not stored ({.code y = FALSE}).",
                     "i" = "Refit with {.code y = TRUE} (the default)."), call = NULL)
  }
  fit$fitted.values + r * fit$family$mu.eta(eta)
}

# Linear predictor of a re-evaluated frame, compared with the fit's own:
# TRUE when the frame reproduces it row for row (for coxph up to the
# centring constant).
.rt_lp_matches <- function(fit, mf, lp_fit, centred = FALSE) {
  b <- stats::coef(fit)
  X <- tryCatch(if (inherits(fit, "coxph")) stats::model.matrix(fit, data = mf) else
    stats::model.matrix(stats::terms(fit), mf, contrasts.arg = fit$contrasts),
    error = function(e) NULL)
  if (is.null(X) || nrow(X) != length(lp_fit) || !all(names(b) %in% colnames(X))) return(FALSE)
  b[is.na(b)] <- 0
  lp <- drop(X[, names(b), drop = FALSE] %*% b)
  off <- stats::model.offset(mf)
  if (!is.null(off)) lp <- lp + off
  d <- lp - lp_fit
  if (centred) d <- d - mean(d)
  all(is.finite(d)) && max(abs(d)) <= 1e-7 * max(1, max(abs(lp_fit)))
}

# Row names of the observations a fit used, in its order (NULL if unknown):
# model-frame row names, which lm/glm keep on their fitted values and coxph
# on its response.
.rt_fit_rownames <- function(fit) {
  rn <- if (inherits(fit, "coxph") || inherits(fit, "survreg")) rownames(fit$y) else
    names(fit$fitted.values %||% fit$linear.predictors)
  if (is.null(rn) || anyNA(rn) || anyDuplicated(rn)) NULL else rn
}

# A re-evaluated frame put back in the fit's row order by row name (the data
# re-sorted with their row names kept, review of the fixes: F36), keeping
# the model-frame attributes.
.rt_align_frame <- function(mf, rn) {
  if (is.null(rn) || !is.data.frame(mf) || nrow(mf) != length(rn) || identical(rownames(mf), rn)) return(mf)
  idx <- match(rn, rownames(mf))
  if (anyNA(idx)) return(mf)
  out <- mf[idx, , drop = FALSE]
  for (a in c("terms", "na.action")) attr(out, a) <- attr(mf, a)
  out
}

# Whether a re-evaluated model frame reproduces the fit's stored response,
# weights and linear predictor (lm keeps its frame and is compared column
# for column instead). For coxph also its stored martingale residuals,
# recomputed on the frame at the fitted coefficients, which depend on the
# strata and risk sets, and its subject count; for survreg its
# log-likelihood, recomputed likewise. Row order is taken from the fit's
# row names where it keeps them.
.rt_frame_matches_fit <- function(fit, mf) {
  if (!is.data.frame(mf)) return(FALSE)
  if (is.data.frame(fit$model)) {
    return(.rt_same_frame(.rt_align_frame(mf, rownames(fit$model)), fit$model))
  }
  # The weights in every class (Codex audit 2, F01 review: coxph and
  # survreg store none when all are 1, and binomial was not compared).
  if (nrow(mf) == length(.rt_frame_fit_weights(fit)) &&
      !.rt_num_equal(.rt_frame_obs_weights(fit, mf), .rt_frame_fit_weights(fit))) return(FALSE)
  if (inherits(fit, "coxph")) {
    # coxph(y = FALSE) keeps no response: its number of failures and the
    # martingale residuals below, recomputed from the frame's response,
    # check it instead.
    if (nrow(mf) != length(fit$linear.predictors)) return(FALSE)
    y <- stats::model.response(mf)
    if (!is.null(fit$y) && !.rt_num_equal(as.matrix(y), as.matrix(fit$y), 1e-7)) return(FALSE)
    if (!is.null(fit$n.id) && !is.null(mf[["(id)"]]) && length(unique(mf[["(id)"]])) != fit$n.id) return(FALSE)
    if (!.rt_cox_nevent_matches(fit, y)) return(FALSE)
    if (!.rt_lp_matches(fit, mf, fit$linear.predictors, centred = TRUE)) return(FALSE)
    return(.rt_cox_resid_match(fit, mf))
  }
  if (inherits(fit, "survreg")) {
    if (nrow(mf) != length(fit$linear.predictors)) return(FALSE)
    # The response too (Codex audit 2, F01: an edit of only the times or
    # statuses passed): the stored one where kept, and in any case the
    # log-likelihood it gives at the fitted coefficients and scale, the one
    # fit-time trace of it that survreg(y = FALSE) keeps.
    y <- tryCatch(.rt_survreg_response(fit, mf), error = function(e) NULL)
    if (is.null(y) || (!is.null(fit$y) && !.rt_num_equal(y, unclass(fit$y), 1e-7))) return(FALSE)
    if (!.rt_lp_matches(fit, mf, fit$linear.predictors)) return(FALSE)
    return(.rt_survreg_ll_matches(fit, mf, fit$linear.predictors))
  }
  if (inherits(fit, "glm")) {
    n <- length(fit$linear.predictors)
    if (nrow(mf) != n) return(FALSE)
    y <- tryCatch(.rt_glm_response(fit$family, stats::model.response(mf)), error = function(e) NULL)
    pw <- fit$prior.weights
    # binomial's initialize() sets y to 0 where the weight is 0.
    pos <- if (length(pw) == n) pw > 0 else rep(TRUE, n)
    if (is.null(y) || length(y) != n || !.rt_num_equal(y[pos], .rt_glm_y(fit)[pos], 1e-8)) return(FALSE)
    return(.rt_lp_matches(fit, mf, fit$linear.predictors))
  }
  # lm with model = FALSE.
  n <- length(fit$fitted.values)
  if (nrow(mf) != n) return(FALSE)
  y <- as.numeric(stats::model.response(mf))
  if (!.rt_num_equal(y, fit$fitted.values + fit$residuals, 1e-8)) return(FALSE)
  .rt_lp_matches(fit, mf, fit$fitted.values)
}

# The weights a fit was fitted with, one per observation (ones when it has
# none: coxph and survreg store none when all are 1; glm's prior weights),
# and those a re-evaluated frame gives, formed alike (a binomial glm with a
# two-column response: the weights times the trials, as its initialize()
# forms them). Codex audit 2, F01 review: an edit of only the weights
# passed the re-sorted-data check.
.rt_frame_fit_weights <- function(fit) {
  n <- length(fit$linear.predictors %||% fit$fitted.values)
  w <- if (inherits(fit, "glm")) fit$prior.weights else fit$weights
  if (is.null(w)) rep(1, n) else as.numeric(w)
}

.rt_frame_obs_weights <- function(fit, mf) {
  w <- stats::model.weights(mf)
  w <- if (is.null(w)) rep(1, nrow(mf)) else as.numeric(w)
  if (inherits(fit, "glm") && fit$family$family %in% c("binomial", "quasibinomial")) {
    y <- stats::model.response(mf)
    if (is.matrix(y) && ncol(y) == 2L) w <- w * rowSums(y)
  }
  w
}

# A survreg fit that ran out of iterations (survreg() warns "Ran out of
# iterations and did not converge"): its fit$loglik is then not the
# log-likelihood at its coefficients, so it cannot check a frame (Codex
# audit 2, F01 review). The limit is the call's survreg.control() iter.max
# (its `maxiter` unless given).
.rt_survreg_unconverged <- function(fit) {
  if (!inherits(fit, "survreg") || is.null(fit$iter)) return(FALSE)
  mx <- tryCatch({
    cl <- fit$call
    env <- environment(stats::formula(fit)) %||% globalenv()
    ctl <- if (!is.null(cl$control)) eval(cl$control, env) else {
      a <- as.list(cl)[intersect(names(cl), c("maxiter", "iter.max"))]
      do.call(survival::survreg.control, lapply(a, eval, envir = env))
    }
    ctl$iter.max
  }, error = function(e) NULL)
  is.numeric(mx) && length(mx) == 1L && fit$iter[1L] >= mx
}

# A re-evaluated frame's survival response as survreg() stores it in fit$y
# (a matrix; an interval time starting at 0 under a log-time distribution
# recoded as left-censored at its end, as survreg() does).
.rt_survreg_response <- function(fit, mf) {
  Y <- stats::model.response(mf)
  if (!inherits(Y, "Surv") || !attr(Y, "type") %in% c("right", "left", "interval")) return(NULL)
  dl <- if (is.list(fit$dist)) fit$dist else survival::survreg.distributions[[fit$dist]]
  type <- attr(Y, "type")
  Y <- unclass(Y)
  attr(Y, "type") <- NULL
  # "Log logisit" reproduces a typo in survreg() itself (survival 3.8.6),
  # deliberately: the loglogistic distribution is named "Log logistic", so
  # survreg() never recodes its intervals starting at 0, and neither may
  # this copy of its response.
  if (type == "interval" && dl$name %in% c("Weibull", "Exponential", "Rayleigh", "Log Normal", "Log logisit")) {
    fix <- Y[, 1L] == 0 & Y[, 3L] == 3
    if (any(fix)) Y[fix, ] <- cbind(Y[fix, 2L], 1, 2)
  }
  Y
}

# survreg's log-likelihood at the fitted coefficients and scale on a
# re-evaluated frame (lp: its linear predictor, in the frame's row order),
# formed as survreg() forms it: an exact time's density on the transformed
# scale plus the log-Jacobian of the transform, a right- (left-) censored
# time's survivor (distribution) function, an interval's difference of the
# two; weights multiply. TRUE when it equals the fit's (Codex audit 2, F01).
# A fit that did not converge stores no log-likelihood at its coefficients:
# TRUE when it keeps its response (checked directly by the callers), FALSE
# for survreg(y = FALSE), which then cannot be checked (Codex audit 2, F01
# review; .rt_abort_stale() names the non-convergence).
.rt_survreg_ll_matches <- function(fit, mf, lp) {
  if (.rt_survreg_unconverged(fit)) return(!is.null(fit$y))
  ok <- tryCatch({
    Y <- .rt_survreg_response(fit, mf)
    type <- attr(stats::model.response(mf), "type")
    dl <- if (is.list(fit$dist)) fit$dist else survival::survreg.distributions[[fit$dist]]
    st <- Y[, ncol(Y)]
    if (type == "left") st <- 2 - st
    t1 <- Y[, 1L]
    t2 <- if (type == "interval") Y[, 2L] else t1
    ex <- st == 1
    jac <- numeric(length(t1))
    if (!is.null(dl$trans)) {
      jac[ex] <- log(dl$dtrans(t1[ex]))
      t1 <- dl$trans(t1)
      t2[st == 3] <- dl$trans(t2[st == 3])
    }
    base <- if (is.null(dl$dist)) dl else if (is.atomic(dl$dist)) survival::survreg.distributions[[dl$dist]] else dl$dist
    # One scale per stratum under strata(): survreg names them by level.
    sig <- rep(fit$scale, length.out = length(t1))
    if (length(fit$scale) > 1L) {
      sv <- survival::untangle.specials(stats::terms(fit), "strata", 1)$vars
      sk <- if (length(sv) == 1L) mf[[sv]] else survival::strata(mf[, sv], shortlabel = TRUE)
      sig <- unname(fit$scale[as.character(sk)])
    }
    parms <- fit$parms %||% base$parms
    d1 <- base$density((t1 - lp) / sig, parms)
    ll <- ifelse(ex, log(d1[, 3L]) - log(sig) + jac, ifelse(st == 0, log(d1[, 2L]), log(d1[, 1L])))
    i3 <- st == 3
    if (any(i3)) ll[i3] <- log(base$density((t2[i3] - lp[i3]) / sig[i3], parms)[, 1L] - d1[i3, 1L])
    w <- stats::model.weights(mf) %||% rep(1, length(ll))
    .rt_close(sum((w * ll)[w != 0]), fit$loglik[length(fit$loglik)], 1e-8)
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# coxph's martingale residuals recomputed on a re-evaluated frame at the
# fitted coefficients (no iterations) equal the stored ones: the strata and
# risk sets are those of the fit. With `ll = TRUE` its partial
# log-likelihood is compared instead, which does not depend on the row
# order (.rt_reordered_ok(); Codex audit 2, F01). coxph.fit() and
# agreg.fit() compute ties = "exact" as Breslow's, whose residuals the exact
# fitters store too but whose log-likelihood differs, so an exact fit's
# residuals are compared as a multiset instead (Codex audit 2, F01 review).
.rt_cox_resid_match <- function(fit, mf, ll = FALSE) {
  if (identical(fit$method, "exact")) ll <- if (ll) "sorted" else FALSE
  if (is.null(fit$residuals) && !isTRUE(ll)) return(FALSE)
  r <- .rt_cox_refit0(fit, mf)
  if (is.null(r)) return(FALSE)
  ok <- tryCatch({
    res <- .rt_cox_stored_resid(fit)
    if (identical(ll, "sorted")) .rt_num_equal(sort(r$residuals), sort(res), 1e-7) else
      if (ll) .rt_close(r$loglik[length(r$loglik)], fit$loglik[length(fit$loglik)], 1e-8) else
        .rt_num_equal(r$residuals, res, 1e-7)
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# A frame's number of failures equals the fit's (fit$nevent, unweighted,
# kept also with y = FALSE): the martingale residuals and the partial
# log-likelihood cannot see a failure that is alone at risk at its time
# (review of the backlog fix: its status flipped, Events 41.13 for 42.13).
.rt_cox_nevent_matches <- function(fit, y) {
  if (is.null(fit$nevent) || is.null(y)) return(TRUE)
  y <- as.matrix(unclass(y))
  isTRUE(sum(y[, ncol(y)] == 1) == fit$nevent)
}

# coxph's fitter run on a frame at the fitted coefficients, no iterations
# (its residuals and log-likelihood), or NULL when it fails.
.rt_cox_refit0 <- function(fit, mf) {
  tryCatch({
    X <- stats::model.matrix(fit, data = mf)
    b <- stats::coef(fit)
    X <- X[, names(b), drop = FALSE]
    y <- stats::model.response(mf)
    sn <- grep("^strata\\(", names(mf), value = TRUE)
    st <- if (length(sn)) as.integer(interaction(mf[sn], drop = TRUE)) else NULL
    off <- stats::model.offset(mf)
    w <- stats::model.weights(mf)
    fitter <- if (ncol(y) == 2L) survival::coxph.fit else survival::agreg.fit
    init <- b
    init[is.na(init)] <- 0
    suppressWarnings(fitter(X, y, st, off, init = init,
                            control = survival::coxph.control(iter.max = 0),
                            weights = w, method = fit$method, rownames = NULL))
  }, error = function(e) NULL)
}

# The fit's stored martingale residuals, one per observation (na.exclude
# pads them with NA).
.rt_cox_stored_resid <- function(fit) {
  res <- fit$residuals
  if (inherits(fit$na.action, "exclude")) res <- res[!is.na(res)]
  res
}

# Stop when a computation needs the rows of a fit whose data changed after
# fitting (see above).
.rt_require_frame <- function(fit, what) {
  if (isS4(fit) || !is.list(fit) || !class(fit)[1] %in% .rt_anchor_classes) return(fit)
  fit <- .rt_anchor(fit)
  if (is.data.frame(fit$model)) return(fit)
  cls <- class(fit)[1]
  if (.rt_has_tt(fit)) {
    cli::cli_abort(c(
      "The {what} of this {.cls {cls}} model cannot be computed: it needs the model's rows, and a model with {.fn tt} terms cannot be re-evaluated row by row (its model frame is expanded over the event times, and survival does not allow {.code model = TRUE} with {.fn tt}).",
      "i" = "Use {.code vce = \"stata\"} or {.code vce = \"model\"} (the fit's stored variance); for a robust variance, fit with {.code cluster =} or {.code robust = TRUE}, whose sandwich survival stores in the fit (regtab applies Stata's G/(G - 1) to it)."
    ), call = NULL)
  }
  if (.rt_survreg_unconverged(fit) && is.null(fit$y)) .rt_abort_unconverged(fit, "This model", paste0("its ", what, " needs the data it was fitted on, and "))
  cli::cli_abort(c(
    "The {what} of this {.cls {cls}} model needs the data it was fitted on, which have changed since or cannot be checked (the current data no longer reproduce its response, weights, linear predictor and risk sets: re-sorted, filtered or edited after fitting, or a term such as {.fn tt} that cannot be re-evaluated).",
    "i" = "Refit it on the current data, or with {.code model = TRUE} so that the model keeps its data."
  ), call = NULL)
}

# Whether the data as they are now hold a stale fit's observations in
# another order (row names reset, so .rt_anchor() cannot put them back):
# the same responses, linear predictors and weights, as a multiset, and for
# coxph and survreg the same (partial) log-likelihood. Quantities that
# do not depend on the row order (labels, factor levels, N, the
# log-likelihood's pieces, a stored robust variance) may then be read from
# them, as the milestone 5w review allowed (.rt_cluster_count_reordered());
# anything that pairs rows still refuses (.rt_require_frame()).
.rt_reordered_ok <- function(fit) {
  mf <- tryCatch(.rt_eval_frame(fit), error = function(e) NULL)
  lp_fit <- fit$linear.predictors %||% fit$fitted.values
  if (!is.data.frame(mf) || is.null(lp_fit) || nrow(mf) != length(lp_fit)) return(FALSE)
  ok <- tryCatch({
    b <- stats::coef(fit)
    X <- if (inherits(fit, "coxph")) stats::model.matrix(fit, data = mf) else
      stats::model.matrix(stats::terms(fit), mf, contrasts.arg = fit$contrasts)
    b[is.na(b)] <- 0
    lp <- drop(X[, names(b), drop = FALSE] %*% b)
    off <- stats::model.offset(mf)
    if (!is.null(off)) lp <- lp + off
    if (inherits(fit, "coxph")) lp <- lp - mean(lp) + mean(lp_fit)
    y_now <- as.matrix(unclass(stats::model.response(mf)))
    y_fit <- if (inherits(fit, "glm")) .rt_glm_y(fit) else if (!is.null(fit$y)) fit$y else
      if (identical(class(fit)[1], "lm")) fit$fitted.values + fit$residuals else NULL
    if (inherits(fit, "glm")) y_now <- as.matrix(.rt_glm_response(fit$family, stats::model.response(mf)))
    # The log-likelihood at the fitted coefficients, a sum over the rows,
    # checks what the multiset below cannot: coxph(y = FALSE) and
    # survreg(y = FALSE) keep no response (Codex audit 2, F01: an edit of
    # only the times or statuses passed on the linear predictors alone), and
    # it also covers the weights, strata and risk sets, whatever the fit
    # stores (Codex audit 2, F01 review).
    if (inherits(fit, "survreg") && !.rt_survreg_ll_matches(fit, mf, lp)) return(FALSE)
    if (inherits(fit, "coxph") && !.rt_cox_nevent_matches(fit, stats::model.response(mf))) return(FALSE)
    if (inherits(fit, "coxph") && !.rt_cox_resid_match(fit, mf, ll = TRUE)) return(FALSE)
    if (is.null(y_fit)) {
      y_now <- matrix(0, length(lp), 1L)
      y_fit <- y_now
    }
    y_fit <- as.matrix(unclass(y_fit))
    # coxph: each row's martingale residual too (status minus its expected
    # count), which ties the statuses to the rows where coxph(y = FALSE)
    # keeps none, so that quantities pairing statuses with weights (the
    # weighted failures, Stata's e(N_fail) after stset [pweight], and the
    # pseudo-log-likelihood) can be read from the re-sorted data (backlog:
    # a weighted coxph(y = FALSE) on re-sorted data dropped its Events).
    # It does not tell a failure alone at risk at its time (residual 1 - 1
    # = 0, as if censored, and no term in the log-likelihood with weight 1)
    # from a censored row: the number of failures, checked above, does.
    r_now <- r_fit <- NULL
    if (inherits(fit, "coxph")) {
      r_fit <- .rt_cox_stored_resid(fit)
      r_now <- .rt_cox_refit0(fit, mf)$residuals
      if (is.null(r_fit) || length(r_now) != length(r_fit)) return(FALSE)
    }
    # The weights with each row (Codex audit 2, F01 review: a weight edit
    # passed and rewrote N, events and the log-likelihood).
    a <- cbind(y_now, signif(lp, 10), .rt_frame_obs_weights(fit, mf), r_now)
    z <- cbind(y_fit, signif(lp_fit, 10), .rt_frame_fit_weights(fit), r_fit)
    identical(dim(a), dim(z)) &&
      .rt_num_equal(a[do.call(order, as.data.frame(a)), , drop = FALSE],
                    z[do.call(order, as.data.frame(z)), , drop = FALSE], 1e-8)
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# A coxph fit with tt() terms.
.rt_has_tt <- function(fit) {
  inherits(fit, "coxph") && !is.null(tryCatch(attr(stats::terms(fit), "specials")$tt, error = function(e) NULL))
}

# The data of a tt() coxph as they are now give its rows: the model frame
# with each tt(x) read as x (no expansion over the event times) has the
# fit's number of observations and events and the same event times (fit$y
# holds the expanded records). The linear predictor cannot be checked (the
# tt terms depend on the event times), so this is weaker than the check of
# .rt_frame_matches_fit(); it is what labels, reference rows and N need,
# and nothing that pairs rows with the fit uses it.
.rt_tt_frame_ok <- function(fit) !is.null(.rt_tt_frame(fit))

# That frame (with the fit's cluster()/id column), or NULL.
.rt_tt_frame <- function(fit) {
  tryCatch({
    cl <- stats::getCall(fit)
    env <- environment(stats::formula(fit))
    txt <- paste(deparse(stats::formula(fit), width.cutoff = 500L), collapse = " ")
    f0 <- stats::as.formula(gsub("\\btt\\(", "(", txt), env = env)
    keep <- intersect(names(cl), c("data", "weights", "subset", "na.action", "cluster", "id"))
    mc <- cl[c(1L, match(keep, names(cl)))]
    mc[[1L]] <- quote(stats::model.frame)
    mc$formula <- f0
    mf <- eval(mc, env)
    y <- as.matrix(stats::model.response(mf))
    ev <- y[, ncol(y)] == 1
    # fit$y, fit$weights and the linear predictor are over the expanded
    # records; the events are the expanded records that end in one.
    yf <- as.matrix(fit$y %||% matrix(numeric(), 0L, 2L))
    evf <- yf[, ncol(yf)] == 1
    ok <- nrow(mf) == fit$n && sum(ev) == fit$nevent &&
      (!nrow(yf) || .rt_num_equal(sort(y[ev, ncol(y) - 1L]), sort(yf[evf, ncol(yf) - 1L]), 1e-10))
    if (ok) mf else NULL
  }, error = function(e) NULL)
}

# A weighted tt() coxph's case weights per observation (not per expanded
# record) with its tt-free frame, list(mf, w, paired), or NULL. survival
# expands a tt() fit into one record per event time of its stratum at which
# the observation is at risk (start < t <= stop), and fit$weights holds the
# weights of those records, so their sum is not Stata's weighted subject
# count and their number not the n that normalises the weights (backlog:
# Subjects 7,925.69 for 375.19 and a wrong pseudo-log-likelihood). The
# frame's times are rounded as survival rounds them (aeqSurv(), when the
# fit used timefix) before the event times are counted.
#
# With y = TRUE (`paired`) the frame's expansion, one (event time, status,
# weight) record per observation and event time at risk, must equal the
# fit's records (fit$y and fit$weights) as a multiset, which ties each
# failure's weight to its status and time: only then are the frame's
# statuses and weights used together (Events, the pseudo-log-likelihood).
# With y = FALSE only the weights can be compared, each repeated as often
# as its observation is expanded (a multiset), which pairs no weight with a
# status (review of the backlog fix: swapping the weights of a failure and
# a censored observation at the same time passed that check and moved
# Events and the log-likelihood) and not even fixes their sum (weights 1,
# 2, 2 of observations expanded 2, 1, 1 times edited to 2, 1, 1 give the
# same multiset, and a sum of 4 for 5). So with y = FALSE the frame is used
# only when all the fit's weights are equal, where the multiset check is
# exact (H-D6: blank, with a note, rather than a number that may be wrong).
.rt_tt_weights <- function(fit) {
  if (is.null(fit$weights)) return(NULL)
  mf <- .rt_tt_frame(fit)
  if (is.null(mf)) return(NULL)
  paired <- !is.null(fit$y)
  ok <- tryCatch({
    w <- stats::model.weights(mf)
    yr <- stats::model.response(mf)
    if (!isFALSE(fit$timefix)) yr <- survival::aeqSurv(yr)
    y <- as.matrix(unclass(yr))
    stop_t <- y[, ncol(y) - 1L]
    start_t <- if (ncol(y) == 3L) y[, 1L] else rep(-Inf, nrow(y))
    ev <- y[, ncol(y)] == 1
    sn <- grep("^strata\\(", names(mf), value = TRUE)
    st <- if (length(sn)) as.integer(interaction(mf[sn], drop = TRUE)) else rep(1L, nrow(y))
    rec <- vector("list", length(unique(st)))
    k <- integer(nrow(y))
    for (s in unique(st)) {
      i <- which(st == s)
      ut <- sort(unique(stop_t[i][ev[i]]))
      lo <- findInterval(start_t[i], ut)
      k[i] <- findInterval(stop_t[i], ut) - lo
      if (paired) {
        ii <- rep(i, k[i])
        tj <- ut[sequence(k[i], from = lo + 1L)]
        rec[[match(s, unique(st))]] <- cbind(tj, as.numeric(ev[ii] & tj == stop_t[ii]), as.numeric(w)[ii])
      }
    }
    if (is.null(w) || sum(k) != length(fit$weights)) {
      FALSE
    } else if (paired) {
      a <- do.call(rbind, rec)
      yf <- as.matrix(unclass(fit$y))
      z <- cbind(yf[, ncol(yf) - 1L], yf[, ncol(yf)], as.numeric(fit$weights))
      identical(dim(a), dim(z)) &&
        .rt_num_equal(a[do.call(order, as.data.frame(a)), , drop = FALSE],
                      z[do.call(order, as.data.frame(z)), , drop = FALSE], 1e-10)
    } else {
      length(unique(fit$weights)) == 1L &&
        .rt_num_equal(sort(rep(as.numeric(w), k)), sort(fit$weights), 1e-10)
    }
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) return(NULL)
  list(mf = mf, w = as.numeric(stats::model.weights(mf)), paired = paired)
}

# regtab() on a fit whose data changed after fitting or cannot be found.
.rt_abort_stale <- function(fit, i) {
  cls <- class(fit)[1]
  if (.rt_has_tt(fit)) {
    cli::cli_abort(c(
      "The data model {i} ({.cls {cls}}) was fitted on have changed since, or cannot be found.",
      "i" = "A model with {.fn tt} terms keeps no copy of its data (survival does not allow {.code model = TRUE} with {.fn tt}), so regtab reads the data again and uses them only when they give the fit's observations, events and event times: here they do not.",
      "i" = "Refit it on the current data."
    ), call = NULL)
  }
  if (.rt_survreg_unconverged(fit) && is.null(fit$y)) .rt_abort_unconverged(fit, paste("Model", i))
  cli::cli_abort(c(
    "The data model {i} ({.cls {cls}}) was fitted on have changed since, or cannot be found.",
    "i" = "A {.cls {cls}} fit without {.code model = TRUE} keeps no copy of its data, so regtab reads the data again and uses them only when they reproduce the fit (its response, weights and linear predictor): here they do not (re-sorted without their row names, filtered, edited or removed after fitting, or a term such as {.fn tt} that cannot be re-evaluated).",
    "i" = "Refit it with {.code model = TRUE}, or on the current data."
  ), call = NULL)
}

# A survreg(y = FALSE) fit that did not converge: its data cannot be checked
# (.rt_survreg_ll_matches(); Codex audit 2, F01 review).
.rt_abort_unconverged <- function(fit, lead, need = "") {
  cli::cli_abort(c(
    "{lead} ({.cls survreg}, fitted with {.code y = FALSE}) did not converge (it ran out of iterations after {fit$iter[1L]}): {need}regtab cannot check that the data as they are now are the ones it was fitted on.",
    "i" = "A {.cls survreg} fit without {.code model = TRUE} keeps no copy of its data; with {.code y = FALSE} its log-likelihood is the only trace of its response, and a fit that did not converge stores none at its coefficients.",
    "i" = "Refit it to convergence (a larger {.arg maxiter} in {.fn survival::survreg.control}), or with {.code y = TRUE} or {.code model = TRUE}."
  ), call = NULL)
}

# ---------------------------------------------------------------------------
# Cluster variable

#' Resolve a `cluster` specification to one value per model-frame row
#'
#' `cluster` is a one-sided formula naming one variable (`~id`), a column
#' name (`"id"`), or a vector with one value per observation of the model
#' frame (or per row of the data before `na.action` dropped rows, which are
#' then dropped from it too). A variable is read from a fit-time source (see
#' "The data a fit was estimated on" above): the model frame, glm's stored
#' data, or the data object as it is now when its model frame reproduces
#' the fit, row-matched by row name. Missing cluster values are an error, as
#' in Stata.
#' @param fit A fitted model.
#' @param cluster The specification.
#' @param n Number of model-frame rows.
#' @return A vector of length `n`.
#' @keywords internal
#' @noRd
.rt_resolve_cluster <- function(fit, cluster, n) {
  if (inherits(cluster, "formula")) {
    vars <- all.vars(cluster)
    if (length(cluster) != 2L || length(vars) != 1L) {
      cli::cli_abort(c("{.arg cluster} must name one variable, e.g. {.code ~id}.",
                       "i" = "Stata's {.code vce(cluster)} takes a single cluster variable."), call = NULL)
    }
    return(.rt_cluster_column(fit, vars, n))
  }
  if (is.character(cluster) && length(cluster) == 1L && n > 1L) {
    return(.rt_cluster_column(fit, cluster, n))
  }
  if (!is.atomic(cluster) && !is.factor(cluster)) {
    cli::cli_abort("{.arg cluster} must be a one-sided formula, a column name, or a vector.", call = NULL)
  }
  cl <- cluster
  if (length(cl) != n) {
    na <- tryCatch(stats::na.action(fit), error = function(e) NULL)
    if (length(na) && length(cl) == n + length(na)) {
      cl <- cl[-as.integer(na)]
    } else {
      cli::cli_abort(c("{.arg cluster} has {length(cluster)} value{?s}; the model has {n} observation{?s}.",
                       "i" = "Give one value per observation, or name a column: {.code cluster = ~id}."),
                     call = NULL)
    }
  }
  .rt_cluster_check_na(cl)
}

.rt_cluster_check_na <- function(cl) {
  if (anyNA(cl)) {
    cli::cli_abort("{.arg cluster} has missing values; every observation needs a cluster.", call = NULL)
  }
  cl
}

.rt_cluster_column <- function(fit, v, n) {
  fit <- .rt_require_frame(fit, "cluster variable")
  mf <- if (!isS4(fit) && is.data.frame(fit$model)) fit$model else
    tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (!is.null(mf) && v %in% names(mf) && nrow(mf) == n) return(.rt_cluster_check_na(mf[[v]]))
  rows <- if (!is.null(mf)) rownames(mf) else NULL
  pick <- function(src) {
    if (!is.data.frame(src) || !v %in% names(src)) return(NULL)
    idx <- if (!is.null(rows)) match(rows, rownames(src)) else NULL
    if (!is.null(idx) && length(idx) == n && !anyNA(idx)) return(src[[v]][idx])
    if (is.null(rows) && nrow(src) == n) return(src[[v]])
    NULL
  }
  # glm keeps the data it was fitted on.
  if (!isS4(fit) && is.data.frame(fit$data)) {
    x <- pick(fit$data)
    if (!is.null(x)) return(.rt_cluster_check_na(x))
  }
  # Otherwise the data as they are now, only if they reproduce the fit.
  src <- .rt_call_data(fit)
  # A fit whose call names no data (mice's with(imp, lm(y ~ x)) evaluates
  # the call inside each completed dataset): the formula's environment
  # holds the dataset's columns (C4 review F1).
  if (is.null(src)) {
    x <- .rt_cluster_from_env(fit, v, n, mf)
    if (!is.null(x)) return(.rt_cluster_check_na(x))
  }
  if (!is.data.frame(src) || !v %in% names(src)) {
    cli::cli_abort(c("Cluster variable {.var {v}} is not in the model's data.",
                     "i" = "Pass the cluster values as a vector instead."), call = NULL)
  }
  now <- tryCatch(.rt_align_frame(.rt_eval_frame(fit), rownames(mf)), error = function(e) NULL)
  if (is.null(mf) || is.null(now) || !.rt_same_frame(now, mf)) {
    cli::cli_abort(c(
      "Cluster variable {.var {v}} is not in the model frame, and the data the model was fitted on have changed since (re-sorted, filtered or edited), so its values cannot be matched to the model's observations.",
      "i" = "Refit the model on the current data, or pass the cluster values as a vector in the order of the fitted observations."
    ), call = NULL)
  }
  x <- pick(src)
  if (is.null(x)) {
    cli::cli_abort(c("Cluster variable {.var {v}} cannot be matched to the model's observations.",
                     "i" = "Pass the cluster values as a vector with one value per observation."), call = NULL)
  }
  .rt_cluster_check_na(x)
}

# The cluster variable from the formula's own environment (not its
# parents), for a call without `data =`: used only when that environment
# still reproduces the model frame, and matched to its rows by row name
# (the positions of the rows na.action kept) or, with every row kept, in
# order.
.rt_cluster_from_env <- function(fit, v, n, mf) {
  cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  if (is.null(cl) || !is.null(cl$data) || is.null(mf)) return(NULL)
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (!is.environment(env) || !exists(v, envir = env, inherits = FALSE)) return(NULL)
  x <- get(v, envir = env, inherits = FALSE)
  if (!is.atomic(x) && !is.factor(x)) return(NULL)
  now <- tryCatch(.rt_align_frame(.rt_eval_frame(fit), rownames(mf)), error = function(e) NULL)
  if (is.null(now) || !.rt_same_frame(now, mf)) return(NULL)
  if (length(x) == n) return(x)
  idx <- suppressWarnings(as.integer(rownames(mf)))
  if (length(idx) == n && !anyNA(idx) && all(idx >= 1L & idx <= length(x))) return(x[idx])
  NULL
}

# Number of distinct clusters.
.rt_n_clusters <- function(cl) length(unique(cl))

# ---------------------------------------------------------------------------
# Sandwich estimators

# Positive-weight rows, weights, and the (non-aliased) design of an lm/glm.
.rt_sw_parts <- function(fit) {
  b <- stats::coef(fit)
  ok <- !is.na(b)
  X <- stats::model.matrix(fit)
  X <- X[, names(b)[ok], drop = FALSE]
  w <- if (inherits(fit, "glm")) fit$prior.weights else fit$weights
  if (is.null(w)) w <- rep(1, nrow(X))
  list(b = b, ok = ok, X = X, w = w, pos = w > 0)
}

# Expand a matrix over the non-aliased coefficients to every coefficient
# (NA rows and columns for aliased ones, as vcov() does).
.rt_sw_full <- function(Vn, b, ok) {
  V <- matrix(NA_real_, length(b), length(b), dimnames = list(names(b), names(b)))
  V[ok, ok] <- Vn
  V
}

# Score contributions and the (unscaled) information matrix of an lm, glm
# or glm.nb fit, the pieces of Stata's robust variance. For glm.nb both are
# over (beta, ln alpha) jointly, as nbreg's.
.rt_sw_pieces <- function(fit, p) {
  X <- p$X
  if (inherits(fit, "glm")) {
    eta <- drop(X %*% p$b[p$ok])
    if (!is.null(fit$offset)) eta <- eta + fit$offset
    fam <- fit$family
    mu <- fam$linkinv(eta)
    d1 <- fam$mu.eta(eta)
    y <- .rt_glm_y(fit)
    pw <- p$w
    if (.rt_nb_glm(fit)) {
      # glm, family(nbinomial ml) vce(robust): alpha fixed, beta's sandwich.
      th <- fit$theta
      Ab <- .rt_vcov_negbin(fit, X, eta, y, pw, fixed = TRUE)
      if (is.null(Ab)) cli::cli_abort("The information matrix is singular; no robust variance.", call = NULL)
      l_mu <- y / mu - (y + th) / (th + mu)
      U <- X * (pw * l_mu * d1)
      return(list(U = U[p$pos, , drop = FALSE], Ainv = Ab))
    }
    if (inherits(fit, "negbin")) {
      th <- fit$theta
      Vj <- .rt_vcov_negbin(fit, X, eta, y, pw, joint = TRUE)
      if (is.null(Vj)) cli::cli_abort("The information matrix is singular; no robust variance.", call = NULL)
      l_mu <- y / mu - (y + th) / (th + mu)
      l_th <- digamma(y + th) - digamma(th) + log(th / (th + mu)) + (mu - y) / (th + mu)
      U <- cbind(X * (pw * l_mu * d1), lnalpha = pw * l_th * (-th))
      return(list(U = U[p$pos, , drop = FALSE], Ainv = Vj))
    }
    vmu <- fam$variance(mu)
    if (.rt_canonical(fam$family, fam$link)) {
      wi <- pw * d1^2 / vmu
    } else {
      d2 <- .rt_link_d2(fam$link, eta)
      vd <- .rt_var_d1(fam$family, mu)
      if (is.null(d2) || is.null(vd)) {
        cli::cli_abort("A robust variance is not available for the {.val {fam$family}}/{.val {fam$link}} family and link.",
                       call = NULL)
      }
      # Observed information: Stata's vce(oim) bread (the expected
      # information moves probit SEs by ~1e-3).
      wi <- pw * (d1^2 / vmu - (y - mu) * (d2 / vmu - d1^2 * vd / vmu^2))
    }
    U <- X * (pw * (y - mu) * d1 / vmu)
    A <- crossprod(X, X * wi)
  } else {
    # fit$residuals, never padded by na.exclude (review P1-1): for a
    # weighted lm the raw residuals y - Xb.
    e <- fit$residuals
    U <- X * (p$w * e)
    A <- crossprod(X, X * p$w)
  }
  Ainv <- tryCatch(solve(A), error = function(e) NULL)
  if (is.null(Ainv)) cli::cli_abort("The information matrix is singular; no robust variance.", call = NULL)
  list(U = U[p$pos, , drop = FALSE], Ainv = Ainv)
}

#' Stata's robust or cluster-robust variance of an lm, glm or glm.nb fit
#'
#' `vce = "robust"`: glm ML fits HC0 x N/(N - 1) (`logit`, `probit`,
#' `poisson`, `glm`, `nbreg` with `[pweight]` or `vce(robust)`), lm HC1 =
#' HC0 x N/(N - k) (`regress`). `vce = "cluster"`: scores summed within
#' clusters; glm x G/(G - 1), lm x G/(G - 1) (N - 1)/(N - k). N and G count
#' observations with a positive weight. The bread is the inverse observed
#' information (the expected information differs for non-canonical links).
#' `joint = TRUE` returns glm.nb's whole (beta, ln alpha) matrix.
#' @keywords internal
#' @noRd
.rt_vcov_sandwich <- function(fit, vce, cluster = NULL, joint = FALSE) {
  fit <- .rt_require_frame(fit, "robust variance")
  p <- .rt_sw_parts(fit)
  N <- sum(p$pos)
  k <- sum(p$ok)
  is_lm <- !inherits(fit, "glm")
  if (is_lm && N <= k) {
    cli::cli_abort(c(
      "A robust variance needs positive residual degrees of freedom; this model has {N} positive-weight observations and rank {k}.",
      "i" = "Use a model with fewer parameters or more observed positive-weight cases."
    ), class = "tabtools_error_vce_sample_size", call = NULL)
  }
  if (!is_lm && !identical(vce, "cluster") && N <= 1L) {
    cli::cli_abort("A robust variance needs at least two positive-weight observations.",
                   class = "tabtools_error_vce_sample_size", call = NULL)
  }
  pc <- .rt_sw_pieces(fit, p)
  U <- pc$U
  if (identical(vce, "cluster")) {
    cl <- .rt_resolve_cluster(fit, cluster, nrow(p$X))[p$pos]
    U <- rowsum(U, as.character(cl), reorder = FALSE)
    G <- nrow(U)
    if (G < 2L) cli::cli_abort("{.arg cluster} has a single cluster; a cluster-robust variance needs at least two.", call = NULL)
    f <- G / (G - 1) * if (is_lm) (N - 1) / (N - k) else 1
  } else {
    f <- if (is_lm) N / (N - k) else N / (N - 1)
  }
  Vn <- pc$Ainv %*% crossprod(U) %*% pc$Ainv * f
  Vn <- (Vn + t(Vn)) / 2
  if (inherits(fit, "negbin") && !.rt_nb_glm(fit)) {
    nm <- c(names(p$b)[p$ok], "lnalpha")
    dimnames(Vn) <- list(nm, nm)
    if (joint) return(Vn)
    Vn <- Vn[seq_len(k), seq_len(k), drop = FALSE]
  }
  .rt_sw_full(Vn, p$b, p$ok)
}

# Clusters counted as the sandwich counts them (positive weights only).
.rt_sw_groups <- function(fit, cluster) {
  fit <- .rt_require_frame(fit, "cluster variable")
  p <- .rt_sw_parts(fit)
  .rt_n_clusters(.rt_resolve_cluster(fit, cluster, nrow(p$X))[p$pos])
}

# ---------------------------------------------------------------------------
# Weights

# The weights a user gave an lm, glm or glm.nb fit (NULL when unweighted;
# the trials of a grouped binomial response are not weights, review P2-4).
.rt_user_weights <- function(fit) {
  mf <- if (is.data.frame(fit$model)) fit$model else NULL
  if (!is.null(mf)) {
    w <- stats::model.weights(mf)
    if (is.null(w)) return(NULL)
    return(as.vector(w))
  }
  if (is.null(tryCatch(stats::getCall(fit)$weights, error = function(e) NULL))) return(NULL)
  if (inherits(fit, "glm")) {
    # The trials test is .rt_binom_trials()'s (review R01).
    if (fit$family$family %in% c("binomial", "quasibinomial") && .rt_binom_trials(fit)) return(NULL)
    return(fit$prior.weights)
  }
  fit$weights
}

# Positive user weights of an lm/glm/glm.nb fit (NULL when unweighted).
.rt_fit_weights <- function(fit) {
  if (!class(fit)[1] %in% c("lm", "glm", "negbin")) return(NULL)
  w <- .rt_user_weights(fit)
  if (is.null(w)) return(NULL)
  w[w > 0]
}

# Case weights of a (non-Fine-Gray) coxph fit, NULL when unweighted: the
# fit's own copy (coxph refuses weights <= 0).
.rt_cox_weights <- function(fit) {
  if (!identical(class(fit)[1], "coxph") || .mi_is_finegray(fit)) return(NULL)
  fit$weights
}

# Weights of any supported fit, as its estimator used them (NULL when
# unweighted): for the IPTW warning (review P0-2).
.rt_case_weights <- function(fit) {
  if (isS4(fit)) {
    w <- tryCatch(stats::weights(fit), error = function(e) NULL)
    return(if (is.numeric(w)) as.vector(w) else NULL)
  }
  if (is.data.frame(fit)) return(NULL)
  cls <- class(fit)[1]
  w <- switch(cls,
    lm = , glm = , negbin = .rt_user_weights(fit),
    coxph = .rt_cox_weights(fit),
    geeglm = if (is.null(fit$call$weights)) NULL else fit$prior.weights,
    polr = , clm = if (is.data.frame(fit$model)) stats::model.weights(fit$model) else NULL,
    survreg = , tobit = fit$weights,
    tryCatch(stats::weights(fit), error = function(e) NULL)
  )
  if (!is.numeric(w)) return(NULL)
  as.vector(w)
}

# Non-integer, non-constant weights: they look like inverse-probability
# weights rather than analytic or frequency weights (EW1). One non-integer
# weight is enough (review W32: IPTW with some weights exactly 1).
.rt_iptw_like_w <- function(w) {
  if (is.null(w)) return(FALSE)
  w <- w[is.finite(w) & w > 0]
  length(unique(w)) > 1L && any(abs(w - round(w)) > 1e-8)
}

.rt_iptw_like <- function(fit) .rt_iptw_like_w(.rt_fit_weights(fit))

# regtab()'s warning when a weighted model's variance is not robust (review
# P0-2): Stata's [pweight] always reports robust standard errors.
.rt_warn_weights <- function(fit, vce, i, ci_method = "wald") {
  if (is.data.frame(fit)) return(invisible(FALSE))
  # vce = "model" with profile intervals (review R07): confint() of the fit
  # reads such weights as frequency weights; its Wald intervals, the fit's
  # own vcov(), stay silent as documented.
  if (identical(vce, "model") && identical(ci_method, "profile") &&
      class(fit)[1] %in% c("lm", "glm", "negbin") && .rt_iptw_like(fit)) {
    cli::cli_warn(c(
      "Model {i} has non-integer, non-constant weights, which look like inverse-probability weights.",
      "i" = "{.code vce = \"model\"} profiles the fit's own likelihood, which reads them as frequency weights.",
      "i" = "Stata's {.code [pweight]} reports robust standard errors: use {.code vce = \"robust\"} with {.code ci_method = \"wald\"}."
    ))
    return(invisible(TRUE))
  }
  if (!identical(vce, "stata")) return(invisible(FALSE))
  cls <- class(fit)[1]
  if (cls %in% c("lm", "glm", "negbin")) {
    if (!.rt_iptw_like(fit)) return(invisible(FALSE))
    cli::cli_warn(c(
      "Model {i} has non-integer, non-constant weights, which look like inverse-probability weights.",
      "i" = "{.code vce = \"stata\"} treats them as analytic weights (Stata {.code [aweight]}), with model-based standard errors.",
      "i" = "Stata's {.code [pweight]} reports robust standard errors: use {.code vce = \"robust\"} (or {.code \"cluster\"} with {.arg cluster})."
    ))
    return(invisible(TRUE))
  }
  if (cls == "coxph") {
    w <- .rt_cox_weights(fit)
    if (is.null(w) || length(unique(w)) < 2L || !is.null(fit$naive.var)) return(invisible(FALSE))
    cli::cli_warn(c(
      "Model {i} ({.cls coxph}) has weights but model-based standard errors: {.fn survival::coxph} uses a robust variance only for non-integer weights, {.code robust = TRUE} or {.code cluster()}, so integer (for instance rounded) weights and {.code robust = FALSE} fits are treated as frequency weights.",
      "i" = "Stata's {.code stset [pweight]} reports robust standard errors: use {.code vce = \"robust\"} (or {.code \"cluster\"} with {.arg cluster}); ignore this if the weights are frequency weights."
    ))
    return(invisible(TRUE))
  }
  if (cls %in% c("geeglm", "svyglm", "glm_weightit", "crr")) return(invisible(FALSE))
  # A robust survreg (robust = TRUE or cluster()) already reports Stata's
  # [pweight] variance, which the hint below asks for (C4 review O1).
  if (cls %in% c("survreg", "tobit") && !is.null(fit$naive.var)) return(invisible(FALSE))
  w <- .rt_case_weights(fit)
  if (!.rt_iptw_like_w(w)) return(invisible(FALSE))
  hint <- if (cls %in% c("survreg", "tobit")) {
    "Refit with {.code survreg(robust = TRUE)} (or {.code cluster(id)}): regtab then reports Stata's robust variance, times G/(G - 1)."
  } else {
    "regtab has no robust variance for {.cls {cls}} fits ({.code vce = \"robust\"} is refused); for IPTW use a model with one ({.fn glm}, {.fn MASS::glm.nb}, {.fn survival::coxph}, {.fn geepack::geeglm}), or pass the estimates with robust standard errors as a data frame."
  }
  cli::cli_warn(c(
    "Model {i} ({.cls {cls}}) has non-integer, non-constant weights, which look like inverse-probability weights.",
    "i" = "Its standard errors treat them as frequency weights (Stata {.code [fweight]}, which Stata refuses for non-integer weights), not probability weights: Stata's {.code [pweight]} reports robust standard errors.",
    "i" = hint
  ))
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Per-model vce and cluster

#' Expand regtab()'s vce and cluster arguments to one entry per model
#'
#' `vce`: one value for every model, a character vector or list with one
#' value per model; when `vce` is not given and `cluster` is, each model
#' with a cluster gets `"cluster"` and the others `"stata"`. `cluster`: one
#' specification for every model (used only by the models whose vce is
#' `"cluster"`), or a list with one entry per model (`NULL` for none).
#' @return list(vce = list, cluster = list), each of length M.
#' @keywords internal
#' @noRd
.rt_expand_vce <- function(vce, cluster, M, vce_missing = FALSE) {
  choices <- c("stata", "model", "robust", "cluster")
  cl_list <- is.list(cluster) && !is.data.frame(cluster)
  if (cl_list) {
    if (length(cluster) != M) {
      cli::cli_abort("{.arg cluster} is a list of {length(cluster)} entr{?y/ies} for {M} model{?s}; give one per model (NULL for none), or a single specification.",
                     call = NULL)
    }
    cls <- cluster
  } else {
    cls <- rep(list(cluster), M)
  }
  if (vce_missing) {
    v <- lapply(cls, function(c) if (is.null(c)) "stata" else "cluster")
  } else {
    if (is.list(vce) && !is.data.frame(vce)) {
      v <- vce
    } else if (is.function(vce) || ((is.matrix(vce) || inherits(vce, "Matrix")) && M == 1L)) {
      # A user-supplied variance (task 5.18): a function applies to every
      # model, a matrix only to a single one.
      v <- rep(list(vce), M)
    } else if (is.matrix(vce) || inherits(vce, "Matrix")) {
      cli::cli_abort("A user-supplied {.arg vce} matrix belongs to one model: with {M} models, give {.arg vce} as a list with one entry per model.",
                     call = NULL)
    } else if (is.character(vce) && length(vce) == 1L) {
      v <- rep(list(vce), M)
    } else if (is.character(vce) && length(vce) == M) {
      v <- as.list(vce)
    } else {
      cli::cli_abort("{.arg vce} must be one value or one value per model ({M}).", call = NULL)
    }
    if (length(v) != M) {
      cli::cli_abort("{.arg vce} is a list of {length(v)} entr{?y/ies} for {M} model{?s}; give one per model.", call = NULL)
    }
    for (m in seq_len(M)) {
      x <- v[[m]]
      if (is.function(x) || is.matrix(x) || inherits(x, "Matrix")) next
      if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
        cli::cli_abort(c("{.arg vce} for model {m} must be one of {.val {choices}}, or a user-supplied variance (a function or a matrix).",
                         "x" = "Got {.val {format(x)}}."), call = NULL)
      }
    }
  }
  # A single cluster applies to the models whose vce is "cluster"; given
  # when no model has that vce it would be ignored, so it is refused
  # (external review F35: vce = "robust", cluster = ~id used to be the
  # plain robust variance, silently).
  if (!cl_list && !is.null(cluster) && !any(vapply(v, identical, TRUE, "cluster"))) {
    cli::cli_abort(c(
      "{.arg cluster} is given, but no model has {.code vce = \"cluster\"}: it would be ignored.",
      "i" = "Use {.code vce = \"cluster\"} (or leave {.arg vce} unset: {.arg cluster} alone implies it), or drop {.arg cluster}."
    ), call = NULL)
  }
  for (m in seq_len(M)) {
    if (!identical(v[[m]], "cluster") && !is.null(cls[[m]])) {
      if (cl_list) {
        what <- if (is.character(v[[m]])) paste0("{.val ", v[[m]], "}") else "a user-supplied variance"
        cli::cli_abort(paste0("{.arg cluster} is given for model {m}, whose {.arg vce} is ", what,
                              "; clusters apply only with {.code vce = \"cluster\"}."), call = NULL)
      }
      cls[m] <- list(NULL)
    }
  }
  list(vce = v, cluster = cls)
}

# ---------------------------------------------------------------------------
# User-supplied variance (task 5.18, R only)

# The classes whose every displayed row is a coefficient of coef(): glm.nb,
# survreg, ordinal, multi-equation and mixed fits add ancillary, cutpoint
# or variance-component rows whose standard errors a matrix over coef()
# does not cover.
.rt_user_vce_classes <- c("lm", "glm", "coxph", "clogit")

# vce_df, one entry per model: NULL, "z", "t" or a positive number, only
# for the models whose vce is a function or a matrix.
.rt_expand_vce_df <- function(vce_df, vces) {
  M <- length(vces)
  user <- !vapply(vces, is.character, TRUE)
  if (is.null(vce_df)) return(rep(list(NULL), M))
  d <- if (is.list(vce_df)) vce_df else if (length(vce_df) == 1L) rep(list(vce_df), M) else as.list(vce_df)
  # A vector may mark the models without a user variance NA (stack review).
  if (!is.list(vce_df)) d[vapply(d, function(v) length(v) == 1L && is.na(v), TRUE)] <- list(NULL)
  if (length(d) != M) {
    cli::cli_abort("{.arg vce_df} must be one value or one per model ({M}).", call = NULL)
  }
  for (m in seq_len(M)) {
    x <- d[[m]]
    if (is.null(x)) next
    ok <- (is.character(x) && length(x) == 1L && x %in% c("z", "t")) ||
      (is.numeric(x) && length(x) == 1L && !is.na(x) && x > 0)
    if (!ok) cli::cli_abort("{.arg vce_df} for model {m} must be {.val z}, {.val t} or a positive number.", call = NULL)
    if (!user[m] && !(is.list(vce_df) || length(vce_df) > 1L)) d[m] <- list(NULL)
    if (!user[m] && !is.null(d[[m]])) {
      cli::cli_abort(c("{.arg vce_df} is given for model {m}, whose {.arg vce} is {.val {vces[[m]]}}.",
                       "i" = "{.arg vce_df} applies only to a user-supplied variance (a function or a matrix)."), call = NULL)
    }
  }
  if (!any(user)) {
    cli::cli_abort(c("{.arg vce_df} is given, but no model has a user-supplied {.arg vce}.",
                     "i" = "The reference distribution of Stata's variances is the Stata command's."), call = NULL)
  }
  d
}

# Check a user-supplied variance against the fit and attach it, with its
# degrees of freedom, as the fit's "tt_user_vce" attribute; the model's vce
# becomes "user", which tt_vcov() and tt_wald_df() read. .rt_expand_vce()
# has already refused a cluster for the model.
.rt_user_vce <- function(fit, x, df, i) {
  where <- paste0("model ", i)
  if (inherits(fit, "tt_mi") || is.data.frame(fit)) {
    what <- if (is.data.frame(fit)) "a data frame (which carries its own standard errors)" else "a multiply imputed model (pooled by Rubin's rules from each imputation's variance)"
    cli::cli_abort("A user-supplied {.arg vce} is not available for {what} ({where}).", call = NULL)
  }
  if (!class(fit)[1] %in% .rt_user_vce_classes) {
    ok <- .rt_user_vce_classes
    cli::cli_abort(c("A user-supplied {.arg vce} is not available for {.cls {class(fit)[1]}} models ({where}).",
                     "i" = "It is available for {.cls {ok}} fits, whose rows are all coefficients."), call = NULL)
  }
  V <- if (is.function(x)) {
    tryCatch(x(fit), error = function(e) {
      cli::cli_abort("The {.arg vce} function failed on {where}.", parent = e, call = NULL)
    })
  } else x
  b <- stats::coef(fit)
  V <- .rt_user_vce_matrix(V, b, where)
  df <- if (is.null(df)) {
    if (identical(class(fit)[1], "lm")) fit$df.residual else Inf
  } else if (identical(df, "z")) {
    Inf
  } else if (identical(df, "t")) {
    r <- tryCatch(stats::df.residual(fit), error = function(e) NULL)
    if (is.null(r) || !is.finite(r) || r <= 0) {
      cli::cli_abort("{.code vce_df = \"t\"}: {where} has no residual degrees of freedom; give a number.", call = NULL)
    }
    r
  } else as.numeric(df)
  attr(fit, "tt_user_vce") <- list(V = V, df = df)
  fit
}

# The matrix over every coefficient (NA rows and columns for aliased ones):
# numeric, square, finite, symmetric, positive semidefinite; named by the
# coefficients (any order) or unnamed in their order, either over all of
# them or over the estimated ones only.
.rt_user_vce_matrix <- function(V, b, where) {
  bad <- function(msg) cli::cli_abort(c("The user-supplied {.arg vce} of {where} is not a valid covariance matrix.", "x" = msg), call = NULL)
  if (inherits(V, "Matrix")) V <- as.matrix(V)
  if (!is.matrix(V) || !is.numeric(V)) bad("It is not a numeric matrix.")
  nm <- names(b)
  est <- nm[!is.na(b)]
  if (nrow(V) != ncol(V)) bad("It is not square.")
  rn <- rownames(V)
  cn <- colnames(V)
  if (!is.null(rn) || !is.null(cn)) {
    if (!identical(rn, cn)) bad("Its row and column names differ.")
    if (anyNA(rn) || any(!nzchar(rn)) || anyDuplicated(rn)) {
      bad("Its coefficient names must be non-missing, non-empty and unique.")
    }
    miss <- setdiff(est, rn)
    if (length(miss)) bad(paste0("It has no row for ", paste(miss, collapse = ", "), "."))
    extra <- setdiff(rn, nm)
    if (length(extra)) bad(paste0("It has rows that are not coefficients: ", paste(extra, collapse = ", "), "."))
    V <- V[intersect(nm, rn), intersect(nm, rn), drop = FALSE]
  } else if (nrow(V) == length(nm)) {
    dimnames(V) <- list(nm, nm)
  } else if (nrow(V) == length(est)) {
    dimnames(V) <- list(est, est)
  } else {
    bad(paste0("It is ", nrow(V), " x ", ncol(V), " for ", length(nm), " coefficients, and has no names."))
  }
  Ve <- V[est, est, drop = FALSE]
  if (any(!is.finite(Ve))) bad("It has missing or infinite entries for estimated coefficients.")
  if (any(diag(Ve) < 0)) bad("It has a negative variance.")
  sc <- max(abs(Ve), 1e-300)
  if (max(abs(Ve - t(Ve))) > 1e-8 * sc) bad("It is not symmetric.")
  ev <- eigen((Ve + t(Ve)) / 2, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < -1e-8 * max(abs(ev), 1e-300)) bad("It is not positive semidefinite (a negative eigenvalue).")
  out <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
  out[est, est] <- Ve
  out
}

# Resolve each model's cluster specification to a vector once, so that the
# several tt_vcov() calls per model do not re-evaluate it.
.rt_prepare_cluster <- function(fit, cluster) {
  if (is.null(cluster) || inherits(fit, "geeglm") || is.data.frame(fit)) return(cluster)
  n <- if (inherits(fit, "coxph")) fit$n else
    tryCatch(nrow(stats::model.frame(.rt_require_frame(fit, "cluster variable"))), error = function(e) NULL)
  if (is.null(n)) return(cluster)
  cl <- .rt_resolve_cluster(fit, cluster, n)
  if (.rt_n_clusters(cl) < 2L) {
    cli::cli_abort("{.arg cluster} has a single cluster; a cluster-robust variance needs at least two.", call = NULL)
  }
  cl
}

# ---------------------------------------------------------------------------
# Statistics under probability weights (review P0-4)

# Whether a model's variance treats its weights as probability weights:
# user weights with a robust or cluster vce (lm, glm, glm.nb, coxph), a
# weighted coxph whose own variance is already robust (survival's default
# for non-integer weights, Stata's stcox after stset [pw]), and a geeglm
# shown as Stata's glm [pw], vce(cluster id).
.rt_pw_reading <- function(fit, vce) {
  if (inherits(fit, "geeglm")) return(identical(.rt_gee_as(fit), "glm"))
  w <- .rt_fit_weights(fit)
  if (!is.null(w)) return(vce %in% c("robust", "cluster"))
  cw <- .rt_cox_weights(fit)
  if (is.null(cw)) return(FALSE)
  vce %in% c("robust", "cluster") || (identical(vce, "stata") && !is.null(fit$naive.var))
}

# Stata's pseudo-log-likelihood of a glm-type fit under [pweight] (glm,
# glm.nb's parent class, geeglm's glm reading), by family; NULL for a
# family whose Stata [pw] log-likelihood has not been verified (binomial
# with trials). Probes: logit/probit/cloglog (X01, X02, X04), poisson
# (X03), Gamma (X06: scale 1), Gaussian (X06b: raw weights, scale
# sum(w e^2)/N), inverse Gaussian (scale 1; L14, L15 in
# fixtures/regtab_glm_ll). With `normalise = TRUE` the weights are rescaled
# to mean 1 first: Stata's glm [aweight] and unweighted log-likelihood
# (.rt_glm_ll(); L01-L12).
.rt_glm_pseudo_ll <- function(fit, normalise = FALSE) {
  fam <- fit$family$family
  y <- if (inherits(fit, "geeglm")) fit$y else .rt_glm_y(fit)
  # fit$fitted.values: never padded by na.exclude.
  mu <- as.vector(fit$fitted.values)
  w <- fit$prior.weights
  if (is.null(w)) w <- rep(1, length(y))
  pos <- w > 0
  y <- as.vector(y)[pos]
  mu <- mu[pos]
  w <- w[pos]
  if (normalise) w <- w / mean(w)
  switch(fam,
    binomial = , quasibinomial = {
      # Trials (review R01): a cbind() response, or proportions.
      if (!all(y %in% c(0, 1)) || (!inherits(fit, "geeglm") && .rt_binom_trials(fit))) return(NULL)
      sum(w * ifelse(y == 1, log(mu), log1p(-mu)))
    },
    poisson = , quasipoisson = sum(w * (ifelse(y > 0, y * log(mu), 0) - mu - lgamma(y + 1))),
    gaussian = {
      phi <- sum(w * (y - mu)^2) / length(y)
      -0.5 * sum(w * ((y - mu)^2 / phi + log(2 * pi * phi)))
    },
    Gamma = -sum(w * (y / mu + log(mu))),
    inverse.gaussian = -0.5 * sum(w * ((y - mu)^2 / (y * mu^2) + log(2 * pi * y^3))),
    {
      # MASS::negative.binomial(theta), Stata glm, family(nbinomial k), under
      # [aweight] only (B08; its [pweight] log-likelihood is unverified).
      th <- .rt_nbk_theta(fit)
      if (is.null(th) || !normalise) return(NULL)
      sum(w * (lgamma(y + th) - lgamma(th) - lgamma(y + 1) + th * log(th / (th + mu)) +
                 ifelse(y > 0, y * log(mu / (th + mu)), 0)))
    }
  )
}

# Stata's stcox log partial likelihood after stset [pweight]: the Breslow
# partial likelihood with the weights normalised to mean 1 (probe X21:
# -3000.907). With c = n / sum(w), l(c w) = c l(w) - c ln(c) sum over
# failures of w, and l(w) is survival's weighted Breslow loglik. NULL for
# Efron ties, which stcox refuses with weights (no Stata analogue), unless
# no failure times are tied: then Efron's likelihood is Breslow's.
.rt_cox_pseudo_ll <- function(fit) {
  if (!identical(fit$method, "breslow") && !(identical(fit$method, "efron") && isFALSE(.rt_cox_tied(fit)))) {
    return(NULL)
  }
  # coxph(y = FALSE): the response of the fit-time frame regtab() anchored
  # (review T2B-05), or of the re-sorted data (.rt_cox_yw()).
  yw <- .rt_cox_yw(fit)
  w <- yw$w
  y <- yw$y
  if (is.null(w) || is.null(y) || NROW(y) != length(w)) return(NULL)
  ev <- y[, ncol(y)] == 1
  cc <- length(w) / sum(w)
  cc * fit$loglik[length(fit$loglik)] - cc * log(cc) * sum(w[ev])
}

# ll, AIC, BIC and pseudo R-squared of a model read under probability
# weights: Stata's pseudo-log-likelihood, or blank with a note where it is
# not reproduced (`why`: "family" unverified, "efron" no Stata analogue).
.rt_pw_stats <- function(fit, s, vce) {
  blank <- function(why) {
    s$ll <- NA_real_
    s$aic <- NA_real_
    s$bic <- NA_real_
    s$r2_p <- NA_real_
    s$pw_note <- why
    s
  }
  ic <- function(s) {
    s$aic <- s$bic <- NA_real_
    if (!is.na(s$ll) && !is.na(s$rank)) {
      s$aic <- -2 * s$ll + 2 * s$rank
      if (!is.na(s$N)) s$bic <- -2 * s$ll + s$rank * log(s$N)
    }
    s
  }
  if (inherits(fit, "coxph")) {
    ll <- .rt_cox_pseudo_ll(fit)
    if (is.null(ll)) {
      efron_tied <- identical(fit$method, "efron") && !isFALSE(.rt_cox_tied(fit))
      return(blank(if (efron_tied) "efron" else if (.rt_has_tt(fit)) "tt" else "response"))
    }
    s$ll <- ll
    return(ic(s))
  }
  # regress [pw] e(ll) is the [aweight] log-likelihood regtab already
  # reports (.rt_lm_ll()); nbreg's is MASS's weighted log-likelihood.
  if (identical(class(fit)[1], "lm") || inherits(fit, "negbin")) return(s)
  if (inherits(fit, "glm") || inherits(fit, "geeglm")) {
    ll <- .rt_glm_pseudo_ll(fit)
    if (is.null(ll) || !is.finite(ll)) return(blank("family"))
    s$ll <- ll
    fam <- fit$family$family
    # logit/probit [pw] store e(r2_p) against the weighted constant-only
    # model (-null deviance / 2 for 0/1 outcomes); poisson [pw] and Stata's
    # glm command (geeglm's glm reading) store none.
    s$r2_p <- if (fam %in% c("binomial", "quasibinomial") && fit$family$link %in% c("logit", "probit") &&
                  !inherits(fit, "geeglm") && !.rt_has_offset(fit) &&
                  is.finite(fit$null.deviance) && fit$null.deviance > 0) {
      1 - ll / (-fit$null.deviance / 2)
    } else NA_real_
    return(ic(s))
  }
  s
}

# ---------------------------------------------------------------------------
# Variance footnote (vce_note = TRUE)

# Name of the fit's own cluster()/id grouping variable (coxph, survreg).
.rt_own_cluster_name <- function(fit) {
  cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  for (a in c("cluster", "id")) {
    if (!is.null(cl[[a]])) return(paste(deparse(cl[[a]]), collapse = ""))
  }
  tl <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) NULL)
  hit <- grep("^cluster\\(", tl, value = TRUE)
  if (length(hit)) return(sub("^cluster\\((.*)\\)$", "\\1", hit[1]))
  NULL
}

# One phrase per model whose variance is not the Stata command's
# model-based default (review P2-6: also the fits that are robust without
# vce = "robust": weighted or clustered coxph, robust survreg, crr, a
# weighted geeglm).
.rt_vce_phrase <- function(fit, info, vce, cluster) {
  if (inherits(fit, "glm_weightit")) {
    type <- fit$vcov_type %||% "asympt"
    return(switch(type,
      asympt = "M-estimation (accounts for the estimated weights)",
      const = NULL,
      HC0 = "robust (not accounting for the estimated weights)",
      BS = "bootstrap",
      FWB = "fractional weighted bootstrap",
      paste0("WeightIt ", type)))
  }
  if (inherits(fit, "svyglm")) return("survey design (linearized)")
  if (inherits(fit, "crr")) return("robust")
  by_name <- function(nm) if (is.null(nm)) "robust" else paste0("robust, clustered by ", nm)
  if (inherits(fit, "geeglm")) {
    if (identical(vce, "model")) return(NULL)
    robust <- vce %in% c("robust", "cluster") || identical(.rt_gee_as(fit), "glm") || .rt_gee_weighted(fit)
    if (!robust) return(NULL)
    return(by_name(if (!is.null(fit$call$id)) paste(deparse(fit$call$id), collapse = "") else "the GEE id"))
  }
  if (vce %in% c("stata", "model") && (inherits(fit, "coxph") || inherits(fit, "survreg")) &&
      !is.null(fit$naive.var)) {
    if (identical(vce, "model")) return(NULL)
    return(by_name(.rt_own_cluster_name(fit)))
  }
  switch(vce,
    user = "user-supplied",
    robust = if (inherits(fit, "coxph")) by_name(.rt_own_cluster_name(fit)) else "robust",
    cluster = {
      nm <- if (inherits(cluster, "formula")) all.vars(cluster)[1] else if (is.character(cluster) && length(cluster) == 1L) cluster else NULL
      if (is.null(nm) && is.null(cluster) && inherits(fit, "coxph")) nm <- .rt_own_cluster_name(fit)
      if (is.null(nm)) "robust, clustered" else paste0("robust, clustered by ", nm)
    },
    NULL
  )
}

# The text of the R-only `vce` stats row (task 5.18): the footnote's phrase
# (.rt_vce_phrase()) where it names one, else the variance the fit
# actually uses. The phrase is NULL for the command's model-based default,
# and also under vce = "model" for a fit whose own vcov() is a sandwich or
# a jackknife (a robust coxph/survreg, a geeglm), which are named here.
# Blank for a data frame (its standard errors are the user's), and for a
# regtab_uv() model whose fits differ.
.rt_vce_label <- function(fit, info, vce, cluster) {
  if (is.data.frame(fit)) return("")
  if (inherits(fit, "tt_uv")) {
    labs <- unique(vapply(fit$fits, function(f) .rt_vce_label(f, tt_model_info(f), vce, cluster), ""))
    return(if (length(labs) == 1L) labs else "")
  }
  fit <- .rt_mi_first(fit)
  ph <- .rt_vce_phrase(fit, info, vce, cluster)
  if (!is.null(ph)) return(ph)
  by_name <- function(nm) if (is.null(nm)) "robust" else paste0("robust, clustered by ", nm)
  if (identical(vce, "model")) {
    if ((inherits(fit, "coxph") || inherits(fit, "survreg")) && !is.null(fit$naive.var)) {
      return(by_name(.rt_own_cluster_name(fit)))
    }
    if (inherits(fit, "geeglm")) {
      id <- if (!is.null(fit$call$id)) paste(deparse(fit$call$id), collapse = "") else "the GEE id"
      return(switch(fit$std.err %||% "san.se", san.se = by_name(id), jack = "jackknife",
                    j1s = "one-step jackknife", fij = "fully iterated jackknife", "model-based"))
    }
  }
  "model-based"
}

# `user_only`: the sentence regtab adds without vce_note, naming only the
# models with a user-supplied variance (task 5.18).
.rt_vce_footnote <- function(fits, infos, vces, clusters, labels, user_only = FALSE) {
  ph <- vapply(seq_along(fits), function(m) {
    if (user_only && !identical(vces[[m]], "user")) return(NA_character_)
    .rt_vce_phrase(fits[[m]], infos[[m]], vces[[m]], clusters[[m]]) %||% NA_character_
  }, "")
  if (all(is.na(ph))) return("")
  if (length(fits) == 1L) return(paste0("Standard errors: ", ph[1], "."))
  # One "<model>: <variance>" clause per model, the model named by its label
  # or its number (label and phrase used to run together: "IPTW, robust
  # robust").
  lab <- if (length(labels) == length(fits)) trimws(labels) else rep("", length(fits))
  lab[is.na(lab) | !nzchar(lab)] <- paste("Model", seq_along(fits))[is.na(lab) | !nzchar(lab)]
  keep <- !is.na(ph)
  paste0("Standard errors, ", paste0(lab[keep], ": ", ph[keep], collapse = "; "), ".")
}
