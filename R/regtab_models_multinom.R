# regtab adapter for multinomial logit (plan task 5.2): nnet::multinom,
# Stata's mlogit.
#
# Stata facts (goldens R17; probes 2026-09-25, qa/stata/make_regtab_phase5a.do):
# - One equation per non-base outcome, labelled with the outcome's value
#   label (`regtab.ado:1353-1379`, `:1552-1557`; underscores become
#   spaces); the base-outcome equation is dropped (`:1570-1605`). R's base
#   outcome is the first factor level (relevel() to change it; Stata's
#   baseoutcome()).
# - RRR: every coefficient, the equation intercepts included, is
#   exponentiated; `nointercept` is automatic (mlogit is ratio-scale) and
#   drops the `<Outcome>: Intercept` rows (Stata by label; R by their
#   intercept role, Milestone H task H5).
# - Milestone H task H2: offsets (H-D10; mlogit refuses offset()), a
#   count-matrix response's row totals (already in fit$weights, F23), data
#   changed after fitting (H-D7, F24) and weight decay (refused, F30); see
#   .rt_mn_parts().
# - Rows: see .rt_multieq_rows(); a factor reads `Secondary: Smoking status`
#   above `Secondary:   Former` (tabtools 2.1.12; 2.1.11 showed the raw
#   `Secondary: 1.smoking`), and factor levels come first in each equation
#   only when the base outcome is the first equation of e(b), i.e. has the
#   lowest value code (its o.-marked continuous terms then open e(b)); see
#   .rt_mn_factor_first() (Phase 5 review P0-4, probes D10, G22). Weights enter the information and N
#   (review P0-2).
# - Standard errors: vce(oim), the analytic information of the multinomial
#   logit, sum_i w_i (diag(p_i) - p_i p_i') (x) x_i x_i', at the fit's
#   estimates. nnet's own Hessian agrees to ~1e-7, but multinom() stops at
#   reltol 1e-8, leaving the estimates ~1e-4 (relative) from the MLE: refit
#   with `reltol = 1e-12, maxit = 1000` when a last digit differs.
# - Stats: e(rank) = coefficients over the non-base equations; e(r2_p) =
#   1 - ll/ll_0 with the constant-only (marginal) log-likelihood.
# - With two mlogit models whose covariates differ, the covariate only the
#   second model has is shown, blank for the first (Stata 2.1.11; 2.1.9
#   omitted it, probe P5; golden R54).

#' @export
tt_regtab_adapter.multinom <- function(fit, ...) TRUE

# nnet's convergence code is 1 when its iteration limit was reached,
# otherwise 0. The covariance can still be finite at a failed iterate.
.rt_check_multinom_convergence <- function(fit, i = 1L) {
  code <- fit[["convergence"]]
  if (!is.null(code) && any(code != 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Model {i} ({.cls multinom}) did not converge; its estimates and inference cannot be reported reliably.",
      "i" = "Refit the model and check {.code fit$convergence == 0}; inspect the iteration limit, predictor scaling and sparse outcome categories."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  invisible(TRUE)
}

# Offset of each non-base outcome relative to the base outcome (n x K, K
# the non-base outcomes), from the model frame's offset() term, or NULL.
# nnet takes an n x (K + 1) matrix over every outcome (the linear predictor
# of outcome k is x'b_k + off_k, the base's b 0), and for a binary outcome
# a vector added to the one non-base linear predictor (external review F03;
# Milestone H decision H-D10: offsets are supported, although Stata's
# mlogit refuses offset()).
.rt_mn_offset <- function(mf, n, K) {
  off <- if (!is.null(mf)) stats::model.offset(mf) else NULL
  if (is.null(off)) return(NULL)
  off <- unclass(off)
  if (!is.matrix(off)) off <- matrix(as.numeric(off), ncol = 1L)
  if (nrow(off) != n) return(NA)
  if (ncol(off) == K + 1L) return(off[, -1L, drop = FALSE] - off[, 1L])
  if (ncol(off) == 1L && K == 1L) return(off)
  NA
}

# Response as outcome shares, n x (K + 1): a factor's indicators, or a
# count matrix's rows divided by their totals (nnet fits the shares with
# the row totals folded into fit$weights).
.rt_mn_shares <- function(Y, lev) {
  if (is.matrix(Y)) {
    tot <- rowSums(Y)
    return(Y / ifelse(tot > 0, tot, 1))
  }
  y <- factor(Y, levels = lev)
  out <- matrix(0, length(y), length(lev))
  out[cbind(seq_along(y), as.integer(y))] <- 1
  out
}

# Non-base outcome probabilities at coefficient matrix B.
.rt_mn_prob <- function(X, B, off = NULL) {
  eta <- X %*% t(B)
  if (!is.null(off)) eta <- eta + off
  m <- pmax(apply(eta, 1L, max), 0)
  ee <- exp(eta - m)
  ee / (exp(-m) + rowSums(ee))
}

# Coefficient matrix (non-base outcomes x design columns).
.rt_mn_coef <- function(fit) {
  cf <- stats::coef(fit)
  if (is.matrix(cf)) return(cf)
  matrix(cf, 1L, dimnames = list(fit$lev[2L], names(cf)))
}

# Element-wise agreement of a recomputed quantity with the stored one.
.rt_close <- function(a, b, tol = 1e-8) {
  a <- as.vector(unclass(a))
  b <- as.vector(unclass(b))
  length(a) == length(b) && all(is.finite(a) == is.finite(b)) &&
    all(abs(a - b)[is.finite(b)] <= tol * pmax(1, abs(b[is.finite(b)])))
}

# Design, response, offset and weights of a multinom fit, from its stored
# model frame (model = TRUE) or the data as they are now, which are used
# only when they reproduce the fit (Milestone H decision H-D7; external
# review F24: nnet's default model = FALSE made every regtab rebuild X and
# y from whatever the data held): the same number of rows, the fitted
# probabilities recomputed from the design and offset at the estimates, and
# the response (fitted values plus residuals). Otherwise an error. fit$weights
# holds the prior weights, times the row totals for a count-matrix response
# (external review F23), one per row, zeros included.
.rt_mn_parts <- function(fit, check = TRUE) {
  B <- .rt_mn_coef(fit)
  msg <- NULL
  mf <- tryCatch(.rt_mn_frame(fit), error = function(e) {
    msg <<- conditionMessage(e)
    NULL
  })
  # Re-sorted data with their row names kept: back in the fit's order.
  rn <- rownames(fit$fitted.values)
  if (!is.null(rn) && !anyDuplicated(rn)) mf <- .rt_align_frame(mf, rn)
  X <- if (is.null(mf)) NULL else tryCatch(
    stats::model.matrix(stats::terms(fit), mf, contrasts.arg = fit$contrasts),
    error = function(e) {
      msg <<- conditionMessage(e)
      NULL
    })
  w <- as.numeric(fit$weights)
  Y <- if (!is.null(mf)) stats::model.response(mf) else NULL
  off <- if (!is.null(X)) .rt_mn_offset(mf, nrow(X), nrow(B)) else NULL
  why <- .rt_mn_mismatch(fit, B, X, Y, off, w, msg)
  if (check && !is.null(why)) {
    if (is.data.frame(fit$model)) {
      # The stored frame does not reproduce the fit: not a data change.
      cli::cli_abort(c(
        "The {.cls multinom} model's stored model frame does not reproduce the fit: {why}.",
        "i" = "regtab supports {.fn nnet::multinom} fits as Stata's {.code mlogit} estimates them (no {.arg summ}, {.arg censored} or {.arg mask} options)."
      ), call = NULL)
    }
    cli::cli_abort(c(
      "The {.cls multinom} model's data cannot be matched to the fit: {why}.",
      "i" = "{.fn nnet::multinom} keeps no copy of its data by default ({.code model = FALSE}), so regtab reads the data again and uses them only when they reproduce the fit (the same rows, response and fitted probabilities); the data have changed since the model was fitted, or cannot be found.",
      "i" = "Refit with {.code model = TRUE}, or on the current data."
    ), call = NULL)
  }
  # A `subset =` frame loses the columns' labels (Phase 5 review P0-7).
  if (is.null(why)) mf <- .rt_restore_attrs(mf, fit)
  # Design columns without a positive-weight observation (a factor level a
  # `subset =` left empty): nnet holds their coefficients at 0 and the
  # information is singular there, so they are left out of it and their
  # rows omitted, as Stata omits an empty level (review T2B-08).
  empty <- if (!is.null(X)) colSums(abs(X[w > 0, , drop = FALSE])) == 0 else logical()
  list(B = B, mf = mf, X = X, Y = Y, off = if (identical(off, NA)) NULL else off, w = w,
       lev = fit$lev %||% c("", rownames(B)), empty = empty)
}

# Model frame of a multinom fit: the stored one (model = TRUE), or the
# call re-evaluated on the data as they are now with the fit's factor
# levels (`xlev`), so that a factor relevelled after fitting keeps the
# fit's coding (review T2B-10; nnet's own model.frame() drops them).
.rt_mn_frame <- function(fit) {
  if (is.data.frame(fit$model)) return(fit$model)
  cl <- fit$call
  keep <- intersect(names(cl), c("formula", "data", "weights", "subset", "na.action"))
  mc <- cl[c(1L, match(keep, names(cl)))]
  mc[[1L]] <- quote(stats::model.frame)
  mc$formula <- fit$terms
  mc$xlev <- fit$xlevels
  env <- environment(fit$terms) %||% parent.frame()
  eval(mc, env)
}

# Why a rebuilt frame does not reproduce a multinom fit (NULL when it does).
.rt_mn_mismatch <- function(fit, B, X, Y, off, w, msg = NULL) {
  if (is.null(X) || is.null(Y)) return(msg %||% "the model frame cannot be rebuilt")
  n <- nrow(X)
  fv <- fit$fitted.values
  if (is.null(fv) || NROW(fv) != n || length(w) != n) {
    return(sprintf("the data give %d observation%s, the fit has %d", n, if (n == 1L) "" else "s", NROW(fv)))
  }
  if (identical(off, NA)) return("its offset has the wrong number of columns")
  if (ncol(X) != ncol(B) || !identical(colnames(X), colnames(B))) return("the design columns differ from the coefficients")
  P <- .rt_mn_prob(X, B, off)
  Pf <- if (ncol(as.matrix(fv)) == ncol(P) + 1L) as.matrix(fv)[, -1L, drop = FALSE] else as.matrix(fv)
  if (!identical(dim(Pf), dim(P)) || !.rt_close(P, Pf)) return("the fitted probabilities differ")
  S <- .rt_mn_shares(Y, fit$lev %||% colnames(fv))
  r <- as.matrix(fit$residuals)
  Sf <- as.matrix(fv) + r
  S <- if (ncol(Sf) == ncol(S)) S else S[, -1L, drop = FALSE]
  pos <- w > 0
  if (!identical(dim(S), dim(Sf)) || !.rt_close(S[pos, , drop = FALSE], Sf[pos, , drop = FALSE])) {
    return("the response differs")
  }
  NULL
}

# Information (Hessian of the log-likelihood) of a multinomial logit at
# coefficient matrix B (non-base outcomes x design columns): -sum_i w_i
# (diag(p_i) - p_i p_i') (x) x_i x_i', p_i with the offset. `w` is
# fit$weights, which already includes a count-matrix response's row totals.
.rt_mn_hessian <- function(X, B, w, off = NULL) {
  P <- .rt_mn_prob(X, B, off)
  K <- nrow(B)
  p <- ncol(X)
  H <- matrix(0, K * p, K * p)
  for (e in seq_len(K)) {
    for (f in e:K) {
      a <- if (e == f) P[, e] * (1 - P[, e]) else -P[, e] * P[, f]
      blk <- -crossprod(X, X * (w * a))
      ie <- (e - 1L) * p + seq_len(p)
      jf <- (f - 1L) * p + seq_len(p)
      H[ie, jf] <- blk
      H[jf, ie] <- t(blk)
    }
  }
  H
}

# The fit's own vcov(), named "<outcome>:<column>" like the information
# below (a binary multinom's vcov() lacks the outcome prefix; review P2-2).
.rt_mn_model_vcov <- function(fit, B) {
  V <- stats::vcov(fit)
  nm <- as.vector(t(outer(rownames(B), colnames(B), paste, sep = ":")))
  if (nrow(V) == length(nm) && !all(nm %in% rownames(V))) dimnames(V) <- list(nm, nm)
  V
}

# Inverse observed information of the multinomial logit at the fit's
# estimates, or at `par` (coefficients in e(b) order by outcome; the tests
# evaluate it at Stata's estimates). An error when it cannot be formed
# (Milestone H decision H-D6: never a silent substitute).
.rt_mn_vcov <- function(fit, par = NULL) {
  pt <- .rt_mn_parts(fit)
  B <- pt$B
  if (!is.null(par)) B[] <- matrix(par, nrow(B), ncol(B), byrow = TRUE)
  ok <- !pt$empty
  Vo <- .rt_inv_neg(.rt_mn_hessian(pt$X[, ok, drop = FALSE], B[, ok, drop = FALSE], pt$w, pt$off))
  if (is.null(Vo) || anyNA(Vo)) {
    cli::cli_abort(c("Stata's observed-information variance cannot be computed for this {.cls multinom} fit (singular information: collinear covariates).",
                     "i" = "Use {.code vce = \"model\"} for the fit's own {.fn vcov}."), call = NULL)
  }
  nm <- as.vector(t(outer(rownames(B), colnames(B), paste, sep = ":")))
  keep <- as.vector(t(outer(rownames(B), ok, function(r, k) k)))
  Vn <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
  Vn[keep, keep] <- Vo
  Vn
}

#' @export
tt_vcov.multinom <- function(fit, vce = "stata", cluster = NULL, ...) {
  .rt_check_multinom_convergence(fit)
  if (identical(vce, "model")) return(.rt_mn_model_vcov(fit, .rt_mn_coef(fit)))
  .rt_mn_vcov(fit)
}

# Penalised fits have no Stata equivalent (mlogit has no penalty): their
# estimates are not the maximum-likelihood estimates the observed
# information describes (external review F30).
#' @export
tt_regtab_check.multinom <- function(fit, i) {
  .rt_check_multinom_convergence(fit, i)
  # summ = 1, 2, 3 fits the unique rows (the fit no longer has one row per
  # observation), censored = TRUE a response of possible outcomes: neither
  # is Stata's mlogit (review T2B-08).
  summ <- fit$call$summ
  if (!is.null(summ) && !identical(tryCatch(as.numeric(eval(summ, environment(fit$terms))), error = function(e) NA), 0)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nnet::multinom} fits with {.code summ = {deparse(summ)}} (model {i}).",
      "i" = "They are fitted to the data's unique rows; refit with {.code summ = 0} (the default)."
    ), call = NULL)
  }
  if (isTRUE(fit$censored)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nnet::multinom} fits with {.code censored = TRUE} (model {i}).",
      "i" = "A response of possible outcomes has no Stata {.code mlogit} equivalent."
    ), call = NULL)
  }
  decay <- fit$decay %||% 0
  if (is.numeric(decay) && length(decay) == 1L && !is.na(decay) && decay > 0) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nnet::multinom} fits with weight decay ({.code decay = {decay}}; model {i}).",
      "i" = "A penalised fit's estimates are not Stata's {.code mlogit} maximum-likelihood estimates; refit with {.code decay = 0} (the default)."
    ), call = NULL)
  }
  invisible(TRUE)
}

# Log-likelihood of the constant-only model on the same sample, with the
# fit's offset and weights: Stata's e(ll_0) for mlogit. Without an offset
# it is the marginal log-likelihood sum_k n_k log(n_k / n); with one
# (H-D10) the intercepts are fitted by Newton-Raphson.
.rt_mn_ll0 <- function(pt) {
  S <- .rt_mn_shares(pt$Y, pt$lev)
  if (is.matrix(pt$Y) && ncol(S) != nrow(pt$B) + 1L) return(NA_real_)
  w <- pt$w
  ll <- function(a) {
    P <- .rt_mn_prob(matrix(1, nrow(S), 1L), matrix(a, ncol = 1L), pt$off)
    P <- cbind(1 - rowSums(P), P)
    sum(w * rowSums(ifelse(S > 0, S * log(P), 0)))
  }
  if (is.null(pt$off)) {
    nk <- colSums(S * w)
    nk <- nk[nk > 0]
    return(sum(nk * log(nk / sum(nk))))
  }
  K <- nrow(pt$B)
  one <- matrix(1, nrow(S), 1L)
  a <- rep(0, K)
  for (it in seq_len(100L)) {
    P <- .rt_mn_prob(one, matrix(a, ncol = 1L), pt$off)
    g <- colSums(w * (S[, -1L, drop = FALSE] - P))
    H <- .rt_mn_hessian(one, matrix(a, ncol = 1L), w, pt$off)
    step <- tryCatch(solve(-H, g), error = function(e) NULL)
    if (is.null(step) || anyNA(step)) return(NA_real_)
    # Step halving keeps the (concave) log-likelihood from falling.
    l0 <- ll(a)
    while (ll(a + step) < l0 - 1e-10 * abs(l0) && max(abs(step)) > 1e-12) step <- step / 2
    a <- a + step
    # Not converged: blank, never a null that is not the maximum.
    if (max(abs(step)) < 1e-10) return(ll(a))
  }
  NA_real_
}

# Whether factor levels come first (Phase 5 review P0-4, probes D10, G22):
# Stata's e(b) lists the outcome equations in value order, and only when
# the base outcome's equation is the first one do its o.-marked
# continuous terms push the factor levels ahead. R's base outcome is the
# first level; its Stata value code comes from the response's "labels"
# attribute (tt_as_factor()) or integer level texts. Without codes
# (relevel() drops the attribute) the base counts as the lowest value.
.rt_mn_factor_first <- function(fit, mf) {
  y <- if (!is.null(mf)) stats::model.response(mf) else NULL
  if (is.null(y) || is.matrix(y)) return(TRUE)
  codes <- .rt_codes(y)
  if (isTRUE(attr(codes, "positional"))) return(TRUE)
  num <- suppressWarnings(as.numeric(codes))
  base <- fit$lev[1] %||% .rt_levels(y)[1]
  if (anyNA(num) || !base %in% names(codes)) return(TRUE)
  num[names(codes) == base] == min(num)
}

#' @export
tt_regtab_rows.multinom <- function(fit, info, level = 0.95, vce = "stata", interactions = "fvgen",
                                    xsymbol = "\u00d7", vsref = NULL, ...) {
  pt <- .rt_mn_parts(fit)
  B <- pt$B
  # Empty design columns (see .rt_mn_parts()): omitted rows.
  B[, pt$empty] <- NA_real_
  V <- tt_vcov(fit, vce)
  assign <- .rt_assign_labels(pt$X, stats::terms(fit))
  bases <- .rt_bases(attr(pt$X, "contrasts"), pt$mf)
  ord <- .rt_formula_order(stats::formula(fit))
  eqs <- lapply(seq_len(nrow(B)), function(k) {
    eq <- rownames(B)[k]
    b <- stats::setNames(B[k, ], paste0(eq, ":", colnames(B)))
    w <- .rt_wald_named(b, V, names(b), Inf, level)
    w$col <- colnames(B)
    list(name = eq, label = .rt_eq_label(eq), wald = w, assign = assign, bases = bases, X = pt$X,
         order = ord)
  })
  res <- .rt_multieq_rows(eqs, pt$mf, factor_first = .rt_mn_factor_first(fit, pt$mf),
                          interactions = interactions, xsymbol = xsymbol, vsref = vsref)
  if (isTRUE(info$exponentiate)) {
    e <- !res$ancillary
    for (col in c("estimate", "conf.low", "conf.high")) res[[col]][e] <- exp(res[[col]][e])
  }
  res
}

#' @export
tt_model_stats.multinom <- function(fit, info, ...) {
  pt <- .rt_mn_parts(fit)
  s <- tt_model_stats.default(fit, NULL)
  # Case (frequency) weights: Stata's mlogit [fweight] reports their sum as
  # e(N) (probe B11: 17,944; Phase 5 review P0-2, P2-1); for a count-matrix
  # response fit$weights holds the row totals, so N is the number of units,
  # as for the one-row-per-unit fit.
  s$N <- sum(pt$w)
  s$rank <- sum(!is.na(pt$B))
  # McFadden against the constant-only model with the same offset (H-D10;
  # external review F27: the null ignored the offset).
  ll0 <- .rt_mn_ll0(pt)
  if (is.finite(ll0)) s$r2_p <- 1 - s$ll / ll0
  s$aic <- -2 * s$ll + 2 * s$rank
  s$bic <- -2 * s$ll + s$rank * log(s$N)
  s
}
