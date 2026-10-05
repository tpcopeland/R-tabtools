# regtab adapter for zero-inflated and hurdle count models (plan task 5.3):
# pscl::zeroinfl (Stata zip/zinb) and pscl::hurdle (shown in Stata's
# churdle layout, the closest two-part command).
#
# Stata facts (goldens R18, R25s; probes 2026-09-25,
# qa/stata/make_regtab_phase5a.do):
# - Coef. on the native (log-count / logit) scale, never exponentiated;
#   `nointercept` is automatic (`regtab.ado:557-560`) and drops the
#   equation intercepts and the ancillary rows by label.
# - Equations (`regtab.ado:1552-1567`): the count equation is keyed by the
#   dependent variable (Stata's coleq, so a Poisson/NB model of the same
#   outcome shares its rows; Phase 5 review P0-3) and labelled with
#   the dependent variable's label ("Event count"; its name when
#   unlabelled, underscores shown as spaces), zip/zinb's `inflate` equation
#   "Inflation equation", churdle's selection equation "Selection
#   equation". zinb's `/lnalpha` shows as "Ancillary: lnalpha" and its
#   diparm as "Ancillary: alpha" (estimate and CI exp-transformed, no
#   p-value). pscl's theta is 1/alpha, so lnalpha = -log(theta).
# - Row order: collect's global colname order within each equation (a
#   covariate of both equations comes first in the inflation equation:
#   "Inflation equation: Female" before "Structural-zero risk" in R18),
#   intercept last in each equation.
# - Standard errors: vce(oim). pscl's vcov() inverts optim()'s
#   finite-difference Hessian at estimates optim() stops ~1e-4 from the
#   MLE (reltol 1e-8); regtab uses the observed information from the
#   analytic score (below), differentiated numerically, which matches
#   Stata's zip/zinb at Stata's estimates to ~1e-9.
# - Stats: e(rank) counts every coefficient and lnalpha; zip/zinb report no
#   pseudo R2.
# - With zip and zinb in one table and `keepintercept`, zinb's ancillary
#   rows are shown as for a zinb alone (Stata 2.1.11; 2.1.9 dropped them,
#   probe P6; golden R53).

#' @export
tt_regtab_adapter.zeroinfl <- function(fit, ...) TRUE

#' @export
tt_regtab_adapter.hurdle <- function(fit, ...) TRUE

# pscl stores the success of both optim() components in `converged`.
# Check that flag before using even a finite Hessian from the last iterate.
.rt_check_twopart_convergence <- function(fit, i = 1L) {
  if (identical(fit[["converged"]], FALSE)) {
    cli::cli_abort(c(
      "Model {i} ({.cls {class(fit)[1]}}) did not converge; its estimates and inference cannot be reported reliably.",
      "i" = "Refit the model and check {.code fit$converged}; inspect the iteration limit and the count and zero equations."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  invisible(TRUE)
}

# log P(Y = 0) and its derivatives for the count distributions, with
# mu = exp(eta) and th = theta (geometric: th = 1).
.rt_cnt <- function(y, mu, th, dist) {
  if (dist == "poisson") {
    return(list(logf0 = -mu, a = -mu, c = 0, s_eta = y - mu, s_lt = 0,
                logf = stats::dpois(y, mu, log = TRUE)))
  }
  r <- th / (th + mu)
  list(logf0 = th * log(r), a = -th * mu / (th + mu),
       c = th * (log(r) + mu / (th + mu)),
       s_eta = th * (y - mu) / (th + mu),
       s_lt = th * (digamma(y + th) - digamma(th) + log(r) + (mu - y) / (th + mu)),
       logf = stats::dnbinom(y, size = th, mu = mu, log = TRUE))
}

.rt_off <- function(o, n) if (is.null(o)) rep(0, n) else rep_len(o, n)

# pscl's model = FALSE leaves no model frame (the rows, labels and the
# response need it); y = FALSE is fine, the response is read from the
# frame (Milestone H task H4: identical output, or a refusal naming the
# option).
#' @export
tt_regtab_check.zeroinfl <- function(fit, i) .rt_twopart_check(fit, i)

#' @export
tt_regtab_check.hurdle <- function(fit, i) .rt_twopart_check(fit, i)

.rt_twopart_check <- function(fit, i = NULL) {
  .rt_check_twopart_convergence(fit, i %||% 1L)
  if (!is.data.frame(fit$model)) {
    where <- if (is.null(i)) "" else paste0(" (model ", i, ")")
    cli::cli_abort(c(
      "{.fn regtab} needs the model frame of this {.cls {class(fit)[1]}} fit{where}, which was fitted with {.code model = FALSE}.",
      "i" = "Refit with {.code model = TRUE} (the default)."
    ), call = NULL)
  }
  invisible(TRUE)
}

.rt_twopart_design <- function(fit) {
  X <- stats::model.matrix(fit, "count")
  Z <- stats::model.matrix(fit, "zero")
  n <- nrow(X)
  w <- fit$weights %||% rep(1, n)
  y <- fit$y %||% (if (is.data.frame(fit$model)) stats::model.response(fit$model))
  list(X = X, Z = Z, y = y, w = w, oc = .rt_off(fit$offset$count, n),
       oz = .rt_off(fit$offset$zero, n))
}

# Score of a zero-inflated model in (count coefs, zero coefs, log theta):
# y = 0: l = log(pi + (1 - pi) f0); y > 0: l = log(1 - pi) + log f(y).
.rt_zi_score <- function(par, d, dist, linkobj) {
  kc <- ncol(d$X)
  kz <- ncol(d$Z)
  bc <- par[seq_len(kc)]
  bz <- par[kc + seq_len(kz)]
  th <- if (dist == "negbin") exp(par[kc + kz + 1L]) else 1
  mu <- exp(drop(d$X %*% bc) + d$oc)
  ez <- drop(d$Z %*% bz) + d$oz
  pz <- linkobj$linkinv(ez)
  dpz <- linkobj$mu.eta(ez)
  cn <- .rt_cnt(d$y, mu, th, dist)
  f0 <- exp(cn$logf0)
  zero <- d$y == 0
  D <- pz + (1 - pz) * f0
  s_z <- ifelse(zero, dpz * (1 - f0) / D, -dpz / (1 - pz))
  s_c <- ifelse(zero, (1 - pz) * f0 * cn$a / D, cn$s_eta)
  out <- c(crossprod(d$X, d$w * s_c), crossprod(d$Z, d$w * s_z))
  if (dist == "negbin") out <- c(out, sum(d$w * ifelse(zero, (1 - pz) * f0 * cn$c / D, cn$s_lt)))
  out
}

# Score of a hurdle model in (count coefs, zero coefs, log theta count,
# log theta zero): zero hurdle P(y > 0) = F(eta_z) (binomial) or
# 1 - f0(mu_z) (count distribution); count part log f(y) - log(1 - f0).
.rt_hurdle_score <- function(par, d, dist, zdist, linkobj) {
  kc <- ncol(d$X)
  kz <- ncol(d$Z)
  bc <- par[seq_len(kc)]
  bz <- par[kc + seq_len(kz)]
  i <- kc + kz
  thc <- if (dist == "negbin") exp(par[i <- i + 1L]) else 1
  thz <- if (zdist == "negbin") exp(par[i <- i + 1L]) else 1
  pos <- d$y > 0
  mu <- exp(drop(d$X %*% bc) + d$oc)
  cn <- .rt_cnt(d$y, mu, thc, dist)
  f0 <- exp(cn$logf0)
  s_c <- ifelse(pos, cn$s_eta + f0 * cn$a / (1 - f0), 0)
  s_lc <- ifelse(pos, cn$s_lt + f0 * cn$c / (1 - f0), 0)
  ez <- drop(d$Z %*% bz) + d$oz
  if (zdist == "binomial") {
    P <- linkobj$linkinv(ez)
    dP <- linkobj$mu.eta(ez)
    s_z <- ifelse(pos, dP / P, -dP / (1 - P))
    s_lz <- 0
  } else {
    muz <- exp(ez)
    zn <- .rt_cnt(rep(0, length(ez)), muz, thz, zdist)
    g0 <- exp(zn$logf0)
    # y = 0: l = log g0; y > 0: l = log(1 - g0).
    s_z <- ifelse(pos, -g0 * zn$a / (1 - g0), zn$a)
    s_lz <- ifelse(pos, -g0 * zn$c / (1 - g0), zn$c)
  }
  out <- c(crossprod(d$X, d$w * s_c), crossprod(d$Z, d$w * s_z))
  if (dist == "negbin") out <- c(out, sum(d$w * s_lc))
  if (zdist == "negbin") out <- c(out, sum(d$w * s_lz))
  out
}

.rt_linkobj <- function(link) tryCatch(stats::make.link(link), error = function(e) NULL)

# Full parameter vector (with log theta) and its names.
.rt_twopart_par <- function(fit) {
  cf <- fit$coefficients
  par <- c(stats::setNames(cf$count, paste0("count_", names(cf$count))),
           stats::setNames(cf$zero, paste0("zero_", names(cf$zero))))
  th <- fit$theta
  if (inherits(fit, "zeroinfl")) {
    if (identical(fit$dist, "negbin")) par <- c(par, "Log(theta)" = log(unname(th)))
  } else {
    if (identical(fit$dist$count, "negbin")) par <- c(par, "Log(theta)" = log(unname(th[["count"]])))
    if (identical(fit$dist$zero, "negbin")) par <- c(par, "Log(theta_zero)" = log(unname(th[["zero"]])))
  }
  par
}

# Observed information of the full parameter vector at `par` (default: the
# fit's estimates); NULL when it cannot be formed.
.rt_twopart_vcov_full <- function(fit, par = NULL) {
  p0 <- .rt_twopart_par(fit)
  if (is.null(par)) par <- p0
  par <- stats::setNames(as.numeric(par), names(p0))
  if (anyNA(par)) return(NULL)
  # A hurdle whose zero part is a count distribution has no binary link
  # (fit$link is NULL; the score does not use one). Its information used to
  # fail here and fall back to pscl's vcov() silently (external review
  # F28).
  count_zero <- inherits(fit, "hurdle") && !identical(fit$dist$zero, "binomial")
  lk <- .rt_linkobj(if (count_zero) "logit" else fit$link)
  d <- tryCatch(.rt_twopart_design(fit), error = function(e) NULL)
  if (is.null(d) || is.null(lk)) return(NULL)
  score <- if (inherits(fit, "zeroinfl")) {
    if (!fit$dist %in% c("poisson", "negbin", "geometric")) return(NULL)
    function(p) .rt_zi_score(p, d, fit$dist, lk)
  } else {
    if (!fit$dist$count %in% c("poisson", "negbin", "geometric") ||
        !fit$dist$zero %in% c("binomial", "poisson", "negbin", "geometric")) return(NULL)
    function(p) .rt_hurdle_score(p, d, fit$dist$count, fit$dist$zero, lk)
  }
  H <- .rt_jacobian(score, par)
  H <- (H + t(H)) / 2
  V <- .rt_inv_neg(H)
  if (is.null(V) || anyNA(V)) return(NULL)
  dimnames(V) <- list(names(par), names(par))
  V
}

# The fit's own vcov() (vce = "model"). pscl leaves Log(theta) out of it;
# with `full` it is added from the fit's SE.logtheta (no covariances), so
# the Ancillary rows keep an interval (review P2-2).
.rt_twopart_model_vcov <- function(fit, full = FALSE) {
  V <- stats::vcov(fit)
  if (!full) return(V)
  add <- function(V, nm, se) {
    if (is.null(se) || !length(se) || !is.finite(se)) return(V)
    k <- nrow(V)
    V2 <- matrix(0, k + 1L, k + 1L, dimnames = list(c(rownames(V), nm), c(colnames(V), nm)))
    V2[seq_len(k), seq_len(k)] <- V
    V2[k + 1L, k + 1L] <- se^2
    V2
  }
  se <- fit$SE.logtheta
  if (inherits(fit, "zeroinfl")) {
    if (identical(fit$dist, "negbin")) V <- add(V, "Log(theta)", unname(se[1]))
  } else {
    if (identical(fit$dist$count, "negbin")) V <- add(V, "Log(theta)", unname(se[["count"]]))
    if (identical(fit$dist$zero, "negbin")) V <- add(V, "Log(theta_zero)", unname(se[["zero"]]))
  }
  V
}

# Stata's vce(oim) (observed information from the analytic score) for the
# coefficients, or with `full` for every parameter including log theta;
# the fit's own variance for vce = "model" or when the information cannot
# be formed.
.rt_twopart_vcov <- function(fit, vce = "stata", full = FALSE) {
  .rt_check_twopart_convergence(fit)
  if (identical(vce, "model")) return(.rt_twopart_model_vcov(fit, full))
  Vf <- .rt_twopart_vcov_full(fit)
  if (is.null(Vf)) {
    # Never pscl's numerical-Hessian vcov() in its place (Milestone H
    # decision H-D6; external review F28).
    .rt_twopart_check(fit)
    cli::cli_abort(c("Stata's observed-information variance cannot be computed for this {.cls {class(fit)[1]}} fit (unsupported distribution or link, or singular information).",
                     "i" = "Use {.code vce = \"model\"} for the fit's own {.fn vcov}."), call = NULL)
  }
  if (full) return(Vf)
  V <- stats::vcov(fit)
  Vf[rownames(V), colnames(V)]
}

#' @export
tt_vcov.zeroinfl <- function(fit, vce = "stata", cluster = NULL, full = FALSE, ...) .rt_twopart_vcov(fit, vce, full)

#' @export
tt_vcov.hurdle <- function(fit, vce = "stata", cluster = NULL, full = FALSE, ...) .rt_twopart_vcov(fit, vce, full)

# Model frame with the labels a `subset =` fit lost (review P0-7).
.rt_twopart_frame <- function(fit) .rt_restore_attrs(fit$model, fit)

# The count equation is named after the dependent variable (Stata's coleq
# is the depvar), so a Poisson or NB model of the same outcome shares its
# rows in a multi-equation table (review P0-3); its label is the
# dependent variable's label.
.rt_depvar_name <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  dv <- if (!is.null(f) && length(f) == 3L) all.vars(f[[2L]])[1] else NA_character_
  if (is.na(dv)) "count" else dv
}

.rt_depvar_label <- function(fit, mf = .rt_twopart_frame(fit)) {
  dv <- .rt_depvar_name(fit)
  if (!is.null(mf) && !is.null(mf[[dv]])) return(var_label(mf[[dv]], dv))
  dv
}

.rt_twopart_rows <- function(fit, info, level, vce, zero_label, interactions = "fvgen", xsymbol = "\u00d7",
                             vsref = NULL) {
  d <- .rt_twopart_design(fit)
  V <- tt_vcov(fit, vce, full = TRUE)
  b <- .rt_twopart_par(fit)
  mf <- .rt_twopart_frame(fit)
  tc <- fit$terms$count
  tz <- fit$terms$zero
  eqs <- list(
    list(name = .rt_depvar_name(fit), prefix = "count", label = .rt_eq_label(.rt_depvar_label(fit, mf)),
         X = d$X, terms = tc, contrasts = fit$contrasts$count),
    # A hurdle's zero part models P(y > 0) and a zero-inflation part the
    # structural zeros: different equations, so different keys, or a
    # zeroinfl and a hurdle side by side would share "zero::x" rows under
    # one label (muse P2-21).
    list(name = if (inherits(fit, "hurdle")) "selection" else "zero", prefix = "zero", label = zero_label,
         X = d$Z, terms = tz, contrasts = fit$contrasts$zero)
  )
  eqs <- lapply(eqs, function(e) {
    cols <- colnames(e$X)
    nm <- paste0(e$prefix, "_", cols)
    w <- .rt_wald_named(b, V, nm, Inf, level)
    w$col <- cols
    tt <- stats::delete.response(e$terms)
    list(name = e$name, label = e$label, wald = w, assign = .rt_assign_labels(e$X, tt),
         bases = .rt_bases(e$contrasts, mf), X = e$X, order = .rt_formula_order(stats::formula(tt)))
  })
  res <- .rt_multieq_rows(eqs, mf, interactions = interactions, xsymbol = xsymbol, vsref = vsref)
  # Ancillary equation: lnalpha = -log(theta) and alpha = 1/theta (diparm).
  anc <- list()
  add_alpha <- function(tn, suffix) {
    if (!tn %in% names(b) || !tn %in% rownames(V)) return()
    wl <- .rt_wald_named(b, V, tn, Inf, level)
    wl$estimate <- -wl$estimate
    lo <- -wl$conf.high
    wl$conf.high <- -wl$conf.low
    wl$conf.low <- lo
    wl$statistic <- -wl$statistic
    anc[[length(anc) + 1L]] <<- .rt_anc_row(.rt_eq_key("/", paste0("lnalpha", suffix)),
                                            paste0("Ancillary: lnalpha", suffix), wl, block = "/")
    anc[[length(anc) + 1L]] <<- .rt_anc_row(.rt_eq_key("/", paste0("alpha", suffix)),
                                            paste0("Ancillary: alpha", suffix), wl, block = "/",
                                            transform = "exp")
  }
  add_alpha("Log(theta)", "")
  add_alpha("Log(theta_zero)", " (zero)")
  if (length(anc)) res <- .rt_add_ancillary(res, do.call(rbind, anc))
  res
}

#' @export
tt_regtab_rows.zeroinfl <- function(fit, info, level = 0.95, vce = "stata", interactions = "fvgen",
                                    xsymbol = "\u00d7", vsref = NULL, ...) {
  .rt_twopart_rows(fit, info, level, vce, "Inflation equation", interactions, xsymbol, vsref)
}

#' @export
tt_regtab_rows.hurdle <- function(fit, info, level = 0.95, vce = "stata", interactions = "fvgen",
                                  xsymbol = "\u00d7", vsref = NULL, ...) {
  .rt_twopart_rows(fit, info, level, vce, "Selection equation", interactions, xsymbol, vsref)
}

.rt_twopart_stats <- function(fit, info, ...) {
  s <- tt_model_stats.default(fit, NULL)
  ll <- stats::logLik(fit)
  # Case (frequency) weights: Stata's zip [fweight] e(N) is their sum
  # (probe B12: 3,000; review P2-1).
  s$N <- if (!is.null(fit$weights)) sum(fit$weights) else fit$n
  s$ll <- as.numeric(ll)
  s$rank <- attr(ll, "df")
  s$aic <- -2 * s$ll + 2 * s$rank
  s$bic <- -2 * s$ll + s$rank * log(s$N)
  s
}

#' @export
tt_model_stats.zeroinfl <- function(fit, info, ...) .rt_twopart_stats(fit, info)

#' @export
tt_model_stats.hurdle <- function(fit, info, ...) .rt_twopart_stats(fit, info)
