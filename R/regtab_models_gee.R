# GEE for regtab: geepack::geeglm (Stata `xtgee`).
#
# Stata facts, from the goldens (R23, R25i) and probes of Stata 17 +
# tabtools 2.1.9:
# - tabtools 2.1.12 classifies xtgee by glm's family/link rule: OR for
#   binomial/logit and IRR for poisson/log (exponentiated, the intercept
#   dropped automatically), "Coef." with the intercept otherwise
#   (tt_model_info.geeglm; 2.1.11 and earlier left every xtgee "Coef.").
# - xtgee's default variance is model-based (e(vce) "conventional"), with
#   the scale fixed at 1 for the binomial and Poisson families and
#   estimated as the Pearson chi-squared over N for the continuous ones
#   (`xtgee` default `scale()`). geepack's naive variance is that
#   variance times its own scale estimate, which for the Gaussian family is
#   the same N-divisor estimate as Stata's (union: 5.693430647670 in
#   both), so the Stata variance is vbeta.naiv / gamma for binomial and
#   Poisson fits and vbeta.naiv otherwise (both agree with Stata's e(V) to
#   ~1e-9). geepack's exchangeable correlation estimate is Stata's
#   (0.46231265 on union), and with a tight tolerance its estimates equal
#   Stata's to 1e-12.
# - geepack's robust (sandwich) variance, `vce = "model"` for the default
#   std.err = "san.se", is Stata's `vce(robust)` without its G/(G-1)
#   small-sample factor (union: 0.035956 vs 0.035964 for the constant,
#   ratio 4434/4433). With std.err = "jack", "j1s" or "fij", vcov() and so
#   `vce = "model"` are geepack's jackknife variances instead; the robust,
#   cluster and weighted variances always take the stored sandwich
#   `geese$vbeta` (Codex audit R2).
# - Statistics (`regtab.ado:813-858`): no log-likelihood, AIC or BIC (a
#   quasi-likelihood); QICu = deviance + 2 rank (Pan 2001) only when the
#   scale is fixed at 1, else a note; with `stats(aic)` the AIC row is
#   relabelled QICu when only GEE models carry a value; Groups = number of
#   clusters.
# - Milestone 5w (task 5.13; probes V50, V53, V55, V56): geepack's sandwich
#   times G/(G - 1) is xtgee's `vce(robust)` (independence and exchangeable
#   to 1e-7, the exchangeable gap being xtgee's convergence), and, for an
#   independence working correlation with weights, Stata's
#   `glm y x [pw=w], family() link() vce(cluster id)` (the GEE estimates
#   equal R's tightly converged glm() to 1e-13). regtab(gee_as = "glm")
#   shows that reading: classified as glm (OR for binomial-logit, IRR for
#   Poisson-log, `regtab.ado:633-655`), vce "stata" the cluster sandwich,
#   no QICu or Groups (glm has no e(N_g)), and ll/AIC/BIC from glm's
#   pseudo-log-likelihood (review P0-4).
# - 5w review P0-2: a geeglm fitted with weights is xtgee [pweight], which
#   forces vce(robust) (e(vce) "robust" without the option; probe D13), so
#   its default (vce = "stata") variance is the sandwich times G/(G - 1).

# geese.fit retains the C++ gee_est diagnostic as geese$error. In
# geepack/src/gee2.cc, error = 1 records exhaustion of the iteration loop;
# geeglm itself drops the starting glm's convergence flag.
.rt_check_gee_convergence <- function(fit, i = 1L) {
  error <- fit$geese$error
  if (is.numeric(error) && length(error) == 1L && !is.na(error) && error != 0) {
    cli::cli_abort(c(
      "Model {i} ({.cls geeglm}) records a GEE fitting error ({error}); its estimates and inference cannot be reported reliably.",
      "i" = "Refit and check {.code fit$geese$error}; inspect the iteration limit and working correlation."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  .rt_check_gee_working_correlation(fit, i)
  invisible(TRUE)
}

# With omitted waves, geese.fit uses 1:clusz within each cluster and
# genZcor names the unstructured parameters alpha.i:j. Thus a largest
# cluster uses this entire matrix. Explicit waves or a custom zcor are
# not recoverable from these stored parameters alone and are not guessed.
.rt_check_gee_working_correlation <- function(fit, i = 1L) {
  g <- fit$geese
  if (!identical(fit$corstr, "unstructured") ||
      !identical(g$model$cor.link, "identity") ||
      !is.null(fit$call$waves) || !is.null(fit$call$zcor)) return(invisible(TRUE))
  sizes <- g$clusz
  if (!is.numeric(sizes) || !length(sizes) || any(!is.finite(sizes))) return(invisible(TRUE))
  n <- max(sizes)
  if (n < 2 || n != floor(n)) return(invisible(TRUE))
  pairs <- utils::combn(n, 2L)
  keys <- paste0("alpha.", pairs[1L, ], ":", pairs[2L, ])
  alpha <- g$alpha
  if (!is.numeric(alpha) || !setequal(names(alpha), keys) ||
      length(alpha) != length(keys) || any(!is.finite(alpha))) return(invisible(TRUE))
  R <- diag(n)
  R[cbind(pairs[1L, ], pairs[2L, ])] <- unname(alpha[keys])
  R[cbind(pairs[2L, ], pairs[1L, ])] <- unname(alpha[keys])
  ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  # Roundoff-scaled threshold: a singular working correlation has no
  # usable inverse, even when geepack's iteration error is zero.
  tolerance <- 100 * .Machine$double.eps * n * max(abs(ev))
  if (min(ev) <= tolerance) {
    cli::cli_abort(c(
      "Model {i} ({.cls geeglm}) has a singular or indefinite unstructured working correlation; its estimates and inference cannot be reported reliably.",
      "i" = "Refit with an identifiable working correlation, such as independence or exchangeable, and inspect the cluster sizes."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_regtab_adapter.geeglm <- function(fit, ...) TRUE

# The reading of a geeglm fit: regtab(gee_as =) tags the fit's copy; a
# direct tt_vcov(fit, gee_as = "glm") call passes it (validated here too,
# review P2-1).
.rt_gee_as <- function(fit, gee_as = NULL) {
  if (!is.null(gee_as)) {
    if (!is.character(gee_as) || length(gee_as) != 1L || is.na(gee_as) || !gee_as %in% c("xtgee", "glm")) {
      cli::cli_abort("{.arg gee_as} must be {.val xtgee} or {.val glm}.", call = NULL)
    }
    if (identical(gee_as, "glm")) .rt_gee_check_glm(fit)
  }
  gee_as %||% fit$tt_gee_as %||% "xtgee"
}

.rt_gee_check_glm <- function(fit, i = NULL) {
  if (!identical(fit$corstr, "independence")) {
    where <- if (is.null(i)) "the model" else paste("model", i)
    cli::cli_abort(c(
      "{.code gee_as = \"glm\"} needs an independence working correlation ({where} uses {.val {fit$corstr}}).",
      "i" = "Only then are the GEE estimates Stata's {.code glm [pw], vce(cluster)} estimates."
    ), call = NULL)
  }
  invisible(TRUE)
}

# Tag a geeglm fit with its reading; the glm reading needs an independence
# working correlation (only then do the GEE estimates equal glm's).
.rt_gee_tag <- function(fit, gee_as, i = 1L) {
  if (identical(gee_as, "glm")) .rt_gee_check_glm(fit, i)
  fit$tt_gee_as <- gee_as
  fit
}

# Clusters as geepack forms them, runs of equal `id` values in row order,
# with at least one positive weight: Stata drops zero-weight observations
# (and so clusters of them only) from the estimation sample.
.rt_gee_groups <- function(fit) {
  w <- fit$prior.weights
  id <- fit$id
  run <- cumsum(c(TRUE, id[-1L] != id[-length(id)]))
  if (!is.null(w) && length(w) == length(run)) run <- run[w > 0]
  length(unique(run))
}

# geepack takes each run of equal `id` values as a cluster, so data not
# sorted by id split an id into several clusters: the fit is then not
# Stata's xtgee, which groups by the panel variable whatever the order
# (external review F31). Refused, with the fix.
.rt_gee_check_id <- function(fit, i = NULL) {
  id <- fit$id
  if (is.null(id) || length(id) < 2L) return(invisible(TRUE))
  runs <- sum(id[-1L] != id[-length(id)]) + 1L
  ids <- length(unique(id))
  if (runs != ids) {
    where <- if (is.null(i)) "" else paste0(" (model ", i, ")")
    cli::cli_abort(c(
      "This {.cls geeglm} fit{where} has {runs} clusters for {ids} distinct {.arg id} values: the data were not sorted by {.arg id}.",
      "i" = "{.fn geepack::geeglm} takes each run of equal {.arg id} values as a cluster, while Stata's {.code xtgee} groups by the panel variable.",
      "i" = "Sort the data by {.arg id} before fitting."
    ), call = NULL)
  }
  invisible(TRUE)
}

# Working correlations whose geepack fit is Stata's xtgee (muse P1-15;
# probe 2026-09-28, 200 x 4 panel, tolerance 1e-12): independence (the
# glm reading), exchangeable and unstructured agree with xtgee corr() to
# 1e-10 in the estimates and model-based SE. geepack's ar1 does not (b_x
# 0.49836 vs xtgee corr(ar1) 0.50005: a different moment estimator of
# the correlation), and "fixed"/"userdefined" have not been verified.
.rt_gee_corstr_ok <- c("independence", "exchangeable", "unstructured")

#' @export
tt_regtab_check.geeglm <- function(fit, i) {
  .rt_check_gee_convergence(fit, i)
  if (!isTRUE(fit$corstr %in% .rt_gee_corstr_ok)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn geepack::geeglm} fits with a {.val {format(fit$corstr)}} working correlation (model {i}).",
      "i" = "Only {.val {(.rt_gee_corstr_ok)}} reproduce Stata's {.code xtgee, corr()}; geepack's {.val ar1} estimates its correlation differently from {.code corr(ar 1)}."
    ), call = NULL)
  }
  .rt_gee_check_id(fit, i)
  .rt_gee_scale(fit, i)
  # xtgee's working-correlation estimate does not depend on a fixed scale
  # (probes G2 = G3), while geepack's scale.fix = TRUE fit estimates the
  # correlation with the dispersion held at its independence start value,
  # so its estimates differ from xtgee, scale(#)'s (8e-5 relative on the G2
  # fixture; review T2B-07). Said once per session.
  if (isTRUE(fit$geese$model$scale.fix) && !identical(fit$corstr, "independence")) {
    cli::cli_inform(c(
      "i" = "Model {i}: {.fn geepack::geeglm} with {.code scale.fix = TRUE} and a {.val {fit$corstr}} working correlation estimates the correlation differently from Stata's {.code xtgee, scale(#)}, so its estimates and standard errors can differ from Stata's beyond convergence.",
      " " = "With an independence working correlation the fit is xtgee's."
    ), .frequency = "once", .frequency_id = "tabtools_gee_scale_fix")
  }
  invisible(TRUE)
}

# A geeglm fitted with weights: xtgee with [pweight] forces vce(robust)
# (review P0-2: `xtgee ... [pw=w]` reports e(vce) "robust" without
# vce(robust); probe D13/E06).
.rt_gee_weighted <- function(fit) !is.null(fit$call$weights)

# robust and cluster: the GEE sandwich (clusters: the geeglm id) times
# G/(G - 1).
#' @export
tt_vce_types.geeglm <- function(fit) c("stata", "model", "robust", "cluster")

# Scale of the xtgee equivalent: the value a geeglm(scale.fix = TRUE) fixed
# (Stata scale(#); geepack's default scale.value is 1), else xtgee's
# default, 1 for binomial and Poisson and geepack's estimate otherwise.
# (With scale.fix = TRUE geepack still reports an estimated gamma and
# scales vbeta.naiv by it, so the variance divides it out either way;
# probes G1, G2: xtgee's scale(2) variance is the default one times
# 2 / phi-hat, with the same estimates, for independent and exchangeable
# working correlations.) geeglm() passes `scale.value` through
# model.frame(), so it takes one value per observation: it is evaluated
# like the formula's variables, and a constant vector stands for its
# scalar; anything else is refused (external review F29: a fixed scale was
# silently read as 1).
#
# Codex audit R1: the fit keeps only the expression (`fit$call$scale.value`):
# geepack does not use the value (geese.fit() holds the scale at its
# independence start whatever `scale.value` says) and stores it nowhere, not
# in `fit$geese`, `fit$model` or `fit$data`. Evaluating the expression in
# the caller's environment read today's value of a variable, so an unchanged
# fit's variance moved when the variable did. The expression is now
# evaluated only against what the fit stored: the columns of `fit$data` and
# the data argument's own name bound to `fit$data` (so
# `rep(2, nrow(d))` with `data = d` works); a scale that refers to anything
# else (`sc`, `rep(2, n)`) cannot be established from the fit and is
# refused, with the self-contained forms named.
.rt_gee_scale <- function(fit, i = NULL) {
  if (isTRUE(fit$geese$model$scale.fix)) {
    v <- fit$call$scale.value
    if (is.null(v)) return(1)
    where <- if (is.null(i)) "" else paste0(" (model ", i, ")")
    expr <- paste(deparse(v), collapse = "")
    dat <- if (is.data.frame(fit$data)) fit$data else NULL
    bind <- if (is.null(dat)) list() else as.list(dat)
    dname <- fit$call$data
    if (!is.null(dat) && is.name(dname)) bind[[as.character(dname)]] <- dat
    free <- setdiff(all.vars(v), names(bind))
    if (length(free)) {
      cli::cli_abort(c(
        "The fixed scale of this {.cls geeglm} fit{where} ({.code scale.value = {expr}}) refers to {.var {free}}, which the fit does not store.",
        "i" = "geepack keeps only the expression, so its value when the model was fitted cannot be established (the variable may have changed since).",
        "i" = "Refit with a scale taken from the data, e.g. {.code scale.value = rep(2, nrow(d))} with {.code data = d}, or a constant column of the data."
      ), call = NULL)
    }
    env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL) %||% globalenv()
    val <- tryCatch(eval(v, bind, env), error = function(e) NULL)
    ok <- is.numeric(val) && length(val) >= 1L && all(is.finite(val)) &&
      diff(range(val)) <= 1e-12 * max(1, abs(val)) && val[1] > 0
    if (!ok) {
      cli::cli_abort(c(
        "The fixed scale of this {.cls geeglm} fit{where} ({.code scale.value = {expr}}) is not one positive constant.",
        "i" = "Stata's {.code xtgee, scale(#)} fixes one value; give {.fn geepack::geeglm} a constant vector with one value per observation, e.g. {.code scale.value = rep(2, nrow(data))}."
      ), call = NULL)
    }
    return(val[1])
  }
  if (fit$family$family %in% c("binomial", "quasibinomial", "poisson", "quasipoisson")) return(1)
  unname(fit$geese$gamma[1])
}

#' @export
tt_vcov.geeglm <- function(fit, vce = "stata", cluster = NULL, gee_as = NULL, ...) {
  .rt_check_gee_convergence(fit)
  .rt_gee_check_id(fit)
  # "model" is the fit's own vcov(): geepack's variance for its std.err,
  # the sandwich by default, a jackknife for "jack", "j1s" or "fij".
  if (identical(vce, "model")) return(as.matrix(stats::vcov(fit)))
  if (!is.null(cluster)) {
    cli::cli_abort("A {.cls geeglm} fit clusters on its own {.arg id}; leave {.arg cluster} empty.", call = NULL)
  }
  reading <- .rt_gee_as(fit, gee_as)
  if (vce %in% c("robust", "cluster") || identical(reading, "glm") || .rt_gee_weighted(fit)) {
    # The GEE sandwich is the stored geese$vbeta (Codex audit R2): vcov()
    # of a geeglm follows its std.err, and returns a jackknife variance
    # (vbeta.ajs, vbeta.j1s, vbeta.fij) for std.err = "jack", "j1s" or
    # "fij". xtgee's vce(robust) and glm's vce(cluster) are the sandwich.
    G <- .rt_gee_groups(fit)
    # One cluster: no sandwich (G/(G - 1) is infinite; muse P2-16).
    if (G < 2L) {
      cli::cli_abort(c(
        "This {.cls geeglm} fit has {G} cluster{?s}: its robust (sandwich) variance needs at least two.",
        "i" = "Use {.code vce = \"stata\"} (xtgee's model-based variance) for an unweighted fit, or check {.arg id}."
      ), call = NULL)
    }
    V <- as.matrix(fit$geese$vbeta) * G / (G - 1)
    nm <- names(stats::coef(fit))
    dimnames(V) <- list(nm, nm)
    return(V)
  }
  g <- fit$geese
  V <- g$vbeta.naiv * .rt_gee_scale(fit) / unname(g$gamma[1])
  nm <- names(stats::coef(fit))
  dimnames(V) <- list(nm, nm)
  V
}

# Wald statistics are normal (Stata's xtgee and geepack both report z /
# chi-squared), also for families with an estimated scale.
#' @export
tt_wald_df.geeglm <- function(fit, vce = "stata", cluster = NULL, ...) Inf

#' @export
tt_model_stats.geeglm <- function(fit, info, ...) {
  b <- stats::coef(fit)
  rank <- sum(!is.na(b))
  if (identical(.rt_gee_as(fit), "glm")) {
    w <- fit$prior.weights
    # ll/AIC/BIC: Stata glm's pseudo-log-likelihood, filled in by
    # .rt_pw_stats() (review P0-4); glm stores no pseudo R-squared.
    return(list(N = if (is.null(w)) length(fit$y) else sum(w > 0), N_sub = NA_real_, ll = NA_real_,
                rank = rank, aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_,
                r2_a = NA_real_, groups = NA_real_, qic = NA_real_, icc = NA_real_))
  }
  y <- fit$y
  mu <- stats::fitted(fit)
  w <- fit$prior.weights
  if (is.null(w)) w <- rep(1, length(y))
  dev <- sum(fit$family$dev.resids(y, mu, w))
  phi <- .rt_gee_scale(fit)
  qic_ok <- is.finite(phi) && abs(phi - 1) <= 1e-10
  # Zero-weight observations (and clusters of them only) are outside
  # Stata's estimation sample.
  list(N = sum(w > 0), N_sub = NA_real_, ll = NA_real_, rank = rank, aic = NA_real_,
       bic = NA_real_, r2 = NA_real_, r2_p = NA_real_, r2_a = NA_real_,
       groups = .rt_gee_groups(fit), qic = if (qic_ok) dev + 2 * rank else NA_real_,
       icc = NA_real_, qic_note = !qic_ok)
}
