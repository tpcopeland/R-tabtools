# nlme::lme as Stata `mixed` (plan task 5.19), on the lmer path of
# R/regtab_models_mixed.R: the same random-effects rows, labels, intervals,
# ICC, groups and likelihood statistics.
#
# Stata facts (Stata 17 + tabtools 2.1.13 at Stata-Tools 68c37a90; goldens
# R78-R82, probe qa/stata/probe_nlme_lme.do):
# - Fixed effects: `mixed` reports the GLS variance (X'V^-1 X)^-1 at the
#   variance-component estimates, which is `fit$varFix` (= vcov() of the
#   lme fit): ML age SE 0.0032326 on mixed_bp in both (varFix 0.003232566;
#   lmer's vcov() agrees to 1e-9). summary.lme() and intervals.lme() do not
#   show that variance for an ML fit: with adjustSigma = TRUE (the default)
#   they rescale sigma, and so every fixed-effect standard error, by
#   sqrt(N / (N - p)) (0.003243396 here, +0.34% with N = 600, p = 4), and
#   they use t(fixDF) intervals where Stata uses z. regtab never reads
#   summary(): standard errors from varFix, z statistics and normal
#   quantiles. REML fits need no adjustment (summary.lme() leaves them
#   alone) and `mixed, reml` reports the REML GLS variance, varFix again
#   (age SE 0.0032412).
# - Log-likelihood: lme's logLik() is Stata's e(ll), for ML and for REML
#   (restricted log-likelihood -851.50061 in both), and its df is e(rank)
#   (6 for a random intercept), so AIC/BIC follow as for lmer.
# - Random-effects rows: exactly as for an lmer fit of the same model, from
#   the fitted covariance matrices (VarCorr) and sigma. The variance of
#   Stata's parameters (ln sd, atanh rho, ln sigma) is the inverse Hessian
#   of the deviance profiled over the fixed effects, the ML deviance or the
#   REML criterion, rebuilt from the fit's own design with lme4's penalized
#   least-squares identities (.rt_lmm_devfun_core(), the function the lmer
#   path differentiates) and checked against -2 logLik() before use. nlme's
#   own apVar is a coarser finite-difference Hessian (var(ln sd) 0.627737
#   vs Stata's 0.626704 on R21's model), so it is not used.
# - Covariance structures: pdSymm/pdLogChol/pdNatural (the default) is
#   `cov(unstructured)`, pdDiag `cov(independent)` (one variance row per
#   component, no covariance row, as an lmer `(1 | g) + (0 + x | g)` fit),
#   and any structure of a single random effect is Stata's identity.
#   Nested levels (`~ 1 | zone/location`) are Stata's `|| zone: ||
#   location:`, outermost first, named `var(_cons[location])`.
# - Boundary: lme's log-scale parameters never reach a zero variance or a
#   correlation of +/-1. The profiled deviance is minimised again with the
#   relative factor's diagonal bounded at 0, as lme4 does
#   (.rt_lme_optimum()): a fit short of that maximum is refused, and a term
#   whose maximum is on the boundary is shown there, with blank intervals,
#   as lmer's (.rt_re_struct_lme()).
#
# Refused, each with a message naming the feature (decision H-D6: no
# silent fallbacks): `correlation =` and `weights =` (residual correlation
# and variance functions: Stata's `mixed, residuals()` has no row in
# regtab's layout for their parameters, so the table would drop them),
# a fit short of its maximum, fixed sigma (`lmeControl(sigma =)`),
# pdIdent/pdCompSymm/pdBlocked with
# several random effects (`cov(identity)`/`cov(exchangeable)`, not checked
# against Stata), and a fit whose data no longer reproduce it (H4 contract: `keep.data = FALSE` and the data changed
# or gone). lme itself refuses offset() terms (Stata `mixed` has none).
# nlme::gls, nlme::nlme and MASS::glmmPQL stay refused by class.

#' @export
tt_regtab_adapter.lme <- function(fit, ...) TRUE

#' @export
tt_coef.lme <- function(fit, ...) nlme::fixef(fit)

# varFix, the GLS variance Stata's `mixed` reports (see the header); never
# summary.lme()'s rescaled one. `vce = "model"` is the same matrix (vcov()
# of an lme fit is varFix).
#' @export
tt_vcov.lme <- function(fit, vce = "stata", cluster = NULL, full = FALSE, ...) {
  if (isTRUE(full)) return(.rt_re_vcov_full(fit))
  b <- nlme::fixef(fit)
  V <- as.matrix(fit$varFix)
  dimnames(V) <- list(names(b), names(b))
  V
}

# Covariance structures of one random-effects level regtab lays out as
# Stata `mixed` does (see the header).
.rt_lme_pd_ok <- c("pdLogChol", "pdSymm", "pdNatural", "pdDiag")

#' @export
tt_regtab_check.lme <- function(fit, i) {
  ms <- fit$modelStruct
  if (!is.null(ms$corStruct)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nlme::lme} fits with a {.arg correlation} structure ({.cls {class(ms$corStruct)[1]}}, model {i}).",
      "i" = "Stata {.code mixed}'s {.code residuals()} option estimates such parameters, but regtab's {.code mixed} layout has no rows for them, so the table would leave them out.",
      "i" = "Fit the model without {.arg correlation}, or pass a data frame of the estimates you want to show (see {.help tabtools::regtab}, Data-frame input)."
    ), call = NULL)
  }
  if (!is.null(ms$varStruct)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nlme::lme} fits with a {.arg weights} variance function ({.cls {class(ms$varStruct)[1]}}, model {i}).",
      "i" = "Stata {.code mixed}'s {.code residuals(, by())} heteroskedastic residuals have no rows in regtab's {.code mixed} layout, so the table would leave their parameters out.",
      "i" = "Fit the model without {.arg weights}, or pass a data frame of the estimates you want to show (see {.help tabtools::regtab}, Data-frame input)."
    ), call = NULL)
  }
  if (isTRUE(attr(ms, "fixedSigma"))) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn nlme::lme} fits with a fixed residual standard deviation ({.code lmeControl(sigma = )}, model {i}).",
      "i" = "Stata {.code mixed} always estimates the residual variance."
    ), call = NULL)
  }
  for (lv in names(ms$reStruct)) {
    pd <- ms$reStruct[[lv]]
    cls <- class(pd)[1]
    k <- ncol(as.matrix(pd))
    if (!(cls %in% .rt_lme_pd_ok || k == 1L)) {
      cli::cli_abort(c(
        "{.fn regtab} does not support the {.fn {cls}} covariance structure of the random effects for {.var {lv}} (model {i}).",
        "i" = "Supported: unstructured ({.fn pdSymm}, {.fn pdLogChol}, the default; Stata {.code cov(unstructured)}) and diagonal ({.fn pdDiag}; Stata {.code cov(independent)}). Stata's {.code cov(identity)} and {.code cov(exchangeable)} rows have not been checked against Stata."
      ), call = NULL)
    }
  }
  .rt_lme_prep(fit, i)
  # A fit short of its maximum (lme stopped on a flat ridge towards a zero
  # variance or a correlation of +/-1): its estimates are not the maximum
  # likelihood ones Stata reports, so it is refused (decision H-D6).
  opt <- .rt_lme_optimum(fit)
  if (is.null(opt$error) && opt$short > .rt_lme_short_tol(opt$n, opt$singular)) {
    ll_fit <- format(-opt$dev_fit / 2, digits = 12)
    ll_max <- format(-opt$dev / 2, digits = 12)
    # lme's log-scale parameters cannot reach a zero variance or a
    # correlation of +/-1, so more iterations only help an interior maximum.
    hint <- if (isTRUE(opt$singular)) {
      c("i" = "The maximum is on the boundary (a zero variance or a correlation of +/-1), which lme's parameterization cannot reach, so refitting with lme will not get there.",
        "i" = "Fit the model with {.fn lme4::lmer}, which reaches it, or, if uncorrelated random effects suit the analysis, use a {.fn pdDiag} structure (Stata {.code cov(independent)}).")
    } else {
      c("i" = "lme stopped before its maximum. Refit with more iterations, e.g. {.code control = nlme::lmeControl(niterEM = 200, msMaxIter = 1000)}, or fit the model with {.fn lme4::lmer}.")
    }
    cli::cli_abort(c(
      "This {.fn nlme::lme} fit (model {i}) is not at its maximum: log-likelihood {ll_fit}, but {ll_max} is reached with the variances bounded at 0 (as lme4 fits them).",
      hint
    ), call = NULL)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# The fit's data

.rt_lme_fingerprint <- function(fit) {
  list(class(fit), deparse(fit$call), nlme::fixef(fit), fit$sigma, as.numeric(fit$logLik), fit$dims$N,
       fit$method, unlist(lapply(fit$modelStruct$reStruct, as.numeric)), sum(fit$fitted))
}

#' The data an lme fit was estimated on, checked against the fit
#'
#' lme keeps its data (`keep.data = TRUE`, the default) but no design
#' matrices. The model frame is rebuilt as lme builds it (every variable of
#' the fixed, random and grouping formulas, the call's `subset`, incomplete
#' rows dropped), put in the fit's row order by row name, and accepted only
#' when it reproduces the fit: the response (fitted + residuals), the
#' fixed-effect predictions X beta, the grouping of every level, and each
#' level's random-effects contribution Z b to the fitted values. Otherwise
#' an error names the fix (Milestone H task H4: the same table, or a
#' refusal; never numbers from other data).
#' @return list(frame = the model frame as a data frame with the data's
#'   variable labels, X, y, groups = fit$groups, Z = per level (nlme's
#'   names) its random-effects design).
#' @keywords internal
#' @noRd
.rt_lme_prep <- function(fit, i = NULL) {
  where <- if (is.null(i)) "" else paste0(" (model ", i, ")")
  kept <- is.data.frame(fit$data)
  no_data <- is.null(fit$call$data)
  # The fix depends on where the data came from: a fit that kept none
  # (keep.data = FALSE), variables read from the formula's environment (no
  # `data =`), or a fit that kept its data frame, which should always
  # reproduce it.
  hint <- if (kept) {
    "The fit keeps its data frame, so a variable its formula reads from the formula's environment (not from {.code data}) was probably changed since: call {.fn regtab} before changing it, or put it in the data frame. Otherwise please report it, with the call that fitted it."
  } else if (no_data) {
    "regtab reads an {.fn nlme::lme} fit's variables to lay out its rows and random effects, here from the formula's environment: refit with a data frame ({.code data = }, which lme keeps with the fit), or call {.fn regtab} before changing the variables."
  } else {
    "regtab reads an {.fn nlme::lme} fit's data to lay out its rows and random effects: refit with {.code keep.data = TRUE} (the default), or call {.fn regtab} before changing the data."
  }
  refuse <- function(why) {
    cli::cli_abort(c(
      paste0("The data of this {.cls lme} fit", where, " no longer reproduce it: ", why, "."),
      "i" = hint
    ), call = NULL, .envir = parent.frame())
  }
  reSt <- fit$modelStruct$reStruct
  ff <- stats::formula(fit$terms)
  env <- environment(ff) %||% globalenv()
  src <- if (kept) fit$data else if (no_data) NULL else tryCatch(eval(fit$call$data, env), error = function(e) NULL)
  if (!no_data && !is.data.frame(src)) {
    dn <- paste(deparse(fit$call$data), collapse = " ")
    refuse("its data {.code {dn}} cannot be found")
  }
  if (!is.null(src)) src <- as.data.frame(src)
  gform <- nlme::getGroupsFormula(reSt)
  fall <- nlme::asOneFormula(stats::formula(reSt), ff, gform)
  environment(fall) <- env
  # As lme.formula builds its frame: unused factor levels dropped (a subset
  # or na.omit that removes a whole level leaves no column for it).
  args <- list(formula = fall, data = src, na.action = stats::na.omit, drop.unused.levels = TRUE)
  # The call's `subset`, an expression or a one-sided formula, as lme reads
  # it (nlme's asOneSidedFormula(), which nlme does not export).
  sub <- fit$call$subset
  if (!is.null(sub)) args$subset <- if (is.call(sub) && identical(sub[[1L]], as.name("~"))) sub[[length(sub)]] else sub
  mf <- tryCatch(do.call(stats::model.frame, args), error = function(e) e)
  if (inherits(mf, "error")) {
    msg <- conditionMessage(mf)
    refuse("its variables cannot be evaluated ({msg})")
  }
  rn <- rownames(fit$groups)
  if (!identical(rownames(mf), rn) && !is.null(rn) && all(rn %in% rownames(mf))) mf <- mf[rn, , drop = FALSE]
  N <- fit$dims$N
  if (nrow(mf) != N) {
    nr <- nrow(mf)
    refuse("they give {nr} complete observation{?s}, the fit has {N}")
  }
  close <- function(a, b) length(a) == length(b) && all(is.finite(a)) && max(abs(a - b)) <= 1e-7 * max(1, abs(b))
  # The designs are built from the frame's columns without its "terms"
  # attribute, which model.matrix() would otherwise take for the random
  # formula's terms and refuse ("model frame and formula mismatch") when
  # that formula transforms a variable (~ log(age) | Subject).
  mfd <- as.data.frame(mf)
  attr(mfd, "terms") <- NULL
  fx <- stats::model.frame(fit$terms, mfd)
  X <- tryCatch(stats::model.matrix(fit$terms, fx, contrasts.arg = fit$contrasts), error = function(e) NULL)
  y <- as.numeric(stats::model.response(fx))
  b <- nlme::fixef(fit)
  fitted <- as.matrix(fit$fitted)
  fixed <- unname(fitted[, "fixed"])
  if (is.null(X) || !identical(colnames(X), names(b))) refuse("their design matrix has other columns than the fit's coefficients")
  X <- unname(X)
  dimnames(X) <- list(NULL, names(b))
  if (!close(y, fixed + unname(as.matrix(fit$residuals)[, "fixed"]))) refuse("the response differs")
  if (!close(drop(X %*% b), fixed)) refuse("the fixed-effect predictions differ")
  gr <- tryCatch(nlme::getGroups(mf, gform), error = function(e) NULL)
  if (is.factor(gr)) gr <- stats::setNames(data.frame(gr), names(fit$groups)[ncol(fit$groups)])
  # lme names an inner level's groups by the whole path ("4/40").
  same_groups <- is.data.frame(gr) && identical(names(gr), names(fit$groups)) && {
    path <- Reduce(function(a, b) paste(a, b, sep = "/"), lapply(gr, as.character), accumulate = TRUE)
    all(vapply(seq_along(path), function(j) identical(path[[j]], as.character(fit$groups[[j]])), TRUE))
  }
  if (!same_groups) refuse("the grouping differs")
  re <- nlme::ranef(fit)
  if (is.data.frame(re)) re <- stats::setNames(list(re), names(reSt))
  cols <- colnames(fitted)
  Z <- list()
  for (lv in names(reSt)) {
    comps <- colnames(as.matrix(reSt[[lv]]))
    Zl <- tryCatch({
      rf <- stats::formula(reSt[[lv]])
      stats::model.matrix(rf, stats::model.frame(rf, mfd))
    }, error = function(e) NULL)
    if (is.null(Zl) || !identical(colnames(Zl), comps)) refuse("the random-effects design for {.var {lv}} differs")
    Zl <- unname(Zl)
    dimnames(Zl) <- list(NULL, comps)
    R <- as.matrix(re[[lv]])
    gi <- match(as.character(fit$groups[[lv]]), rownames(R))
    at <- match(lv, cols)
    if (anyNA(gi) || is.na(at) || at < 2L) refuse("the random effects for {.var {lv}} cannot be matched")
    contrib <- rowSums(Zl * R[gi, comps, drop = FALSE])
    if (!close(contrib, unname(fitted[, at] - fitted[, at - 1L]))) refuse("the random-effects predictions for {.var {lv}} differ")
    Z[[lv]] <- Zl
  }
  frame <- as.data.frame(mf)
  attr(frame, "terms") <- NULL
  attr(frame, "na.action") <- NULL
  for (v in intersect(names(frame), names(src %||% list()))) {
    for (a in c("label", "labels")) {
      val <- attr(src[[v]], a, exact = TRUE)
      if (!is.null(val)) attr(frame[[v]], a) <- val
    }
  }
  list(frame = frame, X = X, y = y, groups = fit$groups, Z = Z)
}

# An lm fit of the fixed part on the checked frame, with the lme's
# contrasts: only its structure is read (terms, levels, labels, reference
# rows) by the Phase 4 row builder, which has no lme methods to call
# (model.frame() of an lme partially matches `$modelStruct`).
.rt_lme_proxy <- function(fit, pr) {
  ff <- stats::formula(fit$terms)
  environment(ff) <- list2env(list(.tt_lme_frame = pr$frame), parent = environment(ff) %||% globalenv())
  ctr <- .rt_lme_contrast_names(fit$contrasts, pr$frame)
  proxy <- eval(bquote(stats::lm(.(ff), data = .tt_lme_frame, contrasts = .(if (length(ctr)) ctr else NULL))),
                environment(ff))
  b <- nlme::fixef(fit)
  if (!identical(names(stats::coef(proxy)), names(b)) || anyNA(stats::coef(proxy))) {
    cli::cli_abort("Internal error: the fixed part of this {.cls lme} fit could not be laid out ({.val {names(b)}}).",
                   call = NULL)
  }
  proxy
}

# lme stores each factor's contrasts as the matrix it used; name the
# standard ones, so that the row builder's contrast checks name them as for
# lmer (contr.sum refused by name, an ordered factor's contr.poly with its
# own hint) instead of calling them "custom".
.rt_lme_contrast_names <- function(ctr, frame) {
  # Treatment-type matrices stay matrices (their column names are the
  # fit's coefficient names, and the row builder accepts them as they are).
  std <- c("contr.sum", "contr.helmert", "contr.poly")
  for (v in names(ctr)) {
    m <- ctr[[v]]
    x <- frame[[v]]
    if (!is.matrix(m) || is.null(x)) next
    lev <- levels(factor(x))
    for (fn in std) {
      cm <- tryCatch(unname(match.fun(fn)(lev)), error = function(e) NULL)
      if (!is.null(cm) && identical(dim(cm), dim(m)) && isTRUE(all.equal(cm, unname(m), check.attributes = FALSE))) {
        ctr[[v]] <- fn
        break
      }
    }
  }
  ctr
}

# ---------------------------------------------------------------------------
# Rows and statistics

# The Phase 4 rows of the proxy's structure, with the lme's own Wald
# numbers: varFix standard errors, z statistics.
#' @export
tt_regtab_rows.lme <- function(fit, info, level = 0.95, ci_method = "wald", vce = "stata", cluster = NULL, ...) {
  wald <- tt_wald(fit, level, ci_method, vce, cluster)
  pr <- .rt_lme_prep(fit)
  rows <- tt_regtab_rows(.rt_lme_proxy(fit, pr), info, level = level, ci_method = "wald", vce = "stata",
                         cluster = NULL, ...)
  est <- rows$status %in% "est"
  hit <- match(rows$term, wald$term)
  if (any(est & is.na(hit))) {
    cli::cli_abort("Internal error: a row of the {.cls lme} fit has no coefficient ({.val {rows$term[est & is.na(hit)]}}).",
                   call = NULL)
  }
  tr <- if (isTRUE(info$exponentiate)) exp else identity
  rows$estimate[est] <- tr(wald$estimate[hit[est]])
  rows$conf.low[est] <- tr(wald$conf.low[hit[est]])
  rows$conf.high[est] <- tr(wald$conf.high[hit[est]])
  rows$p.value[est] <- wald$p.value[hit[est]]
  rows
}

#' @export
tt_regtab_trailing_rows.lme <- function(fit, info, o, ...) {
  if (isTRUE(o$noreeffects)) return(NULL)
  .rt_re_rows(fit, info, o)
}

#' @export
tt_model_stats.lme <- function(fit, info, ...) {
  lg <- stats::logLik(fit)
  .rt_mixed_stats(fit, info, as.numeric(lg), attr(lg, "df"))
}

# ---------------------------------------------------------------------------
# Random-effects structure and uncertainty (the lmer path's shapes; see
# .rt_re_struct() and .rt_re_uncertainty() in R/regtab_models_mixed.R)

# Relative scaled standard deviation below which a variance component of an
# lme fit is on the boundary, used only when the bounded refit below fails
# (see .rt_re_struct_lme()).
.rt_lme_boundary <- 1e-3

# The bounded refit's counterpart (.rt_lme_snap()): contributions below 1e-5
# of sigma are optimizer stops short of the bound (2.2e-7 observed). Larger
# ones, though statistically null, are left as lmer leaves them: interior
# maxima at 6e-4 to 1e-3 of sigma exist (independent review, 2026-10-01).
.rt_lme_snap_bound <- 1e-5

# The lme fit's random-effects terms as estimated (see .rt_re_struct_lme()).
.rt_re_struct_lme_raw <- function(fit) {
  pr <- .rt_lme_prep(fit)
  reSt <- fit$modelStruct$reStruct
  lv <- names(reSt)
  sig <- fit$sigma
  # nlme lists levels innermost first; passed outermost first, so that a tie
  # in the number of groups keeps the hierarchy's order.
  ol <- rev(lv)
  groups <- .rt_re_groups(ol, as.integer(fit$dims$ngrps[ol]), pr$frame)
  terms <- list()
  for (l in lv) {
    pd <- reSt[[l]]
    S <- unname(as.matrix(pd)) * sig^2
    comps <- colnames(as.matrix(pd))
    gi <- match(l, groups$name)
    rms <- sqrt(colMeans(pr$Z[[l]]^2))
    if (identical(class(pd)[1], "pdDiag") && length(comps) > 1L) {
      # Independent components: one term each, as lmer's (1 | g) + (0 + x | g).
      for (j in seq_along(comps)) {
        terms[[length(terms) + 1L]] <- list(group = gi, comps = comps[j], Sigma = S[j, j, drop = FALSE],
                                            level = l, cols = j, rms = rms[j])
      }
    } else {
      terms[[length(terms) + 1L]] <- list(group = gi, comps = comps, Sigma = S, level = l, cols = seq_along(comps),
                                          rms = rms)
    }
  }
  list(style = "mixed", groups = groups, terms = terms, resid = sig^2, sigma = sig, use_sc = TRUE,
       frame = pr$frame, prep = pr)
}

.rt_lme_opt_cache <- new.env(parent = emptyenv())

#' The maximum of an lme fit's likelihood with the variances bounded at 0
#'
#' lme optimises log-scale parameters, which never reach a zero variance or
#' a correlation of +/-1: at such a maximum it stops on a flat ridge, at a
#' tiny variance or short of the maximum. lme4 optimises the relative
#' Cholesky factor with its diagonal bounded at 0, and reports exact zeros
#' there. This minimises the same profiled deviance (.rt_lmm_devfun_core(),
#' which equals -2 logLik of an lmer fit at its parameters) over the
#' factor, bounded as lme4 bounds it, from lme's estimate (stats::nlminb()).
#' @return list(theta = the minimiser, per term in lme4's order; dev = the
#'   minimum; dev_fit = -2 logLik(fit); short = how far the minimum lies
#'   below dev_fit), or list(error = message) when the refit fails. Cached by
#'   the fit's fingerprint.
#' @keywords internal
#' @noRd
.rt_lme_optimum <- function(fit, raw = .rt_re_struct_lme_raw(fit)) {
  key <- .rt_lme_fingerprint(fit)
  if (identical(.rt_lme_opt_cache$key, key)) return(.rt_lme_opt_cache$value)
  val <- tryCatch({
    pr <- raw$prep
    n <- length(pr$y)
    des <- .rt_lme_design(raw)
    dev <- .rt_lmm_devfun_core(X = pr$X, Zt = des$Zt, y = pr$y, off = rep(0, n), w = rep(1, n),
                               reml = identical(fit$method, "REML"), lambdat = des$lambdat)
    theta0 <- unlist(lapply(raw$terms, function(tm) {
      L <- .rt_chol0(tm$Sigma / raw$sigma^2)
      L[lower.tri(L, diag = TRUE)]
    }))
    lower <- unlist(lapply(raw$terms, function(tm) {
      k <- length(tm$comps)
      m <- matrix(-Inf, k, k)
      diag(m) <- 0
      m[lower.tri(m, diag = TRUE)]
    }))
    # Optimised on the scale of each component's contribution (the factor's
    # row times the root mean square of that component's design column):
    # on theta itself nlminb crawls when a covariate's scale is far from 1.
    sc <- unlist(lapply(raw$terms, function(tm) {
      k <- length(tm$comps)
      m <- tm$rms %o% rep(1, k)
      m[lower.tri(m, diag = TRUE)]
    }))
    f <- function(th) dev(th)
    opt <- stats::nlminb(theta0 * sc, function(p) f(p / sc), lower = lower,
                         control = list(rel.tol = 1e-13, x.tol = 1e-12, eval.max = 1000L, iter.max = 300L))
    th <- opt$par / sc
    d_opt <- opt$objective
    # Never report a refit worse than lme's own point.
    d0 <- f(theta0)
    if (!is.finite(d_opt) || d_opt > d0) {
      th <- theta0
      d_opt <- d0
    }
    dev_fit <- -2 * as.numeric(stats::logLik(fit))
    th <- .rt_lme_snap(th, raw$terms, f, d_opt, .rt_lme_short_tol(n, TRUE))
    # Whether the maximum is on the boundary (a zero on some term's factor
    # diagonal: a zero variance or a correlation of +/-1).
    pos <- 0L
    singular <- FALSE
    for (tm in raw$terms) {
      k <- length(tm$comps)
      L <- matrix(0, k, k)
      L[lower.tri(L, diag = TRUE)] <- th[pos + seq_len(k * (k + 1L) / 2L)]
      pos <- pos + k * (k + 1L) / 2L
      singular <- singular || any(.rt_lme_zero_diag(L))
    }
    list(theta = th, dev = d_opt, dev_fit = dev_fit, short = dev_fit - d_opt, n = n, singular = singular)
  }, error = function(e) list(error = conditionMessage(e)))
  .rt_lme_opt_cache$key <- key
  .rt_lme_opt_cache$value <- val
  val
}

# nlminb can stop a hair inside the bound it is converging to: 3.97e-9
# instead of 0 for mixed_bp's age slope where long double is double (macOS
# arm64; CI 2026-10-01), which then read as an interior variance. Each
# diagonal entry of a term's factor whose contribution (times the
# component's root mean square) is below .rt_lme_snap_bound of sigma is put on
# the bound when the deviance there, `dev(theta)`, stays within `tol` of the
# minimum `d_opt`. Returns theta.
.rt_lme_snap <- function(theta, terms, dev, d_opt, tol) {
  pos <- 0L
  for (tm in terms) {
    k <- length(tm$comps)
    idx <- pos + seq_len(k * (k + 1L) / 2L)
    pos <- pos + length(idx)
    L <- matrix(0, k, k)
    L[lower.tri(L, diag = TRUE)] <- theta[idx]
    for (j in seq_len(k)) {
      if (!(L[j, j] > 0 && L[j, j] * tm$rms[j] < .rt_lme_snap_bound)) next
      L0 <- L
      L0[j, j] <- 0
      th0 <- theta
      th0[idx] <- L0[lower.tri(L0, diag = TRUE)]
      d0 <- dev(th0)
      if (is.finite(d0) && d0 - d_opt <= tol) {
        theta <- th0
        L <- L0
      }
    }
  }
  theta
}

# How far below -2 logLik(fit) the bounded maximum may lie before the fit
# counts as short of its maximum. Scale-free: per observation (the deviance
# itself shifts by N log(c^2) when the response is multiplied by c), plus
# 1e-6. At a boundary maximum lme stops short by design (its log-scale
# parameters never reach the zero), by up to 2e-9 per observation in the
# reviews' scans: 5e-9 per observation is allowed there. At an interior
# maximum lme converges to ~1e-11 per observation (goldens R78-R82), so
# 1e-9 is allowed; fits genuinely short of an interior maximum were 1.5e-8
# per observation and more (N = 1e4 and 4e4, the variance ratio off by
# 1.6-2%), and 6e-5 or more in all at small N (task 5.19 reviews).
.rt_lme_short_tol <- function(n, boundary = FALSE) (if (isTRUE(boundary)) 5e-9 else 1e-9) * n + 1e-6

.rt_re_struct_lme <- function(fit) {
  re <- .rt_re_struct_lme_raw(fit)
  opt <- .rt_lme_optimum(fit, re)
  sig <- re$sigma
  pos <- 0L
  for (t in seq_along(re$terms)) {
    tm <- re$terms[[t]]
    k <- length(tm$comps)
    nt <- k * (k + 1L) / 2L
    idx <- pos + seq_len(nt)
    pos <- pos + nt
    if (is.null(opt$error)) {
      # Boundary: where the bounded maximum has a zero on the factor's
      # diagonal (a zero variance, or a correlation of +/-1), lme's estimate
      # sits on a flat ridge towards it. The term is shown at that maximum,
      # zeros exact, with no intervals, as lmer's (its fixed theta).
      L <- matrix(0, k, k)
      L[lower.tri(L, diag = TRUE)] <- opt$theta[idx]
      z <- .rt_lme_zero_diag(L)
      if (any(z)) {
        diag(L)[z] <- 0
        S <- tcrossprod(L) * sig^2
        S[abs(S) < 1e-300] <- 0
        tm$Sigma <- S
        tm$boundary <- TRUE
      }
    } else {
      # The refit failed: a component whose standard deviation, times the
      # root mean square of its design column, is below 1e-3 of sigma (a
      # variance contribution below 1e-6 of the residual variance) is taken
      # to be at the boundary (observed: boundary cases 3e-5 to 2e-4,
      # interior ones 0.027 and above).
      S <- tm$Sigma
      bnd <- sqrt(pmax(diag(S), 0)) * tm$rms < .rt_lme_boundary * sig
      if (any(bnd)) {
        S[bnd, ] <- 0
        S[, bnd] <- 0
        tm$Sigma <- S
        tm$boundary <- TRUE
      }
    }
    re$terms[[t]] <- tm
  }
  re
}

# Transposed random-effects design of the terms (group-major columns, as
# lme4 orders them) and the transposed relative covariance factor at theta
# (each term's lower-triangular factor, column-major, as lme4's theta).
.rt_lme_design <- function(re) {
  pr <- re$prep
  n <- length(pr$y)
  blocks <- lapply(re$terms, function(tm) {
    g <- factor(as.character(pr$groups[[tm$level]]))
    G <- nlevels(g)
    k <- length(tm$cols)
    Zl <- pr$Z[[tm$level]][, tm$cols, drop = FALSE]
    Z <- Matrix::sparseMatrix(i = rep(seq_len(n), k), j = (rep(as.integer(g), k) - 1L) * k + rep(seq_len(k), each = n),
                              x = as.numeric(Zl), dims = c(n, G * k))
    list(Zt = Matrix::t(Z), G = G, k = k)
  })
  Zt <- do.call(rbind, lapply(blocks, `[[`, "Zt"))
  lambdat <- function(theta) {
    pos <- 0L
    Matrix::bdiag(lapply(blocks, function(bl) {
      nt <- bl$k * (bl$k + 1L) / 2L
      L <- matrix(0, bl$k, bl$k)
      L[lower.tri(L, diag = TRUE)] <- theta[pos + seq_len(nt)]
      pos <<- pos + nt
      Matrix::kronecker(Matrix::Diagonal(bl$G), Matrix::Matrix(t(L), sparse = TRUE))
    }))
  }
  list(Zt = Zt, lambdat = lambdat)
}

.rt_re_uncertainty_lme <- function(fit) {
  re <- .rt_re_struct_lme(fit)
  pr <- re$prep
  pm <- .rt_re_param_map(re$terms)
  psi <- c(pm$psi, log(re$sigma))
  free <- c(pm$free, TRUE)
  lnsig <- length(psi)
  map <- pm$map
  des <- .rt_lme_design(re)
  n <- length(pr$y)
  dev <- .rt_lmm_devfun_core(X = pr$X, Zt = des$Zt, y = pr$y, off = rep(0, n), w = rep(1, n),
                             reml = identical(fit$method, "REML"), lambdat = des$lambdat)
  # A term on the boundary keeps the fit's relative factor (as lmer keeps
  # its theta), zero rows included.
  theta_fixed <- lapply(re$terms, function(tm) {
    L <- .rt_chol0(tm$Sigma / re$sigma^2)
    L[lower.tri(L, diag = TRUE)]
  })
  theta_of <- function(p) {
    unlist(lapply(seq_along(re$terms), function(t) {
      m <- map[[t]]
      if (!free[m$lnsd[1]]) return(theta_fixed[[t]])
      .rt_re_theta(p[m$lnsd], p[m$atr], m$pairs, exp(p[lnsig]))
    }))
  }
  ll <- as.numeric(stats::logLik(fit))
  d0 <- dev(theta_of(psi), psi[lnsig])
  if (!is.finite(d0) || abs(d0 + 2 * ll) > 1e-6 * max(1, abs(ll))) {
    stop("its deviance could not be rebuilt from the fit (", format(d0, digits = 12), " vs -2 logLik ",
         format(-2 * ll, digits = 12), ")", call. = FALSE)
  }
  fi <- which(free)
  f <- function(x) {
    p <- psi
    p[fi] <- x
    dev(theta_of(p), p[lnsig])
  }
  H <- .rt_num_hessian(f, psi[fi], rep(0.01, length(fi)))
  V <- matrix(NA_real_, length(psi), length(psi))
  V[fi, fi] <- 2 * solve(H)
  list(psi = psi, V = V, map = map, lnsig = lnsig, lnalpha = NA_integer_, V_beta = NULL)
}

# Lower Cholesky factor of a positive semi-definite matrix whose zero
# variances have zero covariances (a boundary term): zero rows and columns
# where the pivot is zero.
.rt_chol0 <- function(S) {
  k <- nrow(S)
  L <- matrix(0, k, k)
  for (j in seq_len(k)) {
    h <- seq_len(j - 1L)
    v <- S[j, j] - sum(L[j, h]^2)
    if (!(v > 0)) next
    L[j, j] <- sqrt(v)
    if (j < k) for (r in (j + 1L):k) L[r, j] <- (S[r, j] - sum(L[r, h] * L[j, h])) / L[j, j]
  }
  L
}

# Zeros on the diagonal of a bounded refit's factor: exactly 0 (a bound
# nlminb reached), or negligible against the rest of its row (nlminb's
# "singular convergence" stops a hair from the bound on the ridge towards a
# correlation of +/-1: 1e-7 of the row's norm on the reviews' seed 22).
.rt_lme_zero_diag <- function(L) {
  d <- diag(L)
  rn <- sqrt(rowSums(L^2))
  d <= 0 | d <= 1e-6 * rn
}
