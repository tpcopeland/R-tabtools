# regtab adapters for parametric survival and competing-risks models (plan
# task 5.4): survival::survreg (Stata streg, AFT), Fine-Gray through
# survival::coxph on survival::finegray() data, and cmprsk::crr (Stata
# stcrreg).
#
# Stata facts (goldens R19, R20; probes 2026-09-25,
# qa/stata/make_regtab_phase5a.do):
# - streg, time: TR header, estimates and CI bounds exponentiated (R19,
#   tabtools 2.1.10+; 2.1.9 printed the log-time coefficients), automatic
#   nointercept. With keepintercept the ancillary parameter and its diparms
#   follow the covariates and precede the intercept: `ln_p`, `p`, `1/p`
#   (Weibull), `lnsigma`, `sigma` (lognormal), `lngamma`, `gamma`
#   (loglogistic); the diparms are exp-transformed with no p-value. R's
#   Log(scale) is ln(sigma) and ln(gamma), and -ln(p) for the Weibull. The
#   exponential has none.
# - streg's log-likelihood omits the Jacobian of the log-time transform:
#   e(ll) = logLik(<survreg>) + sum over failures of w log(t) (all four
#   distributions agree to 1e-12). N_sub = N, so the n row reads Subjects;
#   e(rank) counts the coefficients and the ancillary parameter.
# - stcrreg: SHR, exponentiated, standard errors from vce(robust), the
#   Fine-Gray sandwich clustered by subject with the small-sample factor
#   N/(N-1): cmprsk::crr's variance times N/(N-1) equals Stata's to 1e-9
#   (R20 probe, 3,500 subjects), and crr's estimates equal stcrreg's.
# - Golden R20 p = 0.64 for age: coxph() on finegray() data reports a
#   sandwich that treats every expanded row as independent (0.62) and
#   se(coef) the naive model-based one (0.65); clustering the sandwich by
#   subject and applying N/(N-1) gives 0.64. It still differs from Stata's
#   by ~1e-4 (relative), because the weighted Cox score omits the term for
#   the estimated censoring distribution, and the estimates by ~1e-3
#   (finegray()'s tied-time weights); use cmprsk::crr() for stcrreg's exact
#   numbers.
# - finegray() drops the "label" attribute of plain numeric columns
#   (`index_age`; haven-labelled columns keep theirs), so regtab restores
#   labels from the data finegray() was given when that data frame is found
#   in the model's environment and matches the expanded data row for row.

# A Cox fit does not keep coxph.fit's convergence flag. For the ordinary
# right-censored Breslow/Efron fitter, coxfit6.c increments iter beyond the
# limit only on exhaustion; convergence exactly at the limit is valid.
# Read only literal limits from the saved call, never a mutable control
# object or expression in the formula environment.
.rt_cox_literal_limit <- function(fit) {
  call <- fit$call
  if (!is.call(call)) return(NA_real_)
  literal <- function(x) {
    if (is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x == floor(x)) {
      return(as.numeric(x))
    }
    NA_real_
  }
  matched_fit <- try(match.call(definition = survival::coxph,
                                call = call, expand.dots = FALSE), silent = TRUE)
  if (inherits(matched_fit, "try-error")) return(NA_real_)
  control <- matched_fit$control
  if (is.null(control)) {
    # coxph passes its named control dots to coxph.control. Normalize the
    # saved expressions as that call would, without evaluating any of them.
    control <- as.call(c(list(quote(survival::coxph.control)), as.list(matched_fit$...)))
  }
  if (!is.call(control) || !paste(deparse(control[[1L]]), collapse = "") %in%
      "survival::coxph.control") return(NA_real_)
  # match.call normalizes partial and positional argument names without
  # evaluating any argument expression or reading today's control value.
  matched <- try(match.call(definition = survival::coxph.control,
                            call = control, expand.dots = FALSE), silent = TRUE)
  if (inherits(matched, "try-error")) return(NA_real_)
  args <- as.list(matched)[-1L]
  if ("iter.max" %in% names(args)) return(literal(args[["iter.max"]]))
  as.numeric(survival::coxph.control()$iter.max)
}

.rt_check_survival_convergence <- function(fit, i = 1L) {
  if (inherits(fit, "coxph") && !inherits(fit, "clogit") &&
      isTRUE(fit$method %in% c("breslow", "efron")) &&
      !is.null(fit$y) && identical(attr(fit$y, "type"), "right")) {
    limit <- .rt_cox_literal_limit(fit)
    if (!is.na(limit) && is.numeric(fit$iter) && length(fit$iter) == 1L &&
        is.finite(fit$iter) && fit$iter > limit) {
      cli::cli_abort(c(
        "Model {i} ({.cls coxph}) exhausted its {limit}-iteration limit; its estimates and inference cannot be reported reliably.",
        "i" = "Refit with a larger iteration limit and inspect any warnings about infinite coefficients."
      ), class = "tabtools_error_model_convergence", call = NULL)
    }
  }
  if (inherits(fit, "survreg") &&
      identical(attr(fit$terms, "intercept"), 1L)) {
    ll <- fit$loglik
    # survreg.fit drops its failure flag, and iter == max also occurs for
    # valid final-step convergence. A materially worse likelihood than the
    # nested intercept-only fit does establish failure to attain an optimum.
    # Without an intercept the reference fit is not nested.
    if (is.numeric(ll) && length(ll) == 2L && all(is.finite(ll)) &&
        ll[2L] < ll[1L] - 1e-7 * (1 + abs(ll[1L]))) {
      cli::cli_abort(c(
        "Model {i} ({.cls survreg}) has a fitted log-likelihood below its intercept-only model; its estimates and inference cannot be reported reliably.",
        "i" = "Refit and check convergence and the iteration limit."
      ), class = "tabtools_error_model_convergence", call = NULL)
    }
  }
  invisible(TRUE)
}

#' @export
tt_regtab_adapter.survreg <- function(fit, ...) TRUE

#' @export
tt_regtab_adapter.crr <- function(fit, ...) TRUE

# Distributions with a Stata equivalent: streg's weibull, exponential,
# lognormal and loglogistic (log time), and intreg/tobit's normal. survreg's
# logistic, t, extreme-value, rayleigh and user distributions have none.
# clogit (task 5.15): Stata's clogit maximizes the exact conditional
# likelihood, survival::clogit's default; its "approximate" and "efron"
# methods are Cox partial likelihoods with ties (equal to it only in 1:1
# sets). An exact fit cannot be weighted: clogit drops the weights with a
# warning and fits the unweighted model, so a call that named weights is
# refused whatever its method (muse P0-3), rather than tabled as if it were
# weighted.
#' @export
tt_regtab_adapter.clogit <- function(fit, ...) TRUE

#' @export
tt_regtab_check.clogit <- function(fit, i) {
  uc <- fit$userCall
  if (is.call(uc) && "weights" %in% names(as.list(uc))[-1L]) {
    cli::cli_abort(c(
      "{.fn regtab} does not support weighted {.fn survival::clogit} fits (model {i}).",
      "i" = "With {.code method = \"exact\"} clogit ignores {.arg weights} (with a warning) and fits the unweighted model; the other methods are Cox partial likelihoods, not Stata's {.code clogit}.",
      "i" = "Refit without {.arg weights}."
    ), call = NULL)
  }
  if (!identical(fit$method, "exact")) {
    cli::cli_abort(c(
      "{.fn regtab} supports {.fn survival::clogit} fits with {.code method = \"exact\"} only (model {i} uses {.val {fit$method}}).",
      "i" = "Stata's {.code clogit} maximizes the exact conditional likelihood: refit with {.code method = \"exact\"} (the default)."
    ), call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_regtab_check.survreg <- function(fit, i) {
  .rt_check_survival_convergence(fit, i)
  dist <- fit$dist
  if (!is.character(dist) || length(dist) != 1L || !dist %in% c(.rt_sr_logtime, "gaussian")) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn survival::survreg} fits with the {.val {format(dist)}} distribution (model {i}).",
      "i" = "Supported: {.val {c(.rt_sr_logtime, 'gaussian')}} (Stata {.code streg}; {.code intreg}/{.code tobit} for the Gaussian)."
    ), call = NULL)
  }
  if (inherits(fit, "tobit") && !identical(dist, "gaussian")) {
    cli::cli_abort("{.fn regtab} supports Gaussian tobit fits only (Stata {.code tobit}; model {i}).", call = NULL)
  }
  # strata() in a survreg formula gives each stratum its own scale (one
  # Log(scale) row set per stratum, labelled by stratum), a layout with no
  # Stata streg equivalent: streg, strata() also shifts the intercept and
  # reports an ln_p equation (muse P1-21). Said once per session.
  if (length(fit$scale) > 1L) {
    cli::cli_inform(c(
      "i" = "Model {i} ({.fn survival::survreg} with {.fn strata}) has one scale per stratum; its ancillary rows, one set per stratum, are an R layout with no Stata {.code streg} equivalent.",
      " " = "Stata's {.code streg, strata()} also lets the intercept vary and reports an {.code ln_p} equation instead."
    ), .frequency = "once", .frequency_id = "tabtools_survreg_strata")
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# survreg (streg)

# Stata's names for the ancillary parameter of each distribution (log
# scale, its diparm, and the diparm of the reciprocal), and the sign that
# maps log(scale) onto it.
.rt_sr_anc_names <- function(dist) {
  switch(dist,
    weibull = list(ln = "ln_p", d1 = "p", d2 = "1/p", sign = -1),
    loglogistic = , logistic = list(ln = "lngamma", d1 = "gamma", d2 = NULL, sign = 1),
    list(ln = "lnsigma", d1 = "sigma", d2 = NULL, sign = 1)
  )
}

#' @export
tt_regtab_ancillary_rows.survreg <- function(fit, info, level = 0.95, vce = "stata", ...) {
  # tobit's variance row is a trailing row (below).
  if (identical(info$stata_cmd, "tobit")) return(NULL)
  V <- tt_vcov(fit, vce)
  anc <- grep("^Log\\(scale(\\[[^]]*\\])?\\)$", rownames(V), value = TRUE)
  if (!length(anc)) return(NULL)
  if (identical(info$stata_cmd, "intreg")) {
    # intreg's e(b) holds lnsigma:_cons, whose colname collides with the
    # intercept's in regtab's colname layout, so only the sigma diparm shows
    # (exp-transformed, no p-value), before the intercept (probes B04b, G11,
    # G13; nointercept keeps it).
    w <- .rt_wald_named(stats::setNames(log(fit$scale[1]), anc[1]), V, anc[1], Inf, level)
    # Kept by nointercept, as Stata's rule does not know `sigma` (H5).
    return(.rt_anc_row("sigma", "sigma", w, transform = "exp", role = "auxiliary"))
  }
  nm <- .rt_sr_anc_names(fit$dist)
  # nointercept drops the Weibull's ln_p, p and 1/p with the intercept, and
  # keeps lnsigma/sigma and lngamma/gamma, as Stata's rule does (its list of
  # ancillary names, `regtab.ado:1619-1630`, has only the first three;
  # fixtures lognormal_default, loglogistic_time_default): a structural
  # role per distribution (H5), not a label match.
  role <- if (identical(nm$ln, "ln_p")) "ancillary" else "auxiliary"
  b <- stats::setNames(log(fit$scale), anc)
  out <- list()
  for (k in seq_along(anc)) {
    w <- .rt_wald_named(b, V, anc[k], Inf, level)
    if (nm$sign < 0) {
      lo <- -w$conf.high
      w$conf.high <- -w$conf.low
      w$conf.low <- lo
      w$estimate <- -w$estimate
      w$statistic <- -w$statistic
    }
    sfx <- if (length(anc) > 1L) paste0(" (", names(fit$scale)[k] %||% k, ")") else ""
    out[[length(out) + 1L]] <- .rt_anc_row(paste0(nm$ln, sfx), paste0(nm$ln, sfx), w, role = role)
    out[[length(out) + 1L]] <- .rt_anc_row(paste0(nm$d1, sfx), paste0(nm$d1, sfx), w, transform = "exp", role = role)
    if (!is.null(nm$d2)) {
      out[[length(out) + 1L]] <- .rt_anc_row(paste0(nm$d2, sfx), paste0(nm$d2, sfx), w, transform = "invexp",
                                             role = role)
    }
  }
  do.call(rbind, out)
}

# survreg: vcov(), with Stata's M/(M - 1) when it is a robust sandwich.
#' @export
tt_vcov.survreg <- function(fit, vce = "stata", cluster = NULL, ...) {
  .rt_check_survival_convergence(fit)
  V <- stats::vcov(fit)
  if (identical(vce, "model")) return(V)
  .rt_robust_factor(fit, V)
}

# Stata tobit shows the residual variance var(e.<depvar>) after the
# intercept, as regtab places every "var(" row with the random effects
# (`regtab.ado:1640-1648`): estimate sigma^2, Wald interval on ln(sigma^2)
# (so exp(2 (ln sigma +/- z se))), no p-value, not exponentiated, a rule
# above it in the workbook; noreeffects drops it and relabel rewrites
# "var(e." as "Variance (Residual" (`:1781`), which reads
# "Variance (Residualyc)" (probes B04, G12, G14, G15).
#' @export
tt_regtab_trailing_rows.survreg <- function(fit, info, o, ...) {
  if (!identical(info$stata_cmd, "tobit") || isTRUE(o$noreeffects)) return(NULL)
  V <- tt_vcov(fit, o$vce)
  if (!"Log(scale)" %in% rownames(V)) return(NULL)
  w <- .rt_wald_named(c("Log(scale)" = log(fit$scale[1])), V, "Log(scale)", Inf, o$level)
  z <- stats::qnorm((1 + o$level) / 2)
  dv <- tryCatch(all.vars(stats::formula(fit)[[2L]])[1], error = function(e) NA_character_)
  key <- paste0("var(e.", dv, ")")
  label <- if (isTRUE(o$relabel)) sub("var(e.", "Variance (Residual", key, fixed = TRUE) else key
  .rt_row(key, key, "re", label, "est", estimate = fit$scale[1]^2,
          conf.low = exp(2 * (w$estimate - z * w$std.error)), conf.high = exp(2 * (w$estimate + z * w$std.error)),
          ancillary = TRUE)
}

# The frame a survreg's counts and log-likelihood are read from: the
# fit-time frame (stored or anchored), else the data as they are now when
# .rt_reordered_ok() matched them to the fit's rows (re-sorted with their
# row names reset; the sums below do not depend on the order), else an
# error. Never model.frame(<survreg>) of the current data, which leaves out
# a `cluster =` argument and so keeps the rows whose cluster is missing
# (backlog, found with the coxph(y = FALSE) Events item: N, Events and the
# log-likelihood of a re-sorted survreg(cluster = inst) on data with
# missing clusters counted those rows).
.rt_survreg_stats_frame <- function(fit) {
  # Subclasses regtab does not anchor (AER's tobit): their own frame.
  if (!class(fit)[1] %in% .rt_anchor_classes) return(tryCatch(stats::model.frame(fit), error = function(e) NULL))
  fa <- .rt_anchor(fit)
  if (is.data.frame(fa$model)) return(fa$model)
  if (.rt_reordered_ok(fa)) {
    mf <- tryCatch(.rt_eval_frame(fa), error = function(e) NULL)
    if (is.data.frame(mf)) return(mf)
  }
  .rt_require_frame(fit, "log-likelihood")$model
}

#' @export
tt_model_stats.survreg <- function(fit, info, ...) {
  s <- tt_model_stats.default(fit, NULL)
  ll <- stats::logLik(fit)
  s$rank <- attr(ll, "df")
  s$ll <- as.numeric(ll)
  mf <- .rt_survreg_stats_frame(fit)
  y <- if (!is.null(mf)) stats::model.response(mf) else NULL
  w <- if (!is.null(mf)) stats::model.weights(mf) else NULL
  if (!is.null(y)) {
    if (is.null(w)) w <- rep(1, nrow(y))
    # survreg's weights are case (frequency) weights: its variance is
    # Stata's streg [fweight] variance, whose e(N) is the weight sum
    # (probe B13: 17,944 on the cohort's fw; Phase 5 review P2-1).
    s$N <- sum(w)
    # streg's likelihood is that of the log time: add back the Jacobian
    # of the log transform that survreg's includes, for failures.
    # stats(events): streg's e(N_fail), the failures weighted by the
    # [fweight]s (probe C3); intreg and tobit (a gaussian survreg) store
    # none, nor does an interval-censored response.
    if (identical(attr(y, "type"), "right") && !identical(fit$dist, "gaussian")) {
      s$events <- sum(w[y[, "status"] == 1])
    }
    trans <- survival::survreg.distributions[[fit$dist]]$trans
    if (!is.null(trans) && identical(attr(y, "type"), "right")) {
      ev <- y[, "status"] == 1
      s$ll <- s$ll + sum(w[ev] * log(y[ev, "time"]))
    }
  }
  # intreg and tobit report Observations; streg counts subjects.
  s$N_sub <- if (identical(info$stata_cmd, "streg") || is.null(info)) s$N else NA_real_
  # A robust weighted fit is streg after stset [pweight] (C4 review O1):
  # e(N) counts the records (probe X5: 2,000 cohort rows), e(N_sub) is the
  # sum of the weights (3,955.10), shown with a note as for coxph, and BIC
  # uses e(N).
  if (!is.null(y) && !is.null(fit$naive.var) && !is.null(stats::model.weights(mf))) {
    s$N <- as.numeric(sum(w > 0))
    if (!is.na(s$N_sub)) {
      s$N_sub <- sum(w)
      s$nsub_note <- "weighted"
    }
    if (!is.null(s$events)) s$events_note <- "weighted"
  }
  s$aic <- -2 * s$ll + 2 * s$rank
  s$bic <- -2 * s$ll + s$rank * log(s$N)
  s
}

# ---------------------------------------------------------------------------
# Fine-Gray via coxph on finegray() data

# The fit-time frame of a Fine-Gray fit (verified, review P0-1) and the
# start time of each record (the first Surv() column).
.rt_fg_parts <- function(fit) {
  fit <- .rt_anchor(fit)
  if (!is.data.frame(fit$model)) return(NULL)
  mf <- fit$model
  y <- stats::model.response(mf)
  if (!inherits(y, "Surv") || ncol(y) != 3L) return(NULL)
  list(mf = mf, start = y[, 1L])
}

# Subject of each record, in the fit's row order: the fit's cluster()/id if
# it has one, else the subjects finegray() wrote (.rt_fg_infer_subject());
# NULL when they cannot be identified. Attribute "first": the row of each
# subject's first record, subjects in finegray()'s order (the order of the
# data finegray() was given).
.rt_fg_subject <- function(fit, parts = .rt_fg_parts(fit)) {
  if (is.null(parts)) return(NULL)
  mf <- parts$mf
  ord <- .rt_fg_order(mf)
  for (col in c("(cluster)", "(id)")) {
    if (!is.null(mf[[col]])) {
      sid <- mf[[col]]
      attr(sid, "first") <- ord[!duplicated(sid[ord])]
      return(sid)
    }
  }
  # The layout is read only from data that are finegray() output (its
  # "event" mark): a fit declared Fine-Gray on other data (regtab(finegray =
  # TRUE)) needs its id (review T2B-02).
  if (!.rt_fg_marked(fit)) return(NULL)
  .rt_fg_infer_subject(mf, ord)
}

# finegray()'s own row order: its output's row names are 1, 2, ... in the
# order it wrote the records, and a data frame re-sorted with `[` keeps
# them, so the fit's rows are put back in that order by their row names
# (external review F04: the subjects used to be read from the current row
# order, and a re-sorted data frame changed the variance and Subjects).
.rt_fg_order <- function(mf) {
  rn <- suppressWarnings(as.numeric(rownames(mf)))
  if (length(rn) && !anyNA(rn) && !anyDuplicated(rn)) order(rn) else seq_len(nrow(mf))
}

# Subjects of finegray() output without an id, read only where the layout
# is unambiguous: right-censored finegray() input (Milestone H review
# T2B-01/T2B-02). finegray() then writes each subject's records together:
# the first with weight exactly 1 starting at the common time origin, then,
# for a subject with a competing event, records over later intervals (each
# starting at or after the previous one's stop) with the subject's
# covariates and the censoring weights, which are below 1. Under delayed
# entry or counting-process input (finegray(Surv(start, stop, event),
# id =)) a later record can also carry weight 1 (a truncation weight of 1,
# or the next piece of a split subject), so a weight-1 record that starts
# after the origin, a continuation record with weight 1 or more, or a
# record that does not continue the previous one (other covariates, an
# earlier start) makes the subjects unidentifiable: NULL, and the variance
# is an error naming the fix (the id on the fit).
.rt_fg_infer_subject <- function(mf, ord) {
  y <- stats::model.response(mf)
  w <- stats::model.weights(mf)
  n <- nrow(mf)
  if (is.null(w) || n < 1L) return(NULL)
  cov <- names(mf)[-1L]
  cov <- cov[!grepl("^\\(", cov)]
  # One text key per record from its covariate values (a matrix column,
  # e.g. a spline basis, row by row).
  col_key <- function(x) {
    x <- format(unclass(x), digits = 17)
    if (is.matrix(x)) apply(x, 1L, paste, collapse = "\r") else x
  }
  key <- if (length(cov)) {
    do.call(paste, c(lapply(mf[ord, cov, drop = FALSE], col_key), sep = "\r"))
  } else rep("", n)
  st <- y[ord, 1L]
  sp <- y[ord, 2L]
  wo <- w[ord]
  first <- abs(wo - 1) <= 1e-12
  origin <- min(y[, 1L])
  if (!first[1L] || any(first & st > origin)) return(NULL)
  chain <- c(FALSE, st[-1L] >= sp[-n] & key[-1L] == key[-n])
  if (any(!first & (!chain | wo >= 1 - 1e-12))) return(NULL)
  cont <- !first
  sid <- integer(n)
  sid[ord] <- cumsum(!cont)
  attr(sid, "first") <- ord[!cont]
  sid
}

# The fit's own grouping (cluster() or `cluster =`, else coxph's `id`) as
# written in its call: list(kind, name), name NULL for an expression.
.rt_own_grouping <- function(fit) {
  cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  if (is.null(cl)) return(NULL)
  if (!is.null(cl$cluster)) {
    return(list(kind = "cluster", name = if (is.name(cl$cluster)) as.character(cl$cluster) else NULL))
  }
  f <- cl$formula
  if (is.null(f) && length(cl) >= 2L) f <- cl[[2L]]
  if (is.name(f)) f <- tryCatch(eval(f, environment(stats::formula(fit))), error = function(e) NULL)
  txt <- paste(deparse(f), collapse = " ")
  if (grepl("cluster(", txt, fixed = TRUE)) {
    m <- regmatches(txt, regexec("cluster\\(([A-Za-z._][A-Za-z0-9._]*)\\)", txt))[[1L]]
    return(list(kind = "cluster", name = if (length(m) == 2L) m[2L] else NULL))
  }
  if (inherits(fit, "coxph") && !is.null(cl$id)) {
    return(list(kind = "id", name = if (is.name(cl$id)) as.character(cl$id) else NULL))
  }
  NULL
}

# Cluster of each observation under the fit's own grouping (NULL when it
# has none), from the fit-time frame (review P0-1).
.rt_own_clusters <- function(fit) {
  g <- .rt_own_grouping(fit)
  if (is.null(g)) return(NULL)
  fit <- .rt_require_frame(fit, "robust variance")
  mf <- stats::model.frame(fit)
  col <- if (identical(g$kind, "cluster")) "(cluster)" else "(id)"
  if (!is.null(mf[[col]])) return(mf[[col]])
  # survreg's re-evaluated frame drops cluster(): read the variable.
  if (!is.null(g$name)) return(.rt_cluster_column(fit, g$name, nrow(mf)))
  cli::cli_abort(c("The clusters of this {.cls {class(fit)[1]}} fit cannot be recovered.",
                   "i" = "Refit it with {.code model = TRUE}."), call = NULL)
}

# Distinct clusters of the fit's own grouping, from current data that hold
# the fit's observations re-ordered (NULL otherwise). The check is
# .rt_reordered_ok()'s (responses, linear predictors, weights and the
# log-likelihood), which also covers fits with y = FALSE: a re-sorted
# survreg(cluster =, y = FALSE) used to be refused here.
.rt_cluster_count_reordered <- function(fit, g) {
  mf <- tryCatch(.rt_eval_frame(fit), error = function(e) NULL)
  if (!is.data.frame(mf) || !.rt_reordered_ok(fit)) return(NULL)
  cl <- mf[["(cluster)"]]
  if (is.null(cl) && !is.null(g$name)) {
    src <- .rt_call_data(fit)
    idx <- if (is.data.frame(src)) match(rownames(mf), rownames(src)) else NA
    if (!anyNA(idx) && g$name %in% names(src)) cl <- src[[g$name]][idx]
  }
  if (is.null(cl) || anyNA(cl)) return(NULL)
  length(unique(cl))
}

# Number of clusters of a robust (sandwich) variance: the fit's cluster()
# (or, for coxph, its id), else one per observation. Read from the fit
# (coxph's n.id, n) wherever possible, never from data changed after the
# fit (review P0-1).
.rt_robust_groups <- function(fit) {
  g <- .rt_own_grouping(fit)
  if (!is.null(g) && identical(g$kind, "id") && !is.null(fit$n.id)) return(fit$n.id)
  if (!is.null(g) && .rt_has_tt(fit)) {
    # A tt() fit's clusters from its tt-free frame (verified; a count of
    # distinct clusters does not depend on the rows' order; review T2B-04).
    mf <- .rt_tt_frame(fit)
    cl <- if (is.null(mf)) NULL else mf[[intersect(c("(cluster)", "(id)", grep("^cluster\\(", names(mf), value = TRUE)),
                                                   names(mf))[1] %||% ""]]
    if (is.null(cl)) .rt_require_frame(fit, "robust variance")
    return(length(unique(cl)))
  }
  if (!is.null(g)) {
    fa <- .rt_anchor(fit)
    if (is.data.frame(fa$model)) return(length(unique(.rt_own_clusters(fa))))
    # The data changed since the fit: the robust variance itself is stored,
    # and G, a count of distinct clusters, does not depend on the row order,
    # so it is read from the current data when they hold the same
    # observations in another order (the same responses, as a multiset).
    G <- .rt_cluster_count_reordered(fit, g)
    if (!is.null(G)) return(G)
    return(length(unique(.rt_own_clusters(fit))))
  }
  n <- if (inherits(fit, "coxph")) fit$n else length(fit$linear.predictors)
  if (is.null(n) || !length(n)) NA_real_ else n
}

# Stata's robust variance carries the small-sample factor M/(M - 1), M the
# number of clusters (every observation when there is no cluster()):
# `_robust` applies it for stcox and streg with vce(robust) or
# vce(cluster), while survival's sandwich (a fit with `cluster()`,
# `robust = TRUE`, or an id with overlapping records) has none (Phase 5
# review P0-9; probes B01-B03: Stata/R SE ratios sqrt(6/5) for 6 region
# clusters, sqrt(400/399) for vce(robust) on 400 subjects).
.rt_robust_factor <- function(fit, V) {
  if (is.null(fit$naive.var)) return(V)
  G <- .rt_robust_groups(fit)
  if (!is.finite(G) || G < 2) return(V)
  V * G / (G - 1)
}

# Milestone 5w (task 5.12; probes V20-V23): stcox's vce(robust) (implied by
# `stset ... [pw=]`) is the dfbeta sandwich grouped by the stset id (one
# row per subject: by observation) times G/(G - 1); vce(cluster c) groups by
# c. `vce = "robust"` groups by the fit's cluster()/id when it has one, else
# by row; `vce = "cluster"` by `cluster`, or the fit's own clusters when
# `cluster` is empty. Fine-Gray fits keep stcrreg's variance only.
#' @export
tt_vce_types.coxph <- function(fit) {
  if (identical(class(fit)[1], "coxph") && !.mi_is_finegray(fit)) {
    c("stata", "model", "robust", "cluster")
  } else c("stata", "model")
}

# A robust fit's own variance is survival's sandwich grouped exactly as
# vce = "robust" (and "cluster" without `cluster`) groups it, computed at
# fit time: use it (times G/(G - 1)), so the data need not be read again
# (review P0-1). Otherwise the grouped dfbeta, from the fit-time frame.
.rt_cox_sandwich <- function(fit, vce, cluster = NULL) {
  if (is.null(cluster) && !is.null(fit$naive.var) &&
      (identical(vce, "robust") || !is.null(.rt_own_grouping(fit)))) {
    V <- .rt_robust_factor(fit, stats::vcov(fit))
    nm <- names(stats::coef(fit))
    dimnames(V) <- list(nm, nm)
    return(V)
  }
  fit <- .rt_require_frame(fit, "robust variance")
  mf <- stats::model.frame(fit)
  cl <- if (!is.null(cluster)) {
    .rt_resolve_cluster(fit, cluster, nrow(mf))
  } else if (!is.null(own <- .rt_own_clusters(fit))) {
    own
  } else if (identical(vce, "cluster")) {
    cli::cli_abort("{.code vce = \"cluster\"} needs {.arg cluster} for a {.cls coxph} fit without {.code cluster()} or {.arg id}.",
                   call = NULL)
  } else seq_len(nrow(mf))
  # na.exclude would pad the residuals (and expect a padded `collapse`):
  # the fitted rows only (review P1-1).
  if (inherits(fit$na.action, "exclude")) class(fit$na.action) <- "omit"
  D <- as.matrix(stats::residuals(fit, type = "dfbeta", collapse = cl, weighted = TRUE))
  G <- nrow(D)
  if (G < 2L) cli::cli_abort("{.arg cluster} has a single cluster; a cluster-robust variance needs at least two.", call = NULL)
  V <- crossprod(D) * G / (G - 1)
  nm <- names(stats::coef(fit))
  dimnames(V) <- list(nm, nm)
  V
}

#' @export
tt_vcov.coxph <- function(fit, vce = "stata", cluster = NULL, ...) {
  .rt_check_survival_convergence(fit)
  if (vce %in% c("robust", "cluster")) return(.rt_cox_sandwich(fit, vce, cluster))
  V <- stats::vcov(fit)
  if (identical(vce, "model")) return(V)
  if (!.mi_is_finegray(fit)) return(.rt_robust_factor(fit, V))
  fit <- .rt_require_frame(fit, "Fine-Gray variance")
  sid <- .rt_fg_subject(fit)
  # Never the record-level variance in its place (Milestone H decision
  # H-D6; external review F28).
  if (is.null(sid)) .rt_fg_abort_subjects()
  D <- tryCatch(stats::residuals(fit, type = "dfbeta", collapse = as.vector(sid), weighted = TRUE),
                error = function(e) e)
  if (inherits(D, "error")) {
    cli::cli_abort("The subject-clustered variance of this Fine-Gray fit could not be computed.", parent = D, call = NULL)
  }
  D <- as.matrix(D)
  G <- nrow(D)
  Vn <- crossprod(D) * G / (G - 1)
  dimnames(Vn) <- dimnames(V)
  Vn
}

# The subjects of a Fine-Gray fit cannot be identified.
.rt_fg_abort_subjects <- function() {
  cli::cli_abort(c(
    "The subjects of this Fine-Gray fit cannot be identified, so its variance (Stata {.code stcrreg}'s, clustered by subject) cannot be formed.",
    "i" = "Without an id on the fit, regtab reads the subjects only from {.fn survival::finegray} output of right-censored data, in its row order (kept in the row names when the data are re-sorted with {.code [}); here that layout is not certain (delayed entry, counting-process input with {.code finegray(id =)}, case weights, data re-sorted with their row names reset, or data without {.fn finegray}'s mark).",
    "i" = "Keep the subject id in the {.fn finegray} formula ({.code finegray(Surv(time, status) ~ x + id, ...)}, and {.code id = id} for (start, stop] data) and fit with {.code coxph(..., cluster = id)} or {.code coxph(..., id = id)}."
  ), call = NULL)
}

# Labels finegray() dropped: look for the data frame finegray() was given
# (in the model's environment and its parents up to the global
# environment), accepted only when its rows equal the first record of every
# subject in the expanded data (in finegray()'s order, which is that data
# frame's), column by column.
.rt_fg_restore_labels <- function(fit, mf) {
  need <- names(mf)[vapply(names(mf), function(v) {
    !grepl("^\\(", v) && is.null(attr(mf[[v]], "label", exact = TRUE))
  }, TRUE)]
  need <- setdiff(need, names(mf)[1L])
  if (!length(need)) return(mf)
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  cl <- fit$call
  if (is.null(env) || is.null(cl$data)) return(mf)
  sid <- .rt_fg_subject(fit, list(mf = mf, start = stats::model.response(mf)[, 1L]))
  if (is.null(sid)) return(mf)
  first <- mf[attr(sid, "first"), need, drop = FALSE]
  data_name <- if (is.name(cl$data)) as.character(cl$data) else ""
  hits <- list()
  e <- env
  repeat {
    for (nm in setdiff(ls(e), data_name)) {
      x <- tryCatch(get(nm, envir = e), error = function(err) NULL)
      if (!is.data.frame(x) || nrow(x) != nrow(first) || !all(need %in% names(x))) next
      if (!any(vapply(need, function(v) !is.null(attr(x[[v]], "label", exact = TRUE)), TRUE))) next
      same <- all(vapply(need, function(v) {
        isTRUE(all.equal(as.vector(unclass(x[[v]])), as.vector(unclass(first[[v]])),
                         check.attributes = FALSE))
      }, TRUE))
      if (same) hits[[length(hits) + 1L]] <- x
    }
    if (identical(e, globalenv()) || identical(e, emptyenv())) break
    e <- parent.env(e)
    if (isNamespace(e) || identical(e, baseenv())) break
  }
  if (length(hits) < 1L) return(mf)
  src <- hits[[1L]]
  for (v in need) {
    lab <- attr(src[[v]], "label", exact = TRUE)
    if (!is.null(lab)) attr(mf[[v]], "label") <- lab
  }
  mf
}

#' @export
tt_regtab_rows.coxph <- function(fit, info, ...) {
  if (.mi_is_finegray(fit)) {
    parts <- .rt_fg_parts(fit)
    if (!is.null(parts)) {
      # model.frame(<coxph>) returns fit$model when present, so the rows
      # see the restored labels.
      fit$model <- .rt_fg_restore_labels(fit, parts$mf)
    }
  }
  tt_regtab_rows.default(fit, info, ...)
}

#' @export
tt_model_stats.coxph <- function(fit, info, ...) {
  s <- tt_model_stats.default(fit, info)
  # Weighted stset: Stata's e(N_sub) is the weighted number of subjects
  # (stset [pw=iptw], id(id): 30,000.07 on the 15,000-subject cohort, probe
  # W04), each subject counted once with its weight; e(N) stays the rows.
  # regtab keeps it (review policy (c)) with a note that it is a sum of
  # weights; Stata refuses weights that vary within a subject (probe E04),
  # so then the cell is blank, with a note (review P2-7).
  w <- .rt_cox_weights(fit)
  tw <- if (!is.null(w) && .rt_has_tt(fit)) .rt_tt_weights(fit)
  if (!is.null(w) && .rt_has_tt(fit) && is.null(tw)) {
    # A tt() fit's weights are those of its expanded records: without the
    # observations' own weights (.rt_tt_weights()) the cell is blank, with
    # a note, never their sum.
    s$N_sub <- NA_real_
    s$nsub_note <- "unverified"
  } else if (!is.null(w)) {
    # The subjects: coxph's `id`, else its cluster() / `cluster =`, which is
    # how a multi-record weighted fit usually names them (a cluster with
    # several rows and varying weights used to add every row's weight:
    # 4,402 "subjects" for 1,000 people).
    idv <- NULL
    if (!is.null(tw)) {
      # tt(): the observations' weights and grouping, from its tt-free frame
    # (matched to the fit's records: y = TRUE, or equal weights).
      w <- tw$w
      gcol <- intersect(c("(id)", "(cluster)", grep("^cluster\\(", names(tw$mf), value = TRUE)), names(tw$mf))
      if (length(gcol)) idv <- tw$mf[[gcol[1L]]]
    } else if (!is.null(fit$call$id)) {
      if (!identical(fit$n.id, fit$n)) {
        idv <- stats::model.frame(.rt_require_frame(fit, "weighted subject count"))[["(id)"]]
      }
    } else if (identical(.rt_own_grouping(fit)$kind, "cluster")) {
      idv <- .rt_own_clusters(fit)
    }
    if (!is.null(idv) && anyDuplicated(idv)) {
      # Several rows per subject: each subject's weight once (one row per
      # subject needs no id: the sum of the weights).
      first <- !duplicated(idv)
      varies <- any(tapply(w, idv, function(x) length(unique(x)) > 1L))
      if (varies) {
        s$N_sub <- NA_real_
        s$nsub_note <- "varies"
      } else {
        s$N_sub <- sum(w[first])
        s$nsub_note <- "weighted"
      }
    } else {
      s$N_sub <- sum(w)
      s$nsub_note <- "weighted"
    }
  }
  s$events <- .rt_cox_events(fit)
  if (!is.null(.rt_cox_weights(fit))) s$events_note <- if (is.na(s$events)) "response" else "weighted"
  if (.mi_is_finegray(fit)) {
    # stcrreg counts subjects, not the expanded records (e(N) = e(N_sub)):
    # blank, never the record count, when they cannot be identified (only
    # reachable with vce = "model", whose variance needs no subjects).
    sid <- .rt_fg_subject(fit)
    s$N <- if (is.null(sid)) NA_real_ else length(unique(sid))
    s$N_sub <- s$N
    # The weighted-stset note does not apply: stcrreg's subjects are counted.
    s$nsub_note <- NULL
    s$bic <- NA_real_
    if (!is.na(s$ll) && !is.na(s$rank)) {
      s$aic <- -2 * s$ll + 2 * s$rank
      if (!is.na(s$N)) s$bic <- -2 * s$ll + s$rank * log(s$N)
    }
  }
  s
}

# clogit (task 5.15): Stata's clogit stores e(N), the observations, and
# neither e(N_sub) nor e(N_fail) nor e(N_group), so the n token reads
# Observations and events/groups add no row (probe, tabtools 2.1.13).
#' @export
tt_model_stats.clogit <- function(fit, info, ...) {
  s <- tt_model_stats.default(fit, info)
  s$N_sub <- NA_real_
  s$events <- NA_real_
  s
}

# stats(events) (task 5.17): Stata's e(N_fail). stcox counts the failure
# records, weighted by the stset weights (stset [pw=iptw], id(id): the sum
# of the failures' weights, 2071.75 on 3,000 cohort rows, probe C1; C2 the
# plain count); stcrreg counts the failures of interest (C4), which on
# survival::finegray() data are the records whose status is 1 (one per
# failing subject), unweighted. NA (a blank cell, with a note: events_note
# "response") when the fit is weighted and its statuses cannot be paired
# with its weights.
.rt_cox_events <- function(fit) {
  # A weighted tt() fit with y = TRUE: its own expanded records, where each
  # failure is one record with its weight, give the sum without the data.
  yw <- if (.rt_has_tt(fit) && !is.null(.rt_cox_weights(fit)) && !is.null(fit$y) &&
            NROW(fit$y) == length(fit$weights)) list(y = as.matrix(unclass(fit$y)), w = fit$weights) else .rt_cox_yw(fit)
  if (is.null(yw)) {
    return(if (is.null(.rt_cox_weights(fit))) as.numeric(fit$nevent %||% NA_real_) else NA_real_)
  }
  ev <- yw$y[, ncol(yw$y)] == 1
  if (is.null(yw$w)) as.numeric(sum(ev)) else sum(yw$w[ev])
}

# A coxph fit's response and case weights (NULL when unweighted or
# Fine-Gray), row by row in the same order, for the order-free sums over
# its failures (the weighted events, the pseudo-log-likelihood): the fit's
# own copies (y = TRUE), else the response of the fit-time frame regtab()
# anchored (review T2B-05), else, when the data were re-sorted with their
# row names reset, the data as they are now, whose rows .rt_reordered_ok()
# has matched to the fit's (response, linear predictor, weight, martingale
# residual and number of failures, which tie the statuses to the rows where
# y = FALSE keeps none). A weighted tt() fit: its tt-free frame, when its
# (event time, status, weight) records match the fit's (.rt_tt_weights(),
# y = TRUE only). NULL when none of these is available.
.rt_cox_yw <- function(fit) {
  w <- .rt_cox_weights(fit)
  if (!is.null(w) && .rt_has_tt(fit)) {
    # tt(): the observations (not the expanded records), whose weights
    # normalise Stata's pseudo-log-likelihood, and only when their failures'
    # weights were matched to the fit's records (y = TRUE).
    tw <- .rt_tt_weights(fit)
    if (is.null(tw) || !isTRUE(tw$paired)) return(NULL)
    return(list(y = as.matrix(unclass(stats::model.response(tw$mf))), w = tw$w))
  }
  y <- fit$y %||% (if (is.data.frame(fit$model)) stats::model.response(fit$model))
  if (!is.null(y)) {
    if (!is.null(w) && NROW(y) != length(w)) return(NULL)
    return(list(y = as.matrix(unclass(y)), w = w))
  }
  if (!isTRUE(fit$tt_stale) || .rt_has_tt(fit) || !.rt_reordered_ok(fit)) return(NULL)
  mf <- tryCatch(.rt_eval_frame(fit), error = function(e) NULL)
  if (!is.data.frame(mf)) return(NULL)
  y <- as.matrix(unclass(stats::model.response(mf)))
  if (!is.null(w)) {
    w <- stats::model.weights(mf)
    if (is.null(w) || length(w) != nrow(y)) return(NULL)
    w <- as.numeric(w)
  }
  list(y = y, w = w)
}

# ---------------------------------------------------------------------------
# cmprsk::crr (stcrreg)

#' @export
tt_vcov.crr <- function(fit, vce = "stata", cluster = NULL, ...) {
  V <- fit$var
  nm <- names(fit$coef)
  dimnames(V) <- list(nm, nm)
  if (identical(vce, "model")) return(V)
  n <- fit$n
  if (is.null(n) || n < 2) return(V)
  V * n / (n - 1)
}

# crr keeps no formula or data: one row per covariate column, labelled by
# its name.
#' @export
tt_regtab_rows.crr <- function(fit, info, level = 0.95, vce = "stata", ...) {
  b <- fit$coef
  V <- tt_vcov(fit, vce)
  w <- .rt_wald_named(b, V, names(b), Inf, level)
  res <- do.call(rbind, lapply(seq_along(b), function(k) {
    .rt_row(names(b)[k], names(b)[k], "var", names(b)[k], if (is.na(b[k])) "omit" else "est",
            term = names(b)[k], estimate = w$estimate[k], conf.low = w$conf.low[k],
            conf.high = w$conf.high[k], p.value = w$p.value[k], sub = k)
  }))
  if (isTRUE(info$exponentiate)) {
    for (col in c("estimate", "conf.low", "conf.high")) res[[col]] <- exp(res[[col]])
  }
  attr(res, "positional") <- character()
  res
}

#' @export
tt_model_stats.crr <- function(fit, info, ...) {
  s <- tt_model_stats.default(fit, info)
  s$N <- fit$n
  s$N_sub <- fit$n
  s$ll <- fit$loglik
  s$rank <- sum(!is.na(fit$coef))
  s$aic <- -2 * s$ll + 2 * s$rank
  s$bic <- -2 * s$ll + s$rank * log(s$N)
  s
}
