# Mixed models for regtab (plan task 5.5): lme4::lmer (Stata `mixed`),
# lme4::glmer (`melogit`, `mepoisson`, `menbreg`, `meprobit`, `mecloglog`,
# `meglm`) and, where cheap, glmmTMB. nlme::lme (Stata `mixed`, task 5.19)
# reuses the structures, rows and deviance below from
# R/regtab_models_nlme.R.
#
# Fixed effects go through the Phase 4 path (tt_regtab_rows() with the
# tt_coef()/tt_vcov() methods below). Random-effects parameters become
# trailing rows (tt_regtab_trailing_rows()), after every fixed-effect row
# (`regtab.ado:1638-1644`), outermost grouping level first, then the
# residual variance. Stata facts, from the goldens (R21, R22, R25f) and
# probes of Stata 17 + tabtools 2.1.9, rechecked against 2.1.11:
# - Raw row names: `mixed` with one grouping level shows `var(_cons)`,
#   `var(age)`, `cov(age,_cons)`, `var(e)`; with several levels each name
#   carries the terminal grouping variable, `var(_cons[location])`,
#   `cov(age,_cons[location])` (`regtab.ado:1461-1541`). The me* commands
#   always use brackets on every component: `var(_cons[location])`,
#   `var(x[clinic])`, `cov(x[clinic],_cons[clinic])`. Within a level,
#   slopes come before `_cons` (Stata lists the constant last), variances
#   before covariances.
# - `relabel` (`regtab.ado:1673-1803`): `Variance: <group label>
#   (Intercept)`, `Variance: <group label> (<slope label>)`, `Covariance:
#   <group label> (<label a>, <label b>)`, `Residual Variance`, for me*
#   models as for `mixed` since tabtools 2.1.11 (2.1.9 left me* slope
#   variances raw and kept a bracket in the first covariance component).
#   Group labels shared by two levels fall back to the variable names
#   (`regtab.ado:1208-1229`).
# - melogit/mecloglog (and mestreg): the random-intercept variance becomes
#   the median odds (hazard) ratio exp(sqrt(2 var) invnormal(0.75)), CI
#   bounds transformed the same way, labelled `Median Odds Ratio (<group
#   label>)` with or without `relabel` (`regtab.ado:1805-1816`, `:2102-2106`,
#   `:2163-2169`).
# - Estimates and CI bounds use the fixed-effect digits; variance rows have
#   no p-value, covariance rows do (`regtab.ado:2112-2119`, `:2176-2186`).
# - Variance CIs are Wald intervals on the log-SD scale, back-transformed
#   (equivalently log-variance): exp(2 (ln sd +/- z se)). Covariance CIs are
#   Wald intervals on the covariance scale, its standard error by the delta
#   method from (ln sd_a, ln sd_b, atanh rho), Stata's own parameterization
#   (`mixed` e(b): lns1_1_1, lns1_1_2, atr1_1_1_2, lnsig_e).
# - Standard errors: `mixed` reports the fixed effects' GLS variance
#   (X'V^-1 X)^-1, which is vcov() of an lmer fit, and for the variance
#   parameters the block of the inverse observed information, i.e. the
#   inverse Hessian of the log-likelihood profiled over the fixed effects
#   (REML criterion for REML fits). The me* commands report the full
#   inverse observed information over (fixed effects, variance
#   parameters). Both are computed here by Richardson-extrapolated central
#   differences of the model's own deviance function.
# - ICC = sum of every `var(_cons)` / (that sum + residual variance), the
#   residual being var(e) for `mixed` and the latent level-1 variance for
#   binary models (logit pi^2/3, probit 1, cloglog pi^2/6); undefined for
#   count families, with a note (`regtab.ado:951-1127`). Computed even
#   under `noreeffects`.
# - Groups: the innermost level's number of groups (the last element of
#   e(N_g), as Stata's regtab shows for multi-level models).

# ---------------------------------------------------------------------------
# Adapter flags, coefficients and variance

# The optimizer's return code is separate from lme4's post-fit checks.
# lme4::convergence documents that scaled-gradient and conditioning
# warnings can be false positives; isSingular() documents valid maxima
# on the random-effects boundary. Neither advisory alone is a refusal.
# checkHess() codes -3/-4/-6 instead identify a degenerate, singular or
# unevaluable Hessian. glmmTMBControl() and its troubleshooting vignette
# identify a nonzero fit$convergence and sdr$pdHess == FALSE as failures.
# The shared public-entry diagnostic gate calls this before inference,
# including before a user-supplied variance can bypass the fitted one.
.rt_check_mixed_convergence <- function(fit, i = 1L) {
  beta_map <- NULL
  if (inherits(fit, "merMod")) {
    opt <- fit@optinfo$conv$opt
    if (length(opt) && any(!is.na(opt) & opt != 0)) {
      detail <- paste(fit@optinfo$message %||% "optimizer failure", collapse = "; ")
      cli::cli_abort(c(
        "Model {i} ({.cls {class(fit)[1]}}) did not converge (optimizer code {paste(opt, collapse = ', ')}: {detail}).",
        "i" = "Refit with suitable optimizer controls and check the convergence diagnostics before reporting inference."
      ), class = "tabtools_error_model_convergence", call = NULL)
    }
    code <- fit@optinfo$conv$lme4$code
    if (any(code %in% c(-3L, -4L, -6L))) {
      detail <- paste(fit@optinfo$conv$lme4$messages, collapse = "; ")
      cli::cli_abort(c(
        "Model {i} ({.cls {class(fit)[1]}}) has an invalid Hessian ({detail}); its inference cannot be reported reliably.",
        "i" = "Check the model specification, scaling and identifiability, then refit and verify the Hessian."
      ), class = "tabtools_error_model_hessian", call = NULL)
    }
    b <- lme4::fixef(fit)
    V <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) {
      cli::cli_abort("Model {i} ({.cls {class(fit)[1]}}) has no usable fixed-effect covariance.",
                     class = "tabtools_error_model_hessian", parent = e, call = NULL)
    })
  } else if (inherits(fit, "glmmTMB")) {
    opt <- fit$fit$convergence
    if (length(opt) && any(!is.na(opt) & opt != 0)) {
      detail <- paste(fit$fit$message %||% "optimizer failure", collapse = "; ")
      cli::cli_abort(c(
        "Model {i} ({.cls glmmTMB}) did not converge (optimizer code {paste(opt, collapse = ', ')}: {detail}).",
        "i" = "Refit with suitable optimizer controls and check the convergence diagnostics before reporting inference."
      ), class = "tabtools_error_model_convergence", call = NULL)
    }
    if (identical(fit$sdr$pdHess, FALSE)) {
      cli::cli_abort(c(
        "Model {i} ({.cls glmmTMB}) has a non-positive-definite Hessian; its inference cannot be reported reliably.",
        "i" = "Check the model specification and identifiability, then refit and verify {.code fit$sdr$pdHess}."
      ), class = "tabtools_error_model_hessian", call = NULL)
    }
    b <- glmmTMB::fixef(fit)$cond
    beta_map <- fit$obj$env$map$beta
    V <- tryCatch(as.matrix(stats::vcov(fit)$cond), error = function(e) {
      cli::cli_abort("Model {i} ({.cls glmmTMB}) has no usable fixed-effect covariance.",
                     class = "tabtools_error_model_hessian", parent = e, call = NULL)
    })
  } else {
    return(invisible(TRUE))
  }
  # Rank-adjusted glmmTMB fits retain NA coefficients and covariance rows
  # for dropped columns. A beta map can fix coefficients (NA map entries)
  # or share one estimated parameter across coefficients. Validate the
  # independent estimated block, rather than rejecting valid constraints.
  estimated <- names(b)[!is.na(b)]
  if (length(beta_map) == length(b) && length(beta_map)) {
    estimated <- names(b)[!is.na(b) & !is.na(beta_map) & !duplicated(beta_map)]
  } else if (length(beta_map) == length(estimated) && length(beta_map)) {
    # With rank adjustment the native map covers retained columns, while
    # fixef()/vcov() restore NA entries for the dropped design columns.
    estimated <- estimated[!is.na(beta_map) & !duplicated(beta_map)]
  }
  if (!all(is.finite(b[!is.na(b)]))) {
    cli::cli_abort("Model {i} ({.cls {class(fit)[1]}}) has unusable fixed-effect estimates.",
                   class = "tabtools_error_model_hessian", call = NULL)
  }
  if (!length(estimated)) return(invisible(TRUE))
  if (!all(estimated %in% rownames(V)) ||
      !all(estimated %in% colnames(V))) {
    cli::cli_abort("Model {i} ({.cls {class(fit)[1]}}) has unusable fixed-effect estimates or covariance.",
                   class = "tabtools_error_model_hessian", call = NULL)
  }
  V <- V[estimated, estimated, drop = FALSE]
  if (!isTRUE(all.equal(V, t(V), tolerance = 1e-8, check.attributes = FALSE)) || !.rt_is_pd(V)) {
    cli::cli_abort(c(
      "Model {i} ({.cls {class(fit)[1]}}) has a non-finite or non-positive-definite fixed-effect covariance; its inference cannot be reported reliably.",
      "i" = "Check the fitted model's Hessian and identifiability, then refit."
    ), class = "tabtools_error_model_hessian", call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_regtab_adapter.merMod <- function(fit, ...) TRUE

#' @export
tt_coef.merMod <- function(fit, ...) lme4::fixef(fit)

# lmer: vcov() is the GLS variance Stata's `mixed` reports. glmer: the full
# inverse observed information at the estimates (Stata's me* vce(oim));
# lme4's own vcov() for nAGQ > 1 fits uses an approximation that is ~0.5%
# off (R22 age: 0.0013851 vs 0.0013915). `full = TRUE` returns the variance
# of Stata's random-effects parameters instead (ln sd, atanh rho, ln sigma,
# and menbreg's ln alpha; see .rt_re_uncertainty()), the matrix the
# random-effects and ancillary rows read, with the whole uncertainty list
# in attribute "re". lme4 reports no variance for those parameters, so
# `vce = "model"` changes only the fixed effects.
#' @export
tt_vcov.merMod <- function(fit, vce = "stata", cluster = NULL, full = FALSE, ...) {
  if (isTRUE(full)) return(.rt_re_vcov_full(fit))
  V <- as.matrix(stats::vcov(fit))
  if (identical(vce, "stata") && inherits(fit, "glmerMod")) {
    unc <- .rt_re_uncertainty(fit)
    if (!is.null(unc$V_beta)) {
      nm <- intersect(rownames(V), rownames(unc$V_beta))
      V[nm, nm] <- unc$V_beta[nm, nm]
    }
  }
  V
}

#' @export
tt_regtab_adapter.glmmTMB <- function(fit, ...) TRUE

#' @export
tt_coef.glmmTMB <- function(fit, ...) glmmTMB::fixef(fit)$cond

# glmmTMB's variance comes from TMB's sdreport(), the inverse observed
# information over every parameter, as Stata's me* commands report it.
#' @export
tt_vcov.glmmTMB <- function(fit, vce = "stata", cluster = NULL, full = FALSE, ...) {
  if (isTRUE(full)) return(.rt_re_vcov_full(fit))
  V <- as.matrix(stats::vcov(fit)$cond)
  b <- glmmTMB::fixef(fit)$cond
  if (nrow(V) == length(b)) dimnames(V) <- list(names(b), names(b))
  V
}

# The random-effects parameter variance as a named matrix (attribute "re":
# the list .rt_re_uncertainty() returns), or NULL.
.rt_re_vcov_full <- function(fit) {
  unc <- .rt_re_uncertainty(fit)
  if (is.null(unc)) return(NULL)
  V <- unc$V
  nm <- paste0("re", seq_len(nrow(V)))
  if (!is.na(unc$lnsig)) nm[unc$lnsig] <- "lnsig_e"
  dimnames(V) <- list(nm, nm)
  attr(V, "re") <- unc
  V
}

# Settings with no Stata me* equivalent, and fits whose lme4 Laplace
# differs from Stata's (Phase 5 review P1-1, P2-6).
#' @export
tt_regtab_check.merMod <- function(fit, i) {
  if (!inherits(fit, "glmerMod")) return(invisible(TRUE))
  nagq <- as.integer(fit@devcomp$dims[["nAGQ"]])
  if (identical(nagq, 0L)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn lme4::glmer} fits with {.code nAGQ = 0} (model {i}).",
      "i" = "Their estimates are not the Laplace optimum, so the observed information regtab reports (Stata's) does not apply; refit with {.code nAGQ = 1} or more."
    ), call = NULL)
  }
  fam <- stats::family(fit)
  nb <- grepl("^Negative Binomial", fam$family)
  if (nb || (nagq <= 1L && fam$link %in% c("probit", "cloglog"))) {
    what <- if (nb) "{.fn lme4::glmer.nb}" else paste0("a ", fam$link, " {.fn lme4::glmer} Laplace fit")
    cli::cli_inform(c(
      "i" = paste0("lme4's Laplace approximation for ", what, " is not Stata's, so its estimates and standard errors can differ from Stata's in the last digit."),
      " " = "{.fn glmmTMB::glmmTMB} reproduces Stata's Laplace fits exactly (see {.help tabtools::regtab}, Mixed models and GEE)."
    ), .frequency = "once", .frequency_id = paste0("tabtools_lme4_laplace_", if (nb) "nb" else fam$link))
  }
  invisible(TRUE)
}

# glmmTMB: the conditional model only. Zero-inflation and dispersion
# submodels have no Stata me* layout, and menbreg's constant-dispersion
# parameterization (nbinom1) has not been checked against Stata (Phase 5
# review P1-2).
#' @export
tt_regtab_check.glmmTMB <- function(fit, i) {
  af <- fit$modelInfo$allForm
  trivial <- function(f, rhs) is.null(f) || identical(deparse(f), rhs)
  bad <- c(if (!trivial(af$ziformula, "~0")) "ziformula", if (!trivial(af$dispformula, "~1")) "dispformula")
  if (length(bad)) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn glmmTMB::glmmTMB} fits with a {.arg {bad}} submodel (model {i}).",
      "i" = "Stata's me* commands have no zero-inflation or dispersion equations, so there is no Stata layout for them."
    ), call = NULL)
  }
  # Random-effect covariance structures (muse P1-18): unstructured (us,
  # Stata cov(unstructured)) and diagonal (diag, cov(independent)) terms,
  # or a single component; glmmTMB's cs (heterogeneous compound symmetry),
  # ar1, toep, ou, ... have no Stata me*/mixed covariance and no
  # interval here, so they are refused as for nlme::lme (R/regtab_models_nlme.R).
  rs <- fit$modelInfo$reStruc$condReStruc
  for (t in seq_along(rs)) {
    code <- names(rs[[t]]$blockCode %||% c(us = 1))
    k <- rs[[t]]$blockSize %||% 1L
    if (k > 1L && !code %in% c("us", "diag")) {
      cli::cli_abort(c(
        "{.fn regtab} does not support the {.fn {code}} covariance structure of the random-effect term {.code {names(rs)[t]}} (model {i}).",
        "i" = "Supported: unstructured ({.code (1 + x | g)}, Stata {.code cov(unstructured)}) and diagonal ({.code diag(1 + x | g)}, Stata {.code cov(independent)})."
      ), class = "tabtools_error_tmb_covstruct", call = NULL)
    }
  }
  if (identical(stats::family(fit)$family, "nbinom1")) {
    cli::cli_abort(c(
      "{.fn regtab} does not support {.fn glmmTMB::glmmTMB} fits with the {.val nbinom1} family (model {i}).",
      "i" = "Use {.code family = nbinom2} (Stata {.code menbreg}'s default mean dispersion)."
    ), call = NULL)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Random-effects structure

# Stata name of a random-effect component.
.rt_re_sname <- function(x) ifelse(x == "(Intercept)", "_cons", x)

# Variable label of a model-frame column, falling back to the name.
.rt_frame_label <- function(frame, v) {
  x <- if (!is.null(frame) && v %in% names(frame)) frame[[v]] else NULL
  lab <- if (is.null(x)) NULL else attr(x, "label", exact = TRUE)
  if (is.character(lab) && length(lab) == 1L && nzchar(lab)) lab else v
}

#' Random-effects structure of a mixed model
#'
#' @return list(style = "mixed" (Stata `mixed` naming) or "me";
#'   groups = data.frame(name, var, label, nlev) outermost first;
#'   terms = list of list(group, comps, Sigma, theta_idx); resid = the
#'   residual variance or NA; sigma; family details), or NULL for a model
#'   without random effects.
#' @keywords internal
#' @noRd
.rt_re_struct <- function(fit) {
  if (inherits(fit, "merMod")) return(.rt_re_struct_mer(fit))
  if (inherits(fit, "glmmTMB")) return(.rt_re_struct_tmb(fit))
  if (inherits(fit, "lme")) return(.rt_re_struct_lme(fit))
  NULL
}

# The grouping variable Stata names a level by (the terminal variable of
# `|| zone: || location:`), from lme4's grouping-factor names. lme4 writes
# `(1 | zone/location)` as "location:zone" (innermost first), but a user's
# `(1 | zone) + (1 | zone:location)` as "zone:location": the level's
# variable is the part of its name that no enclosing level (a factor whose
# parts are a strict subset of its own) already uses. An interaction with
# no enclosing level is named by its whole name (review P3c), and one whose
# every part is itself a level (crossed effects with their interaction) is
# refused, as are two levels that would share a variable: their rows would
# share keys, and the multi-model union would drop one of them (review P3-5
# of task 5.19).
.rt_re_group_vars <- function(names) {
  parts <- lapply(strsplit(gsub("[()]", "", names), ":", fixed = TRUE), trimws)
  rest <- lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    outer <- unlist(parts[vapply(parts, function(q) length(q) < length(p) && all(q %in% p), TRUE)])
    list(outer = outer, rest = setdiff(p, outer))
  })
  # Every part of an interaction level is itself a level: crossed effects
  # with their interaction, (1 | a) + (1 | b) + (1 | a:b). No single
  # variable names that level.
  crossed <- vapply(rest, function(r) length(r$outer) > 0L && !length(r$rest), TRUE)
  if (any(crossed)) {
    cli::cli_abort(c(
      "The random-effects level {.val {names[crossed]}} is the interaction of levels that are themselves in the model (crossed random effects with their interaction).",
      "i" = "Stata's layout names each level by one variable, so regtab cannot name its rows. Pass the estimates as a data frame (see {.help tabtools::regtab}, Data-frame input)."
    ), call = NULL)
  }
  var <- vapply(seq_along(parts), function(i) {
    r <- rest[[i]]
    # A nested level: its own variable. An interaction with no enclosing
    # level (a lone (1 | zone:location)): its whole name, so that its rows
    # are not named after one of its parts.
    if (length(r$outer) && length(r$rest) == 1L) r$rest else if (length(parts[[i]]) > 1L) names[i] else parts[[i]][1]
  }, "")
  dup <- unique(var[duplicated(var)])
  if (length(dup)) {
    cli::cli_abort(c(
      "The random-effects grouping levels {.val {names[var %in% dup]}} would be named by the same variable ({.var {dup}}) in Stata's layout.",
      "i" = "Write nested levels as {.code (1 | outer/inner)}, or as {.code (1 | outer) + (1 | outer:inner)}, so that each level has its own variable."
    ), call = NULL)
  }
  var
}

# Order grouping factors outermost first (Stata requires the hierarchy
# outermost first; lme4 sorts by decreasing number of levels), label them,
# and apply Stata's duplicate-label fallback.
.rt_re_groups <- function(names, nlev, frame) {
  var <- .rt_re_group_vars(names)
  lab <- vapply(seq_along(names), function(i) {
    if (names[i] %in% names(frame)) .rt_frame_label(frame, names[i]) else .rt_frame_label(frame, var[i])
  }, "")
  ord <- order(nlev, seq_along(nlev))
  g <- data.frame(name = names[ord], var = var[ord], label = lab[ord], nlev = nlev[ord],
                  stringsAsFactors = FALSE)
  if (nrow(g) > 1L) {
    dup <- g$label %in% g$label[duplicated(g$label)]
    g$label[dup] <- g$var[dup]
  }
  g
}

.rt_re_struct_mer <- function(fit) {
  g <- function(nm) lme4::getME(fit, nm)
  cnms <- g("cnms")
  if (!length(cnms)) return(NULL)
  flist <- g("flist")
  asgn <- attr(flist, "assign")
  theta <- g("theta")
  use_sc <- isTRUE(as.logical(fit@devcomp$dims[["useSc"]]))
  sig <- if (use_sc) stats::sigma(fit) else 1
  # A `subset =` frame loses its labels (Phase 5 review P0-7).
  frame <- .rt_restore_attrs(fit@frame, fit)
  groups <- .rt_re_groups(names(flist), vapply(flist, nlevels, 1L), frame)
  terms <- list()
  pos <- 0L
  for (t in seq_along(cnms)) {
    k <- length(cnms[[t]])
    nt <- k * (k + 1L) / 2L
    idx <- pos + seq_len(nt)
    pos <- pos + nt
    L <- matrix(0, k, k)
    L[lower.tri(L, diag = TRUE)] <- theta[idx]
    terms[[t]] <- list(group = match(names(flist)[asgn[t]], groups$name), comps = cnms[[t]],
                       Sigma = sig^2 * tcrossprod(L), theta_idx = idx)
  }
  mixed <- inherits(fit, "lmerMod")
  list(style = if (mixed) "mixed" else "me", groups = groups, terms = terms,
       resid = if (mixed) sig^2 else NA_real_, sigma = sig, use_sc = use_sc, frame = frame)
}

# glmmTMB: conditional-model random effects. Its theta holds log-SDs, then
# (for `us`) correlation parameters in glmmTMB's own scale; only the
# log-SDs are used here, so covariance rows of glmmTMB fits get no CI.
.rt_re_struct_tmb <- function(fit) {
  rt <- fit$modelInfo$reTrms$cond
  if (is.null(rt) || !length(rt$flist)) return(NULL)
  vc <- glmmTMB::VarCorr(fit)$cond
  cnms <- rt$cnms
  flist <- rt$flist
  asgn <- attr(flist, "assign")
  frame <- .rt_restore_attrs(fit$frame, fit)
  groups <- .rt_re_groups(names(flist), vapply(flist, nlevels, 1L), frame)
  fam <- stats::family(fit)
  gauss <- fam$family == "gaussian" && fam$link == "identity"
  terms <- lapply(seq_along(cnms), function(t) {
    S <- vc[[t]]
    S <- matrix(as.numeric(S), nrow(S))
    code <- names(fit$modelInfo$reStruc$condReStruc[[t]]$blockCode %||% c(us = 1))
    list(group = match(names(flist)[asgn[t]], groups$name), comps = cnms[[t]], Sigma = S,
         theta_idx = NULL, cov = !identical(code, "diag"))
  })
  list(style = if (gauss) "mixed" else "me", groups = groups, terms = terms,
       resid = if (gauss) stats::sigma(fit)^2 else NA_real_, sigma = stats::sigma(fit),
       use_sc = gauss, frame = frame)
}

# Signature of the grouping structure (Stata's ivars/revars/redim metadata,
# `regtab.ado:1159-1188`), to detect models whose structures differ.
.rt_re_signature <- function(re) {
  if (is.null(re)) return(NULL)
  per <- vapply(seq_len(nrow(re$groups)), function(gi) {
    comps <- unlist(lapply(re$terms[vapply(re$terms, `[[`, 1L, "group") == gi], `[[`, "comps"))
    paste0(re$groups$var[gi], ":", paste(sort(comps), collapse = ","))
  }, "")
  paste(per, collapse = "|")
}

#' Cross-model random-effects checks (`regtab.ado:670-679`, `:1167-1188`)
#'
#' Stata refuses to combine models whose random effects are reported on
#' different scales (variance vs median odds/hazard ratio) unless
#' `noreeffects`, and refuses `relabel` when the models' grouping
#' structures differ; without `relabel` such models keep their raw row
#' names and a median odds ratio row loses its group label. Adds
#' `o$re_shared` (TRUE when the structures agree).
#' @keywords internal
#' @noRd
.rt_re_prepare <- function(fits, infos, o) {
  fam <- vapply(infos, function(i) i$re_family %||% "none", "")
  fam <- unique(fam[fam != "none"])
  if (length(fam) > 1L && !isTRUE(o$noreeffects)) {
    cli::cli_abort(c(
      "Mixed models whose random effects use different scales (variance, median odds ratio, median hazard ratio) cannot share random-effects rows.",
      "i" = "Use separate {.fn regtab} calls, or set {.code noreeffects = TRUE}."
    ), call = NULL)
  }
  sig <- unlist(lapply(fits, function(f) .rt_re_signature(.rt_re_struct(f))))
  o$re_shared <- length(unique(sig)) <= 1L
  if (!o$re_shared && isTRUE(o$relabel) && !isTRUE(o$noreeffects)) {
    cli::cli_abort(c("Random-effects structures differ across the models, so {.arg relabel} cannot name their rows.",
                     "i" = "Use separate {.fn regtab} calls, or omit {.arg relabel}."), call = NULL)
  }
  o
}

# ---------------------------------------------------------------------------
# Uncertainty of the random-effects parameters

# Stata's parameters of one term: ln sd per component, atanh(rho) per pair.
.rt_re_psi <- function(Sigma) {
  sd <- sqrt(pmax(diag(Sigma), 0))
  k <- length(sd)
  pairs <- if (k > 1L) which(upper.tri(diag(k)), arr.ind = TRUE) else matrix(0L, 0, 2)
  pairs <- pairs[order(pairs[, 1], pairs[, 2]), , drop = FALSE]
  rho <- if (nrow(pairs)) Sigma[pairs] / (sd[pairs[, 1]] * sd[pairs[, 2]]) else numeric()
  # A correlation at +/-1 can come out a rounding error beyond it: clamped,
  # so that atanh() gives +/-Inf (a boundary term, no interval) instead of
  # NaN with a warning.
  rho <- pmax(-1, pmin(1, rho))
  list(lnsd = log(sd), atr = atanh(rho), pairs = pairs)
}

# Inverse map: relative Cholesky factor (lme4 theta) of one term.
.rt_re_theta <- function(lnsd, atr, pairs, sig = 1) {
  k <- length(lnsd)
  R <- diag(k)
  if (nrow(pairs)) {
    R[pairs] <- tanh(atr)
    R[pairs[, 2:1, drop = FALSE]] <- tanh(atr)
  }
  S <- (exp(lnsd) %o% exp(lnsd)) * R / sig^2
  L <- t(chol(S))
  L[lower.tri(L, diag = TRUE)]
}

# Hessian of f at x by central differences, Richardson-extrapolated over
# steps h and h/2 (error O(h^4)).
.rt_num_hessian <- function(f, x, h) {
  one <- function(h) {
    p <- length(x)
    H <- matrix(0, p, p)
    f0 <- f(x)
    for (i in seq_len(p)) {
      ei <- replace(numeric(p), i, h[i])
      H[i, i] <- (f(x + ei) - 2 * f0 + f(x - ei)) / h[i]^2
      if (i < p) for (j in (i + 1L):p) {
        ej <- replace(numeric(p), j, h[j])
        H[i, j] <- H[j, i] <- (f(x + ei + ej) - f(x + ei - ej) - f(x - ei + ej) + f(x - ei - ej)) /
          (4 * h[i] * h[j])
      }
    }
    H
  }
  (4 * one(h / 2) - one(h)) / 3
}

# Deviance (-2 log-likelihood) of an lmer fit as a function of (theta,
# ln sigma), profiled over the fixed effects: the ML deviance at the GLS
# beta(theta), or the REML criterion. Written with the penalized
# least-squares identities of lme4 (Bates et al. 2015, JSS 67(1), eq. 34
# and 41), so it equals -2 logLik(fit) at the estimates.
.rt_lmm_devfun <- function(fit) {
  g <- function(nm) lme4::getME(fit, nm)
  Lt0 <- g("Lambdat")
  Lind <- g("Lind")
  .rt_lmm_devfun_core(X = as.matrix(g("X")), Zt = g("Zt"), y = g("y"), off = g("offset"),
                      w = fit@resp$weights, reml = lme4::isREML(fit),
                      lambdat = function(theta) {
                        Lt <- Lt0
                        Lt@x <- theta[Lind]
                        Lt
                      })
}

# The same deviance from the model's pieces: fixed-effect design X, the
# transposed random-effects design Zt (sparse), response y, offset, prior
# weights, and `lambdat(theta)`, the transposed relative covariance factor
# at theta (lmer: lme4's Lambdat template; nlme::lme: built by
# .rt_lme_design()).
.rt_lmm_devfun_core <- function(X, Zt, y, off, w, reml, lambdat) {
  n <- length(y)
  p <- ncol(X)
  q <- nrow(Zt)
  sw <- sqrt(w)
  Zw <- Zt %*% Matrix::Diagonal(x = sw)
  Xw <- X * sw
  yw <- (y - off) * sw
  Iq <- Matrix::Diagonal(q)
  slw <- sum(log(w))
  XtX <- crossprod(Xw)
  Xty <- crossprod(Xw, yw)
  # lnsig = NULL: the deviance profiled over sigma as well (its maximiser
  # pw / n, or pw / (n - p) for REML), the criterion lme4 optimises.
  function(theta, lnsig = NULL) {
    LZ <- lambdat(theta) %*% Zw
    A <- Matrix::forceSymmetric(Matrix::tcrossprod(LZ) + Iq)
    LZX <- as.matrix(LZ %*% Xw)
    LZy <- as.numeric(LZ %*% yw)
    AX <- as.matrix(Matrix::solve(A, LZX))
    Ay <- as.numeric(Matrix::solve(A, LZy))
    M <- XtX - crossprod(LZX, AX)
    b <- solve(M, Xty - crossprod(LZX, Ay))
    u <- Ay - drop(AX %*% b)
    r <- yw - drop(Xw %*% b) - as.numeric(Matrix::crossprod(LZ, u))
    pw <- sum(r^2) + sum(u^2)
    s2 <- if (is.null(lnsig)) pw / (if (reml) n - p else n) else exp(2 * lnsig)
    ld <- as.numeric(Matrix::determinant(A, logarithm = TRUE)$modulus)
    if (reml) {
      (n - p) * log(2 * pi * s2) - slw + ld + as.numeric(determinant(M, logarithm = TRUE)$modulus) + pw / s2
    } else {
      n * log(2 * pi * s2) - slw + ld + pw / s2
    }
  }
}

# Deviance function of a glmer fit over c(theta, beta), with the fit's
# quadrature (nAGQ; Laplace for nAGQ = 0), rebuilt from the fitted object
# with lme4's exported constructors so the fit itself is untouched.
# `family` replaces the fit's (glmer.nb's negative binomial at another
# theta).
.rt_glmm_devfun <- function(fit, family = stats::family(fit)) {
  rt <- lme4::getME(fit, c("Zt", "theta", "Lind", "Gp", "lower", "Lambdat", "flist", "cnms"))
  nagq <- max(1L, as.integer(fit@devcomp$dims[["nAGQ"]]))
  df <- lme4::mkGlmerDevfun(fr = fit@frame, X = lme4::getME(fit, "X"), reTrms = rt,
                            family = family, nAGQ = 1L)
  lme4::updateGlmerDevfun(df, rt, nAGQ = nagq)
}

# Saturated log-likelihood of a glmer fit's family (the family's
# log-density at mu = y): lme4's deviance for nAGQ > 1 is taken relative to
# the saturated model, so its logLik() omits this constant, which Stata's
# e(ll) includes (Phase 5 review P0-6: mepoisson -951.02 vs Stata
# -1746.20); it is 0 for Bernoulli outcomes.
.rt_glmm_ll_sat <- function(fit, family = stats::family(fit)) {
  r <- fit@resp
  -family$aic(r$y, r$n, r$y, r$weights, 0) / 2
}

# Cache: the Hessian is needed for the fixed-effect variance (tt_vcov) and
# the random-effects rows of the same fit. Keyed by a light fingerprint of
# the fit, so the cache neither holds the fit nor compares it in full
# (review P3-2).
.rt_re_cache <- new.env(parent = emptyenv())

.rt_re_fingerprint <- function(fit) {
  if (inherits(fit, "lme")) return(.rt_lme_fingerprint(fit))
  list(class(fit), deparse(stats::getCall(fit)), lme4::getME(fit, "theta"), lme4::fixef(fit),
       fit@devcomp$cmp, fit@devcomp$dims, stats::nobs(fit), sum(fit@resp$y), sum(fit@resp$weights))
}

#' Variance of the random-effects parameters (and, for glmer, the fixed
#' effects) from the observed information
#'
#' @return list(psi = estimates of Stata's parameters, V = their variance
#'   (NA where a parameter sits on the boundary: a zero variance or a
#'   correlation of +/-1), map = per term list(lnsd, atr, pairs) indices into
#'   psi, lnsig = index of ln sigma or NA, lnalpha = index of glmer.nb's
#'   ln alpha or NA, V_beta = glmer fixed-effect variance or NULL), or NULL
#'   when it cannot be computed (with a warning: the fixed effects then use
#'   lme4's vcov() and the random-effects rows have no interval).
#' @keywords internal
#' @noRd
.rt_re_uncertainty <- function(fit) {
  lme <- inherits(fit, "lme")
  if (!inherits(fit, "merMod") && !lme) {
    return(if (inherits(fit, "glmmTMB")) .rt_re_uncertainty_tmb(fit) else NULL)
  }
  key <- .rt_re_fingerprint(fit)
  if (identical(.rt_re_cache$key, key)) {
    val <- .rt_re_cache$value
    why <- .rt_re_cache$why
  } else {
    msg <- NULL
    val <- tryCatch(if (lme) .rt_re_uncertainty_lme(fit) else .rt_re_uncertainty_mer(fit), error = function(e) {
      msg <<- conditionMessage(e)
      NULL
    })
    why <- NULL
    if (is.null(val) && !is.null(.rt_re_struct(fit))) {
      why <- if (!is.null(msg)) msg else
        "the family has an estimated scale parameter (Gamma, or gaussian with a non-identity link), which lme4 keeps outside its deviance function"
    }
    .rt_re_cache$key <- key
    .rt_re_cache$value <- val
    .rt_re_cache$why <- why
  }
  # Silent fallbacks hid wrong intervals (review P2-4); the warning fires on
  # every call, not once per cached fit (Milestone H decision H-D6,
  # external review F28), and regtab records it in stored$vce_fallback.
  if (!is.null(why) && lme) {
    # The fixed effects keep varFix, which is Stata's variance already.
    .rt_warn_fallback(
      c("Stata's variance of this {.cls lme} fit's random-effects parameters could not be formed: {why}.",
        "i" = "The random-effects rows have no confidence interval."),
      "random-effects rows without confidence intervals",
      why = why
    )
  } else if (!is.null(why)) {
    .rt_warn_fallback(
      c("Stata's variance of this {.cls {class(fit)[1]}} fit could not be formed: {why}.",
        "i" = "Fixed effects use lme4's {.fn vcov}; the random-effects rows have no confidence interval."),
      "lme4 vcov() for the fixed effects; random-effects rows without confidence intervals",
      why = why, cls = class(fit)[1]
    )
  }
  val
}

# A variance fallback that policy allows (H-D6: mixed models only): a
# warning of class "tabtools_vce_fallback" carrying a short description in
# `$fallback`, which tt_regtab_build() records per model in
# stored$vce_fallback.
.rt_warn_fallback <- function(message, fallback, ..., .envir = parent.frame()) {
  cli::cli_warn(message, class = "tabtools_vce_fallback", fallback = fallback, .envir = list2env(list(...), parent = .envir))
}

# Stata's parameters of every term (ln sd per component, atanh rho per
# pair), with per term the indices of each (map) and whether the term's
# parameters are free (FALSE for a term on the boundary: a zero variance or
# a correlation of +/-1).
.rt_re_param_map <- function(terms) {
  psi <- numeric()
  map <- list()
  free <- logical()
  for (t in seq_along(terms)) {
    ps <- .rt_re_psi(terms[[t]]$Sigma)
    k <- length(ps$lnsd)
    # A term flagged `boundary` (an nlme::lme term whose maximum is on the
    # boundary, .rt_re_struct_lme()) is fixed, as lmer's exact zeros are.
    ok <- all(is.finite(ps$lnsd)) && all(is.finite(ps$atr)) && !isTRUE(terms[[t]]$boundary)
    i_sd <- length(psi) + seq_len(k)
    i_atr <- length(psi) + k + seq_along(ps$atr)
    psi <- c(psi, ps$lnsd, ps$atr)
    free <- c(free, rep(ok, k + length(ps$atr)))
    map[[t]] <- list(lnsd = i_sd, atr = i_atr, pairs = ps$pairs)
  }
  list(psi = psi, map = map, free = free)
}

.rt_re_uncertainty_mer <- function(fit) {
  re <- .rt_re_struct_mer(fit)
  if (is.null(re)) return(NULL)
  lmm <- inherits(fit, "lmerMod")
  # glmer families with an estimated scale (Gamma, non-identity gaussian)
  # carry sigma outside the deviance function: not supported.
  if (!lmm && re$use_sc) return(NULL)
  fam <- stats::family(fit)
  nb <- !lmm && grepl("^Negative Binomial", fam$family)
  theta0 <- lme4::getME(fit, "theta")
  pm <- .rt_re_param_map(re$terms)
  psi <- pm$psi
  map <- pm$map
  free <- pm$free
  lnsig <- NA_integer_
  lnalpha <- NA_integer_
  if (lmm) {
    psi <- c(psi, log(re$sigma))
    free <- c(free, TRUE)
    lnsig <- length(psi)
  }
  if (nb) {
    # glmer.nb holds theta = 1/alpha outside lme4's deviance; Stata's
    # menbreg estimates ln alpha jointly, so it joins the parameters and the
    # deviance is rebuilt at each alpha (Phase 5 review P0-1).
    psi <- c(psi, -log(lme4::getME(fit, "glmer.nb.theta")))
    free <- c(free, TRUE)
    lnalpha <- length(psi)
  }
  theta_of <- function(p) {
    th <- theta0
    for (t in seq_along(re$terms)) {
      m <- map[[t]]
      if (!free[m$lnsd[1]]) next
      sig <- if (lmm) exp(p[lnsig]) else 1
      th[re$terms[[t]]$theta_idx] <- .rt_re_theta(p[m$lnsd], p[m$atr], m$pairs, sig)
    }
    th
  }
  fi <- which(free)
  if (lmm) {
    dev <- .rt_lmm_devfun(fit)
    f <- function(x) {
      p <- psi
      p[fi] <- x
      dev(theta_of(p), p[lnsig])
    }
    H <- .rt_num_hessian(f, psi[fi], rep(0.01, length(fi)))
    Vf <- 2 * solve(H)
    V_beta <- NULL
  } else {
    nagq <- max(1L, as.integer(fit@devcomp$dims[["nAGQ"]]))
    if (nb) {
      # One deviance function per alpha; for nAGQ > 1 add back the
      # saturated log-likelihood, which depends on alpha.
      devs <- new.env(parent = emptyenv())
      dev_at <- function(la) {
        k <- format(la, digits = 17)
        if (is.null(devs[[k]])) {
          famk <- MASS::negative.binomial(theta = exp(-la))
          d <- .rt_glmm_devfun(fit, famk)
          sat <- if (nagq > 1L) .rt_glmm_ll_sat(fit, famk) else 0
          devs[[k]] <- function(x) d(x) - 2 * sat
        }
        devs[[k]]
      }
    } else {
      dev0 <- .rt_glmm_devfun(fit)
      dev_at <- function(la) dev0
    }
    beta <- lme4::fixef(fit)
    se0 <- sqrt(diag(as.matrix(stats::vcov(fit))))
    nb_ <- length(beta)
    f <- function(x) {
      p <- psi
      p[fi] <- x[seq_along(fi)]
      la <- if (nb) p[lnalpha] else NA_real_
      dev_at(la)(c(theta_of(p), x[length(fi) + seq_len(nb_)]))
    }
    H <- .rt_num_hessian(f, c(psi[fi], beta), c(rep(0.01, length(fi)), 0.1 * se0))
    Vall <- 2 * solve(H)
    bi <- length(fi) + seq_len(nb_)
    V_beta <- Vall[bi, bi, drop = FALSE]
    dimnames(V_beta) <- list(names(beta), names(beta))
    Vf <- Vall[seq_along(fi), seq_along(fi), drop = FALSE]
  }
  V <- matrix(NA_real_, length(psi), length(psi))
  V[fi, fi] <- Vf
  list(psi = psi, V = V, map = map, lnsig = lnsig, lnalpha = lnalpha, V_beta = V_beta)
}

# glmmTMB: theta holds each term's log-SDs, then for `us` terms the
# correlation parameters in glmmTMB's own scale (the below-diagonal entries
# of a unit lower-triangular factor L, correlation = D^-1/2 L L' D^-1/2),
# with their variance from sdreport(). Covariance standard errors use the
# delta method on that scale (numerical gradient); the factor's fill order
# is checked against VarCorr() and a mismatch leaves the CI blank, with a
# console note. Terms with other structures (cs, ar1, ...) never get here:
# tt_regtab_check.glmmTMB() refuses them (class tabtools_error_tmb_covstruct). Residual: the gaussian
# dispersion parameter (`betadisp`, ln sigma; `betad` in older versions).
.rt_tmb_cor <- function(t, k) {
  L <- diag(k)
  L[lower.tri(L)] <- t
  C <- tcrossprod(L)
  d <- 1 / sqrt(diag(C))
  C * (d %o% d)
}

.rt_re_uncertainty_tmb <- function(fit) {
  re <- .rt_re_struct_tmb(fit)
  if (is.null(re)) return(NULL)
  Vfull <- .rt_tmb_vcov_full(fit)
  pf <- fit$fit$par
  if (is.null(Vfull) || is.null(pf) || nrow(Vfull) != length(pf)) return(NULL)
  th_pos <- which(names(pf) == "theta")
  rs <- fit$modelInfo$reStruc$condReStruc
  psi <- numeric()
  sel <- integer()
  map <- list()
  cov_se <- list()
  used <- 0L
  for (t in seq_along(re$terms)) {
    k <- length(re$terms[[t]]$comps)
    code <- names(rs[[t]]$blockCode %||% c(us = 1))
    ntheta <- rs[[t]]$blockNumTheta %||% (if (k == 1L) 1L else k + k * (k - 1L) / 2L)
    blk <- th_pos[used + seq_len(ntheta)]
    used <- used + ntheta
    ok <- k == 1L || code %in% c("diag", "us")
    i_sd <- length(psi) + seq_len(k)
    psi <- c(psi, if (ok) pf[blk[seq_len(k)]] else rep(NA_real_, k))
    sel <- c(sel, if (ok) blk[seq_len(k)] else rep(NA_integer_, k))
    map[[t]] <- list(lnsd = i_sd, atr = integer(), pairs = matrix(0L, 0, 2))
    cs <- matrix(NA_real_, k, k)
    if (k > 1L && identical(code, "us") && length(blk) == ntheta) {
      tb <- pf[blk]
      Vb <- Vfull[blk, blk, drop = FALSE]
      covf <- function(x) {
        sd <- exp(x[seq_len(k)])
        (sd %o% sd) * .rt_tmb_cor(x[-seq_len(k)], k)
      }
      if (isTRUE(all.equal(covf(tb), re$terms[[t]]$Sigma, tolerance = 1e-6, check.attributes = FALSE))) {
        for (a in seq_len(k - 1L)) for (b in (a + 1L):k) {
          gr <- vapply(seq_along(tb), function(m) {
            h <- 1e-6 * max(1, abs(tb[m]))
            (covf(replace(tb, m, tb[m] + h))[a, b] - covf(replace(tb, m, tb[m] - h))[a, b]) / (2 * h)
          }, 0)
          cs[a, b] <- cs[b, a] <- sqrt(drop(crossprod(gr, Vb %*% gr)))
        }
      } else {
        # No silent blank (CAT P1-8): the factor's fill order disagrees with
        # VarCorr(), so the covariance intervals of this term stay blank.
        cli::cli_inform(c("Note: the covariance interval(s) of the random-effect term {.code {names(rs)[t]}} are blank.",
                          "i" = "glmmTMB's correlation parameterization could not be matched to {.fn VarCorr}."),
                        class = "tabtools_note_tmb_cov_ci")
      }
    }
    cov_se[[t]] <- cs
  }
  lnsig <- NA_integer_
  if (identical(re$style, "mixed")) {
    d <- which(names(pf) %in% c("betadisp", "betad"))
    if (length(d) == 1L) {
      # ln sigma directly, or ln sigma^2 in older glmmTMB versions.
      f <- if (abs(exp(pf[d]) - re$sigma) <= 1e-6 * re$sigma) 1 else 0.5
      psi <- c(psi, f * pf[d])
      sel <- c(sel, d)
      lnsig <- length(psi)
    }
  }
  V <- matrix(NA_real_, length(psi), length(psi))
  ok <- !is.na(sel)
  V[ok, ok] <- Vfull[sel[ok], sel[ok], drop = FALSE]
  if (!is.na(lnsig) && f != 1) {
    V[lnsig, ] <- V[lnsig, ] * f
    V[, lnsig] <- V[, lnsig] * f
  }
  list(psi = unname(psi), V = unname(V), map = map, lnsig = lnsig, V_beta = NULL, cov_se = cov_se)
}

# ---------------------------------------------------------------------------
# Rows

# Median odds (hazard) ratio of a variance (`regtab.ado:2102-2106`), for
# the estimate and each CI bound; negative or missing values pass through
# as missing.
.rt_mor <- function(v) {
  ok <- !is.na(v) & v >= 0
  out <- rep(NA_real_, length(v))
  out[ok] <- exp(sqrt(2 * v[ok]) * stats::qnorm(0.75))
  out
}

#' Random-effects rows of a mixed model
#'
#' @return data.frame shaped like tt_regtab_rows() output with kind "re",
#'   one row per variance and covariance, then var(e); NULL when the model
#'   has no random effects.
#' @keywords internal
#' @noRd
.rt_re_rows <- function(fit, info, o) {
  re <- .rt_re_struct(fit)
  if (is.null(re)) return(NULL)
  unc <- attr(tt_vcov(fit, o$vce, full = TRUE), "re")
  z <- stats::qnorm((1 + o$level) / 2)
  me <- identical(re$style, "me")
  multi <- nrow(re$groups) > 1L
  transform <- if (identical(info$re_family, "mor")) "Median Odds Ratio" else
    if (identical(info$re_family, "mhr")) "Median Hazard Ratio" else NA_character_
  slope_label <- function(cn) if (cn == "(Intercept)") "Intercept" else .rt_frame_label(re$frame, cn)
  term_group <- vapply(re$terms, `[[`, 1L, "group")
  psi_se <- function(i) {
    if (is.null(unc) || !length(i) || anyNA(i)) return(NA_real_)
    v <- unc$V[i, i]
    if (is.na(v) || v < 0) NA_real_ else sqrt(v)
  }
  out <- list()
  add <- function(key, label, est, lo, hi, p = NA_real_) {
    out[[length(out) + 1L]] <<- .rt_row(key, key, "re", label, "est",
                                         estimate = est, conf.low = lo, conf.high = hi, p.value = p,
                                         sub = length(out), ancillary = TRUE)
  }
  var_ci <- function(v, se) if (is.na(se) || !(v > 0)) c(NA_real_, NA_real_) else exp(log(v) + c(-2, 2) * z * se)
  for (gi in seq_len(nrow(re$groups))) {
    gv <- re$groups$var[gi]
    glab <- re$groups$label[gi]
    tix <- which(term_group == gi)
    comps <- unlist(lapply(re$terms[tix], `[[`, "comps"))
    # Display order: slopes in order of appearance, then the constant.
    disp <- c(setdiff(unique(comps), "(Intercept)"), intersect("(Intercept)", comps))
    where <- lapply(disp, function(cn) {
      for (t in tix) {
        j <- match(cn, re$terms[[t]]$comps)
        if (!is.na(j)) return(c(t, j))
      }
      NULL
    })
    sfx <- if (me || multi) paste0("[", gv, "]") else ""
    for (d in seq_along(disp)) {
      t <- where[[d]][1]
      j <- where[[d]][2]
      v <- re$terms[[t]]$Sigma[j, j]
      sn <- .rt_re_sname(disp[d])
      key <- paste0("var(", sn, sfx, ")")
      se <- if (is.null(unc)) NA_real_ else psi_se(unc$map[[t]]$lnsd[j])
      ci <- var_ci(v, se)
      label <- key
      if (isTRUE(o$relabel)) {
        # me* slope variances too since tabtools 2.1.11 (2.1.9 left them raw).
        label <- paste0("Variance: ", glab, " (",
                        if (disp[d] == "(Intercept)") "Intercept" else slope_label(disp[d]), ")")
      }
      if (!is.na(transform) && disp[d] == "(Intercept)") {
        label <- if (isTRUE(o$re_shared %||% TRUE)) paste0(transform, " (", glab, ")") else transform
        add(key, label, .rt_mor(v), .rt_mor(ci[1]), .rt_mor(ci[2]))
      } else {
        add(key, label, v, ci[1], ci[2])
      }
    }
    # Covariances within a term, pairs in display order.
    if (length(disp) > 1L) for (a in seq_len(length(disp) - 1L)) for (b in (a + 1L):length(disp)) {
      ta <- where[[a]]
      tb <- where[[b]]
      if (ta[1] != tb[1]) next
      t <- ta[1]
      S <- re$terms[[t]]$Sigma
      cv <- S[ta[2], tb[2]]
      sa <- .rt_re_sname(disp[a])
      sb <- .rt_re_sname(disp[b])
      key <- if (me) paste0("cov(", sa, "[", gv, "],", sb, "[", gv, "])") else paste0("cov(", sa, ",", sb, sfx, ")")
      if (isFALSE(re$terms[[t]]$cov %||% TRUE)) next
      se <- NA_real_
      if (!is.null(unc$cov_se)) {
        se <- unc$cov_se[[t]][ta[2], tb[2]]
      } else if (!is.null(unc)) {
        m <- unc$map[[t]]
        k <- which((m$pairs[, 1] == min(ta[2], tb[2]) & m$pairs[, 2] == max(ta[2], tb[2])))
        idx <- c(m$lnsd[ta[2]], m$lnsd[tb[2]], m$atr[k])
        if (length(idx) == 3L && !anyNA(unc$V[idx, idx])) {
          sd <- exp(unc$psi[idx[1:2]])
          r <- tanh(unc$psi[idx[3]])
          gr <- c(cv, cv, (1 - r^2) * sd[1] * sd[2])
          # An indefinite block (a near-boundary term) has no standard
          # error: blank, never sqrt() of a negative.
          q <- drop(crossprod(gr, unc$V[idx, idx] %*% gr))
          se <- if (is.finite(q) && q >= 0) sqrt(q) else NA_real_
        }
      }
      ci <- if (is.na(se)) c(NA_real_, NA_real_) else cv + c(-1, 1) * z * se
      p <- if (is.na(se) || se <= 0) NA_real_ else 2 * stats::pnorm(abs(cv / se), lower.tail = FALSE)
      label <- key
      if (isTRUE(o$relabel)) {
        # Both components by label, for me* names too (tabtools 2.1.11,
        # `regtab.ado:1721-1750`; 2.1.9 kept the first component's bracket).
        label <- paste0("Covariance: ", glab, " (", slope_label(disp[a]), ", ", slope_label(disp[b]), ")")
      }
      add(key, label, cv, ci[1], ci[2], p)
    }
  }
  if (!is.na(re$resid)) {
    se <- if (is.null(unc) || is.na(unc$lnsig)) NA_real_ else psi_se(unc$lnsig)
    ci <- var_ci(re$resid, se)
    add("var(e)", if (isTRUE(o$relabel)) "Residual Variance" else "var(e)", re$resid, ci[1], ci[2])
  }
  do.call(rbind, out)
}

# menbreg's /lnalpha (Phase 5 review P0-1; probes C13, B20, G17): an
# ancillary row of the model's own equation, so it precedes the intercept
# and the random-effects rows follow; Wald interval on ln alpha, no
# p-value, not exponentiated; nointercept drops it (its label). No alpha
# diparm, unlike nbreg. glmer.nb's theta and glmmTMB nbinom2's dispersion
# parameter are 1/alpha.
.rt_lnalpha_row <- function(est, se, level) {
  z <- stats::qnorm((1 + level) / 2)
  w <- data.frame(term = "lnalpha", estimate = est, std.error = se, statistic = est / se, p.value = NA_real_,
                  conf.low = est - z * se, conf.high = est + z * se, stringsAsFactors = FALSE)
  r <- .rt_anc_row("lnalpha", "lnalpha", w)
  r$p.value <- NA_real_
  r
}

#' @export
tt_regtab_ancillary_rows.merMod <- function(fit, info, level = 0.95, vce = "stata", ...) {
  if (!inherits(fit, "glmerMod") || !grepl("^Negative Binomial", stats::family(fit)$family)) return(NULL)
  unc <- attr(tt_vcov(fit, vce, full = TRUE), "re")
  est <- -log(lme4::getME(fit, "glmer.nb.theta"))
  se <- if (is.null(unc) || is.na(unc$lnalpha)) NA_real_ else sqrt(unc$V[unc$lnalpha, unc$lnalpha])
  .rt_lnalpha_row(est, se, level)
}

# glmmTMB's variance over every parameter (sdreport()); when it fails, a
# warning on every call (not silent: external review F28) and NULL, so the
# random-effects and ancillary rows have no interval.
.rt_tmb_vcov_full <- function(fit) {
  msg <- NULL
  V <- tryCatch(suppressWarnings(stats::vcov(fit, full = TRUE)), error = function(e) {
    msg <<- conditionMessage(e)
    NULL
  })
  pf <- fit$fit$par
  if (is.null(V) || is.null(pf) || nrow(V) != length(pf)) {
    why <- msg %||% "it does not cover every parameter"
    .rt_warn_fallback(
      c("The variance of this {.cls glmmTMB} fit's random-effects parameters could not be formed ({.code vcov(full = TRUE)}: {why}).",
        "i" = "The random-effects and ancillary rows have no confidence interval."),
      "random-effects and ancillary rows without confidence intervals",
      why = why
    )
    return(NULL)
  }
  V
}

#' @export
tt_regtab_ancillary_rows.glmmTMB <- function(fit, info, level = 0.95, vce = "stata", ...) {
  if (!identical(stats::family(fit)$family, "nbinom2")) return(NULL)
  V <- .rt_tmb_vcov_full(fit)
  pf <- fit$fit$par
  d <- which(names(pf) %in% c("betadisp", "betad"))
  if (length(d) != 1L) return(NULL)
  se <- if (is.null(V) || nrow(V) != length(pf)) NA_real_ else sqrt(V[d, d])
  .rt_lnalpha_row(-unname(pf[d]), se, level)
}

#' @export
tt_regtab_trailing_rows.merMod <- function(fit, info, o, ...) {
  if (isTRUE(o$noreeffects)) return(NULL)
  .rt_re_rows(fit, info, o)
}

#' @export
tt_regtab_trailing_rows.glmmTMB <- function(fit, info, o, ...) {
  if (isTRUE(o$noreeffects)) return(NULL)
  .rt_re_rows(fit, info, o)
}

# ---------------------------------------------------------------------------
# Statistics

# ICC (`regtab.ado:1051-1099`): every random-intercept variance summed,
# over itself plus the residual variance (var(e)) or the family's latent
# level-1 variance; NA with a note for count families.
.rt_re_icc <- function(re, info) {
  if (is.null(re)) return(list(icc = NA_real_, note = FALSE))
  if (isTRUE(info$icc_undefined)) return(list(icc = NA_real_, note = TRUE))
  v <- 0
  found <- FALSE
  for (t in re$terms) {
    j <- match("(Intercept)", t$comps)
    if (!is.na(j)) {
      v <- v + t$Sigma[j, j]
      found <- TRUE
    }
  }
  resid <- if (!is.na(re$resid)) re$resid else info$icc_resid
  if (!found || is.na(resid)) return(list(icc = NA_real_, note = FALSE))
  list(icc = v / (v + resid), note = FALSE)
}

.rt_mixed_stats <- function(fit, info, ll, rank) {
  s <- list(N = stats::nobs(fit), N_sub = NA_real_, ll = ll, rank = rank, aic = NA_real_,
            bic = NA_real_, r2 = NA_real_, r2_p = NA_real_, r2_a = NA_real_,
            groups = NA_real_, qic = NA_real_, icc = NA_real_)
  if (!is.na(ll) && !is.na(rank)) {
    s$aic <- -2 * ll + 2 * rank
    s$bic <- -2 * ll + rank * log(s$N)
  }
  re <- .rt_re_struct(fit)
  if (!is.null(re)) s$groups <- max(re$groups$nlev)
  ic <- .rt_re_icc(re, info)
  s$icc <- ic$icc
  s$icc_note <- ic$note
  s
}

# Stata's e(rank) for mixed and me* models counts the fixed effects and the
# variance parameters (and menbreg's ln alpha), as logLik()'s df does.
#' @export
tt_model_stats.merMod <- function(fit, info, ...) {
  lg <- stats::logLik(fit)
  ll <- as.numeric(lg)
  # nAGQ > 1: lme4's logLik() leaves out the saturated log-likelihood.
  if (inherits(fit, "glmerMod") && as.integer(fit@devcomp$dims[["nAGQ"]]) > 1L) {
    ll <- ll + .rt_glmm_ll_sat(fit)
  }
  .rt_mixed_stats(fit, info, ll, attr(lg, "df"))
}

#' @export
tt_model_stats.glmmTMB <- function(fit, info, ...) {
  lg <- stats::logLik(fit)
  .rt_mixed_stats(fit, info, as.numeric(lg), attr(lg, "df"))
}
