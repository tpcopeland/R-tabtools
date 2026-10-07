# Per-model scale, intercept, and metadata detection for regtab (plan task
# 4.3).
#
# Mirrors Stata tabtools 2.1.8 `regtab.ado:486-702`, which classifies each
# collected estimation command by its command word (`_cmdword`) and options:
# the estimate header (`model_coef`), the null value for `dimnonsig`
# (`model_null`), whether the model is eligible for the automatic
# `nointercept` (`model_auto_noint`), the random-effects family used for
# MOR/MHR and ICC (`model_re_family`, `model_icc_resid`, `model_icc_undef`),
# and whether it is a GEE (`model_is_gee`). Each R class maps to the Stata
# command it estimates:
#
#   lm -> regress             glm -> glm (family/link), logit, probit, poisson
#   negbin (MASS::glm.nb)     -> nbreg
#   coxph                     -> stcox, or stcrreg for a Fine-Gray fit
#   clogit (survival)         -> clogit (OR with or without `or`)
#   crr (cmprsk)              -> stcrreg
#   survreg                   -> streg, time (AFT)
#   multinom                  -> mlogit
#   polr, clm                 -> ologit (logit link) / oprobit
#   zeroinfl, hurdle          -> zip/zinb/churdle (multi-equation)
#   lmerMod, lme (nlme)       -> mixed
#   glmerMod, glmmTMB         -> melogit/mepoisson/menbreg/meprobit/
#                                mecloglog/meglm (or the fixed-effect
#                                command when glmmTMB has no random effects)
#   geeglm                    -> xtgee
#
# Stata facts the goldens pin down (tests/testthat/golden/R*.csv):
# - `streg, time` shows time ratios, exp(b) with exp-transformed CI bounds
#   (R19: "0.98", tabtools 2.1.10+; up to 2.1.9 the TR header sat over the
#   log-time coefficients). R's survreg coefficients are the same AFT
#   log-time coefficients, exponentiated the same way.
# - `xtgee` follows glm's family/link rule since tabtools 2.1.12 (R23,
#   R25i): binomial/logit shows odds ratios and poisson/log incidence rate
#   ratios, exponentiated with the intercept dropped; any other family/link
#   is "Coef." (up to 2.1.11 xtgee was unclassified, "Coef." on the
#   linear-predictor scale whatever its family).
# - `probit` (and glm with another link) is "Coef." and not eligible for the
#   automatic nointercept.
# - `ologit` cutpoints kept with `keepintercept` stay on the raw scale (R16)
#   while the slopes are odds ratios; a kept logit intercept is exponentiated
#   (R14).

#' Model scale and metadata for regtab
#'
#' @param fit A fitted model.
#' @param ... Unused.
#' @return A list with:
#'   * `stata_cmd`: the Stata command this fit corresponds to;
#'   * `effect_scale`: estimate header (`"OR"`, `"HR"`, `"IRR"`, `"RRR"`,
#'     `"SHR"`, `"TR"`, `"Coef."`);
#'   * `exponentiate`: whether estimates and CI bounds are exponentiated;
#'   * `null_value`: the value `dimnonsig` compares CIs with (1 on ratio
#'     scales, including TR, else 0; `regtab.ado:521-575`);
#'   * `auto_nointercept`: eligible for the automatic nointercept;
#'   * `intercept_terms`, `cutpoint_terms`, `ancillary_terms`: coefficient
#'     names nointercept drops (`regtab.ado:1619-1632`);
#'   * `equations`: equation names for multi-equation models (else `NULL`);
#'   * `re_family` (`"none"`, `"variance"`, `"mor"`, `"mhr"`), `icc_resid`
#'     (latent level-1 variance, `NA` when estimated), `icc_undefined`,
#'     `is_gee`, `family`, `link`;
#'   * Phase 7 metadata: `model_id` (the call), `outcome_id` (the dependent
#'     variable), `model_label` (`NULL`; set by the caller).
#' @keywords internal
#' @noRd
tt_model_info <- function(fit, ...) UseMethod("tt_model_info")

# Assemble the info list; unspecified fields take Stata's defaults
# (`regtab.ado:500-514`: Coef., null 0, no eform, no auto-noint).
.mi <- function(fit, stata_cmd, effect_scale = "Coef.", exponentiate = FALSE,
                null_value = 0, auto_nointercept = FALSE, intercept_terms = character(),
                cutpoint_terms = character(), ancillary_terms = character(),
                equations = NULL, re_family = "none", icc_resid = NA_real_,
                icc_undefined = FALSE, is_gee = FALSE, family = NA_character_,
                link = NA_character_) {
  list(
    class = class(fit)[1], stata_cmd = stata_cmd, effect_scale = effect_scale,
    exponentiate = exponentiate, null_value = null_value,
    auto_nointercept = auto_nointercept, intercept_terms = intercept_terms,
    cutpoint_terms = cutpoint_terms, ancillary_terms = ancillary_terms,
    equations = equations, re_family = re_family, icc_resid = icc_resid,
    icc_undefined = icc_undefined, is_gee = is_gee, family = family, link = link,
    model_id = .mi_model_id(fit), outcome_id = .mi_outcome_id(fit), model_label = NULL
  )
}

# Ratio-scale models share null 1, auto-noint eligibility, and
# exponentiation (`_regtab_scale` in regtab.ado 2.1.10+).
.mi_ratio <- function(fit, stata_cmd, scale, exponentiate = TRUE, ...) {
  .mi(fit, stata_cmd, effect_scale = scale, exponentiate = exponentiate,
      null_value = 1, auto_nointercept = TRUE, ...)
}

.mi_intercept <- function(names) intersect("(Intercept)", names)

.mi_model_id <- function(fit) {
  cl <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  if (is.null(cl) && is.list(fit)) cl <- fit$call
  if (is.null(cl)) return(NA_character_)
  paste(trimws(deparse(cl, width.cutoff = 500L)), collapse = " ")
}

# Case-preserved dependent variable identity; Stata's collected `depvar`
# (`regtab.ado:455`). For a Surv() response Stata's depvar is `_t` for every
# st model, which cannot identify an outcome; R uses the event variable,
# which is what Phase 7 needs to join rate tables to models.
#
# The event is found by Surv()'s own argument matching (Codex audit R4: the
# last argument was taken, so `Surv(time, status, type = "right")` gave the
# outcome "\"right\""): `event` when given (the counting-process
# `Surv(start, stop, event)`), else `time2`, which is where R matches the
# event of the right-censored shorthand `Surv(time, status)`; `type` and
# `origin` never name the outcome. Named, partial and reordered arguments
# match as in Surv().
.mi_surv_args <- function(time, time2, event, type, origin) NULL

.mi_outcome_id <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f) || length(f) < 3L) return(NA_character_)
  lhs <- f[[2L]]
  if (is.call(lhs) && identical(as.character(lhs[[1L]]), "Surv") ||
      is.call(lhs) && identical(deparse(lhs[[1L]]), "survival::Surv")) {
    cl <- lhs
    cl[[1L]] <- as.name(".mi_surv_args")
    m <- tryCatch(match.call(.mi_surv_args, cl), error = function(e) NULL)
    if (is.null(m)) {
      lhs <- lhs[[length(lhs)]]
    } else {
      arg <- function(a) if (a %in% names(m)) m[[a]] else NULL
      type <- arg("type")
      interval <- is.character(type) && type %in% c("interval", "interval2")
      if (!interval) lhs <- arg("event") %||% arg("time2") %||% arg("time") %||% lhs
    }
  }
  paste(deparse(lhs, width.cutoff = 500L), collapse = "")
}

#' @export
tt_model_info.default <- function(fit, ...) {
  nm <- tryCatch(names(stats::coef(fit)), error = function(e) character())
  .mi(fit, "unknown", intercept_terms = .mi_intercept(nm))
}

#' @export
tt_model_info.lm <- function(fit, ...) {
  .mi(fit, "regress", intercept_terms = .mi_intercept(names(stats::coef(fit))))
}

# glm: Stata classifies logit/logistic and poisson commands and glm with
# family(binomial|bernoulli) link(logit) or family(poisson) link(log); every
# other family/link is "Coef." (`regtab.ado:521-531`, `:537-543`, `:634-659`).
# R's quasibinomial/quasipoisson are the same means with a free dispersion
# (Stata's glm scale() option), so they classify like their base families.
.mi_glm_scale <- function(family, link) {
  if (family %in% c("binomial", "quasibinomial") && link == "logit") return(c("logit", "OR"))
  if (family %in% c("poisson", "quasipoisson") && link == "log") return(c("poisson", "IRR"))
  if (family %in% c("binomial", "quasibinomial") && link == "probit") return(c("probit", "Coef."))
  c("glm", "Coef.")
}

#' @export
tt_model_info.glm <- function(fit, ...) {
  fam <- fit$family$family
  link <- fit$family$link
  sc <- .mi_glm_scale(fam, link)
  icpt <- .mi_intercept(names(stats::coef(fit)))
  if (sc[2] == "Coef.") {
    return(.mi(fit, sc[1], intercept_terms = icpt, family = fam, link = link))
  }
  .mi_ratio(fit, sc[1], sc[2], intercept_terms = icpt, family = fam, link = link)
}

# MASS::glm.nb <-> nbreg (IRR, `regtab.ado:537-543`); its theta is not a
# coefficient: the lnalpha/alpha rows come from
# tt_regtab_ancillary_rows.negbin() (R/regtab_tidy.R). nbreg has only the
# log link; glm.nb(link = identity/sqrt) corresponds to Stata's `glm,
# family(nbinomial ml) link(identity)`, whose coefficients are not log rate
# ratios and are shown as they are, "Coef." (codex audit F03). Probed
# (muse M4, 2026-09-28): that command shows no /lnalpha or alpha row,
# counts only the coefficients in e(rank), and treats alpha, taken from
# nbreg, as fixed; regtab follows it (.rt_nb_glm(), R/regtab_tidy.R), with
# glm.nb's jointly estimated theta, so estimates agree to ~1e-5 unweighted,
# not exactly, and less closely with weights (B09: 3% on an SE with
# [iw=turn] on auto).
#' @export
tt_model_info.negbin <- function(fit, ...) {
  link <- fit$family$link %||% "log"
  icpt <- .mi_intercept(names(stats::coef(fit)))
  if (!identical(link, "log")) return(.mi(fit, "glm", intercept_terms = icpt, family = "negbin", link = link))
  .mi_ratio(fit, "nbreg", "IRR", intercept_terms = icpt, family = "negbin", link = link)
}

# Fine-Gray: a weighted counting-process coxph on survival::finegray()
# data is the stcrreg analogue (SHR, `regtab.ado:556-560`); anything else is
# stcox (HR, `:561-565`). Detected from the structure, never from column
# names (external review F25): regtab(finegray = TRUE/FALSE) tags the fit
# (fit$tt_finegray), else the fit is weighted, its response is
# Surv(start, stop, status), and the data it names carry finegray()'s mark,
# the "event" attribute of its output (kept by `[` and by renaming
# columns). An unweighted fit on finegray() data is a Cox model.
.mi_is_finegray <- function(fit) {
  if (!identical(class(fit)[1], "coxph")) return(FALSE)
  if (!is.null(fit$tt_finegray)) return(isTRUE(fit$tt_finegray))
  .rt_fg_structure(fit) && .rt_fg_marked(fit)
}

# A weighted coxph fit with a Surv(start, stop, status) response.
.rt_fg_structure <- function(fit) {
  if (is.null(fit$weights) && is.null(fit$call$weights)) return(FALSE)
  if (!is.null(fit$y)) return(identical(attr(fit$y, "type"), "counting"))
  lhs <- tryCatch(stats::formula(fit)[[2L]], error = function(e) NULL)
  is.call(lhs) && length(lhs) == 4L
}

# The data the call names carry finegray()'s "event" attribute.
.rt_fg_marked <- function(fit) {
  d <- .rt_call_data(fit)
  ev <- if (is.data.frame(d)) attr(d, "event", exact = TRUE) else NULL
  is.character(ev) && length(ev) == 1L
}

#' @export
tt_model_info.coxph <- function(fit, ...) {
  if (.mi_is_finegray(fit)) return(.mi_ratio(fit, "stcrreg", "SHR"))
  .mi_ratio(fit, "stcox", "HR")
}

#' @export
tt_model_info.crr <- function(fit, ...) .mi_ratio(fit, "stcrreg", "SHR")

# survival::clogit is Stata's `clogit` (task 5.15): odds ratios, as glm's
# binomial family is `logistic`. Stata tabtools 2.1.14 classifies clogit
# like logit (`_regtab_scale`, regtab.ado:4641): header OR, null 1 for
# dimnonsig, eligible for the automatic nointercept (a logistic beside it
# loses its intercept row, golden R76), and a fit without `or` is shown
# exponentiated too (golden R83). Up to 2.1.13 clogit was unclassified.
#' @export
tt_model_info.clogit <- function(fit, ...) .mi_ratio(fit, "clogit", "OR")

# survreg by distribution (Phase 5 review P0-8). The log-time
# distributions are streg, time: TR, null 1, auto-noint, exponentiated
# (golden R19, tabtools 2.1.10+ `_regtab_scale`: the time metric without
# `tr` is exponentiated to reach the TR header); Log(scale) is the
# ancillary parameter (Stata /ln_p etc., `:1628-1629`), never exponentiated. A Gaussian survreg is Stata's intreg (and an
# AER::tobit() fit, class "tobit", Stata's tobit): neither is classified by
# regtab (`regtab.ado:486-660` has no branch), so Coef., null 0, intercept
# kept (probes B04, B04b, G11-G15).
.rt_sr_logtime <- c("weibull", "exponential", "lognormal", "loglogistic")

#' @export
tt_model_info.survreg <- function(fit, ...) {
  anc <- grep("^Log\\(scale(\\[[^]]*\\])?\\)$", rownames(stats::vcov(fit)), value = TRUE)
  icpt <- .mi_intercept(names(stats::coef(fit)))
  if (identical(fit$dist, "gaussian")) {
    cmd <- if (inherits(fit, "tobit")) "tobit" else "intreg"
    return(.mi(fit, cmd, intercept_terms = icpt, ancillary_terms = anc, family = "gaussian"))
  }
  # Only the log-time distributions are streg's; the classifier refuses the
  # rest itself (muse P1-9) rather than rely on tt_regtab_check.survreg()
  # having run first.
  if (!is.character(fit$dist) || length(fit$dist) != 1L || !fit$dist %in% .rt_sr_logtime) {
    cli::cli_abort(c(
      "A {.fn survival::survreg} fit with the {.val {format(fit$dist)}} distribution has no Stata equivalent.",
      "i" = "Supported: {.val {c(.rt_sr_logtime, 'gaussian')}}."
    ), call = NULL)
  }
  response <- fit$y
  if (is.null(response)) {
    frame <- fit$model %||% fit$tt_frame
    if (is.data.frame(frame)) response <- stats::model.response(frame)
  }
  type <- attr(response, "type", exact = TRUE)
  interval <- isTRUE(type %in% c("interval", "interval2"))
  info <- .mi_ratio(fit, if (interval) "stintreg" else "streg", "TR", intercept_terms = icpt, ancillary_terms = anc)
  info$distribution <- fit$dist
  info$metric <- "log_time"
  info$response_type <- type %||% NA_character_
  info$provenance <- if (interval) "fitted_interval_censored_AFT" else "fitted_AFT"
  info
}

# mlogit: RRR per non-base outcome equation (`regtab.ado:544-550`).
#' @export
tt_model_info.multinom <- function(fit, ...) {
  cf <- stats::coef(fit)
  eq <- if (is.matrix(cf)) rownames(cf) else fit$lev[2L]
  nm <- if (is.matrix(cf)) colnames(cf) else names(cf)
  .mi_ratio(fit, "mlogit", "RRR", intercept_terms = .mi_intercept(nm), equations = eq)
}

# Stata command of a non-logit ordinal fit: oprobit for the probit link only;
# cloglog, loglog, cauchit, ... have no Stata ordered command (NA; the link
# is recorded in info$link and footnoted).
.mi_ordinal_cmd <- function(link) if (identical(as.character(link), "probit")) "oprobit" else NA_character_

# ologit: OR with cutpoints; oprobit (probit link only) has Coef.; other links are unclassified.
#' @export
tt_model_info.polr <- function(fit, ...) {
  cut <- names(fit$zeta)
  if (identical(fit$method, "logistic")) {
    return(.mi_ratio(fit, "ologit", "OR", cutpoint_terms = cut, link = "logit"))
  }
  .mi(fit, .mi_ordinal_cmd(fit$method), cutpoint_terms = cut, link = fit$method)
}

#' @export
tt_model_info.clm <- function(fit, ...) {
  cut <- names(fit$alpha)
  if (identical(fit$link, "logit")) {
    return(.mi_ratio(fit, "ologit", "OR", cutpoint_terms = cut, link = "logit"))
  }
  .mi(fit, .mi_ordinal_cmd(fit$link), cutpoint_terms = cut, link = fit$link)
}

# zip/zinb/churdle: "Coef." on the native scale, auto-noint, multi-equation
# (`regtab.ado:551-555`). Coefficient names carry the equation prefix
# (count_, zero_); negbin's Log(theta) is ancillary.
.mi_twopart <- function(fit, stata_cmd) {
  nm <- rownames(stats::vcov(fit))
  icpt <- grep("_\\(Intercept\\)$", nm, value = TRUE)
  # theta is not in vcov(); summary() reports it as "Log(theta)".
  anc <- if (identical(fit$dist, "negbin")) "Log(theta)" else character()
  .mi(fit, stata_cmd, auto_nointercept = TRUE, intercept_terms = icpt, ancillary_terms = anc,
      equations = c("count", "zero"), family = fit$dist)
}

#' @export
tt_model_info.zeroinfl <- function(fit, ...) {
  .mi_twopart(fit, if (identical(fit$dist, "poisson")) "zip" else "zinb")
}

#' @export
tt_model_info.hurdle <- function(fit, ...) .mi_twopart(fit, "churdle")

# Mixed models (`regtab.ado:532-536`, `:566-633`).
.mi_mixed_scale <- function(fit, family, link, icpt, has_re = TRUE) {
  nb <- grepl("^(Negative Binomial|nbinom)", family)
  if (!has_re) {
    if (nb && link == "log") return(.mi_ratio(fit, "nbreg", "IRR", intercept_terms = icpt, family = family, link = link))
    sc <- .mi_glm_scale(family, link)
    if (sc[2] == "Coef.") return(.mi(fit, if (family == "gaussian") "regress" else sc[1], intercept_terms = icpt, family = family, link = link))
    return(.mi_ratio(fit, sc[1], sc[2], intercept_terms = icpt, family = family, link = link))
  }
  if (family == "gaussian" && link == "identity") {
    return(.mi(fit, "mixed", intercept_terms = icpt, re_family = "variance", family = family, link = link))
  }
  if (family == "binomial" && link == "logit") {
    return(.mi_ratio(fit, "melogit", "OR", intercept_terms = icpt, re_family = "mor",
                     icc_resid = pi^2 / 3, family = family, link = link))
  }
  if (family == "poisson" && link == "log") {
    return(.mi_ratio(fit, "mepoisson", "IRR", intercept_terms = icpt, re_family = "variance",
                     icc_undefined = TRUE, family = family, link = link))
  }
  if (nb && link == "log") {
    return(.mi_ratio(fit, "menbreg", "IRR", intercept_terms = icpt, re_family = "variance",
                     icc_undefined = TRUE, family = family, link = link))
  }
  if (family == "binomial" && link == "cloglog") {
    return(.mi_ratio(fit, "mecloglog", "HR", intercept_terms = icpt, re_family = "mhr",
                     icc_resid = pi^2 / 6, family = family, link = link))
  }
  if (family == "binomial" && link == "probit") {
    return(.mi(fit, "meprobit", intercept_terms = icpt, re_family = "variance", icc_resid = 1,
               family = family, link = link))
  }
  # meglm with any other family: variance-scale random effects, ICC undefined
  # outside the Gaussian family (`regtab.ado:607-632`).
  .mi(fit, "meglm", intercept_terms = icpt, re_family = "variance",
      icc_undefined = !(family %in% c("gaussian")), family = family, link = link)
}

#' @export
tt_model_info.lmerMod <- function(fit, ...) {
  icpt <- .mi_intercept(names(lme4::fixef(fit)))
  .mi_mixed_scale(fit, "gaussian", "identity", icpt)
}

# nlme::lme (task 5.19): Stata `mixed`, as an lmer fit.
#' @export
tt_model_info.lme <- function(fit, ...) {
  icpt <- .mi_intercept(names(nlme::fixef(fit)))
  .mi_mixed_scale(fit, "gaussian", "identity", icpt)
}

#' @export
tt_model_info.glmerMod <- function(fit, ...) {
  fam <- stats::family(fit)
  icpt <- .mi_intercept(names(lme4::fixef(fit)))
  .mi_mixed_scale(fit, fam$family, fam$link, icpt)
}

#' @export
tt_model_info.glmmTMB <- function(fit, ...) {
  fam <- stats::family(fit)
  cf <- glmmTMB::fixef(fit)$cond
  icpt <- .mi_intercept(names(cf))
  has_re <- length(fit$modelInfo$reTrms$cond$flist) > 0L
  info <- .mi_mixed_scale(fit, fam$family, fam$link, icpt, has_re)
  nm <- rownames(stats::vcov(fit, full = TRUE))
  info$ancillary_terms <- grep("^(disp~|zi~|d~)", nm, value = TRUE)
  info
}

# xtgee: since tabtools 2.1.12 classified by glm's family/link rule
# (`_regtab_scale`, the glm/xtgee branch): binomial/logit OR and
# poisson/log IRR, exponentiated, with the automatic nointercept; every
# other family/link "Coef." (goldens R23, R25i; fixtures gee_pois, D13,
# E06). R's geeglm has no eform option, so binomial/log (RR under eform)
# and nbinomial/log (IRR under eform) stay "Coef." as in Stata without it.
#' @export
tt_model_info.geeglm <- function(fit, ...) {
  # gee_as = "glm" (task 5.13): Stata's glm [pw], vce(cluster id), which
  # regtab classifies like any glm.
  if (identical(fit$tt_gee_as, "glm")) {
    fam <- fit$family$family
    link <- fit$family$link
    sc <- .mi_glm_scale(fam, link)
    icpt <- .mi_intercept(names(stats::coef(fit)))
    if (sc[2] == "Coef.") return(.mi(fit, sc[1], intercept_terms = icpt, family = fam, link = link))
    return(.mi_ratio(fit, sc[1], sc[2], intercept_terms = icpt, family = fam, link = link))
  }
  fam <- fit$family$family
  link <- fit$family$link
  sc <- .mi_glm_scale(fam, link)
  icpt <- .mi_intercept(names(stats::coef(fit)))
  if (sc[2] %in% c("OR", "IRR")) {
    return(.mi_ratio(fit, "xtgee", sc[2], intercept_terms = icpt, is_gee = TRUE, family = fam, link = link))
  }
  .mi(fit, "xtgee", intercept_terms = icpt, is_gee = TRUE, family = fam, link = link)
}

# ---------------------------------------------------------------------------
# Across models

#' Estimate headers, the stored coef_label, and the automatic nointercept
#'
#' `regtab.ado:305-309` (cdisc), `:420`, `:664-701`, `:1916-1922`: with no
#' user `coef` and no `cdisc`, each model block's row-3 header is its own
#' scale; the shared header and `coef_label` are that scale when all models
#' agree, else `"Estimate"` / `"mixed"`. `cdisc` without `coef` makes the
#' header and label `"Estimate"`. A user `coef` is used for every block. The
#' intercept is dropped automatically only when every model is eligible, the
#' user gave neither `nointercept` nor `keepintercept`.
#' @param infos List of `tt_model_info()` results.
#' @param coef User estimate header or `NULL`.
#' @param nointercept `TRUE`/`FALSE` if the user specified it, else `NULL`.
#' @return list(headers, coef, coef_label, mixed, nointercept).
#' @keywords internal
#' @noRd
tt_models_scale <- function(infos, coef = NULL, cdisc = FALSE, nointercept = NULL,
                            keepintercept = FALSE) {
  scales <- unname(vapply(infos, `[[`, "", "effect_scale"))
  mixed <- length(unique(scales)) > 1L
  if (!is.null(coef)) {
    headers <- rep(coef, length(infos))
    shared <- coef
    label <- coef
  } else if (isTRUE(cdisc)) {
    headers <- rep("Estimate", length(infos))
    shared <- "Estimate"
    label <- "Estimate"
  } else {
    headers <- scales
    shared <- if (mixed) "Estimate" else scales[1]
    label <- if (mixed) "mixed" else scales[1]
  }
  auto <- all(vapply(infos, `[[`, TRUE, "auto_nointercept"))
  noint <- if (!is.null(nointercept)) isTRUE(nointercept) else !isTRUE(keepintercept) && auto
  list(headers = headers, coef = shared, coef_label = label, mixed = mixed, nointercept = noint)
}

#' Stata's nointercept rule, by displayed label (`regtab.ado:1619-1632`)
#'
#' Stata matches the lowercased, trimmed row label: intercepts (`intercept`,
#' `_cons`, `constant`), cutpoints (`cut1`, `/cut1`), ancillary parameters
#' (anything starting with `/`, `alpha`, `lnalpha`, `ln_p`, `p`, `1/p`), and
#' rows starting `ancillary:` or `scale:`, each also after an equation prefix
#' (`"Inflation equation: _cons"`). Stata's rule drops a row whose label is
#' exactly `p` or `alpha` even when it is a real covariate, so fitted models
#' no longer use it (Milestone H task H5: rows carry a structural `role`,
#' .rt_row()); it remains the rule for data-frame input, which carries no
#' coefficient structure, and documents what Stata drops.
#' @keywords internal
#' @noRd
tt_noint_row <- function(labels) {
  x <- tolower(trimws(labels))
  grepl("^(intercept|_cons|constant)$", x) |
    grepl(": +(intercept|_cons|constant)$", x) |
    grepl("^/?cut[0-9]+$", x) |
    grepl(": +/?cut[0-9]+$", x) |
    grepl("^(/|alpha$|lnalpha$|ln_p$|p$|1/p$)", x) |
    grepl(": +(/|alpha|lnalpha|ln_p|p|1/p)$", x) |
    grepl("^ancillary:", x) |
    grepl("^scale:", x)
}

#' The regtab methods sentence, from model metadata (task H11, H-D3)
#'
#' "<Estimates> with <level>% confidence intervals from <univariable |
#' multivariable> <model>", then " across <k> models" for several models,
#' and a full stop. The estimates are named by
#' each model's effect scale and the model by its Stata command, family and
#' link (`.rt_methods_model()`, `R/regtab_methods.R`), never by the header,
#' so a `coef`/`cdisc` relabel does not rename them; "univariable" means one
#' predictor variable (`.rt_n_predictors()`), and an intercept-only model
#' gets no adjective. Models on different effect scales give Stata's
#' "Collected regression estimates ... across k models."; models on one
#' scale but of different kinds list their nouns ("linear regression and
#' probit regression"). Stata (`regtab.ado:2866-2915`) picks the sentence
#' from the header instead; its text is pinned from
#' `qa/stata/make_regtab_methods.do` in `test-regtab-hardening.R`. As in
#' Stata, the stars sentence follows when `stars` (user `starslevels` as
#' Stata's numlist prints them, `.1`; the defaults as `0.05 0.01 0.001`),
#' and Stata's software sentence is left out.
#' @param infos List of `tt_model_info()` results, one per model.
#' @param n_predictors Predictor count per model (`.rt_n_predictors()`).
#' @param level Confidence level in (0, 1).
#' @param ci_method `"wald"` or `"profile"`: profile intervals (R only) are
#'   named "profile-likelihood" and the p-values, which stay Wald, are said
#'   to be so (muse P1-2); an `lm`'s profile intervals are its t intervals,
#'   so a table of `lm` fits only is not flagged.
#' @keywords internal
#' @noRd
tt_regtab_methods <- function(infos, n_predictors = rep(2L, length(infos)), level = 0.95,
                              stars = FALSE, starslevels = NULL, ci_method = "wald") {
  ci <- .rt_pct_text(level)
  profile <- identical(ci_method, "profile") &&
    any(vapply(infos, function(x) !identical(x$class, "lm"), TRUE))
  if (profile) ci <- paste0(ci, "% profile-likelihood")
  else ci <- paste0(ci, "%")
  k <- length(infos)
  scales <- unname(vapply(infos, `[[`, "", "effect_scale"))
  if (length(unique(scales)) > 1L) {
    out <- paste0("Collected regression estimates with ", ci, " confidence intervals across ", k, " models.")
  } else {
    adj <- .rt_methods_adjective(n_predictors)
    model <- .rt_methods_join(vapply(infos, .rt_methods_model, ""))
    multi <- if (k > 1L) paste0(" across ", k, " models") else ""
    out <- paste0(.rt_methods_estimates(scales[1]), " with ", ci, " confidence intervals from ",
                  if (nzchar(adj)) paste0(adj, " "), model, multi, ".")
  }
  if (profile) out <- sub("\\.$", "; p-values from Wald tests.", out)
  if (isTRUE(stars)) {
    sl <- if (is.null(starslevels)) c("0.05", "0.01", "0.001") else stata_fmt(starslevels, "%18.0g")
    out <- paste0(out, " Statistical significance denoted as * p<", sl[1], ", ** p<", sl[2],
                  ", *** p<", sl[3], ".")
  }
  out
}
