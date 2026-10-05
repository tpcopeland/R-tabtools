# Extension points for regtab model families (Phase 5). Every generic is
# defined here, once. A default method that is the Phase 4 code lives next
# to that code in R/regtab_tidy.R (tt_regtab_rows.default,
# tt_vcov.default, tt_wald_df.default, tt_model_stats.default); the
# trivial defaults are below. A model family adds S3 methods in its own
# R/regtab_models_*.R (or R/regtab_escape.R) file instead of editing
# shared if/else chains:
#
#   tt_regtab_adapter(fit, ...)          TRUE: regtab() supports the class
#                                        through the methods below (the one
#                                        gate; default FALSE)
#   tt_coef(fit, ...)                    named coefficients the Wald rows
#                                        cover (default coef(); glmmTMB's
#                                        conditional model)
#   tt_vcov(fit, vce, cluster, ...)      their variance (exported; see its
#                                        help page and R/regtab_vce.R)
#   tt_wald_df(fit, vce, cluster, ...)   Wald reference distribution
#                                        (Inf = normal)
#   tt_regtab_rows(fit, info, ...)       rows of one model (key, block,
#                                        kind, label, status, numbers)
#   tt_regtab_ancillary_rows(fit, info, ...)
#                                        rows of the model's own equation
#                                        after its coefficient rows
#   tt_regtab_trailing_rows(fit, info, o, ...)
#                                        row blocks after every coefficient
#                                        row of every model
#   tt_model_stats(fit, info, ...)       N, N_sub, ll, rank, r2, ...
#
# Two row hooks, because Stata places the two kinds of row differently
# (goldens R16-R22, fixtures regtab_phase5a/weibull_keep.csv and
# regtab_phase5b/slope_raw.csv):
# - Ancillary rows (ologit /cut, streg ln_p/p/1/p, lnsigma/sigma,
#   lngamma/gamma) belong to the model's equation: tt_regtab_rows.default
#   appends them to the model's rows, so they enter the coefficient union,
#   which puts the intercept last, i.e. after them (streg: covariates,
#   ln_p, p, 1/p, Intercept). They obey nointercept/keepintercept through
#   their structural `role` (.rt_row(); Milestone H task H5, never their
#   label) and cutlabels. Multi-equation
#   models (zinb's Ancillary: lnalpha) build theirs inside their own
#   tt_regtab_rows() method.
# - Trailing rows (random effects: var(_cons), cov(), var(e), MOR) follow
#   every coefficient row, intercept included (`regtab.ado:1638-1644`).
#   tt_regtab_build() unions them separately and appends that union below
#   the coefficient union (.rt_union_append()); they are exempt from
#   nointercept and cutlabels, and a row of kind "re" gets row type "re"
#   (a top border above the first in the workbook).
#
# Multi-equation models key their rows "<equation>::<colname>": the
# equation keeps rows of different equations apart in the multi-model
# union, while keep()/drop() match the colname alone, as Stata's raw key
# is the colname in its coleq#colname layout
# (`_tabtools_collect_render.ado:804`, via .rt_match_key()).

#' Whether regtab() supports a model class through Phase 5 methods
#'
#' The single gate for Phase 5 classes: `TRUE` exempts the class from the
#' Phase 5 stop (`.rt_phase5_classes`), from the lm/glm subclass refusal
#' (geeglm inherits from glm), and from the coef()/vcov() check (S4 fits
#' and crr have no `$call` that check can rely on). Every class with a
#' family-specific method of a
#' generic above declares it (checked in test-regtab-generics.R).
#' @keywords internal
#' @noRd
tt_regtab_adapter <- function(fit, ...) UseMethod("tt_regtab_adapter")

#' @export
tt_regtab_adapter.default <- function(fit, ...) FALSE

#' @keywords internal
#' @noRd
tt_coef <- function(fit, ...) UseMethod("tt_coef")

#' @export
tt_coef.default <- function(fit, ...) {
  b <- stats::coef(fit)
  if (is.list(b) && !is.null(b$cond)) b <- b$cond
  unlist(b)
}

#' Variance of a model's coefficients, as regtab reports it
#'
#' The variance-covariance matrix [regtab()] uses for a model's standard
#' errors, confidence intervals and p-values, exported so the same matrix
#' can feed other tools, e.g. `marginaleffects::avg_comparisons(fit, vcov =
#' tt_vcov(fit, "robust"), wts = w)` reproduces Stata's `margins` after a
#' `[pweight]` fit (pass the estimation weights as `wts`, since `margins`
#' averages with them).
#'
#' `vce` values (see [tt_vce_types()] for the ones a class supports):
#' * `"stata"` (default): the default variance of the Stata command the
#'   class stands for: `vce(oim)` for `glm` (logit, probit, poisson, glm)
#'   and ologit/mlogit/zip/nbreg, `regress`'s OLS variance for `lm`, the GLS
#'   block for `mixed`, the inverse observed information for me*, xtgee's
#'   model-based variance for `geeglm` (with `gee_as = "glm"`, the
#'   cluster-robust variance of `glm [pweight], vce(cluster id)`),
#'   stcrreg's clustered sandwich times N/(N - 1), robust `coxph`/`survreg`
#'   fits times G/(G - 1), `survey::svyglm`'s design-based variance, and
#'   `WeightIt::glm_weightit`'s own M-estimation variance.
#' * `"model"`: the fit's own `vcov()`.
#' * `"robust"`: Stata's `vce(robust)`, which a `[pweight]` fit also
#'   reports: for `glm` ML fits the sandwich with the observed-information
#'   bread times N/(N - 1) (`glm.nb`: over the coefficients and ln(alpha)
#'   jointly, as `nbreg`); for `lm` HC1 (times N/(N - k)); for `coxph` the
#'   grouped dfbeta sandwich (clusters from `cluster()`/`id`, else one per
#'   observation) times G/(G - 1); for `geeglm` the GEE sandwich times
#'   G/(G - 1) (xtgee `vce(robust)`, also the `"stata"` variance of a
#'   weighted `geeglm`, since `xtgee [pweight]` forces it).
#' * `"cluster"`: Stata's `vce(cluster c)`, scores summed within the
#'   clusters of `cluster`: `glm` and `coxph` times G/(G - 1), `lm` times
#'   G/(G - 1) (N - 1)/(N - k). N and G count observations with a positive
#'   weight.
#'
#' Reference distributions (regtab's p-values and intervals): normal for
#' every class except `lm` (t with N - k degrees of freedom, G - 1 under
#' `vce = "cluster"`) and `svyglm` (t with the design degrees of freedom,
#' `survey::degf()`).
#' A `glm` whose `converged` flag is `FALSE` is refused with
#' `tabtools_error_model_convergence`. Explicit failed optimizer diagnostics
#' from mixed, count, multinomial, ordinal and GEE fits are also refused.
#' Unusable mixed-model Hessians use `tabtools_error_model_hessian`, and
#' unusable active `clm` covariance uses `tabtools_error_model_covariance`.
#' The checks also apply before a supplied variance can bypass a model's
#' covariance method. Valid random-effect boundaries and gradient advisories
#' alone remain supported. Stored Cox and `survreg` diagnostics establish
#' only some failures; see [regtab()] for their limits.
#' Robust `lm` variances require
#' positive residual degrees of freedom among positive-weight observations;
#' robust `glm` variances require at least two such observations. An
#' insufficient sample is refused with `tabtools_error_vce_sample_size`.
#'
#' @param fit A fitted model of a class [regtab()] supports.
#' @param vce One of `"stata"`, `"model"`, `"robust"`, `"cluster"`; values
#'   outside `tt_vce_types(fit)` are an error naming the class.
#' @param cluster For `vce = "cluster"` only: a one-sided formula naming one
#'   variable (`~id`), a column name (`"id"`), or a vector with one value per
#'   observation, in the order of the fitted rows. A variable is read from
#'   the model frame, `glm`'s stored data, or the data as they are now only
#'   when they reproduce the fit (see "Rows the variance is computed
#'   from"). `coxph` fits default to their own clusters (`cluster()`/`id`);
#'   a `geeglm` fit always clusters on its GEE `id` (leave `cluster`
#'   empty).
#' @param ... Class-specific options: `gee_as = "glm"` for `geeglm`;
#'   internally `full = TRUE` (the whole parameter vector of
#'   zero-inflated, hurdle and mixed models).
#' @param complete `TRUE` (default): a row and column for every
#'   coefficient, `NA` for aliased (collinear) ones, as [stats::vcov()]
#'   returns them. `FALSE` drops the aliased rows and columns, which is what
#'   `marginaleffects` expects (with `NA` rows its standard errors are `NA`).
#' @return A square numeric matrix with row and column names covering the
#'   model's coefficients (`coef()` names; `NA` rows and columns for
#'   aliased coefficients of `lm`/`glm` fits unless `complete = FALSE`), on
#'   the scale of the linear predictor.
#'
#' @section Rows the variance is computed from:
#' A robust or cluster-robust variance pairs each observation's score with
#' its cluster, so it is computed from the rows the model was fitted on:
#' the model frame the fit keeps (`lm`, `glm`, or any fit with
#' `model = TRUE`), `glm`'s stored data, or the data object as it is now
#' when its model frame reproduces the fit exactly (its response, weights
#' and linear predictor, or the stored frame column for column). A `coxph`
#' or `survreg` fit keeps no data by default, so if its data frame is
#' re-sorted, filtered or edited after fitting, a variance that needs the
#' rows (a cluster variable, the grouped dfbeta sandwich, the clusters of
#' a `cluster()` term) is an error: refit on the current data or with
#' `model = TRUE`. A cluster variable that is not in the model frame is
#' read the same way; a vector must be in the order of the fitted
#' observations. A robust `coxph` fit's own sandwich (weighted, `id`,
#' `cluster()`) is stored at fit time and needs no data.
#'
#' After a weighted fit, pass the estimation weights to `marginaleffects`
#' as `wts`: `tt_vcov()` returns the variance only, not the weights.
#' @seealso [regtab()], [tt_vce_types()]
#' @examples
#' d <- data.frame(y = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 1),
#'                 x = c(1, 2, 2, 3, 4, 1, 5, 3, 2, 4),
#'                 w = c(1.2, 0.8, 2.5, 1.1, 0.9, 1.7, 1.3, 0.6, 2.2, 1.0),
#'                 g = rep(1:5, each = 2))
#' fit <- suppressWarnings(glm(y ~ x, family = binomial, data = d, weights = w))
#' tt_vcov(fit, "robust")              # logit [pw=w]
#' tt_vcov(fit, "cluster", cluster = ~g)  # logit [pw=w], vce(cluster g)
#' tt_vce_types(fit)
#' @export
tt_vcov <- function(fit, vce = "stata", cluster = NULL, ..., complete = TRUE) {
  .rt_check_fit_diagnostics(fit)
  if (identical(vce, "user")) {
    # regtab()'s user-supplied variance (task 5.18), attached to the fit.
    V <- .rt_user_vce_of(fit)$V
    if (isTRUE(complete)) return(V)
    keep <- !is.na(diag(V))
    return(V[keep, keep, drop = FALSE])
  }
  .rt_check_vce(fit, vce, cluster)
  if (!is.logical(complete) || length(complete) != 1L || is.na(complete)) {
    cli::cli_abort("{.arg complete} must be TRUE or FALSE.", call = NULL)
  }
  if (!complete) {
    V <- tt_vcov(fit, vce, cluster, ...)
    keep <- !is.na(diag(V))
    return(V[keep, keep, drop = FALSE])
  }
  UseMethod("tt_vcov")
}

# Apply stored diagnostic guards before a user-supplied covariance can bypass
# a class-specific method. Unknown diagnostic status is not a failure flag.
.rt_check_fit_diagnostics <- function(fit, i = 1L) {
  if (inherits(fit, "glm")) .rt_check_glm_convergence(fit, i)
  if (inherits(fit, "merMod") || inherits(fit, "glmmTMB")) .rt_check_mixed_convergence(fit, i)
  if (inherits(fit, c("zeroinfl", "hurdle"))) .rt_check_twopart_convergence(fit, i)
  if (inherits(fit, "multinom")) .rt_check_multinom_convergence(fit, i)
  if (inherits(fit, c("polr", "clm"))) .rt_check_ordinal_convergence(fit, i)
  if (inherits(fit, "geeglm")) .rt_check_gee_convergence(fit, i)
  if (inherits(fit, c("coxph", "survreg"))) .rt_check_survival_convergence(fit, i)
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
tt_wald_df <- function(fit, vce = "stata", cluster = NULL, ...) {
  if (identical(vce, "user")) return(.rt_user_vce_of(fit)$df)
  .rt_check_vce(fit, vce, cluster)
  UseMethod("tt_wald_df")
}

#' Variance types a model class supports
#'
#' The `vce` values [tt_vcov()] and [regtab()] accept for a fit. Every class
#' supports `"stata"` and `"model"` (a data frame, which carries its own
#' standard errors, only `"stata"`); `lm`, `glm` and `MASS::glm.nb` fits
#' (exactly those classes, not subclasses), `survival::coxph` (except
#' Fine-Gray fits on `finegray()` data, whose variance is `stcrreg`'s) and
#' `geepack::geeglm` also support `"robust"` and `"cluster"`. For a
#' `geeglm` fit both are the GEE sandwich clustered by its own `id` (so
#' `cluster` must be left empty). Any other value is refused with an error
#' naming the class, never silently mapped to another variance.
#' @param fit A fitted model.
#' @return A character vector.
#' @seealso [tt_vcov()], [tt_ci_methods()]
#' @examples
#' tt_vce_types(lm(mpg ~ wt, data = mtcars))
#' @export
tt_vce_types <- function(fit) UseMethod("tt_vce_types")

#' @export
tt_vce_types.default <- function(fit) c("stata", "model")

#' Confidence-interval methods a model class supports
#'
#' The `ci_method` values [regtab()] accepts for a fit, beside
#' [tt_vce_types()] for `vce`. Every class supports `"wald"` (Stata's
#' intervals). `"profile"` (profile-likelihood intervals from
#' [stats::confint()]) is available for `lm`, `glm` and `MASS::glm.nb`
#' fits, exactly those classes (not subclasses such as `survey::svyglm` or
#' `WeightIt::glm_weightit`), and only with the model-based variance
#' (`vce = "stata"` or `"model"`); for every other class it is refused with
#' an error naming the class, never silently replaced by Wald intervals.
#' The method used is recorded in the table's `stored$ci_method`. For
#' `glm.nb`, MASS's profile holds theta at its estimate, so the interval is
#' conditional on theta (it ignores theta's uncertainty, which the Wald
#' interval from the joint information includes).
#' @param fit A fitted model, or a data frame of estimates.
#' @return A character vector.
#' @seealso [tt_vce_types()], [regtab()]
#' @examples
#' tt_ci_methods(glm(am ~ wt, family = binomial, data = mtcars))
#' tt_ci_methods(data.frame(term = "x", estimate = 1, std.error = 0.5))
#' @export
tt_ci_methods <- function(fit) UseMethod("tt_ci_methods")

#' @export
tt_ci_methods.default <- function(fit) {
  if (!is.data.frame(fit) && class(fit)[1] %in% c("lm", "glm", "negbin")) c("wald", "profile") else "wald"
}

.rt_user_vce_of <- function(fit) {
  u <- attr(fit, "tt_user_vce", exact = TRUE)
  if (is.null(u)) {
    cli::cli_abort("{.code vce = \"user\"} is set by {.fn regtab} from a function or matrix {.arg vce}; pass the variance itself.", call = NULL)
  }
  u
}

.rt_check_vce <- function(fit, vce, cluster = NULL) {
  if (identical(vce, "user") && !is.null(attr(fit, "tt_user_vce", exact = TRUE)) && is.null(cluster)) return(invisible(TRUE))
  ok <- tt_vce_types(fit)
  if (!is.character(vce) || length(vce) != 1L || is.na(vce) || !vce %in% ok) {
    rc <- is.character(vce) && length(vce) == 1L && vce %in% c("robust", "cluster")
    hint <- if (inherits(fit, "survreg") && rc) {
      "Refit with {.code survreg(robust = TRUE)} or {.code cluster()}: regtab then applies Stata's G/(G - 1) under {.code vce = \"stata\"}."
    }
    if (inherits(fit, "coxph") && rc && .mi_is_finegray(fit)) {
      # Review P2-5: name the Fine-Gray fit, not coxph in general.
      cli::cli_abort(c(
        "{.arg vce} {.val {vce}} is not available for Fine-Gray {.cls coxph} fits (on {.fn survival::finegray} data).",
        "i" = "Their variance under {.code vce = \"stata\"} is already {.code stcrreg}'s robust variance, clustered by subject."
      ), call = NULL)
    }
    cli::cli_abort(c(
      "{.arg vce} {.val {vce}} is not available for {.cls {class(fit)[1]}} models.",
      "i" = "Use one of {.val {ok}}.",
      "i" = hint
    ), call = NULL)
  }
  if (!is.null(cluster) && !identical(vce, "cluster")) {
    cli::cli_abort("{.arg cluster} applies only with {.code vce = \"cluster\"} (got {.val {vce}}).", call = NULL)
  }
  if (!is.null(cluster) && inherits(fit, "geeglm")) {
    # Review P2-2: refused here, before tidying, with the model named.
    cli::cli_abort(c("A {.cls geeglm} fit clusters on its own {.arg id}; leave {.arg cluster} empty.",
                     "i" = "{.code vce = \"cluster\"} (or {.code \"robust\"}) is the GEE sandwich clustered by that id."),
                   call = NULL)
  }
  if (identical(vce, "cluster") && is.null(cluster) && class(fit)[1] %in% c("lm", "glm", "negbin")) {
    cli::cli_abort("{.code vce = \"cluster\"} needs {.arg cluster}: a formula ({.code ~id}), a column name, or a vector.",
                   call = NULL)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
tt_regtab_rows <- function(fit, info, ...) UseMethod("tt_regtab_rows")

#' @keywords internal
#' @noRd
tt_regtab_ancillary_rows <- function(fit, info, ...) UseMethod("tt_regtab_ancillary_rows")

#' @export
tt_regtab_ancillary_rows.default <- function(fit, info, ...) NULL

#' Rows a model adds after every coefficient row
#'
#' Returns `NULL` or a data frame shaped like `tt_regtab_rows()` output
#' (`.rt_row()` columns) on the display scale; see the header of this file
#' for placement and exemptions. Cells with `ancillary = TRUE` are never
#' tested by `dimnonsig` (`regtab.ado:2189-2192`).
#' @param fit A fitted model.
#' @param info Its `tt_model_info()`.
#' @param o The resolved regtab options (`level`, `vce`, `relabel`,
#'   `noreeffects`, ...).
#' @keywords internal
#' @noRd
tt_regtab_trailing_rows <- function(fit, info, o, ...) UseMethod("tt_regtab_trailing_rows")

#' @export
tt_regtab_trailing_rows.default <- function(fit, info, o, ...) NULL

#' @keywords internal
#' @noRd
tt_model_stats <- function(fit, info, ...) UseMethod("tt_model_stats")

# Phase 4 names, kept for the callers and tests that use them.
.rt_vcov <- function(fit, vce = "stata", cluster = NULL) tt_vcov(fit, vce, cluster)
.rt_df <- function(fit, vce = "stata", cluster = NULL) tt_wald_df(fit, vce, cluster)

# Coefficient names the default tt_regtab_rows() leaves to
# tt_regtab_ancillary_rows().
.rt_anc_terms <- function(info) c(info$cutpoint_terms, info$ancillary_terms)

# Append ancillary rows, keeping the attributes of the main rows.
.rt_add_ancillary <- function(res, anc) {
  if (is.null(anc) || !nrow(anc)) return(res)
  # A covariate may carry an ancillary parameter's name (`p`, `alpha`,
  # `cut1`, ...); tt_regtab_build() keys structural rows in Stata's `/`
  # equation (.rt_ns_keys()), so the two never share a row.
  at <- attr(res, "positional")
  res <- rbind(res, anc[, names(res), drop = FALSE])
  rownames(res) <- NULL
  attr(res, "positional") <- at
  res
}

# keep()/drop() key of a row: a multi-equation key "eq::colname" matches as
# its colname.
.rt_match_key <- function(keys) sub("^.*?::", "", keys, perl = TRUE)

# Row key of a multi-equation row.
.rt_eq_key <- function(eq, key) paste0(gsub("::", ":", eq, fixed = TRUE), "::", key)

# Wald statistics of named parameters from a full parameter vector `b` and
# the variance `V` (names in both), on the model's reference distribution.
.rt_wald_named <- function(b, V, terms, df = Inf, conf.level = 0.95) {
  est <- unname(b[terms])
  se <- sqrt(pmax(diag(V)[terms], 0))
  se <- unname(se)
  stat <- est / se
  if (is.finite(df)) {
    p <- 2 * stats::pt(abs(stat), df, lower.tail = FALSE)
    q <- stats::qt((1 + conf.level) / 2, df)
  } else {
    p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    q <- stats::qnorm((1 + conf.level) / 2)
  }
  data.frame(term = terms, estimate = est, std.error = se, statistic = stat, p.value = p,
             conf.low = est - q * se, conf.high = est + q * se, stringsAsFactors = FALSE)
}

# An ancillary row from a Wald line `w` (one row of .rt_wald_named()).
# `transform` gives Stata's diparm rows (`_diparm`: estimate and CI
# transformed, no test, so no p-value): "exp" for p = exp(ln_p), "invexp"
# for 1/p = exp(-ln_p).
.rt_anc_row <- function(key, label, w, block = key, transform = c("none", "exp", "invexp"),
                        sub = 1e6, kind = "var", role = "ancillary") {
  transform <- match.arg(transform)
  est <- w$estimate
  lo <- w$conf.low
  hi <- w$conf.high
  p <- w$p.value
  if (transform == "exp") {
    est <- exp(est)
    lo <- exp(w$conf.low)
    hi <- exp(w$conf.high)
    p <- NA_real_
  } else if (transform == "invexp") {
    est <- exp(-est)
    lo <- exp(-w$conf.high)
    hi <- exp(-w$conf.low)
    p <- NA_real_
  }
  .rt_row(key, block, kind, label, if (is.finite(est)) "est" else "omit", term = w$term,
          estimate = est, conf.low = lo, conf.high = hi, p.value = p, sub = sub, ancillary = TRUE,
          role = role)
}

# Numerical Jacobian of a vector function by the four-point central
# difference (error O(h^4)), used where only an analytic score is written
# down (the observed information of zero-inflated and hurdle models).
.rt_jacobian <- function(f, x, rel = 1e-4) {
  k <- length(x)
  f0 <- f(x)
  J <- matrix(NA_real_, length(f0), k)
  for (j in seq_len(k)) {
    h <- rel * max(1, abs(x[j]))
    e <- replace(numeric(k), j, h)
    J[, j] <- (-f(x + 2 * e) + 8 * f(x + e) - 8 * f(x - e) + f(x - 2 * e)) / (12 * h)
  }
  J
}

# Inverse of the negative Hessian, symmetrised; NULL when singular.
.rt_inv_neg <- function(H) {
  V <- tryCatch(solve(-H), error = function(e) NULL)
  if (is.null(V)) return(NULL)
  (V + t(V)) / 2
}

# Variable label of a column (Stata falls back to the name).
.rt_label_of <- function(mf, v) {
  if (is.null(mf) || is.null(mf[[v]])) return(v)
  var_label(mf[[v]], v)
}

# Stata equation-label rules for multi-equation estimators
# (`regtab.ado:1552-1567`): the dependent variable's label for its own
# equation, fixed names for inflate/selection/lnsigma/ancillary equations,
# then underscores become spaces.
.rt_eq_label <- function(x) gsub("_", " ", x, fixed = TRUE)

# ---------------------------------------------------------------------------
# Multi-equation rows (mlogit, zip/zinb, churdle layouts)

#' Rows of a multi-equation model in Stata's coleq#colname layout
#'
#' `regtab.ado:1465-1605`: every row reads `<equation label>: <row label>`;
#' continuous terms show their variable label, the intercept `Intercept`.
#' Factor covariates get the single-equation layout inside each equation
#' block (tabtools 2.1.12; 2.1.11 showed the raw colname `2.smoking`): a
#' header row `<equation>: <variable label>` above indented level rows
#' `<equation>:   <value label>`, once per consecutive run of levels
#' (.rt_fv_parent()); a factor's base level is a Reference row in each
#' equation that contains the factor. An interaction's cells keep their
#' raw colname under a header naming the parent (`female#smoking`), as in
#' the single-equation layout.
#' Within an equation the rows follow collect's global colname order (the
#' order in which colnames first appear in e(b), across equations), with
#' `_cons` last; e(b) follows the formula as written (`##` expands in
#' place, probe G10), so terms are ordered by their position in the
#' formula (`terms(keep.order = TRUE)`), not R's interactions-last column
#' order. For mlogit, e(b) opens with the base-outcome equation, whose
#' non-factor terms are `o.`-marked (distinct colnames), so factor levels
#' come first when the base outcome is the first equation
#' (`factor_first`). Coefficients the model could not estimate are dropped,
#' as regtab drops a multi-equation row whose label starts `o.`
#' (`regtab.ado:1573-1575`).
#'
#' Interactions (Phase 5 review P1-4; probes E01, E02, G09, G10, G27, G28):
#' natively, one row per cell keyed and labelled like Stata's colname
#' (`1.female#index_age`, `1.female#2.smoking`, `treatment#age_z`), with a
#' Reference row for every cell at a base level, placed with the factor
#' levels (they carry no `o.` mark in the base-outcome equation); in fvgen
#' mode (Stata's fvgen-generated variables are continuous) a factor that
#' appears in an interaction gives flat rows labelled by level (`Female`,
#' or `female=1` when unlabelled) without a Reference row, and each
#' estimable cell one row `part1 x part2`, all in formula order.
#' @param eqs List of equations, each a list with `name` (key prefix),
#'   `label` (display prefix), `wald` (a `tt_wald()`-shaped data frame
#'   whose `col` column holds the design column names and `term` the R
#'   coefficient names), `assign` (term label of each design column, `""`
#'   for the intercept), `bases` (base level of each factor), and
#'   optionally `X` (design matrix, for empty cells) and `order` (term
#'   labels in formula order).
#' @param mf Model frame (labels, factor levels, value-label codes).
#' @param factor_first Put factor levels before other terms (mlogit).
#' @keywords internal
#' @noRd
.rt_multieq_rows <- function(eqs, mf, factor_first = FALSE, interactions = "native",
                             xsymbol = "\u00d7", vsref = NULL) {
  if (!nzchar(xsymbol)) xsymbol <- "\u00d7"
  is_fac <- function(v) {
    x <- if (!is.null(mf)) mf[[v]] else NULL
    !is.null(x) && (is.factor(x) || is.character(x) || is.logical(x))
  }
  one <- function(v, key, label, kind, status = "est", j = NA_integer_) {
    data.frame(key = key, label = label, kind = kind, var = v, status = status, j = j,
               stringsAsFactors = FALSE)
  }
  term_rows <- function(e, t) {
    w <- e$wald
    jj <- which(e$assign == t)
    cols <- w$col[jj]
    if (!nzchar(t)) return(one(NA_character_, "_cons", "Intercept", "intercept", j = jj[1]))
    if (grepl(":", t, fixed = TRUE)) {
      wt <- data.frame(term = cols, estimate = w$estimate[jj], conf.low = w$conf.low[jj],
                       conf.high = w$conf.high[jj], p.value = w$p.value[jj], stringsAsFactors = FALSE)
      r <- .rt_interaction_rows(t, NULL, wt, mf, e$X, interactions, xsymbol, 0, e$bases)
      r <- r[r$kind != "int_header", , drop = FALSE]
      if (!nrow(r)) return(NULL)
      fac_cell <- r$kind == "int_level"
      return(one(NA_character_, r$key, r$label, ifelse(fac_cell, "level", "var"),
                 status = ifelse(r$status %in% c("base", "empty"), r$status, "est"),
                 j = match(r$term, w$col)))
    }
    if (is_fac(t)) {
      x <- mf[[t]]
      lev <- .rt_levels(x)
      codes <- .rt_codes(x)
      flat <- identical(interactions, "fvgen") && t %in% e$inter_vars
      labelled <- .rt_is_labelled(x, codes)
      out <- list()
      hits <- vapply(lev, function(l) match(paste0(t, l), cols), 1L)
      base <- e$bases[[t]]
      for (k in seq_along(lev)) {
        key <- paste0(codes[[lev[k]]], ".", t)
        if (!is.null(base) && identical(lev[k], base)) {
          if (!flat) out[[length(out) + 1L]] <- one(t, key, paste0("  ", .rt_level_text(x, lev[k])), "level", "base")
          next
        }
        if (is.na(hits[k])) next
        if (flat) {
          lab <- .rt_fv_partlabel(t, .rt_level_text(x, lev[k]), labelled)
          if (!is.null(vsref) && !is.null(base)) {
            lab <- paste(lab, gsub("@", .rt_fv_partlabel(t, .rt_level_text(x, base), labelled), vsref, fixed = TRUE))
          }
          out[[length(out) + 1L]] <- one(t, key, .rt_fv80(lab), "var", j = jj[hits[k]])
        } else {
          out[[length(out) + 1L]] <- one(t, key, paste0("  ", .rt_level_text(x, lev[k])), "level", j = jj[hits[k]])
        }
      }
      # The base level just before the first level (Stata lists it there).
      res <- do.call(rbind, out)
      if (!is.null(res) && !flat && any(res$status == "base")) {
        b <- res[res$status == "base", , drop = FALSE]
        res <- rbind(b, res[res$status != "base", , drop = FALSE])
      }
      return(res)
    }
    sq <- .rt_square_of(t)
    if (!is.na(sq) && length(jj) == 1L) {
      lab <- if (identical(interactions, "fvgen")) .rt_fv80(paste0(.rt_square_label(mf, sq, t), "\u00b2")) else paste0(sq, "#", sq)
      return(one(NA_character_, paste0("c.", sq, "#c.", sq), lab, "var", j = jj))
    }
    if (length(jj) == 1L && identical(cols, t)) return(one(NA_character_, t, .rt_label_of(mf, t), "var", j = jj))
    one(NA_character_, cols, cols, "var", j = jj)
  }
  # Describe every design column of every equation, in formula order.
  desc <- lapply(eqs, function(e) {
    tl <- unique(e$assign)
    ord <- e$order %||% character()
    pos <- vapply(tl, function(t) {
      if (!nzchar(t)) return(Inf)
      p <- sort(strsplit(t, ":", fixed = TRUE)[[1]])
      hit <- which(vapply(strsplit(ord, ":", fixed = TRUE), function(q) identical(sort(q), p), TRUE))
      if (length(hit)) hit[1] else match(t, tl) + 1e6
    }, 0)
    tl <- tl[order(pos, seq_along(tl))]
    e$inter_vars <- unique(unlist(strsplit(tl[grepl(":", tl, fixed = TRUE)], ":", fixed = TRUE)))
    out <- do.call(rbind, lapply(tl, function(t) term_rows(e, t)))
    rownames(out) <- NULL
    out
  })
  # Global colname order.
  all_keys <- unlist(lapply(desc, function(d) d$key))
  all_kind <- unlist(lapply(desc, function(d) d$kind))
  if (factor_first) {
    glob <- unique(c(all_keys[all_kind == "level"], all_keys[all_kind == "var"]))
  } else {
    glob <- unique(all_keys[all_kind != "intercept"])
  }
  out <- list()
  for (k in seq_along(eqs)) {
    e <- eqs[[k]]
    d <- desc[[k]]
    w <- e$wald
    d <- d[order(d$kind == "intercept", match(d$key, glob)), , drop = FALSE]
    prev_par <- ""
    for (i in seq_len(nrow(d))) {
      # A factor header above each run of levels with one parent (Stata
      # 2.1.12, `_regtab_fvparent` inside each equation block).
      par <- if (d$kind[i] == "level") .rt_fv_parent(d$key[i]) else ""
      if (nzchar(par) && !identical(par, prev_par)) {
        hl <- if (grepl("#", par, fixed = TRUE)) par else .rt_label_of(mf, par)
        out[[length(out) + 1L]] <- .rt_row(.rt_eq_key(e$name, par), e$name, "cat_header",
                                           paste0(e$label, ": ", hl), "header", sub = k * 1e4 + i - 0.5)
      }
      prev_par <- par
      key <- .rt_eq_key(e$name, d$key[i])
      label <- paste0(e$label, ": ", d$label[i])
      kind <- if (d$kind[i] == "level") "level" else "var"
      # The equation's intercept keeps its structural role (H5) although
      # the layout lists it as an ordinary row.
      role <- if (d$kind[i] == "intercept") "intercept" else "coef"
      if (d$status[i] %in% c("base", "empty")) {
        out[[length(out) + 1L]] <- .rt_row(key, e$name, kind, label, d$status[i], sub = k * 1e4 + i, role = role)
        next
      }
      if (is.na(d$j[i])) next
      r <- w[d$j[i], ]
      if (is.na(r$estimate)) next
      out[[length(out) + 1L]] <- .rt_row(key, e$name, kind, label, "est", term = r$term,
                                         estimate = r$estimate, conf.low = r$conf.low,
                                         conf.high = r$conf.high, p.value = r$p.value,
                                         sub = k * 1e4 + i, role = role)
    }
  }
  res <- if (length(out)) do.call(rbind, out) else .rt_row(character(), character(), character(), character())
  rownames(res) <- NULL
  if (anyDuplicated(res$key)) {
    cli::cli_abort("Internal error: repeated multi-equation row keys {.val {unique(res$key[duplicated(res$key)])}}.",
                   call = NULL)
  }
  attr(res, "positional") <- if (!is.null(mf)) {
    names(mf)[vapply(names(mf), function(v) is.factor(mf[[v]]) && isTRUE(attr(.rt_codes(mf[[v]]), "positional")), TRUE)]
  } else character()
  res
}

# The factor parent of a raw colname key, as Stata's `_regtab_fvparent`
# (regtab.ado 2.1.12) and its renderer's `_tt_collect_factor_parent` build
# it: "2.sex" -> "sex", "1.grp#c.x" -> "grp#x", "1.a#2.b" -> "a#b"; ""
# when no component is a factor level or the key is its own parent.
.rt_fv_parent <- function(key) {
  if (!grepl(".", key, fixed = TRUE)) return("")
  parts <- strsplit(key, "#", fixed = TRUE)[[1]]
  hasfv <- FALSE
  bad <- FALSE
  for (i in seq_along(parts)) {
    p <- parts[i]
    dot <- regexpr(".", p, fixed = TRUE)
    if (dot > 1 && grepl("^[0-9bon]*[0-9][0-9bon]*$", substr(p, 1L, dot - 1L))) {
      p <- substring(p, dot + 1L)
      hasfv <- TRUE
    } else if (dot == 2 && substr(p, 1L, 1L) == "c") {
      p <- substring(p, 3L)
    }
    if (!nzchar(p)) bad <- TRUE
    parts[i] <- p
  }
  par <- paste(parts, collapse = "#")
  if (bad || !hasfv || identical(par, key)) "" else par
}

# Term labels of a formula as written (Stata's e(b) order).
.rt_formula_order <- function(f) {
  tryCatch(attr(stats::terms(f, keep.order = TRUE), "term.labels"), error = function(e) character())
}

# Base level of every factor in a design, from its contrasts (treatment
# coding only, as for single-equation models).
.rt_bases <- function(contrasts, mf) {
  out <- list()
  for (v in names(contrasts)) {
    if (!is.null(mf) && !is.null(mf[[v]])) out[[v]] <- .rt_contrast_base(contrasts[[v]], mf[[v]], v)
  }
  out
}

# Term label of each design column (from the "assign" attribute).
.rt_assign_labels <- function(X, terms) {
  a <- attr(X, "assign")
  tl <- attr(terms, "term.labels")
  if (is.null(a)) return(rep("", ncol(X)))
  ifelse(a == 0L, "", tl[pmax(a, 1L)])
}

#' @rdname regtab
#' @section Ordinal, multinomial, count, and survival models:
#' Each class is shown as the Stata command it estimates, with that
#' command's default standard errors (`vce = "model"` uses the fit's own
#' `vcov()` instead); every test and interval is normal (z):
#' * `MASS::polr`, `ordinal::clm` (Stata `ologit`, OR; `oprobit` and other
#'   links, Coef.): the covariates, then one row per cutpoint, `cut1`,
#'   `cut2`, ..., on the raw scale with no p-value, dropped by the automatic
#'   `nointercept` of ologit and relabelled by `cutlabels`. Standard errors:
#'   the analytic observed information (`clm`'s own; for `polr`, computed at
#'   its estimates instead of `optim()`'s numerical Hessian). `clm` fits
#'   need flexible thresholds and no nominal or scale effects. With an
#'   `offset()`, the Pseudo R-squared is blank, as Stata's `ologit`/
#'   `oprobit` report no e(ll_0); so is a `logit`/`probit` glm's (Stata
#'   stores no e(r2_p)), while a Poisson glm's is against the constant-only
#'   model with the offset, as Stata's `poisson`. Only logit, probit and
#'   log-link Poisson glm fits have one at all: Stata's `cloglog` and `glm`
#'   commands store no e(r2_p).
#' * `nnet::multinom` (Stata `mlogit`, RRR): one equation per non-base
#'   outcome, rows `<outcome label>: <label>`; a factor reads as in the
#'   single-equation layout inside each equation, `Secondary: Smoking
#'   status` above `Secondary:   Former` (Stata tabtools 2.1.12; 2.1.11
#'   showed `Secondary: 1.smoking`). As in Stata, factor levels (and interaction
#'   cells with a factor) come first in each equation only when the base
#'   outcome has the lowest value code; otherwise the rows follow the
#'   formula. The base outcome is the response's first level, where
#'   Stata's `mlogit` without `baseoutcome()` takes the most frequent
#'   outcome: the table matches `mlogit, baseoutcome(#)` with that first
#'   level, and [stats::relevel()] the response to match another; when the
#'   first level is not the most frequent outcome, `regtab()` says so once
#'   per session. Its code is
#'   read from the `"labels"` attribute ([tt_as_factor()]) or integer level
#'   texts, and taken as the lowest when neither exists. `relevel()` drops
#'   the attribute: restore it (`attr(y, "labels") <- codes`) to reproduce
#'   `mlogit, baseoutcome()` with a base other than the lowest value.
#'   Weights enter the information matrix; a count-matrix response
#'   (`cbind(n1, n2, n3) ~ x`) gives the same table as the one-row-per-unit
#'   fit. An `offset()` term (an n x outcomes matrix, or a vector for a
#'   binary outcome) enters the information and the constant-only model of
#'   the pseudo R-squared; Stata's `mlogit` has no offset, so this is an R
#'   extension. `multinom()` keeps no copy of its data unless
#'   `model = TRUE`: the data are read again and used only when they
#'   reproduce the fit (rows, response and fitted probabilities), else the
#'   fit is refused. Weight decay (`decay > 0`, a penalised fit with no
#'   Stata equivalent) is refused. `multinom()` stops ~1e-4 (relative) from
#'   the maximum; refit with `reltol = 1e-12, maxit = 1000` when a last
#'   digit differs.
#' * `pscl::zeroinfl` (Stata `zip`/`zinb`) and `pscl::hurdle` (Stata's
#'   `churdle` layout), Coef.: rows `<dependent variable label>: <label>`,
#'   `Inflation equation: <label>` (zeroinfl) or `Selection equation:
#'   <label>` (hurdle), and, for negative binomial counts, `Ancillary:
#'   lnalpha` (`-log(theta)`) and `Ancillary: alpha`. Standard errors: the
#'   observed information from the analytic score (pscl's `vcov()` comes
#'   from a numerical Hessian at estimates `optim()` stops ~1e-4 from the
#'   maximum).
#' * `MASS::glm.nb` (Stata `nbreg`): with `keepintercept`, the rows
#'   `lnalpha` (ln of 1/theta, from the joint information) and `alpha`
#'   precede the intercept, without a p-value.
#' * `survival::survreg` with a Weibull, exponential, lognormal or
#'   loglogistic distribution (Stata `streg, time`): time ratios (TR),
#'   `exp()` of the log-time coefficients with `exp()`-transformed
#'   confidence bounds, as Stata tabtools 2.1.10 and later show them (2.1.9
#'   printed the coefficients unexponentiated). With `keepintercept`, the
#'   ancillary rows `ln_p`, `p`, `1/p` (Weibull), `lnsigma`, `sigma`
#'   (lognormal) or `lngamma`, `gamma` (loglogistic) precede the intercept,
#'   on their own scale, and the intercept is shown as `exp()` of the
#'   constant, as in Stata tabtools 2.1.12 (2.1.10 and 2.1.11 also
#'   exponentiated the lognormal and loglogistic ancillary rows, which
#'   regtab never copied). A covariate named like one of these parameters
#'   (`ln_p`, `lnsigma`, ...) is shown in its own row beside it, with the
#'   same label, as Stata 2.1.12 shows it. The log-likelihood is
#'   streg's, which leaves out the Jacobian of the log-time transform. A
#'   Gaussian `survreg` is Stata `intreg` (Coef., intercept kept, a `sigma`
#'   row before it); an `AER::tobit()` fit is Stata `tobit` (the residual
#'   variance `var(e.<y>)` after the intercept, like a random-effects row).
#'   Other distributions (logistic, t, extreme value, rayleigh) have no
#'   Stata equivalent and are refused.
#' * Fine-Gray (Stata `stcrreg`, SHR): `survival::coxph()` on
#'   `survival::finegray()` data, with standard errors from the sandwich
#'   clustered by subject times N/(N - 1), Stata's `vce(robust)`;
#'   `coxph()`'s own variance treats every expanded record as independent.
#'   A fit is Fine-Gray when it is weighted, its response is
#'   `Surv(start, stop, status)`, and its data carry `finegray()`'s mark
#'   (its `"event"` attribute), whatever the columns are called; an
#'   unweighted fit on such data is a Cox model; `regtab(finegray =
#'   TRUE/FALSE)` declares it. The subjects are the fit's `cluster()`/`id`
#'   (`coxph(..., cluster = id)` or `coxph(..., id = id)`, with the id kept
#'   in the `finegray()` formula) when given. Without one, they are read
#'   only from `finegray()` output of right-censored data (each subject's
#'   records together, the first with weight 1 at the time origin, the
#'   later ones with censoring weights below 1), in `finegray()`'s own row
#'   order, which data re-sorted with `[` keep in their row names. Delayed
#'   entry and counting-process input (`finegray(Surv(start, stop, event)
#'   ~ ., id = id)`) can write several weight-1 records per subject, so
#'   there, and for data re-sorted with the row names reset, case weights,
#'   or data without `finegray()`'s mark, the variance is an error until the
#'   id is on the fit.
#'   Labels `finegray()` drops are restored from the data it was given when
#'   that data frame is found in the model's environment. The weighted Cox
#'   fit differs from Stata's by ~1e-3 in the estimates and ~1e-4 in the
#'   standard errors; `cmprsk::crr()` reproduces `stcrreg` exactly (its
#'   variance times N/(N - 1)), with rows labelled by the covariate
#'   matrix's column names: name them, `cov1 = cbind(treated = d$treated)`
#'   (an unnamed `cov1 = d$treated` shows `co$treated1`).
#'
#' Beside a multi-equation model (`multinom`, `zeroinfl`, `hurdle`), and
#' without a mixed model, every model takes Stata's `coleq#colname` layout:
#' a single-equation model's rows read `<dependent variable label>:
#' <label>` (a factor as a header row above its indented levels, with a
#' Reference row, as in Stata tabtools 2.1.12; its intercept `Intercept`),
#' so a Poisson or negative binomial model of the
#' same outcome shares the `zip`/`zinb` count equation's rows; ancillary
#' rows read `Ancillary: <name>` (`Ancillary: cut1`, `Ancillary: lnalpha`;
#' `cutlabels` replace the whole label, as in Stata 2.1.12);
#' survival models' equation is Stata's `_t`, "Analysis time when record
#' ends"; intreg's is `model`, with its `Scale: Intercept` row. Every
#' model's equations are kept, with blank cells where a model lacks one, as
#' in Stata tabtools 2.1.11 and later (2.1.9 kept only the equations of its
#' first model).
#' @name regtab
NULL
