# Multiply imputed fits in regtab (task 5.14, conditional task C4; Stata
# tabtools 2.1.12 renders `mi estimate` collections, `regtab.ado:413-522`,
# `:4569-4760`).
#
# A tt_mi object holds one fit per completed dataset. regtab() shows it as
# one model, as Stata shows `collect: mi estimate: <command>`:
# - rows, labels and Reference rows come from the first fit's
#   tt_regtab_rows() (every imputation has the same design: checked);
# - estimates, standard errors, intervals and p-values are Rubin's rules
#   over tt_coef()/tt_vcov(fit, vce, cluster) of each fit, with the degrees
#   of freedom of Stata's `mi estimate` ([MI] mi estimate, Methods and
#   formulas): Barnard and Rubin's small-sample df when the completed-data
#   command has finite residual df (tt_wald_df(), the smallest over the
#   imputations, as mi estimate uses the smallest e(df_r)), Rubin's
#   large-sample df otherwise;
# - stats: Imputations (M) and Largest FMI; the counts (Observations,
#   Subjects, Events) when every imputation has the same; no likelihood
#   (ll, AIC, BIC), R-squared, Root MSE or F, as mi estimate stores none.
#
# The formulas, verified against Stata 17's e(b_mi), e(V_mi), e(W_mi),
# e(B_mi), e(df_mi), e(rvi_mi), e(fmi_mi) to 1e-13 for regress (plain,
# robust, clustered) and glm, and to the fits' convergence (1e-8) for
# logit, poisson and stcox (tests/testthat/fixtures/regtab_mi/mi_emat.csv,
# qa/stata/make_regtab_mi.do):
#   Qbar = mean(Q_i), Ubar = mean(U_i), B = sum (Q_i - Qbar)(Q_i - Qbar)' / (M - 1),
#   T = Ubar + (1 + 1/M) B;  per coefficient
#   r = (1 + 1/M) B / Ubar (RVI),  nu_L = (M - 1) (1 + 1/r)^2 (Rubin 1987),
#   with complete-data df nu_c finite (Barnard and Rubin 1999):
#   gamma = (1 + 1/M) B / T,  nu_obs = nu_c (nu_c + 1) (1 - gamma) / (nu_c + 3),
#   nu = 1 / (1/nu_L + 1/nu_obs);  otherwise nu = nu_L;
#   FMI = (r + 2 / (nu + 3)) / (r + 1) (large sample), or
#   1 - lambda(nu) Ubar / (lambda(nu_c) T), lambda(x) = (x + 1)/(x + 3)
#   (small sample); a coefficient whose estimate is the same in every
#   imputation (B = 0) has nu_L infinite (normal intervals when nu_c is
#   infinite, nu = nu_obs otherwise) and FMI 0, as Stata reports.
#   CI: Qbar -/+ t(nu, (1 + level)/2) sqrt(T), p = 2 P(t_nu > |Qbar| / sqrt(T)).
# mice::pool() differs: it takes the complete-data df from df.residual()
# for every class (nevent - p for coxph), so it applies Barnard-Rubin to
# z-based models too.

#' Multiply imputed fits for regtab()
#'
#' Marks a list of fits, one per completed (imputed) dataset, as a single
#' multiply imputed model, which [regtab()] pools by Rubin's rules with the
#' degrees of freedom of Stata's `mi estimate`. A `mice` `mira` object
#' (`with(imp, lm(...))`) may be passed to `regtab()` directly; `tt_mi()`
#' is for fits made another way (a loop over `mice::complete()` data,
#' `mitools`, `Amelia`, ...).
#'
#' Pooling follows Stata's `mi estimate`: the estimate is the mean of the
#' imputations' estimates, its variance the total variance `T = W + (1 +
#' 1/M) B` (within- plus between-imputation variance), and intervals and
#' p-values use a t distribution with each coefficient's own degrees of
#' freedom: Barnard and Rubin's small-sample degrees of freedom when the
#' completed-data model has finite residual degrees of freedom (`lm`, and
#' `lm` with `vce = "cluster"`: clusters - 1), and Rubin's large-sample
#' degrees of freedom otherwise (`glm`, `coxph`, `crr`: normal-theory
#' models; but see `vce = "model"` below). `mice::pool()` instead applies the small-sample formula to
#' every class, with `df.residual()` (or events minus coefficients for
#' `coxph`) as the complete-data degrees of freedom, so its intervals for
#' a logistic or Cox model differ from Stata's and from regtab's.
#'
#' Each imputation's variance is `tt_vcov(fit, vce, cluster)`, so
#' `regtab(..., vce = "robust")` is Stata's `mi estimate: logit ...,
#' vce(robust)`. The effect scale follows the model class as for a single
#' fit (odds ratios for a logistic model): Stata's `mi estimate` reports
#' coefficients unless it is itself given `or`, `hr`, `eform`, ..., and
#' Stata tabtools shows both on the ratio scale, so the tables agree.
#'
#' The fits must all be of one supported class (`lm`, `glm`,
#' `survival::coxph`, Fine-Gray `coxph` on `finegray()` data, or
#' `cmprsk::crr`), with the same formula, the same coefficients and the
#' same aliased (omitted) coefficients, the same factor coding (contrasts
#' and levels, so that a coefficient of one name compares the same levels
#' in every imputation), on the same observations: as Stata's
#' `mi estimate`, which refuses omitted terms or an estimation sample that
#' vary across imputations (unless `esampvaryok`, which regtab does not
#' offer). Each imputation is compared with imputation 1. The observations
#' are identified by the fits' row names (rows with a positive weight),
#' which `lm`, `glm` and `coxph` keep: fits on different rows of the same
#' size (a subject dropped in one imputation, another in the next) are
#' refused. Row names identify observations only when they are stable
#' identifiers, such as the row names of a `data.frame` subset with `[` or
#' `subset()`, or of `mice::complete()` data: a dataset renumbered after
#' filtering (a tibble, whose rows are always numbered 1 to n, or a
#' `data.frame` after `rownames(d) <- NULL` or written out and read back)
#' is compared by position, so fits on different rows of equal size pass;
#' filter before imputing, keep a `data.frame` with its row names, or supply
#' `observation_ids`. Where the row names cannot identify the observations at all
#' (`cmprsk::crr`, which keeps none, or completed datasets numbered apart,
#' whose fits share no row name, such as the pieces of a split long-format
#' stack), only the number of observations is compared by default.
#' `sample_check = "strict"` requires explicit `observation_ids` and refuses
#' pooling different subject sets even if row names have been renumbered.
#' IDs describe the fitted positive-weight observations, after missing-value
#' deletion and subsetting; they must not describe the original unfiltered
#' data. Their order may differ between imputations. Incorrect caller-supplied
#' IDs cannot be detected. [regtab()] records the check's method and per-fit
#' counts in `meta$mi_sample_identity`, without storing subject IDs there.
#' Invalid IDs are refused with `tabtools_error_mi_observation_ids`; strict
#' checks without IDs use `tabtools_error_mi_sample_identity`, and differing
#' explicit subject sets use `tabtools_error_mi_sample_mismatch`. At least two
#' imputations are needed. [regtab()], [tt_vcov()] and the internal
#' `tt_coef()` all check these before pooling, and [regtab()] also refuses
#' an imputation whose design differs from the first imputation's (the
#' classes of its variables, the terms its columns belong to) when its own
#' rows cannot be built as a single fit's, or map its coefficients to other
#' rows than the first imputation's.
#'
#' Rows and labels come from the first imputation's fit and its data.
#' `mice`'s completed datasets carry no variable labels, so a table from a
#' `mira` shows variable names (`index_age`) where [tt_mi()] of fits on
#' labelled data (`haven::read_dta()`, or a `"label"` attribute on each
#' column) shows the labels; label the columns before imputing, or fit the
#' imputations yourself and pass them through `tt_mi()`.
#'
#' Under `vce = "model"`, a `glm` with an estimated dispersion (gaussian,
#' Gamma, quasi families) keeps its t reference distribution (see
#' [regtab()]), so its complete-data degrees of freedom are
#' `df.residual()` and the small-sample formula applies: R-only, as Stata's
#' `glm` reports z statistics.
#'
#' @param fits A list of fitted models, one per imputation, or a `mice`
#'   `mira` object. The names of a named list are dropped (the imputations
#'   are one model).
#' @param observation_ids Optional list of unique, nonmissing character or
#'   finite numeric ID vectors, one per imputation, with one ID per fitted
#'   positive-weight observation. IDs must identify the same subjects across
#'   imputations; numeric IDs are compared by their character representation.
#' @param sample_check `"auto"` (default) uses supplied IDs, otherwise the
#'   existing row-name checks and count-only fallback. `"strict"` requires
#'   explicit `observation_ids`. Wrapping an existing `tt_mi` preserves its
#'   IDs and policy unless these arguments are supplied.
#' @return An object of class `tt_mi`: a list with elements `analyses`,
#'   `observation_ids` (a list of character vectors, or `NULL`) and
#'   `sample_check`.
#'   [tt_vcov()] of it is the pooled total variance (per imputation
#'   `vce`/`cluster` as given).
#' @references
#' Rubin DB (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
#'
#' Barnard J, Rubin DB (1999). Small-sample degrees of freedom with multiple
#' imputation. *Biometrika*, 86(4), 948-955. \doi{10.1093/biomet/86.4.948}
#'
#' StataCorp (2021). *Stata Multiple-Imputation Reference Manual, Release
#' 17*, `mi estimate`, Methods and formulas. Stata Press.
#' @seealso [regtab()]
#' @examples
#' set.seed(1)
#' d <- data.frame(x = rnorm(60), z = rbinom(60, 1, 0.5))
#' d$y <- 1 + 0.5 * d$x + d$z + rnorm(60)
#' d$x[sample(60, 12)] <- NA
#' # Five crude "imputations" of x (use mice or another imputation method
#' # in practice).
#' fits <- lapply(1:5, function(m) {
#'   dm <- d
#'   miss <- is.na(dm$x)
#'   dm$x[miss] <- rnorm(sum(miss), mean(d$x, na.rm = TRUE), sd(d$x, na.rm = TRUE))
#'   lm(y ~ x + z, data = dm)
#' })
#' regtab(tt_mi(fits), stats = c("n", "mi_m", "fmi"))
#' @export
tt_mi <- function(fits, observation_ids = NULL, sample_check = c("auto", "strict")) {
  if (inherits(fits, "tt_mi")) {
    if (missing(observation_ids) && missing(sample_check)) return(fits)
    if (missing(observation_ids)) observation_ids <- fits$observation_ids
    if (missing(sample_check)) sample_check <- fits$sample_check %||% "auto"
    fits <- fits$analyses
  }
  sample_check <- match.arg(sample_check)
  if (inherits(fits, "mira")) fits <- fits$analyses
  if (!is.list(fits) || (is.object(fits) && !identical(class(fits), "list")) || is.data.frame(fits)) {
    cli::cli_abort("{.arg fits} must be a list of fitted models (one per imputation) or a {.pkg mice} {.cls mira} object.",
                   call = NULL)
  }
  if (length(fits) < 2L) {
    cli::cli_abort(c(
      "Multiple imputation needs at least 2 imputations (got {length(fits)}).",
      "i" = "Stata's {.code mi estimate} refuses fewer than 2 as well."
    ), call = NULL)
  }
  observation_ids <- .rt_mi_validate_ids(fits, observation_ids, sample_check)
  structure(list(analyses = unname(fits), observation_ids = observation_ids,
                 sample_check = sample_check), class = "tt_mi")
}

.rt_mi_validate_ids <- function(fits, ids, sample_check = "auto") {
  if (is.null(ids)) {
    if (identical(sample_check, "strict")) {
      cli::cli_abort(c(
        "{.code sample_check = \"strict\"} requires {.arg observation_ids}.",
        "i" = "Supply a stable subject ID for every fitted positive-weight observation in every imputation."
      ), class = "tabtools_error_mi_sample_identity", call = NULL)
    }
    return(NULL)
  }
  if (!is.list(ids) || is.data.frame(ids) || length(ids) != length(fits)) {
    cli::cli_abort("{.arg observation_ids} must be a list with one ID vector per imputation.",
                   class = "tabtools_error_mi_observation_ids", call = NULL)
  }
  lapply(seq_along(fits), function(k) {
    v <- ids[[k]]
    n <- .rt_mi_nobs(fits[[k]])
    valid_type <- (is.character(v) || is.numeric(v)) && is.null(dim(v)) && !is.object(v)
    if (!valid_type || anyNA(v) || (is.numeric(v) && any(!is.finite(v))) ||
        length(n) != 1L || !is.finite(n) || length(v) != n || !length(v)) {
      cli::cli_abort(c(
        "{.arg observation_ids} for imputation {k} must contain one nonmissing character or finite numeric ID per fitted positive-weight observation.",
        "i" = "Use the estimation sample after subsetting, missing-value deletion and exclusion of zero weights."
      ), class = "tabtools_error_mi_observation_ids", call = NULL)
    }
    v <- unname(as.character(v))
    if (any(!nzchar(trimws(v))) || anyDuplicated(v)) {
      cli::cli_abort("{.arg observation_ids} for imputation {k} must contain unique, nonempty IDs.",
                     class = "tabtools_error_mi_observation_ids", call = NULL)
    }
    v
  })
}

#' @export
print.tt_mi <- function(x, ...) {
  a <- x$analyses
  cat("<tt_mi> ", length(a), " imputations of a <", class(a[[1]])[1], "> fit\n", sep = "")
  f <- tryCatch(stats::formula(a[[1]]), error = function(e) NULL)
  if (!is.null(f)) cat(paste(deparse(f, width.cutoff = 500L), collapse = " "), "\n", sep = "")
  invisible(x)
}

# Classes regtab pools: those whose rows are all coefficient rows of
# tt_coef()/tt_vcov() (no ancillary, random-effects or multi-equation rows,
# whose own transforms and variances are not pooled).
.rt_mi_classes <- c("lm", "glm", "coxph", "crr")

.rt_mi_first <- function(fit) if (inherits(fit, "tt_mi")) fit$analyses[[1]] else fit

# Apply f to every imputation of a tt_mi object (to the fit otherwise).
.rt_mi_apply <- function(fit, f) {
  if (!inherits(fit, "tt_mi")) return(f(fit))
  fit$analyses <- lapply(fit$analyses, f)
  fit
}

.rt_mi_coef <- function(fit) if (inherits(fit, "crr")) fit$coef else tt_coef(fit)

.rt_mi_nobs <- function(fit) {
  if (inherits(fit, c("coxph", "crr"))) return(as.numeric(fit$n))
  if (inherits(fit, "lm")) {
    w <- if (inherits(fit, "glm")) fit$prior.weights else fit$weights
    return(if (is.null(w)) length(fit$residuals) else sum(w > 0))
  }
  tryCatch(as.numeric(stats::nobs(fit)), error = function(e) NA_real_)
}

# The rows a fit was estimated on, by row name, sorted (positive weights
# only, as Stata's e(sample)); NULL where the fit keeps no row names
# (cmprsk::crr). lm, glm and coxph name their residuals by the model
# frame's row names (Codex audit 2, F05).
.rt_mi_rows <- function(fit) {
  if (inherits(fit, "crr")) return(NULL)
  r <- fit$residuals
  rn <- names(r)
  if (is.null(rn) || anyNA(rn) || anyDuplicated(rn)) return(NULL)
  w <- if (inherits(fit, "glm")) fit$prior.weights else fit$weights
  keep <- !is.na(r)
  if (length(w) == length(r)) keep <- keep & w > 0
  sort(rn[keep])
}

# Report what was checked without putting subject identifiers into table metadata.
.rt_mi_sample_identity <- function(x) {
  rows <- lapply(x$analyses, .rt_mi_rows)
  comparable <- all(vapply(rows, function(r) !is.null(r), TRUE)) &&
    all(vapply(rows[-1L], function(r) length(intersect(rows[[1L]], r)) > 0L, TRUE))
  list(method = if (!is.null(x$observation_ids)) "explicit_ids" else if (comparable) "row_names" else "counts_only",
       n = vapply(x$analyses, .rt_mi_nobs, 0.0),
       strict = identical(x$sample_check, "strict"))
}

# Each factor's contrast matrix and levels, as the fit coded them (a
# contrast given by name or function evaluated on the levels), so that
# codings that name their coefficients alike can be told apart (Codex
# audit 2, F06).
.rt_mi_coding <- function(fit) {
  cn <- fit$contrasts
  xl <- fit$xlevels
  cm <- lapply(stats::setNames(nm = names(cn)), function(v) {
    C <- cn[[v]]
    if ((is.character(C) || is.function(C)) && !is.null(xl[[v]])) {
      C <- tryCatch(do.call(C, list(xl[[v]])), error = function(e) C)
    }
    if (is.numeric(C)) unname(as.matrix(C)) else C
  })
  list(contrasts = cm, xlevels = xl)
}

# What the rows tt_regtab_rows() builds for a fit depend on, beyond the
# formula, family and link that .rt_mi_check() compares: its coefficient
# names, the classes of its model-frame variables, the term each column of
# its design belongs to (lm and coxph keep it; for glm it follows from the
# rest) and its factor coding. Imputations with imputation 1's signature
# have its rows; only one whose signature differs has its own rows built
# and compared (Codex audit 2, F06 review: building every imputation's
# rows made regtab() of 20 imputations of n = 5000 ten times slower).
.rt_mi_design_sig <- function(fit) {
  tm <- tryCatch(stats::terms(fit), error = function(e) NULL)
  list(coef = names(.rt_mi_coef(fit)), classes = attr(tm, "dataClasses"),
       assign = fit$assign, coding = .rt_mi_coding(fit))
}

#' @export
tt_regtab_adapter.tt_mi <- function(fit, ...) TRUE

# The gate for a multiply imputed model (from .rt_check_model()): every
# imputation passes the single-fit gate, and they agree on everything
# Stata's mi estimate requires to agree.
.rt_mi_check <- function(x, i) {
  a <- x$analyses
  M <- length(a)
  if (M < 2L) {
    cli::cli_abort("Model {i}: multiple imputation needs at least 2 imputations (got {M}).", call = NULL)
  }
  for (k in seq_len(M)) {
    f <- a[[k]]
    if (inherits(f, c("tt_mi", "mira", "mipo")) || is.data.frame(f)) {
      cli::cli_abort("Model {i}, imputation {k}: each imputation must be a fitted model (got {.cls {class(f)[1]}}).",
                     call = NULL)
    }
    tryCatch(.rt_check_model(f, i), error = function(e) {
      diagnostic_class <- grep("^tabtools_error_(model_|gee_)", class(e), value = TRUE)
      if (length(diagnostic_class)) {
        cli::cli_abort("Model {i}, imputation {k}: fitted-model diagnostics failed.",
                       class = diagnostic_class, parent = e, call = NULL)
      }
      cli::cli_abort("Model {i}, imputation {k}: not a model regtab supports.", parent = e, call = NULL)
    })
  }
  cls <- vapply(a, function(f) class(f)[1], "")
  if (length(unique(cls)) > 1L) {
    cli::cli_abort("Model {i}: the imputations are fits of different classes ({.cls {unique(cls)}}).", call = NULL)
  }
  if (!cls[1] %in% .rt_mi_classes) {
    ok <- .rt_mi_classes
    cli::cli_abort(c(
      "Model {i}: regtab does not pool multiply imputed {.cls {cls[1]}} fits.",
      "i" = "Pooled classes: {.cls {ok}} (their rows are all coefficient rows).",
      "i" = "Pool them yourself and pass the estimates as a data frame (see {.help tabtools::regtab}, Data-frame input)."
    ), call = NULL)
  }
  ids <- .rt_mi_validate_ids(a, x$observation_ids, x$sample_check %||% "auto")
  f1 <- a[[1]]
  sig <- function(f) {
    info <- tt_model_info(f)
    fo <- tryCatch(paste(deparse(stats::formula(f), width.cutoff = 500L), collapse = " "), error = function(e) "")
    paste(info$stata_cmd, info$effect_scale, paste(info$family, collapse = "/"), paste(info$link, collapse = "/"),
          fo, isTRUE(f$tt_finegray), .mi_is_finegray(f), sep = "\r")
  }
  s1 <- sig(f1)
  b1 <- .rt_mi_coef(f1)
  n1 <- .rt_mi_nobs(f1)
  c1 <- .rt_mi_coding(f1)
  r1 <- .rt_mi_rows(f1)
  for (k in seq_len(M)[-1L]) {
    if (!identical(sig(a[[k]]), s1)) {
      cli::cli_abort(c(
        "Model {i}: imputation {k} is not the same model as imputation 1 (formula, family, link or type differ).",
        "i" = "Every imputation must be fitted with the same call, as Stata's {.code mi estimate} fits one command."
      ), call = NULL)
    }
    bk <- .rt_mi_coef(a[[k]])
    if (!identical(names(bk), names(b1))) {
      d <- union(setdiff(names(b1), names(bk)), setdiff(names(bk), names(b1)))
      cli::cli_abort(c(
        "Model {i}: imputations 1 and {k} have different coefficients{if (length(d)) paste0(' (', paste(d, collapse = ', '), ')') else ''}.",
        "i" = "A factor level present in one imputation only (a rare imputed category) changes the design; make the factor's levels the same in every imputation."
      ), call = NULL)
    }
    if (!identical(is.na(bk), is.na(b1))) {
      d <- names(b1)[xor(is.na(bk), is.na(b1))]
      cli::cli_abort(c(
        "Model {i}: the omitted (aliased) coefficients differ between imputations 1 and {k} ({paste(d, collapse = ', ')}).",
        "i" = "Stata's {.code mi estimate} refuses omitted terms that vary across imputations; so does regtab."
      ), call = NULL)
    }
    # The same coefficient names can code different comparisons: custom
    # contrasts (the reverse of imputation 1's) or reordered levels. The
    # imputations' rows are built from imputation 1 alone, so their
    # codings must agree (Codex audit 2, F06).
    ck <- .rt_mi_coding(a[[k]])
    if (!isTRUE(all.equal(ck, c1))) {
      d <- unique(c(names(ck$contrasts), names(c1$contrasts)))
      d <- d[!vapply(d, function(v) isTRUE(all.equal(ck$contrasts[[v]], c1$contrasts[[v]])) &&
                       identical(ck$xlevels[[v]], c1$xlevels[[v]]), TRUE)]
      cli::cli_abort(c(
        "Model {i}: the factor coding differs between imputations 1 and {k}{if (length(d)) paste0(' (', paste(d, collapse = ', '), ')') else ''}: coefficients of the same name would compare different levels.",
        "i" = "Fit every imputation with the same contrasts and factor levels (the same reference level)."
      ), call = NULL)
    }
    nk <- .rt_mi_nobs(a[[k]])
    if (!isTRUE(all.equal(nk, n1))) {
      cli::cli_abort(c(
        "Model {i}: the number of observations differs between imputations 1 ({n1}) and {k} ({nk}).",
        "i" = "Stata's {.code mi estimate} refuses an estimation sample that varies across imputations (unless {.code esampvaryok}); impute every variable of the model."
      ), call = NULL)
    }
    # Which observations, where the fits' row names identify them (Codex
    # audit 2, F05: two fits each dropping a different row have the same
    # N). Fits whose rows share no name at all (datasets renumbered per
    # imputation, such as a split long-format stack) cannot be matched
    # this way and are compared on N only, as is cmprsk::crr.
    if (!is.null(ids)) {
      if (!setequal(ids[[1L]], ids[[k]])) {
        cli::cli_abort(c(
          "Model {i}: imputations 1 and {k} have different explicit observation IDs ({n1} observations each).",
          "i" = "Impute every model variable and retain the same subject set in every imputation."
        ), class = "tabtools_error_mi_sample_mismatch", call = NULL)
      }
      next
    }
    rk <- .rt_mi_rows(a[[k]])
    if (!is.null(r1) && !is.null(rk) && length(intersect(r1, rk)) && !identical(rk, r1)) {
      d <- c(setdiff(r1, rk), setdiff(rk, r1))
      cli::cli_abort(c(
        "Model {i}: imputations 1 and {k} are fitted on different observations ({n1} each; rows {.val {utils::head(d, 5L)}}{if (length(d) > 5L) ', ...' else ''} are in one only).",
        "i" = "Stata's {.code mi estimate} refuses an estimation sample that varies across imputations (unless {.code esampvaryok}); impute every variable of the model, and subset every imputation alike."
      ), call = NULL)
    }
  }
  invisible(TRUE)
}

# Resolve a model's cluster specification for every imputation (a vector
# per imputation, in a "tt_mi_cluster" list), after checking each
# imputation's vce.
.rt_mi_prepare_cluster <- function(x, vce, cluster) {
  cl <- lapply(x$analyses, function(f) {
    .rt_check_vce(f, vce, cluster)
    .rt_prepare_cluster(f, cluster)
  })
  if (is.null(cluster)) return(NULL)
  structure(cl, class = "tt_mi_cluster")
}

.rt_mi_cluster_k <- function(cluster, k) if (inherits(cluster, "tt_mi_cluster")) cluster[[k]] else cluster

# Rubin's rules with Stata mi estimate's degrees of freedom (see the file
# header). Returns one row per estimated coefficient (aliased ones are
# left out): term, estimate, std.error, df, statistic, p.value, conf.low,
# conf.high, riv, fmi; attributes V (total variance), W, B, M, df_c.
.rt_mi_pool <- function(fits, vce = "stata", cluster = NULL, level = 0.95) {
  M <- length(fits)
  b1 <- .rt_mi_coef(fits[[1]])
  terms <- names(b1)[!is.na(b1)]
  k <- length(terms)
  Q <- matrix(vapply(fits, function(f) unname(.rt_mi_coef(f)[terms]), numeric(k)), k, M,
              dimnames = list(terms, NULL))
  U <- lapply(seq_len(M), function(i) {
    V <- as.matrix(tt_vcov(fits[[i]], vce, .rt_mi_cluster_k(cluster, i)))
    V[terms, terms, drop = FALSE]
  })
  qbar <- rowMeans(Q)
  ubar <- Reduce(`+`, U) / M
  dev <- Q - qbar
  # An estimate identical in every imputation has no between-imputation
  # variance: exactly zero (a mean of equal doubles need not return them).
  same <- apply(Q, 1L, function(q) all(q == q[1]))
  dev[same, ] <- 0
  B <- tcrossprod(dev) / (M - 1)
  Tm <- ubar + (1 + 1 / M) * B
  b <- diag(B)
  u <- diag(ubar)
  tv <- diag(Tm)
  r <- ifelse(b > 0, (1 + 1 / M) * b / u, 0)
  nu_l <- ifelse(b > 0, (M - 1) * (1 + 1 / r)^2, Inf)
  df_c <- min(vapply(seq_len(M), function(i) {
    as.numeric(tt_wald_df(fits[[i]], vce, .rt_mi_cluster_k(cluster, i)))
  }, 0))
  if (is.finite(df_c)) {
    gam <- (1 + 1 / M) * b / tv
    nu_obs <- df_c * (df_c + 1) * (1 - gam) / (df_c + 3)
    nu <- 1 / (1 / nu_l + 1 / nu_obs)
    lam <- function(x) (x + 1) / (x + 3)
    fmi <- ifelse(b > 0, 1 - lam(nu) * u / (lam(df_c) * tv), 0)
  } else {
    nu <- nu_l
    fmi <- ifelse(b > 0, (r + 2 / (nu + 3)) / (r + 1), 0)
  }
  # A coefficient with no within-imputation variance (a constrained one)
  # has no usable df: no interval or p-value rather than a degenerate one.
  nu[!(nu > 0)] <- NA_real_
  se <- sqrt(tv)
  stat <- qbar / se
  fin <- is.finite(nu)
  p <- ifelse(fin, 2 * stats::pt(abs(stat), nu, lower.tail = FALSE),
              ifelse(is.na(nu), NA_real_, 2 * stats::pnorm(abs(stat), lower.tail = FALSE)))
  crit <- ifelse(fin, stats::qt((1 + level) / 2, nu), ifelse(is.na(nu), NA_real_, stats::qnorm((1 + level) / 2)))
  out <- data.frame(term = terms, estimate = unname(qbar), std.error = unname(se), df = unname(nu),
                    statistic = unname(stat), p.value = unname(p),
                    conf.low = unname(qbar - crit * se), conf.high = unname(qbar + crit * se),
                    riv = unname(r), fmi = unname(fmi), stringsAsFactors = FALSE)
  attr(out, "V") <- Tm
  attr(out, "W") <- ubar
  attr(out, "B") <- B
  attr(out, "M") <- M
  attr(out, "df_c") <- df_c
  out
}

# The pool of one regtab() call, computed once per (vce, cluster) and
# shared by the rows and stats methods (C4 review F10): regtab() gives each
# tt_mi model a fresh `pool_cache` environment after anchoring its fits, so
# nothing is reused across calls or after the fits change. The largest FMI
# does not depend on the level, so `any_level` reuses a pool made at
# another level.
.rt_mi_pool_cached <- function(x, vce = "stata", cluster = NULL, level = 0.95, any_level = FALSE) {
  cache <- x$pool_cache
  if (!is.environment(cache)) return(.rt_mi_pool(x$analyses, vce, cluster, level))
  for (e in cache$entries) {
    if (identical(e$vce, vce) && identical(e$cluster, cluster) && (any_level || identical(e$level, level))) {
      return(e$pool)
    }
  }
  p <- .rt_mi_pool(x$analyses, vce, cluster, level)
  cache$entries <- c(cache$entries, list(list(vce = vce, cluster = cluster, level = level, pool = p)))
  p
}

#' @export
tt_model_info.tt_mi <- function(fit, ...) {
  info <- tt_model_info(fit$analyses[[1]], ...)
  info$mi <- TRUE
  info$mi_m <- length(fit$analyses)
  info
}

#' @export
tt_vce_types.tt_mi <- function(fit) {
  Reduce(intersect, lapply(fit$analyses, tt_vce_types))
}

#' @export
tt_ci_methods.tt_mi <- function(fit) "wald"

# tt_coef() and tt_vcov() pool directly, outside regtab(), so they run the
# same compatibility gate first (Codex audit R3: an imputation with an extra
# covariate was pooled on the first fit's terms, its extra coefficient
# silently dropped, while regtab() refused the same object).
#' @export
tt_coef.tt_mi <- function(fit, ...) {
  .rt_mi_check(fit, 1L)
  b1 <- .rt_mi_coef(fit$analyses[[1]])
  out <- stats::setNames(rep(NA_real_, length(b1)), names(b1))
  p <- .rt_mi_pool(fit$analyses)
  out[p$term] <- p$estimate
  out
}

#' @export
tt_vcov.tt_mi <- function(fit, vce = "stata", cluster = NULL, ...) {
  .rt_mi_check(fit, 1L)
  b1 <- .rt_mi_coef(fit$analyses[[1]])
  nm <- names(b1)
  p <- .rt_mi_pool(fit$analyses, vce, cluster)
  V <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
  V[p$term, p$term] <- attr(p, "V")
  V
}

#' @export
tt_wald_df.tt_mi <- function(fit, vce = "stata", cluster = NULL, ...) {
  cli::cli_abort("A multiply imputed model has one degrees-of-freedom value per coefficient (Rubin's rules); there is no single Wald df.",
                 call = NULL)
}

# Rows: the first imputation's rows (labels, levels, Reference rows,
# statuses), with every estimated coefficient's numbers replaced by the
# pooled ones on the display scale.
#' @export
tt_regtab_rows.tt_mi <- function(fit, info, level = 0.95, ci_method = "wald", interactions = "fvgen",
                                 xsymbol = "\u00d7", vsref = NULL, vce = "stata", cluster = NULL, ...) {
  if (!identical(ci_method, "wald")) {
    cli::cli_abort("{.code ci_method = \"profile\"} is not available for a multiply imputed model: pooled intervals are Wald intervals.",
                   call = NULL)
  }
  pool <- .rt_mi_pool_cached(fit, vce, cluster, level)
  rows_k <- function(k) {
    tt_regtab_rows(fit$analyses[[k]], info, level = level, ci_method = "wald", interactions = interactions,
                   xsymbol = xsymbol, vsref = vsref, vce = vce, cluster = .rt_mi_cluster_k(cluster, k), ...)
  }
  rows <- rows_k(1L)
  # Every imputation's own rows must map its coefficients as imputation 1's
  # do, or it would be pooled into rows it does not support (Codex audit 2,
  # F06: a coding regtab refuses for a single fit passed as a later
  # imputation). They are built only for an imputation whose design
  # signature differs from imputation 1's (Codex audit 2, F06 review).
  map1 <- rows[!is.na(rows$term), c("key", "term")]
  sig1 <- .rt_mi_design_sig(fit$analyses[[1L]])
  for (k in seq_along(fit$analyses)[-1L]) {
    if (isTRUE(all.equal(.rt_mi_design_sig(fit$analyses[[k]]), sig1))) next
    rk <- tryCatch(rows_k(k), error = function(e) {
      cli::cli_abort("Imputation {k} of the multiply imputed model: its rows cannot be built.", parent = e, call = NULL)
    })
    mk <- rk[!is.na(rk$term), c("key", "term")]
    if (!identical(unname(as.list(mk)), unname(as.list(map1)))) {
      cli::cli_abort(c(
        "Imputation {k} of the multiply imputed model maps its coefficients to other rows than imputation 1.",
        "i" = "Fit every imputation with the same formula, contrasts and factor levels."
      ), call = NULL)
    }
  }
  hit <- match(rows$term, pool$term)
  est <- rows$status %in% "est"
  if (any(est & (is.na(hit) | rows$ancillary))) {
    cli::cli_abort("Internal error: a row of the multiply imputed model has no pooled coefficient ({.val {rows$term[est & is.na(hit)]}}).",
                   call = NULL)
  }
  tr <- if (isTRUE(info$exponentiate)) exp else identity
  rows$estimate[est] <- tr(pool$estimate[hit[est]])
  rows$conf.low[est] <- tr(pool$conf.low[hit[est]])
  rows$conf.high[est] <- tr(pool$conf.high[hit[est]])
  rows$p.value[est] <- pool$p.value[hit[est]]
  rows
}

# Stats: the imputations' counts when they agree (Stata posts a result that
# varies across imputations as missing), Imputations, Largest FMI (over
# every estimated coefficient, the intercept included, as e(fmi_max_mi));
# nothing from the likelihood, R-squared, Root MSE or F.
#' @export
tt_model_stats.tt_mi <- function(fit, info, vce = "stata", cluster = NULL, ...) {
  a <- fit$analyses
  # Only the counts are read from the imputations, and they do not depend
  # on the variance: each imputation's stats under the model variance (no
  # sandwich per imputation for an F or Root MSE that is not shown).
  st <- lapply(a, function(f) .rt_quiet_zero_weight(tt_model_stats(f, info)))
  same <- function(nm) {
    v <- vapply(st, function(s) as.numeric(s[[nm]] %||% NA_real_)[1], 0)
    if (anyNA(v) || any(v != v[1])) NA_real_ else v[1]
  }
  pool <- .rt_mi_pool_cached(fit, vce, cluster, any_level = TRUE)
  s <- list(N = same("N"), N_sub = same("N_sub"), events = same("events"), ll = NA_real_, rank = NA_real_,
            aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_, r2_a = NA_real_,
            groups = NA_real_, qic = NA_real_, icc = NA_real_, rmse = NA_real_, F = NA_real_,
            mi_m = as.numeric(length(a)), fmi = if (nrow(pool)) max(pool$fmi) else NA_real_)
  for (nm in c("nsub_note", "events_note")) {
    notes <- unique(vapply(st, function(x) x[[nm]] %||% "", ""))
    if (length(notes) == 1L && nzchar(notes)) s[[nm]] <- notes
  }
  s
}
