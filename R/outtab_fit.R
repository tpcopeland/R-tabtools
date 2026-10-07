.ot_fit_identity <- function(fit, formula, data, exposure, cc_ids, frozen_frame) {
  if (!identical(class(fit), c("glm", "lm")) || !is.data.frame(fit$model) ||
      !is.matrix(fit$x) || is.null(fit$y)) .ot_abort("Function estimators must return an ordinary glm retaining model, x and y.", "capability")
  family <- fit$family
  if (!((family$family == "poisson" && family$link == "log") ||
        (family$family == "binomial" && family$link == "logit"))) .ot_abort("Only Poisson-log and binomial-logit glm fits are supported.", "capability")
  if (length(fit$prior.weights) != nrow(fit$model) || anyNA(fit$prior.weights) || any(fit$prior.weights != 1) || !is.null(fit$offset)) .ot_abort("Weighted/offset estimators require a separately supported capability.", "capability")
  if (!identical(as.list(stats::formula(fit)), as.list(formula)) ||
      !identical(attr(fit$terms, "term.labels"), attr(stats::terms(formula, data = data), "term.labels")) ||
      !identical(attr(fit$terms, "intercept"), attr(stats::terms(formula), "intercept")) ||
      !identical(all.vars(stats::formula(fit)), all.vars(formula))) .ot_abort("Returned glm formula does not match the explicit model snapshot.", "source")
  names <- rownames(fit$model)
  ids <- match(names, rownames(data))
  if (anyNA(ids) || anyDuplicated(ids) || any(!ids %in% cc_ids)) .ot_abort("Returned glm row identities are not a unique subset of expected complete cases.", "source")
  # The expected transformed values were frozen before invoking the fitter;
  # do not evaluate a potentially mutated formula environment after it returns.
  expected <- droplevels(frozen_frame[match(names, rownames(frozen_frame)), , drop = FALSE])
  # Data-frame row subsetting can discard matrix transformation metadata
  # (scale center/scale; poly coefficients/degree/class). Restore only the
  # original nonstructural attributes; current dimensions and row mapping stay.
  for (column in names(expected)) {
    original <- frozen_frame[[column]]
    if (!is.matrix(original)) next
    original_attributes <- attributes(original)
    for (attribute in setdiff(names(original_attributes), c("dim", "dimnames", "names"))) {
      attr(expected[[column]], attribute) <- original_attributes[[attribute]]
    }
  }
  if (!identical(names(fit$model), names(expected)) ||
      !all(vapply(names(expected), function(column)
        identical(fit$model[[column]], expected[[column]], single.NA = FALSE), logical(1)))) {
    .ot_abort("Returned glm model frame differs from the explicit snapshot.", "source")
  }
  X <- stats::model.matrix(fit$terms, expected, contrasts.arg = fit$contrasts)
  y <- stats::model.response(expected)
  if (!identical(fit$x, X, single.NA = FALSE) || !identical(fit$y, y, single.NA = FALSE)) .ot_abort("Returned glm retained design/response differs from the snapshot.", "source")
  if (!identical(names(fit$fitted.values), names) || length(fit$linear.predictors) != length(ids)) .ot_abort("Returned glm fitted values are not aligned with retained rows.", "source")
  beta <- stats::coef(fit); beta[is.na(beta)] <- 0
  eta <- drop(X %*% beta)
  if (!isTRUE(all.equal(unname(eta), unname(fit$linear.predictors), tolerance = 1e-8)) ||
      !isTRUE(all.equal(unname(family$linkinv(eta)), unname(fit$fitted.values), tolerance = 1e-8))) .ot_abort("Returned glm coefficients and retained fitted values disagree.", "source")
  if (sum(colnames(X) == paste(deparse(as.name(exposure)), collapse = "")) != 1L) .ot_abort("The retained design has no unique exposure coefficient.", "capability")
  list(ids = ids, X = X, y = y, family = family)
}

.ot_fit <- function(samples, block, spec, estimator, estimator_args, vce, cluster,
                    level, minevents) {
  result <- list(b = NA_real_, lb = NA_real_, ub = NA_real_, native_rc = NA_real_,
    status = "empty", reason = "engine_failure", N = NA_real_, N_cc = NA_real_,
    N_clust = NA_real_, df_m = NA_real_, converged = NA_real_, ids = integer(),
    cc_ids = integer(), warnings = character(), error_class = character(), error_message = "",
    coefficient = NA_real_, variance = NA_real_, effect_scale = "", vce = vce,
    reference = "normal", level = level, exponentiation_count = 0L)
  formula <- .ot_formula(spec, block$outcome, samples$exposure, samples$data)
  if (block$counts[["e1"]] < minevents) {
    result$native_rc <- -1; result$status <- "notest"; result$reason <- "minimum_exposed_events"
    return(result)
  }
  d <- samples$data[block$ids, , drop = FALSE]
  frame <- stats::model.frame(formula, data = d, na.action = stats::na.pass)
  cc <- stats::complete.cases(frame)
  result$cc_ids <- block$ids[cc]; result$N_cc <- sum(cc)
  warning_text <- character()
  engine <- tryCatch(withCallingHandlers({
    if (is.function(estimator)) {
      do.call(estimator, c(list(formula = formula, data = d), estimator_args))
    } else {
      family <- if (estimator == "logit") stats::binomial() else stats::poisson()
      if (!"control" %in% names(estimator_args)) {
        estimator_args$control <- stats::glm.control(epsilon = 1e-12, maxit = 100L)
      }
      do.call(stats::glm, c(list(formula = formula, data = d, family = family,
        model = TRUE, x = TRUE, y = TRUE, na.action = stats::na.omit), estimator_args))
    }
  }, warning = function(w) {
    warning_text <<- c(warning_text, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) e)
  result$warnings <- warning_text
  if (inherits(engine, "error")) {
    result$error_class <- class(engine); result$error_message <- conditionMessage(engine)
    return(result)
  }
  evidence <- .ot_fit_identity(engine, formula, samples$data, samples$exposure, result$cc_ids, frame)
  result$ids <- evidence$ids
  result$evidence <- list(formula = paste(deparse(formula, width.cutoff = 500L), collapse = "\n"),
    terms = attr(engine$terms, "term.labels"), contrasts = engine$contrasts,
    family = evidence$family$family,
    link = evidence$family$link, design = evidence$X, response = evidence$y,
    coefficients = stats::coef(engine), row_ids = evidence$ids,
    original_row_names = rownames(engine$model), source = "verified retained glm snapshot")
  result$N <- length(evidence$ids); result$df_m <- engine$rank - as.integer(attr(engine$terms, "intercept"))
  result$converged <- as.numeric(isTRUE(engine$converged))
  result$effect_scale <- if (evidence$family$family == "binomial") "OR" else if (identical(estimator, "modified_poisson")) "RR" else "IRR"
  if (!isTRUE(engine$converged)) {
    result$native_rc <- 430; result$reason <- "nonconvergence"; return(result)
  }
  if (result$N < result$N_cc) {
    result$native_rc <- -2; result$reason <- "estimator_sample_reduced"; return(result)
  }
  g <- samples$data[[samples$exposure]][evidence$ids]
  if (!length(g) || any(vapply(0:1, function(a) sum(evidence$y[g == a]) == 0, logical(1)))) {
    result$native_rc <- 459; result$reason <- "boundary_events"; return(result)
  }
  term <- paste(deparse(as.name(samples$exposure)), collapse = "")
  b <- unname(stats::coef(engine)[term])
  V <- tryCatch(if (identical(vce, "model")) stats::vcov(engine) else {
      .rt_vcov_sandwich(engine, vce = vce,
        cluster = if (is.null(cluster)) NULL else samples$data[[cluster]][evidence$ids])
    }, error = function(e) e)
  if (inherits(V, "error")) {
    result$error_class <- class(V); result$error_message <- conditionMessage(V)
    result$reason <- "variance_engine_failure"
    return(result)
  }
  if (identical(vce, "cluster")) result$N_clust <- length(unique(samples$data[[cluster]][evidence$ids]))
  variance <- unname(V[term, term])
  result$coefficient <- b; result$variance <- variance
  if (!is.finite(b) || !is.finite(variance) || variance <= 0) {
    result$native_rc <- 459; result$reason <- "unavailable_exposure_variance"; return(result)
  }
  z <- stats::qnorm((1 + level) / 2)
  tuple <- exp(c(b, b - z * sqrt(variance), b + z * sqrt(variance)))
  if (any(!is.finite(tuple)) || any(tuple <= 0)) {
    result$native_rc <- 459; result$reason <- "unavailable_ratio_interval"; return(result)
  }
  result$b <- tuple[1L]; result$lb <- tuple[2L]; result$ub <- tuple[3L]
  result$native_rc <- 0; result$status <- "est"; result$reason <- ""
  result$exponentiation_count <- 1L
  result
}
