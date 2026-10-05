# Stored fitted-model sample evidence. These helpers deliberately do not call
# model.frame(), nobs(), or a fitting call: an anchored frame may have been
# reconstructed from data that no longer represent the original input.
.tt_sample_model_field <- function(fit, name) {
  if (is.list(fit) && !isS4(fit)) fit[[name, exact = TRUE]] else NULL
}

.tt_sample_model_size <- function(x) {
  if (is.null(x) || !(is.atomic(x) || is.data.frame(x))) return(NA_real_)
  as.numeric(NROW(x))
}

.tt_sample_model_population <- function(fit, id, model = NA_real_, imputation = NA_real_,
                                        command = "regtab", variable = NA_character_,
                                        scope = NULL, input_n = NULL, input_basis = NULL) {
  if (inherits(fit, "tt_mi")) {
    parts <- lapply(seq_along(fit$analyses), function(k) {
      .tt_sample_model_population(fit$analyses[[k]], id, model, k, command,
                                  variable, scope = "imputation")
    })
    return(.tt_sample_bind(parts, prefixes = paste0("imputation", seq_along(parts))))
  }
  if (inherits(fit, "tt_uv")) {
    parts <- lapply(seq_along(fit$fits), function(k) {
      .tt_sample_model_population(fit$fits[[k]], id, model, imputation, command,
                                  fit$x[[k]], scope = "model",
                                  input_n = fit$sample_input_n,
                                  input_basis = "records supplied to regtab_uv()")
    })
    return(.tt_sample_bind(parts, prefixes = paste0("univariable", seq_along(parts))))
  }
  scope <- scope %||% if (!is.na(imputation)) "imputation" else "model"
  values <- stats::setNames(rep(list(NA_real_), length(.tt_sample_metrics)), .tt_sample_metrics)
  bases <- reasons <- statuses <- list()
  set <- function(metric, value, basis) {
    values[[metric]] <<- as.numeric(value)
    bases[[metric]] <<- basis
  }
  if (is.data.frame(fit)) {
    glance <- attr(fit, "glance", exact = TRUE)
    n <- if (is.list(glance)) glance[["nobs", exact = TRUE]] else NULL
    if (is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0) {
      set("reported_n", n, "user-supplied glance nobs; not coefficient-row count")
    }
    values[c("fitted_n", "frame_n")] <- NULL
    return(.tt_sample_population(id, command, "summary", variable = variable,
                                 model = model, imputation = imputation,
                                 weight_type = "unknown", values = values, bases = bases))
  }
  field <- function(name) .tt_sample_model_field(fit, name)
  mer <- inherits(fit, "merMod")
  tmb <- inherits(fit, "glmmTMB")
  cox <- inherits(fit, "coxph")
  surv <- inherits(fit, "survreg")
  survey <- inherits(fit, c("svyglm", "svrepglm"))
  estimated <- inherits(fit, c("glm_weightit", "lm_weightit"))
  mf <- if (mer) methods::slot(fit, "frame") else if (tmb) field("frame") else field("model")
  if (!is.data.frame(mf)) mf <- NULL
  frame_n <- if (!is.null(mf)) as.numeric(nrow(mf)) else NA_real_
  eligible <- frame_n
  if (is.na(frame_n)) {
    for (name in c("y", "x")) {
      representation <- field(name)
      if (!is.null(dim(representation))) {
        frame_n <- .tt_sample_model_size(representation)
        bases$frame_n <- paste0("stored ", name, " representation rows")
        break
      }
    }
  }
  if (is.na(eligible)) {
    candidates <- if (inherits(fit, "lme")) c("groups", "residuals", "fitted") else
      if (cox || surv) c("y", "linear.predictors", "residuals") else
        c("residuals", "fitted.values", "linear.predictors", "y")
    for (name in candidates) {
      size <- .tt_sample_model_size(field(name))
      if (!is.na(size)) {
        eligible <- size
        bases$eligible_n <- paste0("stored accepted-record ", name, " count")
        break
      }
    }
  }
  if (!is.na(frame_n)) set("frame_n", frame_n, bases$frame_n %||% "stored model-frame rows")
  # coxph's n survives risk-set expansion; nobs.coxph instead counts events.
  if (cox || inherits(fit, "crr")) {
    n <- field("n")
    if (is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0) eligible <- as.numeric(n)
  }
  if (!is.na(eligible)) set("eligible_n", eligible,
                           if (cox || inherits(fit, "crr")) "stored accepted-record n" else
                             bases$eligible_n %||% "stored estimation-sample records")
  origin <- field("data")
  # Native glm/lme retain supplied data. geeglm retains its initial glm's
  # data without replacing it; survey/estimated-weight subclasses may not.
  if (is.null(input_n) && class(fit)[1L] %in% c("glm", "negbin", "geeglm", "lme") &&
      is.data.frame(origin) && !survey && !estimated) {
    input_n <- as.numeric(nrow(origin))
    input_basis <- "native fit's stored original data rows"
  }
  if (!is.null(input_n)) set("input_n", input_n, input_basis %||% "stored original input count")
  reasons$input_n <- if (is.null(input_n)) "original fitting input is not directly retained" else ""

  call <- if (mer) methods::slot(fit, "call") else field("call")
  explicit_weights <- !is.null(call) && !is.null(call[["weights"]])
  if (inherits(fit, "lme")) explicit_weights <- FALSE
  frame_weights <- if (!is.null(mf)) mf[["(weights)", exact = TRUE]] else NULL
  w <- if (inherits(fit, "glm")) field("prior.weights") else frame_weights %||% field("weights")
  response_class <- unname(attr(field("terms"), "dataClasses")[1L])
  # nnet stores factor-response case weights as an N-by-1 matrix. Count
  # responses instead fold row totals into it, so their original weights
  # must not be recovered by the same conversion.
  if (inherits(fit, "multinom") && length(response_class) == 1L &&
      response_class %in% c("factor", "ordered", "character") &&
      is.numeric(w) && is.matrix(w) && ncol(w) == 1L &&
      !is.na(eligible) && nrow(w) == eligible) w <- as.numeric(w)
  if (inherits(fit, "lme")) w <- NULL # varFunc precision factors are not case weights.
  aligned <- !is.na(eligible) && is.numeric(w) && is.null(dim(w)) &&
    length(w) == eligible && all(is.finite(w) & w >= 0)
  keep <- NULL
  if (aligned) {
    keep <- w > 0
    set("zero_weight_n", sum(!keep), "stored accepted-record prior/case weights")
    set("fitted_n", sum(keep), "accepted records with positive stored prior/case weights")
  } else if (!is.na(eligible) && (!explicit_weights || cox || inherits(fit, "lme"))) {
    keep <- rep(TRUE, eligible)
    set("zero_weight_n", 0, if (cox) "coxph requires positive case weights" else
          "native fit has no case-weight argument")
    set("fitted_n", eligible, "accepted estimation records; no zero case weights")
  }
  if (!is.null(keep)) set("used_n", sum(keep), bases$fitted_n)
  if (!is.null(input_n) && !is.na(values$used_n)) {
    set("excluded_n", input_n - values$used_n, "stored original input minus contributing records")
  }
  response <- if (!is.null(mf)) tryCatch(stats::model.response(mf), error = function(e) NULL) else field("y")
  if (!is.na(eligible) && isTRUE(.tt_sample_model_size(response) == eligible)) {
    missing <- is.na(response)
    if (!is.null(dim(missing))) missing <- rowSums(missing) > 0
    set("observed_n", sum(!missing), "nonmissing stored estimation-sample response records")
    set("missing_n", sum(missing), "missing responses within the stored estimation sample")
  } else {
    reasons$observed_n <- reasons$missing_n <- "response records are not retained in the accepted-record representation"
  }

  raw_w <- frame_weights
  if (survey) {
    design <- field("survey.design")
    raw_w <- if (is.list(design) && is.numeric(design$prob)) 1 / design$prob else
      if (is.list(design)) design$pweights else NULL
  }
  family <- field("family")
  grouped_binomial <- inherits(fit, "glm") && (family$family %||% "") %in% c("binomial", "quasibinomial") &&
    (!is.null(dim(response)) && NCOL(response) == 2L ||
       identical(unname(attr(field("terms"), "dataClasses")[1L]), "nmatrix.2"))
  if (is.null(raw_w) && aligned && !grouped_binomial && !survey) raw_w <- w
  if (is.null(raw_w) && !explicit_weights && !survey && !is.na(eligible)) raw_w <- rep(1, eligible)
  valid_raw <- !is.na(eligible) && is.numeric(raw_w) && is.null(dim(raw_w)) &&
    length(raw_w) == eligible && all(is.finite(raw_w) & raw_w >= 0)
  weight_type <- if (survey) "survey" else if (estimated) "estimated" else
    if (valid_raw && (explicit_weights || any(raw_w != 1))) "prior/case" else
      if (explicit_weights) "unknown" else "none"
  if (valid_raw && !is.null(keep)) {
    diagnostics <- .tt_sample_weights(raw_w[keep])
    values[names(diagnostics$values)] <- diagnostics$values
    bases[names(diagnostics$bases)] <- diagnostics$bases
    reasons[names(diagnostics$reasons)] <- diagnostics$reasons
    statuses[names(diagnostics$statuses)] <- diagnostics$statuses
  } else {
    reasons$weight_sum <- reasons$effective_n <- "original-scale accepted-record weights are not retained or aligned"
  }
  reported <- if (cox) field("nevent") else if (inherits(fit, c("polr", "clm"))) field("nobs") else
    if (mer || tmb || inherits(fit, "lme")) eligible else values$used_n
  if (is.numeric(reported) && length(reported) == 1L && is.finite(reported) && reported >= 0) {
    set("reported_n", reported, if (cox) "native Cox event count" else "native stored fit N convention")
  }
  omissions <- if (!is.null(mf)) attr(mf, "na.action", exact = TRUE) else NULL
  omissions <- omissions %||% field("na.action")
  omitted <- if (inherits(omissions, c("omit", "exclude"))) as.numeric(length(omissions)) else NA_real_
  exclusions <- list()
  if (!is.na(omitted) && omitted > 0) {
    exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion("input_to_eligible",
      "missing model variables after subsetting", omitted, "stored na.action omissions; not an original-input total")
  }
  if (!is.null(input_n) && !is.na(eligible)) {
    other <- input_n - eligible - if (is.na(omitted)) 0 else omitted
    if (other > 0) exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion("input_to_eligible",
      if (is.na(omitted)) "input filtering; individual reasons unavailable" else "other input filtering before complete-case selection",
      other, "stored original input minus accepted records and separately stored omissions")
  }
  if (!is.na(values$zero_weight_n) && values$zero_weight_n > 0) {
    exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion("eligible_to_used",
      "zero prior/case weight", values$zero_weight_n, bases$zero_weight_n)
  }
  .tt_sample_population(id, command, scope, variable = variable, model = model,
                        imputation = imputation, weight_type = weight_type,
                        values = values, bases = bases, reasons = reasons,
                        statuses = statuses,
                        exclusions = if (length(exclusions)) do.call(rbind, exclusions) else NULL)
}

# Reuse the statistics already calculated by regtab rather than calling a
# native count method again. Wrapper sources keep each fit's own convention.
.tt_sample_model_reported <- function(sample, stats) {
  if (nrow(sample$populations) != 1L) return(sample)
  value <- stats$N_sub
  source <- "N_sub"
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    value <- stats$N %||% NA_real_
    source <- "N"
  }
  if ((stats$nsub_note %||% "") %in% c("varies", "unverified")) value <- NA_real_
  available <- is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0
  row <- sample$measures$metric == "reported_n"
  sample$measures$value[row] <- if (available) as.numeric(value) else NA_real_
  sample$measures$status[row] <- if (available) "available" else "unavailable"
  sample$measures$basis[row] <- paste0("existing regtab model statistic ", source)
  sample$measures$reason[row] <- if (available) "" else "model statistic has no verified single reported N"
  sample
}
