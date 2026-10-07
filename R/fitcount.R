#' Freeze unweighted counts for a fitted regression model
#'
#' Capture counts immediately after fitting. With `data = NULL`, only source
#' data actually retained by the fit are used. Explicit `data` declares the
#' original fitting source: comparable retained fields must agree exactly and
#' its model frame must reproduce the fit. Compatibility cannot authenticate
#' historical predictors the fitter did not retain. Subsequent reuse compares
#' all captured fit evidence and the frozen record exactly, before restoring a
#' local model frame; later live data never replace captured evidence.
#'
#' Counts describe positive-weight accepted records, without weighting. Generic
#' R case weights require an explicit `aweight` or `pweight` declaration;
#' frequency/importance weights and survey subpopulations are refused. This is
#' not a weighted event statistic or an inference/disclosure guarantee.
#' Invalid count inputs, ambiguous sample/capability evidence and stale records
#' signal `tabtools_error_fitcount`. Frozen formula environments are not a
#' historical-source claim; field availability and source origin are explicit.
#' @param fit A supported fitted model, with unambiguous accepted row identities.
#' @param events Exact column name containing nonnegative integer event counts.
#' @param people Optional exact column name of nonmissing person identifiers.
#' @param exposure Optional exact column name of finite nonnegative exposure.
#' @param terms Capture factor-level and plain 0/1 indicator event counts.
#' @param data Optional data.frame declared to be the original fitting source.
#' @param weight_type NULL for an unweighted fit, or `"aweight"`/`"pweight"`.
#' @return A `tt_fitcount` record containing immutable captured evidence,
#'   sample provenance, counts, term counts and a frozen source/model frame.
#' @examples
#' d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6,
#'                 event = c(1, 0, 2, 0, 1, 0))
#' fit <- lm(y ~ x, data = d)
#' counts <- tt_fitcount(fit, events = "event", data = d, terms = TRUE)
#' counts$counts
#' @export
tt_fitcount <- function(fit, events, people = NULL, exposure = NULL,
                        terms = FALSE, data = NULL, weight_type = NULL) {
  if (inherits(fit, c("tt_mi", "tt_uv")) || is.data.frame(fit)) {
    .fc_abort("Fit counts require one fitted model with an estimation sample.")
  }
  .rt_check_model(fit, 1L)
  if (!is.logical(terms) || length(terms) != 1L || is.na(terms)) .fc_abort("`terms` must be TRUE or FALSE.")
  for (nm in c("events", "people", "exposure")) {
    val <- get(nm)
    if ((nm == "events" || !is.null(val)) &&
        (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val))) {
      .fc_abort(paste0("`", nm, "` must be one exact column name."))
    }
  }
  if (inherits(fit, c("svyglm", "svrepglm"))) {
    .fc_abort("Survey fit counts require unambiguous analysis-population evidence; survey capture is unsupported.")
  }
  original <- .fc_evidence(fit)
  retained <- .fc_retained_frame(fit)
  src <- data
  origin <- "caller_declared"
  if (is.null(src)) {
    src <- .fc_field(fit, "data")
    if (!is.data.frame(src)) src <- retained
    origin <- "retained_fit"
  }
  if (!is.data.frame(src)) .fc_abort("Required source fields were not retained; supply the declared original `data` explicitly.")
  rn <- if (!is.null(retained)) rownames(retained) else .rt_fit_rownames(fit)
  if (is.null(rn) || !length(rn) || anyNA(rn) || anyDuplicated(rn) ||
      anyDuplicated(rownames(src)) || !all(rn %in% rownames(src))) {
    .fc_abort("The estimation sample cannot be aligned unambiguously by row identity.")
  }
  check_input_vector <- function(value, name, n) {
    if (!is.atomic(value) || !is.null(dim(value)) || length(value) != n) {
      .fc_abort(paste0("Count column `", name, "` must contain one atomic vector value per record."))
    }
  }
  # Reject multidimensional columns before data.frame row alignment can
  # dispatch their own subsetting methods or flatten the count inputs.
  for (name in intersect(c(events, people, exposure), names(src))) {
    check_input_vector(src[[name]], name, nrow(src))
  }
  source <- .fc_align_source(src, rn)
  # Exact retained source comparison precedes computational compatibility.
  stored_data <- .fc_field(fit, "data")
  if (is.data.frame(stored_data)) {
    if (!all(rn %in% rownames(stored_data))) .fc_abort("Retained source row identities disagree.")
    held <- .fc_align_source(stored_data, rn)
    for (nm in intersect(names(source), names(held))) {
      if (!identical(.fc_freeze(source[[nm]]), .fc_freeze(held[[nm]]))) .fc_abort(paste0("Declared source disagrees with retained field `", nm, "`."))
    }
  }
  mf <- if (is.null(data) && !is.null(retained)) retained else
    tryCatch(.rt_align_frame(.rt_eval_frame(fit, src), rn), error = function(e) {
      .fc_abort("The declared source cannot supply the fitting model frame.", e)
    })
  if (!is.data.frame(mf) || !identical(rownames(mf), rn)) .fc_abort("Declared frame row identities disagree with the fit.")
  if (!is.null(retained)) {
    # Rebuilt survival frames can lose fit-retained Stata label/code metadata.
    # Restore from the declared, value-checked source, then compare exactly;
    # never add attributes the retained fit did not carry.
    labelled <- .rt_restore_attrs(mf, .fc_restore(fit, mf, source), warn = FALSE)
    for (nm in intersect(names(mf), names(retained))) for (a in c("label", "labels")) {
      if (is.null(attr(mf[[nm]], a, exact = TRUE)) &&
          !is.null(attr(retained[[nm]], a, exact = TRUE))) {
        attr(mf[[nm]], a) <- attr(labelled[[nm]], a, exact = TRUE)
      }
    }
    for (nm in names(retained)) {
      if (!nm %in% names(mf) || !identical(.fc_freeze(retained[[nm]]), .fc_freeze(mf[[nm]]))) {
        .fc_abort(paste0("Declared frame disagrees with retained field `", nm, "`."))
      }
    }
  }
  if (!isS4(fit) && class(fit)[1L] %in% .rt_anchor_classes && !.rt_frame_matches_fit(fit, mf)) {
    .fc_abort("The declared source does not reproduce the fitted response and linear predictor.")
  }
  if (!is.null(.fc_field(fit, "x"))) {
    local_design <- .fc_restore(fit, mf, source)
    design <- tryCatch(stats::model.matrix(local_design), error = function(e) .fc_abort("The declared source cannot reproduce the retained design.", e))
    if (!identical(.fc_freeze(design), .fc_freeze(.fc_field(fit, "x")))) .fc_abort("Declared source disagrees with the retained design.")
  }
  response <- stats::model.response(mf)
  retained_response <- .fc_field(fit, "y")
  if (!is.null(retained_response) && (inherits(fit, "lm") && !inherits(fit, "glm") ||
      inherits(fit, c("coxph", "survreg")))) {
    if (!identical(.fc_freeze(response), .fc_freeze(retained_response))) .fc_abort("Declared source disagrees with the retained response.")
  }
  w <- stats::model.weights(mf)
  if (!is.null(w) && (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w < 0))) .fc_abort("Fit weights must be finite and nonnegative.")
  weighted <- !is.null(w)
  if (is.null(weight_type)) {
    if (weighted) .fc_abort("Generic case weights require explicit `weight_type = 'aweight'` or `'pweight'`.")
    weight_type <- "unweighted"
  }
  if (!is.character(weight_type) || length(weight_type) != 1L || is.na(weight_type) ||
      !weight_type %in% c("unweighted", "aweight", "pweight") ||
      (weighted && weight_type == "unweighted") || (!weighted && weight_type != "unweighted")) {
    .fc_abort("Only unweighted fits or declared aweight/pweight record counts are supported; fweight/iweight are refused.")
  }
  take <- if (weighted) w > 0 else rep(TRUE, nrow(mf))
  if (!any(take)) .fc_abort("The estimation sample is empty.")
  get_input <- function(name) {
    if (is.null(name)) return(NULL)
    if (!name %in% names(source)) .fc_abort(paste0("Required count column `", name, "` is unavailable."))
    value <- source[[name]]
    # Check the column before subsetting: [take] would flatten a matrix
    # and silently count more than one value per accepted record.
    check_input_vector(value, name, nrow(source))
    value[take]
  }
  ev <- get_input(events)
  if ((!is.numeric(ev) && !is.logical(ev)) || anyNA(ev) || any(!is.finite(ev)) || any(ev < 0 | ev != floor(ev))) .fc_abort("Events must be finite nonnegative integer counts in accepted records.")
  pp <- get_input(people)
  if (!is.null(pp) && (!is.atomic(pp) || !is.null(dim(pp)) || anyNA(pp) || (is.character(pp) && any(pp == "")))) .fc_abort("People identifiers must be atomic, nonmissing and nonempty in accepted records.")
  px <- get_input(exposure)
  if (!is.null(px) && (!is.numeric(px) || anyNA(px) || any(!is.finite(px)) || any(px < 0))) .fc_abort("Exposure must be finite and nonnegative in accepted records.")
  local <- .fc_restore(fit, mf, source)
  row_counts <- data.frame()
  levels <- data.frame()
  states <- data.frame()
  if (terms) {
    rows <- tt_regtab_rows(local, tt_model_info(local), vce = "model", interactions = "native")
    rows <- .rt_count_rows(rows, local)
    # Use the same validated code restoration as row extraction before
    # positive-weight subsetting; [.factor otherwise drops its code labels.
    term_frame <- .rt_restore_attrs(mf, local, warn = FALSE)
    info <- .fc_term_counts(rows, .fc_align_source(term_frame, rownames(term_frame)[take]), ev)
    row_counts <- info$terms
    levels <- info$levels
    state_fields <- c("key", "status", "parent_key", "term_signature",
                      intersect(c("constraint_value", "constraint_source"), names(rows)))
    states <- rows[, state_fields, drop = FALSE]
    states$state_source <- ifelse(states$status == "ref", "fit_contrasts",
      ifelse(states$status == "constrained", "declared_fit_constraint", "fit_coefficient_and_design"))
  }
  result <- list(schema_version = 1L, identity = original,
                 evidence = data.frame(field = names(original$fields),
                   available = vapply(original$fields, function(x) !is.null(x), TRUE),
                   origin = "retained_fit", stringsAsFactors = FALSE),
                 sample = list(n = sum(take), row_ids = rn[take], weight_type = weight_type,
                               excluded_zero_weight = sum(!take), source_origin = origin),
                 counts = c(obs = sum(take), events = sum(ev),
                   people = if (is.null(pp)) NA_real_ else length(unique(pp)),
                   people_ev = if (is.null(pp)) NA_real_ else length(unique(pp[ev > 0])),
                   exposure = if (is.null(px)) NA_real_ else sum(px)),
                 terms = row_counts, levels = levels, states = states,
                 snapshot = list(source = .fc_freeze(source), frame = .fc_freeze(mf),
                                 source_origin = origin, compatibility = "initial_only"))
  result$seal <- serialize(result, NULL, version = 2L)
  structure(result, class = "tt_fitcount")
}

# Row subsetting drops factor labels; keep metadata from the same source
# column, without changing its selected values, levels or row identities.
.fc_align_source <- function(source, rn) {
  out <- source[match(rn, rownames(source)), , drop = FALSE]
  for (nm in names(source)) for (a in c("label", "labels")) {
    value <- attr(source[[nm]], a, exact = TRUE)
    if (!is.null(value) && is.null(attr(out[[nm]], a, exact = TRUE))) {
      attr(out[[nm]], a) <- value
    }
  }
  out
}

.fc_abort <- function(message, parent = NULL) {
  cli::cli_abort(message, class = "tabtools_error_fitcount", parent = parent, call = NULL)
}

.fc_field <- function(fit, name) {
  if (isS4(fit)) {
    if (name %in% methods::slotNames(fit)) methods::slot(fit, name) else NULL
  } else if (is.list(fit)) fit[[name, exact = TRUE]] else NULL
}

# Strip mutable environments while preserving types, row order and attributes.
.fc_freeze <- function(x) {
  if (inherits(x, "data.table")) x <- as.data.frame(x)
  if (is.environment(x)) return(NULL)
  if (is.function(x)) return(list(formals = .fc_freeze(formals(x)), body = body(x)))
  at <- attributes(x)
  if (is.list(x)) {
    x <- lapply(x, .fc_freeze)
  } else if (is.pairlist(x)) {
    x <- as.pairlist(lapply(x, .fc_freeze))
  }
  if (!is.null(at)) {
    at <- lapply(at, function(a) if (is.environment(a)) baseenv() else .fc_freeze(a))
    attributes(x) <- at
  }
  x
}

.fc_evidence <- function(fit) {
  fields <- c("model", "data", "frame", "x", "y", "weights", "prior.weights",
              "coefficients", "coef", "fitted.values", "fitted", "linear.predictors",
              "residuals", "na.action", "subset", "contrasts", "xlevels", "terms",
              "formula", "call", "n", "rank", "df.residual", "var", "Cns", "constraints",
              "beta", "theta", "devcomp", "family", "control", "method", "offset",
              "response", "coding", "levels", "qr", "assign", "deviance", "null.deviance",
              "aic", "loglik", "logLik", "means", "scale", "df.null", "converged", "iter")
  list(class = class(fit), fields = stats::setNames(lapply(fields, function(nm) .fc_freeze(.fc_field(fit, nm))), fields),
       coefficients = .fc_freeze(stats::coef(fit)),
       covariance = .fc_freeze(.rt_quiet_zero_weight(stats::vcov(fit))))
}

.fc_retained_frame <- function(fit) {
  for (nm in c("model", "frame")) {
    mf <- .fc_field(fit, nm)
    if (is.data.frame(mf)) return(mf)
  }
  NULL
}

.fc_restore <- function(fit, frame, source) {
  if (!isS4(fit) && is.list(fit)) {
    fit$model <- frame
    # The frozen source supplies labels too; prevent later live data lookup.
    fit$data <- source
    fit$tt_stale <- NULL
  }
  fit
}

.fc_validate <- function(record, fit) {
  if (!identical(class(record), "tt_fitcount") || !identical(record$schema_version, 1L)) .fc_abort("Each `fitcounts` entry must be a tt_fitcount record.")
  payload <- unclass(record)
  payload$seal <- NULL
  if (!identical(serialize(payload, NULL, version = 2L), record$seal)) .fc_abort("The frozen fit-count record has changed.")
  if (!identical(.fc_evidence(fit), record$identity)) .fc_abort("The fit differs from the captured fit-count evidence.")
  invisible(TRUE)
}

.fc_prepare <- function(fits, records, mincount) {
  M <- length(fits)
  if (inherits(records, "tt_fitcount") && M == 1L) records <- list(records)
  if (is.null(records)) records <- rep(list(NULL), M)
  if (!is.list(records) || length(records) != M) .fc_abort("`fitcounts` must have exactly one ordered entry per model (NULL when unused).")
  for (m in seq_len(M)) {
    if (.rt_failed(fits[[m]])) {
      if (!is.null(records[[m]])) .fc_abort("A failed placeholder cannot have fit counts.")
      next
    }
    if (!is.null(mincount) && inherits(fits[[m]], "tt_mi")) .fc_abort("`mincount` on pooled MI and MI esampvaryok are unsupported: there is no single fit-count sample.")
    if (is.null(records[[m]])) {
      if (!is.null(mincount)) .fc_abort(paste0("Model ", m, " requires fit counts with `terms = TRUE` for mincount."))
    } else {
      .fc_validate(records[[m]], fits[[m]])
      if (!is.null(mincount) && !nrow(records[[m]]$states)) .fc_abort("`mincount` requires fit counts captured with `terms = TRUE`.")
      fits[[m]] <- .fc_restore(fits[[m]], records[[m]]$snapshot$frame, records[[m]]$snapshot$source)
    }
  }
  identity <- data.frame(model_index = seq_len(M), repeated_fit = FALSE,
                         repeated_record = FALSE, first_occurrence = seq_len(M))
  for (m in seq_len(M)) {
    if (is.null(records[[m]]) || m == 1L) next
    for (j in seq_len(m - 1L)) {
      if (is.null(records[[j]]) || !identical(records[[m]]$identity, records[[j]]$identity)) next
      identity$repeated_fit[m] <- TRUE
      identity$first_occurrence[m] <- j
      identity$repeated_record[m] <- identical(records[[m]]$seal, records[[j]]$seal)
      break
    }
  }
  if (any(identity$repeated_fit)) cli::cli_inform("Repeated captured fit identities retain their original model positions and separate count-record provenance.")
  list(fits = fits, records = records, identity = identity)
}
