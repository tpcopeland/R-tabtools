# Source sample accounting ----

.tt_sample_metrics <- c("input_n", "eligible_n", "observed_n", "used_n",
                        "fitted_n", "frame_n", "missing_n", "zero_weight_n",
                        "excluded_n", "weight_sum", "effective_n", "reported_n")
.tt_sample_count_metrics <- setdiff(.tt_sample_metrics,
                                    c("weight_sum", "effective_n", "reported_n"))

.tt_sample_population <- function(id, command, scope, component = "",
                                  variable = NA_character_, group = NA_character_,
                                  model = NA_real_, imputation = NA_real_, spec = NA_real_,
                                  weight_type = "none", values = list(), bases = list(),
                                  reasons = list(), statuses = list(), exclusions = NULL) {
  supplied <- lapply(list(values, bases, reasons, statuses), as.list)
  for (v in supplied) {
    if (length(v) && (is.null(names(v)) || anyNA(names(v)) ||
                      any(!nzchar(names(v))) || anyDuplicated(names(v)) ||
                      any(!names(v) %in% .tt_sample_metrics))) {
      cli::cli_abort("Sample measures must have unique standard metric names.",
                     class = "tabtools_error_sample_accounting", call = NULL)
    }
  }
  values <- supplied[[1L]]
  bases <- supplied[[2L]]
  reasons <- supplied[[3L]]
  statuses <- supplied[[4L]]
  status <- vapply(.tt_sample_metrics, function(metric) {
    statuses[[metric]] %||% if (!metric %in% names(values)) "not_applicable" else
      if (length(values[[metric]]) == 1L && is.na(values[[metric]])) "unavailable" else "available"
  }, "")
  val <- vapply(.tt_sample_metrics, function(metric) {
    v <- values[[metric]]
    if (is.null(v)) return(NA_real_)
    if (!is.numeric(v) || is.complex(v) || !is.null(dim(v)) || length(v) != 1L) {
      cli::cli_abort("Sample measure {.val {metric}} must be a numeric scalar.",
                     class = "tabtools_error_sample_accounting", call = NULL)
    }
    as.numeric(v)
  }, 0)
  basis <- vapply(.tt_sample_metrics, function(metric) {
    bases[[metric]] %||% if (status[[metric]] == "available") "supplied source count" else
      if (status[[metric]] == "not_applicable") "not applicable" else "not recoverable from stored source"
  }, "")
  reason <- vapply(.tt_sample_metrics, function(metric) {
    reasons[[metric]] %||% if (status[[metric]] == "available") "" else
      if (status[[metric]] == "not_applicable") "not applicable to this population" else
        "not recoverable from stored source"
  }, "")
  populations <- data.frame(id = id, component = component, command = command,
                            scope = scope, variable = variable, group = group,
                            model = model, imputation = imputation, spec = spec,
                            weight_type = weight_type, stringsAsFactors = FALSE)
  measures <- data.frame(population_id = id, metric = .tt_sample_metrics,
                         value = unname(val), status = unname(status),
                         basis = unname(basis), reason = unname(reason),
                         stringsAsFactors = FALSE)
  if (is.null(exclusions)) {
    exclusions <- data.frame(stage = character(), reason = character(), n = numeric(),
                              status = character(), basis = character(),
                              stringsAsFactors = FALSE)
  }
  exclusions$population_id <- rep(id, nrow(exclusions))
  exclusions <- exclusions[c("population_id", "stage", "reason", "n", "status", "basis")]
  out <- list(version = 1L, populations = populations, measures = measures, exclusions = exclusions)
  .tt_validate_sample_accounting(out)
  out
}

.tt_sample_exclusion <- function(stage, reason, n, basis) {
  data.frame(stage = stage, reason = reason, n = as.numeric(n),
             status = ifelse(is.na(n), "unavailable", "available"), basis = basis,
             stringsAsFactors = FALSE)
}

.tt_sample_weights <- function(w) {
  if (!is.numeric(w) || is.complex(w) || !is.null(dim(w)) ||
      any(!is.finite(w) | w < 0)) {
    cli::cli_abort("Sample weight diagnostics need finite nonnegative record weights.",
                   class = "tabtools_error_sample_accounting", call = NULL)
  }
  total <- sum(w)
  positive <- w[w > 0]
  ess <- if (length(positive)) {
    scaled <- positive / max(positive)
    sum(scaled)^2 / sum(scaled^2)
  } else NA_real_
  list(values = list(weight_sum = if (is.finite(total)) total else NA_real_, effective_n = ess),
       bases = list(weight_sum = "original-scale used record weights",
                    effective_n = "Kish record-weight diagnostic"),
       reasons = list(weight_sum = if (is.finite(total)) "" else "weight total is not finitely representable",
                      effective_n = if (is.finite(ess)) "" else "no positive record weights"),
       statuses = list(weight_sum = if (is.finite(total)) "available" else "unavailable",
                       effective_n = if (is.finite(ess)) "available" else "unavailable"))
}

.tt_sample_bind <- function(samples, prefixes = NULL, commands = NULL) {
  if (!is.list(samples) || !length(samples)) {
    cli::cli_abort("Sample binding needs at least one source.",
                   class = "tabtools_error_sample_accounting", call = NULL)
  }
  if (is.null(prefixes)) prefixes <- paste0("source", seq_along(samples))
  if (is.null(commands)) commands <- rep("unknown", length(samples))
  if (!is.character(prefixes) || length(prefixes) != length(samples) || anyNA(prefixes) ||
      any(!nzchar(prefixes)) || anyDuplicated(prefixes) ||
      !is.character(commands) || length(commands) != length(samples) || anyNA(commands)) {
    cli::cli_abort("Sample source prefixes must be unique nonempty strings, one per source.",
                   class = "tabtools_error_sample_accounting", call = NULL)
  }
  parts <- lapply(seq_along(samples), function(i) {
    sample <- samples[[i]]
    if (is.null(sample)) {
      unknown <- stats::setNames(rep(list(NA_real_), length(.tt_sample_metrics)), .tt_sample_metrics)
      sample <- .tt_sample_population("unknown", commands[[i]], "summary", weight_type = "unknown",
                                      values = unknown,
                                      reasons = stats::setNames(rep(list("source has no sample ledger"),
                                                                    length(.tt_sample_metrics)), .tt_sample_metrics))
    }
    .tt_validate_sample_accounting(sample)
    prefix <- prefixes[[i]]
    sample$populations$id <- paste0(prefix, "/", sample$populations$id)
    sample$populations$component <- ifelse(nzchar(sample$populations$component),
                                           paste0(prefix, "/", sample$populations$component), prefix)
    sample$measures$population_id <- paste0(prefix, "/", sample$measures$population_id)
    if (nrow(sample$exclusions)) {
      sample$exclusions$population_id <- paste0(prefix, "/", sample$exclusions$population_id)
    }
    sample
  })
  out <- list(version = 1L,
              populations = do.call(rbind, lapply(parts, `[[`, "populations")),
              measures = do.call(rbind, lapply(parts, `[[`, "measures")),
              exclusions = do.call(rbind, lapply(parts, `[[`, "exclusions")))
  for (field in c("populations", "measures", "exclusions")) rownames(out[[field]]) <- NULL
  .tt_validate_sample_accounting(out)
  out
}

.tt_validate_sample_accounting <- function(x) {
  bad <- function(message) cli::cli_abort(message, class = "tabtools_error_sample_accounting", call = NULL)
  if (!is.list(x) || !identical(names(x), c("version", "populations", "measures", "exclusions")) ||
      !identical(x[["version", exact = TRUE]], 1L)) bad("Sample accounting needs the documented schema with version 1L.")
  columns <- list(populations = c("id", "component", "command", "scope", "variable", "group",
                                  "model", "imputation", "spec", "weight_type"),
                  measures = c("population_id", "metric", "value", "status", "basis", "reason"),
                  exclusions = c("population_id", "stage", "reason", "n", "status", "basis"))
  for (field in names(columns)) {
    if (!is.data.frame(x[[field]]) || !identical(names(x[[field]]), columns[[field]])) {
      bad(paste0("Sample ", field, " must have the documented columns."))
    }
  }
  p <- x$populations
  m <- x$measures
  e <- x$exclusions
  if (!nrow(p)) bad("Sample accounting must describe at least one population.")
  for (field in setdiff(columns$populations, c("model", "imputation", "spec"))) {
    v <- p[[field]]
    if (!is.character(v) || !is.null(dim(v)) ||
        (field %in% c("id", "component", "command", "scope", "weight_type") && anyNA(v))) {
      bad(paste0("Sample population ", field, " must be character with valid context."))
    }
  }
  if (any(!nzchar(p$id)) || anyDuplicated(p$id) || any(!nzchar(p$command)) || any(!nzchar(p$weight_type))) {
    bad("Sample population IDs must be unique and nonempty; command and weight_type must be nonempty.")
  }
  if (any(!p$scope %in% c("table", "group", "variable", "model", "imputation", "evaluation", "summary"))) {
    bad("Unknown sample population scope.")
  }
  for (field in c("model", "imputation", "spec")) {
    v <- p[[field]]
    if (!is.numeric(v) || is.complex(v) || !is.null(dim(v)) || any(is.nan(v)) ||
        any(!is.na(v) & (!is.finite(v) | v < 1 | v != floor(v)))) {
      bad(paste0("Sample population ", field, " must be positive whole indices or NA."))
    }
  }
  for (field in setdiff(columns$measures, "value")) {
    if (!is.character(m[[field]]) || !is.null(dim(m[[field]])) || anyNA(m[[field]])) {
      bad("Sample measure text fields must be nonmissing character vectors.")
    }
  }
  if (!is.numeric(m$value) || is.complex(m$value) || !is.null(dim(m$value)) || any(is.nan(m$value))) {
    bad("Sample measure values must be numeric vectors, with NA for unknown values.")
  }
  if (any(!m$population_id %in% p$id) || any(!m$metric %in% .tt_sample_metrics) ||
      nrow(m) != nrow(p) * length(.tt_sample_metrics) ||
      anyDuplicated(m[c("population_id", "metric")])) bad("Every sample population needs each standard measure exactly once.")
  if (any(!m$status %in% c("available", "unavailable", "not_applicable")) || any(!nzchar(m$basis))) {
    bad("Sample measures need a valid availability status and an evidence basis.")
  }
  known <- m$status == "available"
  if (any(is.na(m$value[known])) || any(!is.finite(m$value[known]) | m$value[known] < 0) ||
      any(!is.na(m$value[!known])) || any(!nzchar(m$reason[!known]))) {
    bad("Available sample values must be finite and nonnegative; unknown values need NA and a reason.")
  }
  count <- known & m$metric %in% .tt_sample_count_metrics
  if (any(m$value[count] != floor(m$value[count]))) bad("Sample record counts must be whole numbers.")
  values <- matrix(NA_real_, nrow = nrow(p), ncol = length(.tt_sample_metrics),
                    dimnames = list(p$id, .tt_sample_metrics))
  values[cbind(match(m$population_id, p$id), match(m$metric, .tt_sample_metrics))] <- m$value
  pairs <- list(c("input_n", "eligible_n"), c("input_n", "used_n"),
                 c("input_n", "observed_n"), c("input_n", "fitted_n"),
                 c("input_n", "missing_n"), c("input_n", "excluded_n"),
                 c("eligible_n", "used_n"), c("eligible_n", "observed_n"),
                 c("input_n", "zero_weight_n"), c("eligible_n", "fitted_n"))
  for (pair in pairs) {
    v <- values[, pair, drop = FALSE]
    if (any(v[, 2L] > v[, 1L], na.rm = TRUE)) bad("Scoped sample counts contradict their eligibility stages.")
  }
  v <- values[, c("input_n", "used_n", "excluded_n"), drop = FALSE]
  if (any(v[, 3L] != v[, 1L] - v[, 2L], na.rm = TRUE)) bad("Sample excluded_n must reconcile input_n and used_n.")
  for (field in setdiff(columns$exclusions, "n")) {
    if (!is.character(e[[field]]) || !is.null(dim(e[[field]])) || anyNA(e[[field]])) {
      bad("Sample exclusion text fields must be nonmissing character vectors.")
    }
  }
  if (!is.numeric(e$n) || is.complex(e$n) || !is.null(dim(e$n)) || any(is.nan(e$n)) || any(!e$population_id %in% p$id) ||
      any(!e$stage %in% c("input_to_eligible", "eligible_to_used", "retained")) ||
      any(!e$status %in% c("available", "unavailable")) || any(!nzchar(e$reason)) || any(!nzchar(e$basis))) {
    bad("Sample exclusion counts need valid stages, reasons and evidence.")
  }
  known <- e$status == "available"
  if (any(is.na(e$n[known])) || any(!is.finite(e$n[known]) | e$n[known] < 0 | e$n[known] != floor(e$n[known])) ||
      any(!is.na(e$n[!known]))) bad("Sample exclusions must be whole record counts or explicitly unavailable.")
  invisible(x)
}
