# ratetab.ado:217-268: one common outcome sample, with section-specific
# grouping exclusions. Original record positions remain the source identity.
.rat_abort <- function(message, subclass = "input") {
  cli::cli_abort(message, class = c(paste0("tabtools_error_ratetab_", subclass),
                                   "tabtools_error_ratetab"), call = NULL)
}

.rat_names <- function(x, data, arg, repeated = FALSE) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x)) ||
      (!repeated && anyDuplicated(x)) || any(!x %in% names(data))) {
    .rat_abort(paste0(arg, " must name existing columns", if (!repeated) " without repeats", "."))
  }
  x
}

.rat_group_missing <- function(x) is.na(x) | if (is.character(x)) x == "" else FALSE

.rat_group_attributes <- function(x, original) {
  attrs <- attributes(original)
  for (nm in setdiff(names(attrs), c("names", "dim", "dimnames"))) attr(x, nm) <- attrs[[nm]]
  x
}

.rat_factor_codes <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  lev <- levels(x)
  codes <- seq_along(lev)
  if (!is.null(labels)) {
    if (!is.numeric(labels) || is.null(names(labels)) || anyNA(labels) ||
        any(!is.finite(labels)) || anyDuplicated(names(labels)) || anyDuplicated(labels) ||
        any(!lev %in% names(labels))) {
      .rat_abort("Factor value labels must give one unique finite numeric code for every level.", "group")
    }
    codes <- unname(labels[match(lev, names(labels))])
  }
  list(values = codes[as.integer(x)], labels = stats::setNames(codes, lev))
}

.rat_group_values <- function(x, use, name) {
  if (is.list(x) || is.complex(x) || !is.null(dim(x)) ||
      !(is.numeric(x) || is.logical(x) || is.character(x) || is.factor(x))) {
    .rat_abort(paste0("Grouping column ", name, " must be numeric, logical, character, or factor."), "group")
  }
  code <- if (is.factor(x)) .rat_factor_codes(x)$values else if (is.logical(x)) as.integer(x) else unclass(x)
  if (is.numeric(code) && any(!is.finite(code[use]))) {
    .rat_abort(paste0("Grouping column ", name, " contains a non-finite observed value."), "group")
  }
  observed <- sort(unique(code[use]), na.last = NA, method = "radix")
  if (!length(observed)) .rat_abort(paste0("Grouping column ", name, " has no observed values in the common sample."), "sample")
  pos <- match(observed, code)
  values <- .rat_group_attributes(x[pos], x)
  raw_labels <- .st_category_text(values)
  labels <- trimws(raw_labels)
  if (anyNA(labels) || any(!nzchar(trimws(labels))) || anyDuplicated(trimws(labels))) {
    .rat_abort(paste0("Grouping column ", name, " has blank or duplicate category labels."), "group")
  }
  list(code = code, ids = match(code, observed), values = values,
       labels = labels, raw_labels = raw_labels, codes = observed)
}

.rat_sample <- function(data, by, events, exposure, cluster, subset, fweight) {
  if (!is.data.frame(data) || anyDuplicated(names(data))) {
    .rat_abort("data must be a data frame with unique column names.")
  }
  by <- .rat_names(by, data, "by", repeated = TRUE)
  events <- .rat_names(events, data, "events")
  exposure <- .rat_names(exposure, data, "exposure", repeated = TRUE)
  if (!length(exposure) %in% c(1L, length(events))) {
    .rat_abort("exposure must name one shared column or one column per outcome.")
  }
  exposure <- rep_len(exposure, length(events))
  n <- nrow(data)
  selected <- rep(TRUE, n)
  if (!is.null(subset)) {
    if (is.logical(subset) && length(subset) == n && !anyNA(subset)) {
      selected <- subset
    } else if (is.numeric(subset) && !is.complex(subset) && is.null(dim(subset)) &&
               all(is.finite(subset)) && all(subset == floor(subset)) &&
               all(subset >= 1 & subset <= n) && !anyDuplicated(subset)) {
      selected[] <- FALSE
      selected[subset] <- TRUE
    } else .rat_abort("subset must be a nonmissing logical row selector or unique valid row indices.", "sample")
  }
  w <- rep(1, n)
  if (!is.null(fweight)) {
    if (is.character(fweight) && length(fweight) == 1L) {
      .rat_names(fweight, data, "fweight")
      w <- data[[fweight]]
    } else w <- fweight
    if (!is.numeric(w) || is.complex(w) || !is.null(dim(w)) || length(w) != n ||
        any(!is.na(w) & (!is.finite(w) | w < 0 | w != floor(w)))) {
      .rat_abort("fweight must be a column or record vector of nonnegative integer replication weights.", "weight")
    }
  }
  numeric_names <- unique(c(events, exposure))
  for (nm in numeric_names) {
    v <- data[[nm]]
    if (!is.numeric(v) || is.complex(v) || !is.null(dim(v))) {
      .rat_abort(paste0("Event and exposure column ", nm, " must be numeric."))
    }
  }
  for (nm in unique(by)) {
    v <- data[[nm]]
    if (is.list(v) || is.complex(v) || !is.null(dim(v)) ||
        !(is.numeric(v) || is.logical(v) || is.character(v) || is.factor(v))) {
      .rat_abort(paste0("Grouping column ", nm, " must contain scalar numeric, logical, character or factor values."), "group")
    }
  }
  group_ok <- lapply(by, function(nm) !.rat_group_missing(data[[nm]]))
  outcomes_ok <- Reduce(`&`, lapply(numeric_names, function(nm) !is.na(data[[nm]])))
  observed <- selected & Reduce(`|`, group_ok) & outcomes_ok & !is.na(w)
  use <- observed & w > 0
  if (!any(use)) .rat_abort("No observations remain in the common outcome sample.", "sample")
  for (nm in numeric_names) {
    v <- data[[nm]][use]
    if (any(!is.finite(v) | v < 0) || (nm %in% events && any(v != floor(v)))) {
      .rat_abort(paste0("Column ", nm, " has invalid event counts or person-time in the common sample."), "domain")
    }
  }
  for (o in seq_along(events)) {
    if (any(data[[events[o]]][use] > 0 & data[[exposure[o]]][use] == 0)) {
      .rat_abort("Events without person-time are not supported.", "domain")
    }
  }
  if (!is.null(cluster)) {
    .rat_names(cluster, data, "cluster")
    if (length(cluster) != 1L) .rat_abort("cluster must name one column.", "cluster")
    cv <- data[[cluster]]
    if (is.list(cv) || is.complex(cv) || !is.null(dim(cv)) ||
        any(.rat_group_missing(cv)[use]) ||
        (is.numeric(cv) && any(!is.finite(cv[use])))) {
      .rat_abort("Cluster IDs must be finite nonmissing scalar values on the common sample, including zero-time rows.", "cluster")
    }
  }
  groups <- lapply(seq_along(by), function(g) {
    .rat_group_values(data[[by[g]]], use & group_ok[[g]], by[g])
  })
  if (!is.finite(sum(w[use]))) .rat_abort("The expanded sample count is not finitely representable.", "weight")
  wd <- .tt_sample_weights(w[use])
  sample <- .tt_sample_population("common", "ratetab", "table",
    weight_type = if (is.null(fweight)) "none" else "fweight",
    values = c(list(input_n = n, eligible_n = sum(selected), observed_n = sum(observed),
                    used_n = sum(use), missing_n = sum(selected & !observed),
                    zero_weight_n = sum(observed & w == 0), excluded_n = sum(!use),
                    reported_n = sum(w[use])), wd$values),
    bases = c(list(reported_n = "frequency-expanded common-sample records"), wd$bases),
    reasons = wd$reasons, statuses = wd$statuses,
    exclusions = rbind(.tt_sample_exclusion("input_to_eligible", "outside subset", sum(!selected), "original records"),
      .tt_sample_exclusion("eligible_to_used", "missing outcome/exposure/all grouping values or weight", sum(selected & !observed), "original records"),
      .tt_sample_exclusion("eligible_to_used", "zero frequency weight", sum(observed & w == 0), "original records")))
  sections <- lapply(seq_along(by), function(g) {
    ok <- use & group_ok[[g]]
    .tt_sample_population(paste0("group", g), "ratetab", "group", variable = by[g], spec = g,
      weight_type = if (is.null(fweight)) "none" else "fweight",
      values = list(input_n = sum(use), eligible_n = sum(use), observed_n = sum(ok),
                    used_n = sum(ok), missing_n = sum(use & !ok), excluded_n = sum(use & !ok),
                    reported_n = sum(w[ok])),
      bases = list(reported_n = "frequency-expanded section records"),
      exclusions = .tt_sample_exclusion("eligible_to_used", "missing this grouping value", sum(use & !ok), "common-sample original records"))
  })
  list(data = data, by = by, events = events, exposure = exposure, cluster = cluster,
       use = use, w = w, weight_type = if (is.null(fweight)) "none" else "fweight", group_ok = group_ok, groups = groups, row_id = seq_len(n),
       sample = .tt_sample_bind(c(list(sample), sections),
         prefixes = c("common", paste0("section", seq_along(by))),
         commands = rep("ratetab", length(by) + 1L)))
}
