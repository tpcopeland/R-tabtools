# Native levelsof defaults to 16 significant digits and omits a leading zero.
# Keep numerical identities separate from this display/return projection.
.sv_group_text <- function(values) {
  if (!is.numeric(values)) return(as.character(values))
  sub("^(-?)0[.]", "\\1.", sprintf("%.16g", values))
}

# Native survtab.ado:123-153,253-362: frequency replication and fixed groups.
.sv_abort <- function(message, class = "tabtools_error_survival_input") {
  cli::cli_abort(message, class = class, call = NULL)
}

.sv_column <- function(data, name, option) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name) ||
      sum(names(data) == name) != 1L) .sv_abort(paste0(option, " must name one unambiguous column."))
  data[[name]]
}

.sv_numeric <- function(x, name) {
  if (!is.numeric(x) || is.complex(x) || !is.null(dim(x)) || inherits(x, c("Date", "POSIXt")) ||
      any(is.infinite(x))) .sv_abort(paste0(name, " must contain finite numeric values or missing values."))
  as.numeric(x)
}

.sv_subjects <- function(records) {
  for (subject in unique(records$subject)) {
    rows <- which(records$subject == subject)
    r <- records[rows[order(records$entry[rows], records$exit[rows])], , drop = FALSE]
    if (length(unique(r$group)) != 1L || length(unique(r$frequency)) != 1L) {
      .sv_abort("A subject must retain one group and one frequency throughout follow-up.",
                "tabtools_error_survival_inference")
    }
    if (nrow(r) > 1L && any(r$entry[-1L] < r$exit[-nrow(r)])) {
      .sv_abort("Overlapping subject intervals are outside the supported survival inference.",
                "tabtools_error_survival_inference")
    }
    failure <- which(r$event == 1L)
    if (length(failure) > 1L || length(failure) && failure != nrow(r)) {
      .sv_abort("Only one terminal failure is supported; records after failure and recurrent failures refuse.",
                "tabtools_error_survival_inference")
    }
  }
  invisible(records)
}

.sv_prepare <- function(data, time, event, by, entry, id, fweight) {
  if (!is.data.frame(data) || !nrow(data)) .sv_abort("data must be a nonempty data frame.")
  exit <- .sv_numeric(.sv_column(data, time, "time"), "time")
  ev <- .sv_column(data, event, "event")
  if (!is.numeric(ev) && !is.logical(ev) || is.complex(ev) || !is.null(dim(ev)) ||
      any(!is.na(ev) & !ev %in% c(0, 1))) .sv_abort("event must be binary numeric/logical (0/1), or missing.")
  ev <- as.integer(ev)
  start <- if (is.null(entry)) rep(0, nrow(data)) else if (is.character(entry)) {
    .sv_numeric(.sv_column(data, entry, "entry"), "entry")
  } else {
    if (!is.numeric(entry) || length(entry) != 1L || !is.finite(entry) || entry < 0) {
      .sv_abort("entry must be NULL, one nonnegative number, or one column name.")
    }
    rep(entry, nrow(data))
  }
  if (any(!is.na(start) & start < 0)) .sv_abort("entry cannot be negative.")
  frequency <- if (is.null(fweight)) rep(1, nrow(data)) else {
    .sv_numeric(.sv_column(data, fweight, "fweight"), "fweight")
  }
  if (any(!is.na(frequency) & (frequency < 0 | frequency != floor(frequency)))) {
    .sv_abort("fweight must contain nonnegative integer replication counts; other weights are unsupported.",
              "tabtools_error_survival_inference")
  }
  group_source <- if (is.null(by)) rep(1, nrow(data)) else .sv_column(data, by, "by")
  if (is.factor(group_source)) group_source <- as.character(group_source)
  if ((!is.numeric(group_source) && !is.character(group_source) && !is.logical(group_source)) ||
      !is.null(dim(group_source)) || is.complex(group_source) ||
      is.numeric(group_source) && any(is.infinite(group_source))) .sv_abort("by must contain numeric codes or literal strings.")
  ids <- if (is.null(id)) seq_len(nrow(data)) else .sv_column(data, id, "id")
  if (is.factor(ids)) ids <- as.character(ids)
  if ((!is.numeric(ids) && !is.character(ids)) || !is.null(dim(ids)) || is.complex(ids) ||
      is.numeric(ids) && any(is.infinite(ids))) .sv_abort("id must contain finite numbers or literal strings.")
  reasons <- rep("", nrow(data))
  mark <- function(condition, reason) {
    hit <- which(condition & !nzchar(reasons))
    reasons[hit] <- reason
    reasons
  }
  reasons <- mark(is.na(exit) | is.na(start), "missing_time")
  reasons <- mark(!is.na(exit) & !is.na(start) & exit <= start, "nonpositive_interval")
  reasons <- mark(is.na(frequency), "missing_frequency")
  reasons <- mark(!is.na(frequency) & frequency == 0, "zero_frequency")
  reasons <- mark(is.na(ids) | if (is.character(ids)) ids == "" else FALSE, "missing_id")
  reasons <- mark(is.na(group_source) | if (is.character(group_source)) group_source == "" else FALSE, "missing_group")
  keep <- !nzchar(reasons)
  if (!any(keep)) .sv_abort("No eligible survival observations remain.", "tabtools_error_survival_empty")
  primitive_groups <- if (is.integer(group_source)) as.integer(group_source) else if (is.numeric(group_source)) as.numeric(group_source) else group_source
  values <- sort(unique(primitive_groups[keep]), method = "radix")
  if (!is.null(by) && length(values) < 2L) .sv_abort("by requires at least two included groups.")
  labs <- attr(group_source, "labels", exact = TRUE)
  labels <- .sv_group_text(values)
  if (is.null(by)) labels <- "Overall"
  if (is.numeric(labs) && !is.null(names(labs))) {
    hit <- match(values, unname(labs))
    labels[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
  }
  records <- data.frame(record_index = which(keep), subject = match(ids[keep], unique(ids[keep])),
    group = match(group_source[keep], values), entry = start[keep], exit = exit[keep],
    event = ifelse(is.na(ev[keep]), 0L, ev[keep]), frequency = frequency[keep])
  .sv_subjects(records)
  N <- vapply(seq_along(values), function(g) {
    r <- records[records$group == g, , drop = FALSE]
    sum(r$frequency[!duplicated(r$subject)])
  }, 0)
  if (any(!is.finite(N) | N <= 0) || any(N > 2^53)) .sv_abort("Subject frequency totals must be positive and exactly representable.")
  events <- vapply(seq_along(values), function(g) sum(records$frequency[records$group == g & records$event == 1L]), 0)
  support <- vapply(seq_along(values), function(g) max(records$exit[records$group == g]), 0)
  earliest <- vapply(seq_along(values), function(g) min(records$entry[records$group == g]), 0)
  excluded <- data.frame(record_index = which(!keep), reason = reasons[!keep], stringsAsFactors = FALSE)
  exclusion_rows <- lapply(unique(excluded$reason), function(reason) .tt_sample_exclusion(
    "input_to_eligible", reason, sum(excluded$reason == reason), "original record occurrences"))
  exclusion_table <- if (length(exclusion_rows)) do.call(rbind, exclusion_rows) else NULL
  weight_type <- if (is.null(fweight)) "none" else "frequency"
  sample <- .tt_sample_population("survtab:included", "survtab", "table",
    weight_type = weight_type, values = list(input_n = as.numeric(nrow(data)),
      eligible_n = as.numeric(nrow(records)), observed_n = as.numeric(sum(!is.na(ev[keep]))),
      used_n = as.numeric(nrow(records)), missing_n = as.numeric(sum(is.na(ev[keep]))),
      excluded_n = as.numeric(sum(!keep)), reported_n = sum(N)),
    bases = list(input_n = "original record occurrences", eligible_n = "eligible interval occurrences",
      observed_n = "included intervals with nonmissing event", used_n = "included intervals including missing-event censoring",
      missing_n = "missing event among included intervals", excluded_n = "excluded original record occurrences",
      reported_n = "unique included subjects with frequency replication"), exclusions = exclusion_table)
  list(records = records, group_values = values, group_labels = labels, N = N, events = events,
       support = support, earliest_entry = earliest, excluded = excluded, sample = sample,
       id_values = unique(ids[keep]), original_id = ids, original_row_ids = seq_len(nrow(data)),
       missing_event_rows = which(keep & is.na(ev)), weight_type = weight_type,
       declared_fweight = !is.null(fweight), by = by)
}
