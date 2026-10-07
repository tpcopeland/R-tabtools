# Structured R records implement native scalar/pair/text statistics without a
# second string language. Compilation is separate from model extraction.
.rt_statspec_abort <- function(message) cli::cli_abort(message,
  class = "tabtools_error_statspec", call = NULL)

.rt_parse_rich_stats <- function(stats, statlabels, stat_values, exposurelabel, M) {
  builtin <- character()
  items <- list()
  if (is.null(stats) || is.character(stats)) {
    builtin <- stats
  } else {
    if (!is.list(stats)) .rt_statspec_abort("`stats` must be tokens or an ordered list of tokens and structured records.")
    seen <- character()
    nt <- 0L
    for (entry in stats) {
      if (is.character(entry)) {
        if (anyNA(entry)) .rt_statspec_abort("Statistic tokens cannot be missing.")
        builtin <- c(builtin, entry)
        next
      }
      allowed <- c("name", "pair", "label", "fmt", "mincell", "maskwith", "text")
      if (!is.list(entry) || is.null(names(entry)) || anyNA(names(entry)) ||
          anyDuplicated(names(entry)) || any(!names(entry) %in% allowed)) .rt_statspec_abort("Each statistic record must have unique fields name/pair/label/fmt/mincell/maskwith/text.")
      if ("text" %in% names(entry)) {
        if (!setequal(names(entry), c("text", "label")) || !is.character(entry$text) ||
            anyNA(entry$text) || length(entry$text) > M || !is.character(entry$label) ||
            length(entry$label) != 1L || is.na(entry$label) || !nzchar(entry$label)) .rt_statspec_abort("Text records require a nonempty label and at most one literal value per model.")
        nt <- nt + 1L
        items[[length(items) + 1L]] <- list(key = paste0("stat:text(", nt, ")"),
          item = paste0("text(", nt, ")"), label = entry$label, text = entry$text)
        next
      }
      for (field in c("name", "pair")) {
        val <- entry[[field]]
        if ((field == "name" || !is.null(val)) && (!is.character(val) || length(val) != 1L ||
            is.na(val) || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", val))) .rt_statspec_abort("Scalar names must be exact single names (case preserved).")
      }
      parts <- c(entry$name, entry$pair)
      if (anyDuplicated(parts)) .rt_statspec_abort("A pair must contain two different scalars.")
      id <- paste0("e(", paste(parts, collapse = "|"), ")")
      if (id %in% seen) .rt_statspec_abort("A scalar/pair statistic item cannot be repeated exactly.")
      seen <- c(seen, id)
      label <- entry$label %||% if (length(parts) == 1L) entry$name else paste0(parts[1L], " (", parts[2L], ")")
      if (!is.character(label) || length(label) != 1L || is.na(label)) .rt_statspec_abort("A statistic label must be one nonmissing string.")
      fmt <- entry$fmt
      if (!is.null(fmt)) {
        if (!is.character(fmt) || length(fmt) != 1L || is.na(fmt)) .rt_statspec_abort("A statistic format must be one numeric Stata format.")
        .parse_stata_fmt(fmt)
      }
      mincell <- entry$mincell %||% 0L
      if (!is.numeric(mincell) || length(mincell) != 1L || is.na(mincell) ||
          !is.finite(mincell) || mincell > .Machine$integer.max || mincell != floor(mincell) || (mincell != 0 && mincell < 2)) .rt_statspec_abort("`mincell` must be zero or an integer of at least two.")
      links <- entry$maskwith %||% character()
      if (!is.character(links) || anyNA(links) || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", links))) .rt_statspec_abort("`maskwith` must contain exact scalar names.")
      items[[length(items) + 1L]] <- list(key = paste0("stat:", id), item = id,
        label = label, parts = parts, fmt = fmt, mincell = as.integer(mincell), maskwith = unique(links))
    }
  }
  want <- .rt_parse_stats(builtin)
  if (!is.null(statlabels)) {
    if (!is.character(statlabels) || anyNA(statlabels) || is.null(names(statlabels)) ||
        anyNA(names(statlabels)) || anyDuplicated(names(statlabels)) ||
        any(!names(statlabels) %in% names(.rt_stat_registry))) .rt_statspec_abort("`statlabels` must be uniquely named labels of requested built-in statistics.")
    if (any(!vapply(names(statlabels), function(nm) isTRUE(want[[nm]]), TRUE))) .rt_statspec_abort("`statlabels` refers to an unrequested statistic.")
  }
  if (!is.null(exposurelabel)) {
    if (!is.character(exposurelabel) || length(exposurelabel) != 1L || is.na(exposurelabel) ||
        !isTRUE(want$exposure)) .rt_statspec_abort("`exposurelabel` requires requested exposure and one nonmissing string.")
    if ("exposure" %in% names(statlabels)) .rt_statspec_abort("Supply only one exposure label.")
    statlabels <- c(statlabels, exposure = exposurelabel)
  }
  if (is.null(stat_values)) stat_values <- rep(list(list()), M)
  if (!is.list(stat_values) || length(stat_values) != M) .rt_statspec_abort("`stat_values` must have one ordered named list per model.")
  for (values in stat_values) {
    if (!is.list(values) || (length(values) && (is.null(names(values)) || anyNA(names(values)) ||
        anyDuplicated(names(values)) || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", names(values)))))) .rt_statspec_abort("Model scalar values require unique exact names.")
    if (any(!vapply(values, function(x) is.numeric(x) && length(x) == 1L &&
        !is.nan(x) && (is.na(x) || is.finite(x)), TRUE))) .rt_statspec_abort("Every supplied scalar must be one finite number or NA.")
  }
  nodes <- unique(unlist(lapply(items, `[[`, "parts"), use.names = FALSE))
  threshold <- stats::setNames(rep(0L, length(nodes)), nodes)
  group <- stats::setNames(seq_along(nodes), nodes)
  linked <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  for (item in items) {
    if (is.null(item$parts)) next
    threshold[item$parts] <- pmax(threshold[item$parts], item$mincell)
    if (!length(item$maskwith)) next
    if (any(!item$maskwith %in% nodes)) .rt_statspec_abort("Every maskwith scalar must be requested as a scalar or pair part.")
    set <- unique(c(item$parts, item$maskwith))
    old <- unique(group[set])
    group[group %in% old] <- min(old)
    linked[group == min(old)] <- TRUE
  }
  for (g in unique(group[linked])) {
    if (!any(threshold[group == g] > 0L)) cli::cli_inform("No mincell threshold is given for a maskwith group; nothing in it is masked.")
  }
  list(want = want, items = items, values = stat_values, labels = statlabels,
       nodes = nodes, threshold = threshold, group = group, linked = linked)
}

.rt_count_stats <- function(st, records) {
  for (m in seq_along(st)) {
    st[[m]]$obs <- st[[m]]$N %||% NA_real_
    record <- records[[m]]
    if (is.null(record)) next
    for (nm in names(record$counts)) {
      st[[m]][[paste0("tt_", nm)]] <- record$counts[[nm]]
    }
    for (nm in c("obs", "events", "people", "exposure")) st[[m]][[nm]] <- record$counts[[nm]]
  }
  st
}

# Built-in rows may be unavailable in every model; explain their omission.
# Group diagnostics use only retained clogit terms, never a recount from data.
.rt_builtin_stat_notes <- function(want, builtin, st, fits) {
  if (is.null(want)) return(invisible(NULL))
  shown <- vapply(builtin, function(row) sub("^stat:", "", row$key), "")
  # Native identifies the QICu fallback under the requested aic token.
  if (isTRUE(want$aic) && "qic" %in% shown) shown <- c(shown, "aic")
  grouped <- integer()
  descriptions <- character()
  if (isTRUE(want$groups)) for (m in seq_along(fits)) {
    if (!inherits(fits[[m]], "clogit") || !is.na(st[[m]]$groups %||% NA_real_)) next
    terms <- .fc_field(fits[[m]], "terms")
    variables <- as.list(attr(terms, "variables"))[-1L]
    groups <- Filter(function(x) is.call(x) &&
      (identical(x[[1L]], as.name("strata")) || identical(x[[1L]], quote(survival::strata))), variables)
    if (!length(groups)) next
    grouped <- c(grouped, m)
    descriptions <- c(descriptions, paste0("model ", m, " (clogit, grouping term ",
      paste(vapply(groups, function(x) paste(deparse(x), collapse = " "), ""), collapse = " + "), ")"))
  }
  explained <- character()
  if (length(grouped)) {
    if (!"groups" %in% shown) {
      note <- paste0("Note: stats(groups) left out: no model stores a group count; ",
        paste(descriptions, collapse = "; "))
      cli::cli_inform("{note}")
      cli::cli_inform("Capture tt_fitcount() with the fit-time event and people columns, then request stats = 'people' with statlabels = c(people = 'Groups').")
      explained <- "groups"
    } else {
      cli::cli_inform(paste0("(regtab: stats(groups) is blank for model(s) ",
        paste(grouped, collapse = " "), ": the fit stores no group count; see tt_fitcount(..., people = <group column>).)"))
    }
  }
  native_ids <- c("n", "obs", "events", "people", "exposure", "groups", "mi_m",
    "aic", "qic", "bic", "ll", "icc", "r2", "r2_a", "rmse", "F", "fmi")
  requested <- native_ids[vapply(native_ids, function(nm) isTRUE(want[[nm]]), TRUE)]
  omitted <- setdiff(requested, c(shown, explained))
  if (length(omitted)) cli::cli_inform(paste0("(regtab: no model reports ", length(omitted),
    " requested statistic(s), left out of the table: ", paste(omitted, collapse = " "), ")"))
  invisible(NULL)
}

.rt_rich_stats <- function(o, st_all, fits) {
  spec <- o$stats_spec
  st_all <- .rt_count_stats(st_all, o$fitcounts %||% rep(list(NULL), length(fits)))
  builtin <- if (!is.null(o$stats)) .rt_stats_rows(o$stats, st_all) else list(rows = list(), stored = list())
  rows <- lapply(builtin$rows, function(row) {
    name <- sub("^stat:", "", row$key)
    if (name %in% names(spec$labels)) row$label <- spec$labels[[name]]
    row$type <- "stat"
    row$item <- name
    row
  })
  M <- length(fits)
  values <- matrix(NA_real_, length(spec$nodes), M, dimnames = list(spec$nodes, NULL))
  origins <- matrix("unavailable", length(spec$nodes), M)
  for (i in seq_along(spec$nodes)) for (m in seq_len(M)) {
    name <- spec$nodes[i]
    supplied <- spec$values[[m]][[name]]
    alias <- if (name %in% c("tt_obs", "tt_events", "tt_people", "tt_people_ev", "tt_exposure")) st_all[[m]][[name]] else NULL
    if (!is.null(supplied) && !is.null(alias) && !identical(as.numeric(supplied), as.numeric(alias))) .rt_statspec_abort("Supplied values conflict with an authoritative fit-count scalar.")
    val <- supplied %||% alias %||% st_all[[m]][[name]]
    if (is.null(val) && !.rt_failed(fits[[m]])) {
      field <- .fc_field(fits[[m]], name)
      if (is.numeric(field) && length(field) == 1L) val <- field
    }
    if (!is.null(val) && (!is.numeric(val) || length(val) != 1L || is.nan(val) || (!is.na(val) && !is.finite(val)))) .rt_statspec_abort("Requested model scalar is not finite numeric or NA.")
    if (!is.null(val)) {
      values[i, m] <- val
      origins[i, m] <- if (!is.null(supplied)) "explicit_stat_values" else if (!is.null(alias)) "fitcount_unweighted" else "model_scalar"
    }
  }
  if (nrow(values) && any(rowSums(!is.na(values)) == 0L)) .rt_statspec_abort("A requested scalar is unavailable in every model.")
  states <- matrix("blank", nrow(values), M)
  states[!is.na(values)] <- "available"
  for (i in seq_len(nrow(values))) {
    primary <- !is.na(values[i, ]) & values[i, ] >= 1 & values[i, ] < spec$threshold[i]
    states[i, primary] <- "masked"
  }
  for (g in unique(spec$group[spec$linked])) {
    indices <- which(spec$group == g)
    for (m in seq_len(M)) if (any(states[indices, m] == "masked")) {
      indices_linked <- indices[states[indices, m] == "available"]
      states[indices_linked, m] <- "linked"
    }
  }
  provenance <- list()
  n_masked <- n_linked <- 0L
  text_index <- 0L
  for (item in spec$items) {
    if (!is.null(item$text)) {
      text_index <- text_index + 1L
      rows[[length(rows) + 1L]] <- list(key = item$key, item = item$item,
        label = item$label, type = "stat", values = c(item$text, rep("", M))[seq_len(M)],
        raw_values = list(), part_names = character(), part_states = list())
      next
    }
    raw <- state_parts <- text_parts <- list()
    for (p in seq_along(item$parts)) {
      name <- item$parts[p]
      i <- match(name, spec$nodes)
      raw[[p]] <- unname(values[i, ])
      state_parts[[p]] <- states[i, ]
      fmt <- item$fmt %||% if (all(is.na(values[i, ]) | values[i, ] == floor(values[i, ]))) "%12.0fc" else "%12.3f"
      txt <- rep("", M)
      available <- states[i, ] == "available"
      txt[available] <- trimws(stata_fmt(values[i, available], fmt))
      txt[states[i, ] == "masked"] <- paste0("<", spec$threshold[i])
      txt[states[i, ] == "linked"] <- "\u2013"
      text_parts[[p]] <- txt
      n_masked <- n_masked + sum(states[i, ] == "masked")
      n_linked <- n_linked + sum(states[i, ] == "linked")
      provenance[[length(provenance) + 1L]] <- data.frame(item_key = item$key,
        part_index = as.integer(p), part_name = name, model_index = seq_len(M),
        raw_value = unname(values[i, ]), state = states[i, ], threshold = unname(spec$threshold[i]),
        group = as.integer(spec$group[i]), source = origins[i, ], stringsAsFactors = FALSE)
    }
    txt <- text_parts[[1L]]
    if (length(text_parts) == 2L) {
      both <- nzchar(txt) & nzchar(text_parts[[2L]])
      txt[both] <- paste0(txt[both], " (", text_parts[[2L]][both], ")")
    }
    rows[[length(rows) + 1L]] <- list(key = item$key, item = item$item,
      label = item$label, type = "stat", values = unname(txt), raw_values = raw,
      part_names = item$parts, part_states = state_parts)
  }
  stored <- builtin$stored
  if (any(spec$threshold > 0L)) stored$N_stats_masked <- as.integer(n_masked)
  if (any(spec$linked)) stored$N_stats_linked <- as.integer(n_linked)
  if (length(spec$items)) {
    for (i in seq_along(spec$nodes)) for (m in seq_len(M)) {
      if (!is.na(values[i, m])) stored[[paste0("e_", spec$nodes[i], "_", m)]] <- unname(values[i, m])
    }
  }
  .rt_builtin_stat_notes(o$stats, builtin$rows, st_all, fits)
  list(rows = rows, stored = stored, provenance = if (length(provenance)) do.call(rbind, provenance) else data.frame())
}
