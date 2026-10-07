# Native survtab.ado585-914 publication; stored projection969-1018.
.sv_time_text <- function(x) trimws(stata_fmt(x, "%12.0g"))

.sv_stripes <- function(labels) {
  out <- character(length(labels))
  for (i in seq_along(labels)) {
    name <- substr(.stata_strtoname(labels[i]), 1L, 32L)
    if (!nzchar(gsub("_", "", name, fixed = TRUE))) name <- paste0("group", i)
    base <- name; suffix <- 1L
    while (name %in% out[seq_len(i - 1L)]) {
      suffix <- suffix + 1L
      tail <- paste0("_", suffix)
      name <- paste0(substr(base, 1L, 32L - nchar(tail, type = "bytes")), tail)
    }
    out[i] <- name
  }
  out
}

.sv_publication <- function(prepared, curves, queries, medians, restricted, contrast, logrank,
  times, tau, median, riskset, reverse, difference, events, timeunit, level,
  addrow, numeric_format, pdp, highpdp, title, footnote, style, sheet) {
  G <- length(queries); grouped <- !is.null(prepared$by)
  K <- 1L + G + as.integer(difference) + as.integer(grouped)
  diff_col <- if (difference) G + 2L else NA_integer_
  p_col <- if (grouped) K else NA_integer_
  f <- function(x, extra = 0L) {
    spec <- numeric_format; spec$digits <- spec$digits + extra
    .tt_format_numeric(x, spec)
  }
  integer_text <- function(x) trimws(stata_fmt(x, "%11.0fc"))
  ci_text <- function(estimate, lower, upper, extra = 0L) {
    if (is.na(estimate)) return("")
    if (is.na(lower) || is.na(upper)) return(f(estimate, extra))
    paste0(f(estimate, extra), " (", f(lower, extra), ", ", f(upper, extra), ")")
  }
  unit <- c(years = "yr", months = "mo", days = "d", weeks = "wk")[[timeunit]]
  time_label <- function(t) paste0("  ", .sv_time_text(t), " ", if (t == 1) unit else timeunit)
  header <- c("", paste0(prepared$group_labels, " (N=", .sv_time_text(prepared$N), ")"),
    if (difference) paste0("Difference (", prepared$group_labels[1L], " - ", prepared$group_labels[2L], ")"),
    if (grouped) "p")
  assembled <- new.env(parent = emptyenv())
  assembled$body <- list(); assembled$rows <- list(); assembled$states <- list(); assembled$numbers <- list()
  add <- function(label, values = rep("", G), key, type = "stat", state = rep("", G),
                  numbers = rep(NA_real_, G), diff = "", block = 1L) {
    cells <- c(label, values, if (difference) diff, if (grouped) "")
    assembled$body[[length(assembled$body) + 1L]] <- cells
    assembled$rows[[length(assembled$rows) + 1L]] <- data.frame(type = type, key = key,
      block = block, p = NA_real_, stringsAsFactors = FALSE)
    assembled$states[[length(assembled$states) + 1L]] <- state
    assembled$numbers[[length(assembled$numbers) + 1L]] <- numbers
  }
  if (median) {
    estimates <- vapply(medians, `[[`, 0, "estimate")
    add(paste0("Median survival, ", unit), vapply(estimates, function(x) if (is.na(x)) "NR" else f(x), ""),
      "median", state = vapply(medians, `[[`, "", "state"), numbers = estimates,
      diff = if (difference && !anyNA(estimates)) f(estimates[1L] - estimates[2L]) else "")
    add(paste0("  (", .sv_time_text(level), "% CI)"), vapply(medians, function(m) {
      if (is.na(m$lower) || is.na(m$upper)) "" else paste0("(", f(m$lower), ", ", f(m$upper), ")")
    }, ""), "median:CI", state = ifelse(vapply(medians, function(m) !is.na(m$lower) && !is.na(m$upper), TRUE), "est", "notest"))
  }
  if (events) add("Events / N", paste0(integer_text(prepared$events), " / ", integer_text(prepared$N)),
                  "events", state = rep("est", G), numbers = prepared$events)
  add(if (reverse) "Cumulative incidence" else "Survival probability", key = "heading:probability", type = "cat_header")
  for (i in seq_along(times)) {
    probability <- vapply(queries, function(q) q$probability[i], 0)
    se <- vapply(queries, function(q) q$se[i], 0)
    difference_text <- ""
    if (difference) {
      value <- (probability[1L] - probability[2L]) * 100
      if (!anyNA(se)) {
        half <- stats::qnorm((1 + level / 100) / 2) * sqrt(sum(se^2)) * 100
        difference_text <- ci_text(value, value - half, value + half)
      } else difference_text <- f(value)
    }
    add(time_label(times[i]), paste0(f(probability * 100), "%"), paste0("km:", i),
      state = rep("est", G), numbers = probability, diff = difference_text)
  }
  if (!is.null(restricted)) {
    # Native string() labels omit display-format width padding.
    native_horizon <- trimws(if (tau == floor(tau)) stata_fmt(tau, "%3.0f") else stata_fmt(tau, "%5.1f"))
    add(paste0("RMST (", native_horizon, "-", unit, "), ", unit, " (", .sv_time_text(level), "% CI)"),
      vapply(restricted, function(m) ci_text(m$estimate, m$lower, m$upper, 1L), ""), "rmst",
      state = rep("est", G), numbers = vapply(restricted, `[[`, 0, "estimate"),
      diff = if (difference) ci_text(contrast$estimate, contrast$lower, contrast$upper, 1L) else "")
  }
  if (riskset) {
    add("Number at risk", key = "heading:risk", type = "cat_header")
    for (i in seq_along(times)) {
      risk <- vapply(queries, function(q) q$risk[i], 0)
      add(time_label(times[i]), integer_text(risk), paste0("risk:", i), state = rep("est", G), numbers = risk)
    }
  }
  logrank_row <- 0L
  if (grouped) {
    if (logrank$no_failure) {
      label <- "Log-rank test not possible: no failures in the analysis sample"
    } else {
      p <- format_p(logrank$p, pdp, highpdp)
      phrase <- if (startsWith(p, "<")) paste("p <", substring(p, 2L)) else paste("p =", p)
      label <- paste0("Log-rank test: chi2(", logrank$df, ") = ", trimws(stata_fmt(logrank$chi2, "%6.2f")), ", ", phrase)
    }
    add(label, key = "logrank", state = rep("notest", G))
    logrank_row <- length(assembled$body)
  }
  body <- assembled$body; row_metadata <- assembled$rows
  states <- assembled$states; numeric_cells <- assembled$numbers
  if (!is.null(addrow)) {
    if (!is.data.frame(addrow) || ncol(addrow) != K ||
        any(!vapply(addrow, is.character, TRUE)) || anyNA(addrow)) {
      .sv_abort("addrow must be a nonmissing character data frame with exactly the publication column count.")
    }
    for (i in seq_len(nrow(addrow))) {
      body[[length(body) + 1L]] <- unname(unlist(addrow[i, ], use.names = FALSE))
      row_metadata[[length(row_metadata) + 1L]] <- data.frame(type = "addrow", key = paste0("addrow:", i), block = 1L, p = NA_real_)
      states[[length(states) + 1L]] <- rep("", G)
      numeric_cells[[length(numeric_cells) + 1L]] <- rep(NA_real_, G)
    }
  }
  body <- do.call(rbind, body); rows <- do.call(rbind, row_metadata)
  if (grouped && !is.na(logrank$p)) {
    body[1L, p_col] <- format_p(logrank$p, pdp, highpdp)
    rows$p[c(1L, logrank_row)] <- logrank$p
  }
  probability <- do.call(cbind, lapply(queries, `[[`, "probability"))
  dimnames(probability) <- list(paste0("t", .sv_time_text(times)), .sv_stripes(prepared$group_labels))
  beyond <- do.call(cbind, lapply(queries, `[[`, "beyond_support"))
  dimnames(beyond) <- dimnames(probability)
  notes <- character()
  if (reverse) notes <- c(notes, paste0("reverse reports 1 - Kaplan-Meier, which equals cumulative incidence only with a single event type ",
    "(no competing risks). With competing events, use a competing-risks estimator (Aalen-Johansen)."))
  beyond_native <- character()
  for (g in seq_len(G)) if (any(beyond[, g])) beyond_native <- c(beyond_native,
    paste0(prepared$group_labels[g], ": ", paste(.sv_time_text(times[beyond[, g]]), collapse = " "),
           " (last follow-up ", .sv_time_text(prepared$support[g]), ")"))
  if (length(beyond_native)) notes <- c(notes,
    paste0("Times beyond the last observed follow-up repeat the final Kaplan-Meier estimate and are not supported by the data: ",
           paste(beyond_native, collapse = "; "), "."))
  if (grouped && logrank$no_failure) notes <- c(notes,
    "No failures in the analysis sample; the log-rank test is not possible and is omitted.")
  methods <- "Survival was estimated using the Kaplan-Meier product-limit method with Greenwood variance."
  if (prepared$declared_fweight) methods <- paste(methods, "Frequency counts use independent-subject replication semantics.")
  if (any(prepared$earliest_entry > 0)) methods <- paste(methods,
    "Delayed entry gives a conditional curve; integration from zero follows the native initialized-survival convention.")
  if (!is.null(restricted)) methods <- paste(methods,
    "RMST integrates the curve to the stated horizon; its variance sums squared remaining tail areas times Greenwood increments.")
  if (!is.null(contrast)) methods <- paste(methods, "The independent-group RMST contrast is group 1 minus group 2.")
  stored <- list(table = probability, N_rows = nrow(body) + 2L, ci_level = level,
    n_groups = G, by_var = prepared$by %||% "", methods = methods, logrank_p = logrank$p,
    logrank_chi2 = logrank$chi2, logrank_df = logrank$df, beyond_support = paste(beyond_native, collapse = "; "))
  for (g in seq_len(G)) {
    stored[[paste0("group_", g, "_value")]] <- .sv_group_text(prepared$group_values[g])
    stored[[paste0("group_", g, "_label")]] <- prepared$group_labels[g]
    if (median) stored[[paste0("median_", g)]] <- medians[[g]]$estimate
    if (events) {
      stored[[paste0("events_", g)]] <- prepared$events[g]
      stored[[paste0("atrisk_", g)]] <- prepared$N[g]
    }
    if (!is.null(restricted)) for (name in c("estimate", "se", "lower", "upper")) {
      prefix <- c(estimate = "rmst_", se = "rmst_se_", lower = "rmst_lb_", upper = "rmst_ub_")[[name]]
      stored[[paste0(prefix, g)]] <- restricted[[g]][[name]]
    }
  }
  if (!is.null(contrast)) for (name in c("estimate", "se", "lower", "upper", "p")) {
    prefix <- c(estimate = "rmst_diff", se = "rmst_diff_se", lower = "rmst_diff_lb", upper = "rmst_diff_ub", p = "rmst_diff_p")[[name]]
    stored[[prefix]] <- contrast[[name]]
  }
  cols <- data.frame(role = c("label", rep("group", G), if (difference) "value", if (grouped) "p"),
    model = NA_integer_, group = c(NA_integer_, seq_len(G), if (difference) NA_integer_, if (grouped) NA_integer_))
  # Native list keeps even an empty p column at its two-character minimum.
  cols$console_width <- ifelse(cols$role == "p", 2L, NA_integer_)
  frame <- list(source = "survtab", ci_level = level, group_values = prepared$group_values,
                group_labels = prepared$group_labels, contrast_direction = "group_1_minus_group_2")
  meta <- list(sheet = sheet, frame = frame, sample_accounting = prepared$sample,
    survtab_logrank_row = logrank_row,
    survival = list(raw = list(curves = curves, queries = queries, median = medians, rmst = restricted,
      rmst_contrast = contrast, logrank = logrank, probability = probability),
      publication = list(values = do.call(rbind, numeric_cells), states = do.call(rbind, states),
                         probability = probability, beyond_support = beyond),
      group_values = prepared$group_values, group_labels = prepared$group_labels,
      N = prepared$N, events = prepared$events, support = prepared$support,
      earliest_entry = prepared$earliest_entry, conditional_origin = prepared$earliest_entry > 0,
      records = prepared$records, original_row_ids = prepared$original_row_ids,
      original_id = prepared$original_id, id_values = prepared$id_values,
      exclusions = prepared$excluded, missing_event_rows = prepared$missing_event_rows,
      requested_times = times, variance_method = "native_Greenwood_independent_subjects",
      confidence = list(level = level, probability = "log_log_Greenwood", rmst = "normal_Greenwood_squared_tail",
                        median = "native_first_float_half_crossing; SVP_endpoint_capture", group_contrast = "independent_group_normal"),
      raw_role = "raw_analytical", reverse = reverse, weight_type = prepared$weight_type,
      notes = notes, native_query_se = "sts_gen_forward_fill_missing; sts.ado:406",
      endpoint_provenance = "root_actual_SVP001_005_and_SV001_011"))
  layout <- list(indent = 0L, align = "right", console_sepby = FALSE, console_title = TRUE,
    console_blank = TRUE, header_style = "plain", xlsx_rules = "survtab", sheet = "Survival",
    csv_reservedrow = TRUE, console_footnote = TRUE, md_keep_blank = TRUE)
  tt_table(body, list(header), rows = rows, cols = cols, title = title,
    footnote = .tt_append_footnotes(footnote, notes), style = style, stored = stored,
    command = "survtab", layout = layout, meta = meta)
}
