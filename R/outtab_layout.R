.ot_layout <- function(samples, fits, modellabels, grouplabels, ratiolabel,
                       numeric, mask, mintext, nonconvtext, failtext, droptext,
                       title, footnote, font, fontsize, borderstyle, headershade,
                       zebra, headercolor, zebracolor, sheet, minevents) {
  if (!exists(".tc_render", mode = "function")) .ot_abort("outtab requires the merged tabcell formatter capability.", "capability")
  K <- length(modellabels); B <- length(samples$blocks)
  body <- list(); raw <- matrix(NA_real_, B, 4L + 4L * K)
  colnames(raw) <- c("n1", "e1", "n0", "e0",
    unlist(lapply(seq_len(K), function(k) paste0(c("b", "lb", "ub", "rc"), k))))
  display_rows <- integer(B); panel_rows <- integer(); diagnostics <- list()
  states <- raw_states <- matrix("", B, K)
  reasons <- matrix("", B, K); publication <- vector("list", B)
  count_publication <- matrix(NA_real_, B, 4L,
    dimnames = list(NULL, c("n1", "e1", "n0", "e0")))
  count_states <- matrix("notest", B, 2L)
  count_mask_reason <- matrix("", B, 2L)
  ratio_mask_reason <- matrix("", B, K)
  n_masked <- n_linked <- 0L
  count_format <- .tt_resolve_numeric_format(cformat = "%12.0fc", sep = " ")
  percent_format <- .tt_resolve_numeric_format(cformat = "%4.1f", sep = " ")
  populations <- list(.tt_sample_population("base", "outtab", "table",
    values = list(input_n = as.numeric(nrow(samples$data)), eligible_n = as.numeric(length(samples$base_ids))),
    bases = list(input_n = "explicit original rows", eligible_n = "selected rows with observed exposure")))
  prior_panel <- NA_integer_
  for (i in seq_len(B)) {
    block <- samples$blocks[[i]]
    if (!is.null(samples$panels) && !identical(block$panel_index, prior_panel)) {
      panel_rows <- c(panel_rows, length(body) + 1L)
      body[[length(body) + 1L]] <- c(.ot_label(samples$original, block$panel), rep("", K + 2L))
      prior_panel <- block$panel_index
    }
    counts <- block$counts; raw[i, 1:4] <- counts
    e <- unname(counts[c("e1", "e0")]); n <- unname(counts[c("n1", "n0")])
    rendered <- .tc_render("enp", list(e = e, n = n), numeric, count_format,
      percent_format, mincell = 0L)
    e_mask <- e >= 1 & e < mask$threshold
    n_mask <- n >= 1 & n < mask$threshold
    protected <- e_mask | n_mask
    count_text <- rendered$text
    count_text[n_mask] <- mask$text
    count_text[e_mask & !n_mask] <- paste0(mask$text, "/",
      .tt_format_numeric(n[e_mask & !n_mask], count_format))
    count_states[i, protected] <- "masked"
    count_mask_reason[i, e_mask & !n_mask] <- "primary_event_count"
    count_mask_reason[i, n_mask] <- "primary_total_count"
    count_publication[i, ] <- counts
    count_publication[i, c(2L, 4L)[protected]] <- NA_real_
    count_publication[i, c(1L, 3L)[n_mask]] <- NA_real_
    n_masked <- n_masked + as.integer(sum(protected))
    ratio_text <- character(K)
    numeric_publication <- matrix(NA_real_, K, 3L, dimnames = list(NULL, c("b", "lb", "ub")))
    eligible_id <- paste0("eligible:", i)
    populations[[length(populations) + 1L]] <- .tt_sample_population(eligible_id, "outtab", "variable",
      variable = block$outcome, component = paste0("panel:", block$panel_index),
      values = list(input_n = as.numeric(length(samples$base_ids)), eligible_n = as.numeric(length(block$ids))),
      bases = list(input_n = "base selected original row IDs", eligible_n = "outcome observed, panel/own observation indicator exactly 1"))
    for (k in seq_len(K)) {
      fit <- fits[[i]][[k]]
      raw[i, 4L + (k - 1L) * 4L + seq_len(4L)] <- c(fit$b, fit$lb, fit$ub, fit$native_rc)
      raw_states[i, k] <- states[i, k] <- fit$status
      reasons[i, k] <- fit$reason
      ratio_text[k] <- if (fit$status == "est") {
        .tc_render("est", list(b = fit$b, ll = fit$lb, ul = fit$ub), numeric,
          count_format, percent_format)$text
      } else if (identical(fit$native_rc, -1)) mintext else if (identical(fit$native_rc, 430)) nonconvtext else
        if (identical(fit$native_rc, -2)) droptext else if (identical(fit$native_rc, 459)) "not estimable" else gsub("#",
          if (is.na(fit$native_rc)) "R" else as.character(fit$native_rc), failtext, fixed = TRUE)
      if (fit$status == "est") numeric_publication[k, ] <- c(fit$b, fit$lb, fit$ub)
      if (any(protected)) {
        ratio_text[k] <- ""; states[i, k] <- "masked"
        ratio_mask_reason[i, k] <- "linked_primary_count"
        numeric_publication[k, ] <- NA_real_; n_linked <- n_linked + 1L
      }
      diagnostics[[length(diagnostics) + 1L]] <- data.frame(row = i, model = k,
        N = as.numeric(fit$N), N_cc = as.numeric(fit$N_cc), N_clust = as.numeric(fit$N_clust), df_m = as.numeric(fit$df_m),
        converged = fit$converged, rc = fit$native_rc, status = fit$status,
        reason = fit$reason, error_message = fit$error_message,
        effect_scale = fit$effect_scale, vce = fit$vce, reference = fit$reference,
        level = fit$level, stringsAsFactors = FALSE)
      populations[[length(populations) + 1L]] <- .tt_sample_population(paste0("fit:", i, ":", k), "outtab", "model",
        variable = block$outcome, model = as.numeric(k), spec = as.numeric(k),
        component = paste0("panel:", block$panel_index),
        values = list(eligible_n = as.numeric(length(block$ids)), observed_n = fit$N_cc, fitted_n = fit$N),
        bases = list(eligible_n = "outcome-eligible original IDs", observed_n = "explicit formula model-frame complete cases before fitter", fitted_n = "verified retained glm rows"),
        reasons = list(observed_n = if (is.na(fit$N_cc)) "fit not attempted: minimum events" else "",
                       fitted_n = if (is.na(fit$N)) fit$reason else ""))
    }
    display_rows[i] <- length(body) + 1L
    label <- .ot_label(samples$original, block$outcome)
    body[[length(body) + 1L]] <- c(if (!is.null(samples$panels)) paste0("   ", label) else label,
      count_text, ratio_text)
    publication[[i]] <- numeric_publication
  }
  cells <- do.call(rbind, body)
  header <- c(" ", paste0(grouplabels, ", events/N (%)"),
    paste0(modellabels, ", ", ratiolabel, " (", format(fits[[1L]][[1L]]$level * 100, trim = TRUE), "% CI)"))
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
    headershade = headershade, zebra = zebra, headercolor = headercolor, zebracolor = zebracolor)
  for (arg in c("title", "footnote")) .tt_check_text_arg(get(arg), arg)
  tt <- .puttab_table(header, cells, title %||% "", footnote %||% "", style,
    sheet = sheet, source = "data", command = "outtab",
    # Pinned outtab.ado:429-436 passes heading positions plus one to
    # puttab's body-relative boldrows: the following outcome rows are bold.
    rules = list(hlines = integer(), vlines = integer(), boldrows = panel_rows + 1L))
  tt$layout$console_footnote <- TRUE
  tt$cols$role <- c("label", "group", "group", rep("est_ci", K))
  # list rowlabel c*, noheader still reserves rowlabel's eight-character
  # variable name. string(40) is a truncation cap, never a minimum width;
  # observed wide data enlarge that cap and their own natural column width.
  tt$cols$console_width <- c(8L, rep(NA_integer_, K + 2L))
  # Native sepby(_hd) separates the inserted header from one body group.
  tt$layout$console_sepby <- TRUE
  tt$rows$block <- rep.int(1L, nrow(tt$body))
  if (length(panel_rows)) tt$rows$type[panel_rows] <- "cat_header"
  tt$rows$key <- rep(NA_character_, nrow(tt$body))
  tt$meta$sample_accounting <- .tt_sample_bind(populations,
    prefixes = paste0("population", seq_along(populations)), commands = rep("outtab", length(populations)))
  suppression <- list(threshold = as.integer(mask$threshold), mode = "primary",
    n_masked = as.integer(n_masked), n_linked = as.integer(n_linked))
  tt$stored <- c(tt$stored, list(table = raw, fits = do.call(rbind, diagnostics),
    N_rows = nrow(cells), N_models = K, N_outcomes = length(samples$outcomes),
    N_panels = if (is.null(samples$panels)) 1L else length(samples$panels),
    minevents = minevents, smallcells = suppression))
  display_states <- display_raw_states <- matrix("", nrow(cells), K)
  display_states[display_rows, ] <- states
  display_raw_states[display_rows, ] <- raw_states
  tt$meta$outtab <- list(slot_role = "specification", keyed_supported = FALSE,
    selected_ids = samples$selected_ids, base_ids = samples$base_ids,
    original = samples$original, snapshot = samples$data, blocks = samples$blocks,
    fits = fits, display_rows = display_rows, panel_rows = panel_rows,
    raw_states = raw_states, publication_states = states, reasons = reasons,
    display_raw_states = display_raw_states, display_publication_states = display_states,
    fit_occurrence = matrix(seq_len(B * K), B, K, byrow = TRUE),
    count_states = count_states, count_mask_reason = count_mask_reason,
    publication_mask_reason = ratio_mask_reason, publication_counts = count_publication,
    publication_ratios = publication, smallcells = suppression)
  validate_tt_table(tt)
}
