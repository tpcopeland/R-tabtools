# Native 2.3+/2.5.1 rate scaffolds: several model blocks per outcome,
# section|level placement, and explicit model-only appendices.
.ct_ratio_scales <- list(HR = .ct_hr_scales,
  IRR = c("irr", "airr", "rr", "arr", "rateratio", "adjustedrateratio",
          "incidencerateratio", "adjustedincidencerateratio"))

.ct_rates_v251 <- function(ratetable, models, a, rate_label, model_names) {
  .ct_check_common(a)
  keyed <- isTRUE(a$keyed) || isTRUE(a$modelonly)
  rs <- .ct_rate_source(ratetable)
  fm <- rs$frame
  width <- ncol(rs$cells)
  if (width < 3L || width %% 3L || !identical(fm$source, "stratetab") ||
      !identical(fm$statistic_ids, "events person_years rate_ci") || isTRUE(fm$rateratio)) {
    .ct_v251_abort("Rate scaffolds require stratetab output without rate-ratio columns.")
  }
  outcomes <- width %/% 3L
  if (!identical(as.integer(fm$n_outcomes), as.integer(outcomes))) .ct_v251_abort("Rate outcome identity is inconsistent.")
  level <- suppressWarnings(as.numeric(fm$ci_level))
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 100) .ct_v251_abort("Rate confidence level is unknown.")
  ids <- vapply(seq_len(outcomes), function(o) {
    id <- fm[[paste0("outcome_id_", o)]] %||% fm$outcome_id[o]
    if (length(id) != 1L || is.na(id)) "" else .ct_trim(as.character(id))
  }, "")
  if (any(!nzchar(ids)) || anyDuplicated(ids)) .ct_v251_abort("Rate outcome identities must be complete and unique.")
  sources <- lapply(seq_along(models), function(f) .ct_source(models[[f]], f, a$cformat, a$cisep))
  layouts <- lapply(seq_along(sources), function(f) .ct_layout(sources[[f]], f))
  lay <- layouts[[1L]]
  if (any(vapply(layouts, function(l) l$mode != lay$mode || l$n_models != lay$n_models, TRUE))) .ct_v251_abort("Model tables must share their model count and layout.")
  nmod <- lay$n_models
  groups <- a$outcomemap
  explicit_map <- !is.null(groups)
  if (isTRUE(a$allmodels)) {
    if (outcomes != 1L || !is.null(groups)) .ct_v251_abort("allmodels requires one rate outcome and no outcomemap.")
    groups <- list(seq_len(nmod))
  } else {
    if (is.null(groups)) groups <- as.list(ids)
    if (is.character(groups)) {
      if (length(groups) == 1L && grepl("\\", groups, fixed = TRUE)) groups <- .ct_split_bs(groups)
      groups <- lapply(groups, function(g) trimws(strsplit(g, "|", fixed = TRUE)[[1L]]))
    }
    if (!is.list(groups) || length(groups) != outcomes) .ct_v251_abort("outcomemap needs one nonempty model-identity group per rate outcome.")
  }
  counts <- lengths(groups)
  if (any(counts < 1L)) .ct_v251_abort("Every rate outcome needs at least one model block.")
  maps <- vector("list", length(sources)); scales <- character()
  for (f in seq_along(sources)) {
    mf <- sources[[f]]$frame
    if (!isTRUE(as.numeric(mf$ci_level) == level)) .ct_v251_abort("Rate and model confidence levels differ.")
    expected <- if (lay$mode == "standard") "estimate ci pvalue" else "estimate_ci pvalue"
    if (!identical(mf$statistic_ids, expected) || !isTRUE(as.numeric(mf$n_models) == nmod)) .ct_v251_abort("Model statistic provenance is inconsistent.")
    mid <- vapply(seq_len(nmod), function(m) .ct_trim(.ct_meta(mf, "model_id", m)), "")
    oid <- vapply(seq_len(nmod), function(m) .ct_trim(.ct_meta(mf, "outcome_id", m)), "")
    labels <- vapply(seq_len(nmod), function(m) .ct_key(.ct_meta(mf, "model_label", m)), "")
    used <- integer()
    maps[[f]] <- vector("list", outcomes)
    for (o in seq_len(outcomes)) {
      g <- groups[[o]]
      if (isTRUE(a$allmodels)) hit <- g else {
        if (!is.character(g) || anyNA(g) || any(!nzchar(.ct_key(g)))) .ct_v251_abort("Model mapping identities must be nonempty strings.")
        hit <- vapply(.ct_trim(g), function(key) {
          found <- if (explicit_map) which((nzchar(mid) & mid == key) |
            (nzchar(oid) & oid == key) | (nzchar(labels) & labels == .ct_key(key))) else
            which(nzchar(oid) & oid == key)
          if (length(found) != 1L) .ct_v251_abort("A model mapping identity is missing or ambiguous.")
          found
        }, 0L)
      }
      if (anyDuplicated(hit) || any(hit %in% used)) .ct_v251_abort("A model block is mapped more than once.")
      used <- c(used, hit)
      maps[[f]][[o]] <- hit
    }
    for (m in seq_len(nmod)) {
      scale <- .ct_effect_norm(.ct_meta(mf, "effect_scale", m))
      family <- names(.ct_ratio_scales)[vapply(.ct_ratio_scales, function(v) scale %in% v, TRUE)]
      if (length(family) != 1L || .ct_meta(mf, "effect_log_scale", m) %in% c("log", "unknown") ||
          identical(.ct_meta(mf, "effect_additive", m), "TRUE")) .ct_v251_abort("Rate composites require declared exponentiated HR or IRR scales.")
      scales <- c(scales, family)
    }
  }
  if (length(unique(scales)) != 1L) .ct_v251_abort("HR and IRR scales cannot be mixed in one rate composite.")
  effect <- a$effect %||% if (scales[1L] == "HR") "aHR" else "IRR"
  reflabel <- a$reflabel %||% "Reference"
  for (value in list(effect, reflabel)) if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) .ct_v251_abort("Effect and reference labels must be nonempty strings.")
  if (!.ct_effect_norm(effect) %in% .ct_ratio_scales[[scales[1L]]]) .ct_v251_abort("Effect label contradicts the model scale.")
  headings <- which(!startsWith(rs$labels, "   ") & nzchar(.ct_trim(rs$labels)) & rs$cells[, 1L] == "")
  if (!length(headings)) .ct_v251_abort("Rate scaffold has no exposure sections.")
  sections <- lapply(seq_along(headings), function(i) {
    end <- if (i < length(headings)) headings[i + 1L] - 1L else length(rs$labels)
    rows <- if (end > headings[i]) seq.int(headings[i] + 1L, end) else integer()
    list(row = headings[i], cats = rows[startsWith(rs$labels[rows], "   ")])
  })
  section_keys <- .ct_key(rs$labels[headings])
  if (keyed && anyDuplicated(section_keys)) .ct_v251_abort("Keyed rate section labels must be unique ignoring case.")
  if (keyed && any(vapply(sections, function(s) anyDuplicated(.ct_key(rs$labels[s$cats])) > 0L, TRUE))) .ct_v251_abort("Keyed rate categories must be unique within sections.")
  bynames <- !is.null(a$rownames)
  selections <- .ct_selection_list(if (bynames) a$rownames else a$rows, length(sources), if (bynames) "rownames" else "rows")
  for (f in seq_along(selections)) if (!bynames && identical(selections[[f]], "all")) selections[[f]] <- seq_along(sources[[f]]$labels)
  selected <- .ct_expand(selections, bynames, sources, dup_error = TRUE, exact = isTRUE(a$rownames_exact))
  if (keyed) selected <- lapply(selected, function(rows) {
    if (anyDuplicated(rows)) .ct_v251_abort("A model row was selected more than once.")
    sort(rows)
  })
  picks <- do.call(rbind, lapply(seq_along(selected), function(f) data.frame(f = f, r = selected[[f]])))
  if (is.null(picks) || !nrow(picks)) .ct_v251_abort("No model rows were selected.")
  rowmap <- integer(length(rs$labels)); refs <- integer(); extra <- integer(); used_sections <- integer()
  est_cols <- function(f) (unlist(maps[[f]], use.names = FALSE) - 1L) * lay$cpm + 1L
  block <- function(source, row) {
    if (!startsWith(source$labels[row], "  ")) return(list(heading = "", rows = row))
    first <- row
    while (first > 1L && startsWith(source$labels[first - 1L], "  ")) first <- first - 1L
    last <- row
    while (last < length(source$labels) && startsWith(source$labels[last + 1L], "  ")) last <- last + 1L
    list(heading = if (first > 1L) source$labels[first - 1L] else "", rows = seq.int(first, last))
  }
  if (!keyed && nrow(picks) != sum(vapply(sections, function(s) max(0L, length(s$cats) - 1L), 0L))) .ct_v251_abort("Selected rows must fill each rate section except its reference.")
  position <- 0L; extra_keys <- character()
  for (p in seq_len(nrow(picks))) {
    f <- picks$f[p]; r <- picks$r[p]; source <- sources[[f]]; label <- .ct_key(source$labels[r])
    if (all(.ct_trim(source$cells[r, ]) == "")) {
      if (keyed) next
      .ct_v251_abort("Heading rows cannot be selected for unkeyed placement.")
    }
    factor_level <- startsWith(source$labels[r], "  "); b <- block(source, r)
    target <- integer(); sec <- integer()
    if (keyed) {
      if (factor_level) {
        sec <- which(section_keys == .ct_key(b$heading))
        if (length(sec)) {
          target <- sections[[sec]]$cats[.ct_key(rs$labels[sections[[sec]]$cats]) == label]
          if (length(target) != 1L) .ct_v251_abort("A selected factor level matches no unique rate category.")
        }
      } else if (label %in% c(section_keys, .ct_key(rs$labels[unlist(lapply(sections, `[[`, "cats"))]))) .ct_v251_abort("A plain model row cannot fill a matching rate section or category.")
      if (!length(target)) {
        if (!a$modelonly) .ct_v251_abort("A selected model row has no rate section; use modelonly to append it.")
        key <- paste(.ct_key(b$heading), label, sep = "|")
        if (key %in% extra_keys) .ct_v251_abort("Duplicate model-only section|level identity.")
        extra_keys <- c(extra_keys, key); extra <- c(extra, p); next
      }
    } else {
      position <- position + 1L
      cumulative <- cumsum(vapply(sections, function(s) max(0L, length(s$cats) - 1L), 0L))
      sec <- which(position <= cumulative)[1L]
      cats <- sections[[sec]]$cats
      target <- cats[.ct_label_match(.ct_trim(rs$labels[cats]), .ct_trim(source$labels[r]))]
      if (!length(target) && !factor_level && length(cats) == 2L) {
        if (.ct_key(rs$labels[cats[1L]]) %in% c("yes", "1", "true") && .ct_key(rs$labels[cats[2L]]) %in% c("no", "0", "false")) .ct_v251_abort("Indicator categories must place zero before one.")
        target <- cats[2L]
      }
      if (length(target) != 1L) .ct_v251_abort("A selected model row matches no unique rate category.")
      if (any(vapply(est_cols(f), function(c) .ct_is_ref(source, r, c), TRUE))) .ct_v251_abort("Select non-reference rows for unkeyed placement.")
    }
    if (rowmap[target]) .ct_v251_abort("Two model rows map to one rate category.")
    rowmap[target] <- p; used_sections <- union(used_sections, sec)
  }
  for (sec in used_sections) {
    cats <- sections[[sec]]$cats; placed <- rowmap[cats]; unfilled <- cats[placed == 0L]
    explicit_ref <- cats[vapply(placed, function(p) p > 0L && all(vapply(est_cols(picks$f[p]), function(c) .ct_is_ref(sources[[picks$f[p]]], picks$r[p], c), TRUE)), TRUE)]
    if (length(explicit_ref) > 1L || (length(explicit_ref) && length(unfilled)) || (!length(explicit_ref) && length(unfilled) != 1L)) .ct_v251_abort("Used rate sections need exactly one coherent reference category.")
    ref <- if (length(explicit_ref)) explicit_ref else unfilled
    for (p in placed[placed > 0L]) {
      f <- picks$f[p]; r <- picks$r[p]; source <- sources[[f]]; b <- block(source, r)
      if (!nzchar(b$heading)) next
      found <- b$rows[.ct_key(source$labels[b$rows]) == .ct_key(rs$labels[ref])]
      if (length(found) != 1L || !all(vapply(est_cols(f), function(c) .ct_is_ref(source, found, c), TRUE))) .ct_v251_abort("Rate reference is not every selected model block's own reference.")
    }
    refs <- c(refs, ref)
  }
  if (!length(used_sections) && !length(extra)) .ct_v251_abort("No model rows with estimates were selected.")
  # Native model-only rows follow source order. A factor block gets its own
  # heading each time the block changes; a plain row resets that block.
  appendix <- list(); previous <- ""; extra_headings <- integer()
  for (p in extra) {
    s <- sources[[picks$f[p]]]; r <- picks$r[p]; b <- block(s, r)
    identity <- .ct_trim(b$heading)
    if (nzchar(b$heading) && !identical(identity, previous)) {
      appendix[[length(appendix) + 1L]] <- list(p = 0L, label = .ct_trim(b$heading), heading = TRUE)
    }
    appendix[[length(appendix) + 1L]] <- list(p = p,
      label = paste0(if (nzchar(b$heading)) "   " else "", .ct_trim(s$labels[r])),
      heading = FALSE)
    previous <- if (nzchar(b$heading)) identity else ""
  }
  appendix_p <- if (length(appendix)) vapply(appendix, `[[`, 0L, "p") else integer()
  if (length(appendix)) {
    extra_headings <- length(rs$labels) + which(vapply(appendix, `[[`, TRUE, "heading"))
    if (!appendix[[1L]]$heading) extra_headings <- c(length(rs$labels) + 1L, extra_headings)
  }
  sizes <- 3L + 2L * counts; starts <- 2L + c(0L, head(cumsum(sizes), -1L)); nc <- 1L + sum(sizes)
  h1 <- h2 <- rep("", nc); h1[1L] <- rs$h1[1L]
  body <- matrix("", length(rs$labels) + length(appendix), nc); body[seq_along(rs$labels), 1L] <- rs$labels
  if (length(appendix)) body[length(rs$labels) + seq_along(appendix), 1L] <- vapply(appendix, `[[`, "", "label")
  for (o in seq_len(outcomes)) {
    j <- starts[o]; rc <- 1L + (o - 1L) * 3L
    h1[j] <- rs$h1[rc + 1L];h2[j + 0:2] <- rs$h2[rc + 1:3]
    body[seq_along(rs$labels), j + 0:2] <- rs$cells[, rc + 0:2]
    for (k in seq_len(counts[o])) {
      col <- j + 3L + 2L * (k - 1L);m <- maps[[1L]][[o]][k]
      label <- .ct_meta(sources[[1L]]$frame, "model_label", m)
      if (!nzchar(label)) label <- paste("Model", k)
      prefix <- if (counts[o] > 1L) paste0(label, ", ") else ""
      h2[col] <- paste0(prefix, effect, " (", .tt_level_text(level), "% CI)")
      h2[col + 1L] <- paste0(prefix, "p-value")
      for (i in seq_len(nrow(body))) {
        if (i %in% refs) {body[i, col] <- reflabel;next}
        p <- if (i <= length(rowmap)) rowmap[i] else appendix_p[i - length(rowmap)]
        if (!p) next
        f <- picks$f[p];r <- picks$r[p];c <- (maps[[f]][[o]][k] - 1L) * lay$cpm + 1L;s <- sources[[f]]
        e <- .ct_trim(s$cells[r, c]);ci <- if (lay$mode == "standard") .ct_trim(s$cells[r, c + 1L]) else ""
        body[i, col] <- if (!nzchar(ci)) e else if (!nzchar(e)) ci else paste(e, ci)
        body[i, col + 1L] <- .ct_trim(s$cells[r, c + lay$cpm - 1L])
      }
    }
  }
  spans <- data.frame(from = starts, to = starts + sizes - 1L)
  cols <- data.frame(role = c("label", unlist(lapply(counts, function(k) c("value", "value", "est_ci", rep(c("est_ci", "pval"), k))))), model = NA_integer_, console_width = NA_integer_, stringsAsFactors = FALSE)
  appendix_heading_rows <- length(rs$labels) + which(appendix_p == 0L)
  type <- ifelse(seq_len(nrow(body)) %in% c(headings, appendix_heading_rows), "var", ifelse(seq_len(nrow(body)) %in% refs, "ref", "level"))
  rows <- data.frame(type = type, indent = ifelse(startsWith(body[, 1L], "   "), 3L, 0L), block = cumsum(seq_len(nrow(body)) %in% c(headings, extra_headings) | seq_len(nrow(body)) == 1L), stringsAsFactors = FALSE)
  style <- tt_resolve_style(font = a$font, fontsize = a$fontsize, borderstyle = a$borderstyle, headershade = a$headershade, zebra = a$zebra, headercolor = a$headercolor, zebracolor = a$zebracolor);style$boldp <- NA_real_
  rate_display <- rs$h1[2L + 3L * (seq_len(outcomes) - 1L)]
  frame <- list(source = "hrcomptab", ci_level = level, n_outcomes = outcomes, n_models = sum(counts), model_id = vapply(unlist(maps[[1L]]), function(m) .ct_meta(sources[[1L]]$frame, "model_id", m), ""), outcome_id = ids, outcome_label = rate_display, effect_scale = rep(scales[1L], sum(counts)), statistic_ids = "events person_years rate_ci estimate_ci pvalue", models_per_outcome = counts,
    outcome_model_map = lapply(seq_len(outcomes), function(o) list(outcome_id = ids[o], source_model = maps[[1L]][[o]])))
  forest <- list(data = NULL, error = "Forest data are unavailable for keyed, model-only or multiple-model rate composites.")
  if (!keyed && all(counts == 1L)) {
    model_map <- do.call(rbind, lapply(maps, function(x) unlist(x, use.names = FALSE)))
    forest <- .ct_forest_rates(rs, sources, sections, headings, refs, rowmap, picks,
      model_map, outcomes, rate_display, frame, rate_label, model_names)
  }
  stored <- list(N_rows = nrow(body) + 3L, N_outcomes = outcomes, N_sections = length(sections), N_modelrows = sum(rowmap > 0L) + length(extra), N_modelframes = length(sources), N_models_per_outcome = if (length(unique(counts)) == 1L) counts[1L] else counts, N_modelonly = length(extra), ci_level = level, rateframe = rate_label, modelframes = paste(model_names, collapse = " "), effect = effect)
  tt <- tt_table(body, header = list(list(text = h1, spans = spans), list(text = h2)), rows = rows, cols = cols, title = a$title %||% rs$title %||% "", footnote = a$footnote %||% "", style = style, stored = stored, command = "hrcomptab", layout = list(indent = 3L, align = "right", console_sepby = FALSE, console_title = TRUE, console_blank = TRUE, header_style = "plain", xlsx_rules = "hrcomptab", sheet = "Composite", md_header = "first", console_footnote = TRUE), meta = list(sheet = .check_sheet(a$sheet), frame = frame, outcomes = outcomes, outcome_spans = spans, section_rows = c(headings, extra_headings), ref_rows = refs, forest = forest$data, forest_error = forest$error, sample_accounting = .tt_sample_bind(c(list(rs$sample_accounting), lapply(sources, `[[`, "sample_accounting")), prefixes = c("ratetable", paste0("modeltable", seq_along(sources))), commands = c("stratetab", vapply(sources, function(s) s$frame$source %||% "unknown", "")))))
  .ct_write(tt, a)
}
