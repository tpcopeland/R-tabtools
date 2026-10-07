# Build a tt_table by hand from a golden's own sinks (Phase 1 exit criterion):
# the CSV gives every cell, the Markdown tells whether the first/last CSV rows
# are the title/footnote, and the console golden supplies any notes printed
# after the listing.
golden_tt <- function(id, style = NULL, rows = NULL, cols = NULL, meta = list()) {
  sc <- golden_scenario(id)
  g <- golden_read_cells(id)
  md <- golden_read_lines(golden_artifact_path(id, paste0(id, ".md")))
  has_title <- length(md) && startsWith(md[1], "### ")
  has_foot <- length(md) >= 2L && grepl("^\\*.*\\*$", md[length(md)]) && md[length(md) - 1L] == ""
  title <- if (has_title) g[1, 1] else NULL
  contract <- golden_publication_contract(id)
  foot <- paste(contract$paragraphs, collapse = " \\ ")
  if (has_title) g <- g[-1, , drop = FALSE]
  if (has_foot) g <- g[-nrow(g), , drop = FALSE]
  header <- list(g[1, ], g[2, ])
  body <- g[-(1:2), , drop = FALSE]
  con <- golden_console_box(golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt"))))
  edge <- which(grepl("^  \\+-+\\+$", con))
  notes <- if (length(edge) >= 2L) con[-seq_len(edge[2])] else character()
  notes <- setdiff(notes[nzchar(notes)], contract$native_footers$console)
  pre <- if (length(edge)) con[seq_len(edge[1] - 1L)] else character()
  pre <- golden_strip_chatter(pre)
  if (length(pre) && sc$command == "table1_tc") meta$console_before <- pre
  tabtools::tt_table(body, header, rows = rows, cols = cols, title = title, footnote = foot,
                     style = style %||% tabtools:::tt_resolve_style(),
                     command = sc$command, meta = meta,
                     notes = if (sc$command == "table1_tc") notes else character())
}

# Stata's own chatter printed before the listing by commands the engine runs
# ("(1 real change made)", "(8 variables copied from linked frame)"). It is
# not part of the table and R never prints it.
golden_strip_chatter <- function(lines) {
  lines[!grepl("^\\s*\\(.*\\)\\s*$", lines) & nzchar(trimws(lines))]
}

golden_console_expected <- function(id) {
  con <- golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt")))
  edge <- which(grepl("^  \\+-+\\+$", con))
  if (golden_scenario(id)$command == "table1_tc" && length(edge) && edge[1] > 1L) {
    pre <- golden_strip_chatter(con[seq_len(edge[1] - 1L)])
    con <- c(pre, con[edge[1]:length(con)])
  }
  con
}

# Option value from a Stata call: opt("a") matches a(...) with optional quotes.
golden_opt <- function(call, name) {
  m <- regmatches(call, regexec(paste0("\\b", name, "\\(\"?([^)\"]*)\"?\\)"), call))[[1]]
  if (length(m)) m[2] else NULL
}
golden_flag <- function(call, name) grepl(paste0("(^|[ ,])", name, "([ ]|$)"), call)

# A tt_table built from a golden with everything a CSV cannot carry --
# style options, p/SMD values, regtab row types, the stars note -- taken
# from the scenario's Stata call, as an engine would supply them.
golden_tt_full <- function(id) {
  sc <- golden_scenario(id)
  call <- sc$stata_call
  num <- function(n) if (is.null(v <- golden_opt(call, n))) NULL else as.numeric(v)
  style <- tabtools:::tt_resolve_style(
    font = golden_opt(call, "font"), fontsize = num("fontsize"),
    borderstyle = golden_opt(call, "borderstyle"),
    headershade = golden_flag(call, "headershade"), zebra = golden_flag(call, "zebra"),
    headercolor = golden_opt(call, "headercolor"), zebracolor = golden_opt(call, "zebracolor"),
    boldp = num("boldp"), highlight = num("highlight"),
    smdthreshold = num("smdthreshold") %||% 0.1,
    dimnonsig = golden_flag(call, "dimnonsig"))
  base <- golden_tt(id, style = style)
  b <- as.matrix(base$body)
  meta <- base$meta
  role <- base$cols$role
  rows <- base$rows
  if (sc$command == "table1_tc") {
    hdr <- lapply(base$header, function(h) h$text)
    if (golden_flag(call, "extraspace")) {
      meta$extraspace <- TRUE
      unprefix <- function(v) ifelse(startsWith(v, " "), substring(v, 2L), v)
      for (j in which(role == "p")) {
        b[, j] <- unprefix(b[, j])
        hdr <- lapply(hdr, function(h) { h[j] <- unprefix(h[j]); h })
      }
    }
    pj <- which(role == "p")
    if (length(pj)) rows$p <- tabtools:::.p_from_text(b[, pj[1]])
    sj <- which(role == "smd")
    if (length(sj)) rows$smd <- suppressWarnings(as.numeric(b[, sj[1]]))
    return(tabtools::tt_table(b, hdr, rows = rows, cols = base$cols, title = base$title,
                              footnote = base$footnote, style = style, command = "table1_tc",
                              meta = meta, notes = base$notes))
  }
  meta$refcat <- golden_opt(call, "refcat") %||% "Reference"
  meta$omitlabel <- golden_opt(call, "omitlabel") %||% "Omitted"
  meta$emptylabel <- golden_opt(call, "emptylabel") %||% "Empty"
  meta$labelwidth <- num("labelwidth") %||% 0
  refs <- c(meta$refcat, meta$omitlabel, meta$emptylabel)
  model <- base$cols$model
  first <- vapply(sort(unique(model[!is.na(model)])), function(m) min(which(model == m)), 1L)
  others <- setdiff(seq_len(ncol(b))[-1], first)
  only_first <- apply(b, 1L, function(r) any(nzchar(r[first])) && !any(nzchar(r[others])) && !any(r[first] %in% refs))
  stat_labels <- c("Observations", "Subjects", "Groups", "AIC", "QICu", "BIC", "Log-likelihood",
                   "ICC", "R\u00b2", "Pseudo R\u00b2", "R\u00b2 / Pseudo R\u00b2",
                   # tabtools 2.1.12 tokens (task 5.17)
                   "Events", "Imputations", "Adjusted R\u00b2", "Root MSE", "F statistic", "Largest FMI")
  add_labels <- if (grepl("addrow\\(", call)) {
    spec <- regmatches(call, regexpr("addrow\\([^)]*\\)", call))
    regmatches(spec, gregexpr('"[^"]+"', spec))[[1]]
  } else character()
  add_labels <- gsub('"', "", add_labels)
  type <- rows$type
  type[only_first & b[, 1] %in% stat_labels] <- "stat"
  type[only_first & b[, 1] %in% add_labels] <- "addrow"
  type[grepl("^(Variance|Covariance|Residual Variance|Median (Odds|Hazard) Ratio|var\\(|cov\\()", b[, 1])] <- "re"
  rows$type <- type
  if (style$dimnonsig) {
    pv <- sapply(first, function(j) tabtools:::.p_from_text(b[, j + sum(model == model[j], na.rm = TRUE) - 1L]))
    pv <- matrix(pv, nrow(b))
    sig <- apply(pv, 1L, function(p) any(!is.na(p) & p < 0.05))
    isref <- apply(b[, first, drop = FALSE], 1L, function(r) any(r %in% refs))
    anyval <- apply(b[, -1, drop = FALSE], 1L, function(r) any(nzchar(r)))
    cathead <- !anyval & !isref
    dim <- !sig & (type %in% c("var", "level")) & anyval
    dim[isref] <- TRUE
    for (i in which(cathead)) {
      k <- i + 1L
      has_ref <- FALSE
      any_sig <- FALSE
      while (k <= nrow(b) && startsWith(b[k, 1], " ")) {
        has_ref <- has_ref || isref[k]
        any_sig <- any_sig || !dim[k]
        k <- k + 1L
      }
      dim[i] <- has_ref && !any_sig
    }
    rows$dim <- dim
  }
  meta$xlsx_footnote <- base$footnote
  tabtools::tt_table(b, lapply(base$header, function(h) h$text), rows = rows, cols = base$cols,
                     title = base$title, footnote = base$footnote, style = style,
                     command = "regtab", meta = meta)
}
