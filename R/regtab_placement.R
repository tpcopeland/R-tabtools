# Publication placement: pinned regtab.ado:1948-2030 and _regtab_addrow.ado.
.rt_layout_abort <- function(message) {
  cli::cli_abort(message, class = "tabtools_error_regtab_layout", call = NULL)
}

.rt_literal_scalar <- function(x) {
  is.character(x) && length(x) == 1L && is.null(dim(x)) && !is.na(x)
}

# Read literal tokens, including Stata compound quotes; never evaluate text.
.rt_literal_tokens <- function(s) {
  out <- character()
  s <- trimws(s)
  while (nzchar(s)) {
    compound <- startsWith(s, "`\"")
    quoted <- compound || startsWith(s, "\"")
    if (quoted) {
      opener <- if (compound) 2L else 1L
      closer <- if (compound) "\"'" else "\""
      tail <- substring(s, opener + 1L)
      pos <- regexpr(closer, tail, fixed = TRUE)[1L]
      if (pos < 1L) .rt_layout_abort("Unmatched quote in regression layout specification.")
      out <- c(out, substr(tail, 1L, pos - 1L))
      s <- trimws(substring(tail, pos + nchar(closer)))
    } else {
      hit <- regexpr("[[:space:]]", s)[1L]
      if (hit < 0L) { out <- c(out, s); s <- "" } else {
        out <- c(out, substr(s, 1L, hit - 1L))
        s <- trimws(substring(s, hit + 1L))
      }
    }
  }
  out
}

.rt_literal_chunks <- function(s) {
  s <- trimws(s)
  # Whole-spec quote layer, as _regtab_unwrap; only when it encloses all text.
  if (startsWith(s, "`\"") && endsWith(s, "\"'")) {
    t <- .rt_literal_tokens(s)
    if (length(t) == 1L) s <- t
  }
  scan <- .stacktab_scan(s)
  cuts <- which(scan$ch == "\\" & !scan$quoted)
  from <- c(1L, cuts + 1L)
  to <- c(cuts - 1L, length(scan$ch))
  out <- vapply(seq_along(from), function(i) {
    if (to[i] < from[i]) "" else paste0(scan$ch[from[i]:to[i]], collapse = "")
  }, "")
  out <- trimws(out)
  if (any(!nzchar(out))) .rt_layout_abort("An empty regression layout specification follows a separator.")
  out
}

.rt_cellnote_spec <- function(x) {
  if (is.null(x)) return(list())
  if (.rt_literal_scalar(x)) x <- lapply(.rt_literal_chunks(x), function(s) {
    t <- .rt_literal_tokens(s)
    if (length(t) != 3L || !grepl("^[0-9]+$", t[2L])) .rt_layout_abort("cellnote requires row label, model index and literal text.")
    list(row = t[1L], model = as.numeric(t[2L]), text = t[3L])
  })
  if (!is.list(x) || is.data.frame(x)) .rt_layout_abort("cellnote must be a list of row/model/text records or a native specification string.")
  for (r in x) {
    if (!is.list(r) || is.null(names(r)) || anyDuplicated(names(r)) ||
        !setequal(names(r), c("row", "model", "text")) ||
        !.rt_literal_scalar(r$row) || !nzchar(trimws(r$row)) || !.rt_literal_scalar(r$text) ||
        !is.numeric(r$model) || is.complex(r$model) || !is.null(dim(r$model)) || length(r$model) != 1L || !is.finite(r$model) ||
        r$model < 1 || r$model != floor(r$model)) .rt_layout_abort("cellnote requires records list(row = label, model = index, text = literal).")
  }
  x
}

.rt_added_spec <- function(x, arg = "addrow") {
  if (is.null(x)) return(list())
  if (.rt_literal_scalar(x) && is.null(names(x))) x <- lapply(.rt_literal_chunks(x), function(s) {
    scan <- .stacktab_scan(s)
    commas <- which(scan$ch == "," & !scan$quoted)
    after <- NULL
    if (length(commas)) {
      at <- tail(commas, 1L)
      suffix <- substring(s, at)
      m <- regmatches(suffix, regexec("^,[ ]*after\\(([^()]*)\\)[ ]*$", suffix))[[1L]]
      if (!length(m)) .rt_layout_abort(paste0(arg, " has an invalid after() placement."))
      after <- trimws(m[2L]); s <- substr(s, 1L, at - 1L)
    }
    t <- .rt_literal_tokens(s)
    if (!length(t)) .rt_layout_abort(paste0(arg, " requires a label."))
    list(label = t[1L], values = t[-1L], after = after)
  }) else {
    # Preserve existing named list(Label = vector) API; structured records add after.
    if (!is.list(x) && !is.atomic(x)) .rt_layout_abort(paste0(arg, " must be a named list or list of label/values/after records."))
    record <- is.list(x) && length(x) && all(vapply(x, function(r) is.list(r) && all(c("label", "values") %in% names(r)), TRUE))
    if (!record) {
      if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x)))) .rt_layout_abort(paste0(arg, " needs labels for all entries."))
      x <- lapply(seq_along(x), function(i) list(label = names(x)[i], values = x[[i]], after = NULL))
    }
  }
  lapply(x, function(r) {
    if (anyDuplicated(names(r)) || any(!names(r) %in% c("label", "values", "after")) ||
        !.rt_literal_scalar(r$label) || !nzchar(r$label) ||
        !is.atomic(r$values) || !is.null(dim(r$values)) || is.factor(r$values)) .rt_layout_abort(paste0(arg, " requires literal labels and an ordinary values vector."))
    if (!is.null(r$after) && (!.rt_literal_scalar(r$after) || !nzchar(trimws(r$after)))) .rt_layout_abort(paste0(arg, " after() requires one nonempty term."))
    r$values <- if (is.numeric(r$values)) vapply(r$values, function(v) if (is.na(v)) "" else .tt_fmt_sig(v, 15L), "") else as.character(r$values)
    r$values[is.na(r$values)] <- ""
    r
  })
}

.rt_layout_options <- function(reftop, cellnote, transpose, collabels, addcol, addrow, dimnonsig, highlight, boldp) {
  for (value in list(reftop, transpose)) if (!is.logical(value) || length(value) != 1L || is.na(value)) .rt_layout_abort("reftop and transpose must be TRUE or FALSE.")
  if (transpose && (!is.null(addrow) || dimnonsig || !is.null(highlight) || !is.null(boldp))) .rt_layout_abort("transpose cannot be combined with addrow, dimnonsig, highlight or boldp; use addcol for columns.")
  if (!transpose && (!is.null(collabels) || !is.null(addcol))) .rt_layout_abort("collabels and addcol require transpose.")
  list(reftop = reftop, cellnote = .rt_cellnote_spec(cellnote), transpose = transpose,
       collabels = .rt_collabel_spec(collabels), addcol = .rt_added_spec(addcol, "addcol"))
}

# Equation-aware structural parent. A supplies parent_key; fallback mirrors
# _regtab_fvparent for existing adapters, never guesses from displayed labels.
.rt_placement_parent <- function(rows) {
  if (!is.null(rows$parent_key)) return(ifelse(is.na(rows$parent_key), "", rows$parent_key))
  vapply(rows$key, function(key) {
    raw <- .rt_match_key(key)
    parts <- strsplit(raw, "#", fixed = TRUE)[[1L]]
    fv <- grepl("^[0-9bon]*[0-9][0-9bon]*[.]", parts)
    if (!any(fv)) return("")
    parts[fv] <- sub("^[^.]+[.]", "", parts[fv])
    parts <- sub("^c[.]", "", parts)
    prefix <- if (grepl("::", key, fixed = TRUE)) sub("::.*$", "::", key) else ""
    paste0(prefix, paste(parts, collapse = "#"))
  }, "")
}

.rt_reftop <- function(u) {
  parent <- .rt_placement_parent(u$rows)
  structural <- u$rows$kind %in% c("cat_header", "int_header")
  parent[structural] <- ""
  block <- cumsum(c(TRUE, parent[-1L] != head(parent, -1L)))
  order <- seq_len(nrow(u$rows))
  for (b in unique(block[nzchar(parent)])) {
    ix <- which(block == b & nzchar(parent))
    bases <- ix[Reduce(`|`, lapply(u$cells, function(c) c$status[ix] %in% c("base", "ref")))]
    if (length(bases) > 1L) .rt_layout_abort(paste0("reftop: models use different reference levels of ", parent[ix[1L]], "."))
    if (length(bases)) order[ix] <- c(bases, setdiff(ix, bases))
  }
  .rt_subset(u, order)
}

.rt_after_row <- function(rows, term) {
  parent <- .rt_placement_parent(rows)
  raw_parent <- .rt_match_key(parent)
  candidates <- which(nzchar(parent) & raw_parent == term & !rows$kind %in% c("cat_header", "int_header"))
  if (length(candidates)) {
    if (length(unique(parent[candidates])) != 1L || any(diff(candidates) != 1L)) .rt_layout_abort(paste0("after(", term, ") matches several factor blocks."))
    return(max(candidates))
  }
  normalized <- .rt_match_normalize(.rt_match_key(rows$key))
  hit <- which(normalized == .rt_match_normalize(term) & !rows$kind %in% c("cat_header", "int_header"))
  if (length(hit) != 1L) .rt_layout_abort(paste0("after(", term, ") must match exactly one coefficient row."))
  hit
}

.rt_apply_cellnote <- function(u, text, records) {
  M <- length(u$cells)
  for (m in seq_len(M)) {
    u$cells[[m]]$override_origin <- rep("", nrow(u$rows))
    u$cells[[m]]$override_text <- rep("", nrow(u$rows))
  }
  for (r in records) {
    if (r$model > M) .rt_layout_abort("cellnote model index is outside the table.")
    hit <- which(trimws(u$rows$label, whitespace = "[ ]") == trimws(r$row, whitespace = "[ ]"))
    if (length(hit) != 1L) .rt_layout_abort(paste0("cellnote row label '", r$row, "' must match exactly one body row."))
    m <- as.integer(r$model)
    text$est[[m]][hit] <- r$text
    text$ci[[m]][hit] <- text$p[[m]][hit] <- ""
    structural <- u$rows$kind[hit] %in% c("cat_header", "int_header")
    u$cells[[m]]$status[hit] <- if (structural) "" else "masked"
    u$cells[[m]]$override_origin[hit] <- "cellnote"
    u$cells[[m]]$override_text[hit] <- r$text
    for (field in c("estimate", "conf.low", "conf.high", "p.value")) u$cells[[m]][[field]][hit] <- NA_real_
  }
  list(u = u, text = text)
}

.rt_numeric_table <- function(u, publication = FALSE) {
  states <- if (publication) c("est", "constrained") else c("est", "notest", "constrained")
  ok <- lapply(u$cells, function(c) c$status %in% states & is.finite(c$estimate))
  has <- Reduce(`|`, ok, rep(FALSE, nrow(u$rows)))
  if (!any(has)) return(NULL)
  value <- matrix(NA_real_, sum(has), length(u$cells))
  for (m in seq_along(u$cells)) value[ok[[m]][has], m] <- u$cells[[m]]$estimate[has][ok[[m]][has]]
  dimnames(value) <- list(.rt_rowname(u$rows$label[has], seq_len(sum(has))), paste0("c", seq_along(u$cells)))
  value
}

.rt_long_rows <- function(u, type, row_map = seq_len(nrow(u$rows))) {
  do.call(rbind, lapply(seq_along(u$cells), function(m) {
    c <- u$cells[[m]]
    out <- data.frame(row = as.integer(row_map), key = u$rows$key, label = u$rows$label,
                      type = type, model = as.integer(m), model_index = as.integer(m),
                      status = c$status, estimate = c$estimate, conf.low = c$conf.low,
                      conf.high = c$conf.high, p.value = c$p.value, stringsAsFactors = FALSE)
    for (field in setdiff(names(c), names(out))) out[[field]] <- c[[field]]
    for (field in intersect(c("parent_key", "term_signature"), names(u$rows))) out[[field]] <- u$rows[[field]]
    out
  }))
}

.rt_place_extra <- function(u, text, labels, keys, type, dim, stats, addrow) {
  n <- nrow(u$rows); M <- length(u$cells)
  anchors <- vapply(addrow, function(r) if (is.null(r$after)) n + length(stats) else .rt_after_row(u$rows, r$after), 0L)
  all <- c(stats, lapply(seq_along(addrow), function(i) {
    r <- addrow[[i]]; r$key <- paste0("addrow:", i); r$type <- "addrow"; r$inserted <- !is.null(r$after); r
  }))
  original <- seq_len(n + length(stats))
  map <- source <- integer()
  for (j in original) {
    map <- c(map, j); source <- c(source, 0L)
    for (i in which(anchors == j)) { map <- c(map, 0L); source <- c(source, length(stats) + i) }
  }
  # Appended addrows on a table with no statistic rows are handled above.
  analytic_map <- match(seq_len(n), map)
  out_labels <- out_type <- character(length(map)); out_dim <- logical(length(map))
  out_keys <- keys[rep(NA_integer_, length(map)), , drop = FALSE]
  inserted <- logical(length(map))
  result <- lapply(seq_len(M), function(m) list(est = character(length(map)), ci = character(length(map)), p = character(length(map))))
  for (j in seq_along(map)) {
    i <- map[j]
    if (i > 0L && i <= n) {
      out_labels[j] <- labels[i]; out_type[j] <- type[i]; out_dim[j] <- dim[i]; out_keys[j, ] <- keys[i, ]
      for (m in seq_len(M)) { result[[m]]$est[j] <- text$est[[m]][i]; result[[m]]$ci[j] <- text$ci[[m]][i]; result[[m]]$p[j] <- text$p[[m]][i] }
    } else {
      k <- if (i > n) i - n else source[j]
      r <- all[[k]]
      if (length(r$values) > M && !identical(r$type, "addrow")) {
        .rt_layout_abort("A statistic row supplies more values than models.")
      }
      indent <- if (!is.null(r$after)) nchar(labels[anchors[k - length(stats)]]) - nchar(sub("^ +", "", labels[anchors[k - length(stats)]])) else 0L
      out_labels[j] <- paste0(strrep(" ", indent), r$label); out_type[j] <- r$type %||% "stat"
      out_keys$key[j] <- r$key; inserted[j] <- isTRUE(r$inserted)
      # Native ordinary addrow ignores values beyond the model count.
      # Transposed addcol retains its separate excess-value refusal.
      values <- c(r$values, rep("", M))[seq_len(M)]
      for (m in seq_len(M)) result[[m]]$est[j] <- values[m]
    }
  }
  # Build row-aligned publication cells for flat state transport. All
  # synthetic rows are structural with blank status and missing numbers.
  cells <- lapply(u$cells, function(c) {
    out <- c[rep(NA_integer_, length(map)), , drop = FALSE]
    for (nm in names(out)) {
      if (is.character(out[[nm]])) out[[nm]][] <- ""
      if (is.logical(out[[nm]])) out[[nm]][] <- FALSE
    }
    out[analytic_map, ] <- c
    rownames(out) <- NULL; out
  })
  list(labels = out_labels, keys = out_keys, type = out_type, dim = out_dim,
       inserted = inserted, cells = cells, row_map = as.integer(analytic_map),
       text = list(est = lapply(result, `[[`, "est"), ci = lapply(result, `[[`, "ci"), p = lapply(result, `[[`, "p")))
}

# Structural parent labels from retained/custom fit metadata, before union.
.rt_parent_labels <- function(rows, fit) {
  parent <- .rt_placement_parent(rows)
  rows$parent_label <- rep("", nrow(rows))
  if (!any(nzchar(parent))) return(rows)
  frame <- if (is.data.frame(fit)) NULL else .rt_frame(fit)$mf
  for (i in which(nzchar(parent))) {
    key <- .rt_match_key(parent[i])
    h <- which(rows$kind %in% c("cat_header", "int_header") & rows$key == parent[i])
    if (length(h) == 1L) rows$parent_label[i] <- rows$label[h] else if (is.data.frame(fit)) {
      variable <- as.character(.rt_col(fit, "variable", ""))
      label <- as.character(.rt_col(fit, "var_label", ""))
      valid <- variable == key & !is.na(label) & nzchar(label)
      available <- unique(label[valid])
      rows$parent_label[i] <- if (length(available) == 1L) available else key
    } else {
      rows$parent_label[i] <- .rt_var_label(frame, key)
    }
  }
  rows
}
