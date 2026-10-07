# Native regtab.ado:2254-2394: statistics first, then term estimate/CI and p.
.rt_collabel_spec <- function(x) {
  if (is.null(x)) return(stats::setNames(character(), character()))
  if (.rt_literal_scalar(x) && is.null(names(x))) {
    tokens <- .rt_literal_tokens(x)
    if (length(tokens) < 2L || length(tokens) %% 2L) .rt_layout_abort("collabels requires term/label pairs.")
    x <- stats::setNames(tokens[seq.int(2L, length(tokens), by = 2L)], tokens[seq.int(1L, length(tokens), by = 2L)])
  }
  if (!is.character(x) || is.null(names(x)) || anyNA(x) || anyNA(names(x)) || any(!nzchar(names(x)))) .rt_layout_abort("collabels must be a named character vector of exact terms and literal labels.")
  x
}

.rt_transpose_labels <- function(u, replacements) {
  labels <- trimws(u$rows$label, whitespace = "[ ]")
  parent <- .rt_placement_parent(u$rows)
  heads <- which(u$rows$kind %in% c("cat_header", "int_header"))
  for (i in which(nzchar(parent) & !u$rows$kind %in% c("cat_header", "int_header"))) {
    h <- heads[u$rows$key[heads] == parent[i]]
    eq <- if (grepl("::", u$rows$key[i], fixed = TRUE)) sub("::.*$", "", u$rows$key[i]) else ""
    prefix <- if (nzchar(eq)) paste0(.rt_stata_eq_label(eq), ": ") else ""
    core <- trimws(u$rows$label[i])
    if (nzchar(prefix) && startsWith(core, prefix)) core <- trimws(substring(core, nchar(prefix) + 1L))
    parent_label <- if (length(h) == 1L) trimws(u$rows$label[h]) else u$rows$parent_label[i] %||% .rt_match_key(parent[i])
    if (is.na(parent_label) || !nzchar(parent_label)) parent_label <- .rt_match_key(parent[i])
    if (nzchar(prefix) && startsWith(parent_label, prefix)) parent_label <- substring(parent_label, nchar(prefix) + 1L)
    labels[i] <- paste0(prefix, parent_label, ": ", core)
  }
  eq <- ifelse(grepl("::", u$rows$key, fixed = TRUE), sub("::.*$", "", u$rows$key), "")
  for (k in seq_along(replacements)) {
    key <- names(replacements)[k]
    qualified <- grepl("::", key, fixed = TRUE)
    if (qualified) {
      target_eq <- sub("::.*$", "", key); key <- sub("^.*?::", "", key)
    } else if (grepl("^[^:]+:[^:]+$", key)) {
      target_eq <- sub(":.*$", "", key); key <- sub("^[^:]+:", "", key); qualified <- TRUE
    }
    hit <- .rt_match_normalize(.rt_match_key(u$rows$key)) == .rt_match_normalize(key)
    if (qualified) hit <- hit & eq == target_eq
    hit <- hit & !u$rows$kind %in% c("cat_header", "int_header")
    if (!any(hit)) .rt_layout_abort(paste0("collabels term '", names(replacements)[k], "' matches no coefficient row."))
    prefix <- ifelse(nzchar(eq[hit]), paste0(.rt_stata_eq_label(eq[hit]), ": "), "")
    labels[hit] <- paste0(prefix, replacements[k])
  }
  labels
}

# Native factor stripes follow their value codes even when an R treatment
# contrast moves its reference level first. Keep source-row indices intact:
# only the transposed publication columns change order. Explicit reftop is
# applied before transpose in regtab.ado:1948-1978 and must retain its order.
.rt_transpose_order <- function(u, displayed, reftop) {
  if (isTRUE(reftop) || length(displayed) < 2L) return(displayed)
  parent <- .rt_placement_parent(u$rows)
  groups <- cumsum(c(TRUE, parent[-1L] != head(parent, -1L)))
  for (group in unique(groups[displayed])) {
    positions <- which(groups[displayed] == group)
    rows <- displayed[positions]
    if (length(rows) < 2L || !all(nzchar(parent[rows])) ||
        !all(u$rows$kind[rows] == "level")) next
    key <- .rt_match_key(u$rows$key[rows])
    # Only simple factor levels have a single declared numeric code. Mixed
    # and interaction blocks keep the adapter's original structural order.
    if (!all(grepl("^-?[0-9]+[.][^#]+$", key))) next
    code <- as.numeric(sub("[.].*$", "", key))
    displayed[positions] <- rows[order(code)]
  }
  displayed
}

.rt_transpose_table <- function(tt, u, text, stats, o, scale) {
  M <- length(u$cells)
  labels <- .rt_transpose_labels(u, o$collabels)
  vectors <- list(o$models)
  h1 <- h2 <- ""
  role <- "label"
  keys <- ""
  blocks <- NA_integer_
  statistic <- ""
  for (s in stats) {
    vectors <- c(vectors, list(c(s$values, rep("", M))[seq_len(M)]))
    h1 <- c(h1, s$label); h2 <- c(h2, ""); role <- c(role, "value")
    keys <- c(keys, ""); blocks <- c(blocks, NA_integer_); statistic <- c(statistic, s$key)
  }
  estimate_header <- if (length(unique(scale$headers)) == 1L) scale$headers[1L] else "Estimate"
  ci_header <- o$cilabel %||% paste0(.rt_pct_text(o$level), "% CI")
  has <- Reduce(`|`, lapply(text$est, function(v) nzchar(trimws(v))), rep(FALSE, nrow(u$rows)))
  has[u$rows$kind %in% c("cat_header", "int_header")] <- FALSE
  for (i in .rt_transpose_order(u, which(has), o$reftop)) {
    value <- vapply(seq_len(M), function(m) {
      e <- text$est[[m]][i]; ci <- text$ci[[m]][i]
      if (nzchar(ci)) paste(e, ci) else e
    }, "")
    vectors <- c(vectors, list(value)); h1 <- c(h1, labels[i])
    h2 <- c(h2, paste0(estimate_header, " (", ci_header, ")")); role <- c(role, "est_ci")
    keys <- c(keys, u$rows$key[i]); blocks <- c(blocks, i); statistic <- c(statistic, "")
    if (!o$nopvalue) {
      vectors <- c(vectors, list(vapply(text$p, `[`, "", i)))
      h1 <- c(h1, labels[i]); h2 <- c(h2, o$plabel %||% "p-value"); role <- c(role, "pval")
      keys <- c(keys, u$rows$key[i]); blocks <- c(blocks, i); statistic <- c(statistic, "")
    }
  }
  if (length(vectors) == 1L) .rt_layout_abort("transpose: no coefficient or statistic to show.")
  # Resolve all anchors in the original transposed columns. Equal anchors
  # retain input order even after earlier columns are inserted.
  add <- o$addcol
  anchors <- vapply(add, function(r) {
    if (is.null(r$after)) return(length(vectors))
    hit <- which(nzchar(keys) & .rt_match_normalize(.rt_match_key(keys)) == .rt_match_normalize(r$after))
    if (!length(hit)) .rt_layout_abort(paste0("addcol after(", r$after, ") matches no displayed term."))
    max(hit)
  }, 0L)
  if (length(add)) {
    map <- integer(); extra <- integer()
    for (j in seq_along(vectors)) {
      map <- c(map, j); extra <- c(extra, 0L)
      for (k in which(anchors == j)) { map <- c(map, 0L); extra <- c(extra, k) }
    }
    vectors2 <- vector("list", length(map)); a <- b <- r <- k <- st <- character(length(map)); bl <- rep(NA_integer_, length(map))
    for (j in seq_along(map)) {
      if (map[j]) {
        v <- map[j]; vectors2[[j]] <- vectors[[v]]; a[j] <- h1[v]; b[j] <- h2[v]
        r[j] <- role[v]; k[j] <- keys[v]; bl[j] <- blocks[v]; st[j] <- statistic[v]
      } else {
        v <- add[[extra[j]]]
        if (length(v$values) > M) .rt_layout_abort("addcol supplies more values than models.")
        vectors2[[j]] <- c(v$values, rep("", M - length(v$values))); a[j] <- v$label; r[j] <- "value"
      }
    }
    vectors <- vectors2; h1 <- a; h2 <- b; role <- r; keys <- k; blocks <- bl; statistic <- st
  }
  tt$meta$regtab_orientation <- "transpose"
  tt$meta$regtab_source_rows <- tt$rows
  tt$meta$regtab_source_flat <- tt$meta$flat
  tt$meta$flat <- NULL
  tt$body <- as.data.frame(vectors, stringsAsFactors = FALSE, col.names = paste0("c", seq_along(vectors)))
  tt$header <- lapply(list(h1, h2), function(v) list(text = v, spans = data.frame(from = integer(), to = integer())))
  tt$cols <- data.frame(role = role, model = NA_integer_, console_width = NA_integer_,
                        term_key = keys, term_block = blocks, statistic_id = statistic, stringsAsFactors = FALSE)
  tt$rows <- data.frame(type = rep("var", M), key = paste0("model:", seq_len(M)),
                       var = NA_character_, level = NA_character_, indent = 0L,
                       block = seq_len(M), p = NA_real_, smd = NA_real_, dim = FALSE,
                       suppression = "", model_index = seq_len(M), stringsAsFactors = FALSE)
  tt$meta$pvals <- NULL
  tt$meta$compact <- TRUE
  tt$stored$N_rows <- M + 3L; tt$stored$N_cols <- ncol(tt$body) + 1L
  validate_tt_table(tt)
  tt
}

.rt_guard_transpose_composition <- function(tabs) {
  if (any(vapply(tabs, function(t) identical(t$meta$regtab_orientation, "transpose"), TRUE))) {
    cli::cli_abort("Transposed regression tables cannot be merged or stacked; publish their unkeyed flat frames with puttab.",
                   class = "tabtools_error_regtab_orientation", call = NULL)
  }
}
