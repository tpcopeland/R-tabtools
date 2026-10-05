# In-memory composition of tt_tables (task 7.14; the interop research's
# recommendation 9): tt_merge() puts tables side by side, joining their rows
# on rows$key (task 6.7) as regtab joins its models' rows, never on the
# labels (gtsummary's tbl_merge() joins on display labels and hides the
# many-to-many warning); tt_stack() puts tables with the same columns one
# under another, as row groups.

#' Put tables side by side, joined on their row keys
#'
#' Joins the body rows of two or more tables on `rows$key`, in order of
#' first appearance, placing a row only a later table has as [regtab()]
#' places a later model's rows: a new level after its variable's preceding
#' level, a new variable after the earlier tables' variables (before the
#' intercept and the statistics). A table without a row has blank cells
#' there. Rows are never matched by
#' their labels. Each table's value columns follow the label column in
#' order, with their own header rows (a table with fewer header rows gets
#' blank ones on top). The label-column headers come from the first table.
#' `spanners` name each table in the model-label row
#' of model tables such as [regtab()]'s: a one-model table's label is
#' replaced by its spanner, a multi-model table's labels are prefixed
#' with it (`Adjusted: M1`), so every sink shows both; for tables without
#' models they add a header row over each table's columns. A row is dimmed
#' (`dimnonsig`) only when every table that has it dims it. The workbook
#' footnote keeps the tables' stars note. The result keeps the first
#' table's `style` and each model's recorded effect-scale provenance.
#'
#' Every table needs a key for every body row, each key once: [regtab()]
#' tables have them (`6.cyl`, `_cons`, `stat:n`, `addrow:<label>`, ...).
#' A [puttab()] table has none, so it is refused; give `rows$key` yourself
#' to merge such a table. A `table1_tc()` table is refused: its keys are
#' its variables' labels. The tables must come from the same command,
#' whose layout the result keeps, and every model must have the same
#' columns (the same `compact` and `nopvalue`).
#' @param ... `tt_table` objects, or one list of them (names become the
#'   `spanners`).
#' @param spanners Optional labels, one per table, naming each table's
#'   columns in the header.
#' @param title,footnote Title and footnote of the result (default: the
#'   first table's).
#' @return A `tt_table`; `stored$tables` holds each table's `stored`.
#'   `meta$sample_accounting` preserves each source ledger separately, as
#'   described in [tt_table()].
#' @seealso [tt_stack()], [regtab()]
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' crude <- regtab(lm(mpg ~ cyl, d), stats = "n")
#' adj <- regtab(lm(mpg ~ cyl + wt + hp, d), stats = "n")
#' tt_merge(Crude = crude, Adjusted = adj)
#' @export
tt_merge <- function(..., spanners = NULL, title = NULL, footnote = NULL) {
  tabs <- .tt_compose_args(list(...), "tt_merge")
  if (is.null(spanners) && !is.null(names(tabs)) && all(nzchar(names(tabs)))) spanners <- names(tabs)
  if (!is.null(spanners) && (!is.character(spanners) || length(spanners) != length(tabs) || anyNA(spanners))) {
    cli::cli_abort("{.arg spanners} must be one label per table ({length(tabs)}).", call = NULL)
  }
  .tt_compose_same_command(tabs, "tt_merge")
  # table1_tc keys a row by its variable's label (Stata's factor_sep), so a
  # merge would join on labels (stack review item 8).
  if (identical(tabs[[1]]$command, "table1_tc")) {
    cli::cli_abort(c("{.fn tt_merge} cannot merge {.fn table1_tc} tables: their row keys are the variables' labels.",
                     "i" = "{.fn tt_merge} joins rows on keys, never on labels; describe the groups in one {.fn table1_tc} call with {.arg by}."),
                   call = NULL)
  }
  for (k in seq_along(tabs)) {
    key <- tabs[[k]]$rows$key
    if (is.null(key) || anyNA(key)) {
      cli::cli_abort(c("Table {k} ({.fn {tabs[[k]]$command}}) has body rows without a key ({.field rows$key}).",
                       "i" = "{.fn tt_merge} joins rows on their keys, never on their labels; {.fn regtab} tables have keys, or set {.field rows$key}."),
                     call = NULL)
    }
    if (anyDuplicated(key)) {
      cli::cli_abort("Table {k} has the key{?s} {.val {unique(key[duplicated(key)])}} on more than one row.", call = NULL)
    }
  }
  # Every model needs the same columns: the regtab workbook layout reads
  # one column count and p position for all models (stack review item 1).
  sig <- unlist(lapply(tabs, function(t) {
    m <- t$cols$model
    vapply(split(t$cols$role[!is.na(m)], m[!is.na(m)]), paste, "", collapse = " ")
  }))
  if (length(unique(sig)) > 1L) {
    cli::cli_abort(c("The tables' models have different columns ({.val {unique(sig)}}).",
                     "i" = "{.fn tt_merge} needs one column layout for every model: make the tables with the same {.arg compact} and {.arg nopvalue}."),
                   call = NULL)
  }
  keys <- .tt_key_union(lapply(tabs, function(t) t$rows$key), lapply(tabs, function(t) t$rows$var))
  nb <- length(keys)
  # Row metadata and labels from the first table that has the row.
  src <- vapply(keys, function(k) which(vapply(tabs, function(t) k %in% t$rows$key, TRUE))[1], 1L)
  at <- function(t, k) match(k, t$rows$key)
  labels <- vapply(seq_len(nb), function(i) tabs[[src[i]]]$body[[1]][at(tabs[[src[i]]], keys[i])], "")
  rows <- do.call(rbind, lapply(seq_len(nb), function(i) {
    t <- tabs[[src[i]]]
    t$rows[at(t, keys[i]), , drop = FALSE]
  }))
  rownames(rows) <- NULL
  rows$block <- seq_len(nb)
  # A row is dimmed only when every table that has it dims it, and its p is
  # the smallest of the tables' (stack review item 4).
  for (i in seq_len(nb)) {
    has <- Filter(function(t) keys[i] %in% t$rows$key, tabs)
    rows$dim[i] <- all(vapply(has, function(t) isTRUE(t$rows$dim[at(t, keys[i])]), TRUE))
    p <- vapply(has, function(t) as.numeric(t$rows$p[at(t, keys[i])] %||% NA_real_), 0)
    rows$p[i] <- if (all(is.na(p))) NA_real_ else min(p, na.rm = TRUE)
  }
  body <- list(labels)
  cols <- tabs[[1]]$cols[1, , drop = FALSE]
  nh <- max(vapply(tabs, function(t) length(t$header), 1L))
  first_pad <- nh - length(tabs[[1]]$header)
  hdr <- lapply(seq_len(nh), function(h) {
    label <- if (h > first_pad) tabs[[1]]$header[[h - first_pad]]$text[1] else ""
    list(text = label, spans = data.frame(from = integer(), to = integer()))
  })
  pv <- NULL
  any_pv <- any(vapply(tabs, function(t) !is.null(t$meta$pvals), TRUE))
  m_off <- 0L
  c_off <- 1L
  span_row <- list(text = "", spans = data.frame(from = integer(), to = integer()))
  ranges <- list()
  for (k in seq_along(tabs)) {
    t <- tabs[[k]]
    idx <- at(t, keys)
    vc <- seq_len(ncol(t$body))[-1]
    for (j in vc) {
      v <- t$body[[j]][idx]
      v[is.na(v)] <- ""
      body[[length(body) + 1L]] <- v
    }
    cc <- t$cols[vc, , drop = FALSE]
    nm <- max(c(0L, t$cols$model), na.rm = TRUE)
    cc$model <- cc$model + m_off
    cols <- rbind(cols, cc)
    # Header rows, bottom-aligned; spans shifted to the new columns.
    pad <- nh - length(t$header)
    for (h in seq_len(nh)) {
      th <- if (h > pad) t$header[[h - pad]] else NULL
      txt <- if (is.null(th)) rep("", length(vc)) else th$text[vc]
      hdr[[h]]$text <- c(hdr[[h]]$text, txt)
      if (!is.null(th) && !is.null(th$spans) && nrow(th$spans)) {
        hdr[[h]]$spans <- rbind(hdr[[h]]$spans, data.frame(from = th$spans$from + c_off - 1L, to = th$spans$to + c_off - 1L))
      }
    }
    span_row$text <- c(span_row$text, if (is.null(spanners)) rep("", length(vc)) else c(spanners[k], rep("", length(vc) - 1L)))
    if (!is.null(spanners) && length(vc) > 1L) {
      span_row$spans <- rbind(span_row$spans, data.frame(from = c_off + 1L, to = c_off + length(vc)))
    }
    if (any_pv) {
      pk <- t$meta$pvals
      pk <- if (is.null(pk)) matrix(NA_real_, nrow(t$body), nm) else matrix(pk, nrow = nrow(t$body))
      pv <- cbind(pv, pk[idx, , drop = FALSE])
    }
    ranges[[k]] <- c_off + seq_along(vc)
    m_off <- m_off + nm
    c_off <- c_off + length(vc)
  }
  n_models <- vapply(tabs, function(t) length(unique(stats::na.omit(t$cols$model))), 1L)
  fold <- FALSE
  if (!is.null(spanners)) {
    fold <- nh >= 2L && all(n_models >= 1L) && all(vapply(tabs, function(t) length(t$header), 1L) == nh)
    if (fold) {
      # Model tables: the spanners go into the model-label row, which the
      # workbook, CSV and Markdown sinks all show (stack review item 2): a
      # one-model table's label is replaced (Model, Model -> Crude,
      # Adjusted), a multi-model table's labels are prefixed (Adjusted: M1).
      h1 <- hdr[[1]]
      for (k in seq_along(tabs)) {
        cc <- ranges[[k]]
        if (n_models[k] == 1L) {
          h1$text[cc] <- c(spanners[k], rep("", length(cc) - 1L))
          h1$spans <- h1$spans[!(h1$spans$from %in% cc | h1$spans$to %in% cc), , drop = FALSE]
          if (length(cc) > 1L) h1$spans <- rbind(h1$spans, data.frame(from = min(cc), to = max(cc)))
        } else {
          for (j in cc[!duplicated(cols$model[cc])]) {
            lab <- trimws(h1$text[j])
            h1$text[j] <- if (nzchar(lab)) paste0(spanners[k], ": ", lab) else spanners[k]
          }
        }
      }
      hdr[[1]] <- h1
    } else {
      hdr <- c(list(span_row), hdr)
    }
  }
  body <- as.data.frame(body, stringsAsFactors = FALSE, col.names = paste0("c", seq_along(body)))
  rownames(cols) <- NULL
  first <- tabs[[1]]
  footnote <- footnote %||% first$footnote
  meta <- first$meta
  meta$pvals <- pv
  meta$regtab_rows <- NULL
  meta$xlsx_footnote <- .tt_compose_xlsx_footnote(tabs, footnote)
  meta$frame <- .tt_merge_frame(tabs, if (fold) hdr[[1]]$text[-1][!duplicated(cols$model[-1])])
  meta$sample_accounting <- .tt_sample_bind(
    lapply(tabs, function(t) t$meta[["sample_accounting", exact = TRUE]]),
    prefixes = paste0("table", seq_along(tabs)),
    commands = vapply(tabs, `[[`, "", "command"))
  stored <- list(tables = lapply(tabs, `[[`, "stored"))
  if (!is.null(spanners)) names(stored$tables) <- spanners
  out <- tt_table(body, header = hdr, rows = rows, cols = cols, title = title %||% first$title,
                  footnote = footnote, style = first$style, stored = stored,
                  command = first$command, layout = first$layout, meta = meta)
  validate_tt_table(out)
}

#' Stack tables with the same columns as row groups
#'
#' Puts the body rows of two or more tables one under another, each block
#' optionally under a group label row; the result keeps the first table's
#' header, which every table must share (the same header text and column
#' roles), and its command's layout. Row keys are kept as they are, so a
#' key may repeat across groups. Model columns retain unsafe effect-scale
#' provenance from any group: for example, stacking an exponentiated ratio
#' with a raw log ratio still prevents [hrcomptab()] from treating the
#' entire column as hazard ratios.
#' @param ... `tt_table` objects, or one list of them (names become the
#'   `groups`).
#' @param groups Optional group labels, one per table: a row with the label
#'   above each block.
#' @param title,footnote Title and footnote of the result (default: the
#'   first table's).
#' @return A `tt_table`; `stored$tables` holds each table's `stored`.
#'   `meta$sample_accounting` preserves each source ledger separately, as
#'   described in [tt_table()].
#' @seealso [tt_merge()], [stacktab()]
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' auto <- regtab(lm(mpg ~ cyl + wt, d[d$am == 0, ]), stats = "n")
#' manual <- regtab(lm(mpg ~ cyl + wt, d[d$am == 1, ]), stats = "n")
#' tt_stack(Automatic = auto, Manual = manual)
#' @export
tt_stack <- function(..., groups = NULL, title = NULL, footnote = NULL) {
  tabs <- .tt_compose_args(list(...), "tt_stack")
  if (is.null(groups) && !is.null(names(tabs)) && all(nzchar(names(tabs)))) groups <- names(tabs)
  if (!is.null(groups) && (!is.character(groups) || length(groups) != length(tabs) || anyNA(groups))) {
    cli::cli_abort("{.arg groups} must be one label per table ({length(tabs)}).", call = NULL)
  }
  .tt_compose_same_command(tabs, "tt_stack")
  first <- tabs[[1]]
  htext <- function(t) lapply(t$header, `[[`, "text")
  for (k in seq_along(tabs)[-1]) {
    t <- tabs[[k]]
    if (ncol(t$body) != ncol(first$body) || !identical(t$cols$role, first$cols$role) || !identical(htext(t), htext(first))) {
      cli::cli_abort(c("Table {k} does not have table 1's columns: {.fn tt_stack} needs the same header text and column roles.",
                       "i" = "Put tables with different columns side by side with {.fn tt_merge}."), call = NULL)
    }
  }
  nc <- ncol(first$body)
  blank_row <- function(t) {
    r <- t$rows[0, , drop = FALSE]
    r[1, ] <- NA
    r$type <- "var"
    r$indent <- 0L
    r$dim <- FALSE
    r$suppression <- 0L
    r
  }
  body <- NULL
  rows <- NULL
  pv <- NULL
  any_pv <- any(vapply(tabs, function(t) !is.null(t$meta$pvals), TRUE))
  nm <- max(c(0L, first$cols$model), na.rm = TRUE)
  for (k in seq_along(tabs)) {
    t <- tabs[[k]]
    b <- t$body
    names(b) <- paste0("c", seq_len(nc))
    r <- t$rows
    if (!is.null(groups)) {
      g <- as.data.frame(as.list(c(groups[k], rep("", nc - 1L))), stringsAsFactors = FALSE, col.names = names(b))
      b <- rbind(g, b)
      r <- rbind(blank_row(t)[names(r)], r)
    }
    body <- rbind(body, b)
    rows <- rbind(rows, r[names(first$rows)])
    if (any_pv) {
      pk <- t$meta$pvals
      pk <- if (is.null(pk)) matrix(NA_real_, nrow(t$body), nm) else matrix(pk, nrow = nrow(t$body))
      if (!is.null(groups)) pk <- rbind(rep(NA_real_, ncol(pk)), pk)
      pv <- rbind(pv, pk)
    }
  }
  rownames(body) <- NULL
  rownames(rows) <- NULL
  rows$block <- seq_len(nrow(rows))
  footnote <- footnote %||% first$footnote
  meta <- first$meta
  meta$pvals <- pv
  meta$regtab_rows <- NULL
  meta$xlsx_footnote <- .tt_compose_xlsx_footnote(tabs, footnote)
  meta$frame <- .tt_stack_frame(tabs)
  meta$sample_accounting <- .tt_sample_bind(
    lapply(tabs, function(t) t$meta[["sample_accounting", exact = TRUE]]),
    prefixes = paste0("table", seq_along(tabs)),
    commands = vapply(tabs, `[[`, "", "command"))
  stored <- list(tables = lapply(tabs, `[[`, "stored"))
  if (!is.null(groups)) names(stored$tables) <- groups
  out <- tt_table(body, header = first$header, rows = rows, cols = first$cols, title = title %||% first$title,
                  footnote = footnote, style = first$style, stored = stored,
                  command = first$command, layout = first$layout, meta = meta)
  validate_tt_table(out)
}

.tt_compose_args <- function(tabs, fn) {
  if (length(tabs) == 1L && is.list(tabs[[1]]) && !inherits(tabs[[1]], "tt_table")) tabs <- tabs[[1]]
  if (length(tabs) < 2L) cli::cli_abort("{.fn {fn}} needs two or more tables.", call = NULL)
  for (k in seq_along(tabs)) {
    if (!inherits(tabs[[k]], "tt_table")) {
      cli::cli_abort("Argument {k} of {.fn {fn}} is not a {.cls tt_table}.", call = NULL)
    }
    validate_tt_table(tabs[[k]])
  }
  tabs
}

.tt_compose_same_command <- function(tabs, fn) {
  cmd <- unique(vapply(tabs, `[[`, "", "command"))
  if (length(cmd) != 1L) {
    cli::cli_abort(c("{.fn {fn}} needs tables from one command (got {.val {cmd}}).",
                     "i" = "The result keeps that command's layout (Excel styling, console listing)."), call = NULL)
  }
  invisible(TRUE)
}

# Union of key sequences in order of first appearance, placed as regtab
# places a later model's rows: a new level of a variable already shown
# right after its predecessor (same `var`), any other new row before the
# next of its successors already in the union (so before the intercept and
# the statistics), or at the end.
.tt_key_union <- function(seqs, vars = lapply(seqs, function(s) rep(NA_character_, length(s)))) {
  out <- seqs[[1]]
  for (k in seq_along(seqs)[-1]) {
    s <- seqs[[k]]
    v <- vars[[k]]
    for (i in seq_along(s)) {
      if (s[i] %in% out) next
      if (i > 1L && !is.na(v[i]) && identical(v[i], v[i - 1L])) {
        pos <- match(s[i - 1L], out)
      } else {
        nxt <- s[seq_along(s) > i & s %in% out]
        pos <- if (length(nxt)) match(nxt[1], out) - 1L else length(out)
      }
      out <- append(out, s[i], after = pos)
    }
  }
  out
}

# The workbook footnote of a composite: its footnote with the inputs' stars
# notes, as regtab builds its own (`regtab.ado:2985-2998`), so the stars
# legend survives in Excel (stack review item 3). NULL when no input shows
# stars: the workbook then writes the footnote.
.tt_compose_xlsx_footnote <- function(tabs, footnote) {
  notes <- unique(unlist(lapply(tabs, .tt_stars_note)))
  if (!length(notes)) return(NULL)
  note <- paste(notes, collapse = "; ")
  fn <- trimws(footnote %||% "")
  if (!nzchar(fn)) note else if (grepl("[.;:!?]$", fn)) paste(fn, note) else paste0(fn, "; ", note)
}

# A regtab table's stars note: what its workbook footnote adds to its
# footnote (R/regtab_layout.R).
.tt_stars_note <- function(t) {
  if (!identical(t$stored$stars, "stars")) return(NULL)
  xf <- t$meta$xlsx_footnote %||% ""
  fn <- trimws(t$footnote %||% "")
  if (!nzchar(xf) || identical(xf, fn)) return(NULL)
  if (!nzchar(fn)) return(xf)
  if (!startsWith(xf, fn)) return(NULL)
  note <- sub("^[;[:space:]]+", "", substring(xf, nchar(fn) + 1L))
  if (nzchar(note)) note else NULL
}

# The frame characteristics of a merged model table (x$meta$frame; comptab
# and as.data.frame() read them): the models of every table in order, their
# labels as the header shows them when the spanners were folded into the
# model-label row (stack review item 4). NULL when a table has none; a
# confidence level only when the tables share it.
.tt_merge_frame <- function(tabs, labels = NULL) {
  frames <- lapply(tabs, function(t) t$meta$frame)
  if (any(vapply(frames, is.null, TRUE))) return(NULL)
  out <- frames[[1]]
  out$n_models <- sum(vapply(frames, function(f) as.numeric(f$n_models %||% 0), 0))
  for (nm in c("model_id", "outcome_id", "effect_scale", "model_label")) {
    if (!is.null(out[[nm]])) out[[nm]] <- unname(unlist(lapply(frames, `[[`, nm)))
  }
  counts <- vapply(frames, function(f) as.numeric(f$n_models %||% 0), 0)
  for (nm in c("effect_additive", "effect_log_scale")) {
    if (any(vapply(frames, function(f) !is.null(f[[nm]]), TRUE))) {
      out[[nm]] <- unname(unlist(.tt_frame_scale_values(frames, nm, counts)))
    }
  }
  if (!is.null(labels) && length(labels) == out$n_models) out$model_label <- labels
  ci <- unique(unlist(lapply(frames, `[[`, "ci_level")))
  if (length(ci) > 1L) out$ci_level <- NA_real_
  out
}

# Optional scale provenance follows model columns when merged and combines
# conservatively by model position when stacked: an unsafe section cannot
# borrow the first section's ratio provenance.
.tt_frame_scale_values <- function(frames, field, counts) {
  lapply(seq_along(frames), function(i) {
    v <- frames[[i]][[field]]
    if (is.null(v)) return(rep(if (field == "effect_additive") NA else "", counts[i]))
    if (length(v) != counts[i]) {
      cli::cli_abort("Table {i}'s {.field {field}} provenance needs one value per model.", call = NULL)
    }
    v
  })
}

.tt_stack_frame <- function(tabs) {
  frames <- lapply(tabs, function(t) t$meta$frame)
  out <- frames[[1]]
  if (is.null(out)) return(NULL)
  counts <- rep(out$n_models, length(frames))
  for (field in c("effect_additive", "effect_log_scale")) {
    if (!any(vapply(frames, function(f) !is.null(f[[field]]), TRUE))) next
    vals <- .tt_frame_scale_values(frames, field, counts)
    out[[field]] <- vapply(seq_len(out$n_models), function(m) {
      v <- vapply(vals, `[`, if (field == "effect_additive") NA else "", m)
      if (field == "effect_additive") {
        if (any(v %in% TRUE)) TRUE else if (all(v %in% FALSE)) FALSE else NA
      } else {
        if (any(v == "log")) "log" else if (any(v == "unknown")) "unknown" else if (any(v == "ratio")) "ratio" else ""
      }
    }, if (field == "effect_additive") NA else "")
  }
  out
}
