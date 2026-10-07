# Editable flat regression/effect output: Stata 2.5.1
# regtab.ado:2194-2262, :2698-2725; effecttab.ado:1663-1675 and
# _tabtools_flatframe.ado:8-26. Analytical identities come from the row
# union and its cells, never from rendered labels or fitted data revisited.

# tempfile() reserves no file and does not consume the R RNG stream.
.tt_flat_id <- function() basename(tempfile("tt-block-"))

.tt_flat_row_types <- function(rows, headings = integer()) {
  nr <- nrow(rows)
  if (!is.numeric(headings) || is.complex(headings) || is.object(headings) ||
      !is.null(dim(headings)) || anyNA(headings) || any(!is.finite(headings)) ||
      any(headings != round(headings) | headings < 1L | headings > nr) || anyDuplicated(headings)) {
    cli::cli_abort("Flat structural heading positions are invalid.",
                   class = "tabtools_error_flat", call = NULL)
  }
  types <- rows$type
  if (!is.null(rows$addrow)) types[rows$addrow %in% TRUE] <- "addrow"
  types[headings] <- "header"
  types
}

.tt_flat_check_cells <- function(states, types, sources, nr, nm) {
  bad <- function(msg) cli::cli_abort(msg, class = "tabtools_error_flat", call = NULL)
  if (!is.character(types) || length(types) != nr || anyNA(types) ||
      any(!types %in% c("var", "level", "ref", "omitted", "empty", "re",
                        "cat_header", "stat", "addrow", "header"))) {
    bad("Flat structural row types are missing or invalid.")
  }
  if (!is.matrix(states) || !is.character(states) ||
      !identical(dim(states), c(nr, nm)) || anyNA(states)) {
    bad("Flat model-state metadata is missing or misaligned.")
  }
  if (!is.matrix(sources) || !is.character(sources) ||
      !identical(dim(sources), c(nr, nm)) || anyNA(sources)) {
    bad("Flat source-block metadata is missing or misaligned.")
  }
  structural <- types %in% c("cat_header", "stat", "addrow", "header")
  if (any(states[structural, , drop = FALSE] != "") ||
      any(!states[!structural, , drop = FALSE] %in%
          c("est", "ref", "omit", "notest", "absent", "empty", "constrained", "masked"))) {
    bad("Flat model states must be canonical analytical states, or blank on structural rows.")
  }
  required <- states != "" & states != "absent"
  if (any(required & !nzchar(sources))) bad("Flat analytical cells have lost their source-block identity.")
  invisible(NULL)
}

.tt_flat_metadata <- function(cells, rows, n_models) {
  nr <- nrow(rows)
  id <- .tt_flat_id()
  states <- matrix("", nr, n_models)
  types <- .tt_flat_row_types(rows)
  structural <- types %in% c("cat_header", "stat", "addrow", "header")
  for (m in seq_len(n_models)) {
    n <- nrow(cells[[m]])
    s <- cells[[m]]$status
    s[is.na(s)] <- "absent"
    s[s == "base"] <- "ref"
    s[s == "cns"] <- "constrained"
    states[seq_len(n), m] <- s
  }
  states[structural, ] <- ""
  list(block_id = id, row_keys = rows$key, row_types = types,
       row_blocks = rep(id, nr), states = states,
       source_blocks = matrix(id, nr, n_models), composite = FALSE)
}

.tt_flat_metadata_check <- function(f, nr, nm, keys, types) {
  bad <- function() cli::cli_abort("Flat source metadata is missing or no longer aligned with the table.",
                                   class = "tabtools_error_flat", call = NULL)
  if (!is.list(f) || !is.character(f$block_id) || length(f$block_id) != 1L ||
      is.na(f$block_id) || !nzchar(f$block_id) ||
      !is.character(f$row_blocks) || length(f$row_blocks) != nr || anyNA(f$row_blocks) ||
      any(!nzchar(f$row_blocks)) || !is.character(keys) || length(keys) != nr || anyNA(keys) ||
      !identical(f$row_keys, keys) || !identical(f$row_types, types)) bad()
  .tt_flat_check_cells(f$states, f$row_types, f$source_blocks, nr, nm)
  if (any(!nzchar(keys) & types != "header")) bad()
  invisible(f)
}

.tt_flat_check_sources <- function(tabs) {
  for (t in tabs) {
    if (is.null(t$meta$flat)) next
    nm <- max(c(0L, t$cols$model), na.rm = TRUE)
    types <- .tt_flat_row_types(t$rows, t$meta$stack_group_rows %||% integer())
    .tt_flat_metadata_check(t$meta$flat, nrow(t$body), nm, t$rows$key, types)
  }
  invisible(NULL)
}

#' Editable body rows with regression and effect identities
#'
#' Returns one row per rendered body record from [regtab()] or [effecttab()],
#' including their [tt_merge()] and [tt_stack()] compositions. Title, header,
#' and footnote rows are omitted. Visible column names are the printed statistic
#' labels (made unique when repeated; leading underscores receive a
#' `"statistic: "` prefix); `rowlabel` keeps the displayed row label.
#' Model labels and the full source headers are preserved separately.
#'
#' @param x A regression or effect `tt_table` carrying its structural metadata.
#' @param keyed Include `_order` (integer display order), `_term` (the existing
#'   raw, equation-aware row key), `_rowtype` (the existing structural row type),
#'   and `_state_<model-index>` for each model. The default is `TRUE`.
#' @details Analytical states come from the model's existing cells: `base`
#'   becomes `ref`, missing model terms become `absent`, and other analytical
#'   states are retained. Heading, statistic and added-text rows have empty
#'   state strings and are identified by `_rowtype`; statistic keys retain
#'   `stat:<item>`. No states are inferred from displayed text.
#'   Group headings flagged by a stacked source have `_rowtype = "header"`.
#'   Analytical states are `est`, `ref`, `omit`, `notest`, `absent`, `empty`,
#'   `constrained` or `masked`; empty strings are reserved for structural rows.
#'
#'   `block_id` identifies this selector call without consuming random numbers.
#'   `source_block_id` identifies the source table. Compositions add `_block`
#'   when keyed, preserving source row identity; `source_blocks` records source
#'   identity for every row/model cell, including side-by-side merges. Reused
#'   tables in a stack receive separate occurrence identities.
#'
#'   Row and column `[` selections preserve and project metadata. A selection
#'   returning a vector follows data-frame dropping rules. Changing visible
#'   cells is allowed; analytical state remains provenance of the source table.
#'   Lost or misaligned metadata raises `tabtools_error_flat` when validated
#'   for model headers. Duplicating keyed row identities is refused.
#'   Structural row types and state provenance for all original models remain
#'   in attributes when their technical columns are omitted or projected away.
#'   Omitted `_term` and `_order` columns cannot be reconstructed from the
#'   remaining cells. To deliberately discard flat metadata, construct a
#'   plain data frame with `data.frame(lapply(flat, identity), check.names = FALSE)`.
#'   Column names must be unique. Underscore-prefixed names are reserved for
#'   `_order`, `_term`, `_rowtype`, `_block` and `_state_<model-index>`;
#'   other reserved names raise `tabtools_error_flat`.
#' @return A `tt_flat` data frame of character publication cells and optional
#'   technical columns. Attributes include `block_id`, `model_names`,
#'   `full_header` (source text and spans), `column_model` (model indices, named
#'   by visible column), `source_block_id`, `source_blocks`, `row_types`, and
#'   `model_states`, and `row_blocks` (source identity for each row, retained
#'   even when `_block` is projected away). Source/state matrices retain all
#'   original model columns;
#'   projected visible and state columns keep their original model indices.
#' @seealso [as.data.frame.tt_table()], [puttab()]
#' @examples
#' tab <- regtab(lm(mpg ~ wt + factor(cyl), mtcars), stats = "n")
#' flat <- tt_flat(tab)
#' flat[flat$`_term` == "wt", , drop = FALSE]
#' flat[[2]][flat$`_term` == "wt"] <- "See text"
#' tt_flat(tab, keyed = FALSE)
#' @export
tt_flat <- function(x, keyed = TRUE) {
  validate_tt_table(x)
  if (!is.logical(keyed) || length(keyed) != 1L || is.na(keyed)) {
    cli::cli_abort("{.arg keyed} must be TRUE or FALSE.", class = "tabtools_error_flat", call = NULL)
  }
  if (!x$command %in% c("regtab", "effecttab")) {
    cli::cli_abort("{.fn tt_flat} requires a regression or effect table.",
                   class = "tabtools_error_flat", call = NULL)
  }
  nm <- max(c(0L, x$cols$model), na.rm = TRUE)
  f <- x$meta$flat
  types <- .tt_flat_row_types(x$rows, x$meta$stack_group_rows %||% integer())
  .tt_flat_metadata_check(f, nrow(x$body), nm, x$rows$key, types)
  if (anyNA(x$rows$key)) {
    cli::cli_abort("Flat output requires the source key of every body row.",
                   class = "tabtools_error_flat", call = NULL)
  }
  out <- x$body
  labels <- x$header[[length(x$header)]]$text
  model_names <- x$meta$frame$model_label
  labels[1L] <- "rowlabel"
  blank <- which(!nzchar(labels) & !is.na(x$cols$model))
  labels[blank] <- model_names[x$cols$model[blank]]
  labels[!nzchar(labels)] <- names(out)[!nzchar(labels)]
  labels[startsWith(labels, "_")] <- paste0("statistic: ", labels[startsWith(labels, "_")])
  names(out) <- make.unique(labels)
  column_model <- stats::setNames(x$cols$model, names(out))
  if (keyed) {
    out[["_order"]] <- seq_len(nrow(out))
    out[["_term"]] <- x$rows$key
    out[["_rowtype"]] <- types
    for (m in seq_len(nm)) out[[paste0("_state_", m)]] <- f$states[, m]
    if (isTRUE(f$composite)) out[["_block"]] <- f$row_blocks
  }
  attr(out, "block_id") <- .tt_flat_id()
  attr(out, "source_block_id") <- f$block_id
  attr(out, "model_names") <- model_names
  attr(out, "full_header") <- x$header
  attr(out, "column_model") <- column_model
  attr(out, "source_blocks") <- f$source_blocks
  attr(out, "row_types") <- types
  attr(out, "model_states") <- f$states
  attr(out, "row_blocks") <- f$row_blocks
  attr(out, "sample_accounting") <- x$meta$sample_accounting
  class(out) <- c("tt_flat", "data.frame")
  .tt_validate_flat(out)
  out
}

.tt_validate_flat <- function(x) {
  bad <- function(msg) cli::cli_abort(msg, class = "tabtools_error_flat", call = NULL)
  if (!inherits(x, "tt_flat") || !is.data.frame(x)) bad("Expected a {.cls tt_flat} data frame.")
  column_names <- names(x)
  if (anyDuplicated(column_names)) bad("Flat column names must be unique.")
  mn <- attr(x, "model_names", exact = TRUE)
  reserved <- column_names[startsWith(column_names, "_")]
  known <- c("_order", "_term", "_rowtype", "_block",
             if (length(mn)) paste0("_state_", seq_along(mn)))
  if (any(!reserved %in% known)) bad("Flat technical column names are outside the reserved schema.")
  for (a in c("block_id", "source_block_id")) {
    v <- attr(x, a, exact = TRUE)
    if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) bad("Invalid flat block identity.")
  }
  cm <- attr(x, "column_model", exact = TRUE)
  if (!is.numeric(cm) || is.complex(cm) || is.object(cm) || !is.null(dim(cm)) ||
      is.null(names(cm)) || anyDuplicated(names(cm)) ||
      !all(names(cm) %in% names(x)) || !is.character(mn) || anyNA(mn) ||
      any(!is.na(cm) & (!is.finite(cm) | cm != round(cm) | cm < 1L | cm > length(mn)))) {
    bad("Flat column/model metadata is missing or misaligned.")
  }
  # Every nontechnical column belongs to the projected header metadata.
  if (!identical(names(cm), names(x)[!startsWith(names(x), "_")])) bad("Flat visible columns no longer match their headers.")
  h <- attr(x, "full_header", exact = TRUE)
  if (!is.list(h) || !length(h)) bad("Flat full-header metadata is missing.")
  for (r in h) {
    if (!is.list(r) || !is.character(r$text) || length(r$text) != length(cm) || anyNA(r$text)) bad("Flat full-header metadata is misaligned.")
    s <- r$spans
    if (!is.data.frame(s) || !all(c("from", "to") %in% names(s)) ||
        !is.numeric(s$from) || !is.numeric(s$to) || is.complex(s$from) || is.complex(s$to) ||
        is.object(s$from) || is.object(s$to) || !is.null(dim(s$from)) || !is.null(dim(s$to)) ||
        anyNA(s$from) || anyNA(s$to) || any(!is.finite(s$from)) || any(!is.finite(s$to)) ||
        any(s$from < 1L | s$to > length(cm) | s$from > s$to | s$from != round(s$from) | s$to != round(s$to))) {
      bad("Flat full-header spans are invalid.")
    }
  }
  sources <- attr(x, "source_blocks", exact = TRUE)
  types <- attr(x, "row_types", exact = TRUE)
  states <- attr(x, "model_states", exact = TRUE)
  .tt_flat_check_cells(states, types, sources, nrow(x), length(mn))
  blocks <- attr(x, "row_blocks", exact = TRUE)
  if (!is.character(blocks) || length(blocks) != nrow(x) || anyNA(blocks) || any(!nzchar(blocks))) {
    bad("Flat row-block metadata is missing or invalid.")
  }
  for (v in intersect(c("_term", "_rowtype", "_block"), names(x))) {
    if (!is.character(x[[v]]) || anyNA(x[[v]])) bad("Flat row identity columns must be nonmissing character strings.")
  }
  if ("_rowtype" %in% names(x) && !identical(x[["_rowtype"]], types)) bad("Flat row types no longer match their structural provenance.")
  if ("_block" %in% names(x) && !identical(x[["_block"]], blocks)) bad("Flat keyed blocks no longer match their source identities.")
  if ("_term" %in% names(x)) {
    if (any(!nzchar(x[["_term"]]) & types != "header")) bad("Flat analytical and statistic terms must not be empty.")
    id <- data.frame(block = blocks, term = x[["_term"]])
    if (anyDuplicated(id)) bad("Flat keyed terms must be unique within each source block.")
  }
  if ("_order" %in% names(x)) {
    if (!is.integer(x[["_order"]]) || anyNA(x[["_order"]]) || any(x[["_order"]] < 1L)) bad("Flat _order must contain positive integers.")
    id <- data.frame(block = blocks, order = x[["_order"]])
    if (anyDuplicated(id)) bad("Flat keyed row identities must be unique.")
  }
  state_cols <- names(x)[startsWith(names(x), "_state_")]
  for (v in state_cols) {
    m <- suppressWarnings(as.integer(sub("^_state_", "", v)))
    if (is.na(m) || m < 1L || m > length(mn) || v != paste0("_state_", m) ||
        !is.character(x[[v]]) || anyNA(x[[v]]) || !identical(x[[v]], states[, m])) {
      bad("Flat model state columns are invalid.")
    }
  }
  invisible(x)
}

#' @rdname tt_flat
#' @param i,j Row and column selectors, as for a data frame.
#' @param drop Whether to drop to a vector, as for a data frame.
#' @param ... Additional arguments passed to data-frame subsetting.
#' @export
`[.tt_flat` <- function(x, i, j, drop = FALSE, ...) {
  .tt_validate_flat(x)
  one_index <- (nargs() - (!missing(drop))) < 3L
  plain <- x
  class(plain) <- "data.frame"
  # Subset marker rows/columns with the same data-frame rules as the cells.
  marker <- plain
  marker[] <- rep(list(seq_len(nrow(x))), ncol(x))
  out <- if (missing(i) && missing(j)) plain[, , drop = drop, ...] else if (missing(j)) {
    if (one_index) plain[i] else plain[i, , drop = drop, ...]
  } else if (missing(i)) plain[, j, drop = drop, ...] else plain[i, j, drop = drop, ...]
  if (!is.data.frame(out)) return(out)
  marked <- if (missing(i) && missing(j)) marker[, , drop = FALSE] else if (missing(j)) {
    if (one_index) marker[i] else marker[i, , drop = FALSE]
  } else if (missing(i)) marker[, j, drop = FALSE] else marker[i, j, drop = FALSE]
  # With zero columns, row names still encode the selected marker rows.
  row_idx <- if (ncol(marked)) marked[[1L]] else match(rownames(marked), rownames(marker))
  for (a in c("block_id", "source_block_id", "model_names", "sample_accounting")) {
    attr(out, a) <- attr(x, a, exact = TRUE)
  }
  cm <- attr(x, "column_model", exact = TRUE)
  keep <- match(names(out)[names(out) %in% names(cm)], names(cm))
  attr(out, "column_model") <- cm[keep]
  attr(out, "full_header") <- lapply(attr(x, "full_header", exact = TRUE), function(h) {
    spans <- lapply(seq_len(nrow(h$spans)), function(k) {
      at <- which(keep >= h$spans$from[k] & keep <= h$spans$to[k])
      if (!length(at)) return(NULL)
      runs <- split(at, cumsum(c(TRUE, diff(at) != 1L)))
      do.call(rbind, lapply(runs, function(a) data.frame(from = min(a), to = max(a))))
    })
    h$text <- h$text[keep]
    h$spans <- if (length(Filter(Negate(is.null), spans))) do.call(rbind, spans) else data.frame(from = integer(), to = integer())
    h
  })
  attr(out, "source_blocks") <- attr(x, "source_blocks", exact = TRUE)[row_idx, , drop = FALSE]
  attr(out, "row_types") <- attr(x, "row_types", exact = TRUE)[row_idx]
  attr(out, "model_states") <- attr(x, "model_states", exact = TRUE)[row_idx, , drop = FALSE]
  attr(out, "row_blocks") <- attr(x, "row_blocks", exact = TRUE)[row_idx]
  class(out) <- c("tt_flat", "data.frame")
  .tt_validate_flat(out)
  out
}

# Composition must move the existing arrays with rows and models. Scope each
# source occurrence as well as retaining its original identity, so even a
# repeated source table cannot fuse two blocks.
.tt_flat_merge_metadata <- function(tabs, keys, rows, headings, original_keys) {
  if (any(vapply(tabs, function(t) is.null(t$meta$flat), TRUE))) return(NULL)
  id <- .tt_flat_id()
  states <- sources <- NULL
  row_blocks <- rep("", length(keys))
  for (k in seq_along(tabs)) {
    t <- tabs[[k]]
    f <- t$meta$flat
    nm <- max(c(0L, t$cols$model), na.rm = TRUE)
    types <- .tt_flat_row_types(t$rows, t$meta$stack_group_rows %||% integer())
    .tt_flat_metadata_check(f, nrow(t$body), nm, original_keys[[k]], types)
    at <- match(keys, t$rows$key)
    s <- f$states[at, , drop = FALSE]
    s[is.na(s)] <- "absent"
    states <- cbind(states, s)
    origin <- f$source_blocks[at, , drop = FALSE]
    present <- !is.na(origin) & nzchar(origin)
    origin[present] <- paste0(id, "/", k, "/", origin[present])
    origin[is.na(origin)] <- ""
    sources <- cbind(sources, origin)
    first <- !nzchar(row_blocks) & !is.na(at)
    row_blocks[first] <- paste0(id, "/", k, "/", f$row_blocks[at[first]])
  }
  types <- .tt_flat_row_types(rows, headings)
  structural <- types %in% c("cat_header", "stat", "addrow", "header")
  states[structural, ] <- ""
  list(block_id = id, row_keys = rows$key, row_types = types,
       row_blocks = row_blocks, states = states,
       source_blocks = sources, composite = TRUE)
}

.tt_flat_stack_metadata <- function(tabs, groups) {
  if (any(vapply(tabs, function(t) is.null(t$meta$flat), TRUE))) return(NULL)
  id <- .tt_flat_id()
  states <- sources <- NULL
  row_blocks <- character()
  row_types <- character()
  for (k in seq_along(tabs)) {
    t <- tabs[[k]]
    f <- t$meta$flat
    nm <- max(c(0L, t$cols$model), na.rm = TRUE)
    types <- .tt_flat_row_types(t$rows, t$meta$stack_group_rows %||% integer())
    .tt_flat_metadata_check(f, nrow(t$body), nm, t$rows$key, types)
    s <- f$states
    origin <- f$source_blocks
    present <- nzchar(origin)
    origin[present] <- paste0(id, "/", k, "/", origin[present])
    blocks <- paste0(id, "/", k, "/", f$row_blocks)
    if (!is.null(groups)) {
      s <- rbind(rep("", nm), s)
      origin <- rbind(rep("", nm), origin)
      blocks <- c(paste0(id, "/", k, "/heading"), blocks)
      types <- c("header", types)
    }
    states <- rbind(states, s)
    sources <- rbind(sources, origin)
    row_blocks <- c(row_blocks, blocks)
    row_types <- c(row_types, types)
  }
  row_keys <- unlist(lapply(seq_along(tabs), function(k) {
    c(if (!is.null(groups)) .tt_group_keys(groups)[k], tabs[[k]]$rows$key)
  }), use.names = FALSE)
  list(block_id = id, row_keys = row_keys, row_types = row_types,
       row_blocks = row_blocks, states = states,
       source_blocks = sources, composite = TRUE)
}
