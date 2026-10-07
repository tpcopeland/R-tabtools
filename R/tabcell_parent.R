.tc_parent_metadata <- function(src) {
  leaf <- src$cell_provenance
  if (is.null(leaf)) return(NULL)
  if (identical(src$source, "table")) return(leaf)
  inserted <- if (is.null(src$panels)) integer() else
    unique(c(src$panels$heading, src$panels$header, src$panels$inline))
  rows <- setdiff(seq_len(nrow(src$body)), inserted)
  lapply(leaf, function(item) {
    if (length(rows) != nrow(item$provenance$rows)) {
      .tc_abort("Parent cell provenance does not match its data rows.", "provenance")
    }
    item$body_rows <- rows
    item
  })
}

.tc_stack_metadata <- function(tabs, groups) {
  out <- list()
  offset <- 0L
  for (k in seq_along(tabs)) {
    leaf <- tabs[[k]]$meta$cell_provenance
    if (!is.null(leaf)) {
      for (item in leaf) {
        item$body_rows <- item$body_rows + offset + as.integer(!is.null(groups))
        # Composing a source twice creates two occurrences, while retaining
        # the original cell source ID and row identity separately.
        item$parent_occurrence <- k
        out[[length(out) + 1L]] <- item
      }
    }
    offset <- offset + nrow(tabs[[k]]$body) + as.integer(!is.null(groups))
  }
  if (length(out)) out else NULL
}

.tc_merge_metadata <- function(tabs, keys) {
  out <- list()
  offset <- 0L
  for (k in seq_along(tabs)) {
    table <- tabs[[k]]
    leaf <- table$meta$cell_provenance
    if (!is.null(leaf)) {
      for (item in leaf) {
        if (item$body_column == 1L) {
          # Each union label is published by the first source containing
          # its key. Later sources can own labels for their unique rows.
          earlier <- unlist(lapply(tabs[seq_len(k - 1L)], function(t) t$rows$key),
                            use.names = FALSE)
          keep <- !table$rows$key[item$body_rows] %in% earlier
          if (!any(keep)) next
          cells <- structure(item$provenance$text, provenance = item$provenance,
                             class = c("tt_cell", "character"))
          item$provenance <- attr(cells[keep], "provenance", exact = TRUE)
          item$body_rows <- item$body_rows[keep]
          if (!is.null(item$source_rows)) item$source_rows <- item$source_rows[keep]
        } else {
          item$body_column <- item$body_column + offset
        }
        item$body_rows <- match(table$rows$key[item$body_rows], keys)
        if (anyNA(item$body_rows)) .tc_abort("Merged cell rows are absent from the union.", "provenance")
        item$parent_occurrence <- k
        out[[length(out) + 1L]] <- item
      }
    }
    offset <- offset + ncol(table$body) - 1L
  }
  if (length(out)) out else NULL
}
