# Keyed publication consumer: puttab.ado:313-326, :430-457, :525-530
# and :1526-1564; _tabtools_flatframe.ado:8-26 (Stata 2.5.1).

.puttab_flat_attributes <- function() {
  c("block_id", "source_block_id", "model_names", "full_header",
    "column_model", "source_blocks", "row_types", "model_states", "row_blocks")
}

.puttab_check_flat <- function(x) {
  if (inherits(x, "tt_flat")) {
    .tt_validate_flat(x)
    return(TRUE)
  }
  semantic <- intersect(names(attributes(x)), .puttab_flat_attributes())
  if (is.data.frame(x) && length(semantic)) {
    cli::cli_abort(c("The source has flat metadata but has lost its {.cls tt_flat} class.",
      "i" = "To deliberately discard metadata, use data.frame(lapply(x, identity), check.names = FALSE)."),
      class = "tabtools_error_flat", call = NULL)
  }
  FALSE
}

.puttab_from_flat <- function(x, vars, subset, digits, varlabels, noheader,
                              nformat, panel_spec) {
  .tt_validate_flat(x)
  keys <- c("_order", "_term", "_rowtype", "_block",
            paste0("_state_", seq_along(attr(x, "model_names", exact = TRUE))))
  keys <- names(x)[names(x) %in% keys]
  excluded <- if (is.null(panel_spec)) character() else panel_spec$exclude
  omitted <- if (is.null(vars)) setdiff(keys, excluded) else character()
  selected <- if (is.null(vars)) setdiff(names(x), keys) else vars
  # Use the existing formatter/selection rules without its embedded-header
  # heuristic: every flat record is an identified source body record.
  plain <- x
  class(plain) <- "data.frame"
  src <- .puttab_from_data(plain, selected, subset, digits, varlabels, noheader,
                           noembedheader = TRUE, nformat = nformat, panel_spec = panel_spec)
  selected <- setdiff(selected, excluded)
  projected <- x[src$source_rows, selected, drop = FALSE]
  meta <- lapply(.puttab_flat_attributes(), function(a) attr(projected, a, exact = TRUE))
  names(meta) <- .puttab_flat_attributes()
  meta$selected_columns <- selected
  meta$source_rows <- src$source_rows
  meta$omitted_keys <- omitted
  # Raw keys and order exist only when retained by the caller's projection.
  meta$keys <- lapply(intersect(keys, names(x)), function(a) x[[a]][src$source_rows])
  names(meta$keys) <- intersect(keys, names(x))
  src$flat_source <- meta
  src$notes <- if (length(omitted)) paste0("(puttab: key column(s) ",
    paste(omitted, collapse = " "), " not exported; name them in vars to export them)") else character()
  if (!noheader && varlabels) {
    last <- meta$full_header[[length(meta$full_header)]]$text
    visible <- match(selected, names(meta$column_model))
    src$header[!is.na(visible)] <- last[visible[!is.na(visible)]]
    # Native rowlabel uses a space label (_tabtools_flatframe.ado:67-71).
    # Match its retained identity after projection, preserving other blanks.
    stub <- selected == "rowlabel" & !is.na(visible) &
      is.na(meta$column_model[visible]) & src$header == ""
    src$header[stub] <- " "
  }
  src
}

.puttab_block_spans <- function(src) {
  meta <- src$flat_source
  if (is.null(meta)) return(list())
  cm <- unname(meta$column_model[match(meta$selected_columns, names(meta$column_model))])
  labels <- rep("", length(cm))
  labels[!is.na(cm)] <- meta$model_names[cm[!is.na(cm)]]
  spans <- list()
  j <- 1L
  while (j <= length(cm)) {
    if (is.na(cm[j]) || !nzchar(trimws(labels[j]))) {
      j <- j + 1L
      next
    }
    last <- j
    while (last < length(cm) && !is.na(cm[last + 1L]) && cm[last + 1L] == cm[j]) last <- last + 1L
    # Original model indices distinguish equal labels; call and source IDs
    # qualify those indices across selectors and composed source occurrences.
    spans[[length(spans) + 1L]] <- list(text = labels[j], first = j, last = last,
      model_index = cm[j], block_id = meta$block_id,
      source_block_id = meta$source_block_id,
      source_blocks = meta$source_blocks[, cm[j]])
    j <- last + 1L
  }
  spans
}

.puttab_flat_metadata <- function(src) {
  meta <- src$flat_source
  if (is.null(meta)) return(NULL)
  inserted <- if (is.null(src$panels)) integer() else
    unique(c(src$panels$heading, src$panels$header, src$panels$inline))
  # Source attributes remain row-aligned with selected source records; this
  # map explicitly separates synthetic publication headings from those rows.
  meta$body_rows <- setdiff(seq_len(nrow(src$body)), inserted)
  meta
}
