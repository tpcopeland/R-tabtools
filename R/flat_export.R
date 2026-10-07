# A plain editing/export frame deliberately carries no fitted-model row keys.
.tt_unkeyed_export <- function(x) {
  out <- x$body
  if (x$command %in% c("comptab", "hrcomptab")) {
    # Pinned _comptab_flatten retains full block/column headers on the
    # exported variables, which puttab(varlabels=TRUE) consumes.
    header <- x$header[[length(x$header)]]$text
    block <- ""
    starts <- which(!duplicated(x$cols$model[-1L])) + 1L
    names(out) <- c("rowlabel", paste0("c", seq_len(ncol(out) - 1L)))
    attr(out[[1L]], "label") <- ""
    for (j in seq_len(ncol(out))[-1L]) {
      if (j %in% starts) block <- trimws(x$header[[1L]]$text[j], whitespace = "[ ]")
      leaf <- trimws(header[j], whitespace = "[ ]")
      full <- if (nzchar(block) && nzchar(leaf)) paste0(block, ", ", leaf) else paste0(block, leaf)
      attr(out[[j]], "label") <- full
      attr(out[[j]], "tabtools_header") <- full
    }
  }
  attr(out, "header") <- x$header
  attr(out, "command") <- x$command
  attr(out, "frame") <- x$meta$frame
  attr(out, "sample_accounting") <- x$meta[["sample_accounting", exact = TRUE]]
  attr(out, "composition_export") <- TRUE
  out
}
