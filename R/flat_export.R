# A plain editing/export frame deliberately carries no fitted-model row keys.
.tt_unkeyed_export <- function(x) {
  out <- x$body
  if (x$command %in% c("comptab", "hrcomptab")) {
    # Pinned _comptab_flatten retains full block/column headers on the
    # exported variables, which puttab(varlabels=TRUE) consumes.
    header <- x$header[[length(x$header)]]$text
    blocks <- .tt_export_blocks(x$header, ncol(out))
    names(out) <- c("rowlabel", paste0("c", seq_len(ncol(out) - 1L)))
    attr(out[[1L]], "label") <- ""
    for (j in seq_len(ncol(out))[-1L]) {
      block <- blocks[j]
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

# Outcome/model block label of each column, from the top header row's own
# spans and labels (the published block identity). Model keys are not used:
# plain exports never carry them, and outcome blocks need not have one.
.tt_export_blocks <- function(header, n) {
  blocks <- rep("", n)
  if (length(header) < 2L) return(blocks)
  top <- header[[1L]]
  text <- trimws(top$text %||% rep("", n), whitespace = "[ ]")
  spans <- top$spans
  starts <- which(nzchar(text))
  ends <- integer()
  if (is.data.frame(spans) && nrow(spans)) {
    starts <- union(starts, spans$from)
    ends <- spans$to
  }
  # A column after a spanned block that starts nothing new has no block.
  starts <- sort(union(setdiff(starts, 1L), setdiff(ends + 1L, c(starts, n + 1L))))
  block <- ""
  for (j in seq_len(n)[-1L]) {
    if (j %in% starts) block <- text[j]
    blocks[j] <- block
  }
  blocks
}
