# Native survtab.ado1048-1178, separate from the generic puttab geometry.
# Root owns dispatch/allowlists; this helper owns only the new layout.
.xlsx_layout_survtab <- function(x) {
  if (length(x$header) != 1L || !nrow(x$body) || ncol(x$body) < 2L) {
    .sv_abort("A survival layout requires one header and a nonempty grouped body.")
  }
  K <- ncol(x$body) + 1L
  last <- nrow(x$body) + 2L
  foot <- nzchar(x$footnote)
  total <- last + as.integer(foot)
  grid <- matrix("", total, K)
  grid[1L, 1L] <- x$title
  grid[2:last, -1L] <- .tt_cells(x)
  written <- matrix(FALSE, total, K)
  written[seq_len(last), ] <- TRUE
  if (foot) { grid[total, 2L] <- x$footnote; written[total, 2L] <- TRUE }
  style <- x$style
  rules <- new.env(parent = emptyenv()); rules$items <- list()
  add <- function(...) rules$items[[length(rules$items) + 1L]] <- .rule(...)
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = 22)
  add("width", 1, 1, 3, K, value = 18)
  add("font", 1, last, 1, K, value = style$fontsize)
  add("font", 1, 1, 1, K, value = style$fontsize + 2)
  add("merge", 1, 1, 1, K)
  add("bold", 1, 1, 1, 1, code = 1)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  hb <- .border_code(style$hborder)
  add("top", 2, 2, 2, K, code = hb)
  add("bottom", 2, 2, 2, K, code = hb)
  add("bold", 2, 2, 2, K, code = 1)
  add("halign", 2, 2, 2, K, code = 2)
  if (style$headershade) add("fill", 2, 2, 2, K, color = style$headercolor)
  add("halign", 3, last, 3, K, code = 2)
  add("bottom", last, last, 2, K, code = hb)
  if (style$borderstyle != "academic") {
    vb <- if (style$borderstyle == "medium") 2L else 1L
    add("left", 2, last, 2, 2, code = vb)
    add("right", 2, last, K, K, code = vb)
    add("right", 2, last, 2, 2, code = vb)
  }
  if (style$zebra && last >= 4L) for (row in seq.int(4L, last, by = 2L)) {
    add("fill", row, row, 2, K, color = style$zebracolor)
  }
  lr <- x$meta$survtab_logrank_row
  p <- x$stored$logrank_p
  if (lr > 0L) {
    row <- lr + 2L
    pcol <- which(x$cols$role == "p") + 1L
    if (!is.na(p) && !is.na(style$boldp) && p < style$boldp) {
      if (length(pcol)) add("bold", 3, 3, pcol, pcol, code = 1)
      add("bold", row, row, 2, K, code = 1)
    }
    if (!is.na(p) && !is.na(style$highlight) && p < style$highlight) {
      add("fill", row, row, 2, K, color = style$highlightcolor)
    }
    add("merge", row, row, 2, K)
    add("halign", row, row, 2, 2, code = 1)
    add("valign", row, row, 2, 2, code = 2)
  }
  if (foot) .xlsx_footnote_rules(add, total, K, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, rules$items)))
}
