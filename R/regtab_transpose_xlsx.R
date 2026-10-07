# Each transposed column is its own native regression block; models are rows.
.xlsx_layout_regtab_transpose <- function(x) {
  validate_tt_table(x)
  nb <- nrow(x$body); nc <- ncol(x$body) + 1L; last <- nb + 3L
  footnote <- x$meta$xlsx_footnote %||% x$footnote
  foot <- nzchar(footnote); nr <- last + as.integer(foot)
  grid <- matrix("", nr, nc)
  grid[1L, 1L] <- x$title
  grid[2L, -1L] <- x$header[[1L]]$text
  grid[3L, -1L] <- x$header[[2L]]$text
  grid[seq_len(nb) + 3L, -1L] <- as.matrix(x$body)
  written <- matrix(FALSE, nr, nc); written[seq_len(last), ] <- TRUE
  if (foot) { grid[nr, 2L] <- footnote; written[nr, 2L] <- TRUE }
  style <- x$style; R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  hb <- .border_code(style$hborder); vb <- .border_code(style$vborder)
  academic <- identical(style$borderstyle, "academic")
  add("height", 1, 1, 1, 1, value = 30)
  add("width", 1, 1, 1, 1, value = 1)
  label_width <- x$meta$labelwidth %||% 45
  if (label_width <= 0) label_width <- 45
  add("width", 1, 1, 2, 2, value = min(ceiling(max(.blen(grid[seq_len(last), 2L])) * .95) + 2, label_width))
  lines <- 1L
  for (j in seq.int(3L, nc)) {
    role <- x$cols$role[j - 1L]
    width <- tt_colwidth(grid[seq_len(last), j], scale = 1, pad = -.5,
                         minwidth = 10, headerrow = 2L, exclude = c(x$meta$refcat, x$meta$omitlabel, x$meta$emptylabel))
    add("width", 1, 1, j, j, value = width$width)
    lines <- max(lines, tt_colwidth(hlength = width$hlen, blockwidth = width$width)$hlines)
    # regtab.ado:2867: every transposed column is one model block, including
    # statistics and added text, with its own single-cell header merge.
    add("merge", 2, 2, j, j)
    add("halign", 2, 2, j, j, code = 2)
    add("valign", 2, 2, j, j, code = 2)
    add("bold", 2, 2, j, j, code = 1)
    add("wrap", 2, 2, j, j, code = 1)
    if (!academic) add("right", 2, last, j, j, code = vb)
  }
  if (lines > 1L) add("height", 2, 2, 1, 1, value = lines * 15)
  add("font", 1, last, 1, nc, value = style$fontsize)
  add("font", 1, 1, 1, nc, value = style$fontsize + 2)
  add("merge", 1, 1, 1, nc)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("bold", 1, 1, 1, 1, code = 1)
  if (style$headershade) add("fill", 2, 3, 2, nc, color = style$headercolor)
  add("bold", 3, 3, 2, nc, code = 1)
  add("halign", 3, 3, 2, nc, code = 2)
  add("valign", 3, 3, 2, nc, code = 2)
  add("wrap", 4, last, 2, 2, code = 1)
  add("valign", 4, last, 2, nc, code = 3)
  add("halign", 4, last, 3, nc, code = 2)
  add("top", 2, 2, 2, nc, code = hb)
  add("top", 2, 2, 3, nc, code = hb)
  add("top", 3, 3, 3, nc, code = hb)
  add("bottom", 3, 3, 2, nc, code = hb)
  add("bottom", last, last, 2, nc, code = hb)
  if (!academic) {
    add("left", 2, last, 2, 2, code = vb)
    add("right", 2, last, 2, 2, code = vb)
  }
  if (style$zebra && last >= 5L) for (r in seq.int(5L, last, by = 2L)) add("fill", r, r, 2, nc, color = style$zebracolor)
  if (foot) .xlsx_footnote_rules(add, nr, nc, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}
