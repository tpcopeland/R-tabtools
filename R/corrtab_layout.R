.cor_legend <- function(star) {
  if (!length(star)) return("")
  levels <- rev(star)
  threshold <- trimws(stata_fmt(levels, "%12.0g"))
  threshold <- sub("^[.]", "0.", threshold)
  paste0(strrep("*", seq_along(levels)), " p<", threshold, collapse = ", ")
}

# corrtab.ado:304-342: shape controls text only; missing off-diagonals
# show '.', undefined diagonals remain blank, and stars use strict '<'.
.cor_body <- function(C, P, labels, shape, star, pvalues, format) {
  K <- nrow(C)
  text <- matrix("", K, K)
  shown <- switch(shape, lower = row(C) >= col(C), upper = row(C) <= col(C),
                  full = matrix(TRUE, K, K))
  for (i in seq_len(K)) for (j in seq_len(K)) {
    if (!shown[i, j]) next
    if (i == j) {
      if (is.finite(C[i, j])) text[i, j] <- .tt_format_numeric(1, format)
      next
    }
    coefficient <- if (is.finite(C[i, j])) .tt_format_numeric(C[i, j], format) else "."
    if (startsWith(coefficient, "-") && isTRUE(suppressWarnings(as.numeric(substring(coefficient, 2L))) == 0)) {
      coefficient <- substring(coefficient, 2L)
    }
    p <- P[i, j]
    if (is.finite(p)) {
      if (pvalues) {
        ptext <- if (p < .001) "<0.001" else trimws(stata_fmt(p, "%5.3f"))
        coefficient <- paste0(coefficient, " (", ptext, ")")
      } else coefficient <- paste0(coefficient, strrep("*", sum(p < star)))
    }
    text[i, j] <- coefficient
  }
  list(body = cbind(labels, text), shown = shown)
}

# Exact command-owned worksheet rows: title1, header2, data3...;
# corrtab.ado:441-537. Central dispatch is integrated by root.
.xlsx_layout_corrtab <- function(x) {
  if (length(x$header) != 1L) .tt_layout_needs("corrtab", "exactly one header row")
  nb <- nrow(x$body)
  nc <- ncol(x$body) + 1L
  last <- nb + 2L
  foot <- nzchar(x$footnote)
  nr <- last + as.integer(foot)
  grid <- matrix("", nr, nc)
  grid[1, 1] <- x$title
  grid[2, -1] <- x$header[[1]]$text
  grid[seq_len(nb) + 2L, -1] <- as.matrix(x$body)
  written <- matrix(FALSE, nr, nc)
  written[seq_len(last), ] <- TRUE
  if (foot) { grid[nr, 2L] <- x$footnote; written[nr, 2L] <- TRUE }
  style <- x$style
  R <- new.env(parent = emptyenv())
  R$rules <- list()
  add <- function(...) R$rules[[length(R$rules) + 1L]] <- .rule(...)
  max_label <- max(.blen(x$meta$corr_labels))
  label_width <- max(12, ceiling(max_label * .85) + 2)
  data_width <- max(if (x$meta$corr_pvalues) 14 else 10, min(24, ceiling(max_label * .80) + 2))
  hb <- .border_code(style$hborder)
  vb <- if (identical(style$borderstyle, "medium")) 2 else 1
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = label_width)
  add("width", 1, 1, 3, nc, value = data_width)
  add("font", 1, last, 1, nc, value = style$fontsize)
  add("font", 1, 1, 1, nc, value = style$fontsize + 2)
  add("merge", 1, 1, 1, nc)
  add("bold", 1, 1, 1, 1, code = 1)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("top", 2, 2, 2, nc, code = hb)
  add("bottom", 2, 2, 2, nc, code = hb)
  add("bold", 2, 2, 2, nc, code = 1)
  add("halign", 2, 2, 2, nc, code = 2)
  add("wrap", 2, 2, 2, nc, code = 1)
  add("halign", 3, last, 3, nc, code = 2)
  add("bottom", last, last, 2, nc, code = hb)
  if (!identical(style$borderstyle, "academic")) {
    add("left", 2, last, 2, 2, code = vb)
    add("right", 2, last, nc, nc, code = vb)
    add("right", 2, last, 2, 2, code = vb)
  }
  if (style$headershade) add("fill", 2, 2, 2, nc, color = style$headercolor)
  if (style$zebra && last >= 4L) for (r in seq.int(4L, last, by = 2L)) add("fill", r, r, 2, nc, color = style$zebracolor)
  if (foot) {
    add("merge", nr, nr, 2, nc)
    add("halign", nr, nr, 2, 2, code = 1)
    add("valign", nr, nr, 2, 2, code = 2)
    add("wrap", nr, nr, 2, 2, code = 1)
    add("font", nr, nr, 2, 2, value = max(style$fontsize - 2, 6))
    add("italic", nr, nr, 2, 2, code = 1)
  }
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R$rules)))
}
