# Column-width heuristic shared by the xlsx renderer (task 1.6).
# Port of _tabtools_colwidth.ado (v2.1.8): a column is sized from its OWN
# cells, never from a maximum shared with sibling columns.

#' Width of one exported column
#'
#' @param cells Character vector: the column's cells in xlsx-grid row order
#'   (element 1 = worksheet row 1, the title row). `NULL` with `blockwidth`
#'   computes only the header line count.
#' @param firstrow First row measured (Stata `firstrow()`, default 3).
#' @param exclude Cell texts that never set the width (e.g. the reference
#'   label).
#' @param scale,pad Width is `ceil(maxlen * scale) + pad`.
#' @param minwidth,maxwidth Bounds; 0 means unbounded.
#' @param headerrow Row holding the column's own wrapped label (0 = none).
#' @param headerscale Label-length damping before the wrap test.
#' @param headerfloor With `headerrow`, raise the width to
#'   `min(ceil(hlen / 2) + 1, headerfloor)`.
#' @param maxlines Cap on `hlines`.
#' @param hlength Header display width when no column is given.
#' @param blockwidth Width of the merged block the header wraps in.
#' @return list(width, maxlen, hlen, hlines). Lengths are display widths
#'   (Stata `udstrlen`), so "\u00b1" counts one.
#' @keywords internal
#' @noRd
tt_colwidth <- function(cells = NULL, firstrow = 3L, exclude = character(),
                        scale = 0.85, pad = 2, minwidth = 0, maxwidth = 0,
                        headerrow = 0L, headerscale = 0.9, headerfloor = 0,
                        maxlines = 5L, hlength = 0, blockwidth = 0) {
  maxlen <- 0
  width <- 0
  hlen <- hlength
  if (!is.null(cells)) {
    cells[is.na(cells)] <- ""
    idx <- seq_along(cells) >= firstrow & !(cells %in% exclude)
    if (any(idx)) maxlen <- max(.dwidth(cells[idx]))
    width <- ceiling(maxlen * scale) + pad
    if (headerrow > 0 && headerrow <= length(cells)) hlen <- .dwidth(cells[headerrow])
    if (headerfloor > 0 && hlen > 0) {
      floor_w <- min(ceiling(hlen / 2) + 1, headerfloor)
      if (floor_w > width) width <- floor_w
    }
    if (minwidth > 0 && width < minwidth) width <- minwidth
    if (maxwidth > 0 && width > maxwidth) width <- maxwidth
  } else if (blockwidth <= 0) {
    cli::cli_abort("{.fn tt_colwidth} needs {.arg cells} or {.arg blockwidth}.", call = NULL)
  }
  wrap <- if (blockwidth > 0) blockwidth else width
  hlines <- 1L
  if (wrap > 0 && hlen * headerscale > wrap) {
    hlines <- as.integer(min(ceiling(hlen * headerscale / wrap), maxlines))
  }
  list(width = width, maxlen = maxlen, hlen = hlen, hlines = hlines)
}
