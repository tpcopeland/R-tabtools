#' Convert a table to a gt table
#'
#' `tt_as_gt()` builds a [gt][gt::gt] table carrying the tabtools house style,
#' for HTML, Quarto, and R Markdown. The table follows the rules of
#' [tt_write_xlsx()]: font family and size, both header rows, the label
#' indents (as no-break spaces), italic Reference/Omitted/Empty cells,
#' borders per `borderstyle` (`academic`: medium horizontal rules and no
#' vertical ones), `zebra`/`headershade` fills, `boldp`, `highlight`, the SMD
#' flag fill, `dimnonsig` grey, the title as the caption (bold, left-aligned,
#' two points larger), and the footnote as an italic source note (two
#' points smaller). Column widths follow the workbook's: Excel width `w`
#' becomes `7w + 5` pixels plus the 8 pixels of cell padding.
#'
#' The house style's CSS (the caption, and the rule under regtab's model
#' labels, which gt's own `.gt_spanner_row` style would hide) is scoped to
#' the table's HTML id, `tabtools-gt-1`, `tabtools-gt-2`, ... in the order
#' the tables are built in the session.
#'
#' The two header rows map onto gt's column labels and spanners: the lower
#' row (table1_tc's `N=` cells, regtab's `OR`/`95% CI`/`p-value`) becomes the
#' column labels, and the upper row (group labels, merged model labels)
#' becomes spanners. Cells merged over both rows (table1_tc's descriptor,
#' `p-value`, `SMD`) are column labels without a spanner, which gt draws
#' over both rows.
#'
#' gt cannot merge cells in the body, so regtab's Reference/Omitted/Empty
#' text and its statistics/`addrow` values stay in the first column of their
#' model's block (italic and centred for the reference labels) instead of
#' spanning the block as they do in the workbook and in
#' [flextable::as_flextable()]. These cells do not wrap and are not clipped:
#' in a column narrower than the text (a `labelwidth`-narrowed table),
#' "Reference" runs on over the block's empty CI and p-value cells, so it
#' is no longer centred on the block. Use the flextable method when that
#' matters.
#'
#' gt has no generic for this conversion, and gtsummary's `as_gt()` is not
#' one either, so the function carries the package's `tt_` prefix: it never
#' masks `gtsummary::as_gt()`, whichever package is attached first (for a
#' gtsummary table, call `gtsummary::as_gt()`).
#'
#' @param x A `tt_table`, e.g. from [table1_tc()] or [regtab()].
#' @return A `gt_tbl` object.
#' @seealso [as_flextable.tt_table()], [tt_write_xlsx()].
#' @examplesIf requireNamespace("gt", quietly = TRUE)
#' d <- data.frame(arm = rep(c("A", "B"), each = 20),
#'                 age = c(51:70, 55:74),
#'                 sex = factor(rep(c("F", "M"), 20)))
#' tab <- table1_tc(d, by = "arm", vars = c(age = "contn %5.1f", sex = "cat"))
#' tt_as_gt(tab)
#' @export
tt_as_gt <- function(x) {
  if (!inherits(x, "tt_table")) {
    cli::cli_abort(c(
      "{.arg x} must be a {.cls tt_table} (from {.fn table1_tc} or {.fn regtab}), not a {.cls {class(x)[1]}} object.",
      "i" = if (inherits(x, "gtsummary")) "For a gtsummary table, use {.fn gtsummary::as_gt}."
    ), call = NULL)
  }
  .tt_need("gt", "tt_as_gt")
  s <- .tt_render_spec(x, merged = FALSE)
  nc <- ncol(s$body$text)
  keys <- paste0("c", seq_len(nc))
  df <- as.data.frame(s$body$text, stringsAsFactors = FALSE)
  names(df) <- keys
  id <- .gt_next_id()
  g <- gt::gt(df, id = id)
  h <- s$header
  nh <- nrow(h$text)
  mg <- s$merges[s$merges$part == "header", , drop = FALSE]
  # Which header cell each column's label comes from, and the spanners.
  lab_row <- rep(nh, nc)
  vmerged <- integer()
  spanners <- list()
  if (nh >= 2L) {
    v <- mg[mg$i1 == 1L & mg$i2 == nh & mg$j1 == mg$j2, , drop = FALSE]
    vmerged <- v$j1
    lab_row[vmerged] <- 1L
    hz <- mg[mg$i1 == 1L & mg$i2 == 1L, , drop = FALSE]
    covered <- integer()
    for (k in seq_len(nrow(hz))) {
      spanners[[length(spanners) + 1L]] <- list(j = hz$j1[k]:hz$j2[k], from = hz$j1[k])
      covered <- c(covered, hz$j1[k]:hz$j2[k])
    }
    for (j in setdiff(seq_len(nc), c(vmerged, covered))) {
      if (nzchar(trimws(h$text[1, j]))) spanners[[length(spanners) + 1L]] <- list(j = j, from = j)
    }
  }
  labels <- h$text[cbind(lab_row, seq_len(nc))]
  g <- gt::cols_label(g, .list = stats::setNames(as.list(labels), keys))
  for (k in seq_along(spanners)) {
    sp <- spanners[[k]]
    g <- gt::tab_spanner(g, label = h$text[1, sp$from], columns = keys[sp$j],
                         id = paste0("tt_span_", k))
  }
  g <- gt::tab_options(
    g,
    table.font.names = c(s$font, "sans-serif"),
    table.font.size = paste0(s$fontsize, "pt"),
    table.border.top.style = "none", table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    column_labels.border.top.style = "none", column_labels.border.bottom.style = "none",
    column_labels.border.lr.style = "none", column_labels.vlines.style = "none",
    table_body.border.top.style = "none", table_body.border.bottom.style = "none",
    table_body.hlines.style = "none", table_body.vlines.style = "none",
    source_notes.border.bottom.style = "none", source_notes.border.lr.style = "none",
    column_labels.font.weight = "normal",
    data_row.padding = gt::px(3), data_row.padding.horizontal = gt::px(4),
    column_labels.padding = gt::px(3), column_labels.padding.horizontal = gt::px(4)
  )
  g <- gt::cols_width(g, .list = lapply(seq_len(nc), function(j) {
    stats::as.formula(paste0(keys[j], " ~ gt::px(", .tt_width_px(s$widths[j]), ")"))
  }))
  # Body cells.
  body_key <- .gt_style_key(s$body)
  for (grp in .tt_style_groups(body_key)) {
    g <- gt::tab_style(g, style = .gt_styles(grp$value),
                       locations = gt::cells_body(columns = keys[grp$j], rows = grp$i))
  }
  # Column labels: the cell each label came from; a label without a spanner
  # stands for both header rows, so it takes the upper row's top border.
  has_span <- rep(FALSE, nc)
  for (sp in spanners) has_span[sp$j] <- TRUE
  lab_key <- .gt_style_key(h, rows = lab_row)
  if (nh >= 2L) {
    top <- ifelse(has_span, h$top[cbind(lab_row, seq_len(nc))], h$top[1, ])
    top[has_span] <- ifelse(is.na(top[has_span]), h$bottom[1, has_span], top[has_span])
    lab_key <- .gt_style_key(h, rows = lab_row, top = top)
  }
  for (grp in .tt_style_groups(matrix(lab_key, 1L))) {
    g <- gt::tab_style(g, style = .gt_styles(grp$value),
                       locations = gt::cells_column_labels(columns = keys[grp$j]))
  }
  for (k in seq_along(spanners)) {
    sp <- spanners[[k]]
    # The spanner shows the anchor of the workbook's merged range, so it takes
    # the right border of the range's last column.
    key <- .gt_style_key(h, rows = 1L, cols = sp$from, right = h$right[1L, max(sp$j)])
    g <- gt::tab_style(g, style = .gt_styles(key),
                       locations = gt::cells_column_spanners(spanners = paste0("tt_span_", k)))
  }
  # Merged body cells (Reference/Omitted/Empty, stats and addrow values)
  # stay in the block's first column; let their text run over the block's
  # empty cells instead of being clipped by gt's `overflow-x: hidden`.
  bm <- s$merges[s$merges$part == "body" & s$merges$j2 > s$merges$j1, , drop = FALSE]
  for (j in unique(bm$j1)) {
    g <- gt::tab_style(g, style = list(gt::cell_text(whitespace = "nowrap"), gt::css(`overflow-x` = "visible")),
                       locations = gt::cells_body(columns = keys[j], rows = bm$i1[bm$j1 == j]))
  }
  g <- gt::opt_css(g, css = paste(
    # gt hides the bottom edge of the spanner row (`border-bottom-style:
    # hidden`, which wins every border conflict) and with it the rule under
    # regtab's model labels, which the column labels carry as their top.
    sprintf("#%s .gt_spanner_row { border-bottom-style: none; }", id),
    # The workbook's title row: left-aligned, bold, two points larger.
    sprintf("#%s .gt_caption { text-align: left; font-weight: bold; font-size: %spt; }", id, s$title_size),
    sep = "\n"))
  if (nzchar(s$title)) {
    # Bold in the markdown too, for renderers that take the caption without
    # the table's CSS (Quarto cross-references); the text itself is escaped.
    # (`&` too: "&lt;5" is text, not an entity; codex audit F09. `~` too:
    # "~~x~~" is not strikethrough. "$" becomes "&#36;", since gt looks for
    # "$...$" equations before Markdown escapes apply; audit A02.)
    esc <- gsub("([\\\\`*_{}\\[\\]()#+.!|<>&~-])", "\\\\\\1", s$title, perl = TRUE)
    esc <- gsub("$", "&#36;", esc, fixed = TRUE)
    g <- gt::tab_caption(g, caption = gt::md(paste0("**", esc, "**")))
  }
  if (nzchar(s$footnote)) {
    g <- gt::tab_source_note(g, source_note = s$footnote)
    g <- gt::tab_style(g, style = gt::cell_text(style = "italic", size = paste0(s$footnote_size, "pt"),
                                                 align = "left"),
                       locations = gt::cells_source_notes())
  }
  g
}

# One string per cell encoding every style attribute gt sets; `rows`/`cols`
# pick cells (one per column when `rows` has one entry per column).
.gt_style_key <- function(sp, rows = NULL, cols = NULL, top = NULL, right = NULL) {
  if (is.null(rows)) {
    idx <- which(matrix(TRUE, nrow(sp$text), ncol(sp$text)), arr.ind = TRUE)
  } else {
    cols <- cols %||% seq_len(ncol(sp$text))
    if (length(rows) == 1L) rows <- rep(rows, length(cols))
    idx <- cbind(rows, cols)
  }
  get <- function(M) M[idx]
  t <- if (is.null(top)) get(sp$top) else top
  rt <- if (is.null(right)) get(sp$right) else right
  ha <- get(sp$halign)
  ha[is.na(ha)] <- "left"
  va <- get(sp$valign)
  va[is.na(va)] <- "bottom"
  key <- paste(get(sp$bold), get(sp$italic), get(sp$size), get(sp$color), get(sp$fill), ha, va,
               t, get(sp$bottom), get(sp$left), rt, sep = "|")
  if (is.null(rows)) matrix(key, nrow(sp$text), ncol(sp$text)) else key
}

.gt_styles <- function(key) {
  f <- strsplit(key, "|", fixed = TRUE)[[1]]
  na <- function(v) if (identical(v, "NA")) NA_character_ else v
  out <- list(gt::cell_text(
    weight = if (f[1] == "TRUE") "bold" else "normal",
    style = if (f[2] == "TRUE") "italic" else "normal",
    size = paste0(f[3], "pt"),
    color = na(f[4]) %|na|% "#000000",
    align = f[6], v_align = if (f[7] == "center") "middle" else f[7],
    whitespace = "pre-wrap"
  ))
  if (!is.na(na(f[5]))) out[[length(out) + 1L]] <- gt::cell_fill(color = f[5])
  sides <- c("top", "bottom", "left", "right")
  for (k in seq_along(sides)) {
    # A side without a rule is left alone: gt's own rules are switched off,
    # and a "hidden" style would also erase the neighbouring cell's rule.
    b <- na(f[7L + k])
    if (is.na(b)) next
    out[[length(out) + 1L]] <- gt::cell_borders(sides = sides[k], color = "#000000", style = "solid",
                                                weight = gt::px(round(.tt_border_pt[[b]] * 4 / 3)))
  }
  out
}

# HTML ids for the tables tt_as_gt() builds: the house-style CSS is scoped to
# the table (gt's own CSS is too), and a session counter keeps the ids
# unique within a document without drawing from the random number stream.
.gt_ids <- new.env(parent = emptyenv())
.gt_next_id <- function() {
  n <- (.gt_ids$n %||% 0L) + 1L
  .gt_ids$n <- n
  sprintf("tabtools-gt-%d", n)
}
