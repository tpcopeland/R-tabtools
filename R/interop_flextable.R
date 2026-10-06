#' Convert a tt_table to a flextable
#'
#' Builds a [flextable][flextable::flextable] carrying the tabtools house
#' style, for Word (via `officer` or `flextable::save_as_docx()`), R Markdown,
#' and Quarto. The table is laid out by the same rules as [tt_write_xlsx()],
#' so it matches the workbook Stata's `table1_tc`/`regtab` would write:
#'
#' * font family and size from the table's style (Arial 10 unless set);
#' * both header rows: table1_tc's group labels over the `N=` row, with the
#'   descriptor merged over both rows in the label column and `p-value`,
#'   `Test`, `Statistic`, `SMD` merged vertically; regtab's model labels
#'   merged over each model's columns above the `OR`/`95% CI`/`p-value` row;
#' * the literal label indents (3 spaces in table1_tc, 2 in regtab), written
#'   as no-break spaces so Word and HTML keep them;
#' * regtab's italic Reference/Omitted/Empty rows, merged across their own
#'   model's columns only, and statistics/`addrow` values merged across each
#'   model's columns;
#' * borders per `borderstyle` (`academic`: medium horizontal rules and no
#'   vertical ones), `zebra` and `headershade` fills, bold p-values under
#'   `boldp`, the `highlight` row fill, the SMD flag fill, and the grey font
#'   of `dimnonsig` rows;
#' * the title as the table caption (bold, two points larger) and the
#'   footnote as an italic footer line (two points smaller, at least 6);
#'   regtab's footnote includes the significance-stars legend, as in the
#'   workbook.
#'
#' Column widths follow the workbook's: Excel width `w` becomes `7w + 5`
#' pixels plus the 8 pixels of cell padding the method sets, i.e.
#' `(7w + 13) / 96` inches. Row heights are left to Word.
#'
#' Word keeps a single border for the edge two rows share, taken from the
#' upper cell's bottom, so every rule the workbook draws as the top of a row
#' (under the header, under regtab's model labels, above the statistics,
#' `addrow` and random-effects rows) is also set as the bottom of the row
#' above. HTML output is unchanged.
#'
#' The method is registered for [flextable::as_flextable()] when flextable is
#' loaded, so call it as `flextable::as_flextable(x)`.
#'
#' @param x A `tt_table`, e.g. from [table1_tc()] or [regtab()].
#' @param ... Unused.
#' @return A `flextable` object.
#' @seealso [tt_as_gt()], [tt_write_xlsx()].
#' @examplesIf requireNamespace("flextable", quietly = TRUE)
#' d <- data.frame(arm = rep(c("A", "B"), each = 20),
#'                 age = c(51:70, 55:74),
#'                 sex = factor(rep(c("F", "M"), 20)))
#' tab <- table1_tc(d, by = "arm", vars = c(age = "contn %5.1f", sex = "cat"))
#' ft <- flextable::as_flextable(tab)
#' ft
#' @exportS3Method flextable::as_flextable
as_flextable.tt_table <- function(x, ...) {
  .tt_need("flextable", "as_flextable")
  .tt_need("officer", "as_flextable")
  s <- .tt_render_spec(x)
  nc <- ncol(s$body$text)
  keys <- paste0("c", seq_len(nc))
  df <- as.data.frame(s$body$text, stringsAsFactors = FALSE)
  names(df) <- keys
  ft <- flextable::flextable(df, col_keys = keys)
  mapping <- data.frame(col_keys = keys, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(s$header$text))) mapping[[paste0("h", i)]] <- s$header$text[i, ]
  ft <- flextable::set_header_df(ft, mapping = mapping, key = "col_keys")
  if (nzchar(s$footnote)) {
    ft <- flextable::add_footer_lines(ft, values = .tt_nbsp_indent(.tt_footnote_paragraphs(s$footnote)))
  }
  ft <- flextable::border_remove(ft)
  ft <- flextable::font(ft, fontname = s$font, part = "all")
  for (p in c("header", "body")) ft <- .ft_style_part(ft, s[[p]], p)
  for (k in seq_len(nrow(s$merges))) {
    m <- s$merges[k, ]
    ft <- flextable::merge_at(ft, i = m$i1:m$i2, j = m$j1:m$j2, part = m$part)
  }
  if (nzchar(s$footnote)) {
    ft <- flextable::fontsize(ft, size = s$footnote_size, part = "footer")
    ft <- flextable::italic(ft, italic = TRUE, part = "footer")
    ft <- flextable::bold(ft, bold = FALSE, part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::valign(ft, valign = "center", part = "footer")
    ft <- flextable::bg(ft, bg = "transparent", part = "footer")
    ft <- flextable::color(ft, color = "#000000", part = "footer")
  }
  # Excel-like cell margins: 3 points each side, 1.5 above and below.
  ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 3,
                           padding.right = 3, part = "all")
  ft <- flextable::width(ft, j = seq_len(nc), width = .tt_width_px(s$widths) / 96)
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  if (nzchar(s$title)) {
    cap <- flextable::as_paragraph(flextable::as_chunk(
      s$title, props = officer::fp_text(bold = TRUE, font.size = s$title_size,
                                        font.family = s$font)))
    ft <- flextable::set_caption(ft, caption = cap, align_with_table = FALSE,
                                 fp_p = officer::fp_par(text.align = "left", padding = 3))
  }
  ft
}

# Apply one part's style matrices, one call per rectangle of equal value.
.ft_style_part <- function(ft, sp, part) {
  each <- function(ft, key, fun) {
    for (g in .tt_style_groups(key)) ft <- fun(ft, g$i, g$j, g$value)
    ft
  }
  lg <- function(M) {
    M[] <- ifelse(M, "1", "0")
    M
  }
  ft <- each(ft, lg(sp$bold), function(ft, i, j, v) flextable::bold(ft, i, j, bold = v == "1", part = part))
  ft <- each(ft, lg(sp$italic), function(ft, i, j, v) flextable::italic(ft, i, j, italic = v == "1", part = part))
  size <- sp$size
  size[] <- as.character(size)
  ft <- each(ft, size, function(ft, i, j, v) flextable::fontsize(ft, i, j, size = as.numeric(v), part = part))
  col <- sp$color
  col[is.na(col)] <- "#000000"
  ft <- each(ft, col, function(ft, i, j, v) flextable::color(ft, i, j, color = v, part = part))
  fill <- sp$fill
  fill[is.na(fill)] <- "transparent"
  ft <- each(ft, fill, function(ft, i, j, v) flextable::bg(ft, i, j, bg = v, part = part))
  # Excel's general alignment puts text on the left and at the bottom.
  ha <- sp$halign
  ha[is.na(ha)] <- "left"
  ft <- each(ft, ha, function(ft, i, j, v) flextable::align(ft, i, j, align = v, part = part))
  va <- sp$valign
  va[is.na(va)] <- "bottom"
  ft <- each(ft, va, function(ft, i, j, v) flextable::valign(ft, i, j, valign = v, part = part))
  for (side in c("top", "bottom", "left", "right")) {
    ft <- each(ft, sp[[side]], function(ft, i, j, v) {
      b <- officer::fp_border(color = "#000000", style = "solid", width = .tt_border_pt[[v]])
      args <- list(ft, i = i, j = j, part = part)
      args[[paste0("border.", side)]] <- b
      do.call(flextable::border, args)
    })
  }
  ft
}
