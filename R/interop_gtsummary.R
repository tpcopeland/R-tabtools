#' Convert a table to a gtsummary table
#'
#' `tt_as_gtsummary()` builds a gtsummary table from a
#' `tt_table`'s cells, so that a tabtools table works with gtsummary's
#' tools: [gtsummary::tbl_stack()], [gtsummary::as_gt()],
#' [gtsummary::as_flex_table()], [gtsummary::as_kable()], and the
#' `modify_*()` functions. The cells are tabtools' formatted text, never
#' reformatted by gtsummary. The table's lowest header row gives the
#' column labels and the rows above it the spanning headers (regtab's model
#' labels over each model's columns; table1_tc's group labels over their
#' spans), level rows are indented, Reference/Omitted/Empty cells are
#' italic, the title is the caption and the footnote a source note.
#' Header, caption and source-note text is plain text (gtsummary's
#' `text_interpret = "none"`), shown as typed by `as_gt()` and
#' `as_flex_table()`; `as_kable()` writes it into Markdown unescaped, so
#' `*`, `_` or a `$` pair there is read as Markdown or math.
#'
#' The table body keeps each row's identity in hidden columns:
#' `variable` (`rows$var`), `row_type` (`"label"` for a variable, header or
#' statistics row, `"level"` for an indented one), `key` (`rows$key`) and
#' `tt_row_id` (the input row number, used for cell styling),
#' so `tbl_stack()` and `modify_table_body()` can use them. Excel styling
#' (borders, fills, widths) is not carried over: use [tt_write_xlsx()],
#' [tt_as_gt()] or [flextable::as_flextable()] for the house style.
#'
#' gtsummary's `as_gtsummary()` is not a generic, so the function carries
#' the package's `tt_` prefix, as [tt_as_gt()] does. It needs gtsummary
#' 2.3.0 or later.
#' @param x A `tt_table`.
#' @return A `gtsummary` object.
#' @seealso [tt_as_gt()], [flextable::as_flextable()]
#' @examplesIf requireNamespace("gtsummary", quietly = TRUE)
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' tab <- regtab(glm(am ~ wt + cyl, family = binomial, data = d))
#' gts <- tt_as_gtsummary(tab)
#' gtsummary::as_kable(gts)
#' @export
tt_as_gtsummary <- function(x) {
  if (!inherits(x, "tt_table")) {
    cli::cli_abort("{.arg x} must be a {.cls tt_table}, not a {.cls {class(x)[1]}} object.", call = NULL)
  }
  .tt_need("gtsummary", "tt_as_gtsummary", version = "2.3.0")
  validate_tt_table(x)
  nc <- ncol(x$body)
  # paste0() of a zero-length vector is "col_", not character(0): a
  # label-only table has no value columns (stack review item 6).
  vcols <- paste0("col_", seq_len(nc)[-1], recycle0 = TRUE)
  lab <- x$body[[1]]
  indent <- x$rows$indent
  tb <- data.frame(
    variable = ifelse(is.na(x$rows$var), x$rows$key, x$rows$var),
    row_type = ifelse(indent > 0, "level", "label"),
    key = x$rows$key,
    tt_row_id = seq_len(nrow(x$body)),
    label = sub("^ +", "", lab),
    stringsAsFactors = FALSE
  )
  for (j in seq_along(vcols)) tb[[vcols[j]]] <- x$body[[j + 1L]]
  g <- gtsummary::as_gtsummary(tb)
  g <- gtsummary::modify_column_hide(g, columns = c("variable", "row_type", "key", "tt_row_id"))
  # Column labels: the lowest header row, as plain text. A column whose
  # label is empty takes a one-cell label from the row above (table1_tc's
  # descriptor, p-value and SMD cells span both rows), as in tt_as_gt().
  # A table without header rows (puttab noheader) has empty labels (stack
  # review item 6).
  nh <- length(x$header)
  low <- if (nh) x$header[[nh]]$text else rep("", nc)
  up <- if (nh) rev(x$header[-nh]) else list()
  lifted <- integer()
  if (length(up)) {
    one <- .tt_gts_spans(x, up[[1]])
    for (sp in one) {
      if (length(sp$cols) == 1L && !nzchar(trimws(low[sp$cols]))) {
        low[sp$cols] <- sp$text
        lifted <- c(lifted, sp$cols)
      }
    }
  }
  # tabtools text is literal ("&lt;5", "A_B", "$1 and $2" as typed; codex
  # audit F09, audit A02). The header, spanning-header, caption and
  # source-note text is passed with text_interpret = "none", so gt shows it
  # as plain text (no Markdown, no "$...$" equation) and as_flex_table()
  # (Word, PowerPoint) shows it without the escapes a Markdown string would
  # need (review P1-1). as_kable() output is Markdown and is not escaped.
  hl <- stats::setNames(as.list(low), c("label", vcols))
  g <- do.call(gtsummary::modify_header, c(list(g), hl, list(text_interpret = "none")))
  # A label-only table has no value columns to align (stack review item 6).
  if (length(vcols)) g <- gtsummary::modify_column_alignment(g, columns = gtsummary::all_of(vcols), align = "center")
  # Spanning headers from the rows above, the lowest first (level 1).
  for (lv in seq_along(up)) {
    seen <- character()
    for (sp in .tt_gts_spans(x, up[[lv]])) {
      if (lv == 1L && length(sp$cols) == 1L && sp$cols %in% lifted) next
      # gtsummary makes one spanner of all the columns whose spanning text
      # is the same at a level: a repeated label (two models both called
      # "Model") takes zero-width spaces, so each keeps its own spanner, as
      # in tt_as_gt() (stack review item 8).
      txt <- sp$text
      txt <- paste0(txt, strrep("\u200b", sum(seen == txt)))
      seen <- c(seen, sp$text)
      # The label is spliced in, not deparsed: deparse() would write a
      # non-ASCII label as <U+...> in a non-UTF-8 locale.
      f <- stats::as.formula(call("~", str2lang(paste0("c(", paste0("`", vcols[sp$cols - 1L], "`", collapse = ", "), ")")), txt))
      g <- gtsummary::modify_spanning_header(g, f, level = lv, text_interpret = "none")
    }
  }
  # Row predicates are passed quoted: gtsummary evaluates them in the table
  # body.
  if (any(indent > 0)) {
    g <- do.call(gtsummary::modify_indent, list(g, columns = "label", rows = quote(row_type == "level"), indent = 4L))
  }
  refs <- which(x$rows$type %in% c("ref", "omitted", "empty"))
  for (j in seq_along(vcols)) {
    it <- refs[nzchar(x$body[[j + 1L]][refs])]
    if (!length(it)) next
    # Keys may be absent or repeat across stacked groups: select the
    # actual input rows, and let tbl_stack() qualify the predicate by table.
    g <- do.call(gtsummary::modify_italic, list(g, columns = vcols[j], rows = bquote(tt_row_id %in% .(it))))
  }
  if (!is.null(x$title) && nzchar(x$title)) g <- gtsummary::modify_caption(g, x$title, text_interpret = "none")
  if (!is.null(x$footnote) && nzchar(x$footnote)) {
    g <- gtsummary::modify_source_note(g, x$footnote, text_interpret = "none")
  }
  g
}

# Spans of one upper header row, as column indices of the body: an explicit
# merge span (table1_tc's group labels), else a model's columns for a label
# over a model block (regtab), else the cell alone.
.tt_gts_spans <- function(x, h) {
  out <- list()
  used <- integer()
  sp <- h$spans
  if (!is.null(sp) && nrow(sp)) {
    for (k in seq_len(nrow(sp))) {
      cc <- seq.int(sp$from[k], sp$to[k])
      cc <- cc[cc > 1L]
      txt <- h$text[sp$from[k]]
      if (length(cc) && nzchar(trimws(txt))) out[[length(out) + 1L]] <- list(cols = cc, text = txt)
      used <- c(used, cc)
    }
  }
  model <- x$cols$model
  for (j in setdiff(which(nzchar(trimws(h$text))), c(1L, used))) {
    cc <- if (!is.na(model[j])) which(model == model[j] & seq_along(model) >= j) else j
    out[[length(out) + 1L]] <- list(cols = cc, text = h$text[j])
  }
  out
}
