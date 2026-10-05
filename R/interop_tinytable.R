#' Convert a table to a tinytable
#'
#' `tt_as_tinytable()` builds a [tinytable][tinytable::tt] from a
#' `tt_table`'s cells, for LaTeX, Typst, HTML, Markdown and Word output
#' through [tinytable::save_tt()] and Quarto. The cells are tabtools'
#' formatted text. The lowest header row gives the column names and the
#' rows above it column groups ([tinytable::group_tt()]: regtab's model
#' labels over each model's columns, table1_tc's group labels over their
#' spans); level rows are indented, Reference/Omitted/Empty cells are
#' italic, the title is the caption and the footnote a note. Excel styling
#' (borders, fills, widths) is not carried over: use [tt_write_xlsx()],
#' [tt_as_gt()] or [flextable::as_flextable()] for the house style.
#'
#' tinytable has no conversion generic, so the function carries the
#' package's `tt_` prefix, as [tt_as_gt()] does.
#' @param x A `tt_table`.
#' @return A `tinytable` object.
#' @seealso [tt_as_gt()], [tt_as_gtsummary()]
#' @examplesIf requireNamespace("tinytable", quietly = TRUE)
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' tt_as_tinytable(regtab(lm(mpg ~ wt + cyl, data = d)))
#' @export
tt_as_tinytable <- function(x) {
  if (!inherits(x, "tt_table")) {
    cli::cli_abort("{.arg x} must be a {.cls tt_table}, not a {.cls {class(x)[1]}} object.", call = NULL)
  }
  .tt_need("tinytable", "tt_as_tinytable")
  validate_tt_table(x)
  nc <- ncol(x$body)
  # Column names: the lowest header row; an empty one takes a one-cell
  # label from the row above (table1_tc's cells spanning both rows).
  # A table without header rows (puttab noheader) has no column names
  # (stack review item 6).
  nh <- length(x$header)
  low <- if (nh) x$header[[nh]]$text else rep("", nc)
  up <- if (nh) rev(x$header[-nh]) else list()
  lifted <- integer()
  if (length(up)) {
    for (sp in .tt_gts_spans(x, up[[1]])) {
      if (length(sp$cols) == 1L && !nzchar(trimws(low[sp$cols]))) {
        low[sp$cols] <- sp$text
        lifted <- c(lifted, sp$cols)
      }
    }
  }
  body <- x$body
  body[[1]] <- sub("^ +", "", body[[1]])
  names(body) <- low
  cap <- if (!is.null(x$title) && nzchar(x$title)) x$title else NULL
  notes <- if (!is.null(x$footnote) && nzchar(x$footnote)) x$footnote else NULL
  out <- tinytable::tt(body, caption = cap, notes = notes, colnames = nh > 0L)
  # Column groups, the lowest first (group_tt() adds each row on top).
  for (lv in seq_along(up)) {
    spans <- .tt_gts_spans(x, up[[lv]])
    if (lv == 1L) spans <- Filter(function(sp) !(length(sp$cols) == 1L && sp$cols %in% lifted), spans)
    if (!length(spans)) next
    j <- stats::setNames(lapply(spans, `[[`, "cols"), vapply(spans, `[[`, "", "text"))
    out <- tinytable::group_tt(out, j = j)
  }
  lev <- which(x$rows$indent > 0)
  if (length(lev)) out <- tinytable::style_tt(out, i = lev, j = 1, indent = 1)
  refs <- which(x$rows$type %in% c("ref", "omitted", "empty"))
  for (jj in seq_len(nc)[-1]) {
    it <- refs[nzchar(x$body[[jj]][refs])]
    if (length(it)) out <- tinytable::style_tt(out, i = it, j = jj, italic = TRUE)
  }
  if (nc > 1L) out <- tinytable::style_tt(out, j = seq_len(nc)[-1], align = "c")
  # tabtools text is literal: "<0.001", "&lt;5", "A_B" and "<b>x</b>" are
  # shown as written in every output format, never read as markup (codex
  # audit F09). escape = TRUE covers the cells, column names, caption,
  # notes and group_tt() labels for HTML, LaTeX and Typst. It leaves
  # Markdown (and Word, built from it) raw, so for that output the cells
  # and column names are escaped with the Markdown writer's escape, and
  # the caption and notes by a Markdown-only prepare step (review of
  # 2026-09-28, I1). Markdown text escapes "$" as well, since pandoc
  # (Quarto, R Markdown) reads a "$" pair as math, as the .qmd/.rmd writer
  # does; the column-group labels get the same escape (audit A02).
  out <- tinytable::format_tt(out, escape = TRUE)
  out <- tinytable::format_tt(out, i = 0:nrow(body), fn = .tt_tinytable_md_escape, output = "markdown")
  .tt_tinytable_md_text(out)
}

.tt_tinytable_md_escape <- function(x) .md_escape(x, dollar = TRUE)

# The caption, notes and column-group labels of Markdown output, escaped
# when the table is built (tinytable's lazy_prepare steps run per output
# format, before they are written). Skipped on a tinytable without that
# slot.
.tt_tinytable_md_text <- function(out) {
  if (!methods::.hasSlot(out, "lazy_prepare")) return(out)
  md <- .tt_tinytable_md_escape
  esc <- function(n) {
    if (is.character(n)) return(md(n))
    if (is.list(n) && is.character(n$text)) n$text <- md(n$text)
    n
  }
  prep <- function(x) {
    if (length(x@caption) && is.character(x@caption)) x@caption <- md(x@caption)
    x@notes <- lapply(x@notes, esc)
    if (methods::.hasSlot(x, "group_data_j") && is.data.frame(x@group_data_j)) {
      x@group_data_j[] <- lapply(x@group_data_j, function(v) {
        if (!is.character(v)) return(v)
        ok <- !is.na(v)
        v[ok] <- md(v[ok])
        v
      })
    }
    x
  }
  out@lazy_prepare <- c(out@lazy_prepare, list(structure(prep, output = "markdown")))
  out
}
