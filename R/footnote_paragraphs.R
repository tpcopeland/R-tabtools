# Paragraph contract: regtab.sthlp:300-307 and the pinned Stata 2.5.1
# _tabtools_csv_write.ado:123-145, _tabtools_markdown_write.ado:137-159,
# _tabtools_xlsx_apply_styles.ado:405-427. Only a single spaced backslash
# separates paragraphs; unspaced or doubled backslashes remain literal.
.tt_footnote_paragraphs <- function(x) {
  .tt_check_footnote_arg(x)
  if (is.null(x) || !length(x)) return(character())
  out <- unlist(lapply(x, function(s) {
    if (!grepl(" \\ ", s, fixed = TRUE)) return(s)
    pieces <- strsplit(s, " \\ ", fixed = TRUE)[[1L]]
    trimws(pieces, whitespace = "[ ]")
  }), use.names = FALSE)
  out[nzchar(out)]
}

.tt_check_footnote_arg <- function(x, arg = "footnote") {
  if (!is.null(x) && (!is.character(x) || anyNA(x))) {
    cli::cli_abort("{.arg {arg}} must be a character vector of non-missing paragraphs.",
                   class = "tabtools_error_footnote", call = NULL)
  }
  invisible(x)
}

# Keep a scalar internally: existing table consumers rely on nzchar().
.tt_footnote_text <- function(x) paste(.tt_footnote_paragraphs(x), collapse = " \\ ")

.tt_append_footnotes <- function(x, notes) {
  .tt_footnote_text(c(.tt_footnote_paragraphs(x), .tt_footnote_paragraphs(notes)))
}

# Expand the bottom note row and its styles, following
# _tabtools_xlsx_apply_styles.ado:429-485. This runs after command layout,
# before rule application, so every layout and presentation converter uses
# the same paragraphs without changing body borders or widths.
.xlsx_expand_footnotes <- function(lay) {
  R <- nrow(lay$grid)
  rules <- lay$rules
  if (R < 2L || ncol(lay$grid) < 2L || !nrow(rules)) return(lay)
  is_note <- rules$op == .OP[["italic"]] & rules$r1 == R & rules$r2 == R &
    rules$c1 == 2L & rules$code == 1L
  if (!any(is_note)) return(lay)
  paras <- .tt_footnote_paragraphs(lay$grid[R, 2L])
  if (!length(paras)) paras <- ""
  n <- length(paras)
  if (n > 1L) {
    lay$grid <- rbind(lay$grid, matrix("", n - 1L, ncol(lay$grid)))
    lay$written <- rbind(lay$written, matrix(FALSE, n - 1L, ncol(lay$written)))
    src <- rules[rules$r1 <= R & rules$r2 >= R &
                   !rules$op %in% .OP[c("width", "top")], , drop = FALSE]
    for (j in seq_len(n - 1L)) {
      extra <- src
      extra$r1 <- extra$r2 <- R + j
      lay$rules <- rbind(lay$rules, extra)
      lay$written[R + j, ] <- lay$written[R, ]
    }
  }
  lay$grid[seq.int(R, length.out = n), 2L] <- paras
  lay
}
