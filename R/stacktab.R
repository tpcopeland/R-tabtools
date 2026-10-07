# stacktab (task 7.2): assemble blocks -- ranges of existing worksheets, or
# (R only) tt_tables -- into one composite sheet. Port of Stata tabtools
# 2.1.12 stacktab.ado.
#
# Stata reads each block with `import excel ..., allstring`
# (stacktab.ado:227-236): cell values only. Source fonts, fills, borders,
# merges and widths are not copied; the composite gets stacktab's own fixed
# style (_stacktab_apply_style, :1392-1545: Arial 10, wrap, vertically
# centred, a bold first row with a thin rule below it, a thin rule below the
# last row, bold section rows with thin rules above and below, computed
# widths) plus the title and note rows (_stacktab_xlsx_put_text_mata,
# :1632-1662). Verified with a probe on 2026-09-26: blocks written by puttab
# with headershade, zebra, Times New Roman 14 and medium borders stack into
# Arial 10 cells with none of that styling. It does not use
# _tabtools_xlsx_read.ado.
#
# Stacking is positional, as in Stata: vstack appends the blocks' rows in
# order (each block's first row is a section row), hstack joins the blocks
# row by row on the row position (Stata merges 1:1 on `_rowid`). Nothing is
# joined on labels.

#' Assemble a composite table from blocks
#'
#' R implementation of the Stata `stacktab` command, the assembly end of the
#' styled-export pipeline: take ranges of existing sheets (typically written
#' by [puttab()], [table1_tc()], or [regtab()]) and stack them vertically or
#' place them side by side in one sheet of the same workbook, with column
#' merges, section labels, a title, and a note. Tables already in memory can
#' be given as blocks directly, without the workbook round trip.
#'
#' @section Blocks:
#' `blocks` is either Stata's block specification as one string --
#' `"sheet(Block A) rows(2/5) cols(B-D) label(Any use) \\ sheet(Block B) ..."`
#' (blocks separated by a backslash; sub-options `sheet()`, `rows()`,
#' `cols()`, `label()`, `skip()`, and `postfix()`) -- or a list whose
#' elements are each a `tt_table` or a list with the same fields
#' (`sheet` or `table`, `rows`, `cols`, `label`, `skip`, `postfix`).
#'
#' * `sheet`: a sheet of the workbook `xlsx`. Its values are read as text;
#'   formatting is not copied (Stata re-applies stacktab's own style too).
#'   Without `rows` and `cols` the block is the sheet's whole used range
#'   (from its first to its last used row and column, so a table written
#'   from `B2` with a title in `A1` includes the title row and the gutter
#'   column). `rows` and `cols` together select that cell range in sheet
#'   coordinates, which must lie inside the used range. `cols` alone keeps
#'   those sheet columns of the used range, and `rows` alone those sheet
#'   rows of it (as in tabtools 2.1.12; 2.1.11 counted `rows` alone from
#'   the first used row).
#' * `table` (R only): a `tt_table`. The block is its header rows and body as
#'   its CSV sink shows them (no title or footnote); `rows` and `cols` count
#'   within that grid (`cols = "A"` is the label column).
#' * `rows`: `"lo/hi"` (or `"lo hi"`, as Stata also accepts), one row, or a
#'   range such as `2:5`. `cols`: Excel letters, `"B-D"` (or `"B D"`),
#'   `"B"`, or `c("B", "D")`.
#' * `skip`: drop the n-th row of the selected block, a positive whole
#'   number (a number beyond the block's last row, such as 9 for a
#'   two-row block, drops nothing; 0 and negative numbers are errors, as in
#'   Stata). `label`: replace the
#'   first cell of the block's first row. `postfix`: append a space and this
#'   text to every cell of the block's first column.
#'
#' Under `layout = "vstack"` the blocks' rows follow one another (a block
#' narrower than the widest is padded with blank cells), with `spacing`
#' blank rows between blocks; the first row of every block is a section row,
#' bold between thin rules. Under `"hstack"` the blocks must have the same
#' number of rows and are placed side by side.
#'
#' @section Column merges:
#' `columnmerge` takes Stata's pieces, `"B+C as aHR (95% CI)"`, several in a
#' character vector or separated by a backslash. The two columns are named
#' by their letter in the composite as assembled (`A` is its first column)
#' or as `_xcol2`; letters keep their meaning across pieces even after an
#' earlier merge removed a column. Each cell of the second column, when not
#' blank or `"."`, is appended to the first after a space, the second
#' column is dropped, and the header text replaces the merged cell of every
#' section row (the first row under `hstack`).
#'
#' @param frames Explicit in-memory panel mode: a named list of data frames
#'   or `tt_table` sources. Each entry may instead be
#'   `list(data = source, label = "Panel heading")`. Names identify sources
#'   and must be unique; an unnamed list gets `frame1`, `frame2`, etc.
#'   Labels are literal; omitted or blank labels add no heading. Columns
#'   stack by position and must have equal counts. The first source supplies
#'   the only ordinary header, using variable labels where present. Each
#'   data frame's embedded header is dropped; each `tt_table` contributes
#'   its body. Panel headings and indent follow [puttab()]. Numeric
#'   source display formats are retained. Cannot combine with `blocks`,
#'   explicit `layout`, `spacing`, `display`, `append`, `sheetreplace`,
#'   `columnmerge`, `style` or `borders`.
#' @param noindent,headershade,borderstyle,font,fontsize,zebra,digits,open
#'   Frame-panel mode only: passed to [puttab()]. Explicit
#'   `headershade = FALSE` overrides session shading. `noindent` disables
#'   panel row indentation.
#' @param blocks The blocks: a Stata block specification string, or a list
#'   (see Blocks).
#' @param xlsx The workbook (Stata's `using`): sheet blocks are read from
#'   it and the composite is written to it. Optional when every block is a
#'   `tt_table` or `frames` is given; the workbook is then created if needed.
#' @param sheet Output sheet (required with `xlsx`). In block mode an
#'   existing sheet (matched without regard to case) is refused unless
#'   `append` or `sheetreplace` is given. In `frames` mode the sheet is
#'   created or replaced, as by [puttab()], preserving unrelated sheets.
#' @param layout `"vstack"` (default) or `"hstack"`.
#' @param title Title in column `A` above the table, bold Arial 12.
#' @param note,footnote Note below the table, italic Arial 8 (`footnote` is
#'   an alias; give one of them).
#'   Footnotes accept a character vector of paragraphs. The reserved token
#'   `" \\ "` (one backslash with surrounding spaces) splits each element,
#'   including a scalar, into paragraphs. Split pieces are trimmed and empty
#'   pieces dropped; unspaced and doubled backslashes remain literal.
#'   Automatic notes are separate paragraphs in every sink.
#' @param columnmerge Column merges (see Column merges).
#' @param style Row heights and column widths: a list with any of
#'   `titlerowheight` (default 30), `noterowheight` (default 45), and
#'   `colwidth` (named widths by composite column letter,
#'   `c(A = 24, C = 12)`), or Stata's string
#'   `"titlerowheight(40) colwidth(A 24 \\ C 12)"`.
#' @param borders Extra rules: any of `"outer(all)"` (a thin frame around
#'   the table), `"top(row 1)"`, `"bottom(last)"`, and `"bottom(row k)"` (a
#'   rule below composite row `k`), in a character vector or one string.
#' @param spacing Blank rows between vertically stacked blocks.
#' @param csv Also write the composite to this `.csv` file (title as its
#'   first row, note as its last).
#' @param markdown Also write it to this Markdown file (the composite's
#'   first row is the header).
#' @param mdappend Append to an existing `markdown` file.
#' @param display Print the composite as Stata's listing before writing.
#' @param append Write below the used rows of an existing `sheet`.
#' @param sheetreplace Replace an existing `sheet`.
#' @return A `tt_table` (`command = "stacktab"`): the composite's first row
#'   is its header row, the other rows its body (the analogue of Stata's
#'   `frame()`). It is returned invisibly when a file is written or
#'   `display = TRUE` has printed it (assign it and print it to see it).
#'   In `frames` mode the result uses puttab's layout and stored counts,
#'   plus `n_frames`, `frames` (source identities in order), and
#'   `blocks_loaded`; `source` is `"frames"`. Invalid frame-panel arguments
#'   raise `tabtools_error_layout`. For block mode, `$stored` holds Stata's
#'   `r()` results: `blocks_loaded`, `rows_written`, `cols_out`, `layout`,
#'   and, with `xlsx`, `rows_out`, `append_start`, `note_row`, `sheet`,
#'   `book`, `table_start`, `title_cell`; `csv`, `markdown`,
#'   `markdown_rows`, `markdown_cols` for those files.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @section Differences from Stata:
#' * `tt_table` blocks and a missing `xlsx` are R additions.
#' * Numeric and date cells of a source sheet are converted to text by
#'   openxlsx2 (`0.33333333333333331`, `2020-01-06`) and generally differ
#'   from Stata's `allstring` text (`.3333333333333333`, `  1/6/2020`); the
#'   tabtools commands write text cells only, so their sheets read the same
#'   on both sides.
#' * Block sheet names are matched with case, as Stata's `import excel`
#'   does; the output `sheet` is matched without regard to case, as
#'   Stata's `stacktab` does.
#' * Side-by-side (`hstack`) `tt_table` blocks whose rows differ at a
#'   position (by the table's row keys, else the row labels) give a warning
#'   naming them; the rows are still placed by position, as in Stata.
#' * In block mode the files are written together at the end, as in Stata;
#'   a failure while copying them into place restores replaced files.
#'   In `frames` mode, as in native Stata, [puttab()] writes CSV, Markdown
#'   and Excel sequentially after validating source metadata and targets;
#'   a later write failure can leave an earlier sink written. Existing
#'   output sheets are replaced while unrelated sheets are preserved.
#'   There is no `frame()`: the returned table is the composite.
#' * Stata's `stacktab: N blocks -> N rows written -> sheet S` line, printed
#'   when a workbook is written, is a message; nothing is printed for the
#'   CSV and Markdown targets.
#' @seealso [puttab()] to write the source blocks.
#' @examples
#' primary <- data.frame(term = c("Any HRT", "Former smoker"),
#'                       ahr = c("0.82", "1.14"),
#'                       ci = c("(0.69, 0.98)", "(0.97, 1.34)"))
#' dose <- data.frame(term = c("Low dose", "High dose"), ahr = c("0.91", "0.73"),
#'                    ci = c("(0.74, 1.12)", "(0.58, 0.92)"))
#' for (d in c("primary", "dose")) {
#'   x <- get(d)
#'   attr(x$term, "label") <- "Exposure"
#'   attr(x$ahr, "label") <- "aHR"
#'   attr(x$ci, "label") <- "95% CI"
#'   assign(d, x)
#' }
#' path <- tempfile(fileext = ".xlsx")
#' puttab(primary, xlsx = path, sheet = "Primary", varlabels = TRUE, title = "Primary")
#' puttab(dose, xlsx = path, sheet = "Dose", varlabels = TRUE, title = "Dose")
#' stacktab("sheet(Primary) rows(2/4) cols(B-D) label(Any HRT use) \\
#'           sheet(Dose) rows(2/4) cols(B-D) label(By estrogen dose)",
#'          xlsx = path, sheet = "Composite",
#'          columnmerge = "B+C as aHR (95% CI)",
#'          title = "Hormone therapy and recurrent events", display = TRUE)
#'
#' # The same composite from the tables themselves (R only)
#' a <- puttab(primary, varlabels = TRUE)
#' b <- puttab(dose, varlabels = TRUE)
#' stacktab(list(list(table = a, label = "Any HRT use"),
#'               list(table = b, label = "By estrogen dose")),
#'          columnmerge = "B+C as aHR (95% CI)")
#'
#' # Frames are panels with one common header
#' stacktab(frames = list(primary = list(data = primary, label = "Primary"),
#'                        dose = list(data = dose, label = "Dose")))
#' @section Session destinations:
#' Omitted destinations inherit [tabtools_options()] workbook/Markdown values;
#' explicit NULL opts out of each sink. Header shading inherits only for puttab
#' and stacktab frames mode. Stacktab still requires an output sheet with a
#' workbook. See [tabtools_options()] for append/history and warning behavior.
#'
#' @export
stacktab <- function(blocks = NULL, xlsx = NULL, sheet = NULL, layout = "vstack", title = NULL,
                     note = NULL, footnote = NULL, columnmerge = NULL, style = NULL,
                     borders = NULL, spacing = 0, csv = NULL, markdown = NULL,
                     mdappend = FALSE, display = FALSE, append = FALSE,
                     sheetreplace = FALSE, frames = NULL, noindent = FALSE,
                     headershade = FALSE, borderstyle = NULL, font = NULL,
                     fontsize = NULL, zebra = FALSE, digits = NULL, open = FALSE) {
  sinks <- .tt_resolve_sinks(
    list(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, headershade = headershade, append = append,
         sheetreplace = sheetreplace),
    list(xlsx = !missing(xlsx), markdown = !missing(markdown),
         mdappend = !missing(mdappend), sheet = !missing(sheet),
         headershade = !missing(headershade), append = !missing(append),
         sheetreplace = !missing(sheetreplace)),
    policy = "omitted", shade = !is.null(frames))
  xlsx <- sinks$values$xlsx
  markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend
  if (!is.null(frames)) {
    forbidden <- c(blocks = !is.null(blocks), layout = !missing(layout),
                   columnmerge = !is.null(columnmerge), style = !is.null(style),
                   borders = !is.null(borders), spacing = !missing(spacing),
                   display = !missing(display), append = !missing(append),
                   sheetreplace = !missing(sheetreplace))
    if (any(forbidden)) {
      .puttab_abort(paste0("frames cannot be combined with ", paste(names(forbidden)[forbidden], collapse = ", "), "."))
    }
    if (!is.null(xlsx) && is.null(sheet)) .puttab_abort("sheet is required with xlsx in frames mode.")
    args <- list(xlsx = xlsx, title = title, footnote = note %||% footnote, csv = csv,
                 markdown = markdown, mdappend = mdappend, borderstyle = borderstyle,
                 font = font, fontsize = fontsize, zebra = zebra, digits = digits, open = open,
                 noindent = noindent)
    if (!is.null(note) && !is.null(footnote)) .puttab_abort("note and footnote may not be combined.")
    args$headershade <- sinks$values$headershade
    if (!is.null(sheet)) args$sheet <- sheet
    return(.stacktab_frames(frames, args))
  }
  if (!missing(noindent) || !missing(headershade) || !missing(borderstyle) ||
      !missing(font) || !missing(fontsize) || !missing(zebra) || !missing(digits) || !missing(open)) {
    .puttab_abort("noindent, headershade, borderstyle, font, fontsize, zebra, digits and open require frames.")
  }
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  for (a in c("mdappend", "display", "append", "sheetreplace")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  has_xlsx <- !is.null(xlsx)
  has_md <- !is.null(markdown)
  # stacktab.ado:66-121, in Stata's order.
  if (has_xlsx) {
    if (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) || !grepl("\\.xlsx$", tolower(xlsx))) {
      cli::cli_abort("{.arg xlsx} must specify a .xlsx file.", call = NULL)
    }
    if (is.null(sheet)) cli::cli_abort("{.arg sheet} is required with {.arg xlsx}.", call = NULL)
    sheet <- .check_sheet(sheet)
  }
  if (!is.character(layout) || length(layout) != 1L || !tolower(layout) %in% c("vstack", "hstack")) {
    cli::cli_abort("{.arg layout} must be {.val vstack} or {.val hstack}.", call = NULL)
  }
  layout <- tolower(layout)
  if (append && sheetreplace) cli::cli_abort("{.arg append} and {.arg sheetreplace} may not be combined.", call = NULL)
  for (a in c("title", "note", "footnote")) .tt_check_text_arg(get(a), a)
  note <- if (is.null(note)) NULL else .tt_footnote_text(note)
  footnote <- if (is.null(footnote)) NULL else .tt_footnote_text(footnote)
  if (!is.null(note) && nzchar(note) && !is.null(footnote) && nzchar(footnote)) {
    cli::cli_abort("{.arg note} and {.arg footnote} may not be combined.", call = NULL)
  }
  note <- if (!is.null(note) && nzchar(note)) note else footnote %||% ""
  title <- title %||% ""
  if (!is.numeric(spacing) || length(spacing) != 1L || is.na(spacing) || spacing != round(spacing) || spacing < 0) {
    cli::cli_abort("{.arg spacing} must be a nonnegative whole number.", call = NULL)
  }
  spacing <- as.integer(spacing)
  if (!is.null(csv)) .tt_check_csv_path(csv)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L || is.na(markdown) ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  st <- .stacktab_style(style)
  bspecs <- .stacktab_blocks(blocks)
  merges <- .stacktab_parse_merges(columnmerge)
  if (!has_xlsx && (append || sheetreplace)) {
    cli::cli_abort("{.arg append} and {.arg sheetreplace} require {.arg xlsx}.", call = NULL)
  }
  needs_book <- any(vapply(bspecs, function(b) !is.null(b$sheet), TRUE))
  if (needs_book && !has_xlsx) {
    cli::cli_abort("Sheet blocks are read from {.arg xlsx}; give the workbook.", call = NULL)
  }
  if (needs_book && !file.exists(xlsx)) cli::cli_abort("Workbook {.file {xlsx}} not found.", call = NULL)

  # Import and stack (stacktab.ado:184-391).
  wb <- if (has_xlsx && file.exists(xlsx)) .stacktab_load(xlsx) else NULL
  grids <- lapply(seq_along(bspecs), function(b) .stacktab_block_grid(bspecs[[b]], b, wb))
  comp <- .stacktab_stack(grids, layout, spacing)
  cells <- comp$cells
  ids <- seq_len(ncol(cells))
  for (m in merges) {
    res <- .stacktab_merge(cells, ids, m, comp$sections)
    cells <- res$cells
    ids <- res$ids
  }
  borders <- .stacktab_borders(borders, nrow(cells))
  colwidth <- .stacktab_colwidth_index(st$colwidth)
  tt <- .stacktab_table(cells, sections = comp$sections, title = title, note = note,
                        meta = list(title_height = st$titlerowheight, note_height = st$noterowheight,
                                    colwidth = colwidth, borders = borders, layout = layout,
                                    sheet = sheet %||% "Composite",
                                    sample_accounting = .tt_sample_bind(
                                      lapply(bspecs, function(b) if (is.null(b$table)) NULL else
                                        b$table$meta[["sample_accounting", exact = TRUE]]),
                                      prefixes = paste0("block", seq_along(bspecs)),
                                      commands = vapply(bspecs, function(b) if (is.null(b$table))
                                        "stacktab" else b$table$command, ""))),
                        stored = list(blocks_loaded = length(bspecs), rows_written = nrow(cells),
                                      cols_out = ncol(cells), layout = layout))

  if (display) {
    # stacktab.ado:512-545: the listing (title above), a blank line, the note.
    lines <- tt_console_lines(tt)
    # Paragraphs keep their leading/between-paragraph separators, but
    # stacktab's status follows the last paragraph without a blank line.
    if (length(.tt_footnote_paragraphs(tt$footnote))) lines <- lines[-length(lines)]
    cat(lines, sep = "\n")
  }

  # Where the table goes (stacktab.ado:547-574), then the file preflight.
  start_row <- 2L
  title_row <- 1L
  if (has_xlsx) {
    info <- if (!is.null(wb)) .stacktab_sheet_info(wb, sheet) else NULL
    if (!is.null(info)) {
      sheet <- info$sheet
      if (!append && !sheetreplace) {
        cli::cli_abort(c("Sheet {.val {sheet}} already exists in {.file {xlsx}}.",
                         "i" = "Use {.code append = TRUE} or {.code sheetreplace = TRUE}."), call = NULL)
      }
      if (append && info$rows > 0L) {
        title_row <- info$rows + 1L
        start_row <- info$rows + 1L + nzchar(title)
      }
    }
    tt$meta$sheet <- sheet
  }
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)

  # Stage every file, then commit them together (stacktab.ado:615-851).
  stage <- tempfile("stacktab-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged <- character()
  if (!is.null(csv)) {
    staged[csv] <- file.path(stage, "out.csv")
    tt_write_csv(tt, staged[[csv]])
    tt$stored$csv <- csv
  }
  if (has_md) {
    # The staged copy keeps the target's extension, which decides the
    # escaping of "$" (.qmd/.rmd; audit A02).
    staged[markdown] <- file.path(stage, paste0("out.", tools::file_ext(markdown)))
    # Codex audit CX-1: an append starts from a verified copy of the
    # existing file; one that cannot be read (mode 0200) stops here, before
    # any target is touched, instead of staging a new document that the
    # commit would write over the original.
    if (mdappend && file.exists(markdown)) .tt_copy_checked(markdown, staged[[markdown]], "markdown")
    res <- tt_write_markdown(tt, staged[[markdown]], append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
  }
  if (has_xlsx) {
    staged[xlsx] <- file.path(stage, "book.xlsx")
    if (file.exists(xlsx)) .tt_copy_checked(xlsx, staged[[xlsx]], "xlsx")
    .stacktab_write_xlsx(tt, staged[[xlsx]], sheet, start_row = start_row, title_row = title_row,
                         append = append)
    rows_out <- start_row + nrow(cells) - 1L
    tt$stored$rows_out <- rows_out
    tt$stored$append_start <- start_row
    if (nzchar(note)) tt$stored$note_row <- rows_out + 1L
    tt$stored$sheet <- sheet
    tt$stored$book <- xlsx
    tt$stored$table_start <- paste0("B", start_row)
    if (nzchar(title)) tt$stored$title_cell <- paste0("A", title_row)
  }
  .stacktab_commit(staged)
  if (has_md) .tt_mark_sink(markdown, "markdown")
  if (has_xlsx) .tt_mark_sink(xlsx, "workbook")
  if (has_xlsx) {
    message(sprintf("stacktab: %d blocks -> %d rows written -> sheet %s", length(bspecs), nrow(cells), sheet))
  }
  # display has printed the listing already (review F14).
  if (length(staged) || display) invisible(tt) else tt
}

# ---------------------------------------------------------------------------
# Specifications

# The blocks() grammar of Stata tabtools since Stata-Tools 68c37a90
# (`_stacktab_blocks()`, `_stacktab_blk_close()`, `_stacktab_blk_unquote()`
# in stacktab.ado; task C7): blocks are split on backslashes outside
# parentheses and quotes; each block is a sequence of suboptions
# `name(value)` read at its top level only (so `label(rows(3/3))` is a
# label), a value may be in plain or compound double quotes, which are
# removed, and parentheses or backslashes inside quotes are literal; an
# unknown suboption, one given twice, or text that is not a suboption is an
# error.
.stacktab_block_names <- c("sheet", "rows", "cols", "label", "skip", "postfix")

# Characters of `s` with a flag for "inside plain or compound quotes".
.stacktab_scan <- function(s) {
  ch <- strsplit(s, "")[[1]]
  n <- length(ch)
  quoted <- logical(n)
  inq <- FALSE
  cq <- 0L
  i <- 1L
  while (i <= n) {
    two <- if (i < n) paste0(ch[i], ch[i + 1L]) else ""
    if (!inq && two == "`\"") {
      cq <- cq + 1L
      quoted[i:(i + 1L)] <- TRUE
      i <- i + 2L
      next
    }
    if (cq > 0L && two == "\"'") {
      cq <- cq - 1L
      quoted[i:(i + 1L)] <- TRUE
      i <- i + 2L
      next
    }
    if (cq == 0L && ch[i] == "\"") {
      inq <- !inq
      quoted[i] <- TRUE
      i <- i + 1L
      next
    }
    quoted[i] <- inq || cq > 0L
    i <- i + 1L
  }
  list(ch = ch, quoted = quoted)
}

# The position of the `)` closing the parenthesis opened before `start`, or
# 0 when it is never closed.
.stacktab_close <- function(sc, start) {
  depth <- 1L
  for (i in seq.int(start, length(sc$ch))) {
    if (sc$quoted[i]) next
    if (sc$ch[i] == "(") depth <- depth + 1L
    if (sc$ch[i] == ")") {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
  }
  0L
}

.stacktab_unquote <- function(v) {
  n <- nchar(v)
  if (n >= 4L && startsWith(v, "`\"") && endsWith(v, "\"'")) return(substr(v, 3L, n - 2L))
  if (n >= 2L && startsWith(v, "\"") && endsWith(v, "\"")) return(substr(v, 2L, n - 1L))
  v
}

# Split on backslashes outside parentheses and quotes.
.stacktab_split <- function(text) {
  sc <- .stacktab_scan(text)
  depth <- 0L
  cuts <- integer()
  for (i in seq_along(sc$ch)) {
    if (sc$quoted[i]) next
    c <- sc$ch[i]
    if (c == "(") depth <- depth + 1L else if (c == ")") depth <- depth - 1L else if (c == "\\" && depth == 0L) cuts <- c(cuts, i)
  }
  from <- c(1L, cuts + 1L)
  to <- c(cuts - 1L, length(sc$ch))
  out <- mapply(function(a, b) if (b >= a) paste(sc$ch[a:b], collapse = "") else "", from, to)
  out <- trimws(out, whitespace = "[ \t\r\n]")
  out[nzchar(out)]
}

# One block's suboptions, named, their values unquoted.
.stacktab_parse_block <- function(piece, k) {
  sc <- .stacktab_scan(piece)
  ch <- sc$ch
  n <- length(ch)
  out <- list()
  pos <- 1L
  err <- function(msg) cli::cli_abort(paste0("{.arg blocks}, block ", k, ": ", msg), call = NULL)
  while (pos <= n) {
    if (ch[pos] %in% c(" ", "\t", "\r", "\n")) {
      pos <- pos + 1L
      next
    }
    st <- pos
    while (pos <= n && grepl("[A-Za-z]", ch[pos])) pos <- pos + 1L
    name <- if (pos > st) paste(ch[st:(pos - 1L)], collapse = "") else ""
    if (!nzchar(name) || pos > n || ch[pos] != "(") {
      err(paste0("expected a suboption such as {.code sheet(...)} at {.val ", substr(piece, st, n), "}."))
    }
    close <- .stacktab_close(sc, pos + 1L)
    if (!close) err(paste0("malformed {.code ", name, "()}."))
    lname <- tolower(name)
    if (!lname %in% .stacktab_block_names) {
      err(paste0("unknown block suboption {.code ", name, "()}; allowed are sheet(), rows(), cols(), label(), skip(), postfix()."))
    }
    if (!is.null(out[[lname]])) err(paste0("block suboption {.code ", lname, "()} is given more than once."))
    v <- if (close > pos + 1L) paste(ch[(pos + 1L):(close - 1L)], collapse = "") else ""
    out[[lname]] <- .stacktab_unquote(trimws(v, whitespace = "[ \t]"))
    pos <- close + 1L
  }
  out
}

.stacktab_blocks <- function(blocks) {
  if (inherits(blocks, "tt_table")) blocks <- list(blocks)
  if (is.character(blocks)) {
    if (length(blocks) != 1L || is.na(blocks)) {
      cli::cli_abort("A {.arg blocks} specification must be one string.", call = NULL)
    }
    pieces <- .stacktab_split(blocks)
    out <- lapply(seq_along(pieces), function(k) {
      b <- .stacktab_parse_block(pieces[[k]], k)
      b[vapply(b, nzchar, TRUE)]
    })
  } else if (is.list(blocks)) {
    out <- lapply(blocks, function(b) {
      if (inherits(b, "tt_table")) return(list(table = b))
      if (!is.list(b)) cli::cli_abort("Each element of {.arg blocks} must be a {.cls tt_table} or a list.", call = NULL)
      bad <- setdiff(names(b), c("sheet", "table", "rows", "cols", "label", "skip", "postfix"))
      if (length(b) && (is.null(names(b)) || any(!nzchar(names(b))))) {
        cli::cli_abort("The fields of a {.arg blocks} element must be named.", call = NULL)
      }
      if (length(bad)) cli::cli_abort("Unknown {.arg blocks} field{?s}: {.field {bad}}.", call = NULL)
      b
    })
  } else {
    cli::cli_abort("{.arg blocks} must be a block specification string or a list of blocks.", call = NULL)
  }
  if (!length(out)) cli::cli_abort("{.arg blocks} holds no blocks.", call = NULL)
  for (k in seq_along(out)) {
    b <- out[[k]]
    has_sheet <- !is.null(b$sheet)
    has_table <- !is.null(b$table)
    if (has_sheet == has_table) {
      cli::cli_abort("Block {k} needs either {.field sheet} or {.field table}.", call = NULL)
    }
    if (has_sheet && (!is.character(b$sheet) || length(b$sheet) != 1L || is.na(b$sheet) || !nzchar(b$sheet))) {
      cli::cli_abort("Block {k}: {.field sheet} must be a sheet name.", call = NULL)
    }
    if (has_table && !inherits(b$table, "tt_table")) {
      cli::cli_abort("Block {k}: {.field table} must be a {.cls tt_table}.", call = NULL)
    }
    for (f in c("label", "postfix")) {
      v <- b[[f]]
      if (!is.null(v) && (!is.character(v) || length(v) != 1L || is.na(v))) {
        cli::cli_abort("Block {k}: {.field {f}} must be a single string.", call = NULL)
      }
    }
    if (!is.null(b$rows)) b$rows <- .stacktab_rows(b$rows, k)
    if (!is.null(b$cols)) b$cols <- .stacktab_cols(b$cols, k)
    if (!is.null(b$skip)) {
      # Stata's `drop if _n == skip`: since Stata-Tools 68c37a90 skip() must
      # be a positive integer (`confirm integer number`, >= 1); one past the
      # end still drops nothing.
      s <- suppressWarnings(as.numeric(b$skip))
      if (length(s) != 1L || is.na(s) || s < 1 || s != round(s)) {
        cli::cli_abort("Block {k}: {.field skip} must be a positive integer.", call = NULL)
      }
      b$skip <- s
    }
    out[[k]] <- b
  }
  out
}

# rows(lo/hi) (_stacktab_parse_rows, stacktab.ado:1175-1212).
.stacktab_rows <- function(x, k) {
  bad <- function() cli::cli_abort("Block {k}: {.field rows} must be a positive lo/hi range such as {.val 2/5} or {.code 2:5}.",
                                   call = NULL)
  if (is.character(x)) {
    if (length(x) != 1L || is.na(x)) bad()
    # Stata turns "/" into a space and takes two words: rows(2/4) and
    # rows(2 4) are the same.
    p <- strsplit(trimws(gsub("/", " ", x, fixed = TRUE)), "[ ]+")[[1]]
    if (!length(p) || length(p) > 2L || !all(nzchar(p))) bad()
    v <- suppressWarnings(as.numeric(p))
  } else if (is.numeric(x)) {
    v <- x
    if (length(v) > 2L && any(diff(v) != 1)) bad()
    if (length(v) == 2L && v[2] != v[1] + 1 && v[2] != v[1]) {
      cli::cli_abort(c("Block {k}: numeric {.field rows} must be consecutive (such as {.code 2:5}).",
                       "i" = "For a lo/hi range give {.val {paste(v, collapse = '/')}}."), call = NULL)
    }
    v <- range(v)
  } else {
    bad()
  }
  if (anyNA(v) || any(v != round(v)) || any(v < 1)) bad()
  v <- as.integer(c(v[1], v[length(v)]))
  if (v[2] < v[1]) bad()
  v
}

# cols(A-D) (_stacktab_parse_cols, stacktab.ado:1215-1254): Excel letters.
.stacktab_cols <- function(x, k) {
  bad <- function() cli::cli_abort("Block {k}: {.field cols} must be Excel column letters such as {.val B-D}.",
                                   call = NULL)
  if (!is.character(x) || !length(x) || length(x) > 2L || anyNA(x)) bad()
  # As for rows(), "-" and a space both separate the two letters.
  p <- if (length(x) == 1L) strsplit(trimws(gsub("-", " ", x, fixed = TRUE)), "[ ]+")[[1]] else trimws(x)
  if (!length(p) || length(p) > 2L || !all(grepl("^[A-Za-z]+$", p))) bad()
  idx <- .xlsx_col_index(toupper(p))
  if (idx[length(idx)] < idx[1]) {
    cli::cli_abort("Block {k}: the {.field cols} range must run left to right.", call = NULL)
  }
  as.integer(c(idx[1], idx[length(idx)]))
}

.xlsx_col_index <- function(letters) {
  vapply(strsplit(letters, ""), function(ch) Reduce(function(a, v) a * 26L + v, match(ch, LETTERS), 0L), 0L)
}

# columnmerge pieces (stacktab.ado:396-470).
.stacktab_parse_merges <- function(x) {
  if (is.null(x)) return(list())
  if (!is.character(x) || anyNA(x)) cli::cli_abort("{.arg columnmerge} must be a character vector.", call = NULL)
  pieces <- unlist(lapply(x, function(s) trimws(strsplit(s, "\\", fixed = TRUE)[[1]], whitespace = "[ \t\r\n]")))
  pieces <- pieces[nzchar(pieces)]
  lapply(pieces, function(p) {
    at <- regexpr(" as ", p, fixed = TRUE)
    if (at < 0) {
      cli::cli_abort(c("Malformed {.arg columnmerge} piece {.val {p}}.",
                       "i" = "Expected syntax like {.val B+C as Header}."), call = NULL)
    }
    pair <- trimws(substr(p, 1L, at - 1L))
    hdr <- .stacktab_unquote(trimws(substring(p, at + 4L)))
    if (!nzchar(hdr)) cli::cli_abort("{.arg columnmerge}: the header may not be empty.", call = NULL)
    cols <- strsplit(trimws(gsub("+", " ", pair, fixed = TRUE)), "[ ]+")[[1]]
    if (length(cols) != 2L) {
      cli::cli_abort("{.arg columnmerge} requires exactly two columns in {.val {pair}}.", call = NULL)
    }
    ids <- vapply(cols, .stacktab_col_id, 0L, what = "columnmerge")
    if (ids[1] == ids[2]) cli::cli_abort("{.arg columnmerge} cannot merge a column with itself.", call = NULL)
    list(a = ids[[1]], b = ids[[2]], header = hdr, text = p)
  })
}

# A composite column reference: an Excel letter (A = first composite column)
# or Stata's internal name _xcolN (_stacktab_resolve_col, :1145-1172).
.stacktab_col_id <- function(s, what) {
  s <- trimws(s)
  if (grepl("^_xcol[0-9]+$", s)) return(as.integer(sub("^_xcol", "", s)))
  if (grepl("^[A-Za-z]+$", s)) return(.xlsx_col_index(toupper(s)))
  cli::cli_abort("{.arg {what}}: {.val {s}} is not a column letter.", call = NULL)
}

# style(): titlerowheight(#) noterowheight(#) colwidth(COL W \ ...)
# (_stacktab_validate_style, stacktab.ado:1278-1389).
.stacktab_style <- function(x) {
  out <- list(titlerowheight = 30, noterowheight = 45, colwidth = numeric())
  if (is.null(x)) return(out)
  if (is.character(x)) {
    if (length(x) != 1L || is.na(x)) cli::cli_abort("A {.arg style} string must be a single string.", call = NULL)
    s <- gsub("\"", "", x, fixed = TRUE)
    lst <- list()
    for (key in c("titlerowheight", "noterowheight", "colwidth")) {
      pos <- regexpr(paste0(key, "("), tolower(s), fixed = TRUE)
      if (pos < 0) next
      tail <- substring(s, pos + nchar(key) + 1L)
      end <- regexpr(")", tail, fixed = TRUE)
      if (end < 0) cli::cli_abort("{.arg style}: {.code {key}()} is missing its closing parenthesis.", call = NULL)
      val <- trimws(substr(tail, 1L, end - 1L))
      if (key == "colwidth") {
        ent <- trimws(strsplit(val, "\\", fixed = TRUE)[[1]])
        ent <- ent[nzchar(ent)]
        if (!length(ent)) cli::cli_abort("{.arg style}: {.code colwidth()} is empty.", call = NULL)
        w <- numeric()
        for (e in ent) {
          tok <- strsplit(e, "[ ]+")[[1]]
          if (length(tok) != 2L) {
            cli::cli_abort("{.arg style}: {.code colwidth()} entry {.val {e}} needs a column and a width.", call = NULL)
          }
          w[tok[1]] <- suppressWarnings(as.numeric(tok[2]))
        }
        lst$colwidth <- w
      } else {
        lst[[key]] <- suppressWarnings(as.numeric(val))
      }
    }
    x <- lst
  }
  if (!is.list(x)) cli::cli_abort("{.arg style} must be a list or a Stata style string.", call = NULL)
  bad <- setdiff(names(x), names(out))
  if (length(bad) || (length(x) && is.null(names(x)))) {
    cli::cli_abort("Unknown {.arg style} field{?s}: {.field {bad}}.", call = NULL)
  }
  for (key in c("titlerowheight", "noterowheight")) {
    v <- x[[key]]
    if (is.null(v)) next
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0) {
      cli::cli_abort("{.arg style}: {.field {key}} requires a positive number.", call = NULL)
    }
    out[[key]] <- as.numeric(v)
  }
  if (!is.null(x$colwidth)) {
    w <- x$colwidth
    if (!is.numeric(w) || is.null(names(w)) || anyNA(w) || any(w <= 0)) {
      cli::cli_abort("{.arg style}: {.field colwidth} must be positive widths named by column letter.", call = NULL)
    }
    vapply(names(w), .stacktab_col_id, 0L, what = "style")
    out$colwidth <- w
  }
  out
}

.stacktab_colwidth_index <- function(w) {
  if (!length(w)) return(numeric())
  stats::setNames(unname(w), vapply(names(w), .stacktab_col_id, 0L, what = "style"))
}

# borders(): outer(all), top(row 1), bottom(last), bottom(row #)
# (stacktab.ado:1363-1383, :1514-1532).
.stacktab_borders <- function(x, nrows) {
  out <- list(outer = FALSE, top = FALSE, bottom = FALSE, bottom_rows = integer())
  if (is.null(x)) return(out)
  if (!is.character(x) || anyNA(x)) cli::cli_abort("{.arg borders} must be a character vector.", call = NULL)
  s <- tolower(gsub("\"", "", paste(x, collapse = " "), fixed = TRUE))
  out$outer <- grepl("outer(all)", s, fixed = TRUE)
  out$top <- grepl("top(row 1)", s, fixed = TRUE)
  out$bottom <- grepl("bottom(last)", s, fixed = TRUE)
  rest <- s
  for (tok in c("outer(all)", "top(row 1)", "bottom(last)")) rest <- gsub(tok, "", rest, fixed = TRUE)
  rows <- as.integer(sub("^bottom\\(row ([0-9]+)\\)$", "\\1",
                         regmatches(rest, gregexpr("bottom\\(row [0-9]+\\)", rest))[[1]]))
  rest <- gsub("bottom\\(row [0-9]+\\)", "", rest)
  rest <- trimws(gsub(",", " ", rest, fixed = TRUE))
  if (nzchar(rest)) {
    cli::cli_abort(c("{.arg borders}: unrecognised token{?s}: {.val {rest}}.",
                     "i" = "Recognised: {.val outer(all)}, {.val top(row 1)}, {.val bottom(last)}."), call = NULL)
  }
  # tabtools 2.1.12: a rule below row k for k in 1..N, anything else refused
  # (2.1.11 drew it only for k = N).
  if (any(rows < 1L | rows > nrows)) {
    cli::cli_abort("{.arg borders}: {.code bottom(row k)} needs k between 1 and the number of rows ({nrows}).",
                   call = NULL)
  }
  out$bottom_rows <- sort(unique(rows))
  out
}

# ---------------------------------------------------------------------------
# Blocks

.stacktab_load <- function(path) {
  tryCatch(openxlsx2::wb_load(path), error = function(e) {
    cli::cli_abort("Could not read the workbook {.file {path}}.", parent = e, call = NULL)
  })
}

# The used range of a sheet as a character matrix with its sheet row numbers
# and column indices (Stata's `import excel ..., allstring`: from the first
# to the last used row and column, empty cells as "").
# `exact`: block sheets are matched as Stata's `import excel, sheet()`
# matches them, case-sensitively (review F10); the output sheet is found
# without regard to case, as _stacktab_xlsx_sheet_bounds does.
.stacktab_read_sheet <- function(wb, sheet, exact = FALSE) {
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  hit <- which(sheets == sheet)
  if (!length(hit) && !exact) hit <- which(tolower(sheets) == tolower(sheet))
  if (!length(hit)) return(NULL)
  d <- tryCatch(openxlsx2::wb_to_df(wb, sheet = unname(sheets[hit[1]]), col_names = FALSE,
                                    skip_empty_rows = FALSE, skip_empty_cols = FALSE,
                                    convert = FALSE),
                error = function(e) NULL)
  if (is.null(d) || !nrow(d) || !ncol(d)) {
    return(list(cells = matrix("", 0L, 0L), rows = integer(), cols = integer(), sheet = unname(sheets[hit[1]])))
  }
  m <- as.matrix(d)
  storage.mode(m) <- "character"
  m[is.na(m)] <- ""
  dimnames(m) <- NULL
  list(cells = m, rows = as.integer(rownames(d)), cols = .xlsx_col_index(colnames(d)),
       sheet = unname(sheets[hit[1]]))
}

.stacktab_sheet_info <- function(wb, sheet) {
  s <- .stacktab_read_sheet(wb, sheet)
  if (is.null(s)) return(NULL)
  list(sheet = s$sheet, rows = if (length(s$rows)) max(s$rows) else 0L)
}

.stacktab_block_grid <- function(b, k, wb) {
  keys <- NULL
  if (!is.null(b$table)) {
    tt <- .tt_blank_text(validate_tt_table(b$table))
    g <- .tt_cells(tt, extraspace = TRUE)
    keys <- c(rep(NA_character_, length(tt$header)), .tt_row_keys(tt))
    where <- "its table"
    rr <- seq_len(nrow(g))
    cc <- seq_len(ncol(g))
    if (!is.null(b$rows)) {
      if (b$rows[2] > nrow(g)) {
        cli::cli_abort("Block {k}: {.field rows} {b$rows[1]}/{b$rows[2]} lies outside {where} ({nrow(g)} rows).", call = NULL)
      }
      rr <- seq.int(b$rows[1], b$rows[2])
    }
    if (!is.null(b$cols)) {
      if (b$cols[2] > ncol(g)) {
        cli::cli_abort("Block {k}: {.field cols} lies outside {where} ({ncol(g)} columns).", call = NULL)
      }
      cc <- seq.int(b$cols[1], b$cols[2])
    }
    g <- g[rr, cc, drop = FALSE]
    keys <- keys[rr]
  } else {
    s <- .stacktab_read_sheet(wb, b$sheet, exact = TRUE)
    fail <- function(why) {
      cli::cli_abort(c("Could not import block {k} from sheet {.val {b$sheet}}.", "x" = why), call = NULL)
    }
    if (is.null(s)) fail("The workbook has no such sheet (sheet names are matched with case, as in Stata).")
    g <- s$cells
    if (!is.null(b$rows) && !is.null(b$cols)) {
      # A cell range: sheet coordinates inside the used range.
      if (!nrow(g) || b$rows[1] < min(s$rows) || b$rows[2] > max(s$rows) ||
          b$cols[1] < min(s$cols) || b$cols[2] > max(s$cols)) {
        fail("The requested cell range extends beyond the sheet's used range.")
      }
      g <- g[match(seq.int(b$rows[1], b$rows[2]), s$rows), match(seq.int(b$cols[1], b$cols[2]), s$cols), drop = FALSE]
    } else if (!is.null(b$rows)) {
      # Sheet rows (tabtools 2.1.12, stacktab.ado:239-274: the used range
      # imported whole, then keep if inrange(_n + first_row - 1, lo, hi)).
      keep <- if (nrow(g)) min(s$rows) + seq_len(nrow(g)) - 1L else integer()
      g <- g[keep >= b$rows[1] & keep <= b$rows[2], , drop = FALSE]
    } else if (!is.null(b$cols)) {
      if (!nrow(g) || b$cols[1] < min(s$cols) || b$cols[2] > max(s$cols)) {
        cli::cli_abort("Block {k}: {.field cols} not found in sheet {.val {b$sheet}}.", call = NULL)
      }
      g <- g[, match(seq.int(b$cols[1], b$cols[2]), s$cols), drop = FALSE]
    }
  }
  if (!is.null(b$skip) && b$skip >= 1 && b$skip == round(b$skip) && b$skip <= nrow(g)) {
    g <- g[-b$skip, , drop = FALSE]
    keys <- keys[-b$skip]
  }
  if (!nrow(g) || !ncol(g)) {
    src <- if (is.null(b$sheet)) "a table" else paste0("sheet ", b$sheet)
    cli::cli_abort("Block {k} ({src}) imported 0 rows.", call = NULL)
  }
  if (!is.null(b$label)) g[1, 1] <- b$label
  if (!is.null(b$postfix)) g[, 1] <- paste0(g[, 1], " ", b$postfix)
  attr(g, "keys") <- keys
  g
}

# Row keys of a tt_table's body rows, for the hstack check only (nothing is
# joined on them): regtab's term keys, table1's variable (with the level or
# row label), else the trimmed row label.
.tt_row_keys <- function(x) {
  n <- nrow(x$body)
  key <- rep(NA_character_, n)
  rr <- x$meta$regtab_rows
  if (is.data.frame(rr) && all(c("row", "key") %in% names(rr))) {
    ok <- !is.na(rr$row) & !is.na(rr$key) & nzchar(rr$key) & rr$row <= n
    first <- !duplicated(rr$row[ok])
    key[rr$row[ok][first]] <- rr$key[ok][first]
  }
  lab <- trimws(x$body[[1]])
  v <- x$rows$var
  if (!is.null(v)) {
    lv <- x$rows$level %||% rep(NA_character_, n)
    vk <- ifelse(is.na(v), NA_character_, paste0(v, ":", ifelse(is.na(lv), lab, lv)))
    key[is.na(key)] <- vk[is.na(key)]
  }
  key[is.na(key)] <- lab[is.na(key)]
  key
}

# hstack joins rows by position, as Stata does. When two tt_table blocks
# disagree on the row at a position, warn and name the rows (review F7);
# the alignment stays positional.
.stacktab_check_keys <- function(grids) {
  keyed <- Filter(Negate(is.null), lapply(grids, attr, "keys"))
  if (length(keyed) < 2L) return(invisible())
  km <- do.call(cbind, keyed)
  bad <- which(apply(km, 1L, function(r) {
    r <- r[!is.na(r)]
    length(r) > 1L && length(unique(r)) > 1L
  }))
  if (!length(bad)) return(invisible())
  what <- vapply(bad, function(i) sprintf("row %d (%s)", i, paste(km[i, !is.na(km[i, ])], collapse = " / ")), "")
  cli::cli_warn(c("The side-by-side tables' rows differ at {length(bad)} position{?s}: {utils::head(what, 5)}{if (length(what) > 5) ', ...'}.",
                  "i" = "hstack places rows by position, as Stata's stacktab does; it does not match rows by key or label."),
                call = NULL)
  invisible()
}

# Positional stacking: vstack appends rows (narrower blocks padded with ""),
# `spacing` blank rows after every block but the last, each block's first
# row a section row; hstack joins the blocks on the row position.
.stacktab_stack <- function(grids, layout, spacing) {
  if (layout == "vstack") {
    nc <- max(vapply(grids, ncol, 0L))
    pad <- function(g) cbind(g, matrix("", nrow(g), nc - ncol(g)))
    parts <- list()
    sections <- integer()
    at <- 0L
    for (k in seq_along(grids)) {
      g <- pad(.stacktab_unkey(grids[[k]]))
      sections <- c(sections, at + 1L)
      if (spacing > 0L && k < length(grids)) g <- rbind(g, matrix("", spacing, nc))
      parts[[k]] <- g
      at <- at + nrow(g)
    }
    return(list(cells = do.call(rbind, parts), sections = sections))
  }
  n <- vapply(grids, nrow, 0L)
  if (any(n != n[1])) cli::cli_abort("{.code hstack} blocks must have the same row count.", call = NULL)
  .stacktab_check_keys(grids)
  list(cells = do.call(cbind, lapply(grids, .stacktab_unkey)), sections = integer())
}

.stacktab_unkey <- function(g) {
  attr(g, "keys") <- NULL
  g
}

.stacktab_merge <- function(cells, ids, m, sections) {
  ja <- match(m$a, ids)
  jb <- match(m$b, ids)
  miss <- c(m$a, m$b)[is.na(c(ja, jb))]
  if (length(miss)) {
    col <- .xlsx_col(miss[1])
    cli::cli_abort(c("{.arg columnmerge} {.val {m$text}}: column {.val {col}} is not in the composite.",
                     "i" = "The composite has {length(ids)} column{?s}."), call = NULL)
  }
  b <- cells[, jb]
  use <- nzchar(b) & b != "."
  cells[use, ja] <- paste(cells[use, ja], b[use])
  cells[if (length(sections)) sections else 1L, ja] <- m$header
  list(cells = cells[, -jb, drop = FALSE], ids = ids[-jb])
}

# ---------------------------------------------------------------------------
# The composite as a tt_table

.stacktab_table <- function(cells, sections, title, note, meta, stored) {
  nc <- ncol(cells)
  # Column widths from every composite row (stacktab.ado:478-495):
  # ceil(max byte length x 0.95) + 2, the first column [14, 45], the others
  # [10, 24].
  w <- vapply(seq_len(nc), function(j) ceiling(max(.blen(cells[, j])) * 0.95) + 2, 0)
  w <- ifelse(seq_len(nc) == 1L, pmin(pmax(w, 14), 45), pmin(pmax(w, 10), 24))
  meta$widths <- w
  meta$section_rows <- sections
  body <- cells[-1L, , drop = FALSE]
  cols <- data.frame(role = c("label", rep("value", nc - 1L)), model = NA_integer_,
                     # Stata's list shows a column at least 2 wide.
                     console_width = 2L, stringsAsFactors = FALSE)
  rows <- data.frame(type = rep("var", nrow(body)), indent = rep(0L, nrow(body)),
                     section = (seq_len(nrow(body)) + 1L) %in% sections)
  layout <- list(indent = 0L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "plain", xlsx_rules = "stacktab",
                 sheet = "Composite")
  tt_table(body = body, header = list(list(text = cells[1L, ])), rows = rows, cols = cols,
           title = title, footnote = note,
           style = tt_resolve_style(font = "Arial", fontsize = 10, borderstyle = "thin"),
           stored = stored, command = "stacktab", layout = layout, meta = meta)
}

# ---------------------------------------------------------------------------
# Workbook

# stacktab's worksheet (stacktab.ado:676-715 and _stacktab_apply_style):
# the composite from column B at `start_row`, the title in column A of
# `title_row`, the note below the table. tt_write_xlsx() uses start row 2,
# title row 1 (a new or replaced sheet).
.xlsx_layout_stacktab <- function(x, start_row = 2L, title_row = 1L) {
  comp <- .tt_cells(x)
  meta <- x$meta
  nrc <- nrow(comp)
  sc <- 2L
  end_row <- start_row + nrc - 1L
  end_col <- sc + ncol(comp) - 1L
  has_title <- nzchar(x$title)
  has_note <- nzchar(x$footnote)
  note_row <- end_row + 1L
  nr <- max(end_row + has_note, if (has_title) title_row else 0L)
  grid <- matrix("", nr, end_col)
  written <- matrix(FALSE, nr, end_col)
  rr <- seq.int(start_row, end_row)
  cc <- seq.int(sc, end_col)
  grid[rr, cc] <- comp
  written[rr, cc] <- TRUE

  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  add("font", start_row, end_row, sc, end_col, value = 10)
  add("wrap", start_row, end_row, sc, end_col, code = 1)
  add("valign", start_row, end_row, sc, end_col, code = 2)
  add("bold", start_row, start_row, sc, end_col, code = 1)
  add("bottom", start_row, start_row, sc, end_col, code = 1)
  add("bottom", end_row, end_row, sc, end_col, code = 1)
  for (s in meta$section_rows[meta$section_rows >= 1L & meta$section_rows <= nrc]) {
    r <- start_row + s - 1L
    add("bold", r, r, sc, end_col, code = 1)
    add("top", r, r, sc, end_col, code = 1)
    add("bottom", r, r, sc, end_col, code = 1)
  }
  w <- meta$widths %||% rep(10, ncol(comp))
  for (j in seq_along(w)) add("width", 1, 1, sc + j - 1L, sc + j - 1L, value = w[j])
  cw <- meta$colwidth %||% numeric()
  for (k in seq_along(cw)) {
    col <- sc + as.integer(names(cw)[k]) - 1L
    add("width", 1, 1, col, col, value = cw[[k]])
  }
  b <- meta$borders %||% list()
  if (isTRUE(b$outer)) {
    add("top", start_row, start_row, sc, end_col, code = 1)
    add("bottom", end_row, end_row, sc, end_col, code = 1)
    add("left", start_row, end_row, sc, sc, code = 1)
    add("right", start_row, end_row, end_col, end_col, code = 1)
  }
  if (isTRUE(b$top)) add("top", start_row, start_row, sc, end_col, code = 1)
  if (isTRUE(b$bottom)) add("bottom", end_row, end_row, sc, end_col, code = 1)
  for (k in b$bottom_rows %||% integer()) add("bottom", start_row + k - 1L, start_row + k - 1L, sc, end_col, code = 1)
  if (has_title) {
    grid[title_row, 1] <- x$title
    written[title_row, seq_len(end_col)] <- TRUE
    add("merge", title_row, title_row, 1, end_col)
    add("font", title_row, title_row, 1, end_col, value = 12)
    add("wrap", title_row, title_row, 1, end_col, code = 1)
    add("valign", title_row, title_row, 1, end_col, code = 2)
    add("bold", title_row, title_row, 1, end_col, code = 1)
    add("height", title_row, title_row, 1, 1, value = meta$title_height %||% 30)
  }
  if (has_note) {
    grid[note_row, sc] <- x$footnote
    written[note_row, cc] <- TRUE
    if (end_col > sc) add("merge", note_row, note_row, sc, end_col)
    add("font", note_row, note_row, sc, end_col, value = 8)
    add("wrap", note_row, note_row, sc, end_col, code = 1)
    add("valign", note_row, note_row, sc, end_col, code = 2)
    add("italic", note_row, note_row, sc, end_col, code = 1)
    add("height", note_row, note_row, 1, 1, value = meta$note_height %||% 45)
  }
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}

.stacktab_write_xlsx <- function(tt, path, sheet, start_row, title_row, append) {
  lay <- .xlsx_layout_stacktab(tt, start_row = start_row, title_row = title_row)
  st <- .xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), tt$style)
  tryCatch(suppressWarnings(.xlsx_write(path, sheet, lay$grid, lay$written, st, append = append)),
           error = function(e) {
             cli::cli_abort("Could not write the workbook for sheet {.val {sheet}}.", parent = e, call = NULL)
           })
  invisible(path)
}

# Copy the staged files into place; if one copy fails, put back the files
# already replaced (and remove the ones that did not exist). Codex audit
# CX-1: every existing target is backed up, and each backup verified,
# before the first target is replaced; the target whose copy failed is
# restored too (a partial copy may have overwritten it); and the error says
# which files could not be restored, keeping their backups, rather than
# claiming a rollback that did not happen.
.stacktab_commit <- function(staged) {
  if (!length(staged)) return(invisible())
  dests <- names(staged)
  backup <- tempfile("stacktab-backup-")
  dir.create(backup)
  keep_backup <- FALSE
  on.exit(if (!keep_backup) unlink(backup, recursive = TRUE), add = TRUE)
  had <- file.exists(dests)
  saved <- file.path(backup, seq_along(dests))
  for (i in which(had)) .tt_copy_checked(dests[i], saved[i], what = NULL)
  done <- integer()
  for (i in seq_along(dests)) {
    ok <- .tt_file_copy(staged[[i]], dests[i], overwrite = TRUE)
    if (!isTRUE(ok)) {
      # The failed target too: a partial copy may have overwritten it.
      restore <- function(j) {
        if (had[j]) return(.tt_file_copy(saved[j], dests[j], overwrite = TRUE) && .tt_same_file(saved[j], dests[j]))
        if (file.exists(dests[j])) unlink(dests[j])
        !file.exists(dests[j])
      }
      back <- c(done, i)
      lost <- dests[back][!vapply(back, restore, TRUE)]
      if (length(lost)) {
        keep_backup <- TRUE
        cli::cli_abort(c("Could not write {.file {dests[i]}}, and could not restore {.file {lost}}.",
                         "i" = "The original files are kept in {.file {backup}}."), call = NULL)
      }
      cli::cli_abort("Could not write {.file {dests[i]}}; every target was restored to its state before the call.",
                     call = NULL)
    }
    done <- c(done, i)
  }
  invisible()
}
