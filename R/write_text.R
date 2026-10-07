# Text renderers: CSV, Markdown, and the console listing (tasks 1.8, 1.9).

# ---------------------------------------------------------------------------
# CSV

# Stata `export delimited ..., novarnames` quotes a field only when it holds
# the delimiter or a double quote, doubling embedded quotes; leading and
# trailing spaces are written bare (checked against Stata 17, 2026-09-25).
.csv_field <- function(x) {
  x <- .csv_defuse(x)
  q <- grepl("[,\"\r\n]", x)
  x[q] <- paste0("\"", gsub("\"", "\"\"", x[q], fixed = TRUE), "\"")
  x
}

# Spreadsheets run a CSV cell that starts with = + - @ (or a tab or CR) as
# a formula, so `=HYPERLINK(...)` or `+cmd|...` in a label would run when
# the file is opened (Muse audit P0-4). Such a cell gets a leading `'`. A
# cell starting with + or - is left alone unless it holds one of | ! @ = "
# ' & (a DDE payload or a quoted argument), or a letter other than an
# exponent's e right after the sign's first character is not a blank: so
# negative estimates, CIs, the "-" placeholder and model labels such as
# "+ Comorbidities" are written as Stata writes them. Stata's `export
# delimited` does no such escaping; the xlsx sink needs none (its cells are
# strings, never formulas).
.csv_defuse <- function(x) {
  signed <- grepl("^[+-]", x)
  bad <- grepl("^[=@\t\r]", x) |
    (signed & grepl("[|!@=\"'&]", x)) |
    (signed & grepl("^[+-][^ ]", x) & grepl("[A-DF-Za-df-z]", x))
  x[bad] <- paste0("'", x[bad])
  x
}

# Errors name the sink and the file (Milestone H, H20), not only R's
# "cannot open the connection". The connection is closed inside the checked
# path (Codex audit CX-2): a small write stays buffered until close(), and a
# full device reports ENOSPC only there, as a warning, so the writer used to
# return normally with nothing written. A warning or error from writing or
# closing is an error naming the target; the on.exit() handler only closes
# a connection an error or interrupt left open.
.write_lines_lf <- function(lines, path, append = FALSE, arg = "path") {
  fail <- function(e) {
    cli::cli_abort("Could not write the {.arg {arg}} target {.file {path}}.", parent = e, call = NULL)
  }
  # Verify the exact bytes before a caller records successful sink history.
  previous <- if (append && file.exists(path)) tryCatch(readBin(path, "raw", file.size(path)), error = fail) else raw()
  expected <- c(previous, charToRaw(paste0(paste(enc2utf8(lines), collapse = "\n"),
    if (length(lines)) "\n" else "")))
  con <- tryCatch(suppressWarnings(file(path, if (append) "ab" else "wb")), error = fail)
  is_open <- TRUE
  on.exit(if (is_open) try(close(con), silent = TRUE))
  cond <- tryCatch({
    writeLines(enc2utf8(lines), con, sep = "\n", useBytes = TRUE)
    # No flush(): R ignores fflush()'s result, and glibc then drops the
    # buffer, so close() would no longer see the failure.
    is_open <- FALSE
    close(con)
    NULL
  }, warning = function(w) w, error = function(e) e)
  if (!is.null(cond)) fail(cond)
  actual <- tryCatch(readBin(path, "raw", file.size(path)), error = fail)
  if (!identical(actual, expected)) {
    cli::cli_abort("Could not verify the {.arg {arg}} target {.file {path}}.", call = NULL)
  }
  invisible(path)
}

#' Write a tt_table as CSV
#'
#' Rows are the title (if any), the header rows, the body, and the footnote
#' paragraphs (if any), one row per paragraph; title and footnote sit in the first column with empty trailing
#' fields (`_tabtools_csv_write.ado`). Indents are kept. UTF-8, LF line
#' endings.
#'
#' A cell a spreadsheet would run as a formula (starting with `=`, `@`, a
#' tab or a carriage return, or with `+`/`-` followed by a function name or
#' a `|`, as in `=HYPERLINK(...)` or `+cmd|...`) is written with a leading
#' `'`, so opening the file cannot run it. Numbers, intervals, `"-"` and
#' labels such as `"+ Comorbidities"` are written as they are. Stata's
#' `export delimited` does not do this.
#'
#' @param x A tt_table.
#' @param path Output file; must end in `.csv`.
#' @return `path`, invisibly.
#' @examples
#' d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20)
#' tab <- table1_tc(d, by = "arm", vars = c(x = "contn"))
#' path <- tempfile(fileext = ".csv")
#' tt_write_csv(tab, path)
#' readLines(path)
#' @section Session destinations:
#' Writes only the explicitly named path; session defaults never add another
#' sink. CSV has no session destination. See [tabtools_options()].
#'
#' @export
tt_write_csv <- function(x, path) {
  .tt_check_table(x)
  .tt_check_csv_path(path, arg = "path")
  .tt_resolve_sinks(list(csv = path), list(csv = TRUE), policy = "writer")
  x <- .tt_blank_text(x)
  g <- .tt_grid(x)
  lines <- apply(g, 1L, function(r) paste(.csv_field(r), collapse = ","))
  .write_lines_lf(lines, path, arg = "path")
  invisible(path)
}

# ---------------------------------------------------------------------------
# Markdown

# _tt_md_escape (_tabtools_markdown_write.ado, tabtools 2.1.12): trim
# blanks, then backslash-escape the backslash, pipe, asterisk, underscore,
# backtick, <, >, &, [, ] and ~ (in that order, so an inserted backslash is
# never escaped again); line breaks become <br> afterwards, so the <br> and
# the &nbsp; indents the writer adds stay live. `dollar = TRUE` (.qmd/.rmd
# targets, R only) escapes "$" too.
.md_escape <- function(x, dollar = FALSE) {
  x <- gsub("^ +| +$", "", x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  for (ch in c("|", "*", "_", "`", "<", ">", "&", "[", "]", "~", if (dollar) "$")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x <- gsub("\r\n", "<br>", x, fixed = TRUE)
  x <- gsub("\r", "<br>", x, fixed = TRUE)
  gsub("\n", "<br>", x, fixed = TRUE)
}

# Label-column body cell: each leading space becomes &nbsp;, prefixed after
# escaping (D10; _tt_md_body_cell).
.md_label_cell <- function(x, dollar = FALSE) {
  n <- nchar(x) - nchar(sub("^ +", "", x))
  core <- .md_escape(x, dollar)
  ifelse(nzchar(core), paste0(strrep("&nbsp;", n), core), "")
}

# GFM allows one header row. table1_tc flattens its two header rows into
# "Group (N=...)" with the descriptor over the label column
# (desctab.ado:2072-2106); regtab prefixes each statistic header with its
# model label, "Model: 95% CI" (regtab.ado:2874-2903). A table without a
# header row (puttab noheader) writes an empty one, as Stata's `novarnames`
# does. `escape = FALSE` returns the flattened text itself (puttab() of a
# tt_table uses it as its one header row).
.md_header <- function(x, escape = TRUE, dollar = FALSE) {
  h <- x$header
  nc <- ncol(x$body)
  esc <- if (escape) function(v) .md_escape(v, dollar) else identity
  if (!length(h)) return(rep("", nc))
  if (length(x$meta$puttab_spans)) return(esc(.puttab_md_header(x)))
  g <- .tt_cells(x, extraspace = TRUE)
  # layout$md_header = "first" (comptab's rate mode): the first header row
  # as it is, blank cells included (Stata's `strictheaders`).
  if (identical(x$layout$md_header, "first")) return(esc(g[1, ]))
  # layout$md_header = "joined" (stratetab, tabtools 2.1.14,
  # stratetab.ado:692-708): the last header row, each column of a group
  # prefixed with the group's label, "outcome: statistic", untrimmed; the
  # label column keeps its own text.
  if (identical(x$layout$md_header, "joined")) {
    out <- g[length(h), ]
    grp <- x$cols$model
    for (m in unique(grp[!is.na(grp)])) {
      js <- which(grp == m)
      out[js] <- paste0(g[1, js[1]], ": ", out[js])
    }
    return(esc(out))
  }
  for (k in seq_along(h)) h[[k]]$text <- g[k, ]
  if (x$layout$header_style == "descriptor") {
    if (length(h) < 2L) return(esc(h[[1]]$text))
    h1 <- h[[1]]$text
    h2 <- h[[2]]$text
    out <- h2
    for (j in seq_len(nc)) {
      a <- trimws(h1[j], "both", "[ ]")
      b <- trimws(h2[j], "both", "[ ]")
      if (identical(a, b)) b <- ""
      if (nzchar(a) && nzchar(b)) out[j] <- paste0(a, " (", b, ")")
      else if (nzchar(a)) out[j] <- a
    }
    role <- x$cols$role
    out[role == "p" & !nzchar(trimws(out))] <- "p-value"
    out[role == "smd" & !nzchar(trimws(out))] <- "SMD"
    return(esc(out))
  }
  last <- h[[length(h)]]$text
  out <- last
  if (identical(x$meta$regtab_orientation, "transpose")) {
    first <- h[[1L]]$text
    out <- ifelse(nzchar(first) & nzchar(last), paste0(first, ": ", last), ifelse(nzchar(first), first, last))
    return(esc(out))
  }
  if (x$layout$header_style == "model" && length(h) >= 2L) {
    first <- h[[1]]$text
    model <- x$cols$model
    for (m in unique(model[!is.na(model)])) {
      js <- which(model == m)
      name <- trimws(first[js[1]], "both", "[ ]")
      if (!nzchar(name)) next
      for (j in js) {
        s <- trimws(last[j], "both", "[ ]")
        out[j] <- if (nzchar(s)) paste0(name, ": ", s) else name
      }
    }
  }
  esc(out)
}

# Header rows the Markdown header stands for: all of them, or only the first
# under layout$md_header = "first", whose later header rows are written as
# body rows (comptab's rate mode calls the writer with its default
# headerstart(2) datastart(3), so its second header row is data; stratetab
# did so too up to tabtools 2.1.13).
.md_nhead <- function(x) {
  if (!length(x$header)) return(0L)
  if (identical(x$layout$md_header, "first")) 1L else length(x$header)
}

#' Write a tt_table as GitHub-Flavored Markdown
#'
#' `### title` and a blank line only when a title is given; one header row;
#' `| --- |` separator; body cells trimmed and escaped, label indents written
#' as `&nbsp;`; rows blank in every column skipped (kept for a [puttab()]
#' table, whose rows are all data); the footnote as
#' one `*paragraph*` per paragraph, each after a blank line. With `append = TRUE` (Stata `mdappend`)
#' an existing file gains a blank line and the new table; otherwise an
#' existing file is replaced.
#'
#' **Escaping.** As in Stata's writer (`_tt_md_escape()`,
#' `_tabtools_markdown_write.ado`, tabtools 2.1.12), cell, header, title and
#' footnote text is literal: after trimming blanks, the backslash, `|`,
#' `*`, `_`, the backtick, `<`, `>`, `&`, `[`, `]`, and `~` are each
#' preceded by a backslash (a backslash before ASCII punctuation is a
#' literal in CommonMark/GFM), and line breaks become `<br>`. So
#' `<b>x</b>`, `&lt;5`, `[a](b)`, `~~x~~` and a backtick pair show as
#' typed in a Markdown viewer instead of bold text, `<5`, a link, struck
#' text and code. Only the writer's own `<br>` and `&nbsp;` indents stay
#' live. The output is byte-identical to Stata's (probed with
#' `puttab, markdown()` on tabtools 2.1.12).
#' For `.qmd` and `.rmd` targets, `$` is also escaped so Pandoc shows it
#' literally instead of interpreting a pair as mathematics.
#'
#' An existing file is replaced unless `append = TRUE`, as in Stata
#' tabtools 2.1.12 (2.1.11 stopped with r(602)).
#'
#' @param x A tt_table.
#' @param path Output file (`.md`, `.markdown`, `.qmd`, or `.rmd`).
#' @param append Append to an existing file. The commands' Stata option
#'   `mdappend` does the same; the writer takes the R name, as [cat()] does
#'   (see "Argument names" in [tabtools-package]).
#' @return `path`, invisibly, with attributes `n_rows` and `n_cols`.
#' @examples
#' d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20,
#'                 g = factor(rep(c("u", "v"), 10)))
#' tab <- table1_tc(d, by = "arm", vars = c(x = "contn", g = "cat"))
#' path <- tempfile(fileext = ".md")
#' tt_write_markdown(tab, path)
#' tt_write_markdown(tab, path, append = TRUE)
#' cat(readLines(path), sep = "\n")
#' @section Session destinations:
#' Writes only the explicitly named path; session defaults never add another
#' sink. A successful write to an active session destination records its history
#' for later inherited writes. Omitted `append` retains replacement behavior.
#' See [tabtools_options()].
#'
#' @export
tt_write_markdown <- function(x, path, append = FALSE) {
  .tt_check_table(x)
  x <- .tt_blank_text(x)
  view <- .puttab_md_view(x)
  x <- view$table
  .tt_check_path(path, "\\.(md|markdown|qmd|rmd)$", "markdown", "a .md, .markdown, .qmd, or .rmd file")
  .tt_resolve_sinks(list(markdown = path, mdappend = append),
                    list(markdown = TRUE, mdappend = !missing(append)), policy = "writer")
  existing <- append && file.exists(path)
  # Pandoc reads a .qmd/.rmd file with tex_math_dollars, so a "$" pair there
  # is typeset as math: those targets escape "$" as "\\$" (R only; a .md
  # file keeps Stata's escaping, _tabtools_markdown_write.ado; audit A02).
  dollar <- grepl("\\.(qmd|rmd)$", tolower(path))
  lines <- character()
  # The blank line before an appended table; a file whose last line has no
  # newline needs one more line, or the table joins that paragraph (audit
  # A05).
  if (existing) lines <- if (.md_ends_open(path)) c("", "") else ""
  if (nzchar(x$title)) lines <- c(lines, paste0("### ", .md_escape(x$title, dollar)), "")
  hdr <- .md_header(x, dollar = dollar)
  lines <- c(lines, paste0("|", paste0(" ", hdr, " |", collapse = "")))
  lines <- c(lines, paste0("|", strrep(" --- |", length(hdr))))
  b <- .tt_cells(x, extraspace = TRUE)
  if (length(x$header)) b <- b[-seq_len(.md_nhead(x)), , drop = FALSE]
  n_body <- 0L
  # A row blank in every column is a structural spacer and is skipped, except
  # in a puttab table, whose rows are all data: Stata's `keepblank` (since
  # Stata-Tools 68c37a90, codex audit C3; task C7), so the Markdown has as
  # many rows as the workbook and the CSV.
  keep_blank <- identical(x$command, "puttab") || isTRUE(x$layout$md_keep_blank)
  for (i in seq_len(nrow(b))) {
    cells <- c(.md_label_cell(b[i, 1], dollar), .md_escape(b[i, -1], dollar))
    if (!keep_blank && !any(nzchar(cells))) next
    if (i %in% view$bold_rows) {
      nonblank <- nzchar(cells)
      cells[nonblank] <- paste0("**", cells[nonblank], "**")
    }
    lines <- c(lines, paste0("|", paste0(" ", cells, " |", collapse = "")))
    n_body <- n_body + 1L
  }
  for (para in .tt_footnote_paragraphs(x$footnote)) {
    lines <- c(lines, "", paste0("*", .md_escape(para, dollar), "*"))
  }
  .write_lines_lf(lines, path, append = existing, arg = "path")
  .tt_mark_sink(path, "markdown")
  invisible(structure(path, n_rows = n_body, n_cols = length(hdr)))
}

# TRUE when the file is non-empty and its last byte is not a newline. A
# file that cannot be read (write-only; appended to all the same, codex
# audit CX-1) counts as ending in one.
.md_ends_open <- function(path) {
  n <- file.size(path)
  if (is.na(n) || n == 0) return(FALSE)
  last <- tryCatch({
    con <- suppressWarnings(file(path, "rb"))
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    seek(con, n - 1)
    readBin(con, "raw", 1L)
  }, error = function(e) as.raw(10L))
  !identical(last, as.raw(10L))
}

# ---------------------------------------------------------------------------
# Console

# Stata `list ..., noobs noheader table`: columns padded to their widest
# display width and separated by three spaces, one space inside each border
# (desctab.ado:1310-1314; _tabtools_common.ado:827-881).
tt_console_lines <- function(x) {
  x <- .tt_blank_text(x)
  g <- rbind(do.call(rbind, lapply(x$header, function(h) h$text)), as.matrix(x$body))
  nh <- length(x$header)
  lay <- x$layout
  # stratetab.ado:879-886 blanks the lower console header's first cell
  # while retaining both Exposure labels in the frame and workbook.
  if (identical(x$command, "stratetab") && nh == 2L) g[2L, 1L] <- ""
  # layout$console_skip_blank_header (effecttab): a header row blank in
  # every column is not listed.
  if (isTRUE(lay$console_skip_blank_header) && nh) {
    blank <- which(vapply(seq_len(nh), function(k) all(!nzchar(trimws(g[k, ]))), TRUE))
    if (length(blank)) {
      g <- g[-blank, , drop = FALSE]
      nh <- nh - length(blank)
    }
  }
  right <- lay$align == "right"
  w <- apply(g, 2L, function(col) max(.dwidth(col)))
  cw <- x$cols$console_width %||% rep(NA_integer_, length(w))
  w <- pmax(w, ifelse(is.na(cw), 0L, cw))
  pad <- function(s, width) {
    fill <- strrep(" ", pmax(width - .dwidth(s), 0L))
    if (right) paste0(fill, s) else paste0(s, fill)
  }
  row_line <- function(r) {
    paste0("  | ", paste(mapply(pad, r, w), collapse = "   "), " |")
  }
  inner <- sum(w) + 3L * (length(w) - 1L) + 2L
  rule <- paste0("  |", strrep("-", inner), "|")
  edge <- paste0("  +", strrep("-", inner), "+")
  out <- character()
  if (lay$console_title && nzchar(x$title)) out <- c(out, "", x$title)
  out <- c(out, x$meta$console_before, edge)
  # sepby: header rows each form their own group, then one group per
  # variable block.
  grp <- if (lay$console_sepby) c(-seq_len(nh), x$rows$block) else rep(0L, nrow(g))
  for (i in seq_len(nrow(g))) {
    if (i > 1L && grp[i] != grp[i - 1L]) out <- c(out, rule)
    out <- c(out, row_line(g[i, ]))
  }
  out <- c(out, edge, x$notes)
  if (lay$console_blank) out <- c(out, "")
  # Every sink receives the same footnote paragraphs, after the listing.
  for (para in .tt_footnote_paragraphs(x$footnote)) out <- c(out, para, "")
  out
}
