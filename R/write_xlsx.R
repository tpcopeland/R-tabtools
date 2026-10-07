# Excel renderer (task 1.7). All openxlsx2 calls live in this file.
#
# Stata builds a list of style rules -- op r1 r2 c1 c2 value code rgb --
# and applies them in order through Mata xl() (_tabtools_xlsx_apply_styles
# .ado). The same model is used here: the rule list is built per command
# (desctab.ado:1819-2017, regtab.ado:3041-3191), applied in order to a
# per-cell style state, and the final state is written with openxlsx2.
# Worksheet layout: title row 1 (A1, merged across), the table from B2,
# column A a 1-wide gutter.

# Rule op codes (_tabtools_xlsx_apply_styles.ado:100-157).
.OP <- c(font = 1L, bold = 2L, italic = 3L, wrap = 4L, halign = 5L, valign = 6L,
         fill = 7L, top = 8L, bottom = 9L, left = 10L, right = 11L,
         height = 12L, width = 13L, merge = 14L, fontcolor = 15L)

.rule <- function(op, r1, r2, c1, c2, value = 0, code = 0, color = NA_character_) {
  data.frame(op = .OP[[op]], r1 = r1, r2 = r2, c1 = c1, c2 = c2, value = value,
             code = code, color = color, stringsAsFactors = FALSE)
}

.border_name <- function(code) c("thin", "medium", "thick", NA_character_)[code]
.border_code <- function(style) match(style, c("thin", "medium", "thick"), nomatch = 1L)

#' Write a tt_table to an Excel worksheet with the tabtools house style
#'
#' Replaces a same-named sheet (case-insensitive, keeping its position) in
#' an existing workbook, or adds one; creates the workbook if needed
#' (`_tabtools_xlsx_write.ado`).
#'
#' @param x A tt_table.
#' @param path Output `.xlsx` file.
#' @param sheet Sheet name (Excel rules: at most 31 characters, none of
#'   the characters `\`, `/`, `?`, `*`, `[`, `]` and `:`, no leading or
#'   trailing apostrophe, not `History`).
#'   Defaults to the sheet the table was made with: its command's `sheet`
#'   argument (`"Table 1"` for [table1_tc()], `"Regression"` for
#'   [regtab()], ...).
#' @param open Open the file after writing (interactive sessions only; ignored
#'   otherwise, as in `R CMD check`).
#' @return `path`, invisibly.
#' @examples
#' d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20)
#' tab <- table1_tc(d, by = "arm", vars = c(x = "contn"))
#' path <- tempfile(fileext = ".xlsx")
#' tt_write_xlsx(tab, path, sheet = "Table 1")
#' tab$title <- "Table 1. Characteristics"
#' tt_write_xlsx(tab, path, sheet = "Table 1b")
#' @section Session destinations:
#' Writes only the explicitly named path; session defaults never add another
#' sink. A successful write to an active session destination records its history
#' for later inherited writes. See [tabtools_options()].
#'
#' @export
tt_write_xlsx <- function(x, path, sheet = NULL, open = FALSE) {
  .tt_check_table(x)
  x <- .tt_blank_text(x)
  .tt_check_path(path, "\\.xlsx$", "xlsx", "a .xlsx file")
  .tt_resolve_sinks(list(xlsx = path), list(xlsx = TRUE), policy = "writer")
  if (identical(x$layout$xlsx_rules, "none")) {
    cli::cli_abort(c("No Excel layout exists yet for {.val {x$command}} tables.",
                     "i" = "Set {.code layout$xlsx_rules} to {.val descriptive}, {.val regression}, {.val puttab}, {.val stacktab}, {.val stratetab}, {.val comptab}, or {.val hrcomptab}."),
                   call = NULL)
  }
  if (x$layout$xlsx_rules %in% c("descriptive", "regression", "stratetab", "comptab", "hrcomptab") &&
      (!nrow(x$body) || ncol(x$body) < 2L)) {
    # Stata refuses an empty export with r(2000).
    cli::cli_abort("Nothing to export: the table needs at least one body row and one value column.",
                   call = NULL)
  }
  if (!nrow(x$body) && !length(x$header)) {
    cli::cli_abort("Nothing to export: the table has no rows.", call = NULL)
  }
  sheet <- .check_sheet(sheet %||% x$meta$sheet %||% x$layout$sheet)
  lay <- switch(x$layout$xlsx_rules,
                regression = .xlsx_layout_regtab(x),
                descriptive = .xlsx_layout_table1(x),
                puttab = .xlsx_layout_puttab(x),
                stacktab = .xlsx_layout_stacktab(x),
                stratetab = .xlsx_layout_stratetab(x),
                comptab = .xlsx_layout_comptab(x),
                hrcomptab = .xlsx_layout_hrcomptab(x))
  st <- .xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), x$style)
  # A failed load or save names the sink and the file (H20), not only
  # openxlsx2's "Failed to save workbook"; the warnings that come with the
  # failure (e.g. "Permission denied") join the error instead of trailing it.
  warn <- character()
  tryCatch(
    withCallingHandlers(.xlsx_write(path, sheet, lay$grid, lay$written, st), warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      cli::cli_abort(c("Could not write the workbook {.file {path}}.",
                       stats::setNames(gsub("([{}])", "\\1\\1", warn), rep("x", length(warn)))),
                     parent = e, call = NULL)
    })
  for (w in warn) warning(w, call. = FALSE)
  .tt_mark_sink(path, "workbook")
  if (open && interactive()) utils::browseURL(path)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Rule application

.xlsx_apply_rules <- function(rules, nr, nc, style) {
  m <- function(v) matrix(v, nr, nc)
  st <- list(font = m(NA_character_), size = m(NA_real_), bold = m(FALSE),
             italic = m(FALSE), wrap = m(FALSE), halign = m(NA_character_),
             valign = m(NA_character_), fill = m(NA_character_),
             top = m(NA_character_), bottom = m(NA_character_),
             left = m(NA_character_), right = m(NA_character_),
             fcolor = m(NA_character_),
             widths = numeric(), heights = numeric(), merges = character())
  halign <- c("left", "center", "right")
  valign <- c("bottom", "center", "top")
  for (k in seq_len(nrow(rules))) {
    r <- rules[k, ]
    rr <- seq.int(r$r1, r$r2)
    cc <- seq.int(r$c1, r$c2)
    switch(as.character(r$op),
      "1" = { st$font[rr, cc] <- style$font; st$size[rr, cc] <- r$value },
      "2" = st$bold[rr, cc] <- r$code == 1,
      "3" = st$italic[rr, cc] <- r$code == 1,
      "4" = st$wrap[rr, cc] <- r$code == 1,
      "5" = st$halign[rr, cc] <- halign[r$code],
      "6" = st$valign[rr, cc] <- valign[r$code],
      "7" = st$fill[rr, cc] <- r$color,
      "8" = st$top[rr, cc] <- .border_name(r$code),
      "9" = st$bottom[rr, cc] <- .border_name(r$code),
      "10" = st$left[rr, cc] <- .border_name(r$code),
      "11" = st$right[rr, cc] <- .border_name(r$code),
      "12" = st$heights[as.character(rr)] <- r$value,
      "13" = st$widths[as.character(cc)] <- r$value,
      "14" = st$merges <- union(st$merges, .xlsx_ref(r$r1, r$c1, r$r2, r$c2)),
      "15" = { st$font[rr, cc] <- style$font; st$size[rr, cc] <- r$value; st$fcolor[rr, cc] <- r$color }
    )
  }
  st
}

.xlsx_col <- function(n) {
  vapply(n, function(k) {
    s <- ""
    while (k > 0) {
      r <- (k - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      k <- (k - 1L) %/% 26L
    }
    s
  }, "")
}

.xlsx_ref <- function(r1, c1, r2, c2) {
  paste0(.xlsx_col(c1), r1, ":", .xlsx_col(c2), r2)
}

# ---------------------------------------------------------------------------
# Writing

# `append = TRUE` (stacktab's append) writes into an existing sheet without
# clearing it; the cells above the new table keep their values and styles.
.xlsx_write <- function(path, sheet, grid, written, st, append = FALSE) {
  if (file.exists(path)) {
    wb <- openxlsx2::wb_load(path)
    sheets <- openxlsx2::wb_get_sheet_names(wb)
    hit <- which(tolower(sheets) == tolower(sheet))
    if (length(hit) && append) {
      sheet <- unname(sheets[hit[1]])
    } else if (length(hit)) {
      # Stata clears the existing sheet in place and keeps its name
      # (_tabtools_xlsx_write.ado:80-90): position, workbook-level names and
      # column widths beyond the new table survive; values, styles,
      # merges and row heights go (xl() clear_sheet(); probed 2026-09-26
      # with stacktab's sheetreplace: a replaced sheet kept no height).
      sheet <- unname(sheets[hit[1]])
      wb$clean_sheet(sheet = sheet, numbers = TRUE, characters = TRUE, styles = TRUE,
                     merged_cells = TRUE, hyperlinks = TRUE)
      .xlsx_clear_row_heights(wb, sheet)
    } else {
      wb$add_worksheet(sheet = sheet)
    }
  } else {
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet(sheet = sheet)
  }
  nr <- nrow(grid)
  nc <- ncol(grid)
  # Stata's xl() stores a cell holding only spaces as no-break spaces
  # (golden T33: the extraspace " " p cells read back as "\u00a0").
  blank <- grepl("^ +$", grid)
  grid[blank] <- gsub(" ", "\u00a0", grid[blank], fixed = TRUE)
  # Shared strings, as Stata writes them; in a session whose encoding is not
  # UTF-8 (R < 4.2 on Windows, LC_ALL=C), openxlsx2 1.29 gives a non-ASCII
  # string already in a loaded workbook's shared strings no index (<v>NA</v>,
  # an unreadable file; Milestone D review P2-4), so the cells are written
  # as inline strings there. Reported upstream as
  # https://github.com/JanMarvin/openxlsx2/issues/1690; drop this once a
  # fixed openxlsx2 is the minimum version.
  inline <- !isTRUE(l10n_info()[["UTF-8"]])
  for (i in seq_len(nr)) {
    js <- which(written[i, ])
    if (!length(js)) next
    runs <- split(js, cumsum(c(1L, diff(js) != 1L)))
    for (run in runs) {
      wb$add_data(sheet = sheet, x = as.data.frame(t(grid[i, run]), stringsAsFactors = FALSE),
                  start_row = i, start_col = run[1], col_names = FALSE, na = "",
                  inline_strings = inline)
    }
  }
  # One cell style per distinct combination of attributes.
  cells <- which(written, arr.ind = TRUE)
  key_font <- paste(st$font[cells], st$size[cells], st$bold[cells], st$italic[cells], st$fcolor[cells], sep = "|")
  key_border <- paste(st$top[cells], st$bottom[cells], st$left[cells], st$right[cells], sep = "|")
  key_fill <- st$fill[cells]
  key_xf <- paste(key_font, key_border, key_fill, st$halign[cells], st$valign[cells], st$wrap[cells], sep = "#")
  # wb is an R6 object: use its in-place methods (the wb_*() wrappers clone
  # it, which would leave styles_mgr pointing at a stale copy).
  mgr <- wb$styles_mgr
  font_ids <- list()
  border_ids <- list()
  fill_ids <- list()
  uid <- 0L
  for (key in unique(key_xf)) {
    k <- which(key_xf == key)[1]
    i <- cells[k, 1]
    j <- cells[k, 2]
    fk <- key_font[k]
    if (is.null(font_ids[[fk]])) {
      uid <- uid + 1L
      nm <- paste0("tt_font_", uid)
      fcol <- st$fcolor[i, j]
      mgr$add(openxlsx2::create_font(
        name = st$font[i, j] %|na|% "Arial", sz = as.character(st$size[i, j] %|na|% 10),
        b = if (st$bold[i, j]) "1" else "", i = if (st$italic[i, j]) "1" else "",
        color = if (is.na(fcol)) "" else openxlsx2::wb_color(hex = fcol)
      ), nm)
      font_ids[[fk]] <- mgr$get_font_id(nm)
    }
    bk <- key_border[k]
    if (is.null(border_ids[[bk]])) {
      uid <- uid + 1L
      nm <- paste0("tt_border_", uid)
      side <- function(v) if (is.na(v)) NULL else v
      mgr$add(openxlsx2::create_border(
        top = side(st$top[i, j]), bottom = side(st$bottom[i, j]),
        left = side(st$left[i, j]), right = side(st$right[i, j]),
        top_color = if (is.na(st$top[i, j])) NULL else openxlsx2::wb_color(hex = "FF000000"),
        bottom_color = if (is.na(st$bottom[i, j])) NULL else openxlsx2::wb_color(hex = "FF000000"),
        left_color = if (is.na(st$left[i, j])) NULL else openxlsx2::wb_color(hex = "FF000000"),
        right_color = if (is.na(st$right[i, j])) NULL else openxlsx2::wb_color(hex = "FF000000")
      ), nm)
      border_ids[[bk]] <- mgr$get_border_id(nm)
    }
    flk <- key_fill[k]
    fill_id <- NULL
    if (!is.na(flk)) {
      if (is.null(fill_ids[[flk]])) {
        uid <- uid + 1L
        nm <- paste0("tt_fill_", uid)
        mgr$add(openxlsx2::create_fill(pattern_type = "solid", fg_color = openxlsx2::wb_color(hex = flk)), nm)
        fill_ids[[flk]] <- mgr$get_fill_id(nm)
      }
      fill_id <- fill_ids[[flk]]
    }
    ha <- st$halign[i, j]
    va <- st$valign[i, j]
    wr <- st$wrap[i, j]
    uid <- uid + 1L
    nm <- paste0("tt_xf_", uid)
    mgr$add(openxlsx2::create_cell_style(
      font_id = font_ids[[fk]], border_id = border_ids[[bk]], fill_id = fill_id %||% "",
      horizontal = if (is.na(ha)) "" else ha,
      vertical = if (is.na(va)) "" else va,
      wrap_text = if (wr) "1" else ""
    ), nm)
    dims <- paste0(.xlsx_col(cells[key_xf == key, 2]), cells[key_xf == key, 1])
    wb$set_cell_style(sheet = sheet, dims = dims, style = mgr$get_xf_id(nm))
  }
  if (length(st$widths)) {
    o <- order(as.integer(names(st$widths)))
    wb$set_col_widths(sheet = sheet, cols = as.integer(names(st$widths))[o], widths = unname(st$widths)[o])
  }
  if (length(st$heights)) {
    o <- order(as.integer(names(st$heights)))
    wb$set_row_heights(sheet = sheet, rows = as.integer(names(st$heights))[o], heights = unname(st$heights)[o])
  }
  for (mg in st$merges) wb$merge_cells(sheet = sheet, dims = mg)
  .xlsx_save(wb, path)
}

# Save a workbook without destroying the file it replaces (Muse audit
# P1-39): write a temporary file beside it, then rename it into place, so
# a save that fails part-way (a full disk, a permission error) leaves the
# original untouched and no temporary file behind.
.xlsx_save <- function(wb, path) {
  tmp <- tempfile(pattern = paste0(sub("\\.xlsx$", "", basename(path), ignore.case = TRUE), ".tmp"),
                  tmpdir = dirname(path), fileext = ".xlsx")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  .xlsx_save_to(wb, tmp)
  if (!file.exists(tmp) || !isTRUE(file.size(tmp) > 0)) {
    cli::cli_abort("Could not write the workbook {.file {path}}.", call = NULL)
  }
  expected_size <- file.size(tmp)
  expected_hash <- unname(tools::md5sum(tmp))
  if (!isTRUE(suppressWarnings(file.rename(tmp, path)))) {
    # The fallback may overwrite before reporting failure, or report TRUE
    # after a truncated copy. Use the checked multi-target rollback path.
    .stacktab_commit(stats::setNames(tmp, path))
  }
  if (!identical(file.size(path), expected_size) || is.na(expected_hash) ||
      !identical(unname(tools::md5sum(path)), expected_hash)) {
    cli::cli_abort("Could not verify the saved workbook {.file {path}}.", call = NULL)
  }
  invisible(path)
}

.xlsx_save_to <- function(wb, file) wb$save(file, overwrite = TRUE)

`%|na|%` <- function(x, y) if (is.na(x)) y else x

# Explicit row heights of a sheet being replaced. openxlsx2's clean_sheet()
# keeps them; Stata's clear_sheet() does not, so a height the new table does
# not set (a wrapped header row of the old table, a stacktab note row) would
# otherwise survive.
.xlsx_clear_row_heights <- function(wb, sheet) {
  ra <- tryCatch(wb$worksheets[[wb$validate_sheet(sheet)]]$sheet_data$row_attr, error = function(e) NULL)
  if (!is.data.frame(ra) || !nrow(ra) || is.null(ra$ht)) return(invisible(wb))
  rows <- suppressWarnings(as.integer(ra$r[!is.na(ra$ht) & nzchar(ra$ht)]))
  rows <- rows[!is.na(rows)]
  if (length(rows)) wb$remove_row_heights(sheet = sheet, rows = rows)
  invisible(wb)
}

# ---------------------------------------------------------------------------
# table1_tc layout (desctab.ado:1632-1695, 1819-2017)

.xlsx_layout_table1 <- function(x) {
  b <- as.matrix(x$body)
  h1 <- x$header[[1]]$text
  h2 <- if (length(x$header) >= 2L) x$header[[2]]$text else rep("", ncol(b))
  role <- x$cols$role
  style <- x$style
  nb <- nrow(b)
  num_cols <- ncol(b) + 1L
  num_rows <- nb + 3L
  foot <- nzchar(x$footnote)
  nr <- num_rows + as.integer(foot)
  grid <- matrix("", nr, num_cols)
  grid[1, 1] <- x$title
  # The descriptor moves to B2 (merged B2:B3); the N row keeps the counts.
  # A one-row header has its label in the first row (Codex audit CX-5: it
  # was taken from the blank second row and lost).
  grid[2, 2] <- if (length(x$header) >= 2L) h2[1] else h1[1]
  grid[2, -(1:2)] <- h1[-1]
  grid[3, -(1:2)] <- h2[-1]
  if (nb) grid[4:num_rows, -1] <- .tt_cells(x, TRUE, FALSE)[-seq_along(x$header), , drop = FALSE]
  written <- matrix(FALSE, nr, num_cols)
  written[seq_len(num_rows), ] <- TRUE
  if (foot) {
    grid[nr, 2] <- x$footnote
    written[nr, 2] <- TRUE
  }

  pos <- function(r) which(role == r) + 1L
  p_pos <- pos("p")
  test_pos <- pos("test")
  stat_pos <- pos("statistic")
  smd_pos <- pos("smd")
  special <- c(p_pos, test_pos, stat_pos, smd_pos)
  data_cols <- setdiff(which(role %in% c("group", "total")) + 1L, special)

  # Label column width: byte length over every row but the header rows 2-3,
  # widened for a long descriptor, clamped to [15, 60].
  lab <- grid[seq_len(num_rows), 2]
  body_len <- .blen(lab[-(2:3)])
  factorwidth <- ceiling(max(c(body_len, 0)) * 0.85) + 2
  factor2width <- ceiling(max(.blen(lab[2:3])) * 0.85) + 2
  if (factor2width > factorwidth * 2) factorwidth <- factorwidth + (factor2width - factorwidth) / 2.5
  factorwidth <- min(max(factorwidth, 15), 60)

  hdr_lines_data <- 1L
  widths <- numeric()
  for (j in data_cols) {
    cw <- tt_colwidth(grid[seq_len(num_rows), j], firstrow = 3L, minwidth = 12, maxwidth = 30,
                      headerrow = 2L, headerfloor = 22)
    widths[as.character(j)] <- cw$width
    hdr_lines_data <- max(hdr_lines_data, cw$hlines)
  }
  text_width <- function(j, init) max(init, ceiling(max(init, .blen(grid[seq_len(num_rows), j])) * 0.85) + 2)

  hb <- .border_code(style$hborder)
  R <- list(.rule("height", 1, 1, 1, 1, value = 30))
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  hdr_len <- .blen(h2[1])
  hdr_lines <- 1L
  if (hdr_len > factorwidth * 1.2) hdr_lines <- ceiling(hdr_len / (factorwidth * 1.2))
  hdr_lines <- max(hdr_lines, hdr_lines_data)
  if (hdr_lines > 1) add("height", 2, 2, 1, 1, value = hdr_lines * 15)
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = factorwidth)
  for (j in data_cols) add("width", 1, 1, j, j, value = widths[[as.character(j)]])
  for (j in p_pos) add("width", 1, 1, j, j, value = 10)
  for (j in test_pos) add("width", 1, 1, j, j, value = text_width(j, 12))
  for (j in stat_pos) add("width", 1, 1, j, j, value = text_width(j, 14))
  # Stata hard-codes the SMD column width to 8 (desctab.ado:2203-2204,
  # "13 1 1 smd_pos smd_pos 8"); parity outranks fitting a long pair header.
  # A measured width is pending a Stata-side change.
  for (j in smd_pos) add("width", 1, 1, j, j, value = 8)

  add("font", 1, num_rows, 1, num_cols, value = style$fontsize)
  add("font", 1, 1, 1, num_cols, value = style$fontsize + 2)
  add("merge", 1, 1, 1, num_cols)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("bold", 1, 1, 1, 1, code = 1)
  hdr_cell <- function(j, merge) {
    if (merge) add("merge", 2, 3, j, j)
    add("halign", 2, 3, j, j, code = 2)
    add("valign", 2, 3, j, j, code = 2)
    add("wrap", 2, 3, j, j, code = 1)
    add("bold", 2, 3, j, j, code = 1)
  }
  hdr_cell(2, TRUE)
  for (j in setdiff(seq_len(num_cols)[-(1:2)], special)) hdr_cell(j, FALSE)
  for (j in c(p_pos, test_pos, stat_pos, smd_pos)) hdr_cell(j, TRUE)

  add("top", 2, 2, 2, num_cols, code = hb)
  add("top", 4, 4, 2, num_cols, code = hb)
  add("bottom", num_rows, num_rows, 2, num_cols, code = hb)
  academic <- style$borderstyle == "academic"
  if (!academic) {
    add("left", 2, num_rows, 2, 2, code = hb)
    add("right", 2, num_rows, 2, 2, code = hb)
    add("right", 2, num_rows, num_cols, num_cols, code = hb)
    for (j in pos("total")) {
      add("left", 2, num_rows, j, j, code = hb)
      add("right", 2, num_rows, j, j, code = hb)
    }
    for (j in c(p_pos, test_pos, stat_pos, smd_pos)) add("left", 2, num_rows, j, j, code = hb)
  }
  if (style$headershade) add("fill", 2, 3, 2, num_cols, color = style$headercolor)
  if (num_rows >= 4) add("halign", 4, num_rows, 3, num_cols, code = 2)
  if (style$zebra && num_rows >= 5) {
    for (r in seq.int(5, num_rows, by = 2)) add("fill", r, r, 2, num_cols, color = style$zebracolor)
  }
  p <- x$rows$p
  if (!is.na(style$boldp) && length(p_pos)) {
    for (i in which(!is.na(p) & p < style$boldp)) add("bold", i + 3, i + 3, p_pos, p_pos, code = 1)
  }
  if (!is.na(style$highlight) && length(p_pos)) {
    for (i in which(!is.na(p) & p < style$highlight)) {
      add("fill", i + 3, i + 3, 2, num_cols, color = style$highlightcolor)
    }
  }
  smd <- x$rows$smd
  if (length(smd_pos) && style$smdthreshold > 0) {
    # desctab.ado:861 compares abs(smd_val) with the threshold.
    for (i in which(!is.na(smd) & abs(smd) > style$smdthreshold)) {
      add("bold", i + 3, i + 3, smd_pos, smd_pos, code = 1)
      add("fill", i + 3, i + 3, smd_pos, smd_pos, color = style$smdcolor)
    }
  }
  if (foot) .xlsx_footnote_rules(add, nr, num_cols, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}

.xlsx_footnote_rules <- function(add, row, num_cols, style) {
  add("merge", row, row, 2, num_cols)
  add("halign", row, row, 2, 2, code = 1)
  add("valign", row, row, 2, 2, code = 2)
  add("wrap", row, row, 2, 2, code = 1)
  add("font", row, row, 2, 2, value = max(style$fontsize - 2, 6))
  add("italic", row, row, 2, 2, code = 1)
}

# ---------------------------------------------------------------------------
# regtab layout (regtab.ado:2818-2884, 3041-3191)

# regtab reads boldp/highlight from the displayed p text: "<0.001" counts as
# 0 (regtab.ado:3000-3011).
.p_from_text <- function(s) {
  s <- trimws(s)
  out <- suppressWarnings(as.numeric(sub("^>", "", s)))
  out[startsWith(s, "<")] <- 0
  out
}

# A hand-built tt_table (validate_tt_table() does not check a layout's
# structure) that lacks what an Excel layout indexes is refused with what
# the layout needs, not a base-R subscript error (audit A07). `need` may
# carry cli markup.
.tt_layout_needs <- function(rules, need) {
  cli::cli_abort(c(paste0("A {.val ", rules, "} Excel layout needs ", need, "."),
                   "i" = "Build the table with its command, or set {.code layout$xlsx_rules} to {.val puttab} for a plain layout."),
                 call = NULL)
}

# regtab/effecttab tables: at least one header row, and cols$model numbering
# each model's adjacent value columns, the same number for every model
# (estimate, CI and p-value columns, or estimate (CI) and p-value).
.tt_check_regression_shape <- function(x) {
  if (!length(x$header)) .tt_layout_needs("regression", "at least one header row")
  model <- x$cols$model
  role <- x$cols$role
  models <- sort(unique(model[!is.na(model)]))
  blocks <- lapply(models, function(m) which(model == m))
  size <- lengths(blocks)
  min_size <- if (any(role == "est_ci")) 1L else 2L
  ok <- length(models) > 0L && !1L %in% unlist(blocks) &&
    all(vapply(blocks, function(b) all(diff(b) == 1L), TRUE)) &&
    all(size == size[1]) && size[1] >= min_size
  # effecttab's widths read the two columns after each estimate.
  if (ok && identical(x$layout$width_rule, "effecttab")) {
    ok <- max(vapply(blocks, min, 1L)) + 2L <= ncol(x$body)
  }
  if (!ok) {
    .tt_layout_needs("regression", paste(
      "{.field cols$model} numbering each model's block of adjacent value columns,",
      "the same number for every model (estimate, CI and p-value columns, or estimate (CI) and p-value)"))
  }
  invisible(x)
}

.xlsx_layout_regtab <- function(x) {
  .tt_check_regression_shape(x)
  b <- as.matrix(x$body)
  role <- x$cols$role
  model <- x$cols$model
  style <- x$style
  meta <- x$meta
  refcat <- meta$refcat %||% "Reference"
  omitlabel <- meta$omitlabel %||% "Omitted"
  emptylabel <- meta$emptylabel %||% "Empty"
  labelwidth <- meta$labelwidth %||% 0
  if (labelwidth <= 0) labelwidth <- 45
  compact <- any(role == "est_ci")
  show_p <- any(role == "pval")
  nb <- nrow(b)
  num_cols <- ncol(b) + 1L
  num_rows <- nb + 3L
  fn_text <- meta$xlsx_footnote %||% x$footnote
  foot <- nzchar(fn_text)
  nr <- num_rows + as.integer(foot)
  grid <- matrix("", nr, num_cols)
  grid[1, 1] <- x$title
  grid[2, -1] <- x$header[[1]]$text
  grid[3, -1] <- x$header[[length(x$header)]]$text
  if (nb) grid[4:num_rows, -1] <- b
  written <- matrix(FALSE, nr, num_cols)
  written[seq_len(num_rows), ] <- TRUE
  if (foot) {
    grid[nr, 2] <- fn_text
    written[nr, 2] <- TRUE
  }
  models <- sort(unique(model[!is.na(model)]))
  cpm <- sum(model == models[1], na.rm = TRUE)
  first_col <- function(m) min(which(model == m)) + 1L

  # Per-model widths from each model's own cells (rows 3+).
  est_min <- if (compact) 10 else 7
  widths <- list()
  hdr_lines <- 1L
  effecttab_widths <- identical(x$layout$width_rule, "effecttab")
  for (m in models) {
    c1 <- first_col(m)
    col <- grid[seq_len(num_rows), c1]
    if (effecttab_widths) {
      # _tabtools_colwidth defaults, ceil(0.85 L) + 2, Reference cells
      # counted (effecttab.ado:1335-1348, X1).
      ew <- tt_colwidth(col, minwidth = 8, maxwidth = 22, headerrow = 2L)
      w <- c(est = ew$width,
             ci = tt_colwidth(grid[seq_len(num_rows), c1 + 1L], minwidth = 16, maxwidth = 34)$width,
             p = tt_colwidth(grid[seq_len(num_rows), c1 + 2L], minwidth = 8, maxwidth = 12)$width)
    } else {
      ew <- tt_colwidth(col, scale = 1, pad = -0.5, minwidth = est_min, headerrow = 2L,
                        exclude = c(refcat, omitlabel, emptylabel))
      w <- c(est = ew$width)
      if (!compact) w["ci"] <- tt_colwidth(grid[seq_len(num_rows), c1 + 1L], scale = 1, pad = -0.5, minwidth = 10)$width
      if (show_p) w["p"] <- tt_colwidth(grid[seq_len(num_rows), c1 + cpm - 1L], scale = 1, pad = -0.5, minwidth = 7)$width
    }
    widths[[m]] <- w
    hdr_lines <- max(hdr_lines, tt_colwidth(hlength = ew$hlen, blockwidth = sum(w))$hlines)
  }
  factor_length <- ceiling(max(.blen(grid[seq_len(num_rows), 2])) * 0.95) + 2
  # effecttab raises the label column to fit the effect header before the
  # cap (effecttab.ado:1355-1359, X2).
  if (effecttab_widths) factor_length <- max(factor_length, ceiling(.blen(meta$effect_label %||% "") * 0.85) + 2)
  factor_length <- min(factor_length, labelwidth)

  hb <- .border_code(style$hborder)
  vb <- .border_code(style$vborder)
  academic <- style$borderstyle == "academic"
  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  add("height", 1, 1, 1, 1, value = 30)
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = factor_length)
  for (m in models) {
    c1 <- first_col(m)
    w <- widths[[m]]
    add("width", 1, 1, c1, c1, value = w[["est"]])
    if (!compact) add("width", 1, 1, c1 + 1L, c1 + 1L, value = w[["ci"]])
    if (show_p) add("width", 1, 1, c1 + cpm - 1L, c1 + cpm - 1L, value = w[["p"]])
  }
  if (hdr_lines > 1) add("height", 2, 2, 1, 1, value = hdr_lines * 15)
  if (num_rows >= 4) {
    add("wrap", 4, num_rows, 2, 2, code = 1)
    add("valign", 4, num_rows, 2, num_cols, code = 3)
  }
  add("font", 1, num_rows, 1, num_cols, value = style$fontsize)
  add("font", 1, 1, 1, num_cols, value = style$fontsize + 2)
  add("merge", 1, 1, 1, num_cols)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("bold", 1, 1, 1, 1, code = 1)
  if (style$headershade) add("fill", 2, 3, 2, num_cols, color = style$headercolor)
  add("bold", 3, 3, 2, num_cols, code = 1)
  add("halign", 3, 3, 2, num_cols, code = 2)
  add("valign", 3, 3, 2, num_cols, code = 2)
  # Reference/omitted/empty labels merge across their own model's block only.
  for (m in models) {
    c1 <- first_col(m)
    c2 <- c1 + cpm - 1L
    for (r in which(grid[seq_len(num_rows), c1] %in% c(refcat, omitlabel, emptylabel) & seq_len(num_rows) >= 4L)) {
      add("merge", r, r, c1, c2)
      add("halign", r, r, c1, c1, code = 2)
      add("valign", r, r, c1, c1, code = 2)
      add("italic", r, r, c1, c1, code = 1)
    }
  }
  for (m in models) {
    c1 <- first_col(m)
    c2 <- c1 + cpm - 1L
    add("merge", 2, 2, c1, c2)
    add("halign", 2, 2, c1, c1, code = 2)
    add("valign", 2, 2, c1, c1, code = 2)
    add("bold", 2, 2, c1, c1, code = 1)
    add("wrap", 2, 2, c1, c1, code = 1)
    if (!academic) add("right", 2, num_rows, c2, c2, code = vb)
  }
  add("top", 2, 2, 2, num_cols, code = hb)
  add("top", 2, 2, 3, num_cols, code = hb)
  add("top", 3, 3, 3, num_cols, code = hb)
  add("bottom", 3, 3, 2, num_cols, code = hb)
  add("bottom", num_rows, num_rows, 2, num_cols, code = hb)
  if (!academic) {
    add("right", 2, num_rows, num_cols, num_cols, code = vb)
    add("left", 2, num_rows, 2, 2, code = vb)
    add("right", 2, num_rows, 2, 2, code = vb)
  }
  type <- x$rows$type
  re <- which(type == "re")
  if (length(re)) add("top", re[1] + 3, re[1] + 3, 2, num_cols, code = hb)
  for (kind in c("stat", "addrow")) {
    rows <- which(type == kind)
    if (!length(rows)) next
    add("top", rows[1] + 3, rows[1] + 3, 2, num_cols, code = hb)
    for (i in rows) {
      for (m in models) {
        c1 <- first_col(m)
        c2 <- c1 + cpm - 1L
        add("merge", i + 3, i + 3, c1, c2)
        add("halign", i + 3, i + 3, c1, c1, code = 2)
        add("valign", i + 3, i + 3, c1, c1, code = 2)
        if (!academic) add("right", i + 3, i + 3, c2, c2, code = vb)
      }
    }
  }
  if (style$zebra && num_rows >= 5) {
    for (r in seq.int(5, num_rows, by = 2)) add("fill", r, r, 2, num_cols, color = style$zebracolor)
  }
  if (num_rows >= 4) add("halign", 4, num_rows, 3, num_cols, code = 2)
  # p-values per model: engine-supplied (meta$pvals, needed under nopvalue)
  # or read from the displayed p columns.
  pv <- meta$pvals
  if (!is.null(pv) && !is.matrix(pv)) pv <- matrix(pv, ncol = length(models))
  if (is.null(pv) && show_p) {
    pv <- vapply(models, function(m) .p_from_text(b[, first_col(m) + cpm - 2L]), numeric(nb))
    pv <- matrix(pv, nb)
  }
  if (!is.null(pv) && nb && (!is.na(style$boldp) || !is.na(style$highlight))) {
    for (mi in seq_along(models)) {
      pcol <- first_col(models[mi]) + cpm - 1L
      for (i in seq_len(nb)) {
        pn <- pv[i, mi]
        if (is.na(pn)) next
        if (!is.na(style$boldp) && show_p && pn < style$boldp) add("bold", i + 3, i + 3, pcol, pcol, code = 1)
        if (!is.na(style$highlight) && pn < style$highlight) {
          add("fill", i + 3, i + 3, 2, num_cols, color = style$highlightcolor)
        }
      }
    }
  }
  if (style$dimnonsig) {
    for (i in which(x$rows$dim)) add("fontcolor", i + 3, i + 3, 2, num_cols, value = style$fontsize, color = style$dimcolor)
  }
  if (foot) .xlsx_footnote_rules(add, nr, num_cols, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}
