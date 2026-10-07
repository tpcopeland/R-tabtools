# Transitional W02 adapter: authentic native table output plus an independently
# asserted R publication footer. No production note assembly/rendering helpers
# supply expected text. WP-2B owns helper-footnote-parity.R.

# Helper-owned I/O: each suite can run before test-golden-harness.R.
golden_write_lf <- function(lines, path) {
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(lines, con, useBytes = TRUE)
  invisible(path)
}

# Independent RFC4180 literal encoding, separate from production CSV helpers.
# Only punctuation requiring quoting changes an otherwise literal paragraph.
golden_footer_csv_records <- function(paragraphs, columns) {
  vapply(paragraphs, function(text) {
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    escaped <- paste0(ifelse(chars == intToUtf8(34L), paste0(chars, chars), chars), collapse = "")
    if (any(chars %in% c(",", intToUtf8(34L), "\n", "\r"))) {
      escaped <- paste0(intToUtf8(34L), escaped, intToUtf8(34L))
    }
    paste0(escaped, strrep(",", columns - 1L))
  }, "", USE.NAMES = FALSE)
}

golden_footer_bytes <- function(path, body_records) {
  bytes <- golden_bytes(path)
  newlines <- which(bytes == as.raw(10L))
  if (length(newlines) < body_records) return(raw())
  start <- if (body_records) newlines[body_records] + 1L else 1L
  if (start > length(bytes)) raw() else bytes[seq.int(start, length(bytes))]
}

golden_csv_expected_bytes <- function(records) {
  if (!length(records)) return(raw())
  charToRaw(paste0(paste(records, collapse = "\n"), "\n"))
}

# A mutation adapter can emit several successful checks and several failures.
# Capture every result, reject unexpected errors, and assert a detected fault.
golden_expect_detected <- function(expr) {
  reporter <- testthat::ListReporter$new()
  reporter$start_file("golden-mutation")
  # The child test boundary installs its expectation handler under this
  # reporter, so a fault never leaks into the enclosing positive-control test.
  tryCatch(
    testthat::with_reporter(reporter,
      testthat::test_that("golden mutation expression", force(expr))),
    finally = reporter$end_file()
  )
  results <- reporter$get_results()
  events <- unlist(lapply(results, function(x) x$results), recursive = FALSE)
  failures <- Filter(function(x) inherits(x, "expectation_failure"), events)
  errors <- Filter(function(x) inherits(x, "expectation_error"), events)
  testthat::expect_length(results, 1L)
  testthat::expect_length(errors, 0L)
  testthat::expect_true(length(failures) > 0L,
    info = paste(vapply(events, function(x) if (length(x$message)) x$message else class(x)[1L], ""), collapse = "\n"))
  invisible(failures)
}

# Independent literal contract authenticated by native T18 csv/md/console/xlsx.
golden_assert_current_smd <- function(tt) {
  note <- "SMD compares Primary vs Secondary only (the first two of 3 groups)."
  col <- which(tt$cols$role == "smd")
  testthat::expect_length(col, 1L)
  testthat::expect_identical(unname(tt$header[[1L]]$text[col]), "SMD (Primary vs Secondary)")
  testthat::expect_identical(tt$meta$console_before, paste("Note:", note))
  testthat::expect_identical(golden_fn_paragraphs(tt$footnote), note)
  invisible(tt)
}

golden_publication_contract <- function(id) {
  if (startsWith(id, "demo/")) return(golden_demo_publication_contract(id))
  sc <- golden_scenario(id)
  user_raw <- golden_fn_user(sc)
  user <- golden_fn_paragraphs(user_raw)
  native_grid <- golden_read_cells(id)
  native_styles <- golden_cell_styles(golden_book(id), id)
  automatic <- list()
  native_note <- if (length(user)) paste(user_raw, collapse = " \\ ") else character()
  # Native small-cell note is its independent oracle; the fixed source prefix
  # identifies this particular automatic note, never an arbitrary final row.
  if (sc$command == "table1_tc" && grepl("smallcells(", sc$stata_call, fixed = TRUE)) {
    note <- native_grid[nrow(native_grid), 1L]
    stopifnot(startsWith(note, "Counts below "))
    automatic <- list(list(text = note, native = note,
      source = "authentic native small-cell note; desctab.ado:139-148"))
    native_note <- note
  }
  # The Stata reference joins ESS/truncation inline. R publishes each automatic
  # annotation separately. These literals come from the reference source, not
  # .wt_footnote() or the actual returned table.
  if (sc$command == "wttab" && is.null(user_raw)) {
    ess <- "ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N."
    trunc <- paste0("Truncated l/u: weights below the l-th or above the u-th percentile of all weights",
                    " set to that percentile; Truncated (n) counts the weights changed.")
    has_trunc <- grepl("trunc(", sc$stata_call, fixed = TRUE)
    texts <- c(ess, if (has_trunc) trunc)
    automatic <- lapply(texts, function(text) list(text = text, native = text,
      source = "literal native reference constants; qa/stata/golden_wttab.do"))
    native_note <- paste(texts, collapse = " ")
  }
  if (identical(id, "T18")) {
    note <- "SMD compares Primary vs Secondary only (the first two of 3 groups)."
    automatic <- list(list(text = note, native = note, source = "native 2.5.1 T18 csv/md/console/xlsx, c215afdb"))
    native_note <- note
  }
  native_sources <- list(csv = as.vector(native_grid), xlsx = native_styles$value,
    console = golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt"))))
  c <- golden_footnote_contract(id, automatic = automatic, native_sources = native_sources,
                                scenario = sc)
  # The pinned source appends star legends only in the workbook. Its exact
  # punctuation join is authenticated independently of the R paragraphs.
  xnote <- native_note
  if (length(c$stars)) {
    xnote <- if (!length(native_note)) c$stars else {
      join <- if (grepl("[.;:!?]$", native_note)) " " else "; "
      paste0(native_note, join, c$stars)
    }
  }
  console_note <- character()
  if (length(native_note) && (length(automatic) && sc$command == "table1_tc" ||
      sc$command == "hrcomptab" || sc$command == "comptab" && grepl("golden_strate_blocks", sc$r_call, fixed = TRUE))) {
    console_note <- native_note
  }
  c$native_footers <- list(csv = native_note, markdown = golden_fn_md(native_note),
                           console = console_note, xlsx = xnote)
  c$native_grid <- native_grid
  c$native_styles <- native_styles
  c$native_layout <- golden_sheet_layout(golden_book(id), id)
  c$grid_end <- nrow(native_grid) - length(native_note)
  c$sheet_end <- max(native_styles$row) - length(xnote)
  c
}

golden_footer_rows <- function(n, end) if (n > end) seq.int(end + 1L, n) else integer()

# Export commands print only file chatter during the call. Their returned
# table's public console listing still owes the all-sink paragraph contract.
golden_assert_listing_footer <- function(tt, id) {
  c <- golden_publication_contract(id)
  lines <- golden_console_box(utils::capture.output(print(tt)))
  edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
  if (length(edge) < 2L) stop("Publication footer needs a complete boxed listing.", call. = FALSE)
  tail <- lines[golden_footer_rows(length(lines), tail(edge, 1L))]
  golden_assert_footnote_tail(tail[nzchar(trimws(tail))], c, "console")
  invisible(c)
}

golden_publication_cells <- function(tt, id) {
  c <- golden_publication_contract(id)
  g <- golden_as_cells(tt)
  w <- c$native_grid
  gr <- golden_footer_rows(nrow(g), c$grid_end)
  wr <- golden_footer_rows(nrow(w), c$grid_end)
  golden_assert_footnote_tail(g[gr, 1L], c, "csv")
  golden_assert_native_footnote_tail(w[wr, 1L], c, "csv")
  testthat::expect_true(all(g[gr, -1L, drop = FALSE] == ""), label = "complete CSV footer trailing cells")
  testthat::expect_true(all(w[wr, -1L, drop = FALSE] == ""), label = "native CSV footer trailing cells")
  list(got = g[seq_len(min(nrow(g), c$grid_end)), , drop = FALSE],
       want = w[seq_len(c$grid_end), , drop = FALSE], contract = c)
}

# Both sides are split at the last closing box, not at a guessed tail length.
# Every nonblank line after that boundary is an annotation and is asserted.
golden_publication_console <- function(got, want, id) {
  c <- golden_publication_contract(id)
  split <- function(lines) {
    lines <- golden_console_box(lines)
    edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
    if (length(edge) < 2L) stop("Publication footer needs a complete boxed listing.", call. = FALSE)
    end <- tail(edge, 1L)
    foot <- lines[golden_footer_rows(length(lines), end)]
    list(body = lines[seq_len(end)], tail = foot[nzchar(trimws(foot))])
  }
  g <- split(got); w <- split(want)
  golden_assert_footnote_tail(g$tail, c, "console")
  golden_assert_native_footnote_tail(w$tail, c, "console")
  list(got = g$body, want = w$body)
}

# Footer decomposition retains exact original byte contracts: LF line endings
# and a final newline are checked before deriving any body-only scratch file.
golden_publication_sink <- function(path, id, ext, mask, tt = NULL) {
  c <- golden_publication_contract(id)
  want <- golden_artifact_path(id, paste0(id, ".", ext))
  for (file in c(path, want)) {
    bytes <- golden_bytes(file)
    testthat::expect_false(any(bytes == as.raw(13L)), label = paste(ext, "LF endings"))
    testthat::expect_identical(tail(bytes, 1L), as.raw(10L), label = paste(ext, "final newline"))
  }
  g <- golden_read_lines(path); w <- golden_read_lines(want)
  if (ext == "csv") {
    grid <- golden_read_cells_file(path)
    gr <- golden_footer_rows(nrow(grid), c$grid_end)
    wr <- golden_footer_rows(nrow(c$native_grid), c$grid_end)
    golden_assert_footnote_tail(grid[gr, 1L], c, "csv")
    golden_assert_native_footnote_tail(c$native_grid[wr, 1L], c, "csv")
    testthat::expect_true(all(grid[gr, -1L, drop = FALSE] == ""))
    testthat::expect_true(all(c$native_grid[wr, -1L, drop = FALSE] == ""))
    expected_g <- golden_footer_csv_records(c$paragraphs, ncol(c$native_grid))
    expected_w <- golden_footer_csv_records(c$native_footers$csv, ncol(c$native_grid))
    testthat::expect_identical(g[golden_footer_rows(length(g), c$grid_end)], expected_g,
                               label = "exact R CSV footer quoting and empty fields")
    testthat::expect_identical(w[golden_footer_rows(length(w), c$grid_end)], expected_w,
                               label = "exact native CSV footer quoting and empty fields")
    testthat::expect_identical(golden_footer_bytes(path, c$grid_end), golden_csv_expected_bytes(expected_g),
                               label = "exact R CSV footer bytes")
    testthat::expect_identical(golden_footer_bytes(want, c$grid_end), golden_csv_expected_bytes(expected_w),
                               label = "exact native CSV footer bytes")
    # Existing scenarios contain no literal newlines inside a CSV field.
    testthat::expect_identical(length(g), nrow(grid), label = "CSV record boundary")
    testthat::expect_identical(length(w), nrow(c$native_grid), label = "native CSV record boundary")
    g <- g[seq_len(min(length(g), c$grid_end))]
    w <- w[seq_len(c$grid_end)]
  } else {
    native_tail <- c$native_footers$markdown
    end <- length(w) - 2L * length(native_tail)
    wt <- w[golden_footer_rows(length(w), end)]
    gt <- g[golden_footer_rows(length(g), end)]
    golden_assert_native_footnote_tail(wt[nzchar(wt)], c, "markdown")
    golden_assert_footnote_tail(gt[nzchar(gt)], c, "markdown")
    testthat::expect_identical(wt, if (length(native_tail)) as.vector(rbind("", native_tail)) else character(), label = "native complete Markdown footer geometry")
    testthat::expect_identical(gt, if (length(c$paragraphs)) as.vector(rbind("", golden_fn_md(c$paragraphs))) else character(), label = "R complete Markdown footer geometry")
    g <- g[seq_len(min(length(g), end))]; w <- w[seq_len(end)]
  }
  out <- withr::local_tempdir()
  gp <- file.path(out, paste0("actual.", ext)); wp <- file.path(out, paste0("native.", ext))
  golden_write_lf(g, gp); golden_write_lf(w, wp)
  golden_compare_sink(gp, wp, mask,
    p_rows = if (!is.null(tt$rows$vtype)) golden_grid_p_rows(tt),
    p_body = if (!is.null(tt$rows$vtype)) golden_p_masked_rows(tt))
}

# The standard layouts serialize only the merged footer anchor in R
# (written-cell rules in R/write_xlsx.R, R/stratetab.R and R/comptab.R);
# native xl() also
# serializes blank default-format merge children. The anchor determines the
# displayed merged cell. Both address sets and all attributes are asserted
# separately; any visible formatting on an omitted child refuses equivalence.
golden_footer_style_templates <- function(contract, id) {
  native <- contract$native_styles[contract$native_styles$row == contract$sheet_end + 1L, , drop = FALSE]
  command <- if (!is.null(contract$command)) contract$command else golden_scenario(id)$command
  # A first-row template is valid only after every native paragraph proves
  # the same immutable anchor style, merge shape and explicit row height.
  footers <- contract$native_styles[contract$native_styles$row > contract$sheet_end, , drop = FALSE]
  rows <- sort(unique(footers$row))
  anchors <- footers[match(paste0("B", rows), footers$address), , drop = FALSE]
  testthat::expect_false(anyNA(anchors$address), label = paste(id, "every native footer anchor"))
  for (attr in setdiff(c(golden_style_attrs, "number_format", "format_id"), "value")) {
    testthat::expect_identical(anchors[[attr]], rep(anchors[[attr]][1L], length(rows)),
      label = paste(id, "uniform native paragraph anchor", attr))
  }
  merge_end <- as.integer(sub("^.*[A-Z]+([0-9]+)$", "\\1", contract$native_layout$merges))
  merges <- contract$native_layout$merges[merge_end > contract$sheet_end]
  expected_merges <- if (length(merges)) vapply(rows, function(row)
    gsub("[0-9]+", as.character(row), merges[1L]), "") else character()
  testthat::expect_identical(sort(merges), sort(expected_merges),
    label = paste(id, "uniform native paragraph merge geometry"))
  heights <- contract$native_layout$heights[contract$native_layout$heights$row > contract$sheet_end, , drop = FALSE]
  testthat::expect_identical(heights$row, if (nrow(heights)) rows else integer(),
    label = paste(id, "complete native paragraph height rows"))
  testthat::expect_identical(heights$height, if (nrow(heights)) rep(heights$height[1L], length(rows)) else numeric(),
    label = paste(id, "uniform native paragraph heights"))
  sparse <- command %in% c("table1_tc", "desctab", "regtab", "stratetab", "effecttab", "comptab", "hrcomptab")
  if (sparse) {
    footers <- contract$native_styles[contract$native_styles$row > contract$sheet_end, , drop = FALSE]
    children <- footers[footers$col != 2L, , drop = FALSE]
    default <- list(value = "", bold = FALSE, italic = FALSE, font = "Calibri", size = 11,
      number_format = "General", format_id = 1L, font_color = "", halign = "general", valign = "bottom", wrap = FALSE,
      border_top = NA_character_, border_bottom = NA_character_,
      border_left = NA_character_, border_right = NA_character_, fill = "")
    for (attr in names(default)) {
      testthat::expect_identical(children[[attr]], rep(default[[attr]], nrow(children)),
                                 label = paste(id, "lossless native merged child", attr))
    }
    rows <- sort(unique(footers$row))
    expected_merge <- if (nrow(children)) paste0("B", rows, ":", golden_col_letters(max(native$col)), rows) else character()
    merge_end <- as.integer(sub("^.*[A-Z]+([0-9]+)$", "\\1", contract$native_layout$merges))
    footer_merges <- contract$native_layout$merges[merge_end > contract$sheet_end]
    testthat::expect_identical(sort(footer_merges), sort(expected_merge),
                               label = paste(id, "complete correctly bounded native footer merges"))
    testthat::expect_true(all(children$address %in% golden_merge_nonanchors(footer_merges)),
                           label = paste(id, "native default children wholly within merges"))
  }
  list(native = native, R = if (sparse) native[native$col == 2L, , drop = FALSE] else native,
       halign = if (command == "stacktab") "general" else "left")
}

golden_publication_styles <- function(g, w, gl, wl, id) {
  c <- golden_publication_contract(id)
  end <- c$sheet_end
  gr <- golden_footer_rows(max(g$row), end)
  wr <- golden_footer_rows(max(w$row), end)
  anchors <- function(cells, rows) {
    if (!length(rows)) return(character())
    cells$value[match(paste0("B", rows), cells$address)]
  }
  golden_assert_footnote_tail(anchors(g, gr), c, "xlsx")
  golden_assert_native_footnote_tail(anchors(w, wr), c, "xlsx")
  templates <- if (length(gr) || length(wr)) golden_footer_style_templates(c, id) else NULL
  testthat::expect_identical(gr, seq.int(end + 1L, length.out = length(c$paragraphs)),
                             label = "complete R footer worksheet rows")
  testthat::expect_identical(wr, seq.int(end + 1L, length.out = length(c$native_footers$xlsx)),
                             label = "complete native footer worksheet rows")
  testthat::expect_true(all(g$col[g$row > end] <= max(w$col)), label = "no undeclared footer columns")
  testthat::expect_true(all(!nzchar(g$value[g$row > end & g$col != 2L])), label = "all nonanchor footer cells blank")
  testthat::expect_true(all(!nzchar(w$value[w$row > end & w$col != 2L])), label = "all native nonanchor footer cells blank")
  footer_merges <- function(merges) {
    if (!length(merges)) return(character())
    hi <- as.integer(sub("^.*[A-Z]+([0-9]+)$", "\\1", merges))
    merges[hi > end]
  }
  for (rows in list(list(cells = g, rows = gr, side = "R"), list(cells = w, rows = wr, side = "native"))) {
    for (row in rows$rows) {
      actual_row <- rows$cells[rows$cells$row == row, , drop = FALSE]
      template <- templates[[rows$side]]
      expected_addresses <- paste0(golden_col_letters(template$col), row)
      testthat::expect_identical(sort(actual_row$address), sort(expected_addresses),
                                 label = paste(rows$side, "complete footer address multiplicity", row))
      testthat::expect_identical(sort(actual_row$col), sort(template$col),
                                 label = paste(rows$side, "complete footer column multiplicity", row))
      actual_row <- actual_row[match(expected_addresses, actual_row$address), , drop = FALSE]
      for (attr in setdiff(c(golden_style_attrs, "number_format", if (rows$side == "native") "format_id"), "value")) {
        testthat::expect_identical(actual_row[[attr]], template[[attr]],
                                   label = paste(rows$side, "complete immutable footer style", row, attr))
      }
    }
  }
  wm <- footer_merges(c$native_layout$merges)
  testthat::expect_identical(footer_merges(wl$merges), wm, label = "authenticated native footer merges")
  gm <- footer_merges(gl$merges)
  if (length(gr)) {
    # Native template supplies the command's historical note styling and
    # explicit height. Each R paragraph must repeat that exact template.
    source <- c$native_styles[c$native_styles$row == end + 1L & c$native_styles$col == 2L, , drop = FALSE]
    testthat::expect_identical(nrow(source), 1L)
    if (nrow(source) != 1L) stop("No authentic workbook footer style template.", call. = FALSE)
    columns <- seq.int(2L, max(w$col))
    golden_assert_footnote_styles(g, gr, columns, source$size + 2, source$font, halign = templates$halign)
    a <- g[match(paste0("B", gr), g$address), , drop = FALSE]
    for (attr in setdiff(c(golden_style_attrs, "number_format"), "value")) {
      testthat::expect_identical(a[[attr]], rep(source[[attr]], length(gr)),
                                label = paste("independent footer", attr))
    }
    expected_merges <- if (length(wm)) {
      vapply(gr, function(row) gsub("[0-9]+", as.character(row), wm[1L]), "")
    } else character()
    testthat::expect_identical(sort(gm), sort(expected_merges), label = "complete footer merges")
  } else testthat::expect_identical(gm, character(), label = "no undeclared footer merges")
  wh <- c$native_layout$heights[c$native_layout$heights$row > end, , drop = FALSE]
  supplied_native_height <- wl$heights[wl$heights$row > end, , drop = FALSE]
  testthat::expect_identical(supplied_native_height$row, wh$row, label = "authenticated native footer height rows")
  testthat::expect_identical(supplied_native_height$height, wh$height, label = "authenticated native footer heights")
  gh <- gl$heights[gl$heights$row > end, , drop = FALSE]
  testthat::expect_identical(gh$row, if (nrow(wh)) gr else integer(), label = "complete footer custom-height rows")
  testthat::expect_identical(gh$height, if (nrow(wh)) rep(wh$height[1L], length(gr)) else numeric(),
                             label = "exact footer heights")
  # All footer text, counts, cells, styles, merges and custom heights above
  # are asserted. Only these declared rows leave the strict native comparator.
  g <- g[g$row <= end, , drop = FALSE]; w <- w[w$row <= end, , drop = FALSE]
  gl$merges <- setdiff(gl$merges, gm); wl$merges <- setdiff(wl$merges, wm)
  gl$heights <- gl$heights[gl$heights$row <= end, , drop = FALSE]
  wl$heights <- wl$heights[wl$heights$row <= end, , drop = FALSE]
  list(g = g, w = w, gl = gl, wl = wl)
}
