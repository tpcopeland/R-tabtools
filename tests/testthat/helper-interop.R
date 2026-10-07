# Helpers for the flextable/gt converter tests (task 6.1): build a golden
# scenario's table by running its r_call, and read Stata's own workbook for
# that scenario into table coordinates (header rows 1-2 = worksheet rows
# 2-3, body row i = worksheet row i + 3, column j = worksheet column j + 1),
# so the converted objects can be checked against the Stata-written golden.

interop_tt <- function(id, envir = parent.frame()) {
  sc <- golden_scenario(id)
  out <- withr::local_tempdir(.local_envir = envir)
  code <- gsub("@ID@", golden_code_path(out, id), sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  suppressMessages(eval(parse(text = code), envir = env))
}

interop_nbsp <- function(x) gsub("\u00a0", " ", x, fixed = TRUE)

# Stata's workbook for scenario `id`: per-cell expectations in table
# coordinates, the merged ranges (title and footnote merges dropped), the
# title, the footnote, and the column widths (Excel units, read-back offset
# removed).
interop_golden <- function(id, nbody, ncol) {
  sc <- golden_scenario(id)
  xlsx <- golden_book(id)
  w <- golden_cell_styles(xlsx, id)
  lay <- golden_sheet_layout(xlsx, id)
  last <- nbody + 3L
  mg <- tabtools:::.tt_parse_merges(lay$merges)
  mg <- mg[mg$r1 >= 2L & mg$r2 <= last & mg$c1 >= 2L, , drop = FALSE]
  # Stata "merges" one-column blocks cell by cell (C7:C7 under compact or
  # nopvalue, golden R52); a one-cell range is no merge, and flextable has
  # none to show.
  mg <- mg[!(mg$r1 == mg$r2 & mg$c1 == mg$c2), , drop = FALSE]
  cell <- function(r, c) {
    k <- which(w$row == r & w$col == c)
    if (length(k)) w[k[1], ] else NULL
  }
  grid <- expand.grid(r = 2:last, c = 2:(ncol + 1L))
  rows <- lapply(seq_len(nrow(grid)), function(k) {
    r <- grid$r[k]
    c <- grid$c[k]
    x <- cell(r, c)
    get <- function(a, default) if (is.null(x) || is.na(x[[a]])) default else x[[a]]
    data.frame(
      part = if (r <= 3L) "header" else "body", i = if (r <= 3L) r - 1L else r - 3L, j = c - 1L,
      r = r, c = c, value = interop_nbsp(get("value", "")),
      bold = isTRUE(get("bold", FALSE)), italic = isTRUE(get("italic", FALSE)),
      font = get("font", NA_character_), size = get("size", NA_real_),
      color = if (identical(get("font_color", ""), "FFA0A0A0")) "#A0A0A0" else "#000000",
      halign = if (get("halign", "general") %in% c("general", "left")) "left" else get("halign", ""),
      valign = get("valign", "bottom"),
      fill = if (nzchar(get("fill", ""))) paste0("#", substring(get("fill", ""), 3L)) else NA_character_,
      top = get("border_top", NA_character_), bottom = get("border_bottom", NA_character_),
      left = get("border_left", NA_character_), right = get("border_right", NA_character_),
      stringsAsFactors = FALSE)
  })
  cells <- do.call(rbind, rows)
  cells$anchor_of <- NA_integer_
  cells$nonanchor <- FALSE
  for (k in seq_len(nrow(mg))) {
    m <- mg[k, ]
    inside <- cells$r >= m$r1 & cells$r <= m$r2 & cells$c >= m$c1 & cells$c <= m$c2
    anchor <- cells$r == m$r1 & cells$c == m$c1
    cells$nonanchor[inside & !anchor] <- TRUE
    cells$anchor_of[anchor] <- k
  }
  merges <- data.frame(part = ifelse(mg$r2 <= 3L, "header", "body"),
                       i1 = ifelse(mg$r2 <= 3L, mg$r1 - 1L, mg$r1 - 3L),
                       i2 = ifelse(mg$r2 <= 3L, mg$r2 - 1L, mg$r2 - 3L),
                       j1 = mg$c1 - 1L, j2 = mg$c2 - 1L, stringsAsFactors = FALSE)
  title <- cell(1L, 1L)
  foot <- cell(last + 1L, 2L)
  widths <- lay$widths$width[match(seq_len(ncol) + 1L, lay$widths$col)] - golden_width_offset
  list(cells = cells, merges = merges, mg = mg,
       title = if (is.null(title)) "" else title$value,
       footnote = if (is.null(foot)) "" else interop_nbsp(foot$value),
       widths = widths, raw = w)
}

# Expected border of a merged anchor: the range's right/bottom edge, where
# Stata's rules put it.
interop_anchor_borders <- function(cells, mg) {
  for (k in which(!is.na(cells$anchor_of))) {
    m <- mg[cells$anchor_of[k], ]
    cells$right[k] <- cells$right[cells$r == m$r1 & cells$c == m$c2]
    cells$bottom[k] <- cells$bottom[cells$r == m$r2 & cells$c == m$c1]
  }
  cells
}

interop_border_rank <- c(thin = 1L, medium = 2L, thick = 3L)

# Heavier of two border names, NA meaning no rule.
interop_heavier <- function(a, b) {
  ra <- interop_border_rank[a]
  rb <- interop_border_rank[b]
  ifelse(!is.na(rb) & (is.na(ra) | rb > ra), b, a)
}

# The rule on each shared horizontal edge, given to the upper cell's bottom:
# Word keeps only that side of the edge (Phase 6 review P0-1), so
# as_flextable() moves every top rule below the first header row up there.
interop_shared_edges <- function(cells) {
  for (r in setdiff(sort(unique(cells$r)), min(cells$r))) {
    up <- which(cells$r == r - 1L)
    lo <- which(cells$r == r)
    lo <- lo[match(cells$c[up], cells$c[lo])]
    cells$bottom[up] <- interop_heavier(cells$bottom[up], cells$top[lo])
  }
  cells
}

# Cells whose p-value (and the bold/highlight styling that depends on it)
# comes from an R test that differs from Stata's (row-wise p mask, as the
# golden comparators use); the Test/Statistic columns under the test and
# statistic mask tokens (R names its tests differently; their widths follow
# the text); and the pinned rounding knife-edge cells of R13 and R14
# (regtab_known_divergences in test-golden-regtab.R, proven there).
interop_pinned <- list(R13 = c(6L, 2L), R14 = c(3L, 6L))

interop_p_masked <- function(tt, cells, id) {
  sc <- golden_scenario(id)
  mask <- golden_mask(sc$mask)
  rows <- which(golden_p_masked_rows(tt))
  pcol <- which(tt$cols$role == "p")
  heads <- golden_mask_headers[intersect(mask, c("test", "statistic"))]
  hd <- cells[cells$part == "header", ]
  tcols <- unique(hd$j[trimws(hd$value) %in% heads])
  value <- if ("p" %in% mask) cells$part == "body" & cells$i %in% rows & cells$j %in% pcol else rep(FALSE, nrow(cells))
  value <- value | (cells$part == "body" & cells$j %in% tcols)
  pin <- interop_pinned[[id]]
  if (!is.null(pin)) value <- value | (cells$part == "body" & cells$i == pin[1] & cells$j == pin[2])
  list(
    value = value,
    style = if ("pstyle" %in% mask) cells$part == "body" & cells$i %in% rows else rep(FALSE, nrow(cells)),
    pcol = pcol,
    width_cols = tcols
  )
}

interop_border_name <- function(width) {
  out <- rep(NA_character_, length(width))
  out[abs(width - 0.75) < 1e-6] <- "thin"
  out[abs(width - 1.5) < 1e-6] <- "medium"
  out[abs(width - 2.25) < 1e-6] <- "thick"
  out
}

# Per-cell view of a flextable, in the same shape as interop_golden()$cells.
interop_ft_cells <- function(ft) {
  cl <- flextable::information_data_cell(ft)
  ch <- flextable::information_data_chunk(ft)
  pa <- flextable::information_data_paragraph(ft)
  txt <- stats::aggregate(txt ~ .part + .row_id + .col_id, data = ch, FUN = paste, collapse = "")
  first <- ch[ch$.chunk_index == 1L, ]
  key <- function(d) paste(d$.part, d$.row_id, d$.col_id)
  k <- key(cl)
  out <- data.frame(
    part = as.character(cl$.part), i = cl$.row_id, j = as.integer(sub("^c", "", cl$.col_id)),
    value = interop_nbsp(txt$txt[match(k, key(txt))]),
    raw = txt$txt[match(k, key(txt))],
    bold = first$bold[match(k, key(first))], italic = first$italic[match(k, key(first))],
    font = first$font.family[match(k, key(first))], size = first$font.size[match(k, key(first))],
    color = toupper(first$color[match(k, key(first))]),
    halign = pa$text.align[match(k, key(pa))], valign = cl$vertical.align,
    fill = ifelse(cl$background.color == "transparent", NA_character_, toupper(cl$background.color)),
    top = interop_border_name(cl$border.width.top), bottom = interop_border_name(cl$border.width.bottom),
    left = interop_border_name(cl$border.width.left), right = interop_border_name(cl$border.width.right),
    stringsAsFactors = FALSE)
  out$color[out$color == "BLACK"] <- "#000000"
  out
}

# Merged rectangles of a flextable part (anchor rows x columns spans).
interop_ft_merges <- function(ft) {
  out <- list()
  for (p in c("header", "body")) {
    rs <- ft[[p]]$spans$rows
    cs <- ft[[p]]$spans$columns
    for (i in seq_len(nrow(rs))) for (j in seq_len(ncol(rs))) {
      if (rs[i, j] >= 1L && cs[i, j] >= 1L && (rs[i, j] > 1L || cs[i, j] > 1L)) {
        out[[length(out) + 1L]] <- data.frame(part = p, i1 = i, i2 = i + cs[i, j] - 1L,
                                              j1 = j, j2 = j + rs[i, j] - 1L, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(data.frame(part = character(), i1 = integer(), i2 = integer(), j1 = integer(), j2 = integer()))
  do.call(rbind, out)
}

interop_merge_keys <- function(m) sort(paste(m$part, m$i1, m$i2, m$j1, m$j2))

# Combined style of a list of gt style entries: later tab_style() calls
# override earlier ones attribute by attribute, as gt renders them.
interop_gt_combine <- function(entries) {
  s <- list(weight = "normal", style = "normal", size = NA_character_, color = "#000000",
            align = NA_character_, valign = NA_character_, whitespace = NA_character_,
            fill = NA_character_, top = NA_character_, bottom = NA_character_,
            left = NA_character_, right = NA_character_, css = "")
  for (st in entries) {
    for (el in st) {
      if (inherits(el, "cell_text")) {
        for (a in c("weight", "style", "size", "color", "align", "whitespace")) if (!is.null(el[[a]])) s[[a]] <- el[[a]]
        if (!is.null(el$v_align)) s$valign <- el$v_align
      } else if (inherits(el, "cell_fill")) {
        s$fill <- toupper(el$color)
      } else if (inherits(el, "cell_border")) {
        s[[el$side]] <- el$width
      } else if (is.character(el)) {
        s$css <- paste0(s$css, el)
      }
    }
  }
  s
}

interop_gt_frame <- function(res) {
  px <- c("1px" = "thin", "2px" = "medium", "3px" = "thick")
  d <- do.call(rbind, lapply(res, as.data.frame, stringsAsFactors = FALSE))
  d$fill <- ifelse(is.na(d$fill), NA_character_, substr(d$fill, 1L, 7L))
  for (side in c("top", "bottom", "left", "right")) d[[side]] <- unname(px[d[[side]]])
  d$bold <- d$weight == "bold"
  d$italic <- d$style == "italic"
  d$color <- toupper(d$color)
  d$size <- as.numeric(sub("pt$", "", d$size))
  # Excel's "center" is CSS's "middle".
  d$valign[d$valign %in% "middle"] <- "center"
  d
}

# Combined style of every body cell of a gt table.
interop_gt_body <- function(g) {
  st <- g$`_styles`
  st <- st[st$locname == "data", , drop = FALSE]
  n <- nrow(g$`_data`)
  keys <- names(g$`_data`)
  grid <- expand.grid(i = seq_len(n), j = seq_along(keys))
  res <- lapply(seq_len(nrow(grid)), function(k) {
    hit <- st$rownum == grid$i[k] & st$colname == keys[grid$j[k]]
    interop_gt_combine(st$styles[hit])
  })
  d <- interop_gt_frame(res)
  d$value <- interop_nbsp(as.matrix(g$`_data`)[cbind(grid$i, grid$j)])
  cbind(part = "body", grid, d, stringsAsFactors = FALSE)
}

# Combined style of each column label (one row per column) and of each
# spanner (one row per spanner, in g$`_spanners` order).
interop_gt_header <- function(g) {
  st <- g$`_styles`
  keys <- names(g$`_data`)
  lab <- interop_gt_frame(lapply(keys, function(k) {
    interop_gt_combine(st$styles[st$locname == "columns_columns" & st$colname %in% k])
  }))
  sp <- g$`_spanners`
  spn <- if (nrow(sp)) {
    interop_gt_frame(lapply(sp$spanner_id, function(k) {
      interop_gt_combine(st$styles[st$locname == "columns_groups" & st$grpname %in% k])
    }))
  }
  list(labels = cbind(j = seq_along(keys), lab), spanners = spn)
}

# ---------------------------------------------------------------------------
# Golden comparators for the converters (task 6.1; Phase 6 review P1-2,
# P1-3). Both take the table built by interop_tt(id), so one build serves
# both converters in the all-scenario sweep (test-interop-golden.R).

interop_cmp <- function(id, got, want, attr, ok, w = want[[attr]]) {
  bad <- which(ok & !mapply(identical, got[[attr]], w))
  expect(length(bad) == 0L, sprintf(
    "%s %s differs at %s", id, attr,
    paste(utils::head(sprintf("%s[%d,%d] got %s want %s", want$part[bad], want$i[bad], want$j[bad],
                              format(got[[attr]][bad]), format(w[bad])), 5), collapse = "; ")))
}

expect_ft_matches_golden <- function(id, tt = interop_tt(id)) {
  ft <- flextable::as_flextable(tt)
  expect_s3_class(ft, "flextable")
  g <- interop_golden(id, nrow(tt$body), ncol(tt$body))

  # Merged ranges: exactly Stata's (title and footnote merges become the
  # caption and the footer line).
  expect_identical(interop_merge_keys(interop_ft_merges(ft)), interop_merge_keys(g$merges),
                   label = paste(id, "merges"))

  want <- interop_anchor_borders(interop_shared_edges(g$cells), g$mg)
  got <- interop_ft_cells(ft)
  got <- got[got$part %in% c("header", "body"), ]
  key <- function(d) paste(d$part, d$i, d$j)
  got <- got[match(key(want), key(got)), ]
  expect_false(anyNA(got$part), label = paste(id, "every golden cell present"))
  mask <- interop_p_masked(tt, want, id)
  live <- !want$nonanchor
  cmp <- function(attr, ok = live) interop_cmp(id, got, want, attr, ok)
  cmp("value", live & !mask$value)
  cmp("italic")
  cmp("bold", live & !(mask$style & want$j %in% mask$pcol))
  cmp("font")
  cmp("size")
  cmp("color")
  cmp("halign")
  cmp("valign")
  cmp("fill", live & !mask$style)
  for (side in c("top", "bottom", "left", "right")) cmp(side)
  # Literal indents survive as no-break spaces: 3 in table1_tc, 2 in regtab.
  n_ind <- tt$layout$indent
  lvl <- which(startsWith(tt$body[[1]], strrep(" ", n_ind)))
  if (length(lvl)) {
    raw <- got$raw[got$part == "body" & got$j == 1L & got$i %in% lvl]
    expect_true(all(startsWith(raw, strrep("\u00a0", n_ind))), label = paste(id, "nbsp indents"))
  }

  # Title as the caption (bold, two points larger, left-aligned as the
  # workbook's title row), footnote as the footer.
  if (nzchar(g$title)) {
    expect_identical(ft$caption$value$txt, g$title)
    expect_true(ft$caption$value$bold)
    expect_equal(ft$caption$value$font.size, tt$style$fontsize + 2)
    expect_identical(ft$caption$fp_p$text.align, "left")
  } else {
    expect_null(ft$caption$value)
  }
  foot <- flextable::information_data_chunk(ft)
  # The footer line is one cell merged across the table (its hidden cells
  # repeat the text), italic, left-aligned, two points smaller.
  got_foot <- foot[foot$.part == "footer" & foot$.col_id == "c1", ]
  contract <- golden_publication_contract(id)
  golden_assert_native_footnote_tail(if (nzchar(g$footnote)) g$footnote else character(), contract, "xlsx")
  values <- vapply(split(got_foot$txt, got_foot$.row_id), function(x) interop_nbsp(paste(x, collapse = "")), "")
  golden_assert_footnote_tail(unname(values), contract, "presentation")
  if (length(contract$paragraphs)) {
    expect_identical(nrow(ft$footer$spans$rows), length(contract$paragraphs))
    expect_true(all(ft$footer$spans$rows[, 1] == ncol(tt$body)))
    expect_true(all(got_foot$italic))
    expect_equal(unique(got_foot$font.size), max(tt$style$fontsize - 2, 6))
    pa <- flextable::information_data_paragraph(ft)
    expect_identical(unique(pa$text.align[pa$.part == "footer"]), "left")
  } else {
    expect_equal(nrow(got_foot), 0L)
  }
  # Widths: Stata's, within the golden comparators' 0.5 Excel units (the
  # masked Test/Statistic columns follow R's longer test names).
  inch <- (7 * g$widths + 13) / 96
  wok <- !seq_along(inch) %in% mask$width_cols
  expect_true(all(abs(ft$body$colwidths - inch)[wok] <= 3.5 / 96 + 1e-9), label = paste(id, "widths"))
  invisible(list(tt = tt, ft = ft))
}

expect_gt_matches_golden <- function(id, tt = interop_tt(id)) {
  g <- tabtools::tt_as_gt(tt)
  expect_s3_class(g, "gt_tbl")
  gold <- interop_golden(id, nrow(tt$body), ncol(tt$body))
  cells <- gold$cells
  nc <- ncol(tt$body)
  mask <- interop_p_masked(tt, cells, id)

  # Header: the lower row gives the column labels, except columns Stata
  # merges over both rows; the upper row gives the spanners.
  hd <- cells[cells$part == "header", ]
  hv <- function(i, j) hd$value[hd$i == i & hd$j == j]
  hcell <- function(i, j) hd[hd$i == i & hd$j == j, ]
  mg <- gold$merges[gold$merges$part == "header", ]
  vcols <- mg$j1[mg$i1 == 1L & mg$i2 == 2L & mg$j1 == mg$j2]
  want_lab <- vapply(seq_len(nc), function(j) if (j %in% vcols) hv(1L, j) else hv(2L, j), "")
  got_lab <- vapply(g$`_boxhead`$column_label, function(x) interop_nbsp(as.character(x)), "")
  expect_identical(got_lab, want_lab, label = paste(id, "column labels"))
  hz <- mg[mg$i1 == 1L & mg$i2 == 1L, ]
  span_cols <- c(unlist(Map(seq, hz$j1, hz$j2)))
  single <- setdiff(seq_len(nc)[nzchar(trimws(vapply(seq_len(nc), hv, "", i = 1L)))], c(vcols, span_cols))
  want_sp <- data.frame(label = c(vapply(seq_len(nrow(hz)), function(k) hv(1L, hz$j1[k]), ""),
                                  vapply(single, function(j) hv(1L, j), "")),
                        j1 = c(hz$j1, single), j2 = c(hz$j2, single), stringsAsFactors = FALSE)
  sp <- g$`_spanners`
  got_j <- lapply(sp$vars, function(v) match(v, names(g$`_data`)))
  got_sp <- vapply(seq_len(nrow(sp)), function(k) {
    paste(interop_nbsp(as.character(sp$spanner_label[[k]])), min(got_j[[k]]), max(got_j[[k]]), sep = "|")
  }, "")
  expect_setequal(got_sp, paste(want_sp$label, want_sp$j1, want_sp$j2, sep = "|"))

  # Header styling (review P0-2/P1-2): each column label against the cell it
  # comes from; its top is the rule above it (the workbook's rule between
  # the header rows for a label under a spanner, the table's top rule
  # otherwise). Each spanner against its range's anchor, with the range's
  # last right border.
  gh <- interop_gt_header(g)
  under <- rep(FALSE, nc)
  for (k in seq_along(got_j)) under[got_j[[k]]] <- TRUE
  lab_want <- do.call(rbind, lapply(seq_len(nc), function(j) {
    w <- if (j %in% vcols) hcell(1L, j) else hcell(2L, j)
    w$top <- if (under[j]) interop_heavier(hcell(1L, j)$bottom, hcell(2L, j)$top) else hcell(1L, j)$top
    w$bottom <- hcell(2L, j)$bottom
    w
  }))
  lab_got <- gh$labels
  hcmp <- function(got, want, attrs, what) {
    for (a in attrs) interop_cmp(paste(id, what), got, want, a, rep(TRUE, nrow(want)))
  }
  hattrs <- c("bold", "italic", "size", "color", "fill", "valign", "top", "bottom", "left", "right")
  hcmp(lab_got, lab_want, hattrs, "column label")
  interop_cmp(paste(id, "column label"), lab_got, lab_want, "align", rep(TRUE, nc), w = lab_want$halign)
  if (nrow(want_sp)) {
    sp_want <- do.call(rbind, lapply(seq_len(nrow(want_sp)), function(k) {
      w <- hcell(1L, want_sp$j1[k])
      w$right <- hcell(1L, want_sp$j2[k])$right
      w
    }))
    ord <- match(paste(want_sp$j1, want_sp$j2),
                 vapply(got_j, function(j) paste(min(j), max(j)), ""))
    sp_got <- gh$spanners[ord, , drop = FALSE]
    hcmp(sp_got, sp_want, c("bold", "italic", "size", "color", "fill", "right"), "spanner")
  }

  # Body cells: every cell, anchors and the block cells gt cannot merge.
  want <- cells[cells$part == "body", ]
  bmask <- lapply(mask[c("value", "style")], function(m) m[cells$part == "body"])
  got <- interop_gt_body(g)
  got <- got[match(paste(want$i, want$j), paste(got$i, got$j)), ]
  live <- rep(TRUE, nrow(want))
  cmp <- function(attr, ok = live, w = want[[attr]]) interop_cmp(id, got, want, attr, ok, w)
  cmp("value", !bmask$value)
  cmp("italic")
  cmp("bold", live & !(bmask$style & want$j %in% mask$pcol))
  cmp("size")
  cmp("color")
  cmp("align", w = want$halign)
  cmp("valign")
  cmp("fill", live & !bmask$style)
  for (side in c("top", "bottom", "left", "right")) cmp(side)

  # Font family and column widths (7w + 5 px plus 8 px padding, within the
  # 0.5 Excel units of the golden comparators).
  opt <- function(p) g$`_options`$value[g$`_options`$parameter == p][[1]]
  expect_identical(opt("table_font_names")[1], unique(cells$font[!is.na(cells$font)]))
  px <- as.numeric(sub("px$", "", unlist(g$`_boxhead`$column_width)))
  wok <- !seq_len(nc) %in% mask$width_cols
  expect_true(all(abs(px - (7 * gold$widths + 13))[wok] <= 3.5), label = paste(id, "gt widths"))

  # Indents as no-break spaces.
  lvl <- which(startsWith(tt$body[[1]], strrep(" ", tt$layout$indent)))
  if (length(lvl)) {
    expect_true(all(startsWith(g$`_data`$c1[lvl], strrep("\u00a0", tt$layout$indent))))
  }
  # Title as a bold, left-aligned caption two points larger (CSS scoped to
  # the table id), footnote as an italic, smaller, left-aligned source note.
  cap <- opt("table_caption")
  css <- paste(opt("table_additional_css"), collapse = "\n")
  if (nzchar(gold$title)) {
    expect_true(grepl("^\\*\\*.*\\*\\*$", as.character(cap)), label = paste(id, "caption bold"))
    expect_identical(gsub("\\\\", "", sub("^\\*\\*(.*)\\*\\*$", "\\1", as.character(cap))), gold$title)
    expect_true(grepl(sprintf("\\.gt_caption \\{ text-align: left; font-weight: bold; font-size: %spt; \\}",
                              tt$style$fontsize + 2), css), label = paste(id, "caption CSS"))
  } else {
    expect_true(is.null(cap) || all(is.na(cap)))
  }
  sn <- g$`_source_notes`
  contract <- golden_publication_contract(id)
  golden_assert_native_footnote_tail(if (nzchar(gold$footnote)) gold$footnote else character(), contract, "xlsx")
  golden_assert_footnote_tail(as.character(unlist(sn, use.names = FALSE)), contract, "presentation")
  if (length(contract$paragraphs)) {
    st <- g$`_styles`
    note <- interop_gt_frame(list(interop_gt_combine(st$styles[st$locname == "source_notes"])))
    expect_true(note$italic, label = paste(id, "source note italic"))
    expect_equal(note$size, max(tt$style$fontsize - 2, 6))
    expect_identical(note$align, "left")
  } else {
    expect_length(sn, 0L)
  }
  html <- gt::as_raw_html(g, inline_css = TRUE)
  expect_true(nzchar(html))
  # gt's `.gt_spanner_row { border-bottom-style: hidden }` would erase the
  # rule between the header rows (review P0-2): the override must win.
  tr <- regmatches(html, regexpr('<tr class="gt_col_headings gt_spanner_row"[^>]*>', html))
  if (length(tr)) {
    last_bb <- utils::tail(regmatches(tr, gregexpr("border-bottom-style: *[a-z]+", tr))[[1]], 1L)
    expect_false(grepl("hidden", last_bb), label = paste(id, "spanner row bottom not hidden"))
  }
  if (length(lvl)) {
    lab <- sub("^ +", "", tt$body[[1]][lvl[1]])
    expect_true(grepl(paste0(strrep("(\u00a0|&nbsp;|&#160;)", tt$layout$indent), lab), html, fixed = FALSE) ||
                  grepl(paste0(strrep("\u00a0", tt$layout$indent), lab), html, fixed = TRUE),
                label = paste(id, "indent in HTML"))
  }
  invisible(list(tt = tt, g = g))
}

# ---------------------------------------------------------------------------
# Rules in Word output (Phase 6 review P0-1, P1-1). Word keeps one border per
# shared edge, so the check works on edges, not cells: for every horizontal
# edge (above table row b + 1, b = 0 the top of the table) and every vertical
# edge (right of grid column c, c = 0 the left side), the heavier of the two
# cells' borders. Interior edges of merged ranges are not edges.

interop_docx_sz <- c("6" = "thin", "12" = "medium", "18" = "thick")

# Edges of the first table in a .docx, as sorted "h|b|col|weight" and
# "v|row|c|weight" keys; rows beyond `nrows` (the footer line) only close
# the last row's bottom edge.
interop_docx_edges <- function(path, nrows) {
  dir <- withr::local_tempdir()
  utils::unzip(path, "word/document.xml", exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  ns <- xml2::xml_ns(doc)
  trs <- xml2::xml_find_all(xml2::xml_find_first(doc, "//w:tbl", ns), "w:tr", ns)
  side <- function(tc, s) {
    b <- xml2::xml_find_first(tc, paste0("w:tcPr/w:tcBorders/w:", s), ns)
    if (inherits(b, "xml_missing") || xml2::xml_attr(b, "val") %in% c("none", "nil")) return(NA_character_)
    unname(interop_docx_sz[xml2::xml_attr(b, "sz")])
  }
  grid <- lapply(trs, function(tr) {
    tcs <- xml2::xml_find_all(tr, "w:tc", ns)
    out <- list()
    for (tc in tcs) {
      span <- xml2::xml_attr(xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", ns), "val")
      span <- if (is.na(span)) 1L else as.integer(span)
      vm <- xml2::xml_find_first(tc, "w:tcPr/w:vMerge", ns)
      cont <- !inherits(vm, "xml_missing") && !identical(xml2::xml_attr(vm, "val"), "restart")
      for (k in seq_len(span)) {
        out[[length(out) + 1L]] <- data.frame(
          top = side(tc, "top"), bottom = side(tc, "bottom"),
          left = if (k == 1L) side(tc, "left") else NA_character_,
          right = if (k == span) side(tc, "right") else NA_character_,
          inner_left = k > 1L, cont = cont, stringsAsFactors = FALSE)
      }
    }
    do.call(rbind, out)
  })
  nc <- nrow(grid[[1]])
  keys <- character()
  for (b in 0:nrows) for (c in seq_len(nc)) {
    up <- if (b >= 1L) grid[[b]]$bottom[c] else NA_character_
    lo <- if (b < length(grid)) grid[[b + 1L]] else NULL
    if (!is.null(lo) && b >= 1L && b < nrows && lo$cont[c]) next
    w <- interop_heavier(up, if (is.null(lo)) NA_character_ else lo$top[c])
    if (!is.na(w)) keys <- c(keys, paste("h", b, c, w, sep = "|"))
  }
  for (r in seq_len(nrows)) for (c in 0:nc) {
    row <- grid[[r]]
    if (c >= 1L && c < nc && row$inner_left[c + 1L]) next
    w <- interop_heavier(if (c >= 1L) row$right[c] else NA_character_,
                         if (c < nc) row$left[c + 1L] else NA_character_)
    if (!is.na(w)) keys <- c(keys, paste("v", r, c, w, sep = "|"))
  }
  sort(keys)
}

# The same edges from Stata's workbook (interop_golden()): worksheet rows 2
# to the last body row, columns B onwards.
interop_golden_edges <- function(g) {
  cells <- g$cells
  mg <- g$mg
  rows <- sort(unique(cells$r))
  cols <- sort(unique(cells$c))
  at <- function(r, c, a) {
    k <- which(cells$r == r & cells$c == c)
    if (length(k)) cells[[a]][k] else NA_character_
  }
  inside <- function(r1, c1, r2, c2) {
    any(mg$r1 <= min(r1, r2) & mg$r2 >= max(r1, r2) & mg$c1 <= min(c1, c2) & mg$c2 >= max(c1, c2))
  }
  keys <- character()
  for (b in 0:length(rows)) for (k in seq_along(cols)) {
    c <- cols[k]
    r_up <- rows[1] + b - 1L
    r_lo <- r_up + 1L
    if (b >= 1L && b < length(rows) && inside(r_up, c, r_lo, c)) next
    w <- interop_heavier(if (b >= 1L) at(r_up, c, "bottom") else NA_character_,
                         if (b < length(rows)) at(r_lo, c, "top") else NA_character_)
    if (!is.na(w)) keys <- c(keys, paste("h", b, k, w, sep = "|"))
  }
  for (i in seq_along(rows)) for (k in 0:length(cols)) {
    r <- rows[i]
    if (k >= 1L && k < length(cols) && inside(r, cols[k], r, cols[k + 1L])) next
    w <- interop_heavier(if (k >= 1L) at(r, cols[k], "right") else NA_character_,
                         if (k < length(cols)) at(r, cols[k + 1L], "left") else NA_character_)
    if (!is.na(w)) keys <- c(keys, paste("v", i, k, w, sep = "|"))
  }
  sort(keys)
}

# tt_as_gtsummary() (task 6.5): the gtsummary table body holds the
# tt_table's cells, the label without its indent, and each row's key.
expect_gts_keeps_cells <- function(tt) {
  g <- tt_as_gtsummary(tt)
  tb <- as.data.frame(g$table_body)
  vc <- paste0("col_", seq_len(ncol(tt$body))[-1], recycle0 = TRUE)
  expect_identical(tb$label, sub("^ +", "", tt$body[[1]]))
  expect_identical(unname(as.matrix(tb[vc])), unname(as.matrix(tt$body[-1])))
  expect_identical(tb$key, tt$rows$key)
}
