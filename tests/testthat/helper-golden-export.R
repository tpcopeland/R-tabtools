# Golden runner for the export commands of Phase 7a, puttab (P scenarios)
# and stacktab (K scenarios) (IMPLEMENTATION_PLAN.md task 7.6).
#
# They differ from table1_tc/regtab in their sinks: puttab writes to Stata's
# `using file.xlsx` (the golden sheet <id> of puttab.xlsx, or of <id>.xlsx
# when the scenario's Stata call names it: golden_book()), and stacktab
# reads its blocks from, and writes into, one workbook per scenario
# (golden <id>.xlsx, built by the scenario's setup; qa/make_golden.R). Their
# console output is not a listing of the table but the command's messages
# (and stacktab's `display` listing), so the runner captures what the R call
# prints instead of print(tt).

golden_export_ids <- function(command = c("puttab", "stacktab")) {
  sc <- golden_scenarios()
  sc$id[sc$command %in% command]
}

# The demo pipeline's two source blocks (demo/demo_tabtools.do:1514-1538),
# written with R's puttab() into `path`; the Stata twin is golden_hrt_blocks
# in qa/stata/golden_stacktab_blocks.do.
golden_hrt_blocks <- function(path) {
  mk <- function(term, ahr, ci) {
    d <- data.frame(term = term, ahr = ahr, ci = ci)
    attr(d$term, "label") <- "Exposure"
    attr(d$ahr, "label") <- "aHR"
    attr(d$ci, "label") <- "95% CI"
    d
  }
  primary <- mk(c("Any HRT", "Former smoker", "Current smoker"), c("0.82", "1.14", "1.46"),
                c("(0.69, 0.98)", "(0.97, 1.34)", "(1.21, 1.77)"))
  dose <- mk(c("Low dose", "High dose"), c("0.91", "0.73"), c("(0.74, 1.12)", "(0.58, 0.92)"))
  suppressMessages({
    a <- tabtools::puttab(primary, xlsx = path, sheet = "Block Primary",
                          title = "Source block: Primary HRT exposure model", varlabels = TRUE)
    b <- tabtools::puttab(dose, xlsx = path, sheet = "Block Dose",
                          title = "Source block: Estrogen dose-response model", varlabels = TRUE)
  })
  invisible(list(primary = a, dose = b))
}

# Stata's r(table)' after `regress`: rows b, se, t, pvalue, ll, ul, df, crit,
# eform per coefficient. `rows` maps Stata row names to coefficient names of
# the lm fit; NA is a base level (b = 0, se..ul missing).
golden_rtable_regress <- function(fit, rows) {
  cf <- summary(fit)$coefficients
  df <- fit$df.residual
  crit <- stats::qt(0.975, df)
  m <- t(vapply(rows, function(r) {
    if (is.na(r)) return(c(0, NA, NA, NA, NA, NA, df, crit, 0))
    b <- cf[r, 1]
    se <- cf[r, 2]
    c(b, se, cf[r, 3], cf[r, 4], b - crit * se, b + crit * se, df, crit, 0)
  }, numeric(9)))
  dimnames(m) <- list(names(rows), c("b", "se", "t", "pvalue", "ll", "ul", "df", "crit", "eform"))
  m
}

# The console lines of an export scenario, both sides: Stata's log, or the
# R call's printed output followed by its messages, with R's temporary sink
# paths written as the Stata call named them.
golden_export_console <- function(lines) golden_console_box(lines)

golden_expect_none <- function(why, what) {
  if (length(why)) {
    testthat::fail(paste0(what, ":\n", paste(utils::head(why, 12L), collapse = "\n")))
  } else {
    testthat::succeed()
  }
}

run_golden_export_scenario <- function(id) {
  sc <- golden_scenario(id)
  if (!golden_scenario_live(id, sc$phase)) testthat::skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  sinks <- golden_code_path(out, id)
  code <- gsub("@ID@", sinks, sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  msgs <- character()
  printed <- utils::capture.output(
    tt <- withCallingHandlers(eval(parse(text = code), envir = env), message = function(m) {
      msgs <<- c(msgs, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    })
  )
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$command, sc$command)
  expect_cells_match(tt, id)
  golden_assert_listing_footer(tt, id)

  # Console: the paths R wrote to, as Stata named them.
  own <- paste0(sinks, ".xlsx")
  stata_book <- golden_book_name(id)
  got <- gsub(own, stata_book, c(printed, msgs), fixed = TRUE)
  got <- gsub(paste0(gsub("\\", "/", out, fixed = TRUE), "/"), "", got, fixed = TRUE)
  want <- golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt")))
  # R has no frames: a data frame is the source for both Stata's varlist
  # and frame(), and r(source) is "data" (?puttab, "Differences from
  # Stata").
  frame_src <- grepl("frame(", sc$stata_call, fixed = TRUE)
  if (frame_src) {
    expect_true(any(grepl("(frame source)", want, fixed = TRUE)))
    want <- sub("(frame source)", "(data source)", want, fixed = TRUE)
  }
  why <- golden_compare_console(golden_export_console(got), golden_export_console(want))
  golden_expect_none(why, paste("console", id))

  # Stored results, file paths aside (R's are temporary).
  stored <- golden_read_stored(id)
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]), c("file", "book", "csv", "markdown"))
  if (frame_src) {
    expect_identical(stored$value[stored$name == "source"], "frame")
    expect_identical(tt$stored$source, "data")
    fields <- setdiff(fields, "source")
  }
  expect_stored_match(tt, id, fields = fields)

  # Workbook: every sheet of the golden workbook (puttab: the scenario's
  # sheet; stacktab: its composite and the source blocks R's setup wrote).
  if (sc$command == "puttab") {
    want_book <- golden_book(id)
    golden_expect_none(golden_compare_styles(own, id, want_book, id, got_width_offset = golden_r_width_offset, publication_id = id),
                       paste("styles", id))
    # The renderer alone, from the returned table.
    again <- file.path(out, "again.xlsx")
    tabtools::tt_write_xlsx(tt, again, sheet = id)
    golden_expect_none(golden_compare_styles(again, id, want_book, id, got_width_offset = golden_r_width_offset, publication_id = id),
                       paste("re-rendered", id))
  } else {
    want_book <- golden_book(id)
    sheets <- tidyxl::xlsx_sheet_names(want_book)
    expect_setequal(tidyxl::xlsx_sheet_names(own), sheets)
    for (s in sheets) {
      golden_expect_none(golden_compare_styles(own, s, want_book, s, got_width_offset = golden_r_width_offset,
                                               publication_id = if (s == id) id else NULL),
                         paste("styles", id, "sheet", s))
    }
    if (identical(tt$stored$append_start, 2L)) {
      # A composite written at B2 re-renders from the returned table alone.
      again <- file.path(out, "again.xlsx")
      tabtools::tt_write_xlsx(tt, again, sheet = tt$stored$sheet)
      golden_expect_none(golden_compare_styles(again, tt$stored$sheet, want_book, tt$stored$sheet,
                                               got_width_offset = golden_r_width_offset, publication_id = id),
                         paste("re-rendered", id))
    }
  }

  expect_sink_match(paste0(sinks, ".csv"), id, "csv", tt = tt)
  expect_sink_match(paste0(sinks, ".md"), id, "md", tt = tt)
  invisible(tt)
}

# ---------------------------------------------------------------------------
# Converters against the golden sheet (Phase 7a review F4): as_flextable()
# and tt_as_gt() of a puttab/stacktab table keep the workbook's fills, bold,
# rules, alignment, fonts and widths. Geometry: puttab's header (if any) at
# worksheet row 2 and its body below; stacktab's first composite row at its
# table start (B2, or lower after an append) and the body below it.

export_golden_cells <- function(xlsx, sheet, hdr_rows, body_rows, nc) {
  w <- golden_cell_styles(xlsx, sheet)
  rows <- lapply(c(hdr_rows, body_rows), function(r) {
    head <- r %in% hdr_rows
    do.call(rbind, lapply(seq_len(nc) + 1L, function(c) {
      k <- which(w$row == r & w$col == c)
      x <- if (length(k)) w[k[1], ] else NULL
      get <- function(a, default) if (is.null(x) || is.na(x[[a]])) default else x[[a]]
      data.frame(
        part = if (head) "header" else "body",
        i = if (head) match(r, hdr_rows) else match(r, body_rows), j = c - 1L, r = r, c = c,
        value = interop_nbsp(get("value", "")),
        bold = isTRUE(get("bold", FALSE)), italic = isTRUE(get("italic", FALSE)),
        font = get("font", NA_character_), size = get("size", NA_real_),
        halign = if (get("halign", "general") %in% c("general", "left")) "left" else get("halign", ""),
        valign = get("valign", "bottom"),
        fill = if (nzchar(get("fill", ""))) paste0("#", substring(get("fill", ""), 3L)) else NA_character_,
        top = get("border_top", NA_character_), bottom = get("border_bottom", NA_character_),
        left = get("border_left", NA_character_), right = get("border_right", NA_character_),
        stringsAsFactors = FALSE)
    }))
  })
  do.call(rbind, rows)
}

expect_export_converters_match <- function(id, tt) {
  sc <- golden_scenario(id)
  nh <- length(tt$header)
  nb <- nrow(tt$body)
  nc <- ncol(tt$body)
  if (sc$command %in% c("puttab", "wttab")) {
    # wttab (task 7.12) is puttab's layout, in its own golden workbook.
    xlsx <- golden_book(id)
    sheet <- id
    start <- 2L
  } else {
    xlsx <- golden_book(id)
    sheet <- tt$stored$sheet
    start <- tt$stored$append_start
  }
  hdr_rows <- if (nh) seq.int(start, length.out = nh) else integer()
  body_rows <- start + nh + seq_len(nb) - 1L
  want <- export_golden_cells(xlsx, sheet, hdr_rows, body_rows, nc)
  lay <- golden_sheet_layout(xlsx, sheet)
  widths <- lay$widths$width[match(seq_len(nc) + 1L, lay$widths$col)] - golden_width_offset
  attrs <- c("value", "bold", "italic", "font", "size", "halign", "valign", "fill", "top", "bottom", "left", "right")
  key <- function(d) paste(d$part, d$i, d$j)

  # flextable: Word keeps one rule per shared edge, on the upper cell.
  ft <- flextable::as_flextable(tt)
  got <- interop_ft_cells(ft)
  wf <- interop_shared_edges(want)
  g <- got[match(key(wf), key(got)), ]
  expect_false(anyNA(g$part), label = paste(id, "flextable: every golden cell present"))
  for (a in attrs) interop_cmp(paste(id, "flextable"), g, wf, a, rep(TRUE, nrow(wf)))
  if (!nh) {
    expect_identical(unique(got$value[got$part == "header"]), "", label = paste(id, "blank header row"))
  }
  expect_identical(nrow(interop_ft_merges(ft)), 0L, label = paste(id, "no merges inside the table"))
  inch <- (7 * widths + 13) / 96
  expect_true(all(abs(ft$body$colwidths - inch) <= 3.5 / 96 + 1e-9), label = paste(id, "flextable widths"))
  if (nzchar(tt$title)) expect_identical(ft$caption$value$txt, tt$title)
  foot <- flextable::information_data_chunk(ft)
  foot <- foot[foot$.part == "footer" & foot$.col_id == "c1", ]
  contract <- golden_publication_contract(id)
  values <- vapply(split(foot$txt, foot$.row_id), function(x) interop_nbsp(paste(x, collapse = "")), "")
  golden_assert_footnote_tail(unname(values), contract, "presentation")
  if (length(contract$paragraphs)) {
    expect_true(all(foot$italic))
    expect_true(all(foot$font.size == max(tt$style$fontsize - 2, 6)))
  }

  # gt: body cells as the workbook has them; column labels from the header row.
  gt_tab <- tabtools::tt_as_gt(tt)
  golden_assert_footnote_tail(as.character(unlist(gt_tab[["_source_notes"]], use.names = FALSE)), contract, "presentation")
  gb <- interop_gt_body(gt_tab)
  wb <- want[want$part == "body", ]
  gb <- gb[match(paste(wb$i, wb$j), paste(gb$i, gb$j)), ]
  gb$halign <- gb$align
  for (a in setdiff(attrs, c("font", "size"))) interop_cmp(paste(id, "gt body"), gb, wb, a, rep(TRUE, nrow(wb)))
  if (nh) {
    gl <- interop_gt_header(gt_tab)$labels
    wh <- want[want$part == "header", ]
    gl <- gl[match(wh$j, gl$j), ]
    gl$halign <- gl$align
    expect_identical(vapply(gt_tab$`_boxhead`$column_label, function(x) interop_nbsp(as.character(x)), ""),
                     wh$value, label = paste(id, "gt column labels"))
    for (a in c("bold", "fill", "halign", "valign", "top", "bottom")) {
      interop_cmp(paste(id, "gt labels"), gl, wh, a, rep(TRUE, nrow(wh)))
    }
  }
  invisible(list(ft = ft, gt = gt_tab))
}
