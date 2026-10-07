# Self-tests for the golden harness (IMPLEMENTATION_PLAN.md §5, task 0.5).
# Each comparator must accept the golden itself and reject deliberately
# broken input; the manifest must cover the §8 catalogue.

# ---------------------------------------------------------------------------
# Manifest and golden inventory

test_that("scenario manifest is well formed", {
  sc <- golden_scenarios()
  expect_named(sc, c("id", "command", "phase", "fixture", "description", "stata_setup",
                     "stata_call", "r_call", "compare", "mask"))
  expect_false(anyDuplicated(sc$id) > 0)
  expect_true(all(sc$command %in% c("table1_tc", "regtab", "puttab", "stacktab", "stratetab", "effecttab", "wttab",
                                    "comptab", "hrcomptab")))
  expect_true(all(sc$phase %in% c(2:5, 7)))
  expect_true(all(sc$compare %in% c("exact", "tolerance", "structure")))
  fx <- unique(sc$fixture)
  if (!golden_in_source()) fx <- setdiff(fx, golden_source_only_fixtures)
  expect_true(all(file.exists(golden_path("fixtures", paste0(fx, ".dta")))))
  tokens <- unique(unlist(lapply(sc$mask, golden_mask)))
  expect_true(all(tokens %in% c(names(golden_mask_headers), "pstyle")))
  expect_true(all(startsWith(sc$stata_call, sc$command)))
  # Every R call parses (the functions it calls arrive in later phases).
  for (i in seq_len(nrow(sc))) expect_no_error(parse(text = sc$r_call[i]))
})

test_that("manifest covers every scenario in the plan's §8 catalogue", {
  ids <- golden_scenarios()$id
  catalogue <- c(sprintf("T%02d", 1:35), "T03b", "T03c", sprintf("R%02d", 1:29), "R13b", "R13c")
  covered <- vapply(catalogue, function(x) any(ids == x | startsWith(ids, x) & grepl("^[a-z]$", substring(ids, nchar(x) + 1L))), TRUE)
  expect_true(all(covered), label = paste("missing:", paste(catalogue[!covered], collapse = ", ")))
})

test_that("every scenario has all five goldens", {
  skip_if_not_installed("tidyxl")
  sc <- golden_scenarios()
  sc <- sc[sc$id %in% golden_ids_in_build(sc$id), , drop = FALSE]
  for (ext in c(".csv", ".md", "_stored.csv", "_console.txt")) {
    f <- vapply(sc$id, function(id) golden_artifact_path(id, paste0(id, ext)), "")
    expect_true(all(file.exists(f)), label = paste("missing", ext, paste(basename(f[!file.exists(f)]), collapse = " ")))
  }
  own <- vapply(sc$id, function(id) golden_book_name(id) == paste0(id, ".xlsx"), TRUE)
  for (cmd in setdiff(unique(sc$command), "stacktab")) {
    sheets <- tidyxl::xlsx_sheet_names(golden_path(paste0(cmd, ".xlsx")))
    # comptab's demo scenario also keeps the regtab source sheets its setup
    # writes (golden_comptab_source_sheets, helper-golden-comptab.R).
    ids <- sc$id[sc$command == cmd & !own]
    expect_setequal(sheets, c(ids, unlist(golden_comptab_source_sheets[intersect(ids, names(golden_comptab_source_sheets))])))
  }
  # stacktab reads its blocks from its own workbook: one per scenario
  # (<id>.xlsx), holding the source blocks and the composite sheet <id>;
  # P02 and P09 have their own workbook too (golden_book()).
  for (id in sc$id[own]) {
    expect_true(id %in% tidyxl::xlsx_sheet_names(golden_book(id)), label = id)
  }
})

test_that("goldens record the Stata package versions they came from", {
  v <- utils::read.csv(golden_path("VERSIONS.csv"), colClasses = "character")
  ver <- setNames(v$version, v$component)
  comps <- c("desctab", "regtab", "table1_tc", "_tabtools_common", "puttab", "stacktab", "_tabtools_markdown_write",
             "stratetab", "effecttab", "comptab", "hrcomptab")
  expect_identical(unname(ver[comps]), rep("2.5.1", length(comps)))
  expect_identical(unname(ver["fvgen"]), "1.2.7")
  expect_identical(unname(ver["rng"]), "mt64")
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    st <- golden_read_stored(id)
    meta <- setNames(st$value[st$kind == "meta"], st$name[st$kind == "meta"])
    expect_identical(unname(meta[c("_golden_id", "_golden_tabtools_version", "_golden_fvgen_version")]),
                     c(id, golden_baseline(id)$tabtools, golden_baseline(id)$fvgen))
    expect_identical(golden_compare_provenance(st, id), character())
  }
})

test_that("all 272 ordinary scenarios route to the full authenticated native baseline", {
  expect_identical(nrow(golden_scenarios()), 272L)
  expect_error(golden_baseline("unknown"), "Unknown golden scenario")
  for (id in golden_scenarios()$id) {
    expect_identical(golden_baseline(id), list(dir = golden_dir(), tabtools = "2.5.1", fvgen = "1.2.7"), label = id)
    expect_identical(dirname(golden_book(id)), golden_dir(), label = paste(id, "book"))
    for (ext in c(".csv", ".md", "_console.txt", "_stored.csv")) {
      expect_identical(dirname(golden_artifact_path(id, paste0(id, ext))), golden_dir(), label = paste(id, ext))
    }
  }
})

test_that("the complete baseline is byte-identical to retained native c215 inventory", {
  a <- utils::read.csv(golden_path("ARTIFACTS.csv"), colClasses = "character")
  expect_false(anyDuplicated(a$file) > 0L)
  expect_identical(nrow(a), 1143L)
  expect_identical(unique(a$source_commit), "c215afdb788f88363c8d5289d7d13e3a31cfbe38")
  expect_identical(a$source_path, paste0("tests/testthat/golden/", a$file))
  sc <- golden_scenarios()
  expected <- unlist(lapply(sc$id, function(id) paste0(id, c(".csv", ".md", "_console.txt", "_stored.csv"))))
  expect_true(all(expected %in% a$file))
  expect_true(all(vapply(sc$id, golden_book_name, "") %in% a$file))
  excluded <- grepl("^P0[29](\\.|_)", a$file)
  expect_identical(a$distribution, ifelse(excluded, "source-only", "shipped"))
  if (!golden_in_source()) {
    expect_false(any(file.exists(golden_path(a$file[excluded]))))
    a <- a[!excluded, , drop = FALSE]
  }
  files <- golden_path(a$file)
  expect_true(all(file.exists(files)))
  expect_identical(unname(tools::md5sum(files)), a$md5)
  bad <- file.path(withr::local_tempdir(), "T18.csv")
  writeBin(c(golden_bytes(golden_artifact_path("T18", "T18.csv")), charToRaw("fault")), bad)
  expect_false(identical(unname(tools::md5sum(bad)), a$md5[a$file == "T18.csv"]))
})

test_that("wrong per-case version and missing or duplicate provenance fail", {
  for (id in c("S08", "P01", "K01", "W15", "W16", "T01")) {
    st <- golden_read_stored(id)
    expect_identical(golden_compare_provenance(st, id), character())
    for (key in c("_golden_id", "_golden_tabtools_version", "_golden_fvgen_version")) {
      bad <- st
      bad$value[bad$kind == "meta" & bad$name == key] <- "fault"
      expect_gt(length(golden_compare_provenance(bad, id)), 0L, label = paste(id, key))
      expect_gt(length(golden_compare_provenance(st[st$name != key, ], id)), 0L)
      expect_gt(length(golden_compare_provenance(rbind(st, st[st$kind == "meta" & st$name == key, ]), id)), 0L)
    }
  }
})

test_that("all 21 original scenario inputs remain byte-exact across the baseline flip", {
  inputs <- utils::read.csv(golden_path("INPUTS.csv"), colClasses = "character")
  expect_identical(nrow(inputs), 21L)
  expect_false(anyDuplicated(inputs$file) > 0L)
  expect_identical(unique(inputs$retained_from_r_commit), "04aea4495a6c9875a6c8b06f45b7d4c466450a01")
  if (!golden_in_source()) inputs <- inputs[inputs$distribution == "shipped", , drop = FALSE]
  expect_identical(unname(tools::md5sum(golden_path(inputs$file))), inputs$md5)
})

test_that("named historical fixtures preserve their explicit version and original bytes", {
  for (name in c("table1-2.1.14", "table1-mixed-2.1.14-2.5.1")) {
    dir <- test_path("fixtures", "backcompat", name)
    a <- utils::read.csv(file.path(dir, "ARTIFACTS.csv"), colClasses = "character")
    expect_identical(nrow(a), 6L)
    expect_false(anyDuplicated(a$file) > 0L)
    expect_identical(unique(a$retained_from_r_commit), "04aea4495a6c9875a6c8b06f45b7d4c466450a01")
    expect_identical(unname(tools::md5sum(file.path(dir, a$file))), a$md5)
    bad <- file.path(withr::local_tempdir(), "mutant.csv")
    writeBin(c(golden_bytes(file.path(dir, a$file[1L])), charToRaw("fault")), bad)
    expect_false(identical(unname(tools::md5sum(bad)), a$md5[1L]))
  }
})

test_that("routed cells, console, sinks and full setup-sheet styles stay strict", {
  skip_if_not_installed("tidyxl")
  for (id in c("P01", "S08", "W15")) {
    w <- golden_read_cells(id)
    expect_identical(nrow(golden_compare_cells(w, w)), 0L)
    bad <- w
    bad[nrow(w), ncol(w)] <- paste0(w[nrow(w), ncol(w)], "fault")
    expect_gt(nrow(golden_compare_cells(bad, w)), 0L)
    console <- golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt")))
    expect_length(golden_compare_console(console, console), 0L)
    expect_gt(length(golden_compare_console(c(console, "fault"), console)), 0L)
    for (ext in c("csv", "md")) {
      want <- golden_artifact_path(id, paste0(id, ".", ext))
      expect_length(golden_compare_sink(want, want), 0L)
      bad <- file.path(withr::local_tempdir(), paste0(id, ".", ext))
      lines <- golden_read_lines(want)
      lines[1] <- paste0("fault", lines[1])
      golden_write_lf(lines, bad)
      expect_gt(length(golden_compare_sink(bad, want)), 0L)
    }
  }
  for (id in sprintf("K%02d", 1:9)) {
    sheets <- tidyxl::xlsx_sheet_names(golden_book(id))
    expect_setequal(sheets, tidyxl::xlsx_sheet_names(golden_path(paste0(id, ".xlsx"))))
    expect_true(all(c("Block Primary", "Block Dose") %in% sheets))
    for (sheet in sheets) {
      expect_length(golden_compare_styles(golden_book(id), sheet, golden_book(id), sheet,
                                          got_width_offset = golden_width_offset), 0L)
    }
    for (sheet in c("Block Primary", "Block Dose")) {
      # After the baseline flip both lookup paths name the same book. A real
      # copied-body-cell mutation supplies the independent negative control.
      cells <- golden_cell_styles(golden_book(id), sheet)
      hit <- which(cells$row >= 4L & cells$col >= 2L & nzchar(cells$value))[1L]
      expect_false(is.na(hit))
      cell <- cells[hit, , drop = FALSE]
      workbook <- openxlsx2::wb_load(golden_book(id))
      border <- function(x) if (is.na(x) || !nzchar(x)) "none" else x
      changed_top <- if (identical(cell$border_top, "double")) "medium" else "double"
      workbook <- openxlsx2::wb_add_border(workbook, sheet = sheet, dims = cell$address,
        bottom_border = border(cell$border_bottom), left_border = border(cell$border_left),
        right_border = border(cell$border_right), top_border = changed_top, update = TRUE)
      mutant <- file.path(withr::local_tempdir(), paste0(id, "-", gsub(" ", "-", sheet), ".xlsx"))
      openxlsx2::wb_save(workbook, mutant, overwrite = FALSE)
      expect_identical(tidyxl::xlsx_sheet_names(mutant), sheets)
      why <- golden_compare_styles(mutant, sheet, golden_book(id), sheet,
                                    got_width_offset = golden_width_offset)
      expect_gt(length(why), 0L)
      expect_true(any(grepl(paste0(cell$address, " border_top:"), why, fixed = TRUE)))
    }
  }
})

# These are synthetic inputs to the publication adapter, not Stata-generated
# artifacts. Immutable native prefixes and independent literal R paragraphs
# expose footer-routing failures without calling any production note helper.
test_that("publication contracts independently declare native and R paragraph differences", {
  skip_if_not_installed("tidyxl")
  cases <- list(
    R08 = c("Estimates from a single model.", "* p<.1, ** p<.05, *** p<.01"),
    R25m = c("Custom rows appended below model estimates.", "* p<0.05, ** p<0.01, *** p<0.001"),
    W16 = c("ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N.",
            paste0("Truncated l/u: weights below the l-th or above the u-th percentile of all weights",
                   " set to that percentile; Truncated (n) counts the weights changed.")),
    P13 = "tiny note"
  )
  for (id in names(cases)) {
    contract <- golden_publication_contract(id)
    expect_identical(contract$paragraphs, cases[[id]], label = id)
    expect_identical(length(contract$native_footers$csv), 1L)
    expect_identical(length(contract$native_footers$xlsx), 1L)
    expect_identical(contract$native_footers$console, character())
    expect_identical(contract$grid_end, nrow(golden_read_cells(id)) - 1L)
    expect_identical(contract$sheet_end, max(golden_cell_styles(golden_book(id), id)$row) - 1L)
  }
  expect_identical(golden_publication_contract("R08")$native_footers$csv, cases$R08[1])
  expect_identical(golden_publication_contract("R08")$native_footers$xlsx, paste(cases$R08, collapse = " "))
  expect_identical(golden_publication_contract("W16")$native_footers$csv, paste(cases$W16, collapse = " "))
  expect_identical(golden_publication_contract("P01")$paragraphs, character())
})

test_that("complete publication cell regions reject added, omitted and misplaced paragraphs", {
  skip_if_not_installed("tidyxl")
  id <- "R08"
  paragraphs <- c("Estimates from a single model.", "* p<.1, ** p<.05, *** p<.01")
  native <- golden_read_cells(id)
  prefix <- native[-nrow(native), , drop = FALSE]
  make <- function(x) rbind(prefix, cbind(x, matrix("", length(x), ncol(prefix) - 1L)))
  parts <- golden_publication_cells(make(paragraphs), id)
  expect_identical(parts$got, prefix)
  expect_identical(parts$want, prefix)
  faults <- list(c("Invented before", paragraphs), c(paragraphs[1], "Invented between", paragraphs[2]),
                 c(paragraphs, "Invented after"), paragraphs[-1], rev(paragraphs), c("Altered", paragraphs[2]))
  for (bad in faults) golden_expect_detected(golden_publication_cells(make(bad), id))
  bad <- make(paragraphs)
  bad[nrow(bad), ncol(bad)] <- "Leaked into trailing footer cell"
  golden_expect_detected(golden_publication_cells(bad, id))
  bad <- make(paragraphs)
  bad[3, 2] <- paste0(bad[3, 2], "body fault")
  parts <- golden_publication_cells(bad, id)
  expect_gt(nrow(golden_compare_cells(parts$got, parts$want)), 0L)
})

test_that("publication console boundary rejects earlier invented notes and keeps body edits", {
  skip_if_not_installed("tidyxl")
  id <- "R08"
  paragraphs <- c("Estimates from a single model.", "* p<.1, ** p<.05, *** p<.01")
  native <- golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt")))
  box <- golden_console_box(native)
  end <- tail(which(grepl("^\\s*\\+-+\\+\\s*$", box)), 1L)
  prefix <- box[seq_len(end)]
  make <- function(x) c(prefix, as.vector(rbind(x, "")))
  parts <- golden_publication_console(make(paragraphs), native, id)
  expect_length(golden_compare_console(parts$got, parts$want), 0L)
  for (bad in list(c("Invented before", paragraphs), c(paragraphs[1], "Invented between", paragraphs[2]),
                   c(paragraphs, "Invented after"), paragraphs[-1], rev(paragraphs))) {
    golden_expect_detected(golden_publication_console(make(bad), native, id))
  }
  bad <- make(paragraphs)
  bad[3] <- paste0(bad[3], "body fault")
  parts <- golden_publication_console(bad, native, id)
  expect_gt(length(golden_compare_console(parts$got, parts$want)), 0L)
})

test_that("publication sink adapters reject complete-region and line-ending faults", {
  skip_if_not_installed("tidyxl")
  id <- "R08"
  paragraphs <- c("Estimates from a single model.", "* p<.1, ** p<.05, *** p<.01")
  out <- withr::local_tempdir()
  for (ext in c("csv", "md")) {
    native <- golden_read_lines(golden_artifact_path(id, paste0(id, ".", ext)))
    end <- length(native) - if (ext == "csv") 1L else 2L
    prefix <- native[seq_len(end)]
    make <- function(x) {
      foot <- if (ext == "csv") golden_footer_csv_records(x, ncol(golden_read_cells(id))) else {
        if (length(x)) as.vector(rbind("", golden_fn_md(x))) else character()
      }
      path <- file.path(out, paste0("actual.", ext))
      golden_write_lf(c(prefix, foot), path)
      path
    }
    expect_length(golden_publication_sink(make(paragraphs), id, ext, ""), 0L)
    for (bad in list(c("Invented before", paragraphs), c(paragraphs[1], "Invented between", paragraphs[2]),
                     c(paragraphs, "Invented after"), paragraphs[-1], rev(paragraphs))) {
      golden_expect_detected(golden_publication_sink(make(bad), id, ext, ""))
    }
    path <- make(paragraphs)
    lines <- golden_read_lines(path)
    lines[1] <- paste0("body fault", lines[1])
    golden_write_lf(lines, path)
    expect_gt(length(golden_publication_sink(path, id, ext, "")), 0L)
    path <- make(paragraphs)
    bytes <- golden_bytes(path)
    writeBin(bytes[-length(bytes)], path)
    golden_expect_detected(golden_publication_sink(path, id, ext, ""))
  }
})

test_that("publication worksheet adapter asserts all rows, styles, merges and heights", {
  skip_if_not_installed("tidyxl")
  id <- "W16"
  paragraphs <- c("ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N.",
                  paste0("Truncated l/u: weights below the l-th or above the u-th percentile of all weights",
                         " set to that percentile; Truncated (n) counts the weights changed."))
  w <- golden_cell_styles(golden_book(id), id)
  wl <- golden_sheet_layout(golden_book(id), id)
  end <- max(w$row) - 1L
  note <- w[w$row == end + 1L, , drop = FALSE]
  g <- w[w$row <= end, , drop = FALSE]
  gl <- wl
  source_merge <- wl$merges[grepl(paste0("B", end + 1L, ":"), wl$merges, fixed = TRUE)]
  gl$merges <- setdiff(wl$merges, source_merge)
  source_height <- wl$heights[wl$heights$row == end + 1L, , drop = FALSE]
  gl$heights <- wl$heights[wl$heights$row <= end, , drop = FALSE]
  for (i in seq_along(paragraphs)) {
    row <- end + i
    current <- note
    current$row <- row
    current$address <- paste0(golden_col_letters(current$col), row)
    current$value[current$col == 2L] <- paragraphs[i]
    g <- rbind(g, current)
    gl$merges <- sort(c(gl$merges, gsub("[0-9]+", as.character(row), source_merge)))
    if (nrow(source_height)) gl$heights <- rbind(gl$heights, data.frame(row = row, height = source_height$height))
  }
  parts <- golden_publication_styles(g, w, gl, wl, id)
  expect_identical(parts$g$value, parts$w$value)
  expect_identical(parts$gl, parts$wl)
  bad <- g
  bad$value[bad$row == end + 1L & bad$col == 2L] <- "Invented before expected final paragraph"
  golden_expect_detected(golden_publication_styles(bad, w, gl, wl, id))
  for (attr in c("font", "wrap", "border_top", "fill")) {
    bad <- g
    k <- which(bad$row == end + 1L & bad$col == 2L)
    bad[[attr]][k] <- switch(attr, font = "Wrong font", wrap = FALSE, border_top = "thin", fill = "FF00FF00")
    golden_expect_detected(golden_publication_styles(bad, w, gl, wl, id))
  }
  bad_layout <- gl
  bad_layout$merges <- setdiff(gl$merges, tail(gl$merges, 1L))
  golden_expect_detected(golden_publication_styles(g, w, bad_layout, wl, id))
  bad_layout <- gl
  bad_layout$heights <- rbind(gl$heights, data.frame(row = end + 1L, height = 99))
  golden_expect_detected(golden_publication_styles(g, w, bad_layout, wl, id))
  bad <- g
  extra <- note
  extra$row <- end + 3L
  extra$address <- paste0(golden_col_letters(extra$col), extra$row)
  extra$value <- ""
  golden_expect_detected(golden_publication_styles(rbind(bad, extra), w, gl, wl, id))
  bad <- g
  bad$value[bad$row == 4L & bad$col == 2L] <- "body fault"
  parts <- golden_publication_styles(bad, w, gl, wl, id)
  expect_false(identical(parts$g$value, parts$w$value))
  # puttab writes the spacer and blank merged note cells too. Their style
  # corruption must not disappear with an otherwise correct note anchor.
  id <- "P13"
  w <- golden_cell_styles(golden_book(id), id)
  wl <- golden_sheet_layout(golden_book(id), id)
  expect_identical(golden_publication_styles(w, w, wl, wl, id)$g$value,
                   w$value[w$row < max(w$row)])
  for (col in c(1L, 3L)) {
    k <- which(w$row == max(w$row) & w$col == col)
    expect_length(k, 1L)
    bad <- w
    bad$fill[k] <- "FF00FF00"
    golden_expect_detected(golden_publication_styles(bad, w, wl, wl, id))
  }
})

test_that("native current SMD headers and notes are literal and mutant-sensitive", {
  note <- "SMD compares Primary vs Secondary only (the first two of 3 groups)."
  tt <- list(meta = list(console_before = paste("Note:", note)),
             cols = data.frame(role = c("label", "group", "group", "group", "p", "smd")),
             header = list(list(text = c("", "Primary", "Secondary", "Tertiary", "p-value", "SMD (Primary vs Secondary)"))),
             footnote = note)
  golden_assert_current_smd(tt)
  native <- golden_read_cells("T18")
  expect_identical(native[1L, 6L], "SMD (Primary vs Secondary)")
  expect_identical(native[nrow(native), 1L], note)
  expect_identical(golden_read_lines(golden_artifact_path("T18", "T18_console.txt"))[1L], paste("Note:", note))
  for (field in c("header", "footnote", "console")) {
    bad <- tt
    if (field == "header") bad$header[[1]]$text[6L] <- "SMD (Secondary vs Primary)"
    if (field == "footnote") bad$footnote <- paste0("Invented prior paragraph \\ ", note)
    if (field == "console") bad$meta$console_before <- "Note: SMD compares Primary vs Tertiary only (the first two of 3 groups)."
    golden_expect_detected(golden_assert_current_smd(bad))
  }
})

test_that("native scalar mask comparison projects only threshold and preserves P.4 metadata", {
  native <- golden_read_stored("S08")
  canonical <- list(threshold = 0L, mode = "primary", n_masked = 0L, n_linked = 0L)
  stored <- list(smallcells = canonical)
  expect_length(golden_compare_stored(stored, native, fields = "smallcells"), 0L)
  expect_identical(stored$smallcells, canonical)
  bad <- stored
  bad$smallcells$threshold <- 1L
  expect_gt(length(golden_compare_stored(bad, native, fields = "smallcells")), 0L)
  bad <- stored
  bad$smallcells$threshold <- NULL
  expect_gt(length(golden_compare_stored(bad, native, fields = "smallcells")), 0L)
  golden_assert_stratetab_mask_contract(list(stored = stored), "S08")
  for (field in names(canonical)) {
    bad <- stored
    bad$smallcells[[field]] <- if (field == "mode") "strict" else 1L
    golden_expect_detected(golden_assert_stratetab_mask_contract(list(stored = bad), "S08"))
  }
})

test_that("annotation scanner uses the statically returned command across setup and missing slots", {
  expected <- list(T34 = NULL, K05 = NULL, K06 = NULL, K08 = NULL, W18 = "", W22 = NULL)
  for (id in names(expected)) expect_identical(golden_fn_user(golden_scenario(id)), expected[[id]], label = id)
  # Earlier note-bearing setup calls must not bleed into the returned table.
  scenario <- list(command = "puttab", r_call = 'puttab(data, footnote = "Setup"); suppressMessages(puttab(data, footnote = "Returned"))')
  expect_identical(golden_fn_user(scenario), "Returned")
  scenario$r_call <- 'data <- data[data$x > 1, ]; puttab(data, footnote = "Returned")'
  expect_identical(golden_fn_user(scenario), "Returned")
  scenario$r_call <- 'if (flag) puttab(data, footnote = "One") else puttab(data, footnote = "Two")'
  expect_error(golden_fn_user(scenario), "identifiable final")
})

test_that("R publication counts are exact before nonmutating native wttab projection", {
  skip_if_not_installed("tidyxl")
  for (id in c("W16", "W18", "W19", "W21", "W22")) {
    contract <- golden_publication_contract(id)
    original <- structure(list(stored = list(n_rows = contract$grid_end + length(contract$paragraphs),
                                             n_datarows = 71L, custom = "preserved")), class = "tt_table")
    projected <- golden_wttab_publication_projection(original, id)
    expect_identical(projected$stored$n_rows, nrow(contract$native_grid))
    expect_identical(original$stored$n_rows, contract$grid_end + length(contract$paragraphs))
    expect_identical(projected$stored[c("n_datarows", "custom")], original$stored[c("n_datarows", "custom")])
    bad <- original
    bad$stored$n_rows <- bad$stored$n_rows + 1L
    golden_expect_detected(golden_wttab_publication_projection(bad, id))
  }
})

test_that("CSV footer serialization retains exact quoting and empty-field bytes", {
  skip_if_not_installed("tidyxl")
  expect_identical(golden_footer_csv_records(c("tiny note", "a,b", 'a"b'), 3L),
                   c("tiny note,,", '"a,b",,', '"a""b",,'))
  id <- "P13"
  native <- golden_artifact_path(id, paste0(id, ".csv"))
  actual <- file.path(withr::local_tempdir(), "actual.csv")
  file.copy(native, actual)
  expect_length(golden_publication_sink(actual, id, "csv", ""), 0L)
  lines <- golden_read_lines(native)
  lines[length(lines)] <- sub("tiny note", '"tiny note"', lines[length(lines)], fixed = TRUE)
  golden_write_lf(lines, actual)
  golden_expect_detected(golden_publication_sink(actual, id, "csv", ""))
  lines <- golden_read_lines(native)
  lines[length(lines)] <- paste0(lines[length(lines)], ",")
  golden_write_lf(lines, actual)
  golden_expect_detected(golden_publication_sink(actual, id, "csv", ""))
})

test_that("both native footer style mutations and duplicate actual addresses are rejected", {
  skip_if_not_installed("tidyxl")
  id <- "P13"
  w <- golden_cell_styles(golden_book(id), id)
  wl <- golden_sheet_layout(golden_book(id), id)
  end <- max(w$row)
  golden_publication_styles(w, w, wl, wl, id)
  for (attr in c("fill", "font", "border_left", "wrap", "number_format")) {
    bad_native <- w
    k <- which(w$row == end)
    bad_native[[attr]][k] <- switch(attr, fill = "FF00FF00", font = "Wrong font", border_left = "thin", wrap = FALSE, number_format = "0.00")
    golden_expect_detected(golden_publication_styles(w, bad_native, wl, wl, id))
  }
  duplicate <- w[w$row == end & w$col == 3L, , drop = FALSE]
  expect_identical(nrow(duplicate), 1L)
  duplicate$fill <- "FF00FF00"
  golden_expect_detected(golden_publication_styles(rbind(w, duplicate), w, wl, wl, id))
  child <- which(w$row == end & w$col == 3L)
  for (side in c("R", "native")) {
    removed <- w[-child, , drop = FALSE]
    added <- w[child, , drop = FALSE]
    added$col <- max(w$col) + 1L
    added$address <- paste0(golden_col_letters(added$col), end)
    for (bad in list(removed, rbind(w, w[child, , drop = FALSE]), rbind(w, added))) {
      golden_expect_detected(golden_publication_styles(if (side == "R") bad else w,
                                                      if (side == "native") bad else w, wl, wl, id))
    }
  }
})

test_that("complete native footer inventory supports exact command serialization templates", {
  skip_if_not_installed("tidyxl")
  sparse_ids <- character()
  footer_ids <- character()
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    contract <- golden_publication_contract(id)
    if (!length(contract$native_footers$xlsx)) next
    footer_ids <- c(footer_ids, id)
    templates <- golden_footer_style_templates(contract, id)
    standard <- golden_scenario(id)$command %in% c("table1_tc", "regtab", "stratetab", "effecttab", "comptab", "hrcomptab")
    if (standard) {
      sparse_ids <- c(sparse_ids, id)
      expect_identical(templates$R$col, 2L)
      expect_identical(templates$R$address, paste0("B", contract$sheet_end + 1L))
    } else expect_identical(templates$R, templates$native)
    expect_identical(templates$halign, if (golden_scenario(id)$command == "stacktab") "general" else "left")
  }
  # Explicit complete inventory from the reviewed native-byte snapshot.
  expect_setequal(sparse_ids, c("T18", "T20a", "T20b", "T20c", paste0("T", 25:29),
    paste0("T30", c("d", "e", "f", "g", "h", "l", "m", "n")),
    "R08", "R25k", "R25l", "R25m", "S01", "S07", "E02", "E04", "E19", "E24", "W13",
    "C02", "C03", "C04", "C06"))
  expect_length(footer_ids, 47L)
})

test_that("lossless merged footer serialization still detects every child mutation", {
  skip_if_not_installed("tidyxl")
  paragraphs <- list(S01 = "IRR = incidence rate ratio, Female vs Male. CI by log-normal method.",
                     R08 = c("Estimates from a single model.", "* p<.1, ** p<.05, *** p<.01"))
  for (id in names(paragraphs)) {
    w <- golden_cell_styles(golden_book(id), id)
    wl <- golden_sheet_layout(golden_book(id), id)
    end <- max(w$row) - 1L
    note <- w[w$row == end + 1L & w$col == 2L, , drop = FALSE]
    g <- w[w$row <= end, , drop = FALSE]
    gl <- wl
    source_merge <- wl$merges[grepl(paste0("B", end + 1L, ":"), wl$merges, fixed = TRUE)]
    gl$merges <- setdiff(wl$merges, source_merge)
    source_height <- wl$heights[wl$heights$row == end + 1L, , drop = FALSE]
    gl$heights <- wl$heights[wl$heights$row <= end, , drop = FALSE]
    for (i in seq_along(paragraphs[[id]])) {
      row <- end + i
      anchor <- note
      anchor$row <- row
      anchor$address <- paste0("B", row)
      anchor$value <- paragraphs[[id]][i]
      g <- rbind(g, anchor)
      gl$merges <- sort(c(gl$merges, gsub("[0-9]+", as.character(row), source_merge)))
      if (nrow(source_height)) gl$heights <- rbind(gl$heights, data.frame(row = row, height = source_height$height))
    }
    rownames(g) <- NULL
    parts <- golden_publication_styles(g, w, gl, wl, id)
    expect_identical(parts$g, parts$w)
    expect_identical(parts$gl, parts$wl)
    child <- which(w$row == end + 1L & w$col == 3L)
    expect_length(child, 1L)
    added <- w[child, , drop = FALSE]
    extra_native <- added
    extra_native$col <- max(w$col) + 1L
    extra_native$address <- paste0(golden_col_letters(extra_native$col), end + 1L)
    for (bad in list(w[-child, , drop = FALSE], rbind(w, added), rbind(w, extra_native))) {
      golden_expect_detected(golden_publication_styles(g, bad, gl, wl, id))
    }
    for (attr in c("value", "font", "size", "font_color", "bold", "italic", "wrap", "halign", "valign",
                   "fill", "border_top", "border_bottom", "border_left", "border_right", "number_format", "format_id")) {
      bad <- w
      bad[[attr]][child] <- switch(attr, value = "invented child", font = "Wrong font", size = 99,
        font_color = "FF000000", bold = TRUE, italic = TRUE, wrap = TRUE, halign = "right", valign = "top",
        fill = "FF00FF00", border_top = "thin", border_bottom = "thin", border_left = "thin",
        border_right = "thin", number_format = "0.00", format_id = 2L)
      golden_expect_detected(golden_publication_styles(g, bad, gl, wl, id))
    }
    # R's declared representation contains only anchors. A redundant extra
    # default child, a styled extra child, a duplicate or a missing anchor
    # each violates its exact address contract.
    styled <- added
    styled$fill <- "FF00FF00"
    anchor <- which(g$row == end + 1L & g$col == 2L)
    for (bad in list(rbind(g, added), rbind(g, styled), rbind(g, g[anchor, , drop = FALSE]),
                     g[-anchor, , drop = FALSE])) {
      golden_expect_detected(golden_publication_styles(bad, w, gl, wl, id))
    }
    contract <- golden_publication_contract(id)
    contract$native_styles$fill[contract$native_styles$row == end + 1L & contract$native_styles$col == 3L] <- "FF00FF00"
    golden_expect_detected(golden_footer_style_templates(contract, id))
  }
})

# ---------------------------------------------------------------------------
# Cells

test_that("cell comparator accepts every golden against itself", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    w <- golden_read_cells(id)
    expect_identical(nrow(golden_compare_cells(w, w, golden_scenario(id)$mask)), 0L, label = id)
  }
})

test_that("cell comparator rejects broken cells", {
  w <- golden_read_cells("T01")
  row_primary <- which(w[, 1] == "   Primary")
  expect_length(row_primary, 1L)

  # A lost indent is a label mismatch, whatever the mode.
  b <- w
  b[row_primary, 1] <- "Primary"
  for (mode in c("exact", "tolerance", "structure")) {
    mm <- golden_compare_cells(b, w, "p,test,statistic", mode)
    expect_identical(mm$why, "text")
  }

  # One digit off in a value cell.
  b <- w
  age <- which(w[, 1] == "Age at cohort entry (years)")
  b[age, 2] <- "58.4±13.4"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "exact")), 1L)
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 0L)
  b[age, 2] <- "58.5±13.4"
  expect_identical(golden_compare_cells(b, w, "p", "tolerance")$why, "tolerance")
  expect_identical(nrow(golden_compare_cells(b, w, "p", "structure")), 0L)
  b[age, 2] <- "58.3 (13.4)"
  expect_identical(golden_compare_cells(b, w, "p", "structure")$why, "skeleton")

  # Thousands separators are part of the number, not the skeleton.
  fem <- which(w[, 1] == "Female sex")
  b <- w
  b[fem, 2] <- "5,352 (60)"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 0L)
  b[fem, 2] <- "5,353 (60)"
  expect_identical(nrow(golden_compare_cells(b, w, "p", "tolerance")), 1L)

  # Masked p-values: any text passes, but blank vs non-blank does not, and
  # the header text stays exact.
  pcol <- which(w[2, ] == "p-value")
  b <- w
  b[age, pcol] <- "0.031"
  expect_identical(nrow(golden_compare_cells(b, w, "p,test,statistic")), 0L)
  expect_identical(nrow(golden_compare_cells(b, w, character())), 1L)
  b[age, pcol] <- ""
  expect_identical(golden_compare_cells(b, w, "p")$why, "masked blank pattern")
  b <- w
  b[2, pcol] <- "P"
  expect_identical(nrow(golden_compare_cells(b, w, "p")), 1L)

  # Shape.
  expect_identical(golden_compare_cells(w[-1, ], w)$why, "dimensions")
})

test_that("cell numbers carry the unit of their last displayed digit", {
  n <- golden_cell_numbers("1,234 (5.67) 1.0e+08 -0.125")
  expect_equal(n$value, c(1234, 5.67, 1e8, -0.125))
  expect_equal(n$unit, c(1, 0.01, 1e7, 0.001))
})

test_that("tt_table objects are flattened to the CSV grid", {
  tt <- structure(list(
    title = "A title",
    header = list(list(text = c(" ", "G1", "p-value")), list(text = c("Desc", "N=5", ""))),
    body = data.frame(a = c("Var", "   lvl"), b = c("1 (20)", "2"), c = c("0.5", "")),
    footnote = "Note."
  ), class = "tt_table")
  m <- golden_as_cells(tt)
  expect_identical(dim(m), c(6L, 3L))
  expect_identical(m[1, ], c("A title", "", ""))
  expect_identical(m[5, 1], "   lvl")
  expect_identical(m[6, ], c("Note.", "", ""))
})

# ---------------------------------------------------------------------------
# Sinks

copy_golden <- function(id, ext) {
  f <- tempfile(fileext = paste0(".", ext))
  file.copy(golden_path(paste0(id, ".", ext)), f)
  f
}

edit_file <- function(path, from, to) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit <- grep(from, x, fixed = TRUE)
  stopifnot(length(hit) >= 1L)
  x[hit[1]] <- sub(from, to, x[hit[1]], fixed = TRUE)
  golden_write_lf(x, path)
  path
}

test_that("sink comparator accepts every golden against itself", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    for (ext in c("csv", "md")) {
      f <- golden_path(paste0(id, ".", ext))
      expect_length(golden_compare_sink(f, f, golden_scenario(id)$mask), 0L)
    }
  }
})

test_that("CSV sink comparator rejects broken bytes and ignores masked p-values", {
  g <- golden_path("T01.csv")
  expect_length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ",0.031"), g, "p"), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ",0.031"), g, "")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), "   Primary", "  Primary"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), ",0.24", ","), g, "p")), 0L)
  # Quoting a field that needs no quotes changes the bytes.
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "csv"), "58.3±13.4", "\"58.3±13.4\""), g, "p")), 0L)
  # CRLF line endings.
  f <- copy_golden("T01", "csv")
  x <- readLines(f, encoding = "UTF-8")
  con <- file(f, "wb")
  writeLines(x, con, sep = "\r\n", useBytes = TRUE)
  close(con)
  expect_true("line endings differ" %in% golden_compare_sink(f, g, "p"))
  # Unmasked scenario: byte compare.
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T02", "csv"), "0.0", "0.1"), golden_path("T02.csv"), "")), 0L)
})

test_that("Markdown sink comparator rejects broken bytes and ignores masked p-values", {
  g <- golden_path("T01.md")
  expect_length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "| 0.24 |", "| 0.031 |"), g, "p"), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "| 0.24 |", "|  |"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "&nbsp;&nbsp;&nbsp;Primary", "Primary"), g, "p")), 0L)
  expect_gt(length(golden_compare_sink(edit_file(copy_golden("T01", "md"), "### Table 1", "## Table 1"), g, "p")), 0L)
  # mdappend output holds two tables; both are masked.
  g34 <- golden_path("T34.md")
  f <- copy_golden("T34", "md")
  x <- readLines(f, encoding = "UTF-8")
  x <- sub("| 0.24 |", "| 0.5 |", x, fixed = TRUE)
  golden_write_lf(x, f)
  expect_length(golden_compare_sink(f, g34, "p"), 0L)
  expect_gt(length(golden_compare_sink(f, g34, "")), 0L)
})

# ---------------------------------------------------------------------------
# Console

test_that("console comparator accepts goldens and rejects broken listings", {
  for (id in golden_ids_in_build(golden_scenarios()$id)) {
    x <- golden_read_lines(golden_path(paste0(id, "_console.txt")))
    expect_length(golden_compare_console(x, x, golden_scenario(id)$mask), 0L)
  }
  w <- golden_read_lines(golden_path("T01_console.txt"))
  # Sink messages are not part of the listing.
  expect_length(golden_compare_console(golden_console_box(w), w, "p"), 0L)
  # Masked p-value text passes; a changed label or value does not.
  b <- sub("0.24    |", "0.031   |", w, fixed = TRUE)
  expect_false(identical(b, w))
  expect_length(golden_compare_console(b, w, "p,test,statistic"), 0L)
  expect_gt(length(golden_compare_console(b, w, "")), 0L)
  expect_gt(length(golden_compare_console(sub("58.3", "58.4", w, fixed = TRUE), w, "p")), 0L)
  expect_gt(length(golden_compare_console(sub("|    Primary", "|   Primary ", w, fixed = TRUE), w, "p")), 0L)
  expect_gt(length(golden_compare_console(w[-5], w, "p")), 0L)
  # A value on a level row (p blank) is not masked.
  expect_gt(length(golden_compare_console(sub("1,527 (25)", "1,528 (25)", w, fixed = TRUE), w, "p")), 0L)
  # Masked text may change width: widen the p column by 4 on every box line
  # (as a longer p-value would), then put a longer value in it.
  sp <- golden_console_span(w, "p-value")
  box <- golden_is_box_line(w)
  wide <- w
  wide[box] <- paste0(substr(w[box], 1L, sp[2]),
                      ifelse(golden_is_rule_line(w[box]), "----", "    "),
                      substring(w[box], sp[2] + 1L))
  wide <- sub("0.80        |", "<0.0001     |", wide, fixed = TRUE)
  expect_true(any(grepl("<0.0001", wide, fixed = TRUE)))
  expect_length(golden_compare_console(wide, w, "p"), 0L)
  # Test/Statistic/p-value masked together; SMD after them stays exact.
  s <- golden_read_lines(golden_path("T30e_console.txt"))
  expect_length(golden_compare_console(sub("Ind. t test", "Welch-t-tst", s, fixed = TRUE), s, "p,test,statistic"), 0L)
  smd <- s[grepl("^\\s*\\| Age at cohort entry", s)]
  smd_val <- sub("^.*\\s(\\S+)\\s+\\|\\s*$", "\\1", smd)
  expect_gt(length(golden_compare_console(sub(paste0(smd_val, " "), "9.999 ", s, fixed = TRUE), s, "p,test,statistic")), 0L)
  # Chatter printed before the box by commands inside the engine is dropped.
  m <- golden_read_lines(golden_path("T13_console.txt"))
  expect_true(grepl("^\\(", m[1]))
  expect_length(golden_compare_console(golden_console_box(m), m, "p"), 0L)
  expect_true(grepl("^\\s*\\+-", golden_console_box(m)[1]))
  # Wrapped log lines ("> " continuations) are rejoined.
  long <- w[1]
  wrapped <- c(substr(long, 1, 20), paste0("> ", substring(long, 21)), w[-1])
  expect_length(golden_compare_console(wrapped, w, "p"), 0L)
  # Unmasked (regtab): exact characters, including alignment.
  r <- golden_read_lines(golden_path("R01_console.txt"))
  expect_gt(length(golden_compare_console(sub("  1.00", " 1.00 ", r, fixed = TRUE), r, "")), 0L)
})

# ---------------------------------------------------------------------------
# Stored results

test_that("stored-results comparator checks names, values, and tolerance", {
  g <- golden_read_stored("R01")
  got <- g[g$kind != "meta", ]
  # R's methods text has no Stata software sentence; the golden's is dropped.
  expect_gt(length(golden_compare_stored(got, g)), 0L)
  meth <- got$name == "methods"
  got$value[meth] <- sub(" Analysis performed in Stata.*$", "", got$value[meth])
  expect_length(golden_compare_stored(got, g), 0L)
  b <- got
  b$value[meth] <- sub("Odds ratios", "Hazard ratios", b$value[meth])
  expect_gt(length(golden_compare_stored(b, g)), 0L)

  b <- got
  b$value[b$name == "N_rows"] <- "14"
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  b <- got
  b$value[b$name == "coef_label"] <- "HR"
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  b <- got[got$name != "methods", ]
  expect_true(any(grepl("missing macro methods", golden_compare_stored(b, g))))

  k <- which(got$name == "table")[1]
  b <- got
  b$value[k] <- sprintf("%.17g", as.numeric(got$value[k]) * (1 + 1e-12))
  expect_length(golden_compare_stored(b, g), 0L)
  b$value[k] <- sprintf("%.17g", as.numeric(got$value[k]) * (1 + 1e-6))
  expect_gt(length(golden_compare_stored(b, g)), 0L)
  expect_length(golden_compare_stored(b, g, tolerance = 1e-3), 0L)
  expect_length(golden_compare_stored(b, g, fields = c("N_rows", "coef_label")), 0L)
})

test_that("stored-results comparator is two-way: extra R results fail (Phase 4 review P1-4)", {
  g <- golden_read_stored("R01")
  got <- g[g$kind != "meta", ]
  meth <- got$name == "methods"
  got$value[meth] <- sub(" Analysis performed in Stata.*$", "", got$value[meth])
  expect_length(golden_compare_stored(got, g), 0L)
  # An extra r(table) row (mutation M26: Reference rows in r(table)).
  tab <- got[got$name == "table", ]
  extra <- tab[1, ]
  extra$row <- "Reference_row"
  b <- rbind(got, extra)
  why <- golden_compare_stored(b, g)
  expect_true(any(grepl("^extra matrix table\\[Reference_row", why)))
  expect_true(any(grepl("^extra", golden_compare_stored(b, g, fields = "table"))))
  # A repeated key is an extra occurrence.
  b <- rbind(got, got[got$name == "coef_label", ])
  expect_true(any(grepl("^extra macro coef_label", golden_compare_stored(b, g))))
  # Names the golden lacks entirely (R-only results) are not compared, nor
  # are names outside `fields`.
  b <- rbind(got, data.frame(name = "r_only_result", kind = "scalar", row = "", col = "", value = "1"))
  expect_length(golden_compare_stored(b, g), 0L)
  b <- rbind(got, extra)
  expect_length(golden_compare_stored(b, g, fields = c("N_rows", "coef_label")), 0L)
  # End to end: a tt_table whose r(table) has one row too many fails.
  tt <- regtab(lm(price ~ mpg + mpg_dup + foreign * rep78,
                  data = golden_fixture("auto", factors = c("foreign", "rep78"))),
               refcat = "Ref.", omitlabel = "Dropped", emptylabel = "No obs",
               interactions = "native", models = "Constrained")
  expect_length(golden_compare_stored(tt$stored, golden_read_stored("R12"), fields = "table", tolerance = 1e-9), 0L)
  tt$stored$table <- rbind(tt$stored$table, Domestic = NA_real_)
  expect_true(any(grepl("^extra matrix table\\[Domestic", golden_compare_stored(tt$stored, golden_read_stored("R12"),
                                                                                fields = "table"))))
})

test_that("masked p_value rows are not counted as extra", {
  g <- golden_read_stored("T01")
  tab <- g[g$name == "table", ]
  m <- matrix(golden_stored_num(tab$value), ncol = 1, dimnames = list(tab$row, "p_value"))
  m[1, 1] <- 0.5
  expect_true(any(grepl("table", golden_compare_stored(list(table = m), g, fields = "table"))))
  expect_length(golden_compare_stored(list(table = m), g, fields = "table", mask = "p"), 0L)
})

test_that("stored lists flatten to the golden shape; mask p drops test p-values", {
  g <- golden_read_stored("T01")
  tab <- g[g$name == "table", ]
  m <- matrix(golden_stored_num(tab$value), ncol = 1, dimnames = list(tab$row, "p_value"))
  stored <- list(
    Dapa = g$value[g$name == "Dapa"],
    varlist = g$value[g$name == "varlist"],
    table = m
  )
  expect_length(golden_compare_stored(stored, g, fields = c("Dapa", "varlist", "table")), 0L)
  m2 <- m
  m2[1, 1] <- 0.5
  stored$table <- m2
  expect_gt(length(golden_compare_stored(stored, g, fields = "table")), 0L)
  expect_length(golden_compare_stored(stored, g, fields = "table", mask = "p"), 0L)
  # methods is compared under mask p (after the R test-name swap).
  expect_true(any(grepl("missing macro methods", golden_compare_stored(stored, g, fields = "methods", mask = "p"))))
  # Stata missing (".", ".d") equals NA.
  expect_identical(golden_stored_num(c(".", ".d", "0.5")), c(NA, NA, 0.5))
})

# ---------------------------------------------------------------------------
# Excel styling

test_that("style comparator accepts golden sheets against themselves", {
  skip_if_not_installed("tidyxl")
  for (id in c("T01", "T19", "T20c", "T29", "R01", "R12", "R15", "R29")) {
    wb <- golden_path(paste0(golden_scenario(id)$command, ".xlsx"))
    expect_length(golden_compare_styles(wb, id, wb, id, golden_scenario(id)$mask,
                                        got_width_offset = golden_width_offset), 0L)
  }
})

test_that("style comparator detects border, font, fill, and merge differences", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  cmp <- function(a, b, mask = "p,test,statistic") {
    golden_compare_styles(wb, a, wb, b, mask, got_width_offset = golden_width_offset)
  }
  # default (thin) vs medium borders, Calibri 11 vs Times New Roman 12
  d <- cmp("T20a", "T20b")
  expect_true(any(grepl("border_top: 'thin' vs 'medium'", d, fixed = TRUE)))
  expect_true(any(grepl("font: 'Calibri' vs 'Times New Roman'", d, fixed = TRUE)))
  # medium vs academic: academic drops the verticals
  d <- cmp("T20b", "T20c")
  expect_true(any(grepl("border_left: 'medium' vs 'NA'", d, fixed = TRUE)))
  # zebra/headershade fills and the SMD column's extra merge
  d <- cmp("T01", "T19")
  expect_true(any(grepl("fill", d, fixed = TRUE)))
  expect_true(any(grepl("^merges", d)))
  # regtab: reference-row merges differ between R01 and R02 (compact)
  rb <- golden_path("regtab.xlsx")
  d <- golden_compare_styles(rb, "R01", rb, "R02", got_width_offset = golden_width_offset)
  expect_true(any(grepl("^merges", d)))
})

# Rewrite one sheet's XML inside a copy of a golden workbook.
edit_sheet_xml <- function(xlsx, sheet, from, to) {
  skip_if(!nzchar(Sys.which("zip")), "zip not available")
  tmp <- tempfile("xl")
  dir.create(tmp)
  utils::unzip(xlsx, exdir = tmp)
  wbxml <- paste(readLines(file.path(tmp, "xl/workbook.xml"), warn = FALSE), collapse = "")
  rels <- paste(readLines(file.path(tmp, "xl/_rels/workbook.xml.rels"), warn = FALSE), collapse = "")
  tag <- regmatches(wbxml, regexpr(sprintf('<sheet [^>]*name="%s"[^>]*>', sheet), wbxml))
  rid <- sub('^.*r:id="([^"]*)".*$', "\\1", tag)
  rel <- regmatches(rels, regexpr(sprintf('<Relationship [^>]*Id="%s"[^>]*>', rid), rels))
  target <- sub("^/?(xl/)?", "xl/", sub('^.*Target="([^"]*)".*$', "\\1", rel))
  f <- file.path(tmp, target)
  x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  stopifnot(grepl(from, x, perl = TRUE))
  golden_write_lf(sub(from, to, x, perl = TRUE), f)
  out <- tempfile(fileext = ".xlsx")
  old <- setwd(tmp)
  on.exit(setwd(old))
  utils::zip(out, files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-q -X")
  out
}

test_that("style comparator detects edited merges, widths, heights, and cell styles", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  cmp <- function(f, mask = "p,test,statistic") {
    golden_compare_styles(f, "T01", wb, "T01", mask, got_width_offset = golden_width_offset)
  }
  expect_length(cmp(edit_sheet_xml(wb, "T01", "(?!)|$", "")), 0L)
  expect_true(any(grepl("^merges", cmp(edit_sheet_xml(wb, "T01", '<mergeCell ref="B2:B3"/>', "")))))
  expect_true(any(grepl("width", cmp(edit_sheet_xml(wb, "T01", '(<col [^>]*min="3"[^>]*width=")[0-9.]+', "\\140")))))
  expect_true(any(grepl("height", cmp(edit_sheet_xml(wb, "T01", '(<row r="1"[^>]*ht=")[0-9.]+', "\\145")))))
  # Point cell C5 at A1's style (bold, left-aligned title style).
  a1 <- regmatches(golden_sheet_xml(wb, "T01"), regexpr('<c r="A1"[^>]* s="[0-9]+"', golden_sheet_xml(wb, "T01")))
  s_a1 <- sub('^.* s="([0-9]+)"$', "\\1", a1)
  d <- cmp(edit_sheet_xml(wb, "T01", '(<c r="C5"[^>]* s=")[0-9]+', paste0("\\1", s_a1)))
  expect_true(any(grepl("^C5 ", d)))
})

test_that("pstyle mask ignores p-dependent bold and highlight fill only", {
  skip_if_not_installed("tidyxl")
  wb <- golden_path("table1_tc.xlsx")
  x <- golden_cell_styles(wb, "T19")
  hl <- x[x$fill == "FFFFFFCC", ]
  skip_if(!nrow(hl), "T19 has no highlighted rows")
  # Give a highlighted cell the style of an unhighlighted body cell in the same column.
  cell <- hl$address[hl$col == 3L][1]
  plain <- x$address[x$col == 3L & x$row > 4L & x$fill != "FFFFFFCC" & x$row %% 2L == hl$row[hl$address == cell] %% 2L][1]
  xml <- golden_sheet_xml(wb, "T19")
  s_plain <- sub('^.* s="([0-9]+)"$', "\\1", regmatches(xml, regexpr(sprintf('<c r="%s"[^>]* s="[0-9]+"', plain), xml)))
  f <- edit_sheet_xml(wb, "T19", sprintf('(<c r="%s"[^>]* s=")[0-9]+', cell), paste0("\\1", s_plain))
  expect_length(golden_compare_styles(f, "T19", wb, "T19", "p,test,statistic,pstyle",
                                      got_width_offset = golden_width_offset), 0L)
  expect_gt(length(golden_compare_styles(f, "T19", wb, "T19", "p,test,statistic",
                                         got_width_offset = golden_width_offset)), 0L)
})

# ---------------------------------------------------------------------------
# Row-wise p mask (Phase 2 review P1-1/P1-2/P1-3): p cells, r(table)
# p_value, and boldp/highlight are compared exactly on rows whose R test is
# Stata's test (cat, bin, cate, bine, conts with > 2 groups). Each check
# shows the mutation is caught with the row-wise mask and was invisible to
# the old whole-column mask (p_rows = all TRUE / p_table = NULL).

golden_eval_edited <- function(id, edit, dir) {
  sc <- golden_scenario(id)
  code <- edit(gsub("@ID@", golden_code_path(dir, id), sc$r_call, fixed = TRUE))
  eval(parse(text = code), envir = new.env(parent = environment(golden_fixture)))
}

test_that("row-wise p mask covers exactly the rows whose R test differs from Stata's", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T30e", identity, withr::local_tempdir())
  pr <- golden_p_masked_rows(tt)
  vt <- tt$rows$vtype
  expect_true(all(pr[vt %in% c("contn", "contln", "conts")]))
  expect_false(any(pr[vt %in% c("cat", "bin")]))
  # Two groups: conts is masked; with more groups (Kruskal-Wallis) it is not.
  tt3 <- golden_eval_edited("T18", function(x) sub(")$", ", vars = c(index_age = \"conts\", female = \"bin\"))",
                                                        sub("vars = c\\(.*\\), smd", "smd", x)),
                            withr::local_tempdir())
  expect_false(any(golden_p_masked_rows(tt3)))
})

test_that("row-wise p mask: a Yates-corrected chi-squared p fails", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T01", function(x) sub(")$", ", test_args = list(chisq.test = list(correct = TRUE)))", x),
                           withr::local_tempdir())
  g <- golden_read_stored("T01")
  why <- golden_compare_stored(tt$stored, g, fields = "table", mask = "p,test,statistic",
                               p_table = golden_table_p_rows(tt))
  expect_gt(length(why), 0L)
  expect_true(all(grepl("p_value", why)))
  expect_length(golden_compare_stored(tt$stored, g, fields = "table", mask = "p,test,statistic"), 0L)
  # The label and the methods paragraph name the correction (P3-8).
  expect_match(tt$stored$methods, "with Yates' continuity correction", fixed = TRUE)
  expect_gt(length(golden_compare_stored(tt$stored, g, fields = "methods", mask = "p")), 0L)
})

test_that("row-wise p mask: a wrong pdp/highpdp fails", {
  skip_if_not_installed("haven")
  tt <- golden_eval_edited("T17", function(x) sub("pdp = 4, highpdp = 3", "pdp = 3, highpdp = 2", x, fixed = TRUE),
                           withr::local_tempdir())
  want <- golden_read_cells("T17")
  mm <- golden_compare_cells(tt, want, "p,test,statistic")
  expect_gt(nrow(mm), 0L)
  expect_true(all(want[2, mm$col] == "" & want[1, mm$col] == "p-value"))
  expect_identical(nrow(golden_compare_cells(tt, want, "p,test,statistic",
                                             p_rows = rep(TRUE, nrow(want)))), 0L)
})

test_that("row-wise pstyle mask: dropping boldp fails", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  dir <- withr::local_tempdir()
  tt <- golden_eval_edited("T19", function(x) sub("boldp = 0.05, ", "", x, fixed = TRUE), dir)
  xlsx <- file.path(dir, "t19.xlsx")
  tt_write_xlsx(tt, xlsx, sheet = "T19")
  wb <- golden_path("table1_tc.xlsx")
  mask <- "p,test,statistic,pstyle"
  why <- golden_compare_styles(xlsx, "T19", wb, "T19", mask, got_width_offset = golden_r_width_offset,
                               p_rows = golden_p_masked_rows(tt))
  expect_gt(length(why), 0L)
  expect_true(all(grepl(" bold: ", why, fixed = TRUE)))
  expect_length(golden_compare_styles(xlsx, "T19", wb, "T19", mask, got_width_offset = golden_r_width_offset), 0L)
})

# ---------------------------------------------------------------------------
# Derived small-cell suppression (Phase 3 review P1-1): "Suppressed" in a
# masked Test/Statistic/p-value cell is compared exactly on every path, so a
# leaked test name, statistic, or p-value of a protected variable fails
# (mutants M16 and M47 of the review were silent before).

p1_leak <- function(tt, role, value) {
  j <- which(tt$cols$role == role)
  i <- which(golden_suppressed(tt$body[[j]]))[1]
  tt$body[[j]][i] <- value
  tt
}

test_that("a leaked test name, statistic, or p-value fails the cell and console comparators", {
  skip_if_not_installed("haven")
  tt <- table1_tc(sc_pipeline_data("sccont"), by = "group", vars = "value contn \\ category cat",
                  missingsummary = TRUE, test = TRUE, statistic = TRUE, smd = TRUE, smallcells = 5)
  dir <- test_path("fixtures", "table1_phase3")
  want <- golden_read_cells_file(file.path(dir, "S3.csv"))
  cons <- golden_read_lines(file.path(dir, "S3_console.txt"))
  mask <- "p,test,statistic"
  expect_identical(nrow(golden_compare_cells(tt, want, mask)), 0L)
  expect_length(golden_compare_console(utils::capture.output(print(tt)), cons, mask), 0L)
  expect_length(golden_check_derived(tt), 0L)
  # The first Suppressed cell of each column is the contn row, whose p-value
  # is row-masked: exactly what M16 (Test/Statistic) and M47 (p) leaked.
  for (leak in list(c("test", "Welch t test"), c("statistic", "t(2.3)= -9.14"), c("p", "0.007"))) {
    bad <- p1_leak(tt, leak[1], leak[2])
    mm <- golden_compare_cells(bad, want, mask)
    expect_identical(nrow(mm), 1L, label = paste(leak[1], "cells"))
    expect_identical(mm$want, "Suppressed")
    expect_gt(length(golden_compare_console(utils::capture.output(print(bad)), cons, mask)), 0L,
              label = paste(leak[1], "console"))
    expect_gt(length(golden_check_derived(bad)), 0L, label = paste(leak[1], "suppression matrix"))
  }
  # A masked cell that is not Suppressed on either side stays masked.
  w2 <- want
  tc <- which(want[1, ] == "Test")
  w2[which(want[, tc] == "Suppressed")[1], tc] <- "Ind. t test"
  expect_identical(nrow(golden_compare_cells(p1_leak(tt, "test", "Welch t test"), w2, mask)), 0L)
})

test_that("a leaked p-value fails the CSV, Markdown, and xlsx comparators", {
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  dir <- withr::local_tempdir()
  tt <- golden_eval_edited("T29", identity, dir)
  mask <- golden_scenario("T29")$mask
  # T29's price row (contn: p row-masked) is Suppressed.
  i <- which(tt$rows$vtype == "contn")[1]
  expect_true(golden_p_masked_rows(tt)[i])
  expect_identical(tt$body[[which(tt$cols$role == "p")]][i], "Suppressed")
  bad <- p1_leak(tt, "p", "0.007")
  for (x in list(list(tt = tt, ok = TRUE), list(tt = bad, ok = FALSE))) {
    csv <- tempfile(tmpdir = dir, fileext = ".csv")
    md <- tempfile(tmpdir = dir, fileext = ".md")
    xlsx <- tempfile(tmpdir = dir, fileext = ".xlsx")
    tt_write_csv(x$tt, csv)
    tt_write_markdown(x$tt, md)
    tt_write_xlsx(x$tt, xlsx, sheet = "T29")
    why_csv <- golden_compare_sink(csv, golden_path("T29.csv"), mask, p_rows = golden_grid_p_rows(x$tt))
    why_md <- golden_compare_sink(md, golden_path("T29.md"), mask, p_body = golden_p_masked_rows(x$tt))
    sty <- golden_compare_styles(xlsx, "T29", golden_path("table1_tc.xlsx"), "T29", mask,
                                 got_width_offset = golden_r_width_offset, p_rows = golden_p_masked_rows(x$tt))
    if (x$ok) {
      expect_length(c(why_csv, why_md, sty), 0L)
    } else {
      expect_true(any(grepl("'0.007' vs 'Suppressed'", why_csv, fixed = TRUE)), label = "CSV sink")
      # The leaked cell is masked to "#" on the R side; Stata's stays Suppressed.
      expect_true(any(grepl("| # |", why_md, fixed = TRUE) & grepl("| Suppressed |", why_md, fixed = TRUE)),
                  label = "Markdown sink")
      expect_true(any(grepl(" value: '0.007' vs 'Suppressed'", sty, fixed = TRUE)), label = "xlsx")
    }
  }
})

test_that("methods comparator swaps the R test names and rejects other edits", {
  g <- golden_read_stored("T30e")
  w <- g$value[g$name == "methods"]
  r <- golden_methods_swap(w)
  expect_false(grepl("Stata", r, fixed = TRUE))
  expect_match(r, "Welch's t-test, Wilcoxon rank-sum test, Pearson's chi-squared test.", fixed = TRUE)
  expect_length(golden_compare_stored(list(methods = r), g, fields = "methods", mask = "p"), 0L)
  expect_gt(length(golden_compare_stored(list(methods = w), g, fields = "methods", mask = "p")), 0L)
  bad <- sub("Welch's t-test", "Student's t-test", r, fixed = TRUE)
  expect_gt(length(golden_compare_stored(list(methods = bad), g, fields = "methods", mask = "p")), 0L)
})

test_that("stored comparator matches repeated row names by occurrence", {
  g <- data.frame(name = "table", kind = "matrix", row = c("a", "___Missing", "b", "___Missing"),
                  col = "p_value", value = c("0.5", ".", "0.25", "0.75"), stringsAsFactors = FALSE)
  m <- matrix(c(0.5, NA, 0.25, 0.75), ncol = 1, dimnames = list(g$row, "p_value"))
  expect_length(golden_compare_stored(list(table = m), g), 0L)
  m[4, 1] <- 0.7
  expect_gt(length(golden_compare_stored(list(table = m), g)), 0L)
  # Row-wise p mask by r(table) row.
  expect_length(golden_compare_stored(list(table = m), g, mask = "p", p_table = c(FALSE, FALSE, FALSE, TRUE)), 0L)
  expect_gt(length(golden_compare_stored(list(table = m), g, mask = "p", p_table = c(TRUE, TRUE, TRUE, FALSE))), 0L)
})

# ---------------------------------------------------------------------------
# Fixtures and classifier/formatting goldens

test_that("fixtures carry Stata labels", {
  d <- golden_fixture("cohort", factors = "education")
  expect_identical(nrow(d), 15000L)
  expect_identical(attr(d$index_age, "label"), "Age at cohort entry (years)")
  expect_identical(levels(d$education), c("Primary", "Secondary", "Tertiary"))
  expect_identical(attr(d$education, "label"), "Education level")
  a <- golden_fixture("auto", factors = "rep78")
  expect_identical(levels(a$rep78), as.character(1:5))
})

test_that("classifier goldens are complete", {
  rng <- utils::read.csv(golden_path("rng_mt64.csv"))
  expect_identical(as.integer(table(rng$seed)[c("12345", "1", "2147483647", "20260324")]),
                   c(10000L, 1000L, 1000L, 1000L))
  expect_true(all(rng$u > 0 & rng$u < 1))

  sw <- utils::read.csv(golden_path("swilk.csv"), colClasses = c(case = "character"))
  hr <- sw[sw$case == "full" & sw$variable == "headroom", ]
  # D6: Stata's tie-averaged swilk vs R's shapiro.test on auto headroom.
  expect_equal(as.numeric(hr$p), 0.3314, tolerance = 1e-3)
  expect_lt(stats::shapiro.test(golden_fixture("auto")$headroom)$p.value, 0.01)

  ss <- utils::read.csv(golden_path("subsample_ids.csv"))
  d <- golden_fixture("cohort3500")
  for (v in unique(ss$variable)) {
    ids <- ss$id[ss$variable == v]
    expect_identical(length(unique(ids)), 2000L)
    expect_true(all(ids %in% d$id[!is.na(d[[v]])]))
  }

  at <- utils::read.csv(golden_path("autotype.csv"))
  expect_true(all(at$type %in% c("contn", "conts", "cat", "bin")))
  dv <- utils::read.csv(golden_path("detect_vartype.csv"))
  expect_true(all(file.exists(golden_path("fixtures", "vartype_branches.dta"))))
  expect_setequal(unique(dv$type), c("cat", "contn", "bin", "conts"))

  fm <- utils::read.csv(golden_path("stata_fmt.csv"), colClasses = "character")
  expect_setequal(unique(fm$kind), c("string", "round", "headerperc"))
  expect_identical(unique(fm$result[fm$kind == "string" & fm$x == "100000000" & fm$fmt == "%2.0f"]), "1.0e+08")
})

test_that("every non-exact scenario has a per-scenario stored tolerance, tighter than the old 1e-3", {
  sc <- golden_scenarios()
  # effecttab scenarios have their own runner and r(table) tolerances
  # (golden_effect_table_tol, helper-golden-effecttab.R), checked below.
  tol_ids <- sc$id[sc$compare != "exact" & sc$command != "effecttab"]
  expect_setequal(names(golden_stored_tol_tolerance), tol_ids)
  for (id in tol_ids) {
    t <- golden_stored_tol_tolerance[[id]]
    expect_named(t, c("table", "other"))
    expect_true(all(t >= 1e-9 & t <= 1e-4), label = id)
  }
})

test_that("effecttab r(table) tolerances: listed scenarios exist, none looser than 1e-3", {
  sc <- golden_scenarios()
  expect_true(all(names(golden_effect_table_tol) %in% sc$id[sc$command == "effecttab"]))
  expect_true(all(golden_effect_table_tol >= 1e-9 & golden_effect_table_tol <= 1e-3))
  # Every collected E scenario's setup wrote Stata's r(table) rows (the
  # from() matrix scenarios have no collection).
  ids <- sc$id[sc$command == "effecttab"]
  expect_setequal(ids[!grepl("golden_effect_input", sc$stata_setup[sc$command == "effecttab"], fixed = TRUE)],
                  c("E18", "E24"))
  ids <- setdiff(ids, c("E18", "E24"))
  expect_true(all(file.exists(golden_path(paste0(ids, "_input.csv")))))
})

test_that("H11: the regtab methods transform replaces only the model phrase of listed goldens", {
  d <- golden_methods_divergences()
  # Since tabtools 2.1.12 builds r(methods) from the models (take_action
  # item 11, fixed) the list would hold only models Stata tabtools does
  # not classify; the last ones, clogit (R74-R76, task 5.15), are
  # classified since 2.1.14 (C8), so the list is empty.
  expect_identical(names(d), c("set", "id", "stata", "r"))
  expect_identical(nrow(d), 0L)
  expect_false(anyDuplicated(paste(d$set, d$id)) > 0)
  expect_true(all(d$set %in% c("golden", "regtab_5w_review", "regtab_phase5_review", "regtab_h_t2b")))
  sc <- golden_scenarios()
  expect_true(all(d$id[d$set == "golden"] %in% sc$id[sc$command == "regtab"]))
  read_set <- function(set, id) {
    switch(set, golden = golden_read_stored(id),
           utils::read.csv(test_path("fixtures", set, paste0(id, "_stored.csv")), colClasses = "character",
                           na.strings = character(), encoding = "UTF-8"))
  }
  parts <- function(x) {
    re <- "^(.*) with ([0-9.]+)% confidence intervals(?: from (.*?))?( across [0-9]+ models)?\\.( Statistical significance denoted as .*)?$"
    m <- regmatches(x, regexec(re, x, perl = TRUE))[[1]]
    if (!length(m)) return(NULL)
    list(estimates = m[2], level = m[3], model = m[4], across = m[5], stars = m[6])
  }
  for (i in seq_len(nrow(d))) {
    s <- parts(d$stata[i])
    r <- parts(d$r[i])
    expect_false(is.null(s), label = paste(d$id[i], "Stata sentence shape"))
    expect_false(is.null(r), label = paste(d$id[i], "R sentence shape"))
    # Level, model count and stars never change; the model phrase does.
    expect_identical(r$level, s$level, label = d$id[i])
    expect_identical(r$across, s$across, label = d$id[i])
    expect_identical(r$stars, s$stars, label = d$id[i])
    expect_false(identical(r$model, s$model) && identical(r$estimates, s$estimates), label = d$id[i])
    # The golden or fixture still holds the listed Stata sentence.
    g <- read_set(d$set[i], d$id[i])
    expect_identical(golden_methods_regtab(g, d$id[i], d$set[i])$value[g$name == "methods"], d$r[i])
  }
  # An unlisted golden passes through unchanged.
  un <- setdiff(sc$id[sc$command == "regtab"], d$id)[1]
  g <- golden_read_stored(un)
  expect_identical(golden_methods_regtab(g, un), g)
})
