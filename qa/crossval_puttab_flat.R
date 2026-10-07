library(testthat)
library(tabtools)

# Same-author native oracle, pinned Stata 2.5.1. This tests the publication
# consumer, not fitted-estimate parity. Genuine regtab frame(flat keys)
# products keep their native names/labels/per-printed-column states. Both
# editing products receive the same stated literal edits, with immutable
# originals saved before editing. Nothing renames native fields or masks body
# cells. An explicit-vars R publication peer permits complete native byte/style
# comparison; R default additionally owes its independently asserted paragraph.

p04_native_cases <- function() list(
  default = list(frame = "base", native = "", r = NULL, style = "default"),
  thin = list(frame = "base", native = "", r = NULL, style = "thin"),
  medium = list(frame = "base", native = "", r = NULL, style = "medium"),
  academic = list(frame = "base", native = "", r = NULL, style = "academic"),
  ci = list(frame = "base", native = "c2", r = 3L, style = "default"),
  reverse = list(frame = "base", native = "c3 c2 c1 rowlabel", r = c(4L, 3L, 2L, 1L), style = "default"),
  explicit = list(frame = "base", native = "_term c2 _order c3", r = c("_term", "95% CI", "_order", "p-value"), style = "default", noheader = TRUE),
  panel = list(frame = "base", native = "", r = NULL, style = "default", panel = TRUE),
  selected = list(frame = "projected", native = "", r = 3L, style = "default"),
  noeffect = list(frame = "base", native = "rowlabel", r = 1L, style = "default", noeffect = TRUE),
  calls = list(frame = "joined", native = "", r = NULL, style = "default"))

p04_native_run <- function(directory, source_dir, cases) {
  if (grepl('["\r\n`]', source_dir)) stop("TABTOOLS_STATA_DIR contains a Stata quoting character", call. = FALSE)
  body <- c("version 17.0", "clear all", "set more off", "set linesize 255",
    sprintf('adopath ++ "%s"', source_dir),
    'foreach command in regtab puttab _tabtools_flatframe _regtab_keys {',
    '  quietly findfile `command\'.ado',
    sprintf('  assert r(fn) == "%s/`command\'.ado"', source_dir), "}",
    'use "input.dta", clear', "collect clear", "collect: regress mpg wt",
    'regtab, models("Same") coef("Coef.") stats(n) frame(base, replace flat keys)',
    'regtab, models("Same") coef("Coef.") stats(n) frame(second, replace flat keys)',
    'frame base: save "native-original.dta", replace',
    'frame second: save "native-second-original.dta", replace',
    'frame base: local first_id : char c1[tabtools_block_id]',
    'frame second: local second_id : char c1[tabtools_block_id]',
    'assert "`first_id\'" != "`second_id\'"',
    'file open identity using "native-identities.txt", write text replace',
    'file write identity "`first_id\'" _n "`second_id\'" _n', "file close identity",
    "foreach frame in base second {",
    '  frame `frame\': recast strL rowlabel c1 c2 c3',
    '  frame `frame\': replace rowlabel = "  Edited | slash\\path  " if _term == "wt"',
    '  frame `frame\': replace c1 = "Literal"',
    '  frame `frame\': replace c2 = "Reference"',
    '  frame `frame\': replace c3 = "See note"', "}",
    'frame base: save "native-edited.dta", replace',
    'frame second: save "native-second-edited.dta", replace',
    "frame copy base projected", "frame projected: keep c2 _state1",
    "frame copy base joined",
    "forvalues j = 1/3 {",
    '  frame second: local h : variable label c`j\'',
    '  frame second: local full : char c`j\'[tabtools_header]',
    '  frame second: local name : char c`j\'[tabtools_block]',
    '  frame second: local id : char c`j\'[tabtools_block_id]',
    '  local k = `j\' + 3',
    '  frame joined: generate strL c`k\' = c`j\'',
    '  frame joined: label variable c`k\' "`h\'"',
    '  frame joined: char c`k\'[tabtools_header] "`full\'"',
    '  frame joined: char c`k\'[tabtools_block] "`name\'"',
    '  frame joined: char c`k\'[tabtools_block_id] "`id\'"', "}",
    "frame joined: order rowlabel c1 c2 c3 c4 c5 c6",
    'frame joined: save "native-joined.dta", replace',
    'file open counts using "counts.csv", write text replace',
    'file write counts "id,n_rows,n_cols,n_datarows,n_panels,n_spans" _n')
  for (id in names(cases)) {
    case <- cases[[id]]
    layout <- if (isTRUE(case$noheader)) "noheader" else "blockheader"
    if (isTRUE(case$panel)) layout <- paste(layout, "panel(_rowtype) noindent")
    cmd <- sprintf('frame %s: puttab %s using "%s.xlsx", frame(%s) sheet("S") title("Title") footnote("User one \\ User two") csv("%s.csv") markdown("%s.md") borderstyle(%s) headershade headercolor(blue) zebra zebracolor(pink) %s',
                   case$frame, case$native, id, case$frame, id, id, case$style, layout)
    body <- c(body, sprintf('display "P04_START_%s"', id), cmd,
      sprintf('file write counts "%s," %%12.0f (r(n_rows)) "," %%12.0f (r(n_cols)) "," %%12.0f (r(n_datarows)) "," %%12.0f (r(n_panels)) "," %%12.0f (r(n_spans)) _n', id),
      sprintf('display "P04_END_%s"', id))
  }
  body <- c(body, "file close counts",
    'capture frame base: puttab using "refusal.xlsx", frame(base) blockheader noheader csv("refusal.csv") markdown("refusal.md")',
    "assert _rc == 198",
    'foreach file in refusal.xlsx refusal.csv refusal.md {',
    '  capture confirm file "`file\'"', "  assert _rc == 601", "}",
    'capture frame base: puttab using "refusal.xlsx", frame(base) blockheader spanheader("Manual" 2/3)',
    "assert _rc == 198",
    'foreach file in refusal.xlsx refusal.csv refusal.md {',
    '  capture confirm file "`file\'"', "  assert _rc == 601", "}",
    'display "P04_NATIVE_COMPLETE"')
  writeLines(body, file.path(directory, "p04.do"))
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "p04.do"), wd = directory,
                       error_on_status = FALSE, timeout = 120L)
  logs <- list.files(directory, pattern = "\\.log$", full.names = TRUE)
  if (length(logs) != 1L) stop("Native P04 oracle requires exactly one log", call. = FALSE)
  log <- readLines(logs, warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  if (proc$status != 0L || length(errors) || !any(trimws(log) == "P04_NATIVE_COMPLETE") ||
      !any(grepl("end of do-file", log, fixed = TRUE))) stop("Native P04 oracle incomplete: ", paste(errors, collapse = ", "), call. = FALSE)
  list(counts = utils::read.csv(file.path(directory, "counts.csv"), strip.white = TRUE), log = log)
}

test_that("P04 native flat-frame consumers agree in full publication bytes, styles and geometry", {
  skip_if_not_installed("processx")
  skip_if_not_installed("haven")
  skip_if_not_installed("tidyxl")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to pinned Stata 2.5.1")
  for (command in c("regtab", "puttab", "_tabtools_flatframe", "_regtab_keys")) {
    expect_match(readLines(file.path(source_dir, paste0(command, ".ado")), n = 1L), "Version 2.5.1", fixed = TRUE)
  }
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  directory <- withr::local_tempdir(pattern = "tabtools-p04-native-")
  cases <- p04_native_cases()
  source_files <- paste0(c("regtab", "puttab", "_tabtools_flatframe", "_regtab_keys"), ".ado")
  source_hashes <- unname(tools::md5sum(file.path(source_dir, source_files)))
  provenance <- expand.grid(case = names(cases), file = source_files, stringsAsFactors = FALSE)
  provenance$md5 <- source_hashes[match(provenance$file, source_files)]
  utils::write.csv(provenance, file.path(directory, "native-source-provenance.csv"), row.names = FALSE)
  for (j in seq_along(source_files)) cat(sprintf("P04 SOURCE file=%s md5=%s\n", source_files[j], source_hashes[j]))
  haven::write_dta(mtcars[c("mpg", "wt")], file.path(directory, "input.dta"))
  native <- p04_native_run(directory, source_dir, cases)
  expect_identical(native$counts$id, names(cases))
  original <- haven::read_dta(file.path(directory, "native-original.dta"))
  edited <- haven::read_dta(file.path(directory, "native-edited.dta"))
  second_original <- haven::read_dta(file.path(directory, "native-second-original.dta"))
  second_edited <- haven::read_dta(file.path(directory, "native-second-edited.dta"))
  expect_identical(names(original), c("rowlabel", "c1", "c2", "c3", "_order", "_term", "_rowtype", "_state1", "_state2", "_state3"))
  expect_identical(as.character(original$`_term`), c("wt", "_cons", "stat:n"))
  expect_identical(as.character(original$`_rowtype`), c("var", "var", "stat"))
  expect_false(all(as.character(original$c1) == "Literal"))
  for (state in c("_state1", "_state2", "_state3")) expect_identical(as.character(original[[state]]), c("est", "est", "stat"))
  for (key in names(original)[startsWith(names(original), "_")]) {
    expect_identical(edited[[key]], original[[key]])
    expect_identical(second_edited[[key]], second_original[[key]])
  }
  for (column in c("rowlabel", "c1", "c2", "c3")) expect_identical(edited[[column]], second_edited[[column]])
  identities <- readLines(file.path(directory, "native-identities.txt"))
  expect_length(identities, 2L)
  expect_true(all(nzchar(identities)))
  expect_false(identical(identities[1], identities[2]))
  a <- regtab(stats::lm(mpg ~ wt, mtcars), models = "Same", coef = "Coef.", stats = "n")
  flat_original <- tt_flat(a)
  flat <- flat_original
  expect_identical(flat$`_term`, as.character(original$`_term`))
  flat$rowlabel[flat$`_term` == "wt"] <- "  Edited | slash\\path  "
  flat[[2]] <- rep("Literal", nrow(flat)); flat[[3]] <- rep("Reference", nrow(flat)); flat[[4]] <- rep("See note", nrow(flat))
  expect_identical(flat$rowlabel, as.character(edited$rowlabel))
  for (j in 1:3) expect_identical(flat[[j + 1L]], as.character(edited[[paste0("c", j)]]))
  expect_identical(flat$`_state_1`, c("est", "est", ""))
  expect_identical(attr(flat, "model_states"), attr(flat_original, "model_states"))
  expect_identical(attr(flat, "source_blocks"), attr(flat_original, "source_blocks"))
  joined <- tt_flat(tt_merge(a, a))
  joined$rowlabel[joined$`_term` == "wt"] <- "  Edited | slash\\path  "
  for (j in 1:6) joined[[j + 1L]] <- rep(c("Literal", "Reference", "See note")[(j - 1L) %% 3L + 1L], nrow(joined))
  r_original_bytes <- serialize(flat_original, NULL)
  native_original_bytes <- golden_bytes(file.path(directory, "native-original.dta"))
  for (id in names(cases)) {
    case <- cases[[id]]
    f <- if (id == "calls") joined else if (id == "selected") flat[, c("95% CI", "_state_1"), drop = FALSE] else flat
    visible <- names(attr(f, "column_model"))
    selected <- if (is.null(case$r)) visible else if (is.numeric(case$r)) names(attr(flat, "column_model"))[case$r] else case$r
    args <- list(x = f, vars = selected, blockheader = !isTRUE(case$noheader) && !isTRUE(case$noeffect),
                 noheader = isTRUE(case$noheader), varlabels = TRUE, title = "Title", footnote = c("User one", "User two"),
                 borderstyle = case$style, headershade = TRUE, headercolor = "blue", zebra = TRUE, zebracolor = "pink", sheet = "S")
    if (isTRUE(case$panel)) args <- c(args, list(panel = "_rowtype", noindent = TRUE))
    peer <- suppressMessages(do.call(puttab, c(args, list(xlsx = file.path(directory, paste0(id, "-r.xlsx")),
      csv = file.path(directory, paste0(id, "-r.csv")), markdown = file.path(directory, paste0(id, "-r.md"))))))
    for (ext in c("csv", "md")) expect_identical(golden_bytes(file.path(directory, paste0(id, "-r.", ext))),
      golden_bytes(file.path(directory, paste0(id, ".", ext))), info = paste(id, ext))
    why <- golden_compare_styles(file.path(directory, paste0(id, "-r.xlsx")), "S",
      file.path(directory, paste0(id, ".xlsx")), "S", got_width_offset = golden_r_width_offset)
    expect_identical(why, character(), info = paste(id, paste(why, collapse = "\n")))
    row <- native$counts[native$counts$id == id, ]
    expect_identical(nrow(row), 1L)
    for (field in c("n_rows", "n_cols", "n_datarows", "n_panels", "n_spans")) expect_identical(as.integer(row[[field]]), peer$stored[[field]], info = paste(id, field))
    # Assert native console messages using authentic native keys, not R names.
    start <- which(trimws(native$log) == paste0("P04_START_", id))
    end <- which(trimws(native$log) == paste0("P04_END_", id))
    expect_length(start, 1L); expect_length(end, 1L)
    diagnostics <- trimws(native$log[seq.int(start + 1L, end - 1L)])
    diagnostics <- diagnostics[startsWith(diagnostics, "(puttab:")]
    keys <- if (id == "selected") "_state1" else if (id %in% c("default", "thin", "medium", "academic", "panel", "calls"))
      c("_order", "_term", if (id != "panel") "_rowtype", "_state1", "_state2", "_state3") else character()
    expected_native <- if (length(keys)) paste0("(puttab: key column(s) ", paste(keys, collapse = " "), " not exported; name them in the varlist to export them)") else character()
    if (id == "noeffect") expected_native <- c(expected_native, "(puttab: no exported column has char c#[tabtools_block]; blockheader adds no row)")
    expect_identical(diagnostics, expected_native, info = id)
    # Default R output has the same unmasked body/header and precisely the
    # stronger R diagnostic paragraph; its files are checked independently.
    defaults <- args
    if (is.null(case$r)) defaults$vars <- NULL
    if (id == "selected") defaults$vars <- NULL
    if (id == "noeffect") defaults$blockheader <- TRUE
    out <- suppressMessages(do.call(puttab, c(defaults, list(csv = file.path(directory, paste0(id, "-default.csv")),
      markdown = file.path(directory, paste0(id, "-default.md")), xlsx = file.path(directory, paste0(id, "-default.xlsx"))))))
    expect_identical(out$body, peer$body, info = id)
    expect_identical(out$header, peer$header, info = id)
    for (field in c("n_cols", "n_datarows", "n_panels", "n_spans")) expect_identical(out$stored[[field]], peer$stored[[field]], info = paste(id, field))
    rk <- if (id == "selected") "_state_1" else if (is.null(case$r)) names(f)[startsWith(names(f), "_")] else character()
    if (isTRUE(case$panel)) rk <- setdiff(rk, "_rowtype")
    extra <- if (length(rk)) paste0("(puttab: key column(s) ", paste(rk, collapse = " "), " not exported; name them in vars to export them)") else character()
    if (id == "noeffect") extra <- c(extra, "(puttab: no selected statistic columns have model labels; blockheader has no effect)")
    paragraphs <- c("User one", "User two", extra)
    expect_identical(out$footnote, paste(paragraphs, collapse = " \\ "), info = id)
    expect_identical(out$stored$n_rows, peer$stored$n_rows + length(extra), info = id)
    nc <- out$stored$n_cols
    csv <- readLines(file.path(directory, paste0(id, "-default.csv")))
    expect_identical(tail(csv, length(paragraphs)), paste0(paragraphs, strrep(",", nc - 1L)), info = id)
    expect_identical(head(csv, -length(paragraphs)), head(readLines(file.path(directory, paste0(id, "-r.csv"))), -2L), info = paste(id, "unmasked CSV header/body"))
    md <- readLines(file.path(directory, paste0(id, "-default.md")))
    escaped <- gsub("_", "\\_", paragraphs, fixed = TRUE)
    expect_identical(tail(md, 2L * length(paragraphs)), as.vector(rbind("", paste0("*", escaped, "*"))), info = id)
    expect_identical(head(md, -2L * length(paragraphs)), head(readLines(file.path(directory, paste0(id, "-r.md"))), -4L), info = paste(id, "unmasked Markdown header/body"))
    default_cells <- golden_cell_styles(file.path(directory, paste0(id, "-default.xlsx")), "S")
    note_rows <- seq.int(out$stored$n_rows + 1L - length(paragraphs), length.out = length(paragraphs))
    note_index <- match(paste0("B", note_rows), default_cells$address)
    expect_false(anyNA(note_index), info = id)
    expect_identical(default_cells$value[note_index], paragraphs, info = id)
    expect_identical(default_cells$italic[note_index], rep(TRUE, length(paragraphs)), info = id)
    peer_cells <- golden_cell_styles(file.path(directory, paste0(id, "-r.xlsx")), "S")
    default_body <- default_cells[default_cells$row < note_rows[1], , drop = FALSE]
    peer_body <- peer_cells[peer_cells$row < note_rows[1], , drop = FALSE]
    rownames(default_body) <- rownames(peer_body) <- NULL
    expect_identical(default_body, peer_body, info = paste(id, "unmasked workbook title/header/body cells/styles"))
  }
  expect_identical(serialize(flat_original, NULL), r_original_bytes)
  expect_identical(golden_bytes(file.path(directory, "native-original.dta")), native_original_bytes)
  cat(sprintf("P04 NATIVE RECEIPT scenarios=%d full_text_pairs=%d full_workbook_comparisons=%d default_publications=%d native_key_schema=per-printed-column R_key_schema=per-original-model no_body_masks=true\n",
              length(cases), 2L * length(cases), length(cases), length(cases)))
})

test_that("P04 installed help renders the actual key/model publication contract", {
  db <- tools::Rd_db("tabtools")
  expect_true("puttab.Rd" %in% names(db))
  text <- paste(capture.output(tools::Rd2txt(db[["puttab.Rd"]], options = list(underline_titles = FALSE))), collapse = " ")
  text <- gsub("[[:space:]]+", " ", text)
  for (literal in c("blockheader", "technical keys", "tabtools_error_flat", "original column-model indices", "every R sink")) expect_match(text, literal, fixed = TRUE)
  expect_false(grepl("\\\\(item|code|section|verb)\\{", text))
})
