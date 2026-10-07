library(testthat)
library(tabtools)

# Immutable git 712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129: all 73 ado
# files include a conservative superset of the session/puttab/regtab transitive
# closure. The package manifest/help are pinned too. Never derive expected hashes
# from TABTOOLS_STATA_DIR, which is untrusted configuration.
session_native_pin <- c(
  "_desctab_collect.ado" = "d8a40a07c289eb59067b1cb88c10f8e2",
  "_regtab_activeb.ado" = "1d29cc3cbeac50b232d9b95a2b3714b8",
  "_regtab_addcol.ado" = "0b2cb152f958b0ea93650e5f57f9e7a0",
  "_regtab_addrow.ado" = "3126fc9c3b75e0ee907d4d2dd2507eef",
  "_regtab_bnotes.ado" = "7a6980f3d0ba515c9d0df35b61d57b60",
  "_regtab_cellnote.ado" = "9e8db4fe52a83d26ef5084393731d489",
  "_regtab_classes.ado" = "1a4d2af1cc9ba84429d3284990ba3630",
  "_regtab_cmdopts.ado" = "3a95f456b50e564722928692f97d5749",
  "_regtab_cmdsets.ado" = "eb77f936fce877cad5c1ce5d5fd84644",
  "_regtab_collabels.ado" = "a494a4f463487c59151c7d9312f9fee4",
  "_regtab_eqkeys.ado" = "cf0e840aaf2ea05c788348ff17e0647e",
  "_regtab_estats.ado" = "a5f9b1da943b419125354d1b75636ec9",
  "_regtab_fitrec.ado" = "294608f907ae22f88d191bafee1b48cc",
  "_regtab_flatten.ado" = "ab8d0e13215d9cb095178a13dbba6fa7",
  "_regtab_frameopts.ado" = "d4fb9fc4d5a54bde7a8f7253da3e4764",
  "_regtab_fvbase.ado" = "ac8bf7701a8e38ae92854cf9b6b3963a",
  "_regtab_fvparent.ado" = "ac10341b799b1c08ded956b5f78fa3cf",
  "_regtab_keys.ado" = "88e2c67723236b60ae795b1b53b35427",
  "_regtab_methods.ado" = "a28af5152bf9a02e784873bb3c7b8106",
  "_regtab_mincount.ado" = "500a1185c809dbaa9b62f912694fae6f",
  "_regtab_modelnoun.ado" = "7781dc09b90c350911e0262728828613",
  "_regtab_mstats.ado" = "fc4d539ec4643f45ff96199ac4c4aea6",
  "_regtab_optstr.ado" = "ef1a6c592257f4fbbfe37fa2023336e1",
  "_regtab_prefopts.ado" = "4df12920a20e01823807c8980806c641",
  "_regtab_relabel.ado" = "8ce5a4813f369fb3cc2b145f4d6cc3ef",
  "_regtab_remeta.ado" = "31034e877cd70fe439b9bf31d2982708",
  "_regtab_rlabels.ado" = "0f6349a965c0f6fe0f7de88b3622d61a",
  "_regtab_role.ado" = "1aae9184612776b4bfb60553afe8a0f5",
  "_regtab_scale.ado" = "f4ef486205ebc47c6f56c74198540a8f",
  "_regtab_statrows.ado" = "0815dd0a310636c05f03de6cf52df162",
  "_regtab_statspec.ado" = "01c54158f24569f60e063ec54eb72772",
  "_regtab_unwrap.ado" = "3455c5b1d6173c2e29f12c570c3e5f3e",
  "_stacktab_frames.ado" = "4bd04d2b92329c3bd0f942f5ba65f123",
  "_tabcell_render.ado" = "50dc2c3e368fe3c602871111ef2bc650",
  "_tabtools_check_sinks.ado" = "87094c8b81eb03c1d54101cac1894f7d",
  "_tabtools_collect_render.ado" = "d93a6541a0d514f2d7cdafc259682174",
  "_tabtools_colwidth.ado" = "59a385221788e6415f5f5ab8fd51c97b",
  "_tabtools_common.ado" = "b3a1786e5b1357b63bf862ccbd21157e",
  "_tabtools_csv_write.ado" = "6a469f3bb2ab037c20974bdaab385496",
  "_tabtools_fitcount.ado" = "b0c388759ab3a525850808bf412c64ca",
  "_tabtools_flatframe.ado" = "405711be1ad13caaf858d2b9cf8d9135",
  "_tabtools_fmt_p.ado" = "f0a2e0396ed2d4a44e4d237a2ac27a5f",
  "_tabtools_markdown_write.ado" = "65a0b5c52b1320f00ce96ca2c9e683dc",
  "_tabtools_match_rows.ado" = "7e88689938a452e5e274ee630943a8de",
  "_tabtools_set.ado" = "5efc21a70d1b1b8485efc92e32a2d79a",
  "_tabtools_set_sinks.ado" = "a598f471a59a6504a803fca54ab40b34",
  "_tabtools_smallcells.ado" = "37426fa8d859ca5ebcd9ad8f39f4423c",
  "_tabtools_smallcells_opt.ado" = "03943761f5bd265f99ebad68caa20b0a",
  "_tabtools_smallcells_render.ado" = "72f1f4f8ec2365d677c0c9507b4959e9",
  "_tabtools_visible_vars.ado" = "97cc069dec4b99d92c60119c945ea55a",
  "_tabtools_xlsx_apply_styles.ado" = "a3dc04a70fdcdab73ca08e3f7f59fd06",
  "_tabtools_xlsx_build_styles.ado" = "e51052169fed887fe07bebec89c29641",
  "_tabtools_xlsx_compact_styles.ado" = "b9ac8abe05baca1b31fcfa0b97625982",
  "_tabtools_xlsx_deferred_styles.ado" = "70be894dc0641f2f56e4aef007d22b40",
  "_tabtools_xlsx_read.ado" = "ffa8cfcdebf16d23d6e140d84561ee61",
  "_tabtools_xlsx_write.ado" = "598c5cad1ad8f42d508877de7fcdabbf",
  "comptab.ado" = "f2b5d5145197f301f3b985bf39532f2b",
  "corrtab.ado" = "341921ffea86626a5eb92583a90cdea4",
  "crosstab.ado" = "863db5ee91df500962d15093b8c821e6",
  "desctab.ado" = "e3837e120a634f504a8af55538893837",
  "effecttab.ado" = "21ac264e06c237a7f99388843a40d3ae",
  "hrcomptab.ado" = "557eac65f1b5ae090829209e6c866b4a",
  "outtab.ado" = "3336d5fb06052432e3330cebe320ebfd",
  "puttab.ado" = "e6c6c36baa4d9a1a3cf36bb858cdbb96",
  "ratetab.ado" = "309deb89cd39e7daa461bfd588890fa7",
  "regtab.ado" = "544436a8379b28b38cb9df1571809a3c",
  "stacktab.ado" = "646ea06d2b52ef8a639a01f13ed09b5c",
  "stratetab.ado" = "9d92707a0f5defd9b81177a8f9129ad4",
  "survtab.ado" = "b9f09d353b4ff21f84a8871030450e4f",
  "tabcell.ado" = "bac7fd6d34753b6ff27dae69c5875b92",
  "table1_tc.ado" = "0c723c768744a8e95ec14c6bbf05091e",
  "tabtools.ado" = "f121822c5d9219d78f0de3f9ae87bee0",
  "tabtools_tips.ado" = "99700c43a54e5545ae1fa67d7e2213c8",
  "tabtools.sthlp" = "38f7c4448ea86a12fa05d3cdc60b7bb0",
  "tabtools.pkg" = "d3db0eab53254e14e913280525761c9b")

session_native_authenticate <- function(source_dir) {
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = FALSE)
  if (grepl('["\r\n`]', source_dir)) stop("TABTOOLS_STATA_DIR contains a quoting character")
  paths <- file.path(source_dir, names(session_native_pin))
  missing <- names(session_native_pin)[!file.exists(paths)]
  if (length(missing)) stop("Pinned session source files are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  actual <- unname(tools::md5sum(paths))
  if (!identical(actual, unname(session_native_pin))) {
    bad <- names(session_native_pin)[is.na(actual) | actual != unname(session_native_pin)]
    stop("Native source is not Stata-Tools 712044f8 (tabtools 2.5.1): ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  header <- readLines(file.path(source_dir, "tabtools.ado"), n = 1L, warn = FALSE)
  version <- sub("^\\*! tabtools Version ([0-9.]+) .*", "\\1", header)
  if (!identical(version, "2.5.1")) stop("Authenticated native version header is invalid", call. = FALSE)
  list(directory = source_dir, version = version,
       revision = "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129",
       files = names(session_native_pin)[endsWith(names(session_native_pin), ".ado")])
}

session_native_load <- function(pin, extra_adopath = NULL) {
  # clear all precedes these lines. Check resolution before loading any program;
  # then run the exact authenticated absolute files, so already-loaded programs
  # and helpers from another adopath cannot stand in for authenticated source.
  checks <- c(sprintf('adopath ++ "%s"', pin$directory))
  if (!is.null(extra_adopath)) checks <- c(checks, sprintf('adopath ++ "%s"', extra_adopath))
  for (file in pin$files) {
    checks <- c(checks, sprintf('quietly findfile %s', file),
                sprintf('assert r(fn) == "%s/%s"', pin$directory, file))
  }
  c(checks, sprintf('quietly run "%s/%s"', pin$directory, pin$files))
}

# Pinned _tabtools_set_sinks.ado:51-113,197-213, tabtools.sthlp:302-316.
# Compare actual document lifecycle, independent title expectations and ordinary
# command gating. Whole-workbook first replacement is an explicit R difference.
session_native_run <- function(directory, source_dir, extra_adopath = NULL) {
  pin <- session_native_authenticate(source_dir)
  script <- c("version 17.0", "clear all", "set more off", "set linesize 255",
    session_native_load(pin, extra_adopath),
    "set obs 10", "generate x = _n", "generate y = _n * 2 + mod(_n, 3)",
    'generate str8 value = "CELL"', 'tabtools set markdown "A.md"',
    'puttab value, noheader title("FIRST")', 'copy "A.md" "first.md", replace',
    'tabtools set markdown "./A.md"', 'puttab value, noheader title("SECOND")',
    'tabtools set markdown "B.md"', 'puttab value, noheader title("B")',
    'tabtools set markdown "A.md"', 'puttab value, noheader title("THIRD")',
    "tabtools set clear", 'tabtools set markdown "A.md"',
    'puttab value, noheader title("FOURTH")', 'copy "A.md" "history.md", replace',
    'puttab value, noheader markdown("A.md") title("REPLACE")',
    'copy "A.md" "explicit.md", replace',
    'puttab value, noheader title("APPEND")', 'copy "A.md" "append.md", replace',
    'tabtools set workbook "unused.xlsx"', "collect clear", "collect: regress y x",
    "regtab", 'capture confirm file "unused.xlsx"', "assert _rc == 601",
    'copy "A.md" "omitted.md", replace',
    'regtab, sheet("S")', 'copy "A.md" "selected.md", replace',
    'capture confirm file "unused.xlsx"', "assert _rc == 0",
    'display "SESSION_NATIVE_COMPLETE"')
  writeLines(script, file.path(directory, "session.do"))
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "session.do"),
                        wd = directory, error_on_status = FALSE, timeout = 120L)
  log <- readLines(file.path(directory, "session.log"), warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  if (proc$status != 0L || length(errors) || !any(trimws(log) == "SESSION_NATIVE_COMPLETE") ||
      !any(grepl("end of do-file", log, fixed = TRUE))) {
    stop("Native session oracle incomplete: ", paste(errors, collapse = ", "))
  }
  # Recheck hashes after execution before attaching provenance to completion.
  completed_pin <- session_native_authenticate(source_dir)
  if (!identical(completed_pin, pin)) stop("Native source changed during execution", call. = FALSE)
  invisible(list(log = log, pin = completed_pin))
}

test_that("session lifecycle and explicit-sheet gating match pinned native output", {
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to pinned Stata 2.5.1")
  session_native_authenticate(source_dir) # A supplied wrong tree fails before any prerequisite skip.
  skip_if_not_installed("processx")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  directory <- withr::local_tempdir(pattern = "wp-3a-native-")
  native <- file.path(directory, "native")
  dir.create(native)
  completed <- session_native_run(native, source_dir)
  keys <- getFromNamespace(".tt_option_keys", "tabtools")
  option_names <- paste0("tabtools.", keys)
  withr::local_options(stats::setNames(lapply(option_names, getOption), option_names))
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  old_state <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(old_state, envir = state)
  })
  tabtools_options(clear = TRUE)
  state$written <- character()
  a <- file.path(directory, "A.md")
  b <- file.path(directory, "B.md")
  publish <- function(title, ...) suppressMessages(puttab(data.frame(value = rep("CELL", 10)),
                                                        noheader = TRUE, title = title, ...))
  snapshots <- list()
  tabtools_options(markdown = a)
  publish("FIRST")
  snapshots$first <- readLines(a)
  tabtools_options(markdown = file.path(directory, ".", "A.md"))
  publish("SECOND")
  tabtools_options(markdown = b)
  publish("B")
  tabtools_options(markdown = a)
  publish("THIRD")
  tabtools_options(clear = TRUE)
  tabtools_options(markdown = a)
  publish("FOURTH")
  snapshots$history <- readLines(a)
  publish("REPLACE", markdown = a)
  snapshots$explicit <- readLines(a)
  publish("APPEND")
  snapshots$append <- readLines(a)
  tabtools_options(workbook = file.path(directory, "unused.xlsx"))
  fit <- stats::lm(y ~ x, data.frame(x = 1:10, y = 2 * (1:10) + (1:10) %% 3))
  regtab(fit)
  expect_false(file.exists(file.path(directory, "unused.xlsx")))
  expect_identical(readLines(a), snapshots$append)
  snapshots$omitted <- readLines(a)
  suppressMessages(regtab(fit, sheet = "S"))
  expect_true(file.exists(file.path(directory, "unused.xlsx")))
  expect_gt(length(readLines(a)), length(snapshots$omitted))
  expect_gt(length(readLines(file.path(native, "selected.md"))), length(snapshots$omitted))
  expected <- list(first = "FIRST", history = c("FIRST", "SECOND", "THIRD", "FOURTH"),
                   explicit = "REPLACE", append = c("REPLACE", "APPEND"), omitted = c("REPLACE", "APPEND"))
  titles <- function(lines) sub("^### ", "", lines[startsWith(lines, "### ")])
  matched <- TRUE
  for (id in names(expected)) {
    native_lines <- readLines(file.path(native, paste0(id, ".md")))
    expect_identical(titles(native_lines), expected[[id]], info = paste(id, "native"))
    expect_identical(titles(snapshots[[id]]), expected[[id]], info = paste(id, "R"))
    expect_identical(snapshots[[id]], native_lines, info = paste(id, "full Markdown bytes"))
    matched <- matched && identical(titles(native_lines), expected[[id]]) &&
      identical(titles(snapshots[[id]]), expected[[id]]) && identical(snapshots[[id]], native_lines)
  }
  if (!matched) stop("Native session comparison failed; no completion receipt", call. = FALSE)
  cat(sprintf("SESSION NATIVE RECEIPT snapshots=5 native=%s revision=%s authenticated=true lifecycle_and_gating=true\n",
              completed$pin$version, completed$pin$revision))
})

test_that("native session provenance rejects wrong revisions, damaged helpers and adopath fallback", {
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to pinned Stata 2.5.1")
  pin <- session_native_authenticate(source_dir)
  expect_identical(pin$version, "2.5.1")
  expect_identical(pin$revision, "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129")
  directory <- withr::local_tempdir(pattern = "wp-3a-pin-controls-")
  copy <- file.path(directory, "copy")
  dir.create(copy)
  expect_true(all(file.copy(file.path(pin$directory, names(session_native_pin)), copy)))
  expect_identical(session_native_authenticate(copy)$files, pin$files)
  entry <- file.path(copy, "tabtools.ado")
  lines <- readLines(entry, warn = FALSE)
  writeLines(sub("Version 2.5.1", "Version 2.5.0", lines, fixed = TRUE), entry)
  expect_error(session_native_authenticate(copy), "not Stata-Tools 712044f8.*tabtools.ado")
  expect_true(file.copy(file.path(pin$directory, "tabtools.ado"), entry, overwrite = TRUE))
  helper <- file.path(copy, "_tabtools_set_sinks.ado")
  unlink(helper)
  expect_error(session_native_authenticate(copy), "files are missing.*_tabtools_set_sinks.ado")
  expect_true(file.copy(file.path(pin$directory, "_tabtools_set_sinks.ado"), helper))
  cat("\n* MODIFIED HELPER CONTROL\n", file = helper, append = TRUE)
  expect_error(session_native_authenticate(copy), "not Stata-Tools 712044f8.*_tabtools_set_sinks.ado")
  # A complete correct source does not authorize a different path on the adopath.
  # This executes only the loader refusal; no publication oracle can be reached.
  skip_if_not_installed("processx")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  fallback <- file.path(directory, "fallback")
  run <- file.path(directory, "run")
  dir.create(fallback)
  dir.create(run)
  expect_true(file.copy(file.path(pin$directory, "_tabtools_set_sinks.ado"), fallback))
  expect_error(session_native_run(run, source_dir, extra_adopath = fallback), "oracle incomplete")
  expect_false(file.exists(file.path(run, "A.md")))
  expect_false(file.exists(file.path(run, "unused.xlsx")))
})

test_that("installed session publication keeps unrelated sheets and documents the difference", {
  directory <- withr::local_tempdir(pattern = "wp-3a-installed-")
  option_names <- paste0("tabtools.", getFromNamespace(".tt_option_keys", "tabtools"))
  withr::local_options(stats::setNames(lapply(option_names, getOption), option_names))
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  before <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(before, envir = state)
  })
  tabtools_options(clear = TRUE)
  book <- file.path(directory, "book.xlsx")
  tab <- puttab(data.frame(value = "MINE"))
  tt_write_xlsx(tab, book, sheet = "Mine")
  tabtools_options(workbook = book)
  suppressMessages(puttab(data.frame(value = "NEW"), sheet = "Publication"))
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book))), c("Mine", "Publication"))
  expect_true(any(openxlsx2::wb_to_df(openxlsx2::wb_load(book), sheet = "Mine", col_names = FALSE) == "MINE"))
  db <- tools::Rd_db("tabtools")
  text <- paste(capture.output(tools::Rd2txt(db[["tabtools_options.Rd"]])), collapse = " ")
  text <- gsub("[[:space:]]+", " ", text)
  for (literal in c("deliberate difference", "Session destinations", "warn = 2", "A/B/A")) {
    expect_match(text, literal, fixed = TRUE)
  }
})
