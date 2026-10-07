#!/usr/bin/env Rscript
# Native workflow is two runs: reviewed --stage-native --stage-output=NEW_DIR,
# then --update --demo-data-dir=ACCEPTED_DIR --receipt-dir=NEW_DIR, each with
# explicit --commit=712044f8 --tabtools-version=2.5.1 and a Git --stata-tools.
# STAGED, native-accepted/R-pending and final R PASS are separate receipts.

demo_copy_tree <- function(from, to) {
  if (!dir.create(to)) demo_abort("Cannot create copy destination: ", to)
  files <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(files) && !all(file.copy(files, to, recursive = TRUE, copy.date = TRUE))) demo_abort("Cannot preserve complete native stage.")
}

demo_runtime_inventory <- function(out, helper) {
  rows <- new.env(parent = emptyenv())
  rows$value <- list()
  add <- function(kind, artefact, item, stata = "") rows$value[[length(rows$value) + 1L]] <- data.frame(kind = kind, artefact = artefact, item = item, stata = stata, stringsAsFactors = FALSE)
  books <- list.files(out, pattern = "\\.xlsx$")
  expected <- paste0(c("demo_table1", "demo_desctab", "demo_regtab", "demo_regtab_models", "demo_comptab", "demo_effecttab", "demo_stratetab", "demo_corrtab", "demo_crosstab", "demo_survtab", "demo_hrcomptab", "demo_puttab", "demo_stacktab", "demo_ratetab", "demo_outtab", "demo_tabcell"), ".xlsx")
  if (!identical(sort(books), sort(expected))) demo_abort("Native workbook inventory is incomplete or unexpected.")
  for (book in books) for (sheet in tidyxl::xlsx_sheet_names(file.path(out, book))) add("sheet", book, sheet)
  log <- readLines(file.path(out, "console_output.log"), encoding = "UTF-8", warn = FALSE)
  console <- helper$golden_demo_log_blocks(log, "stata")
  md <- helper$golden_demo_md_blocks(readLines(file.path(out, "console_output.md"), encoding = "UTF-8", warn = FALSE), "stata")
  keys <- vapply(console, `[[`, "", "key")
  if (!length(keys) || !identical(keys, vapply(md, `[[`, "", "key"))) demo_abort("Native console log/Markdown identities differ or are empty.")
  for (i in seq_along(keys)) add("console", "console_output.log", sprintf("C%02d", i), keys[i])
  report <- helper$golden_demo_report_tables(readLines(file.path(out, "demo_markdown_report.md"), encoding = "UTF-8", warn = FALSE))
  if (!length(report)) demo_abort("Native Markdown report has no tables.")
  for (head in names(report)) add("report", "demo_markdown_report.md", head)
  do.call(rbind, rows$value)
}

demo_check_manifest <- function(manifest, inventory) {
  required <- c("kind", "artefact", "item", "status", "reason", "golden", "scenario", "mask", "mode", "stata", "r")
  if (!all(required %in% names(manifest)) || any(!manifest$status %in% c("compared", "not_compared", "not_ported")) || any(!nzchar(manifest$reason[manifest$status != "compared"]))) demo_abort("Demo manifest classification/reasons are incomplete.")
  for (kind in c("sheet", "console", "report")) {
    got <- inventory[inventory$kind == kind, , drop = FALSE]
    want <- manifest[manifest$kind == kind, , drop = FALSE]
    if (kind == "sheet") {
      if (!identical(sort(unique(got$artefact)), sort(unique(want$artefact)))) demo_abort("Manifest/native workbook sets differ.")
      for (book in unique(got$artefact)) if (!identical(got$item[got$artefact == book], want$item[want$artefact == book])) demo_abort("Manifest/native sheet order differs: ", book)
      compared <- want[want$status == "compared", , drop = FALSE]
      if (!identical(compared$golden, paste0("demo/", compared$artefact, ":", compared$item)) || any(nzchar(compared$scenario))) demo_abort("Compared sheets require authentic demo books and empty scenario routes.")
    } else if (kind == "console") {
      if (!identical(got$stata, want$stata)) demo_abort("Manifest/native console keys differ.")
    } else if (!identical(got$item, want$item)) demo_abort("Manifest/native report headings differ.")
  }
  invisible(TRUE)
}

demo_update_selection <- function(only, source, commit, version, files) {
  if (is.null(only)) return(files)
  if (grepl("(^,|,$|,,)", only)) demo_abort("Empty --only artifact element.")
  chosen <- strsplit(only, ",", fixed = TRUE)[[1L]]
  if (!identical(unname(source[c("stata_tools_commit", "tabtools_version")]), c(commit, version))) demo_abort("--only requires the same accepted SOURCE pin; use full --update.")
  if (any(c("console_output.log", "console_output.md") %in% chosen)) demo_abort("--only cannot replace console artifacts.")
  if (!length(chosen) || any(!nzchar(chosen)) || anyDuplicated(chosen) || any(!chosen %in% files)) demo_abort("Unknown or duplicate --only artifact.")
  chosen
}

demo_read_runtime <- function(path, expected = NULL) {
  runtime <- readLines(path, warn = FALSE)
  if (length(runtime) != 2L || !nzchar(runtime[1L]) || !identical(runtime[2L], "mt64")) demo_abort("Native RNG/runtime receipt differs.")
  if (!is.null(expected) && !identical(runtime, expected)) demo_abort("Native demo/extraction runtime receipts differ.")
  runtime
}

demo_native_generate <- function(root, opt, work) {
  if (!nzchar(Sys.which("stata-mp"))) demo_abort("stata-mp not found on PATH.")
  git_repo <- demo_canonical_dir(if (is.null(opt[["stata-tools"]])) path.expand("~/Stata-Tools") else opt[["stata-tools"]])
  commit <- opt[["commit"]]
  resolved <- system2("git", c("-C", shQuote(git_repo), "rev-parse", shQuote(paste0(commit, "^{commit}"))), stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(resolved, "status")) || !identical(resolved, demo_native_commit)) demo_abort("Stata-Tools commit does not resolve to the pinned native source.")
  archive <- file.path(work, "native.tar")
  status <- system2("git", c("-C", shQuote(git_repo), "archive", shQuote(commit), "tabtools", "_data", "tc_schemes", "logdoc"), stdout = archive, stderr = file.path(work, "archive.log"))
  if (status != 0L) demo_abort("Native git archive failed.")
  export <- file.path(work, "export")
  out <- file.path(work, "out")
  data_dir <- file.path(work, "data")
  dir.create(export)
  dir.create(out)
  dir.create(data_dir)
  utils::untar(archive, exdir = export)
  demo_file <- file.path(export, "tabtools", "demo", "demo_tabtools.do")
  if (!identical(demo_sha256(demo_file), demo_native_source_hash)) demo_abort("Exported demo source hash differs from pinned catalog.")
  demo_lines <- readLines(demo_file, warn = FALSE)
  run_stata <- function(lines, name, sentinel) {
    do <- file.path(work, paste0(name, ".do"))
    writeLines(lines, do, useBytes = TRUE)
    previous <- setwd(work)
    on.exit(setwd(previous), add = TRUE)
    status <- system2("stata-mp", c("-b", "do", shQuote(do)))
    log <- file.path(work, paste0(name, ".log"))
    txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    if (status != 0L || any(grepl("^r\\([0-9]+\\);$", txt)) || !any(trimws(txt) == sentinel)) demo_abort(name, " failed or did not emit its successful sentinel.")
    txt
  }
  runtime_path <- file.path(work, "runtime.txt")
  run_stata(sprintf('do "%s" "%s" "%s" %s "%s"', file.path(root, "qa", "stata", "run_demo.do"), export, out, opt[["tabtools-version"]], runtime_path), "demo", "RESULT: demo_tabtools tests=1 pass=1 fail=0")
  runtime <- demo_read_runtime(runtime_path)
  plan <- demo_native_plan(demo_lines, export)
  extraction_runtime <- file.path(work, "extraction-runtime.txt")
  extraction_lines <- gsub("@@DATA@@", data_dir, plan$lines, fixed = TRUE)
  extraction_lines <- append(extraction_lines, c(
    sprintf('file open ttver using "%s", write text replace', extraction_runtime),
    'file write ttver "`c(stata_version)\'" _n "`c(rng_current)\'" _n', 'file close ttver'), after = length(extraction_lines) - 1L)
  run_stata(extraction_lines, "demo_data", "RESULT: demo_data tests=8 pass=8 fail=0")
  demo_read_runtime(extraction_runtime, expected = runtime)
  if (!file.copy(file.path(work, "demo_data.do"), file.path(data_dir, "extraction.do"))) demo_abort("Cannot preserve extraction recipe.")
  info <- c(stata_tools_commit = commit, stata_tools_full_commit = resolved, tabtools_version = "2.5.1", stata_version = runtime[1L], rng = runtime[2L], generated = format(Sys.time(), tz = "UTC", usetz = TRUE), demo_source_sha256 = demo_native_source_hash, demo_dir = file.path(export, "tabtools", "demo"), native_adopath = file.path(export, "tabtools"), source_status = "native_staged")
  demo_write_source(data_dir, info)
  helper <- new.env(parent = globalenv())
  sys.source(file.path(root, "tests", "testthat", "helper-golden-demo.R"), envir = helper)
  inventory <- demo_runtime_inventory(out, helper)
  heads <- sub("\\s+$", "", grep("^\\*\\*#", demo_lines, value = TRUE))
  if (length(heads) != 103L) demo_abort("Pinned native heading count differs.")
  writeLines(heads, file.path(work, "sections.txt"), useBytes = TRUE)
  utils::write.csv(inventory, file.path(work, "runtime-inventory.csv"), row.names = FALSE)
  if (!file.copy(demo_file, file.path(work, "native-demo.do"))) demo_abort("Cannot preserve pinned native demo source.")
  list(data_dir = data_dir, out = out, info = info, inventory = inventory, heads = heads)
}

demo_save_execution <- function(work, destination, status, info = character()) {
  if (demo_path_exists(destination) || !dir.create(destination)) demo_abort("Execution destination appeared or cannot be created.")
  for (dir in c("data", "out")) if (dir.exists(file.path(work, dir))) demo_copy_tree(file.path(work, dir), file.path(destination, dir))
  files <- list.files(work, pattern = "\\.(log|do|txt|csv)$", full.names = TRUE)
  if (length(files) && !all(file.copy(files, destination))) demo_abort("Cannot save complete execution diagnostics.")
  paths <- list.files(destination, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  utils::write.csv(data.frame(file = substring(paths, nchar(destination) + 2L), sha256 = demo_sha256(paths)), file.path(destination, "ARTIFACTS.csv"), row.names = FALSE)
  receipt <- c(status = status, info)
  utils::write.csv(data.frame(key = names(receipt), value = unname(receipt)), file.path(destination, "EXECUTION.csv"), row.names = FALSE)
  invisible(destination)
}

demo_native_workflow <- function(root, opt) {
  parent <- demo_canonical_dir(if (is.null(opt[["work-dir"]])) tempdir() else opt[["work-dir"]])
  dest <- demo_new_destination(if (isTRUE(opt[["stage-native"]])) opt[["stage-output"]] else opt[["receipt-dir"]], parent)
  # df -Pk reports available KiB. Refuse large copies without 1 GiB free.
  space <- system2("df", c("-Pk", shQuote(parent)), stdout = TRUE)
  available <- suppressWarnings(as.numeric(strsplit(trimws(tail(space, 1L)), "[[:space:]]+")[[1L]][4L]))
  if (!is.finite(available) || available < 1024^2) demo_abort("At least 1 GiB free space is required.")
  work <- tempfile("tabtools-demo-stata-", tmpdir = parent)
  if (!dir.create(work)) demo_abort("Cannot create owned work directory.")
  on.exit(try(unlink(work, recursive = TRUE), silent = TRUE), add = TRUE)
  tryCatch({
    generated <- demo_native_generate(root, opt, work)
    if (isTRUE(opt[["stage-native"]])) {
      demo_save_execution(work, dest, "STAGED", generated$info)
      message("STAGED: authentic output/source retained at ", dest, "; R parity not run.")
      return(invisible(list(status = "STAGED", destination = dest)))
    }
    accepted <- opt[["demo-data-dir"]]
    pinned <- demo_read_data(accepted)
    fresh <- demo_read_data(generated$data_dir)
    demo_compare_source(fresh, pinned, readRDS(file.path(accepted, "SCHEMA.rds")))
    demo_compare_rds(readRDS(file.path(root, "qa", "demo", "demo_tabtools.rds")), demo_rds_projection(pinned, "native"))
    script <- readLines(file.path(root, "qa", "demo", "demo_tabtools.R"), warn = FALSE)
    heads_r <- sub(" ----$", "", sub("^# ", "", grep("^# \\*\\*#", script, value = TRUE)))
    if (!identical(heads_r, generated$heads)) demo_abort("R/native ordered headings differ.")
    gdir <- file.path(root, "tests", "testthat", "golden", "demo")
    manifest <- utils::read.csv(file.path(gdir, "manifest.csv"), colClasses = "character", na.strings = character())
    demo_check_manifest(manifest, generated$inventory)
    only <- opt[["only"]]
    previous_source <- character()
    if (!is.null(only)) {
      previous <- utils::read.csv(file.path(gdir, "SOURCE.csv"), colClasses = "character")
      previous_source <- stats::setNames(previous$value, previous$key)
    }
    files <- demo_update_selection(only, previous_source, opt[["commit"]], opt[["tabtools-version"]], list.files(generated$out))
    demo_save_execution(work, dest, "NATIVE_ACCEPTED_R_QA_PENDING", c(generated$info, fixture_check = "identical", demo_data_dir = normalizePath(accepted), accepted_source_sha256 = demo_sha256(file.path(accepted, "SOURCE.csv")), accepted_schema_sha256 = demo_sha256(file.path(accepted, "SCHEMA.rds"))))
    if (!all(file.copy(file.path(generated$out, files), file.path(gdir, files), overwrite = TRUE))) demo_abort("Native promotion failed; R parity is pending.")
    if (is.null(only)) {
      if (!file.copy(file.path(work, "sections.txt"), file.path(gdir, "sections.txt"), overwrite = TRUE)) demo_abort("Heading promotion failed.")
      src <- c(generated$info, fixture_check = "identical", fixture_scope = "complete native demo snapshots/storage/schema/values/labels/RDS", demo_data_dir = "qa/demo/data/native-2.5.1", accepted_source_sha256 = demo_sha256(file.path(accepted, "SOURCE.csv")), accepted_schema_sha256 = demo_sha256(file.path(accepted, "SCHEMA.rds")), r_qa_status = "pending")
      utils::write.csv(data.frame(key = names(src), value = unname(src)), file.path(gdir, "SOURCE.csv"), row.names = FALSE)
    }
    invisible(list(status = "NATIVE_ACCEPTED_R_QA_PENDING", destination = dest))
  }, error = function(error) {
    failure <- tempfile(paste0(basename(dest), "-failure-"), tmpdir = dirname(dest))
    tryCatch({
      demo_save_execution(work, failure, "FAILED", c(error = conditionMessage(error)))
      message("FAILED diagnostics: ", failure)
    }, error = function(save_error) message("Could not preserve diagnostics: ", conditionMessage(save_error)))
    stop(error)
  })
}

demo_finish_r_qa <- function(df, stray, qa, receipt_dir = NULL) {
  qa$qa_record_testthat(df, stray, "test-demo-parity.R")
  executed <- qa$qa_state$executed
  skipped <- length(qa$qa_state$skipped)
  failed <- length(qa$qa_state$failed)
  status <- qa$qa_status(executed, skipped, failed)
  if (!is.null(receipt_dir)) {
    saveRDS(df, file.path(receipt_dir, "R-QA-RESULTS.rds"), version = 3)
    saveRDS(stray, file.path(receipt_dir, "R-QA-STRAY.rds"), version = 3)
    utils::write.csv(data.frame(key = c("r_qa_status", "executed", "skipped", "failed"), value = c(status, executed, skipped, failed)), file.path(receipt_dir, "R-QA.csv"), row.names = FALSE)
  }
  # Generic qa_done() quits on FAIL. Keep its same status/count contract locally
  # so failed results are durable before this CLI's final nonzero process exit.
  cat(sprintf("QA-RESULT script=demo_parity.R status=%s executed=%d skipped=%d failed=%d\n", status, executed, skipped, failed))
  list(status = status, results = df, stray = stray)
}

demo_check_r <- function(root, data_dir, receipt_dir = NULL) {
  previous <- setwd(root)
  on.exit(setwd(previous), add = TRUE)
  old_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")
  on.exit(if (is.na(old_cran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_cran), add = TRUE)
  accepted <- demo_read_data(data_dir)
  demo_compare_rds(readRDS(file.path(root, "qa", "demo", "demo_tabtools.rds")), demo_rds_projection(accepted, "native"))
  lib <- Sys.getenv("TABTOOLS_QA_LIB")
  if (nzchar(lib)) suppressPackageStartupMessages(library(tabtools, lib.loc = lib)) else pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
  source(file.path(root, "qa", "tools", "qa_result.R"), local = environment())
  rep <- qa_testthat_reporter()
  res <- testthat::test_file(file.path(root, "tests", "testthat", "test-demo-parity.R"), reporter = rep, load_package = "none", stop_on_failure = FALSE)
  df <- as.data.frame(res)
  result <- demo_finish_r_qa(df, rep$stray, environment(), receipt_dir)
  manifest <- utils::read.csv(file.path(root, "tests", "testthat", "golden", "demo", "manifest.csv"), colClasses = "character", na.strings = character())
  print(as.data.frame(table(kind = manifest$kind, status = manifest$status)))
  result
}

demo_parity_main <- function(args, root) {
  # Helpers share only this script's environment, never the package namespace.
  source(file.path(root, "qa", "tools", "demo_data.R"), local = environment(demo_parity_main))
  opt <- demo_options(args)
  if (!identical(unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1L, 1L]), "tabtools")) demo_abort("Cannot find tabtools package root.")
  if (isTRUE(opt[["stage-native"]])) return(demo_native_workflow(root, opt))
  native <- if (isTRUE(opt[["update"]])) demo_native_workflow(root, opt) else NULL
  data_dir <- if (is.null(opt[["demo-data-dir"]])) file.path(root, "qa", "demo", "data", "native-2.5.1") else opt[["demo-data-dir"]]
  demo_check_r(root, data_dir, if (is.null(native)) NULL else native$destination)
}

if (sys.nframe() == 0L) {
  file_arg <- sub("^--file=", "", commandArgs(FALSE)[startsWith(commandArgs(FALSE), "--file=")])
  root <- if (length(file_arg)) dirname(dirname(normalizePath(file_arg[1L]))) else normalizePath(getwd())
  result <- demo_parity_main(commandArgs(trailingOnly = TRUE), root)
  if (!result$status %in% c("PASS", "STAGED")) quit(save = "no", status = 1L)
}
