# Complete native demo source contracts; synthetic negative fixtures are never
# package goldens or claimed Stata demo data. No Stata/network is invoked here.
library(testthat)
library(tabtools)
source(file.path("tools", "demo_data.R"), local = TRUE)
source("demo_parity.R", local = TRUE)
source("make_demo_data.R", local = TRUE)

demo_test_data <- function() {
  column <- haven::labelled(c(1, NA, 2), c(Control = 1, Exposed = 2), label = "Arm")
  d <- data.frame(arm = column, value = c(2.5, 0, -1))
  data <- stats::setNames(lapply(demo_native_names, function(nm) d), demo_native_names)
  attr(data, "native_storage") <- stats::setNames(lapply(demo_native_names, function(nm) data.frame(variable = c("arm", "value"), storage = c("byte", "double"))), demo_native_names)
  data
}

test_that("the source guard rejects drift on independent shape/type/value/label axes", {
  truth <- demo_test_data()
  schema <- demo_schema(truth)
  expect_true(demo_compare_source(truth, truth, schema))
  changes <- list(
    missing_column = function(d) { d$cohort$value <- NULL; d },
    added_column = function(d) { d$cohort$extra <- 1; d },
    variable_order = function(d) { d$cohort <- d$cohort[c("value", "arm")]; d },
    observation_order = function(d) { d$cohort <- d$cohort[c(3, 2, 1), ]; d },
    observation_count = function(d) { d$cohort <- d$cohort[-1, ]; d },
    numeric_value = function(d) { d$cohort$value[1] <- 2.6; d },
    missingness = function(d) { d$cohort$arm[2] <- 1; d },
    r_type = function(d) { d$cohort$value <- as.integer(d$cohort$value); d },
    variable_label = function(d) { attr(d$cohort$arm, "label") <- "Wrong label"; d },
    value_label = function(d) { attr(d$cohort$arm, "labels") <- c(Control = 2, Exposed = 1); d },
    native_width = function(d) { s <- attr(d, "native_storage"); s$cohort$storage[1] <- "float"; attr(d, "native_storage") <- s; d },
    dataset_order = function(d) d[rev(names(d))])
  for (axis in names(changes)) expect_error(demo_compare_source(changes[[axis]](truth), truth, schema), class = "tabtools_error_demo_source", info = axis)
  # The immutable accepted schema is also checked independently of fresh input.
  wrong_schema <- schema
  wrong_schema$cohort$storage[1] <- "float"
  expect_error(demo_compare_source(truth, truth, wrong_schema), class = "tabtools_error_demo_source")
})

test_that("RDS projection preserves complete members, labels and exact values", {
  source <- demo_test_data()
  projected <- demo_rds_projection(source, "native")
  expect_identical(names(projected), c("cohort", "mixed_bp", "zip", "hrt_bin", "hrt_dose", "union", "auto"))
  expect_identical(projected$cohort$arm, source$cohort$arm)
  expect_identical(projected$cohort$value, c(2.5, 0, -1))
  expect_true(demo_compare_rds(projected, projected))
  altered <- projected
  altered$auto$value[1] <- 0
  expect_error(demo_compare_rds(altered, projected), class = "tabtools_error_demo_source")
  expect_error(demo_compare_rds(projected[-1], projected[-1]), class = "tabtools_error_demo_source")
  expect_error(demo_rds_projection(source, "n"), class = "tabtools_error_demo_source")
  source$cohort$bmi <- 20
  expect_error(demo_rds_projection(source, "native"), class = "tabtools_error_demo_source")
  expect_error(demo_rds_projection(demo_test_data()[demo_rds_names], "legacy"), class = "tabtools_error_demo_source")
})

test_that("CLI rejects stale pins, unused options and ambiguous modes", {
  valid <- c("--stage-native", "--stage-output=new-stage", "--commit=712044f8", "--tabtools-version=2.5.1")
  expect_true(demo_options(valid)[["stage-native"]])
  for (arg in c("--unknown", "--keep-work", "--stage-native", "--update", "--only=book.xlsx", "--demo-data-dir=data", "--receipt-dir=receipt")) expect_error(demo_options(c(valid, arg)), class = "tabtools_error_demo_source")
  expect_error(demo_options(valid[-2]), class = "tabtools_error_demo_source")
  stale <- sub("712044f8", "1255176d", valid, fixed = TRUE)
  expect_error(demo_options(stale), class = "tabtools_error_demo_source")
  expect_error(demo_options("--mode=n", "maker"), class = "tabtools_error_demo_source")
  expect_error(demo_options("--mode=native", "maker"), class = "tabtools_error_demo_source")
  expect_identical(demo_options(character(), "maker")[["mode"]], "legacy")
})

test_that("stage destination refuses existing paths, symlinks and work containment", {
  tmp <- withr::local_tempdir(pattern = "demo-source-path-")
  work <- file.path(tmp, "work")
  dir.create(work)
  existing <- file.path(tmp, "existing")
  writeLines("preserve me", existing)
  expect_error(demo_new_destination(existing), class = "tabtools_error_demo_source")
  expect_false(demo_is_link(file.path(tmp, "absent-new-destination")))
  expect_false(demo_path_exists(file.path(tmp, "absent-new-destination")))
  expect_error(demo_new_destination(work), class = "tabtools_error_demo_source")
  expect_identical(readLines(existing), "preserve me")
  expect_error(demo_new_destination(file.path(work, "stage"), work), class = "tabtools_error_demo_source")
  link <- file.path(tmp, "dangling")
  expect_true(file.symlink(file.path(tmp, "absent"), link))
  expect_error(demo_new_destination(link), class = "tabtools_error_demo_source")
  parent_link <- file.path(tmp, "linked-parent")
  expect_true(file.symlink(work, parent_link))
  expect_error(demo_new_destination(file.path(parent_link, "stage")), class = "tabtools_error_demo_source")
  expect_identical(demo_new_destination(file.path(tmp, "new"), work), file.path(normalizePath(tmp), "new"))
  expect_identical(demo_new_destination(file.path(tmp, "new-receipt"), work), file.path(normalizePath(tmp), "new-receipt"))
})

test_that("snapshot hash guard rejects tampering before data loading", {
  tmp <- withr::local_tempdir(pattern = "demo-source-hashes-")
  # These intentionally invalid files isolate the pre-read integrity boundary.
  for (file in demo_source_files()) writeLines("not a native file", file.path(tmp, file))
  info <- c(stata_tools_full_commit = demo_native_commit, tabtools_version = "2.5.1", demo_source_sha256 = demo_native_source_hash, rng = "mt64", source_status = "native_staged")
  utils::write.csv(data.frame(key = names(info), value = unname(info)), file.path(tmp, "SOURCE.csv"), row.names = FALSE)
  files <- demo_source_files()
  utils::write.csv(data.frame(file = files, sha256 = demo_sha256(file.path(tmp, files))), file.path(tmp, "FILES.csv"), row.names = FALSE)
  writeLines("tampered", file.path(tmp, "cohort.dta"))
  expect_error(demo_read_data(tmp), "hashes differ", class = "tabtools_error_demo_source")
  writeLines("extra", file.path(tmp, "extra.dta"))
  expect_error(demo_read_data(tmp), "Missing or extra", class = "tabtools_error_demo_source")
})

test_that("manifest guards clear scenario routes and enforce actual order", {
  inventory <- data.frame(kind = c("sheet", "sheet", "console", "report"), artefact = c("demo_book.xlsx", "demo_book.xlsx", "console_output.log", "demo_markdown_report.md"), item = c("First", "Second", "C01", "Report"), stata = c("", "", "command", ""))
  manifest <- inventory
  manifest$status <- "compared"
  manifest$reason <- manifest$scenario <- manifest$mask <- manifest$mode <- ""
  manifest$r <- c("", "", "command", "")
  manifest$golden <- c("demo/demo_book.xlsx:First", "demo/demo_book.xlsx:Second", "", "")
  expect_true(demo_check_manifest(manifest, inventory))
  bad <- manifest
  bad$scenario[1] <- "T01"
  expect_error(demo_check_manifest(bad, inventory), class = "tabtools_error_demo_source")
  bad <- manifest[c(2, 1, 3, 4), ]
  expect_error(demo_check_manifest(bad, inventory), class = "tabtools_error_demo_source")
  bad <- manifest
  bad$stata[3] <- "old command"
  expect_error(demo_check_manifest(bad, inventory), class = "tabtools_error_demo_source")
  bad <- manifest
  bad$status[2] <- "not_compared"
  expect_error(demo_check_manifest(bad, inventory), class = "tabtools_error_demo_source")
})

test_that("partial update cannot silently retain a stale source receipt", {
  files <- c("book.xlsx", "console_output.log", "console_output.md", "demo_markdown_report.md")
  accepted <- c(stata_tools_commit = "712044f8", tabtools_version = "2.5.1")
  expect_identical(demo_update_selection("book.xlsx", accepted, "712044f8", "2.5.1", files), "book.xlsx")
  old <- c(stata_tools_commit = "1255176d", tabtools_version = "2.1.14")
  expect_error(demo_update_selection("book.xlsx", old, "712044f8", "2.5.1", files), class = "tabtools_error_demo_source")
  for (only in c("unknown.xlsx", "book.xlsx,book.xlsx", "console_output.log", "console_output.md", ",book.xlsx", "book.xlsx,", "book.xlsx,,demo_markdown_report.md")) expect_error(demo_update_selection(only, accepted, "712044f8", "2.5.1", files), class = "tabtools_error_demo_source")
})

test_that("native runtime receipts use effective in-scope mt64 and reject default", {
  tmp <- withr::local_tempdir(pattern = "demo-source-runtime-")
  receipt <- file.path(tmp, "runtime.txt")
  writeLines(c("17", "mt64"), receipt)
  expect_identical(demo_read_runtime(receipt), c("17", "mt64"))
  expect_identical(demo_read_runtime(receipt, expected = c("17", "mt64")), c("17", "mt64"))
  expect_error(demo_read_runtime(receipt, expected = c("18", "mt64")), class = "tabtools_error_demo_source")
  for (bad in list(c("17", "default"), c("17", "mt64s"), "17", c("", "mt64"), c("17", "mt64", "extra"))) {
    writeLines(bad, receipt)
    expect_error(demo_read_runtime(receipt), class = "tabtools_error_demo_source")
  }
  wrapper <- readLines(file.path("stata", "run_demo.do"), warn = FALSE)
  expect_true('args export outdir want_version runtime_path' %in% wrapper)
  write_at <- which(wrapper == '    file write ttver "`c(stata_version)\'" _n "`c(rng_current)\'" _n')
  expect_length(write_at, 1L)
  expect_true(write_at > which(wrapper == 'do tabtools/demo/demo_tabtools.do'))
  expect_true(write_at < which(grepl('^display as result "run_demo.do:', wrapper)))
  expect_false(any(grepl('c(rng)', wrapper, fixed = TRUE)))
})

test_that("failed native execution preserves diagnostics and cleans only its child", {
  tmp <- withr::local_tempdir(pattern = "demo-source-cleanup-")
  work_parent <- file.path(tmp, "work")
  dir.create(work_parent)
  bystander <- file.path(work_parent, "other-session.txt")
  writeLines("untouched", bystander)
  dest <- file.path(tmp, "stage")
  # Independent forced engine failure exercises real preservation/cleanup;
  # it does not simulate successful native data or parity.
  previous <- demo_native_generate
  assign("demo_native_generate", function(root, opt, work) {
    writeLines("authored forced failure", file.path(work, "demo.log"))
    demo_abort("Forced engine failure.")
  }, envir = environment(demo_native_workflow))
  withr::defer(assign("demo_native_generate", previous, envir = environment(demo_native_workflow)))
  opt <- list("stage-native" = TRUE, "stage-output" = dest, "work-dir" = work_parent)
  expect_error(demo_native_workflow("unused", opt), "Forced engine failure", class = "tabtools_error_demo_source")
  expect_identical(list.files(work_parent), "other-session.txt")
  expect_identical(readLines(bystander), "untouched")
  expect_false(file.exists(dest))
  failed <- list.dirs(tmp, recursive = FALSE, full.names = TRUE)
  failed <- failed[startsWith(basename(failed), "stage-failure-")]
  expect_length(failed, 1L)
  expect_identical(readLines(file.path(failed, "demo.log")), "authored forced failure")
  receipt <- utils::read.csv(file.path(failed, "EXECUTION.csv"), colClasses = "character")
  expect_identical(receipt$value[receipt$key == "status"], "FAILED")
})

test_that("ordinary and tagged native missing states stay exact on both sides", {
  missing_values <- list(ordinary = NA_real_, a = haven::tagged_na("a"), b = haven::tagged_na("b"))
  sources <- lapply(missing_values, function(missing) {
    d <- demo_test_data()
    d$cohort$arm <- haven::labelled(c(1, missing, 2), c(Control = 1, Exposed = 2), label = "Arm")
    d
  })
  expect_false(haven::is_tagged_na(sources$ordinary$cohort$arm[2]))
  expect_identical(haven::na_tag(sources$a$cohort$arm[2]), "a")
  expect_identical(haven::na_tag(sources$b$cohort$arm[2]), "b")
  for (name in names(sources)) {
    same <- sources[[name]]
    expect_true(demo_compare_source(same, same, demo_schema(same)))
    projected <- demo_rds_projection(same, "native")
    expect_true(demo_compare_rds(projected, projected))
  }
  for (pair in list(c("ordinary", "a"), c("a", "b"))) {
    original <- sources[[pair[1]]]
    mutant <- sources[[pair[2]]]
    expect_error(demo_compare_source(mutant, original, demo_schema(original)), class = "tabtools_error_demo_source")
    expect_error(demo_compare_source(original, mutant, demo_schema(original)), class = "tabtools_error_demo_source")
    expect_error(demo_compare_rds(demo_rds_projection(mutant, "native"), demo_rds_projection(original, "native")), class = "tabtools_error_demo_source")
    expect_error(demo_compare_rds(demo_rds_projection(original, "native"), demo_rds_projection(mutant, "native")), class = "tabtools_error_demo_source")
  }
  # Missing codes in labelled dictionaries and nested schema attributes also
  # need exact equality even when the numerical observation vector is unchanged.
  attrs_a <- sources$a
  attr(attrs_a$cohort$arm, "labels") <- c(Control = 1, Unknown = haven::tagged_na("a"), Exposed = 2)
  attr(attrs_a$cohort$arm, "nested_provenance") <- list(code = haven::tagged_na("a"))
  schema_a <- demo_schema(attrs_a)
  expect_identical(haven::na_tag(attr(attrs_a$cohort$arm, "labels")[["Unknown"]]), "a")
  expect_true(demo_compare_source(attrs_a, attrs_a, schema_a))
  for (attribute in c("labels", "nested_provenance")) {
    attrs_b <- attrs_a
    if (attribute == "labels") attr(attrs_b$cohort$arm, "labels")[["Unknown"]] <- haven::tagged_na("b") else attr(attrs_b$cohort$arm, "nested_provenance")$code <- haven::tagged_na("b")
    expect_error(demo_compare_source(attrs_b, attrs_a, schema_a), class = "tabtools_error_demo_source")
    expect_error(demo_compare_source(attrs_a, attrs_b, schema_a), class = "tabtools_error_demo_source")
    expect_error(demo_compare_rds(demo_rds_projection(attrs_b, "native"), demo_rds_projection(attrs_a, "native")), class = "tabtools_error_demo_source")
  }
  schema_b <- schema_a
  schema_b$cohort$columns$arm$attributes$nested_provenance$code <- haven::tagged_na("b")
  expect_error(demo_compare_source(attrs_a, attrs_a, schema_b), class = "tabtools_error_demo_source")
})

test_that("maker writes new outputs atomically and refuses linked or directory outputs", {
  tmp <- withr::local_tempdir(pattern = "demo-maker-boundary-")
  fx <- file.path(tmp, "synthetic-legacy-input")
  dir.create(fx)
  # Deliberately synthetic tiny legacy maker fixtures, never native baselines.
  for (nm in demo_rds_names) {
    d <- demo_test_data()[[nm]]
    drops <- if (nm == "cohort") c("cost_sek", "lab_value", "ctrl_marker", "ctrl_grade", "fw", "event_type", "bmi") else if (nm == "auto") c("age", "stage", "size_class", "mpg_dup", "pclass") else character()
    for (drop in drops) d[[drop]] <- c(0, 1, 2)
    haven::write_dta(d, file.path(fx, paste0(nm, ".dta")))
  }
  root <- normalizePath("..")
  output <- file.path(tmp, "new.rds")
  args <- c("--mode=legacy", paste0("--fixtures-dir=", fx), paste0("--output=", output))
  expect_identical(demo_make_main(args, root), output)
  projected <- readRDS(output)
  expect_identical(names(projected), demo_rds_names)
  expect_identical(projected$cohort$value, structure(c(2.5, 0, -1), format.stata = "%10.0g"))
  expect_identical(attr(projected$cohort$arm, "label"), "Arm")
  expect_identical(attr(projected$cohort$arm, "labels"), c(Control = 1, Exposed = 2))
  expect_false(any(c("bmi", "cost_sek") %in% names(projected$cohort)))
  expect_false(any(c("age", "stage") %in% names(projected$auto)))
  # Explicit output replacement is the maker's established contract, unlike
  # new-only stage/receipt directories; failed preflight leaves it untouched.
  before <- demo_sha256(output)
  expect_identical(demo_make_main(args, root), output)
  expect_identical(demo_sha256(output), before)
  linked <- file.path(tmp, "dangling.rds")
  expect_true(file.symlink(file.path(tmp, "missing.rds"), linked))
  expect_error(demo_make_main(c(args[1:2], paste0("--output=", linked)), root), class = "tabtools_error_demo_source")
  expect_error(demo_make_main(c(args[1:2], paste0("--output=", fx)), root), class = "tabtools_error_demo_source")
  linked_parent <- file.path(tmp, "linked-parent")
  expect_true(file.symlink(fx, linked_parent))
  expect_error(demo_make_main(c(args[1:2], paste0("--output=", file.path(linked_parent, "new.rds"))), root), class = "tabtools_error_demo_source")
  expect_identical(demo_sha256(output), before)
})

test_that("failed R QA results and status are durable before the child exits nonzero", {
  tmp <- withr::local_tempdir(pattern = "demo-r-qa-failure-")
  receipt <- file.path(tmp, "receipt")
  dir.create(receipt)
  script <- file.path(tmp, "failed-result.R")
  paths <- c(normalizePath(file.path("tools", "demo_data.R")), normalizePath("demo_parity.R"), normalizePath(file.path("tools", "qa_result.R")))
  r_literal <- function(x) paste(deparse(x), collapse = "")
  lines <- c("env <- new.env(parent = globalenv())", sprintf("sys.source(%s, envir = env)", vapply(paths, r_literal, "")),
    'stopifnot(!"tabtools" %in% loadedNamespaces())',
    'df <- data.frame(context = "inert failure", test = "forced failing result", nb = 1L, failed = 1L, error = FALSE, warning = 0L, skipped = FALSE)',
    sprintf('result <- env$demo_finish_r_qa(df, "forced outside-block failure", env, %s)', r_literal(receipt)),
    sprintf('writeLines("returned after serialization", %s)', r_literal(file.path(receipt, "RETURNED.txt"))),
    'stopifnot(!"tabtools" %in% loadedNamespaces())',
    'quit(save = "no", status = if (identical(result$status, "PASS")) 0L else 1L)')
  writeLines(lines, script)
  log <- file.path(tmp, "failed-result.log")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = log, stderr = log)
  expect_identical(status, 1L)
  expect_identical(readLines(file.path(receipt, "RETURNED.txt")), "returned after serialization")
  df <- readRDS(file.path(receipt, "R-QA-RESULTS.rds"))
  expect_identical(df$failed, 1L)
  expect_identical(df$nb, 1L)
  expect_identical(readRDS(file.path(receipt, "R-QA-STRAY.rds")), "forced outside-block failure")
  info <- utils::read.csv(file.path(receipt, "R-QA.csv"), colClasses = "character")
  expect_identical(info$key, c("r_qa_status", "executed", "skipped", "failed"))
  expect_identical(info$value, c("FAIL", "2", "0", "2"))
  expect_true(any(readLines(log) == "QA-RESULT script=demo_parity.R status=FAIL executed=2 skipped=0 failed=2"))
})
