# Native demo source contracts; used only by the source-repository QA tools.
demo_native_names <- c("cohort", "mixed_bp", "zip", "hurdle", "hrt_bin", "hrt_dose", "union", "auto")
demo_rds_names <- demo_native_names[demo_native_names != "hurdle"]
demo_native_commit <- "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129"
demo_native_source_hash <- "b2b4c7edcc7c49a029a1e39aeac8e1474cc2ba6bd24e08378a8e2bc645af8abf"

demo_abort <- function(...) {
  text <- paste0(...)
  cli::cli_abort("{text}", class = "tabtools_error_demo_source", call = NULL)
}

demo_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) demo_abort(name, " must be a nonempty string.")
  x
}

demo_options <- function(args, kind = c("updater", "maker")) {
  kind <- match.arg(kind)
  value <- if (kind == "updater") c("commit", "tabtools-version", "stata-tools", "only", "demo-data-dir", "work-dir", "stage-output", "receipt-dir") else c("fixtures-dir", "output", "mode")
  flags <- if (kind == "updater") c("update", "stage-native") else character()
  out <- list()
  for (arg in args) {
    if (!startsWith(arg, "--")) demo_abort("Unknown argument: ", arg)
    key <- sub("=.*$", "", substring(arg, 3L))
    if (key %in% names(out)) demo_abort("Repeated option: --", key)
    if (key %in% flags && identical(arg, paste0("--", key))) out[[key]] <- TRUE
    else if (key %in% value && startsWith(arg, paste0("--", key, "="))) out[[key]] <- demo_scalar(substring(arg, nchar(key) + 4L), paste0("--", key))
    else demo_abort("Unknown or malformed option: ", arg)
  }
  if (kind == "updater") {
    stage <- isTRUE(out[["stage-native"]])
    update <- isTRUE(out[["update"]])
    if (stage && update) demo_abort("--stage-native and --update are mutually exclusive.")
    if (stage && (!is.null(out[["only"]]) || !is.null(out[["demo-data-dir"]]))) demo_abort("Initial stage cannot use --only or --demo-data-dir.")
    if (stage && !is.null(out[["receipt-dir"]])) demo_abort("Initial stage uses --stage-output, not --receipt-dir.")
    if (stage && is.null(out[["stage-output"]])) demo_abort("--stage-native requires --stage-output.")
    if (!stage && !is.null(out[["stage-output"]])) demo_abort("--stage-output requires --stage-native.")
    if (!stage && !update && any(c("commit", "tabtools-version", "stata-tools", "only", "work-dir") %in% names(out))) demo_abort("Native generation options require --stage-native or --update.")
    if (!update && !is.null(out[["receipt-dir"]])) demo_abort("--receipt-dir requires --update.")
    if (update && is.null(out[["demo-data-dir"]])) demo_abort("--update requires the accepted --demo-data-dir.")
    if (stage || update) {
      if (is.null(out[["commit"]]) || is.null(out[["tabtools-version"]])) demo_abort("Native generation requires explicit --commit and --tabtools-version.")
      if (!out[["commit"]] %in% c("712044f8", demo_native_commit) || !identical(out[["tabtools-version"]], "2.5.1")) demo_abort("Native demo generation is pinned to 712044f8 / 2.5.1.")
      if (!stage && is.null(out[["receipt-dir"]])) demo_abort("--update requires a durable --receipt-dir outside work.")
    }
  } else {
    mode <- if (is.null(out[["mode"]])) "legacy" else out[["mode"]]
    if (!mode %in% c("legacy", "native")) demo_abort("--mode must be exactly legacy or native.")
    if (identical(mode, "native") && is.null(out[["fixtures-dir"]])) demo_abort("Native mode requires --fixtures-dir.")
    out[["mode"]] <- mode
  }
  out
}

demo_is_link <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) & nzchar(link)
}

demo_path_exists <- function(path) file.exists(path) || dir.exists(path) || demo_is_link(path)

# Ordinary NA and Stata's tagged .a/.b/... are distinct, also in attributes.
demo_identical <- function(x, y) identical(x, y, single.NA = FALSE)

demo_canonical_dir <- function(path) {
  path <- path.expand(demo_scalar(path, "Directory"))
  if (grepl('["`$\r\n]', path)) demo_abort("Directory contains unsupported native path characters.")
  if (!dir.exists(path)) demo_abort("Directory does not exist: ", path)
  absolute <- if (startsWith(path, "/")) path else file.path(getwd(), path)
  bits <- strsplit(absolute, "/", fixed = TRUE)[[1L]]
  cur <- "/"
  for (bit in bits[nzchar(bits)]) {
    if (bit %in% c(".", "..")) demo_abort("Directory must be canonical: ", path)
    cur <- file.path(cur, bit)
    if (demo_is_link(cur)) demo_abort("Symlink directory is not allowed: ", cur)
  }
  result <- normalizePath(absolute, mustWork = TRUE)
  if (file.access(result, 2L) != 0L) demo_abort("Directory is not writable: ", result)
  result
}

demo_new_destination <- function(path, work_parent = NULL) {
  path <- path.expand(demo_scalar(path, "Destination"))
  if (demo_path_exists(path)) demo_abort("Destination already exists: ", path)
  parent <- demo_canonical_dir(dirname(path))
  leaf <- basename(path)
  if (leaf %in% c(".", "..", "") || grepl('["`$\r\n]', leaf)) demo_abort("Invalid destination name.")
  dest <- file.path(parent, leaf)
  if (!is.null(work_parent) && (identical(dest, work_parent) || startsWith(dest, paste0(work_parent, "/")))) demo_abort("Destination must be outside the work directory.")
  dest
}

demo_sha256 <- function(paths) {
  if (!length(paths)) return(character())
  if (!nzchar(Sys.which("sha256sum"))) demo_abort("sha256sum is required for native provenance.")
  vapply(paths, function(path) {
    if (!file.exists(path) || dir.exists(path) || demo_is_link(path)) demo_abort("Missing or linked source file: ", path)
    out <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(out, "status")) || length(out) != 1L || !grepl("^[[:xdigit:]]{64} ", out)) demo_abort("Cannot hash source file: ", path)
    substr(out, 1L, 64L)
  }, "", USE.NAMES = FALSE)
}

demo_source_files <- function() c(paste0(demo_native_names, ".dta"), paste0(demo_native_names, ".storage.csv"), "SCHEMA.rds", "extraction.do")

demo_native_plan <- function(demo_lines, export_dir) {
  section <- function(head, stop_at) {
    i <- which(sub("\\s+$", "", demo_lines) == head)
    if (length(i) != 1L) demo_abort("Unique native section not found: ", head)
    tail <- demo_lines[-seq_len(i)]
    end <- which(grepl(stop_at, tail))[1L]
    if (is.na(end) || end < 2L) demo_abort("Native section terminator not found: ", head)
    x <- tail[seq_len(end - 1L)]
    x[x != "preserve"]
  }
  save <- function(name) c(
    sprintf('save "@@DATA@@/%s.dta", replace', name),
    sprintf('file open ttmeta using "@@DATA@@/%s.storage.csv", write text replace', name),
    'file write ttmeta "variable,storage" _n',
    'unab ttvars : _all',
    'foreach ttv of local ttvars {',
    '    local tttype : type `ttv\'',
    '    file write ttmeta "`ttv\',`tttype\'" _n',
    '}', 'file close ttmeta')
  lines <- c("version 17.0", "clear all", "set rng mt64", "set more off",
    gsub("`repo_root'", export_dir, section("**# Build analysis dataset", "^tempfile analysis"), fixed = TRUE), save("cohort"),
    section("**# Sheet 15: Mixed Model -- Random effects with relabel + ICC", "^collect clear"), save("mixed_bp"),
    section("**# Sheet 31: ZIP ZINB -- Zero-inflated count models", "^collect clear"), save("zip"),
    section("**# Sheet 32: Hurdle -- Cragg hurdle model", "^collect clear"), save("hurdle"),
    section("* Binary HRT model frame: one non-reference row", "^collect clear"), save("hrt_bin"),
    section("* Dose category model frame: three non-reference rows after header + reference", "^collect clear"), save("hrt_dose"),
    "webuse union, clear", save("union"), "sysuse auto, clear", save("auto"),
    'display as result "RESULT: demo_data tests=8 pass=8 fail=0"')
  list(lines = lines, names = demo_native_names)
}

demo_read_native_files <- function(dir, names = demo_native_names) {
  if (!identical(names, demo_native_names)) demo_abort("Native dataset inventory differs from the fixed eight snapshots.")
  data <- stats::setNames(lapply(names, function(nm) as.data.frame(haven::read_dta(file.path(dir, paste0(nm, ".dta"))))), names)
  storage <- stats::setNames(lapply(names, function(nm) {
    s <- utils::read.csv(file.path(dir, paste0(nm, ".storage.csv")), colClasses = "character", na.strings = character())
    if (!identical(names(s), c("variable", "storage")) || !identical(s$variable, names(data[[nm]])) || anyDuplicated(s$variable) || any(!grepl("^(byte|int|long|float|double|str[1-9][0-9]*|strL)$", s$storage))) demo_abort("Invalid native storage inventory: ", nm)
    s
  }), names)
  attr(data, "native_storage") <- storage
  data
}

demo_schema <- function(data) {
  if (!identical(names(data), demo_native_names) || is.null(attr(data, "native_storage"))) demo_abort("Complete native data and storage inventory required.")
  stats::setNames(lapply(names(data), function(nm) {
    d <- data[[nm]]
    s <- attr(data, "native_storage")[[nm]]
    if (!identical(s$variable, names(d))) demo_abort("Native variable order differs: ", nm)
    list(rows = nrow(d), variables = names(d), storage = s$storage,
      columns = lapply(d, function(x) list(type = typeof(x), class = class(x), attributes = attributes(x))))
  }), names(data))
}

demo_read_data <- function(dir, names = demo_native_names) {
  if (!identical(names, demo_native_names)) demo_abort("Native dataset inventory differs.")
  dir <- demo_canonical_dir(dir)
  if (any(demo_is_link(list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)))) demo_abort("Linked snapshot files are not allowed.")
  if (!identical(sort(list.files(dir, all.files = TRUE, no.. = TRUE)), sort(c(demo_source_files(), "SOURCE.csv", "FILES.csv")))) demo_abort("Missing or extra native snapshot files.")
  source <- utils::read.csv(file.path(dir, "SOURCE.csv"), colClasses = "character", na.strings = character())
  if (!identical(names(source), c("key", "value")) || anyDuplicated(source$key)) demo_abort("Invalid source receipt.")
  info <- stats::setNames(source$value, source$key)
  required <- c(stata_tools_full_commit = demo_native_commit, tabtools_version = "2.5.1", demo_source_sha256 = demo_native_source_hash, rng = "mt64", source_status = "native_staged")
  if (!identical(unname(info[names(required)]), unname(required))) demo_abort("Native snapshot source pin/receipt differs.")
  files <- utils::read.csv(file.path(dir, "FILES.csv"), colClasses = "character", na.strings = character())
  if (!identical(names(files), c("file", "sha256")) || !identical(files$file, demo_source_files()) || anyDuplicated(files$file)) demo_abort("Invalid snapshot hash inventory.")
  if (!identical(demo_sha256(file.path(dir, files$file)), files$sha256)) demo_abort("Native snapshot hashes differ from their receipt.")
  data <- demo_read_native_files(dir, names)
  schema <- readRDS(file.path(dir, "SCHEMA.rds"))
  if (!demo_identical(demo_schema(data), schema)) demo_abort("Native snapshots differ from their frozen schema.")
  data
}

demo_compare_source <- function(fresh, pinned, schema) {
  if (!demo_identical(demo_schema(fresh), schema) || !demo_identical(demo_schema(pinned), schema)) demo_abort("Fresh native schema differs from accepted schema.")
  for (nm in demo_native_names) {
    if (!identical(names(fresh[[nm]]), names(pinned[[nm]])) || nrow(fresh[[nm]]) != nrow(pinned[[nm]])) demo_abort("Fresh native dataset shape differs: ", nm)
    for (v in names(pinned[[nm]])) if (!demo_identical(fresh[[nm]][[v]], pinned[[nm]][[v]])) demo_abort("Fresh native values/labels/order differ: ", nm, "/", v)
  }
  invisible(TRUE)
}

demo_rds_projection <- function(data, mode) {
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) || !mode %in% c("legacy", "native")) demo_abort("Projection mode must be exactly legacy or native.")
  expected <- if (mode == "native") demo_native_names else demo_rds_names
  if (!identical(names(data), expected)) demo_abort("RDS projection dataset inventory differs.")
  drops <- list(cohort = c("cost_sek", "lab_value", "ctrl_marker", "ctrl_grade", "fw", "event_type", "bmi"), auto = c("age", "stage", "size_class", "mpg_dup", "pclass"))
  stats::setNames(lapply(demo_rds_names, function(nm) {
    d <- as.data.frame(data[[nm]])
    drop <- drops[[nm]]
    if (mode == "legacy" && !all(drop %in% names(d))) demo_abort("Missing legacy-only columns: ", nm)
    if (mode == "native" && any(drop %in% names(d))) demo_abort("Scenario-only columns in native demo: ", nm)
    d[if (mode == "legacy") setdiff(names(d), drop) else names(d)]
  }), demo_rds_names)
}

demo_compare_rds <- function(rds, expected) {
  if (!identical(names(rds), demo_rds_names) || !demo_identical(rds, expected)) demo_abort("Complete native demo RDS differs; rebuild with --mode=native.")
  invisible(TRUE)
}

demo_write_source <- function(dir, info) {
  data <- demo_read_native_files(dir)
  saveRDS(demo_schema(data), file.path(dir, "SCHEMA.rds"), version = 3)
  utils::write.csv(data.frame(key = names(info), value = unname(info)), file.path(dir, "SOURCE.csv"), row.names = FALSE)
  files <- demo_source_files()
  utils::write.csv(data.frame(file = files, sha256 = demo_sha256(file.path(dir, files))), file.path(dir, "FILES.csv"), row.names = FALSE)
  invisible(demo_read_data(dir))
}
