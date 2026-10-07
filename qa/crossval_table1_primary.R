library(testthat)
library(tabtools)

# Exact T38–T44 specifications live in qa/data/table1_primary_cases.csv.
# T identifiers were unused in merged scenarios.csv (last T37); TC is reserved
# for tabcell. This isolated oracle never overwrites existing golden fixtures.
# Source identity is the full immutable 2.5.1 ado closure below, not an adopath
# version guess. It is authenticated before prerequisites and after execution.

# Immutable git 712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129: all 73 ado
# files include a conservative superset of the Table1 and shared-sink transitive
# closure. The package manifest/help are pinned too. Never derive expected hashes
# from TABTOOLS_STATA_DIR, which is untrusted configuration.
t1_native_pin <- c(
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

t1_native_authenticate <- function(source_dir) {
  source_dir <- normalizePath(source_dir, winslash = "/", mustWork = FALSE)
  if (grepl('["\r\n`]', source_dir)) stop("TABTOOLS_STATA_DIR contains a quoting character")
  paths <- file.path(source_dir, names(t1_native_pin))
  missing <- names(t1_native_pin)[!file.exists(paths)]
  if (length(missing)) stop("Pinned Table1 source files are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  actual <- unname(tools::md5sum(paths))
  if (!identical(actual, unname(t1_native_pin))) {
    bad <- names(t1_native_pin)[is.na(actual) | actual != unname(t1_native_pin)]
    stop("Native source is not Stata-Tools 712044f8 (tabtools 2.5.1): ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  header <- readLines(file.path(source_dir, "tabtools.ado"), n = 1L, warn = FALSE)
  version <- sub("^\\*! tabtools Version ([0-9.]+) .*", "\\1", header)
  if (!identical(version, "2.5.1")) stop("Authenticated native version header is invalid", call. = FALSE)
  list(directory = source_dir, version = version,
       revision = "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129",
       files = names(t1_native_pin)[endsWith(names(t1_native_pin), ".ado")])
}

t1_native_load <- function(pin, extra_adopath = NULL) {
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

t1_native_cases <- function() list(
  T38 = list(native = "wt(w) wtn wtcompare nosmallcells", weight = TRUE),
  T39 = list(native = "wt(w) wtn wtcompare smallcells(5, primary)", weight = TRUE, mode = "primary"),
  T40 = list(native = "wt(w) wtn wtcompare smallcells(5)", weight = TRUE, mode = "strict"),
  T41 = list(native = "smallcells(5, primary)", mode = "primary"),
  T42 = list(native = 'nosmallcells cellreplace("Age" 1 "FIRST" \\ " Age " "2" "LAST")', numeric = TRUE),
  T43 = list(native = 'wt(w) wtn wtcompare smallcells(5, primary) cellreplace("Effective sample size" 3 "2")',
             weight = TRUE, mode = "primary", unsafe = TRUE),
  T44 = list(native = 'nosmallcells cellreplace("Mean\u00b1SD" 1 "Ncustom")', descriptor = TRUE))

t1_native_read_csv <- function(path, character = FALSE) {
  # Native export delimited writes a one-column missing row as a blank line.
  utils::read.csv(path, check.names = FALSE, blank.lines.skip = FALSE,
    na.strings = if (character) NULL else c("", ".", ".d"), stringsAsFactors = FALSE,
    colClasses = if (character) "character" else NA)
}

t1_native_run <- function(directory, source_dir, cases, extra_adopath = NULL) {
  pin <- t1_native_authenticate(source_dir)
  stata_tmp <- file.path(directory, "stata-tmp")
  dir.create(stata_tmp)
  script <- c("version 17.0", "clear all", "set more off", "set linesize 255", "set type double",
    t1_native_load(pin, extra_adopath),
    'postfile counts str4 id str8 mode double threshold primary secondary linked replacements long table_rows table_cols using "counts.dta", replace')
  for (id in names(cases)) {
    case <- cases[[id]]
    script <- c(script, sprintf('display "TABLE1_NATIVE_START_%s"', id),
      "clear", "set obs 10", "generate byte g = cond(_n <= 2, 1, 2)",
      "generate double age = _n", "generate double w = 1", "replace w = 3 in 2",
      if (isTRUE(case$numeric)) 'label define groups 1 "2" 2 "1", replace' else
        'label define groups 1 "Small" 2 "Large", replace',
      "label values g groups", 'label variable age "Age"',
      sprintf('desctab, by(g) vars(age contn) %s smd nopvalue format(%%6.2f) nformat(%%5.2f) frame(pub, replace)', case$native),
      "matrix T = r(table)", "local replacements = r(n_cellreplace)")
    if (!is.null(case$mode)) {
      script <- c(script, 'local mode "`r(smallcells_mode)\'"', "local k = r(smallcells)",
        "local primary = r(N_primary_suppressed)", "local secondary = r(N_secondary_suppressed)",
        "local linked = r(N_derived_suppressed)", "matrix S = r(suppression)")
    } else script <- c(script, 'local mode ""', "local k 0", "local primary 0", "local secondary 0", "local linked 0")
    script <- c(script,
      sprintf('post counts ("%s") ("`mode\'") (`k\') (`primary\') (`secondary\') (`linked\') (`replacements\') (rowsof(T)) (colsof(T))', id),
      sprintf('frame pub: export delimited using "%s-frame.csv", replace', id),
      "preserve", "clear", "svmat double T, names(col)",
      sprintf('export delimited using "%s-table.csv", replace', id), "restore")
    if (!is.null(case$mode)) script <- c(script, "preserve", "clear", "svmat double S, names(col)",
      sprintf('export delimited using "%s-suppression.csv", replace', id), "restore")
    script <- c(script, sprintf('display "TABLE1_NATIVE_END_%s"', id))
  }
  script <- c(script, "postclose counts", 'use "counts.dta", clear',
    'export delimited using "counts.csv", replace',
    # Native selector/refusal controls are independently expected errors.
    "clear", "set obs 10", "generate byte g = cond(_n <= 5, 1, 2)", "generate age = _n",
    'label define groups 1 "Small" 2 "Large", replace', "label values g groups", 'label variable age "Age"',
    'capture desctab, by(g) vars(age contn) nopvalue cellreplace("age" 1 "x")', "assert _rc == 198",
    'capture desctab, by(g) vars(age contn) nopvalue cellreplace("Age" "small" "x")', "assert _rc == 198",
    'capture desctab, by(g) vars(age contn) nopvalue cellreplace("Age" 99 "x")', "assert _rc == 125",
    'capture desctab, by(g) vars(age contn) nopvalue smallcells(5) nosmallcells', "assert _rc == 198",
    "generate other = age", 'label variable other "Age"',
    'capture desctab, by(g) vars(age contn \\ other contn) nopvalue cellreplace("Age" 1 "x")', "assert _rc == 198",
    'display "TABLE1_NATIVE_COMPLETE"')
  writeLines(script, file.path(directory, "table1.do"), useBytes = TRUE)
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "table1.do"), wd = directory,
                        env = c(STATATMP = stata_tmp), timeout = 120L, error_on_status = FALSE)
  logfile <- file.path(directory, "table1.log")
  if (!file.exists(logfile)) stop("Native Table1 oracle produced no log", call. = FALSE)
  log <- readLines(logfile, warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  markers <- c("TABLE1_NATIVE_COMPLETE", paste0("TABLE1_NATIVE_START_", names(cases)),
               paste0("TABLE1_NATIVE_END_", names(cases)))
  if (proc$status != 0L || length(errors) || !all(markers %in% trimws(log)) ||
      !any(grepl("end of do-file", log, fixed = TRUE))) {
    stop("Native Table1 oracle incomplete: ", paste(errors, collapse = ", "), call. = FALSE)
  }
  completed <- t1_native_authenticate(source_dir)
  if (!identical(pin, completed)) stop("Native Table1 source changed during execution", call. = FALSE)
  invisible(list(pin = completed, log = log))
}

t1_native_r <- function(case) {
  d <- data.frame(g = seq_len(10) > 2L, age = seq_len(10), w = c(1, 3, rep(1, 8)))
  d$g <- haven::labelled(as.integer(d$g) + 1L,
    if (isTRUE(case$numeric)) c("2" = 1L, "1" = 2L) else c(Small = 1L, Large = 2L))
  attr(d$age, "label") <- "Age"
  args <- list(data = d, by = "g", vars = c(age = "contn"), smd = TRUE, nopvalue = TRUE,
               format = "%6.2f", nformat = "%5.2f", smallcells = if (is.null(case$mode)) NULL else 5,
               smallcells_mode = if (is.null(case$mode)) "strict" else case$mode)
  if (isTRUE(case$weight)) args <- c(args, list(wt = "w", wtn = TRUE, wtcompare = TRUE))
  if (isTRUE(case$numeric)) args$cellreplace <- list(list(row = "Age", column = 1L, text = "FIRST"),
    list(row = " Age ", column = "2", text = "LAST"))
  if (isTRUE(case$descriptor)) args$cellreplace <- list(list(row = "Mean\u00b1SD", column = 1L, text = "Ncustom"))
  do.call(table1_tc, args)
}

test_that("native numeric CSV reading preserves literal missing-row matrix shapes", {
  directory <- withr::local_tempdir(pattern = "wp-3f-csv-shape-")
  cases <- list(
    strict_missing = list(bytes = "smd\n\n", values = NA_real_),
    header_only = list(bytes = "smd\n", values = numeric()),
    mixed_missing = list(bytes = "smd\n0.125\n\n.d\n", values = c(0.125, NA_real_, NA_real_)))
  for (id in names(cases)) {
    path <- file.path(directory, paste0(id, ".csv"))
    bytes <- charToRaw(cases[[id]]$bytes)
    writeBin(bytes, path)
    expect_identical(readBin(path, "raw", n = length(bytes) + 1L), bytes)
    actual <- as.matrix(t1_native_read_csv(path))
    storage.mode(actual) <- "double"
    expected <- matrix(cases[[id]]$values, ncol = 1L, dimnames = list(NULL, "smd"))
    expect_identical(actual, expected)
  }
  # The old default reader loses the independently verified T40 blank row.
  old <- utils::read.csv(file.path(directory, "strict_missing.csv"), check.names = FALSE)
  expect_identical(dim(as.matrix(old)), c(0L, 1L))
})

test_that("T38–T44 native Table1 masks, modes, selectors and analytical values are authentic", {
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to pinned Stata 2.5.1")
  t1_native_authenticate(source_dir)
  skip_if_not_installed("processx")
  skip_if_not_installed("haven")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  directory <- withr::local_tempdir(pattern = "wp-3f-native-")
  keys <- paste0("tabtools.", getFromNamespace(".tt_option_keys", "tabtools"))
  withr::local_options(stats::setNames(lapply(keys, getOption), keys))
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  old_state <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(old_state, envir = state)
  })
  tabtools_options(clear = TRUE)
  cases <- t1_native_cases()
  manifest <- utils::read.csv(test_path("data", "table1_primary_cases.csv"),
    colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(manifest$id, names(cases))
  options <- vapply(cases, `[[`, "", "native")
  expect_identical(manifest$native_options, unname(options))
  if (!identical(manifest$id, names(cases)) || !identical(manifest$native_options, unname(options))) {
    stop("Native Table1 scenario manifest disagrees with executable cases", call. = FALSE)
  }
  completed <- t1_native_run(directory, source_dir, cases)
  read <- function(name, character = FALSE) t1_native_read_csv(file.path(directory, name), character)
  counters <- read("counts.csv", TRUE)
  expect_identical(counters$id, names(cases))
  matched <- identical(counters$id, names(cases))
  for (id in names(cases)) {
    case <- cases[[id]]
    native <- read(paste0(id, "-frame.csv"), TRUE)
    count <- counters[counters$id == id, ]
    native_table <- as.matrix(read(paste0(id, "-table.csv")))
    storage.mode(native_table) <- "double"
    native_dimensions <- c(as.integer(count$table_rows), as.integer(count$table_cols))
    # Every declared case analyses exactly Age, with SMD and no p-value column.
    expect_identical(native_dimensions, c(1L, 1L), info = id)
    expect_identical(dim(native_table), native_dimensions, info = id)
    expect_identical(colnames(native_table), "smd", info = id)
    matched <- matched && identical(native_dimensions, c(1L, 1L)) &&
      identical(dim(native_table), native_dimensions) && identical(colnames(native_table), "smd")
    if (id == "T40") {
      expect_identical(unname(native_table), matrix(NA_real_, 1L, 1L))
      matched <- matched && identical(unname(native_table), matrix(NA_real_, 1L, 1L))
    }
    r <- t1_native_r(case)
    if (isTRUE(case$unsafe)) {
      expect_identical(native$Wt_1[3], "2")
      expect_identical(count$replacements, "1")
      matched <- matched && identical(native$Wt_1[3], "2") && identical(count$replacements, "1")
      for (text in list(NULL, "MASK", "")) {
        d <- data.frame(g = haven::labelled(c(1L, 1L, rep(2L, 8)), c(Small = 1L, Large = 2L)),
                        age = seq_len(10), w = c(1, 3, rep(1, 8)))
        error <- tryCatch(table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", wtn = TRUE,
          wtcompare = TRUE, smallcells = 5, smallcells_mode = "primary", masktext = text,
          cellreplace = list(list(row = "Effective sample size", column = 3L, text = "2"))), error = identity)
        expect_s3_class(error, "tabtools_error_cellreplace_protected")
        matched <- matched && inherits(error, "tabtools_error_cellreplace_protected")
      }
    } else {
      cells <- unname(as.matrix(as.data.frame(r)))
      want <- unname(as.matrix(native))
      expect_identical(cells, want, info = id)
      matched <- matched && identical(cells, want)
    }
    if (!is.null(case$mode)) {
      expect_identical(count$threshold, "5", info = id)
      s <- as.matrix(read(paste0(id, "-suppression.csv")))
      storage.mode(s) <- "double"
      # Matrix order is Cr1,Wt1,Cr2,Wt2; publication order is Cr1,Cr2,Wt1,Wt2.
      # Compare exact case-preserved identities and coordinates, never positions.
      expect_identical(colnames(r$stored$suppression), colnames(s), info = id)
      expect_equal(unname(r$stored$suppression), unname(s), tolerance = 0, info = id)
      expected <- list(threshold = 5L, mode = case$mode,
        n_masked = as.integer(sum(s %in% c(1, 2))), n_linked = as.integer(sum(s == 3)))
      expect_identical(r$stored$smallcells, expected, info = id)
      expect_identical(r$stored$smallcells_mode, count$mode)
      expect_equal(c(r$stored$N_primary_suppressed, r$stored$N_secondary_suppressed, r$stored$N_derived_suppressed),
                   as.numeric(unlist(count[c("primary", "secondary", "linked")])), tolerance = 0)
      matched <- matched && identical(r$stored$smallcells, expected) &&
        identical(count$threshold, "5") &&
        identical(r$stored$smallcells_mode, count$mode) && identical(unname(r$stored$suppression), unname(s)) &&
        identical(colnames(r$stored$suppression), colnames(s)) &&
        identical(as.numeric(c(r$stored$N_primary_suppressed, r$stored$N_secondary_suppressed, r$stored$N_derived_suppressed)),
                  as.numeric(unlist(count[c("primary", "secondary", "linked")])))
    } else {
      expect_identical(r$stored$smallcells,
        list(threshold = 0L, mode = "strict", n_masked = 0L, n_linked = 0L), info = id)
      expect_null(r$stored$smallcells_mode)
      matched <- matched && identical(r$stored$smallcells,
        list(threshold = 0L, mode = "strict", n_masked = 0L, n_linked = 0L)) && is.null(r$stored$smallcells_mode)
    }
    if (!isTRUE(case$unsafe)) {
      analytical <- if (!is.null(r$stored$raw)) r$stored$raw$table else r$stored$table
      expect_identical(colnames(analytical), colnames(native_table), info = id)
      expect_equal(unname(analytical), unname(native_table), tolerance = 1e-12, info = id)
      matched <- matched && identical(colnames(analytical), colnames(native_table)) &&
        isTRUE(all.equal(unname(analytical), unname(native_table), tolerance = 1e-12))
    }
    if (isTRUE(case$numeric)) {
      expect_identical(r$stored$n_cellreplace, 2L)
      matched <- matched && identical(r$stored$n_cellreplace, 2L)
    }
    if (isTRUE(case$descriptor)) {
      m <- r$meta$sample_accounting$measures
      hit <- m$population_id == "group/1" & m$metric == "reported_n"
      expect_identical(m$reason[hit], "replaced_by_cellreplace")
      expect_true(is.na(m$value[hit]))
      matched <- matched && identical(m$reason[hit], "replaced_by_cellreplace") &&
        length(m$value[hit]) == 1L && is.na(m$value[hit])
    }
  }
  if (!matched) stop("Native Table1 comparison failed; no completion receipt", call. = FALSE)
  cat(sprintf("TABLE1 NATIVE RECEIPT cases=7 native=%s revision=%s authenticated=true exact_grid_and_matrix_identity=true numeric_matrix_shape=true strict_missing_row=true protected_R_divergence=true\n",
              completed$pin$version, completed$pin$revision))
})

test_that("Table1 native provenance rejects changed helpers and adopath fallback", {
  source_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(source_dir), "set TABTOOLS_STATA_DIR to pinned Stata 2.5.1")
  pin <- t1_native_authenticate(source_dir)
  directory <- withr::local_tempdir(pattern = "wp-3f-native-pin-")
  copy <- file.path(directory, "copy")
  dir.create(copy)
  expect_true(all(file.copy(file.path(pin$directory, names(t1_native_pin)), copy)))
  expect_identical(t1_native_authenticate(copy)$files, pin$files)
  helper <- file.path(copy, "_desctab_collect.ado")
  write("* damaged native helper", helper, append = TRUE)
  expect_error(t1_native_authenticate(copy), "not Stata-Tools")
  expect_true(file.copy(file.path(pin$directory, "_desctab_collect.ado"), helper, overwrite = TRUE))
  expect_identical(t1_native_authenticate(copy)$version, "2.5.1")
  skip_if_not_installed("processx")
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  shadow <- file.path(directory, "shadow")
  dir.create(shadow)
  writeLines(c("program define desctab", "end"), file.path(shadow, "desctab.ado"))
  run <- file.path(directory, "run")
  dir.create(run)
  expect_error(t1_native_run(run, source_dir, t1_native_cases()[1], extra_adopath = shadow), "incomplete")
})
