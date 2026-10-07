# Exact current 2.5.6 person-time rounding beside immutable 2.5.1 S03.
# This is a comparator peer only, after full original input/raw/sink checks.
golden_s03_original <- c(
  "tests/testthat/fixtures/stratetab_blocks.csv" = "a978e4f833d1f62c13829deb2951efb4090bf1f80a4226d47ce64bd83fc63c99",
  "qa/stata/golden_stratetab_blocks.do" = "fe997115dcd8eea5e908c635a229657249cb9c7dcb893ca4a63c37f5ce8a31e8",
  "tests/testthat/golden/scenarios.csv" = "703cfa3b4e682cf2abe6638588dd9c316770ab4d6172e6f834d4a01eace08bbe",
  "tests/testthat/golden/S03.csv" = "89872c33e4c004976dfddf4f409a2ae9686ca1f37d64ce2c9c8c3b356a317be0",
  "tests/testthat/golden/S03.md" = "8230c5528943428b79d88d5cec307b549a014bb302fa0920ebe948e0e490f76f",
  "tests/testthat/golden/S03_console.txt" = "b65810627c4b034b76dca60c9a50e50ddc047b046a8c6b3063cd47b50c5c16ca",
  "tests/testthat/golden/S03_stored.csv" = "ea92de67134607b66dcdacb4fe4906efae8d3643c16d3f9f989d089aaf0f7e66",
  "tests/testthat/golden/stratetab.xlsx" = "b4620b3d3db0f1e1326c1074e69b9e835286db5e43bbf1b8a9e4d1d475dfbf03"
)

golden_patch_s03_round <- function(tt) {
  root <- normalizePath(file.path(golden_dir(), "../../.."), mustWork = TRUE)
  sha <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
  for (path in names(golden_s03_original)) {
    # qa/ is deliberately absent from built packages; its mandatory captured
    # source snapshot is checked below, and a live QA source only if present.
    if (path == "qa/stata/golden_stratetab_blocks.do" && !file.exists(file.path(root, path))) next
    testthat::expect_identical(sha(file.path(root, path)), golden_s03_original[[path]],
      info = paste("immutable S03 input/native artifact", path))
  }
  native <- Sys.getenv("TABTOOLS_S03_NATIVE_DIR",
    unset = file.path(golden_dir(), "post251-s03", "out"))
  if (!nzchar(native)) stop("Authenticated S03 current-native output is required")
  receipt_path <- file.path(dirname(native), "native-receipt-qualified.json")
  testthat::expect_identical(sha(receipt_path), "594baf9e50252dd16a91c0e7bad4e83b08b6e044db95a323f2e48a065feb030d")
  receipt <- jsonlite::fromJSON(receipt_path)
  source_snapshot <- file.path(dirname(native), "source", "golden_stratetab_blocks.do")
  testthat::expect_identical(sha(source_snapshot), golden_s03_original[["qa/stata/golden_stratetab_blocks.do"]])
  input <- receipt$extra_input_sha256
  source_key <- names(input)[endsWith(names(input), "/qa/stata/golden_stratetab_blocks.do")]
  fixture_key <- names(input)[endsWith(names(input), "/tests/testthat/fixtures/stratetab_blocks.csv")]
  testthat::expect_length(source_key, 1L)
  testthat::expect_length(fixture_key, 1L)
  testthat::expect_identical(input[[source_key]], golden_s03_original[["qa/stata/golden_stratetab_blocks.do"]])
  testthat::expect_identical(input[[fixture_key]], golden_s03_original[["tests/testthat/fixtures/stratetab_blocks.csv"]])
  testthat::expect_identical(receipt$source_pin, "4eecca4d09d61df0cfe773fc2024b015920b5fe6")
  testthat::expect_identical(receipt$recipe_sha256,
    "cd3a243c31230fedaddc2b0930c7824164b64c42988b2c529489cbc61487dae7")
  testthat::expect_true(isTRUE(receipt$cleanup_verified))
  expected_files <- c("S03_1.dta", "S03_2.dta", "S03_3.dta", "S03.console.txt",
    "S03.xlsx", "S03.csv", "S03.md", "S03.frame.csv")
  testthat::expect_identical(sort(names(receipt$files)), sort(expected_files))
  testthat::expect_identical(sort(list.files(native)), sort(expected_files))
  for (path in expected_files) testthat::expect_identical(sha(file.path(native, path)), receipt$files[[path]])

  # All source fields/counts/times, including the reordered second outcome,
  # retain their original values and labelled identities, not just the tie.
  blocks <- golden_strate_blocks("S03")
  testthat::expect_length(tt$meta$rate_blocks, 3L)
  for (k in seq_len(3L)) {
    actual <- haven::read_dta(file.path(native, paste0("S03_", k, ".dta")))
    for (field in c("D", "Y", "Rate", "Lower", "Upper")) {
      value <- as.numeric(blocks[[k]][[paste0("_", field)]])
      testthat::expect_identical(as.numeric(actual[[paste0("_", field)]]), value)
      testthat::expect_identical(as.numeric(tt$meta$rate_blocks[[k]][[field]]), value)
    }
    for (field in c("_Lower", "_Upper")) {
      testthat::expect_identical(attr(actual[[field]], "label", exact = TRUE),
        attr(blocks[[k]][[field]], "label", exact = TRUE))
    }
    code <- as.numeric(blocks[[k]]$group)
    labels <- attr(blocks[[k]]$group, "labels", exact = TRUE)
    testthat::expect_identical(as.numeric(actual$group), code)
    testthat::expect_identical(as.numeric(tt$meta$rate_blocks[[k]]$group), code)
    # Stata stores value labels in numeric-code order; compare the complete
    # named mapping independently of storage order.
    testthat::expect_identical(sort(attr(actual$group, "labels", exact = TRUE)), sort(labels))
    testthat::expect_identical(sort(attr(tt$meta$rate_blocks[[k]]$group, "labels", exact = TRUE)), sort(labels))
    testthat::expect_identical(as.numeric(actual$`_Y`[as.numeric(actual$group) == 2]), 2210.125)
  }
  testthat::expect_identical(tt$command, "stratetab")
  testthat::expect_identical(dim(tt$body), c(4L, 10L))
  testthat::expect_identical(tt$body[[1L]], c("Dose tertile", "   Low", "   Medium", "   High"))
  testthat::expect_identical(unname(as.character(unlist(tt$body[3L, c(3L, 6L, 9L)]))), rep("2,210.13", 3L))
  old <- golden_read_cells("S03")
  testthat::expect_identical(dim(old), c(7L, 10L))
  testthat::expect_identical(old[6L, c(3L, 6L, 9L)], rep("2,210.12", 3L))
  current <- old
  current[6L, c(3L, 6L, 9L)] <- "2,210.13"
  testthat::expect_identical(golden_as_cells(tt), current)
  testthat::expect_identical(golden_read_cells_file(file.path(native, "S03.csv")), current)
  # The complete native saved frame retains title + all ten value columns.
  native_frame <- golden_read_cells_file(file.path(native, "S03.frame.csv"))
  expected_frame <- cbind(c(current[1L, 1L], rep("", 6L)), current)
  expected_frame[1L, 2L:11L] <- ""
  expected_frame <- rbind(c("title", paste0("c", seq_len(10L))), expected_frame)
  dimnames(expected_frame) <- NULL
  testthat::expect_identical(native_frame, expected_frame)
  log <- golden_read_lines(file.path(native, "S03.console.txt"))
  returns <- trimws(log[grepl("^[ ]+r\\(", log)])
  return_names <- sub("^r\\(([^)]+)\\).*$", "\\1", returns)
  values <- sub("^r\\([^)]+\\)[ ]*[=:][ ]*", "", returns)
  testthat::expect_identical(return_names, c("N_nopt", "smallcells", "ci_level", "N_outcomes",
    "N_exposures", "N_rows", "markdown_cols", "markdown_rows", "sheet", "xlsx", "frame",
    "methods", "outcome_ids", "markdown", "csv", "rates"))
  testthat::expect_identical(values[1L:8L], c("0", "0", "90", "3", "1", "7", "10", "4"))
  macro <- values[9L:15L]
  testthat::expect_true(all(startsWith(macro, '"') & endsWith(macro, '"')))
  macro <- substring(macro, 2L, nchar(macro) - 1L)
  testthat::expect_identical(macro[c(1L, 3L, 4L, 5L)], c("S03", "s03_round_frame",
    tt$stored$methods, tt$stored$outcome_ids))
  testthat::expect_identical(basename(macro[c(2L, 6L, 7L)]), c("S03.xlsx", "S03.md", "S03.csv"))
  testthat::expect_identical(values[16L], "3 x 3")
  start <- which(log == "S03_rates[3,3]")
  testthat::expect_length(start, 1L)
  rate_lines <- log[seq.int(start + 1L, length(log))]
  rate_lines <- rate_lines[grepl("^[ ]*(Low|Medium|High)[ ]+", rate_lines)]
  tokens <- strsplit(trimws(rate_lines), "[ ]+")
  testthat::expect_identical(vapply(tokens, `[`, "", 1L), c("Low", "Medium", "High"))
  testthat::expect_true(all(lengths(tokens) == 4L))
  native_rates <- do.call(rbind, lapply(tokens, function(x) as.numeric(x[-1L])))
  testthat::expect_identical(dimnames(tt$stored$rates),
    list(c("Low", "Medium", "High"), c("Stroke", "Myocardial_infarction", "Any_event")))
  testthat::expect_equal(unname(tt$stored$rates), native_rates, tolerance = 1e-12)
  for (ext in c("csv", "md")) {
    old_bytes <- golden_bytes(golden_path(paste0("S03.", ext)))
    hit <- gregexpr("2,210.12", rawToChar(old_bytes), fixed = TRUE)[[1L]]
    testthat::expect_identical(length(hit), 3L)
    testthat::expect_true(all(hit > 0L))
    expected <- charToRaw(gsub("2,210.12", "2,210.13", rawToChar(old_bytes), fixed = TRUE))
    testthat::expect_identical(golden_bytes(file.path(native, paste0("S03.", ext))), expected,
      info = paste("authentic full new-native", ext, "only three PY fields changed"))
  }
  # The authenticated native workbook preserves all cell styles and full
  # geometry of the original oracle, with exactly the three published ties.
  old_styles <- golden_cell_styles(golden_path("stratetab.xlsx"), "S03")
  new_styles <- golden_cell_styles(file.path(native, "S03.xlsx"), "S03")
  testthat::expect_identical(old_styles$address, new_styles$address)
  tied <- match(c("D6", "G6", "J6"), old_styles$address)
  testthat::expect_false(anyNA(tied))
  testthat::expect_identical(old_styles$value[tied], rep("2,210.12", 3L))
  testthat::expect_identical(new_styles$value[tied], rep("2,210.13", 3L))
  old_styles$value[tied] <- new_styles$value[tied]
  # Styles are compared by their complete resolved properties; workbook
  # internal style indices need not have matching numeric identifiers.
  old_styles$format_id <- new_styles$format_id <- NULL
  testthat::expect_identical(old_styles, new_styles)
  testthat::expect_identical(golden_sheet_layout(golden_path("stratetab.xlsx"), "S03"),
    golden_sheet_layout(file.path(native, "S03.xlsx"), "S03"))
  out <- withr::local_tempdir(pattern = "tabtools-s03-round-")
  for (ext in c("csv", "md")) {
    path <- file.path(out, paste0("original.", ext))
    if (ext == "csv") tabtools::tt_write_csv(tt, path) else tabtools::tt_write_markdown(tt, path)
    testthat::expect_identical(golden_bytes(path), golden_bytes(file.path(native, paste0("S03.", ext))),
      info = paste("complete original R/current-native", ext, "bytes"))
  }
  original_book <- file.path(out, "original.xlsx")
  tabtools::tt_write_xlsx(tt, original_book, sheet = "S03")
  golden_expect_none(golden_compare_styles(original_book, "S03", file.path(native, "S03.xlsx"), "S03",
    got_width_offset = golden_r_width_offset), "complete original R/current-native S03 styles")
  historical_console <- golden_drop_stratetab_export(golden_read_lines(golden_path("S03_console.txt")), "S03")
  expected_console <- gsub("2,210.12", "2,210.13", golden_console_box(historical_console), fixed = TRUE)
  # The capture log also includes recipe commands and return-list output;
  # compare the complete single published title/box and exact export line.
  published <- which(log == "Three outcomes at 90% confidence")
  testthat::expect_length(published, 1L)
  native_console <- log[seq.int(published, published + length(expected_console) - 1L)]
  testthat::expect_identical(native_console, expected_console)
  testthat::expect_identical(log[published + length(expected_console)], "")
  testthat::expect_identical(log[published + length(expected_console) + 1L],
    paste0("Exported to ", macro[2L], ", sheet S03"))
  testthat::expect_identical(golden_console_box(utils::capture.output(print(tt))), expected_console)

  before <- serialize(tt, NULL)
  peer <- tt
  peer$body[3L, c(3L, 6L, 9L)] <- "2,210.12"
  restored <- peer
  restored$body[3L, c(3L, 6L, 9L)] <- tt$body[3L, c(3L, 6L, 9L)]
  testthat::expect_identical(serialize(restored, NULL), before,
    info = "complete raw metadata/provenance/statistics unchanged by three-string historical peer")
  peer
}
