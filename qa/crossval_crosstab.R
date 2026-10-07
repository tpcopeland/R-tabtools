library(testthat)
library(tabtools)

test_that("installed crosstab reproduces independent arithmetic and four converters", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL,
                           tabtools.smallcells = NULL, tabtools.smallcells_mode = NULL,
                           tabtools.masktext = NULL, tabtools.boldp = NULL))
  d <- xt_qa_data(matrix(c(40, 10, 20, 30), 2))
  x <- crosstab(d, "row", "column", weights = "frequency", or = TRUE, rr = TRUE, rd = TRUE)
  expect_equal(c(x$stored$N, x$stored$chi2, x$stored$or, x$stored$rr, x$stored$rd), c(100, 50 / 3, 6, 3, .4))
  sparse <- crosstab(xt_qa_data(matrix(c(1, 3, 3, 1), 2)), "row", "column", weights = "frequency", or = TRUE)
  expect_equal(c(sparse$stored$p, sparse$stored$or), c(17 / 35, 1 / 9))
  trend <- crosstab(xt_qa_data(rbind(c(10, 20, 30), c(30, 20, 10)), columns = 0:2),
                    "row", "column", weights = "frequency", cochran = TRUE)
  expect_equal(c(trend$stored$z_trend, trend$stored$chi2_trend), c(-sqrt(20), 20))
  protected <- crosstab(xt_qa_data(matrix(c(1, 9, 9, 1), 2)), "row", "column",
                        weights = "frequency", smallcells = 3, footnote = c("one", "two"))
  for (pkg in c("gt", "flextable", "gtsummary", "tinytable")) {
    expect_true(requireNamespace(pkg, quietly = TRUE), info = pkg)
  }
  g <- tt_as_gt(protected); ft <- flextable::as_flextable(protected)
  gs <- tt_as_gtsummary(protected); tiny <- tt_as_tinytable(protected)
  expect_s3_class(g, "gt_tbl"); expect_s3_class(ft, "flextable")
  expect_s3_class(gs, "gtsummary"); expect_s4_class(tiny, "tinytable")
  expect_true(any(g$`_data`$c1 == "Pearson's chi-squared test: Suppressed"))
  expect_false(any(grepl("90.0%", as.matrix(g$`_data`), fixed = TRUE)))
  expect_true(all(is.na(attr(as.data.frame(protected), "sample_accounting")$measures$value[
    attr(as.data.frame(protected), "sample_accounting")$measures$metric != "reported_n"])))
})

test_that("native reader rejects incomplete coordinates, missing-tag swaps and scalar families", {
  records <- data.frame(row = c("1", "1", "2", "2"), col = c("1", "2", "1", "2"),
                        value = c("40", "20", "10", "30"))
  expect_identical(xt_qa_matrix(records, c("1", "2"), c("1", "2")), matrix(c(40, 10, 20, 30), 2))
  for (mutant in list(records[c(1, 2, 3, 2), ], records[-4, ], rbind(records, records[1, ]))) {
    expect_error(xt_qa_matrix(mutant, c("1", "2"), c("1", "2")), class = "crosstab_native_contract_error")
  }
  # The same Cartesian contract is applied to suppression and table matrices.
  suppression <- records; suppression$value <- c("1", "2", "2", "1")
  expect_identical(xt_qa_matrix(suppression, c("1", "2"), c("1", "2")), matrix(c(1, 2, 2, 1), 2))
  expect_error(xt_qa_matrix(suppression[c(1, 2, 3, 3), ], c("1", "2"), c("1", "2")),
               class = "crosstab_native_contract_error")
  tagged <- records; tagged$row <- c(".", ".", ".a", ".a")
  expect_identical(xt_qa_matrix(tagged, c(".", ".a"), c("1", "2")), matrix(c(40, 10, 20, 30), 2))
  swapped <- tagged; swapped$row <- ifelse(swapped$row == ".", ".a", ".")
  expect_error(xt_qa_matrix(swapped, c(".", ".a"), c("1", "2")), class = "crosstab_native_contract_error")
  swapped$row <- rep(".", 4)
  expect_error(xt_qa_matrix(swapped, c(".", ".a"), c("1", "2")), class = "crosstab_native_contract_error")
  keys <- xt_qa_scalar_keys(list(or = TRUE, cochran = TRUE, smallcells = 3))
  scalars <- data.frame(name = rev(keys), value = rep("1", length(keys)))
  expect_identical(xt_qa_unique_keys(scalars, keys)$name, keys)
  expect_error(xt_qa_unique_keys(scalars[scalars$name != "or", ], keys), class = "crosstab_native_contract_error")
  expect_error(xt_qa_unique_keys(rbind(scalars, scalars[1, ]), keys), class = "crosstab_native_contract_error")
  for (mode in c("strict", "primary")) {
    foot <- xt_qa_footer_literals(list(smallcells = 3, smallcells_mode = mode))
    expect_true(xt_qa_footer_tail(foot$R, foot$R))
    expect_true(xt_qa_footer_tail(foot$native, foot$native))
    for (mutant in list(character(), substr(foot$native, 1, 40), c(foot$native, "invented"), foot$R)) {
      expect_error(xt_qa_footer_tail(mutant, foot$native), class = "crosstab_native_contract_error")
    }
  }
})

test_that("authenticated native XT cases compare complete stored and publication surfaces", {
  qa_source_root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
  if (!nzchar(qa_source_root)) qa_source_root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
  qa_source_root <- normalizePath(qa_source_root, mustWork = TRUE)
  if (!file.exists(file.path(qa_source_root,"DESCRIPTION"))) stop("Cannot locate the staged QA package root")
  native <- Sys.getenv("CROSSTAB_NATIVE_DIR", unset = file.path(qa_source_root,"qa","data","native251","crosstab"))
  xt_qa_guard(nzchar(native) && dir.exists(native), "CROSSTAB_NATIVE_DIR must contain reviewed authentic XT artifacts")
  native <- normalizePath(native, mustWork = TRUE)
  xt_qa_authenticate(native)
  cases <- xt_qa_cases()
  status <- do.call(rbind, lapply(names(cases), function(id) {
    row <- utils::read.csv(file.path(native, paste0(id, "_status.csv")), colClasses = "character",
      na.strings = NULL, check.names = FALSE)
    xt_qa_guard(identical(names(row), c("id", "rc", "nrow", "ncol", "pin")) && nrow(row) == 1L &&
      identical(row$id, id) && identical(row$pin, xt_qa_native_pin), "Native status identity differs")
    row
  }))
  expect_identical(status$id, sprintf("XT%03d", 1:28))
  refusal <- c(XT009 = 498L, XT015 = 498L, XT016 = 498L, XT019 = 198L, XT028 = 198L)
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL,
    tabtools.smallcells = NULL, tabtools.smallcells_mode = NULL, tabtools.masktext = NULL,
    tabtools.digits = NULL, tabtools.boldp = NULL))
  directory <- withr::local_tempdir(pattern = "crosstab-crossval-")
  helper <- xt_qa_golden_helpers(native)
  completed <- compared <- character()
  for (id in names(cases)) {
    case <- cases[[id]]
    args <- c(list(data = case$data, rowvar = "row", colvar = "column", weights = "frequency"), case$options)
    observed <- status[status$id == id, , drop = FALSE]
    actual_rc <- as.integer(observed$rc)
    expect_identical(actual_rc, if (id %in% names(refusal)) unname(refusal[id]) else 0L, info = id)
    if (id %in% names(refusal)) {
      expect_identical(c(observed$nrow, observed$ncol), c(".", "."), info = id)
      expect_false(any(file.exists(file.path(native, paste0(id, c(".csv", ".md", ".xlsx", "_stored.csv"))))), info = id)
      expect_error(do.call(crosstab, args), class = case$error)
    } else {
      args$sheet <- id
      args$csv <- file.path(directory, paste0(id, ".csv"))
      args$markdown <- file.path(directory, paste0(id, ".md"))
      args$xlsx <- file.path(directory, paste0(id, ".xlsx"))
      r <- do.call(crosstab, args)
      axes <- xt_qa_axes(case, id)
      expect_identical(as.integer(c(observed$nrow, observed$ncol)), dim(r$stored$table), info = id)
      expect_equal(r$meta$axes$row$code, xt_qa_number(axes$row), tolerance = 0, info = id)
      expect_equal(r$meta$axes$column$code, xt_qa_number(axes$column), tolerance = 0, info = id)
      expect_identical(r$meta$axes$row$missing_tag,
        if (id == "XT017") c(NA_character_, NA_character_, "", "a") else rep(NA_character_, length(axes$row)), info = id)
      expect_identical(r$meta$axes$column$missing_tag, rep(NA_character_, length(axes$column)), info = id)
      stored <- utils::read.csv(file.path(native, paste0(id, "_stored.csv")), colClasses = "character",
        check.names = FALSE, na.strings = NULL)
      meta <- stored[stored$kind == "meta", , drop = FALSE]
      meta <- xt_qa_unique_keys(meta, c("_golden_id", "_golden_tabtools_version", "_golden_fvgen_version", "_golden_stata_version"))
      expect_identical(meta$value, c(id, "2.5.1", "", "17"), info = id)
      scalars <- xt_qa_unique_keys(stored[stored$kind == "scalar", , drop = FALSE], xt_qa_scalar_keys(case$options))
      derived <- isTRUE(r$meta$publication$inference_masked)
      for (key in scalars$name) {
        expected <- switch(key, markdown_cols = ncol(r$body), markdown_rows = nrow(r$body),
          smallcells = r$stored$smallcells$threshold, r$stored[[key]])
        literal <- scalars$value[scalars$name == key]
        if (is.na(expected)) {
          expect_identical(literal, if (key == "N" && id == "XT024") ".p" else if (derived && key %in% c("p", "chi2", "or", "rr", "rd", "p_trend", "chi2_trend", "z_trend")) ".d" else ".", info = paste(id, key, "missing identity"))
        }
        expect_equal(expected, xt_qa_number(literal), tolerance = 1e-7, info = paste(id, key))
      }
      matrices <- stored[stored$kind == "matrix", , drop = FALSE]
      expect_true(setequal(unique(matrices$name), c("table", if (!is.null(case$options$smallcells)) "suppression")), info = id)
      counts <- matrices[matrices$name == "table", , drop = FALSE]
      expect_equal(unname(r$stored$table), xt_qa_matrix(counts, axes$row, axes$column), tolerance = 0, info = id)
      if (!is.null(case$options$smallcells)) {
        masks <- matrices[matrices$name == "suppression", , drop = FALSE]
        native_mask <- xt_qa_matrix(masks, paste0("r", seq_along(axes$row)), paste0("c", seq_along(axes$column)))
        expect_equal(unname(r$stored$suppression), native_mask, tolerance = 0, info = id)
        pos <- cbind(match(counts$row, axes$row), match(counts$col, axes$column))
        code <- native_mask[pos]
        expect_identical(counts$value[code == 1], rep(".p", sum(code == 1)), info = id)
        expect_identical(counts$value[code == 2], rep(".s", sum(code == 2)), info = id)
        expect_identical(r$stored$smallcells_mode, stored$value[stored$name == "smallcells_mode" & stored$kind == "macro"], info = id)
      }
      want <- c(if (isTRUE(case$options$or)) c("or", "or_lo", "or_hi"),
        if (isTRUE(case$options$rr)) c("rr", "rr_lo", "rr_hi"), if (isTRUE(case$options$rd)) c("rd", "rd_lo", "rd_hi"))
      if (length(want) && !derived) {
        association <- utils::read.csv(file.path(native, paste0(id, "_association.csv")),
          colClasses = "character", na.strings = NULL)
        association <- xt_qa_unique_keys(association, want)
        for (key in want) expect_equal(r$stored[[key]], xt_qa_number(association$value[association$name == key]),
          tolerance = if (grepl("or_", key)) 3e-4 else 1e-7, info = paste(id, key))
      } else if (derived) for (key in want) expect_identical(r$stored[[key]], NA_real_, info = paste(id, key))
      expect_identical(r$meta$native_source$pin, xt_qa_native_pin, info = id)
      expect_match(r$stored$methods, paste0("Analysis performed in R ", getRversion(), "."), fixed = TRUE, info = id)
      expect_false(grepl("Analysis performed in Stata", r$stored$methods, fixed = TRUE), info = id)
      native_methods <- stored$value[stored$kind == "macro" & stored$name == "methods"]
      expect_length(native_methods, 1L)
      expect_true(endsWith(native_methods, "Analysis performed in Stata 17 (StataCorp, College Station, TX)."), info = id)
      for (ext in c("csv", "md")) {
        why <- helper$golden_publication_sink(args[[if (ext == "md") "markdown" else "csv"]], id, ext, character(), r)
        expect_identical(why, character(), info = paste(id, ext))
      }
      parts <- helper$golden_publication_console(utils::capture.output(print(r)),
        xt_qa_native_listing(file.path(native, paste0(id, "_console.txt"))), id)
      expect_identical(helper$golden_compare_console(parts$got, parts$want), character(), info = paste(id, "console"))
      expect_identical(tidyxl::xlsx_sheet_names(args$xlsx), id, info = id)
      expect_identical(tidyxl::xlsx_sheet_names(helper$golden_book(id)), id, info = id)
      expect_identical(helper$golden_compare_styles(args$xlsx, id, helper$golden_book(id), id,
        got_width_offset = helper$golden_r_width_offset, publication_id = id), character(), info = paste(id, "full workbook"))
      if (id == "XT018") {
        literal <- xt_qa_xt018_literal
        expect_identical(actual_rc, literal$rc)
        expect_identical(dim(r$stored$table), literal$dimensions)
        expect_equal(r$stored$N, literal$N, tolerance = 0)
        expect_equal(unname(r$stored$table), literal$table, tolerance = 0)
        expect_identical(axes, literal[c("row", "column")])
      }
      compared <- c(compared, id)
    }
    completed <- c(completed, id)
  }
  expect_identical(completed, sprintf("XT%03d", 1:28))
  expect_identical(compared, setdiff(names(cases), names(refusal)))
  expect_length(compared, 23L)
})


test_that("native publication comparators detect actual copied-body mutations", {
  qa_source_root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
  if (!nzchar(qa_source_root)) qa_source_root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
  qa_source_root <- normalizePath(qa_source_root, mustWork = TRUE)
  if (!file.exists(file.path(qa_source_root,"DESCRIPTION"))) stop("Cannot locate the staged QA package root")
  native <- Sys.getenv("CROSSTAB_NATIVE_DIR", unset = file.path(qa_source_root,"qa","data","native251","crosstab"))
  xt_qa_guard(nzchar(native) && dir.exists(native), "CROSSTAB_NATIVE_DIR is required")
  native <- normalizePath(native, mustWork = TRUE)
  xt_qa_authenticate(native)
  helper <- xt_qa_golden_helpers(native)
  directory <- withr::local_tempdir(pattern = "crosstab-surface-mutants-")
  for (ext in c("csv", "md")) {
    original <- file.path(native, paste0("XT001.", ext))
    expect_length(helper$golden_compare_sink(original, original), 0L)
    lines <- helper$golden_read_lines(original)
    at <- if (ext == "csv") 2L else 3L
    lines[at] <- paste0("FAULT", lines[at])
    mutant <- file.path(directory, paste0("body.", ext))
    helper$golden_write_lf(lines, mutant)
    expect_gt(length(helper$golden_compare_sink(mutant, original)), 0L)
  }
  listing <- xt_qa_native_listing(file.path(native, "XT001_console.txt"))
  expect_length(helper$golden_compare_console(listing, listing), 0L)
  mutant <- listing
  expect_true(grepl("row", mutant[2], fixed = TRUE))
  mutant[2] <- sub("row", "FAULT", mutant[2], fixed = TRUE)
  expect_gt(length(helper$golden_compare_console(mutant, listing)), 0L)
  original <- helper$golden_book("XT027")
  expect_length(helper$golden_compare_styles(original, "XT027", original, "XT027",
    got_width_offset = helper$golden_width_offset), 0L)
  cells <- helper$golden_cell_styles(original, "XT027")
  cell <- cells[cells$address == "C3", , drop = FALSE]
  expect_identical(nrow(cell), 1L)
  border <- function(x) if (is.na(x) || !nzchar(x)) "none" else x
  workbook <- openxlsx2::wb_load(original)
  workbook <- openxlsx2::wb_add_border(workbook, sheet = "XT027", dims = "C3",
    bottom_border = border(cell$border_bottom), left_border = border(cell$border_left),
    right_border = border(cell$border_right),
    top_border = if (identical(cell$border_top, "double")) "medium" else "double", update = TRUE)
  mutant <- file.path(directory, "body-style.xlsx")
  openxlsx2::wb_save(workbook, mutant, overwrite = FALSE)
  expect_identical(tidyxl::xlsx_sheet_names(mutant), "XT027")
  why <- helper$golden_compare_styles(mutant, "XT027", original, "XT027",
    got_width_offset = helper$golden_width_offset)
  expect_true(any(grepl("C3 border_top:", why, fixed = TRUE)))
})
