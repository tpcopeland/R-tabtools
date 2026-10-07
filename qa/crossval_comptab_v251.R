library(testthat)
library(tabtools)

# Helpers are source-only builders. The curated runner must register this file
# plus helper-comptab-v251.R; no native process is launched by installed QA.
source(test_path("helper-comptab-v251.R"), local = TRUE)

test_that("installed composites retain literal counts, model-only geometry and two-outcome forest identity", {
  z <- ct_v251_sources();cases <- ct_v251_cases(z)
  expect_identical(cases$CO001$body[[2L]][2:4], c("3", "5", "8"))
  expect_identical(cases$CO001$body[[3L]][2:4], c("120", "120", "120"))
  expect_identical(cases$CO001$header[[2L]]$text[5:8], c("Crude, IRR (95% CI)", "Crude, p-value", "Adjusted, IRR (95% CI)", "Adjusted, p-value"))
  expect_identical(tail(cases$CO003$body[[1L]], 4L), c("other", "   No", "   Yes", "x"))
  expect_identical(cases$CO003$stored$N_modelonly, 3L)
  expect_identical(cases$CO003$stored$N_modelrows, 6L)
  expect_identical(cases$CO004$meta$frame$outcome_label, c("First outcome", "Second outcome"))
  f <- as_forest_data(cases$CO004)
  expect_identical(unique(f$model_label[f$rowtype == "effect"]), c("First outcome", "Second outcome"))
  # Sample Poisson IRR has independent closed-form point and Wald limits.
  q <- qnorm(.975);v <- exp(log(5 / 3) + c(0, -q, q) * sqrt(1 / 5 + 1 / 3))
  expect_identical(cases$CO001$body[[5L]][3L], sprintf("%.4f (%.4f, %.4f)", v[1L], v[2L], v[3L]))
  expect_identical(cases$CO005$body[[2L]][2L], paste0(sprintf("%.4f", 8 / 3), "***"))
  expect_error(as_forest_data(cases$CO001), class = "tabtools_error_composition")
})

test_that("six authentic pinned native composites match complete publication geometry and stored counts", {
  directory <- ct_v251_authenticate()
  cases <- ct_v251_cases(ct_v251_sources())
  for (id in names(cases)) {
    native_path <- test_path("data", "comptab_v251", paste0(id, ".csv"))
    stored_path <- test_path("data", "comptab_v251", paste0(id, "_stored.csv"))
    expect_true(file.exists(native_path), info = id)
    expect_true(file.exists(stored_path), info = id)
    native <- read.csv(native_path, header = FALSE, colClasses = "character", na.strings = NULL,
      check.names = FALSE, blank.lines.skip = FALSE)
    native[is.na(native)] <- ""
    x <- cases[[id]]
    expected <- rbind(do.call(rbind, lapply(x$header, `[[`, "text")), as.matrix(x$body))
    if (nzchar(x$title)) expected <- rbind(c(x$title, rep("", ncol(x$body) - 1L)), expected)
    expect_identical(dim(native), dim(expected), info = id)
    expect_identical(unname(as.matrix(native)), unname(expected), info = id)
    stored <- read.csv(stored_path, stringsAsFactors = FALSE, blank.lines.skip = FALSE)
    keys <- intersect(c("N_rows", "N_outcomes", "N_sections", "N_modelrows", "N_modelframes", "N_models_per_outcome", "N_modelonly", "N_cols", "N_models", "N_frames", "ci_level"), names(x$stored))
    expect_true(length(keys) >= 5L, info = id)
    for (key in keys) {
      row <- stored[stored$name == key & stored$kind == "scalar", , drop = FALSE]
      expect_identical(nrow(row), 1L, info = paste(id, key))
      expect_equal(as.numeric(row$value), as.numeric(x$stored[[key]]), tolerance = 0, info = paste(id, key))
    }
  }
  native_forest <- read.csv(test_path("data", "comptab_v251", "CO004_forest.csv"), stringsAsFactors = FALSE)
  native_effects <- native_forest[native_forest$rowtype == "effect", , drop = FALSE]
  r_forest <- as_forest_data(cases$CO004)
  r_effects <- r_forest[r_forest$rowtype == "effect", , drop = FALSE]
  expect_identical(nrow(native_effects), 4L)
  expect_identical(nrow(r_effects), 4L)
  expect_identical(as.integer(native_effects$model), r_effects$model)
  expect_identical(native_effects$model_label, r_effects$model_label)
  for (field in c("estimate", "ll", "ul", "pvalue")) {
    expect_equal(r_effects[[field]], native_effects[[field]], tolerance = 1e-6, info = field)
  }
})

test_that("installed composite sinks and unkeyed exports preserve text and ledger boundaries", {
  z <- ct_v251_sources()
  directory <- withr::local_tempdir(pattern = "tabtools-wp5d-sinks-")
  files <- file.path(directory, c("composite.xlsx", "composite.csv", "composite.md"))
  out <- hrcomptab(z$rate, z$models, rows = 2:4, allmodels = TRUE, keyed = TRUE,
    cformat = "%8.4f", cisep = ' "to" $ ` \\ ', footnote = c("First paragraph", "Second paragraph"),
    xlsx = files[1L], csv = files[2L], markdown = files[3L], sheet = "Two models")
  expect_true(all(file.exists(files)))
  csv <- read.csv(files[2L], header = FALSE, colClasses = "character", na.strings = NULL, blank.lines.skip = FALSE)
  expect_identical(as.character(csv[5L, 5L]), out$body[[5L]][3L])
  sheet <- openxlsx::read.xlsx(files[1L], sheet = "Two models", colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  expect_identical(as.character(sheet[5L, 6L]), out$body[[5L]][3L])
  md <- readLines(files[3L], warn = FALSE)
  expect_true(any(grepl("First paragraph", md, fixed = TRUE)))
  expect_true(any(grepl("Second paragraph", md, fixed = TRUE)))
  flat <- tt_flat(out, keyed = FALSE)
  expect_identical(attr(flat, "header"), out$header)
  expect_identical(attr(flat, "command"), "hrcomptab")
  expect_identical(attr(flat, "frame"), out$meta$frame)
  expect_identical(attr(flat, "sample_accounting"), out$meta$sample_accounting)
  expect_identical(attr(flat, "composition_export"), TRUE)
  expect_error(comptab(flat, rows = 1L), class = "tabtools_error_composition")
  expect_error(tt_flat(out), class = "tabtools_error_flat")
  bad <- z$models;bad$meta$regtab_rows$estimate[1L] <- 999
  target <- file.path(directory, "refused.csv")
  expect_error(hrcomptab(z$rate, bad, rows = 2:4, allmodels = TRUE, cformat = "%8.4f", csv = target), class = "tabtools_error_composition")
  expect_false(file.exists(target))
})

test_that("all four installed converters consume variable composite spans without invented keys", {
  z <- ct_v251_sources()
  x <- hrcomptab(z$rate, z$models, rows = 2:4, allmodels = TRUE, keyed = TRUE)
  expected <- x$body[[5L]][3L]
  gt <- tt_as_gt(x)
  expect_identical(gt$`_data`[[5L]][3L], expected)
  expect_match(gt::as_raw_html(gt), "Crude, IRR (95% CI)", fixed = TRUE)
  ft <- flextable::as_flextable(x)
  chunks <- flextable::information_data_chunk(ft)
  expect_true(expected %in% chunks$txt[chunks$.part == "body"])
  expect_true("Crude, p-value" %in% chunks$txt[chunks$.part == "header"])
  gs <- tt_as_gtsummary(x)
  expect_identical(gs$table_body$col_5[3L], expected)
  expect_identical(nrow(gs$table_body), 4L)
  tiny <- tt_as_tinytable(x)
  text <- capture.output(print(tiny, output = "markdown"))
  expect_true(any(grepl(expected, text, fixed = TRUE)))
  expect_true(any(grepl("Adjusted, p-value", text, fixed = TRUE)))
})


test_that("ordinary composite wrappers consume inherited sinks once and explicit NULL opts out", {
  directory <- withr::local_tempdir(pattern = "tabtools-wp5d-session-")
  keys <- getFromNamespace(".tt_option_keys", "tabtools")
  option_names <- paste0("tabtools.", keys)
  withr::local_options(stats::setNames(lapply(option_names, getOption), option_names))
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  saved <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(saved, envir = state)
  })
  tabtools_options(clear = TRUE)
  z <- ct_v251_sources()
  book <- file.path(directory, "session.xlsx");md <- file.path(directory, "session.md")
  tt_write_xlsx(puttab(data.frame(value = "unrelated")), book, sheet = "Keep")
  tabtools_options(workbook = book, markdown = md, headershade = TRUE)
  x <- suppressMessages(comptab(z$stars, rows = 3:4, sheet = "Vertical"))
  y <- suppressMessages(hrcomptab(z$rate, z$models, rows = 2:4, allmodels = TRUE, keyed = TRUE, sheet = "Rate"))
  expect_identical(openxlsx::getSheetNames(book), c("Keep", "Vertical", "Rate"))
  expect_identical(x$stored$sheet, "Vertical")
  expect_identical(y$stored$sheet, "Rate")
  # Session headershade is a puttab capability; ordinary composites use
  # their command default unless the caller explicitly selects shading.
  expect_false(x$style$headershade)
  expect_false(y$style$headershade)
  expect_true(comptab(z$stars, rows = 3:4, headershade = TRUE,
    xlsx = NULL, markdown = NULL)$style$headershade)
  expect_true(hrcomptab(z$rate, z$models, rows = 2:4, allmodels = TRUE, keyed = TRUE,
    headershade = TRUE, xlsx = NULL, markdown = NULL)$style$headershade)
  before <- readBin(md, "raw", n = file.info(md)$size)
  expect_warning(suppressMessages(comptab(z$stars, rows = 3:4, sheet = "Disabled", xlsx = NULL, markdown = NULL)),
    class = "tabtools_warning_sheet_without_workbook")
  expect_identical(openxlsx::getSheetNames(book), c("Keep", "Vertical", "Rate"))
  expect_identical(readBin(md, "raw", n = file.info(md)$size), before)
})

test_that("installed canonical references distinguish custom text from equal real IRRs", {
  d <- data.frame(g = factor(rep(c("A", "B", "C"), each = 12L)), time = 10)
  d$ev <- as.numeric(rep(1:12, 3L) <= rep(c(6L, 6L, 9L), each = 12L))
  fit <- glm(ev ~ g, poisson(), d)
  model <- regtab(fit, coef = "IRR", nointercept = TRUE, refcat = "1.00")
  rate <- stratetab(tt_rates(d, "time", "ev", by = "g"), ratescale = 1)
  before <- serialize(list(model, rate), NULL)
  x <- hrcomptab(rate, model, rows = 2:4, keyed = TRUE)
  b <- match("   B", x$body[[1L]])
  expect_identical(model$body[[2L]][2:3], c("1.00", "1.00"))
  expect_match(x$body[[5L]][b], "1.00 (", fixed = TRUE)
  expect_identical(x$meta$ref_rows, match("   A", x$body[[1L]]))
  imported <- as.data.frame(model)
  expect_identical(hrcomptab(rate, imported, rows = 2:4, keyed = TRUE)$body, x$body)
  stamp <- attr(imported, "composition", exact = TRUE)
  stamp$companion$status[stamp$companion$row == 3L] <- "ref"
  attr(imported, "composition") <- stamp
  expect_error(hrcomptab(rate, imported, rows = 2:4, keyed = TRUE),
    class = "tabtools_error_composition")
  expect_identical(serialize(list(model, rate), NULL), before)
})


test_that("authenticated six-case native composite sinks match complete publication surfaces", {
  directory <- ct_v251_authenticate()
  source(test_path("..", "tests", "testthat", "helper-golden.R"), local = TRUE)
  cases <- ct_v251_cases(ct_v251_sources())
  scratch <- withr::local_tempdir(pattern = "tabtools-native-composite-sinks-")
  book <- file.path(scratch, "r-composites.xlsx")
  native_book <- file.path(directory, "comptab.xlsx")
  expect_identical(openxlsx::getSheetNames(native_book), names(cases))
  for (id in names(cases)) {
    x <- cases[[id]]
    csv <- file.path(scratch, paste0(id, ".csv"))
    md <- file.path(scratch, paste0(id, ".md"))
    tt_write_csv(x, csv)
    mr <- tt_write_markdown(x, md)
    tt_write_xlsx(x, book, sheet = id)
    expect_identical(golden_compare_sink(csv, file.path(directory, paste0(id, ".csv"))), character(), info = id)
    expect_identical(golden_compare_sink(md, file.path(directory, paste0(id, ".md"))), character(), info = id)
    expect_identical(ct_v251_console_box(capture.output(print(x))),
      ct_v251_console_box(readLines(file.path(directory, paste0(id, ".console.txt")), warn = FALSE)), info = id)
    expect_identical(golden_compare_styles(book, id, native_book, id,
      got_width_offset = golden_r_width_offset, mask = character()), character(), info = id)
    # No fixture carries a publication footer. Full sink byte/style comparisons
    # above include every row: no footer, numeric, state or style mask applies.
    expect_identical(x$footnote, "", info = id)
    ret <- read.csv(file.path(directory, paste0(id, "_stored.csv")), colClasses = "character")
    expect_identical(names(ret), c("name", "kind", "row", "col", "value"), info = id)
    meta <- ret[ret$kind == "meta", , drop = FALSE]
    expect_identical(stats::setNames(meta$value, meta$name),
      c(`_golden_id` = id, `_golden_tabtools_version` = "2.5.1",
        `_golden_fvgen_version` = "not_used", `_golden_stata_version` = "17"), info = id)
    expected <- c(x$stored, list(markdown_rows = attr(mr, "n_rows"), markdown_cols = attr(mr, "n_cols")))
    scalars <- ret[ret$kind == "scalar", , drop = FALSE]
    expect_false(anyDuplicated(scalars$name) > 0L, info = id)
    expect_setequal(scalars$name, names(expected)[vapply(expected, function(v) is.numeric(v) && length(v) == 1L, TRUE)])
    for (key in scalars$name) expect_equal(as.numeric(scalars$value[scalars$name == key]),
      as.numeric(expected[[key]]), tolerance = 0, info = paste(id, key))
    macros <- ret[ret$kind == "macro", , drop = FALSE]
    expect_false(anyDuplicated(macros$name) > 0L, info = id)
    if (id %in% c("CO005", "CO006")) {
      expect_setequal(macros$name, c("methods", "sheet", "xlsx", "markdown"))
      expect_identical(macros$value[macros$name == "methods"], x$stored$methods, info = id)
    } else {
      fields <- c("sheet", "xlsx", "markdown", "csv", "effect", "modelframes", "rateframe")
      if (id == "CO004") fields <- c(fields, "eplotframe")
      expect_setequal(macros$name, fields)
      # Native opaque frame handles are checked against the actual declared
      # recipe, separately from R source identity (no invented R frame handle).
      handles <- list(CO001 = c(modelframes = "_co_models", rateframe = "_co_rate"),
        CO002 = c(modelframes = "_co_models", rateframe = "_co_rateonly"),
        CO003 = c(modelframes = "_co_appendix", rateframe = "_co_rate"),
        CO004 = c(modelframes = "_co_paired", rateframe = "_co_tworates", eplotframe = "_co_forest"))[[id]]
      expect_identical(stats::setNames(macros$value, macros$name)[names(handles)], handles, info = id)
      expect_identical(macros$value[macros$name == "effect"], "IRR", info = id)
      expect_identical(basename(macros$value[macros$name == "csv"]), paste0(id, ".csv"), info = id)
    }
    expect_identical(macros$value[macros$name == "sheet"], id, info = id)
    expect_identical(basename(macros$value[macros$name == "xlsx"]), "comptab.xlsx", info = id)
    expect_identical(basename(macros$value[macros$name == "markdown"]), paste0(id, ".md"), info = id)
    expect_setequal(unique(ret$kind), c("meta", "scalar", "macro"))
  }
  expect_identical(openxlsx::getSheetNames(book), names(cases))
})

test_that("missing or modified authentic composite controls cannot pass", {
  directory <- ct_v251_authenticate()
  scratch <- withr::local_tempdir(pattern = "tabtools-native-composite-negative-")
  files <- list.files(directory, full.names = TRUE)
  expect_true(all(file.copy(files, scratch)))
  unlink(file.path(scratch, "CO001.csv"))
  expect_error(ct_v251_authenticate(scratch), "changed or missing")
  expect_true(file.copy(file.path(directory, "CO001.csv"), scratch))
  cat("mutant", file = file.path(scratch, "CO004_forest.csv"), append = TRUE)
  expect_error(ct_v251_authenticate(scratch), "changed or missing")
})
