library(testthat)
library(tabtools)

test_that("installed corrtab export and rendered help describe actual inference", {
  expect_true("corrtab" %in% getNamespaceExports("tabtools"))
  h <- utils::help("corrtab", package = "tabtools")
  expect_length(h, 1L)
  rd <- utils:::.getHelpFile(h)
  rendered <- paste(capture.output(tools::Rd2txt(rd)), collapse = "\n")
  expect_match(rendered, "AS89", fixed = TRUE)
  expect_match(rendered, "pair", fixed = TRUE)
  d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
  tt <- corrtab(d, c("x", "y"), pvalues = TRUE)
  expect_identical(tt$body$c2[2], "0.80 (0.200)")
})

test_that("installed ordinary rank inference agrees with independent cor.test", {
  d <- data.frame(x = c(1, 1, 2, 3), y = c(1, 2, 2, 3))
  for (ranked in c(FALSE, TRUE)) {
    t <- corrtab(d, names(d), spearman = ranked)
    oracle <- stats::cor.test(d$x, d$y, method = if (ranked) "spearman" else "pearson", exact = FALSE)
    expect_equal(t$stored$C[2, 1], unname(oracle$estimate), tolerance = 1e-13)
    expect_equal(t$stored$P[2, 1], oracle$p.value, tolerance = 1e-13)
  }
  # R returns p=0 for perfect negative Spearman; that is not our Stata17 target.
  t <- corrtab(data.frame(x = 1:3, y = 3:1), c("x", "y"), spearman = TRUE)
  expect_true(is.na(t$stored$P[2, 1]))
})

test_that("installed sinks share body text and preserve unrelated worksheet", {
  work <- withr::local_tempdir(pattern = "corrtab-installed-sinks-")
  xlsx <- file.path(work, "out.xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("Keep")$add_data("Keep", "retained")
  wb$save(xlsx)
  d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
  tt <- corrtab(d, names(d), pvalues = TRUE, title = "Correlation QA",
    footnote = c("First paragraph", "Second paragraph"), xlsx = xlsx,
    sheet = "QA", csv = file.path(work, "out.csv"), markdown = file.path(work, "out.md"),
    headershade = TRUE, zebra = TRUE)
  expect_identical(openxlsx2::wb_to_df(xlsx, "Keep", col_names = FALSE)[1, 1], "retained")
  expect_identical(openxlsx2::wb_to_df(xlsx, "QA", rows = 4, cols = 3, col_names = FALSE)[1, 1], "0.80 (0.200)")
  for (path in c(tt$stored$csv, tt$stored$markdown)) {
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(txt, "0.80 (0.200)", fixed = TRUE)
    expect_match(txt, "First paragraph", fixed = TRUE)
    expect_match(txt, "Second paragraph", fixed = TRUE)
  }
  expect_false(any(c("model", "fit") %in% names(tt$meta$frame)))
})

test_that("installed session resolution uses one outer call and native alias priority", {
  work <- withr::local_tempdir(pattern = "corrtab-installed-session-")
  old <- tabtools_options()
  withr::defer({tabtools_options(clear = TRUE); if (length(old)) do.call(tabtools_options, old)})
  session_book <- file.path(work, "session.xlsx")
  session_md <- file.path(work, "session.md")
  tabtools_options(workbook = session_book, markdown = session_md, headershade = TRUE,
                   digits = 3L, smallcells = 100L)
  d <- data.frame(x = 1:4, y = c(1, 2, 4, 3))
  t <- corrtab(d, names(d))
  expect_false(file.exists(session_book))
  expect_false(t$style$headershade)
  expect_identical(t$body$c2[2], "0.800")
  t <- corrtab(d, names(d), sheet = "Inherited")
  expect_true(file.exists(session_book))
  expect_true(file.exists(session_md))
  selected <- file.path(work, "selected.xlsx")
  ignored <- file.path(work, "ignored.xlsx")
  corrtab(d, names(d), xlsx = selected, excel = ignored, markdown = NULL)
  expect_true(file.exists(selected))
  expect_false(file.exists(ignored))
  suppressWarnings(corrtab(d, names(d), sheet = "Disabled", xlsx = NULL, markdown = NULL))
  expect_false("Disabled" %in% openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(session_book)))
})

test_that("installed public presentation converters retain coefficient and note text", {
  t <- corrtab(data.frame(x = 1:4, y = c(1, 2, 4, 3)), c("x", "y"),
    pvalues = TRUE, footnote = c("First paragraph", "Second paragraph"))
  if (requireNamespace("gt", quietly = TRUE)) {
    html <- as.character(gt::as_raw_html(tt_as_gt(t)))
    expect_match(html, "0.80 (0.200)", fixed = TRUE)
    expect_match(html, "First paragraph", fixed = TRUE)
    expect_match(html, "Second paragraph", fixed = TRUE)
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::as_flextable(t)
    expect_true(any(as.matrix(ft$body$dataset) == "0.80 (0.200)"))
    expect_true(any(grepl("Second paragraph", as.matrix(ft$footer$dataset), fixed = TRUE)))
  }
  expect_true(requireNamespace("tinytable", quietly = TRUE))
  expect_true(requireNamespace("gtsummary", quietly = TRUE))
  tiny <- tt_as_tinytable(t); gs <- tt_as_gtsummary(t)
  expect_s4_class(tiny, "tinytable"); expect_s3_class(gs, "gtsummary")
  expect_true(any(as.matrix(methods::slot(tiny, "data")) == "0.80 (0.200)"))
  expect_true(any(grepl("Second paragraph", unlist(methods::slot(tiny, "notes")), fixed = TRUE)))
  expect_true(any(as.matrix(gs$table_body) == "0.80 (0.200)"))
  expect_true(any(grepl("Second paragraph", gs$table_styling$source_note$source_note, fixed = TRUE)))
})

test_that("installed unkeyed flattening retains publication and refuses invented keys", {
  t <- corrtab(data.frame(x = 1:4, y = c(1, 2, 4, 3)), c("x", "y"), pvalues = TRUE)
  f <- tt_flat(t, keyed = FALSE)
  expect_s3_class(f, "data.frame")
  expect_identical(unname(as.matrix(f)), unname(as.matrix(t$body)))
  expect_identical(names(f), names(t$body))
  expect_identical(attr(f, "header", exact = TRUE), t$header)
  expect_identical(attr(f, "command", exact = TRUE), "corrtab")
  expect_identical(attr(f, "frame", exact = TRUE), t$meta$frame)
  expect_identical(attr(f, "sample_accounting", exact = TRUE), t$meta$sample_accounting)
  expect_identical(attr(f, "composition_export", exact = TRUE), TRUE)
  expect_false(any(grepl("^_state_|^_model_|^_term$", names(f))))
  expect_error(tt_flat(t), class = "tabtools_error_flat")
})
