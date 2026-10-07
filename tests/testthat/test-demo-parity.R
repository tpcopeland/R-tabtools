# The R demo (qa/demo/demo_tabtools.R) against the Stata demo
# (IMPLEMENTATION_PLAN.md Milestone D, task D3): every ported sheet under
# the golden comparators (cells, xlsx styles, merges, widths, heights), the
# console log and its Markdown rendering block by block (console
# comparator), and the Markdown report (sink comparator: bytes when
# unmasked). golden/demo/manifest.csv lists every Stata artefact; the test
# fails when a listed sheet is missing or an unlisted one appears.
# Regenerate the Stata side with `Rscript qa/demo_parity.R --update`.

skip_on_cran()
for (pkg in golden_demo_packages) skip_if_not_installed(pkg)
# The demo (qa/demo/) and golden/demo are .Rbuildignored: skip under R CMD
# check of the tarball; in a source checkout a missing file is an error.
skip_if(!golden_in_source() &&
          !(file.exists(golden_demo_path("manifest.csv")) && file.exists(golden_demo_script("demo_tabtools.R"))),
        "the demo and golden/demo are not in the package build (source checkout only)")

manifest <- golden_demo_manifest()
source_info <- golden_demo_source()
run <- golden_demo_run()

test_that("the demo runs and writes every ported workbook, the console log and the report", {
  books <- unique(manifest$artefact[manifest$kind == "sheet" & manifest$status == "compared"])
  for (f in c(books, "console_output.log", "console_output.md", "demo_markdown_report.md")) {
    expect_true(file.exists(file.path(run$out_dir, f)), label = f)
  }
  # Nothing else: no workbook of a command the manifest lists as not ported,
  # and the pipeline's scratch workbook is removed.
  expect_setequal(list.files(run$out_dir),
                  c(books, "console_output.log", "console_output.md", "demo_markdown_report.md"))
})

test_that("the manifest names every sheet of the R workbooks, and only those", {
  sheets <- manifest[manifest$kind == "sheet", , drop = FALSE]
  for (b in unique(sheets$artefact)) {
    want <- sheets$item[sheets$artefact == b & sheets$status == "compared"]
    path <- file.path(run$out_dir, b)
    got <- if (file.exists(path)) tidyxl::xlsx_sheet_names(path) else character()
    # In the Stata order, minus the sheets that are not ported.
    expect_identical(got, want, label = paste(b, "sheets"))
  }
  # Every row is classified, every not-ported row says why.
  expect_true(all(manifest$status %in% c("compared", "not_ported", "not_compared")))
  expect_true(all(nzchar(manifest$reason[manifest$status != "compared"])))
  expect_true(all(nzchar(manifest$golden[manifest$kind == "sheet" & manifest$status == "compared"])))
  # Actual authenticated native-v2 inventory, not source declarations.
  expect_identical(length(unique(sheets$artefact)), 16L)
  expect_identical(nrow(sheets), 102L)
  runtime <- utils::read.csv(golden_demo_path("runtime-inventory.csv"), colClasses = "character", na.strings = character())
  expect_identical(nrow(runtime), 162L)
  expect_identical(runtime[, c("kind", "artefact", "item", "stata")], manifest[, c("kind", "artefact", "item", "stata")])
  expect_identical(as.integer(table(manifest$kind)[c("sheet", "console", "report")]), c(102L, 56L, 4L))
  expect_identical(sheets$scenario[sheets$status == "compared"], rep("", sum(sheets$status == "compared")))
  expect_identical(sheets$golden[sheets$status == "compared"], paste0("demo/", sheets$artefact[sheets$status == "compared"], ":", sheets$item[sheets$status == "compared"]))
})

test_that("the script's **# sections are the Stata do-file's, in order", {
  want <- golden_read_lines(golden_demo_path("sections.txt"))
  script <- golden_read_lines(golden_demo_script("demo_tabtools.R"))
  got <- sub(" ----$", "", sub("^# ", "", grep("^# \\*\\*#", script, value = TRUE)))
  expect_identical(got, want)
})

test_that("the goldens come from the baseline Stata demo", {
  expect_identical(unname(source_info["tabtools_version"]), "2.5.1")
  expect_identical(unname(source_info["stata_tools_commit"]), "712044f8")
  expect_identical(unname(source_info["fixture_check"]), "identical")
})

# ---------------------------------------------------------------------------
# Workbooks

sheet_rows <- manifest[manifest$kind == "sheet" & manifest$status == "compared", , drop = FALSE]
for (i in seq_len(nrow(sheet_rows))) {
  local({
    row <- sheet_rows[i, ]
    test_that(paste0(row$artefact, " sheet '", row$item, "' matches ", row$golden), {
      want <- golden_demo_golden(row$golden)
      got <- file.path(run$out_dir, row$artefact)
      tt <- run$tables[[paste(row$artefact, row$item)]]
      expect_s3_class(tt, "tt_table")
      p_rows <- if (!is.null(tt$rows$vtype)) golden_p_masked_rows(tt)
      why <- golden_compare_styles(got, row$item, want$book, want$sheet, mask = row$mask,
                                   got_width_offset = golden_r_width_offset, p_rows = p_rows,
                                   publication_id = row$golden)
      # effecttab's tolerance scenario compares its body values in the
      # cells (golden_effect_style_filter(), as run_golden_effecttab_scenario()).
      if (identical(tt$command, "effecttab")) why <- golden_effect_style_filter(why, row$mode)
      golden_expect_none(why, paste("styles", row$artefact, row$item))

    })
  })
}

# ---------------------------------------------------------------------------
# Console log and Markdown rendering

console_rows <- manifest[manifest$kind == "console", , drop = FALSE]
stata_dirs <- source_info[["demo_dir"]]
r_dirs <- run$out_dir

test_that("the console blocks pair up with the manifest", {
  sb <- golden_demo_log_blocks(golden_read_lines(golden_demo_path("console_output.log")), "stata")
  rb <- golden_demo_log_blocks(golden_read_lines(file.path(run$out_dir, "console_output.log")), "r")
  expect_identical(vapply(sb, `[[`, "", "key"), console_rows$stata)
  expect_identical(vapply(rb, `[[`, "", "key"), console_rows$r[nzchar(console_rows$r)])
  smd <- golden_demo_md_blocks(golden_read_lines(golden_demo_path("console_output.md")), "stata")
  rmd <- golden_demo_md_blocks(golden_read_lines(file.path(run$out_dir, "console_output.md")), "r")
  expect_identical(vapply(smd, `[[`, "", "key"), console_rows$stata)
  expect_identical(vapply(rmd, `[[`, "", "key"), console_rows$r[nzchar(console_rows$r)])
  # R's Markdown holds exactly its log's output.
  expect_identical(lapply(rmd, `[[`, "lines"), lapply(rb, `[[`, "lines"))
})

local({
  sb <- golden_demo_log_blocks(golden_read_lines(golden_demo_path("console_output.log")), "stata")
  rb <- golden_demo_log_blocks(golden_read_lines(file.path(run$out_dir, "console_output.log")), "r")
  smd <- golden_demo_md_blocks(golden_read_lines(golden_demo_path("console_output.md")), "stata")
  rmd <- golden_demo_md_blocks(golden_read_lines(file.path(run$out_dir, "console_output.md")), "r")
  r_index <- cumsum(nzchar(console_rows$r))
  for (i in which(console_rows$status == "compared")) {
    local({
      row <- console_rows[i, ]
      k <- r_index[i]
      test_that(paste0("console block ", row$item, " (", row$stata, ") matches"), {
        skip_if(length(sb) != nrow(console_rows) || k > length(rb), "console blocks do not pair up")
        got <- golden_demo_console_lines(rb[[k]]$lines, r_dirs)
        want <- golden_demo_console_lines(sb[[i]]$lines, stata_dirs)
        got_md <- sub("\\s+$", "", golden_demo_console_lines(rmd[[k]]$lines, r_dirs))
        want_md <- sub("\\s+$", "", golden_demo_console_lines(smd[[i]]$lines, stata_dirs))
        if (identical(row$item, "C28")) {
          console_body <- golden_demo_primary_console(got, want)
          markdown_body <- golden_demo_primary_console(got_md, want_md)
          got <- console_body$got; want <- console_body$want
          got_md <- markdown_body$got; want_md <- markdown_body$want
        }
        if (row$task == "P7B") {
          adapt <- if (row$r == "tabcell") function(g, w) golden_demo_leaf_console(g, w, row$item, run$leaves) else
            function(g, w) golden_demo_phase7_console(g, w, row$item)
          console_body <- adapt(got, want)
          markdown_body <- adapt(got_md, want_md)
          got <- console_body$got; want <- console_body$want
          got_md <- markdown_body$got; want_md <- markdown_body$want
        }
        golden_expect_none(golden_compare_console(got, want, row$mask), paste("console", row$item))
        golden_expect_none(golden_compare_console(got_md, want_md, row$mask), paste("console md", row$item))
      })
    })
  }
})

# ---------------------------------------------------------------------------
# Markdown report

test_that("demo_markdown_report.md equals Stata's without its unported tables", {
  report_rows <- manifest[manifest$kind == "report", , drop = FALSE]
  stata <- golden_demo_report_tables(golden_read_lines(golden_demo_path("demo_markdown_report.md")))
  expect_identical(names(stata), report_rows$item)
  keep <- report_rows$item[report_rows$status == "compared"]
  # Stata's report minus the unported tables, as bytes: the tables are
  # separated by one blank line, which the last table does not end with.
  want_lines <- unlist(stata[keep], use.names = FALSE)
  while (length(want_lines) && !nzchar(want_lines[length(want_lines)])) want_lines <- want_lines[-length(want_lines)]
  want <- withr::local_tempfile(fileext = ".md")
  con <- file(want, open = "wb")
  writeBin(charToRaw(enc2utf8(paste0(paste(want_lines, collapse = "\n"), "\n"))), con)
  close(con)
  got <- file.path(run$out_dir, "demo_markdown_report.md")
  # The p-value column of Table 1: masked on the rows whose R test differs
  # (the row-wise p mask of the golden sinks).
  tt <- run$tables[["demo_markdown_report.md table1_tc"]]
  expect_s3_class(tt, "tt_table")
  why <- golden_compare_sink(got, want, mask = "p", p_body = golden_p_masked_rows(tt))
  golden_expect_none(why, "demo_markdown_report.md")
})

test_that("new group commands and scalar leaves retain genuine publication records", {
  for (i in which(sheet_rows$task == "P7B")) {
    row <- sheet_rows[i, ]
    tt <- run$tables[[paste(row$artefact, row$item)]]
    expect_s3_class(tt, "tt_table")
    expected_command <- row$command
    if (identical(row$artefact, "demo_desctab.xlsx") && identical(row$item, "Small Cells Primary Mode")) {
      expect_identical(row$command, "desctab")
      expected_command <- "table1_tc"
    }
    if (identical(row$artefact, "demo_ratetab.xlsx") && identical(row$item, "Rates Models")) {
      expect_identical(row$command, "comptab")
      expected_command <- "hrcomptab"
    }
    expect_identical(tt$command, expected_command)
  }
  expect_identical(sort(names(run$leaves)), sprintf("L%02d", 1:9))
  expect_identical(unname(vapply(run$leaves[sprintf("L%02d", 1:7)], function(x) length(x), 0L)), rep(1L, 7L))
  expect_identical(vapply(run$leaves[c("L08", "L09")], function(x) length(x), 0L), c(L08 = 3L, L09 = 3L))
  for (name in c("crosstab", "corrtab")) {
    tt <- run$tables[[paste("demo_markdown_report.md", name)]]
    expect_s3_class(tt, "tt_table")
    expect_identical(tt$command, name)
  }
  # The report legend is independently literal on both sides, then the entire
  # existing report byte comparator above still checks all four tables.
  for (path in c(file.path(run$out_dir, "demo_markdown_report.md"), golden_demo_path("demo_markdown_report.md"))) {
    report <- golden_demo_report_tables(golden_read_lines(path))
    block <- report[["Table 3. Correlation Matrix"]]
    expect_identical(block[nzchar(block)][length(block[nzchar(block)])],
      "*\\* p\\<0.05, \\*\\* p\\<0.01, \\*\\*\\* p\\<0.001*")
  }
})
