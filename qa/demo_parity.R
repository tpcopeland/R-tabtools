#!/usr/bin/env Rscript
# qa/demo_parity.R - the R demo against the Stata demo (IMPLEMENTATION_PLAN.md
# Milestone D: D3 check, D6 regeneration).
#
#   Rscript qa/demo_parity.R            run tests/testthat/test-demo-parity.R
#                                       (exit 1 on any failure or skip; ends
#                                       with the QA-RESULT marker)
#   Rscript qa/demo_parity.R --update [--commit=1255176d]
#                            [--tabtools-version=2.1.14] [--stata-tools=DIR]
#                            [--only=FILES] [--keep-work]
#
# --update regenerates the Stata side, then runs the check. It needs
# stata-mp on PATH and network access (the demo runs `webuse union`):
#   1. `git archive <commit> tabtools _data tc_schemes logdoc` of DIR
#      (default ~/Stata-Tools) into a scratch directory: never the working
#      tree or an installed copy;
#   2. qa/stata/run_demo.do runs the exported demo_tabtools.do (the version
#      guard refuses any tabtools but --tabtools-version) and copies its
#      artefacts out;
#   3. rebuilds the demo's datasets from the exported do-file (the dataset
#      build and the mixed, zip, hurdle and hrcomptab draws) and checks that the golden
#      fixtures (and so qa/demo/demo_tabtools.rds, qa/make_demo_data.R)
#      hold them exactly;
#   4. checks that qa/demo/demo_tabtools.R has the do-file's **#
#      sections in order, and tests/testthat/golden/demo/manifest.csv
#      against the artefacts:
#      every Stata sheet, console block and report table listed, in order;
#      every ported sheet whose golden is a scenario golden (table1_tc.xlsx,
#      regtab.xlsx, ...) identical to the Stata demo sheet, so no demo sheet
#      is stored twice;
#   5. copies into tests/testthat/golden/demo/ the Stata workbooks the
#      manifest compares against (golden "demo/<book>:<sheet>"),
#      console_output.log, console_output.md and demo_markdown_report.md
#      and writes SOURCE.csv (--only=FILES: just those workbooks or the
#      report, comma-separated, from the release SOURCE.csv names; SOURCE.csv
#      is kept).
# When the golden baseline moves, regenerate the scenario goldens first
# (qa/make_golden.R), then run this with the new --commit and
# --tabtools-version, and update the version the test expects.

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(x, y) if (is.null(x)) y else x
opt <- function(name) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else NULL
}
file_arg <- sub("^--file=", "", commandArgs(FALSE)[startsWith(commandArgs(FALSE), "--file=")])
root <- if (length(file_arg)) dirname(dirname(normalizePath(file_arg[1]))) else normalizePath(getwd())
if (!file.exists(file.path(root, "DESCRIPTION")) ||
    !identical(unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]), "tabtools")) {
  stop("cannot find the tabtools package root", call. = FALSE)
}
setwd(root)
tests <- file.path(root, "tests", "testthat")
gdemo <- file.path(tests, "golden", "demo")

# The package: the installed copy qa/run_all.R points at, else the source.
lib <- Sys.getenv("TABTOOLS_QA_LIB")
if (nzchar(lib)) {
  suppressPackageStartupMessages(library(tabtools, lib.loc = lib))
} else {
  pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
}

if ("--update" %in% args) {
  if (!nzchar(Sys.which("stata-mp"))) stop("stata-mp not found on PATH", call. = FALSE)
  commit <- opt("commit") %||% "1255176d"
  version <- opt("tabtools-version") %||% "2.1.14"
  stata_tools <- normalizePath(opt("stata-tools") %||% "~/Stata-Tools", mustWork = TRUE)
  only <- opt("only")
  only <- if (is.null(only)) NULL else strsplit(only, ",", fixed = TRUE)[[1]]
  if (!is.null(only)) {
    # A partial update keeps SOURCE.csv, so it must come from the same
    # release, and cannot replace the console log (whose paths SOURCE.csv
    # records).
    old_src <- utils::read.csv(file.path(gdemo, "SOURCE.csv"), colClasses = "character")
    old_src <- stats::setNames(old_src$value, old_src$key)
    if (!identical(unname(old_src[c("stata_tools_commit", "tabtools_version")]), c(commit, version))) {
      stop("--only needs the commit and version of golden/demo/SOURCE.csv; run a full --update", call. = FALSE)
    }
    if (any(c("console_output.log", "console_output.md") %in% only)) {
      stop("--only cannot replace the console log; run a full --update", call. = FALSE)
    }
  }
  # A fixed name, not tempfile()'s random suffix: C28/C29 print this
  # directory, so a fixed-length path keeps where the log wraps their lines
  # the same from one --update to the next (task C7; logdoc up to 1.1.7
  # rendered such a wrap differently depending on the text at the cut).
  work <- file.path(tempdir(), "tabtools-demo-stata")
  unlink(work, recursive = TRUE)
  export <- file.path(work, "export")
  out <- file.path(work, "out")
  dir.create(export, recursive = TRUE)
  if (!("--keep-work" %in% args)) on.exit(unlink(work, recursive = TRUE), add = TRUE)
  message("Stata-Tools ", commit, " -> ", export)
  st <- system(sprintf("git -C %s archive %s tabtools _data tc_schemes logdoc | tar -x -C %s",
                       shQuote(stata_tools), shQuote(commit), shQuote(export)))
  if (st != 0L) stop("git archive failed", call. = FALSE)

  run_stata <- function(lines, name) {
    do <- file.path(work, paste0(name, ".do"))
    writeLines(lines, do)
    old <- setwd(work)
    on.exit(setwd(old))
    st <- system2("stata-mp", c("-b", "do", shQuote(do)))
    log <- file.path(work, paste0(name, ".log"))
    txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
    if (st != 0L || any(grepl("^r\\([0-9]+\\);$", txt))) stop(name, " failed; see ", log, call. = FALSE)
    txt
  }

  # 2. The demo.
  message("Stata: demo_tabtools.do (tabtools ", version, ")")
  run_stata(c(sprintf('do "%s" "%s" "%s" %s', file.path(root, "qa", "stata", "run_demo.do"), export, out, version),
              sprintf('file open fh using "%s", write text replace', file.path(work, "stata_version.txt")),
              'file write fh "`c(stata_version)\'" _n',
              "file close fh"), "demo")
  stata_version <- readLines(file.path(work, "stata_version.txt"))

  # 3. The demo's datasets against the golden fixtures.
  demo <- readLines(file.path(export, "tabtools", "demo", "demo_tabtools.do"), warn = FALSE)
  section <- function(head, stop_at) {
    i <- which(sub("\\s+$", "", demo) == head)
    if (length(i) != 1L) stop("section not found in demo_tabtools.do: ", head, call. = FALSE)
    j <- i + which(grepl(stop_at, demo[-seq_len(i)]))[1]
    x <- demo[(i + 1L):(j - 1L)]
    x[x != "preserve"]
  }
  data_dir <- file.path(work, "data")
  dir.create(data_dir)
  save <- function(name) sprintf('save "%s", replace', file.path(data_dir, paste0(name, ".dta")))
  message("Stata: the demo's datasets")
  run_stata(c("version 17.0", "clear all", "set rng mt64", "set more off",
              gsub("`repo_root'", export, section("**# Build analysis dataset", "^tempfile analysis"), fixed = TRUE),
              save("cohort"),
              section("**# Sheet 15: Mixed Model -- Random effects with relabel + ICC", "^collect clear"), save("mixed_bp"),
              section("**# Sheet 31: ZIP ZINB -- Zero-inflated count models", "^collect clear"), save("zip"),
              section("**# Sheet 32: Hurdle -- Cragg hurdle model", "^collect clear"), save("hurdle"),
              # Sheet 46: the dose data continue the binary data's random-number stream.
              section("* Binary HRT model frame: one non-reference row", "^collect clear"), save("hrt_bin"),
              section("* Dose category model frame: three non-reference rows after header + reference",
                      "^collect clear"), save("hrt_dose"),
              "webuse union, clear", save("union")), "demo_data")
  for (nm in c("cohort", "mixed_bp", "zip", "hurdle", "hrt_bin", "hrt_dose", "union")) {
    a <- haven::read_dta(file.path(data_dir, paste0(nm, ".dta")))
    b <- haven::read_dta(file.path(tests, "golden", "fixtures", paste0(nm, ".dta")))
    common <- intersect(names(a), names(b))
    same <- nrow(a) == nrow(b) && all(vapply(common, function(v) {
      identical(as.vector(a[[v]]), as.vector(b[[v]])) &&
        identical(attr(a[[v]], "label"), attr(b[[v]], "label")) &&
        identical(attr(a[[v]], "labels"), attr(b[[v]], "labels"))
    }, TRUE))
    if (!same) stop("fixture ", nm, ".dta differs from the Stata demo's data: rebuild the fixtures ",
                    "(qa/make_golden.R --fixtures) and qa/make_demo_data.R", call. = FALSE)
    message("  ", nm, ": ", length(common), " variables identical to the fixture")
  }
  rds <- readRDS(file.path(root, "qa", "demo", "demo_tabtools.rds"))
  for (nm in names(rds)) {
    b <- as.data.frame(haven::read_dta(file.path(tests, "golden", "fixtures", paste0(nm, ".dta"))))
    if (!identical(rds[[nm]], b[names(rds[[nm]])])) {
      stop("qa/demo/demo_tabtools.rds is out of date: run qa/make_demo_data.R", call. = FALSE)
    }
  }

  # The R script's sections: the do-file's **# headings, in order.
  heads_do <- sub("\\s+$", "", grep("^\\*\\*#", demo, value = TRUE))
  script <- readLines(file.path(root, "qa", "demo", "demo_tabtools.R"), warn = FALSE)
  heads_r <- sub(" ----$", "", sub("^# ", "", grep("^# \\*\\*#", script, value = TRUE)))
  if (!identical(heads_r, heads_do)) {
    stop("qa/demo/demo_tabtools.R's **# sections differ from demo_tabtools.do's: ",
         paste(setdiff(union(heads_do, heads_r), intersect(heads_do, heads_r)), collapse = "; "), call. = FALSE)
  }
  message("  ", length(heads_r), " **# sections, as in demo_tabtools.do")
  # Kept with the goldens, so test-demo-parity.R checks the script's
  # headings without Stata (Milestone D review).
  con <- file(file.path(gdemo, "sections.txt"), "wb")
  writeLines(enc2utf8(heads_do), con, sep = "\n", useBytes = TRUE)
  close(con)

  # 4. The manifest against the artefacts.
  old <- setwd(tests)
  suppressMessages(invisible(utils::capture.output(testthat::source_test_helpers(".", env = environment()))))
  setwd(old)
  manifest <- utils::read.csv(file.path(gdemo, "manifest.csv"), stringsAsFactors = FALSE,
                              na.strings = character(), colClasses = "character")
  problems <- character()
  sheets <- manifest[manifest$kind == "sheet", , drop = FALSE]
  books <- sort(list.files(out, pattern = "\\.xlsx$"))
  if (!identical(books, sort(unique(sheets$artefact)))) problems <- c(problems, "the manifest's workbooks differ from the demo's")
  for (b in books) {
    got <- tidyxl::xlsx_sheet_names(file.path(out, b))
    if (!identical(got, sheets$item[sheets$artefact == b])) {
      problems <- c(problems, sprintf("%s: sheets [%s], the manifest lists [%s]", b, paste(got, collapse = ", "),
                                      paste(sheets$item[sheets$artefact == b], collapse = ", ")))
    }
  }
  keys <- vapply(golden_demo_log_blocks(readLines(file.path(out, "console_output.log"), encoding = "UTF-8"), "stata"),
                 `[[`, "", "key")
  if (!identical(keys, manifest$stata[manifest$kind == "console"])) problems <- c(problems, "console blocks differ from the manifest")
  heads <- names(golden_demo_report_tables(readLines(file.path(out, "demo_markdown_report.md"), encoding = "UTF-8")))
  if (!identical(heads, manifest$item[manifest$kind == "report"])) problems <- c(problems, "report tables differ from the manifest")
  demo_books <- character()
  for (i in which(sheets$status == "compared")) {
    g <- golden_demo_golden(sheets$golden[i])
    if (startsWith(sheets$golden[i], "demo/")) {
      demo_books <- c(demo_books, sheets$artefact[i])
      next
    }
    why <- golden_compare_styles(file.path(out, sheets$artefact[i]), sheets$item[i], g$book, g$sheet,
                                 got_width_offset = golden_width_offset, width_tol = 0)
    if (length(why)) {
      problems <- c(problems, sprintf("%s '%s' is no longer identical to %s (%s): point its golden at demo/%s",
                                      sheets$artefact[i], sheets$item[i], sheets$golden[i], why[1], sheets$artefact[i]))
    }
  }
  if (length(problems)) stop(paste(c("manifest check failed:", problems), collapse = "\n  "), call. = FALSE)

  # 5. Copy and record.
  files <- c(unique(demo_books), "console_output.log", "console_output.md", "demo_markdown_report.md")
  if (!is.null(only)) files <- intersect(files, only)
  dir.create(gdemo, showWarnings = FALSE)
  for (f in files) {
    file.copy(file.path(out, f), file.path(gdemo, f), overwrite = TRUE)
    message("  golden/demo/", f)
  }
  if (is.null(only)) {
    # demo_dir: where this run's Stata wrote, as its console log names it.
    src <- data.frame(key = c("stata_tools_commit", "tabtools_version", "stata_version", "generated", "demo_dir",
                              "fixture_check"),
                      value = c(commit, version, stata_version, format(Sys.Date()),
                                file.path(export, "tabtools", "demo"), "identical"))
    utils::write.csv(src, file.path(gdemo, "SOURCE.csv"), row.names = FALSE)
    message("wrote golden/demo/SOURCE.csv")
  }
}

# The check.
Sys.setenv(NOT_CRAN = "true")
source(file.path(root, "qa", "tools", "qa_result.R"))
rep <- qa_testthat_reporter()
res <- testthat::test_file(file.path(tests, "test-demo-parity.R"), reporter = rep,
                           load_package = "none", stop_on_failure = FALSE)
df <- as.data.frame(res)
bad <- sum(df$failed) + sum(df$error)
warned <- sum(df$warning)
skipped <- sum(df$skipped)
cat("\ndemo parity:", nrow(df), "tests,", bad, "expectations failed or errored,", warned, "warnings,",
    skipped, "skipped,", length(rep$stray), "outside a test block\n")
# The completion marker qa/run_all.R reconciles (Codex audit CX-6): one
# comparison per test block, which fails on a warning too (codex audit
# F10); a skipped block makes the run INCOMPLETE.
qa_record_testthat(df, rep$stray, "test-demo-parity.R")
st <- qa_done("demo_parity.R")
if (!identical(st, "PASS")) quit(save = "no", status = 1L)
