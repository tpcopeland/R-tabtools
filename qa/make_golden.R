#!/usr/bin/env Rscript
# Generate the Stata goldens for the parity harness (IMPLEMENTATION_PLAN.md §5).
#
# Developer-only: needs stata-mp on PATH and the dev checkouts
# ~/Stata-Tools/tabtools and ~/Stata-Tools/fvgen. Tests never call this; they
# read the committed files in tests/testthat/golden/.
#
# Usage (from the repo root):
#   Rscript qa/make_golden.R [--update] [--only=T01,R01] [--fixtures]
#                                 [--stata-tools=DIR] [--tabtools-version=V]
#                                 [--no-scenarios] [--no-classifier] [--no-fmt]
#                                 [--no-smallcells] [--no-rates]
#
#   --update         allow overwriting existing goldens (refused otherwise)
#   --stata-tools=DIR  take tabtools/ and fvgen/ from DIR (default ~/Stata-Tools)
#   --tabtools-version=V  the tabtools version the goldens must come from
#                    (default 2.1.14); the run aborts if DIR holds another
#   --only=IDS       regenerate only these scenario ids (comma-separated)
#   --fixtures       rebuild tests/testthat/golden/fixtures/*.dta first (changes the data!)
#   --no-scenarios   skip the scenario goldens
#   --no-classifier  skip golden/swilk.csv, rng_mt64.csv, autotype.csv, detect_vartype.csv
#   --no-fmt         skip golden/stata_fmt.csv
#   --no-smallcells  skip golden/smallcells_engine.csv
#   --no-rates       skip golden/rates_strate.csv

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(x, y) if (is.null(x)) y else x
flag <- function(x) x %in% args
opt <- function(name) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else NULL
}

root <- normalizePath(getwd())
if (!file.exists(file.path(root, "DESCRIPTION")) ||
    !identical(unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]), "tabtools")) {
  stop("Run from the R-tabtools repo root.", call. = FALSE)
}
if (!nzchar(Sys.which("stata-mp"))) stop("stata-mp not found on PATH.", call. = FALSE)

# --stata-tools=DIR: a checkout or `git archive` export holding tabtools/
# and fvgen/ (default ~/Stata-Tools). Use an export of a tagged commit when
# the working tree carries uncommitted edits.
stata_tools <- normalizePath(opt("stata-tools") %||% "~/Stata-Tools", mustWork = TRUE)
tabtools_dir <- file.path(stata_tools, "tabtools")
# Golden baseline: the tabtools version every golden records. Moving the
# baseline is a deliberate step (IMPLEMENTATION_PLAN.md, "Golden baseline
# 2.1.12"), so a checkout at another version is refused.
tabtools_version <- opt("tabtools-version") %||% "2.1.14"
fvgen_dir <- file.path(stata_tools, "fvgen")
qa <- file.path(root, "qa")
golden <- file.path(root, "tests", "testthat", "golden")
work <- file.path(qa, ".stata-work")
dir.create(work, recursive = TRUE, showWarnings = FALSE)

update <- flag("--update")
only <- opt("only")
only <- if (is.null(only)) NULL else strsplit(only, ",", fixed = TRUE)[[1]]

run_stata <- function(dofile, label) {
  message("Stata: ", label)
  old <- setwd(work)
  on.exit(setwd(old))
  status <- system2("stata-mp", c("-b", "do", shQuote(dofile)))
  log <- sub("\\.do$", ".log", basename(dofile))
  txt <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
  err <- grep("^r\\([0-9]+\\);$", txt)
  if (status != 0L || length(err)) {
    stop(label, " failed; see ", file.path(work, log), call. = FALSE)
  }
  invisible(txt)
}

refuse_overwrite <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) && !update) {
    stop("Goldens exist (", length(existing), " files, e.g. ", basename(existing[1]),
         "). Re-run with --update to overwrite.", call. = FALSE)
  }
}

# Stata do-file header: dev checkouts first on the adopath, clean session.
stata_header <- function() {
  c("version 17.0",
    "clear all",
    "set more off",
    "set varabbrev off",
    "set linesize 255",
    "set rng mt64",
    sprintf('adopath ++ "%s"', fvgen_dir),
    sprintf('adopath ++ "%s"', tabtools_dir),
    "discard",
    sprintf('run "%s"', file.path(qa, "stata", "golden_helpers.do")),
    sprintf('cd "%s"', golden),
    sprintf('golden_versions "VERSIONS.csv" "%s" "%s" "%s"', tabtools_dir, fvgen_dir, tabtools_version))
}

# ---------------------------------------------------------------------------
# Fixtures
if (flag("--fixtures")) {
  refuse_overwrite(list.files(file.path(golden, "fixtures"), full.names = TRUE))
  old <- setwd(root)
  status <- system2("stata-mp", c("-b", "do", "qa/stata/make_fixtures.do"))
  if (file.exists("make_fixtures.log")) file.rename("make_fixtures.log", file.path(work, "make_fixtures.log"))
  setwd(old)
  if (status != 0L || any(grepl("^r\\([0-9]+\\);$", readLines(file.path(work, "make_fixtures.log"))))) {
    stop("make_fixtures.do failed; see qa/.stata-work/make_fixtures.log", call. = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Scenario goldens
# Sink options appended to each call unless the call already names them.
# Placeholder @ID@ in setup/call expands to the scenario id.
scenario_sinks <- function(call, id, command) {
  add <- character()
  if (!grepl("\\bcsv\\(", call)) add <- c(add, sprintf('csv("%s.csv")', id))
  if (!grepl("\\bmarkdown\\(", call)) add <- c(add, sprintf('markdown("%s.md")', id))
  if (command %in% c("puttab", "wttab")) {
    # puttab writes to `using file.xlsx` (task 7.6): inserted before the
    # options comma unless the command part already names it, plus
    # sheet("<id>"). wttab (task 7.12) is the Stata reference program of
    # qa/stata/golden_wttab.do, which writes through puttab the same way
    # (workbook wttab.xlsx).
    at <- options_comma(call)
    head <- if (at > 0L) substr(call, 1L, at - 1L) else call
    if (!grepl("\\busing\\b", head)) {
      ins <- sprintf(' using "%s.xlsx"', command)
      call <- if (at > 0L) paste0(head, ins, substring(call, at)) else paste0(call, ins, ",")
    }
    if (!grepl("\\bsheet\\(", substring(call, max(at, 1L)))) add <- c(add, sprintf('sheet("%s")', id))
  } else if (command == "stacktab") {
    # stacktab reads its blocks from, and writes into, its own `using`
    # workbook: each scenario names `using "@ID@.xlsx"` and its sheet() in
    # the manifest (blocks() holds sheet() sub-options too), and its setup
    # builds that workbook afresh, so --only reruns need no sheet removal.
    if (!grepl("\\busing\\b", call)) stop("stacktab scenario ", id, " must name its using workbook", call. = FALSE)
  } else if (!grepl("\\b(xlsx|excel)\\(", call)) {
    add <- c(add, sprintf('xlsx("%s.xlsx") sheet("%s")', command, id))
  }
  paste(c(call, add), collapse = " ")
}

# Position of the comma that starts a Stata command's options: the first
# comma outside parentheses and double quotes (a comma inside an `if`
# expression such as inlist(x, 1, 2) does not count); 0 when there is none.
options_comma <- function(call) {
  ch <- strsplit(call, "")[[1]]
  depth <- 0L
  inq <- FALSE
  for (i in seq_along(ch)) {
    if (ch[i] == "\"") inq <- !inq
    if (inq) next
    if (ch[i] == "(") depth <- depth + 1L
    if (ch[i] == ")") depth <- depth - 1L
    if (ch[i] == "," && depth == 0L) return(i)
  }
  0L
}

split_stmts <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  trimws(strsplit(x, " ; ", fixed = TRUE)[[1]])
}

# Stata's text log of the call: drop the wrapper lines so the file holds only
# what the command printed.
clean_console <- function(path) {
  if (!file.exists(path)) return(invisible())
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  start <- grep("^\\. capture noisily _golden_call\\s*$", txt)
  if (length(start)) txt <- txt[-seq_len(start[1])]
  end <- grep("^\\. local rc = _rc\\s*$", txt)
  if (length(end)) txt <- txt[seq_len(end[1] - 1L)]
  while (length(txt) && !nzchar(trimws(txt[length(txt)]))) txt <- txt[-length(txt)]
  writeLines(txt, path, useBytes = TRUE)
}

if (!flag("--no-scenarios")) {
  sc <- read.csv(file.path(golden, "scenarios.csv"), stringsAsFactors = FALSE,
                 encoding = "UTF-8", na.strings = character())
  if (!is.null(only)) {
    bad <- setdiff(only, sc$id)
    if (length(bad)) stop("Unknown scenario ids: ", paste(bad, collapse = ", "), call. = FALSE)
    sc <- sc[sc$id %in% only, ]
  }
  targets <- as.vector(outer(sc$id, c(".csv", ".md", "_stored.csv", "_console.txt"), paste0))
  # effecttab goldens (task 7.11): a setup that calls golden_effect_input
  # also writes <id>_input.csv, Stata's r(table) rows for the r_call.
  inputs <- sc$id[grepl("golden_effect_input", sc$stata_setup, fixed = TRUE)]
  targets <- c(targets, paste0(inputs, "_input.csv"))
  refuse_overwrite(file.path(golden, targets))

  body <- stata_header()
  body <- c(body, 'capture erase "_status.csv"')
  if (is.null(only)) {
    body <- c(body, sprintf('capture erase "%s.xlsx"', unique(sc$command)))
  }
  for (i in seq_len(nrow(sc))) {
    s <- sc[i, ]
    setup <- gsub("@ID@", s$id, split_stmts(s$stata_setup), fixed = TRUE)
    call <- gsub("@ID@", s$id, scenario_sinks(s$stata_call, s$id, s$command), fixed = TRUE)
    body <- c(body, "",
              sprintf("* ---- %s: %s", s$id, s$description),
              "capture program drop _golden_setup",
              "program define _golden_setup",
              "    version 17.0",
              paste0("    ", setup),
              "end",
              "capture program drop _golden_call",
              "program define _golden_call",
              "    version 17.0",
              paste0("    ", call),
              "end",
              sprintf("golden_run %s %s", s$id, s$fixture))
  }
  dofile <- file.path(work, "run_scenarios.do")
  writeLines(enc2utf8(body), dofile, useBytes = TRUE)
  run_stata(dofile, sprintf("%d scenarios", nrow(sc)))

  status <- read.csv(file.path(golden, "_status.csv"), header = FALSE, col.names = c("id", "rc"))
  file.rename(file.path(golden, "_status.csv"), file.path(work, "_status.csv"))
  for (id in status$id[status$rc == 0]) clean_console(file.path(golden, paste0(id, "_console.txt")))
  failed <- status[status$rc != 0, ]
  if (nrow(failed)) {
    for (id in failed$id) {
      for (ext in c(".csv", ".md", "_stored.csv", "_console.txt", "_input.csv")) unlink(file.path(golden, paste0(id, ext)))
    }
    stop("Scenarios failed in Stata: ", paste0(failed$id, " (r(", failed$rc, "))", collapse = ", "),
         "\nSee ", file.path(work, "run_scenarios.log"), call. = FALSE)
  }
  message("Scenario goldens written: ", nrow(status))
}

# ---------------------------------------------------------------------------
# Classifier goldens (task 0.5b) and formatting goldens (task 0.5c)
if (!flag("--no-classifier")) {
  refuse_overwrite(file.path(golden, c("swilk.csv", "rng_mt64.csv", "autotype.csv",
                                       "subsample_ids.csv", "detect_vartype.csv")))
  dofile <- file.path(work, "run_classifier.do")
  writeLines(c(stata_header(), sprintf('do "%s"', file.path(qa, "stata", "make_classifier_goldens.do"))), dofile)
  run_stata(dofile, "classifier goldens")
}
if (!flag("--no-fmt")) {
  refuse_overwrite(file.path(golden, "stata_fmt.csv"))
  dofile <- file.path(work, "run_fmt.do")
  writeLines(c(stata_header(), sprintf('do "%s"', file.path(qa, "stata", "make_fmt_goldens.do"))), dofile)
  run_stata(dofile, "formatting goldens")
}
if (!flag("--no-smallcells")) {
  refuse_overwrite(file.path(golden, "smallcells_engine.csv"))
  dofile <- file.path(work, "run_smallcells.do")
  writeLines(c(stata_header(), sprintf('do "%s"', file.path(qa, "stata", "make_smallcells_goldens.do"))), dofile)
  run_stata(dofile, "small-cell engine goldens")
}
if (!flag("--no-rates")) {
  refuse_overwrite(file.path(golden, "rates_strate.csv"))
  dofile <- file.path(work, "run_rates.do")
  writeLines(c(stata_header(), sprintf('do "%s"', file.path(qa, "stata", "make_rates_goldens.do"))), dofile)
  run_stata(dofile, "strate rate goldens")
}
message("Done.")
