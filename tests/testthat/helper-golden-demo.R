# Parity of the R demo (qa/demo/demo_tabtools.R) with the Stata demo
# (tabtools/demo/demo_tabtools.do), IMPLEMENTATION_PLAN.md Milestone D,
# task D3. The Stata artefacts come from qa/stata/run_demo.do on a `git
# archive` export of the golden baseline (golden/demo/SOURCE.csv);
# golden/demo/manifest.csv lists every Stata sheet, console block and report
# table, with the golden each ported one is compared with (a scenario golden
# when the Stata demo sheet is identical to it, else the Stata demo workbook
# itself) and the reason for each one that is not ported.

golden_demo_path <- function(...) golden_path("demo", ...)

# The demo script and its datasets live in the source repository only
# (qa/demo/, .Rbuildignored with golden/demo/: the datasets include Stata's
# auto and union), so the parity test runs from a source checkout.
golden_demo_script <- function(...) test_path("..", "..", "qa", "demo", ...)

golden_demo_manifest <- function() {
  utils::read.csv(golden_demo_path("manifest.csv"), stringsAsFactors = FALSE,
                  encoding = "UTF-8", na.strings = character(), colClasses = "character")
}

golden_demo_source <- function() {
  s <- utils::read.csv(golden_demo_path("SOURCE.csv"), stringsAsFactors = FALSE,
                       encoding = "UTF-8", na.strings = character(), colClasses = "character")
  stats::setNames(s$value, s$key)
}

golden_demo_packages <- c("haven", "survival", "lme4", "geepack", "MASS", "nnet", "pscl",
                          "WeightIt", "marginaleffects", "tidyxl", "openxlsx2")

# Run the R demo once per session into a temporary directory, recording
# every table the tabtools commands return, keyed by "<workbook> <sheet>"
# (or "<markdown file> <function>" for a Markdown-only call). The recorders shadow
# the exported functions in the environment the script runs in; the script
# itself is unchanged, and its console log still names the real calls.
golden_demo_state <- new.env(parent = emptyenv())

golden_demo_run <- function() {
  if (!is.null(golden_demo_state$run)) return(golden_demo_state$run)
  script <- golden_demo_script("demo_tabtools.R")
  if (!file.exists(script)) stop("qa/demo/demo_tabtools.R not found", call. = FALSE)
  out <- tempfile("tabtools-demo-")
  dir.create(out)
  tables <- new.env(parent = emptyenv())
  env <- new.env(parent = globalenv())
  for (fn in c("table1_tc", "desctab", "regtab", "puttab", "stacktab", "stratetab", "effecttab",
              "comptab", "hrcomptab")) {
    local({
      name <- fn
      f <- getExportedValue("tabtools", name)
      assign(fn, function(...) {
        res <- withVisible(f(...))
        a <- list(...)
        key <- if (!is.null(a$xlsx)) {
          paste(basename(a$xlsx), a$sheet)
        } else if (!is.null(a$markdown)) {
          paste(basename(a$markdown), name)
        }
        if (!is.null(key)) assign(key, res$value, envir = tables)
        if (res$visible) res$value else invisible(res$value)
      }, envir = env)
    })
  }
  env$out_dir <- out
  env$demo_data_file <- normalizePath(golden_demo_script("demo_tabtools.rds"))
  printed <- utils::capture.output(
    suppressMessages(sys.source(script, envir = env, keep.source = FALSE)))
  golden_demo_state$run <- list(out_dir = out, tables = as.list(tables), printed = printed)
  golden_demo_state$run
}

# "table1_tc.xlsx:T30a" -> the workbook path and sheet of a manifest golden;
# "demo/demo_desctab.xlsx:Events" is a Stata demo workbook in golden/demo/.
golden_demo_golden <- function(spec) {
  at <- regexpr(":", spec, fixed = TRUE)
  list(book = do.call(golden_path, as.list(strsplit(substr(spec, 1L, at - 1L), "/", fixed = TRUE)[[1]])),
       sheet = substring(spec, at + 1L))
}

# ---------------------------------------------------------------------------
# Console log and its Markdown rendering

# A log split into command blocks: key (the command's first line, Stata's
# `noisily ` and `///` dropped, or the R function called), and the lines it
# printed. Stata: a block starts at an echoed `. noisily ...` or `. tabtools
# ...` command (its `> ` continuation lines follow the echo); every other
# echoed command (`. log off demo`, `. keep in 1/6`, comments) ends it.
# R: console() writes each call as `> ` + `+ ` lines, then its output.
golden_demo_stata_key <- function(l) {
  k <- sub("^\\. ", "", l)
  k <- sub("^noisily ", "", k)
  trimws(sub("\\s*///\\s*$", "", k))
}

golden_demo_log_blocks <- function(lines, side = c("stata", "r")) {
  side <- match.arg(side)
  blocks <- list()
  cur <- NULL
  in_echo <- FALSE
  flush <- function() if (!is.null(cur)) blocks[[length(blocks) + 1L]] <<- cur
  for (l in lines) {
    if (side == "stata") {
      if (startsWith(l, ". ")) {
        flush()
        cur <- NULL
        in_echo <- TRUE
        if (grepl("^\\. (noisily |tabtools)", l)) cur <- list(key = golden_demo_stata_key(l), lines = character())
        next
      }
      if (in_echo && startsWith(l, "> ")) next
    } else {
      if (startsWith(l, "> ")) {
        flush()
        cur <- list(key = sub("\\(.*$", "", substring(l, 3L)), lines = character())
        if (cur$key == "print") cur$key <- sub("^print\\(([^(]+)\\(.*$", "\\1", substring(l, 3L))
        in_echo <- TRUE
        next
      }
      if (in_echo && startsWith(l, "+ ")) next
    }
    in_echo <- FALSE
    if (!is.null(cur)) cur$lines <- c(cur$lines, l)
  }
  flush()
  blocks
}

# The Markdown rendering: Stata's logdoc writes each command as a ```stata
# fence and its output as the ``` fence after it; console() writes ```r and
# ``` fences the same way.
golden_demo_md_blocks <- function(lines, side = c("stata", "r")) {
  side <- match.arg(side)
  open <- if (side == "stata") "```stata" else "```r"
  blocks <- list()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (lines[i] == open) {
      j <- i + 1L
      while (j <= n && lines[j] != "```") j <- j + 1L
      code <- lines[seq_len(j - i - 1L) + i]
      i <- j + 1L
      while (i <= n && !nzchar(lines[i])) i <- i + 1L
      out <- character()
      if (i <= n && lines[i] == "```") {
        j <- i + 1L
        while (j <= n && lines[j] != "```") j <- j + 1L
        out <- lines[seq_len(j - i - 1L) + i]
        i <- j + 1L
      }
      key <- if (side == "stata") {
        if (!grepl("^\\. (noisily |tabtools)", code[1])) next
        golden_demo_stata_key(code[1])
      } else {
        k <- sub("\\(.*$", "", code[1])
        if (k == "print") k <- sub("^print\\(([^(]+)\\(.*$", "\\1", code[1])
        k
      }
      blocks[[length(blocks) + 1L]] <- list(key = key, lines = out)
      next
    }
    i <- i + 1L
  }
  blocks
}

# The comparable lines of a block: golden_console_box() (Stata's wrapped
# lines rejoined, the chatter before a listing and the sink messages
# dropped), with each side's output directory removed from file paths
# (forward slashes; R's temporary directory is normalised on Windows too).
golden_demo_console_lines <- function(lines, dirs) {
  golden_demo_strip_dirs(golden_console_box(lines), dirs)
}

golden_demo_strip_dirs <- function(out, dirs) {
  for (d in unique(c(dirs, gsub("\\", "/", dirs, fixed = TRUE)))) {
    out <- gsub(paste0(d, "/"), "", out, fixed = TRUE)
  }
  out
}

# The Markdown rendering of a console block: both sides right-trimmed (the
# converters differ in trailing blanks only), each side's output
# directories removed. Up to logdoc 1.1.7 Stata's console_output.md split
# or trimmed lines the log wrapped at its linesize, and the comparison
# accepted those renderings a pinned number of times per block (manifest
# column md_pin; ~/Stata-Dev/_take_action/2026-09-26-logdoc-wrapped-output.md).
# logdoc 1.1.8 (Stata-Tools 1255176d, the 2.1.14 goldens; task C8) keeps
# wrapped lines whole in their output fence, so the lines must match as
# they are.
golden_demo_md_compare <- function(got, want, mask, got_dirs, want_dirs) {
  rt <- function(x) sub("\\s+$", "", x)
  got <- rt(golden_demo_console_lines(got, got_dirs))
  want <- rt(golden_demo_console_lines(want, want_dirs))
  golden_compare_console(got, want, mask)
}

# ---------------------------------------------------------------------------
# Markdown report

# The report split at its "### " headings: one element per table, named by
# the heading, holding its lines up to the next heading.
golden_demo_report_tables <- function(lines) {
  at <- which(startsWith(lines, "### "))
  if (!length(at)) return(list())
  ends <- c(at[-1L] - 1L, length(lines))
  stats::setNames(lapply(seq_along(at), function(k) lines[at[k]:ends[k]]), sub("^### ", "", lines[at]))
}
