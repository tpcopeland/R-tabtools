# qa/tools/qa_result.R - executed/skipped/failed bookkeeping for the QA
# scripts, and the runner's reconciliation of it (Codex audit CX-6).
#
# A QA script sources this file, records each comparison with qa_check()
# and each comparison it cannot run with qa_skip(), and ends with
# qa_done(), which prints one completion marker:
#   QA-RESULT script=<name> status=<PASS|INCOMPLETE|SKIP|FAIL> executed=<n> skipped=<n> failed=<n>
# and exits non-zero on a failure. qa/run_all.R reads the marker back
# (qa_reconcile()) instead of trusting the exit status alone: a script that
# ran no comparison is SKIP, one that skipped some is INCOMPLETE, and a
# script that stops before its marker is a failure. Before this, a script
# whose comparator packages were all missing printed "passed" and the
# runner reported PASS.
#
# qa_has(pkg) is requireNamespace() that honours TABTOOLS_QA_HIDE (a comma
# separated list of packages to treat as missing), so the missing-package
# paths can be exercised without uninstalling anything.

qa_state <- new.env(parent = emptyenv())
qa_state$executed <- 0L
qa_state$failed <- character()
qa_state$skipped <- character()

qa_has <- function(pkg) {
  hidden <- trimws(strsplit(Sys.getenv("TABTOOLS_QA_HIDE"), ",", fixed = TRUE)[[1]])
  !(pkg %in% hidden) && requireNamespace(pkg, quietly = TRUE)
}

qa_check <- function(what, cond) {
  pass <- isTRUE(cond)
  cat(sprintf("%-4s %s\n", if (pass) "ok" else "FAIL", what))
  qa_state$executed <- qa_state$executed + 1L
  if (!pass) qa_state$failed <- c(qa_state$failed, what)
  invisible(pass)
}

qa_skip <- function(what, reason) {
  cat(sprintf("SKIP %s: %s\n", what, reason))
  qa_state$skipped <- c(qa_state$skipped, what)
  invisible(FALSE)
}

qa_status <- function(executed, skipped, failed) {
  if (failed > 0L) return("FAIL")
  if (executed == 0L) return("SKIP")
  if (skipped > 0L) return("INCOMPLETE")
  "PASS"
}

qa_done <- function(script) {
  ex <- qa_state$executed
  sk <- length(qa_state$skipped)
  fa <- length(qa_state$failed)
  st <- qa_status(ex, sk, fa)
  cat(sprintf("QA-RESULT script=%s status=%s executed=%d skipped=%d failed=%d\n", script, st, ex, sk, fa))
  if (identical(st, "FAIL")) quit(save = "no", status = 1L)
  invisible(st)
}

# The runner's verdict for one script from its exit status and its log:
# a list with status (PASS, INCOMPLETE, SKIP or FAIL), the counts, and a
# note. The status is recomputed from the counts, never taken from the
# marker's own label, and the marker must name the script.
qa_reconcile <- function(script, exit_status, log_lines) {
  out <- list(status = "FAIL", executed = NA_integer_, skipped = NA_integer_, failed = NA_integer_, note = "")
  pat <- "^QA-RESULT script=(\\S+) status=(\\S+) executed=([0-9]+) skipped=([0-9]+) failed=([0-9]+)\\s*$"
  hits <- grep(pat, log_lines, value = TRUE)
  if (!identical(exit_status, 0L)) {
    out$note <- paste("exit status", exit_status)
    if (length(hits) == 1L) {
      m <- regmatches(hits, regexec(pat, hits))[[1]]
      out[c("executed", "skipped", "failed")] <- as.list(as.integer(m[4:6]))
    }
    return(out)
  }
  if (length(hits) != 1L) {
    out$note <- if (!length(hits)) "no completion marker (the script stopped early?)" else "more than one completion marker"
    return(out)
  }
  m <- regmatches(hits, regexec(pat, hits))[[1]]
  if (!identical(m[2], script)) {
    out$note <- sprintf("completion marker names %s", m[2])
    return(out)
  }
  n <- as.integer(m[4:6])
  out[c("executed", "skipped", "failed")] <- as.list(n)
  out$status <- qa_status(n[1], n[2], n[3])
  if (!identical(out$status, m[3])) {
    out$status <- "FAIL"
    out$note <- sprintf("marker says %s, its counts say otherwise", m[3])
    return(out)
  }
  out$note <- switch(out$status,
    SKIP = "no comparison ran",
    INCOMPLETE = sprintf("%d comparison(s) skipped", n[2]),
    "")
  out
}

# The interaction manifest is a coverage claim, so reconcile it with the
# successful blocks and completed seeds, not source declarations. A suite
# can report seeds in its successful block names (seed=N), or emit an
# IM-SEED marker after all assertions in each loop iteration.
# An incomplete file remains incomplete through the ordinary result path;
# absent cases in a file claiming PASS are a runner failure.
qa_matrix_coverage <- function(manifest, logs, statuses, suites) {
  fields <- c("case_id", "suite", "adapter", "seeds", "conditions", "oracle", "evidence_limit")
  if (!is.data.frame(manifest) || !all(fields %in% names(manifest)) || !nrow(manifest)) {
    return("interaction manifest: missing columns or no cases")
  }
  values <- manifest[fields]
  if (any(vapply(values, function(x) anyNA(x) || any(!nzchar(trimws(as.character(x)))), logical(1)))) {
    return("interaction manifest: missing or empty case fields")
  }
  if (any(!grepl("^IM[A-Z]-[0-9]{2}$", manifest$case_id))) {
    return("interaction manifest: invalid case IDs")
  }
  seed_ok <- vapply(strsplit(as.character(manifest$seeds), ";", fixed = TRUE), function(x) {
    length(x) >= 2L && !anyDuplicated(x) && all(grepl("^[1-9][0-9]*$", x))
  }, logical(1))
  if (!all(seed_ok)) return("interaction manifest: each case needs at least two distinct positive integer seeds")
  if (anyDuplicated(paste(manifest$case_id, manifest$adapter, sep = "\r"))) {
    return("interaction manifest: duplicate case/adapter rows")
  }
  if (!setequal(unique(manifest$suite), suites)) {
    return("interaction manifest: suites do not match the curated interaction lane")
  }
  ids <- unique(manifest[c("case_id", "suite")])
  if (anyDuplicated(ids$case_id)) return("interaction manifest: a case ID belongs to multiple suites")
  problems <- character()
  for (suite in suites) {
    if (!identical(unname(statuses[suite]), "PASS")) next
    ok <- grep("^ok[[:space:]]+", logs[[suite]], value = TRUE)
    for (id in unique(manifest$case_id[manifest$suite == suite])) {
      case_ok <- ok[grepl(paste0("\\b", id, "\\b"), ok)]
      if (!length(case_ok)) {
        problems <- c(problems, paste0("interaction manifest: ", suite, " claimed PASS without executing ", id))
        next
      }
      declared <- unique(manifest$seeds[manifest$suite == suite & manifest$case_id == id])
      if (length(declared) != 1L) {
        problems <- c(problems, paste0("interaction manifest: conflicting seeds for ", id))
        next
      }
      for (seed in strsplit(declared, ";", fixed = TRUE)[[1L]]) {
        named <- any(grepl(paste0("\\bseed=", seed, "([[:space:]:]|$)"), case_ok))
        marker <- any(grepl(paste0("^IM-SEED case_id=", id, " seed=", seed, "$"), logs[[suite]]))
        if (!named && !marker) {
          problems <- c(problems, paste0("interaction manifest: ", suite, " claimed PASS without executing ", id, " seed ", seed))
        }
      }
    }
  }
  problems
}

# testthat results as comparisons (codex audit F10: demo_parity.R passed a
# block that failed nothing but warned). qa_testthat_reporter() is
# testthat's summary reporter that also keeps what a test file signals
# outside any test block (a top-level warning or error, which neither the
# results data frame nor a calling handler sees). qa_record_testthat()
# records one qa_check() per block of as.data.frame(<test_file() result>):
# a block passes only when it ran expectations and none failed, errored or
# warned; a skipped block (an empty one included) is qa_skip(). A result
# with no blocks, and anything signalled outside a block, is a failure.
# `...` goes to the reporter (`file = <path>` sends its output there).
qa_testthat_reporter <- function(...) {
  cls <- R6::R6Class("QaSummaryReporter", inherit = testthat::SummaryReporter,
    public = list(
      stray = character(),
      add_result = function(context, test, result) {
        if (is.null(test)) {
          self$stray <- c(self$stray, paste0(sub("^expectation_", "", class(result)[1]), ": ",
                                             conditionMessage(result)))
        }
        super$add_result(context, test, result)
      }))
  cls$new(...)
}

qa_record_testthat <- function(df, stray = character(), label = "test file") {
  if (!nrow(df)) qa_check(paste0(label, ": no test blocks ran"), FALSE)
  for (i in seq_len(nrow(df))) {
    what <- paste(df$context[i], df$test[i], sep = ": ")
    if (isTRUE(df$skipped[i])) {
      qa_skip(what, paste("skipped by", label))
      next
    }
    qa_check(what, df$nb[i] > 0L && df$failed[i] == 0L && !isTRUE(df$error[i]) && df$warning[i] == 0L)
  }
  for (s in stray) qa_check(paste0(label, ": outside a test block: ", s), FALSE)
  invisible(NULL)
}

# Run a pure testthat QA file without exposing namespace internals. Its
# own library(tabtools) call resolves to the runner's temporary install.
qa_run_testthat <- function(path) {
  exprs <- parse(path, keep.source = FALSE)
  is_block <- function(x) {
    is.call(x) && (identical(x[[1L]], as.name("test_that")) ||
                    identical(x[[1L]], quote(testthat::test_that)))
  }
  expected <- sum(vapply(exprs, is_block, logical(1)))
  reporter <- qa_testthat_reporter()
  res <- testthat::test_file(path, reporter = reporter, load_package = "none",
                             stop_on_failure = FALSE)
  df <- as.data.frame(res)
  ran <- if (nrow(df)) sum(!is.na(df$test)) else 0L
  if (ran < expected) {
    qa_check(sprintf("%s: ran %d of %d test blocks", basename(path), ran, expected), FALSE)
  }
  empty <- vapply(res, function(x) {
    any(vapply(x$results, function(e) {
      inherits(e, "expectation_skip") && identical(conditionMessage(e), "Reason: empty test")
    }, logical(1)))
  }, logical(1))
  for (i in which(empty)) qa_check(paste0(basename(path), ": empty test: ", df$test[i]), FALSE)
  qa_record_testthat(df, reporter$stray, basename(path))
  invisible(df)
}
