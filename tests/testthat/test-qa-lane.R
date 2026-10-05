# The QA runner's result reconciliation (Codex audit CX-6): a script that
# ran no comparison, or skipped some, never counts as a complete PASS, and a
# script that stops before its completion marker fails. Source-tree only:
# qa/ is .Rbuildignored, so R CMD check skips these.

qa_tools <- function() {
  f <- test_path("..", "..", "qa", "tools", "qa_result.R")
  if (!file.exists(f)) skip("qa/ is not in this tree (installed package)")
  env <- new.env()
  sys.source(f, envir = env)
  env
}

test_that("CX-6: qa_reconcile() recomputes the status from the marker's counts", {
  q <- qa_tools()
  mk <- function(st, ex, sk, fa, s = "x.R") {
    paste0("QA-RESULT script=", s, " status=", st, " executed=", ex, " skipped=", sk, " failed=", fa)
  }
  r <- function(...) q$qa_reconcile("x.R", ...)
  expect_identical(r(0L, c("ok   a", mk("PASS", 3L, 0L, 0L)))$status, "PASS")
  expect_identical(r(0L, mk("INCOMPLETE", 3L, 1L, 0L))$status, "INCOMPLETE")
  expect_identical(r(0L, mk("SKIP", 0L, 2L, 0L))$status, "SKIP")
  # A marker claiming PASS for zero comparisons is not believed.
  expect_identical(r(0L, mk("PASS", 0L, 2L, 0L))$status, "FAIL")
  expect_identical(r(0L, mk("PASS", 0L, 0L, 0L))$status, "FAIL")
  # Failures, a non-zero exit, no marker, two markers, another script's.
  expect_identical(r(0L, mk("FAIL", 3L, 0L, 1L))$status, "FAIL")
  expect_identical(r(1L, mk("PASS", 3L, 0L, 0L))$status, "FAIL")
  expect_identical(r(0L, c("ok   a", "wttab validation passed"))$status, "FAIL")
  expect_match(r(0L, "ok   a")$note, "no completion marker")
  expect_identical(r(0L, rep(mk("PASS", 1L, 0L, 0L), 2))$status, "FAIL")
  expect_identical(r(0L, mk("PASS", 1L, 0L, 0L, s = "y.R"))$status, "FAIL")
  # The counts come back for the report.
  v <- r(0L, mk("INCOMPLETE", 7L, 1L, 0L))
  expect_identical(c(v$executed, v$skipped, v$failed), c(7L, 1L, 0L))
})

test_that("CX-6: validation_wttab.R without its comparators is SKIP or INCOMPLETE, never PASS", {
  skip_on_cran()
  q <- qa_tools()
  script <- normalizePath(test_path("..", "..", "qa", "validation_wttab.R"))
  root <- normalizePath(test_path("..", ".."))
  run <- function(hide) {
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                                    stdout = TRUE, stderr = TRUE,
                                    env = c(paste0("TABTOOLS_QA_HIDE=", hide), "TABTOOLS_QA_LIB=")))
    st <- attr(out, "status") %||% 0L
    q$qa_reconcile("validation_wttab.R", as.integer(st), out)
  }
  withr::local_dir(root)
  both <- run("ipw,WeightIt")
  expect_identical(both$status, "SKIP")
  expect_identical(both$executed, 0L)
  # (ipw is not in Suggests: looked up by a variable, so R CMD check does
  # not take it for an undeclared dependency; qa/ is not in the tarball.)
  comparators <- c("ipw", "WeightIt")
  have <- vapply(comparators, function(p) nzchar(system.file(package = p)), TRUE)
  if (have[["WeightIt"]]) {
    expect_identical(run("ipw")$status, "INCOMPLETE")
  }
  if (have[["ipw"]]) {
    expect_identical(run("WeightIt")$status, "INCOMPLETE")
  }
  if (all(have)) {
    full <- run("")
    expect_identical(full$status, "PASS")
    expect_gt(full$executed, 0L)
    expect_identical(full$skipped, 0L)
  }
})

test_that("F10: a testthat block that warns, or a warning outside any block, is not a QA pass", {
  q <- qa_tools()
  child <- withr::local_tempfile(pattern = "test-", fileext = ".R")
  writeLines(c('warning("file-level warning")',
               'test_that("warns", { expect_true(TRUE); warning("injected warning") })',
               'test_that("clean", { expect_true(TRUE) })'), child)
  # The child's summary goes to a temporary file, not the fast lane's
  # console (review of 2026-09-28, M6).
  rep <- q$qa_testthat_reporter(file = withr::local_tempfile(fileext = ".txt"))
  res <- NULL
  utils::capture.output(res <- testthat::test_file(child, reporter = rep, stop_on_failure = FALSE,
                                                   load_package = "none"))
  df <- as.data.frame(res)
  expect_identical(df$warning, c(1L, 0L))
  expect_length(rep$stray, 1L)
  capture.output(q$qa_record_testthat(df, rep$stray, "child"))
  expect_identical(q$qa_state$executed, 3L)
  expect_length(q$qa_state$failed, 2L)
  expect_match(q$qa_state$failed[1], ": warns$")
  expect_identical(q$qa_state$failed[2], "child: outside a test block: warning: file-level warning")
  # No blocks at all is a failure, never a pass.
  q2 <- qa_tools()
  capture.output(q2$qa_record_testthat(df[0, ], character(), "empty"))
  expect_identical(q2$qa_status(q2$qa_state$executed, length(q2$qa_state$skipped), length(q2$qa_state$failed)),
                   "FAIL")
})
