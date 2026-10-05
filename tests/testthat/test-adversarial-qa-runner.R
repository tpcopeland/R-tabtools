adversarial_qa_tools <- function() {
  path <- test_path("..", "..", "qa", "tools", "qa_result.R")
  if (!file.exists(path)) skip("qa/ is excluded from the package tarball")
  env <- new.env()
  sys.source(path, envir = env)
  env
}

adversarial_qa_file <- function(lines) {
  path <- tempfile("test-adversarial-runner-", fileext = ".R")
  writeLines(lines, path)
  path
}

test_that("the adversarial QA harness reports executed blocks and preserves skips", {
  q <- adversarial_qa_tools()
  path <- adversarial_qa_file(c(
    'test_that("one", { expect_identical(2L + 2L, 4L) })',
    'test_that("two", { expect_identical(3L + 3L, 6L) })'))
  withr::defer(unlink(path))
  capture.output(df <- q$qa_run_testthat(path))
  expect_identical(nrow(df), 2L)
  expect_identical(q$qa_state$executed, 2L)
  expect_length(q$qa_state$failed, 0L)
  expect_length(q$qa_state$skipped, 0L)

  q <- adversarial_qa_tools()
  writeLines(c('test_that("one", { expect_true(TRUE) })',
               'test_that("optional", { skip("oracle unavailable") })'), path)
  capture.output(q$qa_run_testthat(path))
  expect_identical(q$qa_state$executed, 1L)
  expect_length(q$qa_state$failed, 0L)
  expect_length(q$qa_state$skipped, 1L)
  expect_identical(q$qa_status(1L, 1L, 0L), "INCOMPLETE")
})

test_that("an early return cannot silently truncate adversarial QA", {
  q <- adversarial_qa_tools()
  path <- adversarial_qa_file(c(
    'test_that("before", { expect_true(TRUE) })',
    'return()',
    'test_that("after", { expect_true(TRUE) })'))
  withr::defer(unlink(path))
  capture.output(q$qa_run_testthat(path))
  expect_length(q$qa_state$failed, 1L)
  expect_match(q$qa_state$failed[[1L]], "ran 1 of 2 test blocks", fixed = TRUE)
})

test_that("empty tests and expectations outside blocks cannot make QA green", {
  q <- adversarial_qa_tools()
  path <- adversarial_qa_file(c(
    'expect_true(TRUE)',
    'test_that("empty", {})',
    'test_that("real", { expect_true(TRUE) })'))
  withr::defer(unlink(path))
  capture.output(q$qa_run_testthat(path))
  expect_length(q$qa_state$failed, 2L)
  expect_match(q$qa_state$failed[[1L]], "empty test", fixed = TRUE)
  expect_match(q$qa_state$failed[[2L]], "outside a test block: success", fixed = TRUE)
  expect_identical(q$qa_status(q$qa_state$executed, length(q$qa_state$skipped),
                             length(q$qa_state$failed)), "FAIL")
})

test_that("file-level skips and top-level errors fail adversarial QA", {
  path <- adversarial_qa_file('skip("file-level gate")')
  withr::defer(unlink(path))
  q <- adversarial_qa_tools()
  capture.output(q$qa_run_testthat(path))
  expect_gte(length(q$qa_state$failed), 1L)
  expect_match(paste(q$qa_state$failed, collapse = " "), "no test blocks ran", fixed = TRUE)

  q <- adversarial_qa_tools()
  writeLines(c('stop("missing fixture")',
               'test_that("never run", { expect_true(TRUE) })'), path)
  capture.output(q$qa_run_testthat(path))
  expect_gte(length(q$qa_state$failed), 1L)
  expect_match(paste(q$qa_state$failed, collapse = " "), "ran 0 of 1 test blocks", fixed = TRUE)
})

test_that("the runner executes legacy scripts and detects early exit and timeout", {
  skip_if_not_installed("callr")
  q <- adversarial_qa_tools()
  runner <- test_path("..", "..", "qa", "run_all.R")
  exprs <- parse(runner, keep.source = FALSE)
  is_runner <- vapply(exprs, function(x) {
    is.call(x) && identical(x[[1L]], as.name("<-")) &&
      identical(x[[2L]], as.name("run_script"))
  }, logical(1))
  expect_identical(sum(is_runner), 1L)
  env <- new.env()
  env$root <- tempdir()
  env$qa_dir <- normalizePath(dirname(runner))
  env$lib <- .libPaths()[1L]
  env$timeout_seconds <- 10L
  eval(exprs[[which(is_runner)]], envir = env)
  path <- adversarial_qa_file('cat("QA-RESULT script=x.R status=PASS executed=1 skipped=0 failed=0\\n")')
  log <- tempfile(fileext = ".log")
  withr::defer(unlink(c(path, log)))
  run <- env$run_script(path, FALSE, log)
  expect_identical(run$status, 0L)
  expect_identical(q$qa_reconcile("x.R", run$status, readLines(log))$status, "PASS")

  writeLines('quit(save = "no", status = 0L)', path)
  run <- env$run_script(path, FALSE, log)
  expect_identical(run$status, 0L)
  expect_identical(q$qa_reconcile("x.R", run$status, readLines(log))$status, "FAIL")

  env$timeout_seconds <- 1L
  writeLines('Sys.sleep(5)', path)
  run <- env$run_script(path, FALSE, log)
  expect_identical(run$status, 1L)
  expect_identical(run$note, "timeout-after-1s")
})
