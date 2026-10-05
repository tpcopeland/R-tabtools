matrix_runner_tools <- function() {
  path <- test_path("..", "..", "qa", "tools", "qa_result.R")
  if (!file.exists(path)) skip("QA runner is excluded from the installed package")
  env <- new.env()
  sys.source(path, envir = env)
  env
}

test_that("matrix coverage requires successful executed cases in files claiming PASS", {
  q <- matrix_runner_tools()
  m <- data.frame(case_id = c("IML-01", "IML-01", "IMS-01"),
                  suite = c("a.R", "a.R", "b.R"), adapter = c("lm", "glm", "coxph"),
                  seeds = "4101;9917", conditions = "missingness;zero weights",
                  oracle = "native covariance and raw masks", evidence_limit = "none")
  logs <- list(a.R = c("ok   : IML-01 seed=4101 combined conditions", "ok   : IML-01 seed=9917 combined conditions"),
               b.R = c("IM-SEED case_id=IMS-01 seed=4101", "IM-SEED case_id=IMS-01 seed=9917",
                       "ok   : IMS-01 combined conditions"))
  statuses <- c(a.R = "PASS", b.R = "PASS")
  check <- function(manifest = m, output = logs, status = statuses, suites = c("a.R", "b.R")) {
    q$qa_matrix_coverage(manifest, output, status, suites)
  }
  expect_length(check(), 0)
  unseeded <- logs; unseeded$a.R <- "ok   : IML-01 combined conditions"
  expect_length(check(output = unseeded), 2)
  unseeded$a.R <- "ok   : IML-01 seed=4101 combined conditions"
  expect_match(check(output = unseeded), "IML-01 seed 9917")
  unseeded$a.R <- "ok   : IML-01 seed=99170 combined conditions"
  expect_length(check(output = unseeded), 2)
  for (suffix in c(".5", "-9")) {
    unseeded$a.R <- paste0("ok   : IML-01 seed=", c(4101, 9917), suffix)
    expect_length(check(output = unseeded), 2)
  }
  unseeded <- logs; unseeded$b.R <- c("IM-SEED case_id=IMS-010 seed=4101", "ok   : IMS-01 combined conditions")
  expect_length(check(output = unseeded), 2)
  bad <- m; bad$seeds[2] <- "4101;7301"
  expect_match(check(manifest = bad), "conflicting seeds for IML-01")
  wrong <- logs; wrong$a.R <- "ok   : IML-010 is a different case"
  expect_match(check(output = wrong), "without executing IML-01")
  wrong$a.R <- "SKIP : IML-01 missing comparator"
  expect_match(check(output = wrong), "without executing IML-01")
  wrong$a.R <- "FAIL : IML-01 numeric mismatch"
  expect_match(check(output = wrong), "without executing IML-01")
  expect_length(check(output = wrong, status = c(a.R = "INCOMPLETE", b.R = "PASS")), 0)
  expect_match(check(manifest = m[FALSE, ]), "no cases")
  expect_match(check(manifest = m[-1]), "missing columns")
  expect_match(check(manifest = rbind(m, m[1, ])), "duplicate case/adapter")
  bad <- m; bad$oracle[1] <- ""
  expect_match(check(manifest = bad), "empty case fields")
  bad <- m; bad$oracle[1] <- NA_character_
  expect_match(check(manifest = bad), "missing or empty")
  bad <- m; bad$case_id[1] <- "IML-01|.*"
  expect_match(check(manifest = bad), "invalid case IDs")
  for (seeds in c("4101", "4101;4101", "0;9917", "4101;NA")) {
    bad <- m; bad$seeds[1] <- seeds
    expect_match(check(manifest = bad), "two distinct positive integer seeds")
  }
  expect_match(check(suites = c("a.R", "b.R", "missing.R")), "curated interaction lane")
  bad <- m; bad$case_id[3] <- "IML-01"
  expect_match(check(manifest = bad), "multiple suites")
})
