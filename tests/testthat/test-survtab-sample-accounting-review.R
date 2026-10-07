test_that("survival table accounting distinguishes intervals and replicated subjects", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL))
  d <- data.frame(id = c("a", "b", "b", "c", "d", "e", "f"),
    entry = c(0, 0, 1, 0, 0, 0, 0), exit = c(1, 1, 3, 4, NA, 2, 2),
    event = c(1, 0, NA, 0, 1, 1, 0), g = c(1, 2, 2, 1, 1, NA, 2),
    w = c(2, 3, 3, 1, 1, 1, 0))
  original <- d
  prepared <- .sv_prepare(d, "exit", "event", "g", "entry", "id", "w")
  ledger <- prepared$sample
  expect_identical(ledger$populations$id, "survtab:included")
  expect_identical(ledger$populations$scope, "table")
  expect_identical(ledger$populations$command, "survtab")
  expect_identical(ledger$populations$weight_type, "frequency")
  # Four retained intervals describe three subjects with frequencies 2, 3, 1.
  # One included event is missing; three source records fail eligibility.
  metrics <- c("input_n", "eligible_n", "observed_n", "used_n", "missing_n",
               "excluded_n", "reported_n")
  selected <- ledger$measures[match(metrics, ledger$measures$metric), ]
  expect_identical(selected$value, c(7, 4, 3, 4, 1, 3, 6))
  expect_identical(selected$status, rep("available", 7L))
  expect_identical(selected$basis[c(4L, 7L)],
    c("included intervals including missing-event censoring", "unique included subjects with frequency replication"))
  expect_identical(prepared$N, c(3, 3))
  expect_identical(prepared$records$record_index, 1:4)
  expect_identical(prepared$missing_event_rows, 3L)
  expect_identical(ledger$exclusions$reason,
                   c("missing_time", "missing_group", "zero_frequency"))
  expect_identical(ledger$exclusions$stage, rep("input_to_eligible", 3L))
  expect_identical(ledger$exclusions$n, c(1, 1, 1))
  tt <- survtab(d, "exit", "event", c(1, 2), by = "g", entry = "entry", id = "id", fweight = "w")
  expect_identical(tt$meta$sample_accounting, ledger)
  expect_identical(attr(as.data.frame(tt), "sample_accounting", exact = TRUE), ledger)
  expect_identical(d, original)
  invalid <- ledger
  invalid$populations$scope <- "included intervals"
  expect_error(.tt_validate_sample_accounting(invalid), class = "tabtools_error_sample_accounting")
  invalid <- ledger
  invalid$exclusions$stage <- "eligibility"
  expect_error(.tt_validate_sample_accounting(invalid), class = "tabtools_error_sample_accounting")
})
