# Regressions for the documentation review of 2026-09-27 (R-Dev
# _take_action/discovered_errors.md, D03 and D04). Each test failed on the
# reviewed working tree.

# D03 ----

d03_rates <- function(cats = c("Unexposed", "Exposed")) {
  stratetab(ct_block(cats, c(5, 8), c(100, 200)), outlabels = "Death", outcomeids = "death",
            explabels = "Trt")
}

d03_model <- function(levels = c("Unexposed", "Exposed")) {
  regtab(ct_model("death", levels = levels))
}

test_that("D03: a pattern that selects too many rows is named in hrcomptab()'s error", {
  r <- d03_rates()
  m <- d03_model()
  err <- tryCatch(hrcomptab(r, m, rownames = "exposed"), error = function(e) e)
  expect_s3_class(err, "tabtools_error_rownames_count")
  msg <- conditionMessage(err)
  expect_match(msg, "\"exposed\"", fixed = TRUE)
  expect_match(msg, "Unexposed", fixed = TRUE)
  expect_match(msg, "rownames_exact", fixed = TRUE)
  expect_match(msg, "rows", fixed = TRUE)
})

test_that("D03: rownames_exact = TRUE matches whole labels, case-insensitively", {
  r <- d03_rates()
  m <- d03_model()
  a <- hrcomptab(r, m, rownames = "exposed", rownames_exact = TRUE)
  b <- hrcomptab(r, m, rows = 4)
  expect_identical(a$body, b$body)
  # Male/Female: "male" alone selects Female too (the "Treated" row 1 is a
  # different term).
  rs <- d03_rates(c("Male", "Female"))
  ms <- d03_model(c("Male", "Female"))
  expect_error(hrcomptab(rs, ms, rownames = "male"), class = "tabtools_error_rownames_count")
  expect_identical(hrcomptab(rs, ms, rownames = "FEMALE", rownames_exact = TRUE)$body,
                   hrcomptab(rs, ms, rows = 4)$body)
  # Wildcards still apply, anchored to the whole label.
  expect_identical(hrcomptab(r, m, rownames = "ex*ed", rownames_exact = TRUE)$body, b$body)
  # A pattern that is only part of a label no longer matches.
  expect_error(hrcomptab(r, m, rownames = "expos", rownames_exact = TRUE), "not found")
})

test_that("D03: rownames_exact works in vertical mode and defaults to Stata's substring match", {
  m <- d03_model()
  sub <- comptab(m, rownames = "exposed")
  ex <- comptab(m, rownames = "exposed", rownames_exact = TRUE)
  expect_identical(ex$body, comptab(m, rows = 4)$body)
  expect_gt(nrow(sub$body), nrow(ex$body))
  expect_error(comptab(m, rownames = "exposed", rownames_exact = NA), "TRUE or FALSE")
  expect_error(comptab(m, rows = 4, rownames_exact = TRUE), "rownames")
  # Name-only: the Stata-order positional arguments are unchanged.
  expect_identical(names(formals(hrcomptab))[1:7], names(formals(comptab))[1:7])
  expect_identical(utils::tail(names(formals(comptab)), 6),
                   c("rownames_exact", "cformat", "cisep", "allmodels", "keyed", "modelonly"))
  expect_identical(utils::tail(names(formals(hrcomptab)), 1), "rownames_exact")
})

# D04 ----

test_that("D04: the Cox Efron-ties note leads with R, then gives Stata's refit", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$dead <- d$status - 1
  f <- survival::coxph(survival::Surv(time, dead) ~ sex, data = d)
  withr::local_options(rlib_message_verbosity = "verbose")
  msgs <- testthat::capture_messages(regtab(f))
  msg <- paste(msgs, collapse = "")
  expect_match(msg, "R's default", fixed = TRUE)
  expect_match(msg, "estimates are valid", fixed = TRUE)
  expect_match(msg, "ties = \"breslow\"", fixed = TRUE)
  # The first line says nothing about Stata; Stata comes second.
  first <- strsplit(msg, "\n", fixed = TRUE)[[1]][1]
  expect_false(grepl("Stata", first, fixed = TRUE))

  fw <- survival::coxph(survival::Surv(time, dead) ~ sex, data = d, weights = rep(c(1, 2), length.out = nrow(d)))
  msgw <- paste(testthat::capture_messages(suppressWarnings(regtab(fw))), collapse = "")
  expect_match(msgw, "estimates are valid", fixed = TRUE)
  expect_match(msgw, "no Stata analogue", fixed = TRUE)
  expect_false(grepl("Stata", strsplit(msgw, "\n", fixed = TRUE)[[1]][1], fixed = TRUE))
})
