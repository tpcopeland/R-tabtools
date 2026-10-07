post251_stat_messages <- function(code) {
  messages <- character()
  value <- withCallingHandlers(code, message = function(m) {
    messages <<- c(messages, gsub("[[:space:]]+", " ", cli::ansi_strip(conditionMessage(m))))
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = messages)
}

test_that("unavailable built-in tokens are identified without changing row or scalar policy", {
  d <- data.frame(x = 1:8, y = c(1, 3, 2, 6, 4, 9, 8, 10))
  fit <- lm(y ~ x, d)
  result <- post251_stat_messages(regtab(fit, stats = c("n", "events", "groups", "mi_m")))
  expect_true(any(grepl("no model reports 3 requested statistic(s), left out of the table: events groups mi_m",
    result$messages, fixed = TRUE)))
  expect_identical(utils::tail(result$value$body[[1L]], 1L), "Observations")
  expect_identical(result$value$stored$n_1, 8)
  expect_false(any(c("events_1", "groups_1", "mi_m_1") %in% names(result$value$stored)))
  expect_error(regtab(fit, stats = list(list(name = "missing_scalar"))),
    class = "tabtools_error_statspec")
})

test_that("QIC fallback satisfies requested AIC and QIC without false omission notes", {
  want <- .rt_parse_stats(c("aic", "qic"))
  stats <- list(list(aic = NA_real_, qic = 12.5))
  rows <- .rt_stats_rows(want, stats)$rows
  messages <- post251_stat_messages(.rt_builtin_stat_notes(want, rows, stats, list(lm(mpg ~ wt, mtcars))))
  expect_identical(messages$messages, character())
  expect_identical(length(rows), 1L)
  expect_identical(rows[[1L]]$label, "QICu")
  expect_identical(unname(rows[[1L]]$raw_values[[1L]]), 12.5)
})

test_that("retained clogit grouping terms explain missing group counts without recounting", {
  skip_if_not_installed("survival")
  d <- data.frame(case = rep(c(1, 0), 8), set = rep(1:8, each = 2),
    x = c(2, 1, 3, 2, 1, 3, 4, 2, 2, 4, 1, 2, 3, 1, 2, 3))
  fit <- local({
    coxph <- survival::coxph; Surv <- survival::Surv
    survival::clogit(case ~ x + survival::strata(set), data = d)
  })
  result <- post251_stat_messages(regtab(fit, stats = c("n", "groups")))
  expect_true(any(grepl("stats(groups) left out: no model stores a group count", result$messages, fixed = TRUE)))
  expect_true(any(grepl("model 1 (clogit, grouping term strata(set))", result$messages, fixed = TRUE)))
  expect_true(any(grepl("tt_fitcount()", result$messages, fixed = TRUE)))
  expect_false(any(grepl("left out of the table: groups", result$messages, fixed = TRUE)))
  expect_identical(result$value$stored$n_1, 16)
  expect_false("groups_1" %in% names(result$value$stored))
  expect_false("Groups" %in% result$value$body[[1L]])
  # Retained field names are literal text, never cli expressions.
  d[["{missing_column_name}"]] <- d$set
  literal_fit <- local({
    coxph <- survival::coxph; Surv <- survival::Surv
    survival::clogit(case ~ x + survival::strata(`{missing_column_name}`), data = d)
  })
  before_literal <- literal_fit
  literal <- post251_stat_messages(regtab(literal_fit, stats = c("n", "groups")))
  expect_true(any(grepl("strata(`{missing_column_name}`)", literal$messages, fixed = TRUE)))
  expect_true(any(grepl("no model stores a group count", literal$messages, fixed = TRUE)))
  expect_identical(literal$value$stored$n_1, 16)
  expect_false("groups_1" %in% names(literal$value$stored))
  expect_identical(literal_fit, before_literal)
})

test_that("one reported group count keeps its row and identifies only the blank clogit model", {
  want <- .rt_parse_stats("groups")
  grouped <- structure(list(terms = terms(y ~ x + strata(set))), class = "clogit")
  fits <- list(grouped, lm(mpg ~ wt, mtcars))
  stats <- list(list(groups = NA_real_), list(groups = 7))
  rows <- .rt_stats_rows(want, stats)$rows
  result <- post251_stat_messages(.rt_builtin_stat_notes(want, rows, stats, fits))
  expect_identical(rows[[1L]]$values, c("", "7"))
  expect_true(any(grepl("stats(groups) is blank for model(s) 1", result$messages, fixed = TRUE)))
  expect_false(any(grepl("left out", result$messages, fixed = TRUE)))
  expect_identical(stats, list(list(groups = NA_real_), list(groups = 7)))
})
