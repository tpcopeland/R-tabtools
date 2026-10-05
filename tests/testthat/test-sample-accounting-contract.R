# Public construction and validation of the documented source ledger.

sample_contract_ledger <- function(values = list()) {
  metrics <- c("input_n", "eligible_n", "observed_n", "used_n", "fitted_n",
               "frame_n", "missing_n", "zero_weight_n", "excluded_n",
               "weight_sum", "effective_n", "reported_n")
  supplied <- metrics %in% names(values)
  numbers <- vapply(metrics, function(metric) {
    if (metric %in% names(values)) as.numeric(values[[metric]]) else NA_real_
  }, 0)
  status <- ifelse(!supplied, "not_applicable",
                   ifelse(is.na(numbers), "unavailable", "available"))
  list(
    version = 1L,
    populations = data.frame(id = "cohort", component = "", command = "custom",
      scope = "table", variable = NA_character_, group = NA_character_,
      model = NA_real_, imputation = NA_real_, spec = NA_real_,
      weight_type = "frequency", stringsAsFactors = FALSE),
    measures = data.frame(population_id = "cohort", metric = metrics,
      value = unname(numbers), status = status, basis = "independent input count",
      reason = ifelse(status == "available", "", "not identifiable in this example"),
      stringsAsFactors = FALSE),
    exclusions = data.frame(population_id = character(), stage = character(),
      reason = character(), n = numeric(), status = character(), basis = character(),
      stringsAsFactors = FALSE)
  )
}

sample_contract_table <- function(ledger = NULL) {
  tt_table(body = data.frame(label = "Result", value = "3"),
    header = list(c("Label", "Value")), command = "custom",
    rows = data.frame(type = "var", key = "result"),
    meta = if (is.null(ledger)) list() else list(sample_accounting = ledger))
}

sample_contract_set <- function(ledger, metric, value, status = "available") {
  at <- match(metric, ledger$measures$metric)
  ledger$measures$value[at] <- value
  ledger$measures$status[at] <- status
  ledger$measures$reason[at] <- if (status == "available") "" else "not identifiable"
  ledger
}

test_that("legacy tables remain valid without invented accounting", {
  tab <- sample_contract_table()
  expect_identical(validate_tt_table(tab), tab)
  expect_identical(tab$body, data.frame(c1 = "Result", c2 = "3"))
  expect_null(tab$meta[["sample_accounting", exact = TRUE]])
  expect_null(attr(as.data.frame(tab), "sample_accounting", exact = TRUE))
})

test_that("known records, included missingness and native weighted N stay distinct", {
  # Four input zeros can precede shared eligibility; two retained missing
  # categories belong in used_n although only one value is observed.
  ledger <- sample_contract_ledger(list(input_n = 7, eligible_n = 3,
    observed_n = 1, used_n = 3, missing_n = 2, zero_weight_n = 4,
    excluded_n = 4, weight_sum = 8.5, effective_n = 1.8, reported_n = 8.5))
  ledger$exclusions <- data.frame(population_id = "cohort", stage = "retained",
    reason = "missing category included", n = 2, status = "available",
    basis = "explicit missing-category policy", stringsAsFactors = FALSE)
  tab <- sample_contract_table(ledger)
  expect_identical(validate_tt_table(tab), tab)
  expect_identical(tab$meta[["sample_accounting", exact = TRUE]], ledger)
  expect_identical(attr(as.data.frame(tab), "sample_accounting", exact = TRUE), ledger)
  expect_identical(tab$body, sample_contract_table()$body)
})

test_that("true zero and unavailable values have different public meanings", {
  ledger <- sample_contract_ledger(list(input_n = 0, eligible_n = 0,
    used_n = 0, excluded_n = 0, weight_sum = 0, effective_n = NA_real_,
    reported_n = NA_real_))
  tab <- sample_contract_table(ledger)
  m <- tab$meta$sample_accounting$measures
  expect_identical(m$value[m$metric == "used_n"], 0)
  expect_identical(m$status[m$metric == "used_n"], "available")
  expect_identical(m$value[m$metric == "reported_n"], NA_real_)
  expect_identical(m$status[m$metric == "reported_n"], "unavailable")
  expect_identical(m$status[m$metric == "fitted_n"], "not_applicable")
  expect_identical(validate_tt_table(tab), tab)
})

test_that("schema versions, identity and context indices are validated publicly", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = 3, used_n = 3))
  bad <- list()
  for (version in list(1, 2L, NA_integer_, "1")) {
    s <- base; s$version <- version; bad[[length(bad) + 1L]] <- s
  }
  s <- base; s$extra <- TRUE; bad[[length(bad) + 1L]] <- s
  s <- base; s$populations <- s$populations[0, ]; bad[[length(bad) + 1L]] <- s
  s <- base; s$populations <- rbind(s$populations, s$populations); bad[[length(bad) + 1L]] <- s
  for (field in c("id", "command", "weight_type")) {
    s <- base; s$populations[[field]] <- ""; bad[[length(bad) + 1L]] <- s
  }
  s <- base; s$populations$scope <- "cohort"; bad[[length(bad) + 1L]] <- s
  for (field in c("model", "imputation", "spec")) {
    for (index in list(0, -1, 1.5, Inf, NaN, "1", matrix(1, 1, 1), 1 + 0i)) {
      s <- base; s$populations[[field]] <- index; bad[[length(bad) + 1L]] <- s
    }
  }
  for (s in bad) expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
})

test_that("every population needs exactly one complete metric set", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = 3, used_n = 3))
  bad <- list()
  s <- base; s$measures <- s$measures[-1, ]; bad[[1]] <- s
  s <- base; s$measures[2, ] <- s$measures[1, ]; bad[[2]] <- s
  s <- base; s$measures$population_id[1] <- "absent"; bad[[3]] <- s
  s <- base; s$measures$metric[1] <- "table_rows"; bad[[4]] <- s
  s <- base; s$measures <- rbind(s$measures, s$measures[1, ]); bad[[5]] <- s
  s <- base; s$measures$value <- as.character(s$measures$value); bad[[6]] <- s
  s <- base; s$measures$value <- matrix(s$measures$value, ncol = 1); bad[[7]] <- s
  s <- base; s$measures$value <- as.complex(s$measures$value); bad[[8]] <- s
  for (s in bad) expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
})

test_that("available exact records reject invalid numeric counts", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = 3, used_n = 3))
  for (metric in c("input_n", "eligible_n", "observed_n", "used_n", "fitted_n",
                    "frame_n", "missing_n", "zero_weight_n", "excluded_n")) {
    for (value in c(-1, .5, Inf, -Inf, NA_real_, NaN)) {
      s <- sample_contract_set(base, metric, value)
      expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
    }
  }
  # Native weighted/trial quantities and Kish ESS are deliberately fractional.
  good <- sample_contract_set(base, "reported_n", 100.25)
  good <- sample_contract_set(good, "weight_sum", .75)
  good <- sample_contract_set(good, "effective_n", 2.1)
  expect_identical(sample_contract_table(good)$meta$sample_accounting, good)
})

test_that("unknown values require NA and explanatory evidence", {
  base <- sample_contract_ledger(list(input_n = NA_real_))
  for (status in c("unavailable", "not_applicable")) {
    for (value in c(0, 1, Inf, NaN)) {
      s <- sample_contract_set(base, "input_n", value, status)
      expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
    }
    for (field in c("reason", "basis")) {
      s <- sample_contract_set(base, "input_n", NA_real_, status)
      s$measures[[field]][1] <- ""
      expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
    }
  }
  for (status in c("unknown", "", NA_character_)) {
    s <- base; s$measures$status[1] <- status
    expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  }
})

test_that("universal scoped bounds survive unknown intermediate counts", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = NA_real_,
    used_n = NA_real_, observed_n = 0, fitted_n = 0, missing_n = 0,
    zero_weight_n = 0, excluded_n = 0))
  expect_identical(sample_contract_table(base)$meta$sample_accounting, base)
  for (metric in c("eligible_n", "observed_n", "used_n", "fitted_n",
                    "missing_n", "zero_weight_n", "excluded_n")) {
    s <- sample_contract_set(base, metric, 4)
    expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  }
  known <- sample_contract_ledger(list(input_n = 7, eligible_n = 3,
    used_n = 2, observed_n = 1, fitted_n = 2, excluded_n = 5))
  for (metric in c("used_n", "observed_n", "fitted_n")) {
    s <- sample_contract_set(known, metric, 4)
    expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  }
  s <- sample_contract_set(known, "excluded_n", 4)
  expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  # A stored expanded representation and native N are not raw-record bounds.
  expanded <- sample_contract_set(known, "frame_n", 100)
  expanded <- sample_contract_set(expanded, "reported_n", 50.5)
  expect_identical(sample_contract_table(expanded)$meta$sample_accounting, expanded)
})

test_that("exclusion rows require valid identities, stages and count evidence", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = 2, used_n = 2, excluded_n = 1))
  base$exclusions <- data.frame(population_id = "cohort", stage = "input_to_eligible",
    reason = "missing group", n = 1, status = "available", basis = "independent mask",
    stringsAsFactors = FALSE)
  expect_identical(sample_contract_table(base)$meta$sample_accounting, base)
  for (field in c("population_id", "stage", "status", "basis", "reason")) {
    s <- base; s$exclusions[[field]] <- ""
    expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  }
  for (n in c(-1, .5, Inf, NaN, NA_real_)) {
    s <- base; s$exclusions$n <- n
    expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  }
  s <- base; s$exclusions$status <- "unavailable"
  expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
  s$exclusions$n <- NA_real_
  expect_identical(sample_contract_table(s)$meta$sample_accounting, s)
})

test_that("matrix text columns cannot masquerade as scalar ledger context", {
  base <- sample_contract_ledger(list(input_n = 3, eligible_n = 3, used_n = 3))
  base$exclusions <- data.frame(population_id = "cohort", stage = "retained",
    reason = "included missing", n = 1, status = "available", basis = "explicit policy",
    stringsAsFactors = FALSE)
  for (section in c("populations", "measures", "exclusions")) {
    text <- names(base[[section]])[vapply(base[[section]], is.character, TRUE)]
    for (field in text) {
      s <- base; s[[section]][[field]] <- matrix(s[[section]][[field]], ncol = 1)
      expect_error(sample_contract_table(s), class = "tabtools_error_sample_accounting")
    }
  }
})

test_that("public validation and conversion reject a subsequently corrupt ledger", {
  tab <- sample_contract_table(sample_contract_ledger(list(input_n = 3, used_n = 3)))
  tab$meta$sample_accounting$measures$value[1] <- -1
  expect_error(validate_tt_table(tab), class = "tabtools_error_sample_accounting")
  expect_error(as.data.frame(tab), class = "tabtools_error_sample_accounting")
})
