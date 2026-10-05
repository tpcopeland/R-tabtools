test_that("tt_merge keeps the first table's label headers", {
  a <- tt_table(data.frame(label = c("A", "B"), value = c("1", "2")),
                header = list(c("Characteristic", "Value")),
                rows = data.frame(key = c("a", "b")), command = "custom")
  merged <- tt_merge(a, a)
  expect_identical(merged$header[[1]]$text, c("Characteristic", "Value", "Value"))
  csv <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(merged, csv)
  expect_identical(readLines(csv)[1], "Characteristic,Value,Value")
  b <- a
  b$header <- c(list(list(text = c("", "Extra"))), b$header)
  merged <- tt_merge(a, b)
  expect_identical(vapply(merged$header, function(h) h$text[1], ""), c("", "Characteristic"))
})

test_that("gtsummary reference styling selects rows even without unique keys", {
  skip_if_not_installed("gtsummary")
  for (keys in list(c(NA_character_, NA_character_), c("same", "same"))) {
    a <- tt_table(data.frame(label = c("reference", "ordinary"), value = c("Reference", "123")),
                  header = list(c("Characteristic", "Value")),
                  rows = data.frame(type = c("ref", "var"), key = keys), command = "custom")
    g <- tt_as_gtsummary(a)
    md <- paste(as.character(gtsummary::as_kable(g)), collapse = "\n")
    expect_match(md, "_Reference_", fixed = TRUE)
    expect_no_match(md, "_123_", fixed = TRUE)
    b <- a
    b$rows$type <- rev(b$rows$type)
    b$body <- b$body[2:1, , drop = FALSE]
    stacked <- suppressMessages(gtsummary::tbl_stack(list(g, tt_as_gtsummary(b))))
    md <- paste(as.character(gtsummary::as_kable(stacked)), collapse = "\n")
    expect_identical(lengths(regmatches(md, gregexpr("_Reference_", md, fixed = TRUE))), 2L)
    expect_no_match(md, "_123_", fixed = TRUE)
  }
})

test_that("table validation rejects row indentation and atomic metadata before rendering", {
  a <- tt_table(data.frame(label = c("A", "B"), value = c("1", "2")),
                header = list(c("Characteristic", "Value")), command = "custom")
  for (indent in list(c(NA, 0), c(-1, 0), c(0.5, 0), c("one", "two"))) {
    bad <- a
    bad$rows$indent <- indent
    expect_error(validate_tt_table(bad), "rows$indent", fixed = TRUE)
  }
  bad <- a
  bad$meta <- 1
  expect_error(validate_tt_table(bad), "meta", fixed = TRUE)
})

test_that("table validation reports malformed containers at the public boundary", {
  expect_error(validate_tt_table(1), "Invalid <tt_table>", fixed = TRUE)
  a <- tt_table(data.frame(label = "A", value = "1"),
                header = list(c("Characteristic", "Value")), command = "custom")
  a$layout <- 1
  expect_error(validate_tt_table(a), "layout", fixed = TRUE)
  a$layout <- tt_layout_defaults("custom")
  a$header <- list(1)
  expect_error(validate_tt_table(a), "header", fixed = TRUE)
})

test_that("composed effect tables keep every model's unsafe scale provenance", {
  a <- effecttab(data.frame(term = "treat", estimate = 0.8, conf.low = 0.6,
                           conf.high = 1.1, p.value = 0.2),
                 effect = "HR", models = "death", level = 95)
  a$rows$key <- paste0("row", seq_len(nrow(a$body)))
  a$meta$frame$effect_additive <- FALSE
  a$meta$frame$effect_log_scale <- "ratio"
  b <- a
  b$meta$frame$effect_additive <- TRUE
  b$meta$frame$effect_log_scale <- "log"
  merged <- tt_merge(a, b)
  expect_identical(merged$meta$frame$effect_additive, c(FALSE, TRUE))
  expect_identical(merged$meta$frame$effect_log_scale, c("ratio", "log"))
  stacked <- tt_stack(a, b)
  expect_identical(stacked$meta$frame$n_models, a$meta$frame$n_models)
  expect_identical(stacked$meta$frame$effect_additive, TRUE)
  expect_identical(stacked$meta$frame$effect_log_scale, "log")
  rate <- stratetab(data.frame(group = c("No", "Yes"), D = c(5, 8), Y = c(100, 200),
                              Rate = c(0.05, 0.04), Lower = c(0.03, 0.02), Upper = c(0.08, 0.07)),
                    level = 95, outcomeids = "death")
  expect_error(hrcomptab(rate, stacked, rows = 2, outcomemap = "death"),
               class = "tabtools_error_not_hazard_ratio")
  # Optional provenance absent in the first input must not drop later fields.
  unknown <- a
  unknown$meta$frame$effect_additive <- NULL
  unknown$meta$frame$effect_log_scale <- NULL
  merged <- tt_merge(unknown, b)
  expect_identical(merged$meta$frame$effect_additive, c(NA, TRUE))
  expect_identical(merged$meta$frame$effect_log_scale, c("", "log"))
  expect_identical(tt_stack(a, a)$meta$frame$effect_additive, FALSE)
  expect_identical(tt_stack(a, a)$meta$frame$effect_log_scale, "ratio")
  known <- a
  expect_identical(tt_stack(unknown, known)$meta$frame$effect_log_scale, "ratio")
  expect_no_error(hrcomptab(rate, tt_stack(unknown, known), rows = 2, outcomemap = "death"))
})

test_that("optional log-scale metadata does not relabel known Cox hazard ratios as unknown", {
  skip_if_not_installed("survival")
  a <- regtab(survival::coxph(survival::Surv(time, status) ~ age, data = survival::lung, ties = "breslow"),
              models = "death")
  b <- a
  b$meta$frame$effect_additive <- FALSE
  b$meta$frame$effect_log_scale <- "ratio"
  expect_identical(tt_merge(a, b)$meta$frame$effect_log_scale, c("", "ratio"))
  expect_identical(tt_stack(a, b)$meta$frame$effect_log_scale, "ratio")
})
