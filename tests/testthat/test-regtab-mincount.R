fc_state_data <- function() {
  d <- data.frame(g = factor(rep(1:3, 6)), x = rep(0:5, each = 3))
  d$y <- 2 + d$x + as.numeric(d$g) + seq_len(18) %% 4
  d$ev <- c(0, 1, 2)[as.numeric(d$g)]
  d$flag <- as.numeric(d$g == "1")
  d
}

test_that("term counts ignore continuous interaction values including zero", {
  d <- fc_state_data()
  fit <- lm(y ~ g * x, d, contrasts = list(g = contr.treatment(3, base = 2)))
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  expect_equal(record$terms$events[match(c("1.g", "2.g", "3.g", "1.g#x", "2.g#x", "3.g#x"), record$terms$count_key)], c(0, 6, 12, 0, 6, 12))
  expect_equal(record$terms$n_records[match("3.g#x", record$terms$count_key)], 6)
  expect_identical(record$states$status[match(c("2.g", "2.g#c.x"), record$states$key)], c("ref", "ref"))
  expect_false("x" %in% record$terms$count_key)
})

test_that("a missing nonreference Wald row never becomes a reference", {
  mf <- data.frame(g = factor(c("a", "b", "a", "b")))
  rows <- data.frame(label = c("a", "b"), reference_row = c(TRUE, FALSE),
                     term = c(NA_character_, "gb"))
  wald <- data.frame(term = character(), estimate = numeric(), conf.low = numeric(),
                     conf.high = numeric(), p.value = numeric())
  extracted <- tabtools:::.rt_factor_rows("g", rows, wald, mf, NULL, 1)
  expect_identical(extracted$status[extracted$key == "1.g"], "base")
  expect_identical(extracted$status[extracted$key == "2.g"], "notest")
  expect_identical(extracted$term[extracted$key == "2.g"], "gb")
  expect_true(is.na(extracted$estimate[extracted$key == "2.g"]))
})

test_that("mincount masks low events and preserves genuine base2 and continuous rows", {
  d <- fc_state_data()
  fit <- lm(y ~ g * x, d, contrasts = list(g = contr.treatment(3, base = 2)))
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  withr::local_options(list(contrasts = c("contr.sum", "contr.poly")))
  t <- regtab(fit, fitcounts = record, mincount = 7, interactions = "native", nointercept = FALSE)
  r <- t$meta$regtab_rows
  expect_identical(r$status[match(c("1.g", "1.g#c.x", "2.g", "2.g#c.x", "3.g", "x", "_cons"), r$key)],
    c("masked", "masked", "ref", "ref", "est", "est", "est"))
  expect_identical(t$stored$smallcells, list(threshold = 7L, mode = "primary", n_masked = 2L, n_linked = 0L))
  expect_equal(t$stored$N_masked, 2)
  expect_identical(t$body[match("1.g", t$rows$key), 2L], "\u2013")
  expect_identical(t$body[match("2.g#c.x", t$rows$key), 2L], "Reference")
  expect_true(all(is.na(r$estimate[r$status == "masked"])))
  raw <- t$meta$regtab_raw_rows
  expect_true(all(is.finite(raw$estimate[raw$key %in% c("1.g", "1.g#c.x")])))
  expect_identical(t$stored$table_role, "raw_analytical")
})

test_that("plain 01 indicators count the positive level and continuous terms are exempt", {
  d <- fc_state_data()
  fit <- lm(y ~ flag + x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  expect_equal(record$terms$events[record$terms$count_key == "flag"], 0)
  t <- regtab(fit, fitcounts = record, mincount = 1)
  expect_identical(t$meta$regtab_rows$status[match(c("flag", "x"), t$meta$regtab_rows$key)], c("masked", "est"))
  expect_equal(t$stored$N_masked, 1)
})

test_that("unusable variance and omitted aliases take priority over low events", {
  d <- fc_state_data()
  fit <- lm(y ~ g + flag + x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  t <- regtab(fit, fitcounts = record, mincount = 99, notestlabel = "No test", emptylabel = "Low events")
  i <- match("flag", t$rows$key)
  expect_identical(t$body[i, 2L], "No test")
  expect_identical(t$meta$regtab_rows$status[t$meta$regtab_rows$key == "flag"], "notest")
  expect_equal(t$stored$N_masked, 3)
  expect_equal(t$stored$smallcells$n_masked, 2)
  expect_true(all(t$body[i, 3:4] == ""))
})

test_that("sample-excluded levels differ from missing whole model terms", {
  d <- fc_state_data()
  a <- lm(y ~ g * x, d)
  b <- lm(y ~ g * x, d, subset = g != "1")
  c <- lm(y ~ x, d)
  records <- lapply(list(a, b, c), tt_fitcount, events = "ev", data = d, terms = TRUE)
  t <- regtab(a, b, c, fitcounts = records, mincount = 1, absentlabel = "Excluded", interactions = "native")
  r <- t$meta$regtab_rows
  expect_identical(r$status[r$model == 2 & r$key == "1.g"], "absent")
  expect_identical(t$body[match("1.g", t$rows$key), 5L], "Excluded")
  expect_identical(t$body[match("1.g", t$rows$key), 8L], "")
  expect_equal(t$stored$N_absent, 2)
  expect_identical(r$mask_reason[r$model == 2 & r$key == "1.g"], "sample_absent")
})

test_that("positional factor rejoining transports raw count identity rather than labels", {
  d <- fc_state_data()
  d$g <- factor(c("low", "middle", "high")[as.numeric(d$g)], levels = c("low", "middle", "high"))
  a <- lm(y ~ g + x, d)
  b <- lm(y ~ g + x, d, subset = g != "low")
  records <- lapply(list(a, b), tt_fitcount, events = "ev", data = d, terms = TRUE)
  t <- regtab(a, b, fitcounts = records, mincount = 7, absentlabel = "Excluded")
  r <- t$meta$regtab_rows
  expect_identical(r$status[r$model == 2 & r$key == "3.g"], "est")
  expect_identical(r$status[r$model == 2 & r$key == "2.g"], "ref")
  expect_identical(r$status[r$model == 2 & r$key == "1.g"], "absent")
  expect_equal(t$stored$smallcells$n_masked, 1)
})

test_that("count labels and thresholds validate before any sink mutation", {
  d <- fc_state_data(); fit <- lm(y ~ g + x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  file <- withr::local_tempfile(fileext = ".csv")
  writeLines("sentinel", file)
  for (value in list(0, -1, 1.5, NA_real_, Inf, "2", numeric())) {
    expect_error(regtab(fit, fitcounts = record, mincount = value, csv = file), class = "tabtools_error_fitcount")
    expect_identical(readLines(file), "sentinel")
  }
  expect_error(regtab(fit, absentlabel = "Absent"), class = "tabtools_error_fitcount")
  expect_error(regtab(fit, fitcounts = record, mincount = 3, notestlabel = "Reference"), class = "tabtools_error_fitcount")
  explicit <- regtab(fit, fitcounts = record, mincount = 99, emptylabel = "Small")
  expect_identical(explicit$body[match("2.g", explicit$rows$key), 2L], "Small")
})

test_that("failed placeholders retain middle position and default invalid models refuse", {
  d <- fc_state_data(); fit <- lm(y ~ g + x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  failed <- tt_failed_model("Did not converge", "Case-sensitive B")
  t <- regtab(fit, failed, fit, fitcounts = list(record, NULL, record), mincount = 1)
  expect_identical(t$cols$model, c(NA_integer_, rep(1:3, each = 3)))
  expect_true(all(t$body[, 5:7] == ""))
  expect_match(t$footnote, "Model 2: Did not converge", fixed = TRUE)
  expect_identical(t$meta$frame$model_id[2L], "Case-sensitive B")
  expect_error(regtab(failed, fit), class = "tabtools_error_fitcount")
  expect_error(regtab(failed, fitcounts = list(NULL)), class = "tabtools_error_fitcount")
  expect_error(regtab(fit, NULL, fitcounts = list(record, NULL)))
  expect_error(tt_failed_model(""), class = "tabtools_error_fitcount")
})

test_that("missing count capabilities refuse instead of reconstructing live data", {
  d <- fc_state_data(); fit <- lm(y ~ g + x, d)
  record <- tt_fitcount(fit, "ev", data = d)
  expect_error(regtab(fit, mincount = 3), class = "tabtools_error_fitcount")
  expect_error(regtab(fit, fitcounts = record, mincount = 3), class = "tabtools_error_fitcount")
  expect_identical(record$counts[c("people", "exposure")], c(people = NA_real_, exposure = NA_real_))
  t <- regtab(fit, fitcounts = record, stats = "obs events people exposure")
  expect_equal(t$stored$obs_1, 18)
  expect_equal(t$stored$events_1, 18)
  expect_false(any(t$rows$key %in% c("stat:people", "stat:exposure")))
})
