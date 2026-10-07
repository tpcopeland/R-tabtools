rich_stats_fit <- function() lm(y ~ x, data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6))

test_that("scalar pairs text formats and builtin labels retain exact requested order", {
  fit <- rich_stats_fit()
  t <- regtab(fit, fit, stats = list("n", list(name = "amount", label = "Amount", fmt = "%6.1f"),
    list(name = "ev", pair = "total", label = "Events (total)"),
    list(text = c("Unweighted", "IPW"), label = "Weighting")),
    stat_values = list(list(amount = 1.25, ev = 3, total = 13), list(amount = 2.75, ev = 5, total = 20)),
    statlabels = c(n = "Records"))
  ix <- which(t$rows$type == "stat")
  expect_identical(t$rows$key[ix], c("stat:n", "stat:e(amount)", "stat:e(ev|total)", "stat:text(1)"))
  expect_identical(t$body[ix, 1L], c("Records", "Amount", "Events (total)", "Weighting"))
  expect_identical(t$body[ix, 2L], c("6", "1.2", "3 (13)", "Unweighted"))
  expect_identical(t$body[ix, 5L], c("6", "2.8", "5 (20)", "IPW"))
  expect_equal(t$stored$e_amount_1, 1.25)
  expect_equal(t$stored$e_total_2, 20)
})

test_that("strictest scalar threshold applies to all scalar and pair occurrences", {
  fit <- rich_stats_fit()
  t <- regtab(fit, stats = list(list(name = "ev", mincell = 5),
    list(name = "ev", pair = "total", mincell = 7)), stat_values = list(list(ev = 3, total = 13)))
  ix <- which(t$rows$type == "stat")
  expect_identical(t$body[ix, 2L], c("<7", "<7 (13)"))
  expect_equal(t$stored$N_stats_masked, 2)
  expect_null(t$stored$N_stats_linked)
  expect_equal(t$stored$e_ev_1, 3)
  parts <- t$meta$regtab_stats_provenance
  expect_identical(parts$state[parts$part_name == "ev"], c("masked", "masked"))
  expect_identical(parts$raw_value[parts$part_name == "ev"], c(3, 3))
})

test_that("linked groups close transitively and withhold available zeros", {
  fit <- rich_stats_fit()
  t <- regtab(fit, stats = list(list(name = "ev", mincell = 5, maskwith = "a"),
    list(name = "a", maskwith = "b"), list(name = "b"), list(name = "unrelated")),
    stat_values = list(list(ev = 3, a = 10, b = 0, unrelated = 100)))
  ix <- which(t$rows$type == "stat")
  expect_identical(t$body[ix, 2L], c("<5", "\u2013", "\u2013", "100"))
  expect_equal(t$stored$N_stats_masked, 1)
  expect_equal(t$stored$N_stats_linked, 2)
  expect_identical(t$meta$regtab_stats_provenance$state, c("masked", "linked", "linked", "available"))
  expect_equal(t$stored$e_b_1, 0)
})

test_that("pairs create no dependency and a missing first value leaves a blank", {
  fit <- rich_stats_fit()
  t <- regtab(fit, fit, stats = list(list(name = "a", pair = "b", mincell = 5)),
    stat_values = list(list(a = 3, b = 10), list(a = NA_real_, b = 2)))
  i <- match("stat:e(a|b)", t$rows$key)
  expect_identical(unname(unlist(t$body[i, c(2L, 5L)])), c("<5 (10)", ""))
  expect_equal(t$stored$N_stats_masked, 2)
  expect_null(t$stored$N_stats_linked)
  expect_identical(t$meta$regtab_stats_provenance$state, c("masked", "blank", "available", "masked"))
})

test_that("zero and fractional small values are not primary count masks", {
  fit <- rich_stats_fit()
  t <- regtab(fit, fit, fit, stats = list(list(name = "count", mincell = 5)),
    stat_values = list(list(count = 0), list(count = .5), list(count = 5)))
  i <- match("stat:e(count)", t$rows$key)
  expect_identical(unname(unlist(t$body[i, c(2L, 5L, 8L)])), c("0.000", "0.500", "5.000"))
  expect_equal(t$stored$N_stats_masked, 0)
})

test_that("twelve models preserve numeric order in cells raw values and provenance", {
  fit <- rich_stats_fit()
  fits <- rep(list(fit), 12)
  t <- regtab(fits, stats = list(list(name = "index"), list(text = paste0("m", 1:12), label = "Model")),
    stat_values = lapply(1:12, function(i) list(index = as.numeric(i))))
  i <- match("stat:e(index)", t$rows$key)
  expect_identical(unname(unlist(t$body[i, 2L + 3L * (0:11)])), as.character(1:12))
  expect_identical(t$meta$regtab_stats_provenance$model_index, 1:12)
  expect_identical(t$meta$regtab_stats_provenance$raw_value, as.numeric(1:12))
  expect_equal(t$stored$e_index_10, 10)
  expect_equal(t$stored$e_index_12, 12)
  i <- match("stat:text(1)", t$rows$key)
  expect_identical(unname(unlist(t$body[i, 2L + 3L * (0:11)])), paste0("m", 1:12))
})

test_that("missing all models refuses while partial missing scalars remain blank", {
  fit <- rich_stats_fit()
  expect_error(regtab(fit, stats = list(list(name = "unavailable"))), class = "tabtools_error_statspec")
  t <- regtab(fit, fit, stats = list(list(name = "some")), stat_values = list(list(some = 2), list()))
  i <- match("stat:e(some)", t$rows$key)
  expect_identical(t$body[i, 2L], "2")
  expect_identical(t$body[i, 5L], "")
  expect_identical(t$meta$regtab_stats_provenance$state, c("available", "blank"))
})

test_that("fit count scalar aliases are authoritative and separately labeled", {
  d <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6, ev = c(1, 0, 2, 0, 1, 0), exposure = 1:6)
  fit <- lm(y ~ x, d)
  record <- tt_fitcount(fit, "ev", exposure = "exposure", data = d)
  t <- regtab(fit, fitcounts = record, stats = list("obs events exposure", list(name = "tt_events", mincell = 5)), exposurelabel = "Person days")
  i <- match("stat:exposure", t$rows$key)
  expect_identical(t$body[i, 1L], "Person days")
  expect_identical(t$body[i, 2L], "21")
  expect_equal(t$stored$exposure_1, 21)
  expect_identical(t$meta$regtab_stats_provenance$source, "fitcount_unweighted")
  expect_error(regtab(fit, fitcounts = record, stats = list(list(name = "tt_events")),
    stat_values = list(list(tt_events = 99))), class = "tabtools_error_statspec")
  expect_error(regtab(fit, stats = "exposure", exposurelabel = "A", statlabels = c(exposure = "B")), class = "tabtools_error_statspec")
})

test_that("invalid exact records and unsafe values refuse before sinks", {
  fit <- rich_stats_fit()
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("sentinel", path)
  bad <- list(list(name = "x", extra = 1), list(name = "x", mincell = 1),
    list(name = "x", mincell = 2.5), list(name = "x", pair = "x"),
    list(name = c("x", "y")), list(name = "x", maskwith = "absent"),
    list(text = c("a", "b"), label = "Too many"))
  for (entry in bad) {
    expect_error(regtab(fit, stats = list(entry), csv = path), class = "tabtools_error_statspec")
    expect_identical(readLines(path), "sentinel")
  }
  for (value in list(Inf, NaN, c(1, 2), "1")) expect_error(regtab(fit, stats = list(list(name = "x")), stat_values = list(list(x = value))), class = "tabtools_error_statspec")
  expect_error(regtab(fit, stats = list(list(name = "x"), list(name = "x"))), class = "tabtools_error_statspec")
  expect_error(regtab(fit, stats = "n", statlabels = c(aic = "Unused")), class = "tabtools_error_statspec")
})

test_that("literal quote dollar backtick and backslash text reaches every sink", {
  fit <- rich_stats_fit()
  literal <- 'literal "quote" $name `call` \\ path'
  dir <- withr::local_tempdir()
  xlsx <- file.path(dir, "table.xlsx"); csv <- file.path(dir, "table.csv"); md <- file.path(dir, "table.md")
  t <- regtab(fit, stats = list(list(text = literal, label = "Literal")), xlsx = xlsx, csv = csv, markdown = md)
  i <- match("stat:text(1)", t$rows$key)
  expect_identical(t$body[i, 2L], literal)
  expect_true(any(vapply(read.csv(csv, header = FALSE, colClasses = "character", check.names = FALSE), function(column) literal %in% column, TRUE)))
  expect_true(any(grepl("literal", readLines(md), fixed = TRUE)))
  sheet <- openxlsx2::read_xlsx(xlsx, sheet = "Regression", col_names = FALSE)
  expect_true(any(vapply(sheet, function(column) literal %in% column, TRUE)))
  expect_match(paste(capture.output(print(t)), collapse = "\n"), "literal", fixed = TRUE)
  flat <- tt_flat(t)
  expect_true(any(vapply(flat, function(column) literal %in% column, TRUE)))
})


test_that("custom scalar publication keeps identity in fields without incidental matrix names", {
  supplied <- list(list(amount = c(input_label = 1.25)), list(amount = c(other_label = 2.5)))
  before <- serialize(supplied, NULL)
  fit <- rich_stats_fit()
  expect_silent(t <- regtab(fit, fit, stats = list(list(name = "amount", mincell = 5)),
    stat_values = supplied))
  expect_identical(t$stored$e_amount_1, 1.25)
  expect_identical(t$stored$e_amount_2, 2.5)
  expect_identical(t$meta$regtab_stats_provenance$part_name, c("amount", "amount"))
  expect_identical(t$meta$regtab_stats_provenance$raw_value, c(1.25, 2.5))
  expect_identical(t$meta$regtab_stats_provenance$model_index, 1:2)
  expect_identical(serialize(supplied, NULL), before)
})
