library(testthat)
library(tabtools)

# Installed public APIs, with record sets fixed before the table is computed.
integration_sample <- function(x) {
  s <- if (inherits(x, "tt_table")) x$meta[["sample_accounting", exact = TRUE]] else
    attr(x, "sample_accounting", exact = TRUE)
  expect_type(s, "list")
  expect_identical(s$version, 1L)
  expect_identical(names(s), c("version", "populations", "measures", "exclusions"))
  s
}

integration_measure <- function(s, metric, scope = "table", variable = NULL,
                                whole_variable = FALSE) {
  p <- s$populations
  selected <- p$scope == scope
  if (!is.null(variable)) selected <- selected & !is.na(p$variable) & p$variable == variable
  if (whole_variable) selected <- selected & is.na(p$group)
  ids <- p$id[selected]
  expect_length(ids, 1L)
  rows <- s$measures[s$measures$population_id == ids & s$measures$metric == metric, ]
  expect_identical(nrow(rows), 1L)
  rows
}

integration_preserved <- function(actual, original, prefix) {
  # Context paths describe transport; every underlying measure and exclusion
  # must survive without recomputing it from selected display rows.
  expect_identical(actual$version, original$version)
  expect_identical(actual$populations$id, paste0(prefix, "/", original$populations$id))
  components <- ifelse(nzchar(original$populations$component),
    paste0(prefix, "/", original$populations$component), prefix)
  expect_identical(actual$populations$component, components)
  expect_identical(actual$populations[setdiff(names(actual$populations), c("id", "component"))],
    original$populations[setdiff(names(original$populations), c("id", "component"))])
  expect_identical(actual$measures$population_id,
    paste0(prefix, "/", original$measures$population_id))
  expect_identical(actual$measures[-1], original$measures[-1])
  exclusion_ids <- if (nrow(original$exclusions))
    paste0(prefix, "/", original$exclusions$population_id) else character()
  expect_identical(actual$exclusions$population_id, exclusion_ids)
  expect_identical(actual$exclusions[-1], original$exclusions[-1])
}

integration_cohort <- function() {
  d <- data.frame(private_subject = paste0("sensitive-subject-QA-", seq_len(8)),
    group = c("A", "A", "A", "B", "B", NA, "B", "A"),
    value = c(1, NA, 3, 4, NA, 6, 7, 8),
    category = factor(c("red", NA, "blue", "red", NA, "blue", "blue", "red")),
    time = c(2, 3, 0, 4, 5, 6, 7, NA), event = c(1, NA, 1, 0, 1, 1, NA, 1),
    frequency = c(2, 3, 0, 1, 4, 2, NA, 1))
  rownames(d) <- d$private_subject
  d
}

test_that("one cohort retains different truthful populations across three families", {
  d <- integration_cohort(); original <- d
  # Descriptive records: 1,2,4,5,8; rate records: 1,2,4,5,6;
  # diagnosed weight records: 1,2,3,4,5,8, including one zero.
  descriptive <- table1_tc(d, vars = c(category = "cat"), by = "group",
    fweight = "frequency", missing = TRUE, total = "after")
  rates <- tt_rates(d, "time", "event", by = "group", fweight = "frequency",
    missing = TRUE, float_time = FALSE)
  expect_warning(weights <- wttab(d, "frequency", by = "group"), "Dropped 2")
  # Baseline controls run before accounting assertions, so they also run
  # against the preserved package that predates the source ledger.
  expect_identical(descriptive$header[[2]]$text[2:4], c("N=6", "N=5", "N=11"))
  expect_identical(descriptive$body$c4[2:3], c("4 (36)", "7 (64)"))
  expect_identical(rates$D, c(2, 4, 2))
  expect_identical(rates$Y, c(13, 24, 12))
  expect_identical(weights$stored$N, 6L)
  ds <- integration_sample(descriptive)
  rs <- integration_sample(rates)
  ws <- integration_sample(weights)
  for (s in list(ds, rs, ws)) {
    expect_identical(integration_measure(s, "input_n")$value, 8)
    expect_identical(integration_measure(s, "zero_weight_n")$value, 1)
  }
  for (s in list(ds, rs)) {
    expect_identical(integration_measure(s, "used_n")$value, 5)
    expect_identical(integration_measure(s, "excluded_n")$value, 3)
  }
  expect_identical(integration_measure(ds, "reported_n")$value, 11)
  expect_identical(integration_measure(ds, "weight_sum")$value, 11)
  expect_identical(integration_measure(ds, "observed_n", "variable", "category", TRUE)$value, 3)
  expect_identical(integration_measure(ds, "missing_n", "variable", "category", TRUE)$value, 2)
  expect_identical(integration_measure(ds, "used_n", "variable", "category", TRUE)$value, 5)
  expect_identical(integration_measure(rs, "observed_n")$value, 4)
  expect_identical(integration_measure(rs, "missing_n")$value, 1)
  expect_identical(integration_measure(rs, "weight_sum")$value, 12)
  expect_identical(integration_measure(ws, "used_n")$value, 6)
  expect_identical(integration_measure(ws, "excluded_n")$value, 2)
  expect_identical(integration_measure(ws, "reported_n")$value, 6)
  expect_equal(integration_measure(ds, "effective_n")$value, 121 / 31, tolerance = 1e-12)
  expect_equal(integration_measure(rs, "effective_n")$value, 144 / 34, tolerance = 1e-12)
  expect_equal(integration_measure(ws, "effective_n")$value, 121 / 31, tolerance = 1e-12)
  expect_identical(d, original)
  for (s in list(ds, rs, ws)) {
    text <- paste(capture.output(dput(s)), collapse = "\n")
    expect_false(grepl("sensitive-subject-QA-", text, fixed = TRUE))
    expect_identical(names(s), c("version", "populations", "measures", "exclusions"))
  }
})

test_that("rate standardization and presentation selection preserve every source population", {
  d <- integration_cohort()
  # stratetab requires nonblank category labels; name this category before
  # calculating rates while preserving the five predetermined rate records.
  d$group[is.na(d$group)] <- "Missing"
  rates <- tt_rates(d, "time", "event", by = "group", fweight = "frequency",
    missing = TRUE, float_time = FALSE)
  s <- integration_sample(rates)
  standardized <- tt_rates(rates, level = .95)
  expect_identical(standardized[c("D", "Y", "Rate", "Lower", "Upper")],
    rates[c("D", "Y", "Rate", "Lower", "Upper")])
  expect_identical(integration_sample(standardized), s)
  rate_table <- stratetab(rates, outcomeids = "event")
  st <- integration_sample(rate_table)
  expect_identical(integration_measure(st, "used_n")$value, 5)
  expect_identical(attr(as.data.frame(rate_table), "sample_accounting", exact = TRUE), st)
  wrapped <- puttab(as.data.frame(rate_table), subset = 3, noembedheader = TRUE)
  expect_identical(nrow(wrapped$body), 1L)
  integration_preserved(integration_sample(wrapped), st, "source")
  composite <- stacktab(list(list(table = wrapped, rows = 1),
                             list(table = wrapped, rows = 1)))
  combined <- integration_sample(composite)
  expect_identical(nrow(combined$populations), 2L * nrow(st$populations))
  table_ids <- combined$populations$id[combined$populations$scope == "table"]
  expect_length(table_ids, 2L)
  n <- combined$measures$value[combined$measures$metric == "used_n" &
                               combined$measures$population_id %in% table_ids]
  expect_identical(n, c(5, 5))
  expect_identical(length(unique(combined$populations$id)), nrow(combined$populations))
  expect_identical(combined$populations$component[combined$populations$scope == "table"],
    c("block1/source/block1/source1", "block2/source/block1/source1"))
  final <- puttab(as.data.frame(composite), subset = 2, noembedheader = TRUE)
  expect_identical(nrow(final$body), 1L)
  integration_preserved(integration_sample(final), combined, "source")
})

test_that("supplied summaries and legacy presentation sources keep their counts unknown", {
  supplied <- data.frame(group = "A", D = 0, Y = 10, Rate = 0,
    Lower = NA_real_, Upper = NA_real_)
  rates <- tt_rates(supplied, level = .95)
  s <- integration_sample(rates)
  expect_identical(rates$D, 0)
  expect_identical(s$populations$weight_type, "unknown")
  for (metric in c("input_n", "eligible_n", "observed_n", "used_n", "reported_n",
                    "zero_weight_n", "weight_sum", "effective_n")) {
    m <- integration_measure(s, metric, "summary")
    expect_identical(m$value, NA_real_)
    expect_identical(m$status, "unavailable")
    expect_true(nzchar(m$reason))
  }
  effects <- effecttab(data.frame(term = c("age", "treatment"),
      estimate = c(2, 3), std.error = c(.2, .3)), models = "Effects",
    data = data.frame(age = seq_len(20)), level = 95)
  es <- integration_sample(effects)
  expect_identical(nrow(es$populations), 1L)
  expect_identical(es$measures$value, rep(NA_real_, 12))
  expect_identical(es$measures$status, rep("unavailable", 12))
  selected <- comptab(as.data.frame(effects), rows = 1)
  expect_identical(nrow(selected$body), 1L)
  integration_preserved(integration_sample(selected), es, "modeltable1")
  raw <- puttab(data.frame(term = c("a", "b"), estimate = c(2, 3)), subset = 1)
  expect_identical(integration_sample(raw)$measures$value, rep(NA_real_, 12))
  legacy <- effects; legacy$meta$sample_accounting <- NULL
  mixed <- stacktab(list(legacy, puttab(effects)))
  ms <- integration_sample(mixed)
  expect_identical(nrow(ms$populations), 2L)
  expect_identical(ms$populations$component, c("block1", "block2/source/model1/piece1"))
  expect_identical(ms$measures$value, rep(NA_real_, 24))
  expect_identical(ms$measures$status, rep("unavailable", 24))
})

test_that("workbook cells containing N do not establish recoverable subject counts", {
  weights <- wttab(c(1, 2, 3))
  book <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(book))
  tt_write_xlsx(weights, book, sheet = "Evidence")
  out <- stacktab(list(list(sheet = "Evidence", rows = 2:3)),
    xlsx = book, sheet = "Composite")
  s <- integration_sample(out)
  expect_identical(nrow(s$populations), 1L)
  expect_identical(s$populations$scope, "summary")
  expect_identical(s$populations$component, "block1")
  expect_identical(s$measures$status, rep("unavailable", 12))
  expect_identical(s$measures$value, rep(NA_real_, 12))
  expect_identical(weights$stored$N, 3L)
})

test_that("sample construction and transport leave caller state intact", {
  withr::local_preserve_seed()
  set.seed(818)
  before_seed <- .Random.seed
  before_options <- options()
  before_dir <- getwd()
  before_devices <- grDevices::dev.list()
  d <- data.frame(value = c(1, 3, 5, 7), weight = c(0, 1, 2, 3))
  original <- d
  a <- table1_tc(d, vars = c(value = "contn"), wt = "weight")
  b <- wttab(d, "weight")
  out <- stacktab(list(puttab(a), puttab(b)))
  integration_sample(out)
  expect_identical(.Random.seed, before_seed)
  expect_identical(options(), before_options)
  expect_identical(getwd(), before_dir)
  expect_identical(grDevices::dev.list(), before_devices)
  expect_identical(d, original)
})

test_that("stored model frames and contributing records survive overlap and reordering", {
  d <- data.frame(private_subject = paste0("sensitive-fit-QA-", seq_len(10)),
    x = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
    y = 2 + 1.5 * seq_len(10) + rep(c(-.3, .3), 5),
    w = c(1, 2, 1, 0, 1, 1, 1, 1, 1, 1))
  rownames(d) <- d$private_subject
  fit <- lm(y ~ x, data = d, weights = w, subset = seq_len(nrow(d)) <= 8)
  # Accepted rows: 1,2,4,5,6,7,8. Row4 is stored but contributes no weight.
  expect_identical(nrow(model.frame(fit)), 7L)
  expect_identical(stats::nobs(fit), 6L)
  tab <- regtab(fit, models = "M", stats = "n")
  s <- integration_sample(tab)
  for (metric in c("frame_n", "eligible_n", "observed_n")) {
    expect_identical(integration_measure(s, metric, "model")$value, 7)
  }
  for (metric in c("used_n", "fitted_n", "reported_n")) {
    expect_identical(integration_measure(s, metric, "model")$value, 6)
  }
  expect_identical(integration_measure(s, "zero_weight_n", "model")$value, 1)
  expect_identical(integration_measure(s, "input_n", "model")$status, "unavailable")
  expect_identical(integration_measure(s, "excluded_n", "model")$value, NA_real_)
  expect_identical(tail(tab$body$c2, 1), "6")
  # Equal sized but disjoint samples remain two source populations.
  left <- regtab(lm(y ~ x, data = d[c(1, 2, 4, 5), ]), models = "M")
  right <- regtab(lm(y ~ x, data = d[c(6, 7, 8, 9), ]), models = "M")
  composed <- tt_merge(tt_merge(left, right), left)
  cs <- integration_sample(composed)
  expect_identical(cs$populations$component,
    c("table1/table1/model1", "table1/table2/model1", "table2/model1"))
  expect_identical(as.numeric(cs$populations$model), c(1, 1, 1))
  expect_identical(cs$measures$value[cs$measures$metric == "fitted_n"], c(4, 4, 4))
  expect_identical(nrow(cs$populations), 3L)
  expect_false(grepl("sensitive-fit-QA-", paste(capture.output(dput(cs)), collapse = ""), fixed = TRUE))
})

test_that("multiple imputations report separate samples without multiplying subjects", {
  ids <- paste0("sensitive-imputation-QA-", seq_len(12))
  d <- data.frame(x = seq_len(12), y = 1 + 2 * seq_len(12) + rep(c(-.5, .5), 6))
  rownames(d) <- ids
  d2 <- d[c(12, 1:11), ]
  d2$y <- d2$y + rep(c(.1, -.1), 6)
  pooled <- tt_mi(list(lm(y ~ x, data = d), lm(y ~ x, data = d2)),
    observation_ids = list(ids, ids[c(12, 1:11)]), sample_check = "strict")
  tab <- regtab(pooled, stats = "n", models = "MI")
  s <- integration_sample(tab)
  expect_identical(s$populations$scope, c("imputation", "imputation"))
  expect_identical(as.numeric(s$populations$imputation), c(1, 2))
  expect_identical(as.numeric(s$populations$model), c(1, 1))
  expect_identical(s$measures$value[s$measures$metric == "fitted_n"], c(12, 12))
  expect_identical(tail(tab$body$c2, 1), "12")
  expect_identical(attr(as.data.frame(tab), "sample_accounting", exact = TRUE), s)
  expect_false(grepl("sensitive-imputation-QA-", paste(capture.output(dput(s)), collapse = ""), fixed = TRUE))
  selected <- comptab(as.data.frame(tab), rows = 1)
  integration_preserved(integration_sample(selected), s, "modeltable1")
})
