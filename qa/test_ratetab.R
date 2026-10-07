library(testthat)
library(tabtools)

test_that("installed ratetab export, literal inference, sinks and raw saving agree", {
  scratch <- withr::local_tempdir(pattern = "tabtools-installed-ratetab-")
  d <- data.frame(g = c("A", "A", "A", "B"), cid = c(1, 2, 3, 4),
                   e = c(6, 0, 0, 0), y = 1)
  x <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", per = 1,
    headershade = TRUE, zebra = TRUE, footnote = c("First note", "Second note"),
    xlsx = file.path(scratch, "rates.xlsx"), csv = file.path(scratch, "rates.csv"),
    markdown = file.path(scratch, "rates.md"), saving = file.path(scratch, "rates.rds"))
  expect_equal(unname(x$stored$estimates[1, c("rate", "lb", "ub")]),
               c(2, .281726988186435, 14.198142768462672), tolerance = 1e-12)
  expect_identical(colnames(x$stored$estimates),
    c("outcome", "group", "level", "events", "persontime", "rate", "lb", "ub"))
  expect_equal(unname(x$stored$estimates[2, "ub"]), -log(.025))
  expect_identical(x$command, "ratetab")
  expect_identical(x$meta$frame$producer, "ratetab")
  expect_true(all(file.exists(file.path(scratch, c("rates.xlsx", "rates.csv", "rates.md", "rates.rds")))))
  expect_equal(as.numeric(readRDS(file.path(scratch, "rates.rds"))$rate), c(2, 0))
  for (path in file.path(scratch, c("rates.csv", "rates.md"))) {
    lines <- readLines(path, warn = FALSE)
    expect_true(any(grepl("First note", lines, fixed = TRUE)))
    expect_true(any(grepl("Second note", lines, fixed = TRUE)))
  }
  printed <- capture.output(print(x))
  expect_true(any(grepl("First note", printed, fixed = TRUE)))
  expect_true(any(grepl("Second note", printed, fixed = TRUE)))
  frame <- as.data.frame(x)
  expect_identical(attr(frame, "producer", exact = TRUE), "ratetab")
})

test_that("installed exact limits cross-check poisson.test without using output as expected data", {
  counts <- c(0, 1, 3, 17)
  exposure <- c(4, 10, 5, 30)
  d <- data.frame(g = letters[1:4], events = counts, years = exposure)
  x <- ratetab(d, "g", "events", "years", per = 100, level = .9)
  expected <- t(vapply(seq_along(counts), function(i)
    stats::poisson.test(counts[i], T = exposure[i], conf.level = .9)$conf.int * 100, numeric(2)))
  expect_equal(unname(x$stored$estimates[, c("lb", "ub")]), unname(expected), tolerance = 1e-11)
  masked <- ratetab(d, "g", "events", "years", per = 100, level = .9, smallcells = 4, zerocells = "blank")
  expect_equal(masked$stored$estimates, x$stored$estimates)
  expect_identical(masked$meta$rate_rows$state, c("masked", "masked", "masked", "est"))
  expect_true(all(is.na(masked$stored$publication_estimates[1:3, c("rate", "lb", "ub")])))
})

test_that("installed clustered covariance matches independent glm HC0 score sandwich", {
  skip_if_not_installed("sandwich")
  d <- data.frame(g = factor(rep(c("A", "B"), each = 4)),
    cid = c(1, 2, 3, 4, 1, 2, 4, 5), e = c(3, 1, 2, 0, 1, 4, 0, 1),
    y = c(1, 2, 1, 3, 2, 1, 3, 1))
  fit <- stats::glm(e ~ 0 + g + offset(log(y)), data = d, family = stats::poisson())
  expected <- sandwich::vcovCL(fit, cluster = d$cid, type = "HC0", cadjust = TRUE)
  x <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", per = 1)
  expect_equal(unname(x$meta$rate_cluster_diagnostics[[1]]$covariance), unname(expected), tolerance = 1e-9)
  expect_equal(x$stored$clusters[1, 1], 5)
  expected_ci <- cbind(exp(stats::coef(fit) - 1.959963984540054 * sqrt(diag(expected))),
                       exp(stats::coef(fit) + 1.959963984540054 * sqrt(diag(expected))))
  expect_equal(unname(x$stored$estimates[, c("lb", "ub")]), unname(expected_ci), tolerance = 1e-9)
})

test_that("installed native-compatible DTA saving preserves value labels and collision mapping", {
  skip_if_not_installed("haven")
  scratch <- withr::local_tempdir(pattern = "tabtools-installed-ratetab-dta-")
  d <- data.frame(group = haven::labelled(c(2, 1), c(High = 2, Low = 1)),
                   g_group = c("X", "Y"), e = c(1, 0), y = c(2, 4))
  attr(d$group, "label") <- "Grouping label"
  x <- ratetab(d, c("group", "g_group"), "e", "y", saving = file.path(scratch, "saved.dta"), smallcells = 2)
  saved <- haven::read_dta(file.path(scratch, "saved.dta"))
  expect_equal(unname(unclass(saved$g2_group)), c(1, 2, NA, NA), ignore_attr = TRUE)
  # Native DTA stores the label map by numeric code (RT010: Low=1, High=2).
  expect_identical(attr(saved$g2_group, "labels"), c(Low = 1, High = 2))
  # The in-memory raw record and caller keep the original attribute order.
  expect_identical(attr(x$meta$saved_data$g2_group, "labels"), c(High = 2, Low = 1))
  expect_identical(attr(d$group, "labels"), c(High = 2, Low = 1))
  expect_identical(attr(saved$g2_group, "label"), "Grouping label")
  expect_equal(as.numeric(saved$events), c(0, 1, 1, 0))
  expect_identical(x$meta$grouping_name_map, c(group = "g2_group", g_group = "g_group"))
})

test_that("installed ratetab help renders its source and publication boundaries", {
  db <- tools::Rd_db("tabtools")
  expect_true("ratetab.Rd" %in% names(db))
  text <- paste(capture.output(tools::Rd2txt(db[["ratetab.Rd"]],
    options = list(underline_titles = FALSE))), collapse = " ")
  text <- gsub("[[:space:]]+", " ", text)
  expect_match(text, "Incidence-rate tables", fixed = TRUE)
  expect_match(text, "Primary masks provide no complementary protection", fixed = TRUE)
  expect_match(text, "R has no implicit Stata", fixed = TRUE)
  expect_match(text, "one event-contributing cluster", ignore.case = TRUE)
})
