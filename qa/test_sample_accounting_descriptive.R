library(testthat)
library(tabtools)

qa_sa_desc_value <- function(t, id, metric) {
  s <- t$meta$sample_accounting
  expect_type(s, "list")
  if (is.null(s)) stop("The descriptive source has no sample accounting ledger.")
  m <- s$measures[s$measures$population_id == id & s$measures$metric == metric, ]
  expect_equal(nrow(m), 1L)
  m
}

test_that("installed descriptive ledgers recover independently filtered records and frequencies", {
  d <- data.frame(g = c("A", "A", "A", "B", "B", NA, "", "B"),
                  x = c(3, NA, 100, -1, 0, 999, 999, 999),
                  cat = c("a", NA, "drop", "b", NA, "drop", "drop", "drop"),
                  w = c(2, 3, 0, 1, 2, 7, 4, NA))
  attr(d$x, "label") <- "Label retained"
  before <- d
  eligible <- !is.na(d$w) & d$w > 0 & !is.na(d$g) & d$g != ""
  masks <- list(eligible, eligible & !is.na(d$x), eligible & !is.na(d$x) & d$x > 0,
                eligible, eligible & !is.na(d$cat))
  for (kind in c("none", "importance", "frequency")) {
    args <- list(data = d, vars = c(x = "contn", x = "contln", cat = "cat"),
                 by = "g", total = "after", missing = TRUE, nopvalue = TRUE)
    if (kind == "none") {
      args$data <- d[eligible, ]
      local_masks <- lapply(masks, function(m) m[eligible])
    } else {
      args[[if (kind == "importance") "wt" else "fweight"]] <- "w"
      local_masks <- masks
    }
    t <- do.call(table1_tc, args)
    ids <- c("table", "variable/1/table", "variable/2/table", "variable/3/table")
    for (i in seq_along(ids)) {
      m <- local_masks[[i]]
      w <- if (kind == "none") rep(1, sum(m)) else d$w[m]
      expected <- c(used_n = sum(m), weight_sum = sum(w), effective_n = sum(w)^2 / sum(w^2),
                    reported_n = if (kind == "frequency") sum(w) else sum(m))
      for (metric in names(expected)) {
        actual <- qa_sa_desc_value(t, ids[i], metric)
        expect_equal(actual$value, expected[[metric]], tolerance = 1e-12)
        expect_identical(actual$status, "available")
      }
    }
    expect_equal(qa_sa_desc_value(t, "variable/3/table", "observed_n")$value, 2, tolerance = 0)
    expect_equal(qa_sa_desc_value(t, "variable/3/table", "missing_n")$value, 2, tolerance = 0)
    expect_identical(attr(as.data.frame(t), "sample_accounting"), t$meta$sample_accounting)
    expect_identical(do.call(desctab, args), t)
  }
  expect_identical(d, before)
})

test_that("installed overlays keep raw record weight diagnostics distinct from crude summaries", {
  d <- data.frame(g = c("A", "A", "B", "B", "lost", NA),
                  x = c(1, NA, 3, 4, 999, 999), w = c(1, 3, 2, 4, 0, NA))
  for (scale in c(1, 1e-310, 1e300)) {
    ds <- d
    ds$w <- ds$w * scale
    t <- table1_tc(ds, vars = c(x = "contn"), by = "g", wt = "w", wtcompare = TRUE,
                   total = "before", nopvalue = TRUE)
    for (pass in c("crude", "weighted")) {
      w <- if (pass == "crude") rep(1, 4) else ds$w[1:4]
      m <- qa_sa_desc_value(t, paste0(pass, "/table"), "weight_sum")
      expect_equal(m$value / sum(w), 1, tolerance = 1e-12)
      norm <- w / max(w)
      expect_equal(qa_sa_desc_value(t, paste0(pass, "/table"), "effective_n")$value,
                   sum(norm)^2 / sum(norm^2), tolerance = 1e-12)
      expect_equal(qa_sa_desc_value(t, paste0(pass, "/table"), "input_n")$value, 6, tolerance = 0)
      expect_equal(qa_sa_desc_value(t, paste0(pass, "/table"), "used_n")$value, 4, tolerance = 0)
    }
    expect_setequal(t$meta$sample_accounting$populations$component, c("crude", "weighted"))
  }
})

test_that("installed incomplete groups and repeated specs retain distinct source accounting", {
  d <- data.frame(g = factor(c("A", "A", "B", "B", "lost", "lost"),
                             levels = c("lost", "B", "A", "unused")),
                  x = c(NA, NA, -1, 2, 999, 999), z = NA_real_, w = c(2, 1, 3, 4, 0, 0))
  before <- d
  t <- table1_tc(d, vars = c(x = "contn", x = "contln", z = "contn"),
                 by = "g", fweight = "w", total = "after", missingsummary = TRUE,
                 nopvalue = TRUE, smallcells = 3)
  p <- t$meta$sample_accounting$populations
  expect_identical(p$group[p$scope == "group"], c("2", "3"))
  expect_equal(p$spec[p$scope == "variable" & is.na(p$group)], c(1, 2, 3), tolerance = 0)
  for (id in c("variable/1/group/2", "variable/2/group/2", "variable/3/table")) {
    expect_equal(qa_sa_desc_value(t, id, "used_n")$value, 0, tolerance = 0)
    expect_equal(qa_sa_desc_value(t, id, "weight_sum")$value, 0, tolerance = 0)
    ess <- qa_sa_desc_value(t, id, "effective_n")
    expect_identical(ess$status, "unavailable")
    expect_true(is.na(ess$value))
    expect_true(nzchar(ess$reason))
  }
  expect_equal(qa_sa_desc_value(t, "variable/2/table", "observed_n")$value, 2, tolerance = 0)
  expect_equal(qa_sa_desc_value(t, "variable/2/table", "used_n")$value, 1, tolerance = 0)
  expect_equal(qa_sa_desc_value(t, "variable/2/table", "reported_n")$value, 4, tolerance = 0)
  expect_identical(d, before)
})

test_that("installed overflow weight totals remain unknown without losing record ESS", {
  d <- data.frame(x = 1:4, w = rep(8e307, 4))
  t <- table1_tc(d, vars = c(x = "conts"), wt = "w", nopvalue = TRUE)
  total <- qa_sa_desc_value(t, "table", "weight_sum")
  expect_identical(total$status, "unavailable")
  expect_true(is.na(total$value))
  expect_match(total$reason, "not finitely representable")
  expect_equal(qa_sa_desc_value(t, "table", "used_n")$value, 4, tolerance = 0)
  expect_equal(qa_sa_desc_value(t, "table", "effective_n")$value, 4, tolerance = 1e-12)
  expect_equal(qa_sa_desc_value(t, "table", "reported_n")$value, 4, tolerance = 0)
})
