# Record masks and raw-weight arithmetic are independent of the table engine.
sa_desc_measure <- function(t, id, metric) {
  s <- t$meta$sample_accounting
  expect_type(s, "list")
  if (is.null(s)) stop("The descriptive source has no sample accounting ledger.")
  out <- s$measures[s$measures$population_id == id & s$measures$metric == metric, ]
  expect_equal(nrow(out), 1L)
  out
}

sa_desc_values <- function(t, id, expected) {
  for (metric in names(expected)) {
    m <- sa_desc_measure(t, id, metric)
    expect_equal(m$value, expected[[metric]], tolerance = 1e-12,
                 info = paste(id, metric))
    expect_identical(m$status, "available")
  }
}

sa_desc_fixture <- function() {
  d <- data.frame(g = c("A", "A", "A", "B", "B", NA, "", "B"),
                  w = c(2, 3, 0, 1, 2, 7, 4, NA),
                  x = c(2, NA, 1000, -1, 0, 999, 999, 999),
                  cat = c("a", NA, "zero", "b", NA, "nogroup", "blankgroup", "badweight"),
                  bin = c(1, NA, 1, 0, NA, 1, 1, 1), z = NA_real_)
  attr(d$x, "label") <- "Measurement"
  rownames(d) <- paste0("private-record-", seq_len(nrow(d)))
  d
}

test_that("descriptive input exclusions reconcile before variable-specific masks", {
  d <- sa_desc_fixture()
  before <- d
  t <- table1_tc(d, vars = c(x = "contn", x = "contln", cat = "cat", bin = "bin", z = "contn"),
                 by = "g", wt = "w", missing = TRUE, total = "after", nopvalue = TRUE)
  eligible <- !is.na(d$w) & d$w > 0 & !is.na(d$g) & d$g != ""
  w <- d$w[eligible]
  sa_desc_values(t, "table", c(input_n = nrow(d), eligible_n = sum(eligible),
    used_n = sum(eligible), zero_weight_n = sum(d$w == 0, na.rm = TRUE),
    excluded_n = sum(!eligible), weight_sum = sum(w), effective_n = sum(w)^2 / sum(w^2),
    reported_n = sum(eligible)))
  expect_identical(sa_desc_measure(t, "table", "observed_n")$status, "unavailable")
  ex <- t$meta$sample_accounting$exclusions
  ex <- ex[ex$population_id == "table", ]
  expect_identical(ex$reason, c("missing_weight", "zero_weight", "missing_group"))
  expect_equal(ex$n, c(1, 1, 2), tolerance = 0)
  expect_equal(sum(ex$n), sum(!eligible), tolerance = 0)
  observed <- eligible & !is.na(d$x)
  positive <- observed & d$x > 0
  sa_desc_values(t, "variable/1/table", c(input_n = sum(eligible), observed_n = sum(observed),
    used_n = sum(observed), missing_n = sum(eligible & is.na(d$x)),
    weight_sum = sum(d$w[observed]), effective_n = sum(d$w[observed])^2 / sum(d$w[observed]^2)))
  sa_desc_values(t, "variable/2/table", c(observed_n = sum(observed), used_n = sum(positive),
    missing_n = sum(eligible & is.na(d$x)), excluded_n = sum(eligible) - sum(positive),
    weight_sum = sum(d$w[positive]), effective_n = 1))
  p <- t$meta$sample_accounting$populations
  expect_equal(p$spec[p$scope == "variable" & is.na(p$group)], seq_len(5), tolerance = 0)
  expect_identical(p$variable[p$scope == "variable" & is.na(p$group)], c("x", "x", "cat", "bin", "z"))
  expect_identical(p$weight_type, rep("importance", nrow(p)))
  expect_identical(d, before)
  expect_identical(attr(as.data.frame(t), "sample_accounting"), t$meta$sample_accounting)
  expect_false(any(grepl("private-record|nogroup|badweight", capture.output(str(t$meta$sample_accounting)))))
})

test_that("missing categories contribute while binary and continuous missing values do not", {
  d <- sa_desc_fixture()
  t <- table1_tc(d, vars = c(cat = "cate", bin = "bine", z = "conts"),
                 by = "g", fweight = "w", missing = TRUE, total = "after", nopvalue = TRUE)
  sa_desc_values(t, "table", c(used_n = 4, weight_sum = 8, effective_n = 64 / 18, reported_n = 8))
  sa_desc_values(t, "variable/1/table", c(observed_n = 2, used_n = 4, missing_n = 2,
    excluded_n = 0, weight_sum = 8, reported_n = 8))
  sa_desc_values(t, "variable/2/table", c(observed_n = 2, used_n = 2, missing_n = 2,
    excluded_n = 2, weight_sum = 3, reported_n = 3))
  sa_desc_values(t, "variable/3/table", c(observed_n = 0, used_n = 0, missing_n = 4,
    excluded_n = 4, weight_sum = 0, reported_n = 0))
  ess <- sa_desc_measure(t, "variable/3/table", "effective_n")
  expect_true(is.na(ess$value))
  expect_identical(ess$status, "unavailable")
  expect_match(ess$reason, "no positive")
  e <- t$meta$sample_accounting$exclusions
  expect_identical(e$stage[e$population_id == "variable/1/table"], "retained")
  expect_identical(e$reason[e$population_id == "variable/1/table"], "missing_category")
  omitted <- table1_tc(d, vars = c(cat = "cat"), by = "g", fweight = "w",
                       total = "after", nopvalue = TRUE)
  sa_desc_values(omitted, "variable/1/table", c(observed_n = 2, used_n = 2, excluded_n = 2,
    weight_sum = 3, reported_n = 3))
  expect_identical(t$header[[2]]$text[-1], c("N=5", "N=3", "N=8"))
})

test_that("crude and weighted overlays share eligible records but retain distinct weights", {
  d <- sa_desc_fixture()
  t <- table1_tc(d, vars = c(x = "contn", cat = "cat"), by = "g", wt = "w",
                 wtcompare = TRUE, missing = TRUE, total = "after", nopvalue = TRUE)
  sa_desc_values(t, "crude/table", c(input_n = 8, eligible_n = 4, used_n = 4,
    excluded_n = 4, weight_sum = 4, effective_n = 4, reported_n = 4))
  sa_desc_values(t, "weighted/table", c(input_n = 8, eligible_n = 4, used_n = 4,
    excluded_n = 4, weight_sum = 8, effective_n = 64 / 18, reported_n = 4))
  sa_desc_values(t, "crude/variable/1/group/1", c(observed_n = 1, used_n = 1, weight_sum = 1))
  sa_desc_values(t, "weighted/variable/1/group/1", c(observed_n = 1, used_n = 1, weight_sum = 2))
  p <- t$meta$sample_accounting$populations
  expect_setequal(p$component, c("crude", "weighted"))
  expect_true(all(p$weight_type[p$component == "crude"] == "none"))
  expect_true(all(p$weight_type[p$component == "weighted"] == "importance"))
})

test_that("group accounting uses original records and preserves surviving group codes", {
  d <- data.frame(g = factor(c(rep("lost", 8), "left", "right", "left"),
                             levels = c("lost", "left", "right", "unused")),
                  x = seq_len(11), w = c(rep(0, 8), 2, 3, NA))
  before <- d
  t <- table1_tc(d, vars = c(x = "contn"), by = "g", wt = "w", nopvalue = TRUE)
  sa_desc_values(t, "table", c(input_n = 11, eligible_n = 2, zero_weight_n = 8,
    excluded_n = 9, weight_sum = 5, effective_n = 25 / 13))
  sa_desc_values(t, "group/1", c(input_n = 2, eligible_n = 1, used_n = 1, excluded_n = 1))
  sa_desc_values(t, "group/2", c(input_n = 1, eligible_n = 1, used_n = 1, excluded_n = 0))
  p <- t$meta$sample_accounting$populations
  expect_identical(p$group[p$scope == "group"], c("2", "3"))
  expect_identical(t$header[[1]]$text[-1], c("left", "right"))
  expect_identical(sa_desc_measure(t, "table", "reported_n")$status, "unavailable")
  expect_match(sa_desc_measure(t, "table", "reported_n")$reason, "no_aggregate")
  expect_identical(d, before)
})

test_that("descriptive weight metadata retains raw scale and stable record ESS", {
  for (w in list(c(1, 2, 3, 4) * 1e-310, rep(8e307, 4))) {
    d <- data.frame(x = c(1, 2, 3, 4), w = w)
    before <- d
    t <- table1_tc(d, vars = c(x = "contn"), wt = "w", nopvalue = TRUE)
    scaled <- w / max(w)
    sa_desc_values(t, "table", c(used_n = 4, effective_n = sum(scaled)^2 / sum(scaled^2), reported_n = 4))
    m <- sa_desc_measure(t, "table", "weight_sum")
    if (is.finite(sum(w))) {
      expect_equal(m$value / sum(w), 1, tolerance = 1e-12)
      expect_identical(m$status, "available")
    } else {
      expect_true(is.na(m$value))
      expect_identical(m$status, "unavailable")
      expect_match(m$reason, "not finitely representable")
    }
    expect_identical(d, before)
  }
})

test_that("desctab shares the full descriptive ledger including all-missing groups", {
  d <- data.frame(g = c("A", "A", "B", "B"), x = c(NA, NA, 2, 4),
                  cat = factor(c(NA, NA, NA, NA), levels = "unused"))
  args <- list(data = d, vars = c(x = "contn", cat = "cat"), by = "g", missing = TRUE,
               total = "after", nopvalue = TRUE, smallcells = 3, missingsummary = TRUE)
  t <- do.call(table1_tc, args)
  expect_identical(do.call(desctab, args), t)
  sa_desc_values(t, "variable/1/group/1", c(input_n = 2, observed_n = 0, used_n = 0,
    missing_n = 2, excluded_n = 2, weight_sum = 0))
  sa_desc_values(t, "variable/2/table", c(input_n = 4, observed_n = 0, used_n = 4,
    missing_n = 4, excluded_n = 0, weight_sum = 4, effective_n = 4))
  expect_identical(t$meta$sample_accounting$version, 1L)
})
