library(testthat)
library(tabtools)

# Public installed-package checks. These oracles use explicit sample masks,
# base statistics, and physical frequency expansion rather than internals.
qa_desc_row <- function(tab, variable, type = "var") {
  which(tab$rows$var == variable & tab$rows$type == type)
}
qa_desc_pair <- function(cell) {
  as.numeric(strsplit(cell, "±", fixed = TRUE)[[1L]])
}

test_that("frequency-weighted inference follows each covariate's own observed sample", {
  d <- data.frame(g = rep(c("A", "B"), each = 6),
                  x = c(1, NA, 4, 7, 9, 1000, 2, 4, NA, 10, 13, 1000),
                  c = c("a", "b", NA, "a", "b", "excluded", "b", NA, "a", "b", "a", "excluded"),
                  f = c(2, 3, 1, 2, 1, 0, 1, 2, 4, 1, 2, NA))
  extra <- data.frame(g = NA_character_, x = 9999, c = "nogroup", f = 9)
  d <- rbind(d, extra)
  idx <- rep(c(1:5, 7:11), c(2L, 3L, 1L, 2L, 1L, 1L, 2L, 4L, 1L, 2L))
  e <- d[idx, ]
  t <- table1_tc(d, vars = c(x = "contn", c = "cat"), by = "g", fweight = "f",
                 missing = TRUE, smd = TRUE, total = "after", missingsummary = TRUE)
  expect_identical(t$header[[2]]$text[2:4], c("N=9", "N=10", "N=19"))
  a <- e$x[e$g == "A" & !is.na(e$x)]
  b <- e$x[e$g == "B" & !is.na(e$x)]
  expect_equal(unname(t$stored$table["x", "p_value"]), stats::t.test(a, b)$p.value,
               tolerance = 1e-12)
  expect_equal(unname(t$stored$table["x", "smd"]),
               abs(mean(a) - mean(b)) / sqrt((stats::var(a) + stats::var(b)) / 2),
               tolerance = 1e-12)
  ct <- table(addNA(factor(e$c), ifany = TRUE), e$g)
  expect_warning(chisq <- stats::chisq.test(ct, correct = FALSE), "approximation may be incorrect")
  expect_equal(unname(t$stored$table["c", "p_value"]),
               unname(chisq$p.value), tolerance = 1e-12)
})

test_that("importance-weighted moments respect disjoint covariate missingness patterns", {
  d <- data.frame(g = rep(c("A", "B"), each = 5),
                  x = c(1, 3, 7, NA, 999, 2, 5, NA, 11, 999),
                  w = c(1, 2, 4, 8, 0, 3, 1, 7, 2, NA))
  for (missing_row in c(1L, 3L, 6L, 9L)) {
    cur <- d
    cur$x[missing_row] <- NA_real_
    t <- table1_tc(cur, vars = c(x = "contn"), by = "g", wt = "w",
                   total = "after", format = "%15.6f", missingsummary = TRUE)
    expect_identical(t$header[[2]]$text[2:4], c("N=4", "N=4", "N=8"))
    sample <- c(1:4, 6:9)
    for (column in seq_len(3L)) {
      use <- sample
      if (column < 3L) use <- use[cur$g[use] == c("A", "B")[column]]
      use <- use[!is.na(cur$x[use])]
      y <- cur$x[use]
      w <- cur$w[use]
      m <- sum(y * w) / sum(w)
      # Stata's analytic weights use the observed record count in n/(n-1).
      v <- sum(w * (y - m)^2) / sum(w) * length(use) / (length(use) - 1)
      cell <- t$body[qa_desc_row(t, "x"), column + 1L]
      expect_equal(qa_desc_pair(cell), c(m, sqrt(v)), tolerance = 5e-7)
    }
  }
})

test_that("lognormal columns count nonpositive values as unavailable for summaries", {
  d <- data.frame(g = c("A", "A", "A", "B", "B", "B", NA),
                  x = c(0, -1, 4, NA, 2, 8, 100000))
  t <- table1_tc(d, vars = c(x = "contln"), by = "g", total = "after",
                 nopvalue = TRUE, missingsummary = TRUE, format = "%15.6f")
  expect_identical(t$header[[2]]$text[2:4], c("N=3", "N=3", "N=6"))
  expect_identical(as.character(t$body[qa_desc_row(t, "x", "missing_summary"), 2:4]),
                   c("2 (67)", "1 (33)", "3 (50)"))
  # The geometric mean is four for each group and for their union.
  cells <- as.character(t$body[qa_desc_row(t, "x"), 2:4])
  expect_equal(as.numeric(sub(" .*", "", cells)), rep(4, 3L), tolerance = 5e-7)
})

test_that("categorical row percentages use observed row totals without counting Total twice", {
  d <- data.frame(g = c("A", "A", "A", "B", "B", NA),
                  c = c("one", "one", NA, "one", "two", "outside"),
                  f = c(2, 1, 4, 3, 2, 100))
  for (include_missing in c(FALSE, TRUE)) {
    t <- table1_tc(d, vars = c(c = "cat"), by = "g", fweight = "f",
                   missing = include_missing, catrowperc = TRUE, slashN = TRUE,
                   total = "after", nopvalue = TRUE, percformat = "%10.4f",
                   spacelowpercent = FALSE)
    one <- which(t$body[[1]] == "   one")
    two <- which(t$body[[1]] == "   two")
    expect_length(one, 1L)
    expect_length(two, 1L)
    expect_identical(as.character(t$body[one, 2:4]),
                     c("3/6 (50.0000)", "3/6 (50.0000)", "6/6 (100.0000)"))
    expect_identical(as.character(t$body[two, 2:4]),
                     c("0/2 (0.0000)", "2/2 (100.0000)", "2/2 (100.0000)"))
    if (include_missing) {
      miss <- which(t$body[[1]] == "   Missing")
      expect_length(miss, 1L)
      expect_identical(as.character(t$body[miss, 2:4]),
                       c("4/4 (100.0000)", "0/4 (0.0000)", "4/4 (100.0000)"))
    }
  }
})

test_that("wttab cutpoints and every design cell use the retained sample", {
  d <- data.frame(w = c(0, 2, 4, 8, 1, 3, 5, 9, NA, 999, 999, 999),
                  g = factor(c("A", "A", "A", "A", "B", "B", "B", "B", "A", NA, "B", NA),
                             levels = c("A", "B", "unused")),
                  p = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1, NA, NA))
  retained <- 1:8
  for (mode in c("pooled", "period")) {
    expect_warning(t <- wttab(d, "w", by = "g", period = "p", trunc = c(.25, .75),
                               trunc_by = mode), "Dropped 4", class = "rlang_warning")
    expect_identical(t$stored$N, 8L)
    expect_identical(t$stored$n_dropped, 4L)
    st <- t$stored$stats
    expect_identical(nrow(st), 12L)
    expect_identical(unique(st$group), c("Overall", "A", "B"))
    for (i in seq_len(nrow(st))) {
      pr <- as.numeric(st$period[i])
      use <- retained[d$p[retained] == pr]
      cutoff_rows <- if (mode == "pooled") retained else use
      cuts <- unname(stats::quantile(d$w[cutoff_rows], c(.25, .75), type = 2))
      if (st$group[i] != "Overall") use <- use[d$g[use] == st$group[i]]
      z <- d$w[use]
      moved <- sum(z < cuts[1L] | z > cuts[2L])
      truncated <- !is.na(st$weights[i]) && st$weights[i] != "Untruncated"
      if (truncated) z <- pmin(pmax(z, cuts[1L]), cuts[2L])
      expect_identical(st$n[i], as.numeric(length(z)))
      if (!length(z)) {
        expect_true(all(is.na(st[i, c("mean", "sd", "min", "max", "ess", "ess_pct")])))
      } else {
        expect_equal(st$mean[i], mean(z), tolerance = 1e-12)
        expect_equal(c(st$min[i], st$max[i]), range(z), tolerance = 1e-12)
        expect_equal(st$ess[i], sum(z)^2 / sum(z^2), tolerance = 1e-12)
        expect_equal(st$ess_pct[i], 100 * sum(z)^2 / (sum(z^2) * length(z)), tolerance = 1e-12)
        expect_equal(c(st$p25[i], st$p50[i], st$p75[i]),
                     unname(stats::quantile(z, c(.25, .5, .75), type = 2)), tolerance = 1e-12)
        expect_equal(st$sd[i], stats::sd(z), tolerance = 1e-12)
      }
      if (truncated) expect_identical(st$n_trunc[i], as.numeric(moved))
    }
    cut <- t$stored$trunc
    for (i in seq_len(nrow(cut))) {
      use <- if (mode == "pooled") retained else retained[d$p[retained] == as.numeric(cut$period[i])]
      expect_equal(c(cut$lower[i], cut$upper[i]),
                   unname(stats::quantile(d$w[use], c(.25, .75), type = 2)), tolerance = 1e-12)
    }
  }
})

test_that("wttab missing observations preserve Kish ESS across extreme finite weight scales", {
  for (scale in c(1e-200, 1, 1e200)) {
    w <- c(0, 1, 2, 3, NA_real_) * scale
    expect_warning(t <- wttab(w), "Dropped 1", class = "rlang_warning")
    s <- t$stored$stats
    expect_identical(t$stored$N, 4L)
    expect_identical(t$stored$n_dropped, 1L)
    expect_equal(s$mean / scale, 1.5, tolerance = 1e-12)
    expect_equal(s$sd / scale, sqrt(5 / 3), tolerance = 1e-12)
    expect_equal(s$ess, 36 / 14, tolerance = 1e-12)
    expect_equal(s$ess_pct, 100 * 36 / 56, tolerance = 1e-12)
  }
})


test_that("frequency-weighted rank and log tests drop unavailable covariates independently", {
  d <- data.frame(g = rep(c("A", "B"), each = 6),
                  x = c(1, 2, NA, 5, 0, -1, 2, NA, 4, 9, 0, -2),
                  f = c(1, 2, 3, 2, 1, 0, 2, 1, 3, 1, 2, NA))
  idx <- rep(c(1:5, 7:11), c(1L, 2L, 3L, 2L, 1L, 2L, 1L, 3L, 1L, 2L))
  e <- d[idx, ]
  for (type in c("conts", "contln")) {
    args <- if (type == "conts") list(wilcox.test = list(exact = FALSE, correct = FALSE)) else NULL
    t <- table1_tc(d, vars = c(x = type), by = "g", fweight = "f", test_args = args,
                   total = "after", missingsummary = TRUE)
    a <- e$x[e$g == "A" & !is.na(e$x)]
    b <- e$x[e$g == "B" & !is.na(e$x)]
    expected <- if (type == "contln") {
      stats::t.test(log(a[a > 0]), log(b[b > 0]))$p.value
    } else {
      stats::wilcox.test(a, b, exact = FALSE, correct = FALSE)$p.value
    }
    expect_equal(unname(t$stored$table["x", "p_value"]), expected, tolerance = 1e-12)
    expect_identical(t$header[[2]]$text[2:4], c("N=9", "N=9", "N=18"))
  }
})

test_that("installed wttab rejects nonfinite weights but drops ordinary NA weights", {
  for (bad in c(Inf, -Inf, NaN)) {
    expect_error(wttab(c(1, bad), by = c("A", NA)), "finite",
                 class = "tabtools_error_weights_nonfinite")
  }
  expect_warning(t <- wttab(c(0, 2, NA_real_)), "Dropped 1", class = "rlang_warning")
  expect_identical(t$stored$N, 2L)
  expect_identical(t$stored$n_dropped, 1L)
  expect_equal(t$stored$stats$mean, 1, tolerance = 1e-12)
  expect_equal(t$stored$stats$ess, 1, tolerance = 1e-12)
})
