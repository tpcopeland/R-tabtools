test_that("ratetab exact and log limits have independent count controls", {
  d <- data.frame(g = c("A", "B"), e = c(1, 0), y = c(10, 4))
  x <- ratetab(d, "g", "e", "y", per = 1, digits = 2, pydigits = 1)
  expect_equal(unname(x$stored$estimates[, "rate"]), c(.1, 0))
  # Count one has the elementary lower endpoint -log(.975); the upper
  # endpoint solves exp(-u)*(1+u)=.025, independent of production qgamma.
  expect_equal(unname(x$stored$estimates[1, "lb"]), .002531780798428988, tolerance = 1e-13)
  upper <- unname(x$stored$estimates[1, "ub"]) * 10
  expect_equal(exp(-upper) * (1 + upper), .025, tolerance = 1e-13)
  expect_equal(unname(x$stored$estimates[2, "ub"]), -log(.025) / 4, tolerance = 1e-13)
  expect_identical(x$body[[4L]][2L], "0.10 (0.00, 0.56)")
  expect_identical(x$body[[4L]][3L], "0.00 (0.00, 0.92)")
  logx <- ratetab(data.frame(g = "A", e = 4, y = 2), "g", "e", "y", per = 1, ci = "poisson")
  expect_equal(unname(logx$stored$estimates[1, c("lb", "ub")]),
               2 * exp(c(-1, 1) * 1.959963984540054 / 2), tolerance = 1e-13)
  for (method in c("exact", "poisson", "cluster")) {
    z <- d; z$cid <- 1:2
    out <- ratetab(z, "g", "e", "y", per = 1, ci = method,
                   cluster = if (method == "cluster") "cid" else NULL)
    expect_equal(unname(out$stored$estimates[2, "ub"]), -log(.025) / 4, tolerance = 1e-13)
    expect_identical(out$stored$N_zero, 1L)
  }
})

test_that("ratetab common sample and separate sections keep original row identity", {
  d <- data.frame(g = c(1, 1, 2, 2, NA, NA, 3, 4),
    h = c("A", NA, "A", "B", "B", NA, "C", "D"),
    e = c(1, 2, 0, 1, 3, 4, 0, 0), f = c(2, NA, 1, 0, 1, 4, 0, 0),
    y = c(1, 2, 3, 4, 5, 6, 0, 0), z = c(2, 2, 3, 4, 5, 6, 0, 0))
  before <- d
  x <- ratetab(d, c("g", "h"), c("e", "f"), c("y", "z"), per = 1)
  expect_identical(x$meta$source_sample$row_ids, c(1L, 3L, 4L, 5L, 7L, 8L))
  expect_identical(x$meta$source_sample$section_row_ids[[1L]], c(1L, 3L, 4L, 7L, 8L))
  expect_equal(x$stored$N, 6)
  expect_equal(x$stored$N_nopt, 8)
  expect_equal(unname(x$stored$estimates[1:4, "events"]), c(1, 1, 0, 0))
  expect_equal(unname(x$stored$estimates[9:12, "events"]), c(1, 4, 0, 0))
  expect_true(all(x$meta$rate_rows$state[x$meta$rate_rows$mask_reason == "no_display_person_time"] == "empty"))
  expect_silent(tabtools:::.tt_validate_sample_accounting(x$meta$sample_accounting))
  expect_identical(d, before)
})

test_that("cluster inference counts exposure-only clusters and uses native correction", {
  d <- data.frame(g = "A", cid = 1:3, e = c(6, 0, 0), y = 1)
  x <- ratetab(d, "g", "e", "y", per = 1, ci = "cluster", cluster = "cid")
  expect_equal(x$stored$clusters[1, 1], 3)
  expect_equal(unname(x$meta$rate_cluster_diagnostics[[1L]]$scores[, 1]), c(4, -2, -2))
  expect_equal(unname(x$meta$rate_cluster_diagnostics[[1L]]$covariance), matrix(1, 1, 1))
  expect_equal(unname(x$stored$estimates[1, c("lb", "ub")]),
               c(.281726988186435, 14.198142768462672), tolerance = 1e-12)
  expect_identical(x$stored$N_noci, 0L)
  d$cid <- 1
  one <- ratetab(d, "g", "e", "y", per = 1, ci = "cluster", cluster = "cid")
  expect_true(all(is.na(one$stored$estimates[1, c("lb", "ub")])))
  expect_identical(one$meta$rate_rows$state, "notest")
  expect_identical(one$stored$N_noci, 1L)
  expect_identical(one$meta$rate_cluster_diagnostics[[1]]$reason, "single_cluster_native_convention")
  d$cid <- 1:3; d$e <- 1
  balanced <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid")
  expect_identical(balanced$meta$rate_cluster_diagnostics[[1]]$reason, "zero_score_variance")
  expect_identical(balanced$stored$N_noci, 1L)
})

test_that("excludemasked changes the fitted cluster population without changing raw rates", {
  d <- data.frame(g = c("A", "A", "B", "B"), cid = c(1, 2, 2, 3), e = c(4, 2, 1, 0), y = 1)
  ordinary <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", per = 1, smallcells = 2)
  excluded <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", per = 1,
                       smallcells = 2, excludemasked = TRUE)
  expect_equal(ordinary$stored$clusters[1, 1], 3)
  expect_equal(excluded$stored$clusters[1, 1], 2)
  expect_identical(excluded$meta$rate_cluster_diagnostics[[1]]$fitted_row_ids, 1:2)
  expect_equal(ordinary$stored$estimates[, "rate"], excluded$stored$estimates[, "rate"])
  expect_true(all(is.na(excluded$stored$estimates[2, c("lb", "ub")])))
  expect_identical(excluded$stored$N_maskfit, 1L)
  expect_identical(excluded$stored$N_noci, 0L)
})

test_that("frequency expansion preserves original cluster IDs and separate sample counts", {
  d <- data.frame(g = c("A", "A", "A", "B"), cid = c(1, 2, 3, 4),
                   e = c(3, 0, 0, 100), y = 1, w = c(2, 1, 1, 0))
  x <- ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", fweight = "w", per = 1)
  expanded <- d[rep(1:3, c(2, 1, 1)), , drop = FALSE]
  y <- ratetab(expanded, "g", "e", "y", ci = "cluster", cluster = "cid", per = 1)
  expect_equal(x$stored$estimates, y$stored$estimates)
  expect_equal(x$stored$clusters, y$stored$clusters)
  expect_equal(x$stored$N, 4)
  expect_equal(x$stored$N_records, 3)
  expect_equal(x$stored$clusters[1, 1], 3)
  expect_equal(unname(x$meta$rate_cluster_diagnostics[[1]]$covariance), matrix(.5625, 1, 1))
  expect_identical(x$meta$source_sample$row_ids, 1:3)
  expect_identical(x$meta$source_sample$weight_type, "fweight")
  expect_silent(tabtools:::.tt_validate_sample_accounting(x$meta$sample_accounting))
})

test_that("primary and zero masks separate publication companions from D2 analytical numbers", {
  d <- data.frame(g = c("A", "B", "C"), e = c(1, 0, 0), y = c(2, 4, 0))
  x <- ratetab(d, "g", "e", "y", smallcells = 2, zerocells = "dash", zerocells_persontime = TRUE, per = 1)
  expect_identical(x$body[[2L]], c("", "<2", "\u2013", ""))
  expect_identical(x$body[[3L]], c("", "\u2013", "\u2013", ""))
  expect_identical(x$meta$rate_rows$state, c("masked", "masked", "empty"))
  expect_true(all(is.na(x$stored$publication_estimates[, c("events", "persontime", "rate", "lb", "ub")])))
  expect_equal(unname(x$stored$estimates[1, "rate"]), .5)
  expect_equal(unname(x$stored$estimates[2, "ub"]), -log(.025) / 4)
  expect_equal(as.numeric(x$meta$saved_data$events), c(1, 0, 0))
  expect_identical(x$stored$smallcells, list(threshold = 2L, mode = "primary", n_masked = 1L, n_linked = 2L))
  expect_identical(x$stored$estimates_role, "raw_analytical")
  expect_identical(x$stored$N_nopt, 1L)
})

test_that("time scaling occurs once and no-time counts use original exposure", {
  d <- data.frame(g = c("A", "B"), e = c(3, 0), y = c(5, 0))
  a <- ratetab(d, "g", "e", "y", per = 1)
  b <- ratetab(d, "g", "e", "y", per = 100, pyscale = 10)
  expect_equal(b$stored$estimates[1, c("rate", "lb", "ub")], a$stored$estimates[1, c("rate", "lb", "ub")] * 1000)
  expect_equal(unname(b$stored$estimates[1, "persontime"]), .5)
  expect_identical(b$stored$N_nopt, 1L)
  expect_equal(b$meta$rate_raw_rows$source_persontime, c(5, 0))
  expect_identical(b$command, "ratetab")
  expect_identical(b$meta$frame$source, "stratetab")
  expect_identical(b$meta$frame$producer, "ratetab")
  expect_identical(b$meta$frame$outcome_id, "e")
})

test_that("saved analytical data preserves grouping attributes and native collision names", {
  g <- factor(c("High", "Low"), levels = c("High", "Low", "Unused"))
  attr(g, "labels") <- c(High = 2, Low = 1, Unused = 3)
  attr(g, "label") <- "Group label"
  d <- data.frame(group = g, g_group = c("X", "Y"), e = c(1, 0), y = c(2, 3))
  scratch <- withr::local_tempdir(pattern = "tabtools-fast-ratetab-save-")
  path <- file.path(scratch, "raw.rds")
  x <- ratetab(d, c("group", "g_group", "group"), "e", "y", saving = path, smallcells = 2)
  saved <- readRDS(path)
  expect_identical(attr(saved, "grouping_name_map"), c(group = "g2_group", g_group = "g_group"))
  expect_identical(levels(saved$g2_group), levels(g))
  expect_identical(attr(saved$g2_group, "labels"), attr(g, "labels"))
  expect_identical(attr(saved$g2_group, "label"), "Group label")
  expect_identical(as.character(saved$g2_group), c("Low", "High", NA, NA, "Low", "High"))
  expect_equal(as.numeric(saved$events), c(0, 1, 1, 0, 0, 1))
  expect_identical(attr(saved, "analytical_role"), "raw_analytical")
  expect_error(ratetab(d, "group", "e", "y", saving = path), class = "tabtools_error_ratetab_saving")
  expect_silent(ratetab(d, "group", "e", "y", saving = path, replace = TRUE))
})

test_that("domain and inference refusals precede output and preserve selection semantics", {
  d <- data.frame(g = c("A", "A"), e = c(1, 0), y = c(1, 0), cid = c(1, NA))
  expect_error(ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid"), class = "tabtools_error_ratetab_cluster")
  expect_silent(ratetab(d, "g", "e", "y", ci = "cluster", cluster = "cid", subset = 1L))
  expect_error(ratetab(d, "g"), class = "tabtools_error_ratetab_source")
  expect_error(ratetab(d, "g", "e", "y", ci = "cl"), class = "tabtools_error_ratetab_inference")
  expect_error(ratetab(d, "g", "e", "y", excludemasked = TRUE), class = "tabtools_error_ratetab_inference")
  expect_error(ratetab(d, "g", "e", "y", fweight = c(1, .5)), class = "tabtools_error_ratetab_weight")
  d$y[1] <- 0
  expect_error(ratetab(d, "g", "e", "y"), class = "tabtools_error_ratetab_domain")
  d$e[1] <- 0
  expect_error(ratetab(d, "g", "e", "y"), class = "tabtools_error_ratetab_domain")
  expect_error(ratetab(d, "g", "e", "y", subset = integer()), class = "tabtools_error_ratetab_sample")
})

test_that("ratetab session explicitness and numeric-format guards use shared contracts", {
  scratch <- withr::local_tempdir(pattern = "tabtools-fast-ratetab-session-")
  wb <- file.path(scratch, "session.xlsx"); md <- file.path(scratch, "session.md")
  withr::local_options(list(tabtools.workbook = wb, tabtools.markdown = md,
                            tabtools.smallcells = 2L, tabtools.masktext = "Hidden",
                            tabtools.headershade = TRUE))
  d <- data.frame(g = "A", e = 1, y = 2)
  x <- ratetab(d, "g", "e", "y")
  expect_false(file.exists(wb)); expect_false(file.exists(md))
  expect_identical(x$body[[2L]][2L], "Hidden")
  expect_false(x$style$headershade)
  y <- ratetab(d, "g", "e", "y", sheet = "S", nosmallcells = TRUE, masktext = NULL)
  expect_true(file.exists(wb)); expect_true(file.exists(md))
  expect_equal(y$stored$smallcells$threshold, 0L)
  expect_error(ratetab(d, "g", "e", "y", digits = 2, cformat = "%9.2f"), class = "tabtools_error_format_conflict")
  expect_error(ratetab(d, "g", "e", "y", cformat = "%9,2f"), class = "tabtools_error_format_separator")
})
