library(testthat)
library(tabtools)

test_that("crosstab strict and primary metadata disclose only the policy's allowed values", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL,
                           tabtools.smallcells = NULL, tabtools.smallcells_mode = NULL,
                           tabtools.masktext = NULL, tabtools.boldp = NULL))
  d <- xt_qa_data(matrix(c(1, 9, 9, 1), 2))
  directory <- withr::local_tempdir(pattern = "crosstab-privacy-")
  for (mode in c("strict", "primary")) for (marker in list(NULL, "", "999.0%")) {
    suffix <- paste0(mode, if (is.null(marker)) "default" else if (nzchar(marker)) "custom" else "empty")
    args <- list(data = d, rowvar = "row", colvar = "column", weights = "frequency",
                 smallcells = 3, smallcells_mode = mode, or = TRUE, rr = TRUE, rd = TRUE, cochran = TRUE,
                 csv = file.path(directory, paste0(suffix, ".csv")),
                 markdown = file.path(directory, paste0(suffix, ".md")),
                 xlsx = file.path(directory, paste0(suffix, ".xlsx")))
    if (!is.null(marker)) args$masktext <- marker
    x <- do.call(crosstab, args)
    expect_true(all(is.na(diag(x$stored$table))))
    expect_true(all(is.na(diag(x$meta$publication$percentages))))
    expect_null(x$stored$raw)
    expect_null(x$meta$raw)
    ledger <- x$meta$sample_accounting$measures
    expect_true(all(is.na(ledger$value[ledger$metric != "reported_n"])))
    expect_equal(ledger$value[ledger$metric == "reported_n"], 20)
    expect_true(all(file.exists(unlist(args[c("xlsx", "csv", "markdown")], use.names = FALSE))))
    if (mode == "strict") {
      expect_true(all(is.na(x$stored$table)))
      expect_true(all(is.na(unlist(x$stored[c("p", "or", "or_lo", "or_hi", "rr", "rr_lo", "rr_hi", "rd", "rd_lo", "rd_hi",
                                           "z_trend", "chi2_trend", "p_trend")]))))
      expect_true(all(is.na(x$meta$publication$percentages)))
      expect_identical(x$stored$smallcells$n_linked, 9L)
      publication <- paste(readLines(args$csv, warn = FALSE), readLines(args$markdown, warn = FALSE), collapse = "\n")
      expect_false(grepl("90.0%", publication, fixed = TRUE))
    } else {
      expect_equal(x$stored$or, 1 / 81)
      expect_false(is.na(x$stored$p))
      expect_identical(x$stored$smallcells$n_linked, 2L)
      expect_match(x$footnote, "may permit reconstruction", fixed = TRUE)
    }
  }
})

test_that("crosstab extreme scores and literal frequency expansion preserve signed inference", {
  withr::local_options(list(tabtools.workbook = NULL, tabtools.markdown = NULL,
                           tabtools.smallcells = NULL, tabtools.smallcells_mode = NULL, tabtools.masktext = NULL))
  f <- rbind(c(10, 20, 30), c(30, 20, 10))
  for (scores in list(c(0, 1, 2), c(100, 102, 104), 1e16 + c(0, 2, 4), c(-1e308, 0, 1e308))) {
    d <- xt_qa_data(f, columns = scores)
    x <- crosstab(d, "row", "column", weights = "frequency", cochran = TRUE)
    expect_equal(c(x$stored$z_trend, x$stored$chi2_trend), c(-sqrt(20), 20))
    expanded <- d[rep(seq_len(nrow(d)), d$frequency), ]
    y <- crosstab(expanded, "row", "column", cochran = TRUE)
    expect_equal(y$stored$z_trend, x$stored$z_trend)
  }
  for (f in list(matrix(c(5, 3, 2, 7, 4, 6), 2), matrix(c(10, 5, 8, 4, 2, 3, 9, 1, 6), 3))) {
    d <- xt_qa_data(f)
    expanded <- d[rep(seq_len(nrow(d)), d$frequency), ]
    ranks <- cbind(rank(expanded$row), rank(expanded$column))
    centred <- sweep(ranks, 2, colMeans(ranks))
    rho <- sum(centred[, 1] * centred[, 2]) / sqrt(prod(colSums(centred^2)))
    n <- sum(f)
    p <- 2 * stats::pt(abs(rho) * sqrt((n - 2) / (1 - rho^2)), n - 2, lower.tail = FALSE)
    expect_equal(crosstab(d, "row", "column", weights = "frequency", trend = TRUE)$stored$p_trend, p)
  }
})


test_that("rendered installed crosstab help preserves the explicit variance formulas", {
  help <- utils::help("crosstab", package = "tabtools")
  expect_length(help, 1L)
  rendered <- paste(utils::capture.output(tools::Rd2txt(utils:::.getHelpFile(help))), collapse = "\n")
  expect_match(rendered, "c/(a*N1)+d/(b*N0)", fixed = TRUE)
  expect_match(rendered, "a*c/N1^3+b*d/N0^3", fixed = TRUE)
})
