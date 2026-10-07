test_that("seven cell forms have independent literal text", {
  expect_identical(as.character(tabcell("est", b = -2, ll = -3, ul = -1, sep = "-")),
                   "-2.00 (-3.00--1.00)")
  expect_identical(as.character(tabcell("p", p = c(0, .0123, 1))), c("<0.001", "0.012", "1.00"))
  expect_identical(as.character(tabcell("n", n = 12345)), "12,345")
  expect_identical(as.character(tabcell("np", n = 3, d = 40)), "3 (7.5)")
  expect_identical(as.character(tabcell("enp", e = 3, n = 40)), "3/40 (7.5)")
  expect_identical(as.character(tabcell("iqr", median = 2, q1 = 1, q3 = 3)), "2.00 (1.00, 3.00)")
  expect_identical(as.character(tabcell("rate", e = 0, pt = 1000, per = 1000)), "0.0 (0.0, 3.7)")
})

test_that("exact binomial boundary limits have hand-derived endpoints", {
  cells <- tabcell("np", n = c(0, 1), d = 1, ci = "exact")
  expect_identical(as.character(cells), c("0 (0.0; 0.0, 97.5)", "1 (100.0; 2.5, 100.0)"))
  numerical <- attr(cells, "provenance")$publication
  expect_equal(numerical$lb, c(0, 2.5), tolerance = 1e-13)
  expect_equal(numerical$ub, c(97.5, 100), tolerance = 1e-13)
  middle <- attr(tabcell("np", n = 1, d = 2, ci = "exact"), "provenance")$publication
  expect_equal(middle$lb, 100 * (1 - sqrt(.975)), tolerance = 1e-12)
  expect_equal(middle$ub, 100 * sqrt(.975), tolerance = 1e-12)
  zero <- attr(tabcell("np", n = 0, d = 20, ci = "exact"), "provenance")$publication
  expect_equal(zero$ub, 100 * (1 - .025^(1/20)), tolerance = 1e-12)
})

test_that("Poisson exact and log-rate limits use the stated scale once", {
  zero <- attr(tabcell("rate", e = 0, pt = 1000, per = 1000), "provenance")$publication
  expect_equal(zero$estimate, 0)
  expect_equal(zero$lb, 0)
  expect_equal(zero$ub, -log(.025), tolerance = 1e-14)
  one <- attr(tabcell("rate", e = 1, pt = 2, per = 10), "provenance")$publication
  expect_equal(one$lb, -log(.975) * 5, tolerance = 1e-13)
  expect_equal(exp(-one$ub/5) * (1 + one$ub/5), .025, tolerance = 1e-12)
  lograte <- attr(tabcell("rate", e = 4, pt = 2, per = 10, ci = "poisson"), "provenance")$publication
  expect_equal(lograte$estimate, 20)
  expect_equal(lograte$lb, 20 * exp(-qnorm(.975)/2), tolerance = 1e-13)
  expect_equal(lograte$ub, 20 * exp(qnorm(.975)/2), tolerance = 1e-13)
  expect_error(tabcell("rate", e = 2.5, pt = 10, per = 1), class = "tabtools_error_cell_domain")
  expect_s3_class(tabcell("rate", e = 2.5, pt = 10, per = 1, ci = "poisson"), "tt_cell")
})

test_that("leaf masks redact every protected reconstructive companion", {
  counts <- tabcell("n", n = c(0, 3, 5), mincell = 5)
  expect_identical(as.character(counts), c("0", "<5", "5"))
  expect_identical(attr(counts, "provenance")$smallcells,
                   list(threshold = 5L, mode = "primary", n_masked = 1L, n_linked = 0L))
  np <- tabcell("np", n = c(3, 0), d = 40, ci = "exact", mincell = 5)
  expect_identical(as.character(np)[1L], "<5")
  prov <- attr(np, "provenance")
  expect_true(all(is.na(prov$publication[1L, c("count", "pct", "lb", "ub")])))
  expect_true(is.na(prov$raw_inputs$n[1L]))
  expect_identical(prov$rows$status, c("masked", "est"))
  expect_equal(prov$counts$N_missing, 0L)
  expect_identical(as.character(tabcell("np", n = 3, d = 40, ci = "exact", mincell = 5, nocount = TRUE)), "\u2013")
  rate <- tabcell("rate", e = 3, pt = 100, per = 1000, mincell = 5)
  expect_identical(as.character(rate), "\u2013")
  expect_true(all(is.na(attr(rate, "provenance")$publication[, c("estimate", "lb", "ub", "events")])))
  enp <- tabcell("enp", e = c(3, 1), n = c(40, 3), mincell = 5)
  expect_identical(as.character(enp), c("<5/40", "<5"))
  prov <- attr(enp, "provenance")
  expect_equal(prov$publication$denominator, c(40, NA_real_))
  expect_true(all(is.na(prov$raw_inputs$e)))
  expect_true(is.na(prov$raw_inputs$n[2L]))
  expect_equal(prov$smallcells$n_masked, 2L)
})

test_that("ordinary missingness is distinct from invalid finite content", {
  expect_error(tabcell("n", n = NA_real_), class = "tabtools_error_cell_missing")
  replacement <- tabcell("n", n = c(NA_real_, Inf, NaN), missing = "")
  expect_identical(as.character(replacement), rep("", 3))
  expect_equal(attr(replacement, "provenance")$counts$N_missing, 3L)
  literal <- "cost $x \"q\" `literal`"
  expect_identical(as.character(tabcell("p", p = NA_real_, missing = literal)), literal)
  expect_error(tabcell("n", n = -1, missing = "unknown"), class = "tabtools_error_cell_domain")
  expect_error(tabcell("np", n = 1, d = 0, missing = "unknown"), class = "tabtools_error_cell_domain")
  expect_error(tabcell("rate", e = 1, pt = 0, per = 1000, missing = "unknown"), class = "tabtools_error_cell_domain")
  expect_identical(as.character(tabcell("np", n = 0, d = 0)), "0")
  expect_identical(as.character(tabcell("enp", e = 0, n = 0)), "0/0")
  expect_error(tabcell("np", n = 0, d = 0, nocount = TRUE), class = "tabtools_error_cell_missing")
  expect_identical(as.character(tabcell("rate", e = 0, pt = 0, per = 1000, missing = "none")), "none")
  expect_error(tabcell("est", b = 1000, ll = 999, ul = 1001, eform = TRUE, missing = "none"),
               class = "tabtools_error_cell_interval")
  expect_error(tabcell("est", b = 1, se = 0), class = "tabtools_error_cell_missing")
  expect_identical(as.character(tabcell("est", b = 1, se = 0, missing = "not estimable")), "not estimable")
})

test_that("vector rendering recycles only scalars and moves full provenance", {
  values <- c(12345, 3, 8)
  vector <- tabcell("n", n = values, mincell = 5)
  scalar <- vapply(values, function(x) as.character(tabcell("n", n = x, mincell = 5)), "")
  expect_identical(as.character(vector), unname(scalar))
  expect_error(tabcell("np", n = 1:3, d = c(10, 20)), class = "tabtools_error_cell_length")
  expect_error(tabcell("n", n = numeric()), class = "tabtools_error_cell_source")
  selected <- vector[c(3, 2, 2)]
  expect_identical(as.character(selected), c("8", "<5", "<5"))
  expect_identical(attr(selected, "provenance")$rows$source_index, c(3L, 2L, 2L))
  expect_identical(attr(selected, "provenance")$smallcells$n_masked, 2L)
  changed <- vector
  changed[2L] <- "3"
  expect_error(as.character(changed), class = "tabtools_error_cell_provenance")
})

test_that("formats and literal separators obey the shared contract", {
  expect_error(tabcell("est", b = 1, ll = 0, ul = 2, format = "%9.2f", digits = 2),
               class = "tabtools_error_format_conflict")
  expect_error(tabcell("est", b = 1, ll = 0, ul = 2, format = "%9.2f", cformat = "%9.2f"),
               class = "tabtools_error_cell_format")
  expect_error(tabcell("est", b = 1, ll = 0, ul = 2, cformat = "%9,2f"),
               class = "tabtools_error_format_separator")
  expect_identical(as.character(tabcell("est", b = 1, ll = 0, ul = 2, cformat = "%9,2f", sep = "; ")),
                   "1,00 (0,00; 2,00)")
  expect_identical(as.character(tabcell("est", b = 1, ll = 0, ul = 2, sep = " $x `q` ")),
                   "1.00 (0.00 $x `q` 2.00)")
  expect_error(tabcell("p", p = .5, digits = 2), class = "tabtools_error_cell_form")
  expect_error(tabcell("np", n = 1, d = 10, level = .9), class = "tabtools_error_cell_form")
  expect_identical(as.character(tabcell("p", p = c(.0123, .0001), pstyle = "footnote")),
                   c("p = 0.012", "p < 0.001"))
  expect_identical(as.character(tabcell("p", p = .999, pstyle = "Pfootnote")), "P > 0.99")
})

test_that("matrix and supplied contrast sources retain declared inference", {
  source <- rbind(a = c(b = 1, ll = 0, ul = 2), z = c(b = -2, ll = -3, ul = -1))
  expect_identical(as.character(tabcell("est", matrix = source, row = "z")), "-2.00 (-3.00, -1.00)")
  expect_error(tabcell("est", matrix = source, row = "z", level = .95), class = "tabtools_error_cell_source")
  contrast <- list(estimate = 2, conf.low = 0, conf.high = 4, std.error = 1,
                   df = 2, conf.level = .95, effect_scale = "coefficient", p.value = .03)
  supplied <- tabcell("est", contrast = contrast)
  expect_identical(as.character(supplied), "2.00 (0.00, 4.00)")
  expect_identical(attr(supplied, "provenance")$inference[[1L]], contrast)
  changed <- tabcell("est", contrast = contrast, level = .9)
  numerical <- attr(changed, "provenance")$publication
  expect_equal(numerical$lb, 2 - qt(.95, 2), tolerance = 1e-14)
  expect_equal(numerical$ub, 2 + qt(.95, 2), tolerance = 1e-14)
  ratio <- list(estimate = 2, conf.low = 1, conf.high = 4, std.error = .2,
                df = Inf, conf.level = .95, effect_scale = "ratio", se_scale = "log")
  expect_identical(as.character(tabcell("est", contrast = ratio, scale = 10)), "20.00 (10.00, 40.00)")
  expect_error(tabcell("est", contrast = ratio, eform = TRUE), class = "tabtools_error_cell_source")
  scaled <- tabcell("est", b = log(2), ll = 0, ul = log(4), eform = TRUE, scale = 10)
  expect_equal(attr(scaled, "provenance")$publication$estimate, 20, tolerance = 1e-14)
})

test_that("cell sources compose through parent tables with aligned row maps", {
  cells <- tabcell("n", n = c(3, 10, 20), mincell = 5)
  direct <- puttab(cells, xlsx = NULL, markdown = NULL)
  expect_identical(direct$body[[1L]], c("<5", "10", "20"))
  expect_equal(direct$meta$cell_provenance[[1L]]$body_rows, 1:3)
  frame <- data.frame(label = c("a", "b", "c"), value = cells)
  selected <- puttab(frame, subset = c(1, 3), xlsx = NULL, markdown = NULL)
  expect_identical(selected$body[[2L]], c("<5", "20"))
  expect_equal(selected$meta$cell_provenance[[1L]]$source_rows, c(1, 3))
  stacked <- tt_stack(selected, selected, groups = c("first", "second"))
  expect_equal(stacked$meta$cell_provenance[[1L]]$body_rows, c(2, 3))
  expect_equal(stacked$meta$cell_provenance[[2L]]$body_rows, c(5, 6))
  expect_identical(stacked$meta$cell_provenance[[1L]]$provenance$rows$source_id,
                   stacked$meta$cell_provenance[[2L]]$provenance$rows$source_id)
  expect_error(tt_flat(cells), class = "tabtools_error_flat")
})

test_that("fitted coefficients retain model-owned Student inference", {
  fit <- lm(y ~ 1, data = data.frame(y = c(0, 2, 4)))
  result <- tabcell("est", model = fit, term = "(Intercept)")
  numerical <- attr(result, "provenance")$publication
  # t_2(.975) = sqrt(722/39); unbiased variance is four.
  expect_equal(numerical$estimate, 2, tolerance = 1e-14)
  expect_equal(numerical$lb, 2 - sqrt(722/39) * 2/sqrt(3), tolerance = 1e-13)
  expect_equal(numerical$ub, 2 + sqrt(722/39) * 2/sqrt(3), tolerance = 1e-13)
  expect_identical(attr(result, "provenance")$rows$reference, "t")
  expect_equal(attr(result, "provenance")$rows$df, 2)
  explicit <- tabcell("est", b = 2, se = 2/sqrt(3))
  expect_gt(attr(explicit, "provenance")$publication$lb, numerical$lb)
  expect_error(tabcell("est", model = fit, term = "missing"), class = "tabtools_error_cell_source")
})

test_that("parent console and every file sink share final protected text", {
  skip_if_not_installed("openxlsx")
  directory <- withr::local_tempdir(pattern = "tabtools-cell-sinks-")
  workbook <- file.path(directory, "cells.xlsx")
  markdown <- file.path(directory, "cells.md")
  csv <- file.path(directory, "cells.csv")
  cells <- tabcell("np", n = c(3, 0), d = 40, ci = "exact", mincell = 5)
  frame <- data.frame(label = c("masked", "zero"), value = cells)
  parent <- suppressMessages(puttab(frame, xlsx = workbook, markdown = markdown,
    csv = csv, sheet = "Cells", footnote = c("First paragraph", "Second paragraph")))
  expected <- as.character(cells)
  expect_identical(parent$body[[2L]], expected)
  csv_data <- read.csv(csv, check.names = FALSE, colClasses = "character")
  expect_true(all(expected %in% unlist(csv_data, use.names = FALSE)))
  md <- paste(readLines(markdown, warn = FALSE), collapse = "\n")
  expect_true(all(vapply(expected, function(x) grepl(x, md, fixed = TRUE), logical(1))))
  xlsx_data <- openxlsx::read.xlsx(workbook, sheet = "Cells", colNames = FALSE,
                                  skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  expect_true(all(expected %in% unlist(xlsx_data, use.names = FALSE)))
  console <- paste(capture.output(print(parent)), collapse = "\n")
  expect_true(all(vapply(expected, function(x) grepl(x, console, fixed = TRUE), logical(1))))
  expect_match(md, "First paragraph", fixed = TRUE)
  expect_match(md, "Second paragraph", fixed = TRUE)
  expect_true(all(is.na(parent$meta$cell_provenance[[1L]]$provenance$publication[1L,
    c("count", "pct", "lb", "ub")])))
})

test_that("constructing cells leaves options, RNG and sink history unchanged", {
  before_options <- options()
  before_state <- as.list(.tt_sink_state)
  before_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  tabcell("n", n = c(3, 12), mincell = 5)
  expect_identical(options(), before_options)
  expect_identical(as.list(.tt_sink_state), before_state)
  after_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  expect_identical(after_seed, before_seed)
})


test_that("source boundaries reject malformed numeric shapes and protected resurrection", {
  enp <- tabcell("enp", e = 1, n = 3, mincell = 5)
  p <- attr(enp, "provenance")
  p$raw_inputs$n <- 3
  attr(enp, "provenance") <- p
  expect_error(as.character(enp), class = "tabtools_error_cell_provenance")
  expect_error(tabcell("n", n = matrix(1:4, 2)), class = "tabtools_error_cell_source")
  cells <- tabcell("n", n = c(0, 3))
  for (flag in c(TRUE, FALSE)) {
    frame <- data.frame(value = cells, stringsAsFactors = flag)
    expect_s3_class(frame$value, "tt_cell")
    expect_identical(as.character(frame$value), c("0", "3"))
  }
  expect_error(as.data.frame(cells, stringsAsFactors = NA), class = "tabtools_error_cell_provenance")
  source <- list(estimate = "bad", conf.low = 1, conf.high = 3,
                 df = Inf, conf.level = .95, effect_scale = "ratio")
  expect_error(tabcell("est", contrast = source), class = "tabtools_error_cell_source")
  source$estimate <- matrix(2, 1)
  expect_error(tabcell("est", contrast = source), class = "tabtools_error_cell_source")
  source$estimate <- 2
  source$std.error <- .2
  result <- tabcell("est", contrast = source)
  expect_true(is.na(attr(result, "provenance")$rows$se_scale))
  expect_identical(attr(result, "provenance")$inference[[1]], source)
  expect_error(tabcell("est", contrast = source, level = .9), class = "tabtools_error_cell_source")
})

test_that("union label provenance follows the first source containing each key", {
  a <- puttab(data.frame(label = tabcell("n", n = c(3, 10), mincell = 5), value = c("a", "b")),
              xlsx = NULL, markdown = NULL)
  b <- puttab(data.frame(label = tabcell("n", n = c(20, 2), mincell = 5), value = c("b2", "c")),
              xlsx = NULL, markdown = NULL)
  a$rows$key <- a$rows$var <- c("a", "b")
  b$rows$key <- b$rows$var <- c("b", "c")
  merged <- tt_merge(a, b)
  expect_identical(merged$body[[1]], c("<5", "10", "<5"))
  first <- merged$meta$cell_provenance[[1]]
  last <- merged$meta$cell_provenance[[2]]
  expect_identical(first$body_column, 1L)
  expect_identical(last$body_column, 1L)
  expect_identical(last$body_rows, 3L)
  expect_identical(last$source_rows, 2L)
  expect_identical(last$provenance$text, "<5")
  expect_identical(last$provenance$counts$N_selected, 1L)
  expect_identical(last$provenance$smallcells$n_masked, 1L)
  expect_true(is.na(last$provenance$raw_inputs$n))
})
