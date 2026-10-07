library(testthat)
library(tabtools)

# No native processes or fixture updates. Root authenticates the external capture.
tc_native_dir <- function() {
  directory <- Sys.getenv("TABTOOLS_TABCELL_NATIVE_DIR")
  if (!nzchar(directory) || !dir.exists(directory)) {
    stop("TABTOOLS_TABCELL_NATIVE_DIR must name a root-authenticated TC capture.")
  }
  files <- utils::read.csv(file.path(directory, "ARTIFACTS.csv"), colClasses = "character")
  if (!identical(names(files), c("file", "md5")) || !nrow(files) ||
      anyNA(files) || anyDuplicated(files$file) || any(grepl("(^/|[.][.]|\\\\)", files$file))) {
    stop("Invalid authenticated artifact inventory.")
  }
  expect_identical(unname(tools::md5sum(file.path(directory, files$file))), files$md5)
  source <- utils::read.csv(file.path(directory, "SOURCE.csv"), colClasses = "character")
  if (!identical(names(source), c("key", "value")) || anyDuplicated(source$key)) {
    stop("Invalid authenticated native source record.")
  }
  info <- stats::setNames(source$value, source$key)
  expect_identical(unname(info["native_source_commit"]), "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129")
  expect_identical(readLines(file.path(directory, "runtime.txt")), c("17", "mt64"))
  directory
}

tc_read <- function(directory, file) {
  path <- file.path(directory, file)
  inventory <- utils::read.csv(file.path(directory, "ARTIFACTS.csv"), colClasses = "character")
  if (!file %in% inventory$file) stop("Required native control is not authenticated: ", file)
  if (!file.exists(path)) stop("Required native control missing: ", file)
  utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE)
}

tc_returns <- function(directory, id, suffix = "_stored.csv") {
  out <- tc_read(directory, paste0(id, suffix))
  expect_identical(names(out), c("name", "kind", "row", "col", "value"))
  expect_identical(out$value[out$name == "_golden_id"], id)
  expect_identical(out$value[out$name == "_golden_tabtools_version"], "2.5.1")
  expect_identical(out$value[out$name == "_golden_stata_version"], "17")
  out
}

tc_num <- function(value) {
  missing <- grepl("^[.]([a-z])?$", value)
  out <- rep(NA_real_, length(value))
  out[!missing] <- as.numeric(value[!missing])
  out
}

tc_scalars <- function(ret) {
  scalar <- ret[ret$kind == "scalar", , drop = FALSE]
  if (anyDuplicated(scalar$name)) stop("Duplicated native scalar.")
  stats::setNames(as.list(tc_num(scalar$value)), scalar$name)
}

tc_leaf <- function(directory, id, result, source = NULL) {
  ret <- tc_returns(directory, id)
  prov <- attr(result, "provenance", exact = TRUE)
  expect_length(result, 1L)
  macro <- ret[ret$kind == "macro", , drop = FALSE]
  expect_false(anyDuplicated(macro$name) > 0L)
  expected <- c(cell = as.character(result), form = prov$rows$form)
  # Stata does not retain a zero-length returned macro: its absence denotes
  # the blank cell rather than a missing rendered value. Nonblank cells
  # still require the exact named macro and literal publication text.
  if (identical(as.character(result), "")) expected <- expected[names(expected) != "cell"]
  projected_source <- prov$rows$native_source
  if (!is.null(source)) expect_identical(projected_source, source, info = id)
  if (!is.na(projected_source)) expected <- c(expected, source = projected_source)
  if (prov$rows$form == "rate" || (prov$rows$form == "np" && prov$rows$method == "exact")) {
    expected <- c(expected, citype = prov$rows$method)
  }
  expect_identical(sort(macro$name), sort(names(expected)), info = id)
  expect_identical(stats::setNames(macro$value, macro$name)[names(expected)], expected, info = id)
  scalars <- prov$native_returns[[1L]]
  actual <- tc_scalars(ret)
  expect_identical(sort(names(actual)), sort(names(scalars)), info = id)
  for (name in names(scalars)) {
    if (identical(id, "TC004_normal") && name %in% c("estimate", "lb", "ub")) {
      # Stata 17 and R converge with slightly different binomial coefficients.
      # Qualify only these fitted values with the stated mixed 2e-8 bound;
      # literal publication, inventories and all arithmetic controls stay exact.
      expect_true(is.finite(actual[[name]]) && is.finite(scalars[[name]]), info = paste(id, name))
      expect_true(abs(actual[[name]] - scalars[[name]]) <=
                    2e-8 * max(1, abs(actual[[name]])), info = paste(id, name))
    } else {
      expect_equal(actual[[name]], scalars[[name]], tolerance = 2e-8, info = paste(id, name))
    }
  }
  expect_identical(sort(unique(ret$kind)), sort(c("meta", "scalar", "macro")), info = id)
  invisible(ret)
}

test_that("TC001 seven scalar forms expose exact text and immediate native returns", {
  directory <- tc_native_dir()
  cells <- list(
    TC001_est = tabcell("est", b = -2, ll = -3, ul = -1, sep = " to "),
    TC001_se = tabcell("est", b = .2, se = .1, level = .9, eform = TRUE, scale = 10),
    TC001_p = tabcell("p", p = .0004, pstyle = "Pfootnote"),
    TC001_n = tabcell("n", n = 12345),
    TC001_np = tabcell("np", n = 12, d = 74),
    TC001_enp = tabcell("enp", e = 12, n = 74),
    TC001_iqr = tabcell("iqr", median = 20, q1 = 18, q3 = 25, digits = 0),
    TC001_rate = tabcell("rate", e = 12, pt = 975.6, per = 1000, ci = "poisson", level = .9, digits = 2))
  for (id in names(cells)) tc_leaf(directory, id, cells[[id]],
    source = if (startsWith(id, "TC001_est") || id == "TC001_se") "numbers" else NULL)
})

test_that("TC002 endpoint intervals and masked native numerics stay distinct", {
  directory <- tc_native_dir()
  cells <- list(
    TC002_np0 = tabcell("np", n = 0, d = 20, ci = "exact"),
    TC002_npfull = tabcell("np", n = 20, d = 20, ci = "exact"),
    TC002_npmask = tabcell("np", n = 3, d = 20, ci = "exact", mincell = 5),
    TC002_npdash = tabcell("np", n = 3, d = 20, ci = "exact", mincell = 5, nocount = TRUE),
    TC002_npzero = tabcell("np", n = 0, d = 0, ci = "exact"),
    TC002_npempty = tabcell("np", n = 0, d = 0, ci = "exact", nocount = TRUE, missing = ""),
    TC002_rate0 = tabcell("rate", e = 0, pt = 100, per = 1000),
    TC002_rate0wald = tabcell("rate", e = 0, pt = 100, per = 1000, ci = "poisson"),
    TC002_ratemask = tabcell("rate", e = 3, pt = 100, per = 1000, mincell = 5),
    TC002_ratemaskwald = tabcell("rate", e = 3, pt = 100, per = 1000, ci = "poisson", mincell = 5),
    TC002_rateempty = tabcell("rate", e = 0, pt = 0, per = 1000, missing = ""),
    TC002_enpevent = tabcell("enp", e = 3, n = 40, mincell = 5),
    TC002_enptotal = tabcell("enp", e = 1, n = 3, mincell = 5))
  for (id in names(cells)) tc_leaf(directory, id, cells[[id]])
  for (id in c("TC002_npmask", "TC002_npdash", "TC002_ratemask", "TC002_ratemaskwald")) {
    prov <- attr(cells[[id]], "provenance")
    expect_identical(prov$rows$status, "masked")
    expect_false(prov$rows$missing)
    expect_true(all(is.na(prov$publication)))
    expect_true(all(is.na(prov$raw_inputs[1L])))
  }
  expect_equal(attr(cells$TC002_rate0, "provenance")$publication$ub,
    -log(.025) * 10, tolerance = 2e-12)
})

test_that("TC003 literal text, missing inputs and native failures have honest boundaries", {
  directory <- tc_native_dir()
  literal <- "missing $TC_SENTINEL `absent' \"quote\""
  separator <- " | $TC_SENTINEL `absent' \"quote\" | "
  cells <- list(TC003_empty = tabcell("est", b = 2, ll = NA_real_, ul = NA_real_, missing = ""),
    TC003_zeroSE = tabcell("est", b = 2, se = 0, missing = "no estimate"),
    TC003_literal = tabcell("est", b = NA_real_, ll = NA_real_, ul = NA_real_, missing = literal),
    TC003_sep = tabcell("est", b = 2, ll = 1, ul = 3, sep = separator, cformat = "%9.3f"))
  for (id in names(cells)) tc_leaf(directory, id, cells[[id]], source = "numbers")
  empty <- attr(cells$TC003_empty, "provenance")
  expect_equal(empty$native_returns[[1L]]$estimate, 2)
  expect_true(all(is.na(empty$publication)))
  errors <- tc_read(directory, "TC003_errors.csv")
  expect_identical(errors$id, c("missing", "reversed", "pdomain", "npfraction", "ratefraction",
    "exposure", "format", "level", "stale_lincom"))
  expect_identical(errors$rc, c("459", "198", "198", "459", "459", "198", "198", "198", "301"))
  expect_error(tabcell("est", b = NA_real_, ll = NA_real_, ul = NA_real_), class = "tabtools_error_cell_missing")
  expect_error(tabcell("est", b = 2, ll = 3, ul = 1), class = "tabtools_error_cell_interval")
  expect_error(tabcell("p", p = 2), class = "tabtools_error_cell_domain")
  expect_error(tabcell("np", n = .5, d = 20, ci = "exact"), class = "tabtools_error_cell_domain")
  expect_error(tabcell("rate", e = .5, pt = 100, per = 1000), class = "tabtools_error_cell_domain")
  expect_error(tabcell("rate", e = 1, pt = 0, per = 1000), class = "tabtools_error_cell_domain")
  expect_error(tabcell("est", b = 2, ll = 1, ul = 3, format = "%9.2f", cformat = "%9.2f"), class = "tabtools_error_cell_format")
  expect_error(tabcell("est", b = 2, ll = 1, ul = 3, level = .9), class = "tabtools_error_cell_source")
  # Deliberate P.2 refusal: native warns, R refuses ambiguous comma intervals.
  expect_error(tabcell("est", b = 2, ll = 1, ul = 3, format = "%9,2f"))
})

test_that("TC004 fitted t/normal, matrix and genuine contrasts retain source inference", {
  directory <- tc_native_dir()
  i <- seq_len(24L)
  d <- data.frame(x = i %% 6 - 2, z = floor((i - 1)/6))
  d$y <- 2 + .3*d$x + .5*d$z + .2*((i %% 2)*2-1)
  d$event <- as.integer(i %% 4 == 0 | i %% 7 == 0)
  expect_equal(as.matrix(utils::read.csv(file.path(directory, "TC004_input.csv"), na.strings = c("", "."))), as.matrix(d), tolerance = 2e-12)
  fit <- stats::lm(y ~ x + z, d)
  tc_leaf(directory, "TC004_t", tabcell("est", model = fit, term = "x"), source = "e()")
  normal <- stats::glm(event ~ x + z, d, family = stats::binomial(),
    control = stats::glm.control(epsilon = 1e-12, maxit = 100L))
  tc_leaf(directory, "TC004_normal", tabcell("est", model = normal, term = "x", eform = TRUE), source = "e()")
  combination <- c(0, 1, 2)
  estimate <- sum(combination * stats::coef(fit))
  se <- sqrt(drop(t(combination) %*% stats::vcov(fit) %*% combination))
  df <- stats::df.residual(fit)
  original <- tc_returns(directory, "TC004_lincom_input", suffix = ".csv")
  native <- tc_scalars(original)
  expect_equal(native$estimate, estimate, tolerance = 2e-12)
  expect_equal(native$se, se, tolerance = 2e-12)
  expect_equal(native$df, df, tolerance = 0)
  expect_equal(native$p, 2*stats::pt(-abs(estimate/se), df), tolerance = 2e-12)
  expect_equal(native$t, estimate/se, tolerance = 2e-12)
  contrast <- list(estimate = estimate, std.error = se, df = df, conf.level = .95,
    conf.low = estimate - stats::qt(.975, df)*se, conf.high = estimate + stats::qt(.975, df)*se,
    p.value = 2*stats::pt(-abs(estimate/se), df), statistic = estimate/se,
    effect_scale = "coefficient", se_scale = "coefficient", native_source = "lincom")
  cell <- tabcell("est", contrast = contrast, level = .9, eform = TRUE, scale = 10)
  tc_leaf(directory, "TC004_lincom", cell, source = "lincom")
  expect_identical(attr(cell, "provenance")$inference[[1L]], contrast)
  nl <- tc_returns(directory, "TC004_nlcom_input", suffix = ".csv")
  nb <- nl[nl$name == "b" & nl$kind == "matrix", ]
  nv <- nl[nl$name == "V" & nl$kind == "matrix", ]
  expect_equal(tc_num(nb$value), estimate, tolerance = 2e-12)
  expect_equal(sqrt(tc_num(nv$value)), se, tolerance = 2e-8)
  expect_false("df" %in% names(tc_scalars(nl)))
  native_df <- nl[nl$name == "table" & nl$kind == "matrix" & nl$row == "df", ]
  expect_identical(nrow(native_df), 1L)
  expect_true(is.na(tc_num(native_df$value)))
  nonlinear <- list(estimate = estimate, std.error = se, df = Inf,
    conf.level = .95, effect_scale = "coefficient", native_source = "nlcom")
  tc_leaf(directory, "TC004_nlcom", tabcell("est", contrast = nonlinear), source = "nlcom")
  matrix <- matrix(c(2, 4, 1, 2, 3, 6), 2, dimnames = list(c("first", "second"), c("b", "ll", "ul")))
  tc_leaf(directory, "TC004_matrix", tabcell("est", matrix = matrix, row = "second"), source = "matrix")
  tc_leaf(directory, "TC004_cols", tabcell("est", matrix = matrix, row = 1, cols = 1:3), source = "matrix")
})

tc_vector_inputs <- function() {
  a <- c(0, 3, 20, NA_real_, 0, 5)
  list(rowid = 1:6, a = a, total = c(20,20,20,20,0,20), exposure = c(100,100,100,100,0,100),
       pv = c(0,.0004,.1,NA_real_,.999,1), lo = a-1, hi = a+1)
}

test_that("TC005 every generated form has aligned selected text and native row counters", {
  directory <- tc_native_dir()
  d <- tc_vector_inputs()
  expect_equal(as.matrix(utils::read.csv(file.path(directory, "TC005_input.csv"), na.strings = c("", "."))),
    as.matrix(as.data.frame(d)), tolerance = 2e-12)
  j <- 2:6
  cells <- list(est = tabcell("est", b = d$a[j], ll = d$lo[j], ul = d$hi[j], missing = ""),
    p = tabcell("p", p = d$pv[j], pstyle = "footnote", missing = ""),
    n = tabcell("n", n = d$a[j], mincell = 5, missing = ""),
    np = tabcell("np", n = d$a[j], d = d$total[j], ci = "exact", mincell = 5, missing = ""),
    enp = tabcell("enp", e = d$a[j], n = d$total[j], mincell = 5, missing = ""),
    iqr = tabcell("iqr", median = d$a[j], q1 = d$lo[j], q3 = d$hi[j], missing = ""),
    rate = tabcell("rate", e = d$a[j], pt = d$exposure[j], per = 1000, mincell = 5, missing = ""))
  vectors <- tc_read(directory, "TC005_vectors.csv")
  expect_identical(names(vectors), c("rowid", paste0("v_", names(cells))))
  expect_identical(vectors$rowid, as.character(1:6))
  for (form in names(cells)) {
    ret <- tc_returns(directory, paste0("TC005_", form))
    expect_setequal(unique(ret$kind), c("meta", "scalar", "macro"))
    expect_setequal(ret$name[ret$kind == "scalar"], c("N", "N_missing"))
    expect_setequal(ret$name[ret$kind == "macro"], c("varname", "form"))
    expect_identical(ret$value[ret$name == "varname"], paste0("v_", form))
    expect_identical(ret$value[ret$name == "form"], form)
    prov <- attr(cells[[form]], "provenance")
    expect_equal(tc_scalars(ret)$N, prov$counts$N, tolerance = 0)
    expect_equal(tc_scalars(ret)$N_missing, prov$counts$N_missing, tolerance = 0)
    expect_identical(vectors[[paste0("v_", form)]], c("", as.character(cells[[form]])))
    expect_identical(prov$counts$N_selected, 5L)
  }
})

test_that("TC006 authentic puttab publication and sinks carry protected leaf provenance", {
  directory <- tc_native_dir()
  scratch <- withr::local_tempdir(pattern = "tabtools-tc-native-")
  d <- tc_vector_inputs()
  literal <- "missing $TC_SENTINEL `absent' \"quote\""
  cells <- tabcell("np", n = d$a, d = d$total, ci = "exact", mincell = 5, missing = literal)
  rates <- tabcell("rate", e = d$a, pt = d$exposure, per = 1000, mincell = 5, missing = literal)
  for (form in c("np", "rate")) {
    ret <- tc_returns(directory, paste0("TC006_", form, "_generated"), suffix = ".csv")
    expect_setequal(unique(ret$kind), c("meta", "scalar", "macro"))
    expected_cell <- if (form == "np") cells else rates
    prov <- attr(expected_cell, "provenance")
    expect_setequal(ret$name[ret$kind == "scalar"], c("N", "N_missing"))
    expect_setequal(ret$name[ret$kind == "macro"], c("varname", "form"))
    expect_identical(ret$value[ret$name == "form"], form)
    expect_identical(ret$value[ret$name == "varname"], paste0(form, "cell"))
    expect_equal(tc_scalars(ret)$N, prov$counts$N, tolerance = 0)
    expect_equal(tc_scalars(ret)$N_missing, prov$counts$N_missing, tolerance = 0)
  }
  parent <- data.frame(term = paste0("row", 1:6), npcell = cells, ratecell = rates)
  attr(parent$term, "label") <- "Row"
  attr(parent$npcell, "label") <- "Percent"
  attr(parent$ratecell, "label") <- "Rate"
  book <- file.path(scratch, "parent.xlsx")
  csv <- file.path(scratch, "parent.csv")
  md <- file.path(scratch, "parent.md")
  actual <- puttab(parent, xlsx = book, csv = csv, markdown = md, sheet = "Leaf",
    title = "Leaf publication", footnote = "Protected cells stay protected.",
    varlabels = TRUE, headershade = TRUE, zebra = TRUE, borderstyle = "academic", boldrows = 2, hlines = 3)
  expected <- tc_read(directory, "TC006_parent_stored.csv")
  expect_identical(expected$value[expected$name == "_golden_id"], "TC006_parent")
  numeric <- tc_scalars(expected)
  expect_setequal(names(numeric), c("n_rows", "n_cols", "n_datarows", "n_panels", "n_spans",
                                   "markdown_rows", "markdown_cols"))
  macros <- expected[expected$kind == "macro", , drop = FALSE]
  expect_setequal(macros$name, c("source", "sheet", "file", "csv", "markdown"))
  expect_identical(macros$value[macros$name == "source"], "data")
  expect_identical(macros$value[macros$name == "sheet"], "Leaf")
  for (name in c("file", "csv", "markdown")) {
    expect_identical(basename(macros$value[macros$name == name]),
                     switch(name, file = "TC006.xlsx", csv = "TC006.csv", markdown = "TC006.md"))
  }
  expect_setequal(unique(expected$kind), c("meta", "scalar", "macro"))
  for (name in c("n_rows", "n_cols", "n_datarows", "n_panels", "n_spans", "markdown_rows", "markdown_cols")) {
    expect_equal(actual$stored[[name]], numeric[[name]], tolerance = 0, info = name)
  }
  grid <- function(path) as.matrix(utils::read.csv(path, header = FALSE, colClasses = "character",
    na.strings = character(), check.names = FALSE))
  expect_identical(unname(grid(csv)), unname(grid(file.path(directory, "TC006.csv"))))
  bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)
  expect_identical(bytes(md), bytes(file.path(directory, "TC006.md")))
  helpers <- new.env(parent = globalenv())
  sys.source(test_path("..", "tests", "testthat", "helper-golden.R"), envir = helpers)
  expect_identical(helpers$golden_compare_styles(book, "Leaf", file.path(directory, "TC006.xlsx"), "Leaf",
    mask = character(), got_width_offset = helpers$golden_r_width_offset), character())
  expect_identical(actual$command, "puttab")
  expect_length(actual$meta$cell_provenance, 2L)
  for (entry in actual$meta$cell_provenance) {
    expect_identical(entry$body_rows, 1:6)
    protected <- entry$provenance$rows$status == "masked"
    expect_true(any(protected))
    expect_true(all(is.na(entry$provenance$publication[protected, ])))
  }
  expect_error(tt_flat(actual), class = "tabtools_error_flat")
  expect_error(tt_flat(cells), class = "tabtools_error_cell")
  stacked <- tt_stack(actual, actual)
  expect_identical(stacked$body[[2L]], rep(as.character(cells), 2))
  expect_length(stacked$meta$cell_provenance, 4L)
  keyed_parent <- actual
  # Explicit manual row identities, owned by original input rowid; no models.
  keyed_parent$rows$key <- keyed_parent$rows$var <- paste0("leaf:", d$rowid)
  merged <- tt_merge(keyed_parent, keyed_parent)
  expect_identical(merged$body[[2L]], as.character(cells))
  expect_identical(merged$body[[4L]], as.character(cells))
  expect_length(merged$meta$cell_provenance, 4L)
  # Sinks are replayed from the parent table rather than a leaf writer.
  replay <- file.path(scratch, "replay.csv")
  tt_write_csv(actual, replay)
  expect_identical(bytes(replay), bytes(csv))
})
