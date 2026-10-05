# Milestone H group t2b (tasks H2-H6, H19, H21; IMPLEMENTATION_PLAN.md,
# "Group t2b outcome"): Stata expectations from qa/stata/make_regtab_h_t2b.do
# (Stata 17 + tabtools 2.1.12) in tests/testthat/fixtures/regtab_h_t2b/, on
# the synthetic inputs of qa/make_regtab_h_t2b_data.R.

ht2b_path <- function(...) test_path("fixtures", "regtab_h_t2b", ...)

ht2b_read <- function(name) {
  d <- as.data.frame(haven::zap_formats(haven::read_dta(ht2b_path(paste0(name, ".dta")))))
  d[] <- lapply(d, function(x) {
    attr(x, "label") <- NULL
    as.vector(x)
  })
  d
}

ht2b_cells <- function(id) golden_read_cells_file(ht2b_path(paste0(id, ".csv")))

ht2b_stored <- function(id) {
  x <- utils::read.csv(ht2b_path(paste0(id, "_stored.csv")), colClasses = "character",
                       na.strings = character(), encoding = "UTF-8")
  x[!x$name %in% c("xlsx", "sheet", "markdown", "csv", "markdown_rows", "markdown_cols"), , drop = FALSE]
}

# e(b), standard errors and e() scalars (N, N_clust, ll, ll_0, r2_p, phi).
ht2b_e <- function(id) {
  x <- utils::read.csv(ht2b_path(paste0(id, "_e.csv")), colClasses = "character", strip.white = TRUE)
  num <- function(v) suppressWarnings(as.numeric(ifelse(v == ".", NA, v)))
  sc <- grepl("^e\\(", x$term)
  list(b = stats::setNames(num(x$b[!sc]), x$term[!sc]), se = stats::setNames(num(x$se[!sc]), x$term[!sc]),
       e = stats::setNames(num(x$b[sc]), sub("^e\\((.*)\\)$", "\\1", x$term[sc])))
}

# Cells exact and stored results within `tol` against a Stata case.
expect_ht2b <- function(tt, id, tol = 1e-9, drop_names = character()) {
  mm <- golden_compare_cells(golden_as_cells(tt), ht2b_cells(id), mode = "exact")
  if (nrow(mm)) golden_fail(mm, paste("cells", id)) else succeed()
  st <- golden_methods_regtab(ht2b_stored(id), id, "regtab_h_t2b")
  st <- st[!st$name %in% drop_names, , drop = FALSE]
  why <- golden_compare_stored(tt$stored, st, tolerance = tol)
  if (length(why)) fail(paste0("stored ", id, ":\n", paste(utils::head(why, 8L), collapse = "\n"))) else succeed()
  invisible(tt)
}

# Run R code in a fresh R session with this tabtools (the source tree under
# load_all(), else the installed package) and return its output lines.
ht2b_fresh_r <- function(code) {
  src <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  ns_path <- tryCatch(normalizePath(getNamespaceInfo("tabtools", "path"), mustWork = FALSE), error = function(e) "")
  load <- if (identical(ns_path, src) && nzchar(system.file(package = "pkgload"))) {
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE))", deparse(src))
  } else "suppressMessages(library(tabtools))"
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), load, code), script)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
                           stdout = TRUE, stderr = TRUE))
}

# The body of a regtab() call, or the error message.
ht2b_try <- function(expr) {
  tryCatch(expr$body, error = function(e) structure(conditionMessage(e), class = "ht2b_refusal"))
}

# regtab() output unchanged, or a refusal whose message matches `pattern`;
# never a changed table.
expect_same_or_refused <- function(got, want, pattern, label) {
  if (inherits(got, "ht2b_refusal")) {
    expect_match(unclass(got), pattern, label = paste(label, "(refusal)"))
  } else {
    expect_identical(got, want, label = label)
  }
}
