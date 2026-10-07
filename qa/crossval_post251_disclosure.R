library(testthat)
library(tabtools)
root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
if (!nzchar(root)) root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
sys.source(file.path(root, "qa", "helper-post251-native.R"), envir = environment())

p256_disclosure_data <- function(id) {
  if (id == "DS01") return(data.frame(g = rep(1:3, c(4, 9, 9)),
    v = c(3, 3, 1, 1, NA, 1, 1, 1, 3, 1, 1, 3, 1, 2, NA, 2, 1, NA, NA, 1, NA, 1)))
  if (id == "DS02") {
    d <- data.frame(g = rep(1:3, c(6, 13, 5)), b = 1)
    d$b[c(17:19, 24)] <- 0
    return(d)
  }
  if (id %in% c("DS03", "DS04")) {
    n <- c(14, 6, 6, 9); a <- c(2, 2, 1, 1); b <- c(0, 1, 0, 3); c <- c(6, 1, 1, 4)
    v <- unlist(lapply(seq_len(4), function(j) c(rep(1, a[j]), rep(2, b[j]), rep(3, c[j]), rep(NA_real_, n[j]-a[j]-b[j]-c[j]))))
    return(data.frame(g = rep(1:4, n), v = v, x = seq_len(35)))
  }
  if (id == "DS05") {
    m <- c(1, 4, 1, 4, 0); z <- c(1, 2, 0, 2, 0); o <- c(3, 6, 4, 8, 12)
    return(data.frame(g = rep(1:5, m+z+o),
      b = unlist(lapply(seq_len(5), function(j) c(rep(0, z[j]), rep(1, o[j]), rep(NA_real_, m[j]))))))
  }
  d <- data.frame(g = rep(1:3, c(4, 6, 6)), x = seq_len(16))
  d$x[c(4, 11, 12)] <- NA_real_
  d
}
p256_disclosure_args <- function(id) {
  list(vars = switch(id, DS01 = "v cat", DS02 = "b bin", DS03 = "x contn \\ v cat",
      DS04 = "x contn \\ v cat", DS05 = "b bin", "x contn"),
    by = "g", nopvalue = TRUE, smallcells = 3L,
    smallcells_mode = if (id %in% c("DS04", "DS08")) "primary" else "strict",
    slashN = id %in% paste0("DS0", 1:5), catrowperc = id == "DS02",
    missingsummary = id %in% paste0("DS0", 5:8),
    total = if (id == "DS05") "before" else if (id %in% c("DS02", "DS03", "DS04", "DS07")) "after" else "none")
}
for (id in sprintf("DS%02d", 1:8)) local({
  current <- id
  test_that(paste(current, "authentic 2.5.6 disclosure grid, masks and every sink"), {
    p256_clear(environment())
    proof <- p256_auth("disclosure")
    expect_identical(as.character(p256_vector(proof$oracle$case_ids)), sprintf("DS%02d", 1:8))
    h <- p256_helpers(); case <- p256_case(proof, current)
    scratch <- withr::local_tempdir(pattern = paste0("post251-", current, "-"))
    paths <- list(csv = file.path(scratch, "case.csv"), md = file.path(scratch, "case.md"), xlsx = file.path(scratch, "case.xlsx"))
    data <- p256_disclosure_data(current); before <- data
    p256_input(data, proof, current)
    args <- p256_disclosure_args(current)
    r <- do.call(table1_tc, c(list(data = data, sheet = "Table", csv = paths$csv,
      markdown = paths$md, xlsx = paths$xlsx), args))
    expect_identical(data, before)
    expect_s3_class(r, "tt_table"); expect_identical(r$command, "table1_tc")
    expect_identical(r$stored$sheet, "Table")
    primary <- args$smallcells_mode == "primary"
    note <- if (primary) p256_R_primary else p256_strict_note
    expect_identical(r$footnote, note, info = "independent complete publication paragraph")
    p256_table1_frame(r, proof, current)
    matrix <- p256_matrix(case$matrix)
    expect_identical(dimnames(r$stored$suppression), dimnames(matrix))
    expect_identical(dim(r$stored$suppression), dim(matrix))
    expect_equal(r$stored$suppression, matrix, tolerance = 0)
    counts <- utils::read.csv(file.path(proof$directory, "out", paste0(current, "-counts.csv")), check.names = FALSE)
    expect_identical(names(counts), c("id", "command_rc", "N_primary_suppressed", "N_secondary_suppressed", "N_derived_suppressed"))
    expect_identical(counts$id, current); expect_identical(counts$command_rc, 0L)
    for (name in names(counts)[-(1:2)]) expect_equal(r$stored[[name]], counts[[name]], tolerance = 0)
    expect_identical(r$stored$smallcells, list(threshold = 3L, mode = args$smallcells_mode,
      n_masked = as.integer(sum(matrix %in% c(1, 2))), n_linked = as.integer(sum(matrix == 3))))
    expect_identical(r$stored$smallcells_mode, if (primary) "primary" else "full")
    p256_stored(r, case, c("markdown_rows", "markdown_cols", "Dapa", "varlist"))
    expect_true(isTRUE(case$absent_table)); expect_null(r$stored$table)
    if (primary) {expect_type(r$stored$raw, "list"); expect_null(r$stored$raw$table)} else expect_null(r$stored$raw)
    p256_console(r, case, "disclosure", note)
    p256_sinks(r, paths, proof, current, h, note)
    if (current == "DS05") {
      # Physical value column3 is g=2 after Total and g=1. Its visible Missing
      # count is code0, but replacement would disclose the protected N.
      expect_error(do.call(table1_tc, c(list(data = data,
        cellreplace = list(list(row = "Missing", column = 3L, text = "4 (25)"))), args)),
        class = "tabtools_error_cellreplace_protected")
    }
  })
})
