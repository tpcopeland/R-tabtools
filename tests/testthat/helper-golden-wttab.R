# Golden runner for wttab() (W15-W22, IMPLEMENTATION_PLAN.md task 7.12).
#
# Stata tabtools has no wttab: the goldens come from the Stata reference
# program in qa/stata/golden_wttab.do, which computes every number with
# summarize/_pctile, formats it with Stata's string(), and writes the table
# with tabtools' puttab (workbook wttab.xlsx, sheet <id>). The runner checks
# the cells, the CSV and Markdown bytes, the console lines, puttab's stored
# results, the numbers themselves (r(W), r(trunc)) at full precision, and
# the styles of the golden sheet, for the call's own workbook and a
# re-render of the returned table.

golden_wttab_ids <- function() {
  sc <- golden_scenarios()
  sc$id[sc$command == "wttab"]
}

# A stored matrix of the golden in its row/column order (Stata's stripes
# are positional names: r1.., c1.., or the statistic keys).
golden_stored_matrix <- function(stored, name) {
  m <- stored[stored$name == name & stored$kind == "matrix", , drop = FALSE]
  if (!nrow(m)) return(NULL)
  rn <- unique(m$row)
  cn <- unique(m$col)
  out <- matrix(NA_real_, length(rn), length(cn), dimnames = list(rn, cn))
  out[cbind(match(m$row, rn), match(m$col, cn))] <- golden_stored_num(m$value)
  out
}

# Numbers at full precision: equal, or within `tol` relative (the mean, SD
# and ESS are sums in a different order in Stata and R).
golden_expect_numbers <- function(got, want, what, tol = 1e-12) {
  testthat::expect_identical(dim(got), dim(want), label = paste(what, "dimensions"))
  if (!identical(dim(got), dim(want))) return(invisible())
  g <- as.vector(got)
  w <- as.vector(want)
  testthat::expect_identical(is.na(g), is.na(w), label = paste(what, "missing cells"))
  ok <- !is.na(g) & !is.na(w)
  rel <- abs(g[ok] - w[ok]) / pmax(abs(w[ok]), 1)
  testthat::expect_true(all(rel <= tol), label = sprintf("%s within %g (max %g)", what, tol, max(c(0, rel))))
}

run_golden_wttab_scenario <- function(id) {
  sc <- golden_scenario(id)
  if (!golden_scenario_live(id, sc$phase)) testthat::skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  # Forward slashes: a Windows temp path pasted into the parsed r_call
  # would read as escapes ("'\\U' used without hex digits").
  sinks <- golden_code_path(out, id)
  code <- gsub("@ID@", sinks, sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  msgs <- character()
  printed <- utils::capture.output(
    tt <- withCallingHandlers(eval(parse(text = code), envir = env), message = function(m) {
      msgs <<- c(msgs, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    })
  )
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$command, "wttab")
  expect_cells_match(tt, id)

  # Console: the messages, with R's temporary paths as Stata named them.
  own <- paste0(sinks, ".xlsx")
  got <- gsub(own, "wttab.xlsx", c(printed, msgs), fixed = TRUE)
  got <- gsub(paste0(gsub("\\", "/", out, fixed = TRUE), "/"), "", got, fixed = TRUE)
  want <- golden_read_lines(golden_path(paste0(id, "_console.txt")))
  expect_identical(got, want)

  # Stored results: puttab's counts and the sheet, then the numbers.
  stored <- golden_read_stored(id)
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]), c("file", "csv", "markdown", "W", "trunc"))
  expect_stored_match(tt, id, fields = fields)
  golden_expect_numbers(unname(tt$stored$W), unname(golden_stored_matrix(stored, "W")), paste(id, "W"))
  tr <- golden_stored_matrix(stored, "trunc")
  if (is.null(tr)) {
    expect_null(tt$stored$trunc)
  } else {
    got_tr <- as.matrix(tt$stored$trunc[, c("lower_q", "upper_q", "lower", "upper", "n_low", "n_high")])
    # The cut-offs and counts are exact; lower_q/upper_q are the proportions
    # as given in R and p / 100 in Stata (99.9 / 100 is 0.99900000000000011).
    golden_expect_numbers(unname(got_tr[, 3:6, drop = FALSE]), unname(tr[, 3:6, drop = FALSE]), paste(id, "trunc"),
                          tol = 0)
    golden_expect_numbers(unname(got_tr[, 1:2, drop = FALSE]), unname(tr[, 1:2, drop = FALSE]),
                          paste(id, "trunc quantiles"), tol = 1e-15)
  }

  # Workbook: the sheet the call wrote, and a re-render of the table.
  want_book <- golden_path("wttab.xlsx")
  golden_expect_none(golden_compare_styles(own, id, want_book, id, got_width_offset = golden_r_width_offset),
                     paste("styles", id))
  again <- file.path(out, "again.xlsx")
  tabtools::tt_write_xlsx(tt, again, sheet = id)
  golden_expect_none(golden_compare_styles(again, id, want_book, id, got_width_offset = golden_r_width_offset),
                     paste("re-rendered", id))

  expect_sink_match(paste0(sinks, ".csv"), id, "csv")
  expect_sink_match(paste0(sinks, ".md"), id, "md")
  invisible(tt)
}
