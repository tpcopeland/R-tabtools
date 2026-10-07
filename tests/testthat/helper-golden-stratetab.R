# Golden runner for stratetab (S scenarios, IMPLEMENTATION_PLAN.md tasks 7.4
# and 7.6). The Stata side writes one strate-shaped .dta file per block in
# its setup (qa/stata/golden_stratetab_blocks.do, or `strate` itself for
# S02); the R side builds the same blocks from the same CSV
# (fixtures/stratetab_blocks.csv) with golden_strate_blocks().

# The blocks of scenario `id` as strate-shaped data frames, in block order:
# a `group` column (a string, a labelled code, or an unlabelled number; none
# for cat_type "none"), then `_D _Y _Rate _Lower _Upper`, with strate's
# interval labels when label_level is set. The twin of
# golden_strate_blocks in qa/stata/golden_stratetab_blocks.do.
golden_strate_blocks <- function(id) {
  all <- utils::read.csv(test_path("fixtures", "stratetab_blocks.csv"), stringsAsFactors = FALSE,
                         colClasses = c(scenario = "character", cat_type = "character", cat = "character",
                                        label_level = "character"),
                         na.strings = "")
  all$cat[is.na(all$cat)] <- ""
  all$label_level[is.na(all$label_level)] <- ""
  d <- all[all$scenario == id, , drop = FALSE]
  if (!nrow(d)) stop("No stratetab blocks for scenario ", id, call. = FALSE)
  lapply(split(d, d$block), function(b) {
    b <- b[order(b$row), , drop = FALSE]
    type <- b$cat_type[1]
    out <- data.frame(`_D` = b$D, `_Y` = b$Y, `_Rate` = b$Rate, `_Lower` = b$Lower, `_Upper` = b$Upper,
                      check.names = FALSE)
    grp <- switch(type,
                  str = b$cat,
                  num = b$code,
                  lab = haven::labelled(b$code, stats::setNames(b$code, b$cat)),
                  none = NULL)
    if (!is.null(grp)) out <- cbind(data.frame(group = seq_len(nrow(out))), out)
    if (!is.null(grp)) out$group <- grp
    lev <- b$label_level[1]
    if (nzchar(lev)) {
      attr(out[["_Lower"]], "label") <- paste0("Lower ", lev, "% confidence limit")
      attr(out[["_Upper"]], "label") <- paste0("Upper ", lev, "% confidence limit")
    }
    out
  })
}

# Stata's workbook message after the listing ("Exported to stratetab.xlsx,
# sheet S01"). R prints none, as regtab and table1_tc do not (?stratetab,
# "Differences from Stata"): exactly that one line is dropped from the
# Stata side, and only when the scenario wrote the workbook.
golden_drop_stratetab_export <- function(want, id) {
  hit <- which(want == sprintf("Exported to stratetab.xlsx, sheet %s", id))
  testthat::expect_length(hit, 1L)
  if (length(hit)) want[-hit] else want
}

# Inventoried S01-S08 explicitly disable masking. Native threshold/no-time
# counters remain in the full stored comparison; the richer P.4 R list is a
# separately asserted public contract, not a replacement native oracle.
golden_assert_stratetab_mask_contract <- function(tt, id) {
  native <- golden_read_stored(id)
  threshold <- native$value[native$name == "smallcells" & native$kind == "scalar"]
  testthat::expect_identical(threshold, "0", label = paste(id, "authentic disabled-mask threshold"))
  testthat::expect_identical(tt$stored$smallcells,
    list(threshold = 0L, mode = "primary", n_masked = 0L, n_linked = 0L),
    label = paste(id, "complete canonical P.4 mask metadata"))
  invisible(tt)
}

run_golden_stratetab_scenario <- function(id) {
  sc <- golden_scenario(id)
  if (!golden_scenario_live(id, sc$phase)) testthat::skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  sinks <- golden_code_path(out, id)
  code <- gsub("@ID@", sinks, sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  printed <- utils::capture.output(tt <- eval(parse(text = code), envir = env))
  # Every sink was written, so the call returns invisibly and prints nothing.
  expect_identical(printed, character())
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$command, "stratetab")
  if (id == "S03") {
    # Full original current-native/raw/input checks precede the historical
    # comparator peer. Only its owned sink files are then re-rendered.
    tt <- golden_patch_s03_round(tt)
    tabtools::tt_write_csv(tt, paste0(sinks, ".csv"))
    tabtools::tt_write_markdown(tt, paste0(sinks, ".md"))
    tabtools::tt_write_xlsx(tt, paste0(sinks, ".xlsx"), sheet = id)
  }
  expect_cells_match(tt, id)

  # Console: R's listing is the printed table.
  want <- golden_drop_stratetab_export(golden_read_lines(golden_artifact_path(id, paste0(id, "_console.txt"))), id)
  parts <- golden_publication_console(utils::capture.output(print(tt)), want, id)
  why <- golden_compare_console(parts$got, parts$want)
  golden_expect_none(why, paste("console", id))

  # Stored results, file paths aside (R's are temporary); the Markdown
  # counts come from the Markdown file the call wrote.
  stored <- golden_read_stored(id)
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]), c("xlsx", "csv", "markdown"))
  golden_assert_stratetab_mask_contract(tt, id)
  expect_stored_match(tt, id, fields = fields)

  # The workbook the call wrote, and the renderer alone from the table.
  want_book <- golden_book(id)
  golden_expect_none(golden_compare_styles(paste0(sinks, ".xlsx"), id, want_book, id,
                                           got_width_offset = golden_r_width_offset, publication_id = id),
                     paste("styles", id))
  again <- file.path(out, "again.xlsx")
  tabtools::tt_write_xlsx(tt, again, sheet = id)
  golden_expect_none(golden_compare_styles(again, id, want_book, id, got_width_offset = golden_r_width_offset, publication_id = id),
                     paste("re-rendered", id))
  expect_sink_match(paste0(sinks, ".csv"), id, "csv", tt = tt)
  expect_sink_match(paste0(sinks, ".md"), id, "md", tt = tt)

  # The frame characteristics a composite reads (stratetab.ado:729-750),
  # against what Stata's r() says about the same table; a rate-ratio table
  # names its IRR column (tabtools 2.1.14).
  df <- as.data.frame(tt)
  sv <- function(n) stored$value[stored$name == n]
  expect_identical(attr(df, "source"), "stratetab")
  expect_identical(attr(df, "statistic_ids"),
                   paste0("events person_years rate_ci", if (identical(tt$meta$cols_per_outcome, 4L)) " irr_ci"))
  expect_equal(attr(df, "ci_level"), as.numeric(sv("ci_level")))
  expect_equal(attr(df, "n_outcomes"), as.numeric(sv("N_outcomes")))
  ids <- strsplit(sv("outcome_ids"), " \\ ", fixed = TRUE)[[1]]
  expect_identical(attr(df, "outcome_id"), ids)
  for (o in seq_along(ids)) expect_identical(attr(df, paste0("outcome_id_", o)), ids[o])
  invisible(tt)
}
