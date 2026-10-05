# Golden runner for comptab and hrcomptab (C scenarios, IMPLEMENTATION_PLAN.md
# tasks 7.5 and 7.6). The Stata side builds the source frames in each
# scenario's setup (regtab/effecttab/stratetab with frame()); the R side
# builds the same tables in its r_call (regtab() of the same fits,
# effecttab() of the same matrices, stratetab() of the same blocks read by
# golden_strate_blocks()), then composes them.

# Source sheets a scenario's setup writes into the golden workbook (the
# demo_comptab.xlsx "S Binary" and "S Education" sheets, C01): R's r_call
# writes the same sheets with regtab() into its own workbook.
golden_comptab_source_sheets <- list(C01 = c("C01 S Binary", "C01 S Education"))

run_golden_comptab_scenario <- function(id) {
  sc <- golden_scenario(id)
  if (!golden_scenario_live(id, sc$phase)) testthat::skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  sinks <- golden_code_path(out, id)
  code <- gsub("@ID@", sinks, sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  printed <- utils::capture.output(tt <- eval(parse(text = code), envir = env))
  # Every sink was written, so the calls return invisibly and print nothing.
  expect_identical(printed, character())
  expect_s3_class(tt, "tt_table")
  # hrcomptab and comptab's rate mode both build an "hrcomptab" table.
  rate_mode <- sc$command == "hrcomptab" || grepl("golden_strate_blocks", sc$r_call, fixed = TRUE)
  expect_identical(tt$command, if (rate_mode) "hrcomptab" else "comptab")
  expect_cells_match(tt, id)

  # Console: R's listing is the printed table (the comparator drops Stata's
  # "Markdown exported to" and "Exported ... rows" lines).
  why <- golden_compare_console(utils::capture.output(print(tt)),
                                golden_read_lines(golden_path(paste0(id, "_console.txt"))))
  golden_expect_none(why, paste("console", id))

  # Stored results, file paths aside (R's are temporary). rateframe and
  # modelframes hold Stata's frame names; R's hold the arguments as written
  # (?comptab, "Differences from Stata").
  stored <- golden_read_stored(id)
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]),
                    c("xlsx", "csv", "markdown", "rateframe", "modelframes"))
  expect_stored_match(tt, id, fields = fields)

  # The workbook the call wrote, and the renderer alone from the table.
  want_book <- golden_path(paste0(sc$command, ".xlsx"))
  golden_expect_none(golden_compare_styles(paste0(sinks, ".xlsx"), id, want_book, id,
                                           got_width_offset = golden_r_width_offset),
                     paste("styles", id))
  again <- file.path(out, "again.xlsx")
  tabtools::tt_write_xlsx(tt, again, sheet = id)
  golden_expect_none(golden_compare_styles(again, id, want_book, id, got_width_offset = golden_r_width_offset),
                     paste("re-rendered", id))
  for (sh in golden_comptab_source_sheets[[id]]) {
    golden_expect_none(golden_compare_styles(paste0(sinks, ".xlsx"), sh, want_book, sh,
                                             got_width_offset = golden_r_width_offset),
                       paste("source sheet", sh))
  }
  expect_sink_match(paste0(sinks, ".csv"), id, "csv")
  expect_sink_match(paste0(sinks, ".md"), id, "md")

  # The frame characteristics of Stata's frame() (comptab.ado:1485-1496,
  # :2800-2808).
  df <- as.data.frame(tt)
  expect_identical(attr(df, "source"), tt$command)
  expect_equal(attr(df, "ci_level"), as.numeric(stored$value[stored$name == "ci_level"]))
  invisible(tt)
}

# Stata's eplotframe() of a composite (qa/stata/make_comptab_forest.do ->
# fixtures/comptab_forest/<case>.csv): text columns as written, blanks as
# "", numbers at %21.17g.
golden_comptab_forest <- function(case) {
  s <- utils::read.csv(test_path("fixtures", "comptab_forest", paste0(case, ".csv")), colClasses = "character",
                       na.strings = character(), strip.white = FALSE, encoding = "UTF-8")
  for (v in c("estimate", "ll", "ul", "pvalue")) s[[v]] <- suppressWarnings(as.numeric(s[[v]]))
  for (v in c("model", "source_row")) s[[v]] <- suppressWarnings(as.integer(s[[v]]))
  s
}

# R's as_forest_data() against Stata's eplotframe(), column by column:
# labels (indents included), row types, model index and label, section and
# source row exactly; the numbers at a relative 1e-6 (Cox fits by
# different optimisers). source_frame holds Stata's frame names and R's
# argument labels, so it is not compared.
expect_forest_matches <- function(got, want, what) {
  testthat::expect_identical(nrow(got), nrow(want), label = paste(what, "rows"))
  if (nrow(got) != nrow(want)) return(invisible())
  for (v in c("label", "rowtype", "model_label", "section")) {
    testthat::expect_identical(got[[v]], want[[v]], label = paste(what, v))
  }
  for (v in c("model", "source_row")) testthat::expect_identical(got[[v]], want[[v]], label = paste(what, v))
  for (v in c("estimate", "ll", "ul", "pvalue")) {
    testthat::expect_identical(is.na(got[[v]]), is.na(want[[v]]), label = paste(what, v, "missing"))
    ok <- !is.na(want[[v]])
    testthat::expect_true(all(abs(got[[v]][ok] - want[[v]][ok]) <= 1e-6 * abs(want[[v]][ok])), label = paste(what, v))
  }
}

# The model tables of golden C04 (binary and dose Cox models, three outcomes).
golden_c04_models <- function(labels = c("edss4", "edss6", "relapse")) {
  b <- golden_fixture("hrt_bin", factors = c("female", "education"))
  dd <- golden_fixture("hrt_dose", factors = c("dosecat", "female", "education"))
  fits <- function(dat, rhs) {
    lapply(c("edss4", "edss6", "relapse"), function(o) {
      survival::coxph(stats::as.formula(paste0("survival::Surv(time, ", o, ") ~ ", rhs)), data = dat, ties = "breslow")
    })
  }
  list(bin = regtab(fits(b, "hrt + age + female + education"), coef = "HR", nointercept = TRUE, models = labels),
       dose = regtab(fits(dd, "dosecat + age + female + education"), coef = "HR", nointercept = TRUE, models = labels))
}
