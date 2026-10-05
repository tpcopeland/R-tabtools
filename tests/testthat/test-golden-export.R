# Golden parity for puttab (P) and stacktab (K) (IMPLEMENTATION_PLAN.md
# task 7.6): cells, CSV and Markdown sinks, console, stored results, and
# the styles of every golden sheet. Gated like the other goldens
# (golden_scenario_live(): golden_phase_done).

for (id in golden_export_ids()) {
  local({
    id <- id
    test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
      skip_if_not_installed("tidyxl")
      skip_if_not_installed("haven")
      run_golden_export_scenario(id)
    })
  })
}

test_that("stacktab of the tt_tables themselves reproduces the demo composite (K01, K02)", {
  skip_if_not_installed("tidyxl")
  sc <- golden_scenario("K01")
  if (!golden_scenario_live("K01", sc$phase)) skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  blocks <- golden_hrt_blocks(file.path(out, "blocks.xlsx"))
  book <- file.path(out, "K01.xlsx")
  suppressMessages(tt <- stacktab(
    list(list(table = blocks$primary, label = "Any HRT use"),
         list(table = blocks$dose, label = "By estrogen dose")),
    xlsx = book, sheet = "K01", columnmerge = "B+C as \"aHR (95% CI)\"",
    title = "Table 2. Hormone Therapy and Recurrent Events",
    note = "aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age and comorbidities.",
    csv = file.path(out, "K01.csv"), markdown = file.path(out, "K01.md")
  ))
  expect_cells_match(tt, "K01")
  expect_sink_match(file.path(out, "K01.csv"), "K01", "csv")
  expect_sink_match(file.path(out, "K01.md"), "K01", "md")
  expect_length(golden_compare_styles(book, "K01", golden_path("K01.xlsx"), "K01",
                                      got_width_offset = golden_r_width_offset), 0L)
  # Without a workbook: the same table, nothing written.
  # Different exposures side by side: placed by position, with a warning
  # naming the rows that differ (review F7).
  expect_warning(tt2 <- stacktab(list(list(table = blocks$primary, rows = 1:3, label = "Primary model"),
                       list(table = blocks$dose, label = "Dose-response model")),
                  layout = "hstack",
                  columnmerge = c("B+C as \"aHR (95% CI)\"", "E+F as \"aHR (95% CI)\""),
                  title = "Table 2 supplement. Primary and dose-response estimates side by side"),
                 "row 2 \\(Any HRT / Low dose\\)")
  expect_cells_match(tt2, "K02")
  expect_null(tt2$stored$sheet)
})
