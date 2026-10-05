# Golden parity for stratetab (S scenarios, IMPLEMENTATION_PLAN.md tasks 7.4
# and 7.6): cells, CSV and Markdown sinks, console, stored results, the
# styles of every golden sheet, and the frame characteristics. Gated like
# the other goldens (golden_scenario_live(): golden_phase_done).

for (id in golden_scenarios()$id[golden_scenarios()$command == "stratetab"]) {
  local({
    id <- id
    test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
      skip_if_not_installed("tidyxl")
      skip_if_not_installed("haven")
      run_golden_stratetab_scenario(id)
    })
  })
}

test_that("the demo blocks read from .dta files give the demo table (S01)", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("haven")
  sc <- golden_scenario("S01")
  if (!golden_scenario_live("S01", sc$phase)) skip(paste("Phase", sc$phase))
  out <- withr::local_tempdir()
  blocks <- golden_strate_blocks("S01")
  paths <- file.path(out, paste0("block", seq_along(blocks)))
  for (k in seq_along(blocks)) haven::write_dta(blocks[[k]], paste0(paths[k], ".dta"))
  # Stata's using() names, without the extension; one with it.
  paths[2] <- paste0(paths[2], ".dta")
  tt <- stratetab(paths, outcomes = 2, outlabels = c("CV Events", "Self-Harm"),
                  explabels = c("Male", "Female"), rateratio = TRUE, ratiodigits = 2, zebra = TRUE,
                  title = "Table 12. Incidence Rates per 1,000 Person-Years by Sex",
                  footnote = "IRR = incidence rate ratio, Female vs Male. CI by log-normal method.")
  expect_cells_match(tt, "S01")
  book <- file.path(out, "S01.xlsx")
  tt_write_xlsx(tt, book, sheet = "S01")
  expect_length(golden_compare_styles(book, "S01", golden_path("stratetab.xlsx"), "S01",
                                      got_width_offset = golden_r_width_offset), 0L)
})
