# Golden parity for wttab() (W15-W22, IMPLEMENTATION_PLAN.md task 7.12):
# cells, CSV and Markdown sinks, console, stored results, the numbers at
# full precision, and the styles of the golden sheet. Gated like the other
# goldens (golden_scenario_live(): golden_phase_done, or golden_done_phase7w).

for (id in golden_wttab_ids()) {
  local({
    id <- id
    test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
      skip_if_not_installed("tidyxl")
      skip_if_not_installed("haven")
      run_golden_wttab_scenario(id)
    })
  })
}
