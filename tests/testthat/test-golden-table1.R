# Golden parity for table1_tc (IMPLEMENTATION_PLAN.md §8). Each scenario is
# skipped until its phase is done (golden_phase_done in helper-golden.R). The
# full sweep (~20 s) runs off CRAN; a core subset always runs.
sc <- golden_scenarios()
core <- c("T01", "T04", "T13", "T33")
for (id in sc$id[sc$command == "table1_tc"]) {
  test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
    if (!id %in% core) skip_on_cran()
    run_golden_scenario(id, patch = golden_strip_smd_note)
  })
}
