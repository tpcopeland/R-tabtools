# Golden parity for effecttab (E scenarios, IMPLEMENTATION_PLAN.md tasks
# 7.7-7.11): cells, CSV and Markdown sinks, console, stored results, the
# styles of every golden sheet, and the frame characteristics. Gated like
# the other goldens (golden_scenario_live(): golden_phase_done).

for (id in golden_scenarios()$id[golden_scenarios()$command == "effecttab"]) {
  local({
    id <- id
    test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
      skip_if_not_installed("tidyxl")
      skip_if_not_installed("haven")
      call <- golden_scenario(id)$r_call
      for (pkg in c("marginaleffects", "WeightIt")) {
        if (grepl(paste0("\\b", pkg, "::"), call)) skip_if_not_installed(pkg)
      }
      run_golden_effecttab_scenario(id)
    })
  })
}

# The end-to-end scenarios again, from Stata's own numbers (<id>_input.csv,
# the r(table) rows its setup wrote): with the estimator differences out of
# the way every one is exact, including E22 and E23, whose Stata keys
# (1._at, 1.sex#0.highbp) a data frame reproduces (?effecttab: R's own
# marginaleffects rows show values instead, EO6), and E04, where R's
# marginaleffects estimand differs.
local({
  sinks <- ', xlsx = "@ID@.xlsx", sheet = "%s", csv = "@ID@.csv", markdown = "@ID@.md")'
  calls <- c(
    E01 = 'effecttab(golden_effect_input("E01"), data = golden_fixture("cohort"), effect = "ATE", title = "Table 6. Average Treatment Effect on CV Events (IPW)", tlabels = \'0 "SSRI" 1 "SNRI"\'',
    E03 = 'effecttab(golden_effect_input("E03"), data = golden_fixture("cohort"), type = "margins", effect = "Pr(CV Event)", title = "Table 8. Predicted Probability of CV Event by Treatment"',
    E04 = 'effecttab(golden_effect_input("E04"), data = golden_fixture("cohort"), type = "margins", effect = "AME", title = "Table 9. Average Marginal Effects on CV Event Risk", footnote = "AME = average marginal effect. Change in Pr(CV event) per unit change in covariate."',
    E12 = 'effecttab(golden_effect_input("E12"), data = golden_fixture("cattaneo2"), clean = TRUE, effect = "ATE", digits = 0',
    E13 = 'effecttab(golden_effect_input("E13"), data = golden_fixture("cattaneo2"), clean = TRUE',
    E14 = 'effecttab(golden_effect_input("E14"), data = golden_fixture("nhanes2"), effect = "AME"',
    E15 = 'effecttab(golden_effect_input("E15"), data = golden_fixture("nhanes2"), effect = "Pr(Diabetes)"',
    E16 = 'effecttab(golden_effect_input("E16"), data = golden_fixture("nhanes2"), effect = "AME", refcat = "Ref."',
    E17 = 'effecttab(golden_effect_input("E17"), data = golden_fixture("nhanes2"), effect = "AME", models = "M1 \\\\ M2"',
    E22 = 'effecttab(golden_effect_input("E22"), data = golden_fixture("nhanes2"), effect = "Pr(Diabetes)"',
    E23 = 'effecttab(golden_effect_input("E23"), data = golden_fixture("nhanes2"), effect = "Pr(Diabetes)"',
    W12 = 'effecttab(golden_effect_input("W12"), data = golden_fixture("cohort"), effect = "Pr(CV Event)", title = "Predicted risk of a CV event, IPTW"',
    W13 = 'effecttab(golden_effect_input("W13"), data = golden_fixture("cohort"), effect = "RD", footnote = "Robust variance; margins averaged with the IPTW weights."'
  )
  for (id in names(calls)) {
    local({
      id <- id
      call <- paste0(calls[[id]], sprintf(sinks, id))
      test_that(paste(id, "from Stata's r(table): exact"), {
        skip_if_not_installed("tidyxl")
        skip_if_not_installed("haven")
        run_golden_effecttab_scenario(id, r_call = call, mode = "exact")
      })
    })
  }
})
