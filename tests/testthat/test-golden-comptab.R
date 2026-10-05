# Golden parity for comptab and hrcomptab (C scenarios, IMPLEMENTATION_PLAN.md
# tasks 7.5 and 7.6): cells, CSV and Markdown sinks, console, stored results,
# the styles of every golden sheet (C01's regtab source sheets included), and
# the frame characteristics. Gated like the other goldens
# (golden_scenario_live(): golden_phase_done).

for (id in golden_scenarios()$id[golden_scenarios()$command %in% c("comptab", "hrcomptab")]) {
  local({
    id <- id
    test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
      skip_if_not_installed("tidyxl")
      skip_if_not_installed("haven")
      if (grepl("survival::", golden_scenario(id)$r_call, fixed = TRUE)) skip_if_not_installed("survival")
      run_golden_comptab_scenario(id)
    })
  })
}

# Forest data (the composite's eplotframe()) against Stata's, which no
# golden covers (Phase 7c review; qa/stata/make_comptab_forest.do).
test_that("forest data of hrcomptab match Stata's eplotframe (PB: golden C04's tables)", {
  skip_if_not_installed("haven")
  skip_if_not_installed("survival")
  rates <- stratetab(golden_strate_blocks("C04"), outcomes = 3,
                     outlabels = c("Sustained EDSS 4", "Sustained EDSS 6", "Recurring Relapse"),
                     outcomeids = c("edss4", "edss6", "relapse"), explabels = c("Any HRT", "Estrogen Dose"))
  m <- golden_c04_models()
  x <- hrcomptab(rates, list(m$bin, m$dose), rows = list(1, 3:5), outcomemap = c("edss4", "edss6", "relapse"))
  f <- as_forest_data(x)
  expect_forest_matches(f, golden_comptab_forest("PB"), "PB")
  expect_identical(unique(f$source_frame), c("rates", "m$bin", "m$dose"))
  # Deviation 6: a coxph() fit's outcome identity is its event variable, so
  # the rate table's outcomeids match the models without outcomemap (Stata's
  # stcox models are all `_t` and need it).
  y <- hrcomptab(rates, list(m$bin, m$dose), rows = list(1, 3:5))
  expect_identical(tabtools:::.tt_cells(y), tabtools:::.tt_cells(x))
  expect_identical(attr(as.data.frame(m$bin), "outcome_id"), c("edss4", "edss6", "relapse"))
  # Model labels that name no outcome are not used without outcomemap.
  lab <- golden_c04_models(c("A", "B", "C"))
  expect_identical(tabtools:::.tt_cells(hrcomptab(rates, list(lab$bin, lab$dose), rows = list(1, 3:5))),
                   tabtools:::.tt_cells(x))
})

test_that("forest data of a vertical comptab match Stata's, in the table's row order (PC: golden C05's tables)", {
  skip_if_not_installed("haven")
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort")
  cx <- function(rhs) survival::coxph(stats::as.formula(paste0("survival::Surv(follow_up, cv_event) ~ ", rhs)),
                                      data = d, ties = "breslow")
  a <- regtab(list(cx("treated + female"), cx("treated + female + index_age + diabetes")), coef = "HR",
              nointercept = TRUE, models = c("Crude", "Adjusted"))
  bb <- regtab(list(cx("treated + female + index_age + diabetes + hypertension"), cx("treated + female + hypertension")),
               coef = "HR", nointercept = TRUE, models = c("Adjusted", "Crude"))
  f <- as_forest_data(comptab(list(a, bb), rows = list(c(2, 1), 5), section = c("Frame A", "Frame B")))
  want <- golden_comptab_forest("PC")
  # rows(2 1) typed out of order: since tabtools 2.1.14 Stata's
  # eplotframe() follows the table too (each selected row once, in frame
  # order; comptab.ado:2546-2552), as R always did (task C8; 2.1.13 put
  # Female sex before Treatment group).
  expect_identical(want$source_row[2:5], c(1L, 1L, 2L, 2L))
  # Stata copies the source table's model index and label (frame B's model
  # 1 is Adjusted); R gives the composite's (codex audit F05, a deliberate
  # difference): frame B's models sit in the columns of frame A's order,
  # Crude then Adjusted, and the source index is kept in source_model.
  expect_identical(want$model_label[7:8], c("Adjusted", "Crude"))
  r_want <- want
  r_want[7:8, ] <- want[8:7, ]
  r_want$model[7:8] <- c(1L, 2L)
  expect_forest_matches(f, r_want, "PC")
  expect_identical(f$model_label[7:8], c("Crude", "Adjusted"))
  expect_identical(f$source_model[7:8], c(2L, 1L))
})

test_that("forest data with a reference on another base level match Stata's (PE: golden C09's rates)", {
  skip_if_not_installed("haven")
  skip_if_not_installed("survival")
  rates <- stratetab(golden_strate_blocks("C09"), outcomes = 1, outlabels = "Sustained EDSS 4", outcomeids = "edss4",
                     explabels = c("Any HRT", "Estrogen Dose"))
  b <- golden_fixture("hrt_bin")
  dd <- golden_fixture("hrt_dose", factors = "dosecat")
  dd$dosecat <- stats::relevel(dd$dosecat, "Medium dose")
  bin <- regtab(survival::coxph(survival::Surv(time, edss4) ~ hrt + age, data = b, ties = "breslow"), coef = "HR",
                nointercept = TRUE, level = 0.90, models = "EDSS 4")
  dose <- regtab(survival::coxph(survival::Surv(time, edss4) ~ dosecat + age, data = dd, ties = "breslow"), coef = "HR",
                 nointercept = TRUE, level = 0.90, models = "EDSS 4")
  x <- hrcomptab(rates, list(bin, dose), rownames = "hrt \\ no high low", outcomemap = "EDSS 4",
                 reflabel = "1.00 (ref)", effect = "HR")
  want <- golden_comptab_forest("PE")
  # Stata's ib2. keeps the dose rows in code order (No HRT 2, Low 3, High
  # 5); R's relevel() lists Medium dose first (No HRT 3, Low 4, High 5).
  dose_rows <- want$rowtype == "effect" & want$section == "Estrogen Dose"
  want$source_row[dose_rows] <- c(`  No HRT` = 3L, `  Low dose` = 4L, `  High dose` = 5L)[want$label[dose_rows]]
  expect_forest_matches(as_forest_data(x), want, "PE")
  expect_identical(as_forest_data(x)$rowtype[7], "reference")
})
