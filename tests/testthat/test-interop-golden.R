# Converters against Stata's golden workbooks, every scenario (Phase 6
# review P1-3): the table is built once with the scenario's r_call, then
# as_flextable() and tt_as_gt() are compared cell by cell with the workbook
# Stata wrote (expect_ft_matches_golden(), expect_gt_matches_golden() in
# helper-interop.R). Masks: the row-wise p mask, the Test/Statistic columns,
# and the two pinned knife-edge cells of R13/R14.

local({
  sc <- golden_scenarios()
  # puttab and stacktab scenarios have their own workbook geometry and
  # comparator (expect_export_converters_match(), helper-golden-export.R),
  # run at the end of this file.
  for (id in sc$id[sc$command %in% c("table1_tc", "regtab")]) {
    local({
      id <- id
      test_that(paste("as_flextable() and tt_as_gt() match Stata's workbook:", id), {
        skip_on_cran()
        skip_if_not_installed("flextable")
        skip_if_not_installed("gt")
        skip_if_not_installed("tidyxl")
        call <- golden_scenario(id)$r_call
        for (pkg in c("survival", "MASS", "nnet", "pscl", "lme4", "geepack", "glmmTMB", "ordinal", "cmprsk")) {
          if (grepl(paste0("\\b", pkg, "::"), call)) skip_if_not_installed(pkg)
        }
        # R25q: the known glm.nb iteration-limit warning (a boundary MLE; the
        # golden regtab test reports it).
        tt <- if (id == "R25q") suppressWarnings(interop_tt(id)) else interop_tt(id)
        if (golden_scenario(id)$command == "table1_tc") tt <- golden_strip_smd_note(tt)
        expect_ft_matches_golden(id, tt)
        expect_gt_matches_golden(id, tt)
        # Task 6.5: tt_as_gtsummary() keeps every cell's text.
        if (requireNamespace("gtsummary", quietly = TRUE)) expect_gts_keeps_cells(tt)
      })
    })
  }
})

# puttab (P) and stacktab (K): the converters against every golden sheet
# (Phase 7a review F4); wttab (W15-W22, task 7.12) is puttab's layout.
local({
  sc <- golden_scenarios()
  for (id in sc$id[sc$command %in% c("puttab", "stacktab", "wttab")]) {
    local({
      id <- id
      test_that(paste("as_flextable() and tt_as_gt() keep the workbook styling:", id), {
        skip_on_cran()
        skip_if_not_installed("flextable")
        skip_if_not_installed("gt")
        skip_if_not_installed("tidyxl")
        skip_if_not_installed("haven")
        if (!golden_scenario_live(id, golden_scenario(id)$phase)) skip("Phase 7")
        tt <- suppressWarnings(interop_tt(id))
        expect_export_converters_match(id, tt)
      })
    })
  }
})

# stratetab (S): the workbook geometry of table1_tc/regtab (header rows 2-3,
# body from row 4), so the same cell-by-cell comparators apply.
local({
  sc <- golden_scenarios()
  for (id in sc$id[sc$command == "stratetab"]) {
    local({
      id <- id
      test_that(paste("as_flextable() and tt_as_gt() match Stata's workbook:", id), {
        skip_on_cran()
        skip_if_not_installed("flextable")
        skip_if_not_installed("gt")
        skip_if_not_installed("tidyxl")
        skip_if_not_installed("haven")
        if (!golden_scenario_live(id, golden_scenario(id)$phase)) skip("Phase 7")
        tt <- interop_tt(id)
        expect_ft_matches_golden(id, tt)
        expect_gt_matches_golden(id, tt)
      })
    })
  }
})

# effecttab (E): regtab's workbook geometry, so the same cell-by-cell
# comparators apply; the exact-mode scenarios (tolerance and structure
# scenarios hold other cell text by design).
local({
  sc <- golden_scenarios()
  for (id in sc$id[sc$command == "effecttab" & sc$compare == "exact"]) {
    local({
      id <- id
      test_that(paste("as_flextable() and tt_as_gt() match Stata's workbook:", id), {
        skip_on_cran()
        skip_if_not_installed("flextable")
        skip_if_not_installed("gt")
        skip_if_not_installed("tidyxl")
        skip_if_not_installed("haven")
        call <- golden_scenario(id)$r_call
        for (pkg in c("marginaleffects", "WeightIt")) {
          if (grepl(paste0("\\b", pkg, "::"), call)) skip_if_not_installed(pkg)
        }
        if (!golden_scenario_live(id, golden_scenario(id)$phase)) skip("Phase 7")
        tt <- interop_tt(id)
        expect_ft_matches_golden(id, tt)
        expect_gt_matches_golden(id, tt)
      })
    })
  }
})

# comptab and hrcomptab (C): the workbook geometry of regtab/stratetab
# (header rows 2-3, body from row 4), so the same cell-by-cell comparators
# apply (task 7.5).
local({
  sc <- golden_scenarios()
  for (id in sc$id[sc$command %in% c("comptab", "hrcomptab")]) {
    local({
      id <- id
      test_that(paste("as_flextable() and tt_as_gt() match Stata's workbook:", id), {
        skip_on_cran()
        skip_if_not_installed("flextable")
        skip_if_not_installed("gt")
        skip_if_not_installed("tidyxl")
        skip_if_not_installed("haven")
        if (grepl("survival::", golden_scenario(id)$r_call, fixed = TRUE)) skip_if_not_installed("survival")
        if (!golden_scenario_live(id, golden_scenario(id)$phase)) skip("Phase 7")
        tt <- interop_tt(id)
        expect_ft_matches_golden(id, tt)
        expect_gt_matches_golden(id, tt)
      })
    })
  }
})

# tt_as_gtsummary() and tt_as_tinytable() over the other commands' goldens
# (stack review item 6): puttab's noheader and label-only tables (P05, P07,
# P09, P10, P15) among them. gtsummary keeps every cell; tinytable renders.
local({
  sc <- golden_scenarios()
  for (id in sc$id[sc$command %in% c("puttab", "stacktab", "wttab", "effecttab", "comptab", "hrcomptab")]) {
    local({
      id <- id
      test_that(paste("tt_as_gtsummary() and tt_as_tinytable() convert:", id), {
        skip_on_cran()
        skip_if_not_installed("haven")
        call <- golden_scenario(id)$r_call
        for (pkg in c("survival", "marginaleffects", "WeightIt")) {
          if (grepl(paste0("\\b", pkg, "::"), call)) skip_if_not_installed(pkg)
        }
        if (!golden_scenario_live(id, golden_scenario(id)$phase)) skip("Phase 7")
        tt <- suppressWarnings(interop_tt(id))
        if (requireNamespace("gtsummary", quietly = TRUE)) {
          expect_gts_keeps_cells(tt)
          if (requireNamespace("gt", quietly = TRUE)) expect_s3_class(gtsummary::as_gt(tt_as_gtsummary(tt)), "gt_tbl")
        }
        skip_if_not_installed("tinytable")
        expect_no_error(tinytable::save_tt(tt_as_tinytable(tt), "markdown"))
      })
    })
  }
})

