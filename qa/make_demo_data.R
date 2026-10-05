#!/usr/bin/env Rscript
# Build qa/demo/demo_tabtools.rds, the datasets the R demo
# (qa/demo/demo_tabtools.R, IMPLEMENTATION_PLAN.md Milestone D) reads.
#
# The Stata demo (tabtools/demo/demo_tabtools.do) draws its analysis dataset
# and three synthetic datasets with Stata's rnormal()/rpoisson()/runiform(),
# which R cannot redraw, and reads `sysuse auto` and `webuse union`. The
# golden fixtures (tests/testthat/golden/fixtures/*.dta, qa/stata/
# make_fixtures.do) hold exactly those data: qa/demo_parity.R --update
# rebuilds them from the pinned Stata demo and checks them variable by
# variable. This script copies the demo's variables into one compressed RDS
# next to the demo script. Both stay in the repository (qa/ is
# .Rbuildignored): auto and union are StataCorp's example data, which the
# package does not redistribute (user decision 2026-09-27).
#
#   Rscript qa/make_demo_data.R      (from the package root; needs haven)
#
# Contents (data frames as haven::read_dta() returns them, value-labelled
# columns kept as haven_labelled):
#   cohort    demo_tabtools.do:130-191 (the analysis dataset; the fixture's
#             golden-only variables dropped)
#   mixed_bp  demo sheet 15 (mixed model), seed 20260323
#   zip       demo sheet 31 (zero-inflated counts), seed 20260601
#   hrt_bin   demo sheet 46 (hrcomptab), the binary HRT Cox data, seed 20260417
#   hrt_dose  demo sheet 46, the dose-category Cox data (the same stream)
#   union     webuse union (GEE QICu, demo sheet 18)
#   auto      sysuse auto (puttab and desctab sheets)

root <- normalizePath(getwd())
if (!file.exists(file.path(root, "DESCRIPTION")) ||
    !identical(unname(read.dcf(file.path(root, "DESCRIPTION"), "Package")[1, 1]), "tabtools")) {
  stop("Run from the R-tabtools repo root.", call. = FALSE)
}
fx <- file.path(root, "tests", "testthat", "golden", "fixtures")
read <- function(name) as.data.frame(haven::read_dta(file.path(fx, paste0(name, ".dta"))))

# Golden-only variables drawn after the demo's (make_fixtures.do, seed
# 20260925) and the golden additions to auto: not part of the Stata demo.
golden_only <- list(
  cohort = c("cost_sek", "lab_value", "ctrl_marker", "ctrl_grade", "fw", "event_type", "bmi"),
  auto = c("age", "stage", "size_class", "mpg_dup", "pclass")
)
out <- list()
for (nm in c("cohort", "mixed_bp", "zip", "hrt_bin", "hrt_dose", "union", "auto")) {
  d <- read(nm)
  drop <- golden_only[[nm]]
  stopifnot(all(drop %in% names(d)))
  # Column subsetting also drops the data-level attributes (Stata's dataset
  # label and notes); qa/demo_parity.R --update checks the file this way.
  out[[nm]] <- d[setdiff(names(d), drop)]
}
dest <- file.path(root, "qa", "demo", "demo_tabtools.rds")
dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
saveRDS(out, dest, compress = "xz", version = 3)
message(sprintf("wrote %s (%.0f KB)", dest, file.size(dest) / 1024))
