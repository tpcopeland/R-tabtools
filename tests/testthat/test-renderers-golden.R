# Phase 1 exit criterion (IMPLEMENTATION_PLAN.md section 6): tt_tables built by hand
# from Stata goldens render xlsx, CSV, Markdown, and console output that match
# the Stata sinks. The engines do not exist yet, so the cells come from the
# golden CSV and the style from the scenario's Stata call (helper-renderers.R).

render_and_compare <- function(id, tt = golden_tt_full(id)) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  sc <- golden_scenario(id)
  csv <- file.path(out, paste0(id, ".csv"))
  tt_write_csv(tt, csv)
  expect_sink_match(csv, id, "csv", mask = "", tt = tt)
  md <- file.path(out, paste0(id, ".md"))
  tt_write_markdown(tt, md)
  expect_sink_match(md, id, "md", mask = "", tt = tt)
  parts <- golden_publication_console(utils::capture.output(print(tt)), golden_console_expected(id), id)
  why <- golden_compare_console(parts$got, parts$want, "")
  expect_length(why, 0L)
  xlsx <- file.path(out, paste0(sc$command, ".xlsx"))
  tt_write_xlsx(tt, xlsx, sheet = id)
  # openxlsx2, like Stata's xl(), reads back 0.711 wider than the width set.
  expect_styles_match(xlsx, id, id, mask = "", got_width_offset = 0.711)
}

test_that("T01 hand-built from its golden matches every Stata sink", {
  skip_if_not_installed("tidyxl")
  render_and_compare("T01")
  # The unstyled builder (no Stata-call parsing) is enough for T01.
  render_and_compare("T01", golden_tt("T01"))
})

test_that("R01 hand-built from its golden matches every Stata sink", {
  skip_if_not_installed("tidyxl")
  render_and_compare("R01")
})

test_that("every golden scenario's sinks are reproduced from its cells", {
  skip_on_cran()
  skip_if_not_installed("tidyxl")
  sc <- golden_scenarios()
  # golden_tt_full() rebuilds table1_tc and regtab tables from their cells;
  # puttab and stacktab have their own runner (test-golden-export.R).
  for (id in sc$id[sc$command %in% c("table1_tc", "regtab")]) {
    tt <- golden_tt_full(id)
    if (id %in% c("T28", "T30l", "T30m", "T30n")) {
      # Stata lists the table before percentages are withheld, so the Total
      # column prints as wide as its "12 (60)"-style text (the Phase 3 engine
      # supplies this width).
      tt$cols$console_width[tt$cols$role == "total"] <- 7L
    }
    if (id == "T34") {
      # mdappend: the scenario writes the table twice into one file.
      out <- withr::local_tempdir()
      md <- file.path(out, "T34.md")
      tt_write_markdown(tt, md)
      tt_write_markdown(tt, md, append = TRUE)
      expect_sink_match(md, "T34", "md", mask = "")
      next
    }
    render_and_compare(id, tt)
  }
})
