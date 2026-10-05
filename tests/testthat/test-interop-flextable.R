# as_flextable.tt_table() (task 6.1), checked structurally against the
# workbooks Stata wrote for the golden scenarios: merges, cell text (label
# indents as no-break spaces), fonts, bold/italic, fills, font colour, and
# borders cell by cell, plus caption, footer and widths. No pixel tests.

# The cell-by-cell golden comparison runs over every golden scenario in
# test-interop-golden.R (expect_ft_matches_golden() in helper-interop.R).

test_that("regtab Reference/Omitted/Empty rows merge across their own model block only", {
  skip_on_cran()
  skip_if_not_installed("flextable")
  tt <- interop_tt("R04")
  ft <- flextable::as_flextable(tt)
  m <- interop_ft_merges(ft)
  refs <- which(tt$body[[2]] == "Reference" | tt$body[[5]] == "Reference" | tt$body[[8]] == "Reference")
  body <- m[m$part == "body", ]
  expect_true(length(refs) > 0L)
  for (i in refs) {
    for (k in 0:2) {
      c1 <- 2L + 3L * k
      if (tt$body[[c1]][i] == "Reference") {
        expect_true(any(body$i1 == i & body$j1 == c1 & body$j2 == c1 + 2L))
      }
    }
  }
  # Model labels merged over each block in the header's first row.
  head <- m[m$part == "header", ]
  expect_setequal(paste(head$j1, head$j2), c("2 4", "5 7", "8 10"))
  expect_true(all(head$i1 == 1L & head$i2 == 1L))
})

test_that("as_flextable() output writes to .docx and HTML", {
  skip_on_cran()
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  for (id in c("T19", "R12")) {
    ft <- flextable::as_flextable(interop_tt(id))
    f <- withr::local_tempfile(fileext = ".docx")
    doc <- flextable::body_add_flextable(officer::read_docx(), ft)
    print(doc, target = f)
    expect_true(file.exists(f))
    s <- officer::docx_summary(officer::read_docx(f))
    cells <- s$text[s$content_type == "table cell"]
    if (id == "T19") expect_true("\u00a0\u00a0\u00a0Primary" %in% cells)
    if (id == "R12") expect_true(all(c("\u00a0\u00a0Domestic", "Ref.", "Dropped", "No obs") %in% cells))
    h <- withr::local_tempfile(fileext = ".html")
    flextable::save_as_html(ft, path = h)
    expect_true(file.size(h) > 0)
  }
})

# Phase 6 review P0-1/P1-1: Word keeps one border per shared horizontal edge
# (flextable writes it from the upper cell's bottom), so rules the workbook
# draws as a top border vanished from .docx output. Every rule of Stata's
# workbook must be in word/document.xml: the header rule (T19, T20c; academic
# T19/R15), the rule under the model labels (every regtab table), and the
# rules above the stats (R04), addrow (R11) and random-effects (R21) rows.
test_that("every workbook rule reaches the .docx (edges read from document.xml)", {
  skip_on_cran()
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("xml2")
  skip_if_not_installed("tidyxl")
  for (id in c("T19", "T20c", "T33", "R04", "R11", "R15", "R21")) {
    tt <- interop_tt(id)
    ft <- flextable::as_flextable(tt)
    f <- withr::local_tempfile(fileext = ".docx")
    flextable::save_as_docx(ft, path = f)
    got <- interop_docx_edges(f, nrows = 2L + nrow(tt$body))
    want <- interop_golden_edges(interop_golden(id, nrow(tt$body), ncol(tt$body)))
    expect_identical(setdiff(want, got), character(), label = paste(id, "workbook rules missing from the .docx"))
    expect_identical(setdiff(got, want), character(), label = paste(id, "extra rules in the .docx"))
    # The rule under the header must be there in every table.
    expect_true(any(startsWith(got, "h|2|")), label = paste(id, "rule under the header"))
  }
})
