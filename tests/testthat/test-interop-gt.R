# tt_as_gt() (task 6.1; named as_gt() until the pre-release review, P1-3). The cell-by-cell golden comparison (column
# labels, spanners and their styles, body cells, font, widths, caption and
# source note) runs over every golden scenario in test-interop-golden.R
# (expect_gt_matches_golden() in helper-interop.R); these are the checks
# on the rendered HTML.

test_that("the rule under regtab's model labels survives gt's CSS (review P0-2)", {
  skip_on_cran()
  skip_if_not_installed("gt")
  for (id in c("R01", "R04", "R15")) {
    tt <- interop_tt(id)
    g <- tabtools::tt_as_gt(tt)
    for (inline in c(TRUE, FALSE)) {
      html <- as.character(gt::as_raw_html(g, inline_css = inline))
      if (inline) {
        tr <- regmatches(html, regexpr('<tr class="gt_col_headings gt_spanner_row"[^>]*>', html))
        expect_length(tr, 1L)
        bb <- regmatches(tr, gregexpr("border-bottom-style: *[a-z]+", tr))[[1]]
        expect_identical(utils::tail(bb, 1L), "border-bottom-style: none", label = paste(id, "spanner row edge"))
      } else {
        # The override is scoped to this table's id and comes after gt's rule.
        id_html <- sub('^.*<div id="([^"]+)".*$', "\\1", substr(html, 1, 400))
        rules <- regmatches(html, gregexpr(paste0("#", id_html, " \\.gt_spanner_row \\{[^}]*\\}"), html))[[1]]
        expect_true(grepl("hidden", rules[1]))
        expect_true(grepl("border-bottom-style: none", utils::tail(rules, 1L)))
      }
    }
    # The labels under the spanners carry the rule as their top border.
    lab <- interop_gt_header(g)$labels
    expect_true(all(!is.na(lab$top[-1])), label = paste(id, "label top rules"))
  }
})

test_that("reference labels are not clipped in narrow estimate columns (review P1-4)", {
  skip_on_cran()
  skip_if_not_installed("gt")
  tt <- interop_tt("R15")
  g <- tabtools::tt_as_gt(tt)
  html <- as.character(gt::as_raw_html(g, inline_css = TRUE))
  tds <- regmatches(html, gregexpr("<td[^>]*>Reference</td>", html))[[1]]
  expect_true(length(tds) > 0L)
  expect_true(all(grepl("white-space: nowrap", tds)))
  expect_true(all(grepl("overflow-x: visible", tds)))
  expect_false(any(grepl("overflow-x: hidden", tds)))
  # Ordinary cells keep gt's clipping and wrap.
  other <- regmatches(html, gregexpr("<td[^>]*>[0-9.]+</td>", html))[[1]]
  expect_true(all(grepl("pre-wrap", other)))
})

test_that("tt_as_gt() ids are unique and do not touch the random number stream", {
  skip_if_not_installed("gt")
  tt <- tt_table(data.frame(a = c("x", "y"), b = c("1", "2")), list(c("Item", "Value")), command = "custom")
  set.seed(42)
  before <- .Random.seed
  g1 <- tt_as_gt(tt)
  g2 <- tt_as_gt(tt)
  expect_identical(.Random.seed, before)
  id <- function(g) g$`_options`$value[g$`_options`$parameter == "table_id"][[1]]
  expect_false(identical(id(g1), id(g2)))
  expect_match(id(g1), "^tabtools-gt-[0-9]+$")
})

test_that("tabtools exports no as_gt(), so gtsummary's is never masked (pre-release review P1-3)", {
  expect_false("as_gt" %in% getNamespaceExports("tabtools"))
  skip_on_cran()
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  withr::local_package("gtsummary")
  tt <- tt_table(data.frame(a = "x", b = "1"), list(c("Item", "Value")), command = "custom")
  # Whichever package is attached first, as_gt() is gtsummary's and
  # tt_as_gt() is tabtools'.
  expect_identical(get("as_gt", mode = "function"), gtsummary::as_gt)
  expect_s3_class(tt_as_gt(tt), "gt_tbl")
  tb <- gtsummary::tbl_summary(data.frame(a = c(1, 2, 3)))
  expect_s3_class(as_gt(tb), "gt_tbl")
  expect_error(tt_as_gt(tb), "gtsummary::as_gt")
})

test_that("tt_as_gt() names gt when it is not installed", {
  tt <- tt_table(data.frame(a = "x", b = "1"), list(c("Item", "Value")), command = "custom")
  local_mocked_bindings(.tt_has = function(pkg) FALSE)
  expect_error(tt_as_gt(tt), "needs the gt package")
})

test_that("tt_as_gt keeps $ and ~ literal in the caption, footnote and cells (audit A02)", {
  skip_if_not_installed("gt")
  d <- data.frame(g = rep(c("A", "B"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), labels = c(x = "Cost ($) per visit ($)"),
                   title = "Income ($) and cost ($) ~~strike~~", footnote = "Costs in $ (USD); fees in $ (CAD)")
  # Before the fix the caption was read as an equation (an error without
  # katex, math with it) and ~~ as strikethrough.
  html <- as.character(gt::as_raw_html(tt_as_gt(tab)))
  cap <- regmatches(html, regexpr("<caption[^>]*>.*?</caption>", html))
  expect_match(cap, "Income ($) and cost ($) ~~strike~~", fixed = TRUE)
  expect_no_match(html, "<del>", fixed = TRUE)
  expect_match(html, "Costs in $ (USD); fees in $ (CAD)", fixed = TRUE)
  expect_match(html, "Cost ($) per visit ($)", fixed = TRUE)
})
