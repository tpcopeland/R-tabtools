test_that("composite flat exports retain literal native variable headers", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = .01)
  attr(d, "effect_scale") <- "HR"; attr(d, "conf.level") <- .95
  m <- regtab(d, models = "Model")
  tab <- comptab(list(m), rows = 1L, cformat = "%5.3f", cisep = " to ")
  before <- tab
  flat <- tt_flat(tab, keyed = FALSE)
  expect_identical(names(flat), c("rowlabel", "c1", "c2", "c3"))
  expect_identical(vapply(flat, function(v) attr(v, "label"), ""),
    c(rowlabel = "", c1 = "Model, HR", c2 = "Model, 95% CI", c3 = "Model, p-value"))
  expect_identical(vapply(flat[-1L], function(v) attr(v, "tabtools_header"), ""),
    c(c1 = "Model, HR", c2 = "Model, 95% CI", c3 = "Model, p-value"))
  expect_identical(unname(as.matrix(flat)), unname(as.matrix(tab$body)))
  shown <- puttab(flat, varlabels = TRUE)
  expect_identical(shown$header[[1L]]$text, c("rowlabel", "Model, HR", "Model, 95% CI", "Model, p-value"))
  lay <- tabtools:::.xlsx_layout_puttab(shown)
  st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), shown$style)
  expect_identical(unname(st$widths[c("3", "5")]), c(11, 16))
  expect_identical(puttab(flat)$header[[1L]]$text, names(flat))
  expect_identical(tab, before)
  expect_error(comptab(flat, rows = 1L), class = "tabtools_error_composition")
})

test_that("review 2026-10-07 A5: composite footnote paragraphs are joined by the table", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = .01)
  attr(d, "effect_scale") <- "HR"; attr(d, "conf.level") <- .95
  m <- regtab(d, models = "Model")
  tab <- comptab(list(m), rows = 1L, footnote = c("First.", "Second."))
  expect_identical(tab$footnote, tabtools:::.tt_footnote_text(c("First.", "Second.")))
})
