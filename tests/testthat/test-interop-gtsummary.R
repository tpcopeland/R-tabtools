# tt_as_gtsummary() (task 6.5).

gts_reg <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  regtab(glm(am ~ wt + cyl, binomial, d), glm(am ~ wt, binomial, d), stats = "n",
         models = c("Adjusted", "Crude"), title = "Table 2", footnote = "Source: mtcars")
}

test_that("6.5: a regtab table's cells, headers, spanners, indents and notes", {
  skip_if_not_installed("gtsummary")
  tt <- gts_reg()
  g <- tt_as_gtsummary(tt)
  expect_s3_class(g, "gtsummary")
  expect_gts_keeps_cells(tt)
  hd <- g$table_styling$header
  expect_identical(hd$label[match(paste0("col_", 2:7), hd$column)], c("OR", "95% CI", "p-value", "OR", "95% CI", "p-value"))
  expect_true(all(hd$hide[hd$column %in% c("variable", "row_type", "key")]))
  sp <- g$table_styling$spanning_header
  expect_identical(sp$spanning_header[match(paste0("col_", 2:7), sp$column)], rep(c("Adjusted", "Crude"), each = 3))
  expect_identical(g$table_body$row_type, c("label", "label", "level", "level", "level", "label"))
  expect_true(nrow(g$table_styling$indent) > 0)
  expect_identical(as.vector(g$table_styling$caption), "Table 2")
  expect_true("Source: mtcars" %in% unlist(g$table_styling$source_note))
  # Reference is italic in its cell only.
  it <- g$table_styling$text_format
  expect_identical(unique(it$column[it$format_type == "italic"]), "col_2")
})

test_that("6.5: works with gtsummary's converters and tbl_stack()", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  g <- tt_as_gtsummary(gts_reg())
  expect_s3_class(gtsummary::as_gt(g), "gt_tbl")
  expect_type(gtsummary::as_kable(g), "character")
  skip_if_not_installed("flextable")
  expect_s3_class(gtsummary::as_flex_table(g), "flextable")
  st <- suppressMessages(gtsummary::tbl_stack(list(g, g), group_header = c("A", "B")))
  expect_identical(nrow(st$table_body), 2L * nrow(g$table_body))
})

test_that("6.5: table1_tc headers spanning both rows are column labels; text is literal", {
  skip_if_not_installed("gtsummary")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  t1 <- table1_tc(d, by = "am", vars = c(wt = "contn", cyl = "cat"))
  g <- tt_as_gtsummary(t1)
  expect_gts_keeps_cells(t1)
  hd <- g$table_styling$header
  expect_identical(hd$label[hd$column == "col_4"], "p-value")
  expect_false("col_4" %in% g$table_styling$spanning_header$column)
  # Labels are the table's text as typed (text_interpret = "none"; review P1-1).
  expect_identical(hd$label[hd$column == "col_2"], t1$header[[length(t1$header)]]$text[2])
  expect_identical(unique(hd$interpret_label[hd$column %in% c("label", paste0("col_", 2:4))]), "identity")
  expect_error(tt_as_gtsummary(1), "tt_table")
})

test_that("stack review 6: tables without header rows or value columns", {
  skip_if_not_installed("gtsummary")
  nohdr <- puttab(data.frame(a = c("x", "y"), b = 1:2), noheader = TRUE)
  expect_length(nohdr$header, 0L)
  g <- tt_as_gtsummary(nohdr)
  expect_gts_keeps_cells(nohdr)
  expect_identical(g$table_styling$header$label[g$table_styling$header$column == "col_2"], "")
  lab <- puttab(data.frame(make = c("a", "b", "c")))
  g <- tt_as_gtsummary(lab)
  expect_gts_keeps_cells(lab)
  expect_false(any(startsWith(names(g$table_body), "col_")))
  expect_identical(g$table_styling$header$label[g$table_styling$header$column == "label"], "make")
  skip_if_not_installed("gt")
  expect_s3_class(gtsummary::as_gt(tt_as_gtsummary(nohdr)), "gt_tbl")
  expect_s3_class(gtsummary::as_gt(g), "gt_tbl")
})

test_that("stack review 7-8: no deprecation warning; models with one label keep their own spanners", {
  skip_if_not_installed("gtsummary")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  tt <- regtab(lm(mpg ~ cyl, d), lm(mpg ~ cyl + wt, d), models = c("Model", "Model"))
  expect_no_warning(g <- tt_as_gtsummary(tt))
  sp <- g$table_styling$spanning_header
  lab <- sp$spanning_header[match(paste0("col_", 2:7), sp$column)]
  expect_length(unique(lab[1:3]), 1L)
  expect_length(unique(lab[4:6]), 1L)
  expect_false(lab[1] == lab[4])
  expect_identical(gsub("\u200b", "", lab), rep("Model", 6))
  skip_if_not_installed("gt")
  gt_tab <- gtsummary::as_gt(g)
  expect_identical(nrow(gt_tab[["_spanners"]]), 2L)
})


test_that("tt_as_gtsummary keeps $ literal in headers, caption and source note (audit A02)", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  d <- data.frame(g = rep(c("US$ A", "US$ B"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), title = "Income ($) and cost ($)",
                   footnote = "Costs in $ (USD); fees in $ (CAD)")
  html <- as.character(gt::as_raw_html(gtsummary::as_gt(tt_as_gtsummary(tab))))
  expect_match(html, "Income ($) and cost ($)", fixed = TRUE)
  expect_match(html, "Costs in $ (USD); fees in $ (CAD)", fixed = TRUE)
  expect_match(html, "US$ A", fixed = TRUE)
})

test_that("tt_as_gtsummary() text is literal through as_flex_table() (Word) and as_gt() (review P1-1)", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("gt")
  d <- data.frame(g = rep(c("A ($)", "B & C_1"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), title = "Income ($) and cost ($) ~~s~~ <5",
                   footnote = "Costs in $ (USD); A & B, a_b *c*")
  g <- tt_as_gtsummary(tab)
  f <- withr::local_tempfile(fileext = ".docx")
  flextable::save_as_docx(gtsummary::as_flex_table(g), path = f)
  txt <- unique(officer::docx_summary(officer::read_docx(f))$text)
  for (s in c("Income ($) and cost ($) ~~s~~ <5", "A ($)", "B & C_1", "Costs in $ (USD); A & B, a_b *c*")) {
    expect_true(s %in% txt, label = s)
  }
  expect_false(any(grepl("&#36;|&amp;|&lt;|\\\\[&_*$<~]", txt)))
  # gt shows the same text, with no equation or strikethrough.
  html <- as.character(gt::as_raw_html(gtsummary::as_gt(g)))
  expect_match(html, "Income ($) and cost ($) ~~s~~ &lt;5", fixed = TRUE)
  expect_match(html, "Costs in $ (USD); A &amp; B, a_b *c*", fixed = TRUE)
  expect_no_match(html, "<del>", fixed = TRUE)
})
