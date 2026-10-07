library(testthat)
library(tabtools)

# Fresh installed-user surface, independent literal values and every sink.
test_that("installed publication overrides cannot leak stale numerical effects", {
  d <- data.frame(term = c("x", "y"), estimate = c(2, -1), conf.low = c(1, -2), conf.high = c(3, 0), p.value = c(.01, .2))
  attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
  text <- '$literal `macro\' \\ "quotes"'
  scratch <- withr::local_tempdir(pattern = "tabtools-installed-placement-")
  tt <- regtab(d, d, models = c("Case A", "Case a"),
               cellnote = list(list(row = "x", model = 1L, text = text)),
               addrow = list(list(label = "Inside", values = c("a", "b"), after = "x")),
               xlsx = file.path(scratch, "ordinary.xlsx"), csv = file.path(scratch, "ordinary.csv"),
               markdown = file.path(scratch, "ordinary.md"))
  expect_identical(tt$body[[2L]][1L], text)
  expect_identical(tt$body[[3L]][1L], "")
  expect_identical(tt$rows$key, c("x", "addrow:1", "y"))
  expect_identical(tt_flat(tt)[["_state_1"]], c("masked", "", "est"))
  expect_identical(as_forest_data(tt)$source_row, c(1L, 3L, 3L))
  expect_identical(tt$stored$table[1L, 1L], 2)
  expect_true(is.na(tt$stored$publication_table[1L, 1L]))
  composed <- tt_flat(tt_merge(tt, regtab(d, d)))
  expect_identical(attr(composed, "publication_overrides")$origin[1L, ], c("cellnote", "", "", ""))
  expect_identical(attr(composed[1L, ], "publication_overrides")$text[1L, 1L], text)
  expect_true(all(file.exists(file.path(scratch, c("ordinary.xlsx", "ordinary.csv", "ordinary.md")))))
  expect_true(any(grepl("literal", capture.output(print(tt)), fixed = TRUE)))
  expect_true(any(grepl("literal", readLines(file.path(scratch, "ordinary.csv")), fixed = TRUE)))
  expect_true(any(grepl("literal", readLines(file.path(scratch, "ordinary.md")), fixed = TRUE)))
  t <- regtab(d, d, models = c("Case A", "Case a"), transpose = TRUE, nopvalue = TRUE,
              collabels = c(x = "Effect"), addcol = list(list(label = "Text", values = c(text, ""), after = "x")),
              xlsx = file.path(scratch, "transpose.xlsx"), csv = file.path(scratch, "transpose.csv"), markdown = file.path(scratch, "transpose.md"))
  expect_identical(t$header[[1L]]$text, c("", "Effect", "Text", "y"))
  expect_identical(t$body[[2L]], rep("2.00 (1.00, 3.00)", 2L))
  expect_identical(t$body[[3L]], c(text, ""))
  expect_identical(t$meta$regtab_orientation, "transpose")
  expect_error(tt_flat(t), class = "tabtools_error_regtab_orientation")
  roundtrip <- puttab(tt_flat(t, keyed = FALSE))
  expect_identical(roundtrip$body[[2L]], rep("2.00 (1.00, 3.00)", 2L))
  expect_identical(roundtrip$body[[3L]], c(text, ""))
  expect_true(all(file.exists(file.path(scratch, c("transpose.xlsx", "transpose.csv", "transpose.md")))))
})
