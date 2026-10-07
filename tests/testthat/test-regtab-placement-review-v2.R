# Independent literal controls for fresh source-review findings. These tests
# assert worksheet geometry without needing an XLSX sink or native execution.
test_that("ordinary addrow ignores excess values and addcol still refuses them", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = .01)
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  one <- regtab(d, addrow = list(Tail = c("first", "ignored", "also ignored")))
  expect_identical(one$body[[2L]][2L], "first")
  expect_identical(one$body[[3L]][2L], "")
  two <- regtab(d, d, addrow = list(list(label = "Inside", values = c("A", "B", "ignored"), after = "x")))
  expect_identical(two$rows$key, c("x", "addrow:1"))
  expect_identical(two$body[[2L]][2L], "A")
  expect_identical(two$body[[5L]][2L], "B")
  expect_error(regtab(d, transpose = TRUE, addcol = list(Tail = c("first", "excess"))),
               class = "tabtools_error_regtab_layout")
})

test_that("constrained coefficients keep literal numeric and CI cells without label merging", {
  d <- data.frame(term = "x", estimate = 2, std.error = 0, status = "constrained",
                  constraint_value = 2, constraint_source = "declared equality")
  attr(d, "effect_scale") <- "Coef."
  x <- regtab(d)
  expect_identical(unname(unlist(x$body[1L, 2:4])), c("2.00", "(constrained)", ""))
  expect_identical(tt_flat(x)[["_state_1"]], "constrained")
  layout <- tabtools:::.xlsx_layout_regtab(x)
  expect_identical(unname(layout$grid[4L, 3:5]), c("2.00", "(constrained)", ""))
  style <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), x$style)
  expect_false("C4:E4" %in% style$merges)
  expect_false(any(style$italic[4L, 3:5]))
  literal <- regtab(d, cnslabel = "fixed $literal")
  expect_identical(literal$body[[2L]], "2.00")
  expect_identical(literal$body[[3L]], "fixed $literal")
  compact <- regtab(d, compact = TRUE)
  expect_identical(compact$body[[2L]], "2.00 (constrained)")
  compact_layout <- tabtools:::.xlsx_layout_regtab(compact)
  compact_style <- tabtools:::.xlsx_apply_rules(compact_layout$rules,
    nrow(compact_layout$grid), ncol(compact_layout$grid), compact$style)
  expect_false("C4:D4" %in% compact_style$merges)
  expect_false(any(compact_style$italic[4L, 3:4]))
})

test_that("visible sample absence merges its own model block and blank model absence does not", {
  d <- data.frame(g = factor(rep(1:3, each = 8)), x = rep(1:8, 3))
  d$y <- 2 + as.numeric(d$g) + .3 * d$x + rep(c(.1, -.2, .2, -.1), 6)
  d$ev <- c(0, 1, 2)[as.numeric(d$g)]
  fits <- list(lm(y ~ g + x, d), lm(y ~ g + x, d, subset = g != "1"), lm(y ~ x, d))
  records <- lapply(fits, tt_fitcount, events = "ev", data = d, terms = TRUE)
  x <- do.call(regtab, c(fits, list(fitcounts = records, mincount = 1, absentlabel = "Excluded")))
  row <- match("1.g", x$rows$key)
  expect_identical(row, 2L)
  expect_identical(x$body[[5L]][row], "Excluded")
  expect_identical(x$body[[8L]][row], "")
  analytical <- x$meta$regtab_rows
  expect_identical(analytical$mask_reason[analytical$key == "1.g" & analytical$model == 2L], "sample_absent")
  layout <- tabtools:::.xlsx_layout_regtab(x)
  style <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), x$style)
  expect_true("F5:H5" %in% style$merges)
  expect_true(style$italic[5L, 6L])
  expect_false("I5:K5" %in% style$merges)
  expect_false(any(style$italic[5L, 9:11]))
})

test_that("factor-heading cellnotes retain blank structural state and aligned literal provenance", {
  d <- data.frame(term = c("gA", "gB"), variable = "g", var_type = "categorical",
    label = c("A", "B"), var_label = "Group", key = c("1.g", "2.g"),
    estimate = c(2, -1), conf.low = c(1, -2), conf.high = c(3, 0), p.value = c(.01, .05), status = "est")
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  literal <- '$literal `macro\' \\ "quoted"'
  x <- regtab(d, d, cellnote = list(list(row = "Group", model = 1L, text = "first"),
    list(row = "Group", model = 1L, text = literal), list(row = "A", model = 2L, text = "Coefficient note")))
  expect_identical(unname(unlist(x$body[1L, 2:4])), c(literal, "", ""))
  layout <- tabtools:::.xlsx_layout_regtab(x)
  expect_identical(unname(layout$grid[4L, 3:8]), c(literal, "", "", "", "", ""))
  style <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), x$style)
  # Native regtab.ado:2022,2188-2191,2864-2870 keeps note rows per model:
  # the first body row is worksheet row 4, model blocks C:E and F:H.
  expect_true("C4:E4" %in% style$merges)
  expect_identical(style$halign[4L, 3L], "center")
  expect_identical(style$valign[4L, 3L], "center")
  expect_true(style$italic[4L, 3L])
  expect_false("F4:H4" %in% style$merges)
  expect_false(any(style$italic[4L, 6:8]))
  f <- tt_flat(x)
  expect_identical(f[["_state_1"]], c("", "est", "est"))
  expect_identical(f[["_state_2"]], c("", "masked", "est"))
  expect_identical(attr(f, "publication_overrides")$origin[1L, ], c("cellnote", ""))
  expect_identical(attr(f, "publication_overrides")$text[1L, ], c(literal, ""))
  expect_identical(attr(f[1L, ], "publication_overrides")$text, matrix(c(literal, ""), 1L, 2L))
  expect_identical(x$stored$smallcells$n_masked, 0L)
  expect_identical(x$stored$N_masked, 0L)
  header <- subset(x$meta$regtab_rows, key == "g" & model == 1L)
  expect_identical(header$status, "")
  expect_identical(header$override_origin, "cellnote")
  expect_true(is.na(header$estimate) && is.na(header$conf.low) && is.na(header$p.value))
  merged <- tt_flat(tt_merge(x, regtab(d)))
  expect_identical(attr(merged, "publication_overrides")$origin[1L, ], c("cellnote", "", ""))
  expect_identical(attr(merged, "publication_overrides")$text[1L, ], c(literal, "", ""))
  expect_identical(merged[["_state_1"]][1L], "")
  invalid <- f
  override <- attr(invalid, "publication_overrides")
  override$origin[2L, 1L] <- "cellnote"
  override$text[2L, 1L] <- "Unmasked analytical override"
  attr(invalid, "publication_overrides") <- override
  expect_error(invalid[2L, ], class = "tabtools_error_flat")
})

test_that("empty factor-heading cellnotes retain their own model's workbook label geometry", {
  d <- data.frame(term = c("gA", "gB"), variable = "g", var_type = "categorical",
    label = c("A", "B"), var_label = "Group", key = c("1.g", "2.g"),
    estimate = c(2, -1), conf.low = c(1, -2), conf.high = c(3, 0), p.value = c(.01, .05), status = "est")
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  x <- regtab(d, d, cellnote = list(list(row = "Group", model = 2L, text = "")))
  f <- tt_flat(x)
  expect_identical(f[["_state_1"]], c("", "est", "est"))
  expect_identical(f[["_state_2"]], c("", "est", "est"))
  expect_identical(attr(f, "publication_overrides")$origin[1L, ], c("", "cellnote"))
  expect_identical(attr(f, "publication_overrides")$text[1L, ], c("", ""))
  expect_identical(x$stored$smallcells$n_masked, 0L)
  expect_identical(x$stored$N_masked, 0L)
  layout <- tabtools:::.xlsx_layout_regtab(x)
  expect_identical(unname(layout$grid[4L, 3:8]), rep("", 6L))
  style <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), x$style)
  expect_true("F4:H4" %in% style$merges)
  expect_identical(style$halign[4L, 6L], "center")
  expect_identical(style$valign[4L, 6L], "center")
  expect_true(style$italic[4L, 6L])
  expect_false("C4:E4" %in% style$merges)
  expect_false(any(style$italic[4L, 3:5]))
})
