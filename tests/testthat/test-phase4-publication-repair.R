# Literal publication contracts for the actual Phase4 checkpoint faults.
test_that("effecttab custom rows retain native ordinary-cell geometry", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = .01)
  x <- effecttab(d, models = "Estimate", level = 95, addrow = list(Tail = "literal"))
  lay <- tabtools:::.xlsx_layout_regtab(x)
  st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), x$style)
  expect_false("C5:E5" %in% st$merges)
  expect_true(all(is.na(st$top[5L, 2:5])))
  expect_identical(st$valign[5L, 3L], "top")
  attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
  y <- regtab(d, addrow = list(Tail = "literal"))
  ly <- tabtools:::.xlsx_layout_regtab(y)
  sy <- tabtools:::.xlsx_apply_rules(ly$rules, nrow(ly$grid), ncol(ly$grid), y$style)
  expect_true("C5:E5" %in% sy$merges)
  expect_identical(sy$top[5L, 2:5], rep("thin", 4L))
  expect_identical(sy$valign[5L, 3L], "center")
})

test_that("native interaction headings remain undimmed with nonsignificant children", {
  rows <- data.frame(key = c("g", "1.g", "2.g", "g#h", "1.g#1.h", "2.g#2.h"),
    kind = c("cat_header", "level", "level", "int_header", "int_level", "int_level"),
    parent_key = c("", "g", "g", "", "g#h", "g#h"))
  cell <- data.frame(status = c("", "ref", "est", "", "ref", "est"),
    ancillary = FALSE, conf.low = c(NA, NA, -1, NA, NA, -1), conf.high = c(NA, NA, 1, NA, NA, 1))
  u <- list(rows = rows, cells = list(cell))
  expect_identical(tabtools:::.rt_dimnonsig(u, 0), c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  u$cells[[1L]]$conf.low[3L] <- .5
  expect_identical(tabtools:::.rt_dimnonsig(u, 0), c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("addrow labels remain semantic composition keys and repeats stay unique", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = .01)
  attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
  a <- regtab(d, addrow = list(A = "one")); b <- regtab(d, addrow = list(B = "two"))
  joined <- tt_merge(a, b)
  expect_identical(joined$rows$key, c("x", "addrow:A", "addrow:B"))
  expect_identical(unname(unlist(joined$body[2L, c(2L, 5L)])), c("one", ""))
  expect_identical(unname(unlist(joined$body[3L, c(2L, 5L)])), c("", "two"))
  repeated <- regtab(d, addrow = list(list(label = "A", values = "first"), list(label = "A", values = "second")))
  expect_identical(repeated$rows$key, c("x", "addrow:A", "addrow:A:1"))
  expect_identical(tt_flat(repeated)[["_term"]], repeated$rows$key)
})

test_that("reference italics remain per model when a shared row is estimated elsewhere", {
  a <- data.frame(term = c("gA", "gB"), variable = "g", var_label = "Group", var_type = "categorical",
    label = c("A", "B"), reference_row = c(TRUE, FALSE),
    estimate = c(1, 2), conf.low = c(NA, 1.2), conf.high = c(NA, 3), p.value = c(NA, .01))
  attr(a, "effect_scale") <- "HR"; attr(a, "conf.level") <- .95
  b <- a; b$reference_row <- c(FALSE, TRUE); b$estimate <- c(2, 1)
  b$conf.low <- c(1.2, NA); b$conf.high <- c(3, NA); b$p.value <- c(.01, NA)
  x <- regtab(a, b)
  spec <- tabtools:::.tt_render_spec(x, merged = FALSE)
  expect_identical(which(spec$body$italic[, 2L]), 2L)
  expect_identical(which(spec$body$italic[, 5L]), 3L)
  expect_false(any(spec$body$italic[, c(1L, 3L, 4L, 6L, 7L)]))
})
