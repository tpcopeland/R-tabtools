# Fast, CRAN-safe checks of the interop converters (task 6.1) and the
# desctab() alias (task 6.2) on small simulated data; the golden-backed
# checks are in test-interop-flextable.R and test-interop-gt.R.

interop_small <- function() {
  set.seed(1)
  d <- data.frame(arm = rep(c("A", "B"), each = 30),
                  age = round(stats::rnorm(60, 60, 8), 1),
                  sex = factor(sample(c("Female", "Male"), 60, TRUE)),
                  stage = factor(sample(c("I", "II", "III"), 60, TRUE)))
  attr(d$age, "label") <- "Age (years)"
  d
}

test_that("helpers: no-break indents, merge refs, colours", {
  expect_identical(tabtools:::.tt_nbsp_indent(c("   a b", "x  ", "", "  ")),
                   c("\u00a0\u00a0\u00a0a b", "x  ", "", "\u00a0\u00a0"))
  expect_identical(tabtools:::.tt_parse_merges(c("C7:E7", "B2:B3", "AA10:AB10")),
                   data.frame(r1 = c(7L, 2L, 10L), c1 = c(3L, 2L, 27L), r2 = c(7L, 3L, 10L),
                              c2 = c(5L, 2L, 28L)))
  expect_identical(tabtools:::.tt_argb_hex(c("FFDBE5F1", NA)), c("#DBE5F1", NA))
  expect_identical(tabtools:::.tt_width_px(10), 83)
})

test_that("table1_tc: descriptor merged over both header rows, indents kept", {
  skip_if_not_installed("flextable")
  tab <- table1_tc(interop_small(), by = "arm", vars = c(age = "contn %5.1f", stage = "cat"),
                   title = "Table 1", footnote = "Note.", zebra = TRUE,
                   xlsx = withr::local_tempfile(fileext = ".xlsx"))
  ft <- flextable::as_flextable(tab)
  m <- interop_merge_keys(interop_ft_merges(ft))
  expect_true(all(c("header 1 2 1 1", "header 1 2 4 4") %in% m))
  cells <- interop_ft_cells(ft)
  hdr <- cells[cells$part == "header", ]
  expect_identical(hdr$value[hdr$i == 1 & hdr$j == 1], tab$header[[2]]$text[1])
  expect_identical(hdr$value[hdr$i == 2 & hdr$j == 2], tab$header[[2]]$text[2])
  body <- cells[cells$part == "body" & cells$j == 1, ]
  expect_true(any(startsWith(body$raw, "\u00a0\u00a0\u00a0I")))
  expect_true(all(cells$font == "Arial"))
  expect_true(all(cells$bold[cells$part == "header"]))
  expect_identical(ft$caption$value$txt, "Table 1")
})

test_that("desctab() is table1_tc()", {
  d <- interop_small()
  a <- table1_tc(d, by = "arm", vars = c(age = "contn", sex = "cat"), smd = TRUE, nopvalue = TRUE)
  b <- desctab(d, by = "arm", vars = c(age = "contn", sex = "cat"), smd = TRUE, nopvalue = TRUE)
  expect_identical(a, b)
  # Positional arguments and the sheet-without-xlsx rule pass through too.
  expect_identical(desctab(d, c("age", "sex"), "arm"), table1_tc(d, c("age", "sex"), "arm"))
  expect_error(desctab(d, vars = "age", sheet = "S"), "sheet")
})

test_that("regtab: model block merges and italic reference cells", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  d <- interop_small()
  fit <- stats::glm(I(sex == "Male") ~ age + stage, binomial, d)
  tab <- regtab(fit, fit, models = c("A", "B"), borderstyle = "academic", xlsx = withr::local_tempfile(fileext = ".xlsx"))
  ft <- flextable::as_flextable(tab)
  m <- interop_ft_merges(ft)
  ref <- which(tab$body[[2]] == "Reference")
  expect_length(ref, 1L)
  expect_true(all(c(paste("body", ref, ref, 2, 4), paste("body", ref, ref, 5, 7)) %in% interop_merge_keys(m)))
  cells <- interop_ft_cells(ft)
  rc <- cells[cells$part == "body" & cells$i == ref & cells$j %in% c(2, 5), ]
  expect_true(all(rc$italic) && all(rc$halign == "center"))
  # academic: medium horizontal rules, no vertical ones.
  expect_true(all(is.na(cells$left)) && all(is.na(cells$right)))
  expect_setequal(stats::na.omit(unique(c(cells$top, cells$bottom))), "medium")
  g <- tt_as_gt(tab)
  gb <- interop_gt_body(g)
  expect_true(all(gb$italic[gb$i == ref & gb$j %in% c(2, 5)]))
  expect_identical(g$`_data`$c3[ref], "")
  expect_setequal(vapply(g$`_spanners`$spanner_label, as.character, ""), c("A", "B"))
})

test_that("a table without a workbook layout converts with plain styling", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  tt <- tt_table(data.frame(a = c("x", "  y"), b = c("1", "2")), list(c("Item", "Value")),
                 command = "custom")
  ft <- flextable::as_flextable(tt)
  cells <- interop_ft_cells(ft)
  expect_true(all(cells$bold[cells$part == "header"]))
  expect_identical(cells$raw[cells$part == "body" & cells$j == 1], c("x", "\u00a0\u00a0y"))
  # Rules above and below the header and below the body (review M16).
  hr <- tt$style$hborder
  expect_identical(unique(cells$top[cells$part == "header"]), hr)
  expect_identical(unique(cells$bottom[cells$part == "header"]), hr)
  expect_identical(cells$bottom[cells$part == "body" & cells$i == 2], c(hr, hr))
  expect_true(all(is.na(cells$bottom[cells$part == "body" & cells$i == 1])))
  expect_true(all(is.na(cells$top[cells$part == "body"])))
  g <- tt_as_gt(tt)
  expect_identical(vapply(g$`_boxhead`$column_label, as.character, ""), c("Item", "Value"))
  gb <- interop_gt_body(g)
  expect_identical(gb$bottom[gb$i == 2], c(hr, hr))
  expect_true(all(is.na(gb$bottom[gb$i == 1])))
})

test_that("top rules are also the bottom of the row above for Word (review P0-1)", {
  expect_identical(tabtools:::.tt_heavier(c(NA, "thin", "medium", "thin", NA), c("thin", NA, "thin", "thick", NA)),
                   c("thin", "thin", "medium", "thick", NA))
  tab <- table1_tc(interop_small(), by = "arm", vars = c(age = "contn %5.1f", stage = "cat"))
  fit <- stats::glm(I(sex == "Male") ~ age + stage, binomial, interop_small())
  reg <- regtab(fit, stats = "n")
  for (x in list(tab, reg)) {
    word <- tabtools:::.tt_render_spec(x, merged = TRUE)
    html <- tabtools:::.tt_render_spec(x, merged = FALSE)
    # The workbook draws the rule under the header (table1_tc) and under the
    # model labels (regtab) as a top border ...
    top_row <- if (identical(x$command, "regtab")) html$header$top[2, ] else html$body$top[1, ]
    expect_true(any(!is.na(top_row)))
    # ... which the Word-bound spec repeats on the cell above; the top stays.
    above <- if (identical(x$command, "regtab")) word$header$bottom[1, ] else word$header$bottom[2, ]
    expect_identical(above[!is.na(top_row)], top_row[!is.na(top_row)])
    expect_identical(if (identical(x$command, "regtab")) word$header$top[2, ] else word$body$top[1, ], top_row)
  }
  # regtab: the rule above the first statistics row too.
  html <- tabtools:::.tt_render_spec(reg, merged = FALSE)
  word <- tabtools:::.tt_render_spec(reg, merged = TRUE)
  st <- which(reg$rows$type == "stat")[1]
  expect_true(any(!is.na(html$body$top[st, ])))
  ruled <- !is.na(html$body$top[st, ])
  expect_identical(word$body$bottom[st - 1L, ruled], html$body$top[st, ruled])
})

test_that("tt_as_gt() refuses other objects, pointing gtsummary tables to gtsummary::as_gt()", {
  expect_error(tt_as_gt(data.frame(a = 1)), "must be a <tt_table>")
  x <- structure(list(), class = c("tbl_summary", "gtsummary"))
  expect_error(tt_as_gt(x), "gtsummary::as_gt")
})

test_that("an empty table is refused", {
  tt <- tt_table(data.frame(a = character(), b = character()), list(c("Item", "Value")))
  expect_error(tabtools:::.tt_render_spec(tt), "no body rows")
})

test_that("leading blanks of the footnote survive in the flextable footer (review M25)", {
  skip_if_not_installed("flextable")
  tt <- tt_table(data.frame(a = c("x", "y"), b = c("1", "2")), list(c("Item", "Value")),
                 footnote = "  a. Indented note.", command = "custom")
  ch <- flextable::information_data_chunk(flextable::as_flextable(tt))
  foot <- paste(ch$txt[ch$.part == "footer" & ch$.col_id == "c1"], collapse = "")
  expect_identical(foot, "  a. Indented note.")
})
