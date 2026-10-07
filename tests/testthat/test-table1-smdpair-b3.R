# B3: smdpair resolution (values/labels), logical by, SMD column role and
# xlsx width, and " \ " joins of the automatic notes.

b3_data <- function(vals, labs) {
  arm <- rep(vals, each = 10)
  attr(arm, "labels") <- stats::setNames(vals, labs)
  set.seed(1)
  data.frame(arm = arm, age = stats::rnorm(30, 50, 10))
}
b3_hdr <- function(tt) tt$header[[1]]$text[tt$cols$role == "smd"]

test_that("smdpair resolves a numeric token that is only a label (BUG-1)", {
  d <- b3_data(1:3, c("2020", "2021", "2022"))
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c("2020", "2021"))
  expect_identical(b3_hdr(tt), "SMD (2020 vs 2021)")
  tt2 <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c(2L, 3L))
  expect_identical(b3_hdr(tt2), "SMD (2021 vs 2022)")
})

test_that("a value/label ambiguity is refused under auto and resolved by smdpair_as", {
  d <- b3_data(1:3, c("3", "1", "2"))
  f <- function(...) table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, ...)
  e <- expect_error(f(smdpair = c(3, 2)), class = "tabtools_error_smdpair_ambiguous")
  expect_s3_class(e, "tabtools_error_smdpair")
  expect_identical(b3_hdr(f(smdpair = c(3, 2), smdpair_as = "values")), "SMD (2 vs 1)")
  expect_identical(b3_hdr(f(smdpair = c(3, 2), smdpair_as = "labels")), "SMD (3 vs 2)")
  expect_error(f(smdpair = c(3, 2), smdpair_as = "bogus"), class = "tabtools_error_smdpair_as")
  expect_error(f(smdpair_as = "values"), class = "tabtools_error_smdpair_as")
  expect_no_error(f(smdpair_as = "auto"))
})

test_that("a label equal to its own value is not ambiguous", {
  d <- b3_data(1:3, c("1", "2", "3"))
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c(1, 3))
  expect_identical(b3_hdr(tt), "SMD (1 vs 3)")
})

test_that("unknown and identical tokens give classed errors", {
  d <- b3_data(1:3, c("a", "b", "c"))
  f <- function(p) table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = p)
  expect_error(f(c("a", "zz")), class = "tabtools_error_smdpair")
  expect_error(f(c("a", "a")), class = "tabtools_error_smdpair")
  expect_error(f(c(TRUE, FALSE)), class = "tabtools_error_smdpair")
})

test_that("a logical by accepts TRUE/FALSE in smdpair (BUG-6)", {
  set.seed(1)
  d <- data.frame(treat = rep(c(TRUE, FALSE), each = 15), age = stats::rnorm(30, 50, 10))
  for (p in list(c("TRUE", "FALSE"), c(TRUE, FALSE), c(1, 0))) {
    tt <- table1_tc(d, by = "treat", vars = c(age = "contn"), smd = TRUE, smdpair = p)
    expect_identical(b3_hdr(tt), "SMD (1 vs 0)")
  }
  a <- table1_tc(d, by = "treat", vars = c(age = "contn"), smd = TRUE, smdpair = c(TRUE, FALSE))
  b <- table1_tc(d, by = "treat", vars = c(age = "contn"), smd = TRUE, smdpair = c(FALSE, TRUE))
  expect_identical(b3_hdr(b), "SMD (0 vs 1)")
})

test_that(".tt_infer_cols recognises every SMD header (BUG-3)", {
  hdr <- function(s) list(list(text = c("Characteristic", "Group 1", "Group 2", "Group 3", s)),
                          list(text = c("", "N=10", "N=10", "N=10", "")))
  for (s in c("SMD", "SMD (1 vs 2)", "Pop. SB", "Max SMD")) {
    expect_identical(tabtools:::.tt_infer_cols(hdr(s), 5, "descriptor")$role[5], "smd", info = s)
  }
  expect_identical(tabtools:::.tt_infer_cols(hdr("Group 4"), 5, "descriptor")$role[5], "group")
  # A group literally named like an SMD header keeps the group role when its
  # second header row says N=..., as a real SMD column's is blank.
  for (s in c("Max SMD", "SMD (x vs y)", "Pop. SB", "SMD")) {
    h <- list(list(text = c("Characteristic", "Group 1", "Group 2", s)),
              list(text = c("", "N=10", "N=10", "N=10")))
    expect_identical(tabtools:::.tt_infer_cols(h, 4, "descriptor")$role[4], "group", info = s)
  }
})

test_that("the xlsx SMD column fits its header (Stata 2.5.6 desctab.ado:2173)", {
  set.seed(1)
  d <- data.frame(arm = factor(rep(c("LongGroupNameA", "LongGroupNameB", "LongGroupNameC"), each = 10)),
                  age = stats::rnorm(30, 50, 10))
  lay <- tabtools:::.xlsx_layout_table1(table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE))
  w <- lay$rules[lay$rules$op == 13, ]
  # "SMD (LongGroupNameA vs LongGroupNameB)" has 38 characters:
  # native max(8, ceiling(38 * 0.85) + 2) is 35, not the old fixed 8.
  expect_identical(w$value[w$c1 == max(w$c1)], 35)
})

test_that("automatic notes join a paragraph footnote as paragraphs; unspaced stays (BUG-5)", {
  d <- data.frame(arm = factor(rep(c("A", "B", "C"), each = 10)), rare = c(rep(0, 28), 1, 1))
  f <- function(fn) table1_tc(d, by = "arm", vars = c(rare = "cat"), smd = TRUE, smallcells = 5, footnote = fn)$footnote
  small <- paste0("Counts below 5 are shown as <5; complementary cells are shown as \u22655",
                  " to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.")
  smd <- "SMD compares A vs B only (the first two of 3 groups)."
  x <- f("Note 1 \\ Note 2")
  x_paragraphs <- c("Note 1", "Note 2", small, smd)
  expect_identical(x, paste(x_paragraphs, collapse = " \\ "))
  expect_identical(strsplit(x, " \\ ", fixed = TRUE)[[1L]], x_paragraphs)
  y <- f("Note 1\\Note 2")
  y_paragraphs <- c("Note 1\\Note 2", small, smd)
  expect_identical(y, paste(y_paragraphs, collapse = " \\ "))
  expect_identical(strsplit(y, " \\ ", fixed = TRUE)[[1L]], y_paragraphs)
})

test_that("the SMD header and note reach every sink", {
  d <- b3_data(1:3, c("2020", "2021", "2022"))
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c("2021", "2022"))
  note <- "SMD compares 2021 vs 2022 only (2 of 3 groups, chosen with smdpair())."
  expect_identical(tt$footnote, note)
  csv <- withr::local_tempfile(fileext = ".csv"); tt_write_csv(tt, csv)
  md <- withr::local_tempfile(fileext = ".md"); tt_write_markdown(tt, md)
  for (f in c(csv, md)) {
    l <- readLines(f)
    expect_true(any(grepl(note, l, fixed = TRUE)))
    expect_true(any(grepl("SMD (2021 vs 2022)", l, fixed = TRUE)))
  }
  expect_true("SMD (2021 vs 2022)" %in% unlist(as.data.frame(tt)[1, ]))
})
