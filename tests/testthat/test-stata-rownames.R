# r(table) row names as Stata tabtools 2.1.12 builds them (regtab.ado
# :2462-2484, desctab.ado:1497-1530). Expectations from
# qa/stata/make_rowname_probe.do, which runs the ado code on
# fixtures/rownames/labels.txt in Stata 17.

test_that("regtab and desctab row names match Stata's for every probed label", {
  dir <- test_path("fixtures", "rownames")
  labs <- readLines(file.path(dir, "labels.txt"), encoding = "UTF-8")
  exp <- utils::read.csv(file.path(dir, "rownames.csv"), colClasses = "character", encoding = "UTF-8")
  expect_identical(nrow(exp), length(labs))
  expect_identical(tabtools:::.rt_rowname(labs, seq_along(labs)), enc2utf8(exp$regtab))
  d <- character()
  for (i in seq_along(labs)) d <- c(d, tabtools:::.t1_rowname(labs[i], i, d))
  expect_identical(d, enc2utf8(exp$desctab))
})

test_that("an interaction name is sanitised, not stored as c.a#c.b (2.1.11)", {
  expect_identical(tabtools:::.rt_rowname("1.foreign#3.rep78", 1L), "1_foreign_3_rep78")
  expect_identical(tabtools:::.rt_rowname(c("var(x[clinic])", "cov(x[clinic],_cons[clinic])"), 1:2),
                   c("var(x[clinic])", "cov_x_clinic__cons_clinic__"))
})
