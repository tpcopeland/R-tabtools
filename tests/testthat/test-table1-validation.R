# table1_tc argument validation (plan task 2.1). Conditions mirror
# desctab.ado:93-333 and _desctab_collect.ado:185-307; Stata's messages are
# cited beside each check. Stata error behaviour for bad vars() specs was
# probed in Stata 17 on 2026-09-25.

test_that("option errors mirror Stata's conditions", {
  a <- golden_fixture("auto")
  f <- function(...) table1_tc(a, by = "foreign", vars = "price contn", ...)
  md <- withr::local_tempfile(fileext = ".md")
  xl <- withr::local_tempfile(fileext = ".xlsx")
  expect_error(f(pdp = 0), "pdp")                                   # r(198) pdp() must be between 1 and 10
  expect_error(f(highpdp = 11), "highpdp")
  expect_error(f(total = "foo"), "should be one of")                # r(198) total() must be before or after
  expect_error(f(open = TRUE), "requires")                          # r(498) open requires excel() or xlsx()
  expect_error(f(xlsx = "out.xls"), ".xlsx file")                   # r(198)
  expect_error(f(mdappend = TRUE), "requires")                      # r(198) mdappend requires markdown()
  expect_error(f(markdown = "out.txt"), ".md")                      # r(198)
  expect_error(f(sheet = "S"), "only available")                    # r(498) sheet() only with excel()
  expect_no_error(f(title = "T", markdown = md))
  expect_error(f(borderstyle = "dotted", xlsx = xl), "borderstyle")  # r(498)
  expect_error(f(sheet = "a/b", xlsx = xl), "not allowed")
  expect_error(f(boldp = 1), "between 0 and 1")                     # r(198)
  expect_error(f(highlight = 0), "between 0 and 1")
  expect_error(f(smdthreshold = 0), "smdthreshold")                 # r(198)
  expect_error(table1_tc(a, vars = "price contn", smd = TRUE), "requires")  # r(198) smd requires by()
  expect_error(f(smallcells = 2), "greater than or equal to 3")     # r(198)
  expect_error(f(smallcells = 5, percent = TRUE), "percent-only")   # r(198)
  expect_error(f(wt = "price", fweight = "price"), "cannot be used together")
  expect_error(f(wtn = TRUE), "requires")                           # r(198) wtn requires wt()
  expect_error(f(wtcompare = TRUE), "requires")
  expect_error(f(test_args = list(t.tst = list())), "test_args")
  expect_error(f(missing = NA), "TRUE or FALSE")
  expect_error(f(nformat = "%5.1x"), "Unsupported Stata display format")
})

# R-only relaxation (user decision 2026-09-26, Phase 6 review (a)): Stata
# refuses title() without excel()/markdown() (r(498)) and borderstyle()
# without excel() (r(498)) because its table has nowhere else to go; an R
# table goes on to as_flextable()/tt_as_gt()/tt_write_xlsx().
test_that("title, footnote and borderstyle need no file sink", {
  a <- golden_fixture("auto")
  f <- function(...) table1_tc(a, by = "foreign", vars = "price contn", ...)
  tab <- f(title = "Table 1", footnote = "Note.", borderstyle = "academic")
  expect_identical(tab$title, "Table 1")
  expect_identical(tab$footnote, "Note.")
  expect_identical(tab$style$borderstyle, "academic")
  expect_error(f(borderstyle = "dotted"), "borderstyle")
  skip_if_not_installed("flextable")
  cells <- interop_ft_cells(flextable::as_flextable(tab))
  expect_true(all(is.na(cells$left)) && all(is.na(cells$right)))
  expect_setequal(stats::na.omit(unique(c(cells$top, cells$bottom))), "medium")
})

test_that("weights and small cells are implemented (Phase 3)", {
  a <- golden_fixture("auto")
  expect_s3_class(table1_tc(a, by = "foreign", vars = "price contn", wt = "price"), "tt_table")
  expect_s3_class(table1_tc(a, by = "foreign", vars = "price contn", smallcells = 5), "tt_table")
})

test_that("by() must be string or non-negative integers with at least two levels", {
  a <- golden_fixture("auto")
  a$neg <- a$foreign - 1
  a$frac <- a$foreign + 0.5
  expect_error(table1_tc(a, by = "neg", vars = "price contn"), "non-negative integers")   # r(498)
  expect_error(table1_tc(a, by = "frac", vars = "price contn"), "non-negative integers")
  expect_error(table1_tc(a[a$foreign == 1, ], by = "foreign", vars = "price contn"), "at least 2 levels")
  expect_error(table1_tc(a, by = "nosuch", vars = "price contn"), "not found")            # r(111)
  a$allna <- NA_real_
  expect_error(table1_tc(a, by = "allna", vars = "price contn"), "no observations")        # r(2000)
})

test_that("vars() entries are checked as Stata does", {
  a <- golden_fixture("auto")
  a$two <- a$foreign + 1
  a$allmiss <- NA_real_
  f <- function(v) table1_tc(a, by = "foreign", vars = v)
  expect_error(f("price foo"), "not allowed")                 # r(498) -price foo- not allowed in vars()
  expect_error(f("make contn"), "must be numeric")           # r(109) make must be numeric for type contn
  expect_error(f("two bin"), "must be 0 \\(negative\\) or 1") # r(198) ... Did you mean cat?
  expect_error(f("nonexist cat"), "not found")               # r(111) variable nonexist not found
  expect_error(f("allmiss bin"), "no categories")            # r(198)
  expect_error(f("allmiss cat"), "no categories")
  expect_error(f("price contn %5.1x"), "Unsupported Stata display format")  # Stata renders "" silently
  expect_error(f(" \\ "), "did not contain any variables")
})
