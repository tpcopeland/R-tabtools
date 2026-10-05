# Regression guards for the second Codex audit (codexaudit2.md, snapshot
# 35c9a12): the descriptive, export and table-object findings. F08 was still
# live on main after the first audit's fixes and is fixed here; F02-F04,
# F07 and F09-F11 had already been fixed by the first audit's work (CX-*,
# R*, D*), and their tests here pin the second audit's own reproducers.
# The regtab findings (F01, F05, F06) are in test-codex-audit2.R.

# F08 ------------------------------------------------------------------------

test_that("F08: periods that round to one label keep their own labels, block headings and stored keys", {
  t <- wttab(c(1, 2, 10, 20), period = c(.001, .001, .002, .002), by = c("A", "B", "A", "B"))
  expect_identical(t$stored$stats$period, rep(c("0.001", "0.002"), each = 3))
  expect_identical(t$body[, 1], c("0.001", "", "", "0.002", "", ""))
  expect_equal(t$stored$stats$mean[c(1, 4)], c(1.5, 15))
  # Values distinct at `digits` keep the `digits` labels (the Stata twin's).
  t2 <- wttab(c(1, 2, 10, 20), period = c(.5, .5, 1.25, 1.25))
  expect_identical(unique(t2$stored$stats$period), c("0.50", "1.25"))
  # Values that differ beyond 17 decimals get their full representation.
  t3 <- wttab(c(1, 2), period = c(1e-20, 2e-20))
  expect_identical(anyDuplicated(t3$stored$stats$period), 0L)
})

test_that("F08: numeric by groups that round alike stay distinct columns", {
  t <- wttab(c(1, 2, 3, 4), by = c(1.001, 1.002, 1.001, 1.002), digits = 1)
  expect_identical(t$header[[1]]$text, c("Statistic", "Overall", "1.001", "1.002"))
})

test_that("F08: two groups or periods with one label are refused, not merged", {
  d <- data.frame(w = c(1, 2, 3, 4), g = c(1, 2, 1, 2))
  expect_error(wttab(d, "w", by = "g", by_labels = c("1" = "Same", "2" = "Same")),
               "same label")
  expect_no_error(wttab(d, "w", by = "g", by_labels = c("1" = "One", "2" = "Two")))
  skip_if_not_installed("haven")
  p <- haven::labelled(c(1, 1, 2, 2), c(First = 1, First = 2))
  expect_error(wttab(d$w, period = p), "same label")
  expect_error(wttab(d$w, by = haven::labelled(d$g, c(X = 1, X = 2))), "same label")
  # The hint's markup is rendered, not printed raw (review of F08).
  msg <- tryCatch(wttab(d, "w", by = "g", by_labels = c("1" = "S", "2" = "S")), error = conditionMessage)
  expect_false(grepl("{.arg", msg, fixed = TRUE))
  expect_match(msg, "by_labels")
})

test_that("F08 review: blank and sub-second collisions say why; labelled values do not widen the others", {
  expect_error(wttab(c(1, 2, 3), by = c("", " ", "a")), "Empty and blank values")
  t0 <- as.POSIXct("2020-01-01 10:00:00", tz = "UTC")
  expect_error(wttab(c(1, 2), period = c(t0, t0 + 0.4)), "less than the display shows")
  skip_if_not_installed("haven")
  p <- haven::labelled(c(0.001, 0.002, 0.5, 0.5), c(early = 0.001, late = 0.002))
  t <- wttab(c(1, 2, 3, 4), period = p)
  expect_identical(unique(t$stored$stats$period), c("early", "late", "0.50"))
})

# Already fixed on main; the second audit's reproducers ----------------------

test_that("F02: a GEE fixed scale held in an outside variable is refused, not reread", {
  skip_if_not_installed("geepack")
  set.seed(1)
  d <- data.frame(x = rnorm(100), y = rnorm(100), id = rep(1:20, each = 5))
  sc <- rep(2, 100)
  fit <- geepack::geeglm(y ~ x, id = id, data = d, corstr = "independence",
                         scale.fix = TRUE, scale.value = sc)
  expect_error(tt_vcov(fit), "does not store")
  expect_error(regtab(fit), "does not store")
})

test_that("F03: duplicate source names in puttab keep both columns' values", {
  d <- data.frame(first = c(1, 2), second = c(100, 200))
  names(d) <- c("value", "value")
  b <- puttab(d)$body
  expect_identical(unname(as.character(b[[2]])), c("100", "200"))
})

test_that("F04: tt_rates refuses grouping columns named like its statistics", {
  for (nm in c("D", "Y", "Rate", "Lower", "Upper")) {
    d <- data.frame(t = c(1, 2, 3), e = c(1, 1, 1), g = c("A", "B", "A"))
    names(d)[3] <- nm
    expect_error(tt_rates(d, time = "t", event = "e", by = nm), "uses for its statistics")
    expect_error(tt_rates(d, time = "t", event = "e", strata = nm), "uses for its statistics")
  }
  d <- data.frame(t = c(1, 2, 3), e = c(1, 1, 1), grp = c("A", "B", "A"))
  expect_identical(as.character(tt_rates(d, time = "t", event = "e", by = "grp")$grp), c("A", "B"))
})

test_that("F07: Table 1 weighted quartiles do not change with a small weight scale", {
  f <- function(w) table1_tc(data.frame(x = 1:10, w = rep(w, 10)),
                             vars = c(x = "conts %5.1f"), wt = "w")$body
  expect_identical(f(1e-10), f(1))
  expect_identical(f(1e-12), f(1))
})

test_that("F09: wttab ESS is scale-invariant at the ends of the double range", {
  for (s in c(1e200, 1e-200, 1e-160)) {
    st <- wttab(rep(s, 5))$stored$stats
    expect_equal(st$ess, 5)
    expect_equal(st$ess_pct, 100)
  }
  st <- wttab(c(1, 2, 3) * 1e-200)$stored$stats
  expect_equal(st$ess, 36 / 14)
})

test_that("F10: a CSV export whose close() fails is an error, and puttab records no target", {
  skip_on_os(c("windows", "mac"))
  skip_if_not(file.exists("/dev/full"))
  p <- tempfile(fileext = ".csv")
  skip_if_not(file.symlink("/dev/full", p))
  withr::defer(unlink(p))
  expect_error(tt_write_csv(puttab(data.frame(x = 1:3)), p), "Could not write")
  expect_error(puttab(data.frame(x = 1:3), csv = p), "Could not write")
})

test_that("F11: validate_tt_table refuses missing separator blocks that break printing", {
  mk <- function(block) {
    tt_table(data.frame(label = c("a", "b"), value = c("1", "2")),
             header = list(c("Label", "Value")), command = "custom",
             rows = data.frame(block = block), layout = list(console_sepby = TRUE))
  }
  expect_error(mk(c(1, NA)), "rows\\$block")
  t <- mk(c(1, 2))
  expect_s3_class(validate_tt_table(t), "tt_table")
  expect_type(format(t), "character")
})
