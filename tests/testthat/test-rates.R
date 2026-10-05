# tt_rates() (plan task 7.3) against Stata 17 strate on the cohort fixture
# (golden rates_strate.csv, qa/stata/make_rates_goldens.do).

rates_golden <- function() {
  utils::read.csv(golden_path("rates_strate.csv"), colClasses = "character", na.strings = character())
}

rate_scenarios <- list(
  S1 = list(by = "treated", per = 1000),
  S2 = list(by = "education", per = 1000, level = 0.90),
  S3 = list(by = c("treated", "education"), per = 365.25),
  S4 = list(by = NULL, per = 1),
  S5 = list(by = "smoking", per = 1000, missing = TRUE),
  S6 = list(by = "smoking", per = 1000),
  S7 = list(by = "treated", per = 1000, entry = 365),
  S8 = list(by = "treated", per = 1000, fweight = "fw"),
  S9 = list(by = "ctrl_grade", per = 1000)
)

test_that("tt_rates reproduces strate for every golden scenario", {
  g <- rates_golden()
  d <- golden_fixture("cohort")
  num <- function(s) ifelse(s == ".", NA_real_, as.numeric(s))
  for (id in names(rate_scenarios)) {
    a <- rate_scenarios[[id]]
    r <- do.call(tabtools:::tt_rates, c(list(d, time = "follow_up", event = "cv_event"), a))
    w <- g[g$scenario == id, ]
    expect_identical(nrow(r), nrow(w), label = paste(id, "groups"))
    key <- if (is.null(a$by)) rep("all", nrow(r)) else
      apply(r[, a$by, drop = FALSE], 1L, function(x) paste(ifelse(is.na(x), ".", format(x, trim = TRUE)), collapse = ";"))
    expect_identical(unname(key), w$group, label = paste(id, "group order"))
    expect_identical(r$D, num(w$D), label = paste(id, "D"))
    for (col in c("Y", "Rate", "Lower", "Upper")) {
      expect_equal(r[[col]], num(w[[col]]), tolerance = 1e-14, label = paste(id, col))
    }
  }
})

test_that("person-time uses Stata's float storage for compressed whole-day times", {
  d <- golden_fixture("cohort")
  g <- rates_golden()
  exact <- tabtools:::tt_rates(d, "follow_up", "cv_event", by = "treated", per = 1000)
  dbl <- tabtools:::tt_rates(d, "follow_up", "cv_event", by = "treated", per = 1000, float_time = FALSE)
  want <- as.numeric(g$Y[g$scenario == "S1"])
  expect_identical(exact$Y, want)
  expect_false(identical(dbl$Y, want))
  expect_equal(dbl$Y, want, tolerance = 1e-8)
})

test_that("zero-event groups have missing CIs; late entry drops early exits", {
  d <- data.frame(t = c(5, 10, 3, 8, 2), e = c(0, 0, 1, 1, 1), g = c(1, 1, 2, 2, 2))
  r <- tabtools:::tt_rates(d, "t", "e", by = "g")
  expect_equal(r$D, c(0, 3))
  expect_equal(r$Y, c(15, 13))
  expect_true(all(is.na(r[1, c("Lower", "Upper")])))
  expect_equal(r$Lower[2], exp(log(3 / 13) - stats::qnorm(0.975) / sqrt(3)))
  r <- tabtools:::tt_rates(d, "t", "e", by = "g", entry = 3)
  expect_equal(r$D, c(0, 1))
  expect_equal(r$Y, c(2 + 7, 5))
})

test_that("strata are the outer grouping and missing groups sort last", {
  d <- data.frame(t = 1:8, e = rep(c(0, 1), 4), x = c(2, 1, NA, 2, 1, NA, 2, 1), s = rep(c("b", "a"), each = 4))
  r <- tabtools:::tt_rates(d, "t", "e", by = "x", strata = "s", missing = TRUE)
  expect_identical(r$s, c("a", "a", "a", "b", "b", "b"))
  expect_identical(r$x, c(1, 2, NA, 1, 2, NA))
  expect_identical(nrow(tabtools:::tt_rates(d, "t", "e", by = "x")), 2L)
})

test_that("strate-shaped data frames are accepted and standardised", {
  s <- data.frame(drug_class = c("SSRI", "SNRI"), `_D` = c(178, 161), `_Y` = c(28100, 22300),
                  `_Rate` = c(0.00633, 0.00722), `_Lower` = c(0.00545, 0.00616),
                  `_Upper` = c(0.00734, 0.00844), check.names = FALSE)
  r <- tabtools:::tt_rates(s)
  expect_named(r, c("drug_class", "D", "Y", "Rate", "Lower", "Upper"))
  expect_identical(attr(r, "by"), "drug_class")
  expect_identical(attr(r, "per"), 1)
  # The file's units are its own: `per` cannot rescale it (Phase 7b review F7c).
  expect_error(tabtools:::tt_rates(s, per = 1000), "time and event data only")
  expect_error(tabtools:::tt_rates(s[, -2]), "strate shape")
})

test_that("input validation", {
  d <- data.frame(t = 1:3, e = c(0, 1, 1))
  expect_error(tabtools:::tt_rates(d, "t", "nope"), "not found")
  expect_error(tabtools:::tt_rates(d, "t", "e", per = 0), "per")
  # A percentage is accepted as in regtab() (Phase 7b review F7a).
  expect_identical(attr(tabtools:::tt_rates(d, "t", "e", level = 95), "level"), 0.95)
  expect_error(tabtools:::tt_rates(d, "t", "e", level = 100), "level")
  expect_error(tabtools:::tt_rates(d, "t", "e", level = 1), "level")
  expect_error(tabtools:::tt_rates(list(), "t", "e"), "data frame")
})

# Milestone H, H15 (finding F20): validation before 7.4 wires the helpers.

test_that("H15: frequency weights must be non-negative integers, as Stata's [fweight]", {
  d <- data.frame(t = c(2, 4, 3), e = c(1, 1, 0), w = c(-1, 0.5, 1))
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", fweight = "w"), "negative")
  d$w <- c(1, 0.5, 1)
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", fweight = "w"), "noninteger")
  d$w <- c(2, 0, NA)
  r <- tabtools:::tt_rates(d, time = "t", event = "e", fweight = "w")
  expect_identical(c(r$D, r$Y), c(2, 4))
  d$w <- c(1, Inf, 1)
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", fweight = "w"), "finite")
  d$w <- factor(c(1, 2, 1))
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", fweight = "w"), "numeric")
})

test_that("H15: time, event and entry must be finite numbers; per must be finite", {
  d <- data.frame(t = c(2, 4, 3), e = c(1, 1, 0), g = c("a", "b", "a"))
  expect_error(tabtools:::tt_rates(transform(d, t = factor(t)), time = "t", event = "e"), "`time`.*numeric")
  expect_error(tabtools:::tt_rates(transform(d, t = as.character(t)), time = "t", event = "e"), "numeric")
  expect_error(tabtools:::tt_rates(transform(d, e = factor(e)), time = "t", event = "e"), "`event`.*numeric")
  expect_error(tabtools:::tt_rates(transform(d, t = c(2, Inf, 3)), time = "t", event = "e"), "finite")
  expect_error(tabtools:::tt_rates(transform(d, t = c(2, NaN, 3)), time = "t", event = "e"), "finite")
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", per = Inf), "positive number")
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", entry = Inf), "entry")
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", entry = "1"), "not found")
  # A grouping factor is fine; a logical event is 0/1.
  r <- tabtools:::tt_rates(transform(d, g = factor(g), e = e == 1), time = "t", event = "e", by = "g")
  expect_identical(r$D, c(1, 1))
})

test_that("H15: an empty st sample is refused (strate: no observations)", {
  d <- data.frame(t = c(2, 4), e = c(1, 1))
  expect_error(tabtools:::tt_rates(d, time = "t", event = "e", entry = 5), "No observations")
  expect_error(tabtools:::tt_rates(d[0, ], time = "t", event = "e"), "No observations")
  expect_error(tabtools:::tt_rates(data.frame(t = c(2, 4), e = 1, g = NA), time = "t", event = "e", by = "g"),
               "No observations")
})

test_that("H15: strate-shaped imports are validated, with Stata's '.' read as missing", {
  s <- data.frame(D = "garbage", Y = 2, Rate = .5, Lower = .1, Upper = 1)
  expect_error(tabtools:::tt_rates_from_strate(s), "garbage")
  s <- data.frame(D = c("0", "3"), Y = c("10", "20"), Rate = c("0", ".15"), Lower = c(".", "0.05"),
                  Upper = c("", "0.4"), g = c("a", "b"))
  r <- tabtools:::tt_rates_from_strate(s)
  expect_identical(r$D, c(0, 3))
  expect_true(is.na(r$Lower[1]) && is.na(r$Upper[1]))
  s$D <- factor(s$D)
  expect_identical(tabtools:::tt_rates_from_strate(s)$D, c(0, 3))
  expect_error(tabtools:::tt_rates_from_strate(data.frame(D = -1, Y = 2, Rate = 1, Lower = 0, Upper = 1)),
               "non-negative")
  expect_error(tabtools:::tt_rates_from_strate(data.frame(D = 1, Y = Inf, Rate = 1, Lower = 0, Upper = 1)),
               "finite")
  expect_error(tabtools:::tt_rates_from_strate(data.frame(D = TRUE, Y = 2, Rate = 1, Lower = 0, Upper = 1)),
               "numeric")
})

test_that("review R14: hex text is not a number; all-zero frequencies say so", {
  expect_error(tabtools:::tt_rates_from_strate(data.frame(D = "0x10", Y = 2, Rate = 1, Lower = 0, Upper = 1)),
               "0x10")
  expect_error(tabtools:::tt_rates_from_strate(data.frame(D = "Inf", Y = 2, Rate = 1, Lower = 0, Upper = 1)),
               "numeric")
  expect_identical(tabtools:::tt_rates_from_strate(
    data.frame(D = c("1e2", "+3", ".5"), Y = 2, Rate = 1, Lower = 0, Upper = 1))$D, c(100, 3, 0.5))
  expect_error(tabtools:::tt_rates(data.frame(t = 1:2, e = 1, w = 0), time = "t", event = "e", fweight = "w"),
               "frequency weight is zero")
})
