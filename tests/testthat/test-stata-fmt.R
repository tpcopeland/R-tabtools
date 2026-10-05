test_that("stata_fmt never pads left and keeps Stata's rounding", {
  f <- tabtools:::stata_fmt
  expect_identical(f(5, "%9.1f"), "5.0")
  expect_identical(f(c(12.5, 0.125, 2.675), "%5.2f"), c("12.50", "0.12", "2.67"))
  expect_identical(f(-0, "%5.1f"), "0.0")
  expect_identical(f(-0.0004, "%5.1f"), "-0.0")
  expect_identical(f(NA, "%5.1f"), ".")
})

test_that("stata_fmt switches to e-notation only past width and 9 characters", {
  f <- tabtools:::stata_fmt
  expect_identical(f(12345678, "%2.0f"), "12345678")
  expect_identical(f(100000000, "%2.0f"), "1.0e+08")
  expect_identical(f(1e9, "%9.0f"), "1.00e+09")
  expect_identical(f(12345678, "%9.1f"), "1.2e+07")
  expect_identical(f(99999.5, "%5.3f"), "1.0e+05")
})

test_that("stata_fmt comma formats drop commas, then fall back to e-notation", {
  f <- tabtools:::stata_fmt
  expect_identical(f(c(4099, 1234567), "%12.0fc"), c("4,099", "1,234,567"))
  expect_identical(f(1e9, "%12.0fc"), "1000000000")
  expect_identical(f(1e15, "%12.0fc"), "1.00000e+15")
  expect_identical(f(-1234567, "%9.0fc"), "-1234567")
  expect_identical(f(4099, "%-12.0fc"), "4,099       ")
})

test_that("stata_fmt %g drops the leading zero and limits significant digits", {
  f <- tabtools:::stata_fmt
  expect_identical(f(c(0.125, -0.0004, 3.14159265), "%9.0g"), c(".125", "-.0004", "3.141593"))
  expect_identical(f(1234567, "%8.0g"), "1.2e+06")
  expect_identical(f(1e-5, "%9.0g"), ".00001")
  expect_error(f(1, "%9s"), "Unsupported")
})

test_that("stata_round sends halves toward +Inf", {
  r <- tabtools:::stata_round
  expect_equal(r(c(0.125, -0.125, 2.675, 1.115), 0.01), c(0.13, -0.12, 2.68, 1.12))
  expect_identical(tabtools:::stata_fmt(r(8934 / 15000, 0.001) * 100, "%9.1f"), "59.6")
})

test_that("stata_fmt caps the e-notation mantissa at 18 decimals, as Stata does (Phase 7a review F9)", {
  # Probed in Stata 17 (tabtools 2.1.11 review, probe/fmt.do): %24.0f to %32.0f.
  # Exact doubles: R's parser misreads 1e33 and 1.5e200 without long double.
  f <- function(x, fmt) tabtools:::stata_fmt(exact_num(x), fmt)
  expect_identical(f("1e33", "%32.0f"), "9.999999999999999456e+32")
  expect_identical(f("-1e33", "%32.0f"), "-9.999999999999999456e+32")
  expect_identical(f("1e40", "%32.0f"), "1.000000000000000030e+40")
  expect_identical(f("1.5e200", "%32.0f"), "1.499999999999999955e+200")
  expect_identical(f("1e33", "%30.0f"), "9.999999999999999456e+32")
  expect_identical(f("-1e33", "%30.0f"), "-9.999999999999999456e+32")
  expect_identical(f("1e40", "%30.0f"), "1.000000000000000030e+40")
  expect_identical(f("1.5e200", "%30.0f"), "1.499999999999999955e+200")
  expect_identical(f("1e33", "%27.0f"), "9.999999999999999456e+32")
  expect_identical(f("-1e33", "%27.0f"), "-9.999999999999999456e+32")
  expect_identical(f("1e40", "%27.0f"), "1.000000000000000030e+40")
  expect_identical(f("1.5e200", "%27.0f"), "1.499999999999999955e+200")
  expect_identical(f("1e33", "%26.0f"), "9.999999999999999456e+32")
  expect_identical(f("-1e33", "%26.0f"), "-9.999999999999999456e+32")
  expect_identical(f("1e40", "%26.0f"), "1.000000000000000030e+40")
  expect_identical(f("1.5e200", "%26.0f"), "1.499999999999999955e+200")
  expect_identical(f("1e33", "%25.0f"), "9.999999999999999456e+32")
  expect_identical(f("-1e33", "%25.0f"), "-9.999999999999999456e+32")
  expect_identical(f("1e40", "%25.0f"), "1.000000000000000030e+40")
  expect_identical(f("1.5e200", "%25.0f"), "1.49999999999999996e+200")
  expect_identical(f("1e33", "%24.0f"), "9.99999999999999946e+32")
  expect_identical(f("-1e33", "%24.0f"), "-9.99999999999999946e+32")
  expect_identical(f("1e40", "%24.0f"), "1.00000000000000003e+40")
  expect_identical(f("1.5e200", "%24.0f"), "1.5000000000000000e+200")
})

test_that("stata_fmt renders %w.dg with d > 0 and zero-padded %0w.df as Stata 17 does (audit A03)", {
  # fixtures/stata_fmt_a03.csv: string(x, fmt) from Stata 17 (stata-mp -b,
  # 2026-09-29) for the named values and formats of the audit plus a grid of
  # %w.dg (w = 2 to 16) and %0w.df formats. "" is Stata's text for an
  # invalid format (d >= w), which R refuses.
  fx <- utils::read.csv(test_path("fixtures", "stata_fmt_a03.csv"), colClasses = "character",
                        na.strings = character())
  x <- rep(NA_real_, nrow(fx))
  x[fx$x != "NA"] <- exact_num(fx$x[fx$x != "NA"])
  f <- tabtools:::stata_fmt
  for (fmt in unique(fx$fmt)) {
    k <- fx$fmt == fmt
    if (all(fx$stata[k] == "")) {
      expect_error(f(x[k], fmt), "Unsupported Stata display format")
    } else {
      expect_identical(f(x[k], fmt), fx$stata[k], label = fmt)
    }
  }
  expect_identical(f(c(3.14159, 14.795321, 0.000123, 123456.789), "%9.2g"), c("3.1", "15", ".00012", "123457"))
  expect_identical(f(c(3.14159, 14.795321, 0.000123, 1e10), "%10.4g"), c("3.142", "14.8", ".000123", "1.000e+10"))
  expect_identical(f(c(99.5, 999.5, 9.995), "%9.2g"), c("1.0e+02", "1.0e+03", "10"))
  expect_identical(f(c(3.14159, -0.5, 1e10, NA), "%09.2f"), c("000003.14", "-00000.50", "1.00e+10", "."))
  expect_identical(f(1e10, "%-09.2f"), "1.00e+10 ")
  expect_identical(f(3.14159, "%-9.2g"), "3.1      ")
  expect_identical(f(3.14159, "%09.1fc"), "3.1")
  expect_error(f(1, "%5.5g"), "Unsupported")
  expect_error(f(1, "%05.5f"), "Unsupported")
  expect_error(f(1, "%0-9.2f"), "Unsupported")
})

test_that("table1_tc applies a per-variable %w.dg format with Stata's significant digits (audit A03)", {
  d <- data.frame(g = rep(1:2, 10), x = seq(1.11111, 30, length.out = 20))
  out <- as.data.frame(table1_tc(d, by = "g", vars = c(x = "contn %9.2g")))
  expect_identical(unname(unlist(out[3, 2:3])), c("15±9.2", "16±9.2"))
  out0 <- as.data.frame(table1_tc(d, by = "g", vars = c(x = "contn %09.2f")))
  expect_identical(unname(unlist(out0[3, 2:3])), c("000014.80±000009.21", "000016.32±000009.21"))
})

test_that("stata_fmt inserts Stata's thousands separators in %w.0gc and %w.dgc (audit A11)", {
  # fixtures/stata_fmt_a11.csv: string(x, fmt) from Stata 17 (stata-mp -b,
  # 2026-09-29) for %w.dgc formats, their left-justified and zero-flag
  # forms, and every %w.0gc from %1.0gc to %20.0gc.
  fx <- utils::read.csv(test_path("fixtures", "stata_fmt_a11.csv"), colClasses = "character",
                        na.strings = character())
  x <- rep(NA_real_, nrow(fx))
  x[fx$x != "NA"] <- exact_num(fx$x[fx$x != "NA"])
  f <- tabtools:::stata_fmt
  for (fmt in unique(fx$fmt)) {
    k <- fx$fmt == fmt
    expect_identical(f(x[k], fmt), fx$stata[k], label = fmt)
  }
  expect_identical(f(c(999.999, 1000, 1234.5678, 12345.678), "%6.0gc"), c("1,000", "1000", "1235", "1.2e+04"))
  expect_identical(f(c(1234.5678, 1234568), "%8.0gc"), c("1,234.6", "1.2e+06"))
  expect_identical(f(1234568, c("%10.0gc")), "1234568")
  expect_identical(f(1234568, c("%10.1gc")), "1,234,568")
  expect_identical(f(-1234567.8, "%11.8gc"), "-1234567.8")
})

test_that("stata_fmt keeps %w.df fixed when rounding carries the text to a new digit (audit A12)", {
  # fixtures/stata_fmt_a12.csv: string(x, fmt) from Stata 17 (stata-mp -b,
  # 2026-09-29) for values whose rounding adds an integer digit (9.999999,
  # -99.99999, 99999999.5, ...) and their sign twins, under the %w.df,
  # %w.dfc, %-w.df and %0w.df formats where R differed.
  fx <- utils::read.csv(test_path("fixtures", "stata_fmt_a12.csv"), colClasses = "character",
                        na.strings = character())
  x <- exact_num(fx$x)
  f <- tabtools:::stata_fmt
  for (fmt in unique(fx$fmt)) {
    k <- fx$fmt == fmt
    expect_identical(f(x[k], fmt), fx$stata[k], label = fmt)
  }
  expect_identical(f(-9.999999, "%6.5f"), "-10.00000")
  expect_identical(f(-99.99999, "%5.4f"), "-100.0000")
  expect_identical(f(-9999.999, "%3.2f"), "-10000.00")
  expect_identical(f(9999999999.5, "%10.0f"), "10000000000")
  # Without a carry the rule is unchanged.
  expect_identical(f(100000000, "%2.0f"), "1.0e+08")
  expect_identical(f(12345678, "%2.0f"), "12345678")
})

test_that("stata_fmt gives %w.dg and %w.dgc carries to 10^7 and above Stata's mantissa decimals (review P2-1)", {
  # fixtures/stata_fmt_a03_carry.csv: string(x, fmt) from Stata 17 (stata-mp -b,
  # 2026-09-29) for 10^k - {0.5, 0.3, 0.05, 0.001, 1e-6} and 10^k (1 - 1e-12),
  # k = 5..13, both signs, under the %w.dg, %w.dgc and %-w.dg formats where
  # R differed.
  fx <- utils::read.csv(test_path("fixtures", "stata_fmt_a03_carry.csv"), colClasses = "character",
                        na.strings = character())
  x <- exact_num(fx$x)
  f <- tabtools:::stata_fmt
  for (fmt in unique(fx$fmt)) {
    k <- fx$fmt == fmt
    expect_identical(f(x[k], fmt), fx$stata[k], label = fmt)
  }
  expect_identical(f(9999999.5, "%9.2g"), "1.00e+07")
  expect_identical(f(99999999.5, "%10.4g"), "1.000e+08")
  expect_identical(f(9999999999.5, "%12.5g"), "1.00000e+10")
  expect_identical(f(-99999999.5, "%12.3gc"), "-1.000e+08")
  # Carries up to 10^6 keep one decimal.
  expect_identical(f(c(99.5, 999.5), "%9.2g"), c("1.0e+02", "1.0e+03"))
})
