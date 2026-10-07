# Phase 7b review (scratchpad review-7b/REVIEW.md, F1-F14): one test per
# finding, plus the tests that kill the surviving mutants (M02, M03, M09,
# M11, M13, M17, M20, M27). M06 is equivalent (an infinite ratio becomes
# missing through stata_macro_num()) and M15 nearly so (z rounded to 16
# significant digits).

rv_block <- function(cats = c("A", "B"), D = c(10, 20), Y = c(1000, 2000), level = "95", rate = D / Y,
                     lo = rate * 0.5, hi = rate * 2) {
  b <- data.frame(g = cats, `_D` = D, `_Y` = Y, `_Rate` = rate, `_Lower` = lo, `_Upper` = hi,
                  check.names = FALSE)
  if (!is.na(level)) {
    attr(b[["_Lower"]], "label") <- paste0("Lower ", level, "% confidence limit")
    attr(b[["_Upper"]], "label") <- paste0("Upper ", level, "% confidence limit")
  }
  b
}

test_that("F1: a strate label written under set dp comma is read as 97.5%, not 5% (as Stata 2.1.14)", {
  b <- rv_block(level = "97,5")
  expect_identical(attr(tt_rates(b), "level"), 0.975)
  tt <- stratetab(list(b, b), outcomes = 1, rateratio = TRUE)
  expect_identical(tt$header[[2]]$text[4:5], c("Per 1,000 PY (97.5% CI)", "IRR (97.5% CI)"))
  expect_identical(tt$stored$ci_level, 97.5)
  expect_identical(stratetab(b, level = 97.5)$stored$ci_level, 97.5)
  expect_error(stratetab(b, level = 5), "conflicts with block 1's 97.5% intervals")
  # The Stata-written files of the review's probe (qa/stata/make_stratetab_dp.do):
  # Stata 2.1.11-2.1.13 printed "Per 1,000 PY (5% CI)" and IRR 1.00 (0.98, 1.02),
  # intervals at 5%; 2.1.14 reads 97.5% and prints exactly these cells
  # (probed on Stata-Tools 1255176d, task C8).
  skip_if_not_installed("haven")
  paths <- test_path("fixtures", "stratetab_dp", c("dp_1", "dp_2"))
  st <- stratetab(paths, outcomes = 1, rateratio = TRUE)
  expect_identical(st$header[[2]]$text[4:5], c("Per 1,000 PY (97.5% CI)", "IRR (97.5% CI)"))
  expect_identical(st$body$c4, c("", "51.7 (32.7, 81.7)", "69.0 (46.7, 101.9)", "",
                                 "51.7 (32.7, 81.7)", "69.0 (46.7, 101.9)"))
  z <- stats::qnorm(1 - 0.025 / 2)
  want <- sprintf("1.00 (%.2f, %.2f)", exp(-z * sqrt(2 / 24)), exp(z * sqrt(2 / 24)))
  expect_identical(st$body$c5[5], want)
  expect_identical(want, "1.00 (0.52, 1.91)")
})

test_that("F3: an empty unitlabel is the default, as in Stata; a blank one is kept", {
  b <- rv_block()
  expect_identical(stratetab(b, unitlabel = "")$header[[2]]$text[4], "Per 1,000 PY (95% CI)")
  # stratetab.ado:293 tests `"`unitlabel'" == ""`: unitlabel(" ") stays (probed).
  expect_identical(stratetab(b, unitlabel = " ")$header[[2]]$text[4], "Per   PY (95% CI)")
})

test_that("F6: the frame analogue carries the title, the column layout, and a stated row numbering", {
  a <- rv_block()
  tt <- stratetab(list(a, a, a), outcomes = 3, rateratio = FALSE, title = "Rates", footnote = "Note")
  df <- as.data.frame(tt)
  expect_identical(attr(df, "title"), "Rates")
  expect_identical(attr(df, "footnote"), "Note")
  expect_false(attr(df, "rateratio"))
  expect_identical(attr(df, "cols_per_outcome"), 3L)
  rr <- stratetab(list(a, a, a, a, a, a), outcomes = 3, rateratio = TRUE)
  expect_identical(attr(as.data.frame(rr), "cols_per_outcome"), 4L)
  expect_true(attr(as.data.frame(rr), "rateratio"))
  # 3 outcomes with IRR columns and 4 without have the same width.
  plain4 <- stratetab(list(a, a, a, a), outcomes = 4)
  expect_identical(ncol(as.data.frame(rr)), ncol(as.data.frame(plain4)))
  expect_false(identical(attr(as.data.frame(rr), "cols_per_outcome"), attr(as.data.frame(plain4), "cols_per_outcome")))
  # rate_rows$row is the body row: data-frame row row + 2, worksheet row row + 3.
  r <- tt$meta$rate_rows
  expect_identical(df[r$row + 2L, 1], tt$body$c1[r$row])
  expect_identical(df[r$row + 2L, 1], paste0("   ", r$category))
  out <- withr::local_tempdir()
  book <- file.path(out, "f6.xlsx")
  tt_write_xlsx(tt, book, sheet = "Results")
  skip_if_not_installed("openxlsx2")
  grid <- openxlsx2::wb_to_df(book, sheet = "Results", col_names = FALSE, skip_empty_rows = FALSE,
                              skip_empty_cols = FALSE)
  expect_identical(unname(grid[[2]][r$row[1] + 3L]), tt$body$c1[r$row[1]])
  # comptab's reference row is the first category of each block by position.
  first <- !duplicated(tt$rows$block[tt$rows$type == "level"])
  expect_identical(tt$body$c1[tt$rows$type == "level"][first], c("   A"))
})

test_that("F7: tt_rates() takes a percentage level, needs time and event together, refuses per on strate input", {
  set.seed(5)
  d <- data.frame(t = rexp(100, 0.1), e = rbinom(100, 1, 0.3), g = rep(c("u", "v"), 50))
  expect_identical(attr(tt_rates(d, "t", "e", by = "g", level = 90), "level"), 0.9)
  expect_identical(attr(tt_rates(d, "t", "e", by = "g"), "level"), 0.95)
  expect_equal(tt_rates(d, "t", "e", by = "g", level = 90), tt_rates(d, "t", "e", by = "g", level = 0.9))
  expect_error(tt_rates(d, "t"), "must be given together")
  expect_error(tt_rates(d, event = "e"), "must be given together")
  s <- rv_block()
  expect_error(tt_rates(s, per = 1000), "applies to time and event data only")
  expect_identical(attr(tt_rates(s, level = 95), "level"), 0.95)
  expect_error(tt_rates(s, level = 90), "90%\\) conflicts with the 95% intervals")
  # "NA" in a character column is missing, like ".".
  txt <- rv_block(cats = c("A", "B"))
  txt[["_Lower"]] <- c("0.005", "NA")
  expect_true(is.na(tt_rates(txt)$Lower[2]))
})

test_that("F8: a tt_rates(per = 1000) block is refused under the default ratescale, never shown 1000x too high", {
  set.seed(6)
  d <- data.frame(t = rexp(300, 0.1), e = rbinom(300, 1, 0.3), g = rep(c("u", "v", "w"), 100))
  b1 <- tt_rates(d, "t", "e", by = "g")
  b1000 <- tt_rates(d, "t", "e", by = "g", per = 1000)
  expect_error(stratetab(b1000), "computed with `per = 1000`")
  expect_error(stratetab(list(b1, b1000), outcomes = 2), "different `per`")
  # The scales the message names give exactly the per = 1 table.
  expect_identical(stratetab(b1000, ratescale = 1, pyscale = 1 / 1000)$body, stratetab(b1)$body)
  # An explicit ratescale is Stata's semantics: the stored rates are scaled.
  expect_identical(stratetab(b1000, ratescale = 1)$body$c4, stratetab(b1)$body$c4)
})

test_that("F9/C8: stratetab rounds at the exact unit 10^(-d), as Stata 2.1.14, entry by entry", {
  g <- utils::read.csv(test_path("fixtures", "stratetab_round", "stata_round.csv"), stringsAsFactors = FALSE)
  # Stata's %21x: "+1.47ae147ae147cX-007", the exponent in hexadecimal too.
  hex <- function(s) {
    m <- regmatches(s, regexec("^([+-])([0-9a-f.]+)X([+-])([0-9a-f]+)$", s))
    vapply(m, function(p) {
      e <- strtoi(p[5], 16L) * if (p[4] == "-") -1L else 1L
      as.numeric(sprintf("%s0x%sp%d", p[2], p[3], e))
    }, 0)
  }
  expect_false(anyNA(hex(c(g$x, g$unit, g$r, g$munit, g$mr))))
  x <- hex(g$x)
  # Since tabtools 2.1.14 the unit is macro text, `local _unit =
  # 10^(-`digits')` (stratetab.ado:594), which reads back as R's 10^(-d).
  expect_identical(hex(g$munit), 10^(-g$d))
  expect_identical(stata_round(x, 10^(-g$d)), hex(g$mr))
  # Up to 2.1.13 the unit was Stata's inline 10^(-d), one ulp above at
  # d = 2, so exact binary ties rounded down at every d >= 2.
  expect_true(all(vapply(2:10, function(d) any(hex(g$r)[g$d == d] != hex(g$mr)[g$d == d]), TRUE)))
  expect_identical(hex(g$unit)[g$d == 2][1], 0x1.47ae147ae147cp-7)
  # In a table: IRR-sized rate 1.0625 at digits 3 is 1.063 (2.1.13: 1.062).
  b <- rv_block(cats = "A", D = 4, Y = 10, rate = 1.0625, lo = 1, hi = 2)
  expect_identical(stratetab(b, ratescale = 1, digits = 3)$body$c4[2], "1.063 (1.000, 2.000)")
  # regtab's knife edge: 422.395 is 422.40 in both (golden S08, cell I9).
  b <- rv_block(cats = "A", D = 4, Y = 10, rate = 0.1, lo = 0.05, hi = 0.422395)
  expect_identical(stratetab(b)$body$c4[2], "100.0 (50.0, 422.4)")
  expect_identical(stratetab(b, digits = 2, ratescale = 1000)$body$c4[2], "100.00 (50.00, 422.40)")
})

test_that("F9: events and person-years both round half upward (Stata 2.5.2)", {
  b <- rv_block(cats = c("A", "B"), D = c(2.5, 3.5), Y = c(2.5, 0.5))
  tt <- stratetab(b)
  expect_identical(tt$body$c2[-1], c("3", "4"))
  expect_identical(tt$body$c3[-1], c("3", "1"))
})

test_that("F9: footnote font floor, zebra with one category, level x 100, upper-only label, tagged missing (M11, M13, M17, M20, M27)", {
  rules_at <- function(tt, op, r) {
    R <- tabtools:::.xlsx_layout_stratetab(tt)$rules
    R[R$op == tabtools:::.OP[[op]] & R$r1 <= r & R$r2 >= r, , drop = FALSE]
  }
  b <- rv_block(cats = "A", D = 1, Y = 10)
  # fontsize 7: the footnote is max(7 - 2, 6) = 6 (stratetab.ado:915).
  tt <- stratetab(b, fontsize = 7, footnote = "F")
  expect_identical(utils::tail(rules_at(tt, "font", 6L)$value, 1L), 6)
  # One exposure, one category: rows 4-5, so zebra shades row 5 (:882).
  tz <- stratetab(b, zebra = TRUE)
  expect_identical(nrow(tz$body), 2L)
  expect_identical(nrow(rules_at(tz, "fill", 5L)), 1L)
  # A level whose x 100 is not exact in binary.
  set.seed(7)
  d <- data.frame(t = rexp(100, 0.1), e = rbinom(100, 1, 0.3), g = rep(c("u", "v"), 50))
  t57 <- stratetab(tt_rates(d, "t", "e", by = "g", level = 0.57))
  expect_identical(t57$header[[2]]$text[4], "Per 1,000 PY (57% CI)")
  # Only the upper interval label names the level.
  up <- rv_block(level = NA)
  attr(up[["_Upper"]], "label") <- "Upper 90% confidence limit"
  expect_identical(stratetab(up)$stored$ci_level, 90)
  # A labelled extended missing code (Stata's .a) shows its label.
  skip_if_not_installed("haven")
  tg <- rv_block(cats = haven::labelled(c(1, haven::tagged_na("a")), c(One = 1, Refused = haven::tagged_na("a"))))
  expect_identical(stratetab(tg)$body$c1[-1], c("   One", "   Refused"))
})

test_that("F9 / deviation 6: rateratio with only empty comparison blocks returns the table without ratios (as Stata 2.1.14)", {
  b <- rv_block()
  tt <- stratetab(list(b, b[0, ]), outcomes = 1, rateratio = TRUE, level = 95)
  expect_null(tt$stored$ratios)
  expect_identical(tt$body$c1, c("Exposure 1", "   A", "   B", "Exposure 2"))
  expect_identical(tt$body$c5, c("", "Ref.", "Ref.", ""))
})

test_that("F10: strtoname keeps non-ASCII characters, as Stata's does", {
  expect_identical(tabtools:::.stata_strtoname(c("Ålder ≥65", "a€b", "0≥", "a.b", "a~b", "<1 yr")),
                   c("Ålder_≥65", "a€b", "_0≥", "a_b", "a_b", "_1_yr"))
  tt <- stratetab(rv_block(cats = c("Ålder ≥65", "B")))
  expect_identical(rownames(tt$stored$rates)[1], "Ålder_≥65")
})

test_that("F11: a tt_rates() result edited to hold impossible values is refused", {
  set.seed(8)
  d <- data.frame(t = rexp(50, 0.1), e = rbinom(50, 1, 0.3), g = rep(c("u", "v"), 25))
  r <- tt_rates(d, "t", "e", by = "g")
  bad <- r
  bad$D[1] <- -1
  expect_error(stratetab(bad), "Block 1: column D must be non-negative")
  bad <- r
  bad$Rate[2] <- Inf
  expect_error(stratetab(bad), "Block 1: column Rate must be finite")
})

test_that("F12: a block's level is checked before its categories, as in Stata", {
  a <- rv_block(cats = c("A", "B"), level = "95")
  b <- rv_block(cats = "A", D = 1, Y = 10, level = "90")
  # Both mixed levels and a category count mismatch: Stata reports the level.
  expect_error(stratetab(list(a, b), outcomes = 2), "mixed confidence levels")
  expect_error(stratetab(list(a, b), outcomes = 2, level = 95), "conflicts with block 2's 90%")
  # Within one block too: the level (:345-356) before its own labels (:398-412).
  dup <- rv_block(cats = c("A", "A"), level = "90")
  expect_error(stratetab(dup, level = 95), "conflicts with block 1's 90%")
  expect_error(stratetab(list(a, dup), outcomes = 2), "mixed confidence levels")
})

test_that("F14: an empty list of blocks is refused by name", {
  expect_error(stratetab(list()), "holds no blocks")
  expect_error(stratetab(list(), outcomes = 1), "holds no blocks")
})
