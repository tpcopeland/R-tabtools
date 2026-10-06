# B5 verification sweep (bugfix plan 2026-10-06; CAT = tabtools_bugs_and_solutions.md).

# CAT P0-6 residual (decision D5) ----

test_that("D5: a ratio frame's derived Wald interval that reaches 0 warns once, naming each term", {
  x <- data.frame(term = c("a", "b", "c"), estimate = c(1.05, 1.5, 1.2), std.error = c(0.6, 0.1, 0.7),
                  null.value = 1)
  w <- testthat::capture_warnings(t <- effecttab(x, effect = "RR"))
  w <- w[grepl("reaches 0 or below", w)]
  expect_length(w, 1L)
  expect_match(w, "a", fixed = TRUE)
  expect_match(w, "c", fixed = TRUE)
  expect_false(grepl("\"b\"", w, fixed = TRUE))
  expect_warning(effecttab(x, effect = "RR"), class = "tabtools_warning_ratio_interval")
  # The linear (delta-method) interval is kept, as Stata's nlcom/margins give it.
  expect_true(any(grepl("-0.13", unlist(as.data.frame(t)), fixed = TRUE)))
  # A positive lower bound, or a supplied interval, does not warn.
  ok <- data.frame(term = "x", estimate = 1.5, std.error = 0.1, null.value = 1)
  expect_no_warning(effecttab(ok, effect = "RR"))
  sup <- data.frame(term = "x", estimate = 1.05, conf.low = 0.5, conf.high = 2.2,
                    std.error = 0.6, null.value = 1)
  expect_no_warning(effecttab(sup, effect = "RR"))
  # A difference (null 0) crossing 0 is ordinary.
  dif <- data.frame(term = "x", estimate = 0.05, std.error = 0.6)
  expect_no_warning(effecttab(dif))
})

test_that("D5: a ratio header without null.value (p.value supplied) warns too, once", {
  x <- data.frame(term = "a", estimate = 1.05, std.error = 0.6, p.value = 0.3)
  w <- testthat::capture_warnings(effecttab(x, effect = "RR"))
  w <- w[grepl("reaches 0 or below", w)]
  expect_length(w, 1L)
  expect_match(w, "a", fixed = TRUE)
  expect_warning(effecttab(x, effect = "OR"), class = "tabtools_warning_ratio_interval")
  expect_no_warning(effecttab(x, effect = "Difference"))
  # Without a term column the row number is named.
  y <- data.frame(label = "lab", estimate = 1.05, std.error = 0.6, p.value = 0.3)
  expect_warning(effecttab(y, effect = "RR"), "row 1", class = "tabtools_warning_ratio_interval")
})

test_that("D5: a ratio header with p-values derived against a null of 0 is still refused", {
  expect_error(effecttab(data.frame(term = "x", estimate = 1.05, std.error = 0.05), effect = "RR"),
               "null of 0")
})

# CAT P1-8 ----

test_that("P1-8: glmmTMB cs()/ar1() terms are refused with a classed message, never blank", {
  skip_if_not_installed("glmmTMB")
  set.seed(1)
  d <- data.frame(g = factor(rep(1:30, each = 4)), tf = factor(rep(1:4, 30)), x = rnorm(120))
  d$y <- rnorm(120) + rep(rnorm(30), each = 4) + 0.3 * d$x
  f <- suppressWarnings(glmmTMB::glmmTMB(y ~ x + cs(tf + 0 | g), data = d))
  expect_error(regtab(f), class = "tabtools_error_tmb_covstruct")
  expect_error(regtab(f), "covariance structure")
})

test_that("P1-8: an unmatched unstructured glmmTMB term says why its covariance interval is blank", {
  skip_if_not_installed("glmmTMB")
  set.seed(2)
  d <- data.frame(g = factor(rep(1:40, each = 5)), x = rnorm(200))
  d$y <- rnorm(200) + rep(rnorm(40), each = 5) + rep(rnorm(40), each = 5) * d$x
  f <- suppressWarnings(glmmTMB::glmmTMB(y ~ x + (1 + x | g), data = d))
  skip_if(is.null(.rt_re_uncertainty_tmb(f)), "no uncertainty for this fit")
  cov_rows <- function(t) {
    df <- as.data.frame(t)
    df[grepl("cov|Covariance", df[[1]]), , drop = FALSE]
  }
  # Normal path: no note, a covariance interval.
  expect_no_message(t0 <- regtab(f), message = "covariance interval")
  expect_false(grepl("Covariance intervals", paste(t0$footnote, collapse = " ")))
  ok_ci <- cov_rows(t0)
  testthat::local_mocked_bindings(.rt_tmb_cor = function(t, k) diag(k))
  m <- testthat::capture_messages(t1 <- regtab(f))
  expect_true(any(grepl("covariance interval(s) of the random-effect term", m, fixed = TRUE)))
  expect_match(t1$footnote, "Covariance intervals of the random-effect term", fixed = TRUE)
  expect_match(t1$footnote, "Model 1:", fixed = TRUE)
  bl <- cov_rows(t1)
  expect_gt(nrow(bl), 0L)
  expect_false(identical(bl, ok_ci))
  expect_error(regtab(f, xlsx = tempfile(fileext = ".xlsx")), NA)
})

# CAT P1-6 ----

test_that("P1-6: an indefinite observed information is refused (never a NaN SE)", {
  set.seed(3)
  d <- data.frame(x = rnorm(40))
  d$y <- exp(1 + d$x) + rnorm(40)
  f <- glm(y ~ x, family = gaussian(link = "log"), data = d)
  f$coefficients[] <- c(-3, 0)
  expect_error(regtab(f), "not positive definite")
  expect_false(.rt_is_pd(matrix(c(1, 2, 2, 1), 2)))
})

# CAT P1-14 ----

test_that("P1-14: a level estimable only in imputations 2..M is refused with a message", {
  set.seed(4)
  mk <- function(rare) {
    d <- data.frame(y = rnorm(60), x = rnorm(60),
                    g = factor(sample(c("a", "b", if (rare) "c"), 60, TRUE, prob = if (rare) c(.45, .45, .1) else c(.5, .5))))
    lm(y ~ x + g, data = d)
  }
  fits <- list(mk(FALSE), mk(TRUE), mk(TRUE))
  expect_error(regtab(tt_mi(fits)), "different coefficients")
})
