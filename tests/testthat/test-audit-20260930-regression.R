# Regression audit, 2026-09-30: every defect below was reproduced on the
# original code before the fix.

test_that("univariable formulas treat column names as symbols", {
  d <- mtcars
  d[["engine weight"]] <- d$wt
  d[["fuel economy"]] <- d$mpg
  d[["wt + hp"]] <- d$wt
  before <- d

  uv <- regtab_uv(d, "fuel economy", c("engine weight", "wt + hp"), method = lm)
  for (i in seq_along(uv$fits)) {
    expect_equal(unname(stats::coef(uv$fits[[i]])),
                 unname(stats::coef(lm(mpg ~ wt, d))), tolerance = 1e-12)
    expect_identical(all.vars(stats::formula(uv$fits[[i]])),
                     c("fuel economy", uv$x[i]))
  }
  tt <- regtab(uv)
  expect_identical(nrow(tt$body), 2L)
  expect_identical(tt$body[[2]], rep(regtab(lm(mpg ~ wt, d))$body[[2]][1], 2L))
  expect_identical(d, before)

  wrapped <- regtab_uv(d, "fuel economy", "engine weight", method = lm,
                       formula = "{y} ~ log({x})")
  expect_equal(unname(stats::coef(wrapped$fits[[1]])),
               unname(stats::coef(lm(mpg ~ log(wt), d))), tolerance = 1e-12)
  expect_identical(nrow(regtab(wrapped)$body), 1L)

  # Inserted column names must not become further template tokens.
  d[["{x}"]] <- d$mpg
  literal <- regtab_uv(d, "{x}", "wt", method = lm)
  expect_identical(all.vars(stats::formula(literal$fits[[1]])), c("{x}", "wt"))
  expect_equal(unname(stats::coef(literal$fits[[1]])),
               unname(stats::coef(lm(mpg ~ wt, d))), tolerance = 1e-12)
  d[["a#b"]] <- d$wt
  expect_identical(nrow(regtab(regtab_uv(d, "mpg", "a#b", method = lm))$body), 1L)
})

test_that("escaped numeric predictor names retain their coefficient role", {
  d <- mtcars
  for (nm in c("a\\b", "a`b")) {
    d[[nm]] <- d$wt
    f <- stats::as.formula(paste("mpg ~", deparse(as.name(nm), backtick = TRUE)))
    fit <- stats::lm(f, d)
    expect_equal(unname(stats::coef(fit)),
                 unname(stats::coef(lm(mpg ~ wt, d))), tolerance = 1e-12)
    expect_identical(nrow(regtab(fit)$body), 2L)
    expect_identical(nrow(regtab(regtab_uv(d, "mpg", nm, method = lm))$body), 1L)
  }
})

test_that("user covariance matrices refuse ambiguous coefficient names", {
  fit <- lm(mpg ~ wt, mtcars)
  V <- diag(c(1, 2, 50))
  dimnames(V) <- rep(list(c("(Intercept)", "wt", "wt")), 2L)
  expect_error(regtab(fit, vce = V), "names.*unique", class = "rlang_error")

  # A unique permutation still maps every covariance entry by its name.
  valid <- stats::vcov(fit)
  expect_identical(regtab(fit, vce = valid[2:1, 2:1])$body,
                   regtab(fit, vce = valid)$body)
})

test_that("custom statistic labels cannot silently reuse the first function", {
  fit <- lm(mpg ~ wt, mtcars)
  expect_error(regtab(fit, stat_fun = list(Custom = function(fit) 1,
                                          Custom = function(fit) 2)),
               "labels.*unique", class = "rlang_error")
  tt <- regtab(fit, stat_fun = list(First = function(fit) 1,
                                  Second = function(fit) 2))
  expect_identical(tail(tt$body[[1]], 2L), c("First", "Second"))
  expect_identical(tail(tt$body[[2]], 2L), c("1.000", "2.000"))
})

test_that("data-frame glance statistics cannot become factor codes or first rows", {
  x <- data.frame(term = "x", estimate = 1, std.error = 0.3)
  attr(x, "effect_scale") <- "Coef."
  attr(x, "inference_reference") <- "normal"
  attr(x, "glance") <- list(nobs = factor("200"))
  expect_error(regtab(x, stats = "n"), "nobs.*one number", class = "rlang_error")
  attr(x, "glance") <- data.frame(nobs = c(20, 200))
  expect_error(regtab(x, stats = "n"), "exactly one row", class = "rlang_error")
  attr(x, "glance") <- list(nobs = 20, nobs = 200)
  expect_error(regtab(x, stats = "n"), "names.*unique", class = "rlang_error")
  attr(x, "glance") <- list(nobs = c(20, 200))
  expect_error(regtab(x, stats = "n"), "nobs.*one number", class = "rlang_error")
  attr(x, "glance") <- list(nobs = 200 + 50i)
  expect_error(regtab(x, stats = "n"), "nobs.*one number", class = "rlang_error")
  attr(x, "glance") <- list(nobs = 200, logLik = NULL, df = NA)
  expect_identical(tail(regtab(x, stats = c("n", "ll"))$body[[2]], 1L), "200")
})

test_that("modelsummary glance statistics obey the same scalar contract", {
  x <- structure(list(tidy = data.frame(term = "x", estimate = 1, std.error = 0.3),
                      glance = data.frame(nobs = factor("200"))),
                 class = "modelsummary_list")
  expect_error(tt_from_modelsummary(x, exponentiate = FALSE),
               "nobs.*one number", class = "rlang_error")
  x$glance <- data.frame(nobs = c(20, 200))
  expect_error(tt_from_modelsummary(x, exponentiate = FALSE),
               "exactly one row", class = "rlang_error")
  x$glance <- data.frame(nobs = 200)
  out <- tt_from_modelsummary(x, exponentiate = FALSE, inference_reference = "normal")
  expect_identical(tail(regtab(out, stats = "n")$body[[2]], 1L), "200")
})
