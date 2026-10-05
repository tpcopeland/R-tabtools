# User-supplied variances (task 5.18, R only): a function or a matrix as a
# model's vce, with vce_df for the reference distribution.

uv_fit <- function() lm(mpg ~ wt + hp, mtcars)

test_that("5.18: a vce function sets every SE, interval and p-value; t(df.residual) for lm", {
  f <- uv_fit()
  V <- 2 * stats::vcov(f)
  tt <- regtab(f, vce = function(x) 2 * stats::vcov(x), stats = c("n", "F"))
  se <- sqrt(diag(V))
  q <- stats::qt(0.975, f$df.residual)
  expect_equal(unname(tt$stored$table["wt", 1]), unname(stats::coef(f)["wt"]))
  want <- sprintf("(%.2f, %.2f)", stats::coef(f)["wt"] - q * se["wt"], stats::coef(f)["wt"] + q * se["wt"])
  expect_identical(tt$body[[3]][1], want)
  # F: the Wald statistic under V, (b' V^-1 b) / q.
  b <- stats::coef(f)[c("wt", "hp")]
  Fw <- drop(crossprod(b, solve(V[c("wt", "hp"), c("wt", "hp")], b))) / 2
  expect_identical(tt$body[[2]][tt$body[[1]] == "F statistic"], stata_fmt(Fw, "%9.2f"))
  expect_identical(tt$footnote, "Standard errors: user-supplied.")
  # A matrix gives the same table, named or unnamed, in any row order.
  expect_identical(regtab(f, vce = V, stats = c("n", "F"))$body, tt$body)
  expect_identical(regtab(f, vce = unname(V), stats = c("n", "F"))$body, tt$body)
  expect_identical(regtab(f, vce = V[3:1, 3:1], stats = c("n", "F"))$body, tt$body)
})

test_that("5.18: vce_df chooses the reference distribution; glm defaults to the normal", {
  g <- glm(am ~ wt, binomial, mtcars)
  V <- stats::vcov(g)
  z <- regtab(g, vce = V)$body
  expect_identical(z, regtab(g)$body)
  t10 <- regtab(g, vce = V, vce_df = 10)
  se <- sqrt(V["wt", "wt"])
  p <- 2 * stats::pt(-abs(stats::coef(g)["wt"] / se), 10)
  expect_equal(unname(t10$stored$table["wt", 1]), unname(exp(stats::coef(g)["wt"])))
  expect_identical(t10$body[[4]][1], format_p(p))
  f <- uv_fit()
  expect_identical(regtab(f, vce = stats::vcov(f), vce_df = "z")$body[[4]],
                   regtab(f, vce = stats::vcov(f), vce_df = Inf)$body[[4]])
  expect_identical(regtab(f, vce = stats::vcov(f), vce_df = "t")$body, regtab(f)$body)
})

test_that("5.18: per-model user variances, with Stata's beside them; the footnote names the model", {
  f <- uv_fit()
  f2 <- lm(mpg ~ wt, mtcars)
  tt <- regtab(f, f2, vce = list(stats::vcov(f), "robust"), models = c("A", "B"))
  expect_identical(tt$footnote, "Standard errors, A: user-supplied.")
  tv <- regtab(f, f2, vce = list(stats::vcov(f), "robust"), models = c("A", "B"), vce_note = TRUE)
  expect_identical(tv$footnote, "Standard errors, A: user-supplied; B: robust.")
  fn <- regtab(f, vce = stats::vcov(f), footnote = "Source: mtcars")$footnote
  expect_identical(fn, "Source: mtcars. Standard errors: user-supplied.")
})

test_that("5.18: invalid matrices, classes and combinations are refused", {
  f <- uv_fit()
  V <- stats::vcov(f)
  expect_error(regtab(f, vce = V[1:2, 1:2]), "no row for hp")
  expect_error(regtab(f, vce = unname(V[1:2, 1:2])), "2 x 2 for 3 coefficients")
  asym <- V
  asym[1, 2] <- asym[1, 2] + 1
  expect_error(regtab(f, vce = asym), "not symmetric")
  neg <- V
  neg[2, 2] <- -1
  expect_error(regtab(f, vce = neg), "negative variance")
  expect_error(regtab(f, vce = function(x) stop("boom")), "function failed")
  expect_error(regtab(f, vce = "not-a-type"), "user-supplied variance")
  expect_error(regtab(f, lm(mpg ~ wt, mtcars), vce = V), "belongs to one model")
  expect_error(regtab(f, vce = list(V), cluster = list(~cyl)), "a user-supplied variance; clusters apply only")
  expect_error(regtab(f, vce = V, ci_method = "profile"), "profile")
  expect_error(regtab(f, vce_df = "z"), "no model has a user-supplied")
  expect_error(regtab(f, vce = V, vce_df = -1), "positive number")
  skip_if_not_installed("MASS")
  nb <- suppressWarnings(MASS::glm.nb(carb ~ wt, mtcars))
  expect_error(regtab(nb, vce = function(x) stats::vcov(x)), "not available for <negbin>")
  expect_error(regtab(data.frame(term = "x", estimate = 1, std.error = 0.5), vce = function(x) 1), "data frame")
})

test_that("5.18: an aliased coefficient may be left out of the matrix", {
  d <- mtcars
  d$wt2 <- d$wt
  f <- lm(mpg ~ wt + wt2, d)
  V <- stats::vcov(f)
  expect_true(anyNA(stats::coef(f)))
  expect_identical(regtab(f, vce = V)$body, regtab(f)$body)
  expect_identical(regtab(f, vce = unname(V[!is.na(stats::coef(f)), !is.na(stats::coef(f))]))$body, regtab(f)$body)
})

test_that("5.18: survival::coxph and clogit accept a user variance", {
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, status) ~ age + sex, survival::lung, ties = "breslow")
  expect_identical(regtab(cx, vce = stats::vcov(cx))$body, regtab(cx)$body)
})

test_that("stack review: a Matrix vce, NA in a per-model vce_df, and a non-PSD matrix", {
  f <- uv_fit()
  V <- stats::vcov(f)
  skip_if_not_installed("Matrix")
  expect_identical(regtab(f, vce = Matrix::Matrix(V))$body, regtab(f, vce = V)$body)
  f2 <- lm(mpg ~ wt, mtcars)
  expect_no_error(regtab(f, f2, vce = list(V, "stata"), vce_df = c(10, NA)))
  bad <- V
  bad[2, 3] <- bad[3, 2] <- 100
  expect_error(regtab(f, vce = bad), "positive semidefinite")
})
