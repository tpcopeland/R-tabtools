# Post-release backlog items (IMPLEMENTATION_PLAN.md, "Backlog").

test_that("backlog: a coxph factor with contrasts() set gives no 'contrasts dropped' warning", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$ph <- factor(d$ph.ecog)
  stats::contrasts(d$ph) <- stats::contr.treatment(levels(d$ph), base = 2)
  f <- survival::coxph(survival::Surv(time, status) ~ age + ph, d, ties = "breslow")
  expect_no_warning(tt <- regtab(f))
  expect_identical(tt$body[[2]][tt$rows$key == "1.ph"], "Reference")
})

test_that("backlog: a log-binomial glm is Coef. (log risk ratios), as Stata's glm without eform", {
  # This boundary fit needs more than glm's default 25 iterations. The
  # layout contract must use a fit that has actually converged.
  f <- suppressWarnings(glm(am ~ wt, binomial(link = "log"), mtcars, start = c(-0.5, -0.1),
                            control = glm.control(maxit = 500)))
  expect_identical(f$converged, TRUE)
  tt <- regtab(f)
  expect_identical(tt$header[[2]]$text[2], "Coef.")
  expect_match(tt$stored$methods, "log-binomial regression")
})

test_that("backlog: keep/drop of a variable the formula wraps in factor() names the key to use", {
  f <- lm(mpg ~ wt + factor(cyl), mtcars)
  expect_error(regtab(f, keep = "cyl"), "is \"factor\\(cyl\\)\" in the model")
  expect_warning(tt <- regtab(f, drop = "cyl"), "matched no row")
  expect_identical(nrow(tt$body), 6L)
  expect_no_warning(tt2 <- regtab(f, drop = "factor(cyl)"))
  expect_identical(tt2$body[[1]], c("wt", "Intercept"))
})

test_that("backlog: a row's label comes from any model that has one, not only the first", {
  d1 <- mtcars
  d2 <- mtcars
  attr(d2$wt, "label") <- "Weight (1000 lbs)"
  d2$cyl <- factor(d2$cyl)
  attr(d2$cyl, "label") <- "Cylinders"
  d1$cyl <- factor(d1$cyl)
  tt <- regtab(lm(mpg ~ wt + cyl, d1), lm(mpg ~ wt + cyl, d2))
  expect_identical(tt$body[[1]][1:2], c("Weight (1000 lbs)", "Cylinders"))
  # The first model's own label still wins over a later one.
  d3 <- d2
  attr(d3$wt, "label") <- "Other"
  expect_identical(regtab(lm(mpg ~ wt, d2), lm(mpg ~ wt, d3))$body[[1]][1], "Weight (1000 lbs)")
})

test_that("stack review: several wrapped terms share one hint", {
  f <- lm(mpg ~ factor(cyl) + factor(gear), mtcars)
  msg <- tryCatch(regtab(f, keep = c("cyl", "gear")), error = conditionMessage)
  expect_match(msg, "\"cyl\" is \"factor(cyl)\", \"gear\" is \"factor(gear)\"", fixed = TRUE)
})
