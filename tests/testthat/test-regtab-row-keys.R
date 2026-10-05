# Row identity in tt_table$rows (task 6.7): regtab's key, var and level,
# for converters and composites that align rows without parsing labels.

rk <- function(tt) tt$rows[c("type", "key", "var", "level")]

test_that("6.7: factor, level, intercept and statistic rows carry their keys", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  tt <- regtab(lm(mpg ~ wt + cyl, d), stats = c("n", "r2"), addrow = list(FE = "Yes"),
               stat_fun = list(Sigma = function(f) stats::sigma(f)))
  r <- rk(tt)
  expect_identical(r$key, c("wt", "cyl", "4.cyl", "6.cyl", "8.cyl", "_cons", "stat:n", "stat:r2", "statfun:Sigma", "addrow:FE"))
  expect_identical(r$var, c("wt", "cyl", "cyl", "cyl", "cyl", "_cons", NA, NA, NA, NA))
  expect_identical(r$level, c(NA, NA, "4", "6", "8", NA, NA, NA, NA, NA))
  expect_identical(nrow(tt$rows), nrow(tt$body))
})

test_that("6.7: interaction cells, fvgen and native, key the variable and level codes", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am)
  f <- lm(mpg ~ wt + cyl * am, d)
  fv <- rk(regtab(f))
  expect_identical(fv$key[fv$type == "level"], c("6.cyl", "8.cyl", "1.am", "6.cyl#1.am", "8.cyl#1.am"))
  expect_identical(fv$var[fv$type == "level"], c("cyl", "cyl", "am", "cyl#am", "cyl#am"))
  expect_identical(fv$level[fv$type == "level"], c("6", "8", "1", "6#1", "8#1"))
  nat <- rk(regtab(f, interactions = "native"))
  expect_identical(nat$level[nat$key == "4.cyl#1.am"], "4#1")
  expect_identical(nat$var[nat$key == "cyl#am"], "cyl#am")
  # A continuous factor of an interaction is Stata's c. term.
  ci <- rk(suppressMessages(regtab(lm(Sepal.Length ~ Species:Petal.Width, iris), interactions = "native")))
  expect_true("1#c" %in% ci$level)
})

test_that("6.7: a dotted variable name, a matrix term, equations and ancillary rows", {
  r <- rk(regtab(lm(Sepal.Length ~ Sepal.Width + poly(Petal.Width, 2), iris)))
  expect_identical(r$var[1:3], c("Sepal.Width", "poly(Petal.Width, 2)1", "poly(Petal.Width, 2)2"))
  expect_true(all(is.na(r$level)))
  skip_if_not_installed("nnet")
  m <- rk(regtab(nnet::multinom(factor(gear) ~ wt, mtcars, trace = FALSE)))
  expect_identical(m$key, c("4::wt", "5::wt"))
  expect_identical(m$var, c("wt", "wt"))
  skip_if_not_installed("MASS")
  nb <- rk(regtab(suppressWarnings(MASS::glm.nb(carb ~ wt, mtcars)), keepintercept = TRUE))
  expect_identical(nb$key, c("wt", "/::lnalpha", "/::alpha", "_cons"))
  expect_identical(nb$var, c("wt", "lnalpha", "alpha", "_cons"))
})

test_that("6.7: keys survive keep/drop and several models; other commands get an NA key column", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  tt <- regtab(lm(mpg ~ wt + cyl, d), lm(mpg ~ cyl + hp, d), drop = "wt")
  expect_identical(tt$rows$key, c("cyl", "4.cyl", "6.cyl", "8.cyl", "hp", "_cons"))
  p <- puttab(data.frame(a = 1:2, b = 3:4))
  expect_true(all(is.na(p$rows$key)))
  bad <- p
  bad$rows$key <- 1:2
  expect_error(validate_tt_table(bad), "rows\\$key")
})
