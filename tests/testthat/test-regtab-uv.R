# regtab_uv() (task 7.13): univariable fits stacked into one model column.
# Its cells are the separate fits' cells, which golden R77 (Stata's foreach
# loop of collect: logistic y x) pins against Stata.

test_that("7.13: the stacked column is golden R77's separate models, cell for cell", {
  d <- golden_fixture("cohort", factors = "education")
  uv <- regtab_uv(d, "cv_event", c("index_age", "female", "education"), method = glm,
                  method.args = list(family = binomial, control = glm.control(epsilon = 1e-14)))
  tt <- regtab(uv, coef = "OR", nointercept = TRUE, stats = "n")
  g <- utils::read.csv(golden_path("R77.csv"), header = FALSE, colClasses = "character", encoding = "UTF-8")
  gb <- g[-(1:2), ]
  expect_identical(trimws(tt$body[[1]]), trimws(gb[[1]]))
  # Each row's estimate, CI and p-value come from the one model that has it.
  pick <- function(i) {
    for (j in c(2L, 5L, 8L)) if (any(nzchar(unlist(gb[i, j:(j + 2L)])))) return(unlist(gb[i, j:(j + 2L)]))
    rep("", 3L)
  }
  want <- t(vapply(seq_len(nrow(gb) - 1L), pick, character(3)))
  got <- as.matrix(tt$body[seq_len(nrow(gb) - 1L), 2:4])
  dimnames(got) <- NULL
  expect_identical(got, unname(want))
  expect_identical(tt$body[[2]][nrow(gb)], "15,000")
  expect_match(tt$stored$methods, "^Odds ratios .* from univariable logistic regression\\.")
})

test_that("7.13: beside an adjusted model, keys and Reference rows line up", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  crude <- regtab_uv(d, "am", c("wt", "cyl"), method = glm, method.args = list(family = binomial))
  adj <- suppressWarnings(glm(am ~ wt + cyl, binomial, d))
  tt <- suppressWarnings(regtab(list(Crude = crude, Adjusted = adj)))
  expect_identical(tt$rows$key, c("wt", "cyl", "4.cyl", "6.cyl", "8.cyl"))
  expect_identical(tt$body[[2]][3], "Reference")
  expect_identical(tt$body[[5]][3], "Reference")
  expect_match(tt$stored$methods, "univariable and multivariable")
})

test_that("7.13: template covariates are left out; robust variance per fit; survival outcomes", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  uv <- regtab_uv(d, "mpg", c("wt", "hp"), method = "lm", formula = "{y} ~ {x} + cyl")
  tt <- regtab(uv, vce = "robust")
  expect_identical(tt$body[[1]], c("wt", "hp"))
  sep <- regtab(lm(mpg ~ wt + cyl, d), vce = "robust")$body
  expect_identical(unlist(tt$body[1, 2:4]), unlist(sep[1, 2:4]))
  expect_error(regtab(uv, cluster = ~cyl), "not available for <tt_uv>")
  skip_if_not_installed("survival")
  cx <- regtab_uv(survival::lung, "survival::Surv(time, status)", c("age", "sex"), method = survival::coxph,
                  method.args = list(ties = "breslow"))
  tc <- regtab(cx, stats = c("n", "events"))
  expect_identical(tc$header[[2]]$text[2], "HR")
  expect_identical(tc$body[[2]][3:4], c("228", "165"))
})

test_that("7.13: counts are shown only when every fit has the same one", {
  d <- mtcars
  d$hp[1:3] <- NA
  tt <- regtab(regtab_uv(d, "mpg", c("wt", "hp"), method = lm), stats = c("n", "r2"))
  expect_identical(tt$body[[1]], c("wt", "hp"))
})

test_that("7.13: bad input is refused", {
  expect_error(regtab_uv(1, "y", "x"), "data frame")
  expect_error(regtab_uv(mtcars, "mpg", "nope"), "not a column")
  expect_error(regtab_uv(mtcars, "mpg", c("wt", "wt")), "more than once")
  expect_error(regtab_uv(mtcars, "mpg", "wt", formula = "{y} ~ 1"), "must contain")
  expect_error(regtab_uv(mtcars, "mpg", "wt", method.args = list(data = mtcars)), "may not set")
  expect_error(regtab_uv(mtcars, "mpg", "wt", method = 3), "model function")
  expect_error(regtab_uv(mtcars, "nope", "wt", method = lm), "failed")
  expect_error(regtab_uv(mtcars, "mpg", "wt", method = MASS::rlm), "cannot be tabled")
})

test_that("stack review 3: interaction, factor and wrapped templates keep the covariate's rows", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  expect_identical(regtab(regtab_uv(d, "mpg", c("wt", "hp"), method = lm, formula = "{y} ~ {x} * am"))$body[[1]],
                   c("wt", "wt × am", "hp", "hp × am"))
  expect_identical(regtab(regtab_uv(d, "mpg", "cyl", method = lm, formula = "{y} ~ {x} * am"))$rows$key,
                   c("6.cyl", "8.cyl", "6.cyl#c.am", "8.cyl#c.am"))
  expect_identical(regtab(regtab_uv(d, "mpg", c("wt", "hp"), method = lm, formula = "{y} ~ log({x})"))$body[[1]],
                   c("log(wt)", "log(hp)"))
  expect_identical(regtab(regtab_uv(mtcars, "mpg", "gear", method = lm, formula = "{y} ~ factor({x})"))$rows$key,
                   c("factor(gear)", "3.factor(gear)", "4.factor(gear)", "5.factor(gear)"))
})

test_that("stack review 7: each fit of a stack gets the weights warning", {
  d <- mtcars
  set.seed(1)
  d$w <- runif(32, 0.5, 2)
  uv <- suppressWarnings(regtab_uv(d, "am", "wt", method = glm, method.args = list(family = binomial, weights = quote(w))))
  expect_warning(suppressMessages(regtab(uv)), "inverse-probability weights")
})
