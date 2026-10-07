# survival::clogit as Stata's `clogit` (task 5.15; classified like logit
# since tabtools 2.1.14, task C8). The Stata goldens are R74-R76 and R83
# (test-golden-regtab.R); these are the refusals and the R-only paths.

cl_data <- function() {
  set.seed(515)
  d <- data.frame(set = rep(1:60, each = 3), x = rnorm(180), z = rbinom(180, 1, 0.4))
  d$case <- as.integer(ave(d$x + rnorm(180), d$set, FUN = function(v) v == max(v)))
  d
}

cl_fit <- function(d, ...) {
  local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    survival::clogit(case ~ x + z + survival::strata(set), data = d, ...)
  })
}

test_that("5.15: clogit is exponentiated, has no strata row, and counts observations", {
  skip_if_not_installed("survival")
  d <- cl_data()
  f <- cl_fit(d)
  tt <- regtab(f, stats = c("n", "events", "groups", "ll"))
  expect_identical(tt$header[[2]]$text[2], "OR")
  expect_identical(tt$body[[1]], c("x", "z", "Observations", "Log-likelihood"))
  expect_equal(unname(tt$stored$table["x", 1]), unname(exp(stats::coef(f)["x"])))
  expect_identical(tt$body[[2]][3], "180")
  expect_match(tt$stored$methods, "conditional logistic regression")
})

test_that("C8: clogit is classified like logit (null 1, automatic nointercept beside a logistic)", {
  skip_if_not_installed("survival")
  d <- cl_data()
  f <- cl_fit(d)
  info <- tabtools:::tt_model_info(f)
  expect_identical(info[c("effect_scale", "null_value", "auto_nointercept")],
                   list(effect_scale = "OR", null_value = 1, auto_nointercept = TRUE))
  lg <- glm(case ~ x + z, binomial, d)
  expect_false("Intercept" %in% regtab(lg, f)$body[[1]])
  expect_true("Intercept" %in% regtab(lg, f, nointercept = FALSE)$body[[1]])
  # The data-frame escape hatch maps the Stata word the same way.
  expect_identical(tabtools:::.mi_cmd_scale("clogit"), list(scale = "OR", noint = TRUE))
})

test_that("5.15: approximate methods (the only weighted clogit) and robust variances are refused", {
  skip_if_not_installed("survival")
  d <- cl_data()
  expect_error(regtab(cl_fit(d, method = "approximate")), "method = \"exact\"")
  expect_error(regtab(cl_fit(d, method = "efron")), "method = \"exact\"")
  expect_error(regtab(cl_fit(d), vce = "robust"), "not available")
})

test_that("5.15: a clogit fit works with survival not attached (its formula calls Surv() bare)", {
  skip_on_cran()
  skip_if_not_installed("survival")
  d <- cl_data()
  want <- regtab(cl_fit(d), stats = c("n", "ll"))$body
  # clogit() needs survival attached to fit; the table is made after it is
  # detached, as for a fit read from a file in a fresh session.
  env <- new.env(parent = globalenv())
  env$d <- d
  f <- withr::with_package("survival", eval(quote(clogit(case ~ x + z + strata(set), data = d)), env))
  expect_false("package:survival" %in% search())
  expect_identical(regtab(f, stats = c("n", "ll"))$body, want)
})

test_that("stack review 2: a clogit fit is anchored; edited data are refused as for coxph", {
  skip_if_not_installed("survival")
  d <- cl_data()
  d$g <- factor(rep(c("a", "b", "c"), 60))
  f <- local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    survival::clogit(case ~ g + x + survival::strata(set), data = d)
  })
  tt <- regtab(f)
  expect_identical(tt$rows$key[1:4], c("g", "1.g", "2.g", "3.g"))
  expect_identical(tt$body[[2]][2], "Reference")
  d <- d[d$set > 10, ]
  expect_error(regtab(f), "have changed since")
})

test_that("stack review: clogit accepts a user-supplied variance", {
  skip_if_not_installed("survival")
  f <- cl_fit(cl_data())
  expect_identical(regtab(f, vce = stats::vcov(f))$body, regtab(f)$body)
  expect_identical(regtab(f, vce = 4 * stats::vcov(f))$footnote, "Standard errors: user-supplied.")
})

test_that("review 2026-10-07 A1: clogit fit counts authenticate the supplied fit", {
  skip_if_not_installed("survival")
  d <- datasets::infert
  d$sp <- factor(d$spontaneous)
  f <- local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    survival::clogit(case ~ sp + induced + survival::strata(stratum), data = d)
  })
  record <- tt_fitcount(f, events = "case", people = "stratum", data = d, terms = TRUE)
  tt <- regtab(f, fitcounts = record, stats = c("n", "people"), statlabels = c(people = "Groups"))
  last <- nrow(tt$body)
  expect_identical(tt$body[[1]][(last - 1L):last], c("Observations", "Groups"))
  expect_identical(tt$body[[2]][(last - 1L):last], c("248", "83"))
  # 2.sp has 24 events: masked at mincount 30; 1.sp (31) is kept.
  tm <- regtab(f, fitcounts = record, mincount = 30)
  r <- tm$meta$regtab_rows
  expect_identical(r$status[match(c("0.sp", "1.sp", "2.sp"), r$key)], c("ref", "est", "masked"))
  # The shimmed formula environment is regtab's own; a different fit still fails.
  g <- local({
    coxph <- survival::coxph
    Surv <- survival::Surv
    survival::clogit(case ~ sp + survival::strata(stratum), data = d)
  })
  expect_error(regtab(g, fitcounts = record), class = "tabtools_error_fitcount")
})
