# Regression tests for the Phase 6 review findings that are not about the
# converters themselves (those are in test-interop-*.R). IDs refer to the
# review's report (see IMPLEMENTATION_PLAN.md, "Phase 6 review outcome").

test_that("regtab() names a mistyped option instead of calling it a model (P2-9)", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg, a)
  expect_error(regtab(f, excel = "t.xlsx"), "no argument `excel`")
  expect_error(regtab(f, excel = "t.xlsx"), "Use `xlsx`")
  expect_error(regtab(f, exel = TRUE), "no argument `exel`")
  expect_error(regtab(f, frame = NULL), "no argument `frame`")
  # Named models still name their columns.
  tt <- regtab(A = f, B = lm(price ~ mpg + weight, a))
  expect_identical(tt$header[[1]]$text[c(2, 5)], c("A", "B"))
})

# P0-3 (flipped at the merge with the 2.1.11 goldens): survreg fits show time
# ratios, exp(b) with CI exp(b +/- z se) and the p-value of b, as Stata 2.1.11
# shows streg, time; coef = "AF" relabels the header only; a kept intercept is
# exp(_cons) while the ancillary row keeps its own scale (Stata 2.1.11
# exponentiates lognormal/loglogistic ancillary rows; R does not, take_action
# item 14). The vignette's "Models (regtab)" bullets describe this.
test_that("survreg: time ratios under TR; ancillary rows on their own scale", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  fit <- survival::survreg(survival::Surv(time, status) ~ sex, data = lung)
  tt <- regtab(fit)
  row <- trimws(tt$body[[1]]) == "sex"
  b <- unname(stats::coef(fit)["sex"])
  se <- sqrt(stats::vcov(fit)["sex", "sex"])
  expect_identical(tt$header[[2]]$text[2], "TR")
  expect_identical(tt$body[[2]][row], sprintf("%.2f", exp(b)))
  expect_identical(tt$body[[3]][row], sprintf("(%.2f, %.2f)", exp(b - stats::qnorm(0.975) * se),
                                              exp(b + stats::qnorm(0.975) * se)))
  # The 2.1.11 TR sentence, from the model since H11: "univariable" for
  # this one-predictor fit, Stata's exact sentence with two predictors.
  expect_equal(tt$stored$methods,
               "Time ratios with 95% confidence intervals from univariable accelerated failure-time survival regression.")
  fit2 <- survival::survreg(survival::Surv(time, status) ~ sex + age, data = lung)
  expect_equal(regtab(fit2)$stored$methods,
               "Time ratios with 95% confidence intervals from multivariable accelerated failure-time survival regression.")
  af <- regtab(fit, coef = "AF")
  expect_identical(af$header[[2]]$text[2], "AF")
  expect_identical(af$body, tt$body)
  ln <- survival::survreg(survival::Surv(time, status) ~ sex, data = lung, dist = "lognormal")
  kept <- regtab(ln, keepintercept = TRUE)
  lab <- trimws(kept$body[[1]])
  est <- kept$body[[2]]
  expect_identical(est[grepl("Intercept|_cons", lab)][1], sprintf("%.2f", exp(unname(stats::coef(ln)[1]))))
  anc <- grepl("sigma", lab)
  expect_true(any(anc))
  expect_identical(est[anc][1], sprintf("%.2f", log(ln$scale)))
})

test_that("a NULL title or footnote is no title or footnote (P3-6)", {
  tab <- table1_tc(golden_fixture("auto"), by = "foreign", vars = "price contn", title = "T")
  tab$title <- NULL
  tab$footnote <- NULL
  xl <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tt_write_xlsx(tab, xl))
  expect_no_error(tt_write_markdown(tab, withr::local_tempfile(fileext = ".md")))
  expect_no_error(tt_write_csv(tab, withr::local_tempfile(fileext = ".csv")))
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  expect_null(flextable::as_flextable(tab)$caption$value)
  expect_s3_class(tt_as_gt(tab), "gt_tbl")
})

test_that("desctab() offers table1_tc()'s leading arguments (P3-9)", {
  expect_identical(names(formals(desctab))[1:3], c("data", "vars", "by"))
  d <- golden_fixture("auto")
  expect_identical(desctab(d, "price", "foreign"), table1_tc(d, "price", "foreign"))
  expect_identical(desctab(d, by = "foreign"), table1_tc(d, by = "foreign"))
  expect_error(desctab(vars = "price"), "data")
})
