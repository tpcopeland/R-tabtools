# tt_from_modelsummary() (task 7.15): a modelsummary_list through regtab's
# data-frame input. modelsummary is not in Suggests: the real-package test
# reaches it by a name held in a variable (as test-model-zoo.R does).

ms_fake <- function(fit, exponentiated = FALSE) {
  b <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  td <- data.frame(term = names(b), estimate = unname(b), std.error = unname(se),
                   statistic = unname(b / se), p.value = unname(2 * stats::pnorm(-abs(b / se))),
                   conf.low = NA_real_, conf.high = NA_real_, group = "", stringsAsFactors = FALSE)
  if (exponentiated) td$estimate <- exp(td$estimate)
  attr(td, "exponentiate") <- exponentiated
  structure(list(tidy = td, glance = data.frame(aic = stats::AIC(fit), nobs = stats::nobs(fit),
                                                logLik = as.numeric(stats::logLik(fit)))),
            class = "modelsummary_list")
}

test_that("7.15: exponentiate = TRUE shows exp() with Wald intervals from the link-scale SEs", {
  f <- glm(am ~ wt + hp, binomial, mtcars)
  x <- tt_from_modelsummary(ms_fake(f), exponentiate = TRUE, effect_scale = "OR")
  expect_equal(x$estimate, unname(exp(stats::coef(f))))
  expect_identical(attr(x, "se_scale"), "link")
  expect_false(any(c("statistic", "group") %in% names(x)))
  tt <- regtab(x, stats = c("n", "ll", "aic"))
  direct <- regtab(f, stats = c("n", "ll", "aic"))
  expect_identical(tt$header[[2]]$text[2], "OR")
  expect_identical(tt$body[[2]], direct$body[[2]])
  expect_identical(tt$body[[4]], direct$body[[4]])
})

test_that("7.15: exponentiate is required; already-exponentiated lists need se_scale", {
  f <- glm(am ~ wt, binomial, mtcars)
  expect_error(tt_from_modelsummary(ms_fake(f)), "must be")
  expect_error(regtab(ms_fake(f)), "tt_from_modelsummary")
  e <- ms_fake(f, exponentiated = TRUE)
  expect_error(tt_from_modelsummary(e, exponentiate = FALSE), "already exponentiated")
  expect_error(tt_from_modelsummary(e, exponentiate = TRUE), "give `se_scale`")
  expect_identical(attr(tt_from_modelsummary(e, exponentiate = TRUE, se_scale = "link"), "se_scale"), "link")
  expect_error(tt_from_modelsummary(ms_fake(f), exponentiate = TRUE, se_scale = "link"), "only to")
  re <- ms_fake(f)
  re$tidy$group[1] <- "id"
  expect_error(tt_from_modelsummary(re, exponentiate = FALSE), "random-effect")
  expect_error(tt_from_modelsummary(1, exponentiate = FALSE), "modelsummary_list")
  lst <- tt_from_modelsummary(list(ms_fake(f), ms_fake(f)), exponentiate = FALSE)
  expect_length(lst, 2L)
})

test_that("7.15: a real modelsummary list, one and several equations", {
  pkg <- "modelsummary"
  skip_if_not_installed(pkg)
  msum <- getExportedValue(pkg, "modelsummary")
  f <- lm(mpg ~ wt + hp, mtcars)
  x <- tt_from_modelsummary(msum(f, output = "modelsummary_list"), exponentiate = FALSE)
  tt <- regtab(x, stats = c("n", "r2"))
  expect_identical(tt$body[[2]], regtab(f, stats = c("n", "r2"))$body[[2]])
  skip_if_not_installed("nnet")
  m <- nnet::multinom(factor(gear) ~ wt, mtcars, trace = FALSE)
  xm <- tt_from_modelsummary(msum(m, output = "modelsummary_list"), exponentiate = TRUE, effect_scale = "RRR")
  expect_identical(unique(xm$equation), c("4", "5"))
  expect_identical(regtab(xm)$body[[1]], c("4: wt", "5: wt"))
})

test_that("stack review 12: Stata's AIC from logLik, no rmse, F for lm, ordinal refused", {
  f <- lm(mpg ~ wt + hp, mtcars)
  x <- ms_fake(f)
  x$glance$rmse <- 2.39
  x$glance$F <- 69.2
  attr(x$tidy, "model_class") <- c("lm")
  out <- tt_from_modelsummary(x, exponentiate = FALSE)
  g <- attr(out, "glance")
  expect_null(g$rmse)
  expect_identical(g$df, 3L)
  expect_identical(attr(out, "stata_cmd"), "regress")
  tt <- regtab(out, stats = c("aic", "F"))
  expect_identical(tt$body[[2]][tt$body[[1]] == "AIC"], regtab(f, stats = "aic")$body[[2]][4])
  expect_identical(tt$body[[2]][tt$body[[1]] == "F statistic"], "69.20")
  o <- ms_fake(f)
  attr(o$tidy, "model_class") <- "polr"
  expect_error(tt_from_modelsummary(o, exponentiate = TRUE), "ordinal")
  pkg <- "modelsummary"
  skip_if_not_installed(pkg)
  skip_if_not_installed("MASS")
  msum <- getExportedValue(pkg, "modelsummary")
  po <- MASS::polr(factor(gear) ~ wt, mtcars, Hess = TRUE)
  expect_error(tt_from_modelsummary(msum(po, output = "modelsummary_list"), exponentiate = TRUE), "ordinal")
})
