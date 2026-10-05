# Phase 5b (plan task 5.6): regtab with geepack::geeglm (Stata xtgee).
# Stata expectations: goldens R23, R25i (test-golden-regtab.R) and
# tests/testthat/fixtures/regtab_phase5b/ (qa/stata/make_regtab_phase5b.do).

p5b <- function(name) test_path("fixtures", "regtab_phase5b", name)

gee_params <- function(model) {
  d <- utils::read.csv(p5b("re_params.csv"), stringsAsFactors = FALSE, strip.white = TRUE,
                       na.strings = c(".", ".b"))
  d <- d[d$model == model, , drop = FALSE]
  rownames(d) <- d$param
  d
}

gee_stats <- function(call) {
  d <- utils::read.csv(p5b("re_stats.csv"), stringsAsFactors = FALSE, strip.white = TRUE)
  d[d$call == call, , drop = FALSE]
}

expect_gee_csv <- function(tt, tag) {
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, f)
  expect_identical(readLines(f), readLines(p5b(paste0(tag, ".csv"))))
}

union_data <- function() golden_fixture("union")

# geepack's default tolerance (1e-4) leaves the estimates ~1e-6 from
# xtgee's; 1e-6 reproduces xtgee's default fit to ~1e-12.
gee_fit <- function(formula, family, eps = 1e-6, ...) {
  geepack::geeglm(formula, id = idcode, family = family, corstr = "exchangeable", data = union_data(),
                  control = geepack::geese.control(epsilon = eps, maxit = 100), ...)
}

test_that("geeglm: xtgee's model-based variance, estimates, and QICu", {
  skip_on_cran()
  skip_if_not_installed("geepack")
  f <- gee_fit(union ~ age + grade + not_smsa + south, binomial)
  sp <- gee_params("gee_logit")
  terms <- c("_cons", "age", "grade", "not_smsa", "south")
  expect_equal(unname(coef(f)), sp[terms, "b"], tolerance = 1e-9)
  w <- tabtools:::tt_wald(f)
  # vbeta.naiv / gamma: xtgee fixes the binomial scale at 1.
  expect_equal(w$std.error, sp[terms, "se"], tolerance = 1e-7)
  expect_equal(w$conf.low, sp[terms, "ll"], tolerance = 1e-7)
  expect_equal(w$p.value, sp[terms, "p"], tolerance = 1e-6)
  # vce = "model": geepack's sandwich, Stata's vce(robust) without G/(G-1).
  rb <- gee_params("gee_logit_robust")
  G <- length(unique(f$id))
  wm <- tabtools:::tt_wald(f, vce = "model")
  expect_equal(wm$std.error, rb[terms, "se"] * sqrt((G - 1) / G), tolerance = 1e-6)
  # QICu = deviance + 2 rank at the fitted means (Pan 2001; Stata e(deviance)
  # 27132.20315387471 + 2 * 5, golden R23 qic_1).
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  mu <- fitted(f)
  dev <- -2 * sum(f$y * log(mu) + (1 - f$y) * log(1 - mu))
  expect_equal(s$qic, dev + 2 * 5)
  expect_equal(s$qic, 27142.203153874711, tolerance = 1e-10)
  expect_identical(c(s$N, s$groups), c(26200L, 4434L))
  expect_true(is.na(s$aic) && is.na(s$bic) && is.na(s$ll))
})

test_that("geeglm follows glm's family/link rule, as xtgee in tabtools 2.1.12", {
  skip_if_not_installed("geepack")
  # binomial/logit: odds ratios, the intercept dropped.
  f <- gee_fit(union ~ age + south, binomial, eps = 1e-4)
  tt <- regtab(f)
  expect_identical(tt$header[[2]]$text[2], "OR")
  expect_identical(tt$body[[1]], c("Age in current year", "1 if south"))
  expect_equal(tt$meta$regtab_rows$estimate[1], exp(unname(coef(f)["age"])))
  expect_identical(regtab(f, keepintercept = TRUE)$body[[1]][3], "Intercept")
  # Any other link: coefficients with the intercept (a Gaussian identity GEE).
  g <- gee_fit(grade ~ age + south, gaussian, eps = 1e-4)
  tg <- regtab(g)
  expect_identical(tg$header[[2]]$text[2], "Coef.")
  expect_identical(tg$body[[1]][3], "Intercept")
  expect_equal(tg$meta$regtab_rows$estimate[1], unname(coef(g)["age"]))
})

test_that("Gaussian geeglm: Stata's N-divisor scale; QICu unavailable", {
  skip_on_cran()
  skip_if_not_installed("geepack")
  f <- gee_fit(grade ~ age + not_smsa + south, gaussian)
  sp <- gee_params("gee_gauss")
  terms <- c("_cons", "age", "not_smsa", "south")
  expect_equal(tabtools:::tt_wald(f)$std.error, sp[terms, "se"], tolerance = 1e-7)
  expect_message(tt <- regtab(f, stats = c("n", "aic", "groups")), "QICu unavailable")
  expect_gee_csv(tt, "gee_gauss")
  expect_null(tt$stored$qic_1)
  # A scale fixed at 1 makes QICu available again.
  f1 <- gee_fit(grade ~ age + not_smsa + south, gaussian, scale.fix = TRUE)
  s <- tabtools:::tt_model_stats(f1, tabtools:::tt_model_info(f1))
  expect_equal(s$qic, sum(residuals(f1, "response")^2) + 2 * 4)
  expect_false(s$qic_note)
})

test_that("Poisson geeglm: QICu row and value", {
  skip_on_cran()
  skip_if_not_installed("geepack")
  f <- gee_fit(union ~ age + grade + not_smsa + south, poisson)
  tt <- regtab(f, stats = c("n", "qic", "groups"))
  expect_gee_csv(tt, "gee_pois")
  st <- gee_stats("gee_pois")
  expect_equal(tt$stored$qic_1, as.numeric(st$value[st$name == "qic_1"]), tolerance = 1e-9)
  sp <- gee_params("gee_pois")
  expect_equal(tabtools:::tt_wald(f)$std.error, sp[c("_cons", "age", "grade", "not_smsa", "south"), "se"],
               tolerance = 1e-7)
})

test_that("QICu note names only the GEE models whose scale is estimated", {
  skip_if_not_installed("geepack")
  a <- gee_fit(union ~ age, binomial, eps = 1e-4)
  b <- gee_fit(grade ~ age, gaussian, eps = 1e-4)
  expect_message(tt <- regtab(a, b, stats = c("aic", "n")), "GEE model\\(s\\) 2:")
  # AIC relabelled QICu: only GEE values exist.
  expect_true("QICu" %in% tt$body[[1]])
  expect_false("AIC" %in% tt$body[[1]])
  expect_silent(regtab(a, b, stats = "n"))
})
