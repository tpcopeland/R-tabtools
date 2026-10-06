# Unit tests for the regtab engine (plan tasks 4.1-4.11): Wald intervals,
# estimate pre-rounding, row selection, fvgen labels, statistics, the row
# union, and stored results. Stata behaviours cited here were probed in
# Stata 17 with tabtools 2.1.8 (IMPLEMENTATION_PLAN.md, Phase 4 findings).

auto <- function(factors = c("foreign", "rep78")) golden_fixture("auto", factors = factors)

body_col <- function(tt, j) tt$body[[j]]
body_row <- function(tt, label) which(trimws(tt$body[[1]]) == label)

test_that("Wald estimates reproduce summary() and the matching quantile", {
  a <- auto()
  f <- lm(price ~ mpg + weight + foreign, a)
  w <- tabtools:::tt_wald(f, 0.95)
  s <- summary(f)$coefficients
  expect_equal(w$estimate, unname(s[, 1]), tolerance = 1e-12)
  expect_equal(w$std.error, unname(s[, 2]), tolerance = 1e-12)
  expect_equal(w$p.value, unname(s[, 4]), tolerance = 1e-10)
  ci <- confint(f)
  expect_equal(w$conf.low, unname(ci[, 1]), tolerance = 1e-12)
  expect_equal(w$conf.high, unname(ci[, 2]), tolerance = 1e-12)
  w90 <- tabtools:::tt_wald(f, 0.90)
  expect_equal(w90$conf.high, unname(confint(f, level = 0.90)[, 2]), tolerance = 1e-12)

  g <- glm(foreign ~ mpg + weight, binomial, golden_fixture("auto"))
  w <- tabtools:::tt_wald(g, 0.95)
  # normal quantile (z), as logit reports
  z <- qnorm(0.975)
  expect_equal(w$conf.high - w$estimate, z * w$std.error, tolerance = 1e-12)
  expect_equal(w$p.value, 2 * pnorm(-abs(w$estimate / w$std.error)), tolerance = 1e-12)
  # Fisher information at the final estimates: within 1e-6 of vcov.glm's
  # last-iteration weights, and exactly X'WX evaluated at coef().
  expect_equal(w$std.error, unname(sqrt(diag(vcov(g)))), tolerance = 1e-6)
  X <- model.matrix(g)
  mu <- plogis(drop(X %*% coef(g)))
  V <- solve(crossprod(X, X * (mu * (1 - mu))))
  expect_equal(w$std.error, unname(sqrt(diag(V))), tolerance = 1e-12)
})

test_that("aliased coefficients keep a row with missing statistics", {
  a <- auto()
  f <- lm(price ~ mpg + mpg_dup, a)
  w <- tabtools:::tt_wald(f)
  expect_identical(w$term, c("(Intercept)", "mpg", "mpg_dup"))
  expect_true(all(is.na(unlist(w[3, -1]))))
})

test_that("profile intervals replace only the bounds", {
  g <- glm(foreign ~ mpg, binomial, golden_fixture("auto"))
  w <- tabtools:::tt_wald(g, ci_method = "profile")
  pr <- suppressMessages(confint(g))
  expect_equal(w$conf.low, unname(pr[, 1]), tolerance = 1e-8)
  expect_equal(w$p.value, tabtools:::tt_wald(g)$p.value)
})

test_that("estimates are pre-rounded with Stata's round(); CI bounds are not", {
  est <- tabtools:::.rt_est_text
  # Stata round(x, u) = floor(x/u + 0.5) * u: halves go toward +Inf.
  expect_identical(est(c(0.125, 1.115, -0.125, 2.675), 2), c("0.13", "1.12", "-0.12", "2.68"))
  expect_identical(sprintf("%.2f", c(0.125, 2.675)), c("0.12", "2.67"))
  expect_identical(est(c(-0.00336, 1234.5), c(2)), c("0.00", "1234.50"))
  expect_identical(est(1234.5, 0), "1235")
  ci <- tabtools:::.rt_ci_text
  expect_identical(ci(0.125, 2.675, 2, ", "), "(0.12, 2.67)")
  expect_identical(ci(-0.0004, 1, 2, "; "), "(-0.00; 1.00)")
  expect_identical(ci(NA, 1, 2, ", "), "")
})

test_that("keep/drop terms match raw keys exactly (age vs stage)", {
  m <- tabtools:::tt_match_rows
  keys <- c("age", "stage", "1.stage", "2.stage", "_cons")
  expect_identical(m(keys, "age"), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(m(keys, "stage"), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(m(keys, "i.stage"), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(m(keys, "2.stage"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(m(keys, "2b.stage"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(m(keys, "_cons"), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # an explicit level never selects another level
  expect_identical(m(c("2.arm", "20.arm", "arm"), "2.arm"), c(TRUE, FALSE, FALSE))
  expect_identical(m(c("2.arm", "20.arm", "arm"), "arm"), c(TRUE, TRUE, TRUE))
  # a bare name selects interactions containing it; interaction terms match exactly
  ik <- c("rep78", "3.rep78", "foreign#rep78", "1.foreign#3.rep78", "1.foreign#c.mpg", "c.mpg#c.weight")
  expect_identical(m(ik, "i.rep78"), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(m(ik, "mpg"), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(m(ik, "1.foreign#3.rep78"), c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(m(ik, "1b.foreign#3.rep78"), c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(m(ik, "3.rep78"), c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  # R coefficient names are accepted too
  expect_identical(m(c("2.stage", "3.stage"), "stageMid", c("stageMid", "stageHigh")), c(TRUE, FALSE))
  expect_identical(tabtools:::.rt_match_normalize(c("ib3.x", "c.a#i.b", "2o.x", "ibn.x")),
                   c("x", "a#b", "2.x", "x"))
})

test_that("keep() and drop() on a fitted table follow Stata", {
  a <- auto(c("stage", "rep78"))
  tt <- regtab(lm(price ~ age + stage + mpg, a), keep = "age")
  expect_identical(tt$body[[1]], "Owner age")
  tt <- regtab(lm(price ~ age + stage + mpg, a), keep = "stage")
  expect_identical(trimws(tt$body[[1]])[1], attr(a$stage, "label"))
  expect_false("Owner age" %in% tt$body[[1]])
  # probe: keep(1.rep78) keeps only that level row, not its parent
  tt <- regtab(lm(price ~ mpg + rep78, a), keep = "1.rep78")
  expect_identical(tt$body[[1]], "  1")
  expect_identical(tt$body[[2]], "Reference")
  tt <- regtab(lm(price ~ mpg + rep78, a), drop = "_cons")
  expect_false("Intercept" %in% tt$body[[1]])
  expect_error(regtab(lm(price ~ mpg, a), keep = "weight"), "matched no coefficient rows")
  expect_error(regtab(lm(price ~ mpg, a), drop = c("mpg", "_cons")), "remove every coefficient row")
  # labelmatch: case-insensitive substring of displayed labels
  tt <- regtab(lm(price ~ mpg + rep78, a), keep = "MILE", labelmatch = TRUE)
  expect_identical(tt$body[[1]], "Mileage (mpg)")
})

test_that("fvgen labels follow fvgen.ado", {
  fl <- tabtools:::.rt_fv_partlabel
  expect_identical(fl("foreign", "Foreign", TRUE), "Foreign")
  expect_identical(fl("rep78", "3", FALSE), "rep78=3")
  a <- auto(c("foreign", "rep78", "pclass"))
  tt <- regtab(lm(price ~ foreign * mpg, a))
  expect_identical(tt$body[[1]], c("Foreign", "Mileage (mpg)", "Foreign \u00d7 Mileage (mpg)", "Intercept"))
  tt <- regtab(lm(price ~ foreign * pclass + mpg + I(mpg^2), a), vsref = "[ref @]", xsymbol = "x")
  expect_identical(tt$body[[1]][1:5], c("Foreign [ref Domestic]", "Mid-range [ref Budget]",
                                         "Premium [ref Budget]", "Foreign x Mid-range", "Foreign x Premium"))
  expect_identical(tt$body[[1]][7], "Mileage (mpg)\u00b2")
  # unlabelled factor levels read var=level; a variable outside any
  # interaction keeps the header/level/reference layout
  tt <- regtab(lm(price ~ foreign * rep78 + pclass, a, subset = rep78 %in% c("3", "4", "5")))
  lab <- tt$body[[1]]
  expect_true(all(c("rep78=4", "rep78=5", "Foreign \u00d7 rep78=4") %in% lab))
  expect_true(all(c("Price class", "  Budget", "  Mid-range") %in% lab))
  expect_identical(tt$body[[2]][lab == "  Budget"], "Reference")
  # three-way interactions are native only
  expect_error(regtab(lm(price ~ foreign * rep78 * mpg, a)), "two-way")
  expect_s3_class(regtab(lm(price ~ foreign * pclass * mpg, a), interactions = "native"), "tt_table")
})

test_that("native interactions separate base, empty, and omitted cells", {
  a <- auto()
  tt <- regtab(lm(price ~ foreign * rep78, a), interactions = "native")
  lab <- tt$body[[1]]
  est <- tt$body[[2]]
  expect_identical(est[lab == "1.foreign#1.rep78"], "Empty")
  expect_identical(est[lab == "0.foreign#3.rep78"], "Reference")
  expect_identical(est[lab == "1.foreign#5.rep78"], "Omitted")
  expect_identical(tt$rows$type[lab == "foreign#rep78"], "cat_header")
})

test_that("statistics are recomputed as regtab does", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg + weight, a)
  s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
  # Stata's rank counts coefficients only; R's logLik df also counts sigma.
  expect_equal(s$aic, AIC(f) - 2)
  expect_equal(s$bic, BIC(f) - log(nobs(f)))
  expect_equal(s$ll, as.numeric(logLik(f)))
  expect_equal(s$r2, summary(f)$r.squared)
  g <- glm(foreign ~ mpg + weight, binomial, a)
  s <- tabtools:::tt_model_stats(g, tabtools:::tt_model_info(g))
  expect_equal(s$aic, AIC(g))
  expect_equal(s$bic, BIC(g))
  g0 <- glm(foreign ~ 1, binomial, a)
  expect_equal(s$r2_p, 1 - as.numeric(logLik(g) / logLik(g0)))
  expect_true(is.na(s$r2))
  p <- glm(rep78 ~ mpg, poisson, a)
  s <- tabtools:::tt_model_stats(p, tabtools:::tt_model_info(p))
  p0 <- glm(rep78 ~ 1, poisson, a[!is.na(a$rep78), ])
  expect_equal(s$r2_p, 1 - as.numeric(logLik(p) / logLik(p0)))
  skip_if_not_installed("MASS")
  nb <- suppressWarnings(MASS::glm.nb(rep78 ~ mpg, a))
  s <- tabtools:::tt_model_stats(nb, tabtools:::tt_model_info(nb))
  # nbreg's rank includes lnalpha, as R's logLik df includes theta
  expect_equal(s$aic, AIC(nb))
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, status) ~ age, survival::lung, ties = "breslow")
  s <- tabtools:::tt_model_stats(cx, tabtools:::tt_model_info(cx))
  expect_equal(s$ll, cx$loglik[2])
  expect_equal(s$N, cx$n)
  expect_equal(s$aic, -2 * cx$loglik[2] + 2)
})

test_that("stats rows: labels, order, formats, tokens", {
  a <- golden_fixture("auto")
  tt <- regtab(lm(price ~ mpg, a), stats = "r2 ll bic aic n")
  lab <- tt$body[[1]]
  expect_identical(lab[-(1:2)], c("Observations", "AIC", "BIC", "Log-likelihood", "R\u00b2"))
  expect_identical(tt$body[[2]][lab == "Observations"], "74")
  expect_identical(tt$rows$type[-(1:2)], rep("stat", 5))
  expect_identical(tt$body[[3]][lab == "AIC"], "")
  expect_equal(tt$stored$n_1, 74)
  expect_equal(tt$stored$aic_1, AIC(lm(price ~ mpg, a)) - 2)
  expect_null(tt$stored$icc_1)
  expect_warning(regtab(lm(price ~ mpg, a), stats = c("n", "bogus")), "not recognized")
  t2 <- regtab(lm(price ~ mpg, a), stats = "n_sub")
  expect_identical(t2$body[[1]][3], "Observations")
  t3 <- regtab(lm(price ~ mpg, a), stats = "subjects")
  expect_identical(t3$body, t2$body)
  # R\u00b2 / Pseudo R\u00b2 label across mixed models
  tt <- regtab(lm(price ~ mpg, a), glm(foreign ~ mpg, binomial, a), stats = "r2")
  expect_identical(tt$body[[1]][nrow(tt$body)], "R\u00b2 / Pseudo R\u00b2")
  tt <- regtab(glm(foreign ~ mpg, binomial, a), stats = "r2")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Pseudo R\u00b2")
})

test_that("multi-model rows union in first-appearance order with the intercept last", {
  a <- auto()
  # Stata probe P2: mpg turn + mpg weight turn -> mpg, turn, weight
  tt <- regtab(lm(price ~ mpg + turn, a), lm(price ~ mpg + weight + turn, a))
  expect_identical(tt$body[[1]], c("Mileage (mpg)", "Turn circle (ft.)", "Weight (lbs.)", "Intercept"))
  expect_identical(tt$body[[2]][3], "")
  # probe P4: a factor block in model 1, another factor in model 2
  tt <- regtab(lm(price ~ rep78 + mpg, a), lm(price ~ mpg + foreign, a))
  lab <- trimws(tt$body[[1]])
  expect_identical(lab, c("Repair record 1978", as.character(1:5), "Mileage (mpg)", "Car origin",
                          "Domestic", "Foreign", "Intercept"))
  expect_identical(tt$header[[1]]$text, c("", "Model 1", "", "", "Model 2", "", ""))
})

test_that("stored results mirror r()", {
  a <- auto()
  tt <- regtab(lm(price ~ mpg + foreign, a), glm(foreign ~ mpg, binomial, a), stars = TRUE)
  st <- tt$stored
  expect_identical(st$coef_label, "mixed")
  expect_identical(st$stars, "stars")
  expect_identical(st$N_models, 2L)
  expect_equal(st$N_rows, nrow(tt$body) + 3)
  expect_equal(st$N_cols, ncol(tt$body) + 1)
  expect_identical(st$ci_level, 95)
  expect_match(st$methods, "^Collected regression estimates with 95% confidence intervals across 2 models\\.")
  expect_match(st$methods, "\\* p<0.05, \\*\\* p<0.01, \\*\\*\\* p<0.001\\.$")
  expect_identical(rownames(st$table), c("Mileage_(mpg)", "__Foreign", "Intercept"))
  expect_equal(unname(st$table["__Foreign", 2]), NA_real_)
  expect_equal(unname(st$table["Intercept", 2]), exp(unname(coef(glm(foreign ~ mpg, binomial, a))[1])))
  expect_identical(tabtools:::.rt_rowname(c("a.b, c: d", "1.foreign#3.rep78", ""), 1:3),
                   c("a_b_c_d", "1_foreign_3_rep78", "row3"))  # 2.1.11 sanitiser (2.1.9: c.1_foreign#c.3_rep78)
  expect_identical(nchar(tabtools:::.rt_rowname(strrep("x", 40), 1), "bytes"), 32L)
  meta <- tt$meta$frame
  expect_identical(meta$source, "regtab")
  df <- as.data.frame(tt)
  expect_identical(attr(df, "effect_scale"), c("Coef.", "OR"))
  expect_identical(attr(df, "n_models"), 2L)
  expect_identical(meta$outcome_id, c("price", "foreign"))
  expect_identical(meta$effect_scale, c("Coef.", "OR"))
  expect_identical(meta$model_label, c("Model 1", "Model 2"))
})

test_that("cell layout options: compact, nopvalue, stars, sep, digits, level", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg + weight, a)
  tt <- regtab(f, compact = TRUE, nopvalue = TRUE)
  expect_identical(ncol(tt$body), 2L)
  expect_identical(tt$header[[2]]$text, c("", "Coef. 95% CI"))
  expect_match(tt$body[[2]][1], "^-?[0-9.]+ \\(.*\\)$")
  tt <- regtab(f, stars = TRUE, nopvalue = TRUE)
  expect_identical(ncol(tt$body), 3L)
  expect_true(any(grepl("\\*", tt$body[[2]])))
  expect_identical(tt$meta$xlsx_footnote, "* p<0.05, ** p<0.01, *** p<0.001")
  tt <- regtab(f, stars = TRUE, starslevels = c(0.1, 0.05, 0.01), footnote = "Note")
  expect_identical(tt$meta$xlsx_footnote, "Note \\ * p<.1, ** p<.05, *** p<.01")
  tt <- regtab(f, sep = " to ", digits = 3, level = 90)
  expect_identical(tt$header[[2]]$text[3], "90% CI")
  expect_match(tt$body[[3]][1], "^\\(-?[0-9]+\\.[0-9]{3} to -?[0-9]+\\.[0-9]{3}\\)$")
  expect_identical(tt$stored$ci_level, 90)
  # CDISC: digits 4, Estimate header, stats n
  tt <- regtab(f, cdisc = TRUE)
  expect_identical(tt$header[[2]]$text[2], "Estimate")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Observations")
  expect_match(tt$body[[2]][1], "\\.[0-9]{4}$")
  # addrow as a list or a Stata string
  t1 <- regtab(f, addrow = list("P trend" = 0.032))
  t2 <- regtab(f, addrow = "\"P trend\" 0.032")
  expect_identical(t1$body, t2$body)
  expect_identical(t1$rows$type[nrow(t1$body)], "addrow")
})

test_that("dimnonsig follows regtab.ado: raw CIs, reference rows, parents", {
  a <- auto(c("rep78", "pclass"))
  tt <- regtab(lm(price ~ mpg + pclass + rep78, a), dimnonsig = TRUE)
  lab <- trimws(tt$body[[1]])
  dim <- tt$rows$dim
  expect_true(dim[lab == "Mileage (mpg)"])
  expect_true(dim[lab == "Budget"])
  expect_false(dim[lab == "Premium"])
  expect_true(dim[lab == "1"])
  # Repair record: reference child and no significant child -> dimmed
  expect_true(dim[lab == "Repair record 1978"])
  # Price class: Premium is significant -> parent not dimmed
  expect_false(dim[lab == "Price class"])
  # the intercept's CI excludes 0
  expect_false(dim[lab == "Intercept"])
})

test_that("argument validation mirrors Stata", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg, a)
  expect_error(regtab(f, keep = "mpg", drop = "mpg"), "cannot be used together")
  expect_error(regtab(f, labelmatch = TRUE), "requires")
  expect_error(regtab(f, omitlabel = "Reference"), "must differ")
  expect_error(regtab(f, starslevels = c(0.1, 0.05)), "exactly 3")
  expect_error(regtab(f, digits = 7), "between 0 and 6")
  expect_error(regtab(f, level = 1), "level")
  expect_error(regtab(f, open = TRUE), "requires")
  expect_error(regtab(f, markdown = "x.txt"), "markdown")
  expect_error(regtab(f, mdappend = TRUE), "requires")
  expect_error(regtab(f, sheet = "a/b"), "not allowed")
  expect_error(regtab(), "at least one")
  withr::local_options(tabtools.digits = 3)
  expect_match(regtab(f)$body[[2]][1], "\\.[0-9]{3}$")
})

test_that("models accepts a list and Stata-style labels", {
  a <- golden_fixture("auto")
  fs <- list(lm(price ~ mpg, a), lm(price ~ mpg + weight, a))
  t1 <- regtab(fs, models = "A \\ B")
  t2 <- regtab(fs[[1]], fs[[2]], models = c("A", "B"))
  expect_identical(t1$body, t2$body)
  expect_identical(t1$header, t2$header)
  expect_identical(regtab(fs, models = "Only")$header[[1]]$text[5], "Model 2")
})

test_that("as_forest_data mirrors eplotframe()", {
  a <- auto()
  tt <- regtab(glm(foreign ~ mpg + rep78, binomial, a, subset = rep78 %in% c("3", "4", "5")),
               glm(foreign ~ mpg, binomial, a), models = c("Adj", "Crude"))
  fd <- as_forest_data(tt)
  expect_named(fd, c("label", "estimate", "ll", "ul", "pvalue", "model", "model_label", "rowtype",
                     "section", "source_row"))
  ref <- fd[fd$rowtype == "reference", ]
  expect_identical(ref$label, "  3")
  expect_true(all(is.na(ref$estimate)))
  mp <- fd[fd$label == "Mileage (mpg)", ]
  expect_identical(mp$model_label, c("Adj", "Crude"))
  g <- glm(foreign ~ mpg, binomial, a)
  expect_equal(mp$estimate[2], exp(unname(coef(g)["mpg"])))
  expect_identical(attr(fd, "source"), "regtab")
  expect_identical(attr(fd, "statistic_ids"), "estimate ci pvalue")
  expect_error(as_forest_data(table1_tc(a, vars = "mpg")), "regtab")
})

test_that("D04: sheet without xlsx is an error, as in table1_tc", {
  expect_error(regtab(lm(mpg ~ wt, mtcars), sheet = "Zed"), "`sheet` is only available when using `xlsx`")
})

test_that("P2-2: sheet = NULL means the default sheet", {
  expect_no_error(regtab(lm(mpg ~ wt, mtcars), sheet = NULL))
})
