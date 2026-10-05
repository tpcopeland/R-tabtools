# Ports of the Stata regtab QA contracts (plan task 4.12):
# qa/test_regtab_omitted.do (tests 1-10), qa/test_regtab.do (nopvalue
# tests 1-6, nsub tests 1-5, stats-alias tests 1-4, v1015 test B, legacy
# suite), and qa/validation_regtab.do (tests 1-3: stats match the models'
# own information criteria). Data generators follow the Stata ones; draws
# differ, but every assertion is structural or checked against R's own fit.

# _rto_data (test_regtab_omitted.do:69-81): grp 1-4 (One..Four), sex =
# mod(n, 2), flag = (grp == 4), so flag and 4.grp are collinear.
rto_data <- function() {
  set.seed(20260903)
  n <- 800
  i <- seq_len(n)
  grp <- i %% 4 + 1
  d <- data.frame(grp = factor(grp, 1:4, c("One", "Two", "Three", "Four")),
                  sex = factor(i %% 2), flag = as.numeric(grp == 4), x = rnorm(n))
  attr(d$grp, "label") <- "Group"
  attr(d$sex, "label") <- "Sex"
  attr(d$flag, "label") <- "Flag"
  attr(d$x, "label") <- "X score"
  d$y <- 0.5 * d$x + 0.3 * (grp == 2) + rnorm(n)
  d$yb <- as.numeric(d$y > 0)
  d
}

cell <- function(tt, label, col = 2L) {
  i <- which(trimws(tt$body[[1]]) == label)
  if (!length(i)) stop("row not found: ", label)
  tt$body[[col]][i[1]]
}
count_cells <- function(tt, value, col = 2L) sum(tt$body[[col]] == value)

test_that("omitted 1: a collinear factor level is Omitted, the base stays Reference", {
  tt <- regtab(lm(y ~ flag + grp + x, rto_data()))
  expect_identical(cell(tt, "One"), "Reference")
  expect_identical(cell(tt, "Four"), "Omitted")
  expect_identical(count_cells(tt, "Reference"), 1L)
  expect_identical(cell(tt, "Four", 3L), "")
  expect_identical(cell(tt, "Four", 4L), "")
})

test_that("omitted 2: a non-default base is the reference", {
  d <- rto_data()
  d$grp <- stats::relevel(d$grp, ref = "Three")
  tt <- regtab(lm(y ~ flag + grp + x, d))
  expect_identical(cell(tt, "Three"), "Reference")
  expect_identical(cell(tt, "Four"), "Omitted")
  expect_identical(count_cells(tt, "Reference"), 1L)
})

test_that("omitted 3: no intercept leaves no reference category", {
  tt <- regtab(lm(y ~ 0 + flag + grp + x, rto_data()), keepintercept = TRUE)
  expect_identical(count_cells(tt, "Reference"), 0L)
  expect_identical(cell(tt, "Four"), "Omitted")
})

test_that("omitted 4: interaction cells separate base, collinear, and empty", {
  d <- rto_data()
  d <- d[!(d$grp == "Four" & d$sex == "1"), ]
  attr(d$grp, "label") <- "Group"
  # (The positional-codes note is expected: grp is a plain factor.)
  expect_message(tt <- regtab(lm(y ~ grp * sex + x, d), interactions = "native"), "level positions")
  expect_identical(cell(tt, "0"), "Reference")
  expect_identical(cell(tt, "1"), "Omitted")
  expect_identical(cell(tt, "1.grp#0.sex"), "Reference")
  expect_identical(cell(tt, "1.grp#1.sex"), "Empty")
  expect_identical(cell(tt, "2.grp#0.sex"), "Empty")
  expect_identical(cell(tt, "2.grp#1.sex"), "Omitted")
})

test_that("omitted 5: an omitted continuous term keeps its label", {
  tt <- regtab(lm(y ~ grp + x + flag, rto_data()))
  expect_identical(cell(tt, "Flag"), "Omitted")
  expect_false(any(grepl("o.flag", tt$body[[1]], fixed = TRUE)))
})

test_that("omitted 6: the constraint class is per model", {
  d <- rto_data()
  tt <- regtab(lm(y ~ grp + x, d), lm(y ~ flag + grp + x, d), models = "M1 \\ M2")
  four <- cell(tt, "Four")
  expect_false(four %in% c("Omitted", "Reference"))
  expect_false(is.na(suppressWarnings(as.numeric(four))))
  expect_true(nzchar(cell(tt, "Four", 3L)))
  expect_identical(cell(tt, "Four", 5L), "Omitted")
  expect_identical(cell(tt, "One"), "Reference")
  expect_identical(cell(tt, "One", 5L), "Reference")
})

test_that("omitted 7: eform models label the constrained level, not 1.00", {
  tt <- regtab(glm(yb ~ flag + grp + x, binomial, rto_data()))
  expect_identical(cell(tt, "Four"), "Omitted")
  expect_identical(cell(tt, "One"), "Reference")
})

test_that("omitted 8: omitlabel and emptylabel are honoured and distinct", {
  d <- rto_data()
  d <- d[!(d$grp == "Four" & d$sex == "1"), ]
  expect_message(tt <- regtab(lm(y ~ grp * sex + x, d), interactions = "native", refcat = "Ref.",
                              omitlabel = "(omitted)", emptylabel = "(empty)"), "level positions")
  expect_identical(cell(tt, "One"), "Ref.")
  expect_identical(cell(tt, "1"), "(omitted)")
  expect_identical(cell(tt, "1.grp#1.sex"), "(empty)")
  expect_error(regtab(lm(y ~ x, d), omitlabel = "Reference"), "must differ")
})

test_that("omitted 9: constrained rows contribute nothing to r(table)", {
  tt <- regtab(lm(y ~ grp + x + flag, rto_data()))
  rn <- sub("^_+", "", rownames(tt$stored$table))
  expect_false("One" %in% rn)
  expect_false(any(c("Flag", "o_flag") %in% rn))
  expect_true("Two" %in% rn)
})

test_that("omitted 10: a model with only a base level is unchanged", {
  tt <- regtab(lm(y ~ grp + x, rto_data()))
  expect_identical(cell(tt, "One"), "Reference")
  expect_identical(count_cells(tt, "Reference"), 1L)
  expect_identical(count_cells(tt, "Omitted"), 0L)
  expect_identical(count_cells(tt, "Empty"), 0L)
  expect_false(is.na(suppressWarnings(as.numeric(cell(tt, "Four")))))
})

test_that("nopvalue 1-5: p-value columns per model", {
  a <- golden_fixture("auto", factors = "foreign")
  f <- lm(price ~ mpg + weight, a)
  tt <- regtab(f)
  expect_identical(ncol(tt$body), 4L)
  expect_identical(tt$header[[2]]$text[4], "p-value")
  expect_equal(tt$stored$N_cols, 5)
  tt <- regtab(f, nopvalue = TRUE)
  expect_identical(tt$header[[2]]$text[-1], c("Coef.", "95% CI"))
  expect_equal(tt$stored$N_cols, 4)
  tt <- regtab(f, compact = TRUE, nopvalue = TRUE)
  expect_identical(ncol(tt$body), 2L)
  expect_match(tt$header[[2]]$text[2], "CI")
  expect_match(tt$body[[2]][1], "\\(.*\\)")
  expect_equal(tt$stored$N_cols, 3)
  tt <- regtab(f, nopvalue = TRUE, stars = TRUE)
  expect_identical(ncol(tt$body), 3L)
  expect_true(any(grepl("*", tt$body[[2]], fixed = TRUE)))
  tt <- regtab(f, lm(price ~ mpg + weight + foreign, a), nopvalue = TRUE, models = "Base \\ Adjusted")
  expect_identical(tt$header[[1]]$text, c("", "Base", "", "Adjusted", ""))
  expect_identical(grepl("CI", tt$header[[2]]$text), c(FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(tt$stored$N_cols, 6)
})

test_that("nopvalue 6: CSV and Excel carry no p-value header", {
  skip_if_not_installed("tidyxl")
  a <- golden_fixture("auto")
  out <- withr::local_tempdir()
  csv <- file.path(out, "np.csv")
  xl <- file.path(out, "np.xlsx")
  regtab(lm(price ~ mpg + weight, a), csv = csv, xlsx = xl, sheet = "NoP", nopvalue = TRUE)
  expect_false(any(grepl("p-value", readLines(csv), fixed = TRUE)))
  cells <- tidyxl::xlsx_cells(xl, sheets = "NoP")
  expect_false(any(cells$character %in% "p-value"))
})

test_that("nsub 1-2: Cox models report subjects, not episodes", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  lung$id <- seq_len(nrow(lung))
  lung$status <- lung$status - 1
  # survSplit() needs the bare Surv() call on the left-hand side.
  Surv <- survival::Surv
  sp <- survival::survSplit(Surv(time, status) ~ ., lung, cut = c(180, 365), episode = "ep")
  expect_gt(nrow(sp), nrow(lung))
  fit <- survival::coxph(survival::Surv(tstart, time, status) ~ age, sp, id = id, ties = "breslow")
  tt <- regtab(fit, stats = "n")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Subjects")
  expect_identical(tt$body[[2]][nrow(tt$body)], as.character(nrow(lung)))
  expect_equal(tt$stored$n_1, nrow(lung))
  fit2 <- survival::coxph(survival::Surv(time, status) ~ age, lung, ties = "breslow")
  tt <- regtab(fit2, stats = "n")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Subjects")
  expect_equal(tt$stored$n_1, nrow(lung))
})

test_that("nsub 3-5: Observations without survival models, Subjects in a mixed table", {
  a <- golden_fixture("auto")
  tt <- regtab(glm(foreign ~ mpg + weight, binomial, a), stats = "n")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Observations")
  expect_identical(tt$body[[2]][nrow(tt$body)], "74")
  tt <- regtab(lm(price ~ mpg + weight, a), stats = "n")
  expect_identical(tt$body[[1]][nrow(tt$body)], "Observations")
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, status) ~ age, survival::lung, ties = "breslow")
  tt <- regtab(cx, glm(foreign ~ mpg, binomial, a), stats = "n")
  last <- tt$body[nrow(tt$body), ]
  expect_identical(last[[1]], "Subjects")
  expect_identical(last[[2]], as.character(nrow(survival::lung)))
  expect_identical(last[[5]], "74")
})

test_that("stats alias 1-4: n_sub and subjects render the n row; unknown tokens warn", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg + weight, a)
  base <- regtab(f, stats = "n")
  expect_identical(regtab(f, stats = "n_sub")$body, base$body)
  expect_identical(regtab(f, stats = "subjects")$body, base$body)
  expect_warning(tt <- regtab(f, stats = "n foo"), "not recognized")
  expect_identical(tt$body, base$body)
})

test_that("v1015 B: coefficients above 1000 keep full precision and no comma", {
  set.seed(1)
  d <- data.frame(x = runif(200))
  d$y <- 100 + 2500 * d$x + rnorm(200, 0, 50)
  f <- lm(y ~ x, d)
  tt <- regtab(f, digits = 3)
  expect_equal(unname(tt$stored$table[1, 1]), unname(coef(f)["x"]))
  txt <- cell(tt, "x")
  expect_match(txt, "^[0-9]+\\.[0-9]{3}$")
})

test_that("legacy suite: model families, title, separator, stats", {
  a <- golden_fixture("auto")
  expect_identical(regtab(glm(foreign ~ mpg, binomial, a))$header[[2]]$text[2], "OR")
  expect_identical(regtab(glm(rep78 ~ mpg, poisson, a))$header[[2]]$text[2], "IRR")
  expect_identical(regtab(lm(price ~ mpg, a))$header[[2]]$text[2], "Coef.")
  tt <- regtab(lm(price ~ mpg, a), title = "Table X", sep = " - ")
  expect_identical(tt$title, "Table X")
  expect_match(tt$body[[3]][1], "^\\(-?[0-9.]+ - -?[0-9.]+\\)$")
  # the auto nointercept for ratio models, overridable both ways
  expect_false("Intercept" %in% regtab(glm(foreign ~ mpg, binomial, a))$body[[1]])
  expect_true("Intercept" %in% regtab(glm(foreign ~ mpg, binomial, a), keepintercept = TRUE)$body[[1]])
  expect_false("Intercept" %in% regtab(lm(price ~ mpg, a), nointercept = TRUE)$body[[1]])
  # the data are untouched
  a2 <- a
  regtab(lm(price ~ mpg, a))
  expect_identical(a, a2)
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, status) ~ age + sex, survival::lung, ties = "breslow")
  tt <- regtab(cx)
  expect_identical(tt$header[[2]]$text[2], "HR")
  expect_identical(tt$stored$coef_label, "HR")
  expect_equal(unname(tt$stored$table[, 1]), unname(exp(coef(cx))))
})

test_that("validation 1: OLS stats match the fit's own criteria", {
  a <- golden_fixture("auto")
  f <- lm(price ~ mpg + weight, a)
  tt <- regtab(f, stats = "n aic bic ll r2")
  st <- tt$stored
  k <- length(coef(f))
  expect_equal(st$ll_1, as.numeric(logLik(f)))
  expect_equal(st$aic_1, -2 * as.numeric(logLik(f)) + 2 * k)
  expect_equal(st$bic_1, -2 * as.numeric(logLik(f)) + k * log(nobs(f)))
  expect_identical(cell(tt, "R\u00b2"), sprintf("%.3f", summary(f)$r.squared))
})

test_that("validation 2-3: logit stats and multi-model stats", {
  a <- golden_fixture("auto")
  g <- glm(foreign ~ mpg + weight, binomial, a)
  f <- lm(price ~ mpg, a)
  tt <- regtab(g, f, stats = "n aic bic ll r2")
  st <- tt$stored
  expect_equal(st$aic_1, AIC(g))
  expect_equal(st$bic_1, BIC(g))
  expect_equal(st$ll_2, as.numeric(logLik(f)))
  expect_equal(st$n_2, 74)
  r2p <- 1 - as.numeric(logLik(g)) / as.numeric(logLik(glm(foreign ~ 1, binomial, a)))
  i <- which(tt$body[[1]] == "R\u00b2 / Pseudo R\u00b2")
  expect_identical(tt$body[[2]][i], sprintf("%.3f", r2p))
  expect_identical(tt$body[[5]][i], sprintf("%.3f", summary(f)$r.squared))
})
