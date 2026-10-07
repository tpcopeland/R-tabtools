# stats() tokens of Stata tabtools 2.1.12 (task 5.17, conditional task C4):
# events, r2_a, rmse, F, mi_m, fmi, in Stata's fixed row order. Stata's
# e() values of the completed-data commands are in
# fixtures/regtab_mi/stats.csv (qa/stata/make_regtab_mi.do); the rendered
# rows are pinned by goldens ST01-ST05 and MI01-MI07.

st_stata <- function(id, name) {
  x <- utils::read.csv(test_path("fixtures", "regtab_mi", "stats.csv"), colClasses = "character")
  v <- trimws(x$value[x$id == id & x$name == name])
  if (identical(v, ".")) NA_real_ else as.numeric(v)
}

st_stats <- function(fit, vce = "stata", cluster = NULL) {
  tabtools:::tt_model_stats(fit, tabtools:::tt_model_info(fit), vce = vce, cluster = cluster)
}

test_that("5.17: regress's e(r2_a), e(rmse) and e(F) under each variance and weight (probes S1-S8)", {
  d <- golden_fixture("auto")
  d$c3 <- seq_len(nrow(d)) %% 3
  f <- lm(price ~ mpg + weight, data = d)
  chk <- function(s, id, F = TRUE) {
    expect_equal(s$r2_a, st_stata(id, "r2_a"), tolerance = 1e-12)
    expect_equal(s$rmse, st_stata(id, "rmse"), tolerance = 1e-12)
    if (F) expect_equal(s$F, st_stata(id, "F"), tolerance = 1e-10)
  }
  chk(st_stats(f), "S1")
  chk(st_stats(f, "robust"), "S2")
  # vce(cluster c3): 3 clusters cannot identify 3 coefficients; e(F) missing.
  s3 <- st_stats(f, "cluster", ~c3)
  chk(s3, "S3", F = FALSE)
  expect_true(is.na(s3$F) && is.na(st_stata("S3", "F")))
  chk(st_stats(lm(price ~ mpg, data = d), "cluster", ~c3), "S3b")
  fw <- lm(price ~ mpg + weight, data = d, weights = turn)
  chk(st_stats(fw), "S4")
  chk(st_stats(fw, "robust"), "S5")
  chk(st_stats(lm(price ~ 0 + mpg + weight, data = d)), "S6")
  # Constant only: e(F) = 0, which regtab shows.
  s7 <- st_stats(lm(price ~ 1, data = d))
  chk(s7, "S7")
  expect_identical(s7$F, 0)
  # The C4 review's probes (T1-T12): noconstant with few clusters, zero
  # [aw] weights, factor dummies, collinear terms.
  d$c4 <- seq_len(nrow(d)) %% 4
  d$w0 <- ifelse(seq_len(nrow(d)) %% 7 == 0, 0, d$turn / 40)
  d$mpg2 <- 2 * d$mpg
  dd <- tt_as_factor(d, vars = c("foreign", "rep78"))
  chk(st_stats(lm(price ~ 0 + mpg + weight, data = d), "cluster", ~c3), "T1")
  chk(st_stats(lm(price ~ mpg + weight + turn, data = d), "cluster", ~c4), "T2", F = FALSE)
  expect_true(is.na(st_stats(lm(price ~ mpg + weight + turn, data = d), "cluster", ~c4)$F))
  expect_true(is.na(st_stata("T2", "F")))
  chk(st_stats(lm(price ~ mpg + weight, data = d), "cluster", ~c4), "T3")
  chk(st_stats(lm(price ~ mpg + weight, data = d, weights = w0)), "T4")
  chk(st_stats(lm(price ~ mpg + weight, data = d, weights = w0), "cluster", ~c4), "T5")
  chk(st_stats(lm(price ~ 0 + mpg + weight, data = d), "robust"), "T6")
  chk(st_stats(lm(price ~ mpg + weight + foreign + rep78, data = dd[!is.na(dd$rep78), ])), "T7")
  chk(st_stats(lm(price ~ mpg + mpg2 + weight, data = d)), "T8")
  chk(st_stats(lm(price ~ mpg + mpg2 + weight, data = d), "robust"), "T9")
  chk(st_stats(lm(price ~ 1, data = d), "robust"), "T11")
  chk(st_stats(lm(price ~ 0 + mpg, data = d), "cluster", ~c3), "T12")
  # A singleton dummy under vce(robust): the sandwich is singular (rank 2 of
  # 3) and Stata's e(F) is missing (C4 review F2).
  d$single <- as.numeric(seq_len(nrow(d)) == 1)
  s10 <- st_stats(lm(price ~ mpg + single, data = d), "robust")
  chk(s10, "S10", F = FALSE)
  expect_true(is.na(s10$F) && is.na(st_stata("S10", "F")))
  expect_false(is.na(st_stats(lm(price ~ mpg + single, data = d))$F))
  d8 <- d[!is.na(d$rep78), ]
  f8 <- lm(price ~ mpg + weight + turn, data = d8)
  expect_identical(stats::nobs(f8), as.integer(st_stata("S8", "N")))
  chk(st_stats(f8, "cluster", ~rep78), "S8")
})

test_that("5.17: svy: regress stores the adjusted Wald F and no r2_a/rmse; svy: logit no F (probe S9)", {
  skip_if_not_installed("survey")
  d <- golden_fixture("auto")
  d$w <- d$turn / 40
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  s <- st_stats(survey::svyglm(price ~ mpg + weight, design = des))
  expect_equal(s$F, st_stata("S9", "F"), tolerance = 1e-12)
  expect_true(is.na(s$r2_a) && is.na(st_stata("S9", "r2_a")))
  expect_true(is.null(s$rmse) && is.na(st_stata("S9", "rmse")))
  sl <- st_stats(suppressWarnings(survey::svyglm(foreign ~ mpg, design = des, family = quasibinomial)))
  expect_true(is.na(sl$F))
})

test_that("5.17: events is stcox's, stcrreg's and streg's e(N_fail) (probes X1-X5)", {
  skip_if_not_installed("survival")
  d <- golden_fixture("cohort")[1:3000, ]
  cw <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d, weights = iptw,
                        ties = "breslow")
  expect_equal(st_stats(cw)$events, st_stata("X1", "N_fail"), tolerance = 1e-12)
  expect_equal(st_stats(cw)$N_sub, st_stata("X1", "N_sub"), tolerance = 1e-12)
  expect_message(tt <- regtab(cw, stats = c("n", "events")), "Events for model(s) 1 is the sum of the failures' weights",
                 fixed = TRUE)
  expect_identical(tail(tt$body[[2]], 1), "2,072")
  c2 <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d, ties = "breslow")
  expect_identical(st_stats(c2)$events, st_stata("X2", "N_fail"))
  d$fw2 <- 1 + seq_len(nrow(d)) %% 3
  sr <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d, weights = fw2,
                          dist = "weibull")
  expect_identical(st_stats(sr)$events, st_stata("X3", "N_fail"))
  d$etype <- factor(d$event_type, 0:2, c("censor", "cv", "death"))
  fg <- survival::finegray(survival::Surv(follow_up, etype) ~ ., data = d, etype = "cv")
  ffg <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ treated + index_age, data = fg, weights = fgwt)
  expect_identical(st_stats(ffg)$events, st_stata("X4", "N_fail"))
  # streg after stset [pw] (a robust weighted survreg; C4 review O1):
  # e(N) the records, e(N_sub) the weight sum, events weighted (probe X5;
  # golden ST06 pins the BIC from e(N)).
  d5 <- golden_fixture("cohort")[1:2000, ]
  s5 <- st_stats(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d5,
                                   dist = "weibull", weights = iptw, robust = TRUE))
  expect_identical(s5$N, st_stata("X5", "N"))
  expect_equal(s5$N_sub, st_stata("X5", "N_sub"), tolerance = 1e-12)
  expect_equal(s5$events, st_stata("X5", "N_fail"), tolerance = 1e-12)
  expect_identical(s5$nsub_note, "weighted")
  # A robust weighted survreg is already Stata's [pweight] reading: no IPTW
  # warning asking for robust = TRUE.
  sr5 <- survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d5,
                           dist = "weibull", weights = iptw, robust = TRUE)
  expect_no_warning(suppressMessages(regtab(sr5)))
  expect_warning(regtab(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = d5,
                                          dist = "weibull", weights = iptw)), "look like inverse-probability weights")
  # intreg/tobit (a gaussian survreg) store no e(N_fail).
  expect_null(st_stats(survival::survreg(survival::Surv(follow_up, cv_event) ~ treated, data = d,
                                         dist = "gaussian"))$events)
})

test_that("5.17: rows come in Stata's order whatever the token order, and only where a model reports them", {
  d <- golden_fixture("auto")
  f1 <- lm(price ~ mpg, data = d)
  f2 <- glm(price ~ mpg, family = gaussian, data = d)
  tt <- regtab(f1, f2, stats = "F fmi rmse r2_a R2 ll AIC mi_m events N")
  lab <- tt$body[[1]]
  expect_identical(lab[(length(lab) - 6):length(lab)],
                   c("Observations", "AIC", "Log-likelihood", "R\u00b2", "Adjusted R\u00b2", "Root MSE", "F statistic"))
  # F, Root MSE and Adjusted R-squared are regress's only: blank for glm
  # (Stata's glm command stores none), in its column.
  expect_identical(tail(tt$body[[5]], 3), c("", "", ""))
  expect_identical(tail(tt$body[[2]], 3), c("0.209", "2623.653", "20.26"))
  st <- tt$stored
  expect_equal(st$F_1, summary(f1)$fstatistic[["value"]], tolerance = 1e-14)
  expect_equal(st$r2_a_1, summary(f1)$adj.r.squared, tolerance = 1e-14)
  expect_true(is.null(st$F_2) && is.null(st$rmse_2) && is.null(st$events_1) && is.null(st$mi_m_1))
  # A row no model reports is left out entirely.
  tl <- regtab(glm(foreign ~ mpg, binomial, d), stats = c("n", "F", "rmse", "r2_a", "events", "mi_m", "fmi"))
  expect_identical(tail(tl$body[[1]], 1), "Observations")
})

test_that("5.17: unknown tokens warn with the 2.1.12 list; tokens are case-insensitive", {
  d <- golden_fixture("auto")
  f <- lm(price ~ mpg, data = d)
  expect_warning(regtab(f, stats = c("n", "c")),
                 "valid: n (n_sub/subjects) events groups mi_m aic bic qic ll icc r2 r2_a rmse F fmi", fixed = TRUE)
  # Each unknown token as typed, once per occurrence, as Stata echoes it.
  w <- character()
  withCallingHandlers(regtab(f, stats = "n AdjR2 AdjR2"),
                      warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
  expect_length(w, 2L)
  expect_true(all(grepl("AdjR2", w, fixed = TRUE)))
  expect_identical(regtab(f, stats = "f RMSE")$body, regtab(f, stats = c("F", "rmse"))$body)
  expect_error(regtab(f, stats = 1), class = "tabtools_error_statspec")
})

test_that("5.17: stat_fun adds R-only rows below Stata's", {
  d <- golden_fixture("auto")
  f1 <- lm(price ~ mpg, data = d)
  f2 <- lm(price ~ mpg + weight, data = d)
  tt <- regtab(f1, f2, stats = "n", stat_fun = list(
    "Residual df" = list(fun = function(fit) fit$df.residual, fmt = "%9.0f"),
    "Sigma" = function(fit) summary(fit)$sigma,
    "Note" = function(fit) if (length(stats::coef(fit)) > 2) "adjusted" else NA,
    "Nothing" = function(fit) NULL
  ), addrow = list("P trend" = c(0.03, 0.04)))
  lab <- tt$body[[1]]
  expect_identical(tail(lab, 5), c("Observations", "Residual df", "Sigma", "Note", "P trend"))
  expect_identical(tail(tt$body[[2]], 4)[1:3], c("72", "2623.653", ""))
  expect_identical(tail(tt$body[[5]], 4)[1:3], c("71", "2514.029", "adjusted"))
  expect_error(regtab(f1, stat_fun = function(fit) 1), "named list of functions")
  expect_error(regtab(f1, stat_fun = list(a = 1)), "must be a function of the fit")
  expect_error(regtab(f1, stat_fun = list(a = list(fun = function(fit) 1, fmt = "%q"))), "Unsupported Stata display format")
  expect_error(regtab(f1, stat_fun = list(a = function(fit) 1:2)), "one number or one string")
  expect_error(regtab(f1, stat_fun = list(a = function(fit) stop("boom"))), "failed for model 1")
})

test_that("5.17: a data frame's glance F shows for Stata's linear commands only (C4 review F9)", {
  df <- data.frame(term = c("x", "(Intercept)"), estimate = c(0.5, 1), std.error = c(0.1, 0.2))
  attr(df, "effect_scale") <- "Coef."
  attr(df, "conf.level") <- .95
  attr(df, "inference_reference") <- "normal"
  mk <- function(cmd) {
    x <- df
    attr(x, "stata_cmd") <- cmd
    attr(x, "effect_scale") <- if (identical(cmd, "logit")) "OR" else "Coef."
    if (identical(cmd, "logit")) attr(x, "se_scale") <- "link"
    attr(x, "glance") <- list(nobs = 100, F = 12.3456, rmse = 2.5, nevent = 40, nimp = 5, fmi = 0.12345)
    x
  }
  lab <- function(tt) tt$body[[1]]
  ti <- regtab(mk("ivregress"), stats = c("n", "events", "mi_m", "rmse", "F", "fmi"))
  expect_true("F statistic" %in% lab(ti))
  expect_identical(tail(ti$body[[2]], 6), c("100", "40", "5", "2.500", "12.35", "0.1235"))
  expect_true("F statistic" %in% lab(regtab(mk("REGRESS"), stats = "F")))
  expect_false("F statistic" %in% lab(regtab(mk("logit"), stats = c("n", "F"))))
})
