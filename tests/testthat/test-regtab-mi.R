# Multiple imputation (task 5.14, conditional task C4): Rubin's rules with
# Stata `mi estimate`'s degrees of freedom, checked against Stata 17's own
# e(b_mi), e(V_mi), e(W_mi), e(B_mi), e(df_mi), e(rvi_mi), e(fmi_mi) and
# r(table) on the golden fixture mi_cohort.dta
# (fixtures/regtab_mi/mi_emat.csv, qa/stata/make_regtab_mi.do). None of
# these tests needs mice; the mira tests at the end skip without it.

mi_data <- function() golden_fixture("mi_cohort", factors = "education")
mi_rhs <- "treated + index_age + female + education"
mi_fit <- function(kind, d = mi_data()) {
  f <- stats::as.formula(paste(if (kind %in% c("logit", "poisson")) "cv_event" else "bmi", "~", mi_rhs))
  golden_mi_fits(d, switch(kind,
    lm = function(x) lm(f, data = x),
    lm2 = function(x) lm(bmi ~ treated + female, data = x),
    glm = function(x) glm(f, family = gaussian, data = x),
    logit = function(x) glm(f, family = binomial, data = x, control = glm.control(epsilon = 1e-14, maxit = 100)),
    logit2 = function(x) glm(cv_event ~ treated + female, family = binomial, data = x),
    poisson = function(x) glm(f, family = poisson, data = x, control = glm.control(epsilon = 1e-14, maxit = 100)),
    cox = function(x) survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age + female + education,
                                      data = x, ties = "breslow")
  ))
}

# Stata's results for one fit of make_regtab_mi.do, R coefficient names.
mi_stata <- function(fit) {
  e <- utils::read.csv(test_path("fixtures", "regtab_mi", "mi_emat.csv"), colClasses = "character")
  e <- e[e$fit == fit, , drop = FALSE]
  strip <- function(s) sub("^.*:", "", s)
  e$row <- strip(e$row)
  e$col <- strip(e$col)
  e$value <- suppressWarnings(as.numeric(ifelse(trimws(e$value) == ".", NA, trimws(e$value))))
  base <- grepl("^[0-9]+b\\.", e$col) | grepl("^[0-9]+b\\.", e$row)
  e <- e[!base, , drop = FALSE]
  map <- c(`_cons` = "(Intercept)", `2.education` = "educationSecondary", `3.education` = "educationTertiary")
  rn <- function(s) ifelse(s %in% names(map), map[s], s)
  e$row <- rn(e$row)
  e$col <- rn(e$col)
  vec <- function(w) {
    x <- e[e$what == w, ]
    stats::setNames(x$value, x$col)
  }
  mat <- function(w) {
    x <- e[e$what == w, ]
    nm <- unique(x$col)
    m <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
    m[cbind(x$row, x$col)] <- x$value
    m
  }
  tab <- e[e$what == "table", ]
  tv <- function(r) stats::setNames(tab$value[tab$row == r], tab$col[tab$row == r])
  sc <- e[e$what == "scalar", ]
  list(b = vec("b"), df = vec("df"), rvi = vec("rvi"), fmi = vec("fmi"), V = mat("V"), W = mat("W"),
       B = mat("B"), ll = tv("ll"), ul = tv("ul"), p = tv("pvalue"),
       scalar = stats::setNames(sc$value, sc$row))
}

# Largest relative difference over a vector or matrix, the scale being the
# largest magnitude Stata reports.
mi_reldiff <- function(got, want) {
  got <- unname(got)
  want <- unname(want)
  ok <- is.finite(want)
  expect_identical(is.finite(got), ok)
  max(abs(got[ok] - want[ok])) / max(abs(want[ok]), 1e-300)
}

expect_mi_stata <- function(fit, fits, vce = "stata", cluster = NULL, tol_b = 1e-12, tol_df = 1e-11, level = 0.95) {
  s <- mi_stata(fit)
  p <- tabtools:::.rt_mi_pool(fits, vce, cluster, level)
  nm <- names(s$b)
  expect_setequal(p$term, nm)
  o <- match(nm, p$term)
  V <- attr(p, "V")[nm, nm]
  W <- attr(p, "W")[nm, nm]
  B <- attr(p, "B")[nm, nm]
  expect_lt(mi_reldiff(p$estimate[o], s$b), tol_b)
  expect_lt(mi_reldiff(V, s$V[nm, nm]), tol_b)
  expect_lt(mi_reldiff(W, s$W[nm, nm]), tol_b)
  expect_lt(mi_reldiff(B, s$B[nm, nm]), 100 * tol_b)
  df <- p$df[o]
  sdf <- unname(s$df)
  sdf[is.na(sdf)] <- Inf
  expect_identical(is.finite(df), is.finite(sdf))
  if (any(is.finite(sdf))) expect_lt(mi_reldiff(df[is.finite(sdf)], sdf[is.finite(sdf)]), tol_df)
  expect_lt(max(abs(p$riv[o] - unname(s$rvi))), tol_df)
  expect_lt(max(abs(p$fmi[o] - unname(s$fmi))), tol_df)
  expect_lt(mi_reldiff(p$conf.low[o], s$ll), tol_b * 10)
  expect_lt(mi_reldiff(p$conf.high[o], s$ul), tol_b * 10)
  expect_lt(max(abs(p$p.value[o] - unname(s$p)) / pmax(unname(s$p), 1e-300)), tol_df * 10)
  expect_equal(max(p$fmi), s$scalar[["fmi_max_mi"]], tolerance = tol_df)
  expect_identical(attr(p, "M"), as.integer(s$scalar[["M_mi"]]))
  invisible(p)
}

test_that("5.14: Rubin's rules and Barnard-Rubin df reproduce mi estimate: regress (plain, robust, cluster)", {
  d <- mi_data()
  fits <- mi_fit("lm", d)
  p <- expect_mi_stata("A1", fits)
  # Small-sample df: finite, below the complete-data df N - k = 394.
  expect_true(all(is.finite(p$df) & p$df < 394))
  expect_identical(attr(p, "df_c"), 394)
  expect_mi_stata("A6", fits, vce = "robust")
  # Cluster: complete-data df G - 1 = 5, a cluster vector per imputation
  # (as regtab() resolves `cluster = ~region` in each; golden MI06), or one
  # vector for all of them.
  cl <- structure(lapply(1:5, function(m) golden_mi_data(d, m)$region), class = "tt_mi_cluster")
  p5 <- expect_mi_stata("A5", fits, vce = "cluster", cluster = cl)
  expect_identical(attr(p5, "df_c"), 5)
  pv <- tabtools:::.rt_mi_pool(fits, "cluster", golden_mi_data(d, 1)$region)
  expect_equal(pv$conf.low, p5$conf.low, tolerance = 1e-14)
})

test_that("5.14: intervals at level(90) are mi estimate, level(90)'s (C4 review F3)", {
  d <- mi_data()
  fits <- mi_fit("lm", d)
  p90 <- expect_mi_stata("A11", fits, level = 0.9)
  p95 <- tabtools:::.rt_mi_pool(fits)
  expect_true(all(p90$conf.high - p90$conf.low < p95$conf.high - p95$conf.low))
  expect_equal(p90$conf.high - p90$estimate, stats::qt(0.95, p90$df) * p90$std.error, tolerance = 1e-14)
  tt <- regtab(tt_mi(fits), level = 0.9)
  r <- tt$meta$regtab_rows
  est <- r$status == "est"
  hit <- match(c("treated", "index_age", "female", "educationSecondary", "educationTertiary", "(Intercept)"), p90$term)
  expect_equal(r$conf.low[est], p90$conf.low[hit], tolerance = 1e-14)
  expect_equal(r$conf.high[est], p90$conf.high[hit], tolerance = 1e-14)
  expect_identical(tt$header[[2]]$text[3], "90% CI")
})

test_that("5.14: large-sample df where the complete-data df is infinite (logit, glm, poisson, stcox)", {
  d <- mi_data()
  # glm (Stata's glm stores no e(df_r)): Rubin's large-sample df.
  p9 <- expect_mi_stata("A9", mi_fit("glm", d))
  expect_true(is.infinite(attr(p9, "df_c")))
  M <- 5
  r <- p9$riv
  expect_equal(p9$df, (M - 1) * (1 + 1 / r)^2, tolerance = 1e-14)
  # logit, poisson and stcox agree to their fits' convergence (Stata's ml
  # stops ~1e-8 from the optimum; the df, a function of B, moves ~100x more).
  logit <- mi_fit("logit", d)
  expect_mi_stata("A2", logit, tol_b = 5e-8, tol_df = 5e-6)
  expect_mi_stata("A3", logit, vce = "robust", tol_b = 5e-8, tol_df = 5e-6)
  expect_mi_stata("A10", mi_fit("poisson", d), tol_b = 5e-8, tol_df = 5e-6)
  skip_if_not_installed("survival")
  expect_mi_stata("A4", mi_fit("cox", d), tol_b = 5e-8, tol_df = 5e-6)
})

test_that("5.14: a coefficient with no between-imputation variance (B = 0) has FMI 0, as mi estimate", {
  d <- mi_data()
  # regress: df = nu_obs = nu_c (nu_c + 1) / (nu_c + 3) exactly.
  p7 <- expect_mi_stata("A7", mi_fit("lm2", d))
  expect_identical(p7$fmi, c(0, 0, 0))
  expect_equal(p7$df, rep(397 * 398 / 400, 3), tolerance = 1e-15)
  expect_identical(diag(attr(p7, "B")), c(`(Intercept)` = 0, treated = 0, female = 0))
  # logit: infinite df, normal intervals (Stata: df ".", crit 1.959964).
  p8 <- expect_mi_stata("A8", mi_fit("logit2", d), tol_b = 5e-8, tol_df = 5e-6)
  expect_true(all(is.infinite(p8$df)))
  expect_equal(p8$conf.high - p8$estimate, stats::qnorm(0.975) * p8$std.error, tolerance = 1e-14)
})

test_that("5.14: regtab() rows carry the pooled numbers on the display scale", {
  d <- mi_data()
  fits <- mi_fit("logit", d)
  tt <- regtab(tt_mi(fits), stats = c("n", "mi_m", "fmi", "ll", "aic", "bic", "r2", "rmse", "F"))
  p <- tabtools:::.rt_mi_pool(fits)
  r <- tt$meta$regtab_rows
  est <- r$status == "est"
  hit <- match(c("treated", "index_age", "female", "educationSecondary", "educationTertiary"), p$term)
  expect_equal(r$estimate[est], exp(p$estimate[hit]), tolerance = 1e-14)
  expect_equal(r$conf.low[est], exp(p$conf.low[hit]), tolerance = 1e-14)
  expect_equal(r$p.value[est], p$p.value[hit], tolerance = 1e-14)
  # Stats: counts, Imputations and Largest FMI; nothing from the likelihood.
  expect_identical(tail(tt$body[[1]], 3), c("Observations", "Imputations", "Largest FMI"))
  expect_identical(tt$stored$mi_m_1, 5)
  expect_identical(tt$stored$fmi_1, max(p$fmi))
  expect_null(tt$stored$ll_1)
  expect_null(tt$stored$aic_1)
  expect_match(tt$stored$methods, "from multivariable logistic regression with multiple imputation.", fixed = TRUE)
  # tt_coef()/tt_vcov() of a tt_mi object are the pooled estimate and total
  # variance.
  expect_equal(unname(tabtools:::tt_coef(tt_mi(fits))[p$term]), p$estimate, tolerance = 1e-15)
  expect_equal(tt_vcov(tt_mi(fits))[p$term, p$term], attr(p, "V"), tolerance = 1e-15)
  expect_error(tabtools:::tt_wald_df(tt_mi(fits)), "one degrees-of-freedom value per coefficient")
})

test_that("5.14: an MI model beside a single fit, a list of MI models, and the side-by-side escape", {
  d <- mi_data()
  fits <- mi_fit("lm", d)
  single <- fits[[1]]
  tt <- regtab(single, tt_mi(fits), stats = c("n", "mi_m", "fmi", "r2"))
  expect_identical(tt$body[[2]][tt$body[[1]] == "Imputations"], "")
  expect_identical(tt$body[[5]][tt$body[[1]] == "Imputations"], "5")
  expect_identical(tt$body[[2]][tt$body[[1]] == "R\u00b2"], "0.011")
  expect_identical(tt$body[[5]][tt$body[[1]] == "R\u00b2"], "")
  tl <- regtab(list(A = tt_mi(fits), B = tt_mi(mi_fit("lm2", d))))
  expect_identical(tl$header[[1]]$text[c(2, 5)], c("A", "B"))
  # A plain list of the imputations still shows them side by side.
  expect_identical(regtab(fits)$stored$N_models, 5L)
})

test_that("5.14: stats of an MI model: counts that vary are blank; Events for Cox; stat_fun gets the tt_mi", {
  skip_if_not_installed("survival")
  d <- mi_data()
  cx <- tt_mi(mi_fit("cox", d))
  tt <- regtab(cx, stats = c("n", "events", "mi_m", "fmi", "ll"),
               stat_fun = list(M = function(fit) length(fit$analyses)))
  expect_identical(tail(tt$body[[1]], 5), c("Subjects", "Events", "Imputations", "Largest FMI", "M"))
  expect_identical(tail(tt$body[[2]], 5), c("400", "133", "5", "0.2558", "5.000"))
  expect_identical(tt$stored$events_1, 133)
  s <- tabtools:::tt_model_stats.tt_mi(cx, tabtools:::tt_model_info(cx))
  expect_true(is.na(s$ll) && is.na(s$aic) && is.na(s$rmse) && is.na(s$F))
})

test_that("5.14: gate: M < 2, mixed classes, unpooled classes, and designs that differ are refused", {
  d <- mi_data()
  fits <- mi_fit("lm", d)
  expect_error(tt_mi(fits[1]), "at least 2 imputations")
  expect_error(tt_mi(fits[[1]]), "must be a list of fitted models")
  expect_error(regtab(structure(list(analyses = fits[1]), class = "tt_mi")), "at least 2 imputations")
  expect_error(regtab(tt_mi(list(fits[[1]], glm(bmi ~ treated, data = golden_mi_data(d, 2))))),
               "different classes")
  skip_if_not_installed("MASS")
  nb <- golden_mi_fits(d, function(x) suppressWarnings(MASS::glm.nb(cv_event ~ treated, data = x)))
  expect_error(regtab(tt_mi(nb)), "does not pool multiply imputed <negbin> fits")
  # Another formula in one imputation.
  f2 <- fits
  f2[[3]] <- lm(bmi ~ treated + index_age + female, data = golden_mi_data(d, 3))
  expect_error(regtab(tt_mi(f2)), "imputation 3 is not the same model")
  # A factor level missing in one imputation (a rare imputed category).
  x4 <- golden_mi_data(d, 4)
  x4 <- x4[x4$education != "Tertiary", ]
  x4$education <- droplevels(x4$education)
  f3 <- fits
  f3[[4]] <- lm(bmi ~ treated + index_age + female + education, data = x4)
  expect_error(regtab(tt_mi(f3)), "different coefficients (educationTertiary)", fixed = TRUE)
  # A coefficient omitted (collinear) in one imputation only.
  set.seed(5)
  z <- stats::rnorm(400)
  f4 <- golden_mi_fits(d, function(x) {
    x$z <- if (identical(unique(as.vector(x$mi_m)), 5)) as.vector(x$treated) else z
    lm(bmi ~ treated + z, data = x)
  })
  expect_error(regtab(tt_mi(f4)), "omitted (aliased) coefficients differ between imputations 1 and 5 (z)", fixed = TRUE)
  # An estimation sample that varies.
  f5 <- fits
  f5[[2]] <- lm(bmi ~ treated + index_age + female + education, data = golden_mi_data(d, 2)[-1, ])
  expect_error(regtab(tt_mi(f5)), "number of observations differs between imputations 1 (400) and 2 (399)",
               fixed = TRUE)
  # Not a fit.
  expect_error(regtab(tt_mi(list(fits[[1]], "x"))), "imputation 2")
  expect_error(regtab(tt_mi(fits), ci_method = "profile"), "not available")
})

test_that("5.14: cluster vectors that differ between imputations: each imputation's own, nu_c the smallest G - 1", {
  d <- mi_data()
  fits <- mi_fit("lm", d)
  reg <- as.vector(golden_mi_data(d, 1)$region)
  # Imputation 3 clusters coarser (3 clusters instead of 6).
  cl <- lapply(1:5, function(m) if (m == 3) (reg %% 3) + 1 else reg)
  p <- tabtools:::.rt_mi_pool(fits, "cluster", structure(cl, class = "tt_mi_cluster"))
  expect_identical(attr(p, "df_c"), 2)
  U <- lapply(1:5, function(m) tt_vcov(fits[[m]], "cluster", cl[[m]])[p$term, p$term])
  expect_equal(attr(p, "W"), Reduce(`+`, U) / 5, tolerance = 1e-14)
  # Not imputation 1's vector for all.
  p1 <- tabtools:::.rt_mi_pool(fits, "cluster", reg)
  expect_false(isTRUE(all.equal(attr(p, "W"), attr(p1, "W"))))
  expect_identical(attr(p1, "df_c"), 5)
})

test_that("5.14: counts that differ between imputations are blank (an imputed event indicator)", {
  skip_if_not_installed("survival")
  d <- mi_data()
  set.seed(7)
  flip <- sample(400, 20)
  fits <- golden_mi_fits(d, function(x) {
    if (identical(unique(as.vector(x$mi_m)), 2)) x$cv_event[flip] <- 1 - x$cv_event[flip]
    survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age, data = x, ties = "breslow")
  })
  tt <- regtab(tt_mi(fits), stats = c("n", "events", "mi_m"))
  expect_false("Events" %in% tt$body[[1]])
  expect_null(tt$stored$events_1)
  expect_identical(tt$stored$n_1, 400)
})

test_that("5.14: the gate refuses imputations of different families (poisson vs quasipoisson)", {
  d <- mi_data()
  fits <- golden_mi_fits(d, function(x) {
    fam <- if (identical(unique(as.vector(x$mi_m)), 4)) quasipoisson else poisson
    glm(cv_event ~ treated, family = fam, data = x)
  })
  expect_error(regtab(tt_mi(fits)), "imputation 4 is not the same model")
})

test_that("5.14: one pool per regtab() call, shared by the rows and the stats (C4 review F10)", {
  x <- tt_mi(mi_fit("lm2"))
  x$pool_cache <- new.env(parent = emptyenv())
  p1 <- tabtools:::.rt_mi_pool_cached(x, "robust", NULL, 0.95)
  p2 <- tabtools:::.rt_mi_pool_cached(x, "robust", NULL, 0.9, any_level = TRUE)
  expect_identical(p1, p2)
  expect_length(x$pool_cache$entries, 1L)
  tabtools:::.rt_mi_pool_cached(x, "robust", NULL, 0.9)
  expect_length(x$pool_cache$entries, 2L)
  # Without a cache (a tt_mi() outside regtab()) the pool is computed.
  expect_equal(tabtools:::.rt_mi_pool_cached(tt_mi(mi_fit("lm2")), "robust"), p1)
})

test_that("5.14: coefficients aliased in every imputation keep an Omitted row and are not pooled", {
  d <- mi_data()
  fits <- golden_mi_fits(d, function(x) {
    x$treated2 <- as.vector(x$treated)
    lm(bmi ~ treated + treated2 + female, data = x)
  })
  tt <- regtab(tt_mi(fits))
  expect_identical(tt$body[[2]][tt$body[[1]] == "treated2"], "Omitted")
  p <- tabtools:::.rt_mi_pool(fits)
  expect_false("treated2" %in% p$term)
  ref <- tabtools:::.rt_mi_pool(mi_fit("lm2", d))
  expect_equal(p$conf.low, ref$conf.low, tolerance = 1e-12)
})

test_that("5.14: a pooled mice result (mipo) is refused with a hint to pass the mira", {
  mipo <- structure(list(m = 5L, pooled = data.frame(term = "wt", estimate = -5.3)), class = c("mipo", "data.frame"))
  err <- tryCatch(regtab(mipo), error = function(e) conditionMessage(e))
  expect_match(err, "does not take a pooled mice result (<mipo>, model 1)", fixed = TRUE)
  expect_match(err, "Pass the <mira> instead", fixed = TRUE)
})

test_that("5.14: print(tt_mi)", {
  fits <- mi_fit("lm2")
  expect_output(print(tt_mi(fits)), "<tt_mi> 5 imputations of a <lm> fit")
})

# ---------------------------------------------------------------------------
# mice (Suggests): a mira from with(<mids>, ...) is pooled as tt_mi() of its
# analyses; mice::pool() agrees for lm (Barnard-Rubin with df.residual, as
# Stata) and differs for logit (Stata: large-sample df).

mi_mids <- function(d) {
  long <- d
  long$.imp <- long$mi_m
  long$.id <- long$id
  long[] <- lapply(long, function(x) if (is.factor(x)) x else as.vector(x))
  # as.mids() warns about mice's "logged events" (the constant id and mi_m
  # columns): expected, not a test outcome.
  suppressWarnings(mice::as.mids(long, .imp = ".imp", .id = ".id"))
}

test_that("5.14: regtab(mira) pools the mice analyses as tt_mi() does", {
  skip_if_not_installed("mice")
  d <- mi_data()
  imp <- mi_mids(d)
  ml <- with(imp, lm(bmi ~ treated + index_age + female + education))
  expect_s3_class(ml, "mira")
  tt <- regtab(ml, stats = c("n", "mi_m", "fmi"))
  ref <- regtab(tt_mi(mi_fit("lm", d)), stats = c("n", "mi_m", "fmi"))
  # mice's completed data carry no variable labels: compare the numbers.
  expect_identical(tt$body[-1], ref$body[-1])
  # mice::pool() for lm: same estimates, standard errors and df.
  pm <- summary(mice::pool(ml))
  p <- tabtools:::.rt_mi_pool(ml$analyses)
  expect_equal(p$estimate, pm$estimate, tolerance = 1e-12)
  expect_equal(p$std.error, pm$std.error, tolerance = 1e-12)
  expect_equal(p$df, pm$df, tolerance = 1e-10)
  # Logit: mice applies Barnard-Rubin with df.residual; Stata does not.
  mg <- with(imp, glm(cv_event ~ treated + index_age + female + education, family = binomial))
  pg <- summary(mice::pool(mg))
  q <- tabtools:::.rt_mi_pool(mg$analyses)
  expect_equal(q$estimate, pg$estimate, tolerance = 1e-12)
  expect_true(all(q$df > pg$df))
  # A mira beside another model, and a Cox mira (its fits keep no data).
  expect_identical(regtab(ml$analyses[[1]], ml)$stored$N_models, 2L)
  skip_if_not_installed("survival")
  mc <- with(imp, survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age + female + education,
                                  ties = "breslow"))
  tc <- regtab(mc, stats = c("n", "events", "fmi"))
  rc <- regtab(tt_mi(mi_fit("cox", d)), stats = c("n", "events", "fmi"))
  expect_identical(tc$body[-1], rc$body[-1])
  expect_error(regtab(mice::pool(ml)), "Pass the <mira> instead", fixed = TRUE)
})

test_that("5.14: cluster = ~var on a mira reads each completed dataset (C4 review F1)", {
  skip_if_not_installed("mice")
  d <- mi_data()
  imp <- mi_mids(d)
  reg <- as.vector(golden_mi_data(d, 1)$region)
  # lm: MI06's numbers (tt_mi() of fits on labelled data), as a vector too.
  ml <- with(imp, lm(bmi ~ treated + index_age + female + education))
  a <- regtab(ml, vce = "cluster", cluster = ~region, stats = c("n", "mi_m", "fmi"))
  ref <- regtab(tt_mi(mi_fit("lm", d)), vce = "cluster", cluster = reg, stats = c("n", "mi_m", "fmi"))
  expect_identical(a$body[-1], ref$body[-1])
  expect_identical(tail(a$body[[2]], 1), "0.2967")
  mg <- with(imp, glm(cv_event ~ treated + index_age, family = binomial))
  expect_identical(regtab(mg, vce = "cluster", cluster = ~region)$body,
                   regtab(mg, vce = "cluster", cluster = reg)$body)
  skip_if_not_installed("survival")
  mc <- with(imp, survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age, ties = "breslow"))
  expect_identical(regtab(mc, vce = "cluster", cluster = ~region)$body,
                   regtab(mc, vce = "cluster", cluster = reg)$body)
  # A variable the completed data do not have is still an error.
  expect_error(regtab(ml, vce = "cluster", cluster = ~nosuch), "not in the model's data")
})
