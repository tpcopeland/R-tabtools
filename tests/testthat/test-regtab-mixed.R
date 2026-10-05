# Phase 5b (plan task 5.5): regtab with lme4 and glmmTMB mixed models.
# Stata expectations: goldens R21, R22, R25f (test-golden-regtab.R) and
# tests/testthat/fixtures/regtab_phase5b/ (qa/stata/make_regtab_phase5b.do):
# regtab's own CSV sinks, r(table) of each model, and regtab's r()
# results, from Stata 17 with tabtools 2.1.12 (regenerated in C2 and C5).

p5b <- function(name) test_path("fixtures", "regtab_phase5b", name)

p5b_params <- function(model) {
  d <- utils::read.csv(p5b("re_params.csv"), stringsAsFactors = FALSE, strip.white = TRUE,
                       na.strings = c(".", ".b"))
  d <- d[d$model == model, , drop = FALSE]
  rownames(d) <- d$param
  d
}

p5b_stats <- function(call) {
  d <- utils::read.csv(p5b("re_stats.csv"), stringsAsFactors = FALSE, strip.white = TRUE)
  d[d$call == call, , drop = FALSE]
}

# regtab's CSV sink against Stata's; `drop` removes R lines Stata lacks.
expect_p5b_csv <- function(tt, tag, drop = NULL) {
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, f)
  got <- readLines(f)
  if (!is.null(drop)) got <- got[-drop]
  expect_identical(got, readLines(p5b(paste0(tag, ".csv"))))
}

re_rows <- function(tt) {
  r <- tt$meta$regtab_rows
  r[r$type == "re", , drop = FALSE]
}

mixed_bp <- function() golden_fixture("mixed_bp")

nhanes_zone <- function() {
  n <- golden_fixture("nhanes2", factors = "sex")
  n$zone <- ceiling(n$location / 10)
  attr(n$zone, "label") <- "Zone"
  n
}

slope_data <- function() as.data.frame(haven::read_dta(p5b("slope.dta")))

# broom.helpers' glmmTMB model matrix calls lme4::nobars(), which recent
# lme4 versions deprecate with a once-per-session warning (not regtab's).
quiet_nobars <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("nobars", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  })
}

# ---------------------------------------------------------------------------
# Formulas

test_that("median odds ratio and ICC formulas follow regtab.ado", {
  # MOR = exp(sqrt(2 var) invnormal(0.75)) (`regtab.ado:2102-2106`); R22's
  # variance .22076060949696699 gives Stata's r(table) value.
  expect_equal(tabtools:::.rt_mor(.22076060949696699), 1.5654583079935021, tolerance = 1e-14)
  expect_equal(tabtools:::.rt_mor(c(0, -1, NA)), c(1, NA, NA))
  re1 <- list(terms = list(list(comps = "(Intercept)", Sigma = matrix(0.5))), resid = NA_real_)
  icc <- function(info) tabtools:::.rt_re_icc(re1, info)$icc
  expect_equal(icc(list(icc_resid = pi^2 / 3)), 0.5 / (0.5 + pi^2 / 3))
  expect_equal(icc(list(icc_resid = 1)), 0.5 / 1.5)
  expect_equal(icc(list(icc_resid = pi^2 / 6)), 0.5 / (0.5 + pi^2 / 6))
  # Count families: undefined, with a note.
  cnt <- tabtools:::.rt_re_icc(re1, list(icc_undefined = TRUE, icc_resid = NA_real_))
  expect_true(is.na(cnt$icc) && cnt$note)
  # Every random-intercept variance is summed (multi-level); slopes are not.
  re2 <- list(terms = list(list(comps = "(Intercept)", Sigma = matrix(0.2)),
                           list(comps = c("(Intercept)", "x"), Sigma = matrix(c(0.3, 0.1, 0.1, 5), 2))),
              resid = 2)
  expect_equal(tabtools:::.rt_re_icc(re2, list())$icc, 0.5 / 2.5)
  re3 <- list(terms = list(list(comps = "x", Sigma = matrix(0.2))), resid = 2)
  expect_true(is.na(tabtools:::.rt_re_icc(re3, list())$icc))
})

test_that("Stata's variance parameters map to lme4's theta and back", {
  S <- matrix(c(0.5, -0.1, 0.02, -0.1, 0.3, 0.05, 0.02, 0.05, 0.8), 3)
  ps <- tabtools:::.rt_re_psi(S)
  expect_equal(ps$lnsd, 0.5 * log(diag(S)))
  expect_equal(ps$pairs, cbind(row = c(1L, 1L, 2L), col = c(2L, 3L, 3L)))
  th <- tabtools:::.rt_re_theta(ps$lnsd, ps$atr, ps$pairs, sig = 2)
  L <- matrix(0, 3, 3)
  L[lower.tri(L, diag = TRUE)] <- th
  expect_equal(4 * tcrossprod(L), S)
})

test_that("the numerical Hessian is accurate", {
  f <- function(x) exp(x[1]) + x[1]^2 * x[2]^3 + sin(x[2])
  x <- c(0.3, -0.7)
  H <- matrix(c(exp(0.3) + 2 * (-0.7)^3, 6 * 0.3 * (-0.7)^2, 6 * 0.3 * (-0.7)^2,
                6 * 0.3^2 * (-0.7) - sin(-0.7)), 2)
  expect_equal(tabtools:::.rt_num_hessian(f, x, c(0.01, 0.01)), H, tolerance = 1e-8)
})

test_that("random-effects r(table) names follow Stata's row-name sanitiser (2.1.11)", {
  rn <- tabtools:::.rt_rowname
  # A covariance would be read back as a variance, and a second bracket group
  # is rejected: each such name alone is sanitised (2.1.9 stored
  # "var(age_cons)" and sent the whole matrix to r1, r2, ...).
  expect_identical(rn(c("var(_cons)", "cov(age,_cons)", "var(e)"), 1:3),
                   c("var(_cons)", "cov_age_cons_", "var(e)"))
  expect_identical(rn(c("x", "var(x[clinic])", "cov(x[clinic],_cons[clinic])"), 1:3),
                   c("x", "var(x[clinic])", "cov_x_clinic__cons_clinic__"))
  expect_identical(rn("Covariance: Clinic ID (Exposure score, Intercept)", 1), "Covariance_Clinic_ID_(Exposure_s")
  expect_identical(rn("var(_cons[location])", 1), "var(_cons[location])")
})

# ---------------------------------------------------------------------------
# lmer (Stata mixed)

test_that("lmer random slope: rows, labels, CIs and r(table) match Stata", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  # bobyqa reaches Stata's optimum (ll -838.45135); lme4's default optimizer
  # stops short on this unscaled slope.
  # (lme4's convergence check warns about the unscaled age slope even so.)
  f <- suppressWarnings(lme4::lmer(y ~ age + female + bmi + (1 + age | region), data = mixed_bp(),
                                   REML = FALSE, control = lme4::lmerControl(optimizer = "bobyqa")))
  expect_equal(as.numeric(logLik(f)), -838.45135, tolerance = 1e-7)
  raw <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"))
  expect_p5b_csv(raw, "slope_raw")
  rel <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"), relabel = TRUE)
  expect_p5b_csv(rel, "slope_relabel")
  st <- p5b_stats("slope_raw")
  expect_identical(rownames(raw$stored$table), st$value[st$kind == "rowname"])
  st <- p5b_stats("slope_relabel")
  expect_identical(rownames(rel$stored$table), st$value[st$kind == "rowname"])
  expect_equal(raw$stored$icc_1, as.numeric(st$value[st$name == "icc_1"]), tolerance = 1e-4)
  # Estimates, CI bounds and the covariance p-value at full precision.
  sp <- p5b_params("mixed_slope")
  r <- re_rows(raw)
  want <- sp[c("region:var(age)", "region:var(_cons)", "region:cov(age,_cons)", "Residual:var(e)"), ]
  expect_identical(r$key, c("var(age)", "var(_cons)", "cov(age,_cons)", "var(e)"))
  expect_equal(r$estimate, want$b, tolerance = 1e-4)
  expect_equal(r$conf.low, want$ll, tolerance = 1e-3)
  expect_equal(r$conf.high, want$ul, tolerance = 1e-3)
  expect_equal(r$p.value[3], want$p[3], tolerance = 1e-3)
  expect_true(all(is.na(r$p.value[-3])))
  # Row types: RE rows follow the intercept; type "re" draws the top border.
  expect_identical(raw$rows$type[5:8], rep("re", 4))
  expect_identical(raw$body[4, 1], "Intercept")
  # keep()/drop() match RE rows by raw key, as Stata does.
  expect_p5b_csv(regtab(f, coef = "Coef.", keep = "age"), "slope_keep")
  expect_p5b_csv(regtab(f, coef = "Coef.", drop = "age"), "slope_drop")
  # noreeffects drops the rows but not the ICC.
  expect_p5b_csv(regtab(f, coef = "Coef.", noreeffects = TRUE, stats = "icc"), "slope_nore")
})

test_that("lmer REML: variance CIs from the REML criterion", {
  skip_if_not_installed("lme4")
  f <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp(), REML = TRUE)
  tt <- regtab(f, coef = "Coef.", stats = c("n", "groups", "aic", "icc"), relabel = TRUE)
  expect_p5b_csv(tt, "reml_relabel")
  sp <- p5b_params("mixed_reml")
  r <- re_rows(tt)
  expect_equal(r$conf.low, sp[c("region:var(_cons)", "Residual:var(e)"), "ll"], tolerance = 1e-4)
  expect_equal(r$conf.high, sp[c("region:var(_cons)", "Residual:var(e)"), "ul"], tolerance = 1e-4)
  st <- p5b_stats("reml_relabel")
  expect_equal(tt$stored$aic_1, as.numeric(st$value[st$name == "aic_1"]), tolerance = 1e-9)
})

test_that("lmer levels written outer:inner are named by their own variable, never merged (task 5.19 review P3-5)", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  n <- nhanes_zone()
  S <- c("n", "groups", "icc")
  a <- regtab(lme4::lmer(bmi ~ age + sex + (1 | zone) + (1 | zone:location), data = n, REML = FALSE),
              coef = "Coef.", stats = S, relabel = TRUE)
  b <- regtab(lme4::lmer(bmi ~ age + sex + (1 | zone/location), data = n, REML = FALSE),
              coef = "Coef.", stats = S, relabel = TRUE)
  expect_identical(a$body, b$body)
  expect_p5b_csv(regtab(lme4::lmer(bmi ~ age + sex + (1 | zone) + (1 | zone:location), data = n, REML = FALSE),
                        coef = "Coef.", stats = S), "lvl3_raw")
  # A slope at the inner level: its variance, covariance and intercept
  # variance are the location level's (the rows of var(_cons[location])
  # were merged into zone's key and dropped before).
  s <- regtab(lme4::lmer(bmi ~ age + sex + (1 | zone) + (1 + age | zone:location), data = n, REML = FALSE,
                         control = lme4::lmerControl(optimizer = "bobyqa")), stats = S)
  expect_identical(re_rows(s)$key, c("var(_cons[zone])", "var(age[location])", "var(_cons[location])",
                                     "cov(age,_cons[location])", "var(e)"))
  expect_identical(tabtools:::.rt_re_group_vars(c("location:zone", "zone")), c("location", "zone"))
  expect_identical(tabtools:::.rt_re_group_vars(c("location:zone:z2", "zone:z2", "z2")), c("location", "zone", "z2"))
  expect_identical(tabtools:::.rt_re_group_vars(c("zone:location", "zone")), c("location", "zone"))
  # Two levels that would share a variable are refused, not merged.
  expect_error(tabtools:::.rt_re_group_vars(c("a", "c", "a:b", "c:b")), "named by the same variable", fixed = TRUE)
  # Crossed levels with their interaction: refused with its own message,
  # which does not suggest nesting (verification review P3b).
  set.seed(3)
  n$grp <- sample(1:6, nrow(n), TRUE)
  err <- tryCatch(regtab(suppressMessages(lme4::lmer(bmi ~ age + (1 | location) + (1 | grp) + (1 | location:grp),
                                                     data = n, REML = FALSE))), error = function(e) e)
  expect_match(conditionMessage(err), "crossed random effects with their interaction", fixed = TRUE)
  expect_false(grepl("outer/inner", conditionMessage(err), fixed = TRUE))
  # A lone interaction level is named by its whole name, not by one of its
  # parts (its 62 groups are locations, not zones; review P3c).
  expect_identical(tabtools:::.rt_re_group_vars("zone:location"), "zone:location")
  lone <- regtab(lme4::lmer(bmi ~ age + (1 | zone:location), data = n, REML = FALSE), stats = "groups", relabel = TRUE)
  expect_identical(re_rows(lone)$label[1], "Variance: zone:location (Intercept)")
  expect_identical(lone$stored$groups_1, 62)
})

test_that("lmer at a correlation of -1 (rounding beyond it): a boundary term, no NaN warning (task 5.19 review 3, P3-2)", {
  skip_if_not_installed("lme4")
  set.seed(1010)
  G <- 30
  m <- 8
  d <- data.frame(g = factor(rep(1:G, each = m)), x = rep(0:(m - 1), G))
  d$y <- 10 + 0.5 * d$x + rnorm(G)[d$g] + rnorm(G, 0, 0)[d$g] * d$x + rnorm(G * m)
  l <- suppressWarnings(suppressMessages(lme4::lmer(y ~ x + (x | g), data = d)))
  expect_no_warning(tt <- regtab(l))
  r <- re_rows(tt)
  expect_true(all(is.na(r$conf.low[1:3])))
  expect_false(is.na(r$conf.low[4]))
  ps <- tabtools:::.rt_re_psi(matrix(c(1, -1 - 1e-12, -1 - 1e-12, 1), 2))
  expect_identical(ps$atr, -Inf)
})

test_that("three-level lmer: outermost level first, bracketed names", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  f <- lme4::lmer(bmi ~ age + sex + (1 | zone/location), data = nhanes_zone(), REML = FALSE)
  raw <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"))
  # The "Sex" header row of a factor in a multi-level layout: Stata 2.1.9
  # lost it, 2.1.11 keeps it (take_action item 1, fixed), as R always did.
  expect_identical(raw$body[2, 1], "Sex")
  expect_p5b_csv(raw, "lvl3_raw")
  expect_p5b_csv(regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"), relabel = TRUE),
                 "lvl3_relabel")
  sp <- p5b_params("mixed_3lvl")
  r <- re_rows(raw)
  expect_identical(r$key, c("var(_cons[zone])", "var(_cons[location])", "var(e)"))
  want <- sp[c("zone:var(_cons)", "location:var(_cons)", "Residual:var(e)"), ]
  expect_equal(r$conf.low, want$ll, tolerance = 1e-3)
  expect_equal(r$conf.high, want$ul, tolerance = 1e-3)
  st <- p5b_stats("lvl3_raw")
  expect_equal(raw$stored$icc_1, as.numeric(st$value[st$name == "icc_1"]), tolerance = 1e-4)
  expect_equal(raw$stored$groups_1, 62)
})

test_that("lmer on simulated random-slope data matches Stata mixed", {
  skip_if_not_installed("lme4")
  f <- lme4::lmer(yc ~ x + z + (1 + x | clinic), data = slope_data(), REML = FALSE)
  tt <- regtab(f, stats = "icc", relabel = TRUE)
  expect_p5b_csv(tt, "slope2_relabel")
  sp <- p5b_params("mixed_slope2")
  r <- re_rows(tt)
  want <- sp[c("clinic:var(x)", "clinic:var(_cons)", "clinic:cov(x,_cons)", "Residual:var(e)"), ]
  expect_equal(r$conf.low, want$ll, tolerance = 1e-4)
  expect_equal(r$conf.high, want$ul, tolerance = 1e-4)
  expect_equal(r$p.value[3], want$p[3], tolerance = 1e-4)
  # Fixed effects: lmer's GLS variance is Stata mixed's.
  w <- tabtools:::tt_wald(f)
  expect_equal(w$std.error, sp[c("yc:_cons", "yc:x", "yc:z"), "se"], tolerance = 1e-5)
})

# ---------------------------------------------------------------------------
# glmer (Stata me*)

test_that("glmer: full observed information for fixed effects and the MOR", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  g <- lme4::glmer(highbp ~ age + sex + (1 | location), family = binomial, data = nhanes_zone(), nAGQ = 7)
  sp <- p5b_params("melogit")
  w <- tabtools:::tt_wald(g)
  expect_equal(w$std.error, sp[c("highbp:_cons", "highbp:age", "highbp:2.sex"), "se"], tolerance = 1e-5)
  # lme4's own vcov() for nAGQ > 1 is an approximation (0.5% off here).
  wm <- tabtools:::tt_wald(g, vce = "model")
  expect_false(isTRUE(all.equal(wm$std.error, w$std.error, tolerance = 1e-3)))
  tt <- regtab(g, stats = c("n", "icc", "groups"))
  r <- re_rows(tt)
  v <- sp["/var(_cons[location])", ]
  expect_identical(r$key, "var(_cons[location])")
  expect_identical(r$label, "Median Odds Ratio (Location (stand office ID))")
  expect_equal(c(r$conf.low, r$conf.high), tabtools:::.rt_mor(c(v$ll, v$ul)), tolerance = 1e-4)
  expect_p5b_csv(regtab(g, stats = c("n", "icc", "groups"), noreeffects = TRUE), "melogit_nore")
})

test_that("mepoisson: variance rows, relabel, and an ICC note", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  g <- lme4::glmer(highbp ~ age + sex + (1 | location), family = poisson, data = nhanes_zone(), nAGQ = 7)
  expect_message(raw <- regtab(g, stats = c("n", "icc", "groups")), "ICC not computed")
  expect_p5b_csv(raw, "mepois_raw")
  expect_message(rel <- regtab(g, stats = c("n", "icc", "groups"), relabel = TRUE), "ICC not computed")
  expect_p5b_csv(rel, "mepois_relabel")
  st <- p5b_stats("mepois_relabel")
  expect_identical(rownames(rel$stored$table), st$value[st$kind == "rowname"])
  expect_null(raw$stored$icc_1)
})

test_that("me* random slopes: bracketed names, MOR, relabel as for mixed (2.1.11)", {
  skip_if_not_installed("lme4")
  # Laplace in both (Stata intmethod(laplace)): glmer allows nAGQ > 1 only
  # for a single scalar random effect.
  g <- lme4::glmer(yb ~ x + z + (1 + x | clinic), family = binomial, data = slope_data())
  raw <- regtab(g, stats = c("icc", "groups"))
  expect_p5b_csv(raw, "meslope_raw")
  expect_p5b_csv(regtab(g, stats = c("icc", "groups"), relabel = TRUE), "meslope_relabel")
  # The cov(x[clinic],_cons[clinic]) name is an invalid Stata stripe: 2.1.11
  # sanitises that name alone (2.1.9 named every row r1, r2, ...).
  st <- p5b_stats("meslope_raw")
  expect_identical(rownames(raw$stored$table), st$value[st$kind == "rowname"])
  sp <- p5b_params("melogit_slope")
  r <- re_rows(raw)
  expect_equal(r$estimate[c(1, 3)], sp[c("/var(x[clinic])", "/cov(x[clinic],_cons[clinic])"), "b"],
               tolerance = 1e-3)
  expect_equal(r$conf.low[3], sp["/cov(x[clinic],_cons[clinic])", "ll"], tolerance = 1e-3)
  # Both Laplace; the optimizers stop ~3e-4 (relative) apart in var(_cons).
  st <- p5b_stats("meslope_raw")
  expect_equal(raw$stored$icc_1, as.numeric(st$value[st$name == "icc_1"]), tolerance = 1e-3)
})

test_that("cloglog glmer shows the median hazard ratio", {
  skip_if_not_installed("lme4")
  g <- suppressWarnings(lme4::glmer(yb ~ x + (1 | clinic), family = binomial(link = "cloglog"),
                                    data = slope_data()))
  # lme4's cloglog Laplace fit gets a once-per-session note (P3-6: kept quiet).
  tt <- suppressMessages(regtab(g, stats = "icc"))
  r <- re_rows(tt)
  expect_identical(r$label, "Median Hazard Ratio (Clinic ID)")
  v <- lme4::VarCorr(g)$clinic[1, 1]
  expect_equal(r$estimate, tabtools:::.rt_mor(v))
  expect_equal(tt$stored$icc_1, v / (v + pi^2 / 6))
})

# ---------------------------------------------------------------------------
# Options and cross-model rules

test_that("random-effects rows survive nointercept and are never dimmed", {
  skip_if_not_installed("lme4")
  f <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp(), REML = FALSE)
  tt <- regtab(f, nointercept = TRUE, dimnonsig = TRUE, relabel = TRUE)
  expect_false("Intercept" %in% tt$body[[1]])
  expect_identical(tt$body[[1]][tt$rows$type == "re"],
                   c("Variance: Healthcare Region (Intercept)", "Residual Variance"))
  expect_false(any(tt$rows$dim[tt$rows$type == "re"]))
  # The workbook draws a top border above the first random-effects row.
  x <- tabtools:::.xlsx_layout_regtab(tt)$rules
  first <- which(tt$rows$type == "re")[1] + 3L
  expect_true(any(x$op == tabtools:::.OP[["top"]] & x$r1 == first & x$c1 == 2L))
})

test_that("random-effects families and structures across models", {
  skip_if_not_installed("lme4")
  d <- mixed_bp()
  d$hi <- as.integer(d$y > 3.5)
  f <- lme4::lmer(y ~ age + (1 | region), data = d, REML = FALSE)
  g <- lme4::glmer(hi ~ age + (1 | region), family = binomial, data = d)
  expect_error(regtab(f, g), "different scales")
  tt <- regtab(f, g, noreeffects = TRUE)
  expect_false(any(tt$rows$type == "re"))
  # Different grouping structures: relabel refused; without it a MOR row
  # loses its group label (`regtab.ado:1167-1188`, `:1668-1670`).
  d$site <- (seq_len(nrow(d)) %% 7) + 1
  g2 <- suppressMessages(lme4::glmer(hi ~ age + (1 | site), family = binomial, data = d))
  expect_error(regtab(g, g2, relabel = TRUE), "structures differ")
  tt <- regtab(g, g2)
  expect_true(all(tt$body[[1]][tt$rows$type == "re"] == "Median Odds Ratio"))
  # Shared structure: one union row per random-effects key.
  tt <- regtab(f, lme4::lmer(y ~ age + bmi + (1 | region), data = d, REML = FALSE), relabel = TRUE)
  expect_identical(tt$body[[1]][tt$rows$type == "re"],
                   c("Variance: Healthcare Region (Intercept)", "Residual Variance"))
})

test_that("stars apply to covariance rows only", {
  skip_if_not_installed("lme4")
  f <- lme4::lmer(yc ~ x + z + (1 + x | clinic), data = slope_data(), REML = FALSE)
  tt <- regtab(f, stars = TRUE, starslevels = c(0.99, 0.98, 0.97))
  re <- tt$rows$type == "re"
  est <- tt$body[[2]][re]
  expect_identical(grepl("\\*$", est), c(FALSE, FALSE, TRUE, FALSE))
})

# ---------------------------------------------------------------------------
# glmmTMB

test_that("glmmTMB reproduces the R21 and R22 goldens", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  t1 <- glmmTMB::glmmTMB(y ~ age + female + bmi + (1 | region), data = mixed_bp())
  tt <- quiet_nobars(regtab(t1, coef = "Coef.", stats = c("n", "groups", "aic", "icc"), relabel = TRUE,
                            models = "Mixed"))
  expect_cells_match(tt, "R21", mode = "exact")
  expect_console_match(tt, "R21")
  t2 <- glmmTMB::glmmTMB(highbp ~ age + sex + (1 | location), family = binomial, data = nhanes_zone())
  tt <- quiet_nobars(regtab(t2, stats = c("n", "icc", "groups"), models = "Multilevel logistic"))
  expect_cells_match(tt, "R22", mode = "exact")
  expect_console_match(tt, "R22")
})

test_that("glmmTMB us and diag structures", {
  skip_if_not_installed("glmmTMB")
  s <- slope_data()
  t3 <- glmmTMB::glmmTMB(yc ~ x + z + (1 + x | clinic), data = s)
  expect_p5b_csv(quiet_nobars(regtab(t3, stats = "icc", relabel = TRUE)), "slope2_relabel")
  # diag (Stata cov(independent)): variances only.
  t4 <- glmmTMB::glmmTMB(yc ~ x + z + diag(1 + x | clinic), data = s)
  tt <- quiet_nobars(regtab(t4, relabel = TRUE))
  expect_identical(tt$body[[1]][tt$rows$type == "re"],
                   c("Variance: Clinic ID (Exposure score)", "Variance: Clinic ID (Intercept)", "Residual Variance"))
})
