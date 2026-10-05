# Task 5.19: nlme::lme as Stata `mixed`. Stata expectations: goldens
# R78-R82 (test-golden-regtab.R) and the Phase 5b fixtures of Stata's
# `mixed` (tests/testthat/fixtures/regtab_phase5b/, the lmer tests'
# evidence, qa/stata/make_regtab_phase5b.do); Stata 17 probe
# qa/stata/probe_nlme_lme.do for the standard errors.

skip_if_not_installed("nlme")

p5b <- function(name) test_path("fixtures", "regtab_phase5b", name)

p5b_params <- function(model) {
  d <- utils::read.csv(p5b("re_params.csv"), stringsAsFactors = FALSE, strip.white = TRUE,
                       na.strings = c(".", ".b"))
  d <- d[d$model == model, , drop = FALSE]
  rownames(d) <- d$param
  d
}

expect_p5b_csv <- function(tt, tag) {
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, f)
  expect_identical(readLines(f), readLines(p5b(paste0(tag, ".csv"))))
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

lme_ri <- function(method = "ML", data = mixed_bp()) {
  nlme::lme(y ~ age + female + bmi, random = ~ 1 | region, data = data, method = method)
}

# The unscaled age slope needs more EM iterations for lme to reach Stata's
# optimum (ll -838.45135), as lmer needs bobyqa.
lme_slope <- function() {
  nlme::lme(y ~ age + female + bmi, random = ~ age | region, data = mixed_bp(), method = "ML",
            control = nlme::lmeControl(niterEM = 200, msMaxIter = 1000))
}

S6 <- c("n", "groups", "ll", "aic", "bic", "icc")

# ---------------------------------------------------------------------------
# Fixed effects: varFix, never summary.lme()

test_that("fixed effects use varFix (Stata mixed's GLS variance), not summary.lme()'s sqrt(N/(N-p))", {
  f <- lme_ri("ML")
  V <- tt_vcov(f)
  expect_identical(unname(V), unname(f$varFix))
  expect_identical(rownames(V), names(nlme::fixef(f)))
  expect_identical(tt_vcov(f, "model"), V)
  expect_identical(tt_vce_types(f), c("stata", "model"))
  # Stata 17 `mixed y age female bmi || region:` (probe_nlme_lme.do):
  # Std. err. .0032326 .079991 .0078148 .2855678; lme's varFix agrees.
  se <- sqrt(diag(V))
  expect_equal(unname(se), c(0.2855678, 0.0032326, 0.079991, 0.0078148), tolerance = 2e-6)
  # summary.lme() rescales an ML fit's standard errors by sqrt(N / (N - p)).
  N <- 600
  p <- 4
  st <- summary(f)$tTable
  expect_equal(unname(st[, "Std.Error"]), unname(se) * sqrt(N / (N - p)), tolerance = 1e-10)
  # regtab: Wald z intervals from varFix, as Stata prints them.
  tt <- regtab(f, coef = "Coef.")
  r <- tt$meta$regtab_rows
  age <- r[r$key == "age", ]
  z <- stats::qnorm(0.975)
  b <- nlme::fixef(f)[["age"]]
  expect_equal(c(age$conf.low, age$conf.high), b + c(-1, 1) * z * se[["age"]], tolerance = 1e-12)
  expect_equal(age$p.value, 2 * stats::pnorm(-abs(b / se[["age"]])), tolerance = 1e-12)
  # Stata's interval .0019972 .0146686.
  expect_equal(c(age$conf.low, age$conf.high), c(0.0019972, 0.0146686), tolerance = 5e-5)
  # REML: summary.lme() does not rescale, and Stata's `mixed, reml` SE is
  # varFix's (.0032412).
  fr <- lme_ri("REML")
  expect_equal(unname(summary(fr)$tTable[, "Std.Error"]), unname(sqrt(diag(fr$varFix))), tolerance = 1e-12)
  expect_equal(sqrt(fr$varFix["age", "age"]), 0.0032412, tolerance = 5e-5)
})

# ---------------------------------------------------------------------------
# Same table as lmer

test_that("random intercept: the lme table is the lmer table (ML and REML)", {
  skip_if_not_installed("lme4")
  for (reml in c(FALSE, TRUE)) {
    f <- lme_ri(if (reml) "REML" else "ML")
    g <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp(), REML = reml)
    a <- regtab(f, coef = "Coef.", stats = S6, relabel = TRUE)
    b <- regtab(g, coef = "Coef.", stats = S6, relabel = TRUE)
    expect_identical(a$body, b$body)
    expect_identical(a$rows$type, b$rows$type)
    expect_identical(rownames(a$stored$table), rownames(b$stored$table))
    # The two fits agree to their convergence (flat likelihood in the
    # variance), and so do the intervals built from them.
    ra <- a$meta$regtab_rows
    rb <- b$meta$regtab_rows
    for (col in c("estimate", "conf.low", "conf.high", "p.value")) {
      expect_equal(ra[[col]], rb[[col]], tolerance = 1e-5, label = paste(reml, col))
    }
    for (s in c("n_1", "groups_1", "ll_1", "aic_1", "bic_1")) expect_equal(a$stored[[s]], b$stored[[s]], tolerance = 1e-10)
    expect_equal(a$stored$icc_1, b$stored$icc_1, tolerance = 1e-5)
    expect_identical(a$stored$methods, b$stored$methods)
  }
})

test_that("REML: variance CIs from the REML criterion match Stata's mixed, reml", {
  tt <- regtab(lme_ri("REML"), coef = "Coef.", stats = c("n", "groups", "aic", "icc"), relabel = TRUE)
  expect_p5b_csv(tt, "reml_relabel")
  sp <- p5b_params("mixed_reml")
  r <- re_rows(tt)
  want <- sp[c("region:var(_cons)", "Residual:var(e)"), ]
  expect_equal(r$conf.low, want$ll, tolerance = 1e-4)
  expect_equal(r$conf.high, want$ul, tolerance = 1e-4)
})

test_that("unstructured random slope (pdLogChol): rows, keep/drop, noreeffects as Stata's mixed", {
  skip_on_cran()
  f <- lme_slope()
  expect_equal(as.numeric(logLik(f)), -838.45135, tolerance = 1e-8)
  raw <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"))
  expect_p5b_csv(raw, "slope_raw")
  expect_p5b_csv(regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"), relabel = TRUE), "slope_relabel")
  expect_p5b_csv(regtab(f, coef = "Coef.", keep = "age"), "slope_keep")
  expect_p5b_csv(regtab(f, coef = "Coef.", drop = "age"), "slope_drop")
  expect_p5b_csv(regtab(f, coef = "Coef.", noreeffects = TRUE, stats = "icc"), "slope_nore")
  sp <- p5b_params("mixed_slope")
  r <- re_rows(raw)
  want <- sp[c("region:var(age)", "region:var(_cons)", "region:cov(age,_cons)", "Residual:var(e)"), ]
  expect_identical(r$key, c("var(age)", "var(_cons)", "cov(age,_cons)", "var(e)"))
  expect_equal(r$estimate, want$b, tolerance = 1e-4)
  expect_equal(r$conf.low, want$ll, tolerance = 1e-3)
  expect_equal(r$conf.high, want$ul, tolerance = 1e-3)
  expect_equal(r$p.value[3], want$p[3], tolerance = 1e-3)
  expect_true(all(is.na(r$p.value[-3])))
  skip_if_not_installed("lme4")
  g <- suppressWarnings(lme4::lmer(y ~ age + female + bmi + (1 + age | region), data = mixed_bp(), REML = FALSE,
                                   control = lme4::lmerControl(optimizer = "bobyqa")))
  expect_identical(raw$body, regtab(g, coef = "Coef.", stats = c("n", "groups", "icc"))$body)
})

test_that("independent random slope (pdDiag) is Stata's cov(independent), as lmer's (1 | g) + (0 + x | g)", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  d <- golden_fixture("nhanes2", factors = "sex")
  f <- nlme::lme(bmi ~ age + sex, random = list(location = nlme::pdDiag(~ age)), data = d, method = "ML")
  # lme4's default optimizer stops short here (var(_cons) 0.079, ll 3e-8
  # lower); bobyqa reaches lme's and Stata's optimum (R81).
  g <- lme4::lmer(bmi ~ age + sex + (1 | location) + (0 + age | location), data = d, REML = FALSE,
                  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(rhoend = 1e-12)))
  a <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"))
  expect_identical(a$body, regtab(g, coef = "Coef.", stats = c("n", "groups", "icc"))$body)
  # One variance row per component, slope first; no covariance row.
  expect_identical(re_rows(a)$key, c("var(age)", "var(_cons)", "var(e)"))
  rel <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"), relabel = TRUE, models = "Independent")
  expect_identical(re_rows(rel)$label, c("Variance: Location (stand office ID) (Age (years))",
                                         "Variance: Location (stand office ID) (Intercept)", "Residual Variance"))
  # R81's var(age), 2.81e-5, is below the golden's absolute table tolerance
  # (5e-5): checked here relative to Stata's r(table) value.
  st <- golden_read_stored("R81")
  st <- st[st$name == "table" & st$col == "c1", ]
  expect_identical(st$row, rownames(rel$stored$table))
  va <- which(startsWith(st$row, "Variance_Location")) [1]
  expect_equal(unname(rel$stored$table[va, 1]), as.numeric(st$value[va]), tolerance = 1e-3)
  expect_equal(as.numeric(st$value[va]), 2.81085e-5, tolerance = 1e-4)
})

test_that("variance components at the boundary: 0 with blank intervals, as lmer's (review P2-2)", {
  skip_if_not_installed("lme4")
  # mixed_bp's independent age slope: Stata var(age) 3.16e-13 (CI 0 to .),
  # lme 8.7e-12, lmer exactly 0.
  d <- mixed_bp()
  f <- nlme::lme(y ~ age + female + bmi, random = list(region = nlme::pdDiag(~ age)), data = d, method = "ML")
  g <- lme4::lmer(y ~ age + female + bmi + (1 | region) + (0 + age | region), data = d, REML = FALSE)
  a <- regtab(f, stats = c("n", "groups", "ll", "icc"))
  b <- suppressMessages(regtab(g, stats = c("n", "groups", "ll", "icc")))
  expect_identical(a$body, b$body)
  ra <- re_rows(a)
  expect_identical(ra$estimate[1], 0)
  expect_true(is.na(ra$conf.low[1]) && is.na(ra$conf.high[1]))
  expect_identical(is.na(ra$conf.low), is.na(re_rows(b)$conf.low))
  # A random intercept at the boundary.
  f2 <- nlme::lme(y ~ age, random = ~ 1 | region, data = d, method = "ML", subset = 1:300)
  g2 <- suppressMessages(lme4::lmer(y ~ age + (1 | region), data = d, REML = FALSE, subset = 1:300))
  expect_identical(regtab(f2)$body, suppressMessages(regtab(g2))$body)
  # A factor slope whose lme fit stops on the ridge towards the boundary
  # (lmer: var(_cons) 0; lme ll 0.048 lower): refused as short of its
  # maximum, with no base-R NaN warning on the way.
  d$sexf <- factor(d$female, 0:1, c("M", "F"))
  f3 <- nlme::lme(y ~ age + sexf, random = ~ sexf | region, data = d, method = "ML")
  expect_no_warning(err <- tryCatch(regtab(f3), error = function(e) e))
  expect_match(conditionMessage(err), "is not at its maximum", fixed = TRUE)
  skip_on_cran()
  # Nested, with a boundary slope at the outer level (review probe p15).
  n <- nhanes_zone()
  fn <- nlme::lme(bmi ~ age + sex, random = list(zone = nlme::pdDiag(~ age), location = ~ 1), data = n, method = "ML")
  gn <- suppressMessages(lme4::lmer(bmi ~ age + sex + (1 | zone) + (0 + age | zone) + (1 | location:zone), n, REML = FALSE,
                                    control = lme4::lmerControl(optimizer = "bobyqa")))
  an <- regtab(fn, stats = c("n", "groups", "icc"))
  expect_identical(an$body, suppressMessages(regtab(gn, stats = c("n", "groups", "icc")))$body)
  expect_identical(re_rows(an)$key[1], "var(age[zone])")
  expect_true(is.na(re_rows(an)$conf.high[1]))
})

# The verification review's simulation (review-lme2/gen.R): random
# intercepts and slopes with a covariate on scale xs.
sim_slopes <- function(seed) {
  set.seed(seed)
  m <- sample(c(10, 20, 50, 150), 1)
  n <- sample(c(4, 10, 40), 1)
  unbal <- runif(1) < 0.5
  g <- if (unbal) sample(seq_len(m), m * n, replace = TRUE, prob = rexp(m)) else rep(seq_len(m), each = n)
  g <- match(g, unique(g))
  m2 <- max(g)
  xs <- sample(c(1, 1e3, 1e-3), 1)
  xm <- sample(c(0, 5), 1)
  x <- (rnorm(m * n) + xm) * xs
  tau_s <- sample(c(0, 0, 0.002, 0.02, 0.1), 1)
  y <- 1 + rnorm(m2)[g] * 0.5 + (rnorm(m2)[g] * tau_s + 0.3) * x / xs + rnorm(m * n)
  pd <- sample(c("symm", "diag"), 1)
  meth <- sample(c("ML", "REML"), 1)
  list(d = data.frame(y, x, g = factor(g)), meth = meth)
}

test_that("an lme fit short of its maximum is refused, whatever the covariate's scale (verification review P1)", {
  skip_on_cran()
  # Seed 103: the maximum has corr(x, _cons) = -1 (lmer: SD(x) 0.180, ll
  # -8698.8656); lme stops on the flat ridge at SD(x) 0.0047, ll -8698.8743,
  # which the SD cutoff took for a zero variance.
  for (seed in c(103, 22)) for (k in c(0.01, 1, 1e3)) {
    G <- sim_slopes(seed)
    G$d$x <- G$d$x * k
    f <- nlme::lme(y ~ x, random = ~ x | g, data = G$d, method = G$meth)
    err <- tryCatch(regtab(f), error = function(e) e)
    lab <- paste("seed", seed, "scale", k)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "is not at its maximum", fixed = TRUE, label = lab)
    # The maximum is singular (correlation -1): the hint leads with lmer or
    # pdDiag, never lme options that cannot reach it.
    expect_match(conditionMessage(err), "lme4::lmer()", fixed = TRUE, label = lab)
    expect_match(conditionMessage(err), "pdDiag()", fixed = TRUE, label = lab)
    expect_false(grepl("optim", conditionMessage(err), fixed = TRUE))
  }
  opt <- tabtools:::.rt_lme_optimum(f)
  expect_gt(opt$short, tabtools:::.rt_lme_short_tol(opt$n, TRUE))
  expect_true(opt$singular)
})

test_that("the bounded refit puts a near-zero factor diagonal on the bound when the deviance allows (macOS arm64 CI)", {
  snap <- tabtools:::.rt_lme_snap
  # Term 1: one component (rms 56.5); term 2: a 2 x 2 factor (rms 1 and 2),
  # theta in lme4's order (column-major lower triangle).
  terms <- list(list(comps = "age", rms = 56.5), list(comps = c("_cons", "x"), rms = c(1, 2)))
  flat <- function(th) 100
  # nlminb stopped at 3.97e-9 (2.2e-7 of sigma): put on the bound.
  expect_identical(snap(c(3.97e-9, 0.5, -0.2, 1e-6), terms, flat, 100, 1e-6), c(0, 0.5, -0.2, 0))
  # Contributions at or above 1e-5 of sigma are left interior, however flat
  # (5.6e-5 and 2e-5 here; lmer finds interior maxima at 6e-4 of sigma).
  expect_identical(snap(c(1e-6, 0.5, -0.2, 1e-5), terms, flat, 100, 1e-6), c(1e-6, 0.5, -0.2, 1e-5))
  # A small entry whose zero costs more deviance than the tolerance stays.
  steep <- function(th) 100 + 1e6 * (3.97e-9 - th[1])
  expect_identical(snap(c(3.97e-9, 0.5, -0.2, 0.3), terms, steep, 100, 1e-6), c(3.97e-9, 0.5, -0.2, 0.3))
  expect_identical(snap(c(3.97e-9, 0.5, -0.2, 0.3), terms, steep, 100, 1e-2), c(0, 0.5, -0.2, 0.3))
  # A non-finite deviance at the bound keeps the entry.
  expect_identical(snap(1e-9, terms[1], function(th) NaN, 100, 1), 1e-9)
})

test_that("an lme fit at a boundary maximum that nlminb stops short of is shown as lmer's, not refused (independent review 2026-10-01)", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  # pdDiag slope with true SD 1e-3: lmer's theta for x is 0. nlminb stops
  # inside the bound on x86_64 too; before .rt_lme_snap() this fit was
  # refused with the "more iterations" hint, though lme is at its maximum.
  set.seed(311002)
  G <- 30
  ni <- 300
  g <- rep(seq_len(G), each = ni)
  x <- rnorm(G * ni)
  u <- rnorm(G)
  v <- rnorm(G)
  d <- data.frame(y = 1 + 0.3 * u[g] + (0.3 + 0.001 * v[g]) * x + rnorm(G * ni), x, g = factor(g))
  f <- nlme::lme(y ~ x, random = list(g = nlme::pdDiag(~ x)), data = d, method = "ML")
  m <- lme4::lmer(y ~ x + (1 | g) + (0 + x | g), data = d, REML = FALSE,
                  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(rhoend = 1e-10)))
  expect_true(tabtools:::.rt_lme_optimum(f)$singular)
  expect_identical(regtab(f)$body, suppressMessages(regtab(m))$body)
})

test_that("the bounded refit: goldens at their maximum, boundaries zeroed and interior components kept at every scale", {
  skip_on_cran()
  # R78-R82's lme fits are at their own maximum (short < 1e-6).
  for (f in list(lme_ri("ML"), lme_ri("REML"), lme_slope())) {
    o <- tabtools:::.rt_lme_optimum(f)
    expect_null(o$error)
    expect_lt(abs(o$short), 1e-6)
  }
  d <- mixed_bp()
  n <- golden_fixture("nhanes2", factors = "sex")
  for (k in c(1e-3, 1, 1e3)) {
    d2 <- d
    d2$age <- d2$age * k
    # A true boundary (Stata var(age) 3.16e-13): 0, blank interval.
    f <- nlme::lme(y ~ age + female + bmi, random = list(region = nlme::pdDiag(~ age)), data = d2, method = "ML")
    r <- re_rows(regtab(f))
    expect_identical(r$estimate[1], 0, label = paste("boundary, scale", k))
    expect_true(is.na(r$conf.low[1]))
    expect_false(anyNA(r$conf.low[-1]))
    # A small interior variance (R81's var(age), 2.8e-5 at scale 1): kept,
    # with its interval.
    n2 <- n
    n2$age <- n2$age * k
    f2 <- nlme::lme(bmi ~ age + sex, random = list(location = nlme::pdDiag(~ age)), data = n2, method = "ML")
    r2 <- re_rows(regtab(f2))
    expect_equal(r2$estimate[1] * k^2, 2.81e-5, tolerance = 1e-2, label = paste("interior, scale", k))
    expect_false(anyNA(r2$conf.low))
  }
})

test_that("the shortfall verdict does not depend on the response's units (review 3, P1-a)", {
  skip_on_cran()
  # No group effect, N = 1e4: lme stops short of the zero variance by design
  # (about 1e-9 per observation). The verdict used to flip with the units
  # (the old tolerance scaled with |deviance|): accepted at y, refused at
  # 0.3 y.
  set.seed(1)
  d <- data.frame(g = factor(rep(1:500, each = 20)), x = rnorm(1e4))
  d$y <- 3 + d$x + rnorm(1e4)
  for (k in c(0.3, 1, 1e3)) {
    f <- nlme::lme(y ~ x, random = ~ 1 | g, data = transform(d, y = k * y))
    tt <- tryCatch(regtab(f), error = function(e) e)
    expect_s3_class(tt, "tt_table")
    if (inherits(tt, "tt_table")) {
      r <- re_rows(tt)
      expect_identical(r$estimate[1], 0, label = paste("units", k))
      expect_true(is.na(r$conf.low[1]))
    }
  }
})

test_that("an lme stopped short of an interior maximum is refused with a hint to iterate more (review 3, P2)", {
  skip_on_cran()
  # tolscale2.R seed 4, response x10, REML: the variance ratio is off by
  # ~2%, 1.5e-8 per observation short.
  set.seed(4)
  d <- data.frame(g = factor(rep(1:500, each = 20)), x = rnorm(1e4))
  d$y <- 10 * (3 + d$x + rnorm(1e4))
  f <- nlme::lme(y ~ x, random = ~ 1 | g, data = d, method = "REML")
  opt <- tabtools:::.rt_lme_optimum(f)
  expect_false(opt$singular)
  err <- tryCatch(regtab(f), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "is not at its maximum", fixed = TRUE)
  expect_match(conditionMessage(err), "niterEM", fixed = TRUE)
  expect_false(grepl("optim\"", conditionMessage(err), fixed = TRUE))
})

test_that("transformed terms in the random formula (log(), I()) are laid out as for lmer (review 3, P1-b)", {
  skip_if_not_installed("lme4")
  O <- as.data.frame(nlme::Orthodont)
  f <- nlme::lme(distance ~ log(age), random = ~ log(age) | Subject, data = O, method = "ML")
  g <- lme4::lmer(distance ~ log(age) + (log(age) | Subject), data = O, REML = FALSE,
                  control = lme4::lmerControl(optimizer = "bobyqa"))
  a <- regtab(f, stats = c("n", "groups", "ll"))
  expect_identical(a$body, regtab(g, stats = c("n", "groups", "ll"))$body)
  expect_identical(re_rows(a)$key[1:3], c("var(log(age))", "var(_cons)", "cov(log(age),_cons)"))
  skip_on_cran()
  Dia <- as.data.frame(nlme::Dialyzer)
  Dia$Subject <- factor(Dia$Subject, ordered = FALSE)
  f2 <- nlme::lme(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4)) * QB,
                  random = ~ pressure + I(pressure^2) | Subject, data = Dia)
  g2 <- lme4::lmer(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4)) * QB +
                     (pressure + I(pressure^2) | Subject), data = Dia,
                   control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(rhoend = 1e-10)))
  # Same rows, labels and cell text; numbers within one displayed unit. The
  # two optimizers agree to about 7 digits, and a random-effect CI bound
  # near a rounding tie printed 16.51 vs 16.52 on CI's ubuntu and macOS
  # runners (2026-10-01).
  a2 <- regtab(f2, stats = c("n", "ll"), interactions = "native")
  b2 <- regtab(g2, stats = c("n", "ll"), interactions = "native")
  golden_fail(golden_compare_cells(a2, b2, mode = "tolerance"), "lme vs lmer, Dialyzer")
  # Every cell's format (decimals, separators, notation) stays identical.
  digits <- function(t) gsub("[0-9]", "9", golden_as_cells(t))
  expect_identical(digits(a2), digits(b2))
})

test_that("a subset or na.omit that removes a factor level: unused levels dropped, as lme and lmer do (review P2-1)", {
  skip_if_not_installed("lme4")
  d <- as.data.frame(mixed_bp())
  d$grp3 <- factor(ifelse(d$bmi < 25, "lo", ifelse(d$bmi < 30, "mid", "hi")), c("lo", "mid", "hi"))
  f <- nlme::lme(y ~ age + grp3, random = ~ 1 | region, data = d, method = "ML", subset = grp3 != "mid")
  g <- lme4::lmer(y ~ age + grp3 + (1 | region), data = d, REML = FALSE, subset = grp3 != "mid")
  a <- regtab(f, stats = c("n", "groups"))
  expect_identical(a$body, regtab(g, stats = c("n", "groups"))$body)
  expect_false("mid" %in% trimws(a$body[, 1]))
  d2 <- d
  d2$grp3[d2$grp3 == "mid"] <- NA
  d2$age[c(3, 50)] <- NA
  f2 <- nlme::lme(y ~ age + grp3, random = ~ 1 | region, data = d2, method = "ML", na.action = na.omit)
  g2 <- lme4::lmer(y ~ age + grp3 + (1 | region), data = d2, REML = FALSE)
  expect_identical(regtab(f2, stats = "n")$body, regtab(g2, stats = "n")$body)
})

test_that("a fit without data = reads its variables from the formula's environment (review P3-1)", {
  skip_if_not_installed("lme4")
  d <- mixed_bp()
  y <- d$y
  age <- as.numeric(d$age)
  region <- d$region
  f <- nlme::lme(y ~ age, random = ~ 1 | region, method = "ML")
  g <- lme4::lmer(y ~ age + (1 | region), REML = FALSE)
  expect_identical(regtab(f, stats = "n")$body, regtab(g, stats = "n")$body)
  age <- rev(age)
  err <- tryCatch(regtab(f), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "formula's environment", fixed = TRUE)
  expect_false(grepl("keep.data = TRUE", conditionMessage(err), fixed = TRUE))
  # Kept data, but a variable read from the formula's environment changed
  # (verification review P3a).
  # (lme finds such a variable only in the global environment.)
  set.seed(2)
  assign(".tt_test_w", rnorm(nrow(d)), envir = globalenv())
  withr::defer(rm(".tt_test_w", envir = globalenv()))
  f2 <- nlme::lme(y ~ age + .tt_test_w, random = ~ 1 | region, data = d, method = "ML")
  expect_s3_class(regtab(f2), "tt_table")
  assign(".tt_test_w", rev(get(".tt_test_w", envir = globalenv())), envir = globalenv())
  err <- tryCatch(regtab(f2), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "reads from the formula's environment", fixed = TRUE)
})

test_that("three-level nested lme (~ 1 | zone/location): outermost first, bracketed names, as Stata", {
  skip_on_cran()
  f <- nlme::lme(bmi ~ age + sex, random = ~ 1 | zone/location, data = nhanes_zone(), method = "ML")
  raw <- regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"))
  expect_p5b_csv(raw, "lvl3_raw")
  expect_p5b_csv(regtab(f, coef = "Coef.", stats = c("n", "groups", "icc"), relabel = TRUE), "lvl3_relabel")
  expect_identical(re_rows(raw)$key, c("var(_cons[zone])", "var(_cons[location])", "var(e)"))
  # Groups: the innermost level's (zone/location paths, 62), as Stata.
  expect_identical(raw$stored$groups_1, 62)
})

test_that("stats tokens: n, groups, ll, aic, bic, icc (Stata's e(ll) and e(rank))", {
  f <- lme_ri("ML")
  tt <- regtab(f, stats = S6)
  ll <- logLik(f)
  expect_identical(tt$stored$n_1, 600)
  expect_identical(tt$stored$groups_1, 6)
  expect_equal(tt$stored$ll_1, as.numeric(ll))
  expect_identical(attr(ll, "df"), 6)
  expect_equal(tt$stored$aic_1, -2 * as.numeric(ll) + 12)
  expect_equal(tt$stored$bic_1, -2 * as.numeric(ll) + 6 * log(600))
  # Stata: e(ll) -839.117182226987 (ML), -851.500608686442 (REML).
  expect_equal(tt$stored$ll_1, -839.117182226987, tolerance = 1e-10)
  expect_equal(regtab(lme_ri("REML"), stats = "ll")$stored$ll_1, -851.500608686442, tolerance = 1e-10)
  v0 <- as.matrix(f$modelStruct$reStruct$region)[1, 1] * f$sigma^2
  expect_equal(tt$stored$icc_1, v0 / (v0 + f$sigma^2), tolerance = 1e-12)
  expect_identical(tt$stored$coef_label, "Coef.")
  expect_match(tt$stored$methods, "linear mixed-effects regression", fixed = TRUE)
})

test_that("the rebuilt deviance is the fit's -2 logLik and its Hessian gives Stata's variance-parameter variance", {
  f <- lme_ri("ML")
  unc <- tabtools:::.rt_re_uncertainty(f)
  # Stata e(V): lns1_1_1 .626703924210226, lnsig_e .000841753223326233,
  # covariance -.00146225216613897.
  # (Measured: 8e-6, 3e-7 and 4e-5 relative.)
  expect_equal(unc$V[1, 1], 0.626703924210226, tolerance = 5e-5)
  expect_equal(unc$V[2, 2], 0.000841753223326233, tolerance = 1e-6)
  expect_equal(unc$V[1, 2], -0.00146225216613897, tolerance = 2e-4)
  expect_equal(unc$psi, c(-2.60116714857014, -0.0226840750463907), tolerance = 1e-5)
  # The same as the lmer path's on the same model.
  skip_if_not_installed("lme4")
  g <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp(), REML = FALSE)
  expect_equal(unc$V, tabtools:::.rt_re_uncertainty(g)$V, tolerance = 1e-4)
})

test_that("an lme and an lmer fit share random-effects rows in one table", {
  skip_if_not_installed("lme4")
  f <- lme_ri("ML")
  g <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp(), REML = FALSE)
  tt <- regtab(f, g, models = c("nlme", "lme4"), relabel = TRUE, stats = c("n", "icc"))
  expect_identical(sum(tt$rows$type == "re"), 2L)
  expect_identical(tt$body[, 2], tt$body[, 5])
})

test_that("labels and value labels come from the fit's data; subset = is honoured", {
  d <- mixed_bp()
  f <- nlme::lme(y ~ age + female + bmi, random = ~ 1 | region, data = d, method = "ML", subset = region != 3)
  tt <- regtab(f, relabel = TRUE, stats = c("n", "groups"))
  expect_identical(tt$body[1:3, 1], c("Age (years)", "Female sex", "BMI"))
  expect_identical(re_rows(tt)$label[1], "Variance: Healthcare Region (Intercept)")
  expect_identical(tt$stored$n_1, 500)
  expect_identical(tt$stored$groups_1, 5)
  # A factor: header, Reference row and levels, as for any model.
  d$sexf <- factor(d$female, 0:1, c("Male", "Female"))
  f2 <- nlme::lme(y ~ age + sexf, random = ~ 1 | region, data = d, method = "ML")
  b <- regtab(f2)$body
  expect_identical(b[2:4, 1], c("sexf", "  Male", "  Female"))
  expect_identical(b[3, 2], "Reference")
})

test_that("ci_method = 'profile', robust and user-supplied variances are refused for lme, as for lmer", {
  f <- lme_ri("ML")
  expect_error(regtab(f, ci_method = "profile"), "not available for <lme> models", fixed = TRUE)
  expect_error(regtab(f, vce = "robust"), "lme", fixed = TRUE)
  expect_error(regtab(f, vce = 4 * f$varFix), "not available for <lme> models", fixed = TRUE)
})

test_that("non-treatment contrasts are refused by name, as for lmer (review P3-3)", {
  d <- as.data.frame(mixed_bp())
  d$grp3 <- factor(ifelse(d$bmi < 25, "lo", ifelse(d$bmi < 30, "mid", "hi")), c("lo", "mid", "hi"))
  f <- nlme::lme(y ~ age + grp3, random = ~ 1 | region, data = d, method = "ML", contrasts = list(grp3 = "contr.sum"))
  expect_error(regtab(f), "`grp3` uses \"contr.sum\" contrasts")
  d$grpo <- factor(d$grp3, ordered = TRUE)
  f2 <- nlme::lme(y ~ age + grpo, random = ~ 1 | region, data = d, method = "ML")
  expect_error(regtab(f2), "`grpo` is an ordered factor", fixed = TRUE)
  expect_error(regtab(f2), "factor(grpo, ordered = FALSE)", fixed = TRUE)
  # Treatment contrasts with another base level are still treatment-type.
  f3 <- nlme::lme(y ~ age + grp3, random = ~ 1 | region, data = d, method = "ML",
                  contrasts = list(grp3 = contr.treatment(3, base = 3)))
  b <- regtab(f3)$body
  expect_identical(b[trimws(b[, 1]) == "hi", 2], "Reference")
})

# ---------------------------------------------------------------------------
# Refusals (decision H-D6: never silently different numbers)

test_that("lme settings Stata mixed's layout cannot express are refused by name", {
  d <- as.data.frame(mixed_bp())
  refused <- function(fit, ...) {
    err <- tryCatch(regtab(fit), error = function(e) e)
    expect_s3_class(err, "error")
    for (p in c(...)) expect_match(conditionMessage(err), p, fixed = TRUE)
  }
  refused(nlme::lme(y ~ age, random = ~ 1 | region, data = d, correlation = nlme::corAR1()),
          "`correlation` structure", "<corAR1>", "residuals()")
  refused(nlme::lme(y ~ age, random = ~ 1 | region, data = d, weights = nlme::varIdent(form = ~ 1 | female)),
          "`weights` variance function", "<varIdent>")
  refused(nlme::lme(y ~ age, random = ~ 1 | region, data = d, control = nlme::lmeControl(sigma = 1)),
          "fixed residual standard deviation", "lmeControl(sigma = )")
  refused(suppressWarnings(nlme::lme(y ~ age, random = list(region = nlme::pdIdent(~ age)), data = d)),
          "`pdIdent()` covariance structure", "cov(identity)")
  refused(suppressWarnings(nlme::lme(y ~ age, random = list(region = nlme::pdCompSymm(~ age)), data = d)),
          "`pdCompSymm()` covariance structure")
  refused(suppressWarnings(nlme::lme(y ~ age, random = list(region = nlme::pdBlocked(list(nlme::pdSymm(~ 1), nlme::pdIdent(~ age - 1)))),
                                     data = d)),
          "`pdBlocked()` covariance structure")
  # A single random effect is Stata's identity whatever the pdMat class.
  one <- nlme::lme(y ~ age, random = list(region = nlme::pdIdent(~ 1)), data = d, method = "ML")
  expect_identical(regtab(one)$body, regtab(nlme::lme(y ~ age, random = ~ 1 | region, data = d, method = "ML"))$body)
  # Other nlme classes stay refused by class.
  err <- tryCatch(regtab(nlme::gls(y ~ age, data = d)), error = function(e) e)
  expect_match(conditionMessage(err), "does not support <gls> models", fixed = TRUE)
  expect_match(conditionMessage(err), "nlme::lme()", fixed = TRUE)
  nl <- suppressWarnings(nlme::nlme(circumference ~ SSlogis(age, Asym, xmid, scal), data = datasets::Orange,
                                    fixed = Asym + xmid + scal ~ 1, random = Asym ~ 1 | Tree,
                                    start = c(Asym = 170, xmid = 700, scal = 350)))
  err <- tryCatch(regtab(nl), error = function(e) e)
  expect_match(conditionMessage(err), "does not support <nlme> models", fixed = TRUE)
  expect_match(conditionMessage(err), "menl", fixed = TRUE)
})

test_that("a failed variance-parameter Hessian warns and is recorded, never silently blank (H-D6)", {
  f <- lme_ri("ML", data = mixed_bp()[mixed_bp()$region != 6, ])
  local_mocked_bindings(.rt_re_uncertainty_lme = function(fit) stop("boom"))
  # The failure is cached with the fit's fingerprint: clear it afterwards.
  withr::defer(rm(list = ls(tabtools:::.rt_re_cache), envir = tabtools:::.rt_re_cache))
  expect_warning(tt <- regtab(f, coef = "Coef."), class = "tabtools_vce_fallback")
  expect_identical(tt$stored$vce_fallback, "random-effects rows without confidence intervals")
  r <- re_rows(tt)
  expect_true(all(is.na(r$conf.low)))
  # The fixed effects keep varFix.
  age <- tt$meta$regtab_rows[tt$meta$regtab_rows$key == "age", ]
  expect_equal(age$conf.high - age$conf.low, 2 * stats::qnorm(0.975) * sqrt(f$varFix["age", "age"]), tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# Storage and data changed after fitting (Milestone H task H4)

test_that("data changed or removed after fitting: the same table, or a refusal naming keep.data = TRUE", {
  mk <- function() {
    d <- as.data.frame(mixed_bp())
    rownames(d) <- paste0("r", seq_len(nrow(d)))
    d
  }
  S <- c("n", "groups", "ll", "icc")
  for (keep in c(TRUE, FALSE)) {
    for (mut in c("drop", "perm", "edit", "regroup", "remove")) {
      dd <- mk()
      f <- nlme::lme(y ~ age + female + bmi, random = ~ 1 | region, data = dd, method = "ML", keep.data = keep)
      want <- regtab(f, stats = S, relabel = TRUE)$body
      if (mut == "drop") dd <- dd[dd$region != 2, ]
      if (mut == "perm") {
        # Row subsetting drops the columns' labels: put them back.
        dd <- dd[sample(nrow(dd)), ]
        for (v in names(dd)) attr(dd[[v]], "label") <- attr(mk()[[v]], "label")
      }
      if (mut == "edit") dd$age <- rev(dd$age)
      if (mut == "regroup") dd$region <- rev(dd$region)
      if (mut == "remove") rm(dd)
      got <- ht2b_try(regtab(f, stats = S, relabel = TRUE))
      lab <- paste("keep.data", keep, mut)
      expect_same_or_refused(got, want, "keep.data = TRUE", lab)
      # The fit's own copy of the data, or re-sorted data with their row
      # names, always give the same table.
      if (keep || mut == "perm") expect_identical(got, want, label = lab)
      # Filtered, edited or removed data of a fit that kept none refuse.
      if (!keep && mut != "perm") expect_s3_class(got, "ht2b_refusal")
    }
  }
})

test_that("a saved lme fit in a fresh session: the same table, or a refusal naming keep.data = TRUE", {
  skip_on_cran()
  rds <- tempfile(fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)
  out1 <- ht2b_fresh_r(c(
    "set.seed(8); d <- data.frame(g = rep(1:12, each = 10), x = rnorm(120))",
    "d$y <- 1 + 0.5 * d$x + rep(rnorm(12, sd = 0.5), each = 10) + rnorm(120)",
    "attr(d$x, 'label') <- 'Exposure'",
    "fits <- list(kept = nlme::lme(y ~ x, random = ~ 1 | g, data = d, method = 'ML'),",
    "  dropped = nlme::lme(y ~ x, random = ~ 1 | g, data = d, method = 'ML', keep.data = FALSE))",
    "tabs <- lapply(fits, function(f) regtab(f, stats = c('n', 'll', 'icc'))$body)",
    sprintf("saveRDS(list(fits = fits, tabs = tabs), %s)", deparse(rds)),
    "cat('saved\\n')"
  ))
  skip_if_not(any(grepl("^saved", out1)), paste(out1, collapse = "\n"))
  out2 <- ht2b_fresh_r(c(
    sprintf("s <- readRDS(%s)", deparse(rds)),
    "for (nm in names(s$fits)) {",
    "  r <- tryCatch(regtab(s$fits[[nm]], stats = c('n', 'll', 'icc'))$body, error = function(e) e)",
    "  res <- if (inherits(r, 'error')) paste('refused', grepl('keep.data = TRUE', conditionMessage(r), fixed = TRUE))",
    "    else if (identical(r, s$tabs[[nm]])) 'identical' else 'changed'",
    "  cat(nm, res, '\\n')",
    "}"
  ))
  res <- trimws(grep("^(kept|dropped) ", out2, value = TRUE))
  got <- stats::setNames(sub("^\\S+ ", "", res), sub(" .*$", "", res))
  expect_identical(got[c("kept", "dropped")], c(kept = "identical", dropped = "refused TRUE"))
})
