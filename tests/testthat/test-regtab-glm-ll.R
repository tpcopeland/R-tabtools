# Pre-release review P0-1: the log-likelihood, AIC and BIC of glm fits as
# Stata's glm reports them. Gamma and inverse Gaussian at scale 1, and
# analytic weights (vce = "stata" on a weighted glm, Stata's [aweight])
# rescaled to mean 1, where R's logLik() plugs in deviance/n and uses the raw
# weights. Stata expectations: tests/testthat/fixtures/regtab_glm_ll/
# (qa/stata/make_regtab_glm_ll.do, Stata 17 + tabtools 2.1.12), on auto.

gll_path <- function(...) test_path("fixtures", "regtab_glm_ll", ...)

gll_auto <- function() {
  d <- golden_fixture("auto")
  d$cl <- (seq_len(nrow(d)) - 1) %% 10
  d
}

gll_family <- function(family, link) {
  switch(family,
    gaussian = stats::gaussian(link),
    gamma = stats::Gamma(if (link == "power -1") "inverse" else link),
    igaussian = stats::inverse.gaussian(link))
}

# The R fit and regtab() call of one fixture row.
gll_run <- function(r, d = gll_auto(), stats = c("n", "ll", "aic", "bic")) {
  d$w_ <- if (r$weight == "none") 1 else d$turn
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  start <- if (r$link == "log") c(8, 0, 0)
  g <- if (r$weight == "none") {
    stats::glm(price ~ mpg + weight, gll_family(r$family, r$link), d, control = ctl, start = start)
  } else {
    stats::glm(price ~ mpg + weight, gll_family(r$family, r$link), d, weights = w_, control = ctl, start = start)
  }
  vce <- if (r$weight == "pw") {
    if (r$vce == "cluster") "cluster" else "robust"
  } else switch(r$vce, oim = "stata", r$vce)
  regtab(g, vce = vce, cluster = if (vce == "cluster") ~cl, stats = stats)
}

gll_fixture <- function() {
  utils::read.csv(gll_path("glm_ll.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
}

test_that("P0-1: glm ll, AIC and BIC equal Stata's e(ll) and estat ic (every family, weight and vce path)", {
  st <- gll_fixture()
  expect_setequal(st$id, sprintf("L%02d", c(1:6, 8:16)))
  d <- gll_auto()
  for (i in seq_len(nrow(st))) {
    r <- st[i, ]
    s <- gll_run(r, d)$stored
    expect_equal(s$ll_1, r$ll, tolerance = 1e-10, label = paste(r$id, "ll"))
    expect_equal(s$aic_1, r$aic, tolerance = 1e-10, label = paste(r$id, "AIC"))
    expect_equal(s$bic_1, r$bic, tolerance = 1e-10, label = paste(r$id, "BIC"))
    expect_equal(s$n_1, r$N, label = paste(r$id, "N"))
  }
})

test_that("P0-1: regtab's cells equal Stata's regtab, stats(n ll aic bic) csv()", {
  st <- gll_fixture()
  d <- gll_auto()
  for (id in c("L03", "L06", "L08", "L13", "L14")) {
    tt <- gll_run(st[st$id == id, ], d)
    mm <- golden_compare_cells(golden_as_cells(tt), golden_read_cells_file(gll_path(paste0(id, ".csv"))), mode = "exact")
    if (nrow(mm)) golden_fail(mm, paste("cells", id)) else succeed()
  }
})

test_that("P0-1: unweighted and unit-weight fits give the same statistics; gaussian stays logLik()", {
  d <- gll_auto()
  d$one <- 1
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  S <- c("ll", "aic", "bic")
  for (fam in list(stats::Gamma("log"), stats::inverse.gaussian("log"), stats::poisson())) {
    f <- if (fam$family == "poisson") mpg ~ weight else price ~ mpg + weight
    g0 <- stats::glm(f, fam, d, control = ctl, start = if (fam$family != "poisson") c(8, 0, 0))
    g1 <- stats::glm(f, fam, d, weights = one, control = ctl, start = if (fam$family != "poisson") c(8, 0, 0))
    s0 <- regtab(g0, stats = S)$stored
    for (v in c("stata", "model", "robust")) {
      s1 <- regtab(g1, vce = v, stats = S)$stored
      expect_equal(s1[c("ll_1", "aic_1", "bic_1")], s0[c("ll_1", "aic_1", "bic_1")], tolerance = 1e-12,
                   label = paste(fam$family, v))
    }
  }
  g <- stats::glm(price ~ mpg + weight, stats::gaussian(), d)
  expect_equal(regtab(g, stats = "ll")$stored$ll_1, as.numeric(stats::logLik(g)))
  # Quasi families have no likelihood: blank, as before.
  q <- stats::glm(price ~ mpg + weight, stats::quasi(link = "log", variance = "mu^2"), d,
                  control = ctl, start = c(8, 0, 0))
  ll <- regtab(q, stats = "ll")$stored$ll_1
  expect_true(is.null(ll) || is.na(ll))
})

test_that("P0-1: analytic weights are rescaled to mean 1 for binomial and poisson too (Stata glm [aw])", {
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  # Stata 17: glm mpg weight [aw=turn], family(poisson) -> e(ll) -195.17404727692363;
  # glm foreign mpg weight [aw=turn], family(binomial) -> -24.964046287167591.
  p <- stats::glm(mpg ~ weight, stats::poisson(), d, weights = turn, control = ctl)
  expect_equal(regtab(p, stats = "ll")$stored$ll_1, -195.17404727692363, tolerance = 1e-10)
  b <- stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = turn, control = ctl)
  s <- suppressWarnings(regtab(b, stats = c("ll", "r2_p"))$stored)
  expect_equal(s$ll_1, -24.964046287167591, tolerance = 1e-10)
  # The pseudo R-squared does not depend on the weights' scale.
  b2 <- stats::glm(foreign ~ mpg + weight, stats::binomial(), transform(d, turn = turn * 7), weights = turn,
                   control = ctl)
  expect_equal(suppressWarnings(regtab(b2, stats = c("ll", "r2_p"))$stored)$r2_p_1, s$r2_p_1, tolerance = 1e-10)
})

test_that("B01: weighted binomial and poisson glm standard errors are Stata glm [aw]'s, as the ll is", {
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  # The standard error implied by a ratio-scale row's 95% interval.
  ci_se <- function(tt, key) {
    r <- tt$meta$regtab_rows
    r <- r[r$key == key, ]
    (log(r$conf.high) - log(r$conf.low)) / (2 * stats::qnorm(0.975))
  }
  # Stata 17 (audit 2026-09-29, auto):
  #   glm foreign mpg weight [aw=turn], family(binomial) -> se(mpg) .098861409485961046,
  #     se(weight) .0010760604182465535, e(ll) -24.964046287167591;
  #   glm mpg weight [aw=turn], family(poisson) -> se(weight) .0000334639620334724,
  #     e(ll) -195.17404727692363.
  # glm [aw] rescales the weights to mean 1: the frequency-weight reading's
  # standard errors are sqrt(mean(turn)) = 6.3 times smaller.
  b <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = turn, control = ctl))
  tb <- suppressWarnings(regtab(b, stats = "ll"))
  expect_equal(ci_se(tb, "mpg"), 0.098861409481712181, tolerance = 1e-7)
  expect_equal(sqrt(tt_vcov(b)["mpg", "mpg"]), 0.098861409485961046, tolerance = 1e-7)
  expect_equal(sqrt(tt_vcov(b)["weight", "weight"]), 0.0010760604182465535, tolerance = 1e-7)
  expect_equal(tb$stored$ll_1, -24.964046287167591, tolerance = 1e-10)
  p <- stats::glm(mpg ~ weight, stats::poisson(), d, weights = turn, control = ctl)
  tp <- regtab(p, stats = "ll")
  expect_equal(ci_se(tp, "weight"), 3.34639620153458e-05, tolerance = 1e-7)
  expect_equal(tp$stored$ll_1, -195.17404727692363, tolerance = 1e-10)
  # The weights' scale does not matter, as for glm [aw].
  b7 <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), transform(d, turn = turn * 7),
                                    weights = turn, control = ctl))
  expect_equal(tt_vcov(b7), tt_vcov(b), tolerance = 1e-8)
  # vce = "model" stays the fit's own vcov().
  expect_equal(tt_vcov(b, "model"), stats::vcov(b))
  # Zero weights leave the estimation sample: mean over the positive weights.
  d$t0 <- ifelse(seq_len(nrow(d)) %% 9 == 0, 0, d$turn)
  b0 <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = t0, control = ctl))
  b0s <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d[d$t0 > 0, ],
                                     weights = t0 / mean(t0), control = ctl))
  expect_equal(tt_vcov(b0), tt_vcov(b0s), tolerance = 1e-8)
})

test_that("B01: robust/cluster, tt_mi() and regtab_uv() on weighted binomial/poisson glm follow Stata", {
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  b <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = turn, control = ctl))
  p <- stats::glm(mpg ~ weight, stats::poisson(), d, weights = turn, control = ctl)
  # Stata 17: glm ... [pw=turn] (robust) and [pw=turn], vce(cluster cl): unchanged by B01.
  expect_equal(sqrt(tt_vcov(b, "robust")["mpg", "mpg"]), 0.11837125349311281, tolerance = 1e-7)
  expect_equal(sqrt(tt_vcov(b, "cluster", ~cl)["mpg", "mpg"]), 0.11072601698250029, tolerance = 1e-7)
  expect_equal(sqrt(tt_vcov(p, "robust")["weight", "weight"]), 2.49453728868363e-05, tolerance = 1e-7)
  expect_equal(sqrt(tt_vcov(p, "cluster", ~cl)["weight", "weight"]), 1.97391883406439e-05, tolerance = 1e-7)
  # tt_mi() pools the same variance (identical imputations: the within variance).
  ci_se <- function(tt, key, model = 1) {
    r <- tt$meta$regtab_rows
    r <- r[r$key == key & r$model == model, ]
    (log(r$conf.high) - log(r$conf.low)) / (2 * stats::qnorm(0.975))
  }
  tm <- suppressWarnings(regtab(tt_mi(list(b, b))))
  expect_equal(ci_se(tm, "mpg"), 0.098861409485961046, tolerance = 1e-7)
  # regtab_uv(): Stata 17 glm foreign mpg [aw=turn] and glm foreign weight [aw=turn], family(binomial).
  uv <- suppressWarnings(regtab_uv(d, "foreign", c("mpg", "weight"), method = glm,
                                   method.args = list(family = binomial, weights = quote(turn), control = ctl)))
  tu <- suppressWarnings(regtab(uv))
  expect_equal(ci_se(tu, "mpg"), 0.055216939236687558, tolerance = 1e-7)
  expect_equal(ci_se(tu, "weight"), 0.0006378871324610129, tolerance = 1e-7)
})

test_that("B01: glm.nb, gaussian and Gamma weighted fits keep their variance (nbreg has no [aw])", {
  skip_if_not_installed("MASS")
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  # Stata 17 (fix probe 2026-09-29): glm price mpg weight [aw=turn], family(gaussian) -> se(mpg)
  # 91.008100166630101; family(gamma) link(log) -> .011762024006921289. nbreg and glm,
  # family(nbinomial ml) refuse [aw]; nbreg [iw=turn] -> se(mpg) .0017050371466199501,
  # e(ll) -26401.381766047714, which a weighted glm.nb reproduces.
  ga <- stats::glm(price ~ mpg + weight, stats::gaussian(), d, weights = turn)
  expect_equal(sqrt(tt_vcov(ga)["mpg", "mpg"]), 91.008100166630101, tolerance = 1e-9)
  gm <- stats::glm(price ~ mpg + weight, stats::Gamma("log"), d, weights = turn, control = ctl, start = c(8, 0, 0))
  expect_equal(sqrt(tt_vcov(gm)["mpg", "mpg"]), 0.011762024006921289, tolerance = 1e-7)
  nb <- suppressWarnings(MASS::glm.nb(price ~ mpg + weight, d, weights = turn, control = ctl))
  expect_equal(sqrt(tt_vcov(nb)["mpg", "mpg"]), 0.0017050371466199501, tolerance = 1e-6)
  expect_equal(suppressWarnings(regtab(nb, stats = "ll"))$stored$ll_1, -26401.381766047714, tolerance = 1e-9)
})

test_that("B07: profile intervals of weighted binomial/poisson glm follow the [aw] reading", {
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  prof <- function(tt) {
    r <- tt$meta$regtab_rows
    r <- r[r$status == "est", ]
    stats::setNames(data.frame(log(r$estimate), log(r$conf.low), log(r$conf.high)), c("b", "lo", "hi"))
  }
  b <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = turn, control = ctl))
  pr <- prof(suppressWarnings(regtab(b, ci_method = "profile")))
  wa <- prof(suppressWarnings(regtab(b)))
  expect_true(all(pr$lo < pr$b & pr$b < pr$hi))
  ratio <- (pr$hi - pr$lo) / (wa$hi - wa$lo)
  expect_true(all(ratio > 0.8 & ratio < 1.25))
  # The profile of the fit with its weights rescaled to mean 1 (Stata glm [aw]).
  d$tw <- d$turn / mean(d$turn)
  ba <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = tw, control = ctl))
  ci <- suppressWarnings(suppressMessages(stats::confint(ba)))[c("mpg", "weight"), ]
  expect_equal(unname(as.matrix(pr[, c("lo", "hi")])), unname(ci), tolerance = 1e-6)
  # The weights' scale does not matter; vce = "model" keeps confint() of the fit.
  b7 <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), transform(d, turn = turn * 7),
                                    weights = turn, control = ctl))
  expect_equal(prof(suppressWarnings(regtab(b7, ci_method = "profile"))), pr, tolerance = 1e-6)
  pm <- prof(suppressWarnings(regtab(b, ci_method = "profile", vce = "model")))
  expect_equal(unname(as.matrix(pm[, c("lo", "hi")])),
               unname(suppressMessages(stats::confint(b))[c("mpg", "weight"), ]), tolerance = 1e-6)
  # Poisson.
  p <- stats::glm(mpg ~ weight, stats::poisson(), d, weights = turn, control = ctl)
  pp <- prof(regtab(p, ci_method = "profile"))
  pa <- stats::glm(mpg ~ weight, stats::poisson(), d, weights = tw, control = ctl)
  expect_equal(c(pp$lo, pp$hi), unname(suppressMessages(stats::confint(pa))["weight", ]), tolerance = 1e-6)
  # Unweighted fits: confint() as before.
  u <- stats::glm(foreign ~ mpg + weight, stats::binomial(), d, control = ctl)
  pu <- prof(regtab(u, ci_method = "profile"))
  expect_equal(unname(as.matrix(pu[, c("lo", "hi")])),
               unname(suppressMessages(stats::confint(u))[c("mpg", "weight"), ]), tolerance = 1e-10)
})

test_that("B08: a MASS::negative.binomial(theta) glm has Stata glm, family(nbinomial k)'s scale 1 and [aw] reading", {
  skip_if_not_installed("MASS")
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  # Stata 17 (fix probe 2026-09-29, auto):
  #   glm price mpg weight, family(nbinomial 0.1) -> se(mpg) .0098206389251981607,
  #     se(weight) .0000732912406523936, e(ll) -664.31617427205254;
  #   ... [aw=turn] -> se(mpg) .010137321059380705, se(weight) .0000734894986553,
  #     e(ll) -666.14637589524114 ([iw]/[fw]: se(mpg) .0016099374655437948, ll -26411.80).
  g <- stats::glm(price ~ mpg + weight, MASS::negative.binomial(10), d, control = ctl)
  V <- tt_vcov(g)
  expect_equal(sqrt(V["mpg", "mpg"]), 0.0098206389251981607, tolerance = 1e-7)
  expect_equal(sqrt(V["weight", "weight"]), 0.0000732912406523936, tolerance = 1e-7)
  expect_equal(regtab(g, stats = "ll")$stored$ll_1, -664.31617427205254, tolerance = 1e-10)
  gw <- stats::glm(price ~ mpg + weight, MASS::negative.binomial(10), d, weights = turn, control = ctl)
  Vw <- tt_vcov(gw)
  expect_equal(sqrt(Vw["mpg", "mpg"]), 0.010137321059380705, tolerance = 1e-7)
  expect_equal(sqrt(Vw["weight", "weight"]), 0.0000734894986553, tolerance = 1e-7)
  expect_equal(regtab(gw, stats = "ll")$stored$ll_1, -666.14637589524114, tolerance = 1e-10)
  # vce = "model" stays vcov().
  expect_equal(tt_vcov(g, "model"), stats::vcov(g))
})

test_that("R01: a cbind() binomial response keeps its trials, even when every group is all-or-nothing", {
  set.seed(5)
  n <- 60
  d <- data.frame(x = rnorm(n), m = sample(2:4, n, TRUE))
  d$s <- ifelse(runif(n) < plogis(d$x), d$m, 0)
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  fc <- stats::glm(cbind(s, m - s) ~ x, stats::binomial(), d, control = ctl)
  # Stata 17 (review probe cb.do): glm s x, family(binomial m) -> se(x) .18234483359598735,
  # e(ll) -108.40030322618196.
  expect_equal(sqrt(tt_vcov(fc)["x", "x"]), 0.18234483359598735, tolerance = 1e-7)
  expect_equal(regtab(fc, stats = "ll")$stored$ll_1, -108.40030322618196, tolerance = 1e-7)
  # model = FALSE: the trials are read from the re-evaluated frame.
  fm <- stats::glm(cbind(s, m - s) ~ x, stats::binomial(), d, control = ctl, model = FALSE)
  expect_equal(tt_vcov(fm), tt_vcov(fc))
  # Mixed groups: unchanged, the trials are the weights of the variance.
  d$s2 <- stats::rbinom(n, d$m, stats::plogis(d$x))
  f2 <- stats::glm(cbind(s2, m - s2) ~ x, stats::binomial(), d, control = ctl)
  expect_equal(tt_vcov(f2), stats::vcov(f2), tolerance = 1e-6)
  expect_equal(regtab(f2, stats = "ll")$stored$ll_1, as.numeric(stats::logLik(f2)))
  # A cbind() fit with its own weights = still gets the weight warnings
  # (the trials are not the weights), with and without model = FALSE.
  d$wi <- stats::runif(n, 0.5, 3)
  for (keep in c(TRUE, FALSE)) {
    fw <- suppressWarnings(stats::glm(cbind(s2, m - s2) ~ x, stats::binomial(), d, weights = wi, model = keep))
    expect_warning(regtab(fw), "inverse-probability weights", label = paste("model =", keep))
  }
})

test_that("R03: a weight column rescaled after fitting stops the [aw] profile", {
  d <- gll_auto()
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 500)
  b <- suppressWarnings(stats::glm(foreign ~ mpg + weight, stats::binomial(), d, weights = turn, control = ctl))
  d$turn <- d$turn * 2
  expect_error(suppressWarnings(regtab(b, ci_method = "profile")), "weights")
  d$turn <- d$turn / sum(d$turn)
  expect_error(suppressWarnings(regtab(b, ci_method = "profile")), "weights")
  # The Wald interval uses the fit's own weights and is unaffected.
  expect_no_error(suppressWarnings(regtab(b)))
})

test_that("R04: profile intervals are refused for a MASS::negative.binomial(theta) glm", {
  skip_if_not_installed("MASS")
  d <- gll_auto()
  g <- stats::glm(price ~ mpg + weight, MASS::negative.binomial(10), d)
  expect_error(regtab(g, ci_method = "profile"), "ci_method = \"wald\"")
  expect_error(regtab(g, ci_method = "profile", vce = "model"), "negative.binomial")
  expect_no_error(regtab(g))
})
