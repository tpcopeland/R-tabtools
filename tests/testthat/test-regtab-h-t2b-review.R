# Regression tests for the independent review of Milestone H group t2b
# (IMPLEMENTATION_PLAN.md, "Group t2b review outcome"): one test per
# finding, built from the review's repro scripts, and one per surviving
# mutant. The Fine-Gray findings T2B-01/T2B-02 are in
# test-regtab-h3-finegray.R, T2B-07 in test-regtab-h19-gee-scale.R and
# T2B-11 in test-regtab-h4-storage.R.

rv_rows <- function(tt) tt$body[[1]]
rv_cell <- function(tt, label, col = 2L) tt$body[[col]][tt$body[[1]] == label]
# The cell of the row with raw key `key` ("p" for a covariate, "/::p" for
# the ancillary parameter): since tabtools 2.1.12 both read "p".
rv_key <- function(tt, key, col = 2L) {
  r <- tt$meta$regtab_rows
  tt$body[[col]][unique(r$row[r$key == key])]
}

# ---------------------------------------------------------------------------
# T2B-03 (and T2B-14): structural keys never meet a covariate key. Since
# tabtools 2.1.12 the structural row is labelled with its own name (not
# the 2.1.11 colname "/p"), as Stata shows a covariate named like its own
# model's ancillary parameter (goldens R58-R63).

test_that("a covariate p beside a Weibull's p: separate rows in both orders (T2B-03, R04)", {
  skip_if_not_installed("survival")
  l <- survival::lung
  l$p <- l$ph.karno / 10
  w1 <- survival::survreg(survival::Surv(time, status) ~ age + p, l)
  w2 <- survival::survreg(survival::Surv(time, status) ~ age, l)
  tr_p <- sprintf("%.2f", exp(coef(w1)[["p"]]))
  shape2 <- sprintf("%.2f", 1 / w2$scale)
  for (ord in list(c(1, 2), c(2, 1))) {
    fits <- list(w1, w2)[ord]
    m1 <- which(ord == 1)  # the column block of the model with the covariate
    cov_col <- 2L + 3L * (m1 - 1L)
    oth_col <- 2L + 3L * (2L - m1)
    # Default nointercept: the covariate is shown, only in its model.
    t0 <- regtab(fits[[1]], fits[[2]])
    expect_identical(rv_rows(t0), c("age", "p"))
    expect_identical(rv_cell(t0, "p", cov_col), tr_p)
    expect_identical(rv_cell(t0, "p", oth_col), "")
    # keepintercept: the Weibull p is its own row in both models, labelled
    # p as the covariate (Stata 2.1.12's label; Stata puts the two in one
    # row when they come from different models, which R does not copy:
    # take_action item 15); the covariate row keeps model 2's cell empty.
    tk <- regtab(fits[[1]], fits[[2]], keepintercept = TRUE)
    expect_identical(sum(rv_rows(tk) == "p"), 2L)
    expect_identical(rv_key(tk, "p", cov_col), tr_p)
    expect_identical(rv_key(tk, "p", oth_col), "")
    expect_identical(rv_key(tk, "/::p", oth_col), shape2)
  }
})

test_that("nbreg alpha, ologit cut1 and lognormal sigma never share a covariate's row (T2B-03)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  d <- MASS::quine
  d$alpha <- as.numeric(d$Age)
  n1 <- MASS::glm.nb(Days ~ Sex + alpha, d)
  n2 <- MASS::glm.nb(Days ~ Sex, d)
  irr <- sprintf("%.2f", exp(coef(n1)[["alpha"]]))
  for (fits in list(list(n1, n2), list(n2, n1))) {
    t0 <- regtab(fits[[1]], fits[[2]])
    expect_identical(sum(rv_rows(t0) == "alpha"), 1L)
    expect_true(irr %in% t0$body[t0$body[[1]] == "alpha", c(2, 5)])
    expect_identical(sort(unname(unlist(t0$body[t0$body[[1]] == "alpha", c(2, 5)]))), sort(c("", irr)))
    tk <- regtab(fits[[1]], fits[[2]], keepintercept = TRUE)
    expect_identical(sum(rv_rows(tk) == "alpha"), 2L)
    expect_false(any(c(rv_key(tk, "/::alpha", 2L), rv_key(tk, "/::alpha", 5L)) == ""))
  }
  # ologit cut1 beside a logit with a covariate cut1.
  m <- mtcars
  m$cut1 <- m$mpg
  o <- MASS::polr(factor(gear) ~ wt, m, Hess = TRUE)
  g <- glm(am ~ cut1 + wt, binomial, m)
  for (fits in list(list(o, g), list(g, o))) {
    t0 <- regtab(fits[[1]], fits[[2]])
    expect_identical(rv_rows(t0), if (inherits(fits[[1]], "polr")) c("wt", "cut1") else c("cut1", "wt"))
    tk <- regtab(fits[[1]], fits[[2]], keepintercept = TRUE, cutlabels = c("A", "B"))
    expect_true(all(c("cut1", "A", "B") %in% rv_rows(tk)))
  }
  # lognormal sigma (kept by nointercept, as Stata) beside a covariate sigma.
  l <- survival::lung
  l$sigma <- l$ph.karno / 10
  ln <- survival::survreg(survival::Surv(time, status) ~ age, l, dist = "lognormal")
  lc <- survival::survreg(survival::Surv(time, status) ~ age + sigma, l, dist = "lognormal")
  t0 <- regtab(ln, lc)
  expect_identical(sum(rv_rows(t0) == "sigma"), 2L)
  expect_identical(rv_key(t0, "sigma", 2L), "")
  expect_identical(rv_key(t0, "sigma", 5L), sprintf("%.2f", exp(coef(lc)[["sigma"]])))
})

test_that("keep()/drop() still select structural rows by their bare name (T2B-03)", {
  skip_if_not_installed("survival")
  w <- survival::survreg(survival::Surv(time, status) ~ age, survival::lung)
  expect_identical(rv_rows(regtab(w, keepintercept = TRUE, keep = "ln_p p")), c("ln_p", "p"))
  expect_identical(rv_rows(regtab(w, keepintercept = TRUE, drop = "1/p _cons")), c("age", "ln_p", "p"))
})

# ---------------------------------------------------------------------------
# T2B-04: coxph with tt()

test_that("a coxph with tt() terms is tabled from its stored fit; row-pairing paths refuse without model = TRUE (T2B-04, R05)", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  f <- survival::coxph(survival::Surv(time, status) ~ age + ph.karno + tt(ph.karno), lung,
                       tt = function(x, t, ...) x * log(t + 20))
  tt <- suppressMessages(suppressWarnings(regtab(f, stats = c("n", "ll", "aic"))))
  expect_identical(rv_rows(tt)[1:3], c("age", "ph.karno", "tt(ph.karno)"))
  expect_identical(rv_cell(tt, "Subjects"), "227")
  expect_equal(tt$meta$regtab_rows$estimate[1:3], unname(exp(coef(f))), tolerance = 1e-12)
  # survival refuses model = TRUE with tt(); no message may suggest it.
  expect_error(survival::coxph(survival::Surv(time, status) ~ age + tt(age), lung,
                               tt = function(x, t, ...) x * log(t), model = TRUE))
  r <- tryCatch(suppressMessages(suppressWarnings(regtab(f, vce = "robust"))), error = function(e) e)
  expect_s3_class(r, "error")
  expect_match(conditionMessage(r), "vce = \"stata\"", fixed = TRUE)
  expect_false(grepl("model = TRUE`", sub("does not allow `model = TRUE`", "", conditionMessage(r)), fixed = TRUE))
  # A robust tt() fit keeps survival's sandwich, with Stata's G/(G - 1).
  l2 <- lung[!is.na(lung$inst), ]
  g <- survival::coxph(survival::Surv(time, status) ~ age + tt(age), l2, tt = function(x, t, ...) x * log(t + 20),
                       cluster = inst)
  expect_equal(unname(sqrt(diag(tt_vcov(g)) / diag(vcov(g)))), rep(sqrt(18 / 17), 2), tolerance = 1e-12)
  # Data changed after fitting: refused, without model = TRUE advice.
  lung <- lung[-(1:3), ]
  r <- tryCatch(regtab(f), error = function(e) e)
  expect_match(conditionMessage(r), "Refit it on the current data")
  expect_false(grepl("Refit it with `model = TRUE`", conditionMessage(r), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# T2B-05: weighted coxph(y = FALSE) keeps ll/AIC/BIC under robust/cluster

test_that("a weighted Breslow coxph(y = FALSE) under robust/cluster keeps ll, AIC and BIC (T2B-05, R03)", {
  skip_if_not_installed("survival")
  set.seed(11)
  n <- 240
  d <- data.frame(x = rnorm(n), id = rep(1:48, each = 5))
  d$t <- rexp(n, exp(0.2 * d$x))
  d$e <- rbinom(n, 1, 0.7)
  d$w <- sample(1:3, n, TRUE)
  a <- survival::coxph(survival::Surv(t, e) ~ x, d, ties = "breslow", weights = w)
  b <- survival::coxph(survival::Surv(t, e) ~ x, d, ties = "breslow", weights = w, y = FALSE)
  S <- c("n", "ll", "aic", "bic")
  for (v in c("robust", "cluster")) {
    cl <- if (v == "cluster") ~id else NULL
    A <- suppressMessages(regtab(a, vce = v, cluster = cl, stats = S))
    B <- suppressMessages(regtab(b, vce = v, cluster = cl, stats = S))
    expect_identical(B$body, A$body, label = v)
    expect_true(all(c("AIC", "BIC", "Log-likelihood") %in% rv_rows(B)), label = v)
  }
})

# ---------------------------------------------------------------------------
# T2B-08, T2B-10: multinom refusals with their true reason; empty levels;
# factors relevelled after fitting

test_that("multinom: summ and censored refused by name; an empty level omitted; relevelled factors keep the fit's coding (T2B-08, T2B-10, R06)", {
  skip_if_not_installed("nnet")
  set.seed(3)
  n <- 200
  d <- data.frame(x = rnorm(n), g = factor(sample(letters[1:3], n, TRUE)))
  d$y <- factor(sample(c("A", "B", "C"), n, TRUE))
  # (a) A level the subset leaves empty: its rows are omitted (Stata omits
  # the level), not refused as "singular" or shown as 1.00 (1.00, 1.00).
  fa <- nnet::multinom(y ~ x + g, d, subset = g != "c", trace = FALSE, model = TRUE)
  ta <- regtab(fa)
  expect_false(any(grepl("3.g", rv_rows(ta), fixed = TRUE)))
  # (tabtools 2.1.12's layout: a header "B: g" above "B:   a", "B:   b").
  expect_true(all(c("B: g", "B:   a", "B:   b", "C:   b", "B: x") %in% rv_rows(ta)))
  expect_false(any(c("B:   c", "C:   c") %in% rv_rows(ta)))
  ref <- nnet::multinom(y ~ x + g, droplevels(d[d$g != "c", ]), trace = FALSE, model = TRUE)
  expect_identical(ta$body, regtab(ref)$body)
  # (b) summ, (c) censored: refused by name.
  utils::capture.output(fb <- suppressMessages(nnet::multinom(y ~ g, d, summ = 2, trace = FALSE)))
  expect_error(regtab(fb), "summ = 2", fixed = TRUE)
  C <- nnet::class.ind(d$y)
  C[1:10, ] <- 1
  d$C <- C
  fc <- nnet::multinom(C ~ g, d, censored = TRUE, trace = FALSE, model = TRUE)
  expect_error(regtab(fc), "censored = TRUE", fixed = TRUE)
  # T2B-10: a factor relevelled after fitting.
  f <- nnet::multinom(y ~ x + g, d, trace = FALSE)
  want <- regtab(f)$body
  d$g <- stats::relevel(d$g, "c")
  expect_identical(regtab(f)$body, want)
  d$y <- stats::relevel(d$y, "C")
  expect_identical(regtab(f)$body, want)
})

test_that("multinom: a nonzero base-outcome offset column and a response edited after fitting (mutants M02, M05)", {
  skip_if_not_installed("nnet")
  set.seed(482)
  n <- 300
  d <- data.frame(y = factor(sample(1:3, n, TRUE)), x = rnorm(n))
  d$off <- I(cbind(rnorm(n), rnorm(n, sd = 2), rnorm(n, sd = 2)))
  f <- nnet::multinom(y ~ x + offset(off), d, trace = FALSE, reltol = 1e-12, maxit = 1000)
  X <- model.matrix(f)
  iy <- as.integer(d$y)
  nll <- function(b) {
    B <- matrix(b, 2, 2, byrow = TRUE)
    eta <- cbind(0, X %*% t(B)) + d$off
    mx <- apply(eta, 1, max)
    sum(mx + log(rowSums(exp(eta - mx))) - eta[cbind(seq_len(n), iy)])
  }
  oracle <- sqrt(diag(solve(optimHess(as.vector(t(coef(f))), nll))))
  expect_equal(unname(sqrt(diag(tt_vcov(f)))), oracle, tolerance = 1e-6)
  # Only the response edited: refused (the pseudo R2 would change).
  f2 <- nnet::multinom(y ~ x, d, trace = FALSE)
  d$y <- factor(rev(as.integer(d$y)))
  expect_error(regtab(f2), "the response differs")
})

test_that("multinom weight decay in (0, 1] is refused too (mutant M06)", {
  skip_if_not_installed("nnet")
  set.seed(1)
  d <- data.frame(y = factor(sample(c("a", "b", "c"), 200, TRUE)), x = rnorm(200))
  expect_error(regtab(nnet::multinom(y ~ x, d, trace = FALSE, decay = 0.1)), "decay = 0.1", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# T2B-09: cloglog stores no pseudo R-squared (Stata C1, C2)

test_that("cloglog: no pseudo R-squared, with or without an offset, as Stata (T2B-09; C1, C2)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- ht2b_read("offs")
  ctl <- glm.control(epsilon = 1e-14, maxit = 100)
  expect_identical(ht2b_e("C1")$e[["r2_p"]], NA_real_)
  expect_ht2b(regtab(glm(yb ~ x, binomial("cloglog"), d, control = ctl), stats = c("n", "ll", "r2")), "C1", 2e-8)
  # C2: r(table) 2.9e-7 from Stata's (cloglog stops at its default ML
  # tolerance; R's glm is converged to 1e-14).
  expect_ht2b(regtab(glm(yb ~ x + offset(off), binomial("cloglog"), d, control = ctl), stats = c("n", "ll", "r2")),
              "C2", 5e-7)
  expect_true(is.na(tabtools:::tt_model_stats(glm(yb ~ x, binomial("cloglog"), d), NULL)$r2_p))
  # Poisson with another link is Stata's glm command: none either.
  expect_true(is.na(tabtools:::tt_model_stats(glm(yc ~ x, poisson("sqrt"), d), NULL)$r2_p))
})

test_that("the [pw] reading blanks a binomial fit's pseudo R-squared with an offset (mutant M30)", {
  skip_on_cran()
  skip_if_not_installed("haven")
  d <- ht2b_read("offs")
  ctl <- glm.control(epsilon = 1e-14, maxit = 100)
  d$w <- 1 + (d$id %% 3) / 2
  # Probability weights with vce = "robust": Stata's [pw] reading.
  g <- suppressWarnings(glm(yb ~ x + offset(off), binomial, d, weights = w, control = ctl))
  expect_false("Pseudo R\u00b2" %in% rv_rows(regtab(g, vce = "robust", stats = c("n", "ll", "r2"))))
  g0 <- suppressWarnings(glm(yb ~ x, binomial, d, weights = w, control = ctl))
  expect_true("Pseudo R\u00b2" %in% rv_rows(regtab(g0, vce = "robust", stats = c("n", "ll", "r2"))))
})

# ---------------------------------------------------------------------------
# Mutant gaps in H4/H5

test_that("polr(model = FALSE): an edited response alone is refused (mutant M22)", {
  skip_if_not_installed("MASS")
  set.seed(2)
  d <- data.frame(x = rnorm(150))
  d$y <- factor(cut(d$x + rnorm(150), c(-Inf, -0.5, 0.5, Inf)), ordered = TRUE)
  f <- MASS::polr(y ~ x, d, model = FALSE, Hess = TRUE)
  expect_s3_class(regtab(f), "tt_table")
  d$y <- factor(rev(as.character(d$y)), levels = levels(d$y), ordered = TRUE)
  expect_error(regtab(f), "the response differs")
})

test_that("hurdle with a count-distribution zero part: observed information, not pscl's vcov() (mutant M23)", {
  skip_if_not_installed("pscl")
  set.seed(5)
  n <- 400
  d <- data.frame(x = rnorm(n), z = rnorm(n))
  d$y <- ifelse(runif(n) < stats::plogis(-0.3 + 0.5 * d$z), rpois(n, exp(0.4 + 0.3 * d$x)) + 1, 0)
  for (zd in c("poisson", "negbin", "geometric")) {
    h <- suppressWarnings(pscl::hurdle(y ~ x | z, d, dist = "poisson", zero.dist = zd))
    V <- tt_vcov(h)
    expect_false(isTRUE(all.equal(V, vcov(h), tolerance = 1e-12)), label = zd)
    expect_equal(unname(sqrt(diag(V))), unname(sqrt(diag(vcov(h)))), tolerance = 2e-3, label = zd)
    expect_s3_class(regtab(h), "tt_table")
  }
})

test_that("an Ancillary: row beside zip/zinb is dropped by nointercept (mutant M37)", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("survival")
  set.seed(6)
  n <- 300
  d <- data.frame(x = rnorm(n))
  d$y <- ifelse(runif(n) < 0.3, 0, rpois(n, exp(0.5 + 0.3 * d$x)))
  d$t <- rexp(n, exp(0.2 * d$x))
  d$e <- rbinom(n, 1, 0.7)
  z <- pscl::zeroinfl(y ~ x | 1, d)
  ln <- survival::survreg(survival::Surv(t, e) ~ x, d, dist = "lognormal")
  tt <- regtab(z, ln, nointercept = TRUE)
  expect_false(any(grepl("^Ancillary:", rv_rows(tt))))
  tk <- regtab(z, ln, keepintercept = TRUE)
  expect_true(any(grepl("^Ancillary: lnsigma", rv_rows(tk))))
})

test_that("an unsorted geeglm is refused with vce = \"model\" and stats = \"qic\" too (mutant M19, T2B-17)", {
  skip_if_not_installed("geepack")
  set.seed(4)
  d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
  d$y <- d$x + rnorm(30)[d$id] + rnorm(120)
  ds <- d[sample(nrow(d)), ]
  g <- geepack::geeglm(y ~ x, gaussian, ds, id = id, corstr = "exchangeable")
  expect_error(regtab(g, vce = "model", stats = "qic"), "not sorted by `id`", fixed = TRUE)
  # The up-front check names the model.
  ok <- geepack::geeglm(y ~ x, gaussian, d, id = id, corstr = "exchangeable")
  expect_error(regtab(ok, g, vce = "model"), "(model 2)", fixed = TRUE)
  expect_error(tt_vcov(g, "model"), "Sort the data by")
})

# ---------------------------------------------------------------------------
# P3 items

test_that("a standalone tt_vcov() refusal names no model number (T2B-13)", {
  skip_if_not_installed("pscl")
  set.seed(6)
  d <- data.frame(x = rnorm(200))
  d$y <- ifelse(runif(200) < 0.3, 0, rpois(200, exp(0.5 + 0.3 * d$x)))
  z <- pscl::zeroinfl(y ~ x | 1, d, model = FALSE)
  r <- tryCatch(tt_vcov(z), error = function(e) conditionMessage(e))
  expect_false(grepl("model 1", r, fixed = TRUE))
  expect_error(regtab(z), "(model 1)", fixed = TRUE)
})

test_that("a glm.nb whose constant-only model fails leaves the pseudo R-squared blank with a note (T2B-16)", {
  skip_if_not_installed("MASS")
  f <- MASS::glm.nb(Days ~ Age, MASS::quine)
  local_mocked_bindings(.rt_negbin_ll0 = function(fit) NA_real_)
  expect_message(tt <- regtab(f, stats = c("n", "r2")), "constant-only negative binomial")
  expect_false("Pseudo R\u00b2" %in% rv_rows(tt))
})
