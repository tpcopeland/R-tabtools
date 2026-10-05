# Milestone H task H3 (external review F04, F25): Fine-Gray subjects that
# survive re-sorting the expanded data, delayed entry, and detection from
# the fit's structure rather than finegray()'s column names.

h3_data <- function() {
  set.seed(483)
  d <- data.frame(t = rexp(80), e = factor(sample(c("censor", "a", "b"), 80, TRUE),
                                          levels = c("censor", "a", "b")),
                  x = rnorm(80), id = 1:80)
  attr(d$x, "label") <- "Exposure"
  d
}

h3_head <- function(tt) tt$header[[2]]$text[2]

test_that("re-sorting the expanded data changes nothing: variance, Subjects, label (F04)", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  f1 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  # Oracle: the dfbeta sandwich collapsed on the real subject id, N/(N - 1).
  D <- as.matrix(residuals(f1, type = "dfbeta", collapse = fg$id, weighted = TRUE))
  oracle <- crossprod(D) * 80 / 79
  expect_equal(c(oracle), 0.02840656, tolerance = 1e-6)
  want <- regtab(f1, stats = "n")
  expect_equal(c(tt_vcov(f1)), c(oracle), tolerance = 1e-12)
  expect_identical(want$body[[1]], c("Exposure", "Subjects"))
  expect_identical(want$body[[2]][2], "80")
  fs <- fg[order(fg$fgstop), ]
  set.seed(9)
  fr <- fg[sample(nrow(fg)), ]
  for (dd in list(fs, fr)) {
    f2 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = dd, weights = fgwt)
    expect_equal(c(tt_vcov(f2)), c(oracle), tolerance = 1e-12)
    expect_identical(regtab(f2, stats = "n")$body, want$body)
  }
  # With the subject id on the fit (cluster()), the same.
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fr, weights = fgwt, cluster = id)
  expect_equal(c(tt_vcov(fc)), c(oracle), tolerance = 1e-12)
})

test_that("a matrix covariate (a spline basis) does not break the subject layout", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ splines::ns(x, 3), data = fg, weights = fgwt)
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ splines::ns(x, 3), data = fg, weights = fgwt,
                        cluster = id)
  expect_equal(tt_vcov(f), tt_vcov(fc), tolerance = 1e-12)
})

# Review T2B-01: under delayed entry finegray() can write weight-1
# continuation records, so subjects are never inferred from the layout
# there: 50 random datasets without an id are refused (never a different
# number), and with the id on the fit (cluster = id or id = id) they give
# the subject-clustered variance and Subjects.
test_that("delayed entry: refused without an id, the id's variance with one (T2B-01, 50 seeds)", {
  skip_on_cran()  # heavy (CRAN-lane runtime, pre-release review P2-1); runs in every NOT_CRAN gate
  skip_if_not_installed("survival")
  cont1 <- 0L
  for (seed in 1:50) {
    set.seed(seed)
    n <- sample(c(20, 50, 120), 1)
    pc <- runif(1, 0.05, 0.6)
    d <- data.frame(t = rexp(n), e = factor(sample(c("censor", "a", "b"), n, TRUE, prob = c(pc, (1 - pc) * c(.5, .5))),
                                            levels = c("censor", "a", "b")),
                    x = rnorm(n), id = seq_len(n))
    d$t0 <- round(runif(n, 0, 0.3) * d$t, 3)
    fg <- survival::finegray(survival::Surv(t0, t, e) ~ x + id, data = d, id = id, etype = "a")
    cont1 <- cont1 + sum(duplicated(fg$id) & fg$fgwt == 1)
    f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
    fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, cluster = id)
    fi <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, id = id)
    lab <- paste("seed", seed)
    expect_error(tt_vcov(f), "cluster = id", fixed = TRUE, label = lab)
    expect_error(regtab(f), "cannot be identified", label = lab)
    D <- as.matrix(residuals(fc, type = "dfbeta", collapse = fg$id, weighted = TRUE))
    oracle <- crossprod(D) * n / (n - 1)
    expect_equal(c(tt_vcov(fc)), c(oracle), tolerance = 1e-12, label = lab)
    expect_equal(c(tt_vcov(fi)), c(oracle), tolerance = 1e-12, label = lab)
    tc <- regtab(fc, stats = "n")
    expect_identical(tc$body[[2]][tc$body[[1]] == "Subjects"], as.character(n), label = lab)
    expect_identical(suppressMessages(regtab(fi, stats = "n"))$body, tc$body, label = lab)
  }
  # The seeds include the case the review found (weight-1 continuations).
  expect_gt(cont1, 0)
})

# The review's R01 data: three weight-1 continuation records.
test_that("the review's delayed-entry repro is refused without an id (T2B-01, R01)", {
  skip_if_not_installed("survival")
  set.seed(310)
  n <- sample(c(20, 50, 120), 1)
  t <- rexp(n)
  pc <- runif(1, 0.05, 0.6)
  e <- factor(sample(c("censor", "a", "b", "c"), n, TRUE, prob = c(pc, (1 - pc) * c(.4, .4, .2))),
              levels = c("censor", "a", "b", "c"))
  d <- data.frame(t = t, e = e, x = rnorm(n), id = seq_len(n))
  d$t0 <- round(runif(n, 0, 0.3) * d$t, 3)
  fg <- survival::finegray(survival::Surv(t0, t, e) ~ x + id, data = d, id = id, etype = "a")
  expect_identical(sum(duplicated(fg$id) & fg$fgwt == 1), 3L)
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  expect_error(regtab(f, stats = "n"), "cluster = id", fixed = TRUE)
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, cluster = id)
  expect_identical(regtab(fc, stats = "n")$body[[2]][2], "20")
})

# Review T2B-02: counting-process input (a subject split into pieces, each
# of weight 1) is refused without an id and counts subjects with one.
test_that("counting-process input to finegray(id =): refused without an id, 80 subjects with one (T2B-02, R02)", {
  skip_if_not_installed("survival")
  set.seed(483)
  n <- 80
  d <- data.frame(t = rexp(n), e = factor(sample(c("censor", "a", "b"), n, TRUE), levels = c("censor", "a", "b")),
                  x = rnorm(n), z0 = rbinom(n, 1, .5), id = 1:n)
  a <- data.frame(id = d$id, t0 = 0, t1 = d$t / 2, e = factor("censor", levels(d$e)), x = d$x, z = d$z0)
  b <- data.frame(id = d$id, t0 = d$t / 2, t1 = d$t, e = d$e, x = d$x, z = 1 - d$z0)
  cp <- rbind(a, b)
  cp <- cp[order(cp$id, cp$t0), ]
  for (form in list(survival::Surv(t0, t1, e) ~ x + z + id, survival::Surv(t0, t1, e) ~ x + id)) {
    fg <- survival::finegray(form, data = cp, id = id, etype = "a")
    rhs <- if ("z" %in% names(fg)) survival::Surv(fgstart, fgstop, fgstatus) ~ x + z else
      survival::Surv(fgstart, fgstop, fgstatus) ~ x
    f <- survival::coxph(rhs, data = fg, weights = fgwt)
    fc <- survival::coxph(rhs, data = fg, weights = fgwt, cluster = id)
    expect_error(tt_vcov(f), "cannot be identified")
    expect_error(regtab(f, stats = "n"), "cluster = id", fixed = TRUE)
    expect_identical(regtab(fc, stats = "n")$body[[2]][regtab(fc, stats = "n")$body[[1]] == "Subjects"], "80")
  }
  # finegray = TRUE on an ordinary counting-process fit (all weights 1, no
  # finegray() mark, no id): refused, never one subject per record.
  lung <- survival::lung
  Surv <- survival::Surv
  sp <- survival::survSplit(Surv(time, status) ~ ., lung, cut = c(180, 365), episode = "ep")
  sp$w <- 1
  fs <- survival::coxph(Surv(tstart, time, status) ~ age, sp, weights = w)
  expect_error(regtab(fs, finegray = TRUE), "cannot be identified")
})

# A continuation record with a weight of 1 or more is not finegray()'s
# right-censored layout (case weights): refused (review mutant N02).
test_that("a finegray() continuation record with weight 1 or more is refused", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  j <- which(duplicated(fg$id))[1]
  fg$fgwt[j] <- 1.2
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  expect_error(tt_vcov(f), "cannot be identified")
})

# Mutant M11: a continuation record whose covariate differs from the
# subject's first record breaks the layout: refused.
test_that("a finegray() layout whose continuation changes a covariate is refused", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  j <- which(duplicated(fg$id))[1]
  fg$x[j] <- fg$x[j] + 1
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  expect_error(tt_vcov(f), "cannot be identified")
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, cluster = id)
  expect_s3_class(regtab(fc), "tt_table")
})

test_that("detection is structural: renamed columns, unweighted fits, explicit finegray (F25)", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  f1 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  oracle <- c(tt_vcov(f1))
  r <- fg
  names(r)[match(c("fgstart", "fgstop", "fgstatus", "fgwt"), names(r))] <- c("a0", "a1", "st", "w")
  f2 <- survival::coxph(survival::Surv(a0, a1, st) ~ x, data = r, weights = w)
  expect_identical(tabtools:::tt_model_info(f2)$effect_scale, "SHR")
  expect_equal(c(tt_vcov(f2)), oracle, tolerance = 1e-12)
  expect_identical(h3_head(regtab(f2)), "SHR")
  # An unweighted fit on finegray() data is a Cox model, with a note that
  # names the evidence.
  f3 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg)
  expect_identical(tabtools:::tt_model_info(f3)$effect_scale, "HR")
  expect_message(regtab(f3), "not weighted")
  # Data without finegray()'s mark: a Cox model with a note, unless declared.
  u <- fg
  attr(u, "event") <- NULL
  f4 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = u, weights = fgwt)
  expect_identical(tabtools:::tt_model_info(f4)$effect_scale, "HR")
  expect_message(t4 <- regtab(f4), "finegray = TRUE")
  expect_identical(h3_head(t4), "HR")
  # Declared on data without the mark, the subjects need the id (T2B-02).
  expect_error(regtab(f4, finegray = TRUE, stats = "n"), "cannot be identified")
  f4c <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = u, weights = fgwt, cluster = id)
  t5 <- regtab(f4c, finegray = TRUE, stats = "n")
  expect_identical(h3_head(t5), "SHR")
  expect_equal(t5$stored$n_1, 80)
  expect_equal(c(tt_vcov(f4c)), oracle, tolerance = 1e-12)
  f1 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  expect_identical(h3_head(regtab(f1, finegray = FALSE)), "HR")
  # Declarations that cannot hold.
  expect_error(regtab(f3, finegray = TRUE), "cannot be a Fine-Gray fit")
  expect_error(regtab(lm(mpg ~ wt, mtcars), finegray = TRUE), "coxph")
  expect_error(regtab(f4, lm(mpg ~ wt, mtcars), finegray = c(TRUE, TRUE)), "model 2")
  expect_error(regtab(f4, finegray = NA), "finegray")
})

test_that("subjects that cannot be identified are refused, never counted as records", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  fz <- fg[order(fg$fgstop), ]
  rownames(fz) <- NULL
  fz_fit <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fz, weights = fgwt)
  expect_error(tt_vcov(fz_fit), "cannot be identified")
  expect_error(regtab(fz_fit), "cluster = id")
  # The model-based variance needs no subjects, and Subjects is then blank.
  tt <- regtab(fz_fit, vce = "model", stats = "n")
  expect_identical(tt$body[[2]][tt$body[[1]] == "Subjects"], character())
  # With the id on the fit it works again.
  fzc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fz, weights = fgwt, cluster = id)
  f1 <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  expect_equal(c(tt_vcov(fzc)), c(tt_vcov(f1)), tolerance = 1e-12)
})

test_that("Fine-Gray data changed after fitting: the same table, or a refusal (F24, H-D7)", {
  skip_if_not_installed("survival")
  d <- h3_data()
  fg <- survival::finegray(survival::Surv(t, e) ~ x + id, data = d, etype = "a")
  f <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt)
  want <- regtab(f, stats = "n")$body
  keep <- fg
  fg <- keep[keep$id > 10, ]
  expect_error(regtab(f), "model = TRUE")
  fg <- keep
  fg$x <- rev(fg$x)
  expect_error(regtab(f), "model = TRUE")
  rm(fg)
  # (With the data gone the finegray() mark is gone too: a note says so.)
  expect_error(suppressMessages(regtab(f)), "model = TRUE")
  fg <- keep[rev(seq_len(nrow(keep))), ]
  expect_identical(regtab(f, stats = "n")$body, want)
  # model = TRUE with the id keeps the rows (the finegray() mark is read
  # from the data as they are, so declare the fit once they are gone; its
  # subjects then come from the id).
  fg <- keep
  fm <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, model = TRUE,
                        cluster = id)
  rm(fg)
  expect_identical(regtab(fm, finegray = TRUE, stats = "n")$body, want)
})
