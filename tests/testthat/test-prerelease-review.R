# Regression tests for the v0.1.0 pre-release review (IMPLEMENTATION_PLAN.md,
# "Pre-release review outcome"). P0-1 has its own file,
# test-regtab-glm-ll.R.

# ---------------------------------------------------------------------------
# P1-1: tibble data (every haven::read_dta() result) keep factor labels

test_that("P1-1: read_dta() tibbles keep factor labels, with no 'data changed' warning", {
  skip_if_not_installed("haven")
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 300
  d <- data.frame(time = rexp(n), status = rbinom(n, 1, .7), x = rnorm(n),
                  g = factor(sample(c("a", "b", "c"), n, TRUE)), y = rbinom(n, 1, .4),
                  yo = sample(1:3, n, TRUE))
  attr(d$g, "label") <- "Group label"
  attr(d$x, "label") <- "X label"
  f <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(d, f)
  tb <- tt_as_factor(haven::read_dta(f), vars = "g")
  expect_s3_class(tb, "tbl_df")
  d$yo <- factor(d$yo)
  tb$yo <- factor(as.vector(tb$yo))
  fits <- list(
    lm_subset = function(D) lm(x ~ g, D, subset = y == 1),
    glm = function(D) glm(y ~ g + x, binomial, D),
    coxph = function(D) survival::coxph(survival::Surv(time, status) ~ g + x, D),
    survreg = function(D) survival::survreg(survival::Surv(time + .01, status) ~ g + x, D))
  if (requireNamespace("nnet", quietly = TRUE)) {
    fits$multinom <- function(D) nnet::multinom(yo ~ g + x, D, trace = FALSE)
  }
  for (nm in names(fits)) {
    want <- regtab(fits[[nm]](d))$body
    # multinom shows mlogit's raw equation keys (2: 2.g): only the warning mattered.
    if (nm != "multinom") expect_true("Group label" %in% want[, 1], label = paste(nm, "data.frame label"))
    expect_no_warning(got <- regtab(fits[[nm]](tb))$body, message = "could not be restored")
    expect_identical(got, want, label = paste(nm, "tibble body"))
  }
})

# ---------------------------------------------------------------------------
# P2-2: CI header without the level's floating-point noise (Stata tabtools
# 2.1.12 prints the same, golden R67; 2.1.11 printed "99.90000000000001% CI")

test_that("P2-2: 99.9% and 99.99% levels read cleanly in the header and the methods sentence", {
  f <- lm(mpg ~ wt, mtcars)
  hd <- function(l) regtab(f, level = l)$header[[2]]$text[3]
  expect_identical(hd(0.999), "99.9% CI")
  expect_identical(hd(99.9), "99.9% CI")
  expect_identical(hd(0.9999), "99.99% CI")
  expect_identical(hd(99.99), "99.99% CI")
  # Other levels as Stata prints them.
  lv <- c(0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 0.57, 0.58, 0.29, 97.5)
  expect_identical(vapply(lv, hd, ""), paste0(c("80", "90", "95", "97.5", "99", "99.5", "57", "58", "29", "97.5"), "% CI"))
  tt <- regtab(f, level = 0.999)
  expect_match(tt$stored$methods, "with 99.9% confidence intervals", fixed = TRUE)
  # The stored level is still the double Stata stores (r(ci_level)).
  expect_identical(tt$stored$ci_level, 99.9)
})

# ---------------------------------------------------------------------------
# P2-3: a data frame declared on a ratio scale drops its intercept, as a
# fitted ratio-scale model does

test_that("P2-3: effect_scale = 'OR' drops the intercept like stata_cmd and fitted models", {
  f <- glm(am ~ wt + hp, binomial, mtcars)
  b <- summary(f)$coefficients
  z <- stats::qnorm(0.975)
  d <- data.frame(term = rownames(b), estimate = exp(b[, 1]), conf.low = exp(b[, 1] - z * b[, 2]),
                  conf.high = exp(b[, 1] + z * b[, 2]), p.value = b[, 4], row.names = NULL)
  a <- d
  attr(a, "effect_scale") <- "OR"
  s <- d
  attr(s, "stata_cmd") <- "logit"
  want <- regtab(f)$body[, 1]
  expect_false("Intercept" %in% want)
  expect_identical(regtab(a)$body[, 1], regtab(s)$body[, 1])
  expect_false("Intercept" %in% regtab(a)$body[, 1])
  # keepintercept still keeps it; a difference scale keeps it by default.
  expect_true("Intercept" %in% regtab(a, keepintercept = TRUE)$body[, 1])
  r <- d
  r$estimate <- b[, 1]
  r$conf.low <- b[, 1] - z * b[, 2]
  r$conf.high <- b[, 1] + z * b[, 2]
  attr(r, "effect_scale") <- "Coef."
  expect_true("Intercept" %in% regtab(r)$body[, 1])
  # An unknown label declared ratio through null_value = 1 follows the rule too.
  u <- d
  attr(u, "effect_scale") <- "Odds (x)"
  attr(u, "null_value") <- 1L
  expect_false("Intercept" %in% regtab(u)$body[, 1])
})

# ---------------------------------------------------------------------------
# P3-5, P3-8: argument ranges named as Stata's

test_that("P3-5: a persistent fontsize out of range names the persistent range 6-72", {
  expect_error(tabtools_options(fontsize = 100), "between 6 and 72")
  expect_error(tabtools_options(fontsize = 3), "between 6 and 72")
  expect_error(tabtools_options(fontsize = 10.5), "between 6 and 72")
})

test_that("P3-8: level takes Stata's range, 10 to 99.99 percent", {
  f <- lm(mpg ~ wt, mtcars)
  for (l in c(1.5, 5, 0.05, 0.99999, 99.999, 100, 0, -1, 1)) {
    expect_error(regtab(f, level = l), "0.10 to 0.9999 or a percentage from 10 to 99.99", label = l)
  }
  expect_identical(regtab(f, level = 10)$header[[2]]$text[3], "10% CI")
  expect_identical(regtab(f, level = 0.1)$header[[2]]$text[3], "10% CI")
  expect_identical(regtab(f, level = 99.99)$header[[2]]$text[3], "99.99% CI")
})

# ---------------------------------------------------------------------------
# Documentation-agent findings folded into the release (2026-09-26)

# Counting-process data: `n` people with three records each, a weight that
# varies within person (`wv`) or is constant within person (`wc`), and no
# tied failure times (distinct stop times) unless `ties`.
pr_cp_data <- function(n = 100, ties = FALSE) {
  set.seed(11)
  d <- data.frame(id = rep(seq_len(n), each = 3), k = rep(1:3, n))
  d$start <- (d$k - 1) * 10
  d$stop <- d$start + 10
  last <- d$k == 3
  d$stop[last] <- d$start[last] + stats::runif(n, 1, 9)
  if (!ties) d$stop[!last] <- d$stop[!last] - stats::runif(sum(!last), 0, 1e-3)
  d$event <- as.integer(last & stats::runif(nrow(d)) < 0.6)
  if (ties) d$stop[last] <- round(d$stop[last])
  d$x <- stats::rnorm(nrow(d))
  d$wc <- rep(stats::runif(n, 0.5, 3), each = 3)
  d$wv <- stats::runif(nrow(d), 0.5, 3)
  d
}

test_that("weighted coxph: cluster() / cluster = name the subjects of the Subjects row", {
  skip_if_not_installed("survival")
  d <- pr_cp_data()
  S <- survival::Surv
  fits_v <- list(
    arg = survival::coxph(S(start, stop, event) ~ x, d, weights = wv, cluster = id, ties = "breslow"),
    term = survival::coxph(S(start, stop, event) ~ x + cluster(id), d, weights = wv, ties = "breslow"),
    id = survival::coxph(S(start, stop, event) ~ x, d, weights = wv, id = id, ties = "breslow"))
  for (nm in names(fits_v)) {
    msgs <- testthat::capture_messages(tt <- regtab(fits_v[[nm]], stats = c("n", "n_sub")))
    # The only model's Subjects cell is blank, so the row is left out.
    expect_false("Subjects" %in% trimws(tt$body[[1]]), label = paste(nm, "Subjects row"))
    s <- tabtools:::tt_model_stats(fits_v[[nm]], tabtools:::tt_model_info(fits_v[[nm]]))
    expect_true(is.na(s$N_sub), label = paste(nm, "N_sub"))
    expect_identical(s$nsub_note, "varies", label = paste(nm, "nsub_note"))
    expect_true(any(grepl("weights vary within subject", msgs)), label = paste(nm, "note"))
  }
  # Constant within subject: each subject's weight once, not every record's.
  want <- sum(d$wc[!duplicated(d$id)])
  for (f in list(survival::coxph(S(start, stop, event) ~ x, d, weights = wc, cluster = id, ties = "breslow"),
                 survival::coxph(S(start, stop, event) ~ x + cluster(id), d, weights = wc, ties = "breslow"),
                 survival::coxph(S(start, stop, event) ~ x, d, weights = wc, id = id, ties = "breslow"))) {
    s <- tabtools:::tt_model_stats(f, tabtools:::tt_model_info(f))
    expect_equal(s$N_sub, want)
    expect_identical(s$nsub_note, "weighted")
    tt <- suppressMessages(regtab(f, stats = c("n", "n_sub")))
    expect_identical(tt$body[[2]][trimws(tt$body[[1]]) == "Subjects"], format(round(want), big.mark = ","))
  }
  # One record per subject: the sum of the weights, as before.
  d1 <- d[d$k == 3, ]
  f1 <- survival::coxph(S(stop, event) ~ x, d1, weights = wv, cluster = id, ties = "breslow")
  expect_equal(tabtools:::tt_model_stats(f1, tabtools:::tt_model_info(f1))$N_sub, sum(d1$wv))
})

test_that("vce_note: one '<model>: <variance>' clause per model", {
  skip_if_not_installed("survival")
  d <- pr_cp_data()
  g <- glm(event ~ x, quasibinomial, d, weights = wc)
  expect_identical(regtab(g, g, vce = list("robust", "cluster"), cluster = list(NULL, ~id), vce_note = TRUE)$footnote,
                   "Standard errors, Model 1: robust; Model 2: robust, clustered by id.")
  expect_identical(suppressWarnings(regtab(g, g, vce = list("stata", "robust"), models = c("Crude", "IPTW, robust"),
                                           vce_note = TRUE))$footnote,
                   "Standard errors, IPTW, robust: robust.")
  expect_identical(regtab(g, vce = "robust", vce_note = TRUE, footnote = "Weighted")$footnote,
                   "Weighted \\ Standard errors: robust.")
})

test_that("weighted Efron coxph: the note and the blank statistics only on tied failure times", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  d <- pr_cp_data()
  d1 <- d[d$k == 3, ]
  expect_false(anyDuplicated(d1$stop[d1$event == 1]) > 0)
  fe <- survival::coxph(S(stop, event) ~ x, d1, weights = wv)
  fb <- survival::coxph(S(stop, event) ~ x, d1, weights = wv, ties = "breslow")
  expect_false(tabtools:::.rt_note_cox_ties(fe, 1))
  msgs <- testthat::capture_messages(te <- regtab(fe, stats = c("n", "ll")))
  expect_false(any(grepl("Efron", msgs)))
  expect_true(is.numeric(te$stored$ll_1) && is.finite(te$stored$ll_1))
  expect_equal(te$stored$ll_1, suppressMessages(regtab(fb, stats = c("n", "ll")))$stored$ll_1, tolerance = 1e-10)
  dt <- pr_cp_data(ties = TRUE)
  dt1 <- dt[dt$k == 3, ]
  expect_true(anyDuplicated(dt1$stop[dt1$event == 1]) > 0)
  ft <- survival::coxph(S(stop, event) ~ x, dt1, weights = wv)
  expect_true(suppressMessages(tabtools:::.rt_note_cox_ties(ft, 1)))
  msgs <- testthat::capture_messages(tt <- regtab(ft, stats = c("n", "ll")))
  expect_true(any(grepl("Efron ties have no Stata analogue", msgs)))
  expect_null(tt$stored$ll_1)
})
