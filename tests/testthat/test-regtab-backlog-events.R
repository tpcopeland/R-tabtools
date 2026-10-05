# Backlog: a weighted coxph(y = FALSE) on data re-sorted with their row
# names reset dropped the Events row (stats = "events"), and the siblings
# found with it: the same fit's pseudo-log-likelihood (blank with a note),
# survreg's counts and log-likelihood read from model.frame(<survreg>)
# (which leaves out `cluster =`: rows with a missing cluster were counted),
# a re-sorted survreg(cluster =, y = FALSE) refused, and a weighted tt()
# coxph whose Subjects and pseudo-log-likelihood used the weights of its
# tt-expanded records. Events must equal Stata's e(N_fail) from the default
# fit on the original data: the failures weighted by the case weights.

be_data <- function() {
  set.seed(11)
  n <- 240
  d <- data.frame(x = rnorm(n), w = runif(n, 0.5, 2), wi = sample(1:3, n, TRUE),
                  inst = sample(c(1:15, NA), n, TRUE), g = factor(sample(1:2, n, TRUE)))
  d$time <- ceiling(rexp(n, exp(0.3 * d$x)) * 20)
  d$status <- rbinom(n, 1, 0.7)
  d
}

be_resort <- function(d) {
  d <- d[order(d$x, decreasing = TRUE), ]
  rownames(d) <- NULL
  d
}

be_S <- c("n", "events", "ll")

be_tab <- function(fit, vce = "stata") suppressWarnings(suppressMessages(regtab(fit, stats = be_S, vce = vce)))

test_that("backlog: Events of a coxph is e(N_fail), weighted or not, y = TRUE or FALSE, original or re-sorted", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  fits <- alist(
    w_breslow = survival::coxph(S(time, status) ~ x, d, weights = w, ties = "breslow", y = yy),
    w_efron = survival::coxph(S(time, status) ~ x, d, weights = w, y = yy),
    w_robust = survival::coxph(S(time, status) ~ x, d, weights = w, ties = "breslow", robust = TRUE, y = yy),
    w_integer = survival::coxph(S(time, status) ~ x, d, weights = wi, ties = "breslow", y = yy),
    w_strata = survival::coxph(S(time, status) ~ x + strata(g), d, weights = w, ties = "breslow", y = yy),
    unweighted = survival::coxph(S(time, status) ~ x, d, ties = "breslow", y = yy)
  )
  for (nm in names(fits)) {
    d <- be_data()
    wv <- switch(nm, w_integer = d$wi, unweighted = rep(1, nrow(d)), d$w)
    truth <- sum(wv[d$status == 1])
    yy <- TRUE
    want <- be_tab(eval(fits[[nm]]))
    expect_equal(want$stored$events_1, truth, tolerance = 1e-12, label = nm)
    for (yy in c(TRUE, FALSE)) {
      d <- be_data()
      fit <- eval(fits[[nm]])
      got <- be_tab(fit)
      expect_identical(got$body, want$body, label = paste(nm, "y =", yy))
      expect_identical(got$stored$events_1, want$stored$events_1, label = paste(nm, "y =", yy))
      d <- be_resort(d)
      got <- be_tab(fit)
      lab <- paste(nm, "y =", yy, "re-sorted")
      expect_identical(got$body, want$body, label = lab)
      expect_equal(got$stored$events_1, truth, tolerance = 1e-12, label = lab)
      expect_equal(got$stored$ll_1, want$stored$ll_1, tolerance = 1e-12, label = lab)
    }
  }
})

test_that("backlog: the reproducer (weighted coxph(y = FALSE), re-sorted) keeps its Events row and notes", {
  skip_if_not_installed("survival")
  d <- be_data()
  fit <- survival::coxph(survival::Surv(time, status) ~ x, d, weights = w, ties = "breslow", y = FALSE)
  d <- be_resort(d)
  msgs <- testthat::capture_messages(tt <- suppressWarnings(regtab(fit, stats = c("events", "ll"))))
  expect_identical(tt$body[[1]], c("x", "Events", "Log-likelihood"))
  expect_true(any(grepl("Events for model(s) 1 is the sum of the failures' weights", msgs, fixed = TRUE)))
  expect_false(any(grepl("not shown", msgs, fixed = TRUE)))
  # The pseudo-log-likelihood under the robust (stset [pw]) reading too.
  expect_equal(be_tab(fit, "robust")$stored$ll_1,
               tabtools:::.rt_cox_pseudo_ll(survival::coxph(survival::Surv(time, status) ~ x, be_data(),
                                                            weights = w, ties = "breslow")),
               tolerance = 1e-12)
})

test_that("backlog: re-sorted data are matched on the martingale residuals too", {
  skip_if_not_installed("survival")
  d <- be_data()
  fit <- survival::coxph(survival::Surv(time, status) ~ x, d, weights = w, y = FALSE)
  d <- be_resort(d)
  expect_true(tabtools:::.rt_reordered_ok(fit))
  bad <- fit
  bad$residuals <- rev(bad$residuals)
  expect_false(tabtools:::.rt_reordered_ok(bad))
  # An edited status is still refused.
  d$status[d$status == 0][1:5] <- 1L
  expect_error(regtab(fit, stats = "events"), "have changed since")
})

test_that("backlog: survreg (streg) N, Events and log-likelihood on re-sorted data, cluster = with missing clusters", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  fits <- alist(
    fw = survival::survreg(S(time, status) ~ x, d, weights = wi, y = yy),
    pw = survival::survreg(S(time, status) ~ x, d, weights = w, robust = TRUE, y = yy),
    cl = survival::survreg(S(time, status) ~ x, d, weights = w, cluster = inst, y = yy),
    unweighted = survival::survreg(S(time, status) ~ x, d, y = yy)
  )
  for (nm in names(fits)) {
    d <- be_data()
    keep <- if (nm == "cl") !is.na(d$inst) else rep(TRUE, nrow(d))
    wv <- switch(nm, fw = d$wi, unweighted = rep(1, nrow(d)), d$w)
    truth <- sum(wv[keep & d$status == 1])
    yy <- TRUE
    want <- be_tab(eval(fits[[nm]]))
    expect_equal(want$stored$events_1, truth, tolerance = 1e-12, label = nm)
    for (yy in c(TRUE, FALSE)) {
      d <- be_data()
      fit <- eval(fits[[nm]])
      expect_identical(be_tab(fit)$body, want$body, label = paste(nm, "y =", yy))
      d <- be_resort(d)
      got <- be_tab(fit)
      lab <- paste(nm, "y =", yy, "re-sorted")
      # cl, y = TRUE: model.frame(<survreg>) counted the rows whose cluster
      # is missing (N, Events and LL all moved); y = FALSE was refused.
      expect_identical(got$body, want$body, label = lab)
      expect_equal(got$stored$events_1, truth, tolerance = 1e-12, label = lab)
      expect_equal(got$stored$n_1, want$stored$n_1, tolerance = 1e-12, label = lab)
      expect_equal(got$stored$ll_1, want$stored$ll_1, tolerance = 1e-10, label = lab)
    }
  }
})

test_that("backlog: survreg statistics are never read from data that no longer hold the fit", {
  skip_if_not_installed("survival")
  d <- be_data()
  fit <- survival::survreg(survival::Surv(time, status) ~ x, d, weights = wi, y = FALSE)
  info <- tabtools:::tt_model_info(fit)
  rm(d)
  expect_error(tabtools:::tt_model_stats(fit, info), "needs the data it was fitted on")
})

test_that("backlog: Fine-Gray Events (unweighted failures of interest), y = TRUE or FALSE, re-sorted", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  for (yy in c(TRUE, FALSE)) {
    d <- be_data()
    d$etype <- factor(ifelse(d$status == 1, 1 + (d$wi == 1), 0), 0:2, c("censor", "a", "b"))
    fg <- survival::finegray(S(time, etype) ~ ., data = d, etype = "a")
    truth <- sum(d$etype == "a")
    fit <- survival::coxph(S(fgstart, fgstop, fgstatus) ~ x, data = fg, weights = fgwt, y = yy)
    want <- be_tab(fit)
    expect_identical(want$stored$events_1, as.numeric(truth), label = paste("y =", yy))
    fg <- fg[order(fg$x), ]
    rownames(fg) <- NULL
    # Stata's variance needs the subjects: refused on re-sorted data; the
    # stored variance keeps Events.
    expect_error(be_tab(fit), "needs the data it was fitted on")
    expect_identical(be_tab(fit, "model")$stored$events_1, as.numeric(truth), label = paste("y =", yy))
  }
})

test_that("backlog: a weighted tt() coxph counts its observations, not its expanded records", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  ttf <- function(x, t, ...) x * log(t)
  d0 <- be_data()
  cc <- nrow(d0) / sum(d0$w)
  for (yy in c(TRUE, FALSE)) {
    d <- be_data()
    fit <- survival::coxph(S(time, status) ~ x + tt(x), d, weights = w, ties = "breslow", y = yy, tt = ttf)
    # Stata: e(N_sub) and e(N_fail) are weight sums over the observations,
    # and the pseudo-log-likelihood normalises their weights to mean 1.
    truth <- list(n = sum(d0$w), events = sum(d0$w[d0$status == 1]),
                  ll = cc * fit$loglik[2] - cc * log(cc) * sum(d0$w[d0$status == 1]))
    expect_gt(length(fit$weights), nrow(d0))
    for (sorted in c(FALSE, TRUE)) {
      if (sorted) d <- be_resort(d)
      msgs <- testthat::capture_messages(st <- suppressWarnings(regtab(fit, stats = be_S, vce = "robust"))$stored)
      lab <- paste("y =", yy, if (sorted) "re-sorted")
      if (yy) {
        expect_equal(st$n_1, truth$n, tolerance = 1e-12, label = lab)
        expect_equal(st$events_1, truth$events, tolerance = 1e-12, label = lab)
        expect_equal(st$ll_1, truth$ll, tolerance = 1e-10, label = lab)
      } else {
        # y = FALSE keeps no statuses of the expanded records: the data's
        # weights cannot be tied to the observations (blank, with notes
        # that suggest y = TRUE).
        expect_null(st$n_1)
        expect_true(any(grepl("refit with y = TRUE", msgs, fixed = TRUE)), label = lab)
        expect_null(st$events_1)
        expect_null(st$ll_1)
        expect_true(any(grepl("Events not shown for model(s) 1", msgs, fixed = TRUE)), label = lab)
        expect_true(any(grepl("a fit with y = FALSE keeps none of their statuses", msgs, fixed = TRUE)), label = lab)
      }
    }
    # Weights edited in place (times, statuses and covariates unchanged,
    # which is all a tt() fit's data check sees): blank cells with notes,
    # never the expanded records' sums.
    d <- be_data()
    d$w <- d$w * 2
    msgs <- testthat::capture_messages(tt <- suppressWarnings(regtab(fit, stats = be_S, vce = "robust")))
    expect_null(tt$stored$n_1)
    if (yy) {
      expect_equal(tt$stored$events_1, truth$events, tolerance = 1e-12)
    } else {
      expect_null(tt$stored$events_1)
      expect_true(any(grepl("Events not shown for model(s) 1", msgs, fixed = TRUE)))
    }
    expect_null(tt$stored$ll_1)
    expect_true(any(grepl("Subjects not shown for model(s) 1", msgs, fixed = TRUE)))
    expect_true(any(grepl("log-likelihood, AIC and BIC not shown for model(s) 1: the fit's tt() terms", msgs, fixed = TRUE)))
    expect_false(any(c("Subjects", "Observations") %in% tt$body[[1]]))
  }
})

test_that("backlog review: weights swapped between a failure and a censored row at one time move nothing", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  ttf <- function(x, t, ...) x * log(t)
  for (yy in c(TRUE, FALSE)) {
    d <- be_data()
    truth <- sum(d$w[d$status == 1])
    fit <- survival::coxph(S(time, status) ~ x + tt(x), d, weights = w, ties = "breslow", y = yy, tt = ttf)
    want <- be_tab(fit, "robust")$stored
    # Same time, so the same number of expanded records: the weights
    # repeated by their expansion are the same multiset after the swap.
    t0 <- intersect(d$time[d$status == 1], d$time[d$status == 0])[1]
    i <- which(d$time == t0 & d$status == 1)[1]
    j <- which(d$time == t0 & d$status == 0)[1]
    expect_false(isTRUE(all.equal(d$w[i], d$w[j])))
    d[c(i, j), "w"] <- d[c(j, i), "w"]
    msgs <- testthat::capture_messages(st <- suppressWarnings(regtab(fit, stats = be_S, vce = "robust"))$stored)
    if (yy) {
      # Events from the fit's own records; the data no longer pair a
      # failure with its weight, so Subjects and the log-likelihood are
      # blank (the edit gave Events 204.33 and LL -726.14).
      expect_equal(st$events_1, truth, tolerance = 1e-12)
      expect_equal(want$ll_1, fit$loglik[2] * nrow(d) / sum(d$w) -
                     nrow(d) / sum(d$w) * log(nrow(d) / sum(d$w)) * truth, tolerance = 1e-10)
      expect_null(st$n_1)
      expect_null(st$ll_1)
      expect_true(any(grepl("Subjects not shown for model(s) 1", msgs, fixed = TRUE)))
    } else {
      # Nothing ties the data's weights to the observations: blank.
      expect_null(want$n_1)
      expect_null(st$n_1)
      expect_null(st$events_1)
      expect_null(st$ll_1)
    }
  }
})

test_that("backlog review: weights re-dealt among observations expanded 2, 1, 1 times are caught", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  ttf <- function(x, t, ...) x * log(t)
  mk <- function() {
    d <- be_data()
    ut <- sort(unique(d$time[d$status == 1]))
    # a censored at the second event time (at risk at two), b and c at the
    # first (at risk at one each): weights 1, 2, 2 expand to {1, 1, 2, 2}.
    abc <- data.frame(x = c(0.1, -0.2, 0.3), w = c(1, 2, 2), wi = 1L, inst = 1L, g = factor(1, levels = 1:2),
                      time = c(ut[2], ut[1], ut[1]), status = 0L)
    rbind(d, abc)
  }
  for (yy in c(TRUE, FALSE)) {
    d <- mk()
    fit <- survival::coxph(S(time, status) ~ x + tt(x), d, weights = w, ties = "breslow", y = yy, tt = ttf)
    want <- be_tab(fit, "robust")$stored
    if (yy) expect_equal(want$n_1, sum(d$w), tolerance = 1e-12) else expect_null(want$n_1)
    # 2, 1, 1: the same multiset of expanded weights, a sum of 4 for 5.
    n0 <- nrow(d)
    d$w[n0 - 2:0] <- c(2, 1, 1)
    msgs <- testthat::capture_messages(st <- suppressWarnings(regtab(fit, stats = be_S, vce = "robust"))$stored)
    expect_null(st$n_1)
    expect_true(any(grepl("Subjects not shown for model(s) 1", msgs, fixed = TRUE)), label = paste("y =", yy))
  }
})

test_that("backlog review: a tt() coxph(y = FALSE) whose weights are all equal keeps its Subjects", {
  skip_if_not_installed("survival")
  d <- be_data()
  d$w2 <- 2
  fit <- survival::coxph(survival::Surv(time, status) ~ x + tt(x), d, weights = w2, ties = "breslow", y = FALSE,
                         tt = function(x, t, ...) x * log(t))
  expect_equal(be_tab(fit)$stored$n_1, 2 * nrow(d), tolerance = 1e-12)
})

test_that("backlog review: tt() with strata(), counting-process data, cluster() and near-tied times", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  # probeG of the review: the truths are Stata's e(N_sub) (each subject's
  # weight once), e(N_fail) and the pseudo-log-likelihood normalised over
  # the observations.
  set.seed(9)
  n <- 200
  ttf <- function(x, t, ...) x * log(t + 1)
  d <- data.frame(id = rep(1:100, each = 2), x = rnorm(n), g = factor(sample(1:3, n, TRUE)))
  d$w <- runif(100, .5, 2)[d$id]
  d$time <- sample(1:15, n, TRUE)
  d$status <- rbinom(n, 1, .6)
  d$start <- 0
  d$start[c(FALSE, TRUE)] <- d$time[c(TRUE, FALSE)]
  d$time[c(FALSE, TRUE)] <- d$start[c(FALSE, TRUE)] + sample(1:10, 100, TRUE)
  d$status[c(TRUE, FALSE)] <- 0
  dr <- d
  dr$start <- NULL
  dt <- dr
  dt$time <- dt$time + ifelse(seq_len(n) %% 3 == 0, 1e-12, 0)
  truth <- function(fit, dd, idv = NULL) {
    w <- dd$w
    ev <- dd$status == 1
    cc <- length(w) / sum(w)
    c(n = if (is.null(idv)) sum(w) else sum(w[!duplicated(idv)]), events = sum(w[ev]),
      ll = cc * fit$loglik[2] - cc * log(cc) * sum(w[ev]))
  }
  got <- function(fit) {
    st <- be_tab(fit, "robust")$stored
    c(n = st$n_1 %||% NA, events = st$events_1 %||% NA, ll = st$ll_1 %||% NA)
  }
  f1 <- survival::coxph(S(time, status) ~ x + tt(x) + strata(g), dr, weights = w, ties = "breslow", tt = ttf)
  expect_equal(got(f1), truth(f1, dr), tolerance = 1e-10)
  f2 <- survival::coxph(S(start, time, status) ~ x + tt(x), d, weights = w, id = id, ties = "breslow", tt = ttf)
  expect_equal(got(f2), truth(f2, d, d$id), tolerance = 1e-10)
  f3 <- survival::coxph(S(start, time, status) ~ x + tt(x) + strata(g), d, weights = w, cluster = id,
                        ties = "breslow", tt = ttf)
  expect_equal(got(f3), truth(f3, d, d$id), tolerance = 1e-10)
  f5 <- survival::coxph(S(time, status) ~ x + tt(x) + cluster(id), dr, weights = w, ties = "breslow", tt = ttf)
  expect_equal(got(f5), truth(f5, dr, dr$id), tolerance = 1e-10)
  # Times 1e-12 apart, which survival's timefix merges: counted as it does.
  f6 <- survival::coxph(S(time, status) ~ x + tt(x), dt, weights = w, ties = "breslow", tt = ttf)
  expect_equal(got(f6), truth(f6, dt), tolerance = 1e-10)
  # The strata and the start times matter: counted without them, the
  # expansion does not match the fit's records.
  expect_false(is.null(tabtools:::.rt_tt_weights(f1)))
  expect_false(is.null(tabtools:::.rt_tt_weights(f2)))
})

test_that("backlog review: a failure alone at risk at its time is checked by the number of failures", {
  skip_if_not_installed("survival")
  S <- survival::Surv
  for (sorted in c(FALSE, TRUE)) {
    for (yy in c(TRUE, FALSE)) {
      set.seed(4)
      n <- 60
      d <- data.frame(x = rnorm(n), w = runif(n, .5, 2), time = sample(1:20, n, TRUE), status = rbinom(n, 1, .6))
      d$time[1] <- 99
      d$status[1] <- 1
      d$w[1] <- 1
      fit <- survival::coxph(S(time, status) ~ x, d, weights = w, ties = "breslow", y = yy)
      # Its martingale residual is 0 either way and, with weight 1, its
      # log-likelihood term too: only the count tells the flip.
      d$status[1] <- 0
      if (sorted) d <- be_resort(d)
      lab <- paste(if (sorted) "re-sorted" else "in place", "y =", yy)
      if (sorted) expect_false(tabtools:::.rt_reordered_ok(fit), label = lab)
      expect_error(regtab(fit, stats = c("events", "ll")), "have changed since", label = lab)
    }
  }
})
