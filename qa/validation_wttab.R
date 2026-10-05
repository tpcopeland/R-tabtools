# qa/validation_wttab.R - wttab() (task 7.12) against the packages that make
# the weights: real ipw::ipwpoint()/ipwtm() objects (ipw is not in Suggests,
# so the testthat suite uses a duck-typed list; this checks the real thing),
# and WeightIt's own summaries of the same weights.
#   Rscript qa/validation_wttab.R
# qa/run_all.R sets TABTOOLS_QA_LIB to a temporary library holding an
# installed copy; run by hand, the source tree is loaded. A comparator
# package that is not installed is skipped and reported: the script then
# ends INCOMPLETE (or SKIP when neither ran), never PASS (Codex audit CX-6;
# qa/tools/qa_result.R). TABTOOLS_QA_HIDE=ipw,WeightIt simulates their
# absence.
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
qa_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else normalizePath("qa")
source(file.path(qa_dir, "tools", "qa_result.R"))
qa_lib <- Sys.getenv("TABTOOLS_QA_LIB")
if (nzchar(qa_lib)) {
  suppressMessages(library(tabtools, lib.loc = qa_lib))
} else {
  suppressMessages(pkgload::load_all(dirname(qa_dir), quiet = TRUE))
}
check <- qa_check
ess <- function(w) sum(w)^2 / sum(w^2)

if (qa_has("ipw")) {
  # Point treatment: ipwpoint() keeps no data, so by = comes from `data`.
  set.seed(20260926)
  n <- 1000
  d <- data.frame(x = rnorm(n), v = rbinom(n, 1, 0.5))
  d$a <- rbinom(n, 1, plogis(-0.3 + 0.8 * d$x + 0.4 * d$v))
  p <- suppressWarnings(ipw::ipwpoint(exposure = a, family = "binomial", link = "logit",
                                      numerator = ~ 1, denominator = ~ x + v, data = d, trunc = 0.01))
  tt <- wttab(p, by = "a", data = d, trunc = c(0.01, 0.99))
  s <- tt$stored$stats
  check("ipwpoint: weights read from $ipw.weights", isTRUE(all.equal(s$mean[1], mean(p$ipw.weights))))
  check("ipwpoint: ESS by group", isTRUE(all.equal(s$ess[2], ess(p$ipw.weights[d$a == 1]))))
  check("ipwpoint: layout Overall/Treated/Untreated",
        identical(tt$header[[1]]$text[2:4], c("Untruncated: Overall", "Untruncated: Treated", "Untruncated: Untreated")))
  # ipw's own truncation takes R's default quantile (type 7); wttab's cut-offs
  # are Stata's _pctile, so the truncated means may differ in the last digits.
  cut7 <- unname(quantile(p$ipw.weights, c(0.01, 0.99)))
  cat(sprintf("     ipw trunc = 0.01 cut-offs (type 7): %.6f, %.6f; wttab (_pctile): %.6f, %.6f\n",
              cut7[1], cut7[2], tt$stored$trunc$lower, tt$stored$trunc$upper))
  check("ipwpoint: wttab cut-offs are quantile(type = 2) here",
        isTRUE(all.equal(c(tt$stored$trunc$lower, tt$stored$trunc$upper),
                         unname(quantile(p$ipw.weights, c(0.01, 0.99), type = 2)))))

  # Time-varying weights (Cole & Hernan 2008 layout): haartdat, one row per
  # patient-interval; period = the follow-up time.
  utils::data("haartdat", package = "ipw", envir = environment())
  tm <- suppressWarnings(ipw::ipwtm(exposure = haartind, family = "survival", numerator = ~ sex + age,
                                    denominator = ~ cd4.sqrt + sex + age, id = patient, tstart = tstart,
                                    timevar = fuptime, type = "first", data = haartdat))
  haartdat$year <- floor(haartdat$tstart / 365.25)
  tv <- wttab(tm, period = "year", by = "haartind", data = haartdat, digits = 3)
  st <- tv$stored$stats
  check("ipwtm: one row per year x group", nrow(st) == 3L * length(unique(haartdat$year)))
  # (haartdat's tstart starts at -100 days, so the first period is -1.)
  y0 <- haartdat$year == 0
  r0 <- which(st$period == "0" & st$group == "Overall")
  check("ipwtm: year 0 mean", isTRUE(all.equal(st$mean[r0], mean(tm$ipw.weights[y0]))))
  xs0 <- sort(tm$ipw.weights[y0])
  # Stata's rule by hand (P = n p / 100; average when whole), not quantile().
  P <- length(xs0) * 99 / 100
  p99 <- if (P == floor(P)) (xs0[P] + xs0[P + 1]) / 2 else xs0[ceiling(P)]
  check("ipwtm: year 0 P99 (Stata's _pctile rule)", identical(st$p99[r0], p99))
  # ipwtm() returns the weights in the data's row order (wttab() relies on
  # it to match by/period from `data`): refit on shuffled rows.
  sh <- haartdat[sample(nrow(haartdat)), ]
  tm2 <- suppressWarnings(ipw::ipwtm(exposure = haartind, family = "survival", numerator = ~ sex + age,
                                     denominator = ~ cd4.sqrt + sex + age, id = patient, tstart = tstart,
                                     timevar = fuptime, type = "first", data = sh))
  key <- paste(haartdat$patient, haartdat$tstart)
  check("ipwtm: weights follow the data's row order",
        isTRUE(all.equal(tm2$ipw.weights, tm$ipw.weights[match(paste(sh$patient, sh$tstart), key)])))
  check("ipwtm: N adds up", sum(st$n[st$group == "Overall"]) == nrow(haartdat))
  print(tv)
} else {
  qa_skip("ipw comparisons (ipwpoint, ipwtm)", "ipw not installed")
}

if (qa_has("WeightIt")) {
  set.seed(1)
  n <- 800
  d <- data.frame(x1 = rnorm(n), x2 = rbinom(n, 1, 0.4))
  d$t <- rbinom(n, 1, plogis(0.6 * d$x1 - 0.4 * d$x2))
  d$g <- factor(sample(c("A", "B", "C"), n, TRUE))
  for (est in c("ATE", "ATT", "ATO")) {
    W <- WeightIt::weightit(t ~ x1 + x2, data = d, estimand = est)
    tt <- wttab(W)
    sm <- summary(W)
    e <- sm$effective.sample.size
    check(sprintf("weightit %s: ESS = summary()'s", est),
          isTRUE(all.equal(tt$stored$stats$ess[2:3], unlist(e["Weighted", c("Treated", "Control")], use.names = FALSE))))
    check(sprintf("weightit %s: max = summary()'s weight range", est),
          isTRUE(all.equal(tt$stored$stats$max[2:3],
                           c(max(W$weights[d$t == 1]), max(W$weights[d$t == 0])))))
  }
  W3 <- WeightIt::weightit(g ~ x1 + x2, data = d, estimand = "ATE")
  t3 <- wttab(W3)
  e3 <- summary(W3)$effective.sample.size
  check("weightit multi-category: ESS per level",
        isTRUE(all.equal(t3$stored$stats$ess[2:4], unlist(e3["Weighted", c("A", "B", "C")], use.names = FALSE))))
} else {
  qa_skip("WeightIt comparisons (ESS and weight range per estimand)", "WeightIt not installed")
}

qa_done("validation_wttab.R")
