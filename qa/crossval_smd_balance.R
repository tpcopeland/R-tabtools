library(testthat)
library(tabtools)

# table1_tc(smd = TRUE) balance statistics against independent
# implementations, on the installed package:
#   smdtype = "population"  McCaffrey et al. (2013) eq. 5, max over groups:
#                           cobalt 4.6.3 (pairwise = FALSE, s.d.denom = "all")
#                           and twang 2.6.2 (mnps ATE, psList$<arm>$desc)
#   smdtype = "maxpair"     Lopez and Gutman (2017) eq. 27 on the all-groups
#                           pooled SD: cobalt (pairwise = TRUE,
#                           s.d.denom = "pooled"); categorical against a
#                           base-R Yang-Dalton oracle
#   smdtype = "pair"        Yang and Dalton two-group SMD: cobalt on the two
#                           compared arms (s.d.denom = "pooled")
# cobalt's Balance.Across.Pairs Min.Diff/Max.Diff are *signed* extremes, so
# the largest absolute value is max(|Min.Diff|, |Max.Diff|). cobalt keeps the
# unweighted SD under balancing weights, so weighted maxpair (weighted group
# variances, as the weighted pair SMD) has no cobalt oracle; it is checked
# through the two-group identity maxpair = |pair| instead.

# Shared builders ----

# `hot`: the level of reg over-represented in arm C, so the largest
# categorical imbalance sits on a chosen level (the first or the last level
# is where a dropped-level bug would hide it).
xv_data <- function(seed, n = 420, hot = "e") {
  set.seed(seed)
  arm <- sample(c("A", "B", "C"), n, TRUE, prob = c(0.45, 0.35, 0.20))
  lv <- c("e", "n", "s", "w")
  prob_c <- ifelse(lv == hot, 0.55, 0.15)
  reg <- vapply(arm, function(a) sample(lv, 1L, prob = if (a == "C") prob_c else rep(0.25, 4)), "")
  data.frame(
    arm = arm,
    age = rnorm(n, 50 + 6 * (arm == "C") - 3 * (arm == "B"), 10),
    crp = exp(rnorm(n, 1 + 0.4 * (arm == "C"), 0.8)),
    sex = rbinom(n, 1, ifelse(arm == "B", 0.65, 0.40)),
    reg = unname(reg),
    w = runif(n, 0.3, 4),
    f = sample(1:4, n, TRUE),
    stringsAsFactors = FALSE
  )
}

xv_vars <- c(age = "contn", crp = "contln", sex = "bin", reg = "cat")

xv_smd <- function(d, ..., vars = xv_vars) {
  tt <- table1_tc(d, by = "arm", vars = vars, smd = TRUE, nopvalue = TRUE, ...)
  out <- tt$stored$table[, "smd"]
  names(out) <- rownames(tt$stored$table)
  out
}

# cobalt's covariates: log(crp) is the contln analysis scale; reg is split
# into one row per level (reg_e, reg_n, ...).
xv_cobalt <- function(d, pairwise, s.d.denom, weights = NULL) {
  covs <- data.frame(age = d$age, lcrp = log(d$crp), sex = d$sex, reg = factor(d$reg))
  args <- list(covs, treat = d$arm, pairwise = pairwise, s.d.denom = s.d.denom,
               binary = "std", continuous = "std", quick = FALSE)
  if (!is.null(weights)) args <- c(args, list(weights = weights, method = "weighting"))
  bt <- do.call(cobalt::bal.tab, args)
  col <- if (is.null(weights)) "Un" else "Adj"
  # Two arms: a binary-treatment table with one signed Diff per covariate.
  if (length(unique(d$arm)) == 2L) {
    b <- bt$Balance
    absmax <- abs(b[[paste0("Diff.", col)]])
    names(absmax) <- rownames(b)
    return(absmax)
  }
  b <- bt$Balance.Across.Pairs
  absmax <- pmax(abs(b[[paste0("Min.Diff.", col)]]), abs(b[[paste0("Max.Diff.", col)]]))
  names(absmax) <- rownames(b)
  absmax
}

xv_cobalt_vars <- function(cb, reg_rows = TRUE) {
  c(age = cb[["age"]], crp = cb[["lcrp"]], sex = cb[["sex"]],
    reg = if (reg_rows) max(cb[grepl("^reg_", names(cb))]) else NA_real_)
}

# Yang-Dalton Mahalanobis distance, written from the paper's eq. (2) with
# level 1 dropped (tabtools drops the last level; the distance is invariant).
xv_yd <- function(p1, p2, s) {
  d <- p1 - p2
  sqrt(drop(t(d) %*% solve(s, d)))
}

# smdtype = "population" ----

test_that("population SB matches cobalt's group-vs-all SMD, unweighted (eq. 5)", {
  skip_if_not_installed("cobalt", "4.6.3")
  for (hot in c("e", "w")) {
    d <- xv_data(20261006, hot = hot)
    want <- xv_cobalt_vars(xv_cobalt(d, pairwise = FALSE, s.d.denom = "all"))
    expect_equal(xv_smd(d, smdtype = "population"), want, tolerance = 1e-10,
                 label = paste("imbalance on level", hot))
  }
})

test_that("population SB under wt matches cobalt: weighted group means, unweighted reference", {
  skip_if_not_installed("cobalt", "4.6.3")
  d <- xv_data(20261007)
  want <- xv_cobalt_vars(xv_cobalt(d, pairwise = FALSE, s.d.denom = "all", weights = d$w))
  got <- xv_smd(d, smdtype = "population", wt = "w")
  expect_equal(got, want, tolerance = 1e-10)
  # The weights matter: the weighted statistic is not the unweighted one.
  expect_false(isTRUE(all.equal(got, xv_smd(d, smdtype = "population"), tolerance = 1e-6)))
})

test_that("population SB matches twang's mnps eq. 5 values, unweighted and with twang's own weights", {
  skip_if_not_installed("twang", "2.6.2")
  d <- xv_data(20261008, n = 360)
  # twang applies eq. 5 per factor level with sqrt(p (1 - p)), so the binary
  # covariate enters as a factor (as a 0/1 number it would take twang's
  # continuous n / (n - 1) SD).
  td <- data.frame(arm = factor(d$arm), age = d$age, sex = factor(d$sex), reg = factor(d$reg))
  set.seed(1)
  m <- suppressWarnings(twang::mnps(arm ~ age + sex + reg, data = td, estimand = "ATE",
                                    n.trees = 300, stop.method = "es.max", verbose = FALSE))
  twang_psb <- function(lab) {
    es <- lapply(c("A", "B", "C"), function(a) m$psList[[a]]$desc[[lab]]$bal.tab$results)
    pick <- function(rx) max(vapply(es, function(r) max(abs(r[grepl(rx, rownames(r)), "std.eff.sz"])), 0))
    c(age = pick("^age$"), sex = pick("^sex:"), reg = pick("^reg:"))
  }
  vars <- c(age = "contn", sex = "bin", reg = "cat")
  d$w <- twang::get.weights(m, stop.method = "es.max")
  expect_equal(xv_smd(d, smdtype = "population", vars = vars), twang_psb("unw"), tolerance = 1e-10)
  expect_equal(xv_smd(d, smdtype = "population", wt = "w", vars = vars), twang_psb("es.max.ATE"),
               tolerance = 1e-10)
})

# smdtype = "maxpair" ----

test_that("maxpair matches cobalt's pairwise SMD on the all-groups pooled SD (eq. 27)", {
  skip_if_not_installed("cobalt", "4.6.3")
  d <- xv_data(20261009)
  cb <- xv_cobalt(d, pairwise = TRUE, s.d.denom = "pooled")
  # cobalt standardizes each factor level separately; tabtools' categorical
  # maxpair is the multivariate Yang-Dalton distance (next test).
  want <- xv_cobalt_vars(cb, reg_rows = FALSE)[c("age", "crp", "sex")]
  expect_equal(xv_smd(d, smdtype = "maxpair")[c("age", "crp", "sex")], want, tolerance = 1e-10)
})

test_that("categorical maxpair is the largest Yang-Dalton distance with S averaged over all groups", {
  d <- xv_data(20261010)
  arms <- c("A", "B", "C")
  lv <- sort(unique(d$reg))
  # Level shares per arm, dropping the first level.
  p <- vapply(arms, function(a) as.numeric(table(factor(d$reg[d$arm == a], lv)))[-1] /
                sum(d$arm == a), numeric(length(lv) - 1L))
  s <- Reduce(`+`, lapply(arms, function(a) diag(p[, a]) - tcrossprod(p[, a]))) / length(arms)
  pairs <- utils::combn(arms, 2L)
  want <- max(apply(pairs, 2L, function(ab) xv_yd(p[, ab[1]], p[, ab[2]], s)))
  got <- xv_smd(d, smdtype = "maxpair", vars = c(reg = "cat"))
  expect_equal(unname(got), want, tolerance = 1e-10)
})

# smdtype = "pair" and smdpair ----

test_that("the pair SMD and an smdpair choice match cobalt on the two compared arms", {
  skip_if_not_installed("cobalt", "4.6.3")
  d <- xv_data(20261011)
  two <- function(a, b) {
    s <- d[d$arm %in% c(a, b), ]
    cb <- xv_cobalt(s, pairwise = TRUE, s.d.denom = "pooled")
    xv_cobalt_vars(cb, reg_rows = FALSE)[c("age", "crp", "sex")]
  }
  keep <- c("age", "crp", "sex")
  expect_equal(xv_smd(d)[keep], two("A", "B"), tolerance = 1e-10)
  expect_equal(xv_smd(d, smdpair = c("C", "A"))[keep], two("A", "C"), tolerance = 1e-10)
})

test_that("with two groups maxpair equals |pair| unweighted, under wt and under fweight", {
  d <- xv_data(20261012)
  d <- d[d$arm != "C", ]
  for (w in list(list(), list(wt = "w"), list(fweight = "f"))) {
    pair <- do.call(xv_smd, c(list(d), w))
    maxp <- do.call(xv_smd, c(list(d, smdtype = "maxpair"), w))
    expect_equal(maxp, abs(pair), tolerance = 1e-12, label = paste("maxpair", names(w)))
  }
})

# Weights and layouts ----

test_that("fweight gives the statistic of the record-expanded data, for every smdtype", {
  d <- xv_data(20261013)
  e <- d[rep(seq_len(nrow(d)), d$f), ]
  for (st in c("pair", "population", "maxpair")) {
    expect_equal(xv_smd(d, smdtype = st, fweight = "f"), xv_smd(e, smdtype = st),
                 tolerance = 1e-10, label = st)
  }
})

test_that("wtcompare shows the weighted pass's population SB under the Pop. SB header", {
  d <- xv_data(20261014)
  tt <- table1_tc(d, by = "arm", vars = xv_vars, smd = TRUE, wt = "w", wtcompare = TRUE,
                  smdtype = "population")
  smd_col <- which(tt$cols$role == "smd")
  expect_identical(tt$header[[1]]$text[smd_col], "Pop. SB")
  expect_identical(tt$stored$smdtype, "population")
  expect_match(tt$footnote, "Pop. SB: largest absolute difference", fixed = TRUE)
  expect_equal(tt$stored$table[, "smd"], xv_smd(d, smdtype = "population", wt = "w"),
               tolerance = 1e-12, ignore_attr = TRUE)
})
