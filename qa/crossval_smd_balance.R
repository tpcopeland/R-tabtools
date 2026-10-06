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
  withr::local_preserve_seed()
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
  withr::local_preserve_seed()
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

# Stata 2.5.1 parity matrix ----
# Same-author oracle: catches cross-language divergence, but cannot detect
# a shared method error. cobalt/twang above remain the independent checks.
# Source contract: table1_tc.sthlp:325-359; desctab.ado:389-405, 676-692,
# 1843-1849; _desctab_collect.ado:1598-1726. Set TABTOOLS_STATA_DIR to the
# directory containing the pinned 2.5.1 ado files. No installed Stata ado
# or old golden-presentation translation is used as the oracle.

xv_stata_matrix <- function(d, cases, stata_dir) {
  scratch <- tempfile("tabtools-smd-stata-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  # Numeric codes preserve categorical shares and arm order across languages.
  sd <- d
  sd$arm <- match(sd$arm, c("A", "B", "C"))
  sd$reg <- match(sd$reg, sort(unique(sd$reg)))
  utils::write.csv(sd, file.path(scratch, "data.csv"), row.names = FALSE, na = ".")
  # Refuse quote/newline injection into Stata source; ordinary spaces work.
  if (grepl('["\r\n`]', stata_dir)) stop("TABTOOLS_STATA_DIR contains a Stata quoting character")
  body <- c("version 17.0", "clear all", "set more off",
            sprintf('adopath ++ "%s"', stata_dir),
            "quietly findfile desctab.ado",
            sprintf('assert r(fn) == "%s/desctab.ado"', stata_dir),
            'file open result using "results.csv", write text replace',
            'file write result "id,row,smd" _n',
            'file open meta using "metadata.txt", write text replace')
  for (i in seq_len(nrow(cases))) {
    ca <- cases[i, ]
    weight <- if (ca$weight == "fw") "[fweight=f]" else ""
    weight_option <- if (ca$weight == "wt") "wt(w)" else ""
    body <- c(body, 'import delimited using "data.csv", clear asdouble',
              if (ca$empty != "none") sprintf("replace %s = . if arm == 3", ca$empty),
              sprintf("quietly table1_tc %s, by(arm) vars(age contn \\ crp contln \\ sex bin \\ reg cat) smd smdtype(%s) nopvalue %s",
                      weight, ca$type, weight_option),
              "matrix S = r(table)",
              sprintf('file write meta "%s|`r(smdtype)\'|`r(smdnote)\'" _n', ca$id),
              "local names : rownames S", 'local col = colnumb(S, "smd")',
              "forvalues j = 1/`=rowsof(S)' {",
              "    local row : word `j' of `names'",
              sprintf('    file write result "%s,`row\'," %%21.16g (S[`j\', `col\']) _n', ca$id),
              "}")
  }
  body <- c(body, "file close result", "file close meta", 'display "SMD_PARITY_COMPLETE"')
  writeLines(body, file.path(scratch, "smd.do"))
  proc <- processx::run(Sys.which("stata-mp"), c("-b", "do", "smd.do"), wd = scratch,
                       error_on_status = FALSE, timeout = 120000)
  # Batch Stata exits 0 on do-file errors. The completion marker and error
  # codes are decisive; licence-bearing logs stay inside task scratch.
  logs <- list.files(scratch, pattern = "\\.log$", full.names = TRUE)
  if (length(logs) != 1L) stop("Stata SMD oracle did not produce exactly one log")
  log <- readLines(logs, warn = FALSE)
  errors <- grep("^r\\([0-9]+\\);", trimws(log), value = TRUE)
  if (proc$status != 0L || length(errors) || !any(trimws(log) == "SMD_PARITY_COMPLETE") ||
      !any(grepl("end of do-file", log, fixed = TRUE))) {
    stop("Stata SMD oracle failed: ", paste(errors, collapse = ", "))
  }
  list(table = utils::read.csv(file.path(scratch, "results.csv"), na.strings = ".", strip.white = TRUE),
       meta = strsplit(readLines(file.path(scratch, "metadata.txt")), "|", fixed = TRUE))
}

test_that("Stata 2.5.1 agrees on every weighted multi-group row kind and empty-group case", {
  skip_if(!nzchar(Sys.which("stata-mp")), "stata-mp not on PATH")
  skip_if_not_installed("processx")
  stata_dir <- Sys.getenv("TABTOOLS_STATA_DIR")
  skip_if(!nzchar(stata_dir), "set TABTOOLS_STATA_DIR to the pinned Stata 2.5.1 source directory")
  stata_dir <- normalizePath(stata_dir, mustWork = TRUE)
  expect_match(readLines(file.path(stata_dir, "desctab.ado"), n = 1L), "Version 2.5.1", fixed = TRUE)
  cases <- expand.grid(type = c("population", "maxpair"), weight = c("none", "wt", "fw"),
                       empty = c("none", "age", "crp", "sex", "reg"), stringsAsFactors = FALSE)
  cases$id <- sprintf("SMD%02d", seq_len(nrow(cases)))
  d <- xv_data(20261015, n = 90)
  # All four category levels and all three arms are present. Unequal group
  # sizes, variances and weights distinguish a shared scale from pair SDs.
  expect_identical(sort(unique(d$arm)), c("A", "B", "C"))
  expect_identical(sort(unique(d$reg)), c("e", "n", "s", "w"))
  ref <- xv_stata_matrix(d, cases, stata_dir)
  expect_identical(sort(unique(ref$table$id)), cases$id)
  expect_identical(nrow(ref$table), 120L)
  expect_length(ref$meta, 30L)
  for (i in seq_len(nrow(cases))) {
    ca <- cases[i, ]
    dd <- d
    if (ca$empty != "none") dd[dd$arm == "C", ca$empty] <- NA
    weights <- switch(ca$weight, none = list(), wt = list(wt = "w"), fw = list(fweight = "f"))
    tt <- do.call(table1_tc, c(list(data = dd, by = "arm", vars = xv_vars, smd = TRUE,
                                   smdtype = ca$type, nopvalue = TRUE), weights))
    rows <- ref$table[ref$table$id == ca$id, ]
    expect_identical(rows$row, rownames(tt$stored$table), info = ca$id)
    # Same closed-form algorithm; tolerance permits floating-point order
    # and Stata macro rounding, not a different variance convention.
    expect_equal(unname(tt$stored$table[, "smd"]), rows$smd, tolerance = 1e-10, label = ca$id)
    expect_identical(ref$meta[[i]], c(ca$id, tt$stored$smdtype, tt$stored$smdnote))
    if (ca$empty != "none") {
      expect_true(is.na(rows$smd[rows$row == ca$empty]), info = ca$id)
      expect_true(all(is.finite(rows$smd[rows$row != ca$empty])), info = ca$id)
    } else {
      expect_true(all(is.finite(rows$smd)), info = ca$id)
    }
  }
  expect_identical(sum(is.na(ref$table$smd)), 24L)
  expect_identical(sum(is.finite(ref$table$smd)), 96L)
  cat(sprintf("STATA MATRIX RECEIPT scenarios=%d cells=%d missing=%d finite=%d metadata=%d\n",
              length(unique(ref$table$id)), nrow(ref$table), sum(is.na(ref$table$smd)),
              sum(is.finite(ref$table$smd)), length(ref$meta)))
})
