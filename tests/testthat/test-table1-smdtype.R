# table1_tc(smdtype =): the multi-group balance statistics and the footnote
# that names what a three-or-more-group SMD compares.

smd3_data <- function() {
  data.frame(
    arm = factor(rep(c("A", "B", "C"), c(8, 10, 12))),
    age = c(41, 45, 47, 50, 52, 55, 58, 60, 44, 46, 49, 51, 53, 56, 57, 59, 62, 65,
            55, 58, 60, 63, 66, 68, 70, 72, 75, 77, 80, 83),
    sex = c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1,
            1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0),
    grp = factor(rep(c("x", "y", "z"), 10))
  )
}

test_that("a three-group pair SMD names the compared pair in the footnote of every export", {
  d <- smd3_data()
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn", sex = "bin"), smd = TRUE,
                  footnote = "User note.")
  note <- "SMD compares A vs B only (the first two of 3 groups)."
  expect_identical(tt$footnote, paste("User note.", note))
  csv <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt, csv)
  expect_true(any(grepl(note, readLines(csv), fixed = TRUE)))
  md <- withr::local_tempfile(fileext = ".md")
  tt_write_markdown(tt, md)
  expect_true(any(grepl(note, readLines(md), fixed = TRUE)))
  # The header names the pair too, so a sink without the footnote
  # (as.data.frame(), the console listing) never shows a bare "SMD".
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "SMD (A vs B)")
  expect_true("SMD (A vs B)" %in% unlist(as.data.frame(tt)[1, ]))
  expect_identical(tt$meta$console_before, paste("Note:", note))
  expect_identical(tt$stored$smdtype, "pair")
  expect_identical(tt$stored$smdnote, note)
})

test_that("the SMD note joins a backslash-separated footnote with a backslash, once", {
  d <- smd3_data()
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, footnote = "One \\ two")
  expect_identical(tt$footnote, "One \\ two \\ SMD compares A vs B only (the first two of 3 groups).")
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE,
                  footnote = "SMD compares A vs B only (the first two of 3 groups).")
  expect_identical(tt$footnote, "SMD compares A vs B only (the first two of 3 groups).")
})

test_that("smdpair picks the compared pair by label or by numeric value", {
  d <- smd3_data()
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn", sex = "bin"), smd = TRUE, smdpair = c("C", "A"),
                  nopvalue = TRUE)
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "SMD (C vs A)")
  expect_identical(tt$stored$smdnote, "SMD compares C vs A only (2 of 3 groups, chosen with smdpair()).")
  a <- d$age[d$arm == "C"]
  b <- d$age[d$arm == "A"]
  expect_equal(unname(tt$stored$table["age", "smd"]),
               abs(mean(a) - mean(b)) / sqrt((stats::var(a) + stats::var(b)) / 2), tolerance = 1e-12)
  # Weighted pair SMDs follow the chosen pair as well.
  d$w <- rep(c(1, 2), 15)
  tw <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c("C", "A"), wt = "w")
  sub2 <- droplevels(d[d$arm %in% c("A", "C"), ])
  t2 <- table1_tc(sub2, by = "arm", vars = c(age = "contn"), smd = TRUE, wt = "w")
  expect_equal(tw$stored$table[, "smd"], t2$stored$table[, "smd"], tolerance = 1e-12)
  # A numeric by(): numbers are by() values.
  dn <- d
  dn$arm <- c(A = 10, B = 20, C = 30)[as.character(d$arm)]
  tn <- table1_tc(dn, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c(30, 10))
  expect_equal(tn$stored$table[, "smd"], tt$stored$table["age", "smd"], tolerance = 1e-12)
})

test_that("smdpair with two groups names the pair in the header and adds no note", {
  d <- smd3_data()
  d <- droplevels(d[d$arm != "C", ])
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdpair = c("B", "A"))
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "SMD (B vs A)")
  expect_identical(tt$footnote, "")
  expect_null(tt$stored$smdnote)
})

test_that("smdpair is refused when it cannot name a pair or the statistic ignores it", {
  d <- smd3_data()
  run <- function(...) table1_tc(d, by = "arm", vars = c(age = "contn"), ...)
  expect_error(run(smd = TRUE, smdpair = c("A", "A")), "two different groups")
  expect_error(run(smd = TRUE, smdpair = c("A", "Z")), "does not name exactly one group")
  expect_error(run(smd = TRUE, smdpair = "A"), "exactly two groups")
  expect_error(run(smd = TRUE, smdtype = "maxpair", smdpair = c("A", "B")), "requires")
  expect_error(run(smdpair = c("A", "B")), "requires")
})

test_that("two groups get no SMD footnote", {
  d <- smd3_data()
  d <- d[d$arm != "C", ]
  d$arm <- droplevels(d$arm)
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE)
  expect_identical(tt$footnote, "")
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "SMD")
  expect_identical(tt$stored$smdtype, "pair")
  expect_null(tt$stored$smdnote)
})

test_that("smdtype = 'population' matches cobalt's group-vs-all SMD (McCaffrey eq. 5)", {
  # cobalt 4.6.3: bal.tab(arm ~ age + sex, estimand = "ATE", pairwise = FALSE,
  # s.d.denom = "all", binary = "std"), max over groups of |Diff.Un|.
  tt <- table1_tc(smd3_data(), by = "arm", vars = c(age = "contn", sex = "bin"), smd = TRUE,
                  smdtype = "population", nopvalue = TRUE)
  expect_equal(unname(tt$stored$table[, "smd"]), c(0.879755808022535, 0.639039154296497), tolerance = 1e-12)
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "Pop. SB")
  expect_match(tt$footnote, "McCaffrey et al. 2013", fixed = TRUE)
})

test_that("smdtype = 'maxpair' matches cobalt's pairwise SMD on the all-groups pooled SD", {
  # cobalt 4.6.3: bal.tab(..., s.d.denom = "pooled"), max over the three pairs.
  tt <- table1_tc(smd3_data(), by = "arm", vars = c(age = "contn", sex = "bin"), smd = TRUE,
                  smdtype = "maxpair", nopvalue = TRUE)
  expect_equal(unname(tt$stored$table[, "smd"]), c(2.387998740048635, 1.104315260748465), tolerance = 1e-12)
  expect_identical(tt$header[[1]]$text[tt$cols$role == "smd"], "Max SMD")
})

test_that("with two groups maxpair equals the absolute pair SMD, weighted and unweighted", {
  d <- smd3_data()
  d <- d[d$arm != "C", ]
  d$arm <- droplevels(d$arm)
  d$w <- rep(c(1, 2, 0.5), length.out = nrow(d))
  v <- c(age = "contn", sex = "bin", grp = "cat")
  for (wt in list(NULL, "w")) {
    a <- table1_tc(d, by = "arm", vars = v, smd = TRUE, wt = wt, nopvalue = TRUE)
    b <- table1_tc(d, by = "arm", vars = v, smd = TRUE, smdtype = "maxpair", wt = wt, nopvalue = TRUE)
    expect_equal(b$stored$table[, "smd"], a$stored$table[, "smd"], tolerance = 1e-12)
  }
})

test_that("population SB keeps the overall mean and SD unweighted under wt", {
  d <- smd3_data()
  d$w <- rep(c(1, 3), 15)
  tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdtype = "population", wt = "w")
  wm <- vapply(split(seq_len(nrow(d)), d$arm), function(i) sum(d$w[i] * d$age[i]) / sum(d$w[i]), 0)
  expect_equal(unname(tt$stored$table[1, "smd"]), max(abs(wm - mean(d$age))) / stats::sd(d$age),
               tolerance = 1e-12)
})

test_that("categorical population SB is the largest per-level value", {
  d <- smd3_data()
  tt <- table1_tc(d, by = "arm", vars = c(grp = "cat"), smd = TRUE, smdtype = "population", nopvalue = TRUE)
  pp <- prop.table(table(d$grp))
  pg <- prop.table(table(d$arm, d$grp), 1)
  want <- max(abs(sweep(pg, 2, pp)) / rep(sqrt(pp * (1 - pp)), each = 3))
  expect_equal(unname(tt$stored$table[1, "smd"]), want, tolerance = 1e-12)
})

test_that("a group with no values for a variable leaves the multi-group statistic blank", {
  d <- smd3_data()
  d$age[d$arm == "C"] <- NA
  for (ty in c("population", "maxpair")) {
    tt <- table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdtype = ty, nopvalue = TRUE)
    expect_true(is.na(tt$stored$table[1, "smd"]))
  }
})

test_that("smdtype is validated and requires smd = TRUE", {
  d <- smd3_data()
  expect_error(table1_tc(d, by = "arm", vars = c(age = "contn"), smdtype = "population"),
               "requires")
  expect_error(table1_tc(d, by = "arm", vars = c(age = "contn"), smd = TRUE, smdtype = "bogus"))
})

test_that("nosmdhighlight disables Excel styling and preserves SMD values for every type", {
  d <- smd3_data()
  for (st in c("pair", "population", "maxpair")) {
    run <- function(...) table1_tc(d, by = "arm", vars = c(age = "contn", sex = "bin", grp = "cat"),
                                   smd = TRUE, smdtype = st, nopvalue = TRUE, ...)
    normal <- run()
    disabled <- run(nosmdhighlight = TRUE)
    threshold <- run(smdthreshold = -1)
    expect_identical(disabled, threshold)
    expect_identical(disabled$stored, normal$stored)
    expect_identical(disabled$body, normal$body)
    expect_identical(disabled$style$smdthreshold, -1)
    expect_identical(normal, run(nosmdhighlight = FALSE))
    # Inspect the actual Excel layout rules: the default fixture exceeds
    # the threshold; neither disabling spelling emits an SMD fill rule.
    normal_rules <- .xlsx_layout_table1(normal)$rules
    disabled_rules <- .xlsx_layout_table1(disabled)$rules
    expect_true(any(normal_rules$color == normal$style$smdcolor, na.rm = TRUE))
    expect_false(any(disabled_rules$color == disabled$style$smdcolor, na.rm = TRUE))
    md_normal <- withr::local_tempfile(fileext = ".md")
    md_disabled <- withr::local_tempfile(fileext = ".md")
    tt_write_markdown(normal, md_normal)
    tt_write_markdown(disabled, md_disabled)
    expect_identical(readLines(md_normal), readLines(md_disabled))
  }
})

test_that("nosmdhighlight rejects invalid switches and explicit threshold combinations", {
  run <- function(...) table1_tc(smd3_data(), by = "arm", vars = c(age = "contn"), smd = TRUE, ...)
  for (bad in list(NA, NULL, logical(), c(TRUE, FALSE), 1, "TRUE")) {
    expect_error(run(nosmdhighlight = bad), class = "tabtools_error_smdhighlight")
  }
  for (threshold in c(-1, 0.1, 0.5)) {
    expect_error(run(nosmdhighlight = TRUE, smdthreshold = threshold),
                 class = "tabtools_error_smdhighlight")
    expect_identical(run(nosmdhighlight = FALSE, smdthreshold = threshold),
                     run(smdthreshold = threshold))
  }
  expect_identical(desctab(smd3_data(), by = "arm", vars = c(age = "contn"), smd = TRUE,
                           nosmdhighlight = TRUE), run(smdthreshold = -1))
})

test_that("frequency-weighted multi-group SMD equals expanded records for every row kind", {
  d <- smd3_data()
  d$f <- rep(c(1, 3, 2), length.out = nrow(d))
  expanded <- d[rep(seq_len(nrow(d)), d$f), ]
  for (st in c("population", "maxpair")) {
    for (vars in list(c(age = "contn"), c(sex = "bin"), c(grp = "cat"))) {
      weighted <- table1_tc(d, by = "arm", vars = vars, smd = TRUE,
                            smdtype = st, fweight = "f", nopvalue = TRUE)
      replicated <- table1_tc(expanded, by = "arm", vars = vars, smd = TRUE,
                              smdtype = st, nopvalue = TRUE)
      expect_equal(weighted$stored$table, replicated$stored$table, tolerance = 1e-12)
    }
  }
})

test_that("empty-variable groups blank all multi-group row kinds with either weight kind", {
  d <- smd3_data()
  d$w <- rep(c(0.5, 2, 3), length.out = nrow(d))
  d$f <- rep(c(1, 3, 2), length.out = nrow(d))
  # The group remains in by() and has positive weights, but lacks every
  # covariate value. It must not silently disappear from the SMD contrast.
  d[d$arm == "C", c("age", "sex", "grp")] <- NA
  for (st in c("population", "maxpair")) {
    for (weights in list(list(), list(wt = "w"), list(fweight = "f"))) {
      tt <- do.call(table1_tc, c(list(data = d, by = "arm", vars = c(age = "contn", sex = "bin", grp = "cat"),
                                     smd = TRUE, smdtype = st, nopvalue = TRUE), weights))
      expect_identical(rownames(tt$stored$table), c("age", "sex", "grp"))
      expect_identical(unname(tt$stored$table[, "smd"]), rep(NA_real_, 3))
      smd_col <- which(tt$cols$role == "smd")
      expect_true(all(tt$body[, smd_col] == ""))
    }
  }
})
