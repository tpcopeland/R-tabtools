# Golden parity for regtab (IMPLEMENTATION_PLAN.md §8). Each scenario is
# skipped until its phase is done (golden_phase_done in helper-golden.R).

# Known divergences: cells where the R and Stata fits agree to ~1e-11 but the
# displayed text sits on a rounding knife edge, so the digits depend on
# floating-point noise below the fit's precision. Each is asserted exactly
# (the R cell must be one of `got`, or already equal the golden), then the
# golden text is substituted so every comparator still checks the rest of
# the scenario. Proofs: Stata probes in IMPLEMENTATION_PLAN.md, Phase 4
# findings.
regtab_known_divergences <- list(
  # 2.rep78 in regress price i.foreign##i.rep78 is exactly 1403.125 (a
  # difference of cell means). Stata's regress returns 1403.1249999999843,
  # R's lm() 1403.1250000000064; round(x, 0.01) takes them to opposite sides
  # of the tie.
  R13 = list(row = 6L, col = 2L, want = "1403.12", got = "1403.13"),
  # Logistic intercept CI upper bound exp(22.56) = 6.3e9: two decimals need
  # 1e-12 relative agreement. Stata's own fit with tolerance(1e-14)
  # ltolerance(1e-14) nrtolerance(1e-14) gives 6306677263.73 instead of the
  # golden's 6306677255.96 (default convergence); R gives 6306677263.71.
  R14 = list(row = 3L, col = 6L, want = "(127.98, 6306677255.96)",
             check = function(got) {
               hi <- as.numeric(sub("^.*, (.*)\\)$", "\\1", got))
               expect_equal(hi, 6306677263.7290077, tolerance = 1e-11)
               expect_true(startsWith(got, "(127.98, "))
             })
)

regtab_patch <- function(id) {
  k <- regtab_known_divergences[[id]]
  if (is.null(k)) return(NULL)
  function(tt) {
    got <- tt$body[k$row, k$col]
    if (!identical(got, k$want)) {
      if (!is.null(k$check)) k$check(got) else expect_true(got %in% k$got, label = paste(id, "known divergence"))
      tt$body[k$row, k$col] <- k$want
    }
    tt
  }
}

# Warnings a scenario's own model fit must raise (Milestone H, H16; finding
# F22). R25q fits glm.nb(prior_hosp ~ ...) on the cohort, whose prior_hosp is
# equidispersed (mean 1.8071, variance 1.8077): the negative-binomial MLE is
# on the boundary (alpha -> 0, theta -> Inf), so MASS::theta.ml() stops at
# its iteration limit (theta about 2,576). Raising maxit moves theta to
# 6,724 but the coefficients by 2.8e-8, so the displayed cells are not
# affected (Phase 4 review P2-8 keeps R25q a tolerance scenario). The
# warning is asserted, not ignored: if a better-converged fixture or MASS
# version stops raising it, this test fails and the note can be revisited.
regtab_expected_warnings <- list(R25q = "iteration limit reached")

expect_golden_warning <- function(expr, pattern) {
  seen <- character()
  withCallingHandlers(expr, warning = function(w) {
    if (grepl(pattern, conditionMessage(w), fixed = TRUE)) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  })
  expect_true(length(seen) > 0L, label = paste0("the expected warning \"", pattern, "\""))
}

# The full sweep fits every scenario's models on the 15,000-row cohort, so
# it runs off CRAN; a core subset always runs.
sc <- golden_scenarios()
core <- c("R01", "R04", "R12", "R13b")
for (id in sc$id[sc$command == "regtab"]) {
  test_that(paste(id, golden_scenario(id)$description, sep = ": "), {
    if (!id %in% core) skip_on_cran()
    call <- golden_scenario(id)$r_call
    for (pkg in c("survival", "MASS", "nnet", "pscl", "lme4", "nlme", "geepack", "survey")) {
      if (grepl(paste0("\\b", pkg, "::"), call)) skip_if_not_installed(pkg)
    }
    if (id %in% names(regtab_expected_warnings)) {
      expect_golden_warning(run_golden_scenario(id, patch = regtab_patch(id)), regtab_expected_warnings[[id]])
    } else {
      run_golden_scenario(id, patch = regtab_patch(id))
    }
  })
}
