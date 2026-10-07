# Exact event-risk log-rank hypergeometric covariance: installed logrank.ado241-256.
.sv_logrank <- function(records, groups) {
  no_failure <- !any(records$event == 1L)
  if (groups < 2L || no_failure) return(list(p = NA_real_, chi2 = NA_real_, df = NA_real_,
    no_failure = no_failure, state = "notest", reason = if (no_failure) "no_included_failures" else "ungrouped"))
  times <- sort(unique(records$exit[records$event == 1L]))
  score <- numeric(groups); covariance <- matrix(0, groups, groups)
  risks <- deaths <- matrix(0, length(times), groups)
  for (i in seq_along(times)) {
    for (g in seq_len(groups)) {
      r <- records[records$group == g, , drop = FALSE]
      risks[i, g] <- .sv_risk(r, times[i])
      deaths[i, g] <- sum(r$frequency[r$event == 1L & r$exit == times[i]])
    }
    Y <- sum(risks[i, ]); d <- sum(deaths[i, ])
    if (Y <= 0 || d > Y) .sv_abort("Log-rank has an invalid risk set.", "tabtools_error_survival_logrank")
    expected <- risks[i, ] * d / Y
    score <- score + deaths[i, ] - expected
    if (Y > 1) {
      factor <- d * (Y - d) / (Y^2 * (Y - 1))
      covariance <- covariance + factor * (diag(risks[i, ] * Y, groups) - tcrossprod(risks[i, ]))
    }
  }
  eig <- eigen(covariance, symmetric = TRUE)
  threshold <- max(abs(eig$values), 1) * groups * .Machine$double.eps
  if (any(eig$values < -threshold)) .sv_abort("Log-rank covariance is invalid.", "tabtools_error_survival_logrank")
  positive <- eig$values > threshold
  projected <- drop(crossprod(eig$vectors, score))
  if (any(abs(projected[!positive]) > sqrt(threshold) * max(1, max(abs(score))))) {
    .sv_abort("Log-rank score lies outside its covariance support.", "tabtools_error_survival_logrank")
  }
  chi2 <- sum(projected[positive]^2 / eig$values[positive])
  df <- groups - 1L  # Native df is group count minus one, not numerical rank.
  list(p = stats::pchisq(chi2, df, lower.tail = FALSE), chi2 = chi2, df = df,
    score = score, covariance = covariance, numerical_rank = sum(positive),
    time = times, risk = risks, events = deaths, no_failure = FALSE,
    state = "est", reason = "", method = "hypergeometric_logrank_generalized_inverse")
}
