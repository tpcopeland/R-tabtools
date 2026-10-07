# Epitab manual pp.53-55; native crosstab.ado:345-509. No continuity correction.
.xt_fisher <- function(f, level) {
  if (any(f > .Machine$integer.max)) .xt_abort("Exact Fisher counts exceed R's integer table limit.", "inference")
  tryCatch(stats::fisher.test(f, conf.level = level / 100), error = function(e) {
    .xt_abort("Exact Fisher inference could not be computed; reduce the table or counts. No approximation was substituted.",
              "inference", parent = e)
  })
}

# Native cc delegates to Stata17 cci.ado (7.1.10): Cornfield starting
# values, then equal-tailed hypergeometric secant inversion. Its published
# return stops at a 0.5% successive-OR difference, not fisher.test's root.
.xt_or_limits <- function(a, b, c, d, level) {
  z <- stats::qnorm((1 + level / 100) / 2)
  cases <- a + b; exposed <- a + c; controls <- c + d
  odds <- function(x) {
    value <- x * (d - a + x) / ((cases - x) * (exposed - x))
    if (is.finite(value)) value else NA_real_
  }
  cornfield <- function(direction, adjusted = FALSE) {
    x <- a; retry <- 0L
    for (iteration in seq_len(10000L)) {
      previous <- x
      precision <- 1 / x + 1 / (cases - x) + 1 / (exposed - x) + 1 / (d - a + x)
      step <- if (!is.na(precision) && precision >= 0) z / sqrt(precision) else NA_real_
      x <- a + direction * (step + if (adjusted) .5 else 0)
      if (adjusted) {
        retry <- retry + 1L
        x <- (retry * x + (retry - 1L) * previous) / (2 * retry - 1L)
      }
      if (!is.finite(x)) {
        retry <- retry + 1L
        x <- a + direction * retry
        if (x < max(0, a - d) || x > min(cases, exposed)) return(NA_real_)
      }
      if (abs(x - previous) <= if (adjusted) 1e-7 else .001) return(odds(x))
    }
    .xt_abort("Native Cornfield starting values did not converge.", "inference")
  }
  unadjusted <- vapply(c(-1, 1), cornfield, 0.0)
  if (is.na(unadjusted[1L])) unadjusted[1L] <- 0
  # cc/cci explicitly uses Cornfield, rather than exact inversion, with
  # zero cells; undefined Cornfield limits remain ordinary missing.
  if (any(c(a, b, c, d) == 0)) return(unadjusted)
  if (any(c(a, b, c, d) > .Machine$integer.max)) {
    .xt_abort("Exact Fisher counts exceed R's integer table limit.", "inference")
  }
  adjusted <- vapply(c(-1, 1), cornfield, 0.0, adjusted = TRUE)
  woolf <- exp(log(a * d / (b * c)) + c(-1, 1) * z * sqrt(1 / a + 1 / b + 1 / c + 1 / d))
  if (unadjusted[1L] < 0 || is.na(adjusted[1L]) || adjusted[1L] <= 0) {
    unadjusted[1L] <- woolf[1L]; adjusted[1L] <- .75 * woolf[1L]
  }
  if (is.na(unadjusted[2L]) || is.na(adjusted[2L])) {
    unadjusted[2L] <- woolf[2L]; adjusted[2L] <- 1.3 * woolf[2L]
  }
  support <- seq.int(max(0, cases - (b + d)), min(exposed, cases))
  log_mass <- -lgamma(support + 1) - lgamma(exposed - support + 1) -
    lgamma(cases - support + 1) - lgamma(b + d - cases + support + 1)
  tail <- function(psi, lower) {
    if (!is.finite(psi) || psi <= 0) return(NA_real_)
    mass <- log_mass + support * log(psi)
    mass <- exp(mass - max(mass))
    cumulative <- cumsum(mass / sum(sort(mass)))
    at <- match(a, support)
    if (lower) if (at == 1L) 1 else 1 - cumulative[at - 1L] else cumulative[at]
  }
  alpha <- (100 - level) / 200
  invert <- function(index) {
    previous <- unadjusted[index]; current <- adjusted[index]
    lower <- index == 1L
    p_previous <- tail(previous, lower); p_current <- tail(current, lower)
    for (iteration in seq_len(99L)) {
      if (!is.finite(current)) return(if (lower) 0 else NA_real_)
      if (abs(current - previous) < .005 * current) return(current)
      slope <- (p_current - p_previous) / (current - previous)
      next_value <- (alpha + slope * current - p_current) / slope
      previous <- current; p_previous <- p_current
      current <- next_value; p_current <- tail(current, lower)
    }
    if (!is.finite(current)) return(if (lower) 0 else NA_real_)
    if (abs(current - previous) < .005 * current) return(current)
    .xt_abort("Native exact odds-ratio limits did not converge.", "inference")
  }
  vapply(1:2, invert, 0.0)
}

.xt_inference <- function(f, scores, o) {
  n <- sum(f)
  expected <- outer(rowSums(f) / n, colSums(f))
  use_fisher <- o$exact || o$fisher || min(expected) < 5
  chi2 <- if (use_fisher) NA_real_ else sum((f - expected)^2 / expected)
  p <- if (use_fisher) .xt_fisher(f, o$level)$p.value else
    stats::pchisq(chi2, (nrow(f) - 1L) * (ncol(f) - 1L), lower.tail = FALSE)
  if (!is.finite(p)) .xt_abort("The ordinary test is undefined.", "inference")
  out <- list(chi2 = chi2, p = p, test_name = if (use_fisher) "Fisher's exact test" else
                "Pearson's chi-squared test", test_method = if (use_fisher) "Fisher exact" else "Pearson uncorrected")
  if (o$or || o$rr || o$rd) {
    if (!identical(dim(f), c(2L, 2L))) .xt_abort("or, rr and rd require a 2 by 2 table.", "association")
    a <- f[2L, 2L]; b <- f[2L, 1L]; c <- f[1L, 2L]; d <- f[1L, 1L]
    n1 <- a + c; n0 <- b + d
    z <- stats::qnorm((1 + o$level / 100) / 2)
    if (o$or) {
      point <- (a / b) * (d / c)
      if (!is.finite(point)) .xt_abort("Odds ratio is undefined for this table.", "association")
      ci <- .xt_or_limits(a, b, c, d, o$level)
      out$or <- point; out$or_lo <- unname(ci[1L]); out$or_hi <- unname(ci[2L])
    }
    if (o$rr) {
      point <- (a / n1) / (b / n0)
      if (!is.finite(point)) .xt_abort("Risk ratio is undefined for this table.", "association")
      se <- sqrt(c / (a * n1) + d / (b * n0))
      ci <- if (point > 0 && is.finite(se)) exp(log(point) + c(-1, 1) * z * se) else
        rep(NA_real_, 2L)
      # Native undefined Wald limits are ordinary missing, not invented 0/Inf.
      ci[!is.finite(ci)] <- NA_real_
      out$rr <- point; out$rr_lo <- ci[1L]; out$rr_hi <- ci[2L]
    }
    if (o$rd) {
      point <- a / n1 - b / n0
      se <- sqrt(a / n1 * c / n1 / n1 + b / n0 * d / n0 / n0)
      if (!is.finite(point) || !is.finite(se)) .xt_abort("Risk difference is undefined for this table.", "association")
      out$rd <- point; out$rd_lo <- point - z * se; out$rd_hi <- point + z * se
    }
  }
  if (o$cochran) {
    if (nrow(f) != 2L) .xt_abort("cochran requires a binary row variable.", "trend")
    origin <- min(scores)
    s <- scores - origin
    if (any(!is.finite(s))) {
      scale <- max(abs(scores))
      s <- scores / scale - origin / scale
    }
    s <- s / max(s)
    centred <- s - sum(colSums(f) / n * s)
    event <- sum(f[2L, ]) / n
    variance <- event * (1 - event) * sum(colSums(f) * centred^2)
    if (!is.finite(variance) || variance <= 0) .xt_abort("Cochran-Armitage trend is undefined.", "trend")
    z <- sum(f[2L, ] * centred) / sqrt(variance)
    out$z_trend <- z; out$chi2_trend <- z^2
    out$p_trend <- stats::pchisq(z^2, 1, lower.tail = FALSE)
    out$trend_method <- "Cochran-Armitage"
  }
  if (o$trend) {
    # Native crosstab.ado435-449 refuses unavailable Spearman inference; the
    # authenticated XT015 N=2 table returns 498 before producing any sink.
    if (n <= 2) .xt_abort("Spearman trend requires more than two contributing observations.", "trend")
    # Weighted midranks equal literal frequency expansion without allocating it.
    midrank <- function(margin) cumsum(margin) - (margin - 1) / 2
    r <- midrank(rowSums(f)); c <- midrank(colSums(f))
    r <- r - (n + 1) / 2; c <- c - (n + 1) / 2
    rho <- sum(f * outer(r, c)) / sqrt(sum(rowSums(f) * r^2) * sum(colSums(f) * c^2))
    if (nrow(f) == ncol(f)) {
      if (all(f[row(f) != col(f)] == 0)) rho <- 1
      if (all(f[row(f) + col(f) != nrow(f) + 1L] == 0)) rho <- -1
    }
    # Installed Stata17 spearman.ado:293-309: positive perfect rank special case,
    # otherwise t(N-2), including undefined negative-perfect / N=2 endpoints.
    if (!is.finite(rho) || rho < -1 || rho > 1) .xt_abort("Spearman rank correlation is undefined.", "trend")
    p <- if (rho == 1) 0 else if (n <= 2 || rho == -1) NA_real_ else
      2 * stats::pt(abs(rho) * sqrt((n - 2) / (1 - rho^2)), n - 2, lower.tail = FALSE)
    if (!is.finite(p)) .xt_abort("Spearman trend inference is undefined for these ranks and sample size.", "trend")
    out$p_trend <- p; out$trend_method <- "Spearman rank correlation"
  }
  out
}
