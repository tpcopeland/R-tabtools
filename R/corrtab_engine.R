# Pearson: correlate manual p.7; rank correlation: installed Stata 17
# spearman.ado 4.2.8:293-309. Rank within each complete pair, never globally.
.cor_probability <- function(r, n, spearman) {
  if (!is.finite(r) || n <= 2L) return(NA_real_)
  if (r == 1) return(0)
  if (r == -1) return(if (spearman) NA_real_ else 0)
  if (abs(r) > 1) .cor_abort("Correlation is outside the representable unit interval.", "computation")
  2 * stats::pt(abs(r) * sqrt((n - 2) / (1 - r * r)), df = n - 2, lower.tail = FALSE)
}

.cor_engine <- function(s, spearman) {
  K <- length(s$vars)
  dn <- list(s$vars, s$vars)
  C <- P <- matrix(NA_real_, K, K, dimnames = dn)
  N <- matrix(0L, K, K, dimnames = dn)
  samples <- list(s$sample)
  pairs <- list()
  for (i in seq_len(K)) for (j in seq.int(i, K)) {
    keep <- !is.na(s$values[[i]]) & !is.na(s$values[[j]])
    n <- sum(keep)
    N[i, j] <- N[j, i] <- as.integer(n)
    a <- s$values[[i]][keep]
    b <- s$values[[j]][keep]
    if (n >= 2L && any(a != a[1L]) && any(b != b[1L])) {
      if (i == j) r <- 1 else {
        if (spearman) {
          a <- rank(a, ties.method = "average")
          b <- rank(b, ties.method = "average")
        }
        r <- stats::cor(a, b)
      }
      if (!is.finite(r)) .cor_abort("Finite varying inputs produced an undefined correlation.", "computation")
      C[i, j] <- C[j, i] <- r
      if (i != j) P[i, j] <- P[j, i] <- .cor_probability(r, n, spearman)
    }
    id <- paste0("pair", i, "_", j)
    samples[[length(samples) + 1L]] <- .tt_sample_population(id, "corrtab", "evaluation",
      variable = s$vars[i], group = s$vars[j],
      values = list(eligible_n = s$eligible_n, observed_n = n, missing_n = s$eligible_n - n),
      bases = list(observed_n = "original records nonmissing in both variables",
                   missing_n = "selected records missing either variable"))
    pairs[[length(pairs) + 1L]] <- data.frame(population_id = paste0("evaluation", length(pairs) + 1L, "/", id), row_variable = s$vars[i],
      column_variable = s$vars[j], N = n, stringsAsFactors = FALSE)
  }
  # Pearson delegates to pwcorr (native rc2000 for no observations).
  # The native Spearman branch instead retains its initialized NA matrices.
  if (!spearman && !any(diag(N) > 0L)) .cor_abort("No observed values remain in the selected variables.", "sample")
  list(C = C, P = P, N = N, pairs = do.call(rbind, pairs),
       sample = .tt_sample_bind(samples, prefixes = c("selection", paste0("evaluation", seq_len(length(samples) - 1L))),
                                commands = rep("corrtab", length(samples))))
}

.cor_rows <- function(e, s, shown) {
  ij <- expand.grid(row_index = seq_along(s$vars), column_index = seq_along(s$vars))
  ij$row_variable <- s$vars[ij$row_index]
  ij$column_variable <- s$vars[ij$column_index]
  ij$estimate <- as.vector(e$C)
  ij$p_value <- as.vector(e$P)
  ij$N <- as.vector(e$N)
  ij$state <- ifelse(is.na(ij$estimate), "empty", ifelse(is.na(ij$p_value), "notest", "est"))
  ij$displayed <- as.vector(shown)
  ij
}
