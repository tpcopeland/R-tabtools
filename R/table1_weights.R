# table1_tc weighting (plan tasks 3.1-3.4): wt() importance weights, [fweight=]
# frequency weights, the effective-sample-size row, and weighted SMDs. A port
# of the weighted branches of _t1tcfc_collect_mata (_desctab_collect.ado:
# 1391-1645), the SMD loop (:499-617), and the weighted display policy of
# desctab.ado:247-293, :1366-1378.
#
# Weights reach the Phase 2 row builders through `o$wx`, a list with
#   kind  "wt" (importance weights), "fw" (frequency weights), or "none"
#         (unit weights; used when only smallcells() needs the counts)
#   w     statistic weights, one per data row (Mata `stat_source`)
#   d     display weights, one per data row (Mata `disp_source`): 1 under wt(),
#         the frequency under fweight, 1 unweighted
# (_desctab_collect.ado:1436-1447).

#' Resolve wt() / fweight into the analysis rows and weight vectors
#'
#' `wt`: missing weights leave the sample, negative weights are an error
#' (checked before by() is applied, desctab.ado:398-409), zero weights leave
#' the sample. `fweight`: missing and zero weights leave the sample (Stata's
#' `marksample` drops them, so they are not counted anywhere and do not
#' define categories; verified in Stata 17); negative weights are an error
#' (r(402)) and non-integer weights too (r(401)). Both errors come from
#' desctab's `syntax ... [fweight]` / `marksample touse, novarlist`
#' (desctab.ado:37, :398), which validate every record in the sample before
#' by() or the variables mark anything out: a fractional weight on a record
#' with by() or the variable missing is still r(401), and a negative weight
#' is reported before a fractional one (probed in Stata 17, Phase 3 review
#' P2-1).
#'
#' `Inf`, `-Inf` and `NaN` weights are refused (R only: a Stata variable
#' cannot hold them). Finite but huge weights are accepted as Stata accepts
#' them (probed: `[fweight]` of 1e15 or 1e300 and `wt()` of 1e300 or 8.9e307
#' all run). Where a `wt()` sum overflows Stata's range, tabtools 2.1.12
#' divides the weights by a power of two and recomputes (.t1w_wscale();
#' Milestone H, H10, and C5), and so does R; frequency weights whose total
#' overflows are refused: every count would be infinite. A weight of 2^1023 (8.99e307) or more is refused: Stata's
#' largest double is just below it, so there it is a missing value.
#'
#' @return list(keep, kind, w) with `w` aligned to the kept rows, or NULL
#'   when no weight is given.
#' @keywords internal
#' @noRd
.t1_weight_prep <- function(data, wt = NULL, fweight = NULL) {
  if (is.null(wt) && is.null(fweight)) return(NULL)
  kind <- if (!is.null(wt)) "wt" else "fw"
  nm <- if (kind == "wt") wt else fweight
  arg <- if (kind == "wt") "wt" else "fweight"
  if (!is.character(nm) || length(nm) != 1L || is.na(nm)) {
    cli::cli_abort("{.arg {arg}} must be a single variable name.", call = NULL)
  }
  if (!nm %in% names(data)) cli::cli_abort("{.arg {arg}} variable {.var {nm}} not found.", call = NULL)
  x <- data[[nm]]
  if (!is.numeric(x) || is.factor(x)) {
    cli::cli_abort("{.arg {arg}} variable {.var {nm}} must be numeric.", call = NULL)
  }
  w <- as.numeric(unclass(x))
  attributes(w) <- NULL
  bad <- is.nan(w) | is.infinite(w)
  if (any(bad)) {
    cli::cli_abort(c("{.arg {arg}} variable {.var {nm}} must be finite.",
                     "x" = "{sum(bad)} record{?s} hold{?s/} {.val {unique(format(w[bad]))}}.",
                     "i" = "Use {.code NA} for a record that should leave the sample."), call = NULL)
  }
  # Beyond Stata's largest double (8.99e307) a Stata weight is a missing
  # value that drops the record; R refuses rather than drop it silently.
  big <- !is.na(w) & abs(w) >= .stata_missing_from
  if (any(big)) {
    cli::cli_abort(c("{.arg {arg}} variable {.var {nm}} holds values beyond Stata's largest number (8.99e+307).",
                     "x" = "{sum(big)} record{?s} hold{?s/} {.val {unique(format(w[big]))}}.",
                     "i" = "Weighted statistics do not depend on the scale of the weights; rescale them."),
                   call = NULL)
  }
  if (kind == "wt") {
    if (any(!is.na(w) & w < 0)) {
      cli::cli_abort("{.arg wt} variable must be non-negative.", call = NULL)
    }
  } else {
    if (any(!is.na(w) & w < 0)) {
      cli::cli_abort("{.arg fweight}: negative weights encountered.", call = NULL)
    }
    # Every record, whatever its by() value (marksample, desctab.ado:398).
    if (any(!is.na(w) & w != round(w))) {
      cli::cli_abort("{.arg fweight}: may not use noninteger frequency weights.", call = NULL)
    }
  }
  keep <- !is.na(w) & w > 0
  if (!any(keep)) cli::cli_abort("no observations.", call = NULL)
  if (kind == "fw" && !.st_ok(sum(w[keep]))) {
    cli::cli_abort(c("{.arg fweight}: the frequencies of {.var {nm}} sum beyond the largest representable number.",
                     "i" = "Frequency weights count records; no table can be formed from them."),
                   call = NULL)
  }
  w <- w[keep]
  # Tiny importance weights (codex audit F07): w = 1e-310 is accepted, but
  # sums, squares and n / (sum w (n - 1)) under- or overflow on the way to
  # a finite SD. wt() statistics do not depend on the scale of the weights
  # and dividing by a power of two is exact, so weights whose largest is
  # below 1 are divided by the power of two just above it (at least 2^-1021,
  # so 1e-310 becomes about 2.2e-3) first; results for weights of ordinary
  # scale are unchanged (R only; Stata computes at the given scale).
  # Frequency weights count records and are never rescaled.
  if (kind == "wt" && max(w) < 1) w <- w / .t1w_wscale(w)
  list(keep = keep, kind = kind, w = w)
}

# Keep rows of a data frame, preserving every column's attributes (labels).
.t1_keep_rows <- function(data, keep) {
  out <- lapply(data, .t1_sub, keep = keep)
  structure(out, names = names(data), class = "data.frame", row.names = seq_len(sum(keep)))
}

# The o$wx list for a weight kind (see the header comment).
.t1_wx <- function(kind, n, w = NULL) {
  switch(kind,
    wt = list(kind = "wt", w = w, d = rep(1, n)),
    fw = list(kind = "fw", w = w, d = w),
    none = list(kind = "none", w = rep(1, n), d = rep(1, n)))
}

#' Weighted quantile, a port of `_t1tcfc_wquantile`
#'
#' `_desctab_collect.ado:1340-1367`: drop non-positive weights, sort by x
#' (stable), walk the cumulative weight; when it equals `p * total` within
#' `1e-10 * max(1, total)` return the average of this and the next order
#' statistic, else the first value whose cumulative weight exceeds the
#' target. With unit weights this is `quantile(type = 2)`.
#' @keywords internal
#' @noRd
# The power of two `_t1tcfc_wscale()` divides weights by after an
# overflow (`_desctab_collect.ado`, tabtools 2.1.12): 2^k with
# k = ceil(ln(max w) / ln 2) held to [-1021, 1022], so max(w) / 2^k <= 2.
# Weighted means, variances, quantiles, shares and the ESS do not depend on
# the scale of the weights, and dividing by a power of two is exact.
.t1w_wscale <- function(w) {
  if (!length(w)) return(1)
  m <- max(w)
  if (!is.finite(m) || m <= 0) return(1)
  k <- ceiling(log(m) / log(2))
  2^min(max(k, -1021), 1022)
}

.t1w_quantile <- function(x, w, p) {
  if (!length(x)) return(NA_real_)
  keep <- w > 0
  if (!any(keep)) return(NA_real_)
  x <- x[keep]
  w <- w[keep]
  ord <- order(x, method = "radix")
  xs <- x[ord]
  ws <- w[ord]
  # Mata sum() over the sorted weights: double, in that order. A total
  # beyond the double range is formed again from rescaled weights
  # (tabtools 2.1.12; 2.1.11 walked against a missing target and returned
  # the first records, "2 (2, 2)" for 1..10 under weights of 8e307).
  total <- .st_sum(ws)
  if (is.na(total)) {
    ws <- ws / .t1w_wscale(ws)
    total <- .st_sum(ws)
  }
  if (is.na(total) || total <= 0) return(NA_real_)
  # Codex audit D1 (H-D1, not replicated): the tolerance's absolute floor of
  # 1e-10 exceeds every step of the walk when the weights are tiny (all
  # 1e-12: the first cumulative weight is "within tolerance" of every
  # target, so median, Q1 and Q3 all became the mean of the two smallest
  # values; Stata 2.1.12 prints "2 (2, 2)" for 1..10). Multiplying every
  # weight by one constant does not change a weighted quantile, so a total
  # below 1 is rescaled by a power of two first (exact), as the overflow
  # branch above does. Totals of 1 and more walk exactly as Stata's.
  if (total < 1) {
    ws <- ws / .t1w_wscale(ws)
    total <- .st_sum(ws)
  }
  target <- p * total
  tol <- 1e-10 * max(1, total)
  running <- 0
  n <- length(xs)
  for (i in seq_len(n)) {
    running <- running + ws[i]
    if (abs(running - target) <= tol) return(if (i < n) (xs[i] + xs[i + 1L]) / 2 else xs[i])
    if (running > target) return(xs[i])
  }
  xs[n]
}

#' Weighted summary of one continuous variable in one column
#'
#' `_desctab_collect.ado:1860-1920`. Observations need a non-missing value
#' and a positive statistic weight (contln: also x > 0). `n` is the display
#' count (records under wt(), frequencies under fweight). Mean `sum(w y) /
#' sum(w)`; `ss` about the mean,
#' `sum(w d^2) - sum(w d)^2 / sum(w)` with d = y - mean (tiny negatives
#' clamped);
#' variance `n / (sum(w) (n - 1)) * ss` under wt() (Stata `[aw=]`), else
#' `ss / (sum(w) - 1)` (`[fw=]`, the unweighted formula when w = 1). The sums
#' are Mata `sum()`s, in double and in data order (.stata_sum(); products
#' formed first, `w :* (y:^2)`), and a negative variance has no square root
#' (a missing SD, printed "."), as in the unweighted engine (Phase 2 review
#' P0-1).
#' @keywords internal
#' @noRd
.t1w_cont_stats <- function(x, w, d, type, kind) {
  ok <- !is.na(x) & w > 0
  if (type == "contln") ok <- ok & x > 0
  none <- list(n = 0, a = NA_real_, b = NA_real_, c = NA_real_)
  if (!any(ok)) return(none)
  x <- x[ok]
  w <- w[ok]
  n <- sum(d[ok])
  if (type == "conts") {
    return(list(n = n, a = .t1w_quantile(x, w, 0.5), b = .t1w_quantile(x, w, 0.25),
                c = .t1w_quantile(x, w, 0.75)))
  }
  y <- if (type == "contln") log(x) else x
  nobs <- length(y)
  # Products as Mata forms them: `y:^2` beyond Stata's range is missing
  # before the weight multiplies it.
  yy <- y * y
  yy[!.st_ok(yy)] <- NA_real_
  sw <- .st_sum(w)
  p1 <- w * y
  p2 <- w * yy
  # Overflow (H10; tabtools 2.1.12, C5): under wt(), a weight sum, a
  # product w * y or w * y^2, or their sum beyond the double range makes
  # Stata divide the weights by a power of two and start again
  # (.t1w_wscale()); a sum that still holds a missing product (a y^2
  # beyond the range) is missing, so
  # the SD is ".", and a mean whose sum overflows is a blank cell. The
  # centered ss keeps the finite quotient of an overflowing square of
  # sum(w d), where Stata 2.5.1 prints "." (.t1_centered_ss()).
  if (kind == "wt" && (is.na(sw) || !all(.st_ok(p1)) || !all(.st_ok(p2)) ||
                       is.na(.st_sum(p1)) || is.na(.st_sum(p2)))) {
    w <- w / .t1w_wscale(w)
    sw <- .st_sum(w)
    p1 <- w * y
    p2 <- w * yy
  }
  if (is.na(sw) || sw <= 0) return(list(n = n, a = NA_real_, b = NA_real_, c = NA_real_))
  sx <- .st_sum(p1)
  if (is.na(sx)) return(list(n = n, a = NA_real_, b = NA_real_, c = NA_real_))
  # Centered (_desctab_collect.ado:1893-1901, tabtools 2.1.15 F05); the
  # raw-moment sum of w y^2 only gates overflow.
  ss <- .t1_centered_ss(w, y, sx / sw, sw, .st_sum(p2))
  v <- NA_real_
  if (kind == "wt") {
    if (nobs > 1) {
      den <- sw * (nobs - 1)
      # When sw * (nobs - 1) overflows, tabtools 2.1.12 forms the variance
      # as (nobsg / (nobsg - 1)) / swg * ss (take_action item 6, fixed;
      # w = (4e307, 1, 1, 1): "1\u00b10", as R always showed).
      v <- if (.st_ok(den)) (nobs / den) * ss else (nobs / (nobs - 1)) / sw * ss
    }
  } else if (sw > 1) {
    v <- ss / (sw - 1)
  }
  sd <- if (!is.na(v) && v >= 0) sqrt(v) else NA_real_
  m <- sx / sw
  if (type == "contln") list(n = n, a = exp(m), b = exp(sd), c = NA_real_)
  else list(n = n, a = m, b = sd, c = NA_real_)
}

#' Effective sample size per column, `(sum w)^2 / sum w^2`
#' (`_desctab_collect.ado`, `_t1tcfc_collect_mata`, tabtools 2.1.12; Mata
#' accumulates both sums record by record in double). `(sum w)^2 / sum w^2`
#' when both sums are in range, `sum w * (sum w / sum w^2)` when only the
#' square overflows, and otherwise the same from weights divided by a power
#' of two (`_t1tcfc_ess()`): weights of 1e160 or 1e200 give the true ESS
#' (2.1.11 printed "ESS=."; take_action items 4-5, fixed).
#' @keywords internal
#' @noRd
.t1w_ess <- function(w, col_masks) {
  vapply(col_masks, function(m) {
    wm <- w[m]
    if (!length(wm)) return(NA_real_)
    s2 <- .st_sum(wm * wm)
    sw <- .st_sum(wm)
    ess <- if (!is.na(s2) && s2 > 0 && !is.na(sw)) .t1_sq_over(sw, s2) else NA_real_
    if (is.na(ess)) {
      ws <- wm / .t1w_wscale(wm)
      s1 <- .st_sum(ws)
      s2 <- .st_sum(ws * ws)
      ess <- if (is.na(s1) || is.na(s2) || s2 <= 0) NA_real_ else .st_num(s1 * (s1 / s2))
    }
    ess
  }, 0)
}

# Row builders --------------------------------------------------------------
#
# Weighted (and small-cell) counterparts of .t1_cont_rows(), .t1_bin_rows(),
# and .t1_cat_rows(); those dispatch here when `o$wx` is set. Each returns
# the same row records plus, on the first record, `sc` (the small-cell
# result for the variable, see .t1_sc_apply()) and on every record `codes`
# (display suppression codes per column).

.t1w_cont_rows <- function(spec, v, gp, o, col_masks) {
  f <- .t1_fmts(spec, o)
  wx <- o$wx
  K <- length(col_masks)
  st <- lapply(col_masks, function(m) .t1w_cont_stats(v[m], wx$w[m], wx$d[m], spec$type, wx$kind))
  nn <- vapply(st, function(s) s$n, 0)
  cells <- vapply(st, function(s) {
    if (is.na(s$a)) return("")
    switch(spec$type,
      contn = paste0(stata_fmt(s$a, f$fmt1), o$sdleft, stata_fmt(s$b, f$fmt2), o$sdright),
      contln = paste0(stata_fmt(s$a, f$fmt1), o$gsdleft, stata_fmt(s$b, f$fmt2), o$gsdright),
      conts = paste0(stata_fmt(s$a, f$fmt1), " (", stata_fmt(s$b, f$fmt2), o$iqrmiddle,
                     stata_fmt(s$c, f$fmt2), ")"))
  }, "")
  lab <- spec$label
  if (o$varlabplus) {
    lab <- paste0(lab, ", ", switch(spec$type,
      contn = paste0("mean", o$sdleft, "SD", o$sdright),
      contln = paste0("geometric mean", o$gsdleft, "GSD", o$gsdright),
      conts = paste0("median (Q1", o$iqrmiddle, "Q3)")))
  }
  rec <- list(label = lab, cells = cells, type = "var", N = nn, has_stats = TRUE, codes = rep(0, K))
  if (!is.null(o$smallcells)) rec <- .t1_sc_cont(rec, nn, gp, o, col_masks)
  list(rec)
}

# Counts behind a categorical or binary variable (_desctab_collect.ado:
# 1561-1635): per level x column, the display count (col 5), the statistic
# weight sum (col 7); per column the display (col 6) and weight (col 8)
# totals over the variable's non-missing records (all records with
# `missing`); per level the weight row total over the groups (col 9).
.t1w_cat_counts <- function(code, levels, gp, o, col_masks, binary) {
  wx <- o$wx
  base <- wx$w > 0
  if (binary || !o$missing) base <- base & !is.na(code)
  K <- length(col_masks)
  L <- length(levels)
  # Mata sum()s in double, in data order; the row total adds the groups in
  # order (_desctab_collect.ado:1599-1620). Under wt(), a weight sum beyond
  # Stata's range sends Stata (tabtools 2.1.12) through a second pass with
  # the weights divided by a power of two taken over the variable's records
  # in every group (C5; 2.1.11 printed the raw record count, "5 ()").
  ws <- wx$w
  for (pass in 1:2) {
    cell_d <- cell_w <- matrix(0, L, K)
    grp_d <- grp_w <- numeric(K)
    for (k in seq_len(K)) {
      m <- base & col_masks[[k]]
      grp_d[k] <- .stata_sum(wx$d[m])
      grp_w[k] <- .stata_sum(ws[m])
      for (li in seq_len(L)) {
        ml <- m & (if (is.na(levels[li])) is.na(code) else (!is.na(code) & code == levels[li]))
        cell_d[li, k] <- .stata_sum(wx$d[ml])
        cell_w[li, k] <- .stata_sum(ws[ml])
      }
    }
    rowden <- apply(cell_w[, seq_len(gp$G), drop = FALSE], 1L, .stata_sum)
    if (wx$kind != "wt" || pass == 2L) break
    if (all(.st_ok(grp_w)) && all(.st_ok(cell_w)) && all(.st_ok(rowden))) break
    in_group <- base & Reduce(`|`, col_masks[seq_len(gp$G)])
    ws <- wx$w / .t1w_wscale(wx$w[in_group])
  }
  list(cell_d = cell_d, cell_w = cell_w, grp_d = grp_d, grp_w = grp_w, rowden = rowden)
}

# Cell text for one level x column (_desctab_collect.ado:1011-1110,
# :1155-1256). Under wt() the count shown is the effective count
# `(sum w_cell / sum w_group) * N_group`.
.t1w_count_text <- function(cn, li, k, f, o, cat_level, catrowperc) {
  cnt <- cn$cell_d[li, k]
  grpN <- cn$grp_d[k]
  num <- cn$cell_w[li, k]
  den <- if (catrowperc) cn$rowden[li] else cn$grp_w[k]
  if (o$wx$kind == "wt") {
    gw <- cn$grp_w[k]
    # A weight sum still beyond the double range after the rescaled pass
    # (.t1w_cat_counts()) leaves the cell blank.
    if (!.st_ok(gw) || !.st_ok(den)) {
      return(list(cnt = NA_real_, den_slash = if (catrowperc) den else grpN, perc = "", cell = ""))
    }
    if (gw > 0) cnt <- (num / gw) * grpN
  }
  # 100 * num / den in Mata's extended range: a weight sum near the double
  # limit must not overflow the percentage (a weight of 8e307 gives 100%).
  pct <- if (den > 0) {
    h <- 100 * num
    if (.st_ok(h)) h / den else 100 * (num / den)
  } else NA_real_
  den_slash <- if (catrowperc) den else grpN
  perc <- .t1_perc(pct, f$pfmt, o, cat_level)
  list(cnt = cnt, den_slash = den_slash, perc = perc,
       cell = .t1_count_cell(cnt, den_slash, perc, o))
}

.t1w_bin_rows <- function(spec, v, gp, o, col_masks) {
  f <- .t1_fmts(spec, o)
  K <- length(col_masks)
  cn <- .t1w_cat_counts(v, 1, gp, o, col_masks, binary = TRUE)
  tx <- lapply(seq_len(K), function(k) .t1w_count_text(cn, 1L, k, f, o, FALSE, FALSE))
  lab <- if (o$varlabplus) paste0(spec$label, ", ", o$percfootnote) else spec$label
  rec <- list(label = lab, cells = vapply(tx, `[[`, "", "cell"), type = "var", N = cn$grp_d,
              has_stats = TRUE, codes = rep(0, K))
  if (!is.null(o$smallcells)) rec <- .t1_sc_catbin(list(rec), cn, tx, gp, o, col_masks, binary = TRUE)[[1]]
  list(rec)
}

.t1w_cat_rows <- function(spec, code, levlab, gp, o, col_masks) {
  f <- .t1_fmts(spec, o)
  K <- length(col_masks)
  levels <- seq_along(levlab)
  miss_level <- o$missing && any(is.na(code[!is.na(gp$gid)]))
  if (miss_level) levels <- c(levels, NA)
  labs <- c(levlab, if (miss_level) "Missing")
  cn <- .t1w_cat_counts(code, levels, gp, o, col_masks, binary = FALSE)
  lab <- if (o$varlabplus) paste0(spec$label, ", ", o$percfootnote2) else spec$label
  out <- list(list(label = lab, cells = rep("", K), type = "cat_header", N = cn$grp_d, has_stats = TRUE,
                   codes = rep(0, K)))
  tx <- vector("list", length(levels))
  for (li in seq_along(levels)) {
    tx[[li]] <- lapply(seq_len(K), function(k) .t1w_count_text(cn, li, k, f, o, TRUE, o$catrowperc))
    out[[li + 1L]] <- list(label = paste0("   ", labs[li]), cells = vapply(tx[[li]], `[[`, "", "cell"),
                           type = "level", N = rep(NA_real_, K), has_stats = FALSE, codes = rep(0, K))
  }
  if (!is.null(o$smallcells)) {
    out <- .t1_sc_catbin(out, cn, tx, gp, o, col_masks, binary = FALSE,
                         missing_level = if (miss_level) length(levels) else 0L)
  }
  out
}

# Tests and SMDs ------------------------------------------------------------

#' Hypothesis test under frequency weights
#'
#' Stata runs `anova`/`regress`/`tab` with `[fweight]` and the rank tests on
#' the expanded data (`_desctab_collect.ado:362-497`). The R result is
#' defined as R's own test on the data expanded by the frequencies, but is
#' computed without expanding (Phase 3 review P2-3: expansion cost ~66 bytes
#' per unit of total weight, per variable): chi-squared and Fisher run on the
#' frequency-weighted table, Welch t / Welch ANOVA use frequency-weighted
#' moments, and the rank tests use frequency-weighted mid-ranks on the unique
#' values (see .t1_ttest_fw() and friends). With every frequency 1 the data
#' already are the expanded data.
#' @keywords internal
#' @noRd
.t1w_test_fw <- function(type, v, g, fw, include_missing, test_args, var = NULL) {
  if (all(fw == 1)) return(.t1_test(type, v, g, include_missing, test_args, var = var))
  .t1_test(type, v, g, include_missing, test_args, var = var, fw = fw)
}

# Largest total frequency a test may expand to (the cases the aggregated
# forms do not cover: an exact or Edgeworth-corrected Wilcoxon, a Wilcoxon
# confidence interval, paired tests). Beyond it the call is refused rather
# than risking an out-of-memory session.
.t1_fw_expand_max <- 1e7

.t1_fw_expand <- function(x, w) {
  n <- sum(w)
  if (n > .t1_fw_expand_max) {
    cli::cli_abort(c("This {.arg fweight} test would expand the data to {format(n, big.mark = ',', scientific = FALSE)} records.",
                     "i" = "Only the exact or Edgeworth-corrected Wilcoxon test, its confidence interval, and paired tests need the expanded data; the limit is {format(.t1_fw_expand_max, big.mark = ',', scientific = FALSE)}.",
                     "i" = "Use {.code nopvalue = TRUE}, or drop the {.arg test_args} that need it."),
                   class = "tabtools_fw_limit", call = NULL)
  }
  rep.int(x, w)
}

# Frequency-weighted mean and variance, laid out as R's mean() (a second
# pass refines the mean, summary.c) and var() (deviations from that mean,
# divided by n - 1), so the moments equal those of the expanded data to
# rounding.
.t1_fw_moments <- function(y, w) {
  n <- sum(w)
  m <- sum(w * y) / n
  if (is.finite(m)) m <- m + sum(w * (y - m)) / n
  c(n = n, mean = m, var = if (n > 1) sum(w * (y - m)^2) / (n - 1) else NA_real_)
}

# Two-sample t test (stats:::t.test.default, two-sided) from weighted moments.
.t1_ttest_fw <- function(x, y, wx, wy, alternative = "two.sided", mu = 0, paired = FALSE,
                         var.equal = FALSE, conf.level = 0.95) {
  if (isTRUE(paired)) {
    return(stats::t.test(.t1_fw_expand(x, wx), .t1_fw_expand(y, wy), alternative = alternative, mu = mu,
                         paired = TRUE, var.equal = var.equal, conf.level = conf.level))
  }
  if (length(mu) != 1 || is.na(mu)) stop("'mu' must be a single number")
  if (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  a <- .t1_fw_moments(x, wx)
  b <- .t1_fw_moments(y, wy)
  nx <- a[["n"]]
  ny <- b[["n"]]
  if (nx < 1 || (!var.equal && nx < 2)) stop("not enough 'x' observations")
  if (ny < 1 || (!var.equal && ny < 2)) stop("not enough 'y' observations")
  if (var.equal && nx + ny < 3) stop("not enough observations")
  mx <- a[["mean"]]
  my <- b[["mean"]]
  vx <- a[["var"]]
  vy <- b[["var"]]
  if (var.equal) {
    df <- nx + ny - 2
    v <- 0
    if (nx > 1) v <- v + (nx - 1) * vx
    if (ny > 1) v <- v + (ny - 1) * vy
    v <- v / df
    stderr <- sqrt(v * (1 / nx + 1 / ny))
  } else {
    stderrx <- sqrt(vx / nx)
    stderry <- sqrt(vy / ny)
    stderr <- sqrt(stderrx^2 + stderry^2)
    df <- stderr^4 / (stderrx^4 / (nx - 1) + stderry^4 / (ny - 1))
  }
  if (!is.na(stderr) && stderr < 10 * .Machine$double.eps * max(abs(mx), abs(my))) {
    stop("data are essentially constant")
  }
  tstat <- (mx - my - mu) / stderr
  list(statistic = c(t = tstat), parameter = c(df = df), p.value = 2 * stats::pt(-abs(tstat), df),
       method = paste(if (!var.equal) "Welch", "Two Sample t-test"))
}

# One-way test (stats::oneway.test) from weighted group moments.
.t1_oneway_fw <- function(y, g, w, var.equal = FALSE) {
  g <- factor(g)
  k <- nlevels(g)
  if (k < 2L) stop("not enough groups")
  mom <- vapply(split(seq_along(y), g), function(i) .t1_fw_moments(y[i], w[i]), numeric(3))
  n.i <- mom["n", ]
  if (any(n.i < 2)) stop("not enough observations")
  m.i <- mom["mean", ]
  v.i <- mom["var", ]
  w.i <- n.i / v.i
  sum.w.i <- sum(w.i)
  tmp <- sum((1 - w.i / sum.w.i)^2 / (n.i - 1)) / (k^2 - 1)
  method <- "One-way analysis of means"
  if (var.equal) {
    n <- sum(n.i)
    ybar <- .t1_fw_moments(y, w)[["mean"]]
    stat <- (sum(n.i * (m.i - ybar)^2) / (k - 1)) / (sum((n.i - 1) * v.i) / (n - k))
    par <- c(k - 1, n - k)
  } else {
    m <- sum(w.i * m.i) / sum.w.i
    stat <- sum(w.i * (m.i - m)^2) / ((k - 1) * (1 + 2 * (k - 2) * tmp))
    par <- c(k - 1, 1 / (3 * tmp))
    method <- paste(method, "(not assuming equal variances)")
  }
  list(statistic = c(F = stat), parameter = c("num df" = par[1], "denom df" = par[2]),
       p.value = stats::pf(stat, par[1], par[2], lower.tail = FALSE), method = method)
}

# Mid-ranks of the expanded data, one per record: the values' ranks with
# ties averaged (rank()'s default), from the frequency of each unique value.
# Returns the rank of each record and the tie counts (table() of the
# expanded values).
.t1_fw_midranks <- function(x, w) {
  u <- sort(unique(x))
  cnt <- as.vector(rowsum(w, match(x, u), reorder = TRUE))
  mid <- cumsum(cnt) - (cnt - 1) / 2
  list(rank = mid[match(x, u)], ties = cnt)
}

# Wilcoxon rank-sum test (stats::wilcox.test, two-sample, normal
# approximation) from weighted mid-ranks. R uses the exact distribution when
# both groups have fewer than 50 observations (or `exact = TRUE`): those
# cases, a numeric `correct` (R >= 4.6 Edgeworth corrections), paired tests,
# and `conf.int = TRUE` run wilcox.test() on the expanded data, capped at
# .t1_fw_expand_max records.
.t1_wilcox_fw <- function(x, y, wx, wy, alternative = "two.sided", mu = 0, paired = FALSE, exact = NULL,
                          correct = TRUE, conf.int = FALSE, digits.rank = Inf, ...) {
  nx <- sum(wx)
  ny <- sum(wy)
  ex <- if (is.null(exact)) nx < 50 && ny < 50 else exact
  if (isTRUE(ex) || isTRUE(paired) || isTRUE(conf.int) || !is.logical(correct)) {
    return(stats::wilcox.test(.t1_fw_expand(x, wx), .t1_fw_expand(y, wy), alternative = alternative, mu = mu,
                              paired = paired, exact = exact, correct = correct, conf.int = conf.int,
                              digits.rank = digits.rank, ...))
  }
  if (length(mu) > 1L || !is.finite(mu)) stop("'mu' must be a single number")
  r <- c(x - mu, y)
  if (is.finite(digits.rank)) r <- signif(r, digits.rank)
  mr <- .t1_fw_midranks(r, c(wx, wy))
  ix <- seq_along(x)
  stat <- sum(wx * mr$rank[ix]) - nx * (nx + 1) / 2
  sigma <- sqrt((nx * ny / 12) * ((nx + ny + 1) - sum(mr$ties^3 - mr$ties) / ((nx + ny) * (nx + ny - 1))))
  z <- stat - nx * ny / 2
  corr <- if (isTRUE(correct)) sign(z) * 0.5 else 0
  z <- (z - corr) / sigma
  list(statistic = c(W = stat), parameter = NULL, p.value = .t1_wilcox_two_sided(z),
       method = paste("Wilcoxon rank sum test", if (isTRUE(correct)) "with continuity correction"))
}

# Two-sided normal p-value exactly as the running R's wilcox.test() forms
# it: R >= 4.6 takes 2 * min(p, 1 - p) with p = pnorm(z); earlier versions
# 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)). They differ only far in
# the upper tail.
.t1_wilcox_two_sided <- function(z) {
  p <- stats::pnorm(z)
  if (exists(".wilcox_test_two_pval_asymp", envir = asNamespace("stats"), inherits = FALSE)) {
    2 * min(p, 1 - p)
  } else {
    2 * min(p, stats::pnorm(z, lower.tail = FALSE))
  }
}

# Kruskal-Wallis test (stats::kruskal.test) from weighted mid-ranks.
.t1_kruskal_fw <- function(x, g, w) {
  g <- factor(g)
  k <- nlevels(g)
  if (k < 2L) stop("all observations are in the same group")
  n <- sum(w)
  if (n < 2L) stop("not enough observations")
  mr <- .t1_fw_midranks(x, w)
  rs <- as.vector(rowsum(w * mr$rank, g, reorder = TRUE))
  ng <- as.vector(rowsum(w, g, reorder = TRUE))
  stat <- sum(rs^2 / ng)
  stat <- (12 * stat / (n * (n + 1)) - 3 * (n + 1)) / (1 - sum(mr$ties^3 - mr$ties) / (n^3 - n))
  list(statistic = c("Kruskal-Wallis chi-squared" = stat), parameter = c(df = k - 1L),
       p.value = stats::pchisq(stat, k - 1L, lower.tail = FALSE), method = "Kruskal-Wallis rank sum test")
}

# The level-by-group table of the expanded data: frequencies summed per cell.
.t1_fw_table <- function(vf, gf, w) {
  tab <- tapply(w, list(vf, gf), sum, default = 0)
  storage.mode(tab) <- "double"
  as.table(tab)
}

#' Weighted standardized mean difference between the first two groups
#'
#' `_desctab_collect.ado:499-617`: continuous variables from `summarize
#' [aw=]` (wt) or `[fw=]` (fweight) means and SDs on the analysis scale (log
#' for contln; conts uses the mean and SD too); binary from weighted
#' proportions; categorical from weight sums per level (Yang and Dalton).
#' Stata keeps the result in a macro, so it is rounded to macro precision.
#' @keywords internal
#' @noRd
.t1w_smd <- function(type, v, g, w, kind, level1 = 1L, level2 = 2L) {
  w <- .t1w_smd_weights(w, g, kind)
  g1 <- !is.na(g) & g == level1
  g2 <- !is.na(g) & g == level2
  wmean_sd <- function(y, ww) {
    n <- length(y)
    if (!n || sum(ww) <= 0) return(c(NA_real_, NA_real_))
    sw <- sum(ww)
    m <- sum(ww * y) / sw
    ss <- sum(ww * (y - m)^2)
    v <- if (kind == "wt") {
      if (n > 1) n / (sw * (n - 1)) * ss else NA_real_
    } else if (sw > 1) ss / (sw - 1) else NA_real_
    c(m, sqrt(v))
  }
  out <- NA_real_
  if (type %in% c("contn", "contln", "conts")) {
    ok <- !is.na(v)
    y <- v
    if (type == "contln") {
      ok <- ok & v > 0
      y <- log(ifelse(ok, v, NA))
    }
    a <- wmean_sd(y[g1 & ok], w[g1 & ok])
    b <- wmean_sd(y[g2 & ok], w[g2 & ok])
    pool <- sqrt((a[2]^2 + b[2]^2) / 2)
    if (!is.na(pool) && is.finite(pool) && pool > 0) out <- (a[1] - b[1]) / pool
  } else if (type %in% c("bin", "bine")) {
    k1 <- g1 & !is.na(v)
    k2 <- g2 & !is.na(v)
    p1 <- if (any(k1)) sum(w[k1] * v[k1]) / sum(w[k1]) else NA_real_
    p2 <- if (any(k2)) sum(w[k2] * v[k2]) / sum(w[k2]) else NA_real_
    den <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
    if (!is.na(den) && den > 0) out <- (p1 - p2) / den
  } else {
    # _desctab_collect.ado:559-564 (2.1.10+): levels observed in the two
    # compared groups only (a third group's level would blank the SMD).
    lv <- sort(unique(v[(g1 | g2) & !is.na(v)]), method = "radix")
    tot1 <- sum(w[g1 & !is.na(v)])
    tot2 <- sum(w[g2 & !is.na(v)])
    if (length(lv) >= 2L && is.finite(tot1) && is.finite(tot2) && tot1 > 0 && tot2 > 0) {
      k <- lv[-length(lv)]
      p1 <- vapply(k, function(l) sum(w[g1 & !is.na(v) & v == l]), 0) / tot1
      p2 <- vapply(k, function(l) sum(w[g2 & !is.na(v) & v == l]), 0) / tot2
      out <- .t1_cat_smd(p1, p2)
    }
  }
  if (is.na(out) || !.st_ok(out)) NA_real_ else stata_macro_num(out)
}

# Probability-weight SMDs are scale invariant. The post-2.5.1 native
# repair (_desctab_collect.ado:353-369, 4eecca4d) uses one common scale
# over the by() sample before any variable/pair subset. Recover before
# sum(w) or sum(w)*(n-1) can overflow; frequency weights retain their scale.
# Use the existing bounded power-of-two helper, including at R's accepted
# upper weight endpoint where the native inline 2^k can itself overflow.
.t1w_smd_weights <- function(w, g, kind) {
  if (kind != "wt") return(w)
  keep <- !is.na(g) & !is.na(w) & w > 0
  if (!any(keep)) return(w)
  mw <- max(w[keep])
  if (mw * sum(keep)^2 >= 1e300) w <- w / .t1w_wscale(w[keep])
  w
}

# Finishing -----------------------------------------------------------------

#' Insert the effective-sample-size row (desctab/_desctab_collect.ado:891-910)
#'
#' The first body row, label "Effective sample size", cells `ESS=` +
#' `nformat` per group and total column; "Suppressed" where the column's
#' sample size carries a small-cell marker.
#' @keywords internal
#' @noRd
.t1_add_ess <- function(tt, ess, o, sample_codes = NULL) {
  kmap <- .t1_col_groups(tt)
  cells <- rep("", ncol(tt$body))
  cells[1] <- "Effective sample size"
  codes <- rep(0, length(ess))
  for (j in which(!is.na(kmap))) {
    k <- kmap[j]
    if (!is.null(sample_codes) && sample_codes[k] > 0) {
      cells[j] <- o$masktext %||% "Suppressed"
      codes[k] <- 3
    } else {
      # "ESS=" + string(ess, nformat): a missing ESS prints "ESS=."
      # (_desctab_collect.ado:911).
      cells[j] <- paste0("ESS=", stata_fmt(ess[k], o$nformat))
    }
  }
  body <- rbind(as.data.frame(as.list(stats::setNames(cells, names(tt$body))), stringsAsFactors = FALSE),
                tt$body)
  # The ESS row keeps every row column; its console key is Stata's
  # factor_sep "ESS" (_desctab_collect.ado:895), and it has no r(table) row.
  ess_row <- tt$rows[1L, , drop = FALSE]
  ess_row[1L, ] <- NA
  ess_row$type <- "ess"
  ess_row$key <- "ESS"
  ess_row$indent <- 0L
  ess_row$dim <- FALSE
  ess_row$suppression <- 0L
  rows <- rbind(ess_row, tt$rows)
  rows$block <- .t1_blocks(rows$key)
  rows$indent <- NULL
  out <- .t1_retable(tt, body, rows)
  out$meta$row_codes <- c(list(codes), tt$meta$row_codes)
  out$meta$linked_cells <- c(list(rep(FALSE, length(ess))), tt$meta$linked_cells)
  out$meta$cellreplace_spec <- c(NA_integer_, tt$meta$cellreplace_spec)
  out
}

# Rebuild a tt_table after editing its body and row metadata.
.t1_retable <- function(tt, body, rows) {
  tt_table(body, tt$header, rows = rows, cols = tt$cols, title = tt$title, footnote = tt$footnote,
           style = tt$style, stored = tt$stored, command = tt$command, meta = tt$meta, notes = tt$notes)
}

# Column -> col_masks index (groups in order, total last) for every group
# or total column of a single-pass table; NA elsewhere.
.t1_col_groups <- function(tt) {
  role <- tt$cols$role
  out <- rep(NA_integer_, length(role))
  g <- which(role == "group")
  out[g] <- seq_along(g)
  out[role == "total"] <- length(g) + 1L
  out
}

# Dapa under weights (desctab.ado:1366-1372): the unweighted sentence with
# "P-values suppressed." is reworded.
.t1_weighted_dapa <- function(dapa, wtcompare, smd) {
  body <- sub("^Data are presented as ", "", dapa)
  if (wtcompare) return(paste0("Crude and weighted data are presented as ", body,
                               " SMD reflects weighted comparison."))
  out <- paste0("Weighted data are presented as ", body)
  if (smd) out <- paste0(out, " SMD reflects weighted comparison.")
  out
}

#' Side-by-side crude and weighted table (wtcompare, desctab.ado:505-643,
#' :890-950)
#'
#' `crude` and `weighted` are finished single-pass tables over the same
#' variables (the weighted one starts with its ESS row). Columns: label,
#' `Crude <group>` ..., `Weighted <group>` ..., then SMD from the weighted
#' pass. The crude ESS cells are blank (the crude pass has no such row).
#' Total columns keep their place inside each block and get no Total
#' borders (desctab.ado:1786-1797 looks for `<by>_T` only).
#' @keywords internal
#' @noRd
.t1_wtcompare_merge <- function(crude, weighted) {
  cr <- which(crude$cols$role %in% c("group", "total"))
  wt <- which(weighted$cols$role %in% c("group", "total"))
  smd <- which(weighted$cols$role == "smd")
  cr_body <- as.matrix(crude$body)[, cr, drop = FALSE]
  cr_body <- rbind(rep("", length(cr)), cr_body)
  body <- cbind(as.matrix(weighted$body)[, 1L], cr_body, as.matrix(weighted$body)[, c(wt, smd), drop = FALSE])
  h1 <- c(weighted$header[[1]]$text[1], paste("Crude", crude$header[[1]]$text[cr]),
          paste("Weighted", weighted$header[[1]]$text[wt]), weighted$header[[1]]$text[smd])
  h2 <- c(crude$header[[2]]$text[1], crude$header[[2]]$text[cr], weighted$header[[2]]$text[wt],
          weighted$header[[2]]$text[smd])
  role <- c("label", rep("group", length(cr) + length(wt)), rep("smd", length(smd)))
  cols <- data.frame(role = role, model = NA_integer_,
                     pass = c(NA, rep("crude", length(cr)), rep("weighted", length(wt)), rep(NA, length(smd))),
                     group = c(NA, .t1_col_groups(crude)[cr], .t1_col_groups(weighted)[wt],
                               rep(NA, length(smd))),
                     stringsAsFactors = FALSE)
  rows <- weighted$rows
  meta <- weighted$meta
  meta$crude_codes <- c(list(rep(0, length(cr))), crude$meta$row_codes)
  meta$crude_cols <- cr
  meta$weighted_cols <- wt
  meta$crude_sample_codes <- crude$meta$sample_codes
  meta$crude_linked_cells <- c(list(rep(FALSE, length(cr))), crude$meta$linked_cells)
  meta$crude_header_linked <- crude$meta$header_linked
  tt_table(body, list(unname(h1), unname(h2)), rows = rows, cols = cols, title = weighted$title,
           footnote = weighted$footnote, style = weighted$style, stored = weighted$stored,
           command = weighted$command, meta = meta, notes = weighted$notes)
}

#' Build the table: one pass, or the crude and weighted passes of wtcompare
#'
#' Weighted display policy (desctab.ado:256-293, :560-643): under `wt()`
#' p-values are suppressed and cells are percent-only unless `wtn` or
#' `percent_n`; under `wtcompare` the crude pass keeps the user's display
#' with `nopvalue` and no SMD, test, or statistic columns, and the weighted
#' pass is the single weighted table. `fweight` changes only the counts,
#' statistics, and tests.
#' @keywords internal
#' @noRd
.t1_run_passes <- function(data, specs, gp, o, style, title, footnote, sheet, labels,
                           wprep, wtcompare, show_wtn) {
  n <- nrow(data)
  pass <- function(o) {
    pf <- .t1_percfootnotes(o)
    o$percfootnote <- pf$foot
    o$percfootnote2 <- pf$foot2
    .t1_finish_pass(.t1_build(data, specs, gp, o, style, title, footnote, sheet, labels), gp, o)
  }
  unit <- if (!is.null(o$smallcells)) .t1_wx("none", n) else NULL
  if (is.null(wprep)) {
    o$wx <- unit
    return(pass(o))
  }
  if (wprep$kind == "fw") {
    o$wx <- .t1_wx("fw", n, wprep$w)
    return(pass(o))
  }
  o_w <- o
  o_w$wx <- .t1_wx("wt", n, wprep$w)
  o_w$nopvalue <- TRUE
  if (!show_wtn) o_w$percent <- TRUE
  if (!wtcompare) {
    tt <- pass(o_w)
    tt$stored$Dapa <- .t1_weighted_dapa(tt$stored$Dapa, FALSE, o$smd)
    return(tt)
  }
  o_c <- o
  o_c$wx <- unit
  o_c$nopvalue <- TRUE
  o_c$smd <- FALSE
  o_c$test <- FALSE
  o_c$statistic <- FALSE
  crude <- pass(o_c)
  weighted <- pass(o_w)
  tt <- .t1_wtcompare_merge(crude, weighted)
  # A variable protected in either pass withholds its SMD (desctab.ado:
  # 790-801, `_sc_anyderived`). Defensive: both passes run the small-cell
  # engine on the same records, counts, and block options, so the crude
  # derived flags always equal the weighted ones and this adds nothing today
  # (Phase 3 review P3-1, equivalent mutant M22); it keeps Stata's rule if
  # the passes ever diverge.
  if (!is.null(o$smallcells)) {
    cd <- c(FALSE, crude$meta$derived_rows)
    smd_col <- which(tt$cols$role == "smd")
    for (i in which(cd)) {
      tt$body[i, smd_col] <- o$masktext %||% "Suppressed"
      tt$rows$smd[i] <- NA_real_
      tr <- tt$rows$table_row[i]
      if (!is.null(tt$stored$table) && "smd" %in% colnames(tt$stored$table) && !is.na(tr)) {
        tt$stored$table[tr, "smd"] <- NA_real_
      }
    }
    tt$meta$derived_rows <- tt$meta$derived_rows | cd
  }
  tt$stored$Dapa <- .t1_weighted_dapa(crude$stored$Dapa, TRUE, o$smd)
  tt
}

# Group index (col_masks order: groups, then total) of each column; NA for
# the label and statistic columns.
.t1_col_k <- function(tt) if (!is.null(tt$cols$group)) tt$cols$group else .t1_col_groups(tt)

#' Console listing widths
#'
#' Stata's `list, noheader` still reserves each column's variable-name
#' width, abbreviated to 8 characters (probed in Stata 17): `smd_str` makes
#' the SMD column 7 wide and `group_T` a small-cell Total column 7 wide
#' (T28, T30l-n). Names: `factor`, `<by>_<code>` / `<by>_T` (`Total`
#' without by(), `Cr_`/`Wt_` under wtcompare), `pvalue`, `test`,
#' `statistic`, `smd_str` (desctab.ado:872-950).
#' @keywords internal
#' @noRd
.t1_console_widths <- function(tt, gp, by, wtcompare) {
  role <- tt$cols$role
  kmap <- .t1_col_k(tt)
  sfx <- c(stata_macro_text(gp$codes), "T")
  nm <- vapply(seq_along(role), function(j) {
    switch(role[j], label = "factor", p = "pvalue", test = "test", statistic = "statistic",
           smd = "smd_str", {
             s <- sfx[kmap[j]]
             if (wtcompare) paste0(if (identical(tt$cols$pass[j], "crude")) "Cr_" else "Wt_", s)
             else if (is.null(by)) "Total" else paste0(by, "_", s)
           })
  }, "")
  w <- pmin(nchar(nm), 8L)
  cw <- tt$cols$console_width
  tt$cols$console_width <- as.integer(pmax(ifelse(is.na(cw), 0L, cw), w))
  tt
}
