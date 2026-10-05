# Shapiro-Wilk W test, Royston's approximation, computed the way Stata's
# `swilk` computes it, for table1_tc()'s automatic variable typing.
#
# Provenance: written for tabtools (MIT) on 2026-09-28 from the published
# algorithm, replacing an earlier port of Stata's ado code:
# - Royston P (1992). Approximating the Shapiro-Wilk W-test for
#   non-normality. Statistics and Computing 2(3), 117-119.
# - Royston P (1995). Remark AS R94: a remark on algorithm AS 181: the
#   W-test for normality. Applied Statistics 44(4), 547-551. The coefficient
#   polynomials and the normalizing transformation of W below are the
#   constants published there (as listed in the AS R94 Fortran on StatLib,
#   lib.stat.cmu.edu/apstat/R94), and were checked against SciPy's
#   BSD-3-Clause translation (scipy/stats/_ansari_swilk_statistics.pyx).
# - Abramowitz M, Stegun IA (1964). Handbook of Mathematical Functions,
#   formula 4.4.45 (the polynomial arcsin used for the exact n = 3 p-value).
# No code from Stata, R's stats package (GPL) or nortest was used.
#
# Why not stats::shapiro.test(): Stata builds the expected normal scores from
# tie-averaged ranks, R from order-statistic positions, so tie-heavy variables
# disagree (auto `headroom`: Stata p = 0.331, R 0.008; decision D6).
#
# Stata behaviour this function reproduces (documented or observed, and
# pinned by tests/testthat/golden/swilk.csv), none of it part of Royston's
# algorithm:
# 1. Normal scores m_i = qnorm((r_i - 3/8) / (n + 1/4)), with r_i the
#    tie-averaged rank of observation i; their centred sum of squares is the
#    squared norm; the two largest scores give the A1/A2 corrections, and
#    the coefficients are antisymmetric in the ends only (+-A1, +-A2).
# 2. The data, the scores and the coefficients are stored as floats
#    (single precision), which can itself create ties (integers above 2^24).
# 3. Every scalar is kept as macro text (stata_macro_num()), and the results
#    are those rounded numbers.
# 4. n < 3: all results missing. n = 3: fixed coefficients (-1, 0, 1) /
#    sqrt(2), the exact p from a polynomial arcsin with pi/3 rounded to
#    1.047198 (so p can be slightly negative, and z is then missing), and
#    V = (1 - W) / cos^2(pi/12 + 1.047198). n = 4, 5: the A1 correction only.
#    n <= 11: when log(1 - W) >= gamma(n), z = 9.9999 and V is missing
#    (missing W counts as larger than any number); otherwise
#    V = (1 - W) / exp(gamma - exp(-mu)). n > 11: V = (1 - W) / exp(mu).
# 5. Coefficients that come out non-finite (0/0 for constant data) drop from
#    the correlation; with fewer than two usable pairs Stata stops with
#    r(2000); zero variance gives a missing W.

# Royston's polynomial coefficients, lowest order first.
.sw_coef <- list(
  a_n   = c(0, 0.221157, -0.147981, -2.071190, 4.434685, -2.706056),
  a_n1  = c(0, 0.042981, -0.293762, -1.752461, 5.682633, -3.582633),
  gamma = c(-2.273, 0.459),
  mu_s  = c(0.5440, -0.39978, 0.025054, -0.0006714),
  sd_s  = c(1.3822, -0.77857, 0.062767, -0.0020322),
  mu_l  = c(-1.5861, -0.31082, -0.083751, 0.0038915),
  sd_l  = c(-0.4803, -0.082676, 0.0030302),
  asin  = c(1.5707288, -0.2121144, 0.0742610, -0.0187293)
)

# Horner evaluation of sum(cf[k] * x^(k - 1)).
.sw_poly <- function(cf, x) {
  acc <- cf[length(cf)]
  for (k in rev(seq_len(length(cf) - 1L))) acc <- cf[k] + x * acc
  acc
}

# Coefficients a for sorted float data y (Stata behaviour 1-3).
.sw_weights <- function(y) {
  n <- length(y)
  if (n == 3L) return(stata_float(c(-1, 0, 1) * sqrt(0.5)))
  sc <- stata_float(stats::qnorm((rank(y) - 0.375) / (n + 0.25)))
  ss <- stata_macro_num(stats::var(sc) * (n - 1))
  u <- stata_macro_num(1 / sqrt(n))
  top <- sc[n]
  c1 <- stata_macro_num(top / sqrt(ss) + .sw_poly(.sw_coef$a_n, u))
  if (n <= 5L) {
    ends <- 1L
    scale <- stata_macro_num(sqrt((ss - 2 * top^2) / (1 - 2 * c1^2)))
    tail <- c1
  } else {
    ends <- 2L
    nxt <- sc[n - 1L]
    c2 <- stata_macro_num(nxt / sqrt(ss) + .sw_poly(.sw_coef$a_n1, u))
    scale <- stata_macro_num(sqrt((ss - 2 * top^2 - 2 * nxt^2) / (1 - 2 * c1^2 - 2 * c2^2)))
    tail <- c(c2, c1)
  }
  inner <- seq.int(ends + 1L, length.out = max(n - 2L * ends, 0L))
  wt <- numeric(n)
  wt[inner] <- stata_float(sc[inner] / scale)
  hi <- stata_float(tail)
  wt[(n - ends + 1L):n] <- hi
  wt[seq_len(ends)] <- -rev(hi)
  wt
}

# V, z and p for statistic W at sample size n (Stata behaviour 3-4).
.sw_significance <- function(W, n) {
  mac <- stata_macro_num
  if (n == 3L) {
    r <- mac(sqrt(W))
    q <- mac(.sw_poly(.sw_coef$asin, r))
    q <- mac(pi / 2 - q * sqrt(1 - r))
    pi3 <- 1.047198
    p <- mac((6 / pi) * (q - pi3))
    return(list(V = mac((1 - W) / (1 - sin(pi / 12 + pi3)^2)),
                z = mac(-stata_invnorm(p)), p = p))
  }
  lw <- mac(log1p(-W))
  if (n <= 11L) {
    g <- mac(.sw_poly(.sw_coef$gamma, n))
    if (is.na(lw) || lw >= g) {
      y <- 9.9999
      mu <- 0
      sigma <- 1
      V <- NA_real_
    } else {
      y <- mac(-log(g - lw))
      mu <- mac(.sw_poly(.sw_coef$mu_s, n))
      sigma <- mac(exp(.sw_poly(.sw_coef$sd_s, n)))
      V <- mac((1 - W) / exp(g - exp(-mu)))
    }
  } else {
    y <- lw
    ln <- mac(log(n))
    mu <- mac(.sw_poly(.sw_coef$mu_l, ln))
    sigma <- mac(exp(.sw_poly(.sw_coef$sd_l, ln)))
    V <- mac((1 - W) / exp(mu))
  }
  z <- mac((y - mu) / sigma)
  list(V = V, z = z, p = mac(stats::pnorm(-z)))
}

#' Shapiro-Wilk W test as computed by Stata's swilk
#'
#' @param x Numeric vector; missing values are dropped.
#' @return List with `W`, `V`, `z`, `p` (all `NA` when fewer than three
#'   non-missing values), `n`, the number of non-missing values, and `rc`
#'   (0, or 2000 when Stata's swilk would fail with no complete pairs).
#' @keywords internal
#' @noRd
tt_swilk <- function(x) {
  x <- as.numeric(x)
  y <- sort(stata_float(x[!is.na(x)]))
  n <- length(y)
  res <- list(W = NA_real_, V = NA_real_, z = NA_real_, p = NA_real_, n = n, rc = 0L)
  if (n < 3L) return(res)
  wt <- stata_missing(.sw_weights(y))
  use <- !is.na(wt)
  if (sum(use) < 2L) {
    res$rc <- 2000L
    return(res)
  }
  W <- NA_real_
  if (stats::sd(y[use]) > 0 && stats::sd(wt[use]) > 0) {
    W <- stata_macro_num(stats::cor(y[use], wt[use])^2)
  }
  res[c("W", "V", "z", "p")] <- c(list(W), .sw_significance(W, n))
  res
}

# Stata has no infinities or NaN: invalid results (log(0), sqrt(-1), 0/0)
# are missing.
stata_missing <- function(x) {
  x[!is.finite(x)] <- NA_real_
  x
}

# Stata invnormal(): missing outside the open interval (0, 1).
stata_invnorm <- function(p) {
  ok <- !is.na(p) & p > 0 & p < 1
  out <- rep(NA_real_, length(p))
  out[ok] <- stats::qnorm(p[ok])
  out
}

# Round to single precision, as Stata stores a float variable.
stata_float <- function(x) {
  readBin(writeBin(as.double(x), raw(), size = 4L), "double", size = 4L, n = length(x))
}

# The number Stata keeps after `local name = exp`: the value is stored as
# text, fixed notation up to 17 characters (16 significant digits at most,
# trailing zeros dropped), or e-notation with 12 significant digits when
# |x| >= 1e16 or |x| < 1e-5. Probed in Stata 17 on 2026-09-25:
# sqrt(2) * 10^k gives "1.41421356237e-06", ".0000141421356237",
# ".014142135623731", "1.414213562373095", "1414213562373095",
# "1.41421356237e+16".
stata_macro_num <- function(x) {
  out <- stata_missing(as.double(x))
  ok <- !is.na(out) & out != 0
  if (!any(ok)) return(out)
  v <- out[ok]
  e <- floor(log10(abs(v)))
  txt <- ifelse(e >= 16 | e < -5, .tt_fmt_e(v, 11L),
    ifelse(e >= 0, .tt_fmt_f(v, pmax(15 - e, 0)), .tt_fmt_f(v, 16L)))
  out[ok] <- as.numeric(txt)
  out
}
