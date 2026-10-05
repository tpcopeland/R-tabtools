# Automatic variable typing for table1_tc (plan task 2.4), a port of
# _tabtools_detect_vartype (_tabtools_common.ado:274-424).

#' Detect a table1_tc variable type the way Stata tabtools does
#'
#' Branches, in Stata's order: string (or R factor) -> `"cat"`; all missing
#' -> `"contn"`; exactly two distinct values -> `"bin"` when they are 0 and 1,
#' else `"cat"`; any value labels attached -> `"cat"`; at most 7 distinct
#' values -> `"cat"`; fewer than 4 values -> `"contn"`; more than 5,000
#' values -> `"conts"` when |skewness| > 1 or |kurtosis - 3| > 2 (population
#' moments, as Stata `summarize, detail`), else `"contn"`; otherwise Stata's
#' Shapiro-Wilk test (`tt_swilk()`) on all values, or on the reproduced
#' 2,000-row subsample (`tt_swilk_subsample()`) when there are 2,001-5,000,
#' with p >= 0.05 -> `"contn"`, p < 0.05 -> `"conts"`, and a failed test ->
#' `"contn"`.
#'
#' R factors have no Stata counterpart; they are typed `"cat"` like strings.
#'
#' @param x A column of the analysis sample.
#' @return One of `"cat"`, `"bin"`, `"contn"`, `"conts"`.
#' @keywords internal
#' @noRd
tt_detect_vartype <- function(x) {
  # _tabtools_common.ado:295-303: strings are categorical.
  if (is.character(x) || is.factor(x)) return("cat")

  v <- as.numeric(unclass(x))
  v <- v[!is.na(v)]
  nuniq <- length(unique(v))

  # :305-312: all missing.
  if (nuniq == 0L) return("contn")

  # :314-324: only a literal 0/1 coding is binary.
  if (nuniq == 2L) return(if (min(v) == 0 && max(v) == 1) "bin" else "cat")

  # :326-334: a value label attached (with more than two levels) -> cat.
  if (tt_has_value_labels(x)) return("cat")

  # :336-343
  if (nuniq <= 7L) return("cat")

  n <- length(v)
  # :349-356. Unreachable in practice (more than 7 distinct values implies
  # n >= 8), kept for fidelity.
  if (n < 4L) return("contn")

  # :360-374: large samples use moments instead of Shapiro-Wilk.
  # Standardised moments and the Shapiro-Wilk W are scale invariant, so the
  # values (and, for the moments, the deviations) are divided by powers of
  # two near their largest magnitude (exact: ordinary data give the
  # unscaled bits) before powers are taken (Codex audit D5: 6,000 values
  # near 1e103 overflowed d^4, and the NaN moments stopped the `if`; 3,000
  # values near 1e100 failed Shapiro-Wilk, typed contn). Stata tabtools
  # scales the same way since Stata-Tools 68c37a90 (task C7).
  p2 <- function(a) 2^min(max(ceiling(log2(max(abs(a)))), -1074), 1023)
  if (n > 5000L) {
    v <- v / p2(v)
    d <- v - mean(v)
    d <- d / p2(d)
    m2 <- mean(d^2)
    skew <- mean(d^3) / m2^1.5
    kurt <- mean(d^4) / m2^2
    return(if (abs(skew) > 1 || abs(kurt - 3) > 2) "conts" else "contn")
  }

  # :376-400: Shapiro-Wilk on the Stata-identical subsample, or on all values.
  xs <- as.numeric(unclass(x))
  sw <- if (n > 2000L) tt_swilk(xs[tt_swilk_subsample(xs)] / p2(v)) else tt_swilk(v / p2(v))
  # :402-418: a failed test defaults to contn; so does a missing p, which
  # Stata's `p >= 0.05` treats as true.
  if (is.na(sw$p) || sw$p >= 0.05) "contn" else "conts"
}

# Whether a column carries value labels (Stata `: value label`), via haven's
# "labels" attribute. Deliberately not value_labels() (R/labels.R): Stata
# reports an attached label even when it defines no codes, so an empty
# "labels" attribute still counts here.
tt_has_value_labels <- function(x) {
  !is.null(attr(x, "labels", exact = TRUE))
}
