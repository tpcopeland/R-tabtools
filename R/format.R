# Number formatting shared by every renderer.
#
# Stata's string(x, "%w.df") rounds the exact binary value of x, exact .5
# ties to even (0.45 = 0.45000000000000001 -> "0.5", 0.5 -> "0", 1.5 -> "2").
# glibc's printf does the same, but R's sprintf() on Windows does not (0.45
# -> "0.4", 97907712.5 -> "97907713"; CI, windows-latest, R 4.5), so every
# displayed number goes through .tt_fmt_f() / .tt_fmt_e(), an exact
# binary-to-decimal conversion in src/stata_fmt.c that gives the same text on
# every platform.

# printf("%.df") (mode 0) or printf("%.de") (mode 1, `alt` = the '#' flag)
# text of doubles, from src/stata_fmt.c. `d` is recycled. Non-finite values
# print as sprintf() prints them ("NA", "NaN", "Inf", "-Inf").
.tt_fmt_num <- function(x, d, mode, alt = FALSE) {
  x <- as.double(x)
  out <- .Call(tt_fmt_c, x, as.integer(d), as.integer(mode), isTRUE(alt))
  bad <- is.na(out)
  if (any(bad)) out[bad] <- sprintf("%f", x[bad])
  out
}
.tt_fmt_f <- function(x, d) .tt_fmt_num(x, d, 0L)
.tt_fmt_e <- function(x, d, alt = FALSE) .tt_fmt_num(x, d, 1L, alt)
# Stata's e-notation mantissa: the exact value at 18 decimals (ties to
# even), then rounded half up (away from zero) to d < 18 decimals.
.tt_fmt_e_stata <- function(x, d, alt = FALSE) .tt_fmt_num(x, d, 2L, alt)
# The same rounding in fixed notation, as %w.0g uses it (%f does not).
.tt_fmt_f_stata <- function(x, d) .tt_fmt_num(x, d, 3L)

# format(x, digits = digits, scientific = FALSE, trim = TRUE) of each
# element, platform-independent: fixed notation, at most `digits`
# significant digits after rounding, trailing zeros dropped.
.tt_fmt_sig <- function(x, digits) {
  vapply(as.double(x), function(v) {
    if (!is.finite(v)) return(as.character(v))
    if (v == 0) return("0")
    m <- .tt_fmt_e(v, digits - 1L)
    ex <- as.integer(sub("^.*e", "", m))
    mant <- sub("0+$", "", gsub("[-.]", "", sub("e.*$", "", m)))
    .tt_fmt_f(v, max(nchar(mant) - 1L - ex, 0L))
  }, "")
}

# A confidence level as the Phase 7 commands show it in headers and methods
# sentences: strtrim(string(level, "%21.15g")) (tabtools 2.1.12,
# effecttab.ado:260-263, stratetab.ado:484-486), at most 15 significant
# digits, so 99.9 reads "99.9", not the double's 99.90000000000001. Levels
# lie in [10, 99.99] (Stata's set level range), where %21.15g is fixed
# notation.
.tt_level_text <- function(x) .tt_fmt_sig(x, 15L)

#' Format p-values with the tabtools pdp/highpdp rules
#'
#' Mirrors `desctab.ado` (lines 805-822) and `regtab.ado` (lines 2220-2224):
#' p >= 0.10 uses `highpdp` decimals, p < 0.10 uses `pdp` decimals, values
#' above `1 - 10^-highpdp` (and below 1) print as `">0.99"`, and values below
#' `10^-pdp` print as `"<0.001"`. Missing p-values become `""`.
#'
#' @param p Numeric vector of p-values.
#' @param pdp Decimal places for p < 0.10 (1-10). Default 3.
#' @param highpdp Decimal places for p >= 0.10 (1-10). Default 2.
#' @return Character vector the same length as `p`.
#' @keywords internal
#' @noRd
format_p <- function(p, pdp = 3L, highpdp = 2L) {
  pdp <- .check_dp(pdp, "pdp")
  highpdp <- .check_dp(highpdp, "highpdp")
  out <- rep("", length(p))
  ok <- !is.na(p)
  out[ok] <- .tt_fmt_f(p[ok], highpdp)
  lo <- ok & p < 0.10
  out[lo] <- .tt_fmt_f(p[lo], pdp)
  pmax <- 1 - 10^(-highpdp)
  hi <- ok & p > pmax & p < 1
  out[hi] <- paste0(">", .tt_fmt_f(pmax, highpdp))
  pmin <- 10^(-pdp)
  tiny <- ok & p < pmin
  out[tiny] <- paste0("<", .tt_fmt_f(pmin, pdp))
  out
}

# ---------------------------------------------------------------------------
# Stata display formats (task 1.1)

# Parse a Stata numeric display format: %[-][0]w.d{f|g}[c] or %[-][0]w.de.
# string() returns "" for a format whose decimals d > 0 are not fewer than
# its width w (%5.5g, %3.3f, %05.5f; probed in Stata 17, audit A03,
# 2026-09-29), so such a format is refused.
.parse_stata_fmt <- function(fmt) {
  if (!is.character(fmt) || length(fmt) != 1L || is.na(fmt)) {
    cli::cli_abort("{.arg fmt} must be one Stata numeric format string.",
                   class = "tabtools_error_fmt", call = NULL)
  }
  m <- regmatches(fmt, regexec("^%(-?)(0?)([0-9]+)\\.([0-9]+)([fge])(c?)$", fmt))[[1]]
  if (!length(m)) {
    cli::cli_abort("Unsupported Stata display format {.val {fmt}}.", class = "tabtools_error_fmt", call = NULL)
  }
  sizes <- as.double(m[4:5])
  if (any(!is.finite(sizes) | sizes > .Machine$integer.max)) {
    cli::cli_abort("Unsupported Stata display format {.val {fmt}}.", class = "tabtools_error_fmt", call = NULL)
  }
  p <- list(left = nzchar(m[2]), zero = nzchar(m[3]), w = as.integer(sizes[1]),
            d = as.integer(sizes[2]), type = m[6], comma = nzchar(m[7]))
  # Native string(x, "%9.2ec") is empty: the c suffix is not an e format.
  # The C formatter supports at most 400 decimals. General format with
  # zero decimals requests w-2 significant digits, so it has a tighter
  # width limit. Other widths have a bounded allocation ceiling in R.
  if (p$w < 1L || p$w > 10000L || p$d > 400L ||
      (p$type == "g" && p$d == 0L && p$w > 402L)) {
    cli::cli_abort(c("Unsupported Stata display format {.val {fmt}}.",
                     "i" = "R supports widths 1\u201310000 and decimals 0\u2013400; zero-decimal g formats have width at most 402."),
                   class = "tabtools_error_fmt", call = NULL)
  }
  if (p$type == "e" && p$comma) {
    cli::cli_abort("Exponential Stata formats do not support the {.val c} suffix.",
                   class = "tabtools_error_fmt", call = NULL)
  }
  if (p$d > 0L && p$d >= p$w) {
    cli::cli_abort(c("Unsupported Stata display format {.val {fmt}}.",
                     "i" = "Its decimals must be fewer than its width; Stata's {.fn string} gives empty text for it."),
                   class = "tabtools_error_fmt", call = NULL)
  }
  p
}

# Thousands separators in the integer part of a fixed-point string.
.add_commas <- function(s) {
  neg <- startsWith(s, "-")
  body <- sub("^-", "", s)
  int <- sub("\\..*$", "", body)
  frac <- ifelse(grepl(".", body, fixed = TRUE), sub("^[^.]*", "", body), "")
  int <- vapply(int, function(i) {
    n <- nchar(i)
    if (n <= 3) return(i)
    first <- n %% 3
    parts <- character()
    if (first) parts <- substr(i, 1, first)
    starts <- seq(first + 1L, n, by = 3L)
    paste(c(parts, substring(i, starts, starts + 2L)), collapse = ",")
  }, "", USE.NAMES = FALSE)
  paste0(ifelse(neg, "-", ""), int, frac)
}

# Stata's e-notation fallback for a fixed format that does not fit: the
# unsigned text is at most max(w - 1, 7) characters and has at most d
# mantissa decimals (any number for %w.0f). With a two-digit exponent that
# is min(d, max(w - 7, 1)) decimals, observed in golden/stata_fmt.csv (%9.0f
# -> 1.00e+09, %9.1f -> 1.2e+07, %9.4f -> 1.23e+04, %12.0fc -> 1.00000e+15,
# %2.0f -> 1.0e+08). A three-digit exponent takes one character more, so
# one decimal fewer, down to none, with the point kept: %2.0f -> 2.e+200,
# %9.0f -> 2.0e+200, %9.1f -> 1.2e+100, %12.0fc -> 2.0000e+200 (probed in
# Stata 17 on 22 formats, Milestone H task H10). The decimals follow the
# exponent before rounding: 9.99e99 under %2.0f is 1.0e+100, not 1.e+100
# (review R7, probed). The mantissa never has more than 18 decimals
# (%26.0f to %32.0f all give 9.999999999999999456e+32 for 1e33), and it is
# rounded from Stata's own 18-decimal expansion, half up (away from zero):
# 12345000 under %11.3f is 1.235e+07, -12.5 under %8.7f is -1.3e+01, and
# 1.5e200 (1.499999999999999955e+200 at 18 decimals) is
# 1.49999999999999996e+200 under %25.0f, where a correctly rounded printf
# gives 1.234e+07, -1.2e+01 and ...995e+200. Fixed notation keeps ties to
# even. Probed in Stata 17 (Phase 7a review F9, phase 7b review F5,
# 2026-09-26: 5,768 value-format pairs).
.stata_efmt <- function(x, w, d) {
  room <- max(w - 1L, 7L)
  cap <- if (d == 0L) .Machine$integer.max else d
  s <- .tt_fmt_e_stata(x, min(cap, room - 6L, 18L))
  e3 <- abs(as.integer(sub("^.*e", "", .tt_fmt_e(x, 17L)))) >= 100L
  if (any(e3)) {
    k3 <- min(cap, room - 7L, 18L)
    s[e3] <- .tt_fmt_e_stata(x[e3], max(k3, 0L), alt = k3 <= 0L)
  }
  s
}

.stata_fmt_f <- function(x, p) {
  s <- .tt_fmt_f(x, p$d)
  if (p$comma) {
    sc <- .add_commas(s)
    # The comma form must fit the width; otherwise the plain digits are used
    # (%12.0fc renders 1e9 as "1000000000").
    s <- ifelse(nchar(sc) <= p$w, sc, s)
  }
  # Width is a minimum: overflow switches to e-notation only when the text
  # also needs 9+ characters (%2.0f keeps "12345678", "100000000" -> "1.0e+08"),
  # and never for 0 < |x| < 1 (%8.7f of -0.5 is "-0.5000000", of 0.999999999996
  # "1.0000000"; of 0 it is "0.0e+00"; phase 7b review F4, probed). Both
  # lengths are those of x before rounding (sign, integer digits, point and
  # decimals, no commas), so a rounding that adds a digit keeps the fixed
  # text: %6.5f of -9.999999 is "-10.00000", %3.2f of -9999.999 "-10000.00",
  # %10.0f of 9999999999.5 "10000000000" (audit A12; 175,044 Stata 17
  # probes of %w.df, %w.dfc, %-w.df and %0w.df, no mismatch).
  a <- abs(x)
  ex <- floor(log10(pmax(a, 1)))
  ex <- ex - (10^ex > pmax(a, 1)) + (10^(ex + 1) <= pmax(a, 1))
  len <- ex + 1 + (x < 0) + if (p$d > 0L) p$d + 1L else 0L
  wide <- len > p$w & len >= 9L & !(x != 0 & a < 1)
  s[wide] <- .stata_efmt(x[wide], p$w, p$d)
  s
}

# %w.0g, |x| >= 1: at most w - 2 significant digits in fixed notation,
# e-notation with w - 7 mantissa decimals when the integer part needs more
# digits or the text exceeds w - 1 characters. |x| < 1: e-notation below
# 1e-5; otherwise w - 2 decimals, trailing zeros stripped, unless fewer than
# two significant digits would survive the leading zeros (then e-notation).
# The leading zero is always dropped (.125). Rules matched against 1,338
# Stata 17 %g probes (review, 2026-09-25) and golden/stata_fmt.csv.
# Fewer than two significant digits (w <= 3) put 0 and every |x| >= 1 in
# e-notation (%3.0g: 1 -> 1.0e+00, 0 -> 0.0e+00). Unlike %f, %g rounds the
# 18-decimal mantissa half up, as the e-notation does (%10.0g: 12345678.5 ->
# 12345679, %4.0g: 0.125 -> .13; probed in Stata 17, 2026-09-26).
.stata_fmt_g <- function(x, p) {
  sig <- max(p$w - 2L, 1L)
  vapply(x, function(v) {
    if (v == 0) return(if (sig < 2L) .stata_efmt(v, p$w, 0L) else "0")
    a <- abs(v)
    if (a < 1) {
      if (a < 1e-5) return(.stata_efmt(v, p$w, 0L))
      lz <- -floor(log10(a)) - 1
      if (sig - lz < 2) return(.stata_efmt(v, p$w, 0L))
      s <- .tt_fmt_f_stata(v, sig)
      s <- sub("\\.$", "", sub("0+$", "", s))
      return(sub("^(-?)0\\.", "\\1.", s))
    }
    if (sig < 2L) return(.stata_efmt(v, p$w, 0L))
    ex <- as.integer(sub("^.*e", "", .tt_fmt_e_stata(v, sig - 1L)))
    if (ex >= sig) return(.stata_efmt(v, p$w, 0L))
    s <- .tt_fmt_f_stata(v, max(sig - 1L - ex, 0L))
    if (grepl(".", s, fixed = TRUE)) s <- sub("\\.$", "", sub("0+$", "", s))
    s <- sub("^(-?)0\\.", "\\1.", s)
    if (nchar(sub("^-", "", s)) > p$w - 1L) return(.stata_efmt(v, p$w, 0L))
    s
  }, "")
}

# %w.dg with d > 0: d caps the significant digits, which is at least two
# and at most w - 2 (a width below 4 counts as 4), and a number whose
# integer part has more digits keeps them all (%9.2g: 3.14159 -> 3.1,
# 14.795321 -> 15, 123456.789 -> 123457). As for %w.0g, |x| >= 1 switches
# to e-notation when its integer part needs more than w - 2 digits or its
# text more than w - 1 characters, and |x| < 1 below 1e-5 or when two
# significant digits would not fit. A number shown with no decimals whose
# rounding adds a digit is e-notation with max(1, min(E - 5, w - 7))
# mantissa decimals, E the exponent after rounding (%9.2g: 99.5 -> 1.0e+02,
# 999.5 -> 1.0e+03, 9999999.5 -> 1.00e+07; but 9.995 -> 10; review P2-1,
# 26,625 fresh Stata 17 probes of carries at 10^5..10^13, pinned in
# fixtures/stata_fmt_a03_carry.csv). Other e-notation shows d - 1 mantissa
# decimals (at least one) within the width, as %w.df's fallback does. Rules fitted to 86,275 Stata 17 string() probes (119 formats %1.1g
# to %16.15g, 725 values) with no mismatch (audit A03, 2026-09-29);
# tests/testthat/fixtures/stata_fmt_a03.csv pins a subset.
.stata_fmt_gd <- function(x, p) {
  w <- max(p$w, 4L)
  room <- w - 2L
  sig <- max(min(p$d, room), 2L)
  edec <- max(p$d - 1L, 1L)
  strip <- function(s) sub("^(-?)0\\.", "\\1.", sub("\\.$", "", sub("0+$", "", s)))
  vapply(x, function(v) {
    if (v == 0) return("0")
    a <- abs(v)
    if (a < 1) {
      if (a < 1e-5) return(.stata_efmt(v, w, edec))
      lz <- -floor(log10(a)) - 1
      if (room - lz < 2) return(.stata_efmt(v, w, edec))
      return(strip(.tt_fmt_f_stata(v, min(lz + sig, room))))
    }
    ex <- floor(log10(a))
    if (10^ex > a) ex <- ex - 1
    if (10^(ex + 1) <= a) ex <- ex + 1
    if (ex >= room) return(.stata_efmt(v, w, edec))
    dec <- as.integer(max(sig - 1L - ex, 0L))
    s <- .tt_fmt_f_stata(v, dec)
    if (dec == 0L && nchar(sub("^-", "", s)) > ex + 1L) {
      return(.stata_efmt(v, w, max(1L, min(as.integer(ex) + 1L - 5L, w - 7L))))
    }
    if (dec > 0L) s <- strip(s)
    if (nchar(sub("^-", "", s)) > w - 1L) return(.stata_efmt(v, w, edec))
    s
  }, "")
}

# %w.dgc and %w.0gc (audit A11): the plain %g text with thousands
# separators, or the plain text when they do not fit. For %w.0gc each comma
# takes the place of a significant digit: the text is %(w - k).0g with k
# commas inserted, k counted from the integer digits of x before rounding
# (%6.0gc: 999.999 -> 1,000, 1234.5678 -> 1235; %8.0gc: 1234.5678 ->
# 1,234.6), and the plain %w.0g text when %(w - k).0g is e-notation
# (%10.0gc: 1234568 -> 1234568). For d > 0 the %w.dg text gains commas
# when w is at least the integer digits plus 3, at least d + 3, and at
# least the comma text's length, sign included (%10.1gc: 1234568 ->
# 1,234,568; %11.8gc: -1234567.8 stays -1234567.8). E-notation never has
# commas. Rules fitted to 149,556 Stata 17 string() probes (242 formats
# %1.0gc to %20.19gc, 618 values) with no mismatch;
# tests/testthat/fixtures/stata_fmt_a11.csv pins a subset.
.stata_fmt_gc <- function(x, p) {
  plain_at <- function(v, w) {
    q <- p
    q$w <- w
    if (p$d > 0L) .stata_fmt_gd(v, q) else .stata_fmt_g(v, q)
  }
  plain <- plain_at(x, p$w)
  vapply(seq_along(x), function(i) {
    v <- x[i]
    s <- plain[i]
    if (grepl("e", s, fixed = TRUE)) return(s)
    a <- abs(v)
    nd <- 1
    if (a >= 1) {
      ex <- floor(log10(a))
      if (10^ex > a) ex <- ex - 1
      if (10^(ex + 1) <= a) ex <- ex + 1
      nd <- ex + 1
    }
    k <- as.integer((nd - 1) %/% 3)
    if (p$d == 0L) {
      if (p$w - k < 1L) return(s)
      t <- plain_at(v, p$w - k)
      return(if (grepl("e", t, fixed = TRUE)) s else .add_commas(t))
    }
    sc <- .add_commas(s)
    if (p$w >= nd + 3 && p$w >= p$d + 3 && nchar(sc) <= p$w) sc else s
  }, "")
}

# The 0 flag of %0w.df: zeros between the sign and the digits up to width
# w. It is ignored with commas (%09.1fc), by %g, by e-notation, and by
# missing values (%09.2f: 3.14159 -> 000003.14, -0.5 -> -00000.50, 1e10 ->
# 1.00e+10; probed in Stata 17, audit A03). With `-` the zeros come first
# and any remaining width is trailing blanks (%-09.2f: 1e10 -> "1.00e+10 ").
.stata_zero_pad <- function(s, w) {
  pad <- s != "." & !grepl("e", s, fixed = TRUE) & nchar(s) < w
  if (any(pad)) {
    neg <- startsWith(s[pad], "-")
    s[pad] <- paste0(ifelse(neg, "-", ""), strrep("0", w - nchar(s[pad])), sub("^-", "", s[pad]))
  }
  s
}

#' Render numbers with a Stata display format
#'
#' Reproduces Stata's `string(x, fmt)` for `%w.df`, `%w.dfc`, `%-w.df[c]`,
#' `%0w.df`, `%w.0g`, `%w.dg`, `%w.dgc`, and `%w.de`: no leading padding (zeros for `%0w.df`), trailing padding for left-justified
#' formats, the literal `-0` printed as `"0"` (Stata has no negative zero),
#' and Stata's e-notation fallback. Verified against `golden/stata_fmt.csv`.
#' General formats with 13 or more significant digits use the native
#' formatter's digit generation and can differ in the last digits from
#' Stata's additional rounding; exact parity is not claimed there.
#'
#' @param x Numeric vector.
#' @param fmt A single Stata format string, e.g. `"%5.1f"` or `"%12.0fc"`.
#'   R supports widths 1–10000 and decimals 0–400; zero-decimal g formats
#'   have width at most 402. Unsupported bounds raise `tabtools_error_fmt`.
#' @return Character vector; missing and non-finite values become `"."`
#'   (Stata has no infinities).
#' @keywords internal
#' @noRd
stata_fmt <- function(x, fmt) {
  p <- .parse_stata_fmt(fmt)
  x <- as.numeric(x)
  out <- rep(".", length(x))
  ok <- is.finite(x)
  v <- x[ok]
  v[v == 0] <- 0
  if (length(v)) {
    out[ok] <- if (p$type == "f") {
      .stata_fmt_f(v, p)
    } else if (p$type == "e") {
      # Native e uses the same width-limited 18-decimal mantissa as the
      # existing f/g fallback; zero d fills the available width. Pinned
      # Stata 17 probes include three-digit exponents and decimal comma.
      .stata_efmt(v, p$w, p$d)
    } else if (p$comma) {
      .stata_fmt_gc(v, p)
    } else if (p$d > 0L) {
      .stata_fmt_gd(v, p)
    } else {
      .stata_fmt_g(v, p)
    }
  }
  if (p$zero && p$type == "f" && !p$comma) out <- .stata_zero_pad(out, p$w)
  if (p$left) {
    pad <- pmax(p$w - nchar(out), 0L)
    out <- paste0(out, strrep(" ", pad))
  }
  out
}

#' Text of a number as Stata stores it in a macro
#'
#' What `levelsof` and `local x = exp` produce: fixed notation with up to 16
#' significant digits, trailing zeros stripped and the leading zero dropped
#' (`.1`, `-.05`, `100000`), or e-notation with 12 significant digits when
#' |x| >= 1e16 or |x| < 1e-5 (`1.00000000000e-06`). Probed in Stata 17
#' (2026-09-25). Unlabelled category codes and `by = level` headers use it
#' (`_desctab_collect.ado:265-285`). Numeric twin: `stata_macro_num()`.
#'
#' @param x Numeric vector.
#' @return Character vector; missing values become `"."`.
#' @keywords internal
#' @noRd
stata_macro_text <- function(x) {
  x <- as.double(x)
  out <- rep(".", length(x))
  ok <- is.finite(x)
  out[ok & x == 0] <- "0"
  nz <- ok & x != 0
  if (any(nz)) {
    v <- x[nz]
    e <- floor(log10(abs(v)))
    sci <- e >= 16 | e < -5
    txt <- character(length(v))
    txt[sci] <- .tt_fmt_e(v[sci], 11L)
    fx <- !sci
    if (any(fx)) {
      dec <- ifelse(e[fx] >= 0, pmax(15 - e[fx], 0), 16)
      f <- .tt_fmt_f(v[fx], dec)
      f <- ifelse(grepl(".", f, fixed = TRUE), sub("\\.$", "", sub("0+$", "", f)), f)
      txt[fx] <- sub("^(-?)0\\.", "\\1.", f)
    }
    out[nz] <- txt
  }
  out
}

#' Stata's round(x, u)
#'
#' `floor(x / u + 0.5) * u`: halves go toward +Inf (0.125 -> 0.13, -0.125 ->
#' -0.12). regtab pre-rounds estimates with it (`regtab.ado:2079-2085`) and
#' desctab's `headerperc` uses `round(n / den, 0.001) * 100`
#' (`desctab.ado:1175`).
#'
#' @param x Numeric vector.
#' @param u Rounding unit.
#' @return Numeric vector.
#' @keywords internal
#' @noRd
stata_round <- function(x, u = 1) {
  floor(x / u + 0.5) * u
}

# Formatted p-value text as prose: "p = 0.03", "p < 0.001", "p > 0.99".
.tt_p_prose <- function(ptext, letter = "p") {
  inequality <- substr(ptext, 1L, 1L) %in% c("<", ">")
  prose <- paste0(letter, " = ", ptext)
  prose[inequality] <- paste0(letter, " ", substr(ptext[inequality], 1L, 1L),
                              " ", substring(ptext[inequality], 2L))
  prose
}
