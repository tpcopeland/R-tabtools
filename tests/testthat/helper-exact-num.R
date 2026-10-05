# Decimal text to the nearest double, on every platform.
#
# R's own parser (as.numeric(), read.csv(), and numeric literals in this
# source) is correctly rounded only where long double is wider than double.
# Without it (macOS arm64, CI 2026-10-01) "1e33" reads as 1e33 plus one ulp
# and 17-digit values can be several ulps off, so tests that feed Stata-probed
# values to the formatter, or compare goldens at zero tolerance, would test a
# different double from the one Stata printed. jsonlite (a testthat
# dependency) parses numbers with the C library's strtod(), which is correctly
# rounded on glibc, macOS and Windows' UCRT.
exact_num <- function(s) {
  if (is.numeric(s)) return(as.double(s))
  s <- trimws(as.character(s))
  out <- suppressWarnings(as.numeric(s))
  dec <- !is.na(s) & grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", s)
  if (any(dec)) {
    j <- sub("^\\+", "", s[dec])
    j <- sub("^(-?)0+(?=[0-9])", "\\1", j, perl = TRUE)  # JSON: no leading zeros
    j <- sub("^(-?)\\.", "\\10.", j)                     # ".5" -> "0.5"
    j <- sub("\\.(?=[eE]|$)", "", j, perl = TRUE)        # "1." -> "1"
    j <- sub("^(-?[0-9]+)$", "\\1e0", j)                 # integers beyond 2^53 parse as doubles
    out[dec] <- jsonlite::parse_json(paste0("[", paste(j, collapse = ","), "]"), simplifyVector = TRUE)
  }
  out
}
