# Shared Milestone P.2 validation. Decimal-comma formats use the existing
# C-backed formatter with the decimal mark translated only after formatting.
.tt_resolve_numeric_format <- function(cformat = NULL, digits = NULL,
                                       digits_given = FALSE, sep = ", ",
                                       sep_arg = "sep", default_digits = 2L,
                                       max_digits = 10L) {
  if (!is.null(cformat) && digits_given) {
    cli::cli_abort("{.arg cformat} and {.arg digits} may not be combined.",
                   class = "tabtools_error_format_conflict", call = NULL)
  }
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    cli::cli_abort("{.arg {sep_arg}} must be one literal string.",
                   class = "tabtools_error_format_separator", call = NULL)
  }
  if (!nzchar(sep)) sep <- ", "
  decimal_comma <- FALSE
  normalized <- cformat
  if (!is.null(cformat)) {
    if (!is.character(cformat) || length(cformat) != 1L || is.na(cformat)) {
      cli::cli_abort("{.arg cformat} must be one Stata numeric display format.",
                     class = "tabtools_error_format", call = NULL)
    }
    decimal_comma <- grepl("^%-?0?[0-9]+,", cformat)
    normalized <- sub(",", ".", cformat, fixed = TRUE)
    tryCatch(.parse_stata_fmt(normalized), error = function(e) {
      cli::cli_abort("{.arg cformat} must be a supported numeric Stata format (%w.df, %w.dg, or their c suffix).",
                     class = "tabtools_error_format", parent = e, call = NULL)
    })
    if (decimal_comma && grepl(",", sep, fixed = TRUE)) {
      cli::cli_abort("A decimal-comma {.arg cformat} requires a separator without a comma.",
                     class = "tabtools_error_format_separator", call = NULL)
    }
  }
  # Session digits are fallback only: a format never consults that option.
  digits <- if (is.null(cformat)) digits %||% getOption("tabtools.digits") %||% default_digits else
    default_digits
  digits <- .check_int_range(digits, "digits", 0, max_digits)
  list(cformat = cformat, normalized = normalized, decimal_comma = decimal_comma,
       digits = digits, sep = sep)
}

.tt_format_numeric <- function(x, spec) {
  if (is.null(spec$cformat)) {
    return(trimws(stata_fmt(stata_round(x, 10^(-spec$digits)),
                            paste0("%24.", spec$digits, "f"))))
  }
  out <- trimws(stata_fmt(x, spec$normalized))
  if (spec$decimal_comma) out <- chartr(".,", ",.", out)
  out
}
