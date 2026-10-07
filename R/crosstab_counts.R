# Crosstab sample and category identity, pinned crosstab.ado:220-304.
.xt_abort <- function(message, suffix = "input", parent = NULL) {
  cli::cli_abort(message, class = paste0("tabtools_error_crosstab_", suffix),
                 parent = parent, call = NULL)
}

.xt_flag <- function(x, name) {
  if (!is.logical(x) || !is.null(dim(x)) || length(x) != 1L || is.na(x)) {
    .xt_abort(paste0(name, " must be TRUE or FALSE."))
  }
  x
}

.xt_column <- function(data, name, argument) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !is.null(dim(name)) || sum(names(data) == name, na.rm = TRUE) != 1L) {
    .xt_abort(paste0(argument, " must name exactly one data column."))
  }
  x <- data[[name]]
  if (!is.numeric(x) || is.factor(x) || is.complex(x) || !is.null(dim(x)) ||
      length(x) != nrow(data) || any(is.infinite(x))) {
    .xt_abort(paste0(argument, " must be a numeric category vector with finite codes or missing values."))
  }
  x
}

.xt_categories <- function(x, labels, value_labels = attr(x, "labels", exact = TRUE)) {
  code <- sort(unique(as.numeric(x[!is.na(x)])))
  # levelsof.ado uses %21.0g when every level is integer-valued, including
  # double codes above 2^53; noninteger levels use ordinary macro text.
  text <- if (all(code == floor(code))) stata_fmt(code, "%21.0g") else stata_macro_text(code)
  if (labels && !is.null(value_labels) && (!is.numeric(value_labels) || is.complex(value_labels) ||
      !is.null(dim(value_labels)) || is.null(names(value_labels)) || anyNA(names(value_labels)) ||
      anyDuplicated(value_labels[!is.na(value_labels)]))) {
    .xt_abort("Value labels must name distinct numeric codes with nonmissing text.")
  }
  if (labels && is.numeric(value_labels) && !is.null(names(value_labels))) {
    at <- match(code, value_labels)
    found <- !is.na(at) & nzchar(names(value_labels)[at])
    text[found] <- names(value_labels)[at[found]]
  }
  # Stata extended missing codes sort after ordinary '.', then .a through .z.
  tags <- rep("", length(x))
  if (requireNamespace("haven", quietly = TRUE)) {
    tagged <- haven::is_tagged_na(x)
    if (any(tagged)) tags[tagged] <- haven::na_tag(x[tagged])
  }
  missing_codes <- sort(unique(tags[is.na(x)]))
  missing_text <- ifelse(missing_codes == "", "Missing", paste0("Missing (.", missing_codes, ")"))
  index <- match(x, code)
  index[is.na(x)] <- length(code) + match(tags[is.na(x)], missing_codes)
  list(code = c(code, rep(NA_real_, length(missing_codes))),
       missing_tag = c(rep(NA_character_, length(code)), missing_codes),
       text = c(text, missing_text), index = index)
}

.xt_counts <- function(data, rowvar, colvar, weights, subset, include_missing, labels) {
  if (!is.data.frame(data) || !nrow(data)) .xt_abort("data must be a nonempty data frame.")
  r <- .xt_column(data, rowvar, "rowvar")
  c <- .xt_column(data, colvar, "colvar")
  n <- nrow(data)
  take <- rep(TRUE, n)
  if (!is.null(subset)) {
    if (!is.null(dim(subset))) .xt_abort("subset must be a logical vector or row positions.")
    if (is.logical(subset) && length(subset) == n) {
      take <- !is.na(subset) & subset
    } else if (is.numeric(subset) && !is.complex(subset) && !anyNA(subset) &&
               all(is.finite(subset) & subset == floor(subset) & subset >= 1 & subset <= n)) {
      take <- seq_len(n) %in% subset
    } else .xt_abort("subset must be a logical vector or valid positive row positions.")
  }
  w <- if (is.null(weights)) rep(1, n) else if (is.character(weights) && length(weights) == 1L) {
    .xt_column(data, weights, "weights")
  } else weights
  if (!is.numeric(w) || is.complex(w) || !is.null(dim(w)) || length(w) != n ||
      any(is.infinite(w)) || any(w < 0 | w != floor(w), na.rm = TRUE)) {
    .xt_abort("weights must be nonnegative integer frequency weights, one per record.", "weights")
  }
  valid_weight <- !is.na(w) & w > 0
  observed <- include_missing | (!is.na(r) & !is.na(c))
  used <- take & valid_weight & observed
  if (!any(used)) .xt_abort("No contributing records remain.", "sample")
  rc <- .xt_categories(r[used], labels, attr(r, "labels", exact = TRUE))
  cc <- .xt_categories(c[used], labels, attr(c, "labels", exact = TRUE))
  if (length(rc$text) < 2L || length(cc$text) < 2L) {
    .xt_abort("Both category variables must have at least two observed levels.", "sample")
  }
  counts <- matrix(0, length(rc$text), length(cc$text))
  used_weights <- w[used]
  for (i in seq_len(sum(used))) counts[rc$index[i], cc$index[i]] <-
    counts[rc$index[i], cc$index[i]] + used_weights[i]
  if (!is.finite(sum(counts)) || sum(counts) >= 2^53) {
    .xt_abort("Expanded frequency counts must be exactly representable integers below 2^53.", "weights")
  }
  dimnames(counts) <- list(rc$text, cc$text)
  descriptor <- attr(data[[rowvar]], "label", exact = TRUE)
  if (!is.character(descriptor) || length(descriptor) != 1L || is.na(descriptor) || !nzchar(descriptor)) {
    descriptor <- rowvar
  }
  list(counts = counts, row = rc[c("code", "missing_tag", "text")],
       column = cc[c("code", "missing_tag", "text")], descriptor = descriptor,
       sample = list(input_n = n, eligible_n = sum(take), used_n = sum(used),
                     missing_n = sum(take & valid_weight & !observed),
                     zero_weight_n = sum(take & !is.na(w) & w == 0),
                     excluded_n = n - sum(used), reported_n = sum(counts)),
       weighted = !is.null(weights))
}

# Normalize aliases locally: native crosstab chooses a nonempty xlsx first.
# Unselected destinations are not validated. NULL still records R opt-out.
.xt_alias <- function(xlsx, excel, xlsx_given, excel_given) {
  nonempty <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (xlsx_given && nonempty(xlsx)) return(list(value = xlsx, given = TRUE))
  if (excel_given && nonempty(excel)) return(list(value = excel, given = TRUE))
  if (xlsx_given && !is.null(xlsx) && !identical(xlsx, "")) return(list(value = xlsx, given = TRUE))
  if (excel_given && !is.null(excel) && !identical(excel, "")) return(list(value = excel, given = TRUE))
  list(value = NULL, given = xlsx_given || excel_given)
}
