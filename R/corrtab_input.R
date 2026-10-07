.cor_abort <- function(message, subclass = "input") {
  cli::cli_abort(message, class = c(paste0("tabtools_error_corrtab_", subclass),
                                   "tabtools_error_corrtab"), call = NULL)
}

.cor_flag <- function(x, arg) {
  if (!is.logical(x) || !is.null(dim(x)) || length(x) != 1L || is.na(x)) {
    .cor_abort(paste0(arg, " must be TRUE or FALSE."))
  }
  x
}

.cor_sample <- function(data, vars, subset, labels) {
  if (!is.data.frame(data) || anyDuplicated(names(data))) .cor_abort("data must be a data frame with unique column names.")
  if (!is.character(vars) || !is.null(dim(vars)) || length(vars) < 2L || anyNA(vars) || any(!nzchar(vars)) ||
      anyDuplicated(vars) || any(!vars %in% names(data))) {
    .cor_abort("vars must name at least two distinct existing numeric columns.")
  }
  if (nrow(data) > .Machine$integer.max) .cor_abort("The row count exceeds integer pair-count capacity.", "sample")
  for (nm in vars) {
    v <- data[[nm]]
    if (!is.numeric(v) || is.complex(v) || !is.null(dim(v))) {
      .cor_abort(paste0("Selected column ", nm, " must be a real numeric vector; factors and logical columns do not convert automatically."))
    }
  }
  eligible <- rep(TRUE, nrow(data))
  if (!is.null(subset)) {
    if (!is.logical(subset) || !is.null(dim(subset)) || length(subset) != nrow(data)) {
      .cor_abort("subset must be one logical value per original record.", "sample")
    }
    eligible <- !is.na(subset) & subset
  }
  if (!any(eligible)) .cor_abort("No observations remain after row selection.", "sample")
  selected <- lapply(vars, function(nm) as.double(data[[nm]][eligible]))
  if (any(vapply(selected, function(v) any(is.infinite(v)), logical(1)))) {
    .cor_abort("Selected numeric inputs cannot contain infinite values.")
  }
  display <- vapply(vars, function(nm) {
    lab <- attr(data[[nm]], "label", exact = TRUE)
    if (!is.null(lab) && (!is.character(lab) || length(lab) != 1L || is.na(lab))) {
      .cor_abort(paste0("Variable label for ", nm, " must be one nonmissing string."))
    }
    if (is.null(lab) || identical(lab, "")) nm else lab
  }, "")
  if (!is.null(labels)) {
    if (!is.character(labels) || !is.null(dim(labels)) || is.null(names(labels)) || anyNA(labels) || anyNA(names(labels)) ||
        any(!nzchar(names(labels))) || anyDuplicated(names(labels)) || any(!names(labels) %in% vars)) {
      .cor_abort("labels must be a named character vector mapping exact selected variable names.")
    }
    display[match(names(labels), vars)] <- unname(labels)
  }
  if (anyNA(display) || any(!nzchar(display))) .cor_abort("Variable labels must be nonempty strings.")
  sample <- .tt_sample_population("selection", "corrtab", "table",
    values = list(input_n = nrow(data), eligible_n = sum(eligible)),
    exclusions = .tt_sample_exclusion("input_to_eligible", "outside subset or missing subset selector",
      sum(!eligible), "original records"))
  list(values = selected, vars = vars, labels = unname(display),
       row_ids = which(eligible), sample = sample, input_n = nrow(data), eligible_n = sum(eligible))
}

.cor_stars <- function(star, pvalues) {
  if (!is.null(star) && pvalues) .cor_abort("star cannot be combined with pvalues.", "star")
  if (is.null(star)) return(if (pvalues) numeric() else c(.001, .01, .05))
  if (!is.numeric(star) || is.complex(star) || !is.null(dim(star)) ||
      !length(star) || length(star) > 3L || any(!is.finite(star)) ||
      any(star <= 0 | star >= 1) || anyDuplicated(star)) {
    .cor_abort("star must contain one to three unique finite thresholds strictly between zero and one.", "star")
  }
  sort(as.double(star))
}

# corrtab.ado:54 chooses nonempty xlsx first, then excel. Resolve that
# command-local alias before the canonical session resolver, exactly once.
.cor_xlsx_alias <- function(xlsx, excel, xlsx_given, excel_given) {
  nonempty <- function(x) !is.null(x) && !identical(x, "")
  if (nonempty(xlsx)) return(list(path = xlsx, given = xlsx_given))
  if (nonempty(excel)) return(list(path = excel, given = excel_given))
  list(path = NULL, given = xlsx_given || excel_given)
}
