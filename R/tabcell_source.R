.tc_est_source <- function(b, ll, ul, se, model, term, matrix, row, cols,
                           contrast, level, level_given) {
  supplied <- c(explicit = any(!vapply(list(b, ll, ul, se), is.null, logical(1))),
                model = !is.null(model), matrix = !is.null(matrix),
                contrast = !is.null(contrast))
  if (sum(supplied) != 1L) {
    .tc_abort("An estimate requires exactly one explicit, fitted, matrix, or contrast source.", "source")
  }
  if ((!is.null(row) || !is.null(cols)) && is.null(matrix)) {
    .tc_abort("Matrix row/column selection requires {.arg matrix}.", "source")
  }
  if (!is.null(term) && is.null(model) && is.null(contrast)) {
    .tc_abort("A term selection requires a fitted or contrast source.", "source")
  }
  source <- list(type = names(supplied)[supplied], term = NA_character_,
    reference = "none", df = NA_real_, level = NA_real_,
    effect_scale = "coefficient", se_scale = NA_character_, inference = NULL,
    native_source = switch(names(supplied)[supplied], explicit = "numbers",
                           model = "e()", matrix = "matrix", contrast = NA_character_),
    native_returns = NULL)
  if (!is.null(matrix)) {
    if (!is.matrix(matrix) || !is.numeric(matrix) || !nrow(matrix) || !ncol(matrix)) {
      .tc_abort("{.arg matrix} must be a nonempty numeric matrix.", "source")
    }
    if (level_given) .tc_abort("Supplied matrix limits cannot be relabeled with a confidence level.", "source")
    select <- function(index, names, limit, argument) {
      if (is.character(index)) {
        if (anyNA(index) || is.null(names) || anyDuplicated(names) || any(!index %in% names)) {
          .tc_abort(paste0(argument, " must select existing unique names."), "source")
        }
        return(match(index, names))
      }
      if (!is.numeric(index) || anyNA(index) || any(!is.finite(index)) ||
          any(index != floor(index)) || any(index < 1 | index > limit)) {
        .tc_abort(paste0(argument, " must select existing integer indices."), "source")
      }
      as.integer(index)
    }
    if (is.null(row) || length(row) != 1L) .tc_abort("Select exactly one matrix row.", "source")
    row_index <- select(row, rownames(matrix), nrow(matrix), "row")
    cols <- cols %||% c("b", "ll", "ul")
    if (length(cols) != 3L || anyDuplicated(cols)) .tc_abort("Select three distinct matrix columns.", "source")
    col_index <- select(cols, colnames(matrix), ncol(matrix), "cols")
    numbers <- as.double(matrix[row_index, col_index])
    source$term <- if (is.null(rownames(matrix))) as.character(row_index) else rownames(matrix)[row_index]
    source$inference <- list(row = row_index, columns = col_index,
                              estimate = numbers[1L], conf.low = numbers[2L], conf.high = numbers[3L])
    return(list(values = list(b = numbers[1L], ll = numbers[2L], ul = numbers[3L]), source = source))
  }
  if (!is.null(model)) {
    if (!is.character(term) || length(term) != 1L || is.na(term) || !nzchar(term)) {
      .tc_abort("A fitted source requires one exact coefficient {.arg term}.", "source")
    }
    .rt_check_model(model, 1L)
    # Use the reviewed model adapters and their own diagnostics/reference
    # distributions. No ambient last fit or caller data is captured here.
    estimates <- tt_wald(model, conf.level = level, ci_method = "wald", vce = "stata")
    where <- which(estimates$term == term)
    if (length(where) != 1L) .tc_abort("The fitted coefficient name is absent or ambiguous.", "source")
    selected <- estimates[where, , drop = FALSE]
    source$term <- term
    source$df <- tt_wald_df(model, vce = "stata")
    if (!is.numeric(source$df) || length(source$df) != 1L || is.na(source$df) || source$df <= 0) {
      .tc_abort("The fit lacks a valid reference distribution.", "source")
    }
    source$reference <- if (is.finite(source$df)) "t" else "normal"
    source$level <- level
    source$se_scale <- "coefficient"
    source$inference <- selected
    values <- list(b = selected$estimate, ll = selected$conf.low, ul = selected$conf.high)
    if (!is.finite(selected$std.error) || selected$std.error <= 0) {
      # Keep original inference as provenance but publish the supplied
      # missing text. This does not guess whether a zero-SE term is a base
      # or a constraint; fit-owned state integration supplies that identity.
      values$ll <- values$ul <- NA_real_
    }
    return(list(values = values, source = source))
  }
  if (!is.null(contrast)) {
    if (is.data.frame(contrast)) {
      if (!nrow(contrast) || anyDuplicated(names(contrast))) .tc_abort("The contrast frame is empty or ambiguous.", "source")
      if (!is.null(term)) {
        if (!is.character(term) || length(term) != 1L || is.na(term) ||
            !"term" %in% names(contrast)) .tc_abort("Select one named contrast term.", "source")
        where <- which(contrast$term == term)
        if (length(where) != 1L) .tc_abort("The contrast term is absent or ambiguous.", "source")
        contrast <- as.list(contrast[where, , drop = FALSE])
      } else {
        if (nrow(contrast) != 1L) .tc_abort("Select one contrast row with {.arg term}.", "source")
        contrast <- as.list(contrast)
      }
    }
    if (!is.list(contrast) || is.null(names(contrast)) || anyDuplicated(names(contrast))) {
      .tc_abort("A contrast must be a distinctly named list or data frame.", "source")
    }
    if (!is.null(term) && !identical(contrast[["term"]], term)) {
      .tc_abort("The supplied contrast does not match {.arg term}.", "source")
    }
    required <- c("estimate", "df", "conf.level", "effect_scale")
    if (any(!required %in% names(contrast))) {
      .tc_abort("A contrast must declare estimate, df, conf.level and effect_scale.", "source")
    }
    original <- contrast
    declared <- contrast[["native_source"]]
    if (!is.null(declared)) {
      if (!is.character(declared) || !is.null(dim(declared)) || length(declared) != 1L ||
          is.na(declared) || !declared %in% c("lincom", "nlcom")) {
        .tc_abort("A contrast native_source must explicitly name lincom or nlcom.", "source")
      }
      source$native_source <- declared
    }
    source$term <- contrast[["term"]] %||% NA_character_
    source$df <- contrast[["df"]]
    source$level <- contrast[["conf.level"]]
    source$effect_scale <- contrast[["effect_scale"]]
    source$se_scale <- contrast[["se_scale"]] %||%
      if (identical(source$effect_scale, "coefficient") && !is.null(contrast[["std.error"]])) "coefficient" else NA_character_
    if (!is.numeric(source$df) || !is.null(dim(source$df)) || length(source$df) != 1L || is.na(source$df) || source$df <= 0 ||
        !is.numeric(source$level) || !is.null(dim(source$level)) || length(source$level) != 1L || !is.finite(source$level) ||
        source$level <= 0 || source$level >= 1 ||
        !is.character(source$effect_scale) || length(source$effect_scale) != 1L ||
        !source$effect_scale %in% c("coefficient", "ratio")) {
      .tc_abort("The contrast's scale, reference distribution, or level is invalid.", "source")
    }
    source$reference <- if (is.finite(source$df)) "t" else "normal"
    source$inference <- original
    b <- contrast[["estimate"]]
    ll <- contrast[["conf.low"]]
    ul <- contrast[["conf.high"]]
    se <- contrast[["std.error"]]
    for (value in list(b, ll, ul, se)) {
      if (!is.null(value) && (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L)) {
        .tc_abort("A selected contrast must have numeric scalar estimates, limits and SE.", "source")
      }
    }
    if (!is.null(se) && (is.nan(se) || is.infinite(se) || (!is.na(se) && se < 0))) {
      .tc_abort("A contrast standard error must be nonnegative and finite or NA.", "source")
    }
    if (!is.null(contrast[["se_scale"]]) &&
        (!is.character(source$se_scale) || length(source$se_scale) != 1L || is.na(source$se_scale) ||
         !source$se_scale %in% c("coefficient", "log"))) {
      .tc_abort("A contrast SE scale must be declared as coefficient or log.", "source")
    }
    if (xor(is.null(ll), is.null(ul))) .tc_abort("Supply both contrast limits together.", "source")
    if (!is.null(declared)) source$native_returns <- .tc_contrast_native(original, source)
    relevel <- level_given && !identical(as.double(level), as.double(source$level))
    if (is.null(ll) || relevel) {
      if (!is.numeric(se) || length(se) != 1L || (!is.na(se) && se < 0)) {
        .tc_abort("Recomputed contrast inference requires a nonnegative standard error.", "source")
      }
      if (identical(source$effect_scale, "ratio")) {
        if (!identical(source$se_scale, "log") || !is.numeric(b) || length(b) != 1L ||
            !is.finite(b) || b <= 0) {
          .tc_abort("A ratio interval requires a positive ratio and a declared log-scale SE.", "source")
        }
        coefficient <- log(b)
      } else {
        if (!identical(source$se_scale, "coefficient")) .tc_abort("A coefficient contrast requires a coefficient-scale SE.", "source")
        coefficient <- b
      }
      target <- if (level_given) level else source$level
      q <- if (is.finite(source$df)) stats::qt((1 + target) / 2, source$df) else stats::qnorm((1 + target) / 2)
      ll <- coefficient - q * se
      ul <- coefficient + q * se
      if (identical(source$effect_scale, "ratio")) {
        ll <- exp(ll)
        ul <- exp(ul)
      }
      if (is.finite(coefficient) && is.finite(se) && (!is.finite(ll) || !is.finite(ul))) {
        .tc_abort("The recomputed contrast interval overflowed.", "interval")
      }
      source$level <- target
    }
    if (any(vapply(list(b, ll, ul), length, integer(1)) != 1L)) {
      .tc_abort("A selected contrast must have scalar estimates and limits.", "source")
    }
    # Native tabcell's nlcom branch withholds inference when r(V) <= 0
    # (tabcell.ado 253-256), while retaining the finite point estimate.
    if (identical(source$native_source, "nlcom") && (is.na(se) || se <= 0)) {
      ll <- ul <- NA_real_
    }
    return(list(values = list(b = b, ll = ll, ul = ul), source = source))
  }
  if (is.null(b) || xor(is.null(ll), is.null(ul)) ||
      (!is.null(se) && !is.null(ll)) || (is.null(se) && is.null(ll))) {
    .tc_abort("Supply {.arg b} with both limits or with {.arg se}, exclusively.", "source")
  }
  if (!is.null(ll)) {
    if (level_given) .tc_abort("Supplied explicit limits cannot be relabeled with a confidence level.", "source")
    source$inference <- .tc_recycle(list(b = b, ll = ll, ul = ul))
    return(list(values = list(b = b, ll = ll, ul = ul), source = source))
  }
  inputs <- .tc_recycle(list(b = b, se = se))
  original_inputs <- inputs
  # Native tabcell est b()/se() treats nonpositive SE as a non-estimable
  # source. Missing text may replace that cell; a zero-width CI is not
  # synthesized from a failed/base result.
  inputs$se[is.finite(inputs$se) & inputs$se <= 0] <- NA_real_
  q <- stats::qnorm((1 + level) / 2)
  values <- list(b = inputs$b, ll = inputs$b - q * inputs$se, ul = inputs$b + q * inputs$se)
  finite_inputs <- is.finite(inputs$b) & is.finite(inputs$se)
  if (any(finite_inputs & (!is.finite(values$ll) | !is.finite(values$ul)))) {
    .tc_abort("The normal interval overflowed.", "interval")
  }
  source$reference <- "normal"
  source$df <- Inf
  source$level <- level
  source$se_scale <- "coefficient"
  source$inference <- as.data.frame(original_inputs)
  list(values = values, source = source)
}

# The declared native subtype owns this projection. Original inference is
# captured before publication releveling, exponentiation or scaling; native
# lincom fields are never borrowed from a QA oracle or a caller's last fit.
.tc_contrast_native <- function(contrast, source) {
  b <- contrast[["estimate"]]
  se <- contrast[["std.error"]]
  if (!is.numeric(b) || !is.null(dim(b)) || length(b) != 1L ||
      is.null(se) || !is.numeric(se) || !is.null(dim(se)) || length(se) != 1L ||
      is.nan(se) || is.infinite(se) || (!is.na(se) && se < 0) ||
      is.nan(b) || is.infinite(b)) {
    .tc_abort("A declared native contrast requires scalar finite-or-NA estimate and nonnegative SE.", "source")
  }
  ratio <- identical(source$effect_scale, "ratio")
  if ((ratio && (!identical(source$se_scale, "log") || (!is.na(b) && b <= 0))) ||
      (!ratio && !identical(source$se_scale, "coefficient"))) {
    .tc_abort("A declared native contrast requires an SE on its coefficient or log scale.", "source")
  }
  if (identical(source$native_source, "nlcom") &&
      (ratio || !identical(source$reference, "normal"))) {
    .tc_abort("Declared nlcom contrasts require coefficient-scale normal inference (df = Inf).", "source")
  }
  coefficient <- if (ratio) log(b) else b
  statistic <- if (is.na(coefficient) || is.na(se) || se == 0) NA_real_ else coefficient / se
  if (is.infinite(statistic)) .tc_abort("The contrast statistic overflowed.", "interval")
  p <- if (source$reference == "t") 2 * stats::pt(-abs(statistic), source$df) else
    2 * stats::pnorm(-abs(statistic))
  # Optional supplied statistics are original source evidence, not overrides
  # of the declared reference distribution. Preserve them only when coherent.
  for (field in c("statistic", "p.value")) {
    value <- contrast[[field]]
    if (is.null(value)) next
    expected <- if (field == "statistic") statistic else p
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != 1L ||
        is.nan(value) || is.infinite(value) ||
        (field == "p.value" && !is.na(value) && (value < 0 || value > 1)) ||
        !isTRUE(all.equal(as.double(value), as.double(expected), tolerance = 1e-12))) {
      .tc_abort("Supplied contrast statistic or p-value contradicts its declared inference.", "source")
    }
    if (field == "statistic") statistic <- value else p <- value
  }
  q <- if (source$reference == "t") stats::qt((1 + source$level) / 2, source$df) else
    stats::qnorm((1 + source$level) / 2)
  limits <- c(coefficient - q * se, coefficient + q * se)
  if (ratio) limits <- exp(limits)
  if (identical(source$native_source, "nlcom") && (is.na(se) || se <= 0)) {
    limits[] <- NA_real_
  }
  ll <- contrast[["conf.low"]];ul <- contrast[["conf.high"]]
  if (!is.null(ll)) {
    if (any(is.nan(c(ll, ul)) | is.infinite(c(ll, ul))) ||
        (!is.na(ll) && !is.na(ul) && (ll > ul || (!is.na(b) && (b < ll || b > ul))))) {
      .tc_abort("Original contrast limits are invalid.", "source")
    }
    if (!isTRUE(all.equal(as.double(c(ll, ul)), as.double(limits), tolerance = 1e-12))) {
      .tc_abort("Declared native contrast limits must agree with its reference, scale and SE.", "source")
    }
    limits <- c(ll, ul)
  }
  if (any(is.infinite(limits)) || (ratio && is.finite(b) && is.finite(se) && !is.finite(b * se))) {
    .tc_abort("Original contrast inference overflowed.", "interval")
  }
  if (identical(source$native_source, "nlcom")) return(NULL)
  out <- list(lincom_estimate = b, se = if (ratio) b * se else se,
    lincom_lb = limits[1L], lincom_ub = limits[2L], lincom_level = source$level * 100)
  out[[if (source$reference == "t") "t" else "z"]] <- statistic
  out$p <- p
  if (source$reference == "t") out$df <- source$df
  out
}
