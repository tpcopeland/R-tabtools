.ot_abort <- function(message, reason = "input") {
  cli::cli_abort(message, class = c(paste0("tabtools_error_outtab_", reason),
    "tabtools_error_outtab"), call = NULL)
}

.ot_columns <- function(data, value, arg, empty = FALSE) {
  if (!is.character(value) || anyNA(value) || any(!nzchar(value)) ||
      (!empty && !length(value)) || any(!value %in% names(data))) {
    .ot_abort(paste0(arg, " must name existing data columns exactly."))
  }
  value
}

.ot_binary <- function(x, arg) {
  if ((!is.numeric(x) && !is.logical(x)) || is.complex(x) || !is.null(dim(x)) ||
      any(!is.na(x) & !x %in% c(0, 1))) {
    .ot_abort(paste0(arg, " must contain numeric/logical 0, 1 or missing values."))
  }
}

.ot_binary_shape <- function(x, arg, n) {
  if ((!is.numeric(x) && !is.logical(x)) || is.complex(x) ||
      !is.null(dim(x)) || length(x) != n) {
    .ot_abort(paste0(arg, " must be a dimensionless numeric/logical vector with one value per original record."))
  }
}

.ot_samples <- function(data, outcomes, exposure, panels, observed, obsprefix, subset) {
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)) ||
      anyNA(names(data))) .ot_abort("data must be a nonempty data frame with unique column names.")
  outcomes <- .ot_columns(data, outcomes, "outcomes")
  exposure <- .ot_columns(data, exposure, "exposure")
  if (length(exposure) != 1L || exposure %in% outcomes) .ot_abort("exposure must name one distinct column.")
  if (!is.null(panels)) panels <- .ot_columns(data, panels, "panels")
  # Validate original column structure before any row slicing can flatten a
  # matrix. Value-domain checks still apply only to the selected population.
  for (column in unique(c(exposure, outcomes, panels))) {
    .ot_binary_shape(data[[column]], column, nrow(data))
  }
  data <- as.data.frame(data, optional = TRUE)
  selected <- seq_len(nrow(data))
  if (!is.null(subset)) {
    if (!is.null(dim(subset))) .ot_abort("subset must be a dimensionless row selector.")
    if (is.logical(subset) && length(subset) == nrow(data) && !anyNA(subset)) {
      selected <- which(subset)
    } else if (is.numeric(subset) && !is.complex(subset) && !anyNA(subset) &&
               all(is.finite(subset)) && all(subset == floor(subset)) &&
               all(subset >= 1 & subset <= nrow(data)) && !anyDuplicated(subset)) {
      selected <- as.integer(subset)
    } else .ot_abort("subset must be a nonmissing logical row vector or unique original row indices.")
  }
  base <- selected[!is.na(data[[exposure]][selected])]
  if (!length(base)) .ot_abort("The selected exposure population is empty.", "sample")
  .ot_binary(data[[exposure]][base], "exposure")
  for (outcome in outcomes) .ot_binary(data[[outcome]][base], outcome)
  if (!is.null(panels)) {
    for (panel in panels) .ot_binary(data[[panel]][base], panel)
  }
  if (!is.null(observed) && !is.null(obsprefix)) .ot_abort("observed and obsprefix may not be combined.")
  indicators <- rep(NA_character_, length(outcomes))
  if (!is.null(obsprefix)) {
    .tt_check_text_arg(obsprefix, "obsprefix")
    if (length(obsprefix) != 1L) .ot_abort("obsprefix must be one string.")
    indicators <- paste0(obsprefix, outcomes)
  } else if (!is.null(observed)) {
    if (!is.character(observed) || is.null(names(observed)) || anyDuplicated(names(observed)) ||
        !setequal(names(observed), unique(outcomes))) .ot_abort("observed must map each outcome name to one indicator column.")
    indicators <- unname(observed[outcomes])
  }
  if (any(!is.na(indicators))) {
    .ot_columns(data, indicators, "observation indicators")
    for (indicator in indicators) {
      x <- data[[indicator]]
      if (!is.numeric(x) || is.complex(x) || !is.null(dim(x))) .ot_abort("Observation indicators must be numeric; inclusion is exactly == 1.")
    }
  }
  # Copy into ordinary data-frame storage, preserving the original data separately.
  snapshot <- as.data.frame(data, optional = TRUE)
  rownames(snapshot) <- sprintf("ot%010d", seq_len(nrow(snapshot)))
  for (column in unique(c(outcomes, exposure))) {
    if (is.logical(snapshot[[column]])) snapshot[[column]] <- as.numeric(snapshot[[column]])
  }
  blocks <- list()
  for (p in seq_len(if (is.null(panels)) 1L else length(panels))) {
    panel <- if (is.null(panels)) NA_character_ else panels[p]
    for (o in seq_along(outcomes)) {
      rows <- base[!is.na(snapshot[[outcomes[o]]][base])]
      if (!is.na(panel)) rows <- rows[!is.na(snapshot[[panel]][rows]) & snapshot[[panel]][rows] == 1]
      if (!is.na(indicators[o])) rows <- rows[!is.na(snapshot[[indicators[o]]][rows]) & snapshot[[indicators[o]]][rows] == 1]
      g <- snapshot[[exposure]][rows]; y <- snapshot[[outcomes[o]]][rows]
      counts <- c(n1 = sum(g == 1), e1 = sum(y[g == 1]), n0 = sum(g == 0), e0 = sum(y[g == 0]))
      blocks[[length(blocks) + 1L]] <- list(panel = panel, panel_index = p,
        outcome = outcomes[o], outcome_index = o, ids = rows, counts = counts)
    }
  }
  list(data = snapshot, original = data, selected_ids = selected, base_ids = base,
       outcomes = outcomes, exposure = exposure, panels = panels, blocks = blocks)
}

.ot_formula <- function(spec, outcome, exposure, data) {
  if (!inherits(spec, "formula") || !length(spec) %in% c(2L, 3L)) .ot_abort("Each model must be a one-sided covariate formula or full formula.")
  if (length(spec) == 3L) {
    if (!is.symbol(spec[[2L]]) || !identical(as.character(spec[[2L]]), outcome)) .ot_abort("A full model formula response must match its outcome exactly.")
    f <- spec
  } else {
    f <- stats::as.formula(call("~", as.name(outcome), call("+", as.name(exposure), spec[[2L]])), env = environment(spec))
  }
  vars <- all.vars(f)
  if (any(!vars %in% names(data))) .ot_abort("Model variables must come from the explicit data snapshot.", "source")
  terms <- stats::terms(f, data = data)
  if (length(attr(terms, "offset"))) .ot_abort("Offsets require a separately supported estimator capability.", "capability")
  if (!identical(attr(terms, "intercept"), 1L)) .ot_abort("Exposure ratios require an intercept and a comparator group.", "capability")
  factors <- attr(terms, "factors")
  exp_name <- paste(deparse(as.name(exposure)), collapse = "")
  labels <- attr(terms, "term.labels")
  if (sum(labels == exp_name) != 1L || !exposure %in% all.vars(f)) .ot_abort("The exposure must occur as one unambiguous main term.", "capability")
  # The exposure may enter no other term: an interaction (exposed:z) or a
  # term built from it (I(exposed * z)) makes its coefficient conditional.
  others <- labels[labels != exp_name]
  inside <- vapply(others, function(l) exposure %in% all.vars(str2lang(l)), TRUE)
  if (any(inside) || any(colSums(factors != 0) > 1 & factors[exp_name, ] != 0)) {
    .ot_abort("Exposure interactions require a separately supported contrast capability.", "capability")
  }
  f
}

.ot_label <- function(data, name) {
  label <- attr(data[[name]], "label", exact = TRUE)
  if (is.character(label) && length(label) == 1L && !is.na(label)) label else name
}
