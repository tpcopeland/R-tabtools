# Declared display-scale inputs are an escape hatch, not native fit support.
.rt_custom_survival <- function(x, cmd, scale) {
  if (!cmd %in% c("streg", "stintreg", "mestreg")) return(list())
  distribution <- attr(x, "distribution", exact = TRUE)
  metric <- attr(x, "metric", exact = TRUE)
  if (!.rt_literal_scalar(distribution) || !.rt_literal_scalar(metric)) {
    cli::cli_abort("Custom streg/stintreg/mestreg frames require explicit distribution and metric attributes.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  distribution <- tolower(trimws(distribution)); metric <- tolower(trimws(metric))
  allowed <- c("weibull", "exponential", "gompertz", "lognormal", "loglogistic", "generalized_gamma")
  if (!distribution %in% allowed || !metric %in% c("log_time", "log_hazard") ||
      distribution == "gompertz" && metric == "log_time" ||
      distribution %in% c("lognormal", "loglogistic", "generalized_gamma") && metric == "log_hazard") {
    cli::cli_abort("The declared survival distribution and metric are incompatible or unsupported.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  expected <- if (metric == "log_time") "TR" else "HR"
  # Coefficients may explicitly remain on the named link metric; any ratio
  # claim must agree with that metric and may not be inferred from cmd.
  if (!tolower(scale) %in% tolower(c(expected, "Coef.", "Coef", "Coefficient", "Coefficients", "Beta", "b"))) {
    cli::cli_abort("The effect_scale contradicts the declared survival metric.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  list(distribution = distribution, metric = metric, provenance = "declared_custom_frame")
}

.rt_custom_state_check <- function(x, b) {
  state <- if ("status" %in% names(b)) as.character(b$status) else rep(NA_character_, nrow(b))
  if ("status" %in% names(b) && (!is.character(b$status) || anyNA(state) ||
      any(!state %in% c("est", "ref", "omit", "notest", "absent", "empty", "constrained")))) {
    cli::cli_abort("A custom status column must contain explicit canonical analytical states.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  fixed <- state %in% "constrained"
  inactive <- state %in% c("ref", "omit", "notest", "absent", "empty", "constrained")
  for (field in intersect(c("conf.low", "conf.high", "p.value"), names(b))) {
    if (any(inactive & !is.na(b[[field]]))) cli::cli_abort("Non-estimated custom states cannot carry confidence bounds or p-values.", class = "tabtools_error_regtab_metadata", call = NULL)
  }
  if (any(fixed)) {
    if (!all(c("constraint_value", "constraint_source") %in% names(b)) ||
        !is.numeric(b$constraint_value) || !is.character(b$constraint_source) ||
        any(!is.finite(b$constraint_value[fixed])) || anyNA(b$constraint_source[fixed]) ||
        any(!nzchar(b$constraint_source[fixed])) || anyNA(b$estimate[fixed]) ||
        any(b$estimate[fixed] != b$constraint_value[fixed])) {
      cli::cli_abort("Constrained custom rows require exact estimate/constraint_value agreement and nonempty constraint_source evidence.",
                     class = "tabtools_error_regtab_metadata", call = NULL)
    }
  }
  if ("reference_row" %in% names(b) && any(b$reference_row %in% TRUE & !is.na(state) & state != "ref")) {
    cli::cli_abort("reference_row contradicts the declared custom status.", class = "tabtools_error_regtab_metadata", call = NULL)
  }
  invisible(state)
}

.rt_custom_inference_check <- function(fit, rows, need) {
  if (!any(need)) return(invisible(NULL))
  reference <- attr(fit, "inference_reference", exact = TRUE)
  if (!is.null(reference) && !identical(reference, "normal")) {
    cli::cli_abort("inference_reference must be 'normal'; use df or df.error for Student's t inference.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  dfcol <- intersect(c("df", "df.error"), names(rows))
  missing <- if (length(dfcol)) is.na(rows[[dfcol]]) else rep(TRUE, nrow(rows))
  if (any(need & missing) && !identical(reference, "normal")) {
    cli::cli_abort("Deriving custom-frame inference requires df/df.error or explicit inference_reference='normal'.",
                   class = "tabtools_error_regtab_metadata", call = NULL)
  }
  invisible(NULL)
}
