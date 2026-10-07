# modelsummary's modelsummary_list as regtab input (task 7.15): the
# data-frame escape hatch (task 5.8) fed from the list's tidy and glance.

#' Use a modelsummary model list in regtab
#'
#' `tt_from_modelsummary()` turns a `modelsummary_list` (from
#' `modelsummary::modelsummary(model, output = "modelsummary_list")`) into
#' the data frame [regtab()] takes for a model it does not support (see the
#' section on data-frame input in [regtab()]). The list's `tidy` holds the
#' coefficients on the model's own (link) scale, since modelsummary applies
#' its `exponentiate` only when it renders, so the scale must be said:
#' `exponentiate = TRUE` shows `exp()` of the estimates and of Wald
#' intervals built from the standard errors of the coefficients; `FALSE`
#' shows them as they are. A `tidy` already marked as exponentiated needs
#' `se_scale` (`"link"` or `"estimate"`, the scale of its standard errors),
#' which regtab never guesses.
#'
#' The `tidy`'s `response` (or `y.level`, `component`) column becomes the
#' equation (multinomial and multi-part models); rows of random effects
#' (a non-empty `group`) are refused (pass the mixed model itself to
#' [regtab()]); so are ordinal models, whose cutpoints the list types as
#' coefficients (pass the `polr`/`clm` fit itself). The `glance` row gives
#' the statistics `nobs`, `logLik` (with AIC and BIC recomputed from it with
#' Stata's parameter count, the estimated coefficients), `r.squared`,
#' `adj.r.squared` and, for a linear model, `F`; modelsummary's `rmse`
#' (sqrt(RSS / n)) is not Stata's Root MSE and is left out.
#' Intervals, p-values and degrees of freedom are modelsummary's backend's
#' (broom or parameters); intervals it left empty are regtab's Wald ones.
#' @param x A `modelsummary_list`, or a list of them (one data frame each).
#' @param exponentiate `TRUE` or `FALSE`, required: whether the table shows
#'   `exp()` of the coefficients.
#' @param effect_scale The estimate header (default `"exp(b)"` when
#'   exponentiated, else `"Coef."`); `"OR"`, `"HR"`, `"IRR"`, ... for a
#'   named ratio.
#' @param se_scale Only for a `tidy` that is already exponentiated:
#'   `"link"` or `"estimate"`.
#' @param conf.level Confidence level of supplied intervals, when the backend
#'   does not record it. Backend metadata is retained when omitted.
#' @param inference_reference Explicit `"normal"` declaration when derived
#'   inference has no row degrees of freedom. Backend metadata is retained
#'   when omitted; regtab refuses missing evidence.
#' @return A data frame for [regtab()], or a list of them.
#' @seealso [regtab()]
#' @examples
#' # modelsummary is not a dependency of tabtools; its function is looked up
#' # by name when it is installed.
#' pkg <- "modelsummary"
#' if (requireNamespace(pkg, quietly = TRUE)) {
#'   msum <- getExportedValue(pkg, "modelsummary")
#'   fit <- glm(am ~ wt, family = binomial, data = mtcars)
#'   ms <- msum(fit, output = "modelsummary_list")
#'   regtab(tt_from_modelsummary(ms, exponentiate = TRUE, effect_scale = "OR",
#'                              conf.level = .95, inference_reference = "normal"))
#' }
#' @export
tt_from_modelsummary <- function(x, exponentiate, effect_scale = NULL, se_scale = NULL, conf.level = NULL, inference_reference = NULL) {
  if (!inherits(x, "modelsummary_list") && is.list(x) && length(x) &&
      all(vapply(x, inherits, TRUE, "modelsummary_list"))) {
    return(lapply(x, tt_from_modelsummary, exponentiate = exponentiate, effect_scale = effect_scale,
                  se_scale = se_scale, conf.level = conf.level, inference_reference = inference_reference))
  }
  if (!inherits(x, "modelsummary_list")) {
    cli::cli_abort("{.arg x} must be a {.cls modelsummary_list} ({.code modelsummary(model, output = \"modelsummary_list\")}).",
                   call = NULL)
  }
  if (missing(exponentiate) || !is.logical(exponentiate) || length(exponentiate) != 1L || is.na(exponentiate)) {
    cli::cli_abort(c("{.arg exponentiate} must be {.val TRUE} or {.val FALSE}.",
                     "i" = "A {.cls modelsummary_list} keeps the coefficients on the model's scale; say whether the table shows their {.fn exp}."),
                   call = NULL)
  }
  # modelsummary's ordinal_model flag is also set for multinom fits: an
  # ordinal model is known by its class, or by its cutpoints in the "alpha"
  # component (parameters' backend).
  ord_cls <- c("polr", "clm", "clm2", "clmm", "svyolr", "orm", "lrm")
  if (any(as.character(attr(x$tidy, "model_class", exact = TRUE)) %in% ord_cls) ||
      ("component" %in% names(x$tidy) && any(x$tidy$component %in% "alpha"))) {
    cli::cli_abort(c("{.arg x} is an ordinal model: its cutpoints would be read as coefficients.",
                     "i" = "Pass the {.fn MASS::polr} or {.fn ordinal::clm} fit itself to {.fn regtab}."), call = NULL)
  }
  td <- as.data.frame(x$tidy, stringsAsFactors = FALSE)
  if (!all(c("term", "estimate") %in% names(td))) {
    cli::cli_abort("The {.field tidy} of {.arg x} has no {.field term}/{.field estimate} columns.", call = NULL)
  }
  if ("group" %in% names(td) && any(nzchar(trimws(as.character(td$group))) & !is.na(td$group))) {
    cli::cli_abort(c("The {.field tidy} of {.arg x} has random-effect rows (column {.field group}).",
                     "i" = "Pass the mixed model itself to {.fn regtab}."), call = NULL)
  }
  done <- isTRUE(attr(x$tidy, "exponentiate", exact = TRUE))
  if (done && !exponentiate) {
    cli::cli_abort("The {.field tidy} of {.arg x} is already exponentiated; use {.code exponentiate = TRUE}.", call = NULL)
  }
  if (done && is.null(se_scale)) {
    cli::cli_abort(c("The {.field tidy} of {.arg x} is already exponentiated: give {.arg se_scale}.",
                     "i" = "{.val link} when {.field std.error} is that of the coefficient, {.val estimate} when it is that of its {.fn exp} (the delta method, as {.pkg parameters} reports it)."),
                   call = NULL)
  }
  if (!is.null(se_scale) && !done) {
    cli::cli_abort("{.arg se_scale} applies only to a {.field tidy} that is already exponentiated.", call = NULL)
  }
  keep <- intersect(c("term", "estimate", "std.error", "conf.low", "conf.high", "p.value", "df.error"), names(td))
  out <- td[keep]
  eq <- intersect(c("response", "y.level", "component"), names(td))
  if (length(eq)) out$equation <- as.character(td[[eq[1]]])
  if (exponentiate && !done) {
    for (v in intersect(c("estimate", "conf.low", "conf.high"), names(out))) out[[v]] <- exp(out[[v]])
  }
  # Both bounds or neither, as regtab's input requires.
  if (all(c("conf.low", "conf.high") %in% names(out))) {
    one <- is.na(out$conf.low) != is.na(out$conf.high)
    out$conf.low[one] <- NA_real_
    out$conf.high[one] <- NA_real_
    if (all(is.na(out$conf.low))) out$conf.low <- out$conf.high <- NULL
  }
  if (exponentiate) {
    attr(out, "effect_scale") <- effect_scale %||% "exp(b)"
    attr(out, "se_scale") <- if (done) se_scale else "link"
  } else {
    attr(out, "effect_scale") <- effect_scale %||% "Coef."
  }
  cl <- conf.level %||% attr(x$tidy, "conf.level", exact = TRUE) %||% attr(x$tidy, "conf_level", exact = TRUE) %||% attr(x$tidy, "ci", exact = TRUE)
  if (is.numeric(cl) && length(cl) == 1L && "conf.low" %in% names(out)) attr(out, "conf.level") <- cl
  reference <- inference_reference %||% attr(x$tidy, "inference_reference", exact = TRUE)
  if (!is.null(reference)) attr(out, "inference_reference") <- reference
  # Statistics regtab shows as Stata does (stack review 12): AIC and BIC
  # are recomputed from the log-likelihood with Stata's parameter count
  # (the estimated coefficients; R's AIC for lm also counts sigma), and
  # modelsummary's rmse (performance's sqrt(RSS / n)) is not Stata's Root
  # MSE, so it is left out; F is shown for a linear model (regress).
  .rt_glance_check(x$glance, "The {.cls modelsummary_list}")
  gl <- as.list(as.data.frame(x$glance %||% data.frame()))
  map <- c(nobs = "nobs", logLik = "logLik", r.squared = "r.squared", adj.r.squared = "adj.r.squared", F = "F")
  g <- list()
  for (nm in intersect(names(map), names(gl))) {
    v <- suppressWarnings(as.numeric(gl[[nm]])[1])
    if (length(v) && !is.na(v)) g[[map[[nm]]]] <- v
  }
  if (!is.null(g$logLik)) g$df <- sum(!is.na(td$estimate))
  if (length(g)) attr(out, "glance") <- g
  if (identical(as.character(attr(x$tidy, "model_class", exact = TRUE))[1], "lm")) attr(out, "stata_cmd") <- "regress"
  mc <- attr(x$tidy, "model_call", exact = TRUE)
  if (!is.null(mc)) attr(out, "model_id") <- paste(deparse(mc, width.cutoff = 500L), collapse = " ")
  out
}
