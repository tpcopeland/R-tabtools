#' Binary outcomes by exposure
#'
#' Displays crude exposed/comparator events and denominators alongside ordered
#' crude or adjusted model ratios. Counts and adjusted complete-case fits have
#' separate sample populations.
#' @param data Explicit data frame, never reconstructed from a model call.
#' @param outcomes Character outcome column names; repeated occurrences retain order.
#' @param exposure One numeric/logical binary exposure column; 1 is exposed.
#' @param models Ordered list of one-sided covariate formulas, or full formulas
#'   whose response matches the outcome. The default `list(~1)` is crude.
#' @param estimator `"modified_poisson"` (Poisson log-link, robust ML variance),
#'   `"logit"` (model-based odds ratio), `"poisson"` (model-based incidence-rate
#'   ratio), or a function accepting `formula` and `data` and returning an ordinary
#'   `glm` retaining `model`, `x`, `y` and original row names.
#' @param estimator_args Named fitter arguments. Built-ins default to
#'   `glm.control(epsilon = 1e-12, maxit = 100)`; an explicit `control` wins.
#'   Built-ins accept `control`,
#'   `start`, `etastart`, `mustart` and `contrasts`; weights/offsets are unsupported.
#' @param vce NULL for the estimator default, or `"model"`, `"robust"`, `"cluster"`.
#' @param cluster Exact cluster column name, required only with cluster variance.
#' @param level Confidence level strictly between zero and one; normal Wald reference.
#' @param eform FALSE; ratios already receive exactly one exponentiation. TRUE
#'   refuses double transformation.
#' @param modellabels One literal label per model specification.
#' @param grouplabels Two literal labels in exposed, comparator order.
#' @param ratiolabel Literal ratio header label; does not change the estimand.
#' @param panels Optional numeric/logical binary sample indicator columns; panels
#'   may overlap and are displayed in supplied order.
#' @param observed Named outcome-to-numeric-indicator mapping. Inclusion is exactly 1.
#' @param obsprefix Alternative prefix for outcome-specific observation indicators.
#' @param subset Nonmissing logical row vector or unique original row indices.
#' @param minevents Minimum crude exposed event count needed to attempt each fit.
#' @param mintext,nonconvtext,failtext,droptext Literal diagnostic text. In failtext,
#'   `#` becomes an actual equivalent native code, or `R` for portable engine
#'   failures whose actual R condition class/message are retained separately.
#' @param cformat,digits,sep Shared numeric format/digits and literal CI separator.
#' @param smallcells,nosmallcells,masktext Primary printed-count masking threshold,
#'   opt-out and literal replacement text. Analytical returns remain raw.
#' @param xlsx,excel Workbook destination aliases. Non-NULL xlsx wins; otherwise
#'   excel is used. Only the selected alias is validated.
#' @param sheet Workbook sheet. Explicit sheet permits ordinary session inheritance;
#'   explicit NULL destinations opt out.
#' @param csv,markdown,mdappend CSV/Markdown destinations and append flag.
#' @param title,footnote Literal title and publication footnote paragraphs.
#' @param font,fontsize,borderstyle,headershade,zebra,headercolor,zebracolor Shared
#'   publication styling. Omitted header shading inherits the session setting.
#' @param open Open the written workbook in an interactive session.
#' @return A `tt_table`, invisibly after writing. `stored$table` is the raw matrix
#'   `n1 e1 n0 e0 b1 lb1 ub1 rc1 ...`; `stored$fits` records one row per fit.
#'   `meta$outtab` retains exact original/base/eligible/complete-case/fitted row
#'   identities, per-fit diagnostics, raw and publication states and companions.
#'   Ratio columns represent specifications, not one fabricated model. Unkeyed
#'   data-frame conversion is supported; keyed model composition is unsupported.
#' @details Binary response Poisson-log means use the risk model described by
#'   Zou (2004), p.703. The default score sandwich uses the separate Stata ML
#'   correction N/(N-1), with normal Wald intervals on the log scale before one
#'   exponentiation. Cluster scores use G/(G-1); this is a software convention,
#'   not a correction attributed to Zou. Plain Poisson and logit use declared
#'   model variance by default. Function fits are verified against the explicit
#'   formula/design/response snapshot and never refitted from mutable calls.
#'
#'   Minimum events uses crude eligible exposed events. Ordinary covariate
#'   missingness defines complete cases; additional estimator row removal is
#'   diagnosed separately. All diagnostic rows still return their count table.
#'   Non-attempted minimum-event cells are `notest`; failed fits are `empty`,
#'   with distinct reasons. Primary masks are applied last: protected count cells
#'   and all linked ratio cells change publication text/state, while raw counts,
#'   fitted ratios and diagnostics remain available. This is printed-count
#'   protection, not removal of analytical information.
#'
#'   Unsupported families, weighted/MI fits, ambiguous contrasts and stale or
#'   substituted model evidence raise classed `tabtools_error_outtab` errors.
#'   R engine errors have native_rc=NA, never an invented Stata code.
#' @references Zou G (2004). A Modified Poisson Regression Approach to Prospective
#'   Studies with Binary Data. American Journal of Epidemiology 159(7):702--706.
#'   doi:10.1093/aje/kwh090.
#' @examples
#' d <- data.frame(exposed = c(rep(1, 10), rep(0, 20)),
#'   event = c(rep(1, 4), rep(0, 6), rep(1, 2), rep(0, 18)))
#' outtab(d, outcomes = "event", exposure = "exposed")
#' outtab(d, outcomes = "event", exposure = "exposed", estimator = "logit")
#' @export
outtab <- function(data, outcomes, exposure, models = list(~1),
                   estimator = "modified_poisson", estimator_args = list(),
                   vce = NULL, cluster = NULL, level = 0.95, eform = FALSE,
                   modellabels = NULL, grouplabels = NULL, ratiolabel = "RR",
                   panels = NULL, observed = NULL, obsprefix = NULL, subset = NULL,
                   minevents = 0L, mintext = "\u2013", nonconvtext = "did not converge",
                   failtext = "failed, r(#)", droptext = "not estimable (sample reduced)",
                   cformat = NULL, digits = NULL, sep = ", ", smallcells = NULL,
                   nosmallcells = FALSE, masktext = NULL, xlsx = NULL, excel = NULL,
                   sheet = "Outcomes", csv = NULL, markdown = NULL, mdappend = FALSE,
                   title = NULL, footnote = NULL, font = NULL, fontsize = NULL,
                   borderstyle = NULL, headershade = FALSE, zebra = FALSE,
                   headercolor = NULL, zebracolor = NULL, open = FALSE) {
  given <- names(as.list(match.call()))[-1L]
  # Command-specific native alias priority, before the shared resolver sees it.
  chosen <- if (!is.null(xlsx)) xlsx else excel
  chosen_given <- any(c("xlsx", "excel") %in% given)
  sinks <- .tt_resolve_sinks(list(xlsx = chosen, csv = csv, markdown = markdown,
    mdappend = mdappend, sheet = sheet, headershade = headershade,
    smallcells = smallcells, nosmallcells = nosmallcells, masktext = masktext),
    list(xlsx = chosen_given, markdown = "markdown" %in% given,
      mdappend = "mdappend" %in% given, sheet = "sheet" %in% given,
      headershade = "headershade" %in% given, smallcells = "smallcells" %in% given,
      masktext = "masktext" %in% given), policy = "sheet", shade = TRUE, mask = "rates")
  numeric <- .tt_resolve_numeric_format(cformat, digits, "digits" %in% given, sep)
  mask <- sinks$mask
  if (is.null(mask)) mask <- .st_resolve_mask(smallcells, "smallcells" %in% given,
    nosmallcells, masktext, "masktext" %in% given, resolved = sinks$values)
  .puttab_check_flag(eform, "eform")
  if (eform) .ot_abort("outtab ratios are already exponentiated; eform would transform twice.", "scale")
  .puttab_check_flag(open, "open")
  if (!is.numeric(level) || is.complex(level) || !is.null(dim(level)) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) .ot_abort("level must lie strictly between zero and one.")
  minevents <- .check_int_range(minevents, "minevents", 0, .Machine$integer.max)
  if (!is.list(models) || !length(models) || any(!vapply(models, inherits, logical(1), "formula"))) .ot_abort("models must be a nonempty ordered list of formulas.")
  if (!is.function(estimator) && (!is.character(estimator) || length(estimator) != 1L ||
      is.na(estimator) || !estimator %in% c("modified_poisson", "logit", "poisson"))) .ot_abort("Unknown estimator capability.", "capability")
  if (!is.list(estimator_args) || (length(estimator_args) && (is.null(names(estimator_args)) ||
      anyNA(names(estimator_args)) || any(!nzchar(names(estimator_args))) || anyDuplicated(names(estimator_args))))) .ot_abort("estimator_args must be a uniquely named list.")
  forbidden <- c("formula", "data", "family", "model", "x", "y", "weights", "offset", "na.action")
  if (any(names(estimator_args) %in% forbidden) || (!is.function(estimator) &&
      any(!names(estimator_args) %in% c("control", "start", "etastart", "mustart", "contrasts")))) .ot_abort("Unsupported or snapshot-overriding estimator argument.", "capability")
  if (is.null(vce)) vce <- if (identical(estimator, "modified_poisson")) "robust" else "model"
  if (!is.character(vce) || length(vce) != 1L || is.na(vce) || !vce %in% c("model", "robust", "cluster")) .ot_abort("Unsupported variance capability.", "capability")
  if ((vce == "cluster") != !is.null(cluster)) .ot_abort("cluster must be supplied exactly with vce='cluster'.", "capability")
  samples <- .ot_samples(data, outcomes, exposure, panels, observed, obsprefix, subset)
  if (!is.null(cluster)) {
    cluster <- .ot_columns(data, cluster, "cluster")
    if (length(cluster) != 1L || anyNA(data[[cluster]][samples$base_ids])) .ot_abort("Cluster identities must be observed on every base-selected row.", "capability")
  }
  for (arg in c("mintext", "nonconvtext", "failtext", "droptext", "ratiolabel")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1L || is.na(value)) .ot_abort(paste0(arg, " must be one literal string."))
  }
  if (is.null(modellabels)) modellabels <- if (length(models) == 1L &&
      identical(paste(deparse(models[[1L]]), collapse = ""), "~1")) "Crude" else paste("Model", seq_along(models))
  if (!is.character(modellabels) || length(modellabels) != length(models) || anyNA(modellabels)) .ot_abort("modellabels must have exactly one literal label per specification.")
  if (is.null(grouplabels)) {
    labels <- attr(data[[exposure]], "labels", exact = TRUE)
    grouplabels <- vapply(c(1, 0), function(value) {
      at <- if (is.numeric(labels)) which(labels == value) else integer()
      if (length(at) == 1L && !is.null(names(labels))) names(labels)[at] else paste(exposure, "=", value)
    }, "")
  }
  if (!is.character(grouplabels) || length(grouplabels) != 2L || anyNA(grouplabels)) .ot_abort("grouplabels must contain exposed/comparator labels in that order.")
  fits <- lapply(samples$blocks, function(block) lapply(models, function(spec)
    .ot_fit(samples, block, spec, estimator, estimator_args, vce, cluster, level, minevents)))
  tt <- .ot_layout(samples, fits, modellabels, grouplabels, ratiolabel,
    numeric, mask, mintext, nonconvtext, failtext, droptext, title, footnote,
    font, fontsize, borderstyle, sinks$values$headershade, zebra, headercolor,
    zebracolor, sheet %||% "Outcomes", minevents)
  .puttab_export(tt, xlsx = sinks$values$xlsx, csv = csv,
    markdown = sinks$values$markdown, mdappend = sinks$values$mdappend,
    sheet = sheet %||% "Outcomes", sheet_given = "sheet" %in% given && !is.null(sheet), open = open)
}
