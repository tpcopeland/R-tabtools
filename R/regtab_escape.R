# regtab escape hatch (plan task 5.8): a data frame shaped like
# broom.helpers::tidy_plus_plus() output stands in for a fitted model whose
# class regtab does not support (golden R25t: Stata's churdle, which has no
# R equivalent). Milestone H task H1 (external review F01, F02, F32; user
# items (b), (c)): the whole schema is validated before anything is
# converted, the scale of `std.error` must be declared before a ratio row's
# p-value or interval is derived from it, `df`/`df.error` give t
# statistics, supplied intervals fix the confidence level, and factor
# levels get Stata keys so a data frame and a fitted model share rows.

#' @rdname regtab
#' @section Data-frame input:
#' A model regtab does not support can be passed as a data frame shaped
#' like [broom.helpers::tidy_plus_plus()] output, one row per coefficient:
#' * required: `term` (character, no missing or repeated values within an
#'   equation) and `estimate` (numeric), the estimate already on the
#'   display scale (use `exponentiate = TRUE` for ratios; nothing is
#'   transformed). A missing `estimate` is an omitted coefficient;
#' * optional statistics, all numeric and never `NaN`: `std.error`
#'   (positive and finite), `conf.low` and `conf.high` (both or neither,
#'   `conf.low <= conf.high`), `p.value` (in \[0, 1\]), and `df` or
#'   `df.error` (positive, `Inf` for the normal distribution). An infinite
#'   estimate or bound (`exp()` of a huge coefficient) prints blank, as for
#'   a fitted model. Factors and character columns are refused rather than
#'   converted: `as.numeric()` of a factor gives its level codes.
#'   Supplied intervals and p-values are shown as they are. A row with
#'   neither bound gets a Wald interval from `std.error`, and, when the
#'   data frame has no `p.value` column at all, a Wald p-value (a supplied
#'   `p.value` column is kept as it is, missing entries included). These
#'   use Student's t with the row's `df` (or `df.error`) when given, else
#'   the normal distribution;
#' * the scale of `std.error`: on a ratio scale (OR, HR, IRR, RRR, SHR, TR)
#'   a derived interval or p-value needs `attr(x, "se_scale")`: `"link"`
#'   when `std.error` is the standard error of log(`estimate`), as
#'   [broom.helpers::tidy_plus_plus()] with `exponentiate = TRUE` returns it
#'   (z = log(estimate) / std.error, interval exp(log(estimate) +/- q
#'   std.error)), or `"estimate"` when it is on the ratio's own scale (z =
#'   (estimate - 1) / std.error, interval estimate +/- q std.error).
#'   Without it such rows are refused, by name, rather than guessed. On
#'   the `Coef.` scale (and for ancillary rows) `std.error` is on the scale
#'   of `estimate`. Ratio estimates must be positive;
#' * the effect scale: `attr(x, "effect_scale")` (the header; a ratio
#'   label, `OR`, `HR`, `RR`, `IRR`, `RRR`, `SHR`, `TR`, `PR`, `GMR`,
#'   `exp(b)` or its spelled-out name, has null 1, and a difference label,
#'   `Coef.`, `Beta`, `RD`, `MD`, ..., null 0; any other label needs
#'   `attr(x, "null_value")`, 0 or 1, whenever the null is used by a
#'   derived statistic or `dimnonsig`), else `attr(x, "stata_cmd")`, else
#'   broom.helpers' own record that it exponentiated the estimates
#'   (`attr(x, "exponentiate")`, with its `coefficients_label`, e.g. `OR`,
#'   as the header), else `Coef.`. A broom `statistic` column that shows
#'   the estimates were exponentiated (log(estimate) / std.error, as
#'   `broom::tidy(exponentiate = TRUE)` returns it) on a frame taken as
#'   coefficients is refused, and so is a declared `se_scale = "link"` it
#'   contradicts, or any `se_scale` on a frame without ratio rows;
#' * the confidence level of supplied intervals: `attr(x, "conf.level")`
#'   (which [broom.helpers::tidy_plus_plus()] sets) or a constant
#'   `conf.level` column (a proportion or a percentage), else 95%. With
#'   `level` left at its default, regtab uses that level (the data frames
#'   must agree); an explicit `level` that differs is refused: regtab
#'   cannot recompute supplied intervals. `vce` must be `"stata"` for a
#'   data frame, which carries its own standard errors;
#' * other tools: `parameters::model_parameters(exponentiate = TRUE)`
#'   reports the standard error of the ratio itself (delta method), so it
#'   is `se_scale = "estimate"`, unlike broom (`"link"`); rename its
#'   columns first (`Parameter` to `term`, `Coefficient` to `estimate`,
#'   `SE` to `std.error`, `CI_low`/`CI_high` to `conf.low`/`conf.high`,
#'   `p` to `p.value`);
#' * row roles, from the frame's structure and never from a label (a
#'   covariate named `p`, `alpha` or `lnsigma` stays a covariate): a `role`
#'   column (`"coef"`, `"intercept"`, `"cutpoint"`, `"ancillary"`); else
#'   Stata's `/`, `_diparm#`, `lnsigma`, `lnalpha`, `ln_p` and `lngamma`
#'   equations, and a term written in the `/` equation (`/sigma`), hold
#'   ancillary parameters (`/cut#` a cutpoint); else `var_type`
#'   `"cutpoint"`/`"ancillary"`, and a `var_type = "intercept"` row other
#'   than `(Intercept)`/`_cons` (broom.helpers types a model's thresholds
#'   and `Log(scale)` as intercepts) is a cutpoint when broom's `coef.type`
#'   says so or the term is a threshold (`a|b`), else ancillary.
#'   Cutpoints and ancillary rows keep the estimate's own scale (null 0),
#'   follow `nointercept`/`keepintercept` with the intercept, and are keyed
#'   in Stata's `/` equation (`/::sigma`), so they join a fitted model's
#'   ancillary rows of the same name;
#' * optional description columns: `label`, `var_label`, `variable`,
#'   `var_type` (`"intercept"`, `"continuous"`, `"categorical"`,
#'   `"dichotomous"`, `"interaction"`, `"cutpoint"`, `"ancillary"`),
#'   `role`, `reference_row`, `header_row`
#'   (header rows are dropped; regtab writes its own), `contrasts_type`
#'   (only treatment contrasts: a `"sum"` or `"helmert"` factor has no
#'   reference level), and `key`;
#' * keys: a factor level is keyed like a fitted model's level, `2.treat`,
#'   so `regtab(fit, tidy_plus_plus(fit))` shares the level rows of `treat`
#'   (and `keep = "2.treat"` works). The two columns of that example can
#'   show different intervals: broom.helpers' default intervals for a `glm`
#'   are profile-likelihood ones, while regtab computes Wald intervals, and
#'   supplied intervals are shown as they are. The code is the level's label when it
#'   is an integer, else its position among the variable's rows (reference
#'   row included), as for a fitted factor without Stata value codes. A
#'   variable without a reference row keeps its `term` as the key, because
#'   its base level's position is unknown. A `key` column (Stata keys such
#'   as `2.treat`, `age`, `_cons`) overrides the derived key for the rows
#'   where it is not missing. An interaction row is keyed like a fitted
#'   model's (`2.g#c.age`) when each part of its term is a level row or a
#'   continuous variable of the data frame, else by its term;
#' * an equation column, `equation` (or `y.level`, or `component`), turns
#'   on Stata's multi-equation layout: rows read `<equation>: <label>`,
#'   with Stata's equation names translated as regtab does (`inflate` is
#'   "Inflation equation", `selection`/`selection_ll`/`selection_ul`
#'   "Selection equation", `lnsigma` "Scale", `/` and `_diparm*`
#'   "Ancillary") and underscores shown as spaces.
#'
#' Attributes describe the model: `"stata_cmd"` (the Stata command word,
#' which sets the estimate header, the null value for `dimnonsig`, and the
#' automatic `nointercept` as for a collected Stata model, e.g. `"logit"`,
#' `"churdle"`); `"effect_scale"`, `"null_value"`, `"se_scale"` and
#' `"conf.level"` (above); `"glance"`, a
#' named list or one-row data frame with numeric scalar entries (`NA` or
#' `NULL` for unavailable statistics), with any of `nobs`, `n_sub`, `logLik`, `df`
#' (Stata's e(rank)), `AIC`, `BIC` (recomputed from `logLik` and `df` when
#' both are given), `r.squared`, `adj.r.squared`, `pseudo.r.squared`,
#' `groups`, `icc`, `qic`, `nevent` (Events), `rmse` (Root MSE), `F` (the F
#' statistic, shown only when `"stata_cmd"` is `"regress"`, `"anova"`,
#' `"areg"`, `"xtreg"`, `"ivregress"` or `"cnsreg"`, the linear models Stata
#' shows it for), `nimp` (Imputations) and `fmi` (Largest FMI), for
#' `stats`; and `"model_id"`,
#' `"outcome_id"` for [as_forest_data()].
#' @name regtab
NULL

# Header and automatic nointercept by Stata command word, as regtab
# classifies collected models (`regtab.ado:521-660`).
.mi_cmd_scale <- function(cmd) {
  cmd <- tolower(trimws(cmd %||% ""))
  if (cmd %in% c("logit", "logistic", "ologit", "melogit", "clogit")) return(list(scale = "OR", noint = TRUE))
  if (cmd %in% c("poisson", "nbreg", "mepoisson", "menbreg")) return(list(scale = "IRR", noint = TRUE))
  if (cmd == "mlogit") return(list(scale = "RRR", noint = TRUE))
  if (cmd %in% c("stcox", "cox", "mestreg", "mecloglog")) return(list(scale = "HR", noint = TRUE))
  if (cmd %in% c("stcrreg", "finegray")) return(list(scale = "SHR", noint = TRUE))
  # Stata 2.1.10+ shows streg as HR (hazard metric) or TR (time metric),
  # never AF; a tidy data frame of AFT estimates is the time metric.
  if (cmd == "streg") return(list(scale = "TR", noint = TRUE))
  if (cmd %in% c("zip", "zinb", "churdle")) return(list(scale = "Coef.", noint = TRUE))
  list(scale = "Coef.", noint = FALSE)
}

# Effect scales a data frame may declare (attr(x, "effect_scale")), by the
# null value `dimnonsig` and derived statistics compare against (review
# P0-2 of group t2a: an unknown label used to fall back to null 0, so a
# risk ratio at 1 read as significant). Matched case-insensitively; any
# other label needs attr(x, "null_value") (0 or 1) whenever the null is
# used (a derived interval or p-value, or dimnonsig).
.rt_ratio_scales <- c("OR", "HR", "IRR", "RRR", "SHR", "TR", "AF", "RR", "PR", "GMR", "sHR", "csHR",
                      "exp(b)", "exp(beta)", "exp(coef)", "odds ratio", "odds ratios", "hazard ratio",
                      "hazard ratios", "risk ratio", "risk ratios", "relative risk", "relative risks",
                      "rate ratio", "rate ratios", "incidence rate ratio", "incidence rate ratios",
                      "prevalence ratio", "prevalence ratios", "relative risk ratio", "relative risk ratios",
                      "subhazard ratio", "subhazard ratios", "time ratio", "time ratios",
                      "geometric mean ratio", "geometric mean ratios")
.rt_diff_scales <- c("Coef.", "Coef", "Coefficient", "Coefficients", "Beta", "b", "RD", "MD",
                     "risk difference", "risk differences", "mean difference", "mean differences",
                     "difference", "differences")

.rt_scale_null <- function(scale) {
  s <- tolower(trimws(scale))
  if (s %in% tolower(.rt_ratio_scales)) return(1)
  if (s %in% tolower(.rt_diff_scales)) return(0)
  NA_real_
}

#' The effect scale and null value of a data frame
#'
#' In order: `attr(x, "effect_scale")` (with `attr(x, "null_value")` for a
#' label outside the known lists); `attr(x, "stata_cmd")`; broom.helpers'
#' own record that it exponentiated the estimates (`attr(x,
#' "exponentiate")`, with its `coefficients_label` as the header, e.g.
#' "OR", "IRR", "HR", "exp(Beta)"): exponentiated estimates are ratios,
#' null 1. Otherwise coefficients (`Coef.`, null 0), unless a broom
#' `statistic` column shows the estimates were exponentiated
#' (`.rt_df_statistic_check()`). Review P0-1/P0-2 of group t2a.
#' @return list(scale, null (1, 0 or NA when unknown), noint, cmd, source).
#' @keywords internal
#' @noRd
.rt_df_scale <- function(x) {
  cmd <- attr(x, "stata_cmd", exact = TRUE) %||% "unknown"
  cs <- .mi_cmd_scale(cmd)
  es <- attr(x, "effect_scale", exact = TRUE)
  nv <- attr(x, "null_value", exact = TRUE)
  if (!is.null(nv) && (!is.numeric(nv) || length(nv) != 1L || !nv %in% c(0, 1))) {
    cli::cli_abort("The {.code null_value} attribute of a data frame passed to {.fn regtab} must be 0 or 1.", call = NULL)
  }
  if (!is.null(es)) {
    if (!is.character(es) || length(es) != 1L || is.na(es) || !nzchar(trimws(es))) {
      cli::cli_abort("The {.code effect_scale} attribute of a data frame passed to {.fn regtab} must be one string.", call = NULL)
    }
    null <- .rt_scale_null(es)
    if (!is.null(nv)) {
      if (!is.na(null) && null != nv) {
        cli::cli_abort("The data frame's {.code effect_scale} {.val {es}} has null {null}, but its {.code null_value} is {nv}.",
                       call = NULL)
      }
      null <- nv
    }
    # A ratio scale (null 1) is eligible for the automatic nointercept, as
    # a fitted ratio-scale model is (pre-release review P2-3).
    return(list(scale = es, null = null, noint = cs$noint || isTRUE(null == 1), cmd = cmd,
                source = "effect_scale"))
  }
  if (!identical(cmd, "unknown")) {
    return(list(scale = cs$scale, null = if (cs$scale %in% .rt_ratio_scales) 1 else 0, noint = cs$noint,
                cmd = cmd, source = "stata_cmd"))
  }
  if (isTRUE(attr(x, "exponentiate", exact = TRUE))) {
    lab <- attr(x, "coefficients_label", exact = TRUE)
    lab <- if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) lab else "exp(b)"
    return(list(scale = lab, null = 1, noint = TRUE, cmd = cmd, source = "exponentiate"))
  }
  list(scale = "Coef.", null = if (is.null(nv)) 0 else nv, noint = FALSE, cmd = cmd, source = "default")
}

#' Cross-check a broom `statistic` column against the declared scale
#'
#' broom's `statistic` is the Wald statistic of the link-scale coefficient,
#' log(estimate) / std.error for exponentiated output (`broom::tidy(fit,
#' exponentiate = TRUE)` sets no attribute). When every row with a
#' standard error agrees with log(estimate) / std.error and not with the
#' statistic the declared scale implies, the frame is refused rather than
#' tabled on the wrong scale. Rows are compared at a relative tolerance of
#' 1e-4; a statistic that fits neither is left alone (it may be a t from
#' another source).
#' @keywords internal
#' @noRd
.rt_df_statistic_check <- function(x, sc) {
  b <- .rt_df_body(x)
  if (!all(c("statistic", "std.error") %in% names(b)) || !is.numeric(b$statistic)) return(invisible(TRUE))
  est <- as.numeric(b$estimate)
  se <- as.numeric(b$std.error)
  st <- as.numeric(b$statistic)
  ok <- !is.na(est) & !is.na(se) & !is.na(st) & is.finite(est) & est > 0 & se > 0
  ok <- ok & .rt_df_roles(b) == "coef"
  if (!any(ok)) return(invisible(TRUE))
  near <- function(a, v) abs(a - v) <= 1e-4 * pmax(1, abs(v))
  is_log <- near(st[ok], log(est[ok]) / se[ok])
  is_id <- near(st[ok], est[ok] / se[ok])
  is_ratio_est <- near(st[ok], (est[ok] - 1) / se[ok])
  link <- identical(attr(x, "se_scale", exact = TRUE), "link")
  if (isTRUE(sc$null == 0) && all(is_log) && !all(is_id)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} looks exponentiated: its {.field statistic} column is log(estimate) / std.error, as {.code broom::tidy(exponentiate = TRUE)} returns it, but no ratio scale is declared.",
      "i" = "Set {.code attr(x, \"effect_scale\")} (for example {.val OR}, {.val HR}, {.val IRR}, {.val RR}), and {.code attr(x, \"se_scale\") <- \"link\"} if intervals or p-values must be derived from {.field std.error}."
    ), call = NULL)
  }
  if (isTRUE(sc$null == 1) && link && !any(is_log) && all(is_id | is_ratio_est)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} declares {.code se_scale = \"link\"}, but its {.field statistic} column is not log(estimate) / std.error.",
      "i" = "Check that the estimates are exponentiated and {.field std.error} is on the log scale; use {.code se_scale = \"estimate\"} for a standard error of the ratio itself."
    ), call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_model_info.data.frame <- function(fit, ...) {
  .rt_tidy_check(fit)
  sc <- .rt_df_scale(fit)
  .rt_df_statistic_check(fit, sc)
  # A stand-in object: .mi() looks for a call and a formula, which a data
  # frame (or tibble) does not have.
  info <- .mi(structure(list(), class = "tt_tidy_df"), sc$cmd, effect_scale = sc$scale, exponentiate = FALSE,
              null_value = sc$null, auto_nointercept = sc$noint)
  info$class <- class(fit)[1]
  info$model_id <- attr(fit, "model_id", exact = TRUE) %||% NA_character_
  info$outcome_id <- attr(fit, "outcome_id", exact = TRUE) %||% NA_character_
  info
}

# A data frame carries its own standard errors: no other variance exists
# to switch to (external review F32: vce = "model" was accepted silently).
#' @export
tt_vce_types.data.frame <- function(fit) "stata"

# ---------------------------------------------------------------------------
# Schema (external review F02)

.rt_df_num_cols <- c("estimate", "std.error", "conf.low", "conf.high", "p.value", "df", "df.error", "conf.level")
.rt_df_text_cols <- c("term", "label", "var_label", "variable", "var_type", "role", "key", "equation", "y.level",
                      "component", "contrasts_type")
.rt_df_var_types <- c("intercept", "continuous", "categorical", "dichotomous", "interaction", "cutpoint", "ancillary")

# The rows regtab reads: header rows (tidy_plus_plus(add_header_rows =
# TRUE), term NA) are dropped, as regtab writes its own.
.rt_df_body <- function(fit) {
  x <- as.data.frame(fit, stringsAsFactors = FALSE)
  if ("header_row" %in% names(x)) x <- x[!(x$header_row %in% TRUE), , drop = FALSE]
  rownames(x) <- NULL
  x
}

# Rows named in an error: their terms, at most eight.
.rt_df_which <- function(term, bad) {
  t <- term[bad]
  if (length(t) > 8L) t <- c(t[1:8], "...")
  t
}

#' Validate a data frame passed to regtab() (external review F02)
#'
#' Every column regtab reads is checked before anything is converted:
#' identifiers, numeric statistics (a factor would become its level codes),
#' finite values, positive standard errors and degrees of freedom, ordered
#' and paired bounds, p-values in \[0, 1\], logical flags, known `var_type`
#' values, treatment contrasts, and the `se_scale`/`conf.level`
#' declarations.
#' @param x The data frame.
#' @param i Model number for the messages, or `NULL`.
#' @keywords internal
#' @noRd
.rt_tidy_check <- function(x, i = NULL) {
  where <- if (is.null(i)) "The data frame passed to {.fn regtab}" else paste0("Model ", i, " (a data frame)")
  bad <- function(msg, ...) cli::cli_abort(c(paste(where, msg), ...), call = NULL, .envir = parent.frame())
  miss <- setdiff(c("term", "estimate"), names(x))
  if (length(miss)) {
    cli::cli_abort(c(
      "A data frame passed to {.fn regtab} needs the columns {.field term} and {.field estimate}.",
      "x" = "Missing: {.field {miss}}.",
      "i" = "Use {.fn broom.helpers::tidy_plus_plus} output or a data frame of that shape.",
      "i" = if (all(c("Parameter", "Coefficient") %in% names(x))) "For {.fn parameters::model_parameters} output rename {.field Parameter} to {.field term}, {.field Coefficient} to {.field estimate}, {.field SE} to {.field std.error}, {.field CI_low}/{.field CI_high} to {.field conf.low}/{.field conf.high} and {.field p} to {.field p.value}; with {.code exponentiate = TRUE} its SE is on the ratio scale ({.code se_scale = \"estimate\"})."
    ), call = NULL)
  }
  if ("header_row" %in% names(x) && !is.logical(x$header_row)) {
    bad("has a {.field header_row} column that is not logical (TRUE/FALSE).")
  }
  b <- .rt_df_body(x)
  if (!is.numeric(b$estimate) && !(is.logical(b$estimate) && all(is.na(b$estimate)))) {
    cli::cli_abort("Column {.field estimate} must be numeric.", call = NULL)
  }
  .tt_check_unambiguous(b, c(.rt_df_num_cols, .rt_df_text_cols, "reference_row", "header_row"), "fit")
  for (nm in intersect(.rt_df_num_cols, names(b))) {
    v <- b[[nm]]
    if (is.numeric(v) || (is.logical(v) && all(is.na(v)))) next
    kind <- if (is.factor(v)) "a factor" else paste0("of class <", class(v)[1], ">")
    bad(paste0("has a column {.field ", nm, "} that is ", kind, ", not numeric."),
        "i" = if (is.factor(v)) "{.code as.numeric()} of a factor gives its level codes, not the numbers shown; convert it with {.code as.numeric(as.character(x))} first." else
          "Convert it with {.code as.numeric()} first (regtab does not guess at text).")
  }
  for (nm in intersect(.rt_df_text_cols, names(b))) {
    v <- b[[nm]]
    if (!(is.character(v) || is.factor(v) || all(is.na(v)))) {
      bad(paste0("has a column {.field ", nm, "} that is not character (or factor)."))
    }
  }
  for (nm in intersect(c("reference_row", "header_row"), names(b))) {
    if (!is.logical(b[[nm]])) bad(paste0("has a column {.field ", nm, "} that is not logical (TRUE/FALSE/NA)."))
  }
  term <- as.character(b$term)
  noterm <- which(is.na(term) | !nzchar(trimws(ifelse(is.na(term), "", term))))
  if (length(noterm)) bad("has a missing or empty {.field term} in {cli::qty(length(noterm))}row{?s} {noterm}.")
  eqcol <- intersect(c("equation", "y.level", "component"), names(b))[1]
  eq <- if (is.na(eqcol)) rep("", nrow(b)) else as.character(b[[eqcol]])
  eq[is.na(eq)] <- ""
  dup <- duplicated(paste(eq, term, sep = "\r"))
  if (any(dup)) {
    cli::cli_abort(c("The data frame passed to {.fn regtab} repeats a coefficient.",
                     "x" = "Repeated: {.val {unique(term[dup])}}.",
                     "i" = "Give each equation's rows an {.field equation} column."), call = NULL)
  }
  num <- function(nm) if (nm %in% names(b)) as.numeric(b[[nm]]) else rep(NA_real_, nrow(b))
  # NaN is never a statistic. An infinite estimate or bound is what exp()
  # of a huge coefficient gives (a separated logit): shown blank, as for a
  # fitted model (golden R57). A standard error or p-value must be finite.
  for (nm in c("estimate", "std.error", "conf.low", "conf.high", "p.value")) {
    v <- num(nm)
    nf <- is.nan(v) | (nm %in% c("std.error", "p.value") & !is.na(v) & !is.finite(v))
    if (any(nf)) bad(paste0("has non-finite {.field ", nm, "} values ({.val {(.rt_df_which(term, nf))}})."))
  }
  se <- num("std.error")
  if (any(se <= 0, na.rm = TRUE)) {
    bad("has a {.field std.error} that is not positive ({.val {(.rt_df_which(term, !is.na(se) & se <= 0))}}).",
        "i" = "Leave it missing ({.code NA}) for a coefficient without a standard error.")
  }
  lo <- num("conf.low")
  hi <- num("conf.high")
  one <- xor(is.na(lo), is.na(hi))
  if (any(one)) bad("gives only one confidence bound for {.val {(.rt_df_which(term, one))}}.",
                    "i" = "Give both {.field conf.low} and {.field conf.high}, or neither.")
  rev <- !is.na(lo) & !is.na(hi) & lo > hi
  if (any(rev)) bad("has {.field conf.low} above {.field conf.high} for {.val {(.rt_df_which(term, rev))}}.")
  p <- num("p.value")
  pout <- !is.na(p) & (p < 0 | p > 1)
  if (any(pout)) bad("has a {.field p.value} outside [0, 1] for {.val {(.rt_df_which(term, pout))}}.")
  dfc <- intersect(c("df", "df.error"), names(b))
  if (length(dfc) == 2L) bad("has both {.field df} and {.field df.error}; keep one.")
  if (length(dfc)) {
    d <- num(dfc)
    dbad <- is.nan(d) | (!is.na(d) & d <= 0)
    if (any(dbad)) bad(paste0("has a {.field ", dfc, "} that is not positive for {.val {(.rt_df_which(term, dbad))}}."),
                       "i" = "Use {.code Inf} for the normal distribution, or {.code NA}.")
  }
  if ("var_type" %in% names(b)) {
    vt <- as.character(b$var_type)
    unk <- unique(vt[!is.na(vt) & !vt %in% .rt_df_var_types])
    if (length(unk)) bad("has an unknown {.field var_type} {.val {unk}}.",
                         "i" = "Use one of {.val {(.rt_df_var_types)}}.")
  }
  if ("role" %in% names(b)) {
    rl <- as.character(b$role)
    unk <- unique(rl[!is.na(rl) & !rl %in% .rt_df_roles_known])
    if (length(unk)) bad("has an unknown {.field role} {.val {unk}}.", "i" = "Use one of {.val {(.rt_df_roles_known)}}.")
  }
  if ("contrasts_type" %in% names(b)) {
    ct <- as.character(b$contrasts_type)
    nt <- !is.na(ct) & !ct %in% c("treatment", "SAS")
    if (any(nt)) {
      v <- unique(as.character(.rt_col(b, "variable", NA_character_))[nt])
      bad("uses {.val {unique(ct[nt])}} contrasts ({.val {v[!is.na(v)]}}), whose levels have no reference level.",
          "i" = "Refit with treatment contrasts ({.fn contr.treatment}), as for a fitted model.")
    }
  }
  if ("key" %in% names(b)) {
    k <- as.character(b$key)
    if (any(!is.na(k) & !nzchar(trimws(k)))) bad("has an empty {.field key}; use {.code NA} to derive it.")
  }
  sc <- attr(x, "se_scale", exact = TRUE)
  if (!is.null(sc) && (!is.character(sc) || length(sc) != 1L || !sc %in% c("link", "estimate"))) {
    bad("has an {.code se_scale} attribute that is not {.val link} or {.val estimate}.")
  }
  .rt_glance_check(attr(x, "glance", exact = TRUE), where)
  .rt_df_declared_level(x, where)
  invisible(TRUE)
}

# Model statistics must not be truncated to their first observation or
# coerced from factor codes. Unknown backend fields are left unused.
.rt_glance_check <- function(g, where) {
  if (is.null(g)) return(invisible(TRUE))
  bad <- function(msg) cli::cli_abort(paste(where, "has an invalid {.field glance}:", msg), call = NULL)
  if (!is.list(g)) bad("use a named list or one-row data frame.")
  if (is.data.frame(g) && length(g) && nrow(g) != 1L) bad("the data frame must have exactly one row.")
  if (length(g) && (is.null(names(g)) || anyNA(names(g)) || any(!nzchar(names(g))) || anyDuplicated(names(g)))) {
    bad("statistic names must be non-missing, non-empty and unique.")
  }
  known <- c("nobs", "n_sub", "logLik", "df", "AIC", "BIC", "r.squared", "adj.r.squared",
             "pseudo.r.squared", "groups", "icc", "qic", "nevent", "rmse", "F", "nimp", "fmi")
  for (nm in intersect(known, names(g))) {
    v <- g[[nm]]
    if (is.null(v)) next
    missing <- is.logical(v) && length(v) == 1L && is.na(v)
    if (length(v) != 1L || !(is.numeric(v) || missing) || any(is.nan(v))) {
      bad(paste0("statistic {.field ", nm, "} must be one number or {.code NA}, not a factor, text or vector."))
    }
  }
  invisible(TRUE)
}

# The declared confidence level of supplied intervals: attr(x,
# "conf.level") or a constant conf.level column, as a proportion; NULL when
# neither is given.
.rt_df_declared_level <- function(x, where = "The data frame passed to {.fn regtab}") {
  norm <- function(v, what) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0 || v >= 100 || v == 1) {
      cli::cli_abort(paste(where, "has a", what, "that is not one proportion in (0, 1) or percentage in (1, 100)."),
                     call = NULL)
    }
    if (v > 1) v / 100 else v
  }
  a <- attr(x, "conf.level", exact = TRUE)
  if (!is.null(a)) a <- norm(a, "{.code conf.level} attribute")
  col <- NULL
  if ("conf.level" %in% names(x)) {
    v <- unique(as.numeric(x$conf.level[!is.na(x$conf.level)]))
    if (length(v) > 1L) cli::cli_abort(paste(where, "has a {.field conf.level} column with several levels."), call = NULL)
    if (length(v)) col <- norm(v, "{.field conf.level} column")
  }
  if (!is.null(a) && !is.null(col) && abs(a - col) > 1e-9) {
    cli::cli_abort(paste(where, "declares two confidence levels (attribute and column)."), call = NULL)
  }
  a %||% col
}

# `level` left at its default (review P2-2 of group t2a): the level every
# data frame with supplied intervals declares, when they agree; frames that
# declare different levels are refused. Otherwise the default stands.
.rt_df_default_level <- function(fits, level) {
  lv <- c()
  for (i in seq_along(fits)) {
    f <- fits[[i]]
    if (!is.data.frame(f)) next
    b <- .rt_df_body(f)
    if (!"conf.low" %in% names(b) || !any(!is.na(b$conf.low))) next
    d <- .rt_df_declared_level(f, paste0("Model ", i, " (a data frame)"))
    if (!is.null(d)) lv[as.character(i)] <- d
  }
  if (!length(lv)) return(level)
  if (any(abs(lv - lv[1]) > 1e-9)) {
    cli::cli_abort(c(
      "The data frames passed to {.fn regtab} supply intervals at different confidence levels.",
      "x" = "{paste0('model ', names(lv), ': ', .rt_pct_text(lv), '%', collapse = '; ')}."
    ), call = NULL)
  }
  unname(lv[1])
}

#' Refuse a `level` that differs from the level of supplied intervals
#'
#' External review F32: a supplied interval is shown as it is, so `level`
#' would only relabel its header. The supplied intervals' level is the
#' declared one, else broom's default 95%.
#' @keywords internal
#' @noRd
.rt_df_check_level <- function(x, level, i) {
  b <- .rt_df_body(x)
  if (!"conf.low" %in% names(b) || !any(!is.na(b$conf.low))) return(invisible(TRUE))
  declared <- .rt_df_declared_level(x, paste0("Model ", i, " (a data frame)"))
  have <- declared %||% 0.95
  if (abs(have - level) > 1e-9) {
    pct <- function(v) paste0(.rt_pct_text(v), "%")
    cli::cli_abort(c(
      "Model {i} (a data frame) supplies {pct(have)} confidence intervals, but {.code level = {level}} asks for {pct(level)}.",
      "i" = if (is.null(declared)) "Supplied intervals are taken as 95% (broom's default) unless {.code attr(x, \"conf.level\")} or a {.field conf.level} column says otherwise.",
      "i" = "regtab does not recompute supplied intervals: pass {.code level = {have}}, or supply intervals at the level you want."
    ), call = NULL)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Rows

# Stata's equation labels (`regtab.ado:1562-1567`).
.rt_stata_eq_label <- function(eq) {
  lo <- tolower(eq)
  eq[lo == "inflate"] <- "Inflation equation"
  eq[lo %in% c("selection_ll", "selection_ul", "selection")] <- "Selection equation"
  eq[lo == "lnsigma"] <- "Scale"
  eq[lo == "/" | grepl("^_diparm", lo)] <- "Ancillary"
  .rt_eq_label(eq)
}

.rt_col <- function(x, nm, default = NA) if (nm %in% names(x)) x[[nm]] else rep(default, nrow(x))

# Structural roles of a data frame's rows (Milestone H task H5's `role`;
# review of group t2a, P1-2 and T2B-18): "coef", "intercept", "cutpoint"
# or "ancillary", decided from the frame's structure, never from a label,
# so a covariate named or labelled `p`, `alpha`, `lnsigma` or `cut1` stays
# a covariate. In order:
# 1. a `role` column (NA entries fall through);
# 2. the equation: Stata's `/` equation, `_diparm#`, and the scale and
#    ancillary equations `lnsigma`, `lnalpha`, `ln_p`, `lngamma` hold
#    ancillary parameters (cutpoints for `/cut#`); their intercept is too;
# 3. a term in Stata's `/` equation written with its slash (`/sigma`,
#    `/cut1`);
# 4. `var_type`: `"cutpoint"`/`"ancillary"`; `"intercept"` is the intercept
#    for `(Intercept)`, `_cons`, `Intercept`, and otherwise one of the
#    model-level parameters broom.helpers types as intercepts: a cutpoint
#    when broom says so (`coef.type` "scale" for polr, "intercept" for clm)
#    or the term is a threshold (`a|b`), else ancillary (survreg's
#    `Log(scale)`);
# 5. otherwise a coefficient.
.rt_df_roles_known <- c("coef", "intercept", "cutpoint", "ancillary")
.rt_df_anc_eqs <- c("/", "lnsigma", "lnalpha", "ln_p", "lngamma")
.rt_df_icpt_terms <- c("(Intercept)", "_cons", "Intercept")

.rt_df_roles <- function(x) {
  n <- nrow(x)
  term <- as.character(x$term)
  vt <- as.character(.rt_col(x, "var_type", NA_character_))
  role <- as.character(.rt_col(x, "role", NA_character_))
  eqcol <- intersect(c("equation", "y.level", "component"), names(x))[1]
  eq <- if (is.na(eqcol)) rep("", n) else as.character(x[[eqcol]])
  eq[is.na(eq)] <- ""
  ct <- tolower(as.character(.rt_col(x, "coef.type", NA_character_)))
  cutname <- grepl("^/?cut[0-9]+$", term)
  anc_eq <- tolower(eq) %in% .rt_df_anc_eqs | grepl("^_diparm", tolower(eq))
  out <- rep(NA_character_, n)
  out[!is.na(role)] <- role[!is.na(role)]
  todo <- is.na(out)
  out[todo & anc_eq] <- ifelse(cutname[todo & anc_eq], "cutpoint", "ancillary")
  todo <- is.na(out)
  slash <- todo & startsWith(term, "/")
  out[slash] <- ifelse(cutname[slash], "cutpoint", "ancillary")
  todo <- is.na(out)
  out[todo & vt %in% c("cutpoint", "ancillary")] <- vt[todo & vt %in% c("cutpoint", "ancillary")]
  todo <- is.na(out)
  icpt <- todo & (vt %in% "intercept" | (is.na(vt) & term %in% .rt_df_icpt_terms))
  model_icpt <- term %in% .rt_df_icpt_terms
  out[icpt & model_icpt] <- "intercept"
  other <- icpt & !model_icpt
  thr <- ct %in% c("scale", "intercept", "alpha", "threshold") | grepl("|", term, fixed = TRUE)
  out[other] <- ifelse(thr[other], "cutpoint", "ancillary")
  out[is.na(out)] <- "coef"
  out
}

#' @export
tt_regtab_adapter.data.frame <- function(fit, ...) TRUE

#' @export
tt_regtab_rows.data.frame <- function(fit, info, level = 0.95, ...) {
  .rt_tidy_check(fit)
  x <- .rt_df_body(fit)
  n <- nrow(x)
  term <- as.character(x$term)
  est <- as.numeric(x$estimate)
  lo <- as.numeric(.rt_col(x, "conf.low"))
  hi <- as.numeric(.rt_col(x, "conf.high"))
  p <- as.numeric(.rt_col(x, "p.value"))
  se <- as.numeric(.rt_col(x, "std.error"))
  dfc <- intersect(c("df", "df.error"), names(x))[1]
  dfv <- if (is.na(dfc)) rep(Inf, n) else as.numeric(x[[dfc]])
  dfv[is.na(dfv)] <- Inf
  vtype <- as.character(.rt_col(x, "var_type", NA_character_))
  vtype[is.na(vtype) & term %in% .rt_df_icpt_terms] <- "intercept"
  vtype[is.na(vtype)] <- "continuous"
  # Structural roles (H5; never from labels): only the model's intercept is
  # the intercept row; cutpoints and ancillary parameters are rows of their
  # own, keyed in Stata's `/` equation by tt_regtab_build().
  rrole <- .rt_df_roles(x)
  vtype[vtype == "intercept" & rrole != "intercept"] <- "continuous"
  vtype[rrole == "intercept"] <- "intercept"
  variable <- as.character(.rt_col(x, "variable", NA_character_))
  variable[is.na(variable)] <- term[is.na(variable)]
  lab <- as.character(.rt_col(x, "label", NA_character_))
  vlab <- as.character(.rt_col(x, "var_label", NA_character_))
  ukey <- as.character(.rt_col(x, "key", NA_character_))
  ref <- .rt_col(x, "reference_row", FALSE) %in% TRUE
  eqcol <- intersect(c("equation", "y.level", "component"), names(x))[1]
  is_level <- vtype %in% c("categorical", "dichotomous")
  status <- ifelse(ref, "base", ifelse(is.na(est), "omit", "est"))
  cont_label <- ifelse(!is.na(vlab) & nzchar(vlab), vlab, ifelse(!is.na(lab) & nzchar(lab), lab, term))
  lev_label <- ifelse(!is.na(lab) & nzchar(lab), lab, term)
  eq <- if (is.na(eqcol)) rep("", n) else as.character(x[[eqcol]])
  eq[is.na(eq)] <- ""
  eql <- .rt_stata_eq_label(eq)

  # Displayed label of each input row.
  core <- ifelse(vtype == "intercept" | term %in% .rt_df_icpt_terms, "Intercept",
                 ifelse(is_level, lev_label, cont_label))
  disp <- if (!is.na(eqcol)) {
    ifelse(nzchar(eql), paste0(eql, ": ", core), core)
  } else {
    ifelse(vtype == "intercept", "Intercept", ifelse(is_level, paste0("  ", lev_label), cont_label))
  }
  anc <- rrole %in% c("cutpoint", "ancillary")

  # Derived statistics (H-D5). A ratio row (every row of a ratio-scale
  # model but its ancillary rows) needs the declared scale of std.error;
  # other rows have std.error on the scale of estimate and null 0.
  est_row <- status == "est"
  null0 <- info$null_value
  ratio <- isTRUE(null0 == 1) & !anc
  nonpos <- est_row & ratio & est <= 0
  if (any(nonpos)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} has {info$effect_scale} estimates that are not positive: {.val {(.rt_df_which(term, nonpos))}}.",
      "i" = "Ratios are shown as given: pass exponentiated estimates ({.code tidy_plus_plus(exponentiate = TRUE)}), or set {.code attr(x, \"effect_scale\")} (or {.code \"stata_cmd\"}) to the coefficient scale."
    ), call = NULL)
  }
  # Supplied bounds (muse P1-7): a ratio's limits are not negative (0 is
  # exp() of a very negative bound, underflowed, as under separation), and
  # no interval ends below where it starts.
  badlo <- est_row & ratio & ((!is.na(lo) & lo < 0) | (!is.na(hi) & hi < 0))
  if (any(badlo)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} has negative {info$effect_scale} confidence limits: {.val {(.rt_df_which(term, badlo))}}.",
      "i" = "A ratio's confidence limits are ratios too: pass exponentiated limits, or set {.code attr(x, \"effect_scale\")} to the coefficient scale."
    ), call = NULL)
  }
  swapped <- est_row & !is.na(lo) & !is.na(hi) & lo > hi
  if (any(swapped)) {
    cli::cli_abort("The data frame passed to {.fn regtab} has {.field conf.low} above {.field conf.high} for {.val {(.rt_df_which(term, swapped))}}.",
                   call = NULL)
  }
  has_p_col <- "p.value" %in% names(x)
  need_ci <- est_row & is.na(lo) & is.na(hi) & !is.na(se)
  need_p <- est_row & is.na(p) & !is.na(se) & !has_p_col
  se_scale <- attr(fit, "se_scale", exact = TRUE)
  # broom.helpers' exponentiate = TRUE also exponentiates thresholds and
  # ancillary parameters, whose std.error stays on their own scale: no
  # interval or p-value can be derived for them (their scale is shown raw
  # in Stata).
  anc_exp <- (need_ci | need_p) & anc & isTRUE(attr(fit, "exponentiate", exact = TRUE))
  if (any(anc_exp)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} was exponentiated by {.pkg broom.helpers}, cutpoints and ancillary parameters included, and {cli::qty(sum(anc_exp))}row{?s} {.val {(.rt_df_which(term, anc_exp))}} need{?s/} a derived interval or p-value.",
      "i" = "Supply their {.field conf.low}, {.field conf.high} and {.field p.value}, or leave them out ({.code tidy_plus_plus(intercept = FALSE)}, the default)."
    ), call = NULL)
  }
  # An unknown effect scale has no null to test against (review P0-2).
  if (is.na(null0) && any((need_ci | need_p) & !anc)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} has the effect scale {.val {info$effect_scale}}, whose null value regtab does not know, and {cli::qty(sum((need_ci | need_p) & !anc))}row{?s} {.val {(.rt_df_which(term, (need_ci | need_p) & !anc))}} need{?s/} a derived interval or p-value.",
      "i" = "Set {.code attr(x, \"null_value\")} to 1 for a ratio or 0 for a difference (and {.code attr(x, \"se_scale\")} for a ratio), or use a known label such as {.val OR}, {.val RR}, {.val HR}, {.val Coef.}.",
      "i" = "Or supply {.field conf.low}, {.field conf.high} and {.field p.value}."
    ), call = NULL)
  }
  # A declared SE scale is never ignored: on a frame without ratio rows it
  # contradicts the estimates (review P0-1).
  if (!is.null(se_scale) && !any(ratio & est_row)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} declares {.code se_scale = {.val {se_scale}}}, but none of its estimates is on a ratio scale (effect scale {.val {info$effect_scale}}).",
      "i" = "For exponentiated estimates set {.code attr(x, \"effect_scale\")} (for example {.val OR}); for coefficients remove {.code se_scale}."
    ), call = NULL)
  }
  undeclared <- (need_ci | need_p) & ratio & is.null(se_scale)
  if (any(undeclared)) {
    cli::cli_abort(c(
      "The data frame passed to {.fn regtab} is on a ratio scale ({info$effect_scale}), and {cli::qty(sum(undeclared))}row{?s} {.val {(.rt_df_which(term, undeclared))}} need{?s/} a confidence interval or p-value derived from {.field std.error}, whose scale is not declared.",
      "i" = "Set {.code attr(x, \"se_scale\") <- \"link\"} when {.field std.error} is the standard error of log(estimate), as {.code tidy_plus_plus(exponentiate = TRUE)} gives it, or {.code \"estimate\"} when it is on the ratio scale.",
      "i" = if (isTRUE(attr(fit, "exponentiate", exact = TRUE))) "Its {.code exponentiate} attribute says the estimates were exponentiated by {.pkg broom.helpers}, whose {.field std.error} stays on the log scale ({.val link}).",
      "i" = "Or supply {.field conf.low}, {.field conf.high} and {.field p.value}."
    ), call = NULL)
  }
  link <- ratio & identical(se_scale, "link")
  null <- ifelse(ratio, 1, 0)
  b <- ifelse(link, log(pmax(est, .Machine$double.xmin)), est)
  z <- ifelse(link, b / se, (est - null) / se)
  q <- ifelse(is.finite(dfv), stats::qt((1 + level) / 2, pmax(dfv, 1e-300)), stats::qnorm((1 + level) / 2))
  lo[need_ci] <- ifelse(link, exp(b - q * se), est - q * se)[need_ci]
  hi[need_ci] <- ifelse(link, exp(b + q * se), est + q * se)[need_ci]
  pd <- ifelse(is.finite(dfv), 2 * stats::pt(abs(z), pmax(dfv, 1e-300), lower.tail = FALSE),
               2 * stats::pnorm(abs(z), lower.tail = FALSE))
  p[need_p] <- pd[need_p]

  # Keys: a factor level gets a Stata key "<code>.<variable>", the code as
  # for a fitted factor without value codes (.rt_codes(): an integer label
  # is its own code, else the position among the variable's rows), so a
  # data frame and a fitted model share level rows (user item (b)). Only
  # for variables with a reference row: otherwise the base's position is
  # unknown and the R term stays the key. A `key` column overrides.
  key <- term
  positional <- character()
  grp <- paste(eq, variable, sep = "\r")
  for (g in unique(grp[is_level])) {
    ix <- which(grp == g & is_level)
    labs <- lev_label[ix]
    if (anyDuplicated(labs)) next
    codes <- .rt_codes(factor(labs, levels = labs))
    # A logical predictor (broom labels its levels FALSE/TRUE) is 0/1, as
    # for a fitted model.
    if (all(labs %in% c("FALSE", "TRUE"))) codes <- structure(c("FALSE" = "0", "TRUE" = "1")[labs], positional = FALSE)
    pos <- isTRUE(attr(codes, "positional"))
    if (pos && !any(ref[ix])) next
    key[ix] <- paste0(unname(codes[labs]), ".", variable[ix])
    if (pos) positional <- c(positional, variable[ix[1]])
  }
  key[vtype == "intercept"] <- "_cons"
  # A structural row outside an equation: its name in Stata's `/` equation
  # (`/sigma` is `sigma`; tt_regtab_build() keys it `/::sigma`); cutpoints
  # are Stata's cut1, cut2, ... in order (R's `3|4` thresholds), as a
  # fitted polr/clm keys them.
  noeq <- !nzchar(eq)
  key[anc & noeq] <- sub("^/", "", term[anc & noeq])
  cp <- which(rrole == "cutpoint" & noeq & !grepl("^/?cut[0-9]+$", term))
  if (length(cp)) key[cp] <- paste0("cut", seq_along(cp))
  # Interactions (review P2-3 of group t2a): a Stata key when every part of
  # the term is a level row of the data frame (its key) or a continuous
  # variable ("c.<name>"), factor parts first ("2.g#c.age"), as a fitted
  # model keys it; otherwise the term stays the key.
  lev_key <- stats::setNames(key[is_level], term[is_level])
  cont <- unique(c(variable[vtype == "continuous"], term[vtype == "continuous"]))
  for (i in which(vtype == "interaction" & !nzchar(eq))) {
    parts <- strsplit(term[i], ":", fixed = TRUE)[[1]]
    k <- ifelse(parts %in% names(lev_key), lev_key[parts], ifelse(parts %in% cont, paste0("c.", parts), NA_character_))
    if (length(parts) < 2L || anyNA(k)) next
    key[i] <- paste(k[order(startsWith(k, "c."))], collapse = "#")
  }
  given <- !is.na(ukey)
  key[given] <- ukey[given]
  positional <- setdiff(unique(positional), variable[given & is_level])

  row <- function(k, block, kind, label, i, sub) {
    if (status[i] == "est") {
      .rt_row(k, block, kind, label, "est", term = term[i], estimate = est[i],
              conf.low = lo[i], conf.high = hi[i], p.value = p[i], sub = sub, ancillary = anc[i],
              role = rrole[i])
    } else {
      .rt_row(k, block, kind, label, status[i], term = term[i], sub = sub, ancillary = anc[i], role = rrole[i])
    }
  }
  out <- list()
  if (!is.na(eqcol)) {
    # Multi-equation layout: no header rows, "<equation>: <label>".
    for (i in seq_len(n)) {
      out[[i]] <- row(.rt_eq_key(eq[i], key[i]), eq[i], if (is_level[i]) "level" else "var", disp[i], i, i)
    }
  } else {
    seen <- character()
    for (i in seq_len(n)) {
      if (vtype[i] == "intercept") {
        out[[length(out) + 1L]] <- row(key[i], "_cons", "intercept", "Intercept", i, i)
      } else if (is_level[i]) {
        v <- variable[i]
        if (!v %in% seen) {
          seen <- c(seen, v)
          hl <- if (!is.na(vlab[i]) && nzchar(vlab[i])) vlab[i] else v
          out[[length(out) + 1L]] <- .rt_row(v, v, "cat_header", hl, "header", sub = i - 0.5)
        }
        out[[length(out) + 1L]] <- row(key[i], v, "level", disp[i], i, i)
      } else {
        out[[length(out) + 1L]] <- row(key[i], key[i], "var", disp[i], i, i)
      }
    }
  }
  res <- if (length(out)) do.call(rbind, out) else .rt_row(character(), character(), character(), character())
  res <- res[order(res$kind == "intercept", res$sub), , drop = FALSE]
  rownames(res) <- NULL
  if (anyDuplicated(res$key)) {
    cli::cli_abort(c("The data frame passed to {.fn regtab} repeats a row key.",
                     "x" = "Repeated: {.val {unique(res$key[duplicated(res$key)])}}.",
                     "i" = "Keys come from {.field key}, else a level's code and {.field variable}, else {.field term}; give each equation's rows an {.field equation} column."),
         call = NULL)
  }
  attr(res, "positional") <- positional
  res
}

# Stata tabtools shows e(F) only for these commands (`regtab.ado:2983-2988`).
.rt_F_cmds <- c("regress", "anova", "areg", "xtreg", "ivregress", "cnsreg")

#' @export
tt_model_stats.data.frame <- function(fit, info, ...) {
  s <- list(N = NA_real_, N_sub = NA_real_, ll = NA_real_, rank = NA_real_,
            aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_,
            r2_a = NA_real_, groups = NA_real_, qic = NA_real_, icc = NA_real_)
  g <- attr(fit, "glance", exact = TRUE)
  if (is.null(g)) return(s)
  .rt_glance_check(g, "The data frame passed to {.fn tt_model_stats}")
  g <- as.list(g)
  num <- function(nm) {
    v <- g[[nm]]
    if (is.null(v) || !length(v)) NA_real_ else as.numeric(v[[1]])
  }
  s$N <- num("nobs")
  s$N_sub <- num("n_sub")
  s$ll <- num("logLik")
  s$rank <- num("df")
  s$r2 <- num("r.squared")
  s$r2_a <- num("adj.r.squared")
  s$r2_p <- num("pseudo.r.squared")
  s$groups <- num("groups")
  s$icc <- num("icc")
  s$qic <- num("qic")
  s$aic <- num("AIC")
  s$bic <- num("BIC")
  # The stats tokens of tabtools 2.1.12 (task 5.17); F for regress-type
  # commands only, Stata's rule.
  s$events <- num("nevent")
  s$rmse <- num("rmse")
  s$F <- if (tolower(trimws(info$stata_cmd %||% "")) %in% .rt_F_cmds) num("F") else NA_real_
  s$mi_m <- num("nimp")
  s$fmi <- num("fmi")
  if (!is.na(s$ll) && !is.na(s$rank)) {
    s$aic <- -2 * s$ll + 2 * s$rank
    if (!is.na(s$N)) s$bic <- -2 * s$ll + s$rank * log(s$N)
  }
  s
}
