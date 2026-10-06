# effecttab inputs (plan task 7.7): marginaleffects results, data frames
# and matrices become one long data frame of effect rows. Stata's effecttab
# reads a collection of teffects or margins results (effecttab.ado:224-251,
# :457-540) or a matrix (from(), :629-679); it fits nothing, and neither
# does R (decision EO4). marginaleffects is used at run time only (it is in
# Suggests), through its documented columns and components() alone.

#' Effect rows of a marginaleffects result, data frame, or matrix
#'
#' The input stage of [effecttab()]: one row per table row of one result,
#' with the identity Stata would give it (its row key), the numbers, and
#' what the labels are built from. [effecttab()] calls it for every piece of
#' every model; it is exported so the rows can be inspected, and extended to
#' other classes with an S3 method.
#'
#' @param x A marginaleffects result (`predictions`, `comparisons`,
#'   `slopes`), a data frame, or a numeric matrix. See [effecttab()] for
#'   what each may hold.
#' @param type `"teffects"` (treatment contrasts and potential-outcome
#'   means) or `"margins"` (predictive margins and marginal effects).
#' @param data Optional data frame whose columns supply variable labels
#'   and value labels (Stata reads them from the data in memory). For a
#'   marginaleffects result it defaults to the model's data.
#' @param ... For a data frame, `level` states the confidence level used to
#'   derive Wald intervals from `std.error`, as a proportion or percentage
#'   (see [effecttab()]); it must agree with any recorded level. Other
#'   methods do not use additional arguments.
#' @return A data frame with one row per table row: `key` (the row's
#'   identity, as Stata's collection names it: `r1vs0.mbsmoke`,
#'   `0.mbsmoke`, `2.race`, `age`), `kind` (`"contrast"`, `"pomean"`,
#'   `"prediction"`, `"slope"`, `"level"`, `"ref"`), `variable`, `level`
#'   and `base` (level codes as text), `level_text` and `base_text` (the
#'   text the level is shown with: its value label from `data`; for a
#'   marginaleffects result without one, the level itself or its code; `NA`
#'   for data-frame or matrix input whose level has no value label),
#'   `var_label`, `parent` (the key of the variable heading a factor level,
#'   `NA` otherwise), `label` (a label fixed by the input, e.g. a matrix row
#'   name, else `NA`), `estimate`, `conf.low`, `conf.high`, `p.value`,
#'   `status` (`"est"`, `"base"`, `"omit"`, `"empty"`), and `positional`
#'   (`TRUE` when `level`/`base` are level positions rather than Stata
#'   codes: a plain factor's or character variable's levels whose text is
#'   not an integer). Attributes: `source` (`"marginaleffects"`,
#'   `"data.frame"`, `"matrix"`), `level` (the confidence level of the
#'   intervals in percent, `NA` when the input does not record it),
#'   `estimator` (a teffects estimator such as `"ipw"`, or `NULL`),
#'   `model_id`, `estimand` (`"ATE"`, `"ATET"` or `"ATC"` when the input
#'   names one, else `NULL`), `notes` (table notes the input implies, such
#'   as the null a p-value tests, possibly empty), and, for data-frame
#'   input, `p_null_default` (`TRUE` when p-values were derived from the
#'   standard errors against the default null 0).
#' @seealso [effecttab()]
#' @examples
#' # Stata's r(table) of `teffects ipw ..., ate` as a data frame
#' rt <- data.frame(equation = c("ATE", "POmean"),
#'                  term = c("r1vs0.mbsmoke", "0.mbsmoke"),
#'                  estimate = c(-262.98, 3406.38),
#'                  conf.low = c(-307.69, 3387.86), conf.high = c(-218.27, 3424.90),
#'                  p.value = c(9.5e-31, 0))
#' tt_effect_rows(rt, type = "teffects")
#' @export
tt_effect_rows <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  UseMethod("tt_effect_rows")
}

#' @export
tt_effect_rows.default <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  cli::cli_abort(c(
    "{.fn effecttab} cannot read {.obj_type_friendly {x}}.",
    "i" = "Pass a marginaleffects result ({.fn avg_predictions}, {.fn avg_comparisons}, {.fn avg_slopes}), a data frame with {.field term} and {.field estimate} columns, or a matrix with columns estimate, lower bound, upper bound, p-value."
  ), call = NULL)
}

# ---------------------------------------------------------------------------
# Shared pieces

.et_cols <- c("key", "kind", "variable", "level", "base", "level_text", "base_text", "var_label",
              "parent", "label", "estimate", "conf.low", "conf.high", "p.value", "status", "positional")

# One block of rows; every argument is recycled to n.
.et_rows <- function(n, key, kind, variable = NA_character_, level = NA_character_, base = NA_character_,
                     level_text = NA_character_, base_text = NA_character_, var_label = NA_character_,
                     parent = NA_character_, label = NA_character_, estimate = NA_real_,
                     conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, status = "est",
                     positional = FALSE) {
  r <- function(v) rep_len(v, n)
  data.frame(key = r(as.character(key)), kind = r(kind), variable = r(as.character(variable)),
             level = r(as.character(level)), base = r(as.character(base)),
             level_text = r(as.character(level_text)), base_text = r(as.character(base_text)),
             var_label = r(as.character(var_label)), parent = r(as.character(parent)),
             label = r(as.character(label)), estimate = r(as.numeric(estimate)),
             conf.low = r(as.numeric(conf.low)), conf.high = r(as.numeric(conf.high)),
             p.value = r(as.numeric(p.value)), status = r(status), positional = r(as.logical(positional)),
             stringsAsFactors = FALSE)
}

.et_empty_rows <- function() .et_rows(0L, character(), character())

# `a` unless it is NULL, NA or "", else `b`.
.et_or <- function(a, b) if (is.null(a) || length(a) != 1L || is.na(a) || !nzchar(a)) b else a

# Display text of a level code of a variable (Stata `: label (var) code`,
# the code itself when the value has no label).
.et_code_text <- function(col, code) {
  if (is.null(col) || is.na(code)) return(NA_character_)
  if (is.factor(col)) {
    vl <- attr(col, "labels", exact = TRUE)
    if (is.numeric(vl) && !is.null(names(vl))) {
      hit <- match(code, stata_macro_text(unname(unclass(vl))))
      return(if (is.na(hit)) code else names(vl)[hit])
    }
    k <- suppressWarnings(as.integer(code))
    return(if (!is.na(k) && k >= 1L && k <= nlevels(col)) levels(col)[k] else code)
  }
  if (is.logical(col)) return(switch(code, "0" = "FALSE", "1" = "TRUE", code))
  if (is.character(col)) {
    u <- unique(col[!is.na(col)])
    u <- u[order(u, method = "radix")]
    k <- suppressWarnings(as.integer(code))
    return(if (!is.na(k) && k >= 1L && k <= length(u)) u[k] else code)
  }
  vl <- attr(col, "labels", exact = TRUE)
  num <- suppressWarnings(as.numeric(code))
  if (!is.null(vl) && length(vl) && is.numeric(vl) && !is.na(num)) {
    hit <- match(num, as.numeric(unclass(vl)))
    if (!is.na(hit)) return(names(vl)[hit])
  }
  code
}

.et_finish <- function(rows, source, level = NA_real_, estimator = NULL, model_id = "", estimand = NULL,
                       notes = character()) {
  rownames(rows) <- NULL
  attr(rows, "source") <- source
  attr(rows, "level") <- level
  attr(rows, "estimator") <- estimator
  attr(rows, "model_id") <- model_id
  attr(rows, "estimand") <- estimand
  attr(rows, "notes") <- notes
  rows
}

# A variable's label and column in `data` (Stata: `: variable label`, the
# name when there is none; `exists` is Stata's `capture confirm variable`).
.et_var <- function(data, v) {
  col <- if (is.data.frame(data) && !is.na(v) && v %in% names(data)) data[[v]] else NULL
  lab <- if (!is.null(col)) attr(col, "label", exact = TRUE) else NULL
  ok <- is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)
  list(exists = !is.null(col), col = col, label = if (ok) lab else v)
}

# Level codes and display text for values of a variable, as Stata keys and
# labels factor levels: a haven-labelled or numeric variable is keyed by its
# value (`2.race`) and shows the value label, or the value when unlabelled
# (`: label (race) 2`); a factor made by tt_as_factor() keeps its Stata
# codes; any other factor and a character variable are keyed as regtab's
# .rt_codes() keys them (an integer level text by its value, other levels by
# position, .et_level_codes()), a logical by 0/1, and show the level. Without
# the variable, integer values are their own codes and anything else is
# keyed by order of appearance.
.et_levels <- function(col, values) {
  values <- as.character(values)
  n <- length(values)
  code <- rep(NA_character_, n)
  text <- values
  if (is.null(col)) {
    num <- suppressWarnings(as.numeric(values))
    int <- !is.na(num) & is.finite(num) & num == round(num)
    code[int] <- stata_macro_text(num[int])
    u <- unique(values[!int])
    code[!int] <- as.character(match(values[!int], u))
    return(list(code = code, text = text, positional = !int))
  }
  positional <- rep(FALSE, n)
  if (is.factor(col) || is.character(col)) {
    if (is.factor(col)) {
      lev <- levels(col)
    } else {
      lev <- unique(col[!is.na(col)])
      lev <- lev[order(lev, method = "radix")]
    }
    lc <- .et_level_codes(lev, attr(col, "labels", exact = TRUE))
    pos <- match(values, lev)
    code <- lc$code[pos]
    positional <- !is.na(pos) & lc$positional[pos]
  } else if (is.logical(col)) {
    code <- ifelse(values %in% c("TRUE", "1"), "1", ifelse(values %in% c("FALSE", "0"), "0", NA_character_))
    text <- ifelse(code == "1", "TRUE", ifelse(code == "0", "FALSE", values))
  } else if (is.numeric(col)) {
    num <- suppressWarnings(as.numeric(values))
    code <- ifelse(is.na(num), NA_character_, stata_macro_text(num))
    text <- code
    vl <- attr(col, "labels", exact = TRUE)
    if (!is.null(vl) && length(vl) && is.numeric(vl)) {
      hit <- match(num, as.numeric(unclass(vl)), incomparables = NA)
      text[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
    }
  }
  # A value the variable does not hold keeps its own text as the code.
  miss <- is.na(code)
  code[miss] <- values[miss]
  text[is.na(text)] <- values[is.na(text)]
  list(code = code, text = text, positional = positional)
}

# Codes of a factor's (or a character variable's sorted) levels, as
# regtab's .rt_codes() gives them (audit 2026-09-29 B03, B06;
# .rt_level_codes()): a Stata value label's code ("labels" attribute,
# tt_as_factor()), else a level whose text is an integer is that integer
# (factor(0:2) is 0, 1, 2, as Stata teffects keys `r1vs0.dose`), else the
# level's position, flagged positional; when values and positions would
# give two levels one code, the unlabelled levels keep their positions.
.et_level_codes <- function(lev, vl = NULL) .rt_level_codes(lev, vl)

# Whether a variable is categorical in the model (Stata's i. prefix): a
# factor, character or logical column. Numeric columns, value-labelled or
# not, are continuous, as `dydx(x)` without i. treats them.
.et_categorical <- function(col) is.factor(col) || is.character(col) || is.logical(col)

# Split a marginaleffects contrast ("SNRI - SSRI", "mean(1) - mean(0)",
# "mean(Black) / mean(White)") into its two levels; NULL when the contrast
# is not a difference or ratio of two levels ("dY/dX", "+1", "sd"). With
# the variable's levels known, the split is the one whose two sides are
# levels (a level may itself contain " - ").
.et_split_contrast <- function(s, known = NULL) {
  s <- trimws(s)
  # lnratio/lnor contrasts: "ln(mean(1) / mean(0))", "ln(odds(1) / odds(0))".
  s <- sub("^ln\\((.*)\\)$", "\\1", s)
  for (op in c(" - ", " / ")) {
    at <- gregexpr(op, s, fixed = TRUE)[[1]]
    if (at[1] < 0) next
    for (k in at) {
      a <- trimws(substr(s, 1L, k - 1L))
      b <- trimws(substring(s, k + nchar(op)))
      a <- sub("^(mean|odds)\\((.*)\\)$", "\\2", a)
      b <- sub("^(mean|odds)\\((.*)\\)$", "\\2", b)
      if (!nzchar(a) || !nzchar(b)) next
      if (is.null(known) || (a %in% known && b %in% known)) return(c(level = a, base = b))
    }
  }
  NULL
}

# The text forms a variable's values take in marginaleffects output (R's
# own as.character(), which is how the contrast strings name them).
.et_known_levels <- function(col) {
  if (is.null(col)) return(NULL)
  if (is.factor(col)) return(levels(col))
  if (is.logical(col)) return(c("FALSE", "TRUE"))
  if (is.character(col)) return(unique(col[!is.na(col)]))
  v <- unique(as.numeric(unclass(col)))
  as.character(v[!is.na(v)])
}

# ---------------------------------------------------------------------------
# marginaleffects results

.et_me_component <- function(x, what) {
  f <- tryCatch(getExportedValue("marginaleffects", "components"), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  tryCatch(suppressWarnings(f(x, what)), error = function(e) NULL)
}

# The confidence level of a result (percent), NA when it is not recorded.
.et_me_level <- function(x) {
  cl <- .et_me_component(x, "conf_level")
  if (is.numeric(cl) && length(cl) == 1L && is.finite(cl) && cl > 0 && cl < 1) round(cl * 100, 10) else NA_real_
}

# The data the labels come from: `data`, else the model's data.
.et_me_data <- function(x, data) {
  if (!is.null(data)) return(data)
  md <- .et_me_component(x, "modeldata")
  if (is.data.frame(md)) md else NULL
}

# The order the variables were requested in (`variables = c("b", "a")`):
# avg_slopes() and avg_comparisons() sort terms alphabetically, Stata lists
# dydx() terms as typed. Read from the recorded call's `variables` when it
# is written out (a character vector, or c()/list() of strings or named
# entries); nothing is evaluated, so `variables = vars` keeps
# marginaleffects' order.
.et_me_request_order <- function(x) {
  cl <- .et_me_component(x, "call")
  v <- if (is.call(cl)) cl$variables else NULL
  .et_literal_names(v)
}

.et_literal_names <- function(v) {
  if (is.character(v)) return(v)
  if (!is.call(v) || !is.name(v[[1]]) || !as.character(v[[1]]) %in% c("c", "list")) return(NULL)
  args <- as.list(v)[-1L]
  nm <- names(args) %||% rep("", length(args))
  out <- character()
  for (i in seq_along(args)) {
    if (nzchar(nm[i])) out <- c(out, nm[i])
    else if (is.character(args[[i]])) out <- c(out, args[[i]])
    else return(NULL)
  }
  out
}

# A model identity for the frame characteristics (Stata stores the command
# line): the marginaleffects function with its `variables` and `by`.
.et_me_model_id <- function(x) {
  cl <- .et_me_component(x, "call")
  if (!is.call(cl)) return(class(x)[1])
  args <- Filter(Negate(is.null), list(variables = cl$variables, by = cl$by))
  inner <- vapply(names(args), function(a) paste0(a, " = ", paste(deparse(args[[a]], width.cutoff = 500L), collapse = "")), "")
  paste0(sub("^.*::", "", paste(deparse(cl[[1]]), collapse = "")), "(", paste(inner, collapse = ", "), ")")
}

# Columns every marginaleffects result may carry that are not variables.
.et_me_stat_cols <- c("rowid", "rowidcf", "term", "contrast", "estimate", "std.error", "statistic",
                      "p.value", "s.value", "conf.low", "conf.high", "df", "predicted", "predicted_lo",
                      "predicted_hi", "hypothesis", "marginaleffects_wts_internal")

.et_me_numbers <- function(x) {
  num <- function(v) if (v %in% names(x)) as.numeric(x[[v]]) else rep(NA_real_, nrow(x))
  list(estimate = num("estimate"), conf.low = num("conf.low"), conf.high = num("conf.high"),
       p.value = num("p.value"), std.error = num("std.error"), df = num("df"))
}

.et_me_check <- function(x, what) {
  if ("rowid" %in% names(x)) {
    # Predictions at a grid of values (`newdata = datagrid(...)`, Stata's
    # `margins, at()`) are not unit-level results (audit 2026-09-29 B05).
    cl <- .et_me_component(x, "call")
    if (is.call(cl) && !is.null(cl$newdata)) {
      cli::cli_abort(c(
        "{.fn effecttab} formats averaged results, and this {.cls {what}} result has one row per row of its {.arg newdata} (a grid of values, such as {.fn datagrid} makes).",
        "i" = "Average over the grid with {.code avg_{what}(fit, newdata = datagrid(...), by = ...)}, one row per value of {.arg by} (Stata's {.code margins, at()}), or pass the rows as a data frame."
      ), call = NULL)
    }
    cli::cli_abort(c(
      "{.fn effecttab} formats averaged results, and this {.cls {what}} result has one row per observation.",
      "i" = "Use {.fn avg_{what}} (Stata's {.code margins} averages over the sample)."
    ), call = NULL)
  }
  if (!"estimate" %in% names(x)) cli::cli_abort("The {.cls {what}} result has no {.field estimate} column.", call = NULL)
}

# A result's numeric `hypothesis` (the null its p-values test), NULL for
# none (marginaleffects then tests 0).
.et_me_null <- function(x) {
  cl <- .et_me_component(x, "call")
  h <- if (is.call(cl)) cl$hypothesis else NULL
  if (is.null(h)) return(NULL)
  if (is.numeric(h) && length(h) == 1L && is.finite(h)) return(h)
  # `hypothesis = h`, a variable: the null marginaleffects recorded.
  hn <- if ("hypothesis" %in% names(x)) NULL else .et_me_component(x, "hypothesis_null")
  if (is.numeric(hn) && length(hn) == 1L && is.finite(hn)) hn else NULL
}

# A note for the footnote when a result's p-values test a null other than
# the table's (0 for differences, 1 for ratios).
.et_null_note <- function(h, ratio) {
  if (is.null(h) || (!ratio && h == 0) || (ratio && h == 1)) return(character())
  paste0("P-values test a null value of ", .tt_fmt_sig(h, 15L), ".")
}

# teffects' variance is a sandwich; a regression-adjustment result from an
# lm()/glm() fit with its default (model-based) variance gives narrower
# intervals (golden E12). Said once per session.
.et_me_vcov_note <- function(x) {
  model <- .et_me_component(x, "model")
  cl <- .et_me_component(x, "call")
  if (is.null(model) || !class(model)[1] %in% c("lm", "glm") || !is.call(cl)) return(invisible())
  v <- cl$vcov
  if (!is.null(v) && !isTRUE(v)) return(invisible())
  cli::cli_inform(c(
    "i" = "This {.cls {class(model)[1]}} result uses the model's default variance; Stata's {.code teffects ra} reports a robust (sandwich) variance.",
    " " = "Pass {.code vcov = \"HC0\"} to marginaleffects to match it ({.code ?effecttab}, Differences from Stata)."
  ), .frequency = "once", .frequency_id = "tabtools_effecttab_vcov")
  invisible()
}

# The teffects estimator and estimand of a WeightIt::glm_weightit() result
# with logistic propensity-score weights (method "glm"): IPW for y ~ t,
# IPWRA when the outcome model has covariates; ATE, ATET or ATC.
.et_me_teffects_info <- function(x) {
  model <- .et_me_component(x, "model")
  w <- if (inherits(model, "glm_weightit")) model$weightit else NULL
  if (!is.list(w) || !identical(as.character(w$method)[1], "glm")) return(list())
  tv <- attr(w$treat, "treat.name")
  rhs <- tryCatch(all.vars(stats::formula(model)[[3L]]), error = function(e) NULL)
  est <- if (is.null(rhs) || is.null(tv)) NULL else if (identical(rhs, tv)) "ipw" else "ipwra"
  estimand <- switch(w$estimand %||% "", ATE = "ATE", ATT = "ATET", ATC = "ATC", NULL)
  list(estimator = est, estimand = estimand)
}

# Rows of a result with a `hypothesis` column (a `hypothesis =` test, or
# hypotheses()): one row each, labelled by the hypothesis, never by the
# rows it combines ("b2 - b1", "Man - Auto").
.et_hyp_label <- function(h) {
  h <- trimws(as.character(h))
  h <- sub("\\s*=\\s*0$", "", h)
  plain <- !grepl(" ", h, fixed = TRUE)
  h[plain] <- gsub("([[:alnum:])_.])([-+=])([[:alnum:](_.])", "\\1 \\2 \\3", h[plain])
  pair <- grepl("^\\([^()]*\\) [-/] \\([^()]*\\)$", h)
  h[pair] <- gsub("\\(([^()]*)\\)", "\\1", h[pair])
  h
}

.et_me_hypothesis_rows <- function(x, type, what) {
  if (type == "teffects") {
    cli::cli_abort(c("A {.arg hypothesis} test is neither a treatment contrast nor a potential-outcome mean.",
                     "i" = "Use {.code type = \"margins\"}, or pass the result without {.arg hypothesis}."),
                   call = NULL)
  }
  num <- .et_me_numbers(x)
  h <- as.character(x$hypothesis)
  lab <- .et_hyp_label(h)
  if (anyDuplicated(h)) cli::cli_abort("The {.cls {what}} result repeats a hypothesis.", call = NULL)
  rows <- .et_rows(nrow(x), key = h, kind = "slope", label = lab, estimate = num$estimate,
                   conf.low = num$conf.low, conf.high = num$conf.high, p.value = num$p.value)
  .et_finish(rows, "marginaleffects", .et_me_level(x), model_id = .et_me_model_id(x))
}

#' @export
tt_effect_rows.predictions <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  type <- match.arg(type)
  .et_me_check(x, "predictions")
  if ("hypothesis" %in% names(x)) return(.et_me_hypothesis_rows(x, type, "predictions"))
  md <- .et_me_data(x, NULL)
  data <- data %||% md
  num <- .et_me_numbers(x)
  extra <- setdiff(names(x), .et_me_stat_cols)
  # Focal and `by` variables: the extra columns that are model variables.
  focal <- if (is.data.frame(md)) intersect(extra, names(md)) else extra
  n <- nrow(x)
  lev <- .et_me_level(x)
  notes <- .et_null_note(.et_me_null(x), FALSE)
  if (!length(focal)) {
    if (type == "teffects") {
      cli::cli_abort("A potential-outcome mean needs the treatment level: use {.code avg_predictions(fit, variables = \"<treatment>\")}.",
                     call = NULL)
    }
    if (n != 1L) cli::cli_abort("A {.cls predictions} result without a focal variable must have one row.", call = NULL)
    rows <- .et_rows(1L, key = "_cons", kind = "prediction", label = "Overall", estimate = num$estimate,
                     conf.low = num$conf.low, conf.high = num$conf.high, p.value = num$p.value)
    return(.et_finish(rows, "marginaleffects", lev, model_id = .et_me_model_id(x), notes = notes))
  }
  info <- lapply(focal, function(v) {
    vi <- .et_var(data, v)
    c(vi, .et_levels(vi$col, as.character(x[[v]])))
  })
  names(info) <- focal
  if (type == "teffects") {
    if (length(focal) != 1L) {
      cli::cli_abort("Potential-outcome means take one treatment variable; this {.cls predictions} result has {length(focal)} ({.var {focal}}).",
                     call = NULL)
    }
    .et_me_vcov_note(x)
    te <- .et_me_teffects_info(x)
    v <- focal
    i <- info[[1]]
    rows <- .et_rows(n, key = paste0(i$code, ".", v), kind = "pomean", variable = v, level = i$code,
                     level_text = i$text, var_label = i$label, estimate = num$estimate,
                     conf.low = num$conf.low, conf.high = num$conf.high, p.value = num$p.value,
                     positional = i$positional)
    return(.et_finish(rows, "marginaleffects", lev, estimator = te$estimator, model_id = .et_me_model_id(x),
                      estimand = te$estimand, notes = notes))
  }
  # margins: the focal variable heads its values (a factor's levels, or the
  # values at() sets); several variables (by = c("sex", "highbp")) head
  # their combinations, joined by "#" as Stata's margins output does.
  if (length(focal) == 1L) {
    v <- focal
    i <- info[[1]]
    rows <- .et_rows(n, key = paste0(i$code, ".", v), kind = "level", variable = v, level = i$code,
                     level_text = i$text, var_label = i$label, parent = v, estimate = num$estimate,
                     conf.low = num$conf.low, conf.high = num$conf.high, p.value = num$p.value,
                     positional = i$positional)
  } else {
    key <- do.call(paste, c(lapply(focal, function(v) paste0(info[[v]]$code, ".", v)), sep = "#"))
    text <- do.call(paste, c(lapply(focal, function(v) info[[v]]$text), sep = "#"))
    parent <- paste(focal, collapse = "#")
    rows <- .et_rows(n, key = key, kind = "level", variable = parent, level = key, level_text = text,
                     var_label = parent, parent = parent, estimate = num$estimate, conf.low = num$conf.low,
                     conf.high = num$conf.high, p.value = num$p.value,
                     positional = Reduce(`|`, lapply(focal, function(v) info[[v]]$positional)))
  }
  if (anyDuplicated(rows$key)) {
    cli::cli_abort("The {.cls predictions} result repeats a combination of {.var {focal}}; pass one row per level.",
                   call = NULL)
  }
  .et_finish(rows, "marginaleffects", lev, model_id = .et_me_model_id(x), notes = notes)
}

# Contrasts shown as the variable alone (Stata's dydx(x) row): the
# derivative, and the discrete change of a 0/1 variable; any other
# contrast ("+1", "sd", "+10") is appended to the label.
.et_plain_contrasts <- c("", "dY/dX", "mean(dY/dX)", "1 - 0", "mean(1) - mean(0)")

# Rows whose contrast is a plain ratio of two levels ("mean(1) / mean(0)",
# comparison = "ratio"), not a log ratio ("ln(mean(1) / mean(0))").
.et_is_ratio <- function(contrast) grepl(" / ", contrast, fixed = TRUE) & !grepl("^ln\\(", contrast)

.et_me_contrast_rows <- function(x, type, data, what) {
  .et_me_check(x, what)
  if ("hypothesis" %in% names(x)) return(.et_me_hypothesis_rows(x, type, what))
  if (!"term" %in% names(x)) cli::cli_abort("The {.cls {what}} result has no {.field term} column.", call = NULL)
  data <- .et_me_data(x, data)
  extra <- setdiff(names(x), .et_me_stat_cols)
  if (length(extra)) {
    cli::cli_abort(c(
      "{.fn effecttab} does not read {.cls {what}} results split by {.var {extra}} ({.arg by}).",
      "i" = "Pass one result per subgroup as separate models, or a data frame with a {.field label} column."
    ), call = NULL)
  }
  num <- .et_me_numbers(x)
  terms <- unique(as.character(x$term))
  want <- .et_me_request_order(x)
  if (!is.null(want)) terms <- c(intersect(want, terms), setdiff(terms, want))
  contrast <- if ("contrast" %in% names(x)) as.character(x$contrast) else rep("", nrow(x))
  contrast[is.na(contrast)] <- ""
  vclass <- .et_me_component(x, "variable_class")
  if (!is.character(vclass) || is.null(names(vclass))) vclass <- NULL
  h <- .et_me_null(x)
  ratio <- .et_is_ratio(contrast)
  notes <- .et_null_note(h, any(ratio))
  # A plain ratio's p-value must test a ratio of 1: marginaleffects tests
  # every estimate against 0 unless `hypothesis` is given. The Wald test is
  # recomputed from its delta-method standard error (on the ratio scale),
  # z = (ratio - 1) / se, t with the result's df when finite; the interval
  # is kept as marginaleffects gives it. A result that states its null
  # (`hypothesis = 2`) keeps marginaleffects' test of that null, and the
  # footnote names only it (codex audit F06: the table claimed both nulls
  # for one p-value).
  if (any(ratio) && is.null(h)) {
    se <- num$std.error[ratio]
    unusable <- is.finite(num$p.value[ratio]) & (!is.finite(se) | se <= 0)
    if (any(unusable)) {
      cli::cli_abort(c("A plain ratio's p-value cannot be tested against 1 because its ratio-scale standard error is unavailable.",
                       "i" = "Use {.code comparison = \"lnratioavg\", transform = exp}, or specify {.code hypothesis = 1} when computing the plain ratio, before transforming it."),
                     class = "tabtools_error_ratio_inference", call = NULL)
    }
    df <- num$df[ratio]
    stat <- (num$estimate[ratio] - 1) / se
    p <- ifelse(is.finite(df) & df > 0, 2 * stats::pt(-abs(stat), pmax(df, 1e-300)), 2 * stats::pnorm(-abs(stat)))
    p[!is.finite(se) | se <= 0] <- NA_real_
    num$p.value[ratio] <- p
    if (any(is.finite(p))) notes <- c(notes, "P-values of ratios test a ratio of 1.")
  }
  # The delta-method interval of a plain ratio is symmetric on the ratio
  # scale and can reach below 0, which no ratio can be (Muse audit P1-28).
  # Kept as marginaleffects (and Stata's nlcom) gives it, with a warning and
  # a footnote naming the log-scale route.
  if (any(ratio & is.finite(num$conf.low) & num$conf.low <= 0)) {
    cli::cli_warn(c("A ratio's confidence interval reaches below 0 ({.code comparison = \"ratio\"} gives a symmetric delta-method interval).",
                    "i" = "Use {.code comparison = \"lnratioavg\", transform = exp} for an interval on the log scale."),
                  class = "tabtools_warning_ratio_interval", call = NULL)
    notes <- c(notes, paste("A ratio's interval reaches below 0; comparison = \"lnratioavg\" with",
                            "transform = exp gives one on the log scale."))
  }
  if (type == "teffects") {
    .et_me_vcov_note(x)
    te <- .et_me_teffects_info(x)
  } else te <- list()
  out <- list()
  for (t in terms) {
    idx <- which(as.character(x$term) == t)
    vi <- .et_var(data, t)
    known <- .et_known_levels(vi$col)
    sp <- lapply(contrast[idx], .et_split_contrast, known = known)
    vc <- if (!is.null(vclass) && t %in% names(vclass)) vclass[[t]] else NA_character_
    if (type == "teffects") {
      bad <- vapply(sp, is.null, TRUE)
      if (any(bad)) {
        cli::cli_abort(c(
          "A treatment contrast must compare two levels of {.var {t}}, not {.val {contrast[idx][bad][1]}}.",
          "i" = "Use {.code avg_comparisons(fit, variables = \"{t}\")} (a difference), or {.code comparison = \"lnratioavg\", transform = exp} (a ratio); slopes have no teffects counterpart."
        ), call = NULL)
      }
      lv <- .et_levels(vi$col, vapply(sp, `[[`, "", "level"))
      bs <- .et_levels(vi$col, vapply(sp, `[[`, "", "base"))
      rows <- .et_rows(
        length(idx), key = paste0("r", lv$code, "vs", bs$code, ".", t), kind = "contrast", variable = t,
        level = lv$code, base = bs$code, level_text = lv$text, base_text = bs$text, var_label = vi$label,
        estimate = num$estimate[idx], conf.low = num$conf.low[idx], conf.high = num$conf.high[idx],
        p.value = num$p.value[idx], positional = lv$positional)
      # Stata lists a multi-valued treatment's contrasts in level order
      # (r1vs0, r2vs0, r3vs0); marginaleffects sorts them by label.
      codes <- suppressWarnings(as.numeric(rows$level))
      if (!anyNA(codes)) rows <- rows[order(codes), , drop = FALSE]
      out[[length(out) + 1L]] <- rows
      next
    }
    # How the model treats the variable (marginaleffects' variable_class:
    # a factor() in the formula over a numeric column is categorical), else
    # the column's type, else whether the contrast names non-numeric levels.
    categorical <- if (!is.na(vc)) vc %in% c("factor", "character", "logical") else if (vi$exists) .et_categorical(vi$col) else
      all(!vapply(sp, is.null, TRUE)) && anyNA(suppressWarnings(as.numeric(unlist(sp))))
    if (!categorical || any(vapply(sp, is.null, TRUE))) {
      if (identical(what, "slopes") && identical(vc, "binary") && any(contrast[idx] %in% c("1 - 0", "mean(1) - mean(0)"))) {
        cli::cli_inform(c(
          "i" = "{.var {t}} is a 0/1 variable: marginaleffects reports its discrete change (1 - 0), as Stata's {.code dydx(i.{t})} does.",
          " " = "Stata's {.code dydx({t})} without {.code i.} is the derivative instead, which can differ ({.code ?effecttab}, Differences from Stata)."
        ), .frequency = "once", .frequency_id = "tabtools_effecttab_binary")
      }
      # A continuous variable (dydx(x)): one row, labelled by the variable;
      # a contrast other than the derivative (or a 0/1 variable's change)
      # is named after it, and several contrasts of one variable are told
      # apart by it. The key carries a named contrast too, so the +1 and
      # +2 contrasts of two models are two rows, not one (codex audit F01).
      many <- length(idx) > 1L
      named <- many | !contrast[idx] %in% .et_plain_contrasts
      out[[length(out) + 1L]] <- .et_rows(
        length(idx), key = ifelse(named, paste0(t, ":", contrast[idx]), t), kind = "slope", variable = t,
        var_label = vi$label, label = ifelse(named, paste0(vi$label, " (", contrast[idx], ")"), NA_character_),
        estimate = num$estimate[idx], conf.low = num$conf.low[idx], conf.high = num$conf.high[idx],
        p.value = num$p.value[idx])
      next
    }
    # A factor (dydx(i.f)): the base level is a Reference row and each
    # contrast a level row, in level-code order, under the variable.
    lv <- .et_levels(vi$col, vapply(sp, `[[`, "", "level"))
    bs <- .et_levels(vi$col, vapply(sp, `[[`, "", "base"))
    if (length(unique(bs$code)) == 1L) {
      rows <- rbind(
        .et_rows(1L, key = paste0(bs$code[1], ".", t), kind = "ref", variable = t, level = bs$code[1],
                 level_text = bs$text[1], var_label = vi$label, parent = t, estimate = 0, status = "base",
                 positional = bs$positional[1]),
        .et_rows(length(idx), key = paste0(lv$code, ".", t), kind = "level", variable = t, level = lv$code,
                 level_text = lv$text, var_label = vi$label, parent = t, estimate = num$estimate[idx],
                 conf.low = num$conf.low[idx], conf.high = num$conf.high[idx], p.value = num$p.value[idx],
                 positional = lv$positional))
      codes <- suppressWarnings(as.numeric(rows$level))
      if (!anyNA(codes)) rows <- rows[order(codes), , drop = FALSE]
    } else {
      # Contrasts against several levels (pairwise): no single reference.
      # (base_text and positional let .et_join_levels() join them by level.)
      rows <- .et_rows(length(idx), key = paste0(lv$code, "vs", bs$code, ".", t), kind = "level", variable = t,
                       level = lv$code, base = bs$code, level_text = paste(lv$text, "vs", bs$text),
                       base_text = bs$text, var_label = vi$label, parent = t, estimate = num$estimate[idx],
                       conf.low = num$conf.low[idx], conf.high = num$conf.high[idx], p.value = num$p.value[idx],
                       positional = lv$positional | bs$positional)
    }
    out[[length(out) + 1L]] <- rows
  }
  rows <- if (length(out)) do.call(rbind, out) else .et_empty_rows()
  if (anyDuplicated(rows$key)) {
    cli::cli_abort("The {.cls {what}} result repeats a contrast; pass one row per contrast.", call = NULL)
  }
  .et_finish(rows, "marginaleffects", .et_me_level(x), estimator = te$estimator, model_id = .et_me_model_id(x),
             estimand = te$estimand, notes = unique(notes))
}

#' @export
tt_effect_rows.comparisons <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  .et_me_contrast_rows(x, match.arg(type), data, "comparisons")
}

#' @export
tt_effect_rows.slopes <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  type <- match.arg(type)
  if (type == "teffects") {
    cli::cli_abort(c("Slopes have no teffects counterpart.",
                     "i" = "Use {.code avg_comparisons()} for a treatment contrast, or {.code type = \"margins\"}."),
                   call = NULL)
  }
  .et_me_contrast_rows(x, type, data, "slopes")
}

# marginaleffects::hypotheses(): a row per hypothesis (or per coefficient
# when none is given), labelled by it, keeping the recorded level.
#' @export
tt_effect_rows.hypotheses <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  type <- match.arg(type)
  .et_me_check(x, "hypotheses")
  if ("hypothesis" %in% names(x)) return(.et_me_hypothesis_rows(x, type, "hypotheses"))
  if (!"term" %in% names(x)) cli::cli_abort("The {.cls hypotheses} result names no rows ({.field hypothesis} or {.field term}).", call = NULL)
  if (type == "teffects") .et_me_hypothesis_rows(x, type, "hypotheses")
  num <- .et_me_numbers(x)
  key <- make.unique(as.character(x$term), sep = "#")
  rows <- .et_rows(nrow(x), key = key, kind = "slope", label = as.character(x$term), estimate = num$estimate,
                   conf.low = num$conf.low, conf.high = num$conf.high, p.value = num$p.value)
  .et_finish(rows, "marginaleffects", .et_me_level(x), model_id = "hypotheses")
}

# ---------------------------------------------------------------------------
# Matrices (Stata's from(): columns estimate, lower, upper, p)

#' @export
tt_effect_rows.matrix <- function(x, type = c("teffects", "margins"), data = NULL, ...) {
  if (!is.numeric(x) && !is.logical(x)) {
    cli::cli_abort("A matrix passed to {.fn effecttab} must be numeric.", call = NULL)
  }
  if (ncol(x) < 4L) {
    cli::cli_abort("A matrix passed to {.fn effecttab} must have at least 4 columns (estimate, ci_lower, ci_upper, pvalue).",
                   call = NULL)
  }
  if (!nrow(x)) cli::cli_abort("The matrix passed to {.fn effecttab} has no rows.", call = NULL)
  rn <- rownames(x) %||% paste0("r", seq_len(nrow(x)))
  rn[is.na(rn) | !nzchar(rn)] <- paste0("r", which(is.na(rn) | !nzchar(rn)))
  # Row names become labels with "_" shown as a space (effecttab.ado:659);
  # the key keeps repeated names apart.
  key <- make.unique(rn, sep = "#")
  x <- matrix(as.numeric(x), nrow(x), ncol(x))
  rows <- .et_rows(nrow(x), key = key, kind = "slope", label = gsub("_", " ", rn, fixed = TRUE),
                   estimate = x[, 1], conf.low = x[, 2], conf.high = x[, 3], p.value = x[, 4])
  .et_finish(rows, "matrix", NA_real_)
}

# ---------------------------------------------------------------------------
# Data frames

.et_teffects_eqs <- c("ate", "atet", "pomean", "pomeans")

# A data frame's own confidence level (percent): attr(x, "conf.level") or a
# constant conf.level column, as a proportion or a percentage; NA if none.
.et_df_level <- function(x) {
  cl <- attr(x, "conf.level", exact = TRUE)
  if (is.null(cl) && "conf.level" %in% names(x)) {
    u <- unique(x$conf.level[!is.na(x$conf.level)])
    if (length(u) > 1L) cli::cli_abort("The data frame's {.field conf.level} column holds more than one level.", call = NULL)
    cl <- if (length(u)) u else NULL
  }
  if (is.null(cl)) return(NA_real_)
  if (!is.numeric(cl) || length(cl) != 1L || !is.finite(cl) || cl <= 0 || cl >= 100 || cl == 1) {
    cli::cli_abort("The data frame's confidence level must be a proportion in (0, 1) or a percentage in (1, 100).",
                   call = NULL)
  }
  round(if (cl < 1) cl * 100 else cl, 10)
}

# Whether a data frame holds teffects rows (TRUE), margins rows (FALSE), or
# does not say (NA).
.et_df_kind <- function(x) {
  if ("kind" %in% names(x)) {
    k <- unique(as.character(x$kind[!is.na(x$kind)]))
    if (any(k %in% c("contrast", "pomean"))) return(TRUE)
    if (length(k)) return(FALSE)
  }
  # teffects equations: ATE, ATET, POmean(s), and the treatment and outcome
  # models' TME1, OME0, ... that come with them in r(table).
  eq <- if ("equation" %in% names(x)) tolower(trimws(as.character(x$equation))) else character()
  if (any(eq %in% .et_teffects_eqs | grepl("^(tme|ome)[0-9]*$", eq))) return(TRUE)
  term <- .et_df_terms(x)
  if (is.null(term)) return(NA)
  term <- trimws(term[!is.na(term)])
  if (any(grepl("^r[0-9]+vs[0-9]+\\.", term))) return(TRUE)
  # A variable, a base level, an interaction or an omitted term is a
  # margins row; a bare level ("0.t") could be either.
  forms <- vapply(term, function(t) {
    p <- .et_parse_key(t)
    if (p$form %in% c("variable", "interaction", "omitted") || identical(p$status, "base")) "margins" else "either"
  }, "")
  if (any(forms == "margins")) return(FALSE)
  NA
}

.et_df_terms <- function(x) {
  for (v in c("term", "key")) if (v %in% names(x)) return(as.character(x[[v]]))
  NULL
}

.et_df_check <- function(x) {
  if (!"estimate" %in% names(x)) cli::cli_abort("A data frame passed to {.fn effecttab} needs an {.field estimate} column.", call = NULL)
  if (is.null(.et_df_terms(x)) && !"label" %in% names(x) && !"variable" %in% names(x)) {
    cli::cli_abort("A data frame passed to {.fn effecttab} needs a {.field term} (or {.field label}) column naming each row.",
                   call = NULL)
  }
  num_cols <- c("estimate", "conf.low", "conf.high", "p.value", "std.error", "df", "df.error", "null.value")
  .tt_check_unambiguous(x, c(num_cols, "term", "key", "label", "variable"), "x")
  # df and df.error too (Codex audit D3): as.numeric() of a factor "100"
  # is its level code 1, and of unparsable text NA, which fell back to the
  # normal distribution.
  for (v in intersect(num_cols, names(x))) {
    if (is.factor(x[[v]]) || (!is.numeric(x[[v]]) && !all(is.na(x[[v]])))) {
      kind <- if (is.factor(x[[v]])) "a factor" else paste0("of class <", class(x[[v]])[1], ">")
      cli::cli_abort(c("Column {.field {v}} of the data frame must be numeric, not {kind}.",
                       "i" = if (is.factor(x[[v]])) "{.code as.numeric()} of a factor gives its level codes; convert it with {.code as.numeric(as.character(x))} first."),
                     call = NULL)
    }
  }
  if (xor("conf.low" %in% names(x), "conf.high" %in% names(x))) {
    cli::cli_abort("A data frame passed to {.fn effecttab} needs both {.field conf.low} and {.field conf.high}, or neither.",
                   call = NULL)
  }
  if ("p.value" %in% names(x) && any(x$p.value < 0 | x$p.value > 1, na.rm = TRUE)) {
    cli::cli_abort("Column {.field p.value} must lie in [0, 1].", call = NULL)
  }
  if ("status" %in% names(x) && !all(x$status[!is.na(x$status)] %in% c("est", "base", "omit", "empty"))) {
    cli::cli_abort("Column {.field status} takes {.val est}, {.val base}, {.val omit}, or {.val empty}.", call = NULL)
  }
  invisible(x)
}

# Parse a Stata key into its parts: "r1vs0.t" (contrast), "1.f" / "1b.f" /
# "2o.f" (factor level, with Stata's base and omitted markers), "o.x"
# (omitted term), "a#b" (interaction, shown by its key), else a variable.
.et_parse_key <- function(k) {
  if (grepl("^r[0-9]+vs[0-9]+\\.", k)) {
    m <- regmatches(k, regexec("^r([0-9]+)vs([0-9]+)\\.(.+)$", k))[[1]]
    return(list(form = "contrast", level = m[2], base = m[3], variable = m[4], key = k, status = "est"))
  }
  if (grepl("#", k, fixed = TRUE)) {
    parts <- strsplit(k, "#", fixed = TRUE)[[1]]
    st <- "est"
    clean <- vapply(parts, function(p) {
      m <- regmatches(p, regexec("^([0-9]+)([bno]+)\\.(.+)$", p))[[1]]
      if (length(m)) {
        ms <- .et_marker_status(m[3])
        if (ms == "omit") st <<- "omit" else if (ms == "base" && st == "est") st <<- "base"
        return(paste0(m[2], ".", m[4]))
      }
      p
    }, "", USE.NAMES = FALSE)
    key <- paste(clean, collapse = "#")
    return(list(form = "interaction", key = key, variable = NA_character_, level = NA_character_,
                base = NA_character_, status = st, parent = .et_factor_parent(key)))
  }
  m <- regmatches(k, regexec("^([0-9]+)([bno]*)\\.(.+)$", k))[[1]]
  if (length(m)) {
    st <- .et_marker_status(m[3])
    return(list(form = "level", level = m[2], variable = m[4], key = paste0(m[2], ".", m[4]), status = st,
                base = NA_character_))
  }
  if (grepl("^o\\.", k)) {
    return(list(form = "omitted", key = k, variable = sub("^o\\.", "", k), level = NA_character_,
                base = NA_character_, status = "omit"))
  }
  list(form = "variable", key = k, variable = k, level = NA_character_, base = NA_character_, status = "est")
}

# Stata's factor-level markers: "b" the base level, "o" omitted, "bn" (no
# base, as teffects writes its potential-outcome means, 0bn.mbsmoke) and
# none an ordinary level.
.et_marker_status <- function(mk) {
  if (grepl("o", mk, fixed = TRUE)) return("omit")
  if (identical(mk, "b")) return("base")
  "est"
}

# The heading Stata's collection renderer gives an interaction of factor
# levels (_tabtools_collect_render.ado:1028-1066): each part's level prefix
# (digits with b/o/n) removed, "c." dropped; none when no part is a factor.
.et_factor_parent <- function(key) {
  parts <- strsplit(key, "#", fixed = TRUE)[[1]]
  has <- FALSE
  out <- vapply(parts, function(p) {
    m <- regmatches(p, regexec("^([0-9bon]*[0-9][0-9bon]*)\\.(.+)$", p))[[1]]
    if (length(m)) {
      has <<- TRUE
      return(m[3])
    }
    if (grepl("^c\\.", p)) return(sub("^c\\.", "", p))
    p
  }, "", USE.NAMES = FALSE)
  parent <- paste(out, collapse = "#")
  if (!has || !all(nzchar(out)) || identical(parent, key)) NA_character_ else parent
}

# Wald interval and p-value from a standard error, as regtab's data-frame
# input derives them (Milestone H task H1): a row with neither bound gets
# the interval at `level` (percent), and a frame without a p.value column
# the p-value (null 0), from Student's t with the row's `df` (or
# `df.error`) when it is finite, else the normal distribution. attr
# "derived" says whether every interval shown was derived here, so the
# frame's level is then known to be `level`.
.et_df_wald <- function(x, level) {
  n <- nrow(x)
  has_ci <- "conf.low" %in% names(x)
  # One bound without the other used to be shown as a blank interval
  # (Muse audit P1-29).
  if (has_ci || "conf.high" %in% names(x)) {
    lo0 <- if (has_ci) as.numeric(x$conf.low) else rep(NA_real_, n)
    hi0 <- if ("conf.high" %in% names(x)) as.numeric(x$conf.high) else rep(NA_real_, n)
    half <- xor(is.na(lo0), is.na(hi0))
    if (any(half)) {
      cli::cli_abort(c("Row {which(half)[1]} gives one confidence bound without the other.",
                       "i" = "Give both {.field conf.low} and {.field conf.high}, or neither (with {.field std.error} to derive them)."),
                     class = "tabtools_error_df_half_interval", call = NULL)
    }
    reversed <- is.finite(lo0) & is.finite(hi0) & lo0 > hi0
    if (any(reversed)) {
      cli::cli_abort("Row {which(reversed)[1]} gives {.field conf.low} above {.field conf.high}.",
                     class = "tabtools_error_df_interval", call = NULL)
    }
  }
  if (!"std.error" %in% names(x)) {
    attr(x, "derived") <- FALSE
    return(x)
  }
  se <- as.numeric(x$std.error)
  if (any(se <= 0, na.rm = TRUE)) cli::cli_abort("Column {.field std.error} must be positive.", call = NULL)
  dfv <- NULL
  for (v in c("df", "df.error")) if (v %in% names(x)) {
    dfv <- as.numeric(x[[v]])
    break
  }
  if (!is.null(dfv) && any(dfv <= 0, na.rm = TRUE)) {
    cli::cli_abort("Column {.field df} (or {.field df.error}) must be positive ({.code Inf} for the normal distribution).",
                   call = NULL)
  }
  if (is.null(dfv)) dfv <- rep(Inf, n)
  tdist <- is.finite(dfv)
  # Missing degrees of freedom and non-finite standard errors do not
  # identify a sampling distribution; leave their derived inference blank.
  infer <- is.finite(x$estimate) & is.finite(se) & se > 0 & !is.na(dfv)
  q <- ifelse(tdist, stats::qt(1 - (1 - level / 100) / 2, pmax(dfv, 1e-300)), stats::qnorm(1 - (1 - level / 100) / 2))
  lo <- if (has_ci) as.numeric(x$conf.low) else rep(NA_real_, n)
  hi <- if (has_ci) as.numeric(x$conf.high) else rep(NA_real_, n)
  need <- is.na(lo) & is.na(hi) & infer
  lo[need] <- x$estimate[need] - q[need] * se[need]
  hi[need] <- x$estimate[need] + q[need] * se[need]
  shown <- is.finite(lo) & is.finite(hi)
  attr_derived <- any(need) && all(need[shown])
  x$conf.low <- lo
  x$conf.high <- hi
  # A derived p-value tests `null.value` (default 0, a difference); a ratio
  # states its null (1) there, and effecttab() refuses a ratio header whose
  # p-values were derived against the default (Muse audit P1-29).
  has_null <- "null.value" %in% names(x)
  null <- if (has_null) as.numeric(x$null.value) else rep(0, n)
  if (has_null && anyNA(null)) cli::cli_abort("Column {.field null.value} may not be missing.", call = NULL)
  # D5 (plan 2026-10-06): a ratio-scale frame keeps the linear delta-method
  # Wald, as Stata's nlcom/margins do, but a derived interval reaching 0 or
  # below cannot be a ratio's. Recorded here, warned once per effecttab()
  # call where the effect header is known.
  attr(x, "lo_bad") <- need & is.finite(lo) & lo <= 0
  attr(x, "null_one") <- is.finite(null) & null == 1
  p_derived <- FALSE
  if (!"p.value" %in% names(x)) {
    x$p.value <- rep(NA_real_, n)
    valid <- infer & is.finite(null)
    stat <- abs((x$estimate[valid] - null[valid]) / se[valid])
    x$p.value[valid] <- ifelse(tdist[valid], 2 * stats::pt(-stat, dfv[valid]), 2 * stats::pnorm(-stat))
    p_derived <- any(is.finite(x$p.value))
  }
  attr(x, "p_null_default") <- p_derived && !has_null
  attr(x, "derived") <- attr_derived
  x
}

#' @export
tt_effect_rows.data.frame <- function(x, type = c("teffects", "margins"), data = NULL, level = NULL, ...) {
  type <- match.arg(type)
  level_pct <- .et_check_level(level)
  est_attr <- attr(x, "tt_estimator", exact = TRUE)
  id_attr <- attr(x, "model_id", exact = TRUE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  .et_df_check(x)
  lev <- .et_df_level(x)
  if (!is.na(lev) && !is.na(level_pct)) .et_resolve_level(lev, level_pct, TRUE)
  # Intervals derived from std.error are built at the frame's own level,
  # else the requested one (effecttab(level =)), else 95%, and the frame
  # then records that level, so the header always names the level the
  # interval was built at (review F1).
  wald_level <- if (!is.na(lev)) lev else if (!is.na(level_pct)) level_pct else 95
  x <- .et_df_wald(x, wald_level)
  p_null_default <- isTRUE(attr(x, "p_null_default"))
  lo_bad <- attr(x, "lo_bad")
  null_one <- attr(x, "null_one")
  if (is.na(lev) && isTRUE(attr(x, "derived"))) lev <- wald_level
  n <- nrow(x)
  col <- function(v, default = NA) if (v %in% names(x)) x[[v]] else rep(default, n)
  term <- .et_df_terms(x)
  kind <- as.character(col("kind"))
  label <- as.character(col("label"))
  variable <- as.character(col("variable"))
  eq <- tolower(trimws(as.character(col("equation", ""))))
  eq[is.na(eq)] <- ""
  # teffects collections keep the ATE, ATET and potential-outcome-mean
  # equations only (effecttab.ado:462-474); the treatment and outcome
  # models' coefficients (TME1, OME0, ...) are not shown (`full` is not
  # ported, decision EO8).
  keep <- rep(TRUE, n)
  if (type == "teffects" && "equation" %in% names(x)) {
    keep <- !nzchar(eq) | eq %in% .et_teffects_eqs
    if (!any(keep)) cli::cli_abort("No treatment-effect equations (ATE, ATET, POmean) found in the data frame.", call = NULL)
  }
  out <- list()
  for (i in which(keep)) {
    est <- as.numeric(x$estimate[i])
    nums <- list(estimate = est, conf.low = as.numeric(col("conf.low")[i]),
                 conf.high = as.numeric(col("conf.high")[i]), p.value = as.numeric(col("p.value")[i]))
    status <- if ("status" %in% names(x) && !is.na(x$status[i])) x$status[i] else NA_character_
    lab <- if (!is.na(label[i]) && nzchar(label[i])) label[i] else NA_character_
    k <- kind[i]
    if (!is.na(k)) {
      r <- .et_df_kind_row(x, i, k, type, data, lab, nums, status)
    } else if (!is.null(term) && !is.na(term[i]) && nzchar(term[i])) {
      r <- .et_df_term_row(term[i], eq[i], type, data, lab, nums, status)
    } else {
      r <- .et_rows(1L, key = .et_or(lab, variable[i]), kind = "slope", variable = variable[i], label = lab,
                    estimate = nums$estimate, conf.low = nums$conf.low, conf.high = nums$conf.high,
                    p.value = nums$p.value, status = if (is.na(status)) "est" else status)
    }
    if (!is.null(r)) out[[length(out) + 1L]] <- r
  }
  rows <- if (length(out)) do.call(rbind, out) else .et_empty_rows()
  if (type == "teffects" && !any(rows$kind %in% c("contrast", "pomean"))) {
    cli::cli_abort("No treatment contrasts or potential-outcome means found in the data frame.", call = NULL)
  }
  if (anyDuplicated(rows$key)) {
    cli::cli_abort("The data frame names row {.val {rows$key[duplicated(rows$key)][1]}} more than once.", call = NULL)
  }
  estimand <- if (any(eq[keep] == "atet")) "ATET" else if (any(eq[keep] == "ate")) "ATE" else NULL
  rows <- .et_finish(rows, "data.frame", lev,
                     estimator = if (is.character(est_attr) && length(est_attr) == 1L) est_attr else NULL,
                     model_id = id_attr %||% "", estimand = estimand)
  attr(rows, "p_null_default") <- p_null_default
  # Terms (else row numbers) whose derived interval reaches 0 or below.
  tn <- .et_df_terms(x) %||% rep(NA_character_, n)
  tn <- ifelse(is.na(tn) | !nzchar(tn), paste0("row ", seq_len(n)), tn)
  attr(rows, "lo_bad") <- data.frame(term = tn[lo_bad], null_one = null_one[lo_bad], stringsAsFactors = FALSE)
  rows
}

# A row named by a Stata key (the r(table) column name).
.et_df_term_row <- function(t, eq, type, data, lab, nums, status) {
  p <- .et_parse_key(trimws(t))
  st <- if (is.na(status)) p$status else status
  mk <- function(...) .et_rows(1L, ..., estimate = nums$estimate, conf.low = nums$conf.low,
                               conf.high = nums$conf.high, p.value = nums$p.value, status = st)
  vi <- .et_var(data, p$variable)
  info <- function(code) list(code = code, text = if (vi$exists) .et_code_text(vi$col, code) else NA_character_)
  if (type == "teffects") {
    if (p$form == "contrast") {
      lv <- info(p$level)
      bs <- info(p$base)
      return(mk(key = p$key, kind = "contrast", variable = p$variable, level = p$level, base = p$base,
                level_text = lv$text, base_text = bs$text, var_label = if (vi$exists) vi$label else p$variable,
                label = lab))
    }
    if (p$form == "level" && (!nzchar(eq) || eq %in% c("pomean", "pomeans"))) {
      lv <- info(p$level)
      return(mk(key = p$key, kind = "pomean", variable = p$variable, level = p$level, level_text = lv$text,
                var_label = if (vi$exists) vi$label else p$variable, label = lab))
    }
    # Stata keeps only rows keyed as a contrast or a potential-outcome mean.
    return(NULL)
  }
  switch(p$form,
    contrast = mk(key = p$key, kind = "slope", label = .et_or(lab, p$key)),
    level = {
      lv <- info(p$level)
      # Levels of a variable Stata cannot find in the data keep their keys
      # (collect renders "1.race"), under a heading of the variable name.
      mk(key = p$key, kind = if (st == "base") "ref" else "level", variable = p$variable, level = p$level,
         level_text = lv$text, var_label = if (vi$exists) vi$label else p$variable, parent = p$variable,
         label = if (!is.na(lab)) lab else if (!vi$exists) p$key else NA_character_)
    },
    interaction = mk(key = p$key, kind = "level", variable = p$parent, var_label = p$parent,
                     parent = p$parent, label = .et_or(lab, p$key)),
    omitted = mk(key = p$key, kind = "slope", variable = p$variable, label = .et_or(lab, p$key)),
    variable = mk(key = p$key, kind = "slope", variable = p$variable,
                  var_label = if (vi$exists) vi$label else p$variable, label = lab))
}

# A row described by `kind`, `variable`, `level`, `base` columns (an R
# result from another package, reshaped).
.et_df_kind_row <- function(x, i, k, type, data, lab, nums, status) {
  ok_kinds <- c("contrast", "pomean", "prediction", "slope", "level", "ref")
  if (!k %in% ok_kinds) {
    cli::cli_abort("Column {.field kind} takes {.or {.val {ok_kinds}}}, not {.val {k}}.", call = NULL)
  }
  if (type == "teffects" && !k %in% c("contrast", "pomean")) return(NULL)
  if (type == "margins" && k %in% c("contrast", "pomean")) {
    cli::cli_abort("Row kind {.val {k}} is a teffects row; the table type is margins.", call = NULL)
  }
  g <- function(v) if (v %in% names(x) && !is.na(x[[v]][i])) as.character(x[[v]][i]) else NA_character_
  v <- g("variable")
  vi <- .et_var(data, v)
  need <- switch(k, contrast = c("variable", "level", "base"), pomean = , level = , ref = c("variable", "level"),
                 character())
  miss <- need[vapply(need, function(n) is.na(g(n)), TRUE)]
  if (length(miss)) cli::cli_abort("A {.val {k}} row needs {.field {miss}}.", call = NULL)
  lv <- if (!is.na(g("level"))) .et_levels(vi$col, g("level")) else list(code = NA_character_, text = NA_character_)
  bs <- if (!is.na(g("base"))) .et_levels(vi$col, g("base")) else list(code = NA_character_, text = NA_character_)
  st <- if (!is.na(status)) status else if (k == "ref") "base" else "est"
  key <- switch(k, contrast = paste0("r", lv$code, "vs", bs$code, ".", v), pomean = , level = , ref = paste0(lv$code, ".", v),
                .et_or(g("term"), .et_or(lab, v)))
  .et_rows(1L, key = key, kind = k, variable = v, level = lv$code, base = bs$code, level_text = lv$text,
           positional = isTRUE(lv$positional[1]),
           base_text = bs$text, var_label = vi$label, parent = if (k %in% c("level", "ref")) v else NA_character_,
           label = lab, estimate = nums$estimate, conf.low = nums$conf.low, conf.high = nums$conf.high,
           p.value = nums$p.value, status = st)
}
