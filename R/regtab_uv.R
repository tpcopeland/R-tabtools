# regtab_uv(): univariable ("crude") models stacked into one model column
# (task 7.13; the interop research's gtsummary tbl_uvregression idea, MIT).
#
# One fit per covariate from a formula template; regtab() shows them as one
# model whose rows are each fit's own tt_regtab_rows() for its focal
# variable, so keys, labels, Reference rows and Stata's Wald statistics are
# exactly those of the separate fits (a Stata `foreach` loop of
# `collect: logit y x`, golden R77), and line up with an adjusted model
# beside it: regtab(list(Crude = regtab_uv(...), Adjusted = fit)).

#' Univariable models as one regtab column
#'
#' Fits one model per covariate, `y ~ x` for each `x` (or any formula
#' template), and returns them as one model for [regtab()]: its column
#' stacks each fit's rows for its own covariate, the rows and numbers the
#' fits would have as separate models (Stata: a `foreach` loop of `collect:
#' logit y x`). Put it beside the adjusted model for a crude and adjusted
#' table: `regtab(list(Crude = regtab_uv(...), Adjusted = fit))`.
#'
#' Only the focal covariate's rows of each fit are shown: its main effect
#' (also through a call in the template, `"{y} ~ log({x})"`), its factor
#' levels and its interactions (`"{y} ~ {x} * sex"`); the intercept and the
#' rows of any other term (`"{y} ~ {x} + age"`, each covariate adjusted for
#' age) are left out. The model's statistics are
#' the fits' counts (Observations, Subjects, Events, Groups) where every
#' fit has the same one, else blank; likelihood-based statistics are blank.
#' `vce = "stata"`, `"model"` and `"robust"` apply to every fit; clusters
#' and user-supplied variances are not available.
#' @param data A data frame.
#' @param y The outcome: a column name, or for a survival model the left-hand
#'   side as text (`"Surv(time, status)"`).
#' @param x Covariates, a character vector of column names; one model each,
#'   in this order.
#' @param method The model function, unquoted (`glm`, `survival::coxph`) or
#'   as text (`"glm"`).
#' @param method.args A list of further arguments to `method`
#'   (`list(family = binomial)`), inserted into each call as they are: a
#'   column of `data` goes in quoted (`list(weights = quote(w))`).
#' @param formula The formula template: `{y}` and `{x}` are replaced by `y`
#'   and each covariate.
#' @return An object of class `tt_uv`: `$fits`, the fitted models named by
#'   covariate, `$x`, and `$sample_input_n`, the number of records supplied
#'   to this constructor. [regtab()] uses that count in the source ledger
#'   described in [tt_table()], separately for each univariable fit.
#' @seealso [regtab()]
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' crude <- regtab_uv(d, "am", c("wt", "hp", "cyl"), method = glm,
#'                    method.args = list(family = binomial))
#' adj <- glm(am ~ wt + hp + cyl, family = binomial, data = d)
#' regtab(list(Crude = crude, Adjusted = adj))
#' @export
regtab_uv <- function(data, y, x, method = "glm", method.args = list(), formula = "{y} ~ {x}") {
  sample_input_n <- as.numeric(nrow(data))
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.", call = NULL)
  .check_string(y, "y")
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    cli::cli_abort("{.arg x} must be a character vector of column names.", call = NULL)
  }
  if (anyDuplicated(x)) cli::cli_abort("{.arg x} names {.val {unique(x[duplicated(x)])}} more than once.", call = NULL)
  miss <- setdiff(x, names(data))
  if (length(miss)) cli::cli_abort("{.arg x} names {.val {miss}}, not {?a column/columns} of {.arg data}.", call = NULL)
  .check_string(formula, "formula")
  if (!grepl("{x}", formula, fixed = TRUE)) cli::cli_abort("{.arg formula} must contain {.code {x}}.", call = NULL)
  if (!is.list(method.args) || (length(method.args) && is.null(names(method.args))) ||
      any(!nzchar(names(method.args)))) {
    cli::cli_abort("{.arg method.args} must be a named list.", call = NULL)
  }
  if (any(c("formula", "data") %in% names(method.args))) {
    cli::cli_abort("{.arg method.args} may not set {.arg formula} or {.arg data}.", call = NULL)
  }
  # The call names the method as written, so the fit's call reads
  # glm(formula = am ~ wt, family = binomial, data = .tt_uv_data) and
  # regtab can re-read its data.
  fun <- substitute(method)
  if (is.character(method)) {
    .check_string(method, "method")
    fun <- str2lang(method)
  } else if (!is.function(method)) {
    cli::cli_abort("{.arg method} must be a model function, such as {.fn glm} or {.code \"glm\"}.", call = NULL)
  }
  env <- new.env(parent = parent.frame())
  env$.tt_uv_data <- data
  # Column names are symbols, even when their text contains formula
  # operators or spaces. The survival response remains an expression.
  symbol_text <- function(v) paste(deparse(as.name(v), backtick = TRUE, width.cutoff = 500L), collapse = " ")
  y_text <- if (y %in% names(data)) symbol_text(y) else y
  fits <- lapply(x, function(v) {
    txt <- formula
    tokens <- gregexpr("\\{[xy]\\}", txt)
    replacements <- c("{x}" = symbol_text(v), "{y}" = y_text)
    regmatches(txt, tokens) <- list(unname(replacements[regmatches(txt, tokens)[[1L]]]))
    f <- tryCatch(stats::as.formula(txt, env = env), error = function(e) {
      cli::cli_abort("{.arg formula} for {.val {v}} is not a formula: {.code {txt}}.", parent = e, call = NULL)
    })
    cl <- as.call(c(list(fun, formula = f), method.args, list(data = quote(.tt_uv_data))))
    tryCatch(eval(cl, env), error = function(e) {
      cli::cli_abort("The model for {.val {v}} failed ({.code {txt}}).", parent = e, call = NULL)
    })
  })
  names(fits) <- x
  cls <- unique(vapply(fits, function(f) class(f)[1], ""))
  if (length(cls) != 1L) cli::cli_abort("The fits have different classes ({.cls {cls}}).", call = NULL)
  for (k in seq_along(fits)) {
    tryCatch(.rt_check_model(fits[[k]], k), error = function(e) {
      cli::cli_abort("The model for {.val {x[k]}} cannot be tabled.", parent = e, call = NULL)
    })
  }
  structure(list(fits = fits, x = x, y = y, formula = formula,
                 sample_input_n = sample_input_n), class = "tt_uv")
}

#' @export
print.tt_uv <- function(x, ...) {
  cat("<tt_uv> ", length(x$fits), " univariable <", class(x$fits[[1]])[1], "> fits of ", x$y, ": ",
      paste(x$x, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
tt_regtab_adapter.tt_uv <- function(fit, ...) TRUE

# The first fit's model information (the fits share the class and, from
# one template, the family, link and outcome), named as the stack.
#' @export
tt_model_info.tt_uv <- function(fit, ...) {
  info <- tt_model_info(fit$fits[[1]], ...)
  info$model_id <- paste0("regtab_uv(", fit$y, " ~ ", paste(fit$x, collapse = ", "), ")")
  info
}

#' @export
tt_vce_types.tt_uv <- function(fit) {
  setdiff(Reduce(intersect, lapply(fit$fits, tt_vce_types)), "cluster")
}

#' @export
tt_ci_methods.tt_uv <- function(fit) Reduce(intersect, lapply(fit$fits, tt_ci_methods))

# Each fit's own rows, restricted to its focal covariate: the variable (or
# factor) rows whose block is that covariate, and the interaction rows the
# template gives it; the intercept and every other term are left out.
#' @export
tt_regtab_rows.tt_uv <- function(fit, info, ...) {
  pos <- character()
  rows <- lapply(seq_along(fit$fits), function(k) {
    f <- fit$fits[[k]]
    r <- tt_regtab_rows(f, tt_model_info(f), ...)
    pos <<- union(pos, attr(r, "positional"))
    r <- r[r$kind != "intercept" & .rt_uv_focal(r, fit$x[k]), , drop = FALSE]
    if (!nrow(r)) {
      cli::cli_abort("The model for {.val {fit$x[k]}} has no row for it.", call = NULL)
    }
    r$sub <- k * 1e4 + r$sub
    r
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "positional") <- pos
  out
}

# A row is the covariate's when a part of its key (split at `#`, the level
# code and a `c.` prefix removed) is the covariate or a call on it
# (`log(wt)`, `factor(cyl)`): its main-effect, factor and interaction rows
# (stack review 3).
.rt_uv_focal <- function(r, v) {
  vars <- .rt_row_keys(r)$var
  # Parse a whole quoted symbol before interpreting # as the separator
  # in a Stata interaction key: it can be part of a literal column name.
  whole <- vapply(vars, function(p) {
    identical(p, v) || v %in% tryCatch(all.vars(str2lang(p)), error = function(e) character())
  }, TRUE)
  whole | r$block %in% v | vapply(strsplit(vars, "#", fixed = TRUE), function(parts) {
    parts <- sub("^c\\.", "", parts)
    any(parts == v) || any(vapply(parts, function(p) {
      v %in% tryCatch(all.vars(str2lang(p)), error = function(e) character())
    }, TRUE))
  }, TRUE)
}

#' @export
tt_model_stats.tt_uv <- function(fit, info, vce = "stata", cluster = NULL, ...) {
  st <- lapply(fit$fits, function(f) .rt_quiet_zero_weight(tt_model_stats(f, tt_model_info(f))))
  same <- function(nm) {
    v <- vapply(st, function(s) as.numeric(s[[nm]] %||% NA_real_)[1], 0)
    if (anyNA(v) || any(v != v[1])) NA_real_ else v[1]
  }
  list(N = same("N"), N_sub = same("N_sub"), events = same("events"), groups = same("groups"),
       ll = NA_real_, rank = NA_real_, aic = NA_real_, bic = NA_real_, r2 = NA_real_, r2_p = NA_real_,
       r2_a = NA_real_, rmse = NA_real_, F = NA_real_)
}
