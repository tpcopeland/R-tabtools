# Event rates per group, the Stata `strate` analogue (plan task 7.3).
#
# Port of the default (non-jackknife) path of Stata 17's
# `/usr/local/stata17/ado/base/s/strate.ado` (version 1.x, 527 lines), which
# stratetab consumes through strate's saved output (`_D`, `_Y`, `_Rate`,
# `_Lower`, `_Upper` per group):
#
# - person-time per record is `(_t - _t0) / per` (`:91-96`), stored in a
#   variable of `_t`'s storage type; the division promotes byte/int/float to
#   **float** and long/double to double (probed in Stata 17), so compressed
#   whole-day follow-up gives single-precision person-time per record;
# - events and person-time are weighted sums (fweight) in double (`:130-131`,
#   `:219-229`);
# - rate = D / Y; CI **log-normal**: exp(log(D/Y) -/+ z / sqrt(D)) with
#   z = invnorm(0.5 + level/200), missing when D = 0 (`:227-233`);
# - records outside the st sample (`_st == 0`: time <= entry, missing time)
#   and, unless `missing`, records with a missing grouping value are dropped
#   (`:48-49`, `:110-116`); output is sorted by the grouping variables,
#   missing last.
#
# Not ported: the jackknife CIs strate uses for pweight/iweight data or with
# `jackknife`/`cluster()` (`:53-62`, `:133-212`), and `smr()`.

#' Event rates per group (Stata strate)
#'
#' The R counterpart of Stata's `strate`: events, person-time, the rate, and
#' its log-normal confidence interval per group, from one row per subject
#' (or record) with an exit time and a failure indicator. The result is in
#' the shape [stratetab()] takes as one outcome x exposure block, the shape
#' of the file `strate, output()` saves.
#'
#' The interval is `exp(log(D/Y) -/+ z / sqrt(D))` with `z =
#' qnorm(0.5 + level/2)`, missing when there are no events (`strate.ado`).
#' Records outside the analysis sample (exit at or before entry, missing
#' time) are dropped, and so are records with a missing grouping value
#' unless `missing = TRUE`; groups are sorted by the grouping columns,
#' missing last. Not ported: the jackknife intervals `strate` uses for
#' pweight/iweight data or with `jackknife`/`cluster()`, and `smr()`.
#'
#' The result names its statistics `D`, `Y`, `Rate`, `Lower` and `Upper`,
#' so a grouping column of one of those names is refused (rename it
#' first). Stata's `strate` names them `_D`, `_Y`, ... and refuses grouping
#' variables of those names instead, so a `strate` file whose grouping
#' variable is called `Y` (it holds both `Y` and `_Y`) is refused here too,
#' where Stata's `stratetab` reads it.
#'
#' @param data A data frame with one row per record; or a data frame
#'   already in strate shape (columns `_D`/`D`, `_Y`/`Y`, `_Rate`/`Rate`,
#'   `_Lower`/`Lower`, `_Upper`/`Upper` plus grouping columns, e.g. a `strate`
#'   file read with `haven::read_dta()`), which is validated and returned
#'   standardised when `time` and `event` are not given.
#' @param time Name of the exit-time column (Stata `stset` time).
#' @param event Name of the failure indicator: failure when non-zero and not
#'   missing (`stset, failure(var)`). Missing event values retain their
#'   person-time and count as no event, following this Stata convention.
#' @param by Grouping (exposure) variable names, sorted in this order.
#' @param strata Optional stratum variable names; groups are formed by
#'   `c(strata, by)` (strata outermost), like `strate strata by`.
#' @param per Person-time units (`per()`); person-time is divided by it, so
#'   the rate is per `per` units of time. Time and event data only: a
#'   strate-shaped `data` already holds its person-time and rates in the
#'   units it was saved with, so any other value than 1 is refused there.
#' @param level Confidence level, a proportion (`0.90`) or a percentage
#'   (`90`), as in [regtab()]. For time and event data the default is 0.95.
#'   For a strate-shaped `data` it states the level of the supplied
#'   intervals: by default it is read from the `"label"` attributes of the
#'   interval columns (`strate` labels them `"Lower 95% confidence limit"`,
#'   or `"Lower 97,5% ..."` under Stata's `set dp comma`), and it is unknown
#'   (`NA`) when they carry none; a value that contradicts the labels is an
#'   error.
#' @param entry Late-entry time: a number or a column name (`stset
#'   enter(time #)`); records ending at or before entry are dropped.
#' @param fweight Optional frequency-weight column (`stset [fweight=]`).
#' @param missing Keep missing grouping values as a group (`missing`).
#' @param float_time Emulate Stata's single-precision person-time. `NULL`
#'   (default) guesses Stata's compressed storage from the values: whole
#'   numbers within int range (|x| <= 32,740) are byte/int in a Stata dataset,
#'   so person-time becomes float; anything else is treated as double. Set
#'   `TRUE`/`FALSE` when the source dataset's storage type is known.
#' @return A data frame: the grouping columns (with their `"label"`
#'   attributes), then `D` (events), `Y` (person-time / per), `Rate`,
#'   `Lower`, `Upper`; attributes `per`, `level` (a proportion, `NA` when
#'   unknown), `ci_method` (`"lognormal"`), `by`, `strata`, and, for time
#'   and event data, `event` (the event column's name) and `event_label`
#'   (its `"label"` attribute, `NULL` when it has none). [stratetab()]
#'   labels its exposure blocks and outcomes from these.
#'   The `sample_accounting` attribute carries the source ledger described
#'   in [tt_table()], including explicit unknown counts for supplied rates.
#' @seealso [stratetab()], which formats these blocks as a rate table.
#' @examples
#' set.seed(1)
#' d <- data.frame(years = rexp(200, 0.1), died = rbinom(200, 1, 0.3),
#'                 arm = rep(c("A", "B"), 100))
#' tt_rates(d, time = "years", event = "died", by = "arm")
#'
#' # A strate-shaped data frame, validated and standardised
#' s <- data.frame(arm = c("A", "B"), `_D` = c(30, 42), `_Y` = c(1000, 950),
#'                 `_Rate` = c(0.03, 0.0442), `_Lower` = c(0.021, 0.0327),
#'                 `_Upper` = c(0.0429, 0.0598), check.names = FALSE)
#' tt_rates(s, level = 0.95)
#' @export
tt_rates <- function(data, time = NULL, event = NULL, by = NULL, strata = NULL, per = 1,
                     level = NULL, entry = NULL, fweight = NULL, missing = FALSE,
                     float_time = NULL) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.", call = NULL)
  if (!is.logical(missing) || length(missing) != 1L || is.na(missing)) {
    cli::cli_abort("{.arg missing} must be TRUE or FALSE.", call = NULL)
  }
  if (!is.null(float_time) && (!is.logical(float_time) || length(float_time) != 1L || is.na(float_time))) {
    cli::cli_abort("{.arg float_time} must be TRUE or FALSE, or NULL.", call = NULL)
  }
  for (arg in c("time", "event", "fweight")) {
    val <- get(arg)
    if (!is.null(val) && (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val))) {
      cli::cli_abort("{.arg {arg}} must name a single column.", call = NULL)
    }
  }
  .rate_check_reserved(c(strata, by))
  if (is.null(time) && is.null(event)) {
    if (!nrow(data)) {
      cli::cli_abort("The strate data contain no observations.", class = "tabtools_error_rate_empty", call = NULL)
    }
    # The level of supplied intervals is theirs, not a default (plan 7.4):
    # read from the column labels unless the caller states it.
    return(tt_rates_from_strate(data, by = c(strata, by), per = per, level = level))
  }
  # One without the other used to fail deep inside with "attempt to select
  # less than one element" (Phase 7b review F7b).
  if (is.null(time) || is.null(event)) {
    cli::cli_abort(c("{.arg time} and {.arg event} must be given together.",
                     "i" = "Give neither for a data frame already in strate shape."), call = NULL)
  }
  if (is.character(entry) && (length(entry) != 1L || is.na(entry) || !nzchar(entry))) {
    cli::cli_abort("{.arg entry} must name a single column, or be a number.", call = NULL)
  }
  .rate_check_cols(data, c(time, event, by, strata, if (is.character(entry)) entry, fweight))
  if (!is.numeric(per) || length(per) != 1L || !is.finite(per) || per <= 0) {
    cli::cli_abort("{.arg per} must be a positive number.", call = NULL)
  }
  level <- .rate_check_level(level %||% 0.95)
  if (!is.null(entry) && !is.character(entry) &&
      (!is.numeric(entry) || length(entry) != 1L || !is.finite(entry))) {
    cli::cli_abort("{.arg entry} must be a number or a column name.", call = NULL)
  }

  # Numeric columns as Stata stores them (Milestone H, H15; finding F20):
  # no factors or strings (as.numeric() would read factor codes), no
  # infinities.
  t <- .rate_numeric(data, time, "time")
  t0 <- if (is.null(entry)) rep(0, nrow(data)) else if (is.character(entry)) .rate_numeric(data, entry, "entry") else rep(as.numeric(entry), nrow(data))
  ev <- .rate_numeric(data, event, "event")
  d <- as.numeric(!is.na(ev) & ev != 0)
  w <- if (is.null(fweight)) rep(1, nrow(data)) else .rate_fweight(data, fweight)
  groups <- c(strata, by)

  # st sample: stset keeps records with non-missing time after entry
  # (_st == 0 otherwise); strate then marks out missing weights, time, and
  # (without `missing`) grouping values.
  use <- !is.na(t) & !is.na(t0) & t > t0 & !is.na(w)
  if (!is.null(fweight)) use <- use & w > 0
  if (!missing) for (g in groups) use <- use & !is.na(data[[g]])
  # strate: "no observations" (r(2000)).
  if (!is.null(fweight) && !any(!is.na(w) & w > 0)) {
    cli::cli_abort("No observations: every frequency weight is zero or missing.", call = NULL)
  }
  if (!any(use)) cli::cli_abort("No observations: no record has follow-up after entry.", call = NULL)

  if (is.null(float_time)) {
    tt <- c(t[use], t0[use])
    float_time <- all(tt == floor(tt)) && all(abs(tt) <= 32740)
  }
  y <- (t - t0) / per
  if (isTRUE(float_time)) y <- stata_float(y)

  keys <- if (length(groups)) data[use, groups, drop = FALSE] else data.frame(row.names = seq_len(sum(use)))
  wd <- (w * d)[use]
  wy <- (w * y)[use]
  if (length(groups)) {
    # Stata sorts by the grouping variables with missing values last.
    ord <- do.call(order, c(lapply(keys, function(k) as.numeric(.rate_sort_key(k))), list(na.last = TRUE)))
    keys <- keys[ord, , drop = FALSE]
    wd <- wd[ord]
    wy <- wy[ord]
    id <- cumsum(!duplicated(keys))
    out <- keys[!duplicated(keys), , drop = FALSE]
    D <- as.vector(tapply(wd, id, sum))
    Y <- as.vector(tapply(wy, id, sum))
  } else {
    out <- data.frame(row.names = 1L)
    D <- sum(wd)
    Y <- sum(wy)
  }
  rownames(out) <- NULL
  # Subsetting drops the columns' "label" attributes; keep them, as `by`
  # is kept, so stratetab() can label the exposure block (D02).
  for (g in groups) {
    lab <- attr(data[[g]], "label", exact = TRUE)
    if (!is.null(lab)) attr(out[[g]], "label") <- lab
  }
  res <- .rate_ci(out, D, Y, level)
  sample <- .rate_sample_accounting(data, out, groups, event, t, t0, ev, w, use,
                                    fweight, missing)
  structure(res, per = per, level = level, ci_method = "lognormal", by = by, strata = strata,
            event = event, event_label = attr(data[[event]], "label", exact = TRUE),
            sample_accounting = sample)
}

.rate_sample_accounting <- function(data, out, groups, event, t, t0, ev, w, use,
                                    fweight, missing) {
  population <- function(sel, id, scope, group = NA_character_) {
    used <- sel & use
    weights <- .tt_sample_weights(w[used])
    values <- c(list(input_n = sum(sel), eligible_n = sum(used),
                     observed_n = sum(used & !is.na(ev)), used_n = sum(used),
                     missing_n = sum(used & is.na(ev)), excluded_n = sum(sel & !use),
                     zero_weight_n = sum(sel & !is.na(w) & w == 0)), weights$values)
    exclusions <- list()
    remaining <- sel
    rules <- list(missing_exit = is.na(t), missing_entry = is.na(t0),
                  no_follow_up = !is.na(t) & !is.na(t0) & t <= t0,
                  missing_weight = is.na(w))
    if (!is.null(fweight)) rules$zero_frequency_weight <- !is.na(w) & w == 0
    if (!missing && length(groups)) {
      rules$missing_group <- Reduce(`|`, lapply(groups, function(g) is.na(data[[g]])))
    }
    for (reason in names(rules)) {
      excluded <- remaining & rules[[reason]]
      exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion(
        "input_to_eligible", reason, sum(excluded), "sequential input eligibility masks")
      remaining <- remaining & !rules[[reason]]
    }
    exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion(
      "retained", "missing_event_retained_as_no_event", sum(used & is.na(ev)),
      "tt_rates retains exposure when event is missing")
    if (missing && length(groups)) {
      missing_group <- Reduce(`|`, lapply(groups, function(g) is.na(data[[g]])))
      exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion(
        "retained", "missing_group_retained", sum(used & missing_group), "missing = TRUE")
    }
    .tt_sample_population(id, "tt_rates", scope, variable = event, group = group,
      weight_type = if (is.null(fweight)) "none" else "frequency",
      values = values, bases = c(list(input_n = "input data records", eligible_n = "valid follow-up, groups and weights",
        observed_n = "nonmissing events within eligible records", used_n = "eligible records including missing events",
        missing_n = "missing events within eligible records", excluded_n = "input records minus used records",
        zero_weight_n = "zero weights in original input records for this population"), weights$bases),
      reasons = weights$reasons, statuses = weights$statuses, exclusions = do.call(rbind, exclusions))
  }
  samples <- list(population(rep(TRUE, nrow(data)), "rates", "table"))
  if (length(groups)) for (k in seq_len(nrow(out))) {
    sel <- Reduce(`&`, lapply(groups, function(g) {
      value <- out[[g]][k]
      if (is.na(value)) is.na(data[[g]]) else !is.na(data[[g]]) & data[[g]] == value
    }))
    samples[[length(samples) + 1L]] <- population(sel, paste0("rates/group", k), "group", paste0("group", k))
  }
  .tt_sample_bind(samples)
}

#' Standardise a data frame in strate output shape
#'
#' `level = NULL` reads the level of the supplied intervals from the
#' `"label"` attributes of `_Lower`/`_Upper` (strate writes "Lower 95%
#' confidence limit"; `stratetab.ado:324-356` reads it the same way); the
#' result's `level` attribute is `NA` when neither label names one. Labels
#' that disagree, or a `level` that contradicts them, are errors.
#' @keywords internal
#' @noRd
tt_rates_from_strate <- function(data, by = NULL, per = 1, level = NULL) {
  sample <- attr(data, "sample_accounting", exact = TRUE)
  if (!is.null(sample)) .tt_validate_sample_accounting(sample)
  # The file's person-time and rates are already in the units strate saved
  # them in; `per` cannot rescale them after the fact (Phase 7b review F7c).
  if (!identical(per, 1) && !identical(per, 1L)) {
    cli::cli_abort(c("{.arg per} applies to time and event data only.",
                     "i" = "A strate-shaped data frame holds person-time and rates in the units it was saved with; scale them in {.fn stratetab} ({.arg pyscale}, {.arg ratescale})."),
                   call = NULL)
  }
  map <- c(D = "_D", Y = "_Y", Rate = "_Rate", Lower = "_Lower", Upper = "_Upper")
  # A Stata strate file whose grouping variable is named Y (or D, Rate, ...)
  # holds both Y and _Y: strate's statistic is _Y, and Y is the group. R's
  # result names the statistic Y, so the grouping column must be renamed
  # (Codex audit D4; before, Y was read as the person-time).
  both <- names(map)[names(map) %in% names(data) & map %in% names(data)]
  .rate_check_reserved(both, strate_file = TRUE)
  for (nm in names(map)) {
    if (!nm %in% names(data) && map[[nm]] %in% names(data)) names(data)[names(data) == map[[nm]]] <- nm
  }
  need <- names(map)
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stata_names <- unname(map[miss])
    cli::cli_abort(c("{.arg data} is not in strate shape.",
                     "x" = "Missing column{?s}: {.field {miss}}.",
                     "i" = "Stata's names are also accepted: {.field {stata_names}}."),
                   call = NULL)
  }
  # Read before the columns are converted (as.numeric() drops attributes).
  label_level <- .rate_label_level(data)
  if (!is.null(level)) {
    level <- .rate_check_level(level)
    if (!is.na(label_level) && abs(level * 100 - label_level) > 1e-8) {
      cli::cli_abort(c("{.arg level} ({level * 100}%) conflicts with the {label_level}% intervals the columns are labelled with.",
                       "i" = "The interval labels record the level the rates were computed at."), call = NULL)
    }
  } else {
    level <- if (is.na(label_level)) NA_real_ else label_level / 100
  }
  for (nm in need) {
    x <- data[[nm]]
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      # Stata's missing "." (and an empty field) read as NA; any other text
      # is an error rather than a silent NA (H15, F20).
      txt <- trimws(x)
      miss <- is.na(txt) | txt %in% c("", ".", "NA")
      num <- suppressWarnings(as.numeric(txt))
      # Decimal numbers only: as.numeric() also reads hex ("0x10") and "Inf".
      decimal <- grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", txt)
      bad <- !miss & (is.na(num) | !decimal)
      if (any(bad)) {
        cli::cli_abort(c("Column {.field {nm}} must be numeric.",
                         "x" = "It holds {.val {unique(x[bad])}}."), call = NULL)
      }
      num[miss] <- NA_real_
      x <- num
    }
    if (!is.numeric(x)) cli::cli_abort("Column {.field {nm}} must be numeric.", call = NULL)
    x <- as.numeric(x)
    if (any(is.infinite(x) | is.nan(x))) cli::cli_abort("Column {.field {nm}} must be finite.", call = NULL)
    if (any(x < 0, na.rm = TRUE)) cli::cli_abort("Column {.field {nm}} must be non-negative.", call = NULL)
    data[[nm]] <- x
  }
  if (is.null(by)) by <- setdiff(names(data), need)
  .rate_check_cols(data, by)
  out <- data[, c(by, need), drop = FALSE]
  rownames(out) <- NULL
  if (is.null(sample)) {
    unknown <- c("input_n", "eligible_n", "observed_n", "used_n", "missing_n", "excluded_n",
                 "reported_n", "zero_weight_n", "weight_sum", "effective_n")
    sample <- .tt_sample_population("rates", "tt_rates", "summary", weight_type = "unknown",
      values = as.list(stats::setNames(rep(NA_real_, length(unknown)), unknown)),
      reasons = as.list(stats::setNames(rep("supplied aggregated rates do not retain record counts or weights",
                                           length(unknown)), unknown)))
  }
  structure(out, per = per, level = level, ci_method = "lognormal",
            by = by, strata = NULL, sample_accounting = sample)
}

# The confidence level (percent) the `_Lower`/`_Upper` variable labels name,
# NA when neither does (stratetab.ado:324-344: the first "<number>%" in the
# lower-cased label; labels that disagree are an error, r(459)). strate
# writes the level with strsubdp() (strate.ado:444-447), so under Stata's
# `set dp comma` a 97.5% interval is labelled "Lower 97,5% confidence
# limit". Up to tabtools 2.1.13 Stata's regex took "5%" from that and
# labelled the table "5% CI", with rate-ratio intervals computed at 5%. R
# reads the comma as the decimal point (Phase 7b review F1), as Stata does
# since 2.1.14 (stratetab.ado:341-346).
.rate_label_level <- function(data) {
  one <- function(nm) {
    lbl <- attr(data[[nm]], "label", exact = TRUE)
    if (!is.character(lbl) || length(lbl) != 1L || is.na(lbl)) return(NA_real_)
    m <- regmatches(tolower(lbl), regexec("([0-9]+([.,][0-9]+)?)%", tolower(lbl)))[[1]]
    if (length(m)) as.numeric(chartr(",", ".", m[2])) else NA_real_
  }
  lo <- one("Lower")
  hi <- one("Upper")
  if (!is.na(lo) && !is.na(hi) && abs(lo - hi) > 1e-8) {
    cli::cli_abort("The interval columns are labelled with conflicting confidence levels ({lo}% and {hi}%).",
                   call = NULL)
  }
  if (is.na(lo)) hi else lo
}


# A numeric analysis column: not a factor or string, no Inf/NaN.
.rate_numeric <- function(data, col, arg) {
  x <- data[[col]]
  if (!is.numeric(x) && !is.logical(x) || is.factor(x)) {
    cli::cli_abort("{.arg {arg}} column {.field {col}} must be numeric.", call = NULL)
  }
  x <- as.numeric(unclass(x))
  if (any(is.infinite(x) | is.nan(x))) {
    cli::cli_abort(c("{.arg {arg}} column {.field {col}} must be finite.",
                     "i" = "Use {.code NA} for a missing value."), call = NULL)
  }
  x
}

# Frequency weights as Stata's [fweight=] takes them: non-negative integers
# (r(402) negative, r(401) non-integer); zero and missing drop the record.
.rate_fweight <- function(data, col) {
  w <- .rate_numeric(data, col, "fweight")
  if (any(w < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg fweight}: negative weights encountered.", call = NULL)
  }
  if (any(w != round(w), na.rm = TRUE)) {
    cli::cli_abort("{.arg fweight}: may not use noninteger frequency weights.", call = NULL)
  }
  w
}

.rate_ci <- function(out, D, Y, level) {
  if (any(!is.finite(D)) || any(!is.finite(Y) | Y <= 0)) {
    cli::cli_abort(c("Rate totals must contain finite event counts and positive, finite person-time.",
                     "i" = "Check the time and weight scales; person-time can underflow when stored as single precision ({.arg float_time})."),
                   class = "tabtools_error_rate_totals", call = NULL)
  }
  z <- stats::qnorm(0.5 + level / 2)
  rate <- D / Y
  se <- sqrt(1 / D)
  lo <- ifelse(D == 0, NA_real_, exp(log(rate) - z * se))
  hi <- ifelse(D == 0, NA_real_, exp(log(rate) + z * se))
  if (any(!is.finite(rate)) || any(D > 0 & (!is.finite(lo) | !is.finite(hi)))) {
    cli::cli_abort(c("The rate or its confidence bounds overflowed.",
                     "i" = "Use less extreme time and weight scales."),
                   class = "tabtools_error_rate_totals", call = NULL)
  }
  out$D <- D
  out$Y <- Y
  out$Rate <- rate
  out$Lower <- lo
  out$Upper <- hi
  out
}

.rate_sort_key <- function(x) {
  if (is.factor(x)) return(as.integer(x))
  if (is.character(x)) return(match(x, sort(unique(x), method = "radix")))
  as.numeric(x)
}

# The result's statistic columns; a grouping column of one of these names
# was overwritten by the statistic, its labels lost while attr "by" still
# named it (Codex audit D4). Stata's strate names its results _D, _Y,
# _Rate, _Lower, _Upper and refuses a grouping variable of those names
# (r(110), probed 2026-09-26); R's results drop the underscore, so R
# refuses these names.
.rate_stat_names <- c("D", "Y", "Rate", "Lower", "Upper")

.rate_check_reserved <- function(groups, strate_file = FALSE) {
  hit <- intersect(groups, .rate_stat_names)
  if (!length(hit)) return(invisible(groups))
  first <- if (strate_file) {
    "The strate data hold both {.field {paste0('_', hit)}} and a grouping column named {.field {hit}}, which the result uses for its statistic{?s}."
  } else {
    "{.arg by}/{.arg strata} name{?s} {.field {hit}}, which the result uses for its statistics."
  }
  cli::cli_abort(c(first,
                   "i" = "The result's columns are {.field {(.rate_stat_names)}}; rename {.field {hit}} first, e.g. {.code names(d)[names(d) == \"{hit[1]}\"] <- \"{hit[1]}_group\"}."),
                 call = NULL)
}

.rate_check_cols <- function(data, cols) {
  bad <- setdiff(cols, names(data))
  if (length(bad)) cli::cli_abort("Column{?s} not found in {.arg data}: {.field {bad}}.", call = NULL)
  .tt_check_unambiguous(data, cols, "data")
}

# A proportion in (0, 1) or a percentage in (1, 100), as regtab() takes it
# (Phase 7b review F7a); returned as a proportion.
.rate_check_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 100 ||
      level == 1) {
    cli::cli_abort("{.arg level} must be a proportion in (0, 1) or a percentage in (1, 100).", call = NULL)
  }
  if (level > 1) level <- round(level, 10) / 100
  level
}
