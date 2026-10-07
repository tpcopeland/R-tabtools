# comptab and hrcomptab (task 7.5): composite tables from regtab and
# effecttab results -- rows picked from several model tables stacked into
# one table ("vertical" mode), or the cohort-study "Table 2", a stratetab
# rate table with the hazard ratios of selected model rows beside each
# outcome's events, person-years and rates ("rate" mode). Port of Stata
# tabtools 2.1.12 comptab.ado (3,115 lines) and hrcomptab.ado (its
# 28-line wrapper).
#
# Stata reads the source frames that regtab/effecttab/stratetab save with
# frame(): the displayed cells (A = label, c1..cK) and the characteristics
# naming each model's identity (comptab.ado:683-774 in rate mode,
# :2286-2432 in vertical mode). R reads the same from the tt_table objects
# (their cells and $meta$frame) or from as.data.frame() of them (the
# header rows, then the body, with the characteristics as attributes).

#' Composite tables from model tables, and the rate + hazard-ratio "Table 2"
#'
#' Builds one table from tables you already have, without refitting
#' anything: the cohort-study "Table 2" of incidence rates beside hazard
#' ratios (rate mode), or selected rows of several model tables stacked into
#' one (vertical mode). R implementation of the Stata `comptab` and
#' `hrcomptab` commands.
#'
#' * **Rate mode** (`ratetable` given; [hrcomptab()] always): the rows of
#'   a [stratetab()] rate table -- events, person-years and the rate with
#'   its interval, per outcome and exposure category -- with two columns
#'   added per outcome, the hazard ratio (with its interval) and its
#'   p-value, taken from selected rows of one or more [regtab()] tables:
#'   the cohort-study "Table 2". The first category of each exposure block
#'   that no model row fills is the reference and shows `reflabel`.
#' * **Vertical mode** (no `ratetable`): selected rows of several
#'   [regtab()] or [effecttab()] tables stacked into one table, with
#'   optional section headings, relabelled rows and separators, and the
#'   estimate and interval merged into one column with `compact`.
#'
#' @section Selecting rows:
#' Give either `rows` or `rownames`, one selection per model table.
#' * `rows`: body row numbers of each model table (row 1 is the first row
#'   below the two header rows, counting factor headings and reference
#'   rows): a list of integer vectors, `list(1, 3:5)`, or Stata's string
#'   `"1 \\ 3/5"`. With a single model table a plain vector will do.
#' * `rownames`: case-insensitive patterns matched against the displayed
#'   row labels, as substrings (`"dose"` matches `"  Low dose"`, and
#'   `"male"` matches `"  Female"`), with
#'   Stata's wildcards `*` (any text) and `?` (one character): a list of
#'   character vectors, one per model table, each element one pattern
#'   (`list("hrt", c("low", "medium", "high"))`), or Stata's string
#'   `"hrt \\ low medium high"`. As in Stata, a table's patterns given as
#'   one string are split at blanks (`"low high"` is two patterns; quote a
#'   phrase, `'"2 or more"'`), while a vector of several strings is taken
#'   element by element (`c("low", "2 or more")`). Every pattern must match
#'   at least one row, and a pattern may match several; a blank pattern is
#'   refused (in Stata it selects every row). With `rownames_exact = TRUE`
#'   a pattern must match a whole label instead (ignoring case and the
#'   label's indent; `*` and `?` still apply), so `"treated"` selects
#'   `Treated` but not `Untreated`.
#'
#' In vertical mode the rows of each table are shown in the table's own
#' order, each once, whatever the order of the selection. In rate mode the
#' selected rows are consumed exposure block by exposure block, in the rate
#' table's order: a block with `k` categories takes the next `k - 1` rows,
#' so the total must be the number of categories minus one per block.
#' Within a block each row goes to the category with the same label
#' (ignoring case and surrounding blanks), never by position, so the order
#' inside a block does not matter; the category left over is the reference,
#' and for a factor level it must be that model's own base level (a model
#' fitted with another base level shows its reference on that category).
#' The one positional case is a plain row, such as a 0/1 indicator, whose
#' label matches no category of a two-category block: it fills the second
#' category, so code the indicator 1 for that category. Everything that
#' cannot be placed exactly is an error: a heading or reference row, a
#' level that matches no category, two rows for one category, or a
#' reference category that is not the model's base level.
#'
#' @section Matching rates to models:
#' Each model table must hold one model per outcome of the rate table, and
#' they are matched by identity, never by position: the rate table's
#' `outcomeids` against each model's outcome identity (for a
#' `survival::coxph()` fit, the event variable of `Surv()`), both
#' lower-cased and trimmed. `outcomemap` names, in the rate table's outcome
#' order, the model to use for each outcome instead: a model identity,
#' outcome identity or model label (as `models =` gave it) that must match
#' exactly one model in every model table. The models must be on the
#' hazard-ratio scale (the estimate header `HR`, `aHR`, `Hazard ratio` or
#' `Adjusted hazard ratio`, as `coef =` sets it) and at the rate table's
#' confidence level; rate tables with `rateratio = TRUE` are refused, as in
#' Stata.
#'
#' In vertical mode the model columns of every table are aligned to the
#' first table's: by outcome identity when those are unique and non-blank,
#' else by model label, else by model identity; every table must hold the
#' same models, at the same confidence level and with the same statistics.
#'
#' @section Differences from Stata:
#' * R has no frames: the model and rate tables are `tt_table` objects (or
#'   [as.data.frame()] of them), and the result stands for `frame()`. The
#'   stored `rateframe` and `modelframes` hold the arguments as written.
#' * Like the other commands, R prints no `Exported ...` or `Markdown
#'   exported to ...` lines.
#' * Rate mode refuses an [effecttab()] model of differences, predictions
#'   or raw log ratios even when its `effect` header says HR. A log-ratio
#'   comparison must record an explicit `transform = exp` (or `"exp"`)
#'   to establish its ratio scale; an unevaluated transform symbol or custom
#'   function cannot establish it. Alternatively supply a data frame of
#'   exponentiated estimates and intervals. Rate mode also refuses an
#'   indicator row (not a factor level) for a block whose two categories
#'   read Yes/No, 1/0 or TRUE/FALSE, where Stata's rule (the indicator is
#'   the second category) would show it under the "No" category.
#' * The stored `statistic_ids` of a vertical table merged with `compact`
#'   is `"estimate_ci pvalue"`; Stata keeps the sources' `"estimate ci
#'   pvalue"`.
#' * R's model identities differ from Stata's command lines, and a
#'   `survival::coxph()` fit's outcome identity is its event variable
#'   (Stata's `stcox` models all have `_t`), so rate tables and Cox models
#'   match without `outcomemap` when `outcomeids` name the event variables.
#'   A model table without model identities (an [effecttab()] of a data
#'   frame) is refused only when the models have to be aligned by identity.
#' * `forest` (drawing the plot) is not ported; [as_forest_data()] gives
#'   the data. For a vertical table its `model`, `model_label` and `label`
#'   are the composite's, as the table shows them: a model table whose
#'   models were aligned in another order keeps each model in its
#'   composite column, and `relabel` text replaces the row label. Stata's
#'   `eplotframe()` copies the source table's model index and labels; R
#'   keeps that index in `source_model`.
#' * `rows`, `rownames`, `section`, `outcomemap` and `relabel` take R
#'   vectors and lists as well as Stata's strings.
#' * `rownames_exact` is R only: Stata's `rownames()` always matches
#'   substrings.
#'
#' @param ratetable A [stratetab()] table (or [as.data.frame()] of one)
#'   for rate mode; `NULL` for vertical mode. When `modeltables` is not
#'   given and `ratetable` is not a rate table, it is taken as the model
#'   tables (`comptab(list(m1, m2), rows = ...)`).
#' @param modeltables The model tables: a list of [regtab()] or
#'   [effecttab()] tables (or [as.data.frame()] of them), or one table.
#' @param rows,rownames Row selections, one per model table (see Selecting
#'   rows). Give one of the two: `rows` takes body row numbers, `rownames`
#'   case-insensitive patterns matched anywhere in the displayed labels, so
#'   `rownames = "male"` also selects `Female` and `"treated"` also selects
#'   `Untreated`. When a pattern is part of another label, use `rows`, a
#'   pattern only the wanted label contains, or `rownames_exact = TRUE`.
#' @param rownames_exact `TRUE` to make each `rownames` pattern match a
#'   whole displayed label (case-insensitive, `*` and `?` wildcards kept)
#'   rather than any part of one. Default `FALSE`, Stata's substring match.
#'   R only.
#' @param effect Rate mode: the effect header, `"aHR"` (default), `"HR"`,
#'   `"hazard ratio"` or `"adjusted hazard ratio"` (case, blanks and
#'   `-_./` ignored); other scales are refused.
#' @param reflabel Rate mode: the text of the reference rows (default
#'   `"Reference"`).
#' @param outcomemap Rate mode: one model identity per outcome of the rate
#'   table (see Matching rates to models); a character vector or Stata's
#'   `"a \\ b"`.
#' @param compact Vertical mode: merge each model's estimate and interval
#'   into one column (the sources' header text joined by a space).
#' @param separator Vertical mode: body row numbers (sections included)
#'   that get a rule above them; rows outside the table are ignored.
#' @param section Vertical mode: one heading per model table, shown in
#'   bold above its rows with a rule above.
#' @param relabel Vertical mode: new labels for body rows (sections
#'   included), as a character vector named by row number
#'   (`c("3" = "Low dose (vs. none)")`) or Stata's string `'3 "Low dose"'`.
#' @param highlight,boldp Vertical mode: shade rows with a p-value below
#'   `highlight`, and bold p-values below `boldp` (numbers in (0, 1);
#'   `boldp` defaults to [tabtools_options()]). `<0.001` counts as 0.
#' @param labelwidth Vertical mode: the widest label column in characters
#'   (default 45); longer labels wrap.
#' @param xlsx Output `.xlsx` workbook; the sheet is replaced if it exists
#'   (matched without regard to case) and added otherwise.
#' @param sheet Sheet name (default `"Composite"`).
#' @param title Title in cell `A1` (and above the console listing, the
#'   first CSV row and the Markdown heading). Rate mode defaults to the
#'   rate table's title.
#' @param footnote Footnote below the table.
#'   Footnotes accept a character vector of paragraphs. The reserved token
#'   `" \\ "` (one backslash with surrounding spaces) splits each element,
#'   including a scalar, into paragraphs. Split pieces are trimmed and empty
#'   pieces dropped; unspaced and doubled backslashes remain literal.
#'   Automatic notes are separate paragraphs in every sink.
#' @param font,fontsize Font family and size (defaults from
#'   [tabtools_options()], else Arial 10).
#' @param borderstyle `"default"`/`"thin"`, `"medium"`, or `"academic"`.
#' @param headershade Shade the two header rows.
#' @param headercolor,zebracolor Colours for `headershade` and `zebra`.
#' @param zebra Shade every second body row, starting with the second.
#' @param csv Also write the table to this `.csv` file.
#' @param markdown Also write the table to this Markdown file (`.md`,
#'   `.markdown`, `.qmd` or `.rmd`).
#' @param mdappend Append to an existing `markdown` file.
#' @param open Open the workbook after writing (interactive sessions).
#' @return A `tt_table` (`command = "comptab"`, or `"hrcomptab"` in rate
#'   mode), returned invisibly when `xlsx`, `csv` or `markdown` writes a file
#'   (assign it and print it to see it). `$stored` holds Stata's
#'   `r()` results. Vertical mode: `N_rows` (title, two header rows and
#'   the body), `N_cols` (the worksheet columns), `N_models`, `N_frames`,
#'   `ci_level`, `methods`. Rate mode: `N_rows`, `N_outcomes`,
#'   `N_sections`, `N_modelrows` (the selected model rows),
#'   `N_modelframes`, `ci_level`, `effect`, `rateframe`, `modelframes`.
#'   Both: for the files written, `xlsx`, `sheet`, `csv`, `markdown`,
#'   `markdown_rows` and `markdown_cols`.
#'
#'   [as.data.frame()] of the result carries the frame characteristics of
#'   Stata's `frame()` as attributes: `source` (`"comptab"` or
#'   `"hrcomptab"`), `ci_level`, `statistic_ids`, `n_models` (vertical) or
#'   `n_outcomes` (rate), and per model `model_id`, `outcome_id` and
#'   `effect_scale`. Vertical composites also retain `effect_additive`
#'   and `effect_log_scale` from every source section, as described in
#'   [effecttab()], so later HR composition still checks the source scales.
#'   [as_forest_data()] gives Stata's `eplotframe()`:
#'   the selected estimates with section and reference rows, when every
#'   model table is a `tt_table`.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @seealso [stratetab()] and [tt_rates()] for the rates; [regtab()] and
#'   [effecttab()] for the models; [stacktab()] to stack whole sheets.
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' d <- survival::lung
#' d$event <- d$status - 1
#' d$years <- d$time / 365.25
#' d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
#' d$ecog <- factor(pmin(d$ph.ecog, 2), 0:2, c("0", "1", "2+"))
#' d <- d[!is.na(d$ecog), ]
#'
#' # Events, person-years and rates per 100 person-years by sex
#' rates <- stratetab(tt_rates(d, "years", "event", by = "sex"),
#'                    outlabels = "Death", outcomeids = "event",
#'                    explabels = "Sex", ratescale = 100, unitlabel = "100")
#'
#' # Crude and adjusted hazard ratios, one model per table
#' crude <- regtab(survival::coxph(survival::Surv(years, event) ~ sex, data = d,
#'                                 ties = "breslow"), coef = "HR")
#' adj <- regtab(survival::coxph(survival::Surv(years, event) ~ sex + age + ecog, data = d,
#'                               ties = "breslow"), coef = "HR")
#'
#' # Table 2: the rate table with the adjusted HR of Female vs Male
#' hrcomptab(rates, adj, rownames = "female")
#'
#' # Vertical composite: the sex rows of both models, stacked
#' comptab(list(crude, adj), rownames = list("female", "female"),
#'         section = c("Crude", "Adjusted for age and ECOG"), compact = TRUE)
#' @section Session destinations:
#' An explicitly supplied non-NULL `sheet` enables inherited workbook and
#' Markdown destinations from [tabtools_options()]. Default or NULL sheet does
#' not request inherited output. Explicit paths still export, and explicit NULL
#' destinations opt out. A requested sheet without a workbook gives
#' `tabtools_warning_sheet_without_workbook` before output; `options(warn = 2)`
#' interrupts the call. See [tabtools_options()] for append/history behavior.
#'
#' @export
comptab <- function(ratetable = NULL, modeltables = NULL, rows = NULL, rownames = NULL,
                    effect = NULL, reflabel = NULL, outcomemap = NULL,
                    compact = FALSE, separator = NULL, section = NULL, relabel = NULL,
                    highlight = NULL, boldp = NULL, labelwidth = NULL,
                    xlsx = NULL, sheet = "Composite", title = NULL, footnote = NULL,
                    font = NULL, fontsize = NULL, borderstyle = NULL, headershade = FALSE,
                    headercolor = NULL, zebra = FALSE, zebracolor = NULL,
                    csv = NULL, markdown = NULL, mdappend = FALSE, open = FALSE,
                    rownames_exact = FALSE) {
  .ct_check_xlsx(xlsx)
  sinks <- .tt_resolve_sinks(
    list(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, headershade = headershade),
    list(xlsx = !missing(xlsx), markdown = !missing(markdown),
         mdappend = !missing(mdappend), sheet = !missing(sheet),
         headershade = !missing(headershade)), policy = "sheet")
  xlsx <- sinks$values$xlsx
  markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend
  # sheet = NULL is no sheet: the default (review P2-2).
  if (is.null(sheet)) sheet <- "Composite"
  rate_label <- .ct_arg_label(substitute(ratetable))
  model_label <- .ct_arg_label(substitute(modeltables))
  if (!is.null(ratetable) && !.ct_is_rate(ratetable)) {
    if (!is.null(modeltables)) {
      # comptab(m1, m2, rows = ...), the word-for-word translation of Stata's
      # `comptab s1 s2, rows()`, would bind m2 to `modeltables` and m1 to
      # `ratetable` (Phase 7c review P2-3).
      cli::cli_abort(c("{.arg ratetable} is not a {.fn stratetab} rate table, and {.arg modeltables} is given too.",
                       "i" = "To stack model tables, pass them as one list: {.code comptab(list(m1, m2), rows = ...)}.",
                       "i" = "For the rate + hazard-ratio table, the first argument is the {.fn stratetab} table: {.code comptab(rates, list(m1, m2), ...)}."),
                     call = NULL)
    }
    modeltables <- ratetable
    model_label <- rate_label
    ratetable <- NULL
  }
  args <- list(rows = rows, rownames = rownames, rownames_exact = rownames_exact,
               effect = effect, reflabel = reflabel, outcomemap = outcomemap, compact = compact, separator = separator, section = section,
               relabel = relabel, highlight = highlight, boldp = boldp, labelwidth = labelwidth,
               xlsx = xlsx, sheet = sheet, title = title, footnote = footnote, font = font,
               fontsize = fontsize, borderstyle = borderstyle, headershade = headershade,
               headercolor = headercolor, zebra = zebra, zebracolor = zebracolor, csv = csv,
               markdown = markdown, mdappend = mdappend, open = open)
  for (a in c("rownames_exact", "compact", "headershade", "zebra", "mdappend", "open")) {
    v <- args[[a]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  models <- .ct_model_list(modeltables)
  if (!is.null(ratetable)) {
    # comptab.ado:177-183: the vertical-only options are refused in rate mode.
    vert <- c(compact = isTRUE(compact), separator = !is.null(separator), section = !is.null(section),
              relabel = !is.null(relabel), highlight = !is.null(highlight), boldp = !is.null(boldp),
              labelwidth = !is.null(labelwidth))
    if (any(vert)) {
      cli::cli_abort("{.arg {names(vert)[vert]}} {?is/are} not allowed with a rate table ({.arg compact}, {.arg separator}, {.arg section}, {.arg relabel}, {.arg highlight}, {.arg boldp} and {.arg labelwidth} are vertical-mode options).",
                     call = NULL)
    }
    if (!length(models)) cli::cli_abort("{.arg modeltables} requires at least one model table.", call = NULL)
    return(.ct_rates(ratetable, models, args, rate_label, .ct_model_names(modeltables, models, model_label)))
  }
  if (!is.null(effect) || !is.null(reflabel) || !is.null(outcomemap)) {
    cli::cli_abort("{.arg effect}, {.arg reflabel} and {.arg outcomemap} require a rate table ({.arg ratetable}).",
                   call = NULL)
  }
  if (!length(models)) cli::cli_abort("At least one model table is required.", call = NULL)
  .ct_vertical(models, args, .ct_model_names(modeltables, models, model_label))
}

#' @rdname comptab
#' @param ... Further arguments of [comptab()] (the output, content and
#'   formatting arguments; the vertical-mode ones are refused, as in rate
#'   mode).
#' @details `hrcomptab()` is `comptab()` in rate mode with a required rate
#'   table, as Stata's `hrcomptab` is a wrapper of `comptab, rateframe()`.
#' @export
hrcomptab <- function(ratetable, modeltables, rows = NULL, rownames = NULL, effect = NULL,
                      reflabel = NULL, outcomemap = NULL, ..., rownames_exact = FALSE) {
  if (missing(ratetable) || is.null(ratetable)) {
    cli::cli_abort("{.arg ratetable} is required: a {.fn stratetab} table.", call = NULL)
  }
  if (missing(modeltables) || is.null(modeltables)) {
    cli::cli_abort("{.arg modeltables} requires at least one model table.", call = NULL)
  }
  if (!.ct_is_rate(ratetable)) {
    cli::cli_abort(c("{.arg ratetable} must be a {.fn stratetab} table (or {.fn as.data.frame} of one).",
                     "i" = "For a composite of model tables alone, use {.fn comptab}."), call = NULL)
  }
  rate_label <- .ct_arg_label(substitute(ratetable))
  model_label <- .ct_arg_label(substitute(modeltables))
  dots <- list(...)
  bad <- intersect(names(dots), c("ratetable", "modeltables"))
  if (length(bad)) cli::cli_abort("{.arg {bad}} given twice.", call = NULL)
  # Refused only when set: compact = FALSE or separator = NULL asks for
  # nothing (Phase 7c review P3-3).
  vert <- intersect(names(dots), c("compact", "separator", "section", "relabel", "highlight", "boldp", "labelwidth"))
  vert <- vert[vapply(vert, function(a) if (a == "compact") !isFALSE(dots[[a]]) else !is.null(dots[[a]]), TRUE)]
  if (length(vert)) {
    cli::cli_abort("{.arg {vert}} {?is/are} not allowed with a rate table ({.fn hrcomptab} is rate mode).", call = NULL)
  }
  args <- utils::modifyList(list(rows = rows, rownames = rownames, rownames_exact = rownames_exact,
                                 effect = effect, reflabel = reflabel,
                                 outcomemap = outcomemap, compact = FALSE, xlsx = NULL, sheet = "Composite",
                                 title = NULL, footnote = NULL, font = NULL, fontsize = NULL,
                                 borderstyle = NULL, headershade = FALSE, headercolor = NULL, zebra = FALSE,
                                 zebracolor = NULL, csv = NULL, markdown = NULL, mdappend = FALSE,
                                 open = FALSE),
                            dots, keep.null = TRUE)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    cli::cli_abort("Every argument after {.arg outcomemap} must be named.", call = NULL)
  }
  unknown <- setdiff(names(dots), names(formals(comptab)))
  if (length(unknown)) cli::cli_abort("Unknown argument{?s} {.arg {unknown}} to {.fn hrcomptab}.", call = NULL)
  .ct_check_xlsx(args$xlsx)
  sinks <- .tt_resolve_sinks(args,
    stats::setNames(as.list(c("xlsx", "markdown", "mdappend", "sheet", "headershade") %in% names(dots)),
                    c("xlsx", "markdown", "mdappend", "sheet", "headershade")), policy = "sheet")
  args <- sinks$values
  if (is.null(args$sheet)) args$sheet <- "Composite"
  for (a in c("rownames_exact", "headershade", "zebra", "mdappend", "open")) {
    v <- args[[a]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  models <- .ct_model_list(modeltables)
  if (!length(models)) cli::cli_abort("{.arg modeltables} requires at least one model table.", call = NULL)
  .ct_rates(ratetable, models, args, rate_label, .ct_model_names(modeltables, models, model_label))
}

# ---------------------------------------------------------------------------
# Arguments and sources

# The argument as the caller wrote it, for the stored rateframe/modelframes.
.ct_arg_label <- function(expr) {
  if (is.null(expr)) return("")
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

# One label per model table: the list's names, else the elements of a
# literal list(...) call, else "<arg>[[k]]".
.ct_model_names <- function(modeltables, models, label) {
  n <- length(models)
  nm <- if (is.list(modeltables) && !is.data.frame(modeltables) && !inherits(modeltables, "tt_table")) names(modeltables)
  expr <- tryCatch(str2lang(label), error = function(e) NULL)
  lit <- if (is.call(expr) && identical(expr[[1]], as.name("list")) && length(expr) == n + 1L) {
    vapply(as.list(expr)[-1], function(e) paste(deparse(e, width.cutoff = 500L), collapse = " "), "")
  }
  out <- if (n == 1L && (inherits(modeltables, "tt_table") || is.data.frame(modeltables))) label else
    if (!is.null(lit)) lit else paste0(label, "[[", seq_len(n), "]]")
  if (!is.null(nm)) out[nzchar(nm)] <- nm[nzchar(nm)]
  out
}

.ct_model_list <- function(x) {
  if (is.null(x)) return(list())
  if (inherits(x, "tt_table") || is.data.frame(x)) return(list(x))
  if (is.list(x)) return(unname(x))
  cli::cli_abort("{.arg modeltables} must be a list of {.fn regtab} or {.fn effecttab} tables, not {.obj_type_friendly {x}}.",
                 call = NULL)
}

.ct_is_rate <- function(x) {
  src <- if (inherits(x, "tt_table")) x$meta$frame$source else if (is.data.frame(x)) attr(x, "source")
  identical(src, "stratetab")
}

# Stata's lower(): ASCII letters only.
.ct_lower <- function(x) chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
.ct_trim <- function(x) trimws(x, whitespace = "[ ]")
# Positions of `labels` matching `target`: the exact matches when there
# are any, else those equal ignoring case (Stata's lower() comparison).
.ct_label_match <- function(labels, target) {
  exact <- which(labels == target)
  if (length(exact)) exact else which(.ct_lower(labels) == .ct_lower(target))
}
.ct_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  .ct_lower(.ct_trim(x))
}

# A model table as comptab reads a regtab/effecttab frame: the label
# column, the value cells c1..cK, the two header rows (label column
# first), the frame characteristics, and how its reference rows are known
# (comptab.ado:1074-1085: regtab's numeric ref<k> variable, flagging the
# refcat/omitlabel/emptylabel text; without it, the text "reference").
.ct_source <- function(x, k) {
  if (inherits(x, "tt_table")) {
    fm <- x$meta$frame
    if (is.null(fm) || is.null(fm$n_models)) {
      cli::cli_abort(c("Model table {k} carries no model provenance.",
                       "i" = "Model tables come from {.fn regtab} or {.fn effecttab}."), call = NULL)
    }
    h <- x$header
    if (length(h) < 2L) cli::cli_abort("Model table {k} needs its two header rows.", call = NULL)
    body <- as.matrix(x$body)
    dimnames(body) <- NULL
    refset <- unique(c(x$meta$refcat %||% "Reference", x$meta$omitlabel %||% "Omitted",
                       x$meta$emptylabel %||% "Empty"))
    return(list(labels = body[, 1], cells = body[, -1, drop = FALSE], h1 = h[[1]]$text,
                h2 = h[[length(h)]]$text, frame = fm, refset = refset, rows = x$meta$regtab_rows,
                types = x$rows$type, sample_accounting = x$meta[["sample_accounting", exact = TRUE]]))
  }
  if (is.data.frame(x)) {
    src <- attr(x, "source")
    if (is.null(src) || is.null(attr(x, "n_models"))) {
      cli::cli_abort(c("Model table {k} carries no model provenance.",
                       "i" = "Pass the {.fn regtab} or {.fn effecttab} table, or {.fn as.data.frame} of it."),
                     call = NULL)
    }
    m <- as.matrix(x)
    m[is.na(m)] <- ""
    storage.mode(m) <- "character"
    dimnames(m) <- NULL
    if (nrow(m) < 2L || ncol(m) < 2L) cli::cli_abort("Model table {k} has no header rows.", call = NULL)
    fields <- c("source", "ci_level", "n_models", "statistic_ids", "model_id", "outcome_id",
                "effect_scale", "model_label", "effect_additive", "effect_log_scale")
    fm <- attributes(x)[fields]
    names(fm) <- fields
    body <- m[-(1:2), , drop = FALSE]
    return(list(labels = body[, 1], cells = body[, -1, drop = FALSE], h1 = m[1, ], h2 = m[2, ],
                frame = fm, refset = NULL, rows = NULL, types = NULL,
                sample_accounting = attr(x, "sample_accounting", exact = TRUE)))
  }
  cli::cli_abort("Model table {k} must be a {.fn regtab} or {.fn effecttab} table, not {.obj_type_friendly {x}}.",
                 call = NULL)
}

# Reference flags of body rows `r` in the value column `col`.
.ct_is_ref <- function(s, r, col) {
  v <- s$cells[r, col]
  if (!is.null(s$refset)) v %in% s$refset else .ct_key(v) == "reference"
}

.ct_meta <- function(fm, what, m) {
  v <- fm[[what]]
  if (is.null(v) || length(v) < m) return("")
  v <- as.character(v[m])
  if (is.na(v)) "" else v
}

# Column layout from the statistic header row (comptab.ado:635-682,
# :2173-2218): standard (estimate | CI | p) when every block's second header
# holds "ci" and its third starts with "p"; compact (estimate+CI | p) when
# every block's first holds "ci" and its second starts with "p".
.ct_layout <- function(s, k) {
  n <- ncol(s$cells)
  hdr <- .ct_lower(.ct_trim(s$h2[-1]))
  std <- n > 0L && n %% 3L == 0L &&
    all(vapply(seq.int(1L, n, by = 3L), function(c) grepl("ci", hdr[c + 1L], fixed = TRUE) && startsWith(hdr[c + 2L], "p"), TRUE))
  cmp <- n > 0L && n %% 2L == 0L &&
    all(vapply(seq.int(1L, n, by = 2L), function(c) grepl("ci", hdr[c], fixed = TRUE) && startsWith(hdr[c + 1L], "p"), TRUE))
  if (std && !cmp) return(list(mode = "standard", cpm = 3L, n_models = n %/% 3L))
  if (cmp && !std) return(list(mode = "compact", cpm = 2L, n_models = n %/% 2L))
  cli::cli_abort(c("Model table {k} has an unsupported column structure.",
                   "i" = "Expected 3 columns per model (estimate, CI, p-value) or 2 (estimate with CI, p-value), as {.fn regtab} writes them; tables with {.code nopvalue} cannot be composed."),
                 call = NULL)
}

# Split Stata's "a \ b \ c" at the backslashes (comptab.ado:899-912).
.ct_split_bs <- function(x) {
  parts <- strsplit(x, "\\", fixed = TRUE)[[1]]
  if (grepl("\\\\\\s*$", x)) parts <- c(parts, "")
  parts
}

# Blank-separated tokens of a Stata string, as `foreach ... of local`
# splits them: double quotes (and compound quotes) group, and are removed.
.ct_tokens <- function(x) {
  # A compound quote `"..."' may hold double quotes: it ends at the first "'.
  m <- gregexpr('`".*?"\'|"[^"]*"|[^ ]+', x, perl = TRUE)[[1]]
  if (m[1] < 0) return(character())
  tok <- regmatches(x, list(m))[[1]]
  gsub('^`?"|"\'?$', "", tok)
}

# One selection per model table, as a list.
.ct_selection_list <- function(x, n, arg) {
  if (is.character(x) && length(x) == 1L && grepl("\\", x, fixed = TRUE)) {
    x <- as.list(.ct_split_bs(x))
    if (arg == "rownames") x <- lapply(x, .ct_tokens)
    if (arg == "rows") x <- lapply(x, .ct_numlist)
  } else if (is.list(x)) {
    x <- unname(x)
  } else if (n == 1L) {
    x <- list(x)
  } else {
    cli::cli_abort(c("{.arg {arg}} needs one selection per model table ({n}).",
                     "i" = "Give a list, e.g. {.code list(1, 3:5)}, or Stata's string {.code \"1 \\\\ 3/5\"}."),
                   call = NULL)
  }
  if (length(x) != n) {
    cli::cli_abort("{.arg {arg}} requires {n} specification{?s}, one per model table; found {length(x)}.", call = NULL)
  }
  # A table's patterns given as one string are split at blanks, as Stata
  # splits each rownames() specification ("treated age" is two patterns;
  # quote a phrase: '"2 or more"'); a vector of several strings is taken
  # element by element (Phase 7c review P3-2). rows = "1 6" is a numlist.
  if (arg == "rownames") {
    x <- lapply(seq_along(x), function(f) {
      p <- x[[f]]
      if (is.character(p) && length(p) == 1L && !is.na(p)) {
        if (!nzchar(.ct_trim(p))) cli::cli_abort("{.arg rownames} for model table {f} is blank.", call = NULL)
        p <- .ct_tokens(p)
      }
      p
    })
  }
  x
}

# Stata numlist of row numbers: integers, a/b ranges (either direction),
# a(d)b steps.
.ct_numlist <- function(s) {
  if (is.numeric(s)) return(s)
  toks <- strsplit(.ct_trim(gsub(",", " ", s, fixed = TRUE)), " +")[[1]]
  toks <- toks[nzchar(toks)]
  out <- numeric()
  for (t in toks) {
    if (grepl("^-?[0-9]+$", t)) {
      out <- c(out, as.numeric(t))
    } else if (grepl("^-?[0-9]+/-?[0-9]+$", t)) {
      ab <- as.numeric(strsplit(t, "/", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else if (grepl("^-?[0-9]+\\(-?[0-9]+\\)-?[0-9]+$", t)) {
      p <- as.numeric(regmatches(t, gregexpr("-?[0-9]+", t))[[1]])
      if (p[2] == 0) cli::cli_abort("Invalid numlist element {.val {t}} in {.arg rows}.", call = NULL)
      out <- c(out, seq(p[1], p[3], by = p[2]))
    } else {
      cli::cli_abort("Invalid numlist element {.val {t}} in {.arg rows}.", call = NULL)
    }
  }
  out
}

# Stata strmatch("*<pat>*") on lower-cased text: * any text, ? one
# character, everything else literal. `exact` (rownames_exact, R only)
# anchors the pattern to the whole label, matched against trimmed labels.
.ct_pattern_rx <- function(pat, exact = FALSE) {
  ch <- strsplit(.ct_lower(pat), "")[[1]]
  rx <- vapply(ch, function(c) {
    if (c == "*") ".*" else if (c == "?") "." else if (grepl("[][{}()^$.|+\\\\]", c)) paste0("\\", c) else c
  }, "")
  if (exact) return(paste0("^", paste(rx, collapse = ""), "$"))
  paste0("^.*", paste(rx, collapse = ""), ".*$")
}

# Body rows of labels matched by one rownames pattern.
.ct_pattern_hits <- function(p, labels, exact = FALSE) {
  low <- .ct_lower(labels)
  if (exact) low <- .ct_trim(low)
  which(grepl(.ct_pattern_rx(p, exact), low))
}

# Row selections per table (comptab.ado:898-1003 rate mode, :2058-2162
# vertical): integer vectors of body rows, in selection order.
.ct_expand <- function(sel, use_names, srcs, dup_error, exact = FALSE) {
  lapply(seq_along(srcs), function(f) {
    s <- srcs[[f]]
    nb <- length(s$labels)
    if (nb < 1L) cli::cli_abort("Model table {f} has no data rows.", call = NULL)
    if (use_names) {
      pats <- sel[[f]]
      if (!is.character(pats) || anyNA(pats) || !length(pats)) {
        cli::cli_abort("{.arg rownames} for model table {f} must be a character vector of patterns.", call = NULL)
      }
      # A blank pattern would match every row (Stata's strmatch("**");
      # Phase 7c review P3-1).
      if (any(!nzchar(.ct_trim(pats)))) {
        cli::cli_abort("{.arg rownames} for model table {f} holds a blank pattern, which would select every row.",
                       call = NULL)
      }
      out <- integer()
      for (p in .ct_trim(pats)) {
        hit <- .ct_pattern_hits(p, s$labels, exact)
        if (!length(hit)) {
          cli::cli_abort(c("{.arg rownames}: pattern {.val {p}} not found in model table {f}.",
                           "i" = if (exact) "With {.code rownames_exact = TRUE} a pattern must match a whole displayed row label."
                                 else "Patterns match the displayed row labels, not variable names."), call = NULL)
        }
        if (dup_error && any(hit %in% out)) {
          r <- hit[hit %in% out][1]
          cli::cli_abort(c("{.arg rownames}: pattern {.val {p}} selects row {r} of model table {f} again.",
                           "i" = "The patterns must select each model-table row at most once."), call = NULL)
        }
        out <- c(out, hit)
      }
      out
    } else {
      r <- sel[[f]]
      if (is.character(r)) r <- .ct_numlist(r)
      if (!is.numeric(r) || anyNA(r) || !length(r) || any(r != round(r))) {
        cli::cli_abort("{.arg rows} for model table {f} must be whole row numbers.", call = NULL)
      }
      bad <- r[r < 1 | r > nb]
      if (length(bad)) cli::cli_abort("Row {bad[1]} out of range for model table {f} (valid: 1-{nb}).", call = NULL)
      as.integer(r)
    }
  })
}

# Rate mode: the rownames patterns selected more (or fewer) rows than the
# rate table's non-reference categories. Name each pattern that matched
# several labels, and the labels, since substring matching ("treated" in
# "Untreated") is the usual cause (documentation review D03).
.ct_abort_rownames_count <- function(sel, srcs, n_sel, n_nonref, exact) {
  multi <- character()
  for (f in seq_along(srcs)) {
    for (p in .ct_trim(sel[[f]])) {
      hit <- .ct_pattern_hits(p, srcs[[f]]$labels, exact)
      if (length(hit) > 1L) {
        labs <- .ct_trim(srcs[[f]]$labels[hit])
        multi <- c(multi, cli::format_inline("Pattern {.val {p}} matched {length(hit)} rows of model table {f}: {.val {labs}}."))
      }
    }
  }
  names(multi) <- rep("x", length(multi))
  hints <- if (length(multi)) {
    c("i" = if (exact) "Use {.arg rows} to pick row numbers."
            else "Patterns match anywhere in a label, as Stata's; use {.code rownames_exact = TRUE} to match whole labels, or {.arg rows} to pick row numbers.")
  } else {
    c("i" = "Select one model row for each exposure category but the reference.")
  }
  cli::cli_abort(c("The selected model rows ({n_sel}) must match the non-reference rows of the rate table ({n_nonref}).",
                   multi, hints),
                 class = "tabtools_error_rownames_count", call = NULL)
}

# Preserve the composite boundary diagnostic before shared sink preflight.
.ct_check_xlsx <- function(xlsx) {
  if (!is.null(xlsx) && (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) ||
                        !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must have a .xlsx extension.", call = NULL)
  }
  invisible(NULL)
}

.ct_check_common <- function(a) {
  if (is.null(a$rows) && is.null(a$rownames)) cli::cli_abort("One of {.arg rows} or {.arg rownames} is required.", call = NULL)
  if (isTRUE(a$rownames_exact) && is.null(a$rownames)) {
    cli::cli_abort("{.arg rownames_exact} requires {.arg rownames}.", call = NULL)
  }
  if (!is.null(a$rows) && !is.null(a$rownames)) cli::cli_abort("{.arg rows} and {.arg rownames} may not be combined.", call = NULL)
  has_xlsx <- !is.null(a$xlsx)
  if (a$open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  .ct_check_xlsx(a$xlsx)
  if (!is.null(a$csv)) .tt_check_csv_path(a$csv)
  if (a$mdappend && is.null(a$markdown)) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (!is.null(a$markdown) && (!is.character(a$markdown) || length(a$markdown) != 1L || is.na(a$markdown) ||
                               !grepl("\\.(md|markdown|qmd|rmd)$", tolower(a$markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  for (f in c("title", "footnote")) .tt_check_text_arg(a[[f]], f)
  a$footnote <- .tt_footnote_text(a$footnote)
  invisible(TRUE)
}

# Sinks in Stata's order (CSV, Markdown, then the workbook).
.ct_write <- function(tt, a) {
  written <- FALSE
  if (!is.null(a$csv)) {
    tt_write_csv(tt, a$csv)
    tt$stored$csv <- a$csv
    written <- TRUE
  }
  if (!is.null(a$markdown)) {
    res <- tt_write_markdown(tt, a$markdown, append = a$mdappend)
    tt$stored$markdown <- a$markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (!is.null(a$xlsx)) {
    sheet <- .xlsx_existing_sheet(a$xlsx, tt$meta$sheet)
    tt_write_xlsx(tt, a$xlsx, sheet = sheet, open = a$open)
    tt$stored$xlsx <- a$xlsx
    tt$stored$sheet <- sheet
    tt$meta$sheet <- sheet
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}

# ---------------------------------------------------------------------------
# Vertical mode (comptab.ado:1777-3115)

.ct_vertical <- function(models, a, model_names) {
  .ct_check_common(a)
  style <- tt_resolve_style(font = a$font, fontsize = a$fontsize, borderstyle = a$borderstyle,
                            headershade = a$headershade, zebra = a$zebra, headercolor = a$headercolor,
                            zebracolor = a$zebracolor, boldp = a$boldp, highlight = a$highlight)
  labelwidth <- a$labelwidth %||% 0
  if (!is.numeric(labelwidth) || length(labelwidth) != 1L || is.na(labelwidth) || labelwidth != round(labelwidth)) {
    cli::cli_abort("{.arg labelwidth} must be a whole number.", call = NULL)
  }
  if (labelwidth <= 0) labelwidth <- 45
  sheet <- .check_sheet(a$sheet)
  # separator(numlist >0 integer): whole positive row numbers.
  sep <- a$separator
  if (!is.null(sep) && (!is.numeric(sep) || anyNA(sep) || any(sep != round(sep)) || any(sep <= 0))) {
    cli::cli_abort("{.arg separator} must be positive whole row numbers.", call = NULL)
  }
  n_frames <- length(models)
  srcs <- lapply(seq_len(n_frames), function(f) .ct_source(models[[f]], f))

  use_names <- !is.null(a$rownames)
  sel <- .ct_selection_list(if (use_names) a$rownames else a$rows, n_frames, if (use_names) "rownames" else "rows")
  expanded <- .ct_expand(sel, use_names, srcs, dup_error = FALSE, exact = isTRUE(a$rownames_exact))

  # Column compatibility (comptab.ado:2167-2284).
  lay <- .ct_layout(srcs[[1]], 1L)
  n_models <- lay$n_models
  cpm <- lay$cpm
  for (f in seq_len(n_frames)[-1]) {
    lf <- .ct_layout(srcs[[f]], f)
    if (ncol(srcs[[f]]$cells) != ncol(srcs[[1]]$cells)) {
      cli::cli_abort(c("Column mismatch: model table {f} has {ncol(srcs[[f]]$cells)} data columns, model table 1 has {ncol(srcs[[1]]$cells)}.",
                       "i" = "All model tables must hold the same number of models."), call = NULL)
    }
    if (lf$mode != lay$mode || lf$n_models != n_models) {
      cli::cli_abort(c("All model tables must share the same layout.",
                       "x" = "Model table 1 is {lay$mode} with {n_models} model{?s}; model table {f} is {lf$mode} with {lf$n_models}."),
                     call = NULL)
    }
  }

  # Provenance and alignment (comptab.ado:2286-2432).
  fm1 <- srcs[[1]]$frame
  prov_ok <- function(fm) isTRUE(suppressWarnings(as.numeric(fm$n_models)) == n_models) &&
    length(fm$ci_level) && !is.na(fm$ci_level[1]) && nzchar(as.character(fm$ci_level[1])) &&
    length(fm$statistic_ids) && nzchar(fm$statistic_ids[1] %||% "")
  if (!prov_ok(fm1)) cli::cli_abort("Model table 1 lacks the model, confidence-level or statistic provenance of a {.fn regtab} table.", call = NULL)
  ids <- function(fm, what) vapply(seq_len(n_models), function(m) .ct_key(.ct_meta(fm, what, m)), "")
  oid1 <- ids(fm1, "outcome_id")
  lab1 <- ids(fm1, "model_label")
  mid1 <- ids(fm1, "model_id")
  kind <- if (all(nzchar(oid1)) && !anyDuplicated(oid1)) "outcome" else
    if (all(nzchar(lab1)) && !anyDuplicated(lab1)) "label" else
      if (all(nzchar(mid1)) && !anyDuplicated(mid1)) "model" else NULL
  if (is.null(kind)) {
    if (!all(nzchar(mid1))) cli::cli_abort("Model table 1 has a blank model identity and no unique outcome identities or model labels to align on.", call = NULL)
    cli::cli_abort("Model table 1 has no unique model, outcome or label identity to align its models on.", call = NULL)
  }
  what <- c(outcome = "outcome_id", label = "model_label", model = "model_id")[[kind]]
  align1 <- ids(fm1, what)
  scale1 <- vapply(seq_len(n_models), function(m) .ct_meta(fm1, "effect_scale", m), "")
  ci1 <- suppressWarnings(as.numeric(fm1$ci_level[1]))
  for (f in seq_len(n_frames)[-1]) {
    s <- srcs[[f]]
    fm <- s$frame
    if (!prov_ok(fm)) cli::cli_abort("Model table {f} lacks the model, confidence-level or statistic provenance of a {.fn regtab} table.", call = NULL)
    if (!isTRUE(abs(suppressWarnings(as.numeric(fm$ci_level[1])) - ci1) <= 1e-8)) {
      cli::cli_abort("The model tables are at different confidence levels.", call = NULL)
    }
    if (!identical(fm$statistic_ids[1], fm1$statistic_ids[1])) {
      cli::cli_abort("The model tables hold different statistics (or in a different order).", call = NULL)
    }
    al <- ids(fm, what)
    if (kind == "model" && !all(nzchar(al))) cli::cli_abort("Model table {f} has a blank model identity.", call = NULL)
    if (!all(nzchar(al))) cli::cli_abort("Model table {f} lacks the {kind} identity the models are aligned on.", call = NULL)
    if (anyDuplicated(al)) {
      cli::cli_abort("Duplicate {kind} identity {.val {al[duplicated(al)][1]}} in model table {f} cannot be aligned.", call = NULL)
    }
    map <- match(align1, al)
    if (anyNA(map)) {
      cli::cli_abort("The {kind} identity {.val {align1[is.na(map)][1]}} is missing from model table {f}.", call = NULL)
    }
    oid <- ids(fm, "outcome_id")
    if (!identical(oid1, oid[map])) cli::cli_abort("Model table {f} disagrees with model table 1 on the models' outcome identities.", call = NULL)
    sc <- vapply(seq_len(n_models), function(m) .ct_meta(fm, "effect_scale", m), "")
    if (!identical(.ct_lower(scale1), .ct_lower(sc[map]))) {
      cli::cli_abort("Model table {f} disagrees with model table 1 on the models' effect scales.", call = NULL)
    }
    perm <- as.vector(vapply(map, function(m) (m - 1L) * cpm + seq_len(cpm), integer(cpm)))
    s$cells <- s$cells[, perm, drop = FALSE]
    s$h1 <- c(s$h1[1], s$h1[-1][perm])
    s$h2 <- c(s$h2[1], s$h2[-1][perm])
    s$model_map <- map
    if (!identical(.ct_lower(.ct_trim(srcs[[1]]$h2[-1])), .ct_lower(.ct_trim(s$h2[-1])))) {
      cli::cli_abort("Model table {f} disagrees with model table 1 on the effect scale, confidence level or statistic order of its headers.",
                     call = NULL)
    }
    srcs[[f]] <- s
  }
  srcs[[1]]$model_map <- seq_len(n_models)

  sections <- NULL
  if (!is.null(a$section)) {
    sections <- a$section
    if (is.character(sections) && length(sections) == 1L && n_frames > 1L && grepl("\\", sections, fixed = TRUE)) {
      sections <- .ct_trim(.ct_split_bs(sections))
    }
    if (!is.character(sections) || anyNA(sections)) cli::cli_abort("{.arg section} must be a character vector.", call = NULL)
    if (length(sections) != n_frames) {
      cli::cli_abort("{.arg section} requires {n_frames} label{?s}, one per model table; found {length(sections)}.", call = NULL)
    }
  }
  .tt_preflight_targets(xlsx = a$xlsx, csv = a$csv, markdown = a$markdown, mdappend = a$mdappend)

  # Composite body: [section row] + the selected rows of each table, in
  # the table's own order, each once (comptab.ado:2575-2616).
  nc <- ncol(srcs[[1]]$cells)
  lab <- character()
  cells <- matrix("", 0L, nc)
  rtype <- character()
  is_section <- logical()
  picked <- list()
  body_at <- list()
  sec_at <- rep(NA_integer_, n_frames)
  for (f in seq_len(n_frames)) {
    s <- srcs[[f]]
    if (!is.null(sections)) {
      sec_at[f] <- length(lab) + 1L
      lab <- c(lab, sections[f])
      cells <- rbind(cells, rep("", nc))
      rtype <- c(rtype, "var")
      is_section <- c(is_section, TRUE)
    }
    r <- sort(unique(expanded[[f]]))
    picked[[f]] <- r
    body_at[[f]] <- stats::setNames(length(lab) + seq_along(r), r)
    lab <- c(lab, s$labels[r])
    cells <- rbind(cells, s$cells[r, , drop = FALSE])
    ty <- if (!is.null(s$types)) s$types[r] else ifelse(startsWith(s$labels[r], "  "), "level", "var")
    rtype <- c(rtype, ty)
    is_section <- c(is_section, rep(FALSE, length(r)))
  }
  h1 <- srcs[[1]]$h1
  h2 <- srcs[[1]]$h2

  # compact: estimate + " " + CI (comptab.ado:2623-2662).
  source_compact <- lay$mode == "compact"
  compact_out <- source_compact || a$compact
  if (!source_compact && a$compact) {
    est <- seq.int(1L, nc, by = 3L)
    for (j in est) {
      ci <- cells[, j + 1L]
      has <- ci != ""
      cells[has, j] <- paste(cells[has, j], ci[has])
      h2[j + 1L] <- paste(h2[j + 1L], h2[j + 2L])
    }
    keep <- setdiff(seq_len(nc), est + 1L)
    cells <- cells[, keep, drop = FALSE]
    h1 <- c(h1[1], h1[-1][keep])
    h2 <- c(h2[1], h2[-1][keep])
    nc <- ncol(cells)
  }
  cpm_out <- if (compact_out) 2L else 3L
  nb <- length(lab)

  # relabel(): row numbers count every body row, sections included
  # (comptab.ado:2669-2692).
  relabelled <- character()
  if (!is.null(a$relabel)) {
    rl <- .ct_relabel(a$relabel)
    for (i in seq_along(rl$rows)) {
      r <- rl$rows[i]
      if (r < 1 || r > nb) cli::cli_abort("{.arg relabel} row {r} out of range (valid: 1-{nb}).", call = NULL)
      lab[r] <- rl$labels[i]
      relabelled[as.character(r)] <- .ct_trim(rl$labels[i])
    }
  }

  body <- cbind(lab, cells)
  dimnames(body) <- NULL
  role <- c("label", rep(if (compact_out) c("est_ci", "pval") else c("est", "ci", "pval"), n_models))
  cols <- data.frame(role = role, model = c(NA_integer_, rep(seq_len(n_models), each = cpm_out)),
                     console_width = NA_integer_, stringsAsFactors = FALSE)
  rows <- data.frame(type = ifelse(is_section, "var", rtype), indent = nchar(lab) - nchar(sub("^ +", "", lab)),
                     section = is_section, stringsAsFactors = FALSE)
  rows$block <- cumsum(rows$indent == 0L | seq_len(nb) == 1L)

  stat_ids <- if (compact_out) "estimate_ci pvalue" else fm1$statistic_ids[1]
  # A vertical composite can itself feed rate mode. Its sections may hold
  # different scales despite matching headers, so preserve every source's
  # unsafe scale provenance in the aligned model-column order.
  provenance <- function(field) lapply(seq_len(n_models), function(m) {
    vapply(srcs, function(s) .ct_meta(s$frame, field, s$model_map[m]), "")
  })
  additive <- vapply(provenance("effect_additive"), function(v) {
    if (any(v == "TRUE")) TRUE else if (all(v == "FALSE")) FALSE else NA
  }, NA)
  log_scale <- vapply(provenance("effect_log_scale"), function(v) {
    if (any(v == "log")) "log" else if (any(v == "unknown")) "unknown" else if (any(v == "ratio")) "ratio" else ""
  }, "")
  frame <- list(source = "comptab", ci_level = ci1, n_models = n_models, statistic_ids = stat_ids,
                model_id = vapply(seq_len(n_models), function(m) .ct_meta(fm1, "model_id", m), ""),
                outcome_id = vapply(seq_len(n_models), function(m) .ct_meta(fm1, "outcome_id", m), ""),
                effect_scale = scale1, effect_additive = additive, effect_log_scale = log_scale,
                model_label = vapply(seq_len(n_models), function(m) .ct_meta(fm1, "model_label", m), ""))
  num_rows <- nb + 3L
  num_cols <- nc + 2L
  methods <- paste0("Composite table assembled from ", n_frames, " source frame(s) with ", n_models, " model column(s).")
  if (num_rows > 0) methods <- paste0(methods, " The final table contains ", num_rows, " rows and ", num_cols, " columns.")
  stored <- list(N_rows = num_rows, N_cols = num_cols, N_models = n_models, N_frames = n_frames,
                 ci_level = ci1, methods = methods)
  forest <- .ct_forest_vertical(srcs, picked, sections, frame, n_models, model_names,
                                body_at = body_at, sec_at = sec_at, relabelled = relabelled)
  layout <- list(indent = 2L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "model", xlsx_rules = "comptab",
                 sheet = "Composite", md_header = "first")
  tt <- tt_table(body = body, header = list(list(text = h1), list(text = h2)), rows = rows, cols = cols,
                 title = a$title %||% "", footnote = a$footnote %||% "", style = style, stored = stored,
                 command = "comptab", layout = layout,
                 meta = list(sheet = sheet, frame = frame, labelwidth = labelwidth, cpm = cpm_out,
                             n_models = n_models, compact = compact_out, separator = a$separator,
                             forest = forest$data, forest_error = forest$error,
                             sample_accounting = .tt_sample_bind(
                               lapply(srcs, `[[`, "sample_accounting"),
                               prefixes = paste0("modeltable", seq_along(srcs)),
                               commands = vapply(srcs, function(s) s$frame$source %||% "unknown", ""))))
  .ct_write(tt, a)
}

# relabel(): a character vector named by row number, a list of
# (row, label) pairs, or Stata's '3 "Low dose" 5 "High dose"'.
.ct_relabel <- function(x) {
  if (is.character(x) && is.null(names(x)) && length(x) == 1L) {
    tok <- .ct_tokens(x)
    if (length(tok) %% 2L) cli::cli_abort("{.arg relabel} requires pairs: row number and new label.", call = NULL)
    r <- suppressWarnings(as.numeric(tok[c(TRUE, FALSE)]))
    l <- tok[c(FALSE, TRUE)]
  } else if (is.list(x)) {
    if (!all(lengths(x) == 2L)) cli::cli_abort("{.arg relabel} requires pairs: row number and new label.", call = NULL)
    r <- suppressWarnings(as.numeric(vapply(x, function(p) as.character(p[[1]]), "")))
    l <- vapply(x, function(p) as.character(p[[2]]), "")
  } else if (is.character(x) && !is.null(names(x))) {
    r <- suppressWarnings(as.numeric(names(x)))
    l <- unname(x)
  } else {
    cli::cli_abort("{.arg relabel} must be a character vector named by row number, e.g. {.code c(\"3\" = \"Low dose\")}.", call = NULL)
  }
  if (anyNA(r) || any(r != round(r)) || anyNA(l)) {
    cli::cli_abort("{.arg relabel} requires pairs: a whole row number and a new label.", call = NULL)
  }
  list(rows = as.integer(r), labels = l)
}

# The composite's eplotframe() analogue (comptab.ado:2484-2562): per model
# table, a section row (folded into the one plotted row it would head),
# then every estimate/reference row of the selected rows. Unlike Stata's
# eplotframe, which copies the source table's model index, model label and
# row label, the rows follow the composite as it is shown (codex audit
# F05): `model` is the composite's model column (a model table whose
# models were aligned in another order shows a model in another column),
# `model_label` that column's label, and `label` the row's relabel() text
# when it has one. `source_model` keeps the model's index in its own
# table, beside `source_row` and `source_frame`. `body_at`: composite body
# row of each selected source row; `sec_at`: body row of each section.
.ct_forest_vertical <- function(srcs, picked, sections, frame, n_models, model_names,
                                body_at = NULL, sec_at = NULL, relabelled = character()) {
  final_label <- function(at, default) {
    hit <- if (length(relabelled) && length(at)) relabelled[as.character(at)] else rep(NA_character_, length(default))
    unname(ifelse(is.na(hit), default, hit))
  }
  if (any(vapply(srcs, function(s) is.null(s$rows), TRUE))) {
    return(list(data = NULL, error = "Every model table must be a {.fn regtab} or {.fn effecttab} table (not a data frame) for {.fn as_forest_data}."))
  }
  out <- list()
  for (f in seq_along(srcs)) {
    s <- srcs[[f]]
    r <- s$rows
    r <- r[r$status %in% c("est", "base"), , drop = FALSE]
    r <- r[r$row %in% picked[[f]], , drop = FALSE]
    map <- s$model_map %||% seq_len(n_models)
    pos <- match(as.integer(r$model), map)
    # Each row's models in the composite's column order.
    o <- order(r$row, pos)
    r <- r[o, , drop = FALSE]
    pos <- pos[o]
    # unname(): a named modeltables list names these, and a named length-1
    # column in data.frame() gives base R's short-variable row-name warning
    # (audit 2026-09-29 B04).
    sec <- if (is.null(sections)) "" else unname(sections[f])
    sec_lab <- if (is.null(sections)) "" else unname(final_label(if (is.null(sec_at)) integer() else sec_at[f], sec))
    fold <- !is.null(sections) && nrow(r) == 1L
    if (!is.null(sections) && !fold) {
      out[[length(out) + 1L]] <- data.frame(label = sec_lab, estimate = NA_real_, ll = NA_real_, ul = NA_real_,
                                            pvalue = NA_real_, model = NA_integer_, model_label = "",
                                            rowtype = "section", section = sec, source_row = NA_integer_,
                                            source_model = NA_integer_, source_frame = unname(model_names[f]),
                                            stringsAsFactors = FALSE)
    }
    if (!nrow(r)) next
    est <- r$status == "est"
    num <- function(v) as.numeric(ifelse(est, v, NA_real_))
    ml <- frame$model_label
    if (is.null(ml) || !any(nzchar(ml))) ml <- if (is.null(s$frame$model_label)) NULL else s$frame$model_label[map]
    at <- if (is.null(body_at)) integer() else unname(body_at[[f]][as.character(r$row)])
    out[[length(out) + 1L]] <- data.frame(
      label = if (fold) rep(sec_lab, nrow(r)) else final_label(at, as.character(r$label)),
      estimate = num(r$estimate), ll = num(r$conf.low), ul = num(r$conf.high), pvalue = num(r$p.value),
      model = as.integer(pos),
      model_label = as.character(if (is.null(ml)) rep("", nrow(r)) else ml[pos]),
      rowtype = ifelse(est, "effect", "reference"), section = sec, source_row = as.integer(r$row),
      source_model = as.integer(r$model), source_frame = unname(model_names[f]), stringsAsFactors = FALSE)
  }
  d <- if (length(out)) do.call(rbind, out) else
    data.frame(label = character(), estimate = numeric(), ll = numeric(), ul = numeric(), pvalue = numeric(),
               model = integer(), model_label = character(), rowtype = character(), section = character(),
               source_row = integer(), source_model = integer(), source_frame = character(),
               stringsAsFactors = FALSE)
  rownames(d) <- NULL
  attr(d, "source") <- "comptab"
  attr(d, "ci_level") <- frame$ci_level
  attr(d, "n_models") <- n_models
  attr(d, "statistic_ids") <- "estimate ci pvalue"
  list(data = d, error = NULL)
}

# ---------------------------------------------------------------------------
# Rate mode (comptab.ado:258-1775)

.ct_rate_source <- function(x) {
  if (inherits(x, "tt_table")) {
    fm <- x$meta$frame
    body <- as.matrix(x$body)
    dimnames(body) <- NULL
    h <- x$header
    if (length(h) < 2L) cli::cli_abort("The rate table needs its two header rows.", call = NULL)
    return(list(labels = body[, 1], cells = body[, -1, drop = FALSE], h1 = h[[1]]$text,
                h2 = h[[2]]$text, frame = fm, title = fm$title %||% x$title %||% "",
                sample_accounting = x$meta[["sample_accounting", exact = TRUE]]))
  }
  if (is.data.frame(x)) {
    m <- as.matrix(x)
    m[is.na(m)] <- ""
    storage.mode(m) <- "character"
    dimnames(m) <- NULL
    fm <- attributes(x)
    fm <- fm[setdiff(names(fm), c("names", "row.names", "class"))]
    body <- m[-seq_len(min(2L, nrow(m))), , drop = FALSE]
    return(list(labels = body[, 1], cells = body[, -1, drop = FALSE], h1 = m[1, ],
                h2 = if (nrow(m) >= 2L) m[2, ] else rep("", ncol(m)), frame = fm, title = fm$title %||% "",
                sample_accounting = attr(x, "sample_accounting", exact = TRUE)))
  }
  cli::cli_abort("{.arg ratetable} must be a {.fn stratetab} table, not {.obj_type_friendly {x}}.", call = NULL)
}

.ct_effect_norm <- function(x) gsub("[ ._/-]", "", .ct_lower(.ct_trim(x)))
.ct_hr_scales <- c("hr", "ahr", "hazardratio", "adjustedhazardratio")

.ct_rates <- function(ratetable, models, a, rate_label, model_names) {
  .ct_check_common(a)
  effect <- a$effect %||% "aHR"
  reflabel <- a$reflabel %||% "Reference"
  for (f in c("effect", "reflabel")) {
    v <- get(f)
    if (!is.character(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {f}} must be a single string.", call = NULL)
  }
  if (!nzchar(effect)) effect <- "aHR"
  if (!nzchar(reflabel)) reflabel <- "Reference"
  sheet <- .check_sheet(a$sheet)
  .tt_preflight_targets(xlsx = a$xlsx, csv = a$csv, markdown = a$markdown, mdappend = a$mdappend)
  style <- tt_resolve_style(font = a$font, fontsize = a$fontsize, borderstyle = a$borderstyle,
                            headershade = a$headershade, zebra = a$zebra, headercolor = a$headercolor,
                            zebracolor = a$zebracolor)
  style$boldp <- NA_real_

  # The rate table (comptab.ado:396-446). A rate-ratio table is refused
  # by its provenance before the width test (Phase 7b F6): its statistic
  # ids name the IRR column, as Stata tabtools 2.1.14 reads them
  # (comptab.ado:419-426), or its recorded layout says so.
  rs <- .ct_rate_source(ratetable)
  rc <- ncol(rs$cells) + 1L
  cpo <- rs$frame$cols_per_outcome
  sid <- rs$frame$statistic_ids
  irr_ids <- is.character(sid) && length(sid) >= 1L && !is.na(sid[1]) &&
    "irr_ci" %in% strsplit(trimws(sid[1]), " +")[[1]]
  if (irr_ids || isTRUE(rs$frame$rateratio) || (is.numeric(cpo) && length(cpo) == 1L && !is.na(cpo) && cpo == 4)) {
    cli::cli_abort(c("The rate table must come from {.fn stratetab} without {.arg rateratio}.",
                     "i" = "Expected 1 label column plus 3 columns per outcome."), call = NULL)
  }
  if (rc < 4L) cli::cli_abort("The rate table is too narrow to be {.fn stratetab} output.", call = NULL)
  if ((rc - 1L) %% 3L != 0L) {
    cli::cli_abort(c("The rate table must come from {.fn stratetab} without {.arg rateratio}.",
                     "i" = "Expected 1 label column plus 3 columns per outcome."), call = NULL)
  }
  outcomes <- (rc - 1L) %/% 3L
  nb <- length(rs$labels)
  if (nb + 3L < 5L) cli::cli_abort("The rate table has too few rows.", call = NULL)

  # Sections and categories (comptab.ado:439-491).
  secs <- list()
  for (i in seq_len(nb)) {
    lb <- rs$labels[i]
    if (!nzchar(.ct_trim(lb))) next
    if (!startsWith(lb, "   ") && rs$cells[i, 1] == "") {
      secs[[length(secs) + 1L]] <- list(row = i, cats = integer())
      next
    }
    if (startsWith(lb, "   ")) {
      if (!length(secs)) cli::cli_abort("The rate table has a category row before any exposure heading.", call = NULL)
      secs[[length(secs)]]$cats <- c(secs[[length(secs)]]$cats, i)
    }
  }
  n_sections <- length(secs)
  if (!n_sections) cli::cli_abort("The rate table has no exposure heading rows.", call = NULL)
  n_nonref <- sum(vapply(secs, function(s) max(length(s$cats) - 1L, 0L), 0L))
  if (!n_nonref) cli::cli_abort("The rate table has no non-reference rows to fill.", call = NULL)

  # Model tables (comptab.ado:493-752).
  n_frames <- length(models)
  srcs <- lapply(seq_len(n_frames), function(f) .ct_source(models[[f]], f))
  lay <- .ct_layout(srcs[[1]], 1L)
  n_models <- lay$n_models
  cpm <- lay$cpm
  if (n_models != outcomes) {
    cli::cli_abort(c("The model columns ({n_models} model{?s}) must match the rate table's outcomes ({outcomes}).",
                     "i" = "Each model table holds one model per outcome, e.g. {.code regtab(list(fit_outcome1, fit_outcome2))}."),
                   call = NULL)
  }
  for (f in seq_len(n_frames)[-1]) {
    lf <- .ct_layout(srcs[[f]], f)
    if (lf$mode != lay$mode || lf$n_models != n_models) {
      cli::cli_abort(c("All model tables must share the same layout.",
                       "x" = "Model table 1 is {lay$mode} with {n_models} model{?s}; model table {f} is {lf$mode} with {lf$n_models}."),
                     call = NULL)
    }
  }

  # Rate provenance and outcome identities (comptab.ado:754-790).
  fm <- rs$frame
  if (!identical(.ct_key(fm$source %||% ""), "stratetab") ||
      !isTRUE(suppressWarnings(as.numeric(fm$n_outcomes)) == outcomes) ||
      !identical(fm$statistic_ids, "events person_years rate_ci")) {
    cli::cli_abort("The rate table lacks the outcome and statistic provenance of a {.fn stratetab} table.", call = NULL)
  }
  ci_level <- suppressWarnings(as.numeric(fm$ci_level[1]))
  if (!length(ci_level) || is.na(ci_level) || ci_level <= 0 || ci_level >= 100) {
    cli::cli_abort("The rate table has no known confidence level.", call = NULL)
  }
  ci_txt <- .tt_level_text(ci_level)
  rate_ids <- vapply(seq_len(outcomes), function(o) {
    v <- fm[[paste0("outcome_id_", o)]] %||% (if (length(fm$outcome_id) >= o) fm$outcome_id[o]) %||% ""
    .ct_key(v)
  }, "")
  if (any(!nzchar(rate_ids))) cli::cli_abort("The rate table has a blank outcome identity.", call = NULL)
  if (anyDuplicated(rate_ids)) cli::cli_abort("The rate table has duplicate outcome identities.", call = NULL)
  rate_display <- rs$h1[2L + (seq_len(outcomes) - 1L) * 3L]

  # outcomemap() or the outcome identities (comptab.ado:792-817).
  explicit <- !is.null(a$outcomemap)
  if (explicit) {
    om <- a$outcomemap
    if (is.character(om) && length(om) == 1L && grepl("\\", om, fixed = TRUE)) om <- .ct_split_bs(om)
    if (!is.character(om) || anyNA(om)) cli::cli_abort("{.arg outcomemap} must be a character vector.", call = NULL)
    keys <- .ct_key(om)
    if (length(keys) != outcomes || any(!nzchar(keys))) {
      cli::cli_abort("{.arg outcomemap} requires {outcomes} identit{?y/ies}, one per rate outcome.", call = NULL)
    }
  } else {
    keys <- rate_ids
  }
  if (!.ct_effect_norm(effect) %in% .ct_hr_scales) {
    cli::cli_abort(c("{.arg effect} must describe a hazard-ratio scale.",
                     "i" = "Use {.val aHR}, {.val HR}, {.val hazard ratio} or {.val adjusted hazard ratio}."), call = NULL)
  }

  # Each model table: provenance, level, scale, and the model per outcome
  # (comptab.ado:828-895).
  expected_stats <- if (lay$mode == "standard") "estimate ci pvalue" else "estimate_ci pvalue"
  model_map <- matrix(0L, n_frames, outcomes)
  out_model_id <- character(outcomes)
  for (f in seq_len(n_frames)) {
    mf <- srcs[[f]]$frame
    if (!isTRUE(suppressWarnings(as.numeric(mf$n_models)) == n_models) ||
        !identical(mf$statistic_ids[1], expected_stats)) {
      cli::cli_abort("Model table {f} lacks the model and statistic provenance of a {.fn regtab} table.", call = NULL)
    }
    lev <- suppressWarnings(as.numeric(mf$ci_level[1]))
    if (!length(lev) || is.na(lev) || abs(lev - ci_level) > 1e-8) {
      cli::cli_abort(c("The rate table and model table {f} are at different (or unknown) confidence levels.",
                       "i" = "Rates at {ci_txt}%; fit the models' intervals at the same level ({.arg level} of {.fn regtab})."),
                     call = NULL)
    }
    mid <- vapply(seq_len(n_models), function(m) .ct_key(.ct_meta(mf, "model_id", m)), "")
    oid <- vapply(seq_len(n_models), function(m) .ct_key(.ct_meta(mf, "outcome_id", m)), "")
    mlab <- vapply(seq_len(n_models), function(m) .ct_key(.ct_meta(mf, "model_label", m)), "")
    for (m in seq_len(n_models)) {
      if (.ct_meta(mf, "effect_log_scale", m) %in% c("log", "unknown")) {
        cli::cli_abort(c("Model {m} of model table {f} holds a log-ratio comparison whose exponentiation cannot be established from the result's recorded call.",
                         "i" = "Use {.code comparison = \"lnratioavg\", transform = exp} (write {.code exp} explicitly), or supply a data frame of exponentiated estimates and intervals."),
                       class = "tabtools_error_not_hazard_ratio", call = NULL)
      }
      # An effecttab() difference or prediction is not a hazard ratio,
      # whatever its `effect` header (Muse audit P0-5).
      if (identical(.ct_meta(mf, "effect_additive", m), "TRUE")) {
        cli::cli_abort(c("Model {m} of model table {f} holds differences or predictions (an additive scale), not hazard ratios, although it is headed {.val {(.ct_meta(mf, 'effect_scale', m))}}.",
                         "i" = "Rate composites take hazard ratios: {.fn regtab} of Cox models, or an {.fn effecttab} of ratios."),
                       class = "tabtools_error_not_hazard_ratio", call = NULL)
      }
      if (!.ct_effect_norm(.ct_meta(mf, "effect_scale", m)) %in% .ct_hr_scales) {
        cli::cli_abort(c("Model {m} of model table {f} is not on the hazard-ratio scale ({.val {(.ct_meta(mf, 'effect_scale', m))}}).",
                         "i" = "Rate composites take hazard ratios: {.fn regtab} of Cox models, with {.code coef = \"HR\"} or {.val aHR}."),
                       call = NULL)
      }
    }
    used <- integer()
    for (o in seq_len(outcomes)) {
      k <- keys[o]
      hit <- if (explicit) which((nzchar(mid) & mid == k) | (nzchar(oid) & oid == k) | (nzchar(mlab) & mlab == k)) else
        which(nzchar(oid) & oid == k)
      if (length(hit) != 1L) {
        if (explicit) cli::cli_abort("{.arg outcomemap} identity {.val {k}} matched {length(hit)} model block{?s} in model table {f}.", call = NULL)
        cli::cli_abort(c("Rate outcome {.val {k}} could not be matched to exactly one model of model table {f} ({length(hit)} match{?es}).",
                         "i" = "The models' outcome identities are {.val {oid}}; give {.arg outcomemap} (model labels will do)."),
                       call = NULL)
      }
      if (hit %in% used) cli::cli_abort("{.arg outcomemap} maps more than one rate outcome to the same model.", call = NULL)
      used <- c(used, hit)
      model_map[f, o] <- hit
      if (f == 1L) out_model_id[o] <- mid[hit]
    }
  }

  # Row selections (comptab.ado:897-1024).
  use_names <- !is.null(a$rownames)
  sel <- .ct_selection_list(if (use_names) a$rownames else a$rows, n_frames, if (use_names) "rownames" else "rows")
  expanded <- .ct_expand(sel, use_names, srcs, dup_error = TRUE, exact = isTRUE(a$rownames_exact))
  picks <- do.call(rbind, lapply(seq_len(n_frames), function(f) {
    if (!length(expanded[[f]])) return(NULL)
    data.frame(f = f, r = expanded[[f]])
  }))
  n_sel <- if (is.null(picks)) 0L else nrow(picks)
  if (n_sel != n_nonref) {
    if (use_names) .ct_abort_rownames_count(sel, srcs, n_sel, n_nonref, isTRUE(a$rownames_exact))
    cli::cli_abort(c("The selected model rows ({n_sel}) must match the non-reference rows of the rate table ({n_nonref}).",
                     "i" = "Select one model row for each exposure category but the reference."), call = NULL)
  }

  # Place every selected row on its category by label (comptab.ado:1026-1178).
  rowmap <- integer(nb)
  ref_rows <- integer()
  pos <- 0L
  est_col <- function(f, o) (model_map[f, o] - 1L) * cpm + 1L
  for (s in secs) {
    cats <- s$cats
    k <- length(cats)
    if (!k) next
    sec_label <- .ct_trim(rs$labels[s$row])
    level_pos <- integer()
    for (j in seq_len(k - 1L)) {
      pos <- pos + 1L
      f <- picks$f[pos]
      r <- picks$r[pos]
      src <- srcs[[f]]
      mlab_raw <- src$labels[r]
      mlab <- .ct_trim(mlab_raw)
      is_level <- startsWith(mlab_raw, "  ")
      if (all(.ct_trim(src$cells[r, ]) == "")) {
        cli::cli_abort(c("Model row {r} ({.val {mlab}}) of model table {f} is a heading row with no estimate.",
                         "i" = "Select only effect rows; rows are counted below the two header rows."), call = NULL)
      }
      if (any(vapply(seq_len(outcomes), function(o) .ct_is_ref(src, r, est_col(f, o)), TRUE))) {
        cli::cli_abort(c("Model row {r} ({.val {mlab}}) of model table {f} is the model's reference (or an omitted) category.",
                         "i" = "Select only the non-reference rows; the reference category shows {.arg reflabel}."), call = NULL)
      }
      # Matched as Stata matches, ignoring case, unless that is ambiguous:
      # categories "A" and "a" take the model row of the same case, and a
      # row matching two categories only by case is refused, never placed
      # on the last one (codex audit F02).
      hit <- cats[.ct_label_match(.ct_trim(rs$labels[cats]), mlab)]
      if (length(hit) > 1L) {
        cli::cli_abort(c("Model row {r} ({.val {mlab}}) of model table {f} matches more than one category of rate block {.val {sec_label}}: {.val {(.ct_trim(rs$labels[hit]))}}.",
                         "i" = "Categories are matched by label ignoring case; give the categories labels that differ by more than case."),
                       call = NULL)
      }
      target <- if (length(hit)) hit else 0L
      if (!target) {
        if (!is_level && k == 2L) {
          # Stata's rule: an indicator's effect is the second category's,
          # as strate orders 0 before 1. A block whose categories read as
          # 1 then 0 (Yes/No) would show the indicator's effect under the
          # "0" category (Muse audit P1-31).
          two <- tolower(.ct_trim(rs$labels[cats]))
          if (two[1] %in% c("yes", "1", "true") && two[2] %in% c("no", "0", "false")) {
            cli::cli_abort(c("Model row {r} ({.val {mlab}}) of model table {f} is an indicator, placed on the second category of rate block {.val {sec_label}}, but that block lists {.val {(.ct_trim(rs$labels[cats]))}}: the indicator's effect would be shown under {.val {(.ct_trim(rs$labels[cats[2]]))}}.",
                             "i" = "Order the rate categories 0 before 1 (No before Yes), or fit the model with a factor whose levels carry the rate labels."),
                           class = "tabtools_error_indicator_order", call = NULL)
          }
          target <- cats[2]
        } else {
          cli::cli_abort(c("Model row {r} ({.val {mlab}}) of model table {f} matches no category of rate block {.val {sec_label}}.",
                           "i" = "Model rows are placed by label: give the model's factor the category labels of the rates."),
                         call = NULL)
        }
      }
      if (rowmap[target] != 0L) {
        cli::cli_abort("Two selected model rows map to rate category {.val {(.ct_trim(rs$labels[target]))}} in block {.val {sec_label}}.",
                       call = NULL)
      }
      rowmap[target] <- pos
      if (is_level) level_pos <- c(level_pos, pos)
    }
    ref_row <- cats[rowmap[cats] == 0L][1]
    ref_rows <- c(ref_rows, ref_row)
    ref_lab <- .ct_trim(rs$labels[ref_row])
    # The category left over must be the base level of every factor block
    # that supplied an estimate (comptab.ado:1124-1175).
    for (p in level_pos) {
      f <- picks$f[p]
      r <- picks$r[p]
      src <- srcs[[f]]
      lb <- src$labels
      b0 <- r
      while (b0 > 1L && startsWith(lb[b0 - 1L], "  ")) b0 <- b0 - 1L
      b1 <- r
      while (b1 < length(lb) && startsWith(lb[b1 + 1L], "  ")) b1 <- b1 + 1L
      found <- FALSE
      blk <- seq.int(b0, b1)
      for (br in blk[.ct_label_match(.ct_trim(lb[blk]), ref_lab)]) {
        if (all(vapply(seq_len(outcomes), function(o) .ct_is_ref(src, br, est_col(f, o)), TRUE))) found <- TRUE
      }
      if (!found) {
        cli::cli_abort(c("Rate category {.val {(.ct_trim(rs$labels[ref_row]))}} in block {.val {sec_label}} would be shown as the reference, but it is not the reference category of the model in model table {f}.",
                         "i" = "Select every non-reference level of the model, or refit it with the intended base level."),
                       call = NULL)
      }
    }
  }

  # The table (comptab.ado:1302-1436).
  title <- a$title %||% ""
  if (!nzchar(title)) title <- rs$title %||% ""
  nc <- 1L + 5L * outcomes
  h1 <- rep("", nc)
  h2 <- rep("", nc)
  h1[1] <- rs$h1[1]
  body <- matrix("", nb, nc)
  body[, 1] <- rs$labels
  sec_rows <- vapply(secs, function(s) s$row, 0L)
  for (o in seq_len(outcomes)) {
    rsc <- 1L + (o - 1L) * 3L
    j <- 2L + (o - 1L) * 5L
    h1[j] <- rs$h1[rsc + 1L]
    h2[j:(j + 2L)] <- rs$h2[(rsc + 1L):(rsc + 3L)]
    h2[j + 3L] <- paste0(effect, " (", ci_txt, "% CI)")
    h2[j + 4L] <- "p-value"
    body[, j:(j + 2L)] <- rs$cells[, rsc:(rsc + 2L)]
  }
  for (i in seq_len(nb)) {
    if (i %in% sec_rows) next
    for (o in seq_len(outcomes)) {
      j <- 2L + (o - 1L) * 5L + 3L
      if (i %in% ref_rows) {
        body[i, j] <- reflabel
        body[i, j + 1L] <- ""
        next
      }
      p <- rowmap[i]
      if (!p) next
      f <- picks$f[p]
      r <- picks$r[p]
      c1 <- est_col(f, o)
      cl <- srcs[[f]]$cells
      if (lay$mode == "standard") {
        e <- .ct_trim(cl[r, c1])
        ci <- .ct_trim(cl[r, c1 + 1L])
        txt <- if (!nzchar(e)) ci else if (!nzchar(ci)) e else paste(e, ci)
        pv <- cl[r, c1 + 2L]
      } else {
        txt <- .ct_trim(cl[r, c1])
        pv <- cl[r, c1 + 1L]
      }
      body[i, j] <- txt
      body[i, j + 1L] <- .ct_trim(pv)
    }
  }
  spans <- data.frame(from = 2L + (seq_len(outcomes) - 1L) * 5L, to = 1L + seq_len(outcomes) * 5L)
  role <- c("label", rep(c("value", "value", "est_ci", "est_ci", "pval"), outcomes))
  cols <- data.frame(role = role, model = c(NA_integer_, rep(seq_len(outcomes), each = 5L)),
                     console_width = NA_integer_, stringsAsFactors = FALSE)
  type <- ifelse(seq_len(nb) %in% sec_rows, "var", ifelse(seq_len(nb) %in% ref_rows, "ref", "level"))
  rows <- data.frame(type = type, indent = ifelse(type == "var", 0L, 3L),
                     block = cumsum(seq_len(nb) %in% sec_rows | seq_len(nb) == 1L), stringsAsFactors = FALSE)

  frame <- list(source = "hrcomptab", ci_level = ci_level, n_outcomes = outcomes,
                statistic_ids = "events person_years rate_ci estimate_ci pvalue",
                model_id = out_model_id, outcome_id = rate_ids, effect_scale = rep("HR", outcomes),
                outcome_label = rate_display)
  stored <- list(N_rows = nb + 3L, N_outcomes = outcomes, N_sections = n_sections, N_modelrows = n_sel,
                 N_modelframes = n_frames, ci_level = ci_level, rateframe = rate_label,
                 modelframes = paste(model_names, collapse = " "), effect = effect)
  forest <- .ct_forest_rates(rs, srcs, secs, sec_rows, ref_rows, rowmap, picks, model_map, outcomes,
                             rate_display, frame, rate_label, model_names)
  layout <- list(indent = 3L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "plain", xlsx_rules = "hrcomptab",
                 sheet = "Composite", md_header = "first", console_footnote = TRUE)
  tt <- tt_table(body = body, header = list(list(text = h1, spans = spans), list(text = h2)), rows = rows,
                 cols = cols, title = title, footnote = a$footnote %||% "", style = style, stored = stored,
                 command = "hrcomptab", layout = layout,
                 meta = list(sheet = sheet, frame = frame, outcomes = outcomes, section_rows = sec_rows,
                             ref_rows = ref_rows, forest = forest$data, forest_error = forest$error,
                             sample_accounting = .tt_sample_bind(
                               c(list(rs$sample_accounting), lapply(srcs, `[[`, "sample_accounting")),
                               prefixes = c("ratetable", paste0("modeltable", seq_along(srcs))),
                               commands = c("stratetab", vapply(srcs, function(s) s$frame$source %||% "unknown", "")))))
  .ct_write(tt, a)
}

# hrcomptab's eplotframe() analogue (comptab.ado:1180-1300): the rate
# table's rows, section headings (a heading over a single row folded into
# it), reference rows, and each placed model row once per outcome, from
# the model table's as_forest_data() rows.
.ct_forest_rates <- function(rs, srcs, secs, sec_rows, ref_rows, rowmap, picks, model_map, outcomes,
                             rate_display, frame, rate_label, model_names) {
  if (any(vapply(srcs, function(s) is.null(s$rows), TRUE))) {
    return(list(data = NULL, error = "Every model table must be a {.fn regtab} table (not a data frame) for {.fn as_forest_data}."))
  }
  nb <- length(rs$labels)
  fold <- vapply(sec_rows, function(sr) {
    nxt <- sec_rows[sec_rows > sr]
    end <- if (length(nxt)) min(nxt) - 1L else nb
    end - sr == 1L
  }, TRUE)
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)
  cur <- ""
  pending <- ""
  for (i in seq_len(nb)) {
    lab <- rs$labels[i]
    if (i %in% sec_rows) {
      cur <- .ct_trim(lab)
      if (fold[match(i, sec_rows)]) {
        pending <- cur
      } else {
        pending <- ""
        add(label = cur, estimate = NA_real_, ll = NA_real_, ul = NA_real_, pvalue = NA_real_, model = NA_integer_,
            model_label = "", rowtype = "section", section = cur, source_row = NA_integer_,
            source_model = NA_integer_, source_frame = rate_label)
      }
      next
    }
    if (i %in% ref_rows) {
      pl <- if (nzchar(pending)) pending else lab
      pending <- ""
      add(label = pl, estimate = NA_real_, ll = NA_real_, ul = NA_real_, pvalue = NA_real_, model = NA_integer_,
          model_label = "", rowtype = "reference", section = cur, source_row = NA_integer_,
            source_model = NA_integer_, source_frame = rate_label)
      next
    }
    p <- rowmap[i]
    if (!p) next
    f <- picks$f[p]
    r <- picks$r[p]
    rr <- srcs[[f]]$rows
    for (o in seq_len(outcomes)) {
      hit <- rr[rr$row == r & rr$model == model_map[f, o] & rr$status %in% c("est", "base"), , drop = FALSE]
      if (nrow(hit) != 1L) {
        return(list(data = NULL, error = paste0("Model table ", f, " holds no estimate for row ", r, " of outcome ", o,
                                                 ", so the forest data cannot be built.")))
      }
      est <- hit$status == "est"
      pl <- if (nzchar(pending)) pending else hit$label
      pending <- ""
      add(label = pl, estimate = if (est) hit$estimate else NA_real_, ll = if (est) hit$conf.low else NA_real_,
          ul = if (est) hit$conf.high else NA_real_, pvalue = if (est) hit$p.value else NA_real_, model = o,
          model_label = rate_display[o], rowtype = if (est) "effect" else "reference", section = cur,
          source_row = r, source_model = model_map[f, o], source_frame = model_names[f])
    }
  }
  d <- do.call(rbind, out)
  rownames(d) <- NULL
  d$model <- as.integer(d$model)
  d$source_row <- as.integer(d$source_row)
  d$source_model <- as.integer(d$source_model)
  attr(d, "source") <- "hrcomptab"
  attr(d, "ci_level") <- frame$ci_level
  attr(d, "n_models") <- outcomes
  attr(d, "statistic_ids") <- "estimate ci pvalue"
  attr(d, "model_id") <- frame$model_id
  attr(d, "outcome_id") <- frame$outcome_id
  attr(d, "effect_scale") <- frame$effect_scale
  list(data = d, error = NULL)
}

# ---------------------------------------------------------------------------
# Workbook layouts

# p-values read from the displayed text for boldp/highlight
# (comptab.ado:2865-2884): "<..." is 0, anything else real() (">0.99" is
# missing).
.ct_p_text <- function(s) {
  s <- .ct_trim(s)
  out <- suppressWarnings(as.numeric(s))
  out[startsWith(s, "<")] <- 0
  out
}

# Vertical mode (comptab.ado:2719-2753, :2850-2983).
.xlsx_layout_comptab <- function(x) {
  if (length(x$header) < 2L) .tt_layout_needs("comptab", "at least two header rows")
  b <- as.matrix(x$body)
  style <- x$style
  meta <- x$meta
  nb <- nrow(b)
  nc <- ncol(b)
  num_cols <- nc + 1L
  num_rows <- nb + 3L
  foot <- nzchar(x$footnote)
  nr <- num_rows + as.integer(foot)
  grid <- matrix("", nr, num_cols)
  grid[1, 1] <- x$title
  grid[2, -1] <- x$header[[1]]$text
  grid[3, -1] <- x$header[[2]]$text
  if (nb) grid[4:num_rows, -1] <- b
  written <- matrix(FALSE, nr, num_cols)
  written[seq_len(num_rows), ] <- TRUE
  if (foot) {
    grid[nr, 2] <- x$footnote
    written[nr, 2] <- TRUE
  }
  cpm <- meta$cpm %||% (if (any(x$cols$role == "est_ci")) 2L else 3L)
  n_models <- (nc - 1L) %/% cpm
  compact <- cpm == 2L
  first_col <- function(m) 3L + (m - 1L) * cpm

  est_min <- if (compact) 16 else 8
  est_max <- if (compact) 34 else 22
  widths <- list()
  headerht <- 0L
  col <- function(j) grid[seq_len(num_rows), j]
  for (m in seq_len(n_models)) {
    c1 <- first_col(m)
    ew <- tt_colwidth(col(c1), minwidth = est_min, maxwidth = est_max, headerrow = 2L)
    w <- c(est = ew$width, ci = 0)
    if (!compact) w["ci"] <- tt_colwidth(col(c1 + 1L), minwidth = 16, maxwidth = 34)$width
    w["p"] <- tt_colwidth(col(c1 + cpm - 1L), minwidth = 8, maxwidth = 12)$width
    widths[[m]] <- w
    hl <- tt_colwidth(hlength = ew$hlen, blockwidth = sum(w))$hlines
    if (hl > 1L && hl > headerht) headerht <- hl
  }
  lw <- meta$labelwidth %||% 45
  factor_length <- min(ceiling(max(.blen(grid[seq_len(num_rows), 2])) * 0.95) + 2, lw)

  hb <- .border_code(style$hborder)
  vb <- .border_code(style$borderstyle)
  academic <- style$borderstyle == "academic"
  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  add("height", 1, 1, 1, 1, value = 30)
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = factor_length)
  for (m in seq_len(n_models)) {
    c1 <- first_col(m)
    add("width", 1, 1, c1, c1, value = widths[[m]][["est"]])
    if (!compact) add("width", 1, 1, c1 + 1L, c1 + 1L, value = widths[[m]][["ci"]])
    add("width", 1, 1, c1 + cpm - 1L, c1 + cpm - 1L, value = widths[[m]][["p"]])
  }
  if (headerht > 0L) add("height", 2, 2, 1, 1, value = headerht * 15)
  if (num_rows >= 4L) {
    add("wrap", 4, num_rows, 2, 2, code = 1)
    add("valign", 4, num_rows, 2, 2, code = 3)
  }
  add("font", 1, num_rows, 1, num_cols, value = style$fontsize)
  add("font", 1, 1, 1, num_cols, value = style$fontsize + 2)
  add("merge", 1, 1, 1, num_cols)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("bold", 1, 1, 1, 1, code = 1)
  if (style$headershade) add("fill", 2, 3, 2, num_cols, color = style$headercolor)
  add("bold", 3, 3, 2, num_cols, code = 1)
  add("halign", 3, 3, 2, num_cols, code = 2)
  add("valign", 3, 3, 2, num_cols, code = 2)
  # Reference rows merge over the blocks whose own cell reads "Reference",
  # as Stata since tabtools 2.1.14 (comptab.ado:2750-2758, :2961-2967;
  # up to 2.1.13 it merged every block of the row).
  for (m in seq_len(n_models)) {
    c1 <- first_col(m)
    for (r in which(grid[seq_len(num_rows), c1] == "Reference" & seq_len(num_rows) >= 4L)) {
      add("merge", r, r, c1, c1 + cpm - 1L)
      add("halign", r, r, c1, c1, code = 2)
      add("valign", r, r, c1, c1, code = 2)
      add("italic", r, r, c1, c1, code = 1)
    }
  }
  for (m in seq_len(n_models)) {
    c1 <- first_col(m)
    c2 <- c1 + cpm - 1L
    add("merge", 2, 2, c1, c2)
    add("halign", 2, 2, c1, c1, code = 2)
    add("valign", 2, 2, c1, c1, code = 2)
    add("bold", 2, 2, c1, c1, code = 1)
    add("wrap", 2, 2, c1, c1, code = 1)
    if (!academic) add("right", 2, num_rows, c2, c2, code = vb)
  }
  add("top", 2, 2, 2, num_cols, code = hb)
  add("top", 3, 3, 3, num_cols, code = hb)
  add("bottom", 3, 3, 2, num_cols, code = hb)
  add("bottom", num_rows, num_rows, 2, num_cols, code = hb)
  if (!academic) {
    add("right", 2, num_rows, num_cols, num_cols, code = vb)
    add("left", 2, num_rows, 2, 2, code = vb)
    add("right", 2, num_rows, 2, 2, code = vb)
  }
  for (i in which(x$rows$section %||% rep(FALSE, nb))) {
    add("bold", i + 3, i + 3, 2, num_cols, code = 1)
    add("top", i + 3, i + 3, 2, num_cols, code = hb)
  }
  for (s in sort(unique(meta$separator))) {
    r <- s + 3
    if (r >= 4 && r <= num_rows) add("top", r, r, 2, num_cols, code = hb)
  }
  if (style$zebra && num_rows >= 5L) {
    for (r in seq.int(5L, num_rows, by = 2L)) add("fill", r, r, 2, num_cols, color = style$zebracolor)
  }
  if (num_rows >= 4L) add("halign", 4, num_rows, 3, num_cols, code = 2)
  if (nb && (!is.na(style$boldp) || !is.na(style$highlight))) {
    for (m in seq_len(n_models)) {
      pc <- m * cpm + 2L
      pv <- .ct_p_text(grid[4:num_rows, pc])
      for (i in seq_len(nb)) {
        if (is.na(pv[i])) next
        if (!is.na(style$boldp) && pv[i] < style$boldp) add("bold", i + 3, i + 3, pc, pc, code = 1)
        if (!is.na(style$highlight) && pv[i] < style$highlight) add("fill", i + 3, i + 3, 2, num_cols, color = style$highlightcolor)
      }
    }
  }
  if (foot) .xlsx_footnote_rules(add, nr, num_cols, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}

# Rate mode (comptab.ado:1518-1645).
.xlsx_layout_hrcomptab <- function(x) {
  if (length(x$header) != 2L) .tt_layout_needs("hrcomptab", "exactly two header rows")
  cells <- .tt_cells(x)
  nb <- nrow(x$body)
  nc <- ncol(x$body)
  total <- nc + 1L
  last <- nb + 3L
  foot <- nzchar(x$footnote)
  nr <- last + as.integer(foot)
  outcomes <- (nc - 1L) %/% 5L
  style <- x$style
  grid <- matrix("", nr, total)
  grid[1, 1] <- x$title
  grid[2:last, -1] <- cells
  written <- matrix(FALSE, nr, total)
  written[seq_len(last), ] <- TRUE
  if (foot) {
    grid[nr, 2] <- x$footnote
    written[nr, 2] <- TRUE
  }
  lens <- function(j, from) .blen(grid[seq.int(from, last), j])
  lw <- if (nb) ceiling(max(lens(2L, 4L)) * 0.90) else 14
  lw <- min(max(lw, 14), 30)
  cw <- numeric(total)
  for (c in seq.int(2L, length.out = nc - 1L)) {
    j <- c + 1L
    mx <- max(lens(j, 3L))
    pos <- (c - 2L) %% 5L
    cw[j] <- switch(pos + 1L,
                    min(max(ceiling(mx), 7), 10),
                    min(max(ceiling(mx * 0.90), 12), 18),
                    min(max(ceiling(mx * 0.88), 14), 22),
                    min(max(ceiling(mx * 0.88), 13), 20),
                    min(max(ceiling(mx), 7), 10))
  }
  hb <- .border_code(style$hborder)
  vb <- .border_code(style$borderstyle)
  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  add("height", 1, 1, 1, 1, value = 30)
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = lw)
  for (j in seq.int(3L, length.out = nc - 1L)) add("width", 1, 1, j, j, value = cw[j])
  add("font", 1, last, 1, total, value = style$fontsize)
  add("font", 1, 1, 1, total, value = style$fontsize + 2)
  add("merge", 1, 1, 1, total)
  add("bold", 1, 1, 1, 1, code = 1)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("top", 2, 2, 2, total, code = hb)
  add("bottom", 3, 3, 2, total, code = hb)
  col <- 3L
  for (o in seq_len(outcomes)) {
    end <- col + 4L
    add("merge", 2, 2, col, end)
    add("bold", 2, 2, col, col, code = 1)
    add("halign", 2, 2, col, col, code = 2)
    add("valign", 2, 2, col, col, code = 3)
    add("bottom", 2, 2, col, end, code = hb)
    col <- col + 5L
  }
  add("merge", 2, 3, 2, 2)
  add("bold", 2, 3, 2, 2, code = 1)
  add("halign", 2, 3, 2, 2, code = 2)
  add("valign", 2, 3, 2, 2, code = 2)
  add("bottom", 3, 3, 2, 2, code = hb)
  add("bold", 3, 3, 3, total, code = 1)
  add("halign", 3, 3, 3, total, code = 2)
  add("valign", 3, 3, 3, total, code = 2)
  if (style$headershade) add("fill", 2, 3, 2, total, color = style$headercolor)
  if (style$zebra && last >= 5L) {
    for (r in seq.int(5L, last, by = 2L)) add("fill", r, r, 2, total, color = style$zebracolor)
  }
  if (last >= 4L && total >= 3L) add("halign", 4, last, 3, total, code = 2)
  if (style$borderstyle != "academic") {
    add("left", 2, last, 2, 2, code = vb)
    add("right", 2, last, 2, 2, code = vb)
    col <- 3L
    for (o in seq_len(outcomes)) {
      end <- col + 4L
      add("right", 2, last, end, end, code = vb)
      col <- col + 5L
    }
  }
  for (sr in x$meta$section_rows %||% integer()) {
    br <- sr + 3L - 1L
    if (br > 3L) add("bottom", br, br, 2, total, code = hb)
  }
  add("bottom", last, last, 2, total, code = hb)
  if (foot) .xlsx_footnote_rules(add, nr, total, style)
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, R)))
}
