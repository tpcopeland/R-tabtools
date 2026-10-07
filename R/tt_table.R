# The shared in-memory table object. Every renderer reads a tt_table, so
# styling rules live in one place.

.tt_row_types <- c("n_header", "ess", "var", "cat_header", "level", "missing_summary",
                   "ref", "omitted", "empty", "re", "stat", "addrow")
.tt_col_roles <- c("label", "group", "total", "p", "test", "statistic", "smd",
                   "est", "ci", "pval", "est_ci", "value")

# Render settings per command. Renderers branch only on these fields, never
# on the command name, so later ports (crosstab, puttab, ...) set a layout
# instead of teaching every renderer a new command.
#   indent            label indent width used by the engine (informational)
#   align             console alignment of every column ("left"/"right")
#   console_sepby     rule after each variable block (Stata list sepby())
#   console_title     print the title above the listing
#   console_blank     blank line after the listing
#   header_style      "descriptor": row 1 group labels, row 2 descriptor + N
#                     (xlsx moves the descriptor to B2:B3; Markdown flattens to
#                     "Group (N=...)"); "model": row 1 model labels merged over
#                     each model block (Markdown "Model: stat"); "plain": the
#                     last header row is the Markdown header
#   xlsx_rules        rule set for the workbook: "descriptive" (desctab.ado),
#                     "regression" (regtab.ado), "puttab" (puttab.ado),
#                     "stacktab" (stacktab.ado), "stratetab" (stratetab.ado),
#                     "comptab" / "hrcomptab" (comptab.ado, vertical and
#                     rate mode), or "none" (no Excel layout yet;
#                     tt_write_xlsx() refuses)
#   sheet             default sheet name
#   csv_reservedrow   (optional, default TRUE) the CSV sink drops leading
#                     rows blank in every column, as Stata's collect-style
#                     callers ask with `reservedrow`; puttab does not
#                     (_tabtools_csv_write.ado), so its blank data rows stay
#   md_header         (optional) "first": the Markdown header is the first
#                     header row and the later header rows become body rows
#                     (comptab rate mode: _tabtools_markdown_write's default
#                     headerstart(2) datastart(3) over its two header rows);
#                     "joined": one header row, "group: statistic" per
#                     column of each cols$model group (stratetab 2.1.14)
#   width_rule        (optional, regression rule set) "effecttab": column
#                     widths by _tabtools_colwidth's defaults (estimate
#                     [8, 22], CI [16, 34], p [8, 12]) and a label column
#                     raised to fit the effect header (effecttab.ado:1335-
#                     1361, X1/X2); otherwise regtab's widths
#   console_skip_blank_header (optional) TRUE: header rows blank in every
#                     column are not listed (effecttab's blank model row,
#                     effecttab.ado:1369-1383)
#   console_footnote  (optional) TRUE: the footnote and a blank line follow
#                     the listing (comptab.ado:1442-1446, rate mode)
.tt_layouts <- list(
  table1_tc = list(indent = 3L, align = "left", console_sepby = TRUE, console_title = FALSE,
                   console_blank = FALSE, header_style = "descriptor",
                   xlsx_rules = "descriptive", sheet = "Table 1"),
  regtab = list(indent = 2L, align = "right", console_sepby = FALSE, console_title = TRUE,
                console_blank = TRUE, header_style = "model",
                xlsx_rules = "regression", sheet = "Regression"),
  effecttab = list(indent = 2L, align = "right", console_sepby = FALSE, console_title = TRUE,
                   console_blank = TRUE, header_style = "model", xlsx_rules = "regression",
                   sheet = "Effects", width_rule = "effecttab", console_skip_blank_header = TRUE),
  default = list(indent = 2L, align = "left", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "plain",
                 xlsx_rules = "none", sheet = "Sheet1")
)

#' Default layout for a command
#' @param command Command name.
#' @return A layout list (see `tt_table()`).
#' @keywords internal
#' @noRd
tt_layout_defaults <- function(command) {
  .tt_layouts[[command]] %||% .tt_layouts$default
}

#' Build a tt_table
#'
#' Engines call this with fully rendered cell text; the renderers
#' (`tt_write_xlsx()`, [tt_write_csv()], [tt_write_markdown()], `print()`)
#' only lay it out.
#'
#' `tt_table()` and `validate_tt_table()` are experimental: the object's
#' layout (`body`, `header`, `rows`, `cols`, `layout`, `meta`, `notes` and
#' their role vocabularies) may gain fields in a later version. Tables from
#' [table1_tc()] and [regtab()] are the stable interface.
#'
#' @param body data.frame (or character matrix) of body cells exactly as
#'   displayed; the first column is the row label, with literal leading-space
#'   indents (3 spaces in table1_tc, 2 in regtab).
#' @param header List of header rows. Each is a list with `text` (one string
#'   per column) and optionally `spans`, a data.frame of merged column ranges
#'   (`from`, `to`). A bare character vector is accepted for `text`. A table
#'   with the `"plain"` header style may have no header row (`list()`, as
#'   [puttab()]'s `noheader`).
#'   table1_tc: `list(c(" ", groups..., "p-value"), c(descriptor, "N=..."...,
#'   ""))`; regtab: `list(model labels in each block's first column,
#'   statistic headers)`.
#' @param rows Optional per-row metadata (data.frame). Missing columns are
#'   filled with defaults; see Details.
#' @param cols Optional per-column metadata (data.frame).
#' @param title A single string; `NULL` or `""` for none.
#' @param footnote Paragraph text; `NULL`, `character()`, or `""` for none.
#'   Footnotes accept a character vector of paragraphs. The reserved token
#'   `" \\ "` (one backslash with surrounding spaces) splits each element,
#'   including a scalar, into paragraphs. Split pieces are trimmed and empty
#'   pieces dropped; unspaced and doubled backslashes remain literal.
#'   Automatic notes are separate paragraphs in every sink.
#' @param style A style list (font, font size, border style, fills); `NULL`
#'   (the default) takes the session defaults set by [tabtools_options()].
#' @param stored Named list, the `r()` analogue.
#' @param command Name of the command that built the table (`"table1_tc"`,
#'   `"regtab"`, or any other string). It selects the default `layout`.
#' @param layout Optional list overriding render settings: `indent`, `align`
#'   (`"left"`/`"right"`), `console_sepby`, `console_title`, `console_blank`,
#'   `header_style` (`"descriptor"`, `"model"`, `"plain"`), `xlsx_rules`
#'   (`"descriptive"`, `"regression"`, `"puttab"`, `"stacktab"`,
#'   `"stratetab"`, `"comptab"`, `"hrcomptab"`), `sheet`, `csv_reservedrow` (default `TRUE`: the CSV
#'   sink drops leading rows blank in every column), and `md_header`
#'   (`"first"`: the Markdown header is the first header row and the later
#'   header rows are written as body rows, as comptab's rate-mode Markdown
#'   sink does; `"joined"`: one header row, each column of a `cols$model`
#'   group headed `"group: statistic"`, as stratetab's). These fields select
#'   the rendering layout; `command` also selects conventions such as keeping
#'   blank data rows for [puttab()] in Markdown.
#' @param meta Command-specific render settings (regtab: `refcat`,
#'   `omitlabel`, `emptylabel`, `labelwidth`, `compact`, `xlsx_footnote`).
#' @param notes Lines printed after the console listing (e.g. the small-cell
#'   note).
#'
#' @details `rows` columns: `type` (one of `n_header`, `ess`, `var`,
#'   `cat_header`, `level`, `missing_summary`, `ref`, `omitted`, `empty`,
#'   `re`, `stat`, `addrow`), `key`, `var`, `level` (row identity: regtab's
#'   Stata-style key such as `6.cyl`, its variable `cyl` and level `6`;
#'   `stat:n` for a statistics row, `statfun:<label>` for a `stat_fun`
#'   row, `addrow:<label>` for an `addrow` row; table1_tc's variable label,
#'   so its keys are labels and [tt_merge()] refuses its tables; `NA` for
#'   the other commands), `indent`, `block` (console
#'   separator groups), `p`, `smd` (numeric, for `boldp`/`highlight`/SMD
#'   flags), `dim` (regtab `dimnonsig`), `suppression`. `cols` columns:
#'   `role` (`label`, `group`, `total`, `p`, `test`, `statistic`, `smd`,
#'   `est`, `ci`, `pval`, `est_ci`, `value`), `model`, `pass`, `group`, and
#'   `console_width` (minimum listing width: Stata lists the table before
#'   some final edits, e.g. withheld percentages, so a column can print wider
#'   than its final text). `meta$console_before` holds lines printed above
#'   the table1_tc listing (the first-two-groups SMD note).
#'   `meta$extraspace = TRUE` (table1_tc `extraspace`) prefixes one space to
#'   every p-value cell not starting with "<" in the CSV and Markdown sinks
#'   (header rows included) and in the workbook body (header excluded), but
#'   not in the console listing, which Stata prints first.
#'
#'   `validate_tt_table()` establishes what every renderer relies on:
#'   character body and header cells without `NA`; header `spans` with
#'   whole-number, non-missing `from`/`to` inside the table; `rows$type` and
#'   `cols$role` present and from the documented values; a documented
#'   `layout` whose `console_sepby`, `console_title` and `console_blank` are
#'   each `TRUE` or `FALSE`; numeric, non-missing `rows$block` (required
#'   when `console_sepby`); logical, non-missing `rows$dim`;
#'   whole-number, non-negative, non-missing `rows$indent`; list `meta`; numeric
#'   `rows$p` and `rows$smd`; character `rows$key`, `rows$var`, `rows$level`; `title` a single
#'   non-missing string and `footnote` non-missing character paragraphs,
#'   or `NULL` for none. The constructor stores footnotes as a scalar with
#'   spaced-backslash separators. An object that passes renders with `print()`,
#'   [tt_write_csv()] and [tt_write_markdown()]; [tt_write_xlsx()] also needs
#'   an Excel layout (`layout$xlsx_rules`). Descriptive, regression,
#'   stratetab and comptab layouts need a body row and a value column;
#'   puttab can export a label-only or header-only table.
#' @section Sample accounting:
#'   Analytical tables store a source ledger in `meta$sample_accounting`.
#'   It supplements the displayed statistics with record counts and their
#'   evidence. Legacy and hand-built tables may omit it. `as.data.frame()`
#'   carries the ledger in a `sample_accounting` attribute.
#'
#'   The ledger is a list with `version = 1L` and three data frames:
#'   * `populations`: `id`, `component`, `command`, `scope`, `variable`,
#'     `group`, `model`, `imputation`, `spec`, `weight_type`. IDs are unique.
#'     Scope is `table`, `group`, `variable`, `model`, `imputation`,
#'     `evaluation` or `summary`. Model, imputation and specification indices
#'     are positive whole numbers or `NA`; other columns are character.
#'     Variable and group may be `NA`. Component paths distinguish sources;
#'     model indices remain local to their component.
#'   * `measures`: `population_id`, `metric`, `value`, `status`, `basis`,
#'     `reason`. Every population has each metric below exactly once.
#'     Status is `available`, `unavailable` or `not_applicable`. Available
#'     values are finite and nonnegative. Unknown or inapplicable values
#'     are `NA` with a reason; basis identifies the input or stored evidence.
#'   * `exclusions`: `population_id`, `stage`, `reason`, `n`, `status`,
#'     `basis`. Optional reason counts use stages `input_to_eligible`,
#'     `eligible_to_used` or `retained`. Retained reasons describe missing
#'     categories or events intentionally included by the command. Counts
#'     are whole records or explicitly unavailable; overlapping reasons
#'     cannot establish a unique excluded population.
#'
#'   Record metrics are `input_n` (before this population's filtering),
#'   `eligible_n` (shared eligibility), `observed_n` (nonmissing variable or
#'   outcome), `used_n` (contributors under the command's policy),
#'   `fitted_n` (fitted-model contributors, excluding known zero weights),
#'   `frame_n` (stored representation rows), `missing_n`, `zero_weight_n`,
#'   and `excluded_n` (input minus used when both describe the same scope).
#'   Frame rows can include zero weights or expanded Cox risk sets.
#'   A variable population can start with an already eligible group;
#'   earlier exclusions then belong to its parent. Retained missing values
#'   can make used exceed observed, and zero-weight counts can describe
#'   the original input rather than the eligible sample.
#'
#'   `weight_sum` uses original-scale contributor weights; `effective_n`
#'   is a Kish record-weight diagnostic, not survey-design ESS or model
#'   degrees of freedom. `reported_n` preserves the displayed or native N,
#'   whose basis can be frequency totals, events, trials or records.
#'   These three metrics may be fractional. Unrepresentable weight totals
#'   and unrecoverable counts remain unavailable.
#'
#'   Fitted-model metadata uses stored evidence without evaluating fitting
#'   calls. Original input counts are often unavailable. Effect-evaluation
#'   grids are separate from fitted samples. Composition preserves each
#'   source population, including reused sources and display-row selections;
#'   it does not sum groups, models or imputations into a unique cohort N.
#'   Supplied summaries and workbook-only sources cannot establish record
#'   counts. Generated ledgers contain no observation IDs, observation row
#'   names or raw data.
#'   Malformed supplied ledgers raise `tabtools_error_sample_accounting`.
#' @return `tt_table()`: an object of class `tt_table`.
#'   `validate_tt_table()`: `x` unchanged when it is valid; otherwise an
#'   error naming the first problem.
#' @examples
#' # A two-row table built by hand, with a title; print, CSV, Markdown,
#' # flextable and gt take it. Excel needs a layout: set
#' # `layout = list(xlsx_rules = "puttab")`.
#' tab <- tt_table(
#'   body = data.frame(label = c("Age, mean (SD)", "Female, n (%)"),
#'                     value = c("61.2 (8.4)", "112 (56)")),
#'   header = list(c("", "All patients")),
#'   title = "Table 1", command = "custom"
#' )
#' tab
#' validate_tt_table(tab)
#' tt_write_csv(tab, tempfile(fileext = ".csv"))
#'
#' accounted <- table1_tc(data.frame(age = c(40, NA, 60)),
#'                         vars = c(age = "contn"))
#' ledger <- accounted$meta$sample_accounting
#' ledger$measures[ledger$measures$metric %in% c("observed_n", "used_n"), ]
#' @export
tt_table <- function(body, header, rows = NULL, cols = NULL, title = NULL,
                     footnote = NULL, style = NULL, stored = list(),
                     command = "table1_tc", layout = NULL, meta = list(),
                     notes = character()) {
  if (!is.character(command) || length(command) != 1L || is.na(command) || !nzchar(command)) {
    cli::cli_abort("{.arg command} must be a single non-empty string.", call = NULL)
  }
  # Resolved here, not as the default, so the usage names no unexported
  # function (pre-release review P2-5).
  if (is.null(style)) style <- tt_resolve_style()
  lay <- tt_layout_defaults(command)
  lay[names(layout)] <- layout
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  body[] <- lapply(body, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  names(body) <- paste0("c", seq_along(body))
  rownames(body) <- NULL
  header <- lapply(header, function(h) {
    if (is.character(h)) h <- list(text = h)
    if (is.null(h$spans)) h$spans <- data.frame(from = integer(), to = integer())
    h
  })
  if (!all(vapply(header, function(h) is.character(h$text) && length(h$text) == ncol(body), TRUE))) {
    cli::cli_abort(c("Invalid {.cls tt_table}.",
                     "x" = "each header row needs one {.field text} cell per body column ({ncol(body)})."),
                   call = NULL)
  }
  if (is.null(cols)) cols <- .tt_infer_cols(header, ncol(body), lay$header_style)
  if (is.null(cols$console_width)) cols$console_width <- rep(NA_integer_, nrow(cols))
  rows <- .tt_complete_rows(rows, body)
  x <- structure(list(
    body = body, header = header, rows = rows, cols = cols,
    title = title %||% "", footnote = .tt_footnote_text(footnote),
    style = style, stored = stored, command = command, layout = lay,
    meta = meta, notes = notes
  ), class = "tt_table")
  validate_tt_table(x)
}

# `x$title <- NULL` (or the footnote) is the natural way to remove one; the
# renderers treat it as "" (Phase 6 review P3-6), as tt_table() does.
.tt_blank_text <- function(x) {
  if (is.null(x$title)) x$title <- ""
  x$footnote <- .tt_footnote_text(x$footnote)
  x
}

#' @rdname tt_table
#' @param x A tt_table.
#' @export
validate_tt_table <- function(x) {
  bad <- function(msg) cli::cli_abort(c("Invalid {.cls tt_table}.", "x" = msg), call = NULL)
  if (!inherits(x, "tt_table") || !is.list(x)) bad("{.arg x} must be a {.cls tt_table} object.")
  if (!is.list(x$layout)) bad("{.field layout} must be a list of render settings.")
  if (!is.data.frame(x$body) || !ncol(x$body)) bad("{.field body} must be a data frame with at least one column.")
  if (!all(vapply(x$body, is.character, TRUE))) bad("{.field body} cells must be character.")
  if (any(vapply(x$body, anyNA, TRUE))) bad("{.field body} cells must not be {.val NA} (use {.val {''}}).")
  nc <- ncol(x$body)
  if (!is.list(x$header)) bad("{.field header} must be a list of header rows.")
  # Only a plain-header table (puttab's noheader) may have no header row:
  # the descriptor and model layouts are built on theirs.
  if (!length(x$header) && !identical(x$layout$header_style, "plain")) {
    bad("{.field header} must be a non-empty list of header rows.")
  }
  for (h in x$header) {
    if (!is.list(h)) bad("each header row must be a list with {.field text} cells.")
    if (!is.character(h$text) || length(h$text) != nc || anyNA(h$text)) {
      bad("each header row needs one non-missing {.field text} cell per body column ({nc}).")
    }
    s <- h$spans
    if (is.null(s)) next
    if (!is.data.frame(s) || !all(c("from", "to") %in% names(s))) {
      bad("header merge spans must be a data frame with columns {.field from} and {.field to}.")
    }
    if (nrow(s)) {
      if (!is.numeric(s$from) || !is.numeric(s$to) || anyNA(s$from) || anyNA(s$to) ||
          any(s$from != round(s$from) | s$to != round(s$to))) {
        bad("header merge spans need whole-number, non-missing {.field from} and {.field to}.")
      }
      if (any(s$from < 1 | s$to > nc | s$from > s$to)) bad("header merge spans must lie inside the table.")
    }
  }
  if (!is.data.frame(x$rows) || nrow(x$rows) != nrow(x$body)) bad("{.field rows} needs one row per body row.")
  indent <- x$rows$indent
  if (!is.numeric(indent) || anyNA(indent) || any(!is.finite(indent)) ||
      any(indent < 0 | indent != round(indent))) {
    bad("{.field rows$indent} must contain non-negative, non-missing whole numbers.")
  }
  if (is.null(x$rows$type)) bad("{.field rows} needs a {.field type} column.")
  if (!all(x$rows$type %in% .tt_row_types)) bad("unknown row type(s): {setdiff(x$rows$type, .tt_row_types)}.")
  # The row fields and layout flags the renderers read directly (Codex audit
  # CX-4): an NA block made print() fail inside a scalar `if`.
  blk <- x$rows$block
  if (isTRUE(x$layout$console_sepby) && is.null(blk)) bad("{.field rows} needs a {.field block} column.")
  if (!is.null(blk) && (!is.numeric(blk) || anyNA(blk) || any(!is.finite(blk)))) {
    bad("{.field rows$block} must be numeric and non-missing (the console listing's separator groups).")
  }
  dim <- x$rows$dim
  if (!is.null(dim) && (!is.logical(dim) || anyNA(dim))) bad("{.field rows$dim} must be {.val TRUE} or {.val FALSE}, not missing.")
  for (f in c("p", "smd")) {
    v <- x$rows[[f]]
    if (!is.null(v) && !is.numeric(v) && !(is.logical(v) && all(is.na(v)))) {
      bad("{.field rows${f}} must be numeric ({.val NA} for none).")
    }
  }
  # Row identity (task 6.7): text, NA where a row has none.
  for (f in c("key", "var", "level")) {
    v <- x$rows[[f]]
    if (!is.null(v) && !is.character(v) && !(is.logical(v) && all(is.na(v)))) {
      bad("{.field rows${f}} must be character ({.val NA} for none).")
    }
  }
  if (!is.data.frame(x$cols) || nrow(x$cols) != nc) bad("{.field cols} needs one row per body column.")
  # all() of a missing column is TRUE, so a role-less `cols` used to pass
  # and then break print() (H13, F15).
  if (is.null(x$cols$role)) bad("{.field cols} needs a {.field role} column.")
  if (!all(x$cols$role %in% .tt_col_roles)) bad("unknown column role(s): {setdiff(x$cols$role, .tt_col_roles)}.")
  if (!is.character(x$command) || length(x$command) != 1L || is.na(x$command)) {
    bad("{.field command} must be a single string.")
  }
  if (!is.list(x$layout) || !isTRUE(x$layout$align %in% c("left", "right")) ||
      !isTRUE(x$layout$header_style %in% c("descriptor", "model", "plain")) ||
      !isTRUE(x$layout$xlsx_rules %in% c("descriptive", "regression", "puttab", "stacktab", "stratetab",
                                         "comptab", "hrcomptab", "none"))) {
    bad("{.field layout} needs {.field align}, {.field header_style}, and {.field xlsx_rules} from the documented values.")
  }
  for (f in c("console_sepby", "console_title", "console_blank")) {
    v <- x$layout[[f]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) bad("{.field layout${f}} must be {.val TRUE} or {.val FALSE}.")
  }
  # NULL is "" (the renderers treat it so, Phase 6 review P3-6); NA_character_
  # would be written as a literal "NA".
  for (f in c("title", "footnote")) {
    v <- x[[f]]
    if (identical(f, "footnote")) {
      .tt_check_footnote_arg(v)
      next
    }
    if (!is.null(v) && (!is.character(v) || length(v) != 1L || is.na(v))) {
      bad("{.field {f}} must be a single non-missing string (or {.code NULL} for none).")
    }
  }
  if (!is.list(x$stored)) bad("{.field stored} must be a list.")
  if (!is.list(x$meta)) bad("{.field meta} must be a list.")
  sample <- x$meta[["sample_accounting", exact = TRUE]]
  if (!is.null(sample)) .tt_validate_sample_accounting(sample)
  x
}

# Column roles from header text when an engine does not supply them (a
# convenience for hand-built tables; engines pass `cols`, since Stata knows
# roles from variable names, desctab.ado:1709-1760). Test, Statistic,
# p-value and SMD are only recognised as a trailing run, in that order, with
# a blank second header row, and
# Total only beside the label or just before that run, so a group labelled
# "Test" or "Total" elsewhere stays a group.
.tt_infer_cols <- function(header, nc, header_style) {
  role <- rep("group", nc)
  role[1] <- "label"
  model <- rep(NA_integer_, nc)
  if (!length(header)) {
    role[-1] <- "value"
    return(data.frame(role = role, model = model, console_width = NA_integer_,
                      stringsAsFactors = FALSE))
  }
  last <- header[[length(header)]]$text
  first <- header[[1]]$text
  if (header_style == "descriptor") {
    f <- trimws(first)
    # Stata merges these headers over both header rows, so the second row is
    # blank there; group columns carry "N=...".
    second <- if (length(header) > 1L) trimws(header[[2]]$text) else rep("", nc)
    special <- c(Test = "test", Statistic = "statistic", "p-value" = "p", SMD = "smd")
    j <- nc
    last_rank <- Inf
    # The SMD header comes from .t1_smd_header(): "SMD", "SMD (A vs B)",
    # "Pop. SB" or "Max SMD".
    smd_fixed <- c("SMD", .t1_smd_header("population", NULL), .t1_smd_header("maxpair", NULL))
    is_smd <- function(h) h %in% smd_fixed || (startsWith(h, "SMD (") && endsWith(h, ")"))
    key_of <- function(h) if (is_smd(h)) "SMD" else h
    while (j > 1L && (f[j] %in% names(special) || is_smd(f[j])) && !nzchar(second[j])) {
      rank <- match(key_of(f[j]), names(special))
      if (rank >= last_rank) break
      role[j] <- special[[key_of(f[j])]]
      last_rank <- rank
      j <- j - 1L
    }
    # A "Total" column is the total() column only beside group columns; the
    # single column of a table without by() is an ordinary data column.
    if (sum(role == "group") > 1L) {
      if (f[2] == "Total") role[2] <- "total"
      else if (j > 2L && f[j] == "Total") role[j] <- "total"
    }
    role[1] <- "label"
  } else {
    role[-1] <- "est"
    role[grepl("% CI$", last) & !grepl(" ", sub("% CI$", "", last))] <- "ci"
    role[last == "p-value"] <- "pval"
    role[grepl(".+ [0-9.]+% CI$", last)] <- "est_ci"
    role[1] <- "label"
    starts <- which(nzchar(trimws(first)) & seq_len(nc) > 1L)
    if (!length(starts)) starts <- 2L
    # seq_len(nc)[-1]: a label-only table has no value column (2:1 would
    # visit columns 2 and 1; H13, F13).
    for (j in seq_len(nc)[-1]) model[j] <- sum(starts <= j)
  }
  # Stata lists the SMD column at least 7 wide even when every |SMD| is
  # "0.019" (all 11 SMD goldens); the listing runs before the final text is
  # in place. Engines may set console_width for any column.
  console_width <- ifelse(role == "smd", 7L, NA_integer_)
  data.frame(role = role, model = model, console_width = console_width,
             stringsAsFactors = FALSE)
}

.tt_complete_rows <- function(rows, body) {
  n <- nrow(body)
  lab <- body[[1]]
  indent <- nchar(lab) - nchar(sub("^ +", "", lab))
  if (is.null(rows)) rows <- data.frame(row.names = seq_len(n))
  if (is.null(rows$indent)) rows$indent <- indent
  if (is.null(rows$type)) rows$type <- ifelse(rows$indent > 0, "level", "var")
  if (is.null(rows$block)) rows$block <- cumsum(rows$indent == 0 | seq_len(n) == 1L)
  for (v in c("key", "var", "level")) if (is.null(rows[[v]])) rows[[v]] <- rep(NA_character_, n)
  for (v in c("p", "smd")) if (is.null(rows[[v]])) rows[[v]] <- rep(NA_real_, n)
  if (is.null(rows$dim)) rows$dim <- rep(FALSE, n)
  if (is.null(rows$suppression)) rows$suppression <- rep(0L, n)
  rownames(rows) <- NULL
  rows
}

#' @rdname print.tt_table
#' @export
format.tt_table <- function(x, ...) tt_console_lines(x)

#' Print a tt_table as Stata's boxed console listing
#'
#' Mirrors Stata `list ..., noobs noheader table`: table1_tc shows
#' both header rows, left-aligned columns, and a rule after every variable
#' block (`sepby`), followed by notes and footnote paragraphs; regtab prints the title above,
#' right-aligned columns without separators, then a blank line.
#' `format()` returns the same listing as a character vector of lines.
#'
#' @param x A tt_table.
#' @param ... Unused.
#' @return `print()` returns `x`, invisibly. `format()` returns a character
#'   vector, with one element per line of the console listing.
#' @examples
#' d <- data.frame(arm = rep(c("A", "B"), each = 20), age = c(51:70, 55:74))
#' tab <- table1_tc(d, by = "arm", vars = c(age = "contn %5.1f"))
#' print(tab)
#' @export
print.tt_table <- function(x, ...) {
  cat(tt_console_lines(x), sep = "\n")
  invisible(x)
}

#' Header-shaped data frame of a tt_table
#'
#' The analogue of Stata's `frame()` / `clear`: header rows followed by the
#' body, one character column per table column.
#'
#' @param x A tt_table.
#' @param row.names,optional Unused.
#' @param ... Unused.
#' @return data.frame of character cells. Tables that carry Stata frame
#'   characteristics (`x$meta$frame`; regtab: `source`, `ci_level`,
#'   `n_models`, `statistic_ids`, and per-model `model_id`, `outcome_id`,
#'   `effect_scale`, `model_label`) return them as attributes.
#'   When present, the source ledger documented in [tt_table()] is returned
#'   as a separate `sample_accounting` attribute. Producer-owned publication
#'   snapshots, when available, remain in `composition` for exact source reuse
#'   and numeric companion formatting; changing text or identity invalidates
#'   their use by [comptab()].
#' @examples
#' fit <- glm(am ~ wt + factor(cyl), family = binomial, data = mtcars)
#' df <- as.data.frame(regtab(fit))
#' head(df)
#' attr(df, "effect_scale")
#' attr(df, "sample_accounting")$populations
#' @export
as.data.frame.tt_table <- function(x, row.names = NULL, optional = FALSE, ...) {
  hdr <- do.call(rbind, lapply(x$header, function(h) h$text))
  out <- as.data.frame(rbind(hdr, as.matrix(x$body)), stringsAsFactors = FALSE)
  names(out) <- names(x$body)
  rownames(out) <- NULL
  for (a in names(x$meta$frame)) attr(out, a) <- x$meta$frame[[a]]
  if (!is.null(x$meta$composition)) attr(out, "composition") <- x$meta$composition
  sample <- x$meta[["sample_accounting", exact = TRUE]]
  if (!is.null(sample)) {
    .tt_validate_sample_accounting(sample)
    attr(out, "sample_accounting") <- sample
  }
  out
}

# Header rows + body as one character matrix, with the sink-only extraspace
# prefix when requested.
.tt_cells <- function(x, extraspace = FALSE, header_extraspace = TRUE) {
  g <- rbind(do.call(rbind, lapply(x$header, function(h) h$text)), as.matrix(x$body))
  if (extraspace && isTRUE(x$meta$extraspace)) {
    rows <- seq_len(nrow(g))
    if (!header_extraspace) rows <- setdiff(rows, seq_along(x$header))
    for (j in which(x$cols$role == "p")) {
      k <- rows[!startsWith(g[rows, j], "<")]
      g[k, j] <- paste0(" ", g[k, j])
    }
  }
  dimnames(g) <- NULL
  g
}

# CSV-shaped grid: [title] + header rows + body + [footnote] (the csv() sink).
.tt_grid <- function(x, title = TRUE, footnote = TRUE) {
  nc <- ncol(x$body)
  pad <- function(v) c(v, rep("", nc - length(v)))
  g <- .tt_cells(x, extraspace = TRUE)
  # Stata's csv writer drops leading rows blank in every column (the reserved
  # title row of collect-style tables; _tabtools_csv_write.ado reservedrow).
  # puttab exports the caller's rows as they are, so it opts out.
  if (!isFALSE(x$layout$csv_reservedrow)) {
    while (nrow(g) && all(!nzchar(trimws(g[1, ])))) g <- g[-1, , drop = FALSE]
  }
  if (title && nzchar(x$title)) g <- rbind(pad(x$title), g)
  if (footnote) {
    for (para in .tt_footnote_paragraphs(x$footnote)) g <- rbind(g, pad(para))
  }
  dimnames(g) <- NULL
  g
}
