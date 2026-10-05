# stratetab (task 7.4): incidence rates per outcome x exposure block as one
# table -- outcomes as column groups (Events, Person-Years, Rate with its
# interval, and optionally the rate ratio), exposure groups as row blocks.
# Port of Stata tabtools 2.1.12 stratetab.ado (997 lines), following the
# 2.1.14 fixes since task C8 (zero-event cells, one Markdown header row,
# exposure rules by position, the exact rounding unit, irr_ci provenance).
#
# Stata reads one saved `strate` file per outcome x exposure block
# (`_D _Y _Rate _Lower _Upper` per category, :303-462). R takes the same
# blocks as tt_rates() results (computed from time/event data), as
# strate-shaped data frames, or as the .dta files themselves. The numbers
# pass through Stata's local macros (`local D_o1_e1_1 = _D[1]`, :420-458),
# which keep 16 significant digits (12 in e-notation), so every value is
# rounded the same way here (stata_macro_num()) before it is formatted.

#' Incidence-rate table: events, person-years and rates
#'
#' Combines one block of event rates per outcome x exposure (from
#' [tt_rates()], or from Stata's `strate`) into one table: the outcomes as
#' column groups -- Events, Person-Years (PY), and the rate with its
#' confidence interval, plus an incidence rate ratio column with
#' `rateratio` -- and the exposure groups as row blocks, each category
#' indented under its exposure label. R implementation of the Stata
#' `stratetab` command; the table prints, and is written to Excel with the
#' tabtools house style, and to CSV and Markdown, like the other commands.
#'
#' @section Blocks:
#' `x` lists the blocks in Stata's `using()` order: every outcome for
#' exposure 1, then every outcome for exposure 2, and so on (with 3 outcomes
#' and 2 exposures: `O1_E1, O2_E1, O3_E1, O1_E2, O2_E2, O3_E2`). Each block
#' is one of
#' * a [tt_rates()] result (computed from time and event data): its level
#'   is known;
#' * a data frame in strate shape (`_D`/`D`, `_Y`/`Y`, `_Rate`/`Rate`,
#'   `_Lower`/`Lower`, `_Upper`/`Upper`, plus a category column), such as a
#'   `strate` file read with `haven::read_dta()`;
#' * the path of a `strate` output file (`.dta`, added when missing; needs
#'   the haven package).
#'
#' The category column is the first column that is not one of the five;
#' a block without one is a single row labelled `Overall`. A category
#' column named `D`, `Y`, `Rate`, `Lower` or `Upper` beside strate's `_D`,
#' `_Y`, ... (a `strate` file whose grouping variable has one of those
#' names) is refused with a rename hint: R's blocks use those names for the
#' statistics (Stata's `stratetab` reads such a file). Categories show
#' their labels (factor levels, or `haven::labelled()` value labels, as
#' Stata's `decode`), character values as they are, and unlabelled numbers
#' as Stata's `%21.0g` (`1`, `10000000`); a blank or duplicated label is an
#' error. Within an exposure, every outcome's block must hold the same
#' categories: later outcomes are aligned to the first outcome's order by
#' label.
#'
#' @section Scaling and intervals:
#' The table shows each block's events, its person-time divided by
#' `pyscale`, and its rate and bounds multiplied by `ratescale`, as they are
#' stored in the block: a block computed with `tt_rates(per = 1000)` (or
#' `strate, per(1000)`) already holds rates per 1,000 time units and
#' person-time in thousands, so pass `ratescale = 1` and `pyscale = 1/1000`
#' for it. R knows the `per` of a [tt_rates()] result, so such blocks are
#' refused unless `ratescale` is given (the default would show rates `per`
#' times too high), blocks with different `per` are refused, and when
#' `ratescale` is given but a numeric `unitlabel` matches neither
#' `per * ratescale` nor `per * ratescale / pyscale` there is a warning
#' (`per = 1000` with `ratescale = 1000` is per million). A
#' `strate` file does not record its `per()`, so for one it is up to you. A block cannot say which time unit it counts in, so `unitlabel`
#' (the `1,000` of the header `Per 1,000 PY`) is yours to keep in step with
#' `ratescale`; person-time in days, for example, needs `pyscale = 365.25`
#' and `ratescale = 365250` for rates per 1,000 person-years.
#'
#' The interval level is the blocks' own: a [tt_rates()] result records
#' it, and a strate-shaped block names it in the `"label"` attributes of
#' its interval columns (`strate` labels them `Lower 95% confidence
#' limit`). All blocks must agree, and `level`, if given, must match them.
#' When any block carries no level, `level` is required: the level is
#' never assumed to be 95%. The level labels the headers and sets the
#' rate-ratio intervals.
#'
#' With `rateratio = TRUE`, each category of exposure 2, 3, ... is compared
#' with the category of the same label in exposure 1 (the reference group,
#' shown as `Ref.`): the rate ratio of the two rates with the log-normal
#' interval `exp(log(IRR) -/+ z * sqrt(1/D1 + 1/D2))`, which assumes
#' independent, non-overlapping rates. A ratio without events on either
#' side (or a zero reference rate) is shown as a dash. Categories that do
#' not match exposure 1 one to one are an error. A rate without bounds
#' (`strate` gives none for zero events) is shown with a dash for its
#' interval (`0.0` and an en dash in parentheses), as in Stata tabtools
#' 2.1.14.
#'
#' @param x The blocks: a list of [tt_rates()] results, strate-shaped data
#'   frames, or `.dta` paths (see Blocks), in Stata's order; a character
#'   vector of paths; or a single data frame (one outcome, one exposure).
#' @param outcomes Number of outcomes (Stata's required `outcomes()`); the
#'   number of blocks must be a multiple of it. May be omitted for a single
#'   block.
#' @param xlsx Output `.xlsx` workbook. The sheet is replaced if it exists
#'   (matched without regard to case) and added otherwise.
#' @param sheet Sheet name (default `"Results"`).
#' @param title Title in cell `A1` (and above the console listing, the
#'   first CSV row, and the `###` heading of the Markdown file).
#' @param outlabels Outcome labels, one per outcome. By default, when every
#'   block is a [tt_rates()] result from time and event data, each outcome
#'   is named by its event column's `"label"` attribute, else the column's
#'   name; otherwise (strate-shaped blocks and `.dta` files, as in Stata),
#'   and when the names would repeat or differ between exposures,
#'   `"Outcome 1"`, `"Outcome 2"`, ...
#' @param outcomeids Machine-readable outcome identities, one per outcome,
#'   stored with the table for joining rates to models. By default, when
#'   every block is a [tt_rates()] result from time and event data, each
#'   outcome's event column name (a [survival::coxph()] model's outcome
#'   identity, so [hrcomptab()] matches the two without `outcomemap`);
#'   otherwise the outcome labels, as in Stata. Blank or case-insensitively
#'   duplicated identities are errors.
#' @param explabels Exposure-group labels, one per exposure. By default,
#'   when every block is a [tt_rates()] result from time and event data,
#'   each exposure is named by its category column's `"label"` attribute,
#'   else the column's name (`by`); otherwise, and when the names would
#'   repeat (the same `by` in every exposure) or a block has no grouping
#'   column, `"Exposure 1"`, `"Exposure 2"`, ..., as in Stata.
#' @param digits Decimals for rates and their bounds, 0 to 10 (default 1).
#' @param eventdigits Decimals for events, 0 to 10 (default 0).
#' @param pydigits Decimals for person-years, 0 to 10 (default 0).
#' @param unitlabel Rate unit in the header (default `"1,000"`: `Per 1,000
#'   PY (95% CI)`); an empty string is the default too, as in Stata.
#' @param pyscale Person-time is divided by this positive number (default
#'   1).
#' @param ratescale Rates and bounds are multiplied by this positive number
#'   (default 1000).
#' @param rateratio Add an incidence rate ratio column per outcome, against
#'   exposure group 1.
#' @param ratiodigits Decimals for rate ratios, 0 to 10 (default 2).
#' @param footnote Footnote below the table (smaller italic font).
#' @param level Confidence level of the blocks' intervals, a proportion
#'   (`0.90`) or a percentage (`90`); required when a block does not record
#'   its level (see Scaling and intervals).
#' @param font,fontsize Font family and size (defaults from
#'   [tabtools_options()], else Arial 10).
#' @param borderstyle `"default"`/`"thin"`, `"medium"`, or `"academic"` (no
#'   vertical rules, medium horizontal rules).
#' @param headershade Shade the two header rows.
#' @param headercolor,zebracolor Colours for `headershade` and `zebra`.
#' @param zebra Shade every second body row, starting with the second.
#' @param csv Also write the table to this `.csv` file.
#' @param markdown Also write the table to this Markdown file (`.md`,
#'   `.markdown`, `.qmd`, or `.rmd`). Markdown tables have one header row,
#'   so the outcome and statistic headers are joined as `outcome:
#'   statistic` (for example, `CV Events: Events`), as in Stata tabtools
#'   2.1.14; `markdown_rows` counts the data rows.
#' @param mdappend Append to an existing `markdown` file.
#' @param open Open the workbook after writing (interactive sessions).
#' @return A `tt_table` (`command = "stratetab"`), returned invisibly when
#'   `xlsx`, `csv` or `markdown` writes a file (assign it and print it to see
#'   it). `$stored` holds Stata's `r()` results: `N_rows` (the
#'   title row, both header rows, and the body rows), `N_exposures`,
#'   `N_outcomes`, `ci_level`, `outcome_ids` (joined by `" \ "`), `methods`,
#'   the matrices `rates` (one row per category, one column per outcome,
#'   scaled as displayed) and, with `rateratio`, `ratios` (one row per
#'   non-reference category), and, for the files written, `xlsx`, `sheet`,
#'   `csv`, `markdown`, `markdown_rows`, and `markdown_cols`.
#'
#'   Stata's `frame()` analogue is [as.data.frame()] of the table, which
#'   carries the frame characteristics a composite table joins on as
#'   attributes (`$meta$frame`): `source = "stratetab"`, `ci_level` (a
#'   number, e.g. `97.5`; Stata stores the text), `statistic_ids =
#'   "events person_years rate_ci"` (`"events person_years rate_ci
#'   irr_ci"` with `rateratio`, so [comptab()] refuses the table by name),
#'   `n_outcomes`, `outcome_id_1`,
#'   `outcome_id_2`, ... (Stata's characteristic names), the same
#'   identities as one vector, `outcome_id`, with `outcome_label` beside
#'   it, the `title` and `footnote` (Stata's frame holds the title in its
#'   first row), and the column layout: `rateratio` and `cols_per_outcome`
#'   (3, or 4 with the IRR column; the width alone does not tell a
#'   3-outcome rate-ratio table from a 4-outcome plain one).
#'
#'   `$meta$rate_rows` holds the numbers behind every body cell: one row
#'   per category x outcome with `row`, exposure, category, outcome index,
#'   identity and label, `events`, `person_years`, `rate`, `lower`, `upper`
#'   (scaled as displayed), and `irr`, `irr_lower`, `irr_upper`. `row` is
#'   the body row: row `row + 2` of [as.data.frame()] (after its two header
#'   rows) and row `row + 3` of Stata's frame and of the worksheet (after
#'   the title row). `irr` is relative to the same category in exposure 1
#'   (missing in exposure 1 itself, which shows `Ref.`). That is not
#'   comptab's reference row, which is the first category row of each
#'   exposure block by position (`$rows$type == "level"`, first within
#'   `$rows$block`).
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @section Differences from Stata:
#' * R has no frames: the returned table (and [as.data.frame()] of it)
#'   stands for `frame()`. No file is required.
#' * Like [regtab()] and [table1_tc()], R prints no `Exported to ...` or
#'   `Markdown exported to ...` lines; the listing is the printed table.
#' * `level` also takes a proportion (`0.9`), as in [regtab()].
#' * An infinite `pyscale` or `ratescale` is refused (Stata has no
#'   infinities; a missing one is refused on both sides).
#' * Blocks with negative, infinite, or non-numeric counts, person-time, or
#'   rates are refused, [tt_rates()] results and strate-shaped blocks alike;
#'   Stata formats whatever the file holds.
#' * [tt_rates()] blocks computed with `per` other than 1 need an explicit
#'   `ratescale` (see Scaling and intervals).
#' * Labels and identities are character vectors, one element each, rather
#'   than one string split at `\`.
#' * Blocks from [tt_rates()] of time and event data name their exposures
#'   and outcomes by default (the grouping and event columns' labels, else
#'   their names; see `explabels` and `outlabels`). Stata's `strate` files
#'   carry no such names, so for them, as in Stata, the defaults are
#'   `Exposure 1`, ... and `Outcome 1`, ....
#' @seealso [tt_rates()] for the blocks; [regtab()] for the hazard-ratio
#'   models beside them.
#' @examples
#' # Rates per 1,000 person-years from time and event data: one tt_rates()
#' # block per outcome, the arms as the categories
#' set.seed(2)
#' d <- data.frame(years = rexp(400, 0.05), event = rbinom(400, 1, 0.4),
#'                 relapse = rbinom(400, 1, 0.2), arm = rep(c("A", "B"), 200),
#'                 sex = rep(c("Men", "Women"), each = 200))
#' blocks <- list(tt_rates(d, "years", "event", by = "arm"),
#'                tt_rates(d, "years", "relapse", by = "arm"))
#' tab <- stratetab(blocks, outcomes = 2, outlabels = c("Death", "Relapse"),
#'                  outcomeids = c("death", "relapse"), explabels = "Arm")
#' tab
#'
#' # Rate ratios compare each exposure group with the first, category by
#' # category: here women with men, within each arm
#' men <- d[d$sex == "Men", ]
#' women <- d[d$sex == "Women", ]
#' stratetab(list(tt_rates(men, "years", "event", by = "arm"),
#'                tt_rates(women, "years", "event", by = "arm")),
#'           outcomes = 1, outlabels = "Death", explabels = c("Men", "Women"),
#'           rateratio = TRUE)
#'
#' # Written to a workbook, the table is returned invisibly
#' tab <- stratetab(blocks, outcomes = 2, xlsx = tempfile(fileext = ".xlsx"))
#'
#' # Blocks saved by Stata's strate (read with haven::read_dta()) have
#' # _D, _Y, _Rate, _Lower and _Upper columns; the interval labels give
#' # the level
#' blk <- function(d, y, rate, lo, hi) {
#'   b <- data.frame(drug = c("SSRI", "SNRI"), `_D` = d, `_Y` = y, `_Rate` = rate,
#'                   `_Lower` = lo, `_Upper` = hi, check.names = FALSE)
#'   attr(b[["_Lower"]], "label") <- "Lower 95% confidence limit"
#'   attr(b[["_Upper"]], "label") <- "Upper 95% confidence limit"
#'   b
#' }
#' cv_m <- blk(c(178, 161), c(28100, 22300), c(0.00633, 0.00722),
#'             c(0.00545, 0.00616), c(0.00734, 0.00844))
#' cv_f <- blk(c(134, 128), c(24380, 19520), c(0.00550, 0.00656),
#'             c(0.00462, 0.00549), c(0.00652, 0.00781))
#' stratetab(list(cv_m, cv_f), outcomes = 1, outlabels = "CV events",
#'           explabels = c("Male", "Female"), rateratio = TRUE)
#' @export
stratetab <- function(x, outcomes = NULL, xlsx = NULL, sheet = "Results", title = NULL,
                      outlabels = NULL, outcomeids = NULL, explabels = NULL,
                      digits = 1, eventdigits = 0, pydigits = 0, unitlabel = NULL,
                      pyscale = 1, ratescale = 1000, rateratio = FALSE, ratiodigits = 2,
                      footnote = NULL, level = NULL, font = NULL, fontsize = NULL,
                      borderstyle = NULL, headershade = FALSE, headercolor = NULL,
                      zebra = FALSE, zebracolor = NULL, csv = NULL, markdown = NULL,
                      mdappend = FALSE, open = FALSE) {
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Results"
  ratescale_given <- !missing(ratescale)
  for (a in c("rateratio", "headershade", "zebra", "mdappend", "open")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  has_xlsx <- !is.null(xlsx)
  .tt_check_sheet_xlsx(sheet_given, has_xlsx)
  has_md <- !is.null(markdown)
  # stratetab.ado:84-176, in Stata's order.
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must have a .xlsx extension.", call = NULL)
  }
  if (!is.null(csv)) .tt_check_csv_path(csv)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L || is.na(markdown) ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  sheet <- .check_sheet(sheet)
  digits <- .check_int_range(digits, "digits", 0, 10)
  eventdigits <- .check_int_range(eventdigits, "eventdigits", 0, 10)
  pydigits <- .check_int_range(pydigits, "pydigits", 0, 10)
  pyscale <- .st_check_scale(pyscale, "pyscale")
  ratescale <- .st_check_scale(ratescale, "ratescale")
  blocks <- .st_as_blocks(x)
  if (!length(blocks)) cli::cli_abort("{.arg x} holds no blocks.", call = NULL)
  if (is.null(outcomes)) {
    if (length(blocks) != 1L) {
      cli::cli_abort(c("{.arg outcomes} is required: the number of outcomes among the {length(blocks)} blocks.",
                       "i" = "Blocks are ordered all outcomes for exposure 1, then all outcomes for exposure 2, ..."),
                     call = NULL)
    }
    outcomes <- 1L
  }
  if (!is.numeric(outcomes) || length(outcomes) != 1L || is.na(outcomes) || outcomes != round(outcomes)) {
    cli::cli_abort("{.arg outcomes} must be a whole number.", call = NULL)
  }
  if (outcomes < 1) cli::cli_abort("{.arg outcomes} must be at least 1.", call = NULL)
  outcomes <- as.integer(outcomes)
  level_pct <- .st_check_level(level)
  for (a in c("title", "footnote", "unitlabel")) .tt_check_text_arg(get(a), a)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra,
                            headercolor = headercolor, zebracolor = zebracolor)
  ratiodigits <- .check_int_range(ratiodigits, "ratiodigits", 0, 10)
  n_blocks <- length(blocks)
  if (!n_blocks || n_blocks %% outcomes != 0L) {
    cli::cli_abort(c("The number of blocks ({n_blocks}) must be a multiple of {.arg outcomes} ({outcomes}).",
                     "i" = "Blocks are ordered all outcomes for exposure 1, then all outcomes for exposure 2, ..."),
                   call = NULL)
  }
  n_exp <- n_blocks %/% outcomes
  if (rateratio && n_exp < 2L) {
    cli::cli_abort("{.arg rateratio} requires at least two exposure groups.", call = NULL)
  }
  # Stata's "Outcome k" / "Exposure k", unless every block is a tt_rates()
  # result that names its event and grouping variable (D02).
  defaults <- .st_block_defaults(blocks, outcomes, n_exp)
  outlabels <- .st_labels(outlabels, outcomes, "outlabels", "outcome labels", "outcomes",
                          defaults$out %||% paste("Outcome", seq_len(outcomes)))
  # On the tt_rates() route the outcome identity is the event variable,
  # which is a Cox model's outcome identity too (review of 2026-09-28, M2);
  # Stata's default, the outcome label, otherwise.
  ids <- .st_outcome_ids(outcomeids %||% defaults$ids, outlabels, outcomes)
  explabels <- .st_labels(explabels, n_exp, "explabels", "exposure labels", "exposure groups",
                          defaults$exp %||% paste("Exposure", seq_len(n_exp)))
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  # stratetab.ado:293-295: an empty unitlabel() is the default (Phase 7b
  # review F3); a blank one (" ") is kept, as Stata keeps it.
  if (is.null(unitlabel) || !nzchar(unitlabel)) unitlabel <- "1,000"

  # Read every block (stratetab.ado:303-462) and resolve the level.
  data <- .st_read_blocks(blocks, outcomes, n_exp, pyscale, ratescale, level_pct)
  ci_level <- .st_resolve_level(data$levels, level_pct)
  .st_check_pers(data$pers, ratescale_given)
  .st_check_scales(data$pers, pyscale, ratescale, unitlabel)
  ci_txt <- .tt_level_text(ci_level)
  alpha <- stata_macro_num((100 - ci_level) / 200)
  z <- stata_macro_num(stats::qnorm(1 - alpha))
  irr <- if (rateratio) .st_ratios(data, outcomes, n_exp, z) else NULL

  tt <- .st_build(data, irr, outcomes, n_exp, outlabels, ids, explabels, ci_level, ci_txt,
                  unitlabel = unitlabel, digits = digits, eventdigits = eventdigits,
                  pydigits = pydigits, ratiodigits = ratiodigits, rateratio = rateratio,
                  title = title %||% "", footnote = footnote %||% "", style = style, sheet = sheet)
  tt$meta$sample_accounting <- .tt_sample_bind(data$samples,
    prefixes = paste0("block", seq_along(data$samples)), commands = rep("stratetab", length(data$samples)))

  # Sinks in Stata's order: CSV, Markdown, then the workbook (:645-960).
  written <- FALSE
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    tt$stored$csv <- csv
    written <- TRUE
  }
  if (has_md) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (has_xlsx) {
    # Excel matches an existing sheet without regard to case; Stata reports
    # the workbook's own spelling (:812-814).
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx
    tt$stored$sheet <- sheet
    tt$meta$sheet <- sheet
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}

# ---------------------------------------------------------------------------
# Arguments

# pyscale()/ratescale(): positive and nonmissing (stratetab.ado:122-131,
# tabtools 2.1.12); Inf is refused as well.
.st_check_scale <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a positive number.", call = NULL)
  }
  if (x <= 0) cli::cli_abort("{.arg {arg}} must be positive.", call = NULL)
  as.numeric(x)
}

# level(): NULL, a proportion in (0, 1) or a percentage in (1, 100)
# (regtab's convention); returned as a percentage, NA for none.
.st_check_level <- function(level) {
  if (is.null(level)) return(NA_real_)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 100 ||
      level == 1) {
    cli::cli_abort("{.arg level} must be a proportion in (0, 1) or a percentage in (1, 100).", call = NULL)
  }
  pct <- if (level < 1) level * 100 else level
  round(pct, 10)
}

# outlabels()/explabels() (stratetab.ado:178-200, :266-288): one per
# outcome / exposure group, trimmed; Stata's defaults otherwise.
.st_labels <- function(x, n, arg, what, of, default) {
  if (is.null(x)) return(default)
  if (!is.character(x) || anyNA(x)) cli::cli_abort("{.arg {arg}} must be a character vector.", call = NULL)
  x <- trimws(x, whitespace = "[ ]")
  if (length(x) != n) {
    cli::cli_abort("Number of {what} ({length(x)}) must match the number of {of} ({n}).", call = NULL)
  }
  if (any(!nzchar(x))) cli::cli_abort("{.arg {arg}} may not contain blank labels.", call = NULL)
  x
}

# Default outcome and exposure labels from tt_rates() blocks (D02; not in
# Stata, whose strate files carry no variable names to use). Only when every
# block is a tt_rates() result from time and event data (attribute
# `event`): an exposure is named by the label of its category column (the
# column .st_block_cats() shows), else the column's name, and an outcome by
# its event column's label, else its name. Each is NULL -- Stata's
# defaults -- when a block lacks the information, the outcome's blocks
# disagree across exposures, or the labels would repeat (case-insensitively:
# outcome labels are the default outcome identities).
.st_block_defaults <- function(blocks, outcomes, n_exp) {
  none <- list(out = NULL, exp = NULL)
  text1 <- function(x) {
    if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))) trimws(x) else NULL
  }
  computed <- vapply(blocks, function(b) {
    is.data.frame(b) && identical(attr(b, "ci_method"), "lognormal") && !is.null(text1(attr(b, "event")))
  }, logical(1))
  if (!all(computed)) return(none)
  stats <- c("D", "Y", "Rate", "Lower", "Upper")
  exp_lab <- vapply(blocks, function(b) {
    other <- setdiff(names(b), stats)
    if (!length(other)) return(NA_character_)
    text1(attr(b[[other[1]]], "label", exact = TRUE)) %||% other[1]
  }, character(1))
  out_lab <- vapply(blocks, function(b) {
    text1(attr(b, "event_label", exact = TRUE)) %||% text1(attr(b, "event"))
  }, character(1))
  # Blocks run outcome-fastest: block (e - 1) * outcomes + o.
  dim(exp_lab) <- dim(out_lab) <- c(outcomes, n_exp)
  usable <- function(x) !anyNA(x) && !anyDuplicated(tolower(x))
  ex <- exp_lab[1L, ]
  if (!usable(ex) || any(exp_lab != rep(ex, each = outcomes))) ex <- NULL
  ou <- out_lab[, 1L]
  if (!usable(ou) || any(out_lab != ou)) ou <- NULL
  ev <- vapply(blocks, function(b) text1(attr(b, "event")), character(1))
  dim(ev) <- c(outcomes, n_exp)
  id <- ev[, 1L]
  if (!usable(id) || any(ev != id)) id <- NULL
  list(out = ou, exp = ex, ids = id)
}

# outcomeids() (stratetab.ado:202-240): default the outcome labels; blank
# and case-insensitively duplicated identities are refused.
.st_outcome_ids <- function(ids, outlabels, n) {
  if (is.null(ids)) {
    ids <- outlabels
  } else {
    if (!is.character(ids) || anyNA(ids)) cli::cli_abort("{.arg outcomeids} must be a character vector.", call = NULL)
    if (length(ids) != n) {
      cli::cli_abort("Number of outcome IDs ({length(ids)}) must match the number of outcomes ({n}).", call = NULL)
    }
  }
  ids <- trimws(ids, whitespace = "[ ]")
  if (any(!nzchar(ids))) cli::cli_abort("Outcome identities may not be blank.", call = NULL)
  dup <- duplicated(tolower(ids))
  if (any(dup)) cli::cli_abort("Duplicate outcome identity {.val {ids[dup][1]}}.", call = NULL)
  ids
}

# ---------------------------------------------------------------------------
# Blocks

.st_as_blocks <- function(x) {
  if (is.data.frame(x)) return(list(x))
  if (is.character(x)) {
    if (anyNA(x)) cli::cli_abort("{.arg x} must not contain missing paths.", call = NULL)
    return(as.list(x))
  }
  if (is.list(x)) return(unname(x))
  cli::cli_abort("{.arg x} must be a list of rate blocks (data frames or {.file .dta} paths), not {.obj_type_friendly {x}}.",
                 call = NULL)
}

# One block as Stata reads it (stratetab.ado:310-356): the standardised
# columns, the level its intervals were computed at (percent; NA when
# unknown), and the `per` of a tt_rates() result (1 for a strate-shaped
# block, whose units are the file's).
.st_read_block <- function(b, k) {
  if (is.character(b) && length(b) == 1L) b <- .st_read_dta(b, k)
  if (!is.data.frame(b)) {
    cli::cli_abort("Block {k} must be a data frame or the path of a {.file .dta} file, not {.obj_type_friendly {b}}.",
                   call = NULL)
  }
  stats <- c("D", "Y", "Rate", "Lower", "Upper")
  computed <- identical(attr(b, "ci_method"), "lognormal") && all(stats %in% names(b))
  if (computed) {
    lv <- attr(b, "level")
    level <- if (is.numeric(lv) && length(lv) == 1L && !is.na(lv)) round(lv * 100, 10) else NA_real_
    per <- attr(b, "per")
    per <- if (is.numeric(per) && length(per) == 1L && is.finite(per)) per else 1
    s <- b
  } else {
    s <- tryCatch(tt_rates_from_strate(b, level = NULL), error = function(e) {
      cli::cli_abort("Block {k} is not a valid strate block.", parent = e, call = NULL)
    })
    level <- if (is.na(attr(s, "level"))) NA_real_ else round(attr(s, "level") * 100, 10)
    per <- 1
  }
  # The same checks for both kinds of block (Phase 7b review F11): a
  # tt_rates() result edited by hand is validated too.
  num <- lapply(stats, function(v) {
    x <- s[[v]]
    if (!is.numeric(x) && !is.logical(x)) cli::cli_abort("Block {k}: column {.field {v}} must be numeric.", call = NULL)
    x <- as.numeric(unclass(x))
    if (any(is.infinite(x) | is.nan(x))) cli::cli_abort("Block {k}: column {.field {v}} must be finite.", call = NULL)
    if (any(x < 0, na.rm = TRUE)) cli::cli_abort("Block {k}: column {.field {v}} must be non-negative.", call = NULL)
    x
  })
  names(num) <- stats
  list(s = s, level = level, per = per, num = num)
}

# The block's category labels (stratetab.ado:358-412): the first column
# that is not one of the five; none means one overall row.
.st_block_cats <- function(s, k) {
  other <- setdiff(names(s), c("D", "Y", "Rate", "Lower", "Upper"))
  cats <- if (length(other)) .st_category_text(s[[other[1]]]) else rep("Overall", nrow(s))
  cats <- trimws(cats, whitespace = "[ ]")
  if (any(!nzchar(cats))) cli::cli_abort("Blank category labels are not allowed in block {k}.", call = NULL)
  if (anyDuplicated(cats)) {
    cli::cli_abort(c("Duplicate category labels found in block {k}.",
                     "i" = "Each block must have unique category labels."), call = NULL)
  }
  cats
}

.st_read_dta <- function(path, k) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort("Reading strate files needs the {.pkg haven} package; install it, or pass the blocks as data frames.",
                   call = NULL)
  }
  # Stata's `use "file.dta"`: using() names are given without the extension.
  if (!grepl("\\.dta$", path, ignore.case = TRUE)) path <- paste0(path, ".dta")
  if (!file.exists(path)) {
    cli::cli_abort(c("File not found: {.file {path}} (block {k}).",
                     "i" = "Blocks are strate output files, with or without the {.file .dta} extension."),
                   call = NULL)
  }
  as.data.frame(haven::read_dta(path))
}

# Category text as Stata builds catvar_str (stratetab.ado:374-396): a value
# label's text (decode: "" for a value without one), a string as it is, or
# a number as string(x, "%21.0g") ("." when missing).
.st_category_text <- function(v) {
  if (is.factor(v)) {
    out <- as.character(v)
    out[is.na(out)] <- ""
    return(out)
  }
  labs <- attr(v, "labels", exact = TRUE)
  if (is.numeric(v) && !is.null(labs) && is.numeric(labs)) {
    code <- as.numeric(unclass(v))
    hit <- match(code, as.numeric(labs), incomparables = NA)
    miss <- which(is.na(code))
    if (length(miss) && anyNA(labs) && requireNamespace("haven", quietly = TRUE)) {
      hit[miss] <- match(haven::na_tag(code[miss]), haven::na_tag(as.numeric(labs)), incomparables = NA)
    }
    out <- rep("", length(code))
    out[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
    return(out)
  }
  if (is.character(v)) {
    v[is.na(v)] <- ""
    return(v)
  }
  if (is.numeric(v) || is.logical(v)) return(stata_fmt(as.numeric(unclass(v)), "%21.0g"))
  out <- as.character(v)
  out[is.na(out)] <- ""
  out
}

# Every block in Stata's order (exposure outer, outcome inner), aligned per
# exposure to the first outcome's categories (stratetab.ado:419-459), with
# the numbers as Stata's local macros hold them. Each block's level is
# checked before its categories, as Stata does (:324-356 before :358-459;
# Phase 7b review F12).
.st_read_blocks <- function(blocks, outcomes, n_exp, pyscale, ratescale, level_pct) {
  cats <- vector("list", n_exp)
  cell <- list()
  levels <- numeric()
  pers <- numeric()
  samples <- list()
  k <- 0L
  for (e in seq_len(n_exp)) {
    for (o in seq_len(outcomes)) {
      k <- k + 1L
      rb <- .st_read_block(blocks[[k]], k)
      samples[k] <- list(attr(rb$s, "sample_accounting", exact = TRUE))
      levels[k] <- rb$level
      pers[k] <- rb$per
      .st_check_block_level(levels, k, level_pct)
      bc <- .st_block_cats(rb$s, k)
      b <- rb$num
      if (o == 1L) {
        cats[[e]] <- bc
        idx <- seq_along(bc)
      } else {
        if (length(bc) != length(cats[[e]])) {
          cli::cli_abort(c("Category count mismatch for exposure {e}: outcome 1 has {length(cats[[e]])} categor{?y/ies} but outcome {o} has {length(bc)}.",
                           "i" = "All outcome blocks for the same exposure must have identical categories."),
                         call = NULL)
        }
        idx <- match(cats[[e]], bc)
        if (anyNA(idx)) {
          cli::cli_abort(c("Category label mismatch for exposure {e}, outcome {o} (block {k}).",
                           "x" = "Expected category {.val {cats[[e]][is.na(idx)][1]}} from outcome 1."),
                         call = NULL)
        }
      }
      cell[[paste(o, e)]] <- list(
        D = stata_macro_num(b$D[idx]),
        Y = stata_macro_num(b$Y[idx] / pyscale),
        Rate = stata_macro_num(b$Rate[idx] * ratescale),
        Lower = stata_macro_num(b$Lower[idx] * ratescale),
        Upper = stata_macro_num(b$Upper[idx] * ratescale)
      )
    }
  }
  if (!sum(lengths(cats))) {
    cli::cli_abort("The rate blocks contain no observations.", class = "tabtools_error_rate_empty", call = NULL)
  }
  list(cats = cats, cell = cell, levels = levels, pers = pers, samples = samples)
}

# Block k's level against `level` and against the blocks before it
# (stratetab.ado:345-356).
.st_check_block_level <- function(levels, k, level_pct) {
  lv <- levels[k]
  if (is.na(lv)) return(invisible())
  if (!is.na(level_pct) && abs(level_pct - lv) > 1e-8) {
    cli::cli_abort("{.arg level} ({stata_macro_text(level_pct)}) conflicts with block {k}'s {stata_macro_text(lv)}% intervals.",
                   call = NULL)
  }
  known <- levels[seq_len(k - 1L)]
  known <- known[!is.na(known)]
  if (length(known) && abs(known[1] - lv) > 1e-8) {
    cli::cli_abort("The blocks contain mixed confidence levels ({stata_macro_text(unique(c(known, lv)))}%).",
                   call = NULL)
  }
  invisible()
}

# tt_rates(per = ) blocks (Phase 7b review F8): their rates are already per
# `per` time units and their person-time in units of `per`, and stratetab
# scales the stored values as Stata does. With the default ratescale that
# would show rates `per` times too high under "Per 1,000 PY", so the scale
# must be stated; blocks computed with different `per` are refused.
.st_check_pers <- function(pers, ratescale_given) {
  up <- unique(pers)
  if (length(up) > 1L) {
    cli::cli_abort(c("The blocks were computed with different {.arg per} ({up}) in {.fn tt_rates}.",
                     "i" = "Compute every block with the same {.arg per}."), call = NULL)
  }
  if (up != 1 && !ratescale_given) {
    cli::cli_abort(c("The blocks were computed with {.code per = {up}} in {.fn tt_rates}: their rates are already per {up} time units and their person-time in units of {up}.",
                     "x" = "The default {.code ratescale = 1000} would multiply the rates again.",
                     "i" = "State the scales: {.code ratescale = {1000 / up}, pyscale = {1 / up}} gives rates per 1,000 time units, as blocks computed with {.code per = 1} would; or recompute the blocks with {.code per = 1}."),
                   call = NULL)
  }
  invisible()
}

# Blocks from tt_rates(per = k), k != 1, hold rates per k time units. The
# rate shown is then per k * ratescale time units, read either in the raw
# unit or, when pyscale converts the time unit (days to years), per
# k * ratescale / pyscale; a numeric unitlabel matching neither shows
# correct-looking rates on the wrong scale (per = 1000 with ratescale = 1000
# is per million under "Per 1,000 PY"; Muse audit P1-25). A warning, since
# only the user knows the time unit. Blocks without a known per are not
# checked (unitlabel is the user's to keep in step, as in Stata).
.st_check_scales <- function(pers, pyscale, ratescale, unitlabel) {
  per <- unique(pers[!is.na(pers)])
  if (length(per) != 1L || per == 1) return(invisible())
  u <- suppressWarnings(as.numeric(gsub("[ ,]", "", unitlabel)))
  if (length(u) != 1L || !is.finite(u) || u <= 0) return(invisible())
  cand <- c(per * ratescale, per * ratescale / pyscale)
  if (all(abs(cand / u - 1) > 1e-8)) {
    cli::cli_warn(c("The blocks hold rates per {per} time units ({.code tt_rates(per = {per})}); with {.code ratescale = {ratescale}} the rates shown are per {signif(per * ratescale, 6)} time units, not per {unitlabel} as the header says.",
                    "i" = "For rates per 1,000 time units from these blocks use {.code ratescale = {1000 / per}, pyscale = {1 / per}}, or set {.arg unitlabel}."),
                  class = "tabtools_warning_rate_scale", call = NULL)
  }
  invisible()
}

# Confidence-level provenance (stratetab.ado:324-356, :464-481): blocks that
# record a level must agree with each other and with `level`; a block
# without one requires `level`. Percent.
.st_resolve_level <- function(levels, level_pct) {
  given <- !is.na(level_pct)
  known <- levels[!is.na(levels)]
  if (anyNA(levels) && !given) {
    if (length(known)) {
      cli::cli_abort(c("Some blocks carry no confidence-level provenance; specify {.arg level} explicitly.",
                       "i" = "Block{?s} {which(is.na(levels))} {?has/have} no level."), call = NULL)
    }
    cli::cli_abort(c("The blocks carry no confidence-level provenance in their interval labels.",
                     "i" = "Specify {.arg level}; the level is not recoverable and 95% is not assumed."),
                   call = NULL)
  }
  if (length(known)) known[1] else level_pct
}

# Rate ratios against exposure 1 (stratetab.ado:485-523): the category of
# the same label in exposure 1; log-normal interval with independent rates.
# A missing count passes Stata's `> 0` test (missing is larger than any
# number) and propagates.
.st_ratios <- function(data, outcomes, n_exp, z) {
  gt0 <- function(v) is.na(v) | v > 0
  out <- list()
  for (e in seq.int(2L, n_exp)) {
    ref <- match(data$cats[[e]], data$cats[[1]])
    dup_ref <- vapply(data$cats[[e]], function(cc) sum(data$cats[[1]] == cc), 0L)
    bad <- which(is.na(ref) | dup_ref != 1L)
    if (length(bad)) {
      cli::cli_abort(c("{.arg rateratio} requires exposure {e} categories to match exposure 1.",
                       "x" = "No unique match for category {.val {data$cats[[e]][bad[1]]}} in exposure 1."),
                     call = NULL)
    }
    for (o in seq_len(outcomes)) {
      a <- data$cell[[paste(o, 1L)]]
      b <- data$cell[[paste(o, e)]]
      d_ref <- a$D[ref]
      d_exp <- b$D
      r_ref <- a$Rate[ref]
      r_exp <- b$Rate
      ok <- gt0(d_ref) & gt0(d_exp) & gt0(r_ref)
      est <- stata_macro_num(r_exp / r_ref)
      se <- stata_macro_num(sqrt(1 / d_exp + 1 / d_ref))
      lo <- stata_macro_num(exp(log(est) - z * se))
      hi <- stata_macro_num(exp(log(est) + z * se))
      est[!ok] <- NA_real_
      lo[!ok] <- NA_real_
      hi[!ok] <- NA_real_
      out[[paste(o, e)]] <- list(est = est, lo = lo, hi = hi)
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Table

# Rate and ratio cells round at the exact unit 10^(-d): since tabtools
# 2.1.14 stratetab holds the unit in a macro first (`local _unit =
# 10^(-`digits')`, stratetab.ado:594), whose text reads back as the
# correctly rounded double, as regtab does (regtab.ado:321). R's 10^(-d)
# is that double for d = 0..10 (qa/stata/make_stratetab_round.do). Up to
# 2.1.13 the unit was Stata's inline 10^(-d), one ulp above 0.01 at d = 2,
# so round(422.395, .) gave 422.39 where regtab gives 422.40 (golden S08).
.st_fmt_fixed <- function(v, d) {
  trimws(stata_fmt(stata_round(v, 10^(-d)), paste0("%11.", d, "f")))
}

.st_fmt_events <- function(v, d) trimws(stata_fmt(v, paste0("%24.", d, "fc")))

.st_fmt_py <- function(v, d) {
  if (d == 0L) trimws(stata_fmt(stata_round(v, 1), "%24.0fc")) else trimws(stata_fmt(v, paste0("%24.", d, "fc")))
}

.st_fmt_ci <- function(est, lo, hi, d) {
  paste0(.st_fmt_fixed(est, d), " (", .st_fmt_fixed(lo, d), ", ", .st_fmt_fixed(hi, d), ")")
}

# A rate without bounds (strate gives none for zero events) shows the en
# dash the IRR column uses for a missing estimate, "0.0 (\u2013)"
# (stratetab.ado:640-650, tabtools 2.1.14; 2.1.13 printed "0.0 (., .)").
.st_fmt_rate <- function(est, lo, hi, d) {
  out <- .st_fmt_ci(est, lo, hi, d)
  nob <- is.na(lo) | is.na(hi)
  out[nob] <- paste0(.st_fmt_fixed(est[nob], d), " (\u2013)")
  out
}

# Matrix row/column names (stratetab.ado:245-264, :692-771): strtoname(),
# cut to 32 characters, a fallback for a name of underscores only, an
# optional exposure prefix, and "_2", "_3", ... suffixes for repeats.
.st_matrix_names <- function(labels, fallback, prefix = NULL) {
  used <- character()
  out <- character(length(labels))
  for (i in seq_along(labels)) {
    nm <- substr(.stata_strtoname(labels[i]), 1L, 32L)
    if (!nzchar(nm) || !nzchar(trimws(gsub("_", "", nm, fixed = TRUE)))) nm <- fallback[i]
    if (!is.null(prefix)) nm <- substr(paste0(prefix[i], "_", nm), 1L, 32L)
    base <- nm
    j <- 1L
    while (nm %in% used) {
      j <- j + 1L
      suffix <- paste0("_", j)
      nm <- paste0(substr(base, 1L, 32L - nchar(suffix)), suffix)
    }
    used <- c(used, nm)
    out[i] <- nm
  }
  out
}

.st_build <- function(data, irr, outcomes, n_exp, outlabels, ids, explabels, ci_level, ci_txt,
                      unitlabel, digits, eventdigits, pydigits, ratiodigits, rateratio,
                      title, footnote, style, sheet) {
  cpo <- if (rateratio) 4L else 3L
  nc <- 1L + outcomes * cpo
  starts <- 2L + (seq_len(outcomes) - 1L) * cpo
  h1 <- rep("", nc)
  h1[1] <- "Exposure"
  h1[starts] <- outlabels
  sub <- c("Events", "Person-Years (PY)", paste0("Per ", unitlabel, " PY (", ci_txt, "% CI)"),
           if (rateratio) paste0("IRR (", ci_txt, "% CI)"))
  h2 <- c("Exposure", rep(sub, outcomes))
  spans <- data.frame(from = starts, to = starts + cpo - 1L)

  body <- list()
  rtype <- character()
  rblock <- integer()
  rvar <- character()
  rlevel <- character()
  long <- list()
  for (e in seq_len(n_exp)) {
    body[[length(body) + 1L]] <- c(explabels[e], rep("", nc - 1L))
    rtype <- c(rtype, "var")
    rblock <- c(rblock, e)
    rvar <- c(rvar, explabels[e])
    rlevel <- c(rlevel, NA_character_)
    cats <- data$cats[[e]]
    n <- length(cats)
    if (!n) next
    m <- matrix("", n, nc)
    m[, 1] <- paste0("   ", cats)
    for (o in seq_len(outcomes)) {
      cl <- data$cell[[paste(o, e)]]
      j <- starts[o]
      m[, j] <- .st_fmt_events(cl$D, eventdigits)
      m[, j + 1L] <- .st_fmt_py(cl$Y, pydigits)
      m[, j + 2L] <- .st_fmt_rate(cl$Rate, cl$Lower, cl$Upper, digits)
      r <- if (rateratio && e > 1L) irr[[paste(o, e)]] else NULL
      if (rateratio) {
        m[, j + 3L] <- if (e == 1L) "Ref." else
          ifelse(is.na(r$est), "\u2013", .st_fmt_ci(r$est, r$lo, r$hi, ratiodigits))
      }
      na <- rep(NA_real_, n)
      long[[length(long) + 1L]] <- data.frame(
        row = length(body) + seq_len(n), exposure = e, exposure_label = explabels[e],
        category = cats, outcome = o, outcome_id = ids[o], outcome_label = outlabels[o],
        events = cl$D, person_years = cl$Y, rate = cl$Rate, lower = cl$Lower, upper = cl$Upper,
        irr = if (is.null(r)) na else r$est, irr_lower = if (is.null(r)) na else r$lo,
        irr_upper = if (is.null(r)) na else r$hi, stringsAsFactors = FALSE)
    }
    for (i in seq_len(n)) body[[length(body) + 1L]] <- m[i, ]
    rtype <- c(rtype, rep("level", n))
    rblock <- c(rblock, rep(e, n))
    rvar <- c(rvar, rep(explabels[e], n))
    rlevel <- c(rlevel, cats)
  }
  body <- do.call(rbind, body)
  rows <- data.frame(type = rtype, indent = ifelse(rtype == "level", 3L, 0L), block = rblock,
                     var = rvar, level = rlevel, stringsAsFactors = FALSE)
  role <- c("label", rep(c("value", "value", "est_ci", if (rateratio) "est_ci"), outcomes))
  cols <- data.frame(role = role, model = c(NA_integer_, rep(seq_len(outcomes), each = cpo)),
                     console_width = NA_integer_, stringsAsFactors = FALSE)
  long <- if (length(long)) do.call(rbind, long) else NULL
  if (!is.null(long)) {
    long <- long[order(long$row, long$outcome), , drop = FALSE]
    rownames(long) <- NULL
  }

  # Stored results (stratetab.ado:690-798).
  all_cats <- unlist(data$cats)
  ex_idx <- rep(seq_len(n_exp), lengths(data$cats))
  cnames <- .st_matrix_names(ids, paste0("outcome", seq_len(outcomes)))
  stored <- list(N_rows = 3 + nrow(body), N_exposures = n_exp, N_outcomes = outcomes,
                 ci_level = ci_level, outcome_ids = paste(ids, collapse = " \\ "),
                 methods = paste0("Incidence rates and confidence intervals were formatted at the ", ci_txt,
                                  "% level; rate-ratio intervals use an independent-rate log-normal approximation at the same level."))
  if (length(all_cats)) {
    rates <- do.call(rbind, lapply(seq_len(n_exp), function(e) {
      vapply(seq_len(outcomes), function(o) data$cell[[paste(o, e)]]$Rate, numeric(length(data$cats[[e]])))
    }))
    rates <- matrix(rates, length(all_cats), outcomes)
    dimnames(rates) <- list(.st_matrix_names(all_cats, paste0("row", seq_along(all_cats)),
                                             if (n_exp > 1L) paste0("e", ex_idx)), cnames)
    stored$rates <- rates
  }
  if (rateratio) {
    keep <- ex_idx > 1L
    rc <- all_cats[keep]
    if (length(rc)) {
      ratios <- do.call(rbind, lapply(seq.int(2L, n_exp), function(e) {
        vapply(seq_len(outcomes), function(o) irr[[paste(o, e)]]$est, numeric(length(data$cats[[e]])))
      }))
      ratios <- matrix(ratios, length(rc), outcomes)
      dimnames(ratios) <- list(.st_matrix_names(rc, paste0("row", seq_along(rc)),
                                                if (n_exp > 2L) paste0("e", ex_idx[keep])), cnames)
      stored$ratios <- ratios
    }
  }

  # The frame characteristics a composite reads (stratetab.ado:676-688).
  # A rate-ratio table names its fourth column (tabtools 2.1.14,
  # stratetab.ado:733-741), so comptab refuses it by name.
  frame <- list(source = "stratetab", ci_level = ci_level,
                statistic_ids = if (rateratio) "events person_years rate_ci irr_ci" else "events person_years rate_ci",
                n_outcomes = outcomes)
  for (o in seq_len(outcomes)) frame[[paste0("outcome_id_", o)]] <- ids[o]
  frame$outcome_id <- ids
  frame$outcome_label <- outlabels
  # Stata's frame holds the title in row 1 (variable `title`), which comptab
  # uses as its default title (comptab.ado:352-357, :1245); the column
  # layout says whether each outcome has an IRR column, which the width
  # alone cannot (a 3-outcome rateratio table and a 4-outcome plain one
  # both have 13 columns; Phase 7b review F6).
  frame$title <- title
  frame$footnote <- footnote
  frame$rateratio <- rateratio
  frame$cols_per_outcome <- cpo

  layout <- list(indent = 3L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "plain", xlsx_rules = "stratetab",
                 sheet = "Results", md_header = "joined")
  tt_table(body = body, header = list(list(text = h1, spans = spans), list(text = h2)),
           rows = rows, cols = cols, title = title, footnote = footnote, style = style,
           stored = stored, command = "stratetab", layout = layout,
           meta = list(sheet = sheet, frame = frame, rate_rows = long, cols_per_outcome = cpo))
}

# ---------------------------------------------------------------------------
# Workbook layout (stratetab.ado:800-924)

.xlsx_layout_stratetab <- function(x) {
  if (length(x$header) != 2L) .tt_layout_needs("stratetab", "exactly two header rows")
  cells <- .tt_cells(x)
  nb <- nrow(x$body)
  nc <- ncol(x$body)
  total <- nc + 1L
  last <- nb + 3L
  foot <- nzchar(x$footnote)
  nr <- last + as.integer(foot)
  cpo <- x$meta$cols_per_outcome %||% 3L
  outcomes <- (nc - 1L) %/% cpo
  style <- x$style
  grid <- matrix("", nr, total)
  grid[1, 1] <- x$title
  grid[2:last, -1] <- cells
  # The title column and c1..cK are written as one block (put_string of
  # the whole dataset, _tabtools_xlsx_write.ado); the footnote on its own.
  written <- matrix(FALSE, nr, total)
  written[seq_len(last), ] <- TRUE
  if (foot) {
    grid[nr, 2] <- x$footnote
    written[nr, 2] <- TRUE
  }
  hb <- .border_code(style$hborder)
  vb <- .border_code(style$borderstyle)
  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  add("height", 1, 1, 1, 1, value = 30)
  # Widths: 1 and 18, then max(8, length of the row-3 header + 2).
  add("width", 1, 1, 1, 1, value = 1)
  add("width", 1, 1, 2, 2, value = 18)
  for (j in seq.int(3L, length.out = total - 2L)) {
    add("width", 1, 1, j, j, value = max(8, .blen(grid[3, j]) + 2))
  }
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
    end <- col + cpo - 1L
    add("merge", 2, 2, col, end)
    add("bold", 2, 2, col, col, code = 1)
    add("halign", 2, 2, col, col, code = 2)
    add("valign", 2, 2, col, col, code = 3)
    add("bottom", 2, 2, col, end, code = hb)
    col <- col + cpo
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
      end <- col + cpo - 1L
      add("right", 2, last, end, end, code = vb)
      col <- col + cpo
    }
  }
  # A rule above each exposure block but the first, at the exposure header
  # rows recorded while the table was built (stratetab.ado:599-605, since
  # tabtools 2.1.14; 2.1.13 found them from the cell text and skipped a
  # block labelled "Exposure").
  for (r in which(x$rows$type == "var") + 3L) {
    if (r - 1L > 3L) add("bottom", r - 1L, r - 1L, 2, total, code = hb)
  }
  add("bottom", last, last, 2, total, code = hb)
  if (foot) {
    add("merge", nr, nr, 2, total)
    add("halign", nr, nr, 2, 2, code = 1)
    add("valign", nr, nr, 2, 2, code = 2)
    add("wrap", nr, nr, 2, 2, code = 1)
    add("font", nr, nr, 2, 2, value = max(style$fontsize - 2, 6))
    add("italic", nr, nr, 2, 2, code = 1)
  }
  list(grid = grid, written = written, rules = do.call(rbind, R))
}
