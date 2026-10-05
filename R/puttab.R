# puttab (task 7.1): style a table that already exists in memory -- a data
# frame, a numeric matrix, or a tt_table -- as one house-styled sheet, plus
# the CSV and Markdown mirrors. Port of Stata tabtools 2.1.12 puttab.ado.
#
# The table is built as Stata builds its string dataset c1..cK
# (puttab.ado:257-353, the Mata helpers at :604-861): an optional header row
# (variable names, variable labels under `varlabels`, or matrix column
# names), then one row per observation or matrix row. The workbook layout is
# puttab's own rule list (:396-522): title in A1 merged across, a 1-wide
# gutter column A, the table from B2, a bold header rule, centred value
# columns, zebra striping from the second data row, and an italic footnote.

#' Style an in-memory table as one house-styled sheet
#'
#' Turns a table that already exists -- a data frame, a numeric matrix
#' (such as a coefficient table), or a `tt_table` -- into a tabtools table
#' that prints and can be written as one sheet with the tabtools geometry
#' (title in `A1`, the table from `B2`, a header rule, optional header
#' shading and zebra striping, column widths, borders, and an italic
#' footnote). Several such
#' sheets can then be assembled into one composite with [stacktab()]. R
#' implementation of the Stata `puttab` command.
#'
#' @section Sources:
#' * **Data frame** (Stata's varlist, `frame()`, `if`, and `in`): one column
#'   per variable (`vars` picks and orders them) and one row per
#'   observation (`subset` keeps some). Character columns are written as
#'   they are (leading spaces included); factors and labelled values
#'   (`haven::labelled()`) show their labels, including a label on an
#'   extended missing value (`.a`, read by haven as a tagged `NA`; a plain
#'   `NA` stays blank). `Date` and `POSIXct` columns use Stata's display
#'   formats: the column's own Stata format when haven kept one (the
#'   `"format.stata"` attribute, e.g. `%tdCCYY-NN-DD`; the common codes
#'   `CC YY NN DD Mon Month HH MM SS ...` are rendered, any other format
#'   falls back to the default), else `%td` (`06jan2020`) and `%tc`
#'   (`05jan2020 10:30:00`, in the column's time zone). Logical columns are
#'   written as 1/0 (Stata has no logicals). Other numeric columns show
#'   whole numbers when every value is whole, otherwise `digits` decimals; a
#'   value that rounds to zero is written without a minus sign and a
#'   missing value as a blank cell. List, matrix, and data-frame columns
#'   are refused. The header is the column name, or its `"label"` attribute
#'   under `varlabels`. When the header comes from the labels and the first
#'   row merely repeats them (a table that was already header-shaped), that
#'   row is dropped rather than shown twice (Stata's rule: at least two
#'   rows, every exported column character, every first-row cell equal to
#'   its column's label, the first column optionally blank).
#' * **Matrix** (Stata's `matrix()`): the row names become the first
#'   (label) column and the column names the header; the label column's
#'   header is blank. Names of the form `eq:name` are kept as they are
#'   (Stata's equation stripes) and a leading `_:` (Stata's empty equation)
#'   is dropped; unnamed rows and columns are `r1`, `r2`, ... and `c1`,
#'   `c2`, ..., as in Stata. Each column is formatted on its own, as above
#'   (`digits` applies; `varlabels` does not).
#' * **tt_table** (R only): the table's cells as its CSV sink shows them,
#'   under one header row flattened as in its Markdown sink
#'   (`"Treated (N=1,234)"`, `"Model 1: OR"`), or none when the table has
#'   none; `title` and `footnote` default to the table's own. Its cells are
#'   already text, so `digits` and `varlabels` do not apply.
#'
#' @param x A data frame, a numeric matrix, or a `tt_table`.
#' @param vars Data frames only: character vector of the columns to export,
#'   in this order (Stata's varlist). Default: every column.
#' @param subset Data frames only, Stata's `if` and `in`: a logical vector
#'   with one value per row of `x` (`NA` counts as `FALSE`), or the row
#'   numbers to keep. Both refer to the rows of `x` before a repeated-label
#'   header row (see Sources) is dropped, as in Stata tabtools 2.1.14:
#'   `subset = 1:2` is Stata's `in 1/2`, the header
#'   row and the first data row, and that row is looked at only when it is
#'   selected.
#' @param xlsx Output `.xlsx` workbook (Stata's `using`). The sheet is
#'   replaced if it exists (matched without regard to case) and added
#'   otherwise; the workbook is created if needed.
#' @param sheet Sheet name (default `"Table"`).
#' @param title Title in cell `A1` (and the first row of the CSV and the
#'   `###` heading of the Markdown file).
#' @param footnote Italic footnote below the table.
#' @param font,fontsize Font family and size (defaults from
#'   [tabtools_options()], else Arial 10).
#' @param borderstyle `"default"` or `"thin"`, `"medium"`, or `"academic"`
#'   (medium rules). puttab draws horizontal rules only.
#' @param headercolor,zebracolor Colours for `headershade` and `zebra`.
#' @param zebra Shade every second data row, starting with the second.
#' @param headershade Shade the header row.
#' @param digits Decimal places for numeric columns that are not all whole
#'   numbers, 0 to 6 (default from [tabtools_options()], else 2).
#' @param varlabels Use variable labels (the `"label"` attribute) as the
#'   header; a column without one keeps its name.
#' @param noheader Write no header row.
#' @param noembedheader Export a first row that repeats the variable labels
#'   as data (Stata's `noembedheader`): the header is still built from the
#'   labels under `varlabels`, but the row is not taken for the header.
#' @param csv Also write the table to this `.csv` file.
#' @param markdown Also write the table to this Markdown file (`.md`,
#'   `.markdown`, `.qmd`, or `.rmd`).
#' @param mdappend Append to an existing `markdown` file.
#' @param open Open the workbook after writing (interactive sessions).
#' @return A `tt_table` (`command = "puttab"`), returned invisibly when
#'   `xlsx`, `csv` or `markdown` writes a file (assign it and print it to see
#'   it). `$stored` holds Stata's `r()` results: `n_rows` (title,
#'   header, data, and footnote rows), `n_cols`, `n_datarows`, `source`
#'   (`"data"`, `"matrix"`, or `"table"`), and, for the files written,
#'   `sheet`, `file`, `csv`, `markdown`, `markdown_rows`, and
#'   `markdown_cols`.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()]; unavailable record counts remain explicit.
#' @section Differences from Stata:
#' * No output file is required: without `xlsx` and `markdown` (Stata
#'   needs one of them), the table is returned for printing, further
#'   writing ([tt_write_xlsx()]), or [stacktab()].
#' * R has no frames: a data frame stands for both Stata's varlist of the
#'   current data and its `frame()` source, and `source` is `"data"` for
#'   either (Stata's `frame()` gives `"frame"`). Stata's `puttab: wrote ...`
#'   line (with `xlsx`) and `Markdown exported to ...` line (with
#'   `markdown`) are messages.
#' * A `Date` or `POSIXct` column whose Stata format uses a code R does
#'   not render (or an R column without a Stata format) shows the default
#'   `%td`/`%tc` display.
#' @seealso [stacktab()] to assemble puttab sheets into one composite.
#' @examples
#' # A matrix: the row names become the label column
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' m <- cbind(b = coef(fit), se = sqrt(diag(vcov(fit))))
#' puttab(m, title = "OLS coefficients", digits = 3)
#'
#' # A data frame, variable labels as the header, written to a workbook
#' d <- data.frame(term = c("Any HRT", "Former smoker"),
#'                 ahr = c("0.82", "1.14"), ci = c("(0.69, 0.98)", "(0.97, 1.34)"))
#' attr(d$term, "label") <- "Exposure"
#' attr(d$ahr, "label") <- "aHR"
#' attr(d$ci, "label") <- "95% CI"
#' path <- tempfile(fileext = ".xlsx")
#' puttab(d, xlsx = path, sheet = "Block", varlabels = TRUE,
#'        title = "Primary model", zebra = TRUE)
#' @export
puttab <- function(x, vars = NULL, subset = NULL, xlsx = NULL, sheet = "Table",
                   title = NULL, footnote = NULL, font = NULL, fontsize = NULL,
                   borderstyle = NULL, headercolor = NULL, zebracolor = NULL,
                   zebra = FALSE, headershade = FALSE, digits = NULL,
                   varlabels = FALSE, noheader = FALSE, noembedheader = FALSE, csv = NULL,
                   markdown = NULL, mdappend = FALSE, open = FALSE) {
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Table"
  for (a in c("zebra", "headershade", "varlabels", "noheader", "noembedheader", "mdappend", "open")) {
    v <- get(a)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
  }
  has_xlsx <- !is.null(xlsx)
  .tt_check_sheet_xlsx(sheet_given, has_xlsx)
  has_md <- !is.null(markdown)
  # puttab.ado:147-189, in Stata's order.
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || is.na(xlsx) || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must specify a .xlsx file.", call = NULL)
  }
  sheet <- .check_sheet(sheet)
  if (!is.null(csv)) .tt_check_csv_path(csv)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L || is.na(markdown) ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  digits <- .check_int_range(digits %||% getOption("tabtools.digits") %||% 2L, "digits", 0, 6)
  for (a in c("title", "footnote")) .tt_check_text_arg(get(a), a)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra,
                            headercolor = headercolor, zebracolor = zebracolor)
  src <- .puttab_source(x, vars = vars, subset = subset, digits = digits,
                        varlabels = varlabels, noheader = noheader, noembedheader = noembedheader)
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  title <- title %||% src$title %||% ""
  footnote <- footnote %||% src$footnote %||% ""
  tt <- .puttab_table(src$header, src$body, title = title, footnote = footnote, style = style,
                      sheet = sheet, source = src$source, sample = src$sample)

  # Sinks in Stata's order: CSV, Markdown, then the workbook (puttab.ado:363-553).
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    tt$stored$csv <- csv
  }
  if (has_md) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    message("Markdown exported to ", markdown)
  }
  if (has_xlsx) {
    # Excel matches an existing sheet without regard to case; report the
    # spelling actually in the workbook (puttab.ado:528-530).
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$sheet <- sheet
    tt$stored$file <- xlsx
    tt$meta$sheet <- sheet
    message(sprintf("puttab: wrote %d data rows x %d cols (%s source) to sheet %s in %s",
                    nrow(tt$body), ncol(tt$body), src$source, sheet, xlsx))
  }
  if (has_xlsx || has_md || !is.null(csv)) invisible(tt) else tt
}

# The spelling of `sheet` already in the workbook at `path` (Excel matches
# sheet names without regard to case), or `sheet` itself.
.xlsx_existing_sheet <- function(path, sheet) {
  if (!file.exists(path)) return(sheet)
  # Loaded first: an unreadable workbook passed as a promise made
  # openxlsx2 re-evaluate it ("restarting interrupted promise evaluation");
  # the writer then reports the file (Codex audit CX-1 probe).
  wb <- tryCatch(openxlsx2::wb_load(path), error = function(e) NULL)
  sheets <- if (is.null(wb)) character() else openxlsx2::wb_get_sheet_names(wb)
  hit <- which(tolower(sheets) == tolower(sheet))
  if (length(hit)) unname(sheets[hit[1]]) else sheet
}

# A puttab tt_table from header text (NULL for none) and a character body.
.puttab_table <- function(header, body, title, footnote, style, sheet = "Table", source = "data",
                          command = "puttab", sample = NULL) {
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  K <- ncol(body)
  hdr <- if (is.null(header)) list() else list(list(text = header))
  cols <- data.frame(role = c("label", rep("value", K - 1L)), model = NA_integer_,
                     console_width = NA_integer_, stringsAsFactors = FALSE)
  rows <- data.frame(type = rep("var", nrow(body)), indent = rep(0L, nrow(body)))
  layout <- list(indent = 0L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, header_style = "plain", xlsx_rules = "puttab",
                 sheet = "Table", csv_reservedrow = FALSE)
  nr <- nzchar(title) + length(hdr) + nrow(body) + nzchar(footnote)
  stored <- list(n_rows = nr, n_cols = K, n_datarows = nrow(body), source = source)
  tt_table(body = body, header = hdr, rows = rows, cols = cols, title = title,
           footnote = footnote, style = style, stored = stored, command = command,
           layout = layout, meta = list(sheet = sheet,
             sample_accounting = .tt_sample_bind(list(sample), prefixes = "source",
                                                  commands = command)))
}

# ---------------------------------------------------------------------------
# Sources

.puttab_source <- function(x, vars, subset, digits, varlabels, noheader, noembedheader = FALSE) {
  if (inherits(x, "tt_table")) {
    if (!is.null(vars) || !is.null(subset)) {
      cli::cli_abort("{.arg vars} and {.arg subset} apply to a data frame source only.", call = NULL)
    }
    return(.puttab_from_tt(x, noheader))
  }
  if (is.matrix(x)) {
    # puttab.ado:220-228: the matrix is the whole source.
    if (!is.null(vars)) cli::cli_abort("{.arg vars} is not allowed with a matrix source; the matrix is the source.", call = NULL)
    if (!is.null(subset)) cli::cli_abort("{.arg subset} is not allowed with a matrix source.", call = NULL)
    return(.puttab_from_matrix(x, digits, noheader))
  }
  if (is.data.frame(x)) return(.puttab_from_data(x, vars, subset, digits, varlabels, noheader, noembedheader))
  cli::cli_abort("{.arg x} must be a data frame, a numeric matrix, or a {.cls tt_table}, not {.obj_type_friendly {x}}.",
                 call = NULL)
}

.puttab_from_tt <- function(x, noheader) {
  x <- .tt_blank_text(validate_tt_table(x))
  g <- .tt_cells(x, extraspace = TRUE)
  body <- if (length(x$header)) g[-seq_len(.md_nhead(x)), , drop = FALSE] else g
  if (!nrow(body)) cli::cli_abort("The table has no body rows to export.", call = NULL)
  # A table without a header row keeps none (review F5).
  header <- if (noheader || !length(x$header)) NULL else .md_header(x, escape = FALSE)
  list(header = header, body = body, source = "table", title = x$title, footnote = x$footnote,
       sample = x$meta[["sample_accounting", exact = TRUE]])
}

.puttab_from_matrix <- function(x, digits, noheader) {
  if (!is.numeric(x) && !is.logical(x)) {
    cli::cli_abort("A matrix source must be numeric, not {.cls {typeof(x)}}.", call = NULL)
  }
  if (!nrow(x) || !ncol(x)) cli::cli_abort("The matrix source is empty.", call = NULL)
  rn <- .puttab_stripe(rownames(x), "r", nrow(x))
  cn <- .puttab_stripe(colnames(x), "c", ncol(x))
  vals <- vapply(seq_len(ncol(x)), function(j) .puttab_fmt_num(as.numeric(x[, j]), digits),
                 character(nrow(x)))
  # vapply() returns a vector for a one-row matrix.
  body <- cbind(rn, matrix(vals, nrow(x)))
  dimnames(body) <- NULL
  header <- if (noheader) NULL else c("", cn)
  list(header = header, body = body, source = "matrix")
}

# Matrix stripe names (_puttab_stripe_names, puttab.ado:674-693): "eq:name",
# or "name" when the equation is empty or "_". R has no stripes, so names
# are taken as written, with Stata's placeholder equation "_:" dropped;
# missing names are Stata's defaults r1.. / c1...
.puttab_stripe <- function(nm, prefix, n) {
  if (is.null(nm)) return(paste0(prefix, seq_len(n)))
  nm[is.na(nm)] <- ""
  sub("^_:", "", nm)
}

.puttab_from_data <- function(x, vars, subset, digits, varlabels, noheader, noembedheader = FALSE) {
  if (!is.null(vars)) {
    if (!is.character(vars) || !length(vars) || anyNA(vars)) {
      cli::cli_abort("{.arg vars} must be a character vector of column names.", call = NULL)
    }
    bad <- setdiff(vars, names(x))
    if (length(bad)) cli::cli_abort("{.arg vars}: column{?s} {.var {bad}} not found in {.arg x}.", call = NULL)
    if (anyDuplicated(vars)) cli::cli_abort("{.arg vars} names {.var {unique(vars[duplicated(vars)])}} more than once.", call = NULL)
    # Codex audit CX-3: a name the data frame holds twice does not say
    # which column is meant.
    .tt_check_unambiguous(x, vars, "vars")
  }
  if (!ncol(x)) cli::cli_abort("The source contains no variables to export.", call = NULL)
  # Columns by position (Codex audit CX-3): x[[name]] returns the first of
  # two columns of one name, so a data frame with duplicate names exported
  # its first column twice and lost the second.
  idx <- if (is.null(vars)) seq_along(x) else match(vars, names(x))
  use <- names(x)[idx]
  nested <- use[vapply(idx, function(j) is.matrix(x[[j]]) || is.data.frame(x[[j]]), TRUE)]
  if (length(nested)) {
    cli::cli_abort(c("Matrix and data-frame columns cannot be exported: {.var {nested}}.",
                     "i" = "Split them into ordinary columns first."), call = NULL)
  }
  # Stata's if/in select by the source's own row numbers (Stata-Tools
  # 68c37a90, puttab.ado: "before any embedded header is consumed"; task C7;
  # 2.1.12 counted the data rows after it).
  keep <- rep(TRUE, nrow(x))
  if (!is.null(subset)) {
    if (is.logical(subset)) {
      # Stata's `if`: one value per row of x.
      if (length(subset) != nrow(x)) {
        cli::cli_abort("A logical {.arg subset} needs one value per row of {.arg x} ({nrow(x)}), not {length(subset)}.",
                       call = NULL)
      }
      keep <- !is.na(subset) & subset
    } else if (is.numeric(subset)) {
      # Stata's `in`: row numbers of x.
      if (anyNA(subset) || any(subset != round(subset)) || any(subset < 1 | subset > nrow(x))) {
        cli::cli_abort("{.arg subset} row numbers must be whole numbers between 1 and {nrow(x)}.", call = NULL)
      }
      keep <- seq_len(nrow(x)) %in% subset
    } else {
      cli::cli_abort("{.arg subset} must be a logical vector or row numbers.", call = NULL)
    }
  }
  # A header-shaped source repeats its labels in row 1 (puttab.ado
  # _puttab_is_headerrow): with the header drawn from those labels, that row
  # is the header, not data, and is dropped; only when row 1 is inside the
  # selection, and never under noembedheader.
  if (!noheader && varlabels && !noembedheader && keep[1L] && .puttab_is_headerrow(x[idx])) keep[1L] <- FALSE
  if (!any(keep)) cli::cli_abort("The source contains no observations to export.", call = NULL)
  # Labels first: `[` drops a plain vector's "label" attribute.
  header <- NULL
  if (!noheader) header <- if (varlabels) vapply(seq_along(idx), function(k) var_label(x[[idx[k]]], use[k]), "") else use
  n <- sum(keep)
  # The Stata display format travels separately: `[` drops attributes.
  body <- vapply(idx, function(j) .puttab_fmt_col(x[[j]][keep], digits, attr(x[[j]], "format.stata", exact = TRUE)),
                 character(n), USE.NAMES = FALSE)
  body <- matrix(body, n)
  list(header = unname(header), body = body, source = "data",
       sample = attr(x, "sample_accounting", exact = TRUE))
}

.puttab_is_headerrow <- function(d) {
  if (nrow(d) < 2L || !ncol(d)) return(FALSE)
  matched <- 0L
  for (j in seq_along(d)) {
    v <- d[[j]]
    if (!is.character(v) || inherits(v, "haven_labelled")) return(FALSE)
    cell <- trimws(v[1] %|na|% "", whitespace = "[ ]")
    if (!nzchar(cell)) {
      if (j == 1L) next
      return(FALSE)
    }
    lbl <- attr(v, "label", exact = TRUE)
    lbl <- if (is.character(lbl) && length(lbl) == 1L && !is.na(lbl)) trimws(lbl, whitespace = "[ ]") else ""
    if (!nzchar(lbl) || cell != lbl) return(FALSE)
    matched <- matched + 1L
  }
  matched > 0L
}

# One column as display text (_puttab_fmt_num, puttab.ado:618-672).
.puttab_fmt_col <- function(v, digits, fmt = attr(v, "format.stata", exact = TRUE)) {
  if (is.list(v)) cli::cli_abort("List columns cannot be exported.", call = NULL)
  if (inherits(v, "Date") || inherits(v, "POSIXt")) return(.stata_datetime(v, fmt))
  if (is.logical(v)) return(.puttab_fmt_num(as.numeric(v), digits))
  if (is.factor(v)) {
    out <- as.character(v)
    out[is.na(out)] <- ""
    return(out)
  }
  labs <- attr(v, "labels", exact = TRUE)
  if (is.numeric(v) && !is.null(labs) && is.numeric(labs)) {
    code <- as.numeric(v)
    lab_codes <- as.numeric(labs)
    out <- .puttab_fmt_num(code, digits)
    # match() treats every NA alike; Stata labels an extended missing value
    # (.a, .b, ...) by its own code and never a system missing ".". haven
    # reads those as tagged NAs, told apart by na_tag() (review F1).
    hit <- match(code, lab_codes, incomparables = NA)
    miss <- which(is.na(code))
    if (length(miss) && anyNA(lab_codes) && requireNamespace("haven", quietly = TRUE)) {
      hit[miss] <- match(haven::na_tag(code[miss]), haven::na_tag(lab_codes), incomparables = NA)
    }
    out[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
    return(out)
  }
  if (is.numeric(v)) return(.puttab_fmt_num(as.numeric(v), digits))
  out <- as.character(v)
  out[is.na(out)] <- ""
  out
}

# Whole numbers when every non-missing value is whole, else `digits`
# decimals (Stata's %32.0f / %32.<d>f); no minus sign on a value that
# rounds to zero; missing -> blank. Stata has no infinities; R writes them
# as "Inf"/"-Inf".
.puttab_fmt_num <- function(v, digits) {
  out <- rep("", length(v))
  fin <- is.finite(v)
  allint <- all(v[fin] == floor(v[fin]))
  fmt <- if (allint) "%32.0f" else paste0("%32.", digits, "f")
  s <- trimws(stata_fmt(v[fin], fmt))
  out[fin] <- sub("^-(0(\\.0+)?)$", "\\1", s)
  inf <- !is.na(v) & is.infinite(v)
  out[inf] <- ifelse(v[inf] > 0, "Inf", "-Inf")
  out
}

# Dates and date-times in Stata's display formats: a Date as %td
# ("06jan2020"), a POSIXct as %tc ("05jan2020 10:30:00", in the object's
# own time zone), or as the column's own Stata format when it carries one
# (haven keeps it in the "format.stata" attribute, e.g. "%tdCCYY-NN-DD").
# The common format codes are rendered; a format using any other code
# falls back to the default display.
.stata_datetime <- function(v, fmt = NULL) {
  is_date <- inherits(v, "Date")
  out <- rep("", length(v))
  ok <- !is.na(v)
  if (!any(ok)) return(out)
  default <- if (is_date) "DDmonCCYY" else "DDmonCCYY_HH:MM:SS"
  spec <- default
  if (is.character(fmt) && length(fmt) == 1L && !is.na(fmt)) {
    f <- sub("^%-?", "", fmt)
    kind <- substr(f, 1L, 2L)
    body <- substring(f, 3L)
    if ((is_date && kind == "td") || (!is_date && kind %in% c("tc", "tC"))) {
      if (nzchar(body)) spec <- body
    }
  }
  toks <- .stata_dt_tokens(spec)
  if (is.null(toks)) toks <- .stata_dt_tokens(default)
  lt <- as.POSIXlt(v[ok])
  out[ok] <- .stata_dt_render(toks, lt)
  out
}

.stata_dt_codes <- c("Month", "month", ".sss", "Mon", "mon", "JJJ", "jjj", ".ss", "CC", "cc",
                     "YY", "yy", "NN", "nn", "DD", "dd", "HH", "Hh", "hH", "hh", "MM", "mm",
                     "SS", "ss", ".s", "am", "pm", "AM", "PM")

# Tokens of a Stata date/time format body, or NULL when a code is unknown.
.stata_dt_tokens <- function(spec) {
  toks <- character()
  i <- 1L
  n <- nchar(spec)
  while (i <= n) {
    hit <- NA_character_
    for (code in .stata_dt_codes) {
      if (substr(spec, i, i + nchar(code) - 1L) == code) {
        hit <- code
        break
      }
    }
    ch <- substr(spec, i, i)
    if (!is.na(hit)) {
      toks <- c(toks, hit)
      i <- i + nchar(hit)
    } else if (ch == "!" && i < n) {
      toks <- c(toks, paste0("lit:", substr(spec, i + 1L, i + 1L)))
      i <- i + 2L
    } else if (ch == "_") {
      toks <- c(toks, "lit: ")
      i <- i + 1L
    } else if (ch %in% c("-", "/", ".", ",", ":", "'")) {
      toks <- c(toks, paste0("lit:", ch))
      i <- i + 1L
    } else {
      return(NULL)
    }
  }
  toks
}

.stata_dt_render <- function(toks, lt) {
  y <- lt$year + 1900L
  mon <- lt$mon + 1L
  h <- lt$hour
  h12 <- ifelse(h %% 12L == 0L, 12L, h %% 12L)
  sec <- lt$sec
  whole <- floor(sec)
  frac <- function(k) substr(.tt_fmt_f(sec - whole, 3L), 2L, 2L + k)
  pieces <- lapply(toks, function(t) {
    switch(t,
      Month = month.name[mon], month = tolower(month.name[mon]),
      Mon = month.abb[mon], mon = tolower(month.abb[mon]),
      JJJ = sprintf("%03d", lt$yday + 1L), jjj = as.character(lt$yday + 1L),
      CC = sprintf("%02d", y %/% 100L), cc = as.character(y %/% 100L),
      YY = sprintf("%02d", y %% 100L), yy = as.character(y %% 100L),
      NN = sprintf("%02d", mon), nn = as.character(mon),
      DD = sprintf("%02d", lt$mday), dd = as.character(lt$mday),
      HH = sprintf("%02d", h), hH = as.character(h),
      Hh = sprintf("%02d", h12), hh = as.character(h12),
      MM = sprintf("%02d", lt$min), mm = as.character(lt$min),
      SS = sprintf("%02d", as.integer(whole)), ss = as.character(as.integer(whole)),
      ".s" = frac(1L), ".ss" = frac(2L), ".sss" = frac(3L),
      am = , pm = ifelse(h < 12L, "am", "pm"), AM = , PM = ifelse(h < 12L, "AM", "PM"),
      rep(substring(t, 5L), length(y)))
  })
  do.call(paste0, pieces)
}

# ---------------------------------------------------------------------------
# Workbook layout (puttab.ado:396-522)

.xlsx_layout_puttab <- function(x) {
  cells <- .tt_cells(x)
  nh <- length(x$header)
  if (nh > 1L) cli::cli_abort("A puttab layout has at most one header row.", call = NULL)
  K <- ncol(x$body)
  xK <- K + 1L
  nb <- nrow(x$body)
  style <- x$style
  hdr_row <- if (nh) 2L else 0L
  data_start <- 2L + nh
  last_data <- data_start + nb - 1L
  foot <- nzchar(x$footnote)
  total <- last_data + as.integer(foot)
  grid <- matrix("", total, xK)
  grid[1, 1] <- x$title
  if (nh + nb) grid[2:last_data, -1] <- cells
  if (foot) grid[total, 2] <- x$footnote
  # Stata writes every cell of its table, blanks included.
  written <- matrix(TRUE, total, xK)

  hb <- .border_code(style$hborder)
  R <- list()
  add <- function(...) R[[length(R) + 1L]] <<- .rule(...)
  # Spacer column A, then content widths over rows 2..last data row:
  # ceil(max byte length x 0.95) + 2, 10 for an empty column; label column
  # [12, 50], others [8, 32].
  add("width", 1, 1, 1, 1, value = 1)
  for (j in seq_len(K)) {
    col <- grid[seq.int(2L, last_data), j + 1L]
    col <- col[nzchar(col)]
    w <- if (length(col)) ceiling(max(.blen(col)) * 0.95) + 2 else 10
    w <- if (j == 1L) min(max(w, 12), 50) else min(max(w, 8), 32)
    add("width", 1, 1, j + 1L, j + 1L, value = w)
  }
  add("font", 1, total, 1, xK, value = style$fontsize)
  add("wrap", 1, total, 1, xK, code = 1)
  add("valign", 1, total, 1, xK, code = 2)
  add("halign", 1, total, 1, xK, code = 1)
  add("height", 1, 1, 1, 1, value = 30)
  add("merge", 1, 1, 1, xK)
  add("font", 1, 1, 1, xK, value = style$fontsize + 2)
  add("bold", 1, 1, 1, xK, code = 1)
  add("wrap", 1, 1, 1, 1, code = 1)
  add("halign", 1, 1, 1, xK, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  if (nh) {
    add("bold", hdr_row, hdr_row, 2, xK, code = 1)
    add("halign", hdr_row, hdr_row, 2, xK, code = 2)
    add("top", hdr_row, hdr_row, 2, xK, code = hb)
    add("bottom", hdr_row, hdr_row, 2, xK, code = hb)
    if (style$headershade) add("fill", hdr_row, hdr_row, 2, xK, color = style$headercolor)
  } else {
    add("top", data_start, data_start, 2, xK, code = hb)
  }
  if (K >= 2L && nb) add("halign", data_start, last_data, 3, xK, code = 2)
  if (nb) add("bottom", last_data, last_data, 2, xK, code = hb)
  if (style$zebra && data_start + 1L <= last_data) {
    for (r in seq.int(data_start + 1L, last_data, by = 2L)) add("fill", r, r, 2, xK, color = style$zebracolor)
  }
  if (foot) {
    # A one-column table would merge B:B, a single cell: not written, as in
    # tabtools 2.1.12 (puttab.ado:529-538).
    if (xK > 2L) add("merge", total, total, 2, xK)
    add("font", total, total, 2, xK, value = max(style$fontsize - 2, 6))
    add("italic", total, total, 2, xK, code = 1)
    add("halign", total, total, 2, xK, code = 1)
  }
  list(grid = grid, written = written, rules = do.call(rbind, R))
}
