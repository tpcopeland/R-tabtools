#' Incidence-rate tables from event counts and person-time
#'
#' Build one separate section per grouping variable, with one or more event
#' outcomes. Missingness in any event or exposure column defines a common
#' outcome sample; missing grouping values exclude records only from their
#' own section. Recurrent nonnegative integer event counts are supported.
#'
#' @param data A data frame with event, exposure, grouping and optional ID columns.
#' @param by Character vector of grouping column names. Repeated names make
#'   repeated sections. Only observed levels are printed.
#' @param events Character vector of event-count column names.
#' @param exposure One shared person-time column name or one per outcome.
#'   Both `events` and `exposure` must be supplied. R has no implicit Stata
#'   `stset` state; supply already calculated at-risk time and event counts.
#' @param per Positive rate multiplier, default 1000.
#' @param ci Interval method: `"exact"` (Poisson tail inversion), `"poisson"`
#'   (log-rate normal approximation), or `"cluster"`.
#' @param cluster One cluster-ID column name, required only for clustered CIs.
#'   Missing IDs on the common sample refuse, including zero-time records.
#' @param level Confidence level as a proportion or percentage, from 10% to 99.99%.
#' @param pyscale Positive divisor for displayed person-time. Rates and limits
#'   are multiplied by this factor exactly once.
#' @param subset Logical row selector or unique positive row indices.
#' @param fweight NULL, a column name, or nonnegative integer replication
#'   weights. Missing and zero weights exclude records. This R extension is
#'   equivalent to replication with original cluster IDs preserved.
#' @param smallcells,nosmallcells,masktext Primary-only publication masking.
#'   Counts from 1 through `smallcells-1` hide count, time, rate and CI text.
#'   `nosmallcells=TRUE` disables inherited masking. Explicit mask text wins.
#' @param excludemasked For `ci="cluster"`, omit low-count levels from fitting.
#'   Requires an effective `smallcells` threshold of at least 2.
#' @param zerocells NULL, `"dash"`, or `"blank"` to hide zero-event count/rate text.
#' @param zerocells_persontime Also hide zero-event person-time; requires `zerocells`.
#' @param cformat,digits,sep Numeric format, alternative decimal digits (default
#'   1), and literal interval separator. Explicit `digits` conflicts with
#'   `cformat`; decimal-comma formats require a comma-free separator.
#' @param pydigits Decimal digits for displayed person-time, default 0.
#' @param outlabels,outcomeids,explabels Outcome display labels, original
#'   outcome identifiers, and grouping-section display labels.
#' @param unitlabel Text in the rate header, default the formatted `per` value.
#' @param saving Optional `.rds` or `.dta` analytical long-data destination.
#'   RDS retains original grouping classes, levels, value/variable labels and
#'   collision mapping. DTA requires suggested package haven; factors are
#'   exported as labelled numeric codes. Original names colliding with fixed
#'   columns use the native `g_`, `g2_`, ..., `g99_` naming rule.
#' @param replace Allow replacement of an existing `saving` file.
#' @param xlsx,csv,markdown Optional publication destinations.
#' @param sheet Workbook sheet name. An explicitly supplied non-NULL sheet
#'   opts into session workbook/Markdown defaults; omitted sheet does not.
#' @param title,footnote Optional title and note paragraphs.
#' @param font,fontsize,borderstyle,headershade,headercolor,zebra,zebracolor
#'   Publication styles, as in [stratetab()]. Explicit header shading wins;
#'   ordinary-command session shading is not inherited.
#' @param mdappend Append Markdown; inherited session paths use write history.
#' @param open Open the written workbook.
#'
#' @details Zero events with positive time have exact bounds
#'   `[0, -log((1-level)/2)/time]` under every interval method. Zero-time cells
#'   remain empty; positive events without time refuse. Clustered inference
#'   uses one saturated exposure-offset Poisson model per section and outcome:
#'   cluster score sums, inverse-information bread, HC0 meat and the correction
#'   `G/(G-1)`. Limits use a normal critical value. A single fitted cluster or
#'   zero score variance has no interval. One event-contributing cluster can
#'   have an interval when other fitted clusters contribute exposure; the
#'   number of clusters is counted over the full fitted sample. Few-cluster
#'   normal inference can be unreliable. The closed-form implementation does
#'   not emulate an optimizer's convergence warnings.
#'
#'   Primary masks provide no complementary protection. Raw `stored$estimates`,
#'   `stored$rates`, `meta$rate_raw_rows`, `meta$saved_data`, cluster diagnostics,
#'   and `saving` data retain analytical numbers under count/zero masking.
#'   `meta$rate_rows` and `stored$publication_estimates` contain publication
#'   companions with withheld values removed. No-time and unfitted inference
#'   are computational missingness, not publication suppression.
#'
#'   This API implements the documented arguments rather than forwarding an
#'   unrestricted native option string. Delegated `rateratio`/`ratiodigits`
#'   options are not supported here. For compatible rate blocks, [stratetab()]
#'   with `rateratio=TRUE` (including blocks from [tt_rates()]) provides its
#'   existing independent-rate comparison; that helper's defaults and
#'   inference assumptions remain unchanged. Model/rate composition uses
#'   the frame's actual outcome and grouping identities, not invented fits.
#'
#' @return A [tt_table()] with command `"ratetab"`; numerical returns include
#'   `estimates` (columns `outcome,group,level,events,persontime,rate,lb,ub`),
#'   `N` (frequency-expanded common sample), `N_records` (original records),
#'   `N_zero`, `N_noci`, `N_nopt`, and cluster count matrix when applicable.
#'   The sample ledger retains original-record and expanded-record bases.
#'   The frame belongs to the `stratetab` rate family and records its actual
#'   producer `ratetab`, outcome identities, grouping identities and CI method.
#' @references
#' StataCorp (2025). Stata Base Reference Manual, `poisson`, Methods and
#' formulas, p. 8; Stata Programming Reference Manual, `_robust`, pp. 23-24.
#' Pinned tabtools 2.5.1 `ratetab.ado` (commit 712044f8), lines 28-32,
#' 217-268, 348-437 and 535-656.
#' @examples
#' d <- data.frame(group = c("A", "A", "B", "B"),
#'                 events = c(2, 1, 0, 0), years = c(2, 3, 4, 1))
#' ratetab(d, "group", "events", "years", per = 100)
#' @export
ratetab <- function(data, by, events = NULL, exposure = NULL, per = 1000,
                    ci = c("exact", "poisson", "cluster"), cluster = NULL,
                    level = 0.95, pyscale = 1, subset = NULL, fweight = NULL,
                    smallcells = NULL, nosmallcells = FALSE, masktext = NULL,
                    excludemasked = FALSE, zerocells = NULL, zerocells_persontime = FALSE,
                    cformat = NULL, digits = 1, pydigits = 0, sep = ", ",
                    outlabels = NULL, outcomeids = NULL, explabels = NULL, unitlabel = NULL,
                    saving = NULL, replace = FALSE, xlsx = NULL, sheet = "Results",
                    title = NULL, footnote = NULL, font = NULL, fontsize = NULL,
                    borderstyle = NULL, headershade = FALSE, headercolor = NULL,
                    zebra = FALSE, zebracolor = NULL, csv = NULL, markdown = NULL,
                    mdappend = FALSE, open = FALSE) {
  sinks <- .tt_resolve_sinks(
    list(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend,
         sheet = sheet, headershade = headershade, smallcells = smallcells,
         nosmallcells = nosmallcells, masktext = masktext),
    list(xlsx = !missing(xlsx), markdown = !missing(markdown),
         mdappend = !missing(mdappend), sheet = !missing(sheet),
         headershade = !missing(headershade), smallcells = !missing(smallcells),
         nosmallcells = !missing(nosmallcells), masktext = !missing(masktext)),
    policy = "sheet", mask = "rates")
  xlsx <- sinks$values$xlsx
  markdown <- sinks$values$markdown
  mdappend <- sinks$values$mdappend
  format <- .tt_resolve_numeric_format(cformat, if (missing(digits)) NULL else digits,
    digits_given = !missing(digits), sep = sep, default_digits = 1L)
  for (arg in c("excludemasked", "zerocells_persontime", "replace", "headershade", "zebra", "open")) {
    v <- get(arg)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) .rat_abort(paste0(arg, " must be TRUE or FALSE."))
  }
  if (is.null(events) || is.null(exposure)) {
    .rat_abort("Supply both events and exposure; R has no implicit stset source. Calculate at-risk exposure explicitly.", "source")
  }
  if (!is.character(ci) || anyNA(ci) || !length(ci) ||
      !(identical(ci, c("exact", "poisson", "cluster")) || length(ci) == 1L)) {
    .rat_abort("ci must be exact, poisson, or cluster.", "inference")
  }
  ci <- ci[1L]
  if (!ci %in% c("exact", "poisson", "cluster")) .rat_abort("ci must be exact, poisson, or cluster.", "inference")
  if ((ci == "cluster" && is.null(cluster)) || (ci != "cluster" && !is.null(cluster))) {
    .rat_abort("A cluster column is required only with ci='cluster'.", "cluster")
  }
  mask <- sinks$mask
  if (excludemasked && (ci != "cluster" || mask$threshold < 2L)) {
    .rat_abort("excludemasked requires clustered inference and an effective smallcells threshold of at least 2.", "inference")
  }
  if (!is.null(zerocells) && (!is.character(zerocells) || length(zerocells) != 1L ||
      is.na(zerocells) || !zerocells %in% c("dash", "blank"))) .rat_abort("zerocells must be NULL, dash, or blank.")
  if (zerocells_persontime && is.null(zerocells)) .rat_abort("zerocells_persontime requires zerocells.")
  for (arg in c("per", "pyscale", "level")) {
    v <- get(arg)
    if (!is.numeric(v) || is.complex(v) || !is.null(dim(v)) || length(v) != 1L ||
        !is.finite(v) || v <= 0) .rat_abort(paste0(arg, " must be a finite positive numeric scalar."), "domain")
  }
  ci_level <- if (level < 1) level * 100 else level
  if (is.null(ci_level) || ci_level < 10 || ci_level > 99.99) .rat_abort("level must be between 10% and 99.99%.", "inference")
  pydigits <- .check_int_range(pydigits, "pydigits", 0, 10)
  sheet <- .check_sheet(sheet %||% "Results")
  if (open && is.null(xlsx)) .rat_abort("open requires xlsx.")
  for (arg in c("title", "footnote", "unitlabel")) .tt_check_text_arg(get(arg), arg)
  footnote <- if (is.null(footnote)) "" else .tt_footnote_text(footnote)
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
    headershade = headershade, headercolor = headercolor, zebra = zebra, zebracolor = zebracolor)
  s <- .rat_sample(data, by, events, exposure, cluster, subset, fweight)
  defaults <- function(nms) vapply(nms, function(nm) {
    lab <- attr(data[[nm]], "label", exact = TRUE)
    if (is.null(lab) || identical(lab, "")) nm else lab
  }, "", USE.NAMES = FALSE)
  outlabels <- .st_labels(outlabels, length(s$events), "outlabels", "outcome labels", "outcomes", defaults(s$events))
  explabels <- .st_labels(explabels, length(s$by), "explabels", "grouping labels", "groups", defaults(s$by))
  ids <- .st_outcome_ids(outcomeids %||% s$events, outlabels, length(s$events))
  if (is.null(unitlabel) || !nzchar(unitlabel)) unitlabel <- .rat_unitlabel(per)
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  .rat_saving_preflight(saving, replace, list(xlsx, csv, markdown))
  est <- .rat_estimates(s, ci, ci_level / 100, per, pyscale, if (excludemasked) mask$threshold else 1L)
  tt <- .st_build(est$data, NULL, length(s$events), length(s$by), outlabels, ids, explabels,
    ci_level, .tt_level_text(ci_level), unitlabel, format$digits, 0L, pydigits, 2L, FALSE,
    title %||% "", footnote, style, sheet, format, mask, zerocells, zerocells_persontime)
  tt <- .rat_transport(tt, s, est, outlabels, mask, ci, per, pyscale, excludemasked,
                       zerocells, zerocells_persontime, unitlabel)
  tt$meta$numeric_format <- format
  written <- FALSE
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    tt$stored$csv <- csv
    written <- TRUE
  }
  if (!is.null(markdown)) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (!is.null(xlsx)) {
    sheet <- .xlsx_existing_sheet(xlsx, sheet)
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx
    tt$stored$sheet <- sheet
    tt$meta$sheet <- sheet
    written <- TRUE
  }
  if (!is.null(saving)) {
    .rat_write_saved(tt$meta$saved_data, saving, replace)
    tt$stored$saving <- saving
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}

# Native ratetab.ado:301-305 (post-baseline 4eecca4d): retain fractional
# per(), commas, and compact exponent notation instead of forcing fixed text.
.rat_unitlabel <- function(per) {
  out <- trimws(stata_fmt(per, "%21.15gc"))
  if (startsWith(out, ".")) out <- paste0("0", out)
  if (grepl("e", out, fixed = TRUE)) out <- sub("[.]?0+e", "e", out)
  out
}
