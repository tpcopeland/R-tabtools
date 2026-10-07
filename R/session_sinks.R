# Session publication destinations. Native: _tabtools_set_sinks.ado:51-113,
# 197-213 (2.5.1). Successful normalized paths survive changing/clearing keys.
.tt_sink_state <- new.env(parent = emptyenv())
.tt_sink_state$active <- list(workbook = NULL, markdown = NULL)
.tt_sink_state$written <- character()

# Keep the chosen filename/extension (including a file symlink's spelling).
# Identity comparison separately uses .tt_path_key(), which follows that link.
.tt_absolute_sink_path <- function(path) {
  path <- path.expand(path)
  directory <- normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
  if (!grepl("^(/|[A-Za-z]:)", directory)) directory <- file.path(getwd(), directory)
  file.path(directory, basename(path))
}

.tt_set_sink_state <- function() {
  .tt_sink_state$active <- lapply(c("workbook", "markdown"), function(k) {
    p <- getOption(paste0("tabtools.", k))
    if (is.character(p) && length(p) == 1L && !is.na(p) && nzchar(p)) .tt_path_key(p) else NULL
  })
  names(.tt_sink_state$active) <- c("workbook", "markdown")
  invisible(NULL)
}

# Called only after the actual destination has been written successfully.
# Explicit writes count only when they hit the currently active session path.
.tt_mark_sink <- function(path, key) {
  active <- getOption(paste0("tabtools.", key))
  if (is.character(active) && length(active) == 1L && !is.na(active) && nzchar(active) &&
      identical(.tt_path_key(path), .tt_path_key(active))) {
    .tt_sink_state$written <- union(.tt_sink_state$written, .tt_path_key(path))
    .tt_set_sink_state()
  }
  invisible(NULL)
}

.tt_check_session_option <- function(key, value) {
  if (is.null(value)) return(NULL)
  if (key == "workbook") {
    .tt_check_path(value, "\\.xlsx$", key, "a .xlsx file")
    return(.tt_absolute_sink_path(value))
  }
  if (key == "markdown") {
    .tt_check_path(value, "\\.(md|markdown|qmd|rmd)$", key,
                   "a .md, .markdown, .qmd, or .rmd file")
    return(.tt_absolute_sink_path(value))
  }
  if (key == "headershade") {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("{.arg headershade} must be TRUE, FALSE, or NULL.",
                     class = "tabtools_error_session_option", call = NULL)
    }
  } else if (key == "smallcells") {
    value <- .check_int_range(value, key, 3L, .Machine$integer.max)
  } else if (key == "smallcells_mode") {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% c("strict", "primary")) {
      cli::cli_abort("{.arg smallcells_mode} must be {.val strict}, {.val primary}, or NULL.",
                     class = "tabtools_error_session_option", call = NULL)
    }
  } else if (key == "masktext") {
    .tt_check_text_arg(value, key)
  }
  value
}

# Values and their original explicitness travel together. Never infer missing()
# from values after a wrapper has normalized NULL or constructed an args list.
# Policy and capabilities implement tabtools.sthlp:302-316; headershade is
# puttab-only (:292). Explicit path writers activate no additional destinations.
.tt_resolve_sinks <- function(values, given, policy = c("sheet", "omitted", "writer"),
                              shade = FALSE, mask = c("none", "table1", "rates", "crosstab")) {
  policy <- match.arg(policy)
  mask <- match.arg(mask)
  original <- values
  original_given <- given
  supplied <- function(key) isTRUE(given[[key]])
  values$mdappend <- values$mdappend %||% FALSE
  if (!is.logical(values$mdappend) || length(values$mdappend) != 1L || is.na(values$mdappend)) {
    cli::cli_abort("{.arg mdappend} must be TRUE or FALSE.", call = NULL)
  }
  # desctab.ado:106-107: non-NULL excel has priority, including over xlsx=NULL.
  if (supplied("excel") && !is.null(values$excel)) {
    values$xlsx <- values$excel
    given$xlsx <- TRUE
  } else if (supplied("excel") && is.null(values$xlsx)) {
    given$xlsx <- TRUE
  }
  trigger <- policy == "omitted" ||
    (policy == "sheet" && supplied("sheet") && !is.null(values$sheet))
  inherited <- c(xlsx = FALSE, markdown = FALSE, headershade = FALSE,
                 smallcells = FALSE, masktext = FALSE)
  for (key in c("xlsx", "markdown")) {
    if (trigger && !supplied(key)) {
      opt <- if (key == "xlsx") "workbook" else "markdown"
      values[key] <- list(getOption(paste0("tabtools.", opt)))
      inherited[[key]] <- !is.null(values[[key]])
    }
  }
  if (inherited[["markdown"]] && !supplied("mdappend")) {
    values$mdappend <- .tt_path_key(values$markdown) %in% .tt_sink_state$written
  }
  if (shade && !supplied("headershade")) {
    value <- getOption("tabtools.headershade")
    if (!is.null(value)) {
      values$headershade <- .tt_check_session_option("headershade", value)
      inherited[["headershade"]] <- TRUE
    }
  }
  if (shade && is.null(values$headershade)) values$headershade <- FALSE
  if (mask != "none") {
    # Preserve threshold/mode/text explicitness for P.7/WP-3F. An explicit
    # active threshold defaults to strict independently of the session mode.
    if (!supplied("smallcells")) {
      values["smallcells"] <- list(getOption("tabtools.smallcells"))
      inherited[["smallcells"]] <- !is.null(values$smallcells)
    }
    if (!supplied("smallcells_mode")) {
      values$smallcells_mode <- if (supplied("smallcells")) "strict" else
        getOption("tabtools.smallcells_mode") %||% "strict"
    }
    if (!supplied("masktext")) {
      values["masktext"] <- list(getOption("tabtools.masktext"))
      inherited[["masktext"]] <- !is.null(values$masktext)
    }
    if (mask %in% c("table1", "crosstab") && is.numeric(values$smallcells) && !is.complex(values$smallcells) &&
        is.null(dim(values$smallcells)) &&
        length(values$smallcells) == 1L && !is.na(values$smallcells) && values$smallcells == 0) {
      values["smallcells"] <- list(NULL)
    }
  }
  mask_result <- NULL
  if (mask == "rates") {
    mask_result <- .st_resolve_mask(original$smallcells, supplied("smallcells"),
                                    original$nosmallcells, original$masktext,
                                    supplied("masktext"), resolved = values)
    if (isTRUE(original$nosmallcells)) inherited[["smallcells"]] <- FALSE
  } else if (mask %in% c("table1", "crosstab")) {
    # Crosstab uses the same explicit threshold/mode/text policy as Table 1;
    # its analytical and publication masking remain command-specific.
    mask_result <- .t1_resolve_mask(original, original_given, values)
    values["smallcells"] <- list(mask_result$threshold)
    values$smallcells_mode <- mask_result$mode
    values["masktext"] <- list(mask_result$text)
    if (is.null(mask_result$threshold)) {
      inherited[["smallcells"]] <- FALSE
      inherited[["masktext"]] <- FALSE
    }
  }
  if (policy != "writer" && supplied("sheet") && !is.null(values$sheet)) .check_sheet(values$sheet)
  if (policy != "writer" && supplied("sheet") && !is.null(values$sheet) && is.null(values$xlsx)) {
    cli::cli_warn("{.arg sheet} ignored; no {.arg xlsx} and no session workbook.",
                  class = "tabtools_warning_sheet_without_workbook", call = NULL)
  }
  if (!is.null(values$xlsx)) .tt_check_path(values$xlsx, "\\.xlsx$", "xlsx", "a .xlsx file")
  if (!is.null(values$markdown)) {
    .tt_check_path(values$markdown, "\\.(md|markdown|qmd|rmd)$", "markdown",
                   "a .md, .markdown, .qmd, or .rmd file")
  }
  preflight <- function() .tt_preflight_targets(xlsx = values$xlsx, csv = values$csv,
                                               markdown = values$markdown, mdappend = values$mdappend)
  keys <- if (policy == "writer") {
    # Preserve the explicit writers' existing contextual failure contract.
    tryCatch(preflight(), error = function(e) {
      if (!is.null(values$xlsx)) {
        cli::cli_abort("Could not write the workbook {.file {values$xlsx}}.", parent = e, call = NULL)
      }
      path <- values$csv %||% values$markdown
      cli::cli_abort("Could not write the {.arg path} target {.file {path}}.", parent = e, call = NULL)
    })
  } else preflight()
  if (inherited[["xlsx"]]) message("tabtools: using session workbook ", values$xlsx)
  if (inherited[["markdown"]]) {
    message("tabtools: using session Markdown ", values$markdown,
            if (values$mdappend) " (append)" else " (replace)")
  }
  if (inherited[["headershade"]]) message("tabtools: using session headershade")
  if (inherited[["smallcells"]]) message("tabtools: using session smallcells ", values$smallcells)
  if (inherited[["masktext"]]) message("tabtools: using session masktext ", values$masktext)
  list(values = values, given = original_given, inherited = inherited, keys = keys, mask = mask_result)
}
