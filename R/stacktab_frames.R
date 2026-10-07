# Stata 2.5.1 _stacktab_frames.ado:98-216. Each source identity owns a panel,
# even when two labels are identical; headers are taken only from source one.
.stacktab_frames <- function(frames, args) {
  if (!is.list(frames) || is.data.frame(frames) || !length(frames)) {
    .puttab_abort("frames must be a nonempty list of data frames or tt_tables.")
  }
  ids <- names(frames)
  if (is.null(ids)) ids <- paste0("frame", seq_along(frames))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    .puttab_abort("frames source identities must be nonempty and unique.")
  }
  labels <- rep("", length(frames))
  sources <- vector("list", length(frames))
  for (i in seq_along(frames)) {
    x <- frames[[i]]
    if (is.list(x) && !is.data.frame(x) && !inherits(x, "tt_table")) {
      if (!identical(sort(names(x)), sort(c("data", "label")))) {
        .puttab_abort("Each frames entry must be a data frame, tt_table, or list(data = source, label = text).")
      }
      if (!is.character(x$label) || length(x$label) != 1L || is.na(x$label)) {
        .puttab_abort("Frame labels must be single non-missing literal strings.")
      }
      labels[i] <- x$label
      x <- x$data
    }
    if (!is.character(labels[i]) || is.na(labels[i])) .puttab_abort("Frame labels must be single non-missing literal strings.")
    if (inherits(x, "tt_table")) {
      sources[[i]] <- .puttab_from_tt(x, FALSE)
    } else {
      if (!is.data.frame(x)) .puttab_abort("Each frames source must be a data frame or tt_table.")
      if (!ncol(x) || !nrow(x)) .puttab_abort("Each frames source needs columns and observations.")
      sources[[i]] <- .puttab_from_data(x, NULL, NULL, args$digits %||% getOption("tabtools.digits") %||% 2L,
                                      TRUE, FALSE, FALSE)
      # In frames mode Stata follows each column's own numeric display format.
      keep <- sources[[i]]$source_rows
      for (j in seq_along(x)) {
        fmt <- attr(x[[j]], "format.stata", exact = TRUE)
        v <- x[[j]][keep]
        if (is.numeric(v) && !inherits(v, c("Date", "POSIXt")) &&
            is.character(fmt) && length(fmt) == 1L && grepl("^%-?0?[0-9]+[.][0-9]+[fg]c?$", fmt)) {
          rendered <- trimws(stata_fmt(as.numeric(v), fmt))
          rendered[is.na(v)] <- ""
          # Labels have precedence over the raw display format.
          labs <- attr(x[[j]], "labels", exact = TRUE)
          if (is.numeric(labs)) {
            hit <- .puttab_label_matches(as.numeric(v), as.numeric(labs))
            rendered[!is.na(hit)] <- names(labs)[hit[!is.na(hit)]]
          }
          sources[[i]]$body[, j] <- rendered
        }
      }
    }
  }
  sample <- .tt_sample_bind(lapply(sources, function(s) s$sample),
                            prefixes = ids, commands = rep("stacktab", length(sources)))
  widths <- vapply(sources, function(s) ncol(s$body), 0L)
  if (any(widths != widths[1L])) .puttab_abort("frames must have equal column counts; columns are stacked by position.")
  K <- widths[1L]
  header <- sources[[1L]]$header %||% paste0("c", seq_len(K))
  body <- do.call(rbind, lapply(sources, function(s) s$body))
  data <- as.data.frame(body, stringsAsFactors = FALSE)
  names(data) <- paste0("c", seq_len(K))
  for (j in seq_len(K)) attr(data[[j]], "label") <- header[j]
  data$panel_id <- rep(seq_along(sources), vapply(sources, function(s) nrow(s$body), 0L))
  attr(data$panel_id, "labels") <- stats::setNames(seq_along(sources), labels)
  args$x <- data
  args$panel <- "panel_id"
  args$varlabels <- TRUE
  args$noembedheader <- TRUE
  compute_args <- args
  # Keep the names: omission would reactivate the session destinations.
  compute_args[c("xlsx", "csv", "markdown", "sheet")] <- rep(list(NULL), 4L)
  compute_args$mdappend <- FALSE
  compute_args$open <- FALSE
  tt <- do.call(puttab, compute_args)
  tt$command <- "stacktab"
  tt$layout$md_keep_blank <- TRUE
  tt$stored$source <- "frames"
  tt$stored$n_frames <- length(frames)
  tt$stored$frames <- ids
  tt$stored$blocks_loaded <- length(frames)
  tt$meta$frame_sources <- data.frame(identity = ids, label = labels, stringsAsFactors = FALSE)
  tt$meta$sample_accounting <- sample
  if (!is.null(args$sheet)) tt$meta$sheet <- args$sheet
  .puttab_export(tt, xlsx = args$xlsx, csv = args$csv, markdown = args$markdown,
                 sheet = args$sheet %||% "Table", sheet_given = !is.null(args$sheet),
                 mdappend = args$mdappend, open = args$open)
}
