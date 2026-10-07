# Numeric companions are producer-owned publication records. The snapshot
# checks exact reuse; labels or matching matrix dimensions cannot authenticate
# a companion imported separately from its table.
.ct_v251_abort <- function(message) cli::cli_abort(message,
  class = "tabtools_error_composition", call = NULL)

.ct_stamp <- function(x) {
  # Preserve final orientation in the frame that as.data.frame transports;
  # its existing exact snapshot check also binds imported orientation.
  if (identical(x$meta$regtab_orientation, "transpose")) x$meta$frame$orientation <- "transpose"
  x$meta$composition <- list(version = 1L, body = x$body, header = x$header,
    rows = x$rows, frame = x$meta$frame, companion = x$meta$regtab_rows,
    companion_hash = rlang::hash(x$meta$regtab_rows))
  x
}

.ct_forest_record <- function(records) {
  records$status %in% c("base", "ref") |
    (records$status %in% "est" & is.finite(records$estimate) &
      is.finite(records$conf.low) & is.finite(records$conf.high) &
      records$conf.low <= records$conf.high)
}

# Selection/alignment preserves publication records without deriving any
# number from text or from analytical backups. Mixed legacy sources cannot
# acquire a new authenticated numerical companion by composition.
.ct_vertical_companion <- function(sources, picked, body_at, labels) {
  fields <- c("row", "model", "label", "status", "estimate", "conf.low", "conf.high", "p.value")
  valid <- vapply(sources, function(s) isTRUE(s$authenticated) &&
    is.data.frame(s$rows) && all(fields %in% names(s$rows)), TRUE)
  if (!all(valid)) return(NULL)
  out <- lapply(seq_along(sources), function(f) {
    s <- sources[[f]]
    r <- s$rows[s$rows$row %in% picked[[f]], fields, drop = FALSE]
    r$source_row <- r$row;r$source_model <- r$model;r$source_frame <- f
    r$row <- unname(body_at[[f]][as.character(r$row)])
    r$model <- match(r$model, s$model_map)
    r$label <- labels[r$row]
    r
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

.ct_check_companion_source <- function(x) {
  stamp <- if (inherits(x, "tt_table")) x$meta$composition else
    if (is.data.frame(x)) attr(x, "composition", exact = TRUE) else NULL
  if (is.null(stamp)) return(invisible(NULL))
  if (!is.list(stamp) || !identical(stamp$version, 1L) ||
      !is.data.frame(stamp$body) || !is.data.frame(stamp$rows) || !is.list(stamp$frame)) {
    .ct_v251_abort("Composition source snapshot is missing or malformed.")
  }
  if (!identical(stamp$companion_hash, rlang::hash(stamp$companion))) {
    .ct_v251_abort("Composition source state or numeric companions changed after production.")
  }
  if (inherits(x, "tt_table")) {
    if (!identical(stamp$body, x$body) || !identical(stamp$header, x$header) ||
        !identical(stamp$rows, x$rows) || !identical(stamp$frame, x$meta$frame) ||
        !identical(stamp$companion, x$meta$regtab_rows)) {
      .ct_v251_abort("Composition source text, identity or numeric companions changed after production.")
    }
  } else {
    expected <- as.matrix(rbind(do.call(rbind, lapply(stamp$header, `[[`, "text")),
                                as.matrix(stamp$body)))
    actual <- as.matrix(x)
    dimnames(expected) <- dimnames(actual) <- NULL
    frame <- attributes(x)[names(stamp$frame)]
    if (!identical(actual, expected) || !identical(frame, stamp$frame)) {
      .ct_v251_abort("Imported composition text or frame identity differs from its producer snapshot.")
    }
  }
  invisible(stamp)
}

.ct_reformat_source <- function(source, cformat, cisep) {
  if (is.null(cformat) && is.null(cisep)) return(source)
  format <- .tt_resolve_numeric_format(cformat = cformat, sep = cisep %||% ", ", sep_arg = "cisep")
  layout <- .ct_layout(source, 1L)
  if (!is.null(cformat)) {
    records <- source$rows
    required <- c("row", "model", "status", "estimate", "conf.low", "conf.high")
    if (!is.data.frame(records) || any(!required %in% names(records)) ||
        anyDuplicated(records[c("row", "model")])) {
      .ct_v251_abort("cformat requires unique producer-owned publication numeric companions.")
    }
    for (field in c("row", "model")) {
      values <- records[[field]]
      limit <- if (field == "row") length(source$labels) else layout$n_models
      if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
          any(values != trunc(values) | values < 1L | values > limit)) {
        .ct_v251_abort("Publication numeric companions have invalid row/model positions.")
      }
    }
    if (!is.character(records$status) || anyNA(records$status) ||
        any(!vapply(records[c("estimate", "conf.low", "conf.high")], is.numeric, TRUE))) {
      .ct_v251_abort("Publication numeric companion states or values are malformed.")
    }
    for (m in seq_len(layout$n_models)) {
      first <- (m - 1L) * layout$cpm + 1L
      for (r in seq_along(source$labels)) {
        record <- records[records$row == r & records$model == m, , drop = FALSE]
        if (!nrow(record)) next
        if (nrow(record) != 1L) .ct_v251_abort("Composition numeric companions are ambiguous.")
        if (!record$status %in% "est") next
        numbers <- unlist(record[c("estimate", "conf.low", "conf.high")], use.names = FALSE)
        # Native keeps text when the publication companion cannot supply the
        # full interval. Never reconstruct suppressed/overridden values.
        if (length(numbers) != 3L || any(!is.finite(numbers))) next
        if (numbers[2L] > numbers[3L]) .ct_v251_abort("Publication interval bounds are reversed.")
        stars <- regmatches(source$cells[r, first], regexpr("\\*+(?= *(\\(|$))", source$cells[r, first], perl = TRUE))
        if (!length(stars)) stars <- ""
        text <- .tt_format_numeric(numbers, format)
        ci <- paste0("(", text[2L], format$sep, text[3L], ")")
        if (layout$mode == "standard") {
          source$cells[r, first] <- paste0(text[1L], stars)
          source$cells[r, first + 1L] <- ci
        } else source$cells[r, first] <- paste(paste0(text[1L], stars), ci)
      }
    }
  } else {
    # Pinned native cisep-only contract rewrites canonical '(a, b)' text;
    # it does not guess an alternative source separator.
    pattern <- "\\(([^ ,]+(?:,[0-9]{3})*), ([^ ]+)\\)$"
    for (m in seq_len(layout$n_models)) {
      ci_column <- (m - 1L) * layout$cpm + if (layout$mode == "standard") 2L else 1L
      for (r in seq_along(source$labels)) {
        cell <- source$cells[r, ci_column]
        if (!grepl("\\(", cell)) next
        match <- regexec(pattern, cell, perl = TRUE)
        pieces <- regmatches(cell, match)[[1L]]
        if (length(pieces) != 3L) .ct_v251_abort("cisep alone requires canonical '(a, b)' source intervals.")
        start <- match[[1L]][1L]
        source$cells[r, ci_column] <- paste0(substr(cell, 1L, start - 1L),
          "(", pieces[2L], format$sep, pieces[3L], ")")
      }
    }
  }
  source
}
