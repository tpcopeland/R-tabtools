#' Explicit failed-model column for a count/mincount table
#'
#' A failed placeholder is opt-in and carries no analytical sample or inference.
#' At least one real fitted model is required. Ordinary NULL/errors remain
#' invalid models. Count records must be supplied positionally, with NULL at a
#' placeholder position. MI unequal-sample `esampvaryok` is unsupported.
#' @param reason Nonempty literal explanation included in the table footnote.
#' @param model_id Optional case-preserved identifier.
#' @return An explicit `tt_failed_model` sentinel for [regtab()].
#' @examples
#' failed <- tt_failed_model("Fit failed to converge", model_id = "Model B")
#' failed$reason
#' @export
tt_failed_model <- function(reason, model_id = NULL) {
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(trimws(reason))) {
    .fc_abort("A failed model requires one nonempty reason.")
  }
  if (!is.null(model_id) && (!is.character(model_id) || length(model_id) != 1L || is.na(model_id) || !nzchar(model_id))) .fc_abort("`model_id` must be one nonempty string or NULL.")
  structure(list(reason = reason, model_id = model_id), class = "tt_failed_model")
}

.rt_failed <- function(fit) identical(class(fit), "tt_failed_model")

.rt_failed_info <- function(fit, template) {
  info <- .mi(fit, NA_character_, effect_scale = template$effect_scale,
              null_value = template$null_value, auto_nointercept = template$auto_nointercept)
  info$model_id <- fit$model_id %||% NA_character_
  info$outcome_id <- NA_character_
  info$failed_reason <- fit$reason
  info
}

.rt_failed_rows <- function() .rt_row("", "", "", "")[0L, , drop = FALSE]

.rt_count_key <- function(key) gsub("(^|#)c\\.", "\\1", sub("^.*::", "", key))

.rt_count_structure <- function(key, kind) {
  raw <- sub("^.*::", "", key)
  eq <- if (grepl("::", key, fixed = TRUE)) paste0(sub("::.*$", "", key), "::") else ""
  factor <- grepl("(^|#)[0-9]+\\.", raw)
  if (!factor || !kind %in% c("level", "int_level", "flat")) return(c(parent = "", signature = ""))
  parent <- gsub("(^|#)([0-9]+|c)\\.", "\\1", raw)
  signature <- gsub("(^|#)[0-9]+\\.", "\\1*.", raw)
  c(parent = paste0(eq, parent), signature = paste0(eq, signature))
}

# A adds semantic metadata without deriving bases from display text or globals.
.rt_count_rows <- function(rows, fit, record = NULL) {
  structure <- lapply(seq_len(nrow(rows)), function(i) .rt_count_structure(rows$key[i], rows$kind[i]))
  rows$parent_key <- vapply(structure, `[[`, "", "parent")
  rows$term_signature <- vapply(structure, `[[`, "", "signature")
  if (is.null(rows$count_source_key)) rows$count_source_key <- .rt_count_key(rows$key)
  if (is.null(rows$level_identity)) {
    frame <- if (!is.null(record)) record$snapshot$frame else .fc_retained_frame(fit)
    if (!is.null(frame)) {
      # Match row extraction's checked label/code restoration. For captured
      # records use only their frozen source, never a later caller binding.
      source_fit <- if (is.null(record)) fit else
        .fc_restore(fit, frame, record$snapshot$source)
      frame <- .rt_restore_attrs(frame, source_fit, warn = FALSE)
    }
    rows$level_identity <- vapply(rows$count_source_key, function(key) {
      if (is.null(frame)) return("")
      .fc_level_selection(key, frame)$identity
    }, "")
  }
  rows$status[rows$status %in% "base"] <- "ref"
  rows$status[rows$status %in% "cns"] <- "constrained"
  rows$status[rows$status %in% "header"] <- ""
  # Custom frames declare semantic states; never reinterpret them as fit notes.
  if (!is.data.frame(fit) && nrow(rows)) {
    # Adapter covariance is a coefficient matrix even for Matrix-valued
    # lme4 and list-valued glmmTMB vcov methods. A captured ordinary matrix
    # remains authoritative; unsupported covariance leaves states unchanged.
    covariance <- if (!is.null(record) && is.matrix(record$identity$covariance))
      record$identity$covariance else
        tryCatch(.rt_quiet_zero_weight(tt_vcov(fit, vce = "model")), error = function(e) NULL)
    variance <- rep(NA_real_, nrow(rows))
    if (is.matrix(covariance) && !is.null(rownames(covariance))) {
      variance <- diag(covariance)[match(rows$term, rownames(covariance))]
    }
    rows$variance_ok <- is.finite(variance) & variance > 0
    known <- !is.na(rows$term) & rows$term %in% rownames(covariance)
    bad <- rows$status %in% "est" & known & !rows$variance_ok
    rows$status[bad] <- "notest"
  }
  if (!is.null(record)) {
    rows$count_events <- record$terms$events[match(rows$count_source_key, record$terms$count_key)]
  }
  rows
}

.fc_level_selection <- function(key, frame) {
  parts <- strsplit(.rt_count_key(key), "#", fixed = TRUE)[[1L]]
  keep <- rep(TRUE, nrow(frame))
  has_factor <- FALSE
  identity <- list()
  for (part in parts) {
    if (!grepl("^[0-9]+\\.", part)) next
    has_factor <- TRUE
    code <- sub("\\..*$", "", part)
    name <- sub("^[0-9]+\\.", "", part)
    if (!name %in% names(frame)) .fc_abort(paste0("Per-term counting requires captured predictor `", name, "`."))
    x <- frame[[name]]
    codes <- .rt_codes(x)
    lev <- names(codes)[codes == code]
    if (length(lev) != 1L) .fc_abort(paste0("The fit-time level mapping for `", part, "` is ambiguous."))
    keep <- keep & !is.na(x) & as.character(x) == lev
    identity[[name]] <- lev
  }
  if (!has_factor && length(parts) == 1L && parts %in% names(frame)) {
    x <- frame[[parts]]
    if (is.numeric(x) && all(!is.na(x) & x %in% c(0, 1))) {
      keep <- x == 1
      has_factor <- TRUE
    }
  }
  list(eligible = has_factor, take = keep,
       identity = if (length(identity)) paste(format(serialize(identity, NULL, version = 2L)), collapse = "") else "")
}

.fc_term_counts <- function(rows, frame, events) {
  result <- list()
  held <- list()
  for (i in seq_len(nrow(rows))) {
    if (!rows$role[i] %in% "coef" || rows$kind[i] %in% c("cat_header", "int_header")) next
    selection <- .fc_level_selection(rows$key[i], frame)
    if (!selection$eligible) next
    key <- .rt_count_key(rows$key[i])
    variance <- isTRUE(rows$variance_ok[i]) && rows$status[i] %in% "est"
    result[[length(result) + 1L]] <- data.frame(count_key = key,
      term_signature = rows$term_signature[i], events = sum(events[selection$take]),
      n_records = sum(selection$take), variance_ok = variance,
      status = rows$status[i], source_key = rows$key[i], level_identity = selection$identity, stringsAsFactors = FALSE)
    if (nzchar(rows$term_signature[i]) && any(selection$take)) {
      held[[length(held) + 1L]] <- data.frame(term_signature = rows$term_signature[i],
        key = rows$key[i], count_key = key, level_identity = selection$identity, stringsAsFactors = FALSE)
    }
  }
  counts <- if (length(result)) do.call(rbind, result) else data.frame(count_key = character(),
    term_signature = character(), events = numeric(), n_records = integer(), variance_ok = logical(),
    status = character(), source_key = character(), level_identity = character())
  # Native repeated-equation coefficients share counts; every included copy
  # must have a usable variance. Base outcomes supply no estimated copy.
  for (key in unique(counts$count_key)) {
    hit <- which(counts$count_key == key & !counts$status %in% c("ref", "constrained"))
    if (length(hit)) counts$variance_ok[counts$count_key == key] <- all(counts$variance_ok[hit])
  }
  list(terms = counts[!duplicated(counts$count_key), , drop = FALSE],
       levels = if (length(held)) unique(do.call(rbind, held)) else
         data.frame(term_signature = character(), key = character(), count_key = character(), level_identity = character()))
}

.rt_count_options <- function(mincount, notestlabel, absentlabel, cnslabel,
                              refcat, omitlabel, emptylabel, empty_given) {
  if (!is.null(mincount) && (!is.numeric(mincount) || length(mincount) != 1L ||
      is.na(mincount) || !is.finite(mincount) || mincount < 1 || mincount > .Machine$integer.max || mincount != floor(mincount))) .fc_abort("`mincount` must be one positive integer or NULL.")
  if (!is.null(mincount) && !empty_given) emptylabel <- "\u2013"
  if (is.null(notestlabel)) notestlabel <- emptylabel
  if (!is.null(absentlabel) && is.null(mincount)) .fc_abort("`absentlabel` requires `mincount`.")
  for (x in list(notestlabel = notestlabel, absentlabel = absentlabel %||% "", cnslabel = cnslabel)) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) .fc_abort("Count-state labels must be single nonmissing strings.")
  }
  if (notestlabel %in% c(refcat, omitlabel) || (!is.null(absentlabel) && absentlabel == refcat)) .fc_abort("Not-estimable/absent labels must differ from the reference label, and notestlabel from omitlabel.")
  list(mincount = if (is.null(mincount)) NULL else as.integer(mincount),
       notestlabel = notestlabel, absentlabel = absentlabel %||% "", cnslabel = cnslabel, emptylabel = emptylabel)
}

.rt_mincount_union <- function(u, o) {
  provenance <- list()
  masked <- absent <- threshold <- 0L
  for (m in seq_along(u$cells)) {
    c <- u$cells[[m]]
    c$status[is.na(c$status)] <- "absent"
    structural <- u$rows$kind %in% c("cat_header", "int_header")
    c$status[structural] <- ""
    c$mask_reason <- rep("", nrow(c))
    if (!is.null(o$mincount) && !.rt_failed(o$fits[[m]])) {
      record <- o$fitcounts[[m]]
      for (i in seq_len(nrow(c))) {
        if (structural[i] || c$status[i] %in% c("ref", "constrained")) next
        key <- if (!is.null(c$count_source_key) && !is.na(c$count_source_key[i])) c$count_source_key[i] else .rt_count_key(u$rows$key[i])
        j <- match(key, record$terms$count_key)
        reason <- ""
        if (c$status[i] == "absent") {
          signature <- u$rows$term_signature[i] %||% ""
          bare_signature <- sub("^.*::", "", signature)
          record_signatures <- sub("^.*::", "", record$states$term_signature)
          if (is.na(signature) || !nzchar(signature) || !bare_signature %in% record_signatures) next
          absent <- absent + 1L
          held <- u$rows$level_identity[i] %in% record$levels$level_identity[sub("^.*::", "", record$levels$term_signature) == bare_signature]
          if (held) {
            c$status[i] <- "notest"
            reason <- "sample_present_unestimated"
          } else reason <- "sample_absent"
        } else {
          eligible <- nzchar(u$rows$parent_key[i] %||% "") || !is.na(j)
          if (!eligible) next
          if (is.na(j)) .fc_abort(paste0("Model ", m, " has no captured term count for `", u$rows$key[i], "`."))
          if (!record$terms$variance_ok[j] || c$status[i] %in% c("omit", "empty", "notest")) {
            c$status[i] <- "notest"
            reason <- "unusable_variance"
          } else if (record$terms$events[j] < o$mincount) {
            c$status[i] <- "masked"
            reason <- "mincount"
            threshold <- threshold + 1L
          } else next
          masked <- masked + 1L
        }
        c$mask_reason[i] <- reason
        c[i, c("estimate", "conf.low", "conf.high", "p.value")] <- NA_real_
        provenance[[length(provenance) + 1L]] <- data.frame(model = as.integer(m), key = u$rows$key[i],
          state = c$status[i], reason = reason, events = if (is.na(j)) NA_real_ else record$terms$events[j],
          stringsAsFactors = FALSE)
      }
    }
    u$cells[[m]] <- c
  }
  stored <- list(N_masked = masked, N_absent = absent)
  stored$smallcells <- list(threshold = o$mincount %||% 0L, mode = "primary",
    n_masked = as.integer(threshold), n_linked = 0L)
  list(u = u, stored = stored, provenance = if (length(provenance)) do.call(rbind, provenance) else data.frame())
}
