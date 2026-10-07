# Native saving() fixed schema and grouping-name collisions:
# ratetab.ado:134-173,535-656. RDS additionally preserves R classes/attributes.
.rat_saved_names <- function(by) {
  fixed <- c("outcome", "outcome_var", "outcome_label", "group", "groupvar", "level",
             "level_label", "events", "persontime", "rate", "lb", "ub", "masked", "nopersontime")
  original <- unique(by)
  used <- unique(c(fixed, original))
  saved <- original
  for (i in seq_along(original)) {
    if (!original[i] %in% fixed) next
    k <- 1L
    name <- substr(paste0("g_", original[i]), 1L, 32L)
    while (name %in% used && k < 99L) {
      k <- k + 1L
      name <- substr(paste0("g", k, "_", original[i]), 1L, 32L)
    }
    if (name %in% used) .rat_abort("No unique grouping-column name is available for saving.", "saving")
    saved[i] <- name
    used <- c(used, name)
  }
  stats::setNames(saved, original)
}

.rat_saved <- function(raw, s, outlabels, mask, unitlabel, level) {
  out <- raw[c("outcome", "group", "level", "events", "persontime", "rate", "lb", "ub")]
  out$outcome_var <- s$events[out$outcome]
  out$outcome_label <- outlabels[out$outcome]
  out$groupvar <- s$by[out$group]
  out$level_label <- vapply(seq_len(nrow(out)), function(i) s$groups[[out$group[i]]]$raw_labels[out$level[i]], "")
  out$masked <- as.integer(out$events >= 1 & out$events < mask$threshold)
  out$nopersontime <- as.integer(out$persontime == 0)
  out <- out[c("outcome", "outcome_var", "outcome_label", "group", "groupvar", "level",
               "level_label", "events", "persontime", "rate", "lb", "ub", "masked", "nopersontime")]
  labels <- c(outcome = "Outcome (column group) number", outcome_var = "Event variable",
    outcome_label = "Outcome label", group = "Grouping variable number", groupvar = "Grouping variable",
    level = "Level number within the grouping variable", level_label = "Level", events = "Events",
    persontime = "Person-time (divided by pyscale())", rate = paste0("Rate per ", unitlabel, " person-time"),
    lb = paste0("Lower ", .tt_level_text(level), "% limit of the rate"),
    ub = paste0("Upper ", .tt_level_text(level), "% limit of the rate"),
    masked = "1 if printed masked (1 to smallcells()-1 events)",
    nopersontime = "1 if no person-time (rate not computable; printed empty)")
  for (nm in names(labels)) attr(out[[nm]], "label") <- unname(labels[[nm]])
  map <- .rat_saved_names(s$by)
  for (nm in names(map)) {
    value <- .rat_group_attributes(s$data[[nm]][rep(NA_integer_, nrow(out))], s$data[[nm]])
    for (g in which(s$by == nm)) {
      rows <- which(out$group == g)
      value[rows] <- s$groups[[g]]$values[out$level[rows]]
    }
    # Assignment into a factor or labelled vector retains its original levels
    # and source attributes, including value labels and the variable label.
    out[[map[[nm]]]] <- .rat_group_attributes(value, s$data[[nm]])
  }
  rownames(out) <- NULL
  attr(out, "grouping_name_map") <- map
  attr(out, "grouping_original_names") <- s$by
  attr(out, "analytical_role") <- "raw_analytical"
  attr(out, "ci_method") <- unique(raw$ci_method)
  attr(out, "ci_level") <- level
  out
}

.rat_saving_preflight <- function(saving, replace, sinks) {
  if (is.null(saving)) return(invisible(NULL))
  if (!is.character(saving) || length(saving) != 1L || is.na(saving) || !nzchar(saving) ||
      !grepl("\\.(rds|dta)$", saving, ignore.case = TRUE)) {
    .rat_abort("saving must name a .rds or .dta file.", "saving")
  }
  if (!dir.exists(dirname(saving))) .rat_abort("The saving directory does not exist.", "saving")
  link <- Sys.readlink(saving)
  occupied <- file.exists(saving) || dir.exists(saving) || (!is.na(link) && nzchar(link))
  if (occupied && (!replace || dir.exists(saving))) .rat_abort("The saving path already exists; use replace=TRUE for a file.", "saving")
  targets <- Filter(Negate(is.null), sinks)
  if (any(vapply(targets, function(p) identical(.tt_path_key(p), .tt_path_key(saving)), logical(1)))) {
    .rat_abort("saving must differ from every publication destination.", "saving")
  }
  if (grepl("\\.dta$", saving, ignore.case = TRUE) && !requireNamespace("haven", quietly = TRUE)) {
    .rat_abort("Saving a .dta file requires the suggested haven package.", "saving")
  }
  invisible(NULL)
}

.rat_write_saved <- function(data, saving, replace) {
  # Unique owned staging file is always cleaned, including a failed writer.
  stage <- tempfile("ratetab-saving-", tmpdir = dirname(saving), fileext = tools::file_ext(saving))
  on.exit(unlink(stage), add = TRUE)
  if (grepl("\\.rds$", saving, ignore.case = TRUE)) {
    saveRDS(data, stage)
  } else {
    dta <- data
    map <- attr(data, "grouping_name_map", exact = TRUE)
    for (nm in unname(map)) {
      v <- dta[[nm]]
      if (is.factor(v)) {
        label <- attr(v, "label", exact = TRUE)
        codes <- .rat_factor_codes(v)
        dta[[nm]] <- haven::labelled(codes$values, labels = codes$labels, label = label)
      } else if (is.logical(v)) dta[[nm]] <- as.integer(v)
    }
    haven::write_dta(dta, stage, version = 14L)
  }
  link <- Sys.readlink(saving)
  if (!replace && (file.exists(saving) || (!is.na(link) && nzchar(link)))) {
    .rat_abort("The saving path became occupied before publication.", "saving")
  }
  if (!file.rename(stage, saving)) .rat_abort("Could not publish the saved analytical data.", "saving")
  invisible(saving)
}
