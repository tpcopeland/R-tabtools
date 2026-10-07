.rat_methods <- function(s, est, ci, unitlabel, level, mask, excludemasked,
                         zerocells, zerocells_persontime) {
  method <- switch(ci,
    exact = "from exact Poisson limits for the event count",
    poisson = "from the quadratic approximation to the Poisson log likelihood for the log rate",
    cluster = paste0("from one Poisson model per grouping variable with an indicator per level, person-time as exposure and variance clustered on ",
                     s$cluster, " (cluster counts in r(clusters))"))
  text <- paste0("Incidence rates per ", unitlabel, " person-years with ", .tt_level_text(level),
                 "% confidence intervals ", method)
  if (excludemasked && est$N_maskfit) text <- paste0(text, "; levels with 1 to ", mask$threshold - 1L,
    " events were left out of the clustered fit and have no interval")
  if (est$N_zero) {
    text <- paste0(text, if (is.null(zerocells)) "; cells with no events show the exact upper limit" else
      if (zerocells_persontime) "; cells with no events are printed without a count, person-time or rate" else
        "; cells with no events are printed without a count or rate")
  }
  if (est$N_nopt) text <- paste0(text, "; levels with no person-time for an outcome have no rate and are left empty")
  if (mask$threshold > 0) text <- paste0(text, "; cells with 1 to ", mask$threshold - 1L,
    " events are shown as ", mask$text, " with their person-time and rate withheld")
  paste0(text, ".")
}

.rat_key <- function(group, level, outcome) paste0("group:", group, "/level:", level, "/outcome:", outcome)

.rat_transport <- function(tt, s, est, outlabels, mask, ci, per, pyscale,
                           excludemasked, zerocells, zerocells_persontime, unitlabel) {
  raw <- est$raw
  raw$key <- .rat_key(raw$group, raw$level, raw$outcome)
  raw$groupvar <- s$by[raw$group]
  raw$outcome_var <- s$events[raw$outcome]
  raw$exposure_var <- s$exposure[raw$outcome]
  raw$ci_method <- ci
  raw$state <- ifelse(raw$source_persontime == 0, "empty",
    ifelse(!is.finite(raw$rate) | !is.finite(raw$lb) | !is.finite(raw$ub), "notest", "est"))
  raw$role <- "raw_analytical"
  num <- c("outcome", "group", "level", "events", "persontime", "rate", "lb", "ub")
  estimates <- as.matrix(raw[num])
  rownames(estimates) <- NULL
  tt$stored$estimates <- estimates
  tt$stored$estimates_role <- "raw_analytical"
  tt$stored$rates_role <- "raw_analytical"
  tt$stored$N <- sum(s$w[s$use])
  tt$stored$N_records <- sum(s$use)
  tt$stored$per <- per
  tt$stored$level <- tt$stored$ci_level
  tt$stored$N_zero <- est$N_zero
  tt$stored$N_noci <- est$N_noci
  tt$stored$N_nopt <- est$N_nopt
  if (excludemasked) tt$stored$N_maskfit <- est$N_maskfit
  tt$stored$ci_method <- ci
  if (ci == "cluster") {
    tt$stored$cluster <- s$cluster
    tt$stored$clusters <- est$clusters
  }
  tt$stored$methods <- .rat_methods(s, est, ci, unitlabel, tt$stored$ci_level, mask,
                                  excludemasked, zerocells, zerocells_persontime)

  # The renderer row order is section, level, outcome; native analytical
  # estimates are section, outcome, level. Join by positional source identity.
  publication <- tt$meta$rate_rows
  publication$group <- publication$exposure
  publication$level <- ave(publication$row, publication$group, publication$outcome,
                            FUN = function(x) seq_along(x))
  publication$key <- .rat_key(publication$group, publication$level, publication$outcome)
  i <- match(publication$key, raw$key)
  if (anyNA(i) || anyDuplicated(publication$key)) .rat_abort("Rate publication keys do not align with analytical rows.", "metadata")
  publication$groupvar <- raw$groupvar[i]
  publication$exposure_var <- raw$exposure_var[i]
  publication$state <- raw$state[i]
  # Display-time underflow is distinct from raw no-person-time accounting.
  empty <- publication$person_years == 0
  primary <- raw$events[i] >= 1 & raw$events[i] < mask$threshold & !empty
  zero <- !is.null(zerocells) & raw$events[i] == 0 & !empty
  publication$state[empty] <- "empty"
  publication$state[primary | zero] <- "masked"
  publication$mask_reason <- ifelse(empty, "no_display_person_time",
    ifelse(primary, "smallcells", ifelse(zero, "zerocells", "")))
  publication$role <- "publication"
  publication[primary | empty, c("events", "person_years", "rate", "lower", "upper")] <- NA_real_
  publication[zero, c("events", "rate", "lower", "upper")] <- NA_real_
  if (zerocells_persontime) publication[zero, "person_years"] <- NA_real_
  # Preserve actual analytical return precision in publication companions;
  # the existing renderer's local-macro count/time quantization affects text.
  visible <- !(primary | zero | empty)
  publication$events[visible] <- raw$events[i[visible]]
  publication$person_years[visible] <- raw$persontime[i[visible]]
  pub_order <- match(raw$key, publication$key)
  pub <- estimates
  pub[, c("events", "persontime", "rate", "lb", "ub")] <- as.matrix(
    publication[pub_order, c("events", "person_years", "rate", "lower", "upper")])
  tt$stored$publication_estimates <- pub
  tt$meta$rate_rows <- publication
  tt$meta$rate_raw_rows <- raw
  tt$meta$rate_blocks <- est$data$cell
  tt$meta$rate_blocks_role <- "raw_analytical"
  tt$meta$rate_cluster_diagnostics <- est$diagnostic
  tt$meta$sample_accounting <- est$sample
  tt$meta$source_sample <- list(row_ids = s$row_id[s$use], event_columns = s$events,
    exposure_columns = s$exposure, grouping_columns = s$by,
    weight_type = s$weight_type, weights = s$w[s$use],
    section_row_ids = lapply(s$group_ok, function(ok) s$row_id[s$use & ok]))
  tt$meta$saved_data <- .rat_saved(raw, s, outlabels, mask, unitlabel, tt$stored$ci_level)
  tt$meta$saved_data_role <- "raw_analytical"
  tt$meta$grouping_name_map <- attr(tt$meta$saved_data, "grouping_name_map", exact = TRUE)
  tt$meta$rate_group_values <- lapply(s$groups, `[[`, "values")
  tt$meta$rate_group_codes <- lapply(s$groups, `[[`, "codes")
  tt$meta$per <- per
  tt$meta$pyscale <- pyscale
  tt$meta$frame$producer <- "ratetab"
  tt$meta$frame$ci_method <- ci
  tt$meta$frame$grouping_variables <- s$by
  tt$meta$frame$exposure_variables <- s$exposure
  tt$meta$frame$rate_multiplier <- per
  tt$meta$frame$person_time_divisor <- pyscale
  tt$rows$key <- paste0("group:", tt$rows$block, ifelse(tt$rows$type == "var", "", "/level:"),
                        ifelse(tt$rows$type == "var", "", ave(seq_len(nrow(tt$rows)), tt$rows$block,
                          FUN = function(x) seq_along(x) - 1L)))
  tt$command <- "ratetab"
  tt
}
