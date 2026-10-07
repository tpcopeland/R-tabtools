# Exact scalar event times and entry < t <= exit: survtab.ado:379-443,503-562.
.sv_risk <- function(records, time) {
  rows <- which(records$entry < time & records$exit >= time)
  r <- records[rows, , drop = FALSE]
  sum(r$frequency[!duplicated(r$subject)])
}

# Greenwood and log-log bands: held [ST] sts Methods/formulas p17.
.sv_curve <- function(records, level) {
  times <- sort(unique(records$exit[records$event == 1L]))
  grid <- data.frame(time = times, risk = numeric(length(times)), events = numeric(length(times)),
    survival = numeric(length(times)), increment = numeric(length(times)), se = numeric(length(times)),
    lower = numeric(length(times)), upper = numeric(length(times)))
  S <- 1; greenwood <- 0
  z <- stats::qnorm((1 + level / 100) / 2)
  for (i in seq_along(times)) {
    Y <- .sv_risk(records, times[i])
    d <- sum(records$frequency[records$event == 1L & records$exit == times[i]])
    if (!is.finite(Y) || Y <= 0 || d <= 0 || d > Y) .sv_abort("An event has an invalid exact-time risk set.", "tabtools_error_survival_risk")
    S <- S * (1 - d / Y)
    increment <- if (Y > d) d / (Y * (Y - d)) else 0
    greenwood <- greenwood + increment
    se <- if (S > 0 && S < 1) S * sqrt(greenwood) else NA_real_
    lower <- upper <- NA_real_
    if (S > 0 && S < 1) {
      transformed <- log(-log(S))
      transformed_se <- sqrt(greenwood) / abs(log(S))
      lower <- exp(-exp(transformed + z * transformed_se))
      upper <- exp(-exp(transformed - z * transformed_se))
    }
    grid[i, ] <- c(times[i], Y, d, S, increment, se, lower, upper)
  }
  grid
}

.sv_query <- function(records, curve, times, support, reverse) {
  at <- findInterval(times, curve$time)
  S <- c(1, curve$survival)[at + 1L]
  # sts generate se(s) forward-fills missing event values (sts.ado:406).
  # Keep the mathematical event grid unchanged; this native query projection
  # deliberately retains a previous positive SE when terminal S reaches zero.
  native_se <- curve$se
  for (i in seq_along(native_se)) if (i > 1L && is.na(native_se[i])) native_se[i] <- native_se[i - 1L]
  se <- c(0, native_se)[at + 1L]
  se[at == 0L & times >= min(records$exit)] <- NA_real_
  lower <- c(1, curve$lower)[at + 1L]
  upper <- c(1, curve$upper)[at + 1L]
  lower[at == 0L & times >= min(records$exit)] <- NA_real_
  upper[at == 0L & times >= min(records$exit)] <- NA_real_
  if (reverse) {
    S <- 1 - S
    tmp <- lower; lower <- 1 - upper; upper <- 1 - tmp
  }
  data.frame(request = seq_along(times), time = times, probability = S, se = se,
    lower = lower, upper = upper, risk = vapply(times, function(t) .sv_risk(records, t), 0),
    beyond_support = times > support)
}

# stci Findptl:531-545 compares single-precision values, not an R quantile plateau.
.sv_float <- function(x) readBin(writeBin(as.double(x), raw(), size = 4L), numeric(),
                                n = length(x), size = 4L)

.sv_median <- function(curve, declared_fweight, subject_N) {
  missing <- list(estimate = NA_real_, lower = NA_real_, upper = NA_real_,
    state = "notest", reason = if (declared_fweight) "native_stci_declared_fweight_refusal" else "not_reached")
  # stci.ado:28-31 refuses st_wv; survtab.ado:449-466 captures missing medians.
  if (declared_fweight || !nrow(curve)) return(missing)
  # Root-authenticated SVP002: null stcox extraction for one subject gives NR.
  if (subject_N == 1) {
    missing$reason <- "native_stci_single_subject_unavailable"
    return(missing)
  }
  crossing <- function(probability) {
    hit <- which(!is.na(probability) & .sv_float(probability) <= .sv_float(0.5))
    if (length(hit)) curve$time[hit[1L]] else NA_real_
  }
  estimate <- crossing(curve$survival)
  list(estimate = estimate, lower = crossing(curve$lower), upper = crossing(curve$upper),
    state = if (is.na(estimate)) "notest" else "est", reason = if (is.na(estimate)) "not_reached" else "",
    method = "first_float_half_crossing; root_actual_SVP001_005_and_SV009_010")
}
