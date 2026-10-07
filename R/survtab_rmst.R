# Royston/Parmar2013 eq1; native survtab.ado1219-1258 squared-tail Greenwood.
# Independent implementation; no GPL reference-software source is copied.
.sv_rmst <- function(curve, tau, level) {
  events <- curve[curve$time <= tau, , drop = FALSE]
  if (!nrow(events)) return(list(estimate = tau, variance = 0, se = 0, lower = tau, upper = tau))
  width <- diff(c(events$time, tau))
  areas <- events$survival * width
  remaining <- rev(cumsum(rev(areas)))
  estimate <- events$time[1L] + sum(areas)
  # A terminal Y=d event has S=0 and exactly zero tail, so no 0*Inf term.
  terminal <- events$risk == events$events
  if (any(remaining[terminal] != 0)) .sv_abort("Terminal event has a nonzero restricted tail.", "tabtools_error_survival_risk")
  variance <- sum(events$increment * remaining^2)
  se <- sqrt(variance)
  z <- stats::qnorm((1 + level / 100) / 2)
  list(estimate = estimate, variance = variance, se = se, lower = estimate - z * se,
       upper = estimate + z * se, event_times = events$time, tail_areas = remaining,
       variance_method = "Greenwood_squared_tail; independent_subjects")
}

# Independent-arm contrast: Royston/Parmar2013 eq6-7; native first minus second.
.sv_contrast <- function(estimates, ses, level) {
  value <- estimates[1L] - estimates[2L]
  se <- sqrt(sum(ses^2))
  z <- stats::qnorm((1 + level / 100) / 2)
  list(estimate = value, se = se, lower = if (se > 0) value - z * se else NA_real_,
    upper = if (se > 0) value + z * se else NA_real_,
    p = if (se > 0) 2 * stats::pnorm(-abs(value / se)) else NA_real_,
    direction = "group_1_minus_group_2", state = if (se > 0) "est" else "notest")
}
