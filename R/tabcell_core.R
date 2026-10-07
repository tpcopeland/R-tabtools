# Pure rendering engine for the seven tabcell forms. Constructors provide
# source-owned estimates on the publication scale; this engine never fits a
# model or writes a destination.
.tc_abort <- function(message, subclass = "domain") {
  cli::cli_abort(message,
                 class = c(paste0("tabtools_error_cell_", subclass),
                           "tabtools_error_cell"), call = NULL)
}

.tc_recycle <- function(values) {
  if (!length(values) || is.null(names(values)) || anyDuplicated(names(values))) {
    .tc_abort("Cell inputs must have distinct names.", "source")
  }
  sizes <- vapply(values, length, integer(1))
  if (any(sizes == 0L) || any(!vapply(values, function(x) is.numeric(x) && is.null(dim(x)), logical(1)))) {
    .tc_abort("Every cell input must be a nonempty numeric vector.", "source")
  }
  size <- max(sizes)
  if (any(!sizes %in% c(1L, size))) {
    .tc_abort("Cell inputs must have equal lengths; only scalars recycle.", "length")
  }
  lapply(values, function(x) rep_len(as.double(x), size))
}

# Thulin (2014), section 2.1 equations 3-4. Boundaries skip the undefined
# beta shape; the upper tail uses lower.tail=FALSE to avoid subtracting a
# probability close to one.
.tc_binomial_limits <- function(successes, total, level) {
  alpha2 <- (1 - level) / 2
  lower <- upper <- rep(NA_real_, length(successes))
  zero <- successes == 0
  all <- successes == total
  lower[zero] <- 0
  upper[all] <- 1
  lower[!zero] <- stats::qbeta(alpha2, successes[!zero],
                              total[!zero] - successes[!zero] + 1)
  upper[!all] <- stats::qbeta(alpha2, successes[!all] + 1,
                             total[!all] - successes[!all], lower.tail = FALSE)
  cbind(lb = lower * 100, ub = upper * 100)
}

# Stata [R] ci, Poisson mean: equal-tailed Poisson inversion. The gamma
# quantiles are the same inversion through the Poisson/gamma tail identity.
# Log-rate Wald uses strate's normal critical value; both methods use exact
# limits at zero events. Neither computes inference for protected cells.
.tc_rate_limits <- function(events, exposure, per, level, method) {
  alpha2 <- (1 - level) / 2
  rate <- events / exposure * per
  zero <- events == 0
  lower <- upper <- rep(NA_real_, length(events))
  lower[zero] <- 0
  upper[zero] <- -log(alpha2) / exposure[zero] * per
  if (method == "exact") {
    lower[!zero] <- stats::qgamma(alpha2, shape = events[!zero]) /
      exposure[!zero] * per
    upper[!zero] <- stats::qgamma(alpha2, shape = events[!zero] + 1,
                                 lower.tail = FALSE) / exposure[!zero] * per
  } else {
    width <- stats::qnorm(alpha2, lower.tail = FALSE) / sqrt(events[!zero])
    lower[!zero] <- rate[!zero] * exp(-width)
    upper[!zero] <- rate[!zero] * exp(width)
  }
  cbind(estimate = rate, lb = lower, ub = upper)
}

.tc_render <- function(form, values, numeric_format, count_format,
                       percent_format, level = 0.95, ci = NULL,
                       mincell = 0L, nocount = FALSE, per = NULL,
                       missing_text = NULL, missing_given = FALSE,
                       pdp = 3L, highpdp = 2L, pstyle = "table") {
  required <- switch(form, est = c("b", "ll", "ul"), p = "p", n = "n",
                     np = c("n", "d"), enp = c("e", "n"),
                     iqr = c("median", "q1", "q3"), rate = c("e", "pt"))
  if (is.null(required) || !identical(names(values), required)) {
    .tc_abort("The form and named cell inputs do not agree.", "source")
  }
  values <- .tc_recycle(values)
  size <- length(values[[1L]])
  bad <- Reduce(`|`, lapply(values, function(x) !is.finite(x)))
  zero_denominator <- rep(FALSE, size)
  if (form == "rate" || (form == "np" && nocount)) {
    zero_denominator <- !bad & values[[1L]] == 0 & values[[2L]] == 0
    bad <- bad | zero_denominator
  }
  if (any(bad) && !missing_given) {
    .tc_abort("A cell is missing, non-finite, or noncomputable; supply {.arg missing} text.",
              "missing")
  }
  ok <- !bad
  if (form %in% c("est", "iqr") && any(values[[2L]][ok] > values[[3L]][ok])) {
    .tc_abort("A lower limit exceeds its upper limit.", "interval")
  }
  if (form == "iqr" && any(values$median[ok] < values$q1[ok] |
                            values$median[ok] > values$q3[ok])) {
    .tc_abort("A median lies outside its quartiles.", "interval")
  }
  if (form == "p" && any(values$p[ok] < 0 | values$p[ok] > 1)) {
    .tc_abort("P-values must lie in [0, 1].")
  }
  if (form %in% c("n", "np", "enp", "rate")) {
    if (any(values[[1L]][ok] < 0)) .tc_abort("Counts must be nonnegative.")
    if (length(values) > 1L && any(values[[2L]][ok] < 0)) {
      .tc_abort("Denominators and exposure must be nonnegative.")
    }
    if (form %in% c("np", "enp") && any(values[[1L]][ok] > values[[2L]][ok])) {
      .tc_abort("A count exceeds its total.")
    }
    if (form == "rate" && any(values$e[ok] > 0 & values$pt[ok] == 0)) {
      .tc_abort("Positive events require positive exposure.")
    }
    if (identical(ci, "exact") && form %in% c("np", "rate")) {
      exact_inputs <- if (form == "np") values else values["e"]
      if (any(vapply(exact_inputs, function(x) any(x[ok] != floor(x[ok])), logical(1)))) {
        .tc_abort("Exact intervals require whole-number counts.")
      }
    }
  }
  primary <- total_mask <- rep(FALSE, size)
  if (form %in% c("n", "np", "enp", "rate")) {
    primary <- ok & values[[1L]] >= 1 & values[[1L]] < mincell
    if (form == "enp") total_mask <- ok & values$n >= 1 & values$n < mincell
  }
  masked <- primary | total_mask
  show <- ok & !masked
  text <- rep("", size)
  fields <- c("estimate", "lb", "ub", "p", "count", "denominator",
              "events", "exposure", "pct", "median", "q1", "q3")
  publication <- as.data.frame(setNames(rep(list(rep(NA_real_, size)), length(fields)),
                                       fields), check.names = FALSE)
  numeric <- function(x) .tt_format_numeric(x, numeric_format)
  counts <- function(x) .tt_format_numeric(x, count_format)
  percent <- function(x) .tt_format_numeric(x, percent_format)
  sep <- numeric_format$sep
  if (form == "est") {
    text[show] <- paste0(numeric(values$b[show]), " (", numeric(values$ll[show]),
                         sep, numeric(values$ul[show]), ")")
    publication[show, c("estimate", "lb", "ub")] <-
      as.data.frame(values[c("b", "ll", "ul")])[show, , drop = FALSE]
  } else if (form == "iqr") {
    text[show] <- paste0(numeric(values$median[show]), " (", numeric(values$q1[show]),
                         sep, numeric(values$q3[show]), ")")
    publication[show, c("median", "q1", "q3")] <-
      as.data.frame(values)[show, , drop = FALSE]
  } else if (form == "p") {
    ptext <- format_p(values$p[show], pdp, highpdp)
    if (pstyle != "table") {
      letter <- if (pstyle == "Pfootnote") "P" else "p"
      ptext <- .tt_p_prose(ptext, letter)
    }
    text[show] <- ptext
    publication$p[show] <- values$p[show]
  } else if (form == "n") {
    text[show] <- counts(values$n[show])
    publication$count[show] <- values$n[show]
  } else if (form %in% c("np", "enp")) {
    numerator <- values[[1L]]
    denominator <- values[[2L]]
    eligible <- show & denominator > 0
    pct <- numerator[eligible] / denominator[eligible] * 100
    if (any(!is.finite(pct))) .tc_abort("A percentage cannot be computed.", "interval")
    publication$pct[eligible] <- pct
    publication$denominator[show] <- denominator[show]
    if (form == "enp") {
      text[show] <- paste0(counts(numerator[show]), "/", counts(denominator[show]))
      text[eligible] <- paste0(text[eligible], " (", percent(pct), ")")
      publication$events[show] <- numerator[show]
    } else {
      if (!nocount) publication$count[show] <- numerator[show]
      if (!nocount) text[show] <- counts(numerator[show])
      if (is.null(ci)) {
        text[eligible] <- if (nocount) percent(pct) else
          paste0(text[eligible], " (", percent(pct), ")")
      } else {
        bounds <- .tc_binomial_limits(numerator[eligible], denominator[eligible], level)
        if (any(!is.finite(bounds))) .tc_abort("A binomial interval cannot be computed.", "interval")
        publication[eligible, c("lb", "ub")] <- bounds
        interval <- paste0(percent(bounds[, "lb"]), sep, percent(bounds[, "ub"]))
        text[eligible] <- if (nocount) paste0(percent(pct), " (", interval, ")") else
          paste0(text[eligible], " (", percent(pct), "; ", interval, ")")
      }
    }
  } else if (form == "rate") {
    bounds <- .tc_rate_limits(values$e[show], values$pt[show], per, level, ci)
    if (any(!is.finite(bounds))) .tc_abort("A rate or interval cannot be computed.", "interval")
    publication[show, c("estimate", "lb", "ub")] <- bounds
    publication$events[show] <- values$e[show]
    publication$exposure[show] <- values$pt[show]
    text[show] <- paste0(numeric(bounds[, "estimate"]), " (", numeric(bounds[, "lb"]),
                         sep, numeric(bounds[, "ub"]), ")")
  }
  if (any(masked)) {
    token <- paste0("<", mincell)
    text[masked] <- if (form == "rate" || (form == "np" && nocount)) "\u2013" else token
    if (form == "enp") {
      event_only <- primary & !total_mask
      text[event_only] <- paste0(token, "/", counts(values$n[event_only]))
      publication$denominator[event_only] <- values$n[event_only]
    }
  }
  if (any(bad)) text[bad] <- missing_text
  # A leaf retains no protected numerator or reconstructive percentage/rate
  # in its analytical companions. An event-only enp mask keeps its public total.
  raw_inputs <- as.data.frame(values, check.names = FALSE)
  if (any(masked)) {
    raw_inputs[masked, 1L] <- NA_real_
    if (form == "enp") raw_inputs[total_mask, "n"] <- NA_real_
  }
  status <- rep(if (form %in% c("est", "rate") || !is.null(ci)) "est" else "notest", size)
  raw_status <- status
  if (form == "np" && !is.null(ci)) {
    no_interval <- ok & !masked & values$d == 0
    raw_status[no_interval] <- status[no_interval] <- "notest"
  }
  raw_status[bad] <- "empty"
  status[bad] <- "empty"
  status[masked] <- "masked"
  list(text = text, publication = publication, raw_inputs = raw_inputs,
       status = status, raw_status = raw_status, missing = bad,
       missing_reason = ifelse(zero_denominator, "zero_denominator",
                                ifelse(bad, "nonfinite_input", "")),
       masked = masked, mask_reason = ifelse(total_mask, "total",
                                              ifelse(primary, "count", "")),
       counts = list(N = as.integer(sum(!bad)), N_missing = as.integer(sum(bad)),
                     N_selected = as.integer(size)),
       smallcells = list(threshold = as.integer(mincell), mode = "primary",
                         n_masked = as.integer(sum(masked)), n_linked = 0L))
}
