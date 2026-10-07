# Saturated Poisson score sandwich, pinned ratetab.ado:28-32,348-391.
# Stata [R] poisson (2025), p.8; [P] _robust (2025), pp.23-24:
# weighted exposure-offset scores, HC0 meat and G/(G-1), no lm HC1 factor.
.rat_cluster <- function(s, g, o, D, Y, dmin, level, per, pyscale) {
  gr <- s$groups[[g]]
  eligible_levels <- which(D >= dmin)
  fitted <- s$use & s$group_ok[[g]] & s$data[[s$exposure[o]]] > 0 &
    !is.na(gr$ids) & gr$ids %in% eligible_levels
  lower <- upper <- rep(NA_real_, length(D))
  result <- list(lower = lower, upper = upper, G = NA_real_, covariance = NULL,
                 scores = NULL, cluster_ids = NULL, level_ids = eligible_levels,
                 fitted_row_ids = s$row_id[fitted], fitted_n = sum(fitted),
                 fitted_weight_n = sum(s$w[fitted]),
                 reason = rep("not_fitted", length(D)))
  if (!any(fitted)) return(result)
  cv <- s$data[[s$cluster]][fitted]
  cluster_ids <- unique(cv)
  cid <- match(cv, cluster_ids)
  j <- match(gr$ids[fitted], eligible_levels)
  G <- length(cluster_ids)
  J <- length(eligible_levels)
  dc <- yc <- matrix(0, G, J)
  weighted_d <- s$data[[s$events[o]]][fitted] * s$w[fitted]
  weighted_y <- s$data[[s$exposure[o]]][fitted] * s$w[fitted]
  for (i in seq_along(cid)) {
    dc[cid[i], j[i]] <- dc[cid[i], j[i]] + weighted_d[i]
    yc[cid[i], j[i]] <- yc[cid[i], j[i]] + weighted_y[i]
  }
  rate <- D[eligible_levels] / Y[eligible_levels]
  scores <- dc - sweep(yc, 2L, rate, `*`)
  # The default _robust centers cluster scores. At the saturated solution
  # their mean is mathematically zero; retain the explicit centering operation.
  centered <- sweep(scores, 2L, colMeans(scores), `-`)
  standardized <- sweep(centered, 2L, D[eligible_levels], `/`)
  V <- if (G == 1L) matrix(0, J, J) else crossprod(standardized) * (G / (G - 1))
  if (any(!is.finite(V))) .rat_abort("The clustered score covariance is not finitely representable.", "inference")
  dimnames(V) <- list(paste0("level", eligible_levels), paste0("level", eligible_levels))
  se <- sqrt(diag(V))
  z <- stats::qnorm((1 - level) / 2, lower.tail = FALSE)
  b <- log(D[eligible_levels]) - log(Y[eligible_levels])
  valid <- G > 1L & is.finite(se) & se > 0
  lower[eligible_levels[valid]] <- exp(b[valid] - z * se[valid]) * pyscale * per
  upper[eligible_levels[valid]] <- exp(b[valid] + z * se[valid]) * pyscale * per
  lower[!is.finite(lower)] <- upper[!is.finite(upper)] <- NA_real_
  reason <- result$reason
  reason[eligible_levels] <- if (G == 1L) "single_cluster_native_convention" else "zero_score_variance"
  reason[eligible_levels[valid]] <- "finite_positive_variance"
  incomplete <- valid & (!is.finite(lower[eligible_levels]) | !is.finite(upper[eligible_levels]))
  reason[eligible_levels[incomplete]] <- "nonfinite_limits"
  result$lower <- lower
  result$upper <- upper
  result$G <- as.numeric(G)
  result$covariance <- V
  result$scores <- scores
  result$cluster_ids <- cluster_ids
  result$cluster_events <- dc
  result$cluster_exposure <- yc
  result$reason <- reason
  result
}

.rat_estimates <- function(s, ci, level, per, pyscale, dmin) {
  cells <- raw <- diagnostic <- fit_samples <- list()
  cats <- lapply(s$groups, `[[`, "labels")
  clusters <- matrix(NA_real_, length(s$by), length(s$events),
                     dimnames = list(s$by, paste0("c", seq_along(s$events))))
  n_zero <- n_noci <- n_nopt <- n_maskfit <- 0L
  for (g in seq_along(s$by)) {
    gr <- s$groups[[g]]
    section <- s$use & s$group_ok[[g]]
    for (o in seq_along(s$events)) {
      D <- Y <- numeric(length(gr$labels))
      for (l in seq_along(D)) {
        in_level <- section & !is.na(gr$ids) & gr$ids == l
        D[l] <- sum(s$data[[s$events[o]]][in_level] * s$w[in_level])
        Y[l] <- sum(s$data[[s$exposure[o]]][in_level] * s$w[in_level])
      }
      if (any(!is.finite(D) | !is.finite(Y))) .rat_abort("Aggregated events or person-time overflowed.", "domain")
      display_y <- Y / pyscale
      rate <- lower <- upper <- rep(NA_real_, length(D))
      has_time <- Y > 0 & display_y > 0
      rate[has_time] <- D[has_time] / display_y[has_time] * per
      reason <- ifelse(Y == 0, "no_person_time", "finite_inference")
      if (ci == "cluster") {
        fit <- .rat_cluster(s, g, o, D, Y, dmin, level, per, pyscale)
        lower <- fit$lower
        upper <- fit$upper
        reason <- fit$reason
        reason[Y == 0] <- "no_person_time"
        clusters[g, o] <- fit$G
        diagnostic[[paste(o, g)]] <- fit
        n_noci <- n_noci + sum(D >= dmin & (is.na(lower) | is.na(upper)))
        n_maskfit <- n_maskfit + sum(D > 0 & D < dmin)
        fit_samples[[paste(o, g)]] <- .tt_sample_population(paste0("fit", g, "_", o), "ratetab", "model",
          variable = s$by[g], model = o, spec = g,
          weight_type = s$weight_type,
          values = list(input_n = sum(section), eligible_n = sum(section),
                        observed_n = sum(section), used_n = fit$fitted_n,
                        fitted_n = fit$fitted_n, excluded_n = sum(section) - fit$fitted_n,
                        reported_n = fit$fitted_weight_n),
          bases = list(reported_n = "frequency-expanded fitted records"),
          exclusions = .tt_sample_exclusion("eligible_to_used", "zero time or level below fitting threshold",
            sum(section) - fit$fitted_n, "section original records"))
      } else {
        positive <- D > 0 & has_time
        if (any(positive)) {
          # Shared, reviewed leaf method; no inference is borrowed from labels.
          lim <- .tc_rate_limits(D[positive], display_y[positive], per, level, ci)
          lower[positive] <- lim[, "lb"]
          upper[positive] <- lim[, "ub"]
        }
      }
      zero <- D == 0 & has_time
      # Pinned ratetab.ado:433-437 explicitly completes zero events under
      # every method, including cells not included in clustered fitting.
      lower[zero] <- 0
      upper[zero] <- -log((1 - level) / 2) / display_y[zero] * per
      reason[zero] <- "exact_zero_completion"
      n_zero <- n_zero + sum(zero)
      n_nopt <- n_nopt + sum(Y == 0)
      rate[!is.finite(rate)] <- NA_real_
      lower[!is.finite(lower)] <- NA_real_
      upper[!is.finite(upper)] <- NA_real_
      key <- paste(o, g)
      category_data <- s$data[rep(NA_integer_, length(D)), s$by[g], drop = FALSE]
      category_data[[1L]] <- gr$values
      cells[[key]] <- list(D = stata_macro_num(D), Y = stata_macro_num(display_y),
        Rate = rate, Lower = lower, Upper = upper, raw_Y = Y,
        ci_method = rep(ci, length(D)), category_data = category_data)
      raw[[key]] <- data.frame(outcome = o, group = g, level = seq_along(D),
        events = D, persontime = display_y, rate = rate, lb = lower, ub = upper,
        source_persontime = Y, inference_reason = reason, stringsAsFactors = FALSE)
    }
  }
  if (!any(vapply(cells, function(x) any(x$raw_Y > 0), logical(1)))) {
    .rat_abort("No person-time is available in any table cell.", "domain")
  }
  if (length(fit_samples)) {
    sample <- .tt_sample_bind(c(list(s$sample), fit_samples),
      prefixes = c("source", paste0("fit", seq_along(fit_samples))),
      commands = rep("ratetab", length(fit_samples) + 1L))
  } else sample <- s$sample
  list(data = list(cats = cats, cell = cells), raw = do.call(rbind, raw),
       clusters = clusters, diagnostic = diagnostic, sample = sample,
       N_zero = n_zero, N_noci = n_noci, N_nopt = n_nopt, N_maskfit = n_maskfit)
}
