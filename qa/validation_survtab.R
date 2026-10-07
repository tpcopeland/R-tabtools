library(tabtools)
source("qa/tools/qa_result.R")
receipt <- new.env(parent = emptyenv())
receipt$results <- list()
work <- function() {
  if (!qa_has("survival")) {
    qa_skip("KM/Greenwood/log-rank and IJ diagnostic", "survival is not available")
  } else {
    specifications <- list(
      right = data.frame(id = 1:4, entry = 0, exit = 1:4, event = c(1, 1, 0, 0), frequency = 1),
      entry = data.frame(id = 1:3, entry = c(0, 2, 0), exit = c(2, 3, 4), event = c(1, 1, 0), frequency = 1),
      split = data.frame(id = c(1, 2, 3, 4, 4), entry = c(0, 0, 0, 0, 2),
        exit = c(1, 2, 3, 2, 4), event = c(1, 1, 0, 0, 0), frequency = 1),
      frequency = data.frame(id = 1:4, entry = 0, exit = 1:4, event = c(1, 1, 0, 0), frequency = c(10, 1, 10, 1)))
    for (name in names(specifications)) {
      d <- specifications[[name]]
      actual <- survtab(d, "exit", "event", sort(unique(d$exit[d$event == 1])),
        entry = "entry", id = "id", fweight = if (name == "frequency") "frequency" else NULL, rmst = 3, level = 90)
      response <- survival::Surv(d$entry, d$exit, d$event)
      fit <- survival::survfit(response ~ 1, data = d, id = d$id, weights = d$frequency,
        robust = FALSE, timefix = FALSE, stype = 1, ctype = 1, conf.type = "log-log", conf.int = 0.9)
      summary <- summary(fit, times = actual$meta$survival$requested_times, extend = TRUE, dosum = FALSE)
      query <- actual$meta$survival$raw$queries[[1L]]
      qa_check(paste(name, "full event-time KM against explicitly nonrobust survfit"),
        isTRUE(all.equal(query$probability, as.numeric(summary$surv), tolerance = 1e-12)))
      qa_check(paste(name, "Greenwood S-scale SE (no second multiplication)"),
        isTRUE(all.equal(query$se, as.numeric(summary$std.err), tolerance = 1e-12)))
      qa_check(paste(name, "log-log bands"),
        isTRUE(all.equal(cbind(query$lower, query$upper), cbind(summary$lower, summary$upper), tolerance = 1e-12)))
      curve <- actual$meta$survival$raw$curves[[1L]]
      qa_check(paste(name, "subject-tagged exact risk/death grid"),
        isTRUE(all.equal(cbind(curve$risk, curve$events), cbind(summary$n.risk, summary$n.event), tolerance = 1e-12)))
      # Independent integration of the measured curve plus dense Greenwood
      # covariance, instead of the production remaining-tail implementation.
      at <- which(fit$time <= 3)
      boundaries <- c(0, fit$time[at], 3)
      height <- c(1, fit$surv[at])
      width <- diff(boundaries)
      area <- sum(width * height)
      increment <- ifelse(fit$n.risk[at] > fit$n.event[at],
        fit$n.event[at] / (fit$n.risk[at] * (fit$n.risk[at] - fit$n.event[at])), 0)
      cumulative <- c(0, cumsum(increment))
      covariance <- outer(seq_along(height), seq_along(height), function(i, j)
        height[i] * height[j] * cumulative[pmin(i, j)])
      variance <- sum(covariance * tcrossprod(width))
      qa_check(paste(name, "independent dense-covariance RMST integration"),
        isTRUE(all.equal(c(actual$stored$rmst_1, actual$stored$rmst_se_1^2), c(area, variance), tolerance = 1e-12)))
      receipt$results[[name]] <- list(actual = actual, independent_survfit = fit,
                              independent_RMST = area, independent_variance = variance)
    }
    d <- data.frame(exit = 1:4, event = c(1, 1, 0, 0), group = c(1, 1, 2, 2))
    actual <- survtab(d, "exit", "event", 1, by = "group")
    # These exact integer times need no near-time correction. Omit timefix:
    # survival 3.8.6 forwards an explicitly supplied value to model.frame.
    independent <- survival::survdiff(survival::Surv(exit, event) ~ group, d, rho = 0)
    qa_check("ordinary log-rank against actual right-only unweighted survdiff",
      isTRUE(all.equal(actual$stored$logrank_chi2, independent$chisq, tolerance = 1e-12)))
    # IJ is a separately named diagnostic, not native delayed-entry variance.
    ij <- function(d, tau) {
      ids <- unique(d$id); influence <- numeric(nrow(d)); derivative <- numeric(nrow(d)); S <- 1
      events <- sort(unique(d$exit[d$event == 1 & d$exit <= tau]))
      for (i in seq_along(events)) {
        risk <- as.numeric(d$entry < events[i] & d$exit >= events[i])
        death <- as.numeric(d$event == 1 & d$exit == events[i])
        hazard <- sum(death) / sum(risk)
        gradient <- (death - risk * hazard) / sum(risk)
        influence <- influence * (1 - hazard) - S * gradient
        S <- S * (1 - hazard)
        next_time <- if (i < length(events)) events[i + 1L] else tau
        derivative <- derivative + (next_time - events[i]) * influence
      }
      collapsed <- vapply(ids, function(id) sum(derivative[d$id == id]), 0)
      list(variance = sum(collapsed^2), derivative = collapsed, method = "IJ_diagnostic")
    }
    right_ij <- ij(specifications$right, 3)
    qa_check("independent ordinary recursive IJ hand variance", isTRUE(all.equal(right_ij$variance, 11/64, tolerance = 1e-14)))
    receipt$results$IJ_right <- right_ij
    receipt$results$IJ_entry <- ij(specifications$entry, 3)
    qa_check("delayed-entry IJ diagnostic is named separately from native Greenwood",
      identical(receipt$results$IJ_entry$method, "IJ_diagnostic") && is.finite(receipt$results$IJ_entry$variance))
  }
  if (!qa_has("survRM2")) {
    qa_skip("ordinary two-arm RMST", "optional survRM2 is not available")
  } else {
    d <- data.frame(exit = rep(1:4, 2), event = c(1, 1, 0, 0, 0, 1, 0, 0), arm = rep(0:1, each = 4))
    actual <- survtab(d, "exit", "event", c(1, 2, 3), by = "arm", rmst = 3, difference = TRUE)
    independent <- survRM2::rmst2(d$exit, d$event, d$arm, tau = 3, alpha = 0.05)
    qa_check("survRM2 raw arm measure identities", identical(names(independent$RMST.arm0$rmst)[1:2],
      c("Est.", "se")) && identical(names(independent$RMST.arm1$rmst)[1:2], c("Est.", "se")))
    qa_check("survRM2 both raw arm RMST/SE", isTRUE(all.equal(
      c(actual$stored$rmst_1, actual$stored$rmst_se_1, actual$stored$rmst_2, actual$stored$rmst_se_2),
      unname(c(independent$RMST.arm0$rmst[1L], independent$RMST.arm0$rmst[2L],
        independent$RMST.arm1$rmst[1L], independent$RMST.arm1$rmst[2L])), tolerance = 1e-12)))
    comparison <- independent$unadjusted.result[1L, ]  # package arm1 minus arm0
    qa_check("survRM2 contrast direction/CI/p", isTRUE(all.equal(
      c(actual$stored$rmst_diff, actual$stored$rmst_diff_lb, actual$stored$rmst_diff_ub, actual$stored$rmst_diff_p),
      c(-comparison[1L], -comparison[3L], -comparison[2L], comparison[4L]), check.attributes = FALSE, tolerance = 1e-12)))
    receipt$results$survRM2 <- list(actual = actual, independent = independent,
                           version = as.character(utils::packageVersion("survRM2")))
  }
}
tryCatch(work(), error = function(e) {
  receipt$results$error <- list(class = class(e), message = conditionMessage(e), call = conditionCall(e))
  qa_check("unexpected survival validation error", FALSE)
})
destination <- Sys.getenv("TABTOOLS_SURVTAB_VALIDATION_RESULTS")
if (nzchar(destination)) saveRDS(list(results = receipt$results, executed = qa_state$executed,
  failed = qa_state$failed, skipped = qa_state$skipped), destination, version = 3)
qa_done("validation_survtab.R")
