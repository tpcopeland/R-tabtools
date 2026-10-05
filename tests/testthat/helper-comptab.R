# Fixtures for the comptab()/hrcomptab() unit tests: hazard-ratio models as
# regtab() data frames (the task 5.8 escape hatch, so no Suggests are
# needed), and stratetab() rate tables from strate-shaped blocks.

# One Cox-style model: a plain "Treated" row, a factor "Dose" with the
# given levels (reference `ref`), and "Age". `drop` removes levels (a model
# that lacks an exposure level).
ct_model <- function(outcome, shift = 0, levels = c("None", "Low", "High"), ref = 1, drop = character(),
                     label = NULL, scale = "HR", level = NULL) {
  k <- length(levels)
  est <- c(0.8 + shift, rep(1, k), 1.01)
  est[1 + seq_len(k)][-ref] <- 1.2 + shift + seq_len(k - 1) / 10
  lo <- est * 0.8
  hi <- est * 1.25
  isref <- seq_len(k) == ref
  lo[1 + which(isref)] <- NA
  hi[1 + which(isref)] <- NA
  p <- c(0.13, ifelse(isref, NA, 0.02 * seq_len(k)), 0.0004)
  d <- data.frame(term = c("treat", paste0("dose", seq_len(k)), "age"),
                  variable = c("treat", rep("dose", k), "age"),
                  var_label = c("Treated", rep("Dose", k), "Age"),
                  var_type = c("continuous", rep("categorical", k), "continuous"),
                  label = c("Treated", levels, "Age"),
                  reference_row = c(NA, isref, NA),
                  estimate = est, conf.low = lo, conf.high = hi, p.value = p,
                  stringsAsFactors = FALSE)
  if (length(drop)) d <- d[!(d$variable == "dose" & d$label %in% drop), , drop = FALSE]
  attr(d, "effect_scale") <- scale
  attr(d, "outcome_id") <- outcome
  attr(d, "model_id") <- paste("cox", outcome, label)
  if (!is.null(level)) attr(d, "conf.level") <- level
  d
}

# A regtab table of one model per outcome.
ct_models <- function(outcomes = c("death", "relapse"), labels = c("Death", "Relapse"), ...) {
  fits <- lapply(seq_along(outcomes), function(i) ct_model(outcomes[i], shift = (i - 1) / 10, label = labels[i], ...))
  regtab(fits, models = labels)
}

# A strate-shaped block.
ct_block <- function(cats, d, y, level = 95) {
  rate <- d / y
  b <- data.frame(group = cats, `_D` = d, `_Y` = y, `_Rate` = rate, `_Lower` = rate * 0.7,
                  `_Upper` = rate * 1.4, check.names = FALSE)
  attr(b[["_Lower"]], "label") <- paste0("Lower ", level, "% confidence limit")
  attr(b[["_Upper"]], "label") <- paste0("Upper ", level, "% confidence limit")
  b
}

# Rates for two outcomes x two exposures ("Treated": No/Yes; "Dose":
# None/Low/High), in stratetab's block order.
ct_rates <- function(outcomes = c("death", "relapse"), level = 95, dose = c("None", "Low", "High"), ...) {
  bl <- list()
  for (o in seq_along(outcomes)) bl[[length(bl) + 1L]] <- ct_block(c("No", "Yes"), c(40, 30) + o, c(5000, 4800), level)
  for (o in seq_along(outcomes)) {
    bl[[length(bl) + 1L]] <- ct_block(dose, seq(40, by = -8, length.out = length(dose)) + o,
                                      seq(5000, by = -1500, length.out = length(dose)), level)
  }
  stratetab(bl, outcomes = length(outcomes), outlabels = tools::toTitleCase(outcomes), outcomeids = outcomes,
            explabels = c("Treated", "Dose"), ...)
}
