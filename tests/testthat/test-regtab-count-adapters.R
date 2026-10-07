# Formula/model-frame environments contain the changing test execution scope.
# Compare complete copied model/frame structure after stabilizing retained environments;
# assert the original environment identity separately.
count_adapter_snapshot <- function(x) {
  out <- x
  if (!is.null(attr(out, "terms"))) {
    terms <- attr(out, "terms")
    attr(terms, ".Environment") <- baseenv()
    attr(out, "terms") <- terms
  }
  if (is.list(out) && !is.null(out$terms)) {
    terms <- out$terms
    attr(terms, ".Environment") <- baseenv()
    out$terms <- terms
  }
  if (is.list(out) && inherits(out$formula, "formula")) {
    formula <- out$formula
    attr(formula, ".Environment") <- baseenv()
    out$formula <- formula
  }
  unserialize(serialize(out, NULL))
}

test_that("ordinary Cox factor identities use the same checked codes as row extraction", {
  d <- data.frame(time = seq_len(40), event = rep(c(1, 0, 1, 1, 0), 8),
    dosecat = factor(rep(c("No HRT", "Low dose", "Medium dose", "High dose"), 10),
      levels = c("No HRT", "Low dose", "Medium dose", "High dose")),
    x = sin(seq_len(40)))
  attr(d$dosecat, "labels") <- c("No HRT" = 0, "Low dose" = 1, "Medium dose" = 2, "High dose" = 3)
  fit <- survival::coxph(survival::Surv(time, event) ~ dosecat + x, d, ties = "breslow")
  before <- count_adapter_snapshot(fit)
  formula_environment <- attr(fit$terms, ".Environment")
  retained_formula_environment <- attr(fit$formula, ".Environment")
  table <- regtab(fit, stats = NULL)
  rows <- table$meta$regtab_rows
  keys <- c("0.dosecat", "1.dosecat", "2.dosecat", "3.dosecat")
  index <- match(keys, rows$key)
  expect_false(anyNA(index))
  expect_identical(rows$status[index], c("ref", "est", "est", "est"))
  expect_true(all(nzchar(rows$level_identity[index])))
  expect_length(unique(rows$level_identity[index]), 4L)
  expect_true(all(is.finite(rows$estimate[index[-1L]])))
  expect_true(identical(count_adapter_snapshot(fit), before, num.eq = FALSE, single.NA = FALSE))
  expect_identical(attr(fit$terms, ".Environment"), formula_environment)
  expect_identical(attr(fit$formula, ".Environment"), retained_formula_environment)
})

test_that("dropped levels and reversed label codes retain the genuine fit contrast base", {
  d <- data.frame(time = seq_len(48), event = rep(c(1, 0, 1, 1), 12),
    dosecat = factor(rep(c("Low", "Medium", "High"), 16),
      levels = c("Low", "Unused", "Medium", "High")), x = sin(seq_len(48)))
  attr(d$dosecat, "labels") <- c(Low = 3, Unused = 2, Medium = 1, High = 0)
  fit <- survival::coxph(survival::Surv(time, event) ~ dosecat + x, d, ties = "breslow")
  table <- regtab(fit, stats = NULL)
  rows <- table$meta$regtab_rows
  index <- match(c("3.dosecat", "1.dosecat", "0.dosecat"), rows$key)
  expect_false(anyNA(index))
  expect_false("2.dosecat" %in% rows$key)
  expect_identical(rows$status[index], c("ref", "est", "est"))
  # Swapping the semantic code map changes which numerical row denotes High;
  # identity is the factor level, never a guessed numerical base or row text.
  reversed <- d
  attr(reversed$dosecat, "labels") <- c(Low = 0, Unused = 2, Medium = 1, High = 3)
  other <- survival::coxph(survival::Surv(time, event) ~ dosecat + x, reversed, ties = "breslow")
  other_rows <- regtab(other, stats = NULL)$meta$regtab_rows
  expect_identical(other_rows$status[match("0.dosecat", other_rows$key)], "ref")
  expect_identical(rows$level_identity[match("0.dosecat", rows$key)],
                   other_rows$level_identity[match("3.dosecat", other_rows$key)])
  expect_false(identical(rows$level_identity[match("0.dosecat", rows$key)],
                         other_rows$level_identity[match("0.dosecat", other_rows$key)]))
})

test_that("captured counts and source identities survive later caller label mutation", {
  d <- data.frame(y = c(2, 4, 3, 7, 5, 8, 6, 11), x = seq_len(8),
                  g = factor(rep(c("0", "1"), 4)), ev = c(1, 0, 2, 0, 1, 0, 2, 0))
  fit <- lm(y ~ g + x, d)
  record <- tt_fitcount(fit, "ev", data = d, terms = TRUE)
  before <- serialize(record, NULL)
  expect_identical(record$counts[c("obs", "events")], c(obs = 8, events = 6))
  expect_equal(record$terms$events[match(c("0.g", "1.g"), record$terms$count_key)], c(6, 0), tolerance = 0)
  attr(d$g, "labels") <- c("0" = 1, "1" = 0)
  d$ev <- 100
  table <- regtab(fit, fitcounts = record, mincount = 1, stats = "events")
  expect_identical(table$meta$regtab_rows$status[match(c("0.g", "1.g"), table$meta$regtab_rows$key)],
                   c("ref", "masked"))
  expect_equal(table$stored$events_1, 6, tolerance = 0)
  expect_identical(serialize(record, NULL), before)
  expect_identical(record$sample$row_ids, as.character(seq_len(8)))
  damaged <- record
  damaged$snapshot$source$g <- factor(rep(c("1", "0"), 4))
  expect_error(regtab(fit, fitcounts = damaged, mincount = 1), class = "tabtools_error_fitcount")
})

test_that("mixed coefficient matrices preserve estimable states for Matrix and list covariance", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- expand.grid(x = seq(-2.5, 2.5, by = 1), g = factor(seq_len(10)))
  d$y <- 2 + 1.5 * d$x + rep(c(1, -2, 1, -1, 2, -1), 10) / 5 +
    rep(seq(-2, 2, length.out = 10), each = 6)
  d$h <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  d$y[d$h == "c"] <- NA_real_; d$x[5] <- NA_real_; d$g[13] <- NA
  fits <- list(lme4::lmer(y ~ x + h + (1 | g), d, REML = FALSE, na.action = stats::na.exclude),
               glmmTMB::glmmTMB(y ~ x + h + (1 | g), d, na.action = stats::na.exclude))
  expect_s4_class(stats::vcov(fits[[1L]]), "Matrix")
  expect_true(is.list(stats::vcov(fits[[2L]])))
  for (fit in fits) {
    table <- NULL
    expect_warning(table <- regtab(fit, noreeffects = TRUE, stats = "n"), NA)
    rows <- table$meta$regtab_rows
    index <- match(c("x", "2.h", "_cons"), rows$key)
    expect_false(anyNA(index))
    expect_identical(rows$status[index], rep("est", 3))
    expect_identical(rows$variance_ok[index], rep(TRUE, 3))
    expected <- if (inherits(fit, "merMod")) lme4::fixef(fit) else glmmTMB::fixef(fit)$cond
    expect_equal(rows$estimate[index], unname(expected[c("x", "hb", "(Intercept)")]), tolerance = 1e-12)
    expect_true(all(is.finite(rows$conf.low[index]) & is.finite(rows$conf.high[index])))
  }
})

test_that("silent count metadata restoration retains value guards and default label advisory", {
  d <- data.frame(time = seq_len(40), event = rep(c(1, 0, 1, 1, 0), 8),
                  g = factor(rep(c("Control", "Active"), 20), levels = c("Control", "Active")))
  attr(d$g, "labels") <- c(Control = 0, Active = 1)
  fit <- survival::coxph(survival::Surv(time, event) ~ g, d, ties = "breslow")
  fit <- .rt_anchor(fit)
  frame <- .fc_retained_frame(fit)
  original <- count_adapter_snapshot(frame)
  frame_environment <- attr(attr(frame, "terms"), ".Environment")
  restored <- .rt_restore_attrs(frame, fit, warn = FALSE)
  expect_identical(attr(restored$g, "labels"), c(Control = 0, Active = 1))
  expect_identical(as.character(restored$g), as.character(frame$g))
  expect_identical(rownames(restored), rownames(frame))
  d$g <- factor(rev(as.character(d$g)), levels = levels(d$g))
  attr(d$g, "labels") <- c(Control = 1, Active = 0)
  expect_warning(.rt_restore_attrs(frame, fit), "data no longer match its model frame")
  silent <- NULL
  expect_warning(silent <- .rt_restore_attrs(frame, fit, warn = FALSE), NA)
  expect_identical(attr(silent$g, "labels"), attr(frame$g, "labels"))
  expect_true(identical(count_adapter_snapshot(frame), original, num.eq = FALSE, single.NA = FALSE))
  expect_identical(attr(attr(frame, "terms"), ".Environment"), frame_environment)
})
