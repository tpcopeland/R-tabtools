composite_v251_data <- function() {
  d <- data.frame(g = factor(rep(c("A", "B", "C"), each = 12)), x = rep(1:12, 3), time = rep(10, 36))
  d$ev <- as.numeric(d$x <= rep(c(3, 5, 8), each = 12))
  d$other <- factor(rep(c("No", "Yes"), 18))
  d
}
composite_v251_tables <- function() {
  d <- composite_v251_data()
  fits <- list(glm(ev ~ g, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L)),
    glm(ev ~ g + x, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L)))
  list(data = d, fits = fits, model = do.call(regtab, c(fits, list(models = c("Crude", "Adjusted"), coef = "IRR", nointercept = TRUE))),
       rate = stratetab(tt_rates(d, time = "time", event = "ev", by = "g"), ratescale = 1))
}

composite_reference_literals <- function() {
  model <- tt_table(matrix(c("g", "", "", "",
    "  A", "1.00", "", "", "  B", "1.00", "(0.80, 1.25)", "0.900",
    "  C", "2.00", "(1.50, 2.50)", "0.001"), nrow = 4L, byrow = TRUE),
    header = list(list(text = c("", "M", "", "")),
      list(text = c("Factor", "IRR", "95% CI", "p-value"))),
    rows = data.frame(type = c("cat_header", "level", "level", "level"),
      key = c("g", "1.g", "2.g", "3.g")), command = "regtab",
    meta = list(refcat = "1.00", frame = list(source = "regtab", ci_level = 95,
      n_models = 1L, statistic_ids = "estimate ci pvalue", model_id = "1",
      outcome_id = "ev", effect_scale = "IRR", model_label = "M",
      effect_additive = FALSE, effect_log_scale = "ratio"),
      regtab_rows = data.frame(row = 1:4, model = 1L,
        label = c("g", "  A", "  B", "  C"), status = c("blank", "ref", "est", "est"),
        estimate = c(NA, NA, 1, 2), conf.low = c(NA, NA, .8, 1.5),
        conf.high = c(NA, NA, 1.25, 2.5), p.value = c(NA, NA, .9, .001))))
  rate <- tt_table(matrix(c("g", "", "", "", "   A", "3", "100", "3.0 (1.0, 5.0)",
    "   B", "4", "100", "4.0 (2.0, 6.0)", "   C", "5", "100", "5.0 (3.0, 7.0)"),
    nrow = 4L, byrow = TRUE), header = list(list(text = c("Exposure", "Event", "", "")),
      list(text = c("", "Events", "Person-years", "Rate (95% CI)"))),
    command = "stratetab", meta = list(frame = list(source = "stratetab",
      statistic_ids = "events person_years rate_ci", ci_level = 95,
      n_outcomes = 1L, outcome_id = "ev")))
  list(model = tabtools:::.ct_stamp(model), rate = rate)
}

test_that("allmodels preserves two model blocks and independent coefficient intervals", {
  z <- composite_v251_tables()
  x <- hrcomptab(z$rate, z$model, rows = 2:4, allmodels = TRUE, keyed = TRUE, effect = "IRR", cformat = "%8.4f")
  expect_identical(ncol(x$body), 8L)
  expect_identical(x$stored$N_models_per_outcome, 2L)
  expect_identical(x$stored$N_modelonly, 0L)
  expect_identical(x$header[[1L]]$spans, data.frame(from = 2L, to = 8L))
  expect_identical(x$header[[2L]]$text[5:8], c("Crude, IRR (95% CI)", "Crude, p-value", "Adjusted, IRR (95% CI)", "Adjusted, p-value"))
  row <- which(trimws(x$body[[1L]]) == "B")
  for (m in 1:2) {
    b <- coef(z$fits[[m]])[["gB"]];se <- sqrt(vcov(z$fits[[m]])["gB", "gB"])
    values <- exp(c(b, b - qnorm(.975) * se, b + qnorm(.975) * se))
    expected <- sprintf("%.4f (%.4f, %.4f)", values[1L], values[2L], values[3L])
    expect_identical(x$body[[5L + 2L * (m - 1L)]][row], expected)
  }
  expect_error(as_forest_data(x), class = "tabtools_error_composition")
})

test_that("keyed placement ignores selected row order and preserves rates-only sections", {
  z <- composite_v251_tables()
  rates <- stratetab(list(tt_rates(z$data, "time", "ev", by = "g"), tt_rates(z$data, "time", "ev", by = "other")), outcomes = 1, ratescale = 1)
  x <- hrcomptab(rates, z$model, rows = c(4L, 2L, 3L, 1L), allmodels = TRUE, keyed = TRUE, effect = "IRR")
  only <- which(trimws(x$body[[1L]]) %in% c("No", "Yes"))
  expect_length(only, 2L)
  expect_true(all(as.matrix(x$body[only, 5:8]) == ""))
  expect_identical(x$stored$N_sections, 2L)
  expect_identical(x$body[[5L]][match("   A", x$body[[1L]])], "Reference")
})

test_that("modelonly appends unmatched estimates with blank rate columns", {
  z <- composite_v251_tables()
  x <- hrcomptab(z$rate, z$model, rows = "all", allmodels = TRUE, modelonly = TRUE, effect = "IRR")
  expect_identical(x$stored$N_modelonly, 1L)
  expect_identical(tail(x$body[[1L]], 1L), "x")
  expect_identical(unname(unlist(tail(x$body, 1L)[2:4])), rep("", 3L))
  expect_identical(tail(x$body[[5L]], 1L), "")
  expect_true(nzchar(tail(x$body[[7L]], 1L)))
  expect_error(hrcomptab(z$rate, z$model, rows = "all", allmodels = TRUE, keyed = TRUE, effect = "IRR"), class = "tabtools_error_composition")
})

test_that("structured mappings reorder complete blocks and reject ambiguous or mixed scales", {
  z <- composite_v251_tables()
  x <- hrcomptab(z$rate, z$model, rows = 2:4, keyed = TRUE, outcomemap = list(c("Adjusted", "Crude")), effect = "IRR")
  row <- match("   B", x$body[[1L]])
  expect_identical(x$body[[5L]][row], paste(z$model$body[[5L]][3L], z$model$body[[6L]][3L]))
  expect_match(x$header[[2L]]$text[5L], "Adjusted", fixed = TRUE)
  expect_error(hrcomptab(z$rate, z$model, rows = 2:4, keyed = TRUE, outcomemap = list(c("Crude", "Crude")), effect = "IRR"), class = "tabtools_error_composition")
  expect_error(hrcomptab(z$rate, z$model, rows = 2:4, keyed = TRUE, outcomemap = list("unknown"), effect = "IRR"), class = "tabtools_error_composition")
  expect_error(hrcomptab(z$rate, z$model, rows = 2:4, allmodels = TRUE, keyed = TRUE, effect = "HR"), class = "tabtools_error_composition")
})

test_that("companion formatting and separator-only rendering preserve source identity", {
  z <- composite_v251_tables()
  x <- comptab(z$model, rows = 3L, cformat = "%8.4f", cisep = " to ")
  b <- coef(z$fits[[1L]])[["gB"]];se <- sqrt(vcov(z$fits[[1L]])["gB", "gB"])
  expect_identical(x$body[[2L]], sprintf("%.4f", exp(b)))
  expect_identical(x$body[[3L]], sprintf("(%.4f to %.4f)", exp(b - qnorm(.975) * se), exp(b + qnorm(.975) * se)))
  sep <- comptab(z$model, rows = 3L, cisep = " to ")
  expect_identical(sep$body[[3L]], sub(", ", " to ", z$model$body[[3L]][3L], fixed = TRUE))
  imported <- as.data.frame(z$model)
  expect_identical(comptab(imported, rows = 3L, cformat = "%8.4f")$body, comptab(z$model, rows = 3L, cformat = "%8.4f")$body)
  nested <- comptab(z$model, rows = 3L)
  expect_identical(comptab(nested, rows = 1L, cformat = "%8.4f")$body,
    comptab(z$model, rows = 3L, cformat = "%8.4f")$body)
  stale <- z$model;stale$meta$regtab_rows$estimate[1L] <- 100
  expect_error(comptab(stale, rows = 3L, cformat = "%8.4f"), class = "tabtools_error_composition")
  imported[3L, 2L] <- "changed"
  expect_error(comptab(imported, rows = 3L, cformat = "%8.4f"), class = "tabtools_error_composition")
})

test_that("missing or ambiguous companions and noncanonical separators refuse", {
  z <- composite_v251_tables()
  x <- as.data.frame(z$model);attr(x, "composition") <- NULL
  expect_error(comptab(x, rows = 3L, cformat = "%8.4f"), class = "tabtools_error_composition")
  alternate <- do.call(regtab, c(z$fits, list(coef = "IRR", sep = " to ", nointercept = TRUE)))
  expect_error(comptab(alternate, rows = 3L, cisep = " / "), class = "tabtools_error_composition")
  source <- tabtools:::.ct_source(z$model, 1L)
  source$rows <- rbind(source$rows, source$rows[1L, ])
  expect_error(tabtools:::.ct_reformat_source(source, "%8.4f", NULL), class = "tabtools_error_composition")
})

test_that("variable outcome spans and export products keep truthful geometry", {
  z <- composite_v251_tables()
  x <- hrcomptab(z$rate, z$model, rows = 2:4, allmodels = TRUE, keyed = TRUE, effect = "IRR")
  layout <- tabtools:::.xlsx_layout_hrcomptab(x)
  style <- tabtools:::.xlsx_apply_rules(layout$rules, nrow(layout$grid), ncol(layout$grid), x$style)
  expect_true("C2:I2" %in% style$merges)
  expect_identical(dim(layout$grid), c(nrow(x$body) + 3L, 9L))
  expect_error(tt_flat(x), class = "tabtools_error_flat")
  flat <- tt_flat(x, keyed = FALSE)
  expect_identical(unname(as.matrix(flat)), unname(as.matrix(x$body)))
  expect_error(comptab(flat, rows = 1L), class = "tabtools_error_composition")
  vertical <- comptab(z$model, rows = 3L)
  plain <- tt_flat(vertical, keyed = FALSE)
  # Column labels travel independently from the unchanged publication cells.
  expect_identical(unname(as.matrix(plain)), unname(as.matrix(vertical$body)))
  expect_identical(names(plain), c("rowlabel", paste0("c", seq_len(ncol(plain) - 1L))))
  expect_identical(attr(plain, "header"), vertical$header)
  expect_identical(attr(plain, "command"), vertical$command)
  expect_identical(attr(plain, "frame"), vertical$meta$frame)
  expect_identical(attr(plain, "sample_accounting"), vertical$meta$sample_accounting)
  expect_identical(attr(plain, "composition_export"), TRUE)
  expect_identical(class(plain), "data.frame")
})

test_that("model-only factor appendices insert headings without counting them as estimates", {
  z <- composite_v251_tables()
  fit <- glm(ev ~ g + other + x, poisson(), z$data)
  model <- regtab(fit, coef = "IRR", nointercept = TRUE)
  x <- hrcomptab(z$rate, model, rows = "all", modelonly = TRUE)
  expect_identical(tail(x$body[[1L]], 4L), c("other", "   No", "   Yes", "x"))
  expect_identical(x$stored$N_modelonly, 3L)
  expect_identical(x$stored$N_modelrows, 6L)
  expect_identical(x$stored$N_rows, nrow(z$rate$body) + 7L)
  expect_true(all(as.matrix(tail(x$body, 4L)[2:4]) == ""))
  expect_true(all(unlist(tail(x$body, 4L)[1L, 5:6]) == ""))
  expect_identical(x$body[[5L]][nrow(x$body) - 2L], "Reference")
  expect_identical(tail(x$meta$section_rows, 1L), nrow(z$rate$body) + 1L)
})

test_that("multiple outcomes use their own rate header and selected model block", {
  z <- composite_v251_tables()
  d <- z$data;d$ev2 <- as.numeric(d$x <= rep(c(2, 6, 9), each = 12))
  second <- glm(ev2 ~ g, poisson(), d)
  models <- regtab(z$fits[[1L]], second, models = c("First", "Second"), coef = "IRR", nointercept = TRUE)
  rates <- stratetab(list(tt_rates(d, "time", "ev", by = "g"), tt_rates(d, "time", "ev2", by = "g")),
    outcomes = 2, outlabels = c("First outcome", "Second outcome"), outcomeids = c("ev", "ev2"), ratescale = 1)
  x <- hrcomptab(rates, models, rows = 3:4)
  expect_identical(x$meta$frame$outcome_label, c("First outcome", "Second outcome"))
  expect_identical(x$header[[1L]]$spans, data.frame(from = c(2L, 7L), to = c(6L, 11L)))
  forest <- as_forest_data(x)
  expect_identical(unique(forest$model_label[forest$rowtype == "effect"]), c("First outcome", "Second outcome"))
  expect_identical(unname(as.matrix(x$body[, 7:9])), unname(as.matrix(rates$body[, 5:7])))
  expect_identical(names(x$meta$frame$outcome_model_map[[2L]]$source_model), "ev2")
  expect_identical(unname(x$meta$frame$outcome_model_map[[2L]]$source_model), 2L)
  varied <- regtab(z$fits[[1L]], z$fits[[2L]], second, coef = "IRR", nointercept = TRUE,
    models = c("Crude", "Adjusted", "Second"))
  v <- hrcomptab(rates, varied, rows = 2:4, keyed = TRUE, outcomemap = list(c("Crude", "Adjusted"), "Second"))
  expect_identical(v$stored$N_models_per_outcome, c(2L, 1L))
  expect_identical(v$header[[1L]]$spans, data.frame(from = c(2L, 9L), to = c(8L, 13L)))
  expect_identical(unname(as.matrix(v$body[, 9:11])), unname(as.matrix(rates$body[, 5:7])))
})

test_that("rate mapping machine identities retain case while model labels ignore it", {
  d <- composite_v251_data();d$Event <- d$ev
  rate <- stratetab(tt_rates(d, "time", "Event", by = "g"), outcomeids = "Event")
  model <- regtab(glm(Event ~ g, poisson(), d), coef = "IRR", nointercept = TRUE, models = "Human Label")
  expect_s3_class(hrcomptab(rate, model, rows = 3:4), "tt_table")
  expect_error(hrcomptab(rate, model, rows = 3:4, outcomemap = list("event")), class = "tabtools_error_composition")
  expect_s3_class(hrcomptab(rate, model, rows = 3:4, outcomemap = list("human label")), "tt_table")
})

test_that("companion rendering preserves stars and unavailable publication intervals", {
  z <- composite_v251_tables()
  starred <- regtab(z$fits[[1L]], coef = "IRR", nointercept = TRUE, stars = TRUE, starslevels = c(.8, .6, .2))
  x <- comptab(starred, rows = 4L, cformat = "%8.4f")
  expect_match(x$body[[2L]], "\\*+$")
  source <- as.data.frame(starred)
  expect_identical(comptab(source, rows = 4L, cformat = "%8.4f")$body, x$body)
  # A producer with an explicitly unavailable confidence interval keeps its
  # publication text, rather than inventing bounds from an estimate alone.
  summaries <- data.frame(term = "x", estimate = 2, conf.low = NA_real_, conf.high = NA_real_, p.value = .2)
  e <- effecttab(summaries, effect = "IRR", models = "Estimate", level = 95)
  expect_identical(comptab(e, rows = 1L, cformat = "%8.4f")$body, comptab(e, rows = 1L)$body)
  expect_error(as_forest_data(comptab(e, rows = 1L)), class = "tabtools_error_composition")
  finite <- data.frame(term = "x", estimate = 2, conf.low = 1.1, conf.high = 3.5, p.value = NA_real_)
  f <- as_forest_data(comptab(effecttab(finite, effect = "IRR", models = "Estimate", level = 95), rows = 1L))
  expect_identical(f$estimate, 2)
  expect_identical(f$ll, 1.1)
  expect_identical(f$ul, 3.5)
  expect_identical(f$pvalue, NA_real_)
})

test_that("new modes refuse matching plain rows and invalid boolean flags before sinks", {
  z <- composite_v251_tables()
  d <- z$data;attr(d$x, "label") <- "g"
  plain <- regtab(glm(ev ~ x, poisson(), d), coef = "IRR", nointercept = TRUE)
  expect_error(hrcomptab(z$rate, plain, rows = 1L, keyed = TRUE), class = "tabtools_error_composition")
  expect_error(hrcomptab(z$rate, z$model, rows = 3:4, keyed = TRUE, allmodels = NA), class = "tabtools_error_composition")
  expect_error(comptab(z$model, rows = 3L, modelonly = TRUE), class = "tabtools_error_composition")
  expect_error(hrcomptab(z$rate, z$model, rows = c(3L, 3L, 4L), allmodels = TRUE, keyed = TRUE), class = "tabtools_error_composition")
})

test_that("unchanged composed model sources keep snapshots through selection and stacking", {
  z <- composite_v251_tables()
  combined <- tt_stack(z$model, z$model)
  expect_s3_class(comptab(combined, rows = 3L), "tt_table")
  expect_error(comptab(combined, rows = 3L, cformat = "%8.4f"), class = "tabtools_error_composition")
})

test_that("canonical reference states survive custom text equal to a real estimate", {
  z <- composite_reference_literals()
  before <- serialize(z, NULL)
  source <- tabtools:::.ct_source(z$model, 1L)
  expect_true(tabtools:::.ct_is_ref(source, 2L, 1L))
  expect_false(tabtools:::.ct_is_ref(source, 3L, 1L))
  x <- hrcomptab(z$rate, z$model, rows = 2:4, keyed = TRUE)
  b <- match("   B", x$body[[1L]])
  expect_identical(x$body[[5L]][b], "1.00 (0.80, 1.25)")
  expect_identical(x$meta$ref_rows, match("   A", x$body[[1L]]))
  imported <- as.data.frame(z$model)
  expect_identical(hrcomptab(z$rate, imported, rows = 2:4, keyed = TRUE)$body, x$body)
  changed <- z$model
  changed$meta$refcat <- "2.00"
  changed$meta$omitlabel <- "1.00"
  changed$meta$emptylabel <- "2.00"
  expect_identical(hrcomptab(z$rate, changed, rows = 2:4, keyed = TRUE)$body, x$body)
  genuine <- z$model
  genuine$body[2L, 2L] <- "Custom baseline $value"
  genuine <- tabtools:::.ct_stamp(genuine)
  expect_true(tabtools:::.ct_is_ref(tabtools:::.ct_source(genuine, 1L), 2L, 1L))
  expect_identical(serialize(z, NULL), before)
})

test_that("publication overrides and nonreference states never acquire reference identity", {
  z <- composite_reference_literals()
  for (state in c("masked", "omit", "empty", "notest", "constrained", "absent")) {
    model <- z$model
    model$meta$regtab_rows$status[3L] <- state
    model$meta$regtab_rows[3L, c("estimate", "conf.low", "conf.high", "p.value")] <- NA_real_
    model$body[3L, 3:4] <- ""
    model$meta$regtab_rows$override_origin <- c(NA, NA, "cellnote", NA)
    model <- tabtools:::.ct_stamp(model)
    before <- serialize(model, NULL)
    source <- tabtools:::.ct_source(model, 1L)
    expect_false(tabtools:::.ct_is_ref(source, 3L, 1L), info = state)
    expect_true(tabtools:::.ct_is_ref(source, 2L, 1L), info = state)
    expect_identical(serialize(model, NULL), before)
  }
  model <- z$model
  model$meta$regtab_rows$status[2L] <- "base"
  model <- tabtools:::.ct_stamp(model)
  expect_true(tabtools:::.ct_is_ref(tabtools:::.ct_source(model, 1L), 2L, 1L))
  model$meta$regtab_rows <- NULL
  model <- tabtools:::.ct_stamp(model)
  expect_error(hrcomptab(z$rate, model, rows = 2:4, keyed = TRUE),
    class = "tabtools_error_composition")
})

test_that("reference companion mutations refuse before sinks and leave inputs unchanged", {
  z <- composite_reference_literals()
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("sentinel", path)
  for (field in c("status", "row", "model", "estimate")) {
    model <- z$model
    model$meta$regtab_rows[[field]][3L] <- if (field == "status") "ref" else 99
    before <- serialize(model, NULL)
    expect_error(hrcomptab(z$rate, model, rows = 2:4, keyed = TRUE, csv = path),
      class = "tabtools_error_composition")
    expect_identical(serialize(model, NULL), before)
    expect_identical(readLines(path), "sentinel")
    imported <- as.data.frame(z$model)
    stamp <- attr(imported, "composition", exact = TRUE)
    stamp$companion[[field]][3L] <- if (field == "status") "ref" else 99
    attr(imported, "composition") <- stamp
    before <- serialize(imported, NULL)
    expect_error(hrcomptab(z$rate, imported, rows = 2:4, keyed = TRUE, csv = path),
      class = "tabtools_error_composition")
    expect_identical(serialize(imported, NULL), before)
    expect_identical(readLines(path), "sentinel")
  }
})

test_that("canonical references follow original model positions after block reordering", {
  z <- composite_reference_literals()
  model <- z$model
  second <- model$body[, 2:4]
  second[2L, ] <- c("1.30", "(0.90, 1.80)", "0.300")
  second[3L, ] <- c("1.00", "", "")
  model$body <- cbind(model$body, second)
  names(model$body) <- paste0("c", seq_len(ncol(model$body)))
  model$header[[1L]]$text <- c("", "First", "", "", "Second", "", "")
  model$header[[2L]]$text <- c("Factor", rep(c("IRR", "95% CI", "p-value"), 2L))
  model$meta$frame$n_models <- 2L
  model$meta$frame$model_id <- c("1", "2")
  model$meta$frame$model_label <- c("First", "Second")
  records <- model$meta$regtab_rows
  records$model <- 2L
  records$status <- c("blank", "est", "ref", "est")
  records$estimate[2:3] <- c(1.3, NA)
  records$conf.low[2:3] <- c(.9, NA)
  records$conf.high[2:3] <- c(1.8, NA)
  records$p.value[2:3] <- c(.3, NA)
  model$meta$regtab_rows <- rbind(model$meta$regtab_rows, records)
  model <- tt_table(model$body, header = model$header, rows = model$rows,
    command = model$command, meta = model$meta)
  model <- tabtools:::.ct_stamp(model)
  source <- tabtools:::.ct_source(model, 1L)
  expect_true(tabtools:::.ct_is_ref(source, 2L, 1L))
  expect_false(tabtools:::.ct_is_ref(source, 2L, 4L))
  expect_false(tabtools:::.ct_is_ref(source, 3L, 1L))
  expect_true(tabtools:::.ct_is_ref(source, 3L, 4L))
  # The compositor's validated block permutation preserves companion model
  # indices and transports their original positions in model_map.
  source$cells <- source$cells[, c(4:6, 1:3), drop = FALSE]
  source$h1 <- c(source$h1[1L], source$h1[-1L][c(4:6, 1:3)])
  source$h2 <- c(source$h2[1L], source$h2[-1L][c(4:6, 1:3)])
  source$model_map <- c(2L, 1L)
  expect_false(tabtools:::.ct_is_ref(source, 2L, 1L))
  expect_true(tabtools:::.ct_is_ref(source, 2L, 4L))
  expect_true(tabtools:::.ct_is_ref(source, 3L, 1L))
  expect_false(tabtools:::.ct_is_ref(source, 3L, 4L))
})

test_that("review 2026-10-07 A2: plain exports label each outcome block by its own header", {
  skip_if_not_installed("survival")
  set.seed(2)
  d <- data.frame(g = factor(sample(c("A", "B", "C"), 300, TRUE)), time = runif(300, 1, 10))
  d$ev1 <- rbinom(300, 1, .3)
  d$ev2 <- rbinom(300, 1, .2)
  c1 <- survival::coxph(survival::Surv(time, ev1) ~ g, d)
  c2 <- survival::coxph(survival::Surv(time, ev2) ~ g, d)
  mt <- regtab(c1, c2)
  rate <- stratetab(list(tt_rates(d, "time", "ev1", by = "g"), tt_rates(d, "time", "ev2", by = "g")), outcomes = 2)
  x <- hrcomptab(rate, mt, rows = 3:4, outcomemap = list("ev1", "ev2"))
  expect_true(all(is.na(x$cols$model)))
  flat <- tt_flat(x, keyed = FALSE)
  labels <- vapply(flat[-1L], function(v) attr(v, "label"), "")
  expect_identical(unname(sub(",.*$", "", labels)), rep(c("ev1", "ev2"), each = 5L))
  expect_identical(unname(labels[6L]), "ev2, Events")
  shown <- puttab(flat, varlabels = TRUE)
  expect_identical(shown$header[[1L]]$text[8L], "ev2, Person-Years (PY)")
  # The keyed-model path keeps the same labels.
  y <- hrcomptab(rate, mt, rows = 3:4)
  expect_identical(vapply(tt_flat(y, keyed = FALSE)[-1L], function(v) attr(v, "label"), ""), labels)
})
