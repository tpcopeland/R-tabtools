library(testthat)
library(tabtools)

# IMD-01..05 each run at two seeds. Maximum input: 84 records; frequency
# expansion is at most 420 rows. All oracles use public output and raw masks.
imd_data <- function(seed) {
  withr::with_seed(seed, {
    n <- 84L
    g <- sample(c("A", "B"), n, replace = TRUE)
    g[81:84] <- c("sparse", "lost", "lost", "lost")
    g[c(4, 15)] <- NA_character_
    g[28] <- ""
    d <- data.frame(g = factor(g, levels = c("A", "B", "sparse", "lost", "unused", "")),
      x = 4 + rnorm(n), y = rexp(n), z = NA_real_,
      cat = factor(sample(c("a", "b"), n, replace = TRUE), levels = c("a", "b", "absent")),
      bin = rbinom(n, 1, .4), event2 = rbinom(n, 1, .25),
      w = runif(n, .2, 5), fw = sample(1:5, n, replace = TRUE),
      period = rep(1:3, length.out = n), entry = runif(n, 0, 1), time = runif(n, 2, 8))
    d$x[c(2, 3, 4, 12, 22, 81)] <- NA_real_
    d$x[c(8, 9)] <- c(-2, 0)
    d$y[c(5, 12, 15, 23)] <- NA_real_
    d$cat[c(6, 15, 24, 81)] <- NA
    d$bin[c(4, 7, 12, 15, 24)] <- NA_integer_
    d$event2[c(2, 5, 12, 21)] <- NA_integer_
    d$w[c(3, 14, 82:84)] <- 0
    d$fw[c(3, 14, 82:84)] <- 0
    d$w[c(4, 21)] <- NA_real_
    d$fw[c(4, 21)] <- NA_integer_
    d$period[c(15, 31)] <- NA_integer_
    d$period[81] <- 3L
    d$time[c(10, 21)] <- NA_real_
    d$entry[11] <- NA_real_
    d$time[13] <- d$entry[13]
    attr(d$x, "label") <- "Seeded measurement"
    d
  })
}

imd_group_values <- function(g) {
  out <- as.character(g)
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

imd_ledger <- function(t) {
  s <- if (inherits(t, "tt_table")) t$meta$sample_accounting else attr(t, "sample_accounting", exact = TRUE)
  expect_type(s, "list")
  if (is.null(s)) stop("Missing source sample ledger.")
  s
}

imd_population <- function(s, scope, variable = NULL, group = NULL, spec = NULL, component = NULL) {
  p <- s$populations
  use <- p$scope == scope
  if (!is.null(variable)) use <- use & !is.na(p$variable) & p$variable == variable
  if (!is.null(group)) use <- use & !is.na(p$group) & p$group == group
  if (!is.null(spec)) use <- use & !is.na(p$spec) & p$spec == spec
  if (!is.null(component)) use <- use & p$component == component
  if (scope == "variable" && is.null(group)) use <- use & is.na(p$group)
  ids <- p$id[use]
  expect_length(ids, 1L)
  ids
}

imd_measure <- function(s, id, metrics) {
  m <- s$measures[s$measures$population_id == id, ]
  idx <- match(metrics, m$metric)
  expect_false(anyNA(idx))
  m[idx, ]
}

imd_counts <- function(s, id, expected) {
  m <- imd_measure(s, id, names(expected))
  expect_identical(m$status, rep("available", length(expected)))
  expect_equal(m$value, unname(expected), tolerance = 0, info = id)
}

imd_weights <- function(s, id, w) {
  m <- imd_measure(s, id, c("weight_sum", "effective_n"))
  expect_identical(m$status[1], "available")
  if (sum(w) == 0) expect_equal(m$value[1], 0, tolerance = 0) else
    expect_equal(m$value[1] / sum(w), 1, tolerance = 1e-12)
  if (!any(w > 0)) {
    expect_true(is.na(m$value[2]))
    expect_identical(m$status[2], "unavailable")
    expect_true(nzchar(m$reason[2]))
  } else {
    norm <- w / max(w)
    expect_equal(m$value[2], sum(norm)^2 / sum(norm^2), tolerance = 1e-12)
    expect_identical(m$status[2], "available")
  }
}

imd_pair <- function(cell) {
  if (!nzchar(cell)) return(c(NA_real_, NA_real_))
  txt <- strsplit(cell, "±", fixed = TRUE)[[1L]]
  as.numeric(replace(txt, txt == ".", NA_character_))
}

imd_printed <- function(actual, expected) {
  expect_length(actual, length(expected))
  expect_identical(is.na(actual), is.na(expected))
  observed <- !is.na(expected)
  # Five printed decimal places have an absolute half-unit of 5e-6;
  # relative equality would shrink that bound for values below one.
  if (any(observed)) expect_lte(max(abs(actual[observed] - expected[observed])), 5e-6)
}

imd_moments <- function(x, w = rep(1, length(x)), kind = "none") {
  if (!length(x)) return(c(NA_real_, NA_real_))
  if (kind == "frequency") x <- rep(x, w)
  if (kind != "importance") return(c(mean(x), stats::sd(x)))
  norm <- w / max(w)
  mu <- sum(norm * x) / sum(norm)
  sd <- if (length(x) == 1L) NA_real_ else
    sqrt(sum(norm * (x - mu)^2) / sum(norm) * length(x) / (length(x) - 1))
  c(mu, sd)
}

imd_table_check <- function(t, d, kind = "none", component = "", include_missing = FALSE) {
  s <- imd_ledger(t)
  g <- imd_group_values(d$g)
  w <- switch(kind, importance = d$w, frequency = d$fw, rep(1, nrow(d)))
  eligible <- !is.na(g) & !is.na(w) & w > 0
  # Crude overlays use unit weights on the already selected weighted sample.
  if (component == "crude") {
    eligible <- !is.na(g) & !is.na(d$w) & d$w > 0
    w[] <- 1
  }
  id <- imd_population(s, "table", component = component)
  zeros <- if (component == "crude") sum(d$w == 0, na.rm = TRUE) else sum(w == 0, na.rm = TRUE)
  imd_counts(s, id, c(input_n = nrow(d), eligible_n = sum(eligible), used_n = sum(eligible),
    zero_weight_n = zeros, excluded_n = sum(!eligible),
    reported_n = if (kind == "frequency") sum(w[eligible]) else sum(eligible)))
  imd_weights(s, id, w[eligible])
  ex <- s$exclusions[s$exclusions$population_id == id & s$exclusions$stage == "input_to_eligible", ]
  expect_equal(sum(ex$n), sum(!eligible), tolerance = 0)
  names_and_types <- t$rows[t$rows$type %in% c("var", "cat_header"), c("var", "vtype")]
  names_and_types <- unique(names_and_types)
  # Spec IDs, rather than body blocks, distinguish repeated x summaries.
  p <- s$populations
  specs <- p[p$scope == "variable" & is.na(p$group) & p$component == component, ]
  for (i in seq_len(nrow(specs))) {
    variable <- specs$variable[i]
    types <- names_and_types$vtype[names_and_types$var == variable]
    type <- if (length(types) == 1L) types else c("contn", "contln")[sum(specs$variable[seq_len(i)] == variable)]
    observed <- !is.na(d[[variable]])
    used <- eligible & observed
    if (type %in% c("cat", "cate") && include_missing) used <- eligible
    if (type == "contln") used <- used & d[[variable]] > 0
    imd_counts(s, specs$id[i], c(input_n = sum(eligible), eligible_n = sum(eligible),
      observed_n = sum(eligible & observed), used_n = sum(used), missing_n = sum(eligible & !observed),
      excluded_n = sum(eligible) - sum(used), zero_weight_n = 0,
      reported_n = if (kind == "frequency") sum(w[used]) else sum(used)))
    imd_weights(s, specs$id[i], w[used])
  }
  list(eligible = eligible, weights = w, groups = g, ledger = s)
}

imd_column <- function(t, label, pass = "") {
  if (nzchar(pass)) label <- paste(if (pass == "crude") "Crude" else "Weighted", label)
  index <- which(t$header[[1]]$text == label)
  expect_length(index, 1L)
  index
}

imd_table_cells <- function(t, d, check, kind = "none", pass = "") {
  lev <- levels(d$g)
  groups <- lev[lev %in% check$groups[check$eligible]]
  masks <- lapply(groups, function(g) check$eligible & check$groups == g)
  masks <- c(masks, list(check$eligible))
  labels <- c(groups, "Total")
  for (variable in c("x", "z")) {
    row <- which(t$rows$var == variable & t$rows$vtype == "contn" & t$rows$type == "var")
    expect_length(row, 1L)
    for (j in seq_along(masks)) {
      use <- masks[[j]] & !is.na(d[[variable]])
      imd_printed(imd_pair(t$body[row, imd_column(t, labels[j], pass)]),
        imd_moments(d[[variable]][use], check$weights[use], kind))
    }
  }
}

for (imd_seed in c(4101L, 9917L)) {
  local({
    seed <- imd_seed
    test_that(paste0("IMD-01 seed=", seed, ": variablewise and explicitly complete-case tables"), {
      d <- imd_data(seed)
      d <- d[seq_len(nrow(d)) %% 5 != 0, ]
      before <- d
      args <- list(vars = c(x = "contn", y = "conts", cat = "cat", bin = "bin", z = "contn"),
                   by = "g", total = "after", nopvalue = TRUE, format = "%16.5f",
                   missingsummary = TRUE, slashN = TRUE, spacelowpercent = FALSE)
      t <- do.call(table1_tc, c(list(data = d), args))
      checked <- imd_table_check(t, d)
      imd_table_cells(t, d, checked)
      cc <- stats::complete.cases(d[c("x", "y", "cat", "bin")]) & !is.na(imd_group_values(d$g))
      complete <- d[cc, ]
      tc <- do.call(table1_tc, c(list(data = complete), args))
      ccheck <- imd_table_check(tc, complete)
      imd_table_cells(tc, complete, ccheck)
      sid <- imd_population(checked$ledger, "variable", "x", spec = 1)
      expect_gt(imd_measure(checked$ledger, sid, "used_n")$value, nrow(complete))
      all_y <- d$y[checked$eligible & !is.na(d$y)]
      yr <- which(t$rows$var == "y" & t$rows$type == "var")
      numbers <- as.numeric(strsplit(gsub("[(),]", " ", t$body[yr, imd_column(t, "Total")]), " +")[[1]])
      numbers <- numbers[!is.na(numbers)]
      imd_printed(numbers, as.numeric(stats::quantile(all_y, c(.5, .25, .75), type = 2)))
      expect_false(any(grepl("absent|unused", t$body[[1]])))
      expect_identical(d, before)
    })

    test_that(paste0("IMD-02 seed=", seed, ": scaled importance overlays and repeated positive-only specs"), {
      d <- imd_data(seed)
      d$w <- d$w * 1e180
      before <- d
      t <- table1_tc(d, vars = c(x = "contn", x = "contln", cat = "cat", bin = "bin", z = "contn"),
        by = "g", wt = "w", wtcompare = TRUE, missing = TRUE, total = "before",
        nopvalue = TRUE, percent = TRUE, format = "%16.5f", percformat = "%12.5f", percsign = "",
        spacelowpercent = FALSE)
      for (pass in c("crude", "weighted")) {
        kind <- if (pass == "crude") "none" else "importance"
        checked <- imd_table_check(t, d, kind, pass, TRUE)
        imd_table_cells(t, d, checked, kind, pass)
        # Check the repeated positive-only specification numerically as
        # well as its ledger. Work on raw log values and normalized weights.
        positive <- checked$eligible & !is.na(d$x) & d$x > 0
        logged <- log(d$x[positive])
        weights <- checked$weights[positive]
        weights <- weights / max(weights)
        log_mean <- sum(weights * logged) / sum(weights)
        log_variance <- sum(weights * (logged - log_mean)^2) / sum(weights) *
          length(logged) / (length(logged) - 1)
        geometric_row <- which(t$rows$var == "x" & t$rows$vtype == "contln" & t$rows$type == "var")
        expect_length(geometric_row, 1L)
        geometric_text <- t$body[geometric_row, imd_column(t, "Total", pass)]
        geometric_values <- as.numeric(strsplit(gsub("[()/×]", " ", geometric_text), " +")[[1]])
        geometric_values <- geometric_values[!is.na(geometric_values)]
        imd_printed(geometric_values, exp(c(log_mean, sqrt(log_variance))))
        used <- checked$eligible
        missing_row <- which(t$rows$var == "cat" & trimws(t$body[[1]]) == "Missing")
        expect_length(missing_row, 1L)
        imd_printed(as.numeric(t$body[missing_row, imd_column(t, "Total", pass)]),
          100 * sum(checked$weights[used & is.na(d$cat)]) / sum(checked$weights[used]))
      }
      expect_identical(d, before)
      expect_identical(attr(as.data.frame(t), "sample_accounting", exact = TRUE), imd_ledger(t))
    })

    test_that(paste0("IMD-03 seed=", seed, ": subset frequency summaries match physical expansion"), {
      d <- imd_data(seed)
      d <- d[seq_len(nrow(d)) %% 4 != 0 | (!is.na(d$g) & as.character(d$g) == "sparse"), ]
      before <- d
      args <- list(data = d, vars = c(x = "contn", cat = "cat", bin = "bin", z = "contn"),
        by = "g", fweight = "fw", missing = TRUE, total = "after", nopvalue = TRUE,
        format = "%16.5f", percformat = "%12.5f", slashN = TRUE, spacelowpercent = FALSE)
      t <- do.call(desctab, args)
      checked <- imd_table_check(t, d, "frequency", include_missing = TRUE)
      imd_table_cells(t, d, checked, "frequency")
      index <- rep(which(checked$eligible), d$fw[checked$eligible])
      expanded <- d[index, ]
      er <- which(t$rows$var == "bin" & t$rows$type == "var")
      observed <- !is.na(expanded$bin)
      expected <- c(sum(expanded$bin[observed]), sum(observed), 100 * mean(expanded$bin[observed]))
      actual <- as.numeric(strsplit(gsub("[()/]", " ", t$body[er, imd_column(t, "Total")]), " +")[[1]])
      actual <- actual[!is.na(actual)]
      expect_equal(actual[1:2], expected[1:2], tolerance = 0)
      imd_printed(actual[3], expected[3])
      expect_identical(t, do.call(table1_tc, args))
      expect_identical(d, before)
    })

    test_that(paste0("IMD-04 seed=", seed, ": sparse period cells and pooled versus period truncation"), {
      d <- imd_data(seed)
      d$w <- d$w * 1e160
      before <- d
      eligible <- !is.na(d$w) & !is.na(d$g) & !is.na(d$period)
      for (mode in c("pooled", "period")) {
        expect_warning(t <- wttab(d, "w", by = "g", period = "period", trunc = c(.25, .75),
          trunc_by = mode, digits = 5), paste0("Dropped ", sum(!eligible)), class = "rlang_warning")
        s <- imd_ledger(t)
        id <- imd_population(s, "table")
        imd_counts(s, id, c(input_n = nrow(d), eligible_n = sum(eligible), used_n = sum(eligible),
          observed_n = sum(eligible), missing_n = sum(is.na(d$w)), excluded_n = sum(!eligible),
          zero_weight_n = sum(d$w[eligible] == 0), reported_n = sum(eligible)))
        imd_weights(s, id, d$w[eligible])
        st <- t$stored$stats
        expect_true(any(st$n == 0))
        expect_false(any(st$group == "unused"))
        for (i in seq_len(nrow(st))) {
          mask <- eligible & d$period == as.numeric(st$period[i])
          cutmask <- if (mode == "pooled") eligible else mask
          w <- d$w
          moved <- rep(FALSE, nrow(d))
          if (st$weights[i] != "Untruncated") {
            cuts <- stats::quantile(w[cutmask], c(.25, .75), type = 2, names = FALSE)
            moved <- !is.na(w) & (w < cuts[1] | w > cuts[2])
            w <- pmin(pmax(w, cuts[1]), cuts[2])
          }
          if (st$group[i] != "Overall") {
            value <- if (st$group[i] == "(blank)") "" else st$group[i]
            mask <- mask & !is.na(d$g) & as.character(d$g) == value
          }
          w <- w[mask]
          norm <- w / 1e160
          n <- length(w)
          expect_equal(st$n[i], n, tolerance = 0)
          if (n) {
            expected <- c(mean(norm), stats::sd(norm), min(norm),
              stats::quantile(norm, c(.01, .25, .5, .75, .99), type = 2, names = FALSE), max(norm))
            actual <- as.numeric(st[i, c("mean", "sd", "min", "p1", "p25", "p50", "p75", "p99", "max")]) / 1e160
            expect_equal(actual, expected, tolerance = 1e-12)
          } else expect_true(all(is.na(st[i, c("mean", "sd", "min", "max", "ess")])) )
          version <- if (st$weights[i] == "Untruncated") 1 else 2
          cell <- s$populations[s$populations$scope == "group" & s$populations$group == st$group[i] &
            s$populations$spec == version &
            grepl(paste0("/period", match(st$period[i], unique(st$period)), "$"), s$populations$component), ]
          expect_equal(nrow(cell), 1L)
          imd_counts(s, cell$id, c(input_n = n, used_n = n, zero_weight_n = sum(w == 0), reported_n = n))
          imd_weights(s, cell$id, w)
          if (st$weights[i] != "Untruncated") expect_equal(st$n_trunc[i], sum(mask & moved), tolerance = 0)
        }
      }
      expect_identical(d, before)
    })

    test_that(paste0("IMD-05 seed=", seed, ": missing events retained across subset follow-up and rate blocks"), {
      d <- imd_data(seed)
      d <- d[seq_len(nrow(d)) %% 6 != 0, ]
      before <- d
      # Rate groups intentionally keep blank strings; this is the rate API's
      # existing policy, distinct from Table 1's blank-to-missing conversion.
      for (keep_missing in c(FALSE, TRUE)) for (event in c("bin", "event2")) {
        eligible <- !is.na(d$time) & !is.na(d$entry) & d$time > d$entry & !is.na(d$fw) & d$fw > 0
        if (!keep_missing) eligible <- eligible & !is.na(d$g)
        r <- tt_rates(d, "time", event, by = "g", entry = "entry", fweight = "fw",
                      missing = keep_missing, float_time = FALSE)
        s <- imd_ledger(r)
        id <- imd_population(s, "table")
        imd_counts(s, id, c(input_n = nrow(d), eligible_n = sum(eligible), used_n = sum(eligible),
          observed_n = sum(eligible & !is.na(d[[event]])), missing_n = sum(eligible & is.na(d[[event]])),
          excluded_n = sum(!eligible), zero_weight_n = sum(d$fw == 0, na.rm = TRUE)))
        imd_weights(s, id, d$fw[eligible])
        for (i in seq_len(nrow(r))) {
          g <- r$g[i]
          group <- if (is.na(g)) is.na(d$g) else !is.na(d$g) & d$g == g
          use <- eligible & group
          D <- sum(d$fw[use] * (!is.na(d[[event]][use]) & d[[event]][use] != 0))
          Y <- sum(d$fw[use] * (d$time[use] - d$entry[use]))
          expected <- c(D, Y, D / Y, if (D > 0) exp(log(D / Y) - stats::qnorm(.975) / sqrt(D)) else NA_real_,
            if (D > 0) exp(log(D / Y) + stats::qnorm(.975) / sqrt(D)) else NA_real_)
          expect_equal(as.numeric(r[i, c("D", "Y", "Rate", "Lower", "Upper")]), expected, tolerance = 1e-12)
          gid <- s$populations$id[s$populations$scope == "group"][i]
          imd_counts(s, gid, c(input_n = sum(group), used_n = sum(use), observed_n = sum(use & !is.na(d[[event]])),
            missing_n = sum(use & is.na(d[[event]])), zero_weight_n = sum(group & !is.na(d$fw) & d$fw == 0)))
          imd_weights(s, gid, d$fw[use])
        }
        expect_identical(imd_ledger(tt_rates(r, level = .95)), s)
      }
      # A supplied explicit subset removes the blank category before the
      # rate-table renderer, which refuses blank category labels.
      ds <- d[!is.na(d$g) & as.character(d$g) != "", ]
      rates <- lapply(c("bin", "event2"), function(event)
        tt_rates(ds, "time", event, by = "g", entry = "entry", fweight = "fw", float_time = FALSE))
      t <- stratetab(rates, outcomes = 2)
      ledger <- imd_ledger(t)
      ids <- ledger$populations$id[ledger$populations$scope == "table"]
      expect_length(ids, 2L)
      for (i in seq_along(rates)) {
        original <- imd_ledger(rates[[i]])
        oid <- imd_population(original, "table")
        expect_identical(imd_measure(ledger, ids[i], c("input_n", "used_n", "observed_n"))$value,
                         imd_measure(original, oid, c("input_n", "used_n", "observed_n"))$value)
      }
      expect_identical(d, before)
    })
  })
}
