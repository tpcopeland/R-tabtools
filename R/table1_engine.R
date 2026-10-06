# table1_tc statistics (plan tasks 2.5, 2.8, 2.9), unweighted: a port of
# _t1tcfc_collect_mata() and the test/SMD loop of _desctab_collect.ado.

# Source populations of a descriptive calculation. The variable masks are
# independent of the displayed cells: categorical missing values may be
# retained, while geometric summaries additionally require positive values.
# Input vectors are temporary evidence; the returned ledger stores counts
# and weight diagnostics only.
.t1_sample_accounting <- function(input, specs, gp, o, wtcompare) {
  gid <- gp$gid
  eligible <- !is.na(gid)
  raw_weights <- input$weights[input$keep]
  group_masks <- lapply(seq_len(gp$G), function(g) eligible & gid == g)
  original_masks <- if (is.null(input$group)) list(rep(TRUE, input$n)) else {
    kept_group <- input$group[input$keep]
    lapply(seq_len(gp$G), function(g) {
      value <- kept_group[which(group_masks[[g]])[1L]]
      !is.na(input$group) & input$group == value
    })
  }
  missing_group <- if (is.null(input$group)) rep(FALSE, input$n) else is.na(input$group)
  weighted_type <- switch(input$kind, wt = "importance", fw = "frequency", "none")
  passes <- if (wtcompare) c("crude", "weighted") else "main"
  samples <- list()
  add <- function(x) samples[[length(samples) + 1L]] <<- x
  selection_exclusions <- function(mask, include_group) {
    out <- list()
    if (input$kind != "none") {
      out <- list(
        .tt_sample_exclusion("input_to_eligible", "missing_weight",
          sum(mask & is.na(input$weights)), "original_input_weights"),
        .tt_sample_exclusion("input_to_eligible", "zero_weight",
          sum(mask & !is.na(input$weights) & input$weights == 0), "original_input_weights")
      )
    }
    if (include_group && !is.null(input$group)) {
      out[[length(out) + 1L]] <- .tt_sample_exclusion("input_to_eligible", "missing_group",
        sum(mask & input$keep & missing_group), "normalized_group_after_weight_eligibility")
    }
    if (length(out)) do.call(rbind, out) else NULL
  }
  for (pass in passes) {
    crude <- pass == "crude"
    weights <- if (crude) rep(1, length(gid)) else raw_weights
    weight_type <- if (crude) "none" else weighted_type
    component <- if (wtcompare) pass else ""
    prefix <- if (wtcompare) paste0(pass, "/") else ""
    reported <- function(mask) if (weight_type == "frequency") sum(weights[mask]) else sum(mask)
    population <- function(id, scope, variable = NA_character_, group = NA_character_,
                           spec = NA_real_, values, bases, reasons = list(), exclusions = NULL,
                           used) {
      diagnostic <- .tt_sample_weights(weights[used])
      .tt_sample_population(paste0(prefix, id), command = "table1_tc", scope = scope,
        component = component, variable = variable, group = group, spec = spec,
        weight_type = weight_type, values = c(values, diagnostic$values),
        bases = c(bases, diagnostic$bases), reasons = c(reasons, diagnostic$reasons),
        statuses = diagnostic$statuses, exclusions = exclusions)
    }
    overall_reported <- is.null(input$group) || o$total != "none"
    add(population("table", "table",
      values = list(input_n = input$n, eligible_n = sum(eligible), observed_n = NA_real_,
        used_n = sum(eligible), missing_n = NA_real_,
        zero_weight_n = sum(!is.na(input$weights) & input$weights == 0),
        excluded_n = input$n - sum(eligible),
        reported_n = if (overall_reported) reported(eligible) else NA_real_),
      bases = list(input_n = "original_input_records", eligible_n = "weight_and_group_eligibility",
        used_n = "weight_and_group_eligibility", zero_weight_n = "original_input_weights",
        excluded_n = "original_input_to_eligible_records", reported_n = "native_aggregate_header_N"),
      reasons = list(observed_n = "variable_specific_observation_masks",
        missing_n = "variable_specific_missingness",
        reported_n = if (overall_reported) "" else "no_aggregate_header_reported"),
      exclusions = selection_exclusions(rep(TRUE, input$n), TRUE), used = eligible))
    for (g in seq_len(gp$G)) {
      mask <- group_masks[[g]]
      original <- original_masks[[g]]
      add(population(paste0("group/", g), "group", group = as.character(gp$codes[g]),
        values = list(input_n = sum(original), eligible_n = sum(mask), observed_n = NA_real_,
          used_n = sum(mask), missing_n = NA_real_,
          zero_weight_n = sum(original & !is.na(input$weights) & input$weights == 0),
          excluded_n = sum(original) - sum(mask), reported_n = reported(mask)),
        bases = list(input_n = "original_records_in_group", eligible_n = "weight_and_group_eligibility",
          used_n = "weight_and_group_eligibility", zero_weight_n = "original_input_weights",
          excluded_n = "original_group_to_eligible_records", reported_n = "native_group_header_N"),
        reasons = list(observed_n = "variable_specific_observation_masks",
          missing_n = "variable_specific_missingness"),
        exclusions = selection_exclusions(original, FALSE), used = mask))
    }
    for (i in seq_along(specs)) {
      spec <- specs[[i]]
      x <- .t1_values(spec$x)
      observed <- !is.na(x)
      usable <- observed
      include_missing <- spec$type %in% c("cat", "cate") && isTRUE(o$missing)
      if (include_missing) usable[] <- TRUE
      nonpositive <- if (spec$type == "contln") observed & x <= 0 else rep(FALSE, length(x))
      if (spec$type == "contln") usable <- usable & x > 0
      masks <- c(list(eligible), group_masks)
      for (j in seq_along(masks)) {
        mask <- masks[[j]]
        used <- mask & usable
        missing_n <- sum(mask & !observed)
        exclusions <- list(.tt_sample_exclusion(
          if (include_missing) "retained" else "eligible_to_used",
          if (include_missing) "missing_category" else "missing_variable", missing_n,
          "resolved_variable_on_eligible_records"))
        if (spec$type == "contln") {
          exclusions[[length(exclusions) + 1L]] <- .tt_sample_exclusion("eligible_to_used",
            "nonpositive_geometric_value", sum(mask & nonpositive), "resolved_contln_positive_value_policy")
        }
        group <- if (j == 1L) NA_character_ else as.character(gp$codes[j - 1L])
        native_denominator <- j > 1L || overall_reported
        add(population(paste0("variable/", i, "/", if (j == 1L) "table" else paste0("group/", j - 1L)),
          "variable", variable = spec$name, group = group, spec = i,
          values = list(input_n = sum(mask), eligible_n = sum(mask), observed_n = sum(mask & observed),
            used_n = sum(used), missing_n = missing_n, zero_weight_n = 0,
            excluded_n = sum(mask) - sum(used),
            reported_n = if (native_denominator) reported(used) else NA_real_),
          bases = list(input_n = "eligible_scope_records", eligible_n = "eligible_scope_records",
            observed_n = "nonmissing_resolved_variable", used_n = "resolved_variable_missing_and_type_policy",
            missing_n = "missing_resolved_variable", zero_weight_n = "positive_weight_eligible_scope",
            excluded_n = "eligible_scope_to_used_records", reported_n = "native_variable_N"),
          reasons = list(reported_n = if (native_denominator) "" else "no_aggregate_denominator_reported"),
          exclusions = do.call(rbind, exclusions), used = used))
      }
    }
  }
  # These are scoped populations of one source, not separate sources to
  # prefix as a composite. Preserve each pass's component unchanged.
  out <- list(version = 1L,
    populations = do.call(rbind, lapply(samples, `[[`, "populations")),
    measures = do.call(rbind, lapply(samples, `[[`, "measures")),
    exclusions = do.call(rbind, lapply(samples, `[[`, "exclusions")))
  for (field in c("populations", "measures", "exclusions")) rownames(out[[field]]) <- NULL
  .tt_validate_sample_accounting(out)
  out
}

# Sum in data order in IEEE double, as Mata's sum() does (src/stata_sum.c).
# R's sum() accumulates in long double on x86_64 and can round a display tie
# the other way (Phase 2 review P0-1).
.stata_sum <- function(x) .Call(tt_seqsum_c, as.double(x))

# Mean and SD the way _t1tcfc_collect_mata computes them
# (_desctab_collect.ado:1883-1902, tabtools 2.1.15 F05): sums in data order,
# ss about the mean (.t1_centered_ss()), tiny negative ss clamped to 0,
# var = ss / (n - 1). A negative var (ss < -1e-8) has no square root in
# Mata, so the SD is missing and prints as "." (before the centered SD, a
# constant 2.675 in 15,000 rows gave "2.68\u00b1."; now "2.68\u00b10.00").
#
# Overflow (Milestone H, H10; probed in Stata 17 on tabtools 2.1.11 and
# 2.1.12): a sum
# beyond the double range is missing in Mata, so a mean whose sum
# overflows is missing (a blank cell) and so is an SD whose squares
# overflow ("2.e+200\u00b1." for 1e200, 2e200, 3e200). Mata evaluates
# `sx * sx / n` in one expression with extended range (`a * a / b` with
# a = 5.5e201, b = 1e201 gives 3.025e202 in Mata although `a * a` alone is
# missing), so a finite quotient of an overflowing square is kept: R forms
# it as sx * (sx / n) when sx * sx is out of range. Stata's range ends at
# 8.99e307 (R's at 1.80e308), and R applies Stata's limit (.st_sum()).
# Corrected two-pass sum of squares as Mata forms it in tabtools >= 2.1.15
# (_desctab_collect.ado:1893-1901, F05): with wdev = w :* (y :- mean) and
# wdev2 = wdev :* (y :- mean), ss = sum(wdev2) - sum(wdev)^2 / sw. The raw
# moment sum(w y^2) (`sx2`, NA when it overflows) still decides overflow
# exactly as before: ss is missing when sx2 or the mean is missing or any
# wdev2 is beyond Stata's range. Tiny negatives are clamped to 0 (:1901).
.t1_centered_ss <- function(w, y, mean, sw, sx2) {
  if (is.na(mean) || is.na(sx2)) return(NA_real_)
  d <- y - mean
  wdev <- w * d
  wdev2 <- wdev * d
  if (!all(.st_ok(wdev)) || !all(.st_ok(wdev2))) return(NA_real_)
  sd1 <- .st_sum(wdev)
  if (is.na(sd1)) return(NA_real_)
  # Mata's `sum(wdev)^2 / swg` (:1899) has no extended range, unlike
  # `a * a / b`: an overflowing square is missing (Stata 17: a = -6.86e156,
  # b = 3e40 gives "." for a^2/b but 1.5708e273 for a*a/b).
  sq <- sd1 * sd1
  if (!.st_ok(sq)) return(NA_real_)
  ss <- .st_num(.st_sum(wdev2) - sq / sw)
  if (!is.na(ss) && ss < 0 && ss > -1e-8) ss <- 0
  ss
}

.t1_mean_sd <- function(y) {
  n <- length(y)
  if (!n) return(c(mean = NA_real_, sd = NA_real_))
  sx <- .st_sum(y)
  if (is.na(sx)) return(c(mean = NA_real_, sd = NA_real_))
  ss <- .t1_centered_ss(rep(1, n), y, sx / n, n, .st_sum(y * y))
  v <- if (n > 1) ss / (n - 1) else NA_real_
  c(mean = sx / n, sd = if (!is.na(v) && v >= 0) sqrt(v) else NA_real_)
}

# Stata's largest double is (2 - 2^-52) * 2^1022 (maxdouble(), probed);
# 2^1023 and beyond are missing values. A value or sum outside that range
# is missing (.st_num()); .st_sum() is .stata_sum() with Mata's overflow:
# missing when a term or the total is beyond the range.
.stata_missing_from <- 2^1023
.st_ok <- function(x) is.finite(x) & abs(x) < .stata_missing_from
.st_num <- function(x) if (length(x) == 1L && !is.na(x) && .st_ok(x)) x else NA_real_
.st_sum <- function(x) {
  if (!all(.st_ok(x))) return(NA_real_)
  .st_num(.stata_sum(x))
}

# a * a / b as Mata evaluates `sx * sx / n` (extended range inside one
# expression, probed): the double product when in range, else a * (a / b);
# missing when the quotient itself is out of range. The ESS uses it too
# (tabtools 2.1.12 forms `sum w * (sum w / sum w^2)` when `(sum w)^2`
# overflows; see .t1w_ess()).
.t1_sq_over <- function(a, b) {
  aa <- a * a
  .st_num(if (.st_ok(aa)) aa / b else a * (a / b))
}

# _t1tcfc_wquantile() with unit weights (_desctab_collect.ado:1340-1367):
# the first order statistic whose cumulative count exceeds p * n, or the
# average of it and the next when the count hits p * n within 1e-10.
.t1_quantile <- function(x, p) {
  n <- length(x)
  if (!n) return(NA_real_)
  xs <- sort(x)
  target <- p * n
  tol <- 1e-10 * max(1, n)
  i <- round(target)
  if (i >= 1 && abs(i - target) <= tol) {
    return(if (i < n) (xs[i] + xs[i + 1L]) / 2 else xs[i])
  }
  xs[min(floor(target) + 1L, n)]
}

# Summary for one continuous variable in one column (_desctab_collect.ado:
# 1512-1559). contln uses log(x) over x > 0 only, and its n counts x > 0.
.t1_cont_stats <- function(x, type) {
  x <- x[!is.na(x)]
  if (type == "contln") x <- x[x > 0]
  n <- length(x)
  if (!n) return(list(n = 0, a = NA_real_, b = NA_real_, c = NA_real_))
  if (type == "conts") {
    return(list(n = n, a = .t1_quantile(x, 0.5), b = .t1_quantile(x, 0.25),
                c = .t1_quantile(x, 0.75)))
  }
  y <- if (type == "contln") log(x) else x
  ms <- .t1_mean_sd(y)
  if (type == "contln") {
    return(list(n = n, a = exp(ms[["mean"]]), b = exp(ms[["sd"]]), c = NA_real_))
  }
  list(n = n, a = ms[["mean"]], b = ms[["sd"]], c = NA_real_)
}

# Arguments test_args may set, per test function: the formals of the stats
# method minus the data arguments the engine supplies (x, y, g, formula,
# data, subset, na.action). t.test() and wilcox.test() are generics whose
# `...` would swallow a misspelling, so the list is explicit.
.t1_test_arg_names <- list(
  t.test = c("alternative", "mu", "paired", "var.equal", "conf.level"),
  oneway.test = "var.equal",
  wilcox.test = c("alternative", "mu", "paired", "exact", "correct", "conf.int", "conf.level",
                  "tol.root", "digits.rank"),
  kruskal.test = character(),
  chisq.test = c("correct", "p", "rescale.p", "simulate.p.value", "B"),
  fisher.test = c("workspace", "hybrid", "hybridPars", "control", "or", "alternative", "conf.int",
                  "conf.level", "simulate.p.value", "B")
)

# Validate table1_tc(test_args =): a named list keyed by the six test
# functions, each a named list of arguments that function accepts. One-sided
# alternatives are refused because the methods paragraph states two-sided
# p-values (Stata's tests are two-sided).
.t1_check_test_args <- function(test_args) {
  if (is.null(test_args)) return(invisible(NULL))
  fns <- names(.t1_test_arg_names)
  if (!is.list(test_args) || is.null(names(test_args)) || any(!nzchar(names(test_args))) ||
      !all(names(test_args) %in% fns)) {
    cli::cli_abort("{.arg test_args} must be a named list keyed by {.or {.val {fns}}}.", call = NULL)
  }
  for (fn in names(test_args)) {
    a <- test_args[[fn]]
    if (!is.list(a) || (length(a) && (is.null(names(a)) || any(!nzchar(names(a)))))) {
      cli::cli_abort("{.code test_args${fn}} must be a named list of arguments.", call = NULL)
    }
    bad <- setdiff(names(a), .t1_test_arg_names[[fn]])
    if (length(bad)) {
      ok <- .t1_test_arg_names[[fn]]
      cli::cli_abort(c("{.code test_args${fn}} has unknown or reserved argument{?s} {.arg {bad}}.",
                       "i" = if (length(ok)) "{.fn {fn}} accepts {.arg {ok}}." else
                         "{.fn {fn}} takes no extra arguments here."),
                     call = NULL)
    }
    if (!is.null(a$alternative) && !identical(a$alternative, "two.sided")) {
      cli::cli_abort(c("{.code test_args${fn}$alternative} must be {.val two.sided}.",
                       "i" = "table1_tc reports two-sided p-values, as Stata does, and its methods text says so."),
                     call = NULL)
    }
    # Switches must be a single TRUE/FALSE. `paired = 1` used to skip the
    # labelled paired branch (isTRUE(1) is FALSE) while stats::t.test() ran
    # a paired test under a Welch label (Milestone H review R1).
    switches <- c("paired", "var.equal", "simulate.p.value", "conf.int", "hybrid",
                  if (fn == "chisq.test") "correct")
    for (sw in intersect(switches, names(a))) {
      v <- a[[sw]]
      if (!is.logical(v) || length(v) != 1L || is.na(v)) {
        cli::cli_abort("{.code test_args${fn}${sw}} must be TRUE or FALSE.", call = NULL)
      }
    }
    if ("exact" %in% names(a) && !is.null(a$exact) &&
        (!is.logical(a$exact) || length(a$exact) != 1L || is.na(a$exact))) {
      cli::cli_abort("{.code test_args${fn}$exact} must be TRUE, FALSE, or NULL.", call = NULL)
    }
    # A shifted null (mu != 0) has no Stata analogue, and a baseline table
    # compares the groups with each other; the label and methods text could
    # not say what was tested (Milestone H, H7).
    if (!is.null(a$mu) && !(is.numeric(a$mu) && length(a$mu) == 1L && !is.na(a$mu) && a$mu == 0)) {
      cli::cli_abort(c("{.code test_args${fn}$mu} must be 0.",
                       "i" = "table1_tc tests whether the groups differ; a shifted null hypothesis has no Stata equivalent."),
                     call = NULL)
    }
  }
  invisible(NULL)
}

# Errors that mean the data cannot support the test (Stata's `capture` leaves
# p missing for these too); any other error is reported with a warning.
.t1_data_error <- paste(
  c("not enough", "essentially constant", "same group", "at least 2", "at least one entry",
    "all observations", "must have at least", "FEXACT", "LDSTP", "LDKEY", "workspace",
    "hash table key", "grouping factor", "all group levels"),
  collapse = "|")

# Run an R test, merging the user's test_args; NULL on error. An error that
# is not about the data is reported with a warning naming `var`. `impl`
# replaces the stats function (the frequency-weighted forms, which take the
# same test_args); the fweight expansion cap is re-raised, not swallowed.
.t1_run <- function(fn, args, test_args, var = NULL, warn = TRUE, impl = NULL) {
  extra <- test_args[[fn]]
  if (!is.null(extra)) args[names(extra)] <- extra
  f <- impl %||% switch(fn, t.test = stats::t.test, oneway.test = stats::oneway.test,
                        wilcox.test = stats::wilcox.test, kruskal.test = stats::kruskal.test,
                        chisq.test = stats::chisq.test, fisher.test = stats::fisher.test)
  res <- tryCatch(suppressWarnings(do.call(f, args)), error = function(e) e)
  if (!inherits(res, "error")) return(res)
  if (inherits(res, "tabtools_fw_limit")) stop(res)
  if (warn) .t1_test_failed(fn, res, var)
  NULL
}

.t1_test_failed <- function(fn, e, var) {
  msg <- conditionMessage(e)
  if (grepl(.t1_data_error, msg)) return(invisible())
  where <- if (is.null(var)) "" else paste0(" for ", var)
  cli::cli_warn(c("{.fn {fn}} failed{where}; its p-value is left blank.", "x" = "{msg}"), call = NULL)
}

# Monte Carlo replicates for the simulated Fisher fallback, and the internal
# seed that makes any simulated Fisher p-value reproducible.
.t1_fisher_B <- 1e5
.t1_fisher_seed <- 20260925L

# fisher.test by the network algorithm, starting from R's default workspace
# and enlarging it tenfold up to 2e7 (80 MB), so small tables stay fast and
# large ones fail over quickly (a 6 x 3 table with n = 2,000 ran 37 s at a
# fixed 2e8 and still failed). If the exact test cannot run, the p-value is
# simulated from .t1_fisher_B Monte Carlo replicates. Any simulation (the
# fallback or the user's simulate.p.value = TRUE) runs under a fixed internal
# seed via withr::with_seed(), which restores the caller's RNG state and
# kind, so the same call always gives the same p-value. A user-supplied
# workspace is used as given. The result carries `B` (replicates; NA for
# the exact test).
.t1_fisher <- function(tab, test_args, var = NULL) {
  ta <- test_args$fisher.test %||% list()
  simulate <- isTRUE(ta$simulate.p.value)
  if (!simulate) {
    sizes <- if (!is.null(ta$workspace)) ta$workspace else c(2e5, 2e6, 2e7)
    rest <- ta[setdiff(names(ta), c("workspace", "simulate.p.value", "B"))]
    for (ws in sizes) {
      res <- tryCatch(suppressWarnings(do.call(stats::fisher.test, c(list(x = tab, workspace = ws), rest))),
                      error = function(e) e)
      if (!inherits(res, "error")) {
        res$B <- NA_real_
        return(res)
      }
      # Only a workspace/network failure moves on; anything else is final.
      if (!grepl("FEXACT|LDSTP|LDKEY|workspace|hash table", conditionMessage(res))) {
        .t1_test_failed("fisher.test", res, var)
        return(NULL)
      }
    }
  }
  B <- if (simulate && !is.null(ta$B)) ta$B else if (simulate) 2000 else .t1_fisher_B
  rest <- ta[setdiff(names(ta), c("workspace", "simulate.p.value", "B"))]
  res <- withr::with_seed(.t1_fisher_seed,
                          .t1_run("fisher.test", c(list(x = tab, simulate.p.value = TRUE, B = B), rest),
                                  NULL, var),
                          .rng_kind = "Mersenne-Twister", .rng_normal_kind = "Inversion",
                          .rng_sample_kind = "Rejection")
  if (!is.null(res)) res$B <- B
  res
}

# Statistic-column text. A "%6.2f" in `fmt` takes a double and is filled by
# the exact formatter (.tt_fmt_f(), right-aligned in 6 characters), not by
# the C runtime's printf, whose rounding differs on Windows.
.t1_stat_text <- function(fmt, ...) {
  args <- list(...)
  convs <- regmatches(fmt, gregexpr("%[0-9.]*[a-zA-Z]", fmt))[[1]]
  for (i in which(convs == "%6.2f")) args[[i]] <- sprintf("%6s", .tt_fmt_f(args[[i]], 2L))
  do.call(sprintf, c(list(gsub("%6.2f", "%s", fmt, fixed = TRUE)), args))
}

# Do the first two (frequency-weighted) moments of y overflow the double
# range?
.t1_moments_overflow <- function(y, w = NULL) {
  if (!length(y)) return(FALSE)
  w <- w %||% rep(1, length(y))
  s1 <- sum(w * y)
  if (!is.finite(s1)) return(TRUE)
  m <- s1 / sum(w)
  !is.finite(sum(w * (y - m)^2))
}

#' Hypothesis test for one variable (plan section 2 test map)
#'
#' Stata runs a test when at least two groups have data and, for categorical
#' and binary variables, the variable has at least two observed levels
#' (_desctab_collect.ado:340-497); R's conventional tests replace Stata's
#' (Welch t / Welch ANOVA for contn and contln on the log scale, Wilcoxon
#' rank-sum / Kruskal-Wallis for conts, uncorrected Pearson chi-squared for
#' cat/bin, Fisher's exact for cate/bine). A test that fails leaves p missing
#' and the test cells blank, as Stata's `capture` does.
#'
#' The Test label follows the method R actually ran, so `test_args` that
#' change the test (e.g. `chisq.test = list(correct = TRUE)` on a 2 x 2
#' table) are labelled as such. A simulated Fisher p-value (the fallback, or
#' `simulate.p.value = TRUE`) is labelled `Fisher's exact (simulated)`.
#' `paired = TRUE` (t.test, wilcox.test; two groups) gives `Paired t test`
#' and `Wilcoxon signed-rank`; the pairs are formed by row order within the
#' two groups (.t1_pairs()).
#'
#' @param var Variable name, for warnings about failed tests.
#' @param fw Frequency weights (positive integers, one per element of `v`):
#'   the test is R's test on the data expanded by `fw`, computed from the
#'   frequencies (.t1w_test_fw()).
#' @return list(p, test, statistic, used, simulated) where `used` names the
#'   method for the methods paragraph (`"fisher_sim:<B>"` for a simulated
#'   Fisher p-value) and `simulated` flags a Monte Carlo p-value.
#' @keywords internal
#' @noRd
.t1_test <- function(type, v, g, include_missing, test_args, var = NULL, fw = NULL) {
  none <- list(p = NA_real_, test = "", statistic = "", used = NA_character_, simulated = FALSE)
  if (type %in% c("contn", "contln", "conts")) {
    keep <- !is.na(v) & !is.na(g)
    if (type == "contln") keep <- keep & v > 0
    y <- if (type == "contln") log(v[keep]) else v[keep]
    gg <- factor(g[keep])
    wk <- fw[keep]
    k <- nlevels(gg)
    if (k < 2L) return(none)
    paired_fn <- if (type == "conts") "wilcox.test" else "t.test"
    if (k > 2L && isTRUE(test_args[[paired_fn]]$paired)) {
      cli::cli_abort(c("A paired test for {.var {var %||% 'this variable'}} needs exactly two groups, not {k}.",
                       "i" = "Drop {.code paired = TRUE}, or restrict {.arg by} to two groups."),
                     call = NULL)
    }
    # Moments that overflow give no t/ANOVA p-value (Stata's regress-based
    # test leaves p blank there too), never a spurious p of 1 from an
    # infinite standard error (H10).
    if (type != "conts" && .t1_moments_overflow(y, wk)) return(none)
    logged <- if (type == "contln") ", logged data" else ""
    if (type == "conts") {
      if (k == 2L && isTRUE(test_args$wilcox.test$paired)) {
        pr <- .t1_pairs(type, v, g, levels(gg), fw, var)
        r <- .t1_run("wilcox.test", list(x = pr$x, y = pr$y), test_args, var)
        if (is.null(r)) return(none)
        return(list(p = r$p.value, test = "Wilcoxon signed-rank",
                    statistic = .t1_stat_text("V=%s", .tt_fmt_sig(r$statistic, 7L)),
                    used = "signrank", simulated = FALSE))
      }
      if (k == 2L) {
        lv <- levels(gg)
        i1 <- gg == lv[1]
        i2 <- gg == lv[2]
        r <- if (is.null(wk)) .t1_run("wilcox.test", list(x = y[i1], y = y[i2]), test_args, var) else
          .t1_run("wilcox.test", list(x = y[i1], y = y[i2], wx = wk[i1], wy = wk[i2]), test_args, var,
                  impl = .t1_wilcox_fw)
        if (is.null(r)) return(none)
        return(list(p = r$p.value, test = "Wilcoxon rank-sum",
                    statistic = .t1_stat_text("W=%s", .tt_fmt_sig(r$statistic, 7L)),
                    used = "wilcoxon", simulated = FALSE))
      }
      r <- if (is.null(wk)) .t1_run("kruskal.test", list(x = y, g = gg), test_args, var) else
        .t1_run("kruskal.test", list(x = y, g = gg, w = wk), test_args, var, impl = .t1_kruskal_fw)
      if (is.null(r)) return(none)
      return(list(p = r$p.value, test = "Kruskal-Wallis",
                  statistic = .t1_stat_text("Chi2(%d)=%6.2f", as.integer(r$parameter), r$statistic),
                  used = "kruskal", simulated = FALSE))
    }
    if (k == 2L && isTRUE(test_args$t.test$paired)) {
      pr <- .t1_pairs(type, v, g, levels(gg), fw, var)
      r <- .t1_run("t.test", list(x = pr$x, y = pr$y), test_args, var)
      if (is.null(r)) return(none)
      return(list(p = r$p.value, test = paste0("Paired t test", logged),
                  statistic = .t1_stat_text("t(%s)=%6.2f", .tt_fmt_sig(round(r$parameter, 1), 7L),
                                            r$statistic),
                  used = "paired_t", simulated = FALSE))
    }
    if (k == 2L) {
      lv <- levels(gg)
      i1 <- gg == lv[1]
      i2 <- gg == lv[2]
      args <- list(x = y[i1], y = y[i2])
      impl <- NULL
      if (!is.null(wk)) {
        args <- c(args, list(wx = wk[i1], wy = wk[i2]))
        impl <- .t1_ttest_fw
      }
      # Welch fails with one observation in a group; the pooled test then
      # runs (Stata's regress-based p). Only the second failure warns.
      r <- .t1_run("t.test", args, test_args, var, warn = FALSE, impl = impl)
      if (is.null(r)) r <- .t1_run("t.test", c(args, list(var.equal = TRUE)), test_args, var, impl = impl)
      if (is.null(r)) return(none)
      welch <- !isTRUE(grepl("Two Sample t-test", r$method) && !grepl("Welch", r$method))
      return(list(p = r$p.value,
                  test = paste0(if (welch) "Welch t test" else "t test", logged),
                  statistic = .t1_stat_text("t(%s)=%6.2f", .tt_fmt_sig(round(r$parameter, 1), 7L),
                                            r$statistic),
                  used = if (welch) "welch_t" else "t", simulated = FALSE))
    }
    args <- list(formula = y ~ gg)
    impl <- NULL
    if (!is.null(wk)) {
      args <- list(y = y, g = gg, w = wk)
      impl <- .t1_oneway_fw
    }
    r <- .t1_run("oneway.test", args, test_args, var, warn = FALSE, impl = impl)
    if (is.null(r) || is.na(r$p.value)) {
      r <- .t1_run("oneway.test", c(args, list(var.equal = TRUE)), test_args, var, impl = impl)
    }
    if (is.null(r)) return(none)
    welch <- grepl("not assuming", r$method)
    return(list(p = r$p.value,
                test = paste0(if (welch) "Welch ANOVA" else "ANOVA", logged),
                statistic = .t1_stat_text("F(%s,%s)=%6.2f", .tt_fmt_sig(round(r$parameter[1], 1), 7L),
                                          .tt_fmt_sig(round(r$parameter[2], 1), 7L), r$statistic),
                used = if (welch) "welch_anova" else "anova", simulated = FALSE))
  }
  # Categorical and binary: table of level by group.
  keep <- !is.na(g)
  if (!(type %in% c("cat", "cate") && include_missing)) keep <- keep & !is.na(v)
  vv <- v[keep]
  gg <- g[keep]
  if (length(unique(gg)) < 2L) return(none)
  vf <- if (type %in% c("cat", "cate") && include_missing) addNA(factor(vv), ifany = TRUE) else factor(vv)
  if (length(unique(v[!is.na(g) & !is.na(v)])) + (type %in% c("cat", "cate") && include_missing &&
                                                   any(is.na(v[!is.na(g)]))) < 2L) return(none)
  # Under fweight the table holds the frequencies (the expanded data's table).
  tab <- if (is.null(fw)) table(vf, factor(gg)) else .t1_fw_table(vf, factor(gg), fw[keep])
  if (type %in% c("cat", "bin")) {
    run <- function() .t1_run("chisq.test", list(x = tab, correct = FALSE), test_args, var)
    # chisq.test(simulate.p.value = TRUE) draws random tables: fixed seed.
    r <- if (isTRUE(test_args$chisq.test$simulate.p.value)) {
      withr::with_seed(.t1_fisher_seed, run(), .rng_kind = "Mersenne-Twister",
                       .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
    } else run()
    if (is.null(r) || is.na(r$p.value)) return(none)
    yates <- grepl("Yates", r$method, fixed = TRUE)
    sim <- grepl("simulated", r$method, fixed = TRUE)
    stat <- if (sim) .t1_stat_text("Chi2=%6.2f", r$statistic) else
      .t1_stat_text("Chi2(%d)=%6.2f", as.integer(r$parameter), r$statistic)
    return(list(p = r$p.value,
                test = paste0("Pearson's chi-squared", if (yates) " (Yates)" else if (sim) " (simulated)" else ""),
                statistic = stat,
                used = if (yates) "chisq_yates" else if (sim) paste0("chisq_sim:", test_args$chisq.test$B %||% 2000)
                       else "chisq",
                simulated = sim))
  }
  r <- .t1_fisher(tab, test_args, var)
  if (is.null(r)) return(none)
  if (!is.na(r$B)) {
    return(list(p = r$p.value, test = "Fisher's exact (simulated)", statistic = "N/A",
                used = paste0("fisher_sim:", r$B), simulated = TRUE))
  }
  list(p = r$p.value, test = "Fisher's exact", statistic = "N/A", used = "fisher", simulated = FALSE)
}

# Pairs for a paired test (test_args `paired = TRUE`, two groups): the
# analysis values of each group in row order, missing values kept (a
# non-positive contln value is missing on the log scale), so the i-th record
# of the first group is paired with the i-th record of the second and
# t.test()/wilcox.test() drop incomplete pairs. Removing missing values per
# group first would silently shift the pairing. Under fweight the pairs are
# those of the expanded data (each record repeated `fw` times, in row
# order), as for every fweight test. Groups of unequal size cannot be
# paired: an error, not a blank p-value.
.t1_pairs <- function(type, v, g, lv, fw = NULL, var = NULL) {
  y <- if (type == "contln") ifelse(!is.na(v) & v > 0, log(pmax(v, 0)), NA_real_) else v
  pick <- function(l) {
    i <- which(!is.na(g) & as.character(g) == l)
    if (is.null(fw)) y[i] else .t1_fw_expand(y[i], fw[i])
  }
  x1 <- pick(lv[1])
  x2 <- pick(lv[2])
  if (length(x1) != length(x2)) {
    what <- if (is.null(var)) "" else paste0(" for {.var ", var, "}")
    cli::cli_abort(c(paste0("A paired test", what, " needs two groups of equal size."),
                     "x" = "The groups have {length(x1)} and {length(x2)} record{?s}.",
                     "i" = "Pairs are formed by row order within the two groups of {.arg by}; sort the data so that the i-th record of each group belongs to the same pair."),
                   call = NULL)
  }
  list(x = x1, y = x2)
}

# Paired tests under fweight (review R3). A pair is the i-th record of each
# of the two groups, in row order, as without weights; its frequency must be
# the same on both sides (a pair observed f times). Pair-mates with
# different frequencies, or a zero/missing frequency on one side only, would
# shift every later pair after expansion, so they are refused. Checked on
# the records before zero and missing frequencies drop them; with equal
# frequencies, expanding each record and pairing by position gives each
# pair f times.
.t1_check_fw_pairs <- function(data, by, fweight, test_args) {
  if (is.null(fweight) || is.null(by) || !is.character(fweight) || length(fweight) != 1L ||
      !fweight %in% names(data)) return(invisible(NULL))
  if (!isTRUE(test_args$t.test$paired) && !isTRUE(test_args$wilcox.test$paired)) return(invisible(NULL))
  g <- data[[by]]
  lv <- unique(g[!is.na(g)])
  if (length(lv) != 2L) return(invisible(NULL))
  w <- suppressWarnings(as.numeric(unclass(data[[fweight]])))
  w[is.na(w)] <- 0
  f1 <- w[!is.na(g) & g == lv[1]]
  f2 <- w[!is.na(g) & g == lv[2]]
  if (length(f1) != length(f2)) {
    cli::cli_abort(c("A paired test needs two groups of equal size.",
                     "x" = "The groups have {length(f1)} and {length(f2)} record{?s}.",
                     "i" = "Pairs are formed by row order within the two groups of {.arg by}."),
                   call = NULL)
  }
  bad <- which(f1 != f2)
  if (length(bad)) {
    which_pairs <- if (length(bad) == 1L) paste("Pair", bad, "has") else
      paste("Pairs", paste(utils::head(bad, 10), collapse = ", "), if (length(bad) > 10L) "..." else "", "have")
    which_pairs <- gsub(" +", " ", which_pairs)
    cli::cli_abort(c("A paired test under {.arg fweight} needs the same frequency on both members of each pair.",
                     "x" = "{which_pairs} different frequencies (pairs by row order within the groups).",
                     "i" = "A pair observed f times carries frequency f on both of its records."),
                   call = NULL)
  }
  invisible(NULL)
}

# Yang-Dalton categorical SMD (_t1tcfc_cat_smd, _desctab_collect.ado:1369-1389).
.t1_cat_smd <- function(p1, p2) {
  dims <- length(p1)
  if (dims < 1L || length(p2) != dims || anyNA(c(p1, p2)) || any(c(p1, p2) < 0)) return(NA_real_)
  s <- ((diag(p1, dims) - tcrossprod(p1)) + (diag(p2, dims) - tcrossprod(p2))) / 2
  .t1_cat_smd_s(p1, p2, s)
}

#' Standardized mean difference between the first two groups
#'
#' _desctab_collect.ado:499-617, unweighted: continuous on the analysis scale
#' (log for contln), pooled SD `sqrt((s1^2 + s2^2) / 2)`; binary on
#' proportions; categorical by Yang and Dalton over the first K - 1 levels.
#' Stata keeps the value in a macro (`local smd = ...`), so it is rounded to
#' Stata's macro precision.
#'
#' @return Signed SMD or `NA`.
#' @keywords internal
#' @noRd
.t1_smd <- function(type, v, g, level1, level2) {
  g1 <- !is.na(g) & g == level1
  g2 <- !is.na(g) & g == level2
  out <- NA_real_
  if (type %in% c("contn", "contln", "conts")) {
    y <- v
    ok <- !is.na(y)
    if (type == "contln") {
      ok <- ok & y > 0
      y <- log(ifelse(ok, y, NA))
    }
    a <- y[g1 & ok]
    b <- y[g2 & ok]
    s1 <- if (length(a) > 1) stats::sd(a) else NA_real_
    s2 <- if (length(b) > 1) stats::sd(b) else NA_real_
    pool <- sqrt((s1^2 + s2^2) / 2)
    # A mean or SD beyond Stata's range leaves the SMD missing, as in Stata
    # (H10), rather than a spurious 0 from dividing by Inf.
    if (!is.na(pool) && .st_ok(pool) && pool > 0 && !is.na(.st_sum(a)) && !is.na(.st_sum(b))) {
      out <- (mean(a) - mean(b)) / pool
    }
  } else if (type %in% c("bin", "bine")) {
    a <- v[g1 & !is.na(v)]
    b <- v[g2 & !is.na(v)]
    p1 <- if (length(a)) mean(a) else NA_real_
    p2 <- if (length(b)) mean(b) else NA_real_
    den <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
    if (!is.na(den) && den > 0) out <- (p1 - p2) / den
  } else {
    # _desctab_collect.ado:559-564 (2.1.10+): levels observed in the two
    # compared groups only (a third group's level would blank the SMD).
    lv <- sort(unique(v[(g1 | g2) & !is.na(v)]), method = "radix")
    tot1 <- sum(g1 & !is.na(v))
    tot2 <- sum(g2 & !is.na(v))
    if (length(lv) >= 2L && tot1 > 0 && tot2 > 0) {
      k <- lv[-length(lv)]
      p1 <- vapply(k, function(l) sum(g1 & !is.na(v) & v == l), 0) / tot1
      p2 <- vapply(k, function(l) sum(g2 & !is.na(v) & v == l), 0) / tot2
      out <- .t1_cat_smd(p1, p2)
    }
  }
  if (is.na(out) || !.st_ok(out)) NA_real_ else stata_macro_num(out)
}

#' Multi-group balance statistic for `smdtype = "population"` or `"maxpair"`
#'
#' `"population"`: McCaffrey et al. (2013, Stat Med 32:3388) section 4.1.2
#' eq. (5), `max_g |mean_g - mean_pop| / sd_pop`. The group means carry the
#' weights. The pooled sample (all G groups) is the reference, with mean and
#' SD unweighted under `wt` (eq. 5's "unweighted mean and standard deviation
#' of the covariate for the pooled sample") and frequency-weighted under
#' `fweight`, since frequency weights replicate records. Continuous SD
#' uses `n - 1`; binary uses `sqrt(p_pop (1 - p_pop))`; categorical takes the
#' largest of the per-level values with `sqrt(p_pop,l (1 - p_pop,l))`, as
#' twang 2.6.2 does per factor level.
#'
#' `"maxpair"`: Lopez and Gutman (2017, Stat Sci 32:432) eq. (27), the
#' largest absolute pairwise difference. Every pair shares one denominator,
#' the root of the mean of the G group variances (cobalt 4.6.3
#' `s.d.denom = "pooled"`). Binary uses `p_g (1 - p_g)`; categorical is the
#' Yang-Dalton Mahalanobis distance with `S` averaged over all G groups.
#' Group means and variances are weighted as in `.t1w_smd()`, so with
#' G = 2 this is `abs()` of the `"pair"` SMD.
#'
#' A group with no usable values for the variable (fewer than two for a
#' continuous `"maxpair"` variance) leaves the statistic `NA`.
#' @return Non-negative statistic or `NA`.
#' @keywords internal
#' @noRd
.t1_smd_multi <- function(type, v, gid, G, smdtype, w = NULL, kind = "none") {
  if (is.null(w) || !kind %in% c("wt", "fw")) {
    w <- rep(1, length(v))
    kind <- "none"
  }
  # Mean and variance as .t1w_smd() forms them (unit weights: mean(), var()).
  wmv <- function(y, ww) {
    n <- length(y)
    sw <- sum(ww)
    if (!n || !is.finite(sw) || sw <= 0) return(c(NA_real_, NA_real_))
    m <- sum(ww * y) / sw
    ss <- sum(ww * (y - m)^2)
    vv <- if (kind == "fw") {
      if (sw > 1) ss / (sw - 1) else NA_real_
    } else if (n > 1) n / (sw * (n - 1)) * ss else NA_real_
    c(m, vv)
  }
  # The population reference ignores analytic weights (McCaffrey eq. 5).
  wpop <- if (kind == "fw") w else rep(1, length(v))
  ok <- !is.na(gid) & gid >= 1L & gid <= G & !is.na(v)
  y <- v
  if (type == "contln") {
    ok <- ok & v > 0
    y <- log(ifelse(ok, v, NA))
  }
  if (!all(vapply(seq_len(G), function(g) any(ok & gid == g), NA))) return(NA_real_)
  out <- NA_real_
  if (type %in% c("contn", "contln", "conts", "bin", "bine")) {
    bin <- type %in% c("bin", "bine")
    st <- vapply(seq_len(G), function(g) {
      k <- ok & gid == g
      wmv(y[k], w[k])
    }, numeric(2))
    m <- st[1, ]
    if (smdtype == "population") {
      pop <- wmv(y[ok], wpop[ok])
      den <- if (bin) sqrt(pop[1] * (1 - pop[1])) else sqrt(pop[2])
      if (!anyNA(m) && !is.na(den) && is.finite(den) && den > 0) out <- max(abs(m - pop[1])) / den
    } else {
      vg <- if (bin) m * (1 - m) else st[2, ]
      den <- sqrt(mean(vg))
      if (!anyNA(m) && !is.na(den) && is.finite(den) && den > 0) out <- (max(m) - min(m)) / den
    }
  } else {
    lv <- sort(unique(y[ok]), method = "radix")
    if (length(lv) < 2L) return(NA_real_)
    share <- function(k, levels) {
      tot <- sum(w[k])
      vapply(levels, function(l) sum(w[k & y == l]), 0) / tot
    }
    pg <- vapply(seq_len(G), function(g) share(ok & gid == g, lv), numeric(length(lv)))
    if (smdtype == "population") {
      tot <- sum(wpop[ok])
      pp <- vapply(lv, function(l) sum(wpop[ok & y == l]), 0) / tot
      den <- sqrt(pp * (1 - pp))
      if (!anyNA(pg) && all(den > 0)) out <- max(abs(pg - pp) / den)
    } else {
      keep <- seq_len(length(lv) - 1L)
      pk <- pg[keep, , drop = FALSE]
      if (anyNA(pk)) return(NA_real_)
      s <- Reduce(`+`, lapply(seq_len(G), function(g) diag(pk[, g], length(keep)) - tcrossprod(pk[, g]))) / G
      vals <- numeric()
      for (a in seq_len(G - 1L)) for (b in (a + 1L):G) vals <- c(vals, .t1_cat_smd_s(pk[, a], pk[, b], s))
      if (length(vals) && !anyNA(vals)) out <- max(vals)
    }
  }
  if (is.na(out) || !.st_ok(out)) NA_real_ else stata_macro_num(out)
}

# Mahalanobis distance sqrt(d' S^-1 d) for a given S (.t1_cat_smd() with a
# covariance pooled over more than the two compared groups).
.t1_cat_smd_s <- function(p1, p2, s) {
  dims <- length(p1)
  if (qr(s)$rank < dims) return(NA_real_)
  d <- p1 - p2
  d2 <- tryCatch(drop(crossprod(d, solve(s, d))), error = function(e) NA_real_)
  if (is.na(d2)) return(NA_real_)
  if (d2 < 0 && d2 > -1e-12) d2 <- 0
  if (d2 < 0) return(NA_real_)
  sqrt(d2)
}
