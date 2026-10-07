# Golden runner for effecttab (E scenarios, IMPLEMENTATION_PLAN.md tasks
# 7.7-7.11). Every Stata setup writes <id>_input.csv (golden_effect_input
# in qa/stata/golden_helpers.do): Stata's r(table) rows of each collected
# model. The "inject" scenarios' r_call reads them with
# golden_effect_input(), so the R layout formats Stata's own numbers and
# every comparison is exact by construction; the end-to-end scenarios fit
# the models in R (marginaleffects, WeightIt) and are compared in the mode
# scenarios.csv gives them.

# Stata's r(table) rows of scenario `id` as effecttab() data frames, one per
# collected model: equation, term, estimate, conf.low, conf.high, p.value,
# with the level (attr "conf.level") and the teffects estimator (attr
# "tt_estimator") the command recorded.
golden_effect_input <- function(id) {
  d <- utils::read.csv(golden_path(paste0(id, "_input.csv")), colClasses = "character",
                       na.strings = character(), encoding = "UTF-8")
  num <- golden_stored_num  # exact parse on every platform (helper-exact-num.R)
  out <- lapply(split(d, as.integer(d$model)), function(m) {
    df <- data.frame(equation = m$equation, term = m$term, estimate = num(m$estimate),
                     conf.low = num(m$conf_low), conf.high = num(m$conf_high), p.value = num(m$p_value),
                     stringsAsFactors = FALSE)
    attr(df, "conf.level") <- num(m$level[1]) / 100
    if (nzchar(m$subcmd[1])) attr(df, "tt_estimator") <- m$subcmd[1]
    df
  })
  unname(out)
}

# Stata's closing lines after the listing ("Markdown exported to E01.md",
# "Exported 5 rows x 5 cols to effecttab.xlsx, sheet E01"). R prints none,
# as regtab does not (?effecttab, "Differences from Stata"): exactly those
# two lines are dropped from the Stata side.
golden_drop_effecttab_export <- function(want, id) {
  md <- which(want == sprintf("Markdown exported to %s.md", id))
  xl <- grep(sprintf("^Exported [0-9]+ rows .* cols to effecttab\\.xlsx, sheet %s$", id), want)
  testthat::expect_length(md, 1L)
  testthat::expect_length(xl, 1L)
  drop <- c(md, xl)
  if (length(drop)) want[-drop] else want
}

# R inputs that are marginaleffects margins results: their methods sentence
# names the marginaleffects package where Stata names the margins command
# (?effecttab, "Differences from Stata"). The only relaxation of the methods
# comparison, applied to these scenarios alone.
golden_effect_me_margins <- c("E03", "E04", "E14", "E15", "E16", "E22", "E23", "W12", "W13")

# R names what a single teffects table holds (review F19; ?effecttab,
# "Differences from Stata"), where Stata says "Average treatment effects"
# for potential-outcome means and ATET tables too: the golden's noun is
# replaced for exactly these scenarios, whatever the input.
golden_effect_methods_noun <- c(
  E07 = "Potential-outcome means",
  E08 = "Average treatment effects on the treated",
  E19 = "Potential-outcome means"
)

golden_effect_methods <- function(golden, id, injected = FALSE) {
  m <- golden$name == "methods" & golden$kind == "macro"
  if (id %in% names(golden_effect_methods_noun)) {
    stopifnot(sum(startsWith(golden$value[m], "Average treatment effects estimated using ")) == 1L)
    golden$value[m] <- sub("^Average treatment effects", golden_effect_methods_noun[[id]], golden$value[m])
  }
  if (injected || !id %in% golden_effect_me_margins) return(golden)
  stopifnot(sum(grepl("using the margins command", golden$value[m], fixed = TRUE)) == 1L)
  golden$value[m] <- sub("using the margins command", "using the marginaleffects package", golden$value[m], fixed = TRUE)
  golden
}

# r(table) tolerance per scenario (relative above 1, absolute below, as
# golden_compare_stored() measures), the harness default 1e-9 otherwise:
# inject scenarios hold Stata's own numbers; the end-to-end fits (E03,
# E12-E14, E17, E22, E23) agree to 1e-10 or better (marginaleffects 0.32).
# Each override is the smallest 1-2-5 step at or above the largest
# difference seen on 2026-09-26.
golden_effect_table_tol <- c(
  E01 = 5e-7,  # WeightIt M-estimation vs teffects ipw's GMM: p 2.2e-7
  E04 = 5e-4,  # tolerance mode: 0/1 variables are a discrete change in R, a derivative in Stata (p 4.3e-4)
  E15 = 2e-8,  # delta-method p-values, numeric Jacobian: 1.6e-8
  E16 = 1e-7,  # 8.6e-8
  # IPTW logit [pw]: Stata's logit stops ~3e-9 from the MLE glm() reaches;
  # the delta-method p-value moves with it (review F7).
  W12 = 5e-9,  # 3.0e-9
  W13 = 2e-7   # 9.6e-8
)

# Console lines with every number replaced by "#" (tolerance and structure
# scenarios: the numbers are compared in the cells, within their mode).
golden_console_skeleton <- function(lines) gsub("[-+]?[0-9][0-9,]*(\\.[0-9]+)?", "#", lines, perl = TRUE)

# The value mismatches a tolerance or structure scenario's workbook may
# show: body values (compared by the cell comparator, in the scenario's
# mode), and in structure mode the label column's text and width (R's
# labels differ from Stata's raw keys by design, EO6).
golden_effect_style_filter <- function(why, mode) {
  if (mode == "exact" || !length(why)) return(why)
  row_of <- function(w) suppressWarnings(as.integer(sub("^[A-Z]+([0-9]+) .*$", "\\1", w)))
  body_value <- grepl("^[A-Z]+[0-9]+ value:", why) & row_of(why) >= 4L
  label_col <- mode == "structure" & (grepl("^B[0-9]+ value:", why) | grepl("^col B width:", why))
  why[!(body_value | label_col)]
}

# `r_call` replaces the scenario's R call (the injected-input check of the
# end-to-end scenarios: Stata's numbers through effecttab(), compared in
# `mode`).
run_golden_effecttab_scenario <- function(id, r_call = NULL, mode = NULL) {
  sc <- golden_scenario(id)
  if (!golden_scenario_live(id, sc$phase)) testthat::skip(paste("Phase", sc$phase))
  injected <- !is.null(r_call)
  mode <- mode %||% sc$compare
  out <- withr::local_tempdir()
  sinks <- golden_code_path(out, id)
  code <- gsub("@ID@", sinks, r_call %||% sc$r_call, fixed = TRUE)
  env <- new.env(parent = environment(golden_fixture))
  printed <- utils::capture.output(tt <- eval(parse(text = code), envir = env))
  # Every sink was written, so the call returns invisibly and prints nothing.
  expect_identical(printed, character())
  expect_s3_class(tt, "tt_table")
  expect_identical(tt$command, "effecttab")
  parts <- golden_publication_cells(tt, id)
  want_cells <- parts$want
  if (mode == "structure") {
    # The label column differs by design (EO6); everything else by skeleton.
    got <- parts$got
    expect_identical(dim(got), dim(want_cells))
    if (identical(dim(got), dim(want_cells))) {
      mm <- golden_compare_cells(got[, -1, drop = FALSE], want_cells[, -1, drop = FALSE], mode = "structure")
      if (nrow(mm)) golden_fail(mm, paste("cells", id)) else testthat::succeed()
      # Headers, title and footnote in the label column are still Stata's.
      nh <- nrow(want_cells) - nrow(tt$body)
      expect_identical(got[seq_len(nh), 1], want_cells[seq_len(nh), 1])
    }
  } else {
    expect_cells_match(tt, id)
  }

  # Console: R's listing is the printed table.
  want <- golden_drop_effecttab_export(golden_read_lines(golden_path(paste0(id, "_console.txt"))), id)
  console_parts <- golden_publication_console(utils::capture.output(print(tt)), want, id)
  got <- console_parts$got
  want <- console_parts$want
  if (mode != "exact") {
    got <- golden_console_skeleton(got)
    want <- golden_console_skeleton(want)
    if (mode == "structure") {
      # Label column aside: compare the boxed lines right of it.
      cut <- function(l) ifelse(grepl("^  \\| ", l), sub("^  \\| *[^ ].*?   ", "", l, perl = TRUE), l)
      got <- cut(got)
      want <- cut(want)
    }
  }
  why <- if (mode == "structure") {
    if (length(got) != length(want)) sprintf("%d vs %d lines", length(got), length(want)) else character()
  } else golden_compare_console(got, want)
  golden_expect_none(why, paste("console", id))

  # Stored results, file paths aside (R's are temporary).
  stored <- golden_effect_methods(golden_read_stored(id), id, injected)
  fields <- setdiff(unique(stored$name[stored$kind != "meta"]), c("xlsx", "markdown", "table"))
  why <- golden_compare_stored(tt$stored, stored, fields = fields)
  golden_expect_none(why, paste("stored", id))
  tol <- if (id %in% names(golden_effect_table_tol)) golden_effect_table_tol[[id]] else 1e-9
  if (mode == "structure") {
    # r(table) row names come from the labels: compare the values by position.
    w <- stored[stored$name == "table", , drop = FALSE]
    g <- golden_flatten_stored(tt$stored["table"])
    expect_identical(nrow(g), nrow(w))
    if (nrow(g) == nrow(w)) {
      # golden_flatten_stored() lists by column, the golden by row.
      gm <- matrix(golden_stored_num(g$value), ncol = length(unique(g$col)))
      wm <- matrix(golden_stored_num(w$value), ncol = length(unique(w$col)), byrow = TRUE)
      expect_equal(unname(gm), unname(wm), tolerance = tol)
    }
  } else if (id %in% golden_effect_matrix_ids && !injected) {
    golden_effect_expect_matrix_table(tt, stored, id)
  } else {
    why <- golden_compare_stored(tt$stored, stored, fields = "table", tolerance = tol)
    golden_expect_none(why, paste("stored table", id))
  }

  # The workbook the call wrote, and the renderer alone from the table.
  want_book <- golden_path("effecttab.xlsx")
  why <- golden_compare_styles(paste0(sinks, ".xlsx"), id, want_book, id, got_width_offset = golden_r_width_offset, publication_id = id)
  golden_expect_none(golden_effect_style_filter(why, mode), paste("styles", id))
  again <- file.path(out, "again.xlsx")
  tabtools::tt_write_xlsx(tt, again, sheet = id)
  why <- golden_compare_styles(again, id, want_book, id, got_width_offset = golden_r_width_offset, publication_id = id)
  golden_expect_none(golden_effect_style_filter(why, mode), paste("re-rendered", id))
  if (mode == "exact") {
    expect_sink_match(paste0(sinks, ".csv"), id, "csv", tt = tt)
    expect_sink_match(paste0(sinks, ".md"), id, "md", tt = tt)
  }

  # The frame characteristics a composite reads (effecttab.ado:1436-1453).
  df <- as.data.frame(tt)
  sv <- function(n) stored$value[stored$name == n]
  expect_identical(attr(df, "source"), "effecttab")
  expect_identical(attr(df, "statistic_ids"), "estimate ci pvalue")
  expect_equal(attr(df, "ci_level"), as.numeric(sv("ci_level")))
  M <- (as.numeric(sv("N_cols")) - 2) / 3
  expect_identical(attr(df, "n_models"), as.integer(M))
  expect_identical(attr(df, "outcome_id"), rep("", M))
  expect_identical(attr(df, "effect_scale"), rep(sv("effect_label"), M))
  expect_identical(attr(df, "model_label"), unname(tt$header[[1]]$text[2 + 3 * (seq_len(M) - 1)]))
  invisible(tt)
}

# Matrix scenarios (Stata's from()): up to 2.1.13 at 96f090d8 Stata stored
# the estimates of r(table) as displayed (rounded to digits, through the
# text), R at full precision; since Stata-Tools 68c37a90 (task C7) Stata
# stores them at full precision too, so every r(table) cell compares
# exactly.
golden_effect_matrix_ids <- c("E18", "E24")

golden_effect_expect_matrix_table <- function(tt, stored, id) {
  w <- stored[stored$name == "table", , drop = FALSE]
  g <- golden_flatten_stored(tt$stored["table"])
  key <- function(d) paste(d$row, d$col)
  expect_setequal(key(g), key(w))
  hit <- match(key(w), key(g))
  gv <- golden_stored_num(g$value[hit])
  wv <- golden_stored_num(w$value)
  expect_equal(gv, wv, tolerance = 1e-12)
}
