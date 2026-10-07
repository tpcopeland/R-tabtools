# qa/run_all.R - the developer QA lane in one command (Milestone H, H16).
#   Rscript qa/run_all.R [full|adversarial|sample|interaction|crossval|quick|core|benchmark]
#
# 1. Builds the package (R CMD build --no-build-vignettes) and installs the
#    tarball, byte-compiled, into a temporary library. Nothing is installed
#    into the user's library, and the source tree is not compiled in place.
# 2. Runs the selected lane against that installed copy (TABTOOLS_QA_LIB
#    points the scripts at it), each in a supervised R process with a
#    per-file timeout. New adversarial files run with testthat::test_file().
# 3. Reconciles qa/: every top-level qa/*.R is either run here or listed in
#    qa/_skip.txt with a reason; an unlisted script is a failure.
# 4. Reconciles each script's result: every script prints one completion
#    marker (qa/tools/qa_result.R) with the comparisons it executed,
#    skipped and failed. The runner recomputes the status from those counts:
#    PASS (all ran, none failed), INCOMPLETE (some skipped), SKIP (none
#    ran), FAIL (a failure, a non-zero exit, or no marker: the script
#    stopped early). The exit status alone is not trusted (Codex audit
#    CX-6: a script whose comparator packages were all missing exited 0
#    and was reported PASS).
# 5. Prints every result and skip. Exit status: 0 when every script
#    PASSes; 1 on any FAIL; 2 when nothing failed but a script was
#    INCOMPLETE or SKIP (`--allow-incomplete` makes that 0, still reported).
#
# Each execution prints a RESULT line and the final RESULT: TOTAL line.
# The testthat suites (tests/testthat/, `NOT_CRAN=true devtools::test()`)
# and R CMD check are separate gates; see qa/README.md.

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[startsWith(args, "--file=")])
qa_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else normalizePath("qa")
root <- dirname(qa_dir)
if (!file.exists(file.path(root, "DESCRIPTION"))) stop("cannot find the package root above ", qa_dir)
allow_incomplete <- "--allow-incomplete" %in% commandArgs(TRUE)
lane_args <- setdiff(commandArgs(TRUE), "--allow-incomplete")
if (length(lane_args) > 1L) stop("expected at most one QA lane")
lane <- if (length(lane_args)) lane_args[[1L]] else "full"
source(file.path(qa_dir, "tools", "qa_result.R"))  # qa_reconcile()

# Scripts this runner executes, in order, with what each checks.
run <- c(
  check_examples.R = "installed public API: every export documented, its examples run",
  bench_fisher.R = "Fisher exact test wall-clock bounds (workspace escalation, simulated fallback)",
  bench_fweight.R = "fweight tests: time (5 s) and memory (100 MB) bounds up to a total frequency of 5e7",
  validation_wttab.R = "wttab() on real ipw::ipwpoint()/ipwtm() objects and against WeightIt's own summaries",
  crossval_regtab_formats.R = "raw regtab/effecttab full numeric formats against pinned native Stata 2.5.1 string()",
  crossval_puttab_layout.R = "puttab/stacktab frames cells, borders, panels, formats and sinks against native Stata 2.5.1",
  crossval_puttab_flat.R = "puttab keyed flat round trips, model spans and explicit diagnostic differences against native Stata 2.5.1",
  crossval_session_sinks.R = "session destinations, successful-path Markdown lifecycle and explicit-sheet gating against pinned native Stata 2.5.1",
  crossval_smd_balance.R = "table1_tc() smdtype population/maxpair/pair against cobalt and twang",
  demo_parity.R = "the R demo (qa/demo/demo_tabtools.R) against the Stata demo, sheet by sheet (Milestone D)",
  test_adversarial_descriptive.R = "missing values, sparse groups, weight filtering and observed denominators",
  test_adversarial_regression.R = "complete-case models, unavailable terms and unreliable model inference",
  test_adversarial_rates_effects.R = "incomplete exposure/event data, Cox samples and unavailable effect inference",
  test_adversarial_mi_identity.R = "explicit MI subject IDs, strict sample checks and permutation invariance",
  test_adversarial_mixed_models.R = "failed mixed-model optimization, invalid Hessians and valid boundaries",
  test_adversarial_likelihood_models.R = "failed count, multinomial and ordinal fits and valid sparse controls",
  test_adversarial_gee_survival.R = "failed GEE/survival fits and limitations of stored diagnostics",
  test_adversarial_transform_rows.R = "overlapping and nested matrix-valued covariates retain distinct coefficient rows",
  test_sample_accounting_descriptive.R = "scoped observed/used records, missing categories, weight totals and exclusions",
  test_sample_accounting_models.R = "stored fitted samples, zero weights, native N, UV and per-imputation records",
  test_sample_accounting_rates_weights.R = "retained missing events, weighted exposure and provided weight populations",
  test_sample_accounting_composition.R = "source-local ledgers through effects, dataframe conversion and composition",
  test_sample_accounting_integration.R = "independent schema, cross-family count distinctions and presentation transport",
  test_interaction_matrix_descriptive.R = "seeded combined missingness, sparse categories and extreme weights",
  test_interaction_matrix_likelihood_mixed.R = "seeded native likelihood and mixed-model coefficient, covariance and sample comparisons",
  test_interaction_matrix_survival_survey_gee.R = "seeded survival, censored, survey and GEE sample/design interactions",
  test_interaction_matrix_composition.R = "seeded weighted estimation, MI, UV, effects and composed population transport",
  test_interaction_matrix_independent.R = "adapter coverage reconciliation and independent interaction probes"
)
adversarial_files <- c("test_adversarial_descriptive.R", "test_adversarial_regression.R",
                   "test_adversarial_rates_effects.R", "test_adversarial_mi_identity.R",
                   "test_adversarial_mixed_models.R", "test_adversarial_likelihood_models.R",
                   "test_adversarial_gee_survival.R", "test_adversarial_transform_rows.R")
sample_files <- c("test_sample_accounting_descriptive.R", "test_sample_accounting_models.R",
                  "test_sample_accounting_rates_weights.R", "test_sample_accounting_composition.R",
                  "test_sample_accounting_integration.R")
interaction_files <- c("test_interaction_matrix_descriptive.R",
                       "test_interaction_matrix_likelihood_mixed.R",
                       "test_interaction_matrix_survival_survey_gee.R",
                       "test_interaction_matrix_composition.R",
                       "test_interaction_matrix_independent.R")
crossval_files <- c("crossval_smd_balance.R", "crossval_puttab_layout.R", "crossval_regtab_formats.R", "crossval_puttab_flat.R", "crossval_session_sinks.R")
testthat_files <- c(adversarial_files, sample_files, interaction_files, crossval_files)
LANES <- list(
  quick = c(adversarial_files, sample_files),
  core = c(testthat_files, "check_examples.R", "validation_wttab.R"),
  full = names(run),
  adversarial = adversarial_files,
  sample = sample_files,
  interaction = interaction_files,
  crossval = crossval_files,
  benchmark = c("bench_fisher.R", "bench_fweight.R")
)
if (!lane %in% names(LANES)) stop("unknown QA lane: ", lane)
lane_files <- LANES[[lane]]
for (dep in c("callr", "testthat", "R6")) {
  if (!requireNamespace(dep, quietly = TRUE)) stop("install the QA runner dependency: ", dep)
}
timeout_seconds <- 600L

rule <- function(ch = "-") cat(strrep(ch, 72), "\n", sep = "")
say <- function(...) cat(sprintf(...), "\n", sep = "")

# --- 3. reconcile qa/ with the run list and the skip list ------------------
skip_file <- file.path(qa_dir, "_skip.txt")
skip_lines <- if (file.exists(skip_file)) readLines(skip_file, warn = FALSE) else character()
skip_lines <- trimws(skip_lines[!grepl("^\\s*(#|$)", skip_lines)])
skips <- stats::setNames(trimws(sub("^\\S+\\s*", "", skip_lines)), sub("\\s.*$", "", skip_lines))
scripts <- setdiff(list.files(qa_dir, pattern = "\\.R$"), "run_all.R")
unlisted <- setdiff(scripts, c(names(run), names(skips)))
missing_run <- setdiff(names(run), scripts)
stale_skip <- setdiff(names(skips), scripts)
failures <- character()
if (length(unlisted)) failures <- c(failures, paste0(unlisted, ": not run and not in qa/_skip.txt"))
if (length(missing_run)) failures <- c(failures, paste0(missing_run, ": in the run list but missing"))
# A skip entry for a script not (yet) in this tree is only reported: the
# list may name a script another branch adds.

# --- 1. build and install into a temporary library -------------------------
rule("=")
say("tabtools QA lane %s  (%s, %s)", lane, root, format(Sys.time(), "%Y-%m-%d %H:%M"))
rule("=")
work <- tempfile("tabtools-qa-")
lib <- file.path(work, "lib")
dir.create(lib, recursive = TRUE)
R <- file.path(R.home("bin"), "R")
build_log <- file.path(work, "build.log")
old <- setwd(work)  # R CMD build writes the tarball into the working directory
st <- system2(R, c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(root)),
              stdout = build_log, stderr = build_log)
setwd(old)
tarball <- list.files(work, pattern = "^tabtools_.*\\.tar\\.gz$", full.names = TRUE)
if (st != 0L || length(tarball) != 1L) {
  cat(readLines(build_log), sep = "\n")
  stop("R CMD build failed")
}
install_log <- file.path(work, "install.log")
st <- system2(R, c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", shQuote(lib)), shQuote(tarball)),
              stdout = install_log, stderr = install_log)
if (st != 0L) {
  cat(readLines(install_log), sep = "\n")
  stop("R CMD INSTALL into the temporary library failed")
}
say("installed %s (byte-compiled) into %s", basename(tarball), lib)
resolved <- callr::r(function() find.package("tabtools"), libpath = c(lib, .libPaths()))
if (!identical(normalizePath(resolved), normalizePath(file.path(lib, "tabtools")))) {
  stop("tabtools did not resolve to its temporary installation: ", resolved)
}

# --- 2. run each script against the installed copy -------------------------
run_testthat <- function(path, qa_tools) {
  source(qa_tools)
  qa_run_testthat(path)
  qa_done(basename(path))
}
run_script <- function(path, use_testthat, log) {
  if (use_testthat) {
    p <- callr::r_bg(run_testthat,
      args = list(path = path, qa_tools = file.path(qa_dir, "tools", "qa_result.R")),
      libpath = c(lib, .libPaths()),
      env = c(callr::rcmd_safe_env(), NOT_CRAN = "true", TABTOOLS_QA_LIB = lib),
      stdout = log, stderr = "2>&1", supervise = TRUE)
  } else {
    p <- callr::rscript_process$new(callr::rscript_process_options(
      script = path, wd = root, libpath = c(lib, .libPaths()),
      env = c(callr::rcmd_safe_env(), NOT_CRAN = "true", TABTOOLS_QA_LIB = lib),
      stdout = log, stderr = "2>&1", extra = list(supervise = TRUE)))
  }
  on.exit(if (p$is_alive()) p$kill_tree(), add = TRUE)
  if (.Platform$OS.type == "unix") {
    watcher <- paste(
      'while kill -0 "$1" 2>/dev/null && kill -0 "$2" 2>/dev/null; do sleep 1; done',
      'kill -0 "$1" 2>/dev/null || kill -s KILL -- "-$2" 2>/dev/null',
      'exit 0', sep = "\n")
    processx::process$new("sh", c("-c", watcher, "qa-watch", Sys.getpid(), p$get_pid()),
                          cleanup = FALSE)
  }
  p$wait(timeout = timeout_seconds * 1000)
  if (p$is_alive()) {
    p$kill_tree()
    return(list(status = 1L, note = paste0("timeout-after-", timeout_seconds, "s")))
  }
  list(status = as.integer(p$get_exit_status()), note = "")
}
results <- data.frame(script = character(), status = character(), executed = integer(), skipped = integer(),
                      failed = integer(), seconds = numeric(), note = character(), stringsAsFactors = FALSE)
interaction_logs <- list()
for (s in lane_files) {
  if (!s %in% scripts) next
  rule()
  say("RUN   %s  -- %s", s, run[[s]])
  log <- file.path(work, paste0(s, ".log"))
  t0 <- proc.time()[["elapsed"]]
  proc <- tryCatch(run_script(file.path(qa_dir, s), s %in% testthat_files, log),
    error = function(e) list(status = 1L, note = paste0("process-error: ", conditionMessage(e))))
  el <- proc.time()[["elapsed"]] - t0
  out <- if (file.exists(log)) readLines(log, warn = FALSE) else character()
  if (s %in% interaction_files) interaction_logs[[s]] <- out
  cat(paste0("  | ", out), sep = "\n")
  v <- qa_reconcile(s, proc$status, out)
  if (nzchar(proc$note)) v$note <- proc$note
  results <- rbind(results, data.frame(script = s, status = v$status, executed = v$executed, skipped = v$skipped,
                                       failed = v$failed, seconds = round(el, 1), note = v$note,
                                       stringsAsFactors = FALSE))
  if (identical(v$status, "FAIL")) failures <- c(failures, paste0(s, ": ", if (nzchar(v$note)) v$note else "failed checks"))
  say("RESULT: %s executed=%s skipped=%s failed=%s status=%s%s", s, v$executed, v$skipped,
      v$failed, v$status, if (nzchar(v$note)) paste0(" reason=", v$note) else "")
}

if (all(interaction_files %in% lane_files)) {
  manifest_file <- file.path(qa_dir, "data", "interaction_matrix.csv")
  manifest <- tryCatch(utils::read.csv(manifest_file, stringsAsFactors = FALSE),
                       error = function(e) NULL)
  failures <- c(failures, qa_matrix_coverage(manifest, interaction_logs,
    stats::setNames(results$status, results$script), interaction_files))
}

# --- 4. report --------------------------------------------------------------
rule("=")
cnt <- function(x) if (is.na(x)) "-" else as.character(x)
say("%-28s %-10s %4s %4s %4s %8s  %s", "script", "result", "ran", "skip", "fail", "seconds", "")
for (i in seq_len(nrow(results))) {
  say("%-28s %-10s %4s %4s %4s %8.1f  %s", results$script[i], results$status[i], cnt(results$executed[i]),
      cnt(results$skipped[i]), cnt(results$failed[i]), results$seconds[i], results$note[i])
}
for (s in intersect(names(skips), scripts)) say("%-28s %-10s %4s %4s %4s %8s  %s", s, "NOT RUN", "", "", "", "", skips[[s]])
for (s in stale_skip) say("%-28s %-10s %4s %4s %4s %8s  %s", s, "note", "", "", "", "", "listed in qa/_skip.txt, not in this tree")
rule("=")
snaps <- file.path(qa_dir, "_snaps")
if (dir.exists(snaps) && !length(list.files(snaps, recursive = TRUE, all.files = TRUE, no.. = TRUE))) {
  unlink(snaps, recursive = TRUE)
}
unlink(work, recursive = TRUE)
if (length(failures)) {
  say("FAILED:")
  cat(paste0("  - ", failures), sep = "\n")
  say("RESULT: TOTAL pkg=tabtools lane=%s files=%d/%d status=FAIL", lane, nrow(results), length(lane_files))
  quit(save = "no", status = 1L)
}
partial <- results$script[results$status %in% c("INCOMPLETE", "SKIP")]
n_listed <- length(intersect(names(skips), scripts))
if (length(partial)) {
  say("QA lane INCOMPLETE: %s did not run every comparison (%d scripts run, %d not run by design).",
      paste(partial, collapse = ", "), nrow(results), n_listed)
  say("RESULT: TOTAL pkg=tabtools lane=%s files=%d/%d status=INCOMPLETE", lane, nrow(results), length(lane_files))
  quit(save = "no", status = if (allow_incomplete) 0L else 2L)
}
say("All QA scripts passed: %d run, every comparison executed (%d not run by design, listed in qa/_skip.txt).",
    nrow(results), n_listed)
say("RESULT: TOTAL pkg=tabtools lane=%s files=%d/%d status=PASS", lane, nrow(results), length(lane_files))
