# qa/bench_fisher.R - timing guard for table1_tc's Fisher exact test (Phase 2
# review P2-6). Not CRAN-safe (wall-clock bounds), so it lives in qa/.
#   Rscript qa/bench_fisher.R
# With a fixed 2e8 workspace the first case ran 37 s before failing over to
# the simulated p-value; the escalation (2e5, 2e6, 2e7, then Monte Carlo)
# must keep every case below the bound.
# qa/run_all.R sets TABTOOLS_QA_LIB to a temporary library holding an
# installed copy; run by hand, the source tree is loaded.
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
qa_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else normalizePath("qa")
source(file.path(qa_dir, "tools", "qa_result.R"))  # qa_check(), qa_done() (Codex audit CX-6)
qa_lib <- Sys.getenv("TABTOOLS_QA_LIB")
if (nzchar(qa_lib)) {
  suppressMessages(library(tabtools, lib.loc = qa_lib))
} else {
  suppressMessages(pkgload::load_all(dirname(qa_dir), quiet = TRUE))
}
cases <- list(
  list(n = 2000, L = 6, G = 3, bound = 15),
  list(n = 15000, L = 5, G = 2, bound = 20),
  list(n = 5000, L = 8, G = 4, bound = 15),
  list(n = 300, L = 5, G = 3, bound = 10)
)
set.seed(7)
for (cs in cases) {
  d <- data.frame(g = sample(seq_len(cs$G), cs$n, TRUE), c = sample(seq_len(cs$L), cs$n, TRUE))
  el <- system.time(tt <- table1_tc(d, vars = c(c = "cate"), by = "g", test = TRUE))[["elapsed"]]
  sim <- length(tt$stored$fisher_simulated) > 0L
  qa_check(sprintf("n=%5d %dx%d: %5.1f s (bound %d s)  %s  p=%s", cs$n, cs$L, cs$G, el, cs$bound,
                   if (sim) "simulated" else "exact", format(tt$stored$table[1, "p_value"], digits = 6)),
           el <= cs$bound)
}
qa_done("bench_fisher.R")
