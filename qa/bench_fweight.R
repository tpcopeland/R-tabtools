# qa/bench_fweight.R - time and memory guard for table1_tc hypothesis tests
# under fweight (Phase 3 review P2-3). Not CRAN-safe (wall-clock bounds), so
# it lives in qa/.
#   Rscript qa/bench_fweight.R [package dir, default "."] [largest total, default 5e7]
# Before the fix every test expanded the data by the frequencies (about 66
# bytes per unit of total weight, per variable: 1e7 took 10.7 s / 788 MB for
# a Welch t and 18.4 s / 1.35 GB for a Wilcoxon; 5e7 took 3-3.4 GB). The
# aggregated forms must stay flat in the total frequency. Memory is R's
# peak heap during the call (gc() "max used" after a reset), which includes
# the session's own ~55-60 MB.
#
# The measurement is of the call, not of R's byte-compiler (Milestone H,
# H16; finding F21). Under pkgload::load_all() the namespace is not
# byte-compiled, and the JIT compiles closures on their second use, which
# fell in the first measured call of each variable type: 115 MB against the
# 100 MB bound, while the same case repeated later took 71 MB. The JIT is
# therefore switched off around the measured calls (compiler::enableJIT(0),
# restored afterwards), and qa/run_all.R runs this script against a
# byte-compiled installed copy (TABTOOLS_QA_LIB). The bound stays 100 MB
# (decision H-D9).
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
qa_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else normalizePath("qa")
source(file.path(qa_dir, "tools", "qa_result.R"))  # qa_check(), qa_done() (Codex audit CX-6)
args <- commandArgs(TRUE)
pkg <- if (length(args)) args[1] else dirname(qa_dir)
max_total <- if (length(args) > 1L) as.numeric(args[2]) else 5e7
qa_lib <- Sys.getenv("TABTOOLS_QA_LIB")
if (nzchar(qa_lib)) {
  suppressMessages(library(tabtools, lib.loc = qa_lib))
  cat("tabtools", format(utils::packageVersion("tabtools", lib.loc = qa_lib)), "installed in", qa_lib, "\n")
} else {
  suppressMessages(pkgload::load_all(pkg, quiet = TRUE))
}

bound_s <- 5
bound_mb <- 100
n <- 2000
cases <- expand.grid(total = c(1e5, 1e6, 1e7, 5e7), type = c("contn", "cat", "conts"), G = 2L,
                     stringsAsFactors = FALSE)
cases <- rbind(cases, data.frame(total = c(1e7, 5e7), type = c("conts", "contn"), G = 3L))
cases <- cases[cases$total <= max_total, ]
make_data <- function(total, G) {
  set.seed(1)
  d <- data.frame(g = rep_len(seq_len(G), n), x = round(rnorm(n, 50, 10), 1), c = sample(1:4, n, TRUE))
  d$f <- pmax(1, round(total / n * runif(n, 0.5, 1.5)))
  d
}
# Warm up (lazy loading and byte compilation on the first call).
invisible(table1_tc(make_data(1e3, 2L), by = "g", vars = "x contn \\ x conts \\ c cat", fweight = "f"))
jit <- compiler::enableJIT(0)
for (i in seq_len(nrow(cases))) {
  cs <- cases[i, ]
  d <- make_data(cs$total, cs$G)
  vars <- switch(cs$type, contn = "x contn", conts = "x conts", cat = "c cat")
  invisible(gc(reset = TRUE))
  el <- system.time(tt <- tryCatch(table1_tc(d, by = "g", vars = vars, fweight = "f"),
                                   error = function(e) e))[["elapsed"]]
  mb <- sum(gc()[, 6L])
  failed <- inherits(tt, "error")
  p <- if (failed) conditionMessage(tt) else format(tt$stored$table[1, "p_value"], digits = 6)
  qa_check(sprintf("sum(fw)=%-8.3g %-5s G=%d: %6.2f s, peak %7.1f MB (bounds %g s, %g MB)  p=%s",
                   sum(d$f), cs$type, cs$G, el, mb, bound_s, bound_mb, p),
           !failed && el <= bound_s && mb <= bound_mb)
}
invisible(compiler::enableJIT(jit))
qa_done("bench_fweight.R")
