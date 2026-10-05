# golden_tidy(id): the data-frame input (regtab's escape hatch, plan task
# 5.8) of a structure-only golden whose Stata model has no R equivalent.
# Built from Stata's own estimates (qa/stata/make_regtab_phase5a.do writes
# tests/testthat/fixtures/regtab_phase5a/<id>_tidy.csv and <id>_stats.csv).
#
# R25t: `churdle linear annual_cost dose_intensity,
# select(participation_score) ll(0)` on the hurdle fixture. Equation names
# stay Stata's (`selection_ll`, `lnsigma`, `_diparm1`) for regtab to
# translate, except the dependent variable's own equation, which Stata
# labels with the variable label (`regtab.ado:1558-1560`).
golden_tidy <- function(id) {
  dir <- test_path("fixtures", "regtab_phase5a")
  x <- utils::read.csv(file.path(dir, paste0(id, "_tidy.csv")), stringsAsFactors = FALSE,
                       na.strings = c(".", ".b"), strip.white = TRUE, encoding = "UTF-8",
                       check.names = FALSE)
  st <- utils::read.csv(file.path(dir, paste0(id, "_stats.csv")), stringsAsFactors = FALSE,
                        na.strings = ".", strip.white = TRUE)
  fx <- golden_scenario(id)$fixture
  d <- golden_fixture(fx)
  depvar <- switch(id, R25t = "annual_cost", stop("no golden_tidy() recipe for ", id, call. = FALSE))
  x$equation[x$equation == depvar] <- attr(d[[depvar]], "label", exact = TRUE)
  x$var_type <- ifelse(x$term == "_cons", "intercept", "continuous")
  x$var_label[!nzchar(x$var_label)] <- NA_character_
  attr(x, "stata_cmd") <- switch(id, R25t = "churdle")
  attr(x, "glance") <- list(nobs = st$N[1], logLik = st$ll[1], df = st$rank[1],
                            pseudo.r.squared = st$r2_p[1])
  x
}
