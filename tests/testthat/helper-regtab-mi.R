# Multiply imputed goldens (task 5.14; MI01-MI07 in golden/scenarios.csv):
# the flong fixture mi_cohort.dta (qa/make_mi_fixture.R) holds m = 0 (the
# incomplete data) and the imputations m = 1..5 in `mi_m`.

# Imputation m of a flong data frame, each column with its attributes
# (variable and value labels, factor codes), which row subsetting drops
# from plain vectors and factors.
golden_mi_data <- function(d, m) {
  x <- d[d$mi_m == m, , drop = FALSE]
  for (v in names(x)) attributes(x[[v]]) <- attributes(d[[v]])
  rownames(x) <- NULL
  x
}

# One fit per imputation: fit_fun(data of imputation m), m = 1..M.
golden_mi_fits <- function(d, fit_fun, m = setdiff(sort(unique(d$mi_m)), 0)) {
  lapply(m, function(i) fit_fun(golden_mi_data(d, i)))
}

# The C4 goldens are exact-mode: every displayed cell, console line and
# sink byte is compared exactly (C4 review F4). Only stored full-precision
# values that carry Stata's convergence gap get a tolerance, each the
# smallest 1-2-5 step at or above the largest difference measured on
# 2026-09-26 (never below 1e-9): Stata's logit and stcox stop ~1e-8
# (relative) from the optimum R's fits reach, which moves the pooled
# estimates (r(table)) and, through the between-imputation variance, the
# largest FMI. (helper-golden.R, which defines both vectors, sorts before
# this file.)
golden_stored_tol_override[c("MI02", "MI03", "MI07")] <- 2e-9  # fmi_1 1.6e-9
golden_table_tol_override[["MI04"]] <- 1e-8                     # table 7.0e-9 (stcox); logit ones get 2e-8
golden_table_tol_override[["ST06"]] <- 2e-9                     # table 1.5e-9 (survreg vs streg [pw])
