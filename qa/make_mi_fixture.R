# Multiply imputed golden fixture (tasks 5.14, C4): an R-imputed extract of
# the cohort fixture, written in flong form so that Stata's `mi import
# flong` and R analyse the very same imputations (goldens MI01-MI0x,
# tests/testthat/golden/fixtures/mi_cohort.dta).
#
#   400 cohort rows (id 1-400); index_age (15%) and education (10%) set
#   missing at random with a fixed seed, then imputed by mice (pmm and
#   polyreg, m = 5, maxit = 10, seed 20260926). Rows: m = 0 (the
#   incomplete data, as `mi import flong` needs it) and m = 1..5, with
#   `mi_m` the imputation number and `id` the observation id.
#
# Needs mice, which is not a tabtools dependency: install it into a
# temporary library and put that library first, e.g.
#   R_LIBS=/tmp/rlib Rscript qa/make_mi_fixture.R
# Run from the repo root. The committed .dta is the reference; mice's
# imputations may change between mice versions, so a rerun can give other
# numbers (regenerate the MI goldens with it).
if (!requireNamespace("mice", quietly = TRUE)) stop("mice is needed (in a temporary library).", call. = FALSE)
src <- haven::read_dta("tests/testthat/golden/fixtures/cohort.dta")
d <- as.data.frame(src[1:400, c("id", "female", "index_age", "education", "treated", "cv_event",
                                "follow_up", "bmi", "crp", "region")])
set.seed(20260926)
d$index_age[stats::runif(nrow(d)) < 0.15] <- NA
d$education[stats::runif(nrow(d)) < 0.10] <- NA
w <- d
w$education <- factor(as.vector(w$education))
for (v in names(w)) if (!is.factor(w[[v]])) w[[v]] <- as.vector(w[[v]])
meth <- mice::make.method(w)
meth[] <- ""
meth[c("index_age", "education")] <- c("pmm", "polyreg")
pred <- mice::make.predictorMatrix(w)
pred[, "id"] <- 0
imp <- mice::mice(w, m = 5, method = meth, predictorMatrix = pred, maxit = 10, seed = 20260926,
                  printFlag = FALSE)
long <- mice::complete(imp, "long", include = TRUE)
out <- data.frame(mi_m = as.integer(long$.imp), long[, names(d)])
out$education <- as.numeric(as.character(out$education))
out <- out[order(out$mi_m, out$id), ]
rownames(out) <- NULL
# Labels as in the cohort fixture (value labels and variable labels).
for (v in names(d)) {
  lab <- attr(src[[v]], "label")
  vl <- attr(src[[v]], "labels")
  x <- out[[v]]
  if (!is.null(vl)) x <- haven::labelled(x, labels = vl)
  if (!is.null(lab)) attr(x, "label") <- lab
  out[[v]] <- x
}
attr(out$mi_m, "label") <- "Imputation number"
haven::write_dta(out, "tests/testthat/golden/fixtures/mi_cohort.dta")
cat("mi_cohort.dta:", nrow(out), "rows,", sum(is.na(d$index_age)), "age and",
    sum(is.na(d$education)), "education values imputed\n")
