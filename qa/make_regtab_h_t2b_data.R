# Synthetic inputs of the Milestone H group t2b fixtures
# (tests/testthat/fixtures/regtab_h_t2b/, IMPLEMENTATION_PLAN.md, "Group t2b
# outcome"):
#   offs.dta   400 observations with an offset `off` and an exposure
#              `expo` (ln exposure `lnexpo`): binary `yb`, three-level
#              ordered `yo`, count `yc` (task H21, pseudo R-squared with an
#              offset)
#   lung.dta   survival::lung with status recoded 0/1 (task H4, streg
#              vce(cluster inst): G/(G - 1) with G the institutions)
#   gee.dta    60 clusters of 1-6 observations, sorted by `id`, Gaussian `y`
#              (task H19, xtgee scale(2))
# Run from the repo root: Rscript qa/make_regtab_h_t2b_data.R (then
# qa/stata/make_regtab_h_t2b.do). The committed files are the reference; a
# rerun reproduces their data (haven::read_dta() identical).
out <- "tests/testthat/fixtures/regtab_h_t2b"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

set.seed(20260926)
n <- 400
x <- round(rnorm(n), 4)
off <- round(rnorm(n, 0, 0.8), 4)
expo <- round(runif(n, 0.5, 4), 4)
yb <- rbinom(n, 1, stats::plogis(-0.3 + 0.5 * x + off))
lat <- 0.6 * x + off + stats::rlogis(n)
yo <- as.integer(cut(lat, c(-Inf, -0.5, 1, Inf)))
yc <- rpois(n, expo * exp(-0.5 + 0.3 * x))
offs <- data.frame(id = seq_len(n), x = x, off = off, expo = expo, lnexpo = log(expo),
                   yb = yb, yo = yo, yc = yc)
haven::write_dta(offs, file.path(out, "offs.dta"))

lung <- survival::lung
lung$status <- lung$status - 1L
haven::write_dta(lung[, c("inst", "time", "status", "age", "sex")], file.path(out, "lung.dta"))

set.seed(20260927)
G <- 60
size <- sample(1:6, G, TRUE)
id <- rep(seq_len(G), size)
u <- rnorm(G, 0, 0.7)[id]
xg <- round(rnorm(length(id)), 4)
y <- round(1 + 0.5 * xg + u + rnorm(length(id), 0, 1.3), 4)
haven::write_dta(data.frame(id = id, x = xg, y = y), file.path(out, "gee.dta"))
