# Synthetic inputs of the Phase 5 review fixtures
# (tests/testthat/fixtures/regtab_phase5_review/, IMPLEMENTATION_PLAN.md
# "Phase 5 review outcome"), as the reviewer generated them:
#   cntg.dta     overdispersed counts in 40 clinics (menbreg, mepoisson,
#                melogit binomial(n), tobit/intreg on a censored copy)
#   bslope2.dta  binary outcome with correlated random intercept and slope
#                in 80 sites (mecloglog, meprobit)
# Run from the repo root: Rscript qa/make_regtab_phase5_review_data.R
# (then qa/stata/make_regtab_phase5_review.do). The committed files are
# the reference; a rerun reproduces their data (haven::read_dta() identical).
out <- "tests/testthat/fixtures/regtab_phase5_review"
lab <- function(x, l) {
  attr(x, "label") <- l
  x
}
set.seed(5501)
G <- 40
m <- 25
g <- rep(1:G, each = m)
x1 <- rnorm(G * m)
u <- rnorm(G, 0, 0.5)[g]
mu <- exp(0.3 + 0.4 * x1 + u)
y <- rnbinom(G * m, size = 2, mu = mu)
cnt <- data.frame(y = lab(y, "Visits"), x1 = lab(x1, "Exposure score"), clinic = lab(g, "Clinic ID"),
                  grp3 = haven::labelled(as.numeric(sample(1:3, G * m, TRUE)), c(Low = 1, Mid = 2, High = 3),
                                         label = "Risk group"))
haven::write_dta(cnt, file.path(out, "cntg.dta"))
set.seed(7702)
G <- 80
m <- 40
g <- rep(1:G, each = m)
x <- rnorm(G * m)
z0 <- rnorm(G)
z1 <- 0.6 * z0 + 0.8 * rnorm(G)
u0 <- (0.9 * z0)[g]
u1 <- (0.5 * z1)[g]
yy <- as.numeric(runif(G * m) < plogis(-0.5 + (0.7 + u1) * x + u0))
bs <- data.frame(y = lab(yy, "Event"), x = lab(x, "Dose (mg)"), site = lab(g, "Site"),
                 female = lab(as.numeric(runif(G * m) < 0.5), "Female"))
haven::write_dta(bs, file.path(out, "bslope2.dta"))
