# Developer-only: writes the two synthetic golden fixtures added by the
# Phase 4 review (IMPLEMENTATION_PLAN.md, "Phase 4 review outcome"):
#   tests/testthat/golden/fixtures/nbsim.dta     overdispersed counts (nbreg,
#     glm.nb: observed information, Pseudo R2) and a 1..3 labelled group
#   tests/testthat/golden/fixtures/autolong.dta  auto with mpg/weight
#     variable labels over 70 characters (fvgen's 80-character cut)
# Run from the repo root: Rscript qa/make_regtab_fixtures.R. Needs haven and
# MASS. The committed .dta files are the reference; rerunning rewrites them
# (MASS::rnegbin draws are stable for a given R and MASS version).
stopifnot(requireNamespace("haven", quietly = TRUE), requireNamespace("MASS", quietly = TRUE))
fx <- file.path("tests", "testthat", "golden", "fixtures")

set.seed(4242)
n <- 400
x1 <- round(stats::rnorm(n), 4)
grp <- sample(1:3, n, TRUE)
mu <- exp(0.3 + 0.4 * x1 + 0.3 * (grp == 2))
y <- MASS::rnegbin(n, mu, theta = 1.5)
w <- sample(0:3, n, TRUE)
bin <- stats::rbinom(n, 1, stats::plogis(-0.2 + 0.5 * x1))
d <- data.frame(y = y, x1 = x1,
                grp = haven::labelled(grp, c(Low = 1, Mid = 2, High = 3), label = "Group"),
                w = w, bin = bin, mid = as.numeric(grp == 2), high = as.numeric(grp == 3))
attr(d$x1, "label") <- "Exposure score"
attr(d$y, "label") <- "Count outcome"
attr(d$mid, "label") <- "Mid group"
attr(d$high, "label") <- "High group"
attr(d$bin, "label") <- "Binary outcome"
attr(d$w, "label") <- "Frequency weight"
haven::write_dta(d, file.path(fx, "nbsim.dta"))

a <- haven::read_dta(file.path(fx, "auto.dta"))
attr(a$mpg, "label") <- "Mileage in miles per gallon measured on the EPA city driving cycle in 1978"
attr(a$weight, "label") <- "Curb weight of the vehicle in pounds as reported by the manufacturer"
haven::write_dta(a, file.path(fx, "autolong.dta"))
cat("wrote nbsim.dta, autolong.dta\n")
