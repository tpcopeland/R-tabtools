# Synthetic inputs of the milestone 5w review fixtures
# (tests/testthat/fixtures/regtab_5w_review/, IMPLEMENTATION_PLAN.md
# "Milestone 5w review outcome"), as the reviewer generated them (data the
# implementing agent never saw), plus three columns for the fixes:
#   syn.dta    900 subjects: non-integer IPTW `iptw`, rounded `iw`, zero
#              weights `wz` (cluster 1 of `clusz` all zero), NA weights
#              `wna`, constant `wc`; 2, 3, 12 and 6 unequal clusters; a
#              factor `f4` with a level empty when x2 == 1; tied integer
#              survival times; a survey design (4 strata, 16 PSUs, FPC, a
#              lonely-PSU stratum)
#   synpp.dta  1,557 person-periods of 400 subjects with time-varying
#              weights `sw` (clusters cross the weights), plus `wid` (the
#              subject's first `sw`, constant within id), `wz0` (`sw` with
#              every seventh row zero), and `t0`/`t1` (counting-process
#              times)
# Run from the repo root: Rscript qa/make_regtab_5w_review_data.R (then
# qa/stata/make_regtab_5w_review.do). The committed files are the
# reference; a rerun reproduces their data (haven::read_dta() identical).
out <- "tests/testthat/fixtures/regtab_5w_review"
set.seed(20260925)
n <- 900
id <- seq_len(n)
x1 <- round(rnorm(n, 50, 10), 1)
x2 <- rbinom(n, 1, 0.45)
f3 <- sample(1:3, n, TRUE, prob = c(.5, .3, .2))
ps <- plogis(-2 + 0.03 * x1 + 0.5 * x2 + 0.3 * (f3 == 3))
treat <- rbinom(n, 1, ps)
iptw <- ifelse(treat == 1, 1 / ps, 1 / (1 - ps))
iptw <- round(iptw, 6)                       # non-integer weights
iw <- pmax(1, round(iptw))                   # integer (rounded) weights
eta <- -3 + 0.6 * treat + 0.04 * x1 - 0.4 * x2 + 0.5 * (f3 == 2)
y <- rbinom(n, 1, plogis(eta))
expo <- round(runif(n, 0.5, 3), 3)
cnt <- rpois(n, expo * exp(-1 + 0.3 * treat + 0.01 * x1))
yc <- round(100 + 5 * treat + 0.8 * x1 + rnorm(n, 0, 12), 2)
ypos <- round(exp(1 + 0.2 * treat + 0.01 * x1 + rnorm(n, 0, .4)), 4)
# survival with ties (integer months)
tt <- rexp(n, 0.02 * exp(0.5 * treat + 0.02 * (x1 - 50)))
cens <- runif(n, 5, 60)
time <- pmax(1, ceiling(pmin(tt, cens)))
event <- as.integer(tt <= cens)
# clusters
clus2 <- ifelse(id <= 600, 1L, 2L)                       # 2 clusters, unequal
clus3 <- cut(id, c(0, 100, 350, 900), labels = FALSE)    # 3 clusters, unequal
clusu <- sample(1:12, n, TRUE, prob = (1:12)^1.5)        # 12 unequal clusters
# weights with zeros; cluster 1 of clusz is entirely zero-weight
clusz <- ((id - 1) %% 6) + 1
wz <- ifelse(clusz == 1 | id %% 7 == 0, 0, iptw)
# NA weights
wna <- iptw; wna[c(5, 50, 500)] <- NA
# constant weight
wc <- rep(2.5, n)
# factor with empty level in the subset x2 == 1 (level 3 absent)
f4 <- f3; f4[x2 == 1 & f4 == 3] <- 1
# NA in cluster var
clna <- clusu; clna[c(7, 70)] <- NA
# survey design: 4 strata, PSUs nested, stratum 4 has one PSU (lonely)
strata <- ifelse(id <= 300, 1L, ifelse(id <= 600, 2L, ifelse(id <= 850, 3L, 4L)))
psu <- ifelse(strata == 4, 40L, strata * 10L + ((id %% 5) + 1L))
fpc <- ifelse(strata == 1, 20, ifelse(strata == 2, 15, ifelse(strata == 3, 30, 3)))
d <- data.frame(id, x1, x2, f3, f4, treat, iptw, iw, wz, wna, wc, y, cnt, expo, yc, ypos,
                time, event, clus2, clus3, clusu, clusz, clna, strata, psu, fpc)
d$lexpo <- log(d$expo)
haven::write_dta(d, file.path(out, "syn.dta"), version = 15)

# Person-period data with time-varying stabilized weights (weights vary
# within id, so clusters cross the weights).
pp <- do.call(rbind, lapply(seq_len(400), function(i) {
  K <- sample(2:8, 1)
  a <- rbinom(1, 1, 0.4)
  z <- round(rnorm(1), 3)
  per <- seq_len(K)
  sw <- round(exp(rnorm(K, 0, 0.35)), 5)
  ev <- rbinom(K, 1, plogis(-2.5 + 0.4 * a + 0.3 * z + 0.1 * per))
  last <- which(ev == 1)[1]
  if (!is.na(last)) { per <- per[1:last]; sw <- sw[1:last]; ev <- ev[1:last] }
  data.frame(id = i, period = per, a = a, z = z, sw = sw, ev = ev,
             pt = round(runif(length(per), 0.2, 1), 3), site = (i %% 5) + 1L)
}))
# Added for the fixes: a per-subject weight (Stata's stset/xtgee need
# weights constant within id), zero weights, counting-process times.
pp$wid <- ave(pp$sw, pp$id, FUN = function(x) x[1])
pp$wz0 <- ifelse(seq_len(nrow(pp)) %% 7 == 0, 0, pp$sw)
pp$t0 <- pp$period - 1
pp$t1 <- pp$period
haven::write_dta(pp, file.path(out, "synpp.dta"), version = 15)
cat("syn", nrow(d), "synpp", nrow(pp), "ids", length(unique(pp$id)), "\n")
