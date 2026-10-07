library(testthat)
library(tabtools)

# Parse the QA producer but evaluate only its local estimator assignment.
# No workbook, console, random-number stream or other demo section executes.
demo_aipw_function <- function() {
  expressions <- as.list(parse(file = test_path("demo", "demo_tabtools.R"))[[1L]][[2L]])[-1L]
  selected <- vapply(expressions, function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("aipw_estimate")), logical(1))
  if (sum(selected) != 1L) stop("Expected exactly one AIPW producer definition")
  environment <- new.env(parent = baseenv())
  for (name in c("glm", "binomial", "na.fail", "lm", "predict", "fitted", "model.matrix", "coef", "setNames")) {
    environment[[name]] <- getExportedValue("stats", name)
  }
  eval(expressions[[which(selected)]], envir = environment)
  environment$aipw_estimate
}

demo_aipw_profiles <- function() {
  d <- expand.grid(diabetes = 0:1, hypertension = 0:1, anxiety = 0:1,
                   female = 0:1, education = 0:1)
  d$education <- factor(d$education)
  d$index_age <- 1 + seq_len(nrow(d))^2 / 1024
  d
}

test_that("AIPW balanced literal sample has exact ATE, POM and covariance", {
  profiles <- demo_aipw_profiles()
  d <- profiles[rep(seq_len(nrow(profiles)), each = 4L), ]
  d$treated <- rep(c(0, 0, 1, 1), nrow(profiles))
  d$cv_event <- 5 + 2 * d$treated + rep(c(-1, 1, -1, 1), nrow(profiles))
  original <- d
  out <- demo_aipw_function()(d)
  # Both arms share every covariate profile and have paired +/-1 residuals.
  # p=.5, nuisance derivatives of POMs average zero, N=128. The ATE
  # influence is +/-2; POM0 influence is +/-2 in half the records, else0.
  expect_equal(out$estimate, c(2, 5), tolerance = 1e-12)
  expect_equal(out$std.error, c(1 / sqrt(32), 1 / 8), tolerance = 1e-12)
  expect_equal(unname(attr(out, "vcov")), matrix(c(1/32, -1/64, -1/64, 1/64), 2), tolerance = 1e-12)
  expect_identical(attr(out, "aipw_ee")$N, 128L)
  expect_identical(out$equation, c("ATE", "POmean"))
  expect_identical(out$term, c("r1vs0.treated", "0.treated"))
  expect_identical(attr(out, "tt_estimator"), "aipw")
  expect_identical(d, original)
})

test_that("all AIPW Jacobian blocks agree with independent numerical derivatives", {
  profiles <- demo_aipw_profiles()
  d <- profiles[rep(seq_len(nrow(profiles)), each = 6L), ]
  within <- rep(seq_len(6L), nrow(profiles))
  d$treated <- as.numeric(within <= 1 + 3 * d$diabetes + d$female)
  d$cv_event <- d$index_age^2 + .3 * d$diabetes * d$anxiety +
    d$treated * (.6 + .4 * d$female * d$hypertension) + (within %% 3L - 1) / 4
  out <- demo_aipw_function()(d)
  ee <- attr(out, "aipw_ee")
  X <- stats::model.matrix(~ index_age + female + education + diabetes + hypertension + anxiety, d)
  k <- ncol(X)
  b0 <- seq_len(k) + 2L
  b1 <- b0 + k
  gamma <- b1 + k
  # Independently evaluate the published estimating functions at arbitrary
  # theta, not the production derivative code or fitted-model covariance.
  score <- function(theta) {
    p <- stats::plogis(drop(X %*% theta[gamma]))
    m0 <- drop(X %*% theta[b0])
    m1 <- drop(X %*% theta[b1])
    a <- d$treated
    y <- d$cv_event
    cbind(m0 + (1-a)*(y-m0)/(1-p) - theta[1],
          m1 + a*(y-m1)/p - theta[2],
          X*((1-a)*(y-m0)), X*(a*(y-m1)), X*(a-p))
  }
  numerical <- vapply(seq_along(ee$theta), function(j) {
    h <- .Machine$double.eps^(1/3) * (1 + abs(ee$theta[j]))
    plus <- minus <- ee$theta
    plus[j] <- plus[j] + h
    minus[j] <- minus[j] - h
    (colMeans(score(plus)) - colMeans(score(minus))) / (2*h)
  }, numeric(length(ee$theta)))
  expect_equal(unname(ee$jacobian), unname(numerical), tolerance = 1e-7)
  expect_gt(max(abs(ee$jacobian[1:2, c(b0,b1,gamma)])), 1e-4)
  expect_lt(max(abs(colMeans(score(ee$theta)))), 1e-7)
  inverse <- solve(numerical)
  V <- inverse %*% crossprod(score(ee$theta)) %*% t(inverse) / nrow(d)^2
  contrast <- matrix(c(-1,1,1,0), 2, byrow = TRUE)
  expected <- contrast %*% V[1:2,1:2] %*% t(contrast)
  expect_equal(unname(attr(out, "vcov")), unname(expected), tolerance = 1e-7)
  # Preserve the existing plug-in point estimates exactly on the same fits.
  formula <- cv_event ~ index_age + female + education + diabetes + hypertension + anxiety
  p <- stats::fitted(stats::glm(treated ~ index_age + female + education + diabetes + hypertension + anxiety,
                              family = stats::binomial, data = d))
  m0 <- stats::predict(stats::lm(formula, data = d[d$treated==0,]), newdata = d)
  m1 <- stats::predict(stats::lm(formula, data = d[d$treated==1,]), newdata = d)
  f0 <- m0 + (1-d$treated)*(d$cv_event-m0)/(1-p)
  f1 <- m1 + d$treated*(d$cv_event-m1)/p
  expect_identical(out$estimate, c(mean(f1-f0), mean(f0)))
})

test_that("actual native demo cohort agrees with full precision Stata AIPW covariance", {
  source(test_path("tools", "demo_data.R"), local = TRUE)
  cohort <- test_path("demo", "data", "native-2.5.1", "cohort.dta")
  expect_identical(demo_sha256(cohort), "b0eca9c0df532aca005c7e07ae2074c7575389af2c4d7f6dca3e2d0b413ee7c1")
  paths <- test_path("data", c("demo_aipw-b.csv", "demo_aipw-V.csv", "demo_aipw-meta.csv"))
  if (!all(file.exists(paths))) stop("Reviewed full-precision native AIPW control fixtures are required; missing controls are not a pass")
  b <- utils::read.csv(paths[1], stringsAsFactors = FALSE)
  V <- utils::read.csv(paths[2], stringsAsFactors = FALSE)
  meta <- utils::read.csv(paths[3], stringsAsFactors = FALSE)
  expect_identical(b$term[1:2], c("ATE:r1vs0.treated", "POmean:0.treated"))
  expect_equal(meta$converged, 1)
  expect_identical(meta$subcommand, "aipw")
  expect_identical(meta$outcome_model, "linear")
  expect_identical(meta$treatment_model, "logit")
  expect_identical(meta$vce, "robust")
  d <- as.data.frame(haven::read_dta(cohort))
  d <- tt_as_factor(d, vars = "education")
  d$treated <- as.numeric(d$treated)
  out <- demo_aipw_function()(d)
  wanted <- matrix(NA_real_, 2, 2)
  selected <- V$row <= 2 & V$col <= 2
  wanted[cbind(V$row[selected], V$col[selected])] <- V$value[selected]
  expect_equal(out$estimate, b$value[1:2], tolerance = 1e-7)
  expect_equal(unname(attr(out, "vcov")), wanted, tolerance = 1e-7)
  expect_equal(out$std.error, sqrt(diag(wanted)), tolerance = 1e-7)
  expect_equal(attr(out, "aipw_ee")$N, meta$N)
  p <- 2 * stats::pnorm(-abs(out$estimate[1] / out$std.error[1]))
  native_p <- 2 * stats::pnorm(-abs(b$value[1] / sqrt(wanted[1,1])))
  expect_equal(p, native_p, tolerance = 1e-7)
})
