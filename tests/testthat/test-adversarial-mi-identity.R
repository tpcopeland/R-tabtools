test_that("explicit MI IDs detect equal-sized samples after row names are erased", {
  d <- data.frame(id = paste0("subject", 1:12), x = 1:12,
                  y = c(3, 7, 5, 9, 10, 8, 14, 13, 17, 15, 20, 19))
  a <- d[-1, ]
  b <- d[-2, ]
  rownames(a) <- rownames(b) <- NULL
  fits <- list(stats::lm(y ~ x, a), stats::lm(y ~ x, b))
  expect_s3_class(regtab(tt_mi(fits)), "tt_table")
  mi <- tt_mi(fits, observation_ids = list(a$id, b$id), sample_check = "strict")
  expect_error(regtab(mi), class = "tabtools_error_mi_sample_mismatch")
  expect_error(tt_vcov(mi), class = "tabtools_error_mi_sample_mismatch")
})

test_that("strict MI checks require explicit subject identity", {
  fit <- stats::lm(mpg ~ wt, mtcars)
  expect_error(tt_mi(list(fit, fit), sample_check = "strict"),
               class = "tabtools_error_mi_sample_identity")
  expect_error(tt_mi(tt_mi(list(fit, fit)), sample_check = "strict"),
               class = "tabtools_error_mi_sample_identity")
  legacy <- structure(list(analyses = list(fit, fit)), class = "tt_mi")
  expect_s3_class(regtab(legacy), "tt_table")
})

test_that("MI ID validation refuses missing, recycled and duplicate identity", {
  fit <- stats::lm(mpg ~ wt, mtcars)
  ids <- seq_len(nrow(mtcars))
  bad <- list(ids, list(ids), list(ids, ids[-1]), list(ids, rep(1, length(ids))),
              list(ids, replace(ids, 1, NA_real_)), list(ids, replace(ids, 1, Inf)),
              list(ids, replace(ids, 1, NaN)), list(ids, factor(ids)),
              list(ids, replace(as.character(ids), 1, " ")),
              list(ids, matrix(ids, ncol = 1)))
  for (x in bad) {
    expect_error(tt_mi(list(fit, fit), observation_ids = x),
                 class = "tabtools_error_mi_observation_ids")
  }
  mi <- tt_mi(list(fit, fit), observation_ids = list(ids, ids))
  mi$observation_ids[[2]][1] <- mi$observation_ids[[2]][2]
  expect_error(tt_vcov(mi), class = "tabtools_error_mi_observation_ids")
})

test_that("explicit MI IDs permit reordering and differing row-name conventions", {
  d <- mtcars
  d$id <- paste0("subject", seq_len(nrow(d)))
  a <- d
  b <- d[rev(seq_len(nrow(d))), ]
  rownames(a) <- paste0("a", seq_len(nrow(a)))
  rownames(b) <- paste0("b", seq_len(nrow(b)))
  fits <- list(stats::lm(mpg ~ wt, a), stats::lm(mpg ~ wt, b))
  before <- fits
  mi <- tt_mi(fits, observation_ids = list(a$id, b$id), sample_check = "strict")
  V <- tt_vcov(mi)
  expect_equal(V, stats::vcov(fits[[1]]), tolerance = 1e-12)
  tt <- regtab(fits[[1]], mi)
  expect_null(tt$meta$mi_sample_identity[[1]])
  expect_identical(tt$meta$mi_sample_identity[[2]],
                   list(method = "explicit_ids", n = c(32, 32), strict = TRUE))
  expect_false(any(grepl("subject", unlist(tt$meta$mi_sample_identity))))
  expect_identical(fits, before)
  expect_identical(tt_mi(mi), mi)
  expect_identical(tt_mi(mi, sample_check = "auto")$observation_ids, mi$observation_ids)
  expect_error(tt_mi(mi, observation_ids = NULL), class = "tabtools_error_mi_sample_identity")
})

test_that("MI IDs describe observed positive-weight rows rather than input rows", {
  d <- data.frame(id = paste0("s", 1:12), x = 1:12,
                  y = c(2, 6, 4, 9, 8, 11, 12, 10, 16, 14, 17, 18),
                  w = c(1, 0, 2, 1, 3, 1, 0, 2, 1, 1, 1, 1))
  d$x[4] <- NA_real_
  retained <- !is.na(d$x) & d$w > 0
  fits <- list(stats::lm(y ~ x, d, weights = w, na.action = stats::na.exclude),
               stats::lm(y ~ x, d, weights = w, na.action = stats::na.omit))
  ids <- d$id[retained]
  mi <- tt_mi(fits, observation_ids = list(ids, ids), sample_check = "strict")
  expect_identical(regtab(mi)$meta$mi_sample_identity[[1]]$n, c(9, 9))
  expect_equal(tt_vcov(mi), stats::vcov(fits[[1]]), tolerance = 1e-12)
  expect_error(tt_mi(fits, observation_ids = list(d$id, d$id)),
               class = "tabtools_error_mi_observation_ids")
})

test_that("default MI metadata distinguishes row-name checks from count-only checks", {
  a_data <- mtcars
  b_data <- mtcars
  rownames(b_data) <- paste0("other", seq_len(nrow(b_data)))
  a <- stats::lm(mpg ~ wt, a_data)
  b <- stats::lm(mpg ~ wt, b_data)
  expect_identical(regtab(tt_mi(list(a, a)))$meta$mi_sample_identity[[1]]$method, "row_names")
  expect_identical(regtab(tt_mi(list(a, b)))$meta$mi_sample_identity[[1]],
                   list(method = "counts_only", n = c(32, 32), strict = FALSE))
})

test_that("MI diagnostic errors keep their class and name the failed imputation", {
  d <- data.frame(x = 1:10, y = c(1, 1, 3, 4, 5, 8, 9, 10, 12, 16))
  good <- stats::glm(y ~ x, d, family = stats::poisson())
  expect_warning(bad <- stats::glm(y ~ x, d, family = stats::poisson(),
                                   control = stats::glm.control(maxit = 1L)), "did not converge")
  mi <- tt_mi(list(good, bad))
  for (consume in list(regtab, tt_vcov)) {
    expect_error(consume(mi), "imputation 2: fitted-model diagnostics failed",
                 class = "tabtools_error_model_convergence")
  }
})
