test_that("an intreg scale row beside ZIP preserves the count schema and inference", {
  skip_if_not_installed("survival")
  skip_if_not_installed("pscl")
  d <- data.frame(y = c(0, 0, 1, 2, 1, 3, 0, 4, 2, 1, 0, 3,
                        1, 2, 0, 4, 1, 3, 2, 0, 4, 2, 1, 3),
                  x = seq(-1, 1, length.out = 24), z = rep(c(-1, 1), 12))
  fi <- survival::survreg(survival::Surv(y, y > 0, type = "left") ~ x,
                         data = d, dist = "gaussian")
  fp <- pscl::zeroinfl(y ~ x | z, data = d)
  original_fit <- fi
  original_data <- d
  tt <- regtab(fi, fp, keepintercept = TRUE, models = c("Intreg", "ZIP"))
  raw <- tt$meta$regtab_raw_rows
  scale <- raw[raw$model == 1L & raw$key == "lnsigma::_cons", , drop = FALSE]
  expect_identical(nrow(scale), 1L)
  expect_identical(scale$status, "est")
  expect_identical(scale$ancillary, TRUE)
  expect_identical(scale$term, "Log(scale)")
  expect_identical(scale$parent_key, "")
  expect_identical(scale$term_signature, "")
  expect_identical(scale$count_source_key, "_cons")
  expect_identical(scale$level_identity, "")
  expect_identical(scale$variance_ok, TRUE)
  se <- sqrt(stats::vcov(fi)["Log(scale)", "Log(scale)"])
  b <- unname(log(fi$scale))
  expect_equal(scale$estimate, b, tolerance = 1e-12)
  expect_equal(c(scale$conf.low, scale$conf.high),
               b + c(-1, 1) * stats::qnorm(.975) * se, tolerance = 1e-12)
  expect_equal(scale$p.value, 2 * stats::pnorm(abs(b / se), lower.tail = FALSE),
               tolerance = 1e-12)
  counts <- tt_fitcount(fi, events = "y", data = d, terms = TRUE)
  counted <- regtab(fi, fp, keepintercept = TRUE, models = c("Intreg", "ZIP"),
                    fitcounts = list(counts, NULL))
  records <- counted$meta$regtab_raw_rows
  scale_counted <- records[records$model == 1L & records$key == "lnsigma::_cons", , drop = FALSE]
  expect_identical(nrow(scale_counted), 1L)
  expect_identical(scale_counted$status, "est")
  expect_true(is.na(scale_counted$count_events))
  expect_equal(scale_counted$estimate, b, tolerance = 1e-12)
  expect_identical(fi, original_fit)
  expect_identical(d, original_data)
})
