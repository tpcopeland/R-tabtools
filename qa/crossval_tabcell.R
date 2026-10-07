library(testthat)
library(tabtools)

test_that("installed exact binomial cells match binom.test and tail inversion", {
  for (total in c(1L, 2L, 20L, 100L)) {
    successes <- unique(c(0L, 1L, as.integer(total/2), total))
    for (level in c(.9, .95, .99)) {
      result <- tabcell("np", n = successes, d = total, ci = "exact", level = level)
      publication <- attr(result, "provenance")$publication
      alpha2 <- (1 - level) / 2
      for (j in seq_along(successes)) {
        count <- successes[j]
        oracle <- stats::binom.test(count, total, conf.level = level)$conf.int
        expect_equal(publication$lb[j]/100, unname(oracle[1L]), tolerance = 2e-13)
        expect_equal(publication$ub[j]/100, unname(oracle[2L]), tolerance = 2e-13)
        if (count > 0) {
          expect_equal(stats::pbinom(count - 1L, total, publication$lb[j]/100,
                                     lower.tail = FALSE), alpha2, tolerance = 2e-12)
        }
        if (count < total) {
          expect_equal(stats::pbinom(count, total, publication$ub[j]/100),
                       alpha2, tolerance = 2e-12)
        }
      }
    }
  }
})

test_that("installed exact rate cells match poisson.test and Poisson tails", {
  events <- c(0L, 1L, 4L, 20L, 100L)
  for (exposure in c(.25, 2, 1000)) {
    for (level in c(.9, .95, .99)) {
      result <- tabcell("rate", e = events, pt = exposure, per = 1000, level = level)
      publication <- attr(result, "provenance")$publication
      alpha2 <- (1 - level) / 2
      for (j in seq_along(events)) {
        count <- events[j]
        oracle <- stats::poisson.test(count, T = exposure, conf.level = level)$conf.int
        expect_equal(publication$lb[j]/1000, unname(oracle[1L]), tolerance = 2e-13)
        expect_equal(publication$ub[j]/1000, unname(oracle[2L]), tolerance = 2e-13)
        if (count > 0) {
          expect_equal(stats::ppois(count - 1L, publication$lb[j] * exposure/1000,
                                    lower.tail = FALSE), alpha2, tolerance = 2e-12)
        }
        expect_equal(stats::ppois(count, publication$ub[j] * exposure/1000),
                     alpha2, tolerance = 2e-12)
      }
    }
  }
})

test_that("installed scalar vectors and S3 projections preserve masks", {
  result <- tabcell("enp", e = c(3, 1, 0), n = c(40, 3, 0), mincell = 5)
  expect_identical(as.character(result), c("<5/40", "<5", "0/0"))
  expect_s3_class(as.data.frame(result)$cell, "tt_cell")
  selected <- result[c(2, 1)]
  expect_identical(as.character(selected), c("<5", "<5/40"))
  expect_identical(attr(selected, "provenance")$rows$source_index, c(2L, 1L))
  parent <- puttab(as.data.frame(selected), xlsx = NULL, markdown = NULL)
  expect_identical(parent$body[[1L]], c("<5", "<5/40"))
  expect_equal(parent$meta$cell_provenance[[1L]]$provenance$smallcells$n_masked, 2L)
})

test_that("installed supplied inference remains separate from publication scale", {
  original <- list(estimate = log(2), conf.low = 0, conf.high = log(4),
                   std.error = .2, p.value = .017, df = 7, conf.level = .95,
                   effect_scale = "coefficient", se_scale = "coefficient")
  result <- tabcell("est", contrast = original, eform = TRUE, scale = 10)
  expect_identical(as.character(result), "20.00 (10.00, 40.00)")
  expect_identical(attr(result, "provenance")$inference[[1L]], original)
  expect_identical(attr(result, "provenance")$rows$reference, "t")
  expect_equal(attr(result, "provenance")$rows$df, 7)
  expect_equal(attr(result, "provenance")$publication$estimate, 20, tolerance = 2e-14)
})
