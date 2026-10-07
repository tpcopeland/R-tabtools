test_that("declared lincom retains original Cauchy inference before publication changes", {
  # For df=1, the t distribution is Cauchy: these limits and p=.5 have
  # an independent elementary oracle, rather than another call to qt/pt.
  q95 <- tan(.475 * pi)
  input <- list(estimate = 2, std.error = 2, df = 1, conf.level = .95,
    conf.low = 2 - 2 * q95, conf.high = 2 + 2 * q95,
    statistic = 1, p.value = .5, effect_scale = "coefficient",
    native_source = "lincom", term = "difference")
  before <- input
  x <- tabcell("est", contrast = input, level = .9, eform = TRUE, scale = .1)
  p <- attr(x, "provenance")
  expect_identical(input, before)
  expect_identical(p$inference[[1L]], before)
  expect_identical(p$rows$source, "contrast")
  expect_identical(p$rows$native_source, "lincom")
  expect_identical(p$rows$reference, "t")
  expected <- list(missing = 0L, estimate = .1 * exp(2),
    lb = .1 * exp(2 - 2 * tan(.45 * pi)),
    ub = .1 * exp(2 + 2 * tan(.45 * pi)), level = 90, scale = .1,
    lincom_estimate = 2, se = 2, lincom_lb = before$conf.low,
    lincom_ub = before$conf.high, lincom_level = 95, t = 1, p = .5, df = 1)
  expect_setequal(names(p$native_returns[[1L]]), names(expected))
  expect_equal(p$native_returns[[1L]][names(expected)], expected, tolerance = 1e-11)
  expect_equal(p$publication$estimate, .1 * exp(2), tolerance = 1e-13)
  original <- tabcell("est", contrast = input)
  expect_identical(as.character(original), "2.00 (-23.41, 27.41)")
  expect_equal(attr(original, "provenance")$native_returns[[1L]]$lincom_level, 95)
})

test_that("normal lincom projection has z and preserves original ratio SE semantics", {
  # Independently tabulated standard-normal critical values and two-sided p.
  q95 <- 1.959963984540054
  q90 <- 1.644853626951472
  input <- list(estimate = .2, std.error = .1, df = Inf, conf.level = .95,
    effect_scale = "coefficient", native_source = "lincom")
  x <- tabcell("est", contrast = input, level = .9, eform = TRUE, scale = 10)
  p <- attr(x, "provenance");r <- p$native_returns[[1L]]
  expect_false(any(c("df", "t") %in% names(r)))
  expect_equal(r$z, 2, tolerance = 1e-14)
  expect_equal(r$p, .04550026389635842, tolerance = 1e-14)
  expect_equal(c(r$lincom_lb, r$lincom_ub), .2 + c(-1, 1) * q95 * .1, tolerance = 1e-13)
  expect_equal(c(r$lb, r$ub), 10 * exp(.2 + c(-1, 1) * q90 * .1), tolerance = 1e-12)
  expect_identical(p$inference[[1L]], input)
  ratio <- input;ratio$estimate <- exp(.2);ratio$effect_scale <- "ratio"
  ratio$se_scale <- "log"
  y <- tabcell("est", contrast = ratio, level = .9, scale = 10)
  rr <- attr(y, "provenance")$native_returns[[1L]]
  expect_equal(rr$lincom_estimate, exp(.2), tolerance = 1e-14)
  expect_equal(rr$se, exp(.2) * .1, tolerance = 1e-14)
  expect_equal(rr$z, 2, tolerance = 1e-13)
  expect_equal(rr$lb, r$lb, tolerance = 1e-12)
  expect_identical(attr(y, "provenance")$inference[[1L]], ratio)
  expect_error(tabcell("est", contrast = ratio, eform = TRUE), class = "tabtools_error_cell_source")
})

test_that("nlcom and undeclared contrasts never invent original lincom scalars", {
  input <- list(estimate = .2, std.error = .1, df = Inf, conf.level = .95,
    effect_scale = "coefficient", native_source = "nlcom")
  x <- tabcell("est", contrast = input)
  p <- attr(x, "provenance")
  expect_identical(p$rows$native_source, "nlcom")
  expect_setequal(names(p$native_returns[[1L]]), c("missing", "estimate", "lb", "ub", "level"))
  expect_equal(p$native_returns[[1L]]$lb, .0040036015459946, tolerance = 1e-13)
  expect_identical(p$inference[[1L]], input)
  input$native_source <- NULL
  plain <- tabcell("est", contrast = input)
  expect_true(is.na(attr(plain, "provenance")$rows$native_source))
  expect_setequal(names(attr(plain, "provenance")$native_returns[[1L]]), names(p$native_returns[[1L]]))
  expect_identical(attr(plain, "provenance")$inference[[1L]], input)
  expect_identical(attr(tabcell("est", b = 2, ll = 1, ul = 3), "provenance")$rows$native_source, "numbers")
  expect_identical(attr(tabcell("est", matrix = matrix(c(2,1,3), 1), row = 1, cols = 1:3),
    "provenance")$rows$native_source, "matrix")
})

test_that("declared native contrast shape, domain and reference misuse refuse", {
  input <- list(estimate = .2, std.error = .1, df = Inf, conf.level = .95,
    effect_scale = "coefficient", native_source = "lincom")
  bad <- list(list(native_source = "model"), list(native_source = c("lincom", "nlcom")),
    list(native_source = matrix("lincom", 1)), list(std.error = NULL),
    list(estimate = matrix(.2, 1)), list(df = matrix(10, 1)),
    list(conf.level = matrix(.95, 1)), list(estimate = Inf), list(std.error = Inf),
    list(statistic = matrix(2, 1)), list(p.value = 1.01), list(p.value = .7),
    list(statistic = 3), list(conf.low = .3, conf.high = .1),
    list(conf.low = 0, conf.high = 10),
    list(native_source = "nlcom", df = 10),
    list(native_source = "nlcom", effect_scale = "ratio", se_scale = "log"),
    list(native_source = "nlcom", conf.low = 0, conf.high = 10))
  for (change in bad) {
    x <- utils::modifyList(input, change, keep.null = TRUE)
    expect_error(tabcell("est", contrast = x, level = .9), class = "tabtools_error_cell_source")
  }
})

test_that("contrast occurrence selection retains provenance and leaf masks stay redacted", {
  input <- list(estimate = .2, std.error = .1, df = Inf, conf.level = .95,
    effect_scale = "coefficient", native_source = "lincom")
  x <- tabcell("est", contrast = input)
  selected <- x[c(1L,1L)]
  p <- attr(selected, "provenance")
  expect_identical(p$native_returns, rep(attr(x, "provenance")$native_returns, 2L))
  expect_identical(p$inference, list(input,input))
  expect_identical(p$rows$native_source, rep("lincom", 2L))
  masked <- tabcell("np", n = 3, d = 20, ci = "exact", mincell = 5)
  protected <- attr(masked, "provenance")
  expect_identical(protected$rows$status, "masked")
  expect_true(all(is.na(protected$publication)))
  expect_true(all(is.na(unlist(protected$native_returns[[1L]][c("pct","lb","ub")]))))
})

test_that("declared nlcom zero SE withholds limits and requires missing text", {
  # Native tabcell.ado 253-256 maps nonpositive nlcom variance to missing
  # SE and limits. The finite estimate remains an immediate native return.
  input <- list(estimate = 2, std.error = 0, df = Inf, conf.level = .95,
    effect_scale = "coefficient", native_source = "nlcom")
  before <- input
  expect_error(tabcell("est", contrast = input), class = "tabtools_error_cell_missing")
  x <- tabcell("est", contrast = input, missing = "Unavailable")
  p <- attr(x, "provenance")
  expect_identical(as.character(x), "Unavailable")
  expect_identical(p$rows$status, "empty")
  expect_identical(p$rows$native_source, "nlcom")
  expect_true(all(is.na(p$publication)))
  expect_identical(p$native_returns[[1L]],
    list(missing = 1L, estimate = 2, lb = NA_real_, ub = NA_real_, level = 95))
  expect_identical(p$inference[[1L]], before)
  expect_identical(input, before)
  expect_identical(as.character(tabcell("est", contrast = input, missing = "")), "")
  y <- tabcell("est", contrast = input, level = .9, eform = TRUE,
    scale = 10, missing = "Unavailable")
  r <- attr(y, "provenance")$native_returns[[1L]]
  expect_equal(r$estimate, 10 * exp(2), tolerance = 1e-13)
  expect_identical(r[c("missing", "lb", "ub", "level", "scale")],
    list(missing = 1L, lb = NA_real_, ub = NA_real_, level = 90, scale = 10))
  expect_identical(attr(y, "provenance")$inference[[1L]], before)
  with_limits <- input;with_limits$conf.low <- with_limits$conf.high <- NA_real_
  expect_identical(as.character(tabcell("est", contrast = with_limits, missing = "Unavailable")),
    "Unavailable")
  with_limits$conf.low <- with_limits$conf.high <- 2
  expect_error(tabcell("est", contrast = with_limits, missing = "Unavailable"),
    class = "tabtools_error_cell_source")
  # The declared subtype owns the boundary; generic and lincom inputs retain
  # their existing zero-SE interval semantics.
  input$native_source <- "lincom"
  expect_identical(as.character(tabcell("est", contrast = input)), "2.00 (2.00, 2.00)")
  input$native_source <- NULL
  expect_identical(as.character(tabcell("est", contrast = input)), "2.00 (2.00, 2.00)")
})
