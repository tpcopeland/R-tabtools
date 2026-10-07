# Explicit declarations for these known coefficient fixtures; no production
# scale or inference guessing is exercised by this helper.
h1_declared <- function(x, scale, level = .95) {
  attr(x, "effect_scale") <- scale
  attr(x, "conf.level") <- level
  attr(x, "inference_reference") <- "normal"
  if ("reference_row" %in% names(x)) {
    x$status <- ifelse(x$reference_row %in% TRUE, "ref", ifelse(is.na(x$estimate), "omit", "est"))
  }
  x
}

# Milestone H group t2a: regtab hardening (tasks H1, H9, H11, H18). Each
# block names its task and the external-review finding it closes
# (POTENTIAL_ISSUES_2026-09-26.md).

# ---------------------------------------------------------------------------
# H9 (F10): as_forest_data() on a table with no forest-eligible row

forest_cols <- c("label", "estimate", "ll", "ul", "pvalue", "model", "model_label", "rowtype",
                 "section", "source_row")
forest_types <- c(label = "character", estimate = "numeric", ll = "numeric", ul = "numeric",
                  pvalue = "numeric", model = "integer", model_label = "character",
                  rowtype = "character", section = "character", source_row = "integer")

expect_forest_shape <- function(f, n) {
  expect_s3_class(f, "data.frame")
  expect_identical(names(f), forest_cols)
  expect_identical(nrow(f), as.integer(n))
  expect_identical(vapply(f, function(v) class(v)[1], ""), forest_types)
  for (a in c("source", "ci_level", "n_models", "statistic_ids", "model_id", "outcome_id",
              "effect_scale", "model_label")) {
    expect_true(a %in% names(attributes(f)), label = a)
  }
  expect_identical(attr(f, "source"), "regtab")
  expect_identical(attr(f, "statistic_ids"), "estimate ci pvalue")
}

test_that("H9: an all-omitted table gives a zero-row forest frame with every column and attribute", {
  t <- regtab(h1_declared(data.frame(term = "x", estimate = NA_real_), "Coef."))
  expect_identical(t$body[[2]], "Omitted")
  f <- as_forest_data(t)
  expect_forest_shape(f, 0L)
  expect_identical(attr(f, "ci_level"), 95)
  expect_identical(attr(f, "n_models"), 1L)
  expect_identical(attr(f, "effect_scale"), "Coef.")
  # Same columns and types as a table with rows.
  g <- as_forest_data(regtab(glm(am ~ wt, binomial, mtcars)))
  expect_forest_shape(g, 1L)
  expect_identical(lapply(f, class), lapply(g, class))
  expect_identical(setdiff(names(attributes(g)), "row.names"), setdiff(names(attributes(f)), "row.names"))
  # rbind() with a non-empty frame works, as a forest-plot pipeline does.
  expect_identical(nrow(rbind(f, g)), 1L)
})

test_that("H9: keep/drop leaving no eligible row, and two models, give zero rows", {
  d <- data.frame(term = c("x", "y"), estimate = c(NA, 0.5), std.error = c(NA, 0.1))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  t <- regtab(d, drop = "y")
  expect_identical(t$body[[1]], "x")
  expect_forest_shape(as_forest_data(t), 0L)
  t2 <- regtab(d, d, keep = "x")
  f2 <- as_forest_data(t2)
  expect_forest_shape(f2, 0L)
  expect_identical(attr(f2, "n_models"), 2L)
  expect_identical(attr(f2, "model_label"), c("Model 1", "Model 2"))
  # The kept row still renders; only the forest frame is empty.
  expect_identical(regtab(d, keep = "y")$body[[2]], "0.50")
  expect_forest_shape(as_forest_data(regtab(d, keep = "y")), 1L)
})

# ---------------------------------------------------------------------------
# H18 (user item (d), task 5.16): gate messages

# mice is not a test dependency: its two classes are mocked with the
# structure mice 3.19 gives them (`with(imp, lm(...))` is a list of call,
# call1, nmis and analyses with class c("mira", "matrix"); `pool()` returns
# a list with class c("mipo", "data.frame")).
mock_mira <- function(fits) {
  structure(list(call = quote(with(imp, lm(mpg ~ wt))), call1 = quote(mice(d)), nmis = c(mpg = 0L),
                 analyses = fits), class = c("mira", "matrix"))
}

test_that("H18/5.14: regtab(mira) pools the imputations (never 'does not support <call>')", {
  f <- lm(mpg ~ wt, mtcars)
  g <- lm(mpg ~ wt, mtcars[c(2:32, 1), ])
  mira <- mock_mira(list(f, g))
  tt <- regtab(mira, stats = c("n", "mi_m"))
  expect_identical(tt$body, regtab(tt_mi(list(f, g)), stats = c("n", "mi_m"))$body)
  expect_identical(tail(tt$body[[2]], 1), "2")
  # Beside another model, and inside a list: model 2 is the pooled model.
  expect_identical(regtab(f, mira)$stored$N_models, 2L)
  expect_identical(regtab(list(f, mira))$stored$N_models, 2L)
  # structure(class = "mira") alone (older mice) is recognised too.
  expect_identical(regtab(structure(unclass(mira), class = "mira"))$body, regtab(mira)$body)
  # The imputations side by side: pass fit$analyses as a list.
  tt <- regtab(mira$analyses)
  expect_identical(tt$body[[2]], tt$body[[5]])
})

test_that("H18: regtab(mipo) is refused before the data-frame escape hatch reads it", {
  mipo <- structure(list(m = 5L, pooled = data.frame(term = "wt", estimate = -5.3, ubar = 0.3, b = 0.01)),
                    class = c("mipo", "data.frame"))
  err <- tryCatch(regtab(mipo), error = function(e) conditionMessage(e))
  expect_match(err, "does not take a pooled mice result (<mipo>, model 1)", fixed = TRUE)
  expect_match(err, "Pass the <mira> instead", fixed = TRUE)
  expect_false(grepl("needs the columns", err, fixed = TRUE))
  expect_error(regtab(lm(mpg ~ wt, mtcars), mipo), "<mipo>, model 2", fixed = TRUE)
})

test_that("H18: a list with a non-model element names the element", {
  f <- lm(mpg ~ wt, mtcars)
  expect_error(regtab(list(f, "a")), "Element 2 of the list passed to `regtab()` (class <character>) is not a fitted model",
               fixed = TRUE)
  expect_error(regtab(list(f, NULL)), "Element 2 of the list passed to `regtab()` (class <NULL>)", fixed = TRUE)
  expect_error(regtab(list(a = f, b = 1:3)), "Element 2 .* <integer>")
  expect_error(regtab(list(f, list(f))), "not nested lists", fixed = TRUE)
  expect_error(regtab(list(f, mean)), "Element 2 .* <function>")
  # Outside a list: "Model k".
  expect_error(regtab(f, 3), "Model 2 (class <numeric>) is not a fitted model", fixed = TRUE)
  expect_error(regtab(f, list(f)), "unpacks a list only when it is the only argument", fixed = TRUE)
  # Classed objects still reach the class gate, which names the class.
  expect_error(regtab(list(f, structure(list(), class = "foo"))), "does not support <foo> models (model 2)",
               fixed = TRUE)
})

test_that("H18: coxme refusal names the missing Stata equivalent", {
  coxme <- structure(list(coefficients = c(age = 0.1), call = str2lang("coxme::coxme()")), class = "coxme")
  err <- tryCatch(regtab(coxme), error = function(e) conditionMessage(e))
  expect_match(err, "Mixed-effects Cox models have no Stata equivalent", fixed = TRUE)
  expect_match(err, "`mestreg` is parametric", fixed = TRUE)
  expect_match(err, "`stcox, shared()` is gamma frailty", fixed = TRUE)
  expect_false(grepl("(Stata `mestreg`)", err, fixed = TRUE))
})

test_that("H18: an lmerTest fit is cast to lmerMod with a note", {
  skip_if_not_installed("lme4")
  # lmerTest is not a test dependency: an S4 subclass of lmerMod with an
  # extra slot stands in for its lmerModLmerTest class.
  env <- new.env(parent = asNamespace("lme4"))
  methods::setClass("lmerModLmerTest", contains = "lmerMod", slots = c(vcov_beta = "matrix"), where = env)
  on.exit(methods::removeClass("lmerModLmerTest", where = env), add = TRUE)
  d <- lme4::sleepstudy
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), d, REML = FALSE)
  lt <- methods::new("lmerModLmerTest", m, vcov_beta = matrix(0))
  expect_true(inherits(lt, "lmerModLmerTest"))
  expect_message(tt <- regtab(lt, stats = "n groups"), "shown as `as(fit, \"lmerMod\")`", fixed = TRUE)
  expect_message(regtab(lt), "not lmerTest's Satterthwaite t tests", fixed = TRUE)
  ref <- regtab(m, stats = "n groups")
  expect_identical(tt$body, ref$body)
  expect_identical(tt$stored$table, ref$stored$table)
})

# ---------------------------------------------------------------------------
# H1 (F01, F02, F32, user items (b), (c)): the data-frame escape hatch

h1_cells <- function(tt, row = 1L, m = 1L) {
  r <- tt$meta$regtab_rows
  r <- r[r$model == m & r$status %in% "est", , drop = FALSE]
  r[row, c("estimate", "conf.low", "conf.high", "p.value")]
}

test_that("H1 (F01): a ratio at exactly its null has p = 1, and dimnonsig agrees, on both SE scales", {
  for (sc in c("link", "estimate")) {
    d <- data.frame(term = "x", estimate = 1, std.error = 0.1)
    attr(d, "effect_scale") <- "Coef."
    attr(d, "conf.level") <- .95
    attr(d, "inference_reference") <- "normal"
    attr(d, "effect_scale") <- "OR"
    attr(d, "se_scale") <- sc
    tt <- regtab(d, dimnonsig = TRUE)
    expect_identical(tt$body[[4]], "1.00", label = sc)
    expect_true(tt$rows$dim, label = sc)
    expect_identical(h1_cells(tt)$p.value, 1, label = sc)
  }
  # The same through stata_cmd (null 1 from the command word), and an HR
  # just off its null: p and the interval tell the same story.
  d <- data.frame(term = "x", estimate = 1.1, std.error = 0.1)
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "stata_cmd") <- "stcox"
  attr(d, "effect_scale") <- "HR"
  attr(d, "inference_reference") <- "normal"
  attr(d, "se_scale") <- "estimate"
  tt <- regtab(d, dimnonsig = TRUE)
  expect_identical(tt$body[[3]], "(0.90, 1.30)")
  expect_identical(tt$body[[4]], "0.32")
  expect_true(tt$rows$dim)
})

test_that("H1 (H-D5): the two declared SE scales give their own Wald statistics", {
  d <- data.frame(term = c("a", "b"), estimate = c(2, 0.5), std.error = c(0.3, 0.2))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "effect_scale") <- "IRR"
  q <- qnorm(0.975)
  attr(d, "se_scale") <- "link"
  got <- h1_cells(regtab(d), 1:2)
  expect_equal(got$conf.low, exp(log(c(2, 0.5)) - q * c(0.3, 0.2)), tolerance = 1e-12)
  expect_equal(got$conf.high, exp(log(c(2, 0.5)) + q * c(0.3, 0.2)), tolerance = 1e-12)
  expect_equal(got$p.value, 2 * pnorm(-abs(log(c(2, 0.5)) / c(0.3, 0.2))), tolerance = 1e-12)
  attr(d, "se_scale") <- "estimate"
  got <- h1_cells(regtab(d, level = 90), 1:2)
  q90 <- qnorm(0.95)
  expect_equal(got$conf.low, c(2, 0.5) - q90 * c(0.3, 0.2), tolerance = 1e-12)
  expect_equal(got$conf.high, c(2, 0.5) + q90 * c(0.3, 0.2), tolerance = 1e-12)
  expect_equal(got$p.value, 2 * pnorm(-abs((c(2, 0.5) - 1) / c(0.3, 0.2))), tolerance = 1e-12)
  # On the Coef. scale the SE is on the estimate's scale (null 0). A
  # declared se_scale there contradicts the estimates and is refused, never
  # ignored (review P0-1 of group t2a).
  e <- data.frame(term = "a", estimate = 2, std.error = 0.5)
  attr(e, "effect_scale") <- "Coef."
  attr(e, "conf.level") <- .95
  attr(e, "inference_reference") <- "normal"
  base <- h1_cells(regtab(e))
  expect_equal(base$p.value, 2 * pnorm(-4), tolerance = 1e-12)
  attr(e, "se_scale") <- "link"
  expect_error(regtab(e), "declares `se_scale = \"link\"`, but none of its estimates is on a ratio scale", fixed = TRUE)
})

test_that("H1 (H-D5): a ratio row needing a derived statistic is refused without se_scale, naming the rows", {
  d <- data.frame(term = c("a", "b", "c"), estimate = c(2, 0.5, 1.2), std.error = c(0.3, 0.2, 0.1),
                  conf.low = c(1.1, NA, NA), conf.high = c(3.6, NA, NA), p.value = c(0.03, NA, NA))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "stata_cmd") <- "logit"
  attr(d, "effect_scale") <- "OR"
  attr(d, "inference_reference") <- "normal"
  err <- tryCatch(regtab(d), error = function(e) conditionMessage(e))
  expect_match(err, "ratio scale (OR)", fixed = TRUE)
  expect_match(err, "\"b\" and \"c\"", fixed = TRUE)
  expect_false(grepl("\"a\"", err, fixed = TRUE))
  expect_match(err, "se_scale", fixed = TRUE)
  # Fully supplied rows need nothing: no refusal, numbers kept.
  full <- data.frame(term = "a", estimate = 2, std.error = 0.3, conf.low = 1.1, conf.high = 3.6, p.value = 0.02)
  attr(full, "effect_scale") <- "Coef."
  attr(full, "conf.level") <- .95
  attr(full, "inference_reference") <- "normal"
  attr(full, "stata_cmd") <- "logit"
  attr(full, "effect_scale") <- "OR"
  attr(full, "inference_reference") <- "normal"
  expect_identical(regtab(full)$body[[3]], "(1.10, 3.60)")
  # Ancillary rows of a ratio model are on their own scale: no refusal.
  anc <- data.frame(term = c("x", "/lnalpha"), estimate = c(1.5, -0.7), std.error = c(NA, 0.2),
                    conf.low = c(1.2, NA), conf.high = c(1.9, NA), p.value = c(0.01, NA))
  attr(anc, "effect_scale") <- "Coef."
  attr(anc, "conf.level") <- .95
  attr(anc, "inference_reference") <- "normal"
  anc$p.value <- NULL
  attr(anc, "stata_cmd") <- "nbreg"
  attr(anc, "effect_scale") <- "IRR"
  attr(anc, "inference_reference") <- "normal"
  tt <- regtab(anc, keepintercept = TRUE)
  expect_identical(tt$body[[1]], c("x", "/lnalpha"))
  expect_identical(tt$body[[3]][2], sprintf("(%.2f, %.2f)", -0.7 - qnorm(0.975) * 0.2, -0.7 + qnorm(0.975) * 0.2))
  # A ratio estimate must be positive (raw coefficients under an OR header).
  neg <- data.frame(term = "a", estimate = -0.4, conf.low = -0.9, conf.high = 0.1, p.value = 0.1)
  attr(neg, "effect_scale") <- "Coef."
  attr(neg, "conf.level") <- .95
  attr(neg, "inference_reference") <- "normal"
  attr(neg, "stata_cmd") <- "logit"
  attr(neg, "effect_scale") <- "OR"
  attr(neg, "inference_reference") <- "normal"
  expect_error(regtab(neg), "OR estimates that are not positive", fixed = TRUE)
})

test_that("H1 (F01): tidy_plus_plus(exponentiate = TRUE) without CI/p columns needs se_scale = \"link\", then matches the fit", {
  skip_if_not_installed("broom.helpers")
  set.seed(11)
  d <- data.frame(y = rbinom(300, 1, 0.4), wt = rnorm(300, 3, 0.8),
                  treat = factor(sample(c("Control", "Placebo", "Drug"), 300, TRUE),
                                 levels = c("Control", "Placebo", "Drug")))
  fit <- glm(y ~ wt + treat, binomial, d)
  # Wald intervals from vcov() (broom's default for glm would be profile).
  tf <- function(x, exponentiate = FALSE, conf.level = 0.95, ...) {
    w <- tabtools:::tt_wald(x, conf.level)
    if (exponentiate) for (k in c("estimate", "conf.low", "conf.high")) w[[k]] <- exp(w[[k]])
    w
  }
  tp <- broom.helpers::tidy_plus_plus(fit, tidy_fun = tf, exponentiate = TRUE)
  tp <- h1_declared(tp, "OR", level = .95)
  tp <- as.data.frame(tp)[, setdiff(names(tp), c("conf.low", "conf.high", "p.value", "statistic"))]
  attr(tp, "stata_cmd") <- "logit"
  attr(tp, "effect_scale") <- "OR"
  attr(tp, "inference_reference") <- "normal"
  expect_error(regtab(tp), "se_scale", fixed = TRUE)
  attr(tp, "se_scale") <- "link"
  a <- regtab(fit)
  b <- regtab(tp)
  expect_identical(b$body, a$body)
  fa <- as_forest_data(a)
  fb <- as_forest_data(b)
  expect_equal(fb[, c("estimate", "ll", "ul", "pvalue")], fa[, c("estimate", "ll", "ul", "pvalue")], tolerance = 1e-10)
  # The F01 symptom (a negative odds-ratio bound, p > 0.99) cannot come
  # back: "estimate" would be the user's explicit (and here wrong) claim.
  attr(tp, "se_scale") <- "estimate"
  expect_false(identical(regtab(tp)$body, a$body))
})

test_that("H1 (F02): the malformed-schema matrix is refused, each with a message naming the problem", {
  ok <- data.frame(term = c("a", "b"), estimate = c(0.5, 1), std.error = c(0.1, 0.2),
                   conf.low = c(0.3, 0.6), conf.high = c(0.7, 1.4), p.value = c(0.01, 0.2))
  attr(ok, "effect_scale") <- "Coef."
  attr(ok, "conf.level") <- .95
  attr(ok, "inference_reference") <- "normal"
  expect_s3_class(regtab(ok), "tt_table")
  mod <- function(...) {
    x <- ok
    args <- list(...)
    for (nm in names(args)) x[[nm]] <- args[[nm]]
    x
  }
  cases <- list(
    list(mod(conf.low = factor(c("0.3", "0.6"))), "conf.low that is a factor"),
    list(mod(conf.high = factor(c("0.7", "1.4"))), "conf.high that is a factor"),
    list(mod(p.value = factor(c("0.01", "0.2"))), "p.value that is a factor"),
    list(mod(std.error = factor(c("0.1", "0.2"))), "std.error that is a factor"),
    list(mod(conf.low = c("0.3", "0.6")), "conf.low that is of class <character>"),
    list(mod(p.value = c("0.01", "0.2")), "p.value that is of class <character>"),
    list(mod(estimate = factor(c("0.5", "1"))), "estimate must be numeric"),
    list(mod(term = c("a", NA)), "missing or empty term in row 2"),
    list(mod(term = c("a", " ")), "missing or empty term in row 2"),
    list(mod(term = c("a", "a")), "repeats a coefficient"),
    list(mod(std.error = c(-0.1, 0.2)), "std.error that is not positive (\"a\")"),
    list(mod(std.error = c(0, 0.2)), "std.error that is not positive (\"a\")"),
    list(mod(conf.low = c(0.8, 0.6)), "conf.low above conf.high for \"a\""),
    list(mod(p.value = c(2, 0.2)), "p.value outside [0, 1] for \"a\""),
    list(mod(p.value = c(-0.01, 0.2)), "p.value outside [0, 1] for \"a\""),
    list(mod(conf.low = c(NA, 0.6)), "only one confidence bound for \"a\""),
    list(mod(estimate = c(NaN, 1)), "non-finite estimate"),
    list(mod(std.error = c(Inf, 0.2)), "non-finite std.error"),
    list(mod(p.value = c(NaN, 0.2)), "non-finite p.value"),
    list(mod(df = c(0, 3)), "df that is not positive"),
    list(mod(df = c(3, 3), df.error = c(3, 3)), "both df and df.error"),
    list(mod(var_type = c("continuous", "ran_pars")), "unknown var_type \"ran_pars\""),
    list(mod(reference_row = c("no", "no")), "reference_row that is not logical"),
    list(mod(label = c(1, 2)), "column label that is not character"),
    list(mod(key = c("a", "")), "empty key")
  )
  for (cs in cases) {
    err <- tryCatch(regtab(cs[[1]]), error = function(e) conditionMessage(e))
    expect_match(gsub("\n", " ", err), cs[[2]], fixed = TRUE, label = cs[[2]])
  }
  # Sum contrasts have no reference level (a Reference row would lie).
  sc <- data.frame(term = c("f1", "f2", "f3"), variable = "f", var_type = "categorical", label = c("a", "b", "c"),
                   reference_row = c(FALSE, FALSE, TRUE), contrasts_type = "sum", estimate = c(-0.2, 0.2, 0.04),
                   std.error = 0.1)
  attr(sc, "effect_scale") <- "Coef."
  attr(sc, "conf.level") <- .95
  attr(sc, "inference_reference") <- "normal"
  expect_error(regtab(sc), "\"sum\" contrasts", fixed = TRUE)
  bad_attr <- ok
  attr(bad_attr, "se_scale") <- "log"
  expect_error(regtab(bad_attr), "se_scale", fixed = TRUE)
  attr(bad_attr, "se_scale") <- NULL
  attr(bad_attr, "conf.level") <- 150
  expect_error(regtab(bad_attr), "conf.level", fixed = TRUE)
  two <- mod(conf.level = c(0.9, 0.95))
  expect_error(regtab(two), "several levels", fixed = TRUE)
  # Model number in the message beside a fitted model; tibbles too.
  expect_error(regtab(lm(mpg ~ wt, mtcars), mod(p.value = c(2, 0.2))), "Model 2 (a data frame)", fixed = TRUE)
  tb <- structure(mod(conf.low = factor(c("0.3", "0.6"))), class = c("tbl_df", "tbl", "data.frame"))
  expect_error(regtab(tb), "factor", fixed = TRUE)
  # Well-formed edge values pass: character-free NA columns, Inf df, an
  # infinite exp() bound (blank, as for a fitted model), header rows with
  # a missing term, a factor term column.
  edge <- data.frame(term = factor(c("a", "b")), estimate = c(0.5, 1), std.error = c(0.1, NA),
                     conf.low = c(0.3, NA), conf.high = c(Inf, NA), p.value = NA, df = c(Inf, NA))
  attr(edge, "effect_scale") <- "Coef."
  attr(edge, "conf.level") <- .95
  attr(edge, "inference_reference") <- "normal"
  tt <- regtab(edge)
  expect_identical(tt$body[[3]], c("", ""))
  expect_identical(tt$body[[4]], c("", ""))
  hdr <- data.frame(term = c(NA, "fa", "fb"), variable = "f", var_type = c(NA, "categorical", "categorical"),
                    label = c("f", "a", "b"), header_row = c(TRUE, FALSE, FALSE), reference_row = c(NA, TRUE, FALSE),
                    estimate = c(NA, 0, 0.4), std.error = c(NA, NA, 0.2))
  attr(hdr, "effect_scale") <- "Coef."
  attr(hdr, "conf.level") <- .95
  attr(hdr, "inference_reference") <- "normal"
  expect_identical(regtab(hdr)$body[[1]], c("f", "  a", "  b"))
})

test_that("H1: supplied p-values and intervals are never touched", {
  d <- data.frame(term = c("a", "b"), estimate = c(2, 0.8), std.error = c(0.3, 0.5),
                  conf.low = c(1.2, 0.2), conf.high = c(3.1, 1.9), p.value = c(0.004, 0.61), df = c(4, 4))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  for (sc in list(NULL, "link", "estimate")) {
    x <- d
    attr(x, "stata_cmd") <- "poisson"
    attr(x, "effect_scale") <- "IRR"
    attr(x, "inference_reference") <- "normal"
    attr(x, "se_scale") <- sc
    got <- h1_cells(regtab(x), 1:2)
    expect_identical(got$conf.low, c(1.2, 0.2))
    expect_identical(got$conf.high, c(3.1, 1.9))
    expect_identical(got$p.value, c(0.004, 0.61))
  }
  # A supplied p.value column is kept even where it is missing; a row with
  # no bounds gets a derived interval.
  m <- data.frame(term = c("a", "b"), estimate = c(2, 0.8), std.error = c(0.3, 0.5), p.value = c(0.01, NA),
                  conf.low = c(1.5, NA), conf.high = c(2.5, NA))
  attr(m, "effect_scale") <- "Coef."
  attr(m, "conf.level") <- .95
  attr(m, "inference_reference") <- "normal"
  got <- h1_cells(regtab(m), 1:2)
  expect_identical(got$p.value, c(0.01, NA))
  expect_identical(got$conf.low[1], 1.5)
  expect_equal(got$conf.low[2], 0.8 - qnorm(0.975) * 0.5, tolerance = 1e-12)
})

test_that("H1 (user item (c)): a df or df.error column gives t intervals and p-values", {
  d <- data.frame(term = "x", estimate = 2, std.error = 1, df = 3)
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  tt <- regtab(d)
  expect_identical(tt$body[[3]], "(-1.18, 5.18)")
  expect_identical(tt$body[[4]], "0.14")
  got <- h1_cells(tt)
  expect_equal(got$conf.low, 2 - qt(0.975, 3), tolerance = 1e-12)
  expect_equal(got$conf.high, 2 + qt(0.975, 3), tolerance = 1e-12)
  expect_equal(got$p.value, 2 * pt(2, 3, lower.tail = FALSE), tolerance = 1e-12)
  names(d)[names(d) == "df"] <- "df.error"
  expect_identical(h1_cells(regtab(d)), got)
  # Per-row df; NA or Inf is the normal distribution; level honoured.
  e <- data.frame(term = c("a", "b", "c"), estimate = c(1, 1, 1), std.error = c(0.4, 0.4, 0.4), df = c(10, NA, Inf))
  attr(e, "effect_scale") <- "Coef."
  attr(e, "conf.level") <- .95
  attr(e, "inference_reference") <- "normal"
  got <- h1_cells(regtab(e, level = 0.9), 1:3)
  expect_equal(got$conf.high, 1 + 0.4 * c(qt(0.95, 10), qnorm(0.95), qnorm(0.95)), tolerance = 1e-12)
  expect_equal(got$p.value, 2 * c(pt(-2.5, 10), pnorm(-2.5), pnorm(-2.5)), tolerance = 1e-12)
  # With a ratio and se_scale = "link": t on the log scale.
  r <- data.frame(term = "x", estimate = 1.5, std.error = 0.2, df = 12)
  attr(r, "effect_scale") <- "Coef."
  attr(r, "conf.level") <- .95
  attr(r, "inference_reference") <- "normal"
  attr(r, "effect_scale") <- "HR"
  attr(r, "se_scale") <- "link"
  got <- h1_cells(regtab(r))
  expect_equal(got$conf.low, exp(log(1.5) - qt(0.975, 12) * 0.2), tolerance = 1e-12)
  expect_equal(got$p.value, 2 * pt(-abs(log(1.5) / 0.2), 12), tolerance = 1e-12)
  # lm()'s own table is reproduced from tidy columns with df.error.
  f <- lm(mpg ~ wt + hp, mtcars)
  s <- summary(f)$coefficients
  td <- data.frame(term = rownames(s), estimate = s[, 1], std.error = s[, 2], df.error = f$df.residual)
  attr(td, "effect_scale") <- "Coef."
  attr(td, "conf.level") <- .95
  attr(td, "inference_reference") <- "normal"
  expect_identical(regtab(td)$body, regtab(f)$body)
})

test_that("H1 (F32): a level that conflicts with supplied intervals is refused; vce must be \"stata\"", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3, p.value = 0.04)
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  err <- tryCatch(regtab(d, level = 0.90), error = function(e) conditionMessage(e))
  expect_match(err, "supplies 95% confidence intervals", fixed = TRUE)
  expect_match(err, "asks for 90%", fixed = TRUE)
  expect_match(err, "regtab does not recompute supplied intervals", fixed = TRUE)
  expect_error(regtab(d, level = 90), "asks for 90%", fixed = TRUE)
  expect_identical(regtab(d)$stored$ci_level, 95)
  # Declared 90% intervals: level = 0.9 (or 90) is right; left at its
  # default, level follows the declaration (review P2-2); an explicit 95% is
  # refused.
  attr(d, "conf.level") <- 0.9
  expect_identical(regtab(d, level = 0.9)$stored$ci_level, 90)
  expect_identical(regtab(d, level = 90)$header[[2]]$text[3], "90% CI")
  expect_identical(regtab(d)$stored$ci_level, 90)
  expect_error(regtab(d, level = 0.95), "supplies 90% confidence intervals", fixed = TRUE)
  # Two frames declaring different levels: refused.
  d95 <- d
  attr(d95, "conf.level") <- 0.95
  expect_error(regtab(d, d95), "different confidence levels", fixed = TRUE)
  attr(d, "conf.level") <- NULL
  d$conf.level <- 90
  expect_identical(regtab(d, level = 0.9)$stored$ci_level, 90)
  d$conf.level <- NULL
  expect_error(regtab(d), class = "tabtools_error_regtab_metadata")
  attr(d, "conf.level") <- .95
  # Only derived intervals: any level (they are computed at it).
  e <- data.frame(term = "x", estimate = 2, std.error = 0.5)
  attr(e, "effect_scale") <- "Coef."
  attr(e, "conf.level") <- .95
  attr(e, "inference_reference") <- "normal"
  expect_identical(regtab(e, level = 0.9)$body[[3]], sprintf("(%.2f, %.2f)", 2 - qnorm(0.95) * 0.5, 2 + qnorm(0.95) * 0.5))
  # The conflict is named per model beside a fitted model.
  expect_error(regtab(lm(mpg ~ wt, mtcars), d, level = 0.9), "Model 2 (a data frame) supplies 95%", fixed = TRUE)
  # vce: a data frame has one variance, its own.
  expect_identical(tabtools::tt_vce_types(d), "stata")
  expect_error(regtab(d, vce = "model"), "not available for <data.frame>", fixed = TRUE)
  expect_error(regtab(d, vce = "robust"), "not available for <data.frame>", fixed = TRUE)
  f <- lm(mpg ~ wt, mtcars)
  expect_s3_class(regtab(f, d, vce = list("model", "stata")), "tt_table")
})

test_that("H1 (user item (b)): regtab(fit, tidy_plus_plus(fit)) shares the factor's level rows", {
  skip_if_not_installed("broom.helpers")
  set.seed(3)
  d <- data.frame(y = rbinom(250, 1, 0.4), age = rnorm(250, 50, 10),
                  treat = factor(sample(c("Control", "Placebo", "Drug"), 250, TRUE),
                                 levels = c("Control", "Placebo", "Drug")),
                  g = factor(sample(c("0", "1", "2"), 250, TRUE)))
  fit <- glm(y ~ treat + age + g, binomial, d)
  tp <- broom.helpers::tidy_plus_plus(fit, exponentiate = TRUE)
  tp <- h1_declared(tp, "OR", level = .95)
  attr(tp, "stata_cmd") <- "logit"
  attr(tp, "effect_scale") <- "OR"
  attr(tp, "inference_reference") <- "normal"
  tt <- regtab(fit, tp)
  r <- tt$meta$regtab_rows
  keys <- unique(r$key)
  expect_identical(keys, c("treat", "1.treat", "2.treat", "3.treat", "age", "g", "0.g", "1.g", "2.g"))
  expect_identical(tt$body[[1]], c("treat", "  Control", "  Placebo", "  Drug", "age", "g", "  0", "  1", "  2"))
  # Both models fill the 2.treat row: no second Placebo/Drug block.
  two <- r[r$key == "2.treat", ]
  expect_identical(two$status, c("est", "est"))
  expect_equal(two$estimate[2], unname(exp(coef(fit)["treatPlacebo"])), tolerance = 1e-10)
  expect_identical(sum(tt$body[[1]] == "  Placebo"), 1L)
  # Stata keys select data-frame rows too; R terms still work.
  # (As for the fitted model: the header row is not selected by a level.)
  expect_identical(suppressMessages(regtab(tp, keep = "2.treat"))$body[[1]], "  Placebo")
  expect_identical(suppressMessages(regtab(tp, keep = "2.treat"))$body,
                   suppressMessages(regtab(fit, keep = "2.treat"))$body)
  expect_identical(regtab(tp, keep = "treatDrug")$body[[1]], "  Drug")
  expect_identical(regtab(tp, keep = "treat")$body[[1]], c("treat", "  Control", "  Placebo", "  Drug"))
  # Positional codes are flagged, as for the fitted factor.
  expect_message(regtab(tp, keep = "2.treat"), "level positions", fixed = TRUE)
  # tidy_plus_plus() records its conf.level: a 90% frame needs level = 0.9.
  tp90 <- broom.helpers::tidy_plus_plus(fit, exponentiate = TRUE, conf.level = 0.9)
  tp90 <- h1_declared(tp90, "OR", level = .9)
  attr(tp90, "stata_cmd") <- "logit"
  attr(tp90, "effect_scale") <- "OR"
  attr(tp90, "inference_reference") <- "normal"
  expect_error(regtab(fit, tp90, level = 0.95), "Model 2 (a data frame) supplies 90% confidence intervals", fixed = TRUE)
  expect_identical(regtab(fit, tp90, level = 0.9)$stored$ci_level, 90)
  expect_identical(regtab(fit, tp90)$stored$ci_level, 90)
  # Its exponentiate attribute is quoted in the se_scale refusal (a hint,
  # not a guess: the scale must still be declared, H-D5).
  bare <- tp[, c("term", "variable", "var_type", "label", "reference_row", "estimate", "std.error")]
  attr(bare, "stata_cmd") <- "logit"
  attr(bare, "effect_scale") <- "OR"
  attr(bare, "inference_reference") <- "normal"
  attr(bare, "exponentiate") <- TRUE
  expect_error(regtab(bare), "whose std.error stays on the log scale", fixed = TRUE)
  # A logical predictor is 0/1 in both.
  d$old <- d$age > 50
  fl <- glm(y ~ old + age, binomial, d)
  tl <- broom.helpers::tidy_plus_plus(fl, exponentiate = TRUE)
  tl <- h1_declared(tl, "OR", level = .95)
  attr(tl, "stata_cmd") <- "logit"
  attr(tl, "effect_scale") <- "OR"
  attr(tl, "inference_reference") <- "normal"
  rl <- regtab(fl, tl)$meta$regtab_rows
  expect_identical(unique(rl$key), c("old", "0.old", "1.old", "age"))
  expect_identical(rl$status[rl$key == "1.old"], c("est", "est"))
  # Without exponentiate (log-odds, Coef.): the rows are shared all the same.
  tt2 <- regtab(fit, h1_declared(broom.helpers::tidy_plus_plus(fit), "Coef."))
  expect_identical(sum(tt2$body[[1]] == "  Placebo"), 1L)
})

test_that("H1: a key column overrides the derived keys; no reference row keeps the term", {
  d <- data.frame(term = c("armB", "armC", "age"), variable = c("arm", "arm", "age"),
                  var_type = c("categorical", "categorical", "continuous"), label = c("B", "C", "Age"),
                  estimate = c(0.5, 0.7, 0.02), std.error = c(0.2, 0.2, 0.01))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  # No reference row: the base's position is unknown, so the terms stay keys.
  r <- regtab(d)$meta$regtab_rows
  expect_identical(r$key, c("arm", "armB", "armC", "age"))
  d$key <- c("2.arm", "3.arm", NA)
  r <- regtab(d)$meta$regtab_rows
  expect_identical(r$key, c("arm", "2.arm", "3.arm", "age"))
  # A key column joins a fitted model's rows (Stata codes from value labels).
  a <- data.frame(y = c(1.2, 2.3, 3.1, 4.8, 5.2, 6.9, 7.1, 8.4, 9.9, 10.2, 11.5, 12.1),
                  arm = factor(rep(c("A", "B", "C"), 4)), age = c(30, 41, 52, 33, 44, 55, 36, 47, 58, 39, 40, 61))
  f <- lm(y ~ arm + age, a)
  d$label <- c("B", "C", "age")
  d$term <- c("armB", "armC", "age")
  tt <- regtab(f, d)
  expect_identical(sum(tt$body[[1]] == "  B"), 1L)
  # A variable whose labels are integers keys by them, with or without a
  # reference row.
  n <- data.frame(term = c("g1", "g2"), variable = "g", var_type = "categorical", label = c("1", "2"),
                  estimate = c(0.1, 0.2), std.error = 0.1)
  attr(n, "effect_scale") <- "Coef."
  attr(n, "conf.level") <- .95
  attr(n, "inference_reference") <- "normal"
  expect_identical(regtab(n)$meta$regtab_rows$key, c("g", "1.g", "2.g"))
  # Repeated derived keys are refused (two variables in one block).
  dup <- data.frame(term = c("a", "b"), key = c("x", "x"), estimate = 1:2)
  attr(dup, "effect_scale") <- "Coef."
  attr(dup, "conf.level") <- .95
  attr(dup, "inference_reference") <- "normal"
  expect_error(regtab(dup), "repeats a row key", fixed = TRUE)
})

test_that("H1: multi-equation data frames key their levels per equation", {
  d <- data.frame(equation = rep(c("B", "C"), each = 3), term = rep(c("sexF", "sexM", "age"), 2),
                  variable = rep(c("sex", "sex", "age"), 2), label = rep(c("F", "M", "age"), 2),
                  var_type = rep(c("dichotomous", "dichotomous", "continuous"), 2),
                  reference_row = rep(c(TRUE, FALSE, NA), 2), estimate = c(1, 1.4, 1.02, 1, 0.8, 0.99),
                  conf.low = c(NA, 1.1, 1.0, NA, 0.6, 0.97), conf.high = c(NA, 1.8, 1.04, NA, 1.1, 1.01),
                  p.value = c(NA, 0.01, 0.04, NA, 0.2, 0.3))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "stata_cmd") <- "mlogit"
  attr(d, "effect_scale") <- "RRR"
  attr(d, "inference_reference") <- "normal"
  r <- regtab(d)$meta$regtab_rows
  expect_identical(r$key, c("B::1.sex", "B::2.sex", "B::age", "C::1.sex", "C::2.sex", "C::age"))
  expect_identical(regtab(d)$body[[1]], c("B: F", "B: M", "B: age", "C: F", "C: M", "C: age"))
})

# ---------------------------------------------------------------------------
# H11 (F12, H-D3): the methods sentence comes from the models

h11_stata <- function() {
  utils::read.csv(test_path("fixtures", "regtab_methods", "stata_methods.csv"), stringsAsFactors = FALSE)
}

test_that("H11: Stata 2.1.12 builds r(methods) from the models (take_action item 11, fixed)", {
  s <- h11_stata()
  st <- stats::setNames(s$methods, s$id)
  # The verification probes of external review F12 (qa/stata/make_regtab_methods.do,
  # Stata 17, tabtools 2.1.12): the model, not the header, is named; 2.1.11
  # read "linear regression" for the probit, "Poisson regression" for nbreg,
  # "multivariable" for one predictor and "Estimate with ... regression"
  # under coef(Estimate).
  expect_identical(st[["M01"]], "Coefficients with 95% confidence intervals from multivariable probit regression.")
  expect_identical(st[["M02"]], st[["M01"]])  # glm, link(probit)
  expect_identical(st[["M03"]], "Incidence rate ratios with 95% confidence intervals from multivariable negative binomial regression.")
  expect_identical(st[["M05"]], "Odds ratios with 95% confidence intervals from univariable logistic regression.")
  expect_identical(st[["M06"]], st[["M13"]])  # regress, coef(OR)
  expect_identical(st[["M10"]], "Odds ratios with 95% confidence intervals from multivariable logistic regression.")
  expect_identical(st[["M08"]], st[["M07"]])  # cloglog and glm, link(cloglog)
  expect_identical(nrow(s), 25L)
})

test_that("H11: R's sentence for each Stata probe equals Stata 2.1.12's", {
  for (p in c("MASS", "nnet", "pscl", "lme4", "survival", "geepack", "haven")) skip_if_not_installed(p)
  a <- golden_fixture("auto")
  a$foreign <- as.numeric(a$foreign)
  a$cnt <- round(a$price / 1000)
  a$long_trip <- as.numeric(a$mpg > 20)
  a$rep <- factor(a$rep78)
  lung <- survival::lung
  lung$cause <- ifelse(lung$status == 2, 1, ifelse(seq_len(nrow(lung)) %% 3 == 0, 2, 0))
  lung$cause <- factor(lung$cause, 0:2, c("censor", "death", "other"))
  fg <- survival::finegray(survival::Surv(time, cause) ~ ., data = lung[, c("time", "cause", "age", "sex")],
                           etype = "death")
  q <- function(...) suppressWarnings(suppressMessages(regtab(...)))$stored$methods
  r <- list(
    M01 = q(glm(foreign ~ mpg + weight, binomial("probit"), a)),
    M03 = q(MASS::glm.nb(cnt ~ mpg + weight, a)),
    M04 = q(glm(cnt ~ mpg + weight, poisson, a)),
    M05 = q(glm(foreign ~ mpg, binomial, a)),
    M06 = q(lm(price ~ mpg + weight, a), coef = "OR"),
    M07 = q(glm(foreign ~ mpg + weight, binomial("cloglog"), a)),
    M09 = q(glm(price ~ mpg + weight, Gamma("log"), a)),
    M10 = q(glm(foreign ~ mpg + weight, binomial, a), coef = "Estimate"),
    M11 = q(glm(foreign ~ mpg + weight, binomial, a), lm(price ~ mpg + weight, a)),
    M12 = q(glm(foreign ~ mpg, binomial, a), glm(foreign ~ mpg + weight, binomial, a)),
    M13 = q(lm(price ~ mpg + weight, a)),
    M14 = q(MASS::polr(rep ~ mpg + weight, a, Hess = TRUE)),
    M15 = q(nnet::multinom(rep ~ mpg + weight, a, trace = FALSE)),
    # (auto's cnt has no zeros: a zip fit there is degenerate, review P3-7;
    # the sentence does not depend on the data.)
    M16 = q(pscl::zeroinfl(art ~ fem + ment | ment, data = pscl::bioChemists)),
    M17 = q(lme4::lmer(price ~ mpg + weight + (1 | foreign), a, REML = FALSE)),
    M18 = q(lme4::glmer(long_trip ~ weight + (1 | foreign), a, binomial)),
    M19 = q(glm(foreign ~ mpg + weight, binomial, a), cdisc = TRUE),
    M20 = q(glm(foreign ~ mpg + weight, binomial, a), level = 0.9, stars = TRUE),
    M21 = q(survival::coxph(survival::Surv(time, status) ~ age + factor(sex), lung, ties = "breslow")),
    M22 = q(survival::survreg(survival::Surv(time, status) ~ age + factor(sex), lung)),
    M24 = q(survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ age + sex, fg, weights = fgwt)),
    M25 = q(geepack::geeglm(foreign ~ mpg + weight, binomial, a[order(a$rep78), ][!is.na(a$rep78[order(a$rep78)]), ],
                            id = rep78))
  )
  ci <- "with 95% confidence intervals from"
  want <- c(
    M01 = paste("Coefficients", ci, "multivariable probit regression."),
    M03 = paste("Incidence rate ratios", ci, "multivariable negative binomial regression."),
    M04 = paste("Incidence rate ratios", ci, "multivariable Poisson regression."),
    M05 = paste("Odds ratios", ci, "univariable logistic regression."),
    M06 = paste("Coefficients", ci, "multivariable linear regression."),
    M07 = paste("Coefficients", ci, "multivariable complementary log-log regression."),
    M09 = paste("Coefficients", ci, "multivariable gamma regression with a log link."),
    M10 = paste("Odds ratios", ci, "multivariable logistic regression."),
    M11 = "Collected regression estimates with 95% confidence intervals across 2 models.",
    M12 = paste("Odds ratios", ci, "univariable and multivariable logistic regression across 2 models."),
    M13 = paste("Coefficients", ci, "multivariable linear regression."),
    M14 = paste("Odds ratios", ci, "multivariable ordered logistic regression."),
    M15 = paste("Relative risk ratios", ci, "multivariable multinomial logistic regression."),
    M16 = paste("Coefficients", ci, "multivariable zero-inflated Poisson regression."),
    M17 = paste("Coefficients", ci, "multivariable linear mixed-effects regression."),
    M18 = paste("Odds ratios", ci, "univariable mixed-effects logistic regression."),
    M19 = paste("Odds ratios", ci, "multivariable logistic regression."),
    M20 = paste("Odds ratios with 90% confidence intervals from multivariable logistic regression.",
                "Statistical significance denoted as * p<0.05, ** p<0.01, *** p<0.001."),
    M21 = paste("Hazard ratios", ci, "multivariable Cox proportional hazards regression."),
    M22 = paste("Time ratios", ci, "multivariable accelerated failure-time survival regression."),
    M24 = paste("Subhazard ratios", ci, "multivariable Fine-Gray competing-risks regression."),
    M25 = paste("Odds ratios", ci, "multivariable generalized estimating equation (GEE) logistic regression.")
  )
  for (id in names(want)) expect_identical(r[[id]], want[[id]], label = id)
  # R's sentence is Stata's, character for character, for every probe R
  # can fit (M02 and M08 are Stata spellings of M01's and M07's models;
  # M23, streg in the hazard metric, has no survreg analogue).
  st <- stats::setNames(h11_stata()$methods, h11_stata()$id)
  for (id in names(want)) expect_identical(r[[id]], st[[id]], label = id)
  expect_identical(r[["M01"]], st[["M02"]])
  expect_identical(r[["M07"]], st[["M08"]])
})

test_that("H11: a coef/cdisc relabel never renames the model; mixed kinds and scales", {
  f <- glm(am ~ mpg + qsec, binomial, mtcars)
  base <- regtab(f)$stored$methods
  for (h in c("OR", "HR", "Coef.", "Estimate", "Mean difference")) expect_identical(regtab(f, coef = h)$stored$methods, base)
  expect_identical(regtab(f, cdisc = TRUE)$stored$methods, base)
  expect_identical(regtab(lm(mpg ~ wt + hp, mtcars), coef = "OR")$stored$methods,
                   "Coefficients with 95% confidence intervals from multivariable linear regression.")
  # One scale, two kinds of model: both named.
  expect_identical(regtab(glm(vs ~ mpg, binomial("probit"), mtcars), lm(mpg ~ wt + hp, mtcars))$stored$methods,
                   "Coefficients with 95% confidence intervals from univariable and multivariable probit regression and linear regression across 2 models.")
  # Different scales: Stata's collected sentence (accurate), whatever coef says.
  mix <- regtab(f, glm(carb ~ wt, poisson, mtcars), coef = "Estimate")$stored$methods
  expect_identical(mix, "Collected regression estimates with 95% confidence intervals across 2 models.")
  # Predictor counting: variables, not coefficients or terms.
  n <- tabtools:::.rt_n_predictors
  expect_identical(n(lm(mpg ~ poly(wt, 2), mtcars)), 1L)
  expect_identical(n(lm(mpg ~ wt + I(wt^2), mtcars)), 1L)
  expect_identical(n(lm(mpg ~ factor(cyl), mtcars)), 1L)
  expect_identical(n(lm(mpg ~ wt * hp, mtcars)), 2L)
  expect_identical(n(lm(mpg ~ 1, mtcars)), 0L)
  expect_identical(regtab(lm(mpg ~ factor(cyl), mtcars))$stored$methods,
                   "Coefficients with 95% confidence intervals from univariable linear regression.")
  skip_if_not_installed("survival")
  expect_identical(n(survival::coxph(survival::Surv(time, status) ~ age + strata(sex) + cluster(inst), survival::lung)), 1L)
  skip_if_not_installed("lme4")
  expect_identical(n(lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)), 1L)
  # A data frame: its variables; the command word names the model.
  d <- data.frame(term = c("(Intercept)", "x", "gB", "gC"), variable = c(NA, "x", "g", "g"),
                  var_type = c("intercept", "continuous", "categorical", "categorical"),
                  estimate = c(0.1, 1.2, 0.8, 1.1), conf.low = c(0.05, 1, 0.6, 0.9),
                  conf.high = c(0.2, 1.4, 1.1, 1.3), p.value = c(0.01, 0.02, 0.2, 0.3))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "stata_cmd") <- "logit"
  attr(d, "effect_scale") <- "OR"
  attr(d, "inference_reference") <- "normal"
  expect_identical(regtab(d)$stored$methods, "Odds ratios with 95% confidence intervals from multivariable logistic regression.")
  expect_identical(regtab(d[1:2, ])$stored$methods, "Odds ratios with 95% confidence intervals from univariable logistic regression.")
  attr(d, "stata_cmd") <- NULL
  attr(d, "effect_scale") <- "Coef."
  expect_identical(regtab(d)$stored$methods, "Coefficients with 95% confidence intervals from multivariable regression.")
})

# Review of group t2a, P0-3/P1-1: levels are joined by level, as Stata
# joins them by value code (fixtures from qa/stata/make_regtab_union.do,
# Stata 17, tabtools 2.1.11).
union_auto <- function() {
  a <- golden_fixture("auto")
  labs <- lapply(a, attr, "label", exact = TRUE)
  a <- a[!is.na(a$rep78), ]
  for (v in names(a)) attr(a[[v]], "label") <- labs[[v]]
  lab <- labs$rep78
  code <- as.integer(a$rep78)
  a$rep78 <- factor(c("poor", "fair", "avg", "good", "exc")[code], levels = c("poor", "fair", "avg", "good", "exc"))
  attr(a$rep78, "label") <- lab
  a$code <- code
  a$grpc <- ifelse(code < 3, "lo", ifelse(code == 3, "mid", "hi"))
  a
}

expect_union_stata <- function(tt, case) {
  want <- golden_read_cells_file(test_path("fixtures", "regtab_union", paste0(case, ".csv")))
  got <- golden_as_cells(tt)
  expect_identical(dim(got), dim(want), label = case)
  if (identical(dim(got), dim(want))) expect_identical(got, want, label = case)
}

test_that("review P0-3/P1-1: text-labelled levels join by level across models, cell for cell as Stata", {
  a <- union_auto()
  # A: another base level in model 2 (Stata ib3.; R relevel()).
  a3 <- a
  a3$rep78 <- relevel(a3$rep78, "avg")
  expect_union_stata(regtab(lm(price ~ rep78 + mpg, a), lm(price ~ rep78 + mpg, a3)), "A_relevel")
  # The base chosen with contrasts gives the same table.
  ac <- a
  contrasts(ac$rep78) <- contr.treatment(5, base = 3)
  expect_union_stata(regtab(lm(price ~ rep78 + mpg, a), lm(price ~ rep78 + mpg, ac)), "A_relevel")
  # B: model 2 lacks "fair" (lm() drops the unused level).
  expect_union_stata(regtab(lm(price ~ rep78 + mpg, a), lm(price ~ rep78 + mpg, a, subset = code != 2)), "B_subset")
  # D: the subset model first: "fair" keeps its place (Stata's code order).
  expect_union_stata(regtab(lm(price ~ rep78 + mpg, a, subset = code != 2), lm(price ~ rep78 + mpg, a)),
                     "D_subset_first")
  # C: a character predictor (Stata encode) missing "hi" in model 2.
  expect_union_stata(regtab(lm(price ~ grpc + mpg, a), lm(price ~ grpc + mpg, a, subset = grpc != "hi")),
                     "C_encode_subset")
})

test_that("B02: interactions with a factor coded by a contrasts<- matrix map to its coefficients", {
  set.seed(25)
  n <- 500
  d <- data.frame(x = rnorm(n), g = factor(sample(c("a", "b", "c"), n, TRUE)), h = factor(sample(c("u", "v"), n, TRUE)))
  d$y <- d$x + (d$g == "a") * d$x + 0.5 * (d$g == "c") * (d$h == "v") + rnorm(n)
  rows_of <- function(tt) {
    r <- tt$meta$regtab_rows
    stats::setNames(r$estimate, r$key)[r$status == "est"]
  }
  status_of <- function(tt) {
    r <- tt$meta$regtab_rows
    stats::setNames(r$status, r$key)
  }
  # contr.treatment(3, base = 2): columns "1" and "3" (g1, g3), base b.
  # contr.SAS(3): columns "1" and "2", base c (Stata's ib(last).).
  specs <- list(list(ctr = contr.treatment(3, base = 2), base = 2L, lev = c(1L, 3L)),
                list(ctr = contr.SAS(3), base = 3L, lev = c(1L, 2L)))
  for (sp in specs) {
    dc <- d
    contrasts(dc$g) <- sp$ctr
    gx <- lm(y ~ g * x, dc)
    b <- coef(gx)
    for (mode in c("fvgen", "native")) {
      tt <- regtab(gx, interactions = mode)
      est <- rows_of(tt)
      for (k in sp$lev) {
        expect_equal(unname(est[[paste0(k, ".g#c.x")]]), unname(b[[paste0("g", k, ":x")]]), label = paste(mode, k))
        expect_equal(unname(est[[paste0(k, ".g")]]), unname(b[[paste0("g", k)]]), label = paste(mode, k, "main"))
      }
      # The Reference on the base level (native; fvgen shows no base rows).
      st <- status_of(tt)
      if (mode == "native") {
        expect_identical(st[[paste0(sp$base, ".g")]], "ref")
        expect_identical(st[[paste0(sp$base, ".g#c.x")]], "ref")
      } else {
        expect_false(any(c(paste0(sp$base, ".g"), paste0(sp$base, ".g#c.x")) %in% names(st)))
      }
    }
    gh <- lm(y ~ g * h, dc)
    b <- coef(gh)
    for (mode in c("fvgen", "native")) {
      tt <- regtab(gh, interactions = mode)
      est <- rows_of(tt)
      for (k in sp$lev) {
        expect_equal(unname(est[[paste0(k, ".g#2.h")]]), unname(b[[paste0("g", k, ":hv")]]), label = paste(mode, k))
      }
      st <- status_of(tt)
      if (mode == "native") {
        expect_identical(st[[paste0(sp$base, ".g")]], "ref")
        expect_identical(unname(st[paste0(1:3, ".g#1.h")]), rep("ref", 3))
        expect_identical(st[[paste0(sp$base, ".g#2.h")]], "ref")
      } else {
        expect_false(paste0(sp$base, ".g#2.h") %in% names(st))
      }
    }
  }
  # The same numbers as the relevel()'d fit (contr.treatment, base b).
  dc <- d
  contrasts(dc$g) <- contr.treatment(3, base = 2)
  dr <- d
  dr$g <- relevel(dr$g, "b")
  a <- regtab(lm(y ~ g * x, dc))$body
  r <- regtab(lm(y ~ g * x, dr))$body
  expect_identical(a[-1], r[-1])
})

test_that("R02: main-effect rows of a contrasts<- factor sit on the level its column indicates", {
  set.seed(11)
  n <- 900
  mk <- function(levs, ctr) {
    g <- factor(sample(levs, n, TRUE), levels = levs)
    x <- rnorm(n)
    y <- x * (1 + c(0, 0.5, 2)[as.integer(g)]) + c(0, 1, 3)[as.integer(g)] + rnorm(n, 0, .05)
    d <- data.frame(y, x, g)
    contrasts(d$g) <- ctr
    d
  }
  cm_custom <- contr.treatment(3, base = 2)
  colnames(cm_custom) <- c("A", "C")
  cases <- list(
    # factor(0:2), Stata ib(last).: g1 is level "0", g2 level "1", base "2".
    list(d = mk(c("0", "1", "2"), contr.treatment(3, base = 3)), base = "2", col = c("0" = "g1", "1" = "g2")),
    # contr.SAS(3) numbers its columns: g1 = level "3", g2 = level "1", base "2".
    list(d = mk(c("3", "1", "2"), contr.SAS(3)), base = "2", col = c("3" = "g1", "1" = "g2")),
    # An unnamed matrix: model.matrix() numbers its columns (g1 = a, g2 = c).
    list(d = mk(c("a", "b", "c"), matrix(c(1, 0, 0, 0, 0, 1), 3)), base = "b", col = c(a = "g1", c = "g2")),
    # Custom column names (review R08): gA = a, gC = c.
    list(d = mk(c("a", "b", "c"), cm_custom), base = "b", col = c(a = "gA", c = "gC")))
  for (cs in cases) {
    codes <- tabtools:::.rt_codes(cs$d$g)
    for (fm in list(y ~ g + x, y ~ g * x)) {
      f <- lm(fm, cs$d)
      b <- coef(f)
      for (mode in c("fvgen", "native")) {
        r <- regtab(f, interactions = mode)$meta$regtab_rows
        lab <- paste(deparse(fm), mode, cs$base)
        for (lv in names(cs$col)) {
          k <- paste0(codes[[lv]], ".g")
          expect_equal(unname(r$estimate[r$key == k]), unname(b[[cs$col[[lv]]]]), label = paste(lab, lv))
          if (grepl("\\*", deparse(fm))) {
            ki <- paste0(codes[[lv]], ".g#c.x")
            expect_equal(unname(r$estimate[r$key == ki]), unname(b[[paste0(cs$col[[lv]], ":x")]]),
                         label = paste(lab, lv, "cell"))
          }
        }
        kb <- paste0(codes[[cs$base]], ".g")
        if (mode == "native" || !grepl("\\*", deparse(fm))) {
          expect_identical(r$status[r$key == kb], "ref", label = paste(lab, "base"))
        } else {
          expect_false(kb %in% r$key)
        }
      }
    }
  }
  # Without an intercept the factor is coded in full: one row per level, no Reference.
  f0 <- lm(y ~ 0 + g + x, cases[[1]]$d)
  r0 <- regtab(f0)$meta$regtab_rows
  expect_equal(unname(r0$estimate[match(paste0(0:2, ".g"), r0$key)]), unname(coef(f0)[c("g0", "g1", "g2")]))
  expect_false("ref" %in% r0$status)
  # A mapping to coefficients the fit lacks is refused, not shown mislabelled.
  f <- lm(y ~ g + x, cases[[1]]$d)
  fr <- tabtools:::.rt_frame(f)
  tp <- as.data.frame(broom.helpers::tidy_plus_plus(f, add_reference_rows = TRUE))
  tp <- h1_declared(tp, "Coef.", level = .95)
  rows <- tp[tp$variable %in% "g", ]
  wald <- data.frame(term = c("(Intercept)", "x"))
  expect_error(tabtools:::.rt_contrast_rows("g", rows, wald, fr$mf, fr$mm), "cannot be matched")
})

test_that("B06: level texts mixing integers and text whose codes would collide keep positions", {
  set.seed(6)
  n <- 300
  d <- data.frame(x = rnorm(n), g = factor(sample(c("a", "1", "2"), n, TRUE), levels = c("a", "1", "2")))
  d$y <- d$x + (d$g == "2") + rnorm(n)
  # Values would give "1" and "2" the codes 1 and 2, and position gives "a" 1.
  cd <- tabtools:::.rt_codes(d$g)
  expect_identical(as.character(cd), c("1", "2", "3"))
  expect_true(attr(cd, "positional"))
  # No collision: integer texts keep their values (leading zeros too).
  expect_identical(as.character(tabtools:::.rt_codes(factor(c("0", "1", "2")))), c("0", "1", "2"))
  expect_identical(as.character(tabtools:::.rt_codes(factor(c("1", "2", "b")))), c("1", "2", "3"))
  expect_identical(as.character(tabtools:::.rt_codes(factor(c("01", "5", "10")))), c("1", "10", "5"))  # levels "01" "10" "5"
  f <- lm(y ~ g + x, d)
  r <- regtab(f)$meta$regtab_rows
  b <- coef(f)
  expect_equal(unname(r$estimate[r$key == "2.g"]), unname(b[["g1"]]))
  expect_equal(unname(r$estimate[r$key == "3.g"]), unname(b[["g2"]]))
  expect_identical(r$status[r$key == "1.g"], "ref")
  fi <- lm(y ~ g * x, d)
  bi <- coef(fi)
  for (mode in c("fvgen", "native")) {
    ri <- regtab(fi, interactions = mode)$meta$regtab_rows
    expect_equal(unname(ri$estimate[ri$key == "3.g#c.x"]), unname(bi[["g2:x"]]), label = mode)
    expect_equal(unname(ri$estimate[ri$key == "2.g#c.x"]), unname(bi[["g1:x"]]), label = mode)
  }
  # effecttab keys the factor as regtab does.
  skip_if_not_installed("marginaleffects")
  d$yb <- rbinom(n, 1, plogis(0.3 * d$x + 0.5 * (d$g == "2")))
  fb <- glm(yb ~ g + x, binomial, d)
  er <- tt_effect_rows(marginaleffects::avg_comparisons(fb, variables = "g"))
  rb <- regtab(fb)$meta$regtab_rows
  expect_identical(paste0(er$level, ".g"), rb$key[rb$status == "est" & grepl("\\.g$", rb$key)])
  expect_identical(unique(paste0(er$base, ".g")), rb$key[rb$status == "ref"])
})

test_that("review P0-3/P1-1: joined keys, data frames, and what is left to refuse", {
  a <- union_auto()
  a3 <- a
  a3$rep78 <- relevel(a3$rep78, "avg")
  f1 <- lm(price ~ rep78 + mpg, a)
  f3 <- lm(price ~ rep78 + mpg, a3)
  # Keys follow model 1's numbering; keep = "3.rep78" is avg in both models.
  r <- regtab(f1, f3)$meta$regtab_rows
  expect_identical(r$key[r$model == 1 & grepl("^[0-9]+\\.rep78$", r$key)], paste0(1:5, ".rep78"))
  kept <- suppressMessages(regtab(f1, f3, keep = "3.rep78"))$body
  expect_identical(kept[[1]], "  avg")
  expect_identical(kept[[5]], "Reference")
  # Same levels in every model: keys unchanged (the goldens' case).
  expect_identical(regtab(f1, f1)$meta$regtab_rows$key, regtab(f1)$meta$regtab_rows$key[c(seq_len(8), seq_len(8))])
  # A data frame from the relevelled fit joins the same way.
  skip_if_not_installed("broom.helpers")
  tp <- broom.helpers::tidy_plus_plus(f3)
  tp <- h1_declared(tp, "Coef.", level = .95)
  tt <- regtab(f1, tp)
  b <- tt$body
  i <- match(c("poor", "avg"), trimws(b[[1]]))
  expect_identical(b[[5]][i[2]], "Reference")
  expect_identical(b[[5]][i[1]], sprintf("%.2f", coef(f3)[["rep78poor"]]))
  # A character predictor with a value only model 2 has: sorted as encode.
  ch <- a
  ch$g <- ifelse(ch$code <= 2, "b", ifelse(ch$code == 3, "c", "d"))
  ch2 <- ch
  ch2$g[ch2$code == 5] <- "a"
  t2 <- regtab(lm(price ~ g, ch), lm(price ~ g, ch2))
  expect_identical(trimws(t2$body[[1]][2:5]), c("a", "b", "c", "d"))
  expect_identical(t2$body[[2]][2:5], c("", "Reference", sprintf("%.2f", coef(lm(price ~ g, ch))[c("gc", "gd")])))
  # Left to refuse: a factor mixing numeric and text levels keyed by position.
  mx <- data.frame(y = c(1, 3, 2, 5, 4, 6, 8, 7, 9, 12), f = factor(rep(c("0", "a"), 5)))
  mx2 <- mx
  mx2$f <- factor(as.character(mx2$f), levels = c("a", "0"))
  expect_error(regtab(lm(y ~ f, mx), lm(y ~ f, mx2)), "cannot be matched across models", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Review of group t2a, P0-1/P0-2: the ratio scale of a data frame without
# stata_cmd, and effect scales outside the known lists

test_that("P.6: broom declarations are required before statistical scale checks", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("broom")
  fit <- glm(am ~ wt, binomial, mtcars)
  tp <- broom.helpers::tidy_plus_plus(fit, exponentiate = TRUE)
  expect_error(regtab(tp), class = "tabtools_error_regtab_metadata")
  tp <- h1_declared(tp, "OR")
  expect_identical(regtab(tp)$stored$coef_label, "OR")
  bare <- as.data.frame(broom::tidy(fit, exponentiate = TRUE))
  expect_error(regtab(bare), class = "tabtools_error_regtab_metadata")
  bare <- h1_declared(bare, "Coef.")
  expect_error(regtab(bare), "looks exponentiated", fixed = TRUE)
  bare <- h1_declared(bare, "OR")
  attr(bare, "se_scale") <- "link"
  expect_identical(regtab(bare, keep = "wt")$body[[3]], regtab(fit)$body[[3]])
  plain <- h1_declared(as.data.frame(broom::tidy(lm(mpg ~ wt, mtcars))), "Coef.")
  expect_identical(regtab(plain)$stored$coef_label, "Coef.")
})

test_that("review P0-2: an effect scale outside the lists never falls back to null 0", {
  d <- data.frame(term = "x", estimate = 1, std.error = 0.1)
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  attr(d, "effect_scale") <- "RR"
  attr(d, "se_scale") <- "link"
  tt <- regtab(d, dimnonsig = TRUE)
  expect_identical(tt$body[[4]], "1.00")
  expect_true(tt$rows$dim)
  for (lab in c("PR", "Risk ratio", "exp(b)", "GMR")) {
    attr(d, "effect_scale") <- lab
    expect_identical(regtab(d)$body[[4]], "1.00", label = lab)
  }
  # Unknown label: refused when the null is needed, unless null_value says it.
  u <- data.frame(term = "x", estimate = 1, std.error = 0.1)
  attr(u, "effect_scale") <- "Coef."
  attr(u, "conf.level") <- .95
  attr(u, "inference_reference") <- "normal"
  attr(u, "effect_scale") <- "Estimate"
  expect_error(regtab(u), "whose null value regtab does not know", fixed = TRUE)
  attr(u, "null_value") <- 1
  attr(u, "se_scale") <- "estimate"
  expect_identical(regtab(u, dimnonsig = TRUE)$body[[4]], "1.00")
  attr(u, "null_value") <- 0
  attr(u, "se_scale") <- NULL
  expect_identical(regtab(u)$body[[4]], "<0.001")
  # Fully supplied: no null needed, except by dimnonsig.
  s <- data.frame(term = "x", estimate = 1, conf.low = 0.5, conf.high = 2, p.value = 0.9)
  attr(s, "effect_scale") <- "Coef."
  attr(s, "conf.level") <- .95
  attr(s, "inference_reference") <- "normal"
  attr(s, "effect_scale") <- "Estimate"
  expect_identical(regtab(s)$body[[3]], "(0.50, 2.00)")
  expect_error(regtab(s, dimnonsig = TRUE), "needs each model's null value", fixed = TRUE)
  # A null_value contradicting a known label, or not 0/1, is refused.
  attr(s, "effect_scale") <- "OR"
  attr(s, "null_value") <- 0
  expect_error(regtab(s), "has null 1, but its `null_value` is 0", fixed = TRUE)
  attr(s, "null_value") <- 2
  expect_error(regtab(s), "must be 0 or 1", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Review of group t2a: P2-1, P2-3, P3-1..P3-4 and the mutation survivors
# M18, M19, M32

test_that("review P2-1 (M19): offset() is not a predictor, tt() is", {
  d <- data.frame(cnt = c(2, 5, 3, 8, 6, 9, 4, 7, 10, 12), x = c(1, 3, 2, 5, 4, 6, 2, 5, 7, 8),
                  t = c(10, 12, 9, 15, 11, 14, 10, 13, 16, 18))
  f <- glm(cnt ~ x + offset(log(t)), poisson, d)
  # (terms() keeps offset() out of the term labels; the rule in
  # .rt_rhs_terms() is a second guard, so review mutant M19 is equivalent.)
  expect_identical(tabtools:::.rt_n_predictors(f), 1L)
  expect_identical(regtab(f)$stored$methods,
                   "Incidence rate ratios with 95% confidence intervals from univariable Poisson regression.")
  skip_if_not_installed("survival")
  l <- survival::lung
  ft <- survival::coxph(survival::Surv(time, status) ~ sex + tt(age), data = l, tt = function(x, t, ...) x * log(t))
  expect_identical(tabtools:::.rt_n_predictors(ft), 2L)
})

test_that("review P3-1 (M18): GEE and meglm nouns come from the glm family and link", {
  m <- tabtools:::.rt_methods_model
  gee <- function(fam, link) list(stata_cmd = "xtgee", effect_scale = "Coef.", family = fam, link = link)
  expect_identical(m(gee("binomial", "probit")), "generalized estimating equation (GEE) probit regression")
  expect_identical(m(gee("binomial", "cloglog")), "generalized estimating equation (GEE) complementary log-log regression")
  expect_identical(m(gee("binomial", "log")), "generalized estimating equation (GEE) log-binomial regression")
  expect_identical(m(gee("poisson", "log")), "generalized estimating equation (GEE) Poisson regression")
  expect_identical(m(gee("gaussian", "identity")), "generalized estimating equation (GEE) linear regression")
  expect_identical(m(list(stata_cmd = "meglm", effect_scale = "Coef.", family = "Gamma", link = "log")),
                   "mixed-effects gamma regression with a log link")
  expect_identical(m(list(stata_cmd = "meglm", effect_scale = "Coef.", family = "Gamma", link = "inverse")),
                   "mixed-effects generalized linear model (gamma family, reciprocal link)")
  skip_if_not_installed("geepack")
  d <- mtcars[order(mtcars$cyl), ]
  g <- suppressWarnings(geepack::geeglm(am ~ mpg + wt, binomial("probit"), data = d, id = cyl))
  expect_identical(suppressWarnings(regtab(g))$stored$methods,
                   "Coefficients with 95% confidence intervals from multivariable generalized estimating equation (GEE) probit regression.")
})

test_that("review P3-2 (M32): a data frame's interaction variables count as their parts", {
  d <- data.frame(term = c("gB", "age", "gB:age"), variable = c("g", "age", "g:age"),
                  var_type = c("categorical", "continuous", "interaction"),
                  label = c("B", "age", "B * age"), estimate = c(0.5, 0.2, 0.1), std.error = c(0.2, 0.1, 0.05))
  attr(d, "effect_scale") <- "Coef."
  attr(d, "conf.level") <- .95
  attr(d, "inference_reference") <- "normal"
  # g, age and g:age are two variables, not three.
  expect_identical(tabtools:::.rt_n_predictors(d), 2L)
  expect_match(regtab(d)$stored$methods, "from multivariable regression\\.$")
  expect_identical(tabtools:::.rt_n_predictors(d[1, ]), 1L)
  # An interaction alone is two variables.
  expect_identical(tabtools:::.rt_n_predictors(d[3, ]), 2L)
})

test_that("review P3-3/P3-4: crr with several coefficients gets no adjective; svyglm is survey-weighted", {
  skip_if_not_installed("cmprsk")
  set.seed(4)
  n <- 120
  ft <- rexp(n)
  st <- sample(0:2, n, TRUE)
  f2 <- cmprsk::crr(ft, st, cbind(x = rnorm(n), z = rnorm(n)))
  expect_identical(regtab(f2)$stored$methods,
                   "Subhazard ratios with 95% confidence intervals from Fine-Gray competing-risks regression.")
  f1 <- cmprsk::crr(ft, st, cbind(x = rnorm(n)))
  expect_match(regtab(f1)$stored$methods, "from univariable Fine-Gray")
  skip_if_not_installed("survey")
  d <- data.frame(y = rbinom(n, 1, 0.4), x = rnorm(n), w = runif(n, 0.5, 2))
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  s <- suppressWarnings(survey::svyglm(y ~ x, design = des, family = quasibinomial))
  expect_identical(regtab(s)$stored$methods,
                   "Odds ratios with 95% confidence intervals from univariable survey-weighted logistic regression.")
})

test_that("review P2-3: a data frame's interaction rows share the fitted model's keys", {
  skip_if_not_installed("broom.helpers")
  set.seed(1)
  d <- data.frame(y = rnorm(90), g = factor(rep(c("A", "B", "C"), 30)), h = factor(rep(c("u", "v"), 45)), age = rnorm(90))
  f <- lm(y ~ g * age, d)
  tp <- broom.helpers::tidy_plus_plus(f)
  tp <- h1_declared(tp, "Coef.", level = .95)
  k <- regtab(tp)$meta$regtab_rows$key
  expect_true(all(c("2.g#c.age", "3.g#c.age") %in% k))
  for (m in c("fvgen", "native")) {
    tt <- suppressMessages(regtab(f, tp, interactions = m))
    r <- tt$meta$regtab_rows
    both <- r[r$key == "2.g#c.age", ]
    expect_identical(both$status, c("est", "est"), label = m)
    expect_equal(both$estimate[1], both$estimate[2], tolerance = 1e-10)
  }
  # A part that is no row of the data frame (h without a main effect):
  # the term stays the key.
  tq <- broom.helpers::tidy_plus_plus(lm(y ~ g + g:h, d))
  tq <- h1_declared(tq, "Coef.", level = .95)
  expect_true("gA:hv" %in% regtab(tq)$meta$regtab_rows$key)
})

# ---------------------------------------------------------------------------
# Review of group t2a, P1-2 (with t2b's T2B-18): a data frame's row roles
# are structural, as H5 made them for fitted models

test_that("review P1-2: a covariate named p, alpha, lnsigma or cut1 in a data frame stays a covariate", {
  for (nm in c("p", "alpha", "lnalpha", "ln_p", "1/p", "lnsigma", "cut1", "sigma")) {
    d <- data.frame(term = c(nm, "x"), estimate = c(2, 2), std.error = c(0.3, 0.3))
    attr(d, "effect_scale") <- "Coef."
    attr(d, "conf.level") <- .95
    attr(d, "inference_reference") <- "normal"
    attr(d, "stata_cmd") <- "logit"
    attr(d, "effect_scale") <- "OR"
    attr(d, "inference_reference") <- "normal"
    attr(d, "se_scale") <- "link"
    tt <- regtab(d)
    expect_identical(tt$body[[1]], c(nm, "x"), label = nm)
    # Same inputs, same cells (no estimate-scale statistics for "p").
    expect_identical(tt$body[[3]][1], tt$body[[3]][2], label = nm)
    expect_identical(tt$body[[4]][1], tt$body[[4]][2], label = nm)
    r <- tt$meta$regtab_rows
    expect_false(any(startsWith(r$key, "/::")), label = nm)
  }
})

test_that("review P1-2: ancillary rows come from structure: equations, /terms, var_type, role, broom's intercept types", {
  # Stata's lnsigma equation: ancillary, estimate-scale Wald, a negative
  # value is fine (no "TR estimates that are not positive").
  s <- data.frame(equation = c("_t", "lnsigma"), term = c("age", "_cons"), estimate = c(1.02, 0.2),
                  std.error = c(0.01, 0.05))
  attr(s, "effect_scale") <- "Coef."
  attr(s, "conf.level") <- .95
  attr(s, "inference_reference") <- "normal"
  attr(s, "stata_cmd") <- "streg"
  attr(s, "effect_scale") <- "TR"
  attr(s, "distribution") <- "lognormal"
  attr(s, "metric") <- "log_time"
  attr(s, "inference_reference") <- "normal"
  attr(s, "se_scale") <- "link"
  tt <- regtab(s, keepintercept = TRUE)
  expect_identical(tt$body[[1]], c(" t: age", "Scale: Intercept"))
  q <- qnorm(0.975)
  expect_identical(tt$body[[3]][2], sprintf("(%.2f, %.2f)", 0.2 - q * 0.05, 0.2 + q * 0.05))
  expect_identical(regtab(s)$body[[1]], " t: age")
  s$estimate[2] <- -0.3
  expect_identical(regtab(s, keepintercept = TRUE)$body[[2]][2], "-0.30")
  # A /term, a var_type and a role column; a role column beats a name.
  a <- data.frame(term = c("x", "/sigma", "lnalpha", "p"), estimate = c(1.5, 0.7, -0.4, 2),
                  std.error = c(0.1, 0.05, 0.1, 0.3), var_type = c("continuous", NA, "ancillary", NA),
                  role = c(NA, NA, NA, "ancillary"))
  attr(a, "effect_scale") <- "Coef."
  attr(a, "conf.level") <- .95
  attr(a, "inference_reference") <- "normal"
  attr(a, "stata_cmd") <- "nbreg"
  attr(a, "effect_scale") <- "IRR"
  attr(a, "inference_reference") <- "normal"
  attr(a, "se_scale") <- "link"
  expect_identical(regtab(a)$body[[1]], "x")
  k <- regtab(a, keepintercept = TRUE)$meta$regtab_rows$key
  expect_identical(k, c("x", "/::sigma", "/::lnalpha", "/::p"))
  bad <- a
  bad$role[1] <- "nuisance"
  expect_error(regtab(bad), "unknown role \"nuisance\"", fixed = TRUE)
})

test_that("review P1-2: data-frame ancillary rows join a fitted model's /-equation rows", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  f <- survival::survreg(survival::Surv(time, status) ~ age + sex, lung, dist = "lognormal")
  x <- data.frame(term = c("age", "sex", "lnsigma"), estimate = c(1.01, 1.5, 0.1), std.error = c(0.01, 0.1, 0.05),
                  var_type = c("continuous", "continuous", "ancillary"))
  attr(x, "effect_scale") <- "Coef."
  attr(x, "conf.level") <- .95
  attr(x, "inference_reference") <- "normal"
  attr(x, "stata_cmd") <- "streg"
  attr(x, "effect_scale") <- "TR"
  attr(x, "distribution") <- "lognormal"
  attr(x, "metric") <- "log_time"
  attr(x, "inference_reference") <- "normal"
  attr(x, "se_scale") <- "link"
  tt <- regtab(f, x, keepintercept = TRUE)
  r <- tt$meta$regtab_rows
  both <- r[r$key == "/::lnsigma", ]
  expect_identical(both$status, c("est", "est"))
  expect_identical(sum(trimws(tt$body[[1]]) == "lnsigma"), 1L)
  # polr thresholds typed as intercepts by broom.helpers: cutpoints, keyed
  # cut1, cut2 as the fitted polr keys them.
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("MASS")
  p <- MASS::polr(factor(gear) ~ mpg + wt, mtcars, Hess = TRUE)
  tq <- broom.helpers::tidy_plus_plus(p, intercept = TRUE)
  tq <- h1_declared(tq, "Coef.", level = .95)
  r2 <- regtab(p, tq, keepintercept = TRUE)$meta$regtab_rows
  cut <- r2[r2$key %in% c("/::cut1", "/::cut2"), ]
  expect_identical(cut$status, rep("est", 4))
  expect_equal(cut$estimate[cut$model == 2], unname(p$zeta), tolerance = 1e-8)
  expect_identical(regtab(tq, nointercept = TRUE)$body[[1]], c("mpg", "wt"))
  # Exponentiated by broom, thresholds included: no derived statistics.
  te <- broom.helpers::tidy_plus_plus(p, intercept = TRUE, exponentiate = TRUE)
  te <- h1_declared(te, "OR", level = .95)
  te <- te[, setdiff(names(te), c("conf.low", "conf.high", "p.value"))]
  attr(te, "se_scale") <- "link"
  expect_error(regtab(te, keepintercept = TRUE), "cutpoints and ancillary parameters included", fixed = TRUE)
  # survreg's Log(scale), typed as an intercept by broom.helpers: ancillary.
  s <- survival::survreg(survival::Surv(time, status) ~ age, lung)
  ts <- broom.helpers::tidy_plus_plus(s, intercept = TRUE)
  ts <- h1_declared(ts, "Coef.", level = .95)
  rs <- regtab(ts, keepintercept = TRUE)$meta$regtab_rows
  expect_true("/::Log(scale)" %in% rs$key)
  expect_identical(rs$key[rs$label == "Intercept"], "_cons")
})

test_that("review P3-8: a coxph with tt() keeps its variable labels without a warning", {
  skip_if_not_installed("survival")
  l <- survival::lung
  attr(l$sex, "label") <- "Sex"
  f <- survival::coxph(survival::Surv(time, status) ~ sex + tt(age), l, tt = function(x, t, ...) x * log(t),
                       ties = "breslow")
  expect_no_warning(tt <- regtab(f))
  expect_identical(tt$body[[1]][1], "Sex")
  expect_match(tt$stored$methods, "multivariable Cox")
})
