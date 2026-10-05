# The R-only `vce` stats token (task 5.18's optional row): a text row
# "Standard errors" with each model's variance in the vce_note footnote's
# words. Stata regtab has no such token; the row comes after Stata's rows
# and before stat_fun rows, has no stored scalar, and goes through every
# writer, tt_merge()/tt_stack() and the converters as text.

vt_cells <- function(tt) {
  i <- which(tt$rows$key == "stat:vce")
  unname(unlist(tt$body[i, which(tt$cols$role %in% c("est", "est_ci"))]))
}

test_that("vce token: each model's variance in the footnote's words, model-based by default", {
  d <- mtcars
  f1 <- lm(mpg ~ wt, d)
  f2 <- glm(am ~ wt, binomial, d)
  tt <- regtab(f1, f2, f1, stats = "n vce", vce = list("stata", "cluster", "robust"), cluster = list(NULL, ~cyl, NULL))
  i <- which(tt$rows$key == "stat:vce")
  expect_length(i, 1L)
  expect_identical(tt$body[[1]][i], "Standard errors")
  expect_identical(tt$rows$type[i], "stat")
  expect_identical(vt_cells(tt), c("model-based", "robust, clustered by cyl", "robust"))
  # After Stata's rows, before stat_fun rows, whatever the token order.
  t2 <- regtab(f1, stats = c("vce", "n", "ll"), stat_fun = list(Sigma = function(f) stats::sigma(f)))
  expect_identical(t2$rows$key[t2$rows$type == "stat"], c("stat:n", "stat:ll", "stat:vce", "statfun:Sigma"))
  # No stored scalar (the other rows keep theirs).
  expect_false(any(startsWith(names(t2$stored), "vce")))
  expect_true(all(c("n_1", "ll_1") %in% names(t2$stored)))
  # Case-insensitive, like the Stata tokens; no row without the token.
  expect_identical(vt_cells(regtab(f1, stats = "VCE")), "model-based")
  expect_false("stat:vce" %in% regtab(f1, stats = "n")$rows$key)
  # A user-supplied variance.
  tu <- regtab(f1, stats = "vce", vce = function(f) stats::vcov(f) * 2)
  expect_identical(vt_cells(tu), "user-supplied")
})

test_that("vce token: the variance a robust survival fit or a geeglm uses under vce = \"model\"", {
  skip_if_not_installed("survival")
  lu <- survival::lung
  lu$status <- lu$status - 1
  cx <- survival::coxph(survival::Surv(time, status) ~ age, lu, cluster = inst, ties = "breslow")
  expect_identical(vt_cells(regtab(cx, stats = "vce")), "robust, clustered by inst")
  # vce = "model" keeps survival's sandwich (without M/(M - 1)): still
  # robust, although the footnote names only non-default variances.
  expect_identical(vt_cells(regtab(cx, stats = "vce", vce = "model")), "robust, clustered by inst")
  c0 <- survival::coxph(survival::Surv(time, status) ~ age, lu, ties = "breslow")
  expect_identical(vt_cells(regtab(c0, stats = "vce", vce = list("stata"))), "model-based")
  expect_identical(vt_cells(regtab(c0, stats = "vce", vce = "robust")), "robust")
  sr <- survival::survreg(survival::Surv(time, status) ~ age, lu, robust = TRUE)
  expect_identical(vt_cells(regtab(sr, stats = "vce", vce = "model")), "robust")
  skip_if_not_installed("geepack")
  d <- mtcars
  d$id <- rep(1:8, each = 4)
  g <- geepack::geeglm(am ~ wt, binomial, d, id = id)
  expect_identical(vt_cells(regtab(g, stats = "vce", vce = "model")), "robust, clustered by id")
  gj <- suppressWarnings(geepack::geeglm(am ~ wt, binomial, d, id = id, std.err = "jack"))
  expect_identical(vt_cells(regtab(gj, stats = "vce", vce = "model")), "jackknife")
})

test_that("vce token: blank for a data frame, shared for tt_mi and regtab_uv models", {
  d <- mtcars
  df <- data.frame(term = c("wt", "(Intercept)"), estimate = c(-5, 37), std.error = c(0.5, 2))
  t1 <- regtab(lm(mpg ~ wt, d), df, stats = "vce", vce = list("robust", "stata"))
  expect_identical(vt_cells(t1), c("robust", ""))
  # A data frame alone: no model reports it, so no row.
  expect_false("stat:vce" %in% regtab(df, stats = "vce")$rows$key)
  set.seed(4)
  fits <- lapply(1:3, function(k) {
    dk <- d
    dk$mpg <- dk$mpg + stats::rnorm(32, 0, 0.5)
    lm(mpg ~ wt, dk)
  })
  expect_identical(vt_cells(regtab(tt_mi(fits), stats = "vce", vce = "robust")), "robust")
  uv <- regtab_uv(d, "am", c("wt", "hp"), method = glm, method.args = list(family = binomial))
  expect_identical(vt_cells(regtab(uv, stats = "vce")), "model-based")
  expect_identical(vt_cells(regtab(uv, stats = "vce", vce = "robust")), "robust")
  # Fits whose variances differ (one robust Cox fit among model-based ones):
  # no single label, so blank.
  skip_if_not_installed("survival")
  lu <- survival::lung
  lu$status <- lu$status - 1
  cu <- regtab_uv(lu, "survival::Surv(time, status)", c("age", "sex"), method = survival::coxph,
                  method.args = list(ties = "breslow"))
  expect_identical(tabtools:::.rt_vce_label(cu, NULL, "stata", NULL), "model-based")
  cu$fits[[2]] <- survival::coxph(survival::Surv(time, status) ~ sex, lu, ties = "breslow", robust = TRUE)
  expect_identical(tabtools:::.rt_vce_label(cu, NULL, "stata", NULL), "")
})

test_that("vce token: documented as R only in the unknown-token warning", {
  expect_warning(regtab(lm(mpg ~ wt, mtcars), stats = "foo"), "R only: vce", fixed = TRUE)
})

test_that("vce token: csv, Markdown and xlsx sinks carry the row as text", {
  d <- mtcars
  csv <- withr::local_tempfile(fileext = ".csv")
  md <- withr::local_tempfile(fileext = ".md")
  xl <- withr::local_tempfile(fileext = ".xlsx")
  tt <- regtab(lm(mpg ~ wt, d), glm(am ~ wt, binomial, d), stats = "n vce", vce = list("stata", "robust"),
               csv = csv, markdown = md, xlsx = xl, sheet = "T")
  expect_true(any(grepl("Standard errors,model-based,,,robust", readLines(csv), fixed = TRUE)))
  mdl <- readLines(md)
  row <- mdl[grepl("Standard errors", mdl, fixed = TRUE)]
  expect_length(row, 1L)
  expect_match(row, "model-based", fixed = TRUE)
  expect_match(row, "robust", fixed = TRUE)
  wb <- openxlsx2::read_xlsx(xl, sheet = "T", col_names = FALSE, skip_empty_rows = FALSE)
  r <- which(wb[[2]] == "Standard errors")
  expect_length(r, 1L)
  cells <- unlist(wb[r, ])
  expect_true(all(c("model-based", "robust") %in% cells))
  expect_no_error(validate_tt_table(tt))
})

test_that("vce token: tt_merge() and tt_stack() align the row by its key", {
  d <- mtcars
  t1 <- regtab(lm(mpg ~ wt, d), stats = "n vce")
  t2 <- regtab(lm(mpg ~ wt + hp, d), stats = "vce n", vce = "robust")
  m <- tt_merge(t1, t2)
  i <- which(m$rows$key == "stat:vce")
  expect_length(i, 1L)
  expect_identical(vt_cells(m), c("model-based", "robust"))
  s <- tt_stack(t1, t2)
  expect_identical(sum(s$rows$key == "stat:vce", na.rm = TRUE), 2L)
  expect_true(all(c("model-based", "robust") %in% unlist(s$body[s$rows$key %in% "stat:vce", ])))
})

test_that("vce token: the gt, flextable, gtsummary and tinytable converters show the row", {
  d <- mtcars
  tt <- regtab(lm(mpg ~ wt, d), glm(am ~ wt, binomial, d), stats = "n vce", vce = list("stata", "robust"))
  want <- c("Standard errors", "model-based", "robust")
  if (requireNamespace("gt", quietly = TRUE)) {
    html <- as.character(gt::as_raw_html(tt_as_gt(tt)))
    for (w in want) expect_true(grepl(w, html, fixed = TRUE), label = paste("gt", w))
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::as_flextable(tt)
    ds <- unlist(ft$body$dataset)
    for (w in want) expect_true(w %in% ds, label = paste("flextable", w))
  }
  if (requireNamespace("gtsummary", quietly = TRUE) && utils::packageVersion("gtsummary") >= "2.3.0") {
    g <- tt_as_gtsummary(tt)
    ds <- unlist(g$table_body)
    for (w in want) expect_true(w %in% ds, label = paste("gtsummary", w))
  }
  skip_if_not_installed("tinytable")
  md <- utils::capture.output(print(tt_as_tinytable(tt), output = "markdown"))
  for (w in want) expect_true(any(grepl(w, md, fixed = TRUE)), label = paste("tinytable", w))
})
