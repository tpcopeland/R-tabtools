# Regression guards for the Codex audit of 0.1.0.9000 (codexaudit.md,
# snapshot fddddc0). The audit's C-findings are called CX-1..CX-6 here (the
# plan's conditional tasks are C1-C6); R1-R4 and D1-D5 keep their names.
# Every guard asserts the refusal or the corrected value and the good path.

ca_bytes <- function(p) readBin(p, "raw", file.size(p))

ca_unreadable <- function(p, env = parent.frame()) {
  skip_on_os("windows")
  Sys.chmod(p, "0200")
  withr::defer(Sys.chmod(p, "0600"), envir = env)
  if (file.access(p, 4L) == 0L) skip("running with read access everywhere (root?)")
  invisible(p)
}

ca_block <- function() puttab(data.frame(a = "new", b = "body"))

# CX-1 -----------------------------------------------------------------------

test_that("CX-1: stacktab mdappend onto an unreadable Markdown file errors and keeps its bytes", {
  dir <- withr::local_tempdir()
  md <- file.path(dir, "doc.md")
  writeLines("ORIGINAL MUST SURVIVE", md)
  before <- ca_bytes(md)
  csv <- file.path(dir, "t.csv")
  ca_unreadable(md)
  expect_error(stacktab(list(ca_block()), markdown = md, mdappend = TRUE, csv = csv),
               "Could not read the existing `markdown` target")
  Sys.chmod(md, "0600")
  expect_identical(ca_bytes(md), before)
  # Nothing else was written either.
  expect_false(file.exists(csv))
  # Good path: a readable file is appended to.
  stacktab(list(ca_block()), markdown = md, mdappend = TRUE, csv = csv)
  out <- readLines(md)
  expect_identical(out[1], "ORIGINAL MUST SURVIVE")
  expect_true("| new | body |" %in% out)
  expect_true(file.exists(csv))
})

test_that("CX-1: an unreadable workbook is refused by stacktab and puttab, the file unchanged", {
  dir <- withr::local_tempdir()
  xl <- file.path(dir, "book.xlsx")
  suppressMessages(puttab(data.frame(a = "old", b = "sheet"), xlsx = xl, sheet = "Keep"))
  before <- ca_bytes(xl)
  ca_unreadable(xl)
  expect_error(suppressMessages(stacktab(list(ca_block()), xlsx = xl, sheet = "New")), "Could not read")
  expect_error(suppressMessages(puttab(data.frame(a = "x"), xlsx = xl, sheet = "New")))
  Sys.chmod(xl, "0600")
  expect_identical(ca_bytes(xl), before)
  # Good path: the readable workbook gains the sheet and keeps the old one.
  suppressMessages(stacktab(list(ca_block()), xlsx = xl, sheet = "New"))
  expect_setequal(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(xl)), c("Keep", "New"))
})

test_that("CX-1: direct appenders refuse unverifiable bytes and append once readable", {
  dir <- withr::local_tempdir()
  md <- file.path(dir, "doc.md")
  writeLines("ORIGINAL MUST SURVIVE", md)
  before <- ca_bytes(md)
  ca_unreadable(md)
  expect_no_warning(expect_error(
    suppressMessages(puttab(data.frame(a = "new", b = "body"), markdown = md, mdappend = TRUE)),
    "Could not write the `path` target", fixed = TRUE))
  expect_no_warning(expect_error(tt_write_markdown(ca_block(), md, append = TRUE),
    "Could not write the `path` target", fixed = TRUE))
  Sys.chmod(md, "0600")
  expect_identical(ca_bytes(md), before)
  suppressMessages(puttab(data.frame(a = "new", b = "body"), markdown = md, mdappend = TRUE))
  tt_write_markdown(ca_block(), md, append = TRUE)
  out <- readLines(md)
  expect_identical(out[1], "ORIGINAL MUST SURVIVE")
  expect_identical(sum(out == "| new | body |"), 2L)
})

test_that("CX-1: a failed backup stops the commit before any target is replaced", {
  dir <- withr::local_tempdir()
  md <- file.path(dir, "doc.md")
  csv <- file.path(dir, "t.csv")
  writeLines("old md", md)
  writeLines("old,csv", csv)
  b_md <- ca_bytes(md)
  b_csv <- ca_bytes(csv)
  real <- tabtools:::.tt_file_copy
  local_mocked_bindings(.tt_file_copy = function(from, to, overwrite = FALSE) {
    if (grepl("stacktab-backup-", to, fixed = TRUE)) return(FALSE)
    real(from, to, overwrite)
  })
  expect_error(stacktab(list(ca_block()), markdown = md, csv = csv), "Could not back up the existing file")
  expect_identical(ca_bytes(md), b_md)
  expect_identical(ca_bytes(csv), b_csv)
})

test_that("CX-1: the failed target is restored too, and a failed restore is reported, not claimed", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.csv")
  b <- file.path(dir, "b.md")
  writeLines("old a", a)
  writeLines("old b", b)
  stage <- file.path(dir, "stage")
  dir.create(stage)
  writeLines("new", file.path(stage, "1"))
  writeLines("new", file.path(stage, "2"))
  staged <- stats::setNames(file.path(stage, c("1", "2")), c(a, b))
  real <- tabtools:::.tt_file_copy
  # The copy onto b fails after writing half of it.
  local_mocked_bindings(.tt_file_copy = function(from, to, overwrite = FALSE) {
    if (identical(to, b) && startsWith(from, stage)) {
      writeLines("PARTIAL", b)
      return(FALSE)
    }
    real(from, to, overwrite)
  })
  expect_error(tabtools:::.stacktab_commit(staged), "every target was restored")
  expect_identical(readLines(a), "old a")
  expect_identical(readLines(b), "old b")
  # Now the restore of a fails as well: the error names it and keeps the
  # backup instead of claiming a rollback.
  local_mocked_bindings(.tt_file_copy = function(from, to, overwrite = FALSE) {
    if (identical(to, b) && startsWith(from, stage)) return(FALSE)
    if (identical(to, a) && grepl("stacktab-backup-", from, fixed = TRUE)) return(FALSE)
    real(from, to, overwrite)
  })
  e <- tryCatch(tabtools:::.stacktab_commit(staged), error = function(e) e)
  expect_s3_class(e, "error")
  expect_match(conditionMessage(e), "could not restore")
  expect_match(conditionMessage(e), "original files are kept in")
  expect_no_match(conditionMessage(e), "every target was restored")
  # Good path.
  local_mocked_bindings(.tt_file_copy = real)
  tabtools:::.stacktab_commit(staged)
  expect_identical(readLines(a), "new")
  expect_identical(readLines(b), "new")
})

# CX-2 -----------------------------------------------------------------------

ca_dev_full <- function(ext, env = parent.frame()) {
  skip_on_os(c("windows", "mac"))
  if (!file.exists("/dev/full")) skip("no /dev/full")
  p <- tempfile(fileext = ext)
  if (!isTRUE(file.symlink("/dev/full", p))) skip("cannot link to /dev/full")
  withr::defer(unlink(p), envir = env)
  p
}

test_that("CX-2: a close-time write failure is an error naming the target", {
  tab <- ca_block()
  p <- ca_dev_full(".csv")
  n_con <- nrow(showConnections())
  e <- tryCatch(tt_write_csv(tab, p), error = function(e) e)
  expect_s3_class(e, "error")
  expect_match(conditionMessage(e), "Could not write the `path` target", fixed = TRUE)
  expect_match(conditionMessage(e), basename(p), fixed = TRUE)
  m <- ca_dev_full(".md")
  expect_error(tt_write_markdown(tab, m), "Could not write the `path` target", fixed = TRUE)
  expect_no_warning(expect_error(tt_write_markdown(tab, m, append = TRUE),
    "Could not write the `path` target", fixed = TRUE))
  # Through a command: no table comes back as if written.
  res <- tryCatch(suppressMessages(puttab(data.frame(a = "x"), csv = p)), error = function(e) NULL)
  expect_null(res)
  # No connection is left open.
  expect_identical(nrow(showConnections()), n_con)
  # Good path.
  ok <- tempfile(fileext = ".csv")
  expect_identical(tt_write_csv(tab, ok), ok)
  expect_identical(readLines(ok), c("a,b", "new,body"))
  okm <- tempfile(fileext = ".md")
  expect_identical(as.character(tt_write_markdown(tab, okm)), okm)
  expect_true("| new | body |" %in% readLines(okm))
})

# R1 -------------------------------------------------------------------------

ca_gee_data <- function() {
  set.seed(22)
  d <- data.frame(x = stats::rnorm(100), id = rep(1:20, each = 5))
  d$y <- 1 + 0.6 * d$x + stats::rnorm(100)
  d
}

test_that("R1: a geeglm scale.value held outside the data is refused, before and after it changes", {
  skip_if_not_installed("geepack")
  d <- ca_gee_data()
  sc <- rep(2, 100)
  g <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", scale.fix = TRUE, scale.value = sc)
  msg <- "refers to\\s+`sc`,\\s+which\\s+the\\s+fit\\s+does\\s+not\\s+store"
  expect_error(tt_vcov(g), msg)
  expect_error(regtab(g), msg)
  sc[] <- 8
  expect_error(tt_vcov(g), msg)
  rm(sc)
  expect_error(tt_vcov(g), msg)
  n <- 100
  g2 <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", scale.fix = TRUE,
                        scale.value = rep(2, n))
  expect_error(tt_vcov(g2), "refers to\\s+`n`")
})

test_that("R1: a scale from the fit's data gives the same variance whatever happens to the caller's data", {
  skip_if_not_installed("geepack")
  d <- ca_gee_data()
  g <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", scale.fix = TRUE,
                       scale.value = rep(2, nrow(d)))
  v0 <- tt_vcov(g)
  # xtgee's scale(2): the naive variance over geepack's gamma, times 2.
  expect_equal(unname(v0), unname(g$geese$vbeta.naiv * 2 / g$geese$gamma[[1]]), tolerance = 1e-12)
  d <- d[1:50, ]
  expect_identical(tt_vcov(g), v0)
  rm(d)
  expect_identical(tt_vcov(g), v0)
  dc <- ca_gee_data()
  dc$sc <- 2
  gc <- geepack::geeglm(y ~ x, data = dc, id = id, corstr = "independence", scale.fix = TRUE, scale.value = sc)
  vc <- tt_vcov(gc)
  expect_equal(vc, v0)
  dc$sc <- 8
  sc <- 8
  expect_identical(tt_vcov(gc), vc)
  expect_s3_class(regtab(gc), "tt_table")
})

# R2 -------------------------------------------------------------------------

test_that("R2: robust, cluster, weighted and gee_as = 'glm' variances are the stored sandwich for every std.err", {
  skip_if_not_installed("geepack")
  d <- ca_gee_data()
  d$w <- rep(c(0.5, 1, 1.5, 2, 1), 20)
  for (se in c("san.se", "jack", "j1s", "fij")) {
    g <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", std.err = se)
    sand <- unname(g$geese$vbeta) * 20 / 19
    expect_equal(unname(tt_vcov(g, "robust")), sand, tolerance = 1e-12, info = se)
    expect_equal(unname(tt_vcov(g, "cluster")), sand, tolerance = 1e-12, info = se)
    expect_equal(unname(tt_vcov(g, gee_as = "glm")), sand, tolerance = 1e-12, info = se)
    # "model" is the fit's own vcov(): the jackknife where one was chosen.
    expect_equal(tt_vcov(g, "model"), as.matrix(stats::vcov(g)), info = se)
    if (se != "san.se") expect_gt(max(abs(unname(stats::vcov(g)) * 20 / 19 - sand)), 1e-6)
    # Weighted: xtgee [pweight]'s default is the sandwich too.
    gw <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", std.err = se, weights = w)
    expect_equal(unname(tt_vcov(gw)), unname(gw$geese$vbeta) * 20 / 19, tolerance = 1e-12, info = se)
    # regtab shows the sandwich SE.
    r <- as.data.frame(regtab(g, vce = "robust"))
    expect_s3_class(r, "data.frame")
  }
  # The unweighted default is xtgee's model-based variance, unchanged.
  g <- geepack::geeglm(y ~ x, data = d, id = id, corstr = "independence", std.err = "jack")
  expect_equal(unname(tt_vcov(g)), unname(g$geese$vbeta.naiv), tolerance = 1e-12)
})

# CX-3 -----------------------------------------------------------------------

test_that("CX-3: puttab keeps both columns of a duplicated name, in every sink", {
  x <- data.frame(x = c("A", "B"), x = c("C", "D"), check.names = FALSE)
  tt <- puttab(x)
  expect_identical(unname(as.matrix(tt$body)), matrix(c("A", "B", "C", "D"), 2))
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "t.csv")
  xl <- file.path(dir, "t.xlsx")
  suppressMessages(puttab(x, csv = csv, xlsx = xl))
  expect_identical(readLines(csv), c("x,x", "A,C", "B,D"))
  wb <- openxlsx2::wb_to_df(openxlsx2::wb_load(xl), col_names = FALSE)
  cells <- unlist(wb, use.names = FALSE)
  expect_true(all(c("A", "B", "C", "D") %in% cells))
  # An explicit selection of the ambiguous name is refused; a unique one works.
  expect_error(puttab(x, vars = "x"), "more than one column")
  y <- data.frame(x = 1:2, x = 3:4, z = c("u", "v"), check.names = FALSE)
  expect_identical(unname(as.matrix(puttab(y, vars = "z")$body)), matrix(c("u", "v"), 2))
})

test_that("CX-3: the other exporters refuse an ambiguous column name and accept unique ones", {
  r <- data.frame(t = c(2, 3, 4, 5), e = c(0, 1, 0, 1), g = c("A", "A", "B", "B"), g = c("X", "Y", "X", "Y"),
                  check.names = FALSE)
  expect_error(tt_rates(r, "t", "e", by = "g", float_time = FALSE), "more than one column")
  names(r)[4] <- "h"
  expect_identical(tt_rates(r, "t", "e", by = "g", float_time = FALSE)$g, c("A", "B"))
  w <- data.frame(w = c(1, 2, 3, 4), g = c(0, 0, 1, 1), g = c(1, 1, 0, 0), check.names = FALSE)
  expect_error(wttab(w, "w", by = "g"), "more than one column")
  names(w)[3] <- "h"
  expect_identical(wttab(w, "w", by = "g")$stored$stats$n, c(4, 2, 2))
  f <- data.frame(term = c("a", "b"), estimate = c(1, 2), estimate = c(5, 6), std.error = c(0.1, 0.1),
                  check.names = FALSE)
  expect_error(regtab(f), "more than one column")
  expect_error(tt_effect_rows(f, type = "margins"), "more than one column")
  names(f)[3] <- "other"
  attr(f, "effect_scale") <- "Coef."
  attr(f, "inference_reference") <- "normal"
  expect_s3_class(regtab(f), "tt_table")
})

# CX-4 -----------------------------------------------------------------------

test_that("CX-4: renderer-read row fields and layout flags are validated, naming the field", {
  body <- data.frame(label = "a", value = "1")
  hdr <- list(c("", "Value"), c("", "N=1"))
  expect_error(tt_table(body, hdr, rows = data.frame(block = NA_integer_)), "rows$block", fixed = TRUE)
  expect_error(tt_table(body, hdr, rows = data.frame(block = "one")), "rows$block", fixed = TRUE)
  expect_error(tt_table(body, hdr, rows = data.frame(dim = NA)), "rows$dim", fixed = TRUE)
  expect_error(tt_table(body, hdr, layout = list(console_sepby = NA)), "layout$console_sepby", fixed = TRUE)
  good <- tt_table(body, hdr)
  bad <- good
  bad$rows$block <- NA_integer_
  expect_error(validate_tt_table(bad), "rows$block", fixed = TRUE)
  bad <- good
  bad$layout$console_title <- "yes"
  expect_error(validate_tt_table(bad), "layout$console_title", fixed = TRUE)
  bad <- good
  bad$rows$block <- NULL
  expect_error(validate_tt_table(bad), "block", fixed = TRUE)
  bad$layout$console_sepby <- FALSE
  expect_s3_class(validate_tt_table(bad), "tt_table")
  # Good path: supplied, valid metadata renders.
  ok <- tt_table(body, hdr, rows = data.frame(block = 3L))
  expect_s3_class(validate_tt_table(ok), "tt_table")
  expect_true(length(capture.output(print(ok))) > 0L)
  expect_true(length(capture.output(print(good))) > 0L)
})

# CX-5 -----------------------------------------------------------------------

test_that("CX-5: a one-row descriptive header keeps its label in the workbook and the converters", {
  x <- tt_table(data.frame(label = "a", value = "1"), list(c("Characteristic", "Value")))
  p <- tempfile(fileext = ".xlsx")
  tt_write_xlsx(x, p)
  wb <- openxlsx2::wb_to_df(openxlsx2::wb_load(p), col_names = FALSE)
  expect_identical(wb[2, 2], "Characteristic")
  expect_identical(wb[2, 3], "Value")
  expect_identical(c(wb[4, 2], wb[4, 3]), c("a", "1"))
  spec <- tabtools:::.tt_render_spec(x)
  expect_true("Characteristic" %in% spec$header$text)
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::as_flextable(x)
    expect_true("Characteristic" %in% unlist(ft$header$dataset, use.names = FALSE))
  }
  # Good path: a two-row header still takes the descriptor from row 2.
  y <- tt_table(data.frame(label = "a", value = "1"), list(c("", "Value"), c("Mean (SD)", "N=1")))
  q <- tempfile(fileext = ".xlsx")
  tt_write_xlsx(y, q)
  wy <- openxlsx2::wb_to_df(openxlsx2::wb_load(q), col_names = FALSE)
  expect_identical(wy[2, 2], "Mean (SD)")
  expect_identical(wy[3, 3], "N=1")
})

# R3 -------------------------------------------------------------------------

test_that("R3: tt_vcov() and tt_coef() of a tt_mi refuse what regtab() refuses, with the same message", {
  set.seed(5)
  mk <- function() {
    d <- data.frame(x = stats::rnorm(80), f = factor(sample(c("a", "b", "c"), 80, TRUE)))
    d$y <- 1 + d$x + stats::rnorm(80)
    d$z <- d$x
    d
  }
  d1 <- mk()
  d2 <- mk()
  msg <- function(expr) tryCatch({
    expr
    NA_character_
  }, error = function(e) conditionMessage(e))
  d2b <- d2
  d2b$f <- factor(d2b$f, levels = c("a", "b", "c", "d"))
  d2b$f[1] <- "d"
  bad <- list(
    formula = list(stats::lm(y ~ x, d1), stats::lm(y ~ x + I(x^2), d2)),
    class = list(stats::lm(y ~ x, d1), stats::glm(y ~ x, data = d2)),
    terms = list(stats::lm(y ~ f, d1), stats::lm(y ~ f, d2b)),
    alias = list(stats::lm(y ~ x + z, d1), stats::lm(y ~ x + z, transform(d2, z = stats::rnorm(80)))),
    nobs = list(stats::lm(y ~ x, d1), stats::lm(y ~ x, d2[-1, ]))
  )
  for (k in names(bad)) {
    m <- tt_mi(bad[[k]])
    r <- msg(regtab(m))
    expect_false(is.na(r), info = k)
    expect_identical(msg(tt_vcov(m)), r, info = k)
    expect_identical(msg(tabtools:::tt_coef(m)), r, info = k)
  }
  # Good path: compatible fits pool.
  m <- tt_mi(list(stats::lm(y ~ x, d1), stats::lm(y ~ x, d2)))
  V <- tt_vcov(m)
  expect_identical(dimnames(V), list(c("(Intercept)", "x"), c("(Intercept)", "x")))
  expect_true(all(is.finite(V)))
  b <- tabtools:::tt_coef(m)
  expect_equal(unname(b), unname((stats::coef(bad$nobs[[1]]) + stats::coef(stats::lm(y ~ x, d2))) / 2),
               tolerance = 1e-12)
})

# R4 -------------------------------------------------------------------------

test_that("R4: Surv()'s optional, named and reordered arguments do not change the outcome identity", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$start <- 0
  oid <- function(lhs) {
    f <- stats::as.formula(paste(lhs, "~ age"))
    fit <- survival::coxph(f, data = d, ties = "breslow", model = TRUE)
    attr(as.data.frame(suppressMessages(regtab(fit))), "outcome_id")
  }
  for (lhs in c("survival::Surv(time, status)", "survival::Surv(time, status, type = \"right\")",
                "survival::Surv(time, status, ty = \"right\")", "survival::Surv(time = time, event = status)",
                "survival::Surv(event = status, time = time)", "survival::Surv(time, status, origin = 0)",
                "survival::Surv(type = \"right\", time, status)")) {
    expect_identical(oid(lhs), "status", info = lhs)
  }
  expect_identical(oid("survival::Surv(start, time, status)"), "status")
  expect_identical(oid("survival::Surv(start, time, status, type = \"counting\")"), "status")
  expect_identical(oid("survival::Surv(time2 = time, time = start, event = status)"), "status")
})

test_that("R4: hrcomptab matches a model written with an explicit type = 'right' to its rates", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$event <- d$status - 1
  d$years <- d$time / 365.25
  d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
  rates <- stratetab(tt_rates(d, "years", "event", by = "sex"), outlabels = "Death", outcomeids = "event",
                     explabels = "Sex", ratescale = 100, unitlabel = "100")
  m1 <- regtab(survival::coxph(survival::Surv(years, event) ~ sex, data = d, ties = "breslow"), coef = "HR")
  m2 <- regtab(survival::coxph(survival::Surv(years, event, type = "right") ~ sex, data = d, ties = "breslow"),
               coef = "HR")
  a <- suppressMessages(hrcomptab(rates, m1, rownames = "female"))
  b <- suppressMessages(hrcomptab(rates, m2, rownames = "female"))
  expect_identical(b$body, a$body)
})

# D1 -------------------------------------------------------------------------

test_that("D1: weighted quartiles do not depend on the scale of the weights (Stata since 68c37a90 too)", {
  d <- data.frame(x = 1:10, w = 1)
  cell <- function(s) {
    d$w <- s
    table1_tc(d, vars = c(x = "conts"), wt = "w")$body[2, 2]
  }
  # qa/stata/probe_codex_audit.do: Stata tabtools 2.1.12 printed "2 (2, 2)"
  # for w = 1e-12; Stata-Tools 68c37a90 prints "6 (3, 8)" at every scale.
  for (s in c(1, 1e-9, 1e-12, 1e-100, 1e-300, 3e10, 1e100)) expect_identical(cell(s), "6 (3, 8)", info = s)
  q <- tabtools:::.t1w_quantile
  set.seed(3)
  x <- round(stats::runif(25) * 30)
  w <- stats::runif(25)
  for (p in c(0.25, 0.5, 0.75)) {
    # The equal-weight oracle: quantile(type = 2).
    ref <- unname(stats::quantile(x, p, type = 2))
    for (s in c(1, 1e-12, 1e-200, 1e200)) expect_identical(q(x, rep(s, 25), p), ref, info = c(p, s))
    # Unequal weights: the value for w, w / 7e11 and w * 2^-600 agree with
    # the value for w * 1e6 (a total above 1, walked as in Stata).
    big <- q(x, w * 1e6, p)
    for (s in c(1 / 7e11, 2^-600)) expect_identical(q(x, w * s, p), big, info = c(p, s))
  }
})

# D2 -------------------------------------------------------------------------

test_that("D2: wttab's ESS is 18/7 for weights 1, 2, 3 at every scale; all-zero weights have none", {
  for (s in c(1, 1e-200, 1e200, 1e-300, 2^-1000)) {
    st <- wttab(c(1, 2, 3) * s)$stored$stats
    expect_equal(st$ess, 18 / 7, tolerance = 1e-14, info = s)
    expect_equal(st$ess_pct, 100 * (18 / 7) / 3, tolerance = 1e-14, info = s)
  }
  z <- wttab(c(0, 0, 0))$stored$stats
  expect_true(is.na(z$ess))
  expect_true(is.na(z$ess_pct))
  # Ordinary weights keep the raw formula's value exactly.
  w <- c(0.3, 1.7, 2.2, 0.9)
  expect_identical(wttab(w)$stored$stats$ess, sum(w)^2 / sum(w^2))
})

# D3 -------------------------------------------------------------------------

test_that("D3: a factor or character df/df.error is refused; numeric df and Inf work", {
  base <- data.frame(term = "x", estimate = 1, std.error = 0.5)
  for (col in c("df", "df.error")) {
    for (v in list(factor("100"), "100", "abc")) {
      x <- base
      x[[col]] <- v
      expect_error(tt_effect_rows(x, type = "margins"), paste0("Column ", col, " of the data frame must be numeric"),
                   info = paste(col, class(v)))
      expect_error(suppressMessages(effecttab(x, type = "margins")), "must be numeric", info = col)
    }
    x <- base
    x[[col]] <- 100
    r <- tt_effect_rows(x, type = "margins")
    q <- stats::qt(0.975, 100)
    expect_equal(c(r$conf.low, r$conf.high), c(1 - q * 0.5, 1 + q * 0.5), tolerance = 1e-12)
    expect_equal(r$p.value, 2 * stats::pt(-2, 100), tolerance = 1e-12)
    x[[col]] <- Inf
    r <- tt_effect_rows(x, type = "margins")
    expect_equal(r$p.value, 2 * stats::pnorm(-2), tolerance = 1e-12)
  }
})

# D4 -------------------------------------------------------------------------

test_that("D4: tt_rates refuses grouping columns named like its statistics, in by and strata", {
  base <- data.frame(t = c(2, 3, 4, 5), e = c(0, 1, 0, 1), grp = c("A", "A", "B", "B"), s = c(1, 1, 1, 1))
  for (nm in c("D", "Y", "Rate", "Lower", "Upper")) {
    d <- base
    names(d)[names(d) == "grp"] <- nm
    expect_error(tt_rates(d, "t", "e", by = nm, float_time = FALSE), "which the result uses for its statistics",
                 info = nm)
    expect_error(tt_rates(d, "t", "e", strata = nm, by = "s", float_time = FALSE), "rename", info = nm)
  }
  r <- tt_rates(base, "t", "e", by = "grp", float_time = FALSE)
  expect_identical(r$grp, c("A", "B"))
  expect_identical(attr(r, "by"), "grp")
  expect_equal(r$Y, c(5, 9))
  # The handoff: the result goes on to stratetab.
  st <- stratetab(list(r), outcomes = 1)
  expect_true(any(grepl("^A", trimws(st$body[[1]]))))
})

test_that("D4: a Stata strate file grouped by a variable named Y is refused with a rename hint", {
  skip_if_not_installed("haven")
  f <- test_path("fixtures", "codex_audit", "strate_by_Y.dta")
  s <- haven::read_dta(f)
  expect_identical(names(s)[1:3], c("Y", "_D", "_Y"))
  expect_error(tt_rates(s), "hold both")
  expect_error(stratetab(list(f), outcomes = 1), "not a valid strate block")
  # Renamed, it reads as Stata's stratetab reads it: groups 1 and 2, _Y the
  # person-time.
  names(s)[1] <- "grp"
  r <- tt_rates(s)
  expect_equal(r$Y, c(5, 9))
  expect_s3_class(stratetab(list(s), outcomes = 1), "tt_table")
})

# D5 -------------------------------------------------------------------------

test_that("D5: automatic typing of huge finite values takes the standardised moments", {
  v <- seq_len(6000)
  expect_identical(tabtools:::tt_detect_vartype(v * 1e100), tabtools:::tt_detect_vartype(v))
  expect_identical(tabtools:::tt_detect_vartype(v * 1e100), "contn")
  set.seed(9)
  sk <- exp(stats::rnorm(6000))
  expect_identical(tabtools:::tt_detect_vartype(sk * 1e250), "conts")
  expect_identical(tabtools:::tt_detect_vartype(sk * 1e-250), "conts")
  # Stata-Tools 68c37a90 types v * 1e100 contn too (probe_codex_audit.do;
  # 2.1.12 read its missing moments as conts). The table renders.
  tab <- table1_tc(data.frame(x = v * 1e100))
  expect_match(tab$body[nrow(tab$body), 2], "±", fixed = TRUE)
})

test_that("D5 (C7): the Shapiro-Wilk branch is scale invariant, as Stata since 68c37a90", {
  v <- as.numeric(seq_len(3000))
  # probe_codex_audit.do: Stata types both conts.
  for (s in c(1, 1e100, 1e-100, 1e200)) expect_identical(tabtools:::tt_detect_vartype(v * s), "conts", info = s)
  expect_identical(tabtools:::tt_detect_vartype(c(v, NA) * 1e100), "conts")
  for (n in c(100, 1500)) {
    x <- as.numeric(seq_len(n))
    expect_identical(tabtools:::tt_detect_vartype(x * 1e-150), tabtools:::tt_detect_vartype(x), info = n)
  }
})
