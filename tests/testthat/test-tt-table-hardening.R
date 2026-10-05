# Milestone H, task H13 (findings F13, F15, F16 in
# POTENTIAL_ISSUES_2026-09-26.md): the public table constructor and
# validator, label overrides and duplicate column names.

h13_base <- function() {
  tt_table(data.frame(label = c("Age", "  young"), v = c("1", "2")),
           list(list(text = c("", "Model"), spans = data.frame(from = 2, to = 2))),
           command = "custom")
}

# Every renderer either renders a validated object or refuses it by name.
h13_renders <- function(x, label) {
  expect_no_error(utils::capture.output(print(x)), message = label)
  f <- withr::local_tempfile(fileext = ".csv")
  expect_no_error(tt_write_csv(x, f), message = label)
  m <- withr::local_tempfile(fileext = ".md")
  expect_no_error(tt_write_markdown(x, m), message = label)
  xl <- withr::local_tempfile(fileext = ".xlsx")
  r <- tryCatch(tt_write_xlsx(x, xl), error = function(e) e)
  if (inherits(r, "error")) {
    expect_match(conditionMessage(r), "No Excel layout|Nothing to export", label = label)
  }
}

test_that("H13: a one-column (label-only) table is built from inferred metadata and renders", {
  for (cmd in c("custom", "regtab", "table1_tc")) {
    tt <- tt_table(data.frame(label = "x"), list("Label"), command = cmd)
    expect_identical(nrow(tt$cols), 1L)
    expect_identical(tt$cols$role, "label")
    h13_renders(tt, cmd)
  }
  f <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(tt_table(data.frame(label = "x"), list("Label"), command = "custom"), f)
  expect_identical(readLines(f), c("Label", "x"))
})

test_that("H13: the validator refuses incomplete or invalid metadata", {
  x <- h13_base()
  y <- x; y$cols$role <- NULL
  expect_error(validate_tt_table(y), "role")
  y <- x; y$cols$role[2] <- NA
  expect_error(validate_tt_table(y), "role")
  y <- x; y$header[[1]]$spans <- data.frame(from = 1.5, to = 2)
  expect_error(validate_tt_table(y), "whole-number")
  y <- x; y$header[[1]]$spans <- data.frame(from = NA_real_, to = 2)
  expect_error(validate_tt_table(y), "non-missing")
  y <- x; y$header[[1]]$spans <- data.frame(from = 1, to = 3)
  expect_error(validate_tt_table(y), "inside the table")
  y <- x; y$header[[1]]$spans <- data.frame(start = 1)
  expect_error(validate_tt_table(y), "from")
  y <- x; y$title <- NA_character_
  expect_error(validate_tt_table(y), "title")
  y <- x; y$footnote <- NA_character_
  expect_error(validate_tt_table(y), "footnote")
  y <- x; y$title <- c("a", "b")
  expect_error(validate_tt_table(y), "title")
  y <- x; y$rows$type <- NULL
  expect_error(validate_tt_table(y), "type")
  y <- x; y$header[[1]]$text[1] <- NA
  expect_error(validate_tt_table(y), "non-missing")
  y <- x; y$body$c2[1] <- NA
  expect_error(validate_tt_table(y), "NA")
  y <- x; y$layout$align <- NULL
  expect_error(validate_tt_table(y), "layout")
  expect_error(tt_table(data.frame(a = "x"), list("A"), title = NA_character_), "title")
})

test_that("H13: every validated object renders; NULL title and footnote mean none", {
  x <- h13_base()
  h13_renders(x, "base")
  y <- x; y$title <- NULL; y$footnote <- NULL
  expect_identical(validate_tt_table(y), y)
  h13_renders(y, "NULL title")
  y <- x; y$cols$console_width <- NULL
  expect_identical(validate_tt_table(y), y)
  h13_renders(y, "no console_width")
  y <- x; y$header[[1]]$spans <- NULL
  h13_renders(validate_tt_table(y), "no spans")
  d <- data.frame(g = rep(1:2, 5), x = 1:10)
  h13_renders(validate_tt_table(table1_tc(d, by = "g", vars = c(x = "contn"))), "table1_tc")
  h13_renders(validate_tt_table(regtab(stats::lm(mpg ~ wt, mtcars))), "regtab")
})

test_that("H13: label overrides must be named, non-missing strings", {
  d <- data.frame(x = 1:4, g = c(1, 1, 2, 2))
  expect_error(table1_tc(d, vars = c(x = "contn"), labels = c(x = NA_character_)), "missing label")
  expect_error(table1_tc(d, vars = c(x = "contn"), labels = c(x = NA)), "named character")
  expect_error(table1_tc(d, vars = c(x = "contn"), labels = "Age"), "name")
  expect_error(table1_tc(d, vars = c(x = "contn"), labels = c(x = 1)), "character")
  expect_error(table1_tc(d, vars = c(x = "contn"), labels = list(x = c("a", "b"))), "single string")
  expect_error(table1_tc(d, vars = c(x = "contn"), by = "g", labels = c(g = NA_character_)), "`g`")
  expect_identical(table1_tc(d, vars = c(x = "contn"), labels = list(x = "Age"))$body[[1]][1], "Age")
  expect_identical(table1_tc(d, vars = c(x = "contn"), labels = c(x = "Age"))$body[[1]][1], "Age")
})

test_that("H13: duplicate analysis-column names are refused", {
  d <- data.frame(x = 1:4, x2 = 5:8, g = c(1, 1, 2, 2))
  names(d)[2] <- "x"
  expect_error(table1_tc(d, vars = c(x = "contn")), "more than one column named")
  expect_error(table1_tc(d), "`x`")
  e <- data.frame(y = 1:4, g = c(1, 1, 2, 2), g2 = 1)
  names(e)[3] <- "g"
  expect_error(table1_tc(e, vars = c(y = "contn"), by = "g"), "`g`")
  # A duplicate that is not analysed does not matter.
  f <- data.frame(y = 1:4, z = 1, z2 = 2)
  names(f)[3] <- "z"
  expect_no_error(table1_tc(f, vars = c(y = "contn")))
  # The same variable twice in vars is Stata's usage and stays allowed.
  expect_no_error(table1_tc(data.frame(y = 1:4), vars = "y contn \\ y conts"))
})

test_that("review R12: a label override named twice is refused", {
  expect_error(table1_tc(data.frame(x = 1:4), vars = c(x = "contn"), labels = c(x = "A", x = "B")),
               "more than once")
})

test_that("A07: a hand-built table under each Excel layout writes or names what the layout needs", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  mk <- function(r, header = list(c("", "All"))) {
    tt_table(body = data.frame(label = c("Age", "Female"), value = c("61.2", "112 (56)")),
             header = header, command = "custom", layout = list(xlsx_rules = r))
  }
  run <- function(tab) {
    c(xlsx = tryCatch({tt_write_xlsx(tab, withr::local_tempfile(fileext = ".xlsx")); "OK"}, error = conditionMessage),
      flextable = tryCatch({flextable::as_flextable(tab); "OK"}, error = conditionMessage),
      gt = tryCatch({tt_as_gt(tab); "OK"}, error = conditionMessage))
  }
  rules <- c("descriptive", "regression", "puttab", "stacktab", "stratetab", "comptab", "hrcomptab")
  for (r in rules) {
    for (h in list(list(c("", "All")), list(c("", "All"), c("", "N")), list(c("", "A"), c("", "B"), c("", "C")))) {
      res <- run(mk(r, h))
      ok <- res == "OK" | grepl("layout (needs|has)", res)
      expect_true(all(ok), label = paste(r, length(h), "header row(s):", paste(unique(res[!ok]), collapse = "; ")))
    }
  }
  expect_error(tt_write_xlsx(mk("regression"), withr::local_tempfile(fileext = ".xlsx")), "cols\\$model")
  expect_error(tt_write_xlsx(mk("stratetab"), withr::local_tempfile(fileext = ".xlsx")), "two header rows")
  expect_error(tt_write_xlsx(mk("hrcomptab"), withr::local_tempfile(fileext = ".xlsx")), "two header rows")
  expect_error(tt_write_xlsx(mk("comptab"), withr::local_tempfile(fileext = ".xlsx")), "two header rows")
  # A regression table built by hand with model blocks writes.
  reg <- tt_table(body = data.frame(label = c("Age", "Female"), est = c("1.10", "0.90"), ci = c("(1.0, 1.2)", "(0.8, 1.0)"),
                                    p = c("0.04", "0.10")),
                  header = list(c("", "Model 1", "", ""), c("", "OR", "95% CI", "p-value")), command = "custom",
                  cols = data.frame(role = c("label", "est", "ci", "pval"), model = c(NA, 1L, 1L, 1L)),
                  layout = list(xlsx_rules = "regression"))
  expect_identical(run(reg), c(xlsx = "OK", flextable = "OK", gt = "OK"))
})
