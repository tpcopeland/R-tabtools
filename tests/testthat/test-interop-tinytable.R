# tt_as_tinytable() (task 6.6).

test_that("6.6: a regtab table's cells, column groups, notes and outputs", {
  skip_if_not_installed("tinytable")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  tt <- regtab(glm(am ~ wt + cyl, binomial, d), glm(am ~ wt, binomial, d), stats = "n",
               models = c("Adjusted", "Crude"), title = "Table 2", footnote = "Source: mtcars")
  tb <- tt_as_tinytable(tt)
  expect_s4_class(tb, "tinytable")
  md <- utils::capture.output(print(tb, output = "markdown"))
  for (cell in c("Adjusted", "Crude", "95% CI", "Reference", "(0.41, 114989.32)", "Observations", "Source: mtcars", "Table 2")) {
    expect_true(any(grepl(cell, md, fixed = TRUE)), label = cell)
  }
  expect_true(any(grepl("_Reference_", md, fixed = TRUE)))
  for (ext in c(".tex", ".html", ".typ", ".md")) {
    f <- withr::local_tempfile(fileext = ext)
    expect_no_error(tinytable::save_tt(tb, f))
    expect_true(file.exists(f))
  }
})

test_that("6.6: table1_tc headers spanning both rows are column names", {
  skip_if_not_installed("tinytable")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  t1 <- table1_tc(d, by = "am", vars = c(wt = "contn", cyl = "cat"))
  md <- utils::capture.output(print(tt_as_tinytable(t1), output = "markdown"))
  hdr <- md[grep("N=19", md, fixed = TRUE)[1]]
  expect_match(hdr, "p-value", fixed = TRUE)
  expect_true(any(grepl("am = 0", md, fixed = TRUE)))
  expect_error(tt_as_tinytable(1), "tt_table")
})

test_that("stack review 6: tables without header rows or value columns", {
  skip_if_not_installed("tinytable")
  nohdr <- puttab(data.frame(a = c("x", "y"), b = 1:2), noheader = TRUE)
  md <- utils::capture.output(print(tt_as_tinytable(nohdr), output = "markdown"))
  expect_true(any(grepl("| x | 1 |", md, fixed = TRUE)))
  lab <- puttab(data.frame(make = c("a", "b", "c")))
  md <- utils::capture.output(print(tt_as_tinytable(lab), output = "markdown"))
  expect_true(any(grepl("make", md, fixed = TRUE)))
  expect_true(any(grepl("| c ", md, fixed = TRUE)))
})


test_that("tt_as_tinytable() keeps $ literal in Markdown output (audit A02)", {
  skip_if_not_installed("tinytable")
  d <- data.frame(g = rep(c("US$ A", "US$ B"), each = 10), x = 1:20)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), labels = c(x = "Cost ($) per visit ($)"),
                   title = "Income ($) and cost ($)", footnote = "in $ and $")
  f <- withr::local_tempfile(fileext = ".md")
  tinytable::save_tt(tt_as_tinytable(tab), f, overwrite = TRUE)
  md <- readLines(f, encoding = "UTF-8")
  # Pandoc (Quarto, R Markdown) reads an unescaped "$" pair as math.
  expect_false(any(grepl("(^|[^\\\\])\\$", md)))
  expect_true(any(grepl("Cost (\\$) per visit (\\$)", md, fixed = TRUE)))
  expect_true(any(grepl("Income (\\$) and cost (\\$)", md, fixed = TRUE)))
  expect_true(any(grepl("in \\$ and \\$", md, fixed = TRUE)))
  expect_true(any(grepl("US\\$ A", md, fixed = TRUE)))
  # HTML output shows the text as written.
  h <- withr::local_tempfile(fileext = ".html")
  tinytable::save_tt(tt_as_tinytable(tab), h, overwrite = TRUE)
  expect_true(any(grepl("Cost ($) per visit ($)", readLines(h, encoding = "UTF-8"), fixed = TRUE)))
})
