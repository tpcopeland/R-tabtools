# tt_merge() and tt_stack() (task 7.14): tables composed on row keys (task
# 6.7), never on labels.

tc_data <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d
}

test_that("7.14: tt_merge joins on keys; a later table's new row follows its predecessor", {
  d <- tc_data()
  crude <- regtab(lm(mpg ~ cyl, d), stats = "n")
  adj <- regtab(lm(mpg ~ cyl + wt + hp, d), stats = "n")
  m <- tt_merge(Crude = crude, Adjusted = adj)
  expect_identical(m$rows$key, c("cyl", "4.cyl", "6.cyl", "8.cyl", "wt", "hp", "_cons", "stat:n"))
  expect_identical(m$header[[1]]$text, c("", "Crude", "", "", "Adjusted", "", ""))
  expect_identical(length(m$header), 2L)
  expect_identical(m$cols$model, c(NA, 1L, 1L, 1L, 2L, 2L, 2L))
  # Cells are each table's own; a row a table lacks is blank there.
  expect_identical(unname(unlist(m$body[m$rows$key == "6.cyl", 2:4])), unname(unlist(crude$body[crude$rows$key == "6.cyl", 2:4])))
  expect_identical(unname(unlist(m$body[m$rows$key == "wt", 2:7])), c(rep("", 3), unname(unlist(adj$body[adj$rows$key == "wt", 2:4]))))
  expect_identical(m$body[[1]], c("cyl", "  4", "  6", "  8", "wt", "hp", "Intercept", "Observations"))
  expect_identical(names(m$stored$tables), c("Crude", "Adjusted"))
  # Every sink renders it.
  p <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_error(tt_write_xlsx(m, p))
  expect_no_error(tt_write_markdown(m, withr::local_tempfile(fileext = ".md")))
  expect_no_error(format(m))
})

test_that("7.14: tt_merge never joins on labels: rows with one label but different keys stay apart", {
  d <- tc_data()
  a <- regtab(lm(mpg ~ wt, d))
  b <- regtab(lm(mpg ~ hp, d), coef = "Coef.")
  b$body[[1]][b$rows$key == "hp"] <- "wt"
  m <- tt_merge(a, b)
  expect_identical(m$rows$key, c("wt", "hp", "_cons"))
  expect_identical(sum(m$body[[1]] == "wt"), 2L)
})

test_that("7.14: a new level follows its sibling; a new variable goes after the earlier variables", {
  d <- tc_data()
  d$g <- factor(ifelse(d$gear == 5, "c", ifelse(d$gear == 4, "b", "a")))
  a <- regtab(lm(mpg ~ g + wt, d[d$g != "c", ]))
  b <- regtab(lm(mpg ~ g + hp, d))
  m <- tt_merge(a, b)
  expect_identical(m$rows$key, c("g", "1.g", "2.g", "3.g", "wt", "hp", "_cons"))
})

test_that("7.14: spanners over multi-model tables go into the model-label row; p-values follow the rows", {
  d <- tc_data()
  two <- regtab(lm(mpg ~ cyl, d), lm(mpg ~ cyl + wt, d), models = c("A", "B"))
  one <- regtab(lm(mpg ~ cyl + hp, d), models = "C")
  m <- tt_merge(two, one, spanners = c("Nested", "Other"))
  # Stack review item 2: no third header row the workbook would drop.
  expect_identical(length(m$header), 2L)
  expect_identical(m$header[[1]]$text[c(2, 5, 8)], c("Nested: A", "Nested: B", "Other"))
  expect_identical(max(m$cols$model, na.rm = TRUE), 3L)
  mb <- tt_merge(regtab(lm(mpg ~ wt, d), boldp = 0.05), regtab(lm(mpg ~ hp + wt, d), boldp = 0.05))
  expect_identical(dim(as.matrix(mb$meta$pvals %||% matrix(0, nrow(mb$body), 2))), c(nrow(mb$body), 2L))
})

test_that("7.14: tt_merge refuses keyless, duplicate-key and mixed-command tables", {
  d <- tc_data()
  r <- regtab(lm(mpg ~ wt, d))
  t1 <- table1_tc(d, vars = c(wt = "contn", cyl = "cat"))
  expect_error(tt_merge(r, t1), "one command")
  expect_error(tt_merge(t1, t1), "table1_tc")
  p <- puttab(data.frame(a = c("x", "y"), b = 1:2))
  expect_error(tt_merge(p, p), "without a key")
  expect_error(tt_merge(r), "two or more")
  expect_error(tt_merge(r, 1), "not a <tt_table>")
  expect_error(tt_merge(r, r, spanners = "one"), "one label per table")
})

test_that("7.14: tt_stack puts row groups under one header", {
  d <- tc_data()
  a <- regtab(lm(mpg ~ cyl + wt, d[d$am == 0, ]), stats = "n")
  b <- regtab(lm(mpg ~ cyl + wt, d[d$am == 1, ]), stats = "n")
  s <- tt_stack(Automatic = a, Manual = b)
  expect_identical(nrow(s$body), nrow(a$body) + nrow(b$body) + 2L)
  expect_identical(s$body[[1]][c(1, nrow(a$body) + 2L)], c("Automatic", "Manual"))
  expect_identical(s$header, a$header)
  expect_identical(s$rows$key[-c(1, nrow(a$body) + 2L)], c(a$rows$key, b$rows$key))
  expect_no_error(tt_write_xlsx(s, withr::local_tempfile(fileext = ".xlsx")))
  plain <- tt_stack(a, b)
  expect_identical(nrow(plain$body), nrow(a$body) + nrow(b$body))
  expect_error(tt_stack(a, regtab(lm(mpg ~ wt, d), lm(mpg ~ hp, d))), "table 1's columns")
  expect_error(tt_stack(a, b, groups = "x"), "one label per table")
})

# Stack review items 1-5 and 8.

tc_cells <- function(path) {
  golden_cell_styles(path, tidyxl::xlsx_sheet_names(path)[1])
}
tc_merges <- function(path) {
  sort(golden_sheet_layout(path, tidyxl::xlsx_sheet_names(path)[1])$merges)
}

test_that("stack review 1: tt_merge refuses models with different columns; alike ones keep per-model merges", {
  d <- tc_data()
  a <- regtab(lm(mpg ~ cyl, d), stats = "n", compact = TRUE, boldp = 0.05)
  b <- regtab(lm(mpg ~ cyl + wt, d), stats = "n")
  expect_error(tt_merge(a, b), "different columns")
  expect_error(tt_merge(regtab(lm(mpg ~ cyl, d), nopvalue = TRUE), b), "compact.*nopvalue")
  # Both compact: two columns per model, merged and bolded per model.
  skip_if_not_installed("tidyxl")
  m <- tt_merge(a, regtab(lm(mpg ~ cyl + wt, d), stats = "n", compact = TRUE))
  p <- withr::local_tempfile(fileext = ".xlsx")
  tt_write_xlsx(m, p)
  mg <- tc_merges(p)
  expect_true(all(c("C2:D2", "E2:F2", "C5:D5", "E5:F5", "C10:D10", "E10:F10") %in% mg))
  w <- tc_cells(p)
  bold <- w$address[w$row >= 4 & !is.na(w$bold) & w$bold & nzchar(w$value)]
  # Bold p cells only: column D (model 1's p) and F (model 2's), never a CI.
  expect_true(length(bold) > 0)
  expect_true(all(substr(bold, 1, 1) %in% c("D", "F")))
  expect_true(all(grepl("^<?[0-9.]+$", w$value[match(bold, w$address)])))
})

test_that("stack review 2: spanners over a multi-model table reach every sink", {
  d <- tc_data()
  a <- regtab(lm(mpg ~ cyl, d), stats = "n")
  b <- regtab(list(M1 = lm(mpg ~ cyl + wt, d), M2 = lm(mpg ~ cyl + hp, d)), stats = "n")
  m <- tt_merge(Crude = a, Adjusted = b)
  expect_identical(m$header[[1]]$text, c("", "Crude", "", "", "Adjusted: M1", "", "", "Adjusted: M2", "", ""))
  expect_match(paste(format(m), collapse = "\n"), "Adjusted: M2", fixed = TRUE)
  csv <- withr::local_tempfile(fileext = ".csv")
  tt_write_csv(m, csv)
  expect_identical(readLines(csv)[1], ",Crude,,,Adjusted: M1,,,Adjusted: M2,,")
  md <- withr::local_tempfile(fileext = ".md")
  tt_write_markdown(m, md)
  expect_match(readLines(md)[1], "Adjusted: M1: Coef.", fixed = TRUE)
  expect_match(readLines(md)[1], "Adjusted: M2: Coef.", fixed = TRUE)
  skip_if_not_installed("tidyxl")
  p <- withr::local_tempfile(fileext = ".xlsx")
  tt_write_xlsx(m, p)
  w <- tc_cells(p)
  expect_identical(w$value[match(c("C2", "F2", "I2", "C3", "F3", "I3"), w$address)],
                   c("Crude", "Adjusted: M1", "Adjusted: M2", "Coef.", "Coef.", "Coef."))
  expect_true(all(c("C2:E2", "F2:H2", "I2:K2") %in% tc_merges(p)))
})

test_that("stack review 3: the workbook footnote keeps the stars note after tt_merge and tt_stack", {
  skip_if_not_installed("tidyxl")
  d <- tc_data()
  a <- regtab(lm(mpg ~ cyl, d[d$am == 0, ]), stats = "n", stars = TRUE, footnote = "Crude")
  b <- regtab(lm(mpg ~ cyl, d[d$am == 1, ]), stats = "n", stars = TRUE)
  stars <- "* p<0.05, ** p<0.01, *** p<0.001"
  foot <- function(x) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    tt_write_xlsx(x, p)
    w <- tc_cells(p)
    w$value[w$row == max(w$row) & w$col == 2]
  }
  expect_identical(foot(tt_merge(a, b)), stars)
  expect_identical(foot(tt_stack(a, b)), stars)
  expect_identical(foot(tt_merge(b, a, footnote = "Mine.")), stars)
  expect_identical(foot(tt_stack(b, a)), stars)
  # The public footnote carries the same separate automatic legend.
  expect_identical(tt_merge(a, b)$footnote, paste("Crude", "\\", stars))
  # No stars: the workbook writes the footnote.
  n <- tt_merge(regtab(lm(mpg ~ wt, d), footnote = "Plain"), regtab(lm(mpg ~ hp, d)))
  expect_null(n$meta$xlsx_footnote)
  expect_identical(foot(n), "Plain")
})

test_that("stack review 4: a row is dimmed only when every table dims it; p is the smallest; frame counts every model", {
  d <- mtcars
  a <- regtab(lm(mpg ~ qsec, d[1:12, ]), dimnonsig = TRUE)
  b <- regtab(lm(mpg ~ qsec, d), dimnonsig = TRUE)
  expect_identical(a$rows$dim, c(TRUE, TRUE))
  expect_identical(b$rows$dim, c(FALSE, TRUE))
  m <- tt_merge(a, b)
  expect_identical(m$rows$dim, c(FALSE, TRUE))
  expect_identical(m$rows$p, pmin(a$rows$p, b$rows$p))
  two <- regtab(lm(mpg ~ wt, d), lm(mpg ~ wt + hp, d), models = c("A", "B"))
  m3 <- tt_merge(X = two, Y = regtab(lm(mpg ~ hp, d), models = "C"))
  fm <- m3$meta$frame
  expect_identical(fm$n_models, 3)
  expect_identical(fm$model_label, c("X: A", "X: B", "Y"))
  expect_length(fm$effect_scale, 3L)
  expect_identical(attr(as.data.frame(m3), "n_models"), 3)
})

test_that("stack review 5: addrow and stat_fun rows have their own keys, so tables with them merge", {
  d <- tc_data()
  t1 <- regtab(lm(mpg ~ wt, d), addrow = list(Adjusted = "No"), stats = "n")
  t2 <- regtab(lm(mpg ~ wt + cyl, d), addrow = list(Adjusted = "Yes"), stats = "n")
  m <- tt_merge(t1, t2)
  i <- which(m$rows$key == "addrow:Adjusted")
  expect_length(i, 1L)
  expect_identical(unname(unlist(m$body[i, c(1, 2, 5)])), c("Adjusted", "No", "Yes"))
  s <- regtab(lm(mpg ~ wt, d), stat_fun = list(n = function(f) 1), stats = "n")
  expect_identical(s$rows$key[s$rows$type == "stat"], c("stat:n", "statfun:n"))
  expect_no_error(tt_merge(s, s))
})

test_that("stack review 8: table1_tc tables, keyed by label, are refused even when their keys are unique", {
  d <- tc_data()
  t1 <- table1_tc(d, vars = c(wt = "contn", mpg = "contn"))
  expect_false(anyDuplicated(t1$rows$key) > 0)
  expect_error(tt_merge(t1, t1), "variables' labels")
})
