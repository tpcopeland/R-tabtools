# table1_tc engine units: vars parsing (2.2), summaries (2.5), cell text
# (2.6), layout (2.7), tests (2.8), SMD (2.9), stored results and sinks
# (2.10, 2.11).

test_that("vars accepts Stata strings, named vectors, lists, and bare names", {
  p <- tabtools:::.t1_parse_vars
  s <- p("age contn %5.1f %5.2f extra \\ sex bin \\ \\ edu")
  expect_identical(vapply(s, `[[`, "", "name"), c("age", "sex", "edu"))
  expect_identical(vapply(s, `[[`, "", "type"), c("contn", "bin", "auto"))
  expect_identical(s[[1]]$fmt1, "%5.1f")
  expect_identical(s[[1]]$fmt2, "%5.2f")
  expect_true(is.na(s[[2]]$fmt1))
  n <- p(c(age = "contn %5.1f", sex = "", "edu cat"))
  expect_identical(vapply(n, `[[`, "", "type"), c("contn", "auto", "cat"))
  l <- p(list(age = "conts", "sex"))
  expect_identical(vapply(l, `[[`, "", "name"), c("age", "sex"))
  expect_identical(vapply(p(c("a", "b auto")), `[[`, "", "type"), c("auto", "auto"))
  expect_error(p(c(age = "contn \\ sex bin")), "may not contain")
  expect_error(p(list(age = 1)), "single string")
})

test_that("quantiles follow Stata's cumulative-count rule", {
  q <- tabtools:::.t1_quantile
  expect_identical(q(c(1, 2, 3, 4), 0.5), 2.5)      # exact n*p: average
  expect_identical(q(c(1, 2, 3, 4, 5), 0.5), 3)
  expect_identical(q(c(1, 2, 3, 4), 0.25), 1.5)
  expect_identical(q(c(1, 2, 3, 4, 5), 0.25), 2)
  expect_identical(q(5, 0.75), 5)
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  for (p in c(0.25, 0.5, 0.75)) expect_identical(q(x, p), unname(stats::quantile(x, p, type = 2)))
})

test_that("contln summarises positive values and counts the rest as missing", {
  s <- tabtools:::.t1_cont_stats(c(1, exp(1), exp(2), 0, -1, NA), "contln")
  expect_identical(s$n, 3L)
  expect_equal(s$a, exp(1))
  expect_equal(s$b, exp(1))
})

test_that("string by() uses encode (C-locale) order; factors keep level order", {
  a <- golden_fixture("auto")
  tt <- table1_tc(a, by = "size_class", vars = "price contn")
  expect_identical(tt$header[[1]]$text[2:4], c("large", "medium", "small"))
  a$sz <- factor(a$size_class, levels = c("small", "medium", "large"))
  tt <- table1_tc(a, by = "sz", vars = "price contn")
  expect_identical(tt$header[[1]]$text[2:4], c("small", "medium", "large"))
  a$w <- factor(ifelse(a$weight < 3000, "light", "heavy"), levels = c("light", "heavy"))
  tt <- table1_tc(a, by = "foreign", vars = "w cat")
  expect_identical(tt$body[2:3, 1], c("   light", "   heavy"))
})

test_that("missing by() rows leave the analysis sample", {
  a <- golden_fixture("auto")
  tt <- table1_tc(a, by = "rep78", vars = "price contn")
  expect_identical(tt$header[[2]]$text[2], "N=2")
  expect_identical(sum(as.numeric(sub("N=", "", tt$header[[2]]$text[-c(1, 7)]))), 69)
})

test_that("R tests follow the plan's test map, with test_args", {
  a <- golden_fixture("auto")
  tt <- table1_tc(a, by = "foreign", vars = "price contn \\ mpg conts \\ rep78 cat \\ rep78 cate",
                  test = TRUE)
  tests <- tt$body[tt$rows$type %in% c("var", "cat_header"), which(tt$cols$role == "test")]
  expect_identical(tests, c("Welch t test", "Wilcoxon rank-sum", "Pearson's chi-squared", "Fisher's exact"))
  welch <- stats::t.test(a$price[a$foreign == 0], a$price[a$foreign == 1])$p.value
  expect_equal(unname(tt$stored$table[1, "p_value"]), welch)
  pooled <- table1_tc(a, by = "foreign", vars = "price contn", test_args = list(t.test = list(var.equal = TRUE)))
  expect_equal(unname(pooled$stored$table[1, "p_value"]),
               stats::t.test(a$price[a$foreign == 0], a$price[a$foreign == 1], var.equal = TRUE)$p.value)
  expect_match(pooled$stored$methods, "Student's t-test")
  chi <- suppressWarnings(stats::chisq.test(table(a$rep78, a$foreign), correct = FALSE))$p.value
  expect_equal(unname(tt$stored$table[3, "p_value"]), chi)
  three <- table1_tc(a, by = "rep78", vars = "price contn \\ mpg conts")
  expect_match(three$stored$methods, "Welch's one-way ANOVA, Kruskal-Wallis test")
  expect_false(grepl("Stata", three$stored$methods))
})

test_that("Fisher's simulated fallback leaves the session RNG untouched", {
  set.seed(1)
  before <- .Random.seed
  tab <- matrix(c(3, 1, 1, 3), 2)
  res <- tabtools:::.t1_fisher(tab, list(fisher.test = list(workspace = 1)))
  expect_true(is.null(res) || is.numeric(res$p.value))
  expect_identical(.Random.seed, before)
})

test_that("categorical SMD is missing for a singular pooled covariance", {
  smd <- tabtools:::.t1_cat_smd
  expect_true(is.na(smd(c(0, 0.5), c(0, 0.5))))
  expect_equal(smd(c(0.5), c(0.25)), abs(0.25) / sqrt((0.25 + 0.1875) / 2))
})

test_that("labels override variable labels; varlabplus appends the type", {
  a <- golden_fixture("auto")
  tt <- table1_tc(a, by = "foreign", vars = "price contn \\ rep78 cat", labels = c(price = "Cost"),
                  varlabplus = TRUE)
  expect_identical(tt$body[1:2, 1], c("Cost, mean\u00b1SD", "Repair record 1978, No. (%)"))
})

test_that("sinks write files and stored paths; the result is invisible", {
  a <- golden_fixture("auto")
  dir <- withr::local_tempdir()
  x <- file.path(dir, "t.xlsx")
  m <- file.path(dir, "t.md")
  cf <- file.path(dir, "t.csv")
  expect_invisible(tt <- table1_tc(a, by = "foreign", vars = "price contn \\ rep78 cat", xlsx = x,
                                   sheet = "S1", markdown = m, csv = cf, title = "T"))
  expect_true(all(file.exists(c(x, m, cf))))
  expect_identical(tt$stored[c("xlsx", "sheet", "markdown")], list(xlsx = x, sheet = "S1", markdown = m))
  expect_identical(tt$stored$markdown_rows, 7L)
  expect_identical(tt$stored$markdown_cols, 4L)
  expect_visible(table1_tc(a, by = "foreign", vars = "price contn"))
  # An existing Markdown file is replaced (Stata 2.1.12), or appended to.
  expect_no_error(table1_tc(a, by = "foreign", vars = "price contn", markdown = m))
  n1 <- length(readLines(m))
  expect_no_error(table1_tc(a, by = "foreign", vars = "price contn", markdown = m, mdappend = TRUE))
  expect_identical(length(readLines(m)), 2L * n1 + 1L)
})

test_that("a group with no data gives blank cells and no test", {
  a <- golden_fixture("auto")
  a$dom_only <- ifelse(a$foreign == 0, a$mpg, NA)
  tt <- table1_tc(a, by = "foreign", vars = "dom_only contn", test = TRUE)
  expect_identical(tt$body[1, 3], "")
  expect_identical(unlist(tt$body[1, which(tt$cols$role %in% c("test", "p"))], use.names = FALSE), c("", ""))
})
