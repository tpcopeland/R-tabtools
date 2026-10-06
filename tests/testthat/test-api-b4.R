# B4: small API defects (CAT P1-5, P1-9, P1-12; AUD BUG-2, 7, 8; PAR W03).

test_that("tabtools_options() refuses unnamed arguments and still queries/clears (D4)", {
  op <- options(tabtools.font = getOption("tabtools.font"), tabtools.digits = getOption("tabtools.digits"))
  on.exit(options(op), add = TRUE)
  options(tabtools.font = NULL, tabtools.digits = NULL)
  expect_error(tabtools_options("digits"), class = "tabtools_error_options_unnamed")
  expect_error(tabtools_options("digits"), "tabtools_options\\(\\)\\$digits")
  expect_null(getOption("tabtools.font"))
  expect_error(tabtools_options(bogus = 1), class = "tabtools_error_options_unknown")
  expect_type(tabtools_options(), "list")
  tabtools_options(digits = 3)
  expect_equal(tabtools_options()$digits, 3)
  tabtools_options(clear = TRUE)
  expect_null(getOption("tabtools.digits"))
})

test_that("regtab(sep = ' ') renders (a b) and sep is data (AUD BUG-2)", {
  t <- regtab(lm(mpg ~ wt, mtcars), sep = " ")
  ci <- as.data.frame(t)
  expect_true(any(grepl("^\\(-?[0-9.]+ -?[0-9.]+\\)$", unlist(ci))))
  expect_false(any(grepl("^\\(-?[0-9.]+, -?[0-9.]+\\)$", unlist(ci))))
  expect_error(regtab(lm(mpg ~ wt, mtcars), sep = NA_character_), class = "tabtools_error_sep")
  d <- regtab(lm(mpg ~ wt, mtcars), sep = "")
  expect_true(any(grepl(", ", unlist(as.data.frame(d)), fixed = TRUE)))
  e <- effecttab(data.frame(term = c("a", "b"), estimate = c(0.5, 0.2), conf.low = c(0.1, -0.1), conf.high = c(0.9, 0.5), p.value = c(0.01, 0.2)), sep = " ")
  expect_true(any(grepl("^\\(-?[0-9.]+ -?[0-9.]+\\)$", unlist(as.data.frame(e)))))
})

test_that("tt_stack() accepts a 0-row table (AUD BUG-7)", {
  t1 <- regtab(lm(mpg ~ wt, mtcars)); t0 <- t1
  t0$body <- t0$body[0, , drop = FALSE]; t0$rows <- t0$rows[0, , drop = FALSE]
  t0$meta$pvals <- t0$meta$pvals[0, , drop = FALSE]
  s <- tt_stack(t1, t0)
  expect_identical(nrow(s$body), nrow(t1$body))
  s2 <- tt_stack(t1, t0, groups = c("A", "B"))
  expect_identical(nrow(s2$body), nrow(t1$body) + 2L)
})

test_that("tt_stack(groups=) has deterministic keys and passes through tt_merge (CAT P1-12)", {
  t1 <- regtab(lm(mpg ~ wt, mtcars), stats = "n")
  s <- tt_stack(t1, t1, groups = c("A", "B"))
  expect_false(anyNA(s$rows$key))
  expect_identical(s$rows$key[grepl("^group:", s$rows$key)], c("group:1:A", "group:2:B"))
  expect_identical(s$rows$key, tt_stack(t1, t1, groups = c("A", "B"))$rows$key)
  m <- tt_merge(s, s)
  expect_identical(nrow(m$body), nrow(s$body))
  expect_identical(m$rows$key, s$rows$key)
  expect_identical(m$body[[1]], s$body[[1]])
  # a different stack with another group set: headings stay separate rows
  s3 <- tt_stack(t1, t1, groups = c("A", "C"))
  m3 <- tt_merge(s, s3)
  expect_identical(sum(grepl("^group:", m3$rows$key)), 3L)
  expect_false(any(grepl("\u001f", m3$rows$key)))
})

test_that(".ct_tokens keeps compound quotes with inner double quotes (AUD BUG-8)", {
  expect_identical(tabtools:::.ct_tokens('`"Odds Ratio ("adjusted")"\' other_token'),
                   c('Odds Ratio ("adjusted")', "other_token"))
  expect_identical(tabtools:::.ct_tokens('"a b" c'), c("a b", "c"))
})

test_that("every Stata colour name resolves to Stata's RGB (PAR W03)", {
  expect_identical(tt_parse_color("gs0"), "FF000000")
  expect_identical(tt_parse_color("gs12"), "FFC0C0C0")
  expect_identical(tt_parse_color("emerald"), "FF2D6D66")
  expect_identical(tt_parse_color("ltblue"), "FFADD8E6")
  expect_length(tabtools:::.stata_colors, 59L)
  for (nm in c(paste0("gs", 0:16), "cranberry", "stone", "mint")) expect_no_error(tt_parse_color(nm))
  expect_error(tt_parse_color("chartreuse"), "not a supported Stata colour name")
})

test_that("polr/clm non-logit links are not labelled oprobit (CAT P1-9)", {
  skip_if_not_installed("MASS")
  f <- suppressWarnings(MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing,
                                   method = "cloglog"))
  expect_true(is.na(tt_model_info(f)$stata_cmd))
  t <- regtab(f)
  expect_false(grepl("probit", t$stored$methods))
  expect_match(t$stored$methods, "ordinal regression (cloglog link)", fixed = TRUE)
  expect_match(t$footnote, "cloglog link scale")
  p <- suppressWarnings(MASS::polr(Sat ~ Infl, weights = Freq, data = MASS::housing, method = "probit"))
  expect_identical(tt_model_info(p)$stata_cmd, "oprobit")
  expect_match(regtab(p)$stored$methods, "ordered probit")
  skip_if_not_installed("ordinal")
  cl <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing, link = "cloglog")
  expect_true(is.na(tt_model_info(cl)$stata_cmd))
  expect_match(regtab(cl)$footnote, "cloglog link scale")
})
