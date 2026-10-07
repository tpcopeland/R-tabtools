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
  expect_error(tt_stack(t1, t0), class = "tabtools_error_flat")
  # Truncate the existing lineage with the rows; keep the source identity.
  for (field in c("row_keys", "row_types", "row_blocks")) {
    t0$meta$flat[[field]] <- t0$meta$flat[[field]][0]
  }
  for (field in c("states", "source_blocks")) {
    t0$meta$flat[[field]] <- t0$meta$flat[[field]][0, , drop = FALSE]
  }
  expect_identical(nrow(tt_flat(t0)), 0L)
  s <- tt_stack(t1, t0)
  expect_identical(nrow(s$body), nrow(t1$body))
  s2 <- tt_stack(t1, t0, groups = c("A", "B"))
  expect_identical(nrow(s2$body), nrow(t1$body) + 2L)

  before <- list(t1, t0)
  # Literal two-row model, empty-first, and all-empty boundaries. Explicit
  # headings count as rows even when their source contributes no body rows.
  combinations <- list(list(t1, t0), list(t0, t1), list(t0, t0))
  for (case in seq_along(combinations)) for (grouped in c(FALSE, TRUE)) {
    tables <- combinations[[case]]
    out <- tt_stack(tables, groups = if (grouped) c("A", "B") else NULL)
    flat <- tt_flat(out)
    expect_s3_class(flat, "tt_flat")
    keys <- if (grouped) switch(case,
      c("group:A", "wt", "_cons", "group:B"),
      c("group:A", "group:B", "wt", "_cons"), c("group:A", "group:B")) else
      if (case < 3L) c("wt", "_cons") else character()
    types <- if (grouped) switch(case,
      c("header", "var", "var", "header"),
      c("header", "header", "var", "var"), c("header", "header")) else
      if (case < 3L) c("var", "var") else character()
    values <- if (grouped) switch(case,
      c("A", "wt", "Intercept", "B"), c("A", "B", "wt", "Intercept"), c("A", "B")) else
      if (case < 3L) c("wt", "Intercept") else character()
    nr <- length(keys)
    expected_states <- matrix("", nr, 1L)
    expected_states[types == "var", 1L] <- "est"
    prefix <- out$meta$flat$block_id
    occurrence <- if (case < 3L) paste0(prefix, "/", case, "/", t1$meta$flat$block_id)
    expected_sources <- matrix("", nr, 1L)
    expected_sources[types == "var", 1L] <- if (case < 3L) occurrence else character()
    expected_blocks <- if (grouped) switch(case,
      c(paste0(prefix, "/1/heading"), rep(occurrence, 2L), paste0(prefix, "/2/heading")),
      c(paste0(prefix, "/1/heading"), paste0(prefix, "/2/heading"), rep(occurrence, 2L)),
      paste0(prefix, "/", 1:2, "/heading")) else
      if (case < 3L) rep(occurrence, 2L) else character()
    expect_identical(out$body[[1]], values)
    expect_identical(nrow(out$body), nr)
    expect_identical(out$rows$key, keys)
    expect_identical(flat$`_term`, keys)
    expect_identical(flat$`_rowtype`, types)
    expect_identical(flat$`_order`, seq_len(nr))
    expect_identical(flat$`_state_1`, expected_states[, 1L])
    expect_identical(flat$`_block`, expected_blocks)
    expect_identical(out$meta$flat$row_keys, keys)
    expect_identical(out$meta$flat$row_types, types)
    expect_identical(out$meta$flat$row_blocks, expected_blocks)
    expect_identical(out$meta$flat$states, expected_states)
    expect_identical(out$meta$flat$source_blocks, expected_sources)
    expect_identical(attr(flat, "row_blocks"), expected_blocks)
    expect_identical(attr(flat, "model_states"), expected_states)
    expect_identical(attr(flat, "source_blocks"), expected_sources)
    if (case < 3L) {
      expect_identical(unname(as.matrix(out$body[types == "var", , drop = FALSE])),
                       unname(as.matrix(t1$body)))
    }
  }
  expect_identical(list(t1, t0), before)
})

test_that("tt_stack(groups=) has deterministic keys and passes through tt_merge (CAT P1-12)", {
  t1 <- regtab(lm(mpg ~ wt, mtcars), stats = "n")
  s <- tt_stack(t1, t1, groups = c("A", "B"))
  expect_false(anyNA(s$rows$key))
  expect_identical(s$rows$key[grepl("^group:", s$rows$key)], c("group:A", "group:B"))
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

test_that("tt_merge matches stack headings by label, not position (review 2)", {
  a <- regtab(lm(mpg ~ wt, mtcars), stats = "n")
  b <- regtab(lm(mpg ~ hp, mtcars), stats = "n")
  m <- tt_merge(tt_stack(a, b, groups = c("A", "B")), tt_stack(b, a, groups = c("B", "A")))
  expect_identical(m$rows$key, c("group:A", "wt", "_cons", "stat:n", "group:B", "hp", "_cons", "stat:n"))
  expect_identical(m$body[[1]][c(1, 5)], c("A", "B"))
  # the wt row of A holds a's estimate in the first table and (second table: A = a) the same
  expect_identical(m$body[[2]][2], m$body[[5]][2])
  expect_identical(m$meta$stack_group_rows, c(1L, 5L))
  # repeated labels get an occurrence suffix
  s <- tt_stack(a, a, groups = c("A", "A"))
  expect_identical(s$rows$key[grepl("^group:", s$rows$key)], c("group:A", "group:A#1"))
  expect_identical(nrow(tt_merge(s, s)$body), nrow(s$body))
})

test_that("a user row keyed like a heading is never merged onto one (review 3)", {
  a <- regtab(lm(mpg ~ wt, mtcars), stats = "n")
  u <- a
  u$rows$key[1] <- "group:A"
  expect_error(tt_stack(u, a, groups = c("A", "B")), class = "tabtools_error_flat")
  u$meta$flat$row_keys[1] <- "group:A"
  m <- tt_merge(tt_stack(a, a, groups = c("A", "B")), tt_stack(u, a, groups = c("A", "B")))
  expect_identical(sum(m$rows$key == "group:A"), 2L)
  expect_identical(m$meta$stack_group_rows, c(1L, 6L))
  # an unstacked table with that key is an ordinary row
  expect_no_error(tt_merge(u, a))
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
