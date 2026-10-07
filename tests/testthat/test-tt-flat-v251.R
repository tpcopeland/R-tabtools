# G04/E01: analytical identity and projected provenance. These regressions
# cover the five source-review failures and the reserved-name follow-up.

test_that("fitted references, omissions, absent terms and statistic keys come from source rows", {
  tab <- wp2d_factor_table()
  flat <- tt_flat(tab)
  expect_s3_class(flat, "tt_flat")
  expect_s3_class(flat, "data.frame")
  expect_identical(flat$`_order`, seq_len(nrow(tab$body)))
  expect_identical(flat$`_term`, tab$rows$key)
  expect_true(all(c("wt", "factor(cyl)", "4.factor(cyl)", "6.factor(cyl)",
                    "8.factor(cyl)", "stat:n") %in% flat$`_term`))
  expect_identical(flat$`_state_1`[flat$`_term` == "4.factor(cyl)"], "ref")
  expect_identical(flat$`_state_1`[flat$`_term` == "wt"], "est")
  structural <- flat$`_rowtype` %in% c("cat_header", "stat")
  expect_true(all(flat$`_state_1`[structural] == ""))
  expect_identical(flat$`_rowtype`[flat$`_term` == "stat:n"], "stat")
  alias <- regtab(lm(mpg ~ wt + duplicate, transform(mtcars, duplicate = wt)))
  expect_identical(tt_flat(alias)$`_state_1`[alias$rows$key == "duplicate"], "omit")
  two <- tt_flat(regtab(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars), models = c("Weight", "Power")))
  expect_identical(two$`_state_2`[two$`_term` == "wt"], "absent")
  expect_identical(two$`_state_1`[two$`_term` == "hp"], "absent")
  expect_identical(attr(two, "model_names"), c("Weight", "Power"))
  expect_identical(attr(flat, "sample_accounting"), tab$meta$sample_accounting)
})

test_that("equation keys and duplicate effect matrix names retain raw source identities", {
  d <- wp2d_numeric_rows()
  d <- rbind(d, d)
  d$term <- "wt"
  d$equation <- c("4", "6")
  attr(d, "conf.level") <- 0.95
  attr(d, "effect_scale") <- "Coef."
  tab <- regtab(d)
  expect_identical(tt_flat(tab)$`_term`, c("4::wt", "6::wt"))
  m <- matrix(c(0.1, 0, 0.2, 0.04, 0.2, 0.1, 0.3, 0.01), 2L, byrow = TRUE,
              dimnames = list(c("A", "A"), NULL))
  e <- effecttab(m, addrow = list(N = "12"))
  ef <- tt_flat(e)
  expect_identical(ef$`_term`, c("A", "A#1", "addrow:N"))
  expect_identical(ef$`_rowtype`, c("var", "var", "addrow"))
  expect_identical(ef$`_state_1`, c("est", "est", ""))
  expect_identical(names(ef)[seq_len(4L)], c("rowlabel", "Estimate", "95% CI", "p-value"))
  supplied <- data.frame(term = c("age", "o.age2", "3.race"), estimate = c(0.1, 0, NA),
                         conf.low = c(0.05, NA, NA), conf.high = c(0.15, NA, NA),
                         p.value = c(0.01, NA, NA), status = c("est", "omit", "empty"))
  s <- tt_flat(effecttab(supplied, level = 95))
  expect_identical(s$`_state_1`, c("est", "omit", "", "empty"))
  expect_identical(s$`_rowtype`, c("var", "var", "cat_header", "empty"))
})

test_that("source row alignment is checked before group scoping and stack assembly", {
  tab <- wp2d_factor_table()
  swapped <- tab
  order <- c(3L, 1L, 2L, seq.int(4L, nrow(tab$body)))
  swapped$body <- swapped$body[order, , drop = FALSE]
  swapped$rows <- swapped$rows[order, , drop = FALSE]
  expect_error(tt_flat(swapped), class = "tabtools_error_flat")
  expect_error(tt_merge(swapped, tab), class = "tabtools_error_flat")
  expect_error(tt_stack(swapped, tab, groups = c("First", "Second")), class = "tabtools_error_flat")
  for (kind in c("missing", "reversed", "types")) {
    bad <- tab
    if (kind == "missing") bad$meta$flat$row_keys <- NULL
    if (kind == "reversed") bad$meta$flat$row_keys <- rev(bad$meta$flat$row_keys)
    if (kind == "types") bad$meta$flat$row_types[1] <- "level"
    expect_error(tt_merge(bad, tab), class = "tabtools_error_flat")
    expect_error(tt_stack(bad, tab), class = "tabtools_error_flat")
  }
  grouped <- tt_stack(tab, tab, groups = c("A", "B"))
  expect_no_error(tt_flat(tt_merge(grouped, grouped)))
})

test_that("analytical states are canonical source provenance after technical projection", {
  tab <- wp2d_factor_table()
  wt <- match("wt", tab$rows$key)
  for (state in c("est", "ref", "omit", "notest", "absent", "empty", "constrained", "masked")) {
    source <- tab
    source$meta$flat$states[wt, 1L] <- state
    # Deliberately unchanged publication text: the selector reads metadata.
    flat <- tt_flat(source)
    expect_identical(flat$`_state_1`[wt], state)
    expect_identical(flat[[2]][wt], tab$body[[2]][wt])
    expect_no_error(tabtools:::.tt_validate_flat(flat[, "rowlabel", drop = FALSE]))
  }
  for (state in c("", "note")) {
    bad <- tab
    bad$meta$flat$states[wt, 1L] <- state
    expect_error(tt_flat(bad), class = "tabtools_error_flat")
    bad <- tt_flat(tab)
    bad$`_state_1`[wt] <- state
    expect_error(bad[, "rowlabel", drop = FALSE], class = "tabtools_error_flat")
    bad <- tt_flat(tab, keyed = FALSE)
    attr(bad, "model_states")[wt, 1L] <- state
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  flat <- tt_flat(tab)
  stat <- which(flat$`_rowtype` == "stat")
  for (keyed in c(FALSE, TRUE)) {
    bad <- tt_flat(tab, keyed = keyed)
    attr(bad, "model_states")[stat, 1L] <- "est"
    if (keyed) bad$`_state_1`[stat] <- "est"
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  bad <- flat
  bad$`_state_1`[wt] <- "ref"
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  for (field in c("row_types", "model_states", "row_blocks", "source_blocks")) {
    bad <- flat[, "rowlabel", drop = FALSE]
    attr(bad, field) <- NULL
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  bad <- flat[, "rowlabel", drop = FALSE]
  attr(bad, "row_types")[wt] <- "invented"
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  flat[[2]][wt] <- "See text"
  expect_no_error(tabtools:::.tt_validate_flat(flat))
  expect_identical(flat$`_state_1`[wt], "est")
})

test_that("existing heading flags remain structural through repeated and nested composition", {
  tab <- wp2d_factor_table()
  stack <- tt_stack(tab, tab, groups = c("A", "A"))
  flat <- tt_flat(stack)
  expect_identical(flat$`_term`[flat$`_rowtype` == "header"], c("group:A", "group:A#1"))
  expect_true(all(flat$`_state_1`[flat$`_rowtype` == "header"] == ""))
  expect_identical(which(flat$`_rowtype` == "header"), stack$meta$stack_group_rows)
  nested <- tt_flat(tt_stack(stack, stack, groups = c("Outer", "Again")))
  expect_identical(sum(nested$`_rowtype` == "header"), 6L)
  expect_identical(length(unique(nested$`_block`[nested$`_term` == "wt"])), 4L)
  projected <- nested[, c("_term", "_rowtype", "_state_1"), drop = FALSE]
  expect_no_error(tabtools:::.tt_validate_flat(projected))
  expect_identical(attr(projected, "row_blocks"), nested$`_block`)
  expect_identical(sum(projected$`_rowtype` == "header"), 6L)
  merged <- tt_flat(tt_merge(stack, stack))
  ref <- merged$`_term` == "4.factor(cyl)"
  expect_identical(merged$`_state_1`[ref], c("ref", "ref"))
  expect_identical(merged$`_state_2`[ref], c("ref", "ref"))
  expect_identical(sum(merged$`_rowtype` == "header"), 2L)
  refs <- merged[ref, c("_term", "_state_2"), drop = FALSE]
  expect_no_error(tabtools:::.tt_validate_flat(refs))
  expect_identical(length(unique(attr(refs, "row_blocks"))), 2L)
  expect_true(all(nzchar(attr(merged, "source_blocks")[ref, ])))
  expect_false(any(attr(merged, "source_blocks")[ref, 1L] == attr(merged, "source_blocks")[ref, 2L]))
  # An analytical key merely spelled like a group is not a heading flag.
  d <- wp2d_numeric_rows()
  d$term <- "group:user"
  ordinary <- tt_flat(regtab(d))
  expect_identical(ordinary$`_rowtype`, "var")
  expect_identical(ordinary$`_state_1`, "est")
  e <- effecttab(as.matrix(wp2d_numeric_rows()[, 2:5]), addrow = list(N = "12"))
  ef <- tt_flat(tt_stack(e, e, groups = c("A", "B")))
  expect_identical(sum(ef$`_rowtype` == "header"), 2L)
  expect_identical(sum(ef$`_rowtype` == "addrow"), 2L)
  expect_true(all(ef$`_state_1`[ef$`_rowtype` %in% c("header", "addrow")] == ""))
})

test_that("nonempty analytical provenance and row identities are mandatory with explicit exceptions", {
  tab <- wp2d_factor_table()
  flat <- tt_flat(tab)
  wt <- match("wt", flat$`_term`)
  for (value in c("", NA_character_)) {
    bad <- flat
    attr(bad, "source_blocks")[wt, 1L] <- value
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
    bad <- tab
    bad$meta$flat$row_blocks[wt] <- value
    expect_error(tt_stack(bad, tab), class = "tabtools_error_flat")
    bad <- flat[, "rowlabel", drop = FALSE]
    attr(bad, "row_blocks")[wt] <- value
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
    bad <- flat
    bad$`_term`[wt] <- value
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  for (row in which(flat$`_rowtype` %in% c("cat_header", "stat"))) {
    bad <- flat
    bad$`_term`[row] <- ""
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  structural <- flat$`_rowtype` %in% c("cat_header", "stat")
  attr(flat, "source_blocks")[structural, ] <- ""
  expect_no_error(tabtools:::.tt_validate_flat(flat))
  absent <- tab
  absent$meta$flat$states[wt, 1L] <- "absent"
  absent$meta$flat$source_blocks[wt, 1L] <- ""
  expect_no_error(tt_flat(absent))
  heading <- tt_flat(tt_stack(tab, tab, groups = c("A", "B")))
  heading$`_term`[heading$`_rowtype` == "header"] <- ""
  expect_no_error(tabtools:::.tt_validate_flat(heading))
  e <- tt_flat(effecttab(as.matrix(wp2d_numeric_rows()[, 2:5]), addrow = list(N = "12")))
  e$`_term`[e$`_rowtype` == "addrow"] <- ""
  expect_error(tabtools:::.tt_validate_flat(e), class = "tabtools_error_flat")
})

test_that("CI-only and reversed projections keep original model indices and all-model attributes", {
  tab <- regtab(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars), models = c("Weight", "Power"))
  flat <- tt_flat(tab)
  take <- c("p-value.1", "95% CI.1", "95% CI", "_state_2")
  x <- flat[c(2L, 1L), take, drop = FALSE]
  expect_identical(names(x), take)
  expect_identical(attr(x, "model_names"), c("Weight", "Power"))
  expect_identical(attr(x, "column_model"), c("p-value.1" = 2L, "95% CI.1" = 2L, "95% CI" = 1L))
  expect_identical(attr(x, "source_blocks"), attr(flat, "source_blocks")[c(2L, 1L), , drop = FALSE])
  expect_identical(attr(x, "model_states"), attr(flat, "model_states")[c(2L, 1L), , drop = FALSE])
  expect_identical(x$`_state_2`, flat$`_state_2`[c(2L, 1L)])
  expect_identical(attr(x, "block_id"), attr(flat, "block_id"))
  expect_identical(attr(x, "source_block_id"), attr(flat, "source_block_id"))
  expect_true(all(vapply(attr(x, "full_header"), function(h) length(h$text) == 3L, TRUE)))
  expect_identical(unname(attr(x, "model_names")[attr(x, "column_model")]), c("Power", "Power", "Weight"))
  expect_identical(names(flat[c("95% CI.1", "_state_2")]), c("95% CI.1", "_state_2"))
  other_state <- flat[, c("95% CI.1", "_state_1"), drop = FALSE]
  expect_identical(other_state$`_state_1`, flat$`_state_1`)
  expect_identical(unname(attr(other_state, "column_model")), 2L)
  expect_no_error(tabtools:::.tt_validate_flat(other_state))
  expect_identical(flat[, "rowlabel", drop = TRUE], flat$rowlabel)
  expect_no_error(flat[integer(), , drop = FALSE])
  expect_no_error(flat[, integer(), drop = FALSE])
  empty <- flat[c(2L, 1L), integer(), drop = FALSE]
  expect_identical(attr(empty, "row_types"), attr(flat, "row_types")[c(2L, 1L)])
  expect_no_error(tabtools:::.tt_validate_flat(empty))
  expect_error(flat[c(1L, 1L), , drop = FALSE], class = "tabtools_error_flat")
  expect_error(flat[nrow(flat) + 1L, , drop = FALSE], class = "tabtools_error_flat")
  public <- tt_flat(tab, keyed = FALSE)
  expect_false(any(startsWith(names(public), "_")))
  expect_identical(attr(public, "model_states"), attr(flat, "model_states"))
  expect_identical(attr(public, "source_blocks"), attr(flat, "source_blocks"))
  plain <- data.frame(lapply(flat, identity), check.names = FALSE)
  expect_identical(class(plain), "data.frame")
  expect_setequal(names(attributes(plain)), c("names", "row.names", "class"))
  expect_error(tabtools:::.tt_validate_flat(plain), class = "tabtools_error_flat")
  stripped_class <- flat
  class(stripped_class) <- "data.frame"
  expect_error(tabtools:::.tt_validate_flat(stripped_class), class = "tabtools_error_flat")
})

test_that("malformed headers, spans and identities fail classedly, including zero-row endpoints", {
  flat <- tt_flat(wp2d_factor_table())
  values <- list(character = "two", list = I(list(2)), logical = TRUE,
                 factor = factor("2"), complex = 2 + 0i, missing = NA_real_,
                 infinite = Inf, fractional = 1.5, below = 0, above = 99)
  for (name in names(values)) {
    for (empty in if (name %in% c("character", "list", "logical", "factor", "complex")) c(FALSE, TRUE) else FALSE) {
      bad <- flat
      v <- values[[name]]
      if (empty) v <- v[FALSE]
      header <- attr(bad, "full_header")
      header[[1L]]$spans <- data.frame(from = v, to = v)
      attr(bad, "full_header") <- header
      expect_error(bad[, "rowlabel", drop = FALSE], class = "tabtools_error_flat")
    }
  }
  for (v in list(integer(), numeric())) {
    good <- flat
    h <- attr(good, "full_header")
    h[[1L]]$spans <- data.frame(from = v, to = v)
    attr(good, "full_header") <- h
    expect_no_error(tabtools:::.tt_validate_flat(good))
  }
  for (field in c("block_id", "source_block_id", "model_names", "full_header", "column_model")) {
    bad <- flat
    attr(bad, field) <- NULL
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  for (value in list(NA_character_, list("Model"), 1)) {
    bad <- flat
    attr(bad, "model_names") <- value
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  for (field in c("model_states", "source_blocks")) {
    bad <- flat
    attr(bad, field) <- matrix(0, nrow(flat), 1L)
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
    bad <- flat
    attr(bad, field) <- matrix("est", nrow(flat), 2L)
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  for (value in c("", NA_character_)) {
    bad <- flat
    attr(bad, "source_block_id") <- value
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  }
  bad <- flat
  h <- attr(bad, "full_header")
  h[[1L]]$text <- "short"
  attr(bad, "full_header") <- h
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  bad <- flat
  attr(bad, "column_model")[2L] <- 2L
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  bad <- flat
  bad$`_order` <- as.numeric(bad$`_order`)
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  bad <- flat
  bad$`_rowtype`[1L] <- "invented"
  expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
  composite <- tt_flat(tt_stack(wp2d_factor_table(), wp2d_factor_table()))
  composite$`_block`[1L] <- "changed"
  expect_error(tabtools:::.tt_validate_flat(composite), class = "tabtools_error_flat")
})

test_that("reserved schema rejects duplicate columns and unknown underscore names before named lookup", {
  flat <- tt_flat(wp2d_factor_table())
  for (name in c("_user", "_mystery", "_state_0", "_state_2", "_state_01", "_state_x")) {
    bad <- flat
    bad[[name]] <- "user-owned value"
    expect_error(tabtools:::.tt_validate_flat(bad), class = "tabtools_error_flat")
    expect_error(bad[, c("rowlabel", name), drop = FALSE], class = "tabtools_error_flat")
  }
  for (name in c("_term", "_state_1", "rowlabel", "Coef.")) {
    bad <- flat
    bad[["extra"]] <- bad[[name]]
    names(bad)[ncol(bad)] <- name
    attr(bad, "block_id") <- NULL
    # A duplicate _term used to silently consult only the first matching name.
    # The unique-name error must precede missing-identity metadata lookups.
    expect_error(tabtools:::.tt_validate_flat(bad), "column names must be unique", fixed = TRUE,
                 class = "tabtools_error_flat")
  }
  d <- wp2d_numeric_rows()
  prefixed <- tt_flat(regtab(d, cilabel = "_user", plabel = "_state_1"))
  expect_true(all(c("statistic: _user", "statistic: _state_1") %in% names(prefixed)))
  expect_no_error(tabtools:::.tt_validate_flat(prefixed))
  ordinary <- puttab(data.frame(rowlabel = "x", "_user" = "user-owned value", check.names = FALSE))
  expect_identical(ordinary$body[[2]], "user-owned value")
  expect_true("_user" %in% ordinary$header[[length(ordinary$header)]]$text)
})

test_that("selector and composition allocate unique IDs without creating or advancing RNG state", {
  tab <- wp2d_factor_table()
  withr::local_seed(21103)
  before <- .Random.seed
  a <- tt_flat(tab)
  b <- tt_flat(tab)
  merged <- tt_flat(tt_merge(tab, tab))
  stacked <- tt_flat(tt_stack(tab, tab))
  expect_identical(.Random.seed, before)
  expect_identical(length(unique(c(attr(a, "block_id"), attr(b, "block_id"),
                                    attr(merged, "block_id"), attr(stacked, "block_id")))), 4L)
  expect_identical(attr(a, "source_block_id"), attr(b, "source_block_id"))
  expect_identical(attr(a, "full_header"), tab$header)
  # Preserve the caller's absent-RNG state after this scenario as well.
  rm(".Random.seed", envir = .GlobalEnv)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  tt_flat(tt_stack(tab, tab))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("rendered selector help describes projections, canonical states and intentional coercion", {
  text <- wp2d_help_text("tt_flat")
  for (contract in c("_state_", "_block", "stat:<item>", "model_states", "row_blocks", "masked",
                     "data.frame(lapply(flat, identity)", "Column names must be unique")) {
    expect_match(text, contract, fixed = TRUE)
  }
})
