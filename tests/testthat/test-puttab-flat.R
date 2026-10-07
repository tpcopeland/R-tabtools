# P04 consumer contracts; native source execution belongs in qa/.
p04_fixture <- function() {
  a <- regtab(stats::lm(mpg ~ wt, mtcars), models = "Same", stats = "n")
  b <- regtab(stats::lm(mpg ~ hp, mtcars), models = "Same", stats = "n")
  list(a = a, b = b, flat = tt_flat(tt_merge(a, b)))
}
p04_notice <- function(keys) paste0("(puttab: key column(s) ", paste(keys, collapse = " "),
  " not exported; name them in vars to export them)")
p04_noeffect <- function() "(puttab: no selected statistic columns have model labels; blockheader has no effect)"
p04_bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)
p04_spans <- function(tt, field, prototype) vapply(tt$meta$puttab_spans, `[[`, prototype, field)
p04_cell <- function(cells, address, field) {
  index <- match(address, cells$address)
  expect_false(anyNA(index), info = paste(address, collapse = " "))
  unname(cells[[field]][index])
}

test_that("P04 default omission uses the actual ordered key schema and counts full paragraphs", {
  f <- p04_fixture()$flat
  keys <- c("_order", "_term", "_rowtype", "_state_1", "_state_2", "_block")
  expect_identical(tail(names(f), 6L), keys)
  before <- serialize(f, NULL)
  tt <- puttab(f, blockheader = TRUE, title = "Title", footnote = c("One", "Two"))
  expect_identical(tt$header[[2]]$text, c(" ", "Coef.", "95% CI", "p-value", "Coef.", "95% CI", "p-value"))
  expect_identical(tt$header[[1]]$text, c("", "Same", "", "", "Same", "", ""))
  expect_identical(tt$meta$flat_source$full_header, attr(f, "full_header"))
  expect_identical(unname(as.matrix(tt$body)), unname(as.matrix(f[, names(attr(f, "column_model")), drop = FALSE])))
  expect_identical(tt$meta$flat_source$omitted_keys, keys)
  expect_identical(tt$footnote, paste(c("One", "Two", p04_notice(keys)), collapse = " \\ "))
  expect_identical(tt$stored$n_cols, 7L)
  expect_identical(tt$stored$n_spans, 2L)
  expect_identical(tt$stored$n_rows, 1L + 2L + nrow(f) + 3L)
  expect_identical(tt$stored$n_datarows, nrow(f))
  expect_identical(serialize(f, NULL), before)
})

test_that("P04 projected input notices name only retained omitted keys, never invented raw identities", {
  f <- p04_fixture()$flat
  ci <- names(attr(f, "column_model"))[c(3L, 6L)]
  projected <- f[, c(ci[2], "_state_1"), drop = FALSE]
  tt <- puttab(projected, blockheader = TRUE)
  expect_identical(tt$footnote, p04_notice("_state_1"))
  expect_identical(tt$meta$flat_source$omitted_keys, "_state_1")
  expect_identical(names(tt$meta$flat_source$keys), "_state_1")
  expect_null(tt$meta$flat_source$keys$`_term`)
  expect_null(tt$meta$flat_source$keys$`_order`)
  expect_identical(tt$meta$flat_source$model_states, attr(projected, "model_states"))
  expect_identical(dim(tt$meta$flat_source$model_states), c(nrow(f), 2L))
  expect_identical(puttab(f[, ci, drop = FALSE], blockheader = TRUE)$footnote, "")
})

test_that("P04 explicitly selected keys remain visible and do not produce omission notes", {
  f <- p04_fixture()$flat
  visible <- names(attr(f, "column_model"))
  tt <- puttab(f, vars = c("_term", visible[6], "_state_1", visible[7]), blockheader = TRUE)
  expect_identical(tt$body[[1]], f$`_term`)
  expect_identical(tt$body[[3]], f$`_state_1`)
  expect_identical(tt$footnote, "")
  expect_identical(tt$meta$flat_source$omitted_keys, character())
  expect_identical(p04_spans(tt, "first", 0L), c(2L, 4L))
  expect_identical(p04_spans(tt, "last", 0L), c(2L, 4L))
  expect_identical(p04_spans(tt, "model_index", 0), c(2, 2))
})

test_that("P04 CI-only, p-only, reverse and split spans use original model indices", {
  f <- p04_fixture()$flat
  v <- names(attr(f, "column_model"))
  selections <- list(v[c(3, 6)], v[c(7, 6)], rev(v))
  headers <- list(c("95% CI", "95% CI"), c("p-value", "95% CI"),
                  c("p-value", "95% CI", "Coef.", "p-value", "95% CI", "Coef.", " "))
  for (i in seq_along(selections)) {
    selection <- selections[[i]]
    projected <- f[, selection, drop = FALSE]
    tt <- puttab(projected, blockheader = TRUE)
    expect_identical(tt$meta$flat_source$column_model, attr(projected, "column_model"))
    expect_identical(tt$header[[2]]$text, headers[[i]])
    expect_identical(tt$meta$flat_source$full_header, attr(projected, "full_header"))
    expect_identical(p04_spans(tt, "text", ""), rep("Same", tt$stored$n_spans))
    expect_identical(tt$footnote, "")
  }
  tt <- puttab(f, vars = v[c(6, 3, 7)], blockheader = TRUE)
  expect_identical(tt$stored$n_spans, 3L)
  expect_identical(p04_spans(tt, "model_index", 0), c(2, 1, 2))
  expect_identical(p04_spans(tt, "first", 0L), 1:3)
  expect_identical(p04_spans(tt, "last", 0L), 1:3)
})

test_that("P04 ordinary row-label placeholders follow source identity without changing other headers", {
  f <- p04_fixture()$flat
  v <- names(attr(f, "column_model"))
  base <- f[, v[1:2], drop = FALSE]
  set_header <- function(x, text) {
    h <- attr(x, "full_header")
    h[[length(h)]]$text <- text
    attr(x, "full_header") <- h
    x
  }
  other <- base
  other$other <- rep("Other", nrow(other))
  attr(other, "column_model") <- c(attr(base, "column_model"), other = NA_real_)
  attr(other, "full_header") <- lapply(attr(base, "full_header"), function(h) {
    h$text <- c(h$text, "")
    h
  })
  modeled <- f[, v[2], drop = FALSE]
  names(modeled) <- "rowlabel"
  cm <- attr(modeled, "column_model")
  names(cm) <- "rowlabel"
  attr(modeled, "column_model") <- cm
  modeled <- set_header(modeled, "")
  technical <- f[, c("_term", v[1:2]), drop = FALSE]
  cases <- list(
    default = list(x = base, args = list(blockheader = TRUE), header = list(c("", "Same"), c(" ", "Coef."))),
    reverse = list(x = base[, 2:1, drop = FALSE], args = list(blockheader = TRUE), header = list(c("Same", ""), c("Coef.", " "))),
    label_only = list(x = base[, "rowlabel", drop = FALSE], args = list(varlabels = TRUE), header = list(" ")),
    absent_label = list(x = f[, v[2:3], drop = FALSE], args = list(blockheader = TRUE), header = list(c("Same", ""), c("Coef.", "95% CI"))),
    custom_label = list(x = set_header(base, c("  Custom stub  ", "Coef.")), args = list(blockheader = TRUE), header = list(c("", "Same"), c("  Custom stub  ", "Coef."))),
    two_spaces = list(x = set_header(base, c("  ", "Coef.")), args = list(blockheader = TRUE), header = list(c("", "Same"), c("  ", "Coef."))),
    blank_mapped = list(x = set_header(base, c("", "")), args = list(blockheader = TRUE), header = list(c("", "Same"), c(" ", ""))),
    blank_unmapped = list(x = other, args = list(blockheader = TRUE), header = list(c("", "Same", ""), c(" ", "Coef.", ""))),
    modeled_rowlabel = list(x = modeled, args = list(blockheader = TRUE), header = list("Same", "")),
    technical_first = list(x = technical, args = list(vars = names(technical), blockheader = TRUE), header = list(c("", "", "Same"), c("_term", " ", "Coef."))),
    noheader = list(x = base, args = list(noheader = TRUE, varlabels = TRUE), header = list()),
    names_only = list(x = base, args = list(varlabels = FALSE), header = list(c("rowlabel", "Coef.")))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    before <- serialize(case$x, NULL)
    tt <- do.call(puttab, c(list(x = case$x), case$args))
    expect_identical(lapply(tt$header, `[[`, "text"), case$header, info = name)
    expect_identical(tt$meta$flat_source$full_header, attr(case$x, "full_header"), info = name)
    expect_identical(unname(as.matrix(tt$body)), unname(as.matrix(case$x)), info = name)
    expect_identical(serialize(case$x, NULL), before, info = name)
  }
})

test_that("P04 equal model names remain separate across calls and repeated nested compositions", {
  x <- p04_fixture()
  first <- puttab(x$flat, blockheader = TRUE)
  second <- puttab(tt_flat(tt_merge(x$a, x$b)), blockheader = TRUE)
  expect_false(identical(first$meta$flat_source$block_id, second$meta$flat_source$block_id))
  repeated <- puttab(tt_flat(tt_merge(x$a, x$a)), blockheader = TRUE)
  expect_identical(p04_spans(repeated, "text", ""), c("Same", "Same"))
  expect_identical(p04_spans(repeated, "model_index", 0), c(1, 2))
  expect_false(identical(repeated$meta$puttab_spans[[1]]$source_blocks, repeated$meta$puttab_spans[[2]]$source_blocks))
  nested <- tt_flat(tt_stack(tt_stack(x$a, x$a), x$a))
  tt <- puttab(nested, blockheader = TRUE)
  expect_identical(tt$meta$flat_source$row_blocks, attr(nested, "row_blocks"))
  expect_length(unique(tt$meta$flat_source$row_blocks), 3L)
  expect_identical(tt$meta$flat_source$source_blocks, attr(nested, "source_blocks"))
})

test_that("P04 edited literal cells preserve raw equation/statistic keys, absent and structural states", {
  f <- p04_fixture()$flat
  original <- f
  f$rowlabel[f$`_term` == "wt"] <- '  Same | $macro `x` \\literal "quote"  '
  f[[2]][f$`_term` == "wt"] <- "Reference"
  tt <- puttab(f, blockheader = TRUE)
  expect_true(any(tt$body[[1]] == '  Same | $macro `x` \\literal "quote"  '))
  expect_true(any(tt$body[[2]] == "Reference"))
  expect_identical(tt$meta$flat_source$keys$`_term`, original$`_term`)
  expect_identical(tt$meta$flat_source$model_states, attr(original, "model_states"))
  expect_true(any(tt$meta$flat_source$model_states == "absent"))
  stat <- which(tt$meta$flat_source$row_types == "stat")
  expect_length(stat, 1L)
  expect_identical(tt$meta$flat_source$keys$`_term`[stat], "stat:n")
  expect_identical(unname(tt$meta$flat_source$model_states[stat, ]), c("", ""))
  eq <- f
  eq$`_term`[1] <- "equation:wt"
  expect_identical(puttab(eq)$meta$flat_source$keys$`_term`[1], "equation:wt")
})

test_that("P04 all canonical state values survive display edits as validated source provenance", {
  source <- regtab(stats::lm(mpg ~ wt + hp + factor(cyl) + factor(gear) + factor(am), mtcars),
                   stats = "n", interactions = "native")
  f <- tt_flat(source)
  states <- c("est", "ref", "omit", "notest", "absent", "empty", "constrained", "masked")
  analytic <- which(!attr(f, "row_types") %in% c("cat_header", "stat", "addrow", "header"))
  expect_gte(length(analytic), 8L)
  at <- analytic[seq_along(states)]
  attr(f, "model_states")[at, 1] <- states
  f$`_state_1`[at] <- states
  f[[2]][at] <- rep("Same display", 8L)
  tt <- puttab(f)
  expect_identical(unname(tt$meta$flat_source$model_states[at, 1]), states)
  expect_identical(tt$body[[2]][at], rep("Same display", 8L))
  structural <- which(attr(f, "row_types") %in% c("cat_header", "stat", "addrow", "header"))
  expect_gt(length(structural), 0L)
  expect_identical(unname(tt$meta$flat_source$model_states[structural, 1]), rep("", length(structural)))
})

test_that("P04 row subsetting preserves source order and every original model companion", {
  f <- p04_fixture()$flat
  tt <- puttab(f, subset = c(3L, 1L), vars = names(attr(f, "column_model")))
  expect_identical(tt$meta$flat_source$source_rows, c(1L, 3L))
  expect_identical(tt$meta$flat_source$body_rows, 1:2)
  for (a in c("source_blocks", "model_states")) expect_identical(tt$meta$flat_source[[a]], attr(f, a)[c(1L, 3L), , drop = FALSE])
  expect_identical(tt$meta$flat_source$row_types, attr(f, "row_types")[c(1L, 3L)])
  reordered <- f[c(3L, 1L), , drop = FALSE]
  expect_identical(puttab(reordered)$meta$flat_source$keys$`_order`, f$`_order`[c(3L, 1L)])
})

test_that("P04 technical panel and source-header helpers are excluded separately from key notices", {
  f <- p04_fixture()$flat
  expect_identical(f$`_term`, c("wt", "hp", "_cons", "stat:n"))
  expect_identical(f$`_rowtype`, c("var", "var", "var", "stat"))
  v <- names(attr(f, "column_model"))
  tt <- puttab(f, panel = "_rowtype", blockheader = TRUE, noindent = TRUE)
  keys <- c("_order", "_term", "_state_1", "_state_2", "_block")
  expect_identical(tt$footnote, p04_notice(keys))
  expect_identical(tt$meta$flat_source$omitted_keys, keys)
  expect_identical(tt$meta$flat_source$body_rows, c(2L, 3L, 4L, 6L))
  panel_cells <- as.matrix(tt$body[tt$meta$flat_source$body_rows, ])
  ordinary_cells <- as.matrix(puttab(f)$body)
  rownames(panel_cells) <- rownames(ordinary_cells) <- NULL
  expect_identical(panel_cells, ordinary_cells)
  headed <- puttab(f, vars = c(v[1], v[6]), panel = "_rowtype", panelheader = c("_state_1", "_state_2"),
                   blockheader = TRUE, noindent = TRUE)
  expect_identical(headed$footnote, "")
  expect_identical(headed$stored$n_cols, 2L)
  expect_identical(headed$stored$n_spans, 1L)
  expect_identical(headed$meta$flat_source$model_states, attr(f, "model_states"))
  expect_gt(length(headed$meta$puttab_panels$header), 0L)
})

test_that("P04 identified flat first records are never consumed as embedded headers", {
  f <- p04_fixture()$flat
  h <- attr(f, "full_header")[[2]]$text
  v <- names(attr(f, "column_model"))
  for (j in seq_along(v)) f[[v[j]]][1] <- h[j]
  for (flag in c(FALSE, TRUE)) {
    tt <- puttab(f, varlabels = TRUE, noembedheader = flag)
    expect_identical(tt$stored$n_datarows, nrow(f))
    expect_identical(unname(unlist(tt$body[1, ])), h)
    expect_identical(tt$meta$flat_source$keys$`_term`, f$`_term`)
  }
})

test_that("P04 label/key-only and nameless-model selections receive complete no-effect paragraphs", {
  f <- p04_fixture()$flat
  for (selection in list("_term", "rowlabel")) {
    tt <- puttab(f, vars = selection, blockheader = TRUE, footnote = c("One", "Two"))
    expect_identical(tt$footnote, paste(c("One", "Two", p04_noeffect()), collapse = " \\ "))
    expect_identical(tt$stored$n_spans, 0L)
    expect_identical(tt$stored$n_cols, 1L)
    expect_identical(tt$stored$n_rows, 1L + nrow(f) + 3L)
  }
  attr(f, "model_names") <- c("", " ")
  tt <- puttab(f, blockheader = TRUE)
  expect_identical(tt$stored$n_spans, 0L)
  expect_identical(tabtools:::.tt_footnote_paragraphs(tt$footnote),
                   c(p04_notice(c("_order", "_term", "_rowtype", "_state_1", "_state_2", "_block")), p04_noeffect()))
  expect_identical(puttab(data.frame(`_user` = "keep", check.names = FALSE), blockheader = TRUE)$footnote, p04_noeffect())
})

test_that("P04 malformed hidden metadata is rejected before projection and every fresh/preexisting sink", {
  f <- p04_fixture()$flat
  corruptions <- list(
    function(z) { attr(z, "full_header") <- NULL; z },
    function(z) { attr(z, "model_states")[1, 2] <- "bogus"; z },
    function(z) { attr(z, "source_blocks") <- attr(z, "source_blocks")[-1, , drop = FALSE]; z },
    function(z) { attr(z, "column_model")[2] <- 99; z },
    function(z) { attr(z, "row_blocks") <- NULL; z },
    function(z) { attr(z, "row_types")[1] <- "stat"; z },
    function(z) { names(z)[3] <- names(z)[2]; z },
    function(z) { z$`_user` <- "hidden"; z },
    function(z) { z$`_order`[1] <- 0L; z },
    function(z) { z$`_state_2`[1] <- "bogus"; z },
    function(z) { class(z) <- "data.frame"; z })
  directory <- withr::local_tempdir(pattern = "tabtools-p04-schema-")
  paths <- file.path(directory, c("out.csv", "out.md", "out.xlsx"))
  for (edit in corruptions) {
    expect_error(puttab(edit(f), vars = "_term", csv = paths[1], markdown = paths[2], xlsx = paths[3]), class = "tabtools_error_flat")
    expect_identical(file.exists(paths), rep(FALSE, 3L))
  }
  writeLines("KEEP CSV", paths[1]); writeLines("KEEP MD", paths[2])
  suppressMessages(puttab(data.frame(term = "KEEP"), xlsx = paths[3], sheet = "Table"))
  suppressMessages(puttab(data.frame(term = "UNRELATED"), xlsx = paths[3], sheet = "Mine"))
  before <- lapply(paths, p04_bytes)
  for (edit in corruptions) {
    expect_error(puttab(edit(f), vars = "_term", csv = paths[1], markdown = paths[2], xlsx = paths[3]), class = "tabtools_error_flat")
    expect_identical(lapply(paths, p04_bytes), before)
  }
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(paths[3]))), c("Table", "Mine"))
})

test_that("P04 blockheader flags/layout conflicts and empty selections refuse before sink changes", {
  f <- p04_fixture()$flat
  paths <- file.path(withr::local_tempdir(), c("out.csv", "out.md", "out.xlsx"))
  invoke <- function(x = f, ...) puttab(x, ..., csv = paths[1], markdown = paths[2], xlsx = paths[3])
  for (flag in list(NULL, NA, 1, "TRUE", logical(), c(TRUE, FALSE))) expect_error(invoke(blockheader = flag), class = "tabtools_error_layout")
  expect_error(invoke(blockheader = TRUE, noheader = TRUE), class = "tabtools_error_layout")
  expect_error(invoke(blockheader = TRUE, spanheader = list()), class = "tabtools_error_layout")
  expect_error(invoke(x = matrix(1), blockheader = TRUE), class = "tabtools_error_layout")
  expect_error(invoke(x = p04_fixture()$a, blockheader = TRUE), class = "tabtools_error_layout")
  expect_error(invoke(x = f[FALSE, , drop = FALSE]), class = "rlang_error")
  expect_error(invoke(x = f[, FALSE, drop = FALSE]), class = "rlang_error")
  expect_identical(file.exists(paths), rep(FALSE, 3L))
})

test_that("P04 ordinary inherited/accounting frames keep underscore columns and validated ledgers", {
  x <- p04_fixture()
  e <- effecttab(data.frame(term = "effect", estimate = 1, conf.low = .5, conf.high = 1.5, p.value = .01))
  for (table in list(x$a, e)) {
    frame <- as.data.frame(table)
    before <- serialize(frame, NULL)
    tt <- puttab(frame)
    plain <- data.frame(lapply(frame, identity), check.names = FALSE)
    expect_identical(tt$body, puttab(plain)$body)
    expect_identical(tt$header, puttab(plain)$header)
    expect_null(tt$meta$flat_source)
    expect_silent(tabtools:::.tt_validate_sample_accounting(tt$meta$sample_accounting))
    expect_identical(serialize(frame, NULL), before)
  }
  plain <- data.frame(`_term` = "raw literal", `_user` = "keep", check.names = FALSE)
  attr(plain, "sample_accounting") <- x$a$meta$sample_accounting
  attr(plain, "model_label") <- "ordinary"; attr(plain, "model_id") <- "lm"; attr(plain, "n_models") <- 1L
  tt <- puttab(plain)
  expect_identical(tt$header[[1]]$text, c("_term", "_user"))
  expect_identical(unname(unlist(tt$body[1, ])), c("raw literal", "keep"))
  expect_identical(tt$footnote, "")
  attr(plain, "sample_accounting") <- list(bad = TRUE)
  path <- withr::local_tempfile(fileext = ".csv")
  expect_error(puttab(plain, csv = path), class = "tabtools_error_sample_accounting")
  expect_false(file.exists(path))
})

test_that("P04 intentional plain conversion discards semantics without dropping values or keys", {
  f <- p04_fixture()$flat
  plain <- data.frame(lapply(f, identity), check.names = FALSE)
  tt <- puttab(plain)
  expect_identical(tt$stored$n_cols, ncol(f))
  expect_identical(tt$header[[1]]$text, names(f))
  expect_identical(tt$body[[which(names(f) == "_term")]], f$`_term`)
  expect_null(tt$meta$flat_source)
  expect_identical(tt$footnote, "")
})

test_that("P04 actual publication sinks and converters preserve full footer geometry and literal cells", {
  f <- p04_fixture()$flat
  f[[2]][1] <- 'Literal | $macro `tick` \\path "quote"'
  notice <- p04_notice(c("_order", "_term", "_rowtype", "_state_1", "_state_2", "_block"))
  paragraphs <- c("User one", "User two", notice)
  dir <- withr::local_tempdir(pattern = "tabtools-p04-artifacts-")
  paths <- file.path(dir, c("out.csv", "out.md", "out.xlsx"))
  tt <- suppressMessages(puttab(f, blockheader = TRUE, title = "Title", footnote = paragraphs[1:2],
                               headershade = TRUE, headercolor = "blue", csv = paths[1], markdown = paths[2], xlsx = paths[3]))
  expect_identical(readLines(paths[1])[2:3], c(",Same,,,Same,,", " ,Coef.,95% CI,p-value,Coef.,95% CI,p-value"))
  expect_identical(tail(readLines(paths[1]), 3L), paste0(paragraphs, strrep(",", 6L)))
  md_note <- paste0("*", gsub("_", "\\_", notice, fixed = TRUE), "*")
  expect_identical(tail(readLines(paths[2]), 6L), c("", "*User one*", "", "*User two*", "", md_note))
  expect_true(any(grepl('Literal \\| $macro \\`tick\\` \\\\path "quote"', readLines(paths[2]), fixed = TRUE)))
  expect_identical(readLines(paths[2])[3], "|  | Same, Coef. | Same, 95% CI | Same, p-value | Same, Coef. | Same, 95% CI | Same, p-value |")
  lines <- tabtools:::tt_console_lines(tt)
  expect_identical(tail(lines, 6L), as.vector(rbind(paragraphs, "")))
  cells <- golden_cell_styles(paths[3], "Table")
  expect_identical(p04_cell(cells, c("B2", "B3"), "value"), c("", "\u00a0"))
  notes <- seq.int(4L + nrow(f), length.out = 3L)
  expect_identical(p04_cell(cells, paste0("B", notes), "value"), paragraphs)
  expect_identical(p04_cell(cells, paste0("B", notes), "italic"), rep(TRUE, 3L))
  expect_identical(p04_cell(cells, paste0("B", notes), "wrap"), rep(TRUE, 3L))
  expect_identical(p04_cell(cells, paste0("B", notes), "size"), rep(8, 3L))
  expect_identical(p04_cell(cells, c("C2", "F2", "C3", "F3"), "fill"), rep("FF0000FF", 4L))
  expect_identical(p04_cell(cells, "C4", "value"), f[[2]][1])
  expect_identical(golden_sheet_layout(paths[3], "Table")$merges,
    sort(c("A1:H1", "C2:E2", "F2:H2", paste0("B", notes, ":H", notes))))
  expect_identical(tt$stored$n_rows, 1L + 2L + nrow(f) + 3L)
  spec <- tabtools:::.tt_render_spec(tt)
  expect_identical(golden_fn_paragraphs(spec$footnote), paragraphs)
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::as_flextable(tt)
    expect_identical(ft$footer$dataset[[1]], paragraphs)
  }
  before <- serialize(tt, NULL)
  later <- file.path(dir, "later.csv")
  tt_write_csv(tt, later)
  expect_identical(p04_bytes(later), p04_bytes(paths[1]))
  deferred_md <- file.path(dir, "later.md")
  tt_write_markdown(tt, deferred_md)
  expect_identical(p04_bytes(deferred_md), p04_bytes(paths[2]))
  deferred_xlsx <- file.path(dir, "later.xlsx")
  tt_write_xlsx(tt, deferred_xlsx)
  expect_identical(golden_cell_styles(deferred_xlsx, "Table"), cells)
  expect_identical(golden_sheet_layout(deferred_xlsx, "Table"), golden_sheet_layout(paths[3], "Table"))
  empty_stub <- tt
  empty_stub$header[[2]]$text[1] <- ""
  expect_identical(format(tt), format(empty_stub))
  expect_identical(capture.output(print(tt)), capture.output(print(empty_stub)))
  expect_identical(serialize(tt, NULL), before)
})

test_that("P04 rendered puttab help exposes key and blockheader semantics", {
  ns_path <- normalizePath(getNamespaceInfo("tabtools", "path"), mustWork = TRUE)
  man_dir <- file.path(ns_path, "man")
  if (dir.exists(man_dir)) {
    page_path <- file.path(man_dir, "puttab.Rd")
    if (!file.exists(page_path)) stop("Missing current tabtools help page: puttab", call. = FALSE)
    page <- tools::parse_Rd(page_path)
  } else {
    pages <- tools::Rd_db("tabtools", lib.loc = dirname(ns_path))
    page <- pages[["puttab.Rd"]]
    if (is.null(page)) stop("Missing current tabtools help page: puttab", call. = FALSE)
  }
  text <- paste(capture.output(tools::Rd2txt(page, options = list(underline_titles = FALSE))), collapse = " ")
  text <- gsub("[[:space:]]+", " ", text)
  for (literal in c("blockheader", "technical keys", "tabtools_error_flat", "original column-model indices",
                    "synthetic panel rows", "data.frame(lapply(flat, identity)", "every R sink")) {
    expect_match(text, literal, fixed = TRUE)
  }
  expect_false(grepl("\\\\(item|code|section|verb)\\{", text))
})
