# W02 paragraph behavior: explicit R-contract expectations, no goldens edited.
fn_tabs <- function(footnote) {
  d <- data.frame(arm = rep(c("A", "B"), each = 10), x = 1:20)
  model <- regtab(lm(mpg ~ wt, mtcars), stats = "n")
  effects <- data.frame(equation = "ATE", term = "r1vs0.arm", estimate = 1,
                        conf.low = .5, conf.high = 1.5, p.value = .02)
  list(
    table1_tc = table1_tc(d, by = "arm", vars = c(x = "contn"), footnote = footnote),
    regtab = regtab(lm(mpg ~ wt, mtcars), footnote = footnote),
    effecttab = effecttab(effects, footnote = footnote),
    puttab = puttab(data.frame(Item = c("x", "y"), Value = c("1", "2")), footnote = footnote),
    stacktab = stacktab(list(puttab(data.frame(Item = c("x", "y"), Value = c("1", "2")))),
                        note = footnote, style = list(noterowheight = 71)),
    stratetab = ct_rates(footnote = footnote),
    comptab = comptab(list(model, model), rows = list(1, 1), footnote = footnote),
    hrcomptab = hrcomptab(ct_rates(), list(ct_models(), ct_models()), rows = list(1, 4:5), footnote = footnote),
    wttab = wttab(1:4, footnote = footnote)
  )
}

test_that("paragraph vectors, reserved tokens and literal backslashes obey the contract", {
  parse_fn <- tabtools:::.tt_footnote_paragraphs
  expect_identical(parse_fn(c("A \\ B", "C")), c("A", "B", "C"))
  expect_identical(parse_fn("  A \\  \\ B  "), c("A", "B"))
  expect_identical(parse_fn("  $a$ 'quote' `tick` "), "  $a$ 'quote' `tick` ")
  expect_identical(parse_fn("A\\B \\\\ C"), "A\\B \\\\ C")
  expect_identical(parse_fn("A \\ \\ B"), c("A", "\\ B"))
  for (value in list(NULL, character(), "")) expect_identical(parse_fn(value), character())
  for (value in list(numeric(), logical(), list(), 1, NA_character_, c("a", NA))) {
    expect_error(tt_table(data.frame(a = "x"), header = NULL, footnote = value), class = "tabtools_error_footnote")
    expect_error(puttab(data.frame(a = "x"), footnote = value), class = "tabtools_error_footnote")
  }
})

test_that("all commands and sinks carry exact equivalent paragraph text", {
  paras <- c("Costs $5 and $6, \"quoted\" `tick`", "Second | note & <tag>; x\\y")
  tabs <- fn_tabs(paras)
  scalar <- fn_tabs(paste(paras, collapse = " \\ "))
  out <- withr::local_tempdir()
  for (name in names(tabs)) {
    x <- tabs[[name]]
    expect_identical(x$footnote, scalar[[name]]$footnote, label = name)
    expect_identical(golden_fn_paragraphs(x$footnote), paras, label = name)
    lines <- tabtools:::tt_console_lines(x)
    expect_identical(tail(lines, 4L), c(paras[1], "", paras[2], ""), label = name)
    csv <- file.path(out, paste0(name, ".csv"))
    tt_write_csv(x, csv)
    grid <- utils::read.csv(csv, header = FALSE, colClasses = "character", check.names = FALSE,
                            na.strings = NULL, quote = '"')
    expect_identical(tail(grid[[1L]], 2L), paras, label = name)
    expect_true(all(tail(as.matrix(grid[, -1L, drop = FALSE]), 2L) == ""), label = name)
    md <- file.path(out, paste0(name, ".md"))
    tt_write_markdown(x, md)
    expect_identical(tail(readLines(md), 4L), c("", "*Costs $5 and $6, \"quoted\" \\`tick\\`*",
                    "", "*Second \\| note \\& \\<tag\\>; x\\\\y*"), label = name)
    xlsx <- file.path(out, paste0(name, ".xlsx"))
    tt_write_xlsx(x, xlsx)
    wb <- openxlsx2::wb_load(xlsx)
    actual <- openxlsx2::wb_to_df(wb, sheet = 1,
                                 dims = paste0("A1:", tabtools:::.xlsx_col(ncol(x$body) + 1L),
                                               nrow(x$body) + length(x$header) + 3L),
                                 col_names = FALSE, skip_empty_rows = FALSE,
                                 skip_empty_cols = FALSE)
    expect_identical(tail(actual[[2L]], 2L), paras, label = name)
    spec <- tabtools:::.tt_render_spec(x)
    expect_identical(golden_fn_paragraphs(spec$footnote), paras, label = name)
  }
  expect_identical(tabs$puttab$stored$n_rows, 5L)
})

test_that("small-cell console footer prints each paragraph once in order", {
  small <- paste0("Counts below 5 are shown as <5; complementary cells are shown as \u22655",
                  " to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.")
  for (groups in 2:3) {
    d <- data.frame(g = rep(LETTERS[seq_len(groups)], each = 8),
                    cat = factor(rep(c("Rare", rep("Common", 7)), groups)))
    tab <- table1_tc(d, by = "g", vars = c(cat = "cat"), smallcells = 5,
                     smd = groups == 3, footnote = "User note")
    expected <- c("User note", small,
                   if (groups == 3) "SMD compares A vs B only (the first two of 3 groups).")
    lines <- tabtools:::tt_console_lines(tab)
    close_box <- tail(which(grepl("^  \\+-+\\+$", lines)), 1L)
    footer <- lines[seq.int(close_box + 1L, length(lines))]
    expect_identical(footer, as.vector(rbind(expected, "")))
    expect_identical(sum(footer == small), 1L)
    # Informational console notes remain distinct and keep their order.
    tab$notes <- "Console information"
    lines <- tabtools:::tt_console_lines(tab)
    close_box <- tail(which(grepl("^  \\+-+\\+$", lines)), 1L)
    expect_identical(lines[seq.int(close_box + 1L, length(lines))],
                     c("Console information", as.vector(rbind(expected, ""))))
  }
})

test_that("xlsx expanded note rows carry styles, merges and explicit heights", {
  tabs <- fn_tabs(c("One", "Two"))
  layouts <- list(table1_tc = tabtools:::.xlsx_layout_table1, regtab = tabtools:::.xlsx_layout_regtab,
                  effecttab = tabtools:::.xlsx_layout_regtab, puttab = tabtools:::.xlsx_layout_puttab,
                  stacktab = tabtools:::.xlsx_layout_stacktab, stratetab = tabtools:::.xlsx_layout_stratetab,
                  comptab = tabtools:::.xlsx_layout_comptab, hrcomptab = tabtools:::.xlsx_layout_hrcomptab,
                  wttab = tabtools:::.xlsx_layout_puttab)
  for (name in names(tabs)) {
    lay <- layouts[[name]](tabs[[name]])
    rr <- (nrow(lay$grid) - 1L):nrow(lay$grid)
    expect_identical(lay$grid[rr, 2L], c("One", "Two"), label = name)
    expect_true(all(lay$written[rr, 2L]), label = name)
    st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), tabs[[name]]$style)
    expect_true(all(st$italic[rr, 2L] & st$wrap[rr, 2L]), label = name)
    expect_identical(st$size[rr, 2L], rep(max(tabs[[name]]$style$fontsize - 2, 6), 2L), label = name)
    if (ncol(lay$grid) > 2L) {
      expect_true(all(paste0("B", rr, ":", tabtools:::.xlsx_col(ncol(lay$grid)), rr) %in% st$merges), label = name)
    }
    if (name == "stacktab") expect_identical(unname(st$heights[as.character(rr)]), c(71, 71))
  }
})

test_that("automatic note paragraphs include weight diagnostics and model legends", {
  ess <- "ESS = effective sample size, (sum of w)^2 / (sum of w^2); ESS (%) = 100 x ESS / N."
  trunc <- paste0("Truncated l/u: weights below the l-th or above the u-th percentile of all weights",
                  " set to that percentile; Truncated (n) counts the weights changed.")
  sampling <- "Weights include the sampling weights (s.weights)."
  for (t in c(FALSE, TRUE)) for (s in c(FALSE, TRUE)) {
    expect_identical(golden_fn_paragraphs(tabtools:::.wt_footnote(t, s_weights = s)),
                     c(ess, if (t) trunc, if (s) sampling))
  }
  expect_identical(golden_fn_paragraphs(wttab(1:4, trunc = c(.25, .75))$footnote), c(ess, trunc))
  expect_identical(wttab(1:4, footnote = "")$footnote, "")
  expect_identical(wttab(1:4, trunc = c(.25, .75), footnote = c("Mine", "Only"))$footnote, "Mine \\ Only")
  d <- data.frame(g = rep(c("A", "B", "C"), each = 8), x = 1:24)
  tab <- table1_tc(d, by = "g", vars = c(x = "contn"), smd = TRUE, footnote = "Mine.")
  expect_identical(golden_fn_paragraphs(tab$footnote),
                   c("Mine.", "SMD compares A vs B only (the first two of 3 groups)."))
  f <- lm(mpg ~ wt, mtcars)
  tab <- regtab(f, vce = "robust", vce_note = TRUE, stars = TRUE, footnote = "Mine")
  expect_identical(golden_fn_paragraphs(tab$footnote),
                   c("Mine", "Standard errors: robust.", "* p<0.05, ** p<0.01, *** p<0.001"))
})

test_that("composition transports genuine legend identity and preserves user annotation policy", {
  f <- lm(mpg ~ wt, mtcars)
  legend <- "* p<0.05, ** p<0.01, *** p<0.001"
  a <- regtab(f)
  b <- regtab(f, footnote = "* p<literal user note")
  real <- regtab(f, stars = TRUE, footnote = "* p<literal user note")
  for (compose in list(tt_merge, tt_stack)) {
    expect_identical(compose(a, b)$footnote, "")
    expect_identical(compose(a, b, footnote = "Override")$footnote, "Override")
    expect_identical(golden_fn_paragraphs(compose(a, real, footnote = "Override")$footnote), c("Override", legend))
    nested <- compose(compose(a, real, footnote = "First"), real, footnote = "Outer")
    expect_identical(golden_fn_paragraphs(nested$footnote), c("Outer", legend))
    # Deliberately emulate the documented pre-W02 serialized representation.
    old <- real
    old$meta$stars_notes <- NULL
    old$footnote <- "* p<literal user note"
    old$meta$xlsx_footnote <- paste0(old$footnote, "; ", legend)
    expect_identical(golden_fn_paragraphs(compose(a, old, footnote = "Legacy")$footnote), c("Legacy", legend))
  }
})

for (renderer in c("gt", "flextable", "tinytable", "gtsummary")) {
  local({
    renderer <- renderer
    test_that(paste(renderer, "stores literal paragraphs separately"), {
      skip_if_not_installed(renderer)
      paras <- c("Costs $5 and $6; \"quoted\" `tick`", "Second <tag> & note")
      tab <- puttab(data.frame(Item = "x", Value = "1"), footnote = paras)
      if (renderer == "gt") {
        obj <- tt_as_gt(tab)
        expect_identical(unname(unlist(obj[["_source_notes"]], use.names = FALSE)), paras)
        html <- as.character(gt::as_raw_html(obj))
        expect_match(html, "Costs $5 and $6", fixed = TRUE)
        expect_match(html, "&lt;tag&gt;", fixed = TRUE)
      } else if (renderer == "flextable") {
        chunks <- flextable::information_data_chunk(flextable::as_flextable(tab))
        expect_identical(chunks$txt[chunks$.part == "footer" & chunks$.col_id == "c1"], paras)
      } else if (renderer == "tinytable") {
        # Keep the full literal paragraph on one Markdown grid line.
        tab$body[[1L]][1L] <- strrep("x", 80L)
        obj <- tt_as_tinytable(tab)
        expect_identical(unname(unlist(obj@notes, use.names = FALSE)), paras)
        path <- withr::local_tempfile(fileext = ".md")
        tinytable::save_tt(obj, path, overwrite = TRUE)
        text <- paste(readLines(path), collapse = "\n")
        expect_match(text, 'Costs \\$5 and \\$6; "quoted" \\`tick\\`', fixed = TRUE)
        expect_match(text, "Second \\<tag\\> \\& note", fixed = TRUE)
      } else {
        obj <- tt_as_gtsummary(tab)
        expect_identical(obj$table_styling$source_note$source_note, paras)
      }
    })
  })
}
