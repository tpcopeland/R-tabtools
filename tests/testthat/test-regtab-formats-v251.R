# G03/G04/E01/P1-01. Fast literal expectations; the independent native
# formatter matrix is qa/crossval_regtab_formats.R, never a generated golden.

test_that("explicit full formats reject every explicitly supplied digits value before writing", {
  d <- wp2d_numeric_rows()
  scratch <- withr::local_tempdir(pattern = "tabtools-format-conflict-")
  for (fun in list(regtab, effecttab)) {
    for (digits in list(NULL, 0L, 2L)) {
      paths <- file.path(scratch, c("untouched.xlsx", "untouched.csv", "untouched.md"))
      writeBin(charToRaw("existing artifact"), paths[2])
      before <- readBin(paths[2], "raw", n = file.info(paths[2])$size)
      expect_error(do.call(fun, c(list(d), list(cformat = "%12.4f", digits = digits,
                                               xlsx = paths[1], csv = paths[2], markdown = paths[3]))),
                   class = "tabtools_error_format_conflict")
      expect_false(file.exists(paths[1]))
      expect_false(file.exists(paths[3]))
      expect_identical(readBin(paths[2], "raw", n = file.info(paths[2])$size), before)
    }
  }
  withr::local_options(tabtools.digits = 99L)
  expect_identical(regtab(d, cformat = "%24.8f")$body[[2]], "0.12345679")
  expect_identical(effecttab(d, cformat = "%24.8f")$body[[2]], "0.12345679")
})

test_that("full formats retain raw precision in supplied regressions and effect matrices", {
  d <- wp2d_numeric_rows()
  r <- regtab(d, cformat = "%24.8f", sep = " to ")
  expect_identical(r$body[[2]], "0.12345679")
  expect_identical(r$body[[3]], "(-0.98765432 to 0.34567891)")
  expect_identical(r$body[[4]], "0.040")
  expect_identical(r$meta$regtab_rows$estimate, d$estimate)
  m <- as.matrix(d[, c("estimate", "conf.low", "conf.high", "p.value")])
  rownames(m) <- "raw_x"
  e <- effecttab(m, cformat = "%24.9f", sep = " to ")
  expect_identical(e$body[[2]], "0.123456789")
  expect_identical(e$body[[3]], "(-0.987654322 to 0.345678912)")
  # The omitted-format path retains its historical two-pass matrix rounding.
  old <- matrix(c(0.25, 0.1, 0.15, 0.1), 1L, dimnames = list("Tie", NULL))
  expect_identical(effecttab(old, digits = 1)$body[[2]], "0.2")
  expect_identical(effecttab(old, digits = 1)$body[[3]], "(0.1, 0.1)")
  expect_identical(regtab(d)$body[[2]], "0.12")
  expect_identical(regtab(d, cformat = NULL)$body, regtab(d)$body)
})

test_that("decimal comma requires a comma-free literal CI separator", {
  d <- wp2d_numeric_rows()
  for (fun in list(regtab, effecttab)) {
    for (sep in c(", ", "a,b", "")) {
      expect_error(fun(d, cformat = "%12,2f", sep = sep), class = "tabtools_error_format_separator")
    }
    for (fmt in c("%12,2f", "%12,2g", "%12,2e")) {
      t <- fun(d, cformat = fmt, sep = " to ")
      expect_match(t$body[[3]], " to ", fixed = TRUE)
      expect_false(grepl(".", t$body[[3]], fixed = TRUE))
    }
    f <- fun(d, cformat = "%12,2f", sep = " to ")
    expect_identical(f$body[[2]], "0,12")
    expect_identical(f$body[[3]], "(-0,99 to 0,35)")
    expect_identical(fun(d, cformat = "%12.2f", sep = "")$body[[3]], "(-0.99, 0.35)")
    expect_identical(fun(d, cformat = "%12.2f", sep = " ")$body[[3]], "(-0.99 0.35)")
    literal <- " $separator`macro' \\ "
    expect_identical(fun(d, cformat = "%12.2f", sep = literal)$body[[3]],
                     paste0("(-0.99", literal, "0.35)"))
    for (bad in list(NULL, NA_character_, character(), 1, c("a", "b"))) {
      expect_error(fun(d, cformat = "%12.2f", sep = bad), class = "tabtools_error_format_separator")
    }
  }
})

test_that("Stata e formats cover native width, precision and extreme exponents", {
  x <- c(0, 1, -1, 0.45, 1.25, 12345.67, 1.2345e-11, 1e200, -1e200, 9.999)
  expect_identical(trimws(stata_fmt(x, "%9.2e")),
                   c("0.00e+00", "1.00e+00", "-1.00e+00", "4.50e-01", "1.25e+00",
                     "1.23e+04", "1.23e-11", "1.0e+200", "-1.0e+200", "1.00e+01"))
  expect_identical(trimws(stata_fmt(c(0, 1e200), "%12.0e")), c("0.00000e+00", "1.0000e+200"))
  expect_identical(trimws(stata_fmt(c(1.25, 1e200), "%4.2e")), c("1.3e+00", "1.e+200"))
  expect_identical(stata_fmt(1.25, "%-9.2e"), "1.25e+00 ")
  expect_identical(trimws(stata_fmt(9.999, "%24.15e")), "9.999000000000001e+00")
  expect_error(stata_fmt(1, "%9.2ec"), class = "tabtools_error_fmt")
  for (fun in list(regtab, effecttab)) {
    expect_error(fun(wp2d_numeric_rows(), cformat = "%9.2ec"), class = "tabtools_error_format")
    t <- fun(wp2d_numeric_rows(), cformat = "%9.2e", sep = " to ")
    expect_identical(t$body[[2]], "1.23e-01")
    expect_identical(t$body[[3]], "(-9.88e-01 to 3.46e-01)")
  }
  # Ordinary f/g have independent literal expectations as well as native QA.
  expect_identical(trimws(stata_fmt(c(0.45, 1.25, -1.25), "%9.2f")), c("0.45", "1.25", "-1.25"))
  expect_identical(trimws(stata_fmt(c(0, 12345.67), "%12.0fc")), c("0", "12,346"))
  expect_identical(trimws(stata_fmt(c(0, 1.25, -1.25), "%9.3g")), c("0", "1.25", "-1.25"))
})

test_that("zero-length non-character formats fail with the formatter error class", {
  for (bad in list(integer(), numeric(), logical(), NULL, character(), NA_character_, c("%9.2f", "%9.3g"))) {
    expect_error(stata_fmt(1, bad), class = "tabtools_error_fmt")
  }
})

test_that("custom CI and p headers survive every compact and p-value layout", {
  d <- wp2d_numeric_rows()
  for (compact in c(FALSE, TRUE)) {
    for (nopvalue in c(FALSE, TRUE)) {
      t <- regtab(d, d, models = c("First", "Second"), compact = compact, nopvalue = nopvalue,
                  cformat = "%12.2f", sep = " to ", cilabel = "CI custom", plabel = "P custom")
      expected <- c("", rep(c(if (compact) "Coef. CI custom" else c("Coef.", "CI custom"),
                              if (!nopvalue) "P custom"), 2L))
      expect_identical(t$header[[2]]$text, expected)
      expect_identical(t$body[[2]], if (compact) "0.12 (-0.99 to 0.35)" else "0.12")
      console <- paste(capture.output(print(t)), collapse = "\n")
      expect_match(console, "CI custom", fixed = TRUE)
      if (nopvalue) expect_false(grepl("P custom", console, fixed = TRUE)) else
        expect_match(console, "P custom", fixed = TRUE)
      expect_false(grepl("95% CI", console, fixed = TRUE))
    }
  }
  for (arg in c("cilabel", "plabel")) {
    for (bad in list(NA_character_, integer(), c("A", "B"))) {
      expect_error(do.call(regtab, c(list(d), stats::setNames(list(bad), arg))))
    }
  }
  # Header overrides do not change the original p-value display contract.
  expect_identical(regtab(d, cformat = "%12.8f", cdisc = TRUE)$body[[4]], "0.040")
})

test_that("literal ordered headers and formatted cells reach CSV, Markdown and XLSX", {
  scratch <- withr::local_tempdir(pattern = "tabtools-format-sinks-")
  first <- wp2d_numeric_rows()
  first$term <- "x"
  second <- first
  second$estimate <- 0.2345678912
  second$conf.low <- -0.8765432198
  second$conf.high <- 0.4567891234
  second$p.value <- 0.25
  footer <- "Footer *literal*"
  for (compact in c(FALSE, TRUE)) {
    for (nopvalue in c(FALSE, TRUE)) {
      id <- paste0("reg-", compact, "-", nopvalue)
      paths <- file.path(scratch, paste0(id, c(".csv", ".md", ".xlsx")))
      t <- regtab(first, second, models = c("First", "Second"), cformat = "%24.8f", sep = " to ",
                  compact = compact, nopvalue = nopvalue, cilabel = "CI custom", plabel = "P custom",
                  footnote = footer, csv = paths[1], markdown = paths[2], xlsx = paths[3], sheet = "Formats")
      # Literal expected values differ by model. Swapping model columns, or
      # losing a noncompact estimate while retaining its CI, must fail.
      first_cells <- c(if (compact) "0.12345679 (-0.98765432 to 0.34567891)" else
                         c("0.12345679", "(-0.98765432 to 0.34567891)"), if (!nopvalue) "0.040")
      second_cells <- c(if (compact) "0.23456789 (-0.87654322 to 0.45678912)" else
                          c("0.23456789", "(-0.87654322 to 0.45678912)"), if (!nopvalue) "0.25")
      stat_headers <- c(if (compact) "Coef. CI custom" else c("Coef.", "CI custom"), if (!nopvalue) "P custom")
      model_headers <- c("", "First", rep("", length(stat_headers) - 1L),
                         "Second", rep("", length(stat_headers) - 1L))
      body <- c("x", first_cells, second_cells)
      expected <- rbind(model_headers, c("", stat_headers, stat_headers), body,
                        c(footer, rep("", length(body) - 1L)))
      dimnames(expected) <- NULL
      csv <- utils::read.csv(paths[1], header = FALSE, colClasses = "character", check.names = FALSE,
                             na.strings = NULL)
      expect_identical(unname(as.matrix(csv)), expected)
      expect_identical(wp2d_xlsx_matrix(paths[3], "Formats"), expected)
      expect_identical(unname(as.matrix(t$body)), matrix(body, nrow = 1L))
      md_header <- c("", if (compact) "First: Coef. CI custom" else c("First: Coef.", "First: CI custom"),
                     if (!nopvalue) "First: P custom",
                     if (compact) "Second: Coef. CI custom" else c("Second: Coef.", "Second: CI custom"),
                     if (!nopvalue) "Second: P custom")
      expected_md <- c(paste0("| ", paste(md_header, collapse = " | "), " |"),
                       paste0("|", strrep(" --- |", length(body))),
                       paste0("| ", paste(body, collapse = " | "), " |"), "", "*Footer \\*literal\\**")
      expect_identical(readLines(paths[2], warn = FALSE), expected_md)
    }
  }
  # Literal macro-like separators and negative bounds, including the exact
  # Markdown backtick/footer escaping, in all available command layouts.
  literal <- " $separator`macro' "
  footer <- "Footer *literal* $note`macro'"
  for (fun in list(regtab, effecttab)) {
    variants <- if (identical(fun, regtab)) expand.grid(compact = c(FALSE, TRUE), nopvalue = c(FALSE, TRUE)) else
      data.frame(compact = FALSE, nopvalue = FALSE)
    for (i in seq_len(nrow(variants))) {
      compact <- variants$compact[i]
      nopvalue <- variants$nopvalue[i]
      id <- paste(if (identical(fun, regtab)) "reg-literal" else "effect-literal", compact, nopvalue, sep = "-")
      paths <- file.path(scratch, paste0(id, c(".csv", ".md", ".xlsx")))
      args <- list(first, models = "Literal", cformat = "%12,2f", sep = literal, footnote = footer,
                   csv = paths[1], markdown = paths[2], xlsx = paths[3], sheet = "S")
      if (identical(fun, regtab)) {
        args <- c(args, list(compact = compact, nopvalue = nopvalue, cilabel = "CI custom", plabel = "P custom"))
      } else args$effect <- "AME"
      t <- do.call(fun, args)
      ci <- "(-0,99 $separator`macro' 0,35)"
      ci_md <- "(-0,99 $separator\\`macro' 0,35)"
      body <- c("x", if (compact) paste("0,12", ci) else c("0,12", ci), if (!nopvalue) "0.040")
      body_md <- c("x", if (compact) paste("0,12", ci_md) else c("0,12", ci_md), if (!nopvalue) "0.040")
      if (identical(fun, regtab)) {
        stat_headers <- c(if (compact) "Coef. CI custom" else c("Coef.", "CI custom"), if (!nopvalue) "P custom")
        md_header <- c("", if (compact) "Literal: Coef. CI custom" else c("Literal: Coef.", "Literal: CI custom"),
                       if (!nopvalue) "Literal: P custom")
      } else {
        stat_headers <- c("AME", "95% CI", "p-value")
        md_header <- c("", "Literal: AME", "Literal: 95% CI", "Literal: p-value")
      }
      expected <- rbind(c("", "Literal", rep("", length(stat_headers) - 1L)), c("", stat_headers), body,
                        c(footer, rep("", length(body) - 1L)))
      dimnames(expected) <- NULL
      csv <- utils::read.csv(paths[1], header = FALSE, colClasses = "character", check.names = FALSE,
                             na.strings = NULL)
      expect_identical(unname(as.matrix(csv)), expected)
      expect_identical(wp2d_xlsx_matrix(paths[3], "S"), expected)
      expect_identical(unname(as.matrix(t$body)), matrix(body, nrow = 1L))
      expected_md <- c(paste0("| ", paste(md_header, collapse = " | "), " |"),
                       paste0("|", strrep(" --- |", length(body))),
                       paste0("| ", paste(body_md, collapse = " | "), " |"), "", "*Footer \\*literal\\* $note\\`macro'*")
      expect_identical(readLines(paths[2], warn = FALSE), expected_md)
      expect_match(paste(capture.output(print(t)), collapse = "\n"), ci, fixed = TRUE)
    }
  }
})

test_that("rendered help states the full-format and remaining native rounding contracts", {
  for (name in c("regtab", "effecttab")) {
    text <- wp2d_help_text(name)
    expect_match(text, "cformat", fixed = TRUE)
    expect_match(text, "digits", fixed = TRUE)
    expect_match(text, "13", fixed = TRUE)
    expect_match(text, "separator", fixed = TRUE)
  }
})
