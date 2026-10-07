library(testthat)
library(tabtools)

# Default: committed selected-capture proof. Optional overrides must contain
# those exact receipt/proof/artifact bytes, not regenerated or partial oracles.
# This additive 2.5.6 validation never changes the 2.5.1 baseline goldens.
round_native <- Sys.getenv("TABTOOLS_ROUND_NATIVE_DIR", unset = test_path("data", "post251_round_v256", "out"))
round_source <- Sys.getenv("TABTOOLS_ROUND_SOURCE_ROOT", unset = test_path(".."))
round_proof <- Sys.getenv("TABTOOLS_ROUND_PROVENANCE_FILE", unset = file.path(round_native, "..", "source-provenance.json"))
round_native <- normalizePath(round_native, mustWork = TRUE)
round_source <- normalizePath(round_source, mustWork = TRUE)
round_proof <- normalizePath(round_proof, mustWork = TRUE)
round_helper <- file.path(round_source, "tests", "testthat", "helper-golden.R")
round_helper_hashes <- c("05a2c50a2b1f18ca73e38f0fdddfdcd3c29c3a31924a235a5339fd4120042f4c",
  "d445013f1232351a2531d4fb428a74ac5ecaa865b497e995ecce57cdacbcbdb4",
  "e5a80b2760c9c4032ff10a536413c0ca0ff16a8d0c3b1520c8afee61bc31f4ab")
if (!digest::digest(file = round_helper, algo = "sha256") %in% round_helper_hashes) stop("Changed rounding comparator source.")
source(round_helper, local = TRUE)
round_ids <- c("round_ci_default", "round_ci_cformat", "round_ci_negative", "round_ci_zero",
  "round_rate_default", "round_rate_fixed", "round_rate_general", "round_rate_exponential",
  "round_effect_collect", "round_effect_matrix")
round_sheets <- c("CI_default", "CI_cformat", "CI_negative", "CI_zero", "Rate_default",
  "Rate_fixed", "Rate_general", "Rate_exponential", "Effect_collect", "Effect_matrix")
round_local_state <- function() {
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  saved <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(saved, envir = state)
  }, envir = parent.frame())
}
round_sinks <- function(directory, id) {
  list(xlsx = file.path(directory, "round_ports.xlsx"),
    csv = file.path(directory, paste0(id, ".csv")),
    markdown = file.path(directory, paste0(id, ".md")),
    sheet = round_sheets[match(id, round_ids)])
}
round_surface <- function(x, directory, id) {
  sinks <- round_sinks(directory, id)
  expect_identical(x$stored$sheet, sinks$sheet, info = id)
  expect_identical(basename(x$stored$xlsx), "round_ports.xlsx", info = id)
  if (identical(x$command, "stratetab")) {
    expect_identical(basename(x$stored$csv), paste0(id, ".csv"), info = id)
  } else {
    expect_true(x$command %in% c("regtab", "effecttab"), info = id)
    expect_false("csv" %in% names(x$stored), info = id)
  }
  expect_true(all(file.exists(unlist(sinks[c("xlsx", "csv", "markdown")]))), info = id)
  expect_identical(basename(x$stored$markdown), paste0(id, ".md"), info = id)
  expect_identical(golden_compare_sink(sinks$csv, file.path(round_native, paste0(id, ".csv")),
    mask = character()), character(), info = id)
  expect_identical(golden_compare_sink(sinks$markdown, file.path(round_native, paste0(id, ".md")),
    mask = character()), character(), info = id)
  expect_identical(golden_compare_styles(sinks$xlsx, sinks$sheet,
    file.path(round_native, "round_ports.xlsx"), sinks$sheet,
    got_width_offset = golden_r_width_offset, mask = character()), character(), info = id)
  expect_identical(x$footnote, "", info = id)
}

test_that("selected ten-case native rounding capture is exact and authenticated", {
  receipt <- file.path(round_native, "..", "native-receipt.json")
  expect_identical(digest::digest(file = receipt, algo = "sha256"), "63f0c2eba09b08b7b2879408a13e438d69022aa4d5d6d7569d8c3dc951536222")
  r <- jsonlite::read_json(receipt, simplifyVector = TRUE)
  expect_identical(r$source_pin, "4eecca4d09d61df0cfe773fc2024b015920b5fe6")
  expect_identical(digest::digest(file = round_proof, algo = "sha256"), r$source_provenance_sha256)
  proof <- jsonlite::read_json(round_proof, simplifyVector = TRUE)
  expect_identical(proof$pin, r$source_pin)
  expect_identical(proof$version, "2.5.6")
  expect_identical(proof$files[["tabtools/stratetab.ado"]],
    "2722ed4b22250f51665341478a4aa1dec63fdb49f5475fc2c5eddfc5419c6445")
  expect_identical(r$recipe_sha256, "0930003730a7103f3719e039aa637604e545853822414987ba30698c162799b5")
  expect_identical(r$status, "Genuine native runtime completed; R comparison pending")
  expect_true(length(r$files) >= 30L)
  for (name in names(r$files)) {
    p <- file.path(round_native, name)
    expect_true(file.exists(p), info = name)
    expect_identical(digest::digest(file = p, algo = "sha256"), r$files[[name]], info = name)
  }
  expect_identical(openxlsx::getSheetNames(file.path(round_native, "round_ports.xlsx")), round_sheets)
  expect_true(all(file.exists(file.path(round_native, paste0(round_ids, ".csv")))))
})

test_that("four native regression round controls retain complete sinks and raw Wald values", {
  round_local_state()
  directory <- withr::local_tempdir(pattern = "tabtools-selected-round-reg-")
  z <- qnorm(.975)
  estimates <- c(2.125, 2.125, -2.375, 1)
  se <- c(rep(2 / z, 3), 1.0004 / z)
  # Authentic completed native recipe asserts these first three bounds as
  # binary-exact doubles; recomputing the same Wald formula in R can cross
  # display ties by a few ULPs. The near-zero export has 15-digit precision.
  lower <- c(.125, .125, -4.375, NA_real_)
  upper <- c(4.125, 4.125, -.375, NA_real_)
  for (i in 1:4) {
    id <- round_ids[i]
    native <- read.csv(file.path(round_native, paste0(id, ".raw.csv")),
      stringsAsFactors = FALSE, blank.lines.skip = FALSE)
    expect_identical(names(native), c("label", "estimate", "ll", "ul", "pvalue", "model",
      "model_label", "rowtype", "section", "source_row", "source_frame"))
    expect_identical(nrow(native), 1L)
    expect_identical(native$label, "x")
    expect_identical(native$model_label, "Model")
    expect_identical(native$rowtype, "effect")
    if (i <= 3L) {
      expect_identical(native$ll, lower[i], info = id)
      expect_identical(native$ul, upper[i], info = id)
    } else {
      expect_true(native$ll < 0 && native$ll > -.005)
    }
    lo <- if (i <= 3L) lower[i] else native$ll
    hi <- if (i <= 3L) upper[i] else native$ul
    # Independent declared B/V normal-Wald closeness, separately from exact
    # renderer inputs and native 15-digit numerical exports (existing policy).
    expect_equal(lo, estimates[i] - z * se[i], tolerance = 1e-12, info = id)
    expect_equal(hi, estimates[i] + z * se[i], tolerance = 1e-12, info = id)
    d <- data.frame(term = "x", estimate = estimates[i], std.error = se[i],
      conf.low = lo, conf.high = hi,
      p.value = 2 * pnorm(-abs(estimates[i] / se[i])), status = "est")
    attr(d, "effect_scale") <- "Coef."; attr(d, "conf.level") <- .95
    attr(d, "stata_cmd") <- "regress"
    before <- d
    format <- if (i == 2L) list(cformat = "%9.2f") else list(digits = 2L)
    x <- do.call(regtab, c(list(d, models = "Model", nointercept = TRUE), format,
      round_sinks(directory, id)))
    round_surface(x, directory, id)
    raw <- x$meta$regtab_rows
    expect_identical(raw$key, "x")
    fields <- c(estimate = "estimate", conf.low = "ll", conf.high = "ul", p.value = "pvalue")
    for (field in names(fields)) {
      f <- fields[[field]]
      expect_equal(raw[[field]], native[[f]], tolerance = 1e-14, info = paste(id, field))
      expect_identical(raw[[field]], d[[field]], info = paste(id, "raw", field))
    }
    expect_identical(d, before)
    expect_identical(x$stored$table_role, "raw_analytical")
    expect_identical(x$meta$frame$ci_level, 95)
  }
})

test_that("four native rate formats preserve supplied events time and raw rate bounds", {
  round_local_state()
  directory <- withr::local_tempdir(pattern = "tabtools-selected-round-rate-")
  b <- data.frame(arm = 1:5, `_D` = c(2.5, .5, 1.5, 3.5, 7),
    `_Y` = c(2.25, .25, 123.5, 1234567.5, .35),
    `_Rate` = c(2.25, .25, 123.5, 1234567.5, .35),
    `_Lower` = c(.25, 2.25, .35, 1234567.5, 2.5),
    `_Upper` = c(123.5, .75, 2.25, .25, 1.15), check.names = FALSE)
  before <- b
  formats <- list(list(digits = 1L, eventdigits = 0L), list(cformat = "%12.1f", eventdigits = 1L),
    list(cformat = "%12.4g", eventdigits = 1L), list(cformat = "%12.2e", eventdigits = 1L))
  for (i in 1:4) {
    id <- round_ids[4L + i]
    # Explicit95 is a renderer declaration for supplied arbitrary bounds;
    # these reversed limits are not claimed to be recomputed statistical CIs.
    x <- do.call(stratetab, c(list(b, level = 95, outcomes = 1L, ratescale = 1, pydigits = 1L),
      formats[[i]], round_sinks(directory, id)))
    round_surface(x, directory, id)
    expect_identical(x$stored$ci_level, 95)
    expect_identical(x$meta$rate_rows$events, b$`_D`, info = id)
    expect_identical(x$meta$rate_rows$person_years, b$`_Y`, info = id)
    expect_identical(x$meta$rate_rows$rate, b$`_Rate`, info = id)
    expect_identical(x$meta$rate_rows$lower, b$`_Lower`, info = id)
    expect_identical(x$meta$rate_rows$upper, b$`_Upper`, info = id)
    expect_identical(unname(x$stored$rates[, 1L]), b$`_Rate`, info = id)
    expect_identical(b, before)
  }
})

test_that("native collect and matrix effects preserve full surfaces and analytical inputs", {
  round_local_state()
  directory <- withr::local_tempdir(pattern = "tabtools-selected-round-effects-")
  d <- data.frame(y = 2.25 + (seq_len(100) %% 2 * 2 - 1) * 2^-51,
    x = factor(as.integer(seq_len(100) %% 4 < 2), levels = 0:1))
  # Native collects the regression margins. For this rendering control R
  # uses its supported tidy-result adapter with independently solved group
  # means and shared residual-variance Wald quantities. QR tiny-fit drift can
  # cross the 2.25 display tie; we do not alter an R fitted model to mimic it.
  means <- vapply(levels(d$x), function(g) mean(d$y[d$x == g]), 0)
  expect_identical(unname(means), c(2.25, 2.25))
  residuals <- d$y - means[as.character(d$x)]
  variance <- sum(residuals^2) / (nrow(d) - 2L)
  n <- as.numeric(table(d$x))
  se <- sqrt(variance / n)
  predictions <- data.frame(term = paste0(levels(d$x), ".x"),
    kind = "level", variable = "x", level = levels(d$x),
    estimate = unname(means), conf.low = unname(means) - qnorm(.975) * se,
    conf.high = unname(means) + qnorm(.975) * se,
    p.value = 2 * pnorm(-abs(unname(means) / se)))
  attr(predictions, "conf.level") <- .95
  before <- predictions; before_data <- d
  id <- "round_effect_collect"
  x <- do.call(effecttab, c(list(predictions, type = "margins", data = d, digits = 1L), round_sinks(directory, id)))
  round_surface(x, directory, id)
  expect_identical(predictions, before)
  expect_identical(d, before_data)
  complete <- x$meta$regtab_rows
  expect_identical(nrow(complete), 3L)
  expect_identical(complete$key[-1L], c("0.x", "1.x"))
  expect_identical(complete$label, c("x", "  0", "  1"))
  expect_identical(x$meta$effect_rows$kind, c("cat_header", "level", "level"))
  expect_true(all(is.na(unlist(complete[1L, c("estimate", "conf.low", "conf.high", "p.value")]))))
  raw <- complete[-1L, , drop = FALSE]
  expect_identical(nrow(raw), 2L)
  expect_identical(raw$estimate, as.numeric(predictions$estimate))
  expect_identical(raw$conf.low, as.numeric(predictions$conf.low))
  expect_identical(raw$conf.high, as.numeric(predictions$conf.high))
  expect_identical(raw$p.value, as.numeric(predictions$p.value))
  # Preserve the existing from(matrix) conversion route independently from
  # the actual collected-model effect path above.
  m <- rbind(tie = c(.125, .125, 4.125, .5), crossing = c(-.0004, -.0004, 1, .5))
  before_m <- m; id <- "round_effect_matrix"
  x <- do.call(effecttab, c(list(m, type = "margins", digits = 2L), round_sinks(directory, id)))
  round_surface(x, directory, id)
  raw <- x$meta$regtab_rows
  expect_identical(raw$estimate, unname(m[, 1L]))
  expect_identical(raw$conf.low, unname(m[, 2L]))
  expect_identical(raw$conf.high, unname(m[, 3L]))
  expect_identical(raw$p.value, unname(m[, 4L]))
  expect_identical(m, before_m)
  # Strict full-surface comparison must notice both altered bytes and style.
  bad_csv <- file.path(directory, "mutant.csv")
  expect_true(file.copy(file.path(directory, paste0(id, ".csv")), bad_csv))
  cat("changed", file = bad_csv, append = TRUE)
  expect_true(length(golden_compare_sink(bad_csv,
    file.path(round_native, paste0(id, ".csv")), mask = character())) > 0L)
  bad_book <- file.path(directory, "mutant.xlsx")
  # Mutate only a font color in the copied XLSX XML. openxlsx resaving a
  # previously loaded book can corrupt sharedStrings on this supported R;
  # a malformed workbook is not evidence that the strict style oracle works.
  expect_true(file.copy(file.path(directory, "round_ports.xlsx"), bad_book))
  xml_directory <- withr::local_tempdir(pattern = "round-font-mutant-")
  utils::unzip(bad_book, files = "xl/styles.xml", exdir = xml_directory)
  style_file <- file.path(xml_directory, "xl", "styles.xml")
  original_xml <- paste(readLines(style_file, warn = FALSE), collapse = "\n")
  fonts <- regmatches(original_xml, regexpr("<fonts\\b[^>]*>.*?</fonts>", original_xml, perl = TRUE))
  expect_length(fonts, 1L)
  clean_fonts <- gsub('<color[^>]*/>', '', fonts, perl = TRUE)
  changed_fonts <- gsub('</font>', '<color rgb="FFFF0000"/></font>', clean_fonts, fixed = TRUE)
  expect_false(identical(changed_fonts, fonts))
  changed_xml <- sub(fonts, changed_fonts, original_xml, fixed = TRUE)
  writeLines(changed_xml, style_file, useBytes = TRUE)
  local({
    withr::local_dir(xml_directory)
    utils::zip(zipfile = bad_book, files = "xl/styles.xml", flags = "-q")
  })
  original_cells <- golden_cell_styles(file.path(directory, "round_ports.xlsx"), "Effect_matrix")
  changed_cells <- golden_cell_styles(bad_book, "Effect_matrix")
  expect_identical(changed_cells$address, original_cells$address)
  expect_identical(changed_cells$value, original_cells$value)
  expect_true(length(golden_compare_styles(bad_book, "Effect_matrix",
    file.path(round_native, "round_ports.xlsx"), "Effect_matrix",
    got_width_offset = golden_r_width_offset, mask = character())) > 0L)
})
