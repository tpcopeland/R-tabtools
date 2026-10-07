# Source-only demo catalog has the same build boundary as test-demo-parity.R.
skip_if(!golden_in_source() && !file.exists(golden_demo_path("manifest.csv")),
  "native demo source catalog is excluded from package builds")

test_that("actual native commands select full user and star paragraphs", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx2")
  base <- test_path("fixtures", "demo_native_footer")
  inventory <- utils::read.csv(file.path(base, "ARTIFACTS.csv"), colClasses = "character")
  expect_identical(unname(tools::md5sum(file.path(base, inventory$file))), inventory$md5)
  manifest <- golden_demo_manifest()
  sheets <- manifest[manifest$kind == "sheet", , drop = FALSE]
  expect_true(all(nzchar(sheets$native_command)))
  legend <- "* p<0.05, ** p<0.01, *** p<0.001"
  users <- c("Regtab Select" = "Selected covariates from full model.",
    "Regtab AddRow" = "Custom rows appended below model estimates.")
  for (sheet in names(users)) {
    id <- paste0("demo/demo_regtab.xlsx:", sheet)
    contract <- golden_demo_publication_contract(id,
      native_spec = list(book = file.path(base, "demo_regtab.xlsx"), sheet = sheet))
    expected <- c(unname(users[sheet]), legend)
    expect_identical(contract$paragraphs, expected)
    expect_identical(contract$native_footers$xlsx, paste(expected, collapse = " "))
    native <- contract$native_styles$value[match(paste0("B", max(contract$native_styles$row)), contract$native_styles$address)]
    expect_identical(native, paste(expected, collapse = " "))
    golden_assert_footnote_tail(expected, contract, "xlsx")
    golden_assert_native_footnote_tail(native, contract, "xlsx")
    golden_expect_detected(golden_assert_footnote_tail(expected[1L], contract, "xlsx"))
    golden_expect_detected(golden_assert_native_footnote_tail(expected[1L], contract, "xlsx"))
  }
  # Native Drop has no stars; literal full source declarations prevent guessing
  # the flag from neighbouring calls or from the caller's model label.
  drop <- golden_demo_publication_contract("demo/demo_regtab.xlsx:Regtab Drop",
    native_spec = list(book = file.path(base, "demo_regtab.xlsx"), sheet = "Regtab Drop"))
  expect_identical(drop$stars, character())
  expect_identical(drop$paragraphs,
    "Model adjusted for age, sex, and education (coefficients suppressed).")
})

test_that("every native paragraph proves uniform style geometry before template reuse", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx2")
  id <- "demo/demo_puttab.xlsx:Panels"
  contract <- golden_demo_publication_contract(id,
    native_spec = list(book = test_path("fixtures", "demo_native_footer", "demo_puttab.xlsx"), sheet = "Panels"))
  expect_identical(contract$paragraphs, c("Counts are crude.", "Ratios are adjusted."))
  expect_identical(contract$native_footers$xlsx, contract$paragraphs)
  golden_footer_style_templates(contract, id)
  anchors <- which(contract$native_styles$row > contract$sheet_end & contract$native_styles$col == 2L)
  expect_length(anchors, 2L)
  mutant <- contract
  mutant$native_styles$bold[anchors[2L]] <- !mutant$native_styles$bold[anchors[2L]]
  golden_expect_detected(golden_footer_style_templates(mutant, id))
  mutant <- contract
  merges <- which(as.integer(sub("^.*[A-Z]+([0-9]+)$", "\\1", mutant$native_layout$merges)) > mutant$sheet_end)
  expect_length(merges, 2L)
  mutant$native_layout$merges[merges[2L]] <- sub("D", "E", mutant$native_layout$merges[merges[2L]], fixed = TRUE)
  golden_expect_detected(golden_footer_style_templates(mutant, id))
  mutant <- contract
  rows <- sort(unique(contract$native_styles$row[contract$native_styles$row > contract$sheet_end]))
  height <- contract$native_layout$heights$height[contract$native_layout$heights$row == rows[1L]]
  mutant$native_layout$heights <- rbind(
    contract$native_layout$heights[contract$native_layout$heights$row <= contract$sheet_end, , drop = FALSE],
    data.frame(row = rows, height = c(if (length(height)) height else 15, 99)))
  golden_expect_detected(golden_footer_style_templates(mutant, id))
})


test_that("the recorder uses the resolved returned workbook including puttab file", {
  expect_identical(golden_demo_destination(list(stored = list(file = "resolved.xlsx")),
    list(xlsx = "argument.xlsx")), "resolved.xlsx")
  expect_identical(golden_demo_destination(list(stored = list(xlsx = "writer.xlsx", file = "puttab.xlsx")),
    list(xlsx = "argument.xlsx")), "writer.xlsx")
  expect_identical(golden_demo_destination(list(stored = list()), list(xlsx = "argument.xlsx")), "argument.xlsx")
  expect_null(golden_demo_destination(list(stored = list(file = NULL)), list(xlsx = NULL)))
})

test_that("primary demo declares full separate native and reviewed R footer boundaries", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx2")
  r_note <- paste0("Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: ",
    "no complementary cells are masked). Unmasked cells and ordinary variable tests are shown as computed; ",
    "effective sample size linked to a masked sample count is withheld. This protects printed counts only.")
  native_note <- paste0("Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: ",
    "no complementary cells are masked, and other cells, totals and tests are shown as computed). ",
    "This protects printed counts only.")
  contract <- golden_demo_publication_contract("demo/demo_table1.xlsx:Small Cells Primary Mode")
  expect_identical(contract$paragraphs, r_note)
  expect_identical(contract$native_footers$xlsx, native_note)
  golden_assert_footnote_tail(r_note, contract, "xlsx")
  golden_assert_native_footnote_tail(native_note, contract, "xlsx")
  body <- c("+--------+", "| Factor |", "+--------+")
  parts <- golden_demo_primary_console(c(body, "", r_note), c(body, "", native_note))
  expect_identical(parts$got, body)
  expect_identical(parts$want, body)
  golden_expect_detected(golden_demo_primary_console(c(body, native_note), c(body, native_note)))
  golden_expect_detected(golden_demo_primary_console(c(body, r_note), c(body, r_note)))
  golden_expect_detected(golden_demo_primary_console(c(body, r_note, "extra annotation"), c(body, native_note)))
  mutant <- golden_demo_primary_console(c(body[1L], "| fault  |", body[3L], r_note), c(body, native_note))
  expect_gt(length(golden_compare_console(mutant$got, mutant$want)), 0L)
})

test_that("the frame demo adapter retains publication descriptor rows and the exact ledger", {
  # Evaluate only the bounded adapter definition, never the demo producer.
  source <- parse(file = golden_demo_script("demo_tabtools.R"))[[1L]][[2L]]
  definitions <- Filter(function(expr) is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
    identical(expr[[2L]], as.name("native_table1_frame")), as.list(source)[-1L])
  expect_length(definitions, 1L)
  adapter <- eval(definitions[[1L]][[3L]], envir = baseenv())
  ledger <- data.frame(scope = "literal-original-row-population", eligible = 3L, used = 2L)
  tt <- list(command = "table1_tc", header = list(
    data.frame(text = c(" ", "Exposed", "Comparator", "p-value")),
    data.frame(text = c("No. (Column %) or Mean±SD", "N=2", "N=1", ""))),
    body = data.frame(label = c("Age", "Female"), g1 = c("45.0±2.0", "1 (50)"),
      g2 = c("50.0±1.0", "1 (100)"), p = c("0.03", "0.20")), meta = list(sample_accounting = ledger))
  original <- tt
  frame <- adapter(tt, source_data = NULL)
  expected <- data.frame(
    c1 = structure(c("No. (Column %) or Mean±SD", "Age", "Female"), label = "Factor "),
    c2 = structure(c("N=2", "45.0±2.0", "1 (50)"), label = "Exposed"),
    c3 = structure(c("N=1", "50.0±1.0", "1 (100)"), label = "Comparator"),
    c4 = structure(c("", "0.03", "0.20"), label = "p-value"), stringsAsFactors = FALSE)
  attr(expected, "sample_accounting") <- ledger
  expect_identical(frame, expected)
  expect_identical(vapply(frame, function(x) attr(x, "label", exact = TRUE), ""),
    c(c1 = "Factor ", c2 = "Exposed", c3 = "Comparator", c4 = "p-value"))
  expect_identical(attr(frame, "sample_accounting", exact = TRUE), ledger)
  expect_identical(tt, original)
  expect_false(identical(as.matrix(frame[-1L, , drop = FALSE]), as.matrix(frame)))
  # Literal Stata-if label contract: variable headings survive a subset even
  # when ordinary numeric/factor R columns lose their custom attributes.
  labelled <- data.frame(age = 1:3, education = factor(c("Low", "High", "Low")),
                         female = c(0, 1, 1))
  attr(labelled$age, "label") <- "Age at cohort entry (years)"
  attr(labelled$education, "label") <- "Education level"
  attr(labelled$female, "label") <- "Female sex"
  lost <- labelled[2:3, ]
  expect_null(attr(lost$age, "label", exact = TRUE))
  expect_null(attr(lost$education, "label", exact = TRUE))
  raw <- tt
  raw$body <- data.frame(label = c("age", "education", "education", "Custom female label"),
    g1 = c("72.0±4.0", "", "1 (50)", "1 (50)"),
    g2 = c("73.0±5.0", "", "1 (100)", "1 (100)"),
    p = c("0.03", "0.20", "", "0.10"))
  raw$rows <- data.frame(type = c("var", "cat_header", "level", "var"),
                        var = c("age", "education", "education", "female"))
  saved <- raw
  repaired <- adapter(raw, source_data = labelled)
  expect_identical(repaired$c1, structure(c("No. (Column %) or Mean±SD", "Age at cohort entry (years)",
    "Education level", "education", "Custom female label"), label = "Factor "))
  expect_identical(repaired[-1L], adapter(raw, source_data = NULL)[-1L])
  expect_identical(attr(repaired, "sample_accounting", exact = TRUE), ledger)
  expect_identical(raw, saved)
})

test_that("phase7 enrollment preserves native identities and all52 pending intent rows", {
  map <- utils::read.csv(golden_demo_script("phase7b-map.csv"), colClasses = "character")
  manifest <- golden_demo_manifest()
  key <- function(x) paste(x$kind, x$artefact, x$item, sep = "\r")
  expect_identical(as.integer(table(map$kind)[c("sheet", "console", "report")]), c(30L, 20L, 2L))
  expect_false(anyDuplicated(key(map)) > 0L)
  expect_identical(map$state, rep("source_authored_pending_review_execution", 52L))
  selected <- manifest[match(key(map), key(manifest)), , drop = FALSE]
  expect_identical(key(selected), key(map))
  expect_identical(selected$status, rep("compared", 52L))
  expect_identical(selected$task, rep("P7B", 52L))
  expect_identical(selected$r, map$r)
  expect_identical(selected$mask, rep("", 52L))
  expect_identical(sum(manifest$status == "compared"), 146L)
  expect_identical(sum(manifest$status == "not_ported" & manifest$kind == "sheet"), 3L)
})

test_that("new publication annotations are full independent native and R literals", {
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("openxlsx2")
  legend <- "* p<0.05, ** p<0.01, *** p<0.001"
  corr <- golden_demo_publication_contract("demo/demo_corrtab.xlsx:Correlation")
  expect_identical(corr$paragraphs, legend)
  expect_identical(corr$native_footers$xlsx, legend)
  strict <- golden_demo_publication_contract("demo/demo_crosstab.xlsx:Small Cells Complement")
  native <- "Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction."
  r <- paste0(native, " Percentages are withheld when their count or selected denominator is suppressed. ",
    "Tests and association estimates are withheld when primary counts are protected.")
  expect_identical(strict$paragraphs, r)
  expect_identical(strict$native_footers$xlsx, native)
  body <- c("+--------+", "| Factor |", "+--------+")
  parts <- golden_demo_phase7_console(c(body, "", r), c(body, "", native), "C25")
  expect_identical(parts$got, body)
  expect_identical(parts$want, body)
  golden_expect_detected(golden_demo_phase7_console(c(body, native), c(body, native), "C25"))
  golden_expect_detected(golden_demo_phase7_console(c(body, r, "extra"), c(body, native), "C25"))
  clusters <- "(ratetab: treated: 6 clusters of region)"
  rate_parts <- golden_demo_phase7_console(body, c(body, clusters), "C41")
  expect_identical(rate_parts$got, body)
  expect_identical(rate_parts$want, body)
  golden_expect_detected(golden_demo_phase7_console(body,
    c(body, "(ratetab: treated: 5 clusters of region)"), "C41"))
  golden_expect_detected(golden_demo_phase7_console(body, c(body, clusters, "extra"), "C41"))
  reverse <- golden_demo_publication_contract("demo/demo_survtab.xlsx:Cumul Incidence")
  expect_identical(reverse$paragraphs, paste0("reverse reports 1 - Kaplan-Meier, which equals cumulative incidence only with a single event type ",
    "(no competing risks). With competing events, use a competing-risks estimator (Aalen-Johansen)."))
  expect_identical(reverse$native_footers$xlsx, character())
  expect_true(reverse$R_only_reverse)
  expect_identical(reverse$sheet_end, 8L)
})

test_that("leaf comparisons require actual publication, declared form and source", {
  c <- golden_demo_leaf_contract("C35")
  literal <- "1.10 (1.00, 1.22)"
  # Build a genuine cell through the public contrast API so text and the
  # complete analytical provenance remain aligned under registered methods.
  leaf <- tabcell("est", contrast = list(estimate = log(1.10), std.error = 0.051,
    df = Inf, conf.level = 0.95, effect_scale = "coefficient", native_source = "lincom"),
    eform = TRUE, digits = 2L)
  result <- golden_demo_leaf_console('[1] "1.10 (1.00, 1.22)"', literal, "C35", list(L03 = leaf))
  expect_identical(result$got, literal)
  expect_identical(result$want, literal)
  changed <- leaf
  attr(changed, "provenance")$rows$form <- "p"
  golden_expect_detected(golden_demo_leaf_console('[1] "1.10 (1.00, 1.22)"', literal, "C35", list(L03 = changed)))
  golden_expect_detected(golden_demo_leaf_console('[1] "0.10 (-0.00, 0.20)"', literal, "C35", list(L03 = leaf)))
  golden_expect_detected(golden_demo_leaf_console('[1] "1.10 (1.00, 1.22)"', "1.11 (1.00, 1.22)", "C35", list(L03 = leaf)))
})

test_that("R-only reverse footer refuses altered text and complete style geometry", {
  # Synthetic style controls exercise the declared adapter; they are never
  # native goldens or an oracle for the survival estimates/body.
  note <- golden_demo_phase7_notes("survtab", "Cumul Incidence")$R
  native <- data.frame(address = "B8", row = 8L, col = 2L, value = "body")
  footer <- data.frame(address = "B9", row = 9L, col = 2L, value = note,
    bold = FALSE, italic = TRUE, font = "Times New Roman", size = 10,
    number_format = "General", font_color = "", halign = "left", valign = "center", wrap = TRUE,
    border_top = NA_character_, border_bottom = NA_character_, border_left = NA_character_,
    border_right = NA_character_, fill = "")
  layout <- list(merges = "B9:E9", heights = data.frame(row = integer(), height = numeric()))
  native_layout <- list(merges = character(), heights = layout$heights)
  contract <- list(sheet_end = 8L, paragraphs = note, native_footers = list(xlsx = character()))
  result <- golden_demo_reverse_styles(footer, native, layout, native_layout, contract)
  expect_identical(result$g, footer[FALSE, , drop = FALSE])
  bad <- footer; bad$size <- 9
  golden_expect_detected(golden_demo_reverse_styles(bad, native, layout, native_layout, contract))
  bad <- layout; bad$merges <- "B9:F9"
  golden_expect_detected(golden_demo_reverse_styles(footer, native, bad, native_layout, contract))
  bad <- footer; bad$value <- "truncated warning"
  golden_expect_detected(golden_demo_reverse_styles(bad, native, layout, native_layout, contract))
})
