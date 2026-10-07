test_that("primary masks literal printed counts without changing other inference", {
  t1p_local()
  base <- t1p_call(smd = TRUE, total = "after", smallcells = NULL)
  primary <- t1p_call(smd = TRUE, total = "after", smallcells = 5, smallcells_mode = "primary")
  t1p_mask_contract(base, 0L, "strict", 0L, 0L)
  t1p_mask_contract(primary, 5L, "primary", 3L, 0L)
  # Hand-counted: Low/A=2, High/A=4 and High/Total=4. Zero High/B visible.
  expect_identical(unname(primary$body[2, 2]), "<5")
  expect_identical(unname(primary$body[4, 2]), "<5")
  expect_identical(unname(primary$body[4, 4]), "<5")
  expect_identical(primary$body[4, 3], base$body[4, 3])
  expect_match(primary$body[4, 3], "0 (0)", fixed = TRUE)
  expect_identical(primary$body[3, ], base$body[3, ])
  expect_identical(primary$stored$table, base$stored$table)
  expect_identical(primary$rows$p, base$rows$p)
  expect_identical(primary$rows$smd, base$rows$smd)
  expect_identical(primary$stored$raw$table, base$stored$table)
  expect_identical(primary$stored$raw$sample_accounting, base$meta$sample_accounting)
  expect_equal(t1p_measure(primary, "variable/1/group/1", "missing_n", raw = TRUE)$value, 2)
  expect_true(is.na(t1p_measure(primary, "variable/1/group/1", "missing_n")$value))
  expect_identical(attr(as.data.frame(primary), "sample_accounting"), primary$meta$sample_accounting)
  expect_match(primary$footnote, "printed counts only", fixed = TRUE)
  expect_false(grepl("stored|raw\\$", primary$footnote))
  expect_error(as_forest_data(primary), "must be a table returned")
})

test_that("strict stays default, has no raw backup and withholds derived inference", {
  t1p_local()
  strict <- table1_tc(t1p_data(), by = "g", vars = c(category = "cat"), smallcells = 5, smd = TRUE)
  expect_identical(strict$stored$smallcells$mode, "strict")
  expect_identical(strict$stored$smallcells_mode, "full")
  expect_gt(strict$stored$smallcells$n_linked, 0L)
  expect_true(all(is.na(strict$stored$table)))
  expect_null(strict$stored$raw)
  zero <- table1_tc(t1p_data(), by = "g", vars = c(age = "contn"), smallcells = 3)
  t1p_mask_contract(zero, 3L, "strict", 0L, 0L)
  expect_null(zero$stored$raw)
})

test_that("hidden counts and continuous contributing N are not primary printed counts", {
  t1p_local()
  d <- data.frame(g = factor(rep(c("A", "B"), each = 14)),
                  category = factor(rep(c(rep("X", 6), rep("Y", 6), NA, NA), 2)),
                  age = c(1, 2, rep(NA_real_, 12), 15:28))
  p <- table1_tc(d, by = "g", vars = c(category = "cat", age = "contn"),
                smallcells = 5, smallcells_mode = "primary", smd = TRUE)
  plain <- table1_tc(d, by = "g", vars = c(category = "cat", age = "contn"), smd = TRUE)
  expect_identical(p$body, plain$body)
  expect_identical(p$stored$table, plain$stored$table)
  t1p_mask_contract(p, 5L, "primary", 0L, 0L)
  printed <- table1_tc(d, by = "g", vars = c(category = "cat"),
                      smallcells = 5, smallcells_mode = "primary", missingsummary = TRUE)
  expect_true(any(printed$body == "<5"))
  expect_identical(printed$stored$N_secondary_suppressed, 0L)
})

test_that("primary slashN masks a small printed denominator and keeps visible percentages", {
  t1p_local()
  d <- data.frame(g = rep(c("A", "B"), each = 8),
                  b = c(0, 0, 0, 0, rep(NA_real_, 4), 1, 1, 1, 0, 0, 0, 0, 0))
  t <- table1_tc(d, by = "g", vars = c(b = "bin"), smallcells = 5,
                smallcells_mode = "primary", slashN = TRUE)
  expect_identical(unname(t$body[1, 2]), "0/<5 (0)")
  expect_identical(unname(t$body[1, 3]), "<5")
  t1p_mask_contract(t, 5L, "primary", 2L, 0L)
  expect_error(table1_tc(d, by = "g", vars = c(b = "bin"), smallcells = 5,
                        smallcells_mode = "primary", slashN = TRUE,
                        cellreplace = t1p_replace("b", 1L, "0/4")),
               class = "tabtools_error_cellreplace_protected")
})

test_that("session mode, text and opt-outs preserve original explicitness through desctab", {
  t1p_local()
  tabtools_options(smallcells = 5, smallcells_mode = "primary", masktext = "HIDDEN")
  p <- suppressMessages(desctab(t1p_data(), vars = c(category = "cat"), by = "g"))
  expect_identical(p$stored$smallcells$mode, "primary")
  expect_true(any(p$body == "HIDDEN"))
  s <- suppressMessages(desctab(t1p_data(), vars = c(category = "cat"), by = "g", smallcells = 5))
  expect_identical(s$stored$smallcells$mode, "strict")
  expect_null(s$stored$raw)
  for (args in list(list(smallcells = NULL), list(smallcells = 0L), list(nosmallcells = TRUE))) {
    messages <- capture.output(t <- withCallingHandlers(
      do.call(desctab, c(list(data = t1p_data(), vars = c(category = "cat"), by = "g"), args)),
      message = function(cnd) {
        expect_false(grepl("using session smallcells|using session masktext", conditionMessage(cnd)))
        invokeRestart("muffleMessage")
      }))
    expect_identical(t$stored$smallcells$threshold, 0L)
  }
  expect_identical(suppressMessages(t1p_call())$stored$smallcells$mode, "primary")
  explicit_mode <- suppressMessages(t1p_call(smallcells = 5, smallcells_mode = "primary"))
  expect_identical(explicit_mode$stored$smallcells$mode, "primary")
  expect_identical(suppressMessages(t1p_call(smallcells_mode = NULL))$stored$smallcells$mode, "strict")
  defaults <- suppressMessages(t1p_call(masktext = NULL))
  expect_true(any(defaults$body == "<5"))
  blank <- suppressMessages(t1p_call(masktext = ""))
  expect_identical(blank$stored$smallcells$n_masked, 2L)
  expect_match(blank$footnote, "blank cells", fixed = TRUE)
  expect_false(grepl("shown as <", blank$footnote, fixed = TRUE))
})

test_that("numeric-looking and literal mask text never supplies header count evidence", {
  directory <- t1p_local()
  d <- data.frame(g = factor(c(rep("A", 2), rep("B", 8))), age = seq_len(10), w = c(1, 3, rep(1, 8)))
  for (marker in c("12", "literal \" $code `x` \\path", "")) {
    t <- table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
                   smallcells_mode = "primary", masktext = marker, headerperc = TRUE)
    expect_identical(unname(t$header[[2]]$text[2]), marker)
    expect_match(t$header[[2]]$text[3], "8 (.)", fixed = TRUE)
    t1p_mask_contract(t, 5L, "primary", 1L, 0L)
    expect_error(table1_tc(d, by = "g", vars = c(age = "contn"), smallcells = 5,
                          smallcells_mode = "primary", masktext = marker, headerperc = TRUE,
                          cellreplace = t1p_replace(t$header[[2]]$text[1], 2L)),
                 class = "tabtools_error_cellreplace_protected")
    expect_false(grepl("stored|raw\\$", t$footnote))
  }
})

test_that("disabled Table1 and desctab calls report strict unless mode is explicit", {
  t1p_local()
  d <- data.frame(g = factor(rep(c("A", "B"), each = 6)), age = seq_len(12))
  tabtools_options(smallcells = 4L, smallcells_mode = "primary", masktext = "SESSION")
  saved <- tabtools_options()
  for (call in list(table1_tc, desctab)) {
    for (optout in list(list(nosmallcells = TRUE), list(smallcells = NULL), list(smallcells = 0L))) {
      for (mode in list(list(), list(smallcells_mode = "primary"), list(smallcells_mode = NULL))) {
        t <- suppressMessages(do.call(call, c(list(data = d, by = "g", vars = c(age = "contn")), optout, mode)))
        expected <- if (identical(mode, list(smallcells_mode = "primary"))) "primary" else "strict"
        expect_identical(t$stored$smallcells,
          list(threshold = 0L, mode = expected, n_masked = 0L, n_linked = 0L))
        expect_null(t$stored$smallcells_mode)
        expect_identical(tabtools_options(), saved)
      }
    }
    reused <- suppressMessages(call(d, by = "g", vars = c(age = "contn")))
    expect_identical(reused$stored$smallcells,
      list(threshold = 4L, mode = "primary", n_masked = 0L, n_linked = 0L))
    expect_identical(tabtools_options(), saved)
  }
  # Session mode/text can outlive a cleared threshold; neither selects a call mode.
  tabtools_options(smallcells = NULL)
  mode_only <- tabtools_options()
  for (call in list(table1_tc, desctab)) {
    for (mode in list(list(), list(nosmallcells = TRUE), list(smallcells_mode = "primary"),
                      list(smallcells_mode = NULL))) {
      t <- suppressMessages(do.call(call, c(list(data = d, by = "g", vars = c(age = "contn")), mode)))
      expected <- if (identical(mode, list(smallcells_mode = "primary"))) "primary" else "strict"
      expect_identical(t$stored$smallcells,
        list(threshold = 0L, mode = expected, n_masked = 0L, n_linked = 0L))
      expect_null(t$stored$smallcells_mode)
      expect_identical(tabtools_options(), mode_only)
    }
  }
  tabtools_options(smallcells = 4L)
  expect_identical(tabtools_options(), saved)
  for (call in list(table1_tc, desctab)) {
    reused <- suppressMessages(call(d, by = "g", vars = c(age = "contn")))
    expect_identical(reused$stored$smallcells,
      list(threshold = 4L, mode = "primary", n_masked = 0L, n_linked = 0L))
  }
})

test_that("mask arguments are exact typed inputs and canonical mutations are detected", {
  t1p_local()
  for (mode in list("p", "full", c("strict", "primary"), NA_character_, TRUE, 1)) {
    expect_error(t1p_call(smallcells = 5, smallcells_mode = mode), class = "tabtools_error_smallcells_mode")
  }
  for (threshold in list(1, 2, 3.5, Inf, NA_real_, "5", 0+0i, matrix(0))) {
    expect_error(t1p_call(smallcells = threshold), class = "rlang_error")
  }
  for (no in list(NULL, NA, c(TRUE, FALSE), 1, matrix(TRUE))) {
    expect_error(t1p_call(nosmallcells = no), class = "tabtools_error_smallcells")
  }
  for (k in list(0L, 5L)) expect_error(t1p_call(smallcells = k, nosmallcells = TRUE),
                                     class = "tabtools_error_smallcells_conflict")
  expect_error(t1p_call(masktext = "MASK"), class = "tabtools_error_smallcells_masktext")
  for (text in list(NA_character_, c("a", "b"), 1)) expect_error(
    t1p_call(smallcells = 5, masktext = text), class = "tabtools_error_smallcells_masktext")
  t <- t1p_call(smallcells = 5, smallcells_mode = "primary", total = "after")
  t1p_mask_contract(t, 5L, "primary", 3L, 0L)
  for (field in c("threshold", "mode", "n_masked", "n_linked")) {
    bad <- t
    bad$stored$smallcells[[field]] <- if (field == "mode") "strict" else 77L
    golden_expect_detected(t1p_mask_contract(bad, 5L, "primary", 3L, 0L))
    bad$stored$smallcells[[field]] <- NULL
    golden_expect_detected(t1p_mask_contract(bad, 5L, "primary", 3L, 0L))
  }
})

test_that("weighted primary retains native linked ESS and strict frequency safeguards", {
  t1p_local()
  d <- data.frame(g = factor(c(rep("A", 2), rep("B", 8))),
                  age = seq_len(10), w = rep(1, 10), f = rep(1, 10))
  d$w[2] <- 3
  p <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", wtn = TRUE,
                smallcells = 5, smallcells_mode = "primary", wtcompare = TRUE, smd = TRUE,
                format = "%6.2f", nformat = "%5.2f")
  expect_identical(unname(p$body[1, 2]), "")
  expect_identical(unname(p$body[1, 4]), "Suppressed")
  expect_identical(p$stored$smallcells$mode, "primary")
  expect_identical(p$stored$smallcells$n_linked, 1L)
  expect_identical(p$stored$N_derived_suppressed, 1L)
  expect_identical(p$stored$smallcells$n_masked, 2L)
  baseline <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", wtn = TRUE,
                       wtcompare = TRUE, smd = TRUE, format = "%6.2f", nformat = "%5.2f")
  expect_identical(p$body[2, ], baseline$body[2, ])
  expect_identical(p$stored$table, baseline$stored$table)
  expect_equal(unname(p$stored$table[1, "smd"]), 2.660532159653219, tolerance = 1e-12)
  expect_true(is.na(t1p_measure(p, "weighted/group/1", "effective_n")$value))
  expect_equal(t1p_measure(p, "weighted/group/1", "effective_n", raw = TRUE)$value, 1.6)
  for (marker in list(NULL, "MASK", "")) expect_error(
    table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", wtn = TRUE,
              smallcells = 5, smallcells_mode = "primary", wtcompare = TRUE, masktext = marker,
              cellreplace = t1p_replace(p$body[1, 1], 3L, "2")),
    class = "tabtools_error_cellreplace_protected")
  strict <- table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w", wtn = TRUE,
                      wtcompare = TRUE, smd = TRUE, smallcells = 5)
  t1p_mask_contract(strict, 5L, "strict", 4L, 2L)
  unweighted <- table1_tc(d, by = "g", vars = c(age = "contn"), smd = TRUE,
                         smallcells = 5, smallcells_mode = "primary")
  t1p_mask_contract(unweighted, 5L, "primary", 1L, 0L)
  expect_error(table1_tc(d, by = "g", vars = c(age = "contn"), wt = "w",
                        smallcells = 5, smallcells_mode = "primary"),
               class = "tabtools_error_smallcells_percent_only")
  d$f <- rep(2^52, 10)
  for (mode in c("strict", "primary")) expect_error(
    table1_tc(d, by = "g", vars = c(age = "contn"), fweight = "f",
              smallcells = 5, smallcells_mode = mode), "below 2\\^53")
})
