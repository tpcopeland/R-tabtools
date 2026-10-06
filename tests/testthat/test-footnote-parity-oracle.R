# Synthetic, literal fixture fragments test the adapter itself. Real native
# routing and comparator wiring remain the golden bridge's responsibility.
test_that("W02 oracle reads explicit inputs and authentic generated-note evidence", {
  sc <- list(command = "regtab", r_call = 'regtab(fit, footnote = c("One", "Two"))',
             stata_call = 'regtab, stars footnote("One \\ Two")')
  legend <- "* p<0.05, ** p<0.01, *** p<0.001"
  native <- list(csv = "One \\ Two", xlsx = paste0("One \\ Two; ", legend), console = "Native automatic note.")
  contract <- golden_footnote_contract("synthetic", scenario = sc, native_sources = native,
    automatic = list(list(text = "Native automatic note.", native = "Native automatic note.",
                          source = "literal synthetic source fixture")),
    native_footers = list(csv = c("One", "Two"), xlsx = paste0("One \\ Two; ", legend)))
  expect_identical(contract$paragraphs, c("One", "Two", "Native automatic note.", legend))
  expect_identical(contract$user_paragraphs, c("One", "Two"))
  expect_identical(contract$stars, legend)
  expect_identical(contract$native_footers$csv, c("One", "Two"))
  expect_true(any(grepl("regtab.ado:2762-2783", contract$provenance, fixed = TRUE)))
  golden_assert_footnote_tail(contract$paragraphs, contract, "csv")
  golden_assert_native_footnote_tail(c("One", "Two"), contract, "csv")
})

test_that("W02 oracle rejects missing provenance and dynamic annotation expressions", {
  sc <- list(command = "puttab", r_call = 'puttab(data, footnote = "One")', stata_call = 'puttab, footnote("One")')
  native <- list(csv = "One")
  expect_error(golden_footnote_contract("synthetic", scenario = sc, native_sources = native,
    automatic = list(list(text = "Invented", native = "Absent", source = "source"))), "absent")
  expect_error(golden_footnote_contract("synthetic", scenario = sc, native_sources = native,
    automatic = list(list(text = "Invented"))), "provenance")
  sc$r_call <- 'puttab(data, footnote = paste("One", "Two"))'
  expect_error(golden_footnote_contract("synthetic", scenario = sc, native_sources = native), "literal")
  sc$r_call <- 'puttab(data, footnote = "* p<user literal")'
  contract <- golden_footnote_contract("synthetic", scenario = sc, native_sources = native)
  expect_identical(contract$paragraphs, "* p<user literal")
  expect_identical(contract$stars, character())
  expect_error(golden_assert_native_footnote_tail(character(), contract, "csv"), "explicitly declared")
})

test_that("footer assertions catch changed, missing and invented text before stripping", {
  contract <- list(paragraphs = c("One", "Two"), native_footers = list(csv = "One Two"))
  for (bad in list("One", c("One", "Different"), c("One", "Two", "Invented"))) {
    expect_failure(golden_assert_footnote_tail(bad, contract, "console"))
  }
  expect_failure(golden_assert_native_footnote_tail("Changed", contract, "csv"))
  expect_failure(golden_assert_footnote_tail(c("*One*", "*Different*"), contract, "markdown"))
  expect_identical(golden_fn_md('Costs $5; "quote" `tick` | & <tag>', dollar = TRUE),
                   '*Costs \\$5; "quote" \\`tick\\` \\| \\& \\<tag\\>*')
})
