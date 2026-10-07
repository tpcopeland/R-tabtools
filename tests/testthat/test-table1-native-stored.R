test_that("held authentic 2.5.1 T25 stored values include the native full mode", {
  directory <- t1p_local()
  fixture <- normalizePath(test_path("fixtures", "table1_primary_native", "T25_stored.csv"),
                           mustWork = TRUE)
  data <- golden_fixture("sc_primary")
  withr::local_dir(directory)
  expect_identical(unname(tools::md5sum(fixture)), "d61561fd210b16c79a6a4e00277adf34")
  native <- utils::read.csv(fixture, colClasses = "character", na.strings = NULL,
                             check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(native$value[native$name == "_golden_tabtools_version"], "2.5.1")
  expect_identical(native$value[native$name == "smallcells_mode"], "full")
  t <- table1_tc(data, by = "group", vars = c(category = "cat"),
                 smallcells = 5, xlsx = "table1_tc.xlsx", sheet = "T25", markdown = "T25.md")
  # Existing generic projection covers native scalar r(smallcells), only.
  # Every other native field, including matrix coordinates and mode, is compared.
  expect_identical(golden_compare_stored(t$stored, native), character())
  t1p_mask_contract(t, 5L, "strict", 4L, 1L)
  for (mutation in list(list(smallcells_mode = NULL), list(smallcells_mode = "primary"),
                       list(N_derived_suppressed = 0L))) {
    bad <- t$stored
    for (key in names(mutation)) bad[key] <- mutation[key]
    expect_gt(length(golden_compare_stored(bad, native)), 0L)
  }
  expect_identical(t$stored$smallcells$threshold, 5L)
})

for (converter in c("gt", "flextable", "gtsummary", "tinytable")) {
  local({
    package <- converter
    test_that(paste(package, "carries literal publication text without analytical raw backup"), {
      skip_if_not_installed(package)
      directory <- t1p_local()
      literal <- "quote \" $value `code` \\path"
      t <- t1p_call(smallcells = 5, smallcells_mode = "primary", smd = TRUE,
                   cellreplace = t1p_replace("age", "A", literal))
      if (package == "gt") {
        g <- tt_as_gt(t)
        expect_true(any(as.matrix(g[["_data"]]) == literal))
        expect_false(any(grepl("raw", names(g[["_data"]]))))
        expect_match(gt::as_raw_html(g), "$value", fixed = TRUE)
      } else if (package == "flextable") {
        f <- flextable::as_flextable(t)
        expect_true(any(as.matrix(f$body$dataset) == literal))
        expect_false(any(grepl("raw", names(f$body$dataset))))
      } else if (package == "gtsummary") {
        g <- tt_as_gtsummary(t)
        expect_true(any(as.matrix(g$table_body) == literal, na.rm = TRUE))
        expect_false(any(grepl("raw", names(g$table_body))))
      } else {
        q <- tt_as_tinytable(t)
        path <- file.path(directory, "tiny.md")
        tinytable::save_tt(q, path, overwrite = TRUE)
        expect_match(ss_text(path), tabtools:::.md_escape(literal, dollar = TRUE), fixed = TRUE)
      }
    })
  })
}
