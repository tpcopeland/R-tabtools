# Canonical reference placement does not restore invalidated numeric companions.
test_that("row compositions authenticate retained reference states", {
  d <- data.frame(term = c("gA", "gB"), variable = "g", var_label = "Group",
    var_type = "categorical", label = c("A", "B"), reference_row = c(TRUE, FALSE),
    estimate = c(1, 2), conf.low = c(NA, 1.2), conf.high = c(NA, 3), p.value = c(NA, .01))
  attr(d, "effect_scale") <- "HR"; attr(d, "conf.level") <- .95
  one <- regtab(d)
  for (x in list(tt_stack(one, one), tt_merge(one, one))) {
    expect_null(x$meta$regtab_rows)
    expect_identical(x$meta$composition$reference_states, x$meta$flat$states)
    s <- tabtools:::.ct_source(x, 1L)
    expect_identical(tabtools:::.ct_is_ref(s, 2L, 1L), TRUE)
    expect_identical(tabtools:::.ct_is_ref(s, 3L, 1L), FALSE)
    imported <- tabtools:::.ct_source(as.data.frame(x), 1L)
    expect_identical(tabtools:::.ct_is_ref(imported, 2L, 1L), TRUE)
    expect_error(tabtools:::.ct_source(x, 1L, cformat = "%9.1f"),
      "numeric companions", class = "tabtools_error_composition")
    expect_error(as_forest_data(x), "must be a table returned")
    changed <- x; changed$meta$flat$states[3L, 1L] <- "ref"
    expect_error(tabtools:::.ct_source(changed, 1L), class = "tabtools_error_composition")
    changed <- as.data.frame(x)
    stamp <- attr(changed, "composition", exact = TRUE)
    stamp$reference_states[3L, 1L] <- "ref"
    attr(changed, "composition") <- stamp
    expect_error(tabtools:::.ct_source(changed, 1L), class = "tabtools_error_composition")
  }
})
