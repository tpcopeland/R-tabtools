test_that("leaf provenance survives parent publication without changing the source", {
  leaf <- tabcell("n", n = c(3, 8), mincell = 5)
  before <- serialize(leaf, NULL)
  parent <- puttab(data.frame(group = c("A", "B"), count = leaf))
  expect_identical(parent$body[[2L]], c("<5", "8"))
  expect_identical(serialize(leaf, NULL), before)
  expect_error(tt_flat(leaf), class = "tabtools_error_flat")
  expect_error(tt_merge(leaf, parent), class = "tabtools_error_composition")
})

test_that("new group summaries export truthful ordinary metadata and refuse keyed composition", {
  d <- data.frame(g = c("A", "A", "B", "B"), e = c(1, 0, 1, 0), y = 1,
                  exposed = c(1, 0, 1, 0))
  tabs <- list(ratetab(d, "g", "e", "y"), outtab(d, "e", "exposed"))
  model <- regtab(lm(mpg ~ wt, mtcars))
  for (tab in tabs) {
    before <- serialize(tab, NULL)
    flat <- tt_flat(tab, keyed = FALSE)
    expect_identical(class(flat), "data.frame")
    expect_identical(unname(as.matrix(flat)), unname(as.matrix(tab$body)))
    expect_identical(attr(flat, "header"), tab$header)
    expect_identical(attr(flat, "command"), tab$command)
    expect_identical(attr(flat, "frame"), tab$meta$frame)
    expect_identical(attr(flat, "sample_accounting"), tab$meta[["sample_accounting", exact = TRUE]])
    expect_identical(attr(flat, "composition_export"), TRUE)
    expect_error(tt_flat(tab), class = "tabtools_error_flat")
    expect_error(tt_merge(tab, model), class = "tabtools_error_composition")
    expect_error(tt_stack(tab, model), class = "tabtools_error_composition")
    expect_s3_class(puttab(flat), "tt_table")
    expect_identical(serialize(tab, NULL), before)
  }
})
