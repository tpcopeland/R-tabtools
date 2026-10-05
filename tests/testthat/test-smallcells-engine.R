# Small-cell engine (plan task 3.5): parity with Stata's _tabtools_smallcells
# and the validation_smallcells.do cases V1-V7.

test_that("engine decisions match Stata on 600 random blocks", {
  x <- utils::read.csv(golden_path("smallcells_engine.csv"), colClasses = "character")
  expect_identical(nrow(x), 600L)
  expect_true(any(x$rc == "498"))
  expect_true(any(as.numeric(x$n_secondary[x$rc == "0"]) > 0))
  for (i in seq_len(nrow(x))) {
    r <- x[i, ]
    nr <- as.integer(r$nr)
    nc <- as.integer(r$nc)
    call <- function() {
      tabtools:::tt_smallcells(sc_mat(r$counts, nr, nc), as.integer(r$k),
                               sc_mat(r$exact, nr, nc), sc_mat(r$sensitive, nr, nc),
                               sc_num(r$rowexact), sc_num(r$rowsensitive),
                               sc_num(r$colexact), sc_num(r$colsensitive),
                               as.numeric(r$grandexact), as.numeric(r$grandsensitive))
    }
    if (r$rc == "498") {
      expect_error(call(), class = "tabtools_smallcells_uncertified", label = paste("case", r$case))
      next
    }
    expect_identical(r$rc, "0")
    res <- call()
    got <- list(as.vector(t(res$mask)), res$rowmask, res$colmask, res$totalmask,
                res$n_primary, res$n_secondary)
    want <- list(sc_num(r$mask), sc_num(r$rowmask), sc_num(r$colmask), as.numeric(r$totalmask),
                 as.numeric(r$n_primary), as.numeric(r$n_secondary))
    expect_equal(lapply(got, as.numeric), want, label = paste("case", r$case))
  }
})

test_that("V1: tight 2x2 bounds defeat structural closure but pass the oracle", {
  C <- matrix(c(1, 6, 5, 1), 2)
  res <- sc_full(C, 5)
  expect_true(sc_certify(C, res$mask, matrix(1, 2, 2), res$rowmask, res$colmask, res$totalmask, 5, 9))
  expect_true(any(res$rowmask > 0) || any(res$colmask > 0) || res$totalmask > 0)
})

test_that("V2: ordinary protected 2x2 table", {
  C <- matrix(c(2, 6, 8, 4), 2)
  res <- sc_full(C, 5)
  expect_identical(res$mask[1, 1], 1)
  expect_identical(res$mask[2, 2], 1)
  expect_true(res$mask[1, 2] == 2 || res$mask[2, 1] == 2)
  expect_true(sc_certify(C, res$mask, matrix(1, 2, 2), res$rowmask, res$colmask, res$totalmask, 5, 14))
})

test_that("V3: only the necessary grand-total complement remains", {
  C <- matrix(c(0, 1, 1, 4), 2)
  res <- sc_full(C, 5)
  expect_equal(res$mask, matrix(c(0, 1, 1, 1), 2))
  expect_equal(res$rowmask, c(1, 0))
  expect_equal(res$colmask, c(1, 0))
  expect_equal(res$totalmask, 2)
  expect_identical(res$n_primary, 5L)
  expect_identical(res$n_secondary, 1L)
  expect_true(sc_irredundant(C, res, matrix(1, 2, 2), 5, 6, c(1, 1), c(1, 1), 1))
})

test_that("V4: 2x3 structural zeros stay visible", {
  C <- matrix(c(1, 4, 5, 0, 0, 6), 2)
  res <- sc_full(C, 5)
  expect_equal(res$mask[1, 3], 0)
  expect_equal(res$mask[2, 2], 0)
  expect_true(sc_certify(C, res$mask, matrix(1, 2, 3), res$rowmask, res$colmask, res$totalmask, 5, 7))
})

test_that("V5: unreleased logical cells are not exposed as complementary markers", {
  C <- matrix(c(1, 3, 4, 2), 2)
  E <- matrix(c(1, 0, 1, 0), 2)
  res <- tabtools:::tt_smallcells(C, 5, exact = E, sensitive = E, rowexact = c(1, 0),
                                  rowsensitive = c(1, 0), colexact = c(1, 1),
                                  colsensitive = c(1, 1), grandexact = 1, grandsensitive = 1)
  expect_equal(res$mask[1, 1], 1)
  expect_equal(res$mask[2, 1], 0)
  expect_equal(res$mask[2, 2], 0)
  # Row 2's margin is not published, so the oracle must not treat it as known
  # (validation_smallcells.do checks only the cells here).
  expect_true(sc_certify(C, res$mask, E, res$rowmask, res$colmask, res$totalmask, 5, 10,
                         rowexact = c(1, 0)))
})

test_that("V6: full-block fallback markers are truthful at 0/1/k-1/k/k+1", {
  C <- matrix(1, 1, 5)
  res <- tabtools:::tt_smallcells(C, 5, sensitive = matrix(c(1, 0, 0, 0, 0), 1),
                                  rowexact = 1, rowsensitive = 1,
                                  colexact = rep(0, 5), colsensitive = rep(0, 5))
  expect_true(all(res$mask == 1))
  expect_equal(res$rowmask, 2)
  # Never labels an actual 1 as >=k merely because it was outside the
  # sensitive map.
  expect_false(any(res$mask == 2 & C < 5))

  res <- tabtools:::tt_smallcells(matrix(c(0, 1, 4, 5, 6), 1), 5, rowexact = 0, rowsensitive = 0)
  expect_equal(res$mask, matrix(c(0, 1, 1, 0, 0), 1))
})

test_that("V7: input guards and the <k / >=k / Suppressed rendering contract", {
  expect_error(tabtools:::tt_smallcells(matrix(c(1, -1), 1), 5), class = "tabtools_smallcells_input")
  expect_error(tabtools:::tt_smallcells(matrix(c(1, 2.5), 1), 5), class = "tabtools_smallcells_input")
  expect_error(tabtools:::tt_smallcells(matrix(c(1, NA), 1), 5), class = "tabtools_smallcells_input")
  expect_error(tabtools:::tt_smallcells(matrix(1, 1, 2), 5, exact = matrix(1, 2, 2)),
               class = "tabtools_smallcells_input")
  expect_error(tabtools:::tt_smallcells(matrix(1, 1, 2), 5, exact = matrix(c(1, 2), 1)),
               class = "tabtools_smallcells_input")
  expect_error(tabtools:::tt_smallcells(matrix(1, 1, 2), 5, grandexact = 2), class = "tabtools_smallcells_input")
  for (bad in list(0, 1, 2, 3.5, "5", NA, c(5, 6))) {
    expect_error(tabtools:::tt_smallcells(matrix(1), bad), class = "tabtools_smallcells_input")
  }

  r <- tabtools:::tt_sc_render(2, 1, 5)
  expect_identical(as.character(r), "<5")
  expect_identical(attr(r, "stata_missing"), ".p")
  r <- tabtools:::tt_sc_render(8, 2, 5)
  expect_identical(as.character(r), "\u22655")
  expect_identical(attr(r, "stata_missing"), ".s")
  r <- tabtools:::tt_sc_render(8, 3, 5)
  expect_identical(as.character(r), "Suppressed")
  expect_identical(attr(r, "stata_missing"), ".d")
  expect_identical(as.character(tabtools:::tt_sc_render(0, 0, 5, "%9.0f")), "0")
  expect_identical(as.character(tabtools:::tt_sc_render(1234, 0, 5)), "1,234")
  expect_error(tabtools:::tt_sc_render(1, 4, 5), class = "tabtools_smallcells_input")
})

test_that("uncertifiable blocks fail closed (test_smallcells.do: marker bounds)", {
  # Four categories of one observation each in a single group of 4: every
  # cell and the group N are primary, nothing can protect them.
  b <- tabtools:::tt_sc_block_cat(matrix(1, 4, 1), nonmiss = 4, sample_n = 4)
  expect_error(tabtools:::tt_sc_variable(b, 5), class = "tabtools_smallcells_uncertified")
})

test_that("zeros stay visible and are never complementary", {
  C <- matrix(c(0, 2, 0, 9, 0, 7), 2)
  res <- sc_full(C, 5)
  expect_true(all(res$mask[C == 0] == 0))
})

test_that("the engine never touches the session RNG", {
  set.seed(1)
  before <- .Random.seed
  sc_full(matrix(c(2, 6, 8, 4), 2), 5)
  expect_identical(.Random.seed, before)
})

test_that("small-cell footnote matches Stata's wording", {
  g <- golden_read_cells("T25")
  expect_identical(tabtools:::tt_sc_footnote(5), g[nrow(g), 1])
})
