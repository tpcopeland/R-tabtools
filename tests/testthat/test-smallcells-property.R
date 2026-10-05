# Reconstruction oracles for the small-cell engine (plan task 3.6):
# validation_smallcells.do V8 plus a brute-force property test on random
# small blocks with hidden cells and partly published margins.

test_that("V8: all 420 bounded 2x2/2x3 tables certify independently or fail closed", {
  run <- function(shape, maxsum) {
    nr <- shape[1]
    nc <- shape[2]
    grid <- as.matrix(expand.grid(rep(list(0:maxsum), nr * nc)))
    grid <- grid[rowSums(grid) <= maxsum, , drop = FALSE]
    rejected <- 0L
    for (r in seq_len(nrow(grid))) {
      C <- matrix(grid[r, ], nr, nc, byrow = TRUE)
      res <- tryCatch(sc_full(C, 3), tabtools_smallcells_uncertified = function(e) NULL)
      if (is.null(res)) {
        rejected <- rejected + 1L
        next
      }
      maxv <- max(sum(C), 3)
      ok <- sc_irredundant(C, res, matrix(1, nr, nc), 3, maxv, rep(1, nr), rep(1, nc), 1)
      expect_true(ok, label = paste(c(C), collapse = ","))
    }
    c(nrow(grid), rejected)
  }
  r22 <- run(c(2, 2), 6)
  r23 <- run(c(2, 3), 4)
  expect_identical(r22[1], 210L)
  expect_identical(r23[1], 210L)
  expect_gt(r22[2] + r23[2], 0L)
})

test_that("random blocks: every primary has >= 2 feasible values; every complement is necessary", {
  skip_on_cran()  # heavy (CRAN-lane runtime, pre-release review P2-1); runs in every NOT_CRAN gate
  # Profiles mirror the block shapes table1_tc builds: cat with total (all
  # margins published), cat without total (hidden last row), continuous
  # (N row unpublished), and random flags. Tables are small enough to
  # enumerate every completion; candidates are bounded by sum + k.
  rng <- function(seed, n) {
    u <- tabtools:::stata_runiform(n, seed)
    function() {
      x <- u[1]
      u <<- u[-1]
      x
    }
  }
  if (!exists("stata_runiform", envir = asNamespace("tabtools"), inherits = FALSE)) {
    set.seed(20260928)
    runif1 <- function() stats::runif(1)
  } else {
    runif1 <- rng(20260928, 50000)
  }
  # 300 cases locally and in CI (NOT_CRAN), 60 in a plain R CMD check.
  n_cases <- if (identical(Sys.getenv("NOT_CRAN"), "true")) 300L else 60L
  checked <- 0L
  rejected <- 0L
  for (case in seq_len(n_cases)) {
    k <- 3 + floor(2 * runif1())
    profile <- 1 + floor(4 * runif1())
    nr <- if (profile == 3) 2 else 2 + floor(2 * runif1())
    nc <- 1 + floor(3 * runif1())
    if (nr * nc > 6) nc <- floor(6 / nr)
    C <- matrix(0, nr, nc)
    for (i in seq_len(nr)) for (j in seq_len(nc)) {
      u <- runif1()
      C[i, j] <- if (u < 0.25) 0 else if (u < 0.65) 1 + floor((k - 1) * runif1()) else k + floor(4 * runif1())
    }
    E <- S <- matrix(1, nr, nc)
    RE <- RS <- rep(0, nr)
    CE <- CS <- rep(1, nc)
    ge <- gs <- 0
    if (profile == 1) {
      RE <- RS <- rep(1, nr)
      ge <- gs <- 1
    } else if (profile == 2) {
      E[nr, ] <- 0
      S[nr, ] <- 0
    } else if (profile == 3) {
      ms <- as.numeric(runif1() < 0.5)
      tot <- as.numeric(runif1() < 0.5)
      E[1, ] <- 0
      E[2, ] <- ms
      S[2, ] <- ms
      RE <- c(0, ms * tot)
      RS <- c(tot, ms * tot)
      ge <- gs <- tot
    } else {
      E[] <- as.numeric(vapply(seq_along(E), function(i) runif1() < 0.8, TRUE))
      S[] <- as.numeric(vapply(seq_along(S), function(i) runif1() < 0.8, TRUE))
      RE <- as.numeric(vapply(seq_len(nr), function(i) runif1() < 0.5, TRUE))
      RS <- as.numeric(vapply(seq_len(nr), function(i) runif1() < 0.5, TRUE))
      CE <- as.numeric(vapply(seq_len(nc), function(i) runif1() < 0.7, TRUE))
      CS <- as.numeric(vapply(seq_len(nc), function(i) runif1() < 0.7, TRUE))
      ge <- as.numeric(runif1() < 0.5)
      gs <- as.numeric(runif1() < 0.5)
    }
    res <- tryCatch(
      tabtools:::tt_smallcells(C, k, E, S, RE, RS, CE, CS, ge, gs),
      tabtools_smallcells_uncertified = function(e) NULL
    )
    checked <- checked + 1L
    if (is.null(res)) {
      rejected <- rejected + 1L
      next
    }
    # A margin counts as published when flagged exact or when it carries a
    # marker; published cells are exact cells.
    maxv <- sum(C) + k
    ok <- sc_irredundant(C, res, E, k, maxv, RE, CE, ge)
    expect_true(ok, label = sprintf("case %d (profile %d, k %d, counts %s)", case, profile, k,
                                    paste(c(t(C)), collapse = ",")))
  }
  expect_identical(checked, n_cases)
  expect_lt(rejected, n_cases)
})
