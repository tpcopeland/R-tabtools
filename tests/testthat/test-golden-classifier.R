# Parity for the building blocks behind auto-typing and number formatting
# (tasks 0.5b, 0.5c; implemented in tasks 1.1, 2.4, 2.4a, 2.4b).

skip_until_phase <- function(phase) {
  if (phase > golden_phase_done) skip(paste("Phase", phase))
}

test_that("stata_fmt() reproduces Stata string(x, fmt)", {
  skip_until_phase(1)
  fm <- utils::read.csv(golden_path("stata_fmt.csv"), colClasses = "character")
  s <- fm[fm$kind == "string", ]
  got <- mapply(function(x, f) tabtools:::stata_fmt(exact_num(x), f), s$x, s$fmt, USE.NAMES = FALSE)
  expect_identical(got, s$result)
})

test_that("stata_round() reproduces Stata round(x, u) and the regtab estimate text", {
  skip_until_phase(1)
  fm <- utils::read.csv(golden_path("stata_fmt.csv"), colClasses = "character")
  r <- fm[fm$kind == "round", ]
  got <- mapply(function(x, u) tabtools:::stata_round(exact_num(x), exact_num(u)), r$x, r$u, USE.NAMES = FALSE)
  # The golden `result` passed through a Stata local macro (`local r =
  # round(...)`), which stores numbers as %18.0g text; compare the same way.
  expect_identical(as.numeric(tabtools:::stata_fmt(got, "%18.0g")), as.numeric(r$result))
  d <- round(-log10(as.numeric(r$u)))
  fmt <- paste0("%12.", d, "f")
  expect_identical(mapply(tabtools:::stata_fmt, got, fmt, USE.NAMES = FALSE), r$result_fmt)
  h <- fm[fm$kind == "headerperc", ]
  nd <- do.call(rbind, strsplit(h$x, "/", fixed = TRUE))
  hv <- tabtools:::stata_round(as.numeric(nd[, 1]) / as.numeric(nd[, 2]), 0.001) * 100
  expect_identical(tabtools:::stata_fmt(hv, "%9.1f"), h$result_fmt)
})

# Run as soon as the function exists (tasks 2.4, 2.4a, 2.4b), ahead of the
# rest of Phase 2.
skip_until_defined <- function(fn) {
  skip_if_not(exists(fn, envir = asNamespace("tabtools"), inherits = FALSE),
              paste(fn, "not implemented yet"))
}

test_that("stata_runiform() reproduces Stata's mt64 runiform() streams", {
  skip_until_defined("stata_runiform")
  rng <- utils::read.csv(golden_path("rng_mt64.csv"))
  for (seed in unique(rng$seed)) {
    want <- rng$u[rng$seed == seed]
    expect_equal(tabtools:::stata_runiform(length(want), seed), want, tolerance = 1e-15)
  }
})

test_that("the swilk subsample picks Stata's 2,000 rows", {
  skip_until_defined("tt_swilk_subsample")
  ss <- utils::read.csv(golden_path("subsample_ids.csv"))
  d <- golden_fixture("cohort3500")
  for (v in unique(ss$variable)) {
    rows <- tabtools:::tt_swilk_subsample(d[[v]])
    expect_identical(as.numeric(d$id[rows]), as.numeric(ss$id[ss$variable == v]), label = v)
  }
})

# swilk returns its results through macros (see R/swilk.R), so the golden
# values carry Stata's macro precision: 16 significant digits, 12 in
# e-notation (|x| < 1e-5). The port reproduces that rounding; the remaining
# differences are last-digit flips, so compare at relative 1e-10.
test_that("swilk port reproduces Stata W, V, z, p", {
  skip_until_defined("tt_swilk")
  sw <- utils::read.csv(golden_path("swilk.csv"), colClasses = "character")
  num <- function(s) suppressWarnings(as.numeric(s))
  inline <- list(ties3 = c(1, 1, 2), const3 = c(5, 5, 5))
  fixtures <- list()
  for (i in seq_len(nrow(sw))) {
    r <- sw[i, ]
    x <- if (r$fixture == "inline") {
      inline[[r$case]]
    } else {
      if (is.null(fixtures[[r$fixture]])) fixtures[[r$fixture]] <- golden_fixture(r$fixture)
      x <- fixtures[[r$fixture]][[r$variable]]
      switch(sub("[0-9]+$", "", r$case),
        full = x,
        first = x[seq_len(as.integer(sub("^first", "", r$case)))],
        subsample = x[tabtools:::tt_swilk_subsample(x)]
      )
    }
    lab <- paste(r$case, r$fixture, r$variable)
    got <- tabtools:::tt_swilk(x)
    want <- num(unlist(r[c("W", "V", "z", "p")]))
    have <- unname(unlist(got[c("W", "V", "z", "p")]))
    expect_identical(got$n, as.integer(r$n), label = lab)
    expect_identical(got$rc, as.integer(r$rc), label = lab)
    expect_identical(is.na(have), is.na(want), label = lab)
    ok <- !is.na(want)
    expect_true(all(abs(have[ok] - want[ok]) <= 1e-10 * abs(want[ok])), label = lab)
  }
})

test_that("auto-typing matches _tabtools_detect_vartype", {
  skip_until_defined("tt_detect_vartype")
  at <- utils::read.csv(golden_path("autotype.csv"))
  for (i in seq_len(nrow(at))) {
    d <- golden_fixture(at$fixture[i])
    expect_identical(tabtools:::tt_detect_vartype(d[[at$variable[i]]]), at$type[i],
                     label = paste(at$fixture[i], at$variable[i]))
  }
  dv <- utils::read.csv(golden_path("detect_vartype.csv"))
  d <- golden_fixture("vartype_branches")
  for (i in seq_len(nrow(dv))) {
    expect_identical(tabtools:::tt_detect_vartype(d[[dv$variable[i]]]), dv$type[i], label = dv$branch[i])
  }
})
