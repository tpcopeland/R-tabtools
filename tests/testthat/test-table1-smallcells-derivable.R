# smallcells(): hidden counts that follow from printed ones (Stata tabtools
# 2.1.17, _desctab_collect.ado count blocks; desctab.sthlp "Counts that
# follow from printed ones are protected too"). A categorical variable's
# hidden missing count is N minus its printed levels; a binary variable's
# hidden negative and missing counts follow from the denominator a printed
# percentage (or n/N) releases. Such a count below k must be protected like a
# printed cell; when only withholding the shared group/total N could do it,
# the table is refused.

dc_cat <- function(a, b, ma, mb, ga = "x", gb = "y") {
  # Two groups; a/b are length-L level counts per group, ma/mb the missing counts.
  lev <- LETTERS[seq_along(a)]
  mk <- function(cnt, m, g) {
    v <- c(rep(lev, cnt), rep(NA_character_, m))
    data.frame(g = g, v = v, stringsAsFactors = FALSE)
  }
  d <- rbind(mk(a, ma, ga), mk(b, mb, gb))
  d$g <- factor(d$g)
  d$v <- factor(d$v, levels = lev)
  d
}

dc_bin <- function(pa, na, ma, pb, nb, mb) {
  mk <- function(p, n, m, g) data.frame(g = g, b = c(rep(1, p), rep(0, n), rep(NA_real_, m)))
  d <- rbind(mk(pa, na, ma, "x"), mk(pb, nb, mb, "y"))
  d$g <- factor(d$g)
  d
}

dc_ge <- "≥"

# Every printed sink of one table, flattened to text, for leak scans.
dc_sinks <- function(tt, dir) {
  csv <- file.path(dir, "t.csv")
  md <- file.path(dir, "t.md")
  xl <- file.path(dir, "t.xlsx")
  tt_write_csv(tt, csv)
  tt_write_markdown(tt, md)
  tt_write_xlsx(tt, xl, sheet = "T")
  xs <- tidyxl::xlsx_cells(xl)
  list(
    df = unlist(as.data.frame(tt)),
    console = utils::capture.output(print(tt)),
    csv = readLines(csv, encoding = "UTF-8"),
    md = readLines(md, encoding = "UTF-8"),
    xlsx = as.character(xs$character[!is.na(xs$character)])
  )
}

test_that("P0-2: a hidden categorical missing count derivable from N and the printed levels is masked", {
  dm <- data.frame(g = factor(rep(c("x", "y"), each = 14)),
                   v = factor(c(rep("A", 6), rep("B", 6), NA, NA, rep("A", 6), rep("B", 6), NA, NA)))
  tt <- table1_tc(dm, by = "g", vars = c(v = "cat"), smallcells = 5)
  df <- as.data.frame(tt)
  # N=14 with A 6 and B 6 printed would give missing = 2 < 5; one level per
  # group is now coded so the missing count keeps two feasible values.
  expect_equal(unname(colSums(matrix(grepl(dc_ge, unlist(df[4:5, 2:3])), 2))), c(1, 1))
  expect_identical(unname(unlist(df[3, 4])), "Suppressed")
  expect_false(any(grepl("%|\\(", unlist(df[4:5, 2:3]))))
  # Stored values: the suppression map and counts agree with the display.
  expect_gt(tt$stored$N_secondary_suppressed, 0)
  expect_identical(tt$stored$suppression["r3", "pvalue"], 3)
})

test_that("P0-3: a hidden binary negative count derivable from the percentage denominator is masked", {
  db <- data.frame(g = factor(rep(c("x", "y"), each = 12)), b = c(rep(1, 10), 0, 0, rep(1, 10), 0, 0))
  tt <- table1_tc(db, by = "g", vars = c(b = "bin"), smallcells = 5)
  df <- as.data.frame(tt)
  # N=12 with "10 (83)" gives negative = 2 < 5.
  expect_false(any(grepl("83|\\(", unlist(df[3, 2:3]))))
  expect_true(all(grepl(dc_ge, unlist(df[3, 2:3])) | unlist(df[3, 2:3]) == "10"))
  expect_false(any(unlist(df[3, 2:3]) == "10 (83)"))
  expect_identical(unname(unlist(df[3, 4])), "Suppressed")
  # slashN prints the denominator: still protected.
  tt2 <- table1_tc(db, by = "g", vars = c(b = "bin"), smallcells = 5, slashN = TRUE)
  expect_false(any(grepl("10/12", unlist(as.data.frame(tt2)))))
})

test_that("every sink hides the derivable count", {
  skip_if_not_installed("tidyxl")
  dm <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  tt <- table1_tc(dm, by = "g", vars = c(v = "cat"), smallcells = 5)
  s <- dc_sinks(tt, withr::local_tempdir())
  for (nm in names(s)) {
    txt <- paste(s[[nm]], collapse = "\n")
    expect_false(grepl("[0-9] \\([0-9]", txt), info = nm)
    expect_true(grepl(dc_ge, txt), info = nm)
    expect_true(grepl("Suppressed", txt), info = nm)
  }
  expect_true(all(is.na(tt$rows$p[tt$rows$type == "cat_header"])))
  if (requireNamespace("gt", quietly = TRUE)) {
    g <- tt_as_gt(tt)
    expect_false(grepl("[0-9] \\([0-9]", paste(unlist(g[["_data"]]), collapse = " ")))
    expect_true(grepl(dc_ge, paste(unlist(g[["_data"]]), collapse = " ")))
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    tn <- tt_as_tinytable(tt)
    expect_false(grepl("[0-9] \\([0-9]", paste(unlist(tn@data), collapse = " ")))
    expect_true(grepl(dc_ge, paste(unlist(tn@data), collapse = " ")))
  }
})

test_that("a hidden count at or above k stays unprotected and the table is unchanged", {
  dm <- dc_cat(c(10, 12), c(11, 11), 6, 7)
  df <- as.data.frame(table1_tc(dm, by = "g", vars = c(v = "cat"), smallcells = 5))
  expect_identical(unname(unlist(df[4, 2:3])), c("10 (45)", "11 (50)"))
  expect_false(any(grepl("Suppressed", unlist(df))))
})

test_that("total, slashN and missingsummary keep derivable counts protected", {
  dm <- dc_cat(c(8, 9), c(9, 10), 3, 1)
  for (opt in list(list(total = "after"), list(slashN = TRUE), list(missingsummary = TRUE),
                   list(total = "after", missingsummary = TRUE, slashN = TRUE))) {
    tt <- do.call(table1_tc, c(list(dm, by = "g", vars = c(v = "cat"), smallcells = 5), opt))
    df <- as.data.frame(tt)
    expect_identical(unname(unlist(df[3, ncol(df)])), "Suppressed", info = paste(names(opt), collapse = "+"))
    # The missing counts 3 and 1 never appear.
    body <- unlist(df[4:nrow(df), -1])
    expect_false(any(body %in% c("3", "1")), info = paste(names(opt), collapse = "+"))
  }
  # missingsummary shows the missing row as a coded cell.
  df <- as.data.frame(table1_tc(dm, by = "g", vars = c(v = "cat"), smallcells = 5, missingsummary = TRUE))
  expect_identical(unname(unlist(df[6, 2:3])), c("<5", "<5"))
})

test_that("variables sharing a group N: a count only the shared N could protect is refused", {
  # Group x has 3 missing; group y has none. Two variables print the same N.
  d <- dc_cat(c(8, 9), c(9, 11), 3, 0)
  d$w <- factor(rep(c("p", "q"), length.out = nrow(d)))
  err <- tryCatch(table1_tc(d, by = "g", vars = c(v = "cat", w = "cat"), smallcells = 5),
                  error = function(e) e)
  # Either the table is certified without withholding N, or it is refused
  # with the classed error; it is never released with N withheld.
  if (inherits(err, "error")) {
    expect_s3_class(err, "tabtools_error_smallcells_shared_margin")
    expect_match(conditionMessage(err), "group or total N")
  } else {
    expect_false(any(grepl("N=", as.data.frame(err)[2, 2:3]) == FALSE))
  }
  # P0: the shared-N refusal itself (Stata S11/RW11 analogue).
  a <- golden_fixture("auto")
  expect_error(table1_tc(a, by = "rep78", vars = "price contn \\ foreign bin", total = "after", smallcells = 5),
               class = "tabtools_error_smallcells_shared_margin")
  # With one variable the full search (withholding N) is allowed.
  expect_no_error(table1_tc(a, by = "rep78", vars = "foreign bin", total = "after", smallcells = 5))
})

test_that("wtcompare with percent-only weighted columns is refused under smallcells", {
  d <- dc_cat(c(10, 12), c(11, 11), 6, 7)
  d$w <- rep(c(1, 2), length.out = nrow(d))
  expect_error(table1_tc(d, by = "g", vars = c(v = "cat"), wt = "w", wtcompare = TRUE, smallcells = 5),
               "percent-only")
  expect_no_error(table1_tc(d, by = "g", vars = c(v = "cat"), wt = "w", wtcompare = TRUE, wtn = TRUE,
                            smallcells = 5))
  expect_no_error(table1_tc(d, by = "g", vars = c(v = "cat"), wt = "w", wtcompare = TRUE, percent_n = TRUE,
                            smallcells = 5))
})

# --- brute-force feasibility ---------------------------------------------------
#
# Enumerate every non-negative integer table consistent with what the table
# releases (cell ranges for "<k" and ">=k" markers, exact cells, the
# denominators implied by printed percentages and n/N, the group Ns and the
# total column) and require every primary count (a printed "<k" cell, or a
# hidden row below k) to take at least two values among them.

dc_parse <- function(x, k) {
  x <- trimws(x)
  r <- list(lo = 0, hi = Inf, pct = NA_real_, digits = NA_integer_, dlo = NA_real_, dhi = NA_real_, info = FALSE)
  if (!nzchar(x) || x == "Suppressed") return(r)
  rng <- function(s) {
    if (s == paste0("<", k)) return(c(1, k - 1))
    if (s == paste0(dc_ge, k)) return(c(k, Inf))
    if (grepl("^[0-9,]+$", s)) { v <- as.numeric(gsub(",", "", s)); return(c(v, v)) }
    NULL
  }
  m <- regmatches(x, regexec("^([0-9,]+) \\(([0-9]+(?:\\.([0-9]+))?)\\)$", x, perl = TRUE))[[1]]
  if (length(m)) {
    n <- as.numeric(gsub(",", "", m[2]))
    r$lo <- r$hi <- n
    r$pct <- as.numeric(m[3])
    r$digits <- if (nzchar(m[4])) nchar(m[4]) else 0L
    r$info <- TRUE
    return(r)
  }
  if (grepl("/", x, fixed = TRUE)) {
    p <- strsplit(x, "/", fixed = TRUE)[[1]]
    a <- rng(p[1]); b <- rng(p[2])
    if (!is.null(a)) { r$lo <- a[1]; r$hi <- a[2] }
    if (!is.null(b)) { r$dlo <- b[1]; r$dhi <- b[2] }
    r$info <- TRUE
    return(r)
  }
  a <- rng(x)
  if (!is.null(a)) { r$lo <- a[1]; r$hi <- a[2]; r$info <- TRUE }
  r
}

# type: "cat" (rows = levels, hidden or "Missing" row) or "bin".
# d: the data; tt: the table; returns number of primary counts checked, or
# NA when the table was refused.
dc_feasibility <- function(d, k, type, ..., var = if (type == "cat") "v" else "b") {
  tt <- table1_tc(d, by = "g", vars = stats::setNames(type, var), smallcells = k, ...)
  df <- as.data.frame(tt)
  G <- 2L
  total <- "Total" %in% unlist(df[1, ])
  lab <- trimws(df[[1]])
  body <- df[3:nrow(df), , drop = FALSE]
  blab <- trimws(body[[1]])
  L <- if (type == "cat") sum(!(blab %in% c("", "Missing", var))) else 1L
  has_miss_row <- "Missing" %in% blab
  slots <- if (type == "cat") c(paste0("L", seq_len(L)), "miss") else c("pos", "neg", "miss")
  S <- length(slots)
  truth <- lapply(split(d, d$g), function(x) {
    v <- x[[var]]
    if (type == "cat") c(vapply(levels(d[[var]])[seq_len(L)], function(l) sum(v == l, na.rm = TRUE), 0), sum(is.na(v)))
    else c(sum(v == 1, na.rm = TRUE), sum(v == 0, na.rm = TRUE), sum(is.na(v)))
  })
  Ng <- vapply(truth, sum, 0)
  cap <- max(Ng) + (if (total) k else 0) + 2
  # Released display per (slot, column).
  lvl_rows <- if (type == "cat") which(!(blab %in% c("", "Missing", var)) ) else which(blab == var)
  miss_row <- if (has_miss_row) which(blab == "Missing") else NA_integer_
  cols <- seq_len(G + total) + 1L
  cell <- function(row, col) dc_parse(body[row, col], k)
  hdrN <- lapply(seq_len(G + total), function(j) dc_parse(sub("^N=", "", df[2, cols[j]]), k))
  # All integer vectors up to cap for one group.
  grid <- as.matrix(expand.grid(rep(list(0:cap), S)))
  grid <- grid[rowSums(grid) <= cap, , drop = FALSE]
  sol <- vector("list", G)
  for (g in seq_len(G)) {
    ok <- rep(TRUE, nrow(grid))
    tot <- rowSums(grid)
    ok <- ok & tot >= hdrN[[g]]$lo & tot <= hdrN[[g]]$hi
    shown <- list()
    for (i in seq_along(lvl_rows)) shown[[slots[i]]] <- cell(lvl_rows[i], cols[g])
    if (!is.na(miss_row)) shown[["miss"]] <- cell(miss_row, cols[g])
    D <- if (type == "cat") tot - grid[, S] else grid[, 1] + grid[, 2]
    for (s in names(shown)) {
      x <- shown[[s]]
      col <- grid[, match(s, slots)]
      ok <- ok & col >= x$lo & col <= x$hi
      if (!is.na(x$pct)) ok <- ok & D > 0 & abs(col / D * 100 - x$pct) <= 0.5 * 10^(-x$digits) + 1e-9
      if (!is.na(x$dlo)) ok <- ok & D >= x$dlo & D <= x$dhi
    }
    sol[[g]] <- grid[ok, , drop = FALSE]
  }
  # Group solutions independent unless a total column couples them.
  if (total) {
    ia <- rep(seq_len(nrow(sol[[1]])), times = nrow(sol[[2]]))
    ib <- rep(seq_len(nrow(sol[[2]])), each = nrow(sol[[1]]))
    sm <- sol[[1]][ia, , drop = FALSE] + sol[[2]][ib, , drop = FALSE]
    ok <- rep(TRUE, nrow(sm))
    n_all <- rowSums(sm)
    ok <- ok & n_all >= hdrN[[3]]$lo & n_all <= hdrN[[3]]$hi
    for (i in seq_along(lvl_rows)) {
      x <- cell(lvl_rows[i], cols[3])
      ok <- ok & sm[, i] >= x$lo & sm[, i] <= x$hi
    }
    if (!is.na(miss_row)) {
      x <- cell(miss_row, cols[3])
      ok <- ok & sm[, S] >= x$lo & sm[, S] <= x$hi
    }
    keep <- list(ia[ok], ib[ok])
    sol <- list(sol[[1]][keep[[1]], , drop = FALSE], sol[[2]][keep[[2]], , drop = FALSE])
  }
  checked <- 0L
  for (g in seq_len(G)) {
    tr <- truth[[g]]
    expect_true(any(colSums(t(sol[[g]]) == tr) == S), info = "the true table is feasible")
    for (s in seq_len(S)) {
      shown <- if (s <= length(lvl_rows)) lvl_rows[s] else if (slots[s] == "miss" && !is.na(miss_row)) miss_row else NA
      primary <- tr[s] >= 1 && tr[s] < k && (is.na(shown) || grepl(paste0("^<", k, "$"), trimws(body[shown, cols[g]])))
      if (!primary) next
      checked <- checked + 1L
      nvals <- length(unique(sol[[g]][, s]))
      expect_gte(nvals, 2L, label = sprintf("feasible values of %s in group %d (%s)", slots[s], g,
                                           paste(unlist(truth), collapse = ",")))
    }
  }
  checked
}

test_that("brute force: every primary count keeps at least two feasible values (categorical)", {
  k <- 5
  set.seed(20261006)
  total_checked <- 0L
  n_tables <- 0L
  for (i in 1:14) {
    L <- 2L
    a <- sample(2:9, L, TRUE); b <- sample(2:9, L, TRUE)
    ma <- sample(0:4, 1); mb <- sample(0:4, 1)
    d <- dc_cat(a, b, ma, mb)
    for (opt in list(list(), list(missingsummary = TRUE), list(slashN = TRUE))) {
      r <- tryCatch(do.call(dc_feasibility, c(list(d, k, "cat"), opt)),
                    tabtools_error_smallcells = function(e) NA_integer_)
      if (!is.na(r)) { total_checked <- total_checked + r; n_tables <- n_tables + 1L }
    }
  }
  expect_gt(n_tables, 20L)
  expect_gt(total_checked, 10L)
})

test_that("brute force: binary variables and a total column", {
  k <- 5
  set.seed(7)
  total_checked <- 0L
  for (i in 1:10) {
    d <- dc_bin(sample(3:12, 1), sample(0:4, 1), sample(0:3, 1), sample(3:12, 1), sample(0:4, 1), sample(0:3, 1))
    for (opt in list(list(), list(slashN = TRUE), list(missingsummary = TRUE))) {
      r <- tryCatch(do.call(dc_feasibility, c(list(d, k, "bin"), opt)),
                    tabtools_error_smallcells = function(e) NA_integer_)
      if (!is.na(r)) total_checked <- total_checked + r
    }
  }
  expect_gt(total_checked, 5L)
  # Total column with small groups (the group N may be withheld for one variable).
  for (i in 1:5) {
    d <- dc_cat(sample(1:5, 2, TRUE), sample(1:5, 2, TRUE), sample(0:3, 1), sample(0:3, 1))
    r <- tryCatch(dc_feasibility(d, k, "cat", total = "after"),
                  tabtools_error_smallcells = function(e) NA_integer_)
    expect_true(is.na(r) || r >= 0L)
  }
})

test_that("literal variable labels survive in a protected table", {
  d <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  names(d)[2] <- "v$x"
  tt <- table1_tc(d, by = "g", vars = c("v$x" = "cat"), smallcells = 5, labels = c("v$x" = "=Cost `a` \\ \"q\""))
  expect_true(any(grepl("=Cost `a`", as.data.frame(tt)[[1]], fixed = TRUE)))
})
