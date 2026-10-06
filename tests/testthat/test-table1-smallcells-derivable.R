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

dc_ge <- "\u2265"

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
  # The sample-accounting ledger (meta and the as.data.frame attribute) is
  # a sink too: no count of the protected variable or withheld N survives.
  for (led in list(tt$meta$sample_accounting, attr(as.data.frame(tt), "sample_accounting"))) {
    m <- led$measures
    vm <- m[grepl("^variable/1/", m$population_id), ]
    expect_true(all(is.na(vm$value[vm$status != "not_applicable"])))
    expect_false(any(vm$metric == "missing_n" & !is.na(vm$value)))
    expect_true(all(is.na(led$exclusions$n[grepl("^variable/1/", led$exclusions$population_id)])))
  }
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

test_that("the ledger keeps counts for unsuppressed variables and masks withheld group N", {
  # v is protected (missing 2 derivable); w is not (all counts >= k).
  d <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  d$w <- factor(rep(c("p", "q"), length.out = nrow(d)))
  tt <- table1_tc(d, by = "g", vars = c(v = "cat", w = "cat"), smallcells = 5)
  m <- tt$meta$sample_accounting$measures
  get <- function(id, met) m$value[m$population_id == id & m$metric == met]
  expect_true(is.na(get("variable/1/group/1", "missing_n")))
  expect_true(is.na(get("variable/1/group/1", "observed_n")))
  expect_equal(get("variable/2/group/1", "observed_n"), 14)
  expect_equal(get("variable/2/group/1", "missing_n"), 0)
  expect_equal(get("group/1", "input_n"), 14)
  # One variable, N withheld by the engine (total column): the group N and
  # every population holding it are masked.
  dm <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  t2 <- table1_tc(dm, by = "g", vars = c(v = "cat"), smallcells = 5, total = "after")
  expect_true(grepl(dc_ge, as.data.frame(t2)[2, 2]))
  m2 <- t2$meta$sample_accounting$measures
  expect_true(all(is.na(m2$value[m2$population_id %in% c("table", "group/1", "group/2") &
                                  m2$metric %in% c("input_n", "eligible_n", "used_n", "missing_n")])))
  # Without smallcells nothing is masked.
  m3 <- table1_tc(dm, by = "g", vars = c(v = "cat"))$meta$sample_accounting$measures
  expect_equal(m3$value[m3$population_id == "group/1" & m3$metric == "input_n"], 14)
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

test_that("variables sharing a group N: a count the shared N need not protect is certified, the rest refused", {
  # Group x has 3 missing, group y none; two variables print the same N. The
  # engine cannot withhold N, but A's complement alone protects the missing
  # count, so the table is certified with N shown (deterministic).
  d <- dc_cat(c(8, 9), c(9, 11), 3, 0)
  d$w <- factor(rep(c("p", "q"), length.out = nrow(d)))
  df <- as.data.frame(table1_tc(d, by = "g", vars = c(v = "cat", w = "cat"), smallcells = 5))
  expect_identical(unname(unlist(df[2, 2:3])), c("N=20", "N=20"))
  expect_identical(unname(unlist(df[4, 2:3])), c(paste0(dc_ge, "5"), "9"))
  expect_identical(unname(unlist(df[3, 4])), "Suppressed")
  expect_identical(unname(unlist(df[7, 2:3])), c("10 (50)", "10 (50)"))
  # The shared-N refusal itself (Stata S11/RW11 analogue), with the variable
  # named as Stata does.
  a <- golden_fixture("auto")
  err <- expect_error(table1_tc(a, by = "rep78", vars = "price contn \\ foreign bin", total = "after", smallcells = 5),
                      class = "tabtools_error_smallcells_shared_margin")
  expect_match(conditionMessage(err), "Variable price:", fixed = TRUE)
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
# Enumerate every non-negative integer table consistent with everything the
# table releases and require every primary count (a printed "<k" cell, or a
# hidden negative/missing row below k) to take at least two values. Released:
# exact cells; "<k" / ">=k" ranges; group and total N headers; the
# denominators implied by "n (pct)" and printed by "n/den" (group and Total
# columns); Total-column cells. Variables share each group's N (a one-variable
# table withholds it freely; with two, the N of each group must be reachable
# by both variables). The total-N header couples groups through each
# variable's own sums only (a relaxation: it can miss, never invent, a leak).

dc_parse <- function(x, k) {
  x <- trimws(x)
  r <- list(lo = 0, hi = Inf, pct = NA_real_, digits = NA_integer_, dlo = NA_real_, dhi = NA_real_)
  if (!nzchar(x) || x == "Suppressed") return(r)
  rng <- function(s) {
    if (s == paste0("<", k)) return(c(1, k - 1))
    if (s == paste0(dc_ge, k)) return(c(k, Inf))
    if (grepl("^[0-9,]+$", s)) { v <- as.numeric(gsub(",", "", s)); return(c(v, v)) }
    NULL
  }
  m <- regmatches(x, regexec("^([0-9,]+) \\(([0-9]+(?:\\.([0-9]+))?)\\)$", x, perl = TRUE))[[1]]
  if (length(m)) {
    r$lo <- r$hi <- as.numeric(gsub(",", "", m[2]))
    r$pct <- as.numeric(m[3])
    r$digits <- if (nzchar(m[4])) nchar(m[4]) else 0L
    return(r)
  }
  if (grepl("/", x, fixed = TRUE)) {
    p <- strsplit(x, "/", fixed = TRUE)[[1]]
    a <- rng(p[1]); b <- rng(p[2])
    if (!is.null(a)) { r$lo <- a[1]; r$hi <- a[2] }
    if (!is.null(b)) { r$dlo <- b[1]; r$dhi <- b[2] }
    return(r)
  }
  a <- rng(x)
  if (!is.null(a)) { r$lo <- a[1]; r$hi <- a[2] }
  r
}

# (The missingsummary row's percentage is of the group N, not of the non-missing count.)
# Does the slot value `n` (vector) under denominator `D` satisfy the parsed cell?
dc_ok <- function(x, n, D) {
  ok <- n >= x$lo & n <= x$hi
  if (!is.na(x$pct)) ok <- ok & D > 0 & abs(n / D * 100 - x$pct) <= 0.5 * 10^(-x$digits) + 1e-9
  if (!is.na(x$dlo)) ok <- ok & D >= x$dlo & D <= x$dhi
  ok
}

dc_key <- function(m, base) as.vector(m %*% base^(seq_len(ncol(m)) - 1))

dc_msum <- function(A, B) {
  S <- A[rep(seq_len(nrow(A)), times = nrow(B)), , drop = FALSE] +
    B[rep(seq_len(nrow(B)), each = nrow(A)), , drop = FALSE]
  S[!duplicated(S), , drop = FALSE]
}

# vars: named character vector of types ("cat"/"bin"); d$g has 2 or 3 groups.
# Returns the number of primary counts checked, or NA if the table was refused.
dc_feasibility <- function(d, k, vars, ...) {
  tt <- tryCatch(table1_tc(d, by = "g", vars = vars, smallcells = k, ...),
                 tabtools_error_smallcells = function(e) NULL)
  if (is.null(tt)) return(NA_integer_)
  df <- as.data.frame(tt)
  G <- nlevels(d$g)
  total <- "Total" %in% unlist(df[1, ])
  cols <- seq_len(G + total) + 1L
  blab <- trimws(df[[1]][-(1:2)])
  body <- df[-(1:2), , drop = FALSE]
  truth_g <- split(d, d$g)
  # Row layout of each variable.
  pos <- 1L
  info <- list()
  for (nm in names(vars)) {
    type <- vars[[nm]]
    if (type == "cat") {
      # A level with no records is not listed, so it is not a slot.
      lv <- levels(d[[nm]])[table(d[[nm]]) > 0]
      stopifnot(blab[pos] == nm)
      rows <- pos + seq_along(lv)
      pos <- pos + 1L + length(lv)
      slots <- c(paste0("L", seq_along(lv)), "miss")
    } else {
      stopifnot(blab[pos] == nm)
      rows <- pos
      pos <- pos + 1L
      slots <- c("pos", "neg", "miss")
      lv <- NULL
    }
    miss_row <- NA_integer_
    if (pos <= length(blab) && blab[pos] == "Missing") { miss_row <- pos; pos <- pos + 1L }
    truth <- lapply(truth_g, function(x) {
      v <- x[[nm]]
      if (type == "cat") c(vapply(lv, function(l) sum(v == l, na.rm = TRUE), 0), sum(is.na(v)))
      else c(sum(v == 1, na.rm = TRUE), sum(v == 0, na.rm = TRUE), sum(is.na(v)))
    })
    info[[nm]] <- list(type = type, rows = rows, miss_row = miss_row, slots = slots, S = length(slots), truth = truth)
  }
  Ng <- vapply(truth_g, nrow, 0)
  cap <- max(Ng) + k
  hdrN <- lapply(cols, function(j) dc_parse(sub("^N=", "", df[2, j]), k))
  denom <- function(inf, M) if (inf$type == "cat") rowSums(M) - M[, inf$S] else M[, 1] + M[, 2]
  group_ok <- function(inf, g, M) {
    ok <- rowSums(M) >= hdrN[[g]]$lo & rowSums(M) <= hdrN[[g]]$hi
    D <- denom(inf, M)
    for (i in seq_along(inf$rows)) ok <- ok & dc_ok(dc_parse(body[inf$rows[i], cols[g]], k), M[, i], D)
    if (!is.na(inf$miss_row)) ok <- ok & dc_ok(dc_parse(body[inf$miss_row, cols[g]], k), M[, inf$S], rowSums(M))
    ok
  }
  total_ok <- function(inf, Tm) {
    ok <- rowSums(Tm) >= hdrN[[G + 1L]]$lo & rowSums(Tm) <= hdrN[[G + 1L]]$hi
    D <- denom(inf, Tm)
    for (i in seq_along(inf$rows)) ok <- ok & dc_ok(dc_parse(body[inf$rows[i], cols[G + 1L]], k), Tm[, i], D)
    if (!is.na(inf$miss_row)) ok <- ok & dc_ok(dc_parse(body[inf$miss_row, cols[G + 1L]], k), Tm[, inf$S], rowSums(Tm))
    ok
  }
  sol <- lapply(info, function(inf) {
    grid <- as.matrix(expand.grid(rep(list(0:cap), inf$S)))
    grid <- grid[rowSums(grid) <= cap, , drop = FALSE]
    lapply(seq_len(G), function(g) grid[group_ok(inf, g, grid), , drop = FALSE])
  })
  # Shared group N: the sums every variable can reach.
  for (g in seq_len(G)) {
    nset <- Reduce(intersect, lapply(sol, function(s) unique(rowSums(s[[g]]))))
    for (nm in names(sol)) sol[[nm]][[g]] <- sol[[nm]][[g]][rowSums(sol[[nm]][[g]]) %in% nset, , drop = FALSE]
  }
  if (total) {
    for (nm in names(sol)) {
      inf <- info[[nm]]
      new <- sol[[nm]]
      for (g in seq_len(G)) {
        others <- Reduce(dc_msum, sol[[nm]][-g])
        keep <- vapply(seq_len(nrow(sol[[nm]][[g]])), function(r) {
          any(total_ok(inf, sweep(others, 2, sol[[nm]][[g]][r, ], `+`)))
        }, NA)
        new[[g]] <- sol[[nm]][[g]][keep, , drop = FALSE]
      }
      sol[[nm]] <- new
    }
  }
  checked <- 0L
  for (nm in names(info)) {
    inf <- info[[nm]]
    for (g in seq_len(G)) {
      tr <- inf$truth[[g]]
      S <- sol[[nm]][[g]]
      expect_true(any(colSums(t(S) == tr) == inf$S), info = "the true table is feasible")
      for (s in seq_len(inf$S)) {
        shown <- if (s <= length(inf$rows)) inf$rows[s] else if (inf$slots[s] == "miss" && !is.na(inf$miss_row)) inf$miss_row else NA_integer_
        primary <- tr[s] >= 1 && tr[s] < k &&
          (is.na(shown) || trimws(body[shown, cols[g]]) == paste0("<", k))
        if (!primary) next
        checked <- checked + 1L
        expect_gte(length(unique(S[, s])), 2L,
                   label = sprintf("feasible values of %s/%s in group %d (truth %s; opts %s)", nm, inf$slots[s], g,
                                   paste(unlist(inf$truth), collapse = ","), paste(names(list(...)), collapse = "+")))
      }
    }
  }
  checked
}

# A random table: G groups, L-level categorical `v`, binary `b`, optional second
# categorical `w`; sizes keep the enumeration small.
dc_random <- function(G, second = FALSE, L = 2L) {
  one <- function(g) {
    lv <- sample(0:7, L, TRUE)
    m <- sample(0:4, 1)
    n <- max(sum(lv) + m, 2L)
    lv[1] <- lv[1] + (n - sum(lv) - m)
    p <- sample(0:n, 1)
    mb <- sample(0:min(3, n - p), 1)
    out <- data.frame(g = paste0("g", g),
      v = factor(c(rep(LETTERS[seq_len(L)], lv), rep(NA, m)), levels = LETTERS[seq_len(L)]),
      b = sample(c(rep(1, p), rep(0, n - p - mb), rep(NA_real_, mb))))
    if (second) {
      mw <- sample(0:min(4, n - 1), 1)
      a <- sample(1:(n - mw), 1)
      out$w <- factor(sample(c(rep("p", a), rep("q", n - mw - a), rep(NA_character_, mw))), levels = c("p", "q"))
    }
    out
  }
  d <- do.call(rbind, lapply(seq_len(G), one))
  d$g <- factor(d$g)
  d
}

test_that("brute force: every primary count keeps two feasible values (no slashN)", {
  k <- 3
  set.seed(20261006)
  checked <- 0L
  tables <- 0L
  runs <- list(
    list(G = 2, vars = c(v = "cat"), opts = list()),
    list(G = 2, vars = c(v = "cat"), opts = list(missingsummary = TRUE)),
    list(G = 2, vars = c(b = "bin"), opts = list()),
    list(G = 2, vars = c(b = "bin"), opts = list(missingsummary = TRUE)),
    list(G = 2, vars = c(v = "cat"), opts = list(total = "after")),
    list(G = 2, vars = c(b = "bin"), opts = list(total = "after")),
    list(G = 3, vars = c(v = "cat"), opts = list()),
    list(G = 3, vars = c(b = "bin"), opts = list(total = "after")),
    list(G = 2, vars = c(v = "cat", b = "bin"), opts = list()),
    list(G = 2, vars = c(v = "cat", b = "bin"), opts = list(total = "after")),
    list(G = 3, vars = c(v = "cat", b = "bin"), opts = list(missingsummary = TRUE))
  )
  for (r in runs) {
    for (rep in 1:5) {
      d <- dc_random(r$G)
      res <- do.call(dc_feasibility, c(list(d, k, r$vars), r$opts))
      if (!is.na(res)) { checked <- checked + res; tables <- tables + 1L }
    }
  }
  expect_gt(tables, 30L)
  expect_gt(checked, 20L)
})

test_that("brute force: the P0-2 table is checked and passes", {
  # N=14, A 6, B 6 released exactly would pin missing = 2 (the P0-2 table).
  d <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  tt <- table1_tc(d, by = "g", vars = c(v = "cat"), smallcells = 5, missingsummary = TRUE)
  expect_identical(unname(unlist(as.data.frame(tt)[6, 2:3])), c("<5", "<5"))
  expect_gt(dc_feasibility(d, 5, c(v = "cat")), 0L)
})

test_that("brute force: known slashN denominator leak 1 (shared with Stata 2.5.1)", {
  skip("Known slashN denominator leak shared with Stata 2.5.1; see Stata-Dev _take_action 2026-10-06-tabtools-smallcells-slashN-denominator-leak.md")
  mk <- function(g, v) data.frame(g = g, v = v)
  d <- rbind(mk("x", c(3, 3, 1, 1)), mk("y", c(NA, 1, 1, 1, 3, 1, 1, 3, 1)),
             mk("z", c(2, NA, 2, 1, NA, NA, 1, NA, 1)))
  d$g <- factor(d$g)
  d$v <- factor(d$v, levels = 1:3)
  expect_true(!is.na(dc_feasibility(d, 3, c(v = "cat"), slashN = TRUE)))
})

test_that("brute force: known slashN denominator leak 2 (shared with Stata 2.5.1)", {
  skip("Known slashN denominator leak shared with Stata 2.5.1; see Stata-Dev _take_action 2026-10-06-tabtools-smallcells-slashN-denominator-leak.md")
  mk <- function(g, p, n) data.frame(g = g, b = c(rep(1, p), rep(0, n)))
  d <- rbind(mk("x", 6, 0), mk("y", 10, 3), mk("z", 4, 1))
  d$g <- factor(d$g)
  expect_true(!is.na(dc_feasibility(d, 3, c(b = "bin"), slashN = TRUE, catrowperc = TRUE, total = "after")))
})

test_that("brute force: slashN random sweep (known leak; remove the skip with the Stata fix)", {
  skip("Known slashN denominator leak shared with Stata 2.5.1; see Stata-Dev _take_action 2026-10-06-tabtools-smallcells-slashN-denominator-leak.md")
  set.seed(1)
  for (i in 1:30) {
    d <- dc_random(sample(2:3, 1), second = i %% 2 == 0)
    vars <- if (i %% 2 == 0) c(v = "cat", b = "bin") else c(v = "cat")
    dc_feasibility(d, 3, vars, slashN = TRUE, total = "after")
  }
})

test_that("literal variable labels survive in a protected table", {
  d <- dc_cat(c(6, 6), c(6, 6), 2, 2)
  names(d)[2] <- "v$x"
  tt <- table1_tc(d, by = "g", vars = c("v$x" = "cat"), smallcells = 5, labels = c("v$x" = "=Cost `a` \\ \"q\""))
  expect_true(any(grepl("=Cost `a`", as.data.frame(tt)[[1]], fixed = TRUE)))
})
