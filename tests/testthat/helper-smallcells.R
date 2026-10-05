# Helpers for the small-cell engine tests (plan tasks 3.5, 3.6).

sc_num <- function(s) {
  if (!nzchar(s)) return(numeric())
  as.numeric(strsplit(s, ";", fixed = TRUE)[[1]])
}
sc_mat <- function(s, nr, nc) matrix(sc_num(s), nr, nc, byrow = TRUE)

# Full-flag block: every cell and margin published and sensitive (the
# validation_smallcells.do V1-V4/V8 configuration).
sc_full <- function(counts, k, grand = 1) {
  nr <- nrow(counts)
  nc <- ncol(counts)
  tabtools:::tt_smallcells(counts, k, rowexact = rep(1, nr), rowsensitive = rep(1, nr),
                           colexact = rep(1, nc), colsensitive = rep(1, nc),
                           grandexact = grand, grandsensitive = grand)
}

# Independent reconstruction oracle (port of validation_smallcells.do
# `_vsc_certify`, generalised to unpublished cells and margins): enumerate
# every table consistent with the published values and marker ranges,
# candidates bounded by `maxv`, and report whether every primary cell and
# margin still has at least two feasible values. Tables are built row by row:
# each row's cells come from their own ranges and must satisfy the row
# margin, and partial column sums are pruned against the column bounds, so
# the search stays small without changing the answer.
sc_certify <- function(counts, mask, exact, rowmask, colmask, totalmask, k, maxv,
                       rowexact = rowmask * 0 + 1, colexact = colmask * 0 + 1,
                       grandexact = 1) {
  nr <- nrow(counts)
  nc <- ncol(counts)
  cell_range <- function(i, j) {
    m <- mask[i, j]
    if (m == 1) return(seq_len(k - 1))
    if (m == 2) return(if (maxv >= k) k:maxv else integer())
    if (exact[i, j] == 1) return(counts[i, j])
    0:maxv
  }
  # Allowed interval for a margin: exact value, a marker range, or free.
  margin_iv <- function(actual, code, published) {
    if (code == 1) return(c(1, k - 1))
    if (code == 2) return(c(k, Inf))
    if (published == 1) return(c(actual, actual))
    c(0, Inf)
  }
  riv <- lapply(seq_len(nr), function(i) margin_iv(sum(counts[i, ]), rowmask[i], rowexact[i]))
  civ <- lapply(seq_len(nc), function(j) margin_iv(sum(counts[, j]), colmask[j], colexact[j]))
  giv <- margin_iv(sum(counts), totalmask, grandexact)
  col_hi <- vapply(civ, `[`, 0, 2)

  acc <- NULL
  for (i in seq_len(nr)) {
    ranges <- lapply(seq_len(nc), function(j) cell_range(i, j))
    if (any(lengths(ranges) == 0L)) return(FALSE)
    rows <- as.matrix(expand.grid(ranges, KEEP.OUT.ATTRS = FALSE))
    rs <- rowSums(rows)
    rows <- rows[rs >= riv[[i]][1] & rs <= riv[[i]][2], , drop = FALSE]
    if (!nrow(rows)) stop("oracle: the actual table is not feasible")
    if (is.null(acc)) {
      acc <- rows
    } else {
      idx <- expand.grid(a = seq_len(nrow(acc)), b = seq_len(nrow(rows)))
      acc <- cbind(acc[idx$a, , drop = FALSE], rows[idx$b, , drop = FALSE])
    }
    colsum_sofar <- vapply(seq_len(nc), function(j) {
      rowSums(acc[, j + nc * (seq_len(i) - 1L), drop = FALSE])
    }, numeric(nrow(acc)))
    colsum_sofar <- matrix(colsum_sofar, nrow(acc))
    keep <- apply(sweep(colsum_sofar, 2L, col_hi, `<=`), 1L, all)
    acc <- acc[keep, , drop = FALSE]
    if (!nrow(acc)) stop("oracle: the actual table is not feasible")
  }
  colsums <- matrix(vapply(seq_len(nc), function(j) rowSums(acc[, j + nc * (seq_len(nr) - 1L), drop = FALSE]),
                           numeric(nrow(acc))), nrow(acc))
  keep <- rep(TRUE, nrow(acc))
  for (j in seq_len(nc)) keep <- keep & colsums[, j] >= civ[[j]][1] & colsums[, j] <= civ[[j]][2]
  tot <- rowSums(acc)
  keep <- keep & tot >= giv[1] & tot <= giv[2]
  if (!any(keep)) stop("oracle: the actual table is not feasible")
  g <- acc[keep, , drop = FALSE]
  varies <- function(v) length(unique(v)) >= 2L
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (mask[i, j] == 1 && !varies(g[, (i - 1L) * nc + j])) return(FALSE)
    }
    if (rowmask[i] == 1 && !varies(rowSums(g[, (i - 1L) * nc + seq_len(nc), drop = FALSE]))) return(FALSE)
  }
  for (j in seq_len(nc)) {
    if (colmask[j] == 1 && !varies(rowSums(g[, j + nc * (seq_len(nr) - 1L), drop = FALSE]))) return(FALSE)
  }
  if (totalmask == 1 && !varies(rowSums(g))) return(FALSE)
  TRUE
}

# Every complementary marker is necessary: revealing any one of them (cell,
# row margin, column margin, grand total) lets a reader pin down a primary
# (`_vsc_assert_irredundant`).
sc_irredundant <- function(counts, res, exact, k, maxv, rowexact, colexact, grandexact) {
  cert <- function(m, rm, cm, tm) sc_certify(counts, m, exact, rm, cm, tm, k, maxv, rowexact, colexact, grandexact)
  if (!cert(res$mask, res$rowmask, res$colmask, res$totalmask)) return(FALSE)
  for (p in which(res$mask == 2)) {
    m <- res$mask
    m[p] <- 0
    if (cert(m, res$rowmask, res$colmask, res$totalmask)) return(FALSE)
  }
  for (i in which(res$rowmask == 2)) {
    rm <- res$rowmask
    rm[i] <- 0
    if (cert(res$mask, rm, res$colmask, res$totalmask)) return(FALSE)
  }
  for (j in which(res$colmask == 2)) {
    cm <- res$colmask
    cm[j] <- 0
    if (cert(res$mask, res$rowmask, cm, res$totalmask)) return(FALSE)
  }
  if (res$totalmask == 2 && cert(res$mask, res$rowmask, res$colmask, 0)) return(FALSE)
  TRUE
}

# Reference assembly of Stata's display `suppression` matrix for the simple
# table1_tc layouts in goldens T25-T30n: header row 1 (all 0), the N row (the
# combined column-margin mask), then per variable one row (continuous,
# binary) or a label row plus level rows (categorical); the p-value column
# carries 3 on each suppressed variable's first row. Mirrors
# _desctab_collect.ado:867-1070 and desctab.ado:988-1053 for these cases
# only (no missingsummary, slashN, ESS, or wtcompare rows); the general
# layout is .t1_sc_stored() (R/table1_smallcells.R).
sc_display_matrix <- function(vars, pvalue = TRUE) {
  sample <- tabtools:::tt_sc_sample_mask(vars)
  ncolg <- length(sample)
  rows <- list(c(rep(0, ncolg), if (pvalue) 0), c(sample, if (pvalue) 0))
  for (v in vars) {
    d <- if (pvalue) (if (v$derived) 3 else 0) else NULL
    if (v$kind %in% c("cont", "bin")) {
      rows <- c(rows, list(c(v$cells[1, ], d)))
    } else {
      rows <- c(rows, list(c(rep(0, ncolg), d)))
      for (r in seq_len(nrow(v$cells))) rows <- c(rows, list(c(v$cells[r, ], if (pvalue) 0)))
    }
  }
  do.call(rbind, rows)
}

# Blocks for one variable from a fixture, grouped by `by` (missing by values
# dropped, as table1_tc does). Codes of a categorical variable are its sorted
# observed values.
sc_var_from_data <- function(d, by, var, type, total = FALSE, k = 5) {
  d <- d[!is.na(d[[by]]), , drop = FALSE]
  groups <- sort(unique(d[[by]]))
  sample_n <- vapply(groups, function(g) sum(d[[by]] == g), 0)
  x <- d[[var]]
  nonmiss <- vapply(groups, function(g) sum(d[[by]] == g & !is.na(x)), 0)
  if (type == "cont") {
    b <- tabtools:::tt_sc_block_cont(nonmiss, sample_n, total = total)
  } else if (type == "bin") {
    pos <- vapply(groups, function(g) sum(d[[by]] == g & !is.na(x) & x == 1), 0)
    b <- tabtools:::tt_sc_block_cat(matrix(pos, 1), nonmiss, sample_n, binary = TRUE, total = total)
  } else {
    lev <- sort(unique(x[!is.na(x)]))
    lc <- t(vapply(lev, function(l) vapply(groups, function(g) sum(d[[by]] == g & !is.na(x) & x == l), 0),
                   numeric(length(groups))))
    if (length(groups) == 1L) lc <- matrix(lc, ncol = 1)
    b <- tabtools:::tt_sc_block_cat(lc, nonmiss, sample_n, total = total)
  }
  v <- tabtools:::tt_sc_variable(b, k)
  v$kind <- type
  v
}

# The golden suppression matrix and N_* scalars of a scenario.
sc_golden_suppression <- function(id) {
  st <- golden_read_stored(id)
  s <- st[st$name == "suppression", ]
  rows <- unique(s$row)
  cols <- unique(s$col)
  m <- matrix(as.numeric(s$value), length(rows), length(cols), byrow = TRUE,
              dimnames = list(rows, cols))
  sc <- function(n) as.numeric(st$value[st$name == n])
  list(matrix = m, primary = sc("N_primary_suppressed"),
       secondary = sc("N_secondary_suppressed"), derived = sc("N_derived_suppressed"))
}

# Data of the qa/test_smallcells.do pipeline cases, saved by
# qa/stata/make_table1_phase3.do.
sc_pipeline_data <- function(name) {
  testthat::skip_if_not_installed("haven")
  as.data.frame(haven::read_dta(test_path("fixtures", "table1_phase3", paste0(name, ".dta"))))
}
