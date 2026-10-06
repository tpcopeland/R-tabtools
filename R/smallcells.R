# Small-cell disclosure control (plan task 3.5).
#
# Port of the Stata tabtools 2.1.8 exact-disclosure engine
# (`_tabtools_smallcells.ado`, Mata `_ttsc_*`, lines 118-588), the count-block
# construction in `_desctab_collect.ado:626-839`, and the display rendering in
# `_tabtools_smallcells_render.ado`. The engine works on one count block at a
# time: a matrix of cell counts with row, column, and grand margins, each cell
# and margin flagged as published exactly ("exact") and/or subject to the
# threshold ("sensitive").
#
# Cell/margin states (Mata `state`): -1 free (not published: any value
# 0..upper), 0 published exactly, 1 primary suppression (shown "<k", value in
# [1, k-1]), 2 complementary suppression (shown ">=k", value in [k, upper]).
# A primary cell is protected when a reader who knows every published value
# and every marker's range cannot pin it down: actual - 1 or actual + 1 must
# also be feasible. Feasibility is a bounded-flow problem on the bipartite
# rows x columns network with margin arcs, checked by max-flow.

# ---------------------------------------------------------------------------
# Engine internals (Mata `_ttsc_*`)

# Lower/upper bounds for a cell or margin in a given state
# (`_ttsc_bounds`, _tabtools_smallcells.ado:199-209).
.sc_bounds <- function(actual, state, k, upper) {
  if (state == 0) return(c(actual, actual))
  if (state == 1) return(c(1, k - 1))
  if (state == 2) return(c(k, upper))
  c(0, upper)
}

# Edmonds-Karp max-flow on a dense capacity matrix (`_ttsc_maxflow`,
# :153-197). Feasibility only depends on the flow value, not on which
# augmenting paths are found, so the search order need not match Mata's.
.sc_maxflow <- function(capacity, source, sink) {
  n <- nrow(capacity)
  flow <- 0
  repeat {
    parent <- integer(n)
    parent[source] <- -1L
    queue <- source
    head <- 1L
    while (head <= length(queue) && parent[sink] == 0L) {
      u <- queue[head]
      head <- head + 1L
      nb <- which(parent == 0L & capacity[u, ] > 1e-9)
      if (length(nb)) {
        parent[nb] <- u
        queue <- c(queue, nb)
      }
    }
    if (parent[sink] == 0L) break
    path <- integer()
    v <- sink
    while (v != source) {
      path <- c(path, v)
      v <- parent[v]
    }
    from <- parent[path]
    aug <- min(capacity[cbind(from, path)])
    capacity[cbind(from, path)] <- capacity[cbind(from, path)] - aug
    capacity[cbind(path, from)] <- capacity[cbind(path, from)] + aug
    flow <- flow + aug
  }
  flow
}

# Is there a table consistent with every bound, with the target cell/margin
# fixed at `target_value`? (`_ttsc_feasible`, :211-278). target_kind: 0 cell,
# 1 row margin, 2 column margin, 3 grand total.
.sc_feasible <- function(counts, state, rowstate, colstate, grandstate, k,
                         target_kind, target_i, target_j, target_value) {
  nr <- nrow(counts)
  nc <- ncol(counts)
  upper <- sum(counts) + 2 * k * (nr * nc + nr + nc + 1) + 10
  if (upper < k + 1) upper <- k + 1

  source <- 1L
  first_row <- 2L
  first_col <- first_row + nr
  sink <- first_col + nc
  super_source <- sink + 1L
  super_sink <- sink + 2L
  n <- super_sink
  capacity <- matrix(0, n, n)
  balance <- numeric(n)
  add_edge <- function(from, to, b) {
    capacity[from, to] <<- capacity[from, to] + b[2] - b[1]
    balance[from] <<- balance[from] - b[1]
    balance[to] <<- balance[to] + b[1]
  }
  rowtotals <- rowSums(counts)
  coltotals <- colSums(counts)

  for (i in seq_len(nr)) {
    b <- .sc_bounds(rowtotals[i], rowstate[i], k, upper)
    if (target_kind == 1 && target_i == i) b <- c(target_value, target_value)
    add_edge(source, first_row + i - 1L, b)
  }
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      b <- .sc_bounds(counts[i, j], state[i, j], k, upper)
      if (target_kind == 0 && target_i == i && target_j == j) b <- c(target_value, target_value)
      add_edge(first_row + i - 1L, first_col + j - 1L, b)
    }
  }
  for (j in seq_len(nc)) {
    b <- .sc_bounds(coltotals[j], colstate[j], k, upper)
    if (target_kind == 2 && target_j == j) b <- c(target_value, target_value)
    add_edge(first_col + j - 1L, sink, b)
  }
  b <- .sc_bounds(sum(counts), grandstate, k, upper)
  if (target_kind == 3) b <- c(target_value, target_value)
  add_edge(sink, source, b)

  pos <- which(balance[seq_len(sink)] > 1e-9)
  neg <- which(balance[seq_len(sink)] < -1e-9)
  capacity[super_source, pos] <- capacity[super_source, pos] + balance[pos]
  capacity[neg, super_sink] <- capacity[neg, super_sink] - balance[neg]
  total_need <- sum(balance[pos])
  if (total_need >= 2^53) {
    .sc_abort_input("{.arg counts}: the required flow must be below 2^53 for exact-disclosure certification.")
  }
  flow <- .sc_maxflow(capacity, super_source, super_sink)
  abs(flow - total_need) <= 1e-8
}

# Can a primary value move by one in either direction? (`_ttsc_has_alternative`,
# :280-301).
.sc_has_alternative <- function(counts, state, rowstate, colstate, grandstate, k,
                                target_kind, target_i, target_j, actual) {
  if (actual > 1 && .sc_feasible(counts, state, rowstate, colstate, grandstate, k,
                                 target_kind, target_i, target_j, actual - 1)) {
    return(TRUE)
  }
  if (actual < k - 1 && .sc_feasible(counts, state, rowstate, colstate, grandstate, k,
                                     target_kind, target_i, target_j, actual + 1)) {
    return(TRUE)
  }
  FALSE
}

# Number of primary cells and margins a reader could pin down
# (`_ttsc_failures`, :303-343).
.sc_failures <- function(counts, state, rowstate, colstate, grandstate, k) {
  failures <- 0L
  rowtotals <- rowSums(counts)
  coltotals <- colSums(counts)
  for (i in seq_len(nrow(counts))) {
    for (j in seq_len(ncol(counts))) {
      if (state[i, j] == 1 &&
          !.sc_has_alternative(counts, state, rowstate, colstate, grandstate, k, 0, i, j, counts[i, j])) {
        failures <- failures + 1L
      }
    }
  }
  for (i in seq_along(rowstate)) {
    if (rowstate[i] == 1 &&
        !.sc_has_alternative(counts, state, rowstate, colstate, grandstate, k, 1, i, 0, rowtotals[i])) {
      failures <- failures + 1L
    }
  }
  for (j in seq_along(colstate)) {
    if (colstate[j] == 1 &&
        !.sc_has_alternative(counts, state, rowstate, colstate, grandstate, k, 2, 0, j, coltotals[j])) {
      failures <- failures + 1L
    }
  }
  if (grandstate == 1 &&
      !.sc_has_alternative(counts, state, rowstate, colstate, grandstate, k, 3, 0, 0, sum(counts))) {
    failures <- failures + 1L
  }
  failures
}

.sc_valid_binary <- function(x) !anyNA(x) && all(x == 0 | x == 1)

# The whole decision procedure (`_ttsc_run`, :352-587). Returns
# list(status = -1 invalid input | 0 uncertifiable | 1 certified, ...).
.sc_run <- function(counts, exact, sensitive, rowexact, rowsensitive, colexact,
                    colsensitive, grandexact, grandsensitive, k, fixedmargins = FALSE) {
  nr <- nrow(counts)
  nc <- ncol(counts)
  bad <- list(status = -1L)
  if (nr < 1 || nc < 1) return(bad)
  if (!identical(dim(exact), dim(counts)) || !identical(dim(sensitive), dim(counts))) return(bad)
  if (length(rowexact) != nr || length(rowsensitive) != nr) return(bad)
  if (length(colexact) != nc || length(colsensitive) != nc) return(bad)
  if (any(!is.finite(counts)) || any(counts < 0) || any(counts != floor(counts))) return(bad)
  if (!.sc_valid_binary(exact) || !.sc_valid_binary(sensitive)) return(bad)
  if (!.sc_valid_binary(rowexact) || !.sc_valid_binary(rowsensitive)) return(bad)
  if (!.sc_valid_binary(colexact) || !.sc_valid_binary(colsensitive)) return(bad)
  if (!.sc_valid_binary(grandexact) || !.sc_valid_binary(grandsensitive)) return(bad)

  rowtotals <- rowSums(counts)
  coltotals <- colSums(counts)
  grand <- sum(counts)

  # Initial states (:405-424).
  state <- matrix(-1, nr, nc)
  state[exact == 1] <- 0
  state[sensitive == 1 & counts > 0 & counts < k] <- 1
  rowstate <- rep(-1, nr)
  rowstate[rowexact == 1] <- 0
  rowstate[rowsensitive == 1 & rowtotals > 0 & rowtotals < k] <- 1
  colstate <- rep(-1, nc)
  colstate[colexact == 1] <- 0
  colstate[colsensitive == 1 & coltotals > 0 & coltotals < k] <- 1
  grandstate <- if (grandsensitive == 1 && grand > 0 && grand < k) 1 else if (grandexact == 1) 0 else -1

  eligible <- function(i, j) state[i, j] == 0 && exact[i, j] == 1 && counts[i, j] >= k

  # Forced complements (:426-477): an exact margin with exactly one
  # non-published cell, that cell primary, discloses it; publish instead the
  # smallest eligible exact cell as ">=k" (first on ties).
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(nr)) {
      if (rowstate[i] != 0 || !any(state[i, ] == 1)) next
      if (sum(state[i, ] != 0) != 1) next
      candidate <- 0L
      for (j in seq_len(nc)) {
        if (eligible(i, j) && (candidate == 0L || counts[i, j] < counts[i, candidate])) candidate <- j
      }
      if (candidate > 0L) {
        state[i, candidate] <- 2
        changed <- TRUE
      }
    }
    for (j in seq_len(nc)) {
      if (colstate[j] != 0 || !any(state[, j] == 1)) next
      if (sum(state[, j] != 0) != 1) next
      candidate <- 0L
      for (i in seq_len(nr)) {
        if (eligible(i, j) && (candidate == 0L || counts[i, j] < counts[candidate, j])) candidate <- i
      }
      if (candidate > 0L) {
        state[candidate, j] <- 2
        changed <- TRUE
      }
    }
    if (grandstate == 0 && any(state == 1) && sum(state != 0) == 1) {
      best <- NULL
      for (i in seq_len(nr)) {
        for (j in seq_len(nc)) {
          if (eligible(i, j) && (is.null(best) || counts[i, j] < best[1])) best <- c(counts[i, j], i, j)
        }
      }
      if (!is.null(best)) {
        state[best[2], best[3]] <- 2
        changed <- TRUE
      }
    }
  }

  failures <- .sc_failures(counts, state, rowstate, colstate, grandstate, k)

  # Greedy complements (:479-507): candidates in (count, row, col) order; take
  # the first that lowers the failure count, else the first.
  while (failures > 0) {
    idx <- which(state == 0 & exact == 1 & counts >= k, arr.ind = TRUE)
    if (!nrow(idx)) break
    cand <- cbind(counts[idx], idx[, 1], idx[, 2])
    cand <- cand[order(cand[, 1], cand[, 2], cand[, 3]), , drop = FALSE]
    chosen <- 1L
    for (c in seq_len(nrow(cand))) {
      trial <- state
      trial[cand[c, 2], cand[c, 3]] <- 2
      if (.sc_failures(counts, trial, rowstate, colstate, grandstate, k) < failures) {
        chosen <- c
        break
      }
    }
    state[cand[chosen, 2], cand[chosen, 3]] <- 2
    failures <- .sc_failures(counts, state, rowstate, colstate, grandstate, k)
  }

  # Full-block fallback (:509-530): mark every published positive cell and
  # margin truthfully ("<k" below the threshold, ">=k" otherwise). Under
  # `fixedmargins` (tabtools 2.1.17, _tabtools_smallcells.ado:521-535) the
  # column and grand margins are never withheld: desctab prints each group
  # N once for every variable, so withholding them here would protect
  # nothing.
  if (failures > 0) {
    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        if (state[i, j] == 0 && exact[i, j] == 1 && counts[i, j] > 0) {
          state[i, j] <- if (counts[i, j] < k) 1 else 2
        }
      }
      if (rowstate[i] == 0 && rowexact[i] == 1 && rowtotals[i] > 0) {
        rowstate[i] <- if (rowtotals[i] < k) 1 else 2
      }
    }
    for (j in seq_len(nc)) {
      if (!fixedmargins && colstate[j] == 0 && colexact[j] == 1 && coltotals[j] > 0) {
        colstate[j] <- if (coltotals[j] < k) 1 else 2
      }
    }
    if (!fixedmargins && grandstate == 0 && grandexact == 1 && grand > 0) {
      grandstate <- if (grand < k) 1 else 2
    }
    failures <- .sc_failures(counts, state, rowstate, colstate, grandstate, k)
  }
  if (failures > 0) return(list(status = 0L))

  # Irredundancy (:532-573): reveal each complementary marker in stable order
  # (cells row-major, row margins, column margins, grand total); keep the
  # reveal only if every primary stays protected. Repeat until stable.
  ok <- function() .sc_failures(counts, state, rowstate, colstate, grandstate, k) == 0L
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        if (state[i, j] != 2) next
        state[i, j] <- 0
        if (ok()) changed <- TRUE else state[i, j] <- 2
      }
    }
    for (i in seq_len(nr)) {
      if (rowstate[i] != 2) next
      rowstate[i] <- 0
      if (ok()) changed <- TRUE else rowstate[i] <- 2
    }
    for (j in seq_len(nc)) {
      if (colstate[j] != 2) next
      colstate[j] <- 0
      if (ok()) changed <- TRUE else colstate[j] <- 2
    }
    if (grandstate == 2) {
      grandstate <- 0
      if (ok()) changed <- TRUE else grandstate <- 2
    }
  }
  if (!ok()) return(list(status = 0L))

  mask <- pmax(state, 0)
  dim(mask) <- dim(counts)
  rowmask <- pmax(rowstate, 0)
  colmask <- pmax(colstate, 0)
  totalmask <- max(grandstate, 0)
  list(
    status = 1L,
    mask = mask, rowmask = rowmask, colmask = colmask, totalmask = totalmask,
    n_primary = sum(mask == 1) + sum(rowmask == 1) + sum(colmask == 1) + (totalmask == 1),
    n_secondary = sum(mask == 2) + sum(rowmask == 2) + sum(colmask == 2) + (totalmask == 2)
  )
}

# ---------------------------------------------------------------------------
# Engine entry point (`_tabtools_smallcells`, :6-116)

#' Small-cell suppression for one count block
#'
#' Port of Stata tabtools' `_tabtools_smallcells`. Decides which cells and
#' margins of a count block are shown as `<k` (primary suppression), `>=k`
#' (complementary suppression), or exactly, so that no primary count can be
#' reconstructed from the published values and the markers' ranges.
#'
#' @param counts Nonnegative integer matrix (rows x columns).
#' @param smallcells Threshold k, an integer >= 3.
#' @param exact,sensitive 0/1 matrices like `counts`: cell published exactly /
#'   subject to the threshold. Default all 1.
#' @param rowexact,rowsensitive,colexact,colsensitive 0/1 vectors for the row
#'   and column margins. Default all 0.
#' @param grandexact,grandsensitive 0/1 flags for the grand total.
#' @param fixedmargins When `TRUE`, column and grand margins are never
#'   withheld as complementary cells (they are published elsewhere, e.g. the
#'   group N shared by every variable of a table; Stata 2.1.17).
#' @return A list: `mask` (0 shown, 1 primary, 2 complementary), `rowmask`,
#'   `colmask`, `totalmask`, `n_primary`, `n_secondary`, `smallcells`.
#'   Errors of class `tabtools_smallcells_input` (Stata r(198)) or
#'   `tabtools_smallcells_uncertified` (Stata r(498)).
#' @keywords internal
#' @noRd
tt_smallcells <- function(counts, smallcells, exact = NULL, sensitive = NULL,
                          rowexact = NULL, rowsensitive = NULL,
                          colexact = NULL, colsensitive = NULL,
                          grandexact = 0, grandsensitive = 0, fixedmargins = FALSE) {
  k <- .sc_check_threshold(smallcells)
  counts <- as.matrix(counts)
  nr <- nrow(counts)
  nc <- ncol(counts)
  if (!is.numeric(counts) || nr < 1 || nc < 1) {
    .sc_abort_input("{.arg counts} must be a nonempty numeric matrix.")
  }
  if (any(!is.finite(counts))) {
    .sc_abort_input("{.arg counts} must contain only finite nonnegative integers.")
  }
  # The flow network uses integer capacities, including its artificial
  # upper bound. Above 2^53 double arithmetic loses single-count changes,
  # so it can certify disclosure protection without testing actual +/-1.
  # Reject the boundary too: an integer sum just above it can round back.
  upper <- sum(counts) + 2 * k * (nr * nc + nr + nc + 1) + 10
  if (!is.finite(upper) || upper >= 2^53) {
    .sc_abort_input("{.arg counts}: the count total and flow bounds must be below 2^53 for exact-disclosure certification.")
  }
  full <- function(x, fill, n, m) {
    if (is.null(x)) return(matrix(fill, n, m))
    as.matrix(x)
  }
  exact <- full(exact, 1, nr, nc)
  sensitive <- full(sensitive, 1, nr, nc)
  rowexact <- if (is.null(rowexact)) rep(0, nr) else as.vector(rowexact)
  rowsensitive <- if (is.null(rowsensitive)) rep(0, nr) else as.vector(rowsensitive)
  colexact <- if (is.null(colexact)) rep(0, nc) else as.vector(colexact)
  colsensitive <- if (is.null(colsensitive)) rep(0, nc) else as.vector(colsensitive)
  if (!.sc_valid_binary(grandexact) || !.sc_valid_binary(grandsensitive) ||
      length(grandexact) != 1L || length(grandsensitive) != 1L) {
    .sc_abort_input("Grand-margin flags must be 0 or 1.")
  }
  res <- .sc_run(counts, exact, sensitive, rowexact, rowsensitive, colexact,
                 colsensitive, grandexact, grandsensitive, k,
                 fixedmargins = isTRUE(fixedmargins))
  if (res$status == -1L) {
    .sc_abort_input("Counts and masks must be conformable nonnegative integer/binary matrices.")
  }
  if (res$status == 0L) {
    cli::cli_abort(
      "{.arg smallcells}: exact-disclosure protection could not be certified for this count block.",
      class = c("tabtools_error_smallcells", "tabtools_smallcells_uncertified"), call = NULL
    )
  }
  res$status <- NULL
  res$smallcells <- k
  res
}

.sc_check_threshold <- function(k) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k != floor(k) || k < 3 || k > .Machine$integer.max) {
    .sc_abort_input("{.arg smallcells} must be an integer greater than or equal to 3.")
  }
  as.integer(k)
}

.sc_abort_input <- function(msg) {
  cli::cli_abort(msg, class = "tabtools_smallcells_input", call = NULL)
}

# ---------------------------------------------------------------------------
# Rendering (`_tabtools_smallcells_render.ado`)

#' Display text for a count under a suppression code
#'
#' Code 0 formats the value with `format` (Stata `nformat`, default
#' `%12.0fc`); 1 gives `"<k"`, 2 `">=k"` (U+2265), 3 `"Suppressed"`. The
#' `stata_missing` attribute records the extended missing Stata stores for
#' the numeric value (`.p`, `.s`, `.d`; `NA` in R).
#' @keywords internal
#' @noRd
tt_sc_render <- function(value, mask, smallcells, format = "%12.0fc") {
  k <- .sc_check_threshold(smallcells)
  if (length(mask) != 1L || !mask %in% 0:3) {
    .sc_abort_input("{.arg mask} must be 0, 1, 2, or 3.")
  }
  switch(as.character(mask),
    "0" = structure(stata_fmt(value, format), stata_missing = NA_character_),
    "1" = structure(paste0("<", k), stata_missing = ".p"),
    "2" = structure(paste0("\u2265", k), stata_missing = ".s"),
    "3" = structure("Suppressed", stata_missing = ".d")
  )
}

# ---------------------------------------------------------------------------
# Count blocks per variable (`_desctab_collect.ado:626-839`)

#' Count block for a continuous variable (contn/contln/conts)
#'
#' Rows: contributing N and missing count per group (`_desctab_collect.ado:
#' 646-668`). N is never published exactly (only the mean/SD etc. are); the
#' missing row is published under `missingsummary`. The group Ns (column
#' margins) are published and sensitive; the total column publishes the row
#' margins under `missingsummary` and the grand total when `total` is on.
#' @param n Contributing non-missing count per group.
#' @param sample_n Analysis-sample N per group.
#' @keywords internal
#' @noRd
tt_sc_block_cont <- function(n, sample_n, missingsummary = FALSE, total = FALSE) {
  g <- length(sample_n)
  n[is.na(n)] <- 0
  ms <- as.numeric(missingsummary)
  tot <- as.numeric(total)
  list(
    type = "cont",
    counts = rbind(n, sample_n - n, deparse.level = 0),
    exact = rbind(rep(0, g), rep(ms, g)),
    sensitive = rbind(rep(1, g), rep(ms, g)),
    rowexact = c(0, ms * tot),
    rowsensitive = c(tot, ms * tot),
    colexact = rep(1, g),
    colsensitive = rep(1, g),
    grandexact = tot,
    grandsensitive = tot,
    n_levels = 1L, missrow = 2L, negrow = 0L,
    total = total, slash_rules = FALSE
  )
}

#' Count block for a categorical or binary variable (cat/cate/bin/bine)
#'
#' Rows: the displayed levels, then hidden rows (`_desctab_collect.ado:
#' 694-773`): binary variables add the negative and missing rows; categorical
#' variables add a missing row unless `missing` shows it as a level (then
#' `missing_level` is its row index, 0 if absent). Hidden rows are always
#' sensitive (tabtools 2.1.17: they follow by subtraction from the printed
#' levels and group N, or from a printed percentage's denominator) and are
#' exact (published) only when `missingsummary` or `slashN`
#' (row-percent-free) prints them.
#' Level-row margins are published with `total` or `slashN` + `catrowperc`.
#' @param level_counts Level x group matrix of counts (one row, the positive
#'   count, for binary variables).
#' @param nonmiss Non-missing count per group.
#' @param sample_n Analysis-sample N per group.
#' @keywords internal
#' @noRd
tt_sc_block_cat <- function(level_counts, nonmiss, sample_n, binary = FALSE,
                            include_missing = FALSE, missing_level = 0L,
                            missingsummary = FALSE, slashN = FALSE,
                            catrowperc = FALSE, total = FALSE) {
  level_counts <- as.matrix(level_counts)
  nl <- nrow(level_counts)
  g <- ncol(level_counts)
  hidden <- if (binary) 2L else if (!include_missing) 1L else 0L
  nrr <- nl + hidden
  counts <- exact <- sensitive <- matrix(0, nrr, g)
  counts[seq_len(nl), ] <- level_counts
  exact[seq_len(nl), ] <- 1
  sensitive[seq_len(nl), ] <- 1
  # slashN prints the non-missing denominator unless row percentages replace
  # it (binary rows always print it).
  slash_den <- slashN && (binary || !catrowperc)
  missrow <- 0L
  negrow <- 0L
  if (binary) {
    negrow <- nl + 1L
    missrow <- nl + 2L
    counts[negrow, ] <- nonmiss - level_counts[1, ]
    counts[missrow, ] <- sample_n - nonmiss
    # tabtools 2.1.17 (_desctab_collect.ado:~762): the hidden rows are
    # sensitive even when not printed: a printed percentage releases the
    # non-missing denominator, so negative and missing follow by subtraction.
    sensitive[negrow, ] <- 1
    sensitive[missrow, ] <- 1
    if (missingsummary || slash_den) {
      exact[missrow, ] <- 1
      sensitive[missrow, ] <- 1
    }
    if (slash_den) {
      exact[negrow, ] <- 1
      sensitive[negrow, ] <- 1
    }
  } else if (!include_missing) {
    missrow <- nrr
    counts[missrow, ] <- sample_n - nonmiss
    # Levels plus the group N give the missing row (2.1.17).
    sensitive[missrow, ] <- 1
    if (missingsummary || (slashN && !catrowperc)) {
      exact[missrow, ] <- 1
      sensitive[missrow, ] <- 1
    }
  } else {
    missrow <- as.integer(missing_level)
  }
  row_released <- as.numeric(total || (slashN && catrowperc))
  rowexact <- rowsensitive <- rep(0, nrr)
  rowexact[seq_len(nl)] <- row_released
  rowsensitive[seq_len(nl)] <- row_released
  # Hidden rows' row totals are sensitive whenever the level-row margins are
  # released (2.1.17, _desctab_collect.ado:~807).
  if (nrr > nl) rowsensitive[(nl + 1L):nrr] <- row_released
  if (missrow > 0L && missingsummary && total) {
    rowexact[missrow] <- 1
    rowsensitive[missrow] <- 1
  }
  tot <- as.numeric(total)
  list(
    type = if (binary) "bin" else "cat",
    counts = counts, exact = exact, sensitive = sensitive,
    rowexact = rowexact, rowsensitive = rowsensitive,
    colexact = rep(1, g), colsensitive = rep(1, g),
    grandexact = tot, grandsensitive = tot,
    n_levels = nl, missrow = missrow, negrow = negrow,
    total = total, slash_den = slash_den, nonmiss = nonmiss
  )
}

#' Run the engine on a block and map its masks to display cells
#'
#' Mirrors `_desctab_collect.ado:670-690` (continuous) and `:769-836`
#' (categorical/binary). Columns of every returned mask are the groups, then
#' the total column when `block$total` is set.
#' @param fixedmargins `TRUE` when the table has two or more variables (they
#'   share the group and total N, which therefore cannot be withheld).
#' @param variable Variable name for the refusal message.
#' @return list: `cells` (levels x columns; the N row for continuous
#'   variables), `missing` (the missing row's codes, or 0), `denominator`
#'   (slashN denominator codes: 1 small denominator, 3 derived), `sample`
#'   (column-margin/grand codes feeding the N header), `derived` (TRUE when
#'   the block has any primary suppression, so p, test, statistic, and SMD
#'   are withheld), `n_primary`, `n_secondary`, and the raw engine result.
#' @keywords internal
#' @noRd
tt_sc_variable <- function(block, smallcells, fixedmargins = FALSE, variable = NULL) {
  k <- .sc_check_threshold(smallcells)
  res <- tryCatch(
    tt_smallcells(block$counts, k, block$exact, block$sensitive,
                  block$rowexact, block$rowsensitive, block$colexact,
                  block$colsensitive, block$grandexact, block$grandsensitive,
                  fixedmargins = fixedmargins),
    tabtools_smallcells_uncertified = function(e) {
      if (!isTRUE(fixedmargins)) stop(e)
      # _desctab_collect.ado (2.1.17): with two or more variables a count
      # that only withholding a group or total N could protect is refused.
      cli::cli_abort(c(
        "{if (is.null(variable)) 'A variable' else paste0('Variable ', variable)}: a count below {k} can only be protected by withholding a group or total N, which the other variables in the table release.",
        "i" = "Combine sparse levels or leave the variable out of this table."),
        class = c("tabtools_error_smallcells", "tabtools_error_smallcells_shared_margin"),
        call = NULL)
    })
  g <- ncol(block$counts)
  tot <- isTRUE(block$total)
  with_total <- function(body, margin) if (tot) cbind(body, margin, deparse.level = 0) else body
  nl <- block$n_levels
  cells <- with_total(res$mask[seq_len(nl), , drop = FALSE], res$rowmask[seq_len(nl)])
  mr <- block$missrow
  missing <- if (mr > 0L) with_total(res$mask[mr, , drop = FALSE], res$rowmask[mr])[1, ] else rep(0, g + tot)
  den <- rep(0, g + tot)
  if (block$type != "cont") {
    if (mr > 0L && isTRUE(block$slash_den)) den[missing > 0] <- 3
    if (block$type == "bin" && isTRUE(block$slash_den)) {
      neg <- with_total(res$mask[block$negrow, , drop = FALSE], res$rowmask[block$negrow])[1, ]
      den[neg > 0] <- 3
    }
    if (isTRUE(block$slash_den)) {
      d <- block$nonmiss
      if (tot) d <- c(d, sum(d))
      den[d > 0 & d < k] <- 1
    }
  }
  list(
    cells = cells, missing = missing, denominator = den,
    sample = c(res$colmask, if (tot) res$totalmask),
    derived = res$n_primary > 0,
    n_primary = res$n_primary, n_secondary = res$n_secondary,
    engine = res
  )
}

#' Combine per-variable column-margin codes into the N-header mask
#'
#' `_desctab_collect.ado:680-690`, `:825-835`: a primary anywhere wins, else a
#' complement anywhere, else shown.
#' @keywords internal
#' @noRd
tt_sc_sample_mask <- function(variables) {
  if (!length(variables)) return(integer())
  s <- do.call(rbind, lapply(variables, `[[`, "sample"))
  apply(s, 2L, function(x) if (any(x == 1)) 1 else if (any(x == 2)) 2 else 0)
}

#' Standard small-cell footnote (`desctab.ado:120-138`)
#' @keywords internal
#' @noRd
tt_sc_footnote <- function(smallcells) {
  k <- .sc_check_threshold(smallcells)
  paste0("Counts below ", k, " are shown as <", k, "; complementary cells are shown as \u2265", k,
         " to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.")
}
