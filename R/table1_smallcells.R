# table1_tc small-cell wiring (plan task 3.5): per-variable count blocks run
# through the engine in R/smallcells.R, markers rendered into the cells,
# percentages withheld for protected variables, derived suppression of the
# p-value/test/statistic/SMD cells, the protected N row, and the public
# `suppression` matrix. Ports _desctab_collect.ado:626-839 (blocks),
# :877-1256 (rendering), and desctab.ado:704-802, :977-1053, :1527-1539.
#
# Row records built by .t1w_*_rows() carry `codes` (the display code of each
# column: 0 shown, 1 primary "<k", 2 complementary ">=k", 3 derived) and, on
# a variable's first record, `sc` = list(derived, missing, sample): whether
# the variable carries a primary suppression, the codes of its hidden
# missing row (used by missingsummary rows), and its column-margin codes
# (combined into the N row).

# The variable name for a refusal message (Stata names the variable, not its
# label): the spec whose label starts the record's label (varlabplus appends
# text), else the label itself.
.t1_sc_varname <- function(o, label) {
  label <- trimws(label)
  hit <- which(vapply(o$sc_labels, function(l) startsWith(label, l), NA))
  if (!length(hit)) return(label)
  hit <- hit[which.max(nchar(o$sc_labels[hit]))]
  o$sc_names[hit]
}

# Group sample sizes (column margins of every block).
.t1_sc_sample_n <- function(o, gp, col_masks) {
  vapply(col_masks[seq_len(gp$G)], function(m) sum(o$wx$d[m]), 0)
}

#' Small-cell rendering of a continuous variable's record
#'
#' `_desctab_collect.ado:646-693`, `:953-988`: the block holds the
#' contributing N and the missing count per group; a coded N replaces the
#' whole statistic cell with its marker.
#' @keywords internal
#' @noRd
.t1_sc_cont <- function(rec, nn, gp, o, col_masks) {
  G <- gp$G
  K <- length(col_masks)
  tot <- K > G
  b <- tt_sc_block_cont(nn[seq_len(G)], .t1_sc_sample_n(o, gp, col_masks),
                        missingsummary = o$missingsummary, total = tot)
  v <- tt_sc_variable(b, o$smallcells, fixedmargins = o$sc_nvars > 1L,
                      variable = .t1_sc_varname(o, rec$label),
                      primary = identical(o$smallcells_mode, "primary"))
  codes <- v$cells[1, ]
  for (k in which(codes > 0)) rec$cells[k] <- .t1_sc_render(nn[k], codes[k], o)
  rec$codes <- codes
  rec$sc <- list(derived = v$derived, missing = v$missing, sample = v$sample)
  rec
}

#' Small-cell rendering of a categorical or binary variable's records
#'
#' `_desctab_collect.ado:694-836`, `:1011-1110`, `:1155-1256`. A coded cell
#' shows its marker in place of the raw count; otherwise, when the variable
#' carries a primary suppression, the percentage is withheld and the cell is
#' the count alone (with its `slashN` denominator, itself coded when the
#' denominator is small or derivable from a coded missing row).
#' @param recs Records: one for a binary variable; the label row then one
#'   per level for a categorical variable.
#' @param cn Counts from .t1w_cat_counts().
#' @param tx Per level, per column: list(cnt, den_slash, cell) (a single
#'   level's list for binary variables).
#' @keywords internal
#' @noRd
.t1_sc_catbin <- function(recs, cn, tx, gp, o, col_masks, binary, missing_level = 0L) {
  G <- gp$G
  K <- length(col_masks)
  tot <- K > G
  k_sc <- o$smallcells
  b <- tt_sc_block_cat(cn$cell_d[, seq_len(G), drop = FALSE], cn$grp_d[seq_len(G)],
                       .t1_sc_sample_n(o, gp, col_masks), binary = binary,
                       include_missing = o$missing, missing_level = missing_level,
                       missingsummary = o$missingsummary, slashN = o$slashN,
                       catrowperc = o$catrowperc, total = tot)
  v <- tt_sc_variable(b, k_sc, fixedmargins = o$sc_nvars > 1L,
                      variable = .t1_sc_varname(o, recs[[1]]$label),
                      primary = identical(o$smallcells_mode, "primary"))
  if (binary) tx <- list(tx)
  first <- if (binary) 1L else 2L
  for (li in seq_len(nrow(v$cells))) {
    ri <- first + li - 1L
    rec <- recs[[ri]]
    rec$linked <- rep(FALSE, K)
    for (k in seq_len(K)) {
      code <- v$cells[li, k]
      if (code > 0) {
        rec$cells[k] <- .t1_sc_render(cn$cell_d[li, k], code, o)
        rec$codes[k] <- code
        next
      }
      t <- tx[[li]][[k]]
      dcode <- if (o$slashN) {
        if (!binary && o$catrowperc) v$engine$rowmask[li] else v$denominator[k]
      } else 0
      if (!v$derived && dcode == 0) next
      nstr <- stata_fmt(t$cnt, o$nformat)
      if (o$slashN) {
        if (dcode > 0) {
          den <- .t1_sc_render(t$den_slash, dcode, o)
          rec$codes[k] <- dcode
        } else {
          den <- stata_fmt(t$den_slash, o$nformat)
        }
        nstr <- paste0(nstr, "/", den)
      }
      rec$cells[k] <- if (v$derived) nstr else .t1_count_display(nstr, t$perc, o)
      rec$linked[k] <- v$derived
    }
    recs[[ri]] <- rec
  }
  recs[[1]]$sc <- list(derived = v$derived, missing = v$missing, sample = v$sample)
  recs
}

#' Sample-size header row under small-cell protection
#'
#' `_desctab_collect.ado:869-889` renders a coded group N as its marker; then
#' `headerperc` (desctab.ado:1104-1177) strips `N=`, parses each cell with
#' `real()` (a marker parses to missing and gets no percentage) and divides
#' by the total column, else the sum of the groups, which is missing when any
#' group is coded. Stata's `den > 0` is true for a missing denominator, so the
#' other cells then read `n (.)`.
#' @keywords internal
#' @noRd
.t1_sc_nrow <- function(sampleN, codes, o, total_present) {
  txt <- ifelse(codes > 0,
                vapply(seq_along(codes), function(k) {
                  if (codes[k] > 0) .t1_sc_render(sampleN[k], codes[k], o) else ""
                }, ""),
                paste0("N=", stata_fmt(sampleN, o$nformat)))
  if (!o$headerperc) return(txt)
  txt <- gsub("N=", "", txt, fixed = TRUE)
  val <- suppressWarnings(as.numeric(gsub(",", "", txt, fixed = TRUE)))
  # A literal numeric-looking mask is never numerical count evidence.
  val[codes > 0] <- NA_real_
  K <- length(txt)
  den <- if (total_present) val[K] else {
    g <- val[seq_len(if (total_present) K - 1L else K)]
    if (anyNA(g)) NA_real_ else sum(g)
  }
  for (k in seq_len(K)) {
    if (is.na(val[k]) || (!is.na(den) && den <= 0)) next
    pct <- if (is.na(den)) "." else stata_fmt(stata_round(val[k] / den, 0.001) * 100, "%9.1f")
    # desctab.ado:1176: headerperc uses desctab's untrimmed percsign.
    txt[k] <- paste0(txt[k], " (", pct, o$percsign_raw %||% o$percsign, ")")
  }
  txt
}

#' Finish one table1_tc pass: small-cell post-processing and the ESS row
#'
#' Runs on a table from .t1_build(). Under `smallcells`: withholds the
#' p-value, test, statistic, and SMD of every variable with a primary
#' suppression (`Suppressed`, `.d` in `r(table)`), codes the missing-summary
#' cells (desctab.ado:737-756), and re-renders the N row. Under `wt()`,
#' prepends the effective-sample-size row. Leaves per-row codes in
#' `meta$row_codes`, the N-row codes in `meta$sample_codes`, and derived
#' flags in `meta$derived_rows` for .t1_sc_stored().
#' @keywords internal
#' @noRd
.t1_finish_pass <- function(tt, gp, o) {
  blocks <- tt$meta$blocks
  tt$meta$blocks <- NULL
  K <- gp$G + as.integer(o$total != "none" && gp$G > 1L)
  col_masks <- lapply(seq_len(gp$G), function(g) !is.na(gp$gid) & gp$gid == g)
  if (K > gp$G) col_masks <- c(col_masks, list(!is.na(gp$gid)))
  if (!is.null(o$smallcells)) tt <- .t1_sc_pass(tt, blocks, col_masks, gp, o)
  if (identical(o$wx$kind, "wt")) {
    tt <- .t1_add_ess(tt, .t1w_ess(o$wx$w, col_masks), o, tt$meta$sample_codes)
    tt$meta$derived_rows <- c(FALSE, tt$meta$derived_rows)
  }
  tt
}

.t1_sc_pass <- function(tt, blocks, col_masks, gp, o) {
  recs <- unlist(blocks, recursive = FALSE)
  if (length(recs) != nrow(tt$body)) {
    cli::cli_abort("Internal error: small-cell records do not match the table rows.", call = NULL)
  }
  K <- length(col_masks)
  kmap <- .t1_col_groups(tt)
  sampleN <- vapply(col_masks, function(m) sum(o$wx$d[m]), 0)
  heads <- lapply(blocks, `[[`, 1L)
  sample_codes <- tt_sc_sample_mask(lapply(heads, `[[`, "sc"))

  # Missing-summary cells (desctab.ado:704-756): m = column maximum N minus
  # the variable's N; a coded missing row shows its marker.
  maxN <- sampleN
  for (r in recs) maxN <- pmax(maxN, ifelse(is.na(r$N), -Inf, r$N))
  body <- as.matrix(tt$body)
  codes <- vector("list", length(recs))
  derived <- logical(length(recs))
  linked <- vector("list", length(recs))
  i <- 0L
  for (b in blocks) {
    head <- b[[1]]
    for (r in b) {
      i <- i + 1L
      codes[[i]] <- r$codes %||% rep(0, K)
      linked[[i]] <- r$linked %||% rep(FALSE, K)
      if (identical(r$type, "missing_summary")) {
        m <- maxN - head$N
        for (j in which(!is.na(kmap))) {
          k <- kmap[j]
          if (!is.na(m[k]) && m[k] > 0 && head$sc$missing[k] > 0) {
            body[i, j] <- .t1_sc_render(m[k], head$sc$missing[k], o)
            codes[[i]][k] <- head$sc$missing[k]
          } else if (!is.na(m[k]) && m[k] > 0 && sample_codes[k] > 0) {
            # The Missing percentage would reveal the withheld group N.
            body[i, j] <- stata_fmt(m[k], o$nformat)
            linked[[i]][k] <- TRUE
          }
        }
      }
    }
    derived[i - length(b) + 1L] <- isTRUE(head$sc$derived)
  }

  # Derived suppression (desctab.ado:790-833): the test statistics of a
  # protected variable would let a reader back out the suppressed counts.
  role <- tt$cols$role
  stat_cols <- which(role %in% c("p", "test", "statistic", "smd"))
  for (i in which(derived)) {
    body[i, stat_cols] <- .t1_sc_render(NA_real_, 3L, o)
    tt$rows$p[i] <- NA_real_
    tt$rows$smd[i] <- NA_real_
    tr <- tt$rows$table_row[i]
    if (!is.null(tt$stored$table) && !is.na(tr)) tt$stored$table[tr, ] <- NA_real_
  }
  tt$body[] <- lapply(seq_len(ncol(body)), function(j) body[, j])

  # The N row.
  total_present <- any(role == "total")
  ntext <- .t1_sc_nrow(sampleN, sample_codes, o, total_present)
  for (j in which(!is.na(kmap))) tt$header[[2]]$text[j] <- ntext[kmap[j]]

  tt$meta$row_codes <- codes
  tt$meta$sample_codes <- sample_codes
  tt$meta$derived_rows <- derived
  tt$meta$linked_cells <- linked
  # Header percentages depend on the displayed Total, when present, or on
  # every group count otherwise. A masked unrelated group must not protect
  # a visible descriptor whose percentage has a visible Total denominator.
  denominator_masked <- if (total_present) tail(sample_codes, 1L) > 0L else any(sample_codes > 0L)
  tt$meta$header_linked <- rep(isTRUE(o$headerperc) && denominator_masked, length(sample_codes))
  tt
}

#' The public suppression map and counts (desctab.ado:977-1053, :1527-1539)
#'
#' One row per table row, the two header rows included; columns are the
#' group columns in group-code order with the total last (`<by>_<code>`,
#' `<by>_T`, `Total` without by(); `Cr_`/`Wt_` pairs, crude first, under
#' wtcompare), then whichever of `pvalue`, `test`, `statistic`, `smd_str`
#' exist. Codes: 0 visible, 1 primary, 2 complementary, 3 derived. Counts
#' are per display cell, so a margin shown twice counts twice.
#' @keywords internal
#' @noRd
.t1_sc_stored <- function(tt, gp, by, smallcells, total, wtcompare = FALSE, mode = "strict") {
  sfx <- c(stata_macro_text(gp$codes), if (total) "T")
  nrow_body <- nrow(tt$body)
  if (wtcompare) {
    cr <- tt$meta$crude_codes
    wt <- tt$meta$row_codes
    grp <- function(crc, wtc) as.vector(rbind(crc, wtc))
    names_g <- as.vector(rbind(paste0("Cr_", sfx), paste0("Wt_", sfx)))
    rows_g <- c(list(rep(0, 2L * length(sfx)),
                     grp(tt$meta$crude_sample_codes, tt$meta$sample_codes)),
                lapply(seq_len(nrow_body), function(i) grp(cr[[i]], wt[[i]])))
  } else {
    names_g <- if (is.null(by)) "Total" else paste0(by, "_", sfx)
    rows_g <- c(list(rep(0, length(sfx)), tt$meta$sample_codes), tt$meta$row_codes)
  }
  role <- tt$cols$role
  stat_names <- c(p = "pvalue", test = "test", statistic = "statistic", smd = "smd_str")
  stat <- stat_names[names(stat_names) %in% role]
  derived <- c(FALSE, FALSE, tt$meta$derived_rows)
  m <- do.call(rbind, lapply(seq_along(rows_g), function(i) {
    c(rows_g[[i]], rep(if (derived[i]) 3 else 0, length(stat)))
  }))
  dimnames(m) <- list(paste0("r", seq_len(nrow(m))), c(names_g, unname(stat)))
  list(smallcells = list(threshold = as.integer(smallcells), mode = mode,
                         n_masked = as.integer(sum(m %in% c(1, 2))),
                         n_linked = as.integer(sum(m == 3))),
       smallcells_mode = if (mode == "strict") "full" else "primary",
       N_primary_suppressed = sum(m == 1),
       N_secondary_suppressed = sum(m == 2),
       N_derived_suppressed = sum(m == 3),
       suppression = m)
}

#' Mask the sample-accounting ledger under smallcells
#'
#' The ledger holds raw counts (`missing_n`, `observed_n`, group `input_n`,
#' exclusion `n`) that the printed table withholds: the hidden missing count
#' of a protected variable, and a group or total N shown as a marker. Under
#' `smallcells`, every count metric of a population belonging to a variable
#' that carries any suppression code (or a derived suppression) is set to
#' `NA` with status `unavailable` and reason `suppressed_by_smallcells`; so
#' are the group and table populations whose N is withheld, and the N-bearing
#' metrics (`input_n`, `eligible_n`, `excluded_n`, `zero_weight_n`,
#' `missing_n`) of other variables' populations in a withheld group.
#' Unsuppressed variables keep their counts. Statuses other than `available`
#' are left alone.
#' @keywords internal
#' @noRd
.t1_sc_mask_ledger <- function(tt, gp, n_specs) {
  s <- tt$meta$sample_accounting
  rc <- tt$meta$row_codes
  cc <- tt$meta$crude_codes
  types <- tt$rows$type
  G <- gp$G
  head <- which(types %in% c("var", "cat_header"))
  blk <- cumsum(seq_along(types) %in% head)
  any_code <- function(i) any(c(rc[[i]], cc[[i]]) > 0) || isTRUE(tt$meta$derived_rows[i])
  flagged <- if (length(head) == n_specs) {
    vapply(seq_along(head), function(b) any(vapply(which(blk == b), any_code, NA)), NA)
  } else rep(TRUE, n_specs)  # fail safe: cannot map blocks to variables
  sc <- pmax(tt$meta$sample_codes, tt$meta$crude_sample_codes %||% 0)
  K <- length(sc)
  nwith <- sc > 0
  all_metrics <- .tt_sample_metrics
  n_metrics <- c("input_n", "eligible_n", "excluded_n", "zero_weight_n", "missing_n")
  pid <- sub("^(crude|weighted)/", "", s$populations$id)
  parts <- strsplit(pid, "/", fixed = TRUE)
  mask_all <- logical(length(pid))
  mask_n <- logical(length(pid))
  for (j in seq_along(pid)) {
    p <- parts[[j]]
    if (p[1] == "table") mask_all[j] <- any(nwith)
    else if (p[1] == "group") mask_all[j] <- nwith[as.integer(p[2])]
    else if (p[1] == "variable") {
      i <- as.integer(p[2])
      col <- if (p[3] == "table") if (K > G) K else NA_integer_ else as.integer(p[4])
      mask_all[j] <- isTRUE(flagged[i])
      mask_n[j] <- if (p[3] == "table") any(nwith) else isTRUE(nwith[col])
    }
  }
  ids <- s$populations$id
  m <- s$measures
  pos <- match(m$population_id, ids)
  hit <- m$status == "available" & (mask_all[pos] | (mask_n[pos] & m$metric %in% n_metrics))
  m$value[hit] <- NA_real_
  m$status[hit] <- "unavailable"
  m$reason[hit] <- "suppressed_by_smallcells"
  e <- s$exclusions
  ehit <- e$status == "available" & (mask_all | mask_n)[match(e$population_id, ids)]
  e$n[ehit] <- NA_real_
  e$status[ehit] <- "unavailable"
  s$measures <- m
  s$exclusions <- e
  .tt_validate_sample_accounting(s)
  tt$meta$sample_accounting <- s
  tt
}
