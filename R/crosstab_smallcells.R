# Every count and margin is exact/sensitive: pinned crosstab.ado:307-333.
.xt_masks <- function(f, k, mode, percent) {
  nr <- nrow(f); nc <- ncol(f)
  if (is.null(k)) {
    sc <- list(mask = matrix(0L, nr, nc), rowmask = rep(0L, nr),
               colmask = rep(0L, nc), totalmask = 0L, n_primary = 0L, n_secondary = 0L)
  } else if (mode == "primary") {
    upper <- sum(f) + 2 * k * (nr * nc + nr + nc + 1) + 10
    if (!is.finite(upper) || upper >= 2^53) .sc_abort_input("Counts and flow bounds must be below 2^53.")
    below <- function(x) as.integer(x > 0 & x < k)
    sc <- list(mask = matrix(below(f), nr, nc), rowmask = below(rowSums(f)),
               colmask = below(colSums(f)), totalmask = below(sum(f)), n_secondary = 0L)
    sc$n_primary <- sum(sc$mask) + sum(sc$rowmask) + sum(sc$colmask) + sc$totalmask
  } else {
    sc <- tt_smallcells(f, k, rowexact = rep(1L, nr), rowsensitive = rep(1L, nr),
                       colexact = rep(1L, nc), colsensitive = rep(1L, nc),
                       grandexact = 1L, grandsensitive = 1L)
  }
  denom <- switch(percent, column = matrix(rep(sc$colmask, each = nr), nr, nc),
                  row = matrix(rep(sc$rowmask, nc), nr, nc),
                  total = matrix(sc$totalmask, nr, nc))
  sc$pct_linked <- sc$mask > 0L | denom > 0L
  sc$derived <- !is.null(k) && mode == "strict" && sc$n_primary > 0L
  sc
}

.xt_ledger <- function(sample, sc, weighted, rowvar, colvar) {
  # Record-count decompositions/weight totals may reconstruct protected counts.
  # Keep only the independently released grand margin when any count is masked.
  protected <- sc$n_primary + sc$n_secondary > 0L
  val <- sample
  if (protected) val <- lapply(val, function(x) NA_real_)
  val$reported_n <- if (sc$totalmask > 0L) NA_real_ else sample$reported_n
  reason <- if (protected) as.list(stats::setNames(rep("unavailable under count disclosure protection", length(val)), names(val))) else list()
  if (!is.na(val$reported_n)) reason$reported_n <- ""
  bases <- as.list(stats::setNames(rep("source record count", length(val)), names(val)))
  bases$reported_n <- if (weighted) "expanded frequency count" else "contributing record count"
  .tt_sample_population("crosstab:table", "crosstab", "table",
                        variable = paste(rowvar, colvar, sep = " by "),
                        weight_type = if (weighted) "frequency" else "none",
                        values = val, bases = bases, reasons = reason)
}

.xt_mask_note <- function(k, mode, text) {
  if (is.null(k)) return(NULL)
  if (mode == "strict" && is.null(text)) return(paste0(
    "Counts below ", k, " are shown as <", k, "; complementary cells are shown as \u2265", k,
    " to prevent exact reconstruction. Percentages are withheld when their count or selected denominator is suppressed. ",
    "Tests and association estimates are withheld when primary counts are protected."))
  marker <- if (is.null(text)) paste0("shown as <", k) else if (!nzchar(text))
    "withheld as blank cells" else "shown using the chosen masking text"
  if (mode == "primary") return(paste0("Counts from 1 to ", k - 1L, " are ", marker,
    ". Dependent percentages are withheld. Primary protection masks no complementary counts; ",
    "released totals and computed tests or association estimates may permit reconstruction."))
  paste0("Counts below ", k, " and complementary counts are ", marker,
         "; dependent percentages, tests and association estimates are withheld.")
}
