# Table 1 policy. Native _tabtools_smallcells_opt.ado:31-79 and
# _tabtools_smallcells.ado:616-669 (2.5.1); session transport is shared.
.t1_resolve_mask <- function(original, given, resolved) {
  supplied <- function(key) isTRUE(given[[key]])
  no <- if (supplied("nosmallcells")) original$nosmallcells else FALSE
  if (!is.logical(no) || !is.null(dim(no)) || length(no) != 1L || is.na(no)) {
    cli::cli_abort("{.arg nosmallcells} must be TRUE or FALSE.",
                   class = "tabtools_error_smallcells", call = NULL)
  }
  if (no && supplied("smallcells") && !is.null(original$smallcells)) {
    cli::cli_abort("{.arg smallcells} and {.arg nosmallcells} may not be combined.",
                   class = "tabtools_error_smallcells_conflict", call = NULL)
  }
  k <- if (no) NULL else resolved$smallcells
  if (!is.null(k)) {
    if (!is.numeric(k) || is.complex(k) || !is.null(dim(k)) || length(k) != 1L ||
        !is.finite(k) || k != floor(k) || k < 3 || k > .Machine$integer.max) {
      cli::cli_abort("{.arg smallcells} must be an integer greater than or equal to 3.",
                     call = NULL)
    }
    k <- as.integer(k)
  }
  mode <- if (is.null(k) && !supplied("smallcells_mode")) "strict" else
    resolved$smallcells_mode %||% "strict"
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
      !is.null(dim(mode)) || !mode %in% c("strict", "primary")) {
    cli::cli_abort("{.arg smallcells_mode} must be {.val strict}, {.val primary}, or NULL.",
                   class = "tabtools_error_smallcells_mode", call = NULL)
  }
  text <- resolved$masktext
  if (!is.null(text)) {
    if (!is.character(text) || length(text) != 1L || is.na(text) || !is.null(dim(text))) {
      cli::cli_abort("{.arg masktext} must be a single nonmissing string or NULL.",
                     class = "tabtools_error_smallcells_masktext", call = NULL)
    }
    if (supplied("masktext") && is.null(k)) {
      cli::cli_abort("{.arg masktext} requires a small-cell threshold.",
                     class = "tabtools_error_smallcells_masktext", call = NULL)
    }
  }
  list(threshold = k, mode = mode, text = if (is.null(k)) NULL else text)
}

# Exact printed cells/margins only. Keep integer-count safeguards even though
# this mode does not search for complementary reconstruction protection.
.t1_sc_primary <- function(block, k) {
  c <- block$counts
  nr <- nrow(c)
  nc <- ncol(c)
  upper <- sum(c) + 2 * k * (nr * nc + nr + nc + 1) + 10
  if (!is.numeric(c) || is.complex(c) || nr < 1L || nc < 1L ||
      any(!is.finite(c) | c < 0 | c != floor(c)) ||
      !is.finite(upper) || upper >= 2^53) {
    .sc_abort_input("Counts and flow bounds must be nonnegative integers below 2^53.")
  }
  exact <- block$exact
  rowexact <- block$rowexact
  # Primary does not consider hidden slashN-derived missing/negative rows
  # exact. Only an actually printed missing row can be exact.
  if (block$type != "cont") {
    hidden <- setdiff(seq_len(nr), seq_len(block$n_levels))
    if (length(hidden)) {
      exact[hidden, ] <- 0
      rowexact[hidden] <- 0
      if (isTRUE(block$print_missing) && block$missrow > 0L) {
        exact[block$missrow, ] <- 1
        rowexact[block$missrow] <- as.integer(block$total)
      }
    }
  }
  below <- function(x) as.integer(x > 0 & x < k)
  mask <- exact * matrix(below(c), nr, nc)
  rowmask <- rowexact * below(rowSums(c))
  colmask <- block$colexact * below(colSums(c))
  totalmask <- as.integer(block$grandexact == 1 && sum(c) > 0 && sum(c) < k)
  list(mask = mask, rowmask = rowmask, colmask = colmask, totalmask = totalmask,
       n_primary = sum(mask) + sum(rowmask) + sum(colmask) + totalmask,
       n_secondary = 0L, smallcells = k)
}

.t1_sc_render <- function(value, code, o) {
  if (code > 0L && !is.null(o$masktext)) return(o$masktext)
  as.character(tt_sc_render(value, code, o$smallcells, o$nformat))
}

.t1_sc_note <- function(k, mode, text) {
  if (mode == "strict" && is.null(text)) return(tt_sc_footnote(k))
  marker <- if (is.null(text)) paste0("shown as <", k) else if (!nzchar(text))
    "withheld as blank cells" else "shown using the chosen masking text"
  if (mode == "primary") {
    return(paste0("Counts from 1 to ", k - 1L, " are ", marker,
      " without a percentage (primary suppression only: no complementary cells are masked). ",
      "Unmasked cells and ordinary variable tests are shown as computed; effective sample size ",
      "linked to a masked sample count is withheld. This protects printed counts only."))
  }
  paste0("Counts below ", k, " and complementary cells are ", marker,
         "; dependent statistics and percentages are withheld for variables carrying protected counts.")
}
