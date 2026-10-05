# Stata's default runiform() stream and the Shapiro-Wilk subsample that
# _tabtools_detect_vartype draws from it (plan task 2.4b).

#' Stata mt64 runiform() draws
#'
#' Reproduces `set seed <seed>` followed by `runiform()` in Stata 17 with its
#' default generator (`c(rng_current) == "mt64"`): the reference MT19937-64
#' seeded with `init_genrand64(seed)`, returning `genrand64_real3()`. The
#' generator runs in C with its own local state, so R's `.Random.seed` is
#' never read or changed.
#'
#' @param n Number of draws.
#' @param seed Stata seed, a whole number in 0-2147483647.
#' @return Numeric vector of `n` draws on (0, 1).
#' @keywords internal
#' @noRd
stata_runiform <- function(n, seed) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 || n != floor(n)) {
    cli::cli_abort("{.arg n} must be a single non-negative whole number.", call = NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != floor(seed) ||
      seed < 0 || seed > 2147483647) {
    cli::cli_abort("{.arg seed} must be a whole number between 0 and 2147483647.", call = NULL)
  }
  .Call(tt_stata_runiform_c, as.double(n), as.double(seed))
}

#' Rows Stata uses for the auto-typing Shapiro-Wilk test
#'
#' Mirrors `_tabtools_common.ado:376-393`: when a variable has more than
#' `size` non-missing values, Stata keeps the non-missing rows in data order,
#' runs `set seed 12345`, draws `u = runiform()` for each row, sorts by
#' `(u, row)` and tests the first `size` rows. The seed is re-set for every
#' variable, so each variable sees the same stream.
#'
#' Parity assumes `x` arrives in the same row order as the Stata dataset.
#'
#' @param x Vector (the whole column, missing values included).
#' @param size Subsample size (Stata: 2000).
#' @param seed Stata seed (Stata: 12345).
#' @return Integer positions in `x` of the selected rows, in the order Stata
#'   sorts them. With `size` or fewer non-missing values, every non-missing
#'   position in data order (Stata then tests them all).
#' @keywords internal
#' @noRd
tt_swilk_subsample <- function(x, size = 2000L, seed = 12345L) {
  pos <- which(!is.na(x))
  if (length(pos) <= size) return(pos)
  u <- stata_runiform(length(pos), seed)
  pos[order(u, seq_along(pos))[seq_len(size)]]
}
