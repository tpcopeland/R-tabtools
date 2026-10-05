# Task 6.2: Stata's table1_tc is a thin wrapper that runs desctab
# (table1_tc.ado: `desctab `0'`), so the two commands take the same options
# and produce the same table. R mirrors that with an alias. It forwards its
# arguments rather than copying table1_tc's closure, which would make this
# file depend on the collation order of R/.

#' @rdname table1_tc
#' @order 2
#' @details `desctab()` is an alias of `table1_tc()`, as in Stata, where
#'   `table1_tc` runs `desctab`: it takes the same arguments (`data`,
#'   `vars`, `by`, and the rest passed on through `...`) and returns the
#'   same table.
#' @param ... For `desctab()`: further arguments passed on to `table1_tc()`.
#' @export
desctab <- function(data, vars = NULL, by = NULL, ...) table1_tc(data, vars, by, ...)
