/* Sequential double-precision summation, as Stata's Mata sum() does it.
 *
 * _t1tcfc_collect_mata (_desctab_collect.ado:1535-1557) forms the sums of a
 * continuous variable with Mata's sum(), which adds the elements in data
 * order in IEEE double (quadsum() is the quad-precision variant). R's sum()
 * accumulates in long double on x86_64, so a mean or SD at a display
 * rounding tie (e.g. a constant 0.15 in 15,000 rows) can round the other way,
 * and Stata's ss = sum(y^2) - sum(y)^2 / n can be a small negative number
 * that prints as "." where R's is ~0.
 *
 * The caller forms any products (y * y, w * y) in R, so this loop contains
 * only additions: nothing for the compiler to contract into a fused
 * multiply-add. The accumulator is volatile so every partial sum is rounded
 * to double even where registers are wider (x87).
 */
#include <R.h>
#include <Rinternals.h>

SEXP tt_seqsum_c(SEXP x_)
{
    if (TYPEOF(x_) != REALSXP) error("'x' must be a double vector");
    const double *x = REAL(x_);
    R_xlen_t n = XLENGTH(x_);
    volatile double s = 0.0;
    for (R_xlen_t i = 0; i < n; i++) s = s + x[i];
    return ScalarReal(s);
}
