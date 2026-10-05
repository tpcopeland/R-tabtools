#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP tt_stata_runiform_c(SEXP n_, SEXP seed_);
extern SEXP tt_seqsum_c(SEXP x_);
extern SEXP tt_fmt_c(SEXP x_, SEXP d_, SEXP mode_, SEXP alt_);

static const R_CallMethodDef call_methods[] = {
    {"tt_stata_runiform_c", (DL_FUNC) &tt_stata_runiform_c, 2},
    {"tt_seqsum_c", (DL_FUNC) &tt_seqsum_c, 1},
    {"tt_fmt_c", (DL_FUNC) &tt_fmt_c, 4},
    {NULL, NULL, 0}
};

void R_init_tabtools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
