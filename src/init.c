#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

extern SEXP c_parse_arff_levels(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_keep_in_bounds", (DL_FUNC) &c_parse_arff_levels, 1},
    {NULL, NULL, 0}
};

void R_init_mlr3misc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
