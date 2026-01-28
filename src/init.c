#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern SEXP c_parse_arff_levels(SEXP);
extern SEXP c_remove_comment(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_parse_arff_levels", (DL_FUNC) &c_parse_arff_levels, 1},
    {"c_remove_comment", (DL_FUNC) &c_remove_comment, 1},
    {NULL, NULL, 0}
};

void R_init_mlr3oml(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
