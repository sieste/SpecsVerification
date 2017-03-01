#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP SpecsVerification_auc_cpp(SEXP, SEXP);
extern SEXP SpecsVerification_aucdiff_cpp(SEXP, SEXP, SEXP);
extern SEXP SpecsVerification_dresscrps_cpp(SEXP, SEXP, SEXP);
extern SEXP SpecsVerification_enscrps_cpp(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"SpecsVerification_auc_cpp",       (DL_FUNC) &SpecsVerification_auc_cpp,       2},
    {"SpecsVerification_aucdiff_cpp",   (DL_FUNC) &SpecsVerification_aucdiff_cpp,   3},
    {"SpecsVerification_dresscrps_cpp", (DL_FUNC) &SpecsVerification_dresscrps_cpp, 3},
    {"SpecsVerification_enscrps_cpp",   (DL_FUNC) &SpecsVerification_enscrps_cpp,   3},
    {NULL, NULL, 0}
};

void R_init_SpecsVerification(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
