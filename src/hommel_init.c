#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP hommel_adjustedElementary(SEXP, SEXP, SEXP, SEXP);
extern SEXP hommel_adjustedIntersection(SEXP, SEXP, SEXP, SEXP);
extern SEXP hommel_findalpha(SEXP, SEXP, SEXP, SEXP);
extern SEXP hommel_findConcentration(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hommel_findDiscoveries(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hommel_findHalpha(SEXP, SEXP, SEXP);
extern SEXP hommel_findsimesfactor(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"hommel_adjustedElementary",   (DL_FUNC) &hommel_adjustedElementary,   4},
  {"hommel_adjustedIntersection", (DL_FUNC) &hommel_adjustedIntersection, 4},
  {"hommel_findalpha",            (DL_FUNC) &hommel_findalpha,            4},
  {"hommel_findConcentration",    (DL_FUNC) &hommel_findConcentration,    5},
  {"hommel_findDiscoveries",      (DL_FUNC) &hommel_findDiscoveries,      7},
  {"hommel_findHalpha",           (DL_FUNC) &hommel_findHalpha,           3},
  {"hommel_findsimesfactor",      (DL_FUNC) &hommel_findsimesfactor,      2},
  {NULL, NULL, 0}
};

void R_init_hommel(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
