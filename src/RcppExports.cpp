// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// findalpha
Rcpp::NumericVector findalpha(Rcpp::NumericVector& p, int m, Rcpp::NumericVector& simesfactor, bool simes);
RcppExport SEXP _hommel_findalpha(SEXP pSEXP, SEXP mSEXP, SEXP simesfactorSEXP, SEXP simesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type simesfactor(simesfactorSEXP);
    Rcpp::traits::input_parameter< bool >::type simes(simesSEXP);
    rcpp_result_gen = Rcpp::wrap(findalpha(p, m, simesfactor, simes));
    return rcpp_result_gen;
END_RCPP
}
// findsimesfactor
Rcpp::NumericVector findsimesfactor(bool simes, int m);
RcppExport SEXP _hommel_findsimesfactor(SEXP simesSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type simes(simesSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(findsimesfactor(simes, m));
    return rcpp_result_gen;
END_RCPP
}
// adjustedElementary
Rcpp::NumericVector adjustedElementary(Rcpp::NumericVector& p, Rcpp::NumericVector& alpha, int m, Rcpp::NumericVector& simesfactor);
RcppExport SEXP _hommel_adjustedElementary(SEXP pSEXP, SEXP alphaSEXP, SEXP mSEXP, SEXP simesfactorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type p(pSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type simesfactor(simesfactorSEXP);
    rcpp_result_gen = Rcpp::wrap(adjustedElementary(p, alpha, m, simesfactor));
    return rcpp_result_gen;
END_RCPP
}
// adjustedIntersection
double adjustedIntersection(double pI, Rcpp::NumericVector& alpha, int m, Rcpp::NumericVector& simesfactor);
RcppExport SEXP _hommel_adjustedIntersection(SEXP pISEXP, SEXP alphaSEXP, SEXP mSEXP, SEXP simesfactorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type pI(pISEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type simesfactor(simesfactorSEXP);
    rcpp_result_gen = Rcpp::wrap(adjustedIntersection(pI, alpha, m, simesfactor));
    return rcpp_result_gen;
END_RCPP
}
// findHalpha
int findHalpha(Rcpp::NumericVector& jumpalpha, double alpha, int m);
RcppExport SEXP _hommel_findHalpha(SEXP jumpalphaSEXP, SEXP alphaSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type jumpalpha(jumpalphaSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(findHalpha(jumpalpha, alpha, m));
    return rcpp_result_gen;
END_RCPP
}
// findConcentration
int findConcentration(Rcpp::NumericVector& p, double simesfactor, int h, double alpha, int m);
RcppExport SEXP _hommel_findConcentration(SEXP pSEXP, SEXP simesfactorSEXP, SEXP hSEXP, SEXP alphaSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type simesfactor(simesfactorSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(findConcentration(p, simesfactor, h, alpha, m));
    return rcpp_result_gen;
END_RCPP
}
// findDiscoveries
Rcpp::IntegerVector findDiscoveries(Rcpp::IntegerVector& idx, Rcpp::NumericVector& allp, double simesfactor, int h, double alpha, int k, int m);
RcppExport SEXP _hommel_findDiscoveries(SEXP idxSEXP, SEXP allpSEXP, SEXP simesfactorSEXP, SEXP hSEXP, SEXP alphaSEXP, SEXP kSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type idx(idxSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type allp(allpSEXP);
    Rcpp::traits::input_parameter< double >::type simesfactor(simesfactorSEXP);
    Rcpp::traits::input_parameter< int >::type h(hSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(findDiscoveries(idx, allp, simesfactor, h, alpha, k, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_hommel_findalpha", (DL_FUNC) &_hommel_findalpha, 4},
    {"_hommel_findsimesfactor", (DL_FUNC) &_hommel_findsimesfactor, 2},
    {"_hommel_adjustedElementary", (DL_FUNC) &_hommel_adjustedElementary, 4},
    {"_hommel_adjustedIntersection", (DL_FUNC) &_hommel_adjustedIntersection, 4},
    {"_hommel_findHalpha", (DL_FUNC) &_hommel_findHalpha, 3},
    {"_hommel_findConcentration", (DL_FUNC) &_hommel_findConcentration, 5},
    {"_hommel_findDiscoveries", (DL_FUNC) &_hommel_findDiscoveries, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_hommel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
