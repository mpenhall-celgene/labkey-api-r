// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// listToMatrix
CharacterMatrix listToMatrix(List data, List names);
RcppExport SEXP Rlabkey_listToMatrix(SEXP dataSEXP, SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< List >::type names(namesSEXP);
    rcpp_result_gen = Rcpp::wrap(listToMatrix(data, names));
    return rcpp_result_gen;
END_RCPP
}