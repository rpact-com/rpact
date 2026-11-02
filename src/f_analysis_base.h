#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

NumericVector getWeightsFisher(const Environment& design);

NumericVector getWeightsInverseNormal(const Environment& design);
