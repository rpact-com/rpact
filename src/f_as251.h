#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

double as251Normal(NumericVector lower, NumericVector upper, NumericMatrix sigma,
                   double eps = 1e-06,
                   String errorControl = "strict",
                   double intervalSimpsonsRule = 0.0);
