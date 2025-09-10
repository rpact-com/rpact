#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

List logRankTest(NumericVector accrualTime, NumericVector survivalTime,
		NumericVector dropoutTime, IntegerVector treatmentGroup,
		double time, bool directionUpper, double thetaH0, bool returnRawData);
