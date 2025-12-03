#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

LogicalVector selectTreatmentArms(NumericVector effectVector,
                                  std::string typeOfSelection,
                                  double epsilonValue = NA_REAL,
                                  int rValue = NA_INTEGER,
                                  double threshold = NA_REAL,
                                  bool survival = false);
