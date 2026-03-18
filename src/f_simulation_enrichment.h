#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

LogicalVector selectPopulations(NumericVector effectVector,
                               std::string typeOfSelection,
                               double epsilonValue = NA_REAL,
                               int rValue = NA_INTEGER,
                               double threshold = NA_REAL);

List performClosedCombinationTestForSimulationEnrichment(
    List stageResults,
    Environment design,
    IntegerMatrix indices,
    std::string intersectionTest,
    std::string successCriterion);