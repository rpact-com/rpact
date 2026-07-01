#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

LogicalVector selectTreatmentArms(NumericVector effectVector,
                                  std::string typeOfSelection,
                                  double epsilonValue,
                                  int rValue,
                                  double threshold,
                                  bool survival);

List performClosedConditionalDunnettTestForSimulation(
        List stageResults,
        Environment design,
        LogicalMatrix indices,
        NumericVector criticalValuesDunnett,
        std::string successCriterion);

List performClosedCombinationTestForSimulationMultiArm(
        List stageResults,
        Environment design,
        LogicalMatrix indices,
        std::string intersectionTest,
        std::string successCriterion);
