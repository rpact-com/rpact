#include <R_ext/Applic.h>
#include <Rcpp.h>
#include <cmath>

#include "f_analysis_base.h"
#include "f_utilities.h"
#include "rpact_types.h"
#include "f_simulation_multiarm.h"

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// Template function for numerical integration over (-Inf, +Inf)
// T must have a static wrapperFunction(double *x, int n, void *ex) method
template<typename T>
double integrateInfinite(const T* integrand, 
                        double epsabs = 1e-4, 
                        double epsrel = 1e-4) {
    void *ex = (void*) integrand;
    
    double bound = 0.0;  // is ignored for inf = 2
    int inf = 2;         // corresponds to (-Inf, +Inf)
    double result = 0.0;
    double abserr = 0.0;
    int neval = 0;
    int ier = 0;
    int limit = 100;
    int lenw = 400;
    int last = 0;
    int iwork[100];
    double work[400];
    
    Rdqagi(T::wrapperFunction, ex, &bound, &inf, &epsabs, &epsrel, &result, 
           &abserr, &neval, &ier, &limit, &lenw, &last, iwork, work);
    
    if (ier != 0) {
        Rcpp::warning("Integration error code: %d", ier);
    }
    
    return result;
}

// Base class for Dunnett integrands
class DunnettIntegrandBase {
protected:
    double informationAtInterim;
    NumericMatrix signedTestStatistics;
    NumericVector frac;
    LogicalVector indicesRow;
    int gMax;
    
public:
    // Constructor
    DunnettIntegrandBase(double informationAtInterim_,
                        NumericMatrix signedTestStatistics_,
                        NumericVector frac_,
                        LogicalVector indicesRow_)
        : informationAtInterim(informationAtInterim_),
          signedTestStatistics(signedTestStatistics_),
          frac(frac_),
          indicesRow(indicesRow_) {
        gMax = signedTestStatistics.nrow();
    }
    
    virtual ~DunnettIntegrandBase() {}
    
    // Pure virtual function to be implemented by derived classes
    virtual double evaluate(double x) const = 0;
    
    // Vectorized evaluation for integration
    void evaluateVector(double *x, int n) const {
        for (int i = 0; i < n; i++) {
            x[i] = evaluate(x[i]);
        }
    }
    
    // Integration function - can be used by all derived classes
    template<typename Derived>
    double integrate() const {
        return integrateInfinite(static_cast<const Derived*>(this));
    }
};

// Template function to generate static wrapper for derived classes
template<typename T>
void dunnettWrapperFunction(double *x, int n, void *ex) {
    const T *integrand = static_cast<const T*>(ex);
    integrand->evaluateVector(x, n);
}

class DunnettIntegrand1 : public DunnettIntegrandBase {
private:
    double criticalValue;
    
public:
    // Constructor
    DunnettIntegrand1(double criticalValue_,
                     double informationAtInterim_,
                     NumericMatrix signedTestStatistics_,
                     NumericVector frac_,
                     LogicalVector indicesRow_)
        : DunnettIntegrandBase(informationAtInterim_, signedTestStatistics_, frac_, indicesRow_),
          criticalValue(criticalValue_) {
    }
    
    // Function to evaluate the integrand at a single point
    double evaluate(double x) const override {
        double innerProduct = 1.0;
        
        for (int g = 0; g < gMax; g++) {
            if (indicesRow[g]) {
                double numerator = criticalValue -
                    sqrt(informationAtInterim) * signedTestStatistics(g, 0) +
                    sqrt(1.0 - informationAtInterim) * sqrt(frac[g]) * x;
                double denominator = sqrt((1.0 - informationAtInterim) * (1.0 - frac[g]));
                
                innerProduct *= R::pnorm(numerator / denominator, 0.0, 1.0, 1, 0);
            }
        }
        
        return innerProduct * R::dnorm(x, 0.0, 1.0, 0);
    }
    
    // Static wrapper function for Rdqagi
    static void wrapperFunction(double *x, int n, void *ex) {
        dunnettWrapperFunction<DunnettIntegrand1>(x, n, ex);
    }
};

class DunnettIntegrand2 : public DunnettIntegrandBase {
private:
    double maxOverallTestStatistic;
    NumericMatrix overallTestStatistics;
    
public:
    // Constructor
    DunnettIntegrand2(double maxOverallTestStatistic_,
                     double informationAtInterim_,
                     NumericMatrix signedTestStatistics_,
                     NumericVector frac_,
                     LogicalVector indicesRow_,
                     NumericMatrix overallTestStatistics_)
        : DunnettIntegrandBase(informationAtInterim_, signedTestStatistics_, frac_, indicesRow_),
          maxOverallTestStatistic(maxOverallTestStatistic_),
          overallTestStatistics(overallTestStatistics_) {
    }
    
    // Function to evaluate the integrand at a single point
    double evaluate(double x) const override {
        double innerProduct = 1.0;
        
        for (int g = 0; g < gMax; g++) {
            if (indicesRow[g] && !R_IsNA(overallTestStatistics(g, 1))) {
                double numerator = maxOverallTestStatistic -
                    sqrt(informationAtInterim) * signedTestStatistics(g, 0) +
                    sqrt(1.0 - informationAtInterim) * sqrt(frac[g]) * x;
                double denominator = sqrt((1.0 - informationAtInterim) * (1.0 - frac[g]));
                
                innerProduct *= R::pnorm(numerator / denominator, 0.0, 1.0, 1, 0);
            }
        }
        
        return innerProduct * R::dnorm(x, 0.0, 1.0, 0);
    }
    
    // Static wrapper function for Rdqagi
    static void wrapperFunction(double *x, int n, void *ex) {
        dunnettWrapperFunction<DunnettIntegrand2>(x, n, ex);
    }
};

class DunnettIntegrand3 : public DunnettIntegrandBase {
private:
    double maxTestStatistic;
    NumericMatrix separatePValues;
    
public:
    // Constructor
    DunnettIntegrand3(double maxTestStatistic_,
                     NumericVector frac_,
                     LogicalVector indicesRow_,
                     NumericMatrix separatePValues_)
        : DunnettIntegrandBase(0.0, // informationAtInterim not used in this integrand
                              NumericMatrix(0, 0), // signedTestStatistics not used
                              frac_,
                              indicesRow_),
          maxTestStatistic(maxTestStatistic_),
          separatePValues(separatePValues_) {
        gMax = separatePValues.nrow();
    }
    
    // Function to evaluate the integrand at a single point
    double evaluate(double x) const override {
        double innerProduct = 1.0;
        
        for (int g = 0; g < gMax; g++) {
            if (indicesRow[g] && !R_IsNA(separatePValues(g, 1))) {
                double numerator = maxTestStatistic + sqrt(frac[g]) * x;
                double denominator = sqrt(1.0 - frac[g]);
                
                innerProduct *= R::pnorm(numerator / denominator, 0.0, 1.0, 1, 0);
            }
        }
        
        return innerProduct * R::dnorm(x, 0.0, 1.0, 0);
    }
    
    // Static wrapper function for Rdqagi
    static void wrapperFunction(double *x, int n, void *ex) {
        dunnettWrapperFunction<DunnettIntegrand3>(x, n, ex);
    }
};

// [[Rcpp::export(name = ".dunnettIntegrand1IntCpp")]]
double dunnettIntegrand1Int(double criticalValue,
                        double informationAtInterim,
                        NumericMatrix signedTestStatistics,
                        NumericVector frac,
                        LogicalVector indicesRow) {
    
    DunnettIntegrand1 integrand(criticalValue, informationAtInterim, 
                               signedTestStatistics, frac, indicesRow);
    
    return integrand.integrate<DunnettIntegrand1>();
}

// [[Rcpp::export(name = ".dunnettIntegrand1EvaluateCpp")]]
NumericVector dunnettIntegrand1Evaluate(NumericVector x,
                                         double criticalValue,
                                         double informationAtInterim,
                                         NumericMatrix signedTestStatistics,
                                         NumericVector frac,
                                         LogicalVector indicesRow) {
    
    DunnettIntegrand1 integrand(criticalValue, informationAtInterim, 
                               signedTestStatistics, frac, indicesRow);
    
    int n = x.size();
    NumericVector result(n);
    
    for (int i = 0; i < n; i++) {
        result[i] = integrand.evaluate(x[i]);
    }
    
    return result;
}

// [[Rcpp::export(name = ".dunnettIntegrand2IntCpp")]]
double dunnettIntegrand2Int(double maxOverallTestStatistic,
                        double informationAtInterim,
                        NumericMatrix signedTestStatistics,
                        NumericVector frac,
                        LogicalVector indicesRow,
                        NumericMatrix overallTestStatistics) {
    
    DunnettIntegrand2 integrand(maxOverallTestStatistic, informationAtInterim, 
                               signedTestStatistics, frac, indicesRow, 
                               overallTestStatistics);
    
    return integrand.integrate<DunnettIntegrand2>();
}

// [[Rcpp::export(name = ".dunnettIntegrand2EvaluateCpp")]]
NumericVector dunnettIntegrand2Evaluate(NumericVector x,
                                         double maxOverallTestStatistic,
                                         double informationAtInterim,
                                         NumericMatrix signedTestStatistics,
                                         NumericVector frac,
                                         LogicalVector indicesRow,
                                         NumericMatrix overallTestStatistics) {
    
    DunnettIntegrand2 integrand(maxOverallTestStatistic, informationAtInterim, 
                               signedTestStatistics, frac, indicesRow,
                               overallTestStatistics);
    
    int n = x.size();
    NumericVector result(n);
    
    for (int i = 0; i < n; i++) {
        result[i] = integrand.evaluate(x[i]);
    }
    
    return result;
}

// [[Rcpp::export(name = ".dunnettIntegrand3IntCpp")]]
double dunnettIntegrand3Int(double maxTestStatistic,
                        NumericVector frac,
                        LogicalVector indicesRow,
                        NumericMatrix separatePValues) {
    
    DunnettIntegrand3 integrand(maxTestStatistic, frac, indicesRow, separatePValues);
    
    return integrand.integrate<DunnettIntegrand3>();
}

// [[Rcpp::export(name = ".dunnettIntegrand3EvaluateCpp")]]
NumericVector dunnettIntegrand3Evaluate(NumericVector x,
                                         double maxTestStatistic,
                                         NumericVector frac,
                                         LogicalVector indicesRow,
                                         NumericMatrix separatePValues) {
    
    DunnettIntegrand3 integrand(maxTestStatistic, frac, indicesRow, separatePValues);
    
    int n = x.size();
    NumericVector result(n);
    
    for (int i = 0; i < n; i++) {
        result[i] = integrand.evaluate(x[i]);
    }
    
    return result;
}

// [[Rcpp::export(name = ".performClosedConditionalDunnettTestForSimulationCpp")]]
List performClosedConditionalDunnettTestForSimulation(
        List stageResults,
        Environment design,
        LogicalMatrix indices,
        NumericVector criticalValuesDunnett,
        std::string successCriterion) {
    
    NumericMatrix testStatistics = stageResults["testStatistics"];
    NumericMatrix separatePValues = stageResults["separatePValues"];
    NumericMatrix overallTestStatistics = stageResults["overallTestStatistics"];
    
    int gMax = testStatistics.nrow();
    double informationAtInterim = Rcpp::as<double>(design.get("informationAtInterim"));
    bool secondStageConditioning = Rcpp::as<bool>(design.get("secondStageConditioning"));
    int kMax = 2;
    
    NumericVector allocationRatioPlanned = stageResults["allocationRatioPlanned"];
    NumericVector frac = rep(
        allocationRatioPlanned[0] / (1.0 + allocationRatioPlanned[0]), 
        gMax
    );
    
    NumericMatrix conditionalErrorRate(pow(2, gMax) - 1, 2);
    std::fill(conditionalErrorRate.begin(), conditionalErrorRate.end(), NA_REAL);
    NumericMatrix secondStagePValues(pow(2, gMax) - 1, 2);
    std::fill(secondStagePValues.begin(), secondStagePValues.end(), NA_REAL);
    LogicalMatrix rejected(gMax, 2);
    std::fill(rejected.begin(), rejected.end(), false);
    LogicalMatrix rejectedIntersections(indices.nrow(), kMax);
    std::fill(rejectedIntersections.begin(), rejectedIntersections.end(), false);
    bool futilityStop = false;
    LogicalVector successStop(kMax, false);
    
    NumericMatrix signedTestStatistics = clone(testStatistics);
    NumericMatrix signedOverallTestStatistics = clone(overallTestStatistics);
    signedOverallTestStatistics(_, 1) = sqrt(informationAtInterim) * testStatistics(_, 0) +
            sqrt(1.0 - informationAtInterim) * testStatistics(_, 1);   
    
    LogicalMatrix selectedArms = stageResults["selectedArms"];
    bool anySelected = Rcpp::as<bool>(any(na_omit(selectedArms(_, 1))));
    if (!anySelected) {
        futilityStop = true;
    }
    
    for (int i = 0; i < indices.nrow(); i++) {
        LogicalVector indicesRow = indices(i, _);
        
        double int1 = dunnettIntegrand1Int(
            criticalValuesDunnett[i],
            informationAtInterim,
            signedTestStatistics,
            frac,
            indicesRow
        );
        conditionalErrorRate(i, 0) = 1.0 - int1;
        
        // Check if all separatePValues for selected indices are NA
        NumericVector separatePValues2ndStage = separatePValues(_, 1);
        separatePValues2ndStage = separatePValues2ndStage[which(indicesRow)];
        separatePValues2ndStage = na_omit(separatePValues2ndStage);
        bool any_sep_pvalue_available = (separatePValues2ndStage.size() > 0);
                
        if (any_sep_pvalue_available) {
            if (secondStageConditioning) {
                NumericVector signedOverallTestStatistics2ndStage = signedOverallTestStatistics(_, 1);
                signedOverallTestStatistics2ndStage = signedOverallTestStatistics2ndStage[which(indicesRow)];
                signedOverallTestStatistics2ndStage = na_omit(signedOverallTestStatistics2ndStage);
                double maxOverallTestStatistic = max(signedOverallTestStatistics2ndStage);
                
                double int2 = dunnettIntegrand2Int(
                    maxOverallTestStatistic,
                    informationAtInterim,
                    signedTestStatistics,
                    frac,
                    indicesRow,
                    overallTestStatistics
                );
                secondStagePValues(i, 1) = 1.0 - int2;
            } else {
                NumericVector signedTestStatistics2ndStage = signedTestStatistics(_, 1);
                signedTestStatistics2ndStage = signedTestStatistics2ndStage[which(indicesRow)];
                signedTestStatistics2ndStage = na_omit(signedTestStatistics2ndStage);
                double maxTestStatistic = max(signedTestStatistics2ndStage);
               
                double int3 = dunnettIntegrand3Int(
                    maxTestStatistic,
                    frac,
                    indicesRow,
                    separatePValues
                );
                secondStagePValues(i, 1) = 1.0 - int3;
            }
        }
        
        rejectedIntersections(i, 1) = (secondStagePValues(i, 1) <= conditionalErrorRate(i, 0));
        
        LogicalVector missingRejectedIntersections2ndStage = is_na(rejectedIntersections(_, 1));
        IntegerVector missingIndices = which(missingRejectedIntersections2ndStage);
        for (int j = 0; j < missingIndices.size(); j++) {
            int missingIndex = missingIndices[j];
            rejectedIntersections(missingIndex, 1) = false;
        }
        
        if (!rejectedIntersections(0, 1)) {
            break;
        }
    }
    
    for (int j = 0; j < gMax; j++) {
        bool allRejected = true;
        for (int i = 0; i < indices.nrow(); i++) {
            if (indices(i, j)) {
                if (R_IsNA(rejectedIntersections(i, 1))) {
                    continue;
                }
                if (!rejectedIntersections(i, 1)) {
                    allRejected = false;
                    break;
                }
            }
        }
        rejected(j, 1) = allRejected;
    }
    
    if (successCriterion == "all") {
        bool allRejected = true;
        for (int g = 0; g < gMax; g++) {
            if (selectedArms(g, 1) && !rejected(g, 1)) {
                allRejected = false;
                break;
            }
        }
        successStop[1] = allRejected;
    } else {
        bool anyRejected = false;
        for (int g = 0; g < gMax; g++) {
            if (rejected(g, 1)) {
                anyRejected = true;
                break;
            }
        }
        successStop[1] = anyRejected;
    }
    
    return List::create(
        _["separatePValues"] = separatePValues,
        _["conditionalErrorRate"] = conditionalErrorRate,
        _["secondStagePValues"] = secondStagePValues,
        _["rejected"] = rejected,
        _["rejectedIntersections"] = rejectedIntersections,
        _["selectedArms"] = selectedArms,
        _["successStop"] = successStop,
        _["futilityStop"] = futilityStop
    );
}

// [[Rcpp::export(name = ".performClosedCombinationTestForSimulationMultiArmCpp")]]
List performClosedCombinationTestForSimulationMultiArm(
        List stageResults,
        Environment design,
        LogicalMatrix indices,
        std::string intersectionTest,
        std::string successCriterion) {
    
    // Check design type compatibility
    std::string designClass = getClassName(design);
    bool isGroupSequential = designClass == "TrialDesignGroupSequential";
    int kMax = Rcpp::as<int>(design.get("kMax"));
    
    if (isGroupSequential && kMax > 1) {
        stop("Group sequential design cannot be used for designs with treatment arm selection");
    }
    
    // Extract stage results
    NumericMatrix testStatistics = stageResults["testStatistics"];
    NumericMatrix separatePValues = stageResults["separatePValues"];
    LogicalMatrix selectedArms = stageResults["selectedArms"];
    
    int gMax = testStatistics.nrow();
    int nIntersections = indices.nrow();
    
    // Verify that nIntersections equals 2^gMax - 1
    int expectedIntersections = static_cast<int>(pow(2, gMax)) - 1;
    if (nIntersections != expectedIntersections) {
        stop("Number of intersections must equal 2^gMax - 1");
    }
    
    // Initialize result matrices
    NumericMatrix adjustedStageWisePValues(nIntersections, kMax);
    std::fill(adjustedStageWisePValues.begin(), adjustedStageWisePValues.end(), NA_REAL);
    
    NumericMatrix overallAdjustedTestStatistics(nIntersections, kMax);
    std::fill(overallAdjustedTestStatistics.begin(), overallAdjustedTestStatistics.end(), NA_REAL);
    
    LogicalMatrix rejected(gMax, kMax);
    std::fill(rejected.begin(), rejected.end(), false);
    
    LogicalMatrix rejectedIntersections(nIntersections, kMax);
    std::fill(rejectedIntersections.begin(), rejectedIntersections.end(), false);
    
    LogicalMatrix futility(gMax, kMax - 1);
    std::fill(futility.begin(), futility.end(), false);
    
    LogicalMatrix futilityIntersections(nIntersections, kMax - 1);
    std::fill(futilityIntersections.begin(), futilityIntersections.end(), false);
    
    LogicalVector rejectedIntersectionsBefore(nIntersections);
    std::fill(rejectedIntersectionsBefore.begin(), rejectedIntersectionsBefore.end(), false);
    
    LogicalVector successStop(kMax);
    std::fill(successStop.begin(), successStop.end(), false);
    
    LogicalVector futilityStop(kMax - 1);
    std::fill(futilityStop.begin(), futilityStop.end(), false);
    
    // Get weights based on design type
    bool isDesignFisher = designClass == "TrialDesignFisher";
    bool isDesignInverseNormal = designClass == "TrialDesignInverseNormal";
    NumericVector weights;
    if (isDesignFisher) {
        weights = getWeightsFisher(design);
    } else {
        weights = getWeightsInverseNormal(design);
    }
    
    // For single arm, force Bonferroni
    if (gMax == 1) {
        intersectionTest = "Bonferroni";
    }
    
    // Handle Dunnett-specific data
    NumericMatrix subjectsPerStage;
    bool hasDunnettData = (intersectionTest == "Dunnett");
    if (hasDunnettData) {
        if (stageResults.containsElementNamed("subjectsPerStage")) {
            subjectsPerStage = Rcpp::as<NumericMatrix>(stageResults["subjectsPerStage"]);
        } else if (stageResults.containsElementNamed("cumulativeEventsPerStage")) {
            subjectsPerStage = Rcpp::as<NumericMatrix>(stageResults["cumulativeEventsPerStage"]);
        }
    }
    
    // Main loop over stages
    for (int k = 0; k < kMax; k++) {
        
        NumericVector stageSeparatePValues = separatePValues(_, k);
        NumericVector allocationRatioPlanned = stageResults["allocationRatioPlanned"];
        NumericVector allocationRatiosPerStage(gMax);
        NumericVector stageTestStatistics = testStatistics(_, k);
        
        // For Dunnett, set up allocation ratios
        if (hasDunnettData) {
            for (int g = 0; g < gMax; g++) {
                allocationRatiosPerStage[g] = 
                  R_IsNA(subjectsPerStage(g, k)) ? NA_REAL : allocationRatioPlanned[k];
            }
        }
        
        // Loop over intersection hypotheses
        for (int i = 0; i < nIntersections; i++) {
            
            // Get p-values available for this intersection
            LogicalVector isInIntersection = indices(i, _);
            NumericVector intersectStageSeparatePValues = stageSeparatePValues[isInIntersection];
            intersectStageSeparatePValues = na_omit(intersectStageSeparatePValues);
            int nAvailablePVals = intersectStageSeparatePValues.size();
            bool hasValidPValues = nAvailablePVals > 0;
            
            if (hasValidPValues) {
                // Calculate adjusted stage-wise p-value based on intersection test
                if (intersectionTest == "Dunnett") {
                    // Collect selected allocation ratios
                    NumericVector allocationRatiosSelected = allocationRatiosPerStage[isInIntersection];
                    allocationRatiosSelected = na_omit(allocationRatiosSelected);
                    
                    // Compute covariance matrix step by step
                    NumericVector sigmaVec = sqrt(allocationRatiosSelected / (1.0 + allocationRatiosSelected));
                    NumericMatrix sigma = tcrossprod(sigmaVec);
                    for (int i = 0; i < sigma.nrow(); i++) {
                        sigma(i, i) = 1.0;
                    }
                    
                    double maxTestStatistic = 
                        max(na_omit(Rcpp::as<NumericVector>(stageTestStatistics[isInIntersection])));
                    
                    adjustedStageWisePValues(i, k) = 1.0 - getMultivarNormalDistribution(wrap(maxTestStatistic), sigma);
                    
                } else if (intersectionTest == "Bonferroni") {
                    // Bonferroni adjusted p-values
                    double minAvailablePvals = min(intersectStageSeparatePValues);
                    double bonferroniAdjustedPval = nAvailablePVals * minAvailablePvals;
                    adjustedStageWisePValues(i, k) = std::min(1.0, bonferroniAdjustedPval);
                    
                } else if (intersectionTest == "Simes") {
                    // Simes adjusted p-values
                    NumericVector sortedPValues = clone(intersectStageSeparatePValues);
                    std::sort(sortedPValues.begin(), sortedPValues.end());
                    IntegerVector seqAlongPvals = seq_len(nAvailablePVals);
                    NumericVector seqAlongPvalsDouble = Rcpp::as<NumericVector>(seqAlongPvals);
                    NumericVector simesAdjustedPValues = rep(nAvailablePVals * 1.0, nAvailablePVals) /
                        seqAlongPvalsDouble * sortedPValues;
                    adjustedStageWisePValues(i, k) = min(simesAdjustedPValues);
                    
                } else if (intersectionTest == "Sidak") {
                    // Sidak adjusted p-values
                    double minAvailablePvals = min(intersectStageSeparatePValues);
                    double OneMinusMinPvalsToPower = pow(1.0 - minAvailablePvals, nAvailablePVals);
                    adjustedStageWisePValues(i, k) = 1.0 - OneMinusMinPvalsToPower;
                    
                } else if (intersectionTest == "Hierarchical") {
                    // Hierarchically ordered hypotheses
                    NumericMatrix filledSeparatePValues = separatePValues;
                    for (int j = 0; j < gMax; j++) {
                        for (int l = 0; l < kMax; l++) {
                            if (R_IsNA(filledSeparatePValues(j, l))) {
                                filledSeparatePValues(j, l) = 1.0;
                            }
                        }
                    }
                    NumericVector filledStageSeparatePValues = filledSeparatePValues(_, k);
                    NumericVector intersectFilledStageSeparatePValues = filledStageSeparatePValues[isInIntersection];
                    adjustedStageWisePValues(i, k) = intersectFilledStageSeparatePValues[0];
                }
                
                // Calculate overall adjusted test statistic
                if (isDesignFisher) {
                    double product = 1.0;
                    for (int j = 0; j <= k; j++) {
                        product *= pow(adjustedStageWisePValues(i, j), weights[j]);
                    }
                    overallAdjustedTestStatistics(i, k) = product;
                } else {
                    double numerator = 0.0;
                    double denominator = 0.0;
                    for (int j = 0; j <= k; j++) {
                        numerator += weights[j] * getOneMinusQNorm(adjustedStageWisePValues(i, j));
                        denominator += pow(weights[j], 2.0);
                    }
                    overallAdjustedTestStatistics(i, k) = numerator / sqrt(denominator);
                }
                
            }
            // Determine rejection and futility
            NumericVector criticalValues = design.get("criticalValues");
            
            if (isDesignFisher) {
                rejectedIntersections(i, k) = overallAdjustedTestStatistics(i, k) <= criticalValues[k];
                if (k < kMax - 1) {
                    NumericVector alpha0Vec = design.get("alpha0Vec");
                    futilityIntersections(i, k) = adjustedStageWisePValues(i, k) >= alpha0Vec[k];
                }
            } else if (isDesignInverseNormal) {
                rejectedIntersections(i, k) = overallAdjustedTestStatistics(i, k) >= criticalValues[k];
                if (k < kMax - 1) {
                    NumericVector futilityBounds = design.get("futilityBounds");
                    futilityIntersections(i, k) = overallAdjustedTestStatistics(i, k) <= futilityBounds[k];
                }
            }
            
            // Handle missing values
            for (int j = 0; j < rejectedIntersections.nrow(); j++) {
                if (R_IsNA(rejectedIntersections(j, k))) {
                    rejectedIntersections(j, k) = false;
                }
            }
            
            // Early termination check for final stage
            if (k == kMax - 1 && !rejectedIntersections(0, k)) {
                break;
            }
        }
        
        // Update rejectedIntersections with previous rejections
        rejectedIntersections(_, k) = rejectedIntersections(_, k) | rejectedIntersectionsBefore;
        rejectedIntersectionsBefore = rejectedIntersections(_, k);
        
        LogicalVector futilityIntersectionsBefore;
        if (k < kMax - 1) {
            futilityIntersectionsBefore = futilityIntersections(_, k);
        }
        
        // Determine arm-level rejections and futility
        for (int j = 0; j < gMax; j++) {
            LogicalVector isInStage = indices(_, j);
            LogicalVector rejectedIntersectionInStage = rejectedIntersectionsBefore[isInStage];
            rejected(j, k) = Rcpp::as<bool>(all(na_omit(rejectedIntersectionInStage)));
            
            if (k < kMax - 1) {
                LogicalVector futilityIntersectionInStage = futilityIntersectionsBefore[isInStage];
                futility(j, k) = Rcpp::as<bool>(any(na_omit(futilityIntersectionInStage)));
            }
        }
        
        // Determine success stopping
        LogicalVector selectedArmsThisStage = selectedArms(_, k);
        LogicalVector rejectedThisStage = rejected(_, k);
        
        if (successCriterion == "all") {
            LogicalVector rejectedThisStageThisArm = rejectedThisStage[selectedArmsThisStage];
            successStop[k] = Rcpp::as<bool>(all(rejectedThisStageThisArm));
        } else {
            successStop[k] = Rcpp::as<bool>(any(rejectedThisStage));
        }
        
        // Determine futility stopping for intermediate stages
        if (k < kMax - 1) {
            LogicalVector futilityThisStage = futility(_, k);
            LogicalVector futilityThisStageThisArm = futilityThisStage[selectedArmsThisStage];
            futilityStop[k] = Rcpp::as<bool>(all(futilityThisStageThisArm));
            
            LogicalVector selectedArmsNextStage = selectedArms(_, k + 1);
            bool noArmInNextStage = Rcpp::as<bool>(all(na_omit(!selectedArmsNextStage)));
            if (noArmInNextStage) {
                futilityStop[k] = true;
            }
        }
    }
    
    return List::create(
        _["separatePValues"] = separatePValues,
        _["adjustedStageWisePValues"] = adjustedStageWisePValues,
        _["overallAdjustedTestStatistics"] = overallAdjustedTestStatistics,
        _["rejected"] = rejected,
        _["rejectedIntersections"] = rejectedIntersections,
        _["selectedArms"] = selectedArms,
        _["successStop"] = successStop,
        _["futilityStop"] = futilityStop
    );
}

// Select Treatment Arms
//
// Selects treatment arms based on effect vector and selection criteria
//
// @param effectVector Numeric vector of effect estimates
// @param typeOfSelection String specifying selection type: "all", "best", "rBest", "epsilon", "userDefined"
// @param epsilonValue Epsilon value for epsilon selection
// @param rValue R value for rBest selection
// @param threshold Threshold below which arms are not selected
// @param survival Whether this is for survival endpoint (if TRUE, do not append control arm)
//
// [[Rcpp::export(name = ".selectTreatmentArmsCpp")]]
LogicalVector selectTreatmentArms(NumericVector effectVector,
                                  std::string typeOfSelection,
                                  double epsilonValue = NA_REAL,
                                  int rValue = NA_INTEGER,
                                  double threshold = NA_REAL,
                                  bool survival = false) {
    
    int gMax = effectVector.size();
    LogicalVector selectedArms(gMax, false);
    
    // Convert typeOfSelection to lowercase for case-insensitive comparison
    std::string typeOfSelectionLower = typeOfSelection;
    std::transform(typeOfSelectionLower.begin(), typeOfSelectionLower.end(), 
                   typeOfSelectionLower.begin(), ::tolower);
    
    if (typeOfSelectionLower == "all") {
        std::fill(selectedArms.begin(), selectedArms.end(), true);
    } else if (typeOfSelectionLower == "best") {
        // Find index of maximum effect (excluding NA values)
        LogicalVector is_not_na = !is_na(effectVector);
        IntegerVector which_is_not_na = which(is_not_na);
        NumericVector availableEffectVector = effectVector[which_is_not_na];
        if (availableEffectVector.size() > 0) {
            auto maxIndex = std::max_element(availableEffectVector.begin(), availableEffectVector.end());
            selectedArms[which_is_not_na[std::distance(availableEffectVector.begin(), maxIndex)]] = true;
        }
    } else if (typeOfSelectionLower == "rbest") {
        if (R_IsNA(rValue) || rValue <= 0) {
            stop("rValue must be a positive integer for rBest selection");
        }
        rValue = std::min(rValue, gMax);
        LogicalVector is_not_na = !is_na(effectVector);
        IntegerVector which_is_not_na = which(is_not_na);
        NumericVector availableEffectVector = effectVector[which_is_not_na];
        if (availableEffectVector.size() > 0) {
            IntegerVector effectOrder = order(-availableEffectVector);
            int numToSelect = std::min(rValue, static_cast<int>(availableEffectVector.size()));
            effectOrder = effectOrder[seq(0, numToSelect - 1)];
            IntegerVector mapped_indices = which_is_not_na[effectOrder - 1];
            selectedArms[mapped_indices] = true;
        }
    } else if (typeOfSelectionLower == "epsilon") {
        if (R_IsNA(epsilonValue) || epsilonValue <= 0) {
            stop("positive epsilonValue must be specified for epsilon selection");
        }
        double maxEffect = max(na_omit(effectVector));
        NumericVector deltaToMax = maxEffect - effectVector;
        LogicalVector withinEpsilon = deltaToMax <= epsilonValue;
        selectedArms = withinEpsilon;
        // Exclude NAs
        selectedArms[which(is_na(effectVector))] = false;
    } else {
        stop("Invalid typeOfSelection. Must be 'all', 'best', 'rBest', or 'epsilon'");
    }
    
    // Apply threshold: exclude arms below threshold
    if (!R_IsNA(threshold)) {
        LogicalVector effectBelowThreshold = is_na(effectVector) | (effectVector <= threshold);
        selectedArms[effectBelowThreshold] = false;
    }
    
    // If not survival, append control arm (TRUE) which is always selected.
    if (!survival) {
        selectedArms.push_back(true);
    }
    
    return selectedArms;
}
