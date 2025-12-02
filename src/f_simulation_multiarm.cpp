#include <R_ext/Applic.h>
#include <Rcpp.h>
#include <cmath>

#include "f_utilities.h"

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
