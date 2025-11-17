#include <R_ext/Applic.h>
#include <Rcpp.h>
#include <cmath>

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


class DunnettIntegrand1 {
private:
    double criticalValue;
    double informationAtInterim;
    NumericMatrix signedTestStatistics;
    NumericVector frac;
    LogicalVector indicesRow;
    int gMax;
    
public:
    // Constructor
    DunnettIntegrand1(double criticalValue_,
                     double informationAtInterim_,
                     NumericMatrix signedTestStatistics_,
                     NumericVector frac_,
                     LogicalVector indicesRow_)
        : criticalValue(criticalValue_),
          informationAtInterim(informationAtInterim_),
          signedTestStatistics(signedTestStatistics_),
          frac(frac_),
          indicesRow(indicesRow_) {
        gMax = signedTestStatistics.nrow();
    }
    
    // Function to evaluate the integrand at a single point
    double evaluate(double x) const {
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
    
    // Vectorized evaluation for integration
    void evaluateVector(double *x, int n) const {
        for (int i = 0; i < n; i++) {
            x[i] = evaluate(x[i]);
        }
    }
    
    // Static wrapper function for Rdqagi - can be used with C-style function pointers
    static void wrapperFunction(double *x, int n, void *ex) {
        const DunnettIntegrand1 *integrand = static_cast<const DunnettIntegrand1*>(ex);
        integrand->evaluateVector(x, n);
    }
    
    // Integration function
    double integrate() const {
        return integrateInfinite(this);
    }
};

// [[Rcpp::export(name = ".dunnetIntegrand1IntCpp")]]
double dunnetIntegrand1Int(double criticalValue,
                        double informationAtInterim,
                        NumericMatrix signedTestStatistics,
                        NumericVector frac,
                        LogicalVector indicesRow) {
    
    DunnettIntegrand1 integrand(criticalValue, informationAtInterim, 
                               signedTestStatistics, frac, indicesRow);
    
    // Compute the integral
    double integralValue = integrand.integrate();    
    return integralValue;
}

// Separate export for evaluating the function at specific points
// [[Rcpp::export(name = ".dunnetIntegrand1EvaluateCpp")]]
NumericVector dunnetIntegrand1Evaluate(NumericVector x,
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
