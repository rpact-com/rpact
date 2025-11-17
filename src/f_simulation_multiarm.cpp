#include <R_ext/Applic.h>
#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// Forward declaration of the integration wrapper function
void dunnetIntegrand1Wrapper(double *x, int n, void *ex);

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
    
    // Integration function
    double integrate() const {
        // Pack parameters into ex array
        // We need to pass 'this' pointer through the void* ex parameter
        void *ex = (void*) this;
        
        double bound = 0.0;  // is ignored for inf = 2
        int inf = 2;       // corresponds to (-Inf, +Inf)
        double epsabs = 1e-4;
        double epsrel = 1e-4;
        double result = 0.0;
        double abserr = 0.0;
        int neval = 0;
        int ier = 0;
        int limit = 100;
        int lenw = 400;
        int last = 0;
        int iwork[100];
        double work[400];
        
        Rdqagi(dunnetIntegrand1Wrapper, ex, &bound, &inf, &epsabs, &epsrel, &result, 
               &abserr, &neval, &ier, &limit, &lenw, &last, iwork, work);
        
        if (ier != 0) {
            Rcpp::warning("Integration error code: %d", ier);
        }
        
        return result;
    }
};

// Wrapper function for Rdqagi - must be a C-style function
void dunnetIntegrand1Wrapper(double *x, int n, void *ex) {
    const DunnettIntegrand1 *integrand = static_cast<const DunnettIntegrand1*>(ex);
    integrand->evaluateVector(x, n);
}

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
