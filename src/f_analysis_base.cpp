#include <Rcpp.h>
#include <cmath>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "rpact_types.h"

using namespace Rcpp;

// Get Weights Fisher
//
// Returns the weights for Fisher's combination test statistic
//
// @param design Trial design R6 object
NumericVector getWeightsFisher(const Environment& design) {
   
    // Get kMax from design
    int kMax = Rcpp::as<int>(design.get("kMax"));
    
    if (kMax == 1) {
        return NumericVector::create(1.0);
    }
    
    NumericVector informationRates = design.get("informationRates");
    NumericVector weights(kMax);
    weights[0] = 1.0;
    
    for (int i = 1; i < kMax; i++) {
        double numerator = informationRates[i] - informationRates[i - 1];
        double denominator = informationRates[0];
        weights[i] = sqrt(numerator / denominator);
    }
    
    return weights;
}

// Get Weights Inverse Normal
//
// Returns the weights for inverse normal statistic
//
// @param design Trial design R6 object
NumericVector getWeightsInverseNormal(const Environment& design) {
   
    // Get kMax from design
    int kMax = Rcpp::as<int>(design.get("kMax"));
    
    if (kMax == 1) {
        return NumericVector::create(1.0);
    }
    
    NumericVector informationRates = design.get("informationRates");
    NumericVector weights(kMax);
    weights[0] = sqrt(informationRates[0]);
    for (int i = 1; i < kMax; i++) {
        double difference = informationRates[i] - informationRates[i - 1];
        weights[i] = sqrt(difference);
    }
    
    return weights;
}
