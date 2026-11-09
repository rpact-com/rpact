// |
// |  rpact: Confirmatory Adaptive Clinical Trial Design and Analysis
// |  This file is part of the rpact R package.
// |
// |  RPACT — Statistical Tools for Drug Development
// |
// |  Authors: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
// |  SPDX-FileCopyrightText: RPACT GmbH, Germany <info@rpact.com>
// |  SPDX-License-Identifier: LGPL-3.0-only
// |
// |  Licensed under the GNU Lesser General Public License, version 3.
// |  License text: https://www.gnu.org/licenses/lgpl-3.0.html
// |
// |  Package documentation: https://www.rpact.org
// |  Resources and customer portal: https://connect.rpact.app
// |  Consulting and services: https://www.rpact.com
// |  Contact: info@rpact.com
// |

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
