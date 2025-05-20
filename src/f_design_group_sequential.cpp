/**
 *
 * -- Group sequential design --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
 * Licensed under "GNU Lesser General Public License" version 3
 * License text can be found here: https://www.r-project.org/Licenses/LGPL-3
 *
 * RPACT company website: https://www.rpact.com
 * rpact package website: https://www.rpact.org
 *
 * Contact us for information about our services: info@rpact.com
 *
 * File version: $Revision: 8673 $
 * Last changed: $Date: 2025-04-10 10:45:56 +0200 (Do, 10 Apr 2025) $
 * Last changed by: $Author: wassmer $
 *
 */
#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

#include "f_utilities.h"
#include "f_simulation_survival_utilities.h"

using namespace Rcpp;

const int C_MAX_NUMBER_OF_ITERATIONS = 100;
const int C_UPPER_BOUNDS_DEFAULT = 8;
const int C_CONST_NEWTON_COTES_2 = 15; 
const int C_CONST_NEWTON_COTES_4 = 8; 
const int C_NEWTON_COTES_MULTIPLIER = 6;
const int C_NUMBER_OF_GRID_POINTS_ONE_SIDED = C_CONST_NEWTON_COTES_2 * C_NEWTON_COTES_MULTIPLIER + 1;
const int C_NUMBER_OF_GRID_POINTS_TWO_SIDED = C_CONST_NEWTON_COTES_4 * C_NEWTON_COTES_MULTIPLIER + 1;
const NumericVector C_NEWTON_COTES_VEC_4 = NumericVector::create(14, 32, 12, 32);
const NumericVector C_NEWTON_COTES_VEC_5 = NumericVector::create(38, 75, 50, 50, 75);
const NumericVector C_NEWTON_COTES_VEC_6 = NumericVector::create(82, 216, 27, 272, 27, 216);
const NumericVector C_NEWTON_COTES_VEC_7 = NumericVector::create(1502, 3577, 1323, 2989, 2989, 1323, 3577);
const double C_FUTILITY_BOUNDS_DEFAULT = -6;
const double C_GAUSS_LAGUERRE_UPPER_BOUND = -3.0;
const int C_GAUSS_LAGUERRE_N_POINTS = 10;
const NumericVector C_GAUSS_LAGUERRE_POINTS_VEC_10 = NumericVector::create(
	0.137793470540492, 0.729454549503169, 1.80834290174032, 3.4014336978549,
	5.5524961400638, 8.33015274676449, 11.8437858379001, 16.2792578313781,
	21.9965858119808, 29.9206970122739
);
const NumericVector C_GAUSS_LAGUERRE_WEIGHTS_VEC_10 = NumericVector::create(
	0.354009738606996, 0.831902301043581, 1.33028856174932, 1.86306390311114,
	2.45025555808301, 3.12276415513514, 3.93415269556146, 4.99241487219295,
	6.57220248513068, 9.78469584037451
);
const String C_TYPE_OF_DESIGN_AS_USER = "asUser";
const String C_TYPE_OF_DESIGN_BS_USER = "bsUser";
const String C_TYPE_OF_DESIGN_AS_P = "asP";
const String C_TYPE_OF_DESIGN_BS_P = "bsP";
const String C_TYPE_OF_DESIGN_AS_OF = "asOF";
const String C_TYPE_OF_DESIGN_BS_OF = "bsOF";
const String C_TYPE_OF_DESIGN_AS_KD = "asKD";
const String C_TYPE_OF_DESIGN_BS_KD = "bsKD";
const String C_TYPE_OF_DESIGN_AS_HSD = "asHSD";
const String C_TYPE_OF_DESIGN_BS_HSD = "bsHSD";
const String C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY = "noEarlyEfficacy";

double dnorm2(const double x, const double mean, const double stDev) {
	static const double inv_sqrt_2pi = 0.3989422804014327;
	double a = (x - mean) / stDev;

	return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

double getDensityValue(double x, int k, NumericVector informationRates,
		NumericVector epsilonVec, NumericVector x2, NumericVector dn2, int n) {
	try {
		k--;
		double part1 = sqrt((double) informationRates[k - 1] / (double) epsilonVec[k - 1]);
		double sqrtInfRates1 = sqrt((double) informationRates[k - 1]);
		double sqrtInfRates2 = sqrt((double) informationRates[k - 2]);

		const double mean = 0;
		const double stDev = 1;

		double prod1 = x * sqrtInfRates1;
		double divisor = sqrt((double) epsilonVec[k - 1]);
		double resultValue = 0;
		for (int i = 0; i < n; i++) {
			double dnormValue = dnorm2((prod1 - (x2[i] * sqrtInfRates2)) / divisor,
					mean, stDev);
			double prod = part1 * dnormValue * dn2[i];
			resultValue += prod;
		}

		return resultValue;
	} catch (const std::exception& e) {
		throw Exception("Failed to get density value (x = %f, k = %i, n = %i): %s", x, k, n, e.what());
	}
}

NumericVector getDensityValues(NumericVector x, int k,
		NumericVector informationRates, NumericVector epsilonVec,
		NumericVector x2, NumericVector dn2) {
	try {
		int n = x.size();
		NumericVector results = NumericVector(n, NA_REAL);
		for (int i = 0; i < n; i++) {
			if (k == 2) {
				results[i] = dnorm2((double) x[i], 0.0, 1.0);
			} else {
				results[i] = getDensityValue((double) x[i], k, informationRates, epsilonVec, x2, dn2, n);
			}
		}
		return results;
	} catch (const std::exception& e) {
		throw Exception("Failed to get density values (k = %i): %s", k, e.what());
	}
}

NumericVector getW(double dx, int constNewtonCotes) {
	try {
		NumericVector vec;
		double x;
		if (C_NEWTON_COTES_MULTIPLIER == 4) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_4, dx / 90.0);
			vec = 4 * rep(vec, constNewtonCotes);
			x = 28.0 * dx / 90.0;
		}
		else if (C_NEWTON_COTES_MULTIPLIER == 5) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_5, dx / 288.0);
			vec = 5 * rep(vec, constNewtonCotes);
			x = 95.0 * dx / 288.0;
		}
		else if (C_NEWTON_COTES_MULTIPLIER == 6) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_6, dx / 840.0);
			vec = 6 * rep(vec, constNewtonCotes);
			x = 246.0 * dx / 840.0;
		}
		else if (C_NEWTON_COTES_MULTIPLIER == 7) {
		  vec = vectorMultiply(C_NEWTON_COTES_VEC_7, dx / 17280.0);
		  vec = 7 * rep(vec, constNewtonCotes);
		  x = 5257.0 * dx / 17280.0;
		}
		NumericVector result = NumericVector(vec.size() + 1, NA_REAL);
		result[0] = x;
		for (int i = 1; i < vec.size(); i++) {
			result[i] = vec[i];
		}
		result[result.size() - 1] = x;
		return result;
	} catch (const std::exception& e) {
		throw Exception("Failed to get W (dx = %f, constNewtonCotes = %i): %s",
			dx, constNewtonCotes, e.what());
	}
}

double getSeqValue(int paramIndex, int k,
		NumericVector dn, NumericVector x,
		NumericMatrix decisionMatrix,
		NumericVector informationRates, NumericVector epsilonVec) {
	try {
		int kIndex = k - 1;
		NumericVector vec = NumericVector(x.size(), NA_REAL);
		for (int i = 0; i < x.size(); i++) {
			vec[i] = (decisionMatrix(paramIndex, kIndex) * sqrt((double) informationRates[kIndex]) -
				x[i] * sqrt((double) informationRates[kIndex - 1])) / sqrt((double) epsilonVec[kIndex]);
		}
		vec = pnorm(as<NumericVector>(vec));
		return vectorProduct(vec, dn);
	} catch (const std::exception& e) {
		throw Exception("Failed to get sequence values (paramIndex = %i, k = %i): %s",
			paramIndex, k, e.what());
	}
}

double getDxValue(NumericMatrix decisionMatrix, int k, int numberOfGridPoints, int rowIndex) {
	try {
		return (decisionMatrix(rowIndex + 1, k - 2) - decisionMatrix(rowIndex, k - 2)) / (numberOfGridPoints - 1);
	} catch (const std::exception& e) {
		throw Exception("Failed to get dx value (k = %d, numberOfGridPoints = %d, rowIndex = %d): %s",
			k, numberOfGridPoints, rowIndex, e.what());
	}
}

NumericVector getXValues(NumericMatrix decisionMatrix, int k, int numberOfGridPoints, int rowIndex) {
	try {
		NumericVector x = rep(decisionMatrix(rowIndex, k - 2), numberOfGridPoints);
		double dx = getDxValue(decisionMatrix, k, numberOfGridPoints, rowIndex);
		for (int i = 0; i < x.size(); i++) {
			x[i] = x[i] + i * dx;
		}
		return x;
	} catch (const std::exception& e) {
		throw Exception("Failed to get x values (k = %d, numberOfGridPoints = %d, rowIndex = %d): %s",
			k, numberOfGridPoints, rowIndex, e.what());
	}
}

NumericVector getGaussLaguerreXValues(NumericMatrix decisionMatrix, int k, int rowIndex) {
	try {
		NumericVector gridPoints;
		if (C_GAUSS_LAGUERRE_N_POINTS == 10) {
			gridPoints = C_GAUSS_LAGUERRE_POINTS_VEC_10;
		}
		else {
			throw Exception("Gauss Laguerre points not available for %d points", C_GAUSS_LAGUERRE_N_POINTS);
		}
		// This assumes that the rowIndex is pointing to the lower bounds.
		NumericVector x = rep(decisionMatrix(rowIndex, k - 2), gridPoints.size());
		for (int i = 0; i < x.size(); i++) {
			x[i] = x[i] - gridPoints[i];
		}
		return x;
	} catch (const std::exception& e) {
		throw Exception("Failed to get Gauss Laguerre x values (k = %d, rowIndex = %d): %s",
			k, rowIndex, e.what());
	}
}

NumericVector getGaussLaguerreW() {
	NumericVector w;
	if (C_GAUSS_LAGUERRE_N_POINTS == 10) {
		w = C_GAUSS_LAGUERRE_WEIGHTS_VEC_10;
	}
	else {
		throw Exception("Gauss Laguerre weights not available for %d points", C_GAUSS_LAGUERRE_N_POINTS);
	}
	return w;
}

NumericVector getAllXValues(NumericMatrix decisionMatrix, int k, int numberOfGridPoints, int rowIndex) {
	try {
		NumericVector x1 = getXValues(decisionMatrix, k, numberOfGridPoints, rowIndex);
		NumericVector x2 = getGaussLaguerreXValues(decisionMatrix, k, rowIndex);
		NumericVector x = concat(x1, x2);
		return x;
	} catch (const std::exception& e) {
		throw Exception("Failed to get all x values (k = %d, numberOfGridPoints = %d, rowIndex = %d): %s",
			k, numberOfGridPoints, rowIndex, e.what());
	}
}

NumericVector getAllW(double dx, int constNewtonCotes) {
	try {
		NumericVector w1 = getW(dx, constNewtonCotes);
		NumericVector w2 = getGaussLaguerreW();
		NumericVector w = concat(w1, w2);
		return w;
	} catch (const std::exception& e) {
		throw Exception("Failed to get all weights (dx = %f, constNewtonCotes = %d): %s", dx, constNewtonCotes,
		e.what());
	}
}

NumericVector getGroupSequentialProbabilitiesFast(
		NumericMatrix decisionMatrix,
		NumericVector informationRates) {

	// maximum number of stages
	int kMax = informationRates.size();

	// probability matrix output
	NumericVector probs(kMax);

	double decValue = decisionMatrix(0, 0);
	if (decValue > C_UPPER_BOUNDS_DEFAULT) {
		decValue = C_UPPER_BOUNDS_DEFAULT;
	}
	probs[0] = getNormalDistribution(decValue);
	if (kMax == 1) {
		return probs;
	}

	NumericVector epsilonVec = NumericVector(informationRates.size(), NA_REAL);
	epsilonVec[0] = informationRates[0];
	for (int i = 1; i < epsilonVec.size(); i++) {
		epsilonVec[i] = informationRates[i] - informationRates[i - 1];
	}

	NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));
	for (int i = 0; i < decMatrix.nrow(); i++) {
		for (int j = 0; j < decMatrix.ncol(); j++) {
			if (decMatrix(i, j) < C_FUTILITY_BOUNDS_DEFAULT) {
				decMatrix(i, j) = C_FUTILITY_BOUNDS_DEFAULT;
			}
		}
	}

	// density values in recursion
	NumericVector dn2 = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

	// grid points in recursion
	NumericVector x2  = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

	for (int k = 2; k <= kMax; k++) {
		double dx = getDxValue(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);

		NumericVector x = getXValues(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);
		NumericVector w = getW(dx, C_CONST_NEWTON_COTES_2);
		NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
		NumericVector dn = vectorMultiply(w, densityValues);

		double seq1 = getSeqValue(0, k, dn, x, decMatrix, informationRates, epsilonVec);

		x2 = x;
		dn2 = dn;
		probs[k - 1] = seq1;
	}

	return probs;
}

// [[Rcpp::export(name = ".getGroupSequentialProbabilitiesCpp")]]
NumericMatrix getGroupSequentialProbabilitiesCpp(
		NumericMatrix decisionMatrix,
		NumericVector informationRates) {
	try {
		NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));
	
		for (int i = 0; i < decMatrix.nrow(); i++) {
			for (int j = 0; j < decMatrix.ncol(); j++) {
				if (decMatrix(i, j) >= C_UPPER_BOUNDS_DEFAULT) {
					decMatrix(i, j) = C_UPPER_BOUNDS_DEFAULT;
				}
			}
		}

		// maximum number of stages
		int kMax = informationRates.size();

		// probability matrix output
		NumericMatrix probs(decMatrix.nrow() + 1, kMax);

		NumericVector pnormValues = pnorm(decMatrix(_, 0));
		for (int i = 0; i < pnormValues.size(); i++) {
			probs(i, 0) = pnormValues[i];
		}
		probs(probs.nrow() - 1, 0) = 1;
		if (kMax <= 1) {
			return probs;
		}

		NumericVector epsilonVec = NumericVector(informationRates.size(), NA_REAL);
		epsilonVec[0] = informationRates[0];
		for (int i = 1; i < epsilonVec.size(); i++) {
			epsilonVec[i] = informationRates[i] - informationRates[i - 1];
		}

		if (decMatrix.nrow() == 2) {

			int n_lower_bounds_below_thresh = 0;
			for (int j = 0; j < decMatrix.ncol(); j++) {
				// Check lower or equal to this bound, because R passes in with this lower
				// bound in the one sided case.
				if (decMatrix(0, j) <= C_FUTILITY_BOUNDS_DEFAULT) {
					n_lower_bounds_below_thresh++;
				}
			}
			bool one_sided_test = (n_lower_bounds_below_thresh == decMatrix.ncol());

			if (one_sided_test) {
				// One-sided test design.

				// Set lower bounds to constant value.
				// This is only used for the Newton-Cotes integration, and as upper bound for the 
				// Gauss-Laguerre integration.
				decMatrix(0, _) = rep(C_GAUSS_LAGUERRE_UPPER_BOUND, decMatrix.ncol());

				const int n_grid_points_total = C_NUMBER_OF_GRID_POINTS_ONE_SIDED + C_GAUSS_LAGUERRE_N_POINTS;

				// density values in recursion
				NumericVector dn2 = NumericVector(n_grid_points_total, NA_REAL);

				// grid points in recursion
				NumericVector x2  = NumericVector(n_grid_points_total, NA_REAL);

				for (int k = 2; k <= kMax; k++) {
					double dx = getDxValue(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);

					NumericVector x = getAllXValues(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);
					NumericVector w = getAllW(dx, C_CONST_NEWTON_COTES_2);
					NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
					NumericVector dn = vectorMultiply(w, densityValues);

					double seq1 = 0.0; // Because this is a one-sided test design.
					double seq2 = getSeqValue(1, k, dn, x, decMatrix, informationRates, epsilonVec);

					x2 = x;
					dn2 = dn;
					probs(0, k - 1) = seq1;
					probs(1, k - 1) = seq2;
					probs(2, k - 1) = probs(1, k - 2) - probs(0, k - 2);
				}
			} else {
				// Two-sided test design.

				for (int i = 0; i < decMatrix.nrow(); i++) {
					for (int j = 0; j < decMatrix.ncol(); j++) {
						if (decMatrix(i, j) <= C_FUTILITY_BOUNDS_DEFAULT) {
							decMatrix(i, j) = C_FUTILITY_BOUNDS_DEFAULT;
						}
					}
				}

				// density values in recursion
				NumericVector dn2 = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

				// grid points in recursion
				NumericVector x2  = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

				for (int k = 2; k <= kMax; k++) {
					double dx = getDxValue(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);

					NumericVector x = getXValues(decMatrix, k, C_NUMBER_OF_GRID_POINTS_ONE_SIDED, 0);
					NumericVector w = getW(dx, C_CONST_NEWTON_COTES_2);
					NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
					NumericVector dn = vectorMultiply(w, densityValues);

					double seq1 = getSeqValue(0, k, dn, x, decMatrix, informationRates, epsilonVec);
					double seq2 = getSeqValue(1, k, dn, x, decMatrix, informationRates, epsilonVec);

					x2 = x;
					dn2 = dn;
					probs(0, k - 1) = seq1;
					probs(1, k - 1) = seq2;
					probs(2, k - 1) = probs(1, k - 2) - probs(0, k - 2);
				}
			}
		}
		else if (decMatrix.nrow() == 4) {

			for (int i = 0; i < decMatrix.nrow(); i++) {
				for (int j = 0; j < decMatrix.ncol(); j++) {
					if (decMatrix(i, j) <= -C_UPPER_BOUNDS_DEFAULT) {
						decMatrix(i, j) = -C_UPPER_BOUNDS_DEFAULT;
					}
				}
			}

			// density values in recursion
			NumericVector dn2 = NumericVector(2 * C_NUMBER_OF_GRID_POINTS_TWO_SIDED, NA_REAL);

			// grid points in recursion
			NumericVector x2  = NumericVector(2 * C_NUMBER_OF_GRID_POINTS_TWO_SIDED, NA_REAL);

			for (int k = 2; k <= kMax; k++) {
				double dx0 = getDxValue(decMatrix, k, C_NUMBER_OF_GRID_POINTS_TWO_SIDED, 0);
				double dx1 = getDxValue(decMatrix, k, C_NUMBER_OF_GRID_POINTS_TWO_SIDED, 2);

				NumericVector x0 = getXValues(decMatrix, k, C_NUMBER_OF_GRID_POINTS_TWO_SIDED, 0);
				NumericVector x1 = getXValues(decMatrix, k, C_NUMBER_OF_GRID_POINTS_TWO_SIDED, 2);
				NumericVector x = concat(x0, x1);

				NumericVector w0 = getW(dx0, C_CONST_NEWTON_COTES_4);
				NumericVector w1 = getW(dx1, C_CONST_NEWTON_COTES_4);
				NumericVector w = concat(w0, w1);

				NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
				NumericVector dn = vectorMultiply(w, densityValues);

				double seq1 = getSeqValue(0, k, dn, x, decMatrix, informationRates, epsilonVec);
				double seq2 = getSeqValue(1, k, dn, x, decMatrix, informationRates, epsilonVec);
				double seq3 = getSeqValue(2, k, dn, x, decMatrix, informationRates, epsilonVec);
				double seq4 = getSeqValue(3, k, dn, x, decMatrix, informationRates, epsilonVec);

				x2 = x;
				dn2 = dn;
				probs(0, k - 1) = seq1;
				probs(1, k - 1) = seq2;
				probs(2, k - 1) = seq3;
				probs(3, k - 1) = seq4;
				probs(4, k - 1) = probs(3, k - 2) - probs(2, k - 2) + probs(1, k - 2) - probs(0, k - 2);
			}
		}

		return probs;
	} catch (const std::exception& e) {
		throw Exception("Failed to get group sequential probabilities: %s", e.what());
	}
}

// [[Rcpp::export(name = ".getDesignGroupSequentialPampallonaTsiatisCpp")]]
List getDesignGroupSequentialPampallonaTsiatisCpp(
        double tolerance, double beta, double alpha, double kMax, double deltaPT0,
        double deltaPT1, NumericVector informationRates, int sided,
        bool bindingFutility) {

    NumericVector futilityBounds(kMax);
    NumericVector rejectionBounds(kMax);
    NumericMatrix probs(5, kMax);
    int rows = sided == 1 ? 2 : 4;
    double size;
    double delst;
    double power;
    NumericMatrix helper(rows, kMax);
    NumericVector sqrtInformationRates = sqrt(informationRates);
    NumericVector deltaPT0KMaxInformationRates = pow(informationRates * kMax, deltaPT0 - 0.5);
    NumericVector deltaPT1KMaxInformationRates = pow(informationRates * kMax, deltaPT1 - 0.5);

    double pow1 = pow(kMax, deltaPT0 - 0.5);
    double pow2 = pow(kMax, deltaPT1 - 0.5);

    if (bindingFutility) {
        NumericMatrix decisionMatrix(rows, kMax);
        bizero([&](double c2m) {
        	bizero([&](double c1m) {
                delst = c2m * pow1 + c1m * pow2;
                futilityBounds = sqrtInformationRates * delst - deltaPT0KMaxInformationRates * c2m;
                rejectionBounds = deltaPT1KMaxInformationRates * c1m;
                for (int i = 0; i < futilityBounds.length(); i++) {
                    if (futilityBounds[i] > rejectionBounds[i]) {
                        futilityBounds[i] = rejectionBounds[i];
                    }
                    if (sided == 2 && futilityBounds[i] < 0) {
                        futilityBounds[i] = 0;
                    }
                }

                if (sided == 1) {
                    decisionMatrix.row(0) = futilityBounds;
                    decisionMatrix.row(1) = rejectionBounds;
                } else {
                    decisionMatrix.row(0) = -rejectionBounds;
                    decisionMatrix.row(1) = -futilityBounds;
                    decisionMatrix.row(2) = futilityBounds;
                    decisionMatrix.row(3) = rejectionBounds;
                }

                probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates);

                if (sided == 1) {
                    size = sum(probs.row(2) - probs.row(1));
                } else {
                    size = sum(probs.row(4) - probs.row(3) + probs.row(0));
                }

                return size - alpha;
            }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

            for (int i = 0; i < rows; i++) {
                helper.row(i) = sqrtInformationRates * delst;
            }

            NumericMatrix decisionMatrixH1 = matrixSub(decisionMatrix, helper);
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrixH1, informationRates);

            if (sided == 1) {
                power = sum(probs.row(2) - probs.row(1));
            } else {
                power = sum(probs.row(4) - probs.row(3) + probs.row(0));
            }

            return 1.0 - beta - power;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    } else { // non-binding
        double c1m = 0;
        bizero([&](double x) {
        	c1m = x;
            rejectionBounds = deltaPT1KMaxInformationRates * c1m;
            NumericMatrix decisionMatrix(2, kMax);

            if (sided == 1) {
                decisionMatrix.row(0) = rep(-6, kMax);
            } else {
                decisionMatrix.row(0) = -rejectionBounds;
            }

            decisionMatrix.row(1) = rejectionBounds;
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates);
            size = sum(probs.row(2) - probs.row(1));
            if (sided != 1) {
            	size += sum(probs.row(0));
            }
            return size - alpha;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

        rejectionBounds = deltaPT1KMaxInformationRates * c1m;
        bizero([&](double c2m) {
            delst = c2m * pow1 + c1m * pow2;
            futilityBounds = sqrtInformationRates * delst - deltaPT0KMaxInformationRates * c2m;
            for (int i = 0; i < futilityBounds.length(); i++) {
                if (futilityBounds[i] > rejectionBounds[i]) {
                    futilityBounds[i] = rejectionBounds[i];
                }
            }
            NumericMatrix decisionMatrix(rows,kMax);

            if (sided == 1) {
                decisionMatrix.row(0) = futilityBounds;
                decisionMatrix.row(1) = rejectionBounds;
            } else {
                for (int i = 0; i < futilityBounds.length(); i++) {
                    if (futilityBounds[i] < 0) {
                        futilityBounds[i] = 0;
                    }
                }
                decisionMatrix.row(0) = -rejectionBounds;
                decisionMatrix.row(1) = -futilityBounds;
                decisionMatrix.row(2) = futilityBounds;
                decisionMatrix.row(3) = rejectionBounds;
            }
            for (int i = 0; i < helper.nrow();i++) {
                helper.row(i) = sqrtInformationRates * delst;
            }

            NumericMatrix decisionMatrixH1 = matrixSub(decisionMatrix, helper);
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrixH1, informationRates);
            if (sided == 1) {
            	power = sum(probs.row(2) - probs.row(1));
            } else {
            	power = sum(probs.row(4) + probs.row(0) - probs.row(3));
            }
            return 1.0 - beta - power;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    }
    return List::create(
    	_["futilityBounds"] = futilityBounds,
		_["criticalValues"] = rejectionBounds,
		_["probs"] = probs
	);
}

NumericMatrix getDecisionMatrixOneSided(
		NumericVector criticalValues,
		NumericVector futilityBounds,
		bool bindingFutility) {

	int kMax = criticalValues.length();
	NumericMatrix decisionMatrix(2, kMax);
	if (bindingFutility) {
		// add C_FUTILITY_BOUNDS_DEFAULT at the end of the vector, after its current last element
		NumericVector futilityBoundsTemp = Rcpp::clone(futilityBounds);
		if (futilityBoundsTemp.length() < kMax) {
			futilityBoundsTemp.push_back(C_FUTILITY_BOUNDS_DEFAULT);
		}
		decisionMatrix(0, _) = futilityBoundsTemp;
		decisionMatrix(1, _) = criticalValues;
	} else {
		decisionMatrix(0, _) = rep(C_FUTILITY_BOUNDS_DEFAULT, kMax);
		decisionMatrix(1, _) = criticalValues;
	}
	return decisionMatrix;
}

NumericMatrix getDecisionMatrixTwoSided(NumericVector criticalValues) {
	NumericMatrix decisionMatrix(2, criticalValues.length());
    decisionMatrix(0, _) = -criticalValues;
    decisionMatrix(1, _) = criticalValues;
	return decisionMatrix;
}

NumericMatrix getDecisionMatrixSubset(NumericMatrix decisionMatrix, int k) {
	NumericMatrix decisionMatrixSubset(decisionMatrix.nrow(), k);
    for (int i = 0; i < k; i++) {
    	decisionMatrixSubset(_, i) = decisionMatrix(_, i);
    }
	return decisionMatrixSubset;
}

NumericMatrix getDecisionMatrix(
		NumericVector criticalValues, NumericVector futilityBounds,
		bool bindingFutility, int sided, int k = -1) {
	NumericMatrix decisionMatrix;
	if (sided == 1) {
		decisionMatrix = getDecisionMatrixOneSided(criticalValues, futilityBounds, bindingFutility);
	} else {
		decisionMatrix = getDecisionMatrixTwoSided(criticalValues);
	}
	if (k < 0) {
		return decisionMatrix;
	}
	return getDecisionMatrixSubset(decisionMatrix, k);
}

double getZeroApproximation(NumericMatrix probs, double alpha, int sided) {
	if (sided == 1) {
		return sum(probs(2, _) - probs(1, _)) - alpha;
	}

	return sum(probs(2, _) - probs(1, _) + probs(0, _)) - alpha;
}

double getSpendingValueCpp(double alpha, double x, double sided, String typeOfDesign, double gamma) {
    
    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_P || typeOfDesign == C_TYPE_OF_DESIGN_BS_P) {
        return alpha * log(1 + (exp(1) - 1) * x);
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_OF) {
        return 2 * sided * (getOneMinusPNorm(getOneMinusQNorm(alpha / (2 * sided)) / sqrt(x)));
    }
    
    if (typeOfDesign == C_TYPE_OF_DESIGN_BS_OF) {
        return 2 * (getOneMinusPNorm(getOneMinusQNorm(alpha / 2) / sqrt(x)));
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_KD || typeOfDesign == C_TYPE_OF_DESIGN_BS_KD) {
        return alpha * pow(x, gamma);
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_HSD || typeOfDesign == C_TYPE_OF_DESIGN_BS_HSD) {
        if (gamma == 0)	{
        	return alpha * x;
        }
        return alpha * (1 - exp(-gamma * x)) / (1 - exp(-gamma));
    }

    return NA_REAL;
}

double getCriticalValue(
		int k,
		NumericVector criticalValues,
		NumericVector userAlphaSpending,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {

	double alphaSpendingValue;
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER || typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
		alphaSpendingValue = userAlphaSpending[k - 1];
	} else {
		alphaSpendingValue = getSpendingValueCpp(alpha, (double) informationRates[k - 1], sided, typeOfDesign, gammaA);
	}

	if (k == 1) {
		return(getOneMinusQNorm(alphaSpendingValue / sided));
	}

	double criticalValue = NA_REAL;
	NumericVector criticalValuesTemp = Rcpp::clone(criticalValues);
    bisection2([&](double scale) {
    	criticalValue = scale;
    	criticalValuesTemp[k - 1] = criticalValue;
		NumericMatrix decisionMatrix = getDecisionMatrix(
			criticalValuesTemp, futilityBounds,
			bindingFutility, sided, k);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix, rangeVector(informationRates, 0, k - 1));
		return getZeroApproximation(probs, alphaSpendingValue, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

    return criticalValue;
}

NumericVector getDesignGroupSequentialAlphaSpending(
		int kMax,
		NumericVector userAlphaSpending,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {

	NumericVector criticalValues = NumericVector(kMax, NA_REAL);
    for (int k = 1; k <= kMax; k++) {
    	criticalValues[k - 1] = getCriticalValue(
			k,
			criticalValues,
			userAlphaSpending,
			alpha,
			gammaA,
			typeOfDesign,
			sided,
			informationRates,
			bindingFutility,
			futilityBounds,
			tolerance);
    }
    return criticalValues;
}

// [[Rcpp::export(name = ".getDesignGroupSequentialUserDefinedAlphaSpendingCpp")]]
NumericVector getDesignGroupSequentialUserDefinedAlphaSpendingCpp(
		int kMax,
		NumericVector userAlphaSpending,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialAlphaSpending(
		kMax,
		userAlphaSpending,
		NA_REAL,
		NA_REAL,
		C_TYPE_OF_DESIGN_AS_USER,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance);
}

// [[Rcpp::export(name = ".getDesignGroupSequentialAlphaSpendingCpp")]]
NumericVector getDesignGroupSequentialAlphaSpendingCpp(
		int kMax,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialAlphaSpending(
		kMax,
		NumericVector(0),
		alpha,
		gammaA,
		typeOfDesign,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance);
}

// [[Rcpp::export(name = ".getDesignGroupSequentialDeltaWTCpp")]]
NumericVector getDesignGroupSequentialDeltaWTCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance,
		double deltaWT) {

	NumericVector criticalValues(kMax);
    double scale = bizero([&](double scale) {
		for (int k = 0; k < kMax; k++) {
			criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
		}
		NumericMatrix decisionMatrix = getDecisionMatrix(
				criticalValues, futilityBounds,
				bindingFutility, sided);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix, informationRates);
		return getZeroApproximation(probs, alpha, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

	for (int k = 0; k < kMax; k++) {
		criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
	}

    return criticalValues;
}

// [[Rcpp::export(name = ".getDesignGroupSequentialPocockCpp")]]
NumericVector getDesignGroupSequentialPocockCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialDeltaWTCpp(
		kMax,
		alpha,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance,
		0.5);
}

// [[Rcpp::export(name = ".getDesignGroupSequentialOBrienAndFlemingCpp")]]
NumericVector getDesignGroupSequentialOBrienAndFlemingCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialDeltaWTCpp(
		kMax,
		alpha,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance,
		0);
}

NumericMatrix getDecisionMatrixForFutilityBounds(
		NumericVector informationRates,
		NumericVector criticalValues,
		NumericVector futilityBoundsTemp,
		double shift,
		double sided) {

	int kMax = criticalValues.length();
	if (futilityBoundsTemp.length() < kMax) {
		futilityBoundsTemp.push_back(C_FUTILITY_BOUNDS_DEFAULT);
	}

	if (sided == 1) {
		NumericMatrix decisionMatrix(2, kMax);
		decisionMatrix(0, _) = futilityBoundsTemp - sqrt(informationRates) * shift;
		decisionMatrix(1, _) = criticalValues - sqrt(informationRates) * shift;
		return decisionMatrix;
	}

	NumericMatrix decisionMatrix(4, kMax);
	decisionMatrix(0, _) = -criticalValues - sqrt(informationRates) * shift;
	decisionMatrix(1, _) = -futilityBoundsTemp - sqrt(informationRates) * shift;
	decisionMatrix(2, _) = futilityBoundsTemp - sqrt(informationRates) * shift;
	decisionMatrix(3, _) = criticalValues - sqrt(informationRates) * shift;
	return decisionMatrix;
}

double getFutilityBoundOneSided(int k,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	if (k == 1) {
		return getQNorm((double) betaSpendingValues[0]) + sqrt((double) informationRates[0]) * shift;
	}

	double futilityBound = NA_REAL;
	NumericVector futilityBoundsTemp = Rcpp::clone(futilityBounds);
	NumericVector probs;
	NumericMatrix decisionMatrix;
	bisection2([&](double scale) {
		futilityBound = scale;
		futilityBoundsTemp[k - 1] = futilityBound;
		decisionMatrix = getDecisionMatrixForFutilityBounds(
			informationRates, criticalValues, futilityBoundsTemp, shift, 1);
		probs = getGroupSequentialProbabilitiesFast(
			getDecisionMatrixSubset(decisionMatrix, k),
			rangeVector(informationRates, 0, k - 1));
		return (double) betaSpendingValues[k - 1] - sum(probs);
	}, -6.0, 5.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	return futilityBound;
}

NumericVector getFutilityBoundsOneSided(int kMax,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	NumericVector futilityBounds = NumericVector(kMax, NA_REAL);
    for (int k = 1; k <= kMax; k++) {
    	futilityBounds[k - 1] = getFutilityBoundOneSided(k,
			betaSpendingValues,
			informationRates,
			futilityBounds,
			criticalValues,
			shift,
			tolerance);
    }
	return futilityBounds;
}

NumericMatrix getProbabilitiesForFutilityBounds(
		NumericVector informationRates,
		NumericVector criticalValues,
		NumericVector futilityBounds,
		double shift,
		int k,
		double sided) {

	NumericMatrix decisionMatrix = getDecisionMatrixForFutilityBounds(
		informationRates, criticalValues, futilityBounds, shift, sided);
	return getGroupSequentialProbabilitiesCpp(
		getDecisionMatrixSubset(decisionMatrix, k),
		rangeVector(informationRates, 0, k - 1));
}

List getDesignGroupSequentialBetaSpendingOneSidedCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		String typeBetaSpending,
		double gammaA,
		double gammaB,
		double alpha,
		double beta
		) {

	double sided = 1.0;

	criticalValues = Rcpp::clone(criticalValues);

	if (typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        for (int k = 0; k < kMax - 1; k++) {
        	userAlphaSpending[k] = 0;
        	criticalValues[k] = getQNormThreshold();
        }
        userAlphaSpending[kMax - 1] = alpha;
		criticalValues[kMax - 1] = getOneMinusQNorm(alpha / sided);
    }

	NumericVector betaSpendingValues;
	if (typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
		betaSpendingValues = userBetaSpending;
	} else {
		betaSpendingValues = NumericVector(kMax, NA_REAL);
		for (int k = 0; k < kMax; k++) {
			betaSpendingValues[k] = getSpendingValueCpp(beta,
				(double) informationRates[k], sided, typeBetaSpending, gammaB);
		}
	}

    NumericVector futilityBounds;
    double shiftResult;
    if (!bindingFutility) {
		shiftResult = bizero([&](double shift) {
            futilityBounds = getFutilityBoundsOneSided(kMax,
				betaSpendingValues,
				informationRates,
				criticalValues,
				shift,
				tolerance);
            return (double) futilityBounds[kMax - 1] - (double) criticalValues[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    } else {
    	futilityBounds = NumericVector(kMax, NA_REAL);
    	shiftResult = bisection2([&](double shift) {
            for (int k = 1; k <= kMax; k++) {
                if (typeOfDesign != C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
                	criticalValues[k - 1] = getCriticalValue(
						k,
						criticalValues,
						userAlphaSpending,
						alpha,
						gammaA,
						typeOfDesign,
						sided,
						informationRates,
						bindingFutility,
						futilityBounds,
						tolerance);
                }

            	futilityBounds[k - 1] = getFutilityBoundOneSided(k,
        			betaSpendingValues,
        			informationRates,
        			futilityBounds,
					criticalValues,
        			shift,
        			tolerance);
			}
            return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    }

    NumericMatrix probs = getProbabilitiesForFutilityBounds(informationRates,
		criticalValues, futilityBounds, shiftResult, kMax, sided);
	NumericVector betaSpent = cumsum(probs(0, _));
	NumericVector power = cumsum(probs(2, _) - probs(1, _));

	futilityBounds = rangeVector(futilityBounds, 0, kMax - 2);

    return List::create(
    	_["futilityBounds"] = futilityBounds,
		_["criticalValues"] = criticalValues,
		_["betaSpent"] = betaSpent,
		_["power"] = power,
		_["shift"] = shiftResult
	);
}

int getFirstIndexOfValuLargerZero(NumericVector vec) {
	for (int i = 0; i < vec.size(); i++) {
		if (!R_IsNA((double) vec[i]) && vec[i] > 0) {
			return i;
		}
	}
	return -1;
}

// Add additional option betaAdjustment for group sequential design (default = FALSE)
NumericVector getAdjustedBetaSpendingValues(
		int kMax,
		int kMin,
		NumericVector betaSpendingValues,
		bool betaAdjustment) {
    if (kMin <= 0) {
    	return betaSpendingValues;
    }

	NumericVector betaSpendingValuesAdjusted = Rcpp::clone(betaSpendingValues);
	for (int k = 0; k < kMin; k++) {
		betaSpendingValuesAdjusted[k] = 0;
	}
	if (betaAdjustment) {
		for (int k = kMin - 1; k < kMax; k++) {
			betaSpendingValuesAdjusted[k] =
				(betaSpendingValues[k] - betaSpendingValues[kMin - 1]) /
				(betaSpendingValues[kMax - 1] - betaSpendingValues[kMin - 1]) *
				betaSpendingValues[kMax - 1];
		}
	}
	return betaSpendingValuesAdjusted;
}

double getFutilityBoundTwoSided(
		int k,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector futilityBoundsOneSided,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	if (k == 1) {
		double futilityBound = bizero([&](double u) {
			return getNormalDistribution(u - sqrt((double) informationRates[0]) * shift) -
					getNormalDistribution(-u - sqrt((double) informationRates[0]) * shift) -
					betaSpendingValues[0];
		}, -8.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
        if (futilityBound > criticalValues[0]) {
        	futilityBound = criticalValues[0];
        }
        if (futilityBoundsOneSided[0] < 0) {
            futilityBound = 0;
        }
        return futilityBound;
	}

	double futilityBound = NA_REAL;
	double futilityBoundOneSided = 1;
	if (k <= futilityBoundsOneSided.length()) {
		futilityBoundOneSided = futilityBoundsOneSided[k - 1];
	}
	NumericVector futilityBoundsTemp = Rcpp::clone(futilityBounds);
	NumericMatrix decisionMatrix;
	bizero([&](double scale) {
		futilityBound = scale;
        if (futilityBound > criticalValues[k - 1]) {
        	futilityBound = criticalValues[k - 1];
        }
        if (futilityBoundOneSided < 0) {
            futilityBound = 0;
        }
		futilityBoundsTemp[k - 1] = futilityBound;

		decisionMatrix = getDecisionMatrixForFutilityBounds(
			informationRates, criticalValues, futilityBoundsTemp, shift, 2);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix(_, Range(0, k - 1)),
			informationRates[Range(0, k - 1)]);
		double probsSum = sum(probs.row(2) - probs.row(1));
		return (double) betaSpendingValues[k - 1] - probsSum;
	}, -6.0, 5.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	return futilityBound;
}

NumericVector getFutilityBoundsTwoSided(
		int kMax,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector futilityBoundsOneSided,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	NumericVector futilityBounds = NumericVector(kMax, NA_REAL);
    for (int k = 1; k <= kMax; k++) {
    	futilityBounds[k - 1] = getFutilityBoundTwoSided(
			k,
			betaSpendingValues,
			informationRates,
			futilityBounds,
			futilityBoundsOneSided,
			criticalValues,
			shift,
			tolerance);
    }
	return futilityBounds;
}

double getCriticalValueTwoSided(
		int kMax,
		int k,
		NumericVector criticalValues,
		NumericVector userAlphaSpending,
		double alpha,
		double gammaA,
		String typeOfDesign,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {

	double sided = 2.0;
	double alphaSpendingValue;
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER || typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
		alphaSpendingValue = userAlphaSpending[k - 1];
	} else {
		alphaSpendingValue = getSpendingValueCpp(alpha, (double) informationRates[k - 1], sided, typeOfDesign, gammaA);
	}

	if (k == 1) {
		return(getOneMinusQNorm(alphaSpendingValue / sided));
	}

	double criticalValue = NA_REAL;
	NumericVector criticalValuesTemp = Rcpp::clone(criticalValues);
    bisection2([&](double scale) {
    	criticalValue = scale;
    	criticalValuesTemp[k - 1] = criticalValue;

		NumericMatrix decisionMatrix(4, futilityBounds.length());
		decisionMatrix(0, _) = -criticalValuesTemp;
		decisionMatrix(1, _) = -futilityBounds;
		decisionMatrix(2, _) = futilityBounds;
		decisionMatrix(3, _) = criticalValuesTemp;
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix(_, Range(0, k - 1)),
			informationRates[Range(0, k - 1)]);
		return sum(probs(4, _) - probs(3, _) + probs(0, _)) - alphaSpendingValue;
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

    return criticalValue;
}

List getDesignGroupSequentialBetaSpendingTwoSidedCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		String typeBetaSpending,
		double gammaA,
		double gammaB,
		double alpha,
		double beta,
		bool betaAdjustment,
		bool twoSidedPower
		) {

	double sided = 2;

	criticalValues = Rcpp::clone(criticalValues);

	if (typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        for (int k = 0; k < kMax - 1; k++) {
        	userAlphaSpending[k] = 0;
        	criticalValues[k] = getQNormThreshold();
        }
        userAlphaSpending[kMax - 1] = alpha;
		criticalValues[kMax - 1] = getOneMinusQNorm(alpha / sided);
    }

	// Check which of the futilityBounds are negative for the corresponding one-sided case.
	// For these stages, no two-sided futlityBounds are calculated.
	NumericVector futilityBoundsOneSided = getDesignGroupSequentialBetaSpendingOneSidedCpp(
		criticalValues,
		kMax,
		userAlphaSpending / 2.0,
		userBetaSpending,
		informationRates,
		bindingFutility,
		tolerance,
		typeOfDesign,
		typeBetaSpending,
		gammaA,
		gammaB,
		alpha / 2.0,
		beta
	)["futilityBounds"];

	NumericVector betaSpendingValues;
	if (typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
		betaSpendingValues = userBetaSpending;
	} else {
		betaSpendingValues = NumericVector(kMax, NA_REAL);
		for (int k = 0; k < kMax; k++) {
			betaSpendingValues[k] = getSpendingValueCpp(beta,
				(double) informationRates[k], sided, typeBetaSpending, gammaB);
		}
	}

	double kMin = getFirstIndexOfValuLargerZero(futilityBoundsOneSided);

	betaSpendingValues = getAdjustedBetaSpendingValues(
		kMax, kMin, betaSpendingValues, betaAdjustment);

    NumericVector futilityBounds;
    double shiftResult;
    if (!bindingFutility) {
		shiftResult = bisection2([&](double shift) {
			futilityBounds = getFutilityBoundsTwoSided(
				kMax,
				betaSpendingValues,
				informationRates,
				futilityBoundsOneSided,
				criticalValues,
				shift,
				tolerance);
            return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    } else {
    	futilityBounds = NumericVector(kMax, NA_REAL);
    	shiftResult = bisection2([&](double shift) {
            for (int k = 1; k <= kMax; k++) {
				criticalValues[k - 1] = getCriticalValueTwoSided(
					kMax,
					k,
					criticalValues,
					userAlphaSpending,
					alpha,
					gammaA,
					typeOfDesign,
					informationRates,
					bindingFutility,
					futilityBounds,
					tolerance);

            	futilityBounds[k - 1] = getFutilityBoundTwoSided(k,
        			betaSpendingValues,
        			informationRates,
        			futilityBounds,
					futilityBoundsOneSided,
					criticalValues,
        			shift,
        			tolerance);
			}
            return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    }

    NumericMatrix probs = getProbabilitiesForFutilityBounds(informationRates,
		criticalValues, futilityBounds, shiftResult, kMax, sided);
	NumericVector betaSpent = cumsum(probs(2, _) - probs(1, _));
	NumericVector power(kMax);
    if (twoSidedPower) {
    	power = (NumericVector) cumsum(probs(4, _) - probs(3, _) + probs(0, _));
    } else {
    	power = (NumericVector) cumsum(probs(4, _) - probs(3, _));
    }

	futilityBounds = rangeVector(futilityBounds, 0, kMax - 2);
	futilityBounds[futilityBounds <= 1e-05] = NA_REAL;

    return List::create(
    	_["futilityBounds"] = futilityBounds,
		_["criticalValues"] = criticalValues,
		_["betaSpent"] = betaSpent,
		_["power"] = power,
		_["shift"] = shiftResult
	);
}

// [[Rcpp::export(name = ".getDesignGroupSequentialBetaSpendingCpp")]]
List getDesignGroupSequentialBetaSpendingCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		String typeBetaSpending,
		double gammaA,
		double gammaB,
		double alpha,
		double beta,
		double sided,
		bool betaAdjustment,
		bool twoSidedPower
		) {
	if (sided == 1) {
		return getDesignGroupSequentialBetaSpendingOneSidedCpp(
			criticalValues,
			kMax,
			userAlphaSpending,
			userBetaSpending,
			informationRates,
			bindingFutility,
			tolerance,
			typeOfDesign,
			typeBetaSpending,
			gammaA,
			gammaB,
			alpha,
			beta
		);
	}

	return getDesignGroupSequentialBetaSpendingTwoSidedCpp(
		criticalValues,
		kMax,
		userAlphaSpending,
		userBetaSpending,
		informationRates,
		bindingFutility,
		tolerance,
		typeOfDesign,
		typeBetaSpending,
		gammaA,
		gammaB,
		alpha,
		beta,
		betaAdjustment,
		twoSidedPower
	);
}

// [[Rcpp::export(name = ".getDesignGroupSequentialUserDefinedBetaSpendingCpp")]]
List getDesignGroupSequentialUserDefinedBetaSpendingCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		double gammaA,
		double alpha,
		bool betaAdjustment,
		bool twoSidedPower
		) {
	String typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER;
	double gammaB = NA_REAL;
	double beta = NA_REAL;

	return getDesignGroupSequentialBetaSpendingCpp(
		criticalValues,
		kMax,
		userAlphaSpending,
		userBetaSpending,
		informationRates,
		bindingFutility,
		tolerance,
		typeOfDesign,
		typeBetaSpending,
		gammaA,
		gammaB,
		alpha,
		beta,
		sided,
		betaAdjustment,
		twoSidedPower
	);
}
