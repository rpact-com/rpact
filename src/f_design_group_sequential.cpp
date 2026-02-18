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

//' @title Normal density (fast internal)
//' @description Compute the Gaussian probability density function (PDF) for a single point using a minimal implementation (no R overhead).
//' @details
//' This helper is used inside the recursive integration for group sequential boundaries, where the
//' normal density is evaluated extremely often.
//' @param x Evaluation point(s) on the integration grid (stage-wise Z-scale).
//' @param mean Mean of the normal distribution.
//' @param stDev Standard deviation of the normal distribution.
//' @return A scalar double with the normal PDF value at `x` for mean `mean` and standard deviation `stDev`.
//' @keywords internal
//' @noRd
//'
double dnorm2(const double x, const double mean, const double stDev) {
	static const double inv_sqrt_2pi = 0.3989422804014327;
	double a = (x - mean) / stDev;

	return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

//' @title Recursive density update at a single grid point
//' @description Evaluate the stage-`k` recursive density \eqn{f_k(x)} at a single grid location.
//' @details
//' For `k=1`, the density is standard normal. For `k>1`, the density at stage `k` is obtained by
//' integrating the previous-stage density over the continuation region, multiplied by the conditional
//' normal density for the increment from information fraction \eqn{t_{k-1}} to \eqn{t_k}. The
//' implementation uses a discrete grid (`x2`) and the previously computed, quadrature-weighted density
//' values (`dn2`).
//' @param x Evaluation point(s) on the integration grid (stage-wise Z-scale).
//' @param k Stage index (1-based).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param epsilonVec Stage-wise information increments: `epsilonVec[1]=informationRates[1]`, `epsilonVec[k]=informationRates[k]-informationRates[k-1]`.
//' @param x2 Grid points from the previous stage used in the recursion.
//' @param dn2 Quadrature-weighted density values from the previous stage (`w * f_{k-1}(x2)`).
//' @param n Number of grid points (length of `x2` / `dn2`).
//' @return A scalar double: the approximated density value at `x` for stage `k`.
//' @keywords internal
//' @noRd
//'
double getDensityValue(double x, int k, NumericVector informationRates, NumericVector epsilonVec, NumericVector x2,
	NumericVector dn2, int n) {
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
			double dnormValue = dnorm2((prod1 - (x2[i] * sqrtInfRates2)) / divisor, mean, stDev);
			double prod = part1 * dnormValue * dn2[i];
			resultValue += prod;
		}

		return resultValue;
	} catch (const std::exception &e) {
		throw Exception("Failed to get density value (x = %f, k = %i, n = %i): %s", x, k, n, e.what());
	}
}

//' @title Recursive density update on a grid
//' @description Vectorized version of `getDensityValue()` for all grid points of a stage.
//' @details
//' This is performance-critical: it updates the whole density vector on the current grid using the
//' previous grid (`x2`) and weighted densities (`dn2`).
//' @param x Evaluation point(s) on the integration grid (stage-wise Z-scale).
//' @param k Stage index (1-based).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param epsilonVec Stage-wise information increments: `epsilonVec[1]=informationRates[1]`, `epsilonVec[k]=informationRates[k]-informationRates[k-1]`.
//' @param x2 Grid points from the previous stage used in the recursion.
//' @param dn2 Quadrature-weighted density values from the previous stage (`w * f_{k-1}(x2)`).
//' @return A numeric vector of densities for the supplied grid `x`.
//' @keywords internal
//' @noRd
//'
NumericVector getDensityValues(NumericVector x, int k, NumericVector informationRates, NumericVector epsilonVec,
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
	} catch (const std::exception &e) {
		throw Exception("Failed to get density values (k = %i): %s", k, e.what());
	}
}

//' @title Composite Newton–Cotes quadrature weights
//' @description Build the weight vector \eqn{w} used for the composite closed Newton–Cotes rule on an equidistant grid.
//' @details
//' The recursion for multivariate normal probabilities in group sequential designs is implemented via
//' sequential numerical integration over interim Z-statistics. The integral at each stage is
//' approximated on a grid with spacing `dx` and a composite Newton–Cotes rule. The returned vector
//' contains boundary weights at the first and last grid point (often denoted W in the literature /
//' implementation) and the interior weights for all grid points.
//' @param dx Grid spacing for the Newton–Cotes rule.
//' @param constNewtonCotes Number of Newton–Cotes panels (composite rule parameter).
//' @return A numeric vector of length `C_NEWTON_COTES_MULTIPLIER * constNewtonCotes + 1` containing quadrature weights aligned with the grid returned by `getXValues()`.
//' @keywords internal
//' @noRd
//'
NumericVector getW(double dx, int constNewtonCotes) {
	try {
		NumericVector vec;
		double x;
		if (C_NEWTON_COTES_MULTIPLIER == 4) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_4, dx / 90.0);
			vec = 4 * rep(vec, constNewtonCotes);
			x = 28.0 * dx / 90.0;
		} else if (C_NEWTON_COTES_MULTIPLIER == 5) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_5, dx / 288.0);
			vec = 5 * rep(vec, constNewtonCotes);
			x = 95.0 * dx / 288.0;
		} else if (C_NEWTON_COTES_MULTIPLIER == 6) {
			vec = vectorMultiply(C_NEWTON_COTES_VEC_6, dx / 840.0);
			vec = 6 * rep(vec, constNewtonCotes);
			x = 246.0 * dx / 840.0;
		} else if (C_NEWTON_COTES_MULTIPLIER == 7) {
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
	} catch (const std::exception &e) {
		throw Exception("Failed to get W (dx = %f, constNewtonCotes = %i): %s", dx, constNewtonCotes, e.what());
	}
}

//' @title Stage-wise sequential probability contribution
//' @description Compute one probability component (reject/accept/continue) for stage `k` by integrating the current density over a decision region.
//' @details
//' The decision regions are encoded in `decisionMatrix` (critical values and optional futility bounds).
//' The density values `dn` are already multiplied by quadrature weights (`w * f_k(x)`), so this
//' function typically reduces to summing over grid indices that belong to the relevant region.
//' @param paramIndex Index selecting the probability component/region to integrate (implementation-specific).
//' @param k Stage index (1-based).
//' @param dn Quadrature-weighted current-stage density values (`w * f_k(x)`) on the current grid.
//' @param x Evaluation point(s) on the integration grid (stage-wise Z-scale).
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param epsilonVec Stage-wise information increments: `epsilonVec[1]=informationRates[1]`, `epsilonVec[k]=informationRates[k]-informationRates[k-1]`.
//' @return A scalar double with the probability contribution for the requested component (`paramIndex`).
//' @keywords internal
//' @noRd
//'
double getSeqValue(int paramIndex, int k, NumericVector dn, NumericVector x, NumericMatrix decisionMatrix,
	NumericVector informationRates, NumericVector epsilonVec) {
	try {
		int kIndex = k - 1;
		NumericVector vec = NumericVector(x.size(), NA_REAL);
		for (int i = 0; i < x.size(); i++) {
			vec[i] = (decisionMatrix(paramIndex, kIndex) * sqrt((double) informationRates[kIndex])
				- x[i] * sqrt((double) informationRates[kIndex - 1])) / sqrt((double) epsilonVec[kIndex]);
		}
		vec = pnorm(as<NumericVector>(vec));
		return vectorProduct(vec, dn);
	} catch (const std::exception &e) {
		throw Exception("Failed to get sequence values (paramIndex = %i, k = %i): %s", paramIndex, k, e.what());
	}
}

//' @title Grid step width for a decision region
//' @description Compute the grid spacing `dx` for Newton–Cotes integration at stage `k` based on the decision region bounds.
//' @details
//' The integration range is derived from the relevant row in `decisionMatrix` and divided into
//' `numberOfGridPoints - 1` equal intervals.
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param k Stage index (1-based).
//' @param numberOfGridPoints Total number of grid points for the stage-wise integration grid.
//' @param rowIndex Row index in `decisionMatrix` selecting a particular decision region.
//' @return A scalar double containing the grid step size.
//' @keywords internal
//' @noRd
//'
double getDxValue(NumericMatrix decisionMatrix, int k, int numberOfGridPoints, int rowIndex) {
	try {
		return (decisionMatrix(rowIndex + 1, k - 2) - decisionMatrix(rowIndex, k - 2)) / (numberOfGridPoints - 1);
	} catch (const std::exception &e) {
		throw Exception("Failed to get dx value (k = %d, numberOfGridPoints = %d, rowIndex = %d): %s", k,
			numberOfGridPoints, rowIndex, e.what());
	}
}

//' @title Equidistant integration grid for a decision region
//' @description Construct the equidistant grid `x` used at stage `k` for Newton–Cotes integration.
//' @details
//' The grid starts at the lower bound taken from `decisionMatrix` and increases by `dx` up to the upper
//' bound. The bounds may be truncated to package-wide defaults to ensure numerical stability.
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param k Stage index (1-based).
//' @param numberOfGridPoints Total number of grid points for the stage-wise integration grid.
//' @param rowIndex Row index in `decisionMatrix` selecting a particular decision region.
//' @return A numeric vector of grid points.
//' @keywords internal
//' @noRd
//'
NumericVector getXValues(NumericMatrix decisionMatrix, int k, int numberOfGridPoints, int rowIndex) {
	try {
		NumericVector x = rep(decisionMatrix(rowIndex, k - 2), numberOfGridPoints);
		double dx = getDxValue(decisionMatrix, k, numberOfGridPoints, rowIndex);
		for (int i = 0; i < x.size(); i++) {
			x[i] = x[i] + i * dx;
		}
		return x;
	} catch (const std::exception &e) {
		throw Exception("Failed to get x values (k = %d, numberOfGridPoints = %d, rowIndex = %d): %s", k,
			numberOfGridPoints, rowIndex, e.what());
	}
}

//' @title Fast one-sided group sequential probabilities
//' @description Compute stage-wise crossing probabilities for a one-sided group sequential design using recursive density integration.
//' @details
//' This is a streamlined implementation that returns only the efficacy crossing probabilities across
//' stages. It sets up information increments (`epsilonVec`), grids and Newton–Cotes weights, then
//' iterates stages `k=2..kMax` updating the density (`dn2 -> dn`) and integrating over the rejection
//' region via `getSeqValue()`.
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @return A numeric vector of length `kMax` with cumulative probabilities up to each stage.
//' @keywords internal
//' @noRd
//'
NumericVector getGroupSequentialProbabilitiesFast(NumericMatrix decisionMatrix, NumericVector informationRates) {

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
	NumericVector x2 = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

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

//' @title Group sequential probabilities for multiple decision regions
//' @description Compute probabilities for efficacy and futility regions (and the remaining continuation probability) for a given decision matrix and information rates.
//' @details
//' The returned matrix contains one row per decision region plus an additional row for the remaining
//' probability mass. Internally, the function performs recursive density integration using Newton–Cotes
//' quadrature similarly to `getGroupSequentialProbabilitiesFast()`, but for potentially multiple rows
//' in `decisionMatrix`.
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @return A numeric matrix with probabilities by decision row and stage.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getGroupSequentialProbabilitiesCpp")]]
//'
NumericMatrix getGroupSequentialProbabilitiesCpp(NumericMatrix decisionMatrix, NumericVector informationRates) {
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
			NumericVector x2 = NumericVector(C_NUMBER_OF_GRID_POINTS_ONE_SIDED, NA_REAL);

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
		} else if (decMatrix.nrow() == 4) {

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
			NumericVector x2 = NumericVector(2 * C_NUMBER_OF_GRID_POINTS_TWO_SIDED, NA_REAL);

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
	} catch (const std::exception &e) {
		throw Exception("Failed to get group sequential probabilities: %s", e.what());
	}
}

//' @title Pampallona–Tsiatis design search
//' @description Search for critical values (and optionally futility bounds) for the Pampallona–Tsiatis family given error spending and information rates.
//' @details
//' Implements an iterative root-finding / optimization loop that adjusts boundary parameters so that
//' the computed group sequential probabilities match the requested type I error (`alpha`) and type II
//' error (`beta`) constraints at the specified information rates. Uses
//' `getGroupSequentialProbabilitiesCpp()` as the inner probability engine.
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param beta Target type II error (1 - power) at the design alternative.
//' @param alpha Target type I error level.
//' @param kMax Maximum number of stages (analyses).
//' @param deltaPT0 Lower endpoint of the Pampallona–Tsiatis delta search interval (or initial value).
//' @param deltaPT1 Upper endpoint of the Pampallona–Tsiatis delta search interval (or initial value).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param sided Number of sides for testing (1 or 2).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @return A list containing the computed critical values, futility bounds (if applicable), and diagnostic information from the iteration.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialPampallonaTsiatisCpp")]]
//'
List getDesignGroupSequentialPampallonaTsiatisCpp(double tolerance, double beta, double alpha, double kMax,
	double deltaPT0, double deltaPT1, NumericVector informationRates, int sided, bool bindingFutility) {

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
			NumericMatrix decisionMatrix(rows, kMax);

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
			for (int i = 0; i < helper.nrow(); i++) {
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
	return List::create(_["futilityBounds"] = futilityBounds, _["criticalValues"] = rejectionBounds, _["probs"] = probs);
}

//' @title Decision matrix for one-sided boundaries
//' @description Create the decision matrix encoding efficacy and (optional) futility bounds for a one-sided design.
//' @details
//' Rows correspond to different decision regions used in the recursive integration (e.g., rejection at
//' each stage, futility at each stage).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @return A numeric matrix used by the probability engine.
//' @keywords internal
//' @noRd
//'
NumericMatrix getDecisionMatrixOneSided(NumericVector criticalValues, NumericVector futilityBounds,
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

//' @title Decision matrix for two-sided boundaries
//' @description Create the decision matrix encoding symmetric two-sided critical values.
//' @details
//' The matrix encodes lower and upper bounds per stage for the two-sided stopping regions.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @return A numeric matrix used by the probability engine.
//' @keywords internal
//' @noRd

NumericMatrix getDecisionMatrixTwoSided(NumericVector criticalValues) {
	NumericMatrix decisionMatrix(2, criticalValues.length());
	decisionMatrix(0, _) = -criticalValues;
	decisionMatrix(1, _) = criticalValues;
	return decisionMatrix;
}

//' @title Decision matrix subset up to stage k
//' @description Extract the subset of a decision matrix that is required to compute probabilities up to stage `k`.
//' @details
//' This is used in iterative boundary search routines where probabilities are recomputed repeatedly for
//' increasing `k`.
//' @param decisionMatrix Matrix encoding decision region bounds per stage (critical values and optional futility bounds).
//' @param k Stage index (1-based).
//' @return A numeric matrix containing the first `k` stages.
//' @keywords internal
//' @noRd
//'
NumericMatrix getDecisionMatrixSubset(NumericMatrix decisionMatrix, int k) {
	NumericMatrix decisionMatrixSubset(decisionMatrix.nrow(), k);
	for (int i = 0; i < k; i++) {
		decisionMatrixSubset(_, i) = decisionMatrix(_, i);
	}
	return decisionMatrixSubset;
}

//' @title Unified decision matrix constructor
//' @description Dispatch to one- or two-sided decision matrix construction and optionally apply binding futility logic.
//' @details
//' Depending on `sided` and the presence of `futilityBounds`, the matrix encodes the correct stopping
//' regions for the probability recursion.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param sided Number of sides for testing (1 or 2).
//' @param k Stage index (1-based).
//' @return A numeric matrix used by the probability engine.
//' @keywords internal
//' @noRd
//'
NumericMatrix getDecisionMatrix(NumericVector criticalValues, NumericVector futilityBounds, bool bindingFutility,
	int sided, int k = -1) {
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

//' @title Initial root approximation for boundary search
//' @description Compute an initial approximation for a root-finding problem when calibrating boundaries to a target alpha level.
//' @details
//' Uses already computed probability tables to derive a suitable starting value to accelerate
//' convergence.
//' @param probs Probability matrix returned by the group sequential recursion (rows = regions, cols = stages).
//' @param alpha Target type I error level.
//' @param sided Number of sides for testing (1 or 2).
//' @return A scalar double initial value.
//' @keywords internal
//' @noRd
//'
double getZeroApproximation(NumericMatrix probs, double alpha, int sided) {
	if (sided == 1) {
		return sum(probs(2, _) - probs(1, _)) - alpha;
	}

	return sum(probs(2, _) - probs(1, _) + probs(0, _)) - alpha;
}

//' @title Alpha-spending function value
//' @description Evaluate the cumulative spending \eqn{\alpha(t)} for a given information fraction `x` and design type.
//' @details
//' Supports common spending families used in rpact (e.g., O'Brien–Fleming, Pocock, Kim–DeMets with
//' parameter `gamma`). The `sided` argument determines whether the returned spending corresponds to
//' one- or two-sided testing.
//' @param alpha Target type I error level.
//' @param x Evaluation point(s) on the integration grid (stage-wise Z-scale).
//' @param sided Number of sides for testing (1 or 2).
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param gamma Spending function parameter (e.g., Kim–DeMets).
//' @return A scalar double with the cumulative spent alpha at information fraction `x`.
//' @keywords internal
//' @noRd
//'
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
		if (gamma == 0) {
			return alpha * x;
		}
		return alpha * (1 - exp(-gamma * x)) / (1 - exp(-gamma));
	}

	return NA_REAL;
}

//' @title Calibrate one-sided critical value at stage k
//' @description Compute/adjust the critical value at stage `k` so that the incremental (or cumulative) spent alpha matches the target spending.
//' @details
//' This function is used by alpha-spending boundary constructors. It repeatedly calls the probability
//' engine with a candidate boundary to match the requested spending within `tolerance`.
//' @param k Stage index (1-based).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param alpha Target type I error level.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A scalar double critical value for stage `k`.
//' @keywords internal
//' @noRd
//'
double getCriticalValue(int k, NumericVector criticalValues, NumericVector userAlphaSpending, double alpha,
	double gammaA, String typeOfDesign, double sided, NumericVector informationRates, bool bindingFutility,
	NumericVector futilityBounds, double tolerance) {

	double alphaSpendingValue;
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER || typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
		alphaSpendingValue = userAlphaSpending[k - 1];
	} else {
		alphaSpendingValue = getSpendingValueCpp(alpha, (double) informationRates[k - 1], sided, typeOfDesign, gammaA);
	}

	if (k == 1) {
		return (getOneMinusQNorm(alphaSpendingValue / sided));
	}

	double criticalValue = NA_REAL;
	NumericVector criticalValuesTemp = Rcpp::clone(criticalValues);
	bisection2([&](double scale) {
		criticalValue = scale;
		criticalValuesTemp[k - 1] = criticalValue;
		NumericMatrix decisionMatrix = getDecisionMatrix(criticalValuesTemp, futilityBounds, bindingFutility, sided, k);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, rangeVector(informationRates, 0, k - 1));
		return getZeroApproximation(probs, alphaSpendingValue, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

	return criticalValue;
}

//' @title Construct critical values from alpha spending (internal)
//' @description Internal worker that builds the vector of critical values for an alpha-spending design.
//' @details
//' Iterates `k=1..kMax`, computes the target cumulative spending at each information rate, and
//' calibrates the corresponding critical value via `getCriticalValue()`.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param alpha Target type I error level.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
NumericVector getDesignGroupSequentialAlphaSpending(int kMax, NumericVector userAlphaSpending, double alpha,
	double gammaA, String typeOfDesign, double sided, NumericVector informationRates, bool bindingFutility,
	NumericVector futilityBounds, double tolerance) {

	NumericVector criticalValues = NumericVector(kMax, NA_REAL);
	for (int k = 1; k <= kMax; k++) {
		criticalValues[k - 1] = getCriticalValue(k, criticalValues, userAlphaSpending, alpha, gammaA, typeOfDesign,
			sided, informationRates, bindingFutility, futilityBounds, tolerance);
	}
	return criticalValues;
}

//' @title User-defined alpha-spending critical values
//' @description Compute critical values for a group sequential design from user-supplied cumulative alpha-spending values.
//' @details
//' Uses the recursive integration engine to calibrate boundaries stage-by-stage so that the achieved
//' cumulative type I error matches the provided `userAlphaSpending`.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialUserDefinedAlphaSpendingCpp")]]
//'
NumericVector getDesignGroupSequentialUserDefinedAlphaSpendingCpp(int kMax, NumericVector userAlphaSpending,
	double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds,
	double tolerance) {
	return getDesignGroupSequentialAlphaSpending(kMax, userAlphaSpending,
	NA_REAL,
	NA_REAL, C_TYPE_OF_DESIGN_AS_USER, sided, informationRates, bindingFutility, futilityBounds, tolerance);
}

//' @title Alpha-spending design critical values
//' @description Compute critical values for standard alpha-spending families (O'Brien–Fleming, Pocock, Kim–DeMets, etc.).
//' @details
//' Evaluates the spending function at the given information rates and calibrates the resulting
//' boundaries using recursive integration. `gammaA` parametrizes the Kim–DeMets family when applicable.
//' @param kMax Maximum number of stages (analyses).
//' @param alpha Target type I error level.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialAlphaSpendingCpp")]]
//'
NumericVector getDesignGroupSequentialAlphaSpendingCpp(int kMax, double alpha, double gammaA, String typeOfDesign,
	double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds,
	double tolerance) {
	return getDesignGroupSequentialAlphaSpending(kMax, NumericVector(0), alpha, gammaA, typeOfDesign, sided,
		informationRates, bindingFutility, futilityBounds, tolerance);
}

//' @title Delta-WT design critical values
//' @description Compute critical values for the Delta-WT design family at the specified information rates.
//' @details
//' Uses the same calibration approach as other spending designs but with Delta-WT specific
//' parameterization (`deltaWT`).
//' @param kMax Maximum number of stages (analyses).
//' @param alpha Target type I error level.
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param deltaWT Delta parameter for the Delta-WT design family.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialDeltaWTCpp")]]
//'
NumericVector getDesignGroupSequentialDeltaWTCpp(int kMax, double alpha, double sided, NumericVector informationRates,
	bool bindingFutility, NumericVector futilityBounds, double tolerance, double deltaWT) {

	NumericVector criticalValues(kMax);
	double scale = bizero([&](double scale) {
		for (int k = 0; k < kMax; k++) {
			criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
		}
		NumericMatrix decisionMatrix = getDecisionMatrix(criticalValues, futilityBounds, bindingFutility, sided);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates);
		return getZeroApproximation(probs, alpha, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

	for (int k = 0; k < kMax; k++) {
		criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
	}

	return criticalValues;
}

//' @title Pocock design critical values
//' @description Compute Pocock-type constant critical values calibrated to the requested overall alpha.
//' @details
//' This wrapper targets the Pocock boundary family using the generic calibration machinery.
//' @param kMax Maximum number of stages (analyses).
//' @param alpha Target type I error level.
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialPocockCpp")]]
//'
NumericVector getDesignGroupSequentialPocockCpp(int kMax, double alpha, double sided, NumericVector informationRates,
	bool bindingFutility, NumericVector futilityBounds, double tolerance) {
	return getDesignGroupSequentialDeltaWTCpp(kMax, alpha, sided, informationRates, bindingFutility, futilityBounds,
		tolerance, 0.5);
}

//' @title O'Brien–Fleming design critical values
//' @description Compute O'Brien–Fleming-type critical values calibrated to the requested overall alpha.
//' @details
//' This wrapper targets the O'Brien–Fleming boundary family using the generic calibration machinery.
//' @param kMax Maximum number of stages (analyses).
//' @param alpha Target type I error level.
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of critical values.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialOBrienAndFlemingCpp")]]
//'
NumericVector getDesignGroupSequentialOBrienAndFlemingCpp(int kMax, double alpha, double sided,
	NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance) {
	return getDesignGroupSequentialDeltaWTCpp(kMax, alpha, sided, informationRates, bindingFutility, futilityBounds,
		tolerance, 0);
}

//' @title Decision matrix for futility-bound calibration
//' @description Build a temporary decision matrix used while solving for futility bounds under beta spending.
//' @details
//' Applies an optional `shift` and supports one- and two-sided designs via `sided`.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param futilityBoundsTemp Temporary futility bounds used during calibration.
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param sided Number of sides for testing (1 or 2).
//' @return A numeric matrix used by the probability engine.
//' @keywords internal
//' @noRd
//'
NumericMatrix getDecisionMatrixForFutilityBounds(NumericVector informationRates, NumericVector criticalValues,
	NumericVector futilityBoundsTemp, double shift, double sided) {

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

//' @title Calibrate one-sided futility bound at stage k
//' @description Solve for the futility boundary at stage `k` so that the achieved beta spending matches the target.
//' @details
//' Uses the recursive integration engine to compute type II error spending given candidate futility
//' bounds and searches until the discrepancy is within `tolerance`.
//' @param k Stage index (1-based).
//' @param betaSpendingValues Cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A scalar double futility bound for stage `k`.
//' @keywords internal
//' @noRd
//'
double getFutilityBoundOneSided(int k, NumericVector betaSpendingValues, NumericVector informationRates,
	NumericVector futilityBounds, NumericVector criticalValues, double shift, double tolerance) {
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
		probs = getGroupSequentialProbabilitiesFast(getDecisionMatrixSubset(decisionMatrix, k),
			rangeVector(informationRates, 0, k - 1));
		return (double) betaSpendingValues[k - 1] - sum(probs);
	}, -6.0, 5.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	return futilityBound;
}

//' @title One-sided futility bounds from beta spending
//' @description Compute the vector of futility bounds for a one-sided design from beta spending values.
//' @details
//' Iterates stages and calls `getFutilityBoundOneSided()`.
//' @param kMax Maximum number of stages (analyses).
//' @param betaSpendingValues Cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of futility bounds.
//' @keywords internal
//' @noRd
//'
NumericVector getFutilityBoundsOneSided(int kMax, NumericVector betaSpendingValues, NumericVector informationRates,
	NumericVector criticalValues, double shift, double tolerance) {
	NumericVector futilityBounds = NumericVector(kMax, NA_REAL);
	for (int k = 1; k <= kMax; k++) {
		futilityBounds[k - 1] = getFutilityBoundOneSided(k, betaSpendingValues, informationRates, futilityBounds,
			criticalValues, shift, tolerance);
	}
	return futilityBounds;
}

//' @title Probability table for futility-bound solving
//' @description Compute probabilities needed to evaluate the beta-spending objective for a given set of futility bounds.
//' @details
//' Wraps the group sequential probability engine with a decision matrix tailored for futility
//' calibration.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param k Stage index (1-based).
//' @param sided Number of sides for testing (1 or 2).
//' @return A numeric matrix of probabilities.
//' @keywords internal
//' @noRd
//'
NumericMatrix getProbabilitiesForFutilityBounds(NumericVector informationRates, NumericVector criticalValues,
	NumericVector futilityBounds, double shift, int k, double sided) {

	NumericMatrix decisionMatrix = getDecisionMatrixForFutilityBounds(
		informationRates, criticalValues, futilityBounds, shift, sided);
	return getGroupSequentialProbabilitiesCpp(getDecisionMatrixSubset(decisionMatrix, k),
		rangeVector(informationRates, 0, k - 1));
}

//' @title One-sided beta-spending design
//' @description Compute a one-sided group sequential design with efficacy boundaries (alpha spending) and futility boundaries (beta spending).
//' @details
//' Combines alpha-spending critical value calibration with beta-spending futility bound calibration.
//' Supports user-defined spending or standard families depending on `typeOfDesign` and
//' `typeBetaSpending`.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param userBetaSpending User-provided cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param typeBetaSpending Identifier for beta spending family or user-defined spending.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param gammaB Beta spending parameter (e.g., Kim–DeMets gamma for beta).
//' @param alpha Target type I error level.
//' @param beta Target type II error (1 - power) at the design alternative.
//' @return A list containing critical values, futility bounds, and intermediate spending/probability results.
//' @keywords internal
//' @noRd
//'
List getDesignGroupSequentialBetaSpendingOneSidedCpp(NumericVector criticalValues, int kMax,
	NumericVector userAlphaSpending, NumericVector userBetaSpending, NumericVector informationRates,
	bool bindingFutility, double tolerance, String typeOfDesign, String typeBetaSpending, double gammaA, double gammaB,
	double alpha, double beta) {

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
			betaSpendingValues[k] = getSpendingValueCpp(beta, (double) informationRates[k], sided, typeBetaSpending,
				gammaB);
		}
	}

	NumericVector futilityBounds;
	double shiftResult;
	if (!bindingFutility) {
		shiftResult = bizero(
			[&](double shift) {
				futilityBounds = getFutilityBoundsOneSided(kMax, betaSpendingValues, informationRates, criticalValues,
					shift, tolerance);
				return (double) futilityBounds[kMax - 1] - (double) criticalValues[kMax - 1];
			}, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	} else {
		futilityBounds = NumericVector(kMax, NA_REAL);
		shiftResult = bisection2(
			[&](double shift) {
				for (int k = 1; k <= kMax; k++) {
					if (typeOfDesign != C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
						criticalValues[k - 1] = getCriticalValue(k, criticalValues, userAlphaSpending, alpha, gammaA,
							typeOfDesign, sided, informationRates, bindingFutility, futilityBounds, tolerance);
					}

					futilityBounds[k - 1] = getFutilityBoundOneSided(k, betaSpendingValues, informationRates,
						futilityBounds, criticalValues, shift, tolerance);
				}
				return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
			}, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	}

	NumericMatrix probs = getProbabilitiesForFutilityBounds(informationRates, criticalValues, futilityBounds,
		shiftResult, kMax, sided);
	NumericVector betaSpent = cumsum(probs(0, _));
	NumericVector power = cumsum(probs(2, _) - probs(1, _));

	futilityBounds = rangeVector(futilityBounds, 0, kMax - 2);

	return List::create(_["futilityBounds"] = futilityBounds, _["criticalValues"] = criticalValues, _["betaSpent"] =
		betaSpent, _["power"] = power, _["shift"] = shiftResult);
}

//' @title First positive entry index
//' @description Return the first index where the vector is strictly larger than zero.
//' @details
//' Utility used when beta spending is partially specified (e.g., starts at a later look).
//' @param vec Numeric vector to scan.
//' @return An integer index (1-based in R sense when returned to R) or 0 if none found.
//' @keywords internal
//' @noRd
//'
int getFirstIndexOfValuLargerZero(NumericVector vec) {
	for (int i = 0; i < vec.size(); i++) {
		if (!R_IsNA((double) vec[i]) && vec[i] > 0) {
			return i;
		}
	}
	return -1;
}

// Add additional option betaAdjustment for group sequential design (default = FALSE)
//' @title Adjust beta-spending vector
//' @description Optionally adjust beta spending values to account for late start (`kMin`) and numerical constraints.
//' @details
//' If `betaAdjustment` is enabled, the spending vector is rescaled/shifted so that the cumulative
//' spending is consistent with the target overall `beta`.
//' @param kMax Maximum number of stages (analyses).
//' @param kMin First stage where beta spending becomes active (used for adjustment).
//' @param betaSpendingValues Cumulative beta spending values per stage.
//' @param betaAdjustment Whether to apply beta-spending adjustment when beta spending starts later than stage 1.
//' @return A numeric vector of adjusted beta spending values.
//' @keywords internal
//' @noRd
//'
NumericVector getAdjustedBetaSpendingValues(int kMax, int kMin, NumericVector betaSpendingValues, bool betaAdjustment) {
	if (kMin <= 0) {
		return betaSpendingValues;
	}

	NumericVector betaSpendingValuesAdjusted = Rcpp::clone(betaSpendingValues);
	for (int k = 0; k < kMin; k++) {
		betaSpendingValuesAdjusted[k] = 0;
	}
	if (betaAdjustment) {
		for (int k = kMin - 1; k < kMax; k++) {
			betaSpendingValuesAdjusted[k] = (betaSpendingValues[k] - betaSpendingValues[kMin - 1])
				/ (betaSpendingValues[kMax - 1] - betaSpendingValues[kMin - 1]) * betaSpendingValues[kMax - 1];
		}
	}
	return betaSpendingValuesAdjusted;
}

//' @title Calibrate two-sided futility bound at stage k
//' @description Solve for the two-sided futility boundary at stage `k` under beta spending, potentially using an auxiliary one-sided bound.
//' @details
//' Two-sided futility calibration is more complex because the acceptance region is typically central;
//' this routine uses the probability engine and root finding to match the target spending within
//' `tolerance`.
//' @param k Stage index (1-based).
//' @param betaSpendingValues Cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param futilityBoundsOneSided Auxiliary one-sided futility bounds used to stabilize two-sided calibration.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A scalar double futility bound for stage `k`.
//' @keywords internal
//' @noRd
//'
double getFutilityBoundTwoSided(int k, NumericVector betaSpendingValues, NumericVector informationRates,
	NumericVector futilityBounds, NumericVector futilityBoundsOneSided, NumericVector criticalValues, double shift,
	double tolerance) {
	if (k == 1) {
		double futilityBound = bizero(
			[&](double u) {
				return getNormalDistribution(u - sqrt((double) informationRates[0]) * shift)
					- getNormalDistribution(-u - sqrt((double) informationRates[0]) * shift) - betaSpendingValues[0];
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
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(decisionMatrix(_, Range(0, k - 1)),
			informationRates[Range(0, k - 1)]);
		double probsSum = sum(probs.row(2) - probs.row(1));
		return (double) betaSpendingValues[k - 1] - probsSum;
	}, -6.0, 5.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	return futilityBound;
}

//' @title Two-sided futility bounds from beta spending
//' @description Compute the vector of futility bounds for a two-sided design from beta spending values.
//' @details
//' Iterates stages and calls `getFutilityBoundTwoSided()`.
//' @param kMax Maximum number of stages (analyses).
//' @param betaSpendingValues Cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param futilityBoundsOneSided Auxiliary one-sided futility bounds used to stabilize two-sided calibration.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param shift Numeric shift applied to futility bounds during calibration (implementation detail for stability).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A numeric vector of futility bounds.
//' @keywords internal
//' @noRd
//'
NumericVector getFutilityBoundsTwoSided(int kMax, NumericVector betaSpendingValues, NumericVector informationRates,
	NumericVector futilityBoundsOneSided, NumericVector criticalValues, double shift, double tolerance) {
	NumericVector futilityBounds = NumericVector(kMax, NA_REAL);
	for (int k = 1; k <= kMax; k++) {
		futilityBounds[k - 1] = getFutilityBoundTwoSided(k, betaSpendingValues, informationRates, futilityBounds,
			futilityBoundsOneSided, criticalValues, shift, tolerance);
	}
	return futilityBounds;
}

//' @title Calibrate two-sided critical value at stage k
//' @description Compute/adjust the two-sided critical value at stage `k` so that the achieved cumulative alpha matches the target spending.
//' @details
//' Uses a symmetric boundary (±c_k) and repeatedly calls the probability engine until the alpha
//' constraint is met within `tolerance`.
//' @param kMax Maximum number of stages (analyses).
//' @param k Stage index (1-based).
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param alpha Target type I error level.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param futilityBounds Vector of futility bounds per stage (may be `NA` / default when not used).
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @return A scalar double two-sided critical value for stage `k`.
//' @keywords internal
//' @noRd
//'
double getCriticalValueTwoSided(int kMax, int k, NumericVector criticalValues, NumericVector userAlphaSpending,
	double alpha, double gammaA, String typeOfDesign, NumericVector informationRates, bool bindingFutility,
	NumericVector futilityBounds, double tolerance) {

	double sided = 2.0;
	double alphaSpendingValue;
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER || typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
		alphaSpendingValue = userAlphaSpending[k - 1];
	} else {
		alphaSpendingValue = getSpendingValueCpp(alpha, (double) informationRates[k - 1], sided, typeOfDesign, gammaA);
	}

	if (k == 1) {
		return (getOneMinusQNorm(alphaSpendingValue / sided));
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
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(decisionMatrix(_, Range(0, k - 1)), informationRates[Range(0, k - 1)]);
		return sum(probs(4, _) - probs(3, _) + probs(0, _)) - alphaSpendingValue;
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

	return criticalValue;
}

//' @title Two-sided beta-spending design
//' @description Compute a two-sided group sequential design with efficacy and futility boundaries under alpha/beta spending.
//' @details
//' Extends the one-sided machinery to two-sided testing, including options for beta adjustment and
//' specifying whether power is defined two-sided.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param userBetaSpending User-provided cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param typeBetaSpending Identifier for beta spending family or user-defined spending.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param gammaB Beta spending parameter (e.g., Kim–DeMets gamma for beta).
//' @param alpha Target type I error level.
//' @param beta Target type II error (1 - power) at the design alternative.
//' @param betaAdjustment Whether to apply beta-spending adjustment when beta spending starts later than stage 1.
//' @param twoSidedPower Whether to interpret power/beta in a two-sided sense in the two-sided design.
//' @return A list containing critical values, futility bounds, and diagnostics.
//' @keywords internal
//' @noRd
//'
List getDesignGroupSequentialBetaSpendingTwoSidedCpp(NumericVector criticalValues, int kMax,
	NumericVector userAlphaSpending, NumericVector userBetaSpending, NumericVector informationRates,
	bool bindingFutility, double tolerance, String typeOfDesign, String typeBetaSpending, double gammaA, double gammaB,
	double alpha, double beta, bool betaAdjustment, bool twoSidedPower) {

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
	NumericVector futilityBoundsOneSided = getDesignGroupSequentialBetaSpendingOneSidedCpp(criticalValues, kMax,
		userAlphaSpending / 2.0, userBetaSpending, informationRates, bindingFutility, tolerance, typeOfDesign,
		typeBetaSpending, gammaA, gammaB, alpha / 2.0, beta)["futilityBounds"];

	NumericVector betaSpendingValues;
	if (typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
		betaSpendingValues = userBetaSpending;
	} else {
		betaSpendingValues = NumericVector(kMax, NA_REAL);
		for (int k = 0; k < kMax; k++) {
			betaSpendingValues[k] = getSpendingValueCpp(beta, (double) informationRates[k], sided, typeBetaSpending,
				gammaB);
		}
	}

	double kMin = getFirstIndexOfValuLargerZero(futilityBoundsOneSided);

	betaSpendingValues = getAdjustedBetaSpendingValues(kMax, kMin, betaSpendingValues, betaAdjustment);

	NumericVector futilityBounds;
	double shiftResult;
	if (!bindingFutility) {
		shiftResult = bisection2(
			[&](double shift) {
				futilityBounds = getFutilityBoundsTwoSided(kMax, betaSpendingValues, informationRates,
					futilityBoundsOneSided, criticalValues, shift, tolerance);
				return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
			}, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	} else {
		futilityBounds = NumericVector(kMax, NA_REAL);
		shiftResult = bisection2(
			[&](double shift) {
				for (int k = 1; k <= kMax; k++) {
					criticalValues[k - 1] = getCriticalValueTwoSided(kMax, k, criticalValues, userAlphaSpending, alpha,
						gammaA, typeOfDesign, informationRates, bindingFutility, futilityBounds, tolerance);

					futilityBounds[k - 1] = getFutilityBoundTwoSided(k, betaSpendingValues, informationRates,
						futilityBounds, futilityBoundsOneSided, criticalValues, shift, tolerance);
				}
				return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
			}, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	}

	NumericMatrix probs = getProbabilitiesForFutilityBounds(informationRates, criticalValues, futilityBounds,
		shiftResult, kMax, sided);
	NumericVector betaSpent = cumsum(probs(2, _) - probs(1, _));
	NumericVector power(kMax);
	if (twoSidedPower) {
		power = (NumericVector) cumsum(probs(4, _) - probs(3, _) + probs(0, _));
	} else {
		power = (NumericVector) cumsum(probs(4, _) - probs(3, _));
	}

	futilityBounds = rangeVector(futilityBounds, 0, kMax - 2);
	futilityBounds[futilityBounds <= 1e-05] = NA_REAL;

	return List::create(_["futilityBounds"] = futilityBounds, _["criticalValues"] = criticalValues, _["betaSpent"] =
		betaSpent, _["power"] = power, _["shift"] = shiftResult);
}

//' @title General beta-spending design (one- or two-sided)
//' @description Main entry point for constructing group sequential designs with both alpha and beta spending.
//' @details
//' Dispatches to one-sided or two-sided implementations depending on `sided` and returns a unified
//' result structure.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param userBetaSpending User-provided cumulative beta spending values per stage.
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param typeBetaSpending Identifier for beta spending family or user-defined spending.
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param gammaB Beta spending parameter (e.g., Kim–DeMets gamma for beta).
//' @param alpha Target type I error level.
//' @param beta Target type II error (1 - power) at the design alternative.
//' @param sided Number of sides for testing (1 or 2).
//' @param betaAdjustment Whether to apply beta-spending adjustment when beta spending starts later than stage 1.
//' @param twoSidedPower Whether to interpret power/beta in a two-sided sense in the two-sided design.
//' @return A list containing boundaries, spending vectors, and diagnostics.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialBetaSpendingCpp")]]
//'
List getDesignGroupSequentialBetaSpendingCpp(NumericVector criticalValues, int kMax, NumericVector userAlphaSpending,
	NumericVector userBetaSpending, NumericVector informationRates, bool bindingFutility, double tolerance,
	String typeOfDesign, String typeBetaSpending, double gammaA, double gammaB, double alpha, double beta, double sided,
	bool betaAdjustment, bool twoSidedPower) {
	if (sided == 1) {
		return getDesignGroupSequentialBetaSpendingOneSidedCpp(criticalValues, kMax, userAlphaSpending,
			userBetaSpending, informationRates, bindingFutility, tolerance, typeOfDesign, typeBetaSpending, gammaA,
			gammaB, alpha, beta);
	}

	return getDesignGroupSequentialBetaSpendingTwoSidedCpp(criticalValues, kMax, userAlphaSpending, userBetaSpending,
		informationRates, bindingFutility, tolerance, typeOfDesign, typeBetaSpending, gammaA, gammaB, alpha, beta,
		betaAdjustment, twoSidedPower);
}

//' @title User-defined beta spending design
//' @description Construct a group sequential design from user-provided alpha and beta spending vectors.
//' @details
//' Calibrates critical values and futility bounds stage-by-stage to match the supplied spending, using
//' recursive integration for probability evaluation.
//' @param criticalValues Vector of efficacy (upper) critical values per stage.
//' @param kMax Maximum number of stages (analyses).
//' @param userAlphaSpending User-provided cumulative alpha spending values per stage.
//' @param userBetaSpending User-provided cumulative beta spending values per stage.
//' @param sided Number of sides for testing (1 or 2).
//' @param informationRates Information rates / fractions per stage (monotone increasing, length kMax).
//' @param bindingFutility If `true`, futility bounds are binding (affect type I error); otherwise non-binding.
//' @param tolerance Absolute convergence tolerance for iterative calibration / root finding.
//' @param typeOfDesign Identifier for the spending/design family (e.g., "asOF", "asP", "asKD", "asUser").
//' @param gammaA Alpha spending parameter (e.g., Kim–DeMets gamma).
//' @param alpha Target type I error level.
//' @param betaAdjustment Whether to apply beta-spending adjustment when beta spending starts later than stage 1.
//' @param twoSidedPower Whether to interpret power/beta in a two-sided sense in the two-sided design.
//' @return A list containing boundaries and diagnostics.
//' @keywords internal
//' @noRd
//'
//' [[Rcpp::export(name = ".getDesignGroupSequentialUserDefinedBetaSpendingCpp")]]
//'
List getDesignGroupSequentialUserDefinedBetaSpendingCpp(NumericVector criticalValues, int kMax,
	NumericVector userAlphaSpending, NumericVector userBetaSpending, double sided, NumericVector informationRates,
	bool bindingFutility, double tolerance, String typeOfDesign, double gammaA, double alpha, bool betaAdjustment,
	bool twoSidedPower) {
	String typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER;
	double gammaB = NA_REAL;
	double beta = NA_REAL;

	return getDesignGroupSequentialBetaSpendingCpp(criticalValues, kMax, userAlphaSpending, userBetaSpending,
		informationRates, bindingFutility, tolerance, typeOfDesign, typeBetaSpending, gammaA, gammaB, alpha, beta,
		sided, betaAdjustment, twoSidedPower);
}
