/**
 *
 * -- Simulation of survival data with group sequential and combination test --
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
 * File version: $Revision: 8660 $
 * Last changed: $Date: 2025-04-01 11:45:17 +0200 (Di, 01 Apr 2025) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <algorithm>
#include <cmath>
#include <iostream>
#include <Rcpp.h>
#include "f_as251.h"

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace std;

const double C_QNORM_EPSILON = 1.0e-100; // a value between 1e-323 and 1e-16
const double C_QNORM_MAXIMUM = -R::qnorm(C_QNORM_EPSILON, 0, 1, 1, 0);
const double C_QNORM_MINIMUM = -C_QNORM_MAXIMUM;
const double C_QNORM_THRESHOLD = floor(C_QNORM_MAXIMUM);
const double C_FUNCTION_ROOT_TOLERANCE_FACTOR = 100.0;

struct Exception : std::exception {
    char text[1000];

    Exception(char const* fmt, ...) __attribute__((format(printf,2,3))) {
        va_list ap;
        va_start(ap, fmt);
        vsnprintf(text, sizeof text, fmt, ap);
        va_end(ap);
    }

    char const* what() const throw() {
    	return text;
    }
};

double getQNormEpsilon() {
	return C_QNORM_EPSILON;
}

double getQNormThreshold() {
	return C_QNORM_THRESHOLD;
}

double getQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = 1.0e-100) {

	if (p <= 0) {
		p = epsilon;
	}
	if (p > 1) {
		p = 1;
	}

    double result = R::qnorm(p, mean, sd, lowerTail, logP);

    if (result < -C_QNORM_THRESHOLD) {
    	result = C_QNORM_MINIMUM;
    }
    if (result > C_QNORM_THRESHOLD) {
    	result = C_QNORM_MAXIMUM;
    }

    return result;
}

// [[Rcpp::export]]
double getOneMinusQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = 1.0e-100) {

	if (p <= 0) {
		p = epsilon;
	}
	if (p > 1) {
		p = 1;
	}

    double result;
    if (p < 0.5) {
    	result = -R::qnorm(p, mean, sd, lowerTail, logP);
    } else {
		// prevent values that are close to 1 from becoming Inf, see qnorm(1)
		// example: 1 - 1e-17 = 1 in R, i.e., qnorm(1 - 1e-17) = Inf
		// on the other hand: qnorm(1e-323) = -38.44939
    	result = R::qnorm(1 - p, mean, sd, lowerTail, logP);
    }

    if (result < -C_QNORM_THRESHOLD) {
    	result = C_QNORM_MINIMUM;
    }
    if (result > C_QNORM_THRESHOLD) {
    	result = C_QNORM_MAXIMUM;
    }

    return result;
}

double getOneMinusPNorm(double q, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = 1.0e-100) {

	// return 1 - R::pnorm(q, mean, sd, lowerTail, logP);

	if (q == 0) {
		return 0.5;
	}

	double result;
	if (q < 5) {
		result = 1.0 - R::pnorm(q, mean, sd, lowerTail, logP);
	} else {
		result = R::pnorm(-q, mean, sd, lowerTail, logP);
	}

	if (result <= 0) {
		result = epsilon;
	}

	return result;
}

template<int RTYPE>
IntegerVector order_impl(const Vector <RTYPE> &x, bool desc) {
    auto n = x.size();
    IntegerVector idx = no_init(n);
    std::iota(idx.begin(), idx.end(), static_cast<size_t>(1));
    if (desc) {
        auto comparator = [&x](size_t a, size_t b) { return x[a - 1] > x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
    } else {
        auto comparator = [&x](size_t a, size_t b) { return x[a - 1] < x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
        // simulate na.last
        size_t nas = 0;
        for (int i = 0; i < n; ++i, ++nas)
            if (!Vector<RTYPE>::is_na(x[idx[i] - 1])) break;
        std::rotate(idx.begin(), idx.begin() + nas, idx.end());
    }
    return idx;
}

// identical to the R function base::order()
IntegerVector getOrder(SEXP x, bool desc = false) {
    switch (TYPEOF(x)) {
        case INTSXP:
            return order_impl<INTSXP>(x, desc);
        case REALSXP:
            return order_impl<REALSXP>(x, desc);
        case STRSXP:
            return order_impl<STRSXP>(x, desc);
        default:
            stop("Unsupported type.");
    }
    return IntegerVector::create();
}

// Custom order function that returns ascending permutation indices
// @param x NumericVector to be ordered
// @return IntegerVector of 1-based indices in ascending order
//
// [[Rcpp::export(name = ".orderCpp")]]
IntegerVector order(NumericVector x) {
	int n = x.size();
	IntegerVector indices(n);
	
	// Initialize indices vector with 1-based indexing
	for (int i = 0; i < n; i++) {
		indices[i] = i + 1;
	}
	
	// Sort indices based on corresponding values in x
	std::sort(indices.begin(), indices.end(), [&](int a, int b) {
		return x[a-1] < x[b-1];  // Convert back to 0-based for comparison
	});
	
	return indices;
}

NumericVector vectorSum(NumericVector x, NumericVector y) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x[i] + y[i];
    }
    return result;
}

NumericVector vectorSub(NumericVector x, NumericVector y) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x[i] - y[i];
    }
    return result;
}

double vectorSum(NumericVector x) {
    int n = x.size();
    if (n <= 1) {
        return n == 0 ? 0 : x[0];
    }

    double s = x[0];
    for (int i = 1; i < n; i++) {
        s += x[i];
    }
    return s;
}

NumericVector vectorSqrt(NumericVector x) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = sqrt((double) x[i]);
    }
    return result;
}

NumericVector vectorDivide(NumericVector x, double value) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x[i] / value;
    }
    return result;
}

NumericVector vectorDivide(NumericMatrix x, int rowIndex, double value) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x(rowIndex, i) / value;
    }
    return result;
}

NumericVector vectorDivide(NumericVector x, NumericVector y) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        if (y[i] != 0.0) {
            result[i] = x[i] / y[i];
        }
    }
    return result;
}

NumericVector vectorMultiply(NumericVector x, double multiplier) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x[i] * multiplier;
    }
    return result;
}

NumericVector vectorMultiply(NumericVector x, NumericVector y) {
	if (x.size() != y.size()) {
		throw Exception("Failed to multiply vectors: size is different (%i != %i)",
			(int) x.size(), (int) y.size());
	}

    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = x[i] * y[i];
    }
    return result;
}

NumericVector vectorPow(NumericVector x, NumericVector y) {
    int n = x.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = pow((double) x[i], (double) y[i]);
    }
    return result;
}

NumericVector vectorPow(double x, NumericVector y) {
    int n = y.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = pow(x, (double) y[i]);
    }
    return result;
}

NumericVector vectorPow2(NumericVector y, double exp) {
    int n = y.size();
    NumericVector result = NumericVector(n, NA_REAL);
    for (int i = 0; i < n; i++) {
        result[i] = pow((double) y[i], exp);
    }
    return result;
}

NumericVector vectorRepEachValue(NumericVector x, int kMax) {
    int n = x.size();
    NumericVector result = NumericVector(n * kMax, NA_REAL);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < kMax; j++) {
            result[i * kMax + j] = x[i];
        }
    }
    return result;
}

double vectorProduct(NumericVector x) {
    int n = x.size();
    if (n == 0) {
        return 0;
    }

    if (n == 1) {
        return x[0];
    }

    double s = x[0];
    for (int i = 1; i < n; i++) {
        s *= x[i];
    }
    return s;
}

double vectorProduct(NumericVector x, NumericVector y) {
    int n = x.size();
    double s = 0;
    for (int i = 0; i < n; i++) {
        s += x[i] * y[i];
    }
    return s;
}

double round(double value, int digits) {
    double mult = std::pow(10.0, (double) digits);
    return round(value * mult) / mult;
}

void vectorSumC(int i, int j, int kMax, double *x, NumericMatrix y) {
    for (int k = 0; k < kMax; k++) {
        x[i * kMax + k] += y(k, j);
    }
}

void vectorInitC(int i, int kMax, double *x, double value) {
    for (int k = 0; k < kMax; k++) {
        x[i * kMax + k] = value;
    }
}

NumericVector concat(NumericVector a, NumericVector b) {
    for (int i = 0; i < b.size(); i++) {
        a.insert(a.end(), b[i]);
    }
    return a;
}

NumericMatrix matrixAdd(NumericMatrix x, NumericMatrix y) {
    NumericMatrix result(x.nrow(), x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i, j) = x(i, j) + y(i, j);
        }
    }
    return result;
}

NumericMatrix matrixSub(NumericMatrix x, NumericMatrix y) {
    NumericMatrix result(x.nrow(), x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i, j) = x(i, j) - y(i, j);
        }
    }
    return result;
}

NumericMatrix matrixMultiply(NumericMatrix x, double y) {
    NumericMatrix result(x.nrow(), x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i, j) = x(i, j) * y;
        }
    }
    return result;
}

/**
 * Calculates root of function f in given interval using the bisection method
 */
double bisection2(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter) {
    int step = 1;
    double value = 1;
    double result = NA_REAL;
    do {
        value = (lower + upper) / 2;
        result = f(value);
        if (result > 0) {
            lower = value;
        } else {
            upper = value;
        }

        step++;
        if (step > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found.");
        }
    } while ((upper - lower) > tolerance);
    return std::abs(result / C_FUNCTION_ROOT_TOLERANCE_FACTOR) > tolerance ? NA_REAL : value;
}

/**
 * Calculates root of function f in given interval using the bisection method
 */
double bisection(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter) {
    int step = 1;
    double value;
    double result = NA_REAL;
    do {
        value = (lower + upper) / 2;
        result = f(value);
        if ((result < 0) == (f(lower) < 0)) { // since signs are now directly compared
            lower = value;
        } else {
            upper = value;
        }

        step++;
        if (step > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found.");
        }
    } while ((upper - lower) > tolerance);
    return std::abs(result / C_FUNCTION_ROOT_TOLERANCE_FACTOR) > tolerance ? NA_REAL : value;
}

/**
 * Calculates root of function f in given interval using the Brent method
 * See https://www.netlib.org/c/index.html
 * See https://www.netlib.org/c/brent.shar
 */
double bizero(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter) {

    double a, b, c;
    double fa;
    double fb;
    double fc;

    int iter = 0;

    a = lower;
    b = upper;
    fa = f(a);
    fb = f(b);
    c = a;
    fc = fa;

    for (;;) {
        double prev_step = b - a;

        double tol_act;
        double p;
        double q;
        double new_step;

        if (std::abs(fc) < std::abs(fb)) {
            a = b;
            b = c;
            c = a;
            fa = fb;
            fb = fc;
            fc = fa;
        }
        tol_act = 2 * std::numeric_limits<double>::epsilon() * std::abs(b) + tolerance / 2;
        new_step = (c - b) / 2;

        if (std::abs(new_step) <= tol_act || fb == (double) 0) {
        	if (std::abs(fb / C_FUNCTION_ROOT_TOLERANCE_FACTOR) > tolerance) {
        		return bisection(f, lower, upper, tolerance, maxIter);
        	}
            return b;
        }

        if (std::abs(prev_step) >= tol_act
        		&& std::abs(fa) > std::abs(fb)) {
            double t1, cb, t2;
            cb = c - b;
            if (a == c) {
                t1 = fb / fa;
                p = cb * t1;
                q = 1.0 - t1;
            } else {
                q = fa / fc;
                t1 = fb / fc;
                t2 = fb / fa;
                p = t2 * (cb * q * (q - t1) - (b - a) * (t1 - 1.0));
                q = (q - 1.0) * (t1 - 1.0) * (t2 - 1.0);
            }
            if (p > (double) 0)
                q = -q;
            else
                p = -p;

            if (p < (0.75 * cb * q - std::abs(tol_act * q) / 2)
                && p < std::abs(prev_step * q / 2))
                new_step = p / q;
        }

        if (std::abs(new_step) < tol_act) {
            if (new_step > (double) 0) {
                new_step = tol_act;
            } else {
                new_step = -tol_act;
            }
        }
        a = b;
        fa = fb;
        b += new_step;
        fb = f(b);
        if ((fb > 0 && fc > 0) || (fb < 0 && fc < 0)) {
            c = a;
            fc = fa;
        }
        iter++;
        if (iter > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found");
        }
    }

    return bisection(f, lower, upper, tolerance, maxIter);
}

/**
 * Calculates root of function f in given interval using the Brent method
 * See https://www.netlib.org/c/index.html
 * See https://www.netlib.org/c/brent.shar
 */
double zeroin(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter) {

    double a, b, c;
    double fa;
    double fb;
    double fc;

    int iter = 0;

    a = lower;
    b = upper;
    fa = f(a);
    fb = f(b);
    c = a;
    fc = fa;

    for (;;) {
        double prev_step = b - a;

        double tol_act;
        double p;
        double q;
        double new_step;

        if (std::abs(fc) < std::abs(fb)) {
            a = b;
            b = c;
            c = a;
            fa = fb;
            fb = fc;
            fc = fa;
        }
        tol_act = 2 * std::numeric_limits<double>::epsilon() * std::abs(b) + tolerance / 2;
        new_step = (c - b) / 2;

        if (std::abs(new_step) <= tol_act || fb == (double) 0) {
            return b;
        }

        if (std::abs(prev_step) >= tol_act
        		&& std::abs(fa) > std::abs(fb)) {
            double t1, cb, t2;
            cb = c - b;
            if (a == c) {
                t1 = fb / fa;
                p = cb * t1;
                q = 1.0 - t1;
            } else {
                q = fa / fc;
                t1 = fb / fc;
                t2 = fb / fa;
                p = t2 * (cb * q * (q - t1) - (b - a) * (t1 - 1.0));
                q = (q - 1.0) * (t1 - 1.0) * (t2 - 1.0);
            }
            if (p > (double) 0)
                q = -q;
            else
                p = -p;

            if (p < (0.75 * cb * q - std::abs(tol_act * q) / 2)
                && p < std::abs(prev_step * q / 2))
                new_step = p / q;
        }

        if (std::abs(new_step) < tol_act) {
            if (new_step > (double) 0) {
                new_step = tol_act;
            } else {
                new_step = -tol_act;
            }
        }
        a = b;
        fa = fb;
        b += new_step;
        fb = f(b);
        if ((fb > 0 && fc > 0) || (fb < 0 && fc < 0)) {
            c = a;
            fc = fa;
        }
        iter++;
        if (iter > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found");
        }
    }

    return NA_REAL;
}

// [[Rcpp::export]]
double zeroin(Function f, double lower, double upper, double tolerance, int maxIter) {
    return zeroin([&](double x){return Rf_asReal(f(x));}, lower, upper, tolerance, maxIter);
}

double max(NumericVector x) {
    if (x.length() == 0) throw std::invalid_argument("Vector is Empty.");
    double max = x[0];
    for (int i = 1; i < x.length(); i++) {
        if (x[i] > max) max = x[i];
    }
    return max;
}

double min(NumericVector x) {
    if (x.length() == 0) throw std::invalid_argument("Vector is Empty.");
    double min = x[0];
    for (int i = 1; i < x.length(); i++) {
        if (x[i] < min) min = x[i];
    }
    return min;
}

/**
 * Returns the subvector of vector x with the given interval
 */
NumericVector rangeVector(NumericVector x, int from, int to) {
	int index = 0;
    NumericVector res;
    if (from <= to) {
    	res = NumericVector(to - from + 1);
        for (int i = from; i <= to; i++) {
            res[index] = x[i];
            index++;
        }
    } else {
    	res = NumericVector(from - to + 1);
        for (int i = from; i >= to; i--) {
            res[index] = x[i];
            index++;
        }
    }
    return res;
}

// [[Rcpp::export]]
std::string getCipheredValue(String x) {
	std::size_t hashValue = std::hash<std::string>{}(x);
	return std::to_string(hashValue);
}

void logDebug(std::string s) {
    Rcout << s << std::endl;
}

/**
 * Seed safe implementation of stats:rt()
 */
double getRandomTDistribution(double df, double ncp) {
	return Rcpp::rnorm(1, ncp)[0] / sqrt(R::rchisq(df) / df);
}


/**
 * @title Get Fraction
 *
 * @description
 * Converts a decimal number to its fractional representation.
 *
 * @details
 * This function uses a continued fraction algorithm to convert a decimal number
 * to its fractional representation. The algorithm iterates up to a specified
 * number of steps or until the remainder is less than the specified epsilon.
 *
 * @param x The decimal number to be converted.
 * @param epsilon The tolerance for the continued fraction algorithm. Default is 1.0e-6.
 * @param maxNumberOfSearchSteps The maximum number of steps for the continued fraction algorithm. Default is 30.
 *
 * @references See https://proofwiki.org/wiki/Continued_Fraction_Algorithm for more information on continued fractions.
 *
 * @return An IntegerVector containing the numerator and denominator of the fractional representation.
 *
 * @noRd
 *
 */
// [[Rcpp::export(name = ".getFraction")]]
IntegerVector getFraction(
		double x,
		double epsilon = 1.0e-6,
		int maxNumberOfSearchSteps = 30) {

	int numerator = (int) floor(x);
	int numerator0;
	int numerator1 = 1;
	int denominator = 1;
	int denominator0;
	int denominator1 = 0;
	int factor0;
	double factor1 = x - (double) numerator;

	int step = 0;
	while (++step < maxNumberOfSearchSteps) {
		if (fabs(x - (double) numerator / (double) denominator) < epsilon) {
			break;
		}

		factor1 = 1 / factor1;
		factor0 = (int) floor(factor1);
		factor1 = factor1 - factor0;
		numerator0 = numerator1;
		numerator1 = numerator;
		numerator = factor0 * numerator1 + numerator0;
		denominator0 = denominator1;
		denominator1 = denominator;
		denominator = factor0 * denominator1 + denominator0;
	}

	return IntegerVector::create(numerator, denominator);
}

/**
 * @title
 * Get Fractions
 *
 * @description
 * Converts a vector of decimal numbers to their fractional representations.
 *
 * @details
 * This function uses a continued fraction algorithm to convert each decimal number
 * in the input vector to its fractional representation. The algorithm iterates up to
 * a specified number of steps or until the remainder is less than the specified epsilon.
 *
 * @param x A NumericVector of decimal numbers to be converted.
 * @param epsilon The tolerance for the continued fraction algorithm. Default is 1.0e-6.
 * @param maxNumberOfSearchSteps The maximum number of steps for the continued fraction algorithm. Default is 30.
 *
 * @references See https://proofwiki.org/wiki/Continued_Fraction_Algorithm for more information on continued fractions.
 *
 * @return A List containing the original decimal numbers, their numerators, and denominators.
 *
 * @noRd
 *
 */
// [[Rcpp::export(name = ".getFractions")]]
List getFractions(
		NumericVector x,
		double epsilon = 1.0e-6,
		int maxNumberOfSearchSteps = 30) {

	int n = x.size();
	IntegerVector numerators(n);
	IntegerVector denominators(n);
	IntegerVector result;

	for (int i = 0; i < n; ++i) {
		result = getFraction(x[i], epsilon, maxNumberOfSearchSteps);
		numerators[i] = result[0];
		denominators[i] = result[1];
	}

	return List::create(
		Named("x") = x,
		Named("numerator") = numerators,
		Named("denominator") = denominators
	);
}

// Returns the 0-based indices of TRUE values in a logical vector
IntegerVector which(const LogicalVector& x) {
	std::vector<int> indices;
	for (int i = 0; i < x.size(); i++) {
		if (x[i]) {
			indices.push_back(i);
		}
	}
	return Rcpp::wrap(indices);
}

// Returns a logical vector indicating if elements of x are in the set
LogicalVector charInSet(const CharacterVector& x, CharacterVector set) {
    int n = x.size();
    LogicalVector result(n);
    for (int i = 0; i < n; i++) {
        result[i] = false;
        for (int j = 0; j < set.size(); j++) {
            if (Rcpp::as<std::string>(x[i]) == Rcpp::as<std::string>(set[j])) {
                result[i] = true;
                break;
            }
        }
    }
    return result;
}

// Replicates each element in 'values' according to the corresponding count in 'times'
IntegerVector repInt(const IntegerVector& values, const IntegerVector& times) {
    int n = values.size();
    int n_times = times.size();
    if (n != n_times) {
        throw std::invalid_argument("Length of values and times must be the same.");
    }
    
    int length_out = sum(times);
    IntegerVector result(length_out);
    int index = 0;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < times[i]; j++) {
            result[index++] = values[i];
        }
    }
    return result;
}

// Returns the index of the first match of 'value' in 'x', or -1 if not found
int firstMatch(const CharacterVector& x, std::string value) {
    int n = x.size();
    for (int i = 0; i < n; i++) {
        if (Rcpp::as<std::string>(x[i]) == value) {
            return i;
        }
    }
    return -1; // Return -1 if no match is found
}

// Simplified equivalent of R function .applyDirectionOfAlternative.
// Note that directionUpper cannot be a vector here, and values cannot be a logical vector.
NumericVector applyDirectionOfAlternative(const NumericVector& values, 
                                          bool directionUpper, 
                                          String type,
                                          String phase) {
    if (phase == "design") {
        return values;
    }
    if (Rf_isNull(values) || values.size() == 0 || Rcpp::as<bool>(all(is_na(values)))) {
        return values;
    }
    if (R_IsNA(directionUpper) || directionUpper) {
        if (type == "oneMinusValue") {
            return(1 - values);
        }
        if (type == "valueMinusOne") {
            return(values - 1);
        }
        if (type == "negateIfLower") {
            return(values);
        }
        if (type == "negateIfUpper") {
            return(-values);
        }
        if (type == "minMax") {
            return(wrap(min(na_omit(values))));
        }
        if (type == "maxMin") {
            return(wrap(max(na_omit(values))));
        }
    }
    if (type == "negateIfLower") {
        return(-values);
    }
    if (type == "negateIfUpper") {
        return(values);
    }
    if (type == "minMax") {
        return(wrap(max(na_omit(values))));
    }
    if (type == "maxMin") {
        return(wrap(min(na_omit(values))));
    }   
    stop("Unknown type of direction of alternative.");
    return R_NaReal;
}

std::string getClassName(const Environment& x) {
    if (!x.hasAttribute("class")) {
		stop("x has no class attribute");
	}
	std::vector<std::string> xClass = x.attr("class");
    return xClass[0];
}

// [[Rcpp::export(name = ".getMultivarNormalDistributionCpp")]]
double getMultivarNormalDistribution(NumericVector upper, NumericMatrix sigma) {
    
    int dimensionSigma = sigma.ncol();
    
    if (dimensionSigma == 1) {
        return R::pnorm(upper[0], 0.0, 1.0, 1, 0);
    }
    
    NumericVector lower = NumericVector::create(R_NegInf);
    return as251Normal(lower, upper, sigma);
}
