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
 * File version: $Revision: 7019 $
 * Last changed: $Date: 2023-05-31 07:23:47 +0200 (Mi, 31 Mai 2023) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#ifndef PKG_RPACT_H
#define PKG_RPACT_H

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

double getQNormEpsilon();

double getQNormThreshold();

double getQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = getQNormEpsilon());

double getOneMinusQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = getQNormEpsilon());

double getOneMinusPNorm(double q, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = getQNormEpsilon());

IntegerVector getOrder(SEXP x, bool desc = false);

IntegerVector order(NumericVector x);

NumericVector vectorSum(NumericVector x, NumericVector y);

NumericVector vectorSub(NumericVector x, NumericVector y);

double vectorSum(NumericVector x);

NumericVector vectorSqrt(NumericVector x);

NumericVector vectorDivide(NumericVector x, double value);

NumericVector vectorDivide(NumericMatrix x, int rowIndex, double value);

NumericVector vectorDivide(NumericVector x, NumericVector y);

NumericVector vectorMultiply(NumericVector x, double multiplier);

NumericVector vectorMultiply(NumericVector x, NumericVector y);

NumericVector vectorPow(NumericVector x, NumericVector y);

NumericVector vectorPow2(NumericVector y, double exp);

NumericVector vectorRepEachValue(NumericVector x, int kMax);

double vectorProduct(NumericVector x);

double vectorProduct(NumericVector x, NumericVector y);

double round(double value, int digits);

void vectorSumC(int i, int j, int kMax, double* x, NumericMatrix y);

void vectorInitC(int i, int kMax, double* x, double value);

NumericVector concat(NumericVector a, NumericVector b);

NumericMatrix matrixAdd(NumericMatrix x, NumericMatrix y);

NumericMatrix matrixSub(NumericMatrix x, NumericMatrix y);

NumericMatrix matrixMultiply(NumericMatrix x, double y);

NumericVector repInt(int x, int y);

double bisection2(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter);

double bizero(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter);

double zeroin(std::function<double(double)> f, double lower, double upper, double tolerance, int maxIter);

double max(NumericVector x);

double min(NumericVector x);

NumericVector range(int from, int to);

NumericVector rangeVector(NumericVector x, int from, int to);

std::string getCipheredValue(String x);

void logDebug(std::string s);

double getRandomTDistribution(double df, double ncp);

IntegerVector which(const LogicalVector& x);

LogicalVector charInSet(const CharacterVector& x, CharacterVector set);

IntegerVector repInt(const IntegerVector& values, const IntegerVector& times);

int firstMatch(const CharacterVector& x, std::string value);

NumericVector applyDirectionOfAlternative(const NumericVector& values, 
                                          bool directionUpper, 
                                          String type,
                                          String phase);

std::string getClassName(const Environment& x);

double getMultivarNormalDistribution(NumericVector upper, NumericMatrix sigma);

#endif
