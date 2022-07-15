
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <cmath>
#include "f_utilities.h"

using namespace Rcpp;

int C_KMAX_UPPER_BOUND_FISHER = 6;
String C_FISHER_METHOD_USER_DEFINED_ALPHA = "userDefinedAlpha";
String C_FISHER_METHOD_EQUAL_ALPHA = "equalAlpha";
String C_FISHER_METHOD_FULL_ALPHA = "fullAlpha";
String C_FISHER_METHOD_NO_INTERACTION = "noInteraction";

bool isEqualCpp(double x, double y) {
    return std::abs(x - y) < 1e-10;
}

int getFisherCombinationCaseKmax2Cpp(NumericVector tVec) {
    return isEqualCpp((double) tVec[0], 1.0) ? 1 : 2;
}

double getFisherCombinationSizeKmax2Cpp(
		NumericVector alpha0Vec,
		NumericVector criticalValues, NumericVector tVec, double piValue,
		int caseKmax) {
    double a1 = alpha0Vec[0];
    double c1 = criticalValues[0];
    double c2 = criticalValues[1];
    double t2 = tVec[0];

    if (caseKmax == 1) {
        return piValue + c2 * (log(a1) - log(c1));
    } else {
        return piValue + pow(c2, (1 / t2)) * t2 / (t2 - 1) * (pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2)));
    }
}

double getFisherCombinationSizeKmax2Cpp(
		NumericVector alpha0Vec,
		NumericVector criticalValues, NumericVector tVec, double piValue) {
    return getFisherCombinationSizeKmax2Cpp(
		alpha0Vec, criticalValues, tVec, piValue, getFisherCombinationCaseKmax2Cpp(tVec));
}

double getFisherCombinationCaseKmax3Cpp(NumericVector tVec) {
    double t2 = tVec[0];
    double t3 = tVec[1];

    if (isEqualCpp(t2, 1) && isEqualCpp(t3, 1)) {
        return 1;
    } else if (!isEqualCpp(t2, t3) && !isEqualCpp(t2, 1) && !isEqualCpp(t3, 1)) {
        return 2;
    } else if (isEqualCpp(t2, t3) && !isEqualCpp(t2, 1)) {
        return 3;
    } else if (isEqualCpp(t2, 1) && !isEqualCpp(t3, 1)) {
        return 4;
    } else if (!isEqualCpp(t2, 1) && isEqualCpp(t3, 1)) {
        return 5;
    } else return -1;
}

double getFisherCombinationSizeKmax3Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues,
		NumericVector tVec, double piValue, int caseKmax) {

    double a1 = alpha0Vec[0];
    double a2 = alpha0Vec[1];
    double c1 = criticalValues[0];
    double c2 = criticalValues[1];
    double c3 = criticalValues[2];
    double t2 = tVec[0];
    double t3 = tVec[1];

    if (caseKmax == 1) {
        // Wassmer 1999, recursive formula
        return piValue + c3 * (log(a2) * log(a1) - log(a2) * log(c1) +
		   0.5 * pow((log(a1 / c2)), 2) - 0.5 * pow((log(c1 / c2)), 2));
    } else if (caseKmax == 2) {
        return piValue + pow(c3, (1 / t3)) * t3 / (t3 - t2) * (
			pow(a2, (1 - t2 / t3)) * t3 / (t3 - 1) * (pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) -
			pow(c2, (1 / t2 - 1 / t3)) * t2 / (t2 - 1) * (pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))));
    } else if (caseKmax == 3) {
        return piValue + pow(c3, (1 / t3)) * t3 / (t3 - 1) * (
			pow(a1, (1 - 1 / t3)) * (log(a2) - 1 / t2 * (log(c2) - log(a1) + t3 / (t3 - 1))) -
			pow(c1, (1 - 1 / t3)) * (log(a2) - 1 / t2 * (log(c2) - log(c1) + t3 / (t3 - 1))));
    } else if (caseKmax == 4) {
        return piValue + pow(c3, (1 / t3)) * t3 / (t3 - 1) *
			(pow(a2, (1 - 1 / t3)) * t3 / (t3 - 1) * (pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) -
			pow(c2, (1 - 1 / t3)) * (log(a1) - log(c1)));
    } else if (caseKmax == 5) {
        return piValue + c3 / (1 - t2) * (pow(a2, (1 - t2)) * (log(a1) - log(c1)) -
			pow(c2, (1 / t2 - 1)) * t2 / (t2 - 1) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))));
    } else return -1;
}

double getFisherCombinationSizeKmax3Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues, NumericVector tVec, double piValue) {
    return getFisherCombinationSizeKmax3Cpp(
		alpha0Vec, criticalValues, tVec, piValue, getFisherCombinationCaseKmax2Cpp(tVec));
}

double getFisherCombinationCaseKmax4Cpp(NumericVector tVec) {
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];
    return isEqualCpp(t2, 1) && isEqualCpp(t3, 1) && isEqualCpp(t4, 1) ? 1L : 2L;
}

double getFisherCombinationSizeApproximatelyKmax4Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues,
		NumericVector tVec, double piValue, int caseKmax) {

    double a1 = alpha0Vec[0];
    double a2 = alpha0Vec[1];
    double a3 = alpha0Vec[2];
    double c1 = criticalValues[0];
    double c2 = criticalValues[1];
    double c3 = criticalValues[2];
    double c4 = criticalValues[3];
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];

    // Wassmer 1999, recursive formula
    if (caseKmax == 1) {
        return (piValue + c4 *
			(1.0 / 6.0 * pow(log(a1 * a2 / c3), 3) - 1.0 / 6.0 * pow(log(c1 * a2 / c3), 3) +
			0.5 * pow(log(c2 / c3), 2) * log(c1) - 0.5 * pow(log(c2 / c3), 2) * log(a1) +
			0.5 * pow(log(a1 / c2), 2) * log(a3) - 0.5 * pow(log(c1 / c2), 2) * log(a3) +
			log(a3) * log(a2) * log(a1) - log(c1) * log(a2) * log(a3)));
    } else {
        //general case for K = 4
        double eps = 1e-05;
        if (isEqualCpp(t2, 1)) t2 += eps;
        if (isEqualCpp(t3, 1)) t3 += eps;
        if (isEqualCpp(t4, 1)) t4 += eps;
        if (isEqualCpp(t2, t3)) t3 += eps;
        if (isEqualCpp(t2, t4)) t4 += eps;
        if (isEqualCpp(t3, t4)) t4 += eps;

        return piValue + pow(c4, (1.0 / t4)) * t4 / (t4 - t3) * (
			t4 / (t4 - t2) * t4 / (t4 - 1.0) * pow(a3, (1.0 - t3 / t4)) * pow(a2, (1.0 - t2 / t4)) *
			(pow(a1, (1.0 - 1.0 / t4)) - pow(c1, (1.0 - 1.0 / t4))) -
			t4 / (t4 - t2) * t2 / (t2 - 1.0) * pow(a3, (1.0 - t3 / t4)) * pow(c2, (1.0 / t2 - 1.0 / t4)) *
			(pow(a1, (1.0 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))) -
			t3 / (t3 - t2) * t3 / (t3 - 1.0) * pow(c3, (1.0 / t3 - 1.0 / t4)) * pow(a2, (1.0 - t2 / t3)) *
			(pow(a1, (1.0 - 1.0 / t3)) - pow(c1, (1.0 - 1.0 / t3))) +
			t3 / (t3 - t2) * t2 / (t2 - 1.0) * pow(c3, (1.0 / t3 - 1.0 / t4)) * pow(c2, (1.0 / t2 - 1.0 / t3)) *
			(pow(a1, (1 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))));
    }
}

double getFisherCombinationSizeApproximatelyKmax4Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues, NumericVector tVec, double piValue) {
    return getFisherCombinationSizeApproximatelyKmax4Cpp(
		alpha0Vec, criticalValues, tVec, piValue, getFisherCombinationCaseKmax4Cpp(tVec));
}

double getFisherCombinationCaseKmax5Cpp(NumericVector tVec) {
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];
    double t5 = tVec[3];
    return isEqualCpp(t2, 1) && isEqualCpp(t3, 1) && isEqualCpp(t4, 1) && isEqualCpp(t5, 1) ? 1 : 2;
}

double getFisherCombinationSizeApproximatelyKmax5Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues,
		NumericVector tVec, double piValue, int caseKmax) {

    double a1 = alpha0Vec[0];
    double a2 = alpha0Vec[1];
    double a3 = alpha0Vec[2];
    double a4 = alpha0Vec[3];
    double c1 = criticalValues[0];
    double c2 = criticalValues[1];
    double c3 = criticalValues[2];
    double c4 = criticalValues[3];
    double c5 = criticalValues[4];
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];
    double t5 = tVec[3];

    // Wassmer 1999, recursive formula
    if (caseKmax == 1) {
        return piValue +
        	c5 * (1.0 / 24.0 * pow(log(a1 * a2 * a3 / c4), 4) - 1.0 / 24.0 * pow(log(c1 * a2 * a3 / c4), 4) +
			1.0 / 6.0 * pow(log(c2 * a3 / c4), 3) * log(c1) - 1.0 / 6.0 * pow(log(c2 * a3 / c4), 3) * log(a1) +
			1.0 / 4.0 * pow(log(c3 / c4), 2) * pow(log(c1 / c2), 2) -
			1.0 / 4.0 * pow(log(c3 / c4), 2) * pow(log(a1 / c2), 2) +
			0.5 * pow(log(c3 / c4), 2) * log(a2) * log(c1) - 0.5 * pow(log(c3 / c4), 2) * log(a2) * log(a1) +
			1.0 / 6.0 * pow(log(a1 * a2 / c3), 3) * log(a4) - 1.0 / 6.0 * pow(log(c1 * a2 / c3), 3) * log(a4) +
			0.5 * pow(log(c2 / c3), 2) * log(a4) * log(c1) - 0.5 * pow(log(c2 / c3), 2) * log(a4) * log(a1) +
			0.5 * pow(log(a1 / c2), 2) * log(a3) * log(a4) - 0.5 * pow(log(c1 / c2), 2) * log(a3) * log(a4) +
			log(a4) * log(a3) * log(a2) * log(a1) - log(c1) * log(a2) * log(a3) * log(a4));
    } else {
        //general case for K = 5
        double eps = 1e-05;
        if (isEqualCpp(t2, 1)) t2 = t2 + eps;
        if (isEqualCpp(t3, 1)) t3 = t3 + eps;
        if (isEqualCpp(t4, 1)) t4 = t4 + eps;
        if (isEqualCpp(t5, 1)) t5 = t5 + eps;
        if (isEqualCpp(t2, t3)) t3 = t2 + eps;
        if (isEqualCpp(t2, t4)) t4 = t2 + eps;
        if (isEqualCpp(t2, t5)) t5 = t2 + eps;
        if (isEqualCpp(t3, t4)) t4 = t3 + eps;
        if (isEqualCpp(t3, t5)) t5 = t3 + eps;
        if (isEqualCpp(t4, t5)) t5 = t4 + eps;

        return piValue + pow(c5, (1.0 / t5)) * t5 / (t5 - t4) * (
			t5 / (t5 - t3) * t5 / (t5 - t2) * t5 / (t5 - 1.0) * pow(a4, (1.0 - t4 / t5)) *
			pow(a3, (1.0 - t3 / t5)) * pow(a2, (1.0 - t2 / t5)) *
			(pow(a1, (1.0 - 1.0 / t5)) - pow(c1, (1.0 - 1.0 / t5))) -
			t5 / (t5 - t3) * t5 / (t5 - t2) * t2 / (t2 - 1.0) * pow(a4, (1.0 - t4 / t5)) *
			pow(a3, (1.0 - t3 / t5)) * pow(c2, (1.0 / t2 - 1 / t5)) *
			(pow(a1, (1.0 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))) -
			t5 / (t5 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1.0) * pow(a4, (1.0 - t4 / t5)) *
			pow(c3, (1.0 / t3 - 1 / t5)) * pow(a2, (1.0 - t2 / t3)) *
			(pow(a1, (1.0 - 1.0 / t3)) - pow(c1, (1.0 - 1.0 / t3))) +
			t5 / (t5 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1.0) * pow(a4, (1.0 - t4 / t5)) *
			pow(c3, (1.0 / t3 - 1 / t5)) * pow(c2, (1.0 / t2 - 1 / t3)) *
			(pow(a1, (1.0 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))) -
			t4 / (t4 - t3) * t4 / (t4 - t2) * t4 / (t4 - 1.0) * pow(c4, (1.0 / t4 - 1.0 / t5)) *
			pow(a3, (1.0 - t3 / t4)) * pow(a2, (1.0 - t2 / t4)) *
			(pow(a1, (1.0 - 1.0 / t4)) - pow(c1, (1.0 - 1.0 / t4))) +
			t4 / (t4 - t3) * t4 / (t4 - t2) * t2 / (t2 - 1.0) * pow(c4, (1.0 / t4 - 1.0 / t5)) *
			pow(a3, (1.0 - t3 / t4)) * pow(c2, (1.0 / t2 - 1 / t4)) *
			(pow(a1, (1.0 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))) +
			t4 / (t4 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1.0) * pow(c4, (1.0 / t4 - 1.0 / t5)) *
			pow(c3, (1.0 / t3 - 1 / t4)) * pow(a2, (1.0 - t2 / t3)) *
			(pow(a1, (1.0 - 1.0 / t3)) - pow(c1, (1.0 - 1.0 / t3))) -
			t4 / (t4 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1.0) * pow(c4, (1.0 / t4 - 1.0 / t5)) *
			pow(c3, (1.0 / t3 - 1 / t4)) * pow(c2, (1.0 / t2 - 1 / t3)) *
			(pow(a1, (1.0 - 1.0 / t2)) - pow(c1, (1.0 - 1.0 / t2))));
    }
}

double getFisherCombinationSizeApproximatelyKmax5Cpp(
		NumericVector alpha0Vec, NumericVector criticalValues, NumericVector tVec, double piValue) {
    return getFisherCombinationSizeApproximatelyKmax5Cpp(
            alpha0Vec, criticalValues, tVec, piValue, getFisherCombinationCaseKmax5Cpp(tVec));
}

double getFisherCombinationCaseKmax6Cpp(NumericVector tVec) {
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];
    double t5 = tVec[3];
    double t6 = tVec[4];
    return isEqualCpp(t2, 1) && isEqualCpp(t3, 1) && isEqualCpp(t4, 1) && isEqualCpp(t5, 1) && isEqualCpp(t6, 1)
		? 1 : 2;
}

double getFisherCombinationSizeApproximatelyKmax6Cpp(
		NumericVector alpha0Vec,
	 NumericVector criticalValues, NumericVector tVec, double piValue,
	 int caseKmax) {
    double a1 = alpha0Vec[0];
    double a2 = alpha0Vec[1];
    double a3 = alpha0Vec[2];
    double a4 = alpha0Vec[3];
    double a5 = alpha0Vec[4];
    double c1 = criticalValues[0];
    double c2 = criticalValues[1];
    double c3 = criticalValues[2];
    double c4 = criticalValues[3];
    double c5 = criticalValues[4];
    double c6 = criticalValues[5];
    double t2 = tVec[0];
    double t3 = tVec[1];
    double t4 = tVec[2];
    double t5 = tVec[3];
    double t6 = tVec[4];

    // Wassmer 1999, recursive formula
    if (caseKmax == 1) {
        return piValue + c6 * (
			log(a1) * log(a2) * log(a3) * log(a4) * log(a5) +
			1.0 / 24.0 * pow(log(a1 * a2 * a3 / c4), 4) * log(a5) +
			1.0 / 120.0 * pow(log(a1 * a2 * a3 * a4 / c5), 5) -
			0.5 * pow(log(c4 / c5), 2) * log(a3) * log(a2) * log(a1) +
			1.0 / 6.0 * pow(log(a1 * a2 / c3), 3) * log(a4) * log(a5) -
			0.5 * pow(log(c3 / c4), 2) * log(a5) * log(a2) * log(a1) -
			1.0 / 6.0 * pow(log(c3 * a4 / c5), 3) * log(a2) * log(a1) -
			1.0 / 12.0 * pow(log(a1 * a2 / c3), 3) * pow(log(c4 / c5), 2) +
			0.5 * pow(log(a1 / c2), 2) * log(a3) * log(a4) * log(a5) -
			1.0 / 6.0 * pow(log(c2 * a3 / c4), 3) * log(a5) * log(a1) -
			1.0 / 24.0 * pow(log(c2 * a3 * a4 / c5), 4) * log(a1) -
			1.0 / 4.0 * pow(log(c4 / c5), 2) * log(a3) * pow(log(a1 / c2), 2) -
			0.5 * pow(log(c2 / c3), 2) * log(a4) * log(a5) * log(a1) -
			1.0 / 4.0 * pow(log(c3 / c4), 2) * log(a5) * pow(log(a1 / c2), 2) -
			1.0 / 12.0 * pow(log(c3 * a4 / c5), 3) * pow(log(a1 / c2), 2) +
			1.0 / 4.0 * pow(log(c2 / c3), 2) * pow(log(c4 / c5), 2) * log(a1) -
			log(c1) * log(a2) * log(a3) * log(a4) * log(a5) -
			1.0 / 24.0 * pow(log(c1 * a2 * a3 / c4), 4) * log(a5) -
			1.0 / 120.0 * pow(log(c1 * a2 * a3 * a4 / c5), 5) +
			0.5 * pow(log(c4 / c5), 2) * log(a3) * log(a2) * log(c1) -
			1.0 / 6.0 * pow(log(c1 * a2 / c3), 3) * log(a4) * log(a5) +
			0.5 * pow(log(c3 / c4), 2) * log(a5) * log(a2) * log(c1) +
			1.0 / 6.0 * pow(log(c3 * a4 / c5), 3) * log(a2) * log(c1) +
			1.0 / 12.0 * pow(log(c1 * a2 / c3), 3) * pow(log(c4 / c5), 2) -
			0.5 * pow(log(c1 / c2), 2) * log(a3) * log(a4) * log(a5) +
			1.0 / 6.0 * pow(log(c2 * a3 / c4), 3) * log(a5) * log(c1) +
			1.0 / 24.0 * pow(log(c2 * a3 * a4 / c5), 4) * log(c1) +
			1.0 / 4.0 * pow(log(c4 / c5), 2) * log(a3) * pow(log(c1 / c2), 2) +
			0.5 * pow(log(c2 / c3), 2) * log(a4) * log(a5) * log(c1) +
			1.0 / 4.0 * pow(log(c3 / c4), 2) * log(a5) * pow(log(c1 / c2), 2) +
			1.0 / 12.0 * pow(log(c3 * a4 / c5), 3) * pow(log(c1 / c2), 2) -
			1.0 / 4.0 * pow(log(c2 / c3), 2) * pow(log(c4 / c5), 2) * log(c1));
    } else {
        //general case for K = 6
        double eps = 1e-04;
        if (isEqualCpp(t2, 1)) t2 = t2 + eps;
        if (isEqualCpp(t3, 1)) t3 = t3 + eps;
        if (isEqualCpp(t4, 1)) t4 = t4 + eps;
        if (isEqualCpp(t5, 1)) t5 = t5 + eps;
        if (isEqualCpp(t6, 1)) t6 = t6 + eps;
        if (isEqualCpp(t2, t3)) t3 = t2 + eps;
        if (isEqualCpp(t2, t4)) t4 = t2 + eps;
        if (isEqualCpp(t2, t5)) t5 = t2 + eps;
        if (isEqualCpp(t2, t6)) t6 = t2 + eps;
        if (isEqualCpp(t3, t4)) t4 = t3 + eps;
        if (isEqualCpp(t3, t5)) t5 = t3 + eps;
        if (isEqualCpp(t3, t6)) t6 = t3 + eps;
        if (isEqualCpp(t4, t5)) t5 = t4 + eps;
        if (isEqualCpp(t4, t6)) t6 = t4 + eps;
        if (isEqualCpp(t5, t6)) t6 = t5 + eps;

        return piValue + pow(c6, (1 / t6)) * t6 / (t6 - t5) * (
			t6 / (t6 - t4) * t6 / (t6 - t3) * t6 / (t6 - t2) * t6 / (t6 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(a4, (1 - t4 / t6)) * pow(a3, (1 - t3 / t6)) * pow(a2, (1 - t2 / t6)) *
			(pow(a1, (1 - 1 / t6)) - pow(c1, (1 - 1 / t6))) -
			t6 / (t6 - t4) * t6 / (t6 - t3) * t6 / (t6 - t2) * t2 / (t2 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(a4, (1 - t4 / t6)) * pow(a3, (1 - t3 / t6)) * pow(c2, (1 / t2 - 1 / t6)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) -
			t6 / (t6 - t4) * t6 / (t6 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(a4, (1 - t4 / t6)) * pow(c3, (1 / t3 - 1 / t6)) * pow(a2, (1 - t2 / t3)) *
			(pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) +
			t6 / (t6 - t4) * t6 / (t6 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(a4, (1 - t4 / t6)) * pow(c3, (1 / t3 - 1 / t6)) * pow(c2, (1 / t2 - 1 / t3)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) -
			t6 / (t6 - t4) * t4 / (t4 - t3) * t4 / (t4 - t2) * t4 / (t4 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(c4, (1 / t4 - 1 / t6)) * pow(a3, (1 - t3 / t4)) * pow(a2, (1 - t2 / t4)) *
			(pow(a1, (1 - 1 / t4)) - pow(c1, (1 - 1 / t4))) +
			t6 / (t6 - t4) * t4 / (t4 - t3) * t4 / (t4 - t2) * t2 / (t2 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(c4, (1 / t4 - 1 / t6)) * pow(a3, (1 - t3 / t4)) * pow(c2, (1 / t2 - 1 / t4)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) +
			t6 / (t6 - t4) * t4 / (t4 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(c4, (1 / t4 - 1 / t6)) * pow(c3, (1 / t3 - 1 / t4)) * pow(a2, (1 - t2 / t3)) *
			(pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) -
			t6 / (t6 - t4) * t4 / (t4 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1) * pow(a5, (1 - t5 / t6)) *
			pow(c4, (1 / t4 - 1 / t6)) * pow(c3, (1 / t3 - 1 / t4)) * pow(c2, (1 / t2 - 1 / t3)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) -
			t5 / (t5 - t4) * t5 / (t5 - t3) * t5 / (t5 - t2) * t5 / (t5 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(a4, (1 - t4 / t5)) * pow(a3, (1 - t3 / t5)) * pow(a2, (1 - t2 / t5)) *
			(pow(a1, (1 - 1 / t5)) - pow(c1, (1 - 1 / t5))) +
			t5 / (t5 - t4) * t5 / (t5 - t3) * t5 / (t5 - t2) * t2 / (t2 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(a4, (1 - t4 / t5)) * pow(a3, (1 - t3 / t5)) * pow(c2, (1 / t2 - 1 / t5)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) +
			t5 / (t5 - t4) * t5 / (t5 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(a4, (1 - t4 / t5)) * pow(c3, (1 / t3 - 1 / t5)) * pow(a2, (1 - t2 / t3)) *
			(pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) -
			t5 / (t5 - t4) * t5 / (t5 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(a4, (1 - t4 / t5)) * pow(c3, (1 / t3 - 1 / t5)) * pow(c2, (1 / t2 - 1 / t3)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) +
			t5 / (t5 - t4) * t4 / (t4 - t3) * t4 / (t4 - t2) * t4 / (t4 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(c4, (1 / t4 - 1 / t5)) * pow(a3, (1 - t3 / t4)) * pow(a2, (1 - t2 / t4)) *
			(pow(a1, (1 - 1 / t4)) - pow(c1, (1 - 1 / t4))) -
			t5 / (t5 - t4) * t4 / (t4 - t3) * t4 / (t4 - t2) * t2 / (t2 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(c4, (1 / t4 - 1 / t5)) * pow(a3, (1 - t3 / t4)) * pow(c2, (1 / t2 - 1 / t4)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))) -
			t5 / (t5 - t4) * t4 / (t4 - t3) * t3 / (t3 - t2) * t3 / (t3 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(c4, (1 / t4 - 1 / t5)) * pow(c3, (1 / t3 - 1 / t4)) * pow(a2, (1 - t2 / t3)) *
			(pow(a1, (1 - 1 / t3)) - pow(c1, (1 - 1 / t3))) +
			t5 / (t5 - t4) * t4 / (t4 - t3) * t3 / (t3 - t2) * t2 / (t2 - 1) * pow(c5, (1 / t5 - 1 / t6)) *
			pow(c4, (1 / t4 - 1 / t5)) * pow(c3, (1 / t3 - 1 / t4)) * pow(c2, (1 / t2 - 1 / t3)) *
			(pow(a1, (1 - 1 / t2)) - pow(c1, (1 - 1 / t2))));
    }
}

double getFisherCombinationSizeApproximatelyKmax6Cpp(NumericVector alpha0Vec,
                                                     NumericVector criticalValues, NumericVector tVec, double piValue) {
    return getFisherCombinationSizeApproximatelyKmax6Cpp(
            alpha0Vec, criticalValues, tVec, piValue, getFisherCombinationCaseKmax6Cpp(tVec));
}

// [[Rcpp::export]]
double getFisherCombinationSizeCpp(double kMax, NumericVector alpha0Vec,
	   NumericVector criticalValues, NumericVector tVec, NumericVector cases) {
    if (criticalValues.length() < 1 || criticalValues.length() > C_KMAX_UPPER_BOUND_FISHER) {
        stop("length of 'criticalValues' (%d) is out of bounds [1; %d]",
             criticalValues.length(), C_KMAX_UPPER_BOUND_FISHER);
    }
    double piValue = criticalValues[0];
    if (kMax > 1) {
        piValue = getFisherCombinationSizeKmax2Cpp(alpha0Vec, criticalValues, tVec, piValue, (int) cases[0]);
    }
    if (kMax > 2) {
        piValue = getFisherCombinationSizeKmax3Cpp(alpha0Vec, criticalValues, tVec, piValue, (int) cases[1]);
    }
    if (kMax > 3) {
        piValue = getFisherCombinationSizeApproximatelyKmax4Cpp(alpha0Vec, criticalValues, tVec, piValue, (int) cases[2]);
    }
    if (kMax > 4) {
        piValue = getFisherCombinationSizeApproximatelyKmax5Cpp(alpha0Vec, criticalValues, tVec, piValue, (int) cases[3]);
    }
    if (kMax > 5) {
        piValue = getFisherCombinationSizeApproximatelyKmax6Cpp(alpha0Vec, criticalValues, tVec, piValue, (int) cases[4]);
    }
    return piValue;
}

int getRejectValueForOneTrialCpp(int kMax, NumericVector alpha0,
		 NumericVector criticalValues, NumericVector weightsFisher, int stage,
		 NumericVector pValues) {

    if (stage < kMax && pValues[stage - 1] >= alpha0[stage - 1]) {
        return 0;
    }

    double p = 1;
    for (int i = 0; i < stage; i++) {
        p *= pow((double) pValues[i], (double) weightsFisher[i]);
    }
    return p < criticalValues[stage - 1] ? 1 : -1;
}

// [[Rcpp::export]]
double getSimulatedAlphaCpp(int kMax, NumericVector alpha0,
		NumericVector criticalValues, NumericVector tVec, int iterations) {

    NumericVector weightsFisher = clone(tVec);
    weightsFisher.push_front(1);

    double var = 0;
    for (int i = 0; i < iterations; i++) {
        NumericVector pValues = runif(kMax);
		int rejectValue = 0;
        for (int stage = 1; stage <= kMax; stage++) {
            rejectValue = getRejectValueForOneTrialCpp(
				kMax,
				alpha0,
				criticalValues,
				weightsFisher,
				stage,
				pValues);
            if (rejectValue >= 0) {
                break;
            }
        }
		if (rejectValue > 0) {
        	var += rejectValue;
		}
    }

    return var / iterations;
}

// [[Rcpp::export]]
NumericVector getFisherCombinationCasesCpp(int kMax, NumericVector tVec) {
    if (kMax == 1) {
        return NumericVector(0);
    }

    NumericVector cases = {};
    if (kMax > 1) {
        cases.push_back(getFisherCombinationCaseKmax2Cpp(tVec));
    }
    if (kMax > 2) {
        cases.push_back(getFisherCombinationCaseKmax3Cpp(tVec));
    }
    if (kMax > 3) {
        cases.push_back(getFisherCombinationCaseKmax4Cpp(tVec));
    }
    if (kMax > 4) {
        cases.push_back(getFisherCombinationCaseKmax5Cpp(tVec));
    }
    if (kMax > 5) {
        cases.push_back(getFisherCombinationCaseKmax6Cpp(tVec));
    }
    return cases;
}

double getFisherCombinationSizeCpp(double kMax, NumericVector alpha0Vec,
		NumericVector criticalValues, NumericVector tVec) {
    return getFisherCombinationSizeCpp(kMax, alpha0Vec, criticalValues, tVec, getFisherCombinationCasesCpp(kMax, tVec));
}

// [[Rcpp::export]]
List getDesignFisherTryCpp(int kMax, double alpha, double tolerance,
		NumericVector criticalValues, NumericVector scale,
		NumericVector alpha0Vec, NumericVector userAlphaSpending, String method) {

    NumericVector cases = getFisherCombinationCasesCpp(kMax, scale);
    NumericVector alphaSpent(kMax);
    NumericVector stageLevels(kMax);
    bool nonStochasticCurtailment;

    double size = 0;
    if (method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
        criticalValues[0] = userAlphaSpending[0];
        alphaSpent = clone(criticalValues);
        if (kMax > 1) {
            for (int k = 2; k <= kMax; k++) {
                double cLower = 0;
                double cUpper = alpha;
                double prec = 1;
                while (prec > tolerance) {
                    double alpha1 = (cLower + cUpper) * 0.5;
                    criticalValues[k - 1] = alpha1;
                    size = getFisherCombinationSizeCpp(
						k, rangeVector(alpha0Vec, 0, k - 2), criticalValues, scale, cases);
                    if (size < userAlphaSpending[k - 1]) {
                        cLower = alpha1;
                    } else {
                        cUpper = alpha1;
                    }
                    prec = cUpper - cLower;
                }
            }
        }
    } else {
        double prec = 1;
        double cLower = 0;
        double cUpper = alpha;
        double maxIter = 100;
        while (prec > tolerance && maxIter >= 0) {
            double alpha1 = (cLower + cUpper) * 0.5;
            if (method == C_FISHER_METHOD_EQUAL_ALPHA) {
                criticalValues = sapply(seq_len(kMax), [=](int k) {
                    return zeroin([&](double c) {
                        return getFisherCombinationSizeCpp(
                                k, rep(1.0, k - 1), rep(c, k), scale, cases) - alpha1;
                    }, tolerance, alpha, tolerance, 1000);
                });
            } else if (method == C_FISHER_METHOD_FULL_ALPHA) {
                for (int k = 0; k < kMax - 1; k++) {
                    double prec2 = 1;
                    double cLower2 = 0;
                    double cUpper2 = alpha;
                    double c = 0, y;
                    while (prec2 > tolerance) {
                        c = (cLower2 + cUpper2) * 0.5;
                        y = getFisherCombinationSizeCpp(k + 1, rep(1.0, k), rep(c, k + 1), scale, cases);
                        if (y < alpha1) {
                            cLower2 = c;
                        } else {
                            cUpper2 = c;
                        }
                        prec2 = cUpper2 - cLower2;
                    }
                    criticalValues[k] = c;
                }
                criticalValues[kMax - 1] = zeroin([&](double c) {
                    return getFisherCombinationSizeCpp(kMax, rep(1.0, kMax - 1), rep(c, kMax), scale, cases) - alpha;
                }, tolerance, alpha, tolerance, 1000);
            } else if (method == C_FISHER_METHOD_NO_INTERACTION) {
                criticalValues[kMax - 1] = zeroin([&](double c) {
                    return getFisherCombinationSizeCpp(kMax, rep(1.0, kMax - 1), rep(c, kMax), scale, cases) - alpha;
                }, tolerance, alpha, tolerance, 1000);
                criticalValues[0] = alpha1;
                if (kMax < 2) Rcout << "error: kMax < 2";
                for (int k = kMax - 1; k >= 2; k--) {
                    criticalValues[k - 1] = criticalValues[k] / pow((double) alpha0Vec[k - 1], 1 / scale[k - 1]);
                }
            } else {
                throw std::invalid_argument("method in use is unkown. Use a valid method instead.");
            }
            size = getFisherCombinationSizeCpp(kMax, alpha0Vec, criticalValues, scale, cases);
            if (size < alpha) {
                cLower = alpha1;
            } else {
                cUpper = alpha1;
            }
            prec = cUpper - cLower;
            maxIter--;
        }
    }

    for (int k = 1; k <= kMax; k++) {
        stageLevels[k - 1] = getFisherCombinationSizeCpp(
			k, rep(1.0, k - 1), rep((double) criticalValues[k - 1], k), scale, cases);
        alphaSpent[k - 1] = getFisherCombinationSizeCpp(
			k, rangeVector(alpha0Vec, 0, k - 2), rangeVector(criticalValues, 0, k - 1), scale, cases);
    }

    nonStochasticCurtailment = stageLevels[0] < 1e-10;
    if (nonStochasticCurtailment) {
        for (int k = 1; k <= kMax; k++) {
            stageLevels[k - 1] = getFisherCombinationSizeCpp(
				k, rep(1.0, k - 1),
                rep((double) criticalValues[k - 1], k), scale, cases);
            alphaSpent[k - 1] = getFisherCombinationSizeCpp(
            	k, rangeVector(alpha0Vec, 0, k - 2),
                rangeVector(criticalValues, 0, k - 1), scale, cases);
        }
    }
    return List::create(
		_["criticalValues"] = criticalValues,
		_["alphaSpent"] = alphaSpent,
		_["stageLevels"] = stageLevels,
		_["nonStochasticCurtailment"] = nonStochasticCurtailment,
        _["size"] = size);
}


