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
 * File version: $Revision: 6416 $
 * Last changed: $Date: 2022-07-15 09:24:06 +0200 (Fr, 15 Jul 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>

using namespace Rcpp;

const double C_QNORM_EPSILON = 1.0e-100; // a value between 1e-323 and 1e-16
const double C_QNORM_MAXIMUM = -R::qnorm(C_QNORM_EPSILON, 0, 1, 1, 0);
const double C_QNORM_MINIMUM = -C_QNORM_MAXIMUM;
const double C_QNORM_THRESHOLD = floor(C_QNORM_MAXIMUM);
const double C_FUNCTION_ROOT_TOLERANCE_FACTOR = 100.0;

double getQNormEpsilon() {
	return C_QNORM_EPSILON;
}

double getQNormThreshold() {
	return C_QNORM_THRESHOLD;
}

double getQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = C_QNORM_EPSILON) {

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

double getOneMinusQNorm(double p, double mean = 0, double sd = 1,
		double lowerTail = 1, double logP = 0,
		double epsilon = C_QNORM_EPSILON) {

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
    	result = 1 - R::qnorm(p, mean, sd, lowerTail, logP);
    }

    if (result < -C_QNORM_THRESHOLD) {
    	result = C_QNORM_MINIMUM;
    }
    if (result > C_QNORM_THRESHOLD) {
    	result = C_QNORM_MAXIMUM;
    }

    return result;
}

std::string toString(const double i) {
    std::ostringstream ostr;
    ostr << i;
    return ostr.str();
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
 * Returns a string representing the given vector
 */
std::string vectorToString(NumericVector x) {
    if (x.length() == 0) return "[]";
    std::ostringstream os;
    os << "[";
    for (int i = 0; i < x.length(); i++) {
        os << x[i];
        if (i + 1 < x.length()) os << ", ";
    }
    os << "]";
    return os.str();
}

/**
 * Calculates root of function f in given interval using the secant method
 */
double secant(Function f, double x0, double x1, double min, double max, double tolerance, int maxIter) {
    int step = 1;
    double f0, f1, f2, x2;
    if (x0 > max || x1 > max || x0 < min || x1 < min) {
    	Rcout << "x0 or x1 not in bounds. Continuing with either bound as parameter instead.\n";
    }
    do {
        if (x0 < x1) {
            x2 = x0;
            x0 = x1;
            x1 = x2;
        }
        x0 = x0 < min ? min : x0;
        x1 = x1 > max ? max : x1;
        f0 = Rf_asReal(f(x0));
        f1 = Rf_asReal(f(x1));
        if (f0 == f1) {
            x2 = x0 + (x0 / 2.0);
            x2 = x2 < min ? min : x2 > max ? max : x2;
            f0 = Rf_asReal(f(x2));
        }
        x2 = x1 - f1 * (x1 - x0) / (f1 - f0);
        x2 = x2 < min ? min : x2 > max ? max : x2;
        f2 = Rf_asReal(f(x2));

        x0 = x1;
        f0 = f1;
        x1 = x2;
        f1 = f2;

        step++;
        if (step > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found.");
        }
    } while (std::abs(f2) > tolerance);
    return x2;
}

/**
 * Calculates root of function f in given interval using the secant method
 */
double secant(std::function<double(double)> f, double x0, double x1, double min, double max, double tolerance, int maxIter) {
    int step = 1;
    double f0, f1, f2, x2;
    if (x0 > max || x1 > max || x0 < min || x1 < min) {
    	Rcout << "x0 or x1 not in bounds. Continuing with either bound as parameter instead.\n";
    }
    do {
        if (x0 < x1) {
            x2 = x0;
            x0 = x1;
            x1 = x2;
        }
        x0 = x0 < min ? min : x0;
        x1 = x1 > max ? max : x1;
        f0 = f(x0);
        f1 = f(x1);
        if (f0 == f1) {
            x2 = x0 + (x0 / 2.0);
            x2 = x2 < min ? min : x2 > max ? max : x2;
            f0 = f(x2);
        }
        x2 = x1 - f1 * (x1 - x0) / (f1 - f0);
        x2 = x2 < min ? min : x2 > max ? max : x2;
        f2 = f(x2);

        x0 = x1;
        f0 = f1;
        x1 = x2;
        f1 = f2;

        step++;
        if (step > maxIter) {
            throw std::invalid_argument("No root within tolerance after given iterations found.");
        }
    } while (std::abs(f2) > tolerance);
    return x2;
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

double bizero(Function f, double lower, double upper, double tolerance, int maxIter) {
    return bizero([&](double x){return Rf_asReal(f(x));}, lower, upper, tolerance, maxIter);
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
