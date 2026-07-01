/**
 *
 * -- Simulation of count data --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Tobias Muetze, PhD, Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
 * Licensed under "GNU Lesser General Public License" version 3
 * License text can be found here: https://www.r-project.org/Licenses/LGPL-3
 *
 * RPACT company website: https://www.rpact.com
 * rpact package website: https://www.rpact.org
 *
 * Contact us for information about our services: info@rpact.com
 *
 */

#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

double loglikeli_nb(double log_rate1, double log_rate2, double shape, NumericVector counts1, NumericVector counts2,
	NumericVector t1, NumericVector t2) {

	double log_l = 0;
	int n1 = counts1.size();
	int n2 = counts2.size();
	double rate1 = exp(log_rate1);
	double rate2 = exp(log_rate2);

	// likelihood from group 1
	for (int i = 0; i < n1; i++) {
		log_l += counts1[i] * log(shape * t1[i] * rate1) - (counts1[i] + 1 / shape) * log(1 + shape * t1[i] * rate1);
		for (int j = 0; j < counts1[i]; j++) {
			log_l += log(j + 1 / shape);
		}
	}

	// likelihood from group 2
	for (int i = 0; i < n2; i++) {
		log_l += counts2[i] * log(shape * t2[i] * rate2) - (counts2[i] + 1 / shape) * log(1 + shape * t2[i] * rate2);
		for (int j = 0; j < counts2[i]; j++) {
			log_l += log(j + 1 / shape);
		}
	}

	return log_l;
}

SEXP gradient(double log_rate1, double log_rate2, double shape, NumericVector counts1, NumericVector counts2,
	NumericVector t1, NumericVector t2) {
	double grad_rate1 = 0;
	double grad_rate2 = 0;
	double grad_shape = 0;
	int n1 = counts1.size();
	int n2 = counts2.size();
	double rate1 = exp(log_rate1);
	double rate2 = exp(log_rate2);

	NumericVector output(3);

	// Gradient of rate1
	for (int i = 0; i < n1; i++) {
		grad_rate1 += (counts1[i] - t1[i] * rate1) / (1 + shape * t1[i] * rate1);
	}

	// Gradient of rate2
	for (int i = 0; i < n2; i++) {
		grad_rate2 += (counts2[i] - t2[i] * rate2) / (1 + shape * t2[i] * rate2);
	}

	// Gradient of shape
	// Add part from group 1
	for (int i = 0; i < n1; i++) {
		grad_shape += counts1[i] / shape + log(1 + shape * t1[i] * rate1) / pow(shape, 2)
			- (counts1[i] * shape + 1) * t1[i] * rate1 / (shape + pow(shape, 2) * t1[i] * rate1);
		for (int j = 0; j < counts1[i]; j++) {
			grad_shape += -1 / (shape + j * pow(shape, 2));
		}
	}
	// Add part from group 2
	for (int i = 0; i < n2; i++) {
		grad_shape += counts2[i] / shape + log(1 + shape * t2[i] * rate2) / pow(shape, 2)
			- (counts2[i] * shape + 1) * t2[i] * rate2 / (shape + pow(shape, 2) * t2[i] * rate2);
		for (int j = 0; j < counts2[i]; j++) {
			grad_shape += -1 / (shape + j * pow(shape, 2));
		}
	}

	output[0] = grad_rate1;
	output[1] = grad_rate2;
	output[2] = grad_shape;

	return output;
}

double find_shape_start(NumericVector counts1, NumericVector counts2, NumericVector t1, NumericVector t2) {

	double sum_count, sum_t;
	int n1 = counts1.size();
	int n2 = counts2.size();
	double log_rate1, log_rate2;
	double shape, log_l0, log_l1;
	double shape_low, shape_up, shape_mid;
	double grad_low, grad_up, grad_mid;
	int iterate_count = 0;
	NumericVector grad(3);

	// Starting values of rates are the method of moments estimators
	sum_count = sum_t = 0;
	for (int i = 0; i < n1; i++) {
		sum_count += counts1[i];
		sum_t += t1[i];
	}
	log_rate1 = log(sum_count / sum_t);

	sum_count = sum_t = 0;
	for (int i = 0; i < n2; i++) {
		sum_count += counts2[i];
		sum_t += t2[i];
	}
	log_rate2 = log(sum_count / sum_t);

	// Check if shape<1e-6, i.e. shape=0
	log_l0 = loglikeli_nb(log_rate1, log_rate2, shape = pow(10, -7), counts1, counts2, t1, t2);
	log_l1 = loglikeli_nb(log_rate1, log_rate2, shape = pow(10, -6), counts1, counts2, t1, t2);
	if (log_l1 < log_l0) {
		return (0);
	}

	shape_low = 0.0001;
	shape_up = 50;
	grad = gradient(log_rate1, log_rate2, shape = shape_low, counts1, counts2, t1, t2);
	grad_low = grad(2);
	grad = gradient(log_rate1, log_rate2, shape = shape_up, counts1, counts2, t1, t2);
	grad_up = grad(2);

	if (grad_low < 0 && grad_up < 0) {
		return (shape_low);
	}
	if (grad_low > 0 && grad_up > 0) {
		return (shape_up);
	}

	shape_mid = -1;
	while ((iterate_count < 10) && (shape_up / shape_low > 1.2)) {

		shape_mid = (shape_low + shape_up) / 2;
		grad = gradient(log_rate1, log_rate2, shape = shape_mid, counts1, counts2, t1, t2);
		grad_mid = grad(2);

		if ((grad_low < 0 && grad_mid < 0 && grad_up > 0) || (grad_low > 0 && grad_mid > 0 && grad_up < 0)) {
			shape_low = shape_mid;
			grad_low = grad_mid;
		}

		if ((grad_low < 0 && grad_mid > 0 && grad_up > 0) || (grad_low > 0 && grad_mid < 0 && grad_up < 0)) {
			shape_up = shape_mid;
			grad_up = grad_mid;
		}

		iterate_count++;
	}

	return (shape_mid);
}

SEXP inverse_hessian(double log_rate1, double log_rate2, double shape, NumericVector counts1, NumericVector counts2,
	NumericVector t1, NumericVector t2) {
	int n1 = counts1.size();
	int n2 = counts2.size();
	double rate1 = exp(log_rate1);
	double rate2 = exp(log_rate2);
	double determinant;
	double hess_r1, hess_r1shape, hess_r2, hess_r2shape, hess_shape;
	hess_r1 = hess_r1shape = hess_r2 = hess_r2shape = hess_shape = 0;

	NumericMatrix output(3, 3);

	// sec deriv with respect to rate1
	for (int j = 0; j < n1; j++) {
		hess_r1 += -(t1[j] * rate1 * (1 + shape * counts1[j])) / pow(1 + shape * t1[j] * rate1, 2);
	}

	// sec deriv with respect to rate2
	for (int j = 0; j < n2; j++) {
		hess_r2 += -(t2[j] * rate2 * (1 + shape * counts2[j])) / pow(1 + shape * t2[j] * rate2, 2);
	}

	// deriv with respect to rate1 and shape
	for (int j = 0; j < n1; j++) {
		hess_r1shape += -(t1[j] * rate1 * (counts1[j] - t1[j] * rate1)) / pow(1 + shape * t1[j] * rate1, 2);
	}

	// deriv with respect to rate2 and shape
	for (int j = 0; j < n2; j++) {
		hess_r2shape += -(t2[j] * rate2 * (counts2[j] - t2[j] * rate2)) / pow(1 + shape * t2[j] * rate2, 2);
	}

	// second deriv with respect to shape
	// Add part from group 1
	for (int j = 0; j < n1; j++) {
		hess_shape += -counts1[j] / pow(shape, 2) + t1[j] * rate1 / (pow(shape, 2) * (t1[j] * rate1 * shape + 1))
			- 2 * log(1 + t1[j] * rate1 * shape) / pow(shape, 3)
			+ (t1[j] * rate1 * (t1[j] * rate1 * shape * (counts1[j] * shape + 2) + 1))
				/ pow(shape * (t1[j] * rate1 * shape + 1), 2);
		for (int l = 0; l < counts1[j]; l++) {
			hess_shape += (2 * l * shape + 1) / pow(shape + l * pow(shape, 2), 2);
		}
	}
	// Add part from group 2
	for (int j = 0; j < n2; j++) {
		hess_shape += -counts2[j] / pow(shape, 2) + t2[j] * rate2 / (pow(shape, 2) * (t2[j] * rate2 * shape + 1))
			- 2 * log(1 + t2[j] * rate2 * shape) / pow(shape, 3)
			+ (t2[j] * rate2 * (t2[j] * rate2 * shape * (counts2[j] * shape + 2) + 1))
				/ pow(shape * (t2[j] * rate2 * shape + 1), 2);
		for (int l = 0; l < counts2[j]; l++) {
			hess_shape += (2 * l * shape + 1) / pow(shape + l * pow(shape, 2), 2);
		}
	}

	// calculating the inverse hessian
	determinant = hess_r1 * (hess_r2 * hess_shape - pow(hess_r2shape, 2)) + hess_r1shape * (-hess_r2 * hess_r1shape);

	output(0, 0) = (hess_r2 * hess_shape - pow(hess_r2shape, 2)) / determinant;
	output(1, 1) = (hess_r1 * hess_shape - pow(hess_r1shape, 2)) / determinant;
	output(2, 2) = hess_r1 * hess_r2 / determinant;
	output(0, 1) = output(1, 0) = hess_r2shape * hess_r1shape / determinant;
	output(0, 2) = output(2, 0) = -hess_r2 * hess_r1shape / determinant;
	output(1, 2) = output(2, 1) = -hess_r1 * hess_r2shape / determinant;

	return output;
}

// [[Rcpp::export(name = ".getNegativeBinomialEstimates")]]
SEXP estimate_nb(NumericVector counts1, NumericVector counts2, NumericVector t1, NumericVector t2) {

	double log_rate1, log_rate2, shape;
	double log_rate1_updated, log_rate2_updated, shape_updated;
	int n1 = counts1.size();
	int n2 = counts2.size();
	double sum_count1, sum_t1, sum_count2, sum_t2;
	int step_count = 0;
	double eps = pow(10, -5);
	double step_size = 1;

	NumericMatrix inv_hess(3, 3);
	NumericVector grad(3);
	NumericVector estimates(3);

	// Starting values of rates are the method of moments estimators
	sum_count1 = sum_t1 = 0;
	for (int i = 0; i < n1; i++) {
		sum_count1 += counts1[i];
		sum_t1 += t1[i];
	}
	log_rate1 = log(sum_count1 / sum_t1);

	sum_count2 = sum_t2 = 0;
	for (int i = 0; i < n2; i++) {
		sum_count2 += counts2[i];
		sum_t2 += t2[i];
	}
	log_rate2 = log(sum_count2 / sum_t2);

	shape = find_shape_start(counts1, counts2, t1, t2);
	if (shape == 0) {
		estimates(0) = exp(log_rate1);
		estimates(1) = exp(log_rate2);
		estimates(2) = shape;
		return (estimates);
	}

	while (step_count++ <= 10) {
		inv_hess = inverse_hessian(log_rate1, log_rate2, shape, counts1, counts2, t1, t2);
		grad = gradient(log_rate1, log_rate2, shape, counts1, counts2, t1, t2);

		// Adjust step size if shape would become negative
		shape_updated = shape
			- step_size * (inv_hess(2, 0) * grad(0) + inv_hess(2, 1) * grad(1) + inv_hess(2, 2) * grad(2));
		if (shape_updated <= 0) {
			step_size = 0.5 * shape / (inv_hess(2, 0) * grad(0) + inv_hess(2, 1) * grad(1) + inv_hess(2, 2) * grad(2));
			shape_updated = shape
				- step_size * (inv_hess(2, 0) * grad(0) + inv_hess(2, 1) * grad(1) + inv_hess(2, 2) * grad(2));
		}
		log_rate1_updated = log_rate1
			- step_size * (inv_hess(0, 0) * grad(0) + inv_hess(0, 1) * grad(1) + inv_hess(0, 2) * grad(2));
		log_rate2_updated = log_rate2
			- step_size * (inv_hess(1, 0) * grad(0) + inv_hess(1, 1) * grad(1) + inv_hess(1, 2) * grad(2));
		step_size = 1;

		if (fabs(log_rate1 - log_rate1_updated) < eps && fabs(log_rate2 - log_rate2_updated) < eps
			&& fabs(shape - shape_updated) < eps) {
			estimates(0) = exp(log_rate1_updated);
			estimates(1) = exp(log_rate2_updated);
			estimates(2) = shape_updated;
			return (estimates);
		}

		log_rate1 = log_rate1_updated;
		log_rate2 = log_rate2_updated;
		shape = shape_updated;
	}

	if (step_count > 20) {
		stop("Maximum number of steps reached");
	}

	estimates(0) = exp(log_rate1);
	estimates(1) = exp(log_rate2);
	estimates(2) = shape;
	return (estimates);
}
