/**
 *
 * -- AS 251 --
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
 * File version: $Revision: 6890 $
 * Last changed: $Date: 2023-03-24 09:23:55 +0100 (Fri, 24 Mar 2023) $
 * Last changed by: $Author: pahlke $
 *
 */

#include "Rcpp.h"

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

extern "C" {
    void mvnprd_(
    	float* a,
		float* b,
		float* bpd,
		float* eps,
		int* n,
		int* inf,
		int* ierc,
		float* hinc,
		float* prob,
		float* bound,
		int* ifault);
}

// [[Rcpp::export(".mvnprd")]]
NumericVector mvnprd(
	NumericVector a,
	NumericVector b,
	NumericVector bpd,
	float eps,
	IntegerVector inf,
	int ierc,
	float hinc) {

  int n = a.size();

  // initialize float arrays
  float* af = new float[n];
  float* bf = new float[n];
  float* bpdf = new float[n];

  // copy vectors into float arrays
  for (int i = 0; i < n; i++) {
    af[i] = (float) a[i];
    bf[i] = (float) b[i];
    bpdf[i] = (float) bpd[i];
  }

  // initialize result variables
  float prob = 0;
  float bound = 0;
  int ifault = 0;

  int infValue = inf[0];
  mvnprd_(&(af[0]), &(bf[0]), &(bpdf[0]), &eps, &n, &infValue, &ierc, &hinc, &prob, &bound, &ifault);

  // free allocated memory
  delete[] af;
  delete[] bf;
  delete[] bpdf;

  return NumericVector::create(prob, bound, ifault);
}

extern "C" {
    void mvstud_(
    	int* ndf,
    	float* a,
		float* b,
		float* bpd,
		float* errb,
		int* n,
		int* inf,
		float* d,
		int* ierc,
		float* hnc,
		float* prob,
		float* bnd,
		int* iflt);
}

// [[Rcpp::export(".mvstud")]]
NumericVector mvstud(
	int ndf,
	NumericVector a,
	NumericVector b,
	NumericVector bpd,
	NumericVector d,
	float eps,
	IntegerVector inf,
	int ierc,
	float hnc) {

  int n = a.size();

  // initialize float arrays
  float* af = new float[n];
  float* bf = new float[n];
  float* bpdf = new float[n];
  float* df = new float[n];

  // copy vectors into float arrays
  for (int i = 0; i < n; i++) {
    af[i] = (float) a[i];
    bf[i] = (float) b[i];
    bpdf[i] = (float) bpd[i];
    df[i] = (float) d[i];
  }

  // initialize result variables
  float prob = 0;
  float bnd = 0;
  int iflt = 0;

  int infValue = inf[0];
  mvstud_(&ndf, &(af[0]), &(bf[0]), &(bpdf[0]), &eps, &n, &infValue, &(df[0]), &ierc, &hnc, &prob, &bnd, &iflt);

  // free allocated memory
  delete[] af;
  delete[] bf;
  delete[] bpdf;
  delete[] df;

  return NumericVector::create(prob, bnd, iflt);
}
