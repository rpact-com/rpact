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

// [[Rcpp::export(name = ".mvnprd")]]
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

  mvnprd_(&(af[0]), &(bf[0]), &(bpdf[0]), &eps, &n, (int*) &(inf[0]), &ierc, &hinc, &prob, &bound, &ifault);

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

// [[Rcpp::export(name = ".mvstud")]]
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

  // MVSTUD(NDF, A, B, BPD, ERRB, N, INF, D, IERC, HNC, PROB, BND, IFLT)
  mvstud_(&ndf, &(af[0]), &(bf[0]), &(bpdf[0]), &eps, &n, (int*) &(inf[0]), &(df[0]), &ierc, &hnc, &prob, &bnd, &iflt);

  // free allocated memory
  delete[] af;
  delete[] bf;
  delete[] bpdf;
  delete[] df;

  return NumericVector::create(prob, bnd, iflt);
}

NumericVector sigmaToBPD(const NumericMatrix& sigma) {
    int ncol = sigma.ncol();
    NumericVector bpd(ncol);
        
    if (ncol == 2) {
        double sqrtSigma12 = sqrt(sigma(0, 1));
        std::fill(bpd.begin(), bpd.end(), sqrtSigma12);
        return bpd;
    }
    
    bpd[0] = sqrt(sigma(0, 1) * sigma(0, 2) / sigma(1, 2));
    for (int i = 1; i < ncol; i++) {
        bpd[i] = sigma(0, i) / bpd[0];
    }
    
    return bpd;
}

// [[Rcpp::export(name = ".as251NormalCpp")]]
double as251Normal(NumericVector lower, 
                   NumericVector upper, 
                   NumericMatrix sigma,
                   double eps = 1e-06,
                   String errorControl = "strict",
                   double intervalSimpsonsRule = 0.0) {
    
    int errorControlInt;
    if (errorControl == "strict") {
        errorControlInt = 1;
    } else {
        errorControlInt = 0;
    }
    
    NumericVector bpd = sigmaToBPD(sigma);
    int n = bpd.size();
    
    NumericVector lowerExt = rep_len(lower, n);
    NumericVector upperExt = rep_len(upper, n);
    
    IntegerVector inf(n, 2);
    
    for (int i = 0; i < n; i++) {
        if (std::isinf(upperExt[i]) && upperExt[i] > 0) {
            inf[i] = 0;
        }
        if (std::isinf(lowerExt[i]) && lowerExt[i] < 0) {
            inf[i] = 1;
        }
    }
    
    NumericVector result = mvnprd(
      upperExt, 
      lowerExt, 
      bpd, 
      (float)eps, 
      inf, 
      errorControlInt, 
      (float)intervalSimpsonsRule
    );
    
    return result[0];
}

