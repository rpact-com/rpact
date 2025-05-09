// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "rpact_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// mvnprd
NumericVector mvnprd(NumericVector a, NumericVector b, NumericVector bpd, float eps, IntegerVector inf, int ierc, float hinc);
RcppExport SEXP _rpact_mvnprd(SEXP aSEXP, SEXP bSEXP, SEXP bpdSEXP, SEXP epsSEXP, SEXP infSEXP, SEXP iercSEXP, SEXP hincSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bpd(bpdSEXP);
    Rcpp::traits::input_parameter< float >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type inf(infSEXP);
    Rcpp::traits::input_parameter< int >::type ierc(iercSEXP);
    Rcpp::traits::input_parameter< float >::type hinc(hincSEXP);
    rcpp_result_gen = Rcpp::wrap(mvnprd(a, b, bpd, eps, inf, ierc, hinc));
    return rcpp_result_gen;
END_RCPP
}
// mvstud
NumericVector mvstud(int ndf, NumericVector a, NumericVector b, NumericVector bpd, NumericVector d, float eps, IntegerVector inf, int ierc, float hnc);
RcppExport SEXP _rpact_mvstud(SEXP ndfSEXP, SEXP aSEXP, SEXP bSEXP, SEXP bpdSEXP, SEXP dSEXP, SEXP epsSEXP, SEXP infSEXP, SEXP iercSEXP, SEXP hncSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type ndf(ndfSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bpd(bpdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type d(dSEXP);
    Rcpp::traits::input_parameter< float >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type inf(infSEXP);
    Rcpp::traits::input_parameter< int >::type ierc(iercSEXP);
    Rcpp::traits::input_parameter< float >::type hnc(hncSEXP);
    rcpp_result_gen = Rcpp::wrap(mvstud(ndf, a, b, bpd, d, eps, inf, ierc, hnc));
    return rcpp_result_gen;
END_RCPP
}
// estimate_nb
SEXP estimate_nb(NumericVector counts1, NumericVector counts2, NumericVector t1, NumericVector t2);
RcppExport SEXP _rpact_estimate_nb(SEXP counts1SEXP, SEXP counts2SEXP, SEXP t1SEXP, SEXP t2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type counts1(counts1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type counts2(counts2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type t1(t1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type t2(t2SEXP);
    rcpp_result_gen = Rcpp::wrap(estimate_nb(counts1, counts2, t1, t2));
    return rcpp_result_gen;
END_RCPP
}
// getFisherCombinationSizeCpp
double getFisherCombinationSizeCpp(double kMax, NumericVector alpha0Vec, NumericVector criticalValues, NumericVector tVec, NumericVector cases);
RcppExport SEXP _rpact_getFisherCombinationSizeCpp(SEXP kMaxSEXP, SEXP alpha0VecSEXP, SEXP criticalValuesSEXP, SEXP tVecSEXP, SEXP casesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0Vec(alpha0VecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tVec(tVecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cases(casesSEXP);
    rcpp_result_gen = Rcpp::wrap(getFisherCombinationSizeCpp(kMax, alpha0Vec, criticalValues, tVec, cases));
    return rcpp_result_gen;
END_RCPP
}
// getSimulatedAlphaCpp
double getSimulatedAlphaCpp(int kMax, NumericVector alpha0, NumericVector criticalValues, NumericVector tVec, int iterations);
RcppExport SEXP _rpact_getSimulatedAlphaCpp(SEXP kMaxSEXP, SEXP alpha0SEXP, SEXP criticalValuesSEXP, SEXP tVecSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0(alpha0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tVec(tVecSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(getSimulatedAlphaCpp(kMax, alpha0, criticalValues, tVec, iterations));
    return rcpp_result_gen;
END_RCPP
}
// getFisherCombinationCasesCpp
NumericVector getFisherCombinationCasesCpp(int kMax, NumericVector tVec);
RcppExport SEXP _rpact_getFisherCombinationCasesCpp(SEXP kMaxSEXP, SEXP tVecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type tVec(tVecSEXP);
    rcpp_result_gen = Rcpp::wrap(getFisherCombinationCasesCpp(kMax, tVec));
    return rcpp_result_gen;
END_RCPP
}
// getDesignFisherTryCpp
List getDesignFisherTryCpp(int kMax, double alpha, double tolerance, NumericVector criticalValues, NumericVector scale, NumericVector alpha0Vec, NumericVector userAlphaSpending, String method);
RcppExport SEXP _rpact_getDesignFisherTryCpp(SEXP kMaxSEXP, SEXP alphaSEXP, SEXP toleranceSEXP, SEXP criticalValuesSEXP, SEXP scaleSEXP, SEXP alpha0VecSEXP, SEXP userAlphaSpendingSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type scale(scaleSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0Vec(alpha0VecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userAlphaSpending(userAlphaSpendingSEXP);
    Rcpp::traits::input_parameter< String >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignFisherTryCpp(kMax, alpha, tolerance, criticalValues, scale, alpha0Vec, userAlphaSpending, method));
    return rcpp_result_gen;
END_RCPP
}
// getGroupSequentialProbabilitiesCpp
NumericMatrix getGroupSequentialProbabilitiesCpp(NumericMatrix decisionMatrix, NumericVector informationRates);
RcppExport SEXP _rpact_getGroupSequentialProbabilitiesCpp(SEXP decisionMatrixSEXP, SEXP informationRatesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type decisionMatrix(decisionMatrixSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    rcpp_result_gen = Rcpp::wrap(getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialPampallonaTsiatisCpp
List getDesignGroupSequentialPampallonaTsiatisCpp(double tolerance, double beta, double alpha, double kMax, double deltaPT0, double deltaPT1, NumericVector informationRates, int sided, bool bindingFutility);
RcppExport SEXP _rpact_getDesignGroupSequentialPampallonaTsiatisCpp(SEXP toleranceSEXP, SEXP betaSEXP, SEXP alphaSEXP, SEXP kMaxSEXP, SEXP deltaPT0SEXP, SEXP deltaPT1SEXP, SEXP informationRatesSEXP, SEXP sidedSEXP, SEXP bindingFutilitySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type deltaPT0(deltaPT0SEXP);
    Rcpp::traits::input_parameter< double >::type deltaPT1(deltaPT1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< int >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialPampallonaTsiatisCpp(tolerance, beta, alpha, kMax, deltaPT0, deltaPT1, informationRates, sided, bindingFutility));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialUserDefinedAlphaSpendingCpp
NumericVector getDesignGroupSequentialUserDefinedAlphaSpendingCpp(int kMax, NumericVector userAlphaSpending, double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance);
RcppExport SEXP _rpact_getDesignGroupSequentialUserDefinedAlphaSpendingCpp(SEXP kMaxSEXP, SEXP userAlphaSpendingSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP futilityBoundsSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userAlphaSpending(userAlphaSpendingSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialUserDefinedAlphaSpendingCpp(kMax, userAlphaSpending, sided, informationRates, bindingFutility, futilityBounds, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialAlphaSpendingCpp
NumericVector getDesignGroupSequentialAlphaSpendingCpp(int kMax, double alpha, double gammaA, String typeOfDesign, double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance);
RcppExport SEXP _rpact_getDesignGroupSequentialAlphaSpendingCpp(SEXP kMaxSEXP, SEXP alphaSEXP, SEXP gammaASEXP, SEXP typeOfDesignSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP futilityBoundsSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type gammaA(gammaASEXP);
    Rcpp::traits::input_parameter< String >::type typeOfDesign(typeOfDesignSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialAlphaSpendingCpp(kMax, alpha, gammaA, typeOfDesign, sided, informationRates, bindingFutility, futilityBounds, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialDeltaWTCpp
NumericVector getDesignGroupSequentialDeltaWTCpp(int kMax, double alpha, double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance, double deltaWT);
RcppExport SEXP _rpact_getDesignGroupSequentialDeltaWTCpp(SEXP kMaxSEXP, SEXP alphaSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP futilityBoundsSEXP, SEXP toleranceSEXP, SEXP deltaWTSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< double >::type deltaWT(deltaWTSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialDeltaWTCpp(kMax, alpha, sided, informationRates, bindingFutility, futilityBounds, tolerance, deltaWT));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialPocockCpp
NumericVector getDesignGroupSequentialPocockCpp(int kMax, double alpha, double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance);
RcppExport SEXP _rpact_getDesignGroupSequentialPocockCpp(SEXP kMaxSEXP, SEXP alphaSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP futilityBoundsSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialPocockCpp(kMax, alpha, sided, informationRates, bindingFutility, futilityBounds, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialOBrienAndFlemingCpp
NumericVector getDesignGroupSequentialOBrienAndFlemingCpp(int kMax, double alpha, double sided, NumericVector informationRates, bool bindingFutility, NumericVector futilityBounds, double tolerance);
RcppExport SEXP _rpact_getDesignGroupSequentialOBrienAndFlemingCpp(SEXP kMaxSEXP, SEXP alphaSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP futilityBoundsSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialOBrienAndFlemingCpp(kMax, alpha, sided, informationRates, bindingFutility, futilityBounds, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialBetaSpendingCpp
List getDesignGroupSequentialBetaSpendingCpp(NumericVector criticalValues, int kMax, NumericVector userAlphaSpending, NumericVector userBetaSpending, NumericVector informationRates, bool bindingFutility, double tolerance, String typeOfDesign, String typeBetaSpending, double gammaA, double gammaB, double alpha, double beta, double sided, bool betaAdjustment, bool twoSidedPower);
RcppExport SEXP _rpact_getDesignGroupSequentialBetaSpendingCpp(SEXP criticalValuesSEXP, SEXP kMaxSEXP, SEXP userAlphaSpendingSEXP, SEXP userBetaSpendingSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP toleranceSEXP, SEXP typeOfDesignSEXP, SEXP typeBetaSpendingSEXP, SEXP gammaASEXP, SEXP gammaBSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP sidedSEXP, SEXP betaAdjustmentSEXP, SEXP twoSidedPowerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userAlphaSpending(userAlphaSpendingSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userBetaSpending(userBetaSpendingSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< String >::type typeOfDesign(typeOfDesignSEXP);
    Rcpp::traits::input_parameter< String >::type typeBetaSpending(typeBetaSpendingSEXP);
    Rcpp::traits::input_parameter< double >::type gammaA(gammaASEXP);
    Rcpp::traits::input_parameter< double >::type gammaB(gammaBSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< bool >::type betaAdjustment(betaAdjustmentSEXP);
    Rcpp::traits::input_parameter< bool >::type twoSidedPower(twoSidedPowerSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialBetaSpendingCpp(criticalValues, kMax, userAlphaSpending, userBetaSpending, informationRates, bindingFutility, tolerance, typeOfDesign, typeBetaSpending, gammaA, gammaB, alpha, beta, sided, betaAdjustment, twoSidedPower));
    return rcpp_result_gen;
END_RCPP
}
// getDesignGroupSequentialUserDefinedBetaSpendingCpp
List getDesignGroupSequentialUserDefinedBetaSpendingCpp(NumericVector criticalValues, int kMax, NumericVector userAlphaSpending, NumericVector userBetaSpending, double sided, NumericVector informationRates, bool bindingFutility, double tolerance, String typeOfDesign, double gammaA, double alpha, bool betaAdjustment, bool twoSidedPower);
RcppExport SEXP _rpact_getDesignGroupSequentialUserDefinedBetaSpendingCpp(SEXP criticalValuesSEXP, SEXP kMaxSEXP, SEXP userAlphaSpendingSEXP, SEXP userBetaSpendingSEXP, SEXP sidedSEXP, SEXP informationRatesSEXP, SEXP bindingFutilitySEXP, SEXP toleranceSEXP, SEXP typeOfDesignSEXP, SEXP gammaASEXP, SEXP alphaSEXP, SEXP betaAdjustmentSEXP, SEXP twoSidedPowerSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userAlphaSpending(userAlphaSpendingSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type userBetaSpending(userBetaSpendingSEXP);
    Rcpp::traits::input_parameter< double >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< bool >::type bindingFutility(bindingFutilitySEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< String >::type typeOfDesign(typeOfDesignSEXP);
    Rcpp::traits::input_parameter< double >::type gammaA(gammaASEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< bool >::type betaAdjustment(betaAdjustmentSEXP);
    Rcpp::traits::input_parameter< bool >::type twoSidedPower(twoSidedPowerSEXP);
    rcpp_result_gen = Rcpp::wrap(getDesignGroupSequentialUserDefinedBetaSpendingCpp(criticalValues, kMax, userAlphaSpending, userBetaSpending, sided, informationRates, bindingFutility, tolerance, typeOfDesign, gammaA, alpha, betaAdjustment, twoSidedPower));
    return rcpp_result_gen;
END_RCPP
}
// getSimulationMeansLoopCpp
List getSimulationMeansLoopCpp(NumericVector alternative, int kMax, int maxNumberOfIterations, int designNumber, NumericVector informationRates, NumericVector futilityBounds, NumericVector alpha0Vec, NumericVector criticalValues, bool meanRatio, double thetaH0, NumericVector stDev, int groups, bool normalApproximation, NumericVector plannedSubjects, bool directionUpper, NumericVector allocationRatioPlanned, NumericVector minNumberOfSubjectsPerStage, NumericVector maxNumberOfSubjectsPerStage, double conditionalPower, double thetaH1, NumericVector stDevH1, int calcSubjectsFunctionType, Nullable<Function> calcSubjectsFunctionR, SEXP calcSubjectsFunctionCpp);
RcppExport SEXP _rpact_getSimulationMeansLoopCpp(SEXP alternativeSEXP, SEXP kMaxSEXP, SEXP maxNumberOfIterationsSEXP, SEXP designNumberSEXP, SEXP informationRatesSEXP, SEXP futilityBoundsSEXP, SEXP alpha0VecSEXP, SEXP criticalValuesSEXP, SEXP meanRatioSEXP, SEXP thetaH0SEXP, SEXP stDevSEXP, SEXP groupsSEXP, SEXP normalApproximationSEXP, SEXP plannedSubjectsSEXP, SEXP directionUpperSEXP, SEXP allocationRatioPlannedSEXP, SEXP minNumberOfSubjectsPerStageSEXP, SEXP maxNumberOfSubjectsPerStageSEXP, SEXP conditionalPowerSEXP, SEXP thetaH1SEXP, SEXP stDevH1SEXP, SEXP calcSubjectsFunctionTypeSEXP, SEXP calcSubjectsFunctionRSEXP, SEXP calcSubjectsFunctionCppSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfIterations(maxNumberOfIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type designNumber(designNumberSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0Vec(alpha0VecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< bool >::type meanRatio(meanRatioSEXP);
    Rcpp::traits::input_parameter< double >::type thetaH0(thetaH0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type stDev(stDevSEXP);
    Rcpp::traits::input_parameter< int >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< bool >::type normalApproximation(normalApproximationSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type plannedSubjects(plannedSubjectsSEXP);
    Rcpp::traits::input_parameter< bool >::type directionUpper(directionUpperSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type allocationRatioPlanned(allocationRatioPlannedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minNumberOfSubjectsPerStage(minNumberOfSubjectsPerStageSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type maxNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStageSEXP);
    Rcpp::traits::input_parameter< double >::type conditionalPower(conditionalPowerSEXP);
    Rcpp::traits::input_parameter< double >::type thetaH1(thetaH1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type stDevH1(stDevH1SEXP);
    Rcpp::traits::input_parameter< int >::type calcSubjectsFunctionType(calcSubjectsFunctionTypeSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type calcSubjectsFunctionR(calcSubjectsFunctionRSEXP);
    Rcpp::traits::input_parameter< SEXP >::type calcSubjectsFunctionCpp(calcSubjectsFunctionCppSEXP);
    rcpp_result_gen = Rcpp::wrap(getSimulationMeansLoopCpp(alternative, kMax, maxNumberOfIterations, designNumber, informationRates, futilityBounds, alpha0Vec, criticalValues, meanRatio, thetaH0, stDev, groups, normalApproximation, plannedSubjects, directionUpper, allocationRatioPlanned, minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage, conditionalPower, thetaH1, stDevH1, calcSubjectsFunctionType, calcSubjectsFunctionR, calcSubjectsFunctionCpp));
    return rcpp_result_gen;
END_RCPP
}
// getSimulationRatesCpp
List getSimulationRatesCpp(int kMax, NumericVector informationRates, NumericVector criticalValues, NumericVector pi1, double pi2, int maxNumberOfIterations, int designNumber, int groups, NumericVector futilityBounds, NumericVector alpha0Vec, NumericVector minNumberOfSubjectsPerStage, NumericVector maxNumberOfSubjectsPerStage, NumericVector conditionalPower, NumericVector pi1H1, NumericVector pi2H1, bool normalApproximation, NumericVector plannedSubjects, bool directionUpper, NumericVector allocationRatioPlanned, bool riskRatio, double thetaH0, int calcSubjectsFunctionType, Nullable<Function> calcSubjectsFunctionR, SEXP calcSubjectsFunctionCpp);
RcppExport SEXP _rpact_getSimulationRatesCpp(SEXP kMaxSEXP, SEXP informationRatesSEXP, SEXP criticalValuesSEXP, SEXP pi1SEXP, SEXP pi2SEXP, SEXP maxNumberOfIterationsSEXP, SEXP designNumberSEXP, SEXP groupsSEXP, SEXP futilityBoundsSEXP, SEXP alpha0VecSEXP, SEXP minNumberOfSubjectsPerStageSEXP, SEXP maxNumberOfSubjectsPerStageSEXP, SEXP conditionalPowerSEXP, SEXP pi1H1SEXP, SEXP pi2H1SEXP, SEXP normalApproximationSEXP, SEXP plannedSubjectsSEXP, SEXP directionUpperSEXP, SEXP allocationRatioPlannedSEXP, SEXP riskRatioSEXP, SEXP thetaH0SEXP, SEXP calcSubjectsFunctionTypeSEXP, SEXP calcSubjectsFunctionRSEXP, SEXP calcSubjectsFunctionCppSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pi1(pi1SEXP);
    Rcpp::traits::input_parameter< double >::type pi2(pi2SEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfIterations(maxNumberOfIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type designNumber(designNumberSEXP);
    Rcpp::traits::input_parameter< int >::type groups(groupsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0Vec(alpha0VecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minNumberOfSubjectsPerStage(minNumberOfSubjectsPerStageSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type maxNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStageSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type conditionalPower(conditionalPowerSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pi1H1(pi1H1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pi2H1(pi2H1SEXP);
    Rcpp::traits::input_parameter< bool >::type normalApproximation(normalApproximationSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type plannedSubjects(plannedSubjectsSEXP);
    Rcpp::traits::input_parameter< bool >::type directionUpper(directionUpperSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type allocationRatioPlanned(allocationRatioPlannedSEXP);
    Rcpp::traits::input_parameter< bool >::type riskRatio(riskRatioSEXP);
    Rcpp::traits::input_parameter< double >::type thetaH0(thetaH0SEXP);
    Rcpp::traits::input_parameter< int >::type calcSubjectsFunctionType(calcSubjectsFunctionTypeSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type calcSubjectsFunctionR(calcSubjectsFunctionRSEXP);
    Rcpp::traits::input_parameter< SEXP >::type calcSubjectsFunctionCpp(calcSubjectsFunctionCppSEXP);
    rcpp_result_gen = Rcpp::wrap(getSimulationRatesCpp(kMax, informationRates, criticalValues, pi1, pi2, maxNumberOfIterations, designNumber, groups, futilityBounds, alpha0Vec, minNumberOfSubjectsPerStage, maxNumberOfSubjectsPerStage, conditionalPower, pi1H1, pi2H1, normalApproximation, plannedSubjects, directionUpper, allocationRatioPlanned, riskRatio, thetaH0, calcSubjectsFunctionType, calcSubjectsFunctionR, calcSubjectsFunctionCpp));
    return rcpp_result_gen;
END_RCPP
}
// getSimulationSurvivalCpp
List getSimulationSurvivalCpp(int designNumber, int kMax, int sided, NumericVector criticalValues, NumericVector informationRates, double conditionalPower, NumericVector plannedEvents, double thetaH1, NumericVector minNumberOfEventsPerStage, NumericVector maxNumberOfEventsPerStage, bool directionUpper, double allocationRatioPlanned, NumericVector accrualTime, IntegerVector treatmentGroup, double thetaH0, NumericVector futilityBounds, NumericVector alpha0Vec, NumericVector pi1Vec, double pi2, double eventTime, NumericVector piecewiseSurvivalTime, NumericVector cdfValues1, NumericVector cdfValues2, NumericVector lambdaVec1, NumericVector lambdaVec2, NumericVector phi, int maxNumberOfSubjects, int maxNumberOfIterations, int maxNumberOfRawDatasetsPerStage, double kappa, int calcEventsFunctionType, Nullable<Function> calcEventsFunctionR, SEXP calcEventsFunctionCpp);
RcppExport SEXP _rpact_getSimulationSurvivalCpp(SEXP designNumberSEXP, SEXP kMaxSEXP, SEXP sidedSEXP, SEXP criticalValuesSEXP, SEXP informationRatesSEXP, SEXP conditionalPowerSEXP, SEXP plannedEventsSEXP, SEXP thetaH1SEXP, SEXP minNumberOfEventsPerStageSEXP, SEXP maxNumberOfEventsPerStageSEXP, SEXP directionUpperSEXP, SEXP allocationRatioPlannedSEXP, SEXP accrualTimeSEXP, SEXP treatmentGroupSEXP, SEXP thetaH0SEXP, SEXP futilityBoundsSEXP, SEXP alpha0VecSEXP, SEXP pi1VecSEXP, SEXP pi2SEXP, SEXP eventTimeSEXP, SEXP piecewiseSurvivalTimeSEXP, SEXP cdfValues1SEXP, SEXP cdfValues2SEXP, SEXP lambdaVec1SEXP, SEXP lambdaVec2SEXP, SEXP phiSEXP, SEXP maxNumberOfSubjectsSEXP, SEXP maxNumberOfIterationsSEXP, SEXP maxNumberOfRawDatasetsPerStageSEXP, SEXP kappaSEXP, SEXP calcEventsFunctionTypeSEXP, SEXP calcEventsFunctionRSEXP, SEXP calcEventsFunctionCppSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type designNumber(designNumberSEXP);
    Rcpp::traits::input_parameter< int >::type kMax(kMaxSEXP);
    Rcpp::traits::input_parameter< int >::type sided(sidedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type criticalValues(criticalValuesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type informationRates(informationRatesSEXP);
    Rcpp::traits::input_parameter< double >::type conditionalPower(conditionalPowerSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type plannedEvents(plannedEventsSEXP);
    Rcpp::traits::input_parameter< double >::type thetaH1(thetaH1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minNumberOfEventsPerStage(minNumberOfEventsPerStageSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type maxNumberOfEventsPerStage(maxNumberOfEventsPerStageSEXP);
    Rcpp::traits::input_parameter< bool >::type directionUpper(directionUpperSEXP);
    Rcpp::traits::input_parameter< double >::type allocationRatioPlanned(allocationRatioPlannedSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type accrualTime(accrualTimeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type treatmentGroup(treatmentGroupSEXP);
    Rcpp::traits::input_parameter< double >::type thetaH0(thetaH0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type futilityBounds(futilityBoundsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alpha0Vec(alpha0VecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pi1Vec(pi1VecSEXP);
    Rcpp::traits::input_parameter< double >::type pi2(pi2SEXP);
    Rcpp::traits::input_parameter< double >::type eventTime(eventTimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type piecewiseSurvivalTime(piecewiseSurvivalTimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cdfValues1(cdfValues1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cdfValues2(cdfValues2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambdaVec1(lambdaVec1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lambdaVec2(lambdaVec2SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfSubjects(maxNumberOfSubjectsSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfIterations(maxNumberOfIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfRawDatasetsPerStage(maxNumberOfRawDatasetsPerStageSEXP);
    Rcpp::traits::input_parameter< double >::type kappa(kappaSEXP);
    Rcpp::traits::input_parameter< int >::type calcEventsFunctionType(calcEventsFunctionTypeSEXP);
    Rcpp::traits::input_parameter< Nullable<Function> >::type calcEventsFunctionR(calcEventsFunctionRSEXP);
    Rcpp::traits::input_parameter< SEXP >::type calcEventsFunctionCpp(calcEventsFunctionCppSEXP);
    rcpp_result_gen = Rcpp::wrap(getSimulationSurvivalCpp(designNumber, kMax, sided, criticalValues, informationRates, conditionalPower, plannedEvents, thetaH1, minNumberOfEventsPerStage, maxNumberOfEventsPerStage, directionUpper, allocationRatioPlanned, accrualTime, treatmentGroup, thetaH0, futilityBounds, alpha0Vec, pi1Vec, pi2, eventTime, piecewiseSurvivalTime, cdfValues1, cdfValues2, lambdaVec1, lambdaVec2, phi, maxNumberOfSubjects, maxNumberOfIterations, maxNumberOfRawDatasetsPerStage, kappa, calcEventsFunctionType, calcEventsFunctionR, calcEventsFunctionCpp));
    return rcpp_result_gen;
END_RCPP
}
// getOneMinusQNorm
double getOneMinusQNorm(double p, double mean, double sd, double lowerTail, double logP, double epsilon);
RcppExport SEXP _rpact_getOneMinusQNorm(SEXP pSEXP, SEXP meanSEXP, SEXP sdSEXP, SEXP lowerTailSEXP, SEXP logPSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< double >::type sd(sdSEXP);
    Rcpp::traits::input_parameter< double >::type lowerTail(lowerTailSEXP);
    Rcpp::traits::input_parameter< double >::type logP(logPSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(getOneMinusQNorm(p, mean, sd, lowerTail, logP, epsilon));
    return rcpp_result_gen;
END_RCPP
}
// zeroin
double zeroin(Function f, double lower, double upper, double tolerance, int maxIter);
RcppExport SEXP _rpact_zeroin(SEXP fSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP toleranceSEXP, SEXP maxIterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Function >::type f(fSEXP);
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< int >::type maxIter(maxIterSEXP);
    rcpp_result_gen = Rcpp::wrap(zeroin(f, lower, upper, tolerance, maxIter));
    return rcpp_result_gen;
END_RCPP
}
// getCipheredValue
std::string getCipheredValue(String x);
RcppExport SEXP _rpact_getCipheredValue(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(getCipheredValue(x));
    return rcpp_result_gen;
END_RCPP
}
// getFraction
IntegerVector getFraction(double x, double epsilon, int maxNumberOfSearchSteps);
RcppExport SEXP _rpact_getFraction(SEXP xSEXP, SEXP epsilonSEXP, SEXP maxNumberOfSearchStepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfSearchSteps(maxNumberOfSearchStepsSEXP);
    rcpp_result_gen = Rcpp::wrap(getFraction(x, epsilon, maxNumberOfSearchSteps));
    return rcpp_result_gen;
END_RCPP
}
// getFractions
List getFractions(NumericVector x, double epsilon, int maxNumberOfSearchSteps);
RcppExport SEXP _rpact_getFractions(SEXP xSEXP, SEXP epsilonSEXP, SEXP maxNumberOfSearchStepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< int >::type maxNumberOfSearchSteps(maxNumberOfSearchStepsSEXP);
    rcpp_result_gen = Rcpp::wrap(getFractions(x, epsilon, maxNumberOfSearchSteps));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rpact_mvnprd", (DL_FUNC) &_rpact_mvnprd, 7},
    {"_rpact_mvstud", (DL_FUNC) &_rpact_mvstud, 9},
    {"_rpact_estimate_nb", (DL_FUNC) &_rpact_estimate_nb, 4},
    {"_rpact_getFisherCombinationSizeCpp", (DL_FUNC) &_rpact_getFisherCombinationSizeCpp, 5},
    {"_rpact_getSimulatedAlphaCpp", (DL_FUNC) &_rpact_getSimulatedAlphaCpp, 5},
    {"_rpact_getFisherCombinationCasesCpp", (DL_FUNC) &_rpact_getFisherCombinationCasesCpp, 2},
    {"_rpact_getDesignFisherTryCpp", (DL_FUNC) &_rpact_getDesignFisherTryCpp, 8},
    {"_rpact_getGroupSequentialProbabilitiesCpp", (DL_FUNC) &_rpact_getGroupSequentialProbabilitiesCpp, 2},
    {"_rpact_getDesignGroupSequentialPampallonaTsiatisCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialPampallonaTsiatisCpp, 9},
    {"_rpact_getDesignGroupSequentialUserDefinedAlphaSpendingCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialUserDefinedAlphaSpendingCpp, 7},
    {"_rpact_getDesignGroupSequentialAlphaSpendingCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialAlphaSpendingCpp, 9},
    {"_rpact_getDesignGroupSequentialDeltaWTCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialDeltaWTCpp, 8},
    {"_rpact_getDesignGroupSequentialPocockCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialPocockCpp, 7},
    {"_rpact_getDesignGroupSequentialOBrienAndFlemingCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialOBrienAndFlemingCpp, 7},
    {"_rpact_getDesignGroupSequentialBetaSpendingCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialBetaSpendingCpp, 16},
    {"_rpact_getDesignGroupSequentialUserDefinedBetaSpendingCpp", (DL_FUNC) &_rpact_getDesignGroupSequentialUserDefinedBetaSpendingCpp, 13},
    {"_rpact_getSimulationMeansLoopCpp", (DL_FUNC) &_rpact_getSimulationMeansLoopCpp, 24},
    {"_rpact_getSimulationRatesCpp", (DL_FUNC) &_rpact_getSimulationRatesCpp, 24},
    {"_rpact_getSimulationSurvivalCpp", (DL_FUNC) &_rpact_getSimulationSurvivalCpp, 33},
    {"_rpact_getOneMinusQNorm", (DL_FUNC) &_rpact_getOneMinusQNorm, 6},
    {"_rpact_zeroin", (DL_FUNC) &_rpact_zeroin, 5},
    {"_rpact_getCipheredValue", (DL_FUNC) &_rpact_getCipheredValue, 1},
    {"_rpact_getFraction", (DL_FUNC) &_rpact_getFraction, 3},
    {"_rpact_getFractions", (DL_FUNC) &_rpact_getFractions, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_rpact(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
