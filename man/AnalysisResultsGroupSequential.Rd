% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_analysis_results.R
\name{AnalysisResultsGroupSequential}
\alias{AnalysisResultsGroupSequential}
\title{Analysis Results Group Sequential}
\description{
Class for analysis results results based on a group sequential design.
}
\details{
This object cannot be created directly; use \code{\link{getAnalysisResults}}
with suitable arguments to create the analysis results of a group sequential design.
}
\section{Fields}{

\describe{
\item{\code{normalApproximation}}{Describes if a normal approximation was used when calculating p-values. Default for means is \code{FALSE} and \code{TRUE} for rates and hazard ratio. Is a logical vector of length 1.}

\item{\code{directionUpper}}{Specifies the direction of the alternative, only applicable for one-sided testing. Default is \code{TRUE} which means that larger values of the test statistics yield smaller p-values. Is a logical vector of length 1.}

\item{\code{thetaH0}}{The difference or assumed effect under H0. Is a numeric vector of length 1.}

\item{\code{pi1}}{The assumed probability or probabilities in the active treatment group in two-group designs, or the alternative probability for a one-group design.}

\item{\code{pi2}}{The assumed probability in the reference group for two-group designs. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{nPlanned}}{The sample size planned for each of the subsequent stages. Is a numeric vector of length \code{kMax} containing whole numbers.}

\item{\code{allocationRatioPlanned}}{The planned allocation ratio (\code{n1 / n2}) for the groups. For multi-arm designs, it is the allocation ratio relating the active arm(s) to the control. Is a positive numeric vector of length 1.}

\item{\code{thetaH1}}{The assumed effect under the alternative hypothesis. For survival designs, refers to the hazard ratio. Is a numeric vector.}

\item{\code{assumedStDev}}{The assumed standard deviation(s) for means analysis. Is a numeric vector.}

\item{\code{equalVariances}}{Describes if the variances in two treatment groups are assumed to be the same. Is a logical vector of length 1.}

\item{\code{testActions}}{The test decisions at each stage of the trial. Is a character vector of length \code{kMax}.}

\item{\code{conditionalRejectionProbabilities}}{The probabilities of rejecting the null hypothesis at each stage, given the stage has been reached. Is a numeric vector of length \code{kMax} containing values between 0 and 1.}

\item{\code{conditionalPower}}{The conditional power at each stage of the trial. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{repeatedConfidenceIntervalLowerBounds}}{The lower bound of the confidence intervals that are calculated at any stage of the trial. Is a numeric vector of length \code{kMax}.}

\item{\code{repeatedConfidenceIntervalUpperBounds}}{The upper bound of the confidence interval that are calculated at any stage of the trial. Is a numeric vector of length \code{kMax}.}

\item{\code{repeatedPValues}}{The p-values that are calculated at any stage of the trial. Is a numeric vector of length \code{kMax} containing values between 0 and 1.}

\item{\code{finalStage}}{The stage at which the trial ends, either with acceptance or rejection of the null hypothesis. Is a numeric vector of length 1.}

\item{\code{finalPValues}}{The final p-value that is based on the stage-wise ordering. Is a numeric vector of length \code{kMax} containing values between 0 and 1.}

\item{\code{finalConfidenceIntervalLowerBounds}}{The lower bound of the confidence interval that is based on the stage-wise ordering. Is a numeric vector of length \code{kMax}.}

\item{\code{finalConfidenceIntervalUpperBounds}}{The upper bound of the confidence interval that is based on the stage-wise ordering. Is a numeric vector of length \code{kMax}.}

\item{\code{medianUnbiasedEstimates}}{The calculated median unbiased estimates that are based on the stage-wise ordering. Is a numeric vector of length \code{kMax}.}

\item{\code{maxInformation}}{The maximum information. Is a numeric vector of length 1 containing a whole number.}

\item{\code{informationEpsilon}}{The absolute information epsilon, which defines the maximum distance from the observed information to the maximum information that causes the final analysis. Updates at the final analysis if the observed information at the final analysis is smaller ("under-running") than the planned maximum information. Is either a positive integer value specifying the absolute information epsilon or a floating point number >0 and <1 to define a relative information epsilon.}
}}

\keyword{internal}
