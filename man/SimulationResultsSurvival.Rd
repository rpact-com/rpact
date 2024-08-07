% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_simulation_results.R
\name{SimulationResultsSurvival}
\alias{SimulationResultsSurvival}
\title{Class for Simulation Results Survival}
\description{
A class for simulation results survival.
}
\details{
Use \code{\link[=getSimulationSurvival]{getSimulationSurvival()}}
to create an object of this type.

\code{SimulationResultsSurvival} is the basic class for
\itemize{
\item \code{\link{SimulationResultsSurvival}},
\item \code{\link{SimulationResultsMultiArmSurvival}}, and
\item \code{\link{SimulationResultsEnrichmentSurvival}}.
}
}
\section{Fields}{

\describe{
\item{\code{maxNumberOfIterations}}{The number of simulation iterations. Is a numeric vector of length 1 containing a whole number.}

\item{\code{seed}}{The seed used for random number generation. Is a numeric vector of length 1.}

\item{\code{allocationRatioPlanned}}{The planned allocation ratio (\code{n1 / n2}) for the groups. For multi-arm designs, it is the allocation ratio relating the active arm(s) to the control. Is a positive numeric vector of length 1.}

\item{\code{conditionalPower}}{The conditional power at each stage of the trial. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{iterations}}{The number of iterations used for simulations. Is a numeric vector of length 1 containing a whole number.}

\item{\code{futilityPerStage}}{The per-stage probabilities of stopping the trial for futility. Is a numeric matrix.}

\item{\code{futilityStop}}{In simulation results data set: indicates whether trial is stopped for futility or not.}

\item{\code{directionUpper}}{Specifies the direction of the alternative, only applicable for one-sided testing. Default is \code{TRUE} which means that larger values of the test statistics yield smaller p-values. Is a logical vector of length 1.}

\item{\code{plannedEvents}}{Determines the number of cumulated (overall) events in survival designs when the interim stages are planned. For two treatment arms, is the number of events for both treatment arms. For multi-arm designs, refers to the overall number of events for the selected arms plus control. Is a numeric vector of length \code{kMax} containing whole numbers.}

\item{\code{minNumberOfEventsPerStage}}{Determines the minimum number of events per stage for data-driven sample size recalculation. Is a numeric vector of length \code{kMax} containing whole numbers.}

\item{\code{maxNumberOfEventsPerStage}}{Determines the maximum number of events per stage for data-driven sample size recalculation. Is a numeric vector of length \code{kMax} containing whole numbers.}

\item{\code{thetaH1}}{The assumed effect under the alternative hypothesis. For survival designs, refers to the hazard ratio. Is a numeric vector.}

\item{\code{calcEventsFunction}}{An optional function that can be entered to define how event size is recalculated. By default, recalculation is performed with conditional power with specified \code{minNumberOfEventsPerStage} and \code{maxNumberOfEventsPerStage}.}

\item{\code{expectedNumberOfEvents}}{The expected number of events under specified alternative. Is a numeric vector.}

\item{\code{pi1}}{The assumed event rate in the treatment group. Is a numeric vector of length \code{kMax} containing values between 0 and 1.}

\item{\code{pi2}}{The assumed event rate in the control group. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{median1}}{The assumed median survival time in the treatment group. Is a numeric vector.}

\item{\code{median2}}{The assumed median survival time in the reference group. Is a numeric vector of length 1.}

\item{\code{maxNumberOfSubjects}}{The maximum number of subjects for power calculations. Is a numeric vector.}

\item{\code{accrualTime}}{The assumed accrual time intervals for the study. Is a numeric vector.}

\item{\code{accrualIntensity}}{The absolute accrual intensities. Is a numeric vector of length \code{kMax}.}

\item{\code{dropoutRate1}}{The assumed drop-out rate in the treatment group. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{dropoutRate2}}{The assumed drop-out rate in the control group. Is a numeric vector of length 1 containing a value between 0 and 1.}

\item{\code{dropoutTime}}{The assumed time for drop-out rates in the control and treatment group. Is a numeric vector of length 1.}

\item{\code{eventTime}}{The assumed time under which the event rates are calculated. Is a numeric vector of length 1.}

\item{\code{thetaH0}}{The difference or assumed effect under H0. Is a numeric vector of length 1.}

\item{\code{allocation1}}{The number of subjects to be assigned to treatment 1 in subsequent order. Is a numeric vector of length 1 containing a whole number.}

\item{\code{allocation2}}{The number of subjects to be assigned to treatment 2 in subsequent order. Is a numeric vector of length 1 containing a whole number.}

\item{\code{kappa}}{The shape of the Weibull distribution if \code{kappa!=1}. Is a numeric vector of length 1.}

\item{\code{piecewiseSurvivalTime}}{The time intervals for the piecewise definition of the exponential survival time cumulative distribution function. Is a numeric vector.}

\item{\code{lambda1}}{The assumed hazard rate in the treatment group. Is a numeric vector of length \code{kMax}.}

\item{\code{lambda2}}{The assumed hazard rate in the reference group. Is a numeric vector of length 1.}

\item{\code{earlyStop}}{The probability to stopping the trial either for efficacy or futility. Is a numeric vector.}

\item{\code{hazardRatio}}{The hazard ratios under consideration. Is a numeric vector of length \code{kMax}.}

\item{\code{studyDuration}}{The study duration for specified effect size. Is a positive numeric vector.}

\item{\code{eventsNotAchieved}}{The simulated number of cases how often the number of events was not reached. Is a numeric matrix.}

\item{\code{numberOfSubjects}}{In simulation results data set: The number of subjects under consideration when the interim analysis takes place.}

\item{\code{numberOfSubjects1}}{In simulation results data set: The number of subjects under consideration in treatment arm 1 when the interim analysis takes place.}

\item{\code{numberOfSubjects2}}{In simulation results data set: The number of subjects under consideration in treatment arm 2 when the interim analysis takes place.}

\item{\code{singleEventsPerStage}}{The single number of events per stage. Is a numeric matrix.}

\item{\code{cumulativeEventsPerStage}}{The cumulative number of events per stage. Is a numeric matrix.}

\item{\code{expectedNumberOfSubjects}}{The expected number of subjects under specified alternative.}

\item{\code{rejectPerStage}}{The probability to reject a hypothesis per stage of the trial. Is a numeric matrix.}

\item{\code{overallReject}}{The overall rejection probability. Is a numeric vector.}

\item{\code{conditionalPowerAchieved}}{The calculated conditional power, under the assumption of observed or assumed effect sizes. Is a numeric matrix.}
}}

\keyword{internal}
