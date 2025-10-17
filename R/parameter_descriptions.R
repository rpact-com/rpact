## |
## |  *Parameters*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |

#' Parameter Description: "..."
#' @param ... Ensures that all arguments (starting from the "...") are to be named and
#'   that a warning will be displayed if unknown arguments are passed.
#' @name param_three_dots
#' @keywords internal
NULL

#' Parameter Description: "..." (optional plot arguments)
#' @param ... Optional plot arguments. At the moment \code{xlim} and \code{ylim} are implemented
#'   for changing x or y axis limits without dropping data observations.
#' @name param_three_dots_plot
#' @keywords internal
NULL

#' Parameter Description: Maximum Number of Stages
#' @param kMax The maximum number of stages \code{K}.
#'   Must be a positive integer of length 1 (default value is \code{3}).
#'   The maximum selectable \code{kMax} is \code{20} for group sequential or inverse normal and
#'   \code{6} for Fisher combination test designs.
#' @name param_kMax
#' @keywords internal
NULL

#' Parameter Description: Alpha
#' @param alpha The significance level alpha, default is \code{0.025}. Must be a positive numeric of length 1.
#' @name param_alpha
#' @keywords internal
NULL

#' Parameter Description: Beta
#' @param beta Type II error rate, necessary for providing sample size calculations
#'   (e.g., \code{\link[=getSampleSizeMeans]{getSampleSizeMeans()}}), beta spending function designs,
#'   or optimum designs, default is \code{0.20}. Must be a positive numeric of length 1.
#' @name param_beta
#' @keywords internal
NULL

#' Parameter Description: Sided
#' @param sided Is the alternative one-sided (\code{1}) or two-sided (\code{2}), default is \code{1}. 
#'    Must be a positive integer of length 1.
#' @name param_sided
#' @keywords internal
NULL

#' Parameter Description: Information Rates
#' @param informationRates The information rates t_1, ..., t_kMax (that must be fixed prior to the trial),
#'   default is \code{(1:kMax) / kMax}. For the weighted inverse normal design, the weights are derived
#'   through w_1 = sqrt(t_1), and w_k = sqrt(t_k - t_(k-1)). For the weighted Fisher's combination test, the 
#'   weights (scales) are w_k = sqrt((t_k - t_(k-1)) / t_1) (see the documentation).
#' @name param_informationRates
#' @keywords internal
NULL

#' Parameter Description: Binding Futility
#' @param bindingFutility Logical. If \code{bindingFutility = TRUE} is specified the calculation of
#'   the critical values is affected by the futility bounds and the futility threshold is binding in the
#'   sense that the study must be stopped if the futility condition was reached (default is \code{FALSE}).
#' @name param_bindingFutility
#' @keywords internal
NULL

#' Parameter Description: Type of Design
#' @param typeOfDesign The type of design. Type of design is one of the following:
#'   O'Brien & Fleming (\code{"OF"}), Pocock (\code{"P"}), Wang & Tsiatis Delta class (\code{"WT"}),
#'   Pampallona & Tsiatis (\code{"PT"}), Haybittle & Peto ("HP"),
#'   Optimum design within Wang & Tsiatis class (\code{"WToptimum"}),
#'   O'Brien & Fleming type alpha spending (\code{"asOF"}), Pocock type alpha spending (\code{"asP"}),
#'   Kim & DeMets alpha spending (\code{"asKD"}), Hwang, Shi & DeCani alpha spending (\code{"asHSD"}),
#'   user defined alpha spending (\code{"asUser"}), no early efficacy stop (\code{"noEarlyEfficacy"}),
#'   default is \code{"OF"}.
#' @name param_typeOfDesign
#' @keywords internal
NULL

#' Parameter Description: Design
#' @param design The trial design.
#' @name param_design
#' @keywords internal
NULL

#' Parameter Description: Design with Default
#' @param design The trial design. If no trial design is specified, a fixed sample size design is used.
#'   In this case, Type I error rate \code{alpha}, Type II error rate \code{beta}, \code{twoSidedPower},
#'   and \code{sided} can be directly entered as argument where necessary.
#' @name param_design_with_default
#' @keywords internal
NULL

#' Parameter Description: N_max
#' @param nMax The maximum sample size. Must be a positive integer of length 1.
#' @name param_nMax
#' @keywords internal
NULL

#' Parameter Description: Theta
#' @param theta A vector of standardized effect sizes (theta values), default is a sequence from -1 to 1.
#' @name param_theta
#' @keywords internal
NULL

#' Parameter Description: User Alpha Spending
#' @param userAlphaSpending The user defined alpha spending.
#'   Numeric vector of length \code{kMax} containing the cumulative
#'   alpha-spending (Type I error rate) up to each interim stage: \code{0 <= alpha_1 <= ... <= alpha_K <= alpha}.
#' @name param_userAlphaSpending
#' @keywords internal
NULL

##
## Sample Size and Power
##

#' Parameter Description: Effect Under Alternative
#' @param thetaH1 If specified, the value of the alternative under which
#'   the conditional power or sample size recalculation calculation is performed. Must be a numeric of length 1.
#' @name param_thetaH1
#' @keywords internal
NULL

#' Parameter Description: Standard Deviation
#' @param stDev The standard deviation under which the sample size or power
#'   calculation is performed, default is \code{1}.
#'   For two-armed trials, it is allowed to specify the standard deviations separately, 
#'   i.e., as vector with two elements. 
#'   If \code{meanRatio = TRUE} is specified, \code{stDev} defines
#'   the coefficient of variation \code{sigma / mu2}. 
#' @name param_stDev
#' @keywords internal
NULL

#' Parameter Description: Lambda (1)
#' @param lambda1 The assumed hazard rate in the treatment group, there is no default.
#'   \code{lambda1} can also be used to define piecewise exponentially distributed survival times (see details). Must be a positive numeric of length 1.
#' @name param_lambda1
#' @keywords internal
NULL

#' Parameter Description: Lambda (2)
#' @param lambda2 The assumed hazard rate in the reference group, there is no default.
#'   \code{lambda2} can also be used to define piecewise exponentially distributed survival times (see details). Must be a positive numeric of length 1.
#' @name param_lambda2
#' @keywords internal
NULL

#' Parameter Description: Pi (1) for Rates
#' @param pi1 A numeric value or vector that represents the assumed probability in
#'   the active treatment group if two treatment groups
#'   are considered, or the alternative probability for a one treatment group design,
#'   default is \code{seq(0.2, 0.5, 0.1)} (power calculations and simulations) or
#'   \code{seq(0.4, 0.6, 0.1)} (sample size calculations).
#' @name param_pi1_rates
#' @keywords internal
NULL

#' Parameter Description: Pi (1) for Survival Data
#' @param pi1 A numeric value or vector that represents the assumed event rate in the treatment group,
#'   default is \code{seq(0.2, 0.5, 0.1)} (power calculations and simulations) or
#'   \code{seq(0.4, 0.6, 0.1)} (sample size calculations).
#' @name param_pi1_survival
#' @keywords internal
NULL

#' Parameter Description: Pi (2) for Rates
#' @param pi2 A numeric value that represents the assumed probability in the reference group if two treatment
#'   groups are considered, default is \code{0.2}.
#' @name param_pi2_rates
#' @keywords internal
NULL

#' Parameter Description: Pi (2) for Survival Data
#' @param pi2 A numeric value that represents the assumed event rate in the control group, default is \code{0.2}.
#' @name param_pi2_survival
#' @keywords internal
NULL

#' Parameter Description: Median (1)
#' @param median1 The assumed median survival time in the treatment group, there is no default.
#' @name param_median1
#' @keywords internal
NULL

#' Parameter Description: Median (2)
#' @param median2 The assumed median survival time in the reference group, there is no default. Must be a positive numeric of length 1.
#' @name param_median2
#' @keywords internal
NULL

#' Parameter Description: Hazard Ratio
#' @param hazardRatio The vector of hazard ratios under consideration.
#'   If the event or hazard rates in both treatment groups are defined, the hazard ratio needs
#'   not to be specified as it is calculated, there is no default. Must be a positive numeric of length 1.
#' @name param_hazardRatio
#' @keywords internal
NULL

#' Parameter Description: Event Time
#' @param eventTime The assumed time under which the event rates are calculated, default is \code{12}.
#' @name param_eventTime
#' @keywords internal
NULL

#' Parameter Description: Piecewise Survival Time
#' @param piecewiseSurvivalTime A vector that specifies the time intervals for the piecewise
#'   definition of the exponential survival time cumulative distribution function \cr
#'   (for details see \code{\link[=getPiecewiseSurvivalTime]{getPiecewiseSurvivalTime()}}).
#' @name param_piecewiseSurvivalTime
#' @keywords internal
NULL

#' Parameter Description: Kappa
#' @param kappa A numeric value > 0. A \code{kappa != 1} will be used for the specification
#'   of the shape of the Weibull distribution.
#'   Default is \code{1}, i.e., the exponential survival distribution is used instead of the Weibull distribution.
#'   Note that the Weibull distribution cannot be used for the piecewise definition of
#'   the survival time distribution, i.e., only \code{piecewiselambda} (as a single value) and \code{kappa}
#'   can be specified.
#'   This function is equivalent to \code{pweibull(t, shape = kappa, scale = 1 / lambda)}
#'   of the \code{stats} package, i.e., the scale parameter is \code{1 / 'hazard rate'}.\cr
#'   For example,
#'   \code{getPiecewiseExponentialDistribution(time = 130, piecewiseLambda = 0.01, kappa = 4.2)}
#'   and \code{pweibull(q = 130, shape = 4.2, scale = 1 / 0.01)} provide the same result.
#' @name param_kappa
#' @keywords internal
NULL

#' Parameter Description: Type Of Computation
#' @param typeOfComputation Three options are available: \code{"Schoenfeld"}, \code{"Freedman"}, \code{"HsiehFreedman"},
#'   the default is \code{"Schoenfeld"}. For details, see Hsieh (Statistics in Medicine, 1992).
#'   For non-inferiority testing (i.e., \code{thetaH0 != 1}), only Schoenfeld's formula can be used.
#' @name param_typeOfComputation
#' @keywords internal
NULL

#' Parameter Description: Dropout Rate (1)
#' @param dropoutRate1 The assumed drop-out rate in the treatment group, default is \code{0}.
#' @name param_dropoutRate1
#' @keywords internal
NULL

#' Parameter Description: Dropout Rate (2)
#' @param dropoutRate2 The assumed drop-out rate in the control group, default is \code{0}.
#' @name param_dropoutRate2
#' @keywords internal
NULL

#' Parameter Description: Dropout Time
#' @param dropoutTime The assumed time for drop-out rates in the control and the
#'   treatment group, default is \code{12}.
#' @name param_dropoutTime
#' @keywords internal
NULL

#' Parameter Description: Alternative
#' @param alternative The alternative hypothesis value for testing means. This can be a vector of assumed
#'   alternatives, default is \code{seq(0, 1, 0.2)} (power calculations) or \code{seq(0.2, 1, 0.2)} (sample size calculations).
#' @name param_alternative
#' @keywords internal
NULL

#' Parameter Description: Alternative for Simulation
#' @param alternative The alternative hypothesis value for testing means under which the data is simulated.
#' This can be a vector of assumed alternatives, default is \code{seq(0, 1, 0.2)}.
#' @name param_alternative_simulation
#' @keywords internal
NULL

##  Count data

#' Parameter Description: lambda for Counts
#' @param lambda A numeric value or vector that represents the assumed rate of a homogeneous Poisson process in
#'   the pooled treatment groups, there is no default.
#' @name param_lambda_counts
#' @keywords internal
NULL

#' Parameter Description: theta for Counts
#' @param theta A numeric value or vector that represents the assumed mean ratios lambda1/lambda2 of a homogeneous
#' Poisson process, there is no default.
#' @name param_theta_counts
#' @keywords internal
NULL

#' Parameter Description: lambda (1) for Counts
#' @param lambda1 A numeric value or vector that represents the assumed rate of a homogeneous Poisson process in
#'   the active treatment group, there is no default.
#' @name param_lambda1_counts
#' @keywords internal
NULL

#' Parameter Description: lambda (2) for Counts
#' @param lambda2 A numeric value that represents the assumed rate of a homogeneous Poisson process in
#'   the control group, there is no default.
#' @name param_lambda2_counts
#' @keywords internal
NULL

#' Parameter Description: overdispersion for Counts
#' @param overdispersion A numeric value that represents the assumed overdispersion of the negative binomial distribution,
#' default is \code{0}.
#' @name param_overdispersion_counts
#' @keywords internal
NULL

#' Parameter Description: fixedExposureTime for Counts
#' @param fixedExposureTime If specified, the fixed time of exposure per subject for count data, there is no default.
#' @name param_fixedExposureTime_counts
#' @keywords internal
NULL

#' Parameter Description: accrualTime for Counts
#' @param accrualTime If specified, the assumed accrual time interval(s) for the study, there is no default.
#' @name param_accrualTime_counts
#' @keywords internal
NULL

#' Parameter Description: accrualIntensity for Counts
#' @param accrualIntensity If specified, the assumed accrual intensities for the study, there is no default.
#' @name param_accrualIntensity_counts
#' @keywords internal
NULL

#' Parameter Description: followUpTime for Counts
#' @param followUpTime If specified, the assumed (additional) follow-up time for the study, there is no default.
#'        The total study duration is \code{accrualTime + followUpTime}.
#' @name param_followUpTime_counts
#' @keywords internal
NULL

##
## Analysis
##

#' Parameter Description: Stage Results
#' @param stageResults The results at given stage, obtained from \code{\link[=getStageResults]{getStageResults()}}.
#' @name param_stageResults
#' @keywords internal
NULL

#' Parameter Description: Stage
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#' @name param_stage
#' @keywords internal
NULL

#' Parameter Description: N Planned
#' @param nPlanned The additional (i.e., "new" and not cumulative) sample size planned for each of the subsequent stages.
#'   The argument must be a vector with length equal to the number of remaining stages and contain
#'   the combined sample size from both treatment groups if two groups are considered. For survival outcomes,
#'   it should contain the planned number of additional events.
#'   For multi-arm designs, it is the per-comparison (combined) sample size.
#'   For enrichment designs, it is the (combined) sample size for the considered sub-population.
#' @name param_nPlanned
#' @keywords internal
NULL

#' Parameter Description: Allocation Ratio Planned
#' @param allocationRatioPlanned The planned allocation ratio \code{n1 / n2} for a two treatment groups
#'   design, default is \code{1}. For multi-arm designs, it is the allocation ratio relating the active arm(s) to the control.
#'   For simulating means and rates for a two treatment groups design, it can be a vector of length \code{kMax}, the number of stages.
#'   It can be a vector of length \code{kMax}, too, for multi-arm and enrichment designs.
#' 	 In these cases, a change of allocating subjects to treatment groups over the stages can be assessed.
#'   Note that internally \code{allocationRatioPlanned} is treated as a vector of length \code{kMax}, not a scalar.
#' @name param_allocationRatioPlanned
#' @keywords internal
NULL

#' Parameter Description: Allocation Ratio Planned With Optimum Option
#' @param allocationRatioPlanned The planned allocation ratio \code{n1 / n2} for a two treatment groups
#'   design, default is \code{1}. If \code{allocationRatioPlanned = 0} is entered,
#'   the optimal allocation ratio yielding the smallest overall sample size is determined.
#' @name param_allocationRatioPlanned_sampleSize
#' @keywords internal
NULL

#' Parameter Description: Direction Upper
#' @param directionUpper Logical. Specifies the direction of the alternative,
#'   only applicable for one-sided testing; default is \code{TRUE}
#'   which means that larger values of the test statistics yield smaller p-values.
#' @name param_directionUpper
#' @keywords internal
NULL

#' Parameter Description: Data Input
#' @param dataInput The summary data used for calculating the test results.
#'   This is either an element of \code{DatasetMeans}, of \code{DatasetRates}, or of \code{DatasetSurvival}
#'   and should be created with the function \code{\link[=getDataset]{getDataset()}}.
#'   For more information see \code{\link[=getDataset]{getDataset()}}.
#' @name param_dataInput
#' @keywords internal
NULL

#' Parameter Description: Normal Approximation
#' @param normalApproximation The type of computation of the p-values. Default is \code{FALSE} for
#'        testing means (i.e., the t test is used) and \code{TRUE} for testing rates and the hazard ratio.
#'        For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test
#'        (one sample) or the exact test of Fisher (two samples) is used for calculating the p-values.
#'        In the survival setting \code{normalApproximation = FALSE} has no effect.
#' @name param_normalApproximation
#' @keywords internal
NULL

#' Parameter Description: Theta H0
#' @param thetaH0 The null hypothesis value,
#'   default is \code{0} for the normal and the binary case (testing means and rates, respectively),
#'   it is \code{1} for the survival case (testing the hazard ratio).\cr\cr
#'   For non-inferiority designs, \code{thetaH0} is the non-inferiority bound.
#'   That is, in case of (one-sided) testing of
#'   \itemize{
#'     \item \emph{means}: a value \code{!= 0}
#'       (or a value \code{!= 1} for testing the mean ratio) can be specified.
#'     \item \emph{rates}: a value \code{!= 0}
#'       (or a value \code{!= 1} for testing the risk ratio \code{pi1 / pi2}) can be specified.
#'     \item \emph{survival data}: a bound for testing H0: \code{hazard ratio = thetaH0 != 1} can be specified.
#'     \item \emph{count data}: a bound for testing H0: \code{lambda1 / lambda2 = thetaH0 != 1} can be specified.
#'   }
#'   For testing a rate in one sample, a value \code{thetaH0} in (0, 1) has to be specified for
#'   defining the null hypothesis H0: \code{pi = thetaH0}.
#' @name param_thetaH0
#' @keywords internal
NULL

#' Parameter Description: Legend Position On Plots
#' @param legendPosition The position of the legend.
#'   By default (\code{NA_integer_}) the algorithm tries to find a suitable position.
#'   Choose one of the following values to specify the position manually:
#'   \itemize{
#'     \item \code{-1}: no legend will be shown
#'     \item \code{NA}: the algorithm tries to find a suitable position
#'     \item \code{0}: legend position outside plot
#'     \item \code{1}: legend position left top
#'     \item \code{2}: legend position left center
#'     \item \code{3}: legend position left bottom
#'     \item \code{4}: legend position right top
#'     \item \code{5}: legend position right center
#'     \item \code{6}: legend position right bottom
#'   }
#' @name param_legendPosition
#' @keywords internal
NULL

#' Parameter Description: Grid (Output Specification Of Multiple Plots)
#' @param grid An integer value specifying the output of multiple plots.
#'   By default (\code{1}) a list of \code{ggplot} objects will be returned.
#'   If a \code{grid} value > 1 was specified, a grid plot will be returned
#'   if the number of plots is <= specified \code{grid} value;
#'   a list of \code{ggplot} objects will be returned otherwise.
#'   If \code{grid = 0} is specified, all plots will be created using \code{\link[base]{print}} command
#'   and a list of \code{ggplot} objects will be returned invisible.
#'   Note that one of the following packages must be installed to create a grid plot:
#'   'ggpubr', 'gridExtra', or 'cowplot'.
#' @name param_grid
#' @keywords internal
NULL

##
## Simulation
##

#' Parameter Description: Min Number Of Events Per Stage
#' @param minNumberOfEventsPerStage When performing a data driven sample size recalculation,
#'   the numeric vector \code{minNumberOfEventsPerStage} with length kMax determines the
#'   minimum number of events per stage (i.e., not cumulated), the first element
#'   is not taken into account.
#' @name param_minNumberOfEventsPerStage
#' @keywords internal
NULL

#' Parameter Description: Max Number Of Events Per Stage
#' @param maxNumberOfEventsPerStage When performing a data driven sample size recalculation,
#'   the numeric vector \code{maxNumberOfEventsPerStage} with length \code{kMax} determines the maximum number
#'   of events per stage (i.e., not cumulated), the first element is not taken into account.
#' @name param_maxNumberOfEventsPerStage
#' @keywords internal
NULL

#' Parameter Description: Planned Subjects
#' @param plannedSubjects \code{plannedSubjects} is a numeric vector of length \code{kMax} (the number of stages of the design)
#'   that determines the number of cumulated (overall) subjects when the interim stages are planned.
#'   For two treatment arms, it is the number of subjects for both treatment arms.
#'   For multi-arm designs, \code{plannedSubjects} refers to the number of subjects per selected active arm.
#' @name param_plannedSubjects
#' @keywords internal
NULL

#' Parameter Description: Planned Events
#' @param plannedEvents \code{plannedEvents} is a numeric vector of length \code{kMax} (the number of stages of the design)
#'   that determines the number of cumulated (overall) events in survival designs when the interim stages are planned.
#'   For two treatment arms, it is the number of events for both treatment arms.
#'   For multi-arm designs, \code{plannedEvents} refers to the overall number of events for the selected arms plus control.
#' @name param_plannedEvents
#' @keywords internal
NULL

#' Parameter Description: Minimum Number Of Subjects Per Stage
#' @param minNumberOfSubjectsPerStage When performing a data driven sample size recalculation,
#'   the numeric vector \code{minNumberOfSubjectsPerStage} with length \code{kMax} determines the
#'   minimum number of subjects per stage (i.e., not cumulated), the first element
#'   is not taken into account. For two treatment arms, it is the number of subjects for both treatment arms.
#'   For multi-arm designs \code{minNumberOfSubjectsPerStage} refers
#'   to the minimum number of subjects per selected active arm.
#' @name param_minNumberOfSubjectsPerStage
#' @keywords internal
NULL

#' Parameter Description: Maximum Number Of Subjects Per Stage
#' @param maxNumberOfSubjectsPerStage When performing a data driven sample size recalculation,
#'   the numeric vector \code{maxNumberOfSubjectsPerStage} with length \code{kMax} determines the maximum number
#'   of subjects per stage (i.e., not cumulated), the first element is not taken into account.
#'   For two treatment arms, it is the number of subjects for both treatment arms.
#'   For multi-arm designs \code{maxNumberOfSubjectsPerStage} refers
#'   to the maximum number of subjects per selected active arm.
#' @name param_maxNumberOfSubjectsPerStage
#' @keywords internal
NULL

#' Parameter Description: Conditional Power
#' @param conditionalPower The conditional power for the subsequent stage
#'   under which the sample size recalculation is performed. Must be a positive numeric of length 1.
#' @name param_conditionalPower
#' @keywords internal
NULL

#' Parameter Description: Conditional Power
#' @param conditionalPower If \code{conditionalPower} together with \code{minNumberOfSubjectsPerStage} and
#'   \code{maxNumberOfSubjectsPerStage} (or \code{minNumberOfEventsPerStage} and \code{maxNumberOfEventsPerStage}
#'   for survival designs) is specified, a sample size recalculation based on the specified conditional power is performed.
#'   It is defined as the power for the subsequent stage given the current data. By default,
#'   the conditional power will be calculated under the observed effect size. Optionally, you can also specify \code{thetaH1} and
#'   \code{stDevH1} (for simulating means), \code{pi1H1} and \code{pi2H1} (for simulating rates), or \code{thetaH1} (for simulating
#'   hazard ratios) as parameters under which it is calculated and the sample size recalculation is performed.
#' @name param_conditionalPowerSimulation
#' @keywords internal
NULL

#' Parameter Description: Maximum Number Of Iterations
#' @param maxNumberOfIterations The number of simulation iterations, default is \code{1000}. Must be a positive integer of length 1.
#' @name param_maxNumberOfIterations
#' @keywords internal
NULL

#' Parameter Description: Calculate Subjects Function
#' @param calcSubjectsFunction Optionally, a function can be entered that defines the way of performing the sample size
#'   recalculation. By default, sample size recalculation is performed with conditional power and specified
#'   \code{minNumberOfSubjectsPerStage} and \code{maxNumberOfSubjectsPerStage} (see details and examples).
#' @name param_calcSubjectsFunction
#' @keywords internal
NULL

#' Parameter Description: Calculate Events Function
#' @param calcEventsFunction Optionally, a function can be entered that defines the way of performing the sample size
#'   recalculation. By default, event number recalculation is performed with conditional power and specified
#'   \code{minNumberOfEventsPerStage} and \code{maxNumberOfEventsPerStage} (see details and examples).
#' @name param_calcEventsFunction
#' @keywords internal
NULL

#' Parameter Description: Seed
#' @param seed The seed to reproduce the simulation, default is a random seed.
#' @name param_seed
#' @keywords internal
NULL

#' Parameter Description: Show Statistics
#' @param showStatistics Logical. If \code{TRUE}, summary statistics of the simulated data
#'    are displayed for the \code{print} command, otherwise the output is suppressed, default
#'    is \code{FALSE}.
#' @name param_showStatistics
#' @keywords internal
NULL

#' Parameter Description: Maximum Number Of Subjects
#' @param maxNumberOfSubjects \code{maxNumberOfSubjects > 0} needs to be specified for power calculations or calculation
#' of necessary follow-up (count data). For two treatment arms, it is the maximum number of subjects for both treatment arms.
#' @name param_maxNumberOfSubjects
#' @keywords internal
NULL

#' Parameter Description: Maximum Number Of Subjects For Survival Endpoint
#' @param maxNumberOfSubjects \code{maxNumberOfSubjects > 0} needs to be specified.
#'   If accrual time and accrual intensity are specified, this will be calculated. Must be a positive integer of length 1.
#' @name param_maxNumberOfSubjects_survival
#' @keywords internal
NULL

#' Parameter Description: Accrual Time
#' @param accrualTime The assumed accrual time intervals for the study, default is
#'   \code{c(0, 12)} (for details see \code{\link[=getAccrualTime]{getAccrualTime()}}).
#' @name param_accrualTime
#' @keywords internal
NULL

#' Parameter Description: Accrual Intensity
#' @param accrualIntensity A numeric vector of accrual intensities, default is the relative
#'   intensity \code{0.1} (for details see \code{\link[=getAccrualTime]{getAccrualTime()}}).
#' @name param_accrualIntensity
#' @keywords internal
NULL

#' Parameter Description: Accrual Intensity Type
#' @param accrualIntensityType A character value specifying the accrual intensity input type.
#'   Must be one of \code{"auto"}, \code{"absolute"}, or \code{"relative"}; default is \code{"auto"},
#'   i.e., if all values are < 1 the type is \code{"relative"}, otherwise it is \code{"absolute"}.
#' @name param_accrualIntensityType
#' @keywords internal
NULL

#' Parameter Description: Standard Deviation Under Alternative
#' @param stDevH1 If specified, the value of the standard deviation under which
#'   the conditional power or sample size recalculation calculation is performed,
#'   default is the value of \code{stDev}.
#' @name param_stDevH1
#' @keywords internal
NULL

#' Parameter Description: Standard Deviation for Simulation
#' @param stDev The standard deviation under which the data is simulated,
#'   default is \code{1}.
#'   For two-armed trials, it is allowed to specify the standard deviations separately, 
#'   i.e., as vector with two elements. 
#'   If \code{meanRatio = TRUE} is specified, \code{stDev} defines
#'   the coefficient of variation \code{sigma / mu2}. 
#' @name param_stDevSimulation
#' @keywords internal
NULL

#' Parameter Description: Number Of Treatment Groups
#' @param groups The number of treatment groups (1 or 2), default is \code{2}.
#' @name param_groups
#' @keywords internal
NULL

##
## Other
##

#' Parameter Description: Nice Column Names Enabled
#' @param niceColumnNamesEnabled Logical. If \code{TRUE}, nice looking column
#'   names will be used; syntactic names (variable names) otherwise
#'   (see \code{\link[base]{make.names}}).
#' @name param_niceColumnNamesEnabled
#' @keywords internal
NULL

#' Parameter Description: Include All Parameters
#' @param includeAllParameters Logical. If \code{TRUE}, all available
#'   parameters will be included in the data frame;
#'   a meaningful parameter selection otherwise, default is \code{FALSE}.
#' @name param_includeAllParameters
#' @keywords internal
NULL

#' Parameter Description: Digits
#' @param digits Defines how many digits are to be used for numeric values. Must be a positive integer of length 1.
#' @name param_digits
#' @keywords internal
NULL

#' Parameter Description: Tolerance
#' @param tolerance The numerical tolerance, default is \code{1e-06}. Must be a positive numeric of length 1.
#' @name param_tolerance
#' @keywords internal
NULL

##
## Plots
##

#' Parameter Description: Plot Points Enabled
#' @param plotPointsEnabled Logical. If \code{TRUE}, additional points will be plotted.
#' @name param_plotPointsEnabled
#' @keywords internal
NULL

#' Parameter Description: Palette
#' @param palette The palette, default is \code{"Set1"}.
#' @name param_palette
#' @keywords internal
NULL


##
## Multi-Arm and Enrichment Designs
##


#' Parameter Description: Intersection Test
#' @param intersectionTest Defines the multiple test for the intersection
#'   hypotheses in the closed system of hypotheses.
#'   Five options are available in multi-arm designs: \code{"Dunnett"}, \code{"Bonferroni"}, \code{"Simes"},
#'   \code{"Sidak"}, and \code{"Hierarchical"}, default is \code{"Dunnett"}.
#' @name param_intersectionTest_MultiArm
#' @keywords internal
NULL

#' Parameter Description: Intersection Test
#' @param intersectionTest Defines the multiple test for the intersection
#'   hypotheses in the closed system of hypotheses.
#'   Four options are available in enrichment designs: \code{"SpiessensDebois"}, \code{"Bonferroni"}, \code{"Simes"},
#'   and \code{"Sidak"}, default is \code{"Simes"}.
#' @name param_intersectionTest_Enrichment
#' @keywords internal
NULL

#' Parameter Description: Type of Selection
#' @param typeOfSelection The way the treatment arms or populations are selected at interim.
#'   Five options are available: \code{"best"}, \code{"rbest"}, \code{"epsilon"}, \code{"all"}, and \code{"userDefined"},
#'   default is \code{"best"}.\cr
#'   For \code{"rbest"} (select the \code{rValue} best treatment arms/populations), the parameter \code{rValue} has to be specified,
#'   for \code{"epsilon"} (select treatment arm/population not worse than epsilon compared to the best), the parameter
#'   \code{epsilonValue} has to be specified.
#'   If \code{"userDefined"} is selected, \code{"selectArmsFunction"} or \code{"selectPopulationsFunction"} has to be specified.
#' @name param_typeOfSelection
#' @keywords internal
NULL

#' Parameter Description: Effect Measure
#' @param effectMeasure Criterion for treatment arm/population selection, either based on test statistic
#'   (\code{"testStatistic"}) or effect estimate (difference for means and rates or ratio for survival) (\code{"effectEstimate"}),
#'   default is \code{"effectEstimate"}.
#' @name param_effectMeasure
#' @keywords internal
NULL

#' Parameter Description: Adaptations
#' @param adaptations A logical vector of length \code{kMax - 1} indicating whether or not an adaptation takes
#'   place at interim k, default is \code{rep(TRUE, kMax - 1)}.
#' @name param_adaptations
#' @keywords internal
NULL

#' Parameter Description: Threshold
#' @param threshold Selection criterion: treatment arm / population is selected only if \code{effectMeasure}
#'   exceeds \code{threshold}, default is \code{-Inf}.
#'   \code{threshold} can also be a vector of length \code{activeArms} referring to
#'   a separate threshold condition over the treatment arms.
#' @name param_threshold
#' @keywords internal
NULL

#' Parameter Description: Effect Matrix
#' @param effectMatrix Matrix of effect sizes with \code{activeArms} columns and number of rows
#'   reflecting the different situations to consider.
#' @name param_effectMatrix
#' @keywords internal
NULL

#' Parameter Description: Effect List
#' @param effectList List of subsets, prevalences, and effect sizes with columns and number of rows
#'   reflecting the different situations to consider (see examples).
#' @name param_effectList
#' @keywords internal
NULL

#' Parameter Description: Active Arms
#' @param activeArms The number of active treatment arms to be compared with control, default is \code{3}.
#' @name param_activeArms
#' @keywords internal
NULL

#' Parameter Description: Populations
#' @param populations The number of populations in a two-sample comparison, default is \code{3}.
#' @name param_populations
#' @keywords internal
NULL

#' Parameter Description: Success Criterion
#' @param successCriterion Defines when the study is stopped for efficacy at interim.
#'   Two options are available: \code{"all"} stops the trial
#'   if the efficacy criterion is fulfilled for all selected treatment arms/populations,
#'   \code{"atLeastOne"} stops if at least one of the selected treatment arms/populations is shown to be
#'   superior to control at interim, default is \code{"all"}.
#' @name param_successCriterion
#' @keywords internal
NULL

#' Parameter Description: Type Of Shape
#' @param typeOfShape The shape of the dose-response relationship over the treatment groups.
#'   This can be either \code{"linear"}, \code{"sigmoidEmax"}, or \code{"userDefined"},
#'   default is \code{"linear"}.\cr
#'   For \code{"linear"}, \code{muMaxVector} specifies the range
#'   of effect sizes for the treatment group with highest response.
#'   If \code{"sigmoidEmax"} is selected, \code{gED50} and \code{slope} has to be entered
#'   to specify the ED50 and the slope of the sigmoid Emax model.
#'   For \code{"sigmoidEmax"}, \code{muMaxVector} specifies the range
#'   of effect sizes for the treatment group with response according to infinite dose.
#'   If \code{"userDefined"} is selected, \code{effectMatrix} has to be entered.
#' @name param_typeOfShapeMeans 
#' @keywords internal
NULL

#' Parameter Description: Type Of Shape
#' @param typeOfShape The shape of the dose-response relationship over the treatment groups.
#'   This can be either \code{"linear"}, \code{"sigmoidEmax"}, or \code{"userDefined"},
#'   default is \code{"linear"}.\cr
#'   For \code{"linear"}, \code{piMaxVector} specifies the range
#'   of effect sizes for the treatment group with highest response.
#'   If \code{"sigmoidEmax"} is selected, \code{gED50} and \code{slope} has to be entered
#'   to specify the ED50 and the slope of the sigmoid Emax model.
#'   For \code{"sigmoidEmax"}, \code{piMaxVector} specifies the range
#'   of effect sizes for the treatment group with response according to infinite dose.
#'   If \code{"userDefined"} is selected, \code{effectMatrix} has to be entered.
#' @name param_typeOfShapeRates
#' @keywords internal
NULL

#' Parameter Description: Type Of Shape
#' @param typeOfShape The shape of the dose-response relationship over the treatment groups.
#'   This can be either \code{"linear"}, \code{"sigmoidEmax"}, or \code{"userDefined"},
#'   default is \code{"linear"}.\cr
#'   For \code{"linear"}, \code{omegaMaxVector} specifies the range
#'   of effect sizes for the treatment group with highest response.
#'   If \code{"sigmoidEmax"} is selected, \code{gED50} and \code{slope} has to be entered
#'   to specify the ED50 and the slope of the sigmoid Emax model.
#'   For \code{"sigmoidEmax"}, \code{omegaMaxVector} specifies the range
#'   of effect sizes for the treatment group with response according to infinite dose.
#'   If \code{"userDefined"} is selected, \code{effectMatrix} has to be entered.
#' @name param_typeOfShapeSurvival
#' @keywords internal
NULL

#' Parameter Description: Variance Option
#' @param varianceOption Defines the way to calculate the variance in multiple treatment arms (> 2)
#'   or population enrichment designs for testing means. For multiple arms, three options are available:
#'   \code{"overallPooled"}, \code{"pairwisePooled"}, and \code{"notPooled"}, default is \code{"overallPooled"}.
#'   For enrichment designs, the options are: \code{"pooled"}, \code{"pooledFromFull"} (one subset only),
#'   and \code{"notPooled"}, default is \code{"pooled"}.
#' @name param_varianceOption
#' @keywords internal
NULL

#' Parameter Description: Select Arms Function
#' @param selectArmsFunction Optionally, a function can be entered that defines the way of how treatment arms
#' are selected. This function is allowed to depend on \code{effectVector} with length \code{activeArms}, 
#' \code{stage}, \code{conditionalPower}, \code{conditionalCriticalValue}, \code{plannedSubjects/plannedEvents}, 
#' \code{allocationRatioPlanned}, \code{selectedArms}, \code{thetaH1} (for means and survival), \code{stDevH1} (for means), 
#' \code{overallEffects}, and for rates additionally: \code{piTreatmentsH1}, \code{piControlH1}, \code{overallRates}, and 
#' \code{overallRatesControl} (see examples). 
#' @name param_selectArmsFunction
#' @keywords internal
NULL

#' Parameter Description: Select Populations Function
#' @param selectPopulationsFunction Optionally, a function can be entered that defines the way of how populations
#' are selected. This function is allowed to depend on \code{effectVector} with length \code{populations}
#' \code{stage}, \code{conditionalPower}, \code{conditionalCriticalValue}, \code{plannedSubjects/plannedEvents}, 
#' \code{allocationRatioPlanned}, \code{selectedPopulations}, \code{thetaH1} (for means and survival), \code{stDevH1} (for means), 
#' \code{overallEffects}, and for rates additionally: \code{piTreatmentsH1}, \code{piControlH1}, \code{overallRates}, and 
#' \code{overallRatesControl} (see examples). 
#' @name param_selectPopulationsFunction
#' @keywords internal
NULL

#' Parameter Description: Stratified Analysis
#' @param stratifiedAnalysis Logical. For enrichment designs, typically a stratified analysis should be chosen.
#' For testing rates, also a non-stratified analysis based on overall data can be performed.
#' For survival data, only a stratified analysis is possible (see Brannath et al., 2009),
#' default is \code{TRUE}.
#' @name param_stratifiedAnalysis
#' @keywords internal
NULL

#' Parameter Description: Show Source
#' @param showSource Logical. If \code{TRUE}, the parameter names of the object will
#'   be printed which were used to create the plot; that may be, e.g.,
#'   useful to check the values or to create own plots with the base R \code{plot} function.
#'   Alternatively \code{showSource} can be defined as one of the following character values:
#'   \itemize{
#'     \item \code{"commands"}: returns a character vector with plot commands
#'     \item \code{"axes"}: returns a list with the axes definitions
#'     \item \code{"test"}: all plot commands will be validated with \code{eval(parse())} and
#'           returned as character vector (function does not stop if an error occurs)
#'     \item \code{"validate"}: all plot commands will be validated with \code{eval(parse())} and
#'           returned as character vector (function stops if an error occurs)
#'   }
#'   Note: no plot object will be returned if \code{showSource} is a character.
#' @name param_showSource
#' @keywords internal
NULL

#' Parameter Description: R Value
#' @param rValue For \code{typeOfSelection = "rbest"} (select the \code{rValue} best treatment arms / populations),
#'   the parameter \code{rValue} has to be specified.
#' @name param_rValue
#' @keywords internal
NULL

#' Parameter Description: Epsilon Value
#' @param epsilonValue For \code{typeOfSelection = "epsilon"} (select treatment arm / population not worse than
#'   epsilon compared to the best), the parameter \code{epsilonValue} has to be specified. Must be a numeric of length 1.
#' @name param_epsilonValue
#' @keywords internal
NULL

#' Parameter Description: G ED50
#' @param gED50 If \code{typeOfShape = "sigmoidEmax"} is selected, \code{gED50} has to be entered
#'   to specify the ED50 of the sigmoid Emax model.
#' @name param_gED50
#' @keywords internal
NULL

#' Parameter Description: Slope
#' @param slope If \code{typeOfShape = "sigmoidEmax"} is selected, \code{slope} can be entered
#'   to specify the slope of the sigmoid Emax model, default is 1.
#' @name param_slope
#' @keywords internal
NULL

#' Parameter Description: Dose Levels
#' @param doseLevels The dose levels for the dose response relationship. 
#'   If not specified, these dose levels are \code{1,...,activeArms}.
#' @name param_doseLevels
#' @keywords internal
NULL

#' Parameter Description: Maximum Information
#' @param maxInformation Positive value specifying the maximum information.
#' @name param_maxInformation
#' @keywords internal
NULL

#' Parameter Description: Information Epsilon
#' @param informationEpsilon Positive integer value specifying the absolute information epsilon, which
#'    defines the maximum distance from the observed information to the maximum information that causes the final analysis.
#'    Updates at the final analysis in case the observed information at the final
#'    analysis is smaller ("under-running") than the planned maximum information \code{maxInformation}, default is 0.
#'    Alternatively, a floating-point number > 0 and < 1 can be specified to define a relative information epsilon.
#' @name param_informationEpsilon
#' @keywords internal
NULL

#' Parameter Description: Plot Settings
#' @param plotSettings An object of class \code{PlotSettings} created by \code{\link[=getPlotSettings]{getPlotSettings()}}.
#' @name param_plotSettings
#' @keywords internal
NULL

#' Parameter Description: Planned Calendar Time
#' @param plannedCalendarTime For simulating count data, the time points where an analysis is planned to be performed.
#' Should be a vector of length \code{kMax}
#' @name param_plannedCalendarTime
#' @keywords internal
NULL
