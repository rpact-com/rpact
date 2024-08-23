#' @examples
#' \dontrun{
#' # Fixed sample size with minimum required definitions, pi1 = (0.3,0.4,0.5,0.6) and
#' # pi2 = 0.3 at event time 12, and accrual time 24
#' getSimulationSurvival(
#'     pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 10
#' )
#' 
#' # Increase number of simulation iterations
#' getSimulationSurvival(
#'     pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, eventTime = 12,
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Determine necessary accrual time with default settings if 200 subjects and
#' # 30 subjects per time unit can be recruited
#' getSimulationSurvival(
#'     plannedEvents = 40, accrualTime = 0,
#'     accrualIntensity = 30, maxNumberOfSubjects = 200, maxNumberOfIterations = 50
#' )
#'
#' # Determine necessary accrual time with default settings if 200 subjects and
#' # if the first 6 time units 20 subjects per time unit can be recruited,
#' # then 30 subjects per time unit
#' getSimulationSurvival(
#'     plannedEvents = 40, accrualTime = c(0, 6),
#'     accrualIntensity = c(20, 30), maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Determine maximum number of Subjects with default settings if the first
#' # 6 time units 20 subjects per time unit can be recruited, and after
#' # 10 time units 30 subjects per time unit
#' getSimulationSurvival(
#'     plannedEvents = 40, accrualTime = c(0, 6, 10),
#'     accrualIntensity = c(20, 30), maxNumberOfIterations = 50
#' )
#'
#' # Specify accrual time as a list
#' at <- list(
#'     "0 - <6"  = 20,
#'     "6 - Inf" = 30
#' )
#' getSimulationSurvival(
#'     plannedEvents = 40, accrualTime = at,
#'     maxNumberOfSubjects = 200, maxNumberOfIterations = 50
#' )
#'
#' # Specify accrual time as a list, if maximum number of subjects need to be calculated
#' at <- list(
#'     "0 - <6"   = 20,
#'     "6 - <=10" = 30
#' )
#' getSimulationSurvival(plannedEvents = 40, accrualTime = at, maxNumberOfIterations = 50)
#'
#' # Specify effect size for a two-stage group sequential design with
#' # O'Brien & Fleming boundaries. Effect size is based on event rates
#' # at specified event time, directionUpper = FALSE needs to be specified
#' # because it should be shown that hazard ratio < 1
#' designGS <- getDesignGroupSequential(kMax = 2)
#' getSimulationSurvival(
#'     design = designGS,
#'     pi1 = 0.2, pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40),
#'     maxNumberOfSubjects = 200, directionUpper = FALSE, maxNumberOfIterations = 50
#' )
#'
#' # As above, but with a three-stage O'Brien and Fleming design with
#' # specified information rates, note that planned events consists of integer values
#' designGS2 <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
#' getSimulationSurvival(
#'     design = designGS2, 
#'     pi1 = 0.2, pi2 = 0.3, eventTime = 24,
#'     plannedEvents = round(designGS2$informationRates * 40),
#'     maxNumberOfSubjects = 200, directionUpper = FALSE,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Effect size is based on event rate at specified event time for the reference
#' # group and hazard ratio, directionUpper = FALSE needs to be specified because
#' # it should be shown that hazard ratio < 1
#' getSimulationSurvival(
#'     design = designGS, hazardRatio = 0.5,
#'     pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
#'     directionUpper = FALSE, maxNumberOfIterations = 50
#' )
#'
#' # Effect size is based on hazard rate for the reference group and
#' # hazard ratio, directionUpper = FALSE needs to be specified because
#' # it should be shown that hazard ratio < 1
#' getSimulationSurvival(
#'     design = designGS,
#'     hazardRatio = 0.5, lambda2 = 0.02, plannedEvents = c(20, 40),
#'     maxNumberOfSubjects = 200, directionUpper = FALSE,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Specification of piecewise exponential survival time and hazard ratios,
#' # note that in getSimulationSurvival only on hazard ratio is used
#' # in the case that the survival time is piecewise expoential
#' getSimulationSurvival(
#'     design = designGS,
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
#'     hazardRatio = 1.5, plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 50
#' )
#'
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04
#' )
#' getSimulationSurvival(
#'     design = designGS,
#'     piecewiseSurvivalTime = pws, hazardRatio = c(1.5),
#'     plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Specification of piecewise exponential survival time for both treatment arms
#' getSimulationSurvival(
#'     design = designGS,
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
#'     lambda1 = c(0.015, 0.03, 0.06), plannedEvents = c(20, 40),
#'     maxNumberOfSubjects = 200, maxNumberOfIterations = 50
#' )
#'
#' # Specification of piecewise exponential survival time as a list,
#' # note that in getSimulationSurvival only on hazard ratio
#' # (not a vector) can be used
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04
#' )
#' getSimulationSurvival(
#'     design = designGS,
#'     piecewiseSurvivalTime = pws, hazardRatio = 1.5,
#'     plannedEvents = c(20, 40), maxNumberOfSubjects = 200,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Specification of piecewise exponential survival time and delayed effect
#' # (response after 5 time units)
#' getSimulationSurvival(
#'     design = designGS,
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04),
#'     lambda1 = c(0.01, 0.02, 0.06), plannedEvents = c(20, 40),
#'     maxNumberOfSubjects = 200, maxNumberOfIterations = 50
#' )
#'
#' # Specify effect size based on median survival times
#' getSimulationSurvival(
#'     median1 = 5, median2 = 3, plannedEvents = 40,
#'     maxNumberOfSubjects = 200, directionUpper = FALSE,
#'     maxNumberOfIterations = 50
#' )
#'
#' # Specify effect size based on median survival
#' # times of Weibull distribtion with kappa = 2
#' getSimulationSurvival(
#'     median1 = 5, median2 = 3, kappa = 2,
#'     plannedEvents = 40, maxNumberOfSubjects = 200,
#'     directionUpper = FALSE, maxNumberOfIterations = 50
#' )
#'
#' # Perform recalculation of number of events based on conditional power for a
#' # three-stage design with inverse normal combination test, where the conditional power
#' # is calculated under the specified effect size thetaH1 = 1.3 and up to a four-fold
#' # increase in originally planned sample size (number of events) is allowed.
#' # Note that the first value in minNumberOfEventsPerStage and
#' # maxNumberOfEventsPerStage is arbitrary, i.e., it has no effect.
#' designIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))
#'
#' resultsWithSSR1 <- getSimulationSurvival(
#'     design = designIN,
#'     hazardRatio = seq(1, 1.6, 0.1),
#'     pi2 = 0.3, conditionalPower = 0.8, thetaH1 = 1.3,
#'     plannedEvents = c(58, 102, 146),
#'     minNumberOfEventsPerStage = c(NA, 44, 44),
#'     maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
#'     maxNumberOfSubjects = 800, maxNumberOfIterations = 50
#' )
#' resultsWithSSR1
#'
#' # If thetaH1 is unspecified, the observed hazard ratio estimate
#' # (calculated from the log-rank statistic) is used for performing the
#' # recalculation of the number of events
#' resultsWithSSR2 <- getSimulationSurvival(
#'     design = designIN,
#'     hazardRatio = seq(1, 1.6, 0.1),
#'     pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146),
#'     minNumberOfEventsPerStage = c(NA, 44, 44),
#'     maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
#'     maxNumberOfSubjects = 800, maxNumberOfIterations = 50
#' )
#' resultsWithSSR2
#'
#' # Compare it with design without event size recalculation
#' resultsWithoutSSR <- getSimulationSurvival(
#'     design = designIN,
#'     hazardRatio = seq(1, 1.6, 0.1), pi2 = 0.3,
#'     plannedEvents = c(58, 102, 145), maxNumberOfSubjects = 800,
#'     maxNumberOfIterations = 50
#' )
#' resultsWithoutSSR$overallReject
#' resultsWithSSR1$overallReject
#' resultsWithSSR2$overallReject
#'
#' # Confirm that event size racalcuation increases the Type I error rate,
#' # i.e., you have to use the combination test
#' resultsWithSSRGS <- getSimulationSurvival(
#'     design = designGS2, 
#'     hazardRatio = seq(1),
#'     pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 145),
#'     minNumberOfEventsPerStage = c(NA, 44, 44),
#'     maxNumberOfEventsPerStage = 4 * c(NA, 44, 44),
#'     maxNumberOfSubjects = 800, maxNumberOfIterations = 50
#' )
#' resultsWithSSRGS$overallReject
#'
#' # Set seed to get reproducable results
#' identical(
#'     getSimulationSurvival(
#'         plannedEvents = 40, maxNumberOfSubjects = 200,
#'         seed = 99
#'     )$analysisTime,
#'     getSimulationSurvival(
#'         plannedEvents = 40, maxNumberOfSubjects = 200,
#'         seed = 99
#'     )$analysisTime
#' )
#'
#' # Perform recalculation of number of events based on conditional power as above.
#' # The number of events is recalculated only in the first interim, the recalculated number
#' # is also used for the final stage. Here, we use the user defind calcEventsFunction as
#' # follows (note that the last stage value in minNumberOfEventsPerStage and maxNumberOfEventsPerStage
#' # has no effect):
#' myCalcEventsFunction <- function(...,
#'         stage, conditionalPower, estimatedTheta,
#'         plannedEvents, eventsOverStages,
#'         minNumberOfEventsPerStage, maxNumberOfEventsPerStage,
#'         conditionalCriticalValue) {
#'     theta <- max(1 + 1e-12, estimatedTheta)
#'     if (stage == 2) {
#'         requiredStageEvents <-
#'             max(0, conditionalCriticalValue + qnorm(conditionalPower))^2 * 4 / log(theta)^2
#'         requiredOverallStageEvents <- min(
#'             max(minNumberOfEventsPerStage[stage], requiredStageEvents),
#'             maxNumberOfEventsPerStage[stage]
#'         ) + eventsOverStages[stage - 1]
#'     } else {
#'         requiredOverallStageEvents <- 2 * eventsOverStages[stage - 1] - eventsOverStages[1]
#'     }
#'     return(requiredOverallStageEvents)
#' }
#' resultsWithSSR <- getSimulationSurvival(
#'     design = designIN,
#'     hazardRatio = seq(1, 2.6, 0.5),
#'     pi2 = 0.3,
#'     conditionalPower = 0.8,
#'     plannedEvents = c(58, 102, 146),
#'     minNumberOfEventsPerStage = c(NA, 44, 4),
#'     maxNumberOfEventsPerStage = 4 * c(NA, 44, 4),
#'     maxNumberOfSubjects = 800,
#'     calcEventsFunction = myCalcEventsFunction,
#'     seed = 1234,
#'     maxNumberOfIterations = 50
#' )
#' }
#'
