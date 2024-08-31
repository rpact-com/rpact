#' @examples
#' \dontrun{
#' # Fixed sample size with minimum required definitions, pi1 = c(0.2, 0.3, 0.4, 0.5) and 
#' # pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default 
#' getPowerSurvival(maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # Four stage O'Brien & Fleming group sequential design with minimum required 
#' # definitions, pi1 = c(0.2, 0.3, 0.4, 0.5) and pi2 = 0.2 at event time 12, 
#' # accrual time 12 and follow-up time 6 as default  
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 4), 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # For fixed sample design, determine necessary accrual time if 200 subjects and 
#' # 30 subjects per time unit can be recruited 
#' getPowerSurvival(maxNumberOfEvents = 40, accrualTime = c(0), 
#'     accrualIntensity = 30, maxNumberOfSubjects = 200)
#' 
#' # Determine necessary accrual time if 200 subjects and if the first 6 time units 
#' # 20 subjects per time unit can be recruited, then 30 subjects per time unit 
#' getPowerSurvival(maxNumberOfEvents = 40, accrualTime = c(0, 6), 
#'     accrualIntensity = c(20, 30), maxNumberOfSubjects = 200)
#' 
#' # Determine maximum number of Subjects if the first 6 time units 20 subjects per 
#' # time unit can be recruited, and after 10 time units 30 subjects per time unit
#' getPowerSurvival(maxNumberOfEvents = 40, accrualTime = c(0, 6, 10), 
#'     accrualIntensity = c(20, 30))
#' 
#' # Specify accrual time as a list
#' at <- list(
#'     "0 - <6"  = 20,
#'     "6 - Inf" = 30)
#' getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at, maxNumberOfSubjects = 200)
#' 
#' # Specify accrual time as a list, if maximum number of subjects need to be calculated
#' at <- list(
#'     "0 - <6"   = 20,
#'     "6 - <=10" = 30) 
#' getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at)
#' 
#' # Specify effect size for a two-stage group design with O'Brien & Fleming boundaries
#' # Effect size is based on event rates at specified event time, directionUpper = FALSE 
#' # needs to be specified because it should be shown that hazard ratio < 1
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), pi1 = 0.2, pi2 = 0.3, 
#'     eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, 
#'     directionUpper = FALSE)
#' 
#' # Effect size is based on event rate at specified event time for the reference group 
#' # and hazard ratio, directionUpper = FALSE needs to be specified 
#' # because it should be shown that hazard ratio < 1
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, 
#'     pi2 = 0.3, eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, 
#'     directionUpper = FALSE)
#' 
#' # Effect size is based on hazard rate for the reference group and hazard ratio, 
#' # directionUpper = FALSE needs to be specified because it should be shown that 
#' # hazard ratio < 1
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, 
#'     lambda2 = 0.02, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, 
#'     directionUpper = FALSE) 
#' 
#' # Specification of piecewise exponential survival time and hazard ratios  
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#'     hazardRatio = c(1.5, 1.8, 2),  maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # Specification of piecewise exponential survival time as list and hazard ratios 
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04)
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # Specification of piecewise exponential survival time for both treatment arms  
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#'     lambda1 = c(0.015,0.03,0.06),  maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # Specification of piecewise exponential survival time as a list
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04)
#' getPowerSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 200)
#' 
#' # Specify effect size based on median survival times
#' getPowerSurvival(median1 = 5, median2 = 3, 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE)
#' 
#' # Specify effect size based on median survival times of 
#' # Weibull distribtion with kappa = 2
#' getPowerSurvival(median1 = 5, median2 = 3, kappa = 2, 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE)
#' }
#' 
