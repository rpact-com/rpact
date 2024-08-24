#' @examples
#' \dontrun{
#' # Fixed sample size trial with median survival 20 vs. 30 months in treatment and 
#' # reference group, respectively, alpha = 0.05 (two-sided), and power 1 - beta = 90%.
#' # 20 subjects will be recruited per month up to 400 subjects, i.e., accrual time 
#' # is 20 months.  
#' getSampleSizeSurvival(alpha = 0.05, sided = 2, beta = 0.1, lambda1 = log(2) / 20, 
#'     lambda2 = log(2) / 30, accrualTime = c(0,20), accrualIntensity = 20)
#' 
#' # Fixed sample size with minimum required definitions, pi1 = c(0.4,0.5,0.6) and 
#' # pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default, 
#' # only alpha = 0.01 is specified  
#' getSampleSizeSurvival(alpha = 0.01)
#' 
#' # Four stage O'Brien & Fleming group sequential design with minimum required 
#' # definitions, pi1 = c(0.4,0.5,0.6) and pi2 = 0.2 at event time 12, 
#' # accrual time 12 and follow-up time 6 as default  
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 4))
#' 
#' # For fixed sample design, determine necessary accrual time if 200 subjects and 
#' # 30 subjects per time unit can be recruited 
#' getSampleSizeSurvival(accrualTime = c(0), accrualIntensity = c(30), 
#'     maxNumberOfSubjects = 200)
#' 
#' # Determine necessary accrual time if 200 subjects and if the first 6 time units 
#' # 20 subjects per time unit can be recruited, then 30 subjects per time unit 
#' getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(20, 30), 
#'     maxNumberOfSubjects = 200)
#' 
#' # Determine maximum number of Subjects if the first 6 time units 20 subjects 
#' # per time unit can be recruited, and after 10 time units 30 subjects per time unit
#' getSampleSizeSurvival(accrualTime = c(0, 6, 10), accrualIntensity = c(20, 30))
#' 
#' # Specify accrual time as a list
#' at <- list(
#'     "0 - <6"  = 20,
#'     "6 - Inf" = 30)
#' getSampleSizeSurvival(accrualTime = at, maxNumberOfSubjects = 200)
#' 
#' # Specify accrual time as a list, if maximum number of subjects need to be calculated
#' at <- list(
#'     "0 - <6"   = 20,
#'     "6 - <=10" = 30)
#' getSampleSizeSurvival(accrualTime = at)
#' 
#' # Specify effect size for a two-stage group design with O'Brien & Fleming boundaries
#' # Effect size is based on event rates at specified event time 
#' # needs to be specified because it should be shown that hazard ratio < 1
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     pi1 = 0.2, pi2 = 0.3, eventTime = 24)
#' 
#' # Effect size is based on event rate at specified event 
#' # time for the reference group and hazard ratio 
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     hazardRatio = 0.5, pi2 = 0.3, eventTime = 24)
#' 
#' # Effect size is based on hazard rate for the reference group and hazard ratio
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     hazardRatio = 0.5, lambda2 = 0.02) 
#' 
#' # Specification of piecewise exponential survival time and hazard ratios  
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#'     hazardRatio = c(1.5, 1.8, 2))
#' 
#' # Specification of piecewise exponential survival time as a list and hazard ratios 
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04)
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))
#' 
#' # Specification of piecewise exponential survival time for both treatment arms
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#'     lambda1 = c(0.015, 0.03, 0.06))
#' 
#' # Specification of piecewise exponential survival time as a list
#' pws <- list(
#'     "0 - <5"  = 0.01,
#'     "5 - <10" = 0.02,
#'     ">=10"    = 0.04)
#' getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))
#' 
#' # Specify effect size based on median survival times
#' getSampleSizeSurvival(median1 = 5, median2 = 3)
#' 
#' # Specify effect size based on median survival times of Weibull distribtion with 
#' # kappa = 2
#' getSampleSizeSurvival(median1 = 5, median2 = 3, kappa = 2)
#' 
#' # Identify minimal and maximal required subjects to 
#' # reach the required events in spite of dropouts
#' getSampleSizeSurvival(accrualTime = c(0, 18), accrualIntensity = c(20, 30), 
#'     lambda2 = 0.4, lambda1 = 0.3, followUpTime = Inf, dropoutRate1 = 0.001, 
#'     dropoutRate2 = 0.005)
#' getSampleSizeSurvival(accrualTime = c(0, 18), accrualIntensity = c(20, 30), 
#'     lambda2 = 0.4, lambda1 = 0.3, followUpTime = 0, dropoutRate1 = 0.001, 
#'     dropoutRate2 = 0.005)
#' }
#' 
