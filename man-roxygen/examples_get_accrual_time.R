#' @examples 
#' \dontrun{
#' # Assume that in a trial the accrual after the first 6 months is doubled 
#' # and the total accrual time is 30 months.
#' # Further assume that a total of 1000 subjects are entered in the trial.
#' # The number of subjects to be accrued in the first 6 months and afterwards 
#' # is achieved through   
#' getAccrualTime(accrualTime = c(0, 6, 30), 
#'     accrualIntensity = c(0.1, 0.2), maxNumberOfSubjects = 1000)  
#' 
#' # The same result is obtained via the list based definition
#' getAccrualTime(list(
#'      "0 - <6"   = 0.1,
#'      "6 - <=30" = 0.2), 
#'      maxNumberOfSubjects = 1000)
#' 
#' # Calculate the end of accrual at given absolute intensity:
#' getAccrualTime(accrualTime = c(0, 6),
#'     accrualIntensity = c(18, 36), maxNumberOfSubjects = 1000)
#' 
#' # Via the list based definition this is
#' getAccrualTime(list(
#'     "0 - <6" = 18,
#'     ">=6" = 36), 
#'     maxNumberOfSubjects = 1000)
#'
#' # You can use an accrual time object in getSampleSizeSurvival() or 
#' # getPowerSurvival().
#' # For example, if the maximum number of subjects and the follow up 
#' # time needs to be calculated for a given effect size: 
#' accrualTime = getAccrualTime(accrualTime = c(0, 6, 30), 
#'     accrualIntensity = c(0.1, 0.2))
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' # Or if the power and follow up time needs to be calculated for given 
#' # number of events and subjects:
#' accrualTime = getAccrualTime(accrualTime = c(0, 6, 30),
#'     accrualIntensity = c(0.1, 0.2), maxNumberOfSubjects = 110)
#' getPowerSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2, 
#' maxNumberOfEvents = 46)
#' 
#' # How to show accrual time details
#' 
#' # You can use a sample size or power object as argument for the function 
#' # getAccrualTime():
#' sampleSize <- 
#' getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53),  
#'     lambda2 = 0.05, hazardRatio = 0.8, followUpTime = 6)
#' sampleSize
#' accrualTime <- getAccrualTime(sampleSize)
#' accrualTime
#' }
#' 
