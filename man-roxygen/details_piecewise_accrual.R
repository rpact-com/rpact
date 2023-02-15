#' @section Staggered patient entry:
#' \code{accrualTime} is the time period of subjects' accrual in a study. 
#' It can be a value that defines the end of accrual or a vector.
#' In this case, \code{accrualTime} can be used to define a non-constant accrual over time. 
#' For this, \code{accrualTime} is a vector that defines the accrual intervals. 
#' The first element of \code{accrualTime} must be equal to \code{0} and, additionally, 
#' \code{accrualIntensity} needs to be specified. 
#' \code{accrualIntensity} itself is a value or a vector (depending on the 
#' length of \code{accrualtime}) that defines the intensity how subjects 
#' enter the trial in the intervals defined through \code{accrualTime}.
#'  
#' \code{accrualTime} can also be a list that combines the definition of the accrual time and 
#' accrual intensity (see below and examples for details).
#'
#' If the length of \code{accrualTime} and the length of \code{accrualIntensity} are the same 
#' (i.e., the end of accrual is undefined), \code{maxNumberOfSubjects > 0} needs to be specified  
#' and the end of accrual is calculated.
#' In that case, \code{accrualIntensity} is the number of subjects per time unit, i.e., the absolute accrual intensity.
#' 
#' If the length of \code{accrualTime} equals the length of \code{accrualIntensity - 1}   
#' (i.e., the end of accrual is defined), \code{maxNumberOfSubjects} is calculated if the absolute accrual intensity is given.
#' If all elements in \code{accrualIntensity} are smaller than 1, \code{accrualIntensity} defines 
#' the *relative* intensity how subjects enter the trial.
#' For example, \code{accrualIntensity = c(0.1, 0.2)} specifies that in the second accrual interval 
#' the intensity is doubled as compared to the first accrual interval. The actual (absolute) accrual intensity 
#' is calculated for the calculated or given \code{maxNumberOfSubjects}.
#' Note that the default is \code{accrualIntensity = 0.1} meaning that the *absolute* accrual intensity 
#' will be calculated. 
