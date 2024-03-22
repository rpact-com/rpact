library(rpact)

##   Fixed sample size   ###
alpha <- 0.025
beta <- 0.15
accrualTime <- 24/52
lambda1 <- 0.55
lambda2 <- 5.5
overdispersion <- (7.3 + 14) / 2 
directionUpper <- (lambda1 > lambda2)

design <- getDesignGroupSequential(typeOfDesign = "asHSD", gammaA = -4, futilityBounds = c(0, 0))

y <- getSampleSizeCounts(design = design, 
                         accrualTime = accrualTime,
                         lambda1 = lambda1, 
                         lambda2 = lambda2, 
                         overdispersion = overdispersion, 
                         fixedExposureTime = 24/52)
y$nFixed
y$calendarTime

z <- getPowerCounts(alpha = alpha, 
                    directionUpper = directionUpper,
                    maxNumberOfSubjects = 100,
                    lambda1 = lambda1,
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    fixedExposureTime = 24/52) 

z$overallReject

tictoc::tic()
s <- getSimulationCounts(design = design, 
                         directionUpper = directionUpper,
                         plannedMaxSubjects = 100,
                         plannedCalendarTime = as.numeric(y$calendarTime),
                         lambda1 = lambda2,
                         lambda2 = lambda2,
                         overdispersion = overdispersion,
                         maxNumberOfIterations = 10000,
                         accrualTime = accrualTime,
                         fixedExposureTime = 24/52)
s$overallReject
tictoc::toc()


##   Fixed sample size   ###
alpha <- 0.025
beta <- 0.1
accrualTime <- 12
lambda1 <- 0.6
lambda2 <- 0.3
overdispersion <- 2
directionUpper <- (lambda1 > lambda2)

# Case variable exposure
followUpTime <- 6
y <- getSampleSizeCounts(alpha = alpha, beta = beta,
                         lambda1 = lambda1, lambda2 = lambda2, overdispersion = overdispersion,
                         accrualTime = accrualTime, followUpTime = followUpTime)

y$calendarTime
y$nFixed


z <- getPowerCounts(alpha = alpha, 
                    directionUpper = directionUpper,
                    maxNumberOfSubjects = y$nFixed,
                    lambda1 = c(lambda2, lambda1),
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    accrualTime = accrualTime,
                    followUpTime = followUpTime) 

z$overallReject

tictoc::tic()
s <- getSimulationCounts(alpha = alpha, 
                   directionUpper = directionUpper,
                   plannedMaxSubjects = y$nFixed,
                   plannedCalendarTime = as.numeric(y$calendarTime),
                   lambda1 = c(lambda2, lambda1),
                   lambda2 = lambda2,
                   overdispersion = overdispersion,
                   maxNumberOfIterations = 500,
                   accrualTime = accrualTime,
                   followUpTime = followUpTime)
s$overallReject
tictoc::toc()


# Case fixed exposure
fixedExposureTime <- 1 
y <- getSampleSizeCounts(alpha = alpha, beta = beta,
                   lambda1 = lambda1, lambda2 = lambda2, overdispersion = overdispersion,
                   accrualTime = accrualTime, fixedExposureTime = fixedExposureTime)
y$calendarTime
y$nFixed

z <- getPowerCounts(alpha = alpha, 
                    directionUpper = directionUpper,
                    maxNumberOfSubjects = y$nFixed,
                    lambda1 = c(lambda2, lambda1),
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    fixedExposureTime = fixedExposureTime,
                    accrualTime = accrualTime
) 
z$overallReject

tictoc::tic()
s <- getSimulationCounts(alpha = alpha, 
                   directionUpper = directionUpper,
                   plannedMaxSubjects = y$nFixed,
                   plannedCalendarTime = y$calendarTime,
                   lambda1 = c(lambda2, lambda1),
                   lambda2 = lambda2,
                   overdispersion = overdispersion,
                   maxNumberOfIterations = 500,
                   fixedExposureTime = fixedExposureTime,
                   accrualTime = accrualTime)
s$overallReject
tictoc::toc()


#################################################################
#################################################################

## Group sequential design
alpha <- 0.025
beta <- 0.2
accrualTime <- 12
lambda1 <- 0.3
lambda2 <- 0.7
overdispersion <- 2
directionUpper <- (lambda1 > lambda2)
informationRates <- c(0.3, 0.55, 1)

design <- getDesignGroupSequential(informationRates = informationRates, alpha = alpha, beta = beta, 
                                   typeOfDesign = "asOF", typeBetaSpending = "bsOF", bindingFutility = TRUE)

# Case variable exposure
y <- getSampleSizeCounts(design,
                         lambda1 = lambda1, lambda2 = lambda2, overdispersion = overdispersion,
                         accrualTime = accrualTime, followUpTime = followUpTime 
)

y$calendarTime
y$maxNumberOfSubjects

z <- getPowerCounts(design = design,
                    directionUpper = directionUpper,
                    maxNumberOfSubjects = y$maxNumberOfSubjects,
                    lambda1 = c(lambda2, lambda1),
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    accrualTime = accrualTime,
                    followUpTime = followUpTime) 

z$rejectPerStage
z$futilityPerStage
z$overallReject

tictoc::tic()
s <- getSimulationCounts(design = design,
                         directionUpper = directionUpper,
                         plannedMaxSubjects = 400,
                         plannedCalendarTime = y$calendarTime,
                         lambda1 = c(lambda2),
                         lambda2 = lambda2,
                         overdispersion = overdispersion,
                         maxNumberOfIterations = 10000,
                         accrualTime = accrualTime,
                         followUpTime = followUpTime)

s$rejectPerStage
s$futilityPerStage
s$overallReject
tictoc::toc()

sqrt((1- 0.025)*0.025) / 100

# Case fixed exposure
fixedExposureTime <- 1 
y <- getSampleSizeCounts(design,
                         lambda1 = lambda1, lambda2 = lambda2, overdispersion = overdispersion,
                         accrualTime = accrualTime, fixedExposureTime = fixedExposureTime
)
y$calendarTime
y$maxNumberOfSubjects

z <- getPowerCounts(design = design,
                    directionUpper = directionUpper,
                    maxNumberOfSubjects = y$maxNumberOfSubjects,
                    lambda1 = c(lambda2, lambda1),
                    lambda2 = lambda2,
                    overdispersion = overdispersion,
                    fixedExposureTime = fixedExposureTime) 

z$rejectPerStage
z$futilityPerStage
z$overallReject

tictoc::tic()
s <- getSimulationCounts(design = design,
                         directionUpper = directionUpper,
                         plannedMaxSubjects = y$maxNumberOfSubjects,
                         plannedCalendarTime = y$calendarTime,
                         lambda1 = c(lambda2, lambda1),
                         lambda2 = lambda2,
                         overdispersion = overdispersion,
                         accrualTime = accrualTime,
                         fixedExposureTime = fixedExposureTime,
                         maxNumberOfIterations = 500
)
s$rejectPerStage
s$futilityPerStage
s$overallReject
tictoc::toc()
