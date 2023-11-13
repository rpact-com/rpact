## |
## |  *Count data utilities*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, Tobias Muetze, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File version: $Revision: 7428 $
## |  Last changed: $Date: 2023-11-13 10:42:22 +0100 (Mo, 13 Nov 2023) $
## |  Last changed by: $Author: pahlke $
## |

.assertIsValidEffectCountData <- function(
        sampleSizeEnabled,
        sided,
        lambda1,
        lambda2,
        lambda,
        theta,
        thetaH0,
        overDispersion) {
    .assertIsSingleInteger(sided, "sided", validateType = FALSE)
    if (sided != 1 && sided != 2) {
        stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'sided' (", sided, ") must be defined as 1 or 2")
    }
    .assertIsSingleNumber(lambda, "lambda", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda, "lambda", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsNumericVector(lambda1, "lambda1", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda1, "lambda1", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(lambda2, "lambda2", naAllowed = TRUE)
    .assertIsInOpenInterval(lambda2, "lambda2", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsNumericVector(theta, "theta", naAllowed = TRUE)
    .assertIsInOpenInterval(theta, "theta", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(thetaH0, "thetaH0")
    .assertIsInOpenInterval(thetaH0, "thetaH0", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(overDispersion, "overDispersion", naAllowed = TRUE)
    .assertIsInClosedInterval(overDispersion, "overDispersion", lower = 0, upper = NULL, naAllowed = TRUE)
    if (!is.na(lambda) && all(!is.na(theta))) {
        if (all(!is.na(lambda1)) || !is.na(lambda2)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda1' and/or 'lambda2' need not to be specified if 'lambda' and 'theta' are specified"
            )
        }
        if (length(theta) > 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "theta cannot be specified as vector if lambda is specified"
            )
        }
    } else if (!is.na(lambda2) && all(!is.na(theta))) {
        if (all(!is.na(lambda1)) || !is.na(lambda)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda1' and/or 'lambda' need not to be specified if 'lambda2' and 'theta' are specified"
            )
        }
    } else if (all(!is.na(lambda1)) && all(!is.na(theta))) {
        if (!is.na(lambda2) || !is.na(lambda)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda2' and/or 'lambda' need not to be specified if 'lambda1' and 'theta' are specified"
            )
        }
        if (length(theta) > 1) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "theta cannot be specified as vector if lambda1 is specified"
            )
        }
    } else if (all(!is.na(lambda1)) && !is.na(lambda2)) {
        if (!is.na(lambda) || all(!is.na(theta))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'lambda' and/or 'theta' need not to be specified if 'lambda1' and 'lambda2' are specified"
            )
        }
    } else if (!is.na(lambda) && all(!is.na(lambda1))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'lambda2' and/or 'theta' need not to be specified if 'lambda' and 'lambda1' are specified"
        )
    } else if (!is.na(lambda) && !is.na(lambda2)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'lambda1' and/or 'theta' need not to be specified if 'lambda' and 'lambda2' are specified"
        )
    } else if (sum(is.na(lambda2), any(is.na(lambda1)), is.na(lambda), any(is.na(theta))) != 2) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "this parameter configuration is not possible: exactly two of the ",
            "parameters 'lambda', 'lambda1', 'lambda2', 'theta' must be specified"
        )
    }
    if (!is.na(lambda2) && !any(is.na(theta))) {
        lambda1 <- lambda2 * theta
    } else if (!any(is.na(lambda1)) && !any(is.na(theta))) {
        lambda2 <- lambda1 / theta
    }
    if (!any(is.na(c(lambda1, lambda2))) && any(abs(lambda1 / lambda2 - thetaH0) < 1E-12) &&
            sampleSizeEnabled) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "any 'lambda1 / lambda2' (", .arrayToString(lambda1 / lambda2), ") must be != 'thetaH0' (", thetaH0, ")"
        )
    }
}

.assertIsValidParametersCountData <- function(
        sampleSizeEnabled,
        fixedExposureTime,
        followUpTime,
        accrualTime,
        accrualIntensity,
        maxNumberOfSubjects) {
    .assertIsSingleLogical(sampleSizeEnabled, "sampleSizeEnabled")
    .assertIsSingleNumber(fixedExposureTime, "fixedExposureTime", naAllowed = TRUE)
    .assertIsInOpenInterval(fixedExposureTime, "fixedExposureTime", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleNumber(followUpTime, "followUpTime", naAllowed = TRUE)
    .assertIsInClosedInterval(followUpTime, "followUpTime", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsNumericVector(accrualTime, "accrualTime", naAllowed = TRUE)
    .assertIsInClosedInterval(accrualTime, "accrualTime", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertValuesAreStrictlyIncreasing(accrualTime, "accrualTime")
    .assertIsNumericVector(accrualIntensity, "accrualIntensity", naAllowed = TRUE)
    .assertIsInClosedInterval(accrualIntensity, "accrualIntensity", lower = 0, upper = NULL, naAllowed = TRUE)
    .assertIsSingleInteger(maxNumberOfSubjects, "maxNumberOfSubjects", validateType = FALSE, naAllowed = TRUE)
    if (sampleSizeEnabled) {
        if (is.na(maxNumberOfSubjects) && any(is.na(accrualIntensity))) {
            if (!is.na(fixedExposureTime) && !is.na(followUpTime)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "'fixedExposureTime' and 'studyTime' cannot together be specified"
                )
            }
            if (is.na(fixedExposureTime) && is.na(followUpTime)) {
                stop(
                    C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                    "either 'fixedExposureTime' or 'followUpTime' needs to be specified"
                )
            }
        }
    } else {
        if (!is.na(fixedExposureTime) && !is.na(followUpTime)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'fixedExposureTime' or 'followUpTime' cannot together be specified"
            )
        }
        if (is.na(fixedExposureTime) && is.na(followUpTime)) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "either 'fixedExposureTime' or 'followUpTime' needs to be specified"
            )
        }
        if (is.na(maxNumberOfSubjects) && any(is.na(accrualIntensity))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "either 'maxNumberOfSubjects' or 'accrualIntensity' needs to be specified"
            )
        }
    }
    if (any(is.na(accrualTime)) && !is.na(followUpTime)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' needs to be specified if 'followUpTime' is given"
        )
    }
    if (sampleSizeEnabled) {
        if ((!is.na(maxNumberOfSubjects) || !any(is.na(accrualIntensity))) &&
                (!is.na(followUpTime) || !is.na(fixedExposureTime))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'followUpTime' and/or 'fixedExposureTime' is not allowed to be specified"
            )
        }
        if (!is.na(maxNumberOfSubjects) && any(is.na(accrualTime))) {
            stop(
                C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
                "'accrualTime' needs to be specified if 'maxNumberOfSubjects' is specified"
            )
        }
    }
    if (!is.na(maxNumberOfSubjects) && any(is.na(accrualTime))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' needs to be specified if 'maxNumberOfSubjects' is specified"
        )
    }
    if (!is.na(maxNumberOfSubjects) && !any(is.na(accrualIntensity))) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'maxNumberOfSubjects' and 'accrualIntensity' cannot be specified together"
        )
    }
    if (!any(is.na(accrualIntensity)) && (length(accrualIntensity) < 2)) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualIntensity' needs to be a vector with at least two elements"
        )
    }
    if (!any(is.na(accrualIntensity)) &&
            ((accrualTime[1] != 0) && (length(accrualTime) != length(accrualIntensity)) ||
                (accrualTime[1] == 0) && (length(accrualTime) - 1 != length(accrualIntensity)))
        ) {
        stop(
            C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
            "'accrualTime' and 'accrualIntensity' does not match"
        )
    }
}

.generateEventTimes <- function(
        recruit1, recruit2, accrualTime, followUpTime,
        lambda1, lambda2, overDispersion, fixedFollowUp = FALSE) {
    n1 <- length(recruit1)
    n2 <- length(recruit2)
    totalRecruitment <- c(recruit1, recruit2)

    if (fixedFollowUp) {
        followUp <- rep(followUpTime, times = n1 + n2)
    } else {
        followUp <- accrualTime + followUpTime - totalRecruitment
    }

    # generate number of events by subject
    nEvents <- rnbinom(
        n = n1 + n2,
        mu = c(lambda1 * followUp[1:n1], lambda2 * followUp[(n1 + 1):(n1 + n2)]),
        size = 1 / overDispersion
    )

    # generate event times through a homogeneous Poisson process
    output <- matrix(NA, nrow = sum(nEvents) + n1 + n2, ncol = 8)
    colnames(output) <- c(
        "id", "recruitTime", "startEvent", "stopEvent",
        "startCalendar", "stopCalendar", "status", "group"
    )
    index <- 1
    for (i in 1:(n1 + n2)) {
        if (nEvents[i] == 0) {
            output[index, c("id", "recruitTime", "startEvent", "stopEvent")] <-
                c(i, totalRecruitment[i], 0, followUp[i])
            output[index, "status"] <- c(0)
            output[index, "group"] <- if (i <= n1) 1 else 2
            index <- index + 1
        } else if (nEvents[i] != 0) {
            eventTime <- sort(runif(nEvents[i], min = 0, max = followUp[i]))
            indices <- index:(index + nEvents[i])
            output[indices, "id"] <- i
            output[indices, "recruitTime"] <- totalRecruitment[i]
            output[indices, "startEvent"] <- c(0, eventTime)
            output[indices, "stopEvent"] <- c(eventTime, followUp[i])
            output[indices, "status"] <- c(rep(1, times = nEvents[i]), 0)
            output[indices, "group"] <- if (i <= n1) 1 else 2
            index <- index + nEvents[i] + 1
        }
    }

    # calculate the calendar times
    output[, "startCalendar"] <- output[, "recruitTime"] + output[, "startEvent"]
    output[, "stopCalendar"] <- output[, "recruitTime"] + output[, "stopEvent"]

    return(list(output = output, nEvents = nEvents))
}

.getInformation <- function(
        lambda1,
        lambda2,
        overDispersion,
        recruit1,
        recruit2) {
    sumLambda1 <- sum(recruit1 * lambda1 /
        (1 + overDispersion * recruit1 * lambda1))
    sumLambda2 <- sum(recruit2 * lambda2 /
        (1 + overDispersion * recruit2 * lambda2))
    return(1 / (1 / sumLambda1 + 1 / sumLambda2))
}
