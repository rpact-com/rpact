## |
## |  *Unit tests*
## |
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |
## |  Contact us for information about our services: info@rpact.com
## |
## |  File name: test-f_design_plan_survival.R
## |  Creation date: 21 December 2023, 08:52:49
## |  File version: $Revision: 7547 $
## |  Last changed: $Date: 2024-01-10 08:13:40 +0100 (Mi, 10 Jan 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing the Sample Size Calculation of Survival Designs for Different Designs and Arguments")


test_that("'getSampleSizeSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default, only alpha = 0.01 is specified", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(alpha = 0.01)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(58.52451, 31.248898, 20.120262), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(16.482222, 7.5670212, 4.2761841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(58.52451, 31.248898, 20.120262), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(197.78666, 90.804254, 51.314209), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.8370942, 2.2986321, 2.821477), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'Schoenfeld'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 2
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 218.14225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 145.42817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 218.14225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 145.42817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.178521, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.542817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.357042, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.714085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 72.310048, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.043375, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.956243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.03082, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 187.52311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 62.507704, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 146.60794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 238.15931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.03082, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 109.95596, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 178.61948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 187.52311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 36.651986, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 59.539826, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 62.507704, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.835901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 14.391854, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 236.50498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 354.24994, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.12497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 177.12497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 354.24994, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.520829, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 404.85708, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 303.64281, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 101.21427, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 404.85708, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 303.64281, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 101.21427, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 33.73809, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 108.2069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 108.2069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 168.44491, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 216.4138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 27.051725, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 212.39441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 190.83096, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 247.45413, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 143.12322, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 185.5906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 47.70774, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 61.863534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.931767, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 242.70959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.91804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 177.91804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 276.96374, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 355.83608, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.47951, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 349.22724, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 313.77176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 406.87381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 235.32882, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 305.15536, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 78.442941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 101.71845, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 50.859227, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 399.07264, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 112.129, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 112.129, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 172.34323, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 224.258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 28.03225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 219.90797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 195.71655, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 257.43359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 146.78741, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 193.07519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 48.929138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 64.358398, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 32.179199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.771337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 252.26222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 184.36691, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 184.36691, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 283.37351, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 368.73381, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 46.091727, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 361.58134, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 321.80485, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 423.28243, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 241.35364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 317.46182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 80.451212, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 105.82061, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 52.910303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.771337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 414.77946, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 275.50245, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 359.78876, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 158.48615, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 206.97289, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 117.01629, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 152.81587, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 108.73874, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.973595, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.802401, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.747749, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 54.369372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 108.73874, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 108.13454, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 101.75403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 79.192297, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 352.72627, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'Freedman'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.04092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 393.13025, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 294.84769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 98.282562, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 393.13025, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 294.84769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 98.282562, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 32.760854, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 304.68325, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 395.08857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 228.51244, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 296.31643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 76.170813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 98.772142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 49.386071, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 387.51336, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1,
        pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1,
        dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 182.82647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 236.31869, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 69.22509, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 89.479288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 113.60138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 146.8394, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 62.770758, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.539836, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.846839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.554152, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 31.385379, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 62.770758, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 62.421971, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 58.738747, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 45.714713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 231.83649, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 5.3084847, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.4084373, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7178517, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival designs for one sided group sequential design and typeOfComputation = 'HsiehFreedman'", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.24552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 240.49104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.04092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 274.8469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 206.13518, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 68.711726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 274.8469, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 206.13518, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 68.711726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 22.903909, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.872604, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 213.01146, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 276.21601, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 159.75859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 207.16201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 53.252865, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 69.054003, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 34.527001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.832344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 270.92, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.818027, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 187.03142, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 244.2512, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 107.59211, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 140.50849, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 79.439306, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 103.74271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 73.819895, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.5314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$informationRates[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$informationRates[3, ], 1, label = paste0("c(", paste0(sampleSizeResult$informationRates[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 13.802401, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.763979, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.909947, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 73.819895, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 73.409713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 69.078154, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 53.761583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 239.45666, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$informationRates, sampleSizeResult$informationRates, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Schoenfeld'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.4,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 102.56356, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.281781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.281781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 102.56356, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 8.5469636, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 117.2155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 87.911625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.303875, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 117.2155, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 87.911625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.303875, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 9.7679584, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.511393, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.511393, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 80.187417, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 103.02279, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.877848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.672467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.108996, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 90.844192, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 117.79939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.133144, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 88.349544, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.711048, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.449848, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 14.724924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.623913, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 109.63825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.378489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.378489, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 82.043195, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 106.75698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.344622, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.606421, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 99.274467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Schoenfeld",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 79.764338, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.16718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 45.885412, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 59.923444, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 33.878926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 44.243734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 31.482385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.020897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 11.587598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.2964769, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.741192, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 31.482385, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 26.193621, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 23.694677, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 18.039036, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.778811, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Freedman'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.25,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 146.14538, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 73.072689, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 73.072689, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 86.438503, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 140.00653, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 146.14538, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.178781, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.987598, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 130.17577, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 235.6363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 176.72722, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 58.909074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 235.6363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 176.72722, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 58.909074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 19.636358, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 16.494541, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 41.236352, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 75.015902, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.069247, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 51.322759, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.33945377, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.56614959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.70454917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.9459092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.7663176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4193473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 112.6984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.09901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.334566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 137.49311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 182.62251, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 236.81008, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 136.96688, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 177.60756, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 45.655628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 59.20252, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 29.60126, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.287795, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 16.494541, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 41.236352, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 82.472703, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 75.015902, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 68.069247, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 51.322759, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 224.48637, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.33945377, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.56614959, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.70454917, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.9459092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.7663176, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4193473, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 75.020128, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 75.020128, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 115.30658, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 150.04026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.755032, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.271017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 142.14088, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "Freedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 109.58341, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 141.64583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 41.492467, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 53.632525, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 68.09094, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 88.013303, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 37.623838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$allocationRatioPlanned, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 17.705728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.304499, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 7.5247676, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 18.811919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 37.623838, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 34.222064, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 31.053018, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 23.413313, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.35397, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.23978557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.47145911, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.62947897, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 4.1703928, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 2.1210747, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5886154, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$allocationRatioPlanned, sampleSizeResult$allocationRatioPlanned, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
    getDesignCharacteristics(designGS2)
})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'HsiehFreedman'", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), alpha = 0.25,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        accountForObservationTimes = FALSE,
        typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.14667, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.073337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.073337, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], NA_real_, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.14667, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 12.012223, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 167.28361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.46271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 41.820903, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.088334, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 159.34095, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 167.28361, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 73.56625, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 119.50572, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 125.46271, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 24.522083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 39.835239, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 41.820903, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 13.940301, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.948104, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 148.45363, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 72.396041, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 112.6984, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 144.79208, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 18.09901, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.334566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.6488004, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 21.622001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 43.244002, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 39.334079, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 35.691647, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 26.91074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 137.49311, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 127.67583, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 165.55968, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 95.756873, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 124.16976, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 31.918958, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 41.389919, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 20.69496, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.287795, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 156.94387, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = c(0.3, 0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(493.77723, 130.94455), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(370.33292, 98.20841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(123.44431, 32.736137), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(79.299498, 21.529557), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(12.333995, 12.216859), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(602.41549, 162.84556), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.3, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 27.207015, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.025476782, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 1.5984103, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 493.77723, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 634.39599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 370.33292, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 475.79699, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 123.44431, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 158.599, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 167.01364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 79.299498, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267383, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.326085, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.333995, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 33.402728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 83.50682, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 167.01364, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 151.91304, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 137.84552, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 103.93258, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 602.41549, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.4680288, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.67047266, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.78185284, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1366206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.4914851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.2790131, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10,
        dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE,
        allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 130.94455, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], 172.23645, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 98.20841, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 129.17734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 32.736137, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 43.059113, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 21.529557, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 12.216859, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 11.531734, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 28.829335, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], 57.658669, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 52.445438, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 47.588863, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 35.880987, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 162.84556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.27467837, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50642065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65781752, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.6406215, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.974643, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5201784, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    sampleSizeResult <- getSampleSizeSurvival(designGS2,
        maxNumberOfSubjects = 0,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1,
        pi1 = c(0.3, 0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8,
        followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(493.77723, 130.94455), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(634.39599, 172.23645), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(370.33292, 98.20841), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(475.79699, 129.17734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(123.44431, 32.736137), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(158.599, 43.059113), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(79.299498, 21.529557), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(12.333995, 12.216859), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(602.41549, 162.84556), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
    # @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
    sampleSizeResult <- getSampleSizeSurvival(
        maxNumberOfSubjects = 468, designGS2,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = c(0.3, 0.4), pi2 = 0.2,
        eventTime = 14, accrualTime = 8, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(468, 468), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(351, 351), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(117, 117), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(426.26609, 212.47186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(468, 340.72461), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(468, 468), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(319.69957, 159.35389), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(351, 255.54346), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(351, 351), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects1[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(106.56652, 53.117965), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(117, 85.181153), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(117, 117), label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects2[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 58.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(16.753912, 0.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.22742698, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.38942935, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.28314367, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.61685633, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(7.2865998, 3.6319976), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(12.859517, 5.8243524), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[3, ], c(24.753912, 8.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(16.149347, 6.305235), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(24.753912, 8.380791), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(33.402728, 11.531734), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(83.50682, 28.829335), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[3, ], c(167.01364, 57.658669), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(151.91304, 52.445438), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(137.84552, 47.588863), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(103.93258, 35.880987), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(458.50858, 360.32124), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.4680288, 0.27467837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.67047266, 0.50642065), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.78185284, 0.65781752), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(2.1366206, 3.6406215), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4914851, 1.974643), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2790131, 1.5201784), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.057428091, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.11367628, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.16847851, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects1, sampleSizeResult$numberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects2, sampleSizeResult$numberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleLower, sampleSizeResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScaleUpper, sampleSizeResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesPValueScale, sampleSizeResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Sample Size Calculation of Survival Designs for Other Parameter Variants")


test_that("'getSampleSizeSurvival': For fixed sample design, determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(
        accrualTime = c(0), accrualIntensity = c(30),
        maxNumberOfSubjects = 120
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.350651, 4.1854022, 1.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(18.350651, 8.1854022, 5.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(18.350651, 8.1854022, 5.0840261), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(
        beta = 0.01, accrualTime = c(0, 4), accrualIntensity = c(10, 20),
        maxNumberOfSubjects = 180
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(107.13798, 57.20584, 36.833186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 11, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(27.319035, 6.0447949, 0.58657023), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(107.13798, 57.20584, 36.833186), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(180, 180, 180), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(90, 90, 90), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(90, 90, 90), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(38.319035, 17.044795, 11.58657), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(38.319035, 17.044795, 11.58657), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.4603989, 1.6791239, 1.9076838), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0, 3, 5), accrualIntensity = c(20, 30))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify accrual time as a list", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    at <- list("0 - <3" = 20, "3 - Inf" = 30)
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = at, maxNumberOfSubjects = 120)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    at <- list("0 - <3" = 20, "3 - <=5" = 30)
    sampleSizeResult <- getSampleSizeSurvival(accrualTime = at)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 5, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$followUpTime, c(14.113265, 3.9529427, 0.85781252), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, c(120, 120, 120), label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, c(60, 60, 60), label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(19.113265, 8.9529427, 5.8578125), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$followUpTime, sampleSizeResult$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, pi2 = 0.3, eventTime = 24)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$pi1, 0.16333997, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$pi1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 93.281194, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 46.640597, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.007430728, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 532.72433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 266.36217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 266.36217, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 524.59793, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 532.72433, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 44.393694, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 11.816947, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 16.704001, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 531.021, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$pi1, sampleSizeResult$pi1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda2, sampleSizeResult$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Effect size is based on hazard rate for the reference group and hazard ratio", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, lambda2 = 0.02)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$lambda1, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.47112, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 203.23556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 203.23556, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 398.17083, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.47112, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 33.872594, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 11.754955, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 16.691007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 404.73134, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$lambda1, sampleSizeResult$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time for both treatment arms", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), lambda1 = c(0.015, 0.03, 0.06)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 1.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, 381.35099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, 381.35099, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 192.45497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 63.558499, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 13.350554, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 17.025453, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], 96.227483, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], 192.45497, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, 192.20642, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, 189.01379, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, 172.2852, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 762.70199, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.76855, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3298684, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    sampleSizeResult <- getSampleSizeSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$earlyStop, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18), label = paste0("c(", paste0(sampleSizeResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH0, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH01, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedEventsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$expectedNumberOfSubjectsH1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects1, sampleSizeResult$maxNumberOfSubjects1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects2, sampleSizeResult$maxNumberOfSubjects2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$numberOfSubjects, sampleSizeResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$rejectPerStage, sampleSizeResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$earlyStop, sampleSizeResult$earlyStop, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxStudyDuration, sampleSizeResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsPerStage, sampleSizeResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH0, sampleSizeResult$expectedEventsH0, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH01, sampleSizeResult$expectedEventsH01, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedEventsH1, sampleSizeResult$expectedEventsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$expectedNumberOfSubjectsH1, sampleSizeResult$expectedNumberOfSubjectsH1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult <- getSampleSizeSurvival(lambda1 = log(2) / 5, lambda2 = log(2) / 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 5, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 3, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.6, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 120.3157, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 11.772201, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 120.3157, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 141.26641, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 70.633206, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 70.633206, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-06, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-06)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-06)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult2 <- getSampleSizeSurvival(median1 = 5, median2 = 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.13862944, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.23104906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.6, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 11.772201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 141.26641, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 70.633206, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult <- getSampleSizeSurvival(
        lambda1 = getLambdaByMedian(median = 5, kappa = 2),
        lambda2 = getLambdaByMedian(median = 3, kappa = 2), kappa = 2
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 5, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 3, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.36, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 2.6040472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 31.248566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.48932026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    sampleSizeResult2 <- getSampleSizeSurvival(median1 = 5, median2 = 3, kappa = 2)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.16651092, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.2775182, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.36, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 2.6040472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 30.078926, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 31.248566, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 15.624283, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.48932026, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Specify effect size based on rates with kappa = 3", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabypi}
    sampleSizeResult <- getSampleSizeSurvival(
        lambda1 = (-log(1 - 0.23))^(1 / 3) / 14,
        lambda2 = (-log(1 - 0.38))^(1 / 3) / 14, kappa = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 19.378531, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 15.845881, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.54674726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualIntensity, 30.926108, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 371.1133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.65547761, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualIntensity, sampleSizeResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabypi}
    sampleSizeResult2 <- getSampleSizeSurvival(pi1 = 0.23, pi2 = 0.38, eventTime = 14, kappa = 3)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
    expect_equal(sampleSizeResult2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$median1, 19.378531, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$median2, 15.845881, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda1, 0.045668945, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$lambda2, 0.055850291, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$hazardRatio, 0.54674726, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$maxNumberOfEvents, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$accrualIntensity, 30.926108, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$eventsFixed, 86.124472, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed, 371.1133, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed1, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$nFixed2, 185.55665, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$analysisTime[1, ], 18, label = paste0("c(", paste0(sampleSizeResult2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$studyDuration, 18, label = paste0("c(", paste0(sampleSizeResult2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.65547761, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult2), NA)))
        expect_output(print(sampleSizeResult2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult2), NA)))
        expect_output(summary(sampleSizeResult2)$show())
        sampleSizeResult2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult2CodeBased$directionUpper, sampleSizeResult2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$median1, sampleSizeResult2$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$median2, sampleSizeResult2$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda1, sampleSizeResult2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$lambda2, sampleSizeResult2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$hazardRatio, sampleSizeResult2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$maxNumberOfEvents, sampleSizeResult2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$accrualIntensity, sampleSizeResult2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$eventsFixed, sampleSizeResult2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed, sampleSizeResult2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed1, sampleSizeResult2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$nFixed2, sampleSizeResult2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$analysisTime, sampleSizeResult2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$studyDuration, sampleSizeResult2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult2CodeBased$criticalValuesEffectScale, sampleSizeResult2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult2), "character")
        df <- as.data.frame(sampleSizeResult2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': Calculation of maximum number of subjects for given follow-up time", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22, 53),
        lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 5
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
    expect_equal(sampleSizeResult$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfSubjects, 477.30924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$accrualTime, c(6, 12.515269), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$totalAccrualTime, 12.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed, 477.30924, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed1, 238.65462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$nFixed2, 238.65462, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$analysisTime[1, ], 17.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult$studyDuration, 17.515269, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult), NA)))
        expect_output(print(sampleSizeResult)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult), NA)))
        expect_output(summary(sampleSizeResult)$show())
        sampleSizeResultCodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResultCodeBased$directionUpper, sampleSizeResult$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median1, sampleSizeResult$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$median2, sampleSizeResult$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$hazardRatio, sampleSizeResult$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfSubjects, sampleSizeResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$maxNumberOfEvents, sampleSizeResult$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$accrualTime, sampleSizeResult$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$totalAccrualTime, sampleSizeResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$eventsFixed, sampleSizeResult$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed, sampleSizeResult$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed1, sampleSizeResult$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$nFixed2, sampleSizeResult$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$analysisTime, sampleSizeResult$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$studyDuration, sampleSizeResult$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResultCodeBased$criticalValuesEffectScale, sampleSizeResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult), "character")
        df <- as.data.frame(sampleSizeResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult3 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult3' with expected results
    expect_equal(sampleSizeResult3$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult3$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult3$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult3), NA)))
        expect_output(print(sampleSizeResult3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult3), NA)))
        expect_output(summary(sampleSizeResult3)$show())
        sampleSizeResult3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult3CodeBased$directionUpper, sampleSizeResult3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$median1, sampleSizeResult3$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$median2, sampleSizeResult3$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$hazardRatio, sampleSizeResult3$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$maxNumberOfSubjects, sampleSizeResult3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$maxNumberOfEvents, sampleSizeResult3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$followUpTime, sampleSizeResult3$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$eventsFixed, sampleSizeResult3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed, sampleSizeResult3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed1, sampleSizeResult3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$nFixed2, sampleSizeResult3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$analysisTime, sampleSizeResult3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$studyDuration, sampleSizeResult3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult3CodeBased$criticalValuesEffectScale, sampleSizeResult3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult3), "character")
        df <- as.data.frame(sampleSizeResult3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult4 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult4' with expected results
    expect_equal(sampleSizeResult4$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult4$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult4), NA)))
        expect_output(print(sampleSizeResult4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult4), NA)))
        expect_output(summary(sampleSizeResult4)$show())
        sampleSizeResult4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult4CodeBased$directionUpper, sampleSizeResult4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$median1, sampleSizeResult4$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$median2, sampleSizeResult4$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$hazardRatio, sampleSizeResult4$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$maxNumberOfSubjects, sampleSizeResult4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$maxNumberOfEvents, sampleSizeResult4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$followUpTime, sampleSizeResult4$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$eventsFixed, sampleSizeResult4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed, sampleSizeResult4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed1, sampleSizeResult4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$nFixed2, sampleSizeResult4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$analysisTime, sampleSizeResult4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$studyDuration, sampleSizeResult4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult4CodeBased$criticalValuesEffectScale, sampleSizeResult4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult4), "character")
        df <- as.data.frame(sampleSizeResult4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult6 <- getSampleSizeSurvival(
        accrualTime = c(0), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01), maxNumberOfSubjects = 300
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult6' with expected results
    expect_equal(sampleSizeResult6$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult6$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$followUpTime, 9.9154676, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed, 300, label = paste0("c(", paste0(sampleSizeResult6$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed1, 150, label = paste0("c(", paste0(sampleSizeResult6$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$nFixed2, 150, label = paste0("c(", paste0(sampleSizeResult6$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$analysisTime[1, ], 23.551831, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$studyDuration, 23.551831, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult6$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult6$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult6), NA)))
        expect_output(print(sampleSizeResult6)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult6), NA)))
        expect_output(summary(sampleSizeResult6)$show())
        sampleSizeResult6CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult6, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult6CodeBased$directionUpper, sampleSizeResult6$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$median1, sampleSizeResult6$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$median2, sampleSizeResult6$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$hazardRatio, sampleSizeResult6$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$maxNumberOfEvents, sampleSizeResult6$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$followUpTime, sampleSizeResult6$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$eventsFixed, sampleSizeResult6$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed, sampleSizeResult6$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed1, sampleSizeResult6$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$nFixed2, sampleSizeResult6$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$analysisTime, sampleSizeResult6$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$studyDuration, sampleSizeResult6$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult6CodeBased$criticalValuesEffectScale, sampleSizeResult6$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult6), "character")
        df <- as.data.frame(sampleSizeResult6)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult6)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult7 <- getSampleSizeSurvival(
        accrualTime = c(0, 3), accrualIntensity = c(22, 53),
        lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 44
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult7' with expected results
    expect_equal(sampleSizeResult7$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult7$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$maxNumberOfSubjects, 135.32074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$accrualTime, c(3, 4.3079386), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$totalAccrualTime, 4.3079386, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed, 135.32074, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed1, 67.660372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$nFixed2, 67.660372, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$analysisTime[1, ], 48.307942, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$studyDuration, 48.307942, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult7$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult7$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult7), NA)))
        expect_output(print(sampleSizeResult7)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult7), NA)))
        expect_output(summary(sampleSizeResult7)$show())
        sampleSizeResult7CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult7, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult7CodeBased$directionUpper, sampleSizeResult7$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$median1, sampleSizeResult7$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$median2, sampleSizeResult7$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$hazardRatio, sampleSizeResult7$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$maxNumberOfSubjects, sampleSizeResult7$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$maxNumberOfEvents, sampleSizeResult7$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$accrualTime, sampleSizeResult7$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$totalAccrualTime, sampleSizeResult7$totalAccrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$eventsFixed, sampleSizeResult7$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed, sampleSizeResult7$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed1, sampleSizeResult7$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$nFixed2, sampleSizeResult7$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$analysisTime, sampleSizeResult7$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$studyDuration, sampleSizeResult7$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult7CodeBased$criticalValuesEffectScale, sampleSizeResult7$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult7), "character")
        df <- as.data.frame(sampleSizeResult7)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult7)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeResult8 <- getSampleSizeSurvival(
        accrualTime = c(0, 6), accrualIntensity = c(22),
        lambda2 = 0.02, lambda1 = c(0.01)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult8' with expected results
    expect_equal(sampleSizeResult8$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeResult8$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$hazardRatio, 0.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$maxNumberOfSubjects, 132, label = paste0("c(", paste0(sampleSizeResult8$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$maxNumberOfEvents, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$followUpTime, 44.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$eventsFixed, 65.345659, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed, 132, label = paste0("c(", paste0(sampleSizeResult8$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed1, 66, label = paste0("c(", paste0(sampleSizeResult8$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$nFixed2, 66, label = paste0("c(", paste0(sampleSizeResult8$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$analysisTime[1, ], 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$studyDuration, 50.431065, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeResult8$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeResult8$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeResult8), NA)))
        expect_output(print(sampleSizeResult8)$show())
        invisible(capture.output(expect_error(summary(sampleSizeResult8), NA)))
        expect_output(summary(sampleSizeResult8)$show())
        sampleSizeResult8CodeBased <- eval(parse(text = getObjectRCode(sampleSizeResult8, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeResult8CodeBased$directionUpper, sampleSizeResult8$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$median1, sampleSizeResult8$median1, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$median2, sampleSizeResult8$median2, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$hazardRatio, sampleSizeResult8$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$maxNumberOfSubjects, sampleSizeResult8$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$maxNumberOfEvents, sampleSizeResult8$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$followUpTime, sampleSizeResult8$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$eventsFixed, sampleSizeResult8$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed, sampleSizeResult8$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed1, sampleSizeResult8$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$nFixed2, sampleSizeResult8$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$analysisTime, sampleSizeResult8$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$studyDuration, sampleSizeResult8$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeResult8CodeBased$criticalValuesEffectScale, sampleSizeResult8$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeResult8), "character")
        df <- as.data.frame(sampleSizeResult8)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeResult8)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': analysis time at last stage equals accrual time + follow-up time", {
    .skipTestIfDisabled()

    x1 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12, maxNumberOfSubjects = 766,
        pi2 = 0.05, pi1 = 0.1
    )
    expect_equal(x1$analysisTime[3], x1$accrualTime + x1$followUpTime)

    x2 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12, maxNumberOfSubjects = 766,
        lambda2 = 0.005, lambda1 = 0.01
    )

    expect_equal(x2$analysisTime[3], x2$accrualTime + x2$followUpTime)

    x3 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0),
        accrualTime = c(0, 12, 15), accrualIntensity = c(20, 30),
        lambda2 = 0.005, lambda1 = 0.01
    )

    expect_equal(x3$analysisTime[length(x3$analysisTime)], x3$accrualTime[length(x3$accrualTime)] + x3$followUpTime)

    x4 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0),
        accrualTime = c(0, 12, 15), accrualIntensity = c(40, 60),
        piecewiseSurvivalTime = c(0, 5), lambda2 = c(0.005, 0.01), hazardRatio = 0.8
    )

    expect_equal(x4$analysisTime[length(x4$analysisTime)], x4$accrualTime[length(x4$accrualTime)] + x4$followUpTime)
})

test_that("'getSampleSizeSurvival': follow-up time is equal for different argument-target constellations", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.2, 0.5, 1), sided = 1,
        beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3
    )

    x5 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 14, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )
    x6 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 14, accrualTime = 8, maxNumberOfSubjects = x5$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 1
    )
    expect_equal(x5$followUpTime, x6$followUpTime)

    .skipTestIfDisabled()

    x7 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1, median1 = 44, median2 = 66,
        accrualTime = 43, followUpTime = 22,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 2
    )
    x8 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "Freedman", thetaH0 = 1, median1 = 44, median2 = 66,
        accrualTime = 43, maxNumberOfSubjects = x7$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 2
    )
    expect_equal(x7$followUpTime, x8$followUpTime)

    x9 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 16, accrualTime = 8, followUpTime = 10,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 0.2
    )
    x10 <- getSampleSizeSurvival(designGS1,
        typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2,
        eventTime = 16, accrualTime = 8, maxNumberOfSubjects = x9$maxNumberOfSubjects,
        accountForObservationTimes = TRUE, allocationRatioPlanned = 0.2
    )
    expect_equal(x9$followUpTime, x10$followUpTime)
})

test_that("'getSampleSizeSurvival': testing expected warnings and errors", {
    .skipTestIfDisabled()

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, lambda1 = c(0.01, 0.015), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'lambda1'; lambda1 = c(0.01, 0.015)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, median1 = c(5, 6), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'lambda1'; lambda1 = c(0.139, 0.116)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            median2 = 4, median1 = c(5, 6), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'median1'; median1 = c(5, 6)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            pi2 = 0.213, pi1 = c(0.113, 0.165), followUpTime = 5
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'pi1'; pi1 = c(0.113, 0.165)"
        ),
        fixed = TRUE
    )

    expect_error(
        getSampleSizeSurvival(
            accrualTime = c(0), pi1 = c(0.4, 0.5),
            accrualIntensity = c(22), followUpTime = 6
        ),
        paste0(
            "Illegal argument: the calculation of 'maxNumberOfSubjects' for given 'followUpTime' ",
            "is only available for a single 'pi1'; pi1 = c(0.4, 0.5)"
        ),
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = -1, hazardRatio = 2),
        "Argument out of bounds: 'lambda2' (-1) must be >= 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = 0, hazardRatio = 2),
        "Illegal argument: 'lambda2' (0) not allowed: at least one lambda value must be > 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(lambda2 = 0.9, hazardRatio = 0.8, kappa = 0),
        "Argument out of bounds: 'kappa' (0) must be > 0",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(pi1 = getPiByMedian(0.1), pi2 = getPiByMedian(0.2)))

    expect_warning(getSampleSizeSurvival(median1 = 0.1, median2 = 0.2, eventTime = 0.5),
        "'eventTime' (0.5) will be ignored",
        fixed = TRUE
    )

    expect_error(getSampleSizeSurvival(
        lambda2 = 0.2, hazardRatio = c(0.6, 0.7),
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_error(getSampleSizeSurvival(
        lambda1 = c(0.02, 0.03), lambda2 = 0.2, hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_error(getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = c(0.6, 0.8),
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    ))

    expect_warning(getSampleSizeSurvival(median1 = 0.1, median2 = 0.2, eventTime = 4),
        "'eventTime' (4) will be ignored",
        fixed = TRUE
    )

    .skipTestIfDisabled()

    expect_warning(
        getSampleSizeSurvival(
            accrualTime = c(0, 6), accrualIntensity = c(22, 53),
            lambda2 = 0.02, lambda1 = c(0.01), followUpTime = -1
        ),
        "Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -1",
        fixed = TRUE
    )

    expect_warning(
        getSampleSizeSurvival(
            accrualTime = c(0, 6, 30), pi1 = 0.4,
            accrualIntensity = c(0.22, 0.53), maxNumberOfSubjects = 1000
        ),
        "Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -17.501",
        fixed = TRUE
    )
})

test_plan_section("Testing Other Functions of the Sample Size Calculator for Survival Designs")


test_that("'getEventProbabilities': check expected events over time for overall survival (case 1)", {
    .skipTestIfDisabled()

    design <- getDesignGroupSequential(
        sided = 1, alpha = 0.025, beta = 0.2,
        informationRates = c(0.33, 0.7, 1),
        futilityBounds = c(0, 0),
        bindingFutility = FALSE
    )

    piecewiseSurvivalTime <- list(
        "0 - <6"   = 0.025,
        "6 - <9"   = 0.04,
        "9 - <15"  = 0.015,
        "15 - <21" = 0.01,
        ">=21"     = 0.007
    )

    accrualTime <- list(
        "0  - <12" = 15,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    powerResults <- getPowerSurvival(
        design = design, typeOfComputation = "Schoenfeld",
        thetaH0 = 1, directionUpper = FALSE,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        allocationRatioPlanned = 1,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTime,
        hazardRatio = seq(0.6, 1, 0.05),
        maxNumberOfEvents = 404,
        maxNumberOfSubjects = 1405
    )

    piecewiseSurvivalTimeOS <- list(
        "0  - <14" = 0.015,
        "14 - <24" = 0.01,
        "24 - <44" = 0.005,
        ">=44"     = 0.0025
    )

    timeOS <- c(powerResults$analysisTime[2:3, 4], 17 + 3.5 * 12)
    eventsOS <- getEventProbabilities(
        timeOS,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
        allocationRatioPlanned = 1, hazardRatio = 0.8,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        maxNumberOfSubjects = 1405
    )$cumulativeEventProbabilities
    eventsOS <- eventsOS * 1405

    expect_equal(round(timeOS, 2), c(37.60, 46.72, 59.00))
    expect_equal(round(eventsOS, 1), c(194.1, 288.7, 365.1))
})

test_that("'getEventProbabilities': check expected events over time for overall survival (case 2)", {
    .skipTestIfDisabled()

    accrualTime <- list(
        "0  - <12" = 15,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    piecewiseSurvivalTimeOS <- list(
        "0  - <14" = 0.015,
        "14 - <24" = 0.01,
        "24 - <44" = 0.005,
        ">=44"     = 0.0025
    )

    timeOS <- c(37.59823, 46.71658, 59)
    eventsOS <- getEventProbabilities(
        timeOS,
        accrualTime = accrualTime,
        piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
        allocationRatioPlanned = 1, hazardRatio = 0.8,
        dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
        maxNumberOfSubjects = 1405
    )

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvivalWithDropOuts}

    ## Comparison of the results of EventProbabilities object 'eventsOS' with expected results
    expect_equal(eventsOS$lambda1, c(0.012, 0.008, 0.004, 0.002), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$lambda1, collapse = ", "), ")"))
    expect_equal(eventsOS$cumulativeEventProbabilities, c(0.13811859, 0.20546928, 0.2598385), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$cumulativeEventProbabilities, collapse = ", "), ")"))
    expect_equal(eventsOS$eventProbabilities1, c(0.12437783, 0.18544801, 0.23527681), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$eventProbabilities1, collapse = ", "), ")"))
    expect_equal(eventsOS$eventProbabilities2, c(0.15185935, 0.22549055, 0.28440019), tolerance = 1e-07, label = paste0("c(", paste0(eventsOS$eventProbabilities2, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(eventsOS), NA)))
        expect_output(print(eventsOS)$show())
        invisible(capture.output(expect_error(summary(eventsOS), NA)))
        expect_output(summary(eventsOS)$show())
        eventsOSCodeBased <- eval(parse(text = getObjectRCode(eventsOS, stringWrapParagraphWidth = NULL)))
        expect_equal(eventsOSCodeBased$lambda1, eventsOS$lambda1, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$cumulativeEventProbabilities, eventsOS$cumulativeEventProbabilities, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$eventProbabilities1, eventsOS$eventProbabilities1, tolerance = 1e-07)
        expect_equal(eventsOSCodeBased$eventProbabilities2, eventsOS$eventProbabilities2, tolerance = 1e-07)
        expect_type(names(eventsOS), "character")
        df <- as.data.frame(eventsOS)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(eventsOS)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getNumberOfSubjects': check the number of recruited subjects at given time vector", {
    .skipTestIfDisabled()

    accrualTime1 <- list(
        "0  - <12" = 12,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39,
        ">=16"     = 45
    )

    numberOfSubjects1 <- getNumberOfSubjects(
        time = 1:3,
        accrualTime = getAccrualTime(accrualTime1, maxNumberOfSubjects = 1405)
    )

    ## Comparison of the results of NumberOfSubjects object 'numberOfSubjects1' with expected results
    expect_equal(numberOfSubjects1$numberOfSubjects, c(12, 24, 36), tolerance = 1e-07, label = paste0("c(", paste0(numberOfSubjects1$numberOfSubjects, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(numberOfSubjects1), NA)))
        expect_output(print(numberOfSubjects1)$show())
        invisible(capture.output(expect_error(summary(numberOfSubjects1), NA)))
        expect_output(summary(numberOfSubjects1)$show())
        numberOfSubjects1CodeBased <- eval(parse(text = getObjectRCode(numberOfSubjects1, stringWrapParagraphWidth = NULL)))
        expect_equal(numberOfSubjects1CodeBased$numberOfSubjects, numberOfSubjects1$numberOfSubjects, tolerance = 1e-07)
        expect_type(names(numberOfSubjects1), "character")
        df <- as.data.frame(numberOfSubjects1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(numberOfSubjects1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    accrualTime2 <- list(
        "0  - <12" = 12,
        "12 - <13" = 21,
        "13 - <14" = 27,
        "14 - <15" = 33,
        "15 - <16" = 39
    )

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    numberOfSubjects2 <- getNumberOfSubjects(time = 1:3, accrualTime = getAccrualTime(accrualTime2))

    ## Comparison of the results of NumberOfSubjects object 'numberOfSubjects2' with expected results
    expect_equal(numberOfSubjects2$maxNumberOfSubjects, 264, label = paste0("c(", paste0(numberOfSubjects2$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(numberOfSubjects2$numberOfSubjects, c(12, 24, 36), label = paste0("c(", paste0(numberOfSubjects2$numberOfSubjects, collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(numberOfSubjects2), NA)))
        expect_output(print(numberOfSubjects2)$show())
        invisible(capture.output(expect_error(summary(numberOfSubjects2), NA)))
        expect_output(summary(numberOfSubjects2)$show())
        numberOfSubjects2CodeBased <- eval(parse(text = getObjectRCode(numberOfSubjects2, stringWrapParagraphWidth = NULL)))
        expect_equal(numberOfSubjects2CodeBased$maxNumberOfSubjects, numberOfSubjects2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(numberOfSubjects2CodeBased$numberOfSubjects, numberOfSubjects2$numberOfSubjects, tolerance = 1e-07)
        expect_type(names(numberOfSubjects2), "character")
        df <- as.data.frame(numberOfSubjects2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(numberOfSubjects2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': check the calculation of 'maxNumberOfSubjects' for given 'followUpTime'", {
    .skipTestIfDisabled()

    sampleSizeSurvival1 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival1' with expected results
    expect_equal(sampleSizeSurvival1$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival1$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival1), NA)))
        expect_output(print(sampleSizeSurvival1)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival1), NA)))
        expect_output(summary(sampleSizeSurvival1)$show())
        sampleSizeSurvival1CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival1, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival1CodeBased$directionUpper, sampleSizeSurvival1$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda1, sampleSizeSurvival1$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfSubjects, sampleSizeSurvival1$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfEvents, sampleSizeSurvival1$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$accrualTime, sampleSizeSurvival1$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$eventsFixed, sampleSizeSurvival1$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed, sampleSizeSurvival1$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed1, sampleSizeSurvival1$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed2, sampleSizeSurvival1$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$analysisTime, sampleSizeSurvival1$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$studyDuration, sampleSizeSurvival1$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$criticalValuesEffectScale, sampleSizeSurvival1$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival1), "character")
        df <- as.data.frame(sampleSizeSurvival1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival2 <- getSampleSizeSurvival(
        piecewiseSurvivalTime = list(
            "<12"  = 0.02,
            ">=12" = 0.03
        ), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival2' with expected results
    expect_equal(sampleSizeSurvival2$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival2), NA)))
        expect_output(print(sampleSizeSurvival2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival2), NA)))
        expect_output(summary(sampleSizeSurvival2)$show())
        sampleSizeSurvival2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival2CodeBased$directionUpper, sampleSizeSurvival2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda1, sampleSizeSurvival2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfSubjects, sampleSizeSurvival2$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfEvents, sampleSizeSurvival2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$accrualTime, sampleSizeSurvival2$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$eventsFixed, sampleSizeSurvival2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed, sampleSizeSurvival2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed1, sampleSizeSurvival2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed2, sampleSizeSurvival2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$analysisTime, sampleSizeSurvival2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$studyDuration, sampleSizeSurvival2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$criticalValuesEffectScale, sampleSizeSurvival2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival2), "character")
        df <- as.data.frame(sampleSizeSurvival2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival3 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival3' with expected results
    expect_equal(sampleSizeSurvival3$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda1, c(0.012, 0.018), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfSubjects, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfEvents, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$accrualTime, 16.155013, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$eventsFixed, 120.3157, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed, 484.65038, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed1, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed2, 242.32519, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$analysisTime[1, ], 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$studyDuration, 24.155014, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival3), NA)))
        expect_output(print(sampleSizeSurvival3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival3), NA)))
        expect_output(summary(sampleSizeSurvival3)$show())
        sampleSizeSurvival3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival3CodeBased$directionUpper, sampleSizeSurvival3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda1, sampleSizeSurvival3$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfSubjects, sampleSizeSurvival3$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfEvents, sampleSizeSurvival3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$accrualTime, sampleSizeSurvival3$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$eventsFixed, sampleSizeSurvival3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed, sampleSizeSurvival3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed1, sampleSizeSurvival3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed2, sampleSizeSurvival3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$analysisTime, sampleSizeSurvival3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$studyDuration, sampleSizeSurvival3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$criticalValuesEffectScale, sampleSizeSurvival3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival3), "character")
        df <- as.data.frame(sampleSizeSurvival3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival4 <- getSampleSizeSurvival(
        lambda2 = c(0.02, 0.03),
        piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.8,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival4' with expected results
    expect_equal(sampleSizeSurvival4$directionUpper, FALSE, label = paste0("c(", paste0(sampleSizeSurvival4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda1, c(0.016, 0.024), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfSubjects, 1325.4661, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfEvents, 630.52017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$accrualTime, 44.182203, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$eventsFixed, 630.52017, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed, 1325.4661, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed1, 662.73305, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed2, 662.73305, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$analysisTime[1, ], 52.182201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$studyDuration, 52.182201, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$criticalValuesEffectScale[1, ], 0.85546574, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival4), NA)))
        expect_output(print(sampleSizeSurvival4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival4), NA)))
        expect_output(summary(sampleSizeSurvival4)$show())
        sampleSizeSurvival4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival4CodeBased$directionUpper, sampleSizeSurvival4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda1, sampleSizeSurvival4$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfSubjects, sampleSizeSurvival4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfEvents, sampleSizeSurvival4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$accrualTime, sampleSizeSurvival4$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$eventsFixed, sampleSizeSurvival4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed, sampleSizeSurvival4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed1, sampleSizeSurvival4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed2, sampleSizeSurvival4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$analysisTime, sampleSizeSurvival4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$studyDuration, sampleSizeSurvival4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$criticalValuesEffectScale, sampleSizeSurvival4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival4), "character")
        df <- as.data.frame(sampleSizeSurvival4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    sampleSizeSurvival5 <- getSampleSizeSurvival(
        lambda1 = 0.03, lambda2 = 0.02,
        followUpTime = 8, accrualIntensity = 30, accrualTime = 0
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival5' with expected results
    expect_equal(sampleSizeSurvival5$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeSurvival5$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$median1, 23.104906, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$hazardRatio, 1.5, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$maxNumberOfSubjects, 557.38443, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$maxNumberOfEvents, 190.96804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$accrualTime, 18.579481, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$accrualTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$eventsFixed, 190.96804, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed, 557.38443, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed1, 278.69222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$nFixed2, 278.69222, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$analysisTime[1, ], 26.579477, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$studyDuration, 26.579477, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival5$criticalValuesEffectScale[1, ], 1.327981, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival5$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival5), NA)))
        expect_output(print(sampleSizeSurvival5)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival5), NA)))
        expect_output(summary(sampleSizeSurvival5)$show())
        sampleSizeSurvival5CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival5, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival5CodeBased$directionUpper, sampleSizeSurvival5$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$median1, sampleSizeSurvival5$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$median2, sampleSizeSurvival5$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$hazardRatio, sampleSizeSurvival5$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$maxNumberOfSubjects, sampleSizeSurvival5$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$maxNumberOfEvents, sampleSizeSurvival5$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$accrualTime, sampleSizeSurvival5$accrualTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$eventsFixed, sampleSizeSurvival5$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed, sampleSizeSurvival5$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed1, sampleSizeSurvival5$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$nFixed2, sampleSizeSurvival5$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$analysisTime, sampleSizeSurvival5$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$studyDuration, sampleSizeSurvival5$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival5CodeBased$criticalValuesEffectScale, sampleSizeSurvival5$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival5), "character")
        df <- as.data.frame(sampleSizeSurvival5)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival5)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getSampleSizeSurvival': check calculations for fixed design with relative accrual intensity", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival1 <- getSampleSizeSurvival(accrualIntensity = 0.1, accrualTime = 10)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival1' with expected results
    expect_equal(sampleSizeSurvival1$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival1$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival1$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival1$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival1$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival1$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival1), NA)))
        expect_output(print(sampleSizeSurvival1)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival1), NA)))
        expect_output(summary(sampleSizeSurvival1)$show())
        sampleSizeSurvival1CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival1, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival1CodeBased$directionUpper, sampleSizeSurvival1$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$median1, sampleSizeSurvival1$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$median2, sampleSizeSurvival1$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda1, sampleSizeSurvival1$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$lambda2, sampleSizeSurvival1$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$hazardRatio, sampleSizeSurvival1$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$maxNumberOfEvents, sampleSizeSurvival1$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$accrualIntensity, sampleSizeSurvival1$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$eventsFixed, sampleSizeSurvival1$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed, sampleSizeSurvival1$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed1, sampleSizeSurvival1$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$nFixed2, sampleSizeSurvival1$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$analysisTime, sampleSizeSurvival1$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$studyDuration, sampleSizeSurvival1$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival1CodeBased$criticalValuesEffectScale, sampleSizeSurvival1$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival1), "character")
        df <- as.data.frame(sampleSizeSurvival1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival2 <- getSampleSizeSurvival(accrualIntensity = 0.99, accrualTime = c(0, 10))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival2' with expected results
    expect_equal(sampleSizeSurvival2$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival2$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival2$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival2$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival2$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival2$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival2), NA)))
        expect_output(print(sampleSizeSurvival2)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival2), NA)))
        expect_output(summary(sampleSizeSurvival2)$show())
        sampleSizeSurvival2CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival2, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival2CodeBased$directionUpper, sampleSizeSurvival2$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$median1, sampleSizeSurvival2$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$median2, sampleSizeSurvival2$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda1, sampleSizeSurvival2$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$lambda2, sampleSizeSurvival2$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$hazardRatio, sampleSizeSurvival2$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$maxNumberOfEvents, sampleSizeSurvival2$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$accrualIntensity, sampleSizeSurvival2$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$eventsFixed, sampleSizeSurvival2$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed, sampleSizeSurvival2$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed1, sampleSizeSurvival2$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$nFixed2, sampleSizeSurvival2$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$analysisTime, sampleSizeSurvival2$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$studyDuration, sampleSizeSurvival2$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival2CodeBased$criticalValuesEffectScale, sampleSizeSurvival2$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival2), "character")
        df <- as.data.frame(sampleSizeSurvival2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    sampleSizeSurvival3 <- getSampleSizeSurvival(accrualIntensity = 1e-12, accrualTime = c(0, 10))

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival3' with expected results
    expect_equal(sampleSizeSurvival3$directionUpper, c(TRUE, TRUE, TRUE), label = paste0("c(", paste0(sampleSizeSurvival3$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$accrualIntensity, c(16.554072, 7.5582097, 4.2441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$accrualIntensity, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$eventsFixed, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed, c(165.54072, 75.582097, 42.441939), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed1, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$nFixed2, c(82.77036, 37.791048, 21.220969), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$analysisTime[1, ], 16, label = paste0("c(", paste0(sampleSizeSurvival3$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$studyDuration, 16, label = paste0("c(", paste0(sampleSizeSurvival3$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival3$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival3$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival3), NA)))
        expect_output(print(sampleSizeSurvival3)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival3), NA)))
        expect_output(summary(sampleSizeSurvival3)$show())
        sampleSizeSurvival3CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival3, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival3CodeBased$directionUpper, sampleSizeSurvival3$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$median1, sampleSizeSurvival3$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$median2, sampleSizeSurvival3$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda1, sampleSizeSurvival3$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$lambda2, sampleSizeSurvival3$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$hazardRatio, sampleSizeSurvival3$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$maxNumberOfEvents, sampleSizeSurvival3$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$accrualIntensity, sampleSizeSurvival3$accrualIntensity, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$eventsFixed, sampleSizeSurvival3$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed, sampleSizeSurvival3$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed1, sampleSizeSurvival3$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$nFixed2, sampleSizeSurvival3$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$analysisTime, sampleSizeSurvival3$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$studyDuration, sampleSizeSurvival3$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival3CodeBased$criticalValuesEffectScale, sampleSizeSurvival3$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival3), "character")
        df <- as.data.frame(sampleSizeSurvival3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    expect_equal(sampleSizeSurvival1$accrualIntensity, sampleSizeSurvival2$accrualIntensity)
    expect_equal(sampleSizeSurvival1$accrualIntensity, sampleSizeSurvival3$accrualIntensity)

    sampleSizeSurvival4 <- getSampleSizeSurvival(accrualIntensity = 1, accrualTime = c(0, 50), pi1 = 0.4)

    ## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival4' with expected results
    expect_equal(sampleSizeSurvival4$directionUpper, TRUE, label = paste0("c(", paste0(sampleSizeSurvival4$directionUpper, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$median1, 16.282985, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$median1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$median2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda1, 0.042568802, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$lambda2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$hazardRatio, 2.2892242, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$hazardRatio, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfSubjects, 50, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$maxNumberOfEvents, 45.770282, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$maxNumberOfEvents, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$followUpTime, 77.550073, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$followUpTime, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$eventsFixed, 45.770282, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$eventsFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed, 50, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed1, 25, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed1, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$nFixed2, 25, label = paste0("c(", paste0(sampleSizeSurvival4$nFixed2, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$analysisTime[1, ], 127.55007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$studyDuration, 127.55007, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$studyDuration, collapse = ", "), ")"))
    expect_equal(sampleSizeSurvival4$criticalValuesEffectScale[1, ], 1.7849857, tolerance = 1e-07, label = paste0("c(", paste0(sampleSizeSurvival4$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(sampleSizeSurvival4), NA)))
        expect_output(print(sampleSizeSurvival4)$show())
        invisible(capture.output(expect_error(summary(sampleSizeSurvival4), NA)))
        expect_output(summary(sampleSizeSurvival4)$show())
        sampleSizeSurvival4CodeBased <- eval(parse(text = getObjectRCode(sampleSizeSurvival4, stringWrapParagraphWidth = NULL)))
        expect_equal(sampleSizeSurvival4CodeBased$directionUpper, sampleSizeSurvival4$directionUpper, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$median1, sampleSizeSurvival4$median1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$median2, sampleSizeSurvival4$median2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda1, sampleSizeSurvival4$lambda1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$lambda2, sampleSizeSurvival4$lambda2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$hazardRatio, sampleSizeSurvival4$hazardRatio, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfSubjects, sampleSizeSurvival4$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$maxNumberOfEvents, sampleSizeSurvival4$maxNumberOfEvents, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$followUpTime, sampleSizeSurvival4$followUpTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$eventsFixed, sampleSizeSurvival4$eventsFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed, sampleSizeSurvival4$nFixed, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed1, sampleSizeSurvival4$nFixed1, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$nFixed2, sampleSizeSurvival4$nFixed2, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$analysisTime, sampleSizeSurvival4$analysisTime, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$studyDuration, sampleSizeSurvival4$studyDuration, tolerance = 1e-07)
        expect_equal(sampleSizeSurvival4CodeBased$criticalValuesEffectScale, sampleSizeSurvival4$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(sampleSizeSurvival4), "character")
        df <- as.data.frame(sampleSizeSurvival4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(sampleSizeSurvival4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'.getLambdaStepFunctionByTime': return correct lambda for specified time and piecewise exponential bounds", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda1 <- .getLambdaStepFunctionByTime(time = 1, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda1' with expected results
    expect_equal(lambda1, 0.025, tolerance = 1e-07, label = paste0("c(", paste0(lambda1, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda2 <- .getLambdaStepFunctionByTime(time = 6, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda2' with expected results
    expect_equal(lambda2, 0.025, tolerance = 1e-07, label = paste0("c(", paste0(lambda2, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda3 <- .getLambdaStepFunctionByTime(time = 7, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda3' with expected results
    expect_equal(lambda3, 0.04, tolerance = 1e-07, label = paste0("c(", paste0(lambda3, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda4 <- .getLambdaStepFunctionByTime(time = 9, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda4' with expected results
    expect_equal(lambda4, 0.04, tolerance = 1e-07, label = paste0("c(", paste0(lambda4, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda5 <- .getLambdaStepFunctionByTime(time = 14, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda5' with expected results
    expect_equal(lambda5, 0.015, tolerance = 1e-07, label = paste0("c(", paste0(lambda5, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda6 <- .getLambdaStepFunctionByTime(time = 15, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda6' with expected results
    expect_equal(lambda6, 0.015, tolerance = 1e-07, label = paste0("c(", paste0(lambda6, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda7 <- .getLambdaStepFunctionByTime(time = 16, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda7' with expected results
    expect_equal(lambda7, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(lambda7, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda8 <- .getLambdaStepFunctionByTime(time = 21, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda8' with expected results
    expect_equal(lambda8, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(lambda8, collapse = ", "), ")"))

    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    # @refFS[Formula]{fs:pieceWiseExponentialRandomVariable}
    lambda9 <- rpact:::.getLambdaStepFunctionByTime(time = 50, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

    ## Comparison of the results of numeric object 'lambda9' with expected results
    expect_equal(lambda9, 0.007, tolerance = 1e-07, label = paste0("c(", paste0(lambda9, collapse = ", "), ")"))
})

test_plan_section("Testing the Power Calculation of Survival Designs for Different Designs and Arguments")


test_that("'getPowerSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(maxNumberOfEvents = 40, maxNumberOfSubjects = 200)

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(6.1115255, 3.442577, 1.6316894, 0.30440109), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Power calculation of survival designs for one-sided group sequential design", {
    .skipTestIfDisabled()

    designGS1 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), sided = 1, alpha = 0.07,
        beta = 0.1, futilityBounds = c(-0.5, 0.5), typeOfDesign = "WT", deltaWT = 0.22
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6),
        dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.496718, 34.368969), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.25463139, 0.54601962), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.036015488, 0.087726198), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.11913846, 0.27563412), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.099477436, 0.1826593), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.43269831, 0.2052719), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.16216653, 0.076412449), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.27053178, 0.12885945), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.58785226, 0.56863222), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(32.017976, 30.394846, 25.872188), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 45, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.824774), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        typeOfComputation = "Freedman",
        pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.256688, 34.504982), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.23410594, 0.44983629), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.033136424, 0.067729226), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.10902189, 0.22109606), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.091947627, 0.16101101), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.45476178, 0.26727979), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1715797, 0.098248524), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.28318207, 0.16903127), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.59692009, 0.55610508), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(32.017976, 30.163653, 26.008714), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 45, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.822811), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        typeOfComputation = "HsiehFreedman",
        pi2 = 0.4, pi1 = c(0.4, 0.5, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 80, maxNumberOfEvents = 45, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(32.565971, 24, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.021284401, 0.028881133, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.3569154, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(80, 80, 78.932452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(80, 80, 80), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 6.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(41.628872, 30.417026, 22.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(29.092161, 33.473935, 34.421802), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.25255296, 0.52822452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03572104, 0.083721511), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.11810922, 0.2653086), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.098722701, 0.17919441), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.43487767, 0.2160418), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.16308496, 0.080152238), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.27179271, 0.13588956), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.58870793, 0.56507191), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.123713, 13.244904, 11.839868), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(32.118539, 26.401459, 22.217088), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(32.017976, 30.372933, 25.919163), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(53.628872, 42.417026, 34.638977), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 13.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 31.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 45, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(80, 80, 79.825057), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 4.203458, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 2.0990582, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.7531447, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.73032205, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.2284311, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        lambda2 = 0.04, thetaH0 = 1.25,
        hazardRatio = 0.8, directionUpper = FALSE,
        maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, 21.660849, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, 0.032, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 145.15218, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, 5.7883102, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 49.818428, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.49283375, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.076192913), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.24509523), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c("stage = 3" = 0.17154561), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, 0.2383697, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c("stage = 1" = 0.087970326), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c("stage = 2" = 0.15039938), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, 0.55965784, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 8.7091306, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], 13.807185, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], 17.78831, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 14.723329, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, 17.78831, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 65, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 190.996, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.37847558, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.67448058, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 0.78350426, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 1.623577, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 1.0533329, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        lambda2 = 0.04, thetaH0 = 0.8,
        hazardRatio = seq(0.8, 1.4, 0.2), directionUpper = TRUE,
        maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(21.660849, 17.32868, 14.440566, 12.377628), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.032, 0.04, 0.048, 0.056), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(141.27981, 134.32068, 128.46086, 123.43376), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200, 194.7445), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.1617391, 4.0656056, 3.2120436, 2.5256004), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(42.02201, 48.445748, 49.742518, 47.47852), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.25860493, 0.52208361, 0.74266051), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03658032, 0.082375002, 0.14710823), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.12110923, 0.26177073, 0.39724295), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.10091538, 0.17793787, 0.19830932), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.42856456, 0.21982747, 0.10295201), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1604311, 0.08147133, 0.041317452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.26813346, 0.13835614, 0.061634556), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.58625412, 0.5639732, 0.6473032), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(8.4767885, 8.0592408, 7.7076518, 7.4060255), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(13.399188, 12.692623, 12.137705, 11.68467), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(17.161739, 16.065606, 15.212044, 14.5256), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(12.758265, 13.175351, 12.752351, 11.880451), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(17.161739, 16.065606, 15.212044, 14.5256), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 65, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(181.22667, 187.06042, 188.27858, 183.16132), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.2513678, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3650021, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.1988902, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.63788392, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.92784212, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS1,
        eventTime = 120, pi2 = 0.4,
        thetaH0 = 0.8, hazardRatio = seq(0.8, 1.4, 0.2), directionUpper = TRUE,
        maxNumberOfSubjects = 200, maxNumberOfEvents = 65, allocationRatioPlanned = 1
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$pi1, c(0.33546019, 0.4, 0.45827173, 0.51088413), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$pi1, collapse = ", "), ")"))
    expect_equal(powerResult$median1, c(203.53732, 162.82985, 135.69154, 116.30704), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 162.82985, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.0034055042, 0.0042568802, 0.0051082562, 0.0059596323), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.0042568802, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(96.86335, 86.356678, 78.102375, 71.398147), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(42.02201, 48.445748, 49.742518, 47.47852), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.067448723, 0.25860493, 0.52208361, 0.74266051), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.011170644, 0.03658032, 0.082375002, 0.14710823), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.030436001, 0.12110923, 0.26177073, 0.39724295), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.025842077, 0.10091538, 0.17793787, 0.19830932), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityStop, c(0.71047424, 0.42856456, 0.21982747, 0.10295201), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityStop, collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[1, ], c(0.30853754, 0.1604311, 0.08147133, 0.041317452), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityPerStage[2, ], c(0.40193671, 0.26813346, 0.13835614, 0.061634556), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.75208089, 0.58625412, 0.5639732, 0.6473032), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(32.816894, 30.124548, 27.945787, 26.142615), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(73.505015, 66.662265, 61.211479, 56.744296), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(108.86335, 98.356678, 90.102375, 83.398147), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(69.262697, 72.57735, 68.358222, 60.378881), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(108.86335, 98.356678, 90.102375, 83.398147), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 19.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 45.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 65, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 2.2513678, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.3650021, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 1.1988902, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[1, ], 0.63788392, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsEffectScale[2, ], 0.92784212, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[1, ], 0.69146246, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$futilityBoundsPValueScale[2, ], 0.30853754, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$futilityBoundsPValueScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$pi1, powerResult$pi1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityStop, powerResult$futilityStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityPerStage, powerResult$futilityPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsEffectScale, powerResult$futilityBoundsEffectScale, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$futilityBoundsPValueScale, powerResult$futilityBoundsPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Power calculation of survival designs for two-sided group sequential design", {
    .skipTestIfDisabled()

    designGS2 <- getDesignGroupSequential(
        informationRates = c(0.3, 0.7, 1), alpha = 0.11,
        sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.32
    )

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6),
        dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40.275667, 53.258703, 46.484493), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.80955491, 0.11, 0.5536311), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.20812766, 0.025692757, 0.10981107), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.4067526, 0.045583354, 0.25986553), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.19467465, 0.038723888, 0.1839545), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.61488026, 0.071276112, 0.3696766), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(20.378955, 22.994709, 18.586202), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(177.90788, 179.45429, 176.38168), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        typeOfComputation = "Freedman",
        pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(44.992896, 53.258703, 44.408918), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.62751278, 0.11, 0.65422406), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.13113454, 0.025692757, 0.13983652), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.30051056, 0.045583354, 0.31559857), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.19586767, 0.038723888, 0.19878897), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.4316451, 0.071276112, 0.45543509), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(22.281639, 22.994709, 17.952578), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(178.68182, 179.45429, 175.39233), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        pi2 = 0.4, pi1 = c(0.2, 0.4, 0.6), dropoutRate1 = 0.1, dropoutTime = 12, eventTime = 24,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(74.550809, 32.565971, 18.155299), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 32.565971, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.009297648, 0.021284401, 0.03817878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.021284401, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(0.43682921, 1, 1.7937447), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(169.94792, 158.76005, 147.04957), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(14.361102, 11.603566, 9.1966475), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(41.467466, 53.258703, 46.846888), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.77062516, 0.11, 0.53442991), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.18711904, 0.025692757, 0.10481397), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.38354247, 0.045583354, 0.24956205), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.19996364, 0.038723888, 0.18005389), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.57066151, 0.071276112, 0.35437602), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(11.329861, 10.584003, 9.8033045), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(19.345216, 17.58497, 15.96575), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(20.85758, 22.994709, 18.697033), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(26.361102, 23.603566, 21.196648), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(178.11906, 179.45429, 176.54633), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8),
        dropoutRate1 = 0.1, dropoutTime = 12,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(37.895698, 53.258703, 46.404972), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.8740886, 0.11, 0.55777827), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.25384788, 0.025692757, 0.11091682), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.44431262, 0.045583354, 0.26210486), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.17592811, 0.038723888, 0.18475659), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.6981605, 0.071276112, 0.37302168), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(13.20331, 15.121757, 12.72043), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(166.366, 178.38985, 170.00949), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalFreedman}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        typeOfComputation = "Freedman",
        lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8), dropoutRate1 = 0.1, dropoutTime = 12,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(43.761896, 53.258703, 44.296935), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.68239647, 0.11, 0.65920633), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.14972738, 0.025692757, 0.14152926), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.33173334, 0.045583354, 0.31843565), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.20093576, 0.038723888, 0.19924141), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.48146072, 0.071276112, 0.45996492), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(14.524507, 15.121757, 12.352885), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(171.95824, 178.38985, 167.38024), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialTwoSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalHsieh}
    # @refFS[Formula]{fs:sampleSizeSurvivalExpectedPatientAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
    # @refFS[Formula]{fs:sampleSizeSurvivalFindFollowUpTime}
    # @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
    powerResult <- getPowerSurvival(designGS2,
        typeOfComputation = "HsiehFreedman",
        lambda2 = 0.04, hazardRatio = c(0.4, 1, 1.8), dropoutRate1 = 0.1, dropoutTime = 12,
        maxNumberOfSubjects = 180, maxNumberOfEvents = 55, allocationRatioPlanned = 0.3
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(43.321699, 17.32868, 9.6270442), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 17.32868, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.016, 0.04, 0.072), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(126.29066, 117.33052, 108.83344), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(180, 180, 171.99961), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(180, 180, 180), label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 15, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.0603074, 3.4618446, 2.2113432), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(39.493229, 53.258703, 46.77542), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.83266548, 0.11, 0.53825584), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.2225769, 0.025692757, 0.10579404), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.42045819, 0.045583354, 0.25160664), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.18963039, 0.038723888, 0.18085515), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.64303509, 0.071276112, 0.35740069), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(8.4193774, 7.8220347, 7.2555625), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(13.316286, 12.307189, 11.466641), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(13.562832, 15.121757, 12.784878), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(17.060307, 15.461845, 14.211343), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 16.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 38.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 55, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(168.04554, 178.38985, 170.45805), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[1, ], 0.27158358, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[2, ], 0.48064547, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleLower[3, ], 0.5627937, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleLower[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[1, ], 3.6821078, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[2, ], 2.0805356, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScaleUpper[3, ], 1.77685, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScaleUpper[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[1, ], 0.025692757, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[2, ], 0.055458318, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesPValueScale[3, ], 0.072467622, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesPValueScale[3, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleLower, powerResult$criticalValuesEffectScaleLower, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScaleUpper, powerResult$criticalValuesEffectScaleUpper, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesPValueScale, powerResult$criticalValuesPValueScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_plan_section("Testing the Power Calculation of Survival Designs for Other Parameter Variants")


test_that("'getPowerSurvival': Four stage O'Brien and Fleming group sequential design with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:PowerGroupSequentialOneSided}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 4),
        maxNumberOfEvents = 40, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], c(137.30481, 121.07229, 108.36969, 97.806661), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(196.25264, 173.42164, 155.68664, 141.01041), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[3, ], c(200, 200, 193.01085, 175.29605), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[4, ], c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[4, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(6.1115255, 3.442577, 1.6316894, 0.30440109), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(39.87408, 38.142534, 33.62741, 28.346513), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.30882929, 0.73475105, 0.94374207), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(2.5763449e-05, 0.00047146778, 0.0030806507, 0.012020122), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.0020845834, 0.034441261, 0.15314753, 0.35953485), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[3, ], c(0.0083455469, 0.11544971, 0.32172195, 0.41021864), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[4, ], c(0.014544106, 0.15846685, 0.25680093, 0.16196846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[4, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.010455897, 0.15036244, 0.47795013, 0.78177362), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(8.2382885, 7.2643376, 6.5021817, 5.8683997), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(11.775158, 10.405299, 9.3411982, 8.4606249), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[3, ], c(14.851313, 12.90759, 11.580651, 10.517763), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[4, ], c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[4, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(18.070854, 14.972567, 12.292784, 10.112156), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(18.111525, 15.442577, 13.631689, 12.304401), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 10, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[3, ], 30, label = paste0("c(", paste0(powerResult$eventsPerStage[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[4, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[4, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(199.99057, 199.0474, 190.68267, 167.42879), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 12.942983, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 3.5976357, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[3, ], 2.3478921, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[3, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[4, ], 1.8967435, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[4, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': For fixed sample design, determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        maxNumberOfEvents = 40,
        accrualTime = c(0), accrualIntensity = 30, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(8.7010979, 6.004962, 4.1561659, 2.779256), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.367765, 12.671629, 10.822833, 9.4459226), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(15.367765, 12.671629, 10.822833, 9.4459226), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        maxNumberOfEvents = 40,
        accrualTime = c(0, 6), accrualIntensity = c(20, 30), maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$totalAccrualTime, 8.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(8.127286, 5.4402735, 3.6040872, 2.2435211), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        maxNumberOfEvents = 40,
        accrualTime = c(0, 6, 10), accrualIntensity = c(20, 30)
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$maxNumberOfSubjects, 240, label = paste0("c(", paste0(powerResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$totalAccrualTime, 10, label = paste0("c(", paste0(powerResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.3825871, 3.1889048, 1.691326, 0.58951828), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(240, 240, 240, 240), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxNumberOfSubjects, powerResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specify accrual time as a list", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    at <- list("0 - <6" = 20, "6 - Inf" = 30)
    powerResult <- getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at, maxNumberOfSubjects = 200)

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$totalAccrualTime, 8.6666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(8.127286, 5.4402735, 3.6040872, 2.2435211), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(16.793953, 14.10694, 12.270754, 10.910188), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 200, 200, 200), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    at <- list("0 - <6" = 20, "6 - <=10" = 30)
    powerResult <- getPowerSurvival(maxNumberOfEvents = 40, accrualTime = at)

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 37.275405, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.018595296, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$maxNumberOfSubjects, 240, label = paste0("c(", paste0(powerResult$maxNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$totalAccrualTime, 10, label = paste0("c(", paste0(powerResult$totalAccrualTime, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(5.3825871, 3.1889048, 1.691326, 0.58951828), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(40, 40, 40, 40), label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.025, 0.31674317, 0.74507635, 0.94783846), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(15.382587, 13.188905, 11.691326, 10.589518), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(240, 240, 240, 240), label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 1.8585471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxNumberOfSubjects, powerResult$maxNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$totalAccrualTime, powerResult$totalAccrualTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specify effect size for a two-stage group design with O'Brien & Fleming boundaries Effect size is based on event rates at specified event time, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        pi1 = 0.2, pi2 = 0.3, eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, 74.550809, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 46.640597, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, 0.009297648, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, 0.62562161, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, 12.65889, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 39.194966, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.31394451, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.04025172), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.27369279), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, 0.040251721, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 14.822645, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], 24.65889, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 24.262964, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, 24.65889, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 200, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:ShiftParameterSurvivalSchoenfeld}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        hazardRatio = 0.5, pi2 = 0.3, eventTime = 24, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$pi1, 0.16333997, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$pi1, collapse = ", "), ")"))
    expect_equal(powerResult$median1, 93.281194, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 46.640597, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, 0.007430728, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$lambda2, 0.014861456, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda2, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, 14.346945, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 37.874505, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.5879328, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.48165803), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, 0.10627477, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 15.582247, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], 26.346945, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 25.202929, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, 26.346945, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 200, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$pi1, powerResult$pi1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda2, powerResult$lambda2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Effect size is based on hazard rate for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalDefinitionPieceWiseAccrual}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        hazardRatio = 0.5, lambda2 = 0.02, maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, 69.314718, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 34.657359, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$lambda1, 0.01, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$lambda1, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, 9.1631017, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 37.874505, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.5879328, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.48165803), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, 0.10627477, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 13.164641, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], 21.163102, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 20.313067, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, 21.163102, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 200, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.28632231, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 0.53509093, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$lambda1, powerResult$lambda1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time as list and hazard ratios", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time for both treatment arms", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), lambda1 = c(0.015, 0.03, 0.06), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$hazardRatio, 1.5, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[1, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], 200, label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, 4.2070411, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 39.412236, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.24668111, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c("stage = 1" = 0.0293882), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c("stage = 2" = 0.21729291), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, 0.029388201, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 12.173669, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], 16.207041, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 16.088508, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, 16.207041, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 200, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specification of piecewise exponential survival time as a list", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:sampleSizeSurvivalExponentialPieceWiseAccrual}
    # @refFS[Formula]{fs:sampleSizeSurvivalGeneralPieceWiseAccrual}
    # @refFS[Formula]{fs:pieceWiseExponentialSurvival}
    pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
    powerResult <- getPowerSurvival(
        design = getDesignGroupSequential(kMax = 2),
        piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), maxNumberOfEvents = 40, maxNumberOfSubjects = 200
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$numberOfSubjects[1, ], c(200, 195.08808, 190.46935), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$numberOfSubjects[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$numberOfSubjects[2, ], c(200, 200, 200), label = paste0("c(", paste0(powerResult$numberOfSubjects[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, c(4.2070411, 3.5734432, 3.2068918), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, c(39.412236, 38.617073, 37.874505), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, c(0.24668111, 0.45613948, 0.5879328), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[1, ], c(0.0293882, 0.069146371, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$rejectPerStage[2, ], c(0.21729291, 0.38699311, 0.48165803), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$rejectPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$earlyStop, c(0.029388201, 0.069146372, 0.10627477), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$earlyStop, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], c(12.173669, 11.705285, 11.428161), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[2, ], c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, c(16.088508, 15.305974, 14.805308), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$maxStudyDuration, c(16.207041, 15.573443, 15.206892), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$maxStudyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[1, ], 20, label = paste0("c(", paste0(powerResult$eventsPerStage[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$eventsPerStage[2, ], 40, label = paste0("c(", paste0(powerResult$eventsPerStage[2, ], collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, c(200, 199.66036, 198.98713), tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 3.4925675, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[2, ], 1.8688412, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[2, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$numberOfSubjects, powerResult$numberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$rejectPerStage, powerResult$rejectPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$earlyStop, powerResult$earlyStop, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$maxStudyDuration, powerResult$maxStudyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$eventsPerStage, powerResult$eventsPerStage, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    powerResult <- getPowerSurvival(
        lambda1 = log(2) / 5, lambda2 = log(2) / 3,
        maxNumberOfEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, 5, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 3, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, 0.6, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, -5.9093279, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 40, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.36520074, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 6.0906721, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 6.0906721, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 101.5112, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.53805471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Specify effect size based on median survival times of Weibull distribtion with kappa = 2 (median1 = 5, median2 = 3)", {
    .skipTestIfDisabled()

    # @refFS[Tab.]{fs:tab:output:getSampleSizeSurvival}
    # @refFS[Formula]{fs:lambdabymedian}
    powerResult <- getPowerSurvival(
        lambda1 = getLambdaByMedian(median = 5, kappa = 2),
        lambda2 = getLambdaByMedian(median = 3, kappa = 2),
        kappa = 2, maxNumberOfEvents = 40,
        maxNumberOfSubjects = 200, directionUpper = FALSE
    )

    ## Comparison of the results of TrialDesignPlanSurvival object 'powerResult' with expected results
    expect_equal(powerResult$median1, 5, label = paste0("c(", paste0(powerResult$median1, collapse = ", "), ")"))
    expect_equal(powerResult$median2, 3, label = paste0("c(", paste0(powerResult$median2, collapse = ", "), ")"))
    expect_equal(powerResult$hazardRatio, 0.36, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$hazardRatio, collapse = ", "), ")"))
    expect_equal(powerResult$accrualIntensity, 16.666667, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$accrualIntensity, collapse = ", "), ")"))
    expect_equal(powerResult$followUpTime, -5.7378582, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$followUpTime, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfEvents, 40, label = paste0("c(", paste0(powerResult$expectedNumberOfEvents, collapse = ", "), ")"))
    expect_equal(powerResult$overallReject, 0.8980967, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$overallReject, collapse = ", "), ")"))
    expect_equal(powerResult$analysisTime[1, ], 6.2621418, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$analysisTime[1, ], collapse = ", "), ")"))
    expect_equal(powerResult$studyDuration, 6.2621418, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$studyDuration, collapse = ", "), ")"))
    expect_equal(powerResult$expectedNumberOfSubjects, 104.36903, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$expectedNumberOfSubjects, collapse = ", "), ")"))
    expect_equal(powerResult$criticalValuesEffectScale[1, ], 0.53805471, tolerance = 1e-07, label = paste0("c(", paste0(powerResult$criticalValuesEffectScale[1, ], collapse = ", "), ")"))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(powerResult), NA)))
        expect_output(print(powerResult)$show())
        invisible(capture.output(expect_error(summary(powerResult), NA)))
        expect_output(summary(powerResult)$show())
        powerResultCodeBased <- eval(parse(text = getObjectRCode(powerResult, stringWrapParagraphWidth = NULL)))
        expect_equal(powerResultCodeBased$median1, powerResult$median1, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$median2, powerResult$median2, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$hazardRatio, powerResult$hazardRatio, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$accrualIntensity, powerResult$accrualIntensity, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$followUpTime, powerResult$followUpTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfEvents, powerResult$expectedNumberOfEvents, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$overallReject, powerResult$overallReject, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$analysisTime, powerResult$analysisTime, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$studyDuration, powerResult$studyDuration, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$expectedNumberOfSubjects, powerResult$expectedNumberOfSubjects, tolerance = 1e-07)
        expect_equal(powerResultCodeBased$criticalValuesEffectScale, powerResult$criticalValuesEffectScale, tolerance = 1e-07)
        expect_type(names(powerResult), "character")
        df <- as.data.frame(powerResult)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(powerResult)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getPowerSurvival': Analysis time at last stage equals accrual time + follow-up time", {
    .skipTestIfDisabled()

    x1 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12,
        lambda2 = 0.005, lambda1 = 0.01,
        maxNumberOfSubjects = 766, maxNumberOfEvents = 76
    )

    expect_equal(x1$overallReject, 1 - x1$.design$beta, tolerance = 0.01)
    expect_equal(x1$analysisTime[3], x1$accrualTime + x1$followUpTime)

    x2 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "P"),
        accrualTime = 12, maxNumberOfEvents = 76, maxNumberOfSubjects = 766,
        lambda2 = 0.005, lambda1 = 0.01
    )

    expect_equal(x2$analysisTime[3], x2$accrualTime + x2$followUpTime)

    x3 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0.3),
        accrualTime = c(0, 12, 15), accrualIntensity = c(20, 30),
        lambda2 = 0.005, lambda1 = 0.01, maxNumberOfEvents = 76
    )

    expect_equal(x3$analysisTime[length(x3$analysisTime)], x3$accrualTime[length(x3$accrualTime)] + x3$followUpTime)

    x4 <- getPowerSurvival(getDesignGroupSequential(typeOfDesign = "WT", deltaWT = 0.3),
        accrualTime = c(0, 12, 15), accrualIntensity = c(40, 60), maxNumberOfEvents = 76,
        piecewiseSurvivalTime = c(0, 5), lambda2 = c(0.005, 0.01), hazardRatio = 0.8
    )

    expect_equal(x4$analysisTime[length(x4$analysisTime)], x4$accrualTime[length(x4$accrualTime)] + x4$followUpTime)
})
