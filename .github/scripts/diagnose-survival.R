# Temporary diagnostic: capture an existing private regression case unchanged.
library(rpact)
outputPath <- commandArgs(trailingOnly = TRUE)[1]
probeStages <- list()
probeUpdates <- list()
trace('updateSubGroupVector', where = asNamespace('rpact'), print = FALSE,
    tracer = quote(probeUpdateEntry <- list(k = k, probabilities = prevSelected,
        seedBefore = .Random.seed)),
    exit = quote({
        probeUpdates[[length(probeUpdates) + 1L]] <<- c(probeUpdateEntry,
            list(result = returnValue(), seedAfter = .Random.seed))
    })
)
trace('.getSimulatedStageResultsSurvivalEnrichmentPatientWise',
    where = asNamespace('rpact'), print = FALSE,
    tracer = quote(probeEntrySeed <- .Random.seed),
    exit = quote({
        probeStages[[length(probeStages) + 1L]] <<- list(
            result = returnValue(), seedBefore = probeEntrySeed,
            seedAfter = .Random.seed, data = survivalDataSet,
            lambdaControl = lambdaControl, lambdaActive = lambdaActive,
            criticalValues = design$criticalValues,
            recruitmentTimes = recruitmentTimes
        )
    })
)
expressions <- parse('private-tests/tests/testthat/test-f_simulation_enrichment_survival_patient_wise.R')
case <- Filter(function(e) is.call(e) && identical(e[[1]], as.name('test_that')) &&
    identical(e[[2]], "'getSimulationEnrichmentSurvival': patient-wise simulation for a three-stage design"),
    as.list(expressions))[[1]]
for (statement in as.list(case[[3]])[-1]) {
    if (is.call(statement) && identical(statement[[1]], as.name('.skipTestIfDisabled'))) next
    if (is.call(statement) && identical(statement[[1]], as.name('expect_equal'))) break
    eval(statement)
}
saveRDS(list(stages = probeStages, updates = probeUpdates, machine = .Machine,
    cppEvents = simResults2$expectedNumberOfEvents,
    basicEvents = simResults2Basic$expectedNumberOfEvents,
    session = sessionInfo()), outputPath)
print(c(cppEvents = simResults2$expectedNumberOfEvents,
    basicEvents = simResults2Basic$expectedNumberOfEvents))
cat('Saved', length(probeStages), 'iterations to', outputPath, '\n')
