#'
#' Patient-wise simulation reports \code{observedDropoutProportion1} and
#' \code{observedDropoutProportion2} at each interim and final analysis.
#' At each stage, a dropout must precede the event and occur by that stage's
#' analysis time, and the denominator counts subjects enrolled by that time.
#' The proportion is calculated separately in each replication and then averaged
#' over replications contributing at that stage with a finite analysis time and
#' nonempty denominator. Counts are not pooled across replications. A proportion
#' is NA when no replication contributes; otherwise zero dropouts gives zero.
#'
#' The outputs are matrices with stage rows and scenario columns for two-arm
#' trials. Multi-arm treatment outputs are arrays with dimensions stage, scenario,
#' and active arm; shared control is a stage-by-scenario matrix. Enrichment outputs
#' have dimensions stage, scenario, and population (subpopulations followed by the
#' full population). A single active arm or population uses a stage-by-scenario
#' matrix. Stages and scenarios are never dropped, including for a fixed design.
#' Discontinued groups contribute through their discontinuation analysis and
#' stopped trials through their stopping analysis, with no later contributions.
#' Overlapping enrichment populations include their shared subjects.
#'
#' The corresponding \code{dropoutObservationTime1} and
#' \code{dropoutObservationTime2} fields retain the mean stage analysis time over
#' exactly the same contributing replications, with the same dimensions as the
#' proportions. They are omitted from print and summary output but can be accessed
#' directly as \code{result$dropoutObservationTime1} and
#' \code{result$dropoutObservationTime2}. In multi-arm trials, the first field
#' refers to the active treatment arms and the second to shared control. In
#' enrichment trials, both fields contain population-specific results for their
#' respective treatment arms. See [SimulationResultsSurvival],
#' [SimulationResultsMultiArmSurvival], and [SimulationResultsEnrichmentSurvival]
#' for the result fields and their dimensions.
#'
#' These averages can differ from the trial-level \code{analysisTime}, because
#' arm or population selection and the requirement for a nonempty enrolled-subject
#' denominator can change which replications contribute. Within a replication,
#' all arms or populations contributing at a given stage share its analysis time;
#' differences between the reported averages arise from the contributing sets of
#' replications, rather than different analysis schedules within that replication.
#' When both input dropout rates are zero, the proportions are also omitted from
#' print and summary output while remaining in the result object.
#'
#' These outputs require patient histories and are unavailable for
#' \code{simulationType = "testStatisticBased"}. Existing recruitment and
#' allocation mechanisms are retained. The input dropout probability at
#' \code{dropoutTime} describes the dropout distribution without competing events;
#' it is not the simulated observed proportion. Analytic expected proportions
#' condition on deterministic planned accrual at each planned analysis time,
#' whereas simulation includes random analysis times, early stopping and selection.
#' Compare them only with matching stage, horizon, enrollment and allocation assumptions.
#'
