# Get Fisher Information From a Design Plan or Simulation Results

Calculates cumulative or stage-wise Fisher information at planned
analyses for a design plan or simulation results object for means,
rates, survival, or count data endpoints. This is particularly useful
for calculating the `information` argument of
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md).

## Usage

``` r
getFisherInformation(
  designPlan,
  stage = NA_integer_,
  type = c("cumulative", "stageWise")
)
```

## Arguments

- designPlan:

  A trial design plan or simulation results object as returned by
  functions such as
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),
  [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md),
  [`getSampleSizeRates()`](https://docs.rpact.org/reference/getSampleSizeRates.md),
  [`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md),
  [`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md),
  [`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md),
  [`getSampleSizeCounts()`](https://docs.rpact.org/reference/getSampleSizeCounts.md),
  [`getPowerCounts()`](https://docs.rpact.org/reference/getPowerCounts.md),
  [`getSimulationMeans()`](https://docs.rpact.org/reference/getSimulationMeans.md),
  [`getSimulationRates()`](https://docs.rpact.org/reference/getSimulationRates.md),
  [`getSimulationSurvival()`](https://docs.rpact.org/reference/getSimulationSurvival.md),
  [`getSimulationCounts()`](https://docs.rpact.org/reference/getSimulationCounts.md),
  or the corresponding multi-arm simulation functions.

- stage:

  Integer vector. The analysis stage or stages for which Fisher
  information is requested. If `NA` (default), all stages of the design
  are used.

- type:

  Character. Defines whether cumulative information through each
  requested analysis (`"cumulative"`, the default) or the information
  increment contributed by each requested stage (`"stageWise"`) is
  returned.

## Value

A `FisherInformation` R6 object. Its public fields are `information`
(the calculated numeric value, vector, or matrix), `type`, `stage`, and
`situations`. The latter contains descriptive labels such as the
corresponding alternatives, event probabilities, hazard ratios, or
count-data rates where available. The originating design plan is stored
privately for subsequent pipe-based conversion with
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md).

Use [`as.numeric()`](https://rdrr.io/r/base/numeric.html) to extract a
plain numeric vector,
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) to obtain a
consistently arranged stage-by-situation matrix, and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to obtain
a long-format data frame. Unsupported endpoint types are represented by
`NA_real_` in the `information` field.

## Details

**Cumulative information**

With `type = "cumulative"`, the function returns the total Fisher
information available at each requested analysis, including information
accumulated during all preceding stages. With `type = "stageWise"`, it
returns the increment contributed by each requested stage. The
stage-wise value for stage `j > 1` is calculated by subtracting the
cumulative information at stage `j - 1` from that at stage `j`, even if
stage `j - 1` was not included in `stage`. At the first analysis,
cumulative and stage-wise information are identical.

To obtain stage-wise increments from cumulative values
\\I_1,\ldots,I_k\\, use \\I_1, I_2-I_1,\ldots,I_k-I\_{k-1}\\. For
example:


    informationCumulative <- as.numeric(getFisherInformation(
        designPlan,
        stage = 1:2
    ))
    informationStageWise <- c(
        informationCumulative[1],
        diff(informationCumulative)
    )

If the result contains several planning alternatives or treatment
comparisons, apply the differences separately to each corresponding
series. The function performs this calculation directly when
`type = "stageWise"`; the explicit calculation above illustrates its
definition.

**Calculation by endpoint**

- Means:

  For a one-sample comparison, information is the cumulative sample size
  divided by the variance. For a two-sample comparison, it is the
  inverse variance of the estimated treatment difference, based on
  cumulative group sizes, standard deviations, and the planned
  allocation ratio. For a mean-ratio analysis, the null value is
  additionally taken into account.

- Rates:

  Information is the inverse binomial variance of the estimated rate or
  rate difference under the planning assumptions. It uses cumulative
  planned sample sizes, event probabilities, and, for multiple groups,
  the planned allocation ratio.

- Survival:

  Information is based on the cumulative number of events at the
  requested analysis and the planned allocation ratio. For multi-arm
  designs, the available events are apportioned to the relevant
  treatment comparisons using the planning assumptions.

- Count data:

  Information is based on the negative binomial model and includes all
  exposure accumulated up to the requested analysis time. It accounts
  for recruitment, exposure or follow-up time, event rates, allocation
  ratio, and overdispersion. If the design plan already contains
  information by analysis, those stored cumulative values are used.

**Use with getFutilityBounds**

For a conversion involving `"effectEstimate"`, pass the cumulative
information returned for the relevant analysis as `information[1]`. For
conditional- or predictive-power conversions in a two-stage setting,
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
interprets `information[1]` as the cumulative information at the first
analysis and `information[2]` as the additional information after that
analysis. It can therefore be populated directly with:


    information <- getFisherInformation(
        designPlan,
        stage = 1:2,
        type = "stageWise"
    )

For an effect-estimate conversion at analysis `j`, use:


    information <- getFisherInformation(
        designPlan,
        stage = j,
        type = "cumulative"
    )

The `type` field in every result enables
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
to detect incompatible use. The complete result can also be piped into
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md).
In that case, its `stage` and `situations` fields are used to convert
every applicable futility bound separately. With the default cumulative
type, an omitted target scale means conversion to the effect-estimate
scale:


    designPlan |>
        getFisherInformation() |>
        getFutilityBounds()

Alternatively, pipe `designPlan` directly into
[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md);
the latter then calls this function internally with the information type
required by the requested target scale. Cumulative information can also
be piped into a conversion to the design-specific treatment-effect
scale:


    designPlan |>
        getFisherInformation(type = "cumulative") |>
        getFutilityBounds(targetScale = "treatmentEffect")

In this case, the information object's type, stages, and originating
design plan are validated before the endpoint-specific transformation is
applied. For two-group rates, the Farrington–Manning inversion uses the
design plan's group sizes and null-restricted rates directly. For Count
Data, the design and stages are reused without recalculating Fisher
information, while the exact negative-binomial inversion evaluates its
variance at each candidate rate ratio from the design-specific
sample-size and exposure assumptions.

## See also

[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)
for converting futility bounds using the calculated information.

## Examples

``` r
if (FALSE) { # \dontrun{
designPlan <- getSampleSizeMeans(alternative = 0.4)
getFisherInformation(designPlan)

design <- getDesignGroupSequential(kMax = 3)
designPlan <- getPowerMeans(design,
    alternative = c(0.3, 0.4), maxNumberOfSubjects = 100
)
getFisherInformation(designPlan)
getFisherInformation(designPlan, stage = 2)
getFisherInformation(designPlan, stage = 1:3, type = "stageWise")

simulationResults <- getSimulationMeans(design,
    plannedSubjects = c(20, 40, 60), alternative = 0.4,
    maxNumberOfIterations = 10
)
getFisherInformation(simulationResults)
} # }
```
