# Get Conditional Power

Calculates and returns the conditional power.

## Usage

``` r
getConditionalPower(stageResults, ..., nPlanned, allocationRatioPlanned = 1)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

- ...:

  Further (optional) arguments to be passed:

  `thetaH1` and `stDevH1` (or `assumedStDev` / `assumedStDevs`), `pi1`, `pi2`, or `piTreatments`, `piControl(s)`

  :   The assumed effect size, standard deviation or rates to calculate
      the conditional power if `nPlanned` is specified. For survival
      designs, `thetaH1` refers to the hazard ratio. For one-armed
      trials with binary outcome, only `pi1` can be specified, for
      two-armed trials with binary outcome, `pi1` and `pi2` can be
      specified referring to the assumed treatment and control rate,
      respectively. In multi-armed or enrichment designs, you can
      specify a value or a vector with elements referring to the
      treatment arms or the sub-populations, respectively. For testing
      rates, the parameters to be specified are `piTreatments` and
      `piControl` (multi-arm designs) and `piTreatments` and
      `piControls` (enrichment designs).  
      If not specified, the conditional power is calculated under the
      assumption of observed effect sizes, standard deviations, rates,
      or hazard ratios.

  `iterations`

  :   Iterations for simulating the power for Fisher's combination test.
      If the power for more than one remaining stages is to be
      determined for Fisher's combination test, it is estimated via
      simulation with specified  
      `iterations`, the default is `1000`.

  `seed`

  :   Seed for simulating the conditional power for Fisher's combination
      test. See above, default is a random seed.

- nPlanned:

  The additional (i.e., "new" and not cumulative) sample size planned
  for each of the subsequent stages. The argument must be a vector with
  length equal to the number of remaining stages and contain the
  combined sample size from both treatment groups if two groups are
  considered. For survival outcomes, it should contain the planned
  number of additional events. For multi-arm designs, it is the
  per-comparison (combined) sample size. For enrichment designs, it is
  the (combined) sample size for the considered sub-population.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. For multi-arm designs, it is the allocation
  ratio relating the active arm(s) to the control. For simulating means
  and rates for a two treatment groups design, it can be a vector of
  length `kMax`, the number of stages. It can be a vector of length
  `kMax`, too, for multi-arm and enrichment designs. In these cases, a
  change of allocating subjects to treatment groups over the stages can
  be assessed. Note that internally `allocationRatioPlanned` is treated
  as a vector of length `kMax`, not a scalar.

## Value

Returns a
[`ConditionalPowerResults`](https://rpact-com.github.io/rpact/reference/ConditionalPowerResults.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.ParameterSet.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

The conditional power is calculated if the planned sample size for the
subsequent stages is specified.  
For testing rates in a two-armed trial, pi1 and pi2 typically refer to
the rates in the treatment and the control group, respectively. This is
not mandatory, however, and so pi1 and pi2 can be interchanged. In
many-to-one multi-armed trials, piTreatments and piControl refer to the
rates in the treatment arms and the one control arm, and so they cannot
be interchanged. piTreatments and piControls in enrichment designs can
principally be interchanged, but we use the plural form to indicate that
the rates can be differently specified for the sub-populations.

For Fisher's combination test, the conditional power for more than one
remaining stages is estimated via simulation.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md).

## See also

[`plot.StageResults()`](https://rpact-com.github.io/rpact/reference/plot.StageResults.md)
or
[`plot.AnalysisResults()`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md)
for plotting the conditional power.

Other analysis functions:
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalRejectionProbabilities()`](https://rpact-com.github.io/rpact/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://rpact-com.github.io/rpact/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://rpact-com.github.io/rpact/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://rpact-com.github.io/rpact/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://rpact-com.github.io/rpact/reference/getRepeatedPValues.md),
[`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md),
[`getTestActions()`](https://rpact-com.github.io/rpact/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- getDataset(
    n1     = c(22, 13, 22, 13),
    n2     = c(22, 11, 22, 11),
    means1 = c(1, 1.1, 1, 1),
    means2 = c(1.4, 1.5, 1, 2.5),
    stds1  = c(1, 2, 2, 1.3),
    stds2  = c(1, 2, 2, 1.3)
)
stageResults <- getStageResults(
    getDesignGroupSequential(kMax = 4),
    dataInput = data, stage = 2, directionUpper = FALSE
)
getConditionalPower(stageResults, thetaH1 = -0.4,
    nPlanned = c(64, 64), assumedStDev = 1.5, 
    allocationRatioPlanned = 3
)
} # }
```
