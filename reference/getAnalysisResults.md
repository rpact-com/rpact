# Get Analysis Results

Calculates and returns the analysis results for the specified design and
data.

## Usage

``` r
getAnalysisResults(
  design,
  dataInput,
  ...,
  directionUpper = NA,
  thetaH0 = NA_real_,
  nPlanned = NA_real_,
  allocationRatioPlanned = 1,
  stage = NA_integer_,
  maxInformation = NULL,
  informationEpsilon = NULL
)
```

## Arguments

- design:

  The trial design.

- dataInput:

  The summary data used for calculating the test results. This is either
  an element of `DatasetMeans`, of `DatasetRates`, or of
  `DatasetSurvival` and should be created with the function
  [`getDataset()`](https://rpact-com.github.io/rpact/reference/getDataset.md).
  For more information see
  [`getDataset()`](https://rpact-com.github.io/rpact/reference/getDataset.md).

- ...:

  Further arguments to be passed to methods (cf., separate functions in
  "See Also" below), e.g.,

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

  `normalApproximation`

  :   The type of computation of the p-values. Default is `FALSE` for
      testing means (i.e., the t test is used) and `TRUE` for testing
      rates and the hazard ratio. For testing rates, if
      `normalApproximation = FALSE` is specified, the binomial test (one
      sample) or the exact test of Fisher (two samples) is used for
      calculating the p-values. In the survival setting,
      `normalApproximation = FALSE` has no effect.

  `equalVariances`

  :   The type of t test. For testing means in two treatment groups,
      either the t test assuming that the variances are equal or the t
      test without assuming this, i.e., the test of Welch-Satterthwaite
      is calculated, default is `TRUE`.

  `stdErrorEstimate`

  :   Estimate of standard error for calculation of final confidence
      intervals for comparing rates in two treatment groups, default is
      `"pooled"`.

  `intersectionTest`

  :   Defines the multiple test for the intersection hypotheses in the
      closed system of hypotheses when testing multiple hypotheses. Five
      options are available in multi-arm designs: `"Dunnett"`,
      `"Bonferroni"`, `"Simes"`, `"Sidak"`, and `"Hierarchical"`,
      default is `"Dunnett"`. Four options are available in population
      enrichment designs: `"SpiessensDebois"` (one subset only),
      `"Bonferroni"`, `"Simes"`, and `"Sidak"`, default is `"Simes"`.

  `varianceOption`

  :   Defines the way to calculate the variance in multiple treatment
      arms (\> 2) or population enrichment designs for testing means.
      For multiple arms, three options are available: `"overallPooled"`,
      `"pairwisePooled"`, and `"notPooled"`, default is
      `"overallPooled"`. For enrichment designs, the options are:
      `"pooled"`, `"pooledFromFull"` (one subset only), and
      `"notPooled"`, default is `"pooled"`.

  `stratifiedAnalysis`

  :   For enrichment designs, typically a stratified analysis should be
      chosen. For testing means and rates, also a non-stratified
      analysis based on overall data can be performed. For survival
      data, only a stratified analysis is possible (see Brannath et al.,
      2009), default is `TRUE`.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- thetaH0:

  The null hypothesis value, default is `0` for the normal and the
  binary case (testing means and rates, respectively), it is `1` for the
  survival case (testing the hazard ratio).  
    
  For non-inferiority designs, `thetaH0` is the non-inferiority bound.
  That is, in case of (one-sided) testing of

  - *means*: a value `!= 0` (or a value `!= 1` for testing the mean
    ratio) can be specified.

  - *rates*: a value `!= 0` (or a value `!= 1` for testing the risk
    ratio `pi1 / pi2`) can be specified.

  - *survival data*: a bound for testing H0:
    `hazard ratio = thetaH0 != 1` can be specified.

  - *count data*: a bound for testing H0:
    `lambda1 / lambda2 = thetaH0 != 1` can be specified.

  For testing a rate in one sample, a value `thetaH0` in (0, 1) has to
  be specified for defining the null hypothesis H0: `pi = thetaH0`.

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

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

- maxInformation:

  Positive value specifying the maximum information.

- informationEpsilon:

  Positive integer value specifying the absolute information epsilon,
  which defines the maximum distance from the observed information to
  the maximum information that causes the final analysis. Updates at the
  final analysis in case the observed information at the final analysis
  is smaller ("under-running") than the planned maximum information
  `maxInformation`, default is 0. Alternatively, a floating-point number
  \> 0 and \< 1 can be specified to define a relative information
  epsilon.

## Value

Returns an
[`AnalysisResults`](https://rpact-com.github.io/rpact/reference/AnalysisResults.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names`](https://rpact-com.github.io/rpact/reference/names.AnalysisResults.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.ParameterSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.AnalysisResults.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.AnalysisResults.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Given a design and a dataset, at given stage the function calculates the
test results (effect sizes, stage-wise test statistics and p-values,
overall p-values and test statistics, conditional rejection probability
(CRP), conditional power, Repeated Confidence Intervals (RCIs), repeated
overall p-values, and final stage p-values, median unbiased effect
estimates, and final confidence intervals.

For designs with more than two treatments arms (multi-arm designs) or
enrichment designs a closed combination test is performed. That is,
additionally the statistics to be used in a closed testing procedure are
provided.

The conditional power is calculated if the planned sample size for the
subsequent stages (`nPlanned`) is specified. The conditional power is
calculated either under the assumption of the observed effect or under
the assumption of an assumed effect, that has to be specified (see
above).  
For testing rates in a two-armed trial, pi1 and pi2 typically refer to
the rates in the treatment and the control group, respectively. This is
not mandatory, however, and so pi1 and pi2 can be interchanged. In
many-to-one multi-armed trials, piTreatments and piControl refer to the
rates in the treatment arms and the one control arm, and so they cannot
be interchanged. piTreatments and piControls in enrichment designs can
principally be interchanged, but we use the plural form to indicate that
the rates can be differently specified for the sub-populations.

Median unbiased effect estimates and confidence intervals are calculated
if a group sequential design or an inverse normal combination test
design was chosen, i.e., it is not applicable for Fisher's p-value
combination test design. For the inverse normal combination test design
with more than two stages, a warning informs that the validity of the
confidence interval is theoretically shown only if no sample size change
was performed.

A final stage p-value for Fisher's combination test is calculated only
if a two-stage design was chosen. For Fisher's combination test, the
conditional power for more than one remaining stages is estimated via
simulation.

Final stage p-values, median unbiased effect estimates, and final
confidence intervals are not calculated for multi-arm and enrichment
designs.

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

[`getObservedInformationRates()`](https://rpact-com.github.io/rpact/reference/getObservedInformationRates.md)

Other analysis functions:
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getClosedConditionalDunnettTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedConditionalDunnettTestResults.md),
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md),
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
# Example 1 One-Sample t Test
# Perform an analysis within a three-stage group sequential design with
# O'Brien & Fleming boundaries and one-sample data with a continuous outcome
# where H0: mu = 1.2 is to be tested
dsnGS <- getDesignGroupSequential()
dataMeans <- getDataset(
    n = c(30, 30),
    means = c(1.96, 1.76),
    stDevs = c(1.92, 2.01)
)
getAnalysisResults(design = dsnGS, dataInput = dataMeans, thetaH0 = 1.2)

# You can obtain the results when performing an inverse normal combination test
# with these data by using the commands
dsnIN <- getDesignInverseNormal()
getAnalysisResults(design = dsnIN, dataInput = dataMeans, thetaH0 = 1.2)

# Example 2 Use Function Approach with Time to Event Data
# Perform an analysis within a use function approach according to an
# O'Brien & Fleming type use function and survival data where
# where H0: hazard ratio = 1 is to be tested. The events were observed
# over time and maxInformation = 120, informationEpsilon = 5 specifies
# that 116 > 120 - 5 observed events defines the final analysis.
design <- getDesignGroupSequential(typeOfDesign = "asOF")
dataSurvival <- getDataset(
    cumulativeEvents = c(33, 72, 116),
    cumulativeLogRanks = c(1.33, 1.88, 1.902)
)
getAnalysisResults(design,
    dataInput = dataSurvival,
    maxInformation = 120, informationEpsilon = 5
)

# Example 3 Multi-Arm Design
# In a four-stage combination test design with O'Brien & Fleming boundaries
# at the first stage the second treatment arm was dropped. With the Bonferroni
# intersection test, the results together with the CRP, conditional power
# (assuming a total of 40 subjects for each comparison and effect sizes 0.5
# and 0.8 for treatment arm 1 and 3, respectively, and standard deviation 1.2),
# RCIs and p-values of a closed adaptive test procedure are
# obtained as follows with the given data (treatment arm 4 refers to the
# reference group; displayed with summary and plot commands):
data <- getDataset(
    n1 = c(22, 23),
    n2 = c(21, NA),
    n3 = c(20, 25),
    n4 = c(25, 27),
    means1 = c(1.63, 1.51),
    means2 = c(1.4, NA),
    means3 = c(0.91, 0.95),
    means4 = c(0.83, 0.75),
    stds1 = c(1.2, 1.4),
    stds2 = c(1.3, NA),
    stds3 = c(1.1, 1.14),
    stds4 = c(1.02, 1.18)
)
design <- getDesignInverseNormal(kMax = 4)
x <- getAnalysisResults(design,
    dataInput = data, intersectionTest = "Bonferroni",
    nPlanned = c(40, 40), thetaH1 = c(0.5, NA, 0.8), assumedStDevs = 1.2
)
summary(x)
if (require(ggplot2)) plot(x, thetaRange = c(0, 0.8))
design <- getDesignConditionalDunnett(secondStageConditioning = FALSE)
y <- getAnalysisResults(design,
    dataInput = data,
    nPlanned = 40, thetaH1 = c(0.5, NA, 0.8), assumedStDevs = 1.2, stage = 1
)
summary(y)
if (require(ggplot2)) plot(y, thetaRange = c(0, 0.4))

# Example 4 Enrichment Design
# Perform an two-stage enrichment design analysis with O'Brien & Fleming boundaries
# where one sub-population (S1) and a full population (F) are considered as primary
# analysis sets. At interim, S1 is selected for further analysis and the sample
# size is increased accordingly. With the Spiessens & Debois intersection test,
# the results of a closed adaptive test procedure together with the CRP, repeated
# RCIs and p-values are obtained as follows with the given data (displayed with
# summary and plot commands):
design <- getDesignInverseNormal(kMax = 2, typeOfDesign = "OF")
dataS1 <- getDataset(
    means1 = c(13.2, 12.8),
    means2 = c(11.1, 10.8),
    stDev1 = c(3.4, 3.3),
    stDev2 = c(2.9, 3.5),
    n1 = c(21, 42),
    n2 = c(19, 39)
)
dataNotS1 <- getDataset(
    means1 = c(11.8, NA),
    means2 = c(10.5, NA),
    stDev1 = c(3.6, NA),
    stDev2 = c(2.7, NA),
    n1 = c(15, NA),
    n2 = c(13, NA)
)
dataBoth <- getDataset(S1 = dataS1, R = dataNotS1)
x <- getAnalysisResults(design,
    dataInput = dataBoth,
    intersectionTest = "SpiessensDebois",
    varianceOption = "pooledFromFull",
    stratifiedAnalysis = TRUE
)
summary(x)
if (require(ggplot2)) plot(x, type = 2)
} # }
```
