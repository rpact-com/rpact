# Get Dataset

Creates a dataset object and returns it.

## Usage

``` r
getDataset(..., floatingPointNumbersEnabled = FALSE)

getDataSet(..., floatingPointNumbersEnabled = FALSE)
```

## Arguments

- ...:

  A `data.frame` or some data vectors defining the dataset.

- floatingPointNumbersEnabled:

  If `TRUE`, sample sizes and event numbers can be specified as
  floating-point numbers (this make sense, e.g., for theoretical
  comparisons);  
  by default `floatingPointNumbersEnabled = FALSE`, i.e., samples sizes
  and event numbers defined as floating-point numbers will be truncated.

## Value

Returns a
[`Dataset`](https://rpact-com.github.io/rpact/reference/Dataset.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.Dataset.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

The different dataset types `DatasetMeans`, of `DatasetRates`, or
`DatasetSurvival` can be created as follows:

- An element of
  [`DatasetMeans`](https://rpact-com.github.io/rpact/reference/DatasetMeans.md)
  for one sample is created by  
  `getDataset(sampleSizes =, means =, stDevs =)` where  
  `sampleSizes`, `means`, `stDevs` are vectors with stage-wise sample
  sizes, means and standard deviations of length given by the number of
  available stages.

- An element of
  [`DatasetMeans`](https://rpact-com.github.io/rpact/reference/DatasetMeans.md)
  for two samples is created by  
  `getDataset(sampleSizes1 =, sampleSizes2 =, means1 =, means2 =, `  
  `stDevs1 =, stDevs2 =)` where `sampleSizes1`, `sampleSizes2`,
  `means1`, `means2`, `stDevs1`, `stDevs2` are vectors with stage-wise
  sample sizes, means and standard deviations for the two treatment
  groups of length given by the number of available stages.

- An element of
  [`DatasetRates`](https://rpact-com.github.io/rpact/reference/DatasetRates.md)
  for one sample is created by  
  `getDataset(sampleSizes =, events =)` where `sampleSizes`, `events`
  are vectors with stage-wise sample sizes and events of length given by
  the number of available stages.

- An element of
  [`DatasetRates`](https://rpact-com.github.io/rpact/reference/DatasetRates.md)
  for two samples is created by  
  `getDataset(sampleSizes1 =, sampleSizes2 =, events1 =, events2 =)`
  where `sampleSizes1`, `sampleSizes2`, `events1`, `events2` are vectors
  with stage-wise sample sizes and events for the two treatment groups
  of length given by the number of available stages.

- An element of
  [`DatasetSurvival`](https://rpact-com.github.io/rpact/reference/DatasetSurvival.md)
  is created by  
  `getDataset(events =, logRanks =, allocationRatios =)` where `events`,
  `logRanks`, and `allocation ratios` are the stage-wise events,
  (one-sided) logrank statistics, and allocation ratios.

- An element of
  [`DatasetMeans`](https://rpact-com.github.io/rpact/reference/DatasetMeans.md),
  [`DatasetRates`](https://rpact-com.github.io/rpact/reference/DatasetRates.md),
  and
  [`DatasetSurvival`](https://rpact-com.github.io/rpact/reference/DatasetSurvival.md)
  for more than one comparison is created by adding subsequent digits to
  the variable names. The system can analyze these data in a multi-arm
  many-to-one comparison setting where the group with the highest index
  represents the control group.

Prefix `overall[Capital case of first letter of variable name]...` for
the variable names enables entering the overall (cumulative) results and
calculates stage-wise statistics. Since rpact version 3.2, the prefix
`cumulative[Capital case of first letter of variable name]...` or
`cum[Capital case of first letter of variable name]...` can
alternatively be used for this.

`n` can be used in place of `samplesizes`.

Note that in survival design usually the overall (cumulative) events and
logrank test statistics are provided in the output, so  
`getDataset(cumulativeEvents=, cumulativeLogRanks =, cumulativeAllocationRatios =)`  
is the usual command for entering survival data. Note also that for
`cumulativeLogranks` also the z scores from a Cox regression can be
used.

For multi-arm designs, the index refers to the considered comparison.
For example,  
` getDataset(events1=c(13, 33), logRanks1 = c(1.23, 1.55), events2 = c(16, NA), logRanks2 = c(1.55, NA)) `  
refers to the case where one active arm (1) is considered at both stages
whereas active arm 2 was dropped at interim. Number of events and
logrank statistics are entered for the corresponding comparison to
control (see Examples).

For enrichment designs, the comparison of two samples is provided for an
unstratified (sub-population wise) or stratified data input.  
For non-stratified (sub-population wise) data input the data sets are
defined for the sub-populations S1, S2, ..., F, where F refers to the
full populations. Use of `getDataset(S1 = , S2, ..., F = )` defines the
data set to be used in
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md)
(see examples)  
For stratified data input the data sets are defined for the strata S1,
S12, S2, ..., R, where R refers to the remainder of the strata such that
the union of all sets is the full population. Use of
`getDataset(S1 = , S12 = , S2, ..., R = )` defines the data set to be
used in
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md)
(see examples)  
For survival data, for enrichment designs the log-rank statistics can
only be entered as stratified log-rank statistics in order to provide
strong control of Type I error rate. For stratified data input, the
variables to be specified in `getDataset()` are `cumEvents`,
`cumExpectedEvents`, `cumVarianceEvents`, and `cumAllocationRatios` or
`overallEvents`, `overallExpectedEvents`, `overallVarianceEvents`, and
`overallAllocationRatios`. From this, (stratified) log-rank tests and
and the independent increments are calculated.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a Dataset of Means (one group):
datasetOfMeans <- getDataset(
    n      = c(22, 11, 22, 11),
    means  = c(1, 1.1, 1, 1),
    stDevs = c(1, 2, 2, 1.3)
)
datasetOfMeans
datasetOfMeans$show(showType = 2)

datasetOfMeans2 <- getDataset(
    cumulativeSampleSizes = c(22, 33, 55, 66),
    cumulativeMeans = c(1.000, 1.033, 1.020, 1.017),
    cumulativeStDevs = c(1.00, 1.38, 1.64, 1.58)
)
datasetOfMeans2
datasetOfMeans2$show(showType = 2)
as.data.frame(datasetOfMeans2)

# Create a Dataset of Means (two groups):
datasetOfMeans3 <- getDataset(
    n1 = c(22, 11, 22, 11),
    n2 = c(22, 13, 22, 13),
    means1  = c(1, 1.1, 1, 1),
    means2  = c(1.4, 1.5, 3, 2.5),
    stDevs1 = c(1, 2, 2, 1.3),
    stDevs2 = c(1, 2, 2, 1.3)
)
datasetOfMeans3

datasetOfMeans4 <- getDataset(
    cumulativeSampleSizes1 = c(22, 33, 55, 66),
    cumulativeSampleSizes2 = c(22, 35, 57, 70),
    cumulativeMeans1  = c(1, 1.033, 1.020, 1.017),
    cumulativeMeans2  = c(1.4, 1.437, 2.040, 2.126),
    cumulativeStDevs1 = c(1, 1.38, 1.64, 1.58),
    cumulativeStDevs2 = c(1, 1.43, 1.82, 1.74)
)
datasetOfMeans4

df <- data.frame(
    stages = 1:4,
    n1      = c(22, 11, 22, 11),
    n2      = c(22, 13, 22, 13),
    means1  = c(1, 1.1, 1, 1),
    means2  = c(1.4, 1.5, 3, 2.5),
    stDevs1 = c(1, 2, 2, 1.3),
    stDevs2 = c(1, 2, 2, 1.3)
)
datasetOfMeans5 <- getDataset(df)
datasetOfMeans5

# Create a Dataset of Means (three groups) where the comparison of 
# treatment arm 1 to control is dropped at the second interim stage:
datasetOfMeans6 <- getDataset(
   cumN1      = c(22, 33, NA),
   cumN2      = c(20, 34, 56),
   cumN3      = c(22, 31, 52),
   cumMeans1  = c(1.64, 1.54, NA),
   cumMeans2  = c(1.7, 1.5, 1.77),
   cumMeans3  = c(2.5, 2.06, 2.99),
   cumStDevs1 = c(1.5, 1.9, NA),
   cumStDevs2 = c(1.3, 1.3, 1.1),
   cumStDevs3 = c(1, 1.3, 1.8))
datasetOfMeans6

# Create a Dataset of Rates (one group):
datasetOfRates <- getDataset(
    n = c(8, 10, 9, 11), 
    events = c(4, 5, 5, 6)
)
datasetOfRates

# Create a Dataset of Rates (two groups):
datasetOfRates2 <- getDataset(
    n2      = c(8, 10, 9, 11),
    n1      = c(11, 13, 12, 13),
    events2 = c(3, 5, 5, 6),
    events1 = c(10, 10, 12, 12)
)
datasetOfRates2

# Create a Dataset of Rates (three groups) where the comparison of 
# treatment arm 2 to control is dropped at the first interim stage:
datasetOfRates3 <- getDataset(
    cumN1      = c(22, 33, 44),
    cumN2      = c(20, NA, NA),
    cumN3      = c(20, 34, 44),
    cumEvents1 = c(11, 14, 22),
    cumEvents2 = c(17, NA, NA),
    cumEvents3 = c(17, 19, 33))
datasetOfRates3

# Create a Survival Dataset
datasetSurvival <- getDataset(
    cumEvents = c(8, 15, 19, 31),
    cumAllocationRatios = c(1, 1, 1, 2),
    cumLogRanks = c(1.52, 1.98, 1.99, 2.11)
)
datasetSurvival
 
# Create a Survival Dataset with four comparisons where treatment
# arm 2 was dropped at the first interim stage, and treatment arm 4
# at the second.
datasetSurvival2 <- getDataset(
    cumEvents1   = c(18, 45, 56),
    cumEvents2   = c(22, NA, NA),
    cumEvents3   = c(12, 41, 56),
    cumEvents4   = c(27, 56, NA),
    cumLogRanks1 = c(1.52, 1.98, 1.99),
    cumLogRanks2 = c(3.43, NA, NA),
    cumLogRanks3 = c(1.45, 1.67, 1.87),
    cumLogRanks4 = c(1.12, 1.33, NA)
)
datasetSurvival2

# Enrichment: Stratified and unstratified data input
# The following data are from one study. Only the first 
# (stratified) data input enables a stratified analysis. 

# Stratified data input
S1 <- getDataset(
    sampleSize1 = c(18, 17), 
    sampleSize2 = c(12, 33), 
    mean1       = c(125.6, 111.1), 
    mean2       = c(107.7, 77.7), 
    stDev1      = c(120.1, 145.6),
    stDev2      = c(128.5, 133.3)) 
S2 <- getDataset(
    sampleSize1 = c(11, NA), 
    sampleSize2 = c(14, NA), 
    mean1       = c(100.1, NA), 
    mean2      = c( 68.3, NA), 
    stDev1      = c(116.8, NA),
    stDev2      = c(124.0, NA)) 
S12 <- getDataset(           
    sampleSize1 = c(21, 17), 
    sampleSize2 = c(21, 12), 
    mean1       = c(135.9, 117.7), 
    mean2       = c(84.9, 107.7), 
    stDev1      = c(185.0, 92.3),
    stDev2      = c(139.5, 107.7)) 
R <- getDataset(
    sampleSize1 = c(19, NA), 
    sampleSize2 = c(33, NA), 
    mean1       = c(142.4, NA), 
    mean2       = c(77.1, NA), 
    stDev1      = c(120.6, NA),
    stDev2      = c(163.5, NA)) 
dataEnrichment <- getDataset(S1 = S1, S2 = S2, S12 = S12, R = R)
dataEnrichment

# Unstratified data input
S1N <- getDataset(
    sampleSize1 = c(39, 34), 
    sampleSize2 = c(33, 45), 
    stDev1      = c(156.503, 120.084), 
    stDev2      = c(134.025, 126.502), 
    mean1       = c(131.146, 114.4), 
    mean2       = c(93.191, 85.7))
S2N <- getDataset(
    sampleSize1 = c(32, NA), 
    sampleSize2 = c(35, NA), 
    stDev1      = c(163.645, NA), 
    stDev2      = c(131.888, NA),
    mean1       = c(123.594, NA), 
    mean2       = c(78.26, NA))
F <- getDataset(
    sampleSize1 = c(69, NA), 
    sampleSize2 = c(80, NA), 
    stDev1      = c(165.468, NA), 
    stDev2      = c(143.979, NA), 
    mean1       = c(129.296, NA), 
    mean2       = c(82.187, NA))
dataEnrichmentN <- getDataset(S1 = S1N, S2 = S2N, F = F)
dataEnrichmentN
} # }
```
