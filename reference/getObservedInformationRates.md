# Get Observed Information Rates

Recalculates the observed information rates from the specified dataset.

## Usage

``` r
getObservedInformationRates(
  dataInput,
  ...,
  maxInformation = NULL,
  informationEpsilon = NULL,
  stage = NA_integer_
)
```

## Arguments

- dataInput:

  The dataset for which the information rates shall be recalculated.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

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

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

## Value

Returns a list that summarizes the observed information rates.

## Details

For means and rates the maximum information is the maximum number of
subjects or the relative proportion if `informationEpsilon` \< 1; for
survival data it is the maximum number of events or the relative
proportion if `informationEpsilon` \< 1.

## See also

- [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md)
  for using `getObservedInformationRates()` implicit,

- [www.rpact.org/vignettes/planning/rpact_boundary_update_example](https://www.rpact.org/vignettes/planning/rpact_boundary_update_example/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Absolute information epsilon:
# decision rule 45 >= 46 - 1, i.e., under-running
data <- getDataset(
    overallN = c(22, 45),
    overallEvents = c(11, 28)
)
getObservedInformationRates(data,
    maxInformation = 46, informationEpsilon = 1
)

# Relative information epsilon:
# last information rate = 45/46 = 0.9783,
# is > 1 - 0.03 = 0.97, i.e., under-running
data <- getDataset(
    overallN = c(22, 45),
    overallEvents = c(11, 28)
)
getObservedInformationRates(data,
    maxInformation = 46, informationEpsilon = 0.03
)
} # }
```
