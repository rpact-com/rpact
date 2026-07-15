# Get Fisher Information From a Design Plan

Calculates the Fisher information at the first planned analysis stage
for a design plan for means, rates, or survival endpoints.

## Usage

``` r
getFisherInformation(designPlan)
```

## Arguments

- designPlan:

  A trial design plan object as returned by functions such as
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),
  [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md),
  [`getSampleSizeRates()`](https://docs.rpact.org/reference/getSampleSizeRates.md),
  [`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md),
  [`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md),
  or
  [`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md).

## Value

A numeric value or numeric vector containing the first-stage Fisher
information. A vector is returned if the design plan contains several
planning alternatives or sample size values. `NA_real_` is returned if
the endpoint type is not supported by this helper.

## Details

The returned information is the information used at the first stage of
the design plan. For group sequential designs, information at later
stages can be obtained by multiplying this value by the ratio of the
corresponding information rate to the first information rate.

For means, the information is based on the planned sample size, standard
deviations, allocation ratio, and, if applicable, the mean-ratio null
value. For rates, it is based on the planned sample size and the
binomial variance under the corresponding planning assumptions. For
survival endpoints, it is based on the planned number of events and the
allocation ratio.

## See also

[`getFutilityBounds()`](https://docs.rpact.org/reference/getFutilityBounds.md)

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
} # }
```
