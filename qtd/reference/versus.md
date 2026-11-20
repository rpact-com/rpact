# Compare and Summarize Trial Planning Objects

Combines two operands with
[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md)
and returns the summary of the resulting comparison. Repeated use
extends the summarized comparison.

## Usage

``` r
x %vs% y
```

## Arguments

- x, y:

  Supported trial planning objects, their `SummaryFactory` summaries,
  `TrialComparison` objects, or `TrialComparisonSummary` objects.

## Value

A visible `TrialComparisonSummary`, as returned by
`summary(getComparison(x, y))` after unwrapping summary operands.

## Details

Summary operands contribute their underlying `object`. For
`ParameterSet` operands, the supported classes are those accepted by
[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md):
designs, design characteristics, sample size or power plans,
simulations, and power/ASN results, event probabilities, recruitment
results, conditional power, performance scores, Fisher information, and
optimal conditional-error characteristics. Existing comparisons
contribute their contained objects in order, retaining names and
references. The operands' collection membership is not modified. The
validation and unique-name rules of
[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md)
apply.

The result can be assigned, extended with another `%vs%`, or passed
directly to
[`plot.TrialComparison()`](https://docs.rpact.org/qtd/reference/plot.TrialComparison.md).
Chained operations do not print intermediate summaries. Plot axes must
still be specified when their selection is ambiguous.

## See also

[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md),
[`compare()`](https://docs.rpact.org/qtd/reference/compare.md)

## Examples

``` r
comparison <- getSampleSizeMeans(alternative = 0.22, stDev = 0.5) %vs%
    getSampleSizeMeans(alternative = 0.44, stDev = 1) %vs%
    getSampleSizeMeans(alternative = 0.59, stDev = 1)
comparison
#> Sample size calculation comparison summary
#> 
#> Comparison of 3 sample size means calculations.
#> Result numbers follow the comparison input order.
#> The comparison identifies 6 differing parameters and
#> 15 shared parameters or profiles.
#> Shared (examples): Object type, Endpoint, Calculation target, Normal
#>     approximation, Mean ratio, Treatment groups, ... (all shared captions:
#>     showAll = TRUE)
#> 
#> Inputs and design settings:
#> 
#>     Alternatives (Result 1 vs. 2 vs. 3): 0.22 vs. 0.44 vs. 0.59
#> 
#>     Standard deviation (Result 1 vs. 2 vs. 3): 0.5 vs. 1.0 vs. 1.0
#> 
#> Results and derived parameters:
#> 
#>     Critical values (treatment effect scale) [3 profiles]
#>         Result 1: 0.1541470
#>         Result 2: 0.3082939
#>         Result 3: 0.4138996
#> 
#>     Number of subjects fixed [2 profiles]
#>         Result 1, 2: 164.10993
#>         Result 3: 92.15175
#> 
#>     Number of subjects fixed (1) [2 profiles]
#>         Result 1, 2: 82.05496
#>         Result 3: 46.07588
#> 
#>     Number of subjects fixed (2) [2 profiles]
#>         Result 1, 2: 82.05496
#>         Result 3: 46.07588
#> 
#> Legend:
#>     Profiles compare exact values on their applicable coordinates.
#> 
#> Complete display: summary(x, showAll = TRUE).
#> Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.

comp1 <- getSampleSizeMeans(alternative = 0.22) %vs%
    compare(getSampleSizeMeans(alternative = 0.44))
comp2 <- comp1 %vs% compare(getSampleSizeMeans(alternative = 0.59))
comp2
#> Sample size calculation comparison summary
#> 
#> Comparison of 3 sample size means calculations.
#> Result numbers follow the comparison input order.
#> The comparison identifies 5 differing parameters and
#> 16 shared parameters or profiles.
#> Shared (examples): Object type, Endpoint, Calculation target, Standard
#>     deviation, Normal approximation, Mean ratio, ... (all shared captions:
#>     showAll = TRUE)
#> 
#> Inputs and design settings:
#> 
#>     Alternatives (Result 1 vs. 2 vs. 3): 0.22 vs. 0.44 vs. 0.59
#> 
#> Results and derived parameters:
#> 
#>     Critical values (treatment effect scale) [3 profiles]
#>         Result 1: 0.1539693
#>         Result 2: 0.3082939
#>         Result 3: 0.4138996
#> 
#>     Number of subjects fixed [3 profiles]
#>         Result 1: 650.59405
#>         Result 2: 164.10993
#>         Result 3: 92.15175
#> 
#>     Number of subjects fixed (1) [3 profiles]
#>         Result 1: 325.29703
#>         Result 2: 82.05496
#>         Result 3: 46.07588
#> 
#>     Number of subjects fixed (2) [3 profiles]
#>         Result 1: 325.29703
#>         Result 2: 82.05496
#>         Result 3: 46.07588
#> 
#> Legend:
#>     Profiles compare exact values on their applicable coordinates.
#> 
#> Complete display: summary(x, showAll = TRUE).
#> Full data: as.data.frame(x). Exact profiles: summary(x)$parameterInfo.
comp2 |> getComparison()
#> Sample size calculation comparison
#> 
#> 3 objects; 3 comparison rows; 24 columns.
#> 
#> Object types:
#>     Sample size means : 3
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1
#>     Significance level       : 0.025
#>     Type II error rate       : 0.2
#>     Number of subjects fixed : 92.15175, 650.59405
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects

if (FALSE) { # \dontrun{
getSampleSizeMeans(alternative = 0.22, stDev = 0.5) %vs%
    getSampleSizeMeans(alternative = 0.44, stDev = 1) %vs%
    getSampleSizeMeans(alternative = 0.59, stDev = 1) |>
    plot(
        xAxis = "alternative",
        yAxis = c("nFixed", "criticalValuesEffectScale"), category = "stDev"
    )
} # }
```
