# Add Objects to a Collection

S3 generic for adding objects. The `TrialComparison` method delegates to
`$add()` and modifies the existing comparison by reference.

The `TrialComparison` method appends objects to the comparison.

## Usage

``` r
add(x, ...)

# S3 method for class 'TrialComparison'
add(x, ...)
```

## Arguments

- x:

  An object supporting an `add()` method, such as a `TrialComparison`.

- ...:

  For `TrialComparison`, designs, plans, simulations, lists or
  comparisons to append. The optional named argument `objects` is
  appended after the other arguments, with the same rules as
  [`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md).

## Value

The modified comparison for `TrialComparison`.

## See also

[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md),
[`delete()`](https://docs.rpact.org/qtd/reference/delete.md)

## Examples

``` r
comparison <- getComparison(fixed = getDesignFixed())
comparison |> add(sequential = getDesignGroupSequential(kMax = 2))
#> Design comparison
#> 
#> 2 objects; 3 comparison rows; 19 columns.
#> 
#> Object types:
#>     Fixed sample size design : 1
#>     Group sequential design  : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1, 2
#>     Significance level       : 0.025
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
comparison |> add(objects = list(other = getDesignFixed(alpha = 0.05)))
#> Design comparison
#> 
#> 3 objects; 4 comparison rows; 19 columns.
#> 
#> Object types:
#>     Fixed sample size design : 2
#>     Group sequential design  : 1
#> 
#> Observed metrics (across objects, stages and situations):
#>     Maximum number of stages : 1, 2
#>     Significance level       : 0.025, 0.050
#>     Type II error rate       : 0.2
#> 
#> Available operations:
#>     summary(x)                        : Compare differences
#>     as.data.frame(x)                  : Access all data
#>     plot(x, xAxis = ..., yAxis = ...) : Plot selected parameters
#>     getObjects(x)                     : Access contained objects
```
