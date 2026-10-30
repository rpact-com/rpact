# Select Comparison Parameters

Selects varying parameters, axis candidates, or grouping variables using
getComparisonParameterInfo(). Column order is retained.

## Usage

``` r
getComparisonParameterNames(
  data,
  ...,
  varyingOnly = TRUE,
  type = "all",
  exclude = character(),
  maxNumberOfLevels = 5L,
  binaryIntegerAsFactor = TRUE
)
```

## Arguments

- data:

  A data.frame, typically returned by getComparisonData(). Columns must
  be one-dimensional atomic vectors, including factors and dates.

- ...:

  Further arguments. Currently unused.

- varyingOnly:

  Whether to exclude constant and empty columns.

- type:

  One of "all", "continuous", "factor", "binary", or "groupable".
  Groupable columns are categorical or have at most maxNumberOfLevels
  observed values. An empty column is never groupable. Continuous binary
  columns remain eligible as continuous axes.

- exclude:

  Character vector of names to exclude, e.g., identifiers or variables
  already selected for an axis or a category.

- maxNumberOfLevels:

  Positive upper limit for numerical grouping variables.

- binaryIntegerAsFactor:

  Whether integer-valued binary variables are also eligible for use as
  factors. This does not change the source column.

## Value

A character vector of column names. No plot or GUI state is changed.

## Examples

``` r
data <- data.frame(alpha = 0.025, effect = c(0.2, 0.3), power = c(0.8, 0.9))
getComparisonParameterNames(data)
#> [1] "effect" "power" 
getComparisonParameterNames(data, exclude = "effect", type = "continuous")
#> [1] "power"
```
