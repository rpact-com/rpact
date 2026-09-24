# Plot the optimal conditional error function

The returned plot is a `ggplot2` object and can be supplemented with
additional layers using `ggplot2` commands.

## Usage

``` r
# S3 method for class 'TrialDesignOptimalConditionalError'
plot(
  x,
  y,
  ...,
  range = c(0, 1),
  type = "conditionalError",
  plotNonMonotoneFunction = FALSE
)
```

## Arguments

- x:

  Design object of class `TrialDesignOptimalConditionalError`.

- y:

  Not used; included for compatibility with
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- ...:

  Additional arguments for the generic method.

- range:

  Numeric vector with two entries specifying the range of the x-axis of
  the plot.

- type:

  Type of plot to be created. Options are:

  - `type = "conditionalError"` (or `1`): Plot the values of the optimal
    conditional error function against the first-stage p-value.

  - `type = "stageInformation"` (or `2`): Plot the second-stage
    information resulting from the optimal conditional error function
    against the first-stage p-value.

  - `type = "likelihoodRatio"` (or `3`): Plot the likelihood ratio of
    the given specification of the optimal conditional error function
    against the first-stage p-value.

  - `type = "qFunction"` (or `4`): Plot the function Q of the given
    specification of the optimal conditional error function against the
    first-stage p-value.

- plotNonMonotoneFunction:

  Logical. Should the non-monotone version of the plot be drawn? Not
  applicable for `type = "likelihoodRatio"`. Default: `FALSE`.

## Value

A `ggplot` object.
