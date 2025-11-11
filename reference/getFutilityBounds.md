# Get Futility Bounds

This function converts futility bounds between different scales such as
z-value, p-value, conditional power, predictive power, reverse
conditional power, and effect estimate.

## Usage

``` r
getFutilityBounds(
  sourceValue,
  ...,
  sourceScale = c("zValue", "pValue", "conditionalPower", "condPowerAtObserved",
    "predictivePower", "reverseCondPower", "effectEstimate"),
  targetScale = c("zValue", "pValue", "conditionalPower", "condPowerAtObserved",
    "predictivePower", "reverseCondPower", "effectEstimate"),
  design = NULL,
  theta = NA_real_,
  information = NA_real_,
  naAllowed = FALSE
)
```

## Arguments

- sourceValue:

  A numeric vector representing the futility bounds in the source scale.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- sourceScale:

  Character. The scale of the input futility bounds. Must be one of
  `"zValue"`, `"pValue"`, `"conditionalPower"`, "condPowerAtObserved",
  `"predictivePower"`, `"reverseCondPower"`, or `"effectEstimate"`.

- targetScale:

  Character. The scale to which the futility bounds should be converted.
  Must be one of `"zValue"`, `"pValue"`, `"conditionalPower"`,
  "condPowerAtObserved", `"predictivePower"`, `"reverseCondPower"`, or
  `"effectEstimate"`.

- design:

  The trial design. Required if either the `sourceScale` or
  `targetScale` is `"reverseCondPower"` or if the conversion involves
  conditional or predictive power in a group sequential or Fisher
  design. Must be a one-sided two-stage group sequential design or
  Fisher's combination test design.

- theta:

  Numeric. The assumed effect size under the alternative hypothesis.

- information:

  Numeric vector of length 2. The information levels at the two stages.

- naAllowed:

  Logical. Indicates if `NA` `sourceValue` are permitted. Default is
  `FALSE`.

## Value

A numeric vector representing the futility bounds in the target scale,
or `NULL` if the conversion is not implemented or yields no result.

## Details

If the `sourceScale` and `targetScale` are the same, the function
returns the input `sourceValue` without modification. Otherwise, the
function is designed to convert between the specified scales.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with identical source and target scales
getFutilityBounds(
    sourceValue = c(0, 0.5),
    sourceScale = "zValue",
    targetScale = "zValue"
)

# Example with different scales
getFutilityBounds(
    design = getDesignGroupSequential(kMax = 2, typeOfDesign = "noEarlyEfficacy", alpha = 0.05),
    information = c(10, 10),
    sourceValue = 0.5,
    sourceScale = "condPowerAtObserved",
    targetScale = "pValue"
)
} # }
```
