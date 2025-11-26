# Get Power And Average Sample Number

Returns the power and average sample number of the specified design.

## Usage

``` r
getPowerAndAverageSampleNumber(design, theta = seq(-1, 1, 0.02), nMax = 100)
```

## Arguments

- design:

  The trial design.

- theta:

  A vector of standardized effect sizes (theta values), default is a
  sequence from -1 to 1.

- nMax:

  The maximum sample size. Must be a positive integer of length 1.

## Value

Returns a
[`PowerAndAverageSampleNumberResult`](https://docs.rpact.org/reference/PowerAndAverageSampleNumberResult.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.ParameterSet.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.PowerAndAverageSampleNumberResult.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

This function returns the power and average sample number (ASN) of the
specified design for the prototype case which is testing H0: mu = mu0 in
a one-sample design. `theta` represents the standardized effect
`(mu - mu0) / sigma` and power and ASN is calculated for maximum sample
size `nMax`. For other designs than the one-sample test of a mean the
standardized effect needs to be adjusted accordingly.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://docs.rpact.org/reference/plot.AnalysisResults.md).

## See also

Other design functions:
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md),
[`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate power, stopping probabilities, and expected sample 
# size for the default design with specified theta and nMax  
getPowerAndAverageSampleNumber(
    getDesignGroupSequential(), 
    theta = seq(-1, 1, 0.5), nMax = 100)
} # }
```
