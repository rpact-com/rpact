# Get Fixed Design

Create a fixed (single-stage) trial design. This convenience wrapper
constructs an object of class `TrialDesignFixed` with `kMax = 1`.

## Usage

``` r
getDesignFixed(
  alpha = 0.025,
  beta = 0.2,
  sided = 1L,
  directionUpper = NA,
  twoSidedPower = NA
)
```

## Arguments

- alpha:

  The significance level alpha, default is `0.025`. Must be a positive
  numeric of length 1.

- beta:

  Type II error rate, necessary for providing sample size calculations
  (e.g.,
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md)),
  beta spending function designs, or optimum designs, default is `0.20`.
  Must be a positive numeric of length 1.

- sided:

  Is the alternative one-sided (`1`) or two-sided (`2`), default is `1`.
  Must be a positive integer of length 1.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- twoSidedPower:

  For two-sided testing, if `twoSidedPower = TRUE` is specified the
  sample size calculation is performed by considering both tails of the
  distribution. Default is `FALSE`, i.e., it is assumed that one tail
  probability is equal to 0 or the power should be directed to one
  part.#'

## Value

Returns a
[`TrialDesign`](https://docs.rpact.org/reference/TrialDesign.md) object.
The following generics (R generic functions) are available for this
result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.TrialDesign.md) to
  plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.TrialDesign.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

The fixed design represents a single-stage hypothesis test. The returned
object can be used with the standard analysis and plotting helpers in
the package. Typical use is to specify the type I error `alpha` and the
desired type II error `beta` (or equivalently power = 1 - beta).

## See also

Other design functions:
[`getDesignCharacteristics()`](https://docs.rpact.org/reference/getDesignCharacteristics.md),
[`getDesignConditionalDunnett()`](https://docs.rpact.org/reference/getDesignConditionalDunnett.md),
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md),
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)

## Examples

``` r
# Basic fixed design with default alpha and beta
design <- getDesignFixed()
design
#> Design parameters and output of fixed sample size design:
#> 
#> User defined parameters: not available
#> 
#> Derived from user defined parameters: not available
#> 
#> Default parameters:
#>   Significance level              : 0.0250 
#>   Type II error rate              : 0.2000 
#>   Test                            : one-sided 
#> 
#> Output:
#>   Critical values                 : 1.960 
#>   Stage levels (one-sided)        : 0.0250 
#> 

# Custom significance and power
design2 <- getDesignFixed(alpha = 0.05, beta = 0.1)
design2
#> Design parameters and output of fixed sample size design:
#> 
#> User defined parameters:
#>   Significance level              : 0.0500 
#>   Type II error rate              : 0.1000 
#> 
#> Derived from user defined parameters: not available
#> 
#> Default parameters:
#>   Test                            : one-sided 
#> 
#> Output:
#>   Critical values                 : 1.645 
#>   Stage levels (one-sided)        : 0.0500 
#> 
```
