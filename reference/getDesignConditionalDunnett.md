# Get Design Conditional Dunnett Test

Defines the design to perform an analysis with the conditional Dunnett
test.

## Usage

``` r
getDesignConditionalDunnett(
  alpha = 0.025,
  informationAtInterim = 0.5,
  ...,
  secondStageConditioning = TRUE,
  directionUpper = NA
)
```

## Arguments

- alpha:

  The significance level alpha, default is `0.025`. Must be a positive
  numeric of length 1.

- informationAtInterim:

  The information to be expected at interim, default is
  `informationAtInterim = 0.5`.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- secondStageConditioning:

  The way the second stage p-values are calculated within the closed
  system of hypotheses. If `secondStageConditioning = FALSE` is
  specified, the unconditional adjusted p-values are used, otherwise
  conditional adjusted p-values are calculated, default is
  `secondStageConditioning = TRUE` (for details, see Koenig et al.,
  2008).

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

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

For performing the conditional Dunnett test the design must be defined
through this function. You can define the information fraction and the
way of how to compute the second stage p-values only in the design
definition, and not in the analysis call.  
See
[`getClosedConditionalDunnettTestResults()`](https://docs.rpact.org/reference/getClosedConditionalDunnettTestResults.md)
for an example and Koenig et al. (2008) and Wassmer & Brannath (2025),
chapter 11 for details of the test procedure.

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
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md),
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getGroupSequentialProbabilities()`](https://docs.rpact.org/reference/getGroupSequentialProbabilities.md),
[`getPowerAndAverageSampleNumber()`](https://docs.rpact.org/reference/getPowerAndAverageSampleNumber.md)
