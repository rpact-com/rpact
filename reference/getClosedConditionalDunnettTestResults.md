# Get Closed Conditional Dunnett Test Results

Calculates and returns the results from the closed conditional Dunnett
test.

## Usage

``` r
getClosedConditionalDunnettTestResults(
  stageResults,
  ...,
  stage = stageResults$stage
)
```

## Arguments

- stageResults:

  The results at given stage, obtained from
  [`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md).

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- stage:

  The stage number (optional). Default: total number of existing stages
  in the data input.

## Value

Returns a
[`ClosedCombinationTestResults`](https://rpact-com.github.io/rpact/reference/ClosedCombinationTestResults.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object,

- [`summary()`](https://rpact-com.github.io/rpact/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://rpact-com.github.io/rpact/reference/plot.ParameterSet.md)
  to plot the object,

- [`as.data.frame()`](https://rpact-com.github.io/rpact/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://rpact-com.github.io/rpact/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

For performing the conditional Dunnett test the design must be defined
through the function
[`getDesignConditionalDunnett()`](https://rpact-com.github.io/rpact/reference/getDesignConditionalDunnett.md).  
See Koenig et al. (2008) and Wassmer & Brannath (2025), chapter 11 for
details of the test procedure.

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md).

## See also

Other analysis functions:
[`getAnalysisResults()`](https://rpact-com.github.io/rpact/reference/getAnalysisResults.md),
[`getClosedCombinationTestResults()`](https://rpact-com.github.io/rpact/reference/getClosedCombinationTestResults.md),
[`getConditionalPower()`](https://rpact-com.github.io/rpact/reference/getConditionalPower.md),
[`getConditionalRejectionProbabilities()`](https://rpact-com.github.io/rpact/reference/getConditionalRejectionProbabilities.md),
[`getFinalConfidenceInterval()`](https://rpact-com.github.io/rpact/reference/getFinalConfidenceInterval.md),
[`getFinalPValue()`](https://rpact-com.github.io/rpact/reference/getFinalPValue.md),
[`getRepeatedConfidenceIntervals()`](https://rpact-com.github.io/rpact/reference/getRepeatedConfidenceIntervals.md),
[`getRepeatedPValues()`](https://rpact-com.github.io/rpact/reference/getRepeatedPValues.md),
[`getStageResults()`](https://rpact-com.github.io/rpact/reference/getStageResults.md),
[`getTestActions()`](https://rpact-com.github.io/rpact/reference/getTestActions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a two-stage design a conditional Dunnett test should be performed
# where the  unconditional second stage p-values should be used for the
# test decision.
# At the first stage the second treatment arm was dropped. The results of
# a closed conditionsal Dunnett test are obtained as follows with the given
# data (treatment arm 4 refers to the reference group):
data <- getDataset(
    n1 = c(22, 23),
    n2 = c(21, NA),
    n3 = c(20, 25),
    n4 = c(25, 27),
    means1 = c(1.63, 1.51),
    means2 = c(1.4, NA),
    means3 = c(0.91, 0.95),
    means4 = c(0.83, 0.75),
    stds1 = c(1.2, 1.4),
    stds2 = c(1.3, NA),
    stds3 = c(1.1, 1.14),
    stds4 = c(1.02, 1.18)
)

# For getting the results of the closed test procedure, use the following commands:
design <- getDesignConditionalDunnett(secondStageConditioning = FALSE)
stageResults <- getStageResults(design, dataInput = data)
getClosedConditionalDunnettTestResults(stageResults)
} # }
```
