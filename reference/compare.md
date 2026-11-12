# Start or Extend a Trial Comparison

S3 generic for comparing existing trial planning objects. The
`ParameterSet` method creates a comparison or appends to an existing
`TrialComparison`.

The `ParameterSet` method starts or extends a trial comparison.

## Usage

``` r
compare(x, y)

# S3 method for class 'ParameterSet'
compare(x, y)
```

## Arguments

- x:

  A supported trial planning object or a `TrialComparison`.

- y:

  A trial design, design characteristics, sample size or power plan,
  simulation result, list of supported objects, or `TrialComparison` to
  add. May be omitted to create a comparison containing only `x`.

## Value

A `TrialComparison`. When `x` is already a comparison, it is modified by
reference if `y` is supplied. Without `y`, a new comparison is returned.

## Details

For supported `ParameterSet` objects, `compare(x, y)` calls
[`getComparison()`](https://docs.rpact.org/reference/getComparison.md)
with `x` and `y`. Supported classes are `TrialDesign`,
`TrialDesignCharacteristics`, `TrialDesignPlan`, `SimulationResults`,
`PowerAndAverageSampleNumberResult`, `EventProbabilities`,
`NumberOfSubjects`, `ConditionalPowerResults` (including subclasses),
`PerformanceScore`, `FisherInformation`, and
`TrialDesignOptimalConditionalErrorCharacteristics`. If `x` is a
`TrialComparison`, it calls `x$add(y)` instead. The input validation and
collection rules of
[`getComparison()`](https://docs.rpact.org/reference/getComparison.md)
apply in both cases. Objects are retained by reference without
recalculating them.

## See also

[`getComparison()`](https://docs.rpact.org/reference/getComparison.md),
[`add()`](https://docs.rpact.org/reference/add.md)

## Examples

``` r
comp <- getDesignInverseNormal(kMax = 2) |>
    compare(getSampleSizeMeans(alternative = 0.2)) |>
    compare(getSampleSizeMeans(alternative = 0.59))
length(comp)
#> [1] 3
```
