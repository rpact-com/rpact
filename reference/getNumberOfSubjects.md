# Get Number Of Subjects

Returns the number of recruited subjects at given time vector.

## Usage

``` r
getNumberOfSubjects(
  time,
  ...,
  accrualTime = c(0, 12),
  accrualIntensity = 0.1,
  accrualIntensityType = c("auto", "absolute", "relative"),
  maxNumberOfSubjects = NA_real_
)
```

## Arguments

- time:

  A numeric vector with time values.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- accrualTime:

  The assumed accrual time intervals for the study, default is
  `c(0, 12)` (for details see
  [`getAccrualTime()`](https://docs.rpact.org/reference/getAccrualTime.md)).

- accrualIntensity:

  A numeric vector of accrual intensities, default is the relative
  intensity `0.1` (for details see
  [`getAccrualTime()`](https://docs.rpact.org/reference/getAccrualTime.md)).

- accrualIntensityType:

  A character value specifying the accrual intensity input type. Must be
  one of `"auto"`, `"absolute"`, or `"relative"`; default is `"auto"`,
  i.e., if all values are \< 1 the type is `"relative"`, otherwise it is
  `"absolute"`.

- maxNumberOfSubjects:

  If `maxNumberOfSubjects > 0` is specified, the end of accrual at
  specified `accrualIntensity` for the specified number of subjects is
  determined or `accrualIntensity` is calculated at fixed end of
  accrual.

## Value

Returns a
[`NumberOfSubjects`](https://docs.rpact.org/reference/NumberOfSubjects.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://docs.rpact.org/reference/names.FieldSet.md) to
  obtain the field names,

- [`print()`](https://docs.rpact.org/reference/print.FieldSet.md) to
  print the object,

- [`summary()`](https://docs.rpact.org/reference/summary.ParameterSet.md)
  to display a summary of the object,

- [`plot()`](https://docs.rpact.org/reference/plot.NumberOfSubjects.md)
  to plot the object,

- [`as.data.frame()`](https://docs.rpact.org/reference/as.data.frame.ParameterSet.md)
  to coerce the object to a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),

- [`as.matrix()`](https://docs.rpact.org/reference/as.matrix.FieldSet.md)
  to coerce the object to a
  [`matrix`](https://rdrr.io/r/base/matrix.html).

## Details

Calculate number of subjects over time range at given accrual time
vector and accrual intensity. Intensity can either be defined in
absolute or relative terms (for the latter, `maxNumberOfSubjects` needs
to be defined)  
The function is used by
[`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md).

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

[`AccrualTime`](https://docs.rpact.org/reference/AccrualTime.md) for
defining the accrual time.

## Examples

``` r
if (FALSE) { # \dontrun{
getNumberOfSubjects(time = seq(10, 70, 10), accrualTime = c(0, 20, 60), 
    accrualIntensity = c(5, 20))

getNumberOfSubjects(time = seq(10, 70, 10), accrualTime = c(0, 20, 60), 
    accrualIntensity = c(0.1, 0.4), maxNumberOfSubjects = 900)
} # }
```
