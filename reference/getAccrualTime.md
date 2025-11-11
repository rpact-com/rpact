# Get Accrual Time

Returns an `AccrualTime` object that contains the accrual time and the
accrual intensity.

## Usage

``` r
getAccrualTime(
  accrualTime = NA_real_,
  ...,
  accrualIntensity = NA_real_,
  accrualIntensityType = c("auto", "absolute", "relative"),
  maxNumberOfSubjects = NA_real_
)
```

## Arguments

- accrualTime:

  The assumed accrual time intervals for the study, default is
  `c(0, 12)` (for details see `getAccrualTime()`).

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- accrualIntensity:

  A numeric vector of accrual intensities, default is the relative
  intensity `0.1` (for details see `getAccrualTime()`).

- accrualIntensityType:

  A character value specifying the accrual intensity input type. Must be
  one of `"auto"`, `"absolute"`, or `"relative"`; default is `"auto"`,
  i.e., if all values are \< 1 the type is `"relative"`, otherwise it is
  `"absolute"`.

- maxNumberOfSubjects:

  The maximum number of subjects.

## Value

Returns an
[`AccrualTime`](https://rpact-com.github.io/rpact/reference/AccrualTime.md)
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

## Staggered patient entry

`accrualTime` is the time period of subjects' accrual in a study. It can
be a value that defines the end of accrual or a vector. In this case,
`accrualTime` can be used to define a non-constant accrual over time.
For this, `accrualTime` is a vector that defines the accrual intervals.
The first element of `accrualTime` must be equal to `0` and,
additionally, `accrualIntensity` needs to be specified.
`accrualIntensity` itself is a value or a vector (depending on the
length of `accrualTime`) that defines the intensity how subjects enter
the trial in the intervals defined through `accrualTime`.

`accrualTime` can also be a list that combines the definition of the
accrual time and accrual intensity (see below and examples for details).

If the length of `accrualTime` and the length of `accrualIntensity` are
the same (i.e., the end of accrual is undefined),
`maxNumberOfSubjects > 0` needs to be specified and the end of accrual
is calculated. In that case, `accrualIntensity` is the number of
subjects per time unit, i.e., the absolute accrual intensity.

If the length of `accrualTime` equals the length of
`accrualIntensity - 1` (i.e., the end of accrual is defined),
`maxNumberOfSubjects` is calculated if the absolute accrual intensity is
given. If all elements in `accrualIntensity` are smaller than 1,
`accrualIntensity` defines the *relative* intensity how subjects enter
the trial. For example, `accrualIntensity = c(0.1, 0.2)` specifies that
in the second accrual interval the intensity is doubled as compared to
the first accrual interval. The actual (absolute) accrual intensity is
calculated for the calculated or given `maxNumberOfSubjects`. Note that
the default is `accrualIntensity = 0.1` meaning that the *absolute*
accrual intensity will be calculated.

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

[`getNumberOfSubjects()`](https://rpact-com.github.io/rpact/reference/getNumberOfSubjects.md)
for calculating the number of subjects at given time points.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume that in a trial the accrual after the first 6 months is doubled
# and the total accrual time is 30 months.
# Further assume that a total of 1000 subjects are entered in the trial.
# The number of subjects to be accrued in the first 6 months and afterwards
# is achieved through
getAccrualTime(
    accrualTime = c(0, 6, 30),
    accrualIntensity = c(0.1, 0.2), maxNumberOfSubjects = 1000
)

# The same result is obtained via the list based definition
getAccrualTime(
    list(
        "0 - <6"   = 0.1,
        "6 - <=30" = 0.2
    ),
    maxNumberOfSubjects = 1000
)

# Calculate the end of accrual at given absolute intensity:
getAccrualTime(
    accrualTime = c(0, 6),
    accrualIntensity = c(18, 36), maxNumberOfSubjects = 1000
)

# Via the list based definition this is
getAccrualTime(
    list(
        "0 - <6" = 18,
        ">=6" = 36
    ),
    maxNumberOfSubjects = 1000
)

# You can use an accrual time object in getSampleSizeSurvival() or
# getPowerSurvival().
# For example, if the maximum number of subjects and the follow up
# time needs to be calculated for a given effect size:
accrualTime <- getAccrualTime(
    accrualTime = c(0, 6, 30),
    accrualIntensity = c(0.1, 0.2)
)
getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)

# Or if the power and follow up time needs to be calculated for given
# number of events and subjects:
accrualTime <- getAccrualTime(
    accrualTime = c(0, 6, 30),
    accrualIntensity = c(0.1, 0.2), maxNumberOfSubjects = 110
)
getPowerSurvival(
    accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2,
    maxNumberOfEvents = 46
)

# How to show accrual time details

# You can use a sample size or power object as argument for the function
# getAccrualTime():
sampleSize <- getSampleSizeSurvival(
    accrualTime = c(0, 6), accrualIntensity = c(22, 53),
    lambda2 = 0.05, hazardRatio = 0.8, followUpTime = 6
)
sampleSize
accrualTime <- getAccrualTime(sampleSize)
accrualTime
} # }
```
