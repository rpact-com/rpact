# Compute operating characteristics for a fixed quantitative design

getOperatingCharcteristicsMeans computes the operating characteristics
(probabilities of the decision categories) for a fixed (non-adaptive)
two-group design with a quantitative endpoint using a normal
approximation. The decision boundaries are defined by a
`TrialDesignPlanQuantitative` object (created by
`getDesignQuantitative`).

## Usage

``` r
getOperatingCharcteristicsMeans(
  design,
  numberOfSubjects,
  normalApproximation = TRUE,
  groups = 2,
  alternative = seq(0, 1, 0.2),
  stDev = 1
)
```

## Arguments

- design:

  An object of class `TrialDesignPlanQuantitative` describing the
  decision rule (target value, lower reference value, and percentiles
  defining the decision boundaries).

- numberOfSubjects:

  Numeric matrix with two columns (control, treatment) and one or more
  rows. Each row defines a scenario for the per-arm sample sizes to
  evaluate. All entries must be positive integers.

- normalApproximation:

  Logical scalar. Whether to use the normal approximation for the
  sampling distribution of the difference in means. Must be TRUE for
  this implementation. Default: TRUE.

- groups:

  Integer scalar. Number of groups; currently only 2 is supported
  (control and treatment). Default: 2.

- alternative:

  Numeric vector. Grid of true mean differences (treatment - control) at
  which to evaluate the operating characteristics. Default: seq(0, 1,
  0.2).

- stDev:

  Numeric scalar. Common standard deviation for the endpoint in each
  group. Must be positive. Default: 1.

## Value

An object of class `OperatingCharcteristicsMeans` (R6). The object
contains fields: `design`, `numberOfSubjects`, `normalApproximation`,
`alternative`, `stDev`, and `probabilities` (a data.frame with columns
`category`, `probability`, `nControl`, `nTreatment`, `alternative`, and
`stDev`).

## Details

The function validates inputs and then calls internal helpers to compute
category probabilities for each combination of sample sizes and
alternatives. The categories follow the fixed-design decision rule
convention used by the QTD module: "Go (Fixed)", "Go", "Indeterminate
1", "Indeterminate 2", "Stop", and "Stop (Fixed)". Probabilities are
returned as part of an `OperatingCharcteristicsMeans` R6 object which
contains both the design and a data.frame of probabilities for plotting
and reporting.

## Examples

``` r
# Create a decision rule
decisionRule <- getDesignQuantitative(targetValue = 1.5, lowerReferenceValue = 0)

# Evaluate operating characteristics for two sample-size scenarios
nMatrix <- matrix(c(50, 100, 75, 150), ncol = 2, byrow = TRUE)
oc <- getOperatingCharcteristicsMeans(
    design = decisionRule,
    numberOfSubjects = nMatrix,
    alternative = seq(0, 2, 0.5),
    stDev = 1
)
oc$probabilities
#>           category  probability nControl nTreatment alternative stDev
#> 1       Go (Fixed) 1.030720e-21       50        100         0.0     1
#> 2       Go (Fixed) 1.856203e-11       50        100         0.5     1
#> 3       Go (Fixed) 9.636016e-05       50        100         1.0     1
#> 4       Go (Fixed) 2.000000e-01       50        100         1.5     1
#> 5       Go (Fixed) 9.795790e-01       50        100         2.0     1
#> 6               Go 7.993606e-14       50        100         0.0     1
#> 7               Go 3.528663e-06       50        100         0.5     1
#> 8               Go 5.412856e-02       50        100         1.0     1
#> 9               Go 7.000000e-01       50        100         1.5     1
#> 10              Go 2.040567e-02       50        100         2.0     1
#> 11 Indeterminate 1 0.000000e+00       50        100         0.0     1
#> 12 Indeterminate 1 0.000000e+00       50        100         0.5     1
#> 13 Indeterminate 1 0.000000e+00       50        100         1.0     1
#> 14 Indeterminate 1 0.000000e+00       50        100         1.5     1
#> 15 Indeterminate 1 0.000000e+00       50        100         2.0     1
#> 16 Indeterminate 2 2.000000e-01       50        100         0.0     1
#> 17 Indeterminate 2 9.795755e-01       50        100         0.5     1
#> 18 Indeterminate 2 9.457747e-01       50        100         1.0     1
#> 19 Indeterminate 2 1.000000e-01       50        100         1.5     1
#> 20 Indeterminate 2 1.534380e-05       50        100         2.0     1
#> 21            Stop 7.000000e-01       50        100         0.0     1
#> 22            Stop 2.040567e-02       50        100         0.5     1
#> 23            Stop 4.072058e-07       50        100         1.0     1
#> 24            Stop 2.670006e-15       50        100         1.5     1
#> 25            Stop 4.801576e-27       50        100         2.0     1
#> 26    Stop (Fixed) 1.000000e-01       50        100         0.0     1
#> 27    Stop (Fixed) 1.534380e-05       50        100         0.5     1
#> 28    Stop (Fixed) 8.626669e-13       50        100         1.0     1
#> 29    Stop (Fixed) 1.369091e-23       50        100         1.5     1
#> 30    Stop (Fixed) 5.672885e-38       50        100         2.0     1
#> 31      Go (Fixed) 1.200077e-30       75        150         0.0     1
#> 32      Go (Fixed) 1.259440e-15       75        150         0.5     1
#> 33      Go (Fixed) 6.011920e-06       75        150         1.0     1
#> 34      Go (Fixed) 2.000000e-01       75        150         1.5     1
#> 35      Go (Fixed) 9.964691e-01       75        150         2.0     1
#> 36              Go 0.000000e+00       75        150         0.0     1
#> 37              Go 3.529470e-09       75        150         0.5     1
#> 38              Go 1.209263e-02       75        150         1.0     1
#> 39              Go 7.000000e-01       75        150         1.5     1
#> 40              Go 3.530205e-03       75        150         2.0     1
#> 41 Indeterminate 1 0.000000e+00       75        150         0.0     1
#> 42 Indeterminate 1 0.000000e+00       75        150         0.5     1
#> 43 Indeterminate 1 0.000000e+00       75        150         1.0     1
#> 44 Indeterminate 1 0.000000e+00       75        150         1.5     1
#> 45 Indeterminate 1 0.000000e+00       75        150         2.0     1
#> 46 Indeterminate 2 2.000000e-01       75        150         0.0     1
#> 47 Indeterminate 2 9.964691e-01       75        150         0.5     1
#> 48 Indeterminate 2 9.879014e-01       75        150         1.0     1
#> 49 Indeterminate 2 1.000000e-01       75        150         1.5     1
#> 50 Indeterminate 2 7.283515e-07       75        150         2.0     1
#> 51            Stop 7.000000e-01       75        150         0.0     1
#> 52            Stop 3.530205e-03       75        150         0.5     1
#> 53            Stop 2.340428e-10       75        150         1.0     1
#> 54            Stop 7.957132e-23       75        150         1.5     1
#> 55            Stop 1.149407e-40       75        150         2.0     1
#> 56    Stop (Fixed) 1.000000e-01       75        150         0.0     1
#> 57    Stop (Fixed) 7.283515e-07       75        150         0.5     1
#> 58    Stop (Fixed) 3.338277e-17       75        150         1.0     1
#> 59    Stop (Fixed) 6.819446e-33       75        150         1.5     1
#> 60    Stop (Fixed) 5.671924e-54       75        150         2.0     1
```
