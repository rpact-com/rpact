# Add Expected Dropout Proportions to a Survival Design Plan

Populates derived group-specific dropout proportions at each stage's
planned analysis time in each scenario, including analyses before
recruitment ends.

## Usage

``` r
.addExpectedDropoutProportions(designPlan)
```

## Arguments

- designPlan:

  Survival sample-size or power design plan, modified by reference. Its
  survival and accrual inputs must already be normalized.

## Value

Invisibly returns the assigned derived parameter type; the design plan
is updated by reference.

## Details

Converts input dropout probabilities to exponential hazards and averages
competing-risk dropout incidence over accrued subjects. Stores matrices
with stage rows and alternative-scenario columns. Values remain missing
when observation times are not accounted for or the horizon is
unavailable.
