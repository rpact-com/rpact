# Raw Dataset Of A Two Arm Continuous Outcome With Covariates

An artificial dataset that was randomly generated with simulated normal
data. The data set has six variables:

1.  Subject id

2.  Stage number

3.  Group name

4.  An example outcome in that we are interested in

5.  The first covariate *gender*

6.  The second covariate *covariate*

## Usage

``` r
rawDataTwoArmNormal
```

## Format

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) object.

## Details

See the vignette "Two-arm analysis for continuous data with covariates
from raw data" to learn how to

- import raw data from a csv file,

- calculate estimated adjusted (marginal) means (EMMs, least-squares
  means) for a linear model, and

- perform two-arm interim analyses with these data.

You can use `rawDataTwoArmNormal` to reproduce the examples in the
vignette.
