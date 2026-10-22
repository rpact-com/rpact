# Create an Array of Piecewise Hazard Ratios

Combines one or more group-by-interval hazard-ratio matrices into an
array with dimensions group by interval by situation.

## Usage

``` r
getPiecewiseHazardRatioArray(..., groupNames = NULL, intervalNames = NULL)
```

## Arguments

- ...:

  Matrices, or one named list of matrices. Each matrix contains groups
  in rows and piecewise survival intervals in columns.

- groupNames:

  Optional group names. Existing row names must agree in all matrices
  and with these names, including their order. Unnamed matrices are
  interpreted positionally.

- intervalNames:

  Optional interval names.

## Value

A numeric array with dimensions group by interval by situation.
