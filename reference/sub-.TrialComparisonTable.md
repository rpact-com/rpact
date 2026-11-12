# Subset a Trial Comparison Table

Subsets a reporting data frame while preserving its rendering
attributes.

## Usage

``` r
# S3 method for class 'TrialComparisonTable'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  A `TrialComparisonTable`.

- i, j:

  Row and column indices, as for a data frame.

- ...:

  Further arguments passed to the data-frame subsetting method.

- drop:

  Whether to simplify the result, as for a data frame.

## Value

A subset retaining its reporting metadata when the result is a data
frame.
