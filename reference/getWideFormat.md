# Get Wide Format

Returns the specified dataset as a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) in so-called wide
format.

## Usage

``` r
getWideFormat(dataInput)
```

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) will be
returned.

## Details

In the wide format (unstacked), the data are presented with each
different data variable in a separate column, i.e., the different groups
are in separate columns.

## See also

[`getLongFormat()`](https://docs.rpact.org/reference/getLongFormat.md)
for returning the dataset as a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) in long format.
