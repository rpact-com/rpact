# Get Long Format

Returns the specified dataset as a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) in so-called long
format.

## Usage

``` r
getLongFormat(dataInput)
```

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) will be
returned.

## Details

In the long format (narrow, stacked), the data are presented with one
column containing all the values and another column listing the context
of the value, i.e., the data for the different groups are in one column
and the dataset contains an additional "group" column.

## See also

[`getWideFormat()`](https://rpact-com.github.io/rpact/reference/getWideFormat.md)
for returning the dataset as a
[`data.frame`](https://rdrr.io/r/base/data.frame.html) in wide format.
