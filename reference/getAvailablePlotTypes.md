# Get Available Plot Types

Function to identify the available plot types of an object.

## Usage

``` r
plotTypes(
  obj,
  output = c("numeric", "caption", "numcap", "capnum"),
  numberInCaptionEnabled = FALSE
)

getAvailablePlotTypes(
  obj,
  output = c("numeric", "caption", "numcap", "capnum"),
  numberInCaptionEnabled = FALSE
)
```

## Arguments

- obj:

  The object for which the plot types shall be identified, e.g. produced
  by
  [`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md)
  or
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md).

- output:

  The output type. Can be one of
  `c("numeric", "caption", "numcap", "capnum")`.

- numberInCaptionEnabled:

  If `TRUE`, the number will be added to the caption, default is
  `FALSE`.

## Value

Returns a list if `option` is either `capnum` or `numcap` or returns a
vector that is of character type for `option=caption` or of numeric type
for `option=numeric`.

## Details

`plotTypes` and `getAvailablePlotTypes()` are equivalent, i.e.,
`plotTypes` is a short form of `getAvailablePlotTypes()`.

`output`:

1.  `numeric`: numeric output

2.  `caption`: caption as character output

3.  `numcap`: list with number and caption

4.  `capnum`: list with caption and number

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(kMax = 2)
getAvailablePlotTypes(design, "numeric")
plotTypes(design, "caption")
getAvailablePlotTypes(design, "numcap")
plotTypes(design, "capnum")
} # }
```
