# Get Output Format

With this function the format of the standard outputs of all `rpact`
objects can be shown and written to a file.

## Usage

``` r
getOutputFormat(
  parameterName = NA_character_,
  ...,
  file = NA_character_,
  default = FALSE,
  fields = TRUE
)
```

## Arguments

- parameterName:

  The name of the parameter whose output format shall be returned. Leave
  the default `NA_character_` if the output format of all parameters
  shall be returned.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- file:

  An optional file name where to write the output formats (see Details
  for more information).

- default:

  If `TRUE` the default output format of the specified parameter(s) will
  be returned, default is `FALSE`.

- fields:

  If `TRUE` the names of all affected object fields will be displayed,
  default is `TRUE`.

## Value

A named list of output formats.

## Details

Output formats can be written to a text file by specifying a `file`. See
[`setOutputFormat()`](https://rpact-com.github.io/rpact/reference/setOutputFormat.md)()
to learn how to read a formerly saved file.

Note that the `parameterName` must not match exactly, e.g., for p-values
the following parameter names will be recognized amongst others:

1.  `p value`

2.  `p.values`

3.  `p-value`

4.  `pValue`

5.  `rpact.output.format.p.value`

## See also

Other output formats:
[`setOutputFormat()`](https://rpact-com.github.io/rpact/reference/setOutputFormat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# show output format of p values
getOutputFormat("p.value")

# set new p value output format
setOutputFormat("p.value", digits = 5, nsmall = 5)

# show sample sizes as smallest integers not less than the not rounded values
setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "ceiling")
getSampleSizeMeans()

# show sample sizes as smallest integers not greater than the not rounded values
setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "floor")
getSampleSizeMeans()

# set new sample size output format without round function
setOutputFormat("sample size", digits = 2, nsmall = 2)
getSampleSizeMeans()

# reset sample size output format to default
setOutputFormat("sample size")
getSampleSizeMeans()
getOutputFormat("sample size")
} # }
```
