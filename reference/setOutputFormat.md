# Set Output Format

With this function the format of the standard outputs of all `rpact`
objects can be changed and set user defined respectively.

## Usage

``` r
setOutputFormat(
  parameterName = NA_character_,
  ...,
  digits = NA_integer_,
  nsmall = NA_integer_,
  trimSingleZeros = NA,
  futilityProbabilityEnabled = NA,
  file = NA_character_,
  resetToDefault = FALSE,
  roundFunction = NA_character_,
  persist = TRUE
)
```

## Arguments

- parameterName:

  The name of the parameter whose output format shall be edited. Leave
  the default `NA_character_` if the output format of all parameters
  shall be edited.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- digits:

  How many significant digits are to be used for a numeric value. The
  default, `NULL`, uses getOption("digits"). Allowed values are
  `0 <= digits <= 20`.

- nsmall:

  The minimum number of digits to the right of the decimal point in
  formatting real numbers in non-scientific formats. Allowed values are
  `0 <= nsmall <= 20`.

- trimSingleZeros:

  If `TRUE` zero values will be trimmed in the output, e.g., "0.00" will
  displayed as "0"

- futilityProbabilityEnabled:

  If `TRUE` very small value (\< 1e-09) will be displayed as "0",
  default is `FALSE`.

- file:

  An optional file name of an existing text file that contains output
  format definitions (see Details for more information).

- resetToDefault:

  If `TRUE` all output formats will be reset to default value. Note that
  other settings will be executed afterwards if specified, default is
  `FALSE`.

- roundFunction:

  A character value that specifies the R base round function to use,
  default is `NA_character_`. Allowed values are "ceiling", "floor",
  "trunc", "round", "signif", and `NA_character_`.

- persist:

  A logical value indicating whether the output format settings should
  be saved persistently. Default is `TRUE`.

## Details

Output formats can be written to a text file (see
[`getOutputFormat()`](https://docs.rpact.org/reference/getOutputFormat.md)).
To load your personal output formats read a formerly saved file at the
beginning of your work with `rpact`, e.g. execute
`setOutputFormat(file = "my_rpact_output_formats.txt")`.

Note that the `parameterName` must not match exactly, e.g., for p-values
the following parameter names will be recognized amongst others:

1.  `p value`

2.  `p.values`

3.  `p-value`

4.  `pValue`

5.  `rpact.output.format.p.value`

## See also

[`format`](https://rdrr.io/r/base/format.html) for details on the
function used internally to format the values.

Other output formats:
[`getOutputFormat()`](https://docs.rpact.org/reference/getOutputFormat.md)

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
