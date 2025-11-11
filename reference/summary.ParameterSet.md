# Parameter Set Summary

Displays a summary of
[`ParameterSet`](https://rpact-com.github.io/rpact/reference/ParameterSet.md)
object.

## Usage

``` r
# S3 method for class 'ParameterSet'
summary(
  object,
  ...,
  type = 1,
  digits = NA_integer_,
  output = c("all", "title", "overview", "body"),
  printObject = FALSE,
  sep = NA_character_
)
```

## Arguments

- object:

  A
  [`ParameterSet`](https://rpact-com.github.io/rpact/reference/ParameterSet.md)
  object.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- digits:

  Defines how many digits are to be used for numeric values. Must be a
  positive integer of length 1.

- output:

  The output parts, default is `"all"`.

- printObject:

  Show also the print output after the summary, default is `FALSE`.

- sep:

  The separator line between the summary and the optional print output,
  default is `"\n\n-----\n\n"`.

## Value

Returns a
[`SummaryFactory`](https://rpact-com.github.io/rpact/reference/SummaryFactory.md)
object. The following generics (R generic functions) are available for
this result object:

- [`names()`](https://rpact-com.github.io/rpact/reference/names.FieldSet.md)
  to obtain the field names,

- [`print()`](https://rpact-com.github.io/rpact/reference/print.FieldSet.md)
  to print the object

## Details

Summarizes the parameters and results of a parameter set.

## Summary options

The following options can be set globally:

1.  `rpact.summary.output.size`: one of `c("small", "medium", "large")`;
    defines how many details will be included into the summary; default
    is `"large"`, i.e., all available details are displayed.

2.  `rpact.summary.justify`: one of `c("right", "left", "centre")`;
    shall the values be right-justified (the default), left-justified or
    centered.

3.  `rpact.summary.width`: defines the maximum number of characters to
    be used per line (default is `83`).

4.  `rpact.summary.intervalFormat`: defines how intervals will be
    displayed in the summary, default is `"[%s; %s]"`.

5.  `rpact.summary.digits`: defines how many digits are to be used for
    numeric values (default is `3`).

6.  `rpact.summary.digits.probs`: defines how many digits are to be used
    for numeric values (default is one more than value of
    `rpact.summary.digits`, i.e., `4`).

7.  `rpact.summary.trim.zeroes`: if `TRUE` (default) zeroes will always
    displayed as "0", e.g. "0.000" will become "0".

Example: `options("rpact.summary.intervalFormat" = "%s - %s")`

## How to get help for generic functions

Click on the link of a generic in the list above to go directly to the
help documentation of the `rpact` specific implementation of the
generic. Note that you can use the R function
[`methods`](https://rdrr.io/r/utils/methods.html) to get all the methods
of a generic and to identify the object specific name of it, e.g., use
`methods("plot")` to get all the methods for the `plot` generic. There
you can find, e.g., `plot.AnalysisResults` and obtain the specific help
documentation linked above by typing
[`?plot.AnalysisResults`](https://rpact-com.github.io/rpact/reference/plot.AnalysisResults.md).
