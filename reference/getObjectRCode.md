# Get Object R Code

Returns the R source command of a result object.

## Usage

``` r
rcmd(
  obj,
  ...,
  leadingArguments = NULL,
  includeDefaultParameters = FALSE,
  stringWrapParagraphWidth = 90,
  prefix = "",
  postfix = "",
  stringWrapPrefix = "",
  newArgumentValues = list(),
  tolerance = 1e-07,
  pipeOperator = c("auto", "none", "magrittr", "R"),
  output = c("vector", "cat", "test", "markdown", "internal"),
  explicitPrint = FALSE
)

getObjectRCode(
  obj,
  ...,
  leadingArguments = NULL,
  includeDefaultParameters = FALSE,
  stringWrapParagraphWidth = 90,
  prefix = "",
  postfix = "",
  stringWrapPrefix = "",
  newArgumentValues = list(),
  tolerance = 1e-07,
  pipeOperator = c("auto", "none", "magrittr", "R"),
  output = c("vector", "cat", "test", "markdown", "internal"),
  explicitPrint = FALSE
)
```

## Arguments

- obj:

  The result object.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- leadingArguments:

  A character vector with arguments that shall be inserted at the
  beginning of the function command, e.g., `design = x`. Be careful with
  this option because the created R command may no longer be valid if
  used.

- includeDefaultParameters:

  If `TRUE`, default parameters will be included in all `rpact`
  commands; default is `FALSE`.

- stringWrapParagraphWidth:

  An integer value defining the number of characters after which a line
  break shall be inserted; set to `NULL` to insert no line breaks.

- prefix:

  A character string that shall be added to the beginning of the R
  command.

- postfix:

  A character string that shall be added to the end of the R command.

- stringWrapPrefix:

  A prefix character string that shall be added to each new line,
  typically some spaces.

- newArgumentValues:

  A named list with arguments that shall be renewed in the R command,
  e.g., `newArgumentValues = list(informationRates = c(0.5, 1))`.

- tolerance:

  The tolerance for defining a value as default.

- pipeOperator:

  The pipe operator to use in the R code, default is "none".

- output:

  The output format, default is a character "vector".

- explicitPrint:

  Show an explicit `print` command, default is `FALSE`.

## Value

A [`character`](https://rdrr.io/r/base/character.html) value or vector
will be returned.

## Details

`getObjectRCode()` (short: `rcmd()`) recreates the R commands that
result in the specified object `obj`. `obj` must be an instance of class
`ParameterSet`.
