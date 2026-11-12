# Trial Comparison Data Frame

Converts a trial comparison to a data frame, optionally prepared for
reporting.

## Usage

``` r
# S3 method for class 'TrialComparison'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  niceColumnNamesEnabled = FALSE,
  includeAllParameters = FALSE,
  harmonizeColumnNames = TRUE,
  removeConstantParameters = FALSE,
  transposed = FALSE,
  removeVariableParameters = FALSE,
  digits = NA_integer_,
  ceilingEnabled = FALSE,
  output = "data.frame"
)
```

## Arguments

- x:

  A `TrialComparison`.

- row.names:

  Optional row names for the result.

- optional:

  Unused; included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Further arguments. Currently unused.

- niceColumnNamesEnabled:

  Logical. If `TRUE`, nice looking column names will be used; syntactic
  names (variable names) otherwise (see
  [`make.names`](https://rdrr.io/r/base/make.names.html)).

- includeAllParameters:

  If `TRUE`, include all parameters supported by the existing data-frame
  methods of the individual objects.

- harmonizeColumnNames:

  If `TRUE` (default), apply the class-specific mappings described
  below. If `FALSE`, retain original parameter names and omit extra
  fields that are included only to support these mappings. Readable
  labels remain controlled by `niceColumnNamesEnabled`.

- removeConstantParameters:

  If `TRUE`, retain identifiers and only parameters with different
  object profiles, using the same rules as
  [`summary()`](https://rdrr.io/r/base/summary.html).

- transposed:

  If `TRUE`, return parameters as rows and comparison situations as
  columns. Mixed column types are converted to a common type by
  transposition.

- removeVariableParameters:

  If `TRUE`, return shared parameter profiles once, with stage and
  situation coordinates where needed, without object identifiers. Cannot
  be combined with `removeConstantParameters = TRUE`.

- digits:

  Integer number of decimal places for numeric parameters, or `NA` to
  retain full precision. Parameter profiles are compared before
  rounding.

- ceilingEnabled:

  If `TRUE`, round sample-size and event-count parameters upwards to
  integers, using rpact parameter metadata. Variances are excluded.

- output:

  Output mode: `"data.frame"` (default), `"auto"`, `"html"`, or `"pdf"`.
  Reporting modes add class `TrialComparisonTable` and rendering
  attributes; they still return a data frame. `"auto"` selects HTML or
  LaTeX during knitting. `"pdf"` produces LaTeX tables for a PDF
  document, not a PDF file.

## Value

A data.frame, unrounded and with all columns by default. Reporting modes
additionally inherit from `TrialComparisonTable` for automatic knitr
output.

## Details

With `harmonizeColumnNames = TRUE`, fixed-plan sample size columns are
harmonized when the comparison includes interim analyses: `nFixed`,
`nFixed1`, and `nFixed2` become `numberOfSubjects`, `numberOfSubjects1`,
and `numberOfSubjects2`. Fixed survival events use
`cumulativeEventsPerStage` in mixed comparisons with interim analyses.
Available sample-size H1 expectations use `expectedNumberOfSubjects` and
`expectedNumberOfEvents`, referring to each scenario's supplied
alternative. See
[`getComparison()`](https://docs.rpact.org/reference/getComparison.md)
for the class-specific rules and reference values.

Shared profiles use the same exact comparison as
[`summary.TrialComparison()`](https://docs.rpact.org/reference/summary.TrialComparison.md),
applied to the selected harmonized or original columns. The summary's
`harmonizeColumnNames` argument can be set independently of this
data-frame option. Filtering precedes rounding, readable labels and
transposition. Common scalar parameters are returned once; common stage
or situation profiles retain their coordinates. Reporting modes render
HTML in a horizontally scrollable container or LaTeX longtables split
into column blocks with repeated identifier columns. LaTeX output
requires the suggested rmarkdown package and registers the longtable
dependency through knitr metadata. Other knitting targets use ordinary
data-frame output. Reporting does not modify the comparison or its
contained objects.
