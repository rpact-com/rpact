# Summarize Differences Between Trial Planning Objects

Summarizes differences between the object profiles in a trial
comparison.

The `TrialComparisonSummary` method refreshes a summary from its
underlying comparison, retaining stored display settings unless
explicitly overridden. This also supports `%vs%` chains followed by
`summary(...)`.

## Usage

``` r
# S3 method for class 'TrialComparison'
summary(
  object,
  ...,
  digits = NA_integer_,
  maxParameters = NA_integer_,
  maxProfiles = NA_integer_,
  maxObjects = NA_integer_,
  maxValues = NA_integer_,
  output = c("all", "title", "overview", "body"),
  harmonizeColumnNames = TRUE,
  textMode = c("wrap", "truncate", "full"),
  width = NA_integer_,
  showAll = NA
)

# S3 method for class 'TrialComparisonSummary'
summary(object, ...)
```

## Arguments

- object:

  A `TrialComparison` or an existing `TrialComparisonSummary`.

- ...:

  Further arguments. Currently unused.

- digits:

  Display rounding only; exact values determine differences.

- maxParameters:

  Maximum number of differing parameters shown, across all sections.
  Increase this to inspect further differences.

- maxProfiles:

  Maximum number of distinct profiles shown per parameter.

- maxObjects:

  Maximum object numbers/names listed per profile.

- maxValues:

  Maximum profile values shown per preview.

- output:

  One of "all", "title", "overview", or "body".

- harmonizeColumnNames:

  If `TRUE` (default), compare the harmonized columns described in
  [`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md).
  If `FALSE`, compare the original parameter names, as in
  `as.data.frame(object, harmonizeColumnNames = FALSE)`.

- textMode:

  Console text handling: `"wrap"` (default) wraps complete text,
  `"truncate"` shows compact previews with an explicit shortening
  notice, and `"full"` writes complete text without automatic wrapping.
  Markdown always retains complete text in its profile tables.

- width:

  Console line width, at least 20 characters. Defaults to
  `getOption("width", 80L)`. Long unbroken words can exceed this width.

- showAll:

  If `TRUE`, lift all four `max*` limits and disable text truncation.
  Wrapping remains active when `textMode = "wrap"`. Defaults to `FALSE`.

## Value

A `TrialComparisonSummary` inheriting from `SummaryFactory`. Its
`parameterInfo` contains metadata, full exact profiles and object group
IDs. The displayed summary is a snapshot; call
[`summary()`](https://rdrr.io/r/base/summary.html) again after changing
an underlying object. The `harmonizeColumnNames` attribute of
`parameterInfo` records the selected naming mode for plotting and R-code
reconstruction. Input roles take precedence when roles differ across the
compared object types.

## Details

Constant numeric vector cells compare equal to their scalar value
regardless of vector length. Nonconstant vectors, missing values, and
their positions remain part of the exact comparison. The stored
comparison data are unchanged. This value normalization also applies
when column-name harmonization is disabled. Plotting a summary retains
its selected column-name mode unless custom data are supplied.

Parameter captions are resolved separately for each original object,
including its design when a parameter comes from that design.
`parameterInfo$paramName` retains the column names selected by
`harmonizeColumnNames`; `paramCaption` contains the display label and
`paramCaptions` contains captions in object order (NA for absent
parameters). These captions are captured with the summary. Distinct
object-specific captions are shown together and identified alongside the
corresponding object labels; they do not change exact value-profile
grouping. Unnamed entries are labelled by their calculation or design
type and collection position. The overview and these type labels are
captured with the summary. The legend explains exact profile comparison
and mentions NA only when displayed profiles contain missing values.
Titles distinguish designs, sample size calculations, power
calculations, and power simulations. Mixed comparisons use a design plan
comparison title; empty comparisons retain the general trial comparison
title. Markdown output presents each differing parameter as a table with
one row per displayed profile and columns labelled by stage and
situation. Scalar profiles use a Value column. Blank cells indicate
coordinates absent from a profile; explicit missing values are shown as
NA. The display limits also apply to these tables, with omitted values
and profiles indicated explicitly.

A `NULL` display argument reads its setting through
`.getEnvironmentVariable()`. Explicit arguments take precedence over
environment variables, which take precedence over R options and
defaults. The four default limits are 12 parameters, 3 profiles, 6
objects and 4 values. R options use the prefix
`rpact.comparison.summary.` followed by the argument name:
`maxParameters`, `maxProfiles`, `maxObjects`, `maxValues`, `textMode`,
`width` or `showAll`. The corresponding environment variables are
`RPACT_COMPARISON_SUMMARY_MAX_PARAMETERS`,
`RPACT_COMPARISON_SUMMARY_MAX_PROFILES`,
`RPACT_COMPARISON_SUMMARY_MAX_OBJECTS`,
`RPACT_COMPARISON_SUMMARY_MAX_VALUES`,
`RPACT_COMPARISON_SUMMARY_TEXT_MODE`, `RPACT_COMPARISON_SUMMARY_WIDTH`
and `RPACT_COMPARISON_SUMMARY_SHOW_ALL`. Settings are captured when the
summary is created. For a complete display use
`summary(x, showAll = TRUE)`; for a compact display use
`textMode = "truncate"`.
