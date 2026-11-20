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
  showAll = NA,
  showDataType = FALSE,
  maxTableColumns = NA_integer_
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

- showDataType:

  If `TRUE`, include data type descriptions such as
  `integer, continuous` in parameter headings. Defaults to `FALSE`.

- maxTableColumns:

  Maximum result/profile columns per Markdown table block (default 4).
  Coordinate columns are additional. Also applies with `showAll = TRUE`.

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
comparison data are unchanged. Profile coordinates follow the
parameter's source and native dimensions. Repeated design settings on
simulation category rows are normalized only when the remaining
coordinates uniquely determine the exact values. Stage and group domains
of dimensioned parameters remain meaningful when their values are equal.
Uncertain or conflicting dependencies retain additional coordinates
rather than averaging or selecting an arbitrary value. Both
parameter-filter options of
[`as.data.frame.TrialComparison()`](https://docs.rpact.org/qtd/reference/as.data.frame.TrialComparison.md)
use these profiles; unfiltered data-frame rows remain unchanged for
plotting. This value normalization also applies when column-name
harmonization is disabled. Plotting a summary retains its selected
column-name mode unless custom data are supplied.

Parameter captions are resolved separately for each original object,
including its design when a parameter comes from that design.
`parameterInfo$paramName` retains the column names selected by
`harmonizeColumnNames`; `paramCaption` contains the display label and
`paramCaptions` contains captions in object order (NA for absent
parameters). These captions are captured with the summary. Distinct
object-specific captions are shown together and identified alongside the
corresponding object labels; they do not change exact value-profile
grouping. Unnamed entries are labelled `Simulation` when all objects are
simulation results, `Design` when all objects are trial designs, and
`Result` otherwise, followed by their collection position. Explicit
names take precedence. Full type descriptions remain in the overview.
The overview and these labels are captured with the summary. The legend
explains exact profile comparison and mentions NA only when displayed
profiles contain missing values. Titles distinguish designs, sample size
calculations, power calculations, and power simulations. Mixed
comparisons use a design plan comparison title; empty comparisons retain
the general trial comparison title. Markdown output places coordinates
in rows and displayed profiles in columns. Scalar profiles use one-row
tables. At most `maxTableColumns` profile columns (default 4) appear per
block, with coordinate columns repeated in each block. This layout limit
also applies with `showAll = TRUE`. Long profile labels are listed above
the tables with short references. Incompatible coordinate structures use
separate tables. Missing and omitted coordinates use the same markers as
console tables. Data limits apply independently of table layout.

Console output uses a single comparison line for numeric, character or
logical scalar values when at most four objects are compared, all fit
within the profile/object limits, captions agree and the complete line
fits within `width`. Otherwise scalars retain the labelled profile
display. Profiles with compatible coordinate columns are aligned by
their exact coordinates and displayed side by side. Coordinates constant
across the complete parameter are listed once, except redundant constant
category types, categories and situation identifiers, which are omitted.
Varying categories use an Arm or Comparison column caption where
appropriate. A scalar missing profile is displayed as an `NA` column
alongside the coordinates of the other profiles; non-missing scalars are
not expanded. If the table exceeds `width`, values are listed vertically
per coordinate. Profiles sharing at least one coordinate can use a joint
table even if their remaining axes differ. A dash marks an inapplicable
coordinate; values are not broadcast to categories absent from their
source profile. Profiles without shared coordinates retain separate
displays. `<absent>` denotes a coordinate absent from a profile, whereas
`<omitted>` denotes a value outside that profile's `maxValues` preview.
Neither marker represents a stored `NA`. Display choices do not change
exact profile grouping.

A `NULL` display argument reads its setting through
`.getEnvironmentVariable()`. Explicit arguments take precedence over
environment variables, which take precedence over R options and
defaults. The four default limits are 12 parameters, 3 profiles, 6
objects and 4 values. R options use the prefix
`rpact.comparison.summary.` followed by the argument name:
`maxParameters`, `maxProfiles`, `maxObjects`, `maxValues`,
`maxTableColumns`, `textMode`, `width`, `showAll` or `showDataType`. The
corresponding environment variables are
`RPACT_COMPARISON_SUMMARY_MAX_PARAMETERS`,
`RPACT_COMPARISON_SUMMARY_MAX_PROFILES`,
`RPACT_COMPARISON_SUMMARY_MAX_OBJECTS`,
`RPACT_COMPARISON_SUMMARY_MAX_VALUES`,
`RPACT_COMPARISON_SUMMARY_TEXT_MODE`, `RPACT_COMPARISON_SUMMARY_WIDTH`,
`RPACT_COMPARISON_SUMMARY_SHOW_ALL` and
`RPACT_COMPARISON_SUMMARY_SHOW_DATA_TYPE`. Settings are captured when
the summary is created. For a complete display use
`summary(x, showAll = TRUE)`; for a compact display use
`textMode = "truncate"`.
