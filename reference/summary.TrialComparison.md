# Summarize Differences Between Trial Planning Objects

Summarizes differences between the object profiles in a trial
comparison.

## Usage

``` r
# S3 method for class 'TrialComparison'
summary(
  object,
  ...,
  digits = NA_integer_,
  maxParameters = 12L,
  maxProfiles = 3L,
  maxObjects = 6L,
  maxValues = 4L,
  output = c("all", "title", "overview", "body")
)
```

## Arguments

- object:

  A `TrialComparison`.

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

## Value

A `TrialComparisonSummary` inheriting from `SummaryFactory`. Its
`parameterInfo` contains metadata, full exact profiles and object group
IDs. The displayed summary is a snapshot; call
[`summary()`](https://rdrr.io/r/base/summary.html) again after changing
an underlying object. Input roles take precedence when roles differ
across the compared object types.

## Details

Parameter captions are resolved separately for each original object,
including its design when a parameter comes from that design.
`parameterInfo$paramName` retains the original column names;
`paramCaption` contains the display label and `paramCaptions` contains
captions in object order (NA for absent parameters). These captions are
captured with the summary. Distinct object-specific captions are shown
together and identified alongside the corresponding object labels; they
do not change exact value-profile grouping. Unnamed entries are labelled
by their calculation or design type and collection position. The
overview and these type labels are captured with the summary. The legend
explains exact profile comparison and mentions NA only when displayed
profiles contain missing values. Titles distinguish designs, sample size
calculations, power calculations, and power simulations. Mixed
comparisons use a design plan comparison title; empty comparisons retain
the general trial comparison title.
