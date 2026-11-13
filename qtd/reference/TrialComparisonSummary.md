# Summary of a Trial Comparison

A snapshot of comparison profiles and display settings, created by
[`summary.TrialComparison()`](https://docs.rpact.org/qtd/reference/summary.TrialComparison.md)
or [`compare()`](https://docs.rpact.org/qtd/reference/compare.md).

## Details

Inherits from `SummaryFactory` and `ParameterSet`. Each resolved summary
input is classified centrally as a default or user-defined parameter.
[`getObjectRCode()`](https://docs.rpact.org/qtd/reference/getObjectRCode.md)
includes only user-defined inputs unless
`includeDefaultParameters = TRUE`. Settings resolved from R options or
environment variables are retained when they differ from package
defaults. `showAll` affects rendering without replacing the stored
limits or text mode. The underlying comparison is retained by reference,
whereas profiles and display labels are snapshots. Recompute the summary
after changing objects.

## Fields

- `parameterInfo`:

  A data frame of derived parameter metadata, exact profiles and object
  group IDs, with one row per comparison parameter. Profiles retain
  their stage and situation coordinates.

- `digits`:

  Number of digits supplied for display rounding; NA leaves values
  unrounded. Is a single numeric value representing a whole number.

- `maxParameters`:

  Maximum number of differing parameters displayed; defaults to 12.
  Ignored for display when showAll is TRUE. Is a single numeric value
  representing a whole number.

- `maxProfiles`:

  Maximum number of distinct profiles displayed per parameter; defaults
  to 3. Ignored for display when showAll is TRUE. Is a single numeric
  value representing a whole number.

- `maxObjects`:

  Maximum number of object names or numbers displayed per profile;
  defaults to 6. Ignored for display when showAll is TRUE. Is a single
  numeric value representing a whole number.

- `maxValues`:

  Maximum number of values displayed per profile preview; defaults to 4.
  Ignored for display when showAll is TRUE. Is a single numeric value
  representing a whole number.

- `harmonizeColumnNames`:

  Whether the snapshot uses harmonized comparison column names; defaults
  to TRUE. Is a single logical value.

- `textMode`:

  Resolved console text handling: wrap, truncate or full. Defaults to
  wrap; showAll disables truncation without changing this stored input.
  Is a single character value.

- `width`:

  Resolved console line width in characters, at least 20. Taken from the
  supplied value, comparison setting or the R width option (fallback
  80). Is a single numeric value representing a whole number.

- `showAll`:

  Whether all profiles, objects and values are displayed without the max
  limits or text truncation; defaults to FALSE. Stored limits remain
  unchanged. Is a single logical value.

- `object`:

  The underlying TrialComparison object, retained by reference.

- `title`:

  Derived comparison summary title. Is a single character value.

- `header`:

  Inherited optional summary header; NULL for comparison summaries.

- `summaryItems`:

  Inherited list of summary items; empty for comparison summaries, which
  render parameterInfo directly.

- `intervalFormat`:

  Inherited interval format string, initially `[%s; %s]`; comparison
  profile rendering does not use this field. Is a single character
  value.

- `justify`:

  Inherited alignment setting resolved from the summary configuration,
  with default right. Is a single character value.

- `output`:

  Selected summary sections: all, title, overview or body; defaults to
  all. Is a single character value.

- `markdown`:

  Inherited Markdown output flag, initially FALSE. Is a single logical
  value.

## See also

[`getComparison()`](https://docs.rpact.org/qtd/reference/getComparison.md),
[`summary.TrialComparison()`](https://docs.rpact.org/qtd/reference/summary.TrialComparison.md),
[`getObjectRCode()`](https://docs.rpact.org/qtd/reference/getObjectRCode.md)
