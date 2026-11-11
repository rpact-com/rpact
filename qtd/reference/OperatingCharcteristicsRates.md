# Operating Characteristics for Quantitative Decisions Comparing Rates

Stores exact binomial decision-category probabilities for two
independent groups. Obtain instances with
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md).

Initializes a rates result. Use getOperatingCharacteristicsRates() to
validate inputs and calculate probabilities.

Prints the result to the console.

Internal rendering method used by standard console and Markdown output.

Returns the result description used in output headings.

## Arguments

- design:

  The originating quantitative decision rule.

- numberOfSubjects:

  Matrix of control and treatment sample sizes.

- pi1:

  Vector of treatment event probabilities.

- pi2:

  Single control event probability.

- directionUpper:

  Whether larger treatment event rates are beneficial.

- intervalMethod:

  Risk-difference interval construction.

- probabilities:

  Computed probability data frame.

- showType:

  Output mode passed to ParameterSet.

- digits:

  Optional display precision.

- consoleOutputEnabled:

  Whether to print or collect the output.

- startWithUpperCase:

  Whether to capitalize the first character.

## Details

Inherits the sample-size matrix and probability table from
`OperatingCharcteristics`. The originating quantitative decision rule is
stored internally. Each sample-size scenario contains all six categories
and the supplied treatment rates in their original order, including
repeats. Console and Markdown printing use the standard ParameterSet
mechanism. Use [`summary()`](https://rdrr.io/r/base/summary.html) for
decision-rule assumptions and probability tables, and
[`plot.OperatingCharcteristics()`](https://docs.rpact.org/qtd/reference/plot.OperatingCharcteristics.md)
for the shared QTD plot families.

## Fields

- `numberOfSubjects`:

  Supplied sample-size scenarios. Rows correspond to scenarios; the two
  columns contain the positive integer numbers of control and treatment
  subjects, in that order. Repeated scenarios are retained. Is a numeric
  matrix.

- `probabilities`:

  Computed data frame with columns category (character), probability
  (numeric), nControl and nTreatment (sample sizes), alternative
  (beneficial true risk difference), pi1 (treatment event probability)
  and pi2 (control event probability). Each sample-size scenario has six
  category blocks; each block contains the supplied treatment rates in
  their original order, including repeats. The six probabilities sum to
  one for each sample-size and rate scenario.

- `groups`:

  Number of independent treatment groups, fixed at 2 (control and
  treatment). Is a single numeric value representing a whole number.

- `pi1`:

  Supplied numeric vector of true treatment event probabilities between
  0 and 1, inclusive. Its order and repeated entries are retained. Every
  rate is evaluated for every supplied sample-size scenario against the
  single control probability pi2.

- `pi2`:

  The assumed probability in the reference group for two-group designs.
  Is a single numeric value between 0 and 1.

- `directionUpper`:

  Whether larger treatment event probabilities are beneficial. TRUE
  (default) defines the effect as pi1 minus pi2; FALSE defines it as pi2
  minus pi1. Reference values use the same beneficial-effect scale. Is a
  single logical value.

- `intervalMethod`:

  Interval construction for observed risk differences: qtd1 (default),
  farringtonManning, miettinenNurminen or newcombe. The category
  probabilities are obtained by exact binomial summation for the
  selected interval construction. Is a single character value.

- `alternative`:

  Derived true risk differences in the beneficial direction, one per
  supplied treatment rate: pi1 minus pi2 when directionUpper is TRUE,
  otherwise pi2 minus pi1. Entries are between -1 and 1, inclusive, in
  the same order as pi1, and are not additional user-supplied
  assumptions. Is a numeric vector.

## See also

[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md),
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
