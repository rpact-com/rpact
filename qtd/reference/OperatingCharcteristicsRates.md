# Operating Characteristics for Quantitative Decisions Comparing Rates

Stores exact binomial decision-category probabilities for two
independent groups. Obtain instances with
[`getOperatingCharcteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharcteristicsRates.md).

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

## See also

[`getOperatingCharcteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharcteristicsRates.md),
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)

## Super classes

[`FieldSet`](https://docs.rpact.org/qtd/reference/FieldSet.md) -\>
[`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
-\>
[`OperatingCharcteristics`](https://docs.rpact.org/qtd/reference/OperatingCharcteristics.md)
-\> `OperatingCharcteristicsRates`

## Public fields

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

## Active bindings

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

## Methods

### Public methods

- [`OperatingCharcteristicsRates$new()`](#method-OperatingCharcteristicsRates-initialize)

- [`OperatingCharcteristicsRates$show()`](#method-OperatingCharcteristicsRates-show)

- [`OperatingCharcteristicsRates$.show()`](#method-OperatingCharcteristicsRates-.show)

- [`OperatingCharcteristicsRates$.toString()`](#method-OperatingCharcteristicsRates-.toString)

- [`OperatingCharcteristicsRates$clone()`](#method-OperatingCharcteristicsRates-clone)

Inherited methods

- [`FieldSet$.cat()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.cat)
- [`FieldSet$.catMarkdownText()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.catMarkdownText)
- [`FieldSet$.getFieldNames()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.getFieldNames)
- [`FieldSet$.getFields()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.getFields)
- [`FieldSet$.getVisibleFieldNames()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.getVisibleFieldNames)
- [`FieldSet$.resetCat()`](https://docs.rpact.org/qtd/reference/FieldSet.html#method-.resetCat)
- [`ParameterSet$.containsMultidimensionalParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.containsMultidimensionalParameters)
- [`ParameterSet$.extractParameterNameAndValue()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.extractParameterNameAndValue)
- [`ParameterSet$.formatDataFrameParametersAsCharacter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.formatDataFrameParametersAsCharacter)
- [`ParameterSet$.getDataFrameColumnCaption()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getDataFrameColumnCaption)
- [`ParameterSet$.getDefaultParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getDefaultParameters)
- [`ParameterSet$.getDerivedParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getDerivedParameters)
- [`ParameterSet$.getGeneratedParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getGeneratedParameters)
- [`ParameterSet$.getInputParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getInputParameters)
- [`ParameterSet$.getMultidimensionalNumberOfStages()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getMultidimensionalNumberOfStages)
- [`ParameterSet$.getNChar()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getNChar)
- [`ParameterSet$.getNumberOfRows()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getNumberOfRows)
- [`ParameterSet$.getParameterType()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getParameterType)
- [`ParameterSet$.getParameterValueIfUserDefinedOrDefault()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getParameterValueIfUserDefinedOrDefault)
- [`ParameterSet$.getParametersOfOneGroup()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getParametersOfOneGroup)
- [`ParameterSet$.getParametersToShow()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getParametersToShow)
- [`ParameterSet$.getSubListByNames()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getSubListByNames)
- [`ParameterSet$.getUndefinedParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getUndefinedParameters)
- [`ParameterSet$.getUnidimensionalNumberOfStages()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getUnidimensionalNumberOfStages)
- [`ParameterSet$.getUserDefinedParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getUserDefinedParameters)
- [`ParameterSet$.getVariedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getVariedParameter)
- [`ParameterSet$.getVisibleFieldNamesOrdered()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.getVisibleFieldNamesOrdered)
- [`ParameterSet$.initParameterTypes()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.initParameterTypes)
- [`ParameterSet$.printAsDataFrame()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.printAsDataFrame)
- [`ParameterSet$.setParameterType()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.setParameterType)
- [`ParameterSet$.showAllParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showAllParameters)
- [`ParameterSet$.showParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParameter)
- [`ParameterSet$.showParameterFormatted()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParameterFormatted)
- [`ParameterSet$.showParameterSingle()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParameterSingle)
- [`ParameterSet$.showParameterType()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParameterType)
- [`ParameterSet$.showParameterTypeDescription()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParameterTypeDescription)
- [`ParameterSet$.showParametersOfOneGroup()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showParametersOfOneGroup)
- [`ParameterSet$.showUnknownParameters()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.showUnknownParameters)
- [`ParameterSet$isApplicableParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isApplicableParameter)
- [`ParameterSet$isDefaultParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isDefaultParameter)
- [`ParameterSet$isDefinedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isDefinedParameter)
- [`ParameterSet$isDerivedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isDerivedParameter)
- [`ParameterSet$isGeneratedOrDerivedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isGeneratedOrDerivedParameter)
- [`ParameterSet$isGeneratedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isGeneratedParameter)
- [`ParameterSet$isNotApplicableParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isNotApplicableParameter)
- [`ParameterSet$isNotGeneratedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isNotGeneratedParameter)
- [`ParameterSet$isNotUserDefinedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isNotUserDefinedParameter)
- [`ParameterSet$isUndefinedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isUndefinedParameter)
- [`ParameterSet$isUserDefinedOrDerivedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isUserDefinedOrDerivedParameter)
- [`ParameterSet$isUserDefinedParameter()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-isUserDefinedParameter)

------------------------------------------------------------------------

### `OperatingCharcteristicsRates$new()`

Initializes a rates result. Use getOperatingCharcteristicsRates() to
validate inputs and calculate probabilities.

#### Usage

    OperatingCharcteristicsRates$new(
      design,
      numberOfSubjects,
      pi1,
      pi2,
      directionUpper = TRUE,
      intervalMethod = "qtd1",
      probabilities = NA_real_
    )

#### Arguments

- `design`:

  The originating quantitative decision rule.

- `numberOfSubjects`:

  Matrix of control and treatment sample sizes.

- `pi1`:

  Vector of treatment event probabilities.

- `pi2`:

  Single control event probability.

- `directionUpper`:

  Whether larger treatment event rates are beneficial.

- `intervalMethod`:

  Risk-difference interval construction.

- `probabilities`:

  Computed probability data frame.

------------------------------------------------------------------------

### `OperatingCharcteristicsRates$show()`

Prints the result to the console.

#### Usage

    OperatingCharcteristicsRates$show(showType = 1, digits = NA_integer_)

#### Arguments

- `showType`:

  Output mode passed to ParameterSet.

- `digits`:

  Optional display precision.

------------------------------------------------------------------------

### `OperatingCharcteristicsRates$.show()`

Internal rendering method used by standard console and Markdown output.

#### Usage

    OperatingCharcteristicsRates$.show(
      showType = 1,
      digits = NA_integer_,
      consoleOutputEnabled = TRUE
    )

#### Arguments

- `showType`:

  Output mode passed to ParameterSet.

- `digits`:

  Optional display precision.

- `consoleOutputEnabled`:

  Whether to print or collect the output.

------------------------------------------------------------------------

### `OperatingCharcteristicsRates$.toString()`

Returns the result description used in output headings.

#### Usage

    OperatingCharcteristicsRates$.toString(startWithUpperCase = FALSE)

#### Arguments

- `startWithUpperCase`:

  Whether to capitalize the first character.

------------------------------------------------------------------------

### `OperatingCharcteristicsRates$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OperatingCharcteristicsRates$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
