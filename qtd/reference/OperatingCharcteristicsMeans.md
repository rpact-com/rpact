# Operating Characteristics for Comparing Means

Stores fixed-design quantitative decision probabilities for means.
Obtain instances through
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md).

## Details

Inherits sample sizes, probabilities, priors and controlMean from
OperatingCharcteristics. The rates priors priorDist1 and priorDist2
remain NULL.

## Inherited fields

- `probabilities`:

  Calculated data frame of decision-category probabilities. Frequentist
  results store six categories; Bayesian results store Go, Indeterminate
  1, Indeterminate 2 and Stop. Within each sample-size scenario,
  categories form blocks retaining supplied effect order and repeats.
  Probabilities are on the scale 0 to 1.

- `priorDist`:

  Supplied normal-mixture effect prior (RBesT normMix) or a two-element
  list of control and treatment mean priors, in that order. NULL for
  frequentist results and rates results. Default Bayesian effect prior
  is mixnorm(c(1, 0, 100)).

- `priorDist1`:

  Supplied RBesT betaMix prior for the treatment event probability. NULL
  for frequentist results and means results. Default Bayesian prior is
  mixbeta(c(1, 1/3, 1/3)).

- `priorDist2`:

  Supplied RBesT betaMix prior for the control event probability. NULL
  for frequentist results and means results. Default Bayesian prior is
  mixbeta(c(1, 1/3, 1/3)).

- `controlMean`:

  Single supplied true control mean when separate Bayesian mean priors
  are used. The true treatment mean is controlMean plus alternative.
  NULL for an effect prior, frequentist results and rates results.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md),
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md)

## Super classes

[`FieldSet`](https://docs.rpact.org/qtd/reference/FieldSet.md) -\>
[`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
-\>
[`OperatingCharcteristics`](https://docs.rpact.org/qtd/reference/OperatingCharcteristics.md)
-\> `OperatingCharcteristicsMeans`

## Public fields

- `normalApproximation`:

  TRUE for the normal sampling model used by the frequentist means
  calculation. For Bayesian means the model has known standard
  deviations and this field does not select a t approximation. Is a
  single logical value.

- `groups`:

  Number of independent treatment groups, fixed at 2 (control and
  treatment). Is a single numeric value representing a whole number.

- `alternative`:

  Supplied finite numeric vector of true treatment-minus-control mean
  differences. Order and repeated entries are retained for every
  sample-size scenario.

- `stDev`:

  Supplied known standard deviation for means calculations. A positive
  scalar shared by both arms, or for Bayesian calculations a vector of
  two positive values ordered control, treatment. Bayesian OC plot data
  use the root mean square as the scalar SD facet label and retain both
  arm-specific values in four-category data.

## Methods

### Public methods

- [`OperatingCharcteristicsMeans$new()`](#method-OperatingCharcteristicsMeans-initialize)

- [`OperatingCharcteristicsMeans$show()`](#method-OperatingCharcteristicsMeans-show)

- [`OperatingCharcteristicsMeans$.show()`](#method-OperatingCharcteristicsMeans-.show)

- [`OperatingCharcteristicsMeans$.toString()`](#method-OperatingCharcteristicsMeans-.toString)

- [`OperatingCharcteristicsMeans$clone()`](#method-OperatingCharcteristicsMeans-clone)

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

### `OperatingCharcteristicsMeans$new()`

Initializes means OC assumptions and probability output.

#### Usage

    OperatingCharcteristicsMeans$new(
      ...,
      design,
      numberOfSubjects,
      normalApproximation = TRUE,
      groups = 2L,
      alternative = seq(0, 1, 0.2),
      stDev = 1,
      probabilities = NA_real_
    )

#### Arguments

- `...`:

  Reserved for ParameterSet compatibility.

- `design`:

  Originating quantitative rule.

- `numberOfSubjects`:

  Control and treatment sample sizes.

- `normalApproximation`:

  Normal sampling model flag.

- `groups`:

  Number of groups.

- `alternative`:

  True effect vector.

- `stDev`:

  Known standard deviations.

- `probabilities`:

  Category-block probability table.

------------------------------------------------------------------------

### `OperatingCharcteristicsMeans$show()`

Prints the result.

#### Usage

    OperatingCharcteristicsMeans$show(showType = 1, digits = NA_integer_)

#### Arguments

- `showType`:

  Output mode.

- `digits`:

  Display precision.

------------------------------------------------------------------------

### `OperatingCharcteristicsMeans$.show()`

Renders the result.

#### Usage

    OperatingCharcteristicsMeans$.show(
      showType = 1,
      digits = NA_integer_,
      consoleOutputEnabled = TRUE
    )

#### Arguments

- `showType`:

  Output mode.

- `digits`:

  Display precision.

- `consoleOutputEnabled`:

  Print or collect output.

------------------------------------------------------------------------

### `OperatingCharcteristicsMeans$.toString()`

Returns the result description.

#### Usage

    OperatingCharcteristicsMeans$.toString(startWithUpperCase = FALSE)

#### Arguments

- `startWithUpperCase`:

  Capitalize the description.

------------------------------------------------------------------------

### `OperatingCharcteristicsMeans$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OperatingCharcteristicsMeans$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
