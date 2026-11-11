# Posterior Probability Criteria for Quantitative Decisions

Internal ParameterSet holding a conjunction of posterior probability
criteria. Obtain instances through
[`getQuantitativeDecisionThresholds()`](https://docs.rpact.org/qtd/reference/getQuantitativeDecisionThresholds.md).

## Details

All criteria must hold, using strict probability comparisons as in
RBesT. Thresholds refer to the effect in the beneficial direction.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)

## Super classes

[`FieldSet`](https://docs.rpact.org/qtd/reference/FieldSet.md) -\>
[`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
-\> `QuantitativeDecisionThresholds`

## Public fields

- `quantiles`:

  Supplied finite numeric vector of posterior effect thresholds on the
  beneficial-effect scale, one per criterion. All criteria are combined
  with logical AND.

- `probabilities`:

  Supplied numeric vector of strict posterior probability thresholds in
  (0, 1), one per quantile in the same order.

- `lowerTail`:

  Supplied logical vector, expanded from a scalar if needed, one per
  quantile. TRUE specifies P(effect \<= quantile) \> probability and
  FALSE specifies P(effect \> quantile) \> probability.

## Methods

### Public methods

- [`QuantitativeDecisionThresholds$new()`](#method-QuantitativeDecisionThresholds-initialize)

- [`QuantitativeDecisionThresholds$.toString()`](#method-QuantitativeDecisionThresholds-.toString)

- [`QuantitativeDecisionThresholds$clone()`](#method-QuantitativeDecisionThresholds-clone)

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
- [`ParameterSet$.show()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.show)
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

### `QuantitativeDecisionThresholds$new()`

Stores validated posterior criteria.

#### Usage

    QuantitativeDecisionThresholds$new(quantiles, probabilities, lowerTail)

#### Arguments

- `quantiles`:

  Effect thresholds.

- `probabilities`:

  Posterior probability thresholds.

- `lowerTail`:

  Directions of the probability inequalities.

------------------------------------------------------------------------

### `QuantitativeDecisionThresholds$.toString()`

Returns the object description.

#### Usage

    QuantitativeDecisionThresholds$.toString(startWithUpperCase = FALSE)

#### Arguments

- `startWithUpperCase`:

  Capitalize the description.

------------------------------------------------------------------------

### `QuantitativeDecisionThresholds$clone()`

The objects of this class are cloneable with this method.

#### Usage

    QuantitativeDecisionThresholds$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
