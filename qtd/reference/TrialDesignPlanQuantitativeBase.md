# Common Quantitative Trial Decision Rule

Internal ParameterSet base for frequentist and Bayesian quantitative
decision rules. Obtain instances of its concrete subclasses with
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).

## Details

The reference values and percentiles are optional for explicit Bayesian
rules. The concrete subclasses are TrialDesignPlanQuantitative and
TrialDesignPlanQuantitativeBayes. ParameterSet supplies shared parameter
and output management; internal dot-prefixed fields are implementation
details.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)

## Super classes

[`FieldSet`](https://docs.rpact.org/qtd/reference/FieldSet.md) -\>
[`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
-\> `TrialDesignPlanQuantitativeBase`

## Public fields

- `targetValue`:

  Optional target effect on the beneficial-effect scale. A supplied
  finite scalar for reference-value rules; NA for explicit Bayesian
  criteria.

- `lowerReferenceValue`:

  Optional lower reference effect on the beneficial-effect scale. A
  supplied finite scalar for reference-value rules; NA for explicit
  Bayesian criteria.

- `upperPercentile`:

  Upper percentile supplied for a reference-value rule, strictly between
  lowerPercentile and 1. NA for explicit Bayesian criteria. For Bayesian
  rules its interpretation is controlled by percentileMapping.

- `lowerPercentile`:

  Lower percentile supplied for a reference-value rule, strictly between
  0 and upperPercentile. NA for explicit Bayesian criteria. For Bayesian
  rules its interpretation is controlled by percentileMapping.

## Methods

### Public methods

- [`TrialDesignPlanQuantitativeBase$new()`](#method-TrialDesignPlanQuantitativeBase-initialize)

- [`TrialDesignPlanQuantitativeBase$show()`](#method-TrialDesignPlanQuantitativeBase-show)

- [`TrialDesignPlanQuantitativeBase$.show()`](#method-TrialDesignPlanQuantitativeBase-.show)

- [`TrialDesignPlanQuantitativeBase$.toString()`](#method-TrialDesignPlanQuantitativeBase-.toString)

- [`TrialDesignPlanQuantitativeBase$clone()`](#method-TrialDesignPlanQuantitativeBase-clone)

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

### `TrialDesignPlanQuantitativeBase$new()`

Initializes shared reference parameters.

#### Usage

    TrialDesignPlanQuantitativeBase$new(
      ...,
      targetValue = NA_real_,
      lowerReferenceValue = NA_real_,
      upperPercentile = 0.9,
      lowerPercentile = 0.2
    )

#### Arguments

- `...`:

  Reserved for ParameterSet compatibility.

- `targetValue, lowerReferenceValue`:

  Reference effects or NA.

- `upperPercentile, lowerPercentile`:

  Reference percentiles or NA.

------------------------------------------------------------------------

### `TrialDesignPlanQuantitativeBase$show()`

Prints the quantitative rule.

#### Usage

    TrialDesignPlanQuantitativeBase$show(showType = 1, digits = NA_integer_)

#### Arguments

- `showType`:

  Output mode.

- `digits`:

  Display precision.

------------------------------------------------------------------------

### `TrialDesignPlanQuantitativeBase$.show()`

Renders the quantitative rule.

#### Usage

    TrialDesignPlanQuantitativeBase$.show(
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

### `TrialDesignPlanQuantitativeBase$.toString()`

Returns the object description.

#### Usage

    TrialDesignPlanQuantitativeBase$.toString(startWithUpperCase = FALSE)

#### Arguments

- `startWithUpperCase`:

  Capitalize the description.

------------------------------------------------------------------------

### `TrialDesignPlanQuantitativeBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TrialDesignPlanQuantitativeBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
