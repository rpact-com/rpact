# Operating Characteristics for Quantitative Decisions

Internal ParameterSet container shared by means and rates results.
Obtain concrete instances through the endpoint-specific OC functions.

## Details

The originating rule is stored in the internal .design field. Priors and
the control mean are NULL when not applicable. Bayesian results store
four exclusive categories; frequentist results store six. Sampling
probabilities are conditional on the supplied true effect or response
rates; they are not predictive assurance.

## See also

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)

## Super classes

[`FieldSet`](https://docs.rpact.org/qtd/reference/FieldSet.md) -\>
[`ParameterSet`](https://docs.rpact.org/qtd/reference/ParameterSet.md)
-\> `OperatingCharcteristics`

## Public fields

- `numberOfSubjects`:

  Supplied sample-size scenarios. Rows correspond to scenarios; the two
  columns contain the positive integer numbers of control and treatment
  subjects, in that order. Repeated scenarios are retained. Is a numeric
  matrix.

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

## Methods

### Public methods

- [`OperatingCharcteristics$new()`](#method-OperatingCharcteristics-initialize)

- [`OperatingCharcteristics$clone()`](#method-OperatingCharcteristics-clone)

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
- [`ParameterSet$.toString()`](https://docs.rpact.org/qtd/reference/ParameterSet.html#method-.toString)
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

### `OperatingCharcteristics$new()`

Initializes the shared quantitative operating-characteristics fields.

#### Usage

    OperatingCharcteristics$new(design, numberOfSubjects, probabilities = NA_real_)

#### Arguments

- `design`:

  The originating quantitative decision rule.

- `numberOfSubjects`:

  Matrix of control and treatment sample sizes.

- `probabilities`:

  Computed probability data frame.

------------------------------------------------------------------------

### `OperatingCharcteristics$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OperatingCharcteristics$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
