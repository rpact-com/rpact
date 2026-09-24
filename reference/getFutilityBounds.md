# Get Futility Bounds

This function converts futility bounds between different scales such as
z-value, p-value, conditional power, predictive power, reverse
conditional power, Wald effect estimate, and design-specific treatment
effect.

## Usage

``` r
getFutilityBounds(
  sourceValue,
  ...,
  sourceScale = c("zValue", "pValue", "conditionalPower", "condPowerAtObserved",
    "predictivePower", "reverseCondPower", "effectEstimate", "treatmentEffect"),
  targetScale = c("zValue", "pValue", "conditionalPower", "condPowerAtObserved",
    "predictivePower", "reverseCondPower", "effectEstimate", "treatmentEffect"),
  design = NULL,
  directionUpper = NA,
  theta = NA_real_,
  information = NA_real_,
  stage = NA_integer_,
  naAllowed = FALSE
)
```

## Arguments

- sourceValue:

  A numeric vector or matrix representing the futility bounds in the
  source scale. Alternatively, a trial design plan, simulation results,
  or a `FisherInformation` object returned by
  [`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
  can be piped in; see Details.

- ...:

  Ensures that all arguments (starting from the "...") are to be named
  and that a warning will be displayed if unknown arguments are passed.

- sourceScale:

  Character. The scale of the input futility bounds. Must be one of
  `"zValue"`, `"pValue"`, `"conditionalPower"`, `"condPowerAtObserved"`,
  `"predictivePower"`, `"reverseCondPower"`, `"effectEstimate"`, or
  `"treatmentEffect"`.

- targetScale:

  Character. The scale to which the futility bounds should be converted.
  Must be one of `"zValue"`, `"pValue"`, `"conditionalPower"`,
  `"condPowerAtObserved"`, `"predictivePower"`, `"reverseCondPower"`,
  `"effectEstimate"`, or `"treatmentEffect"`.

- design:

  The trial design. Required if either the `sourceScale` or
  `targetScale` is `"reverseCondPower"` or if the conversion involves
  conditional or predictive power in a group sequential or Fisher
  design; these conversions require a supported one-sided two-stage
  design. Conversion from or to `"treatmentEffect"` requires a trial
  design plan, rather than only its contained trial design, because
  endpoint-specific planning parameters are needed.

- directionUpper:

  Logical. Specifies the direction of the alternative, only applicable
  for one-sided testing; default is `TRUE` which means that larger
  values of the test statistics yield smaller p-values.

- theta:

  Numeric. The assumed treatment effect under the alternative hypothesis
  on the unstandardized analysis scale. For example, in a survival
  design this is specified on the log hazard-ratio scale.

- information:

  Numeric vector of length 1 or 2 specifying the information used in the
  conversion. In general, `information[1]` is the cumulative information
  available at the analysis to which the futility bound refers, whereas
  `information[2]` is the additional information planned after that
  analysis. The exact requirements depend on `sourceScale` and
  `targetScale`. A `FisherInformation` object can also be supplied; its
  `type` field must be consistent with the requested conversion; it must
  represent a single planning situation when used as this argument. See
  Details.

- stage:

  Integer vector identifying the analysis stages represented by numeric
  `sourceValue` when converting from or to the `"treatmentEffect"`
  scale. It is inferred automatically from piped `FutilityBounds` or
  `FisherInformation` objects. If omitted otherwise, all interim stages
  are used.

- naAllowed:

  Logical. Indicates if `NA` `sourceValue` are permitted. Default is
  `FALSE`.

## Value

A numeric vector or matrix of class `FutilityBounds` representing the
futility bounds in the target scale, or `NULL` if the conversion is not
implemented or yields no result. For pipe-based input with multiple
stages and situations, rows represent interim stages and columns
represent planning situations. The corresponding labels and calculation
context are retained as attributes and displayed by
[`print.FutilityBounds()`](https://docs.rpact.org/reference/print.FutilityBounds.md).

## Details

If the `sourceScale` and `targetScale` are the same, the function
returns the input `sourceValue` without modification. Otherwise, the
function is designed to convert between the specified scales.

**Available scales**

Each scale expresses the same interim futility threshold from a
different perspective:

- `"zValue"`:

  The standardized interim test statistic. For `directionUpper = TRUE`,
  larger values favor the alternative; `directionUpper = FALSE` reverses
  this direction. Conversion between this scale and `"pValue"` does not
  require Fisher information.

- `"pValue"`:

  The one-sided p-value corresponding to the z-value and the selected
  direction. Smaller values indicate stronger evidence in favor of the
  alternative. Conversion between this scale and `"zValue"` does not
  require Fisher information.

- `"effectEstimate"`:

  The unstandardized, null-centered effect estimate \\\widehat{\delta}\\
  on the Wald analysis scale, related to the z-value by \\z =
  \widehat{\delta}\sqrt{I_1}\\. Depending on the endpoint, this can be a
  mean or rate difference, or an effect on a transformed scale such as
  the log hazard-ratio scale. Adding the null value or applying a
  back-transformation may be necessary to obtain the endpoint's usual
  presentation scale. Fisher information is evaluated under the planning
  assumptions.

- `"treatmentEffect"`:

  The endpoint's natural treatment-effect scale, calculated with the
  same endpoint- and test-specific transformation used for
  `futilityBoundsEffectScale` in the design plan. For two-group binary
  rates this inverts the Farrington–Manning score statistic; for count
  data it uses the validated negative-binomial inversion with a
  candidate-dependent variance estimate; for survival data it returns
  the hazard-ratio scale; and other endpoints retain their existing
  design-specific transformations. This scale requires a trial design
  plan, either supplied as `design` or retained in a piped result.
  Cumulative Fisher information is calculated internally where the
  endpoint-specific transformation needs it, or taken from a piped
  `FisherInformation` object after validation. The Count Data inversion
  additionally uses the stage-specific sample size, exposure,
  recruitment, allocation, and overdispersion from the design plan.

- `"conditionalPower"`:

  The probability of rejecting the null hypothesis at the final
  analysis, conditional on the interim result and assuming the
  user-specified treatment effect `theta` for the future observations.
  It requires `theta` and the additional second-stage information.

- `"condPowerAtObserved"`:

  Conditional power calculated by using the interim effect estimate in
  place of a separately assumed effect. It treats the observed estimate
  as fixed when projecting the future data and, in the supported
  two-stage setting, depends on the information through the ratio of
  second-stage to first-stage information.

- `"predictivePower"`:

  The Bayesian predictive probability of rejecting the null hypothesis
  at the final analysis under a flat (improper) prior for the treatment
  effect. Unlike conditional power at the observed effect, it integrates
  uncertainty about that effect. In the supported two-stage setting, it
  also depends only on the information ratio.

- `"reverseCondPower"`:

  Reverse conditional power (also called reverse stochastic
  curtailment): the conditional probability that the interim result
  would be at least as unfavorable as the observed result, given that
  the final combined test statistic is at its critical boundary. It is
  independent of an assumed treatment effect and, for the supported
  inverse normal or group sequential setting, coincides with the
  predictive power based on a flat prior. Conversion between this scale
  and `"zValue"` or `"pValue"` uses the design's information rates, but
  does not require endpoint-specific Fisher information.

The four power-based scales take values between `0` and `1`. Their
values describe a futility threshold at an interim analysis; they should
not be confused with the unconditional power of the trial design.

**Piping design plans or Fisher information into getFutilityBounds**

A trial design plan or simulation results object can be supplied
directly. If the requested conversion needs Fisher information,
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
is called internally with the required type: `"cumulative"` for an
effect-estimate conversion and `"stageWise"` for conversions involving
conditional or predictive power. It is not called for conversion from
the design's z-value bounds to `"zValue"`, `"pValue"`, or
`"reverseCondPower"`; the latter uses information rates from the design
itself. This provides the short form


    designPlan |>
        getFutilityBounds(targetScale = "condPowerAtObserved")

instead of explicitly inserting
`getFisherInformation(type = "stageWise")` into the pipe.

A complete result of
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
can be supplied as `sourceValue`, most conveniently with the base R
pipe. In this form, `getFutilityBounds()` obtains the design, its
z-value futility bounds, the requested stages, the information type, and
the planning-situation labels from the fields of the `FisherInformation`
object. If `targetScale` is omitted, the bounds are converted to
`"effectEstimate"`; another target scale can be requested explicitly. If
Fisher information is not needed for that scale, the object remains a
valid source of the design and its futility bounds, but its information
values are ignored and a warning is issued. The printed result then does
not claim that Fisher information was used. An exception is
`targetScale = "treatmentEffect"`: the cumulative information object is
passed to and validated by the endpoint-specific transformation instead
of being discarded. For Count Data, its originating design and requested
stages are reused without recalculating Fisher information; the actual
nonlinear inversion uses the corresponding design-specific sample sizes
and a variance estimate evaluated at each candidate rate ratio.

Conversion to `"treatmentEffect"` uses
`.getFutilityBoundsTreatmentEffectScale()` and therefore reproduces its
validated endpoint-specific values exactly (up to the eight-digit
rounding used for `futilityBoundsEffectScale`). This includes the
nonlinear Count Data calculation and reproduces the corresponding
design-plan field. Conversely, a result on this scale can be piped back
into `getFutilityBounds()`; its retained design plan and stage metadata
are used to recover the corresponding standardized bounds before
conversion to the requested target scale. For two-group rates the output
explicitly identifies the design-specific Farrington–Manning inversion.
The distinct `"effectEstimate"` scale continues to report the
null-centered Wald transformation based on cumulative Fisher information
under the planning assumptions.

The conversion is performed separately for every planning situation and
for every supplied interim analysis. Information for the final analysis
is used where a conversion requires future information, but no
final-stage futility bound is returned. Consequently, cumulative
information is appropriate for the default conversion to the
effect-estimate scale. Stage-wise information is required for
conversions involving conditional or predictive power. An informative
error is issued if its `type` field is incompatible with the requested
target scale. Conversions to conditional power, conditional power at the
observed effect, predictive power, or reverse conditional power remain
restricted to one-sided two-stage designs. For a design with more than
two stages, the conditional probability would additionally require a
precise definition of how all remaining analyses and their stopping
boundaries are handled. If a group sequential or inverse normal design
contains only the default futility bound, that bound represents the
absence of futility stopping. Its conversion to a conditional-power,
predictive-power, or reverse-conditional- power scale is therefore
returned as the exact boundary value `0`, without a numerical-range
warning. This applies in both directions of the alternative.

**Interpretation of information**

The elements of `information` have different interpretations:

- `information[1]` is the cumulative Fisher information underlying the
  test statistic or effect estimate at the analysis where the futility
  bound is evaluated. In a two-stage design this is also the information
  contributed by the first stage.

- `information[2]` is the additional, non-cumulative Fisher information
  to be collected after that analysis. In a two-stage design this is the
  information contributed by the second stage, not the cumulative
  information at the second analysis.

Consequently, if \\I_1\\ and \\I_2\\ denote the cumulative information
at the first and second analyses, respectively, specify
`information = c(I1, I2 - I1)`. A single value is used for both
elements.

**Information required by conversion type**

The required elements are determined by all scales involved in the
conversion:

- `"zValue"` and `"pValue"`:

  Conversions between these two scales do not require information.

- `"effectEstimate"`:

  Requires `information[1]`. The standardized statistic and effect
  estimate are related by \\z = \widehat{\theta}\sqrt{I_1}\\. Thus the
  cumulative information at the analysis represented by the z-value or
  effect estimate must be supplied.

- `"treatmentEffect"`:

  Does not use the separately supplied numeric `information` argument.
  Instead, it requires a trial design plan and applies the endpoint- and
  test-specific transformation used by `futilityBoundsEffectScale`.
  Cumulative information is calculated internally when needed.
  Alternatively, a piped `FisherInformation` object of type
  `"cumulative"` is validated and used. For two-group rates, the
  validated Farrington–Manning calculation is based directly on the
  planned group sizes and null-restricted rates rather than on a generic
  Wald-information substitution. For Count Data, the validated
  negative-binomial inversion evaluates the variance at each candidate
  rate ratio and uses the planned stage sample size, recruitment,
  exposure, allocation ratio, and overdispersion. A piped cumulative
  `FisherInformation` object supplies and validates the design/stage
  context, but its stored numeric values do not replace that
  candidate-dependent variance calculation.

- `"conditionalPower"`:

  Requires `information[2]` together with `theta`. Here `information[2]`
  is the additional information available for the future stage over
  which conditional power is calculated.

- `"condPowerAtObserved"`:

  Requires both elements. The current effect is estimated using the
  cumulative information `information[1]` and projected over the
  additional future information `information[2]`.

- `"predictivePower"`:

  Requires both elements. Predictive power combines uncertainty based on
  the cumulative information already observed with the additional
  information planned for the future stage.

- `"reverseCondPower"`:

  Does not require an explicit `information` value; the required
  information fractions are taken from the specified `design`.

If `condPowerAtObserved` or `predictivePower` is involved and no
complete information vector is supplied, the relative first- and
second-stage information can be derived from an eligible two-stage
`design` as
`c(design$informationRates[1], 1 - design$informationRates[1])`. This
normalization is sufficient for conversions that depend only on
information ratios. An explicitly supplied vector is needed when
absolute information is required, for example when conditional power is
calculated under a specified value of `theta`.

A warning is issued if a two-element vector contains an information
value not needed for the requested conversion. Set the unused element to
`NA`, or pass a single value when using the same value for both elements
is intended. A `FisherInformation` object returned by
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
has a `type` field. `getFutilityBounds()` verifies that it is
`"cumulative"` for an effect-estimate conversion or `"stageWise"` for a
conditional- or predictive-power conversion, and stops with an error if
the types do not match. Plain numeric input without type metadata
remains supported for backward compatibility. If a `FisherInformation`
object contains multiple planning situations, pipe it into
`getFutilityBounds()` as `sourceValue`; the conversion is then performed
separately for every situation. The `information` argument itself
accepts such an object only when it represents one situation.

**Obtaining information from a design plan**

Use
[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
to calculate cumulative information from a design plan or simulation
results object. For an analysis at stage `j`, its result can be passed
directly as `information[1]` when converting between an effect estimate
and a standardized statistic. To construct the information vector for a
two-stage conditional- or predictive-power conversion, calculate the
cumulative information at both analyses and use:


    informationStage1 <- as.numeric(getFisherInformation(designPlan, stage = 1))
    informationCumulative2 <- as.numeric(getFisherInformation(designPlan, stage = 2))
    information <- c(
        informationStage1,
        informationCumulative2 - informationStage1
    )

## See also

[`getFisherInformation()`](https://docs.rpact.org/reference/getFisherInformation.md)
for calculating the value of the `information` argument;
[`getDesignGroupSequential()`](https://docs.rpact.org/reference/getDesignGroupSequential.md),
[`getDesignInverseNormal()`](https://docs.rpact.org/reference/getDesignInverseNormal.md),
[`getDesignFisher()`](https://docs.rpact.org/reference/getDesignFisher.md)
for direct specification of futility bounds on different scales using
the argument `futilityBoundsScale`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with identical source and target scales
getFutilityBounds(
    sourceValue = c(0, 0.5),
    sourceScale = "zValue",
    targetScale = "zValue"
)

# Example with different scales
getFutilityBounds(
    design = getDesignGroupSequential(kMax = 2, typeOfDesign = "noEarlyEfficacy", alpha = 0.05),
    information = c(10, 10),
    sourceValue = 0.5,
    sourceScale = "condPowerAtObserved",
    targetScale = "pValue"
)

# Pipe all planning situations into an effect-estimate-scale conversion
getDesignGroupSequential(
    informationRates = c(0.2, 0.7, 1),
    futilityBounds = c(-0.5, 0)
) |>
    getSampleSizeRates() |>
    getFisherInformation() |>
    getFutilityBounds()

# Fisher information is calculated internally with type = "stageWise"
getDesignGroupSequential(kMax = 2, futilityBounds = 0.3) |>
    getSampleSizeRates() |>
    getFutilityBounds(targetScale = "condPowerAtObserved")

# Reproduce the design-specific futility bounds on the treatment-effect scale
treatmentEffectBounds <- getDesignGroupSequential(
    informationRates = c(0.2, 0.7, 1),
    futilityBounds = c(0.3, 0.2)
) |>
    getSampleSizeRates() |>
    getFutilityBounds(targetScale = "treatmentEffect")

# Use treatmentEffect as a source scale; sourceScale is inferred here
treatmentEffectBounds |>
    getFutilityBounds(targetScale = "pValue")
} # }
```
