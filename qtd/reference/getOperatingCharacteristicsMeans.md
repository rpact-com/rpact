# Operating Characteristics for Quantitative Decisions Comparing Means

Computes frequentist or Bayesian decision probabilities, selected by the
class of the quantitative design rule.

## Usage

``` r
getOperatingCharacteristicsMeans(
  design,
  ...,
  numberOfSubjects,
  normalApproximation = TRUE,
  groups = 2,
  alternative = seq(0, 1, 0.2),
  stDev = 1,
  priorDist = NULL,
  controlMean = NULL
)
```

## Arguments

- design:

  A rule from
  [`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).

- ...:

  Additional arguments are checked for unknown names.

- numberOfSubjects:

  Positive integer matrix with columns control, treatment; each row is a
  sample-size scenario.

- normalApproximation:

  Frequentist option, currently TRUE. Omit for Bayes.

- groups:

  Number of groups; currently two.

- alternative:

  Finite vector of true treatment-minus-control effects.

- stDev:

  Positive common SD or, for Bayes, two known SDs ordered control,
  treatment. Defaults to one. Sampling uncertainty of SDs is not
  modeled.

- priorDist:

  Bayesian RBesT normMix prior on the effect or a list of two normMix
  priors ordered control, treatment. NULL selects mixnorm(c(1, 0, 100)).
  Only identity-link normal mixtures are supported.

- controlMean:

  True control mean, required only with separate group priors.

## Value

An OperatingCharcteristicsMeans object with the rule, assumptions and
category-block probability data frame. Bayesian data additionally retain
stDevControl and stDevTreatment; the scalar stDev facet is their root
mean square.

## Details

Bayesian calculations require RBesT. Single-component normal priors use
the analytic conjugate normal calculation; normal mixtures use RBesT
oc1S (effect prior) or oc2S (group priors). The effect-prior sampling
variance is stDevControl^2/nControl + stDevTreatment^2/nTreatment. For
group priors, the true treatment mean is controlMean + alternative.

RBesT mixture OCs use numerical decision-boundary searches and
integration. Agreement with independent references is therefore
approximate, typically around 1e-4 with the package defaults; exact
threshold ties and near-boundary values require care. No t-based or
unknown-variance Bayesian model is implied. Bayesian probabilities are
conditional sampling OCs at fixed true effects, not prior-predictive
probabilities. All explicit categories are validated for exclusion
before the residual category is calculated.

There are six frequentist categories, or four Bayesian categories: Go,
Indeterminate 1, Indeterminate 2 and Stop. Three-category plots merge
the two intermediate categories. Sample-size scenarios, effect order and
repeats are retained. Bayesian priors and controlMean are stored for
summaries, sensitivity recalculation and
[`getObjectRCode()`](https://docs.rpact.org/qtd/reference/getObjectRCode.md).

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md)

## Examples

``` r
rule <- getDesignQuantitative(1)
getOperatingCharacteristicsMeans(rule, numberOfSubjects = matrix(c(20, 30), ncol = 2))
#> Parameters and output of operating-characteristics for comparing means:
#> 
#> User defined parameters:
#>   Number of subjects           : 20, 30 
#> 
#> Derived from user defined parameters: not available
#> 
#> Default parameters:
#>   Normal approximation         : TRUE 
#>   Treatment groups             : 2 
#>   Alternatives                 : 0.0, 0.2, 0.4, 0.6, 0.8, 1.0 
#>   Standard deviation           : 1 
#> 
#> Output:
#>           Category  Probability Number of   Number of Alternatives  Standard
#>                                  subjects    subjects              deviation
#>                                 (control) (treatment)                       
#>  1      Go (Fixed) 8.322067e-06        20          30          0.0         1
#>  2      Go (Fixed) 1.513943e-04        20          30          0.2         1
#>  3      Go (Fixed) 1.749695e-03        20          30          0.4         1
#>  4      Go (Fixed) 1.296489e-02        20          30          0.6         1
#>  5      Go (Fixed) 6.246053e-02        20          30          0.8         1
#>  6      Go (Fixed) 2.000000e-01        20          30          1.0         1
#>  7              Go 1.452616e-02        20          30          0.0         1
#>  8              Go 6.799626e-02        20          30          0.2         1
#>  9              Go 2.110021e-01        20          30          0.4         1
#> 10              Go 4.455844e-01        20          30          0.6         1
#> 11              Go 6.595187e-01        20          30          0.8         1
#> 12              Go 7.000000e-01        20          30          1.0         1
#> 13 Indeterminate 1 0.000000e+00        20          30          0.0         1
#> 14 Indeterminate 1 0.000000e+00        20          30          0.2         1
#> 15 Indeterminate 1 0.000000e+00        20          30          0.4         1
#> 16 Indeterminate 1 0.000000e+00        20          30          0.6         1
#> 17 Indeterminate 1 0.000000e+00        20          30          0.8         1
#> 18 Indeterminate 1 0.000000e+00        20          30          1.0         1
#> 19 Indeterminate 2 1.854655e-01        20          30          0.0         1
#> 20 Indeterminate 2 3.727077e-01        20          30          0.2         1
#> 21 Indeterminate 2 4.940341e-01        20          30          0.4         1
#> 22 Indeterminate 2 4.333774e-01        20          30          0.6         1
#> 23 Indeterminate 2 2.511963e-01        20          30          0.8         1
#> 24 Indeterminate 2 9.563539e-02        20          30          1.0         1
#> 25            Stop 7.000000e-01        20          30          0.0         1
#> 26            Stop 5.349749e-01        20          30          0.2         1
#> 27            Stop 2.893897e-01        20          30          0.4         1
#> 28            Stop 1.076836e-01        20          30          0.6         1
#> 29            Stop 2.679919e-02        20          30          0.8         1
#> 30            Stop 4.363575e-03        20          30          1.0         1
#> 31    Stop (Fixed) 1.000000e-01        20          30          0.0         1
#> 32    Stop (Fixed) 2.416974e-02        20          30          0.2         1
#> 33    Stop (Fixed) 3.824396e-03        20          30          0.4         1
#> 34    Stop (Fixed) 3.896947e-04        20          30          0.6         1
#> 35    Stop (Fixed) 2.530058e-05        20          30          0.8         1
#> 36    Stop (Fixed) 1.039173e-06        20          30          1.0         1
#> 
if (requireNamespace("RBesT", quietly = TRUE)) {
    bayes <- getDesignQuantitative(1, bayesian = TRUE)
    getOperatingCharacteristicsMeans(bayes,
        numberOfSubjects = matrix(c(20, 30), ncol = 2), alternative = c(0, 1, 2))
}
#> Parameters and output of operating-characteristics for comparing means:
#> 
#> User defined parameters:
#>   Number of subjects           : 20, 30 
#>   Prior distribution [1]       : 1 
#>   Prior distribution [2]       : 0 
#>   Prior distribution [3]       : 100 
#>   Alternatives                 : 0, 1, 2 
#> 
#> Derived from user defined parameters: not available
#> 
#> Default parameters:
#>   Normal approximation         : TRUE 
#>   Treatment groups             : 2 
#>   Standard deviation           : 1 
#> 
#> Output:
#>           Category  Probability Number of   Number of Alternatives  Standard
#>                                  subjects    subjects              deviation
#>                                 (control) (treatment)                       
#>  1              Go 1.453361e-02        20          30            0         1
#>  2              Go 8.999959e-01        20          30            1         1
#>  3              Go 9.999990e-01        20          30            2         1
#>  4 Indeterminate 1 0.000000e+00        20          30            0         1
#>  5 Indeterminate 1 0.000000e+00        20          30            1         1
#>  6 Indeterminate 1 0.000000e+00        20          30            2         1
#>  7 Indeterminate 2 1.854654e-01        20          30            0         1
#>  8 Indeterminate 2 9.563947e-02        20          30            1         1
#>  9 Indeterminate 2 1.038717e-06        20          30            2         1
#> 10            Stop 8.000010e-01        20          30            0         1
#> 11            Stop 4.364660e-03        20          30            1         1
#> 12            Stop 5.767456e-10        20          30            2         1
#> 
#>     Standard    Standard
#>    deviation   deviation
#>    (control) (treatment)
#>  1         1           1
#>  2         1           1
#>  3         1           1
#>  4         1           1
#>  5         1           1
#>  6         1           1
#>  7         1           1
#>  8         1           1
#>  9         1           1
#> 10         1           1
#> 11         1           1
#> 12         1           1
#> 
```
