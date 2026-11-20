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

  Finite vector of true treatment-minus-control effects. Optional names
  must be unique and non-empty. They label summary rows and appear in
  the `scenarioName` column of the probability table and plot data.

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

An OperatingCharacteristicsMeans object with the rule, assumptions and
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
#>     Category  Probability Number of   Number of Alternatives  Standard
#>                            subjects    subjects              deviation
#>                           (control) (treatment)                       
#> 1 Go (Fixed) 8.322067e-06        20          30          0.0         1
#> 2 Go (Fixed) 1.513943e-04        20          30          0.2         1
#> 3 Go (Fixed) 1.749695e-03        20          30          0.4         1
#> 4 Go (Fixed) 1.296489e-02        20          30          0.6         1
#> 5 Go (Fixed) 6.246053e-02        20          30          0.8         1
#> 6 Go (Fixed) 2.000000e-01        20          30          1.0         1
#> 
#> Preview limited to 6 of 36 rows.
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
#>          Category Probability Number of   Number of Alternatives  Standard
#>                                subjects    subjects              deviation
#>                               (control) (treatment)                       
#> 1              Go  0.01453361        20          30            0         1
#> 2              Go  0.89999587        20          30            1         1
#> 3              Go  0.99999896        20          30            2         1
#> 4 Indeterminate 1  0.00000000        20          30            0         1
#> 5 Indeterminate 1  0.00000000        20          30            1         1
#> 6 Indeterminate 1  0.00000000        20          30            2         1
#> 
#>    Standard    Standard
#>   deviation   deviation
#>   (control) (treatment)
#> 1         1           1
#> 2         1           1
#> 3         1           1
#> 4         1           1
#> 5         1           1
#> 6         1           1
#> 
#> Preview limited to 6 of 12 rows.
#> 
```
