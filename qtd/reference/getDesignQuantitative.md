# Create a Frequentist or Bayesian Quantitative Decision Rule

Constructs a fixed-design quantitative decision rule. Its class selects
the calculation in the common operating-characteristics functions.

## Usage

``` r
getDesignQuantitative(
  targetValue,
  lowerReferenceValue = 0,
  upperPercentile = 0.9,
  lowerPercentile = 0.2,
  bayesian = FALSE,
  useSignificanceAndTarget = FALSE,
  alpha = 0.05,
  go = NULL,
  stop = NULL,
  indeterminate1 = NULL,
  percentileMapping = "quantile"
)
```

## Arguments

- targetValue:

  Single target effect on the beneficial-effect scale.

- lowerReferenceValue:

  Single lower reference effect; default zero.

- upperPercentile:

  Upper percentile in (0, 1); default 0.9.

- lowerPercentile:

  Lower percentile in (0, upperPercentile); default 0.2.

- bayesian:

  Whether to construct a Bayesian rule. Explicit criteria imply TRUE
  when this argument is omitted.

- useSignificanceAndTarget:

  Construct a dual significance/target rule.

- alpha:

  Two-sided frequentist significance level or Bayesian posterior tail
  probability in (0, 0.5); default 0.05. These are different
  conventions.

- go, stop, indeterminate1:

  Criteria from
  [`getQuantitativeDecisionThresholds()`](https://docs.rpact.org/qtd/reference/getQuantitativeDecisionThresholds.md).
  Supply all three without reference values, percentiles or shortcut
  options.

- percentileMapping:

  Bayesian interpretation: "quantile" (default) or
  "posteriorProbability". Stored in the design for reproducibility.

## Value

A TrialDesignPlanQuantitative or TrialDesignPlanQuantitativeBayes,
inheriting from TrialDesignPlanQuantitativeBase and ParameterSet.

## Details

For mapping "quantile", Go requires P(effect \> LRV) \>
1-lowerPercentile and P(effect \> TV) \> 1-upperPercentile. Stop
requires P(effect \<= LRV) \> lowerPercentile and P(effect \<= TV) \>
upperPercentile. This follows by comparing posterior quantiles with the
references, and agrees with normal CI decisions in the diffuse-prior
limit. It differs from the exploratory prototype mapping.

Mapping "posteriorProbability" is a different rule: the beneficial-tail
Go thresholds are upperPercentile at LRV and lowerPercentile at TV, and
Stop uses their complements. For both mappings, Indeterminate 1 fails
the LRV criterion and passes TV. Indeterminate 2 is the complement of
Go, Stop and Indeterminate 1, including exact probability-threshold
ties. Comparisons are strict, following RBesT; ties can have positive
sampling probability for rates.

The significance/target shortcut uses lowerPercentile = alpha/2 and
upperPercentile = 0.5 for frequentist rules. Bayesian rules require
posterior probabilities 1-alpha at LRV and 0.5 at TV, independently of
percentileMapping. The shortcut stores the canonical quantile mapping
for reproducibility. The two shortcuts are not claimed to be equivalent:
for example 95 percent two-sided confidence corresponds to a 97.5
percent one-sided tail probability.

All explicit categories must be provably pairwise incompatible by
monotonicity of a continuous posterior CDF. Potential overlaps are
rejected; a particular prior might permit additional disjoint rules that
this conservative validation cannot certify. No category precedence is
introduced. When defining custom intermediate criteria, explicitly
exclude futility, as in the RBesT examples. Reference effects always use
the beneficial direction, also for decreasing rates.

## See also

[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md),
<https://opensource.nibr.com/RBesT/reference/decision2S.html>

## Examples

``` r
rule <- getDesignQuantitative(targetValue = 1)
bayes <- getDesignQuantitative(targetValue = 1, bayesian = TRUE)
dual <- getDesignQuantitative(targetValue = 1, bayesian = TRUE,
    useSignificanceAndTarget = TRUE)
```
