# Operating Characteristics for Quantitative Decisions Comparing Rates

Calculates probabilities of all six quantitative decision categories by
exact summation over two independent binomial event counts.

## Usage

``` r
getOperatingCharacteristicsRates(
  design,
  ...,
  numberOfSubjects,
  pi1,
  pi2,
  directionUpper = TRUE,
  intervalMethod = "wilson",
  priorDist1 = NULL,
  priorDist2 = NULL
)
```

## Arguments

- design:

  A decision rule from
  [`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md).
  Reference values refer to the risk difference in the beneficial
  direction.

- ...:

  Additional arguments are checked for unknown names.

- numberOfSubjects:

  Numeric matrix with two columns, control followed by treatment, and at
  least one row. Each row is a sample-size scenario; all entries must be
  positive integers.

- pi1:

  Numeric vector of true treatment event probabilities in `[0, 1]`.
  Optional names must be unique and non-empty. They label summary rows
  and appear in the `scenarioName` column of the probability table and
  plot data.

- pi2:

  Single true control event probability in `[0, 1]`.

- directionUpper:

  Whether increasing treatment event rates are beneficial. If TRUE
  (default), the effect is `pi1 - pi2`; if FALSE, it is `pi2 - pi1`.

- intervalMethod:

  One of `"wilson"` (default), `"farringtonManning"`,
  `"miettinenNurminen"`, or `"newcombe"`, as described in
  [`plot.TrialDesignPlanQuantitative()`](https://docs.rpact.org/qtd/reference/plot.TrialDesignPlanQuantitative.md).

- priorDist1, priorDist2:

  Bayesian RBesT betaMix priors for treatment and control, respectively.
  NULL selects mixbeta(c(1, 1/3, 1/3)) for each arm.

## Value

An `OperatingCharacteristicsRates` object supporting console and
Markdown printing, summaries and the QTD plot families. Its
`probabilities` data frame contains `category`, `probability`,
`nControl`, `nTreatment`, `alternative`, `pi1`, and `pi2`. `alternative`
is the true risk difference in the beneficial direction.

## Details

Bayesian rules use RBesT postmix and decision2S on the complete binomial
count grid, followed by exact binomial probability summation. This
avoids truncation by eps and mixed-tail boundary sentinel edge cases
observed with oc2S in RBesT 1.12.0. Strict posterior comparisons follow
RBesT. The four Bayesian categories are Go, Indeterminate 1,
Indeterminate 2 and Stop; exact probability-threshold ties belong to
Indeterminate 2. Omit intervalMethod for Bayesian rules. Priors are
stored for reproducible output and recalculation. Only identity-link
beta mixtures are supported. The count-grid limit applies to Bayesian
rules as well; evaluation of large grids can be expensive.

Decision boundaries use the same interval calculations and equality
rules as the binary decision map. The QTD1 method reproduces the
original program, including Wilson limits rounded to four decimal places
in its variance construction, and requires
`lowerPercentile <= 0.5 <= upperPercentile`. The other methods support
ordered percentiles anywhere in (0, 1). Exact summation describes the
sampling distribution of the counts; the interval methods themselves are
approximate frequentist constructions.

The six exclusive categories are Go (Fixed), Go, Indeterminate 1,
Indeterminate 2, Stop, and Stop (Fixed). Add the two Go, Indeterminate,
or Stop categories for the respective three-category probabilities. Each
scenario contains six blocks, with the supplied treatment rates in order
within each block. Repeated rates and sample-size scenarios are
retained. A count grid may contain at most one million cells per
sample-size scenario. Counts and probabilities are computed without
requiring ggplot2 or Java.

## See also

[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md),
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md),
[OperatingCharacteristicsRates](https://docs.rpact.org/qtd/reference/OperatingCharacteristicsRates.md),
[summaryQuantitative](https://docs.rpact.org/qtd/reference/summaryQuantitative.md),
[`plot.OperatingCharacteristics()`](https://docs.rpact.org/qtd/reference/plot.OperatingCharacteristics.md)

## Examples

``` r
# Binary example from the original QTD1 demonstration:
rule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
oc <- getOperatingCharacteristicsRates(rule,
    numberOfSubjects = matrix(c(75, 75, 75, 150), ncol = 2, byrow = TRUE),
    pi1 = c(0.1, 0.15, 0.175, 0.2, 0.25), pi2 = 0.15,
    directionUpper = FALSE)
print(oc)
#> Parameters and output of operating-characteristics for comparing rates:
#> 
#> User defined parameters:
#>   Number of subjects [1]       : 75, 75 
#>   Number of subjects [2]       : 75, 150 
#>   Assumed treatment rate       : 0.100, 0.150, 0.175, 0.200, 0.250 
#>   Assumed control rate         : 0.150 
#>   Direction upper              : FALSE 
#> 
#> Derived from user defined parameters:
#>   Alternatives                 : 0.050, 0.000, -0.025, -0.050, -0.100 
#> 
#> Default parameters:
#>   Treatment groups             : 2 
#>   Interval method              : wilson 
#> 
#> Output:
#>     Category Probability Number of   Number of Alternatives   Assumed
#>                           subjects    subjects              treatment
#>                          (control) (treatment)                   rate
#> 1 Go (Fixed)  0.53523266        75          75        0.050     0.100
#> 2 Go (Fixed)  0.20377574        75          75        0.000     0.150
#> 3 Go (Fixed)  0.10670767        75          75       -0.025     0.175
#> 4 Go (Fixed)  0.05023338        75          75       -0.050     0.200
#> 5 Go (Fixed)  0.00831052        75          75       -0.100     0.250
#> 6         Go  0.31322328        75          75        0.050     0.100
#> 
#>        Assumed
#>   control rate
#> 1         0.15
#> 2         0.15
#> 3         0.15
#> 4         0.15
#> 5         0.15
#> 6         0.15
#> 
#> Preview limited to 6 of 60 rows.
#> 
cat(print(oc, markdown = TRUE)[1], "\n")
#> *Parameters and output of operating-characteristics for comparing rates*
#> 
#> *User defined parameters*
#> 
#> * *Number of subjects [1]*: 75, 75 
#> * *Number of subjects [2]*: 75, 150 
#> * *Assumed treatment rate*: 0.100, 0.150, 0.175, 0.200, 0.250 
#> * *Assumed control rate*: 0.150 
#> * *Direction upper*: FALSE 
#> 
#> *Derived from user defined parameters*
#> 
#> * *Alternatives*: 0.050, 0.000, -0.025, -0.050, -0.100 
#> 
#> *Default parameters*
#> 
#> * *Treatment groups*: 2 
#> * *Interval method*: wilson 
#> 
#> *Output*
#> 
#> | | Category| Probability| Number of subjects (control)| Number of subjects (treatment)| Alternatives| Assumed treatment rate| Assumed control rate|
#> | -----| -----| -----| -----| -----| -----| -----| -----|
#> | 1| Go (Fixed)| 0.53523266| 75        | 75        | 0.050     | 0.100     | 0.15      |
#> | 2| Go (Fixed)| 0.20377574| 75        | 75        | 0.000     | 0.150     | 0.15      |
#> | 3| Go (Fixed)| 0.10670767| 75        | 75        | -0.025    | 0.175     | 0.15      |
#> | 4| Go (Fixed)| 0.05023338| 75        | 75        | -0.050    | 0.200     | 0.15      |
#> | 5| Go (Fixed)| 0.00831052| 75        | 75        | -0.100    | 0.250     | 0.15      |
#> | 6| Go        | 0.31322328| 75        | 75        | 0.050     | 0.100     | 0.15      |
#> 
#> Preview limited to 6 of 60 rows.
#>  
```
