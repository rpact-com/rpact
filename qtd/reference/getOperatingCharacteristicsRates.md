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
  intervalMethod = "qtd1",
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

- pi2:

  Single true control event probability in `[0, 1]`.

- directionUpper:

  Whether increasing treatment event rates are beneficial. If TRUE
  (default), the effect is `pi1 - pi2`; if FALSE, it is `pi2 - pi1`.

- intervalMethod:

  One of `"qtd1"` (default), `"farringtonManning"`,
  `"miettinenNurminen"`, or `"newcombe"`, as described in
  [`plot.TrialDesignPlanQuantitative()`](https://docs.rpact.org/qtd/reference/plot.TrialDesignPlanQuantitative.md).

- priorDist1, priorDist2:

  Bayesian RBesT betaMix priors for treatment and control, respectively.
  NULL selects mixbeta(c(1, 1/3, 1/3)) for each arm.

## Value

An `OperatingCharcteristicsRates` object supporting console and Markdown
printing, summaries and the QTD plot families. Its `probabilities` data
frame contains `category`, `probability`, `nControl`, `nTreatment`,
`alternative`, `pi1`, and `pi2`. `alternative` is the true risk
difference in the beneficial direction.

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
[OperatingCharcteristicsRates](https://docs.rpact.org/qtd/reference/OperatingCharcteristicsRates.md),
[summaryQuantitative](https://docs.rpact.org/qtd/reference/summaryQuantitative.md),
[`plot.OperatingCharcteristics()`](https://docs.rpact.org/qtd/reference/plot.OperatingCharcteristics.md)

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
#>   Interval method              : qtd1 
#> 
#> Output:
#>           Category  Probability Number of   Number of Alternatives   Assumed
#>                                  subjects    subjects              treatment
#>                                 (control) (treatment)                   rate
#>  1      Go (Fixed) 5.352327e-01        75          75        0.050     0.100
#>  2      Go (Fixed) 2.037757e-01        75          75        0.000     0.150
#>  3      Go (Fixed) 1.067077e-01        75          75       -0.025     0.175
#>  4      Go (Fixed) 5.023338e-02        75          75       -0.050     0.200
#>  5      Go (Fixed) 8.310520e-03        75          75       -0.100     0.250
#>  6              Go 3.132233e-01        75          75        0.050     0.100
#>  7              Go 3.043533e-01        75          75        0.000     0.150
#>  8              Go 2.267809e-01        75          75       -0.025     0.175
#>  9              Go 1.469166e-01        75          75       -0.050     0.200
#> 10              Go 4.383561e-02        75          75       -0.100     0.250
#> 11 Indeterminate 1 1.383506e-01        75          75        0.050     0.100
#> 12 Indeterminate 1 3.913004e-01        75          75        0.000     0.150
#> 13 Indeterminate 1 4.711368e-01        75          75       -0.025     0.175
#> 14 Indeterminate 1 4.822145e-01        75          75       -0.050     0.200
#> 15 Indeterminate 1 3.433772e-01        75          75       -0.100     0.250
#> 16 Indeterminate 2 6.447415e-08        75          75        0.050     0.100
#> 17 Indeterminate 2 2.235870e-09        75          75        0.000     0.150
#> 18 Indeterminate 2 3.442681e-10        75          75       -0.025     0.175
#> 19 Indeterminate 2 4.756762e-11        75          75       -0.050     0.200
#> 20 Indeterminate 2 6.683973e-13        75          75       -0.100     0.250
#> 21            Stop 1.232006e-02        75          75        0.050     0.100
#> 22            Stop 8.432942e-02        75          75        0.000     0.150
#> 23            Stop 1.509382e-01        75          75       -0.025     0.175
#> 24            Stop 2.224109e-01        75          75       -0.050     0.200
#> 25            Stop 3.065523e-01        75          75       -0.100     0.250
#> 26    Stop (Fixed) 8.733303e-04        75          75        0.050     0.100
#> 27    Stop (Fixed) 1.624114e-02        75          75        0.000     0.150
#> 28    Stop (Fixed) 4.443645e-02        75          75       -0.025     0.175
#> 29    Stop (Fixed) 9.822453e-02        75          75       -0.050     0.200
#> 30    Stop (Fixed) 2.979244e-01        75          75       -0.100     0.250
#> 31      Go (Fixed) 5.947519e-01        75         150        0.050     0.100
#> 32      Go (Fixed) 1.987206e-01        75         150        0.000     0.150
#> 33      Go (Fixed) 9.205907e-02        75         150       -0.025     0.175
#> 34      Go (Fixed) 3.674273e-02        75         150       -0.050     0.200
#> 35      Go (Fixed) 3.869971e-03        75         150       -0.100     0.250
#> 36              Go 3.201810e-01        75         150        0.050     0.100
#> 37              Go 3.720240e-01        75         150        0.000     0.150
#> 38              Go 2.716497e-01        75         150       -0.025     0.175
#> 39              Go 1.625763e-01        75         150       -0.050     0.200
#> 40              Go 3.558971e-02        75         150       -0.100     0.250
#> 41 Indeterminate 1 7.488036e-02        75         150        0.050     0.100
#> 42 Indeterminate 1 3.309465e-01        75         150        0.000     0.150
#> 43 Indeterminate 1 4.311718e-01        75         150       -0.025     0.175
#> 44 Indeterminate 1 4.466464e-01        75         150       -0.050     0.200
#> 45 Indeterminate 1 2.770091e-01        75         150       -0.100     0.250
#> 46 Indeterminate 2 9.126242e-09        75         150        0.050     0.100
#> 47 Indeterminate 2 1.591020e-11        75         150        0.000     0.150
#> 48 Indeterminate 2 4.410220e-13        75         150       -0.025     0.175
#> 49 Indeterminate 2 9.714064e-15        75         150       -0.050     0.200
#> 50 Indeterminate 2 2.480298e-18        75         150       -0.100     0.250
#> 51            Stop 9.867110e-03        75         150        0.050     0.100
#> 52            Stop 8.685674e-02        75         150        0.000     0.150
#> 53            Stop 1.672263e-01        75         150       -0.025     0.175
#> 54            Stop 2.579502e-01        75         150       -0.050     0.200
#> 55            Stop 3.414298e-01        75         150       -0.100     0.250
#> 56    Stop (Fixed) 3.196266e-04        75         150        0.050     0.100
#> 57    Stop (Fixed) 1.145223e-02        75         150        0.000     0.150
#> 58    Stop (Fixed) 3.789310e-02        75         150       -0.025     0.175
#> 59    Stop (Fixed) 9.608442e-02        75         150       -0.050     0.200
#> 60    Stop (Fixed) 3.421014e-01        75         150       -0.100     0.250
#> 
#>         Assumed
#>    control rate
#>  1         0.15
#>  2         0.15
#>  3         0.15
#>  4         0.15
#>  5         0.15
#>  6         0.15
#>  7         0.15
#>  8         0.15
#>  9         0.15
#> 10         0.15
#> 11         0.15
#> 12         0.15
#> 13         0.15
#> 14         0.15
#> 15         0.15
#> 16         0.15
#> 17         0.15
#> 18         0.15
#> 19         0.15
#> 20         0.15
#> 21         0.15
#> 22         0.15
#> 23         0.15
#> 24         0.15
#> 25         0.15
#> 26         0.15
#> 27         0.15
#> 28         0.15
#> 29         0.15
#> 30         0.15
#> 31         0.15
#> 32         0.15
#> 33         0.15
#> 34         0.15
#> 35         0.15
#> 36         0.15
#> 37         0.15
#> 38         0.15
#> 39         0.15
#> 40         0.15
#> 41         0.15
#> 42         0.15
#> 43         0.15
#> 44         0.15
#> 45         0.15
#> 46         0.15
#> 47         0.15
#> 48         0.15
#> 49         0.15
#> 50         0.15
#> 51         0.15
#> 52         0.15
#> 53         0.15
#> 54         0.15
#> 55         0.15
#> 56         0.15
#> 57         0.15
#> 58         0.15
#> 59         0.15
#> 60         0.15
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
#> * *Interval method*: qtd1 
#> 
#> *Output*
#> 
#> | | Category| Probability| Number of subjects (control)| Number of subjects (treatment)| Alternatives| Assumed treatment rate| Assumed control rate|
#> | -----| -----| -----| -----| -----| -----| -----| -----|
#> | 1| Go (Fixed)     | 5.352327e-01   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 2| Go (Fixed)     | 2.037757e-01   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 3| Go (Fixed)     | 1.067077e-01   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 4| Go (Fixed)     | 5.023338e-02   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 5| Go (Fixed)     | 8.310520e-03   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 6| Go             | 3.132233e-01   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 7| Go             | 3.043533e-01   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 8| Go             | 2.267809e-01   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 9| Go             | 1.469166e-01   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 10| Go             | 4.383561e-02   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 11| Indeterminate 1| 1.383506e-01   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 12| Indeterminate 1| 3.913004e-01   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 13| Indeterminate 1| 4.711368e-01   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 14| Indeterminate 1| 4.822145e-01   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 15| Indeterminate 1| 3.433772e-01   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 16| Indeterminate 2| 6.447415e-08   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 17| Indeterminate 2| 2.235870e-09   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 18| Indeterminate 2| 3.442681e-10   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 19| Indeterminate 2| 4.756762e-11   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 20| Indeterminate 2| 6.683973e-13   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 21| Stop           | 1.232006e-02   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 22| Stop           | 8.432942e-02   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 23| Stop           | 1.509382e-01   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 24| Stop           | 2.224109e-01   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 25| Stop           | 3.065523e-01   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 26| Stop (Fixed)   | 8.733303e-04   | 75             | 75             | 0.050          | 0.100          | 0.15           |
#> | 27| Stop (Fixed)   | 1.624114e-02   | 75             | 75             | 0.000          | 0.150          | 0.15           |
#> | 28| Stop (Fixed)   | 4.443645e-02   | 75             | 75             | -0.025         | 0.175          | 0.15           |
#> | 29| Stop (Fixed)   | 9.822453e-02   | 75             | 75             | -0.050         | 0.200          | 0.15           |
#> | 30| Stop (Fixed)   | 2.979244e-01   | 75             | 75             | -0.100         | 0.250          | 0.15           |
#> | 31| Go (Fixed)     | 5.947519e-01   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 32| Go (Fixed)     | 1.987206e-01   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 33| Go (Fixed)     | 9.205907e-02   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 34| Go (Fixed)     | 3.674273e-02   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 35| Go (Fixed)     | 3.869971e-03   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#> | 36| Go             | 3.201810e-01   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 37| Go             | 3.720240e-01   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 38| Go             | 2.716497e-01   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 39| Go             | 1.625763e-01   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 40| Go             | 3.558971e-02   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#> | 41| Indeterminate 1| 7.488036e-02   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 42| Indeterminate 1| 3.309465e-01   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 43| Indeterminate 1| 4.311718e-01   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 44| Indeterminate 1| 4.466464e-01   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 45| Indeterminate 1| 2.770091e-01   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#> | 46| Indeterminate 2| 9.126242e-09   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 47| Indeterminate 2| 1.591020e-11   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 48| Indeterminate 2| 4.410220e-13   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 49| Indeterminate 2| 9.714064e-15   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 50| Indeterminate 2| 2.480298e-18   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#> | 51| Stop           | 9.867110e-03   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 52| Stop           | 8.685674e-02   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 53| Stop           | 1.672263e-01   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 54| Stop           | 2.579502e-01   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 55| Stop           | 3.414298e-01   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#> | 56| Stop (Fixed)   | 3.196266e-04   | 75             | 150            | 0.050          | 0.100          | 0.15           |
#> | 57| Stop (Fixed)   | 1.145223e-02   | 75             | 150            | 0.000          | 0.150          | 0.15           |
#> | 58| Stop (Fixed)   | 3.789310e-02   | 75             | 150            | -0.025         | 0.175          | 0.15           |
#> | 59| Stop (Fixed)   | 9.608442e-02   | 75             | 150            | -0.050         | 0.200          | 0.15           |
#> | 60| Stop (Fixed)   | 3.421014e-01   | 75             | 150            | -0.100         | 0.250          | 0.15           |
#>  
```
