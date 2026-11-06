# Summaries of Quantitative Trial Designs and Operating Characteristics

The standard [`summary()`](https://rdrr.io/r/base/summary.html) method
supports quantitative decision rules from
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
and results from
[`getOperatingCharcteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharcteristicsMeans.md)
and
[`getOperatingCharcteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharcteristicsRates.md).

## Value

A `SummaryFactory` object supporting console and Markdown output.

## Details

Use `summary(rule)` for the target value, lower reference value and
decision percentiles. Use `summary(oc)` for the decision rule,
assumptions and category probabilities. Each sample-size scenario has a
separate table, with one row per alternative and one column per decision
category. Categories are retained separately; probabilities are neither
recomputed nor aggregated. Rates tables show the treatment event
probability and the beneficial risk difference. Their assumptions
include the control event probability, beneficial direction and selected
interval method.

The `digits` and `output` arguments follow
[`summary.ParameterSet()`](https://docs.rpact.org/qtd/reference/summary.ParameterSet.md).
The output parts are `"title"`, `"overview"`, `"body"` and `"all"`. For
a decision rule, the body contains its parameters. For operating
characteristics, the overview contains the rule and assumptions, and the
body contains the tables. Summary precision follows
`rpact.summary.digits` and `rpact.summary.digits.probs`. The summary
keeps a copy of the supplied object, so later changes to the original do
not change an existing summary.

Print a summary with `print(summary(oc), markdown = TRUE)` to obtain
Markdown with probability tables. Console tables use wrapped, readable
column names.

## See also

[`summary.ParameterSet()`](https://docs.rpact.org/qtd/reference/summary.ParameterSet.md)

## Examples

``` r
rule <- getDesignQuantitative(targetValue = 0, lowerReferenceValue = -0.05)
rates <- getOperatingCharcteristicsRates(rule,
    numberOfSubjects = matrix(c(75, 75, 75, 150), ncol = 2, byrow = TRUE),
    pi1 = c(0.1, 0.15, 0.175, 0.2, 0.25), pi2 = 0.15, directionUpper = FALSE)
summary(rates)
#> Operating-characteristics for comparing rates
#> 
#> Decision rule:
#> 
#>   Target value: 0
#>   Lower reference value: -0.050
#>   Upper percentile: 0.9000
#>   Lower percentile: 0.2000
#> 
#> Assumptions:
#> 
#>   Treatment groups: 2
#>   Assumed control rate: 0.1500
#>   Direction upper: FALSE
#>   Interval method: qtd1
#> 
#> Number of subjects (control = 75, treatment = 75)
#> 
#>     Assumed Alternatives Go (Fixed)     Go Indeterminate Indeterminate   Stop
#>   treatment                                            1             2       
#>        rate                                                                  
#> 1    0.1000        0.050     0.5352 0.3132        0.1384       <0.0001 0.0123
#> 2    0.1500        0.000     0.2038 0.3044        0.3913       <0.0001 0.0843
#> 3    0.1750       -0.025     0.1067 0.2268        0.4711       <0.0001 0.1509
#> 4    0.2000       -0.050     0.0502 0.1469        0.4822             0 0.2224
#> 5    0.2500       -0.100     0.0083 0.0438        0.3434             0 0.3066
#> 
#>   Stop (Fixed)
#> 1       0.0009
#> 2       0.0162
#> 3       0.0444
#> 4       0.0982
#> 5       0.2979
#> 
#> Number of subjects (control = 75, treatment = 150)
#> 
#>     Assumed Alternatives Go (Fixed)     Go Indeterminate Indeterminate   Stop
#>   treatment                                            1             2       
#>        rate                                                                  
#> 1    0.1000        0.050     0.5948 0.3202        0.0749       <0.0001 0.0099
#> 2    0.1500        0.000     0.1987 0.3720        0.3309             0 0.0869
#> 3    0.1750       -0.025     0.0921 0.2716        0.4312             0 0.1672
#> 4    0.2000       -0.050     0.0367 0.1626        0.4466             0 0.2580
#> 5    0.2500       -0.100     0.0039 0.0356        0.2770             0 0.3414
#> 
#>   Stop (Fixed)
#> 1       0.0003
#> 2       0.0115
#> 3       0.0379
#> 4       0.0961
#> 5       0.3421
#> 
cat(print(summary(rates), markdown = TRUE)[1], "\n")
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
#> 
#> -----
#> 
#> *Operating-characteristics for comparing rates*
#> 
#> *Decision rule*
#> 
#> * *Target value*: 0
#> * *Lower reference value*: -0.050
#> * *Upper percentile*: 0.9000
#> * *Lower percentile*: 0.2000
#> 
#> *Assumptions*
#> 
#> * *Treatment groups*: 2
#> * *Assumed control rate*: 0.1500
#> * *Direction upper*: FALSE
#> * *Interval method*: qtd1
#> 
#> *Number of subjects (control = 75, treatment = 75)*
#> 
#> | | Assumed treatment rate| Alternatives| Go (Fixed)| Go| Indeterminate 1| Indeterminate 2| Stop| Stop (Fixed)|
#> | -----| -----| -----| -----| -----| -----| -----| -----| -----|
#> | 1| 0.1000 | 0.050  | 0.5352 | 0.3132 | 0.1384 | &lt;0.0001| 0.0123 | 0.0009 |
#> | 2| 0.1500 | 0.000  | 0.2038 | 0.3044 | 0.3913 | &lt;0.0001| 0.0843 | 0.0162 |
#> | 3| 0.1750 | -0.025 | 0.1067 | 0.2268 | 0.4711 | &lt;0.0001| 0.1509 | 0.0444 |
#> | 4| 0.2000 | -0.050 | 0.0502 | 0.1469 | 0.4822 | 0      | 0.2224 | 0.0982 |
#> | 5| 0.2500 | -0.100 | 0.0083 | 0.0438 | 0.3434 | 0      | 0.3066 | 0.2979 |
#> 
#> *Number of subjects (control = 75, treatment = 150)*
#> 
#> | | Assumed treatment rate| Alternatives| Go (Fixed)| Go| Indeterminate 1| Indeterminate 2| Stop| Stop (Fixed)|
#> | -----| -----| -----| -----| -----| -----| -----| -----| -----|
#> | 1| 0.1000 | 0.050  | 0.5948 | 0.3202 | 0.0749 | &lt;0.0001| 0.0099 | 0.0003 |
#> | 2| 0.1500 | 0.000  | 0.1987 | 0.3720 | 0.3309 | 0      | 0.0869 | 0.0115 |
#> | 3| 0.1750 | -0.025 | 0.0921 | 0.2716 | 0.4312 | 0      | 0.1672 | 0.0379 |
#> | 4| 0.2000 | -0.050 | 0.0367 | 0.1626 | 0.4466 | 0      | 0.2580 | 0.0961 |
#> | 5| 0.2500 | -0.100 | 0.0039 | 0.0356 | 0.2770 | 0      | 0.3414 | 0.3421 |
#>  
```
