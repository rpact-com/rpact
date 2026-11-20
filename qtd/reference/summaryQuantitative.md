# Summaries of Quantitative Trial Designs and Operating Characteristics

The standard [`summary()`](https://rdrr.io/r/base/summary.html) method
supports quantitative decision rules from
[`getDesignQuantitative()`](https://docs.rpact.org/qtd/reference/getDesignQuantitative.md)
and results from
[`getOperatingCharacteristicsMeans()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsMeans.md)
and
[`getOperatingCharacteristicsRates()`](https://docs.rpact.org/qtd/reference/getOperatingCharacteristicsRates.md).

## Value

A `SummaryFactory` object supporting console and Markdown output.

## Details

Use `print(rule)` for compact decision criteria and `summary(rule)` for
reference cutoffs, applicable percentile inputs and detailed decision
tables. Use `summary(oc)` for the decision rule, assumptions and
category probabilities. Each sample-size scenario has a separate table,
with one row per alternative and one column per decision category.
Categories are retained separately; probabilities are neither recomputed
nor aggregated. Names supplied with `pi1` or `alternative` label the
rows in each table. Rates tables show the treatment event probability
and the beneficial risk difference. Their assumptions include the
control event probability, beneficial direction and selected interval
method.

The `digits` and `output` arguments follow
[`summary.ParameterSet()`](https://docs.rpact.org/qtd/reference/summary.ParameterSet.md).
The output parts are `"title"`, `"overview"`, `"body"` and `"all"`. For
a decision rule, the body contains its tables. For operating
characteristics, the overview contains the rule and assumptions, and the
body contains the tables. An explicit `digits` sets the precision for
both general values and probabilities. Otherwise, precision follows
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
rates <- getOperatingCharacteristicsRates(rule,
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
#>   Interval method: wilson
#> 
#> control: 75, treatment: 75
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
#> control: 75, treatment: 150
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
#> * *Interval method*: wilson
#> 
#> *control: 75, treatment: 75*
#> 
#> | | Assumed treatment rate| Alternatives| Go (Fixed)| Go| Indeterminate 1| Indeterminate 2| Stop| Stop (Fixed)|
#> | -----| -----| -----| -----| -----| -----| -----| -----| -----|
#> | 1| 0.1000 | 0.050  | 0.5352 | 0.3132 | 0.1384 | &lt;0.0001| 0.0123 | 0.0009 |
#> | 2| 0.1500 | 0.000  | 0.2038 | 0.3044 | 0.3913 | &lt;0.0001| 0.0843 | 0.0162 |
#> | 3| 0.1750 | -0.025 | 0.1067 | 0.2268 | 0.4711 | &lt;0.0001| 0.1509 | 0.0444 |
#> | 4| 0.2000 | -0.050 | 0.0502 | 0.1469 | 0.4822 | 0      | 0.2224 | 0.0982 |
#> | 5| 0.2500 | -0.100 | 0.0083 | 0.0438 | 0.3434 | 0      | 0.3066 | 0.2979 |
#> 
#> *control: 75, treatment: 150*
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
