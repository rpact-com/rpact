# Analysis Results Plotting

Plots the conditional power together with the likelihood function.

## Usage

``` r
# S3 method for class 'AnalysisResults'
plot(
  x,
  y,
  ...,
  type = 1L,
  nPlanned = NA_real_,
  allocationRatioPlanned = NA_real_,
  main = NA_character_,
  xlab = NA_character_,
  ylab = NA_character_,
  legendTitle = NA_character_,
  palette = "Set1",
  legendPosition = "auto",
  showSource = FALSE,
  grid = 1,
  plotSettings = NULL
)
```

## Arguments

- x:

  The analysis results at given stage, obtained from
  [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md).

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional [plot
  arguments](https://docs.rpact.org/reference/param_three_dots_plot.md).
  Furthermore the following arguments can be defined:

  - `thetaRange`: A range of assumed effect sizes if testing means or a
    survival design was specified. Additionally, if testing means was
    selected, `assumedStDev` (assumed standard deviation) can be
    specified (default is `1`).

  - `piTreatmentRange`: A range of assumed rates pi1 to calculate the
    conditional power. Additionally, if a two-sample comparison was
    selected, `pi2` can be specified (default is the value from
    [`getAnalysisResults()`](https://docs.rpact.org/reference/getAnalysisResults.md)).

  - `directionUpper`: Specifies the direction of the alternative, only
    applicable for one-sided testing; default is `TRUE` which means that
    larger values of the test statistics yield smaller p-values.

  - [`thetaH0`](https://docs.rpact.org/reference/param_thetaH0.md): The
    null hypothesis value, default is `0` for the normal and the binary
    case, it is `1` for the survival case. For testing a rate in one
    sample, a value thetaH0 in (0, 1) has to be specified for defining
    the null hypothesis H0: `pi = thetaH0`.

- type:

  The plot type (default = 1). Note that at the moment only one type
  (the conditional power plot) is available.

- nPlanned:

  The additional (i.e., "new" and not cumulative) sample size planned
  for each of the subsequent stages. The argument must be a vector with
  length equal to the number of remaining stages and contain the
  combined sample size from both treatment groups if two groups are
  considered. For survival outcomes, it should contain the planned
  number of additional events. For multi-arm designs, it is the
  per-comparison (combined) sample size. For enrichment designs, it is
  the (combined) sample size for the considered subpopulation.

- allocationRatioPlanned:

  The planned allocation ratio `n1 / n2` for a two treatment groups
  design, default is `1`. For multi-arm designs, it is the allocation
  ratio relating the active arm(s) to the control. For simulating means
  and rates for a two treatment groups design, it can be a vector of
  length `kMax`, the number of stages. It can be a vector of length
  `kMax`, too, for multi-arm and enrichment designs. In these cases, a
  change of allocating subjects to treatment groups over the stages can
  be assessed. Note that internally `allocationRatioPlanned` is treated
  as a vector of length `kMax`, not a scalar.

- main:

  The main title, default is `"Dataset"`.

- xlab:

  The x-axis label, default is `"Stage"`.

- ylab:

  The y-axis label.

- legendTitle:

  The legend title, default is `""`.

- palette:

  The palette, default is `"Set1"`.

- legendPosition:

  The legend position. The default, `"auto"`, measures the legend and
  chooses among six inside positions by minimizing overlap with points,
  line segments, reference lines, bars, boxes, and error bars. Long
  labels are wrapped and entries arranged vertically in automatic mode.
  Oversized legends and faceted plots use the outside right position.
  This heuristic uses the current graphics device (seven inches square
  when no device is open); resizing may require recreating the plot or
  setting a position explicitly. Manual positions are:

  - `-1` or `"none"`: hide the legend

  - `"left"`, `"right"`, `"bottom"`, `"top"`: outside the plot

  - `0`: default outside position with a square aspect ratio

  - `1`, `2`, `3`: inside left, at the top, centre, or bottom

  - `4`, `5`, `6`: inside right, at the top, centre, or bottom

  - `NA`: retain the legacy plot-specific positioning rules

- showSource:

  Logical. If `TRUE`, the parameter names of the object will be printed
  which were used to create the plot; that may be, e.g., useful to check
  the values or to create own plots with the base R `plot` function.
  Alternatively `showSource` can be defined as one of the following
  character values:

  - `"commands"`: returns a character vector with plot commands

  - `"axes"`: returns a list with the axes definitions

  - `"test"`: all plot commands will be validated with `eval(parse())`
    and returned as character vector (function does not stop if an error
    occurs)

  - `"validate"`: all plot commands will be validated with
    `eval(parse())` and returned as character vector (function stops if
    an error occurs)

  Note: no plot object will be returned if `showSource` is a character.

- grid:

  An integer value specifying the output of multiple plots. By default
  (`1`) a list of `ggplot` objects will be returned. If a `grid` value
  \> 1 was specified, a grid plot will be returned if the number of
  plots is \<= specified `grid` value; a list of `ggplot` objects will
  be returned otherwise. If `grid = 0` is specified, all plots will be
  created using [`print`](https://rdrr.io/r/base/print.html) command and
  a list of `ggplot` objects will be returned invisible. Note that one
  of the following packages must be installed to create a grid plot:
  'ggpubr', 'gridExtra', or 'cowplot'.

- plotSettings:

  An object of class `PlotSettings` created by
  [`getPlotSettings()`](https://docs.rpact.org/reference/getPlotSettings.md).

## Value

Returns a `ggplot2` object.

## Details

The conditional power is calculated only if effect size and sample size
is specified.

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignGroupSequential(kMax = 2)

dataExample <- getDataset(
    n = c(20, 30),
    means = c(50, 51),
    stDevs = c(130, 140)
)

result <- getAnalysisResults(design = design, 
    dataInput = dataExample, thetaH0 = 20, 
    nPlanned = c(30), thetaH1 = 1.5, stage = 1) 

if (require(ggplot2)) plot(result, thetaRange = c(0, 100))
} # }
```
