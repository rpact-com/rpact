# Trial Design Plan Plotting

Plots a trial design plan.

## Usage

``` r
# S3 method for class 'TrialDesignPlan'
plot(
  x,
  y,
  ...,
  main = NA_character_,
  xlab = NA_character_,
  ylab = NA_character_,
  type = NA_integer_,
  palette = "Set1",
  theta = NA_real_,
  plotPointsEnabled = NA,
  legendPosition = "auto",
  showSource = FALSE,
  grid = 1,
  plotSettings = NULL
)
```

## Arguments

- x:

  The trial design plan, obtained from  
  [`getSampleSizeMeans()`](https://docs.rpact.org/reference/getSampleSizeMeans.md),  
  [`getSampleSizeRates()`](https://docs.rpact.org/reference/getSampleSizeRates.md),  
  [`getSampleSizeSurvival()`](https://docs.rpact.org/reference/getSampleSizeSurvival.md),  
  [`getSampleSizeCounts()`](https://docs.rpact.org/reference/getSampleSizeCounts.md),  
  [`getPowerMeans()`](https://docs.rpact.org/reference/getPowerMeans.md),  
  [`getPowerRates()`](https://docs.rpact.org/reference/getPowerRates.md)
  or  
  [`getPowerSurvival()`](https://docs.rpact.org/reference/getPowerSurvival.md)
  or  
  [`getPowerCounts()`](https://docs.rpact.org/reference/getPowerCounts.md).

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- main:

  The main title.

- xlab:

  The x-axis label.

- ylab:

  The y-axis label.

- type:

  The plot type (default = `1`). The following plot types are available:

  - `1`: creates a 'Boundaries' plot

  - `2`: creates a 'Boundaries Effect Scale' plot

  - `3`: creates a 'Boundaries p Values Scale' plot

  - `4`: creates a 'Error Spending' plot

  - `5`: creates a 'Sample Size' or 'Overall Power and Early Stopping'
    plot

  - `6`: creates a 'Number of Events' or 'Sample Size' plot

  - `7`: creates an 'Overall Power' plot

  - `8`: creates an 'Overall Early Stopping' plot

  - `9`: creates an 'Expected Number of Events' or 'Expected Sample
    Size' plot

  - `10`: creates a 'Study Duration' plot

  - `11`: creates an 'Expected Number of Subjects' plot

  - `12`: creates an 'Analysis Times' plot

  - `13`: creates a 'Cumulative Distribution Function' plot

  - `14`: creates a 'Survival Function' plot

  - `"all"`: creates all available plots and returns it as a grid plot
    or list

- palette:

  The palette, default is `"Set1"`.

- theta:

  A vector of standardized effect sizes (theta values), default is a
  sequence from -1 to 1.

- plotPointsEnabled:

  Logical. If `TRUE`, additional points will be plotted.

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

Generic function to plot all kinds of trial design plans.

## Examples

``` r
if (FALSE) { # \dontrun{
if (require(ggplot2)) plot(getSampleSizeMeans())
} # }
```
