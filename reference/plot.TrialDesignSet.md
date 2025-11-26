# Trial Design Set Plotting

Plots a trial design set.

## Usage

``` r
# S3 method for class 'TrialDesignSet'
plot(
  x,
  y,
  ...,
  type = 1L,
  main = NA_character_,
  xlab = NA_character_,
  ylab = NA_character_,
  palette = "Set1",
  theta = seq(-1, 1, 0.02),
  nMax = NA_integer_,
  plotPointsEnabled = NA,
  legendPosition = NA_integer_,
  showSource = FALSE,
  grid = 1,
  plotSettings = NULL
)
```

## Arguments

- x:

  The trial design set, obtained from
  [`getDesignSet()`](https://docs.rpact.org/reference/getDesignSet.md).

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- type:

  The plot type (default = `1`). The following plot types are available:

  - `1`: creates a 'Boundaries' plot

  - `3`: creates a 'Stage Levels' plot

  - `4`: creates a 'Error Spending' plot

  - `5`: creates a 'Power and Early Stopping' plot

  - `6`: creates an 'Average Sample Size and Power / Early Stop' plot

  - `7`: creates an 'Power' plot

  - `8`: creates an 'Early Stopping' plot

  - `9`: creates an 'Average Sample Size' plot

  - `"all"`: creates all available plots and returns it as a grid plot
    or list

- main:

  The main title.

- xlab:

  The x-axis label.

- ylab:

  The y-axis label.

- palette:

  The palette, default is `"Set1"`.

- theta:

  A vector of standardized effect sizes (theta values), default is a
  sequence from -1 to 1.

- nMax:

  The maximum sample size. Must be a positive integer of length 1.

- plotPointsEnabled:

  Logical. If `TRUE`, additional points will be plotted.

- legendPosition:

  The position of the legend. By default (`NA_integer_`) the algorithm
  tries to find a suitable position. Choose one of the following values
  to specify the position manually:

  - `-1`: no legend will be shown

  - `NA`: the algorithm tries to find a suitable position

  - `0`: legend position outside plot

  - `1`: legend position left top

  - `2`: legend position left center

  - `3`: legend position left bottom

  - `4`: legend position right top

  - `5`: legend position right center

  - `6`: legend position right bottom

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

Generic function to plot a trial design set. Is, e.g., useful to compare
different designs or design parameters visual.

## Examples

``` r
if (FALSE) { # \dontrun{
design <- getDesignInverseNormal(
    kMax = 3, alpha = 0.025,
    typeOfDesign = "asKD", gammaA = 2,
    informationRates = c(0.2, 0.7, 1), typeBetaSpending = "bsOF"
)

# Create a set of designs based on the master design defined above
# and varied parameter 'gammaA'
designSet <- getDesignSet(design = design, gammaA = 4)

if (require(ggplot2)) plot(designSet, type = 1, legendPosition = 6)
} # }
```
