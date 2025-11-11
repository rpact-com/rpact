# Plot Trial Design Summaries

Generic function to plot a `TrialDesignSummaries` object.

## Usage

``` r
# S3 method for class 'TrialDesignSummaries'
plot(x, ..., type = 1L, grid = 1)
```

## Arguments

- x:

  a `TrialDesignSummaries` object to plot.

- ...:

  further arguments passed to or from other methods.

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
