# Summary Factory Plotting

Plots a summary factory.

## Usage

``` r
# S3 method for class 'SummaryFactory'
plot(x, y, ..., showSummary = FALSE)
```

## Arguments

- x:

  The summary factory object.

- y:

  Not available for this kind of plot (is only defined to be compatible
  to the generic plot function).

- ...:

  Optional plot arguments. At the moment `xlim` and `ylim` are
  implemented for changing x or y axis limits without dropping data
  observations.

- showSummary:

  Show the summary before creating the plot output, default is `FALSE`.

## Value

Returns a `ggplot2` object.

## Details

Generic function to plot all kinds of summary factories.
