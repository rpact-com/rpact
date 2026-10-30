# Plot Comparison Data

Produces a ggplot object from unrounded comparison data.

## Usage

``` r
plotComparison(
  data,
  xParamName = NULL,
  yParamName = NULL,
  ...,
  groupParamName = NULL,
  categoryParamName = NULL,
  mainTitle = NULL,
  legendTitle = NULL,
  plotLineEnabled = TRUE,
  plotPointsEnabled = TRUE,
  pointBorder = 4,
  discreteXAxis = FALSE,
  mirrorModeEnabled = FALSE,
  legendPosition = "auto",
  shinyOptimization = FALSE,
  objects = NULL,
  yAxisSide = "auto",
  categoryLegendPosition = NULL,
  parameterLegendPosition = NULL
)
```

## Arguments

- data:

  A non-empty comparison data frame, a `TrialComparison`, or its
  summary.

- xParamName, yParamName:

  Optional axis columns, selected by
  [`getComparisonPlotParameters()`](https://docs.rpact.org/reference/getComparisonPlotParameters.md)
  when omitted. `yParamName` may be a vector of distinct numeric
  columns.

- ...:

  Reserved for future extensions.

- groupParamName:

  Optional grouping column, mapped to colour when categories are also
  present.

- categoryParamName:

  Optional character vector of category columns. Multiple columns define
  their joint combinations. NULL selects categories automatically;
  NA_character\_ or character(0) disables categories.

- mainTitle:

  Plot title. When omitted, generated from the original objects using
  the same title as print and summary. Supply NULL or NA to hide it.

- legendTitle:

  Category/group legend title. When omitted, object identifiers use
  Design, Calculation, Simulation, or a combination derived from the
  original objects or comparison metadata. Other categories use their
  parameter captions. With multiple y parameters and no categories, the
  title is Parameter. Explicit titles, including NULL to hide titles,
  take precedence.

- plotLineEnabled, plotPointsEnabled:

  Whether to show lines and points.

- pointBorder:

  Point border style, as used by rpact plots.

- discreteXAxis:

  Whether numeric x ticks should use the observed values only.

- mirrorModeEnabled:

  Whether to use fill categories without a legend.

- legendPosition:

  Shared legend position. "auto" (default) uses the adaptive placement
  described below. Alternatively use rpact positions 1 (top left), 2
  (centre left), 3 (bottom left), 4 (top right), 5 (centre right), 6
  (bottom right), -1 (hidden), 0 (default position with square aspect),
  or a ggplot position name (none, left, right, bottom, top).

- shinyOptimization:

  Whether to use compact sizing for Shiny displays.

- objects:

  Optional original objects for role-aware selection with a data frame,
  as in
  [`getComparisonPlotParameters()`](https://docs.rpact.org/reference/getComparisonPlotParameters.md).

- yAxisSide:

  Axis assignment: "auto" (default), "left", or "right". Supply one
  value for all y parameters, a vector in their order, or a named vector
  assigning individual parameters. Unspecified named entries remain
  automatic. The same argument is accepted by `plotComparison()`.

- categoryLegendPosition, parameterLegendPosition:

  Optional separate positions for category/group and y-parameter
  legends, using the same values as `legendPosition`. NULL inherits the
  shared setting. With both guides present, shared automatic positioning
  uses the adaptive layout described below. Explicit positions override
  this layout; two explicit inside positions may place the guides
  independently. A shared hidden legend remains hidden.

## Value

A ggplot object, which can be customized with ggplot2 layers.

## Details

Selection is delegated to
[`getComparisonPlotParameters()`](https://docs.rpact.org/reference/getComparisonPlotParameters.md).
If its `message` is non-NULL, plotting raises an error using that text.
Applications can call the selector first to display choices and messages
without creating a plot. Multiple category columns are combined into a
discrete colour (or fill when an explicit group is present or mirror
mode is enabled). Legend labels show category values in the supplied
column order, separated by ' / '. Every observed combination remains
distinct, including missing values and values containing separators.
Category columns must be distinct valid column names. More than nine
colour groups use a hue palette instead of the limited Set1 palette, so
every combination receives a colour. Categories and groups jointly
identify lines. Without either, objectNumber (or designNumber)
identifies separate objects. Stages are separated unless used as an
axis. Select categories explicitly to compare results across objects
along a parameter axis.

Multiple y parameters are drawn as separate series, identified by line
type and point shape. These styles cycle for larger selections and can
be replaced with ggplot2 scales. Categories and groups remain
independent of the parameter identity. All axis labels and parameter
legends use the rpact parameter captions with the original object
context when available, falling back to technical names for unknown
parameters. This also applies to a single y parameter. With two y axes,
right-axis values are transformed onto the left plotting scale and the
right axis displays the exact inverse transformation, retaining original
units. Raw values and original columns remain in the long plot data; the
caller's data are not modified. Explicitly placing all parameters on the
right uses one primary axis on the right. Parameters assigned to the
same side always share that scale.

For multiple y parameters with category/group legends,
`legendPosition = "auto"` prefers horizontal category entries below the
x-axis and a separate vertical parameter legend automatically placed
inside. Category entries wrap into rows according to measured guide and
panel dimensions. If the parameter legend takes too much inside space or
covers more data, the alternative layout places it vertically below the
panel and positions the category legend inside. Layout changes and label
wrapping precede font reductions, which are limited to ten percent. If
neither inside layout fits, both guides are placed below the panel. Very
large legends still require a larger graphics device or an explicit
layout. Explicit shared positions, such as "bottom", continue to apply
to both guides unless a separate position is supplied. With only one
guide, the common rpact positioning rules apply. Automatic inside
placement scores points and line segments in panel coordinates,
including secondary-axis scaling. This is a heuristic using the current
device (seven inches square if none is open); resizing or exporting to
another size may require recreating the plot.

Interactive converters must preserve the secondary-axis transformation.
A plain `plotly::ggplotly()` conversion may omit the right axis and
display transformed hover values.
[`getComparisonPlotParameters()`](https://docs.rpact.org/reference/getComparisonPlotParameters.md)
exposes the side assignments and transformation for application-specific
adapters.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- getComparisonData(list(getDesignFixed(), getDesignGroupSequential()))
plotComparison(data, "stages", "criticalValues", plotLineEnabled = FALSE)
plotComparison(data, "stages", "criticalValues",
    categoryParamName = c("objectType", "sided"), plotLineEnabled = FALSE)
performance <- as.data.frame(getPowerAndAverageSampleNumber(
    getDesignGroupSequential(kMax = 3), nMax = 100))
plotComparison(performance, "theta",
    c("averageSampleNumber", "overallEarlyStop", "calculatedPower"),
    yAxisSide = c(averageSampleNumber = "left",
        overallEarlyStop = "right", calculatedPower = "right"),
    plotPointsEnabled = FALSE)
plotComparison(data, "stages", c("criticalValues", "stageLevels"),
    categoryLegendPosition = "auto", parameterLegendPosition = "bottom")
} # }
```
