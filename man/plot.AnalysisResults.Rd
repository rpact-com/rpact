% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_analysis_results.R
\name{plot.AnalysisResults}
\alias{plot.AnalysisResults}
\title{Analysis Results Plotting}
\usage{
\method{plot}{AnalysisResults}(
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
  legendPosition = NA_integer_,
  showSource = FALSE,
  grid = 1,
  plotSettings = NULL
)
}
\arguments{
\item{x}{The analysis results at given stage, obtained from \code{\link[=getAnalysisResults]{getAnalysisResults()}}.}

\item{y}{Not available for this kind of plot (is only defined to be compatible to the generic plot function).}

\item{...}{Optional \link[=param_three_dots_plot]{plot arguments}. Furthermore the following arguments can be defined:
\itemize{
\item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified.
Additionally, if testing means was selected, \code{assumedStDev} (assumed standard deviation)
can be specified (default is \code{1}).
\item \code{piTreatmentRange}: A range of assumed rates pi1 to calculate the conditional power.
Additionally, if a two-sample comparison was selected, \code{pi2} can be specified (default is the value from
\code{\link[=getAnalysisResults]{getAnalysisResults()}}).
\item \code{directionUpper}: Specifies the direction of the alternative,
only applicable for one-sided testing; default is \code{TRUE}
which means that larger values of the test statistics yield smaller p-values.
\item \code{\link[=param_thetaH0]{thetaH0}}: The null hypothesis value, default is \code{0} for
the normal and the binary case, it is \code{1} for the survival case.
For testing a rate in one sample, a value thetaH0 in (0, 1) has to be specified for
defining the null hypothesis H0: \code{pi = thetaH0}.
}}

\item{type}{The plot type (default = 1). Note that at the moment only one type (the conditional power plot) is available.}

\item{nPlanned}{The additional (i.e., "new" and not cumulative) sample size planned for each of the subsequent stages.
The argument must be a vector with length equal to the number of remaining stages and contain
the combined sample size from both treatment groups if two groups are considered. For survival outcomes,
it should contain the planned number of additional events.
For multi-arm designs, it is the per-comparison (combined) sample size.
For enrichment designs, it is the (combined) sample size for the considered sub-population.}

\item{allocationRatioPlanned}{The planned allocation ratio \code{n1 / n2} for a two treatment groups
design, default is \code{1}. For multi-arm designs, it is the allocation ratio relating the active arm(s) to the control.
For simulating means and rates for a two treatment groups design, it can be a vector of length \code{kMax}, the number of stages.
It can be a vector of length \code{kMax}, too, for multi-arm and enrichment designs.
In these cases, a change of allocating subjects to treatment groups over the stages can be assessed.
Note that internally \code{allocationRatioPlanned} is treated as a vector of length \code{kMax}, not a scalar.}

\item{main}{The main title, default is \code{"Dataset"}.}

\item{xlab}{The x-axis label, default is \code{"Stage"}.}

\item{ylab}{The y-axis label.}

\item{legendTitle}{The legend title, default is \code{""}.}

\item{palette}{The palette, default is \code{"Set1"}.}

\item{legendPosition}{The position of the legend.
By default (\code{NA_integer_}) the algorithm tries to find a suitable position.
Choose one of the following values to specify the position manually:
\itemize{
\item \code{-1}: no legend will be shown
\item \code{NA}: the algorithm tries to find a suitable position
\item \code{0}: legend position outside plot
\item \code{1}: legend position left top
\item \code{2}: legend position left center
\item \code{3}: legend position left bottom
\item \code{4}: legend position right top
\item \code{5}: legend position right center
\item \code{6}: legend position right bottom
}}

\item{showSource}{Logical. If \code{TRUE}, the parameter names of the object will
be printed which were used to create the plot; that may be, e.g.,
useful to check the values or to create own plots with the base R \code{plot} function.
Alternatively \code{showSource} can be defined as one of the following character values:
\itemize{
\item \code{"commands"}: returns a character vector with plot commands
\item \code{"axes"}: returns a list with the axes definitions
\item \code{"test"}: all plot commands will be validated with \code{eval(parse())} and
returned as character vector (function does not stop if an error occurs)
\item \code{"validate"}: all plot commands will be validated with \code{eval(parse())} and
returned as character vector (function stops if an error occurs)
}
Note: no plot object will be returned if \code{showSource} is a character.}

\item{grid}{An integer value specifying the output of multiple plots.
By default (\code{1}) a list of \code{ggplot} objects will be returned.
If a \code{grid} value > 1 was specified, a grid plot will be returned
if the number of plots is <= specified \code{grid} value;
a list of \code{ggplot} objects will be returned otherwise.
If \code{grid = 0} is specified, all plots will be created using \code{\link[base]{print}} command
and a list of \code{ggplot} objects will be returned invisible.
Note that one of the following packages must be installed to create a grid plot:
'ggpubr', 'gridExtra', or 'cowplot'.}

\item{plotSettings}{An object of class \code{PlotSettings} created by \code{\link[=getPlotSettings]{getPlotSettings()}}.}
}
\value{
Returns a \code{ggplot2} object.
}
\description{
Plots the conditional power together with the likelihood function.
}
\details{
The conditional power is calculated only if effect size and sample size is specified.
}
\examples{
\dontrun{
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
}

}
