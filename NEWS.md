
# rpact 4.0.1

* Minimum version of suggested package `ggplot2` changed from 2.2.0 to 3.2.0
* When analyzing with a two-sided test, an issue with the calculation of the conditional rejection probability was fixed
* Issue [#41](https://github.com/rpact-com/rpact/issues/41) fixed
* Usage of pipe-operators improved
* Minor summary improvements


# rpact 4.0.0

## New features

* All reference classes in the package have been replaced by [R6](https://cran.r-project.org/package=R6) classes. This change brings significant advantages, including improved performance, more flexible and cleaner object-oriented programming, and enhanced encapsulation of methods and properties. The transition to R6 classes allows for more efficient memory management and faster execution, making the package more robust and scalable. Additionally, R6 classes provide a more intuitive and user-friendly interface for developers, facilitating the creation and maintenance of complex data structures and workflows.
* Extension of the function `getPerformanceScore()` for sample size recalculation rules to the setting of binary endpoints according to [Bokelmann et al. (2024)](https://doi.org/10.1186/s12874-024-02150-4)
* The `getSimulationMultiArmMeans()`, `getSimulationMultiArmRates()`, and `getSimulationMultiArmSurvival()` functions now support an enhanced `selectArmsFunction` argument. Previously, only `effectVector` and `stage` were allowed as arguments. Now, users can optionally utilize additional arguments for more powerful custom function implementations, including `conditionalPower`, `conditionalCriticalValue`, `plannedSubjects/plannedEvents`, `allocationRatioPlanned`, `selectedArms`, `thetaH1` (for means and survival), `stDevH1` (for means), `overallEffects`, and for rates additionally: `piTreatmentsH1`, `piControlH1`, `overallRates`, and `overallRatesControl`.
* Same as above for`getSimulationEnrichmentMeans()`, `getSimulationEnrichmentRates()`, and `getSimulationEnrichmentSurvival()`. Specifically, support for population selection with `selectPopulationsFunction` argument based on predictive/posterior probabilities added (see [#32](https://github.com/rpact-com/rpact/issues/32))
* The `fetch()` and `obtain()` functions can be used to extract a single parameter from an rpact result object, which is useful for writing pipe-operator linked commands

## Improvements, issues, and changes

* Issues [#25](https://github.com/rpact-com/rpact/issues/25), [#35](https://github.com/rpact-com/rpact/issues/35), and [#36](https://github.com/rpact-com/rpact/issues/36) fixed
* Minor improvements


# rpact 3.5.1

* The internal fields `.parameterNames` and `.parameterFormatFunctions` were removed from all rpact result objects in favor of a more efficient solution
* Issues [#15](https://github.com/rpact-com/rpact/issues/15), [#16](https://github.com/rpact-com/rpact/issues/16), [#17](https://github.com/rpact-com/rpact/issues/17), [#19](https://github.com/rpact-com/rpact/issues/19), and [#23](https://github.com/rpact-com/rpact/issues/23) fixed
* Fixed inconsistent naming of variables and class fields (issue [#21](https://github.com/rpact-com/rpact/issues/21))
    - `getSampleSizeSurvival()` / `getPowerSurvival()`:
       + Field `eventsPerStage` replaced by `cumulativeEventsPerStage`
       + Field `singleEventsPerStage` added
    - `getSimulationSurvival()`: 
       + Field `eventsPerStage` replaced by `singleEventsPerStage` 
       + Field `overallEventsPerStage` replaced by `cumulativeEventsPerStage` 
    - `getSimulationMultiArmSurvival()`: 
       + Field `eventsPerStage` replaced by `cumulativeEventsPerStage` 
       + Field `singleNumberOfEventsPerStage` replaced by `singleEventsPerArmAndStage` 
       + Field `singleEventsPerStage` added
    - `getSimulationEnrichmentSurvival()`: 
       + field `singleNumberOfEventsPerStage` replaced by `singleEventsPerSubsetAndStage` 
* Test coverage CI/CD pipeline activated with the assistance of GitHub Actions, which runs `covr` and uploads the results to [codecov.io](https://app.codecov.io/gh/rpact-com)
* Minor improvements


# rpact 3.5.0

## New features

* The new functions `getSampleSizeCounts()` and `getPowerCounts()` can be used to perform sample size calculations and the assessment of test characteristics for clinical trials with negative binomial distributed count data. This is possible for fixed sample size and group sequential designs. For the latter, the methodology described in Muetze et al. (2019) is implemented. These functions can also be used to perform blinded sample size reassessments according to Friede and Schmidli (2010).

## Improvements, issues, and changes

* Original Fortran 77 code of AS 251 included into the package, see functions `mvnprd`, `mvstud`, `as251Normal`, and `as251StudentT`
* R package `mnormt` dependency has been removed 
* Argument `theta` can be used for plotting of sample size and power results
* Pipe operator usage improved
* Shiny app link changed to https://rpact.shinyapps.io/cloud
* Several minor improvements


# rpact 3.4.0

## New features

* The new function `getPerformanceScore()` calculates the conditional performance score, its sub-scores and components according to [Herrmann et al. (2020)](https://doi.org/10.1002/sim.8534) for a given simulation result from a two-stage design
* `allocationRatioPlanned` for simulating multi-arm and enrichment designs can be a vector of length kMax, the number of stages 
* `getObjectRCode()` (short: `rcmd()`): with the new arguments `pipeOperator` and `output` many new output variants can be specified, e.g., the native R pipe operator or the magrittr pipe operator can be used
* Generic function `knitr::knit_print` for all result objects implemented and automatic code chunk option `results = 'asis'` activated

## Improvements, issues, and changes

* Improved speed of numerical computation of group sequential designs and test characteristics
* Multivariate t distribution restricted to `df <= 500` because of erroneous results in `mnormt` package otherwise. For `df > 500`, multivariate normal distribution is used 
* Performance of cumulative distribution function and survival function plot improved
* Test coverage extended and improved
* Descriptions for all class fields added
* Renamed field `omega` to `chi` in class `TrialDesignPlanSurvival`
* Several minor improvements


# rpact 3.3.4

* Rcpp sugar function `sapply` removed from C++ code to stop deprecated warnings on r-devel-linux-x86_64-fedora-clang 
* Minor improvements

# rpact 3.3.3

* `allocationRatioPlanned` for simulating means and rates for a two treatment groups design can be a vector of length kMax, the number of stages 
* `calcSubjectsFunction` can be used in C++ version for simulating means and rates
* `calcEventsFunction` added in getSimulationSurvival()
* `getPerformanceScore()` added: calculates the performance score for simulation means results (1 and 2 groups; 2 stages)
* Performance of simulation rates improved for 1 and 2 groups (by translating from R to C++)
* Performance of simulation means improved for 1 and 2 groups
* Two-sided O'Brien and Fleming beta-spending function corrected
* Issue in plot type 5 for sample size means and rates fixed
* Added dependency on R >= 3.6.0
* Minor improvements

# rpact 3.3.2

* Design objects can be piped into `getDataset()` to enable pipe syntax for analysis, e.g., 
`getDesignGroupSequential() |> getDataset(dataMeans) |> getAnalysisResults()`
* Performance of simulation means improved for 1 and 2 groups (by translating from R to C++)
* Total test time was cut in half by improving simulation performance and enabling parallel testing
* `SystemRequirements: C++11` added to DESCRIPTION to enable C++ 11 compilation on R 3.x
* Minor improvements

# rpact 3.3.1

* Help pages improved
* Parameter `betaAdjustment` can also be used in `getDesignInverseNormal()`
* `subsets` removed from result of `getWideFormat()` for non-enrichment datasets
* Summary of enrichment survival simulation results improved
* Parameter `populations` in `getSimulationEnrichmentMeans()`, `getSimulationEnrichmentRates()`, and `getSimulationEnrichmentSurvival()` has been removed since it is always derived from `effectList`
* Bug fixed in `getSimulationEnrichmentRates()` for calculated non-integer number of subjects 
* Futility probabilities and futility bounds corrected for two-sided beta-spending function approach
* `getRawData()`: the resulting `data.frame` now contains the correct `stopStage` and `lastObservationTime` (formerly `observationTime`)
* `deltaWT` is provided with three decimal points for typeOfDesign = "WToptimum"
* Generic `as.data.frame` functions improved
* testthat version changed to edition 3
* The rpact source code has been published on GitHub and the bug report link has been changed to https://github.com/rpact-com/rpact/issues
* Minor improvements

# rpact 3.3.0

## New features

* Two-sided beta-spending approach with binding and non-binding futility bounds
* Delayed response utility added in design specification

## Improvements, issues, and changes

* `getSimulationMultiArmSurvival()`: single stage treatment arm specific event numbers account for selection procedure
* User defined selection function can be used in `getSimulationEnrichmentRates()` and  `getSimulationEnrichmentSurvival()`
* Design summary extended by information of `getDesignCharacteristics()`
* `getSimulationSurvival()`: the result object now contains the new parameter `overallEventsPerStage`, which contains the values previously given in `eventsPerStage` (it was "cumulative" by mistake); `eventsPerStage` contains now the non-cumulative values as expected
* Minor improvements

# rpact 3.2.3

* Performance of group sequential and Fisher's combination test designs improved
* 'register' storage class specifier removed from C++ sources
* Minor improvements

# rpact 3.2.2

* Performance of group sequential and Fisher's combination test designs improved (by translating from R to C++)
* Numerical issue in analysis time calculation for survival design in specific cases resolved
* The internally used minimum quantile function value was changed from `stats::qnorm(1e-323)` to `stats::qnorm(1e-100)`
* Unit tests extended
* Minor improvements

# rpact 3.2.1

* C++ warning "using integer absolute value function 'abs' when argument is of floating point type" under r-devel-linux-x86_64-debian-clang removed
* getDataset: support of emmeans result objects as input improved 
* `getAnalysisResults()`: issue with zero values in the argument 'userAlphaSpending' fixed
* Minor improvements

# rpact 3.2.0

## New features

* Simulation tools for enrichment design testing means, rates, and hazard ratios: function `getSimulationEnrichmentMeans()`, `getSimulationEnrichmentRates()`, `getSimulationEnrichmentSurvival()` available for simulation of enrichment designs; 
note that this is a novel implementation, hence experimental
* `getDesignGroupSequential()` / `getDesignInverseNormal()`: new typeOfDesign = "noEarlyEfficacy" added

## Improvements, issues, and changes

* `getSimulationSurvival()`: bug fixed for accruallIntensity = 0 at some accrual intervals
* For observed conditional power, standardized theta not truncated to 0 any more in `getSimulationMultiArmMeans()`, `getSimulationMultiArmRates()`, and `getSimulationMultiArmSurvival()`
* Conditional power calculation for analysis rates takes into account differently the null value of condErrorRate
* Function `testPackage()`: a problem with downloading full set of unit tests under Debian/Linux has been fixed
* Generic function `kable()` improved: optional knitr::kable arguments enabled, e.g., format
* In print and summary output, "overall" renamed to "cumulative" if means, stDevs, or rate are calculated over stages rather than stage-wise 
* getDataset: support of emmeans result objects as input improved 
* Numerical accuracy of `qnorm()` calculations improved 
* Analysis enrichment results now support the generic function `as.data.frame()`
* Naming of the stage results parameters in the print output improved
* New example data added: "rawDataTwoArmNormal"
* Issue in summary fixed: earlyStop and rejectPerStage were no longer displayed
* Minor improvements

# rpact 3.1.1

* Performance of two-sided Pampallona & Tsiatis design improved
* 12 example datasets added
* Sample sizes in plots now have the same format as in print output; format can be changed using setOutputFormat()
* getDataset supports emmeans result objects as input
* Print output of simulation results improved
* Added dependency on R >= 3.5.0 because serialized objects in serialize/load version 3 cannot be read in older versions of R
* Plot label interface for configuration via the rpact Shiny app implemented
* Minor improvements

# rpact 3.1.0

## New features

* Analysis tools for enrichment design testing means, rates, and hazard ratios: function `getAnalysisResults()` generalized for enrichment designs; function `getDataset()` generalized for entering stratified data; manual extended for enrichment designs
* Automatic boundary recalculations during the trial for analysis with alpha spending approach, 
  including under- and over-running: 
  setup via the optional parameters 'maxInformation' and 'informationEpsilon' in function `getAnalysisResults()`
* The new function `getObjectRCode()` (short: `rcmd()`) returns the original R command which 
  produced any rpact result object, including all dependencies
* `getWideFormat()` and `getLongFormat()` return a dataset object in wide format (unstacked) or long format (narrow, stacked)
* Generic function `kable()` returns the output of an rpact result object formatted in Markdown.
* Generic function `t()` returns the transpose of an rpact result object

## Improvements, issues, and changes

* New argument 'plotSettings' added to all plot functions
* Summary for design, simulation, and analysis unified and extended
* Issue in `getDesignFisher()` fixed: `getDesignFisher(method = "noInteraction", kMax = 3)` and `getDesignFisher(method = "noInteraction")` produced different results
* 'normalApproximation' default value changed to TRUE for multi-arm analysis of rates
* Repeated p-values: in search algorithm, upper bound of significance level corrected when considering binding futility bounds
* `testPackage()`: the default call is now running only a small subset of all available unit tests; with the new 
  argument 'connection' the owners of the rpact validation documentation 
  can enter a 'token' and a 'secret' to get full access to all unit tests
* Scaling of grid plots improved
* Minor improvements

# rpact 3.0.4

* Beta-spending function approach with binding futility bounds
* Pampallona & Tsiatis design with binding and non-binding futility bounds
* Argument 'accrualIntensityType' added to `getSampleSizeSurvival()`, `getSimulationSurvival()`, `getNumberOfSubjects()`, and `getEventProbabilities()`
* Specification of Weibull survival times possible through definition of hazard rates or medians in simulation tool
* Minor improvements

# rpact 3.0.3

* New utility functions `getParameterCaption()` and `getParameterName()` implemented
* Design parameters added to simulation print output
* Generic function `as.matrix()` improved for several result objects
* Issue in `getAvailablePlotTypes()` for sample size and power results fixed
* Issue for `getDesignFisher(kMax = 1)` in `getSimulationMultiArm...()` fixed 
* `getSimulationMultiArmSurvival()`: correlation of log-rank statistics revised and improved 
* `getSimulationMultiArmMeans()`: name of the first effectMeasure option "effectDifference" changed to "effectEstimate" 
* `getSimulation[MultiArm][Means/Rates/Survival]()`: argument 'showStatistics' now works correctly and is consistently FALSE by default for multi-arm and non-multi-arm
* `getSimulation[MultiArm]Survival()`: generic function `summary()` improved
* `getAnalysisResults()`: generic function `summary()` improved
* `getAccrualTime()`: improved and new argument 'accrualIntensityType' added
* Header text added to design summaries
* `getSampleSizeSurvival()`: field 'studyDurationH1' in result object was replaced by 'studyDuration', i.e., 'studyDurationH1' is deprecated and will be removed in future versions
* Minor changes in the inline help and manual
* Minor improvements

# rpact 3.0.2

* `getSimulationMultiArmSurvival()`: plannedEvents redefined as overall events over treatment arms
* `getStageResults()`: element overallPooledStDevs added; print output improved
* Unit tests improved: test coverage and references to the functional specification optimized
* plot type 13 of `getSampleSizeSurvival()` with user defined lambdas with different lengths: issue fixed
* Minor improvements

# rpact 3.0.1

* Vignette "rpact: Getting Started" included into the package
* New summary output option "rpact.summary.width" added
* Generic function `summary()` improved for several result objects
* Result output of function `testPackage()` improved
* `getSimulationMultiArm[Means/Rates/Survival]()`: stage index corrected for user defined calcSubjectsFunction or calcEventsFunction
* `getSimulationMultiArmRates()`: adjustment for identical simulated rates to account for ties
* `getSimulationMultiArmSurvival()`: corrected correlation of test statistics
* Output formatting improved
* Minor improvements

# rpact 3.0.0

## New features

* Simulation tools for multi-arm design testing means, rates, and hazard ratios
* Analysis tools for multi-arm design testing means, rates, and hazard ratios
* `getSimulationRates()`: exact versions for testing a rate (one-sample case) and equality of rates (two-sample case)
* getDataset: multi-arm datasets for means, rates, and survival data
* Analysis of fixed designs
* Summary for analysis and simulation result objects newly implemented
* Summary for most rpact result objects substantially improved and enhanced
* `getEventProbabilities()`: plot of result object
* `getNumberOfSubjects()`: plot of result object
* Visual comparison of two designs: `plot(design1, design2)`
* Functions setOutputFormat and getOutputFormat implemented: definition of user defined output formats
* `getSimulationMeans()`: thetaH1 and stDevH1 can be specified for assessment of sample size recalculation (replaces thetaStandardized)
* `getSimulationSurvival()`: separate p-values added to the aggregated simulation data for Fisher designs
* `getSimulationMeans()`, `getSimulationRates()`: Cumulated number of subjects integrated in getData object
* `getSimulation[MultiArm][Means/Rates/Survival]()`: new logical argument 'showStatistics' added
* Example datasets (csv files) added to the package
* plot type "all": plot all available plots of an object in one step using `plot(x, type = "all")`
* plot type improved: 'type' now can be a vector, e.g., `plot(x, type = c(1, 3))`
* `plot(x, grid = 1)`: new plot argument 'grid' enables the plotting of 2 or more plots in one graphic

## Improvements, issues, and changes

* `getAnalysisResults()`: list output implemented analogous to the output of all other rpact objects
* `getAnalysisResults()`: the following stage result arguments were removed from result object because they were redundant: effectSizes, testStatistics, and pValues. Please use the '.stageResults' object to access them, e.g., results\$.stageResults\$effectSizes
* `getAnalysisResults()`: the following design arguments were removed from result object because they were redundant: stages, informationRates, criticalValues, futilityBounds, alphaSpent, and stageLevels. Please use the '.design' object to access them, e.g., results\$.design\$informationRates
* Optional argument 'stage' removed from functions getConditionalPower, getConditionalRejectionProbabilities, getFinalPValue, getRepeatedPValues, and getTestActions
* Function testPackage improved, e.g., results will be displayed now on screen
* Help system renewed and approved, e.g., help for corresponding generic functions (e.g., plot) linked where applicable
* Function getPiecewiseSurvivalTime improved: pi1 and pi2 will not be calculated any longer for lambda- or median-based definitions; eventTime only required for pi-based definitions
* `plot(x, showSource = TRUE)` improved for all rpact result objects x
* Performance of plotting analysis results of Fisher designs improved
* `getSimulationRates()`: issue for futility stopping for Fisher's combination test fixed
* `getSimulationSurvival()`: issue for expected number of events fixed
* `getSimulationSurvival()`: if eventsNotAchieved > 0, rejection/futility rate and analysis time is estimated for valid simulation runs
* `getSimulationSurvival()`: output improved for lambda1/median1/hazardRatio with length > 1
* `getSampleSizeSurvival()`: calculation of the maximum number of subjects given the provided argument 'followUpTime' improved
* `getPiecewiseSurvivalTime()`: delayed response via list-based piecewiseSurvivalTime definition enabled
* `getAccrualTime()` / `getSimulationSurvival()`: issue with the calculation of absolute accrual intensity by given relative accrual intensity fixed
* `getRawData()`: issue for multiple pi1 solved 
* Implementation of the generic function 'names' improved
* Test coverage improved: lots of new unit tests added
* License information in the DESCRIPTION file corrected: changed from GPL-3 to LGPL-3
* Minor improvements

# rpact 2.0.6

* Boundaries on effect scale for testing means now accounts for the unknown variance case
* `getAnalysisSurvival()`: calculation of stage wise results not more in getStageResults
* `getStageResults()`: the calculation of 'effectSizes' for survival data and thetaH0 != 1 was corrected 
* `getDataset()` of survival data: issue with the internal storage of log ranks fixed
* Sample size plot: issue for kMax = 1 fixed
* `getSampleSizeSurvival()` with piecewise survival time: issue with calculation of 'maxNumberOfSubjects' for given 'followUpTime' fixed 
* Internal Shiny app interface improved
* Minor improvements

# rpact 2.0.5

* Assumed median survival time: get[SampleSize/Power/Simulation]Survival now support direct input of arguments 'median1' and 'median2'
* Output of generic function `summary()` improved
* Plot type 5 of getPower[...] and getSimulation[...] objects improved
* Output of `getSampleSizeSurvival()` with given maxNumberOfSubjects improved
* Output of `get[SampleSize/Power]Survival()` for Kappa != 1 improved
* Assert function for minNumberOfSubjectsPerStage corrected for undefined conditionalPower
* Two-sided boundaries on effect scale in survival design improved
* Error in `summary()` for `getDesign[...]()` fixed
* Other minor improvements

# rpact 2.0.4

* Incorrect output of function `summary()` fixed for `getSampleSize[...]()` and `getPower[...]()`
* as.data.frame: default value of argument 'niceColumnNamesEnabled' changed from TRUE to FALSE

# rpact 2.0.3

## New features

* Plot function for Fisher design implemented
* Generic function `summary()` implemented for `getDesign[...]()`, `getSampleSize[...]()`, `getPower[...]()`, and `getSimulation[...]()` results: a simple boundary summary will be displayed

## Improvements, issues, and changes

* Generic function as.data.frame improved for `getDesign[...]()`, `getSampleSize[...]()`, `getPower[...]()`, and `getSimulation[...]()` results
* Output of `getStageResults()` improved
* Improvements for Shiny app compatibility and better Shiny app performance
* Repeated p-values are no longer calculated for typeOfDesign = "WToptimum"
* Piecewise survival time improved for numeric definition: median and pi will not be calculated and displayed any longer
* Plot: legend title and tick mark positioning improved; optional arguments xlim and ylim implemented
* Sample size/power: usage of argument 'twoSidedPower' optimized
* Performance of function rpwexp/getPiecewiseExponentialRandomNumbers improved (special thanks to Marcel Wolbers for his example code)
* For group sequential designs a warning will be displayed if information rates from design not according to data information
* Format for output of standard deviation optimized

# rpact 2.0.2

* Minor corrections in the inline help
* Labeling of lower and upper critical values (effect scale) reverted
* Simulation for Fisher's combination test corrected
* Parameter minNumberOfAdditionalEventsPerStage renamed to minNumberOfEventsPerStage
* Parameter maxNumberOfAdditionalEventsPerStage renamed to maxNumberOfEventsPerStage
* Parameter minNumberOfAdditionalSubjectsPerStage renamed to minNumberOfSubjectsPerStage
* Parameter maxNumberOfAdditionalSubjectsPerStage renamed to maxNumberOfSubjectsPerStage
* Output of function `getAccrualTime()` improved
* Validation of arguments maxNumberOfIterations, allocation1, and allocation2 added: check for positive integer
* Function `getSampleSizeSurvival()` improved: numeric search for accrualTime if followUpTime is given
* Default value improved for analysis tools: if no effect was specified for conditional power calculation, the observed effect is selected
* Fixed: function getDataset produced an error if only one log-rank value and one event was defined
* Number of subjects per treatment arm are provided in output of simulation survival if allocation ratio != 1
* Function getSimulationSurvival improved: first value of minNumberOfEventsPerStage and maxNumberOfEventsPerStage must be NA or equal to first value of plannedSubjects

# rpact 2.0.1

* Function base::isFALSE replaced to guarantee R 3.4.x compatibility
* C++ compiler warning on r-devel-linux-x86_64-debian-clang system removed 
* C++ compiler error on r-patched-solaris-x86 system fixed

# rpact 2.0.0

## New features

* Power calculation at given or adapted sample size for means, rates and survival data
* Sample size and power calculation for survival trials with piecewise accrual time and intensity
* Sample size and power calculation for survival trials with exponential survival time, piecewise exponential survival time and survival times that follow a Weibull distribution
* Simulation tool for survival trials; our simulator is very fast because it was implemented with C++. Adaptive event number recalculations based on conditional power can be assessed
* Simulation tool for designs with continuous and binary endpoints. Adaptive sample size recalculations based on conditional power can be assessed
* Comprehensive and unified tool for performing sample size calculation for fixed sample size design
* Enhanced plot functionalities

## Improvements, issues, and changes

* Fisher design, analysis of means or rates, conditional rejection probabilities (CRP): calculation issue fixed for stage > 2
* Call of getSampleSize[Means/Rates/Survival] without design argument implemented
* For all `set.seed()` calls 'kind' and 'normal.kind' were specified as follows: kind = "Mersenne-Twister", normal.kind = "Inversion"
* Minor code optimizations, e.g. 'return()' replaced by 'return(invisible())' if reasonable
* Bug in `readDatasets()` fixed: variable names 'group' and 'groups' are now accepted
* "Overall reject per stage" and "Overall futility per stage" renamed to "Overall reject" and "Overall futility", respectively (also variable names)
* Labels "events.." and "..patients.." consistently changed to "# events.." and "# patients...", respectively
* Output format for 'allocationRatioPlanned' specified
* Method 'show' of class 'ParameterSet' expanded: R Markdown output features implemented
* `getSampleSizeSurvival()`: argument 'maxNumberOfPatients' was renamed in 'maxNumberOfSubjects'
* Result output, inline help and documentation: the word 'patient' was replaced by 'subject'
* Variables 'numberOfSubjectsGroup1' and 'numberOfSubjectsGroup2' were renamed to 'numberOfSubjects1' and 'numberOfSubjects1'
* Final p-values for two-sided test (group sequential, inverse normal, and Fisher combination test) available
* Upper and lower boundaries on effect scale for testing rates in two samples

# rpact 1.0.0

* First release of rpact
