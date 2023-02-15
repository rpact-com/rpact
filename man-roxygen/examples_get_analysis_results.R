#' @examples
#' \dontrun{
#' # Example 1 One-Sample t Test
#' # Perform an analysis within a three-stage group sequential design with
#' # O'Brien & Fleming boundaries and one-sample data with a continuous outcome
#' # where H0: mu = 1.2 is to be tested
#' dsnGS <- getDesignGroupSequential()
#' dataMeans <- getDataset(
#'     n = c(30, 30),
#'     means = c(1.96, 1.76),
#'     stDevs = c(1.92, 2.01))
#' getAnalysisResults(design = dsnGS, dataInput = dataMeans, thetaH0 = 1.2)
#'
#' # You can obtain the results when performing an inverse normal combination test
#' # with these data by using the commands
#' dsnIN <- getDesignInverseNormal()
#' getAnalysisResults(design = dsnIN, dataInput = dataMeans, thetaH0 = 1.2)
#'
#' # Example 2 Use Function Approach with Time to Event Data
#' # Perform an analysis within a use function approach according to an
#' # O'Brien & Fleming type use function and survival data where
#' # where H0: hazard ratio = 1 is to be tested. The events were observed
#' # over time and maxInformation = 120, informationEpsilon = 5 specifies
#' # that 116 > 120 - 5 observed events defines the final analysis.
#' design <- getDesignGroupSequential(typeOfDesign = "asOF")
#' dataSurvival <- getDataset(
#' 		cumulativeEvents = c(33, 72, 116),
#' 		cumulativeLogRanks = c(1.33, 1.88, 1.902))
#' getAnalysisResults(design, dataInput = dataSurvival, maxInformation = 120,
#' 		informationEpsilon = 5)
#'
#' # Example 3 Multi-Arm Design
#' # In a four-stage combination test design with O'Brien & Fleming boundaries
#' # at the first stage the second treatment arm was dropped. With the Bonferroni
#' # intersection test, the results together with the CRP, conditional power
#' # (assuming a total of 40 subjects for each comparison and effect sizes 0.5
#' # and 0.8 for treatment arm 1 and 3, respectively, and standard deviation 1.2),
#' # RCIs and p-values of a closed adaptive test procedure are
#' # obtained as follows with the given data (treatment arm 4 refers to the
#' # reference group; displayed with summary and plot commands):
#' data <- getDataset(
#'     n1 = c(22, 23),
#'     n2 = c(21, NA),
#'     n3 = c(20, 25),
#'     n4 = c(25, 27),
#'     means1 = c(1.63, 1.51),
#'     means2 = c(1.4, NA),
#'     means3 = c(0.91, 0.95),
#'     means4 = c(0.83, 0.75),
#'     stds1 = c(1.2, 1.4),
#'     stds2 = c(1.3, NA),
#'     stds3 = c(1.1, 1.14),
#'     stds4 = c(1.02, 1.18))
#' design <- getDesignInverseNormal(kMax = 4)
#' x <- getAnalysisResults(design, dataInput = data, intersectionTest = "Bonferroni",
#'     nPlanned = c(40, 40), thetaH1 = c(0.5, NA, 0.8), assumedStDevs = 1.2)
#' summary(x)
#' if (require(ggplot2)) plot(x, thetaRange = c(0, 0.8))
#' design <- getDesignConditionalDunnett(secondStageConditioning = FALSE)
#' y <- getAnalysisResults(design, dataInput = data,
#'     nPlanned = 40, thetaH1 = c(0.5, NA, 0.8), assumedStDevs = 1.2,  stage = 1)
#' summary(y)
#' if (require(ggplot2)) plot(y, thetaRange = c(0, 0.4))
#'
#' # Example 4 Enrichment Design
#' # Perform an two-stage enrichment design analysis with O'Brien & Fleming boundaries
#' # where one sub-population (S1) and a full population (F) are considered as primary
#' # analysis sets. At interim, S1 is selected for further analysis and the sample
#' # size is increased accordingly. With the Spiessens & Debois intersection test,
#' # the results of a closed adaptive test procedure together with the CRP, repeated
#' # RCIs and p-values are obtained as follows with the given data (displayed with
#' # summary and plot commands):
#' design <- getDesignInverseNormal(kMax = 2, typeOfDesign = "OF")
#' dataS1 <- getDataset(
#'     means1 = c(13.2, 12.8),
#'     means2 = c(11.1, 10.8),
#'     stDev1 = c(3.4, 3.3),
#'     stDev2 = c(2.9, 3.5),
#'     n1 = c(21, 42),
#'     n2 = c(19, 39))
#' dataNotS1 <- getDataset(
#'     means1 = c(11.8, NA),
#'     means2 = c(10.5, NA),
#'     stDev1 = c(3.6, NA),
#'     stDev2 = c(2.7, NA),
#'     n1 = c(15, NA),
#'     n2 = c(13, NA))
#' dataBoth <- getDataset(S1 = dataS1, R = dataNotS1)
#' x <- getAnalysisResults(design, dataInput = dataBoth,
#'     intersectionTest = "SpiessensDebois",
#'     varianceOption = "pooledFromFull",
#'     stratifiedAnalysis = TRUE)
#' summary(x)
#' if (require(ggplot2)) plot(x, type = 2)
#' }
#'
