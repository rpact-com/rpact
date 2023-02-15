#' @examples
#' # Create a Dataset of Means (one group):
#' datasetOfMeans <- getDataset(
#'     n      = c(22, 11, 22, 11),
#'     means  = c(1, 1.1, 1, 1),
#'     stDevs = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans
#' datasetOfMeans$show(showType = 2)
#' \dontrun{
#' datasetOfMeans <- getDataset(
#'     cumulativeSampleSizes = c(22, 33, 55, 66),
#'     cumulativeMeans = c(1.000, 1.033, 1.020, 1.017),
#'     cumulativeStDevs = c(1.00, 1.38, 1.64, 1.58)
#' )
#' datasetOfMeans
#' datasetOfMeans$show(showType = 2)
#' as.data.frame(datasetOfMeans)
#' 
#' # Create a Dataset of Means (two groups):
#' datasetOfMeans <- getDataset(
#'     n1 = c(22, 11, 22, 11),
#'     n2 = c(22, 13, 22, 13),
#'     means1  = c(1, 1.1, 1, 1),
#'     means2  = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans
#' 
#' datasetOfMeans <- getDataset(
#'     cumulativeSampleSizes1 = c(22, 33, 55, 66),
#'     cumulativeSampleSizes2 = c(22, 35, 57, 70),
#'     cumulativeMeans1  = c(1, 1.033, 1.020, 1.017),
#'     cumulativeMeans2  = c(1.4, 1.437, 2.040, 2.126),
#'     cumulativeStDevs1 = c(1, 1.38, 1.64, 1.58),
#'     cumulativeStDevs2 = c(1, 1.43, 1.82, 1.74)
#' )
#' datasetOfMeans
#' 
#' df <- data.frame(
#'     stages = 1:4,
#'     n1      = c(22, 11, 22, 11),
#'     n2      = c(22, 13, 22, 13),
#'     means1  = c(1, 1.1, 1, 1),
#'     means2  = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans <- getDataset(df)
#' datasetOfMeans
#' 
#' # Create a Dataset of Means (three groups) where the comparison of 
#' # treatment arm 1 to control is dropped at the second interim stage:
#' datasetOfMeans <- getDataset(
#'    cumN1      = c(22, 33, NA),
#'    cumN2      = c(20, 34, 56),
#'    cumN3      = c(22, 31, 52),
#'    cumMeans1  = c(1.64, 1.54, NA),
#'    cumMeans2  = c(1.7, 1.5, 1.77),
#'    cumMeans3  = c(2.5, 2.06, 2.99),
#'    cumStDevs1 = c(1.5, 1.9, NA),
#'    cumStDevs2 = c(1.3, 1.3, 1.1),
#'    cumStDevs3 = c(1, 1.3, 1.8))
#' datasetOfMeans
#' 
#' # Create a Dataset of Rates (one group):
#' datasetOfRates <- getDataset(
#'     n = c(8, 10, 9, 11), 
#'     events = c(4, 5, 5, 6)
#' )
#' datasetOfRates
#' 
#' # Create a Dataset of Rates (two groups):
#' datasetOfRates <- getDataset(
#'     n2      = c(8, 10, 9, 11),
#'     n1      = c(11, 13, 12, 13),
#'     events2 = c(3, 5, 5, 6),
#'     events1 = c(10, 10, 12, 12)
#' )
#' datasetOfRates
#' 
#' # Create a Dataset of Rates (three groups) where the comparison of 
#' # treatment arm 2 to control is dropped at the first interim stage:
#' datasetOfRates <- getDataset(
#'     cumN1      = c(22, 33, 44),
#'     cumN2      = c(20, NA, NA),
#'     cumN3      = c(20, 34, 44),
#'     cumEvents1 = c(11, 14, 22),
#'     cumEvents2 = c(17, NA, NA),
#'     cumEvents3 = c(17, 19, 33))
#' datasetOfRates
#' 
#' # Create a Survival Dataset
#' datasetSurvival <- getDataset(
#'     cumEvents = c(8, 15, 19, 31),
#'     cumAllocationRatios = c(1, 1, 1, 2),
#'     cumLogRanks = c(1.52, 1.98, 1.99, 2.11)
#' )
#' datasetSurvival
#'  
#' # Create a Survival Dataset with four comparisons where treatment
#' # arm 2 was dropped at the first interim stage, and treatment arm 4
#' # at the second.
#' datasetSurvival <- getDataset(
#'     cumEvents1   = c(18, 45, 56),
#'     cumEvents2   = c(22, NA, NA),
#'     cumEvents3   = c(12, 41, 56),
#'     cumEvents4   = c(27, 56, NA),
#'     cumLogRanks1 = c(1.52, 1.98, 1.99),
#'     cumLogRanks2 = c(3.43, NA, NA),
#'     cumLogRanks3 = c(1.45, 1.67, 1.87),
#'     cumLogRanks4 = c(1.12, 1.33, NA)
#' )
#' datasetSurvival
#' 
#' # Enrichment: Stratified and unstratified data input
#' # The following data are from one study. Only the first 
#' # (stratified) data input enables a stratified analysis. 
#' 
#' # Stratified data input
#' S1 <- getDataset(
#'     sampleSize1 = c(18, 17), 
#'     sampleSize2 = c(12, 33), 
#'     mean1       = c(125.6, 111.1), 
#'     mean2       = c(107.7, 77.7), 
#'     stDev1      = c(120.1, 145.6),
#'     stDev2      = c(128.5, 133.3)) 
#' S2 <- getDataset(
#'     sampleSize1 = c(11, NA), 
#'     sampleSize2 = c(14, NA), 
#'     mean1       = c(100.1, NA), 
#'     mean2      = c( 68.3, NA), 
#'     stDev1      = c(116.8, NA),
#'     stDev2      = c(124.0, NA)) 
#' S12 <- getDataset(           
#'     sampleSize1 = c(21, 17), 
#'     sampleSize2 = c(21, 12), 
#'     mean1       = c(135.9, 117.7), 
#'     mean2       = c(84.9, 107.7), 
#'     stDev1      = c(185.0, 92.3),
#'     stDev2      = c(139.5, 107.7)) 
#' R <- getDataset(
#'     sampleSize1 = c(19, NA), 
#'     sampleSize2 = c(33, NA), 
#'     mean1       = c(142.4, NA), 
#'     mean2       = c(77.1, NA), 
#'     stDev1      = c(120.6, NA),
#'     stDev2      = c(163.5, NA)) 
#' dataEnrichment <- getDataset(S1 = S1, S2 = S2, S12 = S12, R = R)
#' dataEnrichment
#'
#' # Unstratified data input
#' S1N <- getDataset(
#'     sampleSize1 = c(39, 34), 
#'     sampleSize2 = c(33, 45), 
#'     stDev1      = c(156.503, 120.084), 
#'     stDev2      = c(134.025, 126.502), 
#'     mean1       = c(131.146, 114.4), 
#'     mean2       = c(93.191, 85.7))
#' S2N <- getDataset(
#'     sampleSize1 = c(32, NA), 
#'     sampleSize2 = c(35, NA), 
#'     stDev1      = c(163.645, NA), 
#'     stDev2      = c(131.888, NA),
#'     mean1       = c(123.594, NA), 
#'     mean2       = c(78.26, NA))
#' F <- getDataset(
#'     sampleSize1 = c(69, NA), 
#'     sampleSize2 = c(80, NA), 
#'     stDev1      = c(165.468, NA), 
#'     stDev2      = c(143.979, NA), 
#'     mean1       = c(129.296, NA), 
#'     mean2       = c(82.187, NA))
#' dataEnrichmentN <- getDataset(S1 = S1N, S2 = S2N, F = F)
#' dataEnrichmentN
#' }
#'
