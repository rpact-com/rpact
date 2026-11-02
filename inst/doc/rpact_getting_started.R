## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----planning, message=FALSE--------------------------------------------------
library(rpact)
design <- getDesignGroupSequential(
    kMax = 3, informationRates = c(0.5, 0.75, 1),
    alpha = 0.025, beta = 0.1, sided = 1, typeOfDesign = "OF"
)
sampleSize <- getSampleSizeMeans(
    design = design, groups = 2, alternative = 0.5,
    stDev = 1, allocationRatioPlanned = 1
)
summary(sampleSize)

## ----analysis-----------------------------------------------------------------
trialData <- getDataset(
    sampleSizes1 = c(40, 20, 20), sampleSizes2 = c(40, 20, 20),
    means1 = c(0.2, 0.3, 0.5), means2 = c(0, 0, 0),
    stDevs1 = c(1, 1, 1), stDevs2 = c(1, 1, 1)
)
analysisResults <- getAnalysisResults(
    design = design, dataInput = trialData, directionUpper = TRUE
)
summary(analysisResults)

